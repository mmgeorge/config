use super::{HarnessEvent, ProviderEvent, ToolAddress};
use anyhow::{Result, anyhow};
use forge_provider_api::{ItemCompletion, ItemEvent, ItemStart, ToolItemEvent, TurnEvent};
use std::collections::HashMap;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::time::Duration;
use tokio::sync::Notify;
use tokio::time::Instant;

/// Retains each invocation's original deadline, independently of output and replay.
pub(crate) struct ToolWatchdog {
    duration: Duration,
    state: Mutex<WatchdogState>,
    changed: Notify,
    monitor_started: AtomicBool,
    timeout_reported: Arc<AtomicBool>,
    #[cfg(test)]
    forced_expiry: Notify,
}

#[derive(Default)]
struct WatchdogState {
    tools: HashMap<ToolAddress, ToolDeadline>,
    active_native: Option<super::TurnAddress>,
    foreground_finished: bool,
    /// Record that a foreground transaction collects this invocation when a deadline elapses.
    foreground_attached: bool,
    idle_retired: bool,
    expiration: Option<ToolTimeout>,
    /// Treats every deadline at or before this instant as elapsed, as a real elapsed deadline would.
    #[cfg(test)]
    forced_through: Option<Instant>,
}

enum ToolDeadline {
    Running {
        deadline: Instant,
        title: String,
        background: bool,
    },
    UserInput,
    Finished,
}

/// Hold foreground ownership of timeout collection until the foreground transaction ends.
///
/// The foreground and the session's inbox monitor both observe one watchdog. Only the attached
/// foreground collects its native turn on expiry; the monitor collects only after this owner is
/// released, so an expiry has exactly one collector and one terminal exchange outcome.
pub(crate) struct ForegroundToolOwner {
    watchdog: Arc<ToolWatchdog>,
}

impl ForegroundToolOwner {
    /// Create foreground ownership of the watchdog's timeout collection.
    pub(crate) fn attach(watchdog: Arc<ToolWatchdog>) -> Result<Self> {
        watchdog.set_foreground_attached(true)?;
        Ok(Self { watchdog })
    }
}

impl Drop for ForegroundToolOwner {
    fn drop(&mut self) {
        // A poisoned watchdog already fails its waiters; release must not panic during unwinding.
        let _ = self.watchdog.set_foreground_attached(false);
    }
}

/// Identifies the invocation whose deadline stopped provider execution.
#[derive(Clone, Debug)]
pub(crate) struct ToolTimeout {
    /// Retains the exact provider invocation that exceeded its deadline.
    pub(crate) address: ToolAddress,
    title: String,
    duration: Duration,
    reported: Arc<AtomicBool>,
}

impl std::fmt::Display for ToolTimeout {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "Tool or shell command '{}' ({}) exceeded its {} second timeout; execution stopped",
            self.title,
            self.address.item_id,
            self.duration.as_secs()
        )
    }
}

impl std::error::Error for ToolTimeout {}

impl ToolTimeout {
    /// Reserve the single diagnostic and tool-state publication for this timeout.
    pub(crate) fn claim_report(&self) -> bool {
        !self.reported.swap(true, Ordering::AcqRel)
    }
}

impl ToolWatchdog {
    /// Create an invocation tracker with a required, finite execution deadline.
    pub(crate) fn new(timeout: crate::session::InputTimeout) -> Result<Self> {
        Ok(Self {
            duration: timeout.require_expiry()?.duration(),
            state: Mutex::new(WatchdogState::default()),
            changed: Notify::new(),
            monitor_started: AtomicBool::new(false),
            #[cfg(test)]
            forced_expiry: Notify::new(),
            timeout_reported: Arc::new(AtomicBool::new(false)),
        })
    }

    /// Track execution deadlines while leaving explicit waits for user answers unbounded.
    pub(crate) fn observe(&self, observation: &HarnessEvent) -> Result<()> {
        let HarnessEvent::Native { scope, event, .. } = observation else {
            return Ok(());
        };
        let event = event.as_ref();
        let Some(scope) = scope.invocation() else {
            return Ok(());
        };
        let (thread_id, turn_id) = match event {
            ProviderEvent::Turn {
                thread_id, turn_id, ..
            }
            | ProviderEvent::Item {
                thread_id, turn_id, ..
            } => (thread_id, turn_id),
            ProviderEvent::Provider { .. }
            | ProviderEvent::Thread { .. }
            | ProviderEvent::Agent { .. } => return Ok(()),
        };
        let turn = super::TurnAddress {
            scope: scope.clone(),
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
        };
        if let ProviderEvent::Turn {
            event: TurnEvent::Started(_),
            ..
        } = event
        {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
            if !state.idle_retired {
                state.foreground_finished = false;
                state.active_native = Some(turn);
                drop(state);
                self.changed.notify_waiters();
            }
            return Ok(());
        }
        if let ProviderEvent::Turn {
            event: TurnEvent::Completed(_),
            ..
        } = event
        {
            return self.finish_native_execution(&turn);
        }
        let ProviderEvent::Item { item_id, event, .. } = event else {
            return Ok(());
        };
        let address = ToolAddress {
            turn,
            item_id: item_id.clone(),
        };
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        if (state.foreground_finished || state.idle_retired) && !state.tools.contains_key(&address)
        {
            return Ok(());
        }
        match event {
            ItemEvent::Started(
                ItemStart::CommandExecution(descriptor)
                | ItemStart::FileChange(descriptor)
                | ItemStart::ToolCall(descriptor),
            ) => {
                state.tools.entry(address).or_insert_with(|| {
                    if descriptor.name == "question_ask"
                        && descriptor.arguments.value()["wait"].as_bool() == Some(true)
                    {
                        ToolDeadline::UserInput
                    } else {
                        ToolDeadline::Running {
                            deadline: Instant::now() + self.duration,
                            title: descriptor.title.clone(),
                            background: false,
                        }
                    }
                });
            }
            ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            } => {
                let title = address.item_id.to_string();
                state
                    .tools
                    .entry(address)
                    .or_insert_with(|| ToolDeadline::Running {
                        deadline: Instant::now() + self.duration,
                        title,
                        background: false,
                    });
            }
            ItemEvent::Completed(
                ItemCompletion::CommandExecution(_)
                | ItemCompletion::FileChange(_)
                | ItemCompletion::ToolCall(_),
            ) => {
                state.tools.insert(address, ToolDeadline::Finished);
            }
            ItemEvent::Tool {
                event: ToolItemEvent::TitleChanged(title),
            } => {
                if let Some(ToolDeadline::Running { title: current, .. }) =
                    state.tools.get_mut(&address)
                {
                    current.clone_from(title);
                }
            }
            ItemEvent::Tool {
                event: ToolItemEvent::DescriptorUpdated(descriptor),
            } => {
                if let Some(ToolDeadline::Running { title, .. }) = state.tools.get_mut(&address) {
                    title.clone_from(&descriptor.title);
                }
            }
            ItemEvent::Tool {
                event: ToolItemEvent::Backgrounded,
            } => {
                if let Some(ToolDeadline::Running { background, .. }) =
                    state.tools.get_mut(&address)
                {
                    *background = true;
                }
            }
            ItemEvent::Started(
                ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_),
            )
            | ItemEvent::Completed(
                ItemCompletion::UserMessage(_)
                | ItemCompletion::AgentMessage(_)
                | ItemCompletion::Reasoning(_),
            )
            | ItemEvent::Message { .. }
            | ItemEvent::Tool {
                event:
                    ToolItemEvent::Input(_) | ToolItemEvent::Output(_) | ToolItemEvent::Changes(_),
            } => {}
        }
        drop(state);
        self.changed.notify_one();
        Ok(())
    }

    /// Wait until no foreground transaction owns timeout collection for this invocation.
    pub(crate) async fn foreground_released(&self) -> Result<()> {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if !self
                .state
                .lock()
                .map_err(|_| anyhow!("tool deadline lock poisoned"))?
                .foreground_attached
            {
                return Ok(());
            }
            changed.await;
        }
    }

    /// Record whether a foreground transaction owns timeout collection.
    fn set_foreground_attached(&self, attached: bool) -> Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?
            .foreground_attached = attached;
        self.changed.notify_waiters();
        Ok(())
    }

    /// Retain existing background deadlines while stopping adoption of later native executions.
    pub(crate) fn retire_idle(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        state.idle_retired = true;
        state.foreground_finished = true;
        drop(state);
        self.changed.notify_waiters();
        Ok(())
    }

    /// Reserve session-owned deadline observation exactly once for this scope tracker.
    pub(crate) fn start_monitor(&self) -> bool {
        !self.monitor_started.swap(true, Ordering::AcqRel)
    }

    /// Report whether session ownership of deadline observation has already been installed.
    pub(crate) fn is_monitored(&self) -> bool {
        self.monitor_started.load(Ordering::Acquire)
    }

    /// Report whether an invocation's execution deadline is armed and still unfinished.
    #[cfg(test)]
    pub(crate) fn has_running_deadline(&self) -> bool {
        self.state
            .lock()
            .expect("tool deadline lock poisoned")
            .tools
            .values()
            .any(|deadline| matches!(deadline, ToolDeadline::Running { .. }))
    }

    /// Get the first deadline failure even after native cancellation reports terminal tools.
    pub(crate) fn timeout(&self) -> Result<Option<ToolTimeout>> {
        Ok(self
            .state
            .lock()
            .map_err(|_| anyhow!("tool expiration lock poisoned"))?
            .expiration
            .clone())
    }

    /// Wait for positively certified native activity or retirement of this retained scope.
    pub(crate) async fn resumed(&self) -> Result<bool> {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
                if state.idle_retired {
                    return Ok(false);
                }
                if !state.foreground_finished {
                    return Ok(true);
                }
            }
            changed.await;
        }
    }

    /// Retire one native execution's ordinary tools while preserving background deadlines.
    fn finish_native_execution(&self, address: &crate::provider::TurnAddress) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        for (tool, deadline) in state.tools.iter_mut() {
            if &tool.turn == address
                && matches!(
                    deadline,
                    ToolDeadline::Running {
                        background: false,
                        ..
                    }
                )
            {
                *deadline = ToolDeadline::Finished;
            }
        }
        if state.active_native.as_ref() == Some(address) {
            state.active_native = None;
        }
        drop(state);
        self.changed.notify_one();
        Ok(())
    }

    /// Wait for the earliest unfinished invocation without extending it on activity.
    pub(crate) async fn expired(&self) -> Result<Option<ToolTimeout>> {
        loop {
            if let Some(timeout) = self.timeout()? {
                return Ok(Some(timeout));
            }
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let (next, foreground_finished) = {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
                let next = state
                    .tools
                    .iter()
                    .filter_map(|(address, state)| match state {
                        ToolDeadline::Running {
                            deadline, title, ..
                        } => Some((*deadline, address.clone(), title.clone())),
                        ToolDeadline::UserInput | ToolDeadline::Finished => None,
                    })
                    .min_by_key(|(deadline, _, _)| *deadline);
                (next, state.foreground_finished)
            };
            let Some((deadline, address, title)) = next else {
                if foreground_finished {
                    return Ok(None);
                }
                changed.await;
                continue;
            };
            tokio::select! {
                biased;
                forced = self.deadline_elapsed(deadline) => {
                    let mut state = self.state.lock().map_err(|_| anyhow!("tool deadline lock poisoned"))?;
                    if matches!(state.tools.get(&address), Some(ToolDeadline::Running { deadline, .. }) if forced || *deadline <= Instant::now()) {
                        let timeout = state.expiration.get_or_insert_with(|| ToolTimeout {
                            address, title, duration: self.duration, reported: self.timeout_reported.clone(),
                        });
                        return Ok(Some(timeout.clone()));
                    }
                }
                _ = changed => {},
            }
        }
    }

    /// Wait for the earliest deadline, returning whether a test forced expiry before it elapsed.
    async fn deadline_elapsed(&self, deadline: Instant) -> bool {
        #[cfg(test)]
        loop {
            let forced = self.forced_expiry.notified();
            tokio::pin!(forced);
            forced.as_mut().enable();
            if self.expiry_forced_through(deadline) {
                return true;
            }
            tokio::select! {
                biased;
                () = tokio::time::sleep_until(deadline) => return false,
                () = forced => {}
            }
        }
        #[cfg(not(test))]
        {
            tokio::time::sleep_until(deadline).await;
            false
        }
    }

    /// Report whether a forced expiry covers this deadline.
    #[cfg(test)]
    fn expiry_forced_through(&self, deadline: Instant) -> bool {
        self.state
            .lock()
            .expect("tool deadline lock poisoned")
            .forced_through
            .is_some_and(|forced| deadline <= forced)
    }

    /// Expire the earliest running invocation as if its deadline had elapsed.
    ///
    /// Tests whose runtime cannot auto-advance a paused clock use this instead of waiting on
    /// the real clock. Several tasks wait on one watchdog: the inbox monitor interrupts native
    /// work while the foreground transaction fails the exchange. A real deadline wakes all of
    /// them at once, so the forced deadline is recorded as state that every current and later
    /// wait observes. A single-waiter permit let the monitor interrupt native work while the
    /// transaction saw the timeout only after the interrupted turn's result, racing ordinary
    /// finalization against timeout failure.
    #[cfg(test)]
    pub(crate) fn force_expiry(&self) {
        let mut state = self.state.lock().expect("tool deadline lock poisoned");
        let earliest = state
            .tools
            .values()
            .filter_map(|deadline| match deadline {
                ToolDeadline::Running { deadline, .. } => Some(*deadline),
                ToolDeadline::UserInput | ToolDeadline::Finished => None,
            })
            .min();
        state.forced_through = state.forced_through.max(earliest);
        drop(state);
        self.forced_expiry.notify_waiters();
    }

    /// Retire completed foreground tools without ending a newer certified native execution.
    pub(crate) fn finish_foreground(&self) -> Result<bool> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        // A certified continuation can start between host commit and result delivery.
        if state.active_native.is_none() {
            for deadline in state.tools.values_mut() {
                if matches!(
                    deadline,
                    ToolDeadline::Running {
                        background: false,
                        ..
                    }
                ) {
                    *deadline = ToolDeadline::Finished;
                }
            }
            state.foreground_finished = true;
        }
        // Background deadlines that survive settlement belong to the session monitor.
        state.foreground_attached = false;
        let pending = state
            .tools
            .values()
            .any(|state| matches!(state, ToolDeadline::Running { .. }));
        drop(state);
        self.changed.notify_waiters();
        Ok(pending)
    }
}

impl super::events::EventObserver for ToolWatchdog {
    fn before_delivery(
        &self,
        event: &HarnessEvent,
    ) -> Result<(), forge_provider_api::events::EventDeliveryError> {
        self.observe(event)
            .map_err(forge_provider_api::events::EventDeliveryError::transport)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::ScopeOwner;
    use forge_provider_api::{InspectableArguments, ItemId, ToolDescriptor};

    enum TestTool {
        Command,
        Tool,
    }

    /// Bind each tool fixture to its original admitted response cycle.
    fn address(id: &str) -> ToolAddress {
        let scope = super::super::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(super::super::ThreadOwner::root(
                "session",
            )),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        };
        ToolAddress {
            turn: super::super::TurnAddress {
                thread_id: scope.owner().thread_id(),
                turn_id: scope.turn_id().unwrap(),
                scope,
            },
            item_id: ItemId::new(id).unwrap(),
        }
    }

    /// Wrap native item activity with its already resolved host owner.
    fn item(address: &ToolAddress, event: ItemEvent) -> HarnessEvent {
        crate::provider::test_support::native_event(
            address.turn.scope.clone(),
            ProviderEvent::Item {
                thread_id: address.turn.thread_id.clone(),
                turn_id: address.turn.turn_id.clone(),
                item_id: address.item_id.clone(),
                event,
            },
        )
    }

    /// Create one-second deadlines under virtual time.
    fn watchdog() -> Result<ToolWatchdog> {
        ToolWatchdog::new(crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        })
    }

    /// Admit a native command or tool descriptor without conflating its final outcome.
    fn requested(
        address: &ToolAddress,
        kind: TestTool,
        arguments: serde_json::Value,
    ) -> HarnessEvent {
        let descriptor = ToolDescriptor {
            semantics: Default::default(),
            name: address.item_id.to_string(),
            title: address.item_id.to_string(),
            arguments: InspectableArguments::new(arguments),
        };
        item(
            address,
            ItemEvent::Started(match kind {
                TestTool::Command => ItemStart::CommandExecution(descriptor),
                TestTool::Tool => ItemStart::ToolCall(descriptor),
            }),
        )
    }

    /// Questions remain unbounded without disabling unrelated tool deadlines.
    #[tokio::test(start_paused = true)]
    async fn waiting_questions_do_not_expire_but_other_tools_do() -> Result<()> {
        let watchdog = watchdog()?;
        let question = address("question_ask");
        watchdog.observe(&requested(
            &question,
            TestTool::Tool,
            serde_json::json!({"wait":true}),
        ))?;
        watchdog.observe(&item(
            &question,
            ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            },
        ))?;
        tokio::time::advance(Duration::from_secs(10)).await;
        let mut pending = Box::pin(watchdog.expired());
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        let command = address("command");
        watchdog.observe(&requested(
            &command,
            TestTool::Command,
            serde_json::json!({}),
        ))?;
        assert_eq!(pending.await?.unwrap().address, command);
        Ok(())
    }

    /// Repeated admission, starts, and output cannot extend the original execution deadline.
    #[tokio::test(start_paused = true)]
    async fn activity_never_extends_shell_or_tool_deadlines() -> Result<()> {
        for kind in [TestTool::Command, TestTool::Tool] {
            let watchdog = watchdog()?;
            let started = Instant::now();
            let tool = address("stalled");
            let requested = requested(&tool, kind, serde_json::json!({}));
            watchdog.observe(&requested)?;
            tokio::time::advance(Duration::from_millis(750)).await;
            watchdog.observe(&requested)?;
            watchdog.observe(&item(
                &tool,
                ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                },
            ))?;
            watchdog.observe(&item(
                &tool,
                ItemEvent::Tool {
                    event: ToolItemEvent::Output(forge_provider_api::TextUpdate::Append(
                        "still running".into(),
                    )),
                },
            ))?;
            assert_eq!(watchdog.expired().await?.unwrap().address, tool);
            assert_eq!(started.elapsed(), Duration::from_secs(1));
        }
        Ok(())
    }
}
