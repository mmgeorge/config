use super::{ProviderEvent, ToolAddress, ToolUpdate};
use anyhow::{Result, anyhow};
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
}

#[derive(Default)]
struct WatchdogState {
    tools: HashMap<ToolAddress, ToolDeadline>,
    active_native: Option<super::TurnAddress>,
    foreground_finished: bool,
    idle_retired: bool,
    expiration: Option<ToolTimeout>,
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
            self.address.tool_id,
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
            timeout_reported: Arc::new(AtomicBool::new(false)),
        })
    }

    /// Track execution deadlines while leaving explicit waits for user answers unbounded.
    pub(crate) fn observe(&self, event: &ProviderEvent) -> Result<()> {
        if let ProviderEvent::Execution { observation } = event
            && let forge_provider_api::lifecycle::ExecutionUpdate::Started { address } =
                observation.update()
        {
            let mut state = self
                .state
                .lock()
                .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
            if !state.idle_retired {
                state.foreground_finished = false;
                state.active_native = Some(address.clone());
                drop(state);
                self.changed.notify_waiters();
            }
            return Ok(());
        }
        if let ProviderEvent::Execution { observation } = event
            && let forge_provider_api::lifecycle::ExecutionUpdate::Finished { address, .. } =
                observation.update()
        {
            return self.finish_native_execution(address);
        }
        let ProviderEvent::Tool { address, update } = event else {
            return Ok(());
        };
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        if (state.foreground_finished || state.idle_retired) && !state.tools.contains_key(address) {
            return Ok(());
        }
        match update {
            ToolUpdate::Requested { invocation } => {
                state.tools.entry(address.clone()).or_insert_with(|| {
                    if invocation.name == "question_ask"
                        && invocation.arguments["wait"].as_bool() == Some(true)
                    {
                        ToolDeadline::UserInput
                    } else {
                        ToolDeadline::Running {
                            deadline: Instant::now() + self.duration,
                            title: invocation.title.clone(),
                            background: false,
                        }
                    }
                });
            }
            ToolUpdate::Started => {
                state
                    .tools
                    .entry(address.clone())
                    .or_insert_with(|| ToolDeadline::Running {
                        deadline: Instant::now() + self.duration,
                        title: address.tool_id.to_string(),
                        background: false,
                    });
            }
            ToolUpdate::Finished { .. } => {
                state.tools.insert(address.clone(), ToolDeadline::Finished);
            }
            ToolUpdate::TitleChanged { title } => {
                if let Some(ToolDeadline::Running { title: current, .. }) =
                    state.tools.get_mut(address)
                {
                    current.clone_from(title);
                }
            }
            ToolUpdate::Backgrounded => {
                if let Some(ToolDeadline::Running { background, .. }) = state.tools.get_mut(address)
                {
                    *background = true;
                }
            }
            ToolUpdate::Input { .. }
            | ToolUpdate::Output { .. }
            | ToolUpdate::Changes { .. }
            | ToolUpdate::SkillRead { .. } => {}
        }
        drop(state);
        self.changed.notify_one();
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
                _ = tokio::time::sleep_until(deadline) => {
                    let mut state = self.state.lock().map_err(|_| anyhow!("tool deadline lock poisoned"))?;
                    if matches!(state.tools.get(&address), Some(ToolDeadline::Running { deadline, .. }) if *deadline <= Instant::now()) {
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
        let pending = state
            .tools
            .values()
            .any(|state| matches!(state, ToolDeadline::Running { .. }));
        drop(state);
        self.changed.notify_one();
        Ok(pending)
    }
}

impl forge_provider_api::events::EventObserver for ToolWatchdog {
    fn before_delivery(
        &self,
        event: &ProviderEvent,
    ) -> Result<(), forge_provider_api::events::EventDeliveryError> {
        self.observe(event)
            .map_err(forge_provider_api::events::EventDeliveryError::transport)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{
        ExecutionOwner, InvocationScope, TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome,
    };
    use anyhow::Context;

    /// Bind each fixture invocation to a real execution identity.
    fn address(id: &str) -> ToolAddress {
        ToolAddress {
            turn: crate::provider::test_support::address(
                &InvocationScope {
                    owner: ExecutionOwner::root("session"),
                    exchange_id: "exchange".into(),
                    invocation_id: "invocation".into(),
                },
                "thread",
                "turn",
            ),
            tool_id: id.into(),
        }
    }

    /// Create a one-second policy for deterministic virtual-clock tests.
    fn watchdog() -> Result<ToolWatchdog> {
        ToolWatchdog::new(crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        })
    }

    /// Admit shell and MCP calls through the same event contract.
    fn requested(id: &str, kind: ToolActivityKind) -> ProviderEvent {
        ProviderEvent::Tool {
            address: address(id),
            update: ToolUpdate::Requested {
                invocation: ToolInvocation {
                    semantics: Default::default(),
                    kind,
                    name: id.into(),
                    title: id.into(),
                    arguments: serde_json::json!({}),
                },
            },
        }
    }

    /// Wait for user decisions without disabling deadlines on unrelated execution.
    #[tokio::test(start_paused = true)]
    async fn waiting_questions_do_not_expire_but_other_tools_do() -> Result<()> {
        let watchdog = watchdog()?;
        let mut question = requested("question_ask", ToolActivityKind::ToolCall);
        if let ProviderEvent::Tool {
            update: ToolUpdate::Requested { invocation },
            ..
        } = &mut question
        {
            invocation.arguments = serde_json::json!({"wait":true});
        }
        watchdog.observe(&question)?;
        watchdog.observe(&ProviderEvent::Tool {
            address: address("question_ask"),
            update: ToolUpdate::Started,
        })?;
        tokio::time::advance(Duration::from_secs(10)).await;
        let mut pending = Box::pin(watchdog.expired());
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        watchdog.observe(&requested("command", ToolActivityKind::Command))?;
        let expired = pending.await?.unwrap();
        assert_eq!(expired.address, address("command"));
        Ok(())
    }

    /// Output and repeated starts cannot keep either shell or tool execution alive indefinitely.
    #[tokio::test(start_paused = true)]
    async fn activity_never_extends_shell_or_tool_deadlines() -> Result<()> {
        for kind in [ToolActivityKind::Command, ToolActivityKind::ToolCall] {
            let watchdog = watchdog()?;
            let started = Instant::now();
            let event = requested("stalled", kind);
            watchdog.observe(&event)?;
            tokio::time::advance(Duration::from_millis(750)).await;
            watchdog.observe(&event)?;
            watchdog.observe(&ProviderEvent::Tool {
                address: address("stalled"),
                update: ToolUpdate::Started,
            })?;
            watchdog.observe(&ProviderEvent::Tool {
                address: address("stalled"),
                update: ToolUpdate::Output {
                    update: TextUpdate::Append("still running".into()),
                },
            })?;
            let expired = watchdog.expired().await?.unwrap();
            assert_eq!(started.elapsed(), Duration::from_secs(1));
            assert_eq!(expired.address, address("stalled"));
            assert!(expired.to_string().contains("execution stopped"));
        }
        Ok(())
    }

    /// Completed calls and their replay cannot expire or hide another concurrent deadline.
    #[tokio::test(start_paused = true)]
    async fn completion_and_replay_preserve_other_deadlines() -> Result<()> {
        let watchdog = watchdog()?;
        let completed = requested("completed", ToolActivityKind::ToolCall);
        watchdog.observe(&completed)?;
        tokio::time::advance(Duration::from_millis(500)).await;
        watchdog.observe(&requested("pending", ToolActivityKind::Command))?;
        watchdog.observe(&ProviderEvent::Tool {
            address: address("completed"),
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        })?;
        watchdog.observe(&completed)?;
        let expired = watchdog.expired().await?.unwrap();
        assert_eq!(expired.address, address("pending"));
        Ok(())
    }

    /// Keep background deadlines and preserve an elapsed timeout through a late success frame.
    #[tokio::test(start_paused = true)]
    async fn background_deadlines_survive_foreground_completion() -> Result<()> {
        let watchdog = watchdog()?;
        let started = Instant::now();
        watchdog.observe(&requested("background", ToolActivityKind::Command))?;
        watchdog.observe(&ProviderEvent::Tool {
            address: address("background"),
            update: ToolUpdate::Backgrounded,
        })?;
        tokio::time::advance(Duration::from_millis(750)).await;
        assert!(watchdog.finish_foreground()?);
        let expired = watchdog.expired().await?.unwrap();
        assert_eq!(started.elapsed(), Duration::from_secs(1));
        assert_eq!(expired.address, address("background"));
        watchdog.observe(&ProviderEvent::Tool {
            address: address("background"),
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        })?;
        watchdog.observe(&requested("later-attempt", ToolActivityKind::Command))?;
        assert_eq!(
            watchdog.expired().await?.unwrap().address,
            address("background")
        );
        Ok(())
    }

    /// Retire an earlier native turn without disabling deadlines for its same-scope continuation.
    #[tokio::test(start_paused = true)]
    async fn native_completion_preserves_continuation_deadlines() -> Result<()> {
        let watchdog = watchdog()?;
        let started = Instant::now();
        watchdog.observe(&requested("ordinary", ToolActivityKind::Command))?;
        tokio::time::advance(Duration::from_millis(750)).await;
        let turn = address("ordinary").turn;
        watchdog.observe(&ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                turn.scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                    address: turn,
                    covered_submissions: Vec::new(),
                    result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    },
                    completed_at_ms: 0,
                },
            )?,
        })?;
        let mut next = requested("ordinary", ToolActivityKind::Command);
        let ProviderEvent::Tool { address: later, .. } = &mut next else {
            unreachable!("fixture tool event")
        };
        later.turn.turn_id = "later-turn".into();
        let later = later.clone();
        watchdog.observe(&next)?;

        let timeout = watchdog
            .expired()
            .await?
            .context("continuation deadline ended without expiry")?;
        assert_eq!(timeout.address, later);
        assert_eq!(started.elapsed(), Duration::from_millis(1750));
        Ok(())
    }

    /// Preserve a positively reopened tool when delivery of the prior host result finishes late.
    #[tokio::test(start_paused = true)]
    async fn late_host_completion_cannot_retire_reopened_tool() -> Result<()> {
        let watchdog = watchdog()?;
        let mut tool = requested("continued", ToolActivityKind::Command);
        let ProviderEvent::Tool { address, .. } = &mut tool else {
            unreachable!("fixture tool event")
        };
        address.turn.turn_id = "reopened".into();
        let expected = address.clone();
        watchdog.observe(&ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                expected.turn.scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Started {
                    address: expected.turn.clone(),
                },
            )?,
        })?;
        watchdog.observe(&tool)?;
        assert!(watchdog.finish_foreground()?);
        assert_eq!(watchdog.expired().await?.unwrap().address, expected);
        Ok(())
    }
}
