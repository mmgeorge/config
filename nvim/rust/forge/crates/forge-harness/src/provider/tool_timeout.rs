use super::{HarnessEvent, ProviderEvent, ToolAddress};
use anyhow::{Result, anyhow};
use forge_provider_api::{ItemCompletion, ItemEvent, ItemStart, ToolItemEvent, TurnEvent};
use std::collections::{BTreeSet, HashMap};
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
    /// Mode of the turns this watchdog observes, which decides whether a question waits.
    mode: crate::provider::PromptMode,
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
    /// Record that a foreground transaction collects this invocation when a deadline elapses.
    foreground_attached: bool,
    idle_retired: bool,
    expiration: Option<ToolTimeout>,
}

enum ToolDeadline {
    Running {
        deadline: Instant,
        title: String,
        background: bool,
    },
    /// Suspend the deadline while Harness waits for a decision on this tool's approval.
    ///
    /// Providers announce a tool before asking to run it, so a running deadline would otherwise
    /// charge the user's decision time to the tool and could stop the turn before the approval's
    /// own timeout resolves it. The deadline resumes with `remaining` once every approval in
    /// `approval_list` is resolved or cancelled.
    AwaitingApproval {
        remaining: Duration,
        title: String,
        background: bool,
        approval_list: BTreeSet<String>,
    },
    UserInput,
    /// Discovery's check owner enforces its deadline and collects children before replying.
    CheckDiscovery,
    Finished,
}

impl ToolDeadline {
    /// Report whether this deadline belongs to foreground execution, which ends with its turn.
    fn is_foreground(&self) -> bool {
        matches!(
            self,
            Self::Running {
                background: false,
                ..
            } | Self::AwaitingApproval {
                background: false,
                ..
            }
        )
    }
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
            "`{}` exceeded its {} tool timeout; execution stopped",
            self.title,
            timeout_label(self.duration)
        )
    }
}

impl std::error::Error for ToolTimeout {}

impl ToolTimeout {
    /// Reserve the single diagnostic and tool-state publication for this timeout.
    pub(crate) fn claim_report(&self) -> bool {
        !self.reported.swap(true, Ordering::AcqRel)
    }

    /// Describe the expiry durably on the exchange that owned the invocation.
    pub(crate) fn record(&self) -> ToolTimeoutRecord {
        ToolTimeoutRecord {
            thread_id: self.address.turn.thread_id.clone(),
            turn_id: self.address.turn.turn_id.clone(),
            item_id: self.address.item_id.clone(),
            title: self.title.clone(),
            timeout_ms: u64::try_from(self.duration.as_millis()).unwrap_or(u64::MAX),
        }
    }
}

/// Retain which invocation exceeded its deadline after the watchdog that observed it is gone.
///
/// Providers report a stopped invocation as interrupted, which says nothing about why it stopped,
/// so the exchange keeps this record to mark the tool failed and to show the reason in its
/// transcript.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
pub struct ToolTimeoutRecord {
    pub thread_id: forge_provider_api::ThreadId,
    pub turn_id: forge_provider_api::TurnId,
    pub item_id: forge_provider_api::ItemId,
    pub title: String,
    pub timeout_ms: u64,
}

impl ToolTimeoutRecord {
    /// Name the configured duration the invocation exceeded, such as `10m` or `1m 30s`.
    pub fn timeout_label(&self) -> String {
        timeout_label(Duration::from_millis(self.timeout_ms))
    }
}

/// Format a configured timeout in the whole units `/config` accepts.
fn timeout_label(duration: Duration) -> String {
    let seconds = duration.as_secs();
    let (hours, minutes, seconds) = (seconds / 3600, seconds % 3600 / 60, seconds % 60);
    let label = [(hours, "h"), (minutes, "m"), (seconds, "s")]
        .into_iter()
        .filter(|(amount, _)| *amount > 0)
        .map(|(amount, unit)| format!("{amount}{unit}"))
        .collect::<Vec<_>>()
        .join(" ");
    if label.is_empty() {
        format!("{}ms", duration.as_millis())
    } else {
        label
    }
}

impl ToolWatchdog {
    /// Create an invocation tracker with a required, finite execution deadline.
    pub(crate) fn new(timeout: crate::session::InputTimeout) -> Result<Self> {
        Ok(Self {
            duration: timeout.require_expiry()?.duration(),
            mode: crate::provider::PromptMode::Chat,
            state: Mutex::new(WatchdogState::default()),
            changed: Notify::new(),
            monitor_started: AtomicBool::new(false),
            timeout_reported: Arc::new(AtomicBool::new(false)),
        })
    }

    /// Observe turns of `mode`, whose waiting questions have no deadline even when the call
    /// does not ask to wait, as in a planning turn.
    pub(crate) fn for_turn_mode(mut self, mode: crate::provider::PromptMode) -> Self {
        self.mode = mode;
        self
    }

    /// Track execution deadlines while leaving explicit waits for user answers unbounded.
    pub(crate) fn observe(&self, observation: &HarnessEvent) -> Result<()> {
        if let HarnessEvent::Host {
            scope,
            event: super::HostEvent::PermissionApproval { event, .. },
        } = observation
        {
            return self.observe_approval(scope, event);
        }
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
                // An approval or execution report can arm the deadline first, under the item
                // identifier; the start supplies the title the expiry names.
                if let Some(
                    ToolDeadline::Running { title, .. }
                    | ToolDeadline::AwaitingApproval { title, .. },
                ) = state.tools.get_mut(&address)
                {
                    title.clone_from(&descriptor.title);
                }
                state.tools.entry(address).or_insert_with(|| {
                    if descriptor.name == "question_ask"
                        && crate::tools::question_ask_waits(descriptor.arguments.value(), self.mode)
                    {
                        ToolDeadline::UserInput
                    } else if descriptor.name == "check_discover" {
                        ToolDeadline::CheckDiscovery
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
                if let Some(
                    ToolDeadline::Running { title: current, .. }
                    | ToolDeadline::AwaitingApproval { title: current, .. },
                ) = state.tools.get_mut(&address)
                {
                    current.clone_from(title);
                }
            }
            ItemEvent::Tool {
                event: ToolItemEvent::DescriptorUpdated(descriptor),
            } => {
                if let Some(
                    ToolDeadline::Running { title, .. }
                    | ToolDeadline::AwaitingApproval { title, .. },
                ) = state.tools.get_mut(&address)
                {
                    title.clone_from(&descriptor.title);
                }
            }
            ItemEvent::Tool {
                event: ToolItemEvent::Backgrounded,
            } => {
                if let Some(
                    ToolDeadline::Running { background, .. }
                    | ToolDeadline::AwaitingApproval { background, .. },
                ) = state.tools.get_mut(&address)
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

    /// Suspend a tool's deadline while its approval is pending and resume it once resolved.
    ///
    /// Only approvals scoped to an item identify the tool they gate. The item is matched by its
    /// invocation and host item identity, which adapters allocate uniquely per tool call.
    fn observe_approval(
        &self,
        scope: &super::EventScope,
        event: &super::PermissionApprovalEvent,
    ) -> Result<()> {
        let Some(invocation) = scope.invocation() else {
            return Ok(());
        };
        let (request_id, approval_scope, opened) = match event {
            super::PermissionApprovalEvent::Requested(request) => {
                (request.request_id.as_str(), &request.scope, true)
            }
            super::PermissionApprovalEvent::Resolved(resolution) => {
                (resolution.request_id.as_str(), &resolution.scope, false)
            }
            super::PermissionApprovalEvent::Cancelled(cancellation) => {
                (cancellation.request_id.as_str(), &cancellation.scope, false)
            }
        };
        let super::ApprovalScope::Item {
            thread_id,
            turn_id,
            item_id,
        } = approval_scope
        else {
            return Ok(());
        };
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        let admitted = state
            .tools
            .keys()
            .find(|address| &address.turn.scope == invocation && &address.item_id == item_id)
            .cloned();
        let now = Instant::now();
        match (admitted, opened) {
            (Some(address), true) => {
                let deadline = state
                    .tools
                    .get_mut(&address)
                    .expect("admitted deadline is present");
                match deadline {
                    ToolDeadline::Running {
                        deadline: expires,
                        title,
                        background,
                    } => {
                        *deadline = ToolDeadline::AwaitingApproval {
                            remaining: expires.saturating_duration_since(now),
                            title: std::mem::take(title),
                            background: *background,
                            approval_list: BTreeSet::from([request_id.to_owned()]),
                        };
                    }
                    ToolDeadline::AwaitingApproval { approval_list, .. } => {
                        approval_list.insert(request_id.to_owned());
                    }
                    ToolDeadline::UserInput
                    | ToolDeadline::CheckDiscovery
                    | ToolDeadline::Finished => {}
                }
            }
            // An approval for a tool this watchdog has not admitted yet holds the full deadline
            // for that tool until the approval resolves.
            (None, true) if !state.foreground_finished && !state.idle_retired => {
                state.tools.insert(
                    ToolAddress {
                        turn: super::TurnAddress {
                            scope: invocation.clone(),
                            thread_id: thread_id.clone(),
                            turn_id: turn_id.clone(),
                        },
                        item_id: item_id.clone(),
                    },
                    ToolDeadline::AwaitingApproval {
                        remaining: self.duration,
                        title: item_id.to_string(),
                        background: false,
                        approval_list: BTreeSet::from([request_id.to_owned()]),
                    },
                );
            }
            (Some(address), false) => {
                let deadline = state
                    .tools
                    .get_mut(&address)
                    .expect("admitted deadline is present");
                if let ToolDeadline::AwaitingApproval {
                    remaining,
                    title,
                    background,
                    approval_list,
                } = deadline
                {
                    approval_list.remove(request_id);
                    if approval_list.is_empty() {
                        *deadline = ToolDeadline::Running {
                            deadline: now + *remaining,
                            title: std::mem::take(title),
                            background: *background,
                        };
                    }
                }
            }
            (None, _) => return Ok(()),
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
            if &tool.turn == address && deadline.is_foreground() {
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
                        ToolDeadline::AwaitingApproval { .. }
                        | ToolDeadline::UserInput
                        | ToolDeadline::CheckDiscovery
                        | ToolDeadline::Finished => None,
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
                () = tokio::time::sleep_until(deadline) => {
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
                if deadline.is_foreground() {
                    *deadline = ToolDeadline::Finished;
                }
            }
            state.foreground_finished = true;
        }
        // Background deadlines that survive settlement belong to the session monitor.
        state.foreground_attached = false;
        let pending = state.tools.values().any(|state| {
            matches!(
                state,
                ToolDeadline::Running { .. } | ToolDeadline::AwaitingApproval { .. }
            )
        });
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

    /// Publish one approval transition scoped to `address`'s tool item.
    fn approval(address: &ToolAddress, request_id: &str, transition: &str) -> HarnessEvent {
        let request_id = crate::provider::ApprovalRequestId::new(request_id).unwrap();
        let scope = crate::provider::ApprovalScope::Item {
            thread_id: address.turn.thread_id.clone(),
            turn_id: address.turn.turn_id.clone(),
            item_id: address.item_id.clone(),
        };
        let event = match transition {
            "requested" => crate::provider::PermissionApprovalEvent::Requested(
                crate::provider::PermissionApprovalRequest {
                    request_id,
                    scope,
                    reason: None,
                    targets: Vec::new(),
                    deadline: None,
                },
            ),
            "resolved" => crate::provider::PermissionApprovalEvent::Resolved(
                crate::provider::PermissionApprovalResolution {
                    request_id,
                    scope,
                    targets: Vec::new(),
                    resolved_at_ms: 0,
                },
            ),
            "cancelled" => crate::provider::PermissionApprovalEvent::Cancelled(
                crate::provider::PermissionApprovalCancellation {
                    request_id,
                    scope,
                    reason: crate::provider::ApprovalCancellationReason::UserCancelled,
                    cancelled_at_ms: 0,
                },
            ),
            other => panic!("unknown approval transition {other}"),
        };
        HarnessEvent::Host {
            scope: address.turn.scope.clone().into(),
            event: crate::provider::HostEvent::PermissionApproval {
                execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                event,
            },
        }
    }

    /// Start a command the way Codex does: announced and running before its approval opens.
    fn start_command(watchdog: &ToolWatchdog, command: &ToolAddress) -> Result<()> {
        watchdog.observe(&requested(
            command,
            TestTool::Command,
            serde_json::json!({}),
        ))?;
        watchdog.observe(&item(
            command,
            ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            },
        ))
    }

    /// Time spent waiting for approval does not count toward the tool's deadline.
    ///
    /// The deadline keeps the time it had left when the approval opened and resumes only after
    /// every approval for the tool resolves, whether by decision or cancellation.
    #[tokio::test(start_paused = true)]
    async fn approval_suspends_its_tool_deadline_until_every_approval_resolves() -> Result<()> {
        let watchdog = watchdog()?;
        let command = address("command");
        start_command(&watchdog, &command)?;
        tokio::time::advance(Duration::from_millis(400)).await;
        watchdog.observe(&approval(&command, "first", "requested"))?;
        watchdog.observe(&approval(&command, "second", "requested"))?;
        let mut pending = Box::pin(watchdog.expired());
        tokio::time::advance(Duration::from_secs(60)).await;
        assert!(
            futures_util::poll!(pending.as_mut()).is_pending(),
            "a pending approval let the tool deadline expire"
        );
        watchdog.observe(&approval(&command, "first", "resolved"))?;
        tokio::time::advance(Duration::from_secs(60)).await;
        assert!(
            futures_util::poll!(pending.as_mut()).is_pending(),
            "the deadline resumed while another approval was pending"
        );
        watchdog.observe(&approval(&command, "second", "cancelled"))?;
        tokio::time::advance(Duration::from_millis(500)).await;
        assert!(
            futures_util::poll!(pending.as_mut()).is_pending(),
            "the resumed deadline lost the time it had left"
        );
        tokio::time::advance(Duration::from_millis(200)).await;
        assert_eq!(pending.await?.unwrap().address, command);
        Ok(())
    }

    /// A tool whose deadline was armed before its start event still expires under its own title.
    ///
    /// An approval or execution report can reach the watchdog before the item's start, which
    /// armed the deadline under the item identifier. The start kept that placeholder, so the
    /// expiry read "Stopped: `25781846-…` exceeded its 15s tool timeout".
    #[tokio::test(start_paused = true)]
    async fn early_deadline_takes_the_title_from_the_item_start() -> Result<()> {
        for early in ["approval", "execution"] {
            let watchdog = watchdog()?;
            let command = address("command");
            watchdog.observe(&match early {
                "approval" => approval(&command, "first", "requested"),
                _ => item(
                    &command,
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                ),
            })?;
            watchdog.observe(&item(
                &command,
                ItemEvent::Started(ItemStart::CommandExecution(ToolDescriptor {
                    semantics: Default::default(),
                    name: "shell".into(),
                    title: "sleep 45sec".into(),
                    arguments: InspectableArguments::new(serde_json::json!({})),
                })),
            ))?;
            if early == "approval" {
                watchdog.observe(&approval(&command, "first", "resolved"))?;
            }
            tokio::time::advance(Duration::from_secs(2)).await;
            let expired = watchdog.expired().await?.unwrap();
            assert_eq!(expired.record().title, "sleep 45sec", "armed by {early}");
        }
        Ok(())
    }

    /// An approval suspends only the tool it names.
    #[tokio::test(start_paused = true)]
    async fn approval_for_another_tool_leaves_running_deadlines_in_force() -> Result<()> {
        let watchdog = watchdog()?;
        let command = address("command");
        start_command(&watchdog, &command)?;
        watchdog.observe(&approval(&address("other"), "other", "requested"))?;
        tokio::time::advance(Duration::from_secs(2)).await;
        assert_eq!(watchdog.expired().await?.unwrap().address, command);
        Ok(())
    }

    /// User waits and owner-timed discovery do not disable unrelated tool deadlines.
    #[tokio::test(start_paused = true)]
    async fn waiting_questions_do_not_expire_but_other_tools_do() -> Result<()> {
        let watchdog = watchdog()?;
        for (name, arguments) in [
            ("question_ask", serde_json::json!({"wait":true})),
            ("check_discover", serde_json::json!({"check":"unit"})),
        ] {
            let managed = address(name);
            watchdog.observe(&requested(&managed, TestTool::Tool, arguments))?;
            watchdog.observe(&item(
                &managed,
                ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                },
            ))?;
        }
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

    /// A planning turn's question waits for its answer without a deadline, as a planning
    /// question always does, while the same call in a chat turn returns at once and keeps one.
    #[tokio::test(start_paused = true)]
    async fn planning_questions_wait_without_a_deadline() -> Result<()> {
        let question = address("question_ask");
        let planning = watchdog()?.for_turn_mode(crate::provider::PromptMode::Plan);
        planning.observe(&requested(&question, TestTool::Tool, serde_json::json!({})))?;
        tokio::time::advance(Duration::from_secs(10)).await;
        let pending = std::pin::pin!(planning.expired());
        assert!(
            futures_util::poll!(pending).is_pending(),
            "a waiting planning question expired"
        );
        let chat = watchdog()?;
        chat.observe(&requested(&question, TestTool::Tool, serde_json::json!({})))?;
        tokio::time::advance(Duration::from_secs(2)).await;
        assert_eq!(chat.expired().await?.unwrap().address, question);
        Ok(())
    }
}
