use super::{ProviderEvent, ToolAddress, ToolUpdate};
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use std::sync::{
    Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::time::Duration;
use tokio::sync::Notify;
use tokio::time::Instant;

/// Retains each invocation's original deadline, independently of output and replay.
pub(crate) struct ToolWatchdog {
    duration: Duration,
    tools: Mutex<HashMap<ToolAddress, ToolDeadline>>,
    changed: Notify,
    foreground_finished: AtomicBool,
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
#[derive(Debug)]
pub(crate) struct ToolTimeout {
    /// Retains the exact provider invocation that exceeded its deadline.
    pub(crate) address: ToolAddress,
    title: String,
    duration: Duration,
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

impl ToolWatchdog {
    /// Create an invocation tracker with a required, finite execution deadline.
    pub(crate) fn new(timeout: crate::session::InputTimeout) -> Result<Self> {
        Ok(Self {
            duration: timeout.require_expiry()?.duration(),
            tools: Mutex::new(HashMap::new()),
            changed: Notify::new(),
            foreground_finished: AtomicBool::new(false),
        })
    }

    /// Track execution deadlines while leaving explicit waits for user answers unbounded.
    pub(crate) fn observe(&self, event: &ProviderEvent) -> Result<()> {
        let ProviderEvent::Tool { address, update } = event else {
            return Ok(());
        };
        let mut tools = self
            .tools
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        match update {
            ToolUpdate::Requested { invocation } => {
                tools.entry(address.clone()).or_insert_with(|| {
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
                tools
                    .entry(address.clone())
                    .or_insert_with(|| ToolDeadline::Running {
                        deadline: Instant::now() + self.duration,
                        title: address.tool_id.to_string(),
                        background: false,
                    });
            }
            ToolUpdate::Finished { .. } => {
                tools.insert(address.clone(), ToolDeadline::Finished);
            }
            ToolUpdate::TitleChanged { title } => {
                if let Some(ToolDeadline::Running { title: current, .. }) = tools.get_mut(address) {
                    current.clone_from(title);
                }
            }
            ToolUpdate::Backgrounded => {
                if let Some(ToolDeadline::Running { background, .. }) = tools.get_mut(address) {
                    *background = true;
                }
            }
            ToolUpdate::Input { .. }
            | ToolUpdate::Output { .. }
            | ToolUpdate::Changes { .. }
            | ToolUpdate::SkillRead { .. } => {}
        }
        drop(tools);
        self.changed.notify_one();
        Ok(())
    }

    /// Wait for the earliest unfinished invocation without extending it on activity.
    pub(crate) async fn expired(&self) -> Result<Option<ToolTimeout>> {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let next = self
                .tools
                .lock()
                .map_err(|_| anyhow!("tool deadline lock poisoned"))?
                .iter()
                .filter_map(|(address, state)| match state {
                    ToolDeadline::Running {
                        deadline, title, ..
                    } => Some((*deadline, address.clone(), title.clone())),
                    ToolDeadline::UserInput | ToolDeadline::Finished => None,
                })
                .min_by_key(|(deadline, _, _)| *deadline);
            let Some((deadline, address, title)) = next else {
                if self.foreground_finished.load(Ordering::Acquire) {
                    return Ok(None);
                }
                changed.await;
                continue;
            };
            tokio::select! {
                biased;
                _ = tokio::time::sleep_until(deadline) => {
                    let tools = self.tools.lock().map_err(|_| anyhow!("tool deadline lock poisoned"))?;
                    if matches!(tools.get(&address), Some(ToolDeadline::Running { deadline, .. }) if *deadline <= Instant::now()) {
                        return Ok(Some(ToolTimeout { address, title, duration: self.duration }));
                    }
                }
                _ = changed => {},
            }
        }
    }

    /// Retain deadlines only for provider-confirmed background commands after the turn ends.
    pub(crate) fn finish_foreground(&self) -> Result<bool> {
        let mut tools = self
            .tools
            .lock()
            .map_err(|_| anyhow!("tool deadline lock poisoned"))?;
        for state in tools.values_mut() {
            if matches!(
                state,
                ToolDeadline::Running {
                    background: false,
                    ..
                }
            ) {
                *state = ToolDeadline::Finished;
            }
        }
        let pending = tools
            .values()
            .any(|state| matches!(state, ToolDeadline::Running { .. }));
        self.foreground_finished.store(true, Ordering::Release);
        drop(tools);
        self.changed.notify_one();
        Ok(pending)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{
        ExecutionOwner, InvocationScope, TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome,
    };

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

    /// Keep background command deadlines after foreground completion and retire them on success.
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
        assert!(watchdog.expired().await?.is_none());
        Ok(())
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
