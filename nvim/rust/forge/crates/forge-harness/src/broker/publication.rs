use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::provider::PermissionApprovalEvent;
use crate::provider::{HarnessEvent, ProviderEvent};

impl super::HarnessBroker {
    /// Consume the prompt token once and retain its ordered admission publication.
    pub(crate) fn prepare_prompt_admission(
        &mut self,
        admitted: bool,
    ) -> anyhow::Result<super::AppliedProviderEvent> {
        let Some((document, token)) = self.prompt_admission.take() else {
            return Ok(super::AppliedProviderEvent::None);
        };
        let transition = if let Some(document) = document {
            self.presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .complete_submission(&document, token, admitted)?
        } else {
            Some(
                json!({"document":null,"token":token,"state":if admitted { "accepted" } else { "rejected" }}),
            )
        };
        Ok(match transition {
            Some(transition) => super::AppliedProviderEvent::Update(SessionUpdate::Submission {
                transition: serde_json::from_value(transition)?,
            }),
            None => super::AppliedProviderEvent::None,
        })
    }
}

/// Classify a presentation diagnostic without granting lifecycle authority.
#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DiagnosticSeverity {
    Warning,
    Error,
}
/// Preserve typed failure category and its original operational and native owners.
#[derive(Clone, Debug, Serialize)]
pub struct DiagnosticView {
    pub kind: forge_provider_api::ProviderFailureKind,
    pub message: forge_provider_api::SanitizedMessage,
    pub severity: DiagnosticSeverity,
    pub scope: crate::provider::EventScope,
    pub native_scope: forge_provider_api::WarningScope,
}
impl DiagnosticView {
    /// Present a typed failure at its original native address.
    pub fn failure(
        scope: crate::provider::EventScope,
        native_scope: forge_provider_api::WarningScope,
        failure: &forge_provider_api::ProviderFailure,
    ) -> Self {
        Self {
            kind: failure.kind(),
            message: forge_provider_api::SanitizedMessage::new(failure.to_string())
                .expect("typed failure display is nonempty"),
            severity: DiagnosticSeverity::Error,
            scope,
            native_scope,
        }
    }
}

pub enum LiveUpdate {
    StateChanged,
    Observation(Box<HarnessEvent>),
    Notification(Box<SessionUpdate>),
}

impl From<HarnessEvent> for LiveUpdate {
    fn from(event: HarnessEvent) -> Self {
        Self::Observation(Box::new(event))
    }
}

impl From<SessionUpdate> for LiveUpdate {
    fn from(update: SessionUpdate) -> Self {
        Self::Notification(Box::new(update))
    }
}

#[derive(Clone, Debug)]
pub enum SessionUpdate {
    /// Invalidate check catalogs after a committed check update.
    ChecksChanged,
    /// Publish adapter-owned terminal inventory independently of foreground work.
    Terminals {
        snapshot: forge_provider_api::terminal::TerminalSnapshot,
    },
    ExecutionState {
        snapshot: Box<super::BrokerSnapshot>,
    },
    Submission {
        transition: SubmissionUpdate,
    },
    Approval {
        execution_owner: crate::provider::ThreadOwner,
        execution_kind: crate::provider::ApprovalExecutionKind,
        update: Box<PermissionApprovalEvent>,
    },
    Agent {
        snapshot: super::AgentSnapshot,
    },
    Runtime {
        session_id: String,
        runtime: crate::provider::ProviderRuntime,
    },
    ContextUsage {
        usage: crate::session::ContextUsage,
    },
    /// Publish the committed snapshot after confirmed model-context replacement.
    ContextCompacted {
        snapshot: Box<super::BrokerSnapshot>,
    },
    TurnStarted {
        address: crate::provider::TurnAddress,
    },
    Timeline {
        update: TimelineUpdate,
    },
    PlanProgress {
        progress: PlanProgressUpdate,
    },
    DocumentChanged {
        patch: crate::timeline::stream::TimelinePatch,
    },
    Configuration {
        snapshot: Box<super::BrokerSnapshot>,
        operation_id: Option<String>,
        catalog: Box<crate::provider::ProviderCatalogRequest>,
    },
    ConfigurationProgress {
        operation_id: String,
        state: ConfigurationProgressState,
    },
    /// Publish cleanup observation without claiming execution failure or settlement.
    CleanupProgress {
        operation_id: String,
        policy: forge_provider_api::CleanupPolicy,
        progress: forge_provider_api::CleanupProgress,
    },
    Diagnostic {
        diagnostic: DiagnosticView,
    },
    CheckFailure {
        run_id: String,
        error: String,
    },
}

#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ConfigurationProgressState {
    Applying,
    Verifying,
    Resuming,
}

impl ConfigurationProgressState {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Applying => "applying",
            Self::Verifying => "verifying",
            Self::Resuming => "resuming",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SubmissionUpdate {
    pub document: Option<String>,
    pub token: u64,
    pub state: SubmissionState,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SubmissionState {
    Accepted,
    Rejected,
    Retracted,
}

#[derive(Clone, Debug)]
pub enum TimelineUpdate {
    Node {
        prompt: Option<crate::exchange::ExchangeInput>,
    },
    Wait {
        exchange_id: String,
        wait: Option<crate::exchange::ActiveWait>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanProgressUpdate {
    pub execution_id: String,
    pub phase: crate::plan::PlanPhase,
    pub revision: u32,
    pub progress: crate::plan::execution::SemanticProgress,
}

impl SessionUpdate {
    /// Project already attributed native and host observations into editor notifications.
    pub fn from_observation(observation: &HarnessEvent) -> Option<Self> {
        use crate::provider::HostEvent;
        use forge_provider_api::{ProviderConnectionEvent, ThreadEvent, TurnEvent};
        match observation {
            HarnessEvent::Native { scope, event, .. } => match event.as_ref() {
                ProviderEvent::Provider { event } => match event {
                    ProviderConnectionEvent::CapabilitiesChanged(_) => None,
                    ProviderConnectionEvent::Warning(warning) => Some(Self::Diagnostic {
                        diagnostic: DiagnosticView {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: warning.message.clone(),
                            severity: DiagnosticSeverity::Warning,
                            scope: scope.clone(),
                            native_scope: warning.scope.clone(),
                        },
                    }),
                    ProviderConnectionEvent::Unavailable(unavailable) => Some(Self::Diagnostic {
                        diagnostic: DiagnosticView {
                            kind: unavailable.failure.kind(),
                            message: unavailable.message.clone(),
                            severity: DiagnosticSeverity::Error,
                            scope: scope.clone(),
                            native_scope: forge_provider_api::WarningScope::Provider,
                        },
                    }),
                },
                ProviderEvent::Thread { thread_id, event } => match event {
                    ThreadEvent::Error { error } => Some(Self::Diagnostic {
                        diagnostic: DiagnosticView::failure(
                            scope.clone(),
                            forge_provider_api::WarningScope::Thread {
                                thread_id: thread_id.clone(),
                            },
                            error,
                        ),
                    }),
                    ThreadEvent::TerminalsUpdated(Ok(snapshot)) => Some(Self::Terminals {
                        snapshot: snapshot.clone(),
                    }),
                    ThreadEvent::TerminalsUpdated(Err(error)) => Some(Self::Diagnostic {
                        diagnostic: DiagnosticView::failure(
                            scope.clone(),
                            forge_provider_api::WarningScope::Thread {
                                thread_id: thread_id.clone(),
                            },
                            error,
                        ),
                    }),
                    ThreadEvent::ContextUsage(usage) => Some(Self::ContextUsage {
                        usage: usage.clone(),
                    }),
                    ThreadEvent::IdentityResolved(_) | ThreadEvent::CompactionUpdated(_) => None,
                },
                ProviderEvent::Turn {
                    thread_id,
                    turn_id,
                    event,
                } => match event {
                    TurnEvent::Started(_) => {
                        scope.invocation().map(|invocation| Self::TurnStarted {
                            address: crate::provider::TurnAddress {
                                scope: invocation.clone(),
                                thread_id: thread_id.clone(),
                                turn_id: turn_id.clone(),
                            },
                        })
                    }
                    TurnEvent::Error { error } => Some(Self::Diagnostic {
                        diagnostic: DiagnosticView::failure(
                            scope.clone(),
                            forge_provider_api::WarningScope::Turn {
                                thread_id: thread_id.clone(),
                                turn_id: turn_id.clone(),
                            },
                            error,
                        ),
                    }),
                    TurnEvent::Completed(_)
                    | TurnEvent::ModelResolved(_)
                    | TurnEvent::Usage(_)
                    | TurnEvent::ChecklistUpdated(_) => None,
                },
                ProviderEvent::Item { .. } | ProviderEvent::Agent { .. } => None,
            },
            HarnessEvent::Host { scope, event } => match event {
                HostEvent::PermissionApproval {
                    execution_kind,
                    event,
                } => Some(Self::Approval {
                    execution_owner: scope.owner().clone(),
                    execution_kind: *execution_kind,
                    update: Box::new(event.clone()),
                }),
                HostEvent::Failure { failure, message } => Some(Self::Diagnostic {
                    diagnostic: DiagnosticView {
                        kind: failure.kind(),
                        message: message.clone(),
                        severity: DiagnosticSeverity::Error,
                        scope: scope.clone(),
                        native_scope: forge_provider_api::WarningScope::Provider,
                    },
                }),
                HostEvent::DirectoryAccessGranted(_)
                | HostEvent::CapabilitiesNegotiated { .. }
                | HostEvent::RetryUpdated { .. }
                | HostEvent::ChildWait { .. }
                | HostEvent::SkillRead { .. } => None,
            },
        }
    }

    pub fn wakes_input(&self) -> bool {
        matches!(self, Self::ExecutionState { .. } | Self::TurnStarted { .. })
    }
}

/// Add immutable host control ownership to the canonical permission payload.
fn approval_payload(
    owner: &crate::provider::ThreadOwner,
    execution_kind: crate::provider::ApprovalExecutionKind,
    request: &impl Serialize,
) -> Result<Value, serde_json::Error> {
    let mut value = serde_json::to_value(request)?;
    value["execution_owner"] = serde_json::to_value(owner)?;
    value["execution_kind"] = serde_json::to_value(execution_kind)?;
    Ok(value)
}

pub fn encode_session_update(update: &SessionUpdate) -> Result<Value, serde_json::Error> {
    let (kind, text, data) = match update {
        SessionUpdate::ChecksChanged => ("checks_changed", None, json!({})),
        SessionUpdate::Terminals { snapshot } => {
            ("terminals_changed", None, serde_json::to_value(snapshot)?)
        }
        SessionUpdate::ExecutionState { snapshot } => {
            ("execution_state", None, json!({"snapshot":snapshot}))
        }
        SessionUpdate::Submission { transition } => {
            ("prompt_submission", None, serde_json::to_value(transition)?)
        }
        SessionUpdate::Approval {
            execution_owner,
            execution_kind,
            update,
        } => match update.as_ref() {
            PermissionApprovalEvent::Requested(request) => (
                "approval_requested",
                None,
                approval_payload(execution_owner, *execution_kind, request)?,
            ),
            PermissionApprovalEvent::Resolved(request) => (
                "approval_resolved",
                None,
                approval_payload(execution_owner, *execution_kind, request)?,
            ),
            PermissionApprovalEvent::Cancelled(request) => (
                "approval_cancelled",
                None,
                approval_payload(execution_owner, *execution_kind, request)?,
            ),
        },
        SessionUpdate::Agent { snapshot } => {
            ("agent_updated", None, serde_json::to_value(snapshot)?)
        }
        SessionUpdate::Runtime {
            session_id,
            runtime,
        } => (
            "runtime_resolved",
            None,
            json!({"session_id":session_id,"provider":runtime.provider,"model":runtime.model}),
        ),
        SessionUpdate::ContextUsage { usage } => {
            ("context_usage", None, serde_json::to_value(usage)?)
        }
        SessionUpdate::ContextCompacted { snapshot } => {
            ("context_compacted", None, serde_json::to_value(snapshot)?)
        }
        SessionUpdate::TurnStarted { address } => {
            ("turn_started", None, serde_json::to_value(address)?)
        }
        SessionUpdate::Timeline { update } => match update {
            TimelineUpdate::Node { prompt } => (
                "timeline_node_updated",
                None,
                json!({"node":{"prompt":prompt}}),
            ),
            TimelineUpdate::Wait { exchange_id, wait } => (
                "timeline_wait_updated",
                None,
                json!({"exchange_id":exchange_id,"wait":wait}),
            ),
        },
        SessionUpdate::PlanProgress { progress } => (
            "plan_execution_progress",
            None,
            serde_json::to_value(progress)?,
        ),
        SessionUpdate::DocumentChanged { patch } => return Ok(patch.notification()),
        SessionUpdate::Configuration {
            snapshot,
            operation_id,
            catalog: _,
        } => (
            "configuration_applied",
            None,
            json!({"snapshot":snapshot,"operation_id":operation_id}),
        ),
        SessionUpdate::ConfigurationProgress {
            operation_id,
            state,
        } => (
            "configuration_progress",
            None,
            json!({"operation_id":operation_id,"state":state}),
        ),
        SessionUpdate::CleanupProgress {
            operation_id,
            policy,
            progress,
        } => (
            "cleanup_progress",
            Some(policy.message(*progress).to_owned()),
            json!({
                "operation_id": operation_id,
                "policy": match policy {
                    forge_provider_api::CleanupPolicy::Cancellation => "cancellation",
                    forge_provider_api::CleanupPolicy::Close => "close",
                },
                "state": match progress {
                    forge_provider_api::CleanupProgress::Pending => "pending",
                    forge_provider_api::CleanupProgress::TimedOut => "timed_out",
                },
                "message": policy.message(*progress),
            }),
        ),
        SessionUpdate::Diagnostic { diagnostic } => (
            "error",
            Some(diagnostic.message.to_string()),
            json!({"kind":diagnostic.kind,"severity":diagnostic.severity,"scope":diagnostic.scope,"native_scope":diagnostic.native_scope}),
        ),
        SessionUpdate::CheckFailure { run_id, error } => (
            "checks_configuration_error",
            None,
            json!({"run_id":run_id,"error":error}),
        ),
    };
    Ok(json!({"kind":kind,"text":text,"data":data}))
}

/// Translate one committed update into its client event without parsing its encoded kind.
pub(crate) fn encode_session_event(
    session: &str,
    update: &SessionUpdate,
) -> Result<forge_protocol::message::SessionEvent, serde_json::Error> {
    let payload = encode_session_update(update)?;
    let (event, payload) = match update {
        SessionUpdate::ContextCompacted { .. } => ("context_compacted", payload["data"].clone()),
        SessionUpdate::ExecutionState { .. } => ("session_changed", payload["data"].clone()),
        SessionUpdate::Configuration { .. } => ("session_configured", payload["data"].clone()),
        SessionUpdate::DocumentChanged { .. } => ("document_changed", payload),
        SessionUpdate::ChecksChanged => ("checks_changed", json!({})),
        SessionUpdate::Terminals { .. }
        | SessionUpdate::Submission { .. }
        | SessionUpdate::Approval { .. }
        | SessionUpdate::Agent { .. }
        | SessionUpdate::Runtime { .. }
        | SessionUpdate::ContextUsage { .. }
        | SessionUpdate::TurnStarted { .. }
        | SessionUpdate::Timeline { .. }
        | SessionUpdate::PlanProgress { .. }
        | SessionUpdate::ConfigurationProgress { .. }
        | SessionUpdate::CleanupProgress { .. }
        | SessionUpdate::Diagnostic { .. }
        | SessionUpdate::CheckFailure { .. } => ("provider_event", payload),
    };
    Ok(forge_protocol::message::SessionEvent {
        session_id: session.to_owned(),
        event: event.to_owned(),
        payload,
    })
}

#[derive(Clone)]
pub struct SessionUpdateSink(tokio::sync::mpsc::Sender<SessionPublication>);

pub struct SessionUpdateStream(tokio::sync::mpsc::Receiver<SessionPublication>);

enum SessionPublication {
    Update(SessionUpdate),
    Flush(tokio::sync::oneshot::Sender<()>),
}

pub fn channel() -> (SessionUpdateSink, SessionUpdateStream) {
    let (sender, receiver) = tokio::sync::mpsc::channel(32);
    (SessionUpdateSink(sender), SessionUpdateStream(receiver))
}

impl SessionUpdateSink {
    pub async fn send_wait(&self, update: SessionUpdate) -> Result<(), std::io::Error> {
        self.0
            .send(SessionPublication::Update(update))
            .await
            .map_err(|_| {
                std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "session notification receiver closed",
                )
            })
    }

    /// Wait until the consumer has processed all preceding updates.
    pub async fn flush(&self) -> Result<(), std::io::Error> {
        let (reply, received) = tokio::sync::oneshot::channel();
        self.0
            .send(SessionPublication::Flush(reply))
            .await
            .map_err(|_| {
                std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "session notification receiver closed",
                )
            })?;
        received.await.map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "session notification flush was not consumed",
            )
        })
    }

    pub async fn failed(&self) -> std::io::Error {
        self.0.closed().await;
        std::io::Error::new(
            std::io::ErrorKind::BrokenPipe,
            "session notification receiver closed",
        )
    }
}

impl SessionUpdateStream {
    /// Stop observation admission while retaining the accepted prefix for draining.
    /// This closes only the publication receiver; native execution and cleanup keep their owners.
    pub(crate) fn close(&mut self) {
        self.0.close();
    }

    pub async fn recv(&mut self) -> Result<Option<SessionUpdate>, std::io::Error> {
        loop {
            match self.0.recv().await {
                Some(SessionPublication::Update(update)) => return Ok(Some(update)),
                Some(SessionPublication::Flush(reply)) => {
                    let _ = reply.send(());
                }
                None => return Ok(None),
            }
        }
    }

    pub fn try_recv(&mut self) -> Result<SessionUpdate, std::io::Error> {
        loop {
            match self.0.try_recv() {
                Ok(SessionPublication::Update(update)) => return Ok(update),
                Ok(SessionPublication::Flush(reply)) => {
                    let _ = reply.send(());
                }
                Err(error) => {
                    return Err({
                        std::io::Error::new(
                            match error {
                                tokio::sync::mpsc::error::TryRecvError::Empty => {
                                    std::io::ErrorKind::WouldBlock
                                }
                                tokio::sync::mpsc::error::TryRecvError::Disconnected => {
                                    std::io::ErrorKind::UnexpectedEof
                                }
                            },
                            "session notification queue has no available update",
                        )
                    });
                }
            }
        }
    }
}

pub async fn failed(sink: Option<&SessionUpdateSink>) -> std::io::Error {
    match sink {
        Some(sink) => sink.failed().await,
        None => std::future::pending().await,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Thread error presentation does not claim turn failure or connection unavailability.
    #[test]
    fn thread_error_presentation_retains_nonterminal_category() {
        let owner = forge_provider_api::ThreadOwner::root("thread-error");
        let event = crate::provider::test_support::native_event(
            crate::provider::EventScope::Binding {
                owner: owner.clone(),
            },
            ProviderEvent::Thread {
                thread_id: owner.thread_id(),
                event: forge_provider_api::ThreadEvent::Error {
                    error: forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::RateLimited,
                    ),
                },
            },
        );
        assert!(
            matches!(SessionUpdate::from_observation(&event), Some(SessionUpdate::Diagnostic { diagnostic })
            if diagnostic.kind == forge_provider_api::ProviderFailureKind::RateLimited && matches!(diagnostic.severity, DiagnosticSeverity::Error))
        );
    }

    #[tokio::test]
    async fn flush_waits_for_prior_delivery_and_fails_when_consumer_closes() {
        let (sink, mut stream) = channel();
        sink.send_wait(SessionUpdate::Diagnostic {
            diagnostic: DiagnosticView {
                kind: forge_provider_api::ProviderFailureKind::Unknown,
                message: forge_provider_api::SanitizedMessage::new("first")
                    .expect("fixture diagnostic is nonempty"),
                severity: DiagnosticSeverity::Warning,
                scope: crate::provider::EventScope::Binding {
                    owner: crate::provider::ThreadOwner::root("publication-fixture"),
                },
                native_scope: forge_provider_api::WarningScope::Provider,
            },
        })
        .await
        .unwrap();
        let flush_sink = sink.clone();
        let mut flush = tokio::spawn(async move { flush_sink.flush().await });
        assert!(matches!(
            stream.recv().await.unwrap(),
            Some(SessionUpdate::Diagnostic { .. })
        ));
        assert!(!flush.is_finished());
        let next = tokio::spawn(async move { stream.recv().await });
        tokio::time::timeout(std::time::Duration::from_secs(1), &mut flush)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        next.abort();

        let (sink, stream) = channel();
        drop(stream);
        assert_eq!(
            sink.flush().await.unwrap_err().kind(),
            std::io::ErrorKind::BrokenPipe
        );
    }
}
