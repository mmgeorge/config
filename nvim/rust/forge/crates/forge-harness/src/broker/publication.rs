use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::provider::{ApprovalUpdate, ProviderDiagnostic, ProviderEvent, RuntimeUpdate};

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

pub enum LiveUpdate {
    StateChanged,
    Observation(Box<ProviderEvent>),
    Notification(Box<SessionUpdate>),
}

impl From<ProviderEvent> for LiveUpdate {
    fn from(event: ProviderEvent) -> Self {
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
    ExecutionState {
        session: Box<crate::session::HarnessSession>,
    },
    Submission {
        transition: SubmissionUpdate,
    },
    Approval {
        update: Box<ApprovalUpdate>,
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
        session: Box<crate::session::HarnessSession>,
        access_abbreviation: String,
        operation_id: Option<String>,
        catalog: Box<crate::provider::ProviderCatalogRequest>,
    },
    ConfigurationProgress {
        operation_id: String,
        state: ConfigurationProgressState,
    },
    Diagnostic {
        diagnostic: ProviderDiagnostic,
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
    pub fn from_observation(event: &ProviderEvent) -> Option<Self> {
        match event {
            ProviderEvent::Approval { update, .. } => Some(Self::Approval {
                update: Box::new(update.clone()),
            }),
            ProviderEvent::Runtime {
                scope,
                update: RuntimeUpdate::Resolved { runtime },
            } => Some(Self::Runtime {
                session_id: scope.owner.session_id.to_string(),
                runtime: runtime.clone(),
            }),
            ProviderEvent::Runtime {
                update: RuntimeUpdate::ContextUsage { usage },
                ..
            } => Some(Self::ContextUsage {
                usage: usage.clone(),
            }),
            ProviderEvent::Diagnostic { diagnostic, .. } => Some(Self::Diagnostic {
                diagnostic: diagnostic.clone(),
            }),
            ProviderEvent::Turn {
                address,
                update: crate::provider::TurnUpdate::Started { .. },
            } => Some(Self::TurnStarted {
                address: address.clone(),
            }),
            ProviderEvent::Execution { .. }
            | ProviderEvent::Message { .. }
            | ProviderEvent::Tool { .. }
            | ProviderEvent::Turn { .. }
            | ProviderEvent::Usage { .. }
            | ProviderEvent::Runtime { .. }
            | ProviderEvent::Compaction { .. }
            | ProviderEvent::Agent { .. }
            | ProviderEvent::Checklist { .. }
            | ProviderEvent::DirectoryAccess { .. }
            | ProviderEvent::InputAccepted { .. }
            | ProviderEvent::Retry { .. }
            | ProviderEvent::ParentBoundary { .. } => None,
        }
    }

    pub fn wakes_input(&self) -> bool {
        matches!(self, Self::ExecutionState { .. } | Self::TurnStarted { .. })
    }
}

pub fn encode_session_update(update: &SessionUpdate) -> anyhow::Result<Value> {
    let (kind, text, data) = match update {
        SessionUpdate::ExecutionState { session } => {
            ("execution_state", None, json!({"session":session}))
        }
        SessionUpdate::Submission { transition } => {
            ("prompt_submission", None, serde_json::to_value(transition)?)
        }
        SessionUpdate::Approval { update } => match update.as_ref() {
            ApprovalUpdate::Requested(request) => {
                ("approval_requested", None, serde_json::to_value(request)?)
            }
            ApprovalUpdate::Resolved(request) => {
                ("approval_resolved", None, serde_json::to_value(request)?)
            }
            ApprovalUpdate::Cancelled(request) => {
                ("approval_cancelled", None, serde_json::to_value(request)?)
            }
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
            session,
            access_abbreviation,
            operation_id,
            catalog,
        } => (
            "configuration_applied",
            None,
            json!({"session":session,"access_abbreviation":access_abbreviation,"operation_id":operation_id,"catalog":catalog}),
        ),
        SessionUpdate::ConfigurationProgress {
            operation_id,
            state,
        } => (
            "configuration_progress",
            None,
            json!({"operation_id":operation_id,"state":state}),
        ),
        SessionUpdate::Diagnostic { diagnostic } => (
            "error",
            Some(diagnostic.message.clone()),
            json!({"code":diagnostic.code,"fatal":diagnostic.fatal}),
        ),
        SessionUpdate::CheckFailure { run_id, error } => (
            "checks_configuration_error",
            None,
            json!({"run_id":run_id,"error":error}),
        ),
    };
    Ok(json!({"kind":kind,"text":text,"data":data}))
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

    #[tokio::test]
    async fn flush_waits_for_prior_delivery_and_fails_when_consumer_closes() {
        let (sink, mut stream) = channel();
        sink.send_wait(SessionUpdate::Diagnostic {
            diagnostic: ProviderDiagnostic {
                code: None,
                message: "first".into(),
                fatal: false,
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
