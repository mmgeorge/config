use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::backend::{ApprovalUpdate, BackendEvent, ProviderDiagnostic, RuntimeUpdate};

pub enum LiveUpdate {
    StateChanged,
    Observation(Box<BackendEvent>),
    Notification(Box<SessionUpdate>),
}

impl From<BackendEvent> for LiveUpdate {
    fn from(event: BackendEvent) -> Self {
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
        runtime: crate::backend::BackendRuntime,
    },
    ContextUsage {
        usage: crate::session::ContextUsage,
    },
    TurnStarted {
        address: crate::backend::TurnAddress,
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
        catalog: Box<crate::backend::BackendCatalogRequest>,
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
    pub fn from_observation(event: &BackendEvent) -> Option<Self> {
        match event {
            BackendEvent::Approval { update, .. } => Some(Self::Approval {
                update: Box::new(update.clone()),
            }),
            BackendEvent::Runtime {
                scope,
                update: RuntimeUpdate::Resolved { runtime },
            } => Some(Self::Runtime {
                session_id: scope.owner.session_id.to_string(),
                runtime: runtime.clone(),
            }),
            BackendEvent::Runtime {
                update: RuntimeUpdate::ContextUsage { usage },
                ..
            } => Some(Self::ContextUsage {
                usage: usage.clone(),
            }),
            BackendEvent::Diagnostic { diagnostic, .. } => Some(Self::Diagnostic {
                diagnostic: diagnostic.clone(),
            }),
            BackendEvent::Turn {
                address,
                update: crate::backend::TurnUpdate::Started { .. },
            } => Some(Self::TurnStarted {
                address: address.clone(),
            }),
            BackendEvent::Message { .. }
            | BackendEvent::Tool { .. }
            | BackendEvent::Turn { .. }
            | BackendEvent::Usage { .. }
            | BackendEvent::Runtime { .. }
            | BackendEvent::Compaction { .. }
            | BackendEvent::Agent { .. }
            | BackendEvent::Checklist { .. }
            | BackendEvent::DirectoryAccess { .. }
            | BackendEvent::InputAccepted { .. }
            | BackendEvent::Retry { .. }
            | BackendEvent::ParentBoundary { .. } => None,
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
pub struct SessionUpdateSink(tokio::sync::mpsc::Sender<SessionUpdate>);

pub struct SessionUpdateStream(tokio::sync::mpsc::Receiver<SessionUpdate>);

pub fn channel() -> (SessionUpdateSink, SessionUpdateStream) {
    let (sender, receiver) = tokio::sync::mpsc::channel(32);
    (SessionUpdateSink(sender), SessionUpdateStream(receiver))
}

impl SessionUpdateSink {
    pub async fn send_wait(&self, update: SessionUpdate) -> Result<(), std::io::Error> {
        self.0.send(update).await.map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "session notification receiver closed",
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
        Ok(self.0.recv().await)
    }

    pub fn try_recv(&mut self) -> Result<SessionUpdate, std::io::Error> {
        self.0.try_recv().map_err(|error| {
            std::io::Error::new(
                match error {
                    tokio::sync::mpsc::error::TryRecvError::Empty => std::io::ErrorKind::WouldBlock,
                    tokio::sync::mpsc::error::TryRecvError::Disconnected => {
                        std::io::ErrorKind::UnexpectedEof
                    }
                },
                "session notification queue has no available update",
            )
        })
    }
}

pub async fn failed(sink: Option<&SessionUpdateSink>) -> std::io::Error {
    match sink {
        Some(sink) => sink.failed().await,
        None => std::future::pending().await,
    }
}
