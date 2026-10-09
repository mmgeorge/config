use crate::session::PermissionMode;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
/// Identifies the workflow independently of its permission boundary.
pub enum TaskKind {
    /// Creates a reviewable design.
    Plan,
    /// Implements an accepted design through its verification gates.
    Execute,
    /// Continues toward an explicit objective.
    Goal,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
/// Records whether a workflow can admit another attempt.
pub enum TaskStatus {
    /// Persisted before its first attempt starts.
    Ready,
    /// Owns an admitted provider attempt.
    Running,
    /// Collects an interrupted attempt before another can start.
    Stopping,
    /// Requires an explicit resume intent.
    Paused,
    /// Requires an answer or plan review.
    Waiting,
    /// Cannot advance without resolving a recorded constraint.
    Blocked,
    /// Satisfied its completion gates and cannot resume.
    Completed,
    /// Was cleared and cannot resume.
    Cancelled,
    /// Retains a failure for inspection before explicit retry.
    Failed,
}

impl TaskStatus {
    /// Terminal tasks retain history but cannot admit new attempts.
    pub fn terminal(self) -> bool {
        matches!(self, Self::Completed | Self::Cancelled)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Persists the selected workflow and its resumption boundary.
pub struct TaskRecord {
    /// Stable workflow identity across attempts and Plan-to-Execute acceptance.
    pub id: String,
    /// Conversation that owns this workflow.
    pub session_id: String,
    /// User-facing objective or plan title.
    pub title: String,
    /// Workflow independent of permissions.
    pub kind: TaskKind,
    /// Admission and recovery state.
    pub status: TaskStatus,
    /// Saved workflow checkpoint for resumption.
    pub phase: String,
    /// Explanation of the current pause, block, or failure.
    pub reason: Option<String>,
    /// Associated design artifact when planning or executing.
    pub plan_id: Option<String>,
    /// Associated continuation evidence when executing or pursuing a goal.
    pub goal_id: Option<String>,
    /// Last permission selected for this workflow.
    pub permission: PermissionMode,
    /// Writable permission restored when resuming Execute or Goal.
    pub last_write_permission: PermissionMode,
    /// Revision used to reject stale picker selections.
    pub generation: u64,
    /// Identity of the latest admitted attempt.
    pub attempt_id: Option<String>,
    /// Durable intent that selected the current attempt.
    pub operation_id: Option<String>,
    /// Creation time in Unix milliseconds.
    pub created_at_ms: i64,
    /// Latest lifecycle update in Unix milliseconds.
    pub updated_at_ms: i64,
}

impl TaskRecord {
    /// Creates a workflow before any provider dispatch occurs.
    pub fn new(
        session_id: String,
        title: String,
        kind: TaskKind,
        permission: PermissionMode,
        now_ms: i64,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            session_id,
            title,
            kind,
            status: TaskStatus::Ready,
            phase: match kind {
                TaskKind::Plan => "draft",
                TaskKind::Execute => "implement",
                TaskKind::Goal => "continue",
            }
            .into(),
            reason: None,
            plan_id: None,
            goal_id: None,
            permission,
            last_write_permission: if permission.is_write_default() {
                permission
            } else {
                PermissionMode::Write
            },
            generation: 0,
            attempt_id: None,
            operation_id: None,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        }
    }

    /// Advances the attempt fence before invoking an external provider.
    pub fn begin(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            !self.status.terminal(),
            "task is terminal. Fork its plan or create a new task"
        );
        self.generation += 1;
        self.attempt_id = Some(uuid::Uuid::new_v4().to_string());
        self.status = TaskStatus::Running;
        self.reason = None;
        self.updated_at_ms = now_ms;
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Identifies a durable user intent, including uncertain delivery outcomes.
pub struct TaskOperation {
    /// Client-generated idempotency key retained after uncertain delivery.
    pub id: String,
    /// Conversation receiving the intent.
    pub session_id: String,
    /// Immutable submitted action used to validate duplicate identities.
    pub action: serde_json::Value,
    /// Durable admission, execution, or settlement outcome.
    pub state: String,
    /// Failure detail retained for reconnect and inspection.
    pub error: Option<String>,
    /// Admission time in Unix milliseconds.
    pub created_at_ms: i64,
    #[serde(default)]
    /// Retains running intent across successive settings changes during cleanup.
    pub resume_after_transition: bool,
}

impl TaskOperation {
    /// Projects bounded control status without retransmitting the submitted prompt.
    pub fn view(&self) -> serde_json::Value {
        serde_json::json!({
            "id": self.id, "state": self.state,
            "action": self.action.get("action"),
            "error": self.error.as_ref().map(|error| error.chars().take(4096).collect::<String>()),
        })
    }
}
