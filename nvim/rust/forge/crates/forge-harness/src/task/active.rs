use anyhow::Result;
use serde::{Deserialize, Serialize};

use crate::goal::GoalState;
use crate::plan::{PlanExecutionRecord, PlanRecord};
use crate::session::continuation::ContinuationBudget;

/// Identifies one admitted task contract, independently of exchange and provider identities.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
pub struct TaskRevision(String);

/// Identifies one task across phase transitions, workflow changes, and resumes.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
pub struct TaskId(String);

impl TaskId {
    /// Create a distinct identity for a newly installed task.
    pub fn fresh() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }

    /// Get the stable serialized task identity.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TaskRevision {
    pub fn fresh() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Carries the only selected workflow in a session. Transcript entries do not own this state.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TaskSnapshot {
    pub id: TaskId,
    pub revision: TaskRevision,
    /// Fence turn accounting across user control and explicit recovery, but not phase handoffs.
    pub(crate) observation_revision: TaskRevision,
    /// Accept complete plan submissions until the user disables this task policy.
    pub auto_accept_plan: bool,
    /// Preserve execution choices independently of automatic acceptance.
    pub plan_execution: Option<PlanExecutionChoice>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<TaskEntry>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub turn: Option<TaskTurn>,
    /// Deduplicate model observations independently of local-work reservation source turns.
    pub observed_turn: Option<TaskTurnId>,
    #[serde(flatten)]
    pub workflow: WorkflowSnapshot,
}

/// Own the only mutable task lifecycle; adapter snapshots expose no mutable borrow.
#[derive(Clone, Debug, Deserialize)]
#[serde(try_from = "StoredTask")]
pub struct Task {
    lifecycle: super::state::TaskLifecycle,
}

/// Validate the persisted lifecycle summary against its complete phase payload.
#[derive(Deserialize)]
struct StoredTask {
    #[serde(flatten)]
    draft: TaskSnapshot,
    phase: super::TaskPhase,
    status: super::TaskStatus,
}

impl TryFrom<StoredTask> for Task {
    type Error = anyhow::Error;

    fn try_from(stored: StoredTask) -> Result<Self> {
        let StoredTask {
            draft,
            phase,
            status,
        } = stored;
        let task = Self::try_from(draft)?;
        anyhow::ensure!(
            task.phase() == phase && task.status() == status,
            "persisted task phase or lifecycle contradicts its workflow payload"
        );
        Ok(task)
    }
}

impl Task {
    /// Create a goal with its own stable identity and continuation allowance.
    pub fn goal(objective: String, continuation: ContinuationBudget, now_ms: i64) -> Self {
        Self {
            lifecycle: super::state::TaskLifecycle::from_snapshot(TaskSnapshot::goal(
                objective,
                continuation,
                now_ms,
            )),
        }
    }

    /// Create a planning task around its canonical artifact.
    pub fn plan(artifact: PlanRecord, now_ms: i64) -> Result<Self> {
        Self::try_from(TaskSnapshot::plan(artifact, now_ms))
    }

    /// Create an execution task bound to an accepted artifact revision.
    pub fn execute(
        title: String,
        record: PlanExecutionRecord,
        continuation: ContinuationBudget,
        now_ms: i64,
    ) -> Result<Self> {
        Self::try_from(TaskSnapshot::execute(title, record, continuation, now_ms))
    }

    /// Get the selected lifecycle without exposing mutation of its payload.
    pub fn lifecycle(&self) -> &super::state::TaskLifecycle {
        &self.lifecycle
    }

    /// Get the phase preserved by active, suspended, and finished tasks.
    pub fn phase(&self) -> super::state::TaskPhase {
        self.lifecycle.workflow().phase()
    }

    /// Get the lifecycle condition from the canonical state.
    pub fn status(&self) -> super::TaskStatus {
        self.lifecycle.status()
    }

    /// Derive the next operation from the committed workflow and lifecycle.
    pub(crate) fn next_action(&self) -> super::state::TaskNextAction {
        self.lifecycle.next_action()
    }

    /// Create an owned adapter draft; edits cannot affect committed state.
    pub(crate) fn draft(&self) -> TaskSnapshot {
        self.lifecycle.snapshot().clone()
    }

    /// Validate a complete candidate before replacing the current task value.
    pub(crate) fn edit<Output>(
        &mut self,
        edit: impl FnOnce(&mut TaskSnapshot) -> Result<Output>,
    ) -> Result<Output> {
        let mut draft = self.draft();
        let output = edit(&mut draft)?;
        *self = Self::try_from(draft)?;
        Ok(output)
    }

    /// Renew control admission while retaining stable task identity and phase.
    pub fn renew(&mut self, now_ms: i64) {
        let mut draft = self.draft();
        draft.renew(now_ms);
        self.lifecycle = super::state::TaskLifecycle::from_snapshot(draft);
    }

    /// Reserve a provider entry without exposing task internals.
    pub(crate) fn request_entry(
        &mut self,
        instruction: forge_provider_api::forge::TurnInput,
        now_ms: i64,
    ) -> Result<()> {
        self.edit(|draft| draft.request_entry(instruction, now_ms))
    }
}

impl std::ops::Deref for Task {
    type Target = TaskSnapshot;

    fn deref(&self) -> &Self::Target {
        self.lifecycle.snapshot()
    }
}

impl TryFrom<TaskSnapshot> for Task {
    type Error = anyhow::Error;

    fn try_from(draft: TaskSnapshot) -> Result<Self> {
        super::state::validate_snapshot(&draft)?;
        anyhow::ensure!(
            !draft.id.as_str().trim().is_empty(),
            "task identity is required"
        );
        anyhow::ensure!(
            !draft.revision.as_str().trim().is_empty(),
            "task revision is required"
        );
        if let Some(entry) = &draft.entry {
            anyhow::ensure!(
                !entry.instruction.label().trim().is_empty(),
                "provider entry instruction is required"
            );
        }
        if let Some(input) = draft
            .turn
            .as_ref()
            .and_then(|turn| turn.continuation.as_ref())
        {
            anyhow::ensure!(
                input
                    .work
                    .instruction()
                    .is_none_or(|instruction| !instruction.label().trim().is_empty()),
                "provider continuation instruction is required"
            );
            anyhow::ensure!(
                input.work != TaskWork::Checks
                    || matches!(&draft.workflow, WorkflowSnapshot::Execute { execution } if execution.record.phase == crate::plan::PlanPhase::Checking)
                    || input.delivery != TaskInputDelivery::Pending,
                "pending local checks require the Check phase"
            );
        }
        Ok(Self {
            lifecycle: super::state::TaskLifecycle::from_snapshot(draft),
        })
    }
}

impl Serialize for Task {
    fn serialize<Serializer: serde::Serializer>(
        &self,
        serializer: Serializer,
    ) -> std::result::Result<Serializer::Ok, Serializer::Error> {
        #[derive(Serialize)]
        struct TaskView<'task> {
            #[serde(flatten)]
            snapshot: &'task TaskSnapshot,
            phase: super::state::TaskPhase,
            status: super::TaskStatus,
            #[serde(skip_serializing_if = "Option::is_none")]
            suspension_reason: Option<&'task super::state::SuspensionReason>,
        }
        TaskView {
            snapshot: self.lifecycle.snapshot(),
            phase: self.phase(),
            status: self.status(),
            suspension_reason: match &self.lifecycle {
                super::state::TaskLifecycle::Suspended(task) => Some(task.reason()),
                _ => None,
            },
        }
        .serialize(serializer)
    }
}

/// Identifies a task-generated provider input across delivery retries.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct TaskInputId(String);

impl TaskInputId {
    /// Create a distinct identity for one task-generated input.
    pub fn fresh() -> Self {
        Self(uuid::Uuid::new_v4().to_string())
    }
    /// Get the opaque input identity for persistence and correlation.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for TaskInputId {
    type Error = TaskInputIdentityError;
    /// Reject missing authority while loading task/input journals or caller targets.
    fn try_from(value: String) -> std::result::Result<Self, Self::Error> {
        if value.trim().is_empty() {
            return Err(TaskInputIdentityError);
        }
        Ok(Self(value))
    }
}

impl From<TaskInputId> for String {
    /// Persist an already-validated input identity without changing its spelling.
    fn from(value: TaskInputId) -> Self {
        value.0
    }
}

/// Reject an empty task-generated input identity without retaining arbitrary supplied text.
#[derive(Debug, thiserror::Error)]
#[error("task input identity must be nonempty")]
pub struct TaskInputIdentityError;

/// Orders completed root turns without retaining prior exchanges in task state.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct TaskTurnId {
    pub exchange_ordinal: u64,
    pub turn_ordinal: usize,
}

/// Retains the latest observed turn and its durable continuation delivery intent.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskTurn {
    pub id: TaskTurnId,
    pub continuation: Option<TaskContinuation>,
}

/// Owns the exact instruction and provider acknowledgement for one continuation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskContinuation {
    pub id: TaskInputId,
    pub reason: String,
    pub delivery: TaskInputDelivery,
    pub work: TaskWork,
}

/// Bind provider instructions to provider work; local checks carry no fake prompt.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TaskWork {
    Provider {
        instruction: forge_provider_api::forge::TurnInput,
    },
    Checks,
}

impl TaskWork {
    /// Get the typed provider input only when this operation admits a provider turn.
    pub fn instruction(&self) -> Option<&forge_provider_api::forge::TurnInput> {
        match self {
            Self::Provider { instruction } => Some(instruction),
            Self::Checks => None,
        }
    }

    /// Identify local work without an independently mutable operation discriminator.
    pub fn is_checks(&self) -> bool {
        matches!(self, Self::Checks)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskEntry {
    pub id: TaskInputId,
    /// Typed provider input that starts or resumes the task.
    pub instruction: forge_provider_api::forge::TurnInput,
    pub delivery: TaskInputDelivery,
    /// Keep review feedback and its revision turns in their originating exchange.
    pub review_exchange: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution: Option<ExecutionEntry>,
    /// A resumed entry continues an already announced task instead of starting it.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub resumed: bool,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ExecutionEntry {
    pub approval_mode: crate::session::ApprovalMode,
    pub executor: crate::session::PlanExecutor,
    pub compact: bool,
}

/// Retain the user's execution settings and review context during planning.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanExecutionChoice {
    /// Settings selected for entering implementation.
    pub execution: ExecutionEntry,
    /// Reviewer context carried into execution prompts.
    pub comment: Option<String>,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskInputDelivery {
    Pending,
    Accepted,
    Cancelled,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum WorkflowSnapshot {
    Goal { goal: GoalSnapshot },
    Plan { plan: Box<PlanSnapshot> },
    Execute { execution: Box<ExecutionSnapshot> },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GoalSnapshot {
    pub objective: String,
    pub state: GoalState,
    pub continuation: ContinuationBudget,
    /// Compare actual workspace content instead of treating tool activity as progress.
    pub observed_checkpoint: Option<String>,
    /// Preserve the latest explicit explanation for a blocked logical goal.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reported_status: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanSnapshot {
    pub artifact: PlanRecord,
    pub paused: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExecutionSnapshot {
    pub title: String,
    pub record: PlanExecutionRecord,
    pub continuation: ContinuationBudget,
}

/// Defines the next action after provider execution settles under the current task contract.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TaskDecision {
    Continue { instruction: String, reason: String },
    Wait { reason: String },
    Advance { instruction: String, reason: String },
    Complete { summary: String },
    Block { reason: String },
}

/// Retains a terminal task result without retaining a task owner in the transcript.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TaskOutcome {
    pub id: TaskId,
    pub kind: super::TaskKind,
    pub title: String,
    pub state: TaskOutcomeState,
    pub summary: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskOutcomeState {
    Complete,
    Cancelled,
}

/// Binds a task-specific control call to the contract that admitted it.
#[derive(Clone, Debug)]
pub struct TaskPermit {
    id: TaskId,
    revision: TaskRevision,
    observation_revision: TaskRevision,
}

/// Identify the task-owned input whose real native receipt is being observed.
#[derive(Clone, Copy, Debug)]
pub(crate) enum TaskReceiptInput {
    Entry,
    Continuation,
}

/// Persist the observation owner of an input receipt across legal task revisions.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct TaskReceiptPermit {
    id: TaskId,
    observation_revision: TaskRevision,
}

impl TaskReceiptPermit {
    /// Resolve only against the same task generation; user control invalidates this proof.
    pub(crate) fn permit_for(&self, task: &Task) -> Result<TaskPermit> {
        if task.id != self.id || task.observation_revision != self.observation_revision {
            return Err(StaleTask.into());
        }
        Ok(task.permit())
    }
}

#[derive(Debug)]
pub(crate) struct StaleTask;

impl std::fmt::Display for StaleTask {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("task changed before the control operation completed")
    }
}

impl std::error::Error for StaleTask {}

impl TaskPermit {
    /// Retain only the immutable authority needed to observe this input's later receipt.
    pub(crate) fn receipt(&self) -> TaskReceiptPermit {
        TaskReceiptPermit {
            id: self.id.clone(),
            observation_revision: self.observation_revision.clone(),
        }
    }

    /// Attribute a completed model turn after its own legal phase transition.
    /// User control rotates this fence, so a late completion cannot affect resumed work.
    pub(crate) fn validate_observation(&self, task: Option<&Task>) -> Result<()> {
        if task.is_none_or(|task| {
            task.id != self.id || task.observation_revision != self.observation_revision
        }) {
            return Err(StaleTask.into());
        }
        Ok(())
    }

    /// Get the stable owner when an admitted turn advances its task revision.
    pub(crate) fn task_id(&self) -> &TaskId {
        &self.id
    }
    pub(crate) fn revision(&self) -> &TaskRevision {
        &self.revision
    }

    pub fn validate(&self, task: Option<&Task>) -> Result<()> {
        if task.is_none_or(|task| task.id != self.id || task.revision != self.revision) {
            return Err(StaleTask.into());
        }
        Ok(())
    }
}

impl TaskSnapshot {
    pub fn goal(objective: String, continuation: ContinuationBudget, now_ms: i64) -> Self {
        Self {
            id: TaskId::fresh(),
            revision: TaskRevision::fresh(),
            observation_revision: TaskRevision::fresh(),
            auto_accept_plan: false,
            plan_execution: None,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            observed_turn: None,
            workflow: WorkflowSnapshot::Goal {
                goal: GoalSnapshot {
                    objective,
                    state: GoalState::Active,
                    continuation,
                    observed_checkpoint: None,
                    reported_status: None,
                },
            },
        }
    }

    pub fn plan(artifact: PlanRecord, now_ms: i64) -> Self {
        Self {
            id: TaskId::fresh(),
            revision: TaskRevision::fresh(),
            observation_revision: TaskRevision::fresh(),
            auto_accept_plan: false,
            plan_execution: None,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            observed_turn: None,
            workflow: WorkflowSnapshot::Plan {
                plan: Box::new(PlanSnapshot {
                    artifact,
                    paused: false,
                }),
            },
        }
    }

    pub fn execute(
        title: String,
        record: PlanExecutionRecord,
        continuation: ContinuationBudget,
        now_ms: i64,
    ) -> Self {
        Self {
            id: TaskId::fresh(),
            revision: TaskRevision::fresh(),
            observation_revision: TaskRevision::fresh(),
            auto_accept_plan: false,
            plan_execution: None,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            entry: None,
            turn: None,
            observed_turn: None,
            workflow: WorkflowSnapshot::Execute {
                execution: Box::new(ExecutionSnapshot {
                    title,
                    record,
                    continuation,
                }),
            },
        }
    }

    pub fn permit(&self) -> TaskPermit {
        TaskPermit {
            id: self.id.clone(),
            revision: self.revision.clone(),
            observation_revision: self.observation_revision.clone(),
        }
    }

    /// Retains entry intent until the matching provider receipt activates control admission.
    pub(crate) fn request_entry(
        &mut self,
        instruction: forge_provider_api::forge::TurnInput,
        now_ms: i64,
    ) -> Result<()> {
        anyhow::ensure!(
            !instruction.label().trim().is_empty(),
            "task entry instruction is required"
        );
        self.renew(now_ms);
        self.entry = Some(TaskEntry {
            id: TaskInputId::fresh(),
            instruction,
            delivery: TaskInputDelivery::Pending,
            review_exchange: None,
            execution: None,
            resumed: false,
        });
        Ok(())
    }

    /// Retain a resumption's entry intent without re-announcing the task start.
    pub(crate) fn request_resume_entry(
        &mut self,
        instruction: forge_provider_api::forge::TurnInput,
        now_ms: i64,
    ) -> Result<()> {
        self.request_entry(instruction, now_ms)?;
        if let Some(entry) = self.entry.as_mut() {
            entry.resumed = true;
        }
        Ok(())
    }

    pub(crate) fn entry_accepted(&self) -> bool {
        self.entry
            .as_ref()
            .is_none_or(|entry| entry.delivery == TaskInputDelivery::Accepted)
    }

    pub fn title(&self) -> &str {
        match &self.workflow {
            WorkflowSnapshot::Goal { goal } => &goal.objective,
            WorkflowSnapshot::Plan { plan } => &plan.artifact.title,
            WorkflowSnapshot::Execute { execution } => &execution.title,
        }
    }

    pub fn kind(&self) -> super::TaskKind {
        match self.workflow.clone() {
            WorkflowSnapshot::Goal { .. } => super::TaskKind::Goal,
            WorkflowSnapshot::Plan { .. } => super::TaskKind::Plan,
            WorkflowSnapshot::Execute { .. } => super::TaskKind::Execute,
        }
    }

    /// Get the lifecycle condition represented by this adapter snapshot.
    pub fn status(&self) -> super::TaskStatus {
        super::state::snapshot_status(&self.workflow)
    }

    pub(crate) fn same_contract(&self, other: &Self) -> bool {
        match (&self.workflow, &other.workflow) {
            (WorkflowSnapshot::Goal { goal }, WorkflowSnapshot::Goal { goal: other }) => {
                goal.objective == other.objective
                    && goal.state == other.state
                    && goal.continuation.max_turn_count == other.continuation.max_turn_count
                    && goal.continuation.max_consecutive_no_progress
                        == other.continuation.max_consecutive_no_progress
            }
            (WorkflowSnapshot::Plan { plan }, WorkflowSnapshot::Plan { plan: other }) => {
                plan.artifact.id == other.artifact.id
                    && plan.artifact.state == other.artifact.state
                    && plan.paused == other.paused
            }
            (
                WorkflowSnapshot::Execute { execution },
                WorkflowSnapshot::Execute { execution: other },
            ) => {
                execution.record.id == other.record.id
                    && execution.record.plan_id == other.record.plan_id
                    && execution.record.revision == other.record.revision
                    && execution.record.generation == other.record.generation
                    && execution.record.phase == other.record.phase
                    && execution.record.state == other.record.state
                    && execution.record.resume_after_review == other.record.resume_after_review
            }
            _ => false,
        }
    }

    /// Change admission identity whenever the task's allowed operations change.
    pub fn renew(&mut self, now_ms: i64) {
        self.revision = TaskRevision::fresh();
        self.updated_at_ms = now_ms;
        if let Some(continuation) = self
            .turn
            .as_mut()
            .and_then(|turn| turn.continuation.as_mut())
        {
            continuation.delivery = TaskInputDelivery::Cancelled;
        }
    }
}
