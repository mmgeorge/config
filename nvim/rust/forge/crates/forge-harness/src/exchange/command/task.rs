//! Capture task intent independently of transport aliases and native execution state.

use serde::{Deserialize, Serialize};

use super::{
    Command, CommandDecodeError, PlanCancellation, PlanChangeRequest, PlanEdit,
    PlanExecutionSelection, PlanId, PlanQuestionSetSelection, PlanRevisionSelection, PlanSelection,
    PlanSourceGuard, TaskResumeRequest,
};
use crate::task::{Task, TaskId, TaskRevision, command::SessionAction};

/// Manage the session task through the same FIFO as input and configuration.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action", content = "request", rename_all = "snake_case")]
pub(crate) enum TaskCommand {
    /// Replace current task work and confirm initial goal-input dispatch, not goal completion.
    StartGoal(NewGoal),
    /// Replace task work and dispatch plan generation; approval and plan completion happen later.
    StartPlan(NewPlan),
    /// Attach the captured saved plan; dispatch only if its state requires model work.
    /// Reattaching the current plan preserves its task, questions, and already-owned work.
    AttachPlan(PlanSelection),
    /// Fork the captured working plan into a new lineage and dispatch its initial planning input.
    ForkPlan(PlanSelection),
    /// Replan the captured historical revision in the same workspace and dispatch its initial input.
    Replan(PlanRevisionSelection),
    /// Approve the captured revision or commit its required questions before releasing the FIFO.
    ExecutePlan(PlanExecutionSelection),
    /// Resume the captured paused task after validating its required decisions.
    Resume(TaskResumeRequest),
    /// Commit a direct revision-checked plan-document edit.
    EditPlan(PlanEdit),
    /// Revise the captured plan from explicit user feedback.
    RequestPlanChanges(PlanChangeRequest),
    /// Reject only the captured plan or proposed revision.
    CancelPlan(PlanCancellation),
    /// Collect task work and commit a paused task while preserving resumable history.
    Pause(TaskSelection),
    /// Collect task work and remove its authority while retaining historical records.
    Clear(TaskSelection),
    /// Change whether eligible plan submissions may execute automatically.
    /// Any resulting execution is separately scheduled after this policy commit.
    SetAutoAccept(PlanAcceptancePolicy),
}

impl TaskCommand {
    /// Get the committed task identity/revision that this request must not retarget.
    pub(crate) fn target(&self) -> Option<&TaskSelection> {
        match self {
            Self::StartGoal(_)
            | Self::StartPlan(_)
            | Self::AttachPlan(_)
            | Self::ForkPlan(_)
            | Self::Replan(_) => None,
            Self::ExecutePlan(selection) => selection.prior(),
            Self::Resume(request) => Some(request.target()),
            Self::EditPlan(edit) => edit.prior(),
            Self::RequestPlanChanges(request) => request.selection().prior(),
            Self::CancelPlan(request) => request.selection().prior(),
            Self::Pause(target) | Self::Clear(target) => Some(target),
            Self::SetAutoAccept(policy) => Some(&policy.target),
        }
    }

    /// Get a bounded intent name without logging task objectives or plan contents.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::StartGoal(_) => "task.start_goal",
            Self::StartPlan(_) => "task.start_plan",
            Self::AttachPlan(_) => "task.attach_plan",
            Self::ForkPlan(_) => "task.fork_plan",
            Self::Replan(_) => "task.replan",
            Self::ExecutePlan(_) => "task.execute_plan",
            Self::Resume(_) => "task.resume",
            Self::EditPlan(_) => "task.edit_plan",
            Self::RequestPlanChanges(_) => "task.request_plan_changes",
            Self::CancelPlan(_) => "task.cancel_plan",
            Self::Pause(_) => "task.pause",
            Self::Clear(_) => "task.clear",
            Self::SetAutoAccept(_) => "task.set_auto_accept",
        }
    }

    /// Validate that a domain commit describes this intent and its captured task identity.
    pub(crate) fn accepts_outcome(&self, outcome: &TaskCommandOutcome) -> bool {
        match outcome {
            TaskCommandOutcome::PlanEdited { source, .. } => {
                matches!(self, Self::EditPlan(edit) if edit.source().id() == source.id())
            }
            TaskCommandOutcome::PlanChangesRequested { source, .. } => {
                matches!(self, Self::RequestPlanChanges(request) if request.selection().source().id() == source.id())
            }
            TaskCommandOutcome::Resumed { task } => {
                matches!(self, Self::Resume(request) if request.target().id() == task.id())
            }
            TaskCommandOutcome::PlanCancelled { source, .. } => {
                matches!(self, Self::CancelPlan(request) if request.selection().source().id() == source.id())
            }
            TaskCommandOutcome::Continued { .. }
            | TaskCommandOutcome::ContinuationReserved { .. }
            | TaskCommandOutcome::WorkflowAwaitingInput { .. } => false,
            TaskCommandOutcome::ExecutionUpdated { source, .. }
            | TaskCommandOutcome::AwaitingInput { source, .. } => match self {
                Self::ExecutePlan(selection) => selection.source().id() == source.id(),
                Self::StartGoal(_)
                | Self::StartPlan(_)
                | Self::AttachPlan(_)
                | Self::ForkPlan(_)
                | Self::Replan(_)
                | Self::Pause(_)
                | Self::Clear(_)
                | Self::SetAutoAccept(_)
                | Self::Resume(_)
                | Self::RequestPlanChanges(_)
                | Self::CancelPlan(_)
                | Self::EditPlan(_) => false,
            },
            TaskCommandOutcome::Started { .. } => match self {
                Self::StartGoal(_)
                | Self::StartPlan(_)
                | Self::AttachPlan(_)
                | Self::ForkPlan(_)
                | Self::Replan(_)
                | Self::ExecutePlan(_)
                | Self::Resume(_)
                | Self::RequestPlanChanges(_) => true,
                Self::Pause(_)
                | Self::Clear(_)
                | Self::SetAutoAccept(_)
                | Self::CancelPlan(_)
                | Self::EditPlan(_) => false,
            },
            TaskCommandOutcome::Attached { plan, .. } => match self {
                Self::AttachPlan(source) => source.id() == plan,
                Self::StartGoal(_)
                | Self::StartPlan(_)
                | Self::ForkPlan(_)
                | Self::Replan(_)
                | Self::ExecutePlan(_)
                | Self::Pause(_)
                | Self::Clear(_)
                | Self::SetAutoAccept(_)
                | Self::Resume(_)
                | Self::RequestPlanChanges(_)
                | Self::CancelPlan(_)
                | Self::EditPlan(_) => false,
            },
            TaskCommandOutcome::Paused { task } | TaskCommandOutcome::Unchanged { task } => {
                match self {
                    Self::Pause(target) => target.id == task.id,
                    Self::StartGoal(_)
                    | Self::StartPlan(_)
                    | Self::AttachPlan(_)
                    | Self::ForkPlan(_)
                    | Self::Replan(_)
                    | Self::ExecutePlan(_)
                    | Self::Clear(_)
                    | Self::SetAutoAccept(_)
                    | Self::Resume(_)
                    | Self::RequestPlanChanges(_)
                    | Self::CancelPlan(_)
                    | Self::EditPlan(_) => false,
                }
            }
            TaskCommandOutcome::Cleared { task } => match self {
                Self::Clear(target) => &target.id == task,
                Self::StartGoal(_)
                | Self::StartPlan(_)
                | Self::AttachPlan(_)
                | Self::ForkPlan(_)
                | Self::Replan(_)
                | Self::ExecutePlan(_)
                | Self::Pause(_)
                | Self::SetAutoAccept(_)
                | Self::Resume(_)
                | Self::RequestPlanChanges(_)
                | Self::CancelPlan(_)
                | Self::EditPlan(_) => false,
            },
            TaskCommandOutcome::AutoAcceptChanged { task, enabled } => match self {
                Self::SetAutoAccept(policy) => {
                    policy.target.id == task.id && policy.enabled == *enabled
                }
                Self::StartGoal(_)
                | Self::StartPlan(_)
                | Self::AttachPlan(_)
                | Self::ForkPlan(_)
                | Self::Replan(_)
                | Self::ExecutePlan(_)
                | Self::Pause(_)
                | Self::Clear(_)
                | Self::Resume(_)
                | Self::RequestPlanChanges(_)
                | Self::CancelPlan(_)
                | Self::EditPlan(_) => false,
            },
        }
    }

    /// Get a saved-plan source and the scope permitted by this command's semantics.
    pub(crate) fn plan_source(&self) -> Option<PlanSourceGuard> {
        match self {
            Self::AttachPlan(source) | Self::ForkPlan(source) => {
                Some(PlanSourceGuard::Working(source.clone()))
            }
            Self::Replan(selection) => Some(PlanSourceGuard::Historical(selection.clone())),
            Self::ExecutePlan(selection) => {
                Some(PlanSourceGuard::Working(selection.source().clone()))
            }
            Self::RequestPlanChanges(request) => Some(PlanSourceGuard::Working(
                request.selection().source().clone(),
            )),
            Self::CancelPlan(request) => Some(PlanSourceGuard::Working(
                request.selection().source().clone(),
            )),
            Self::Resume(_) => None,
            Self::EditPlan(edit) => Some(PlanSourceGuard::Working(edit.source().clone())),
            Self::StartGoal(_)
            | Self::StartPlan(_)
            | Self::Pause(_)
            | Self::Clear(_)
            | Self::SetAutoAccept(_) => None,
        }
    }

    /// Bind the prepared task kind and source guard to this immutable creation request.
    pub(crate) fn accepts_installation(
        &self,
        kind: crate::task::TaskKind,
        source: Option<&PlanSourceGuard>,
    ) -> bool {
        let matches_kind = match self {
            Self::StartGoal(_) => kind == crate::task::TaskKind::Goal,
            Self::StartPlan(_) | Self::AttachPlan(_) | Self::ForkPlan(_) | Self::Replan(_) => {
                kind == crate::task::TaskKind::Plan
            }
            Self::ExecutePlan(_) | Self::RequestPlanChanges(_) => matches!(
                kind,
                crate::task::TaskKind::Plan | crate::task::TaskKind::Execute
            ),
            Self::Resume(_) => true,
            Self::Pause(_)
            | Self::Clear(_)
            | Self::SetAutoAccept(_)
            | Self::CancelPlan(_)
            | Self::EditPlan(_) => false,
        };
        matches_kind && self.plan_source().as_ref() == source
    }

    /// Identify the saved plan to capture for fresh legacy source-based commands.
    pub(crate) fn legacy_plan_id(action: &SessionAction) -> Option<&str> {
        match action {
            SessionAction::Attach { plan_id }
            | SessionAction::Fork { plan_id }
            | SessionAction::Replan { plan_id, .. } => Some(plan_id),
            SessionAction::NewGoal { .. }
            | SessionAction::NewPlan { .. }
            | SessionAction::PlanAccept { .. }
            | SessionAction::Execute { .. }
            | SessionAction::Pause
            | SessionAction::Resume { .. }
            | SessionAction::Clear
            | SessionAction::ReserveContinuation { .. }
            | SessionAction::RequestChanges { .. }
            | SessionAction::CancelPlan { .. } => None,
        }
    }

    /// Compare retry intent without reading or recapturing mutable target authority.
    fn replay_intent(&self) -> TaskIntent {
        match self {
            Self::StartGoal(goal) => TaskIntent::StartGoal(goal.clone()),
            Self::StartPlan(plan) => TaskIntent::StartPlan(plan.clone()),
            Self::AttachPlan(source) => TaskIntent::AttachPlan(source.id().clone()),
            Self::ForkPlan(source) => TaskIntent::ForkPlan(source.id().clone()),
            Self::Replan(selection) => TaskIntent::Replan {
                plan: selection.source().id().clone(),
                revision: selection.revision(),
            },
            Self::ExecutePlan(selection) => TaskIntent::ExecutePlan(selection.request().clone()),
            Self::Resume(request) => TaskIntent::Resume(request.clone()),
            Self::EditPlan(edit) => TaskIntent::EditPlan(edit.clone()),
            Self::RequestPlanChanges(request) => TaskIntent::RequestPlanChanges(request.clone()),
            Self::CancelPlan(request) => TaskIntent::CancelPlan(request.clone()),
            Self::Pause(_) => TaskIntent::Pause,
            Self::Clear(_) => TaskIntent::Clear,
            Self::SetAutoAccept(policy) => TaskIntent::SetAutoAccept(policy.enabled),
        }
    }

    /// Normalize migrated task aliases and reuse their original target on an identified retry.
    /// Other task families keep their existing ingress until their owner stages are migrated.
    pub(crate) fn from_session_action(
        action: &SessionAction,
        current: Option<&Task>,
        source_plan: Option<&crate::plan::PlanRecord>,
        previous: Option<&Command>,
    ) -> Result<Option<Self>, TaskCommandFailure> {
        match action {
            SessionAction::Resume { request } => {
                if let Some(previous) = previous {
                    let Command::Task(Self::Resume(prior)) = previous else {
                        return Err(TaskCommandFailure::IdentityConflict);
                    };
                    return if prior.matches_request(request) {
                        Ok(Some(Self::Resume(prior.clone())))
                    } else {
                        Err(TaskCommandFailure::IdentityConflict)
                    };
                }
                return Ok(Some(Self::Resume(TaskResumeRequest::capture(
                    current.ok_or(TaskCommandFailure::MissingTask)?,
                    request,
                )?)));
            }
            SessionAction::RequestChanges { review } | SessionAction::CancelPlan { review } => {
                if let Some(previous) = previous {
                    return match (action, previous) {
                        (
                            SessionAction::RequestChanges { .. },
                            Command::Task(Self::RequestPlanChanges(prior)),
                        ) if prior.selection().matches_request(review) => {
                            Ok(Some(Self::RequestPlanChanges(prior.clone())))
                        }
                        (
                            SessionAction::CancelPlan { .. },
                            Command::Task(Self::CancelPlan(prior)),
                        ) if prior.selection().matches_request(review) => {
                            Ok(Some(Self::CancelPlan(prior.clone())))
                        }
                        _ => Err(TaskCommandFailure::IdentityConflict),
                    };
                }
                let plan = source_plan.ok_or(TaskCommandFailure::PlanMissing)?;
                return Ok(Some(
                    if matches!(action, SessionAction::RequestChanges { .. }) {
                        Self::RequestPlanChanges(PlanChangeRequest::capture(plan, current, review)?)
                    } else {
                        Self::CancelPlan(PlanCancellation::capture(plan, current, review)?)
                    },
                ));
            }
            SessionAction::NewGoal { .. }
            | SessionAction::NewPlan { .. }
            | SessionAction::PlanAccept { .. }
            | SessionAction::Execute { .. }
            | SessionAction::Attach { .. }
            | SessionAction::Fork { .. }
            | SessionAction::Replan { .. }
            | SessionAction::Pause
            | SessionAction::Clear
            | SessionAction::ReserveContinuation { .. } => {}
        }
        let intent = match action {
            SessionAction::NewGoal { text } => TaskIntent::StartGoal(NewGoal::new(text.clone())?),
            SessionAction::NewPlan { text } => TaskIntent::StartPlan(NewPlan::new(text.clone())?),
            SessionAction::Attach { plan_id } => TaskIntent::AttachPlan(
                PlanId::new(plan_id.clone())
                    .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
            ),
            SessionAction::Fork { plan_id } => TaskIntent::ForkPlan(
                PlanId::new(plan_id.clone())
                    .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
            ),
            SessionAction::Replan { plan_id, revision } => TaskIntent::Replan {
                plan: PlanId::new(plan_id.clone())
                    .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
                revision: *revision,
            },
            SessionAction::Pause => TaskIntent::Pause,
            SessionAction::Clear => TaskIntent::Clear,
            SessionAction::PlanAccept { enabled } => TaskIntent::SetAutoAccept(*enabled),
            SessionAction::Execute { selection } => TaskIntent::ExecutePlan(selection.clone()),
            SessionAction::Resume { .. }
            | SessionAction::ReserveContinuation { .. }
            | SessionAction::RequestChanges { .. }
            | SessionAction::CancelPlan { .. } => return Ok(None),
        };
        if let Some(previous) = previous {
            let Command::Task(previous) = previous else {
                return Err(TaskCommandFailure::IdentityConflict);
            };
            let matches = intent == previous.replay_intent();
            return if matches {
                Ok(Some(previous.clone()))
            } else {
                Err(TaskCommandFailure::IdentityConflict)
            };
        }
        let source = |id: &PlanId| -> Result<PlanSelection, TaskCommandFailure> {
            let plan = source_plan.ok_or(TaskCommandFailure::PlanMissing)?;
            if id.as_str() != plan.id {
                return Err(TaskCommandFailure::PlanChanged);
            }
            PlanSelection::capture(plan)
        };
        Ok(Some(match intent {
            TaskIntent::StartGoal(goal) => Self::StartGoal(goal),
            TaskIntent::StartPlan(plan) => Self::StartPlan(plan),
            TaskIntent::AttachPlan(id) => Self::AttachPlan(source(&id)?),
            TaskIntent::ForkPlan(id) => Self::ForkPlan(source(&id)?),
            TaskIntent::Replan { plan, revision } => {
                Self::Replan(PlanRevisionSelection::new(source(&plan)?, revision)?)
            }
            TaskIntent::ExecutePlan(request) => Self::ExecutePlan(PlanExecutionSelection::capture(
                source_plan.ok_or(TaskCommandFailure::PlanMissing)?,
                current,
                request,
            )?),
            TaskIntent::Resume(request) => Self::Resume(request),
            TaskIntent::EditPlan(edit) => Self::EditPlan(edit),
            TaskIntent::RequestPlanChanges(request) => Self::RequestPlanChanges(request),
            TaskIntent::CancelPlan(request) => Self::CancelPlan(request),
            TaskIntent::Pause => Self::Pause(TaskSelection::capture(
                current.ok_or(TaskCommandFailure::MissingTask)?,
            )),
            TaskIntent::Clear => Self::Clear(TaskSelection::capture(
                current.ok_or(TaskCommandFailure::MissingTask)?,
            )),
            TaskIntent::SetAutoAccept(enabled) => Self::SetAutoAccept(PlanAcceptancePolicy {
                target: TaskSelection::capture(current.ok_or(TaskCommandFailure::MissingTask)?),
                enabled,
            }),
        }))
    }
}

/// Supply the objective for a new goal; the owner selects replaced work when the command starts.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct NewGoal {
    objective: TaskObjective,
}

impl NewGoal {
    /// Validate the objective once without treating its text as a protocol discriminator.
    pub(crate) fn new(objective: String) -> Result<Self, TaskCommandFailure> {
        Ok(Self {
            objective: TaskObjective::try_from(objective)?,
        })
    }
    /// Get the original objective for task preparation.
    pub(crate) fn objective(&self) -> &str {
        &self.objective.0
    }
}

/// Supply a new planning objective independently of approval and execution decisions.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct NewPlan {
    objective: TaskObjective,
}

impl NewPlan {
    /// Validate a planning objective before admitting task preparation.
    pub(crate) fn new(objective: String) -> Result<Self, TaskCommandFailure> {
        Ok(Self {
            objective: TaskObjective::try_from(objective)?,
        })
    }

    /// Get the original objective for plan generation.
    pub(crate) fn objective(&self) -> &str {
        &self.objective.0
    }
}

/// Retain nonempty user intent without interpreting its content as a protocol discriminator.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
struct TaskObjective(String);

impl TryFrom<String> for TaskObjective {
    type Error = TaskCommandFailure;
    /// Validate both incoming requests and persisted task objectives at their boundary.
    fn try_from(objective: String) -> Result<Self, Self::Error> {
        if objective.trim().is_empty() {
            return Err(TaskCommandFailure::InvalidObjective);
        }
        Ok(Self(objective))
    }
}

impl From<TaskObjective> for String {
    /// Preserve the caller's complete objective and its existing string representation.
    fn from(objective: TaskObjective) -> Self {
        objective.0
    }
}

/// Select one task revision from a committed snapshot rather than whichever task is newest later.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawTaskSelection", into = "RawTaskSelection")]
pub(crate) struct TaskSelection {
    id: TaskId,
    revision: TaskRevision,
}

impl TaskSelection {
    /// Capture an existing task's identity and mutation revision.
    pub(crate) fn capture(task: &Task) -> Self {
        Self {
            id: task.id.clone(),
            revision: task.revision.clone(),
        }
    }

    /// Get the selected task identity for history and scope checks.
    pub(crate) fn id(&self) -> &TaskId {
        &self.id
    }

    /// Get the selected task revision for atomic mutation guards.
    pub(crate) fn revision(&self) -> &TaskRevision {
        &self.revision
    }

    /// Reject absent, replaced, or revised task authority before starting effects.
    pub(crate) fn validate(&self, current: Option<&Task>) -> Result<(), TaskCommandFailure> {
        match current {
            Some(task) if task.id == self.id && task.revision == self.revision => Ok(()),
            Some(_) => Err(TaskCommandFailure::Changed),
            None => Err(TaskCommandFailure::MissingTask),
        }
    }
}

impl TryFrom<RawTaskSelection> for TaskSelection {
    type Error = CommandDecodeError;
    /// Reject empty persisted or caller-supplied identities before constructing a task target.
    fn try_from(raw: RawTaskSelection) -> Result<Self, Self::Error> {
        let RawTaskSelection { id, revision } = raw;
        if id.as_str().trim().is_empty() || revision.as_str().trim().is_empty() {
            return Err(CommandDecodeError::EmptyIdentity);
        }
        Ok(Self { id, revision })
    }
}

impl From<TaskSelection> for RawTaskSelection {
    /// Preserve the selected task's wire spelling without exposing mutation access.
    fn from(selection: TaskSelection) -> Self {
        let TaskSelection { id, revision } = selection;
        Self { id, revision }
    }
}

/// Commit automatic approval policy against its observed task revision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct PlanAcceptancePolicy {
    /// Planning or executing task whose policy is being changed.
    pub(crate) target: TaskSelection,
    /// Whether eligible revisions may pass the normal approval gate automatically.
    pub(crate) enabled: bool,
}

/// Preserve the actual task postcondition independently of initial input scheduling or later work.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "disposition", rename_all = "snake_case")]
pub(crate) enum TaskCommandOutcome {
    /// The captured document mutation committed without executing the plan.
    PlanEdited {
        source: PlanSelection,
        task: Option<TaskSelection>,
        plan: Box<crate::plan::PlanRecord>,
    },
    /// Feedback committed while the task remained paused or needed no dispatch.
    PlanChangesRequested {
        source: PlanSelection,
        task: Option<TaskSelection>,
    },
    /// Resume prerequisites committed without requiring provider work.
    Resumed { task: TaskSelection },
    /// The selected plan or proposed revision was rejected and its restore journal settled.
    PlanCancelled {
        source: PlanSelection,
        task: Option<TaskSelection>,
    },
    /// Local check work was reserved without inventing a provider dispatch.
    ContinuationReserved { task: TaskSelection },
    /// The selected workflow still requires answers; the finite command has completed.
    WorkflowAwaitingInput {
        task: Option<TaskSelection>,
        question_set: PlanQuestionSetSelection,
    },
    /// The captured workflow continuation was dispatched without waiting for its answer.
    Continued {
        task: Option<TaskSelection>,
        input: super::InputId,
        scope: crate::provider::InvocationScope,
        binding: forge_provider_api::BindingId,
    },
    /// Execution policy changed without requiring a fresh provider input.
    ExecutionUpdated {
        task: TaskSelection,
        source: PlanSelection,
    },
    /// Required revision-scoped questions committed without approving or dispatching execution.
    AwaitingInput {
        task: Option<TaskSelection>,
        source: PlanSelection,
        question_set: PlanQuestionSetSelection,
    },
    /// The task owns the selected plan without claiming a new model invocation was dispatched.
    Attached { task: TaskSelection, plan: PlanId },
    /// Initial input handoff was confirmed for the committed task; later workflow revisions may have advanced.
    Started {
        task: TaskSelection,
        input: crate::task::TaskInputId,
        scope: crate::provider::InvocationScope,
        binding: forge_provider_api::BindingId,
    },
    /// Active task work settled and the selected task is now paused.
    Paused { task: TaskSelection },
    /// Task authority was removed; historical records remain available.
    Cleared { task: TaskId },
    /// The selected terminal task needed no further pause or native collection.
    Unchanged { task: TaskSelection },
    /// Automatic acceptance policy committed without starting execution in this command.
    AutoAcceptChanged { task: TaskSelection, enabled: bool },
}

/// Keep task admission, stale targets, state failures, and initial input scheduling failures distinct.
#[derive(Clone, Debug, Deserialize, Serialize, thiserror::Error)]
#[serde(tag = "failure", rename_all = "snake_case")]
pub(crate) enum TaskCommandFailure {
    /// Accepted artifacts require a new lineage before further planning edits.
    #[error("the selected accepted plan must be forked before attachment")]
    PlanRequiresFork,
    /// Rejected/cancelled plans cannot silently acquire new active planning authority.
    #[error("the selected plan cannot be attached in its current state")]
    PlanAttachmentUnavailable,
    /// The selected saved artifact or source authority is malformed.
    #[error("saved plan selection is invalid")]
    InvalidPlanSelection,
    /// The selected saved plan or its owning session no longer exists.
    #[error("the selected saved plan is missing")]
    PlanMissing,
    /// Content or approval state changed after the saved plan was selected.
    #[error("the selected saved plan changed before the command could apply")]
    PlanChanged,
    /// Working-plan forks and attachments cannot select a different conversation.
    #[error("the selected saved plan belongs to another session")]
    PlanForeignSession,
    /// Historical replanning cannot import another workspace's authority.
    #[error("the selected saved plan belongs to another workspace")]
    PlanForeignWorkspace,
    /// The requested model revision must be positive and present in the captured artifact.
    #[error("the selected plan revision is unavailable")]
    PlanRevisionUnavailable,
    /// Goal objectives cannot be empty, including after trimming whitespace.
    #[error("task objective must be nonempty")]
    InvalidObjective,
    /// Task preparation failed before task installation or native dispatch.
    #[error("task preparation failed")]
    PreparationUnavailable,
    /// A retained preparation worker did not settle within its observation deadline.
    #[error("task preparation exceeded its observation deadline")]
    PreparationTimedOut,
    /// No task existed when a legacy request needed to capture its target.
    #[error("the session has no selected task")]
    MissingTask,
    /// The task identity or expected revision no longer matches.
    #[error("the selected task changed before the command could apply")]
    Changed,
    /// Automatic plan acceptance is meaningful only for Plan and Execute workflows.
    #[error("the selected task does not support plan acceptance policy")]
    InvalidPolicyTask,
    /// An identified retry selected another action or policy value.
    #[error("task command identity was reused for a different intent")]
    IdentityConflict,
    /// Task storage could not certify its read or mutation.
    #[error("task state could not be read or committed")]
    StateUnavailable,
}

/// Preserve only domain facts committed before synchronization or initial input handoff completes.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(from = "StoredTaskCheckpoint", into = "StoredTaskCheckpoint")]
pub(crate) enum TaskCommandCheckpoint {
    /// A workflow input committed under captured authority before its handoff was confirmed.
    WorkflowInputPending {
        task: Option<TaskSelection>,
        input: super::InputId,
    },
    /// A finite control mutation committed; its input scheduling result is observed separately.
    Control { outcome: Box<TaskCommandOutcome> },
    /// A new task and its original input identity committed before any dispatch claim was made.
    InputPending {
        task: TaskSelection,
        input: crate::task::TaskInputId,
    },
}

impl TaskCommandCheckpoint {
    /// Get the selected task where its authority must still be validated before another effect.
    pub(crate) fn task(&self) -> Option<&TaskSelection> {
        match self {
            Self::WorkflowInputPending { task, .. } => task.as_ref(),
            Self::InputPending { task, .. } => Some(task),
            Self::Control { outcome } => match outcome.as_ref() {
                TaskCommandOutcome::PlanEdited { task, .. }
                | TaskCommandOutcome::PlanChangesRequested { task, .. }
                | TaskCommandOutcome::PlanCancelled { task, .. }
                | TaskCommandOutcome::AwaitingInput { task, .. }
                | TaskCommandOutcome::Continued { task, .. }
                | TaskCommandOutcome::WorkflowAwaitingInput { task, .. } => task.as_ref(),
                TaskCommandOutcome::Resumed { task }
                | TaskCommandOutcome::ContinuationReserved { task }
                | TaskCommandOutcome::ExecutionUpdated { task, .. }
                | TaskCommandOutcome::Started { task, .. }
                | TaskCommandOutcome::Attached { task, .. }
                | TaskCommandOutcome::Paused { task }
                | TaskCommandOutcome::Unchanged { task }
                | TaskCommandOutcome::AutoAcceptChanged { task, .. } => Some(task),
                TaskCommandOutcome::Cleared { .. } => None,
            },
        }
    }
}

impl From<StoredTaskCheckpoint> for TaskCommandCheckpoint {
    /// Convert earlier control-only checkpoints explicitly at the persistence boundary.
    fn from(stored: StoredTaskCheckpoint) -> Self {
        match stored {
            StoredTaskCheckpoint::Legacy(outcome)
            | StoredTaskCheckpoint::Current(TaskCheckpointRecord::Control { outcome }) => {
                Self::Control { outcome }
            }
            StoredTaskCheckpoint::Current(TaskCheckpointRecord::WorkflowInputPending {
                task,
                input,
            }) => Self::WorkflowInputPending { task, input },
            StoredTaskCheckpoint::Current(TaskCheckpointRecord::InputPending { task, input }) => {
                Self::InputPending { task, input }
            }
        }
    }
}

impl From<TaskCommandCheckpoint> for StoredTaskCheckpoint {
    /// Persist only the current tagged checkpoint representation.
    fn from(checkpoint: TaskCommandCheckpoint) -> Self {
        Self::Current(match checkpoint {
            TaskCommandCheckpoint::Control { outcome } => TaskCheckpointRecord::Control { outcome },
            TaskCommandCheckpoint::WorkflowInputPending { task, input } => {
                TaskCheckpointRecord::WorkflowInputPending { task, input }
            }
            TaskCommandCheckpoint::InputPending { task, input } => {
                TaskCheckpointRecord::InputPending { task, input }
            }
        })
    }
}

impl TaskCommandFailure {
    /// Get the stable task error code without parsing display text or native messages.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::PlanRequiresFork => "plan_requires_fork",
            Self::PlanAttachmentUnavailable => "plan_attachment_unavailable",
            Self::InvalidPlanSelection => "plan_selection_invalid",
            Self::PlanMissing => "plan_missing",
            Self::PlanChanged => "plan_changed",
            Self::PlanForeignSession => "plan_foreign_session",
            Self::PlanForeignWorkspace => "plan_foreign_workspace",
            Self::PlanRevisionUnavailable => "plan_revision_unavailable",
            Self::InvalidObjective => "task_objective_required",
            Self::PreparationUnavailable => "task_preparation_unavailable",
            Self::PreparationTimedOut => "task_preparation_timeout",
            Self::MissingTask => "task_missing",
            Self::Changed => "task_changed",
            Self::InvalidPolicyTask => "task_policy_unavailable",
            Self::IdentityConflict => "command_identity_conflict",
            Self::StateUnavailable => "task_state_unavailable",
        }
    }
}

/// Describe failure after task state has already committed; none of these outcomes authorizes replay.
#[derive(Clone, Debug, Deserialize, Serialize, thiserror::Error)]
#[serde(tag = "failure", rename_all = "snake_case")]
pub(crate) enum TaskSynchronizationFailure {
    /// Task/input authority or compatibility resources prevented initial delivery after task commit.
    #[error("initial task input could not be prepared for dispatch")]
    InputPreparationUnavailable,
    /// The provider worker ended before it confirmed the task input's dispatch, so the input is never resent.
    #[error("the provider did not confirm it received the task input")]
    DispatchLost,
    /// Host restart ended observation after the task checkpoint committed.
    #[error("synchronization was interrupted by a host restart")]
    Interrupted,
    /// Later task authority cannot be certified as the committed postcondition.
    #[error("the task changed while its committed transition was synchronizing")]
    TargetChanged,
    /// The committed postcondition could not be read for final validation.
    #[error("committed task state is unavailable for synchronization validation")]
    StateUnavailable,
}

impl TaskSynchronizationFailure {
    /// Get the stage-specific failure code without losing the committed task outcome.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::InputPreparationUnavailable => "task_input_preparation_unavailable",
            Self::DispatchLost => "provider_dispatch_unconfirmed",
            Self::Interrupted => "command_interrupted",
            Self::TargetChanged => "task_changed",
            Self::StateUnavailable => "task_state_unavailable",
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RawTaskSelection {
    id: TaskId,
    revision: TaskRevision,
}

#[derive(Deserialize, Serialize)]
#[serde(untagged)]
enum StoredTaskCheckpoint {
    Current(TaskCheckpointRecord),
    Legacy(Box<TaskCommandOutcome>),
}

#[derive(Deserialize, Serialize)]
#[serde(tag = "stage", rename_all = "snake_case")]
enum TaskCheckpointRecord {
    WorkflowInputPending {
        task: Option<TaskSelection>,
        input: super::InputId,
    },
    Control {
        outcome: Box<TaskCommandOutcome>,
    },
    InputPending {
        task: TaskSelection,
        input: crate::task::TaskInputId,
    },
}

#[derive(Eq, PartialEq)]
enum TaskIntent {
    StartGoal(NewGoal),
    StartPlan(NewPlan),
    AttachPlan(PlanId),
    ForkPlan(PlanId),
    Replan { plan: PlanId, revision: u32 },
    Pause,
    Clear,
    SetAutoAccept(bool),
    ExecutePlan(crate::task::command::ExecuteSelection),
    Resume(TaskResumeRequest),
    EditPlan(PlanEdit),
    RequestPlanChanges(PlanChangeRequest),
    CancelPlan(PlanCancellation),
}
