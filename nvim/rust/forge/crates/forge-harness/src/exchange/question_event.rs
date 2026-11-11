//! Carry validated user decisions and scoped tool question mutations to the exchange owner.

use std::sync::Arc;

use crate::question::{
    LocalQuestionWait, QuestionAction, QuestionCommand, QuestionController, QuestionDraft,
    QuestionId, QuestionOrigin, QuestionSnapshot,
};
use crate::task::Task;
use crate::tools::TaskAuthority;

/// Bind question tool effects to one authenticated invocation and provider-assigned tool call.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct QuestionToolCall {
    scope: crate::provider::InvocationScope,
    call_id: forge_provider_api::control::ToolCallId,
}

impl QuestionToolCall {
    /// Capture host-authenticated identities, never fields from tool arguments.
    pub(crate) fn new(
        scope: crate::provider::InvocationScope,
        call_id: forge_provider_api::control::ToolCallId,
    ) -> Self {
        Self { scope, call_id }
    }
    /// Get the invocation whose current admission gate must authorize new effects.
    pub(super) fn scope(&self) -> &crate::provider::InvocationScope {
        &self.scope
    }
    /// Get the original tool-call identity for correlation and duplicate detection.
    pub(super) fn call_id(&self) -> &forge_provider_api::control::ToolCallId {
        &self.call_id
    }
    /// Derive one durable operation identity across replay without trusting provider payload fields.
    pub(crate) fn operation_id(&self) -> QuestionOperationId {
        let encoded = serde_json::to_vec(&(&self.scope, &self.call_id))
            .expect("authenticated question identities have fixed serialization");
        QuestionOperationId(format!("question-tool:{}", crate::plan::digest(&encoded)))
    }
}

/// Identify one immutable question operation, independently of its source exchange or command receipt.
pub(crate) struct QuestionOperationId(String);

impl QuestionOperationId {
    /// Create a nonempty operation identity at tool/event ingress.
    #[cfg(test)]
    pub(crate) fn new(value: String) -> Result<Self, QuestionEventError> {
        if value.trim().is_empty() {
            return Err(QuestionEventError::InvalidRequest);
        }
        Ok(Self(value))
    }

    /// Get the original identity used by the durable question operation journal.
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

/// Distinguish user-owned decisions from model-owned question discussion and content changes.
enum ChangeSource {
    User,
    Tool {
        authority: Box<TaskAuthority>,
        call: QuestionToolCall,
    },
}

/// Keep an allowed question action paired with the scope that may commit it.
pub(crate) struct QuestionChange {
    command: QuestionCommand,
    source: ChangeSource,
}

impl QuestionChange {
    /// Create an answer, clarification, correction, or dismissal event from the user interface.
    pub(crate) fn user(command: QuestionCommand) -> Result<Self, QuestionEventError> {
        match command.action {
            QuestionAction::Answer { .. }
            | QuestionAction::Clarify { .. }
            | QuestionAction::Correct { .. }
            | QuestionAction::Dismiss => {}
            QuestionAction::Reply { .. }
            | QuestionAction::Revise { .. }
            | QuestionAction::Withdraw { .. } => return Err(QuestionEventError::InvalidAction),
        }
        Self::validate(&command)?;
        Ok(Self {
            command,
            source: ChangeSource::User,
        })
    }

    /// Create a model-owned mutation with the original task and permission authority retained.
    pub(crate) fn tool(
        command: QuestionCommand,
        authority: TaskAuthority,
        call: QuestionToolCall,
    ) -> Result<Self, QuestionEventError> {
        if command.operation_id != call.operation_id().as_str() {
            return Err(QuestionEventError::InvalidRequest);
        }
        match command.action {
            QuestionAction::Reply { .. }
            | QuestionAction::Revise { .. }
            | QuestionAction::Withdraw { .. } => {}
            QuestionAction::Answer { .. }
            | QuestionAction::Clarify { .. }
            | QuestionAction::Correct { .. }
            | QuestionAction::Dismiss => return Err(QuestionEventError::InvalidAction),
        }
        Self::validate(&command)?;
        Ok(Self {
            command,
            source: ChangeSource::Tool {
                authority: Box::new(authority),
                call,
            },
        })
    }

    /// Replay the original committed mutation before a new-effect admission gate.
    pub(super) fn replay(
        &self,
        controller: &QuestionController,
    ) -> Result<Option<QuestionSnapshot>, QuestionEventError> {
        controller
            .replay_mutation(&self.command)
            .map_err(QuestionEventError::Replay)
    }

    /// Get invocation authority for model-owned mutations; user decisions remain independent events.
    pub(super) fn tool_call(&self) -> Option<&QuestionToolCall> {
        match &self.source {
            ChangeSource::User => None,
            ChangeSource::Tool { call, .. } => Some(call),
        }
    }

    /// Get the caller's immutable operation identity for correlated owner logs.
    pub(super) fn operation_id(&self) -> &str {
        &self.command.operation_id
    }

    /// Get the exact question selected before asynchronous intake.
    pub(super) fn question(&self) -> &QuestionId {
        &self.command.question_id
    }

    /// Commit under the owner's mutation boundary and the tool's captured authority where applicable.
    pub(super) fn apply(
        self,
        controller: &QuestionController,
        tasks: &Arc<crate::task::SessionTaskController>,
    ) -> Result<QuestionSnapshot, QuestionEventError> {
        if let Some(snapshot) = self.replay(controller)? {
            return Ok(snapshot);
        }
        match self.source {
            ChangeSource::User => controller
                .apply(self.command)
                .map_err(QuestionEventError::State),
            ChangeSource::Tool { authority, call: _ } => {
                if !authority.belongs_to(tasks) {
                    return Err(QuestionEventError::ForeignScope);
                }
                let questions = controller.snapshot().map_err(QuestionEventError::State)?;
                let question = questions
                    .questions
                    .iter()
                    .find(|question| question.id == self.command.question_id)
                    .ok_or(QuestionEventError::TargetMissing)?;
                if question.task_id.as_deref()
                    != authority.task_id().map(crate::task::TaskId::as_str)
                {
                    return Err(QuestionEventError::ForeignScope);
                }
                authority
                    .commit(|| Ok(controller.apply(self.command)))
                    .map_err(QuestionEventError::Authority)?
                    .map_err(QuestionEventError::State)
            }
        }
    }

    /// Reject incomplete identities/revisions before they enter owner intake.
    fn validate(command: &QuestionCommand) -> Result<(), QuestionEventError> {
        if command.operation_id.trim().is_empty() || command.revision == 0 {
            return Err(QuestionEventError::InvalidRequest);
        }
        Ok(())
    }
}

/// Select asynchronous workflow questions or discussion that grants no automatic continuation.
#[derive(Clone, Copy)]
pub(crate) enum QuestionCreationOrigin {
    /// Attribute questions to the captured task, or to the source chat when no task was admitted.
    Workflow,
    /// Retain discussion without granting automatic workflow continuation.
    Discussion,
}

/// Retain one tool's immutable question creation and revocable task/permission scope.
pub(crate) struct QuestionCreation {
    operation: QuestionOperationId,
    exchange: crate::provider::ExchangeId,
    task: Option<Task>,
    drafts: Vec<QuestionDraft>,
    origin: QuestionCreationOrigin,
    authority: TaskAuthority,
    call: Option<QuestionToolCall>,
}

impl QuestionCreation {
    /// Create authenticated question input before waiting for owner admission.
    pub(crate) fn from_tool(
        call: QuestionToolCall,
        exchange: crate::provider::ExchangeId,
        task: Option<Task>,
        drafts: Vec<QuestionDraft>,
        origin: QuestionCreationOrigin,
        authority: TaskAuthority,
    ) -> Result<Self, QuestionEventError> {
        if call.scope.exchange_id != exchange {
            return Err(QuestionEventError::ForeignScope);
        }
        let mut creation = Self::new_inner(
            call.operation_id(),
            exchange,
            task,
            drafts,
            origin,
            authority,
        )?;
        creation.call = Some(call);
        Ok(creation)
    }

    /// Create local fixture input without granting production tool admission authority.
    #[cfg(test)]
    pub(crate) fn new(
        operation: QuestionOperationId,
        exchange: crate::provider::ExchangeId,
        task: Option<Task>,
        drafts: Vec<QuestionDraft>,
        origin: QuestionCreationOrigin,
        authority: TaskAuthority,
    ) -> Result<Self, QuestionEventError> {
        Self::new_inner(operation, exchange, task, drafts, origin, authority)
    }

    /// Validate shared question payload independently of invocation admission.
    fn new_inner(
        operation: QuestionOperationId,
        exchange: crate::provider::ExchangeId,
        task: Option<Task>,
        drafts: Vec<QuestionDraft>,
        origin: QuestionCreationOrigin,
        authority: TaskAuthority,
    ) -> Result<Self, QuestionEventError> {
        if exchange.as_str().trim().is_empty()
            || drafts.is_empty()
            || drafts
                .iter()
                .any(|draft| draft.header.trim().is_empty() || draft.question.trim().is_empty())
        {
            return Err(QuestionEventError::InvalidRequest);
        }
        if task.as_ref().map(|task| &task.id) != authority.task_id() {
            return Err(QuestionEventError::ForeignScope);
        }
        Ok(Self {
            operation,
            exchange,
            task,
            drafts,
            origin,
            authority,
            call: None,
        })
    }

    /// Get authenticated tool authority; unscoped fixtures cannot authorize production effects.
    pub(super) fn call(&self) -> Option<&QuestionToolCall> {
        self.call.as_ref()
    }

    /// Normalize a blocking tool request before duplicate comparison or durable admission.
    pub(crate) fn waiting(mut self) -> Self {
        for draft in &mut self.drafts {
            draft.requirement = crate::question::QuestionRequirement::Required;
        }
        self
    }

    /// Read the original result before any new-effect gate; a retry never republishes questions.
    pub(super) fn replay(
        &self,
        controller: &QuestionController,
    ) -> Result<Option<Vec<QuestionId>>, QuestionEventError> {
        controller
            .replay_creation(
                self.operation.as_str(),
                self.exchange.as_str(),
                self.question_origin(),
                &self.drafts,
            )
            .map_err(QuestionEventError::Replay)
    }

    /// Replay a blocking creation whose answer destination is the original tool call.
    pub(super) fn replay_wait(
        &self,
        controller: &QuestionController,
    ) -> Result<Option<Vec<QuestionId>>, QuestionEventError> {
        controller
            .replay_creation(
                self.operation.as_str(),
                self.exchange.as_str(),
                QuestionOrigin::Tool(self.task.as_ref()),
                &self.drafts,
            )
            .map_err(QuestionEventError::Replay)
    }

    /// Get the question domain attribution without exposing mutable task authority.
    fn question_origin(&self) -> QuestionOrigin<'_> {
        match self.origin {
            QuestionCreationOrigin::Workflow => self
                .task
                .as_ref()
                .map_or(QuestionOrigin::Chat, QuestionOrigin::Task),
            QuestionCreationOrigin::Discussion => QuestionOrigin::Discussion(self.task.as_ref()),
        }
    }

    /// Get the original tool operation identity for creation and wait-publication logs.
    pub(super) fn operation_id(&self) -> &str {
        self.operation.as_str()
    }

    /// Publish asynchronous questions only while the captured task/permission authority remains current.
    pub(super) fn publish(
        self,
        controller: &QuestionController,
        services: &super::services::OwnerServices,
    ) -> Result<Vec<QuestionId>, QuestionEventError> {
        if let Some(ids) = self.replay(controller)? {
            return Ok(ids);
        }
        self.validate_scope(services)?;
        self.authority
            .commit(|| {
                Ok(controller.ask(
                    self.operation.as_str(),
                    self.exchange.as_str(),
                    match self.origin {
                        QuestionCreationOrigin::Workflow => self
                            .task
                            .as_ref()
                            .map_or(QuestionOrigin::Chat, QuestionOrigin::Task),
                        QuestionCreationOrigin::Discussion => {
                            QuestionOrigin::Discussion(self.task.as_ref())
                        }
                    },
                    self.drafts,
                ))
            })
            .map_err(QuestionEventError::Authority)?
            .map_err(QuestionEventError::State)
    }

    /// Prepare publication with local rollback custody; the driver installs retirement before replying.
    pub(super) fn prepare_wait(
        mut self,
        controller: &Arc<QuestionController>,
        services: &super::services::OwnerServices,
    ) -> Result<LocalQuestionWait, QuestionEventError> {
        for draft in &mut self.drafts {
            draft.requirement = crate::question::QuestionRequirement::Required;
        }
        if let Some(ids) = self.replay_wait(controller)? {
            return controller
                .replay_tool_wait(ids)
                .map_err(QuestionEventError::State);
        }
        self.validate_scope(services)?;
        if matches!(self.origin, QuestionCreationOrigin::Workflow)
            && controller
                .snapshot()
                .map_err(QuestionEventError::State)?
                .required_pending()
        {
            return Err(QuestionEventError::RequiredPending);
        }
        for draft in &mut self.drafts {
            draft.requirement = crate::question::QuestionRequirement::Required;
        }
        controller
            .prepare_tool_wait(
                self.operation.as_str(),
                self.exchange.as_str(),
                self.task.as_ref(),
                self.drafts,
                Some(&self.authority),
            )
            .map_err(QuestionEventError::State)
    }

    /// Reject foreign or completed source exchanges before any waiting destination or question is created.
    fn validate_scope(
        &self,
        services: &super::services::OwnerServices,
    ) -> Result<(), QuestionEventError> {
        if !self.authority.belongs_to(&services.tasks) {
            return Err(QuestionEventError::ForeignScope);
        }
        self.authority
            .validate_scope()
            .map_err(QuestionEventError::Authority)?;
        let source = crate::storage::HistoryReader::current(&services.root)
            .map_err(QuestionEventError::State)?
            .load_exchange(self.exchange.as_str())
            .map_err(QuestionEventError::State)?
            .ok_or(QuestionEventError::TargetMissing)?;
        if source.session_id != services.observer.session_id() {
            return Err(QuestionEventError::ForeignScope);
        }
        if source.is_finished() {
            return Err(QuestionEventError::SourceFinished);
        }
        Ok(())
    }
}

/// Preserve question-event failures without exposing provider protocol diagnostics or parsing strings.
#[derive(Debug, thiserror::Error)]
pub(crate) enum QuestionEventError {
    /// The durable authenticated call receipt rejected replay or could not be reconstructed.
    #[error(transparent)]
    Replay(crate::question::QuestionCreationReplayError),
    /// A revision-scoped plan answer failed before its result could be returned.
    #[error(transparent)]
    PlanAnswer(crate::plan::PlanQuestionAnswerError),
    /// The owner cannot admit another live destination without retiring an existing wait.
    #[error("question wait capacity is exhausted")]
    WaitCapacity,
    /// The callback controller or question attribution differs from the receiving session/task.
    #[error("question event belongs to another session or task scope")]
    ForeignScope,
    /// The selected question or originating exchange is absent from this session.
    #[error("question event target is missing")]
    TargetMissing,
    /// The source exchange settled before its scoped creation event was applied.
    #[error("question source exchange has already finished")]
    SourceFinished,
    /// A workflow already has required decisions and cannot admit another blocking question wait.
    #[error("required questions must be resolved before another workflow wait")]
    RequiredPending,
    /// The supplied action belongs to the other event source, such as a model answering for the user.
    #[error("question action is not allowed for this event source")]
    InvalidAction,
    /// An ingress identity, revision, or required question field is empty or invalid.
    #[error("question event requires valid identities and revisions")]
    InvalidRequest,
    /// New tool effects are fenced while invocation admission or durable storage is unavailable.
    #[error("question tool admission is closed for this invocation")]
    AdmissionClosed,
    /// No live owner can admit this event; no question mutation was started by this send.
    #[error("question mutation owner is closed")]
    OwnerClosed,
    /// Bounded owner intake rejected this event before admission.
    #[error("question mutation intake is full")]
    QueueFull,
    /// The event was admitted but its response was lost; this is not proof that no mutation committed.
    #[error("question mutation result was not observed")]
    ObservationLost,
    /// Captured task generation or permission authority was revoked before mutation.
    #[error("question tool authority is no longer available")]
    Authority(#[source] anyhow::Error),
    /// The legacy question store rejected or failed the mutation; preserve its cause for local diagnosis.
    #[error("question mutation could not be committed")]
    State(#[source] anyhow::Error),
}

impl QuestionEventError {
    /// Get the stable event failure code independently of legacy storage/callback error text.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::Replay(crate::question::QuestionCreationReplayError::IdentityConflict) => {
                "question_call_conflict"
            }
            Self::Replay(crate::question::QuestionCreationReplayError::LegacyResultUnavailable) => {
                "question_legacy_result_unavailable"
            }
            Self::Replay(crate::question::QuestionCreationReplayError::Incomplete) => {
                "question_receipt_incomplete"
            }
            Self::Replay(crate::question::QuestionCreationReplayError::State(_)) => {
                "question_receipt_unavailable"
            }
            Self::PlanAnswer(error) => error.code(),
            Self::WaitCapacity => "question_wait_capacity_exceeded",
            Self::ForeignScope => "question_scope_mismatch",
            Self::TargetMissing => "question_target_missing",
            Self::SourceFinished => "question_source_finished",
            Self::RequiredPending => "question_required_pending",
            Self::InvalidAction => "question_action_not_allowed",
            Self::InvalidRequest => "question_event_invalid",
            Self::AdmissionClosed => "question_tool_admission_closed",
            Self::OwnerClosed => "question_owner_closed",
            Self::QueueFull => "question_event_queue_full",
            Self::ObservationLost => "question_event_observation_lost",
            Self::Authority(_) => "question_authority_unavailable",
            Self::State(_) => "question_mutation_failed",
        }
    }
}
