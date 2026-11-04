//! Typed caller intents and durable session-owner results.

mod compaction;
mod continuation;
pub(crate) use continuation::{
    ExchangeContinuationSelection, QuestionContinuationSelection, QuestionWorkflow,
    WorkflowContinuationRequest, WorkflowContinuationTarget,
};
mod execution;
pub(crate) use execution::{PlanExecutionSelection, PlanQuestionSetSelection};
mod configuration;
pub(crate) use compaction::{
    ContextCompactionFailure, ContextCompactionOutcome, ContextCompactionRequest,
};
mod plan;
mod plan_edit;
pub(crate) use plan_edit::{PlanEdit, PlanEditChange, PlanEditRequest, PlanEntitySelection};
mod settings;
pub(crate) use plan::{PlanRevisionSelection, PlanSelection, PlanSourceGuard};
mod task;
mod task_requests;
pub(crate) use configuration::{
    CheckPreferencesUpdate, ConfigurationChange, ConfigurationOutcome, ConfigurationRejection,
    InputTimeoutConfiguration, InvocationDisposition, PermissionChange,
};
pub(crate) use settings::SessionConfigurationPatch;
pub(crate) use task::{
    NewGoal, NewPlan, TaskCommand, TaskCommandCheckpoint, TaskCommandFailure, TaskCommandOutcome,
    TaskSelection, TaskSynchronizationFailure,
};
pub(crate) use task_requests::{PlanCancellation, PlanChangeRequest, TaskResumeRequest};

#[cfg(test)]
pub(crate) use task::PlanAcceptancePolicy;

use serde::{Deserialize, Serialize};

use crate::exchange::agent_command::{AgentCommand, AgentFailure, AgentOutcome};
use crate::exchange::rollback::{RollbackCommand, RollbackFailure, RollbackOutcome};

use crate::attachment::AttachmentSelection;
use crate::input::{
    InputAction, InputEdit, InputIntent, InputMessage, InputOperation, InputPurpose, InputRejection,
};

macro_rules! identity {
    ($name:ident, $description:literal) => {
        #[doc = $description]
        #[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
        #[serde(try_from = "String", into = "String")]
        pub(crate) struct $name(String);

        impl $name {
            /// Create an identity after rejecting empty input at the boundary.
            pub(crate) fn new(value: impl Into<String>) -> Result<Self, CommandDecodeError> {
                let value = value.into();
                if value.trim().is_empty() {
                    return Err(CommandDecodeError::EmptyIdentity);
                }
                Ok(Self(value))
            }

            /// Get the original opaque identity for transport or persistence.
            pub(crate) fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl TryFrom<String> for $name {
            type Error = CommandDecodeError;
            /// Validate identifiers loaded from a journal or transport.
            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::new(value)
            }
        }

        impl From<$name> for String {
            /// Encode an already validated identity without changing its spelling.
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}

identity!(
    CommandId,
    "Identify one immutable caller intent within its session's durable journal."
);
identity!(
    PlanId,
    "Identify one saved plan independently of task, session, or native provider identities."
);
identity!(
    InputId,
    "Identify one session input independently of its command or provider submission."
);
identity!(
    InputEditId,
    "Identify the edit reservation required to commit or abandon input changes."
);

identity!(
    ApprovalQuestionSetId,
    "Identify the exact committed approval question set independently of its plan source."
);

identity!(
    ContextCompactionId,
    "Identify a host compaction attempt independently of its native observations."
);

/// Admit a caller intent to the session FIFO; completion does not await an answer.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "command", content = "request", rename_all = "snake_case")]
pub(crate) enum Command {
    /// Control children through their owned lifecycle and input paths.
    Agent(AgentCommand),
    /// Revalidate the captured workflow and dispatch its finite continuation.
    ContinueWorkflow(WorkflowContinuationRequest),
    /// Apply a captured restore or reverse operation through the owner.
    Rollback(RollbackCommand),
    /// Collect current exchange work and settle cancellation before completion.
    CancelExchange(CancelExchangeRequest),
    /// Commit a local policy change; provider settings transitions are added by the same owner.
    ConfigureSession(ConfigurationChange),
    /// Confirm context compaction and any required replacement handoff before completion.
    CompactContext(ContextCompactionRequest),
    /// Submit input or mutate its stored content and delivery eligibility.
    Input(InputCommand),
    /// Control the captured task after collecting affected work where required.
    Task(TaskCommand),
}

impl Command {
    /// Get the finite command log label without recording input or native payloads.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Task(task) => task.name(),
            Self::Rollback(command) => command.name(),
            Self::ContinueWorkflow(_) => "continue_workflow",
            Self::Agent(command) => command.name(),
            Self::CancelExchange(_) => "cancel_exchange",
            Self::CompactContext(_) => "compact_context",
            Self::ConfigureSession(ConfigurationChange::Permission(_)) => {
                "configure_session.permission"
            }
            Self::ConfigureSession(ConfigurationChange::Settings(_)) => {
                "configure_session.settings"
            }
            Self::ConfigureSession(ConfigurationChange::CheckPreferences(_)) => {
                "configure_session.check_preferences"
            }
            Self::ConfigureSession(ConfigurationChange::InputTimeouts(_)) => {
                "configure_session.input_timeouts"
            }
            Self::Input(input) => match input {
                InputCommand::Submit(_) => "input.submit",
                InputCommand::Retry(_) => "input.retry",
                InputCommand::ResumeDelivery => "input.resume_delivery",
                InputCommand::BeginEdit(_) => "input.begin_edit",
                InputCommand::CommitEdit(_) => "input.commit_edit",
                InputCommand::CancelEdit(_) => "input.cancel_edit",
                InputCommand::Remove(_) => "input.remove",
            },
        }
    }
}

/// Select whether an unconsumed cancelled prompt returns to the editor draft.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CancelledInputPolicy {
    /// Keep accepted input and its delivery evidence in their existing journals.
    #[default]
    Retain,
    /// Restore the draft only if the existing retraction rules prove it was unconsumed.
    RestoreIfUnconsumed,
}

/// Cancel the current exchange selected by the owner when this command starts.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct CancelExchangeRequest {
    /// Draft restoration policy, independent of native cancellation evidence.
    pub(crate) input: CancelledInputPolicy,
}

/// Select one input operation without caller-controlled delivery or provider state.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action", content = "request", rename_all = "snake_case")]
pub(crate) enum InputCommand {
    /// Durably accept content and return its input identity before native delivery.
    Submit(InputSubmission),
    /// Re-enable the selected input only after proving replay is safe.
    Retry(InputRevision),
    /// Enable delivery of already eligible queued inputs.
    ResumeDelivery,
    /// Begin editing at the selected input revision and acquire its edit identity.
    BeginEdit(InputEditRequest),
    /// Commit an atomic set of edits owned by the supplied reservation.
    CommitEdit(InputEditCommit),
    /// Release an edit without changing or removing input content.
    CancelEdit(InputEditId),
    /// Remove an input whose revision and unsent status still match.
    Remove(InputRevision),
}

/// Preserve a model choice's explicit clearing separately from an omitted patch.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "change", content = "value", rename_all = "snake_case")]
pub(crate) enum SelectionChange<T> {
    /// Preserve the stored selection.
    Keep,
    /// Store the selected value.
    Set(T),
    /// Restore the provider-neutral default selection.
    Clear,
}

impl<T> Default for SelectionChange<T> {
    /// Preserve omission without requiring the selected value itself to have a default.
    fn default() -> Self {
        Self::Keep
    }
}

/// Select model behavior for one queued input, without arbitrary configuration keys.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QueuedModelSettings {
    /// Omitted model choices preserve the session's committed model.
    pub(crate) model: Option<String>,
    /// Open reasoning catalog identifier advertised by the selected model.
    pub(crate) effort: Option<String>,
    /// Normalized provider service policy, independent of native spellings.
    pub(crate) service_tier: Option<crate::provider::ServiceTier>,
    /// Explicit context-window selection or clearing.
    pub(crate) context_window: SelectionChange<String>,
}

impl QueuedModelSettings {
    /// Get whether this selection leaves every model preference unchanged.
    pub(super) fn is_empty(&self) -> bool {
        self.model.is_none()
            && self.effort.is_none()
            && self.service_tier.is_none()
            && matches!(self.context_window, SelectionChange::Keep)
    }
    /// Decode the legacy configuration object once at the transport boundary.
    pub(super) fn decode(value: serde_json::Value) -> Result<Self, CommandDecodeError> {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Fields {
            model: Option<String>,
            effort: Option<String>,
            service_tier: Option<crate::provider::ServiceTier>,
            context_window: Option<String>,
        }
        let context_present = value.get("context_window").is_some();
        let Fields {
            model,
            effort,
            service_tier,
            context_window,
        } = serde_json::from_value(value)?;
        if model.as_ref().is_some_and(|value| value.trim().is_empty())
            || effort.as_ref().is_some_and(|value| value.trim().is_empty())
            || context_window
                .as_ref()
                .is_some_and(|value| value.trim().is_empty())
        {
            return Err(CommandDecodeError::EmptyModelChoice);
        }
        Ok(Self {
            model,
            effort,
            service_tier,
            context_window: match context_window {
                Some(value) => SelectionChange::Set(value),
                None if context_present => SelectionChange::Clear,
                None => SelectionChange::Keep,
            },
        })
    }

    /// Encode the existing queue journal representation without changing its meaning.
    fn encode(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        if let Some(model) = &self.model {
            fields.insert("model".into(), model.clone().into());
        }
        if let Some(effort) = &self.effort {
            fields.insert("effort".into(), effort.clone().into());
        }
        if let Some(tier) = self.service_tier {
            fields.insert(
                "service_tier".into(),
                serde_json::to_value(tier).expect("service tier has a fixed wire representation"),
            );
        }
        match &self.context_window {
            SelectionChange::Keep => {}
            SelectionChange::Clear => {
                fields.insert("context_window".into(), serde_json::Value::Null);
            }
            SelectionChange::Set(value) => {
                fields.insert("context_window".into(), value.clone().into());
            }
        }
        fields.into()
    }
}

/// Select public clarification intent without granting caller-controlled input purpose or delivery.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "owner", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum InputSubmissionTarget {
    /// Ordinary conversation input; the owner selects its current delivery context.
    #[default]
    Conversation,
    /// Clarify one independent question at its observed content revision.
    Question {
        question_id: crate::question::QuestionId,
        revision: std::num::NonZeroU64,
    },
    /// Clarify one exchange-owned legacy question set.
    Interaction {
        id: crate::provider::ExchangeId,
        set_id: ApprovalQuestionSetId,
        revision: std::num::NonZeroU32,
        question_id: crate::question::QuestionId,
    },
    /// Clarify one planning question without approving its plan.
    Plan {
        id: PlanId,
        set_id: ApprovalQuestionSetId,
        revision: std::num::NonZeroU32,
        question_id: crate::question::QuestionId,
    },
    /// Clarify one review decision without implicitly executing its plan.
    #[serde(alias = "plan_acceptance")]
    PlanReview {
        id: PlanId,
        set_id: ApprovalQuestionSetId,
        revision: std::num::NonZeroU32,
        question_id: crate::question::QuestionId,
    },
}

/// Retain caller content without accepting forged provider delivery metadata.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct InputSubmission {
    /// Explicit question selection, or ordinary conversation input when omitted.
    #[serde(default)]
    pub(crate) target: InputSubmissionTarget,
    /// Session input identity selected by the caller or generated at admission.
    pub(crate) id: InputId,
    /// Literal user content, including editable attachment labels.
    pub(crate) text: String,
    /// Retained attachment references parsed before entering the command boundary.
    pub(crate) attachments: Vec<AttachmentSelection>,
    /// Model preferences applied by internal delivery rather than by submission.
    pub(crate) config: Option<QueuedModelSettings>,
    /// Automatic steering/delivery or explicit queued delivery.
    pub(crate) intent: InputIntent,
}

/// Select an existing input at the exact revision observed by its caller.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct InputRevision {
    /// Selected session input.
    pub(crate) id: InputId,
    /// Expected content/delivery revision.
    pub(crate) revision: u64,
}

/// Acquire an edit identity after validating the selected input revision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct InputEditRequest {
    /// Input and revision to reserve.
    pub(crate) input: InputRevision,
    /// Edit identity retained for commit or cancellation.
    pub(crate) edit: InputEditId,
}

/// Replace one reserved input's draft without changing its delivery history.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct QueuedInputEdit {
    /// Input and revision observed when editing began.
    pub(crate) input: InputRevision,
    /// Replacement literal content.
    pub(crate) text: String,
    /// Replacement retained attachment references.
    pub(crate) attachments: Vec<AttachmentSelection>,
    /// Replacement queued model choices.
    pub(crate) config: Option<QueuedModelSettings>,
}

/// Commit all selected input edits or preserve all previous drafts on rejection.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct InputEditCommit {
    /// Reservation authorizing every supplied edit.
    pub(crate) edit: InputEditId,
    /// Complete replacement drafts in caller order.
    pub(crate) edits: Vec<QueuedInputEdit>,
}

/// Resolve typed caller input policy when its command executes, or preserve an explicit legacy guard.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(untagged, deny_unknown_fields)]
pub(crate) enum InputControl {
    /// Reject a stale legacy caller observation rather than silently resuming delivery.
    Observed {
        /// Delivery-control revision explicitly supplied by the caller.
        revision: u64,
        /// Earlier control operation authorizing automatic delivery resumption.
        after: Option<CommandId>,
    },
    /// Read the owner's committed control policy after earlier FIFO commands finish.
    #[default]
    AtExecution,
}

/// Retain the immutable normalized request independently of each RPC attempt.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct CommandRequest {
    /// Session-scoped idempotency identity.
    pub(crate) id: CommandId,
    /// Typed caller intent; transport request IDs are not stored here.
    pub(crate) command: Command,
    /// Explicit legacy guard or policy resolved when this command reaches execution.
    pub(crate) input_control: InputControl,
}

impl CommandRequest {
    /// Normalize legacy steering into durable session input without promising native delivery.
    pub(crate) fn from_steering(params: serde_json::Value) -> Result<Self, CommandDecodeError> {
        #[derive(Deserialize)]
        struct SteeringFields {
            text: String,
            #[serde(default)]
            attachments: Vec<AttachmentSelection>,
            input_id: Option<InputId>,
        }
        let id = params
            .get("operation_id")
            .or_else(|| params.pointer("/_command/id"))
            .cloned()
            .map(serde_json::from_value::<CommandId>)
            .transpose()?
            .unwrap_or_else(|| {
                CommandId::new(uuid::Uuid::new_v4().to_string())
                    .expect("generated command UUID is nonempty")
            });
        let SteeringFields {
            text,
            attachments,
            input_id,
        } = serde_json::from_value(params)?;
        if text.trim().is_empty() {
            return Err(CommandDecodeError::EmptyInput);
        }
        let input = input_id.unwrap_or_else(|| {
            InputId::new(format!("legacy-input:{}", id.as_str()))
                .expect("a generated input identity includes a validated command ID")
        });
        Ok(Self {
            id,
            command: Command::Input(InputCommand::Submit(InputSubmission {
                target: InputSubmissionTarget::Conversation,
                id: input,
                text,
                attachments,
                config: None,
                intent: InputIntent::Auto,
            })),
            input_control: InputControl::AtExecution,
        })
    }

    /// Attach validated public clarification intent while preserving all host-only purpose guards.
    pub(crate) fn from_targeted_input(
        operation: InputOperation,
        target: InputSubmissionTarget,
    ) -> Result<Self, CommandDecodeError> {
        let mut request = Self::from_input(operation)?;
        let Command::Input(InputCommand::Submit(input)) = &mut request.command else {
            return Err(CommandDecodeError::HostOnlyInput);
        };
        input.target = target;
        Ok(request)
    }

    /// Normalize an input RPC without letting arbitrary JSON cross the owner API.
    pub(crate) fn from_input(operation: InputOperation) -> Result<Self, CommandDecodeError> {
        let InputOperation {
            id,
            control_revision,
            after_control,
            action,
        } = operation;
        let command = match action {
            InputAction::Submit { message } => {
                let InputMessage {
                    id,
                    revision: _,
                    text,
                    purpose,
                    attachments,
                    config,
                    intent,
                    delivery: _,
                    route: _,
                    reservation: _,
                    submission_id: _,
                    error: _,
                } = *message;
                if !matches!(purpose, InputPurpose::User) {
                    return Err(CommandDecodeError::HostOnlyInput);
                }
                InputCommand::Submit(InputSubmission {
                    target: InputSubmissionTarget::Conversation,
                    id: InputId::new(id)?,
                    text,
                    attachments: attachments
                        .into_iter()
                        .map(serde_json::from_value)
                        .collect::<Result<_, _>>()?,
                    config: config.map(QueuedModelSettings::decode).transpose()?,
                    intent,
                })
            }
            InputAction::Retry { id, revision } => InputCommand::Retry(InputRevision {
                id: InputId::new(id)?,
                revision,
            }),
            InputAction::Resume => InputCommand::ResumeDelivery,
            InputAction::Reserve {
                id,
                revision,
                reservation,
            } => InputCommand::BeginEdit(InputEditRequest {
                input: InputRevision {
                    id: InputId::new(id)?,
                    revision,
                },
                edit: InputEditId::new(reservation)?,
            }),
            InputAction::Commit { reservation, edits } => {
                InputCommand::CommitEdit(InputEditCommit {
                    edit: InputEditId::new(reservation)?,
                    edits: edits
                        .into_iter()
                        .map(|edit| {
                            let InputEdit {
                                id,
                                revision,
                                text,
                                attachments,
                                config,
                            } = edit;
                            Ok(QueuedInputEdit {
                                input: InputRevision {
                                    id: InputId::new(id)?,
                                    revision,
                                },
                                text,
                                attachments: attachments
                                    .into_iter()
                                    .map(serde_json::from_value)
                                    .collect::<Result<_, _>>()?,
                                config: config.map(QueuedModelSettings::decode).transpose()?,
                            })
                        })
                        .collect::<Result<_, CommandDecodeError>>()?,
                })
            }
            InputAction::Release { reservation } => {
                InputCommand::CancelEdit(InputEditId::new(reservation)?)
            }
            InputAction::Remove { id, revision } => InputCommand::Remove(InputRevision {
                id: InputId::new(id)?,
                revision,
            }),
        };
        Ok(Self {
            id: CommandId::new(id)?,
            command: Command::Input(command),
            input_control: InputControl::Observed {
                revision: control_revision,
                after: after_control.map(CommandId::new).transpose()?,
            },
        })
    }

    /// Encode a normalized input operation for the existing input domain store.
    pub(crate) fn input_operation(
        &self,
        snapshot: &crate::input::InputSnapshot,
    ) -> Result<InputOperation, serde_json::Error> {
        let command = match &self.command {
            Command::Input(command) => command,
            Command::CancelExchange(_)
            | Command::CompactContext(_)
            | Command::Task(_)
            | Command::ConfigureSession(_)
            | Command::Agent(_)
            | Command::Rollback(_)
            | Command::ContinueWorkflow(_) => {
                unreachable!("only an admitted input command prepares an input operation")
            }
        };
        let action = match command {
            InputCommand::Submit(input) => InputAction::Submit {
                message: Box::new(InputMessage {
                    id: input.id.as_str().into(),
                    revision: 0,
                    text: input.text.clone(),
                    purpose: InputPurpose::User,
                    attachments: input
                        .attachments
                        .iter()
                        .map(serde_json::to_value)
                        .collect::<Result<_, _>>()?,
                    config: input.config.as_ref().map(QueuedModelSettings::encode),
                    intent: input.intent.clone(),
                    delivery: crate::input::InputDelivery::Queued,
                    route: None,
                    reservation: None,
                    submission_id: None,
                    error: None,
                }),
            },
            InputCommand::Retry(input) => InputAction::Retry {
                id: input.id.as_str().into(),
                revision: input.revision,
            },
            InputCommand::ResumeDelivery => InputAction::Resume,
            InputCommand::BeginEdit(request) => InputAction::Reserve {
                id: request.input.id.as_str().into(),
                revision: request.input.revision,
                reservation: request.edit.as_str().into(),
            },
            InputCommand::CommitEdit(request) => InputAction::Commit {
                reservation: request.edit.as_str().into(),
                edits: request
                    .edits
                    .iter()
                    .map(|edit| {
                        Ok(InputEdit {
                            id: edit.input.id.as_str().into(),
                            revision: edit.input.revision,
                            text: edit.text.clone(),
                            attachments: edit
                                .attachments
                                .iter()
                                .map(serde_json::to_value)
                                .collect::<Result<_, _>>()?,
                            config: edit.config.as_ref().map(QueuedModelSettings::encode),
                        })
                    })
                    .collect::<Result<_, serde_json::Error>>()?,
            },
            InputCommand::CancelEdit(edit) => InputAction::Release {
                reservation: edit.as_str().into(),
            },
            InputCommand::Remove(input) => InputAction::Remove {
                id: input.id.as_str().into(),
                revision: input.revision,
            },
        };
        let (control_revision, after_control) = match &self.input_control {
            InputControl::Observed { revision, after } => {
                (*revision, after.as_ref().map(|id| id.as_str().to_owned()))
            }
            InputControl::AtExecution => (
                snapshot.control_revision,
                snapshot.control_operation_id.clone(),
            ),
        };
        Ok(InputOperation {
            id: self.id.as_str().into(),
            control_revision,
            after_control,
            action,
        })
    }
}

/// Reject malformed caller intent before durable queue admission.
#[derive(Debug, thiserror::Error)]
pub(crate) enum CommandDecodeError {
    /// A legacy steering request supplied no user content.
    #[error("steering requires nonempty input text")]
    EmptyInput,
    /// A configuration patch must select at least one supported policy field.
    #[error("configuration patch must contain a supported policy field")]
    EmptyConfiguration,
    /// A command or its task/input/edit target supplied empty authority.
    #[error("command and target identities must be nonempty")]
    EmptyIdentity,
    /// Caller attempted to manufacture host-generated task/question input.
    #[error("task and question delivery input can only be generated by Harness")]
    HostOnlyInput,
    /// Model catalog selections must not be empty strings.
    #[error("queued model choices must be nonempty")]
    EmptyModelChoice,
    /// Transport content did not match the typed command contract.
    #[error("invalid command: {0}")]
    Decode(#[from] serde_json::Error),
}

/// Preserve a finite command failure category independently of display wording.
#[derive(Clone, Debug, Deserialize, Serialize, thiserror::Error)]
#[serde(tag = "failure", content = "reason", rename_all = "snake_case")]
pub(crate) enum CommandFailure {
    /// Child agent admission or its captured authority failed.
    #[error(transparent)]
    Agent(#[from] AgentFailure),
    /// Restore authority or application failed without silently retargeting the journal.
    #[error(transparent)]
    Rollback(#[from] RollbackFailure),
    /// Task authority, its commit, or required native-goal synchronization failed.
    #[error(transparent)]
    Task(#[from] TaskCommandFailure),
    /// Task state committed, but required goal synchronization did not establish completion.
    #[error("task change committed, but native synchronization failed: {failure}")]
    TaskSynchronization {
        committed: TaskCommandCheckpoint,
        failure: TaskSynchronizationFailure,
    },
    /// Context compaction failed without conflating native uncertainty with storage failure.
    #[error(transparent)]
    Compaction(#[from] ContextCompactionFailure),
    /// Input content or delivery authority changed before the command could apply.
    #[error(transparent)]
    Input(#[from] InputRejection),
    /// A prior process exited before this admitted operation's result committed.
    #[error("command was interrupted by a host restart; inspect retained state before retrying")]
    Interrupted,
    /// Native delivery remains uncertain and cannot be replayed.
    #[error("input delivery is still unconfirmed; the input was not resent")]
    DeliveryUncertain,
    /// Retained attachments could not be validated before committing an edit.
    #[error("a retained input attachment is unavailable")]
    AttachmentUnavailable,
    /// A legacy receipt records rejection without a machine-readable category.
    #[error("the stored input operation was rejected by an earlier runtime")]
    LegacyInputRejected,
    /// A prerequisite failed without a supported domain recovery category.
    #[error("the command prerequisite could not be completed")]
    PrerequisiteFailed,
    /// The host closed admission before queued work began.
    #[error("the session closed before the command started")]
    OwnerStopping,
    /// Compatibility admission could not be acquired before any collection work began.
    #[error("session ownership was unavailable; the command could not proceed")]
    OwnershipUnavailable,
    /// Native handoff was not confirmed; no replay is authorized by this failure.
    #[error("provider dispatch was not confirmed; execution remains owned for collection")]
    DispatchUnconfirmed,
    /// Native or host collection could not establish safe execution availability.
    #[error("execution collection did not establish quiescence; replacement remains blocked")]
    CollectionUncertain,
    /// Native collection still owns work after its observation deadline elapsed.
    #[error(
        "execution collection exceeded its deadline; cleanup remains owned and replacement is blocked"
    )]
    CollectionTimedOut,
    /// The selected current exchange changed before its command could commit.
    #[error("the selected exchange changed before the command could apply")]
    ExchangeChanged,
    /// A settings job remains retained after its observation deadline.
    #[error("configuration exceeded its deadline; pending work remains owned for collection")]
    ConfigurationTimedOut,
    /// Preflight rejected the selection before collecting or changing native work.
    #[error(transparent)]
    ConfigurationRejected(#[from] ConfigurationRejection),
    /// Native settings application failed before its host commit; execution remains fenced.
    #[error(
        "provider settings reconciliation failed ({kind:?}); execution remains blocked for recovery"
    )]
    ConfigurationReconcileFailed {
        kind: forge_provider_api::ProviderFailureKind,
    },
}

impl CommandFailure {
    /// Get a stable host failure code without interpreting provider or diagnostic text.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::Task(failure) => failure.code(),
            Self::Rollback(failure) => failure.code(),
            Self::Agent(failure) => failure.code(),
            Self::TaskSynchronization { failure, .. } => failure.code(),
            Self::Compaction(failure) => failure.code(),
            Self::Input(_) => "input_rejected",
            Self::Interrupted => "command_interrupted",
            Self::DeliveryUncertain => "input_delivery_uncertain",
            Self::AttachmentUnavailable => "input_attachment_unavailable",
            Self::LegacyInputRejected => "legacy_input_rejected",
            Self::PrerequisiteFailed => "command_prerequisite_failed",
            Self::OwnerStopping => "command_owner_stopping",
            Self::OwnershipUnavailable => "session_ownership_unavailable",
            Self::DispatchUnconfirmed => "provider_dispatch_unconfirmed",
            Self::CollectionUncertain => "execution_collection_uncertain",
            Self::CollectionTimedOut => "execution_collection_timeout",
            Self::ExchangeChanged => "command_exchange_changed",
            Self::ConfigurationTimedOut => "configuration_timeout",
            Self::ConfigurationRejected(reason) => reason.code(),
            Self::ConfigurationReconcileFailed { .. } => "configuration_reconciliation_failed",
        }
    }
}

/// Capture a committed command outcome separately from transport observation failure.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct CommandResult {
    /// Immutable session command identity.
    pub(crate) id: CommandId,
    /// FIFO sequence assigned by durable admission.
    pub(crate) sequence: u64,
    /// Committed command-record revision.
    pub(crate) revision: u64,
    /// Typed command-specific result.
    pub(crate) outcome: CommandOutcome,
}

/// Describe the finite postcondition that released the active command slot.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "outcome", content = "result", rename_all = "snake_case")]
pub(crate) enum CommandOutcome {
    /// The child operation reached its finite admission or control result.
    Agent(AgentOutcome),
    /// The captured restore operation committed its finite result.
    Rollback(RollbackOutcome),
    /// Task policy or lifecycle committed and any required native synchronization settled.
    Task(TaskCommandOutcome),
    /// Context replacement was confirmed and any required continuation handoff completed.
    Compacted(ContextCompactionOutcome),
    /// Owned work settled or the owner established that there was no current work.
    Cancelled(CancellationOutcome),
    /// Effective configuration committed without waiting for subsequent exchange work.
    Configured(ConfigurationOutcome),
    /// Input acceptance/content mutation committed; native delivery is separate.
    Input {
        /// Committed input-queue publication revision.
        queue_revision: u64,
        /// Typed postcondition and identities needed for subsequent caller actions.
        change: InputChange,
    },
}

/// Distinguish an actual cancellation from already-completed or absent exchange work.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "disposition", rename_all = "snake_case")]
pub(crate) enum CancellationOutcome {
    /// No current root exchange required cancellation; terminal history is unchanged.
    NoWork,
    /// The selected exchange reached its cancelled, collected postcondition.
    Cancelled {
        exchange: crate::provider::ExchangeId,
    },
}

/// Preserve input/edit identities without confusing acceptance with native delivery.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "change", rename_all = "snake_case")]
pub(crate) enum InputChange {
    /// A new input was durably accepted for later internal delivery.
    Submitted { input: InputRevision },
    /// A definitely unsent input became eligible for another delivery attempt.
    RetryEligible { input: InputRevision },
    /// Reconciliation proved an earlier attempt was already accepted.
    DeliveryReconciled { input: InputId },
    /// Delivery policy was resumed without retrying failed or uncertain input.
    DeliveryResumed,
    /// The selected input is now reserved by the returned edit identity.
    EditBegun {
        input: InputRevision,
        edit: InputEditId,
    },
    /// The complete reserved set was edited atomically.
    Edited { inputs: Vec<InputEditResult> },
    /// A reservation was released while retaining its original input content.
    EditCancelled { edit: InputEditId },
    /// The selected unsent input was removed.
    Removed { input: InputId },
}

/// Preserve the existing blank-draft removal behavior of an atomic edit commit.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "result", content = "input", rename_all = "snake_case")]
pub(crate) enum InputEditResult {
    /// The input remains queued with this new content revision.
    Updated(InputRevision),
    /// An empty draft without configuration removed this input.
    Removed(InputId),
}

/// Distinguish admission rejection, durable failure, and lost completion observation.
#[derive(Clone, Debug, thiserror::Error)]
pub(crate) enum ExecuteCommandError {
    /// No durable admission occurred because the bounded queue was full.
    #[error("session command queue is full")]
    QueueFull,
    /// The session owner has closed command admission.
    #[error("session command owner is unavailable")]
    OwnerClosed,
    /// A durable identity already describes a different intent.
    #[error("command identity was reused for a different intent")]
    IdentityConflict,
    /// The command's typed failure was committed before notification.
    #[error("command failed: {0}")]
    Failed(CommandFailure),
    /// Storage could not establish a committed result; effects may have occurred.
    #[error("command outcome could not be committed; inspect retained state before retrying")]
    OutcomeUncommitted,
    /// The response channel ended without establishing the operation outcome.
    #[error("command completion observation was lost; inspect the command by its identity")]
    ObservationLost,
}

impl ExecuteCommandError {
    /// Encode a stable RPC failure code without requiring clients to parse reasons.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::QueueFull => "command_queue_full",
            Self::OwnerClosed => "command_owner_closed",
            Self::IdentityConflict => "command_identity_conflict",
            Self::OutcomeUncommitted => "command_outcome_uncommitted",
            Self::ObservationLost => "command_observation_lost",
            Self::Failed(failure) => failure.code(),
        }
    }
}
