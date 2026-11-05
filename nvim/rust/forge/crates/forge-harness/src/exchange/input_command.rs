//! Prepare input I/O independently and apply its results only in the exchange owner.

use super::services::OwnerServices;
use std::{path::PathBuf, sync::Arc};

use super::command::{Command, CommandFailure, InputCommand, InputRevision};
use super::queue::QueuedCommand;
use crate::input::{InputDelivery, InputUpdate};
use crate::provider::submission::{Submission, SubmissionReconciliation, SubmissionState};

/// Hold a command's active stage and resolver ownership in the persistent driver.
pub(super) struct InputCommandRun {
    /// Durable identity and all attached completion observers.
    pub(super) command: QueuedCommand,
    stage: InputStage,
}

impl InputCommandRun {
    /// Prepare a command only after its active revision has committed.
    pub(super) fn start(command: QueuedCommand, services: &OwnerServices) -> Self {
        let input = match &command.record.request.command {
            Command::Input(input) => input,
            Command::Agent(_)
            | Command::ContinueWorkflow(_)
            | Command::Rollback(_)
            | Command::Task(_)
            | Command::CancelExchange(_)
            | Command::CompactContext(_)
            | Command::ConfigureSession(_) => {
                unreachable!("input stages only own input commands")
            }
        };
        let stage = match input {
            InputCommand::Submit(input) if !input.attachments.is_empty() => {
                prepare_attachments(services.root.clone(), vec![input.attachments.clone()])
            }
            InputCommand::CommitEdit(edits)
                if edits.edits.iter().any(|edit| !edit.attachments.is_empty()) =>
            {
                prepare_attachments(
                    services.root.clone(),
                    edits
                        .edits
                        .iter()
                        .map(|edit| edit.attachments.clone())
                        .collect(),
                )
            }
            InputCommand::Retry(input) => prepare_retry(input, services),
            InputCommand::Submit(_)
            | InputCommand::ResumeDelivery
            | InputCommand::BeginEdit(_)
            | InputCommand::CommitEdit(_)
            | InputCommand::CancelEdit(_)
            | InputCommand::Remove(_) => InputStage::Ready(Ok(PreparedInput::Ordinary)),
        };
        Self { command, stage }
    }

    /// Await preparation while the owner continues reading commands and provider facts.
    pub(super) async fn next(&mut self) -> Result<PreparedInput, CommandFailure> {
        let result = match &mut self.stage {
            InputStage::Ready(_) => {
                let InputStage::Ready(result) =
                    std::mem::replace(&mut self.stage, InputStage::Consumed)
                else {
                    unreachable!("ready input stage was selected synchronously")
                };
                return result;
            }
            InputStage::Waiting(task) => task
                .await
                .unwrap_or(Err(CommandFailure::PrerequisiteFailed)),
            InputStage::Consumed => std::future::pending().await,
        };
        self.stage = InputStage::Consumed;
        result
    }
}

enum InputStage {
    Ready(Result<PreparedInput, CommandFailure>),
    Waiting(tokio::task::JoinHandle<Result<PreparedInput, CommandFailure>>),
    Consumed,
}

/// Carry preparation evidence without giving a worker input/exchange mutation authority.
pub(super) enum PreparedInput {
    /// Attachments and ordinary command prerequisites are ready.
    Ordinary,
    /// Native delivery was reconciled under a binding retirement permit.
    Reconciled {
        input: InputRevision,
        submission: Arc<Submission>,
        observation: Box<SubmissionReconciliation>,
    },
}

/// Validate the retained attachment sources without applying input or lifecycle changes.
fn prepare_attachments(
    root: PathBuf,
    drafts: Vec<Vec<crate::attachment::AttachmentSelection>>,
) -> InputStage {
    InputStage::Waiting(tokio::spawn(async move {
        for attachments in drafts {
            crate::attachment::resolve_input(
                &root,
                &serde_json::json!({"attachments":attachments}),
            )
            .await
            .map_err(|_| CommandFailure::AttachmentUnavailable)?;
        }
        Ok(PreparedInput::Ordinary)
    }))
}

/// Reconcile uncertain native receipts before the owner decides retry eligibility.
fn prepare_retry(input: &InputRevision, services: &OwnerServices) -> InputStage {
    let snapshot = match services.input.snapshot() {
        Ok(snapshot) => snapshot,
        Err(_) => return InputStage::Ready(Err(CommandFailure::PrerequisiteFailed)),
    };
    let Some(message) = snapshot
        .messages
        .iter()
        .find(|message| message.id == input.id.as_str())
    else {
        return InputStage::Ready(Err(crate::input::InputRejection::InputMissing.into()));
    };
    if message.revision != input.revision {
        return InputStage::Ready(Err(crate::input::InputRejection::RevisionChanged.into()));
    }
    let Some(submission) = &message.submission_id() else {
        return InputStage::Ready(Ok(PreparedInput::Ordinary));
    };
    if !matches!(
        message.delivery(),
        InputDelivery::Unknown | InputDelivery::Failed
    ) {
        return InputStage::Ready(Ok(PreparedInput::Ordinary));
    }
    let submission = match services.submissions.submission(submission) {
        Ok(submission) => submission,
        Err(_) => return InputStage::Ready(Err(CommandFailure::PrerequisiteFailed)),
    };
    let input = input.clone();
    let lane = services.provider_lane.clone();
    let provider = services.provider.clone();
    InputStage::Waiting(tokio::spawn(async move {
        let _permit = lane.read_owned().await;
        let provider = provider
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let observation = submission
            .observe_reconciliation(provider.as_ref())
            .await
            .map_err(|_| CommandFailure::DeliveryUncertain)?;
        Ok(PreparedInput::Reconciled {
            input,
            submission,
            observation: Box::new(observation),
        })
    }))
}

/// Revalidate preparation and commit input mutation in the owning update step.
pub(super) fn apply_input(
    run: &InputCommandRun,
    prepared: PreparedInput,
    services: &OwnerServices,
) -> Result<InputUpdate, InputCommandFailure> {
    let policy = services
        .input
        .snapshot()
        .map_err(InputCommandFailure::Domain)?;
    let mut operation = run
        .command
        .record
        .request
        .input_operation(&policy)
        .map_err(InputCommandFailure::Codec)?;
    match prepared {
        PreparedInput::Ordinary => {
            let target = match &run.command.record.request.command {
                Command::Input(InputCommand::Submit(input)) => Some(&input.target),
                _ => None,
            };
            let question = target
                .map(|target| super::input_target::prepare(services, &mut operation, target))
                .transpose()?
                .flatten();
            let update = services
                .input
                .apply(operation)
                .map_err(InputCommandFailure::Domain)?;
            if update.error.is_none()
                && let Some(question) = question
            {
                services
                    .questions
                    .admitted(&question)
                    .map_err(InputCommandFailure::Domain)?;
            }
            Ok(update)
        }
        PreparedInput::Reconciled {
            input,
            submission,
            observation,
        } => {
            let snapshot = services
                .input
                .snapshot()
                .map_err(InputCommandFailure::Domain)?;
            let message = snapshot
                .messages
                .iter()
                .find(|message| message.id == input.id.as_str())
                .ok_or(InputCommandFailure::Rejected(
                    crate::input::InputRejection::InputMissing.into(),
                ))?;
            if message.revision != input.revision
                || message.submission_id() != Some(submission.id())
            {
                return Err(InputCommandFailure::Rejected(
                    crate::input::InputRejection::RevisionChanged.into(),
                ));
            }
            let record = submission
                .apply_reconciliation(*observation)
                .map_err(InputCommandFailure::Domain)?;
            match &record.delivery {
                SubmissionState::Accepted { .. } => services
                    .input
                    .acknowledge_retry(operation)
                    .map_err(InputCommandFailure::Domain),
                SubmissionState::Sending { .. } => Err(InputCommandFailure::Rejected(
                    CommandFailure::DeliveryUncertain,
                )),
                SubmissionState::Prepared | SubmissionState::Cancelled => {
                    submission.cancel().map_err(InputCommandFailure::Domain)?;
                    services
                        .input
                        .retry_reconciled(operation, &record)
                        .map_err(InputCommandFailure::Domain)
                }
            }
        }
    }
}

/// Preserve typed rejections separately from storage/legacy-domain commit failures.
#[derive(Debug, thiserror::Error)]
pub(super) enum InputCommandFailure {
    /// A durable caller failure can be recorded without pretending a domain write succeeded.
    #[error(transparent)]
    Rejected(CommandFailure),
    /// Transport encoding failed before a domain write.
    #[error("input operation could not be encoded: {0}")]
    Codec(serde_json::Error),
    /// The existing domain API failed; the owner must treat its outcome as uncommitted.
    #[error("input domain commit failed: {0}")]
    Domain(#[source] anyhow::Error),
}
