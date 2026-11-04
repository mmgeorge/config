//! Admit question replies and chat continuations at the exchange owner's update boundary.

use forge_protocol::{message::SessionEvent, outbound::MessageSender};
use tokio::time::Instant;

use super::services::OwnerServices;
use crate::input::{
    InputAction, InputDelivery, InputIntent, InputMessage, InputOperation, InputPurpose,
    InputRejection, InputUpdate,
};

const ADMISSION_BUDGET: usize = 8;

/// Describe bounded internal scheduling progress, independently of native input delivery.
pub(crate) struct QuestionInputProgress {
    /// Current committed question-list revision after this update step.
    pub(crate) revision: u64,
    /// Number of durable reply/continuation admissions acknowledged in this step.
    pub(crate) admitted: usize,
    /// More eligible internal input remains; no caller command is enqueued to process it.
    pub(crate) remaining: bool,
}

/// Retain automatic question scheduling and notification deduplication on the exchange owner.
pub(super) struct QuestionInputs {
    requested: bool,
    retry_at: Option<Instant>,
    published_revision: Option<u64>,
}

impl Default for QuestionInputs {
    /// Request initial recovery of unacknowledged question outbox records without requiring a UI.
    fn default() -> Self {
        Self {
            requested: true,
            retry_at: None,
            published_revision: None,
        }
    }
}

impl QuestionInputs {
    /// Request reconciliation after committed question changes or an explicit internal readiness event.
    pub(super) fn request(&mut self) {
        self.requested = true;
    }

    /// Apply at most one bounded batch and retain retry policy without blocking native work.
    pub(super) fn update(
        &mut self,
        services: &OwnerServices,
        messages: Option<&MessageSender>,
    ) -> Result<Option<QuestionInputProgress>, QuestionInputError> {
        if !self.requested
            || self
                .retry_at
                .is_some_and(|deadline| Instant::now() < deadline)
        {
            return Ok(None);
        }
        let result = self.reconcile(services, messages);
        match &result {
            Ok(progress) => {
                self.requested = progress.remaining;
                self.retry_at = None;
            }
            Err(_) => {
                self.retry_at = Some(Instant::now() + std::time::Duration::from_millis(250));
            }
        }
        result.map(Some)
    }

    /// Observe changes and bounded retry readiness without retaining any domain lock while waiting.
    pub(super) async fn next(&self, changes: &mut tokio::sync::watch::Receiver<u64>) {
        if self.requested {
            match self.retry_at {
                Some(deadline) => tokio::time::sleep_until(deadline).await,
                None => tokio::task::yield_now().await,
            }
            return;
        }
        let _ = changes.changed().await;
    }

    /// Commit durable input before acknowledging its question source; recover retries through that receipt.
    fn reconcile(
        &mut self,
        services: &OwnerServices,
        messages: Option<&MessageSender>,
    ) -> Result<QuestionInputProgress, QuestionInputError> {
        let mut admitted = 0;
        for message in services
            .questions
            .outbox()
            .map_err(QuestionInputError::QuestionState)?
            .into_iter()
            .take(ADMISSION_BUDGET)
        {
            let id = message.id.clone();
            let update = admit(
                services,
                format!("question:{}", message.id),
                InputMessage {
                    id: message.id,
                    revision: 0,
                    text: message.text,
                    purpose: InputPurpose::Question {
                        content_revision: Some(message.reply.revision),
                        question_id: message.question_id,
                        task_id: message.task_id,
                        message_kind: message.kind,
                        reply: message.reply,
                    },
                    attachments: Vec::new(),
                    config: None,
                    intent: InputIntent::Auto,
                    delivery: InputDelivery::Queued,
                    route: None,
                    reservation: None,
                    submission_id: None,
                    error: None,
                },
            )?;
            publish_input(services, messages, &id, update)?;
            services
                .questions
                .admitted(&id)
                .map_err(QuestionInputError::QuestionState)?;
            admitted += 1;
        }

        // Required decisions and unacknowledged replies gate chat continuation admission.
        for exchange in services
            .questions
            .ready_chat_continuations()
            .map_err(QuestionInputError::QuestionState)?
            .into_iter()
            .take(ADMISSION_BUDGET - admitted)
        {
            let original = crate::storage::SqliteStore::read_only(&services.root)
                .map_err(QuestionInputError::SourceState)?
                .load_exchange(&exchange)
                .map_err(QuestionInputError::SourceState)?
                .ok_or(QuestionInputError::SourceMissing)?;
            let id = format!("question-continuation:{exchange}");
            let update = admit(
                services,
                id.clone(),
                InputMessage {
                    id,
                    revision: 0,
                    text: format!(
                        "Continue the original request using the recorded user answers. Do not repeat resolved questions.\n\nOriginal request:\n{}",
                        original.prompt
                    ),
                    purpose: InputPurpose::QuestionContinuation {
                        exchange_id: exchange.clone(),
                    },
                    attachments: Vec::new(),
                    config: None,
                    intent: InputIntent::Queue,
                    delivery: InputDelivery::Queued,
                    route: None,
                    reservation: None,
                    submission_id: None,
                    error: None,
                },
            )?;
            let id = format!("question-continuation:{exchange}");
            publish_input(services, messages, &id, update)?;
            services
                .questions
                .chat_continuation_admitted(&exchange)
                .map_err(QuestionInputError::QuestionState)?;
            admitted += 1;
        }
        let snapshot = services
            .questions
            .snapshot()
            .map_err(QuestionInputError::QuestionState)?;
        let revision = snapshot.revision;
        if self.published_revision != Some(revision) {
            services.publications.publish_once(
                super::publication::PublicationOrigin::Questions(revision),
                SessionEvent {
                    session_id: services.observer.session_id().into(),
                    event: "questions_changed".into(),
                    payload: serde_json::to_value(snapshot)?,
                },
                messages,
            )?;
            self.published_revision = Some(revision);
        }
        let remaining = !services
            .questions
            .outbox()
            .map_err(QuestionInputError::QuestionState)?
            .is_empty()
            || !services
                .questions
                .ready_chat_continuations()
                .map_err(QuestionInputError::QuestionState)?
                .is_empty();
        Ok(QuestionInputProgress {
            revision,
            admitted,
            remaining,
        })
    }
}

/// Keep normalized scheduling decisions distinct from legacy store implementation errors.
#[derive(Debug, thiserror::Error)]
pub(crate) enum QuestionInputError {
    #[error("question input owner is closed")]
    #[cfg(test)]
    OwnerClosed,
    #[error("question input readiness queue is full")]
    #[cfg(test)]
    QueueFull,
    #[error("question input readiness response was not observed")]
    #[cfg(test)]
    ObservationLost,
    #[error("question state is unavailable")]
    QuestionState(#[source] anyhow::Error),
    #[error("question input journal is unavailable")]
    InputState(#[source] anyhow::Error),
    #[error("question input admission receipt is unavailable")]
    ReceiptUnavailable(#[from] crate::input::InputJournalUnavailable),
    #[error("question input admission receipt is missing after commit")]
    ReceiptMissing,
    #[error("question continuation source is unavailable")]
    SourceState(#[source] anyhow::Error),
    #[error("question continuation lost its original request")]
    SourceMissing,
    #[error("question input was rejected: {0}")]
    Rejected(InputRejection),
    #[error("legacy question input admission was rejected")]
    LegacyRejected,
    #[error("question input admission identity refers to different content")]
    IdentityConflict,
    #[error("question notification could not be encoded")]
    Codec(#[from] serde_json::Error),
    #[error("question notification could not be retained")]
    Publication(#[from] super::publication::PublicationError),
}

impl QuestionInputError {
    /// Get the stable diagnostic code without parsing a native or store error message.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            #[cfg(test)]
            Self::OwnerClosed => "question_input_owner_closed",
            #[cfg(test)]
            Self::QueueFull => "question_input_queue_full",
            #[cfg(test)]
            Self::ObservationLost => "question_input_observation_lost",
            Self::QuestionState(_) => "question_state_unavailable",
            Self::InputState(_) => "question_input_state_unavailable",
            Self::ReceiptUnavailable(_) | Self::ReceiptMissing => {
                "question_input_receipt_unavailable"
            }
            Self::SourceState(_) => "question_continuation_source_unavailable",
            Self::SourceMissing => "question_continuation_source_missing",
            Self::Rejected(_) | Self::LegacyRejected => "question_input_rejected",
            Self::IdentityConflict => "question_input_identity_conflict",
            Self::Codec(_) => "question_notification_encoding_failed",
            Self::Publication(_) => "question_notification_retention_failed",
        }
    }
}

/// Reuse the original admission receipt before capturing current input-control authority on recovery.
fn admit(
    services: &OwnerServices,
    operation_id: String,
    message: InputMessage,
) -> Result<InputUpdate, QuestionInputError> {
    let update = match services.input.admission_receipt(&operation_id)? {
        Some(update) => {
            if update.error.is_none()
                && !update
                    .upsert
                    .iter()
                    .any(|saved| same_input(saved, &message))
            {
                return Err(QuestionInputError::IdentityConflict);
            }
            update
        }
        None => {
            let snapshot = services
                .input
                .snapshot()
                .map_err(QuestionInputError::InputState)?;
            services
                .input
                .apply(InputOperation {
                    id: operation_id.clone(),
                    control_revision: snapshot.control_revision,
                    after_control: snapshot.control_operation_id,
                    action: InputAction::Submit {
                        message: Box::new(message),
                    },
                })
                .map_err(QuestionInputError::InputState)?;
            services
                .input
                .admission_receipt(&operation_id)?
                .ok_or(QuestionInputError::ReceiptMissing)?
        }
    };
    match update.error_code {
        Some(rejection) => Err(QuestionInputError::Rejected(rejection)),
        None if update.error.is_some() => Err(QuestionInputError::LegacyRejected),
        None => Ok(update),
    }
}

/// Compare immutable question content, preserving typed answer and continuation identity on recovery.
fn same_input(saved: &InputMessage, expected: &InputMessage) -> bool {
    let purpose_matches = match (&saved.purpose, &expected.purpose) {
        (
            InputPurpose::Question {
                content_revision: saved_revision,
                question_id: saved_question,
                task_id: saved_task,
                message_kind: saved_kind,
                reply: saved_reply,
            },
            InputPurpose::Question {
                content_revision,
                question_id,
                task_id,
                message_kind,
                reply,
            },
        ) => {
            saved_revision.is_none_or(|saved| Some(saved) == *content_revision)
                && saved_question == question_id
                && saved_task == task_id
                && saved_kind == message_kind
                && saved_reply == reply
        }
        (
            InputPurpose::QuestionContinuation {
                exchange_id: saved_exchange,
            },
            InputPurpose::QuestionContinuation { exchange_id },
        ) => saved_exchange == exchange_id,
        (
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::WorkflowContinuation { .. }
            | InputPurpose::TaskEntry { .. }
            | InputPurpose::TaskContinuation { .. },
            _,
        ) => false,
        (
            InputPurpose::Question { .. },
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::QuestionContinuation { .. }
            | InputPurpose::WorkflowContinuation { .. }
            | InputPurpose::TaskEntry { .. }
            | InputPurpose::TaskContinuation { .. },
        ) => false,
        (
            InputPurpose::QuestionContinuation { .. },
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::WorkflowContinuation { .. }
            | InputPurpose::Question { .. }
            | InputPurpose::TaskEntry { .. }
            | InputPurpose::TaskContinuation { .. },
        ) => false,
    };
    purpose_matches
        && saved.id == expected.id
        && saved.text == expected.text
        && saved.attachments == expected.attachments
        && saved.config == expected.config
        && saved.intent == expected.intent
}

/// Retain committed queue progress without waiting for a UI channel or native acceptance.
fn publish_input(
    services: &OwnerServices,
    messages: Option<&MessageSender>,
    message_id: &str,
    update: InputUpdate,
) -> Result<(), QuestionInputError> {
    let (revision, pending) = services
        .input
        .has_pending()
        .map_err(QuestionInputError::InputState)?;
    services
        .observer
        .completion_queue
        .update(revision, pending)
        .map_err(QuestionInputError::InputState)?;
    let payload = serde_json::to_value(update)?;
    services.publications.publish_once(
        super::publication::PublicationOrigin::QuestionInput(
            super::command::InputId::new(message_id)
                .map_err(|_| QuestionInputError::IdentityConflict)?,
        ),
        SessionEvent {
            session_id: services.observer.session_id().into(),
            event: "input_changed".into(),
            payload,
        },
        messages,
    )?;
    Ok(())
}
