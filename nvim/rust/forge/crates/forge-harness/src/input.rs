use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

mod authority;
mod journal;
mod message;
pub(crate) use authority::{InputAuthorityError, input_purpose_is_current};
pub(crate) use journal::reserve_workflow_input;
pub(crate) use message::InputDraft;
pub use message::InputMessage;
mod rejection;
pub(crate) use rejection::InputRejection;

/// Distinguish an unavailable retained journal from a missing operation receipt.
#[derive(Debug, thiserror::Error)]
#[error("input operation journal is unavailable")]
pub(crate) struct InputJournalUnavailable;

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputIntent {
    #[default]
    Auto,
    Queue,
}

/// Selects whether admission starts a queued turn or adds input to the active turn.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum InputRoute {
    Turn,
    Steering,
}

/// Require observed queued/claimed content or its bound receipt before updating delivery.
pub(crate) enum InputDeliveryGuard<'a> {
    /// Settle local work only while the original unsent revision is still unreserved.
    Queued(u64),
    /// Bind a receipt only to the exact input revision claimed before preparation.
    Claimed(u64),
    /// Apply later evidence only to the input still bound to this receipt.
    Receipt(&'a forge_provider_api::SubmissionId),
}

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputDelivery {
    #[default]
    Queued,
    Delivering,
    Accepted,
    Unknown,
    Failed,
}

/// Keeps user input independent of task selection and fences generated task input by revision.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InputPurpose {
    #[default]
    User,
    /// Preserve the exact legacy planning/exchange question selected by the caller.
    Clarification {
        target: crate::exchange::command::InputSubmissionTarget,
    },
    Question {
        #[serde(default, skip_serializing_if = "Option::is_none")]
        content_revision: Option<u64>,
        question_id: crate::question::QuestionId,
        task_id: Option<String>,
        message_kind: crate::question::QuestionMessageKind,
        reply: forge_provider_api::QuestionReply,
    },
    /// Keep taskless continuation tied to the immutable exchange selected before admission.
    WorkflowContinuation {
        exchange: crate::exchange::command::ExchangeContinuationSelection,
    },
    QuestionContinuation {
        exchange_id: String,
    },
    TaskEntry {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        /// Announce the task only on its first entry; a resumed entry continues the same task.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        start: Option<crate::exchange::TaskStart>,
    },
    TaskContinuation {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        completed_turn_id: crate::task::TaskTurnId,
        reason: String,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputEdit {
    pub id: String,
    pub revision: u64,
    pub text: String,
    #[serde(default)]
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InputAction {
    Retry {
        id: String,
        revision: u64,
    },
    Resume,
    Submit {
        message: Box<InputMessage>,
    },
    Reserve {
        id: String,
        revision: u64,
        reservation: String,
    },
    Commit {
        reservation: String,
        edits: Vec<InputEdit>,
    },
    Release {
        reservation: String,
    },
    Remove {
        id: String,
        revision: u64,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputOperation {
    pub id: String,
    #[serde(default)]
    pub control_revision: u64,
    pub after_control: Option<String>,
    pub action: InputAction,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InputUpdate {
    pub snapshot: Option<InputSnapshot>,
    pub operation_id: Option<String>,
    pub revision: u64,
    pub upsert: Vec<InputMessage>,
    pub removed: Vec<String>,
    pub suspended: bool,
    pub control_revision: u64,
    pub error: Option<String>,
    /// Normalized rejection, absent on success and on receipts from older runtimes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) error_code: Option<InputRejection>,
    pub prepend: bool,
    pub retracted: Option<String>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InputSnapshot {
    pub revision: u64,
    pub messages: Vec<InputMessage>,
    pub suspended: bool,
    pub control_revision: u64,
    pub control_operation_id: Option<String>,
}

#[derive(Clone, Default)]
struct InputState {
    snapshot: InputSnapshot,
    submitted: std::collections::HashSet<String>,
    operation: HashMap<String, (String, InputUpdate)>,
}

/// Own one session's accepted input and commit mutations before publishing them.
#[derive(Default)]
pub(crate) struct SessionInputQueue {
    state: Mutex<InputState>,
    journal: Option<Mutex<journal::InputJournal>>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionInputQueue {
    /// Restore accepted input suspended, retaining delivery evidence without replaying it.
    pub(crate) fn open(
        root: &std::path::Path,
        session_id: &str,
        submissions: &Arc<crate::provider::submission::SubmissionStore>,
    ) -> Result<Self> {
        // Load active input independently of completed interaction history.
        let mut journal = journal::InputJournal::open(root, session_id)?;
        let (mut state, restored) = journal.load()?;
        if restored {
            let previous = state.clone();
            state.snapshot.control_revision = state
                .snapshot
                .control_revision
                .checked_add(1)
                .context("input control revision exhausted")?;
            state.snapshot.revision = state
                .snapshot
                .revision
                .checked_add(1)
                .context("input revision exhausted")?;
            // Preserve uncertain sends; accepted or explicitly cancelled inputs cannot be replayed.
            let mut cancelled = std::collections::HashSet::new();
            for message in &mut state.snapshot.messages {
                if message.reservation().is_some() {
                    message.reserve(None)?;
                    message.revision = message
                        .revision
                        .checked_add(1)
                        .context("input message revision exhausted")?;
                }
                if let Some(id) = message.submission_id() {
                    let record = submissions.submission(id)?.record()?;
                    ensure!(
                        record.request.harness_session_id == session_id,
                        "input submission belongs to another session"
                    );
                    match record.delivery {
                        crate::provider::submission::SubmissionState::Accepted { .. } => {
                            message.set_delivery(InputDelivery::Accepted, None, None)?;
                        }
                        crate::provider::submission::SubmissionState::Sending { .. } => {
                            message.set_delivery(
                                InputDelivery::Unknown,
                                None,
                                Some("Delivery requires reconciliation before retry".into()),
                            )?;
                        }
                        crate::provider::submission::SubmissionState::Prepared => {
                            message.set_delivery(
                                InputDelivery::Failed,
                                None,
                                Some(
                                    "Interrupted before provider dispatch; retry explicitly".into(),
                                ),
                            )?;
                        }
                        crate::provider::submission::SubmissionState::Cancelled => {
                            cancelled.insert(message.id.clone());
                        }
                    }
                    message.revision = message
                        .revision
                        .checked_add(1)
                        .context("input message revision exhausted")?;
                } else if message.delivery() == InputDelivery::Delivering {
                    message.queue();
                    message.revision = message
                        .revision
                        .checked_add(1)
                        .context("input message revision exhausted")?;
                }
            }
            state.snapshot.messages.retain(|message| {
                message.delivery() != InputDelivery::Accepted && !cancelled.contains(&message.id)
            });
            if !state.snapshot.messages.is_empty() {
                state.snapshot.suspended = true;
            }
            // Publish recovered revisions only after the suspended state is durable.
            journal.save(&previous, &state)?;
        }
        Ok(Self {
            state: Mutex::new(state),
            journal: Some(Mutex::new(journal)),
            changed: Arc::default(),
        })
    }

    /// Remove queue state and duplicate receipts only when the session is explicitly deleted.
    pub(crate) fn delete_persisted(root: &std::path::Path, session_id: &str) -> Result<()> {
        journal::InputJournal::open(root, session_id)?.delete()
    }

    /// Commit a candidate atomically before exposing any changed queue state.
    fn commit(&self, retained: &mut InputState, candidate: InputState) -> Result<()> {
        if let Some(journal) = &self.journal {
            journal
                .lock()
                .map_err(|_| anyhow::anyhow!("input journal lock poisoned"))?
                .save(retained, &candidate)?;
        }
        *retained = candidate;
        Ok(())
    }

    pub(crate) fn has_pending(&self) -> Result<(u64, bool)> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        Ok((
            state.snapshot.revision,
            state.snapshot.messages.iter().any(|message| {
                !matches!(
                    message.delivery(),
                    InputDelivery::Delivering | InputDelivery::Accepted
                )
            }),
        ))
    }

    pub fn snapshot(&self) -> Result<InputSnapshot> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        Ok(state.snapshot.clone())
    }

    pub fn operation(&self, id: &str) -> Result<Option<InputUpdate>> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        Ok(state.operation.get(id).map(|(_, outcome)| {
            let mut outcome = outcome.clone();
            outcome.revision = state.snapshot.revision;
            outcome.snapshot = Some(state.snapshot.clone());
            outcome.suspended = state.snapshot.suspended;
            outcome.control_revision = state.snapshot.control_revision;
            outcome
        }))
    }

    /// Get the immutable admission receipt without pairing an old delta with the current queue revision.
    pub(crate) fn admission_receipt(
        &self,
        id: &str,
    ) -> std::result::Result<Option<InputUpdate>, InputJournalUnavailable> {
        let state = self.state.lock().map_err(|_| InputJournalUnavailable)?;
        Ok(state.operation.get(id).map(|(_, receipt)| receipt.clone()))
    }

    /// Reconcile canonical pending task work after recovery or a revision change.
    /// Provider submission receipts retain delivery authority for the stable message identity.
    pub(crate) fn reconcile_task_input(
        &self,
        mut message: InputMessage,
    ) -> Result<Option<InputUpdate>> {
        let revision = match &message.purpose {
            InputPurpose::TaskEntry { revision, id, .. }
            | InputPurpose::TaskContinuation { revision, id, .. } => {
                ensure!(id.as_str() == message.id, "task input identity changed");
                revision
            }
            InputPurpose::Clarification { .. }
            | InputPurpose::User
            | InputPurpose::Question { .. }
            | InputPurpose::QuestionContinuation { .. }
            | InputPurpose::WorkflowContinuation { .. } => {
                anyhow::bail!("task reconciliation requires task input")
            }
        };
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        if let Some(previous) = state
            .snapshot
            .messages
            .iter_mut()
            .find(|previous| previous.id == message.id)
        {
            let previous_revision = match &previous.purpose {
                InputPurpose::TaskEntry { revision, .. }
                | InputPurpose::TaskContinuation { revision, .. } => revision,
                InputPurpose::Clarification { .. }
                | InputPurpose::User
                | InputPurpose::Question { .. }
                | InputPurpose::QuestionContinuation { .. }
                | InputPurpose::WorkflowContinuation { .. } => {
                    anyhow::bail!("task input collides with user input")
                }
            };
            if previous_revision == revision
                || matches!(
                    previous.delivery(),
                    InputDelivery::Delivering | InputDelivery::Accepted | InputDelivery::Unknown
                )
            {
                return Ok(None);
            }
            message.revision = previous
                .revision
                .checked_add(1)
                .context("input message revision exhausted")?;
            *previous = message.clone();
        } else {
            message.revision = 1;
            state.submitted.insert(message.id.clone());
            // Initial task input must precede the directive awaiting its dispatch acknowledgement.
            // Ordinary queued input remains ordered behind the explicitly admitted task command.
            if matches!(message.purpose, InputPurpose::TaskEntry { .. }) {
                state.snapshot.messages.insert(0, message.clone());
            } else {
                state.snapshot.messages.push(message.clone());
            }
        }
        let mut update = InputUpdate {
            upsert: vec![message],
            ..Default::default()
        };
        Self::advance(&mut state.snapshot, &mut update);
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(Some(update))
    }

    /// Remove queued task inputs whose task authority can never become current again.
    /// Only unreserved revisions outside delivery are removed; see [`task_input_retirable`].
    pub(crate) fn retire_task_input(
        &self,
        retired: &[(String, u64)],
    ) -> Result<Option<InputUpdate>> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        let mut update = InputUpdate::default();
        state.snapshot.messages.retain(|message| {
            let remove = matches!(
                message.purpose,
                InputPurpose::TaskEntry { .. } | InputPurpose::TaskContinuation { .. }
            ) && task_input_retirable(message)
                && message.reservation().is_none()
                && retired
                    .iter()
                    .any(|(id, revision)| *id == message.id && *revision == message.revision);
            if remove {
                update.removed.push(message.id.clone());
            }
            !remove
        });
        if update.removed.is_empty() {
            return Ok(None);
        }
        Self::advance(&mut state.snapshot, &mut update);
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(Some(update))
    }

    /// Publish an input reservation committed with its owning workflow transaction.
    pub(crate) fn refresh_workflow_reservation(&self) -> Result<()> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let journal = self
            .journal
            .as_ref()
            .context("workflow input requires durable storage")?;
        let (state, _) = journal
            .lock()
            .map_err(|_| anyhow::anyhow!("input journal lock poisoned"))?
            .load()?;
        *retained = state;
        self.changed.notify_one();
        Ok(())
    }

    pub fn apply(&self, operation: InputOperation) -> Result<InputUpdate> {
        self.apply_with_retry_evidence(operation, None)
    }

    /// Re-enable uncertain input only with retained proof of non-acceptance.
    /// Validate revision and receipt identity again under the input commit lock.
    pub(crate) fn retry_reconciled(
        &self,
        operation: InputOperation,
        evidence: &crate::provider::submission::SubmissionRecord,
    ) -> Result<InputUpdate> {
        self.apply_with_retry_evidence(operation, Some(evidence))
    }

    /// Validate a complete candidate before atomically publishing input and command receipts.
    fn apply_with_retry_evidence(
        &self,
        operation: InputOperation,
        evidence: Option<&crate::provider::submission::SubmissionRecord>,
    ) -> Result<InputUpdate> {
        ensure!(
            !operation.id.is_empty(),
            "input operation identity is required"
        );
        let request = serde_json::to_string(&operation)?;
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        if let Some((original, outcome)) = state.operation.get(&operation.id) {
            ensure!(*original == request, "input operation identity was reused");
            let mut outcome = outcome.clone();
            outcome.revision = state.snapshot.revision;
            outcome.snapshot = Some(state.snapshot.clone());
            outcome.suspended = state.snapshot.suspended;
            outcome.control_revision = state.snapshot.control_revision;
            return Ok(outcome);
        }
        let mut update = InputUpdate {
            operation_id: Some(operation.id.clone()),
            ..Default::default()
        };
        // Validate the complete edit before changing any message in the queue.
        let mut candidate = state.snapshot.clone();
        let validation = if let InputAction::Submit { message } = &operation.action
            && state.submitted.contains(&message.id)
        {
            Err(InputRejection::AlreadySubmitted)
        } else {
            Self::validate_retry_evidence(&mut candidate, &operation, evidence)
                .and_then(|()| Self::validate(&candidate, &operation))
        };
        if validation.is_ok() {
            state.snapshot = candidate;
        }
        if validation.is_ok()
            && let InputAction::Submit { message } = &operation.action
        {
            state.submitted.insert(message.id.clone());
        }
        match validation {
            Err(error) => {
                update.error = Some(error.to_string());
                update.error_code = Some(error);
            }
            Ok(()) => Self::apply_action(
                &mut state.snapshot,
                operation.action,
                operation.control_revision,
                operation.after_control.as_deref(),
                &mut update,
            ),
        }
        Self::advance(&mut state.snapshot, &mut update);
        let mut receipt = update.clone();
        receipt.snapshot = None;
        state.operation.insert(operation.id, (request, receipt));
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(update)
    }

    /// Apply receipt proof to a private draft; never expose a partial reconciliation.
    fn validate_retry_evidence(
        snapshot: &mut InputSnapshot,
        operation: &InputOperation,
        evidence: Option<&crate::provider::submission::SubmissionRecord>,
    ) -> Result<(), InputRejection> {
        let Some(evidence) = evidence else {
            return Ok(());
        };
        let InputAction::Retry { id, revision } = &operation.action else {
            return Err(InputRejection::DeliveryUnconfirmed);
        };
        if !matches!(
            evidence.delivery,
            crate::provider::submission::SubmissionState::Prepared
                | crate::provider::submission::SubmissionState::Cancelled
        ) {
            return Err(InputRejection::DeliveryUnconfirmed);
        }
        let message = snapshot
            .messages
            .iter_mut()
            .find(|message| message.id == *id)
            .ok_or(InputRejection::InputMissing)?;
        if message.revision != *revision || message.submission_id() != Some(evidence.id.as_str()) {
            return Err(InputRejection::RevisionChanged);
        }
        if message.delivery() == InputDelivery::Unknown {
            message
                .set_delivery(
                    InputDelivery::Failed,
                    None,
                    message.error().map(str::to_owned),
                )
                .map_err(|_| InputRejection::DeliveryUnconfirmed)?;
        }
        Ok(())
    }

    fn validate(
        snapshot: &InputSnapshot,
        operation: &InputOperation,
    ) -> Result<(), InputRejection> {
        match &operation.action {
            InputAction::Retry { id, revision } => {
                let message = Self::editable(snapshot, id, *revision)?;
                if message.delivery() != InputDelivery::Failed {
                    return Err(InputRejection::DeliveryUnconfirmed);
                }
                if message.reservation().is_some() {
                    return Err(InputRejection::EditInProgress);
                }
            }
            InputAction::Resume => {
                if snapshot.control_revision != operation.control_revision {
                    return Err(InputRejection::ControlChanged);
                }
            }
            InputAction::Submit { message } => {
                if message.id.is_empty()
                    || message.text.trim().is_empty() && message.config.is_none()
                {
                    return Err(InputRejection::MissingContent);
                }
                if snapshot.messages.iter().any(|entry| entry.id == message.id) {
                    return Err(InputRejection::AlreadyQueued);
                }
            }
            InputAction::Reserve {
                id,
                revision,
                reservation,
            } => {
                if reservation.is_empty() {
                    return Err(InputRejection::MissingEditIdentity);
                }
                let message = Self::editable(snapshot, id, *revision)?;
                if message
                    .reservation()
                    .is_some_and(|owner| owner != reservation)
                {
                    return Err(InputRejection::EditOwned);
                }
            }
            InputAction::Commit { reservation, edits } => {
                let mut seen = std::collections::HashSet::new();
                for edit in edits {
                    if !seen.insert(&edit.id) {
                        return Err(InputRejection::DuplicateEdit);
                    }
                    let message = Self::editable(snapshot, &edit.id, edit.revision)?;
                    if message.reservation() != Some(reservation.as_str()) {
                        return Err(InputRejection::EditExpired);
                    }
                }
            }
            InputAction::Release { .. } => {}
            InputAction::Remove { id, revision } => {
                Self::editable(snapshot, id, *revision)?;
            }
        }
        Ok(())
    }

    fn apply_action(
        snapshot: &mut InputSnapshot,
        action: InputAction,
        control_revision: u64,
        after_control: Option<&str>,
        update: &mut InputUpdate,
    ) {
        match action {
            InputAction::Retry { id, .. } => {
                let message = snapshot
                    .messages
                    .iter_mut()
                    .find(|message| message.id == id)
                    .expect("validated message");
                message.queue();
                message.revision += 1;
                update.upsert.push(message.clone());
            }
            InputAction::Resume => {
                snapshot.suspended = false;
                snapshot.control_revision += 1;
            }
            InputAction::Submit { message } => {
                let mut message = *message;
                message.revision = 1;
                message.queue();
                let resume = message.intent == InputIntent::Auto
                    && (control_revision == snapshot.control_revision
                        || after_control.is_some()
                            && after_control == snapshot.control_operation_id.as_deref());
                update.prepend = snapshot.suspended && resume;
                update.upsert.push(message.clone());
                let index = if message.intent == InputIntent::Auto {
                    snapshot
                        .messages
                        .iter()
                        .position(|entry| entry.intent == InputIntent::Queue)
                        .unwrap_or(snapshot.messages.len())
                } else {
                    snapshot.messages.len()
                };
                snapshot.messages.insert(index, message);
                if resume {
                    snapshot.suspended = false;
                }
            }
            InputAction::Reserve {
                id, reservation, ..
            } => {
                let message = snapshot
                    .messages
                    .iter_mut()
                    .find(|entry| entry.id == id)
                    .expect("validated message");
                message
                    .reserve(Some(reservation))
                    .expect("validated edit reservation");
                message.revision += 1;
                update.upsert.push(message.clone());
            }
            InputAction::Commit { reservation, edits } => {
                for edit in edits {
                    let index = snapshot
                        .messages
                        .iter()
                        .position(|entry| entry.id == edit.id)
                        .expect("validated message");
                    if edit.text.trim().is_empty() && edit.config.is_none() {
                        snapshot.messages.remove(index);
                        update.removed.push(edit.id);
                    } else {
                        let message = &mut snapshot.messages[index];
                        message.text = edit.text;
                        message.attachments = edit.attachments;
                        if edit.config.is_some() {
                            message.config = edit.config;
                        }
                        message.queue();
                        message.revision += 1;
                        update.upsert.push(message.clone());
                    }
                }
                Self::release(snapshot, &reservation, update);
            }
            InputAction::Release { reservation } => Self::release(snapshot, &reservation, update),
            InputAction::Remove { id, .. } => {
                snapshot.messages.retain(|message| message.id != id);
                update.removed.push(id);
            }
        }
    }

    fn release(snapshot: &mut InputSnapshot, reservation: &str, update: &mut InputUpdate) {
        for message in &mut snapshot.messages {
            if message.reservation() == Some(reservation) {
                message.reserve(None).expect("reserved input is editable");
                message.revision += 1;
                update.upsert.push(message.clone());
            }
        }
    }

    pub fn suspend(&self, suspended: bool, operation_id: Option<&str>) -> Result<InputUpdate> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        state.snapshot.suspended = suspended;
        state.snapshot.control_revision += 1;
        state.snapshot.control_operation_id = operation_id.map(str::to_owned);
        let mut update = InputUpdate::default();
        Self::advance(&mut state.snapshot, &mut update);
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(update)
    }

    /// Retract the newest user prompt that no owner has claimed yet.
    ///
    /// A cancel can run before the owner claims a just-submitted prompt. That cancel
    /// finds no foreground work, yet its suspension would refuse every later claim and
    /// strand the prompt in the queue. Retracting it returns the draft to the composer
    /// as a cancel of started work does. Deliberately queued follow-ups stay queued.
    pub(crate) fn retract_unclaimed_submission(
        &self,
        operation_id: &str,
    ) -> Result<Option<InputUpdate>> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        let Some(index) = state.snapshot.messages.iter().rposition(|message| {
            matches!(message.purpose, InputPurpose::User)
                && message.intent == InputIntent::Auto
                && message.delivery() == InputDelivery::Queued
                && message.reservation().is_none()
                && message.submission_id().is_none()
        }) else {
            return Ok(None);
        };
        let message = state.snapshot.messages.remove(index);
        let mut update = InputUpdate {
            operation_id: Some(operation_id.to_owned()),
            removed: vec![message.id.clone()],
            retracted: Some(message.id),
            ..InputUpdate::default()
        };
        Self::advance(&mut state.snapshot, &mut update);
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(Some(update))
    }

    pub fn claim(
        &self,
        id: &str,
        revision: u64,
        route: InputRoute,
    ) -> Result<Option<InputMessage>> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        let snapshot = &mut state.snapshot;
        let Some(index) = snapshot
            .messages
            .iter()
            .position(|message| message.id == id)
        else {
            return Ok(None);
        };
        let message = &snapshot.messages[index];
        if (snapshot.suspended
            && !matches!(
                message.purpose,
                InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
            ))
            || snapshot
                .messages
                .iter()
                .any(|previous| previous.delivery() == InputDelivery::Unknown)
            || message.revision != revision
            || message.reservation().is_some()
            || message.delivery() != InputDelivery::Queued
            || (route == InputRoute::Steering
                && (message.intent != InputIntent::Auto
                    || message.config.is_some()
                    || matches!(message.purpose, InputPurpose::TaskContinuation { .. })))
            || (matches!(message.purpose, InputPurpose::TaskContinuation { .. })
                && snapshot.messages.iter().any(|other| {
                    !matches!(other.purpose, InputPurpose::TaskContinuation { .. })
                        && !matches!(
                            other.delivery(),
                            InputDelivery::Delivering | InputDelivery::Accepted
                        )
                }))
        {
            return Ok(None);
        }
        if snapshot.messages[..index].iter().any(|previous| {
            let user_precedes_continuation =
                matches!(previous.purpose, InputPurpose::TaskContinuation { .. })
                    && !matches!(message.purpose, InputPurpose::TaskContinuation { .. });
            let steering_precedes_queue = route == InputRoute::Steering
                && previous.intent == InputIntent::Queue
                && previous.delivery() == InputDelivery::Queued;
            let question_precedes_suspended = snapshot.suspended
                && matches!(
                    message.purpose,
                    InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
                )
                && !matches!(
                    previous.purpose,
                    InputPurpose::Question { .. } | InputPurpose::Clarification { .. }
                );
            if user_precedes_continuation || steering_precedes_queue || question_precedes_suspended
            {
                return false;
            }
            !matches!(
                previous.delivery(),
                InputDelivery::Delivering | InputDelivery::Accepted
            )
        }) {
            return Ok(None);
        }
        let message = &mut snapshot.messages[index];
        message.claim(route);
        message.revision += 1;
        let claimed = message.clone();
        Self::advance(snapshot, &mut InputUpdate::default());
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(Some(claimed))
    }

    #[cfg(test)]
    pub fn delivery(
        &self,
        id: &str,
        delivery: Option<InputDelivery>,
        submission_id: Option<String>,
        error: Option<String>,
    ) -> Result<InputUpdate> {
        self.delivery_matching(id, None, delivery, submission_id, error)?
            .context("unconditional input delivery must produce an update")
    }

    /// Commit a delivery fact only while its original input/receipt authority still matches.
    pub(crate) fn delivery_if_current(
        &self,
        id: &str,
        guard: InputDeliveryGuard<'_>,
        delivery: Option<InputDelivery>,
        submission_id: Option<String>,
        error: Option<String>,
    ) -> Result<Option<InputUpdate>> {
        self.delivery_matching(id, Some(guard), delivery, submission_id, error)
    }

    /// Check authority and commit the queue mutation within the same journal lock.
    fn delivery_matching(
        &self,
        id: &str,
        guard: Option<InputDeliveryGuard<'_>>,
        delivery: Option<InputDelivery>,
        submission_id: Option<String>,
        error: Option<String>,
    ) -> Result<Option<InputUpdate>> {
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let mut state = retained.clone();
        let Some(index) = state
            .snapshot
            .messages
            .iter()
            .position(|message| message.id == id)
        else {
            if guard.is_some() {
                return Ok(None);
            }
            return Ok(Some(InputUpdate {
                revision: state.snapshot.revision,
                snapshot: Some(state.snapshot.clone()),
                ..Default::default()
            }));
        };
        if let Some(guard) = guard {
            let message = &state.snapshot.messages[index];
            let matches = match guard {
                InputDeliveryGuard::Queued(revision) => {
                    message.revision == revision
                        && message.delivery() == InputDelivery::Queued
                        && message.reservation().is_none()
                        && message.submission_id().is_none()
                }
                InputDeliveryGuard::Claimed(revision) => {
                    message.revision == revision && message.delivery() == InputDelivery::Delivering
                }
                InputDeliveryGuard::Receipt(receipt) => {
                    message.submission_id() == Some(receipt.as_str())
                }
            };
            if !matches {
                return Ok(None);
            }
        }
        let mut update = InputUpdate::default();
        if state.snapshot.messages[index].delivery() == InputDelivery::Accepted
            && delivery.is_some()
        {
            return Ok(Some(InputUpdate {
                revision: state.snapshot.revision,
                snapshot: Some(state.snapshot.clone()),
                ..Default::default()
            }));
        }
        if let Some(delivery) = delivery {
            let message = &mut state.snapshot.messages[index];
            message.set_delivery(delivery, submission_id, error)?;
            message.revision += 1;
            update.upsert.push(message.clone());
        } else {
            state.snapshot.messages.remove(index);
            update.removed.push(id.into());
        }
        Self::advance(&mut state.snapshot, &mut update);
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(Some(update))
    }

    /// Acknowledge an already accepted provider retry and its removal in one transaction.
    pub(crate) fn acknowledge_retry(&self, operation: InputOperation) -> Result<InputUpdate> {
        ensure!(
            !operation.id.is_empty(),
            "input operation identity is required"
        );
        let InputAction::Retry { id, revision } = &operation.action else {
            anyhow::bail!("accepted delivery requires a retry operation");
        };
        let request = serde_json::to_string(&operation)?;
        let mut retained = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        if let Some((original, outcome)) = retained.operation.get(&operation.id) {
            ensure!(original == &request, "input operation identity was reused");
            let mut outcome = outcome.clone();
            outcome.snapshot = Some(retained.snapshot.clone());
            outcome.revision = retained.snapshot.revision;
            outcome.suspended = retained.snapshot.suspended;
            outcome.control_revision = retained.snapshot.control_revision;
            return Ok(outcome);
        }
        let mut state = retained.clone();
        if let Some(message) = state
            .snapshot
            .messages
            .iter()
            .find(|message| &message.id == id)
        {
            ensure!(
                message.revision == *revision,
                "message changed before retry acknowledgment"
            );
        }
        state.snapshot.messages.retain(|message| &message.id != id);
        let mut update = InputUpdate {
            operation_id: Some(operation.id.clone()),
            removed: vec![id.clone()],
            ..Default::default()
        };
        Self::advance(&mut state.snapshot, &mut update);
        let mut receipt = update.clone();
        receipt.snapshot = None;
        state.operation.insert(operation.id, (request, receipt));
        self.commit(&mut retained, state)?;
        drop(retained);
        self.changed.notify_one();
        Ok(update)
    }

    /// Validate the selected input and revision before permitting content mutation.
    fn editable<'state>(
        snapshot: &'state InputSnapshot,
        id: &str,
        revision: u64,
    ) -> Result<&'state InputMessage, InputRejection> {
        let message = snapshot
            .messages
            .iter()
            .find(|entry| entry.id == id)
            .ok_or(InputRejection::InputMissing)?;
        if message.revision != revision {
            return Err(InputRejection::RevisionChanged);
        }
        if !matches!(
            message.delivery(),
            InputDelivery::Queued | InputDelivery::Failed
        ) {
            return Err(InputRejection::DeliveryStarted);
        }
        Ok(message)
    }

    fn advance(snapshot: &mut InputSnapshot, update: &mut InputUpdate) {
        snapshot.revision += 1;
        update.revision = snapshot.revision;
        update.suspended = snapshot.suspended;
        update.control_revision = snapshot.control_revision;
        update.snapshot = Some(snapshot.clone());
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod durability_test;

/// Report whether task input sits outside native delivery, so retiring its task authority can
/// remove it. Queued input never reached the provider, and failed input ended without acceptance;
/// a failed task input whose revision is retired can never be retried under its own authority.
/// Delivering, unknown, and accepted input stay owned by their delivery receipts.
pub(crate) fn task_input_retirable(message: &InputMessage) -> bool {
    matches!(
        message.delivery(),
        InputDelivery::Queued | InputDelivery::Failed
    )
}
