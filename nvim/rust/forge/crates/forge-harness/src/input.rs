use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

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

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
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
    TaskEntry {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        start: crate::exchange::TaskStart,
    },
    TaskContinuation {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        completed_turn_id: crate::task::TaskTurnId,
        reason: String,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputMessage {
    pub id: String,
    #[serde(default)]
    pub revision: u64,
    pub text: String,
    #[serde(default)]
    pub purpose: InputPurpose,
    #[serde(default)]
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
    #[serde(default)]
    pub intent: InputIntent,
    #[serde(default)]
    pub delivery: InputDelivery,
    #[serde(default)]
    pub route: Option<InputRoute>,
    pub reservation: Option<String>,
    pub submission_id: Option<String>,
    pub error: Option<String>,
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

#[derive(Default)]
struct InputState {
    snapshot: InputSnapshot,
    submitted: std::collections::HashSet<String>,
    operation: HashMap<String, (String, InputUpdate)>,
}

/// One session's volatile queue. No lock is retained across provider or storage work.
#[derive(Default)]
pub(crate) struct SessionInputQueue {
    state: Mutex<InputState>,
    pub changed: Arc<tokio::sync::Notify>,
}

impl SessionInputQueue {
    pub(crate) fn has_pending(&self) -> Result<(u64, bool)> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        Ok((
            state.snapshot.revision,
            state.snapshot.messages.iter().any(|message| {
                !matches!(
                    message.delivery,
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
            outcome
        }))
    }

    pub fn apply(&self, operation: InputOperation) -> Result<InputUpdate> {
        ensure!(
            !operation.id.is_empty(),
            "input operation identity is required"
        );
        let request = serde_json::to_string(&operation)?;
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        if let Some((original, outcome)) = state.operation.get(&operation.id) {
            ensure!(*original == request, "input operation identity was reused");
            let mut outcome = outcome.clone();
            outcome.revision = state.snapshot.revision;
            outcome.snapshot = Some(state.snapshot.clone());
            return Ok(outcome);
        }
        let mut update = InputUpdate {
            operation_id: Some(operation.id.clone()),
            ..Default::default()
        };
        // Validate the complete edit before changing any message in the queue.
        let validation = if let InputAction::Submit { message } = &operation.action
            && state.submitted.contains(&message.id)
        {
            Err(anyhow::anyhow!("message identity was already submitted"))
        } else {
            Self::validate(&state.snapshot, &operation)
        };
        if validation.is_ok()
            && let InputAction::Submit { message } = &operation.action
        {
            state.submitted.insert(message.id.clone());
        }
        match validation {
            Err(error) => update.error = Some(format!("{error:#}")),
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
        drop(state);
        self.changed.notify_one();
        Ok(update)
    }

    fn validate(snapshot: &InputSnapshot, operation: &InputOperation) -> Result<()> {
        match &operation.action {
            InputAction::Retry { id, revision } => {
                let message = Self::editable(snapshot, id, *revision)?;
                ensure!(
                    message.delivery == InputDelivery::Failed,
                    "only a definitely unsent message can be retried"
                );
                ensure!(
                    message.reservation.is_none(),
                    "finish editing the message before retrying"
                );
            }
            InputAction::Resume => ensure!(
                snapshot.control_revision == operation.control_revision,
                "delivery control changed before resume"
            ),
            InputAction::Submit { message } => {
                ensure!(
                    !message.id.is_empty()
                        && (!message.text.trim().is_empty() || message.config.is_some()),
                    "message identity and text are required"
                );
                ensure!(
                    !snapshot.messages.iter().any(|entry| entry.id == message.id),
                    "message already exists"
                );
            }
            InputAction::Reserve {
                id,
                revision,
                reservation,
            } => {
                ensure!(
                    !reservation.is_empty(),
                    "edit reservation identity is required"
                );
                let message = Self::editable(snapshot, id, *revision)?;
                ensure!(
                    message
                        .reservation
                        .as_ref()
                        .is_none_or(|owner| owner == reservation),
                    "message is being edited by another owner"
                );
            }
            InputAction::Commit { reservation, edits } => {
                let mut seen = std::collections::HashSet::new();
                for edit in edits {
                    ensure!(seen.insert(&edit.id), "edit repeats a message");
                    let message = Self::editable(snapshot, &edit.id, edit.revision)?;
                    ensure!(
                        message.reservation.as_ref() == Some(reservation),
                        "edit reservation is no longer owned"
                    );
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
                message.delivery = InputDelivery::Queued;
                message.submission_id = None;
                message.route = None;
                message.error = None;
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
                message.delivery = InputDelivery::Queued;
                message.route = None;
                message.reservation = None;
                message.submission_id = None;
                message.error = None;
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
                message.reservation = Some(reservation);
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
                        message.reservation = None;
                        message.delivery = InputDelivery::Queued;
                        message.error = None;
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
            if message.reservation.as_deref() == Some(reservation) {
                message.reservation = None;
                message.revision += 1;
                update.upsert.push(message.clone());
            }
        }
    }

    pub fn suspend(&self, suspended: bool, operation_id: Option<&str>) -> Result<InputUpdate> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        state.snapshot.suspended = suspended;
        state.snapshot.control_revision += 1;
        state.snapshot.control_operation_id = operation_id.map(str::to_owned);
        let mut update = InputUpdate::default();
        Self::advance(&mut state.snapshot, &mut update);
        drop(state);
        self.changed.notify_one();
        Ok(update)
    }

    pub fn claim(
        &self,
        id: &str,
        revision: u64,
        route: InputRoute,
    ) -> Result<Option<InputMessage>> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let snapshot = &mut state.snapshot;
        let Some(index) = snapshot
            .messages
            .iter()
            .position(|message| message.id == id)
        else {
            return Ok(None);
        };
        let message = &snapshot.messages[index];
        if snapshot.suspended
            || snapshot
                .messages
                .iter()
                .any(|previous| previous.delivery == InputDelivery::Unknown)
            || message.revision != revision
            || message.reservation.is_some()
            || message.delivery != InputDelivery::Queued
            || (route == InputRoute::Steering
                && (message.intent != InputIntent::Auto
                    || message.config.is_some()
                    || matches!(message.purpose, InputPurpose::TaskContinuation { .. })))
            || (matches!(message.purpose, InputPurpose::TaskContinuation { .. })
                && snapshot.messages.iter().any(|other| {
                    !matches!(other.purpose, InputPurpose::TaskContinuation { .. })
                        && !matches!(
                            other.delivery,
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
                && previous.delivery == InputDelivery::Queued;
            if user_precedes_continuation || steering_precedes_queue {
                return false;
            }
            !matches!(
                previous.delivery,
                InputDelivery::Delivering | InputDelivery::Accepted
            )
        }) {
            return Ok(None);
        }
        let message = &mut snapshot.messages[index];
        message.delivery = InputDelivery::Delivering;
        message.route = Some(route);
        message.revision += 1;
        Ok(Some(message.clone()))
    }

    pub fn delivery(
        &self,
        id: &str,
        delivery: Option<InputDelivery>,
        submission_id: Option<String>,
        error: Option<String>,
    ) -> Result<InputUpdate> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("input queue lock poisoned"))?;
        let Some(index) = state
            .snapshot
            .messages
            .iter()
            .position(|message| message.id == id)
        else {
            return Ok(InputUpdate {
                revision: state.snapshot.revision,
                snapshot: Some(state.snapshot.clone()),
                ..Default::default()
            });
        };
        let mut update = InputUpdate::default();
        if state.snapshot.messages[index].delivery == InputDelivery::Accepted && delivery.is_some()
        {
            return Ok(InputUpdate {
                revision: state.snapshot.revision,
                snapshot: Some(state.snapshot.clone()),
                ..Default::default()
            });
        }
        if let Some(delivery) = delivery {
            let message = &mut state.snapshot.messages[index];
            message.delivery = delivery;
            message.submission_id = submission_id.or(message.submission_id.take());
            message.error = error;
            message.revision += 1;
            update.upsert.push(message.clone());
        } else {
            state.snapshot.messages.remove(index);
            update.removed.push(id.into());
        }
        Self::advance(&mut state.snapshot, &mut update);
        drop(state);
        self.changed.notify_one();
        Ok(update)
    }

    fn editable<'state>(
        snapshot: &'state InputSnapshot,
        id: &str,
        revision: u64,
    ) -> Result<&'state InputMessage> {
        let message = snapshot
            .messages
            .iter()
            .find(|entry| entry.id == id)
            .context("message was already delivered or removed")?;
        ensure!(
            message.revision == revision,
            "message changed before the edit was accepted"
        );
        ensure!(
            matches!(
                message.delivery,
                InputDelivery::Queued | InputDelivery::Failed
            ),
            "message delivery has already started"
        );
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
#[path = "input/test.rs"]
mod tests;
