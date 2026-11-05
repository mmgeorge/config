//! Keep canonical input delivery combinations valid independently of their wire projection.

use forge_provider_api::SubmissionId;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::{InputDelivery, InputIntent, InputPurpose, InputRoute};

/// Author input content without granting delivery, reservation, or receipt authority.
#[derive(Clone, Debug)]
pub(crate) struct InputDraft {
    pub id: String,
    pub text: String,
    pub purpose: InputPurpose,
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
    pub intent: InputIntent,
}

/// Retain content together with exactly one valid delivery phase.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "InputRecord", into = "InputRecord")]
pub struct InputMessage {
    pub id: String,
    pub revision: u64,
    pub text: String,
    pub purpose: InputPurpose,
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
    pub intent: InputIntent,
    state: DeliveryState,
}

#[derive(Clone, Debug)]
enum DeliveryState {
    Queued {
        reservation: Option<String>,
    },
    Delivering {
        route: InputRoute,
        submission: Option<SubmissionId>,
    },
    Accepted {
        route: InputRoute,
        submission: SubmissionId,
    },
    Unknown {
        route: InputRoute,
        submission: SubmissionId,
        error: String,
    },
    Failed {
        route: Option<InputRoute>,
        submission: Option<SubmissionId>,
        reservation: Option<String>,
        error: String,
    },
}

/// Decode the transport representation before validating its delivery combinations.
#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct InputRecord {
    id: String,
    #[serde(default)]
    revision: u64,
    text: String,
    #[serde(default)]
    purpose: InputPurpose,
    #[serde(default)]
    attachments: Vec<Value>,
    config: Option<Value>,
    #[serde(default)]
    intent: InputIntent,
    #[serde(default)]
    delivery: InputDelivery,
    route: Option<InputRoute>,
    reservation: Option<String>,
    submission_id: Option<String>,
    error: Option<String>,
}

/// Reject contradictory delivery data before it enters the canonical queue.
#[derive(Debug, thiserror::Error)]
pub enum InputMessageError {
    #[error("input delivery requires its selected route")]
    MissingRoute,
    #[error("input delivery requires its submission receipt")]
    MissingReceipt,
    #[error("failed or uncertain input requires a reason")]
    MissingReason,
    #[error("input submission identity is invalid")]
    InvalidReceipt,
    #[error("input reservation identity is empty")]
    InvalidReservation,
    #[error("input delivery contains fields belonging to another phase")]
    InvalidPhase,
}

impl InputMessage {
    /// Create new unsent content without accepting caller-supplied delivery metadata.
    pub(crate) fn queued(draft: InputDraft) -> Self {
        let InputDraft {
            id,
            text,
            purpose,
            attachments,
            config,
            intent,
        } = draft;
        Self {
            id,
            revision: 0,
            text,
            purpose,
            attachments,
            config,
            intent,
            state: DeliveryState::Queued { reservation: None },
        }
    }

    /// Create the first committed revision for input reserved in a workflow transaction.
    pub(crate) fn admitted(draft: InputDraft) -> Self {
        let mut message = Self::queued(draft);
        message.revision = 1;
        message
    }

    /// Consume content independently of the queue's delivery authority.
    pub(crate) fn into_draft(self) -> InputDraft {
        let Self {
            id,
            revision: _,
            text,
            purpose,
            attachments,
            config,
            intent,
            state: _,
        } = self;
        InputDraft {
            id,
            text,
            purpose,
            attachments,
            config,
            intent,
        }
    }

    /// Get the presentation category for the current phase.
    pub fn delivery(&self) -> InputDelivery {
        match self.state {
            DeliveryState::Queued { .. } => InputDelivery::Queued,
            DeliveryState::Delivering { .. } => InputDelivery::Delivering,
            DeliveryState::Accepted { .. } => InputDelivery::Accepted,
            DeliveryState::Unknown { .. } => InputDelivery::Unknown,
            DeliveryState::Failed { .. } => InputDelivery::Failed,
        }
    }

    /// Get the delivery route once the owner has selected it.
    pub fn route(&self) -> Option<InputRoute> {
        match self.state {
            DeliveryState::Queued { .. } => None,
            DeliveryState::Delivering { route, .. }
            | DeliveryState::Accepted { route, .. }
            | DeliveryState::Unknown { route, .. } => Some(route),
            DeliveryState::Failed { route, .. } => route,
        }
    }

    /// Get the current editor reservation, available only before delivery.
    pub fn reservation(&self) -> Option<&str> {
        match &self.state {
            DeliveryState::Queued { reservation } | DeliveryState::Failed { reservation, .. } => {
                reservation.as_deref()
            }
            DeliveryState::Delivering { .. }
            | DeliveryState::Accepted { .. }
            | DeliveryState::Unknown { .. } => None,
        }
    }

    /// Get the exact submission identity bound by the owner.
    pub fn submission_id(&self) -> Option<&str> {
        match &self.state {
            DeliveryState::Queued { .. } => None,
            DeliveryState::Delivering { submission, .. }
            | DeliveryState::Failed { submission, .. } => {
                submission.as_ref().map(SubmissionId::as_str)
            }
            DeliveryState::Accepted { submission, .. }
            | DeliveryState::Unknown { submission, .. } => Some(submission.as_str()),
        }
    }

    /// Get the normalized failure reason without manufacturing a failure for cancelled input.
    pub fn error(&self) -> Option<&str> {
        match &self.state {
            DeliveryState::Unknown { error, .. } | DeliveryState::Failed { error, .. } => {
                Some(error)
            }
            DeliveryState::Queued { .. }
            | DeliveryState::Delivering { .. }
            | DeliveryState::Accepted { .. } => None,
        }
    }

    /// Reset edited or explicitly retried content to unsent state, retiring old receipt authority.
    pub(super) fn queue(&mut self) {
        self.state = DeliveryState::Queued { reservation: None };
    }

    /// Claim unsent content for the route chosen by the exchange owner.
    pub(super) fn claim(&mut self, route: InputRoute) {
        self.state = DeliveryState::Delivering {
            route,
            submission: None,
        };
    }

    /// Reserve or release only content whose delivery has not begun.
    pub(super) fn reserve(&mut self, reservation: Option<String>) -> Result<(), InputMessageError> {
        if reservation.as_ref().is_some_and(String::is_empty) {
            return Err(InputMessageError::InvalidReservation);
        }
        match &mut self.state {
            DeliveryState::Queued {
                reservation: current,
            }
            | DeliveryState::Failed {
                reservation: current,
                ..
            } => {
                *current = reservation;
                Ok(())
            }
            DeliveryState::Delivering { .. }
            | DeliveryState::Accepted { .. }
            | DeliveryState::Unknown { .. } => Err(InputMessageError::InvalidPhase),
        }
    }

    /// Validate the complete replacement phase before changing canonical delivery state.
    pub(super) fn set_delivery(
        &mut self,
        delivery: InputDelivery,
        submission: Option<String>,
        error: Option<String>,
    ) -> Result<(), InputMessageError> {
        let mut record = InputRecord::from(self.clone());
        record.delivery = delivery;
        record.submission_id = submission.or(record.submission_id);
        record.error = error;
        if delivery == InputDelivery::Queued {
            record.route = None;
            record.submission_id = None;
            record.reservation = None;
            record.error = None;
        }
        let replacement = Self::try_from(record)?;
        self.state = replacement.state;
        Ok(())
    }
}

impl TryFrom<InputRecord> for InputMessage {
    type Error = InputMessageError;

    fn try_from(record: InputRecord) -> Result<Self, Self::Error> {
        let InputRecord {
            id,
            revision,
            text,
            purpose,
            attachments,
            config,
            intent,
            delivery,
            route,
            reservation,
            submission_id,
            error,
        } = record;
        if reservation.as_ref().is_some_and(String::is_empty) {
            return Err(InputMessageError::InvalidReservation);
        }
        let submission = submission_id
            .map(SubmissionId::try_from)
            .transpose()
            .map_err(|_| InputMessageError::InvalidReceipt)?;
        let state = match delivery {
            InputDelivery::Queued => {
                if route.is_some() || submission.is_some() || error.is_some() {
                    return Err(InputMessageError::InvalidPhase);
                }
                DeliveryState::Queued { reservation }
            }
            InputDelivery::Delivering => {
                if reservation.is_some() || error.is_some() {
                    return Err(InputMessageError::InvalidPhase);
                }
                DeliveryState::Delivering {
                    route: route.ok_or(InputMessageError::MissingRoute)?,
                    submission,
                }
            }
            InputDelivery::Accepted => {
                if reservation.is_some() || error.is_some() {
                    return Err(InputMessageError::InvalidPhase);
                }
                DeliveryState::Accepted {
                    route: route.ok_or(InputMessageError::MissingRoute)?,
                    submission: submission.ok_or(InputMessageError::MissingReceipt)?,
                }
            }
            InputDelivery::Unknown => {
                if reservation.is_some() {
                    return Err(InputMessageError::InvalidPhase);
                }
                DeliveryState::Unknown {
                    route: route.ok_or(InputMessageError::MissingRoute)?,
                    submission: submission.ok_or(InputMessageError::MissingReceipt)?,
                    error: error.ok_or(InputMessageError::MissingReason)?,
                }
            }
            InputDelivery::Failed => DeliveryState::Failed {
                route,
                submission,
                reservation,
                error: error.ok_or(InputMessageError::MissingReason)?,
            },
        };
        Ok(Self {
            id,
            revision,
            text,
            purpose,
            attachments,
            config,
            intent,
            state,
        })
    }
}

impl From<InputMessage> for InputRecord {
    fn from(message: InputMessage) -> Self {
        let delivery = message.delivery();
        let route = message.route();
        let reservation = message.reservation().map(str::to_owned);
        let submission_id = message.submission_id().map(str::to_owned);
        let error = message.error().map(str::to_owned);
        let InputMessage {
            id,
            revision,
            text,
            purpose,
            attachments,
            config,
            intent,
            state: _,
        } = message;
        Self {
            id,
            revision,
            text,
            purpose,
            attachments,
            config,
            intent,
            delivery,
            route,
            reservation,
            submission_id,
            error,
        }
    }
}
