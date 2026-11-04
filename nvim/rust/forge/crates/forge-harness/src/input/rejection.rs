//! Preserve input admission decisions independently of diagnostic wording.

use serde::{Deserialize, Serialize};

/// Reject an input mutation before changing queued content or delivery authority.
/// Persist the category with its receipt; callers never infer it from text.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, thiserror::Error)]
#[serde(rename_all = "snake_case")]
pub enum InputRejection {
    /// Input identity was submitted previously, including after removal or delivery.
    #[error("message identity was already submitted")]
    AlreadySubmitted,
    /// Retry requires proof that native delivery did not occur.
    #[error("only a definitely unsent message can be retried")]
    DeliveryUnconfirmed,
    /// Retry cannot replace content while another edit owns the input.
    #[error("finish editing the message before retrying")]
    EditInProgress,
    /// A delivery-control revision no longer matches the caller's observation.
    #[error("delivery control changed before resume")]
    ControlChanged,
    /// A submitted input lacks an identity or usable content/configuration.
    #[error("message identity and text are required")]
    MissingContent,
    /// A queued input already owns the submitted identity.
    #[error("message already exists")]
    AlreadyQueued,
    /// Edit admission requires a nonempty edit identity.
    #[error("edit reservation identity is required")]
    MissingEditIdentity,
    /// An edit is already owned by another reservation.
    #[error("message is being edited by another owner")]
    EditOwned,
    /// An atomic edit repeats the same input identity.
    #[error("edit repeats a message")]
    DuplicateEdit,
    /// An edit commit no longer owns the input reservation.
    #[error("edit reservation is no longer owned")]
    EditExpired,
    /// Input was delivered or removed before the selected mutation started.
    #[error("message was already delivered or removed")]
    InputMissing,
    /// Input changed after the caller observed its revision.
    #[error("message changed before the edit was accepted")]
    RevisionChanged,
    /// Potentially sent content cannot be edited or removed.
    #[error("message delivery has already started")]
    DeliveryStarted,
}
