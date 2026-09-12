mod hunks;
mod ignored;
mod navigation;
mod mutation;
pub use mutation::{StatusActionAccepted, StatusOperationTicket, StatusUpdate};
pub mod protocol;
pub use protocol::{StatusDelta, StatusInput, StatusLocation, StatusSelection, StatusSnapshot};
use serde::{Deserialize, Serialize};

pub mod context;
mod document;
mod reconcile;
mod service;
mod source;
pub mod source_document;

pub use service::{
    BodyDelivery, BodyState, NavigationDelivery, NavigationEffect, StatusAction, StatusOpenTarget,
    StatusService,
};

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StatusSection {
    Staged,
    Unstaged,
    Untracked,
    Conflicted,
    Ignored,
}
