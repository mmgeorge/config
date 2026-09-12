//! Review field ownership independent of generated document layout.

pub mod comments;
pub mod commit_diff;
pub mod edit;
pub mod review;
pub mod service;

pub mod inventory;
pub mod issue;
pub mod notification;
pub mod walkthrough;

mod issue_presentation;
pub mod issue_service;
pub(crate) mod presentation_time;
