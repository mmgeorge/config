//! GitHub records and issue storage shared by Forge consumers.

pub mod comment;
mod completion;
mod draft;
pub mod issue_store;
pub mod lease;
pub mod metadata;
pub mod model;
pub mod notification;
mod publication;
pub mod pull_request;
pub mod queue;
pub mod recovery;
pub mod remote;
pub mod review_api;
pub mod review_mutation;
pub mod review_source;
pub mod service;
pub mod sync;
