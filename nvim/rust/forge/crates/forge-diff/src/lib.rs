//! Immutable source contracts and shared diff analysis, independent of repository and UI owners.

pub mod body;
pub mod cache;
pub mod declaration_change;
pub mod display;
pub mod edit;
pub mod engine;
pub mod file_header;
pub mod intraline;
pub mod patch;
pub mod presentation;
pub mod projection;
pub mod raw;
pub mod saved_projection;
pub mod saved_source;
pub mod semantic_view;
pub mod source;
pub mod source_projection;
pub mod source_view;
pub mod syntax;
pub mod unified;
pub mod view;
pub mod workers;
