//! Local target-only contextual correction, independent of editor and provider sessions.

mod cache;
pub mod contract;
mod error;
mod model;
mod service;
mod token;

pub use error::TextError;
pub use service::TextCorrectionService;
