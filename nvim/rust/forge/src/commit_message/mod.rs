mod context;
mod excerpt;
mod service;
mod settings;

pub use service::{Comparison, GenerationRequest, GenerationService};
pub use settings::dispatch as settings;
