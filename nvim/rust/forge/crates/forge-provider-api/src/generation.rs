use crate::{ProviderError, ProviderModel};

/// Select reasoning independently of the purpose of a text request.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum ReasoningSelection {
    /// Use the provider's default reasoning level.
    #[default]
    Default,
    /// Use the lowest level advertised by the selected model.
    Lowest,
    /// Use an explicitly selected reasoning level.
    Level(String),
}

/// Prepare an isolated text request without retaining Harness task state.
#[derive(Clone)]
pub struct TextGenerationRequest {
    /// Select the admitted workspace for temporary native configuration and files.
    pub workspace: String,
    /// Model selected by the caller.
    pub model: String,
    /// Reasoning policy selected by the caller.
    pub reasoning: ReasoningSelection,
    /// Complete instructions for the isolated conversation.
    pub instructions: String,
    /// Prepared evidence and user input for generation.
    pub input: String,
    /// Apply the caller's output-specific acceptance rule before native cleanup completes.
    pub validator: std::sync::Arc<dyn TextValidator>,
}

/// Validate generated text according to the host's requested output surface.
pub trait TextValidator: Send + Sync {
    /// Normalize accepted output or explain why generation failed.
    fn validate(&self, text: &str) -> Result<String, ProviderError>;
}

/// Select the lowest advertised reasoning level from the adapter-normalized ascending catalog.
pub fn lowest_effort(model: &ProviderModel) -> Result<Option<String>, ProviderError> {
    Ok(model.reasoning.first().cloned())
}
