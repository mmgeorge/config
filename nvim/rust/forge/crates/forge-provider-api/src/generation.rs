use crate::{ProviderError, ProviderModel};

/// Select reasoning independently of the purpose of a text request.
#[derive(Clone, Debug)]
pub enum ReasoningSelection {
    /// Use the provider's default reasoning level.
    Default,
    /// Use the lowest level advertised by the selected model.
    Lowest,
    /// Use an explicitly selected reasoning level.
    Level(String),
}

/// Prepare an isolated text request without retaining Harness task state.
#[derive(Clone, Debug)]
pub struct TextGenerationRequest {
    /// Model selected by the caller.
    pub model: String,
    /// Reasoning policy selected by the caller.
    pub reasoning: ReasoningSelection,
    /// Complete instructions for the isolated conversation.
    pub instructions: String,
    /// Prepared evidence and user input for generation.
    pub input: String,
}

/// Validate generated text according to the host's requested output surface.
pub trait TextValidator: Send + Sync {
    /// Normalize accepted output or explain why generation failed.
    fn validate(&self, text: &str) -> Result<String, ProviderError>;
}

/// Select the lowest advertised reasoning level independently of catalog order.
pub fn lowest_effort(model: &ProviderModel) -> Result<Option<String>, ProviderError> {
    if model.reasoning.is_empty() {
        return Ok(None);
    }
    let order = ["none", "minimal", "low", "medium", "high", "xhigh", "max"];
    let selected = order
        .iter()
        .find(|effort| model.reasoning.iter().any(|value| value == **effort));
    match selected {
        Some(effort) => Ok(Some((*effort).to_owned())),
        None => Err(ProviderError::Rejected {
            message: format!("Model {} has no recognized thinking level", model.id),
        }),
    }
}
