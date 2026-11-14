use super::events::{
    DiagnosticProgress, DiagnosticResult, FormatProgress, FormatResult, TestProgress, TestRunResult,
};
use serde::{Deserialize, Serialize};

/// Retain the structured outcome under the contract that produced it.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "category", rename_all = "snake_case")]
pub enum AdapterResult {
    Tests(TestRunResult),
    Diagnostics(DiagnosticResult),
    Format(FormatResult),
}

impl AdapterResult {
    /// Get test coverage when this command used the test contract.
    pub fn tests(&self) -> Option<&TestRunResult> {
        match self {
            Self::Tests(result) => Some(result),
            Self::Diagnostics(_) | Self::Format(_) => None,
        }
    }
}

/// Publish contract-specific progress without nullable category-dependent fields.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "category", rename_all = "snake_case")]
pub enum AdapterProgress {
    Tests(TestProgress),
    Diagnostics(DiagnosticProgress),
    Format(FormatProgress),
}
