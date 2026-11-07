use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ToolDiagnostic {
    /// Carry a normalized failure class, without native codes or verbatim messages.
    pub kind: crate::error::ProviderFailureKind,
    /// Preserve useful host or adapter text after native detail has been sanitized.
    pub message: crate::diagnostic::SanitizedMessage,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "text", rename_all = "snake_case")]
pub enum TextUpdate {
    Append(String),
    Replace(String),
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ToolOutcome {
    Succeeded,
    Failed {
        diagnostic: ToolDiagnostic,
    },
    Denied {
        reason: crate::diagnostic::SanitizedMessage,
    },
    Cancelled,
    Interrupted,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Provider-independent tool execution status. Late output does not reopen execution.
pub enum ToolStatus {
    Running,
    Succeeded,
    Failed,
    Denied,
    Cancelled,
    Interrupted,
}

impl ToolStatus {
    /// Decode only the canonical host representation; native aliases belong to adapters.
    pub fn parse(status: &str) -> Result<Self, ParseToolStatusError> {
        match status {
            "running" => Ok(Self::Running),
            "completed" => Ok(Self::Succeeded),
            "failed" => Ok(Self::Failed),
            "denied" => Ok(Self::Denied),
            "cancelled" => Ok(Self::Cancelled),
            "interrupted" => Ok(Self::Interrupted),
            _ => Err(ParseToolStatusError),
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Running => "running",
            Self::Succeeded => "completed",
            Self::Failed => "failed",
            Self::Denied => "denied",
            Self::Cancelled => "cancelled",
            Self::Interrupted => "interrupted",
        }
    }
}

/// Reject a noncanonical status without retaining provider-specific text.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ParseToolStatusError;

impl std::fmt::Display for ParseToolStatusError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("invalid normalized tool status")
    }
}

impl std::error::Error for ParseToolStatusError {}

impl std::fmt::Display for ToolStatus {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.as_str())
    }
}

/// Defines the visible action verb for one normalized tool invocation.
/// Carries the operation meaning without requiring native tool-name or argument parsing.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ToolSemantics {
    #[default]
    Other,
    Command {
        text: String,
        shell: Option<String>,
        directory: Option<String>,
    },
    Read {
        paths: Vec<String>,
        directory: Option<String>,
    },
}

/// Retain native arguments for inspection without defining event lifecycle.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(transparent)]
pub struct InspectableArguments(serde_json::Value);

impl InspectableArguments {
    /// Create an inspection-only native argument snapshot.
    pub fn new(value: serde_json::Value) -> Self {
        Self(value)
    }
    /// Get the native snapshot for inspection or rendering.
    pub fn value(&self) -> &serde_json::Value {
        &self.0
    }
    /// Consume the native snapshot at a transport boundary.
    pub fn into_value(self) -> serde_json::Value {
        self.0
    }
}

/// Describe a native operation independently of the immutable enclosing item type.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolDescriptor {
    /// Preserve the native operation name for display.
    pub name: String,
    /// Preserve the current display title.
    pub title: String,
    /// Carry normalized operation meaning without parsing native arguments.
    pub semantics: ToolSemantics,
    /// Retain raw arguments exclusively for inspection.
    pub arguments: InspectableArguments,
}

/// Preserve streamed arguments and structured native snapshots separately.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ToolInputUpdate {
    /// Apply an ordered textual argument update.
    Text(TextUpdate),
    /// Replace the structured argument snapshot.
    Arguments(InspectableArguments),
}

/// Preserve metadata, positive execution evidence, output, and native changes.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ToolItemEvent {
    /// Refine the descriptor without admitting another item.
    DescriptorUpdated(ToolDescriptor),
    /// Change the displayed title of the same operation.
    TitleChanged(String),
    /// Apply argument text or a structured snapshot.
    Input(ToolInputUpdate),
    /// Record positive evidence that execution began.
    ExecutionStarted,
    /// Record positive evidence that work survives foreground completion.
    Backgrounded,
    /// Append output or replace it with an authoritative snapshot.
    Output(TextUpdate),
    /// Preserve native file observations separately from host checkpoints.
    Changes(crate::change::ProviderChangeSet),
}

/// Freeze the tool's outcome and independently available final snapshots.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolCompletion {
    /// Preserve the actual native operation result.
    pub outcome: ToolOutcome,
    /// Replace the prior descriptor only when a final snapshot was reported.
    pub descriptor: Option<ToolDescriptor>,
    /// Replace accumulated output only when a final snapshot was reported.
    pub output: Option<String>,
    /// Preserve final native file observations.
    pub changes: crate::change::ProviderChangeSet,
}
