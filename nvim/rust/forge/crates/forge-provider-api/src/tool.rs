use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolInvocation {
    /// Adapter-normalized operation metadata; raw arguments are inspection-only.
    #[serde(default)]
    pub semantics: ToolSemantics,
    pub kind: ToolActivityKind,
    pub name: String,
    pub title: String,
    pub arguments: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(from = "StoredToolDiagnostic")]
pub struct ToolDiagnostic {
    /// Carry a normalized failure class, without native codes or verbatim messages.
    pub kind: crate::error::ProviderFailureKind,
    /// Preserve useful host or adapter text after native detail has been sanitized.
    pub message: crate::diagnostic::SanitizedMessage,
}

#[derive(Deserialize)]
#[serde(untagged, deny_unknown_fields)]
enum StoredToolDiagnostic {
    Current {
        kind: crate::error::ProviderFailureKind,
        message: crate::diagnostic::SanitizedMessage,
    },
    Legacy {
        code: serde::de::IgnoredAny,
        message: serde::de::IgnoredAny,
    },
}

impl From<StoredToolDiagnostic> for ToolDiagnostic {
    /// Normalize legacy diagnostics without restoring native codes or error text.
    fn from(stored: StoredToolDiagnostic) -> Self {
        match stored {
            StoredToolDiagnostic::Current { kind, message } => Self { kind, message },
            StoredToolDiagnostic::Legacy { code, message } => {
                let _ = (code, message);
                Self {
                    kind: crate::error::ProviderFailureKind::Unknown,
                    message: crate::diagnostic::SanitizedMessage::new("Tool execution failed")
                        .expect("static diagnostic is nonempty"),
                }
            }
        }
    }
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

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ToolUpdate {
    Requested {
        invocation: ToolInvocation,
    },
    /// Update display text without replacing the admitted tool invocation.
    TitleChanged {
        title: String,
    },
    Input {
        update: TextUpdate,
    },
    Started,
    /// Retain a proven running command after its foreground turn completes.
    Backgrounded,
    Output {
        update: TextUpdate,
    },
    Changes {
        changes: crate::change::ProviderChangeSet,
    },
    SkillRead {
        read: Vec<crate::skill::SkillRead>,
    },
    Finished {
        outcome: ToolOutcome,
    },
}

impl ToolUpdate {
    /// Permit late metadata and output without reopening an existing tool execution.
    pub fn can_update_settled(&self) -> bool {
        match self {
            Self::TitleChanged { .. }
            | Self::Output { .. }
            | Self::Changes { .. }
            | Self::SkillRead { .. } => true,
            Self::Requested { .. }
            | Self::Input { .. }
            | Self::Started
            | Self::Backgrounded
            | Self::Finished { .. } => false,
        }
    }
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolActivityKind {
    Command,
    FileChange,
    ToolCall,
}

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

#[cfg(test)]
mod tests {
    use super::*;

    /// Load old failed tools while keeping their native diagnostic details private.
    #[test]
    fn legacy_tool_diagnostics_are_normalized_on_load() {
        for code in [serde_json::Value::Null, serde_json::json!("native-code")] {
            let diagnostic: ToolDiagnostic = serde_json::from_value(serde_json::json!({
                "code":code,"message":"private native error"
            }))
            .unwrap();
            assert_eq!(diagnostic.kind, crate::error::ProviderFailureKind::Unknown);
            let serialized = serde_json::to_string(&diagnostic).unwrap();
            assert!(!serialized.contains("private native"));
            assert!(!serialized.contains("native-code"));
            assert_eq!(
                serde_json::from_str::<ToolDiagnostic>(&serialized).unwrap(),
                diagnostic
            );
        }
        assert!(
            serde_json::from_value::<ToolDiagnostic>(
                serde_json::json!({"message":"missing category"})
            )
            .is_err()
        );
        assert!(
            serde_json::from_value::<ToolDiagnostic>(serde_json::json!({
                "kind":"invalid", "code":null, "message":"bad current category"
            }))
            .is_err()
        );
    }

    /// Preserve the existing denial wire shape while validating its diagnostic text.
    #[test]
    fn denial_reason_roundtrips_as_validated_text() {
        let value =
            serde_json::json!({"kind":"denied", "reason":"Host policy denied this operation"});
        let outcome: ToolOutcome = serde_json::from_value(value.clone()).unwrap();
        assert_eq!(serde_json::to_value(outcome).unwrap(), value);
        assert!(
            serde_json::from_value::<ToolOutcome>(
                serde_json::json!({"kind":"denied", "reason":" "})
            )
            .is_err()
        );
    }
}
