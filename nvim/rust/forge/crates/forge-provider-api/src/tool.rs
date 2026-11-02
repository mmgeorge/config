use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolInvocation {
    pub kind: ToolActivityKind,
    pub name: String,
    pub title: String,
    pub arguments: serde_json::Value,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolDiagnostic {
    pub code: Option<String>,
    pub message: String,
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
    Failed { diagnostic: ToolDiagnostic },
    Denied { reason: String },
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
    pub fn parse(status: &str) -> anyhow::Result<Self> {
        match status.to_ascii_lowercase().as_str() {
            "pending" | "running" | "inprogress" | "in_progress" => Ok(Self::Running),
            "completed" | "complete" | "success" | "succeeded" => Ok(Self::Succeeded),
            "failed" | "error" => Ok(Self::Failed),
            "denied" | "declined" | "rejected" => Ok(Self::Denied),
            "cancelled" | "canceled" => Ok(Self::Cancelled),
            "interrupted" => Ok(Self::Interrupted),
            _ => anyhow::bail!("unsupported provider tool status: {status}"),
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

impl std::ops::Deref for ToolStatus {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

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

impl ToolInvocation {
    /// Formats a call with sorted, credential-redacted arguments, decoding JSON strings first.
    /// Returns `None` for an empty name or invalid encoded JSON.
    pub fn call_title(name: &str, arguments: &Value) -> Option<String> {
        let name = name.split_whitespace().collect::<Vec<_>>().join(" ");
        if name.is_empty() {
            return None;
        }
        let arguments = match arguments {
            Value::String(encoded) => serde_json::from_str(encoded).ok()?,
            arguments => arguments.clone(),
        };
        let mut arguments = Self::redact_arguments(&arguments);
        arguments.sort_all_objects();
        Some(format!(
            "{name}({})",
            serde_json::to_string(&arguments).ok()?
        ))
    }

    fn redact_arguments(value: &Value) -> Value {
        match value {
            Value::Array(values) => {
                Value::Array(values.iter().map(Self::redact_arguments).collect())
            }
            Value::Object(values) => Value::Object(
                values
                    .iter()
                    .map(|(key, value)| {
                        let value = if Self::sensitive_argument(key) {
                            Value::String("[REDACTED]".into())
                        } else {
                            Self::redact_arguments(value)
                        };
                        (key.clone(), value)
                    })
                    .collect(),
            ),
            _ => value.clone(),
        }
    }

    fn sensitive_argument(key: &str) -> bool {
        matches!(
            key.to_ascii_lowercase().replace(['-', '_'], "").as_str(),
            "token"
                | "apikey"
                | "key"
                | "secret"
                | "password"
                | "passphrase"
                | "authorization"
                | "bearer"
                | "cookie"
                | "session"
                | "credential"
                | "accesstoken"
                | "refreshtoken"
                | "clientsecret"
                | "privatekey"
        )
    }
}
