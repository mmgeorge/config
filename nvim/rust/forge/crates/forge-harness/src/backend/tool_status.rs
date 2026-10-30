use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct ToolInvocation {
    pub kind: super::ToolActivityKind,
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
    Requested { invocation: ToolInvocation },
    Input { update: TextUpdate },
    Started,
    Output { update: TextUpdate },
    Changes { changes: super::ProviderChangeSet },
    SkillRead { read: Vec<crate::skill::SkillRead> },
    Finished { outcome: ToolOutcome },
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
    pub(crate) fn parse(status: &str) -> anyhow::Result<Self> {
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

    pub(crate) fn as_str(self) -> &'static str {
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
