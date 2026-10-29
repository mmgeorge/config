use serde::{Deserialize, Serialize};

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
