use crate::ProviderError;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Store versioned, adapter-owned recovery evidence without exposing native variants.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "tracking", rename_all = "snake_case")]
pub enum SubmissionTarget {
    Native {
        format: String,
        version: u32,
        conversation_id: String,
        checkpoint: serde_json::Value,
    },
    Untracked,
}

impl SubmissionTarget {
    /// Capture an adapter's exact current checkpoint before uncertain delivery begins.
    pub fn native(
        format: &str,
        conversation_id: impl Into<String>,
        checkpoint: serde_json::Value,
    ) -> Self {
        Self::Native {
            format: format.into(),
            version: 1,
            conversation_id: conversation_id.into(),
            checkpoint,
        }
    }
    /// Read evidence only when the adapter recognizes its exact checkpoint version.
    pub fn native_checkpoint(
        &self,
        expected: &str,
        version: u32,
    ) -> Option<(&str, &serde_json::Value)> {
        match self {
            Self::Native {
                format,
                version: stored,
                conversation_id,
                checkpoint,
            } if format == expected && *stored == version => Some((conversation_id, checkpoint)),
            Self::Native { .. } | Self::Untracked => None,
        }
    }
    /// Get the opaque conversation identity after the adapter confirms acceptance.
    pub fn conversation_id(&self) -> Option<&str> {
        match self {
            Self::Native {
                conversation_id, ..
            } => Some(conversation_id),
            Self::Untracked => None,
        }
    }
}

/// Commit host-owned delivery evidence around a native provider send.
#[async_trait]
pub trait InputReceipt: Send + Sync {
    /// Get the host-generated identity used for native input correlation.
    fn id(&self) -> &str;

    /// Commit an uncertain send before the provider receives input.
    async fn record_sending(self: Arc<Self>, target: SubmissionTarget)
    -> Result<(), ProviderError>;

    /// Commit native evidence that the provider accepted input.
    async fn record_accepted(self: Arc<Self>, receipt: String) -> Result<(), ProviderError>;

    /// Record an explicit native rejection that proves input was not accepted.
    async fn record_rejected(self: Arc<Self>, message: String) -> Result<(), ProviderError>;

    /// Record a failure without treating missing acknowledgment as rejection.
    async fn record_failure(self: Arc<Self>, message: String) -> Result<(), ProviderError>;
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn recovery_requires_the_adapters_exact_checkpoint_version() {
        let target = SubmissionTarget::native(
            "fixture",
            "opaque-conversation",
            serde_json::json!({"cursor":"before-send"}),
        );
        assert_eq!(target.conversation_id(), Some("opaque-conversation"));
        assert!(target.native_checkpoint("another-adapter", 1).is_none());
        assert!(target.native_checkpoint("fixture", 2).is_none());
        assert_eq!(
            target.native_checkpoint("fixture", 1).unwrap().1["cursor"],
            "before-send"
        );
    }
}
