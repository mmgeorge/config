use crate::ProviderError;
use async_trait::async_trait;
use std::sync::Arc;

/// Commit host-owned delivery evidence around a native provider send.
#[async_trait]
pub trait InputReceipt: Send + Sync {
    /// Get the host-generated identity used for native input correlation.
    fn id(&self) -> &crate::SubmissionId;

    /// Commit opaque adapter recovery evidence before the provider receives input.
    async fn record_sending(
        self: Arc<Self>,
        token: Option<crate::RecoveryToken>,
    ) -> Result<(), ProviderError>;

    /// Commit native evidence that the provider accepted input.
    async fn record_accepted(
        self: Arc<Self>,
        receipt: Option<crate::RecoveryToken>,
    ) -> Result<(), ProviderError>;

    /// Record an explicit native rejection that proves input was not accepted.
    async fn record_rejected(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError>;

    /// Record a failure without treating missing acknowledgment as rejection.
    async fn record_failure(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError>;
}

#[cfg(test)]
mod tests {
    use crate::RecoveryToken;

    /// Persist opaque recovery evidence without exposing an adapter format to the host.
    #[test]
    fn recovery_token_round_trips_as_opaque_bytes() {
        let token = RecoveryToken::new(vec![4, 2, 1]).unwrap();
        let encoded = serde_json::to_vec(&token).unwrap();
        let restored: RecoveryToken = serde_json::from_slice(&encoded).unwrap();
        assert_eq!(restored.as_bytes(), &[4, 2, 1]);
        assert!(serde_json::from_str::<RecoveryToken>("[]").is_err());
    }
}
