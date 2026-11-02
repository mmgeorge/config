use serde_json::Value;

/// Records native diagnostics without depending on a host's logging store.
pub trait ProviderTrace: Send + Sync {
    /// Record a provider event under the host's correlation identity.
    fn record(&self, session_id: &str, event: &str, payload: Value);
}
