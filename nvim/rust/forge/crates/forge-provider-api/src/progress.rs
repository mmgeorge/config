use crate::{ProviderRuntime, context::ContextUsage};

/// Receive native turn observations without exposing host workflow state to a provider.
pub trait TurnProgress: Send + Sync {
    /// Get the conversation receiving the current provider events.
    fn session_id(&self) -> Option<&str>;

    /// Get the provider identity and resolved model.
    fn runtime(&self) -> &ProviderRuntime;

    /// Record the model selected by the native runtime.
    fn set_model(&mut self, model: String);

    /// Record the context usage reported by the provider.
    fn set_context_usage(&mut self, usage: ContextUsage);

    /// Record that a native tool was admitted during this turn.
    fn record_tool_call(&mut self);

    /// Record a successfully completed native workspace mutation.
    fn record_workspace_change(&mut self, changed: bool);
}
