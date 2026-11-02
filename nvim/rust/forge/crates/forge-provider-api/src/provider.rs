use crate::capability::{CapabilityManifest, UnsupportedCapability};
use crate::catalog::{CatalogMutation, McpDefinition, ProviderCatalogRequest, SkillDefinition};
use crate::*;
use crate::{events, terminal};
use anyhow::Result;
use async_trait::async_trait;
use serde_json::Value;

/// Host-owned request, workflow, and result types used by a provider integration.
/// Native protocol implementations remain independent of these associated types.
pub trait ProviderHost: Send + Sync + 'static {
    type Request: Send + Sync;
    type Completion: Send;
    type Output: Send;
    type ForkRequest: Send;
    type CompactionRequest: Send;
    type TextGeneration: Send;
    type SteeringInput: Send;
    type Settings: Sync;
    type Submission: Sync;
    type MockResponseControl: Sync;
    fn owner(request: &Self::Request) -> Result<ExecutionOwner, ProviderError>;
    fn output(events: Vec<ProviderEvent>, completion: Self::Completion) -> Self::Output;
    fn has_resources(settings: &Self::Settings) -> bool;
}

/// Requires session lifecycle ownership and provides explicit failures for optional operations.
#[async_trait]
pub trait Provider: Send + Sync + 'static {
    type Host: ProviderHost;
    /// Submit input to a retained session; success means acceptance, not execution completion.
    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: <Self::Host as ProviderHost>::Request,
        scope: InvocationScope,
    ) -> Result<(), ProviderError>;

    /// Observe normalized events through the lifetime of one execution owner's session.
    async fn session_events(
        &self,
        owner: ExecutionOwner,
    ) -> Result<events::ProviderEventStream, ProviderError>;

    /// Retire one failed owner's native binding and observation generation before replacement.
    async fn reset_session(&self, _owner: &ExecutionOwner) -> Result<()> {
        Err(UnsupportedCapability("session_replacement").into())
    }

    /// Revoke callback authority before collecting an owner with unreliable observation.
    async fn fence_session(&self, _owner: &ExecutionOwner) -> Result<()> {
        Err(UnsupportedCapability("session_fencing").into())
    }

    /// Submit steering to its original owner without exposing native turn placement.
    async fn session_steer(
        self: std::sync::Arc<Self>,
        owner: &ExecutionOwner,
        scope: InvocationScope,
        input: <Self::Host as ProviderHost>::SteeringInput,
    ) -> Result<(), ProviderError> {
        if scope.owner != *owner {
            return Err(ProviderError::Rejected {
                message: "steering scope belongs to a different execution owner".into(),
            });
        }
        let _ = input;
        Err(UnsupportedCapability("native_steer").into())
    }
    /// Create or resume provider history and connections without admitting a model turn.
    /// The returned identity belongs to the requested Harness session in this runtime.
    /// The broker decides whether to resume or replace it when reopening local history.
    async fn initialize_session(
        &self,
        _request: <Self::Host as ProviderHost>::Request,
    ) -> Result<Option<ProviderSessionId>> {
        Ok(None)
    }

    /// Expose explicit response barriers only for the in-process mock provider.
    fn mock_response_control(&self) -> Option<&<Self::Host as ProviderHost>::MockResponseControl> {
        None
    }

    /// Reports whether every local tool waits on the request's checkpoint barrier.
    fn checkpoint_tool_gate(&self) -> bool {
        false
    }

    /// Inspect the installed provider runtime without creating a conversation.
    async fn setup(&self, _request: ProviderCatalogRequest) -> Result<Value> {
        Err(UnsupportedCapability("runtime_setup").into())
    }

    /// Authenticate explicitly without admitting a model prompt or creating provider history.
    async fn authenticate(&self, _request: ProviderCatalogRequest, _method: String) -> Result<()> {
        Err(UnsupportedCapability("authentication").into())
    }

    /// Return support and discovery state for the selected session without launching provider work.
    async fn capabilities(&self, _request: ProviderCatalogRequest) -> Result<CapabilityManifest> {
        Ok(CapabilityManifest::from_descriptor(self.descriptor()))
    }

    /// Reconcile portable resource selection before reporting configuration as applied.
    async fn reconcile_settings(
        &self,
        _request: ProviderCatalogRequest,
        setting: &<Self::Host as ProviderHost>::Settings,
    ) -> Result<()> {
        if <Self::Host as ProviderHost>::has_resources(setting) {
            return Err(UnsupportedCapability("resource_selection").into());
        }
        Ok(())
    }
    /// Collect provider process ownership after admitted session work has drained.
    /// Publish final observations for known background tools before closing every
    /// session event stream. Callers drain those streams before releasing session leases.
    async fn shutdown(&self) -> Result<()>;

    /// Return stable provider identity and capability without starting a provider process.
    fn descriptor(&self) -> ProviderDescriptor;

    /// Plan and execution scopes remain bound to their provider request until its turn ends.
    fn supports_task_steering(&self, kind: crate::input::TaskKind) -> bool {
        kind == crate::input::TaskKind::Goal
    }

    /// Returns a native receipt only when history proves this exact submission was accepted.
    /// Missing history and unsupported reconciliation return None and never authorize replay.
    async fn reconcile_submission(
        &self,
        _submission: &<Self::Host as ProviderHost>::Submission,
        _known_receipts: &std::collections::HashSet<String>,
    ) -> Result<Option<String>> {
        Ok(None)
    }

    /// Fork a provider session only when the provider advertises native support.
    async fn fork(
        &self,
        _request: <Self::Host as ProviderHost>::ForkRequest,
    ) -> Result<ProviderForkResult> {
        Err(UnsupportedCapability("native_fork").into())
    }

    /// Query live provider shells independently of prompt execution and exchange completion.
    async fn background_terminals(
        &self,
        _request: ProviderCatalogRequest,
    ) -> Result<terminal::TerminalSnapshot> {
        Err(UnsupportedCapability("background_terminals").into())
    }

    /// Terminate one provider-owned background shell without interrupting its exchange.
    async fn terminate_terminal(&self, _request: ProviderCatalogRequest, _id: &str) -> Result<()> {
        Err(UnsupportedCapability("background_termination").into())
    }

    /// Generate text independently of the main provider conversation.
    async fn generate_text(
        &self,
        _request: ProviderCatalogRequest,
        _purpose: <Self::Host as ProviderHost>::TextGeneration,
        _model: &str,
        _reasoning_effort: Option<&str>,
        _history: &str,
    ) -> Result<String> {
        Err(UnsupportedCapability("text_generation").into())
    }

    /// List provider models for the Harness model picker.
    async fn model_list(
        &self,
        _request: <Self::Host as ProviderHost>::Request,
    ) -> Result<Vec<ProviderModel>> {
        Err(UnsupportedCapability("model_selection").into())
    }

    /// List user-invocable provider skills with their effective enabled state.
    async fn skill_list(&self, _request: ProviderCatalogRequest) -> Result<Vec<SkillDefinition>> {
        Err(UnsupportedCapability("skill_catalog").into())
    }

    /// Persist one provider skill's enabled state for future turns.
    async fn set_skill_enabled(
        &self,
        _request: ProviderCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        Err(UnsupportedCapability("skill_mutation").into())
    }

    /// List configured servers without waiting for their startup handshakes.
    async fn mcp_configuration(
        &self,
        _request: ProviderCatalogRequest,
    ) -> Result<Vec<McpDefinition>> {
        Err(UnsupportedCapability("mcp_catalog").into())
    }

    /// Query server readiness and tool definitions when the provider exposes them.
    async fn mcp_list(&self, _request: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
        Err(UnsupportedCapability("mcp_catalog").into())
    }

    /// Persist one MCP server's enabled state and report whether its turn must resume.
    async fn set_mcp_enabled(
        &self,
        _request: ProviderCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        Err(UnsupportedCapability("mcp_mutation").into())
    }

    /// Return whether one Harness session currently owns an in-flight provider turn.
    async fn has_active_turn(&self, session_id: &str) -> bool;

    /// Clear provisional native turn identity before admitting the next broker invocation.
    async fn prepare_turn(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }

    /// Stop native continuation through the active reader before acquiring the broker lock.
    /// The caller races reader readiness against broker availability and may cancel this wait.
    async fn stop_goal_session(&self, _session_id: &str, _clear: bool) -> Result<()> {
        Ok(())
    }

    /// Update a native provider goal when that provider owns goal persistence.
    async fn goal_status(
        &self,
        _request: <Self::Host as ProviderHost>::Request,
        _update: NativeGoalUpdate,
    ) -> Result<()> {
        Ok(())
    }

    /// Compact provider-owned conversation history when the provider advertises support.
    async fn compact(
        &self,
        _request: <Self::Host as ProviderHost>::CompactionRequest,
    ) -> Result<<Self::Host as ProviderHost>::Output> {
        Err(UnsupportedCapability("native_compact").into())
    }

    /// Interrupt the parent and descendants for one session and await terminal evidence.
    async fn cleanup_execution(&self, session_id: &str) -> Result<()>;

    /// Stop and collect one session through the same execution cleanup contract.
    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        self.cleanup_execution(session_id).await
    }

    /// Return one Harness session's provider conversation after interruption.
    async fn active_session_id_for(&self, session_id: &str) -> Result<Option<ProviderSessionId>>;

    /// Roll back a cancelled turn after the broker proves it produced no visible output or changes.
    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(false)
    }

    /// Roll back one Harness session's output-free cancelled turn.
    async fn rollback_cancelled_turn_for(&self, _session_id: &str) -> Result<bool> {
        self.rollback_cancelled_turn().await
    }
}
