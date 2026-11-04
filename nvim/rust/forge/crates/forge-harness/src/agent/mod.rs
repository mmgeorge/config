mod activity;
mod capability;
mod coordinator;
#[cfg(test)]
mod coordinator_test;
mod delegation;
mod identity;
mod profile;
mod provider;
mod registry;
mod runtime;
mod selection;
mod task;
mod tool_scope;

pub(crate) use activity::status as tool_activity_status;
pub(crate) use activity::{AgentActivity, AgentToolReference};
pub use coordinator::AgentTarget;
pub use coordinator::{
    AgentAdmission, AgentCommand, AgentCommandSender, AgentCommit, AgentCoordinator,
    AgentOperation, AgentQuiescence, AgentRequest, AgentRequestAuthority, AgentWork,
    SpawnAgentRequest,
};
pub use profile::{AgentProfile, AgentProfileDocument, AgentProfileStore};
pub use selection::AgentSelection;
pub use task::{AgentMessage, AgentSource, AgentTask, MessageDelivery};

pub use capability::AgentCapability;
pub use delegation::Delegation;
pub use forge_provider_api::agent::AgentLifecycleEvent;
pub use identity::{Agent, AgentExecutionState, AgentState};
pub use provider::AgentProviderFactory;
pub use registry::AgentRegistry;
pub use runtime::AgentRuntime;
pub use tool_scope::{AgentToolLease, AgentToolScope};
