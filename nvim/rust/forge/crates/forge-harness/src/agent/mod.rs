mod agent;
mod backend;
mod capability;
mod coordinator;
mod delegation;
mod event;
mod registry;
mod runtime;
mod selection;
mod task;
mod tool_scope;
#[cfg(test)]
mod coordinator_test;

pub use selection::AgentSelection;
pub use coordinator::{AgentAdmission, AgentCommand, AgentCommandSender, AgentCoordinator, AgentOperation, AgentQuiescence, AgentRequest, SpawnAgentRequest};
pub use task::{AgentMessage, AgentSource, AgentTask, MessageDelivery};

pub use agent::{Agent, AgentExecutionState, AgentState};
pub use backend::{AgentBackendFactory, provider_name};
pub use capability::AgentCapability;
pub use delegation::Delegation;
pub use event::AgentLifecycleEvent;
pub use registry::AgentRegistry;
pub use runtime::AgentRuntime;
pub use tool_scope::{AgentToolLease, AgentToolScope};
