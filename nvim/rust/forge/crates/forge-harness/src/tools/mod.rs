mod authority;
pub(crate) mod control_server;
pub(crate) mod control_session;
mod error;
mod executor;
mod failure;
mod invocation;
pub(crate) mod mcp;
mod registry;
mod runtime;
mod schema;
mod stdio;

mod agents;
mod checks;
mod config;
mod goals;
pub(crate) mod lsp;
mod plan;
mod questions;
pub mod repository;
pub(crate) mod session;
mod toolsets;

pub(crate) use toolsets::{
    CHECK_DEFINITION_EDIT, list as toolsets, unavailable_reason as unavailable_tool_reason,
};

pub(crate) use authority::{PermissionAuthoritySnapshot, TaskAuthority, TaskAuthorityProgress};
pub use error::{ToolError, ToolResult};
pub(crate) use executor::ControlToolExecutor;
pub(crate) use failure::{
    ControlToolArgumentError, control_tool_failure_text, schema_violation_list,
};
pub use invocation::{ControlToolInvocation, apply_invocation};
pub(crate) use invocation::{json_pointer_to_path, validate_arguments};
pub use questions::{
    plan_question_input_schema, question_answer_input_schema, question_withdraw_input_schema,
};
pub use registry::{ControlToolDefinition, ControlToolRegistry};
pub(crate) use runtime::ProviderExecutionPreview;
pub use runtime::{ControlToolResult, ControlTurnContext};
pub use stdio::run_stdio;
