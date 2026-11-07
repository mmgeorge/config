//! Native provider protocols and state independent of Harness workflows and policy.

// Match the host recursion budget when proving nested SDK callback futures are Send.
#![recursion_limit = "256"]

mod acp;
mod codex;
mod copilot;
mod mock;
mod pending_input;
mod retained_task;
mod terminal;
pub mod testing;
mod tool_display;
mod turn_runner;

mod error;
mod profile;

mod permission_input;
pub mod registry;
