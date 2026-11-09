//! Claude Code through its native stream-json protocol.
//!
//! Forge launches `claude -p` with stream-json input and output, owns the control channel
//! (permissions, SDK MCP host tools, interrupts, model and mode changes), and decodes the
//! native event stream into provider events.

mod conversation;
mod host_tools;
mod process;
mod profile;
mod provider;
mod session;
mod tool_targets;
mod transport;
mod wire;

pub use provider::ClaudeProvider;

#[cfg(test)]
mod provider_test;
#[cfg(test)]
mod test_peer;
