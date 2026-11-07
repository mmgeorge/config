mod catalog;
mod child_turn;
mod client_generation;
mod compaction;
mod control;
pub(crate) mod decode;
pub(crate) mod error;
mod event_decoder;
mod event_stream;
mod file_change;
mod handle;
mod history_repair;
mod input;
mod intake;
mod mcp;
mod message_state;
mod operation;
mod permission;
mod prompt;
pub(crate) mod provider;
mod runtime;
mod session;
mod session_directory;
mod sessions;
mod shell_session;
mod shell_tools;
mod terminal;
mod turn;

mod compaction_metrics;
mod tool_semantics;

#[cfg(test)]
mod test_peer;

#[cfg(test)]
mod test_turn;
