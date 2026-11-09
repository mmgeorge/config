mod attachments;
mod child_turn;
mod connection;
mod kind;
mod resources;
pub(crate) use kind::AcpKind;
mod profile;
mod response;

pub use connection::{AcpConnection, AcpConnectionError, AcpHandler, AcpLaunch, AcpRemoteFailure};
pub(crate) use response::PromptOutcome;
pub mod runtime;

pub mod submission;

pub mod children;
pub mod permission;
pub mod session;

pub mod conversation;

pub(crate) mod provider;

#[cfg(test)]
mod in_memory_peer;
#[cfg(test)]
mod session_fixture_test;
#[cfg(test)]
mod wire_fixture_test;

mod tool_semantics;
