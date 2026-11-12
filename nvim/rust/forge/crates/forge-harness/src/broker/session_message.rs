//! Route a message from one session into another session's input queue.
//!
//! A broker owns one session and cannot reach another session's controller. The service that owns
//! every controller binds a route here when its registry exists, and a tool that addresses another
//! session delivers through it. A host without a bound route, such as a standalone broker, rejects
//! delivery instead of dropping the message.

use anyhow::{Context, Result};
use async_trait::async_trait;
use std::sync::{Arc, RwLock};

/// One message addressed to another session.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SessionMessage {
    pub(crate) target_session_id: String,
    /// Identity of the input the message becomes in the target's queue.
    pub(crate) input_id: String,
    /// The sender and its words, which reach the target's model as a `<session_message>`.
    pub(crate) content: forge_provider_api::forge::SessionMessage,
    pub(crate) delivery: SessionMessageDelivery,
}

/// Selects when the target session reads a message.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SessionMessageDelivery {
    /// Wait behind the target's running turn and queued input.
    #[default]
    Queue,
    /// Enter the target's running turn as steering, or start a turn when it is idle.
    Steer,
}

/// Admits a message into its target session as that session's editor would admit user input.
#[async_trait]
pub(crate) trait SessionMessageRoute: Send + Sync {
    async fn deliver(&self, message: SessionMessage) -> Result<()>;
}

/// Holds the route the owning service bound for this host's sessions.
#[derive(Default)]
pub(crate) struct SessionMessageRouter {
    route: RwLock<Option<Arc<dyn SessionMessageRoute>>>,
}

impl SessionMessageRouter {
    /// Bind the route of the service that currently owns the session controllers.
    pub(crate) fn bind(&self, route: Arc<dyn SessionMessageRoute>) {
        *self
            .route
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(route);
    }

    /// Deliver one message, or report that this host routes no messages between sessions.
    pub(crate) async fn deliver(&self, message: SessionMessage) -> Result<()> {
        let route = self
            .route
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
            .context("this Harness host cannot deliver messages between sessions")?;
        route.deliver(message).await
    }
}
