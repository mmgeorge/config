//! Transport policy shared by the host reader and editor client.

use serde::Deserialize;

/// Determines whether a request envelope names an existing session.
///
/// Optional context never selects the active editor session when omitted.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum SessionContext {
    /// Session context is forbidden for this service-owned operation.
    None,
    /// Explicit session context is permitted but omission is independent of sessions.
    Optional,
    /// A nonempty session identity is required before dispatch.
    Required,
}

/// Minimum service initialization completed before sending an application request.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Readiness {
    /// The host has negotiated the current wire version.
    Host,
    /// Shared runtime and storage are ready, without implicitly opening a session.
    Runtime,
}

/// Selects the connection's concurrent-request admission category.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Admission {
    /// Shares ordinary capacity while preserving the reserved control allowance.
    Ordinary,
    /// May use capacity reserved for lifecycle and coordination requests.
    Control,
}

/// Owner responsible for reconciling a mutation after an uncertain response.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Recovery {
    /// The generic transport does not reconcile or replay this operation.
    None,
    /// The session command coordinator preserves and queries the original intent.
    SessionCommand,
    /// The feature service owns its durable operation identity and recovery rules.
    Service,
}

/// Immutable transport requirements generated from the shared method inventory.
///
/// The client and host consume the same inventory. These values do not grant
/// resource authorization or replace method-specific payload validation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct MethodPolicy {
    /// Requiredness of the envelope's explicit session identity.
    pub session_context: SessionContext,
    /// Minimum initialized service stage.
    pub readiness: Readiness,
    /// Concurrent-request capacity category.
    pub admission: Admission,
    /// Mutation reconciliation owner.
    pub recovery: Recovery,
}

impl MethodPolicy {
    /// Validate session-context presence without opening or selecting a session.
    ///
    /// Returns an error for missing required context, whitespace-only identities,
    /// or context supplied to an independent operation. Resource existence and
    /// authorization remain the owning service's responsibility.
    pub fn validate_session(self, session_id: Option<&str>) -> Result<(), String> {
        if session_id.is_some_and(|value| value.trim().is_empty()) {
            return Err("session_id must be a nonempty identifier".into());
        }
        match (self.session_context, session_id) {
            (SessionContext::Required, None) => {
                Err("method requires an explicit session_id".into())
            }
            (SessionContext::None, Some(_)) => Err("method does not accept session_id".into()),
            _ => Ok(()),
        }
    }
}
