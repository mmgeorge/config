use forge_provider_api::ProviderId;

/// Select a private implementation without exposing native variants in the shared API.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum AcpKind {
    Claude,
    Antigravity,
}

impl AcpKind {
    /// Get the canonical registry identity for this native implementation.
    pub(crate) fn id(self) -> ProviderId {
        let value = match self {
            Self::Claude => "claude",
            Self::Antigravity => "antigravity",
        };
        ProviderId::new(value).expect("native implementation identities are nonempty")
    }
}
