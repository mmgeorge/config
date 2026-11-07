//! Host-assigned identities for normalized provider objects.
//!
//! A thread identity cannot be substituted for a turn identity.
//!
//! ```compile_fail
//! use forge_provider_api::{ThreadId, TurnId};
//! fn take_turn(_: TurnId) {}
//! let thread = ThreadId::new("thread:original").unwrap();
//! take_turn(thread);
//! ```

use serde::{Deserialize, Serialize};

/// Reject an identity that cannot name a normalized object.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("normalized object identity is empty")]
pub struct IdentityError;

macro_rules! identity {
    ($name:ident, $description:literal) => {
        #[doc = $description]
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
        #[serde(try_from = "String", into = "String")]
        pub struct $name(String);

        impl $name {
            /// Create an identity without interpreting its opaque contents.
            pub fn new(value: impl Into<String>) -> Result<Self, IdentityError> {
                let value = value.into();
                if value.trim().is_empty() {
                    return Err(IdentityError);
                }
                Ok(Self(value))
            }

            /// Get the original identity text.
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl TryFrom<String> for $name {
            type Error = IdentityError;

            /// Validate an identity received through serialization.
            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::new(value)
            }
        }

        impl From<$name> for String {
            /// Encode the identity without adding provider-specific prefixes.
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl std::fmt::Display for $name {
            /// Render the opaque identity at a presentation boundary.
            fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(formatter)
            }
        }
    };
}

identity!(
    ThreadId,
    "Identify a host-registered provider conversation."
);
identity!(
    TurnId,
    "Identify one host-admitted response cycle within a thread."
);
identity!(
    ItemId,
    "Identify one item retained under its original thread and turn."
);

identity!(
    UsageReportId,
    "Identify one native or adapter-local accounting report."
);
identity!(
    ModelCallId,
    "Identify one model-call occurrence within a normalized turn."
);

#[cfg(test)]
mod tests {
    use super::*;

    /// Transport decoding must enforce the same nonempty invariant as admission.
    #[test]
    fn transport_rejects_empty_normalized_identities() {
        for raw in ["\"\"", "\" \\t\""] {
            assert!(serde_json::from_str::<ThreadId>(raw).is_err());
            assert!(serde_json::from_str::<TurnId>(raw).is_err());
            assert!(serde_json::from_str::<ItemId>(raw).is_err());
            assert!(serde_json::from_str::<UsageReportId>(raw).is_err());
            assert!(serde_json::from_str::<ModelCallId>(raw).is_err());
        }
        let id = ItemId::new("item:original").unwrap();
        let encoded = serde_json::to_string(&id).unwrap();
        assert_eq!(serde_json::from_str::<ItemId>(&encoded).unwrap(), id);
    }
}
