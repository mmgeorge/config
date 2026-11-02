//! Validate native runtime versions independently from compiled SDK versions.

use std::{fmt, path::PathBuf};

use crate::ProviderKind;

/// An adapter-owned minimum version and the runtime selected for this initialization.
#[derive(Clone, Debug)]
pub struct RuntimeRequirement {
    provider: ProviderKind,
    runtime: PathBuf,
    minimum: semver::Version,
}

impl RuntimeRequirement {
    /// Create a requirement after resolving the actual executable or native connection.
    pub fn new(provider: ProviderKind, runtime: PathBuf, minimum: semver::Version) -> Self {
        Self {
            provider,
            runtime,
            minimum,
        }
    }

    /// Validate the native version before creating or resuming a conversation.
    pub fn validate(
        &self,
        reported: Option<&str>,
    ) -> Result<ValidatedRuntime, RuntimeVersionError> {
        let reported = reported
            .filter(|version| !version.trim().is_empty())
            .ok_or_else(|| RuntimeVersionError::Missing {
                requirement: self.clone(),
            })?;
        let version =
            semver::Version::parse(reported).map_err(|source| RuntimeVersionError::Malformed {
                requirement: self.clone(),
                reported: reported.to_owned(),
                source,
            })?;
        if version < self.minimum {
            return Err(RuntimeVersionError::Unsupported {
                requirement: self.clone(),
                detected: version,
            });
        }
        Ok(ValidatedRuntime {
            requirement: self.clone(),
            detected: version,
        })
    }
}

impl fmt::Display for RuntimeRequirement {
    /// Render the selected runtime and upgrade requirement at a diagnostic boundary.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} runtime {} requires version {} or newer",
            self.provider,
            self.runtime.display(),
            self.minimum
        )
    }
}

/// Proof that the selected runtime passed its adapter's version requirement.
#[derive(Clone, Debug)]
pub struct ValidatedRuntime {
    requirement: RuntimeRequirement,
    detected: semver::Version,
}

impl ValidatedRuntime {
    /// Get the validated native version.
    pub fn version(&self) -> &semver::Version {
        &self.detected
    }

    /// Get the runtime identity to which this proof is bound.
    pub fn requirement(&self) -> &RuntimeRequirement {
        &self.requirement
    }
}

/// Distinguish missing, malformed, and unsupported runtime versions for initialization.
#[derive(Debug)]
pub enum RuntimeVersionError {
    /// The native runtime supplied no usable version.
    Missing { requirement: RuntimeRequirement },
    /// The reported value was not a semantic version.
    Malformed {
        requirement: RuntimeRequirement,
        reported: String,
        source: semver::Error,
    },
    /// The selected runtime predates the adapter's required native API.
    Unsupported {
        requirement: RuntimeRequirement,
        detected: semver::Version,
    },
}

impl fmt::Display for RuntimeVersionError {
    /// Describe the installed and required versions with an explicit upgrade action.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Missing { requirement } => write!(
                f,
                "{requirement}; no version was reported. Install a supported runtime."
            ),
            Self::Malformed {
                requirement,
                reported,
                ..
            } => write!(
                f,
                "{requirement}; reported version {reported:?} is invalid. Install a supported runtime."
            ),
            Self::Unsupported {
                requirement,
                detected,
            } => write!(
                f,
                "{requirement}; detected {detected}. Upgrade this runtime before retrying."
            ),
        }
    }
}

impl std::error::Error for RuntimeVersionError {
    /// Preserve the parser cause for malformed native versions.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Malformed { source, .. } => Some(source),
            Self::Missing { .. } | Self::Unsupported { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compare versions numerically and reject prereleases below a stable minimum.
    #[test]
    fn initialization_requires_supported_native_version() {
        let requirement = RuntimeRequirement::new(
            ProviderKind::Copilot,
            "copilot".into(),
            semver::Version::new(1, 0, 10),
        );
        assert!(requirement.validate(Some("1.0.10")).is_ok());
        assert!(requirement.validate(Some("1.0.100")).is_ok());
        assert!(matches!(
            requirement.validate(Some("1.0.9")),
            Err(RuntimeVersionError::Unsupported { .. })
        ));
        assert!(matches!(
            requirement.validate(Some("1.0.10-beta.1")),
            Err(RuntimeVersionError::Unsupported { .. })
        ));
        assert!(matches!(
            requirement.validate(None),
            Err(RuntimeVersionError::Missing { .. })
        ));
        assert!(matches!(
            requirement.validate(Some("unknown")),
            Err(RuntimeVersionError::Malformed { .. })
        ));
    }
}
