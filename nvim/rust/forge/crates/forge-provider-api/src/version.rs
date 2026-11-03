//! Validate native runtime versions independently from compiled SDK versions.

use std::{
    fmt,
    path::{Path, PathBuf},
    time::Duration,
};

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

    /// Get the provider whose native runtime is being validated.
    pub fn provider(&self) -> ProviderKind {
        self.provider
    }

    /// Get the selected executable or native connection identity.
    pub fn runtime(&self) -> &Path {
        &self.runtime
    }

    /// Get the minimum supported native runtime version.
    pub fn minimum(&self) -> &semver::Version {
        &self.minimum
    }

    /// Preserve a failed native probe and its selected runtime requirement.
    pub fn probe_failed(
        &self,
        source: impl Into<Box<dyn std::error::Error + Send + Sync>>,
    ) -> RuntimeVersionError {
        RuntimeVersionError::Probe {
            requirement: self.clone(),
            failure: RuntimeProbeFailure::Request {
                source: source.into(),
            },
        }
    }

    /// Preserve a probe deadline and its original timer failure.
    pub fn probe_timed_out(
        &self,
        deadline: Duration,
        source: impl std::error::Error + Send + Sync + 'static,
    ) -> RuntimeVersionError {
        RuntimeVersionError::Probe {
            requirement: self.clone(),
            failure: RuntimeProbeFailure::TimedOut {
                deadline,
                source: Box::new(source),
            },
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

/// Distinguish failed requests from expired version-probe deadlines.
#[derive(Debug)]
pub enum RuntimeProbeFailure {
    /// The native transport or remote runtime failed the probe.
    Request {
        source: Box<dyn std::error::Error + Send + Sync>,
    },
    /// The native runtime did not answer before the probe deadline.
    TimedOut {
        deadline: Duration,
        source: Box<dyn std::error::Error + Send + Sync>,
    },
}

impl fmt::Display for RuntimeProbeFailure {
    /// Render the probe classification while retaining its underlying source.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Request { source } => write!(f, "version probe failed: {source}"),
            Self::TimedOut {
                deadline,
                source: _,
            } => write!(f, "version probe exceeded {deadline:?}"),
        }
    }
}

impl std::error::Error for RuntimeProbeFailure {
    /// Preserve the concrete external transport or timer error at the adapter boundary.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Request { source }
            | Self::TimedOut {
                source,
                deadline: _,
            } => Some(source.as_ref()),
        }
    }
}

/// Distinguish failed, missing, malformed, and unsupported runtime versions for initialization.
#[derive(Debug)]
pub enum RuntimeVersionError {
    /// The runtime could not provide a version because the native probe failed.
    Probe {
        requirement: RuntimeRequirement,
        failure: RuntimeProbeFailure,
    },
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
            Self::Probe {
                requirement,
                failure,
            } => write!(
                f,
                "{requirement}; {failure}. Check the selected runtime and install a supported version."
            ),
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
    /// Preserve the native probe or parser cause without formatting it into a string.
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Probe { failure, .. } => Some(failure),
            Self::Malformed { source, .. } => Some(source),
            Self::Missing { .. } | Self::Unsupported { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;

    /// Retain the concrete failed-probe cause together with the adapter's selected requirement.
    #[test]
    fn failed_probe_preserves_source_and_runtime_requirement() {
        let requirement = RuntimeRequirement::new(
            ProviderKind::Copilot,
            "selected-copilot".into(),
            semver::Version::new(1, 0, 103),
        );
        let error = requirement.probe_failed(std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            "fixture native status failure",
        ));
        let RuntimeVersionError::Probe {
            requirement,
            failure,
        } = &error
        else {
            panic!("request failure lost probe classification");
        };
        assert_eq!(requirement.provider(), ProviderKind::Copilot);
        assert_eq!(requirement.runtime(), Path::new("selected-copilot"));
        assert_eq!(requirement.minimum(), &semver::Version::new(1, 0, 103));
        assert!(matches!(failure, RuntimeProbeFailure::Request { .. }));
        let source = error
            .source()
            .unwrap()
            .source()
            .unwrap()
            .downcast_ref::<std::io::Error>()
            .expect("probe failure erased the native source");
        assert_eq!(source.kind(), std::io::ErrorKind::PermissionDenied);
        let diagnostic = error.to_string();
        assert!(
            diagnostic.contains("selected-copilot")
                && diagnostic.contains("1.0.103")
                && diagnostic.contains("fixture native status failure")
        );
    }

    /// Keep an expired probe distinct from a native request failure and preserve the timer source.
    #[tokio::test]
    async fn timed_out_probe_preserves_deadline_and_runtime_requirement() {
        let requirement = RuntimeRequirement::new(
            ProviderKind::Copilot,
            "selected-copilot".into(),
            semver::Version::new(1, 0, 103),
        );
        let deadline = Duration::from_millis(1);
        let elapsed = tokio::time::timeout(deadline, std::future::pending::<()>())
            .await
            .unwrap_err();
        let error = requirement.probe_timed_out(deadline, elapsed);
        assert!(
            matches!(&error, RuntimeVersionError::Probe { requirement: actual, failure: RuntimeProbeFailure::TimedOut { deadline: actual_deadline, .. }} if actual.runtime() == requirement.runtime() && *actual_deadline == deadline)
        );
        assert!(
            error
                .source()
                .unwrap()
                .source()
                .unwrap()
                .is::<tokio::time::error::Elapsed>()
        );
    }

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
