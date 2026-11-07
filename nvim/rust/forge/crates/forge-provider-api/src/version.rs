//! Validate native runtime versions independently from compiled SDK versions.

use std::{
    fmt,
    path::{Path, PathBuf},
    time::Duration,
};

use crate::ProviderId;

/// An adapter-owned minimum version and the runtime selected for this initialization.
#[derive(Clone, Debug)]
pub struct RuntimeRequirement {
    provider: ProviderId,
    runtime: PathBuf,
    minimum: semver::Version,
}

impl RuntimeRequirement {
    /// Create a requirement after resolving the actual executable or native connection.
    pub fn new(provider: ProviderId, runtime: PathBuf, minimum: semver::Version) -> Self {
        Self {
            provider,
            runtime,
            minimum,
        }
    }

    /// Get the provider whose native runtime is being validated.
    pub fn provider(&self) -> &ProviderId {
        &self.provider
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
    pub fn probe_failed(&self, failure: RuntimeProbeFailure) -> RuntimeVersionError {
        RuntimeVersionError::Probe {
            requirement: Box::new(self.clone()),
            failure,
        }
    }

    /// Preserve a probe deadline and its original timer failure.
    pub fn probe_timed_out(&self, deadline: Duration) -> RuntimeVersionError {
        RuntimeVersionError::Probe {
            requirement: Box::new(self.clone()),
            failure: RuntimeProbeFailure::TimedOut { deadline },
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
                requirement: Box::new(self.clone()),
            })?;
        let version =
            semver::Version::parse(reported).map_err(|_| RuntimeVersionError::Malformed {
                requirement: Box::new(self.clone()),
            })?;
        if version < self.minimum {
            return Err(RuntimeVersionError::Unsupported {
                requirement: Box::new(self.clone()),
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
#[derive(Clone, Debug)]
pub enum RuntimeProbeFailure {
    /// The selected runtime denied access to its version endpoint.
    PermissionDenied,
    /// The selected runtime could not provide a version response.
    Unavailable,
    /// The selected runtime did not answer before the probe deadline.
    TimedOut { deadline: Duration },
}

impl fmt::Display for RuntimeProbeFailure {
    /// Render the probe classification while retaining its underlying source.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PermissionDenied => f.write_str("version probe permission was denied"),
            Self::Unavailable => f.write_str("runtime version probe was unavailable"),
            Self::TimedOut { deadline } => write!(f, "version probe exceeded {deadline:?}"),
        }
    }
}

impl std::error::Error for RuntimeProbeFailure {}

/// Distinguish failed, missing, malformed, and unsupported runtime versions for initialization.
#[derive(Clone, Debug)]
pub enum RuntimeVersionError {
    /// The runtime could not provide a version because the native probe failed.
    Probe {
        requirement: Box<RuntimeRequirement>,
        failure: RuntimeProbeFailure,
    },
    /// The native runtime supplied no usable version.
    Missing {
        requirement: Box<RuntimeRequirement>,
    },
    /// The reported value was not a semantic version.
    Malformed {
        requirement: Box<RuntimeRequirement>,
    },
    /// The selected runtime predates the adapter's required native API.
    Unsupported {
        requirement: Box<RuntimeRequirement>,
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
            Self::Malformed { requirement } => write!(
                f,
                "{requirement}; reported version is malformed. Install a supported runtime."
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

impl std::error::Error for RuntimeVersionError {}
