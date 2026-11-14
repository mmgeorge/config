use super::super::AdapterError;
use semver::Version;

/// Lowest nextest version validated against the committed native protocol recordings.
pub(super) const MINIMUM_VERSION: Version = Version::new(0, 9, 151);
/// Native reporter whose additional metadata preserves case identities.
pub(super) const MESSAGE_FORMAT: &str = "libtest-json-plus";
/// Explicit native event schema required for translation.
pub(super) const MESSAGE_FORMAT_VERSION: &str = "0.1";

/// Identify the installed runner after checking the supported minimum.
pub(super) struct NextestVersion(Version);

impl NextestVersion {
    /// Parse the runner's own version output and reject unsupported or ambiguous identities.
    pub(super) fn parse(output: &str) -> Result<Self, AdapterError> {
        let version = output.lines().next()
            .and_then(|line| line.strip_prefix("cargo-nextest "))
            .and_then(|rest| rest.split_whitespace().next())
            .and_then(|text| Version::parse(text).ok())
            .ok_or_else(|| AdapterError::Selection(format!(
                "cannot identify cargo-nextest version from {output:?}; requires cargo-nextest {MINIMUM_VERSION} or later with {MESSAGE_FORMAT} {MESSAGE_FORMAT_VERSION}; install with `cargo install cargo-nextest --locked`"
            )))?;
        if version < MINIMUM_VERSION {
            return Err(AdapterError::Selection(format!(
                "cargo-nextest {version} is unsupported; requires {MINIMUM_VERSION} or later with {MESSAGE_FORMAT} {MESSAGE_FORMAT_VERSION}; install with `cargo install cargo-nextest --locked`"
            )));
        }
        Ok(Self(version))
    }

    /// Describe the actual runner and required format beside a native failure.
    pub(super) fn operation(&self, operation: &str) -> String {
        format!(
            "cargo nextest {operation} (runner {}; required {MESSAGE_FORMAT} {MESSAGE_FORMAT_VERSION})",
            self.0
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Pin support to observed native output and name rejected versions in actionable errors.
    #[test]
    fn recorded_version_and_unsupported_versions() {
        let version = NextestVersion::parse(include_str!("fixtures/version.stdout")).unwrap();
        assert!(version.operation("run").contains("0.9.151"));
        assert!(version.operation("run").contains("libtest-json-plus 0.1"));
        for raw in [
            "cargo-nextest 0.9.150",
            "cargo-nextest 0.9.151-beta.1",
            "unexpected",
        ] {
            let error = NextestVersion::parse(raw).err().unwrap().to_string();
            assert!(error.contains("0.9.151"));
            assert!(error.contains("cargo install cargo-nextest --locked"));
        }
    }
}
