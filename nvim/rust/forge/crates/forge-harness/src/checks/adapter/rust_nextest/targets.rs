use super::super::AdapterError;
use nextest_metadata::RustTestBinaryKind;
use serde::Deserialize;
use std::path::{Path, PathBuf};

/// Classify Cargo targets independently of library output crate types.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum TargetKind {
    Library,
    ProcMacro,
    Binary,
    Test,
    Example,
    Bench,
}

impl TargetKind {
    /// Normalize metadata's library crate types while excluding build scripts.
    pub(super) fn parse(kinds: &[String]) -> Result<Option<Self>, AdapterError> {
        for kind in kinds {
            let kind = match kind.as_str() {
                "lib" | "rlib" | "dylib" | "cdylib" | "staticlib" => Self::Library,
                "proc-macro" => Self::ProcMacro,
                "bin" => Self::Binary,
                "test" => Self::Test,
                "example" => Self::Example,
                "bench" => Self::Bench,
                "custom-build" => return Ok(None),
                _ => continue,
            };
            return Ok(Some(kind));
        }
        Err(AdapterError::Protocol(format!(
            "unknown Cargo target kinds: {kinds:?}"
        )))
    }

    /// Get nextest's test-binary kind after normalizing Cargo library outputs.
    pub(super) fn binary_kind(self) -> RustTestBinaryKind {
        match self {
            Self::Library => RustTestBinaryKind::LIB,
            Self::ProcMacro => RustTestBinaryKind::PROC_MACRO,
            Self::Binary => RustTestBinaryKind::BIN,
            Self::Test => RustTestBinaryKind::TEST,
            Self::Example => RustTestBinaryKind::EXAMPLE,
            Self::Bench => RustTestBinaryKind::BENCH,
        }
    }
}

/// Retain validated native arguments and their static target-selection meaning.
pub(super) struct BuildArguments {
    arguments: Vec<String>,
    targets: Vec<TargetRule>,
}

impl BuildArguments {
    /// Validate supported flags once and normalize Cargo job aliases for nextest.
    pub(super) fn parse(arguments: &[String]) -> Result<Self, AdapterError> {
        let mut native = Vec::new();
        let mut targets = Vec::new();
        let mut args = arguments.iter();
        while let Some(argument) = args.next() {
            let (flag, inline) = argument
                .split_once('=')
                .map_or((argument.as_str(), None), |(flag, value)| {
                    (flag, Some(value))
                });
            let takes_value = match flag {
                "--lib"
                | "--bins"
                | "--tests"
                | "--examples"
                | "--benches"
                | "--all-targets"
                | "--no-default-features"
                | "--offline"
                | "--frozen" => false,
                "--bin" | "--test" | "--example" | "--bench" | "--target" | "--target-dir"
                | "--build-jobs" | "--jobs" | "-j" => true,
                _ => {
                    return Err(AdapterError::Protocol(format!(
                        "nextest cargo_args flag is not supported or is adapter-owned: {argument}"
                    )));
                }
            };
            let value = if takes_value {
                let value = inline
                    .or_else(|| args.next().map(String::as_str))
                    .ok_or_else(|| AdapterError::Protocol(format!("missing value for {flag}")))?;
                if value.is_empty() || value.starts_with('-') || value.contains('\0') {
                    return Err(AdapterError::Protocol(format!("invalid value for {flag}")));
                }
                Some(value)
            } else {
                if inline.is_some() {
                    return Err(AdapterError::Protocol(format!(
                        "{flag} does not take a value"
                    )));
                }
                None
            };
            if let Some(rule) = TargetRule::parse(flag, value)? {
                targets.push(rule);
            }
            native.push(match flag {
                "--jobs" | "-j" => "--build-jobs".into(),
                _ => flag.into(),
            });
            if let Some(value) = value {
                native.push(value.into());
            }
        }
        if targets
            .iter()
            .any(|rule| matches!(rule, TargetRule::AllTargets))
        {
            targets = vec![TargetRule::AllTargets];
        }
        Ok(Self {
            arguments: native,
            targets,
        })
    }

    /// Get validated arguments with nextest's native option spellings.
    pub(super) fn arguments(&self) -> &[String] {
        &self.arguments
    }

    /// Indicate when target filtering needs a flag absent from Cargo metadata.
    pub(super) fn requires_bench_manifest(&self) -> bool {
        self.targets
            .iter()
            .any(|rule| matches!(rule, TargetRule::Benched | TargetRule::AllTargets))
    }

    /// Apply Cargo's union of explicit target rules, or its default tested-target set.
    pub(super) fn target_includes(
        &self,
        kind: TargetKind,
        name: &str,
        tested: bool,
        benched: bool,
    ) -> bool {
        if self.targets.is_empty() {
            return tested;
        }
        self.targets.iter().any(|rule| match rule {
            TargetRule::Library => matches!(kind, TargetKind::Library | TargetKind::ProcMacro),
            TargetRule::Kind(expected) => kind == *expected,
            TargetRule::Named {
                kind: expected,
                pattern,
            } => kind == *expected && pattern.matches(name),
            TargetRule::Tested => tested,
            TargetRule::Benched => benched,
            TargetRule::AllTargets => {
                tested
                    || benched
                    || matches!(
                        kind,
                        TargetKind::Library
                            | TargetKind::ProcMacro
                            | TargetKind::Binary
                            | TargetKind::Example
                    )
            }
        })
    }
}

enum TargetRule {
    Library,
    Kind(TargetKind),
    Named {
        kind: TargetKind,
        pattern: glob::Pattern,
    },
    Tested,
    Benched,
    AllTargets,
}

impl TargetRule {
    /// Parse only the target-selecting flags from the validated option vocabulary.
    fn parse(flag: &str, value: Option<&str>) -> Result<Option<Self>, AdapterError> {
        let rule = match flag {
            "--lib" => Self::Library,
            "--bins" => Self::Kind(TargetKind::Binary),
            "--examples" => Self::Kind(TargetKind::Example),
            "--tests" => Self::Tested,
            "--benches" => Self::Benched,
            "--all-targets" => Self::AllTargets,
            "--bin" | "--test" | "--example" | "--bench" => {
                let kind = match flag {
                    "--bin" => TargetKind::Binary,
                    "--test" => TargetKind::Test,
                    "--example" => TargetKind::Example,
                    "--bench" => TargetKind::Bench,
                    _ => unreachable!(),
                };
                let value = value.expect("named target flag was validated as requiring a value");
                let pattern = glob::Pattern::new(value).map_err(|error| {
                    AdapterError::Protocol(format!(
                        "invalid {flag} target pattern {value:?}: {error}"
                    ))
                })?;
                Self::Named { kind, pattern }
            }
            _ => return Ok(None),
        };
        Ok(Some(rule))
    }
}

/// Read only per-target bench overrides missing from Cargo metadata's target records.
#[derive(Default, Deserialize)]
pub(super) struct TargetManifest {
    lib: Option<ManifestTarget>,
    #[serde(default)]
    bin: Vec<ManifestTarget>,
    #[serde(default)]
    test: Vec<ManifestTarget>,
    #[serde(default)]
    example: Vec<ManifestTarget>,
    #[serde(default)]
    bench: Vec<ManifestTarget>,
}

impl TargetManifest {
    /// Parse the package manifest without resolving dependencies or running build scripts.
    pub(super) fn parse(source: &str, path: &Path) -> Result<Self, AdapterError> {
        toml::from_str(source)
            .map_err(|error| AdapterError::Protocol(format!("{}: {error}", path.display())))
    }

    /// Apply an explicit bench flag by target name or source path, then Cargo's kind default.
    pub(super) fn bench_enabled(
        &self,
        kind: TargetKind,
        name: &str,
        source: &Path,
        directory: &Path,
    ) -> bool {
        let declarations: &[ManifestTarget] = match kind {
            TargetKind::Library | TargetKind::ProcMacro => self.lib.as_slice(),
            TargetKind::Binary => &self.bin,
            TargetKind::Test => &self.test,
            TargetKind::Example => &self.example,
            TargetKind::Bench => &self.bench,
        };
        declarations
            .iter()
            .find(|target| {
                matches!(kind, TargetKind::Library | TargetKind::ProcMacro)
                    || target.name.as_deref() == Some(name)
                    || target.name.is_none()
                        && target.path.as_ref().is_some_and(|path| {
                            dunce::simplified(&directory.join(path)) == dunce::simplified(source)
                        })
            })
            .and_then(|target| target.bench)
            .unwrap_or(!matches!(kind, TargetKind::Test | TargetKind::Example))
    }
}

#[derive(Deserialize)]
struct ManifestTarget {
    name: Option<String>,
    path: Option<PathBuf>,
    bench: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Distinguish forced targets, manifest-controlled groups, and Cargo glob matching.
    #[test]
    fn explicit_targets_override_test_flags_without_widening_other_kinds() {
        let parse = |args: &[&str]| {
            BuildArguments::parse(&args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>())
                .unwrap()
        };
        let named = parse(&["--bin", "tool-[ab]", "--example=demo"]);
        assert!(named.target_includes(TargetKind::Binary, "tool-a", false, false));
        assert!(!named.target_includes(TargetKind::Binary, "tool-c", true, true));
        assert!(named.target_includes(TargetKind::Example, "demo", false, false));
        assert!(!named.target_includes(TargetKind::Test, "demo", true, true));
        let tests = parse(&["--tests"]);
        assert!(tests.target_includes(TargetKind::Example, "demo", true, false));
        assert!(!tests.target_includes(TargetKind::Test, "skipped", false, true));
        let benches = parse(&["--benches"]);
        assert!(benches.target_includes(TargetKind::Binary, "tool", false, true));
        assert!(!benches.target_includes(TargetKind::Bench, "off", true, false));
        let all = parse(&["--all-targets", "--test", "off"]);
        assert!(all.target_includes(TargetKind::Library, "lib", false, false));
        assert!(all.target_includes(TargetKind::Example, "demo", false, false));
        assert!(!all.target_includes(TargetKind::Test, "off", false, false));
        assert_eq!(
            parse(&["--jobs=2", "-j", "3"]).arguments(),
            ["--build-jobs", "2", "--build-jobs", "3"]
        );
        assert!(BuildArguments::parse(&["--bin=[".into()]).is_err());
    }

    /// Use package bench overrides without treating integration tests as benches by default.
    #[test]
    fn bench_flags_follow_target_declarations_and_kind_defaults() {
        let manifest = TargetManifest::parse(
            r#"
[lib]
bench = false
[[bin]]
path = "src/main.rs"
bench = false
[[test]]
name = "integration"
bench = true
[[bench]]
name = "disabled"
bench = false
"#,
            Path::new("Cargo.toml"),
        )
        .unwrap();
        let enabled = |kind, name, source| {
            manifest.bench_enabled(kind, name, Path::new(source), Path::new("repo"))
        };
        assert!(!enabled(TargetKind::Library, "lib", "repo/src/lib.rs"));
        assert!(!enabled(TargetKind::Binary, "binary", "repo/src/main.rs"));
        assert!(enabled(
            TargetKind::Test,
            "integration",
            "repo/tests/integration.rs"
        ));
        assert!(!enabled(
            TargetKind::Bench,
            "disabled",
            "repo/benches/disabled.rs"
        ));
        assert!(enabled(TargetKind::Bench, "auto", "repo/benches/auto.rs"));
        assert!(!enabled(
            TargetKind::Example,
            "demo",
            "repo/examples/demo.rs"
        ));
    }
}
