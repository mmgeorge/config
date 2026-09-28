use super::RustdocError;
use semver::Version;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::process::Command;

#[derive(Clone, Debug)]
/// Cargo installation used to acquire dependency sources without compiling them.
pub struct CargoSourceResolverConfig {
    pub cargo_executable: PathBuf,
    pub cargo_home: PathBuf,
}

impl CargoSourceResolverConfig {
    pub fn production() -> anyhow::Result<Self> {
        Ok(Self {
            cargo_executable: "cargo".into(),
            cargo_home: home::cargo_home()?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// A declaration in an exact version of a downloaded Cargo package.
pub struct RustdocSourceLocation {
    pub package: String,
    pub version: String,
    pub path: PathBuf,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Deserialize)]
/// Cargo's resolved package graph, including dependency aliases and library roots.
pub(crate) struct SourceGraph {
    pub packages: Vec<SourcePackage>,
    pub resolve: SourceResolution,
}

#[derive(Clone, Deserialize)]
pub(crate) struct SourcePackage {
    pub id: String,
    pub name: String,
    pub version: String,
    pub manifest_path: PathBuf,
    pub targets: Vec<SourceTarget>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct SourceTarget {
    pub name: String,
    pub kind: Vec<String>,
    pub src_path: PathBuf,
}

#[derive(Clone, Deserialize)]
pub(crate) struct SourceResolution {
    pub nodes: Vec<SourceNode>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct SourceNode {
    pub id: String,
    pub deps: Vec<SourceDependency>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct SourceDependency {
    pub name: String,
    pub pkg: String,
}

/// Downloads and resolves sources in an isolated manifest, leaving the project untouched.
pub struct CargoSourceResolver {
    config: CargoSourceResolverConfig,
}

impl CargoSourceResolver {
    pub fn new(config: CargoSourceResolverConfig) -> Self {
        Self { config }
    }

    pub(crate) async fn graph(
        &self,
        cache: &Path,
        package: &str,
        version: &str,
    ) -> Result<SourceGraph, RustdocError> {
        if package.is_empty()
            || !package.bytes().all(|character| {
                character.is_ascii_alphanumeric() || matches!(character, b'-' | b'_')
            })
        {
            return Err(RustdocError::Missing("invalid Cargo package name".into()));
        }
        Version::parse(version).map_err(|error| RustdocError::Missing(error.to_string()))?;
        let directory = cache.join(format!("{package}-{version}"));
        tokio::fs::create_dir_all(&directory)
            .await
            .map_err(unavailable)?;
        let manifest = directory.join("Cargo.toml");
        if !manifest.exists() {
            let mut dependency = toml::map::Map::new();
            dependency.insert("package".into(), toml::Value::String(package.into()));
            dependency.insert("version".into(), toml::Value::String(format!("={version}")));
            let mut dependencies = HashMap::new();
            dependencies.insert("dependency", dependency);
            let contents = format!(
                "[package]\nname = \"forge-source-query\"\nversion = \"0.0.0\"\nedition = \"2024\"\n[workspace]\n[lib]\npath = \"lib.rs\"\n{}",
                toml::to_string(&HashMap::from([("dependencies", dependencies)]))
                    .map_err(unavailable)?
            );
            tokio::fs::write(directory.join("lib.rs"), "")
                .await
                .map_err(unavailable)?;
            tokio::fs::write(&manifest, contents)
                .await
                .map_err(unavailable)?;
        }
        let mut command = Command::new(&self.config.cargo_executable);
        command
            .current_dir(&directory)
            .env("CARGO_HOME", &self.config.cargo_home)
            .args(["metadata", "--format-version", "1", "--manifest-path"])
            .arg(&manifest)
            .arg("--color")
            .arg("never")
            .kill_on_drop(true);
        if directory.join("Cargo.lock").exists() {
            command.arg("--locked");
        }
        let output = tokio::time::timeout(Duration::from_secs(120), command.output())
            .await
            .map_err(|_| {
                RustdocError::Unavailable(format!(
                    "Cargo source acquisition for `{package}` {version} exceeded 120 seconds"
                ))
            })?
            .map_err(unavailable)?;
        if !output.status.success() {
            return Err(RustdocError::Unavailable(format!(
                "Cargo source acquisition for `{package}` {version} failed: {}",
                String::from_utf8_lossy(&output.stderr).trim()
            )));
        }
        serde_json::from_slice(&output.stdout).map_err(unavailable)
    }
}

fn unavailable(error: impl std::fmt::Display) -> RustdocError {
    RustdocError::Unavailable(format!("could not acquire Cargo source: {error}"))
}
