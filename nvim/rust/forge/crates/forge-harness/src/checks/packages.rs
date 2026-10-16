use anyhow::{Context, Result, bail, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
};

#[derive(
    Clone, Copy, Debug, Deserialize, Eq, JsonSchema, Ord, PartialEq, PartialOrd, Serialize,
)]
#[serde(rename_all = "snake_case")]
/// Selects the authoritative package membership rules for a check.
pub enum PackageManager {
    /// Uses Cargo workspace metadata and target definitions.
    Cargo,
    /// Uses package.json workspace membership.
    Npm,
    /// Uses pnpm-workspace.yaml package membership.
    Pnpm,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies a discovered package independently of a command invocation.
pub struct PackageIdentity {
    /// Declared package name, when present.
    pub name: Option<String>,
    /// Workspace-relative directory, with the root represented by a dot.
    pub directory: String,
    /// Workspace-relative package manifest.
    pub manifest: String,
}

#[derive(Clone, Debug, Deserialize)]
/// Describes a Cargo target used to resolve a planned test to an executable.
pub(super) struct CargoTarget {
    pub(super) name: String,
    pub(super) kind: Vec<String>,
    pub(super) src_path: String,
    #[serde(default)]
    pub(super) test: bool,
    #[serde(skip)]
    pub(super) harness: bool,
}

#[derive(Clone, Debug)]
/// Retains package identity and Cargo targets discovered together.
pub(super) struct Package {
    pub(super) identity: PackageIdentity,
    pub(super) targets: Vec<CargoTarget>,
}

#[derive(Clone, Debug)]
/// Owns one workspace package inventory for invocation resolution.
pub(super) struct PackageCatalog {
    root: PathBuf,
    manifest_name: &'static str,
    pub(super) inputs: BTreeMap<String, String>,
    pub(super) packages: Vec<Package>,
}

impl PackageCatalog {
    /// Discovers only workspace members, without installing dependencies or executing tests.
    pub(super) async fn discover(
        workspace: &Path,
        directory: &str,
        manager: PackageManager,
        toolchain: Option<&str>,
    ) -> Result<Self> {
        let root = workspace.canonicalize()?;
        let directory = root
            .join(super::config::relative_path(directory)?)
            .canonicalize()?;
        ensure!(
            directory.starts_with(&root),
            "package discovery directory escapes the workspace"
        );
        let packages = match manager {
            PackageManager::Cargo => {
                let mut args = toolchain.into_iter().map(str::to_owned).collect::<Vec<_>>();
                args.extend([
                    "metadata", "--format-version", "1", "--no-deps", "--offline", "--locked",
                ].map(str::to_owned));
                let output = super::invocation::capture(
                    "cargo",
                    &args,
                    &directory,
                    None,
                    None,
                    30,
                )
                .await?;
                #[derive(Deserialize)]
                struct Metadata {
                    packages: Vec<CargoPackage>,
                    workspace_members: Vec<String>,
                }
                #[derive(Deserialize)]
                struct CargoPackage {
                    id: String,
                    name: String,
                    manifest_path: String,
                    targets: Vec<CargoTarget>,
                }
                let metadata: Metadata =
                    serde_json::from_slice(&output).context("invalid Cargo package metadata")?;
                let members: BTreeSet<_> = metadata.workspace_members.into_iter().collect();
                let mut packages = Vec::new();
                for package in metadata
                    .packages
                    .into_iter()
                    .filter(|package| members.contains(&package.id))
                {
                    let manifest = PathBuf::from(package.manifest_path).canonicalize()?;
                    ensure!(
                        manifest.starts_with(&root),
                        "Cargo member is outside the Harness workspace: {}",
                        manifest.display()
                    );
                    let mut targets = package.targets;
                    let manifest_source = read(&manifest)?;
                    let manifest_settings: toml::Value =
                        toml::from_str(std::str::from_utf8(&manifest_source)?)?;
                    for target in &mut targets {
                        target.src_path = relative(&root, Path::new(&target.src_path))?;
                        let kind = if target
                            .kind
                            .iter()
                            .any(|kind| matches!(kind.as_str(), "lib" | "proc-macro"))
                        {
                            "lib"
                        } else if target.kind.iter().any(|kind| kind == "bin") {
                            "bin"
                        } else {
                            "test"
                        };
                        let settings = if kind == "lib" {
                            manifest_settings.get(kind)
                        } else {
                            manifest_settings
                                .get(kind)
                                .and_then(toml::Value::as_array)
                                .and_then(|targets| {
                                    targets.iter().find(|settings| {
                                        settings.get("name").and_then(toml::Value::as_str)
                                            == Some(target.name.as_str())
                                    })
                                })
                        };
                        target.harness = settings
                            .and_then(|settings| settings.get("harness"))
                            .and_then(toml::Value::as_bool)
                            .unwrap_or(true);
                    }
                    packages.push(Package {
                        identity: PackageIdentity {
                            name: Some(package.name),
                            directory: relative(&root, manifest.parent().unwrap())?,
                            manifest: relative(&root, &manifest)?,
                        },
                        targets,
                    });
                }
                packages
            }
            PackageManager::Npm | PackageManager::Pnpm => {
                let root_manifest = directory.join("package.json");
                let document: serde_json::Value = serde_json::from_slice(&read(&root_manifest)?)
                    .context("invalid package.json")?;
                let patterns: Vec<String> = if manager == PackageManager::Pnpm
                    && directory.join("pnpm-workspace.yaml").is_file()
                {
                    #[derive(Deserialize)]
                    struct Workspace {
                        #[serde(default)]
                        packages: Vec<String>,
                    }
                    serde_yaml_ng::from_slice::<Workspace>(&read(
                        &directory.join("pnpm-workspace.yaml"),
                    )?)?
                    .packages
                } else if manager == PackageManager::Npm {
                    match document.get("workspaces") {
                        Some(serde_json::Value::Array(values)) => strings(values)?,
                        Some(serde_json::Value::Object(values)) => strings(
                            values
                                .get("packages")
                                .and_then(|value| value.as_array())
                                .context("workspaces.packages must be an array")?,
                        )?,
                        Some(_) => {
                            bail!("workspaces must be an array or an object containing packages")
                        }
                        None => Vec::new(),
                    }
                } else {
                    Vec::new()
                };
                for pattern in &patterns {
                    let pattern = pattern.strip_prefix('!').unwrap_or(pattern);
                    super::config::relative_path(pattern)?;
                    ensure!(
                        !pattern.contains(['[', ']', '{', '}']),
                        "unsupported workspace glob: {pattern}"
                    );
                }
                let mut manifests = vec![root_manifest];
                if !patterns.is_empty() {
                    let mut candidates = Vec::new();
                    scan(&directory, &mut candidates)?;
                    for manifest in candidates {
                        let path = relative(&directory, manifest.parent().unwrap())?;
                        let included = patterns
                            .iter()
                            .filter(|pattern| !pattern.starts_with('!'))
                            .any(|pattern| {
                                super::applicability::path_matches(
                                    pattern.trim_end_matches('/'),
                                    &path,
                                )
                            });
                        let excluded = patterns
                            .iter()
                            .filter_map(|pattern| pattern.strip_prefix('!'))
                            .any(|pattern| {
                                super::applicability::path_matches(
                                    pattern.trim_end_matches('/'),
                                    &path,
                                )
                            });
                        if path != "." && included && !excluded {
                            manifests.push(manifest);
                        }
                    }
                }
                let mut packages = Vec::new();
                for manifest in manifests {
                    let document: serde_json::Value = serde_json::from_slice(&read(&manifest)?)
                        .with_context(|| format!("invalid {}", manifest.display()))?;
                    let name = document
                        .get("name")
                        .map(|value| value.as_str().context("package name must be a string"))
                        .transpose()?
                        .map(str::to_owned);
                    packages.push(Package {
                        identity: PackageIdentity {
                            name,
                            directory: relative(&root, manifest.parent().unwrap())?,
                            manifest: relative(&root, &manifest)?,
                        },
                        targets: Vec::new(),
                    });
                }
                packages
            }
        };
        ensure!(
            !packages.is_empty(),
            "package inventory must contain workspace packages"
        );
        for package in &packages {
            ensure!(
                package
                    .identity
                    .name
                    .as_ref()
                    .is_none_or(|name| !name.is_empty()
                        && name.len() <= 256
                        && name
                            .chars()
                            .all(|character| character.is_ascii_alphanumeric()
                                || "@/_.-".contains(character))),
                "invalid package selector name in {}",
                package.identity.manifest
            );
        }
        let mut inputs = BTreeMap::new();
        for manifest in packages
            .iter()
            .map(|package| root.join(&package.identity.manifest))
            .chain(
                [
                    "Cargo.toml",
                    "Cargo.lock",
                    "package.json",
                    "package-lock.json",
                    "pnpm-workspace.yaml",
                    "pnpm-lock.yaml",
                ]
                .map(|name| directory.join(name)),
            )
        {
            if manifest.is_file() {
                inputs.insert(
                    relative(&root, &manifest)?,
                    crate::plan::digest(&read(&manifest)?),
                );
            }
        }
        Ok(Self {
            root,
            manifest_name: if manager == PackageManager::Cargo {
                "Cargo.toml"
            } else {
                "package.json"
            },
            inputs,
            packages,
        })
    }

    /// Selects the deepest discovered owner, including paths whose files were deleted.
    pub(super) fn owner(&self, file: &str) -> Result<&Package> {
        let file = super::config::relative_path(file)?;
        let owner = self.packages.iter().filter(|package| package.identity.directory == "." || file.starts_with(&package.identity.directory))
            .max_by_key(|package| package.identity.directory.len())
            .with_context(|| format!("No discovered package owns {}. Configure a workspace-wide check for shared files or removed packages.", file.display()))?;
        for directory in file.parent().into_iter().flat_map(Path::ancestors) {
            let relative_directory = if directory.as_os_str().is_empty() {
                ".".into()
            } else {
                directory.to_string_lossy().replace('\\', "/")
            };
            if relative_directory == owner.identity.directory {
                break;
            }
            ensure!(
                !self.root.join(directory).join(self.manifest_name).is_file(),
                "{} belongs to an excluded or independent package",
                file.display()
            );
        }
        Ok(owner)
    }
}

fn strings(values: &[serde_json::Value]) -> Result<Vec<String>> {
    values
        .iter()
        .map(|value| {
            value
                .as_str()
                .map(str::to_owned)
                .context("workspace patterns must be strings")
        })
        .collect()
}

fn read(path: &Path) -> Result<Vec<u8>> {
    let metadata =
        std::fs::symlink_metadata(path).with_context(|| format!("read {}", path.display()))?;
    ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "manifest must be a regular file: {}",
        path.display()
    );
    Ok(std::fs::read(path)?)
}

fn relative(root: &Path, path: &Path) -> Result<String> {
    let root = dunce::simplified(root);
    let path = dunce::simplified(path);
    let path = path.strip_prefix(root).with_context(|| {
        format!(
            "package path is outside {}: {}",
            root.display(),
            path.display()
        )
    })?;
    let path = path.to_string_lossy().replace('\\', "/");
    Ok(if path.is_empty() { ".".into() } else { path })
}

fn scan(
    directory: &Path,
    manifests: &mut Vec<PathBuf>,
) -> Result<()> {
    for entry in std::fs::read_dir(directory)? {
        let entry = entry?;
        if entry.file_type()?.is_dir()
            && !matches!(
                entry.file_name().to_str(),
                Some("node_modules" | ".git" | "target" | ".forge")
            )
        {
            scan(&entry.path(), manifests)?;
        } else if entry.file_name() == "package.json" && entry.file_type()?.is_file() {
            manifests.push(entry.path());
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_reads_preserve_complete_large_content() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("package.json");
        let source = serde_json::to_vec(&serde_json::json!({
            "name":"fixture","description":"λ".repeat(1024 * 1024)
        }))?;
        std::fs::write(&path, &source)?;
        assert_eq!(read(&path)?, source);
        assert!(read(directory.path()).is_err());
        Ok(())
    }
}
