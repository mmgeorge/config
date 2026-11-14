use super::{
    super::{AdapterError, TestItem, TestItemId},
    targets::{BuildArguments, TargetKind, TargetManifest},
};
use nextest_metadata::RustBinaryId;
use serde::Deserialize;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

/// Retain Cargo's workspace membership and source roots for one operation.
pub(super) struct WorkspaceModel {
    pub root: PathBuf,
    pub packages: Vec<Package>,
}

/// Identify a workspace package independently of target names.
pub(super) struct Package {
    pub id: String,
    pub name: String,
    pub directory: PathBuf,
    pub targets: Vec<Target>,
}

/// Identify one test-capable Cargo target and its source entry point.
pub(super) struct Target {
    pub binary: String,
    pub name: String,
    pub source: PathBuf,
}

#[derive(Deserialize)]
struct Metadata {
    workspace_root: PathBuf,
    workspace_members: Vec<String>,
    packages: Vec<RawPackage>,
}

#[derive(Deserialize)]
struct RawPackage {
    id: String,
    name: String,
    manifest_path: PathBuf,
    targets: Vec<RawTarget>,
}

#[derive(Deserialize)]
struct RawTarget {
    name: String,
    kind: Vec<String>,
    src_path: PathBuf,
    test: bool,
}

impl WorkspaceModel {
    /// Parse offline metadata and preserve only actual workspace members.
    pub(super) fn parse(
        json: &str,
        build: &BuildArguments,
        read: &impl Fn(&Path) -> Result<String, std::io::Error>,
    ) -> Result<Self, AdapterError> {
        let metadata: Metadata = serde_json::from_str(json)
            .map_err(|error| AdapterError::Protocol(format!("Cargo metadata: {error}")))?;
        let mut packages = Vec::new();
        for package in metadata.packages {
            if !metadata.workspace_members.contains(&package.id) {
                continue;
            }
            let directory = package
                .manifest_path
                .parent()
                .ok_or_else(|| {
                    AdapterError::Protocol("Cargo package manifest has no parent".into())
                })?
                .to_owned();
            let manifest = if build.requires_bench_manifest() {
                let source = read(&package.manifest_path).map_err(AdapterError::Storage)?;
                Some(TargetManifest::parse(&source, &package.manifest_path)?)
            } else {
                None
            };
            let mut targets = Vec::new();
            for target in package.targets {
                let Some(kind) = TargetKind::parse(&target.kind)? else {
                    continue;
                };
                let benched = manifest.as_ref().is_some_and(|manifest| {
                    manifest.bench_enabled(kind, &target.name, &target.src_path, &directory)
                });
                if !build.target_includes(kind, &target.name, target.test, benched) {
                    continue;
                }
                let binary =
                    RustBinaryId::from_parts(&package.name, &kind.binary_kind(), &target.name);
                targets.push(Target {
                    binary: binary.as_str().to_owned(),
                    name: target.name,
                    source: target.src_path,
                });
            }
            packages.push(Package {
                id: package.id,
                name: package.name,
                directory,
                targets,
            });
        }
        Ok(Self {
            root: metadata.workspace_root,
            packages,
        })
    }

    /// Assemble an unambiguous package, binary, module, and case hierarchy.
    pub(super) fn tree(
        &self,
        cases: impl IntoIterator<Item = Case>,
    ) -> Result<Vec<TestItem>, AdapterError> {
        let mut items = BTreeMap::new();
        for case in cases {
            let package = self
                .packages
                .iter()
                .find(|package| package.id == case.package)
                .ok_or_else(|| {
                    AdapterError::Protocol(format!("unknown Cargo package {}", case.package))
                })?;
            let package_id = item_id(format!("package:{}", package.name))?;
            insert_group(&mut items, package_id.clone(), None, package.name.clone());
            let binary_id = item_id(case.binary.clone())?;
            insert_group(&mut items, binary_id.clone(), Some(package_id), case.label);
            let mut parent = binary_id;
            let segments: Vec<_> = case.name.split("::").collect();
            for end in 1..segments.len() {
                let id = item_id(format!("{} {}::", case.binary, segments[..end].join("::")))?;
                insert_group(
                    &mut items,
                    id.clone(),
                    Some(parent),
                    segments[end - 1].to_owned(),
                );
                parent = id;
            }
            let id = item_id(format!("{} {}", case.binary, case.name))?;
            let item = TestItem {
                id: id.clone(),
                parent: Some(parent),
                label: segments.last().unwrap_or(&"").to_string(),
                case: true,
                name: Some(case.name),
                path: case.path,
                line: case.line,
            };
            if items.insert(id, item).is_some() {
                return Err(AdapterError::Protocol("duplicate nextest case".into()));
            }
        }
        Ok(items.into_values().collect())
    }
}

/// Preserve exact case identity with an optional statically discovered location.
pub(super) struct Case {
    pub package: String,
    pub binary: String,
    pub label: String,
    pub name: String,
    pub path: Option<String>,
    pub line: Option<u32>,
}

/// Validate nextest's IDs through the shared protocol boundary.
pub(super) fn item_id(value: String) -> Result<TestItemId, AdapterError> {
    value.try_into()
}

/// Insert shared ancestors once while preserving their original identity.
fn insert_group(
    items: &mut BTreeMap<TestItemId, TestItem>,
    id: TestItemId,
    parent: Option<TestItemId>,
    label: String,
) {
    items.entry(id.clone()).or_insert(TestItem {
        id,
        parent,
        label,
        case: false,
        name: None,
        path: None,
        line: None,
    });
}

/// Convert an absolute source path to the shared repository-relative wire format.
pub(super) fn relative(path: &Path, repository: &Path) -> Option<String> {
    let path = dunce::canonicalize(path).unwrap_or_else(|_| path.to_owned());
    let repository = dunce::simplified(repository);
    dunce::simplified(&path)
        .strip_prefix(repository)
        .ok()?
        .to_str()
        .map(|path| path.replace('\\', "/"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve the library's nextest ID for every Cargo library output kind.
    #[test]
    fn library_output_kinds_share_one_binary_identity() {
        let build = BuildArguments::parse(&["--lib".into()]).unwrap();
        for kind in ["lib", "rlib", "dylib", "cdylib", "staticlib", "proc-macro"] {
            let json = serde_json::json!({
                "workspace_root":"repo", "workspace_members":["id"],
                "packages":[{"id":"id", "name":"package", "manifest_path":"repo/Cargo.toml",
                    "targets":[{"name":"custom_name", "kind":[kind], "src_path":"repo/src/lib.rs", "test":false}]}]
            });
            let workspace = WorkspaceModel::parse(&json.to_string(), &build, &|_| {
                panic!("no manifest read needed")
            })
            .unwrap();
            assert_eq!(workspace.packages[0].targets.len(), 1, "{kind}");
            assert_eq!(workspace.packages[0].targets[0].binary, "package", "{kind}");
        }
    }
}
