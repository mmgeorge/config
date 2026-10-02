//! Selects cached Cargo sources for navigation without resolving the project graph.

use super::*;
use semver::{Version, VersionReq};

/// Retains source candidates and reached manifests for one immutable review snapshot.
pub(super) struct RustSourceCache {
    root: PathBuf,
    registry: Option<BTreeMap<String, Vec<(Version, PathBuf)>>>,
    manifest: HashMap<PathBuf, Arc<toml::Value>>,
    /// Requests Cargo acquisition only after a reached dependency lacks cached sources.
    pub(super) requires_fetch: bool,
}

impl RustSourceCache {
    /// Use Cargo's existing registry directories without starting a Cargo process.
    pub(super) fn new(root: PathBuf) -> Self {
        Self {
            root,
            registry: None,
            manifest: HashMap::new(),
            requires_fetch: false,
        }
    }

    /// Resolve one manifest alias, then register its library for lazy module indexing.
    pub(super) fn dependency(
        &mut self,
        resolver: &mut DeclarationResolver,
        package: &str,
        alias: &str,
    ) -> Result<Option<String>> {
        let Some(path) = resolver
            .package
            .get(package)
            .and_then(|state| state.source_manifest.clone())
        else {
            return Ok(None);
        };
        let manifest = self.read_manifest(resolver, &path)?;
        let Some(mut specification) = dependency_specification(&manifest, alias).cloned() else {
            return Ok(None);
        };
        let mut directory = path.parent().unwrap().to_path_buf();
        if specification
            .get("workspace")
            .and_then(toml::Value::as_bool)
            == Some(true)
        {
            let mut inherited = None;
            for parent in directory.ancestors() {
                let workspace_path = parent.join("Cargo.toml");
                let Some(text) = resolver.read(&workspace_path) else {
                    continue;
                };
                let workspace: toml::Value = toml::from_str(&text)?;
                if let Some(dependency) = workspace
                    .get("workspace")
                    .and_then(|workspace| workspace.get("dependencies"))
                    .and_then(toml::Value::as_table)
                    .and_then(|table| {
                        table
                            .iter()
                            .find(|(name, _)| name.replace('-', "_") == alias)
                    })
                    .map(|(_, specification)| specification.clone())
                {
                    inherited = Some((parent.to_path_buf(), dependency));
                    break;
                }
            }
            let Some((parent, dependency)) = inherited else {
                return Ok(None);
            };
            directory = parent;
            specification = dependency;
        }
        let target = if let Some(path) = specification.get("path").and_then(toml::Value::as_str) {
            normalize(&directory.join(path).join("Cargo.toml"))
        } else {
            if specification.get("git").is_some() || specification.get("registry").is_some() {
                self.requires_fetch = true;
                return Ok(None);
            }
            let name = specification
                .get("package")
                .and_then(toml::Value::as_str)
                .map(str::to_owned)
                .unwrap_or_else(|| {
                    dependency_name(&manifest, alias)
                        .unwrap_or(alias)
                        .to_owned()
                });
            let requirement = specification
                .as_str()
                .or_else(|| specification.get("version").and_then(toml::Value::as_str));
            let Some(requirement) = requirement.and_then(|value| VersionReq::parse(value).ok())
            else {
                self.requires_fetch = true;
                return Ok(None);
            };
            let Some(root) = self.registry_source(&name, &requirement)? else {
                self.requires_fetch = true;
                return Ok(None);
            };
            root.join("Cargo.toml")
        };
        if resolver.read(&target).is_none() {
            self.requires_fetch = true;
            return Ok(None);
        }
        let identity = if let Some((identity, _)) = resolver
            .package
            .iter()
            .find(|(_, state)| state.source_manifest.as_ref() == Some(&target))
        {
            identity.clone()
        } else {
            let manifest = self.read_manifest(resolver, &target)?;
            let identity = format!("cached:{}", target.display());
            let edition = manifest
                .get("package")
                .and_then(|package| package.get("edition"))
                .and_then(toml::Value::as_str)
                .unwrap_or("2021")
                .to_owned();
            let dependency = dependency_aliases(&manifest)
                .into_iter()
                .map(|alias| (alias.clone(), format!("unavailable:{alias}")))
                .collect();
            resolver.package.insert(
                identity.clone(),
                RustPackage {
                    edition,
                    dependency,
                    source_manifest: Some(target.clone()),
                    ..Default::default()
                },
            );
            let root = manifest
                .get("lib")
                .and_then(|library| library.get("path"))
                .and_then(toml::Value::as_str)
                .unwrap_or("src/lib.rs");
            resolver.pending_module.insert(
                (identity.clone(), Vec::new()),
                (normalize(&target.parent().unwrap().join(root)), false),
            );
            identity
        };
        resolver
            .package
            .get_mut(package)
            .unwrap()
            .dependency
            .insert(alias.into(), identity.clone());
        Ok(Some(identity))
    }

    fn read_manifest(
        &mut self,
        resolver: &DeclarationResolver,
        path: &Path,
    ) -> Result<Arc<toml::Value>> {
        if let Some(manifest) = self.manifest.get(path) {
            return Ok(manifest.clone());
        }
        let source = resolver
            .read(path)
            .with_context(|| format!("dependency manifest {} is unavailable", path.display()))?;
        let manifest = Arc::new(toml::from_str::<toml::Value>(&source)?);
        self.manifest.insert(path.to_path_buf(), manifest.clone());
        Ok(manifest)
    }

    fn registry_source(&mut self, name: &str, requirement: &VersionReq) -> Result<Option<PathBuf>> {
        if self.registry.is_none() {
            let mut registry: BTreeMap<String, Vec<(Version, PathBuf)>> = BTreeMap::new();
            let directory = self.root.join("registry/src");
            if directory.is_dir() {
                for source in std::fs::read_dir(directory)? {
                    let source = source?.path();
                    if !source.is_dir() {
                        continue;
                    }
                    for entry in std::fs::read_dir(source)? {
                        let entry = entry?;
                        if !entry.file_type()?.is_dir() {
                            continue;
                        }
                        let filename = entry.file_name().to_string_lossy().into_owned();
                        // Try separators until the suffix forms a complete semantic version.
                        for (offset, _) in filename.match_indices('-') {
                            if let Ok(version) = Version::parse(&filename[offset + 1..]) {
                                registry
                                    .entry(filename[..offset].into())
                                    .or_default()
                                    .push((version, entry.path()));
                                break;
                            }
                        }
                    }
                }
            }
            for candidates in registry.values_mut() {
                candidates.sort_by(|left, right| right.0.cmp(&left.0).then(left.1.cmp(&right.1)));
            }
            self.registry = Some(registry);
        }
        Ok(self
            .registry
            .as_ref()
            .unwrap()
            .get(name)
            .into_iter()
            .flatten()
            .find(|(version, path)| {
                requirement.matches(version) && path.join("Cargo.toml").is_file()
            })
            .map(|(_, path)| path.clone()))
    }
}

fn dependency_specification<'manifest>(
    manifest: &'manifest toml::Value,
    alias: &str,
) -> Option<&'manifest toml::Value> {
    dependency_tables(manifest).into_iter().find_map(|table| {
        table
            .iter()
            .find(|(name, _)| name.replace('-', "_") == alias)
            .map(|(_, value)| value)
    })
}

fn dependency_name<'manifest>(
    manifest: &'manifest toml::Value,
    alias: &str,
) -> Option<&'manifest str> {
    dependency_tables(manifest).into_iter().find_map(|table| {
        table
            .keys()
            .find(|name| name.replace('-', "_") == alias)
            .map(String::as_str)
    })
}

/// Include target-specific aliases without evaluating conditional compilation.
pub(super) fn dependency_aliases(manifest: &toml::Value) -> Vec<String> {
    dependency_tables(manifest)
        .into_iter()
        .flat_map(|table| table.keys().map(|name| name.replace('-', "_")))
        .collect()
}

fn dependency_tables(manifest: &toml::Value) -> Vec<&toml::map::Map<String, toml::Value>> {
    let mut tables = Vec::new();
    for owner in std::iter::once(manifest).chain(
        manifest
            .get("target")
            .and_then(toml::Value::as_table)
            .into_iter()
            .flat_map(|target| target.values()),
    ) {
        for section in ["dependencies", "dev-dependencies", "build-dependencies"] {
            if let Some(table) = owner.get(section).and_then(toml::Value::as_table) {
                tables.push(table);
            }
        }
    }
    tables
}
