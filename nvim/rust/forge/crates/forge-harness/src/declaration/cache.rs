//! Selects cached Cargo sources for validation and navigation without resolving the project graph.

use super::*;
use semver::{Version, VersionReq};

/// Distinguishes registered source from missing or unsupported dependency source.
pub(super) enum DependencySource {
    /// Identifies the registered package whose library can be loaded lazily.
    Available(String),
    /// Retains why Cargo acquisition is needed for this reached dependency.
    Unavailable { reason: String },
}

/// Retains source candidates and reached manifests for one immutable declaration snapshot.
#[derive(Clone)]
pub(super) struct RustSourceCache {
    root: PathBuf,
    registry: Option<Arc<BTreeMap<String, Vec<(Version, PathBuf)>>>>,
    manifest: HashMap<PathBuf, Arc<toml::Value>>,
}

impl RustSourceCache {
    /// Use Cargo's existing registry directories without starting a Cargo process.
    pub(super) fn new(root: PathBuf) -> Self {
        Self {
            root,
            registry: None,
            manifest: HashMap::new(),
        }
    }

    /// Resolve one manifest alias, then register its library for lazy module indexing.
    pub(super) fn dependency(
        &mut self,
        resolver: &mut DeclarationResolver,
        package: &str,
        alias: &str,
    ) -> Result<DependencySource> {
        let Some(path) = resolver
            .package
            .get(package)
            .and_then(|state| state.source_manifest.clone())
        else {
            return Ok(DependencySource::Unavailable { reason: "dependency manifest is unavailable".into() });
        };
        let manifest = self.read_manifest(resolver, &path)?;
        let Some(mut specification) = dependency_specification(&manifest, alias).cloned() else {
            return Ok(DependencySource::Unavailable { reason: format!("dependency alias `{alias}` has no manifest declaration") });
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
                return Ok(DependencySource::Unavailable { reason: format!("workspace dependency `{alias}` is unavailable") });
            };
            directory = parent;
            specification = dependency;
        }
        let target = if let Some(path) = specification.get("path").and_then(toml::Value::as_str) {
            normalize(&directory.join(path).join("Cargo.toml"))
        } else {
            if specification.get("git").is_some() || specification.get("registry").is_some() {
                return Ok(DependencySource::Unavailable { reason: "Git or custom registry source requires Cargo acquisition".into() });
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
                return Ok(DependencySource::Unavailable { reason: "dependency version cannot be selected from the source cache".into() });
            };
            let Some(root) = self.registry_source(&name, &requirement)? else {
                return Ok(DependencySource::Unavailable { reason: format!("no cached source matches `{name}` {requirement}") });
            };
            root.join("Cargo.toml")
        };
        if resolver.read(&target).is_none() {
            return Ok(DependencySource::Unavailable { reason: format!("dependency manifest {} is unavailable", target.display()) });
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
            let dependency = dependency_aliases(&manifest)
                .into_iter()
                .map(|alias| (alias.clone(), format!("unavailable:{alias}")))
                .collect();
            resolver.register_package(identity.clone(), target.clone(), &manifest, dependency);
            let root = manifest
                .get("lib")
                .and_then(|library| library.get("path"))
                .and_then(toml::Value::as_str)
                .unwrap_or("src/lib.rs");
            resolver.register_root(&identity, target.parent().unwrap().join(root))?;
            identity
        };
        Ok(DependencySource::Available(identity))
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
            self.registry = Some(Arc::new(registry));
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
