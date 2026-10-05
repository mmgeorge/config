//! Resolves declaration references once for validation and source navigation.

mod sources;
mod cache;
pub(crate) mod trace;
#[cfg(test)]
mod tests;
mod typescript;

use crate::plan::DeclarationDesign;
use anyhow::{Context, Result};
use forge_diff::syntax::{
    DeclarationIndex, DeclarationIndexTiming, DeclarationPosition, DeclarationReference, DeclarationSymbol,
    SymbolVisibility,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Instant;

/// A resolved declaration in the proposal or an exact source file.
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, Eq, PartialEq)]
pub struct DeclarationDestination {
    pub path: String,
    pub line: u32,
    pub column: u32,
    pub proposed: bool,
    pub name: String,
    /// Select the external module's file rather than a declaration token within it.
    #[serde(default)]
    pub module_file: bool,
}

/// Evidence distinguishing invalid references from unavailable analysis.
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, Eq, PartialEq)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum DeclarationResolution {
    Resolved { destination: DeclarationDestination },
    Intrinsic,
    Invalid { reason: String },
    Ambiguous { reason: String },
    Unverified { reason: String },
}

/// One location-bearing diagnostic returned to the model and reviewer.
#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema, Eq, PartialEq)]
pub struct DeclarationDiagnostic {
    pub path: String,
    pub line: u32,
    pub column: u32,
    pub reference: String,
    pub error: bool,
    pub reason: String,
}

/// Generated submission evidence, never authored by the planning model.
#[derive(Clone, Debug, Default, Deserialize, Serialize, JsonSchema, Eq, PartialEq)]
pub struct DeclarationValidation {
    pub fingerprint: String,
    pub checked: usize,
    pub diagnostic: Vec<DeclarationDiagnostic>,
}

/// Keeps invalid reference locations intact across provider tool transports.
#[derive(Debug)]
pub(crate) struct DeclarationValidationError {
    pub diagnostic: Vec<DeclarationDiagnostic>,
}

impl std::fmt::Display for DeclarationValidationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(formatter, "Declaration validation failed:")?;
        for diagnostic in &self.diagnostic {
            writeln!(
                formatter,
                "{}:{}:{}: {}: {}",
                diagnostic.path,
                diagnostic.line,
                diagnostic.column + 1,
                diagnostic.reference,
                diagnostic.reason
            )?;
        }
        Ok(())
    }
}

impl std::error::Error for DeclarationValidationError {}

impl DeclarationValidation {
    pub fn ensure_valid(&self) -> Result<()> {
        let errors = self
            .diagnostic
            .iter()
            .filter(|diagnostic| diagnostic.error)
            .cloned()
            .collect::<Vec<_>>();
        if !errors.is_empty() {
            return Err(DeclarationValidationError { diagnostic: errors }.into());
        }
        Ok(())
    }
    pub fn warnings(&self) -> Vec<crate::plan::PlanViolation> {
        self.diagnostic
            .iter()
            .filter(|diagnostic| !diagnostic.error)
            .map(|diagnostic| crate::plan::PlanViolation {
                path: format!(
                    "{}:{}:{}",
                    diagnostic.path,
                    diagnostic.line,
                    diagnostic.column + 1
                ),
                message: diagnostic.reason.clone(),
            })
            .collect()
    }
}

#[derive(Clone)]
struct IndexedFile {
    path: PathBuf,
    index: Arc<DeclarationIndex>,
    original: Option<Arc<DeclarationIndex>>,
    package: String,
    module: Vec<String>,
    proposed: bool,
}

#[derive(Clone, Default)]
struct RustPackage {
    edition: String,
    source_manifest: Option<PathBuf>,
    dependency: HashMap<String, String>,
    incomplete: bool,
    no_std: bool,
    no_implicit_prelude: bool,
}

#[derive(Clone, Copy)]
struct RustAccess<'scope> {
    package: &'scope str,
    scope: &'scope [String],
    navigation: bool,
    macro_namespace: bool,
}

fn accessible(
    visibility: SymbolVisibility,
    package: &str,
    scope: &[String],
    origin: RustAccess<'_>,
) -> bool {
    match visibility {
        SymbolVisibility::Public => true,
        SymbolVisibility::Crate => origin.package == package,
        SymbolVisibility::Private => origin.package == package && origin.scope.starts_with(scope),
        SymbolVisibility::Parent => {
            origin.package == package
                && origin
                    .scope
                    .starts_with(&scope[..scope.len().saturating_sub(1)])
        }
    }
}

/// Owns a snapshot's module graph and the same resolution evidence used by jumps.
#[derive(Clone)]
pub(crate) struct DeclarationResolver {
    workspace: PathBuf,
    snapshot: BTreeMap<String, String>,
    cargo_lock: BTreeMap<String, String>,
    workspace_files: HashSet<String>,
    changed: HashSet<String>,
    baseline: bool,
    file: BTreeMap<PathBuf, IndexedFile>,
    package: HashMap<String, RustPackage>,
    module: HashMap<(String, Vec<String>), PathBuf>,
    pending_module: HashMap<(String, Vec<String>), (PathBuf, bool)>,
    warning: Vec<String>,
    typescript: typescript::TypescriptEnvironment,
    rust_library: bool,
    library_checked: bool,
    source_cache: Option<cache::RustSourceCache>,
    source_request: BTreeMap<String, String>,
    source_acquired: bool,
    bytes: usize,
    /// Correlates navigation work with the currently admitted review input.
    pub(crate) trace: Option<trace::DeclarationTrace>,
    work: ResolutionWork,
}

#[derive(Clone, Default, Serialize)]
struct ResolutionWork {
    member_calls: usize,
    max_depth: usize,
    cycle_or_depth_stops: usize,
    glob_branches: usize,
    file_scans: usize,
    parse_cache_hits: usize,
    parse_cache_misses: usize,
    file_load_ms: f64,
    file_read_ms: f64,
    file_parse_ms: f64,
    file_extract_ms: f64,
    module_probe_ms: f64,
    module_probes: usize,
}

type ParseCache = Mutex<BTreeMap<String, Arc<DeclarationIndex>>>;
static PARSE_CACHE: OnceLock<ParseCache> = OnceLock::new();

impl DeclarationResolver {
    /// Index snapshot declarations and discover dependency sources only when reached.
    pub(crate) fn local(workspace: &Path, design: &DeclarationDesign, baseline: bool) -> Result<Self> {
        let mut resolver = Self::snapshot(workspace, design, baseline)?;
        sources::project(&mut resolver)?;
        resolver.typescript_environment()?;
        if !resolver.package.is_empty() {
            resolver.source_cache = Some(cache::RustSourceCache::new(home::cargo_home()?));
        }
        Ok(resolver)
    }
    /// Prepare cached sources and toolchain roots without resolving the Cargo graph.
    pub(crate) async fn prepare(
        workspace: &Path,
        design: &DeclarationDesign,
        baseline: bool,
        trace: Option<trace::DeclarationTrace>,
    ) -> Result<Self> {
        let stage = trace.as_ref().map(|trace| trace.stage("prepare_resolver", Some(baseline)));
        let mut resolver = Self::local(workspace, design, baseline)?;
        resolver.trace = trace;
        resolver.prepare_sources(design).await?;
        if let Some(stage) = stage { stage.complete(resolver.statistics()); }
        Ok(resolver)
    }

    /// Report whether a reached dependency requires Cargo source acquisition.
    pub(crate) fn requires_source_fetch(&self) -> bool {
        !self.source_acquired && !self.source_request.is_empty()
    }

    /// Avoid repeating toolchain discovery when rust-src is unavailable.
    pub(crate) fn library_sources_checked(&self) -> bool {
        self.library_checked
    }

    /// Extend available evidence while retaining indexes and outstanding source requests.
    pub(crate) async fn prepare_sources(&mut self, design: &DeclarationDesign) -> Result<()> {
        if !self.package.is_empty() && !self.library_checked {
            sources::standard_library(self).await?;
        }
        if self.requires_source_fetch() {
            self.acquire(design).await?;
        }
        Ok(())
    }

    /// Retry unavailable reached sources once through Cargo without modifying the workspace.
    pub(crate) async fn acquire(&mut self, design: &DeclarationDesign) -> Result<()> {
        let mut acquired = Self::snapshot(&self.workspace, design, self.baseline)?;
        acquired.trace = self.trace.clone();
        acquired.source_request = self.source_request.clone();
        acquired.source_acquired = true;
        if !sources::acquire(&mut acquired).await? {
            self.source_acquired = true;
            self.warning.extend(acquired.warning);
            return Ok(());
        }
        acquired.typescript_environment()?;
        *self = acquired;
        Ok(())
    }

    /// Validate with cached evidence, acquiring only missing reached dependency sources.
    pub(crate) async fn validate_sources(&mut self, design: &DeclarationDesign) -> Result<DeclarationValidation> {
        let report = self.validate(design);
        if self.requires_source_fetch() {
            self.prepare_sources(design).await?;
            return Ok(self.validate(design));
        }
        Ok(report)
    }

    /// Create an isolated lexical snapshot before acquiring external evidence.
    fn snapshot(workspace: &Path, design: &DeclarationDesign, baseline: bool) -> Result<Self> {
        let workspace = dunce::canonicalize(workspace).unwrap_or_else(|_| workspace.to_path_buf());
        let mut snapshot: BTreeMap<String, String> = if baseline {
            design
                .baseline
                .iter()
                .map(|(path, file)| (path.clone(), file.text.clone()))
                .collect()
        } else {
            design.proposed.clone()
        };
        let workspace_files: HashSet<String> = design
            .baseline
            .keys()
            .chain(design.proposed.keys())
            .cloned()
            .collect();
        let changed = design.changed_paths().into_iter().collect();
        let source_paths = snapshot.keys().filter(|path| path.ends_with(".rs")).cloned().collect::<Vec<_>>();
        for path in source_paths {
            for directory in Path::new(&path).ancestors().skip(1) {
                let manifest = directory.join("Cargo.toml").to_string_lossy().replace('\\', "/");
                if snapshot.contains_key(&manifest) || workspace_files.contains(&manifest) {
                    continue;
                }
                if let Ok(text) = std::fs::read_to_string(workspace.join(&manifest)) {
                    anyhow::ensure!(text.len() <= 1024 * 1024, "{manifest}: manifest exceeds 1 MiB");
                    snapshot.insert(manifest, text);
                }
            }
        }
        let mut resolver = Self {
            workspace,
            snapshot,
            cargo_lock: design.cargo_lock.clone(),
            workspace_files,
            changed,
            baseline,
            file: BTreeMap::new(),
            package: HashMap::new(),
            module: HashMap::new(),
            pending_module: HashMap::new(),
            warning: Vec::new(),
            typescript: Default::default(),
            rust_library: false,
            library_checked: false,
            source_cache: None,
            source_request: BTreeMap::new(),
            source_acquired: false,
            bytes: 0,
            trace: None,
            work: Default::default(),
        };
        let paths = resolver
            .snapshot
            .keys()
            .filter(|path| {
                matches!(
                    Path::new(path)
                        .extension()
                        .and_then(|extension| extension.to_str()),
                    Some("ts" | "tsx" | "mts" | "cts")
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        for path in paths {
            resolver.load_file(&resolver.workspace.join(path), "", &[])?;
        }
        Ok(resolver)
    }

    /// Capture graph size and aggregate traversal work without recording source text.
    pub(crate) fn statistics(&self) -> serde_json::Value {
        let mut statistics = serde_json::to_value(&self.work).unwrap_or_default();
        statistics["files"] = serde_json::json!(self.file.len());
        statistics["packages"] = serde_json::json!(self.package.len());
        statistics["pending_modules"] = serde_json::json!(self.pending_module.len());
        statistics["bytes"] = serde_json::json!(self.bytes);
        statistics["rust_library"] = serde_json::json!(self.rust_library);
        statistics
    }

    fn read(&self, path: &Path) -> Option<String> {
        let path = normalize(path);
        if let Ok(relative) = path.strip_prefix(&self.workspace) {
            let key = relative.to_string_lossy().replace('\\', "/");
            // Captured removals stay absent, while untouched dependencies load on demand.
            if matches!(
                path.extension().and_then(|extension| extension.to_str()),
                Some("rs" | "ts" | "tsx" | "mts" | "cts" | "toml" | "json" | "jsonc")
            ) {
                if let Some(text) = self.snapshot.get(&key) {
                    return Some(text.clone());
                }
                if self.workspace_files.contains(&key) {
                    return None;
                }
            }
        }
        std::fs::read_to_string(path).ok()
    }

    fn load_file(&mut self, path: &Path, package: &str, module: &[String]) -> Result<bool> {
        let path = normalize(path);
        if self.file.contains_key(&path) {
            return Ok(true);
        }
        let stage = self.trace.as_ref().map(|trace| trace.stage("index_file", Some(self.baseline)));
        let started = Instant::now();
        let read_started = Instant::now();
        let Some(text) = self.read(&path) else {
            if let Some(stage) = stage {
                stage.complete(serde_json::json!({"path":path,"available":false}));
            }
            return Ok(false);
        };
        let read_ms = read_started.elapsed().as_secs_f64() * 1000.0;
        self.bytes += text.len();
        anyhow::ensure!(
            self.bytes <= 128 * 1024 * 1024 && self.file.len() < 16384,
            "declaration index exceeded 128 MiB or 16384 files"
        );
        let key = format!(
            "{}:{}",
            path.extension()
                .and_then(|extension| extension.to_str())
                .unwrap_or(""),
            crate::plan::digest(text.as_bytes())
        );
        let cache = PARSE_CACHE.get_or_init(Default::default);
        let cached = cache
            .lock()
            .map_err(|_| anyhow::anyhow!("declaration cache lock poisoned"))?
            .get(&key)
            .cloned();
        if self.trace.is_some() {
            if cached.is_some() { self.work.parse_cache_hits += 1; }
            else { self.work.parse_cache_misses += 1; }
        }
        let cache_hit = cached.is_some();
        let mut timing = DeclarationIndexTiming::default();
        let index = match cached {
            Some(index) => index,
            None => {
                let (index, measured) = DeclarationIndex::extract_timed(&path.to_string_lossy(), &text)
                    .map_err(|error| anyhow::anyhow!("{}: {error:?}", path.display()))?;
                timing = measured;
                let index = Arc::new(index);
                let mut cache = cache
                    .lock()
                    .map_err(|_| anyhow::anyhow!("declaration cache lock poisoned"))?;
                if cache.len() >= 512 {
                    cache.clear();
                }
                cache.insert(key, index.clone());
                index
            }
        };
        let proposed = !self.baseline
            && path.strip_prefix(&self.workspace).ok().is_some_and(|path| {
                self.changed
                    .contains(&path.to_string_lossy().replace('\\', "/"))
            });
        let original = if !self.baseline && !proposed && path.starts_with(&self.workspace) {
            std::fs::read_to_string(&path)
                .ok()
                .and_then(|source| DeclarationIndex::extract(&path.to_string_lossy(), &source).ok())
                .map(Arc::new)
        } else {
            None
        };
        self.file.insert(
            path.clone(),
            IndexedFile {
                path: path.clone(),
                index,
                original,
                package: package.into(),
                module: module.to_vec(),
                proposed,
            },
        );
        if !package.is_empty() {
            self.module.insert((package.into(), module.to_vec()), path.clone());
        }
        if let Some(stage) = stage {
            let load_ms = started.elapsed().as_secs_f64() * 1000.0;
            let parse_ms = timing.parse.as_secs_f64() * 1000.0;
            let extract_ms = timing.extract.as_secs_f64() * 1000.0;
            self.work.file_load_ms += load_ms;
            self.work.file_read_ms += read_ms;
            self.work.file_parse_ms += parse_ms;
            self.work.file_extract_ms += extract_ms;
            stage.complete(serde_json::json!({"path":path,"package":package,"module":module,
                "available":true,"bytes":text.len(),"cached":cache_hit,"read_ms":read_ms,
                "parse_ms":parse_ms,"extract_ms":extract_ms,"load_ms":load_ms}));
        }
        Ok(true)
    }

    fn load_rust(
        &mut self,
        path: &Path,
        package: &str,
        module: &[String],
        depth: usize,
    ) -> Result<()> {
        if depth > 64 {
            self.package.entry(package.into()).or_default().incomplete = true;
            return Ok(());
        }
        let path = normalize(path);
        if self.file.contains_key(&path) {
            return Ok(());
        }
        if !self.load_file(&path, package, module)? {
            self.package.entry(package.into()).or_default().incomplete = true;
            if !path.starts_with(&self.workspace)
                && self.package.get(package).is_some_and(|state| state.source_manifest.is_some())
            {
                self.source_request.insert(path.to_string_lossy().into_owned(), "module source is unavailable".into());
            }
            return Ok(());
        }
        let index = self.file[&path].index.clone();
        let state = self.package.entry(package.into()).or_default();
        state.incomplete |= index.incomplete;
        if module.is_empty() {
            state.no_std = index.no_std;
            state.no_implicit_prelude = index.no_implicit_prelude;
        }
        let directory = path.parent().unwrap_or(Path::new("."));
        let base = if module.is_empty() || path.file_name().is_some_and(|name| name == "mod.rs") {
            directory.to_path_buf()
        } else {
            directory.join(path.file_stem().unwrap_or_default())
        };
        for declaration in &index.module {
            let mut nested = module.to_vec();
            nested.extend(declaration.scope.clone());
            nested.push(declaration.name.clone());
            if declaration.inline {
                self.module.insert((package.into(), nested), path.clone());
                continue;
            }
            let container = declaration
                .scope
                .iter()
                .fold(base.clone(), |directory, name| directory.join(name));
            let explicit = declaration
                .path
                .as_ref()
                .map(|relative| container.join(relative));
            let direct = container.join(format!("{}.rs", declaration.name));
            let target = explicit.unwrap_or_else(|| {
                let probe_started = Instant::now();
                let available = self.read(&direct).is_some();
                if self.trace.is_some() {
                    self.work.module_probe_ms += probe_started.elapsed().as_secs_f64() * 1000.0;
                    self.work.module_probes += 1;
                }
                if available {
                    direct
                } else {
                    container.join(&declaration.name).join("mod.rs")
                }
            });
            if !path.starts_with(&self.workspace) {
                self.pending_module
                    .insert((package.into(), nested), (target, declaration.conditional));
                continue;
            }
            self.load_rust(&target, package, &nested, depth + 1)?;
            if declaration.conditional {
                for file in self
                    .file
                    .values_mut()
                    .filter(|file| file.package == package && file.module.starts_with(&nested))
                {
                    let index = Arc::make_mut(&mut file.index);
                    for symbol in &mut index.symbol {
                        symbol.conditional = true;
                    }
                    for reference in &mut index.reference {
                        reference.conditional = true;
                    }
                    for import in &mut index.import {
                        import.conditional = true;
                    }
                }
            }
        }
        Ok(())
    }

    fn register_package(
        &mut self,
        identity: String,
        manifest_path: PathBuf,
        manifest: &toml::Value,
        dependency: HashMap<String, String>,
    ) {
        let edition = manifest.get("package")
            .and_then(|package| package.get("edition"))
            .and_then(toml::Value::as_str)
            .unwrap_or("2015").to_owned();
        self.package.insert(identity, RustPackage {
            edition,
            source_manifest: Some(normalize(&manifest_path)),
            dependency,
            ..Default::default()
        });
    }

    fn register_root(&mut self, package: &str, root: PathBuf) -> Result<()> {
        let root = normalize(&root);
        if root.starts_with(&self.workspace) {
            self.load_rust(&root, package, &[], 0)?;
        } else {
            self.pending_module.insert((package.into(), Vec::new()), (root, false));
        }
        Ok(())
    }

    /// Validate imports and signature references only in proposed changed files.
    pub(crate) fn validate(&mut self, design: &DeclarationDesign) -> DeclarationValidation {
        let changed = design.changed_paths().into_iter().collect::<HashSet<_>>();
        let files = self
            .file
            .values()
            .filter(|file| {
                file.path
                    .strip_prefix(&self.workspace)
                    .ok()
                    .is_some_and(|relative| {
                        changed.contains(&relative.to_string_lossy().replace('\\', "/"))
                    })
            })
            .cloned()
            .collect::<Vec<_>>();
        let mut report = DeclarationValidation {
            fingerprint: fingerprint(design),
            ..Default::default()
        };
        for warning in &self.warning {
            report.diagnostic.push(DeclarationDiagnostic {
                path: "plan".into(),
                line: 1,
                column: 0,
                reference: String::new(),
                error: false,
                reason: warning.clone(),
            });
        }
        if !self.rust_library
            && !self.package.is_empty()
            && !report
                .diagnostic
                .iter()
                .any(|diagnostic| diagnostic.reason.contains("rust-src"))
        {
            report.diagnostic.push(DeclarationDiagnostic { path:"plan".into(),line:1,column:0,reference:String::new(),error:false,reason:"rust-src is unavailable. Run `rustup component add rust-src` in the workspace. Standard-library and prelude checks are disabled.".into() });
        }
        for file in files {
            for reference in &file.index.reference {
                if reference.macro_namespace { continue; }
                let result = self.resolve_reference(&file, reference, false);
                record(
                    &mut report,
                    &self.workspace,
                    &file.path,
                    reference.position,
                    &reference.path.join("::"),
                    result,
                );
            }
            for import in &file.index.import {
                if import.conditional {
                    continue;
                }
                let mut path = import.path.clone();
                if import.glob || import.namespace {
                    // Namespace imports require a module, not a same-named declaration.
                    path.push("*".into());
                }
                let result = if file.package.is_empty() {
                    let result =
                        self.resolve_ts_import(&file, import, &path, false, &mut HashSet::new());
                    if matches!(result, DeclarationResolution::Invalid { .. }) {
                        self.resolve_ts_import(&file, import, &path, true, &mut HashSet::new())
                    } else {
                        result
                    }
                } else {
                    let result = self.resolve_rust_path(
                        &file.package,
                        &joined(&file.module, &import.scope),
                        &path,
                        false,
                        RustAccess {
                            package: &file.package,
                            scope: &file.module,
                            navigation: false,
                            macro_namespace: false,
                        },
                        &mut HashSet::new(),
                    );
                    if matches!(result, DeclarationResolution::Invalid { .. }) {
                        self.resolve_rust_path(
                            &file.package,
                            &joined(&file.module, &import.scope),
                            &path,
                            true,
                            RustAccess {
                                package: &file.package,
                                scope: &file.module,
                                navigation: false,
                                macro_namespace: false,
                            },
                            &mut HashSet::new(),
                        )
                    } else {
                        result
                    }
                };
                let result = if import.export && !file.package.is_empty() {
                    self.exported_resolution(result, import.visibility)
                } else {
                    result
                };
                record(
                    &mut report,
                    &self.workspace,
                    &file.path,
                    import.position,
                    &import.source.clone().unwrap_or_else(|| path.join("::")),
                    result,
                );
            }
        }
        let mut seen = HashSet::new();
        report.diagnostic.retain(|diagnostic| {
            seen.insert((
                diagnostic.path.clone(),
                diagnostic.line,
                diagnostic.column,
                diagnostic.reason.clone(),
            ))
        });
        report
    }

    /// Resolve the exact signature token selected in saved declaration coordinates.
    pub(crate) fn at(&mut self, path: &str, line: u32, column: u32) -> DeclarationResolution {
        let stage = self
            .trace
            .as_ref()
            .map(|trace| trace.stage("resolve", Some(self.baseline)));
        self.work = Default::default();
        let before_files = self.file.len();
        let result = (|| {
            let absolute = normalize(&self.workspace.join(path));
            let Some(file) = self.file.get(&absolute).cloned() else {
                return unverified("this file has no Rust or TypeScript declaration index");
            };
            if let Some(reference) = file.index.reference.iter().find(|reference| {
                reference.position.line == line
                    && reference.position.column <= column
                    && (column as usize) < reference.position.column as usize + reference.length
            }) {
                return self.resolve_reference(&file, reference, true);
            }
            if let Some(symbol) = file.index.symbol.iter().find(|symbol| {
                symbol.position.line == line
                    && symbol.position.column <= column
                    && column < symbol.position.column + symbol.name.len() as u32
            }) {
                if let Some(module) = file.index.module.iter().find(|module| {
                    !module.inline && module.name == symbol.name && module.scope == symbol.scope
                }) {
                    if module.conditional {
                        return unverified("module is conditionally compiled");
                    }
                    let mut scope = joined(&file.module, &module.scope);
                    scope.push(module.name.clone());
                    let destination = self
                        .module
                        .get(&(file.package.clone(), scope))
                        .and_then(|path| self.file.get(path));
                    return match destination {
                        Some(destination) => DeclarationResolution::Resolved {
                            destination: DeclarationDestination {
                                path: destination.path.to_string_lossy().into_owned(),
                                line: 1,
                                column: 0,
                                proposed: destination.proposed,
                                name: module.name.clone(),
                                module_file: true,
                            },
                        },
                        None => unverified("module source is unavailable"),
                    };
                }
                return resolved(&file, symbol);
            }
            if let Some(import) = file
                .index
                .import
                .iter()
                .filter(|import| import.position.line == line && import.position.column <= column)
                .max_by_key(|import| import.position.column)
            {
                return if file.package.is_empty() {
                    self.resolve_ts_import(&file, import, &import.path, false, &mut HashSet::new())
                } else {
                    self.resolve_rust_path(
                        &file.package,
                        &joined(&file.module, &import.scope),
                        &import.path,
                        false,
                        RustAccess {
                            package: &file.package,
                            scope: &file.module,
                            navigation: true,
                            macro_namespace: false,
                        },
                        &mut HashSet::new(),
                    )
                };
            }
            unverified("cursor is not on a declaration type or import")
        })();
        if let Some(stage) = stage {
            let mut details = self.statistics();
            details["path"] = serde_json::json!(path);
            details["line"] = serde_json::json!(line);
            details["column"] = serde_json::json!(column);
            details["loaded_files"] = serde_json::json!(self.file.len() - before_files);
            details["resolution"] = serde_json::to_value(&result).unwrap_or_default();
            stage.complete(details);
        }
        result
    }
    fn resolve_reference(
        &mut self,
        file: &IndexedFile,
        reference: &DeclarationReference,
        navigation: bool,
    ) -> DeclarationResolution {
        if reference.conditional {
            return unverified("reference is conditionally compiled");
        }
        if reference.path.iter().any(|part| part.contains(['<', '>'])) {
            return unverified("qualified or dependent types require semantic evidence");
        }
        if file.package.is_empty() {
            return self.resolve_ts(file, reference, &mut HashSet::new());
        }
        let scope = joined(&file.module, &reference.scope);
        for depth in (0..=scope.len()).rev() {
            let result = self.rust_member(
                &file.package,
                &scope[..depth],
                &reference.path,
                reference.value_namespace,
                RustAccess {
                    package: &file.package,
                    scope: &scope,
                    navigation,
                    macro_namespace: reference.macro_namespace,
                },
                &mut HashSet::new(),
            );
            let bound = reference.path.first().is_some_and(|name| {
                self.rust_bound(&file.package, &scope[..depth], name, reference.value_namespace, reference.macro_namespace)
            });
            if bound
                || matches!(
                    result,
                    DeclarationResolution::Resolved { .. }
                        | DeclarationResolution::Intrinsic
                        | DeclarationResolution::Ambiguous { .. }
                )
            {
                return result;
            }
        }
        let first = reference.path.first().map(String::as_str).unwrap_or("");
        let package = self.package.get(&file.package).cloned().unwrap_or_default();
        if first == "crate"
            || first == "self"
            || first == "super"
            || package.dependency.contains_key(first)
            || matches!(first, "std" | "core" | "alloc")
        {
            return self.resolve_rust_path(
                &file.package,
                &file.module,
                &reference.path,
                reference.value_namespace,
                RustAccess {
                    package: &file.package,
                    scope: &scope,
                    navigation,
                    macro_namespace: reference.macro_namespace,
                },
                &mut HashSet::new(),
            );
        }
        if !reference.value_namespace && reference.path.len() == 1 && rust_primitive(first) {
            return DeclarationResolution::Intrinsic;
        }
        if !package.no_implicit_prelude && !file.index.no_implicit_prelude {
            if !self.rust_library {
                return unverified("rust-src is unavailable, so prelude resolution is disabled");
            }
            let library = if package.no_std { "core" } else { "std" };
            let path = [
                vec![
                    "prelude".into(),
                    format!(
                        "rust_{}",
                        if package.edition.is_empty() {
                            "2015"
                        } else {
                            &package.edition
                        }
                    ),
                ],
                reference.path.clone(),
            ]
            .concat();
            let result = self.rust_member(
                library,
                &[],
                &path,
                reference.value_namespace,
                RustAccess {
                    package: library,
                    scope: &[],
                    navigation,
                    macro_namespace: reference.macro_namespace,
                },
                &mut HashSet::new(),
            );
            if !matches!(result, DeclarationResolution::Invalid { .. }) {
                return result;
            }
        }
        self.missing_rust(&file.package, &reference.path.join("::"))
    }

    fn exported_resolution(
        &self,
        result: DeclarationResolution,
        visibility: SymbolVisibility,
    ) -> DeclarationResolution {
        if visibility == SymbolVisibility::Public {
            if let DeclarationResolution::Resolved { destination } = &result {
                if let Some(file) = self.file.get(Path::new(&destination.path)) {
                    let index = file.original.as_deref().unwrap_or(&file.index);
                    if index.symbol.iter().any(|symbol| {
                        symbol.position.line == destination.line
                            && symbol.position.column == destination.column
                            && symbol.visibility != SymbolVisibility::Public
                    }) {
                        return invalid(
                            "public re-export refers to a declaration with restricted visibility",
                        );
                    }
                }
            }
        }
        result
    }

    fn resolve_rust_path(
        &mut self,
        package: &str,
        scope: &[String],
        path: &[String],
        value: bool,
        origin: RustAccess<'_>,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        let Some(first) = path.first().map(String::as_str) else {
            return invalid("empty Rust import path");
        };
        match first {
            "crate" => self.rust_member(package, &[], &path[1..], value, origin, visited),
            "self" => self.rust_member(package, scope, &path[1..], value, origin, visited),
            "super" => {
                if scope.is_empty() {
                    invalid("super refers outside the crate root")
                } else {
                    self.resolve_rust_path(
                        package,
                        &scope[..scope.len() - 1],
                        &path[1..],
                        value,
                        origin,
                        visited,
                    )
                }
            }
            "std" | "core" | "alloc" => {
                if self.rust_library {
                    self.rust_member(first, &[], &path[1..], value, origin, visited)
                } else {
                    unverified(
                        "rust-src is unavailable, so standard-library resolution is disabled",
                    )
                }
            }
            _ => {
                if self.rust_bound(package, scope, first, value, origin.macro_namespace) {
                    return self.rust_member(package, scope, path, value, origin, visited);
                }
                if self.package.get(package).and_then(|state| state.dependency.get(first))
                    .is_some_and(|identity| identity.starts_with("unavailable:")) {
                    if let Some(mut cache) = self.source_cache.take() {
                        let stage = self.trace.as_ref().map(|trace| trace.stage("cached_dependency", Some(self.baseline)));
                        let result = cache.dependency(self, package, first);
                        self.source_cache = Some(cache);
                        let cached = match &result {
                            Ok(cache::DependencySource::Available(identity)) => {
                                self.package.get_mut(package).unwrap().dependency.insert(first.into(), identity.clone());
                                true
                            }
                            Ok(cache::DependencySource::Unavailable { reason }) => {
                                self.source_request.insert(format!("{package}:{first}"), reason.clone());
                                false
                            }
                            Err(error) => {
                                self.source_request.insert(format!("{package}:{first}"), format!("{error:#}"));
                                false
                            }
                        };
                        if let Some(stage) = stage {
                            stage.complete(serde_json::json!({"package":package,"alias":first,
                                "cached":cached,"requires_fetch":self.requires_source_fetch()}));
                        }
                        if let Err(error) = result {
                            return unverified(&format!("cached dependency source cannot be located: {error:#}"));
                        }
                    }
                }
                if let Some(dependency) = self
                    .package
                    .get(package)
                    .and_then(|package| package.dependency.get(first))
                    .cloned()
                {
                    return self.rust_member(&dependency, &[], &path[1..], value, origin, visited);
                }
                self.rust_member(package, &[], path, value, origin, visited)
            }
        }
    }

    fn rust_bound(&self, package: &str, scope: &[String], name: &str, value: bool, macro_namespace: bool) -> bool {
        self.rust_scope_file(package, scope).is_some_and(|file| {
            let relative = scope.strip_prefix(file.module.as_slice()).unwrap();
            file.index.symbol.iter().any(|symbol| {
                symbol.scope == relative && symbol.name == name
                    && if macro_namespace { symbol.macro_namespace } else if value { symbol.value_namespace } else { symbol.type_namespace }
            }) || file.index.import.iter().any(|import| {
                import.scope == relative && !import.glob && import.alias.as_deref() == Some(name)
            })
        })
    }

    fn rust_scope_file(&self, package: &str, scope: &[String]) -> Option<&IndexedFile> {
        (0..=scope.len()).rev().find_map(|depth| {
            self.module.get(&(package.into(), scope[..depth].to_vec()))
                .and_then(|path| self.file.get(path))
        })
    }

    fn rust_member(
        &mut self,
        package: &str,
        scope: &[String],
        path: &[String],
        value: bool,
        origin: RustAccess<'_>,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        if self.trace.is_some() {
            self.work.member_calls += 1;
            self.work.max_depth = self.work.max_depth.max(visited.len());
        }
        let key = format!("{package}:{}:{}:{value}", scope.join("::"), path.join("::"));
        if visited.len() > 256 || !visited.insert(key.clone()) {
            if self.trace.is_some() { self.work.cycle_or_depth_stops += 1; }
            return unverified("cyclic or excessively deep re-export chain");
        }
        let result = self.rust_member_inner(package, scope, path, value, origin, visited);
        visited.remove(&key);
        result
    }

    fn rust_member_inner(
        &mut self,
        package: &str,
        scope: &[String],
        path: &[String],
        value: bool,
        origin: RustAccess<'_>,
        visited: &mut HashSet<String>,
    ) -> DeclarationResolution {
        for depth in 0..=scope.len() {
            let module = &scope[..depth];
            if let Some((target, conditional)) = self
                .pending_module
                .remove(&(package.into(), module.to_vec()))
            {
                if let Err(error) = self.load_rust(&target, package, module, depth) {
                    self.package.entry(package.into()).or_default().incomplete = true;
                    return unverified(&format!("module source cannot be indexed: {error:#}"));
                }
                if conditional {
                    return unverified("module is conditionally compiled");
                }
            }
        }
        if path.is_empty() || path == ["*"] {
            return if self.module.contains_key(&(package.into(), scope.to_vec())) {
                DeclarationResolution::Intrinsic
            } else {
                self.missing_rust(package, &scope.join("::"))
            };
        }
        let first = &path[0];
        let mut results = Vec::new();
        let files = self.rust_scope_file(package, scope).cloned().into_iter().collect::<Vec<_>>();
        if self.trace.is_some() { self.work.file_scans += files.len(); }
        for file in &files {
            let relative = scope.strip_prefix(file.module.as_slice());
            let Some(relative) = relative else {
                continue;
            };
            for symbol in file.index.symbol.iter().filter(|symbol| {
                symbol.scope == relative
                    && &symbol.name == first
                    && if origin.macro_namespace {
                        symbol.macro_namespace
                    } else if value {
                        symbol.value_namespace
                    } else {
                        symbol.type_namespace
                    }
            }) {
                if !accessible(symbol.visibility, package, scope, origin) {
                    return invalid("declaration is not public outside its crate");
                }
                if symbol.conditional {
                    results.push(unverified("declaration is conditionally compiled"));
                } else if path.len() == 1 {
                    results.push(resolved(file, symbol));
                } else if symbol.parameter {
                    results.push(unverified(
                        "associated types on generic parameters require semantic evidence",
                    ));
                } else {
                    let mut nested = scope.to_vec();
                    nested.push(first.clone());
                    results.push(self.rust_member(
                        package,
                        &nested,
                        &path[1..],
                        value,
                        origin,
                        visited,
                    ));
                }
            }
            // A qualified path enters its lexical container before considering same-named aliases.
            if path.len() > 1 && !results.is_empty() {
                return combine(results, || self.missing_rust(package, &joined(scope, path).join("::")), origin.navigation);
            }
            for import in file.index.import.iter().filter(|import| {
                import.scope == relative && !import.glob && import.alias.as_ref() == Some(first)
            }) {
                if !accessible(import.visibility, package, scope, origin) {
                    continue;
                }
                if import.conditional {
                    results.push(unverified("re-export is conditionally compiled"));
                    continue;
                }
                let mut target = import.path.clone();
                target.extend(if import.glob { path } else { &path[1..] }.iter().cloned());
                let result = self.resolve_rust_path(
                    package,
                    scope,
                    &target,
                    value,
                    RustAccess {
                        package,
                        scope,
                        ..origin
                    },
                    visited,
                );
                results.push(self.exported_resolution(result, import.visibility));
            }
        }
        // Lexical declarations and explicit imports take precedence over wildcard imports.
        if !results.is_empty() {
            return combine(
                results,
                || self.missing_rust(package, &joined(scope, path).join("::")),
                origin.navigation,
            );
        }
        for file in &files {
            let Some(relative) = scope.strip_prefix(file.module.as_slice()) else {
                continue;
            };
            for import in file
                .index
                .import
                .iter()
                .filter(|import| import.scope == relative && import.glob)
            {
                if !accessible(import.visibility, package, scope, origin) {
                    continue;
                }
                if import.conditional {
                    results.push(unverified("re-export is conditionally compiled"));
                    continue;
                }
                if self.trace.is_some() { self.work.glob_branches += 1; }
                let target = joined(&import.path, path);
                let result = self.resolve_rust_path(
                    package,
                    scope,
                    &target,
                    value,
                    RustAccess {
                        package,
                        scope,
                        ..origin
                    },
                    visited,
                );
                results.push(self.exported_resolution(result, import.visibility));
            }
        }
        combine(
            results,
            || self.missing_rust(package, &joined(scope, path).join("::")),
            origin.navigation,
        )
    }

    fn missing_rust(&self, package: &str, name: &str) -> DeclarationResolution {
        match self.package.get(package) {
            None => unverified("dependency source or standard-library source is unavailable"),
            Some(package) if package.incomplete => unverified(
                "source graph contains macros, generated modules, unsupported syntax, or unavailable modules",
            ),
            _ => invalid(&format!("no accessible declaration resolves `{name}`")),
        }
    }
}

pub(crate) fn fingerprint(design: &DeclarationDesign) -> String {
    crate::plan::digest(
        &serde_json::to_vec(&(&design.proposed, &design.cargo_lock))
            .expect("string maps serialize"),
    )
}

fn normalize(path: &Path) -> PathBuf {
    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                result.pop();
            }
            _ => result.push(component.as_os_str()),
        }
    }
    result
}

fn joined(prefix: &[String], suffix: &[String]) -> Vec<String> {
    prefix.iter().chain(suffix).cloned().collect()
}
fn invalid(reason: &str) -> DeclarationResolution {
    DeclarationResolution::Invalid {
        reason: reason.into(),
    }
}
fn unverified(reason: &str) -> DeclarationResolution {
    DeclarationResolution::Unverified {
        reason: reason.into(),
    }
}

fn resolved(file: &IndexedFile, symbol: &DeclarationSymbol) -> DeclarationResolution {
    let position = file
        .original
        .as_ref()
        .and_then(|index| {
            index
                .symbol
                .iter()
                .find(|original| original.name == symbol.name && original.scope == symbol.scope)
        })
        .map(|original| original.position)
        .unwrap_or(symbol.position);
    DeclarationResolution::Resolved {
        destination: DeclarationDestination {
            path: file.path.to_string_lossy().into_owned(),
            line: position.line,
            column: position.column,
            proposed: file.proposed,
            name: symbol.name.clone(),
            module_file: false,
        },
    }
}

fn combine(
    results: Vec<DeclarationResolution>,
    missing: impl FnOnce() -> DeclarationResolution,
    navigation: bool,
) -> DeclarationResolution {
    let mut destinations = Vec::new();
    let mut uncertain = None;
    for result in results {
        match result {
            DeclarationResolution::Resolved { destination } => {
                if !destinations.contains(&destination) {
                    destinations.push(destination);
                }
            }
            DeclarationResolution::Intrinsic => return DeclarationResolution::Intrinsic,
            DeclarationResolution::Ambiguous { .. } => return result,
            DeclarationResolution::Unverified { .. } => {
                uncertain = Some(result)
            }
            DeclarationResolution::Invalid { .. } => {}
        }
    }
    if destinations.len() > 1 {
        return DeclarationResolution::Ambiguous {
            reason: "multiple accessible declarations resolve this name".into(),
        };
    }
    // Navigation can use one concrete destination without claiming feature availability.
    if let Some(result) = uncertain.filter(|_| !navigation || destinations.is_empty()) {
        return result;
    }
    match destinations.pop() {
        Some(destination) => DeclarationResolution::Resolved { destination },
        None => missing(),
    }
}

fn rust_primitive(name: &str) -> bool {
    matches!(
        name,
        "bool"
            | "char"
            | "str"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "f16"
            | "f32"
            | "f64"
            | "f128"
    )
}

fn record(
    report: &mut DeclarationValidation,
    workspace: &Path,
    path: &Path,
    position: DeclarationPosition,
    reference: &str,
    result: DeclarationResolution,
) {
    report.checked += 1;
    if matches!(&result, DeclarationResolution::Unverified { reason } if reason.starts_with("rust-src is unavailable"))
    {
        return;
    }
    let (error, reason) = match result {
        DeclarationResolution::Resolved { .. } | DeclarationResolution::Intrinsic => return,
        DeclarationResolution::Invalid { reason } | DeclarationResolution::Ambiguous { reason } => {
            (true, reason)
        }
        DeclarationResolution::Unverified { reason } => (false, reason),
    };
    report.diagnostic.push(DeclarationDiagnostic {
        path: path
            .strip_prefix(workspace)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/"),
        line: position.line,
        column: position.column,
        reference: reference.into(),
        error,
        reason,
    });
}
