use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use super::design_document::DesignDocument;
use anyhow::{Context, Result, ensure};
use forge_diff::syntax::DeclarationOverview;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Retains the immutable declaration source and workspace content identity.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationFile {
    pub text: String,
    pub source_digest: String,
}

/// Borrow the original and proposed paths while owning the computed change list.
#[derive(Serialize)]
pub(crate) struct DesignFileInventory<'a> {
    paths: BTreeSet<&'a String>,
    changed_paths: Vec<String>,
}

const PLAN_DOCUMENT_PATH: &str = "plan.json";

type SourceOverviewCache =
    std::sync::Mutex<BTreeMap<String, (DeclarationFile, Vec<super::FunctionBody>)>>;
static SOURCE_OVERVIEW_CACHE: std::sync::OnceLock<SourceOverviewCache> = std::sync::OnceLock::new();

/// Owns baseline declarations and the agent's proposed file contents.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationDesign {
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Proposed check commands, distinct from executed verification evidence.
    pub check_preview: Vec<crate::checks::CheckPreview>,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    /// Input identity preventing edited plans from displaying obsolete commands.
    pub check_preview_source: String,
    /// Captures read-only project checks displayed with a submitted design.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub check_configuration: Option<crate::checks::ConfigSnapshot>,
    /// Retains dependency selection independently of subsequent checkout edits.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub cargo_lock: BTreeMap<String, String>,
    /// Generated reference evidence for the exact submitted declaration snapshot.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub validation: Option<crate::declaration::DeclarationValidation>,
    #[serde(default)]
    pub document: DesignDocument,
    #[serde(default = "default_line_width")]
    pub line_width: usize,
    pub baseline: BTreeMap<String, DeclarationFile>,
    pub proposed: BTreeMap<String, String>,
    pub moved: BTreeMap<String, String>,
    /// Function metadata captured with each source baseline, retaining reference extraction order.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub baseline_calls: BTreeMap<String, Vec<super::FunctionBody>>,
    /// Function changes and reference relationships accompanying proposed declarations.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub proposed_calls: BTreeMap<String, Vec<super::FunctionBody>>,
}

fn default_line_width() -> usize {
    80
}

impl Default for DeclarationDesign {
    fn default() -> Self {
        Self {
            check_preview: Vec::new(),
            check_preview_source: String::new(),
            check_configuration: None,
            cargo_lock: BTreeMap::new(),
            validation: None,
            document: DesignDocument::default(),
            line_width: default_line_width(),
            baseline: BTreeMap::new(),
            proposed: BTreeMap::new(),
            moved: BTreeMap::new(),
            baseline_calls: BTreeMap::new(),
            proposed_calls: BTreeMap::new(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct ValidatedDesign {
    design: DeclarationDesign,
    report: crate::declaration::DeclarationValidation,
}

impl ValidatedDesign {
    pub(crate) fn warnings(&self) -> Vec<super::PlanViolation> {
        self.report.warnings()
    }

    pub(crate) fn into_design(mut self) -> DeclarationDesign {
        self.design.validation = Some(self.report);
        self.design
    }
}

impl DeclarationDesign {
    /// Format and validate one submission using only captured declaration evidence.
    pub(crate) async fn validated(&self, workspace: &Path) -> Result<ValidatedDesign> {
        self.check_workspace(workspace)?;
        self.validated_revision(workspace).await
    }

    /// Collect every rule the planning model must satisfy in authored declarations: comment
    /// wording on this design, then documentation and incoming use on its formatted form,
    /// whose positions match the shared reference index.
    pub(super) fn authored_diagnostics(
        &self,
        formatted: &Self,
        workspace: &Path,
    ) -> Result<Vec<crate::declaration::DeclarationDiagnostic>> {
        let index = super::references::PlanReferenceIndex::design(formatted, workspace)?;
        let mut authored = super::comment_lint::diagnostics(self)?;
        authored.extend(super::documentation::diagnostics(formatted, &index)?);
        authored.extend(super::usage::diagnostics(formatted, &index, workspace)?);
        Ok(authored)
    }

    /// Validate a new target without replacing the immutable pre-execution baseline.
    pub(crate) async fn validated_revision(&self, workspace: &Path) -> Result<ValidatedDesign> {
        let mut design = self.formatted()?;
        let configuration = crate::checks::ConfigSnapshot::load(workspace)?;
        configuration
            .config
            .validate_selection(&design.document.checks)?;
        for id in &design.document.disabled_checks {
            anyhow::ensure!(
                configuration
                    .config
                    .definitions
                    .iter()
                    .any(|check| &check.id == id),
                "unknown disabled planned check: {id}"
            );
        }
        design.check_configuration = Some(configuration);
        // Authored-rule diagnostics wait for the resolver so one rejection reports them with
        // every reference failure instead of one rule family per submission.
        let mut authored = self.authored_diagnostics(&design, workspace)?;
        let paths = design
            .proposed
            .keys()
            .filter(|path| path.ends_with(".rs") || path.ends_with("Cargo.toml"))
            .cloned()
            .collect::<Vec<_>>();
        for path in paths {
            for directory in Path::new(&path).ancestors().skip(1) {
                let lock = directory
                    .join("Cargo.lock")
                    .to_string_lossy()
                    .replace('\\', "/");
                if !design.cargo_lock.contains_key(&lock)
                    && let Some(text) = workspace_source(workspace, &lock)?
                {
                    toml::from_str::<toml::Value>(&text)
                        .with_context(|| format!("parse {lock}"))?;
                    design.cargo_lock.insert(lock, text);
                }
            }
        }
        design.validate()?;
        let mut resolver =
            crate::declaration::DeclarationResolver::planned(workspace, &design, false)?;
        authored.extend(super::calls::validate_behaviors(&design, &mut resolver)?);
        let report = resolver.validate(&design);
        crate::declaration::DeclarationValidationError::ensure_empty(
            report.diagnostic.iter().cloned().chain(authored).collect(),
        )?;
        design.check_preview = crate::checks::preview_plan(workspace, &design).await?;
        design.check_preview_source = design.check_preview_fingerprint()?;
        Ok(ValidatedDesign { design, report })
    }

    /// Identifies only the planned inputs that determine check selection.
    pub(crate) fn check_preview_fingerprint(&self) -> Result<String> {
        let manifests: BTreeMap<_, _> = self
            .proposed
            .iter()
            .filter(|(path, _)| {
                path.ends_with("Cargo.toml")
                    || path.ends_with("package.json")
                    || path.ends_with("pnpm-workspace.yaml")
            })
            .collect();
        Ok(super::digest(&super::codec::to_vec(&(
            &self.check_configuration,
            self.changed_paths(),
            &self.document.tests,
            &self.document.checks,
            &self.document.disabled_checks,
            manifests,
        ))?))
    }

    /// Projects plan choices independently of repository defaults and trigger applicability.
    pub(crate) fn check_options(&self) -> Vec<serde_json::Value> {
        self.check_configuration.as_ref().map(|snapshot| snapshot.config.definitions.iter().map(|check| {
            let enabled = check.enabled && !self.document.disabled_checks.contains(&check.id)
                && (check.auto || self.document.checks.contains(&check.id)
                    || self.check_preview.iter().any(|preview| preview.id == check.id && preview.selected));
            serde_json::json!({"id":check.id, "name":check.name, "stage":check.category,
                "enabled":enabled, "available":check.enabled,
                "reason":self.check_preview.iter().find(|preview| preview.id == check.id).map(|preview| &preview.reason)})
        }).collect()).unwrap_or_default()
    }

    /// Rejects cached command previews after selection inputs change.
    pub(crate) fn check_preview_current(&self) -> Result<bool> {
        Ok(!self.check_preview.is_empty()
            && self.check_preview_source == self.check_preview_fingerprint()?)
    }

    /// Initialize plan settings without discovering or extracting workspace files.
    pub fn open(workspace: &Path) -> Result<Self> {
        let mut design = Self::default();
        match std::fs::read_to_string(workspace.join(".forge.json")) {
            Ok(config) => {
                let config: serde_json::Value =
                    serde_json::from_str(&config).context("decode .forge.json")?;
                if let Some(width) = config.get("declaration_line_width") {
                    design.line_width = width
                        .as_u64()
                        .and_then(|width| usize::try_from(width).ok())
                        .context(".forge.json declaration_line_width must be an integer")?;
                }
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(error).context("read .forge.json"),
        }
        design.validate()?;
        Ok(design)
    }

    pub(crate) fn source(
        &self,
        workspace: &Path,
        path: &str,
    ) -> Result<(DeclarationFile, Vec<super::FunctionBody>)> {
        validate_path(path)?;
        let source = workspace_source(workspace, path)?
            .with_context(|| format!("declaration file does not exist: {path}"))?;
        let source_digest = super::digest(source.as_bytes());
        let key = format!("{path}:{}:{source_digest}", self.line_width);
        let cache = SOURCE_OVERVIEW_CACHE.get_or_init(Default::default);
        let cached = cache
            .lock()
            .map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?
            .get(&key)
            .cloned();
        let overview = if let Some(overview) = cached {
            overview
        } else {
            let (text, calls) = DeclarationOverview::extract_with_calls(path, &source)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("extract {path}"))?;
            let text = DeclarationOverview::format_with_width(path, &text, self.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let calls = super::calls::from_extracted(calls);
            let saved_owners = forge_diff::syntax::DeclarationCalls::extract(path, &text, true)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?
                .into_iter()
                .map(|function| function.owner)
                .collect::<BTreeSet<_>>();
            let calls = calls
                .into_iter()
                .filter(|function| saved_owners.contains(&function.owner))
                .collect();
            let overview = (
                DeclarationFile {
                    text,
                    source_digest,
                },
                calls,
            );
            let mut cache = cache
                .lock()
                .map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?;
            let bytes = cache
                .values()
                .map(|(file, calls)| {
                    file.text.len()
                        + calls
                            .iter()
                            .map(|function| {
                                function.owner.len()
                                    + function
                                        .call
                                        .iter()
                                        .flatten()
                                        .map(|call| call.name.len() + 16)
                                        .sum::<usize>()
                            })
                            .sum::<usize>()
                })
                .sum::<usize>();
            if cache.len() >= 16 || bytes >= 8 * 1024 * 1024 {
                cache.clear();
            }
            cache.insert(key, overview.clone());
            overview
        };
        let (file, mut calls) = overview;
        if calls
            .iter()
            .flat_map(|function| function.call.iter().flatten())
            .any(|reference| {
                matches!(
                    reference.kind,
                    super::CallKind::Value | super::CallKind::Property | super::CallKind::Call
                )
            })
        {
            let mut captured = self.clone();
            captured.proposed.insert(path.into(), file.text.clone());
            let mut resolver =
                crate::declaration::DeclarationResolver::planned(workspace, &captured, false)?;
            super::calls::classify(path, &mut calls, &mut resolver);
        }
        Ok((file, calls))
    }

    /// Inspect one uncaptured workspace file without changing the saved design.
    /// Captured, deleted, and moved paths always use the immutable plan snapshot.
    pub fn inspect(
        &self,
        workspace: &Path,
        path: Option<&str>,
        baseline: bool,
    ) -> Result<serde_json::Value> {
        if let Some(path) = path
            && path != PLAN_DOCUMENT_PATH
            && !self.baseline.contains_key(path)
            && !self.proposed.contains_key(path)
        {
            let (file, calls) = self.source(workspace, path)?;
            let text = super::calls::combined(path, &file.text, &calls)?;
            return Ok(
                serde_json::json!({"path":path,"side":"workspace","text":text,"source_digest":file.source_digest}),
            );
        }
        self.read(path, baseline)
    }

    /// Validate every proposed overview before exposing a persisted revision.
    pub fn validate(&self) -> Result<()> {
        self.document.validate()?;
        for (path, text) in &self.cargo_lock {
            validate_relative_path(path)?;
            ensure!(
                Path::new(path)
                    .file_name()
                    .is_some_and(|name| name == "Cargo.lock"),
                "invalid Cargo lockfile path: {path}"
            );
            toml::from_str::<toml::Value>(text).with_context(|| format!("parse {path}"))?;
        }
        for (path, file) in &self.baseline {
            validate_path(path)?;
            DeclarationOverview::parse(path, &file.text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("validate baseline {path}"))?;
        }
        for (path, text) in &self.proposed {
            validate_path(path)?;
            DeclarationOverview::parse(path, text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("validate {path}"))?;
        }
        for (files, baseline) in [(&self.baseline_calls, true), (&self.proposed_calls, false)] {
            for (path, calls) in files {
                let text = if baseline {
                    self.baseline.get(path).map(|file| &file.text)
                } else {
                    self.proposed.get(path)
                }
                .with_context(|| format!("call metadata has no declaration file: {path}"))?;
                let combined = super::calls::combined(path, text, calls)?;
                let (_, parsed) = super::calls::parse(path, &combined, calls)?;
                ensure!(
                    parsed.len() == calls.len(),
                    "call metadata has an absent or ambiguous callable: {path}"
                );
            }
        }
        for (from, to) in &self.moved {
            ensure!(
                self.baseline.contains_key(from)
                    && self.proposed.contains_key(to)
                    && !self.proposed.contains_key(from),
                "invalid declaration move {from} → {to}"
            );
        }
        Ok(())
    }

    /// Format both declaration snapshots atomically before freezing their review identity.
    pub fn formatted(&self) -> Result<Self> {
        self.validate()?;
        let mut candidate = self.clone();
        for (path, file) in &mut candidate.baseline {
            file.text = DeclarationOverview::format_with_width(path, &file.text, self.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("format baseline {path}"))?;
        }
        for (path, text) in &mut candidate.proposed {
            *text = DeclarationOverview::format_with_width(path, text, self.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("format proposed {path}"))?;
        }
        candidate.validate()?;
        if candidate
            .validation
            .as_ref()
            .is_some_and(|report| report.fingerprint != crate::declaration::fingerprint(&candidate))
        {
            candidate.validation = None;
        }
        Ok(candidate)
    }

    /// Return original paths whose declaration content or location changed.
    pub fn changed_paths(&self) -> Vec<String> {
        self.baseline
            .keys()
            .chain(self.proposed.keys())
            .cloned()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .filter(|path| {
                self.baseline.get(path).map(|file| file.text.as_str())
                    != self.proposed.get(path).map(String::as_str)
                    || self.baseline_calls.get(path) != self.proposed_calls.get(path)
            })
            .collect()
    }

    /// Reject submission against changed workspace sources or newly occupied destinations.
    pub fn check_workspace(&self, workspace: &Path) -> Result<()> {
        for path in self.changed_paths() {
            let target = workspace.join(&path);
            let source = match std::fs::read(&target) {
                Ok(source) => Some(source),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                Err(error) => return Err(error.into()),
            };
            match self.baseline.get(&path) {
                Some(before) => ensure!(
                    source
                        .as_ref()
                        .is_some_and(|source| super::digest(source) == before.source_digest),
                    "{path}: workspace changed since extraction. Start a fresh design baseline."
                ),
                None => ensure!(
                    source.is_none(),
                    "{path}: proposed destination already exists. Start a fresh design baseline."
                ),
            }
        }
        Ok(())
    }

    /// List project files represented by the plan without exposing internal metadata paths.
    pub(crate) fn file_inventory(&self) -> DesignFileInventory<'_> {
        let paths = self
            .baseline
            .keys()
            .chain(self.proposed.keys())
            .collect::<BTreeSet<_>>();
        DesignFileInventory {
            paths,
            changed_paths: self.changed_paths(),
        }
    }

    /// Read one virtual overview or list paths without sending the complete snapshot.
    pub fn read(&self, path: Option<&str>, baseline: bool) -> Result<serde_json::Value> {
        if let Some(path) = path {
            if path == PLAN_DOCUMENT_PATH {
                ensure!(!baseline, "plan.json has no source baseline");
                return Ok(
                    serde_json::json!({"path":path,"side":"proposed","text":format!("{}\n",super::codec::to_string_pretty(&self.document)?)}),
                );
            }
            validate_path(path)?;
            let text = if baseline {
                self.baseline.get(path).map(|file| &file.text)
            } else {
                self.proposed.get(path)
            }
            .with_context(|| format!("declaration file does not exist: {path}"))?;
            let calls = if baseline {
                self.baseline_calls.get(path)
            } else {
                self.proposed_calls.get(path)
            };
            let text =
                super::calls::combined(path, text, calls.map(Vec::as_slice).unwrap_or_default())?;
            Ok(
                serde_json::json!({"path":path,"side":if baseline {"baseline"} else {"proposed"},"text":text}),
            )
        } else {
            Ok(
                serde_json::json!({"paths":self.baseline.keys().chain(self.proposed.keys()).map(String::as_str).chain([PLAN_DOCUMENT_PATH]).collect::<BTreeSet<_>>(),"changed_paths":self.changed_paths()}),
            )
        }
    }

    /// Apply a familiar multi-file patch atomically to virtual proposed files.
    pub fn patch(
        &self,
        workspace: &Path,
        source_digests: &BTreeMap<String, String>,
        patch: &str,
    ) -> Result<Self> {
        let lines = patch.lines().collect::<Vec<_>>();
        ensure!(
            lines.first() == Some(&"*** Begin Patch") && lines.last() == Some(&"*** End Patch"),
            "patch must start with *** Begin Patch and end with *** End Patch"
        );
        let mut candidate = self.clone();
        candidate.validation = None;
        let mut position = 1;
        let mut edited = BTreeSet::new();
        while position < lines.len() - 1 {
            let operation = lines[position];
            position += 1;
            let (kind, path) = operation
                .strip_prefix("*** Add File: ")
                .map(|path| ("add", path))
                .or_else(|| {
                    operation
                        .strip_prefix("*** Update File: ")
                        .map(|path| ("update", path))
                })
                .or_else(|| {
                    operation
                        .strip_prefix("*** Delete File: ")
                        .map(|path| ("delete", path))
                })
                .context("expected Add File, Update File, or Delete File header")?;
            if path == PLAN_DOCUMENT_PATH {
                ensure!(
                    kind == "update",
                    "plan.json already exists and can only be updated"
                );
            } else {
                validate_path(path)?;
                if !candidate.proposed.contains_key(path) && !candidate.baseline.contains_key(path)
                {
                    if kind == "add" {
                        ensure!(
                            std::fs::symlink_metadata(workspace.join(path))
                                .is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound),
                            "{path}: Add File destination already exists or cannot be inspected"
                        );
                    } else {
                        let (file, calls) = candidate.source(workspace, path)?;
                        if let Some(expected) = source_digests.get(path) {
                            ensure!(
                                *expected == file.source_digest,
                                "{path}: workspace changed since inspection. Read the declaration file again."
                            );
                        }
                        candidate.proposed.insert(path.into(), file.text.clone());
                        if !calls.is_empty() {
                            candidate.baseline_calls.insert(path.into(), calls.clone());
                            candidate.proposed_calls.insert(path.into(), calls);
                        }
                        candidate.baseline.insert(path.into(), file);
                    }
                }
            }
            ensure!(
                edited.insert(path.to_owned()),
                "file appears more than once in patch: {path}"
            );
            let mut destination = path;
            if position < lines.len()
                && let Some(to) = lines[position].strip_prefix("*** Move to: ")
            {
                ensure!(kind == "update", "Move to requires Update File");
                ensure!(
                    path != PLAN_DOCUMENT_PATH && to != PLAN_DOCUMENT_PATH,
                    "plan.json cannot be moved"
                );
                validate_path(to)?;
                if !candidate.baseline.contains_key(to) {
                    ensure!(
                        std::fs::symlink_metadata(workspace.join(to))
                            .is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound),
                        "{to}: move destination already exists or cannot be inspected"
                    );
                }
                ensure!(
                    to != path
                        && !candidate.proposed.contains_key(to)
                        && (!candidate.baseline.contains_key(to)
                            || candidate
                                .moved
                                .get(to)
                                .is_some_and(|current| current == path))
                        && edited.insert(to.to_owned()),
                    "move destination already exists or is edited twice: {to}"
                );
                destination = to;
                position += 1;
            }
            let start = position;
            while position < lines.len() - 1
                && (!lines[position].starts_with("*** ") || lines[position] == "*** End of File")
            {
                position += 1;
            }
            let body = &lines[start..position];
            if path == PLAN_DOCUMENT_PATH {
                let original =
                    format!("{}\n", super::codec::to_string_pretty(&candidate.document)?);
                let text = patch_file(&original, body)?;
                candidate.document =
                    super::codec::from_str(&text).context("invalid plan.json document")?;
                continue;
            }
            match kind {
                "delete" => {
                    ensure!(body.is_empty(), "Delete File must have no patch body");
                    ensure!(
                        candidate.proposed.remove(path).is_some(),
                        "cannot delete absent declaration file: {path}"
                    );
                    candidate.moved.retain(|_, to| to != path);
                    candidate.proposed_calls.remove(path);
                }
                "add" => {
                    ensure!(
                        !candidate.proposed.contains_key(path),
                        "cannot add existing declaration file: {path}"
                    );
                    let mut text = String::new();
                    for line in body {
                        text.push_str(
                            line.strip_prefix('+')
                                .context("Add File lines must start with +")?,
                        );
                        text.push('\n');
                    }
                    let (text, calls) = super::calls::parse(path, &text, &[])?;
                    candidate.proposed.insert(path.into(), text);
                    if !calls.is_empty() {
                        candidate.proposed_calls.insert(path.into(), calls);
                    }
                }
                _ => {
                    let original = candidate.proposed.get(path).with_context(|| {
                        format!("cannot update absent declaration file: {path}")
                    })?;
                    let calls = candidate
                        .proposed_calls
                        .get(path)
                        .map(Vec::as_slice)
                        .unwrap_or_default();
                    let original = super::calls::combined(path, original, calls)?;
                    let text = patch_file(&original, body)?;
                    let (text, calls) = super::calls::parse(destination, &text, calls)?;
                    candidate.proposed.remove(path);
                    candidate.proposed_calls.remove(path);
                    candidate.proposed.insert(destination.into(), text);
                    if !calls.is_empty() {
                        candidate.proposed_calls.insert(destination.into(), calls);
                    }
                    if destination != path {
                        let original = candidate
                            .moved
                            .iter()
                            .find(|(_, to)| to.as_str() == path)
                            .map(|(from, _)| from.clone())
                            .unwrap_or_else(|| path.into());
                        if candidate.baseline.contains_key(&original) {
                            if destination == original {
                                candidate.moved.remove(&original);
                            } else {
                                candidate.moved.insert(original, destination.into());
                            }
                        }
                    }
                }
            }
        }
        ensure!(!edited.is_empty(), "patch has no file operations");
        if candidate
            .proposed_calls
            .values()
            .flatten()
            .flat_map(|function| function.call.iter().flatten())
            .any(|reference| {
                !reference.unresolved
                    && matches!(
                        reference.kind,
                        super::CallKind::Value | super::CallKind::Property | super::CallKind::Call
                    )
            })
        {
            let mut resolver =
                crate::declaration::DeclarationResolver::planned(workspace, &candidate, false)?;
            for (path, functions) in &mut candidate.proposed_calls {
                super::calls::classify(path, functions, &mut resolver);
            }
        }
        candidate.validate()?;
        if candidate.proposed == self.proposed
            && candidate.proposed_calls == self.proposed_calls
            && candidate.document == self.document
            && candidate.moved == self.moved
        {
            candidate.validation = self.validation.clone();
        }
        Ok(candidate)
    }
}

/// Carries an optimistic patch against proposed declaration files.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DesignPatchRequest {
    pub plan_id: String,
    pub expected_version: u64,
    pub patch: String,
    pub title: Option<String>,
    /// Checks inspected workspace identities when their baselines are first captured.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub source_digests: BTreeMap<String, String>,
}

fn validate_path(path: &str) -> Result<()> {
    validate_relative_path(path)?;
    ensure!(
        DeclarationOverview::supports(path),
        "unsupported declaration path: {path}. Supported files are .rs, .ts, .tsx, .lua, JSON/JSONC, TOML, YAML, XML, and .gitignore configuration."
    );
    Ok(())
}

/// Read workspace source after validating its path and regular-file identity.
pub(crate) fn workspace_source(workspace: &Path, path: &str) -> Result<Option<String>> {
    validate_relative_path(path)?;
    let target = workspace.join(path);
    let metadata = match std::fs::symlink_metadata(&target) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).with_context(|| format!("inspect {path}")),
    };
    ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "{path}: source is not a regular file"
    );
    ensure!(
        std::fs::canonicalize(&target)?.starts_with(std::fs::canonicalize(workspace)?),
        "{path}: source escapes workspace"
    );
    let source = std::fs::read_to_string(&target).with_context(|| format!("read {path}"))?;
    Ok(Some(source))
}

/// Rejects absolute and escaping paths before accessing project-relative plan data.
pub(super) fn validate_relative_path(path: &str) -> Result<()> {
    ensure!(
        !path.is_empty()
            && !path.contains(['\\', ':', '\n', '\r', '\0'])
            && path
                .split('/')
                .all(|part| !part.is_empty() && part != "." && part != ".."),
        "expected a project-relative declaration path: {path}"
    );
    ensure!(
        Path::new(path)
            .components()
            .all(|part| matches!(part, Component::Normal(_))),
        "path escapes declaration proposal: {path}"
    );
    Ok(())
}

fn patch_file(original: &str, patch: &[&str]) -> Result<String> {
    let mut text = original.lines().map(str::to_owned).collect::<Vec<_>>();
    let mut position = 0;
    let mut search_start = 0;
    while position < patch.len() {
        let header = patch[position];
        ensure!(
            header == "@@" || header.starts_with("@@ "),
            "update chunk must start with @@"
        );
        if let Some(context) = header
            .strip_prefix("@@ ")
            .filter(|context| !context.is_empty())
        {
            search_start = text
                .iter()
                .enumerate()
                .skip(search_start)
                .find(|(_, line)| line.as_str() == context)
                .map(|(index, _)| index + 1)
                .with_context(|| format!("patch context was not found: {context}"))?;
        }
        position += 1;
        let mut before = Vec::new();
        let mut after = Vec::new();
        while position < patch.len()
            && !patch[position].starts_with("@@")
            && patch[position] != "*** End of File"
        {
            let line = patch[position];
            match line.as_bytes().first() {
                Some(b' ') => {
                    before.push(line[1..].to_owned());
                    after.push(line[1..].to_owned());
                }
                Some(b'-') => before.push(line[1..].to_owned()),
                Some(b'+') => after.push(line[1..].to_owned()),
                _ => anyhow::bail!("patch line must start with space, +, or -"),
            }
            position += 1;
        }
        let at_end = position < patch.len() && patch[position] == "*** End of File";
        if at_end {
            position += 1;
        }
        let index = if before.is_empty() {
            text.len()
        } else {
            (search_start..=text.len().saturating_sub(before.len()))
                .find(|index| {
                    text.get(*index..*index + before.len()) == Some(before.as_slice())
                        && (!at_end || *index + before.len() == text.len())
                })
                .context("patch context does not match the proposed declaration file")?
        };
        search_start = index + after.len();
        text.splice(index..index + before.len(), after);
    }
    Ok(if text.is_empty() {
        String::new()
    } else {
        format!("{}\n", text.join("\n"))
    })
}
