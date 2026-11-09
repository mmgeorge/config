use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use super::design_document::DesignDocument;
use anyhow::{Context, Result, bail, ensure};
use forge_diff::syntax::DeclarationOverview;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Retains the immutable declaration source and workspace content identity.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationFile {
    pub text: String,
    pub source_digest: String,
    /// Holds the text `plan_read` shows for this baseline: its declarations with the captured
    /// function sections. A proposed file whose text equals it proposes no change.
    pub editable_text: String,
}

impl DeclarationFile {
    /// Create a baseline file whose editable text shows `calls` beneath their signatures.
    pub fn new(
        path: &str,
        text: String,
        source_digest: String,
        calls: &[super::FunctionBody],
    ) -> Result<Self> {
        let editable_text = super::calls::combined(path, &text, calls)?;
        Ok(Self {
            text,
            source_digest,
            editable_text,
        })
    }
}

/// Borrow the original and proposed paths while owning the computed change list.
#[derive(Serialize)]
pub(crate) struct DesignFileInventory<'a> {
    paths: BTreeSet<&'a String>,
    changed_paths: Vec<String>,
}

type SourceOverviewCache = std::sync::Mutex<BTreeMap<String, String>>;
static SOURCE_OVERVIEW_CACHE: std::sync::OnceLock<SourceOverviewCache> = std::sync::OnceLock::new();

/// Owns baseline declarations and the agent's proposed file contents.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationDesign {
    /// Bind a submitted planning directory to its immutable raw input snapshot.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub authoring_fingerprint: Option<String>,
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
    #[serde(default)]
    pub document: DesignDocument,
    /// Holds the agent's text for each authored `plan:` artifact it has edited, keyed by
    /// artifact path. Submission parses this text into `document`; an artifact without an
    /// entry renders from `document`.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub artifact_text: BTreeMap<String, String>,
    #[serde(default = "default_line_width")]
    pub line_width: usize,
    pub baseline: BTreeMap<String, DeclarationFile>,
    pub moved: BTreeMap<String, String>,
    /// Function metadata captured with each source baseline, retaining reference extraction order.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub baseline_calls: BTreeMap<String, Vec<super::FunctionBody>>,
    /// Holds the text of every proposed project file, keyed by proposed path, as the last plan
    /// edit or review rename left it. Source files contain declarations and function sections;
    /// configuration files contain complete contents.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub proposed_source: BTreeMap<String, String>,
    /// Holds the structure that the last submission parsed from `proposed_source`. Drafts have
    /// none, and every effective plan edit removes it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parsed: Option<ParsedDeclarations>,
}

/// Holds declarations and function bodies derived from one exact `proposed_source`.
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct ParsedDeclarations {
    /// Identifies the plan inputs this structure was derived from.
    pub source_fingerprint: String,
    /// Declarations of each proposed file without function sections, keyed by proposed path.
    pub proposed: BTreeMap<String, String>,
    /// Function changes and reference relationships accompanying proposed declarations.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub proposed_calls: BTreeMap<String, Vec<super::FunctionBody>>,
    /// Reference evidence generated for exactly this structure.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub validation: Option<crate::declaration::DeclarationValidation>,
}

fn default_line_width() -> usize {
    80
}

impl Default for DeclarationDesign {
    fn default() -> Self {
        Self {
            authoring_fingerprint: None,
            check_preview: Vec::new(),
            check_preview_source: String::new(),
            check_configuration: None,
            cargo_lock: BTreeMap::new(),
            document: DesignDocument::default(),
            artifact_text: BTreeMap::new(),
            line_width: default_line_width(),
            baseline: BTreeMap::new(),
            moved: BTreeMap::new(),
            baseline_calls: BTreeMap::new(),
            proposed_source: BTreeMap::new(),
            parsed: None,
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
        self.design
            .parsed
            .as_mut()
            .expect("validated_revision stores the parsed declarations it validated")
            .validation = Some(self.report);
        self.design
    }
}

impl DeclarationDesign {
    /// Validate one submission using only captured declaration evidence.
    pub(crate) async fn validated(&self, workspace: &Path, title: &str) -> Result<ValidatedDesign> {
        self.check_workspace(workspace)?;
        self.validated_revision(workspace, title).await
    }

    /// Collect every rule the planning model must satisfy in authored declarations: comment
    /// wording and documentation. The design keeps the agent's layout, so every
    /// position names a line the agent can read and patch.
    pub(super) fn authored_diagnostics(
        &self,
        workspace: &Path,
    ) -> Result<Vec<crate::declaration::DeclarationDiagnostic>> {
        let index = super::references::PlanReferenceIndex::design(self, workspace)?;
        let mut authored = super::comment_lint::diagnostics(self)?;
        authored.extend(super::documentation::diagnostics(self, &index)?);
        Ok(authored)
    }

    /// Validate a new target without replacing the immutable pre-execution baseline.
    ///
    /// The authored artifacts are parsed here, the only path from their text into `document`,
    /// and their diagnostics join the declaration diagnostics in one rejection. Declarations
    /// keep the agent's layout; only presentation formats them. A proposed file whose structure
    /// does not parse reports its line together with the artifact problems, because reference
    /// rules need the complete structure.
    pub(crate) async fn validated_revision(
        &self,
        workspace: &Path,
        title: &str,
    ) -> Result<ValidatedDesign> {
        self.validate()?;
        let mut design = self.clone();
        let (parsed, structure) = design.derive_parsed()?;
        design.parsed = Some(parsed);
        let configuration = crate::checks::ConfigSnapshot::load(workspace)?;
        let artifacts =
            super::design_artifact::parse_authored(title, &design, &configuration.config)?;
        if !structure.is_empty() {
            crate::declaration::DeclarationValidationError::ensure_empty(
                structure
                    .into_iter()
                    .chain(artifacts.diagnostic.iter().cloned())
                    .collect(),
            )?;
        }
        let artifact_errors = artifacts
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.error);
        if !artifact_errors {
            design.document = artifacts.document;
        }
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
        // every declaration failure instead of one rule family per submission.
        let mut authored = design.authored_diagnostics(workspace)?;
        authored.extend(artifacts.diagnostic);
        let paths = design
            .proposed_source
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
        authored.extend(super::calls::validate_behaviors(&design)?);
        let report = resolver.validate(&design)?;
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
            .proposed_source
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

    /// Capture one workspace file's baseline without changing the saved design.
    pub(crate) fn source(
        &self,
        workspace: &Path,
        path: &str,
    ) -> Result<(DeclarationFile, Vec<super::FunctionBody>)> {
        validate_path(path)?;
        let source = workspace_source(workspace, path)?
            .with_context(|| format!("declaration file does not exist: {path}"))?;
        self.capture_baseline(path, &source)
    }

    /// Extract one file's baseline declarations from its source text.
    ///
    /// Planning-time capture and Resolve-phase recapture from an execution checkpoint both use
    /// this, so their baselines agree. A baseline holds no behavior blocks, so its function
    /// list is empty.
    pub(crate) fn capture_baseline(
        &self,
        path: &str,
        source: &str,
    ) -> Result<(DeclarationFile, Vec<super::FunctionBody>)> {
        let source_digest = super::digest(source.as_bytes());
        let key = format!("{path}:{}:{source_digest}", self.line_width);
        let cache = SOURCE_OVERVIEW_CACHE.get_or_init(Default::default);
        let cached = cache
            .lock()
            .map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?
            .get(&key)
            .cloned();
        let text = if let Some(text) = cached {
            text
        } else {
            let text = DeclarationOverview::extract(path, source)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("extract {path}"))?;
            let text = DeclarationOverview::format_with_width(path, &text, self.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let mut cache = cache
                .lock()
                .map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?;
            let bytes = cache.values().map(String::len).sum::<usize>();
            if cache.len() >= 16 || bytes >= 8 * 1024 * 1024 {
                cache.clear();
            }
            cache.insert(key, text.clone());
            text
        };
        Ok((
            DeclarationFile::new(path, text, source_digest, &[])?,
            Vec::new(),
        ))
    }

    /// Inspect one uncaptured workspace file without changing the saved design.
    /// Captured, deleted, and moved paths always use the immutable plan snapshot.
    pub fn inspect(
        &self,
        workspace: &Path,
        path: &str,
        baseline: bool,
    ) -> Result<serde_json::Value> {
        if !self.baseline.contains_key(path) && !self.proposed_source.contains_key(path) {
            let (file, _) = self.source(workspace, path)?;
            return Ok(
                serde_json::json!({"path":path,"side":"workspace","text":file.editable_text,"source_digest":file.source_digest}),
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
        for path in self.proposed_source.keys() {
            validate_path(path)?;
        }
        for (path, calls) in &self.baseline_calls {
            let text = &self
                .baseline
                .get(path)
                .with_context(|| format!("call metadata has no declaration file: {path}"))?
                .text;
            validate_calls(path, text, calls)?;
        }
        for (from, to) in &self.moved {
            ensure!(
                self.baseline.contains_key(from)
                    && self.proposed_source.contains_key(to)
                    && !self.proposed_source.contains_key(from),
                "invalid declaration move {from} → {to}"
            );
        }
        if let Some(parsed) = &self.parsed {
            ensure!(
                parsed.source_fingerprint == self.source_fingerprint()?,
                "parsed declarations do not match the proposed source"
            );
            ensure!(
                parsed.proposed.keys().eq(self.proposed_source.keys()),
                "parsed declarations do not cover the proposed files"
            );
            for (path, text) in &parsed.proposed {
                DeclarationOverview::parse(path, text)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))
                    .with_context(|| format!("validate {path}"))?;
            }
            for (path, calls) in &parsed.proposed_calls {
                let text = parsed
                    .proposed
                    .get(path)
                    .with_context(|| format!("call metadata has no declaration file: {path}"))?;
                validate_calls(path, text, calls)?;
            }
        }
        Ok(())
    }

    /// Get the structure the last submission parsed from the proposed source.
    pub(crate) fn parsed(&self) -> Result<&ParsedDeclarations> {
        self.parsed
            .as_ref()
            .context("plan has no submitted declaration structure; submit it first")
    }

    /// Get mutable access to the submitted structure for edits that keep it current.
    pub(crate) fn parsed_mut(&mut self) -> Result<&mut ParsedDeclarations> {
        self.parsed
            .as_mut()
            .context("plan has no submitted declaration structure; submit it first")
    }

    /// Store one file's parsed declarations and regenerate its proposed source from them.
    ///
    /// Review edits that change parsed structure directly, such as a symbol rename, use this so
    /// the proposed source and the parsed structure keep describing each other. Every other
    /// proposed file must already be parsed. The validation report no longer applies.
    pub fn parsed_insert(
        &mut self,
        path: &str,
        declarations: String,
        calls: Vec<super::FunctionBody>,
    ) -> Result<()> {
        let text = super::calls::combined(path, &declarations, &calls)?;
        self.proposed_source.insert(path.into(), text);
        let mut parsed = self.parsed.take().unwrap_or_default();
        parsed.proposed.insert(path.into(), declarations);
        if calls.is_empty() {
            parsed.proposed_calls.remove(path);
        } else {
            parsed.proposed_calls.insert(path.into(), calls);
        }
        parsed.validation = None;
        parsed.source_fingerprint = self.source_fingerprint()?;
        self.parsed = Some(parsed);
        Ok(())
    }

    /// Identify the plan inputs that parsed declarations are derived from.
    pub(crate) fn source_fingerprint(&self) -> Result<String> {
        Ok(super::digest(&super::codec::to_vec(&(
            &self.proposed_source,
            &self.moved,
            &self.baseline,
            &self.baseline_calls,
        ))?))
    }

    /// Create a design whose proposed source and parsed declarations are the given texts, for
    /// callers that need a design only as input to declaration indexing.
    pub(crate) fn scratch(proposed: BTreeMap<String, String>) -> Result<Self> {
        let mut design = Self {
            proposed_source: proposed.clone(),
            ..Self::default()
        };
        design.parsed = Some(ParsedDeclarations {
            source_fingerprint: design.source_fingerprint()?,
            proposed,
            ..ParsedDeclarations::default()
        });
        Ok(design)
    }

    /// Parse every proposed file against its baseline evidence.
    ///
    /// Each file is matched against the function metadata captured from its original path, so
    /// captured source positions survive any sequence of edits that keeps a reference. A file
    /// whose structure does not parse becomes a diagnostic at its line, and the returned
    /// structure omits it.
    fn derive_parsed(
        &self,
    ) -> Result<(
        ParsedDeclarations,
        Vec<crate::declaration::DeclarationDiagnostic>,
    )> {
        let mut parsed = ParsedDeclarations {
            source_fingerprint: self.source_fingerprint()?,
            ..ParsedDeclarations::default()
        };
        let mut diagnostic = Vec::new();
        for (path, text) in &self.proposed_source {
            match super::calls::parse(path, text) {
                Ok((declarations, calls)) => {
                    parsed.proposed.insert(path.clone(), declarations);
                    if !calls.is_empty() {
                        parsed.proposed_calls.insert(path.clone(), calls);
                    }
                }
                Err(error) => {
                    let (line, reason) =
                        match error.downcast_ref::<super::calls::DeclarationStructureError>() {
                            Some(structure) => (structure.line, structure.message.clone()),
                            None => (1, format!("{error:#}")),
                        };
                    diagnostic.push(crate::declaration::DeclarationDiagnostic {
                        path: path.clone(),
                        line: u32::try_from(line)?,
                        column: 0,
                        reference: text
                            .lines()
                            .nth(line.saturating_sub(1))
                            .unwrap_or_default()
                            .trim()
                            .into(),
                        error: true,
                        reason,
                        kind: crate::declaration::DeclarationDiagnosticKind::DeclarationStructure,
                    });
                }
            }
        }
        Ok((parsed, diagnostic))
    }

    /// Return original paths whose declaration content or location changed.
    pub fn changed_paths(&self) -> Vec<String> {
        self.baseline
            .keys()
            .chain(self.proposed_source.keys())
            .cloned()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .filter(|path| {
                self.baseline
                    .get(path)
                    .map(|file| file.editable_text.as_str())
                    != self.proposed_source.get(path).map(String::as_str)
            })
            .collect()
    }

    /// Reject submission against changed workspace sources or newly occupied destinations.
    pub fn check_workspace(&self, workspace: &Path) -> Result<()> {
        let paths = if self.authoring_fingerprint.is_some() {
            self.baseline
                .keys()
                .chain(self.proposed_source.keys())
                .cloned()
                .collect::<BTreeSet<_>>()
        } else {
            self.changed_paths().into_iter().collect()
        };
        for path in paths {
            let source = workspace_source(workspace, &path)?.map(String::into_bytes);
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
            .chain(self.proposed_source.keys())
            .collect::<BTreeSet<_>>();
        DesignFileInventory {
            paths,
            changed_paths: self.changed_paths(),
        }
    }

    /// Read one captured file's baseline or proposed text as `plan_read` shows it.
    pub fn read(&self, path: &str, baseline: bool) -> Result<serde_json::Value> {
        validate_path(path)?;
        let text = if baseline {
            self.baseline.get(path).map(|file| &file.editable_text)
        } else {
            self.proposed_source.get(path)
        }
        .with_context(|| format!("declaration file does not exist: {path}"))?;
        Ok(
            serde_json::json!({"path":path,"side":if baseline {"baseline"} else {"proposed"},"text":text}),
        )
    }

    /// Withdraw every proposal for one project file, including a move, so it reads from the
    /// workspace again.
    fn revert_file(&mut self, path: &str) -> Result<()> {
        validate_path(path)?;
        let original = self
            .moved
            .iter()
            .find(|(_, to)| to.as_str() == path)
            .map(|(from, _)| from.clone())
            .unwrap_or_else(|| path.into());
        ensure!(
            self.baseline.contains_key(&original) || self.proposed_source.contains_key(path),
            "{path}: the plan proposes no change to this file"
        );
        if let Some(destination) = self.moved.remove(&original) {
            self.proposed_source.remove(&destination);
        }
        self.baseline.remove(&original);
        self.baseline_calls.remove(&original);
        self.proposed_source.remove(&original);
        Ok(())
    }

    /// Apply a familiar multi-file patch atomically to virtual proposed files and plan artifacts.
    ///
    /// Artifact text is stored as written; `plan_submit` reports its problems. The overview
    /// artifact carries the plan title, so the patch receives the current title and returns the
    /// title its `# Title` line leaves.
    pub fn patch(
        &self,
        workspace: &Path,
        source_digests: &BTreeMap<String, String>,
        patch: &str,
        title: &str,
    ) -> Result<(Self, String)> {
        use super::design_artifact::{self, PlanArtifact};
        let lines = patch.lines().collect::<Vec<_>>();
        ensure!(
            lines.first() == Some(&"*** Begin Patch") && lines.last() == Some(&"*** End Patch"),
            "patch must start with *** Begin Patch and end with *** End Patch"
        );
        let mut candidate = self.clone();
        let mut title = title.to_owned();
        candidate.parsed = None;
        let mut position = 1;
        let mut edited = BTreeSet::new();
        let body_end = |position: usize| {
            let mut end = position;
            while end < lines.len() - 1
                && (!lines[end].starts_with("*** ") || lines[end] == "*** End of File")
            {
                end += 1;
            }
            end
        };
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
                .or_else(|| {
                    operation
                        .strip_prefix("*** Revert File: ")
                        .map(|path| ("revert", path))
                })
                .context("expected Add File, Update File, Delete File, or Revert File header")?;
            if let Some(artifact) = PlanArtifact::from_path(path)? {
                ensure!(
                    artifact.authored(),
                    "{path} is read-only; Harness generates it from execution state"
                );
                // An artifact has no baseline or move bookkeeping, so its operations compose in
                // patch order: Delete then Add replaces it, and repeated Update chunks stack.
                edited.insert(path.to_owned());
                ensure!(
                    !lines[position].starts_with("*** Move to: "),
                    "{path} cannot be moved"
                );
                let end = body_end(position);
                let body = &lines[position..end];
                position = end;
                match kind {
                    "delete" => {
                        ensure!(body.is_empty(), "Delete File must have no patch body");
                        // Empty text stays a draft; plan_submit reports any section it requires.
                        design_artifact::store_authored(
                            artifact,
                            String::new(),
                            &mut title,
                            &mut candidate,
                        )?;
                    }
                    "revert" => {
                        ensure!(body.is_empty(), "Revert File must have no patch body");
                        design_artifact::revert_authored(artifact, &mut candidate);
                    }
                    _ => {
                        let patched = if kind == "add" {
                            added_text(body)?
                        } else {
                            let original =
                                design_artifact::authored_text(artifact, &title, &candidate)?;
                            patch_file(path, &original, body)?
                        };
                        design_artifact::store_authored(
                            artifact,
                            patched,
                            &mut title,
                            &mut candidate,
                        )?;
                    }
                }
                continue;
            }
            if kind == "revert" {
                ensure!(
                    edited.insert(path.to_owned()),
                    "file appears more than once in patch: {path}"
                );
                let end = body_end(position);
                ensure!(end == position, "Revert File must have no patch body");
                candidate.revert_file(path)?;
                continue;
            }
            validate_path(path)?;
            if !candidate.proposed_source.contains_key(path)
                && !candidate.baseline.contains_key(path)
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
                    candidate
                        .proposed_source
                        .insert(path.into(), file.editable_text.clone());
                    if !calls.is_empty() {
                        candidate.baseline_calls.insert(path.into(), calls);
                    }
                    candidate.baseline.insert(path.into(), file);
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
                    PlanArtifact::from_path(to)?.is_none(),
                    "a project file cannot move into a plan artifact path: {to}"
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
                        && !candidate.proposed_source.contains_key(to)
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
            position = body_end(position);
            let body = &lines[start..position];
            match kind {
                "delete" => {
                    ensure!(body.is_empty(), "Delete File must have no patch body");
                    ensure!(
                        candidate.proposed_source.remove(path).is_some(),
                        "cannot delete absent declaration file: {path}"
                    );
                    candidate.moved.retain(|_, to| to != path);
                }
                "add" => {
                    ensure!(
                        !candidate.proposed_source.contains_key(path),
                        "cannot add existing declaration file: {path}"
                    );
                    let text = added_text(body)?;
                    validate_configuration_text(path, &text)?;
                    candidate.proposed_source.insert(path.into(), text);
                }
                _ => {
                    let original = candidate.proposed_source.remove(path).with_context(|| {
                        format!("cannot update absent declaration file: {path}")
                    })?;
                    let text = patch_file(path, &original, body)?;
                    validate_configuration_text(destination, &text)?;
                    candidate.proposed_source.insert(destination.into(), text);
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
        candidate.validate()?;
        if candidate.proposed_source == self.proposed_source
            && candidate.baseline == self.baseline
            && candidate.baseline_calls == self.baseline_calls
            && candidate.document == self.document
            && candidate.artifact_text == self.artifact_text
            && candidate.moved == self.moved
        {
            candidate.parsed = self.parsed.clone();
        }
        Ok((candidate, title))
    }
}

/// Apply a patch that may touch only `path` to that file's current text.
///
/// Verify edits exactly one execution artifact, so this rejects any other file, a move, a delete,
/// or a revert instead of silently dropping it.
pub(crate) fn patch_single_file(patch: &str, path: &str, original: &str) -> Result<String> {
    let lines = patch.lines().collect::<Vec<_>>();
    ensure!(
        lines.len() >= 3
            && lines.first() == Some(&"*** Begin Patch")
            && lines.last() == Some(&"*** End Patch"),
        "patch must start with *** Begin Patch and end with *** End Patch"
    );
    let body = &lines[2..lines.len() - 1];
    ensure!(
        body.iter()
            .all(|line| !line.starts_with("*** ") || *line == "*** End of File"),
        "this patch may contain only one Update File or Add File section for {path}"
    );
    if lines[1] == format!("*** Update File: {path}") {
        patch_file(path, original, body)
    } else if lines[1] == format!("*** Add File: {path}") {
        added_text(body)
    } else {
        bail!("this patch may edit only {path}, with *** Update File or *** Add File")
    }
}

/// Report whether `path` holds configuration, whose complete contents are parsed on every edit.
fn is_configuration(path: &str) -> bool {
    forge_diff::syntax::ConfigurationFormat::for_path(path).is_some()
}

/// Reject configuration text that does not parse, so an invalid value never reaches the plan.
///
/// Source files are stored as written; `plan_submit` reports their structure problems.
fn validate_configuration_text(path: &str, text: &str) -> Result<()> {
    if is_configuration(path) {
        DeclarationOverview::parse(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    }
    Ok(())
}

/// Check that captured or parsed function metadata belongs to exactly the callables of `text`.
fn validate_calls(path: &str, text: &str, calls: &[super::FunctionBody]) -> Result<()> {
    let combined = super::calls::combined(path, text, calls)?;
    let (_, parsed) = super::calls::parse(path, &combined)?;
    ensure!(
        parsed.len() == calls.len(),
        "call metadata has an absent or ambiguous callable: {path}"
    );
    Ok(())
}

/// Collect the text of an Add File body, whose every line starts with `+`.
fn added_text(body: &[&str]) -> Result<String> {
    let mut text = String::new();
    for line in body {
        text.push_str(
            line.strip_prefix('+')
                .context("Add File lines must start with +")?,
        );
        text.push('\n');
    }
    Ok(text)
}

/// Carries an optimistic patch against proposed project files and plan artifacts.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DesignPatchRequest {
    pub plan_id: String,
    pub expected_version: u64,
    pub patch: String,
    /// Checks inspected workspace identities when their baselines are first captured.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub source_digests: BTreeMap<String, String>,
}

pub(super) fn validate_path(path: &str) -> Result<()> {
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

/// Apply context chunks to `original`, the text `plan_read` shows for `path`.
///
/// A chunk applies where its removed and context lines match exactly. Failing that, it applies
/// at the one place where every nonblank line matches after shifting the whole chunk by the same
/// number of spaces, and its added lines shift with it. A uniform shift keeps relative nesting,
/// such as an Algorithm statement inside its loop, so a chunk copied at the wrong base indent
/// still lands with the structure its author wrote. A mismatch names the first chunk line the
/// file lacks.
fn patch_file(path: &str, original: &str, patch: &[&str]) -> Result<String> {
    let mut text = original.lines().map(str::to_owned).collect::<Vec<_>>();
    let mut position = 0;
    let mut search_start = 0;
    let mut chunk = 0;
    while position < patch.len() {
        chunk += 1;
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
        let candidates = search_start..=text.len().saturating_sub(before.len());
        let fits_end = |index: usize| !at_end || index + before.len() == text.len();
        let (index, after) = if before.is_empty() {
            (text.len(), after)
        } else if let Some(index) = candidates.clone().find(|index| {
            text.get(*index..*index + before.len()) == Some(before.as_slice()) && fits_end(*index)
        }) {
            (index, after)
        } else {
            let shifted = candidates
                .filter(|index| index + before.len() <= text.len() && fits_end(*index))
                .filter_map(|index| {
                    indentation_shift(&before, &text[index..index + before.len()])
                        .map(|shift| (index, shift))
                })
                .collect::<Vec<_>>();
            let [(index, shift)] = shifted.as_slice() else {
                bail!(context_mismatch(path, chunk, &text, search_start, &before));
            };
            let after = after
                .iter()
                .map(|line| {
                    shift_indentation(line, *shift).with_context(|| {
                        format!(
                            "{path}: chunk {chunk} matches {} columns {}, but added line `{line}` has too little indentation to move with it",
                            shift.unsigned_abs(),
                            if *shift > 0 { "deeper" } else { "shallower" }
                        )
                    })
                })
                .collect::<Result<Vec<_>>>()?;
            (*index, after)
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

fn leading_spaces(line: &str) -> usize {
    line.len() - line.trim_start_matches(' ').len()
}

/// Move `line` by `shift` columns of spaces, leaving blank lines blank.
fn shift_indentation(line: &str, shift: isize) -> Option<String> {
    if line.trim().is_empty() {
        return Some(line.to_owned());
    }
    if shift >= 0 {
        return Some(format!("{}{line}", " ".repeat(shift.unsigned_abs())));
    }
    let removed = shift.unsigned_abs();
    (leading_spaces(line) >= removed).then(|| line[removed..].to_owned())
}

/// Return the nonzero space shift that makes every chunk line equal its target, if one exists.
fn indentation_shift(chunk: &[String], target: &[String]) -> Option<isize> {
    let (first, anchor) = chunk
        .iter()
        .zip(target)
        .find(|(line, _)| !line.trim().is_empty())?;
    let shift = leading_spaces(anchor) as isize - leading_spaces(first) as isize;
    (shift != 0
        && chunk.iter().zip(target).all(|(line, target)| {
            if line.trim().is_empty() {
                target.trim().is_empty()
            } else {
                shift_indentation(line, shift).as_deref() == Some(target.as_str())
            }
        }))
    .then_some(shift)
}

/// Describe why a chunk matched nowhere: the first chunk line that does not continue the lines
/// before it, and where the file holds that text at another indentation.
fn context_mismatch(
    path: &str,
    chunk: usize,
    text: &[String],
    search_start: usize,
    before: &[String],
) -> String {
    let matches_prefix = |length: usize| {
        (search_start..=text.len().saturating_sub(length))
            .any(|index| text.get(index..index + length) == Some(&before[..length]))
    };
    let missing = (1..=before.len())
        .find(|length| !matches_prefix(*length))
        .unwrap_or(before.len())
        - 1;
    let line = &before[missing];
    let mut message = if missing == 0 {
        format!(
            "{path}: chunk {chunk} does not match: its first line `{line}` is not in the file{}",
            if search_start > 0 {
                " after the previous chunk"
            } else {
                ""
            }
        )
    } else {
        format!(
            "{path}: chunk {chunk} does not match: its line {} `{line}` does not follow the line before it in the file",
            missing + 1
        )
    };
    if let Some(row) = text.iter().position(|candidate| {
        candidate != line && candidate.trim() == line.trim() && !line.trim().is_empty()
    }) {
        message.push_str(&format!(
            "; file line {} holds that text with {} leading spaces instead of {}",
            row + 1,
            leading_spaces(&text[row]),
            leading_spaces(line)
        ));
    }
    message.push_str(". Read the file again and copy context lines exactly.");
    message
}

#[cfg(test)]
mod tests {
    use super::*;

    const GAME: &str = "/// Help once.\nfn helper() {}\n/// Run the helper.\nfn run() {\n    helper();\n}\n/// Walk with the helper.\nfn walk() {\n    helper();\n}\n";

    fn workspace() -> tempfile::TempDir {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("Game.rs"), GAME).unwrap();
        workspace
    }

    fn patched(design: &DeclarationDesign, workspace: &Path, patch: &str) -> DeclarationDesign {
        design
            .patch(workspace, &BTreeMap::new(), patch, "Rounds")
            .unwrap()
            .0
    }

    /// Capture `Game.rs` without changing its text.
    fn captured(workspace: &Path) -> DeclarationDesign {
        let design = DeclarationDesign::open(workspace).unwrap();
        patched(
            &design,
            workspace,
            "*** Begin Patch\n*** Update File: Game.rs\n@@\n-fn helper();\n+fn helper();\n*** End Patch",
        )
    }

    #[test]
    fn unedited_capture_submits_the_baseline_unchanged() {
        let workspace = workspace();
        let design = captured(workspace.path());
        assert!(design.changed_paths().is_empty());
        assert_eq!(
            design.proposed_source["Game.rs"],
            design.baseline["Game.rs"].editable_text
        );
        let (parsed, diagnostic) = design.derive_parsed().unwrap();
        assert!(diagnostic.is_empty());
        assert_eq!(parsed.proposed["Game.rs"], design.baseline["Game.rs"].text);
        assert!(!parsed.proposed_calls.contains_key("Game.rs"));
        assert!(!design.baseline_calls.contains_key("Game.rs"));
    }

    #[test]
    fn parsed_files_store_only_written_behaviors() {
        let workspace = workspace();
        let design = captured(workspace.path());
        assert!(
            !design.proposed_source["Game.rs"].contains("Calls"),
            "{}",
            design.proposed_source["Game.rs"]
        );
        assert!(!design.baseline_calls.contains_key("Game.rs"));
        let changed = patched(
            &design,
            workspace.path(),
            "*** Begin Patch\n*** Update File: Game.rs\n@@\n fn run();\n+Change\n+  Run the helper twice.\n*** End Patch",
        );
        let (parsed, diagnostic) = changed.derive_parsed().unwrap();
        assert!(diagnostic.is_empty());
        let [run] = &parsed.proposed_calls["Game.rs"][..] else {
            panic!("{:?}", parsed.proposed_calls["Game.rs"]);
        };
        assert_eq!(run.owner, "run");
        assert!(matches!(
            run.behavior,
            Some(super::super::FunctionBehavior::Change(_))
        ));
    }

    #[test]
    fn written_reference_sections_are_rejected() {
        let workspace = workspace();
        let design = captured(workspace.path());
        let written = patched(
            &design,
            workspace.path(),
            "*** Begin Patch\n*** Update File: Game.rs\n@@\n fn run();\n+Calls\n+  helper\n*** End Patch",
        );
        let (_, diagnostic) = written.derive_parsed().unwrap();
        assert_eq!(diagnostic.len(), 1, "{diagnostic:?}");
        assert!(
            diagnostic[0]
                .reason
                .contains("Calls sections are not written in plans"),
            "{}",
            diagnostic[0].reason
        );
    }

    #[test]
    fn plan_edit_removes_submitted_structure_and_a_failed_edit_keeps_it() {
        let workspace = workspace();
        let mut design = captured(workspace.path());
        let (parsed, _) = design.derive_parsed().unwrap();
        design.parsed = Some(parsed);
        design.validate().unwrap();

        let failed = design.patch(
            workspace.path(),
            &BTreeMap::new(),
            "*** Begin Patch\n*** Update File: Game.rs\n@@\n-fn absent();\n+fn present();\n*** End Patch",
            "Rounds",
        );
        assert!(failed.is_err());
        assert!(
            design.parsed().is_ok(),
            "a failed edit leaves the design as it was"
        );

        let edited = patched(
            &design,
            workspace.path(),
            "*** Begin Patch\n*** Add File: plan:overview.md\n+# Rounds\n+\n+## Objective\n+Play.\n*** End Patch",
        );
        assert!(edited.parsed.is_none());
        assert!(edited.parsed().is_err());
    }

    #[test]
    fn artifact_operations_compose_in_patch_order() {
        let workspace = workspace();
        let design = captured(workspace.path());
        let overview = |design: &DeclarationDesign| {
            design
                .artifact_text
                .get("plan:overview.md")
                .cloned()
                .unwrap()
        };

        let replaced = patched(
            &design,
            workspace.path(),
            "*** Begin Patch\n*** Delete File: plan:overview.md\n*** Add File: plan:overview.md\n+# Rounds\n+\n+## Objective\n+Play.\n*** End Patch",
        );
        assert_eq!(overview(&replaced), "# Rounds\n\n## Objective\nPlay.\n");

        let stacked = patched(
            &replaced,
            workspace.path(),
            "*** Begin Patch\n*** Update File: plan:overview.md\n@@\n-Play.\n+Play twice.\n*** Update File: plan:overview.md\n@@\n-Play twice.\n+Play three times.\n*** End Patch",
        );
        assert_eq!(
            overview(&stacked),
            "# Rounds\n\n## Objective\nPlay three times.\n"
        );

        let cleared = patched(
            &stacked,
            workspace.path(),
            "*** Begin Patch\n*** Delete File: plan:overview.md\n*** End Patch",
        );
        assert_eq!(overview(&cleared), "");

        let reverted = patched(
            &cleared,
            workspace.path(),
            "*** Begin Patch\n*** Revert File: plan:overview.md\n*** End Patch",
        );
        assert!(!reverted.artifact_text.contains_key("plan:overview.md"));
    }

    #[test]
    fn a_project_file_still_appears_once_per_patch() {
        let workspace = workspace();
        let design = captured(workspace.path());
        let error = design
            .patch(
                workspace.path(),
                &BTreeMap::new(),
                "*** Begin Patch\n*** Update File: Game.rs\n@@\n fn run();\n+Change\n+  Run twice.\n*** Update File: Game.rs\n@@\n fn walk();\n+Change\n+  Walk twice.\n*** End Patch",
                "Rounds",
            )
            .unwrap_err();
        assert!(
            format!("{error:#}").contains("file appears more than once in patch: Game.rs"),
            "{error:#}"
        );
    }

    #[test]
    fn stale_parsed_structure_is_rejected() {
        let workspace = workspace();
        let mut design = captured(workspace.path());
        let (parsed, _) = design.derive_parsed().unwrap();
        design.parsed = Some(parsed);
        design.validate().unwrap();
        design
            .proposed_source
            .get_mut("Game.rs")
            .unwrap()
            .push_str("/// Added later.\nfn later();\n");
        let error = design.validate().unwrap_err();
        assert!(
            format!("{error:#}").contains("parsed declarations do not match the proposed source"),
            "{error:#}"
        );
    }

    #[test]
    fn review_rename_regenerates_the_renamed_proposed_source() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = patched(
            &DeclarationDesign::open(workspace.path()).unwrap(),
            workspace.path(),
            "*** Begin Patch\n*** Add File: Rounds.rs\n+/// Score one round.\n+fn score(round: u32) -> u32;\n+Algorithm\n+  return round\n+\n+/// Play every round.\n+fn play(rounds: u32);\n+Algorithm\n+  for each round in rounds\n+    score(round)\n*** End Patch",
        );
        let (parsed, diagnostic) = design.derive_parsed().unwrap();
        assert!(diagnostic.is_empty());
        design.parsed = Some(parsed);
        let mut document = crate::plan::test_fixture("plan", "Rounds");
        document.design = Some(design.clone());
        let index =
            super::super::references::PlanReferenceIndex::planned(&document, workspace.path())
                .unwrap();
        let score_line = u32::try_from(
            design.parsed().unwrap().proposed["Rounds.rs"]
                .lines()
                .position(|line| line.starts_with("fn score("))
                .unwrap()
                + 1,
        )
        .unwrap();
        let identity = index
            .definition
            .iter()
            .find(|((path, line, _), _)| path == "Rounds.rs" && *line == score_line)
            .map(|(_, (identity, _))| identity.clone())
            .unwrap();

        let renamed = index.renamed(&document, &identity, "tally").unwrap();
        let renamed = renamed.design.unwrap();
        renamed.validate().unwrap();
        let parsed = renamed.parsed().unwrap();
        assert!(parsed.validation.is_none());
        let text = &renamed.proposed_source["Rounds.rs"];
        assert_eq!(
            *text,
            super::super::calls::combined(
                "Rounds.rs",
                &parsed.proposed["Rounds.rs"],
                &parsed.proposed_calls["Rounds.rs"],
            )
            .unwrap()
        );
        assert!(text.contains("fn tally(round: u32)"), "{text}");
    }
}
