use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use anyhow::{Context, Result, ensure};
use forge_diff::syntax::DeclarationOverview;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use super::design_document::DesignDocument;

/// Retains the immutable declaration source and workspace content identity.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationFile {
    pub text: String,
    pub source_digest: String,
}

const PLAN_DOCUMENT_PATH: &str = "plan.json";

type SourceOverviewCache = std::sync::Mutex<BTreeMap<String, (DeclarationFile, Vec<super::FunctionBody>)>>;
static SOURCE_OVERVIEW_CACHE: std::sync::OnceLock<SourceOverviewCache> = std::sync::OnceLock::new();

/// Owns baseline declarations and the agent's proposed file contents.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationDesign {
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

impl DeclarationDesign {
    /// Format and validate one submission using the shared lazy source resolver.
    pub(crate) async fn validated(&self, workspace: &Path) -> Result<Self> {
        self.check_workspace(workspace)?;
        self.validated_revision(workspace).await
    }

    /// Validate a new target without replacing the immutable pre-execution baseline.
    pub(crate) async fn validated_revision(&self, workspace: &Path) -> Result<Self> {
        let mut design = self.formatted()?;
        super::comment_lint::validate(self)?;
        super::usage::validate(&design, workspace)?;
        let paths = design.proposed.keys().filter(|path| path.ends_with(".rs") || path.ends_with("Cargo.toml")).cloned().collect::<Vec<_>>();
        for path in paths {
            for directory in Path::new(&path).ancestors().skip(1) {
                let lock = directory.join("Cargo.lock").to_string_lossy().replace('\\', "/");
                if !design.cargo_lock.contains_key(&lock)
                    && let Some(text) = workspace_source(workspace, &lock)?
                {
                    toml::from_str::<toml::Value>(&text).with_context(|| format!("parse {lock}"))?;
                    design.cargo_lock.insert(lock, text);
                }
            }
        }
        design.validate()?;
        let mut resolver = crate::declaration::DeclarationResolver::prepare(workspace, &design, false, None).await?;
        let report = resolver.validate_sources(&design).await?;
        report.ensure_valid()?;
        design.validation = Some(report);
        Ok(design)
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
        ensure!(
            (40..=240).contains(&design.line_width),
            "declaration line width must be between 40 and 240"
        );
        design.validate()?;
        Ok(design)
    }

    pub(crate) fn source(&self, workspace: &Path, path: &str) -> Result<(DeclarationFile, Vec<super::FunctionBody>)> {
        validate_path(path)?;
        let source = workspace_source(workspace, path)?.with_context(|| format!("declaration file does not exist: {path}"))?;
        let source_digest = super::digest(source.as_bytes());
        let key = format!("{path}:{}:{source_digest}", self.line_width);
        let cache = SOURCE_OVERVIEW_CACHE.get_or_init(Default::default);
        let cached = cache.lock().map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?.get(&key).cloned();
        let overview = if let Some(overview) = cached { overview } else {
            let (text, calls) = DeclarationOverview::extract_with_calls(path, &source)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("extract {path}"))?;
            let text = DeclarationOverview::format_with_width(path, &text, self.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let calls = super::calls::from_extracted(calls);
            let saved_owners = forge_diff::syntax::DeclarationCalls::extract(path, &text, true).map_err(|error| anyhow::anyhow!("{error:?}"))?.into_iter().map(|function| function.owner).collect::<BTreeSet<_>>();
            let calls = calls.into_iter().filter(|function| saved_owners.contains(&function.owner)).collect();
            let overview = (DeclarationFile { text, source_digest }, calls);
            let mut cache = cache.lock().map_err(|_| anyhow::anyhow!("source overview cache lock poisoned"))?;
            let bytes = cache.values().map(|(file, calls)| file.text.len() + calls.iter().map(|function| function.owner.len() + function.call.iter().flatten().map(|call| call.name.len() + 16).sum::<usize>()).sum::<usize>()).sum::<usize>();
            if cache.len() >= 16 || bytes >= 8 * 1024 * 1024 { cache.clear(); }
            cache.insert(key, overview.clone());
            overview
        };
        let (file, mut calls) = overview;
        if calls.iter().flat_map(|function| function.call.iter().flatten()).any(|reference| matches!(reference.kind, super::CallKind::Value | super::CallKind::Property)) {
            let mut captured = self.clone();
            captured.proposed.insert(path.into(), file.text.clone());
            let mut resolver = crate::declaration::DeclarationResolver::planned(workspace, &captured, false)?;
            super::calls::classify(path, &mut calls, &mut resolver);
        }
        Ok((file, calls))
    }

    /// Inspect one uncaptured workspace file without changing the saved design.
    /// Captured, deleted, and moved paths always use the immutable plan snapshot.
    pub fn inspect(&self, workspace: &Path, path: Option<&str>, baseline: bool) -> Result<serde_json::Value> {
        if let Some(path) = path
            && path != PLAN_DOCUMENT_PATH
            && !self.baseline.contains_key(path)
            && !self.proposed.contains_key(path)
        {
            let (file, calls) = self.source(workspace, path)?;
            let text = super::calls::combined(path, &file.text, &calls)?;
            return Ok(serde_json::json!({"path":path,"side":"workspace","text":text,"source_digest":file.source_digest}));
        }
        self.read(path, baseline)
    }

    /// Validate every proposed overview before exposing a persisted revision.
    pub fn validate(&self) -> Result<()> {
        ensure!(
            (40..=240).contains(&self.line_width),
            "declaration line width must be between 40 and 240"
        );
        self.document.validate()?;
        let mut bytes = 0usize;
        for (path, text) in &self.cargo_lock {
            validate_relative_path(path)?;
            ensure!(Path::new(path).file_name().is_some_and(|name| name == "Cargo.lock"), "invalid Cargo lockfile path: {path}");
            toml::from_str::<toml::Value>(text).with_context(|| format!("parse {path}"))?;
            bytes += text.len();
        }
        for (path, file) in &self.baseline {
            validate_path(path)?;
            DeclarationOverview::parse(path, &file.text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("validate baseline {path}"))?;
            bytes += file.text.len();
        }
        for (path, text) in &self.proposed {
            validate_path(path)?;
            DeclarationOverview::parse(path, text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("validate {path}"))?;
            bytes += text.len();
            ensure!(
                bytes <= 8 * 1024 * 1024,
                "baseline and proposed declaration snapshot exceeds 8 MiB"
            );
        }
        ensure!(bytes <= 8 * 1024 * 1024, "declaration snapshot exceeds 8 MiB");
        for (files, baseline) in [(&self.baseline_calls, true), (&self.proposed_calls, false)] {
            for (path, calls) in files {
                let text = if baseline { self.baseline.get(path).map(|file| &file.text) } else { self.proposed.get(path) }
                    .with_context(|| format!("call metadata has no declaration file: {path}"))?;
                let combined = super::calls::combined(path, text, calls)?;
                let (_, parsed) = super::calls::parse(path, &combined, calls)?;
                ensure!(parsed.len() == calls.len(), "call metadata has an absent or ambiguous callable: {path}");
                bytes += serde_json::to_vec(calls)?.len();
            }
        }
        ensure!(bytes <= 8 * 1024 * 1024, "declaration and call snapshot exceeds 8 MiB");
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
        if candidate.validation.as_ref().is_some_and(|report| report.fingerprint != crate::declaration::fingerprint(&candidate)) {
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

    /// Read one virtual overview or list paths without sending the complete snapshot.
    pub fn read(&self, path: Option<&str>, baseline: bool) -> Result<serde_json::Value> {
        if let Some(path) = path {
            if path == PLAN_DOCUMENT_PATH {
                ensure!(!baseline, "plan.json has no source baseline");
                return Ok(
                    serde_json::json!({"path":path,"side":"proposed","text":format!("{}\n",serde_json::to_string_pretty(&self.document)?)}),
                );
            }
            validate_path(path)?;
            let text = if baseline {
                self.baseline.get(path).map(|file| &file.text)
            } else {
                self.proposed.get(path)
            }
            .with_context(|| format!("declaration file does not exist: {path}"))?;
            let calls = if baseline { self.baseline_calls.get(path) } else { self.proposed_calls.get(path) };
            let text = super::calls::combined(path, text, calls.map(Vec::as_slice).unwrap_or_default())?;
            Ok(serde_json::json!({"path":path,"side":if baseline {"baseline"} else {"proposed"},"text":text}))
        } else {
            Ok(
                serde_json::json!({"paths":self.baseline.keys().chain(self.proposed.keys()).map(String::as_str).chain([PLAN_DOCUMENT_PATH]).collect::<BTreeSet<_>>(),"changed_paths":self.changed_paths()}),
            )
        }
    }

    /// Apply a familiar multi-file patch atomically to virtual proposed files.
    pub fn patch(&self, workspace: &Path, source_digests: &BTreeMap<String, String>, patch: &str) -> Result<Self> {
        ensure!(patch.len() <= 1024 * 1024, "design patch exceeds 1 MiB");
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
                if !candidate.proposed.contains_key(path) && !candidate.baseline.contains_key(path) {
                    if kind == "add" {
                        ensure!(std::fs::symlink_metadata(workspace.join(path)).is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound), "{path}: Add File destination already exists or cannot be inspected");
                    } else {
                        let (file, calls) = candidate.source(workspace, path)?;
                        if let Some(expected) = source_digests.get(path) {
                            ensure!(*expected == file.source_digest, "{path}: workspace changed since inspection. Read the declaration file again.");
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
                    ensure!(std::fs::symlink_metadata(workspace.join(to)).is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound), "{to}: move destination already exists or cannot be inspected");
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
                let original = format!("{}\n", serde_json::to_string_pretty(&candidate.document)?);
                let text = patch_file(&original, body)?;
                candidate.document = serde_json::from_str(&text)
                    .context("plan.json requires objective, requirements, background, decisions, design, verification with automated and manual strings, and tests; flows is an array of {title, description, root} objects and usage is optional")?;
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
                    if !calls.is_empty() { candidate.proposed_calls.insert(path.into(), calls); }
                }
                _ => {
                    let original = candidate.proposed.get(path).with_context(|| {
                        format!("cannot update absent declaration file: {path}")
                    })?;
                    let calls = candidate.proposed_calls.get(path).map(Vec::as_slice).unwrap_or_default();
                    let original = super::calls::combined(path, original, calls)?;
                    let text = patch_file(&original, body)?;
                    let (text, calls) = super::calls::parse(destination, &text, calls)?;
                    candidate.proposed.remove(path);
                    candidate.proposed_calls.remove(path);
                    candidate.proposed.insert(destination.into(), text);
                    if !calls.is_empty() { candidate.proposed_calls.insert(destination.into(), calls); }
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
        if candidate.proposed_calls.values().flatten().flat_map(|function| function.call.iter().flatten()).any(|reference| !reference.unresolved && matches!(reference.kind, super::CallKind::Value | super::CallKind::Property)) {
            let mut resolver = crate::declaration::DeclarationResolver::planned(workspace, &candidate, false)?;
            for (path, functions) in &mut candidate.proposed_calls {
                super::calls::classify(path, functions, &mut resolver);
            }
        }
        candidate.validate()?;
        if candidate.proposed == self.proposed && candidate.proposed_calls == self.proposed_calls && candidate.document == self.document && candidate.moved == self.moved {
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

/// Read one bounded workspace source after validating its path and regular-file identity.
pub(crate) fn workspace_source(workspace: &Path, path: &str) -> Result<Option<String>> {
    use std::io::Read;
    validate_relative_path(path)?;
    let target = workspace.join(path);
    let metadata = match std::fs::symlink_metadata(&target) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => return Err(error).with_context(|| format!("inspect {path}")),
    };
    ensure!(metadata.is_file() && !metadata.file_type().is_symlink(), "{path}: source is not a regular file");
    ensure!(std::fs::canonicalize(&target)?.starts_with(std::fs::canonicalize(workspace)?), "{path}: source escapes workspace");
    ensure!(metadata.len() <= 1024 * 1024, "{path}: source exceeds 1 MiB");
    let mut source = String::new();
    std::fs::File::open(&target)?.take(1024 * 1024 + 1).read_to_string(&mut source)?;
    ensure!(source.len() <= 1024 * 1024, "{path}: source exceeds 1 MiB");
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

#[cfg(test)]
mod tests {
    #[test]
    fn callback_capture_uses_saved_alias_evidence_without_scanning_other_sources() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("run.ts"), "import { worker as callback } from './api'; export function install() { register(callback); }").unwrap();
        std::fs::write(workspace.path().join("unrelated.ts"), "invalid syntax {").unwrap();
        let mut design = super::DeclarationDesign::default();
        design.proposed.insert("api.ts".into(), "export function worker(): void;\n".into());
        let (_, functions) = design.source(workspace.path(), "run.ts").unwrap();
        let reference = functions[0].call.as_ref().unwrap().iter().find(|reference| reference.name == "callback").unwrap();
        assert_eq!(reference.kind, crate::plan::CallKind::Callback);
        let (_, warmed) = design.source(workspace.path(), "run.ts").unwrap();
        assert_eq!(functions, warmed);
        assert_eq!(design.proposed.len(), 1);
    }
    use super::*;

    #[test]
    fn first_edit_captures_only_its_source_and_failed_patches_publish_nothing() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("Foo.rs"), "pub struct Foo;\nimpl Foo { pub fn old(&self) {} }\n").unwrap();
        std::fs::write(workspace.path().join("unrelated.rs"), "invalid Rust {").unwrap();
        std::fs::write(workspace.path().join("unrelated.json"), "invalid JSON").unwrap();
        let design = DeclarationDesign::open(workspace.path()).unwrap();
        let inspected = design.inspect(workspace.path(), Some("Foo.rs"), false).unwrap();
        assert!(design.baseline.is_empty() && design.proposed.is_empty());
        assert!(!inspected["text"].as_str().unwrap().contains("{}"));
        let digests = BTreeMap::from([("Foo.rs".into(), inspected["source_digest"].as_str().unwrap().into())]);
        let patch = "*** Begin Patch\n*** Update File: Foo.rs\n@@\n   pub fn old(&self);\n+\n+  pub fn new(&self);\n*** End Patch";
        let invalid = patch.replace("*** End Patch", "*** Add File: bad.rs\n+fn invalid() {}\n*** End Patch");
        assert!(design.patch(workspace.path(), &digests, &invalid).is_err());
        assert!(design.baseline.is_empty() && design.proposed.is_empty());
        let changed = design.patch(workspace.path(), &digests, patch).unwrap();
        assert_eq!(changed.baseline.len(), 1);
        assert_eq!(changed.proposed.len(), 1);
        assert!(!changed.baseline["Foo.rs"].text.contains("fn new"));
        assert!(changed.proposed["Foo.rs"].contains("fn new"));
        std::fs::write(workspace.path().join("Foo.rs"), "pub struct Changed;\n").unwrap();
        assert!(design.patch(workspace.path(), &digests, patch).is_err());
        let next = changed.patch(workspace.path(), &digests, "*** Begin Patch\n*** Update File: Foo.rs\n@@\n-  pub fn new(&self);\n+  pub fn revised(&self);\n*** End Patch").unwrap();
        assert_eq!(next.baseline, changed.baseline);
        assert!(next.check_workspace(workspace.path()).is_err());
    }

    #[test]
    fn lazy_deletes_and_moves_cannot_recapture_or_overwrite_workspace_sources() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("first.rs"), "pub struct First;\n").unwrap();
        std::fs::write(workspace.path().join("occupied.rs"), "pub struct Occupied;\n").unwrap();
        let design = DeclarationDesign::open(workspace.path()).unwrap();
        assert!(design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Add File: occupied.rs\n+pub struct Replacement;\n*** End Patch").is_err());
        assert!(design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: first.rs\n*** Move to: occupied.rs\n@@\n pub struct First;\n*** End Patch").is_err());
        let deleted = design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Delete File: first.rs\n*** End Patch").unwrap();
        assert!(deleted.inspect(workspace.path(), Some("first.rs"), false).is_err());
        assert!(deleted.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: first.rs\n@@\n-pub struct First;\n+pub struct Recaptured;\n*** End Patch").is_err());
        let moved = design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: first.rs\n*** Move to: moved.rs\n@@\n pub struct First;\n*** End Patch").unwrap();
        let saved: DeclarationDesign = serde_json::from_slice(&serde_json::to_vec(&moved).unwrap()).unwrap();
        let restored = saved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: moved.rs\n*** Move to: first.rs\n@@\n pub struct First;\n*** End Patch").unwrap();
        assert!(restored.moved.is_empty());
        assert_eq!(restored.baseline, moved.baseline);
        assert!(restored.changed_paths().is_empty());
    }

    #[test]
    fn initialization_uses_repository_width_without_capturing_sources() {
        let workspace = tempfile::tempdir().unwrap();
        let initialized = std::process::Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(workspace.path())
            .status()
            .unwrap();
        assert!(initialized.success());
        std::fs::write(workspace.path().join("lib.rs"), "/// This long paragraph describes the registry contract and should wrap to the repository configured width.\npub struct Registry;\n").unwrap();
        std::fs::write(
            workspace.path().join(".forge.json"),
            r#"{"branch_prefix":"feature/","declaration_line_width":50}"#,
        )
        .unwrap();
        std::fs::write(workspace.path().join("Cargo.lock"), "version = 4\n").unwrap();
        let design = DeclarationDesign::open(workspace.path()).unwrap();
        assert_eq!(design.line_width, 50);
        assert!(design.cargo_lock.is_empty());
        assert!(design.baseline.is_empty());
        assert!(!design.proposed.contains_key("Cargo.lock"));
        assert!(
            design.inspect(workspace.path(), Some("lib.rs"), false).unwrap()["text"].as_str().unwrap()
                .lines()
                .all(|line| line.chars().count() <= 50)
        );
        assert!(design.changed_paths().is_empty());
        std::fs::write(
            workspace.path().join(".forge.json"),
            r#"{"declaration_line_width":12}"#,
        )
        .unwrap();
        assert!(DeclarationDesign::open(workspace.path()).is_err());
        std::fs::write(
            workspace.path().join(".forge.json"),
            r#"{"declaration_line_width":"80"}"#,
        )
        .unwrap();
        assert!(DeclarationDesign::open(workspace.path()).is_err());
    }

    #[test]
    fn description_and_declaration_edits_commit_atomically() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = DeclarationDesign::default();
        design.document.background = "Texture requests currently run to completion.".into();
        design.document.requirements = vec!["Cancellation must prevent publication.".into()];
        let patch = "*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"objective\": \"\",\n+  \"objective\": \"Support cancellable texture loading.\",\n@@\n-  \"design\": \"\",\n+  \"design\": \"Add cancellable requests.\",\n*** Add File: src/request.rs\n+pub struct Request;\n*** End Patch";
        let changed = design.patch(workspace.path(), &Default::default(), patch).unwrap();
        assert_eq!(changed.document.objective, "Support cancellable texture loading.");
        assert_eq!(changed.document.design, "Add cancellable requests.");
        assert_eq!(changed.changed_paths(), vec!["src/request.rs"]);
        assert!(!changed.proposed.contains_key("plan.json"));
        let read = changed.read(Some("plan.json"), false).unwrap();
        assert!(
            read["text"]
                .as_str()
                .unwrap()
                .contains("Add cancellable requests.")
        );
        assert!(changed.read(Some("plan.json"), true).is_err());
        assert!(
            changed.read(None, false).unwrap()["paths"]
                .as_array()
                .unwrap()
                .iter()
                .any(|path| path == "plan.json")
        );
        assert!(
            design
                .patch(workspace.path(), &Default::default(), &patch.replace("pub struct Request;", "fn invalid() {}"))
                .is_err()
        );
        assert!(design.document.design.is_empty() && design.proposed.is_empty());
        let invalid = "*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"design\": \"\",\n+  \"design\": \"Change\",\n+  \"tasks\": []\n*** End Patch";
        assert!(design.patch(workspace.path(), &Default::default(), invalid).is_err());
        assert!(
            design
                .patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Delete File: plan.json\n*** End Patch")
                .is_err()
        );
        assert!(design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: plan.json\n*** Move to: other.rs\n@@\n {}\n*** End Patch").is_err());
        let mut document = crate::plan::document::test_fixture("description", "Design");
        document.design = Some(design);
        assert!(document.validate_for_submission().is_err());
        document.design = Some(changed.clone());
        document.validate_for_submission().unwrap();
        document.design.as_mut().unwrap().document.objective.clear();
        assert!(document.validate_for_submission().is_err());
        let oversized = changed.patch(workspace.path(), &Default::default(), &format!("*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"design\": \"Add cancellable requests.\",\n+  \"design\": \"{}\",\n*** End Patch", "x".repeat(16 * 1024 + 1)));
        assert!(oversized.is_err());
    }

    #[test]
    fn ignore_file_proposals_preserve_rules_without_modifying_the_workspace() {
        let workspace = tempfile::tempdir().unwrap();
        let empty = DeclarationDesign::default();
        let added = empty.patch(workspace.path(), &Default::default(),
            "*** Begin Patch\n*** Add File: .gitignore\n+# Build output\n+/target/\n+!Cargo.lock\n*** End Patch").unwrap();
        assert_eq!(added.read(Some(".gitignore"), false).unwrap()["text"], "# Build output\n/target/\n!Cargo.lock\n");
        assert!(!workspace.path().join(".gitignore").exists());

        let existing = "# Keep local exceptions\n*.log\n!important.log\n";
        std::fs::write(workspace.path().join(".gitignore"), existing).unwrap();
        let updated = empty.patch(workspace.path(), &Default::default(),
            "*** Begin Patch\n*** Update File: .gitignore\n@@\n !important.log\n+/target/\n*** End Patch").unwrap();
        assert_eq!(updated.baseline[".gitignore"].text, existing);
        assert_eq!(updated.proposed[".gitignore"], format!("{existing}/target/\n"));
        assert_eq!(updated.formatted().unwrap().proposed, updated.proposed);
        let mut document = crate::plan::document::test_fixture("ignore", "Ignore outputs");
        document.design = Some(updated);
        assert!(crate::plan::render_plan_at(&document, workspace.path()).unwrap().markdown.contains("/target/"));
        assert_eq!(std::fs::read_to_string(workspace.path().join(".gitignore")).unwrap(), existing);
    }

    #[test]
    fn validation_requirements_survive_submission_and_revision_without_running_commands() {
        let workspace = tempfile::tempdir().unwrap();
        let store = crate::plan::PlanFileStore::new(workspace.path().join("data"), workspace.path());
        let mut design = DeclarationDesign::default();
        design.document.objective = "Verify score resets.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Reset score when a new round starts.".into();
        let patch = r#"*** Begin Patch
*** Update File: plan.json
@@
-    "automated": "",
-    "manual": ""
+    "automated": "echo test > should-not-exist.txt\nnvim --headless -l tests/score.lua",
+    "manual": "- Start a new round and confirm the score is zero.\n- Confirm `Score` remains visible after resize."
*** End Patch"#;
        let changed = design.patch(workspace.path(), &Default::default(), patch).unwrap();
        let mut document = crate::plan::document::test_fixture("validation", "Score validation");
        document.design = Some(changed.clone());
        store.write_working_document("session", "validation", &document).unwrap();
        let (_, rendered, _) = store.submit_document_revision("session", "validation", 1, 1).unwrap();
        let original = store.read_submitted_document("session", "validation", 1).unwrap();
        assert_eq!(original.design.as_ref().unwrap().document, changed.document);
        assert!(rendered.markdown.contains(" Verification \n  Automated:\n    echo test > should-not-exist.txt\n    nvim --headless -l tests/score.lua"));
        assert!(rendered.markdown.contains("  Manual:\n  - Start a new round"));
        assert!(!workspace.path().join("should-not-exist.txt").exists());
        let serialized = changed.read(Some("plan.json"), false).unwrap();
        let metadata: serde_json::Value = serde_json::from_str(serialized["text"].as_str().unwrap()).unwrap();
        assert_eq!(metadata["verification"]["automated"], changed.document.verification.automated);

        let revised = changed.patch(workspace.path(), &Default::default(), r#"*** Begin Patch
*** Update File: plan.json
@@
-    "automated": "echo test > should-not-exist.txt\nnvim --headless -l tests/score.lua",
+    "automated": "nvim --headless -l tests/score.lua",
@@
-    "manual": "- Start a new round and confirm the score is zero.\n- Confirm `Score` remains visible after resize."
+    "manual": ""
*** End Patch"#).unwrap();
        document.version += 1;
        document.design = Some(revised);
        store.write_working_document("session", "validation", &document).unwrap();
        store.submit_document_revision("session", "validation", 2, document.version).unwrap();
        let current = store.read_submitted_document("session", "validation", 2).unwrap();
        let delta = crate::plan::revision::DeclarationDelta::between(Some(&original), &current).unwrap();
        assert!(delta.files.is_empty());
        assert!(delta.document.contains("a/Verification/Automated b/Verification/Automated"));
        assert!(delta.document.contains("-echo test > should-not-exist.txt"));
        assert!(delta.document.contains("-- Start a new round and confirm the score is zero."));
        assert_eq!(store.read_submitted_document("session", "validation", 1).unwrap(), original);

        for invalid in ["\\u0000".to_owned(), "x".repeat(16 * 1024 + 1)] {
            let patch = format!("*** Begin Patch\n*** Update File: plan.json\n@@\n-    \"automated\": \"\",\n+    \"automated\": \"{invalid}\",\n*** Add File: unwanted.rs\n+pub struct Unwanted;\n*** End Patch");
            assert!(design.patch(workspace.path(), &Default::default(), &patch).is_err());
            assert!(design.document.verification.automated.is_empty() && design.proposed.is_empty());
        }
    }

    #[test]
    fn complete_specification_survives_review_and_optional_section_removal() {
        let workspace = tempfile::tempdir().unwrap();
        let store = crate::plan::PlanFileStore::new(workspace.path().join("data"), workspace.path());
        let empty = DeclarationDesign::default();
        let metadata = serde_json::json!({
            "objective": "Make cancellation observable.",
            "usage": "```text\napp cancel 42\nCancelled request 42\n```",
            "requirements": ["Cancellation preserves the published texture."],
            "background": "`src/request.rs` owns pending uploads. Publication currently happens immediately.",
            "decisions": [{"decision": "Publish at frame boundaries.", "rationale": "Each frame observes one consistent texture selection."}],
            "design": "`Request` retains cancellation state until pending work finishes.",
            "flows": [{"title":"Cancel request", "description":"Cancel before publication and retain the current texture.", "root":{"text":"Request.cancel", "children":[{"text":"TextureStreaming.discard", "via":"cancellation"}]}}],
            "verification": {"automated": "cargo test --release cancellation", "manual": "- Cancel an upload and confirm the current texture remains visible."},
            "tests": [{"file":"src/request.rs", "cases":[{"name":"tests::cancellation", "change":"new", "description":"Cancel before publication and retain the current texture."}]}]
        });
        let replace = |design: &DeclarationDesign, metadata: &serde_json::Value| {
            let before = design.read(Some("plan.json"), false).unwrap();
            let after = serde_json::to_string_pretty(metadata).unwrap();
            format!("*** Begin Patch\n*** Update File: plan.json\n@@\n{}\n{}\n*** End Patch",
                before["text"].as_str().unwrap().lines().map(|line| format!("-{line}")).collect::<Vec<_>>().join("\n"),
                after.lines().map(|line| format!("+{line}")).collect::<Vec<_>>().join("\n"))
        };
        let changed = empty.patch(workspace.path(), &Default::default(), &replace(&empty, &metadata)).unwrap();
        let mut document = crate::plan::document::test_fixture("specification", "Cancellation");
        document.design = Some(changed.clone());
        store.write_working_document("session", "specification", &document).unwrap();
        let (_, rendered, _) = store.submit_document_revision("session", "specification", 1, 1).unwrap();
        let original = store.read_submitted_document("session", "specification", 1).unwrap();
        assert_eq!(serde_json::to_value(&original.design.as_ref().unwrap().document).unwrap(), metadata);
        let headings = [" Objective ", " Usage ", " Requirements ", " Background ", " Decisions ", " Design ", " Flows ", " Changes ", " Tests · 1 new ", " Verification "];
        let positions = headings.map(|heading| rendered.markdown.lines().position(|line| line == heading).unwrap());
        assert!(positions.windows(2).all(|pair| pair[0] < pair[1]));
        assert!(rendered.markdown.contains("app cancel 42\nCancelled request 42"));
        assert!(rendered.markdown.contains("Request.cancel → cancellation → TextureStreaming.discard"));
        for path in ["objective", "usage", "requirements", "background", "decisions", "design", "flows", "verification/automated", "verification/manual", "tests"] {
            assert!(rendered.navigation.anchor.iter().any(|anchor| anchor.json_path == format!("/design/document/{path}")));
        }
        for field in ["objective", "background", "design"] {
            let mut invalid = metadata.clone();
            invalid[field] = serde_json::json!("");
            let incomplete = empty.patch(workspace.path(), &Default::default(), &replace(&empty, &invalid)).unwrap();
            assert!(incomplete.document.validate_for_submission().is_err(), "{field}");
        }
        for invalid in [
            serde_json::json!({"decision": "Choose an approach.", "rationale": ""}),
            serde_json::json!({"decision": "Choose an approach.", "rationale": "Reason", "milestone": 1}),
        ] {
            let mut invalid_metadata = metadata.clone();
            invalid_metadata["decisions"] = serde_json::json!([invalid]);
            assert!(changed.patch(workspace.path(), &Default::default(), &replace(&changed, &invalid_metadata)).is_err());
        }
        let mut invalid_flow = metadata.clone();
        invalid_flow["flows"][0]["root"]["text"] = serde_json::json!("");
        assert!(changed.patch(workspace.path(), &Default::default(), &replace(&changed, &invalid_flow)).is_err());
        assert_eq!(store.read_submitted_document("session", "specification", 1).unwrap(), original);
        for tests in [
            serde_json::json!([{"file":"../outside.rs", "cases":metadata["tests"][0]["cases"]}]),
            serde_json::json!([metadata["tests"][0].clone(), metadata["tests"][0].clone()]),
            serde_json::json!([{"file":"src/request.rs", "cases":[]}]),
            serde_json::json!([{"file":"src/request.rs", "cases":[metadata["tests"][0]["cases"][0].clone(), metadata["tests"][0]["cases"][0].clone()]}]),
            serde_json::json!([{"file":"src/request.rs", "cases":[{"name":"", "change":"new", "description":"Expected behavior."}]}]),
            serde_json::json!([{"file":"src/request.rs", "cases":[{"name":"test", "change":"passed", "description":"Expected behavior."}]}]),
            serde_json::json!([{"file":"src/request.rs", "cases":[{"name":"test", "change":"new", "description":" "}]}]),
            serde_json::json!([{"file":"src/request.rs", "cases":[{"name":"test", "change":"new", "description":"Expected behavior.", "result":"passed"}]}]),
        ] {
            let mut invalid = metadata.clone();
            invalid["tests"] = tests;
            assert!(changed.patch(workspace.path(), &Default::default(), &replace(&changed, &invalid)).is_err());
            assert_eq!(store.read_submitted_document("session", "specification", 1).unwrap(), original);
        }
        let mut missing_inventory = metadata.clone();
        missing_inventory.as_object_mut().unwrap().remove("tests");
        assert!(changed.patch(workspace.path(), &Default::default(), &replace(&changed, &missing_inventory)).is_err());
        let mut revised_metadata = metadata.clone();
        revised_metadata.as_object_mut().unwrap().remove("usage");
        revised_metadata["decisions"] = serde_json::json!([]);
        revised_metadata["tests"][0]["cases"][0]["change"] = serde_json::json!("reused");
        revised_metadata["requirements"] = serde_json::json!([]);
        revised_metadata["flows"] = serde_json::json!([]);
        document.design = Some(changed.patch(workspace.path(), &Default::default(), &replace(&changed, &revised_metadata)).unwrap());
        document.version += 1;
        store.write_working_document("session", "specification", &document).unwrap();
        let (_, rendered, _) = store.submit_document_revision("session", "specification", 2, document.version).unwrap();
        assert!(!rendered.markdown.lines().any(|line| matches!(line, " Usage " | " Requirements " | " Decisions " | " Flows ")));
        assert!(!rendered.navigation.anchor.iter().any(|anchor| anchor.json_path == "/design/document/requirements"));
        let current = store.read_submitted_document("session", "specification", 2).unwrap();
        assert!(current.design.as_ref().unwrap().document.requirements.is_empty());
        let delta = crate::plan::revision::DeclarationDelta::between(Some(&original), &current).unwrap();
        assert!(delta.files.is_empty());
        assert!(delta.document.contains("a/Usage b/Usage") && delta.document.contains("a/Decisions b/Decisions"));
        assert!(delta.document.contains("a/Requirements b/Requirements"));
        assert!(delta.document.contains("a/Flows b/Flows"));
        assert!(delta.document.contains("a/Tests b/Tests"));
        assert!(rendered.markdown.contains("Tests · 1 reused"));
        assert!(!rendered.markdown.contains("= tests::cancellation"));
        assert_eq!(store.read_submitted_document("session", "specification", 1).unwrap(), original);
    }

    #[test]
    fn saved_layout_survives_validation_round_trip_and_agent_patches() {
        let workspace = tempfile::tempdir().unwrap();
        let text = "pub struct Registry {\n    first: u64,\n    second: u64,\n}\n\n\nimpl Registry { pub fn first(&self) -> u64; }\n";
        let mut design = DeclarationDesign::default();
        design.baseline.insert(
            "registry.rs".into(),
            DeclarationFile {
                text: text.into(),
                source_digest: String::new(),
            },
        );
        design.proposed.insert("registry.rs".into(), text.into());
        let bytes = serde_json::to_vec(&design).unwrap();
        let saved: DeclarationDesign = serde_json::from_slice(&bytes).unwrap();
        saved.validate().unwrap();
        assert_eq!(serde_json::to_vec(&saved).unwrap(), bytes);
        let changed = saved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: registry.rs\n@@\n-    second: u64,\n+    second: String,\n*** End Patch").unwrap();
        assert_eq!(
            changed.proposed["registry.rs"],
            text.replace("second: u64", "second: String")
        );
        assert_eq!(changed.baseline, saved.baseline);
        assert!(changed.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: registry.rs\n@@\n-impl Registry { pub fn first(&self) -> u64; }\n+impl Registry { pub fn first(&self) -> u64 { 1 } }\n*** End Patch").is_err());
    }

    #[test]
    fn moves_round_trip_and_cannot_hide_a_deleted_baseline_file() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = DeclarationDesign::default();
        for path in ["first.rs", "second.rs"] {
            let text = "pub struct Contract;\n";
            design.baseline.insert(
                path.into(),
                DeclarationFile {
                    text: text.into(),
                    source_digest: super::super::digest(text.as_bytes()),
                },
            );
            design.proposed.insert(path.into(), text.into());
        }
        let moved = design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: first.rs\n*** Move to: moved.rs\n@@\n pub struct Contract;\n*** End Patch").unwrap();
        assert_eq!(moved.moved["first.rs"], "moved.rs");
        let restored = moved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: moved.rs\n*** Move to: first.rs\n@@\n pub struct Contract;\n*** End Patch").unwrap();
        assert!(restored.moved.is_empty());
        assert!(restored.changed_paths().is_empty());
        let deleted = design
            .patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Delete File: second.rs\n*** End Patch")
            .unwrap();
        assert!(deleted.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: first.rs\n*** Move to: second.rs\n@@\n pub struct Contract;\n*** End Patch").is_err());
    }

    #[test]
    fn patch_is_atomic_and_workspace_changes_block_submission() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("lib.rs"), "pub struct Before;\n").unwrap();
        let mut design = DeclarationDesign::default();
        design.baseline.insert(
            "lib.rs".into(),
            DeclarationFile {
                text: "pub struct Before;\n".into(),
                source_digest: super::super::digest(b"pub struct Before;\n"),
            },
        );
        design
            .proposed
            .insert("lib.rs".into(), "pub struct Before;\n".into());
        let patch = "*** Begin Patch\n*** Update File: lib.rs\n@@\n-pub struct Before;\n+pub struct After;\n*** Add File: extra.lua\n+function M.get(id)\n*** End Patch";
        let updated = design.patch(workspace.path(), &Default::default(), patch).unwrap();
        assert_eq!(updated.proposed["lib.rs"], "pub struct After;\n");
        assert!(updated.check_workspace(workspace.path()).is_ok());
        assert!(
            design
                .patch(workspace.path(), &Default::default(), &patch.replace("+function M.get(id)", "+function M.get(id) return id end"))
                .is_err()
        );
        assert_eq!(design.proposed["lib.rs"], "pub struct Before;\n");
        std::fs::write(workspace.path().join("lib.rs"), "pub struct Changed;\n").unwrap();
        assert!(updated.check_workspace(workspace.path()).is_err());
        assert!(design.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Add File: ../escape.rs\n+pub struct Escaped;\n*** End Patch").is_err());
    }
}
    #[test]
    fn change_only_patch_captures_one_file_and_preserves_source_evidence() {
        let workspace = tempfile::tempdir().unwrap();
        let source = "pub fn run() { before(); before(); }\n";
        std::fs::write(workspace.path().join("main.rs"), source).unwrap();
        std::fs::write(workspace.path().join("unrelated.rs"), "invalid syntax {").unwrap();
        let design = DeclarationDesign::default();
        let patch = "*** Begin Patch\n*** Update File: main.rs\n@@\n pub fn run();\n+Change\n+  Stop retrying authentication failures.\n Calls\n*** End Patch";
        let changed = design.patch(workspace.path(), &Default::default(), patch).unwrap();
        assert_eq!(changed.baseline.len(), 1);
        assert_eq!(changed.baseline["main.rs"].text, changed.proposed["main.rs"]);
        assert_eq!(changed.changed_paths(), vec!["main.rs".to_owned()]);
        assert_eq!(changed.baseline_calls["main.rs"][0].call, changed.proposed_calls["main.rs"][0].call);
        assert_eq!(changed.baseline_calls["main.rs"][0].change, None);
        assert_eq!(changed.proposed_calls["main.rs"][0].change.as_deref(), Some("Stop retrying authentication failures."));
        assert_eq!(std::fs::read_to_string(workspace.path().join("main.rs")).unwrap(), source);
        assert!(changed.read(Some("main.rs"), false).unwrap()["text"].as_str().unwrap().contains("Change\n"));
        let snapshot = serde_json::to_vec(&changed).unwrap();
        let saved: DeclarationDesign = serde_json::from_slice(&snapshot).unwrap();
        saved.validate().unwrap();
        let moved = saved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: main.rs\n*** Move to: moved.rs\n@@\n Change\n*** End Patch").unwrap();
        assert_eq!(moved.proposed_calls["moved.rs"], saved.proposed_calls["main.rs"]);
        let removed = saved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: main.rs\n@@\n-Change\n-  Stop retrying authentication failures.\n Calls\n*** End Patch").unwrap();
        assert!(removed.changed_paths().is_empty());
        assert!(saved.patch(workspace.path(), &Default::default(), "*** Begin Patch\n*** Update File: main.rs\n@@\n Change\n-  Stop retrying authentication failures.\n Calls\n*** End Patch").is_err());
        assert_eq!(serde_json::to_vec(&saved).unwrap(), snapshot);
    }

    #[test]
    fn lazy_call_capture_and_patch_are_atomic_and_snapshot_bound() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(workspace.path().join("main.rs"), "pub fn run() { before(); before(); }\n").unwrap();
        std::fs::write(workspace.path().join("unrelated.rs"), "invalid syntax {").unwrap();
        let design = DeclarationDesign::default();
        let read = design.inspect(workspace.path(), Some("main.rs"), false).unwrap();
        assert!(read["text"].as_str().unwrap().contains("Calls\n  before\n  before\n"));
        assert!(design.baseline.is_empty() && design.baseline_calls.is_empty());
        let patch = "*** Begin Patch\n*** Update File: main.rs\n@@\n Calls\n-  before\n+  after\n   before\n*** End Patch";
        let changed = design.patch(workspace.path(), &Default::default(), patch).unwrap();
        assert_eq!(changed.baseline_calls["main.rs"][0].call.as_ref().unwrap()[0].name, "before");
        assert_eq!(changed.proposed_calls["main.rs"][0].call.as_ref().unwrap()[0].name, "after");
        assert!(changed.proposed_calls["main.rs"][0].call.as_ref().unwrap()[0].source.is_none());
        assert!(changed.proposed_calls["main.rs"][0].call.as_ref().unwrap()[1].source.is_some());
        assert_eq!(changed.baseline.len(), 1);
        assert!(design.patch(workspace.path(), &Default::default(), &patch.replace("+  after", "+  after(value)")).is_err());
        assert!(design.baseline_calls.is_empty());
        std::fs::write(workspace.path().join("main.rs"), "pub fn run() { live(); }\n").unwrap();
        assert!(changed.inspect(workspace.path(), Some("main.rs"), true).unwrap()["text"].as_str().unwrap().contains("before"));
        assert!(!changed.read(Some("main.rs"), false).unwrap()["text"].as_str().unwrap().contains("live"));
    }
