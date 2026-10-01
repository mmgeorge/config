use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

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

const PLAN_DOCUMENT_PATH: &str = "plan.json";

/// Records the model-authored description independently of declaration files.
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DesignDocument {
    /// Describes the intended behavioral change and its design.
    pub description: String,
}

/// Owns baseline declarations and the agent's proposed file contents.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct DeclarationDesign {
    #[serde(default)]
    pub document: DesignDocument,
    #[serde(default = "default_line_width")]
    pub line_width: usize,
    pub baseline: BTreeMap<String, DeclarationFile>,
    pub proposed: BTreeMap<String, String>,
    pub moved: BTreeMap<String, String>,
}

fn default_line_width() -> usize {
    80
}

impl Default for DeclarationDesign {
    fn default() -> Self {
        Self {
            document: DesignDocument::default(),
            line_width: default_line_width(),
            baseline: BTreeMap::new(),
            proposed: BTreeMap::new(),
            moved: BTreeMap::new(),
        }
    }
}

impl DeclarationDesign {
    /// Capture eligible working-tree declarations without modifying the checkout.
    pub fn capture(workspace: &Path) -> Result<Self> {
        let result = std::process::Command::new("git")
            .current_dir(workspace)
            .args([
                "ls-files",
                "--cached",
                "--others",
                "--exclude-standard",
                "-z",
            ])
            .output()?;
        ensure!(
            result.status.success(),
            "discover declaration files: {}",
            String::from_utf8_lossy(&result.stderr)
        );
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
        let mut retained_bytes = 0usize;
        for bytes in result
            .stdout
            .split(|byte| *byte == 0)
            .filter(|path| !path.is_empty())
        {
            let path = std::str::from_utf8(bytes)?.replace('\\', "/");
            if DeclarationOverview::language(&path).is_none() {
                continue;
            }
            validate_path(&path)?;
            let target = workspace.join(&path);
            let metadata = match std::fs::symlink_metadata(&target) {
                Ok(metadata) => metadata,
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
                Err(error) => return Err(error.into()),
            };
            if !metadata.is_file() || metadata.file_type().is_symlink() {
                continue;
            }
            ensure!(
                metadata.len() <= 1024 * 1024,
                "{path}: source exceeds 1 MiB"
            );
            let source =
                std::fs::read_to_string(&target).with_context(|| format!("read {path}"))?;
            let text = DeclarationOverview::extract(&path, &source)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("extract {path}"))?;
            let text = DeclarationOverview::format_with_width(&path, &text, design.line_width)
                .map_err(|error| anyhow::anyhow!("{error:?}"))
                .with_context(|| format!("format baseline {path}"))?;
            retained_bytes += text.len();
            ensure!(
                retained_bytes <= 8 * 1024 * 1024,
                "declaration snapshot exceeds 8 MiB"
            );
            design.baseline.insert(
                path.clone(),
                DeclarationFile {
                    text: text.clone(),
                    source_digest: super::digest(source.as_bytes()),
                },
            );
            design.proposed.insert(path, text);
        }
        Ok(design)
    }

    /// Validate every proposed overview before exposing a persisted revision.
    pub fn validate(&self) -> Result<()> {
        ensure!(
            (40..=240).contains(&self.line_width),
            "declaration line width must be between 40 and 240"
        );
        ensure!(
            self.document.description.len() <= 16 * 1024,
            "plan description exceeds 16 KiB"
        );
        ensure!(
            !self.document.description.contains('\0'),
            "plan description contains a NUL byte"
        );
        let mut bytes = 0usize;
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
    pub fn patch(&self, patch: &str) -> Result<Self> {
        ensure!(patch.len() <= 1024 * 1024, "design patch exceeds 1 MiB");
        let lines = patch.lines().collect::<Vec<_>>();
        ensure!(
            lines.first() == Some(&"*** Begin Patch") && lines.last() == Some(&"*** End Patch"),
            "patch must start with *** Begin Patch and end with *** End Patch"
        );
        let mut candidate = self.clone();
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
                    .context("plan.json must contain only a string description")?;
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
                    candidate.proposed.insert(
                        path.into(),
                        DeclarationOverview::parse(path, &text)
                            .map_err(|error| anyhow::anyhow!("{error:?}"))?,
                    );
                }
                _ => {
                    let original = candidate.proposed.get(path).with_context(|| {
                        format!("cannot update absent declaration file: {path}")
                    })?;
                    let text = patch_file(original, body)?;
                    let text = DeclarationOverview::parse(destination, &text)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                    candidate.proposed.remove(path);
                    candidate.proposed.insert(destination.into(), text);
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
}

fn validate_path(path: &str) -> Result<()> {
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
    ensure!(
        DeclarationOverview::language(path).is_some(),
        "unsupported declaration path: {path}. Only .rs, .ts, .tsx, and .lua are supported."
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
    use super::*;

    #[test]
    fn capture_uses_repository_width_and_rejects_invalid_configuration() {
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
        let design = DeclarationDesign::capture(workspace.path()).unwrap();
        assert_eq!(design.line_width, 50);
        assert!(
            design.baseline["lib.rs"]
                .text
                .lines()
                .all(|line| line.chars().count() <= 50)
        );
        assert!(design.changed_paths().is_empty());
        std::fs::write(
            workspace.path().join(".forge.json"),
            r#"{"declaration_line_width":12}"#,
        )
        .unwrap();
        assert!(DeclarationDesign::capture(workspace.path()).is_err());
        std::fs::write(
            workspace.path().join(".forge.json"),
            r#"{"declaration_line_width":"80"}"#,
        )
        .unwrap();
        assert!(DeclarationDesign::capture(workspace.path()).is_err());
    }

    #[test]
    fn description_and_declaration_edits_commit_atomically() {
        let design = DeclarationDesign::default();
        let patch = "*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"description\": \"\"\n+  \"description\": \"Add cancellable requests.\"\n*** Add File: src/request.rs\n+pub struct Request;\n*** End Patch";
        let changed = design.patch(patch).unwrap();
        assert_eq!(changed.document.description, "Add cancellable requests.");
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
                .patch(&patch.replace("pub struct Request;", "fn invalid() {}"))
                .is_err()
        );
        assert!(design.document.description.is_empty() && design.proposed.is_empty());
        let invalid = "*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"description\": \"\"\n+  \"description\": \"Change\",\n+  \"tasks\": []\n*** End Patch";
        assert!(design.patch(invalid).is_err());
        assert!(
            design
                .patch("*** Begin Patch\n*** Delete File: plan.json\n*** End Patch")
                .is_err()
        );
        assert!(design.patch("*** Begin Patch\n*** Update File: plan.json\n*** Move to: other.rs\n@@\n {}\n*** End Patch").is_err());
        let mut document = crate::plan::document::test_fixture("description", "Description");
        document.design = Some(design);
        assert!(document.validate_for_submission().is_err());
        document.design = Some(changed.clone());
        document.validate_for_submission().unwrap();
        let oversized = changed.patch(&format!("*** Begin Patch\n*** Update File: plan.json\n@@\n-  \"description\": \"Add cancellable requests.\"\n+  \"description\": \"{}\"\n*** End Patch", "x".repeat(16 * 1024 + 1)));
        assert!(oversized.is_err());
    }

    #[test]
    fn saved_layout_survives_validation_round_trip_and_agent_patches() {
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
        let changed = saved.patch("*** Begin Patch\n*** Update File: registry.rs\n@@\n-    second: u64,\n+    second: String,\n*** End Patch").unwrap();
        assert_eq!(
            changed.proposed["registry.rs"],
            text.replace("second: u64", "second: String")
        );
        assert_eq!(changed.baseline, saved.baseline);
        assert!(changed.patch("*** Begin Patch\n*** Update File: registry.rs\n@@\n-impl Registry { pub fn first(&self) -> u64; }\n+impl Registry { pub fn first(&self) -> u64 { 1 } }\n*** End Patch").is_err());
    }

    #[test]
    fn moves_round_trip_and_cannot_hide_a_deleted_baseline_file() {
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
        let moved = design.patch("*** Begin Patch\n*** Update File: first.rs\n*** Move to: moved.rs\n@@\n pub struct Contract;\n*** End Patch").unwrap();
        assert_eq!(moved.moved["first.rs"], "moved.rs");
        let restored = moved.patch("*** Begin Patch\n*** Update File: moved.rs\n*** Move to: first.rs\n@@\n pub struct Contract;\n*** End Patch").unwrap();
        assert!(restored.moved.is_empty());
        assert!(restored.changed_paths().is_empty());
        let deleted = design
            .patch("*** Begin Patch\n*** Delete File: second.rs\n*** End Patch")
            .unwrap();
        assert!(deleted.patch("*** Begin Patch\n*** Update File: first.rs\n*** Move to: second.rs\n@@\n pub struct Contract;\n*** End Patch").is_err());
    }

    #[test]
    fn patch_is_atomic_and_workspace_changes_block_submission() {
        let root = tempfile::tempdir().unwrap();
        std::fs::write(root.path().join("lib.rs"), "pub struct Before;\n").unwrap();
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
        let updated = design.patch(patch).unwrap();
        assert_eq!(updated.proposed["lib.rs"], "pub struct After;\n");
        assert!(updated.check_workspace(root.path()).is_ok());
        assert!(
            design
                .patch(&patch.replace("+function M.get(id)", "+function M.get(id) return id end"))
                .is_err()
        );
        assert_eq!(design.proposed["lib.rs"], "pub struct Before;\n");
        std::fs::write(root.path().join("lib.rs"), "pub struct Changed;\n").unwrap();
        assert!(updated.check_workspace(root.path()).is_err());
        assert!(design.patch("*** Begin Patch\n*** Add File: ../escape.rs\n+pub struct Escaped;\n*** End Patch").is_err());
    }
}
