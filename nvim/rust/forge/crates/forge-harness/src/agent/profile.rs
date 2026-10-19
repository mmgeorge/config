use std::{
    collections::BTreeMap,
    io::{Read, Write},
    path::PathBuf,
    sync::Mutex,
};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

fn inherit() -> String {
    "inherit".into()
}
fn enabled() -> bool {
    true
}

/// Defines user-owned child behavior independently of model-supplied task text.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AgentProfile {
    #[serde(default = "enabled")]
    pub enabled: bool,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub instructions: String,
    #[serde(default = "inherit")]
    pub provider: String,
    #[serde(default = "inherit")]
    pub model: String,
    #[serde(default = "inherit")]
    pub reasoning_effort: String,
    #[serde(default = "inherit")]
    pub service_tier: String,
    #[serde(default = "inherit")]
    pub permission_mode: String,
}

impl Default for AgentProfile {
    fn default() -> Self {
        Self {
            enabled: true,
            description: String::new(),
            instructions: String::new(),
            provider: inherit(),
            model: inherit(),
            reasoning_effort: inherit(),
            service_tier: inherit(),
            permission_mode: inherit(),
        }
    }
}

/// Stores the global profile catalog and the profile used when a tool omits its name.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AgentProfileDocument {
    pub default_profile: String,
    pub profiles: BTreeMap<String, AgentProfile>,
}

impl Default for AgentProfileDocument {
    fn default() -> Self {
        let profiles = [
            ("worker", "Implement a scoped task", "Complete the assigned task and report the result and verification."),
            ("reviewer", "Review changes for defects", "Review the assigned changes for correctness and regressions. Report findings with file references. Do not modify files."),
            ("code-explorer", "Explore the local codebase", "Inspect the local codebase and answer the assigned question with source references. Do not modify files."),
            ("remote-explorer", "Research external code and documentation", "Research the assigned external code or documentation. Prefer primary sources and cite them. Do not modify local files."),
        ].into_iter().map(|(name, description, instructions)| (name.into(), AgentProfile {
            description: description.into(), instructions: instructions.into(), ..Default::default()
        })).collect();
        Self {
            default_profile: "worker".into(),
            profiles,
        }
    }
}

impl AgentProfileDocument {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.profiles
                .get(&self.default_profile)
                .is_some_and(|profile| profile.enabled),
            "default_profile must name an enabled profile"
        );
        for (name, profile) in &self.profiles {
            ensure!(
                !name.is_empty()
                    && name.len() <= 64
                    && name
                        .bytes()
                        .all(|character| character.is_ascii_alphanumeric()
                            || matches!(character, b'-' | b'_')),
                "profile names require 1 to 64 ASCII letters, digits, underscores, or hyphens"
            );
            ensure!(
                name != "root" && name != "Main" && name != "primary",
                "reserved profile name: {name}"
            );
            ensure!(
                matches!(
                    profile.provider.as_str(),
                    "inherit" | "codex" | "copilot" | "claude" | "antigravity"
                ),
                "profile {name} has an unsupported provider"
            );
            ensure!(
                !profile.model.trim().is_empty() && !profile.reasoning_effort.trim().is_empty(),
                "profile {name} requires model and reasoning_effort, or inherit/default"
            );
            ensure!(
                matches!(
                    profile.service_tier.as_str(),
                    "inherit" | "default" | "fast" | "ultrafast"
                ),
                "profile {name} has an unsupported service_tier"
            );
            ensure!(
                matches!(
                    profile.permission_mode.as_str(),
                    "inherit" | "read" | "write" | "yolo"
                ),
                "profile {name} has an unsupported permission_mode"
            );
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub struct AgentProfileSnapshot {
    pub path: String,
    pub revision: String,
    pub document: AgentProfileDocument,
    pub error: Option<String>,
}

/// Reloads external edits while retaining the last validated catalog for active hosts.
pub struct AgentProfileStore {
    path: PathBuf,
    valid: Mutex<Option<AgentProfileDocument>>,
}

impl AgentProfileStore {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            valid: Mutex::new(None),
        }
    }

    fn bytes(&self) -> Result<Option<Vec<u8>>> {
        match std::fs::File::open(&self.path) {
            Ok(mut file) => {
                let mut bytes = Vec::new();
                file.read_to_end(&mut bytes)?;
                Ok(Some(bytes))
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(error).context("read subagent profiles"),
        }
    }

    pub fn snapshot(&self) -> Result<AgentProfileSnapshot> {
        let bytes = match self.bytes() {
            Ok(bytes) => bytes,
            Err(error) => {
                return Ok(AgentProfileSnapshot {
                    path: self.path.to_string_lossy().into_owned(),
                    revision: "unreadable".into(),
                    document: self
                        .valid
                        .lock()
                        .expect("subagent profiles poisoned")
                        .clone()
                        .unwrap_or_default(),
                    error: Some(format!("{error:#}")),
                });
            }
        };
        let revision = revision(bytes.as_deref());
        let parsed = match bytes {
            Some(bytes) => serde_json::from_slice::<AgentProfileDocument>(&bytes)
                .context("parse subagent profiles")
                .and_then(|document| {
                    document.validate()?;
                    Ok(document)
                }),
            None => Ok(AgentProfileDocument::default()),
        };
        let mut valid = self.valid.lock().expect("subagent profiles poisoned");
        let (document, error) = match parsed {
            Ok(document) => {
                *valid = Some(document.clone());
                (document, None)
            }
            Err(error) => (
                valid.clone().unwrap_or_default(),
                Some(format!("{error:#}")),
            ),
        };
        Ok(AgentProfileSnapshot {
            path: self.path.to_string_lossy().into_owned(),
            revision,
            document,
            error,
        })
    }

    pub fn resolve(&self, name: Option<&str>) -> Result<(String, AgentProfile)> {
        let snapshot = self.snapshot()?;
        ensure!(
            self.valid
                .lock()
                .expect("subagent profiles poisoned")
                .is_some(),
            "no valid subagent profile catalog: {}",
            snapshot.error.unwrap_or_default()
        );
        let name = name.unwrap_or(&snapshot.document.default_profile);
        let profile = snapshot
            .document
            .profiles
            .get(name)
            .with_context(|| format!("unknown subagent profile: {name}"))?;
        ensure!(profile.enabled, "subagent profile is disabled: {name}");
        Ok((name.to_owned(), profile.clone()))
    }

    pub fn save(
        &self,
        expected_revision: &str,
        document: AgentProfileDocument,
    ) -> Result<AgentProfileSnapshot> {
        document.validate()?;
        let encoded = serde_json::to_vec_pretty(&document)?;
        let parent = self
            .path
            .parent()
            .context("subagent profiles require a parent directory")?;
        std::fs::create_dir_all(parent)?;
        let lock_path = self.path.with_extension("json.lock");
        let lock = std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&lock_path)?;
        lock.try_lock()
            .context("subagent profiles are being saved by another process")?;
        let result = (|| -> Result<()> {
            ensure!(
                revision(self.bytes()?.as_deref()) == expected_revision,
                "subagent profiles changed on disk. Reload before saving"
            );
            let mut temporary = tempfile::NamedTempFile::new_in(parent)?;
            temporary.write_all(&encoded)?;
            temporary.write_all(b"\n")?;
            temporary.as_file().sync_all()?;
            temporary
                .persist(&self.path)
                .context("publish subagent profiles")?;
            Ok(())
        })();
        drop(lock);
        result?;
        self.snapshot()
    }
}

fn revision(bytes: Option<&[u8]>) -> String {
    bytes.map_or_else(
        || "missing".into(),
        |bytes| hex::encode(Sha256::digest(bytes)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn large_profile_documents_round_trip_complete_instructions() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("subagents.json");
        let store = AgentProfileStore::new(path.clone());
        let mut document = AgentProfileDocument::default();
        let instructions = "λ".repeat(1024 * 1024);
        let description = "d".repeat(4097);
        let worker = document.profiles.get_mut("worker").unwrap();
        worker.instructions = instructions.clone();
        worker.description = description.clone();
        let saved = store.save("missing", document)?;
        assert!(saved.error.is_none());
        assert!(std::fs::metadata(&path)?.len() > 1024 * 1024);
        let reloaded = AgentProfileStore::new(path).resolve(None)?.1;
        assert_eq!(reloaded.instructions, instructions);
        assert_eq!(reloaded.description, description);
        Ok(())
    }

    #[test]
    fn profile_file_reloads_external_edits_and_rejects_stale_saves() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("subagents.json");
        let store = AgentProfileStore::new(path.clone());
        let initial = store.snapshot()?;
        assert_eq!(initial.revision, "missing");
        let saved = store.save(&initial.revision, initial.document.clone())?;
        assert!(saved.error.is_none());
        let other_workspace = AgentProfileStore::new(path.clone());
        let mut document = saved.document.clone();
        document.profiles.get_mut("worker").unwrap().instructions = "Updated globally".into();
        other_workspace.save(&saved.revision, document)?;
        assert!(
            store
                .save(&saved.revision, saved.document)
                .unwrap_err()
                .to_string()
                .contains("changed on disk")
        );
        assert_eq!(store.resolve(None)?.1.instructions, "Updated globally");
        std::fs::write(&path, b"invalid json")?;
        assert!(store.snapshot()?.error.is_some());
        assert_eq!(store.resolve(None)?.1.instructions, "Updated globally");
        assert!(AgentProfileStore::new(path).resolve(None).is_err());
        Ok(())
    }

    #[test]
    fn profile_validation_rejects_disabled_default_and_unknown_fields() -> Result<()> {
        let mut document = AgentProfileDocument::default();
        document.profiles.get_mut("worker").unwrap().enabled = false;
        assert!(document.validate().is_err());
        document.default_profile = "reviewer".into();
        document.validate()?;
        let directory = tempfile::tempdir()?;
        let store = AgentProfileStore::new(directory.path().join("subagents.json"));
        store.save("missing", document)?;
        assert!(
            store
                .resolve(Some("worker"))
                .unwrap_err()
                .to_string()
                .contains("disabled")
        );
        assert!(
            store
                .resolve(Some("unknown"))
                .unwrap_err()
                .to_string()
                .contains("unknown")
        );
        let mut value = serde_json::to_value(AgentProfile::default())?;
        value["typo"] = serde_json::json!(true);
        assert!(serde_json::from_value::<AgentProfile>(value).is_err());
        Ok(())
    }
}
