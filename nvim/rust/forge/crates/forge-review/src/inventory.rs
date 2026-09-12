use anyhow::{Context, Result, ensure};
use forge_git::{
    RepositoryPath,
    command::{CommandLimits, read_command},
    repository::RepositoryState,
    snapshot::{ChangeKind, ObservedPath, PathState},
    store::RepositoryStore,
};
use serde::Deserialize;
use std::{
    collections::{BTreeMap, HashSet},
    path::{Component, Path, PathBuf},
    sync::Arc,
    time::{Duration, Instant},
};

#[derive(Clone, Debug)]
pub struct InventoryEntry {
    pub kind: String,
    pub action: String,
    pub name: String,
    pub path: RepositoryPath,
    pub line: Option<usize>,
}

#[derive(Clone, Debug, Default)]
pub struct SemInventory {
    pub entry: Vec<InventoryEntry>,
}

#[derive(Deserialize)]
struct Tracked {
    changes: Vec<Entity>,
}

#[derive(Deserialize)]
struct Entity {
    #[serde(rename = "filePath", alias = "file", default)]
    file: String,
    #[serde(rename = "entityType", alias = "type", default)]
    kind: String,
    #[serde(rename = "entityName", alias = "name", default)]
    name: String,
    #[serde(rename = "changeType", default)]
    action: String,
    #[serde(rename = "entityId", alias = "parent_id", default)]
    identity: serde_json::Value,
    #[serde(rename = "startLine", alias = "start_line")]
    line: Option<usize>,
    #[serde(rename = "oldStartLine", alias = "old_start_line")]
    old_line: Option<usize>,
}

impl SemInventory {
    pub async fn collect(
        store: &RepositoryStore,
        repository: Arc<RepositoryState>,
        observation: &[ObservedPath],
        check_owner: Arc<dyn Fn() -> Result<()> + Send + Sync>,
    ) -> Result<Self> {
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("Sem inventory requires a worktree")?;
        let mut untracked = Vec::new();
        let mut status = BTreeMap::new();
        for observed in observation {
            let record = &observed.change;
            let action = if matches!(record.state, PathState::Untracked)
                || record.staged == ChangeKind::Added
                || record.unstaged == ChangeKind::Added
            {
                "added"
            } else if record.staged == ChangeKind::Deleted || record.unstaged == ChangeKind::Deleted
            {
                "removed"
            } else {
                "modified"
            };
            status.insert(record.path.raw().to_vec(), action.to_owned());
            if matches!(record.state, PathState::Untracked) {
                untracked.push(forge_git::validate_path(&root, &record.path)?);
            }
        }
        ensure!(
            untracked
                .iter()
                .map(|path| path.as_os_str().as_encoded_bytes().len() + 1)
                .sum::<usize>()
                <= 24 * 1024,
            "Sem batched untracked request exceeds command admission"
        );
        let generation = repository.generation();
        let result = store
            .reads
            .submit(16 * 1024 * 1024, move |cancellation| {
                let started = Instant::now();
                let mut check = || {
                    cancellation.check()?;
                    check_owner()?;
                    ensure!(
                        started.elapsed() < Duration::from_secs(30),
                        "Sem inventory exceeded 30 seconds"
                    );
                    Ok(())
                };
                let tracked = run(
                    std::process::Command::new("sem")
                        .args(["diff", "HEAD", "--format", "json", "--no-cosmetics", "-C"])
                        .arg(&root),
                    &mut check,
                )?;
                let tracked: Tracked = serde_json::from_slice(&tracked)
                    .context("invalid Sem tracked inventory JSON")?;
                let added = if untracked.is_empty() {
                    Vec::new()
                } else {
                    let output = run(
                        std::process::Command::new("sem")
                            .args(["entities", "--format", "json"])
                            .args(&untracked),
                        &mut check,
                    )?;
                    decode_untracked(&output, &untracked)?
                };
                Self::normalize(&root, tracked.changes, added, status)
            })?
            .finish()
            .await?;
        ensure!(
            repository.generation() == generation,
            "Sem inventory was superseded by repository change"
        );
        Ok(result)
    }

    fn normalize(
        root: &Path,
        tracked: Vec<Entity>,
        added: Vec<Entity>,
        status: BTreeMap<Vec<u8>, String>,
    ) -> Result<Self> {
        ensure!(
            tracked.len() + added.len() <= 10_000,
            "Sem inventory exceeds 10000 entities"
        );
        let mut entry = Vec::new();
        let mut seen = HashSet::new();
        let mut touched = BTreeMap::new();
        for (entity, added) in tracked
            .into_iter()
            .map(|entity| (entity, false))
            .chain(added.into_iter().map(|entity| (entity, true)))
        {
            let path = inventory_path(root, &entity.file)?;
            touched.insert(path.raw().to_vec(), path.clone());
            let kind = entity.kind.to_ascii_lowercase();
            if ![
                "function",
                "struct",
                "class",
                "interface",
                "enum",
                "trait",
                "type",
                "module",
            ]
            .contains(&kind.as_str())
            {
                continue;
            }
            let action = if added {
                "added"
            } else {
                match entity.action.to_ascii_lowercase().as_str() {
                    "added" => "added",
                    "deleted" => "removed",
                    "modified" | "moved" | "renamed" | "reordered" => "modified",
                    _ => continue,
                }
            };
            if entity.name.is_empty() {
                continue;
            }
            let identity = (
                action.to_owned(),
                kind.clone(),
                path.raw().to_vec(),
                entity.identity.to_string(),
                entity.name.clone(),
            );
            if !seen.insert(identity) {
                continue;
            }
            entry.push(InventoryEntry {
                kind,
                action: action.into(),
                name: entity.name,
                path,
                line: entity
                    .line
                    .or(entity.old_line)
                    .map(|line| line.saturating_sub(1)),
            });
        }
        for (raw, path) in touched {
            let action = status.get(&raw).map_or("modified", String::as_str);
            let name = String::from_utf8(raw).context("Sem inventory filename is not UTF-8")?;
            entry.push(InventoryEntry {
                kind: "files".into(),
                action: action.into(),
                name: name.clone(),
                path: path.clone(),
                line: None,
            });
            if let Some(kind) = name.split('/').find(|part| {
                part.eq_ignore_ascii_case("docs") || part.eq_ignore_ascii_case("plans")
            }) {
                entry.push(InventoryEntry {
                    kind: kind.to_ascii_lowercase(),
                    action: action.into(),
                    name,
                    path,
                    line: None,
                });
            }
        }
        ensure!(
            entry.len() <= 30_000,
            "Sem normalized inventory exceeds admission"
        );
        Ok(Self { entry })
    }
}

fn decode_untracked(output: &[u8], requested: &[PathBuf]) -> Result<Vec<Entity>> {
    let mut entity: Vec<Entity> =
        serde_json::from_slice(output).context("invalid Sem untracked inventory JSON")?;
    for entry in &mut entity {
        if entry.file.is_empty() {
            let [path] = requested else {
                anyhow::bail!("Sem batched entity filename is missing");
            };
            entry.file = path
                .to_str()
                .context("Sem inventory requires UTF-8 filenames")?
                .to_owned();
        }
    }
    Ok(entity)
}

fn inventory_path(root: &Path, filename: &str) -> Result<RepositoryPath> {
    let filename = filename.replace('\\', "/");
    let path = Path::new(&filename);
    if !path.is_absolute() {
        ensure!(
            path.components()
                .all(|component| matches!(component, Component::Normal(_))),
            "Sem entity path must be worktree-relative or an absolute worktree path"
        );
        return RepositoryPath::new(filename.into_bytes());
    }
    let root = root
        .to_str()
        .context("Sem inventory requires a UTF-8 root")?
        .replace('\\', "/");
    let prefix = format!("{}/", root.trim_end_matches('/'));
    let matches = if cfg!(windows) {
        filename
            .get(..prefix.len())
            .is_some_and(|value| value.eq_ignore_ascii_case(&prefix))
    } else {
        filename.starts_with(&prefix)
    };
    ensure!(matches, "Sem entity is outside the resolved worktree");
    RepositoryPath::new(filename[prefix.len()..].as_bytes().to_vec())
}

fn run(
    command: &mut std::process::Command,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    let output = read_command(
        command,
        CommandLimits {
            stdout_bytes: 8 * 1024 * 1024,
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(30),
        },
        check,
    )
    .context("Sem inventory failed, no fallback inventory engine is available")?;
    ensure!(
        output.status.success(),
        "Sem inventory failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(output.stdout)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn native_inventory_uses_only_sem_entities_and_rejects_root_escape() {
        let root = if cfg!(windows) {
            Path::new("C:/repo")
        } else {
            Path::new("/repo")
        };
        let file = root.join("src/main.rs").to_string_lossy().into_owned();
        let entity: Entity = serde_json::from_value(serde_json::json!({"filePath":file,"entityType":"function","entityName":"run","changeType":"modified","startLine":8})).unwrap();
        let result =
            SemInventory::normalize(root, vec![entity], Vec::new(), BTreeMap::new()).unwrap();
        assert_eq!(result.entry.len(), 2);
        assert_eq!(result.entry[0].line, Some(7));
        assert_eq!(result.entry[1].kind, "files");
        let relative_entity: Entity = serde_json::from_value(serde_json::json!({
            "filePath": "src/main.rs", "entityType": "function", "entityName": "main",
            "changeType": "modified", "startLine": 1
        }))
        .unwrap();
        let relative_result =
            SemInventory::normalize(root, vec![relative_entity], Vec::new(), BTreeMap::new())
                .unwrap();
        assert_eq!(relative_result.entry[0].path.raw(), b"src/main.rs");
        assert_eq!(relative_result.entry[0].name, "main");
        for invalid in [
            "../outside.rs",
            "src/../../outside.rs",
            "src/../main.rs",
            "",
            "src//main.rs",
        ] {
            assert!(inventory_path(root, invalid).is_err(), "admitted {invalid}");
        }
        assert!(inventory_path(root, &format!("{}/../outside.rs", root.display())).is_err());
        assert!(
            inventory_path(
                root,
                if cfg!(windows) {
                    "C:/repository/src/main.rs"
                } else {
                    "/repository/src/main.rs"
                }
            )
            .is_err()
        );
        assert!(
            SemInventory::normalize(
                root,
                Vec::new(),
                Vec::new(),
                BTreeMap::from([(b"git-only.rs".to_vec(), "modified".into())])
            )
            .unwrap()
            .entry
            .is_empty()
        );
    }

    #[test]
    fn single_untracked_sem_output_inherits_only_its_requested_path() {
        let requested = [PathBuf::from("src/new.rs")];
        let output = br#"[{"name":"added","type":"function","start_line":3,"parent_id":null}]"#;
        let entity = decode_untracked(output, &requested).unwrap();
        assert_eq!(entity.len(), 1);
        assert_eq!(entity[0].file, "src/new.rs");
        assert_eq!(entity[0].line, Some(3));
        assert!(decode_untracked(output, &[]).is_err());
        assert!(
            decode_untracked(output, &[PathBuf::from("one.rs"), PathBuf::from("two.rs")]).is_err()
        );
        let named = br#"[{"file":"src/two.rs","name":"added","type":"function","start_line":3}]"#;
        assert_eq!(
            decode_untracked(named, &[PathBuf::from("one.rs"), PathBuf::from("two.rs")]).unwrap()
                [0]
            .file,
            "src/two.rs"
        );
    }
}
