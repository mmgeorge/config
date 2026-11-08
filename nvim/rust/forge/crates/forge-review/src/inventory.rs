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
    RepositoryPath::new(filename.as_bytes()[prefix.len()..].to_vec())
}

fn run(
    command: &mut std::process::Command,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    let output = read_command(
        command,
        CommandLimits {
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
