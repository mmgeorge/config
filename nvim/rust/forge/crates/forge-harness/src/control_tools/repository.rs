use std::path::PathBuf;
use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

use anyhow::{Context, Result, ensure};
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::workers::WorkPriority;
use forge_git::RepositoryPath;
use forge_git::content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion};
use forge_git::repository::RepositoryState;
use forge_git::snapshot::{ChangeKind, HeadState, PathRecord, PathState};
use forge_git::store::RepositoryStore;
use serde::Deserialize;
use serde_json::{Value, json};

use crate::permissions::document::PermissionDecision;
use crate::permissions::matcher::{
    CompiledPermissionDocument, PermissionRequest, PermissionTarget,
};

const MAX_REPLY_BYTES: usize = 256 * 1024;

/// Restricts repository inspection to one live broker interaction and its captured read policy.
pub struct RepositoryToolScope {
    session: String,
    interaction: String,
    workspace: PathBuf,
    policy: Arc<CompiledPermissionDocument>,
    repository: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    active: AtomicBool,
    cancelled: Arc<AtomicBool>,
}

pub(crate) struct RepositoryToolLease(Arc<RepositoryToolScope>);

impl Drop for RepositoryToolLease {
    fn drop(&mut self) {
        self.0.active.store(false, Ordering::Release);
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Inspection {
    #[serde(default)]
    offset: usize,
    #[serde(default)]
    side: DiffSide,
    path_hex: Option<String>,
}

#[derive(Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
enum DiffSide {
    Staged,
    #[default]
    Unstaged,
}

impl RepositoryToolScope {
    pub(crate) fn open(
        session: String,
        interaction: String,
        workspace: PathBuf,
        policy: Arc<CompiledPermissionDocument>,
        cancelled: Arc<AtomicBool>,
        repository: Arc<RepositoryStore>,
        diff: Arc<DiffEngine>,
    ) -> Result<(Arc<Self>, RepositoryToolLease)> {
        ensure!(
            !session.is_empty()
                && session.len() <= 256
                && !interaction.is_empty()
                && interaction.len() <= 256,
            "repository tools require an active session and interaction"
        );
        ensure!(
            workspace.is_absolute() && policy.workspace() == workspace,
            "repository tool workspace differs from the active policy"
        );
        let scope = Arc::new(Self {
            session,
            interaction,
            workspace,
            policy,
            repository,
            diff,
            active: AtomicBool::new(true),
            cancelled,
        });
        Ok((Arc::clone(&scope), RepositoryToolLease(scope)))
    }

    fn check(&self) -> Result<()> {
        ensure!(
            self.active.load(Ordering::Acquire) && !self.cancelled.load(Ordering::Acquire),
            "repository inspection belongs to an inactive interaction"
        );
        Ok(())
    }

    async fn authorize(
        &self,
        repository: &Arc<RepositoryState>,
        path: &RepositoryPath,
    ) -> Result<()> {
        self.check()?;
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("repository has no worktree")?;
        let path = path.clone();
        let input_bytes = path.retained_bytes() + root.as_os_str().len() * 2;
        let absolute = self
            .repository
            .read(
                Arc::clone(repository),
                input_bytes,
                move |_, cancellation| {
                    cancellation.check()?;
                    let result = forge_git::validate_path(&root, &path)?;
                    cancellation.check()?;
                    Ok(result)
                },
            )
            .await?
            .value;
        self.check()?;
        let path = absolute
            .to_str()
            .context("repository path cannot be represented by the active read policy")?;
        let evaluation = self.policy.evaluate(&PermissionRequest {
            id: self.interaction.clone(),
            provider: "harness-repository".into(),
            reason: None,
            target_list: vec![PermissionTarget::Read { path: path.into() }],
        })?;
        ensure!(
            evaluation.decision == PermissionDecision::Allow,
            "active policy does not authorize repository path inspection"
        );
        Ok(())
    }

    pub(crate) async fn invoke(&self, name: &str, arguments: Value) -> Result<String> {
        self.check()?;
        ensure!(
            matches!(
                name,
                "repository_status"
                    | "repository_changed_paths"
                    | "repository_diff"
                    | "repository_file_diff"
            ),
            "unknown repository inspection tool"
        );
        ensure!(
            self.policy
                .evaluate(&PermissionRequest {
                    id: self.interaction.clone(),
                    provider: "harness-repository".into(),
                    reason: None,
                    target_list: vec![PermissionTarget::Read {
                        path: self
                            .workspace
                            .to_str()
                            .context("workspace cannot be represented by read policy")?
                            .into()
                    }]
                })?
                .decision
                == PermissionDecision::Allow,
            "active policy does not authorize repository inspection"
        );
        let input: Inspection = serde_json::from_value(arguments)?;
        let selected_path = input
            .path_hex
            .as_deref()
            .map(|path| {
                RepositoryPath::new(
                    hex::decode(path)
                        .context("path_hex must encode exact repository path bytes")?,
                )
            })
            .transpose()?;
        if name == "repository_file_diff" {
            ensure!(selected_path.is_some(), "file_diff requires path_hex");
        }
        let operation = async {
            let repository = self
                .repository
                .open(self.workspace.clone())
                .await?
                .context("interaction workspace is not a Git repository")?;
            self.check()?;
            let snapshot = repository.snapshot(&self.repository).await?;
            self.check()?;
            let mut result = Vec::new();
            let mut retained = 0usize;
            let is_diff = matches!(name, "repository_diff" | "repository_file_diff");
            let limit = if is_diff { 8 } else { 256 };
            let selected: Vec<_> = snapshot
                .path
                .iter()
                .filter(|path| {
                    selected_path
                        .as_ref()
                        .is_none_or(|selected| path.change.path == *selected)
                })
                .collect();
            let head = match &snapshot.head {
                HeadState::Unborn { reference } => {
                    json!({ "kind": "unborn", "reference_hex": hex::encode(reference) })
                }
                HeadState::Attached { reference, target } => {
                    json!({ "kind": "attached", "reference_hex": hex::encode(reference), "object": target.to_string() })
                }
                HeadState::Detached { target } => {
                    json!({ "kind": "detached", "object": target.to_string() })
                }
            };
            let envelope = json!({ "session": self.session, "interaction": self.interaction, "head": head,
                "items": [], "total": selected.len(), "next_offset": selected.len(), "fresh_observation": true });
            let capacity = MAX_REPLY_BYTES.saturating_sub(serde_json::to_vec(&envelope)?.len() + 1);
            drop(envelope);
            for observed in selected.iter().skip(input.offset).take(limit) {
                self.authorize(&repository, &observed.change.path).await?;
                if let PathState::Tracked {
                    relocation: Some(relocation),
                    ..
                } = &observed.change.state
                {
                    self.authorize(&repository, &relocation.origin).await?;
                }
                let record = if is_diff {
                    self.file_diff(&repository, &observed.change, input.side)
                        .await?
                } else if name == "repository_changed_paths" {
                    path_value(&observed.change.path)
                } else {
                    json!({ "path": path_value(&observed.change.path), "origin": relocation_value(&observed.change), "staged": change_name(observed.change.staged), "unstaged": change_name(observed.change.unstaged) })
                };
                if !append_record(&mut result, &mut retained, record, capacity)? {
                    break;
                }
            }
            self.check()?;
            let end = (input.offset + result.len()).min(selected.len());
            let value = json!({ "session": self.session, "interaction": self.interaction, "head": head,
                "items": result, "total": selected.len(), "next_offset": (end < selected.len()).then_some(end),
                "fresh_observation": true });
            Ok::<_, anyhow::Error>(serde_json::to_string(&value)?)
        };
        self.collect(operation).await
    }

    async fn collect(
        &self,
        operation: impl std::future::Future<Output = Result<String>>,
    ) -> Result<String> {
        self.check()?;
        let result = tokio::time::timeout(std::time::Duration::from_secs(30), operation)
            .await
            .context("repository inspection exceeded 30 seconds")??;
        self.check()?;
        Ok(result)
    }

    async fn file_diff(
        &self,
        repository: &Arc<RepositoryState>,
        path: &PathRecord,
        side: DiffSide,
    ) -> Result<Value> {
        let change = match (&path.state, side) {
            (PathState::Untracked, DiffSide::Unstaged) => ChangeKind::Added,
            (_, DiffSide::Staged) => path.staged,
            (_, DiffSide::Unstaged) => path.unstaged,
        };
        if change == ChangeKind::Unchanged {
            return Ok(json!({ "path": path_value(&path.path), "state": "unchanged" }));
        }
        let empty = || {
            SourceVersion::new(Vec::new(), Representation::GitCanonical)
                .map(ContentSource::Supplied)
                .map_err(anyhow::Error::from)
        };
        let (old, new) = match (&path.state, side) {
            (PathState::Tracked { head, index, .. }, DiffSide::Staged) => (
                if head.mode == 0 {
                    empty()?
                } else {
                    ContentSource::Object(head.object)
                },
                if index.mode == 0 {
                    empty()?
                } else {
                    ContentSource::Object(index.object)
                },
            ),
            (PathState::Tracked { index, .. }, DiffSide::Unstaged) => (
                if index.mode == 0 {
                    empty()?
                } else {
                    ContentSource::Object(index.object)
                },
                ContentSource::Worktree {
                    path: path.path.clone(),
                    conversion: WorktreeConversion::GitCanonical,
                },
            ),
            (PathState::Untracked, DiffSide::Unstaged) => (
                empty()?,
                ContentSource::Worktree {
                    path: path.path.clone(),
                    conversion: WorktreeConversion::GitCanonical,
                },
            ),
            _ => {
                return Ok(
                    json!({ "path": path_value(&path.path), "state": "unsupported", "reason": "conflicted or ignored source requires explicit inspection" }),
                );
            }
        };
        let old = self.content(repository, old).await?;
        let new = self.content(repository, new).await?;
        let (old, new) = match (old, new) {
            (Ok(old), Ok(new)) => (old, new),
            (Err(reason), _) | (_, Err(reason)) => {
                return Ok(json!({ "path": path_value(&path.path), "state": reason }));
            }
        };
        self.check()?;
        let analysis = self
            .diff
            .compare(DiffRequest {
                source: SourcePair { old, new },
                priority: WorkPriority::Foreground,
            })
            .await
            .map_err(|error| anyhow::anyhow!("repository diff unavailable: {error:?}"))?;
        self.check()?;
        let mut hunks = Vec::new();
        for hunk in analysis.hunks() {
            let value = json!({ "old_lines": [hunk.old_lines.start, hunk.old_lines.end], "new_lines": [hunk.new_lines.start, hunk.new_lines.end],
                "removed": std::str::from_utf8(&analysis.source().old.bytes()[hunk.old_bytes.clone()])?,
                "added": std::str::from_utf8(&analysis.source().new.bytes()[hunk.new_bytes.clone()])? });
            hunks.push(value);
        }
        let record = json!({ "path": path_value(&path.path), "origin": relocation_value(path), "state": "ready", "hunks": hunks,
            "old_identity": hex::encode(analysis.source().old.identity().content_hash), "new_identity": hex::encode(analysis.source().new.identity().content_hash) });
        Ok(record)
    }

    async fn content(
        &self,
        repository: &Arc<RepositoryState>,
        source: ContentSource,
    ) -> Result<std::result::Result<SourceVersion, &'static str>> {
        self.check()?;
        let acquired = repository
            .content(
                &self.repository,
                ContentRequest {
                    source,
                    expected: None,
                },
            )
            .await?;
        self.check()?;
        Ok(match acquired.value {
            ContentResult::Ready(content) => Ok(content.source),
            ContentResult::Missing => Ok(SourceVersion::new(
                Vec::new(),
                Representation::GitCanonical,
            )?),
            ContentResult::Binary => Err("binary"),
            ContentResult::Unavailable(_) => Err("unavailable"),
            ContentResult::Failed(error) => return Err(error),
        })
    }
}

fn path_value(path: &RepositoryPath) -> Value {
    json!({ "path_hex": hex::encode(path.raw()), "display": path.display_label() })
}

fn relocation_value(path: &PathRecord) -> Value {
    match &path.state {
        PathState::Tracked {
            relocation: Some(relocation),
            ..
        } => path_value(&relocation.origin),
        _ => Value::Null,
    }
}

fn append_record(
    records: &mut Vec<Value>,
    retained: &mut usize,
    record: Value,
    capacity: usize,
) -> Result<bool> {
    let bytes = serde_json::to_vec(&record)?.len();
    if !records.is_empty() && bytes > capacity.saturating_sub(*retained + 1) {
        return Ok(false);
    }
    *retained += bytes + 1;
    records.push(record);
    Ok(true)
}
fn change_name(change: ChangeKind) -> &'static str {
    match change {
        ChangeKind::Unchanged => "unchanged",
        ChangeKind::Modified => "modified",
        ChangeKind::TypeChanged => "type_changed",
        ChangeKind::Added => "added",
        ChangeKind::Deleted => "deleted",
        ChangeKind::Renamed => "renamed",
        ChangeKind::Copied => "copied",
        ChangeKind::Unmerged => "unmerged",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::permissions::document::default_permission_document;

    fn services() -> (Arc<RepositoryStore>, Arc<DiffEngine>) {
        (
            Arc::new(RepositoryStore::default()),
            DiffEngine::new(
                forge_diff::cache::CacheLimits {
                    entries: 8,
                    source_bytes: 4 * 1024 * 1024,
                    result_bytes: 1024 * 1024,
                },
                4,
            ),
        )
    }

    #[test]
    fn pages_preserve_unreturned_records_and_accept_oversized_first_records() {
        let mut records = Vec::new();
        let mut retained = 0;
        let record = json!({ "text": "\\".repeat(100000) });
        assert!(
            append_record(&mut records, &mut retained, record.clone(), MAX_REPLY_BYTES).unwrap()
        );
        let accepted = retained;
        assert!(!append_record(&mut records, &mut retained, record, MAX_REPLY_BYTES).unwrap());
        assert_eq!(records.len(), 1);
        assert_eq!(retained, accepted);
        assert!(retained > 200000);
        let mut large = Vec::new();
        assert!(
            append_record(
                &mut large,
                &mut 0,
                json!({ "text": "x".repeat(MAX_REPLY_BYTES) }),
                MAX_REPLY_BYTES,
            )
            .unwrap()
        );
        assert_eq!(large[0]["text"].as_str().unwrap().len(), MAX_REPLY_BYTES);
    }

    #[tokio::test]
    async fn inspection_inherits_read_denial_and_active_cancellation() {
        let directory = tempfile::tempdir().unwrap();
        let workspace = directory.path().to_path_buf();
        let mut policy = default_permission_document();
        policy
            .permission
            .get_mut("read")
            .unwrap()
            .insert("*".into(), PermissionDecision::Deny);
        let policy = Arc::new(CompiledPermissionDocument::compile(
            policy,
            workspace.clone(),
        ));
        let (repository, diff) = services();
        let cancelled = Arc::new(AtomicBool::new(false));
        let (scope, _lease) = RepositoryToolScope::open(
            "session".into(),
            "interaction".into(),
            workspace,
            policy,
            Arc::clone(&cancelled),
            repository,
            diff,
        )
        .unwrap();
        assert!(
            scope
                .invoke("repository_status", json!({}))
                .await
                .unwrap_err()
                .to_string()
                .contains("active policy")
        );
        cancelled.store(true, Ordering::Release);
        assert!(scope.check().is_err());
    }

    #[tokio::test]
    async fn inspection_discards_completed_read_after_interaction_revocation() {
        let directory = tempfile::tempdir().unwrap();
        let workspace = directory.path().to_path_buf();
        let policy = Arc::new(CompiledPermissionDocument::compile(
            default_permission_document(),
            workspace.clone(),
        ));
        let (repository, diff) = services();
        let (scope, lease) = RepositoryToolScope::open(
            "session".into(),
            "interaction".into(),
            workspace,
            policy,
            Arc::new(AtomicBool::new(false)),
            repository,
            diff,
        )
        .unwrap();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let read = scope.collect(async {
            receiver.await?;
            Ok("private repository content".into())
        });
        tokio::pin!(read);
        assert!(futures_util::poll!(&mut read).is_pending());
        drop(lease);
        sender.send(()).unwrap();
        assert!(
            read.await
                .unwrap_err()
                .to_string()
                .contains("inactive interaction")
        );
    }

    #[tokio::test]
    async fn inspection_rejects_wrong_workspace_inactive_interaction_and_forged_identity() {
        let directory = tempfile::tempdir().unwrap();
        let workspace = directory.path().to_path_buf();
        let policy = Arc::new(CompiledPermissionDocument::compile(
            default_permission_document(),
            workspace.clone(),
        ));
        let (repository, diff) = services();
        assert!(
            RepositoryToolScope::open(
                "session".into(),
                "interaction".into(),
                workspace.join("other"),
                Arc::clone(&policy),
                Arc::new(AtomicBool::new(false)),
                Arc::clone(&repository),
                Arc::clone(&diff)
            )
            .is_err()
        );
        let (scope, lease) = RepositoryToolScope::open(
            "session".into(),
            "interaction".into(),
            workspace,
            policy,
            Arc::new(AtomicBool::new(false)),
            repository,
            diff,
        )
        .unwrap();
        assert!(
            scope
                .invoke("repository_status", json!({ "workspace": "another" }))
                .await
                .is_err()
        );
        drop(lease);
        assert!(
            scope
                .invoke("repository_status", json!({}))
                .await
                .unwrap_err()
                .to_string()
                .contains("inactive interaction")
        );
        let mut runtime = crate::control_tools::ControlToolRuntime::new(
            crate::control_tools::ControlTurnContext::inactive(crate::backend::PromptMode::Chat),
        );
        assert!(
            runtime
                .invoke(crate::control_tools::ControlToolInvocation {
                    name: "repository_status".into(),
                    arguments: json!({})
                })
                .await
                .unwrap_err()
                .to_string()
                .contains("authenticated active interaction")
        );
    }

    #[tokio::test]
    async fn inspection_uses_shared_repository_and_diff_without_status_publication() {
        let directory = tempfile::tempdir().unwrap();
        let workspace = directory.path().to_path_buf();
        let initialization_workspace = workspace.clone();
        let output = tokio::task::spawn_blocking(move || {
            let mut command = std::process::Command::new("git");
            command
                .args(["init", "--quiet"])
                .current_dir(initialization_workspace);
            forge_git::command::write_command(
                &mut command,
                forge_git::command::CommandLimits {
                    stderr_bytes: 4 * 1024,
                    timeout: std::time::Duration::from_secs(5),
                },
                &[],
                || Ok(()),
            )
        })
        .await
        .unwrap()
        .unwrap();
        assert!(output.status.success());
        std::fs::write(workspace.join("example.txt"), "literal **content**\n").unwrap();
        let (repository, diff) = services();
        let state = repository.open(workspace.clone()).await.unwrap().unwrap();
        let policy = Arc::new(CompiledPermissionDocument::compile(
            default_permission_document(),
            workspace.clone(),
        ));
        let (scope, lease) = RepositoryToolScope::open(
            "session".into(),
            "interaction".into(),
            workspace,
            policy,
            Arc::new(AtomicBool::new(false)),
            Arc::clone(&repository),
            Arc::clone(&diff),
        )
        .unwrap();
        let status: Value =
            serde_json::from_str(&scope.invoke("repository_status", json!({})).await.unwrap())
                .unwrap();
        assert_eq!(status["session"], "session");
        assert_eq!(
            status["items"][0]["path"]["path_hex"],
            hex::encode(b"example.txt")
        );
        let result: Value = serde_json::from_str(
            &scope
                .invoke(
                    "repository_file_diff",
                    json!({ "path_hex": hex::encode(b"example.txt"), "side": "unstaged" }),
                )
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(
            result["items"][0]["hunks"][0]["added"], "literal **content**\n",
            "repository file diff result: {result}"
        );
        std::fs::write(scope.workspace.join("quoted.txt"), "\\".repeat(128 * 1024)).unwrap();
        let bounded: Value = serde_json::from_str(
            &scope
                .invoke(
                    "repository_file_diff",
                    json!({ "path_hex": hex::encode(b"quoted.txt") }),
                )
                .await
                .unwrap(),
        )
        .unwrap();
        assert_eq!(bounded["items"][0]["state"], "ready");
        assert_eq!(
            bounded["items"][0]["hunks"][0]["added"],
            "\\".repeat(128 * 1024)
        );
        assert!(state.current_observation().is_none());
        drop(lease);
        assert!(scope.check().is_err());
        repository.shutdown(std::time::Duration::from_secs(2)).await;
        diff.shutdown(std::time::Duration::from_secs(2)).await;
    }
}
