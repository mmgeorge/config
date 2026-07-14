use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Represents one nonignored workspace file stored by content digest.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckpointFile {
    pub path: String,
    pub object_id: String,
}

/// Represents a complete worktree checkpoint without mutating Git history or index state.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CheckpointRecord {
    pub id: String,
    pub session_id: String,
    pub workspace: String,
    pub head: String,
    pub index_digest: String,
    pub file: Vec<CheckpointFile>,
    pub created_at_ms: i64,
}

/// Defines workspace snapshot operations consumed by interaction tracking.
pub trait WorkspaceSnapshot {
    /// Capture tracked and nonignored untracked files into durable object storage.
    fn capture(
        &self,
        store: &SqliteStore,
        session_id: &str,
        now_ms: i64,
    ) -> Result<CheckpointRecord>;

    /// Restore a target checkpoint after validating the expected current checkpoint.
    fn restore(
        &self,
        store: &SqliteStore,
        expected_current: &CheckpointRecord,
        target: &CheckpointRecord,
    ) -> Result<()>;
}

/// Owns Git-backed worktree checkpoint capture and safe rollback.
pub struct GitCheckpoint {
    workspace: PathBuf,
}

impl GitCheckpoint {
    /// Build a checkpoint adapter for one exact worktree root.
    pub fn new(workspace: impl Into<PathBuf>) -> Self {
        Self {
            workspace: workspace.into(),
        }
    }

    fn git_output(&self, args: &[&str]) -> Result<Vec<u8>> {
        let output = std::process::Command::new("git")
            .args(args)
            .current_dir(&self.workspace)
            .output()
            .with_context(|| format!("run git {}", args.join(" ")))?;
        anyhow::ensure!(
            output.status.success(),
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
        Ok(output.stdout)
    }

    fn file_path(&self, relative: &str) -> Result<PathBuf> {
        anyhow::ensure!(
            Path::new(relative).components().all(|component| matches!(
                component,
                std::path::Component::Normal(_) | std::path::Component::CurDir
            )),
            "checkpoint path escaped workspace: {relative}"
        );
        Ok(self.workspace.join(relative))
    }

    fn head(&self) -> Result<String> {
        let output = std::process::Command::new("git")
            .args(["rev-parse", "--verify", "HEAD"])
            .current_dir(&self.workspace)
            .output()
            .context("resolve Git HEAD")?;
        if output.status.success() {
            return Ok(String::from_utf8(output.stdout)?.trim().to_owned());
        }
        self.git_output(&["rev-parse", "--is-inside-work-tree"])?;
        Ok("UNBORN".into())
    }
}

impl WorkspaceSnapshot for GitCheckpoint {
    fn capture(
        &self,
        store: &SqliteStore,
        session_id: &str,
        now_ms: i64,
    ) -> Result<CheckpointRecord> {
        let head = self.head()?;
        let index_digest = crate::plan::digest(&self.git_output(&["ls-files", "-z", "--stage"])?);
        let listed = self.git_output(&[
            "ls-files",
            "-z",
            "--cached",
            "--others",
            "--exclude-standard",
        ])?;
        let mut file = Vec::new();
        for entry in listed
            .split(|byte| *byte == 0)
            .filter(|entry| !entry.is_empty())
        {
            let relative =
                String::from_utf8(entry.to_vec()).context("decode Git workspace path")?;
            let path = self.file_path(&relative)?;
            if !path.is_file() {
                continue;
            }
            let content = fs::read(&path)
                .with_context(|| format!("read checkpoint file {}", path.display()))?;
            file.push(CheckpointFile {
                path: relative,
                object_id: store.put_object(&content)?,
            });
        }
        file.sort_by(|left, right| left.path.cmp(&right.path));
        let manifest = serde_json::to_vec(&(session_id, &head, &index_digest, &file))?;
        Ok(CheckpointRecord {
            id: crate::plan::digest(&manifest),
            session_id: session_id.into(),
            workspace: self.workspace.to_string_lossy().into_owned(),
            head,
            index_digest,
            file,
            created_at_ms: now_ms,
        })
    }

    fn restore(
        &self,
        store: &SqliteStore,
        expected_current: &CheckpointRecord,
        target: &CheckpointRecord,
    ) -> Result<()> {
        anyhow::ensure!(
            expected_current.workspace == target.workspace,
            "rollback crossed a worktree boundary"
        );
        let actual = self.capture(
            store,
            &expected_current.session_id,
            expected_current.created_at_ms,
        )?;
        anyhow::ensure!(
            actual.head == expected_current.head,
            "HEAD changed after the interaction completed"
        );
        anyhow::ensure!(
            actual.index_digest == expected_current.index_digest,
            "Git index changed after the interaction completed"
        );
        anyhow::ensure!(
            actual.id == expected_current.id,
            "workspace diverged after the interaction completed"
        );
        anyhow::ensure!(
            target.head == expected_current.head,
            "rollback would cross a HEAD boundary"
        );
        anyhow::ensure!(
            target.index_digest == expected_current.index_digest,
            "rollback would cross an index boundary"
        );

        let actual_file: BTreeMap<_, _> = actual
            .file
            .iter()
            .map(|item| (&item.path, &item.object_id))
            .collect();
        let target_file: BTreeMap<_, _> = target
            .file
            .iter()
            .map(|item| (&item.path, &item.object_id))
            .collect();
        for relative in actual_file
            .keys()
            .filter(|path| !target_file.contains_key(*path))
        {
            let path = self.file_path(relative)?;
            if path.exists() {
                fs::remove_file(&path)
                    .with_context(|| format!("remove rollback file {}", path.display()))?;
            }
        }
        for item in &target.file {
            if actual_file.get(&item.path) == Some(&&item.object_id) {
                continue;
            }
            let path = self.file_path(&item.path)?;
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&path, store.get_object(&item.object_id)?)
                .with_context(|| format!("restore rollback file {}", path.display()))?;
        }
        Ok(())
    }
}

/// Build the exact per-interaction diff between two durable checkpoints.
pub fn checkpoint_diff(
    store: &SqliteStore,
    before: &CheckpointRecord,
    after: &CheckpointRecord,
) -> Result<String> {
    let before_file: BTreeMap<_, _> = before
        .file
        .iter()
        .map(|item| (item.path.as_str(), item.object_id.as_str()))
        .collect();
    let after_file: BTreeMap<_, _> = after
        .file
        .iter()
        .map(|item| (item.path.as_str(), item.object_id.as_str()))
        .collect();
    let mut path_set = std::collections::BTreeSet::new();
    path_set.extend(before_file.keys().copied());
    path_set.extend(after_file.keys().copied());
    let mut output = String::new();
    for path in path_set {
        let before_id = before_file.get(path).copied();
        let after_id = after_file.get(path).copied();
        if before_id == after_id {
            continue;
        }
        let before_content = before_id
            .map(|id| store.get_object(id))
            .transpose()?
            .unwrap_or_default();
        let after_content = after_id
            .map(|id| store.get_object(id))
            .transpose()?
            .unwrap_or_default();
        output.push_str(&format!("diff --git a/{path} b/{path}\n"));
        let (Ok(before_text), Ok(after_text)) = (
            std::str::from_utf8(&before_content),
            std::str::from_utf8(&after_content),
        ) else {
            output.push_str(&format!("Binary files a/{path} and b/{path} differ\n"));
            continue;
        };
        let diff = similar::TextDiff::from_lines(before_text, after_text);
        let before_header = if before_id.is_some() {
            format!("a/{path}")
        } else {
            "/dev/null".into()
        };
        let after_header = if after_id.is_some() {
            format!("b/{path}")
        } else {
            "/dev/null".into()
        };
        output.push_str(
            &diff
                .unified_diff()
                .context_radius(3)
                .header(&before_header, &after_header)
                .to_string(),
        );
    }
    Ok(output)
}

#[cfg(test)]
mod test {
    use super::*;

    fn git(workspace: &Path, args: &[&str]) {
        let status = std::process::Command::new("git")
            .args(args)
            .current_dir(workspace)
            .status()
            .unwrap();
        assert!(status.success(), "git {} failed", args.join(" "));
    }

    fn repository() -> tempfile::TempDir {
        let temporary = tempfile::tempdir().unwrap();
        git(temporary.path(), &["init", "-q"]);
        git(
            temporary.path(),
            &["config", "user.email", "harness@example.invalid"],
        );
        git(temporary.path(), &["config", "user.name", "Harness Test"]);
        fs::write(
            temporary.path().join(".gitignore"),
            "ignored.tmp\ntarget/\n",
        )
        .unwrap();
        fs::write(temporary.path().join("tracked.txt"), "before\n").unwrap();
        git(temporary.path(), &["add", "."]);
        git(temporary.path(), &["commit", "-qm", "seed"]);
        temporary
    }

    #[test]
    fn diffs_and_restores_tracked_and_nonignored_untracked_files() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot.capture(&store, "session", 1).unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        fs::write(repository.path().join("new.txt"), "new\n").unwrap();
        fs::write(repository.path().join("ignored.tmp"), "ignored\n").unwrap();
        fs::create_dir_all(repository.path().join("target/generated/deep")).unwrap();
        fs::write(
            repository.path().join("target/generated/deep/artifact.txt"),
            "ignored nested artifact\n",
        )
        .unwrap();
        let after = snapshot.capture(&store, "session", 2).unwrap();
        let diff = checkpoint_diff(&store, &before, &after).unwrap();
        assert!(diff.contains("tracked.txt"));
        assert!(diff.contains("new.txt"));
        assert!(!diff.contains("ignored.tmp"));
        assert!(!diff.contains("target/generated/deep/artifact.txt"));

        snapshot.restore(&store, &after, &before).unwrap();
        assert_eq!(
            fs::read_to_string(repository.path().join("tracked.txt")).unwrap(),
            "before\n"
        );
        assert!(!repository.path().join("new.txt").exists());
        assert!(repository.path().join("ignored.tmp").exists());
        assert!(
            repository
                .path()
                .join("target/generated/deep/artifact.txt")
                .exists()
        );
    }

    #[test]
    fn refuses_rollback_after_workspace_divergence() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let before = snapshot.capture(&store, "session", 1).unwrap();
        fs::write(repository.path().join("tracked.txt"), "after\n").unwrap();
        let after = snapshot.capture(&store, "session", 2).unwrap();
        fs::write(
            repository.path().join("tracked.txt"),
            "unrelated later edit\n",
        )
        .unwrap();
        assert!(snapshot.restore(&store, &after, &before).is_err());
    }

    #[test]
    fn rejects_checkpoint_paths_outside_the_worktree() {
        let repository = repository();
        let snapshot = GitCheckpoint::new(repository.path());
        assert!(snapshot.file_path("../escape.txt").is_err());
        assert!(snapshot.file_path("/absolute.txt").is_err());
    }

    #[test]
    fn scopes_identical_checkpoint_content_to_its_session() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let snapshot = GitCheckpoint::new(repository.path());
        let first = snapshot.capture(&store, "session-one", 1).unwrap();
        let second = snapshot.capture(&store, "session-two", 1).unwrap();
        assert_ne!(first.id, second.id);
    }

    #[test]
    fn captures_an_unborn_git_worktree() {
        let repository = tempfile::tempdir().unwrap();
        git(repository.path(), &["init", "-q"]);
        fs::write(repository.path().join("first.txt"), "first\n").unwrap();
        let data = tempfile::tempdir().unwrap();
        let store = SqliteStore::open(data.path()).unwrap();
        let checkpoint = GitCheckpoint::new(repository.path())
            .capture(&store, "session", 1)
            .unwrap();
        assert_eq!(checkpoint.head, "UNBORN");
        assert_eq!(checkpoint.file.len(), 1);
    }
}
