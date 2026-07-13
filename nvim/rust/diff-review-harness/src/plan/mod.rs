use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;

mod prompt;

pub use prompt::PlanPrompt;

/// Represents the review lifecycle of one model-authored plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanState {
    Generating,
    AwaitingReview,
    Revising,
    Accepted,
    Rejected,
    Cancelled,
    Failed,
}

/// Represents one durable plan and the exact digest under review.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanRecord {
    pub id: String,
    pub session_id: String,
    pub request: String,
    #[serde(default)]
    pub title: String,
    pub state: PlanState,
    pub working_path: String,
    pub model_revision: u32,
    pub user_revision: u32,
    pub review_digest: Option<String>,
    pub accepted_digest: Option<String>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

/// Defines one durable event in a reviewed plan lifecycle.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanLifecycleKind {
    Created,
    ChangesRequested,
    RevisionCreated,
    Accepted,
    Cancelled,
}

/// Represents one immutable plan lifecycle event in the session timeline.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanLifecycleRecord {
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub kind: PlanLifecycleKind,
    pub model_revision: u32,
    pub user_revision: u32,
    pub overall_comment: Option<String>,
    #[serde(default)]
    pub annotation: Vec<PlanAnnotation>,
    pub created_at_ms: i64,
}

/// Defines terminal and nonterminal states for one accepted plan execution.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanExecutionState {
    Active,
    Complete,
    Paused,
    Stalled,
    Blocked,
}

/// Tracks one accepted plan through its guarded execution goal.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanExecutionRecord {
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub goal_id: String,
    pub state: PlanExecutionState,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
}

/// Describes one plan artifact for the Harness picker and winbar.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ArtifactSummary {
    pub id: String,
    pub title: String,
    pub state: PlanState,
    pub working_path: String,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

impl From<&PlanRecord> for ArtifactSummary {
    fn from(plan: &PlanRecord) -> Self {
        Self {
            id: plan.id.clone(),
            title: plan.title.clone(),
            state: plan.state,
            working_path: plan.working_path.clone(),
            created_at_ms: plan.created_at_ms,
            updated_at_ms: plan.updated_at_ms,
        }
    }
}

/// Represents one annotation anchored to a reviewed plan line.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAnnotation {
    pub line: u32,
    pub body: String,
}

/// Owns physical plan files and immutable revision history.
pub struct PlanFileStore {
    root: PathBuf,
}

impl PlanFileStore {
    /// Build a plan file store beneath the Harness data directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// Write a model revision and refresh the editable working copy.
    pub fn write_model_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        markdown: &str,
    ) -> Result<(PathBuf, String)> {
        let directory = self.plan_dir(session_id, plan_id);
        fs::create_dir_all(directory.join("revisions"))
            .with_context(|| format!("create plan directory {}", directory.display()))?;
        let revision_path = directory
            .join("revisions")
            .join(format!("model-{revision:04}.md"));
        fs::write(&revision_path, markdown)
            .with_context(|| format!("write model plan revision {}", revision_path.display()))?;
        let working_path = directory.join("working.md");
        fs::write(&working_path, markdown)
            .with_context(|| format!("write working plan {}", working_path.display()))?;
        Ok((working_path, digest(markdown.as_bytes())))
    }

    /// Save the complete edited plan as an immutable user revision.
    pub fn save_user_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<(String, String)> {
        let directory = self.plan_dir(session_id, plan_id);
        let working_path = directory.join("working.md");
        let markdown = fs::read_to_string(&working_path)
            .with_context(|| format!("read working plan {}", working_path.display()))?;
        let revision_path = directory
            .join("revisions")
            .join(format!("user-{revision:04}.md"));
        fs::write(&revision_path, &markdown)
            .with_context(|| format!("write user plan revision {}", revision_path.display()))?;
        Ok((markdown.clone(), digest(markdown.as_bytes())))
    }

    /// Read and hash the editable working plan before accepting it.
    pub fn read_working(&self, session_id: &str, plan_id: &str) -> Result<(String, String)> {
        let working_path = self.plan_dir(session_id, plan_id).join("working.md");
        let markdown = fs::read_to_string(&working_path)
            .with_context(|| format!("read working plan {}", working_path.display()))?;
        let checksum = digest(markdown.as_bytes());
        Ok((markdown, checksum))
    }

    /// Read one immutable model revision for timeline expansion.
    pub fn read_model_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<String> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("model-{revision:04}.md"));
        fs::read_to_string(&path)
            .with_context(|| format!("read model plan revision {}", path.display()))
    }

    /// Read one immutable user revision for timeline expansion.
    pub fn read_user_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<String> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("user-{revision:04}.md"));
        fs::read_to_string(&path)
            .with_context(|| format!("read user plan revision {}", path.display()))
    }

    /// Build the user-edit diff against the last immutable model revision.
    pub fn user_edit_diff(
        &self,
        session_id: &str,
        plan_id: &str,
        model_revision: u32,
        edited: &str,
    ) -> Result<String> {
        let revision_path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("model-{model_revision:04}.md"));
        let model = fs::read_to_string(&revision_path)
            .with_context(|| format!("read model plan revision {}", revision_path.display()))?;
        Ok(similar::TextDiff::from_lines(&model, edited)
            .unified_diff()
            .context_radius(3)
            .header("model-plan.md", "user-edited-plan.md")
            .to_string())
    }

    /// Delete physical plan files for one removed Harness session.
    pub fn delete_session(&self, session_id: &str) -> Result<()> {
        let session_path = PathBuf::from(session_id);
        let mut component = session_path.components();
        anyhow::ensure!(
            matches!(component.next(), Some(std::path::Component::Normal(_)))
                && component.next().is_none(),
            "invalid Harness session identifier"
        );
        let directory = self.root.join("plans").join(session_id);
        if directory.exists() {
            fs::remove_dir_all(&directory).with_context(|| {
                format!("delete session plan directory {}", directory.display())
            })?;
        }
        Ok(())
    }

    /// Resolve the physical editable path for Neovim PlanReview.
    pub fn working_path(&self, session_id: &str, plan_id: &str) -> PathBuf {
        self.plan_dir(session_id, plan_id).join("working.md")
    }

    /// Copy one complete plan artifact into a forked Harness session.
    pub fn copy_plan(
        &self,
        source_session_id: &str,
        source_plan_id: &str,
        target_session_id: &str,
        target_plan_id: &str,
    ) -> Result<PathBuf> {
        let source = self.plan_dir(source_session_id, source_plan_id);
        let target = self.plan_dir(target_session_id, target_plan_id);
        fs::create_dir_all(target.join("revisions"))?;
        fs::copy(source.join("working.md"), target.join("working.md"))?;
        let source_revision = source.join("revisions");
        if source_revision.exists() {
            for entry in fs::read_dir(&source_revision)? {
                let entry = entry?;
                if entry.file_type()?.is_file() {
                    fs::copy(
                        entry.path(),
                        target.join("revisions").join(entry.file_name()),
                    )?;
                }
            }
        }
        Ok(target.join("working.md"))
    }

    fn plan_dir(&self, session_id: &str, plan_id: &str) -> PathBuf {
        self.root.join("plans").join(session_id).join(plan_id)
    }
}

/// Resolve a stable content digest for immutable plan acceptance.
pub fn digest(content: &[u8]) -> String {
    hex::encode(Sha256::digest(content))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn preserves_model_and_user_revisions() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path());
        let (working, first_digest) = store
            .write_model_revision("session", "plan", 1, "# Plan\n\nInitial")
            .unwrap();
        fs::write(&working, "# Plan\n\nEdited").unwrap();
        let (edited, edited_digest) = store.save_user_revision("session", "plan", 1).unwrap();
        assert_eq!(edited, "# Plan\n\nEdited");
        assert_ne!(first_digest, edited_digest);
        assert!(
            store
                .user_edit_diff("session", "plan", 1, &edited)
                .unwrap()
                .contains("Edited")
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/model-0001.md")
                .exists()
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/user-0001.md")
                .exists()
        );
        store.delete_session("session").unwrap();
        assert!(!temporary.path().join("plans/session").exists());
    }
}
