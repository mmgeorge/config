use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;

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
    pub state: PlanState,
    pub working_path: String,
    pub model_revision: u32,
    pub user_revision: u32,
    pub review_digest: Option<String>,
    pub accepted_digest: Option<String>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
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
