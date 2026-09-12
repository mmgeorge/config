use super::{PlanDocument, PlanFileStore, RenderedPlan, digest, render_plan_at};
use anyhow::{Context, Result};
use std::fs::File;
use std::io::Read;
use std::path::Path;

const SOURCE_CAPACITY: u64 = 8 * 1024 * 1024;

pub(crate) struct PlanReviewSource {
    pub path: std::path::PathBuf,
    pub workspace: std::path::PathBuf,
    pub document: PlanDocument,
    pub rendered: RenderedPlan,
    pub saved_digest: String,
    pub syntax: Option<forge_diff::syntax::SyntaxHandle>,
}

impl PlanFileStore {
    pub(crate) fn capture_review_source(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_digest: &str,
    ) -> Result<PlanReviewSource> {
        let directory = self.plan_dir(session_id, plan_id);
        let submitted = read_source(
            &directory
                .join("revisions")
                .join(format!("submitted-{revision:04}.json")),
        )?;
        let document: PlanDocument = serde_json::from_slice(&submitted)?;
        document.validate_for_submission()?;
        anyhow::ensure!(
            document.plan_id == plan_id,
            "submitted plan identity changed"
        );
        let canonical = serde_json::to_vec(&document)?;
        anyhow::ensure!(
            digest(&canonical) == expected_digest,
            "submitted plan bytes changed after review"
        );
        let working = read_source(&directory.join("working.json"))?;
        let saved: PlanDocument = serde_json::from_slice(&working)?;
        anyhow::ensure!(
            serde_json::to_vec(&saved)? == canonical,
            "working plan changed after the submitted review revision"
        );
        let rendered = render_plan_at(&document, &self.workspace)?;
        let physical = read_source(&directory.join("working.md"))?;
        anyhow::ensure!(
            physical == rendered.markdown.as_bytes(),
            "physical plan review changed after the submitted revision"
        );
        Ok(PlanReviewSource {
            path: directory.join("working.md"),
            workspace: self.workspace.clone(),
            document,
            rendered,
            saved_digest: digest(&working),
            syntax: None,
        })
    }
}

pub(super) fn read_source(path: &Path) -> Result<Vec<u8>> {
    let metadata = std::fs::symlink_metadata(path)
        .with_context(|| format!("inspect physical plan source {}", path.display()))?;
    anyhow::ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "plan source is not a regular file"
    );
    anyhow::ensure!(
        metadata.len() <= SOURCE_CAPACITY,
        "plan source exceeds 8 MiB"
    );
    let mut source = Vec::new();
    File::open(path)?
        .take(SOURCE_CAPACITY + 1)
        .read_to_end(&mut source)?;
    anyhow::ensure!(
        source.len() as u64 <= SOURCE_CAPACITY,
        "plan source exceeds 8 MiB"
    );
    Ok(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn review_capture_rejects_changed_working_and_submitted_sources() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let document = crate::plan::document::test_fixture("plan", "Initial");
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        assert_eq!(source.document.overview, "Initial");
        assert!(!source.rendered.markdown.is_empty());
        assert_eq!(source.saved_digest.len(), 64);
        let mut changed = document.clone();
        changed.overview = "Changed".into();
        store
            .write_working_document("session", "plan", &changed)
            .unwrap();
        assert!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .err()
                .unwrap()
                .to_string()
                .contains("working plan changed")
        );
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        std::fs::write(
            temporary
                .path()
                .join("plans/session/plan/revisions/submitted-0001.json"),
            serde_json::to_vec(&changed).unwrap(),
        )
        .unwrap();
        assert!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .err()
                .unwrap()
                .to_string()
                .contains("submitted plan bytes changed")
        );
    }
}
