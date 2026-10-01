use super::{PlanDocument, PlanFileStore, RenderedPlan, digest, render_plan_at};
use anyhow::{Context, Result};
use std::fs::File;
use std::io::Read;
use std::path::Path;

const SOURCE_CAPACITY: u64 = 8 * 1024 * 1024;

pub(crate) struct PlanReviewSource {
    pub historical: bool,
    pub public_only: bool,
    pub path: std::path::PathBuf,
    pub workspace: std::path::PathBuf,
    pub document: PlanDocument,
    pub rendered: RenderedPlan,
    pub saved_navigation: Option<super::PlanNavigationIndex>,
    pub saved_digest: String,
    pub syntax: Option<forge_diff::syntax::SyntaxHandle>,
    pub declaration_syntax:
        std::collections::HashMap<(String, String), forge_diff::syntax::SyntaxHandle>,
}

impl PlanFileStore {
    /// Capture a submitted revision independently of the mutable working document.
    pub(crate) fn capture_revision_source(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<PlanReviewSource> {
        anyhow::ensure!(revision > 0, "plan revision must be positive");
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("submitted-{revision:04}.json"));
        let submitted = read_source(&path)?;
        let document: PlanDocument = serde_json::from_slice(&submitted)?;
        document.validate_for_submission()?;
        anyhow::ensure!(
            document.plan_id == plan_id,
            "submitted plan identity changed"
        );
        let saved = RenderedPlan {
            markdown: String::from_utf8(read_source(&path.with_extension("md"))?)?,
            navigation: serde_json::from_slice(&read_source(&path.with_extension("index.json"))?)?,
        };
        let (rendered, saved_navigation) = if document.design.is_some() {
            (
                render_plan_at(&document, &self.workspace)?,
                Some(saved.navigation),
            )
        } else {
            (saved, None)
        };
        Ok(PlanReviewSource {
            public_only: false,
            historical: true,
            path: path.with_extension("md"),
            workspace: self.workspace.clone(),
            document,
            rendered,
            saved_navigation,
            saved_digest: digest(&submitted),
            syntax: None,
            declaration_syntax: Default::default(),
        })
    }

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
        let saved_navigation = if document.design.is_some() {
            let revision = directory
                .join("revisions")
                .join(format!("submitted-{revision:04}.md"));
            anyhow::ensure!(
                physical == read_source(&revision)?,
                "physical plan review changed after the submitted revision"
            );
            Some(serde_json::from_slice(&read_source(
                &revision.with_extension("index.json"),
            )?)?)
        } else {
            anyhow::ensure!(
                physical == rendered.markdown.as_bytes(),
                "physical plan review changed after the submitted revision"
            );
            None
        };
        Ok(PlanReviewSource {
            public_only: false,
            historical: false,
            path: directory.join("working.md"),
            workspace: self.workspace.clone(),
            document,
            rendered,
            saved_navigation,
            saved_digest: digest(&working),
            syntax: None,
            declaration_syntax: Default::default(),
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
    fn existing_design_projection_is_rebuilt_without_rewriting_reviewed_artifacts() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let mut document = crate::plan::document::test_fixture("plan", "Design");
        let mut design = crate::plan::DeclarationDesign::default();
        design.proposed.insert(
            "registry.rs".into(),
            "pub struct Registry {\n    first: u64,\n    second: u64,\n}\n".into(),
        );
        design.document.description = "Revise registry declarations.".into();
        document.design = Some(design);
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let directory = store.plan_dir("session", "plan");
        let revision = directory.join("revisions/submitted-0001.json");
        let saved = std::fs::read(&revision).unwrap();
        let working = std::fs::read(directory.join("working.json")).unwrap();
        let projection = b"Previous formatter output\n";
        std::fs::write(revision.with_extension("md"), projection).unwrap();
        std::fs::write(directory.join("working.md"), projection).unwrap();
        let current = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let historical = store.capture_revision_source("session", "plan", 1).unwrap();
        assert!(current.rendered.markdown.contains("  first: u64,"));
        assert_eq!(historical.rendered.markdown, current.rendered.markdown);
        assert_eq!(std::fs::read(&revision).unwrap(), saved);
        assert_eq!(
            std::fs::read(directory.join("working.json")).unwrap(),
            working
        );
        assert_eq!(
            std::fs::read(directory.join("working.md")).unwrap(),
            projection
        );
        std::fs::write(directory.join("working.md"), b"Changed after review").unwrap();
        assert!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .is_err()
        );
    }

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
