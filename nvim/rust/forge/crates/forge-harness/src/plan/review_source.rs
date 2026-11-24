use super::review_cache::PlanReviewRevision;
use super::{PlanDocument, PlanFileStore, RenderedPlan, digest};
use anyhow::{Context, Result};
use std::path::Path;

#[derive(Clone)]
/// Native declarations and isolated Algorithm rows for one immutable file side.
pub(crate) struct ReviewSyntax {
    /// Native-language analysis of the declaration-only source.
    pub declaration: forge_diff::syntax::SyntaxHandle,
    /// Algorithm analysis with rows aligned to the displayed declaration source.
    pub algorithm: Option<forge_diff::syntax::SyntaxHandle>,
}

pub(crate) enum PlanReviewSource {
    Working(EditableReviewSource),
    Historical(ReviewSourceData),
}

pub(crate) struct ReviewSourceData {
    pub files: Option<super::review_files::ReviewFiles>,
    pub execution_review: Option<String>,
    pub trace: Option<ReviewTrace>,
    pub resolver: std::sync::Arc<
        tokio::sync::OnceCell<
            std::sync::Mutex<(
                crate::declaration::DeclarationResolver,
                crate::declaration::DeclarationResolver,
            )>,
        >,
    >,
    pub public_only: bool,
    /// File heading identities marked viewed for this exact review revision.
    pub viewed_files: std::collections::BTreeSet<String>,
    /// Keeps selected reference context visible without changing the public filter.
    pub revealed: std::collections::HashSet<(String, String, u32)>,
    pub collapse: std::collections::HashMap<forge_buffer::identity::FoldId, bool>,
    pub path: std::path::PathBuf,
    pub workspace: std::path::PathBuf,
    pub(crate) revision: PlanReviewRevision,
    pub syntax: Option<forge_diff::syntax::SyntaxHandle>,
    pub declaration_syntax: std::collections::HashMap<(String, String), ReviewSyntax>,
}

impl std::ops::Deref for ReviewSourceData {
    type Target = PlanReviewRevision;

    fn deref(&self) -> &Self::Target {
        &self.revision
    }
}

impl ReviewSourceData {}

/// Owns a working review source whose access mode cannot change during analysis.
pub(crate) struct EditableReviewSource(ReviewSourceData);

impl TryFrom<PlanReviewSource> for EditableReviewSource {
    type Error = anyhow::Error;

    fn try_from(source: PlanReviewSource) -> Result<Self> {
        match source {
            PlanReviewSource::Working(source) => Ok(source),
            PlanReviewSource::Historical(_) => {
                anyhow::bail!("historical plan revisions are read-only")
            }
        }
    }
}

impl std::ops::Deref for EditableReviewSource {
    type Target = ReviewSourceData;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for EditableReviewSource {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl EditableReviewSource {
    pub(super) fn into_source(self) -> PlanReviewSource {
        PlanReviewSource::Working(self)
    }
}

impl PlanReviewSource {
    /// Locate review metadata shared by the working and archived copies of a revision.
    pub(super) fn plan_directory(&self) -> Result<&Path> {
        let parent = self
            .path
            .parent()
            .context("physical plan parent is missing")?;
        if self.is_historical() {
            return parent.parent().context("plan revision parent is missing");
        }
        Ok(parent)
    }

    /// Locate the viewed file set for this plan's exact saved revision.
    pub(super) fn viewed_path(&self) -> Result<std::path::PathBuf> {
        Ok(self
            .plan_directory()?
            .join(format!("review-viewed-{}.json", self.saved_digest)))
    }

    /// Read the revision's viewed file headings, including changes from other open tabs.
    pub(super) fn read_viewed_files(&self) -> Result<std::collections::BTreeSet<String>> {
        let path = self.viewed_path()?;
        let Some(bytes) = super::read_optional_review_file(&path)? else {
            return Ok(Default::default());
        };
        serde_json::from_slice(&bytes)
            .with_context(|| format!("decode viewed plan files {}", path.display()))
    }

    pub(crate) fn into_data(self) -> ReviewSourceData {
        match self {
            Self::Working(source) => source.0,
            Self::Historical(source) => source,
        }
    }

    pub(crate) fn is_historical(&self) -> bool {
        matches!(self, Self::Historical(_))
    }
}

impl std::ops::Deref for PlanReviewSource {
    type Target = ReviewSourceData;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Working(source) => &source.0,
            Self::Historical(source) => source,
        }
    }
}

impl std::ops::DerefMut for PlanReviewSource {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Working(source) => &mut source.0,
            Self::Historical(source) => source,
        }
    }
}

pub(crate) struct ReviewTrace {
    pub store: std::sync::Arc<crate::trace::TraceStore>,
    pub session_id: String,
}

impl ReviewTrace {
    pub fn record(&self, event: &str, elapsed: std::time::Duration, count: usize) {
        self.store.record(
            &self.session_id,
            event,
            serde_json::json!({
                "elapsed_ms": elapsed.as_secs_f64() * 1000.0,
                "count": count,
            }),
        );
    }
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
        let document: PlanDocument = super::codec::from_slice(&submitted)?;
        anyhow::ensure!(
            document.plan_id == plan_id,
            "submitted plan identity changed"
        );
        let saved = RenderedPlan {
            markdown: String::from_utf8(read_source(&path.with_extension("md"))?)?,
            navigation: super::codec::from_slice(&read_source(
                &path.with_extension("index.json"),
            )?)?,
        };
        Ok(PlanReviewSource::Historical(ReviewSourceData {
            files: None,
            trace: None,
            execution_review: None,
            resolver: Default::default(),
            public_only: true,
            viewed_files: Default::default(),
            collapse: Default::default(),
            revealed: Default::default(),
            path: path.with_extension("md"),
            workspace: self.workspace.clone(),
            revision: PlanReviewRevision::from_saved(document, saved, digest(&submitted))?,
            syntax: None,
            declaration_syntax: Default::default(),
        }))
    }

    /// Capture the checked working review, using a submitted baseline or the current draft when absent.
    pub(crate) fn capture_review_source(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: Option<u32>,
        expected_digest: &str,
    ) -> Result<PlanReviewSource> {
        let directory = self.plan_dir(session_id, plan_id);
        let working = read_source(&directory.join("working.json"))?;
        let document: PlanDocument = super::codec::from_slice(&working)?;
        anyhow::ensure!(
            document.plan_id == plan_id,
            "reviewed plan identity changed"
        );
        let canonical = super::codec::to_vec(&document)?;
        anyhow::ensure!(
            digest(&canonical) == expected_digest,
            "working plan changed after review"
        );
        let physical = read_source(&directory.join("working.md"))?;
        let navigation_path = if let Some(revision) = revision {
            let submitted_path = directory
                .join("revisions")
                .join(format!("submitted-{revision:04}.json"));
            let submitted = read_source(&submitted_path)?;
            if submitted != working {
                let submitted: PlanDocument = super::codec::from_slice(&submitted)?;
                anyhow::ensure!(
                    super::codec::to_vec(&submitted)? == canonical,
                    "submitted plan bytes changed after review"
                );
            }
            anyhow::ensure!(
                physical == read_source(&submitted_path.with_extension("md"))?,
                "physical plan review changed after the submitted revision"
            );
            submitted_path.with_extension("index.json")
        } else {
            directory.join("working.index.json")
        };
        let rendered = RenderedPlan {
            markdown: String::from_utf8(physical)?,
            navigation: super::codec::from_slice(&read_source(&navigation_path)?)?,
        };
        Ok(PlanReviewSource::Working(EditableReviewSource(
            ReviewSourceData {
                files: None,
                trace: None,
                execution_review: None,
                resolver: Default::default(),
                public_only: true,
                viewed_files: Default::default(),
                collapse: Default::default(),
                revealed: Default::default(),
                path: directory.join("working.md"),
                workspace: self.workspace.clone(),
                revision: PlanReviewRevision::from_saved(document, rendered, digest(&working))?,
                syntax: None,
                declaration_syntax: Default::default(),
            },
        )))
    }
}

pub(super) fn read_source(path: &Path) -> Result<Vec<u8>> {
    let metadata = std::fs::symlink_metadata(path)
        .with_context(|| format!("inspect physical plan source {}", path.display()))?;
    anyhow::ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "plan source is not a regular file"
    );
    std::fs::read(path).with_context(|| format!("read physical plan source {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_new_revision_has_its_own_viewed_files() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let files = PlanFileStore::new(temporary.path().join("data"), temporary.path())?;
        let mut document = crate::plan::document::test_fixture("plan", "Review a file.");
        files.write_working_document("session", "plan", &document)?;
        let (_, _, digest) = files.submit_document_revision("session", "plan", 1, 1)?;
        let first = files.capture_review_source("session", "plan", Some(1), &digest)?;
        let viewed = std::collections::BTreeSet::from([String::from("plan:design:file:first")]);
        super::super::write_json_atomically(&first.viewed_path()?, &viewed)?;

        document.version += 1;
        files.write_working_document("session", "plan", &document)?;
        let (_, _, digest) =
            files.submit_document_revision("session", "plan", 2, document.version)?;
        let second = files.capture_review_source("session", "plan", Some(2), &digest)?;
        assert!(second.read_viewed_files()?.is_empty());
        assert_eq!(
            files
                .capture_revision_source("session", "plan", 1)?
                .read_viewed_files()?,
            viewed
        );
        Ok(())
    }

    #[test]
    fn review_reads_the_submitted_rendering_and_rejects_a_divergent_working_copy() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let files = PlanFileStore::new(temporary.path().join("data"), temporary.path())?;
        let document = crate::plan::document::test_fixture("plan", "Read the saved revision.");
        files.write_working_document("session", "plan", &document)?;
        let (_, rendered, checksum) = files.submit_document_revision("session", "plan", 1, 1)?;
        let directory = files.plan_dir("session", "plan");
        // A previous renderer can leave different trailing whitespace at the same anchors.
        let saved = format!("{}\n", rendered.markdown);
        let working = directory.join("working.md");
        std::fs::write(directory.join("revisions/submitted-0001.md"), &saved)?;
        std::fs::write(&working, &saved)?;
        assert_eq!(
            files
                .capture_revision_source("session", "plan", 1)?
                .rendered
                .markdown,
            saved
        );
        assert_eq!(
            files
                .capture_review_source("session", "plan", Some(1), &checksum)?
                .rendered
                .markdown,
            saved
        );
        std::fs::write(&working, "changed")?;
        assert!(
            files
                .capture_review_source("session", "plan", Some(1), &checksum)
                .is_err()
        );
        Ok(())
    }
}
