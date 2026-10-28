use super::{PlanDocument, RenderedPlan, digest};
use anyhow::Result;
use std::sync::Mutex;

/// Owns one immutable plan, its matching rendered navigation, and its saved digest.
pub(crate) struct PlanReviewRevision(PlanReviewRevisionData);

pub(crate) struct PlanReviewRevisionData {
    pub document: PlanDocument,
    pub rendered: RenderedPlan,
    pub saved_digest: String,
}

impl PlanReviewRevision {
    pub(crate) fn new(
        plan_id: &str,
        document: PlanDocument,
        rendered: RenderedPlan,
    ) -> Result<Self> {
        anyhow::ensure!(
            document.plan_id == plan_id,
            "rendered plan identity changed"
        );
        let checksum = digest(&crate::plan::codec::to_vec(&document)?);
        Self::from_saved(document, rendered, checksum)
    }

    pub(super) fn from_saved(
        document: PlanDocument,
        rendered: RenderedPlan,
        saved_digest: String,
    ) -> Result<Self> {
        anyhow::ensure!(
            rendered.navigation.plan_id == document.plan_id
                && rendered.navigation.plan_version == document.version,
            "rendered plan identity changed"
        );
        Ok(Self(PlanReviewRevisionData {
            document,
            rendered,
            saved_digest,
        }))
    }

    pub(crate) fn into_parts(self) -> (PlanDocument, RenderedPlan, String) {
        (self.0.document, self.0.rendered, self.0.saved_digest)
    }
}

impl std::ops::Deref for PlanReviewRevision {
    type Target = PlanReviewRevisionData;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Default)]
/// Retains one validated canonical plan and its rendered revision.
pub(super) struct ReviewCache {
    current: Mutex<Option<CachedReview>>,
}

impl ReviewCache {
    /// Reuses validation only for identical canonical bytes at the requested strength.
    pub(super) fn validate(&self, document: &PlanDocument, submitted: bool) -> Result<()> {
        let checksum = digest(&crate::plan::codec::to_vec(document)?);
        {
            let current = self
                .current
                .lock()
                .map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
            if current
                .as_ref()
                .is_some_and(|entry| entry.checksum == checksum && (!submitted || entry.submitted))
            {
                return Ok(());
            }
        }
        if submitted {
            document.validate_for_submission()?;
        } else {
            document.validate()?;
        }
        let mut current = self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        if let Some(entry) = current.as_mut().filter(|entry| entry.checksum == checksum) {
            entry.submitted |= submitted;
        } else {
            *current = Some(CachedReview {
                checksum,
                rendered: None,
                submitted,
            });
        }
        Ok(())
    }

    /// Returns a rendering only when its complete canonical input is unchanged.
    pub(super) fn rendered(&self, document: &PlanDocument) -> Result<Option<RenderedPlan>> {
        let checksum = digest(&crate::plan::codec::to_vec(document)?);
        let current = self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        Ok(current
            .as_ref()
            .filter(|entry| entry.checksum == checksum)
            .and_then(|entry| entry.rendered.clone()))
    }

    /// Publishes a fully validated revision or a checked edit derived from one.
    pub(super) fn remember(&self, revision: &PlanReviewRevision) -> Result<()> {
        let checksum = digest(&crate::plan::codec::to_vec(&revision.document)?);
        let mut current = self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        *current = Some(CachedReview {
            checksum,
            rendered: Some(revision.rendered.clone()),
            submitted: true,
        });
        Ok(())
    }
}

struct CachedReview {
    checksum: String,
    rendered: Option<RenderedPlan>,
    submitted: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn revision_rejects_mismatched_rendering_before_publication() {
        let document = super::super::document::test_fixture("owned", "Owned plan");
        let mut rendered = super::super::render_plan(&document).unwrap();
        rendered.navigation.plan_version += 1;
        assert!(PlanReviewRevision::new("owned", document, rendered).is_err());
    }

    #[test]
    fn cached_validation_requires_identical_bytes_and_submission_strength() {
        let cache = ReviewCache::default();
        let mut document = super::super::document::test_fixture("cached", "Cached plan");
        let mut design = super::super::DeclarationDesign::default();
        design
            .proposed
            .insert("src/lib.rs".into(), "pub fn run();\n".into());
        document.design = Some(design);
        cache.validate(&document, false).unwrap();
        assert!(
            cache.validate(&document, true).is_err(),
            "draft validation cannot authorize submission"
        );
        document
            .design
            .as_mut()
            .unwrap()
            .proposed
            .insert("../escape.rs".into(), "pub fn invalid_path();\n".into());
        assert!(
            cache.validate(&document, false).is_err(),
            "changed bytes cannot reuse validation"
        );
    }
}
