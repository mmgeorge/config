use super::{PlanDocument, RenderedPlan, digest};
use anyhow::Result;
use std::sync::Mutex;

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
            let current = self.current.lock().map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
            if current.as_ref().is_some_and(|entry| entry.checksum == checksum && (!submitted || entry.submitted)) {
                return Ok(());
            }
        }
        if submitted { document.validate_for_submission()?; } else { document.validate()?; }
        let mut current = self.current.lock().map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        if let Some(entry) = current.as_mut().filter(|entry| entry.checksum == checksum) {
            entry.submitted |= submitted;
        } else {
            *current = Some(CachedReview { checksum, rendered: None, submitted });
        }
        Ok(())
    }

    /// Returns a rendering only when its complete canonical input is unchanged.
    pub(super) fn rendered(&self, document: &PlanDocument) -> Result<Option<RenderedPlan>> {
        let checksum = digest(&crate::plan::codec::to_vec(document)?);
        let current = self.current.lock().map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        Ok(current.as_ref().filter(|entry| entry.checksum == checksum).and_then(|entry| entry.rendered.clone()))
    }

    /// Publishes a fully validated revision or a checked edit derived from one.
    pub(super) fn remember(&self, document: &PlanDocument, rendered: &RenderedPlan) -> Result<()> {
        anyhow::ensure!(rendered.navigation.plan_id == document.plan_id
            && rendered.navigation.plan_version == document.version, "rendered plan identity changed");
        let checksum = digest(&crate::plan::codec::to_vec(document)?);
        let mut current = self.current.lock().map_err(|_| anyhow::anyhow!("plan cache lock poisoned"))?;
        *current = Some(CachedReview { checksum, rendered: Some(rendered.clone()), submitted: true });
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
    fn cached_validation_requires_identical_bytes_and_submission_strength() {
        let cache = ReviewCache::default();
        let mut document = super::super::document::test_fixture("cached", "Cached plan");
        let mut design = super::super::DeclarationDesign::default();
        design.proposed.insert("src/lib.rs".into(), "pub fn run();\n".into());
        document.design = Some(design);
        cache.validate(&document, false).unwrap();
        assert!(cache.validate(&document, true).is_err(), "draft validation cannot authorize submission");
        document.design.as_mut().unwrap().proposed.insert("../escape.rs".into(), "pub fn invalid_path();\n".into());
        assert!(cache.validate(&document, false).is_err(), "changed bytes cannot reuse validation");
    }
}
