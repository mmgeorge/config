use anyhow::{Context, Result, ensure};
use std::sync::Arc;
use std::time::Instant;

use super::review_document::{PlanReviewQuestion, PlanReviewStore};
use forge_buffer::identity::DocumentId;

/// Binds a fork's final response to its captured annotation and source body.
pub(crate) struct ReviewAnswerScope {
    store: Arc<PlanReviewStore>,
    document: DocumentId,
    pub(crate) question: PlanReviewQuestion,
    started: Instant,
}

impl ReviewAnswerScope {
    pub(crate) fn new(
        store: Arc<PlanReviewStore>,
        document: DocumentId,
        question: PlanReviewQuestion,
    ) -> Arc<Self> {
        Arc::new(Self {
            store,
            document,
            question,
            started: Instant::now(),
        })
    }

    /// Reject stale annotations before spending another provider turn on their answer.
    pub(crate) fn check(&self) -> Result<()> {
        self.store.check_question(&self.document, &self.question)
    }

    /// Accept only this question's nonempty answer while its captured annotation is still current.
    pub(crate) fn submit(&self, answer: String) -> Result<()> {
        ensure!(!answer.trim().is_empty(), "review answer cannot be empty");
        self.store
            .answer(
                &self.document,
                &self.question,
                answer,
                self.started.elapsed().as_millis() as u64,
            )
            .context("save review answer")?;
        Ok(())
    }
}
