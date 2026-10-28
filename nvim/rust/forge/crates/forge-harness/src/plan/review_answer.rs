use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use super::review_document::{PlanReviewQuestion, PlanReviewStore};
use forge_buffer::identity::DocumentId;

/// Carries the structured answer to the one annotation authorized for a forked session.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ReviewAnswer {
    pub question_id: String,
    pub answer: String,
}

/// Owns answer acceptance independently of provider messages and main-timeline execution.
pub(crate) struct ReviewAnswerScope {
    store: Arc<PlanReviewStore>,
    document: DocumentId,
    pub(crate) question: PlanReviewQuestion,
    started: Instant,
    submitted: Mutex<Option<String>>,
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
            submitted: Mutex::new(None),
        })
    }

    /// Reject stale annotations before spending another provider turn on their answer.
    pub(crate) fn check(&self) -> Result<()> {
        self.store.check_question(&self.document, &self.question)
    }

    pub(crate) fn answered(&self) -> Result<bool> {
        Ok(self
            .submitted
            .lock()
            .map_err(|_| anyhow::anyhow!("review answer lock poisoned"))?
            .is_some())
    }

    /// Accept only this question's nonempty answer while its captured annotation is still current.
    pub(crate) fn submit(&self, answer: ReviewAnswer) -> Result<()> {
        ensure!(
            answer.question_id == self.question.id,
            "answer targets a different review question"
        );
        ensure!(
            !answer.answer.trim().is_empty(),
            "review answer cannot be empty"
        );
        let mut submitted = self
            .submitted
            .lock()
            .map_err(|_| anyhow::anyhow!("review answer lock poisoned"))?;
        if let Some(previous) = submitted.as_ref() {
            ensure!(
                previous == &answer.answer,
                "review question already has a different answer"
            );
            return Ok(());
        }
        self.store
            .answer(
                &self.document,
                &self.question,
                answer.answer.clone(),
                self.started.elapsed().as_millis() as u64,
            )
            .context("save review answer")?;
        *submitted = Some(answer.answer);
        Ok(())
    }
}
