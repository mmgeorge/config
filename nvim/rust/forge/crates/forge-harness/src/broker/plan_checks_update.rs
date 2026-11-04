use super::*;
use std::collections::BTreeMap;

/// Own a version-checked check-selection edit independently of broker state.
pub(crate) struct PlanChecksUpdateJob {
    plan_file: Arc<PlanFileStore>,
    plan: PlanRecord,
    session_id: String,
    expected_version: u64,
    expected_digest: String,
    changes: BTreeMap<String, bool>,
    tests: Vec<crate::plan::DesignTestSelection>,
    trace: crate::plan::edit_trace::PlanEditTrace,
}

impl PlanChecksUpdateJob {
    /// Edit and render the reviewed document without retaining the broker.
    pub(crate) async fn run(self) -> Result<PlanChecksUpdateResult> {
        let (document, rendered, checksum) = self
            .plan_file
            .update_checks(
                &self.session_id,
                &self.plan.id,
                self.expected_version,
                &self.expected_digest,
                crate::plan::PlanCheckSelection {
                    enabled: &self.changes,
                    removed_tests: &self.tests,
                },
                Some(&self.trace),
            )
            .await?;
        Ok(PlanChecksUpdateResult {
            plan: self.plan,
            document,
            markdown: rendered.markdown,
            checksum,
            changes: self.changes,
        })
    }
}

/// Carry an edited document until its original review authority is revalidated.
pub(crate) struct PlanChecksUpdateResult {
    plan: PlanRecord,
    document: crate::plan::PlanDocument,
    markdown: String,
    checksum: String,
    changes: BTreeMap<String, bool>,
}

impl HarnessBroker {
    /// Validate check-selection ownership and capture the reviewed source version.
    pub(crate) fn prepare_plan_checks_update(&self, params: Value) -> Result<PlanChecksUpdateJob> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.check.update",
            &params,
        );
        let plan_id = required_text(&params, "plan_id")?;
        let expected_digest = required_text(&params, "digest")?;
        let expected_version = params
            .get("expected_version")
            .and_then(Value::as_u64)
            .context("check selection requires the reviewed plan version")?;
        let changes = serde_json::from_value(
            params
                .get("enabled")
                .cloned()
                .context("check selection is missing")?,
        )?;
        let tests =
            serde_json::from_value(params.get("tests").cloned().unwrap_or_else(|| json!([])))?;
        let plan = self
            .store
            .load_plan(&plan_id)?
            .context("plan record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "only plans awaiting review can change checks"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(&expected_digest),
            "plan changed before check selection"
        );
        Ok(PlanChecksUpdateJob {
            plan_file: self.plan_file.clone(),
            plan,
            session_id: self.session.id.clone(),
            expected_version,
            expected_digest,
            changes,
            tests,
            trace,
        })
    }

    /// Publish check selection only while its plan and document remain the reviewed revision.
    pub(crate) fn commit_plan_checks_update(
        &mut self,
        result: PlanChecksUpdateResult,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let PlanChecksUpdateResult {
            plan: source,
            document,
            markdown,
            checksum,
            changes,
        } = result;
        anyhow::ensure!(
            source.session_id == self.session.id,
            "check selection crosses session ownership"
        );
        let mut plan = self
            .store
            .load_plan(&source.id)?
            .context("plan record disappeared during check selection")?;
        anyhow::ensure!(
            plan.session_id == source.session_id
                && plan.state == PlanState::AwaitingReview
                && plan.model_revision == source.model_revision
                && plan.user_revision == source.user_revision
                && plan.document_version == source.document_version
                && plan.review_digest == source.review_digest,
            "plan changed while updating check selection"
        );
        let current = self
            .plan_file
            .read_working_document(&self.session.id, &source.id)?;
        anyhow::ensure!(
            current.version == document.version
                && crate::plan::digest(&crate::codec::to_vec(&current)?) == checksum,
            "plan document changed before publishing check selection"
        );
        plan.document_version = document.version;
        plan.review_digest = Some(checksum);
        plan.review = None;
        plan.manual_changes.push(format!(
            "Updated plan checks: {}",
            changes
                .iter()
                .map(|(id, enabled)| format!(
                    "{id} {}",
                    if *enabled { "enabled" } else { "disabled" }
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ));
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;
        self.save_session()?;
        let payload = json!({ "plan":plan, "content":markdown, "document":document });
        Ok((
            payload.clone(),
            vec![self.event("plan_checks_updated", payload)?],
        ))
    }
}
