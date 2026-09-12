use super::*;
use crate::edit::{FieldRestoration, SaveRestoration};

#[derive(Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PendingPrField {
    region: RegionId,
    capture: SaveRestoration,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct BatchedDraft {
    mode: ReviewMode,
    summary: ReviewField,
    viewed_file: std::collections::BTreeSet<String>,
}

impl ReviewDocument {
    pub(crate) fn reconciled_draft(
        &self,
        observed: &forge_github::pull_request::PullRequestEdit,
        require_submission_match: bool,
    ) -> Result<serde_json::Value> {
        let mut draft = self.draft_payload()?;
        for submission in &self.pending {
            let text = if submission.region().0 == "title" {
                observed.title.as_deref()
            } else {
                observed.body.as_deref()
            }
            .context("PR reconciliation omitted captured field text")?;
            ensure!(
                !require_submission_match || text == submission.text(),
                "remote PR text no longer matches the captured operation"
            );
            let fields = draft
                .pointer_mut("/pr_fields/field")
                .and_then(serde_json::Value::as_array_mut)
                .context("PR draft omitted fields")?;
            let field = fields
                .iter_mut()
                .find(|field| {
                    field.get("region").and_then(serde_json::Value::as_str)
                        == Some(submission.region().0.as_str())
                })
                .context("PR draft omitted captured field")?;
            field["baseline"] = serde_json::json!(text);
            field["dirty"] = serde_json::json!(field["text"].as_str() != Some(text));
            field["uncertain"] = serde_json::json!(false);
        }
        draft["pr_fields"]["saving"] = serde_json::json!(false);
        draft["pr_fields"]["uncertain"] = serde_json::json!(false);
        draft["pr_pending"] = serde_json::json!([]);
        draft["pr_operation"] = serde_json::Value::Null;
        Ok(draft)
    }

    pub(crate) fn restore_pr_fields(&mut self, draft: &serde_json::Value) -> Result<()> {
        let Some(fields) = draft.pointer("/pr_fields/field") else {
            return Ok(());
        };
        ensure!(
            draft.get("repo").and_then(serde_json::Value::as_str)
                == Some(self.target.repository.repository_name().as_str())
                && draft.get("number").and_then(serde_json::Value::as_u64)
                    == Some(self.target.number),
            "stored PR draft belongs to another resource"
        );
        let fields: Vec<ReviewField> = serde_json::from_value(fields.clone())?;
        ensure!(fields.len() == 2, "stored PR field set is incomplete");
        let pending: Vec<PendingPrField> = draft
            .get("pr_pending")
            .map(|pending| serde_json::from_value(pending.clone()))
            .transpose()?
            .unwrap_or_default();
        ensure!(pending.len() <= 2, "stored PR pending field limit exceeded");
        let operation = draft
            .get("pr_operation")
            .and_then(serde_json::Value::as_str);
        ensure!(
            pending.is_empty()
                || operation.is_some_and(|operation| !operation.is_empty()
                    && operation.len() <= 256
                    && operation.bytes().all(|byte| byte.is_ascii_graphic())),
            "stored PR capture has no valid operation identity"
        );
        let mut capture = BTreeMap::new();
        for mut pending in pending {
            pending.capture.uncertain = true;
            ensure!(
                matches!(pending.region.0.as_str(), "title" | "body")
                    && capture.insert(pending.region, pending.capture).is_none(),
                "stored PR capture region is invalid or duplicated"
            );
        }
        let mut seen = std::collections::BTreeSet::new();
        for field in fields {
            ensure!(
                matches!(field.region.0.as_str(), "title" | "body")
                    && seen.insert(field.region.clone()),
                "stored PR field region is invalid or duplicated"
            );
            let observed = self.edits.snapshot(&field.region)?.text.to_owned();
            let pending = capture.remove(&field.region);
            ensure!(
                !field.uncertain || pending.is_some(),
                "stored uncertain PR field is missing its exact capture"
            );
            let restored_pending = self.edits.restore_field(
                field.region.clone(),
                FieldRestoration {
                    revision: field.revision,
                    sequence: field.sequence,
                    text: field.text,
                    baseline: field.baseline,
                    remote: None,
                    pending,
                },
            )?;
            if let Some(pending) = restored_pending {
                self.pending.push(pending);
            } else {
                self.edits.merge(&field.region, observed)?;
            }
        }
        self.pending_operation = (!self.pending.is_empty()).then(|| operation.unwrap().to_owned());
        Ok(())
    }

    pub(crate) fn restore_batched(&mut self, draft: &serde_json::Value) -> Result<()> {
        let Some(stored) = draft.get("batched").filter(|stored| !stored.is_null()) else {
            ensure!(
                draft
                    .get("batched_operation")
                    .is_none_or(serde_json::Value::is_null)
                    && draft
                        .get("batched_capture")
                        .is_none_or(serde_json::Value::is_null),
                "stored review submission operation requires batched state"
            );
            return Ok(());
        };
        let stored: BatchedDraft = serde_json::from_value(stored.clone())?;
        ensure!(
            stored.mode == ReviewMode::Batched,
            "stored review mode is invalid"
        );
        ensure!(
            stored.summary.region.0 == "review_summary" && !stored.summary.uncertain,
            "stored review summary is invalid"
        );
        for path in &stored.viewed_file {
            RepositoryPath::new(path.as_bytes().to_vec())?;
        }
        self.edits.insert(
            RegionId("review_summary".into()),
            RegionRevision(0),
            String::new(),
        )?;
        self.edits.restore_field(
            RegionId("review_summary".into()),
            FieldRestoration {
                revision: stored.summary.revision,
                sequence: stored.summary.sequence,
                text: stored.summary.text,
                baseline: stored.summary.baseline,
                remote: None,
                pending: None,
            },
        )?;
        let operation = draft
            .get("batched_operation")
            .and_then(serde_json::Value::as_str);
        let capture = draft.get("batched_capture");
        ensure!(
            operation.is_some() == capture.is_some(),
            "stored review submission operation and capture differ"
        );
        if let (Some(operation), Some(capture)) = (operation, capture) {
            ensure!(
                !operation.is_empty()
                    && operation.len() <= 256
                    && operation.bytes().all(|byte| byte.is_ascii_graphic()),
                "stored review submission operation is invalid"
            );
            let capture: ReviewMutation = serde_json::from_value(capture.clone())?;
            ensure!(
                matches!(capture, ReviewMutation::ReviewSubmit { .. }),
                "stored review capture is not a submission"
            );
            self.batched_operation = Some(operation.to_owned());
            self.batched_capture = Some(capture);
            self.saving = true;
        }
        self.comments.set_mode(ReviewMode::Batched)?;
        self.mode = ReviewMode::Batched;
        self.viewed_file = stored.viewed_file;
        Ok(())
    }

    pub(super) fn pending_draft(&self) -> Vec<PendingPrField> {
        self.pending
            .iter()
            .map(|pending| PendingPrField {
                region: pending.region().clone(),
                capture: SaveRestoration {
                    revision: pending.revision(),
                    sequence: pending.sequence(),
                    text: pending.text().to_owned(),
                    uncertain: true,
                },
            })
            .collect()
    }
}
