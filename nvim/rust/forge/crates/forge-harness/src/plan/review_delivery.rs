//! Track which review comments and edits a prompt delivered to the planner.
//!
//! A review that shows one revision while acceptance applies edits recorded on another is no
//! approval gate. Every prompt to the planner while review comments or edits are pending carries
//! them, and this module records that it did. When a newer model revision replaces the reviewed
//! document, what a prompt delivered retires with the revision it applied to; a comment no prompt
//! delivered moves onto the new revision, or is reported when the new revision no longer has
//! its rows, and an edit no prompt delivered stays pending.

use super::review_annotation::{ReviewAnnotation, ReviewAnnotationKind, ReviewAnnotationStore};
use super::{PlanFileStore, PlanRecord, RenderedPlan, digest};
use anyhow::Result;
use forge_provider_api::forge::PlanEdit;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs;

/// What prompts to the planner delivered of the review pending on one model revision.
#[derive(Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct ReviewDelivery {
    model_revision: u32,
    /// The edits pending when the latest prompt carried them, in their recorded order.
    edits: Vec<PlanEdit>,
    /// Saved comments any prompt carried, by annotation identity.
    comment_ids: BTreeSet<String>,
    /// Comments without rows that the latest prompt carried.
    unplaced: Vec<String>,
}

/// The saved comments of the working document a submission replaces, with the rows they name.
pub(crate) struct ReviewCommentSnapshot {
    rendered: Option<RenderedPlan>,
    annotation: Vec<ReviewAnnotation>,
}

impl PlanFileStore {
    fn review_delivery_path(&self, plan: &PlanRecord) -> std::path::PathBuf {
        self.plan_dir(&plan.session_id, &plan.id)
            .join("review-delivery.json")
    }

    /// Read what prompts delivered of the review pending on the plan's current model revision.
    pub(crate) fn review_delivery(&self, plan: &PlanRecord) -> Result<ReviewDelivery> {
        let delivery = match fs::read(self.review_delivery_path(plan)) {
            Ok(bytes) => super::codec::from_slice::<ReviewDelivery>(&bytes)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                return Ok(ReviewDelivery::default());
            }
            Err(error) => return Err(error.into()),
        };
        Ok(if delivery.model_revision == plan.model_revision {
            delivery
        } else {
            ReviewDelivery::default()
        })
    }

    /// Record that a prompt to the planner carried every review comment and edit now pending.
    pub(crate) fn record_review_delivery(&self, plan: &PlanRecord) -> Result<()> {
        let mut delivery = self.review_delivery(plan)?;
        delivery.model_revision = plan.model_revision;
        delivery.edits.clone_from(&plan.manual_changes);
        delivery.unplaced.clone_from(&plan.unplaced_review_comments);
        delivery.comment_ids.extend(
            self.review_comment_snapshot(&plan.session_id, &plan.id)?
                .annotation
                .into_iter()
                .filter(|annotation| annotation.kind == ReviewAnnotationKind::Comment)
                .map(|annotation| annotation.id),
        );
        super::write_json_atomically(&self.review_delivery_path(plan), &delivery)
    }

    /// Read the saved review annotations of the current working document and the rendered rows
    /// they name, before a submission replaces that document.
    pub(crate) fn review_comment_snapshot(
        &self,
        session_id: &str,
        plan_id: &str,
    ) -> Result<ReviewCommentSnapshot> {
        let directory = self.plan_dir(session_id, plan_id);
        let path = directory.join("working.json");
        let bytes = match fs::read(&path) {
            Ok(bytes) => bytes,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                return Ok(ReviewCommentSnapshot {
                    rendered: None,
                    annotation: Vec::new(),
                });
            }
            Err(error) => return Err(error.into()),
        };
        let annotation = ReviewAnnotationStore::open(path, digest(&bytes))?
            .annotation()
            .to_vec();
        if annotation.is_empty() {
            return Ok(ReviewCommentSnapshot {
                rendered: None,
                annotation,
            });
        }
        // The saved lines name rows of the rendering the review showed, which the working
        // Markdown and its navigation index hold.
        let rendered = RenderedPlan {
            markdown: fs::read_to_string(directory.join("working.md"))?,
            navigation: super::codec::from_slice(&fs::read(directory.join("working.index.json"))?)?,
        };
        Ok(ReviewCommentSnapshot {
            rendered: Some(rendered),
            annotation,
        })
    }

    /// Move the saved comments no prompt delivered onto the revision that replaced their
    /// document, now the working document, and return the text of each one whose rows that
    /// revision no longer has. Delivered comments and questions stay with the replaced revision.
    pub(crate) fn carry_review_comments(
        &self,
        session_id: &str,
        plan_id: &str,
        previous: ReviewCommentSnapshot,
        delivery: &ReviewDelivery,
        rendered: &RenderedPlan,
    ) -> Result<Vec<String>> {
        let ReviewCommentSnapshot {
            rendered: previous_rendered,
            annotation,
        } = previous;
        let pending = annotation
            .into_iter()
            .filter(|annotation| {
                annotation.kind == ReviewAnnotationKind::Comment
                    && !annotation.source.body.trim().is_empty()
                    && !delivery.comment_ids.contains(&annotation.id)
            })
            .collect::<Vec<_>>();
        let Some(previous_rendered) = previous_rendered.filter(|_| !pending.is_empty()) else {
            return Ok(Vec::new());
        };
        let (retained, unplaced) =
            super::review_annotation::relocate_annotations(&previous_rendered, rendered, pending);
        if !retained.is_empty() {
            let path = self.plan_dir(session_id, plan_id).join("working.json");
            let mut store = ReviewAnnotationStore::open(path.clone(), digest(&fs::read(&path)?))?;
            let mut merged = store.annotation().to_vec();
            merged.extend(
                retained
                    .into_iter()
                    .filter(|carried| !merged.iter().any(|saved| saved.id == carried.id))
                    .collect::<Vec<_>>(),
            );
            store.replace(merged)?;
        }
        Ok(unplaced
            .into_iter()
            .map(|annotation| annotation.source.body)
            .collect())
    }
}

impl PlanRecord {
    /// Retire the review a prompt delivered for the revision a newer model revision replaced.
    ///
    /// The edits and row-less comments a prompt carried leave the record; the ones recorded after
    /// the latest prompt stay pending, and `unplaced` adds the carried comments the new revision
    /// has no rows for.
    pub(crate) fn retire_delivered_review(
        &mut self,
        delivery: &ReviewDelivery,
        unplaced: Vec<String>,
    ) {
        let mut delivered = delivery.edits.clone();
        self.manual_changes.retain(|edit| {
            match delivered.iter().position(|candidate| candidate == edit) {
                Some(index) => {
                    delivered.remove(index);
                    false
                }
                None => true,
            }
        });
        self.unplaced_review_comments
            .retain(|body| !delivery.unplaced.contains(body));
        self.unplaced_review_comments.extend(unplaced);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::review_annotation::ReviewAnnotation;

    /// Write the planning directory and submit it as the next model revision.
    async fn submit(
        files: &PlanFileStore,
        workspace: &std::path::Path,
        plan_id: &str,
        revision: u32,
        tests: &str,
    ) -> Result<RenderedPlan> {
        let directory = files.authoring_directory("session", plan_id);
        fs::write(
            directory.join("docs/overview.md"),
            "# Plan\n\n## Objective\nKeep state.\n\n## Background\nNo changes yet.\n\n## Design\nRetain the public contract.\n",
        )?;
        fs::write(directory.join("docs/tests.md"), tests)?;
        let original = files.read_working_document("session", plan_id)?;
        let mut document = files.authoring_import("session", &original)?;
        let validated = document
            .design
            .as_ref()
            .unwrap()
            .validated(workspace, &document.title)
            .await?;
        document.design = Some(validated.into_design());
        let (_, rendered, _) = files.submit_validated_document_revision_if_unchanged(
            "session",
            plan_id,
            revision,
            original.version,
            &original,
            document,
        )?;
        Ok(rendered)
    }

    fn comment(id: &str, line: u32, body: &str) -> ReviewAnnotation {
        ReviewAnnotation {
            id: id.into(),
            parent_id: None,
            source: crate::plan::PlanAnnotationInput {
                start_line: line,
                end_line: line,
                body: body.into(),
            },
            anchor: None,
            kind: ReviewAnnotationKind::Comment,
            reply: None,
        }
    }

    fn line_of(rendered: &RenderedPlan, text: &str) -> u32 {
        rendered
            .markdown
            .lines()
            .position(|row| row.contains(text))
            .map(|index| index as u32 + 1)
            .unwrap()
    }

    /// A newer model revision keeps the comments no prompt delivered: one whose row the revision
    /// still shows moves to that row, and one whose row it dropped is reported. A comment a
    /// prompt delivered retires with the revision it was saved on.
    #[tokio::test]
    async fn a_new_revision_keeps_only_undelivered_comments() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path())?;
        let prepared = files.prepare_entry("session", "Document a plan.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let rendered = submit(
            &files,
            temporary.path(),
            &plan_id,
            1,
            "## tests.rs\n- new `checks_state`: State stays independent.\n- new `checks_order`: Order stays stable.\n",
        )
        .await?;
        let working = files.plan_dir("session", &plan_id).join("working.json");
        ReviewAnnotationStore::open(working.clone(), digest(&fs::read(&working)?))?.replace(
            vec![
                comment(
                    "kept",
                    line_of(&rendered, "Retain the public contract."),
                    "Name the contract.",
                ),
                comment(
                    "dropped",
                    line_of(&rendered, "Order stays stable."),
                    "Order by what?",
                ),
                comment(
                    "delivered",
                    line_of(&rendered, "Keep state."),
                    "Already sent.",
                ),
            ],
        )?;
        let snapshot = files.review_comment_snapshot("session", &plan_id)?;
        let delivery = ReviewDelivery {
            model_revision: 1,
            comment_ids: BTreeSet::from(["delivered".to_owned()]),
            ..Default::default()
        };
        let rendered = submit(
            &files,
            temporary.path(),
            &plan_id,
            2,
            "## tests.rs\n- new `checks_state`: State stays independent.\n",
        )
        .await?;
        let unplaced =
            files.carry_review_comments("session", &plan_id, snapshot, &delivery, &rendered)?;
        assert_eq!(unplaced, ["Order by what?"]);
        let carried = ReviewAnnotationStore::open(working.clone(), digest(&fs::read(&working)?))?
            .annotation()
            .to_vec();
        assert_eq!(
            carried
                .iter()
                .map(|annotation| (annotation.id.as_str(), annotation.source.start_line))
                .collect::<Vec<_>>(),
            [("kept", line_of(&rendered, "Retain the public contract."))]
        );
        Ok(())
    }

    /// Retirement removes the edits and row-less comments a prompt delivered, keeps the ones
    /// recorded after it, and adds the comments the new revision had no rows for.
    #[test]
    fn retirement_keeps_what_no_prompt_delivered() {
        let rename = |from: &str| PlanEdit::Rename {
            from: from.into(),
            to: format!("{from}_renamed"),
        };
        let temporary = tempfile::tempdir().unwrap();
        crate::checks::configure_fixture(temporary.path());
        let files = PlanFileStore::new(temporary.path().join("storage"), temporary.path()).unwrap();
        let mut plan = files
            .prepare_entry("session", "Document a plan.".into(), None, 1)
            .unwrap()
            .record;
        plan.manual_changes = vec![rename("delivered"), rename("later")];
        plan.unplaced_review_comments = vec!["sent".into(), "unsent".into()];
        let delivery = ReviewDelivery {
            model_revision: plan.model_revision,
            edits: vec![rename("delivered")],
            unplaced: vec!["sent".into()],
            ..Default::default()
        };
        plan.retire_delivered_review(&delivery, vec!["new".into()]);
        assert_eq!(plan.manual_changes, [rename("later")]);
        assert_eq!(plan.unplaced_review_comments, ["unsent", "new"]);
    }
}
