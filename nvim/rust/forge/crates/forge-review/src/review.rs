use anyhow::{Context, Result, ensure};
use std::collections::BTreeMap;

mod comment;
mod restore;
pub(crate) use comment::{CommentRequest, CommentSettlement, PreparedComment};
pub use comment::{ReviewCommentCommand, ReviewCommentSnapshot};
use forge_buffer::editable::EditAcknowledgement;
use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use forge_git::identity::RepositoryPath;
use forge_github::pull_request::{
    PullRequestEdit, PullRequestOutcome, PullRequestResult, PullRequestTarget,
};
use forge_github::review_mutation::{
    DiffSide, ReviewEvent, ReviewMutation, ReviewSubmissionComment,
};
use serde::{Deserialize, Serialize};

use crate::comments::{CommentLimits, CommentStore};
use crate::edit::{EditBudget, EditLimits, EditStore, RegionEdit, SaveOutcome, SaveSubmission};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewMode {
    Overview,
    Batched,
}

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewVerdict {
    Approve,
    RequestChanges,
    Comment,
}

impl ReviewVerdict {
    pub(crate) fn event(self) -> ReviewEvent {
        match self {
            Self::Approve => ReviewEvent::Approve,
            Self::RequestChanges => ReviewEvent::RequestChanges,
            Self::Comment => ReviewEvent::Comment,
        }
    }
}

#[derive(Debug, Serialize, serde::Deserialize)]
pub struct ReviewField {
    pub region: RegionId,
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub baseline: String,
    pub dirty: bool,
    pub uncertain: bool,
}

#[derive(Debug, Serialize)]
pub struct ReviewSnapshot {
    pub document: DocumentId,
    pub field: Vec<ReviewField>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<ReviewField>,
    pub saving: bool,
    pub uncertain: bool,
}

/// Owns PR fields, comment bodies, and one submitted mutation independently of physical buffer rows.
/// Local edits remain admissible during a save. A second save requires settlement of the first.
pub struct ReviewDocument {
    pub(crate) views: forge_buffer::view::DocumentViews,
    pub(crate) input:
        BTreeMap<forge_buffer::identity::ViewId, forge_buffer::identity::InputSequence>,
    pub(crate) projection_target:
        BTreeMap<forge_buffer::identity::TargetId, crate::service::thread_projection::ReviewTarget>,
    pub(crate) thread_loading: std::collections::BTreeSet<String>,
    pub(crate) projection_width: Option<forge_buffer::width::WidthProfile>,
    pub(crate) file: BTreeMap<String, crate::service::file::RetainedFile>,
    pub(crate) file_loading: std::collections::BTreeSet<String>,
    pub(crate) commit_diff: crate::commit_diff::CommitDiffStore,
    pub(crate) commit_projected: BTreeMap<String, usize>,
    pub(crate) projection_link:
        BTreeMap<forge_buffer::identity::BlockId, Vec<forge_buffer::markdown::MarkdownLink>>,
    pub(crate) section_revision: u64,
    pub(crate) section:
        BTreeMap<crate::service::ReviewSectionKind, crate::service::ReviewSectionState>,
    pub(crate) projection: Option<forge_buffer::document::BufferDocument>,
    pub(crate) id: DocumentId,
    pub(crate) target: PullRequestTarget,
    pub(crate) mode: ReviewMode,
    pub(crate) viewed_file: std::collections::BTreeSet<String>,
    edits: EditStore,
    pub comments: CommentStore,
    pending: Vec<SaveSubmission>,
    pub(crate) pending_operation: Option<String>,
    restored_uncertain_comment: std::collections::BTreeSet<crate::comments::CommentId>,
    saving: bool,
    pending_comment: Option<crate::comments::CommentSave>,
    comment_target: BTreeMap<crate::comments::CommentId, forge_github::comment::CommentTarget>,
    pub(crate) batched_operation: Option<String>,
    pub(crate) batched_capture: Option<ReviewMutation>,
}

impl ReviewDocument {
    pub(crate) fn draft_payload(&self) -> Result<serde_json::Value> {
        let comments: Vec<_> = self.comments.records().map(|(identity, record)| {
            Ok(serde_json::json!({"identity":identity, "record":record, "field":self.comment_snapshot(identity)?, "target":self.comment_target.get(&identity)}))
        }).collect::<Result<_>>()?;
        Ok(
            serde_json::json!({"repo":self.target.repository.repository_name(), "number":self.target.number,
                "pr_fields":self.snapshot()?, "pr_pending":self.pending_draft(), "pr_operation":self.pending_operation,
                "native_comments":comments, "batched": self.batched_draft()?,
                "batched_operation": self.batched_operation,
                "batched_capture": self.batched_capture}),
        )
    }

    /// Adopts validated remote text as the initial baseline without accepting client-owned baselines.
    pub fn open(
        id: DocumentId,
        target: PullRequestTarget,
        text: PullRequestEdit,
        budget: EditBudget,
    ) -> Result<Self> {
        target.validate()?;
        text.validate()?;
        let title = text.title.context("PR observation omitted title")?;
        let body = text.body.context("PR observation omitted body")?;
        let mut edits = EditStore::new(id.clone(), EditLimits::default(), budget)?;
        let comments = CommentStore::new(
            id.clone(),
            target.clone(),
            ReviewMode::Overview,
            CommentLimits::default(),
        )?;
        edits.insert(RegionId("title".into()), RegionRevision(0), title)?;
        edits.insert(RegionId("body".into()), RegionRevision(0), body)?;
        Ok(Self {
            views: forge_buffer::view::DocumentViews::default(),
            input: BTreeMap::new(),
            projection_target: BTreeMap::new(),
            thread_loading: std::collections::BTreeSet::new(),
            projection_width: None,
            file: BTreeMap::new(),
            file_loading: std::collections::BTreeSet::new(),
            commit_diff: crate::commit_diff::CommitDiffStore::default(),
            commit_projected: BTreeMap::new(),
            projection_link: BTreeMap::new(),
            section_revision: 0,
            section: BTreeMap::new(),
            projection: None,
            id,
            target,
            mode: ReviewMode::Overview,
            viewed_file: std::collections::BTreeSet::new(),
            edits,
            comments,
            pending: Vec::new(),
            pending_operation: None,
            restored_uncertain_comment: std::collections::BTreeSet::new(),
            saving: false,
            pending_comment: None,
            comment_target: BTreeMap::new(),
            batched_operation: None,
            batched_capture: None,
        })
    }

    /// Retains native text without requiring a layout acknowledgement. Title shape is checked at save.
    pub fn region_edit(&mut self, edit: RegionEdit) -> Result<EditAcknowledgement> {
        if self.comments.contains_region(&edit.region) {
            return self.comments.accept(&mut self.edits, edit);
        }
        if edit.region.0 == "title" {
            ensure!(
                edit.text.len() <= 4096,
                "PR title exceeds its local text limit"
            );
        }
        Ok(self.edits.accept(edit)?)
    }

    /// Captures all dirty fields as one remote mutation, leaving omitted fields unchanged.
    pub(crate) fn begin_save(&mut self, operation_id: String) -> Result<Option<PullRequestEdit>> {
        ensure!(!self.saving, "review save is already running");
        ensure!(
            self.pending.is_empty()
                && self.pending_comment.is_none()
                && self.restored_uncertain_comment.is_empty(),
            "review save outcome requires reconciliation"
        );
        let title = self.edits.snapshot(&RegionId("title".into()))?;
        let body = self.edits.snapshot(&RegionId("body".into()))?;
        if !title.dirty && !body.dirty {
            return Ok(None);
        }
        let edit = PullRequestEdit {
            title: title.dirty.then(|| title.text.to_owned()),
            body: body.dirty.then(|| body.text.to_owned()),
        };
        edit.validate()?;
        for region in [RegionId("title".into()), RegionId("body".into())] {
            match self.edits.begin_save(&region) {
                Ok(Some(submission)) => self.pending.push(submission),
                Ok(None) => {}
                Err(error) => {
                    for submission in self.pending.drain(..) {
                        self.edits
                            .complete_save(&submission, SaveOutcome::Rejected)?;
                    }
                    return Err(error.into());
                }
            }
        }
        self.saving = true;
        self.pending_operation = Some(operation_id);
        Ok(Some(edit))
    }

    /// Settles only captured fields. Delivery failure conservatively retains uncertainty.
    pub(crate) fn complete_save(&mut self, result: Option<&PullRequestResult>) -> Result<()> {
        if !self.saving {
            return Ok(());
        }
        let outcome = match result {
            Some(result)
                if result.outcome == PullRequestOutcome::Confirmed
                    && result.matches_submission == Some(true) =>
            {
                SaveOutcome::Confirmed
            }
            Some(result) if result.outcome == PullRequestOutcome::Rejected => SaveOutcome::Rejected,
            _ => SaveOutcome::Uncertain,
        };
        for submission in &self.pending {
            self.edits.complete_save(submission, outcome)?;
        }
        self.saving = false;
        if outcome != SaveOutcome::Uncertain {
            self.pending.clear();
            self.pending_operation = None;
        }
        Ok(())
    }

    /// Applies a read-only observation to unresolved captures while preserving all current text.
    /// If byte admission fails, unreconciled captures remain available for another observation.
    pub(crate) fn reconcile(&mut self, text: PullRequestEdit) -> Result<()> {
        ensure!(!self.saving, "review save is still running");
        text.validate()?;
        let title = text.title.context("PR observation omitted title")?;
        let body = text.body.context("PR observation omitted body")?;
        while let Some(submission) = self.pending.last() {
            let observed = if submission.region().0 == "title" {
                &title
            } else {
                &body
            };
            self.edits.reconcile_save(submission, observed.clone())?;
            self.pending.pop();
        }
        self.pending_operation = None;
        Ok(())
    }

    pub fn snapshot(&self) -> Result<ReviewSnapshot> {
        let mut field = Vec::with_capacity(2);
        for region in [RegionId("title".into()), RegionId("body".into())] {
            let state = self.edits.snapshot(&region)?;
            field.push(ReviewField {
                region,
                revision: state.revision,
                sequence: state.sequence,
                text: state.text.into(),
                baseline: state.baseline.into(),
                dirty: state.dirty,
                uncertain: state.uncertain,
            });
        }
        Ok(ReviewSnapshot {
            document: self.id.clone(),
            uncertain: !self.saving && (!self.pending.is_empty() || self.pending_comment.is_some()),
            field,
            summary: (self.mode == ReviewMode::Batched)
                .then(|| self.summary_field())
                .transpose()?,
            saving: self.saving,
        })
    }

    fn batched_draft(&self) -> Result<Option<serde_json::Value>> {
        (self.mode == ReviewMode::Batched)
            .then(|| {
                Ok(serde_json::json!({
                    "mode": ReviewMode::Batched,
                    "summary": self.summary_field()?,
                    "viewed_file": self.viewed_file,
                }))
            })
            .transpose()
    }

    fn summary_field(&self) -> Result<ReviewField> {
        let state = self.edits.snapshot(&RegionId("review_summary".into()))?;
        Ok(ReviewField {
            region: RegionId("review_summary".into()),
            revision: state.revision,
            sequence: state.sequence,
            text: state.text.into(),
            baseline: state.baseline.into(),
            dirty: state.dirty,
            uncertain: state.uncertain,
        })
    }

    pub(crate) fn begin_batched(&mut self) -> Result<()> {
        ensure!(
            self.mode == ReviewMode::Overview,
            "review document is already batched"
        );
        ensure!(
            self.pending.is_empty() && self.pending_comment.is_none(),
            "review outcome requires reconciliation"
        );
        self.edits.insert(
            RegionId("review_summary".into()),
            RegionRevision(0),
            String::new(),
        )?;
        self.comments.set_mode(ReviewMode::Batched)?;
        self.mode = ReviewMode::Batched;
        Ok(())
    }

    pub(crate) fn begin_batched_submission(
        &mut self,
        operation_id: String,
        verdict: ReviewVerdict,
    ) -> Result<(ReviewMutation, u64)> {
        ensure!(
            self.mode == ReviewMode::Batched,
            "review submission requires batched mode"
        );
        ensure!(
            !self.saving
                && self.pending.is_empty()
                && self.pending_comment.is_none()
                && self.restored_uncertain_comment.is_empty()
                && self.batched_operation.is_none(),
            "review outcome requires explicit recovery"
        );
        let summary = self.summary_field()?;
        let mut comment = Vec::new();
        let mut commit_id = None;
        let mut sequence = summary.sequence.0;
        for (identity, record) in self.comments.records() {
            if record.deleted || record.remote.is_some() || record.reply_to.is_some() {
                continue;
            }
            let Some(anchor) = &record.anchor else {
                continue;
            };
            let body = self.comments.body(&self.edits, identity)?;
            ensure!(
                !body.text.trim().is_empty(),
                "review draft comment body is empty"
            );
            sequence = sequence.max(body.sequence.0);
            if let Some(existing) = &commit_id {
                ensure!(
                    existing == &anchor.revision,
                    "review drafts span different head revisions"
                );
            } else {
                commit_id = Some(anchor.revision.clone());
            }
            let side = match anchor.side {
                crate::comments::CommentSide::Left => DiffSide::Left,
                crate::comments::CommentSide::Right => DiffSide::Right,
            };
            let start_line = (anchor.first_line != anchor.last_line)
                .then(|| u32::try_from(anchor.first_line))
                .transpose()?;
            comment.push(ReviewSubmissionComment {
                body: body.text.into(),
                path: anchor.path.clone(),
                line: u32::try_from(anchor.last_line)?,
                side,
                start_line,
                start_side: start_line.map(|_| side),
            });
        }
        ensure!(
            commit_id.is_some(),
            "review submission requires an inline draft at the retained PR head"
        );
        let mutation = ReviewMutation::ReviewSubmit {
            body: summary.text,
            event: verdict.event(),
            pending_review_id: None,
            commit_id,
            pending_comment: Some(Vec::new()),
            comment,
        };
        self.saving = true;
        self.batched_operation = Some(operation_id);
        self.batched_capture = Some(mutation.clone());
        Ok((mutation, sequence))
    }

    pub(crate) fn settle_batched_submission(
        &mut self,
        confirmed: bool,
        terminal: bool,
    ) -> Result<()> {
        ensure!(
            self.batched_operation.is_some(),
            "review submission has no captured operation"
        );
        if confirmed {
            let summary = self
                .edits
                .snapshot(&RegionId("review_summary".into()))?
                .text
                .to_owned();
            self.edits
                .restore_confirmed_baseline(&RegionId("review_summary".into()), summary)?;
            self.comments.mark_local_inline_deleted()?;
            self.batched_operation = None;
            self.batched_capture = None;
        } else if terminal {
            self.batched_operation = None;
            self.batched_capture = None;
        }
        self.saving = false;
        Ok(())
    }

    pub(crate) fn set_viewed(&mut self, path: String, viewed: bool) -> Result<()> {
        ensure!(
            self.mode == ReviewMode::Batched,
            "viewed state requires batched review mode"
        );
        RepositoryPath::new(path.as_bytes().to_vec())?;
        if viewed {
            self.viewed_file.insert(path);
        } else {
            self.viewed_file.remove(&path);
        }
        Ok(())
    }
}
