use anyhow::{Context, Result, ensure};
use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use forge_github::comment::{
    CommentKind, CommentOperation, CommentResult, CommentState, CommentTarget,
};
use serde::{Deserialize, Serialize};

use super::ReviewDocument;
use crate::comments::{
    CommentAnchor, CommentId, CommentSaveAction, CommentSaveOutcome, RemoteComment,
    RemoteCommentIdentity,
};
use crate::edit::ConflictResolution;

#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewCommentCommand {
    Recover {
        comment: CommentId,
        resolution: Option<forge_github::review_mutation::RecoveryResolution>,
    },
    DraftConversation,
    DraftInline {
        anchor: CommentAnchor,
    },
    DraftReply {
        parent: CommentId,
    },
    LoadThreadComment {
        thread_node_id: String,
        comment_node_id: String,
    },
    Load {
        target: Box<CommentTarget>,
        anchor: Option<CommentAnchor>,
    },
    Save {
        comment: CommentId,
        action: CommentSaveAction,
    },
    Reconcile {
        comment: CommentId,
    },
    Snapshot {
        comment: CommentId,
    },
    Resolve {
        comment: CommentId,
        base: RegionRevision,
        choice: ConflictResolution,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReviewCommentSnapshot {
    pub document: DocumentId,
    pub comment: CommentId,
    pub region: RegionId,
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub baseline: String,
    pub conflict: Option<String>,
    pub dirty: bool,
    pub uncertain: bool,
    pub saving: bool,
    pub deleted: bool,
    pub viewer_did_author: bool,
}

pub(crate) enum PreparedComment {
    Recovery {
        comment: CommentId,
        resolution: Option<forge_github::review_mutation::RecoveryResolution>,
    },
    Snapshot(ReviewCommentSnapshot),
    Remote {
        request: CommentRequest,
        settlement: CommentSettlement,
    },
}

pub(crate) enum CommentRequest {
    Existing {
        target: CommentTarget,
        operation: CommentOperation,
    },
    Create {
        mutation: forge_github::review_mutation::ReviewMutation,
        receipt: String,
        kind: CommentKind,
    },
}

pub(crate) enum CommentSettlement {
    Load {
        target: Box<CommentTarget>,
        anchor: Option<CommentAnchor>,
    },
    Create {
        comment: CommentId,
    },
    Save {
        comment: CommentId,
    },
    Reconcile {
        target: Box<CommentTarget>,
        comment: CommentId,
    },
}

impl ReviewDocument {
    pub(crate) fn recover_comment(
        &mut self,
        comment: CommentId,
        result: &CommentResult,
    ) -> Result<ReviewCommentSnapshot> {
        ensure!(
            matches!(
                result,
                CommentResult::Confirmed { .. } | CommentResult::Rejected { .. }
            ),
            "recovery has no terminal comment outcome"
        );
        if let Some(save) = &self.pending_comment {
            ensure!(
                save.comment() == comment,
                "another comment owns the pending mutation"
            );
            match result {
                CommentResult::Confirmed { state } => {
                    let anchor = self.comments.record(comment)?.anchor.clone();
                    self.comments.reconcile_save(
                        &mut self.edits,
                        save,
                        state.as_ref().map(|state| observed(state, anchor)),
                    )?;
                }
                CommentResult::Rejected { .. } => {
                    self.comments.close_unknown(&mut self.edits, save)?
                }
                _ => anyhow::bail!("recovery has no terminal comment outcome"),
            }
            self.pending_comment = None;
        } else if let CommentResult::Confirmed { state } = result {
            let anchor = self.comments.record(comment)?.anchor.clone();
            self.comments.recover_identity(
                &mut self.edits,
                comment,
                state.as_ref().map(|state| observed(state, anchor)),
            )?;
        }
        if let CommentResult::Confirmed { state: Some(state) } = result {
            let target = CommentTarget {
                pull_request: self.target.clone(),
                kind: state.kind,
                node_id: state.id.clone(),
                database_id: state.database_id,
            };
            state.validate(&target)?;
            self.comment_target.insert(comment, target);
        }
        self.saving = false;
        self.restored_uncertain_comment.remove(&comment);
        self.comment_snapshot(comment)
    }

    pub(crate) fn comment_settlement_draft(
        &self,
        comment: CommentId,
        result: &CommentResult,
    ) -> Result<serde_json::Value> {
        let mut draft = self.draft_payload()?;
        let entry = draft
            .get_mut("native_comments")
            .and_then(serde_json::Value::as_array_mut)
            .and_then(|entry| {
                entry.iter_mut().find(|entry| {
                    entry.get("identity").and_then(serde_json::Value::as_u64) == Some(comment.0)
                })
            })
            .context("captured comment is absent from durable draft")?;
        let field = &mut entry["field"];
        field["saving"] = false.into();
        field["uncertain"] = false.into();
        match result {
            CommentResult::Confirmed { state: Some(state) }
            | CommentResult::Reconciled { state: Some(state) } => {
                let target = CommentTarget {
                    pull_request: self.target.clone(),
                    kind: state.kind,
                    node_id: state.id.clone(),
                    database_id: state.database_id,
                };
                state.validate(&target)?;
                field["baseline"] = state.body.clone().into();
                field["dirty"] = (field.get("text").and_then(serde_json::Value::as_str)
                    != Some(state.body.as_str()))
                .into();
                field["viewer_did_author"] = state.viewer_did_author.into();
                field["conflict"] = serde_json::Value::Null;
                entry["record"]["remote"] = serde_json::to_value(RemoteCommentIdentity {
                    node_id: state.id.clone(),
                    database_id: state.database_id,
                    url: state.url.clone(),
                })?;
                entry["record"]["viewer_did_author"] = state.viewer_did_author.into();
                entry["record"]["reply_to"] = serde_json::Value::Null;
                entry["target"] = serde_json::to_value(target)?;
            }
            CommentResult::Confirmed { state: None }
            | CommentResult::Reconciled { state: None } => {
                field["deleted"] = true.into();
                entry["record"]["deleted"] = true.into();
            }
            CommentResult::Rejected { .. } => {}
            CommentResult::OutcomeUnknown { .. } => {
                anyhow::bail!("unknown comment outcome cannot acknowledge a draft")
            }
        }
        Ok(draft)
    }

    pub(crate) fn restore_comments(&mut self, draft: &serde_json::Value) -> Result<()> {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct StoredComment {
            identity: CommentId,
            record: crate::comments::CommentRecord,
            field: ReviewCommentSnapshot,
            target: Option<CommentTarget>,
        }
        let Some(stored) = draft.get("native_comments") else {
            return Ok(());
        };
        let stored: Vec<StoredComment> = serde_json::from_value(stored.clone())?;
        for stored in stored {
            ensure!(
                stored.identity == stored.field.comment,
                "stored comment field identity differs from its owner"
            );
            if let Some(target) = &stored.target {
                target.validate()?;
                ensure!(
                    target.pull_request.repository == self.target.repository
                        && target.pull_request.number == self.target.number
                        && target.pull_request.node_id == self.target.node_id,
                    "stored comment target belongs to another pull request"
                );
                let remote = stored
                    .record
                    .remote
                    .as_ref()
                    .context("stored comment target has no remote identity")?;
                ensure!(
                    remote.node_id == target.node_id && remote.database_id == target.database_id,
                    "stored comment target differs from its remote identity"
                );
            } else {
                ensure!(
                    stored.record.remote.is_none(),
                    "stored remote comment has no target"
                );
            }
            let identity = stored.identity;
            self.comments.restore(
                &mut self.edits,
                identity,
                stored.record,
                stored.field.baseline.clone(),
            )?;
            let region = self.comments.record(identity)?.region.clone();
            ensure!(
                stored.field.region == region,
                "stored comment region differs from its owner"
            );
            if stored.field.saving || stored.field.uncertain {
                self.restored_uncertain_comment.insert(identity);
            }
            self.edits.restore_field(
                region,
                crate::edit::FieldRestoration {
                    revision: stored.field.revision,
                    sequence: stored.field.sequence,
                    text: stored.field.text,
                    baseline: stored.field.baseline,
                    remote: stored.field.conflict,
                    pending: None,
                },
            )?;
            if let Some(target) = stored.target {
                self.comment_target.insert(identity, target);
            }
        }
        Ok(())
    }

    pub fn comment_snapshot(&self, comment: CommentId) -> Result<ReviewCommentSnapshot> {
        let record = self.comments.record(comment)?;
        let body = self.comments.body(&self.edits, comment)?;
        Ok(ReviewCommentSnapshot {
            document: self.id.clone(),
            comment,
            region: record.region.clone(),
            revision: body.revision,
            sequence: body.sequence,
            text: body.text.into(),
            baseline: body.baseline.into(),
            conflict: body.remote.map(str::to_owned),
            dirty: body.dirty,
            uncertain: body.uncertain || self.restored_uncertain_comment.contains(&comment),
            saving: self.saving
                && self
                    .pending_comment
                    .as_ref()
                    .is_some_and(|save| save.comment() == comment),
            deleted: record.deleted,
            viewer_did_author: record.viewer_did_author,
        })
    }

    pub(crate) fn prepare_comment(
        &mut self,
        command: ReviewCommentCommand,
    ) -> Result<PreparedComment> {
        if let ReviewCommentCommand::Snapshot { comment } = command {
            return Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?));
        }
        ensure!(!self.saving, "review save is already running");
        match command {
            ReviewCommentCommand::Recover {
                comment,
                resolution,
            } => {
                self.comments.record(comment)?;
                ensure!(
                    self.pending.is_empty()
                        && self
                            .pending_comment
                            .as_ref()
                            .is_none_or(|save| save.comment() == comment),
                    "another captured mutation requires recovery"
                );
                Ok(PreparedComment::Recovery {
                    comment,
                    resolution,
                })
            }
            ReviewCommentCommand::DraftConversation => {
                let comment = self.comments.draft(&mut self.edits, None)?;
                Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?))
            }
            ReviewCommentCommand::DraftInline { anchor } => {
                anchor.validate()?;
                u32::try_from(anchor.last_line).context("inline line exceeds GitHub limit")?;
                let comment = self.comments.draft(&mut self.edits, Some(anchor))?;
                Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?))
            }
            ReviewCommentCommand::DraftReply { parent } => {
                let comment = self.comments.reply_draft(&mut self.edits, parent)?;
                Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?))
            }
            ReviewCommentCommand::LoadThreadComment {
                thread_node_id,
                comment_node_id,
            } => {
                ensure!(
                    self.pending_comment.is_none(),
                    "comment outcome requires reconciliation"
                );
                let thread = self
                    .section
                    .get(&crate::service::ReviewSectionKind::Threads)
                    .into_iter()
                    .flat_map(|section| &section.item)
                    .filter_map(|item| item.thread.as_ref())
                    .find(|thread| thread.node_id == thread_node_id)
                    .context("thread is not retained by this document")?;
                let comment = thread
                    .comment
                    .iter()
                    .find(|comment| comment.node_id == comment_node_id)
                    .context("comment is not retained by this thread")?;
                let target = CommentTarget {
                    pull_request: self.target.clone(),
                    kind: CommentKind::PullRequestReviewComment,
                    node_id: comment.node_id.clone(),
                    database_id: comment.database_id,
                };
                let state = CommentState {
                    kind: target.kind,
                    id: comment.node_id.clone(),
                    database_id: comment.database_id,
                    body: comment.body.clone(),
                    url: comment.url.clone(),
                    viewer_did_author: comment.viewer_did_author,
                };
                state.validate(&target)?;
                let (commit, last, first) = if thread.outdated {
                    (
                        &comment.original_commit,
                        comment.original_line.or(thread.original_line),
                        comment.original_start_line.or(thread.original_start_line),
                    )
                } else {
                    (
                        &comment.commit,
                        comment.line.or(thread.line),
                        comment.start_line.or(thread.start_line),
                    )
                };
                let anchor = match (commit, last) {
                    (Some(commit), Some(last)) => Some(CommentAnchor {
                        revision: commit.clone(),
                        path: thread.path.clone(),
                        side: thread.side,
                        first_line: first.unwrap_or(last),
                        last_line: last,
                    }),
                    _ => None,
                };
                let (comment, _) = self
                    .comments
                    .merge(&mut self.edits, observed(&state, anchor))?;
                self.comment_target.insert(comment, target);
                Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?))
            }
            ReviewCommentCommand::Resolve {
                comment,
                base,
                choice,
            } => {
                let region = &self.comments.record(comment)?.region;
                self.edits.resolve(region, base, choice)?;
                Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?))
            }
            ReviewCommentCommand::Load { target, anchor } => {
                ensure!(
                    self.pending_comment.is_none(),
                    "comment outcome requires reconciliation"
                );
                target.validate()?;
                ensure!(
                    target.pull_request.repository == self.target.repository
                        && target.pull_request.number == self.target.number
                        && target.pull_request.node_id == self.target.node_id,
                    "comment belongs to another review document"
                );
                if let Some(anchor) = &anchor {
                    anchor.validate()?;
                }
                Ok(PreparedComment::Remote {
                    request: CommentRequest::Existing {
                        target: (*target).clone(),
                        operation: CommentOperation::Reconcile,
                    },
                    settlement: CommentSettlement::Load { target, anchor },
                })
            }
            ReviewCommentCommand::Save { comment, action } => {
                ensure!(
                    !self.restored_uncertain_comment.contains(&comment),
                    "comment outcome requires explicit recovery"
                );
                ensure!(
                    self.pending.is_empty() && self.pending_comment.is_none(),
                    "review outcome requires reconciliation"
                );
                let target = self.comment_target.get(&comment).cloned();
                let creation = if target.is_none() {
                    let record = self.comments.record(comment)?;
                    ensure!(
                        record.remote.is_none(),
                        "remote comment has no native target"
                    );
                    let body = self.comments.body(&self.edits, comment)?.text.to_owned();
                    use forge_github::review_mutation::{DiffSide, ReviewMutation};
                    Some(if let Some(parent) = record.reply_to {
                        let parent = self
                            .comment_target
                            .get(&parent)
                            .context("reply parent has no native target")?;
                        ensure!(
                            parent.kind == CommentKind::PullRequestReviewComment,
                            "reply parent is not an inline comment"
                        );
                        (
                            ReviewMutation::ReplyCreate {
                                body,
                                parent_comment_id: parent.database_id,
                            },
                            CommentKind::PullRequestReviewComment,
                        )
                    } else if let Some(anchor) = &record.anchor {
                        let side = match anchor.side {
                            crate::comments::CommentSide::Left => DiffSide::Left,
                            crate::comments::CommentSide::Right => DiffSide::Right,
                        };
                        let multiline = anchor.first_line != anchor.last_line;
                        (
                            ReviewMutation::InlineCreate {
                                body,
                                path: anchor.path.clone(),
                                line: u32::try_from(anchor.last_line)
                                    .context("inline line exceeds GitHub limit")?,
                                side,
                                start_line: multiline
                                    .then(|| u32::try_from(anchor.first_line))
                                    .transpose()?,
                                start_side: multiline.then_some(side),
                                commit_id: anchor.revision.clone(),
                            },
                            CommentKind::PullRequestReviewComment,
                        )
                    } else {
                        (
                            ReviewMutation::ConversationCreate { body },
                            CommentKind::IssueComment,
                        )
                    })
                } else {
                    None
                };
                let Some(save) = self.comments.save(&mut self.edits, comment, action)? else {
                    return Ok(PreparedComment::Snapshot(self.comment_snapshot(comment)?));
                };
                let receipt = uuid::Uuid::new_v4().to_string();
                let (request, settlement) = if let Some(target) = target {
                    let operation = match action {
                        CommentSaveAction::Save => CommentOperation::Edit {
                            body: save.text().into(),
                            receipt,
                        },
                        CommentSaveAction::Delete => CommentOperation::Delete { receipt },
                    };
                    (
                        CommentRequest::Existing { target, operation },
                        CommentSettlement::Save { comment },
                    )
                } else {
                    (
                        {
                            let (mutation, kind) = creation.expect("validated native draft");
                            CommentRequest::Create {
                                mutation,
                                receipt,
                                kind,
                            }
                        },
                        CommentSettlement::Create { comment },
                    )
                };
                self.pending_comment = Some(save);
                self.saving = true;
                Ok(PreparedComment::Remote {
                    request,
                    settlement,
                })
            }
            ReviewCommentCommand::Reconcile { comment } => {
                let save = self
                    .pending_comment
                    .as_ref()
                    .context("comment has no uncertain save")?;
                ensure!(
                    save.comment() == comment,
                    "another comment requires reconciliation"
                );
                let target = self
                    .comment_target
                    .get(&comment)
                    .context("comment has no native target")?
                    .clone();
                Ok(PreparedComment::Remote {
                    request: CommentRequest::Existing {
                        target: target.clone(),
                        operation: CommentOperation::Reconcile,
                    },
                    settlement: CommentSettlement::Reconcile {
                        target: Box::new(target),
                        comment,
                    },
                })
            }
            ReviewCommentCommand::Snapshot { .. } => unreachable!(),
        }
    }

    pub(crate) fn settle_comment(
        &mut self,
        settlement: CommentSettlement,
        result: Option<&CommentResult>,
    ) -> Result<ReviewCommentSnapshot> {
        match settlement {
            CommentSettlement::Load { target, anchor } => {
                let Some(CommentResult::Reconciled { state: Some(state) }) = result else {
                    anyhow::bail!("comment load omitted remote body");
                };
                state.validate(&target)?;
                let (comment, _) = self
                    .comments
                    .merge(&mut self.edits, observed(state, anchor))?;
                self.comment_target.insert(comment, *target);
                self.comment_snapshot(comment)
            }
            CommentSettlement::Create { comment } => {
                let target = if let Some(CommentResult::Confirmed { state: Some(state) }) = result {
                    let target = CommentTarget {
                        pull_request: self.target.clone(),
                        kind: state.kind,
                        node_id: state.id.clone(),
                        database_id: state.database_id,
                    };
                    target.validate()?;
                    state.validate(&target)?;
                    Some(target)
                } else {
                    None
                };
                self.complete_comment_save(result)?;
                if let Some(target) = target {
                    self.comment_target.insert(comment, target);
                }
                self.comment_snapshot(comment)
            }
            CommentSettlement::Save { comment } => {
                self.complete_comment_save(result)?;
                self.comment_snapshot(comment)
            }
            CommentSettlement::Reconcile { target, comment } => {
                let Some(CommentResult::Reconciled { state }) = result else {
                    anyhow::bail!("comment reconciliation omitted observation");
                };
                if let Some(state) = state {
                    state.validate(&target)?;
                }
                let anchor = self.comments.record(comment)?.anchor.clone();
                let save = self
                    .pending_comment
                    .as_ref()
                    .context("comment has no uncertain save")?;
                self.comments.reconcile_save(
                    &mut self.edits,
                    save,
                    state.as_ref().map(|state| observed(state, anchor)),
                )?;
                self.pending_comment = None;
                self.comment_snapshot(comment)
            }
        }
    }

    fn complete_comment_save(&mut self, result: Option<&CommentResult>) -> Result<()> {
        if !self.saving {
            return Ok(());
        }
        let save = self
            .pending_comment
            .as_ref()
            .context("comment save capture is missing")?;
        let anchor = self.comments.record(save.comment())?.anchor.clone();
        let outcome = match result {
            Some(CommentResult::Confirmed { state: Some(state) }) => {
                CommentSaveOutcome::Confirmed(observed(state, anchor))
            }
            Some(CommentResult::Confirmed { state: None }) => CommentSaveOutcome::Deleted,
            Some(CommentResult::Rejected { .. }) => CommentSaveOutcome::Rejected,
            _ => CommentSaveOutcome::Uncertain,
        };
        let uncertain = matches!(outcome, CommentSaveOutcome::Uncertain);
        self.saving = false;
        self.comments
            .complete_save(&mut self.edits, save, outcome)?;
        if !uncertain {
            self.pending_comment = None;
        }
        Ok(())
    }

    pub(crate) fn abandon_save(&mut self) {
        if self.pending_comment.is_some() {
            let _ = self.complete_comment_save(None);
        } else {
            let _ = self.complete_save(None);
        }
    }
}

fn observed(state: &CommentState, anchor: Option<CommentAnchor>) -> RemoteComment {
    RemoteComment {
        identity: RemoteCommentIdentity {
            node_id: state.id.clone(),
            database_id: state.database_id,
            url: state.url.clone(),
        },
        anchor,
        viewer_did_author: state.viewer_did_author,
        body: state.body.clone(),
    }
}
