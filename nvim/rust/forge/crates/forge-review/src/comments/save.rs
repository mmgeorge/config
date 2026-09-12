use serde::Deserialize;
use std::sync::Arc;

use anyhow::{Context, Result, ensure};

use super::{CommentAnchor, CommentId, CommentStore, RemoteComment, RemoteCommentIdentity};
use crate::edit::{EditStore, SaveOutcome, SaveSubmission};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommentSaveAction {
    Save,
    Delete,
}

#[derive(Debug)]
pub enum CommentMutation {
    Create { anchor: Option<CommentAnchor> },
    Edit { remote: RemoteCommentIdentity },
    Reply { parent: RemoteCommentIdentity },
    Delete { remote: RemoteCommentIdentity },
}

/// Retains one immutable operation and its budgeted submitted body across view closure.
#[derive(Clone, Debug)]
pub struct CommentSave {
    record: Arc<CommentSaveRecord>,
}

#[derive(Debug)]
struct CommentSaveRecord {
    comment: CommentId,
    mutation: CommentMutation,
    submission: SaveSubmission,
}

pub(super) struct PendingCommentSave {
    record: Arc<CommentSaveRecord>,
    uncertain: bool,
}

#[derive(Debug)]
pub enum CommentSaveOutcome {
    Confirmed(RemoteComment),
    Deleted,
    Rejected,
    Uncertain,
}

impl CommentSave {
    pub fn comment(&self) -> CommentId {
        self.record.comment
    }

    pub fn mutation(&self) -> &CommentMutation {
        &self.record.mutation
    }

    pub fn text(&self) -> &str {
        self.record.submission.text()
    }
}

impl CommentStore {
    /// Captures one operation per owner. Unknown outcomes block admission until observation.
    pub fn save(
        &mut self,
        edits: &mut EditStore,
        comment: CommentId,
        action: CommentSaveAction,
    ) -> Result<Option<CommentSave>> {
        self.check_owner(edits)?;
        ensure!(
            !self.pending.contains_key(&comment),
            "comment mutation is pending"
        );
        let record = self.record(comment)?;
        ensure!(
            record.viewer_did_author && !record.deleted,
            "comment is not writable"
        );
        let mutation = match (action, &record.remote, record.reply_to) {
            (CommentSaveAction::Delete, Some(remote), _) => CommentMutation::Delete {
                remote: remote.clone(),
            },
            (CommentSaveAction::Delete, None, _) => {
                anyhow::bail!("local draft has no remote deletion target")
            }
            (CommentSaveAction::Save, Some(remote), _) => {
                if !edits.snapshot(&record.region)?.dirty {
                    return Ok(None);
                }
                CommentMutation::Edit {
                    remote: remote.clone(),
                }
            }
            (CommentSaveAction::Save, None, Some(parent)) => {
                let parent = self.record(parent)?;
                ensure!(!parent.deleted, "reply parent was deleted");
                CommentMutation::Reply {
                    parent: parent
                        .remote
                        .clone()
                        .context("reply parent has no remote identity")?,
                }
            }
            (CommentSaveAction::Save, None, None) => CommentMutation::Create {
                anchor: record.anchor.clone(),
            },
        };
        if action == CommentSaveAction::Save {
            ensure!(
                !edits.snapshot(&record.region)?.text.trim().is_empty(),
                "comment body is empty"
            );
        }
        let submission = edits
            .capture(&record.region, true)?
            .expect("required comment capture");
        let record = Arc::new(CommentSaveRecord {
            comment,
            mutation,
            submission,
        });
        self.pending.insert(
            comment,
            PendingCommentSave {
                record: Arc::clone(&record),
                uncertain: false,
            },
        );
        Ok(Some(CommentSave { record }))
    }

    /// Applies correlated remote evidence. Invalid confirmations remain uncertain and cannot repost.
    pub fn complete_save(
        &mut self,
        edits: &mut EditStore,
        save: &CommentSave,
        outcome: CommentSaveOutcome,
    ) -> Result<()> {
        self.check_save(edits, save, false)?;
        if matches!(outcome, CommentSaveOutcome::Rejected) {
            edits.complete_save(&save.record.submission, SaveOutcome::Rejected)?;
            self.pending.remove(&save.comment());
            return Ok(());
        }
        edits.complete_save(&save.record.submission, SaveOutcome::Uncertain)?;
        self.pending
            .get_mut(&save.comment())
            .expect("validated comment save")
            .uncertain = true;
        match outcome {
            CommentSaveOutcome::Confirmed(remote) => {
                ensure!(
                    remote.body == save.text(),
                    "comment confirmation differs from submitted text"
                );
                ensure!(
                    !matches!(save.mutation(), CommentMutation::Delete { .. }),
                    "deletion received a body confirmation"
                );
                self.reconcile_save(edits, save, Some(remote))
            }
            CommentSaveOutcome::Deleted => {
                ensure!(
                    matches!(save.mutation(), CommentMutation::Delete { .. }),
                    "non-delete operation received deletion confirmation"
                );
                self.reconcile_save(edits, save, None)
            }
            CommentSaveOutcome::Uncertain => Ok(()),
            CommentSaveOutcome::Rejected => unreachable!(),
        }
    }

    pub(crate) fn close_unknown(
        &mut self,
        edits: &mut EditStore,
        save: &CommentSave,
    ) -> Result<()> {
        self.check_save(edits, save, true)?;
        edits.close_unknown(&save.record.submission)?;
        self.pending.remove(&save.comment());
        Ok(())
    }

    /// Observes an uncertain target without retrying. Absence cannot disprove an unknown creation.
    pub fn reconcile_save(
        &mut self,
        edits: &mut EditStore,
        save: &CommentSave,
        observed: Option<RemoteComment>,
    ) -> Result<()> {
        self.check_save(edits, save, true)?;
        let comment = save.comment();
        match observed {
            Some(remote) => {
                self.validate_remote(&remote)?;
                let existing = self.record(comment)?;
                ensure!(
                    remote.viewer_did_author,
                    "mutation observation is not viewer authored"
                );
                match save.mutation() {
                    CommentMutation::Edit { remote: identity }
                    | CommentMutation::Delete { remote: identity } => {
                        ensure!(
                            identity.node_id == remote.identity.node_id
                                && identity.database_id == remote.identity.database_id,
                            "mutation observation has another identity"
                        );
                    }
                    CommentMutation::Create { .. } | CommentMutation::Reply { .. } => {
                        ensure!(
                            remote.anchor == existing.anchor,
                            "creation observation has another anchor"
                        );
                        ensure!(
                            !self.remote.contains_key(&remote.identity.node_id),
                            "creation identity belongs to another comment"
                        );
                    }
                }
                edits.reconcile_save(&save.record.submission, remote.body)?;
                self.remote.insert(remote.identity.node_id.clone(), comment);
                let record = self.comment.get_mut(&comment).expect("validated comment");
                record.remote = Some(remote.identity);
                record.anchor = remote.anchor;
                if let Some(parent) = record.reply_to.take() {
                    self.reply.remove(&parent);
                }
            }
            None => {
                ensure!(
                    matches!(
                        save.mutation(),
                        CommentMutation::Edit { .. } | CommentMutation::Delete { .. }
                    ),
                    "absence cannot resolve an unknown creation"
                );
                edits.reconcile_save(&save.record.submission, String::new())?;
                self.comment
                    .get_mut(&comment)
                    .expect("validated comment")
                    .deleted = true;
            }
        }
        self.pending.remove(&comment);
        Ok(())
    }

    fn check_save(&self, edits: &EditStore, save: &CommentSave, uncertain: bool) -> Result<()> {
        self.check_owner(edits)?;
        let pending = self
            .pending
            .get(&save.comment())
            .context("unknown comment save")?;
        ensure!(
            Arc::ptr_eq(&pending.record, &save.record),
            "comment save belongs to another operation"
        );
        ensure!(
            pending.uncertain == uncertain,
            "comment save requires a different settlement phase"
        );
        Ok(())
    }
}
