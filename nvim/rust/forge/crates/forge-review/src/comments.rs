use std::collections::BTreeMap;

use anyhow::{Context, Result, ensure};
use forge_buffer::MAX_COUNTER;
use forge_buffer::editable::EditAcknowledgement;
use forge_buffer::identity::{DocumentId, RegionId, RegionRevision};
use forge_git::identity::RepositoryPath;
use forge_github::pull_request::PullRequestTarget;
use serde::{Deserialize, Serialize};

use crate::edit::{EditStore, FieldSnapshot, MergeOutcome, RegionEdit};
use crate::review::ReviewMode;

mod save;
use save::PendingCommentSave;
pub use save::{CommentMutation, CommentSave, CommentSaveAction, CommentSaveOutcome};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct CommentId(pub u64);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct CommentOccurrenceId(pub u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommentSide {
    Left,
    Right,
}

/// Immutable source coordinates, independent of the rows used to render a comment occurrence.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CommentAnchor {
    pub revision: String,
    pub path: String,
    pub side: CommentSide,
    pub first_line: u64,
    pub last_line: u64,
}

impl CommentAnchor {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            (self.revision.len() == 40 || self.revision.len() == 64)
                && self.revision.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "comment anchor requires a full commit identity"
        );
        ensure!(
            self.path.len() <= 4096,
            "comment source path exceeds its byte limit"
        );
        RepositoryPath::new(self.path.as_bytes().to_vec())?;
        ensure!(
            self.first_line > 0
                && self.first_line <= self.last_line
                && self.last_line <= MAX_COUNTER,
            "invalid comment source range"
        );
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct RemoteCommentIdentity {
    pub node_id: String,
    pub database_id: u64,
    pub url: String,
}

#[derive(Debug)]
pub struct RemoteComment {
    pub identity: RemoteCommentIdentity,
    pub anchor: Option<CommentAnchor>,
    pub viewer_did_author: bool,
    pub body: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OccurrenceKind {
    Conversation,
    Inline,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FocusReason {
    Open,
    Cursor,
}

#[derive(Debug)]
pub struct CommentOccurrence {
    pub comment: CommentId,
    pub kind: OccurrenceKind,
}

#[derive(Debug)]
pub enum CommentActionTarget {
    Comment(CommentOccurrenceId),
    Code(CommentAnchor),
}

#[derive(Debug, Eq, PartialEq)]
pub enum CommentBrowserTarget<'store> {
    Comment(&'store str),
    Code(&'store CommentAnchor),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CommentRecord {
    pub region: RegionId,
    pub remote: Option<RemoteCommentIdentity>,
    pub anchor: Option<CommentAnchor>,
    pub viewer_did_author: bool,
    pub reply_to: Option<CommentId>,
    pub deleted: bool,
}

#[derive(Clone, Copy, Debug)]
pub struct CommentLimits {
    pub comments: usize,
    pub occurrences: usize,
    pub occurrence_lifetimes: u64,
}

impl Default for CommentLimits {
    fn default() -> Self {
        Self {
            comments: 256,
            occurrences: 1024,
            occurrence_lifetimes: 4096,
        }
    }
}

/// Owns comment identity and occurrence focus. Bodies live in the document's shared EditStore.
/// One reply draft per parent survives occurrence changes and remote refreshes.
pub struct CommentStore {
    document: DocumentId,
    target: PullRequestTarget,
    mode: ReviewMode,
    limits: CommentLimits,
    next_comment: u64,
    next_occurrence: u64,
    comment: BTreeMap<CommentId, CommentRecord>,
    remote: BTreeMap<String, CommentId>,
    region: BTreeMap<RegionId, CommentId>,
    occurrence: BTreeMap<CommentOccurrenceId, CommentOccurrence>,
    reply: BTreeMap<CommentId, CommentId>,
    focused: Option<CommentOccurrenceId>,
    pending: BTreeMap<CommentId, PendingCommentSave>,
}

impl CommentStore {
    pub(crate) fn set_mode(&mut self, mode: ReviewMode) -> Result<()> {
        ensure!(
            self.pending.is_empty(),
            "comment outcome requires reconciliation"
        );
        ensure!(
            self.reply.is_empty(),
            "inline reply drafts prevent batched review mode"
        );
        self.mode = mode;
        Ok(())
    }
    pub(crate) fn records(&self) -> impl Iterator<Item = (CommentId, &CommentRecord)> {
        self.comment
            .iter()
            .map(|(identity, record)| (*identity, record))
    }

    pub fn new(
        document: DocumentId,
        target: PullRequestTarget,
        mode: ReviewMode,
        limits: CommentLimits,
    ) -> Result<Self> {
        document.validate()?;
        target.validate()?;
        ensure!(
            limits.comments > 0
                && limits.occurrences > 0
                && limits.occurrence_lifetimes >= limits.occurrences as u64
                && limits.occurrence_lifetimes <= MAX_COUNTER,
            "invalid comment limits"
        );
        Ok(Self {
            document,
            target,
            mode,
            limits,
            next_comment: 0,
            next_occurrence: 0,
            comment: BTreeMap::new(),
            remote: BTreeMap::new(),
            region: BTreeMap::new(),
            occurrence: BTreeMap::new(),
            reply: BTreeMap::new(),
            focused: None,
            pending: BTreeMap::new(),
        })
    }

    /// Adopts a new remote comment or merges its body into the existing stable owner.
    /// Dirty bodies and reply drafts survive refresh. Metadata changes follow successful text admission.
    pub fn merge(
        &mut self,
        edits: &mut EditStore,
        remote: RemoteComment,
    ) -> Result<(CommentId, MergeOutcome)> {
        self.check_owner(edits)?;
        self.validate_remote(&remote)?;
        if let Some(id) = self.remote.get(&remote.identity.node_id).copied() {
            let existing = self.comment.get(&id).expect("indexed remote comment");
            ensure!(
                !existing.deleted && !self.pending.contains_key(&id),
                "comment is deleted or has a pending mutation"
            );
            ensure!(
                existing
                    .remote
                    .as_ref()
                    .expect("remote identity")
                    .database_id
                    == remote.identity.database_id,
                "remote comment identity changed"
            );
            let outcome = edits.merge(&existing.region, remote.body)?;
            let existing = self.comment.get_mut(&id).expect("indexed remote comment");
            existing.remote = Some(remote.identity);
            existing.anchor = remote.anchor;
            existing.viewer_did_author = remote.viewer_did_author;
            return Ok((id, outcome));
        }
        let id = self.insert(
            edits,
            remote.body,
            CommentRecord {
                region: RegionId(String::new()),
                remote: Some(remote.identity),
                anchor: remote.anchor,
                viewer_did_author: remote.viewer_did_author,
                reply_to: None,
                deleted: false,
            },
        )?;
        Ok((
            id,
            MergeOutcome::Updated {
                revision: RegionRevision(0),
            },
        ))
    }

    /// Creates an empty local draft. An anchor binds inline drafts to one exact source revision.
    pub fn draft(
        &mut self,
        edits: &mut EditStore,
        anchor: Option<CommentAnchor>,
    ) -> Result<CommentId> {
        self.check_owner(edits)?;
        if let Some(anchor) = &anchor {
            anchor.validate()?;
        }
        self.insert(
            edits,
            String::new(),
            CommentRecord {
                region: RegionId(String::new()),
                remote: None,
                anchor,
                viewer_did_author: true,
                reply_to: None,
                deleted: false,
            },
        )
    }

    /// Returns the parent's single reply draft. Batched review mode has no inline reply editor.
    pub fn reply_draft(&mut self, edits: &mut EditStore, parent: CommentId) -> Result<CommentId> {
        self.check_owner(edits)?;
        ensure!(
            self.mode == ReviewMode::Overview,
            "inline replies require PR overview mode"
        );
        let record = self.comment.get(&parent).context("unknown reply parent")?;
        ensure!(
            record.remote.is_some() && record.anchor.is_some() && !record.deleted,
            "inline reply requires a remote review comment"
        );
        if let Some(draft) = self.reply.get(&parent) {
            return Ok(*draft);
        }
        let anchor = record.anchor.clone();
        let draft = self.insert(
            edits,
            String::new(),
            CommentRecord {
                region: RegionId(String::new()),
                remote: None,
                anchor,
                viewer_did_author: true,
                reply_to: Some(parent),
                deleted: false,
            },
        )?;
        self.reply.insert(parent, draft);
        Ok(draft)
    }

    /// Accepts native text only for a viewer-authored owner, regardless of its current occurrence.
    pub fn accept(&self, edits: &mut EditStore, edit: RegionEdit) -> Result<EditAcknowledgement> {
        self.check_owner(edits)?;
        let id = self
            .region
            .get(&edit.region)
            .context("unknown comment region")?;
        ensure!(
            self.comment
                .get(id)
                .expect("indexed comment")
                .viewer_did_author,
            "comment is read-only for the current viewer"
        );
        ensure!(!self.record(*id)?.deleted, "comment was deleted remotely");
        Ok(edits.accept(edit)?)
    }

    pub fn contains_region(&self, region: &RegionId) -> bool {
        self.region.contains_key(region)
    }

    pub fn record(&self, id: CommentId) -> Result<&CommentRecord> {
        self.comment.get(&id).context("unknown comment")
    }

    pub fn body<'store>(
        &self,
        edits: &'store EditStore,
        id: CommentId,
    ) -> Result<FieldSnapshot<'store>> {
        self.check_owner(edits)?;
        Ok(edits.snapshot(&self.record(id)?.region)?)
    }

    pub(crate) fn mark_local_inline_deleted(&mut self) -> Result<()> {
        ensure!(
            self.pending.is_empty(),
            "comment outcome requires reconciliation"
        );
        for record in self.comment.values_mut() {
            if record.remote.is_none() && record.reply_to.is_none() && record.anchor.is_some() {
                record.deleted = true;
            }
        }
        Ok(())
    }

    pub fn add_occurrence(
        &mut self,
        comment: CommentId,
        kind: OccurrenceKind,
    ) -> Result<CommentOccurrenceId> {
        self.record(comment)?;
        ensure!(
            self.occurrence.len() < self.limits.occurrences
                && self.next_occurrence < self.limits.occurrence_lifetimes,
            "comment occurrence admission is full"
        );
        self.next_occurrence += 1;
        let id = CommentOccurrenceId(self.next_occurrence);
        self.occurrence
            .insert(id, CommentOccurrence { comment, kind });
        Ok(id)
    }

    /// Focuses exactly one occurrence. Conversation comments require the explicit open action.
    pub fn focus(&mut self, occurrence: CommentOccurrenceId, reason: FocusReason) -> Result<()> {
        let visible = self
            .occurrence
            .get(&occurrence)
            .context("unknown comment occurrence")?;
        if reason == FocusReason::Cursor {
            ensure!(
                visible.kind == OccurrenceKind::Inline
                    && self.record(visible.comment)?.viewer_did_author,
                "comment occurrence requires explicit open"
            );
        }
        self.focused = Some(occurrence);
        Ok(())
    }

    pub fn focused(&self) -> Option<CommentOccurrenceId> {
        self.focused
    }

    pub fn remove_occurrence(&mut self, occurrence: CommentOccurrenceId) -> Result<()> {
        self.occurrence
            .remove(&occurrence)
            .context("unknown comment occurrence")?;
        if self.focused == Some(occurrence) {
            self.focused = None;
        }
        Ok(())
    }

    /// Resolves an explicit semantic target. Adjacent source code never inherits a comment URL.
    pub fn browser_target<'store>(
        &'store self,
        target: &'store CommentActionTarget,
    ) -> Result<Option<CommentBrowserTarget<'store>>> {
        match target {
            CommentActionTarget::Code(anchor) => {
                anchor.validate()?;
                Ok(Some(CommentBrowserTarget::Code(anchor)))
            }
            CommentActionTarget::Comment(occurrence) => {
                let visible = self
                    .occurrence
                    .get(occurrence)
                    .context("unknown comment occurrence")?;
                Ok(self
                    .record(visible.comment)?
                    .remote
                    .as_ref()
                    .map(|remote| CommentBrowserTarget::Comment(remote.url.as_str())))
            }
        }
    }

    pub(crate) fn recover_identity(
        &mut self,
        edits: &mut EditStore,
        comment: CommentId,
        remote: Option<RemoteComment>,
    ) -> Result<()> {
        self.check_owner(edits)?;
        let existing = self.record(comment)?;
        ensure!(
            !self.pending.contains_key(&comment),
            "comment recovery still owns a save capture"
        );
        if let Some(remote) = remote {
            self.validate_remote(&remote)?;
            ensure!(
                remote.viewer_did_author,
                "recovered mutation has another author"
            );
            if let Some(identity) = &existing.remote {
                ensure!(
                    identity.node_id == remote.identity.node_id
                        && identity.database_id == remote.identity.database_id,
                    "recovered mutation has another comment identity"
                );
            }
            ensure!(
                self.remote
                    .get(&remote.identity.node_id)
                    .is_none_or(|owner| *owner == comment),
                "recovered remote identity belongs to another comment"
            );
            edits.restore_confirmed_baseline(&existing.region, remote.body)?;
            self.remote.insert(remote.identity.node_id.clone(), comment);
            let record = self.comment.get_mut(&comment).expect("validated comment");
            record.remote = Some(remote.identity);
            record.viewer_did_author = true;
            if let Some(parent) = record.reply_to.take() {
                self.reply.remove(&parent);
            }
        } else {
            ensure!(
                existing.remote.is_some(),
                "an unknown creation cannot recover as absent"
            );
            self.comment
                .get_mut(&comment)
                .expect("validated comment")
                .deleted = true;
        }
        Ok(())
    }

    pub(crate) fn restore(
        &mut self,
        edits: &mut EditStore,
        identity: CommentId,
        record: CommentRecord,
        baseline: String,
    ) -> Result<()> {
        self.check_owner(edits)?;
        ensure!(
            identity.0 > self.next_comment && identity.0 <= MAX_COUNTER,
            "stored comment identity is duplicated or unordered"
        );
        if let Some(remote) = &record.remote {
            self.validate_remote(&RemoteComment {
                identity: remote.clone(),
                anchor: record.anchor.clone(),
                viewer_did_author: record.viewer_did_author,
                body: baseline.clone(),
            })?;
            ensure!(
                !self.remote.contains_key(&remote.node_id),
                "stored remote comment identity is duplicated"
            );
        } else if let Some(anchor) = &record.anchor {
            anchor.validate()?;
        }
        let reply = record.reply_to;
        if let Some(parent) = reply {
            let parent_record = self
                .comment
                .get(&parent)
                .context("stored reply parent is missing or unordered")?;
            ensure!(
                record.remote.is_none()
                    && parent_record.remote.is_some()
                    && parent_record.anchor.is_some()
                    && parent_record.anchor == record.anchor,
                "stored reply does not match its retained remote parent"
            );
            ensure!(
                record.deleted || !self.reply.contains_key(&parent),
                "stored reply draft owner is duplicated"
            );
        }
        let active_reply = reply.filter(|_| !record.deleted);
        self.next_comment = identity.0 - 1;
        let restored = self.insert(edits, baseline, record)?;
        ensure!(restored == identity, "stored comment identity changed");
        if let Some(parent) = active_reply {
            self.reply.insert(parent, restored);
        }
        Ok(())
    }

    fn insert(
        &mut self,
        edits: &mut EditStore,
        body: String,
        mut record: CommentRecord,
    ) -> Result<CommentId> {
        ensure!(
            self.comment.len() < self.limits.comments && self.next_comment < MAX_COUNTER,
            "comment admission is full"
        );
        let id = CommentId(self.next_comment + 1);
        record.region = RegionId(format!("comment-{}/body", id.0));
        edits.insert(record.region.clone(), RegionRevision(0), body)?;
        self.next_comment = id.0;
        if let Some(remote) = &record.remote {
            self.remote.insert(remote.node_id.clone(), id);
        }
        self.region.insert(record.region.clone(), id);
        self.comment.insert(id, record);
        Ok(id)
    }

    fn check_owner(&self, edits: &EditStore) -> Result<()> {
        ensure!(
            edits.document_id() == &self.document,
            "comment store belongs to a different document"
        );
        Ok(())
    }

    fn validate_remote(&self, remote: &RemoteComment) -> Result<()> {
        let identity = &remote.identity;
        ensure!(
            !identity.node_id.is_empty()
                && identity.node_id.len() <= 256
                && identity.node_id.bytes().all(|byte| byte.is_ascii_graphic())
                && identity.database_id > 0
                && identity.database_id <= MAX_COUNTER,
            "invalid remote comment identity"
        );
        let prefix = format!(
            "https://{}/{}/pull/{}#",
            self.target.repository.hostname(),
            self.target.repository.repository_name(),
            self.target.number
        );
        ensure!(
            identity.url.len() <= 4096
                && identity
                    .url
                    .to_ascii_lowercase()
                    .starts_with(&prefix.to_ascii_lowercase())
                && !identity.url.chars().any(char::is_control),
            "comment URL belongs to another resource"
        );
        if let Some(anchor) = &remote.anchor {
            anchor.validate()?;
        }
        Ok(())
    }
}
