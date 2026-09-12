use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use forge_buffer::editable::EditAcknowledgement;
use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use forge_buffer::{ContractError, MAX_COUNTER};
use serde::{Deserialize, Serialize};

mod budget;
mod merge;
mod restore;
pub use budget::EditBudget;
use budget::StoredText;
pub use merge::PreparedFieldMerge;
pub use restore::{FieldRestoration, SaveRestoration};

/// Per-document cardinality and per-field text bounds. A shared EditBudget bounds allocated text.
#[derive(Clone, Copy, Debug)]
pub struct EditLimits {
    pub active_fields: usize,
    pub region_lifetimes: usize,
    pub pending_saves: usize,
    pub field_bytes: usize,
}

impl Default for EditLimits {
    fn default() -> Self {
        Self {
            active_fields: 256,
            region_lifetimes: 4096,
            pending_saves: 64,
            field_bytes: 256 * 1024,
        }
    }
}

/// Full native field text addressed by its accepted region revision, never a layout revision.
#[derive(Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RegionEdit {
    pub document: DocumentId,
    pub region: RegionId,
    pub base: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
}

/// Field state borrowed from its owner, without allocating a second text snapshot.
#[derive(Debug)]
pub struct FieldSnapshot<'store> {
    pub text: &'store str,
    pub baseline: &'store str,
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub dirty: bool,
    pub pending_saves: usize,
    pub uncertain: bool,
    pub remote: Option<&'store str>,
}

#[derive(Clone, Debug)]
struct EditableField {
    current: Arc<StoredText>,
    baseline: Arc<StoredText>,
    revision: RegionRevision,
    sequence: EditSequence,
    settled_save: u64,
    remote: Option<Arc<StoredText>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MergeOutcome {
    Unchanged,
    Updated { revision: RegionRevision },
    Converged,
    Conflict,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ConflictResolution {
    KeepLocal,
    TakeRemote,
}

/// Immutable submitted text and sequence. Retaining this value retains its text budget charge.
#[derive(Debug)]
pub struct SaveSubmission {
    record: Arc<SaveRecord>,
}

#[derive(Debug)]
struct SaveRecord {
    owner: Arc<()>,
    serial: u64,
    region: RegionId,
    revision: RegionRevision,
    sequence: EditSequence,
    text: Arc<StoredText>,
}

#[derive(Debug)]
struct PendingSave {
    record: Arc<SaveRecord>,
    uncertain: bool,
}

/// Remote evidence for one submitted mutation. Uncertain evidence requires explicit reconciliation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SaveOutcome {
    Confirmed,
    Rejected,
    Uncertain,
}

/// Save settlement does not acknowledge newer local text or advance its region revision.
#[derive(Debug, Eq, PartialEq)]
pub struct SaveCompletion {
    pub dirty: bool,
    pub current_sequence: EditSequence,
    pub superseded: bool,
    pub uncertain: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum EditError {
    Invalid(&'static str),
    Contract(ContractError),
    UnknownRegion,
    Conflict { current: RegionRevision },
    StaleSequence { current: EditSequence },
    Capacity(&'static str),
    AlreadySaving,
    UncertainSave,
    UnknownSave,
    WrongOwner,
    DirtyRegion,
    RemoteConflict,
}

impl std::fmt::Display for EditError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid(message) | Self::Capacity(message) => formatter.write_str(message),
            Self::Contract(error) => error.fmt(formatter),
            other => write!(formatter, "{other:?}"),
        }
    }
}
impl std::error::Error for EditError {}
impl From<ContractError> for EditError {
    fn from(error: ContractError) -> Self {
        Self::Contract(error)
    }
}

/// Owns accepted review text, saved baselines, and bounded save captures for one document lifetime.
///
/// Region identities cannot be reused within this lifetime. Callers execute remote mutations in
/// submission order, though completion delivery can arrive out of order. Layout changes never enter
/// this store. Methods require exclusive access, while immutable submissions can cross threads.
#[derive(Debug)]
pub struct EditStore {
    document: DocumentId,
    owner: Arc<()>,
    limits: EditLimits,
    budget: EditBudget,
    field: BTreeMap<RegionId, EditableField>,
    used_region: BTreeSet<RegionId>,
    pending: BTreeMap<u64, PendingSave>,
    next_save: u64,
}

pub struct PreparedRegionEdit<'store> {
    store: &'store mut EditStore,
    text: Arc<StoredText>,
    acknowledgement: EditAcknowledgement,
}
impl PreparedRegionEdit<'_> {
    pub fn snapshot(&self) -> Result<FieldSnapshot<'_>, EditError> {
        let previous = self.store.snapshot(&self.acknowledgement.region)?;
        let converged = previous.remote == Some(self.text.value.as_ref());
        let baseline = if converged {
            self.text.value.as_ref()
        } else {
            previous.baseline
        };
        Ok(FieldSnapshot {
            text: &self.text.value,
            baseline,
            revision: self.acknowledgement.revision,
            sequence: self.acknowledgement.sequence,
            dirty: self.text.value.as_ref() != baseline,
            pending_saves: previous.pending_saves,
            uncertain: previous.uncertain,
            remote: if converged { None } else { previous.remote },
        })
    }
    pub fn commit(self) -> EditAcknowledgement {
        let field = self
            .store
            .field
            .get_mut(&self.acknowledgement.region)
            .expect("prepared review region");
        field.current = self.text;
        if field
            .remote
            .as_ref()
            .is_some_and(|remote| remote.value == field.current.value)
        {
            field.baseline = Arc::clone(&field.current);
            field.remote = None;
        }
        field.revision = self.acknowledgement.revision;
        field.sequence = self.acknowledgement.sequence;
        self.acknowledgement
    }
}

impl EditStore {
    pub fn document_id(&self) -> &DocumentId {
        &self.document
    }

    /// Validates limits and document identity without creating fields or allocating field text.
    pub fn new(
        document: DocumentId,
        limits: EditLimits,
        budget: EditBudget,
    ) -> Result<Self, EditError> {
        document.validate()?;
        if limits.active_fields == 0
            || limits.region_lifetimes < limits.active_fields
            || limits.pending_saves == 0
            || limits.field_bytes == 0
        {
            return Err(EditError::Invalid("invalid review edit limits"));
        }
        Ok(Self {
            document,
            owner: Arc::new(()),
            limits,
            budget,
            field: BTreeMap::new(),
            used_region: BTreeSet::new(),
            pending: BTreeMap::new(),
            next_save: 0,
        })
    }

    /// Registers a fresh region with one shared current/baseline allocation and sequence zero.
    pub fn insert(
        &mut self,
        region: RegionId,
        revision: RegionRevision,
        text: String,
    ) -> Result<(), EditError> {
        region.validate()?;
        revision.validate()?;
        self.validate_text(&text)?;
        if self.used_region.contains(&region) {
            return Err(EditError::Invalid(
                "review region identity was already used",
            ));
        }
        if self.field.len() >= self.limits.active_fields
            || self.used_region.len() >= self.limits.region_lifetimes
        {
            return Err(EditError::Capacity("review region admission is full"));
        }
        let text = self.budget.retain(text)?;
        self.used_region.insert(region.clone());
        self.field.insert(
            region,
            EditableField {
                current: Arc::clone(&text),
                baseline: text,
                revision,
                sequence: EditSequence(0),
                settled_save: 0,
                remote: None,
            },
        );
        Ok(())
    }

    /// Accepts a newer local sequence against exactly the current region revision.
    /// Invalid, conflicting, stale, or over-budget edits leave every field unchanged.
    pub fn accept(&mut self, edit: RegionEdit) -> Result<EditAcknowledgement, EditError> {
        Ok(self.prepare_accept(edit)?.commit())
    }

    pub fn prepare_accept(
        &mut self,
        edit: RegionEdit,
    ) -> Result<PreparedRegionEdit<'_>, EditError> {
        edit.document.validate()?;
        edit.region.validate()?;
        edit.base.validate()?;
        edit.sequence.validate()?;
        if edit.document != self.document {
            return Err(EditError::WrongOwner);
        }
        if edit.sequence.0 == 0 {
            return Err(EditError::Invalid("local edit sequence must be positive"));
        }
        self.validate_text(&edit.text)?;
        let field = self
            .field
            .get(&edit.region)
            .ok_or(EditError::UnknownRegion)?;
        if edit.sequence <= field.sequence {
            return Err(EditError::StaleSequence {
                current: field.sequence,
            });
        }
        if edit.base != field.revision {
            return Err(EditError::Conflict {
                current: field.revision,
            });
        }
        let revision = field.revision.next()?;
        let text = self.retain_text(field, edit.text)?;
        let acknowledgement = EditAcknowledgement {
            document: self.document.clone(),
            region: edit.region,
            sequence: edit.sequence,
            revision,
        };
        Ok(PreparedRegionEdit {
            store: self,
            text,
            acknowledgement,
        })
    }

    /// Captures current text for one save, or returns None for a clean field with no pending writes.
    /// A local revert still needs a submission when an earlier write is pending. Duplicate captures,
    /// unresolved outcomes, and exhausted admission return errors without losing current text.
    pub fn begin_save(&mut self, region: &RegionId) -> Result<Option<SaveSubmission>, EditError> {
        self.capture(region, false)
    }

    pub(crate) fn capture(
        &mut self,
        region: &RegionId,
        required: bool,
    ) -> Result<Option<SaveSubmission>, EditError> {
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        if field.remote.is_some() {
            return Err(EditError::RemoteConflict);
        }
        let mut has_pending = false;
        for pending in self
            .pending
            .values()
            .filter(|pending| &pending.record.region == region)
        {
            has_pending = true;
            if pending.uncertain {
                return Err(EditError::UncertainSave);
            }
            if pending.record.sequence == field.sequence {
                return Err(EditError::AlreadySaving);
            }
        }
        if !required && !has_pending && field.current.value == field.baseline.value {
            return Ok(None);
        }
        if self.pending.len() >= self.limits.pending_saves {
            return Err(EditError::Capacity("review save admission is full"));
        }
        if self.next_save == MAX_COUNTER {
            return Err(EditError::Capacity("review save identity exhausted"));
        }
        self.next_save += 1;
        let record = Arc::new(SaveRecord {
            owner: Arc::clone(&self.owner),
            serial: self.next_save,
            region: region.clone(),
            revision: field.revision,
            sequence: field.sequence,
            text: Arc::clone(&field.current),
        });
        self.pending.insert(
            record.serial,
            PendingSave {
                record: Arc::clone(&record),
                uncertain: false,
            },
        );
        Ok(Some(SaveSubmission { record }))
    }

    /// Settles one remote result. Confirmed text advances only its submitted baseline.
    /// Older delivery cannot replace a later confirmed baseline. Unknown results retain their
    /// capture and block further saves until reconcile_save observes remote text.
    pub fn complete_save(
        &mut self,
        submission: &SaveSubmission,
        outcome: SaveOutcome,
    ) -> Result<SaveCompletion, EditError> {
        let pending = self.pending_save(submission)?;
        if pending.uncertain {
            return Err(EditError::UncertainSave);
        }
        let record = Arc::clone(&pending.record);
        let field = self
            .field
            .get_mut(&record.region)
            .ok_or(EditError::UnknownRegion)?;
        let superseded = record.serial < field.settled_save;
        let uncertain = outcome == SaveOutcome::Uncertain && !superseded;
        if outcome == SaveOutcome::Confirmed && !superseded {
            field.baseline = Arc::clone(&record.text);
            field.settled_save = record.serial;
        }
        let completion = settlement(field, superseded, uncertain);
        if uncertain {
            self.pending
                .get_mut(&record.serial)
                .expect("pending review save")
                .uncertain = true;
        } else {
            self.pending.remove(&record.serial);
        }
        Ok(completion)
    }

    /// Reconciles an uncertain save from a fresh remote read, preserving current local text.
    /// It never starts a mutation. Failed validation or allocation leaves uncertainty retained.
    pub fn reconcile_save(
        &mut self,
        submission: &SaveSubmission,
        observed: String,
    ) -> Result<SaveCompletion, EditError> {
        self.validate_text(&observed)?;
        let pending = self.pending_save(submission)?;
        if !pending.uncertain {
            return Err(EditError::Invalid("review save has no uncertain outcome"));
        }
        let record = Arc::clone(&pending.record);
        let field = self
            .field
            .get(&record.region)
            .ok_or(EditError::UnknownRegion)?;
        if record.serial < field.settled_save {
            let completion = settlement(field, true, false);
            self.pending.remove(&record.serial);
            return Ok(completion);
        }
        let text = if record.text.value.as_ref() == observed {
            Arc::clone(&record.text)
        } else {
            self.retain_text(field, observed)?
        };
        let field = self
            .field
            .get_mut(&record.region)
            .expect("pending review region");
        field.baseline = text;
        field.remote = None;
        field.settled_save = field.settled_save.max(record.serial);
        let completion = settlement(field, false, false);
        self.pending.remove(&record.serial);
        Ok(completion)
    }

    pub(crate) fn close_unknown(&mut self, submission: &SaveSubmission) -> Result<(), EditError> {
        let pending = self.pending_save(submission)?;
        if !pending.uncertain {
            return Err(EditError::Invalid("review save has no uncertain outcome"));
        }
        self.pending.remove(&submission.record.serial);
        Ok(())
    }

    pub(crate) fn restore_confirmed_baseline(
        &mut self,
        region: &RegionId,
        observed: String,
    ) -> Result<(), EditError> {
        self.validate_text(&observed)?;
        if self
            .pending
            .values()
            .any(|pending| &pending.record.region == region)
        {
            return Err(EditError::AlreadySaving);
        }
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        let text = self.retain_text(field, observed)?;
        let field = self.field.get_mut(region).expect("validated review region");
        field.baseline = text;
        field.remote = None;
        Ok(())
    }

    /// Replaces a clean field from remote truth. Dirty or pending fields return a conflict.
    pub fn refresh(
        &mut self,
        region: &RegionId,
        base: RegionRevision,
        observed: String,
    ) -> Result<RegionRevision, EditError> {
        base.validate()?;
        self.validate_text(&observed)?;
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        if field.revision != base
            || field.remote.is_some()
            || field.current.value != field.baseline.value
            || self
                .pending
                .values()
                .any(|pending| &pending.record.region == region)
        {
            return Err(EditError::Conflict {
                current: field.revision,
            });
        }
        if field.current.value.as_ref() == observed {
            return Ok(field.revision);
        }
        let revision = field.revision.next()?;
        let text = self.budget.retain(observed)?;
        let field = self.field.get_mut(region).expect("validated review region");
        field.current = Arc::clone(&text);
        field.baseline = text;
        field.revision = revision;
        Ok(revision)
    }

    /// Retires only clean regions without pending saves. Its identity remains reserved.
    pub fn remove(&mut self, region: &RegionId) -> Result<(), EditError> {
        let field = self.snapshot(region)?;
        if field.dirty || field.remote.is_some() || field.pending_saves != 0 {
            return Err(EditError::DirtyRegion);
        }
        self.field.remove(region);
        Ok(())
    }

    /// Borrows current text and derives save state without cloning retained field content.
    pub fn snapshot(&self, region: &RegionId) -> Result<FieldSnapshot<'_>, EditError> {
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        let (pending_saves, uncertain) = self
            .pending
            .values()
            .filter(|pending| &pending.record.region == region)
            .fold((0, false), |(count, uncertain), pending| {
                (count + 1, uncertain || pending.uncertain)
            });
        Ok(FieldSnapshot {
            text: &field.current.value,
            baseline: &field.baseline.value,
            revision: field.revision,
            sequence: field.sequence,
            dirty: field.current.value != field.baseline.value,
            pending_saves,
            uncertain,
            remote: field.remote.as_ref().map(|remote| remote.value.as_ref()),
        })
    }

    /// Merges a remote body without replacing a dirty local draft. Conflicting observations share
    /// the text budget and require explicit resolution before another save or region retirement.
    pub fn merge(
        &mut self,
        region: &RegionId,
        observed: String,
    ) -> Result<MergeOutcome, EditError> {
        let prepared = self.prepare_merge(vec![(region.clone(), observed)])?;
        let outcome = *prepared.outcome(region)?;
        prepared.commit();
        Ok(outcome)
    }

    pub fn resolve(
        &mut self,
        region: &RegionId,
        base: RegionRevision,
        choice: ConflictResolution,
    ) -> Result<RegionRevision, EditError> {
        base.validate()?;
        let field = self.field.get(region).ok_or(EditError::UnknownRegion)?;
        if field.revision != base {
            return Err(EditError::Conflict {
                current: field.revision,
            });
        }
        let remote = Arc::clone(
            field
                .remote
                .as_ref()
                .ok_or(EditError::Invalid("review region has no remote conflict"))?,
        );
        let revision = if choice == ConflictResolution::TakeRemote {
            field.revision.next()?
        } else {
            field.revision
        };
        let field = self.field.get_mut(region).expect("validated review region");
        if choice == ConflictResolution::TakeRemote {
            field.current = Arc::clone(&remote);
        }
        field.baseline = remote;
        field.remote = None;
        field.revision = revision;
        Ok(revision)
    }

    fn validate_text(&self, text: &str) -> Result<(), EditError> {
        if text.len() > self.limits.field_bytes || text.contains('\0') {
            return Err(EditError::Invalid("review field exceeds its text contract"));
        }
        Ok(())
    }

    fn retain_text(
        &self,
        field: &EditableField,
        text: String,
    ) -> Result<Arc<StoredText>, EditError> {
        if field.current.value.as_ref() == text {
            Ok(Arc::clone(&field.current))
        } else if field.baseline.value.as_ref() == text {
            Ok(Arc::clone(&field.baseline))
        } else if let Some(remote) = &field.remote
            && remote.value.as_ref() == text
        {
            Ok(Arc::clone(remote))
        } else {
            self.budget.retain(text)
        }
    }

    fn pending_save(&self, submission: &SaveSubmission) -> Result<&PendingSave, EditError> {
        if !Arc::ptr_eq(&self.owner, &submission.record.owner) {
            return Err(EditError::WrongOwner);
        }
        self.pending
            .get(&submission.record.serial)
            .ok_or(EditError::UnknownSave)
    }
}

impl SaveSubmission {
    pub fn text(&self) -> &str {
        &self.record.text.value
    }
    pub fn region(&self) -> &RegionId {
        &self.record.region
    }
    pub fn revision(&self) -> RegionRevision {
        self.record.revision
    }
    pub fn sequence(&self) -> EditSequence {
        self.record.sequence
    }
}

fn settlement(field: &EditableField, superseded: bool, uncertain: bool) -> SaveCompletion {
    SaveCompletion {
        dirty: field.current.value != field.baseline.value,
        current_sequence: field.sequence,
        superseded,
        uncertain,
    }
}
