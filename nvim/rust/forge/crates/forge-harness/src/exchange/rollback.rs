//! Capture immutable rollback authority before the FIFO changes session history.
use crate::checkpoint::{RestoreJournal, RestorePreview};
use serde::{Deserialize, Serialize};

/// Select a checkpoint and the history observed by the caller.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RollbackSelection {
    pub(crate) exchange_id: String,
    pub(crate) latest_exchange_id: String,
    #[serde(default)]
    pub(super) history_digest: Option<String>,
}
impl RollbackSelection {
    /// Create caller selection whose complete history authority is captured at admission.
    pub(crate) fn new(exchange_id: String, latest_exchange_id: String) -> Self {
        Self {
            exchange_id,
            latest_exchange_id,
            history_digest: None,
        }
    }
    /// Bind the caller selection to one observed history snapshot.
    pub(crate) fn capture_history(
        &mut self,
        history: &[crate::exchange::Exchange],
    ) -> Result<(), RollbackFailure> {
        let authority: Vec<_> = history
            .iter()
            .map(|exchange| {
                (
                    &exchange.id,
                    exchange.disposition(),
                    &exchange.checkpoint_before,
                )
            })
            .collect();
        let observed = digest(&authority)?;
        if self
            .history_digest
            .as_ref()
            .is_some_and(|expected| expected != &observed)
        {
            return Err(RollbackFailure::StaleAuthority);
        }
        self.history_digest = Some(observed);
        Ok(())
    }
}
/// Bind application to an exact retained preview.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub(crate) enum PreparedRollbackToken {
    Workspace {
        preview_id: String,
        digest: String,
        recovery: Option<RestoreRevision>,
    },
    Conversation {
        selection: RollbackSelection,
    },
}
impl PreparedRollbackToken {
    /// Capture all preview contents.
    pub(crate) fn capture(preview: &RestorePreview) -> Result<Self, RollbackFailure> {
        Ok(Self::Workspace {
            preview_id: preview.id.clone(),
            digest: digest(preview)?,
            recovery: None,
        })
    }
    /// Reject replacement after admission.
    pub(crate) fn validate(&self, preview: &RestorePreview) -> Result<(), RollbackFailure> {
        match self {
            Self::Workspace {
                preview_id,
                digest: expected,
                ..
            } if preview_id == &preview.id && expected == &digest(preview)? => Ok(()),
            Self::Workspace { .. } | Self::Conversation { .. } => {
                Err(RollbackFailure::StaleAuthority)
            }
        }
    }
}
/// Bind recovery to its journal and preview.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RestoreRevision {
    pub(crate) preview_id: String,
    pub(crate) digest: String,
}
impl RestoreRevision {
    /// Capture progress without authorizing new restoration.
    pub(crate) fn capture(journal: &RestoreJournal) -> Result<Self, RollbackFailure> {
        Ok(Self {
            preview_id: journal.preview.id.clone(),
            digest: digest(journal)?,
        })
    }
    /// Reject changed progress or file authority.
    pub(crate) fn validate(&self, journal: &RestoreJournal) -> Result<(), RollbackFailure> {
        if self.preview_id != journal.preview.id || self.digest != digest(journal)? {
            return Err(RollbackFailure::StaleAuthority);
        }
        Ok(())
    }
}
/// Separate preparation from authorized restoration.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action", content = "selection", rename_all = "snake_case")]
pub(crate) enum RollbackCommand {
    Prepare(RollbackSelection),
    Apply(PreparedRollbackToken),
    ResumeRestore(RestoreRevision),
    PrepareUndoRestore(RestoreRevision),
}
impl RollbackCommand {
    /// Get a stable trace name.
    pub(crate) fn name(&self) -> &'static str {
        match self {
            Self::Prepare(_) => "rollback_prepare",
            Self::Apply(_) => "rollback_apply",
            Self::ResumeRestore(_) => "rollback_resume_restore",
            Self::PrepareUndoRestore(_) => "rollback_prepare_undo_restore",
        }
    }
    /// Identify commands requiring execution collection.
    pub(crate) fn requires_collection(&self) -> bool {
        matches!(self, Self::Apply(_) | Self::ResumeRestore(_))
    }
}
/// Preserve preparation and restoration postconditions.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "disposition", rename_all = "snake_case")]
pub(crate) enum RollbackOutcome {
    Prepared {
        token: PreparedRollbackToken,
        files: usize,
        warnings: Vec<String>,
    },
    Restored,
}
/// Preserve recovery decisions independently of display text.
#[derive(Clone, Debug, Deserialize, Serialize, thiserror::Error)]
pub(crate) enum RollbackFailure {
    #[error("rollback authority changed; prepare again")]
    StaleAuthority,
    #[error("restore journal is missing")]
    JournalMissing,
    #[error("rollback authority could not be read or encoded")]
    AuthorityUnavailable,
    #[error("rollback preparation failed")]
    PreparationFailed,
    #[error("restoration did not complete; inspect retained recovery status")]
    RecoveryRequired,
}
impl RollbackFailure {
    /// Get a stable transport failure code.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::StaleAuthority => "rollback_stale_authority",
            Self::JournalMissing => "restore_journal_missing",
            Self::AuthorityUnavailable => "rollback_authority_unavailable",
            Self::PreparationFailed => "rollback_preparation_failed",
            Self::RecoveryRequired => "restore_recovery_required",
        }
    }
}
/// Fingerprint every retained authority field.
fn digest(value: &impl Serialize) -> Result<String, RollbackFailure> {
    serde_json::to_vec(value)
        .map(|bytes| crate::plan::digest(&bytes))
        .map_err(|_| RollbackFailure::AuthorityUnavailable)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reject replacement even when the persisted preview identity is unchanged.
    #[test]
    fn preview_token_rejects_changed_history() {
        let mut preview = preview();
        let token = PreparedRollbackToken::capture(&preview).unwrap();
        token.validate(&preview).unwrap();
        preview.exchange_ids.push("later".into());
        assert!(matches!(
            token.validate(&preview),
            Err(RollbackFailure::StaleAuthority)
        ));
    }

    /// Bind recovery to progress as well as the selected preview.
    #[test]
    fn recovery_revision_rejects_changed_progress() {
        let mut journal = RestoreJournal {
            preview: preview(),
            applied: 0,
            state: crate::checkpoint::RestoreState::Applying,
        };
        let revision = RestoreRevision::capture(&journal).unwrap();
        journal.state = crate::checkpoint::RestoreState::FilesComplete;
        assert!(matches!(
            revision.validate(&journal),
            Err(RollbackFailure::StaleAuthority)
        ));
    }

    /// Create file-free authority for deterministic comparison.
    fn preview() -> RestorePreview {
        RestorePreview {
            id: "preview".into(),
            session_id: "session".into(),
            workspace: "workspace".into(),
            head: "head".into(),
            path: Vec::new(),
            warning: Vec::new(),
            exchange_ids: vec!["exchange".into()],
            recovery_of: None,
        }
    }
}
