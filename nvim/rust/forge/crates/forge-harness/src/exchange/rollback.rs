//! Capture immutable rollback authority before the FIFO changes session history.
use crate::checkpoint::{RestoreJournal, RestorePreview};
use crate::exchange::{Exchange, HistoryDisposition};
use serde::{Deserialize, Serialize};

/// Name the native history position that matches a set of retained exchanges.
pub(crate) enum NativeBoundary {
    /// The newest retained provider turn reported this checkpoint.
    Checkpoint(String),
    /// A retained exchange ran provider turns and none of them reported a checkpoint.
    Unrecorded {
        /// The exchange whose turns have no checkpoint.
        ordinal: u64,
    },
    /// No retained exchange ran a provider turn.
    Empty,
}

impl NativeBoundary {
    /// Find the boundary after the newest retained exchange that ran a provider turn.
    ///
    /// Conversation rollback and forking select their native position by this one rule.
    pub(crate) fn after<'exchange>(
        retained: impl DoubleEndedIterator<Item = &'exchange Exchange>,
    ) -> Self {
        for exchange in retained.rev() {
            if let Some(checkpoint) = exchange.native_checkpoint() {
                return Self::Checkpoint(checkpoint.to_owned());
            }
            if !exchange.turn.is_empty() {
                return Self::Unrecorded {
                    ordinal: exchange.ordinal,
                };
            }
        }
        Self::Empty
    }
}

/// Describe the provider facts that decide whether its conversation can be rewound.
pub(crate) struct RewindProvider<'session> {
    pub(crate) label: &'session str,
    pub(crate) capability: &'session crate::provider::ProviderCapability,
    /// Whether the provider holds a conversation for this session at all.
    pub(crate) native_history: bool,
}

/// Select where a conversation rollback to `first_removed` truncates native history.
///
/// `Ok(None)` means the provider holds no conversation yet, so nothing needs rewinding, and
/// `Ok(Some(None))` empties the provider's conversation. Every refusal is known before any file or
/// history changes, so the interaction list reports it on the option and preparation rejects it.
pub(crate) fn rewind_target(
    exchanges: &[Exchange],
    first_removed: usize,
    provider: &RewindProvider<'_>,
) -> Result<Option<Option<String>>, RollbackFailure> {
    if !provider.capability.native_conversation_rewind {
        return Err(RollbackFailure::Unsupported {
            reason: format!(
                "Conversation rollback is unsupported: {} cannot rewind its conversation",
                provider.label
            ),
        });
    }
    if !provider.native_history {
        return Ok(None);
    }
    let retained = exchanges[..first_removed]
        .iter()
        .filter(|exchange| exchange.disposition() == HistoryDisposition::Current);
    match NativeBoundary::after(retained) {
        NativeBoundary::Checkpoint(checkpoint) => Ok(Some(Some(checkpoint))),
        NativeBoundary::Unrecorded { ordinal } => Err(RollbackFailure::Unsupported {
            reason: format!(
                "Conversation rollback is unsupported here: interaction {ordinal} has no recorded provider checkpoint"
            ),
        }),
        NativeBoundary::Empty if provider.capability.conversation_rewind_requires_checkpoint => {
            Err(RollbackFailure::Unsupported {
                reason: format!(
                    "Conversation rollback is unsupported here: {} can rewind only to the end of an earlier interaction",
                    provider.label
                ),
            })
        }
        NativeBoundary::Empty => Ok(Some(None)),
    }
}

/// Decide whether a task survives the rollback of `removed` exchanges.
///
/// A task's start opens the first exchange created after it, so the task belongs to the retained
/// history exactly when a retained exchange was created at or after the task. Rolling back an
/// interaction inside a goal keeps the goal; rolling back its start removes it.
pub(crate) fn rollback_keeps_task(
    exchanges: &[Exchange],
    removed: &[String],
    task_created_at_ms: i64,
) -> bool {
    exchanges.iter().any(|exchange| {
        exchange.disposition() == HistoryDisposition::Current
            && !removed.contains(&exchange.id)
            && exchange.created_at_ms >= task_created_at_ms
    })
}

/// Present one exchange together with the reason a rollback to it is unavailable.
#[derive(Serialize)]
pub(crate) struct ExchangeRollbackReview {
    #[serde(flatten)]
    pub(crate) exchange: Exchange,
    /// Why rolling back to this exchange cannot rewind the provider; absent when it can.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) rollback_unavailable: Option<String>,
}

impl ExchangeRollbackReview {
    /// Annotate every current, finished exchange of one ordered history with its rollback refusal.
    pub(crate) fn list(exchanges: Vec<Exchange>, provider: &RewindProvider<'_>) -> Vec<Self> {
        let unavailable = (0..exchanges.len())
            .map(|position| {
                let exchange = &exchanges[position];
                (exchange.is_finished() && exchange.disposition() == HistoryDisposition::Current)
                    .then(|| rewind_target(&exchanges, position, provider).err())
                    .flatten()
                    .map(|failure| failure.to_string())
            })
            .collect::<Vec<_>>();
        exchanges
            .into_iter()
            .zip(unavailable)
            .map(|(exchange, rollback_unavailable)| Self {
                exchange,
                rollback_unavailable,
            })
            .collect()
    }
}

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
    ///
    /// A files preview compares the workspace with the retained history's closing checkpoint, which
    /// a running or paused exchange has not written yet, so preparation collects active execution
    /// first, as applying does. Undoing a restore reads only its own journal.
    pub(crate) fn requires_collection(&self) -> bool {
        matches!(
            self,
            Self::Prepare(_) | Self::Apply(_) | Self::ResumeRestore(_)
        )
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
    /// Retains the restore worker's cause, which is otherwise invisible because a
    /// failed apply leaves only the journal state behind.
    #[error("restoration did not complete ({cause}); inspect retained recovery status")]
    RestoreIncomplete { cause: String },
    /// The provider cannot rewind this conversation, so nothing changed.
    #[error("{reason}")]
    Unsupported { reason: String },
    /// Native rewind failed before host history changed; repeating the rollback is safe.
    #[error("{cause}; undo again to retry")]
    RewindFailed { cause: String },
}
impl RollbackFailure {
    /// Get a stable transport failure code.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::StaleAuthority => "rollback_stale_authority",
            Self::JournalMissing => "restore_journal_missing",
            Self::AuthorityUnavailable => "rollback_authority_unavailable",
            Self::PreparationFailed => "rollback_preparation_failed",
            Self::RecoveryRequired | Self::RestoreIncomplete { .. } => "restore_recovery_required",
            Self::Unsupported { .. } => "rollback_unsupported",
            Self::RewindFailed { .. } => "rollback_rewind_failed",
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

    /// A provider that rewinds only to a recorded checkpoint cannot roll back the first
    /// interaction, and the exchange list names that refusal on the first exchange alone, before
    /// any preparation or confirmation.
    #[test]
    fn checkpoint_only_rewind_refuses_the_first_interaction() {
        let exchanges = vec![
            finished_exchange("first", 1),
            finished_exchange("second", 2),
        ];
        let capability = crate::provider::ProviderCapability {
            native_conversation_rewind: true,
            conversation_rewind_requires_checkpoint: true,
            ..Default::default()
        };
        let provider = RewindProvider {
            label: "Claude Code",
            capability: &capability,
            native_history: true,
        };
        assert!(matches!(
            rewind_target(&exchanges, 1, &provider),
            Ok(Some(Some(checkpoint))) if checkpoint == "checkpoint-first"
        ));
        let refusal = rewind_target(&exchanges, 0, &provider).unwrap_err();
        assert_eq!(refusal.code(), "rollback_unsupported");
        let review = ExchangeRollbackReview::list(exchanges, &provider);
        assert_eq!(review[0].rollback_unavailable, Some(refusal.to_string()));
        assert_eq!(review[1].rollback_unavailable, None);

        let rewinds_to_start = crate::provider::ProviderCapability {
            conversation_rewind_requires_checkpoint: false,
            ..capability.clone()
        };
        let provider = RewindProvider {
            capability: &rewinds_to_start,
            ..provider
        };
        let exchanges = vec![finished_exchange("first", 1)];
        assert!(matches!(
            rewind_target(&exchanges, 0, &provider),
            Ok(Some(None))
        ));
    }

    /// Build a finished primary exchange whose provider turn reported `checkpoint-<id>`.
    fn finished_exchange(id: &str, ordinal: u64) -> Exchange {
        let delegation = crate::agent::Delegation {
            id: format!("delegation-{id}"),
            parent_exchange_id: String::new(),
            parent_turn_id: None,
            child_agent_id: crate::session::HarnessSession::primary_agent_id("session"),
            child_exchange_id: id.into(),
            task: format!("prompt {id}"),
            created_at_ms: ordinal as i64,
        };
        let mut exchange = Exchange::delegated("session", &delegation, ordinal);
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            crate::exchange::ExchangeState::Complete,
            Some(ordinal as i64),
            None,
        );
        exchange.provider_checkpoint_id = Some(format!("checkpoint-{id}"));
        exchange
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
