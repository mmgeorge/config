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
    /// When the newest compaction recorded as a session row, outside every exchange, started.
    ///
    /// A compaction that stopped a running exchange is a row of that exchange instead, so the
    /// exchange list carries it.
    pub(crate) newest_session_compaction_ms: Option<i64>,
}

impl RewindProvider<'_> {
    /// Find when the newest session-row compaction that may have replaced native history started.
    ///
    /// A failed compaction left the history in place. An unconfirmed one may have replaced it,
    /// and offering a rollback the provider then refuses is the failure this rule prevents.
    pub(crate) fn newest_session_compaction(
        events: &[crate::timeline::SessionEventRecord],
    ) -> Option<i64> {
        events
            .iter()
            .filter_map(|event| match &event.detail {
                crate::timeline::SessionEventKind::Compaction { compaction }
                    if replaced_history(compaction) =>
                {
                    Some(compaction.started_at_ms.unwrap_or(event.created_at_ms))
                }
                _ => None,
            })
            .max()
    }
}

/// Report whether a compaction may have replaced the provider's native history.
fn replaced_history(compaction: &crate::provider::Compaction) -> bool {
    matches!(
        compaction.state,
        crate::provider::CompactionState::Completed | crate::provider::CompactionState::Unconfirmed
    )
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
    let retained = || {
        exchanges[..first_removed]
            .iter()
            .filter(|exchange| exchange.disposition() == HistoryDisposition::Current)
    };
    match NativeBoundary::after(retained()) {
        NativeBoundary::Checkpoint(_)
            if provider.capability.conversation_rewind_stops_at_compaction
                && retained()
                    .rev()
                    .find(|exchange| exchange.native_checkpoint().is_some())
                    .is_some_and(|kept| compacted_after(exchanges, kept, provider)) =>
        {
            Err(RollbackFailure::Unsupported {
                reason: format!(
                    "Conversation rollback is unsupported here: {} cannot rewind to before its context was compacted",
                    provider.label
                ),
            })
        }
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

/// Report whether a compaction may have replaced the native history that ends at `kept`'s
/// checkpoint.
///
/// The checkpoint is the end of `kept`'s newest checkpointed turn, so a compaction that started
/// after that turn ended replaced it: an idle `/compact`, an automatic compaction at the turn
/// boundary, or one inside any later exchange. A compaction that stopped `kept` itself and after
/// which a turn ran again precedes the checkpoint and leaves it resumable. Every exchange counts
/// whatever its disposition, because a Harness rollback does not undo a compaction.
fn compacted_after(exchanges: &[Exchange], kept: &Exchange, provider: &RewindProvider<'_>) -> bool {
    let checkpoint_recorded_at_ms = kept
        .turn
        .iter()
        .rev()
        .find(|turn| {
            turn.completed()
                .is_some_and(|completed| completed.result().checkpoint_id().is_some())
        })
        .or(kept.turn.last())
        .and_then(crate::turn::Turn::completed_at_ms)
        .or_else(|| kept.completed_at_ms())
        .unwrap_or(kept.created_at_ms);
    let root_thread = crate::provider::ThreadOwner::root(&kept.session_id)
        .thread_id()
        .to_string();
    let compacted_in_exchange = exchanges.iter().any(|exchange| {
        exchange.node_list.iter().any(|node| match node {
            crate::exchange::ExchangeNode::Compaction { compaction, .. }
                if compaction.thread_id == root_thread && replaced_history(compaction) =>
            {
                match compaction.started_at_ms.or(compaction.completed_at_ms) {
                    Some(started_at_ms) => started_at_ms >= checkpoint_recorded_at_ms,
                    None => exchange.ordinal > kept.ordinal,
                }
            }
            _ => false,
        })
    });
    compacted_in_exchange
        || provider
            .newest_session_compaction_ms
            .is_some_and(|started_at_ms| started_at_ms >= checkpoint_recorded_at_ms)
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
    /// Active exchange time at the time of the review.
    elapsed_ms: u64,
    /// Net workspace changes from the exchange's completed checkpoint comparison.
    #[serde(skip_serializing_if = "Option::is_none")]
    edits: Option<forge_diff::patch::PatchStats>,
    /// Why rolling back to this exchange cannot rewind the provider; absent when it can.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) rollback_unavailable: Option<String>,
    /// The command that started this exchange's task, which a rollback of the start returns to
    /// the composer; absent for every other exchange.
    ///
    /// A task start's prompt is the notice Harness wrote, so the composer receives the command
    /// the user typed instead: `/goal <objective>`, whose objective is the goal's title, or
    /// `/plan <request>`, whose request the plan record keeps because the title holds only its
    /// first line. An execution start follows an accepted plan, which no command recreates.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) task_command: Option<String>,
    /// Whether the exchange is parked: its state stays `running` in history while a paused task
    /// holds it open with no turn running, so the list names it paused instead.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub(crate) paused: bool,
}

impl ExchangeRollbackReview {
    /// Annotate every current, finished exchange of one ordered history with its rollback refusal
    /// and, for a task start, the command a rollback returns; `plan_list` is the session's plans.
    pub(crate) fn list(
        now_ms: i64,
        exchanges: Vec<Exchange>,
        plan_list: &[crate::plan::PlanRecord],
        provider: &RewindProvider<'_>,
    ) -> Vec<Self> {
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
                elapsed_ms: exchange.elapsed(now_ms),
                edits: captured_edit_stats(&exchange),
                task_command: task_command(&exchange, plan_list),
                paused: exchange.is_parked(),
                exchange,
                rollback_unavailable,
            })
            .collect()
    }
}

/// Summarize the same display comparison as the timeline's Edits fold.
fn captured_edit_stats(exchange: &Exchange) -> Option<forge_diff::patch::PatchStats> {
    let super::ExchangeChangeResult::Captured(comparison) = exchange.change_result() else {
        return None;
    };
    let patch = forge_diff::patch::UnifiedPatch::parse(comparison.text()).ok()?;
    Some(patch.display().ok()?.stats())
}

/// Name the command that started a task start's task, as the user typed it.
///
/// A plan start opens right after its plan is created, so its plan is the newest one created at or
/// before the exchange.
fn task_command(exchange: &Exchange, plan_list: &[crate::plan::PlanRecord]) -> Option<String> {
    let crate::exchange::ExchangeOpening::TaskStarted { start } = &exchange.opening else {
        return None;
    };
    match start.kind {
        crate::task::TaskKind::Goal => {
            (!start.title.is_empty()).then(|| format!("/goal {}", start.title))
        }
        crate::task::TaskKind::Plan => plan_list
            .iter()
            .filter(|plan| plan.created_at_ms <= exchange.created_at_ms)
            .max_by_key(|plan| plan.created_at_ms)
            .filter(|plan| !plan.request.trim().is_empty())
            .map(|plan| format!("/plan {}", plan.request)),
        crate::task::TaskKind::Execute => None,
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
    /// The files reached the rollback target but the provider conversation did not, so the
    /// journal stays incomplete; the message names both ways out of that state.
    #[error(
        "the files were restored, but the conversation was not rolled back ({cause}); open /undo and choose Continue interrupted restore to retry the conversation, or Restore pre-restore files to return the files to their state before the rollback"
    )]
    ConversationNotRewound { cause: String },
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
            Self::RecoveryRequired
            | Self::RestoreIncomplete { .. }
            | Self::ConversationNotRewound { .. } => "restore_recovery_required",
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

    #[test]
    fn edit_stats_distinguish_changed_empty_and_missing_comparisons() {
        use forge_diff::patch::PatchStats;

        let mut exchange = finished_exchange("edited", 1);
        assert_eq!(captured_edit_stats(&exchange), None);

        exchange.checkpoint_before = Some("before".into());
        exchange.checkpoint_after = Some("after".into());
        exchange.checkpoint_diff_text = Some(String::new());
        assert_eq!(
            captured_edit_stats(&exchange),
            Some(PatchStats {
                file_count: 0,
                additions: 0,
                deletions: 0,
            })
        );

        exchange.checkpoint_diff_text = Some(
            concat!(
                "diff --git a/manifest.json b/manifest.json\n",
                "--- a/manifest.json\n+++ b/manifest.json\n",
                "@@ -1,2 +1,3 @@\n-old\n+new\n+extra\n context\n",
                "diff --git a/main.js b/main.js\n",
                "old mode 100644\nnew mode 100755\n",
            )
            .into(),
        );
        assert_eq!(
            captured_edit_stats(&exchange),
            Some(PatchStats {
                file_count: 2,
                additions: 2,
                deletions: 1,
            })
        );
    }

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
            newest_session_compaction_ms: None,
        };
        assert!(matches!(
            rewind_target(&exchanges, 1, &provider),
            Ok(Some(Some(checkpoint))) if checkpoint == "checkpoint-first"
        ));
        let refusal = rewind_target(&exchanges, 0, &provider).unwrap_err();
        assert_eq!(refusal.code(), "rollback_unsupported");
        let review = ExchangeRollbackReview::list(0, exchanges, &[], &provider);
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

    /// A provider whose compaction replaces the native history before it cannot resume at an
    /// earlier checkpoint, so the exchange list marks every rollback that would keep only
    /// pre-compaction history unavailable before confirmation, and preparation refuses it.
    /// A compaction that stopped an exchange which then ran again precedes that exchange's
    /// checkpoint, so rolling back to just after that exchange stays available.
    #[test]
    fn rewind_never_targets_history_a_compaction_replaced() {
        let completed_at = |id: &str, ordinal: u64, completed_at_ms: i64| {
            let mut exchange = finished_exchange(id, ordinal);
            exchange.phase = crate::exchange::ExchangePhase::fixture(
                crate::exchange::ExchangeState::Complete,
                Some(completed_at_ms),
                None,
            );
            exchange
        };
        let mut compacted = completed_at("third", 3, 30);
        compacted.observe_compaction(
            crate::provider::Compaction {
                id: "mid-turn".into(),
                thread_id: crate::provider::ThreadOwner::root("session")
                    .thread_id()
                    .to_string(),
                state: crate::provider::CompactionState::Completed,
                manual: true,
                started_at_ms: Some(25),
                completed_at_ms: Some(27),
                result: Default::default(),
            },
            25,
        );
        let exchanges = vec![
            completed_at("first", 1, 10),
            completed_at("second", 2, 20),
            compacted,
            completed_at("fourth", 4, 40),
        ];
        let capability = crate::provider::ProviderCapability {
            native_conversation_rewind: true,
            conversation_rewind_requires_checkpoint: true,
            conversation_rewind_stops_at_compaction: true,
            ..Default::default()
        };
        let provider = RewindProvider {
            label: "Claude Code",
            capability: &capability,
            native_history: true,
            newest_session_compaction_ms: None,
        };
        let refusal = rewind_target(&exchanges, 2, &provider).unwrap_err();
        assert_eq!(refusal.code(), "rollback_unsupported");
        assert!(refusal.to_string().contains("compacted"), "{refusal}");
        assert!(matches!(
            rewind_target(&exchanges, 3, &provider),
            Ok(Some(Some(checkpoint))) if checkpoint == "checkpoint-third"
        ));
        let review = ExchangeRollbackReview::list(0, exchanges.clone(), &[], &provider);
        let unavailable = review
            .iter()
            .map(|entry| entry.rollback_unavailable.is_some())
            .collect::<Vec<_>>();
        assert_eq!(unavailable, [true, true, true, false]);
        assert_eq!(review[2].rollback_unavailable, Some(refusal.to_string()));

        // A compaction recorded as a session row after the last exchange replaced it as well.
        let idle = RewindProvider {
            newest_session_compaction_ms: Some(45),
            ..provider
        };
        assert_eq!(
            rewind_target(&exchanges, 3, &idle).unwrap_err().to_string(),
            refusal.to_string()
        );

        // A provider that rewinds across a compaction offers every target after the first.
        let crosses = crate::provider::ProviderCapability {
            conversation_rewind_stops_at_compaction: false,
            ..capability.clone()
        };
        let crosses = RewindProvider {
            capability: &crosses,
            ..idle
        };
        let review = ExchangeRollbackReview::list(0, exchanges, &[], &crosses);
        assert_eq!(
            review
                .iter()
                .map(|entry| entry.rollback_unavailable.is_some())
                .collect::<Vec<_>>(),
            [true, false, false, false]
        );
    }

    /// Only a session-row compaction that may have replaced native history bounds a rewind.
    #[test]
    fn newest_session_compaction_ignores_failed_attempts() {
        let event = |id: &str, state: crate::provider::CompactionState, started_at_ms: i64| {
            crate::timeline::SessionEventRecord {
                id: id.into(),
                session_id: "session".into(),
                owner: crate::timeline::TimelineOwner::session("session"),
                created_at_ms: started_at_ms,
                detail: crate::timeline::SessionEventKind::Compaction {
                    compaction: Box::new(crate::provider::Compaction {
                        id: id.into(),
                        thread_id: crate::provider::ThreadOwner::root("session")
                            .thread_id()
                            .to_string(),
                        state,
                        manual: true,
                        started_at_ms: Some(started_at_ms),
                        completed_at_ms: None,
                        result: Default::default(),
                    }),
                },
            }
        };
        let events = vec![
            event("completed", crate::provider::CompactionState::Completed, 10),
            event(
                "unconfirmed",
                crate::provider::CompactionState::Unconfirmed,
                20,
            ),
            event("failed", crate::provider::CompactionState::Failed, 30),
        ];
        assert_eq!(RewindProvider::newest_session_compaction(&events), Some(20));
        assert_eq!(
            RewindProvider::newest_session_compaction(&events[2..]),
            None
        );
    }

    /// Rolling back a task start names the command that started it, so the composer receives
    /// `/goal <objective>` or `/plan <request>`; a plan's title is only the request's first line,
    /// so the request comes from the plan the start opened.
    #[test]
    fn task_starts_name_the_command_a_rollback_returns() {
        let task_start = |id: &str, ordinal: u64, kind: crate::task::TaskKind, title: &str| {
            let mut exchange = finished_exchange(id, ordinal);
            exchange.created_at_ms = ordinal as i64 * 10;
            exchange.opening = crate::exchange::ExchangeOpening::TaskStarted {
                start: crate::exchange::TaskStart {
                    kind,
                    title: title.into(),
                },
            };
            exchange
        };
        let plan = |id: &str, request: &str, created_at_ms: i64| -> crate::plan::PlanRecord {
            serde_json::from_value(serde_json::json!({
                "id": id, "session_id": "session", "request": request, "state": "cancelled",
                "working_path": "plan.json", "model_revision": 0, "user_revision": 0,
                "created_at_ms": created_at_ms, "updated_at_ms": created_at_ms,
            }))
            .unwrap()
        };
        let exchanges = vec![
            finished_exchange("prompt", 1),
            task_start("goal", 2, crate::task::TaskKind::Goal, "Port the parser"),
            task_start("plan", 3, crate::task::TaskKind::Plan, "Add moneyHalve"),
            task_start(
                "execution",
                4,
                crate::task::TaskKind::Execute,
                "Add moneyHalve",
            ),
        ];
        let plan_list = vec![
            plan("earlier", "Add moneyAllocate", 5),
            plan(
                "current",
                "Add moneyHalve
Keep the rounding rule.",
                29,
            ),
        ];
        let capability = crate::provider::ProviderCapability {
            native_conversation_rewind: true,
            ..Default::default()
        };
        let provider = RewindProvider {
            label: "Codex",
            capability: &capability,
            native_history: true,
            newest_session_compaction_ms: None,
        };
        let review = ExchangeRollbackReview::list(0, exchanges, &plan_list, &provider);
        assert_eq!(
            review
                .iter()
                .map(|entry| entry.task_command.as_deref())
                .collect::<Vec<_>>(),
            [
                None,
                Some("/goal Port the parser"),
                Some(
                    "/plan Add moneyHalve
Keep the rounding rule."
                ),
                None,
            ]
        );
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
