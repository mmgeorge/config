//! Run retained rollback work without holding the exchange broker lock.
use super::*;
use crate::checkpoint::{CheckpointRecord, RestoreJournal, RestorePreview};
use crate::exchange::rollback::{
    PreparedRollbackToken, RollbackCommand, RollbackFailure, RollbackOutcome,
};

/// Own immutable restore inputs and native teardown across asynchronous work.
pub(crate) struct RollbackEffect {
    objects: crate::storage::objects::ObjectStore,
    repositories: Arc<forge_git::store::RepositoryStore>,
    work: RollbackWork,
    teardown: Option<foreground_undo::UndoTeardown>,
}
struct RollbackPreparation {
    workspace: std::path::PathBuf,
    expected: CheckpointRecord,
    target: CheckpointRecord,
    exchanges: Vec<String>,
}

enum RollbackWork {
    Prepare(Box<RollbackPreparation>),
    Restore {
        preview: RestorePreview,
        resume: bool,
    },
    Reverse(RestoreJournal),
    Conversation,
}
/// Carry settled work back to the sole owner for its synchronous commit.
pub(crate) struct RollbackEffectResult {
    prepared: Option<RestorePreview>,
    journal: Option<RestoreJournal>,
    teardown: Option<foreground_undo::UndoTeardown>,
    authority: Option<crate::exchange::rollback::RestoreRevision>,
    failure: Option<RollbackFailure>,
}
impl RollbackEffect {
    /// Collect native resources and execute serialized repository work without broker ownership.
    pub(crate) async fn run(
        mut self,
    ) -> std::result::Result<RollbackEffectResult, RollbackFailure> {
        if let Some(teardown) = &mut self.teardown
            && teardown.run().await.is_err()
        {
            return Ok(RollbackEffectResult {
                prepared: None,
                journal: None,
                teardown: self.teardown,
                authority: None,
                failure: Some(RollbackFailure::RecoveryRequired),
            });
        }
        let authority = match &self.work {
            RollbackWork::Reverse(journal) => Some(
                crate::exchange::rollback::RestoreRevision::capture(journal)?,
            ),
            RollbackWork::Prepare(_)
            | RollbackWork::Restore { .. }
            | RollbackWork::Conversation => None,
        };
        let work = async {
            let (prepared, journal) = match self.work {
                RollbackWork::Prepare(preparation) => {
                    let RollbackPreparation {
                        workspace,
                        expected,
                        target,
                        exchanges,
                    } = *preparation;
                    let objects = self.objects.clone();
                    let preview = tokio::task::spawn_blocking(move || {
                        let mut preview =
                            RestorePreview::prepare(&objects, &workspace, &expected, &target)?;
                        preview.exchange_ids = exchanges;
                        preview.save(&objects)?;
                        Ok::<_, anyhow::Error>(preview)
                    })
                    .await
                    .map_err(|_| RollbackFailure::PreparationFailed)?
                    .map_err(|_| RollbackFailure::PreparationFailed)?;
                    (Some(preview), None)
                }
                RollbackWork::Reverse(journal) => {
                    let objects = self.objects.clone();
                    let preview = tokio::task::spawn_blocking(move || {
                        let preview = journal.reverse(&objects)?;
                        preview.save(&objects)?;
                        Ok::<_, anyhow::Error>(preview)
                    })
                    .await
                    .map_err(|_| RollbackFailure::PreparationFailed)?
                    .map_err(|_| RollbackFailure::PreparationFailed)?;
                    (Some(preview), None)
                }
                RollbackWork::Restore { preview, resume } => {
                    let journal = GitCheckpoint::new(&preview.workspace)
                        .apply_preview(self.repositories, self.objects, preview, resume)
                        .await
                        .map_err(|_| RollbackFailure::RecoveryRequired)?;
                    (None, Some(journal))
                }
                RollbackWork::Conversation => (None, None),
            };
            Ok::<_, RollbackFailure>((prepared, journal))
        }
        .await;
        match work {
            Ok((prepared, journal)) => Ok(RollbackEffectResult {
                prepared,
                journal,
                teardown: self.teardown,
                authority,
                failure: None,
            }),
            Err(failure) => Ok(RollbackEffectResult {
                prepared: None,
                journal: None,
                teardown: self.teardown,
                authority,
                failure: Some(failure),
            }),
        }
    }
}
impl HarnessBroker {
    /// Capture an owned effect after the owner's collection barrier.
    pub(crate) fn prepare_rollback_effect(
        &mut self,
        command: RollbackCommand,
    ) -> std::result::Result<RollbackEffect, RollbackFailure> {
        let command = self.capture_rollback(&command)?;
        let mut teardown = None;
        let work = match command {
            RollbackCommand::Prepare(selection) => {
                self.require_restore_recovered()
                    .map_err(|_| RollbackFailure::RecoveryRequired)?;
                let id = self
                    .store
                    .checkpoint_owner(&self.session.id, &selection.exchange_id)
                    .map_err(|_| RollbackFailure::PreparationFailed)?;
                let history = self
                    .store
                    .list_exchange(&self.session.id)
                    .map_err(|_| RollbackFailure::PreparationFailed)?;
                let position = history
                    .iter()
                    .position(|exchange| exchange.id == id)
                    .ok_or(RollbackFailure::StaleAuthority)?;
                if history[position].disposition() != crate::exchange::HistoryDisposition::Current
                    || !history[position..]
                        .iter()
                        .all(|exchange| exchange.is_finished())
                {
                    return Err(RollbackFailure::StaleAuthority);
                }
                let exchanges: Vec<_> = history[position..]
                    .iter()
                    .map(|exchange| exchange.id.clone())
                    .collect();
                self.store
                    .require_settled_exchange_tree(&self.session.id, &exchanges)
                    .map_err(|_| RollbackFailure::PreparationFailed)?;
                let expected_id = history
                    .iter()
                    .rev()
                    .find_map(|exchange| match exchange.disposition() {
                        crate::exchange::HistoryDisposition::Current => {
                            exchange.checkpoint_after.as_deref()
                        }
                        crate::exchange::HistoryDisposition::RolledBack => {
                            exchange.checkpoint_before.as_deref()
                        }
                        crate::exchange::HistoryDisposition::Superseded => None,
                    })
                    .ok_or(RollbackFailure::PreparationFailed)?;
                let target_id = history[position]
                    .checkpoint_before
                    .as_deref()
                    .ok_or(RollbackFailure::PreparationFailed)?;
                let expected = self
                    .store
                    .load_checkpoint(expected_id)
                    .map_err(|_| RollbackFailure::PreparationFailed)?
                    .ok_or(RollbackFailure::PreparationFailed)?;
                let target = self
                    .store
                    .load_checkpoint(target_id)
                    .map_err(|_| RollbackFailure::PreparationFailed)?
                    .ok_or(RollbackFailure::PreparationFailed)?;
                let WorkspaceKind::Git(workspace) = &self.workspace_kind else {
                    return Err(RollbackFailure::PreparationFailed);
                };
                RollbackWork::Prepare(Box::new(RollbackPreparation {
                    workspace: workspace.clone(),
                    expected,
                    target,
                    exchanges,
                }))
            }
            RollbackCommand::Apply(PreparedRollbackToken::Conversation { selection }) => {
                teardown = Some(self.prepare_rollback_teardown(&json!({"rollback":{"scope":"conversation","exchange_id":selection.exchange_id,"latest_exchange_id":selection.latest_exchange_id}}), false)
                    .map_err(|_| RollbackFailure::RecoveryRequired)?);
                RollbackWork::Conversation
            }
            RollbackCommand::Apply(PreparedRollbackToken::Workspace { preview_id, .. }) => {
                let preview =
                    RestorePreview::load(&self.store.objects, &self.session.id, &preview_id)
                        .map_err(|_| RollbackFailure::AuthorityUnavailable)?;
                teardown = self.rollback_preview_teardown(&preview)?;
                RollbackWork::Restore {
                    preview,
                    resume: false,
                }
            }
            RollbackCommand::ResumeRestore(_) => {
                let journal = RestoreJournal::load(&self.store.objects, &self.session.id)
                    .map_err(|_| RollbackFailure::AuthorityUnavailable)?
                    .ok_or(RollbackFailure::JournalMissing)?;
                if journal.state == crate::checkpoint::RestoreState::Complete {
                    return Err(RollbackFailure::StaleAuthority);
                }
                teardown = self.rollback_preview_teardown(&journal.preview)?;
                RollbackWork::Restore {
                    preview: journal.preview,
                    resume: true,
                }
            }
            RollbackCommand::PrepareUndoRestore(_) => {
                let journal = RestoreJournal::load(&self.store.objects, &self.session.id)
                    .map_err(|_| RollbackFailure::AuthorityUnavailable)?
                    .ok_or(RollbackFailure::JournalMissing)?;
                RollbackWork::Reverse(journal)
            }
        };
        Ok(RollbackEffect {
            objects: self.store.objects.clone(),
            repositories: self.repositories.clone(),
            work,
            teardown,
        })
    }

    /// Commit settled rollback effects before the owner releases completion waiters.
    pub(crate) fn commit_rollback_effect(
        &mut self,
        mut result: RollbackEffectResult,
    ) -> std::result::Result<(RollbackOutcome, Vec<SessionEvent>), RollbackFailure> {
        if let Some(failure) = result.failure {
            return Err(failure);
        }
        if let Some(preview) = result.prepared {
            if let Some(authority) = result.authority {
                let journal = RestoreJournal::load(&self.store.objects, &self.session.id)
                    .map_err(|_| RollbackFailure::AuthorityUnavailable)?
                    .ok_or(RollbackFailure::JournalMissing)?;
                authority.validate(&journal)?;
            }
            return Ok((
                RollbackOutcome::Prepared {
                    token: self.rollback_token(&preview.id)?,
                    files: preview.path.len(),
                    warnings: preview.warning,
                },
                Vec::new(),
            ));
        }
        let events = if let Some(teardown) = result.teardown {
            let (events, lane) = self
                .commit_undo_teardown(teardown)
                .map_err(|_| RollbackFailure::RecoveryRequired)?;
            self.task_controller
                .native_goal_written()
                .map_err(|_| RollbackFailure::RecoveryRequired)?;
            drop(lane);
            events
        } else {
            Vec::new()
        };
        if let Some(journal) = &mut result.journal {
            journal.state = crate::checkpoint::RestoreState::Complete;
            journal
                .save(&self.store.objects)
                .map_err(|_| RollbackFailure::RecoveryRequired)?;
        }
        Ok((RollbackOutcome::Restored, events))
    }

    /// Capture native teardown only while history still needs its restore commit.
    fn rollback_preview_teardown(
        &mut self,
        preview: &RestorePreview,
    ) -> std::result::Result<Option<foreground_undo::UndoTeardown>, RollbackFailure> {
        if !crate::workspace::same(&preview.workspace, &self.session.workspace) {
            return Err(RollbackFailure::StaleAuthority);
        }
        let Some(first) = preview.exchange_ids.first() else {
            return Ok(None);
        };
        let history = self
            .store
            .list_exchange(&self.session.id)
            .map_err(|_| RollbackFailure::AuthorityUnavailable)?;
        let position = history
            .iter()
            .position(|exchange| &exchange.id == first)
            .ok_or(RollbackFailure::StaleAuthority)?;
        if history[position].disposition() == crate::exchange::HistoryDisposition::RolledBack {
            if !preview.exchange_ids.iter().enumerate().all(|(index, id)| {
                history.iter().any(|exchange| {
                    &exchange.id == id
                        && exchange.disposition()
                            == if index == 0 {
                                crate::exchange::HistoryDisposition::RolledBack
                            } else {
                                crate::exchange::HistoryDisposition::Superseded
                            }
                })
            }) {
                return Err(RollbackFailure::StaleAuthority);
            }
            return Ok(None);
        }
        if !history[position..]
            .iter()
            .map(|exchange| &exchange.id)
            .eq(preview.exchange_ids.iter())
        {
            return Err(RollbackFailure::StaleAuthority);
        }
        let latest = history
            .last()
            .ok_or(RollbackFailure::StaleAuthority)?
            .id
            .clone();
        self.prepare_rollback_teardown(&json!({"rollback":{"scope":"conversation","exchange_id":first,"latest_exchange_id":latest}}), true)
            .map(Some).map_err(|_| RollbackFailure::RecoveryRequired)
    }
}
