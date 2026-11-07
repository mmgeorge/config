use super::*;
use std::collections::BTreeSet;

/// Capture final workspace evidence without retaining mutable host state.
pub(crate) struct FinalCheckpointJob {
    #[cfg(test)]
    barrier: Option<(Arc<tokio::sync::Notify>, Arc<tokio::sync::Notify>)>,
    checkpoint: GitCheckpoint,
    objects: crate::storage::objects::ObjectStore,
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    session_id: String,
    exchange_id: String,
    owner: CaptureOwner,
    before: Option<crate::checkpoint::CheckpointRecord>,
    paths: BTreeSet<String>,
    outcome: crate::exchange::ExchangeOutcome,
    captured_at_ms: i64,
}

enum CaptureOwner {
    BeforeSubmission,
    Invocation {
        scope: crate::provider::InvocationScope,
    },
}

impl FinalCheckpointJob {
    /// Collect checkpoint and diff evidence from the captured immutable inputs.
    pub(crate) async fn run(self) -> Result<FinalCheckpointResult> {
        #[cfg(test)]
        if let Some((started, release)) = &self.barrier {
            started.notify_one();
            release.notified().await;
        }
        let checkpoint = self
            .checkpoint
            .capture(
                &self.objects,
                &self.repositories,
                &self.session_id,
                self.captured_at_ms,
            )
            .await?;
        let expected_before = self.before.as_ref().map(|before| before.id.clone());
        let before = match self.before {
            Some(before) => before,
            None => {
                anyhow::ensure!(
                    self.outcome != crate::exchange::ExchangeOutcome::Complete,
                    "completed interaction has no initial checkpoint"
                );
                checkpoint.clone()
            }
        };
        let checkpoint_diff_text = checkpoint_diff(
            &self.objects,
            &self.repositories.reads,
            &self.diff,
            &before,
            &checkpoint,
        )
        .await?;
        let attributed_diff_text = if self.paths.is_empty() {
            None
        } else {
            Some(
                checkpoint_diff_for_paths(
                    &self.objects,
                    &self.repositories.reads,
                    &self.diff,
                    &before,
                    &checkpoint,
                    &self.paths,
                )
                .await?,
            )
        };
        Ok(FinalCheckpointResult {
            session_id: self.session_id,
            exchange_id: self.exchange_id,
            owner: self.owner,
            expected_before,
            baseline: before.id,
            paths: self.paths,
            outcome: self.outcome,
            checkpoint,
            checkpoint_diff_text,
            attributed_diff_text,
        })
    }
}

/// Retain final workspace evidence until its authorizing host revision is validated.
pub(crate) struct FinalCheckpointResult {
    session_id: String,
    exchange_id: String,
    owner: CaptureOwner,
    expected_before: Option<String>,
    baseline: String,
    paths: BTreeSet<String>,
    outcome: crate::exchange::ExchangeOutcome,
    checkpoint: crate::checkpoint::CheckpointRecord,
    checkpoint_diff_text: String,
    attributed_diff_text: Option<String>,
}

/// Distinguish committed workspace evidence from a capture invalidated by newer work.
pub(crate) enum FinalCheckpointCommit {
    /// The captured checkpoint and complete exchange were committed together.
    Committed(String),
    /// Newer host or native state requires a fresh capture.
    Stale,
}

impl HarnessBroker {
    /// Capture final-checkpoint authority and inputs after initial checkpoint collection.
    pub(crate) fn prepare_final_checkpoint(
        &self,
        interaction: &Exchange,
        outcome: crate::exchange::ExchangeOutcome,
    ) -> Result<FinalCheckpointJob> {
        anyhow::ensure!(
            interaction.session_id == self.session.id,
            "checkpoint capture crosses session ownership"
        );
        anyhow::ensure!(
            interaction.finalization_outcome() == Some(outcome),
            "checkpoint capture requires the admitted finalization outcome"
        );
        let descendants: Vec<_> = referenced_child_exchange_list(interaction)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        self.store
            .require_settled_exchange_tree(&self.session.id, &descendants)?;
        let owner = match interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone())
        {
            Some(scope) => {
                anyhow::ensure!(
                    scope.owner().session_id().as_str() == self.session.id
                        && scope.owner().agent_id().as_str() == interaction.agent_id
                        && scope.exchange_id
                            == forge_provider_api::ExchangeId::from(interaction.id.as_str()),
                    "checkpoint capture crosses invocation ownership"
                );
                anyhow::ensure!(
                    interaction.turn.last().is_some_and(|turn| matches!(
                        turn.state(),
                        crate::turn::TurnState::Finished { .. }
                    )),
                    "checkpoint capture requires terminal native work"
                );
                CaptureOwner::Invocation { scope }
            }
            None => CaptureOwner::BeforeSubmission,
        };
        let before = interaction
            .checkpoint_before
            .as_deref()
            .map(|id| {
                self.store
                    .load_checkpoint(id)
                    .and_then(|record| record.context("interaction before checkpoint is missing"))
            })
            .transpose()?;
        Ok(FinalCheckpointJob {
            #[cfg(test)]
            barrier: None,
            checkpoint: self
                .checkpoint
                .clone()
                .context("Git checkpoint adapter is missing")?,
            objects: self.store.objects.clone(),
            repositories: self.repositories.clone(),
            diff: self.diff.clone(),
            session_id: self.session.id.clone(),
            exchange_id: interaction.id.clone(),
            owner,
            before,
            paths: self
                .interaction_provider_change_index(interaction)?
                .paths()
                .clone(),
            outcome,
            captured_at_ms: self.clock.now_ms(),
        })
    }

    /// Commit captured evidence only when the baseline, ownership and native revision remain current.
    pub(crate) fn commit_final_checkpoint(
        &mut self,
        interaction: &mut Exchange,
        captured: FinalCheckpointResult,
    ) -> Result<FinalCheckpointCommit> {
        anyhow::ensure!(
            captured.session_id == self.session.id
                && captured.exchange_id == interaction.id
                && interaction.session_id == self.session.id,
            "checkpoint result crosses exchange ownership"
        );
        let current_scope = interaction.turn.last().map(|turn| &turn.provider().scope);
        let owner_current = match &captured.owner {
            CaptureOwner::BeforeSubmission => current_scope.is_none(),
            CaptureOwner::Invocation { scope } => {
                current_scope == Some(scope)
                    && interaction.turn.last().is_some_and(|turn| {
                        matches!(turn.state(), crate::turn::TurnState::Finished { .. })
                    })
            }
        };
        if !owner_current
            || interaction.checkpoint_before != captured.expected_before
            || interaction
                .finalization_outcome()
                .is_some_and(|outcome| outcome != captured.outcome)
            || self.interaction_provider_change_index(interaction)?.paths() != &captured.paths
        {
            return Ok(FinalCheckpointCommit::Stale);
        }
        let descendants: Vec<_> = referenced_child_exchange_list(interaction)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        self.store
            .require_settled_exchange_tree(&self.session.id, &descendants)?;
        // Publish the complete durable exchange only after all stale-evidence checks pass.
        let mut settled = interaction.clone();
        settled.checkpoint_before = Some(captured.baseline);
        settled.attributed_matches_checkpoint = captured
            .attributed_diff_text
            .as_deref()
            .is_some_and(|diff| diff == captured.checkpoint_diff_text);
        settled.attributed_diff_text = captured.attributed_diff_text;
        settled.checkpoint_diff_text = Some(captured.checkpoint_diff_text);
        settled.checkpoint_after = Some(captured.checkpoint.id.clone());
        settled.finalization_error = None;
        settled.finish(captured.outcome, self.clock.now_ms())?;
        self.store
            .save_checkpoint_exchange(&captured.checkpoint, &settled)?;
        *interaction = settled;
        Ok(FinalCheckpointCommit::Committed(captured.checkpoint.id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::broker::test::{planning_question_broker, repository};

    /// An original completed invocation's workspace capture does not hold the broker mutex.
    #[tokio::test]
    async fn idle_checkpoint_capture_keeps_broker_available() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange = super::super::test::completed_interaction(
            "idle-checkpoint",
            &broker.session.id,
            Vec::new(),
        );
        exchange.phase = crate::exchange::ExchangePhase::Running;
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root(&broker.session.id),
            exchange.id.clone(),
        );
        let first = crate::provider::test_support::address(&scope);
        exchange.start_turn(first.clone(), 10)?;
        exchange
            .turn_mut(&first)
            .context("first turn missing")?
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                11,
            )?;
        exchange.begin_finalization(crate::exchange::ExchangeOutcome::Failed, 12)?;
        broker.store.save_exchange(&exchange)?;
        let started = Arc::new(tokio::sync::Notify::new());
        let release = Arc::new(tokio::sync::Notify::new());
        let shared = Arc::new(tokio::sync::Mutex::new(broker));
        let mut capture = shared
            .lock()
            .await
            .prepare_final_checkpoint(&exchange, crate::exchange::ExchangeOutcome::Failed)?;
        capture.barrier = Some((started.clone(), release.clone()));
        let job = tokio::spawn(capture.run());
        started.notified().await;
        assert!(
            shared.try_lock().is_ok(),
            "idle checkpoint held the broker mutex"
        );
        release.notify_one();
        job.await??;
        Ok(())
    }

    /// Discard an unlocked capture when initial checkpoint collection changed its baseline.
    #[tokio::test]
    async fn final_capture_rechecks_baseline_before_committing() -> Result<()> {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut exchange, _, _) = broker
            .test_stage_interaction_for_turn("capture", true, 10)
            .await?;
        let outcome = crate::exchange::ExchangeOutcome::Failed;
        exchange.begin_finalization(outcome, 11)?;
        let captured = broker
            .prepare_final_checkpoint(&exchange, outcome)?
            .run()
            .await?;
        let baseline = broker
            .checkpoint
            .as_ref()
            .context("fixture checkpoint adapter is missing")?
            .capture(
                &broker.store.objects,
                &broker.repositories,
                &broker.session.id,
                12,
            )
            .await?;
        broker.store.save_checkpoint(&baseline)?;
        exchange.checkpoint_before = Some(baseline.id);
        assert!(matches!(
            broker.commit_final_checkpoint(&mut exchange, captured)?,
            FinalCheckpointCommit::Stale
        ));
        assert!(exchange.checkpoint_after.is_none());
        assert_eq!(exchange.state(), ExchangeState::Finalizing);
        let fresh = broker
            .prepare_final_checkpoint(&exchange, outcome)?
            .run()
            .await?;
        let FinalCheckpointCommit::Committed(id) =
            broker.commit_final_checkpoint(&mut exchange, fresh)?
        else {
            anyhow::bail!("fresh final capture was rejected");
        };
        let persisted = broker
            .store
            .load_exchange(&exchange.id)?
            .context("committed exchange is missing")?;
        assert_eq!(persisted.checkpoint_after.as_deref(), Some(id.as_str()));
        assert_eq!(persisted.state(), ExchangeState::Failed);
        Ok(())
    }
}
