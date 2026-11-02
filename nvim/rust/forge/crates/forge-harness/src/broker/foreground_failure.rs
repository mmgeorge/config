//! Fence a failed foreground owner before exposing its error to the caller.
use super::checkpoint_settlement::FinalCheckpointJob;
use super::*;

/// Preserve the first failure and any distinct cleanup or presentation failure.
pub(crate) struct ForegroundFailure {
    original: anyhow::Error,
    cleanup: Option<ProviderError>,
    secondary: Vec<String>,
    checkpoint_pending: bool,
    checkpoint_done: bool,
    retraction_waiting: bool,
    retraction_prepared: bool,
    retraction_effects: Option<AppliedProviderEvent>,
    admission_cleanup_waiting: bool,
    admission_cleanup_started: bool,
    admission_checkpoint_pending: bool,
    admission_baseline_failed: bool,
    admission_effects: Option<AppliedProviderEvent>,
    admission_events: Vec<SessionEvent>,
    rollback: CancellationRollbackPhase,
}

enum CancellationRollbackPhase {
    Unexamined,
    Pending,
    Complete,
}

/// Collect a cancelled baseline before optional native rollback, without the broker lock.
pub(crate) struct CancelledTurnRollbackJob {
    provider: Arc<Provider>,
    session_id: String,
    exchange_id: String,
    baseline: CancelledBaseline,
    policy: CancelledTurnRollbackPolicy,
}

enum CancelledTurnRollbackPolicy {
    Retain,
    RequestNativeRollback,
}

enum CancelledTurnRollback {
    Retracted,
    Retained,
}

/// Carry the captured exchange identity and collection/rollback result back to its owner.
pub(crate) struct CancelledTurnRollbackResult {
    exchange_id: String,
    outcome: Result<CancelledTurnRollback>,
}

/// Transfer an uncollected baseline to the registry if its foreground waiter disappears.
struct CancelledBaseline {
    task: Option<crate::checkpoint::CheckpointTask>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
}

impl CancelledBaseline {
    /// Stop the baseline while retaining its handle until an explicit join.
    fn new(
        task: Option<crate::checkpoint::CheckpointTask>,
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    ) -> Self {
        if let Some(task) = &task {
            task.0.abort();
        }
        Self { task, registry }
    }

    /// Bound foreground waiting while leaving incomplete work owned for drop recovery.
    async fn collect(&mut self) -> Result<()> {
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        match tokio::time::timeout(std::time::Duration::from_secs(30), Self::join(task)).await {
            Ok(result) => {
                self.task.take();
                result
            }
            Err(error) => Err(error).context("cancelled baseline collection timed out"),
        }
    }

    /// Treat requested task cancellation as collected, while preserving worker failures.
    async fn join(task: &mut crate::checkpoint::CheckpointTask) -> Result<()> {
        match (&mut task.0).await {
            Ok(result) => result
                .map(|_| ())
                .context("cancelled baseline worker failed"),
            Err(error) if error.is_cancelled() => Ok(()),
            Err(error) => Err(error).context("cancelled baseline worker panicked"),
        }
    }
}

impl Drop for CancelledBaseline {
    fn drop(&mut self) {
        let Some(mut task) = self.task.take() else {
            return;
        };
        if let Err(error) = self
            .registry
            .retain_cleanup(async move { Self::join(&mut task).await })
        {
            self.registry.record_cleanup_failure(error);
        }
    }
}

impl CancelledTurnRollbackJob {
    /// Join the aborted baseline before requesting provider rollback or releasing the host.
    pub(crate) async fn run(mut self) -> CancelledTurnRollbackResult {
        let outcome = async {
            self.baseline.collect().await?;
            match self.policy {
                CancelledTurnRollbackPolicy::Retain => Ok(CancelledTurnRollback::Retained),
                CancelledTurnRollbackPolicy::RequestNativeRollback => {
                    let retracted = tokio::time::timeout(
                        std::time::Duration::from_secs(30),
                        self.provider.rollback_cancelled_turn_for(&self.session_id),
                    )
                    .await
                    .context("native cancelled-turn rollback timed out")??;
                    Ok(if retracted {
                        CancelledTurnRollback::Retracted
                    } else {
                        CancelledTurnRollback::Retained
                    })
                }
            }
        }
        .await;
        CancelledTurnRollbackResult {
            exchange_id: self.exchange_id,
            outcome,
        }
    }
}

/// Own the staged admission's provider fence and activated baseline worker.
pub(crate) struct AdmissionCleanupJob {
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ExecutionOwner,
    checkpoint: Option<crate::checkpoint::CheckpointTask>,
}

pub(crate) struct AdmissionCleanupResult {
    engine: Result<(), ProviderError>,
    checkpoint: Option<Result<crate::checkpoint::CheckpointRecord>>,
}

impl AdmissionCleanupJob {
    pub(crate) async fn run(mut self) -> AdmissionCleanupResult {
        let checkpoint = async {
            match self.checkpoint.as_mut() {
                Some(task) => Some(
                    (&mut task.0)
                        .await
                        .context("admission checkpoint worker failed")
                        .and_then(|result| result),
                ),
                None => None,
            }
        };
        let (engine, checkpoint) = tokio::join!(self.registry.fence(&self.owner), checkpoint);
        AdmissionCleanupResult { engine, checkpoint }
    }
}

/// Own the native goal update required by a retracted host snapshot.
pub(crate) struct RetractionGoalJob {
    provider: Arc<Provider>,
    request: ProviderRequest,
    update: crate::provider::NativeGoalUpdate,
    controller: Arc<crate::task::SessionTaskController>,
    expected_task: Option<crate::task::Task>,
    data_root: PathBuf,
    reconcile: bool,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ExecutionOwner,
}

impl RetractionGoalJob {
    pub(super) fn new(
        broker: &HarnessBroker,
        request: ProviderRequest,
        update: crate::provider::NativeGoalUpdate,
        expected_task: Option<crate::task::Task>,
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    ) -> Self {
        Self {
            provider: broker.provider.clone(),
            request,
            update,
            controller: broker.task_controller.clone(),
            expected_task,
            data_root: broker.data_root.clone(),
            reconcile: broker.input_wakeup.is_some(),
            registry,
            owner: crate::provider::ExecutionOwner::root(&broker.session.id),
        }
    }

    pub(crate) async fn run(self) -> Result<()> {
        let lane = self.controller.native_goal_lane();
        let guard = match tokio::time::timeout(std::time::Duration::from_secs(30), lane.lock())
            .await
        {
            Ok(guard) => guard,
            Err(_) => {
                self.controller.mark_native_goal_uncertain();
                let error = anyhow::Error::new(ProviderError::DeliveryUncertain {
                    message: "native goal transport lane timed out; provider outcome is uncertain"
                        .into(),
                });
                return Err(match self.registry.fence(&self.owner).await {
                    Ok(()) => error,
                    Err(cleanup) => error.context(cleanup),
                });
            }
        };
        self.controller.ensure_native_goal_certain()?;
        anyhow::ensure!(
            same_retraction_task(
                self.controller.current()?.as_ref(),
                self.expected_task.as_ref()
            ),
            "task changed before native retraction goal restore"
        );
        let session = crate::storage::SqliteStore::read_only(&self.data_root)?
            .load_session(&self.request.harness_session_id)?
            .context("retraction session is missing")?;
        anyhow::ensure!(
            session.provider_session_id.as_deref()
                == self
                    .request
                    .provider_session_id
                    .as_ref()
                    .map(|id| id.as_str()),
            "provider session changed before native retraction goal restore"
        );
        let result = match tokio::time::timeout(
            std::time::Duration::from_secs(30),
            self.provider.goal_status(self.request, self.update),
        )
        .await
        {
            Ok(Ok(())) => Ok(()),
            Ok(Err(error)) => {
                self.controller.mark_native_goal_uncertain();
                Err(error.context(ProviderError::DeliveryUncertain { message: "native retraction goal update failed after dispatch; provider outcome is uncertain".into() }))
            }
            Err(_) => {
                self.controller.mark_native_goal_uncertain();
                Err(ProviderError::DeliveryUncertain {
                    message:
                        "native retraction goal restore timed out; provider outcome is uncertain"
                            .into(),
                }
                .into())
            }
        };
        let epoch = match self.controller.native_goal_written() {
            Ok(epoch) => epoch,
            Err(error) => {
                drop(guard);
                return Err(match self.registry.fence(&self.owner).await {
                    Ok(()) => error,
                    Err(cleanup) => error.context(cleanup),
                });
            }
        };
        let changed = !same_retraction_task(
            self.controller.current()?.as_ref(),
            self.expected_task.as_ref(),
        );
        drop(guard);
        if let Err(uncertain) = self.controller.ensure_native_goal_certain() {
            let error = result.err().unwrap_or(uncertain);
            return Err(match self.registry.fence(&self.owner).await {
                Ok(()) => error,
                Err(cleanup) => error.context(cleanup),
            });
        }
        if self.reconcile {
            if tokio::time::timeout(
                std::time::Duration::from_secs(30),
                self.controller.wait_native_goal_acknowledged(epoch),
            )
            .await
            .is_err()
            {
                self.controller.mark_native_goal_uncertain();
                let error = anyhow::Error::new(ProviderError::DeliveryUncertain {
                    message: "native goal reconciliation timed out; provider outcome is uncertain"
                        .into(),
                });
                return Err(match self.registry.fence(&self.owner).await {
                    Ok(()) => error,
                    Err(cleanup) => error.context(cleanup),
                });
            }
        } else if changed {
            self.controller.mark_native_goal_uncertain();
            let error = anyhow::Error::new(ProviderError::DeliveryUncertain { message: "task changed during native retraction goal restore; provider outcome is uncertain".into() });
            return Err(match self.registry.fence(&self.owner).await {
                Ok(()) => error,
                Err(cleanup) => error.context(cleanup),
            });
        }
        result
    }
}

/// Compare task authority and goal contract without serializing it as a synchronization token.
pub(super) fn same_retraction_task(
    current: Option<&crate::task::Task>,
    expected: Option<&crate::task::Task>,
) -> bool {
    match (current, expected) {
        (None, None) => true,
        (Some(current), Some(expected)) => {
            current.id == expected.id
                && current.revision == expected.revision
                && current.observation_revision == expected.observation_revision
                && current.entry_accepted() == expected.entry_accepted()
                && current.same_contract(expected)
        }
        _ => false,
    }
}

#[derive(Debug)]
pub(crate) struct ForegroundFailureContext {
    original_message: String,
    pub(crate) cleanup: Option<ProviderError>,
    secondary: Vec<String>,
}

impl std::fmt::Display for ForegroundFailureContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.original_message)?;
        if let Some(cleanup) = &self.cleanup {
            write!(f, ": {cleanup}")?;
        }
        for error in &self.secondary {
            write!(f, "; {error}")?;
        }
        Ok(())
    }
}

impl std::error::Error for ForegroundFailureContext {}

impl HarnessBroker {
    /// Fence the native owner and transfer its task to an owned cleanup job.
    pub(crate) fn fail_foreground(
        &mut self,
        transaction: &mut ForegroundTransaction,
        error: anyhow::Error,
    ) -> Result<ForegroundAction> {
        if transaction.failure_cleanup_active {
            let message = format!("additional foreground failure: {error:#}");
            if let Some(failure) = &mut transaction.failure {
                failure.secondary.push(message);
            } else {
                transaction.failure_secondary.push(message);
            }
            return Ok(ForegroundAction::Wait);
        }
        transaction.failure_cleanup_active = true;
        transaction.failure_inbox_open = true;
        transaction
            .native
            .cancellation
            .requested
            .store(true, Ordering::Release);
        let native = std::mem::replace(&mut transaction.native.task, tokio::task::JoinSet::new());
        Ok(ForegroundAction::CancelJobsAndEffect(
            ForegroundJob::CleanupForeground {
                engine: transaction.engine.clone(),
                native,
                original: error,
            },
        ))
    }

    /// Retain the watchdog's failed tool observation before fencing the owner.
    pub(crate) fn handle_foreground_timeout(
        &mut self,
        transaction: &mut ForegroundTransaction,
        expired: crate::provider::tool_timeout::ToolTimeout,
    ) -> Result<ForegroundAction> {
        if !expired.claim_report() {
            return self.fail_foreground(transaction, expired.into());
        }
        let observation = ProviderEvent::Tool {
            address: expired.address.clone(),
            update: crate::provider::ToolUpdate::Finished {
                outcome: crate::provider::ToolOutcome::Failed {
                    diagnostic: crate::provider::ToolDiagnostic {
                        code: Some("tool_timeout".into()),
                        message: expired.to_string(),
                    },
                },
            },
        };
        transaction
            .pending_session_observations
            .push_back(observation);
        self.fail_foreground(transaction, expired.into())
    }

    /// Start failure processing even if the request has not acquired a native owner.
    pub(crate) fn fail_foreground_dispatch(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        error: anyhow::Error,
    ) -> Result<ForegroundAction> {
        match dispatch {
            ForegroundDispatch::Admitting(admission) => {
                if let Some(failure) = &mut admission.failure {
                    failure
                        .secondary
                        .push(format!("additional admission failure: {error:#}"));
                    return Ok(ForegroundAction::Wait);
                }
                admission.failure = Some(ForegroundFailure {
                    original: error,
                    cleanup: None,
                    secondary: Vec::new(),
                    checkpoint_pending: false,
                    checkpoint_done: true,
                    retraction_waiting: false,
                    retraction_prepared: false,
                    retraction_effects: None,
                    admission_cleanup_waiting: true,
                    admission_cleanup_started: false,
                    admission_checkpoint_pending: false,
                    admission_baseline_failed: false,
                    admission_effects: None,
                    admission_events: Vec::new(),
                    rollback: CancellationRollbackPhase::Unexamined,
                });
                Ok(ForegroundAction::WaitForPublicationDrain)
            }
            ForegroundDispatch::Running {
                transaction,
                admission,
            }
            | ForegroundDispatch::Finalizing {
                transaction,
                admission,
            } => {
                if let Some(failure) = &mut admission.failure {
                    failure
                        .secondary
                        .push(format!("additional foreground failure: {error:#}"));
                    Ok(ForegroundAction::Wait)
                } else {
                    self.fail_foreground(transaction, error)
                }
            }
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => Err(error),
        }
    }

    /// Collect any staged owner and baseline only after already-started admission jobs finish.
    fn begin_staged_admission_cleanup(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let failure = admission
            .failure
            .as_mut()
            .context("admission failure is missing")?;
        failure.admission_cleanup_waiting = false;
        if admission.reset_check_setup {
            self.check_setup = false;
            admission.reset_check_setup = false;
        }
        if let Some(mut prior) = admission.prior_finalization.take()
            && let Err(error) = self.abort_prior_finalization(&mut prior, &failure.original)
        {
            failure
                .secondary
                .push(format!("abort prior finalization: {error:#}"));
        }
        failure.admission_events.append(&mut admission.events);
        let Some(staged) = admission.staged_interaction.as_mut() else {
            return self.prepare_foreground_failure_publication(admission);
        };
        if let Err(error) = staged.submission.failed(format!("{:#}", failure.original)) {
            failure
                .secondary
                .push(format!("record rejected input: {error:#}"));
        }
        if failure.original.downcast_ref::<TurnCancelled>().is_some()
            && let Err(error) = staged.submission.cancel()
        {
            failure
                .secondary
                .push(format!("cancel unsubmitted input: {error:#}"));
        }
        let checkpoint = match self.exchange_runtime.as_mut() {
            Some(runtime) if runtime.exchange_id == staged.exchange.id => runtime.checkpoint.take(),
            Some(_) => {
                failure
                    .secondary
                    .push("staged admission runtime belongs to another exchange".into());
                failure.admission_baseline_failed = true;
                None
            }
            None => None,
        };
        failure.admission_cleanup_started = true;
        Ok(ForegroundAction::Effect(ForegroundJob::AdmissionCleanup(
            AdmissionCleanupJob {
                registry: self.execution_registry(),
                owner: crate::provider::ExecutionOwner::root(&self.session.id),
                checkpoint,
            },
        )))
    }

    /// Apply the joined staged baseline and retain any typed engine cleanup error.
    pub(crate) fn complete_staged_admission_cleanup(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: AdmissionCleanupResult,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Admitting(admission) = dispatch else {
            anyhow::bail!("admission cleanup has no admission owner");
        };
        let failure = admission
            .failure
            .as_mut()
            .context("admission failure is missing")?;
        anyhow::ensure!(
            failure.admission_cleanup_started,
            "unexpected admission cleanup result"
        );
        failure.admission_cleanup_started = false;
        if let Err(error) = result.engine
            && failure.cleanup.is_none()
        {
            failure.cleanup = Some(error);
        }
        if let Some(checkpoint) = result.checkpoint {
            let staged = admission
                .staged_interaction
                .as_mut()
                .context("staged exchange disappeared")?;
            let Some(mut runtime) = self.exchange_runtime.take() else {
                failure
                    .secondary
                    .push("staged checkpoint runtime disappeared".into());
                failure.admission_baseline_failed = true;
                return self.advance_staged_admission_failure(admission);
            };
            let applied = if runtime.exchange_id == staged.exchange.id {
                self.finish_initial_checkpoint(&mut runtime, &mut staged.exchange, checkpoint)
            } else {
                Err(anyhow::anyhow!(
                    "staged checkpoint crossed exchange ownership"
                ))
            };
            self.exchange_runtime = Some(runtime);
            if let Err(error) = applied {
                failure
                    .secondary
                    .push(format!("staged baseline cleanup: {error:#}"));
                failure.admission_baseline_failed = true;
            }
        }
        admission.engine = None;
        self.advance_staged_admission_failure(admission)
    }

    fn advance_staged_admission_failure(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let failure = admission
            .failure
            .as_mut()
            .context("admission failure is missing")?;
        let Some(staged) = admission.staged_interaction.as_mut() else {
            return self.prepare_foreground_failure_publication(admission);
        };
        if failure.admission_checkpoint_pending {
            return Ok(ForegroundAction::Wait);
        }
        if failure.original.downcast_ref::<TurnRetracted>().is_some()
            && staged.exchange.execution_started_at_ms().is_none()
            && matches!(
                staged.submission.record()?.delivery,
                crate::provider::submission::SubmissionState::Prepared
            )
        {
            staged.submission.cancel()?;
            self.store.delete_exchange(&staged.exchange.id)?;
            if self
                .exchange_runtime
                .as_ref()
                .is_some_and(|runtime| runtime.exchange_id == staged.exchange.id)
            {
                self.exchange_runtime = None;
            }
            failure.admission_effects =
                Some(AppliedProviderEvent::Timeline(self.reconcile_timeline()?));
            admission.staged_interaction = None;
            return self.prepare_foreground_failure_publication(admission);
        }
        if !staged.exchange.is_finished() && failure.admission_baseline_failed {
            let outcome = if failure.original.downcast_ref::<TurnCancelled>().is_some() {
                crate::exchange::ExchangeOutcome::Cancelled
            } else {
                crate::exchange::ExchangeOutcome::Failed
            };
            if let Err(error) = staged
                .exchange
                .begin_finalization(outcome, self.clock.now_ms())
            {
                failure
                    .secondary
                    .push(format!("mark staged failure: {error:#}"));
            }
            staged.exchange.finalization_error = Some(
                failure
                    .secondary
                    .last()
                    .cloned()
                    .unwrap_or_else(|| "staged checkpoint unavailable".into()),
            );
            if let Err(error) = self.store.save_exchange(&staged.exchange) {
                failure
                    .secondary
                    .push(format!("persist staged failure: {error:#}"));
            }
        }
        if !staged.exchange.is_finished() && !failure.admission_baseline_failed {
            let outcome = if failure.original.downcast_ref::<TurnCancelled>().is_some() {
                crate::exchange::ExchangeOutcome::Cancelled
            } else {
                crate::exchange::ExchangeOutcome::Failed
            };
            let prepared: Result<Option<FinalCheckpointJob>> = (|| {
                staged
                    .exchange
                    .begin_finalization(outcome, self.clock.now_ms())?;
                self.store.save_exchange(&staged.exchange)?;
                if self.checkpoint.is_none() {
                    let mut changes = crate::exchange::ProviderDiffBuilder::default();
                    self.visit_provider_exchanges(&staged.exchange, |exchange| {
                        changes.record(exchange)
                    })?;
                    staged.exchange.attributed_diff_text = changes.finish();
                    staged.exchange.attributed_matches_checkpoint = false;
                    staged.exchange.finish(outcome, self.clock.now_ms())?;
                    self.store.save_exchange(&staged.exchange)?;
                    Ok(None)
                } else {
                    Ok(Some(
                        self.prepare_final_checkpoint(&staged.exchange, outcome)?,
                    ))
                }
            })();
            match prepared {
                Ok(Some(job)) => {
                    failure.admission_checkpoint_pending = true;
                    return Ok(ForegroundAction::Effect(
                        ForegroundJob::AdmissionFailureCheckpoint(Box::new(job)),
                    ));
                }
                Ok(None) => {}
                Err(error) => {
                    staged.exchange.finalization_error = Some(format!("{error:#}"));
                    if let Err(save) = self.store.save_exchange(&staged.exchange) {
                        failure
                            .secondary
                            .push(format!("record staged failure: {save:#}"));
                    }
                    failure
                        .secondary
                        .push(format!("finalize staged exchange: {error:#}"));
                }
            }
        }
        if self
            .exchange_runtime
            .as_ref()
            .is_some_and(|runtime| runtime.exchange_id == staged.exchange.id)
        {
            self.exchange_runtime = None;
        }
        match self.prepare_live_interaction(publication::LiveUpdate::StateChanged, &staged.exchange)
        {
            Ok(effects) => failure.admission_effects = Some(effects),
            Err(error) => failure
                .secondary
                .push(format!("stage failed exchange presentation: {error:#}")),
        }
        self.prepare_foreground_failure_publication(admission)
    }

    /// Commit final staged evidence only for the originally captured exchange revision.
    pub(crate) fn complete_staged_admission_checkpoint(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<checkpoint_settlement::FinalCheckpointResult>,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Admitting(admission) = dispatch else {
            anyhow::bail!("staged failure checkpoint has no admission owner");
        };
        let failure = admission
            .failure
            .as_mut()
            .context("admission failure is missing")?;
        anyhow::ensure!(
            failure.admission_checkpoint_pending,
            "unexpected staged failure checkpoint"
        );
        failure.admission_checkpoint_pending = false;
        let staged = admission
            .staged_interaction
            .as_mut()
            .context("staged exchange disappeared")?;
        match result.and_then(|result| self.commit_final_checkpoint(&mut staged.exchange, result)) {
            Ok(checkpoint_settlement::FinalCheckpointCommit::Committed(_)) => {}
            Ok(checkpoint_settlement::FinalCheckpointCommit::Stale) => {
                match self.store.load_exchange(&staged.exchange.id) {
                    Ok(Some(exchange)) => staged.exchange = exchange,
                    Ok(None) => {
                        failure
                            .secondary
                            .push("staged exchange disappeared during checkpoint".into());
                        failure.admission_baseline_failed = true;
                    }
                    Err(error) => {
                        failure
                            .secondary
                            .push(format!("reload staged exchange: {error:#}"));
                        failure.admission_baseline_failed = true;
                    }
                }
            }
            Err(error) => {
                staged.exchange.finalization_error = Some(format!("{error:#}"));
                if let Err(save) = self.store.save_exchange(&staged.exchange) {
                    failure
                        .secondary
                        .push(format!("record staged checkpoint failure: {save:#}"));
                }
                failure
                    .secondary
                    .push(format!("staged final checkpoint: {error:#}"));
                failure.admission_baseline_failed = true;
            }
        }
        self.advance_staged_admission_failure(admission)
    }

    /// Record typed cleanup separately and retain all observations accepted before closure.
    pub(crate) fn complete_foreground_cleanup(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        original: anyhow::Error,
        cleanup: Result<(), ProviderError>,
    ) -> Result<ForegroundAction> {
        let (transaction, _admission) = match dispatch {
            ForegroundDispatch::Running {
                transaction,
                admission,
            }
            | ForegroundDispatch::Finalizing {
                transaction,
                admission,
            } => (transaction, admission),
            _ => anyhow::bail!("cleanup completed without its foreground owner"),
        };
        if transaction.failure.is_none() {
            transaction.failure = Some(ForegroundFailure {
                original,
                cleanup: cleanup.err(),
                secondary: std::mem::take(&mut transaction.failure_secondary),
                checkpoint_pending: false,
                checkpoint_done: false,
                retraction_waiting: false,
                retraction_prepared: false,
                retraction_effects: None,
                admission_cleanup_waiting: false,
                admission_cleanup_started: false,
                admission_checkpoint_pending: false,
                admission_baseline_failed: false,
                admission_effects: None,
                admission_events: Vec::new(),
                rollback: CancellationRollbackPhase::Unexamined,
            });
        }
        if transaction.native_goal.take().is_some() {
            if let Some(scope) = transaction
                .state
                .interaction
                .turn
                .last()
                .map(|turn| turn.provider().scope.clone())
            {
                self.reopened_execution.end_settlement(&scope);
            }
            transaction.state.interaction.finalization_error =
                Some("native goal stopped during foreground cleanup".into());
            if let Err(error) = self.store.save_exchange(&transaction.state.interaction) {
                transaction
                    .failure
                    .as_mut()
                    .unwrap()
                    .secondary
                    .push(format!("record interrupted native goal: {error:#}"));
            }
        }
        loop {
            match transaction.native.events.try_next() {
                Ok(Some(event)) => transaction.pending_session_observations.push_back(event),
                Ok(None) => break,
                Err(error) => {
                    transaction
                        .failure
                        .as_mut()
                        .unwrap()
                        .secondary
                        .push(format!("native observation drain failed: {error:#}"));
                    break;
                }
            }
        }
        match transaction.session_events.drain() {
            Ok(events) => transaction.pending_session_observations.extend(events),
            Err(error) => transaction
                .failure
                .as_mut()
                .unwrap()
                .secondary
                .push(format!("session observation drain failed: {error:#}")),
        }
        self.advance_foreground_failure(dispatch)
    }

    /// Advance to failure publication after retained observations have been applied.
    pub(crate) fn advance_foreground_failure(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let (transaction, admission) = match dispatch {
            ForegroundDispatch::Running {
                transaction,
                admission,
            }
            | ForegroundDispatch::Finalizing {
                transaction,
                admission,
            } => (transaction, admission),
            _ => anyhow::bail!("failure observations have no foreground owner"),
        };
        if transaction.failure.is_none() || !transaction.pending_session_observations.is_empty() {
            return Ok(ForegroundAction::Wait);
        }
        let failure = transaction.failure.as_mut().unwrap();
        if matches!(failure.rollback, CancellationRollbackPhase::Pending) {
            return Ok(ForegroundAction::Wait);
        }
        if matches!(failure.rollback, CancellationRollbackPhase::Unexamined)
            && failure.original.is::<TurnCancelled>()
            && failure.cleanup.is_none()
            && transaction
                .state
                .runtime
                .as_ref()
                .is_some_and(ExchangeRuntime::tools_blocked)
        {
            let runtime = transaction
                .state
                .runtime
                .as_mut()
                .context("blocked cancellation requires its checkpoint runtime")?;
            let retractable = runtime.retraction_eligible
                && self.capability.native_turn_rollback
                && transaction
                    .native
                    .cancellation
                    .restore_prompt
                    .load(Ordering::Acquire)
                && transaction
                    .native
                    .cancellation
                    .retraction_allowed
                    .load(Ordering::Acquire);
            // The native owner is collected and this gate proves no provider tool
            // could pass the baseline. Cancel its owned worker instead of waiting
            // for unrelated worktree writes before settling the stopped prompt.
            runtime
                .tool_gate
                .as_ref()
                .context("blocked checkpoint gate is missing")?
                .complete(Err(
                    "turn cancelled before checkpoint admission completed".into()
                ))?;
            // CleanupForeground has already joined and closed this execution owner.
            // Replacement must collect registry cleanup before opening its successor.
            let baseline =
                CancelledBaseline::new(runtime.checkpoint.take(), self.execution_registry());
            let interaction = &mut transaction.state.interaction;
            self.questions.cancel_chat(Some(&interaction.id))?;
            interaction.set_awaiting_input(false);
            interaction.finalization_error = None;
            interaction.finish(
                crate::exchange::ExchangeOutcome::Cancelled,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(interaction)?;
            failure.checkpoint_done = true;
            failure.rollback = CancellationRollbackPhase::Pending;
            return Ok(ForegroundAction::Effect(
                ForegroundJob::CancelledTurnRollback(CancelledTurnRollbackJob {
                    provider: self.provider.clone(),
                    session_id: self.session.id.clone(),
                    exchange_id: interaction.id.clone(),
                    baseline,
                    policy: if retractable {
                        CancelledTurnRollbackPolicy::RequestNativeRollback
                    } else {
                        CancelledTurnRollbackPolicy::Retain
                    },
                }),
            ));
        }
        if transaction
            .state
            .runtime
            .as_ref()
            .is_some_and(|runtime| runtime.checkpoint.is_some())
        {
            return Ok(ForegroundAction::Wait);
        }
        if failure.checkpoint_pending {
            return Ok(ForegroundAction::Wait);
        }
        if failure.original.downcast_ref::<TurnRetracted>().is_some() {
            failure.checkpoint_done = true;
        }
        if !failure.checkpoint_done {
            let interaction = &mut transaction.state.interaction;
            if interaction.is_finished() {
                failure.checkpoint_done = true;
            } else {
                if interaction
                    .turn
                    .last()
                    .is_some_and(|turn| self.reopened_execution.active(&turn.provider().scope))
                {
                    return Ok(ForegroundAction::Wait);
                }
                let outcome = if failure.original.downcast_ref::<TurnCancelled>().is_some() {
                    crate::exchange::ExchangeOutcome::Cancelled
                } else {
                    crate::exchange::ExchangeOutcome::Failed
                };
                let prepared: Result<Option<FinalCheckpointJob>> = (|| {
                    interaction.begin_finalization(outcome, self.clock.now_ms())?;
                    self.store.save_exchange(interaction)?;
                    if let Some(scope) = interaction
                        .turn
                        .last()
                        .map(|turn| turn.provider().scope.clone())
                    {
                        self.reopened_execution.begin_settlement(&scope);
                    }
                    if self.checkpoint.is_none() {
                        let mut changes = crate::exchange::ProviderDiffBuilder::default();
                        self.visit_provider_exchanges(interaction, |exchange| {
                            changes.record(exchange)
                        })?;
                        interaction.attributed_diff_text = changes.finish();
                        interaction.attributed_matches_checkpoint = false;
                        interaction.finish(outcome, self.clock.now_ms())?;
                        self.store.save_exchange(interaction)?;
                        return Ok(None);
                    }
                    Ok(Some(self.prepare_final_checkpoint(interaction, outcome)?))
                })();
                match prepared {
                    Ok(Some(job)) => {
                        failure.checkpoint_pending = true;
                        return Ok(ForegroundAction::Effect(ForegroundJob::FailureCheckpoint(
                            Box::new(job),
                        )));
                    }
                    Ok(None) => {
                        if let Some(scope) = interaction
                            .turn
                            .last()
                            .map(|turn| turn.provider().scope.clone())
                        {
                            self.reopened_execution.end_settlement(&scope);
                        }
                        failure.checkpoint_done = true;
                    }
                    Err(error) => {
                        if let Some(scope) = interaction
                            .turn
                            .last()
                            .map(|turn| turn.provider().scope.clone())
                        {
                            self.reopened_execution.end_settlement(&scope);
                        }
                        interaction.finalization_error = Some(format!("{error:#}"));
                        if let Err(save) = self.store.save_exchange(interaction) {
                            failure
                                .secondary
                                .push(format!("record failure checkpoint: {save:#}"));
                        }
                        failure
                            .secondary
                            .push(format!("prepare failure checkpoint: {error:#}"));
                        failure.checkpoint_done = true;
                    }
                }
            }
        }
        if admission.submission.is_none()
            && let Some(finalization) = transaction.finalization.take()
        {
            admission.submission = finalization.submission;
            admission.events.extend(finalization.events);
        }
        admission.failure = transaction.failure.take();
        if let Some(failure) = &mut admission.failure {
            failure.admission_events.append(&mut admission.events);
            // Accepted prompts no longer have an admission rejection to refresh their
            // presentation. Publish the settled exchange through the same ordered
            // failure delivery before releasing its foreground owner.
            if failure.original.downcast_ref::<TurnRetracted>().is_none() {
                match self.reconcile_timeline() {
                    Ok(timeline) => {
                        let settled = AppliedProviderEvent::Timeline(timeline);
                        failure.admission_effects = Some(match failure.admission_effects.take() {
                            Some(staged) => AppliedProviderEvent::Effects(vec![staged, settled]),
                            None => settled,
                        });
                    }
                    Err(error) => failure
                        .secondary
                        .push(format!("stage failed exchange presentation: {error:#}")),
                }
            }
        }
        self.prepare_foreground_failure_publication(admission)
    }

    /// Apply native proof only to the cancelled exchange that requested rollback.
    pub(crate) fn complete_cancelled_turn_rollback(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: CancelledTurnRollbackResult,
    ) -> Result<ForegroundAction> {
        let transaction = match dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => transaction,
            _ => anyhow::bail!("native rollback has no foreground owner"),
        };
        let failure = transaction
            .failure
            .as_mut()
            .context("native rollback failure owner is missing")?;
        anyhow::ensure!(
            matches!(failure.rollback, CancellationRollbackPhase::Pending)
                && transaction.state.interaction.id == result.exchange_id,
            "native rollback result crosses foreground ownership"
        );
        failure.rollback = CancellationRollbackPhase::Complete;
        match result.outcome {
            Ok(CancelledTurnRollback::Retracted) => {
                self.store.delete_exchange(&result.exchange_id)?;
                transaction.state.runtime.take();
                failure.original = TurnRetracted {
                    prompt: transaction.state.interaction.prompt.clone(),
                }
                .into();
                failure.admission_effects =
                    Some(AppliedProviderEvent::Timeline(self.reconcile_timeline()?));
            }
            Ok(CancelledTurnRollback::Retained) => {}
            Err(error) => failure.secondary.push(format!(
                "collect cancelled baseline or roll back native turn: {error:#}"
            )),
        }
        self.advance_foreground_failure(dispatch)
    }

    /// Commit the final failure checkpoint or retry if its revision became stale.
    pub(crate) fn complete_foreground_failure_checkpoint(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<checkpoint_settlement::FinalCheckpointResult>,
    ) -> Result<ForegroundAction> {
        let transaction = match dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => transaction,
            _ => anyhow::bail!("failure checkpoint has no foreground owner"),
        };
        let failure = transaction
            .failure
            .as_mut()
            .context("failure checkpoint state missing")?;
        anyhow::ensure!(
            failure.checkpoint_pending,
            "unexpected failure checkpoint result"
        );
        failure.checkpoint_pending = false;
        match result.and_then(|result| {
            self.commit_final_checkpoint(&mut transaction.state.interaction, result)
        }) {
            Ok(checkpoint_settlement::FinalCheckpointCommit::Stale) => {
                match transaction.session_events.drain() {
                    Ok(observations) => transaction
                        .pending_session_observations
                        .extend(observations),
                    Err(error) => failure
                        .secondary
                        .push(format!("drain stale checkpoint observations: {error:#}")),
                }
                match self.store.load_exchange(&transaction.state.interaction.id) {
                    Ok(Some(exchange)) => transaction.state.interaction = exchange,
                    Ok(None) => {
                        failure
                            .secondary
                            .push("failure exchange disappeared during checkpoint".into());
                        failure.checkpoint_done = true;
                    }
                    Err(error) => {
                        failure
                            .secondary
                            .push(format!("reload failure exchange: {error:#}"));
                        failure.checkpoint_done = true;
                    }
                }
                if failure.checkpoint_done
                    && let Some(scope) = transaction
                        .state
                        .interaction
                        .turn
                        .last()
                        .map(|turn| turn.provider().scope.clone())
                {
                    self.reopened_execution.end_settlement(&scope);
                }
            }
            Ok(checkpoint_settlement::FinalCheckpointCommit::Committed(_)) => {
                failure.checkpoint_done = true;
                if let Some(scope) = transaction
                    .state
                    .interaction
                    .turn
                    .last()
                    .map(|turn| turn.provider().scope.clone())
                {
                    self.reopened_execution.end_settlement(&scope);
                }
            }
            Err(error) => {
                failure
                    .secondary
                    .push(format!("final failure checkpoint: {error:#}"));
                transaction.state.interaction.finalization_error = Some(format!("{error:#}"));
                if let Err(save) = self.store.save_exchange(&transaction.state.interaction) {
                    failure
                        .secondary
                        .push(format!("record failure checkpoint: {save:#}"));
                }
                if let Some(scope) = transaction
                    .state
                    .interaction
                    .turn
                    .last()
                    .map(|turn| turn.provider().scope.clone())
                {
                    self.reopened_execution.end_settlement(&scope);
                }
                failure.checkpoint_done = true;
            }
        }
        self.advance_foreground_failure(dispatch)
    }

    /// Join the baseline worker without triggering a native-goal transition during cleanup.
    pub(crate) fn apply_foreground_failure_checkpoint(
        &mut self,
        transaction: &mut ForegroundTransaction,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<ForegroundAction> {
        let runtime = transaction
            .state
            .runtime
            .as_mut()
            .context("failure runtime is missing")?;
        if let Err(error) =
            self.finish_initial_checkpoint(runtime, &mut transaction.state.interaction, result)
        {
            let message = format!("initial checkpoint failed during cleanup: {error:#}");
            if let Some(failure) = &mut transaction.failure {
                failure.secondary.push(message);
            } else {
                transaction.failure_secondary.push(message);
            }
        }
        Ok(ForegroundAction::Wait)
    }

    /// Publish rejection before releasing the captured admission.
    pub(crate) fn prepare_foreground_failure_publication(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        if admission.reset_check_setup {
            self.check_setup = false;
            admission.reset_check_setup = false;
        }
        if let Some(failure) = &mut admission.failure
            && !failure.admission_events.is_empty()
        {
            return Ok(ForegroundAction::Effect(
                ForegroundJob::FailureAdmissionEvents(std::mem::take(
                    &mut failure.admission_events,
                )),
            ));
        }
        if let Some(discussion) = admission.question_discussion.take()
            && let Err(error) = self.finish_question_discussion(discussion)
        {
            admission
                .failure
                .as_mut()
                .context("failure state missing")?
                .secondary
                .push(format!("restore question state: {error:#}"));
        }
        if let Some(failure) = &mut admission.failure
            && failure.original.downcast_ref::<TurnRetracted>().is_some()
            && !failure.retraction_prepared
        {
            failure.retraction_waiting = true;
            return Ok(ForegroundAction::WaitForPublicationDrain);
        }
        self.event_sink = admission.sink.clone();
        let staged_effects = admission
            .failure
            .as_mut()
            .and_then(|failure| failure.admission_effects.take());
        let retraction_effects = admission
            .failure
            .as_mut()
            .and_then(|failure| failure.retraction_effects.take());
        let effects = match retraction_effects
            .map(Ok)
            .unwrap_or_else(|| self.prepare_prompt_admission(false))
        {
            Ok(effects) => {
                let failure = admission
                    .failure
                    .as_ref()
                    .context("failure state missing")?;
                if failure.original.downcast_ref::<TurnRetracted>().is_some() {
                    effects
                } else {
                    let diagnostic = crate::provider::ProviderDiagnostic {
                        code: None,
                        message: format!("{:#}", failure.original),
                        fatal: failure.original.downcast_ref::<TurnCancelled>().is_none(),
                    };
                    AppliedProviderEvent::Effects(vec![
                        effects,
                        AppliedProviderEvent::Update(SessionUpdate::Diagnostic { diagnostic }),
                    ])
                }
            }
            Err(error) => {
                admission
                    .failure
                    .as_mut()
                    .context("failure state missing")?
                    .secondary
                    .push(format!("reject prompt admission: {error:#}"));
                return Ok(ForegroundAction::WaitForPublicationDrain);
            }
        };
        let effects = match staged_effects {
            Some(staged) => AppliedProviderEvent::Effects(vec![staged, effects]),
            None => effects,
        };
        match self.prepare_provider_delivery(effects) {
            Ok(delivery) => Ok(ForegroundAction::CancelJobsAndEffect(
                ForegroundJob::FailurePublication(delivery),
            )),
            Err(error) => {
                admission
                    .failure
                    .as_mut()
                    .context("failure state missing")?
                    .secondary
                    .push(format!("prepare failure publication: {error:#}"));
                Ok(ForegroundAction::WaitForPublicationDrain)
            }
        }
    }

    /// Continue failure publication after explicit admission events have been forwarded.
    pub(crate) fn foreground_failure_admission_events_forwarded(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => {
                anyhow::bail!("admission event forwarding has no failure owner")
            }
        };
        if let Err(error) = result {
            admission
                .failure
                .as_mut()
                .context("failure state missing")?
                .secondary
                .push(format!("forward admission events: {error:#}"));
        }
        self.prepare_foreground_failure_publication(admission)
    }

    /// Restore a retracted host snapshot after every already-started service job has joined.
    fn prepare_foreground_retraction(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<ForegroundAction> {
        let mut effects = Vec::new();
        match self.prepare_prompt_admission(false) {
            Ok(effect) => effects.push(effect),
            Err(error) => admission
                .failure
                .as_mut()
                .unwrap()
                .secondary
                .push(format!("reject prompt admission: {error:#}")),
        }
        let mut goal = None;
        if let Some(PromptSubmission {
            snapshot,
            submitted_prompt,
        }) = admission.submission.take()
        {
            match self.prepare_retracted_control_restore(snapshot) {
                Ok(job) => goal = job,
                Err(error) => admission
                    .failure
                    .as_mut()
                    .unwrap()
                    .secondary
                    .push(format!("restore retracted control state: {error:#}")),
            }
            if let Some((document, token)) = submitted_prompt {
                let transition = (|| -> Result<_> {
                    if let Some(document) = document {
                        self.presentation
                            .lock()
                            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                            .retract_submission(&document, token)
                    } else {
                        Ok(Some(
                            json!({"document":null,"token":token,"state":"retracted"}),
                        ))
                    }
                })();
                match transition {
                    Ok(Some(transition)) => {
                        match serde_json::from_value(transition) {
                            Ok(transition) => effects.push(AppliedProviderEvent::Update(
                                SessionUpdate::Submission { transition },
                            )),
                            Err(error) => admission
                                .failure
                                .as_mut()
                                .unwrap()
                                .secondary
                                .push(format!("decode retracted submission: {error}")),
                        }
                    }
                    Ok(None) => {}
                    Err(error) => admission
                        .failure
                        .as_mut()
                        .unwrap()
                        .secondary
                        .push(format!("retract submission: {error:#}")),
                }
            }
        } else {
            admission
                .failure
                .as_mut()
                .unwrap()
                .secondary
                .push("retracted prompt snapshot is missing".into());
        }
        let failure = admission
            .failure
            .as_mut()
            .context("failure state missing")?;
        failure.retraction_waiting = false;
        failure.retraction_prepared = true;
        failure.retraction_effects = Some(AppliedProviderEvent::Effects(effects));
        match goal {
            Some(job) => Ok(ForegroundAction::Effect(ForegroundJob::RetractionGoal(
                Box::new(job),
            ))),
            None => self.prepare_foreground_failure_publication(admission),
        }
    }

    /// Preserve the original retraction even if restoring the native goal fails.
    pub(crate) fn complete_foreground_retraction_goal(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => {
                anyhow::bail!("retraction goal has no foreground owner")
            }
        };
        if let Err(error) = result {
            admission
                .failure
                .as_mut()
                .context("failure state missing")?
                .secondary
                .push(format!("restore native goal: {error:#}"));
        }
        self.prepare_foreground_failure_publication(admission)
    }

    /// Record flush completion and wait until service-owned effects have joined.
    pub(crate) fn foreground_failure_published(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<Vec<SessionEvent>>,
    ) -> Result<ForegroundAction> {
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => {
                anyhow::bail!("failure publication has no owner")
            }
        };
        let failure = admission
            .failure
            .as_mut()
            .context("failure state missing")?;
        match result {
            Ok(events) => admission.events.extend(events),
            Err(error) => failure
                .secondary
                .push(format!("failure publication: {error:#}")),
        }
        Ok(ForegroundAction::WaitForPublicationDrain)
    }

    /// Release the owner only after cleanup, presentation and service job collection.
    pub(crate) fn finish_foreground_failure(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => {
                anyhow::bail!("failure drain has no owner")
            }
        };
        if admission
            .failure
            .as_ref()
            .is_some_and(|failure| failure.admission_cleanup_waiting)
        {
            return self.begin_staged_admission_cleanup(admission);
        }
        if admission
            .failure
            .as_ref()
            .is_some_and(|failure| failure.retraction_waiting)
        {
            return self.prepare_foreground_retraction(admission);
        }
        let idle_scope = match &*dispatch {
            ForegroundDispatch::Finalizing {
                transaction,
                admission,
            } if admission.idle_reopen => Some(transaction.state.scope.clone()),
            _ => None,
        };
        if let Some(scope) = idle_scope {
            self.reopened_execution.end_settlement(&scope);
        }
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            _ => unreachable!("failure owner checked above"),
        };
        let failure = admission.failure.take().context("failure state missing")?;
        if admission.automatic_check_owner.is_none() {
            self.event_sink = None;
        }
        let error = if failure.cleanup.is_some() || !failure.secondary.is_empty() {
            let original_message = format!("{:#}", failure.original);
            failure.original.context(ForegroundFailureContext {
                original_message,
                cleanup: failure.cleanup,
                secondary: failure.secondary,
            })
        } else {
            failure.original
        };
        Ok(ForegroundAction::FinishFailure(error))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct BaselineExit(Option<tokio::sync::oneshot::Sender<()>>);

    impl Drop for BaselineExit {
        fn drop(&mut self) {
            if let Some(sender) = self.0.take() {
                let _ = sender.send(());
            }
        }
    }

    /// A retained cancellation joins the baseline, including when its waiter disappears.
    #[tokio::test]
    async fn cancelled_baseline_is_joined_before_retention_or_waiter_release() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let registry = broker.execution_registry();
        for abandoned in [false, true] {
            let (started, ready) = tokio::sync::oneshot::channel();
            let (exited, mut exit) = tokio::sync::oneshot::channel();
            let task = tokio::spawn(async move {
                let _exit = BaselineExit(Some(exited));
                let _ = started.send(());
                std::future::pending::<Result<crate::checkpoint::CheckpointRecord>>().await
            });
            ready.await?;
            let job = CancelledTurnRollbackJob {
                provider: broker.provider.clone(),
                session_id: broker.session.id.clone(),
                exchange_id: "cancelled-baseline".into(),
                baseline: CancelledBaseline::new(
                    Some(crate::checkpoint::CheckpointTask(task)),
                    registry.clone(),
                ),
                policy: CancelledTurnRollbackPolicy::Retain,
            };
            if abandoned {
                drop(job);
                tokio::time::timeout(
                    std::time::Duration::from_secs(2),
                    registry.collect_cleanup(),
                )
                .await??;
            } else {
                let result =
                    tokio::time::timeout(std::time::Duration::from_secs(2), job.run()).await?;
                assert!(matches!(result.outcome?, CancelledTurnRollback::Retained));
            }
            assert_eq!(
                exit.try_recv(),
                Ok(()),
                "baseline was released without joining its task"
            );
        }
        Ok(())
    }

    /// Cleanup failure must not replace the failure that caused the owner to close.
    #[test]
    fn report_retains_original_and_typed_cleanup_failure() {
        let report = anyhow::Error::new(TurnCancelled).context(ForegroundFailureContext {
            original_message: "turn cancelled".into(),
            cleanup: Some(ProviderError::Protocol {
                message: "cleanup failed".into(),
            }),
            secondary: Vec::new(),
        });
        assert!(report.downcast_ref::<TurnCancelled>().is_some());
        assert!(matches!(
            report
                .downcast_ref::<ForegroundFailureContext>()
                .and_then(|context| context.cleanup.as_ref()),
            Some(ProviderError::Protocol { .. })
        ));
        assert!(report.to_string().contains("cleanup failed"));
        let retracted = anyhow::Error::new(TurnRetracted {
            prompt: "retracted prompt".into(),
        })
        .context(ForegroundFailureContext {
            original_message: "Output-free turn retracted".into(),
            cleanup: Some(ProviderError::Protocol {
                message: "goal restore failed".into(),
            }),
            secondary: Vec::new(),
        });
        assert!(retracted.downcast_ref::<TurnRetracted>().is_some());
        assert!(matches!(
            retracted
                .downcast_ref::<ForegroundFailureContext>()
                .and_then(|context| context.cleanup.as_ref()),
            Some(ProviderError::Protocol { .. })
        ));
    }

    /// A retracted submission restores its snapshot and is published before its typed error.
    #[tokio::test]
    async fn retraction_restores_snapshot_and_flushes_submission_before_error() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.session.provider_session_id = None;
        let original_model = broker.session.resolved_model.clone();
        let (sink, mut stream) = publication::channel();
        let request = Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"retract me", "_prompt_admission":{"document":null,"token":17}}),
        };
        let mut dispatch = broker.begin_foreground_dispatch(request, sink)?;
        broker.session.resolved_model = Some("temporary-model".into());
        let action = broker.fail_foreground_dispatch(
            &mut dispatch,
            TurnRetracted {
                prompt: "retract me".into(),
            }
            .into(),
        )?;
        assert!(matches!(action, ForegroundAction::WaitForPublicationDrain));
        assert!(matches!(
            broker.finish_foreground_failure(&mut dispatch)?,
            ForegroundAction::WaitForPublicationDrain
        ));
        let action = broker.finish_foreground_failure(&mut dispatch)?;
        assert_eq!(broker.session.resolved_model, original_model);
        let ForegroundAction::CancelJobsAndEffect(job) = action else {
            panic!("missing retraction publication");
        };
        let publication = tokio::spawn(job.run());
        let first = stream
            .recv()
            .await?
            .context("missing rejected submission")?;
        let second = stream
            .recv()
            .await?
            .context("missing retracted submission")?;
        assert!(matches!(
            first,
            SessionUpdate::Submission {
                transition: publication::SubmissionUpdate {
                    state: publication::SubmissionState::Rejected,
                    ..
                }
            }
        ));
        assert!(matches!(
            second,
            SessionUpdate::Submission {
                transition: publication::SubmissionUpdate {
                    state: publication::SubmissionState::Retracted,
                    ..
                }
            }
        ));
        let drain = tokio::spawn(async move { stream.recv().await });
        let signal = publication.await??;
        drain.abort();
        let ForegroundSignal::FailurePublicationCompleted(result) = signal else {
            panic!("wrong publication result");
        };
        assert!(matches!(
            broker.foreground_failure_published(&mut dispatch, result)?,
            ForegroundAction::WaitForPublicationDrain
        ));
        let ForegroundAction::FinishFailure(error) =
            broker.finish_foreground_failure(&mut dispatch)?
        else {
            panic!("missing retraction error");
        };
        assert!(error.downcast_ref::<TurnRetracted>().is_some());
        Ok(())
    }

    /// Native goal restore failure remains secondary to the original retraction.
    #[tokio::test]
    async fn retraction_goal_failure_preserves_original_and_stages_retracted_publication()
    -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.capability.native_goal = true;
        broker.session.provider_session_id = Some("native-session".into());
        let (sink, _stream) = publication::channel();
        let request = Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"retract me", "_prompt_admission":{"document":null,"token":18}}),
        };
        let mut dispatch = broker.begin_foreground_dispatch(request, sink)?;
        assert!(matches!(
            broker.fail_foreground_dispatch(
                &mut dispatch,
                TurnRetracted {
                    prompt: "retract me".into()
                }
                .into()
            )?,
            ForegroundAction::WaitForPublicationDrain
        ));
        assert!(matches!(
            broker.finish_foreground_failure(&mut dispatch)?,
            ForegroundAction::WaitForPublicationDrain
        ));
        assert!(matches!(
            broker.finish_foreground_failure(&mut dispatch)?,
            ForegroundAction::Effect(ForegroundJob::RetractionGoal(_))
        ));
        let action = broker.complete_foreground_retraction_goal(
            &mut dispatch,
            Err(anyhow::anyhow!("goal restore unavailable")),
        )?;
        assert!(matches!(
            action,
            ForegroundAction::CancelJobsAndEffect(ForegroundJob::FailurePublication(_))
        ));
        let ForegroundDispatch::Admitting(admission) = &dispatch else {
            panic!("lost admission");
        };
        let failure = admission.failure.as_ref().context("missing failure")?;
        assert!(failure.original.downcast_ref::<TurnRetracted>().is_some());
        assert!(
            failure
                .secondary
                .iter()
                .any(|error| error.contains("goal restore unavailable"))
        );
        Ok(())
    }

    /// A failure after durable admission collects its owner and settles the staged exchange.
    #[tokio::test]
    async fn staged_admission_failure_settles_exchange_before_rejection() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        let (sink, _stream) = publication::channel();
        let request = Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({"text":"staged"}),
        };
        let mut dispatch = broker.begin_foreground_dispatch(request, sink)?;
        let staged = broker.stage_foreground_interaction(
            "staged",
            PromptMode::Chat,
            Some(&ExchangeAdmission::chat("staged".into())),
            ProviderInput::from_text("staged"),
        )?;
        let exchange_id = staged.exchange.id.clone();
        let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
            panic!("missing admission");
        };
        admission.staged_interaction = Some(staged);
        admission.reset_check_setup = true;
        broker.check_setup = true;
        broker.activate_staged_runtime(admission)?;
        assert!(matches!(
            broker.fail_foreground_dispatch(
                &mut dispatch,
                anyhow::anyhow!("provider preparation failed")
            )?,
            ForegroundAction::WaitForPublicationDrain
        ));
        let ForegroundAction::Effect(ForegroundJob::AdmissionCleanup(job)) =
            broker.finish_foreground_failure(&mut dispatch)?
        else {
            panic!("missing owned cleanup");
        };
        let result = job.run().await;
        let action = broker.complete_staged_admission_cleanup(&mut dispatch, result)?;
        assert!(matches!(
            action,
            ForegroundAction::CancelJobsAndEffect(ForegroundJob::FailurePublication(_))
                | ForegroundAction::Effect(ForegroundJob::FailureAdmissionEvents(_))
        ));
        let exchange = broker
            .store
            .load_exchange(&exchange_id)?
            .context("staged exchange disappeared")?;
        assert_eq!(exchange.state(), ExchangeState::Failed);
        assert!(!broker.check_setup);
        assert!(broker.exchange_runtime.is_none());
        Ok(())
    }
}
