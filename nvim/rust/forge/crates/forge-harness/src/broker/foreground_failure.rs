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
    baseline_cleanup: FailureBaselinePhase,
    settlement_commit: FailureSettlementCommit,
}

/// Track who returns a failed invocation's native settlement to its execution owner.
///
/// Engine closure discards every retained invocation, but scoped collection leaves the stopped
/// invocation registered until its ticket commits. An uncommitted ticket keeps the owner
/// occupied, so the next prompt would fail with "execution owner is unavailable or already
/// active" although the failure settled cleanly.
enum FailureSettlementCommit {
    /// Engine closure or an undispatched admission leaves no retained invocation.
    Closed,
    /// Scoped collection stopped the invocation; failure settlement must commit its ticket.
    Collected,
    /// The settlement commit job owns the ticket.
    Pending,
    /// The commit reported its result, or no ticket remained to commit.
    Complete,
}

enum FailureBaselinePhase {
    Unexamined,
    Pending,
    Complete,
}

/// Collect a failed pre-baseline owner before optional cancellation rollback, without the broker lock.
pub(crate) struct FailureBaselineJob {
    provider: Arc<Provider>,
    exchange_id: String,
    baseline: AbortedBaseline,
    policy: FailureBaselinePolicy,
}

enum FailureBaselinePolicy {
    Retain,
    NeverConsumed {
        submission: Arc<crate::provider::submission::Submission>,
        attempt: forge_provider_api::DeliveryAttemptId,
    },
    Retract {
        context: forge_provider_api::OperationContext,
        request: forge_provider_api::InputRetractRequest,
    },
}

enum BaselineDisposition {
    Retracted,
    Retained,
}

/// Carry the captured exchange identity and collection/rollback result back to its owner.
pub(crate) struct FailureBaselineResult {
    exchange_id: String,
    outcome: Result<BaselineDisposition>,
}

/// Transfer an uncollected baseline to the registry if its foreground waiter disappears.
struct AbortedBaseline {
    task: Option<crate::checkpoint::CheckpointTask>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
}

impl AbortedBaseline {
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
    async fn collect(&mut self) -> std::result::Result<(), crate::checkpoint::CheckpointFailure> {
        let Some(task) = self.task.as_mut() else {
            return Ok(());
        };
        match tokio::time::timeout(std::time::Duration::from_secs(30), Self::join(task)).await {
            Ok(result) => {
                self.task.take();
                result
            }
            Err(source) => Err(crate::checkpoint::CheckpointFailure::CollectionDeadline { source }),
        }
    }

    /// Treat requested task cancellation as collected, while preserving worker failures.
    async fn join(
        task: &mut crate::checkpoint::CheckpointTask,
    ) -> std::result::Result<(), crate::checkpoint::CheckpointFailure> {
        match (&mut task.0).await {
            Ok(result) => result.map(|_| ()),
            Err(error) if error.is_cancelled() => Ok(()),
            Err(source) => Err(crate::checkpoint::CheckpointFailure::WorkerJoin { source }),
        }
    }
}

impl Drop for AbortedBaseline {
    fn drop(&mut self) {
        let Some(mut task) = self.task.take() else {
            return;
        };
        if let Err(error) = self.registry.retain_cleanup(
            crate::provider::session_execution::CleanupWorkKind::CheckpointCollection,
            async move {
                Self::join(&mut task)
                    .await
                    .map_err(crate::provider::session_execution::CleanupWorkFailure::Checkpoint)
            },
        ) {
            self.registry.record_cleanup_failure(error);
        }
    }
}

impl FailureBaselineJob {
    /// Join the aborted baseline before requesting provider rollback or releasing the host.
    pub(crate) async fn run(mut self) -> FailureBaselineResult {
        let outcome = async {
            self.baseline.collect().await?;
            match self.policy {
                FailureBaselinePolicy::Retain => Ok(BaselineDisposition::Retained),
                FailureBaselinePolicy::NeverConsumed {
                    submission,
                    attempt,
                } => {
                    let record = submission.record()?;
                    Ok(
                        if record.attempt_id == attempt
                            && record.delivery
                                == crate::provider::submission::SubmissionState::Cancelled
                        {
                            BaselineDisposition::Retracted
                        } else {
                            BaselineDisposition::Retained
                        },
                    )
                }
                FailureBaselinePolicy::Retract { context, request } => {
                    let owner = request.scope.owner().clone();
                    let outcome = tokio::time::timeout(
                        std::time::Duration::from_secs(30),
                        self.provider.input_retract(context, &owner, request),
                    )
                    .await
                    .context("native cancelled-turn rollback timed out")??;
                    Ok(match outcome {
                        forge_provider_api::InputRetractOutcome::Retracted { .. }
                        | forge_provider_api::InputRetractOutcome::NeverConsumed => {
                            BaselineDisposition::Retracted
                        }
                        forge_provider_api::InputRetractOutcome::Unavailable(_)
                        | forge_provider_api::InputRetractOutcome::Unknown => {
                            BaselineDisposition::Retained
                        }
                    })
                }
            }
        }
        .await;
        FailureBaselineResult {
            exchange_id: self.exchange_id,
            outcome,
        }
    }
}

/// Own the staged admission's provider fence and activated baseline worker.
pub(crate) struct AdmissionCleanupJob {
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
    baseline: AdmissionBaseline,
    native: AdmissionNativeCollection,
}

/// Carry native collection and the staged baseline's exact disposition to its host owner.
pub(crate) struct AdmissionCleanupResult {
    engine: Result<(), ProviderError>,
    baseline: AdmissionBaselineResult,
}

#[derive(Clone, Copy)]
enum AdmissionNativeCollection {
    Required,
    OwnerCollected,
}

enum AdmissionBaseline {
    Capture(Option<crate::checkpoint::CheckpointTask>),
    Abort(AbortedBaseline),
}

enum AdmissionBaselineResult {
    Captured(Option<Result<crate::checkpoint::CheckpointRecord>>),
    Aborted(std::result::Result<(), crate::checkpoint::CheckpointFailure>),
}

impl AdmissionCleanupJob {
    /// Fence the staged native owner and collect its captured or explicitly aborted baseline.
    pub(crate) async fn run(mut self) -> AdmissionCleanupResult {
        let baseline = async {
            match &mut self.baseline {
                AdmissionBaseline::Abort(task) => {
                    AdmissionBaselineResult::Aborted(task.collect().await)
                }
                AdmissionBaseline::Capture(task) => {
                    AdmissionBaselineResult::Captured(match task.as_mut() {
                        Some(task) => Some(
                            (&mut task.0)
                                .await
                                .context("admission checkpoint worker failed")
                                .and_then(|result| result.map_err(anyhow::Error::new)),
                        ),
                        None => None,
                    })
                }
            }
        };
        let native = async {
            match self.native {
                AdmissionNativeCollection::Required => self.registry.fence(&self.owner).await,
                AdmissionNativeCollection::OwnerCollected => Ok(()),
            }
        };
        let (engine, baseline) = tokio::join!(native, baseline);
        AdmissionCleanupResult { engine, baseline }
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
        self.trace.record(&self.session.id, "exchange.foreground.failure_classified", json!({
            "exchange_id":transaction.state.scope.exchange_id,
            "error_code": if error.is::<crate::task::StaleTask>() { "task_authority_revoked" }
                else if error.is::<TurnCancelled>() { "turn_cancelled" }
                else if error.downcast_ref::<ProviderError>().is_some_and(ProviderError::is_clean_cancellation) { "provider_cancelled" }
                else { "foreground_failure" },
            "reason": format!("{error:#}"),
        }));
        // Cancellation revokes the old task authority before its final worker results drain.
        // Preserve scoped cancellation instead of closing the binding for that expected revocation.
        let error = if matches!(
            transaction.collection,
            Some(ForegroundCollection::Cancel(_))
        ) && (error.is::<crate::task::StaleTask>()
            || error
                .downcast_ref::<ProviderError>()
                .is_some_and(ProviderError::is_clean_cancellation))
        {
            TurnCancelled.into()
        } else {
            error
        };
        if error.is::<TurnCancelled>() && transaction.collection.is_none() {
            transaction.collection = Some(ForegroundCollection::Cancel(
                crate::exchange::command::CancelledInputPolicy::RestoreIfUnconsumed,
            ));
        }
        transaction.fail_live_host_observer(&self.trace, &self.session.id);
        if transaction.failure_cleanup_active {
            if error.is::<TurnCancelled>()
                && matches!(
                    transaction.collection,
                    Some(ForegroundCollection::Cancel(_))
                )
            {
                return Ok(ForegroundAction::Wait);
            }
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
                trace: self.trace.clone(),
                scope: transaction.state.scope.clone(),
            },
        ))
    }

    /// Settle a cancellation the exchange owner already collected, restoring an output-free prompt.
    ///
    /// The owner's collection stopped the native invocation, so a second cleanup would retire
    /// its reader early. Failure settlement therefore starts with that collection as its
    /// successful cleanup, and `advance_foreground_failure` makes the restore-or-finalize decision.
    pub(crate) fn settle_collected_cancellation(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<ForegroundAction> {
        let ForegroundDispatch::Running { transaction, .. } = dispatch else {
            anyhow::bail!("collected cancellation has no running foreground owner");
        };
        transaction.fail_live_host_observer(&self.trace, &self.session.id);
        transaction.failure_cleanup_active = true;
        transaction.failure_inbox_open = true;
        self.complete_foreground_cleanup(dispatch, TurnCancelled.into(), Ok(()))
    }

    /// Decide whether foreground cleanup stops only the failed exchange instead of closing its engine.
    ///
    /// Cancellation and an elapsed tool deadline leave the session usable, so cleanup collects the
    /// exchange's native turn through its scope and failure settlement later commits that turn's
    /// ticket. Every other failure closes the engine.
    pub(crate) fn collects_exchange_scope(original: &anyhow::Error) -> bool {
        original.is::<TurnCancelled>()
            || original.is::<crate::provider::tool_timeout::ToolTimeout>()
    }

    /// Commit a scoped-collected invocation's settlement after failure settlement drains.
    ///
    /// Run only once every failure checkpoint and publication has settled, which is when the
    /// clean finalization path commits its own ticket. Skip the commit when no ticket remains.
    fn begin_failure_settlement_commit(
        &mut self,
        dispatch: &mut ForegroundDispatch,
    ) -> Result<Option<ForegroundAction>> {
        let (ForegroundDispatch::Running {
            transaction,
            admission,
        }
        | ForegroundDispatch::Finalizing {
            transaction,
            admission,
        }) = dispatch
        else {
            return Ok(None);
        };
        let Some(failure) = admission.failure.as_mut() else {
            return Ok(None);
        };
        if !matches!(
            failure.settlement_commit,
            FailureSettlementCommit::Collected
        ) || failure.cleanup.is_some()
        {
            return Ok(None);
        }
        failure.settlement_commit = FailureSettlementCommit::Complete;
        let cell = transaction.settlement.clone();
        let prepared = (|| -> Result<bool, ProviderError> {
            if cell.checkout()?.ticket().is_none() {
                return Ok(false);
            }
            cell.seal_effects(&[])?;
            Ok(true)
        })();
        match prepared {
            Ok(false) => Ok(None),
            Ok(true) => {
                failure.settlement_commit = FailureSettlementCommit::Pending;
                Ok(Some(ForegroundAction::Effect(
                    ForegroundJob::SettlementCommit {
                        cell,
                        requirement: SettlementRequirement::Native,
                        undispatched: Vec::new(),
                    },
                )))
            }
            Err(error) => {
                failure
                    .secondary
                    .push(format!("prepare collected settlement commit: {error:#}"));
                Ok(None)
            }
        }
    }

    /// Record the scoped-collected settlement commit and resume failure settlement.
    pub(crate) fn complete_failure_settlement_commit(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: Result<(), crate::provider::session_execution::SettlementError>,
    ) -> Result<ForegroundAction> {
        let failure = match dispatch {
            ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission.failure.as_mut(),
            ForegroundDispatch::Admitting(_)
            | ForegroundDispatch::Done(_)
            | ForegroundDispatch::Transitioning => None,
        }
        .context("settlement commit completed without its failure owner")?;
        anyhow::ensure!(
            matches!(failure.settlement_commit, FailureSettlementCommit::Pending),
            "unexpected failure settlement commit"
        );
        failure.settlement_commit = FailureSettlementCommit::Complete;
        if let Err(error) = result {
            failure
                .secondary
                .push(format!("commit collected settlement: {error}"));
        }
        self.finish_foreground_failure(dispatch)
    }

    /// Decide whether a cancellation may restore its prompt instead of persisting a partial exchange.
    ///
    /// Both owner-collected cancellation and foreground failure settlement consult this one rule,
    /// so a restoring Ctrl-C cannot depend on which path observed the native termination.
    pub(crate) fn cancellation_restores_prompt(transaction: &ForegroundTransaction) -> bool {
        transaction
            .state
            .runtime
            .as_ref()
            .is_some_and(|runtime| runtime.retraction_eligibility.is_output_free())
            && matches!(
                transaction.collection,
                Some(ForegroundCollection::Cancel(
                    crate::exchange::command::CancelledInputPolicy::RestoreIfUnconsumed
                ))
            )
    }

    /// Create the native rollback request for the exact cancelled submission and history boundary.
    fn output_free_retraction(
        transaction: &ForegroundTransaction,
    ) -> Result<Option<FailureBaselinePolicy>> {
        let Some((scope, boundary)) = &transaction.retraction_boundary else {
            return Ok(None);
        };
        let Some(submission) = &transaction.request.submission else {
            return Ok(None);
        };
        let Some(handoff) = transaction.request.dispatch.observer().current()? else {
            return Ok(None);
        };
        let record = submission.record()?;
        if scope != &transaction.state.scope
            || handoff.scope() != scope
            || handoff.attempt_id() != &record.attempt_id
            || !matches!(handoff.target(), forge_provider_api::submission::DispatchTarget::Input(id) if id == submission.receipt_id())
        {
            return Ok(None);
        }
        let operation = crate::provider::operation::OperationControl::new(
            forge_provider_api::ProviderOperation::InputRetract,
            forge_provider_api::OperationId::new(format!(
                "retract:{}:{}",
                submission.id(),
                record.attempt_id.as_str()
            ))?,
        )?;
        Ok(Some(FailureBaselinePolicy::Retract {
            context: operation.context(),
            request: forge_provider_api::InputRetractRequest {
                scope: scope.clone(),
                submission_id: submission.receipt_id().clone(),
                attempt_id: record.attempt_id,
                boundary: boundary.clone(),
                authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation,
            },
        }))
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
        let observation = crate::provider::HarnessEvent::Host {
            scope: expired.address.turn.scope.clone().into(),
            event: crate::provider::HostEvent::Failure {
                failure: forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::Timeout,
                ),
                message: forge_provider_api::SanitizedMessage::new(expired.to_string())
                    .expect("timeout diagnostics are nonempty"),
            },
        };
        transaction.pending_session_observations.push_back(
            crate::provider::session_execution::OwnerObservation::Event(observation),
        );
        self.fail_foreground(transaction, expired.into())
    }

    /// Start failure processing even if the request has not acquired a native owner.
    pub(crate) fn fail_foreground_dispatch(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        error: anyhow::Error,
    ) -> Result<ForegroundAction> {
        dispatch.fail_live_host_observer(&self.trace, &self.session.id);
        match dispatch {
            ForegroundDispatch::Admitting(admission) => {
                let error = if (admission.owner_collection_pending()
                    || admission.owner_collection_complete())
                    && error
                        .downcast_ref::<ProviderError>()
                        .is_some_and(ProviderError::is_clean_cancellation)
                {
                    TurnCancelled.into()
                } else {
                    error
                };
                if let Some(failure) = &mut admission.failure {
                    if failure.original.is::<TurnCancelled>() && error.is::<TurnCancelled>() {
                        return Ok(ForegroundAction::Wait);
                    }
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
                    baseline_cleanup: FailureBaselinePhase::Unexamined,
                    settlement_commit: FailureSettlementCommit::Closed,
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
        if admission.owner_collection_pending() {
            return Ok(ForegroundAction::Wait);
        }
        let native = if admission.owner_collection_complete() {
            AdmissionNativeCollection::OwnerCollected
        } else {
            AdmissionNativeCollection::Required
        };
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
        let registry = self.execution_registry();
        let baseline = match self.exchange_runtime.as_mut() {
            Some(runtime)
                if runtime.exchange_id == staged.exchange.id
                    && runtime.tools_blocked()
                    && runtime.checkpoint.is_some() =>
            {
                runtime
                    .tool_gate
                    .as_ref()
                    .context("blocked staged baseline has no tool gate")?
                    .complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
                AdmissionBaseline::Abort(AbortedBaseline::new(
                    runtime.checkpoint.take(),
                    registry.clone(),
                ))
            }
            Some(runtime) if runtime.exchange_id == staged.exchange.id => {
                AdmissionBaseline::Capture(runtime.checkpoint.take())
            }
            Some(_) => {
                failure
                    .secondary
                    .push("staged admission runtime belongs to another exchange".into());
                failure.admission_baseline_failed = true;
                AdmissionBaseline::Capture(None)
            }
            None => AdmissionBaseline::Capture(None),
        };
        failure.admission_cleanup_started = true;
        Ok(ForegroundAction::Effect(ForegroundJob::AdmissionCleanup(
            AdmissionCleanupJob {
                registry,
                owner: crate::provider::ThreadOwner::root(&self.session.id),
                baseline,
                native,
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
        let checkpoint = match result.baseline {
            AdmissionBaselineResult::Captured(checkpoint) => checkpoint,
            AdmissionBaselineResult::Aborted(collected) => {
                let staged = admission
                    .staged_interaction
                    .as_mut()
                    .context("failed staged baseline lost its exchange")?;
                if let Err(error) = collected {
                    failure
                        .secondary
                        .push(format!("collect failed admission baseline: {error}"));
                }
                let cancelled = failure.original.is::<TurnCancelled>();
                staged.exchange.set_awaiting_input(false);
                self.questions.cancel_chat(Some(&staged.exchange.id))?;
                staged.exchange.finalization_error =
                    (!cancelled).then(|| format!("{:#}", failure.original));
                staged.exchange.finish(
                    if cancelled {
                        crate::exchange::ExchangeOutcome::Cancelled
                    } else {
                        crate::exchange::ExchangeOutcome::Failed
                    },
                    self.clock.now_ms(),
                )?;
                self.store.save_exchange(&staged.exchange)?;
                None
            }
        };
        if let Some(checkpoint) = checkpoint {
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
            self.release_automatic_session_name(&staged.exchange.prompt)?;
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
                staged
                    .exchange
                    .settle_running_turns(outcome, self.clock.now_ms())?;
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
            let settlement_commit = if cleanup.is_ok() && Self::collects_exchange_scope(&original) {
                FailureSettlementCommit::Collected
            } else {
                FailureSettlementCommit::Closed
            };
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
                baseline_cleanup: FailureBaselinePhase::Unexamined,
                settlement_commit,
            });
        }
        if let Err(error) = self.collect_foreground_observations(transaction) {
            transaction
                .failure
                .as_mut()
                .expect("foreground cleanup installs its failure before draining observations")
                .secondary
                .push(format!("foreground observation drain failed: {error:#}"));
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
        if transaction.failure.is_none() {
            return Ok(ForegroundAction::Wait);
        }
        if transaction.pending_session_observations.is_empty()
            && let Err(error) = self.collect_foreground_observations(transaction)
        {
            transaction
                .failure
                .as_mut()
                .expect("foreground failure was checked before refilling observations")
                .secondary
                .push(format!("foreground observation drain failed: {error:#}"));
        }
        if !transaction.pending_session_observations.is_empty()
            || transaction.observation_publication_in_flight
        {
            return Ok(ForegroundAction::Wait);
        }
        let restores_prompt = Self::cancellation_restores_prompt(transaction);
        let native_retraction = if restores_prompt && self.capability.native_turn_rollback {
            Self::output_free_retraction(transaction)?
        } else {
            None
        };
        let failure = transaction.failure.as_mut().unwrap();
        self.trace.record(&self.session.id, "foreground.failure.advance", json!({
            "exchange_id":transaction.state.interaction.id,
            "cause":format!("{:#}",failure.original),
            "cleanup":failure.cleanup.as_ref().map(ToString::to_string),
            "checkpoint_pending":failure.checkpoint_pending,
            "checkpoint_done":failure.checkpoint_done,
            "baseline_pending":transaction.state.runtime.as_ref().is_some_and(|runtime| runtime.checkpoint.is_some()),
            "tools_blocked":transaction.state.runtime.as_ref().is_some_and(ExchangeRuntime::tools_blocked),
            "retraction_eligible":transaction.state.runtime.as_ref().is_some_and(|runtime| runtime.retraction_eligibility.is_output_free()),
            "native_rollback":self.capability.native_turn_rollback,
            "restore_unconsumed_prompt":matches!(transaction.collection, Some(ForegroundCollection::Cancel(crate::exchange::command::CancelledInputPolicy::RestoreIfUnconsumed))),
        }));
        if matches!(failure.baseline_cleanup, FailureBaselinePhase::Pending) {
            return Ok(ForegroundAction::Wait);
        }
        // Restoration deletes the provisional exchange, and the exchange owner validates that
        // exchange when its collection completes. `CollectionCompleted` resumes this decision.
        if restores_prompt && admission.owner_collection_pending() {
            return Ok(ForegroundAction::Wait);
        }
        let restoration_authorized =
            failure.original.is::<TurnCancelled>() && failure.cleanup.is_none() && restores_prompt;
        let never_consumed = if restoration_authorized
            && matches!(failure.baseline_cleanup, FailureBaselinePhase::Unexamined)
            && let Some(submission) = &transaction.request.submission
        {
            if matches!(
                submission.retire_prepared()?,
                crate::provider::submission::SubmissionRetirement::Cancelled
            ) {
                Some((submission.clone(), submission.record()?.attempt_id))
            } else {
                None
            }
        } else {
            None
        };
        let native_retraction = native_retraction.filter(|_| {
            restoration_authorized
                && never_consumed.is_none()
                && matches!(failure.baseline_cleanup, FailureBaselinePhase::Unexamined)
        });
        let cancelled = failure.original.is::<TurnCancelled>();
        let (baseline_blocked, baseline_committed) =
            transaction
                .state
                .runtime
                .as_ref()
                .map_or((false, false), |runtime| {
                    (
                        runtime.tools_blocked() && (runtime.checkpoint.is_some() || cancelled),
                        !runtime.tools_blocked() && runtime.checkpoint.is_none(),
                    )
                });
        // A committed baseline still permits retraction of an output-free prompt, but only an
        // actual native rollback request may bypass the ordinary cancelled checkpoint.
        let retract_after_baseline = native_retraction.is_some() && baseline_committed;
        if matches!(failure.baseline_cleanup, FailureBaselinePhase::Unexamined)
            && (never_consumed.is_some() || baseline_blocked || retract_after_baseline)
        {
            let runtime = transaction
                .state
                .runtime
                .as_mut()
                .context("failed pre-baseline execution requires its checkpoint runtime")?;
            // This denied gate proves no provider tool could pass the baseline,
            // even when native collection reported failure. Collect its owned
            // worker independently; native cleanup failure still forbids rollback.
            if let Some(gate) = &runtime.tool_gate {
                gate.complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
            }
            // Retain checkpoint collection even when journal retirement proves no native input was sent.
            let baseline =
                AbortedBaseline::new(runtime.checkpoint.take(), self.execution_registry());
            let settles_now = never_consumed.is_some() || baseline_blocked;
            let policy = match (never_consumed, native_retraction) {
                (Some((submission, attempt)), _) => FailureBaselinePolicy::NeverConsumed {
                    submission,
                    attempt,
                },
                (None, Some(policy)) => policy,
                (None, None) => FailureBaselinePolicy::Retain,
            };
            failure.baseline_cleanup = FailureBaselinePhase::Pending;
            // After a committed baseline, a retained rollback result continues through the
            // ordinary cancelled checkpoint, so only an aborted baseline settles the exchange here.
            if !settles_now {
                return Ok(ForegroundAction::Effect(ForegroundJob::FailureBaseline(
                    FailureBaselineJob {
                        provider: self.provider.clone(),
                        exchange_id: transaction.state.interaction.id.clone(),
                        baseline,
                        policy,
                    },
                )));
            }
            let interaction = &mut transaction.state.interaction;
            self.questions.cancel_chat(Some(&interaction.id))?;
            interaction.set_awaiting_input(false);
            interaction.finalization_error = match &failure.cleanup {
                Some(cleanup) => Some(format!(
                    "{:#}; native cleanup failed: {cleanup}",
                    failure.original
                )),
                None => (!cancelled).then(|| format!("{:#}", failure.original)),
            };
            interaction.finish(
                if cancelled {
                    crate::exchange::ExchangeOutcome::Cancelled
                } else {
                    crate::exchange::ExchangeOutcome::Failed
                },
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(interaction)?;
            failure.checkpoint_done = true;
            return Ok(ForegroundAction::Effect(ForegroundJob::FailureBaseline(
                FailureBaselineJob {
                    provider: self.provider.clone(),
                    exchange_id: interaction.id.clone(),
                    baseline,
                    policy,
                },
            )));
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
                let outcome = if failure.original.downcast_ref::<TurnCancelled>().is_some() {
                    crate::exchange::ExchangeOutcome::Cancelled
                } else {
                    crate::exchange::ExchangeOutcome::Failed
                };
                let prepared: Result<Option<FinalCheckpointJob>> = (|| {
                    interaction.begin_finalization(outcome, self.clock.now_ms())?;
                    // Native cleanup has joined; a terminal it dropped will not arrive later.
                    interaction.settle_running_turns(outcome, self.clock.now_ms())?;
                    self.store.save_exchange(interaction)?;
                    if let Some(scope) = interaction
                        .turn
                        .last()
                        .map(|turn| turn.provider().scope.clone())
                    {
                        self.host_settlement.begin_settlement(&scope);
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
                            self.host_settlement.end_settlement(&scope);
                        }
                        failure.checkpoint_done = true;
                    }
                    Err(error) => {
                        if let Some(scope) = interaction
                            .turn
                            .last()
                            .map(|turn| turn.provider().scope.clone())
                        {
                            self.host_settlement.end_settlement(&scope);
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

    /// Apply collection or cancellation proof only to the failed exchange that owns the baseline.
    pub(crate) fn complete_failure_baseline(
        &mut self,
        dispatch: &mut ForegroundDispatch,
        result: FailureBaselineResult,
    ) -> Result<ForegroundAction> {
        let transaction = match dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => transaction,
            _ => anyhow::bail!("baseline collection has no foreground owner"),
        };
        let failure = transaction
            .failure
            .as_mut()
            .context("baseline collection failure owner is missing")?;
        anyhow::ensure!(
            matches!(failure.baseline_cleanup, FailureBaselinePhase::Pending)
                && transaction.state.interaction.id == result.exchange_id,
            "baseline collection result crosses foreground ownership"
        );
        failure.baseline_cleanup = FailureBaselinePhase::Complete;
        match result.outcome {
            Ok(BaselineDisposition::Retracted) => {
                self.store.delete_exchange(&result.exchange_id)?;
                self.release_automatic_session_name(&transaction.state.interaction.prompt)?;
                self.retracted_exchange_id = Some(result.exchange_id.clone());
                transaction.state.runtime.take();
                failure.original = TurnRetracted {
                    prompt: transaction.state.interaction.prompt.clone(),
                }
                .into();
                failure.admission_effects =
                    Some(AppliedProviderEvent::Timeline(self.reconcile_timeline()?));
            }
            Ok(BaselineDisposition::Retained) => {}
            Err(error) => failure.secondary.push(format!(
                "collect failed baseline or roll back cancelled native turn: {error:#}"
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
                    self.host_settlement.end_settlement(&scope);
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
                    self.host_settlement.end_settlement(&scope);
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
                    self.host_settlement.end_settlement(&scope);
                }
                failure.checkpoint_done = true;
            }
        }
        self.advance_foreground_failure(dispatch)
    }

    /// Join the baseline worker without admitting any successor during cleanup.
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
                if failure.original.downcast_ref::<TurnRetracted>().is_some()
                    || (failure.original.is::<TurnCancelled>()
                        && admission.owner_collection_restarts())
                {
                    effects
                } else {
                    let diagnostic = super::publication::DiagnosticView {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new(
                            crate::storage::error_message(&failure.original),
                        )
                        .expect("host diagnostic is nonempty"),
                        severity: super::publication::DiagnosticSeverity::Error,
                        scope: crate::provider::EventScope::Binding {
                            thread_id: forge_provider_api::ThreadId::from(
                                crate::provider::ThreadOwner::root(&self.session.id),
                            ),
                        },
                        native_scope: forge_provider_api::WarningScope::Provider,
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
        if let Some(PromptSubmission {
            snapshot,
            submitted_prompt,
        }) = admission.submission.take()
        {
            match self.prepare_retracted_control_restore(snapshot) {
                Ok(()) => {}
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
        if let Some(action) = self.begin_failure_settlement_commit(dispatch)? {
            return Ok(action);
        }
        let restarting = match &*dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => {
                admission.owner_collection_restarts()
            }
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => false,
        };
        let failed_task = match &*dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => {
                transaction.dispatched_task_permit.clone()
            }
            ForegroundDispatch::Admitting(admission) => match &admission.purpose {
                crate::input::InputPurpose::TaskEntry { revision, .. }
                | crate::input::InputPurpose::TaskContinuation { revision, .. } => self
                    .task_controller
                    .current()?
                    .filter(|task| &task.revision == revision)
                    .map(|task| task.permit()),
                _ => None,
            },
            ForegroundDispatch::Done(_) | ForegroundDispatch::Transitioning => None,
        };
        if !restarting
            && let Some(permit) = failed_task
            && let Some(task) = self.task_controller.current()?
            && permit.validate_observation(Some(&task)).is_ok()
            && task.status() == crate::task::TaskStatus::Running
            && matches!(
                task.kind(),
                crate::task::TaskKind::Goal | crate::task::TaskKind::Execute
            )
        {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Pause,
                self.clock.now_ms(),
            )?;
        }
        let settlement = match &*dispatch {
            ForegroundDispatch::Running { transaction, .. }
            | ForegroundDispatch::Finalizing { transaction, .. } => {
                Some(transaction.settlement.clone())
            }
            ForegroundDispatch::Admitting(_)
            | ForegroundDispatch::Done(_)
            | ForegroundDispatch::Transitioning => None,
        };
        let collected = (|| -> Result<()> {
            if let Some(settlement) = settlement
                && self.host_settlement().is_some()
            {
                // Native cleanup and all publication jobs have joined. Keep an
                // ambiguous or unpublished host mutation fenced for repair.
                settlement.seal_effects(&[])?;
                settlement.validate_effects_settled()?;
                let mut checkout = settlement.checkout()?;
                drop(checkout.take());
                drop(checkout);
                self.clear_host_settlement(&settlement)?;
            }
            Ok(())
        })();
        let admission = match dispatch {
            ForegroundDispatch::Admitting(admission)
            | ForegroundDispatch::Running { admission, .. }
            | ForegroundDispatch::Finalizing { admission, .. } => admission,
            _ => unreachable!("failure owner checked above"),
        };
        let mut failure = admission.failure.take().context("failure state missing")?;
        if let Err(error) = collected {
            failure
                .secondary
                .push(format!("collect failed host settlement: {error:#}"));
        }
        if admission.automatic_check_owner.is_none() {
            self.event_sink = None;
        }
        self.trace.record(
            &self.session.id,
            "exchange.foreground.failure_settled",
            json!({
                "reason": format!("{:#}", failure.original),
                "cleanup": failure.cleanup.as_ref().map(ToString::to_string),
                "secondary": failure.secondary,
                "owner_collected": admission.owner_collection_complete(),
            }),
        );
        // Positive owner collection makes admission cancellation, and the output-free
        // retraction it authorized, a settled result. An elapsed tool deadline is settled by the
        // failure cleanup's own scoped collection. Sending any of them through fatal recovery
        // would fence an already-quiescent session, so queued input could never dispatch again.
        let tool_timeout = failure
            .original
            .is::<crate::provider::tool_timeout::ToolTimeout>();
        if (admission.owner_collection_complete() || tool_timeout)
            && failure.cleanup.is_none()
            && failure.secondary.is_empty()
        {
            let response = if let Some(retracted) = failure.original.downcast_ref::<TurnRetracted>()
            {
                Some(Response::failure_with_data(
                    admission.request.id,
                    "turn_retracted",
                    retracted.to_string(),
                    json!({"prompt":retracted.prompt}),
                ))
            } else if failure.original.is::<TurnCancelled>() {
                Some(Response::failure(
                    admission.request.id,
                    if admission.owner_collection_restarts() {
                        "turn_reconfigured"
                    } else {
                        "turn_cancelled"
                    },
                    failure.original.to_string(),
                ))
            } else if tool_timeout {
                Some(Response::failure(
                    admission.request.id,
                    super::request_failure_code(&failure.original),
                    failure.original.to_string(),
                ))
            } else {
                None
            };
            if let Some(response) = response {
                let result = DispatchResult {
                    response,
                    event: std::mem::take(&mut admission.events),
                };
                *dispatch = ForegroundDispatch::Done(result);
                return Ok(ForegroundAction::Finish);
            }
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
                std::future::pending::<
                    std::result::Result<
                        crate::checkpoint::CheckpointRecord,
                        crate::checkpoint::CheckpointFailure,
                    >,
                >()
                .await
            });
            ready.await?;
            let job = FailureBaselineJob {
                provider: broker.provider.clone(),
                exchange_id: "cancelled-baseline".into(),
                baseline: AbortedBaseline::new(
                    Some(crate::checkpoint::CheckpointTask(task)),
                    registry.clone(),
                ),
                policy: FailureBaselinePolicy::Retain,
            };
            if abandoned {
                drop(job);
                registry.collect_cleanup().await?;
            } else {
                let result = job.run().await;
                assert!(matches!(result.outcome?, BaselineDisposition::Retained));
            }
            assert_eq!(
                exit.try_recv(),
                Ok(()),
                "baseline was released without joining its task"
            );
        }
        Ok(())
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

    #[derive(Clone, Copy)]
    enum UndispatchedBoundary {
        Prepared,
        Sending,
    }

    /// A prepared receipt restores the exact draft after cancellation.
    #[tokio::test]
    async fn undispatched_prepared_cancellation_restores_exact_draft() -> Result<()> {
        assert_undispatched_cancellation_boundary(UndispatchedBoundary::Prepared).await
    }

    /// An unknown sending receipt retains history instead of restoring the draft.
    #[tokio::test]
    async fn undispatched_unknown_sending_cancellation_retains_history() -> Result<()> {
        assert_undispatched_cancellation_boundary(UndispatchedBoundary::Sending).await
    }

    /// Only positive journal retirement may restore an undispatched draft without native history proof.
    async fn assert_undispatched_cancellation_boundary(
        boundary: UndispatchedBoundary,
    ) -> Result<()> {
        use crate::provider::submission::SubmissionState;
        use UndispatchedBoundary as Boundary;
        {
            let workspace = super::super::test::repository();
            let data = tempfile::tempdir()?;
            let mut broker =
                super::super::test::planning_question_broker(workspace.path(), data.path(), false);
            broker.checkpoint = None;
            broker.capability.native_turn_rollback = false;
            broker.turn_cancellation.arm();
            let original_model = broker.session.resolved_model.clone();
            let prompt = "Keep this exact draft: Î±\nsecond line";
            let (sink, _updates) = publication::channel();
            let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
                Request {
                    id: 19,
                    method: "session.prompt.submit".into(),
                    params: json!({"text":prompt,"_prompt_admission":{"document":null,"token":97}}),
                },
                sink,
            )?
            else {
                anyhow::bail!("fixture admission is missing");
            };
            let mut staged = broker.stage_foreground_interaction(
                prompt,
                PromptMode::Chat,
                Some(&ExchangeAdmission::chat(prompt.into())),
                ProviderInput::from_text(prompt),
            )?;
            broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
            let exchange_id = staged.exchange.id.clone();
            let engine = broker
                .execution_registry()
                .owner(crate::provider::ThreadOwner::root(&broker.session.id))
                .await?;
            let mut transaction =
                broker.construct_foreground_transaction(&staged, engine.clone())?;
            let submission = transaction
                .request
                .submission
                .as_ref()
                .context("prepared receipt is missing")?
                .clone();
            match boundary {
                Boundary::Prepared => {}
                Boundary::Sending => submission.sending(None)?,
            }
            assert!(transaction.retraction_boundary.is_none());
            assert!(transaction.native.task.is_empty());
            transaction.native.pending_submission.take();
            transaction.native_open = false;
            broker.session.resolved_model = Some("discard this temporary admission value".into());
            broker.turn_cancellation.block_submissions();
            broker.turn_cancellation.request();
            transaction.collection = Some(ForegroundCollection::Cancel(
                crate::exchange::command::CancelledInputPolicy::RestoreIfUnconsumed,
            ));
            let mut dispatch = ForegroundDispatch::Running {
                transaction: Box::new(transaction),
                admission,
            };
            let action =
                broker.complete_foreground_cleanup(&mut dispatch, TurnCancelled.into(), Ok(()))?;
            match boundary {
                Boundary::Prepared => {
                    let ForegroundAction::Effect(ForegroundJob::FailureBaseline(job)) = action
                    else {
                        anyhow::bail!(
                            "undispatched input did not retain its local collection proof"
                        );
                    };
                    assert!(matches!(
                        job.policy,
                        FailureBaselinePolicy::NeverConsumed { .. }
                    ));
                    let result = job.run().await;
                    assert!(matches!(
                        &result.outcome,
                        Ok(BaselineDisposition::Retracted)
                    ));
                    broker.complete_failure_baseline(&mut dispatch, result)?;
                    assert!(broker.store.load_exchange(&exchange_id)?.is_none());
                    assert!(matches!(
                        broker.finish_foreground_failure(&mut dispatch)?,
                        ForegroundAction::CancelJobsAndEffect(ForegroundJob::FailurePublication(_))
                    ));
                    assert_eq!(broker.session.resolved_model, original_model);
                    let ForegroundDispatch::Running { admission, .. } = &dispatch else {
                        anyhow::bail!("foreground owner disappeared");
                    };
                    let failure = admission
                        .failure
                        .as_ref()
                        .context("retraction result missing")?;
                    assert_eq!(
                        failure
                            .original
                            .downcast_ref::<TurnRetracted>()
                            .context("exact draft restoration missing")?
                            .prompt,
                        prompt
                    );
                    assert_eq!(submission.record()?.delivery, SubmissionState::Cancelled);
                }
                Boundary::Sending => {
                    assert!(matches!(
                        action,
                        ForegroundAction::CancelJobsAndEffect(ForegroundJob::FailurePublication(_))
                    ));
                    let retained = broker
                        .store
                        .load_exchange(&exchange_id)?
                        .context("uncertain input lost its history")?;
                    assert!(retained.turn.is_empty());
                    assert_eq!(retained.state(), ExchangeState::Cancelled);
                    assert!(matches!(
                        submission.record()?.delivery,
                        SubmissionState::Sending { .. }
                    ));
                    assert_eq!(
                        broker.session.resolved_model.as_deref(),
                        Some("discard this temporary admission value")
                    );
                    let ForegroundDispatch::Running { admission, .. } = &dispatch else {
                        anyhow::bail!("foreground owner disappeared");
                    };
                    assert!(
                        admission
                            .failure
                            .as_ref()
                            .unwrap()
                            .original
                            .downcast_ref::<TurnRetracted>()
                            .is_none()
                    );
                }
            }
            engine.close().await?;
        }
        Ok(())
    }

    /// Collection during provider preparation waits for the owner and never starts a second native fence.
    #[tokio::test]
    async fn staged_admission_collection_retains_owner_until_positive_completion() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        let (sink, _stream) = publication::channel();
        let mut dispatch = broker.begin_foreground_dispatch(
            Request {
                id: 3,
                method: "session.prompt.submit".into(),
                params: json!({"text":"staged"}),
            },
            sink,
        )?;
        let staged = broker.stage_foreground_interaction(
            "staged",
            PromptMode::Chat,
            Some(&ExchangeAdmission::chat("staged".into())),
            ProviderInput::from_text("staged"),
        )?;
        let ForegroundDispatch::Admitting(admission) = &mut dispatch else {
            unreachable!()
        };
        admission.staged_interaction = Some(staged);
        broker.activate_staged_runtime(admission)?;
        assert!(matches!(
            broker.step_foreground(
                &mut dispatch,
                ForegroundSignal::CollectionStarted(ForegroundCollection::Restart)
            )?,
            ForegroundAction::Wait
        ));
        assert!(matches!(
            broker.step_foreground(
                &mut dispatch,
                ForegroundSignal::EffectFailed(TurnCancelled.into()),
            )?,
            ForegroundAction::WaitForPublicationDrain
        ));
        assert!(matches!(
            broker.step_foreground(
                &mut dispatch,
                ForegroundSignal::EffectFailed(
                    ProviderError::PreparationFailed(Box::new(ProviderError::Cancelled)).into(),
                ),
            )?,
            ForegroundAction::Wait
        ));
        let ForegroundDispatch::Admitting(admission) = &dispatch else {
            unreachable!()
        };
        assert!(admission.failure.as_ref().unwrap().secondary.is_empty());
        assert!(matches!(
            broker.finish_foreground_failure(&mut dispatch)?,
            ForegroundAction::Wait
        ));
        assert!(matches!(
            broker.step_foreground(
                &mut dispatch,
                ForegroundSignal::CollectionCompleted(ForegroundCollection::Restart)
            )?,
            ForegroundAction::WaitForPublicationDrain
        ));
        let ForegroundAction::Effect(ForegroundJob::AdmissionCleanup(job)) =
            broker.finish_foreground_failure(&mut dispatch)?
        else {
            anyhow::bail!("positive owner collection must release retained baseline cleanup")
        };
        assert!(matches!(
            job.native,
            AdmissionNativeCollection::OwnerCollected
        ));
        let result = job.run().await;
        assert!(result.engine.is_ok());
        let ForegroundDispatch::Admitting(admission) = &dispatch else {
            unreachable!()
        };
        assert!(
            admission
                .failure
                .as_ref()
                .unwrap()
                .original
                .to_string()
                .contains("cancelled")
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
