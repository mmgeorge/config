//! Provider-neutral scripted backend for Harness lifecycle and contract tests.

use async_trait::async_trait;
use forge_provider_api::retained_operation::{IdentityRegistry, Interruption, Store};
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    approval::ApprovalMode,
    compaction::CompactionResult,
    events::{self, ProviderEventSink},
    permissions::{PermissionRequest, PermissionResponse, PermissionTarget},
    provider::Provider,
    session::{
        BindingId, InputAcceptance, PromptRequest, RecoveryToken, SessionBinding, SessionRequest,
        SteeringRequest,
    },
    testing::{
        PendingTestResponse, ProviderTestControl, TestControlCommand, TestControlResult,
        TestHoldId, TestPromptScript, TestPromptStep, TestPromptTerminal,
    },
    turn::TurnOutcome,
    *,
};
use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};
use tokio::sync::{Mutex, watch};

use crate::pending_input::{PendingInput, PendingInputs};

#[derive(Clone, Copy)]
enum NativeDispatch {
    Preparing,
    Dispatched,
}

enum OperationStage {
    Preparing,
    Dispatched,
    InterruptedBefore(OperationInterruption),
    InterruptedAfter(OperationInterruption),
    Completed,
}

#[derive(Clone)]
struct MockOperation {
    context: OperationContext,
    stage: Arc<std::sync::Mutex<OperationStage>>,
}

impl MockOperation {
    /// Retain native phase independently of a caller's observation future.
    fn new(context: OperationContext) -> Self {
        Self {
            context,
            stage: Arc::new(std::sync::Mutex::new(OperationStage::Preparing)),
        }
    }

    /// Recheck a contended admission boundary before any native mutation.
    fn admit(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        let stage = self
            .stage
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match *stage {
            OperationStage::Preparing => self.context.check_admission(operation),
            OperationStage::InterruptedBefore(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Admission,
                FailureDisposition::Unchanged,
            )),
            OperationStage::InterruptedAfter(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Observation,
                FailureDisposition::Uncertain,
            )),
            OperationStage::Dispatched | OperationStage::Completed => Ok(()),
        }
    }

    /// Linearize native handoff with deadline interruption under the same local phase lock.
    fn dispatch(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        let mut stage = self
            .stage
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match *stage {
            OperationStage::Preparing => {
                self.context.check_admission(operation)?;
                *stage = OperationStage::Dispatched;
                Ok(())
            }
            OperationStage::InterruptedBefore(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Admission,
                FailureDisposition::Unchanged,
            )),
            OperationStage::InterruptedAfter(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Observation,
                FailureDisposition::Uncertain,
            )),
            OperationStage::Dispatched | OperationStage::Completed => Ok(()),
        }
    }

    /// Reject a live result when interruption has already scheduled binding invalidation.
    fn finish(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        let mut stage = self
            .stage
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match *stage {
            OperationStage::InterruptedBefore(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Admission,
                FailureDisposition::Unchanged,
            )),
            OperationStage::InterruptedAfter(reason) => Err(reason.failure(
                operation,
                ProviderPhase::Observation,
                FailureDisposition::Invalidated,
            )),
            OperationStage::Preparing | OperationStage::Dispatched | OperationStage::Completed => {
                *stage = OperationStage::Completed;
                Ok(())
            }
        }
    }

    /// Retain proof that native work completed before its result future was observed.
    fn complete(&self) {
        *self
            .stage
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = OperationStage::Completed;
    }

    /// Classify interruption from actual native phase without manufacturing negative delivery proof.
    fn interrupt(
        &self,
        reason: OperationInterruption,
        operation: ProviderOperation,
    ) -> (ProviderError, NativeDispatch) {
        let mut stage = self
            .stage
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let (phase, disposition, dispatched) = match *stage {
            OperationStage::Preparing | OperationStage::InterruptedBefore(_) => {
                *stage = OperationStage::InterruptedBefore(reason);
                (
                    ProviderPhase::Admission,
                    FailureDisposition::Unchanged,
                    NativeDispatch::Preparing,
                )
            }
            OperationStage::Dispatched | OperationStage::InterruptedAfter(_) => {
                *stage = OperationStage::InterruptedAfter(reason);
                let disposition = if matches!(
                    operation,
                    ProviderOperation::Inspect
                        | ProviderOperation::ModelList
                        | ProviderOperation::SkillList
                        | ProviderOperation::McpList
                        | ProviderOperation::TerminalList
                        | ProviderOperation::InputReconcile
                ) {
                    FailureDisposition::Unchanged
                } else {
                    FailureDisposition::Uncertain
                };
                (
                    ProviderPhase::Observation,
                    disposition,
                    NativeDispatch::Dispatched,
                )
            }
            OperationStage::Completed => (
                ProviderPhase::Observation,
                FailureDisposition::Uncertain,
                NativeDispatch::Preparing,
            ),
        };
        (reason.failure(operation, phase, disposition), dispatched)
    }
}

type SettlementResult = Result<CompletedTurn, ProviderError>;
type SharedReceipts = Arc<Mutex<HashMap<(SubmissionId, DeliveryAttemptId), AcceptedInput>>>;

/// Admit steering only while the exact response still accepts input.
enum MockInputPhase {
    Receiving(Vec<MockSteering>),
    Responding,
}

#[derive(Clone)]
enum HoldState {
    Armed,
    Pending(PendingTestResponse),
    Released,
    Cancelled,
}

/// Track dispatch separately from acceptance, including terminal-only acknowledgments.
enum PromptDelivery {
    Preparing,
    Dispatched,
    Accepted,
}

/// Native provider-neutral mock used by shared Harness acceptance and lifecycle flows.
#[derive(Clone)]
pub(crate) struct MockProvider {
    bindings: Arc<Mutex<HashMap<BindingId, Arc<MockSession>>>>,
    shutting_down: Arc<std::sync::atomic::AtomicBool>,
    admission: Arc<Mutex<()>>,
    closed_bindings: Arc<Mutex<HashMap<BindingId, Result<(), ProviderError>>>>,
    receipts: SharedReceipts,
    owners: Arc<Mutex<HashMap<ThreadOwner, BindingId>>>,
    control: Arc<MockControl>,
    operations: Arc<MockOperations>,
    shutdown_lock: Arc<Mutex<()>>,
    pending_inputs: Arc<PendingInputs>,
}

impl MockProvider {
    /// Construct a deterministic mock backend without Harness policy or workflow dependencies.
    pub(crate) fn new() -> Result<Self, ProviderError> {
        Ok(Self {
            bindings: Arc::new(Mutex::new(HashMap::new())),
            shutting_down: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            admission: Arc::new(Mutex::new(())),
            closed_bindings: Arc::new(Mutex::new(HashMap::new())),
            receipts: Arc::new(Mutex::new(HashMap::new())),
            owners: Arc::new(Mutex::new(HashMap::new())),
            control: Arc::new(MockControl::default()),
            operations: Arc::new(MockOperations::new()),
            shutdown_lock: Arc::new(Mutex::new(())),
            pending_inputs: Arc::new(PendingInputs::new()),
        })
    }

    /// Get provider-neutral fixture controls independently of the provider contract.
    pub(crate) fn fixture_control(&self) -> Arc<dyn ProviderTestControl> {
        self.control.clone()
    }

    /// Resolve one exact binding without implicitly opening a replacement.
    async fn binding(
        &self,
        id: &BindingId,
        operation: ProviderOperation,
    ) -> Result<Arc<MockSession>, ProviderError> {
        if self.closed_bindings.lock().await.contains_key(id) {
            return Err(
                ProviderFailure::new(operation, ProviderFailureKind::BindingClosed)
                    .at_phase(ProviderPhase::Admission, FailureDisposition::Closed)
                    .into(),
            );
        }
        self.bindings
            .lock()
            .await
            .get(id)
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))
    }

    /// Install cancellation and collection ownership before confirming transport handoff.
    async fn submit(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: PromptRequest,
        pending: &PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.binding(binding, ProviderOperation::TurnStart).await?;
        ensure_open(&session, ProviderOperation::TurnStart)?;
        pending.check_admission(ProviderOperation::TurnStart)?;
        let _admission = tokio::select! {
            admission = session.admission.lock() => admission,
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::TurnStart)
                .expect_err("pending cancellation is sticky before admission")),
        };
        pending.check_admission(ProviderOperation::TurnStart)?;
        operation
            .admit(ProviderOperation::TurnStart)
            .map_err(|cause| ProviderError::PreparationFailed(Box::new(cause)))?;
        ensure_open(&session, ProviderOperation::TurnStart)?;
        if request.scope.owner != session.owner
            || request.receipt.id() != &request.submission_id
            || request.receipt.attempt_id()? != request.attempt_id
            || !request.dispatch.matches(
                binding,
                &request.scope,
                &request.submission_id,
                &request.attempt_id,
            )
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session
            .receipts
            .lock()
            .await
            .contains_key(&(request.submission_id.clone(), request.attempt_id.clone()))
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::HistoryConflict,
            ));
        }
        if session
            .settlements
            .lock()
            .await
            .contains_key(&request.scope)
        {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session.isolated && (!request.tools.is_empty() || request.tool_endpoint.is_some()) {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Policy,
            ));
        }
        if let Err(error) = validate_invocation(&request) {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let preparation = tokio::select! {
            result = request.turn_control.before_send(&request.scope) => result,
            () = pending.cancelled() => return Err(pending.check_admission(ProviderOperation::TurnStart)
                .expect_err("pending cancellation is sticky during preparation")),
        };
        if let Err(error) = preparation {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        pending.check_admission(ProviderOperation::TurnStart)?;
        ensure_open(&session, ProviderOperation::TurnStart)?;
        // Admission serializes replacement, so reject occupied sessions before
        // consuming the scenario reserved for the next admissible prompt.
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let script = self.control.take_script(&session.owner);
        if let Some(script) = &script {
            match &script.terminal {
                TestPromptTerminal::RejectedBeforeDispatch(kind) => {
                    let error = failure(ProviderOperation::TurnStart, *kind);
                    return match request.receipt.clone().record_rejected(error.clone()).await {
                        Ok(()) => Err(ProviderError::PreparationFailed(Box::new(error))),
                        Err(cleanup) => Err(ProviderError::PreparationCleanup {
                            cause: Box::new(error),
                            cleanup: Box::new(cleanup),
                        }),
                    };
                }
                TestPromptTerminal::DeliveryUncertain => {
                    let token = RecoveryToken::new(
                        format!(
                            "mock-sending\n{}\n{}\n{}",
                            request.submission_id.as_str(),
                            request.attempt_id.as_str(),
                            session
                                .binding
                                .provider_session_id
                                .as_ref()
                                .map(ProviderSessionId::as_str)
                                .unwrap_or_default()
                        )
                        .into_bytes(),
                    )
                    .map_err(|_| {
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown)
                    })?;
                    request.receipt.clone().record_sending(Some(token)).await?;
                    request
                        .receipt
                        .clone()
                        .record_failure(ProviderError::DeliveryUncertain)
                        .await?;
                    return Err(ProviderError::DeliveryUncertain);
                }
                TestPromptTerminal::Completed
                | TestPromptTerminal::Cancelled
                | TestPromptTerminal::Failed(_)
                | TestPromptTerminal::ObservationLost(_) => {}
            }
        }
        pending.check_admission(ProviderOperation::TurnStart)?;
        operation
            .dispatch(ProviderOperation::TurnStart)
            .map_err(|cause| ProviderError::PreparationFailed(Box::new(cause)))?;
        let (cancelled, cancel_receiver) = watch::channel(false);
        let (settled, _) = watch::channel(None);
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        let scope = request.scope.clone();
        *session.invocation_model.lock().await = request.configuration.model.model.clone();
        *session.terminal.lock().await = None;
        *session.cancelled_submission.lock().await = None;
        *session.active.lock().await = Some(MockInvocation {
            scope: scope.clone(),
            cancelled,
            settled: settled.clone(),
            phase: MockInputPhase::Receiving(Vec::new()),
        });
        if pending.is_cancelled() {
            session
                .active
                .lock()
                .await
                .as_ref()
                .expect("the invocation was installed under admission")
                .cancelled
                .send_replace(true);
        }
        let address = TurnAddress {
            scope: scope.clone(),
            thread_id: request.thread_id.clone(),
            turn_id: request.turn_id.clone(),
        };
        let driver = Arc::new(MockTurnDriver {
            session: session.clone(),
            scope: scope.clone(),
            input: Mutex::new(Some(MockRun {
                control: self.control.clone(),
                request,
                script,
                cancelled: cancel_receiver,
                accepted,
            })),
        });
        let runner = Arc::new(crate::turn_runner::RetainedTurn::spawn(
            driver,
            session.sink.clone(),
            address,
        )?);
        let for_collection = session.clone();
        let for_start = runner.clone();
        tokio::spawn(async move {
            let result = runner.join().await;
            let _admission = for_collection.admission.lock().await;
            if result.is_err() {
                *for_collection
                    .state
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) =
                    MockBindingState::Invalidated;
            }
            for_collection
                .settlements
                .lock()
                .await
                .insert(scope, result.clone());
            if result.is_ok() {
                for_collection.active.lock().await.take();
            }
            settled.send_replace(Some(result));
        });
        pending.registered();
        drop(_admission);
        let accepted = acceptance
            .await
            .unwrap_or_else(|_| Err(ProviderError::DeliveryUncertain));
        for_start.started().await?;
        accepted
    }
}

#[async_trait]
impl Provider for MockProvider {
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        request.validate_thread_owner()?;
        let payload = request.identity();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        let cleanup_owner = request.owner.clone();
        let cleanup = self.clone();
        self.operations
            .thread_open
            .take(
                context,
                ProviderOperation::ThreadOpen,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.session_open_native(native_operation, request).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ThreadOpen);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => {
                                cleanup.close_open_owner(&cleanup_owner).await
                            }
                        }
                    })
                },
            )
            .await
    }

    async fn thread_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        let payload = (binding.clone(), configuration.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();
        let cleanup_binding = binding.clone();
        let cleanup = self.clone();
        self.operations
            .thread_configure
            .observe(
                context,
                ProviderOperation::ThreadConfigure,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .session_configure_native(native_operation, &native_binding, configuration)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ThreadConfigure);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => {
                                cleanup.close_binding(&cleanup_binding).await
                            }
                        }
                    })
                },
            )
            .await
    }

    async fn thread_fork(
        &self,
        context: OperationContext,
        request: SessionForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        let payload = request.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .thread_fork
            .observe(
                context,
                ProviderOperation::ThreadFork,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.session_fork_native(native_operation, request).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ThreadFork);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn thread_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let payload = (binding.clone(), request.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();
        let cleanup_binding = binding.clone();
        let cleanup = self.clone();
        self.operations
            .thread_compact
            .observe(
                context,
                ProviderOperation::ThreadCompact,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .session_compact_native(native_operation, &native_binding, request)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ThreadCompact);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => {
                                cleanup.close_binding(&cleanup_binding).await
                            }
                        }
                    })
                },
            )
            .await
    }

    async fn thread_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let payload = binding.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();

        self.operations
            .thread_close
            .observe(
                context,
                ProviderOperation::ThreadClose,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .session_close_native(native_operation, &native_binding)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ThreadClose);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        let payload = (binding.clone(), request.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();

        self.operations
            .cancel
            .observe(
                context,
                ProviderOperation::TurnInterrupt,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .cancel_native(native_operation, &native_binding, request)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::TurnInterrupt);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn turn_start(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        request.validate_turn_identity()?;
        let payload = (binding.clone(), request.identity());
        let pending = self
            .pending_inputs
            .register(binding, &request.scope, context.id());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();
        let cleanup_binding = binding.clone();
        let cleanup_scope = request.scope.clone();
        let cleanup = self.clone();
        self.operations
            .turn_start
            .observe(
                context,
                ProviderOperation::TurnStart,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .input_submit_native(native_operation, &native_binding, request, &pending)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::TurnStart);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => {
                                cleanup
                                    .pending_inputs
                                    .cancel(&cleanup_binding, &cleanup_scope)
                                    .await;
                                Ok(())
                            }
                            NativeDispatch::Dispatched => {
                                cleanup
                                    .interrupt_exact(&cleanup_binding, &cleanup_scope)
                                    .await
                            }
                        }
                    })
                },
            )
            .await
    }

    async fn turn_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let payload = (binding.clone(), request.identity());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();
        let cleanup_binding = binding.clone();
        let cleanup_scope = request.scope.clone();
        let cleanup = self.clone();
        self.operations
            .turn_steer
            .observe(
                context,
                ProviderOperation::TurnSteer,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .input_steer_native(native_operation, &native_binding, request)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::TurnSteer);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => {
                                cleanup
                                    .interrupt_exact(&cleanup_binding, &cleanup_scope)
                                    .await
                            }
                        }
                    })
                },
            )
            .await
    }

    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        let payload = request.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .input_reconcile
            .observe(
                context,
                ProviderOperation::InputReconcile,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .input_reconcile_native(native_operation, request)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::InputReconcile);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn input_retract(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        let payload = (binding.clone(), request.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();

        self.operations
            .input_retract
            .observe(
                context,
                ProviderOperation::InputRetract,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .input_retract_native(native_operation, &native_binding, request)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::InputRetract);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn inspect(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
    ) -> Result<ProviderSetup, ProviderError> {
        let payload = workspace.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .inspect
            .observe(
                context,
                ProviderOperation::Inspect,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.inspect_native(native_operation, workspace).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::Inspect);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn authenticate(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
        method: AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let payload = (workspace.clone(), method.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .authenticate
            .observe(
                context,
                ProviderOperation::Authenticate,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .authenticate_native(native_operation, workspace, method)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::Authenticate);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError> {
        let payload = ();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .shutdown
            .observe(
                context,
                ProviderOperation::Shutdown,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.shutdown_native(native_operation).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::Shutdown);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn model_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let payload = query.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .model_list
            .observe(
                context,
                ProviderOperation::ModelList,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.model_list_native(native_operation, query).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::ModelList);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn skill_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<catalog::SkillDefinition>, ProviderError> {
        let payload = query.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .skill_list
            .observe(
                context,
                ProviderOperation::SkillList,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.skill_list_native(native_operation, query).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::SkillList);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn mcp_list(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        let payload = query.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();

        self.operations
            .mcp_list
            .observe(
                context,
                ProviderOperation::McpList,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native.mcp_list_native(native_operation, query).await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::McpList);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn terminal_list(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<terminal::TerminalSnapshot, ProviderError> {
        let payload = binding.clone();
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();

        self.operations
            .terminal_list
            .observe(
                context,
                ProviderOperation::TerminalList,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .terminal_list_native(native_operation, &native_binding)
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::TerminalList);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }

    async fn terminal_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
        terminal: &terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        let payload = (binding.clone(), terminal.clone());
        let operation = MockOperation::new(context.clone());
        let native_operation = operation.clone();
        let native = self.clone();
        let native_binding = binding.clone();
        let native_terminal = terminal.clone();

        self.operations
            .terminal_terminate
            .observe(
                context,
                ProviderOperation::TerminalTerminate,
                payload,
                async move {
                    let completed = native_operation.clone();
                    let result = native
                        .terminal_terminate_native(
                            native_operation,
                            &native_binding,
                            &native_terminal,
                        )
                        .await;
                    completed.complete();
                    result
                },
                move |reason| {
                    let (observation, dispatched) =
                        operation.interrupt(reason, ProviderOperation::TerminalTerminate);
                    Interruption::new(observation, async move {
                        match dispatched {
                            NativeDispatch::Preparing => Ok(()),
                            NativeDispatch::Dispatched => Ok(()),
                        }
                    })
                },
            )
            .await
    }
}

impl MockProvider {
    async fn session_open_native(
        &self,
        operation: MockOperation,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        let _admission = self.admission.lock().await;
        operation.admit(ProviderOperation::ThreadOpen)?;
        if self
            .shutting_down
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        validate_model_selection(&request.configuration.model)?;
        let isolated = matches!(request.history, SessionHistory::IsolatedText(_));
        if isolated
            && (!request.tools.is_empty()
                || request.tool_endpoint.is_some()
                || !request.configuration.resources.skills.is_empty()
                || !request.configuration.resources.mcp.is_empty())
        {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Policy,
            ));
        }
        if self.owners.lock().await.contains_key(&request.owner) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        let binding_id =
            BindingId::new(uuid::Uuid::new_v4().to_string()).expect("UUID is nonempty");
        let provider_session_id = request.history.resume_id().cloned().or_else(|| {
            Some(
                uuid::Uuid::new_v4()
                    .to_string()
                    .try_into()
                    .expect("UUID is nonempty"),
            )
        });
        let binding = SessionBinding {
            thread_id: request.thread_id.clone(),
            id: binding_id.clone(),
            provider_session_id,
        };
        let owner_scope = EventScope::Binding {
            owner: request.owner.clone(),
        };
        let (sink, events) = events::channel(binding_id.clone(), owner_scope);
        let SessionConfiguration {
            model,
            access,
            approval,
            resources,
        } = request.configuration;
        let session = Arc::new(MockSession {
            binding: binding.clone(),
            owner: request.owner.clone(),
            receipts: self.receipts.clone(),
            sink,
            _resource_lease: request.resource_lease,
            selection: Mutex::new(resources),
            model: Mutex::new(model),
            invocation_model: Mutex::new(None),
            access: Mutex::new(access),
            approval: Mutex::new(approval),
            state: std::sync::Mutex::new(MockBindingState::Open),
            isolated,
            settlements: Mutex::new(HashMap::new()),
            terminal: Mutex::new(None),
            operation: Mutex::new(()),
            admission: Mutex::new(()),
            active: Mutex::new(None),
            cancelled_submission: Mutex::new(None),
            history_boundary: Mutex::new(None),
        });
        let mut bindings = self.bindings.lock().await;
        let mut owners = self.owners.lock().await;
        operation.dispatch(ProviderOperation::ThreadOpen)?;
        let identity_writer = session.sink.activity_writer();
        bindings.insert(binding_id.clone(), session);
        owners.insert(request.owner, binding_id);
        drop(owners);
        drop(bindings);
        if let Some(history) = &binding.provider_session_id {
            identity_writer
                .send_wait(AdapterActivity::Thread {
                    thread_id: binding.thread_id.clone(),
                    event: ThreadEvent::IdentityResolved(history.clone()),
                })
                .await?;
        }
        operation.finish(ProviderOperation::ThreadOpen)?;
        Ok(OpenedSession {
            binding,
            capabilities: mock_capabilities(),
            events,
        })
    }

    async fn session_configure_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError> {
        let session = self
            .binding(binding, ProviderOperation::ThreadConfigure)
            .await?;
        let _admission = session.admission.lock().await;
        ensure_open(&session, ProviderOperation::ThreadConfigure)?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        validate_model_selection(&configuration.model)?;
        if session.isolated
            && (!configuration.resources.skills.is_empty()
                || !configuration.resources.mcp.is_empty())
        {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Policy,
            ));
        }
        operation.dispatch(ProviderOperation::ThreadConfigure)?;
        *session.model.lock().await = configuration.model.clone();
        *session.access.lock().await = configuration.access.clone();
        *session.approval.lock().await = configuration.approval;
        *session.selection.lock().await = configuration.resources.clone();
        operation.finish(ProviderOperation::ThreadConfigure)?;
        Ok(ConfiguredSession {
            configuration,
            capabilities: mock_capabilities(),
            watermark: session.sink.watermark(),
        })
    }

    async fn session_fork_native(
        &self,
        operation: MockOperation,
        request: SessionForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        let source_id = match &request.source {
            session::ForkSource::Binding(binding) => binding.clone(),
            session::ForkSource::Checkpoint { .. } => {
                return Err(capability::UnsupportedCapability(
                    capability::ProviderFeature::CheckpointFork,
                )
                .into());
            }
        };
        let source = self
            .binding(&source_id, ProviderOperation::ThreadFork)
            .await?;
        let _admission = source.admission.lock().await;
        ensure_open(&source, ProviderOperation::ThreadFork)?;
        if source.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ThreadFork,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        validate_model_selection(&request.target.configuration.model)?;
        operation.dispatch(ProviderOperation::ThreadFork)?;
        let result = ProviderForkResult::unprofiled(
            uuid::Uuid::new_v4()
                .to_string()
                .try_into()
                .expect("UUID is nonempty"),
        );
        Ok(result)
    }

    async fn session_compact_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        let session = self
            .binding(binding, ProviderOperation::ThreadCompact)
            .await?;
        let _admission = session.admission.lock().await;
        ensure_open(&session, ProviderOperation::ThreadCompact)?;
        request.validate_context(&operation.context)?;
        if request.scope.owner() != &session.owner {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        let _operation = session.operation.lock().await;
        operation.dispatch(ProviderOperation::ThreadCompact)?;
        let result = Compaction {
            id: operation.context.id().as_str().to_owned(),
            thread_id: session.owner.thread_id().as_str().to_owned(),
            state: CompactionState::Completed,
            manual: true,
            started_at_ms: None,
            completed_at_ms: Some(now_ms()),
            result: CompactionResult {
                summary_content: Some(
                    "Mock compacted context preserves the conversation constraints.".into(),
                ),
                pre_compaction_tokens: Some(80_000),
                post_compaction_tokens: Some(20_000),
                context_tokens_after: Some(20_000),
                tokens_removed: Some(60_000),
                ..Default::default()
            },
        };
        let sink = session.sink.bind_scope(request.scope)?;
        sink.activity_writer()
            .send_wait(AdapterActivity::Thread {
                thread_id: session.binding.thread_id.clone(),
                event: ThreadEvent::CompactionUpdated(Box::new(result.clone())),
            })
            .await?;
        let result = forge_provider_api::CompactionResult {
            compaction: result,
            watermark: sink.watermark(),
        };
        Ok(result)
    }

    /// Revoke native admission before awaiting any retained binding cleanup.
    async fn session_close_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        operation.dispatch(ProviderOperation::ThreadClose)?;
        self.close_binding(binding).await
    }

    /// Close an allocated open result after its allocation boundary has completed.
    async fn close_open_owner(&self, owner: &ThreadOwner) -> Result<(), ProviderError> {
        let admission = self.admission.lock().await;
        let binding = self.owners.lock().await.get(owner).cloned();
        drop(admission);
        match binding {
            Some(binding) => self.close_binding(&binding).await,
            None => Ok(()),
        }
    }

    /// Request exact interruption only when that invocation was installed by the original attempt.
    async fn interrupt_exact(
        &self,
        binding: &BindingId,
        scope: &InvocationScope,
    ) -> Result<(), ProviderError> {
        self.pending_inputs.cancel(binding, scope).await;
        let session = match self
            .binding(binding, ProviderOperation::TurnInterrupt)
            .await
        {
            Ok(session) => session,
            Err(ProviderError::Failure(cause))
                if matches!(
                    cause.kind(),
                    ProviderFailureKind::SessionMissing | ProviderFailureKind::BindingClosed
                ) =>
            {
                return Ok(());
            }
            Err(error) => return Err(error),
        };
        let admission = session.admission.lock().await;
        let active = session
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.scope == *scope);
        drop(admission);
        if active {
            self.cancel_exact(
                binding,
                CancelRequest {
                    scope: scope.clone(),
                },
            )
            .await
            .map(|_| ())
        } else {
            Ok(())
        }
    }

    /// Collect a revoked exact binding while preserving its first collected failure.
    async fn close_binding(&self, binding: &BindingId) -> Result<(), ProviderError> {
        if let Some(result) = self.closed_bindings.lock().await.get(binding).cloned() {
            return result;
        }
        let session = self
            .binding(binding, ProviderOperation::ThreadClose)
            .await?;
        *session
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = MockBindingState::Closing;
        self.pending_inputs.cancel_binding(binding).await;
        let admission = session.admission.lock().await;
        let active = {
            let active = session.active.lock().await;
            active.as_ref().map(|invocation| {
                invocation.cancelled.send_replace(true);
                invocation.settled.subscribe()
            })
        };
        self.control.cancel_owner(Some(&session.owner));
        drop(admission);
        let settlement_failure = if let Some(mut settled) = active {
            wait_settlement(&mut settled).await.err()
        } else {
            None
        };
        if session.isolated {
            session.receipts.lock().await.retain(|_, evidence| {
                Some(&evidence.provider_session_id) != session.binding.provider_session_id.as_ref()
            });
        }
        let result = match settlement_failure {
            Some(ProviderError::Failure(failure)) => Err(ProviderError::Failure(
                failure.at_phase(failure.phase(), FailureDisposition::Closed),
            )),
            Some(error) => Err(error),
            None => Ok(()),
        };
        self.closed_bindings
            .lock()
            .await
            .insert(binding.clone(), result.clone());
        self.bindings.lock().await.remove(binding);
        let mut owners = self.owners.lock().await;
        if owners.get(&session.owner) == Some(binding) {
            owners.remove(&session.owner);
        }
        result
    }

    /// Retain exact native cancellation independently of the observer deadline.
    async fn cancel_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError> {
        operation.dispatch(ProviderOperation::TurnInterrupt)?;
        let scope = request.scope.clone();
        self.cancel_exact(binding, request).await?;
        let session = self
            .binding(binding, ProviderOperation::TurnInterrupt)
            .await?;
        Ok(TurnInterruptResult {
            operation_id: operation.context.id().clone(),
            thread_id: scope.owner.thread_id(),
            turn_id: scope.turn_id().map_err(|_| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::WrongInvocation,
                )
            })?,
            watermark: session.sink.watermark(),
        })
    }

    /// Interrupt only the matching invocation while retaining its collection receiver.
    async fn cancel_exact(
        &self,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<CompletedTurn, ProviderError> {
        self.pending_inputs.cancel(binding, &request.scope).await;
        let session = self
            .binding(binding, ProviderOperation::TurnInterrupt)
            .await?;
        let admission = session.admission.lock().await;
        if let Some(result) = session
            .settlements
            .lock()
            .await
            .get(&request.scope)
            .cloned()
        {
            return result;
        }
        let mut settled = {
            let active = session.active.lock().await;
            let invocation = active
                .as_ref()
                .filter(|invocation| invocation.scope == request.scope)
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::TurnInterrupt,
                        ProviderFailureKind::WrongInvocation,
                    )
                })?;
            invocation.cancelled.send_replace(true);
            invocation.settled.subscribe()
        };
        drop(admission);
        wait_settlement(&mut settled).await
    }

    async fn input_submit_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: PromptRequest,
        pending: &PendingInput,
    ) -> Result<InputAcceptance, ProviderError> {
        self.submit(operation, binding, request, pending).await
    }

    async fn input_steer_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<session::SteeringAcceptance, ProviderError> {
        let session = self.binding(binding, ProviderOperation::TurnSteer).await?;
        let _admission = session.admission.lock().await;
        ensure_open(&session, ProviderOperation::TurnSteer)?;
        {
            let active = session.active.lock().await;
            if active.as_ref().is_none_or(|invocation| {
                invocation.scope != request.scope
                    || *invocation.cancelled.borrow()
                    || !matches!(invocation.phase, MockInputPhase::Receiving(_))
            }) {
                return Err(failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::WrongInvocation,
                ));
            }
        }
        let submission_id = request.delivery.submission_id().cloned();
        let sending = if let Some(receipt) = request.delivery.receipt() {
            let attempt = receipt.attempt_id()?;
            if session
                .receipts
                .lock()
                .await
                .contains_key(&(receipt.id().clone(), attempt.clone()))
            {
                return Err(failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::HistoryConflict,
                ));
            }
            let history = session
                .binding
                .provider_session_id
                .as_ref()
                .map(ProviderSessionId::as_str)
                .unwrap_or_default();
            let token = RecoveryToken::new(
                format!(
                    "mock-sending\n{}\n{}\n{history}",
                    receipt.id().as_str(),
                    attempt.as_str()
                )
                .into_bytes(),
            )
            .map_err(|_| failure(ProviderOperation::TurnSteer, ProviderFailureKind::Protocol))?;
            receipt.clone().record_sending(Some(token)).await?;
            Some((receipt.clone(), attempt))
        } else {
            None
        };
        let mut text = render_input(&request.input);
        if let Some(task) = &request.task {
            text = format!("{}\n\n{text}", task.heading());
        }
        if !request.attachments.is_empty() {
            let attachments = request
                .attachments
                .iter()
                .map(|attachment| attachment.context.as_str())
                .collect::<Vec<_>>()
                .join("\n");
            text = format!("{attachments}\n\n{text}");
        }
        {
            let mut active = session.active.lock().await;
            ensure_open(&session, ProviderOperation::TurnSteer)?;
            let invocation = active
                .as_mut()
                .filter(|invocation| {
                    invocation.scope == request.scope && !*invocation.cancelled.borrow()
                })
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::TurnSteer,
                        ProviderFailureKind::WrongInvocation,
                    )
                })?;
            let MockInputPhase::Receiving(pending) = &mut invocation.phase else {
                return Err(failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::WrongInvocation,
                ));
            };
            if let Err(error) = operation.dispatch(ProviderOperation::TurnSteer) {
                drop(active);
                return Err(match request.delivery.receipt() {
                    Some(receipt) => reject_before_dispatch(receipt, error).await,
                    None => error,
                });
            }
            pending.push(MockSteering {
                text,
                submission: submission_id.clone(),
                covered: sending.as_ref().map(|(receipt, attempt)| {
                    CoveredInput::new(receipt.id().clone(), attempt.clone())
                }),
            });
        }
        if let Some((receipt, attempt)) = sending {
            let history = session
                .binding
                .provider_session_id
                .clone()
                .expect("mock history is allocated before input");
            let token = RecoveryToken::new(
                format!(
                    "mock-accepted\n{}\n{}\n{}",
                    receipt.id().as_str(),
                    attempt.as_str(),
                    history.as_str()
                )
                .into_bytes(),
            )
            .map_err(|_| failure(ProviderOperation::TurnSteer, ProviderFailureKind::Protocol))?;
            session.receipts.lock().await.insert(
                (receipt.id().clone(), attempt),
                AcceptedInput {
                    provider_session_id: history,
                    receipt: token.clone(),
                },
            );
            receipt.record_accepted(Some(token)).await?;
        }
        Ok(match submission_id {
            Some(submission_id) => session::SteeringAcceptance::Journaled { submission_id },
            None => session::SteeringAcceptance::Context,
        })
    }

    async fn input_reconcile_native(
        &self,
        operation: MockOperation,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        operation.dispatch(ProviderOperation::InputReconcile)?;
        let Some(token) = request.token else {
            return Ok(SubmissionEvidence::Unknown);
        };
        let Ok(encoded) = std::str::from_utf8(token.as_bytes()) else {
            return Ok(SubmissionEvidence::Unknown);
        };
        let fields = encoded.split('\n').collect::<Vec<_>>();
        if fields.len() != 4
            || fields[0] != "mock-sending"
            || fields[1] != request.submission_id.as_str()
            || fields[2] != request.attempt_id.as_str()
            || request
                .provider_session_id
                .as_ref()
                .is_some_and(|history| history.as_str() != fields[3])
        {
            return Ok(SubmissionEvidence::Unknown);
        }
        if let Some(evidence) = self
            .receipts
            .lock()
            .await
            .get(&(request.submission_id.clone(), request.attempt_id.clone()))
            .cloned()
            && evidence.provider_session_id.as_str() == fields[3]
        {
            return Ok(SubmissionEvidence::Accepted {
                receipt: Some(evidence.receipt),
                provider_session_id: Some(evidence.provider_session_id),
            });
        }
        for receipt in request.accepted_receipts {
            let Ok(encoded) = std::str::from_utf8(receipt.as_bytes()) else {
                continue;
            };
            let accepted = encoded.split('\n').collect::<Vec<_>>();
            if accepted.len() != 4
                || accepted[0] != "mock-accepted"
                || accepted[1] != fields[1]
                || accepted[2] != fields[2]
                || accepted[3] != fields[3]
            {
                continue;
            }
            let Ok(history) = ProviderSessionId::try_from(accepted[3].to_owned()) else {
                continue;
            };
            return Ok(SubmissionEvidence::Accepted {
                receipt: Some(receipt),
                provider_session_id: Some(history),
            });
        }
        Ok(SubmissionEvidence::Unknown)
    }

    async fn input_retract_native(
        &self,
        operation: MockOperation,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        let session = self
            .binding(binding, ProviderOperation::InputRetract)
            .await?;
        let _admission = session.admission.lock().await;
        ensure_open(&session, ProviderOperation::InputRetract)?;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::ExecutionActive,
            ));
        }
        match request.authorization {
            RetractionAuthorization::OutputFreeCancellation => {}
        }
        let settled = session.settlements.lock().await;
        let outcome = match settled.get(&request.scope) {
            None => InputRetractOutcome::Unavailable(InputRetractUnavailable::WrongInvocation),
            Some(Err(_)) => InputRetractOutcome::Unavailable(InputRetractUnavailable::NotCancelled),
            Some(Ok(settlement)) => {
                if !matches!(settlement.result().outcome(), TurnOutcome::Interrupted) {
                    InputRetractOutcome::Unavailable(InputRetractUnavailable::NotCancelled)
                } else if !session
                    .receipts
                    .lock()
                    .await
                    .contains_key(&(request.submission_id.clone(), request.attempt_id.clone()))
                {
                    InputRetractOutcome::Unavailable(InputRetractUnavailable::WrongInvocation)
                } else if session.history_boundary.lock().await.as_ref() != Some(&request.boundary)
                    || request.boundary.as_str() != request.submission_id.as_str()
                {
                    InputRetractOutcome::Unavailable(
                        InputRetractUnavailable::HistoryBoundaryChanged,
                    )
                } else if session.cancelled_submission.lock().await.as_ref()
                    != Some(&request.submission_id)
                {
                    InputRetractOutcome::Unavailable(InputRetractUnavailable::WrongInvocation)
                } else {
                    InputRetractOutcome::Retracted {
                        boundary: request.boundary.clone(),
                    }
                }
            }
        };
        if matches!(outcome, InputRetractOutcome::Retracted { .. }) {
            operation.dispatch(ProviderOperation::InputRetract)?;
            session.history_boundary.lock().await.take();
        }
        Ok(outcome)
    }

    async fn inspect_native(
        &self,
        operation: MockOperation,
        _context: catalog::WorkspaceContext,
    ) -> Result<provider::ProviderSetup, ProviderError> {
        operation.dispatch(ProviderOperation::Inspect)?;
        Ok(provider::ProviderSetup {
            runtime: provider::RuntimeAvailability::Available,
            origin: provider::RuntimeOrigin::Configured,
            versions: Default::default(),
            authentication: Vec::new(),
            capabilities: mock_capabilities(),
        })
    }

    async fn authenticate_native(
        &self,
        operation: MockOperation,
        _workspace: WorkspaceContext,
        _method: AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        operation.admit(ProviderOperation::Authenticate)?;
        Err(capability::UnsupportedCapability(capability::ProviderFeature::Authentication).into())
    }

    async fn shutdown_native(&self, operation: MockOperation) -> Result<(), ProviderError> {
        let _shutdown = self.shutdown_lock.lock().await;
        operation.dispatch(ProviderOperation::Shutdown)?;
        self.operations.identities.close_admission();
        self.shutting_down
            .store(true, std::sync::atomic::Ordering::Release);
        let admission = self.admission.lock().await;
        self.control.cancel_owner(None);
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        drop(admission);
        let mut failures = Vec::new();
        for binding in bindings {
            if let Err(error) = self.close_binding(&binding).await {
                failures.push(error);
            }
        }
        if let Err(error) = self
            .operations
            .join_all_except(operation.context.id())
            .await
        {
            failures.push(error);
        }
        match failures.split_first() {
            None => Ok(()),
            Some((first, rest)) => Err(ProviderError::Combined {
                primary: Box::new(first.clone()),
                secondary: rest.to_vec(),
            }),
        }
    }

    async fn model_list_native(
        &self,
        operation: MockOperation,
        query: catalog::QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        operation.dispatch(ProviderOperation::ModelList)?;
        let selected = match query {
            catalog::QueryContext::Workspace(_) => None,
            catalog::QueryContext::Binding(binding) => {
                Some(self.binding(&binding, ProviderOperation::ModelList).await?)
            }
        };
        let (selected_reasoning, selected_context_window) = if let Some(session) = selected {
            let model = session.model.lock().await;
            let reasoning = match &model.reasoning {
                generation::ReasoningSelection::Default => Some("medium".into()),
                generation::ReasoningSelection::Lowest => Some("low".into()),
                generation::ReasoningSelection::Level(level) => Some(level.clone()),
            };
            (reasoning, model.context_window.clone())
        } else {
            (None, None)
        };
        Ok(vec![ProviderModel {
            service_tiers: vec![
                ServiceTier::Standard,
                ServiceTier::Fast,
                ServiceTier::Ultrafast,
            ],
            id: "mock-model".into(),
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning,
            context_window: vec![ProviderContextWindow {
                id: "default".into(),
                token_limit: Some(100_000),
            }],
            default_context_window: Some("default".into()),
            selected_context_window,
            vision: false,
            description: Some("Deterministic provider-neutral test model.".into()),
            is_default: true,
        }])
    }

    async fn skill_list_native(
        &self,
        operation: MockOperation,
        query: catalog::QueryContext,
    ) -> Result<Vec<catalog::SkillDefinition>, ProviderError> {
        operation.dispatch(ProviderOperation::SkillList)?;
        match query {
            catalog::QueryContext::Workspace(_) => Ok(Vec::new()),
            catalog::QueryContext::Binding(binding) => Ok(self
                .binding(&binding, ProviderOperation::SkillList)
                .await?
                .selection
                .lock()
                .await
                .skills
                .clone()),
        }
    }

    async fn mcp_list_native(
        &self,
        operation: MockOperation,
        query: catalog::McpQuery,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        operation.dispatch(ProviderOperation::McpList)?;
        let context = match query {
            catalog::McpQuery::Configured(context) | catalog::McpQuery::Observed(context) => {
                context
            }
        };
        let catalog::QueryContext::Binding(binding) = context else {
            return Ok(Vec::new());
        };
        let session = self.binding(&binding, ProviderOperation::McpList).await?;
        Ok(session
            .selection
            .lock()
            .await
            .mcp
            .iter()
            .map(|launch| catalog::McpDefinition {
                resource_id: None,
                scope: None,
                source_path: None,
                name: launch.name.clone(),
                transport: match &launch.transport {
                    session::McpTransport::Stdio { .. } => catalog::McpTransportKind::Stdio,
                    session::McpTransport::Http { .. } => catalog::McpTransportKind::Http,
                    session::McpTransport::Sse { .. } => catalog::McpTransportKind::Sse,
                },
                enabled: true,
                status: catalog::McpStatus::Connected,
                status_detail: None,
                token_count: None,
                token_estimated: false,
                tools: Vec::new(),
                tool_error: None,
            })
            .collect())
    }

    async fn terminal_list_native(
        &self,
        operation: MockOperation,
        _binding: &BindingId,
    ) -> Result<terminal::TerminalSnapshot, ProviderError> {
        operation.admit(ProviderOperation::TerminalList)?;
        Err(
            capability::UnsupportedCapability(capability::ProviderFeature::BackgroundTerminals)
                .into(),
        )
    }

    async fn terminal_terminate_native(
        &self,
        operation: MockOperation,
        _binding: &BindingId,
        _terminal: &terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        operation.admit(ProviderOperation::TerminalTerminate)?;
        Err(
            capability::UnsupportedCapability(capability::ProviderFeature::BackgroundTermination)
                .into(),
        )
    }
}

struct MockOperations {
    identities: Arc<IdentityRegistry>,
    thread_open: Store<SessionRequestIdentity, OpenedSession>,
    thread_configure: Store<(BindingId, SessionConfiguration), ConfiguredSession>,
    thread_fork: Store<SessionForkRequest, ProviderForkResult>,
    thread_compact: Store<(BindingId, CompactionRequest), forge_provider_api::CompactionResult>,
    thread_close: Store<BindingId, ()>,
    cancel: Store<(BindingId, CancelRequest), TurnInterruptResult>,
    turn_start: Store<(BindingId, PromptRequestIdentity), InputAcceptance>,
    turn_steer: Store<(BindingId, SteeringRequestIdentity), SteeringAcceptance>,
    input_reconcile: Store<InputReconcileRequest, SubmissionEvidence>,
    input_retract: Store<(BindingId, InputRetractRequest), InputRetractOutcome>,
    inspect: Store<WorkspaceContext, ProviderSetup>,
    authenticate: Store<(WorkspaceContext, AuthenticationMethodId), ()>,
    shutdown: Store<(), ()>,
    model_list: Store<QueryContext, Vec<ProviderModel>>,
    skill_list: Store<QueryContext, Vec<catalog::SkillDefinition>>,
    mcp_list: Store<McpQuery, Vec<catalog::McpDefinition>>,
    terminal_list: Store<BindingId, terminal::TerminalSnapshot>,
    terminal_terminate: Store<(BindingId, terminal::TerminalId), ()>,
}

impl MockOperations {
    /// Create typed operation domains sharing one immutable identity namespace.
    fn new() -> Self {
        let identities = Arc::new(IdentityRegistry::new());
        Self {
            thread_open: Store::registered(identities.clone()),
            thread_configure: Store::registered(identities.clone()),
            thread_fork: Store::registered(identities.clone()),
            thread_compact: Store::registered(identities.clone()),
            thread_close: Store::registered(identities.clone()),
            cancel: Store::registered(identities.clone()),
            turn_start: Store::registered(identities.clone()),
            turn_steer: Store::registered(identities.clone()),
            input_reconcile: Store::registered(identities.clone()),
            input_retract: Store::registered(identities.clone()),
            inspect: Store::registered(identities.clone()),
            authenticate: Store::registered(identities.clone()),
            shutdown: Store::registered(identities.clone()),
            model_list: Store::registered(identities.clone()),
            skill_list: Store::registered(identities.clone()),
            mcp_list: Store::registered(identities.clone()),
            terminal_list: Store::registered(identities.clone()),
            terminal_terminate: Store::registered(identities.clone()),
            identities,
        }
    }

    /// Join retained workers after native resources close; serialized shutdown observers cannot join each other.
    async fn join_all_except(&self, excluded: &OperationId) -> Result<(), ProviderError> {
        let mut failures = Vec::new();
        if let Err(error) = self.thread_open.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.thread_configure.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.thread_fork.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.thread_compact.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.thread_close.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.cancel.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.turn_start.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.turn_steer.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.input_reconcile.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.input_retract.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.inspect.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.authenticate.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.model_list.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.skill_list.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.mcp_list.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.terminal_list.join_all_except(excluded).await {
            failures.push(error);
        }
        if let Err(error) = self.terminal_terminate.join_all_except(excluded).await {
            failures.push(error);
        }
        match failures.split_first() {
            None => Ok(()),
            Some((first, [])) => Err(first.clone()),
            Some((first, rest)) => Err(ProviderError::Combined {
                primary: Box::new(first.clone()),
                secondary: rest.to_vec(),
            }),
        }
    }
}

#[derive(Clone)]
struct AcceptedInput {
    provider_session_id: ProviderSessionId,
    receipt: RecoveryToken,
}

struct MockSession {
    binding: SessionBinding,
    owner: ThreadOwner,
    receipts: SharedReceipts,
    sink: ProviderEventSink,
    _resource_lease: Option<Arc<dyn session::ResourceLease>>,
    selection: Mutex<session::ResourceSelection>,
    model: Mutex<session::ModelSelection>,
    invocation_model: Mutex<Option<String>>,
    access: Mutex<access::AccessPolicy>,
    approval: Mutex<ApprovalMode>,
    state: std::sync::Mutex<MockBindingState>,
    isolated: bool,
    settlements: Mutex<HashMap<InvocationScope, SettlementResult>>,
    terminal: Mutex<Option<TurnResult>>,
    operation: Mutex<()>,
    admission: Mutex<()>,
    active: Mutex<Option<MockInvocation>>,
    cancelled_submission: Mutex<Option<SubmissionId>>,
    history_boundary: Mutex<Option<CheckpointId>>,
}

#[derive(Clone, Copy)]
enum MockBindingState {
    Open,
    Closing,
    Invalidated,
}

struct MockInvocation {
    scope: InvocationScope,
    cancelled: watch::Sender<bool>,
    settled: watch::Sender<Option<SettlementResult>>,
    phase: MockInputPhase,
}

/// Retain the native scripted operation without coupling its worker to a result waiter.
struct MockTurnDriver {
    session: Arc<MockSession>,
    scope: InvocationScope,
    input: Mutex<Option<MockRun>>,
}

struct MockRun {
    control: Arc<MockControl>,
    request: PromptRequest,
    script: Option<TestPromptScript>,
    cancelled: watch::Receiver<bool>,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for MockTurnDriver {
    /// Run the one admitted native script and retain exact terminal evidence.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
        let input = self
            .input
            .lock()
            .await
            .take()
            .expect("the retained runner starts its native driver once");
        let MockRun {
            control,
            request,
            script,
            cancelled,
            accepted,
        } = input;
        run_prompt(
            self.session.clone(),
            control,
            request,
            script,
            cancelled,
            accepted,
            start,
        )
        .await?;
        self.session
            .terminal
            .lock()
            .await
            .clone()
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
    }
    /// Signal the original script's cancellation channel without changing its result.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        if let Some(active) = self
            .session
            .active
            .lock()
            .await
            .as_ref()
            .filter(|active| active.scope == self.scope)
        {
            active.cancelled.send_replace(true);
        }
        Ok(())
    }
    /// Observe synchronous script/callback collection after its run future released ownership.
    async fn collect(&self) -> Result<(), ProviderError> {
        Ok(())
    }
}

struct MockSteering {
    text: String,
    submission: Option<SubmissionId>,
    covered: Option<CoveredInput>,
}

struct ResponseHold {
    id: TestHoldId,
    owner: ThreadOwner,
    any_owner: bool,
    mode: Option<PromptMode>,
    state: watch::Sender<HoldState>,
}

#[derive(Default)]
struct MockControl {
    holds: std::sync::Mutex<VecDeque<Arc<ResponseHold>>>,
    scripts: std::sync::Mutex<HashMap<ThreadOwner, VecDeque<testing::TestPromptScript>>>,
}

impl MockControl {
    fn find(
        &self,
        owner: &ThreadOwner,
        id: &TestHoldId,
    ) -> Result<Arc<ResponseHold>, ProviderError> {
        self.holds
            .lock()
            .unwrap()
            .iter()
            .find(|hold| hold.owner == *owner && hold.id == *id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))
    }

    fn remove(&self, id: &TestHoldId) {
        self.holds.lock().unwrap().retain(|hold| hold.id != *id);
    }
}

#[async_trait]
impl ProviderTestControl for MockControl {
    async fn dispatch(
        &self,
        owner: &ThreadOwner,
        command: TestControlCommand,
    ) -> Result<TestControlResult, ProviderError> {
        match command {
            TestControlCommand::Hold { mode, any_owner } => {
                let id = TestHoldId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown)
                })?;
                let (state, _) = watch::channel(HoldState::Armed);
                self.holds.lock().unwrap().push_back(Arc::new(ResponseHold {
                    id: id.clone(),
                    owner: owner.clone(),
                    any_owner,
                    mode,
                    state,
                }));
                Ok(TestControlResult::Held { id })
            }
            TestControlCommand::Wait { id } => {
                let mut state = self.find(owner, &id)?.state.subscribe();
                loop {
                    match state.borrow_and_update().clone() {
                        HoldState::Pending(response) => {
                            return Ok(TestControlResult::Pending { response });
                        }
                        HoldState::Cancelled => return Err(ProviderError::Cancelled),
                        HoldState::Armed | HoldState::Released => {}
                    }
                    state.changed().await.map_err(|_| {
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown)
                    })?;
                }
            }
            TestControlCommand::Release { id } => {
                let hold = self.find(owner, &id)?;
                if !matches!(*hold.state.borrow(), HoldState::Pending(_)) {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::Rejected,
                    ));
                }
                hold.state.send_replace(HoldState::Released);
                self.remove(&id);
                Ok(TestControlResult::Released { id })
            }
            TestControlCommand::Cancel { id } => {
                let hold = self.find(owner, &id)?;
                hold.state.send_replace(HoldState::Cancelled);
                self.remove(&id);
                Ok(TestControlResult::Cancelled { id })
            }
            TestControlCommand::Script { script } => {
                self.scripts
                    .lock()
                    .unwrap()
                    .entry(owner.clone())
                    .or_default()
                    .push_back(script);
                Ok(TestControlResult::Scripted)
            }
        }
    }

    async fn arrive(
        &self,
        owner: &ThreadOwner,
        mode: PromptMode,
        prompt: String,
    ) -> Result<(), ProviderError> {
        let hold = {
            let holds = self.holds.lock().unwrap();
            holds
                .iter()
                .find(|hold| {
                    (hold.any_owner || hold.owner == *owner)
                        && hold.mode.is_none_or(|expected| expected == mode)
                        && matches!(*hold.state.borrow(), HoldState::Armed)
                })
                .cloned()
        };
        let Some(hold) = hold else {
            return Ok(());
        };
        let pending = PendingTestResponse {
            id: hold.id.clone(),
            owner: owner.clone(),
            mode,
            prompt,
        };
        hold.state.send_replace(HoldState::Pending(pending));
        let mut state = hold.state.subscribe();
        loop {
            match state.borrow_and_update().clone() {
                HoldState::Released => return Ok(()),
                HoldState::Cancelled => return Err(ProviderError::Cancelled),
                HoldState::Armed | HoldState::Pending(_) => {}
            }
            state
                .changed()
                .await
                .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown))?;
        }
    }

    fn cancel_owner(&self, owner: Option<&ThreadOwner>) {
        self.holds.lock().unwrap().retain(|hold| {
            let matched_owner = owner.is_none_or(|owner| hold.owner == *owner
                || matches!(&*hold.state.borrow(), HoldState::Pending(PendingTestResponse { owner: actual, .. }) if actual == owner));
            if matched_owner {
                hold.state.send_replace(HoldState::Cancelled);
                false
            } else { true }
        });
        // Queued scripts describe future prompts, not work owned by the cancelled
        // invocation. Only provider shutdown discards those unconsumed scenarios.
        if owner.is_none() {
            self.scripts.lock().unwrap().clear();
        }
    }

    fn take_script(&self, owner: &ThreadOwner) -> Option<testing::TestPromptScript> {
        let mut scripts = self.scripts.lock().unwrap();
        let queue = scripts.get_mut(owner)?;
        let script = queue.pop_front();
        if queue.is_empty() {
            scripts.remove(owner);
        }
        script
    }
}

struct PromptProgress {
    delivery: PromptDelivery,
    start: NativeTurnStart,
    covered: Vec<CoveredInput>,
}

/// Reject admission immediately after local authority revocation.
fn ensure_open(session: &MockSession, operation: ProviderOperation) -> Result<(), ProviderError> {
    let state = *session
        .state
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    match state {
        MockBindingState::Open => Ok(()),
        MockBindingState::Closing => Err(ProviderFailure::new(
            operation,
            ProviderFailureKind::BindingClosed,
        )
        .at_phase(ProviderPhase::Admission, FailureDisposition::Invalidated)
        .into()),
        MockBindingState::Invalidated => Err(ProviderFailure::new(
            operation,
            ProviderFailureKind::BindingInvalidated,
        )
        .at_phase(ProviderPhase::Admission, FailureDisposition::Invalidated)
        .into()),
    }
}

/// Reject unsupported mock input configuration before the receipt records dispatch.
fn validate_invocation(request: &PromptRequest) -> Result<(), ProviderError> {
    request.validate_turn_identity()?;
    validate_model_selection(&request.configuration.model)
}

/// Validate every normalized model option the deterministic mock advertises.
fn validate_model_selection(model: &session::ModelSelection) -> Result<(), ProviderError> {
    let descriptor =
        crate::registry::descriptor(&ProviderId::new("mock").expect("mock provider ID is valid"))
            .expect("mock is registered");
    model
        .service_tier
        .validate(&descriptor.capability)
        .map_err(|_| capability::UnsupportedCapability(capability::ProviderFeature::ServiceTier))?;
    if model
        .model
        .as_deref()
        .is_some_and(|model| model != "mock-model")
    {
        return Err(failure(
            ProviderOperation::ThreadConfigure,
            ProviderFailureKind::Rejected,
        ));
    }
    if model
        .context_window
        .as_deref()
        .is_some_and(|window| window != "default")
    {
        return Err(capability::UnsupportedCapability(
            capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    if let generation::ReasoningSelection::Level(level) = &model.reasoning
        && !["low", "medium", "high"].contains(&level.as_str())
    {
        return Err(capability::UnsupportedCapability(
            capability::ProviderFeature::EffortSelection,
        )
        .into());
    }
    Ok(())
}

/// Record a definite no-send result while preserving an independent receipt failure.
async fn reject_before_dispatch(
    receipt: &Arc<dyn submission::InputReceipt>,
    cause: ProviderError,
) -> ProviderError {
    match receipt.clone().record_rejected(cause.clone()).await {
        Ok(()) => ProviderError::PreparationFailed(Box::new(cause)),
        Err(cleanup) => ProviderError::PreparationCleanup {
            cause: Box::new(cause),
            cleanup: Box::new(cleanup),
        },
    }
}

async fn run_prompt(
    session: Arc<MockSession>,
    control: Arc<MockControl>,
    request: PromptRequest,
    script: Option<TestPromptScript>,
    cancelled: watch::Receiver<bool>,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
    start: NativeTurnStart,
) -> Result<(), ProviderError> {
    let mut progress = PromptProgress {
        delivery: PromptDelivery::Preparing,
        start,
        covered: request.input_coverage(),
    };
    let mut accepted = Some(accepted);
    let result = run_prompt_inner(
        session.clone(),
        control,
        &request,
        script,
        cancelled,
        &mut accepted,
        &mut progress,
    )
    .await;
    match result {
        Ok(()) => accept_prompt(&session, &request, &mut accepted, &mut progress).await,
        Err(error) => Err(error),
    }
}

async fn run_prompt_inner(
    session: Arc<MockSession>,
    control: Arc<MockControl>,
    request: &PromptRequest,
    script: Option<TestPromptScript>,
    mut cancelled: watch::Receiver<bool>,
    accepted: &mut Option<tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>>,
    progress: &mut PromptProgress,
) -> Result<(), ProviderError> {
    let _operation = session.operation.lock().await;
    if *cancelled.borrow() {
        return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
    }

    let owner = session.owner.clone();
    let scope = request.scope.clone();
    let sink = session.sink.invocation_sink(&scope)?.activity_writer();
    let token = RecoveryToken::new(
        format!(
            "mock-sending\n{}\n{}\n{}",
            request.submission_id.as_str(),
            request.attempt_id.as_str(),
            session
                .binding
                .provider_session_id
                .as_ref()
                .map(ProviderSessionId::as_str)
                .unwrap_or_default()
        )
        .into_bytes(),
    )
    .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown))?;
    request
        .receipt
        .clone()
        .record_sending(Some(token))
        .await
        .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
    ensure_open(&session, ProviderOperation::TurnStart)?;
    request.dispatch.confirm();
    progress.delivery = PromptDelivery::Dispatched;
    let timing = script
        .as_ref()
        .map(|script| script.acceptance)
        .unwrap_or_default();
    if timing == testing::TestInputAcceptance::BeforeResponse {
        accept_prompt(&session, request, accepted, progress).await?;
    }

    // A cancellation observed after dispatch reaches the arrival select below, which retains an
    // interrupted terminal. Returning an error here would invalidate the binding instead.
    let turn = TurnAddress {
        scope: scope.clone(),
        thread_id: request.thread_id.clone(),
        turn_id: request.turn_id.clone(),
    };
    progress.start.observed(now_ms()).await?;
    if let Some(model_id) = session.invocation_model.lock().await.clone() {
        sink.send_wait(AdapterActivity::Turn {
            thread_id: turn.thread_id.clone(),
            turn_id: turn.turn_id.clone(),
            event: TurnMetadataEvent::ModelResolved(ResolvedModel {
                model_id: ModelId::new(model_id).map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                })?,
                observed_at_ms: now_ms(),
            }),
        })
        .await?;
    }
    let mut prompt_parts = Vec::new();
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        prompt_parts.push(format!(
            "Earlier user input {} was saved locally but not accepted:",
            index + 1
        ));
        if !recovered.instructions.is_empty() {
            prompt_parts.push(recovered.instructions.clone());
        }
        prompt_parts.extend(
            recovered
                .attachments
                .iter()
                .map(|attachment| attachment.context.clone()),
        );
        prompt_parts.push(render_input(&recovered.input));
    }
    if !request.recovered_inputs.is_empty() {
        prompt_parts.push("Newest user input:".into());
    }
    // Harness workflow instructions are a provider contract, not user-visible prompt text, so
    // response holds and echoes report only what the user and task supplied.
    if let Some(task) = &request.task {
        prompt_parts.push(task.heading());
    }
    prompt_parts.extend(
        request
            .attachments
            .iter()
            .map(|attachment| attachment.context.clone()),
    );
    prompt_parts.push(render_input(&request.input));
    let prompt = prompt_parts.join("\n\n");
    let hold = tokio::select! {
        result = control.arrive(&owner, request.mode, prompt.clone()) => result,
        _ = cancelled.wait_for(|value| *value) => Err(ProviderError::Cancelled),
    };
    let steering = {
        let mut active = session.active.lock().await;
        let invocation = active
            .as_mut()
            .filter(|active| active.scope == scope)
            .ok_or_else(|| {
                ProviderError::Failure(
                    ProviderFailure::new(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    )
                    .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated),
                )
            })?;
        match std::mem::replace(&mut invocation.phase, MockInputPhase::Responding) {
            MockInputPhase::Receiving(steering) => steering,
            MockInputPhase::Responding => Vec::new(),
        }
    };
    let mut submissions = vec![request.submission_id.clone()];
    submissions.extend(steering.iter().filter_map(|input| input.submission.clone()));
    progress
        .covered
        .extend(steering.iter().filter_map(|input| input.covered.clone()));
    if let Err(error) = hold {
        if matches!(error, ProviderError::Cancelled) {
            retain_terminal(
                &session,
                &sink,
                &scope,
                Some(turn.clone()),
                TurnOutcome::Interrupted,
                &progress.covered,
                None,
            )
            .await?;
            *session.cancelled_submission.lock().await = Some(request.submission_id.clone());
            return Ok(());
        }
        return Err(error);
    }

    let unscripted = script.is_none();
    let script = match script {
        Some(script) => script,
        None => {
            let steering = if steering.is_empty() {
                String::new()
            } else {
                format!(
                    "\n\nSteering constraints:\n{}",
                    steering
                        .iter()
                        .map(|input| format!("- {}", input.text))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            };
            let mut steps = planning_steps(request, &prompt);
            steps.push(TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: format!("Mock response: {prompt}{steering}"),
            });
            TestPromptScript {
                acceptance: testing::TestInputAcceptance::BeforeResponse,
                steps,
                terminal: TestPromptTerminal::Completed,
            }
        }
    };
    let steps = if script.steps.is_empty() {
        vec![TestPromptStep::Message {
            content: TestMessageContent::Response,
            text: format!("Mock response: {prompt}"),
        }]
    } else {
        script.steps.clone()
    };
    if unscripted {
        let configured = tokio::select! {
            result = configure_unset_checks(&session, request, &prompt, &sink, &turn) => result,
            _ = cancelled.wait_for(|value| *value) => Err(ProviderError::Cancelled),
        };
        if let Err(error) = configured {
            if matches!(error, ProviderError::Cancelled) {
                retain_terminal(
                    &session,
                    &sink,
                    &scope,
                    Some(turn),
                    TurnOutcome::Interrupted,
                    &progress.covered,
                    None,
                )
                .await?;
                return Ok(());
            }
            return Err(error);
        }
    }
    for step in steps {
        match step {
            TestPromptStep::Message { content, text } => {
                let item_id = ItemId::new(uuid::Uuid::new_v4().to_string())
                    .expect("allocated item ID is nonempty");
                let start = match content {
                    TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Full) => {
                        ItemStart::Reasoning(ReasoningStart {
                            text: text.clone(),
                            kind: ReasoningKind::Full,
                        })
                    }
                    TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Summary) => {
                        ItemStart::Reasoning(ReasoningStart {
                            text: text.clone(),
                            kind: ReasoningKind::Summary,
                        })
                    }
                    TestMessageContent::Commentary => ItemStart::AgentMessage(AgentMessageStart {
                        text: text.clone(),
                        classification: MessageClassification::Known(AgentMessageRole::Commentary),
                    }),
                    TestMessageContent::Response => ItemStart::AgentMessage(AgentMessageStart {
                        text: text.clone(),
                        classification: MessageClassification::Known(AgentMessageRole::Response),
                    }),
                };
                let completion = MessageCompletion {
                    text,
                    outcome: MessageOutcome::Completed,
                };
                let completion = match content {
                    TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Full)
                    | TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Summary) => {
                        ItemCompletion::Reasoning(completion)
                    }
                    TestMessageContent::Commentary | TestMessageContent::Response => {
                        ItemCompletion::AgentMessage(completion)
                    }
                };
                sink.send_wait(AdapterActivity::Item {
                    thread_id: turn.thread_id.clone(),
                    turn_id: turn.turn_id.clone(),
                    item_id: item_id.clone(),
                    event: ItemEvent::Started(start),
                })
                .await?;
                sink.send_wait(AdapterActivity::Item {
                    thread_id: turn.thread_id.clone(),
                    turn_id: turn.turn_id.clone(),
                    item_id,
                    event: ItemEvent::Completed(completion),
                })
                .await?;
            }
            TestPromptStep::Checklist { update } => {
                sink.send_wait(AdapterActivity::Turn {
                    thread_id: turn.thread_id.clone(),
                    turn_id: turn.turn_id.clone(),
                    event: forge_provider_api::TurnMetadataEvent::ChecklistUpdated(update),
                })
                .await?;
            }
            TestPromptStep::ToolCall { name, arguments } => {
                let called = tokio::select! {
                    result = run_tool_call(&session, request, &sink, &turn, &name, arguments) => result.map(drop),
                    _ = cancelled.wait_for(|value| *value) => Err(ProviderError::Cancelled),
                };
                if let Err(error) = called {
                    if matches!(error, ProviderError::Cancelled) {
                        retain_terminal(
                            &session,
                            &sink,
                            &scope,
                            Some(turn),
                            TurnOutcome::Interrupted,
                            &progress.covered,
                            None,
                        )
                        .await?;
                        return Ok(());
                    }
                    return Err(error);
                }
            }
        }
    }
    let terminal = script.terminal.clone();
    let outcome = match &terminal {
        TestPromptTerminal::Completed => TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        },
        TestPromptTerminal::Cancelled => TurnOutcome::Interrupted,
        TestPromptTerminal::Failed(_) => TurnOutcome::Failed {
            error: forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Unknown,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
            retry: forge_provider_api::turn::RetryDecision::NotAllowed,
        },
        TestPromptTerminal::ObservationLost(_) => TurnOutcome::Interrupted,
        TestPromptTerminal::RejectedBeforeDispatch(_) | TestPromptTerminal::DeliveryUncertain => {
            unreachable!("pre-dispatch terminals are handled before task creation")
        }
    };
    if let TestPromptTerminal::ObservationLost(failure) = terminal.clone() {
        return Err(ProviderError::Observation(failure));
    }
    let failure = match terminal {
        TestPromptTerminal::Failed(kind) => Some(kind),
        _ => None,
    };
    retain_terminal(
        &session,
        &sink,
        &scope,
        Some(turn),
        outcome,
        &progress.covered,
        failure,
    )
    .await?;
    Ok(())
}

/// Commit native acceptance once, independently of dispatch and response observation.
async fn accept_prompt(
    session: &MockSession,
    request: &PromptRequest,
    accepted: &mut Option<tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>>,
    progress: &mut PromptProgress,
) -> Result<(), ProviderError> {
    if matches!(progress.delivery, PromptDelivery::Accepted) {
        return Ok(());
    }
    let provider_session_id = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str())
        .unwrap_or_default();
    let receipt = RecoveryToken::new(
        format!(
            "mock-accepted\n{}\n{}\n{provider_session_id}",
            request.submission_id.as_str(),
            request.attempt_id.as_str()
        )
        .into_bytes(),
    )
    .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown))?;
    if let Some(history) = &session.binding.provider_session_id {
        session.receipts.lock().await.insert(
            (request.submission_id.clone(), request.attempt_id.clone()),
            AcceptedInput {
                provider_session_id: history.clone(),
                receipt: receipt.clone(),
            },
        );
    }
    request
        .receipt
        .clone()
        .record_accepted(Some(receipt))
        .await?;
    progress.delivery = PromptDelivery::Accepted;
    let acceptance = InputAcceptance {
        submission_id: request.submission_id.clone(),
    };
    if let Some(accepted) = accepted.take() {
        let _ = accepted.send(Ok(acceptance));
    }
    Ok(())
}

/// Draft and submit one plan through the host plan tools when an unscripted Plan turn carries a
/// declaration design.
///
/// Harness renders the canonical design as a JSON block in the planning instructions or, on a
/// continuation, in the turn input. The overview edit names that document's version so every
/// revision changes content; the mock then adds one declaration file and submits the version
/// those edits produce. Turns without plan tools or a readable design keep the plain response.
fn planning_steps(request: &PromptRequest, prompt: &str) -> Vec<TestPromptStep> {
    let has_plan_tools = ["plan_overview_edit", "plan_file_edit", "plan_submit"]
        .iter()
        .all(|name| request.tools.iter().any(|tool| tool.name == *name));
    if request.mode != PromptMode::Plan || !has_plan_tools {
        return Vec::new();
    }
    // A revision keeps the original design in its instructions and renders the current design in
    // its input, so the newer version names the document the edits must revise.
    let Some(declaration) = [
        declaration_identity(&request.instructions),
        declaration_identity(prompt),
    ]
    .into_iter()
    .flatten()
    .max_by_key(|identity| identity.version) else {
        return Vec::new();
    };
    let objective = declaration.request.trim();
    let mut steps = vec![TestPromptStep::ToolCall {
        name: "plan_overview_edit".into(),
        arguments: serde_json::json!({
            "plan_id": declaration.plan_id,
            "expected_version": declaration.version,
            "title": "Implement the requested change",
            "objective": if objective.is_empty() { "Implement the requested change." } else { objective },
            "background": format!(
                "The mock provider drafted this overview from declaration version {}.",
                declaration.version
            ),
            "requirements": ["Preserve the requested behavior at its owning boundary."],
            "design": "Apply the requested behavior at its owning boundary.",
            "verification": ["Confirm the requested interface behaves as designed."],
        }),
    }];
    let mut submitted_version = declaration.version + 1;
    // A revision already declares the mock file, and adding it again would reject the edit.
    if !declaration.declares_mock_file {
        steps.push(TestPromptStep::ToolCall {
            name: "plan_file_edit".into(),
            arguments: serde_json::json!({
                "plan_id": declaration.plan_id,
                "expected_version": submitted_version,
                "patch": MOCK_DECLARATION_PATCH,
            }),
        });
        submitted_version += 1;
    }
    steps.push(TestPromptStep::ToolCall {
        name: "plan_submit".into(),
        arguments: serde_json::json!({
            "plan_id": declaration.plan_id,
            "expected_version": submitted_version,
        }),
    });
    steps
}

/// Workspace-relative declaration file that the mock planner adds.
const MOCK_DECLARATION_PATH: &str = "src/change.rs";

/// Declaration file the mock planner proposes: one new public callable with the Algorithm that
/// Harness requires of every added callable.
const MOCK_DECLARATION_PATCH: &str = "*** Begin Patch\n*** Add File: src/change.rs\n\
    +/// Apply the requested behavior.\n+pub fn requested_change();\n+Algorithm\n+  return none\n\
    *** End Patch";

/// Identity of the declaration design a planning turn must revise.
struct DeclarationIdentity {
    plan_id: String,
    version: u64,
    request: String,
    /// The design already declares [`MOCK_DECLARATION_PATH`].
    declares_mock_file: bool,
}

/// Read the first fenced JSON block that names a plan and its version.
fn declaration_identity(text: &str) -> Option<DeclarationIdentity> {
    let mut remaining = text;
    while let Some(offset) = remaining.find("```json\n") {
        let json_start = offset + "```json\n".len();
        let json_end = json_start + remaining[json_start..].find("\n```")?;
        if let Ok(document) =
            serde_json::from_str::<serde_json::Value>(&remaining[json_start..json_end])
            && let (Some(plan_id), Some(version)) = (
                document.get("plan_id").and_then(serde_json::Value::as_str),
                document.get("version").and_then(serde_json::Value::as_u64),
            )
        {
            let request = document
                .get("request")
                .and_then(serde_json::Value::as_str)
                .unwrap_or_default();
            let declares_mock_file = document
                .pointer("/declaration_files/paths")
                .and_then(serde_json::Value::as_array)
                .is_some_and(|paths| {
                    paths
                        .iter()
                        .any(|path| path.as_str() == Some(MOCK_DECLARATION_PATH))
                });
            return Some(DeclarationIdentity {
                plan_id: plan_id.to_owned(),
                version,
                request: request.to_owned(),
                declares_mock_file,
            });
        }
        remaining = &remaining[json_end + "\n```".len()..];
    }
    None
}

/// Execute a scripted host tool while retaining its original item ownership.
async fn run_tool_call(
    session: &MockSession,
    request: &PromptRequest,
    sink: &AdapterActivityWriter,
    turn: &TurnAddress,
    name: &str,
    arguments: serde_json::Value,
) -> Result<Option<String>, ProviderError> {
    if !request.tools.iter().any(|tool| tool.name == name) {
        return Err(failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Rejected,
        ));
    }
    let item_id =
        ItemId::new(uuid::Uuid::new_v4().to_string()).expect("allocated tool identity is nonempty");
    let item = |event| AdapterActivity::Item {
        thread_id: turn.thread_id.clone(),
        turn_id: turn.turn_id.clone(),
        item_id: item_id.clone(),
        event,
    };
    let descriptor = ToolDescriptor {
        semantics: Default::default(),
        name: name.into(),
        title: name.into(),
        arguments: InspectableArguments::new(arguments.clone()),
    };
    sink.send_wait(item(ItemEvent::Started(ItemStart::ToolCall(descriptor))))
        .await?;
    let permission = PermissionRequest {
        id: uuid::Uuid::new_v4().to_string(),
        provider: ProviderId::new("mock").expect("mock ID is valid"),
        reason: None,
        target_list: vec![PermissionTarget::HostTool { name: name.into() }],
    };
    ensure_open(session, ProviderOperation::TurnStart)?;
    request.permissions.before_tool(&request.scope).await?;
    let (outcome, output, cancelled) = match request
        .permissions
        .authorize(&request.scope, permission)
        .await
    {
        PermissionResponse::Allow => {
            ensure_open(session, ProviderOperation::TurnStart)?;
            sink.send_wait(item(ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            }))
            .await?;
            match request
                .provider_tools
                .call(
                    &request.scope,
                    control::ToolCall {
                        id: control::ToolCallId::new(item_id.as_str())
                            .expect("allocated tool identity is nonempty"),
                        name: name.into(),
                        arguments,
                    },
                )
                .await
            {
                Ok(output) => (ToolOutcome::Succeeded, Some(output), false),
                Err(_) => (
                    ToolOutcome::Failed {
                        diagnostic: ToolDiagnostic {
                            kind: ProviderFailureKind::Unknown,
                            message: SanitizedMessage::new("host tool failed")
                                .expect("static tool failure is sanitized"),
                        },
                    },
                    None,
                    false,
                ),
            }
        }
        PermissionResponse::Deny { reason } => (
            ToolOutcome::Denied {
                reason: SanitizedMessage::new(
                    reason
                        .filter(|text| !text.trim().is_empty())
                        .unwrap_or_else(|| "Permission denied".into()),
                )
                .expect("denial fallback is nonempty"),
            },
            None,
            false,
        ),
        PermissionResponse::Cancelled => (ToolOutcome::Cancelled, None, true),
    };
    let result = output.clone();
    sink.send_wait(item(ItemEvent::Completed(ItemCompletion::ToolCall(
        ToolCompletion {
            outcome,
            output,
            descriptor: None,
            changes: Default::default(),
        },
    ))))
    .await?;
    if cancelled {
        return Err(ProviderError::Cancelled);
    }
    Ok(result)
}

/// Save the project's current check catalog when an unscripted planning or check-setup turn finds
/// checks unconfigured or invalid.
///
/// Planning needs a saved check configuration before it can select coverage. Harness asks for that
/// configuration in a Chat-mode turn that directs the provider to the harness-checks skill, so the
/// mock treats a Plan turn or a turn naming that skill as setup. It reads the catalog, then submits
/// its valid definitions through check_definition_edit, which follows the session's tool-approval
/// mode like any other edit. An invalid configuration lists no definitions and reports its parse
/// error, so the resubmission replaces it with an empty valid configuration.
async fn configure_unset_checks(
    session: &MockSession,
    request: &PromptRequest,
    prompt: &str,
    sink: &AdapterActivityWriter,
    turn: &TurnAddress,
) -> Result<(), ProviderError> {
    let has_check_tools = [
        "check_definition_list",
        "check_definition_details",
        "check_definition_edit",
    ]
    .iter()
    .all(|name| request.tools.iter().any(|tool| tool.name == *name));
    let requests_setup = request.mode == PromptMode::Plan || prompt.contains("harness-checks");
    if !requests_setup || !has_check_tools {
        return Ok(());
    }
    let read = |output: Option<String>| -> Result<serde_json::Value, ProviderError> {
        output
            .and_then(|text| serde_json::from_str(&text).ok())
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
    };
    let catalog = read(
        run_tool_call(
            session,
            request,
            sink,
            turn,
            "check_definition_list",
            serde_json::json!({}),
        )
        .await?,
    )?;
    let needs_setup =
        catalog["configured"] == serde_json::Value::Bool(false) || !catalog["error"].is_null();
    if !needs_setup {
        return Ok(());
    }
    let mut config = serde_json::Map::new();
    for definition in catalog["definitions"].as_array().into_iter().flatten() {
        let Some(check_id) = definition["check_id"].as_str() else {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ));
        };
        let details = read(
            run_tool_call(
                session,
                request,
                sink,
                turn,
                "check_definition_details",
                serde_json::json!({ "check_id": check_id }),
            )
            .await?,
        )?;
        config.insert(check_id.to_owned(), details["definition"].clone());
    }
    run_tool_call(
        session,
        request,
        sink,
        turn,
        "check_definition_edit",
        serde_json::json!({ "expected_revision": catalog["revision"], "config": config }),
    )
    .await?;
    Ok(())
}

/// Retain native completion evidence without publishing a lifecycle event.
async fn retain_terminal(
    session: &MockSession,
    _sink: &AdapterActivityWriter,
    _scope: &InvocationScope,
    _address: Option<TurnAddress>,
    outcome: TurnOutcome,
    covered: &[CoveredInput],
    failure_kind: Option<ProviderFailureKind>,
) -> Result<(), ProviderError> {
    if matches!(outcome, TurnOutcome::Interrupted) {
        *session.cancelled_submission.lock().await =
            covered.first().map(|input| input.submission_id().clone());
    }
    let checkpoint = covered.last().map(|input| {
        CheckpointId::new(input.submission_id().as_str())
            .expect("covered journal input is nonempty")
    });
    *session.history_boundary.lock().await = checkpoint.clone();
    let outcome = match outcome {
        TurnOutcome::Failed { error, retry } => TurnOutcome::Failed {
            error: ProviderFailure::new(error.operation(), failure_kind.unwrap_or(error.kind()))
                .at_phase(error.phase(), error.disposition()),
            retry,
        },
        TurnOutcome::Completed { .. } | TurnOutcome::Interrupted => outcome,
    };
    *session.terminal.lock().await = Some(
        TurnResult::new(
            outcome,
            now_ms(),
            None,
            covered.to_vec(),
            vec![],
            checkpoint,
        )
        .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?,
    );
    Ok(())
}

fn render_input(input: &PromptInput) -> String {
    match input {
        PromptInput::Text(text) => text.clone(),
        PromptInput::Skill { name, arguments } => format!("/{name} {arguments}"),
        PromptInput::QuestionReply(reply) => {
            let response = match &reply.response {
                QuestionReplyResponse::Answer {
                    answer,
                    required_questions_remain,
                } => format!(
                    "Answer: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
                QuestionReplyResponse::Clarification {
                    options,
                    text,
                    reply_to,
                } => {
                    let options = options
                        .iter()
                        .map(|option| {
                            format!(
                                "{} [{}]: {}{}",
                                option.label,
                                option.id,
                                option.description,
                                if option.recommended {
                                    " (recommended)"
                                } else {
                                    ""
                                }
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!(
                        "Clarification: {text}\nOptions:\n{options}\nReply to: {}",
                        reply_to.as_deref().unwrap_or("none")
                    )
                }
                QuestionReplyResponse::Correction {
                    answer,
                    required_questions_remain,
                } => format!(
                    "Correction: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
            };
            format!(
                "Question response for {} (revision {}, message {}):\n{}\n{}",
                reply.question_id, reply.revision, reply.message_id, reply.question, response
            )
        }
    }
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
        .try_into()
        .unwrap_or(i64::MAX)
}

/// Classify a mock operation rejection before native effects.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}
/// Wait on retained collection evidence without holding adapter admission.
async fn wait_settlement(
    receiver: &mut watch::Receiver<Option<SettlementResult>>,
) -> Result<CompletedTurn, ProviderError> {
    loop {
        if let Some(result) = receiver.borrow_and_update().clone() {
            return result;
        }
        receiver.changed().await.map_err(|_| {
            failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::Unknown,
            )
        })?;
    }
}

/// Get the deterministic fixture's normalized baseline capability snapshot.
fn mock_capabilities() -> capability::CapabilityManifest {
    let mut manifest = capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(&ProviderId::new("mock").expect("mock ID is valid"))
            .expect("mock is registered"),
    );
    manifest.task_steering = vec![
        input::TaskKind::Goal,
        input::TaskKind::Plan,
        input::TaskKind::Execute,
    ];
    manifest
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use forge_provider_api::ProviderEventStream;
    use forge_provider_api::{CompletedTurn, ProviderConnectionEvent, TurnEvent};
    use forge_provider_api::{
        control::{ProviderTools, ToolCall},
        permissions::{PermissionRequest, PermissionResponse, ProviderPermissions},
        submission::InputReceipt,
        turn_control::{HostTurnStatus, TurnControl},
    };
    use std::sync::Mutex as StdMutex;

    /// Create independent bounded host operation authority for one fixture call.
    fn test_context() -> OperationContext {
        let (_, cancellation) = OperationCancellation::channel();
        OperationContext::new(
            OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap()
    }

    struct Trace;
    impl trace::ProviderTrace for Trace {
        fn record(&self, _: trace::ProviderTraceRecord) {}
    }

    #[derive(Default)]
    struct ReceiptState {
        sending: bool,
        accepted: bool,
        sending_token: Option<RecoveryToken>,
    }
    struct Receipt {
        id: SubmissionId,
        state: StdMutex<ReceiptState>,
    }
    #[async_trait]
    impl InputReceipt for Receipt {
        fn id(&self) -> &SubmissionId {
            &self.id
        }
        fn attempt_id(&self) -> Result<DeliveryAttemptId, ProviderError> {
            Ok(DeliveryAttemptId::new(format!("{}-attempt", self.id.as_str())).unwrap())
        }
        async fn record_sending(
            self: Arc<Self>,
            token: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.state.lock().unwrap().sending_token = token;
            self.state.lock().unwrap().sending = true;
            Ok(())
        }
        async fn record_accepted(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.state.lock().unwrap().accepted = true;
            Ok(())
        }
        async fn record_rejected(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_failure(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    struct Permissions;
    #[async_trait]
    impl ProviderPermissions for Permissions {
        async fn authorize(&self, _: &InvocationScope, _: PermissionRequest) -> PermissionResponse {
            PermissionResponse::Allow
        }
        async fn before_tool(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    struct Tools;
    #[async_trait]
    impl ProviderTools for Tools {
        async fn call(&self, _: &InvocationScope, _: ToolCall) -> Result<String, ProviderError> {
            Ok("fixture tool result".into())
        }
    }

    struct Control;
    #[async_trait]
    impl TurnControl for Control {
        async fn before_send(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn receive(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn close(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn status(&self, _: &InvocationScope) -> Result<HostTurnStatus, ProviderError> {
            Ok(HostTurnStatus {
                replay_safe: false,
                control_handoff: false,
                cancelled: false,
                error: None,
            })
        }
        fn tool_finished(&self, _: &InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    struct Effects;
    #[async_trait]
    impl ProviderHostEffects for Effects {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &InvocationScope,
        ) -> Result<(), session::HostEffectFailure> {
            Ok(())
        }
    }

    async fn initialized(
        provider: &MockProvider,
    ) -> (SessionBinding, ThreadOwner, ProviderEventStream) {
        let owner = make_owner("mock-lifecycle");
        let binding = provider
            .thread_open(
                test_context(),
                SessionRequest {
                    thread_id: (owner.clone()).thread_id(),
                    owner: owner.clone(),
                    mode: SessionMode::Interactive,
                    workspace: ".".into(),
                    history: SessionHistory::Durable { resume: None },
                    configuration: configuration(),
                    resource_lease: None,
                    tools: Vec::new(),
                    tool_endpoint: None,
                    system_instructions: String::new(),
                    trace: Arc::new(Trace),
                },
            )
            .await
            .unwrap();
        (binding.binding, owner, binding.events)
    }

    fn prompt(
        binding: &BindingId,
        owner: ThreadOwner,
        exchange: &str,
        receipt: Arc<Receipt>,
    ) -> PromptRequest {
        let scope = InvocationScope::new(owner, exchange);
        let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
            binding.clone(),
            scope.clone(),
            receipt.id.clone(),
            receipt.attempt_id().unwrap(),
        );
        PromptRequest {
            thread_id: (scope).owner.thread_id(),
            turn_id: (scope)
                .turn_id()
                .expect("admitted scope has a nonempty host turn identity"),

            scope,
            dispatch,
            mode: PromptMode::Chat,
            configuration: session::InvocationConfiguration {
                model: Default::default(),
                access: Default::default(),
                approval: ApprovalMode::Ask,
            },
            task: None,
            submission_id: receipt.id.clone(),
            attempt_id: receipt.attempt_id().unwrap(),
            input: PromptInput::Text("hello".into()),
            recovered_inputs: Vec::new(),
            instructions: String::new(),
            attachments: Vec::new(),
            tools: Vec::new(),
            receipt,
            tool_endpoint: None,
            resource_lease: None,
            permissions: Arc::new(Permissions),
            provider_tools: Arc::new(Tools),
            turn_control: Arc::new(Control),
            host_effects: Arc::new(Effects),
        }
    }

    fn make_owner(name: &str) -> ThreadOwner {
        ThreadOwner::root(name)
    }

    #[tokio::test]
    async fn normalized_response_holds_match_owner_mode_and_release_once() {
        let control = MockControl::default();
        let owner = make_owner("session");
        let held = control
            .dispatch(
                &owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::PlanQuestion),
                    any_owner: false,
                },
            )
            .await
            .unwrap();
        let TestControlResult::Held { id } = held else {
            panic!("hold result missing")
        };
        assert!(
            control
                .dispatch(
                    &make_owner("other"),
                    TestControlCommand::Wait { id: id.clone() }
                )
                .await
                .is_err()
        );
        assert!(
            control
                .dispatch(&owner, TestControlCommand::Release { id: id.clone() })
                .await
                .is_err()
        );
        assert!(
            control
                .arrive(&owner, PromptMode::Chat, "wrong mode".into())
                .await
                .is_ok()
        );
        let arriving = {
            let control = Arc::new(control);
            let arrival_control = control.clone();
            let arriving = tokio::spawn(async move {
                arrival_control
                    .arrive(
                        &make_owner("session"),
                        PromptMode::PlanQuestion,
                        "normalized prompt".into(),
                    )
                    .await
            });
            let pending = control
                .dispatch(
                    &make_owner("session"),
                    TestControlCommand::Wait { id: id.clone() },
                )
                .await
                .unwrap();
            let TestControlResult::Pending { response } = pending else {
                panic!("pending response missing")
            };
            assert_eq!(response.prompt, "normalized prompt");
            control
                .dispatch(
                    &make_owner("session"),
                    TestControlCommand::Release { id: id.clone() },
                )
                .await
                .unwrap();
            assert!(arriving.await.unwrap().is_ok());
            control
        };
        assert!(arriving.holds.lock().unwrap().is_empty());
        assert!(
            arriving
                .dispatch(&make_owner("session"), TestControlCommand::Release { id })
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn owner_cancellation_wakes_any_owner_response_waiter() {
        let control = Arc::new(MockControl::default());
        let source = make_owner("source");
        let target = make_owner("target");
        let held = control
            .dispatch(
                &source,
                TestControlCommand::Hold {
                    mode: None,
                    any_owner: true,
                },
            )
            .await
            .unwrap();
        let TestControlResult::Held { id } = held else {
            panic!("hold result missing")
        };
        let arrival_control = control.clone();
        let arrival_target = target.clone();
        let arrival = tokio::spawn(async move {
            arrival_control
                .arrive(&arrival_target, PromptMode::Chat, "prompt".into())
                .await
        });
        let observed = control
            .dispatch(&source, TestControlCommand::Wait { id })
            .await
            .unwrap();
        assert!(
            matches!(observed, TestControlResult::Pending { response } if response.owner == target)
        );
        control.cancel_owner(Some(&target));
        assert!(matches!(
            arrival.await.unwrap(),
            Err(ProviderError::Cancelled)
        ));
    }

    /// Retire the old generation without holding the registry lock needed by retirement.
    /// Get a complete session configuration for lifecycle fixtures.
    fn configuration() -> SessionConfiguration {
        SessionConfiguration {
            model: Default::default(),
            access: Default::default(),
            approval: ApprovalMode::Ask,
            resources: Default::default(),
        }
    }

    /// Create a receipt with independent durable acceptance tracking.
    fn receipt(id: &str) -> Arc<Receipt> {
        Arc::new(Receipt {
            id: id.try_into().unwrap(),
            state: StdMutex::new(ReceiptState::default()),
        })
    }

    /// Arm the next response barrier for this owner.
    async fn hold(provider: &MockProvider, owner: &ThreadOwner) -> TestHoldId {
        let TestControlResult::Held { id } = provider
            .fixture_control()
            .dispatch(
                owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_owner: false,
                },
            )
            .await
            .unwrap()
        else {
            panic!("hold not registered")
        };
        id
    }

    /// Wait for native settlement while preserving the ordered binding stream.
    async fn settled(stream: &mut ProviderEventStream) -> CompletedTurn {
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                let event = stream.recv().await.unwrap().unwrap();
                if let ProviderEvent::Turn {
                    event: TurnEvent::Completed(completed),
                    ..
                } = event.event
                {
                    assert!(completed.collected_at_ms() >= completed.result().completed_at_ms());
                    return completed;
                }
            }
        })
        .await
        .unwrap()
    }
    /// Publish actual native history before any prompt and keep compaction identities canonical.
    #[tokio::test]
    async fn opened_native_history_is_published_before_compaction() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let identity = events.recv().await.unwrap().unwrap();
        match identity.event {
            ProviderEvent::Thread {
                thread_id,
                event: ThreadEvent::IdentityResolved(history),
            } => {
                assert_eq!(thread_id, owner.thread_id());
                assert_eq!(Some(history), binding.provider_session_id);
            }
            event => panic!("native open did not publish its actual identity: {event:?}"),
        }
        assert!(
            events.try_next().unwrap().is_none(),
            "native open manufactured a response lifecycle"
        );
        let context = test_context();
        let result = provider
            .thread_compact(
                context.clone(),
                &binding.id,
                CompactionRequest {
                    scope: EventScope::SessionOperation {
                        owner: owner.clone(),
                        operation_id: context.id().clone(),
                    },
                },
            )
            .await
            .unwrap();
        assert_eq!(result.compaction.thread_id, owner.thread_id().as_str());
        match events.recv().await.unwrap().unwrap().event {
            ProviderEvent::Thread {
                thread_id,
                event: ThreadEvent::CompactionUpdated(compaction),
            } => {
                assert_eq!(thread_id, owner.thread_id());
                assert_eq!(compaction.thread_id, thread_id.as_str());
                assert_eq!(compaction.id, context.id().as_str());
            }
            event => panic!("native compaction changed observation family: {event:?}"),
        }
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Preserve terminal/collection ordering and prevent repeated cancellation from touching a successor.
    #[tokio::test]
    async fn automatic_settlement_and_exact_cancel_preserve_successor() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let request = prompt(&binding.id, owner.clone(), "first", receipt("first"));
        let scope = request.scope.clone();
        provider
            .turn_start(test_context(), &binding.id, request)
            .await
            .unwrap();
        let mut started_sequence = None;
        let first = tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                let event = events.recv().await.unwrap().unwrap();
                match event.event {
                    ProviderEvent::Turn {
                        event: TurnEvent::Started(_),
                        ..
                    } => started_sequence = Some(event.sequence),
                    ProviderEvent::Turn {
                        thread_id,
                        turn_id,
                        event: TurnEvent::Completed(completed),
                    } => {
                        assert!(started_sequence.unwrap() < event.sequence);
                        assert_eq!(thread_id, scope.owner.thread_id());
                        assert_eq!(turn_id, scope.turn_id().unwrap());
                        assert!(
                            completed.collected_at_ms() >= completed.result().completed_at_ms()
                        );
                        break completed;
                    }
                    ProviderEvent::Provider { .. }
                    | ProviderEvent::Thread { .. }
                    | ProviderEvent::Turn { .. }
                    | ProviderEvent::Item { .. }
                    | ProviderEvent::Agent { .. } => {}
                }
            }
        })
        .await
        .unwrap();
        assert!(matches!(
            first.result().outcome(),
            TurnOutcome::Completed { .. }
        ));
        let id = hold(&provider, &owner).await;
        let request = prompt(&binding.id, owner.clone(), "second", receipt("second"));
        let successor = request.scope.clone();
        provider
            .turn_start(test_context(), &binding.id, request)
            .await
            .unwrap();
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id })
            .await
            .unwrap();
        let repeated = provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: scope.clone(),
                },
            )
            .await
            .unwrap();
        assert_eq!(repeated.turn_id, scope.turn_id().unwrap());
        assert_eq!(
            provider
                .binding(&binding.id, ProviderOperation::TurnStart)
                .await
                .unwrap()
                .active
                .lock()
                .await
                .as_ref()
                .unwrap()
                .scope,
            successor
        );
        let cancelled = provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: successor.clone(),
                },
            )
            .await
            .unwrap();
        let interrupted = settled(&mut events).await;
        assert_eq!(interrupted.result().outcome(), TurnOutcome::Interrupted);
        assert_eq!(cancelled.turn_id, successor.turn_id().unwrap());
        let repeated = provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest { scope: successor },
            )
            .await
            .unwrap();
        assert_eq!(cancelled.watermark, repeated.watermark);
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Reject altered owner, receipt, attempt, and transport authority before recording native dispatch.
    #[tokio::test]
    async fn input_admission_rejects_exact_authority_mismatches_without_dispatch() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, _events) = initialized(&provider).await;
        let receipt = receipt("authority");
        let original = prompt(&binding.id, owner.clone(), "authority", receipt.clone());
        let mut requests = Vec::new();
        let mut changed = original.clone();
        changed.scope.owner = make_owner("foreign");
        requests.push(changed);
        let mut changed = original.clone();
        changed.submission_id = SubmissionId::try_from("different-receipt").unwrap();
        requests.push(changed);
        let mut changed = original.clone();
        changed.attempt_id = DeliveryAttemptId::new("different-attempt").unwrap();
        requests.push(changed);
        let mut changed = original;
        changed.dispatch = forge_provider_api::submission::DispatchReporter::channel(
            BindingId::new("different-binding").unwrap(),
            changed.scope.clone(),
            changed.submission_id.clone(),
            changed.attempt_id.clone(),
        )
        .0;
        requests.push(changed);
        for request in requests {
            let error = provider
                .turn_start(test_context(), &binding.id, request)
                .await
                .unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure)
                if failure.kind() == ProviderFailureKind::WrongInvocation
                    && failure.operation() == ProviderOperation::TurnStart
                    && failure.phase() == ProviderPhase::Admission
                    && failure.disposition() == FailureDisposition::Unchanged));
        }
        assert!(!receipt.state.lock().unwrap().sending);
        assert!(!receipt.state.lock().unwrap().accepted);
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
        let closed_receipt = self::receipt("closed");
        let input = prompt(&binding.id, owner, "closed", closed_receipt.clone());
        let scope = input.scope.clone();
        let error = provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::BindingClosed
                && failure.operation() == ProviderOperation::TurnStart
                && failure.phase() == ProviderPhase::Admission
                && failure.disposition() == FailureDisposition::Closed));
        let error = provider
            .turn_steer(
                test_context(),
                &binding.id,
                SteeringRequest {
                    scope,
                    delivery: session::SteeringDelivery::Journaled {
                        receipt: closed_receipt.clone(),
                    },
                    input: PromptInput::Text("closed".into()),
                    attachments: Vec::new(),
                    task: None,
                },
            )
            .await
            .unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::BindingClosed
                && failure.operation() == ProviderOperation::TurnSteer
                && failure.phase() == ProviderPhase::Admission
                && failure.disposition() == FailureDisposition::Closed));
        assert!(!closed_receipt.state.lock().unwrap().sending);
        assert!(!closed_receipt.state.lock().unwrap().accepted);
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Reject stale steering instead of starting another execution under the old invocation.
    #[tokio::test]
    async fn steering_after_settlement_never_submits_new_input() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let request = prompt(&binding.id, owner, "first", receipt("first"));
        let scope = request.scope.clone();
        provider
            .turn_start(test_context(), &binding.id, request)
            .await
            .unwrap();
        settled(&mut events).await;
        let result = provider
            .turn_steer(
                test_context(),
                &binding.id,
                SteeringRequest {
                    scope,
                    delivery: session::SteeringDelivery::Journaled {
                        receipt: receipt("late"),
                    },
                    input: PromptInput::Text("late".into()),
                    attachments: Vec::new(),
                    task: None,
                },
            )
            .await;
        assert!(
            matches!(result, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation
                && failure.operation() == ProviderOperation::TurnSteer
                && failure.phase() == ProviderPhase::Admission
                && failure.disposition() == FailureDisposition::Unchanged)
        );
        assert!(
            provider
                .binding(&binding.id, ProviderOperation::TurnStart)
                .await
                .unwrap()
                .active
                .lock()
                .await
                .is_none()
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Reject configuration during execution and preserve defaults across invocation overrides.
    #[tokio::test]
    async fn configuration_requires_settlement_and_invocation_overrides_are_temporary() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let id = hold(&provider, &owner).await;
        let mut request = prompt(&binding.id, owner.clone(), "first", receipt("first"));
        request.configuration.model.model = Some("mock-model".into());
        provider
            .turn_start(test_context(), &binding.id, request)
            .await
            .unwrap();
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id: id.clone() })
            .await
            .unwrap();
        let rejected = provider
            .thread_configure(test_context(), &binding.id, configuration())
            .await;
        assert!(
            matches!(rejected, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::ExecutionActive)
        );
        assert!(
            provider
                .binding(&binding.id, ProviderOperation::TurnStart)
                .await
                .unwrap()
                .model
                .lock()
                .await
                .model
                .is_none()
        );
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Release { id })
            .await
            .unwrap();
        settled(&mut events).await;
        let applied = provider
            .thread_configure(test_context(), &binding.id, configuration())
            .await
            .unwrap();
        assert!(applied.configuration.model.model.is_none());
        provider
            .turn_start(
                test_context(),
                &binding.id,
                prompt(&binding.id, owner, "second", receipt("second")),
            )
            .await
            .unwrap();
        let result = tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                let event = events.recv().await.unwrap().unwrap();
                assert!(
                    !matches!(
                        event.event,
                        ProviderEvent::Turn {
                            event: TurnEvent::ModelResolved(_),
                            ..
                        }
                    ),
                    "an invocation override must not become the next turn's confirmed model"
                );
                if let ProviderEvent::Turn {
                    event: TurnEvent::Completed(completed),
                    ..
                } = event.event
                {
                    break completed;
                }
            }
        })
        .await
        .unwrap();
        assert!(matches!(
            result.result().outcome(),
            TurnOutcome::Completed { .. }
        ));
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Keep a late native acceptance discoverable after close without inventing negative evidence.
    #[tokio::test]
    async fn reconcile_late_acceptance_after_binding_loss_is_attempt_scoped() {
        let provider = Arc::new(MockProvider::new().unwrap());
        let (binding, owner, mut events) = initialized(&provider).await;
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: testing::TestInputAcceptance::AfterResponse,
                        steps: Vec::new(),
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await
            .unwrap();
        let id = hold(&provider, &owner).await;
        let receipt = receipt("late");
        let request = prompt(&binding.id, owner.clone(), "late", receipt.clone());
        let attempt_id = request.attempt_id.clone();
        let task_provider = provider.clone();
        let task_binding = binding.id.clone();
        let submitted = tokio::spawn(async move {
            task_provider
                .turn_start(test_context(), &task_binding, request)
                .await
        });
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id: id.clone() })
            .await
            .unwrap();
        let query = InputReconcileRequest {
            submission_id: receipt.id.clone(),
            attempt_id,
            provider_session_id: binding.provider_session_id.clone(),
            token: receipt.state.lock().unwrap().sending_token.clone(),
            accepted_receipts: Vec::new(),
        };
        assert_eq!(
            provider
                .input_reconcile(test_context(), query.clone())
                .await
                .unwrap(),
            SubmissionEvidence::Unknown
        );
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Release { id })
            .await
            .unwrap();
        submitted.await.unwrap().unwrap();
        settled(&mut events).await;
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
        assert!(matches!(
            provider
                .input_reconcile(test_context(), query.clone())
                .await
                .unwrap(),
            SubmissionEvidence::Accepted { .. }
        ));
        let mut stale = query;
        stale.attempt_id = DeliveryAttemptId::new("other-attempt").unwrap();
        assert_eq!(
            provider
                .input_reconcile(test_context(), stale)
                .await
                .unwrap(),
            SubmissionEvidence::Unknown
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Allow terminal-only acceptance to coexist with cancellation and collection ownership.
    #[tokio::test]
    async fn terminal_only_acceptance_does_not_block_cancel() {
        let provider = Arc::new(MockProvider::new().unwrap());
        let (binding, owner, mut events) = initialized(&provider).await;
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: testing::TestInputAcceptance::AfterResponse,
                        steps: Vec::new(),
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await
            .unwrap();
        let id = hold(&provider, &owner).await;
        let request = prompt(&binding.id, owner.clone(), "cancel", receipt("cancel"));
        let scope = request.scope.clone();
        let task_provider = provider.clone();
        let task_binding = binding.id.clone();
        let submitted = tokio::spawn(async move {
            task_provider
                .turn_start(test_context(), &task_binding, request)
                .await
        });
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id })
            .await
            .unwrap();
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            provider.turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: scope.clone(),
                },
            ),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result.turn_id, scope.turn_id().unwrap());
        submitted.await.unwrap().unwrap();
        settled(&mut events).await;
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Reject stale bindings after close and preserve a fresh resumed binding's identity.
    #[tokio::test]
    async fn closed_binding_cannot_reconfigure_or_cancel_replacement() {
        let provider = MockProvider::new().unwrap();
        let (first, _, _events) = initialized(&provider).await;
        provider
            .thread_close(test_context(), &first.id)
            .await
            .unwrap();
        provider
            .thread_close(test_context(), &first.id)
            .await
            .unwrap();
        let (second, _, _events) = initialized(&provider).await;
        assert_ne!(first.id, second.id);
        assert!(
            provider
                .thread_configure(test_context(), &first.id, configuration())
                .await
                .is_err()
        );
        assert!(
            provider
                .binding(&second.id, ProviderOperation::TurnStart)
                .await
                .is_ok()
        );
        provider.shutdown(test_context()).await.unwrap();
    }
    /// Require the complete isolation policy before admitting any inherited authority.
    #[tokio::test]
    async fn isolated_sessions_reject_resources_and_do_not_change_persistent_configuration() {
        let provider = MockProvider::new().unwrap();
        let (persistent, _, _events) = initialized(&provider).await;
        let request = SessionRequest {
            thread_id: (make_owner("isolated")).thread_id(),
            owner: make_owner("isolated"),
            mode: SessionMode::Background,
            workspace: ".".into(),
            history: SessionHistory::IsolatedText(IsolatedTextPolicy::new()),
            configuration: configuration(),
            resource_lease: None,
            tools: Vec::new(),
            tool_endpoint: None,
            system_instructions: "Supplied instructions only".into(),
            trace: Arc::new(Trace),
        };
        let mut invalid = request.clone();
        invalid
            .configuration
            .resources
            .mcp
            .push(session::McpLaunch {
                name: "inherited".into(),
                transport: session::McpTransport::Stdio {
                    command: "unused".into(),
                    arguments: Vec::new(),
                    environment: Default::default(),
                },
                timeout: None,
                working_directory: None,
                tools: None,
            });
        assert!(
            matches!(provider.thread_open(test_context(), invalid).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Policy)
        );
        let opened = provider.thread_open(test_context(), request).await.unwrap();
        let isolated_id = opened.binding.id.clone();
        let mut events = opened.events;
        let mut input = prompt(
            &isolated_id,
            make_owner("isolated"),
            "utility",
            receipt("utility"),
        );
        input.input = PromptInput::Text("temporary text".into());
        provider
            .turn_start(test_context(), &isolated_id, input)
            .await
            .unwrap();
        settled(&mut events).await;
        provider
            .thread_close(test_context(), &isolated_id)
            .await
            .unwrap();
        assert!(
            provider
                .binding(&isolated_id, ProviderOperation::TurnStart)
                .await
                .is_err()
        );
        assert!(
            provider
                .binding(&persistent.id, ProviderOperation::TurnStart)
                .await
                .unwrap()
                .model
                .lock()
                .await
                .model
                .is_none()
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Replay one exact rollback result without removing any subsequent history item.
    #[tokio::test]
    async fn retraction_is_exact_and_retries_cannot_remove_new_history() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let id = hold(&provider, &owner).await;
        let cancelled_receipt = receipt("cancelled");
        let input = prompt(
            &binding.id,
            owner.clone(),
            "cancelled",
            cancelled_receipt.clone(),
        );
        let scope = input.scope.clone();
        let attempt_id = input.attempt_id.clone();
        provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap();
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id })
            .await
            .unwrap();
        provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: scope.clone(),
                },
            )
            .await
            .unwrap();
        settled(&mut events).await;
        let request = InputRetractRequest {
            scope,
            submission_id: cancelled_receipt.id.clone(),
            attempt_id,
            boundary: CheckpointId::new(cancelled_receipt.id.as_str()).unwrap(),
            authorization: RetractionAuthorization::OutputFreeCancellation,
        };
        let retraction_context = test_context();
        let retracted = provider
            .input_retract(retraction_context.clone(), &binding.id, request.clone())
            .await
            .unwrap();
        assert!(matches!(retracted, InputRetractOutcome::Retracted { .. }));
        let next_receipt = receipt("next");
        let next = prompt(&binding.id, owner, "next", next_receipt.clone());
        let next_scope = next.scope.clone();
        let next_attempt = next.attempt_id.clone();
        provider
            .turn_start(test_context(), &binding.id, next)
            .await
            .unwrap();
        settled(&mut events).await;
        let normal_completion = InputRetractRequest {
            scope: next_scope,
            submission_id: next_receipt.id.clone(),
            attempt_id: next_attempt,
            boundary: CheckpointId::new(next_receipt.id.as_str()).unwrap(),
            authorization: RetractionAuthorization::OutputFreeCancellation,
        };
        assert_eq!(
            provider
                .input_retract(test_context(), &binding.id, normal_completion)
                .await
                .unwrap(),
            InputRetractOutcome::Unavailable(InputRetractUnavailable::NotCancelled)
        );
        assert_eq!(
            provider
                .input_retract(retraction_context.clone(), &binding.id, request.clone())
                .await
                .unwrap(),
            retracted
        );
        assert_eq!(
            provider
                .binding(&binding.id, ProviderOperation::TurnStart)
                .await
                .unwrap()
                .history_boundary
                .lock()
                .await
                .as_ref()
                .unwrap()
                .as_str(),
            "next"
        );
        let mut wrong_invocation = request.clone();
        wrong_invocation.scope = InvocationScope::new(make_owner("mock-lifecycle"), "other");
        assert_eq!(
            provider
                .input_retract(test_context(), &binding.id, wrong_invocation)
                .await
                .unwrap(),
            InputRetractOutcome::Unavailable(InputRetractUnavailable::WrongInvocation)
        );
        let mut wrong_boundary = request.clone();
        wrong_boundary.boundary = CheckpointId::new("other-boundary").unwrap();
        assert_eq!(
            provider
                .input_retract(test_context(), &binding.id, wrong_boundary)
                .await
                .unwrap(),
            InputRetractOutcome::Unavailable(InputRetractUnavailable::HistoryBoundaryChanged)
        );
        let stale = request;
        assert_eq!(
            provider
                .input_retract(test_context(), &binding.id, stale)
                .await
                .unwrap(),
            InputRetractOutcome::Unavailable(InputRetractUnavailable::HistoryBoundaryChanged)
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Preserve a queued scenario when a second submit loses atomic admission.
    #[tokio::test]
    async fn occupied_submission_preserves_script_for_next_prompt() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let id = hold(&provider, &owner).await;
        let input = prompt(&binding.id, owner.clone(), "first", receipt("first"));
        let scope = input.scope.clone();
        provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap();
        provider
            .fixture_control()
            .dispatch(&owner, TestControlCommand::Wait { id })
            .await
            .unwrap();
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: Default::default(),
                        steps: Vec::new(),
                        terminal: TestPromptTerminal::RejectedBeforeDispatch(
                            ProviderFailureKind::Rejected,
                        ),
                    },
                },
            )
            .await
            .unwrap();
        let rejected = provider
            .turn_start(
                test_context(),
                &binding.id,
                prompt(&binding.id, owner.clone(), "occupied", receipt("occupied")),
            )
            .await;
        assert!(
            matches!(rejected, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::ExecutionActive
                && failure.operation() == ProviderOperation::TurnStart
                && failure.phase() == ProviderPhase::Admission
                && failure.disposition() == FailureDisposition::Unchanged)
        );
        provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: scope.clone(),
                },
            )
            .await
            .unwrap();
        settled(&mut events).await;
        let rejected = provider
            .turn_start(
                test_context(),
                &binding.id,
                prompt(&binding.id, owner, "next", receipt("next")),
            )
            .await;
        assert!(matches!(rejected, Err(ProviderError::PreparationFailed(_))));
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Hold permission authorization until cancellation drops the provider callback future.
    struct WaitingPermissions {
        entered: tokio::sync::Notify,
        collected: std::sync::atomic::AtomicBool,
    }

    struct CallbackLifetime<'a>(&'a std::sync::atomic::AtomicBool);

    impl Drop for CallbackLifetime<'_> {
        fn drop(&mut self) {
            self.0.store(true, std::sync::atomic::Ordering::Release);
        }
    }

    #[async_trait]
    impl ProviderPermissions for WaitingPermissions {
        async fn authorize(&self, _: &InvocationScope, _: PermissionRequest) -> PermissionResponse {
            let _callback = CallbackLifetime(&self.collected);
            self.entered.notify_one();
            std::future::pending().await
        }
        async fn before_tool(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    /// Collect a blocked provider callback before advertising successful close and settlement.
    #[tokio::test]
    async fn close_collects_permission_wait_before_settlement_without_observation_loss() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: Default::default(),
                        steps: vec![TestPromptStep::ToolCall {
                            name: "waiting_tool".into(),
                            arguments: serde_json::json!({}),
                        }],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await
            .unwrap();
        let permissions = Arc::new(WaitingPermissions {
            entered: tokio::sync::Notify::new(),
            collected: std::sync::atomic::AtomicBool::new(false),
        });
        let mut input = prompt(&binding.id, owner, "permission", receipt("permission"));
        input.tools.push(control::ControlToolDefinition {
            name: "waiting_tool",
            description: "Wait for authorization",
            input_schema: serde_json::json!({}),
        });
        input.permissions = permissions.clone();
        provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap();
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            permissions.entered.notified(),
        )
        .await
        .unwrap();
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
        assert!(
            permissions
                .collected
                .load(std::sync::atomic::Ordering::Acquire)
        );
        loop {
            let event = events.recv().await.unwrap().unwrap();
            assert!(!matches!(
                &event.event,
                ProviderEvent::Provider {
                    event: ProviderConnectionEvent::Unavailable(_)
                }
            ));
            if let ProviderEvent::Turn {
                event: TurnEvent::Completed(completed),
                ..
            } = event.event
            {
                assert_eq!(completed.result().outcome(), TurnOutcome::Interrupted);
                break;
            }
        }
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Keep a collected binding's first failure when its close request is retried.
    #[tokio::test]
    async fn failed_settlement_is_scoped_and_close_replays_collected_failure() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: Default::default(),
                        steps: Vec::new(),
                        terminal: TestPromptTerminal::ObservationLost(
                            ObservationFailure::Disconnected,
                        ),
                    },
                },
            )
            .await
            .unwrap();
        let input = prompt(&binding.id, owner, "failure", receipt("failure"));
        provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                let event = events.recv().await.unwrap().unwrap();
                if let ProviderEvent::Provider {
                    event: ProviderConnectionEvent::Unavailable(unavailable),
                } = event.event
                {
                    let failure = unavailable.failure;
                    assert_eq!(failure.phase(), ProviderPhase::Observation);
                    assert_eq!(failure.disposition(), FailureDisposition::Invalidated);
                    break;
                }
            }
        })
        .await
        .unwrap();
        let error = provider
            .thread_configure(test_context(), &binding.id, configuration())
            .await
            .unwrap_err();
        assert!(matches!(error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::BindingInvalidated
                && failure.operation() == ProviderOperation::ThreadConfigure
                && failure.phase() == ProviderPhase::Admission
                && failure.disposition() == FailureDisposition::Invalidated));
        for _ in 0..2 {
            let error = provider
                .thread_close(test_context(), &binding.id)
                .await
                .unwrap_err();
            assert!(matches!(
                error,
                ProviderError::Observation(ObservationFailure::Disconnected)
            ));
        }
        assert!(
            provider
                .binding(&binding.id, ProviderOperation::ThreadClose)
                .await
                .is_err()
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    struct PendingPreparationControl {
        entered: tokio::sync::Notify,
        destroyed: std::sync::atomic::AtomicBool,
    }

    struct PreparationLifetime<'a>(&'a std::sync::atomic::AtomicBool);

    impl Drop for PreparationLifetime<'_> {
        /// Record destruction of the actual preparation future before cancellation reports collection.
        fn drop(&mut self) {
            self.0.store(true, std::sync::atomic::Ordering::Release);
        }
    }

    #[async_trait]
    impl TurnControl for PendingPreparationControl {
        async fn before_send(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            let _lifetime = PreparationLifetime(&self.destroyed);
            self.entered.notify_one();
            std::future::pending().await
        }
        async fn receive(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
            Control.receive(scope).await
        }
        async fn close(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
            Control.close(scope).await
        }
        async fn status(&self, scope: &InvocationScope) -> Result<HostTurnStatus, ProviderError> {
            Control.status(scope).await
        }
        fn tool_finished(&self, _: &InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    /// Join exact native preparation before absent invocation evidence can authorize owner reuse.
    #[tokio::test]
    async fn cancellation_and_close_collect_pending_preparation_before_returning() {
        let provider = Arc::new(MockProvider::new().unwrap());
        let (binding, owner, mut events) = initialized(&provider).await;
        for close in [false, true] {
            let control = Arc::new(PendingPreparationControl {
                entered: tokio::sync::Notify::new(),
                destroyed: std::sync::atomic::AtomicBool::new(false),
            });
            let receipt = receipt(if close {
                "pending-close"
            } else {
                "pending-cancel"
            });
            let mut input = prompt(
                &binding.id,
                owner.clone(),
                receipt.id.as_str(),
                receipt.clone(),
            );
            input.turn_control = control.clone();
            let scope = input.scope.clone();
            let submitted_provider = provider.clone();
            let submitted_binding = binding.id.clone();
            let submitted = tokio::spawn(async move {
                submitted_provider
                    .turn_start(test_context(), &submitted_binding, input)
                    .await
            });
            tokio::time::timeout(
                std::time::Duration::from_secs(2),
                control.entered.notified(),
            )
            .await
            .unwrap();
            if close {
                tokio::time::timeout(
                    std::time::Duration::from_secs(2),
                    provider.thread_close(test_context(), &binding.id),
                )
                .await
                .unwrap()
                .unwrap();
            } else {
                let error = tokio::time::timeout(
                    std::time::Duration::from_secs(2),
                    provider.turn_interrupt(
                        test_context(),
                        &binding.id,
                        CancelRequest {
                            scope: scope.clone(),
                        },
                    ),
                )
                .await
                .unwrap()
                .unwrap_err();
                assert!(
                    matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::WrongInvocation)
                );
            }
            assert!(control.destroyed.load(std::sync::atomic::Ordering::Acquire));
            let error = submitted.await.unwrap().unwrap_err();
            if close {
                assert!(matches!(error, ProviderError::Failure(failure)
                    if failure.kind() == ProviderFailureKind::BindingClosed
                        && failure.operation() == ProviderOperation::TurnStart
                        && failure.phase() == ProviderPhase::Admission
                        && failure.disposition() == FailureDisposition::Invalidated));
            } else {
                assert!(matches!(error, ProviderError::PreparationFailed(cause)
                    if matches!(*cause, ProviderError::Failure(failure)
                        if failure.kind() == ProviderFailureKind::Cancelled
                            && failure.operation() == ProviderOperation::TurnStart
                            && failure.phase() == ProviderPhase::Admission
                            && failure.disposition() == FailureDisposition::Unchanged)));
            }
            assert!(!receipt.state.lock().unwrap().sending);
            assert!(!receipt.state.lock().unwrap().accepted);
            if !close {
                provider
                    .turn_start(
                        test_context(),
                        &binding.id,
                        prompt(
                            &binding.id,
                            owner.clone(),
                            "successor",
                            self::receipt("successor"),
                        ),
                    )
                    .await
                    .unwrap();
                assert!(matches!(
                    settled(&mut events).await.result().outcome(),
                    TurnOutcome::Completed { .. }
                ));
            }
        }
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Keep an exact cancellation fence when the original submit future has never been polled.
    #[tokio::test]
    async fn cancellation_before_submit_first_poll_prevents_late_dispatch() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, _events) = initialized(&provider).await;
        let receipt = receipt("late-first-poll");
        let input = prompt(&binding.id, owner, "late-first-poll", receipt.clone());
        let scope = input.scope.clone();
        let submitted = provider.turn_start(test_context(), &binding.id, input);
        let error = provider
            .turn_interrupt(
                test_context(),
                &binding.id,
                CancelRequest {
                    scope: scope.clone(),
                },
            )
            .await
            .unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::WrongInvocation)
        );
        let error = submitted.await.unwrap_err();
        assert!(matches!(error, ProviderError::PreparationFailed(cause)
            if matches!(*cause, ProviderError::Failure(failure)
                if failure.kind() == ProviderFailureKind::Cancelled
                    && failure.operation() == ProviderOperation::TurnStart
                    && failure.phase() == ProviderPhase::Admission
                    && failure.disposition() == FailureDisposition::Unchanged)));
        assert!(!receipt.state.lock().unwrap().sending);
        assert!(!receipt.state.lock().unwrap().accepted);
        provider.shutdown(test_context()).await.unwrap();
    }

    struct PendingHostBarriers;

    #[async_trait]
    impl TurnControl for PendingHostBarriers {
        async fn before_send(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn receive(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn close(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            std::future::pending().await
        }
        async fn status(&self, scope: &InvocationScope) -> Result<HostTurnStatus, ProviderError> {
            Control.status(scope).await
        }
        fn tool_finished(&self, _: &InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    #[async_trait]
    impl ProviderHostEffects for PendingHostBarriers {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &InvocationScope,
        ) -> Result<(), session::HostEffectFailure> {
            std::future::pending().await
        }
    }

    /// Publish native collection without waiting on host work which itself awaits provider settlement.
    #[tokio::test]
    async fn provider_settlement_does_not_await_host_collection_barriers() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let mut input = prompt(
            &binding.id,
            owner,
            "host-barriers",
            receipt("host-barriers"),
        );
        input.turn_control = Arc::new(PendingHostBarriers);
        input.host_effects = Arc::new(PendingHostBarriers);
        provider
            .turn_start(test_context(), &binding.id, input)
            .await
            .unwrap();
        let settlement = settled(&mut events).await;
        assert!(matches!(
            settlement.result().outcome(),
            TurnOutcome::Completed { .. }
        ));
        provider.shutdown(test_context()).await.unwrap();
    }

    /// Preserve one immutable fork operation across retries without accepting altered authority.
    #[tokio::test]
    async fn fork_retry_rejects_changed_source_target_and_configuration() {
        let provider = MockProvider::new().unwrap();
        let (binding, _, _events) = initialized(&provider).await;
        let request = SessionForkRequest {
            source: session::ForkSource::Binding(binding.id.clone()),
            target: session::ForkTarget {
                owner: make_owner("fork-target"),
                workspace: ".".into(),
                configuration: configuration(),
                system_instructions: "original instructions".into(),
            },
        };
        let fork_context = test_context();
        let result = provider
            .thread_fork(fork_context.clone(), request.clone())
            .await
            .unwrap();
        assert_eq!(
            provider
                .thread_fork(fork_context.clone(), request.clone())
                .await
                .unwrap()
                .provider_session_id,
            result.provider_session_id
        );
        let mut changed = Vec::new();
        let mut source = request.clone();
        source.source = session::ForkSource::Binding(BindingId::new("another-generation").unwrap());
        changed.push(source);
        let mut owner = request.clone();
        owner.target.owner = make_owner("another-target");
        changed.push(owner);
        let mut workspace = request.clone();
        workspace.target.workspace = "another-workspace".into();
        changed.push(workspace);
        let mut instructions = request.clone();
        instructions.target.system_instructions = "different instructions".into();
        changed.push(instructions);
        let mut model = request.clone();
        model.target.configuration.model.model = Some("mock-model".into());
        changed.push(model);
        let mut access = request.clone();
        access.target.configuration.access.sandbox = !access.target.configuration.access.sandbox;
        changed.push(access);
        let mut approval = request.clone();
        approval.target.configuration.approval = ApprovalMode::Allow;
        changed.push(approval);
        let mut resources = request.clone();
        resources
            .target
            .configuration
            .resources
            .mcp
            .push(session::McpLaunch {
                name: "changed-resource".into(),
                transport: session::McpTransport::Stdio {
                    command: "unused".into(),
                    arguments: vec!["changed-argument".into()],
                    environment: Default::default(),
                },
                timeout: None,
                working_directory: None,
                tools: None,
            });
        changed.push(resources);
        for altered in changed {
            assert!(
                matches!(provider.thread_fork(fork_context.clone(), altered).await, Err(ProviderError::Failure(failure))
                if failure.operation() == ProviderOperation::ThreadFork && failure.kind() == ProviderFailureKind::OperationConflict)
            );
        }
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
        assert_eq!(
            provider
                .thread_fork(fork_context, request)
                .await
                .unwrap()
                .provider_session_id,
            result.provider_session_id
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    struct DelayedAcceptanceReceipt {
        receipt: Arc<Receipt>,
        entered: tokio::sync::Notify,
        release: tokio::sync::Notify,
    }

    #[async_trait]
    impl InputReceipt for DelayedAcceptanceReceipt {
        fn id(&self) -> &SubmissionId {
            &self.receipt.id
        }
        fn attempt_id(&self) -> Result<DeliveryAttemptId, ProviderError> {
            self.receipt.attempt_id()
        }
        async fn record_sending(
            self: Arc<Self>,
            token: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.receipt.clone().record_sending(token).await
        }
        async fn record_accepted(
            self: Arc<Self>,
            token: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.entered.notify_one();
            self.release.notified().await;
            self.receipt.clone().record_accepted(token).await
        }
        async fn record_rejected(
            self: Arc<Self>,
            error: ProviderError,
        ) -> Result<(), ProviderError> {
            self.receipt.clone().record_rejected(error).await
        }
        async fn record_failure(
            self: Arc<Self>,
            error: ProviderError,
        ) -> Result<(), ProviderError> {
            self.receipt.clone().record_failure(error).await
        }
    }

    /// Keep a timed-out operation owned until late acceptance and exact native collection prove its result.
    #[tokio::test]
    async fn operation_interruption_retains_late_acceptance_and_replays_without_resending() {
        let provider = Arc::new(MockProvider::new().unwrap());
        let (binding, owner, mut events) = initialized(&provider).await;
        provider
            .fixture_control()
            .dispatch(
                &owner,
                TestControlCommand::Script {
                    script: TestPromptScript {
                        acceptance: testing::TestInputAcceptance::AfterResponse,
                        steps: Vec::new(),
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await
            .unwrap();
        let receipt = receipt("retained-operation");
        let delayed = Arc::new(DelayedAcceptanceReceipt {
            receipt: receipt.clone(),
            entered: tokio::sync::Notify::new(),
            release: tokio::sync::Notify::new(),
        });
        let mut request = prompt(&binding.id, owner, "retained-operation", receipt.clone());
        request.receipt = delayed.clone();
        let retained_request = request.clone();
        let (source, cancellation) = OperationCancellation::channel();
        let context = OperationContext::new(
            OperationId::new("retained-submit-operation").unwrap(),
            std::time::Duration::from_secs(10),
            cancellation,
        )
        .unwrap();
        let retained_context = context.clone();
        let submitted_provider = provider.clone();
        let submitted_binding = binding.id.clone();
        let submitted = tokio::spawn(async move {
            submitted_provider
                .turn_start(context, &submitted_binding, request)
                .await
        });
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            delayed.entered.notified(),
        )
        .await
        .unwrap();
        source.cancel();
        let observed = submitted.await.unwrap().unwrap_err();
        assert!(
            matches!(observed, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() == FailureDisposition::Uncertain)
        );
        assert!(!receipt.state.lock().unwrap().accepted);
        delayed.release.notify_one();
        tokio::time::timeout(
            std::time::Duration::from_secs(2),
            provider.operations.turn_start.join_all(),
        )
        .await
        .unwrap()
        .unwrap();
        let accepted = provider
            .turn_start(retained_context, &binding.id, retained_request)
            .await
            .unwrap();
        assert_eq!(accepted.submission_id, receipt.id);
        assert!(receipt.state.lock().unwrap().accepted);
        let settled = settled(&mut events).await;
        assert!(
            matches!(settled.result().outcome(), TurnOutcome::Completed { .. }),
            "late interruption replaced independently observed normal completion"
        );
        provider.shutdown(test_context()).await.unwrap();
    }
    /// A well-typed foreign thread cannot allocate a native binding or poison retained open identity.
    #[tokio::test]
    async fn thread_open_rejects_foreign_host_identity_before_native_admission() {
        let provider = MockProvider::new().unwrap();
        let owner = make_owner("thread-owner");
        let request = SessionRequest {
            thread_id: owner.thread_id(),
            owner,
            mode: SessionMode::Interactive,
            workspace: ".".into(),
            history: SessionHistory::Durable { resume: None },
            configuration: configuration(),
            resource_lease: None,
            tools: Vec::new(),
            tool_endpoint: None,
            system_instructions: String::new(),
            trace: Arc::new(Trace),
        };
        let mut foreign = request.clone();
        foreign.thread_id = make_owner("other-owner").thread_id();
        assert!(request.identity() != foreign.identity());
        let context = test_context();
        assert!(
            matches!(provider.thread_open(context.clone(), foreign).await,
            Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)
        );
        assert!(provider.bindings.lock().await.is_empty());
        assert!(provider.owners.lock().await.is_empty());
        let opened = provider.thread_open(context, request).await.unwrap();
        assert_eq!(provider.bindings.lock().await.len(), 1);
        assert_eq!(
            opened.binding.thread_id,
            make_owner("thread-owner").thread_id()
        );
        provider
            .thread_close(test_context(), &opened.binding.id)
            .await
            .unwrap();
    }

    /// Normalized ownership mismatches must reject before committing any delivery receipt.
    #[tokio::test]
    async fn turn_start_rejects_foreign_normalized_ids_before_dispatch() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, _events) = initialized(&provider).await;
        for foreign_thread in [true, false] {
            let receipt = receipt(if foreign_thread {
                "foreign-thread"
            } else {
                "foreign-turn"
            });
            let mut request = prompt(&binding.id, owner.clone(), "exchange", receipt.clone());
            if foreign_thread {
                request.thread_id = make_owner("other").thread_id();
            } else {
                request.turn_id = forge_provider_api::TurnId::new("another-turn").unwrap();
            }
            assert!(
                matches!(provider.turn_start(test_context(), &binding.id, request).await,
                Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation)
            );
            let state = receipt.state.lock().unwrap();
            assert!(!state.sending);
            assert!(!state.accepted);
        }
        provider
            .thread_close(test_context(), &binding.id)
            .await
            .unwrap();
    }
    /// Retain original attempts when one admitted prompt accepts recovered inputs together.
    #[tokio::test]
    async fn grouped_prompt_completion_covers_recovered_and_current_attempts() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut events) = initialized(&provider).await;
        let current = receipt("current");
        let original = CoveredInput::new(
            SubmissionId::try_from("original").unwrap(),
            DeliveryAttemptId::new("original-attempt").unwrap(),
        );
        let current_coverage = CoveredInput::new(current.id.clone(), current.attempt_id().unwrap());
        let mut request = prompt(&binding.id, owner, "grouped", current);
        request.recovered_inputs.push(session::RecoveredInput {
            covered_input: original.clone(),
            input: PromptInput::Text("Earlier prepared input".into()),
            attachments: vec![],
            instructions: String::new(),
        });
        provider
            .turn_start(test_context(), &binding.id, request)
            .await
            .unwrap();
        let completed = settled(&mut events).await;
        assert_eq!(
            completed.result().covered_inputs(),
            &[original, current_coverage]
        );
        provider.shutdown(test_context()).await.unwrap();
    }

    /// A revision submits the newer design without adding its already-declared file again.
    #[tokio::test]
    async fn revision_planning_revises_the_current_design_without_readding_its_file() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, _events) = initialized(&provider).await;
        let mut request = prompt(&binding.id, owner, "revision", receipt("revision"));
        request.mode = PromptMode::Plan;
        for name in ["plan_overview_edit", "plan_file_edit", "plan_submit"] {
            request.tools.push(control::ControlToolDefinition {
                name,
                description: "Edit the plan",
                input_schema: serde_json::json!({}),
            });
        }
        let design = |version: u64, paths: serde_json::Value| {
            format!(
                "Active declaration design:\n```json\n{}\n```",
                serde_json::json!({"plan_id":"plan","version":version,"request":"add a greeting",
                    "declaration_files":{"paths":paths}})
            )
        };
        request.instructions = design(1, serde_json::json!([]));
        let steps = planning_steps(
            &request,
            &design(4, serde_json::json!([MOCK_DECLARATION_PATH])),
        );
        let calls = steps
            .iter()
            .map(|step| match step {
                TestPromptStep::ToolCall { name, arguments } => {
                    (name.as_str(), arguments["expected_version"].as_u64())
                }
                other => panic!("planner produced a non-tool step: {other:?}"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            calls,
            [("plan_overview_edit", Some(4)), ("plan_submit", Some(5))]
        );
        let initial = planning_steps(&request, "");
        assert_eq!(
            initial
                .iter()
                .filter_map(|step| match step {
                    TestPromptStep::ToolCall { name, .. } => Some(name.as_str()),
                    _ => None,
                })
                .collect::<Vec<_>>(),
            ["plan_overview_edit", "plan_file_edit", "plan_submit"]
        );
        provider.shutdown(test_context()).await.unwrap();
    }
}
