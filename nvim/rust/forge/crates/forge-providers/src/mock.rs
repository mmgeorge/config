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
    owners: Arc<Mutex<HashMap<ThreadId, BindingId>>>,
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
        if request.scope.thread_id != session.owner
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

        let cleanup_owner = request.thread_id.clone();
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
    ) -> Result<terminal::TerminalInventory, ProviderError> {
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
        if self.owners.lock().await.contains_key(&request.thread_id) {
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
            thread_id: request.thread_id.clone(),
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
            owner: request.thread_id.clone(),
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
        owners.insert(request.thread_id, binding_id);
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
        if request.scope.thread_id() != &session.owner {
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
            thread_id: session.owner.clone().as_str().to_owned(),
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
    async fn close_open_owner(&self, owner: &ThreadId) -> Result<(), ProviderError> {
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
        self.control.cancel_thread(Some(&session.owner));
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
            thread_id: scope.thread_id.clone(),
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
        self.control.cancel_thread(None);
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
            catalog::QueryContext::Workspace(_)
                if self
                    .control
                    .binding_model_catalog
                    .load(std::sync::atomic::Ordering::Acquire) =>
            {
                return Err(capability::UnsupportedCapability(
                    capability::ProviderFeature::ModelSelection,
                )
                .into());
            }
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
    ) -> Result<terminal::TerminalInventory, ProviderError> {
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
    terminal_list: Store<BindingId, terminal::TerminalInventory>,
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
    owner: ThreadId,
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
    owner: ThreadId,
    any_owner: bool,
    mode: Option<ExecutionMode>,
    state: watch::Sender<HoldState>,
}

#[derive(Default)]
struct MockControl {
    holds: std::sync::Mutex<VecDeque<Arc<ResponseHold>>>,
    scripts: std::sync::Mutex<HashMap<ThreadId, VecDeque<testing::TestPromptScript>>>,
    binding_model_catalog: std::sync::atomic::AtomicBool,
}

impl MockControl {
    fn find(&self, owner: &ThreadId, id: &TestHoldId) -> Result<Arc<ResponseHold>, ProviderError> {
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
        owner: &ThreadId,
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
            TestControlCommand::CancelOwner => {
                self.cancel_thread(Some(owner));
                Ok(TestControlResult::OwnerCancelled)
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
            TestControlCommand::RequireBindingModelCatalog => {
                self.binding_model_catalog
                    .store(true, std::sync::atomic::Ordering::Release);
                Ok(TestControlResult::Configured)
            }
        }
    }

    async fn arrive(
        &self,
        owner: &ThreadId,
        mode: ExecutionMode,
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
            thread_id: owner.clone(),
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

    fn cancel_thread(&self, owner: Option<&ThreadId>) {
        self.holds.lock().unwrap().retain(|hold| {
            let matched_owner = owner.is_none_or(|owner| hold.owner == *owner
                || matches!(&*hold.state.borrow(), HoldState::Pending(PendingTestResponse { thread_id: actual, .. }) if actual == owner));
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

    fn take_script(&self, owner: &ThreadId) -> Option<testing::TestPromptScript> {
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
    prompt_parts.extend(
        request
            .attachments
            .iter()
            .map(|attachment| attachment.context.clone()),
    );
    prompt_parts.push(render_input(&request.input));
    let prompt = prompt_parts.join("\n\n");
    let hold = tokio::select! {
        result = control.arrive(&owner, request.execution, prompt.clone()) => result,
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
    if request.execution != ExecutionMode::Planning || !has_plan_tools {
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
    let requests_setup =
        request.execution == ExecutionMode::Planning || prompt.contains("harness-checks");
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
    capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(&ProviderId::new("mock").expect("mock ID is valid"))
            .expect("mock is registered"),
    )
}
