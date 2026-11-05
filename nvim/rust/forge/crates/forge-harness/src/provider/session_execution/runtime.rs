//! Own session intake independently of submissions and scoped result waiters.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use tokio::sync::{OnceCell, mpsc, oneshot};
use tokio::task::{JoinHandle, JoinSet};

use forge_provider_api::lifecycle::{ExecutionUpdate, ObservationFailure, SubmissionId};

use super::{DeliveryState, ExecutionReadiness, ExecutionState};
use crate::provider::{
    ExecutionOwner, InvocationId, InvocationScope, Provider, ProviderCompletion, ProviderError,
    ProviderEvent, ProviderEventSink, ProviderRequest,
};

/// Preserve native and retained-work failures at the registry boundary.
#[derive(Debug)]
pub(crate) enum RegistryError {
    Provider(ProviderError),
    RetainedWork(super::RetainedCleanupFailure),
}

impl std::fmt::Display for RegistryError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Provider(error) => std::fmt::Display::fmt(error, formatter),
            Self::RetainedWork(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for RegistryError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Provider(error) => Some(error),
            Self::RetainedWork(error) => Some(error),
        }
    }
}

impl From<ProviderError> for RegistryError {
    fn from(error: ProviderError) -> Self {
        Self::Provider(error)
    }
}

/// Retain one engine for each independent execution owner in a shared provider runtime.
pub(crate) struct SessionExecutionRegistry {
    provider: Arc<Provider>,
    trace: Arc<crate::trace::TraceStore>,
    owners: Mutex<HashMap<ExecutionOwner, Arc<OwnerExecution>>>,
    gate: tokio::sync::RwLock<()>,
    closed: AtomicBool,
    cleanup: super::cleanup::CleanupLedger,
}

#[derive(Default)]
struct OwnerExecution {
    engine: OnceCell<Arc<SessionExecution>>,
    observer: Arc<super::SessionObservationInbox>,
}

impl SessionExecutionRegistry {
    /// Get an already installed owner without creating a binding or observer generation.
    pub(crate) fn existing_owner(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<Arc<SessionExecution>>, ProviderError> {
        Ok(self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .get(owner)
            .and_then(|slot| slot.engine.get())
            .cloned())
    }
    pub(crate) fn new(provider: Arc<Provider>, trace: Arc<crate::trace::TraceStore>) -> Self {
        Self {
            provider,
            trace,
            owners: Mutex::new(HashMap::new()),
            gate: tokio::sync::RwLock::new(()),
            closed: AtomicBool::new(false),
            cleanup: super::cleanup::CleanupLedger::default(),
        }
    }

    pub(crate) fn matches(&self, provider: &Arc<Provider>) -> bool {
        Arc::ptr_eq(&self.provider, provider)
    }

    /// Create replacement infrastructure while preserving the host's observer handle.
    pub(crate) fn with_observer(
        provider: Arc<Provider>,
        owner: ExecutionOwner,
        observer: Arc<super::SessionObservationInbox>,
        trace: Arc<crate::trace::TraceStore>,
    ) -> Self {
        let mut owners = HashMap::new();
        owners.insert(
            owner,
            Arc::new(OwnerExecution {
                engine: OnceCell::new(),
                observer,
            }),
        );
        Self {
            provider,
            trace,
            owners: Mutex::new(owners),
            gate: tokio::sync::RwLock::new(()),
            closed: AtomicBool::new(false),
            cleanup: super::cleanup::CleanupLedger::default(),
        }
    }

    pub(crate) fn retain_cleanup<F>(
        &self,
        kind: super::CleanupWorkKind,
        work: F,
    ) -> Result<oneshot::Receiver<Result<(), super::RetainedCleanupFailure>>, super::CleanupError>
    where
        F: std::future::Future<Output = Result<(), super::CleanupWorkFailure>> + Send + 'static,
    {
        self.cleanup.start(kind, work)
    }

    pub(crate) fn record_cleanup_failure(&self, error: super::CleanupError) {
        self.cleanup.record_failure(error);
    }

    pub(crate) async fn collect_cleanup(&self) -> Result<(), super::RetainedCleanupFailure> {
        self.cleanup.collect().await
    }

    fn slot(&self, owner: &ExecutionOwner) -> Result<Arc<OwnerExecution>, ProviderError> {
        if self.closed.load(Ordering::Acquire) {
            return Err(protocol("execution registry is closed"));
        }
        Ok(self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .entry(owner.clone())
            .or_default()
            .clone())
    }

    /// Get the owner queue without starting a provider or creating another observation stream.
    pub(crate) fn observer(
        &self,
        owner: ExecutionOwner,
    ) -> Result<Arc<super::SessionObservationInbox>, ProviderError> {
        Ok(self.slot(&owner)?.observer.clone())
    }

    /// Install intake once, before initialization can emit provider events.
    pub(crate) async fn owner(
        &self,
        owner: ExecutionOwner,
    ) -> Result<Arc<SessionExecution>, ProviderError> {
        let _gate = self.gate.read().await;
        let slot = self.slot(&owner)?;
        slot.engine
            .get_or_try_init(|| async {
                SessionExecution::open(
                    self.provider.clone(),
                    owner,
                    slot.observer.clone(),
                    self.trace.clone(),
                )
                .await
                .map(Arc::new)
            })
            .await
            .cloned()
    }

    /// Retire an existing owner without starting a new generation.
    pub(crate) async fn fence(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let _gate = self.gate.write().await;
        let slot = self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .get(owner)
            .cloned();
        if let Some(slot) = slot {
            if let Some(engine) = slot.engine.get() {
                engine.close().await?;
            } else {
                slot.observer.close();
            }
        }
        Ok(())
    }

    /// Retire one binding through native cleanup without closing sibling owners.
    pub(crate) async fn retire(
        &self,
        owner: &ExecutionOwner,
        reason: OwnerCloseReason,
    ) -> Result<(), ProviderError> {
        let _gate = self.gate.write().await;
        let slot = self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .get(owner)
            .cloned();
        if let Some(slot) = slot {
            if let Some(engine) = slot.engine.get() {
                engine.begin_orderly_shutdown();
            }
            // Reader closure is not native collection evidence. Perform this
            // even if an earlier fence already joined the reader, and retain
            // the binding on any failure so a later close retries it.
            self.provider.session_close_owner(owner).await?;
            if let Some(engine) = slot.engine.get() {
                engine.close().await?;
            }
            slot.observer.close_collected(reason);
        }
        Ok(())
    }

    /// Replace one failed owner only after cleanup and consumption of its observation failure.
    pub(crate) async fn replace(
        &self,
        owner: ExecutionOwner,
    ) -> Result<Arc<super::SessionObservationInbox>, RegistryError> {
        self.collect_cleanup()
            .await
            .map_err(RegistryError::RetainedWork)?;
        let _gate = self.gate.write().await;
        let old = self.slot(&owner)?;
        let engine = old
            .engine
            .get()
            .cloned()
            .ok_or_else(|| protocol("execution owner has not been opened"))?;
        if engine.is_healthy().await {
            return Err(protocol("healthy execution owner cannot be replaced").into());
        }
        engine.close().await?;
        if !old.observer.ready_for_replacement()? {
            return Err(protocol("old execution observations have not been consumed").into());
        }
        old.observer.retire_background().await?;
        self.provider.session_close_owner(&owner).await?;
        let observer = Arc::new(super::SessionObservationInbox::default());
        let engine = Arc::new(
            SessionExecution::open(
                self.provider.clone(),
                owner.clone(),
                observer.clone(),
                self.trace.clone(),
            )
            .await?,
        );
        let slot = Arc::new(OwnerExecution {
            engine: OnceCell::new(),
            observer: observer.clone(),
        });
        slot.engine
            .set(engine)
            .map_err(|_| protocol("replacement execution owner was already initialized"))?;
        self.owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .insert(owner, slot);
        Ok(observer)
    }

    /// Fence owner admission and mark each native observer before provider shutdown.
    pub(crate) async fn begin_orderly_shutdown(&self) -> Result<(), ProviderError> {
        let _gate = self.gate.write().await;
        self.closed.store(true, Ordering::Release);
        let owners = self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?;
        for owner in owners.values() {
            if let Some(engine) = owner.engine.get() {
                engine.begin_orderly_shutdown();
            }
        }
        Ok(())
    }

    /// Fence new owners and join every initialized execution driver.
    pub(crate) async fn close(&self) -> Result<(), RegistryError> {
        self.cleanup
            .seal()
            .map_err(|error| RegistryError::RetainedWork(error.into()))?;
        self.collect_cleanup()
            .await
            .map_err(RegistryError::RetainedWork)?;
        let _gate = self.gate.write().await;
        self.closed.store(true, Ordering::Release);
        let owners: Vec<_> = self
            .owners
            .lock()
            .map_err(|_| protocol("execution registry poisoned"))?
            .values()
            .cloned()
            .collect();
        let mut failure = None;
        for owner in owners {
            if let Some(engine) = owner.engine.get()
                && let Err(error) = engine.close().await
            {
                failure.get_or_insert(error);
            }
            if let Err(error) = owner.observer.retire_background().await {
                failure.get_or_insert(error);
            }
        }
        match failure {
            Some(error) => Err(error.into()),
            None => Ok(()),
        }
    }
}

/// Share an owner's event-driven execution without transferring its reader to a caller.
pub(crate) struct SessionExecution {
    owner: ExecutionOwner,
    provider: Arc<Provider>,
    commands: mpsc::Sender<Command>,
    healthy: Arc<AtomicBool>,
    orderly_shutdown: Arc<AtomicBool>,
    task: tokio::sync::Mutex<ExecutionTask>,
    activity: Arc<Mutex<Option<InvocationScope>>>,
    admitted: Arc<Mutex<std::collections::HashSet<InvocationScope>>>,
    #[cfg(test)]
    delivery_pressure: tokio::sync::watch::Receiver<bool>,
    #[cfg(test)]
    presentation_queue: mpsc::WeakSender<Delivery>,
}

/// Native completion preview and the exclusive token required for host settlement.
pub(crate) struct NativeReady {
    /// Native outcome merged with a snapshot of retained host effects.
    pub(crate) outcome: Result<ProviderCompletion, ProviderError>,
    /// Commit only after all host prerequisites have completed.
    pub(crate) ticket: SettlementTicket,
    /// Individually identified host-effect publications retained by this invocation.
    pub(crate) batches: Vec<crate::provider::HostEffectBatch>,
}

/// Result of checking a continuation against the retained host revision.
pub(crate) enum ContinuationAttempt {
    Ready(Box<NativeReady>),
    /// No reducer admission or native dispatch occurred; retain this prepared input.
    Stale(Box<ProviderRequest>),
}

/// Own exclusive settlement authority for one revision, with idempotent commit acknowledgment.
pub(crate) struct SettlementTicket {
    scope: InvocationScope,
    revision: u64,
    effects_revision: u64,
    nonce: u64,
    commands: mpsc::Sender<Command>,
    abandoned: Arc<AtomicBool>,
}

impl Drop for SettlementTicket {
    fn drop(&mut self) {
        self.abandoned.store(true, Ordering::Release);
    }
}

/// Preserve waiter cancellation across queued readiness delivery without sending another command.
struct ReadinessWaiter {
    abandoned: Option<Arc<AtomicBool>>,
}

impl ReadinessWaiter {
    /// Register an exclusive readiness observer before its command is queued.
    fn new() -> Self {
        Self {
            abandoned: Some(Arc::new(AtomicBool::new(false))),
        }
    }

    /// Get the ownership fence retained by the invocation and its readiness token.
    fn ownership(&self) -> Arc<AtomicBool> {
        self.abandoned
            .as_ref()
            .expect("readiness waiter is armed until delivery")
            .clone()
    }

    /// Transfer cancellation responsibility to the delivered settlement token.
    fn delivered(mut self) {
        self.abandoned.take();
    }
}

impl Drop for ReadinessWaiter {
    fn drop(&mut self) {
        if let Some(abandoned) = &self.abandoned {
            abandoned.store(true, Ordering::Release);
        }
    }
}

/// Retain one noncloneable ticket across cancellable host and provider futures.
#[derive(Default)]
pub(crate) struct SettlementCell {
    state: Mutex<SettlementCellState>,
    batches: Mutex<Vec<crate::provider::HostEffectBatch>>,
    effects: Mutex<HostEffectLedger>,
}

/// Identify one host workflow mutation within an adapter publication.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum HostEffectKind {
    /// A semantic plan edit.
    PlanEdit,
    /// A declaration design edit.
    DesignPatch,
    /// A validated plan submission.
    PlanSubmit,
    /// A model-presented question.
    PlanQuestion,
    /// A model answer to an elicitation.
    QuestionAnswer,
    /// A model withdrawal of an elicitation.
    QuestionWithdrawal,
    /// Goal or task continuation evidence derived from a completed native turn.
    GoalEvidence,
}

/// Distinguish repeated effects even when their payloads are identical.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct HostEffectId {
    pub(crate) batch_revision: u64,
    pub(crate) kind: HostEffectKind,
    pub(crate) ordinal: usize,
}

#[derive(Default)]
struct HostEffectLedger {
    receipts: HashMap<HostEffectId, HostEffectState>,
    publication_order: Vec<HostEffectId>,
    document: Option<(String, u64, String)>,
    sealed: bool,
}

enum HostEffectState {
    Applying,
    Applied(Option<Box<HostEffectOutput>>),
    Ambiguous,
}

/// Retain presentation created by a host mutation until publication is acknowledged.
#[derive(Clone)]
pub(crate) struct HostEffectOutput {
    pub(crate) nodes: Vec<crate::exchange::ExchangeNode>,
    pub(crate) events: Vec<forge_protocol::message::SessionEvent>,
    pub(crate) elicitation: Option<crate::plan::PlanElicitation>,
    pub(crate) awaiting_input: bool,
    pub(crate) publication_drained: bool,
    pub(crate) plan_document: Option<(String, u64, String)>,
    pub(crate) continuing: Option<bool>,
    pub(crate) plan_record: Option<crate::plan::PlanRecord>,
}

/// Return either a retained result or an exclusive new mutation reservation.
pub(crate) enum ResultEffectAdmission {
    Applied(Box<HostEffectOutput>),
    Apply(HostEffectGuard),
}

/// Distinguish a previously applied effect from one exclusively checked out for application.
pub(crate) enum HostEffectAdmission {
    Applied(Box<Option<crate::plan::PlanRecord>>),
    Apply(HostEffectGuard),
}

/// Mark a partial host mutation ambiguous if application exits before recording its receipt.
pub(crate) struct HostEffectGuard {
    cell: Arc<SettlementCell>,
    id: HostEffectId,
    complete: bool,
    mutation_started: bool,
}

impl HostEffectGuard {
    /// Fence retries before the first potentially irreversible mutation.
    pub(crate) fn begin_mutation(&mut self) {
        self.mutation_started = true;
    }
    /// Release a reservation when validation failed before any mutation began.
    pub(crate) fn cancel_definite(mut self) -> Result<(), ProviderError> {
        let mut ledger = self
            .cell
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        ledger.receipts.remove(&self.id);
        self.complete = true;
        Ok(())
    }

    /// Record a completed mutation together with its resulting document identity.
    pub(crate) fn commit(
        mut self,
        plan: String,
        version: u64,
        digest: String,
        plan_record: Option<crate::plan::PlanRecord>,
    ) -> Result<(), ProviderError> {
        let mut ledger = self
            .cell
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        ledger.receipts.insert(
            self.id,
            HostEffectState::Applied(Some(Box::new(HostEffectOutput {
                nodes: Vec::new(),
                events: Vec::new(),
                elicitation: None,
                awaiting_input: false,
                publication_drained: true,
                plan_document: None,
                continuing: None,
                plan_record,
            }))),
        );
        ledger.document = Some((plan, version, digest));
        self.complete = true;
        Ok(())
    }

    /// Retain an applied mutation's exact presentation until it has been delivered.
    pub(crate) fn commit_output(mut self, output: HostEffectOutput) -> Result<(), ProviderError> {
        let mut ledger = self
            .cell
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        if let Some(document) = &output.plan_document {
            ledger.document = Some(document.clone());
        }
        ledger
            .receipts
            .insert(self.id, HostEffectState::Applied(Some(Box::new(output))));
        ledger.publication_order.push(self.id);
        self.complete = true;
        Ok(())
    }
}

impl Drop for HostEffectGuard {
    fn drop(&mut self) {
        if !self.complete {
            let mut ledger = self
                .cell
                .effects
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if self.mutation_started {
                ledger.receipts.insert(self.id, HostEffectState::Ambiguous);
            } else {
                ledger.receipts.remove(&self.id);
            }
        }
    }
}

#[derive(Default)]
enum SettlementCellState {
    #[default]
    Empty,
    Available(SettlementTicket),
    CheckedOut,
}

/// Restore the current ticket to its owner when a cancellable operation ends.
pub(crate) struct SettlementCheckout {
    cell: Arc<SettlementCell>,
    ticket: Option<SettlementTicket>,
}

impl SettlementCell {
    /// Reserve an individual effect and verify the latest applied plan document has not changed.
    pub(crate) fn begin_plan_effect(
        self: &Arc<Self>,
        id: HostEffectId,
        plan: &str,
        version: u64,
        digest: &str,
    ) -> Result<HostEffectAdmission, ProviderError> {
        let mut ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        if let Some((previous_plan, previous_version, previous_digest)) = &ledger.document
            && (previous_plan != plan || *previous_version != version || previous_digest != digest)
        {
            return Err(protocol(
                "plan document changed after a retained host effect",
            ));
        }
        match ledger.receipts.get(&id) {
            Some(HostEffectState::Applied(output)) => {
                return Ok(HostEffectAdmission::Applied(Box::new(
                    output
                        .as_ref()
                        .and_then(|output| output.plan_record.clone()),
                )));
            }
            Some(HostEffectState::Applying | HostEffectState::Ambiguous) => {
                return Err(protocol(
                    "host effect application is incomplete or ambiguous",
                ));
            }
            None => {}
        }
        ledger.receipts.insert(id, HostEffectState::Applying);
        ledger.sealed = false;
        Ok(HostEffectAdmission::Apply(HostEffectGuard {
            cell: self.clone(),
            id,
            complete: false,
            mutation_started: false,
        }))
    }

    /// Retrieve a committed result before validating mutable document evidence.
    pub(crate) fn applied_result(
        &self,
        id: HostEffectId,
    ) -> Result<Option<Box<HostEffectOutput>>, ProviderError> {
        let ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        match ledger.receipts.get(&id) {
            Some(HostEffectState::Applied(Some(output))) => Ok(Some(output.clone())),
            Some(HostEffectState::Applied(None)) => {
                Err(protocol("host effect receipt has no retained result"))
            }
            Some(HostEffectState::Applying | HostEffectState::Ambiguous) => Err(protocol(
                "host effect application is incomplete or ambiguous",
            )),
            None => Ok(None),
        }
    }
    /// Reserve a presentation-producing effect or retrieve its retained result.
    pub(crate) fn begin_result_effect(
        self: &Arc<Self>,
        id: HostEffectId,
    ) -> Result<ResultEffectAdmission, ProviderError> {
        self.begin_result_effect_with_document(id, None)
    }

    /// Reserve a planning result only while the last recorded working document still matches.
    pub(crate) fn begin_plan_result_effect(
        self: &Arc<Self>,
        id: HostEffectId,
        plan: &str,
        version: u64,
        digest: &str,
    ) -> Result<ResultEffectAdmission, ProviderError> {
        self.begin_result_effect_with_document(id, Some((plan, version, digest)))
    }

    /// Validate optional document evidence and reserve under one ledger lock.
    fn begin_result_effect_with_document(
        self: &Arc<Self>,
        id: HostEffectId,
        document: Option<(&str, u64, &str)>,
    ) -> Result<ResultEffectAdmission, ProviderError> {
        let mut ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        match ledger.receipts.get(&id) {
            Some(HostEffectState::Applied(Some(output))) => {
                return Ok(ResultEffectAdmission::Applied(output.clone()));
            }
            Some(HostEffectState::Applied(None)) => {
                return Err(protocol("host effect receipt has no retained result"));
            }
            Some(HostEffectState::Applying | HostEffectState::Ambiguous) => {
                return Err(protocol(
                    "host effect application is incomplete or ambiguous",
                ));
            }
            None => {}
        }
        if let (
            Some((plan, version, digest)),
            Some((previous_plan, previous_version, previous_digest)),
        ) = (document, &ledger.document)
            && (previous_plan != plan || *previous_version != version || previous_digest != digest)
        {
            return Err(protocol(
                "plan document changed after a retained host effect",
            ));
        }
        ledger.receipts.insert(id, HostEffectState::Applying);
        ledger.sealed = false;
        Ok(ResultEffectAdmission::Apply(HostEffectGuard {
            cell: self.clone(),
            id,
            complete: false,
            mutation_started: false,
        }))
    }

    /// Record that a retained effect's publication has drained successfully.
    pub(crate) fn acknowledge_effect_publication(
        &self,
        id: HostEffectId,
    ) -> Result<(), ProviderError> {
        let mut ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        match ledger.receipts.get_mut(&id) {
            Some(HostEffectState::Applied(Some(output))) => {
                output.publication_drained = true;
                Ok(())
            }
            _ => Err(protocol("host effect has no completed publication")),
        }
    }

    /// Get retained effect publications awaiting an explicit delivery acknowledgment.
    pub(crate) fn pending_effect_publications(&self) -> Result<Vec<HostEffectId>, ProviderError> {
        let ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        let pending = ledger
            .publication_order
            .iter()
            .filter_map(|id| match ledger.receipts.get(id) {
                Some(HostEffectState::Applied(Some(output))) if !output.publication_drained => {
                    Some(*id)
                }
                _ => None,
            })
            .collect();
        Ok(pending)
    }

    /// Report only a mutation whose ordered presentation was acknowledged.
    pub(crate) fn effect_published(&self, id: HostEffectId) -> Result<bool, ProviderError> {
        let ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        Ok(match ledger.receipts.get(&id) {
            Some(HostEffectState::Applied(Some(output))) => output.publication_drained,
            Some(HostEffectState::Applied(None)) => true,
            Some(HostEffectState::Applying | HostEffectState::Ambiguous) | None => false,
        })
    }

    /// Reject final settlement while a mutation or its publication remains unresolved.
    pub(crate) fn validate_effects_settled(&self) -> Result<(), ProviderError> {
        let ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        if !ledger.sealed
            || ledger.receipts.values().any(|state| match state {
                HostEffectState::Applying | HostEffectState::Ambiguous => true,
                HostEffectState::Applied(Some(output)) => !output.publication_drained,
                HostEffectState::Applied(None) => false,
            })
        {
            return Err(protocol(
                "host effect mutation or publication is not settled",
            ));
        }
        Ok(())
    }

    /// Seal the exact applicable effect identities after their host mutations have succeeded.
    pub(crate) fn seal_effects(&self, expected: &[HostEffectId]) -> Result<(), ProviderError> {
        let mut ledger = self
            .effects
            .lock()
            .map_err(|_| protocol("host effect ledger poisoned"))?;
        if expected
            .iter()
            .any(|id| !matches!(ledger.receipts.get(id), Some(HostEffectState::Applied(_))))
            || ledger.receipts.values().any(|state| {
                matches!(
                    state,
                    HostEffectState::Applying | HostEffectState::Ambiguous
                )
            })
        {
            return Err(protocol(
                "applicable host effects have not been fully applied",
            ));
        }
        ledger.sealed = true;
        Ok(())
    }

    /// Retain the latest cumulative batch provenance for host application.
    pub(crate) fn set_batches(
        &self,
        batches: Vec<crate::provider::HostEffectBatch>,
    ) -> Result<(), ProviderError> {
        let mut current = self
            .batches
            .lock()
            .map_err(|_| protocol("settlement batch ledger poisoned"))?;
        if current
            .iter()
            .map(|batch| batch.revision)
            .ne(batches.iter().map(|batch| batch.revision))
        {
            self.effects
                .lock()
                .map_err(|_| protocol("host effect ledger poisoned"))?
                .sealed = false;
        }
        *current = batches;
        Ok(())
    }

    /// Snapshot published batches without consuming their effects.
    pub(crate) fn batches(&self) -> Result<Vec<crate::provider::HostEffectBatch>, ProviderError> {
        Ok(self
            .batches
            .lock()
            .map_err(|_| protocol("settlement batch ledger poisoned"))?
            .clone())
    }

    /// Check whether no settlement token remains retained or checked out.
    pub(crate) fn is_empty(&self) -> Result<bool, ProviderError> {
        let state = self
            .state
            .lock()
            .map_err(|_| protocol("settlement cell poisoned"))?;
        Ok(matches!(*state, SettlementCellState::Empty))
    }

    /// Exclusively borrow the current ticket, including across an awaited operation.
    pub(crate) fn checkout(self: &Arc<Self>) -> Result<SettlementCheckout, ProviderError> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| protocol("settlement cell poisoned"))?;
        let ticket = match std::mem::replace(&mut *state, SettlementCellState::CheckedOut) {
            SettlementCellState::Empty => None,
            SettlementCellState::Available(ticket) => Some(ticket),
            SettlementCellState::CheckedOut => {
                return Err(protocol("settlement ticket is already checked out"));
            }
        };
        Ok(SettlementCheckout {
            cell: self.clone(),
            ticket,
        })
    }
}

impl SettlementCheckout {
    /// Get the retained token without transferring its ownership.
    pub(crate) fn ticket(&self) -> Option<&SettlementTicket> {
        self.ticket.as_ref()
    }

    /// Retain a newer token after a same-invocation attempt completes.
    pub(crate) fn replace(&mut self, ticket: SettlementTicket) {
        self.ticket = Some(ticket);
    }

    /// Transfer the token to an explicit host commit operation.
    pub(crate) fn take(&mut self) -> Option<SettlementTicket> {
        self.ticket.take()
    }
}

impl Drop for SettlementCheckout {
    fn drop(&mut self) {
        let mut state = self
            .cell
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *state = match self.ticket.take() {
            Some(ticket) => SettlementCellState::Available(ticket),
            None => SettlementCellState::Empty,
        };
    }
}

/// Explain why a host settlement token could not be committed.
#[derive(Debug)]
pub(crate) enum SettlementError {
    /// A continuation, pending control, or new host effect invalidated this token.
    Stale,
    /// The execution owner closed before settlement.
    Unavailable,
    /// The host effect store failed while committing.
    Provider(ProviderError),
}

impl std::fmt::Display for SettlementError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stale => formatter.write_str("host settlement revision is stale"),
            Self::Unavailable => formatter.write_str("execution owner closed before settlement"),
            Self::Provider(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for SettlementError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Provider(error) => Some(error),
            Self::Stale | Self::Unavailable => None,
        }
    }
}

impl SettlementTicket {
    /// Retire this preview while retaining the token until a newer readiness result is owned.
    pub(crate) fn retire_for_refresh(&self) {
        self.abandoned.store(true, Ordering::Release);
    }
    /// Get the invocation covered by this settlement token.
    #[cfg(test)]
    pub(crate) fn scope(&self) -> &InvocationScope {
        &self.scope
    }

    /// Commit the reviewed execution and host-effect revisions exactly once.
    pub(crate) async fn commit(&self) -> Result<(), SettlementError> {
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::CommitSettlement {
                scope: self.scope.clone(),
                revision: self.revision,
                effects_revision: self.effects_revision,
                nonce: self.nonce,
                reply,
            })
            .await
            .map_err(|_| SettlementError::Unavailable)?;
        response.await.map_err(|_| SettlementError::Unavailable)?
    }
}

enum ExecutionTask {
    Running(JoinHandle<Result<(), ProviderError>>),
    Closed,
    Failed(Arc<forge_provider_api::error::ObservationCleanup>),
}

/// Identify deliberate resource retirement without claiming native execution success.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum OwnerCloseReason {
    Cancelled,
    Shutdown,
}

enum Command {
    Close(Option<OwnerCloseReason>),
    ReserveInvocation {
        scope: InvocationScope,
        reservation: InputReservation,
    },
    ResumeExchange {
        collection: ExchangeCollection,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
    CancelExchange {
        scope: InvocationScope,
        reply: oneshot::Sender<Result<ExchangeCollection, ProviderError>>,
    },
    ReconcileInput {
        submission: Arc<crate::provider::submission::Submission>,
        observation: Box<crate::provider::submission::SubmissionReconciliation>,
        reply: oneshot::Sender<anyhow::Result<crate::provider::submission::SubmissionRecord>>,
    },
    HostEffectsCollected {
        scope: InvocationScope,
        operation_id: forge_provider_api::OperationId,
        result: Result<(), ProviderError>,
    },
    Cancel {
        scope: InvocationScope,
        reply: CancellationReply,
    },
    CancellationFinished {
        scope: InvocationScope,
        outcome: CancellationKind,
        result: Result<(), ProviderError>,
        reply: CancellationReply,
    },
    #[cfg(test)]
    TestPublishEffect {
        scope: InvocationScope,
        completion: Box<ProviderCompletion>,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
    SteerCurrent {
        input: crate::provider::SteeringInput,
        submission: Option<SubmissionId>,
        authority: Option<crate::tools::TaskAuthority>,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
    Steer {
        scope: InvocationScope,
        input: crate::provider::SteeringInput,
        submission: Option<SubmissionId>,
        authority: Option<crate::tools::TaskAuthority>,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
    ObserveSession {
        sink: ProviderEventSink,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
    Execute {
        request: Box<ProviderRequest>,
        scope: InvocationScope,
        submission: Option<SubmissionId>,
        sink: ProviderEventSink,
        completion: Box<crate::tools::ControlTurnContext>,
        continuation: Option<(InvocationScope, u64, u64, u64)>,
        stale: Option<oneshot::Sender<ProviderRequest>>,
        abandoned: Arc<AtomicBool>,
        reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
    },
    WaitReady {
        scope: InvocationScope,
        abandoned: Arc<AtomicBool>,
        reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
    },
    CommitSettlement {
        scope: InvocationScope,
        revision: u64,
        effects_revision: u64,
        nonce: u64,
        reply: oneshot::Sender<Result<(), SettlementError>>,
    },
}

enum Delivery {
    Inbox {
        inbox: Arc<super::SessionObservationInbox>,
        event: ProviderEvent,
    },
    Event {
        sink: ProviderEventSink,
        event: Box<ProviderEvent>,
        fallback: Option<SessionObserver>,
    },
    Complete {
        reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
        result: Box<Result<NativeReady, ProviderError>>,
    },
}

struct Invocation {
    scope: InvocationScope,
    output: ProviderEventSink,
    reply: Option<oneshot::Sender<Result<NativeReady, ProviderError>>>,
    completion: crate::tools::ControlTurnContext,
    terminal: Option<Result<forge_provider_api::lifecycle::ExecutionResult, ProviderError>>,
    native_settlement: Option<forge_provider_api::InvocationSettlement>,
    host_collection: HostCollection,
    publication_failure: Option<ProviderError>,
    pending_controls: usize,
    revision: u64,
    ticket_outstanding: bool,
    ticket_nonce: u64,
    abandoned: Arc<AtomicBool>,
    idle_observation: bool,
}

/// Keep native settlement separate from host-owned callback and checkpoint collection.
#[derive(Clone, Copy)]
enum HostCollection {
    Pending,
    Collecting,
    Settled,
}

struct SubmissionOutcome {
    scope: InvocationScope,
    submission: Option<SubmissionId>,
    control_reply: Option<oneshot::Sender<Result<(), ProviderError>>>,
    result: Result<(), ProviderError>,
}

/// Retain exact dispatch and receipt ownership while native acceptance remains asynchronous.
struct AttemptControl {
    submission: Arc<crate::provider::submission::Submission>,
    dispatch: crate::provider::DispatchObserver,
    task: tokio::task::AbortHandle,
    finished: tokio::sync::watch::Receiver<bool>,
    attempt_id: forge_provider_api::DeliveryAttemptId,
}

/// Retain the exact journal authority registered before prepared foreground publication.
struct InputReservation {
    submission: Arc<crate::provider::submission::Submission>,
    attempt_id: forge_provider_api::DeliveryAttemptId,
}

/// Preserve collected predispatch evidence without retaining live receipt or callback resources.
struct PreventedInput {
    submission_id: SubmissionId,
    attempt_id: forge_provider_api::DeliveryAttemptId,
}

/// Distinguish externally committed preparation from a later execution attempt stopped at its fence.
enum ReservationOrigin {
    PreparedForeground,
    Execution,
}

/// Prove the exact owner-issued cancellation generation collected for configuration replacement.
#[derive(Clone)]
pub(crate) struct ExchangeCollection {
    scope: InvocationScope,
    fence_id: forge_provider_api::OperationId,
    commands: mpsc::Sender<Command>,
}

/// Preserve the exact cancellation postcondition for invocation and configuration observers.
enum CancellationReply {
    #[cfg(test)]
    Invocation(oneshot::Sender<Result<(), ProviderError>>),
    Exchange {
        collection: ExchangeCollection,
        reply: oneshot::Sender<Result<ExchangeCollection, ProviderError>>,
    },
}

impl CancellationReply {
    /// Publish collection proof only after the owner establishes successful cancellation collection.
    fn complete(self, result: Result<(), ProviderError>) {
        match self {
            #[cfg(test)]
            Self::Invocation(reply) => {
                let _ = reply.send(result);
            }
            Self::Exchange { collection, reply } => {
                let _ = reply.send(result.map(|_| collection));
            }
        }
    }
}

/// Distinguish a proven unsent input from native work whose own settlement must be observed.
enum CancellationKind {
    Prevented(PreventedInput),
    Native,
}

#[derive(Clone)]
enum SessionObserver {
    Inbox(Arc<super::SessionObservationInbox>),
    Stream(Box<ProviderEventSink>),
}

impl SessionObserver {
    /// Preserve an invocation event when its result observer has been dropped.
    async fn retain(&self, event: ProviderEvent) -> Result<(), ProviderError> {
        match self {
            Self::Inbox(inbox) => inbox.push(event),
            Self::Stream(sink) => {
                sink.bind_scope(event.scope())?.send_wait(event).await?;
                Ok(())
            }
        }
    }

    fn close(&self, reason: Option<OwnerCloseReason>) {
        if let Self::Inbox(inbox) = self {
            if let Some(reason) = reason {
                inbox.close_collected(reason);
            } else {
                inbox.close();
            }
        }
    }

    fn diagnostic(&self, event: ProviderEvent) {
        match self {
            Self::Inbox(inbox) => {
                let _ = inbox.push(event);
            }
            Self::Stream(sink) => {
                if let Ok(scoped) = sink.bind_scope(event.scope()) {
                    let _ = scoped.send(event);
                }
            }
        }
    }
}

enum FailureOrigin {
    Observation,
    Error(ProviderError),
}

struct FailedWaiter {
    reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
    original: Option<ProviderError>,
}

struct Driver {
    owner: ExecutionOwner,
    provider: Arc<Provider>,
    trace: Arc<crate::trace::TraceStore>,
    queue_loss_reported: AtomicBool,
    state: ExecutionState,
    invocations: HashMap<InvocationId, Invocation>,
    /// Retain completed authority to reject late native work and deduplicate terminal evidence.
    committed_invocations: HashMap<InvocationId, Invocation>,
    submissions: JoinSet<SubmissionOutcome>,
    deliveries: JoinSet<Result<(), ProviderError>>,
    delivery: Option<mpsc::Sender<Delivery>>,
    pending_delivery: VecDeque<Delivery>,
    #[cfg(test)]
    delivery_pressure: tokio::sync::watch::Sender<bool>,
    cleanup: JoinSet<forge_provider_api::error::ObservationCleanup>,
    failed_waiters: Vec<FailedWaiter>,
    commands: mpsc::Sender<Command>,
    next_ticket_nonce: u64,
    committed_tickets: HashMap<(InvocationId, u64), InvocationScope>,
    failed: bool,
    healthy: Arc<AtomicBool>,
    first_failure: Option<ObservationFailure>,
    cleanup_failure: Option<Arc<forge_provider_api::error::ObservationCleanup>>,
    retiring: Option<OwnerCloseReason>,
    idle: SessionObserver,
    activity: Arc<Mutex<Option<InvocationScope>>>,
    admitted: Arc<Mutex<std::collections::HashSet<InvocationScope>>>,
    attempts: HashMap<InvocationScope, AttemptControl>,
    cancelled_admissions: std::collections::HashSet<InvocationScope>,
    cancelled_exchanges: HashMap<crate::provider::ExchangeId, ExchangeCollection>,
    prevented_invocations: HashMap<InvocationScope, PreventedInput>,
    reservations: HashMap<InvocationScope, InputReservation>,
    latest_reservations: HashMap<crate::provider::ExchangeId, InvocationScope>,
    submission_exits: HashMap<tokio::task::Id, tokio::sync::watch::Sender<bool>>,
}

impl SessionExecution {
    /// Inspect already-admitted presentation frames without retaining the delivery worker's lifetime.
    #[cfg(test)]
    pub(crate) fn presentation_queued_snapshot(&self) -> usize {
        self.presentation_queue
            .upgrade()
            .map_or(0, |sender| sender.max_capacity() - sender.capacity())
    }

    /// Transfer exact prepared input authority to the owner before foreground scheduling is externally committed.
    pub(crate) fn reserve_invocation(
        &self,
        scope: InvocationScope,
        submission: Arc<crate::provider::submission::Submission>,
    ) -> Result<(), ProviderError> {
        if scope.owner != self.owner || !self.healthy.load(Ordering::Acquire) {
            return Err(protocol("prepared reservation has no live owner"));
        }
        let record = submission
            .record()
            .map_err(crate::provider::submission::journal_failure)?;
        if record.request.owner()? != self.owner
            || !matches!(
                record.delivery,
                crate::provider::submission::SubmissionState::Prepared
                    | crate::provider::submission::SubmissionState::Cancelled
            )
        {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::WrongInvocation,
                ),
            ));
        }
        self.commands
            .try_send(Command::ReserveInvocation {
                scope,
                reservation: InputReservation {
                    submission,
                    attempt_id: record.attempt_id,
                },
            })
            .map_err(|_| {
                ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ))
            })
    }
    /// Authorize an explicit configuration replacement after the prior exchange's native and host collection.
    pub(crate) async fn resume_exchange(
        &self,
        collection: ExchangeCollection,
    ) -> Result<(), ProviderError> {
        if collection.scope.owner != self.owner || !self.commands.same_channel(&collection.commands)
        {
            return Err(protocol(
                "configuration replacement belongs to another owner",
            ));
        }
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::ResumeExchange { collection, reply })
            .await
            .map_err(|_| protocol("execution owner closed before configuration replacement"))?;
        response
            .await
            .map_err(|_| protocol("execution owner lost configuration replacement result"))?
    }
    /// Stop the host continuation scheduler and cancel the exact active attempt of this exchange.
    pub(crate) async fn cancel_exchange(
        &self,
        scope: InvocationScope,
    ) -> Result<(), ProviderError> {
        self.collect_exchange(scope).await.map(drop)
    }

    /// Collect the actual latest scheduled attempt and retain its exact cancellation generation proof.
    pub(crate) async fn collect_exchange(
        &self,
        scope: InvocationScope,
    ) -> Result<ExchangeCollection, ProviderError> {
        if scope.owner != self.owner {
            return Err(protocol("exchange cancellation belongs to another owner"));
        }
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::CancelExchange { scope, reply })
            .await
            .map_err(|_| protocol("execution owner closed before exchange cancellation"))?;
        response
            .await
            .map_err(|_| protocol("execution owner lost exchange cancellation result"))?
    }
    /// Prove that this owner admitted a scope even after its native settlement was published.
    pub(crate) fn admitted_invocation(
        &self,
        scope: &InvocationScope,
    ) -> Result<bool, ProviderError> {
        if scope.owner != self.owner {
            return Ok(false);
        }
        self.admitted
            .lock()
            .map(|scopes| scopes.contains(scope))
            .map_err(|_| protocol("admitted scope snapshot poisoned"))
    }
    /// Get the retained owner's currently admitted scope for an explicit host exchange cancellation.
    pub(crate) fn active_invocation_scope(&self) -> Result<Option<InvocationScope>, ProviderError> {
        self.activity
            .lock()
            .map(|scope| scope.clone())
            .map_err(|_| protocol("activity snapshot poisoned"))
    }
    /// Validate and commit worker evidence on the sole owner before recovery can use it.
    pub(crate) async fn reconcile_input(
        &self,
        submission: Arc<crate::provider::submission::Submission>,
        observation: crate::provider::submission::SubmissionReconciliation,
    ) -> anyhow::Result<crate::provider::submission::SubmissionRecord> {
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::ReconcileInput {
                submission,
                observation: Box::new(observation),
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner closed before reconciliation"))?;
        response
            .await
            .map_err(|_| protocol("execution owner lost reconciliation result"))?
    }
    /// Cancel exact host admission or await its native settlement without selecting a successor.
    #[cfg(test)]
    pub(crate) async fn cancel(&self, scope: InvocationScope) -> Result<(), ProviderError> {
        if scope.owner != self.owner {
            return Err(protocol("cancellation belongs to another owner"));
        }
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::Cancel {
                scope,
                reply: CancellationReply::Invocation(reply),
            })
            .await
            .map_err(|_| protocol("execution owner closed before cancellation"))?;
        response
            .await
            .map_err(|_| protocol("execution owner lost cancellation result"))?
    }
    /// Inspect the owner's published admission in deterministic host lifecycle fixtures.
    #[cfg(test)]
    pub(crate) fn active_invocation_snapshot(
        &self,
    ) -> Result<Option<InvocationScope>, ProviderError> {
        self.activity
            .lock()
            .map(|scope| scope.clone())
            .map_err(|_| protocol("activity snapshot poisoned"))
    }
    /// Read the exact owner's published admission state while applying an exchange command.
    pub(crate) fn invocation_active_snapshot(
        &self,
        scope: &InvocationScope,
    ) -> Result<bool, ProviderError> {
        if scope.owner != self.owner || !self.healthy.load(Ordering::Acquire) {
            return Err(protocol("invocation observation has no live owner"));
        }
        let active = self
            .activity
            .lock()
            .map_err(|_| protocol("activity snapshot poisoned"))?;
        Ok(active.as_ref() == Some(scope))
    }
    /// Get immutable registration identity for host-authored journal entries.
    pub(crate) fn provider_id(&self) -> crate::provider::ProviderId {
        self.provider.descriptor().kind
    }
    #[cfg(test)]
    pub(crate) async fn publish_idle_test_effect(
        &self,
        scope: InvocationScope,
        completion: ProviderCompletion,
    ) -> Result<(), ProviderError> {
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::TestPublishEffect {
                scope,
                completion: Box::new(completion),
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is closed"))?;
        response
            .await
            .map_err(|_| protocol("execution owner closed before test effect"))?
    }
    /// Check whether the retained driver can still accept work.
    pub(crate) async fn is_healthy(&self) -> bool {
        if !self.healthy.load(Ordering::Acquire) {
            return false;
        }
        match &*self.task.lock().await {
            ExecutionTask::Running(task) => !task.is_finished(),
            ExecutionTask::Closed | ExecutionTask::Failed(_) => false,
        }
    }

    async fn open(
        provider: Arc<Provider>,
        owner: ExecutionOwner,
        observer: Arc<super::SessionObservationInbox>,
        trace: Arc<crate::trace::TraceStore>,
    ) -> Result<Self, ProviderError> {
        let mut events = provider.session_events(owner.clone()).await?;
        let (commands, mut receive) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (delivery, output) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        #[cfg(test)]
        let presentation_queue = delivery.downgrade();
        let mut deliveries = JoinSet::new();
        deliveries.spawn(deliver(output));
        #[cfg(test)]
        let (delivery_pressure, pressure_observer) = tokio::sync::watch::channel(false);
        let healthy = Arc::new(AtomicBool::new(true));
        let activity = Arc::new(Mutex::new(None));
        let admitted = Arc::new(Mutex::new(Default::default()));
        let orderly_shutdown = Arc::new(AtomicBool::new(false));
        let driver_shutdown = orderly_shutdown.clone();
        let mut driver = Driver {
            owner: owner.clone(),
            provider: provider.clone(),
            trace,
            queue_loss_reported: AtomicBool::new(false),
            state: ExecutionState::new(owner.clone()),
            invocations: HashMap::new(),
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries,
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            #[cfg(test)]
            delivery_pressure: delivery_pressure.clone(),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands: commands.clone(),
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: healthy.clone(),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(observer),
            activity: activity.clone(),
            admitted: admitted.clone(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: Default::default(),
            prevented_invocations: Default::default(),
            reservations: Default::default(),
            latest_reservations: Default::default(),
            submission_exits: Default::default(),
        };
        let task = tokio::spawn(async move {
            let mut events_closed = false;
            loop {
                // Stop native intake at the first blocked frame, but keep control and collection selectable.
                let delivery_capacity = driver
                    .delivery
                    .clone()
                    .expect("intake retains its delivery sender");
                tokio::select! {
                    capacity = delivery_capacity.reserve_owned(), if !driver.pending_delivery.is_empty() => {
                        match capacity {
                            Ok(permit) => {
                                let delivery = driver.pending_delivery.pop_front()
                                    .expect("capacity is requested only for retained delivery");
                                permit.send(delivery);
                                #[cfg(test)]
                                driver.delivery_pressure.send_replace(!driver.pending_delivery.is_empty());
                            },
                            Err(_) => {
                                for delivery in driver.pending_delivery.drain(..) {
                                    delivery.fail(ObservationFailure::Disconnected);
                                }
                                driver.fail_observation(ObservationFailure::Disconnected);
                            },
                        }
                    },
                    command = receive.recv() => match command {
                        Some(Command::Close(reason)) => {
                            while driver.pending_delivery.is_empty() {
                                match events.try_recv() {
                                    Ok(event) => {
                                        driver.native_event(event).await;
                                        // Let the presentation worker collect each accepted frame.
                                        tokio::task::yield_now().await;
                                    },
                                    Err(crate::provider::events::EventDeliveryError::Empty) => break,
                                    Err(crate::provider::events::EventDeliveryError::Closed) => {
                                        if !driver_shutdown.load(Ordering::Acquire) { driver.fail_observation(ObservationFailure::Disconnected); }
                                        break;
                                    },
                                    Err(crate::provider::events::EventDeliveryError::Observation(failure)) => { driver.fail_observation(failure); break; },
                                    Err(crate::provider::events::EventDeliveryError::OwnershipMismatch { .. }) => { driver.fail_observation(ObservationFailure::OwnershipConflict); break; },
                                    Err(error) => { driver.fail_all(&error.to_string()); break; },
                                }
                            }
                            if let Some(reason) = reason
                                && !driver.failed {
                                driver_shutdown.store(true, Ordering::Release);
                                driver.begin_retirement(reason);
                                continue;
                            }
                            if !driver.failed
                                && (!driver.invocations.is_empty() || driver.state.active_scope().is_some())
                            {
                                driver.fail_observation(ObservationFailure::Disconnected);
                            }
                            // Native close waits for its final events to be consumed.
                            // Keep this owner reading until that cleanup finishes.
                            if !driver.cleanup.is_empty() {
                                continue;
                            }
                            break;
                        },
                        Some(command) => driver.command(command),
                        None => break,
                    },
                    event = events.recv(), if !events_closed && driver.pending_delivery.is_empty() => match event {
                        Ok(Some(event)) => driver.native_event(event).await,
                        Ok(None) => {
                            if !driver_shutdown.load(Ordering::Acquire) {
                                driver.fail_observation(ObservationFailure::Disconnected);
                                break;
                            }
                            if driver.retiring.is_none() { break; }
                            events_closed = true;
                        },
                        Err(crate::provider::events::EventDeliveryError::Closed) if driver_shutdown.load(Ordering::Acquire) => {
                            if driver.retiring.is_none() { break; }
                            events_closed = true;
                        },
                        Err(crate::provider::events::EventDeliveryError::Observation(failure)) => {
                            driver.fail_observation(failure); break;
                        },
                        Err(crate::provider::events::EventDeliveryError::OwnershipMismatch { .. }) => {
                            driver.fail_observation(ObservationFailure::OwnershipConflict); break;
                        },
                        Err(error) => { driver.fail_all(&error.to_string()); break; }
                    },
                    delivered = driver.submissions.join_next_with_id(), if !driver.submissions.is_empty() => {
                        match delivered {
                            Some(Ok((id, outcome))) => { driver.submission_finished(id); driver.acceptance(outcome); },
                            Some(Err(error)) if error.is_cancelled() => { driver.submission_finished(error.id()); },
                            Some(Err(error)) => { driver.submission_finished(error.id()); driver.fail_operation(ObservationFailure::LifecycleUnavailable, forge_provider_api::error::ProviderOperation::InputSubmit, Arc::new(error)); },
                            None => {},
                        }
                    },
                    result = driver.deliveries.join_next(), if !driver.deliveries.is_empty() => {
                        match result {
                            Some(Ok(Ok(()))) | None => {},
                            Some(Ok(Err(ProviderError::Observation(failure)))) => driver.fail_observation(failure),
                            Some(Ok(Err(error))) => driver.fail_operation(ObservationFailure::Disconnected, forge_provider_api::error::ProviderOperation::InputSubmit, Arc::new(error)),
                            Some(Err(error)) => driver.fail_operation(ObservationFailure::LifecycleUnavailable, forge_provider_api::error::ProviderOperation::InputSubmit, Arc::new(error)),
                        }
                    },
                    result = driver.cleanup.join_next(), if !driver.cleanup.is_empty() => { driver.cleanup_finished(result); },
                }
                driver.publish_activity();
                if driver.retiring.is_some()
                    && driver.cleanup.is_empty()
                    && !driver.failed
                    && driver.pending_delivery.is_empty()
                {
                    while driver.pending_delivery.is_empty() {
                        match events.try_recv() {
                            Ok(event) => driver.native_event(event).await,
                            Err(
                                crate::provider::events::EventDeliveryError::Empty
                                | crate::provider::events::EventDeliveryError::Closed,
                            ) => break,
                            Err(crate::provider::events::EventDeliveryError::Observation(
                                failure,
                            )) => {
                                driver.fail_observation(failure);
                                break;
                            }
                            Err(
                                crate::provider::events::EventDeliveryError::OwnershipMismatch {
                                    ..
                                },
                            ) => {
                                driver.fail_observation(ObservationFailure::OwnershipConflict);
                                break;
                            }
                            Err(
                                crate::provider::events::EventDeliveryError::InvalidTransportData
                                | crate::provider::events::EventDeliveryError::IncompleteTransfer {
                                    ..
                                },
                            ) => {
                                driver.fail_observation(ObservationFailure::Protocol);
                                break;
                            }
                        }
                    }
                    while let Some(outcome) = driver.submissions.try_join_next_with_id() {
                        match outcome {
                            Ok((id, outcome)) => {
                                driver.submission_finished(id);
                                driver.acceptance(outcome);
                            }
                            Err(error) if error.is_cancelled() => {
                                driver.submission_finished(error.id())
                            }
                            Err(error) => {
                                driver.submission_finished(error.id());
                                driver.fail_operation(
                                    ObservationFailure::LifecycleUnavailable,
                                    forge_provider_api::error::ProviderOperation::InputSubmit,
                                    Arc::new(error),
                                );
                            }
                        }
                    }
                    if driver.pending_delivery.is_empty()
                        && (driver.state.active_scope().is_some()
                            || !driver.submissions.is_empty()
                            || driver
                                .invocations
                                .values()
                                .any(|invocation| invocation.pending_controls != 0))
                    {
                        driver.fail_observation(ObservationFailure::LifecycleUnavailable);
                    }
                }
                if (driver.failed
                    || driver.retiring.is_some() && driver.pending_delivery.is_empty())
                    && driver.cleanup.is_empty()
                {
                    break;
                }
            }
            if !driver.failed
                && driver.retiring.is_none()
                && (!driver.invocations.is_empty() || driver.state.active_scope().is_some())
            {
                driver.fail_observation(ObservationFailure::Disconnected);
            }
            driver.submissions.abort_all();
            while let Some(result) = driver.submissions.join_next_with_id().await {
                match result {
                    Ok((id, _)) => driver.submission_finished(id),
                    Err(error) => {
                        driver.submission_finished(error.id());
                        if !error.is_cancelled() {
                            driver.retain_collection_failure(
                                forge_provider_api::error::ProviderOperation::InputSubmit,
                                Arc::new(error),
                            );
                        }
                    }
                }
            }
            while let Some(result) = driver.cleanup.join_next().await {
                driver.cleanup_finished(Some(result));
            }
            let delivery = driver.delivery.take();
            if let Some(delivery) = delivery {
                while let Some(retained) = driver.pending_delivery.pop_front() {
                    if let Err(error) = delivery.send(retained).await {
                        error.0.fail(ObservationFailure::Disconnected);
                        driver.retain_collection_failure(
                            forge_provider_api::ProviderOperation::InputSubmit,
                            Arc::new(ProviderError::Observation(ObservationFailure::Disconnected)),
                        );
                    }
                }
                drop(delivery);
            }
            while let Some(result) = driver.deliveries.join_next().await {
                match result {
                    Ok(Ok(())) => {}
                    Ok(Err(error)) => driver.retain_collection_failure(
                        forge_provider_api::error::ProviderOperation::InputSubmit,
                        Arc::new(error),
                    ),
                    Err(error) => driver.retain_collection_failure(
                        forge_provider_api::error::ProviderOperation::InputSubmit,
                        Arc::new(error),
                    ),
                }
            }
            driver.idle.close(
                (!driver.failed).then_some(driver.retiring.unwrap_or(OwnerCloseReason::Shutdown)),
            );
            if let Ok(mut activity) = driver.activity.lock() {
                *activity = None;
            }
            if let Some(report) = driver.cleanup_failure {
                return Err(ProviderError::Cleanup(report));
            }
            if driver.retiring.is_some()
                && let Some(failure) = driver.first_failure
            {
                return Err(ProviderError::Observation(failure));
            }
            Ok(())
        });
        Ok(Self {
            owner,
            provider,
            commands,
            healthy,
            orderly_shutdown,
            task: tokio::sync::Mutex::new(ExecutionTask::Running(task)),
            activity,
            admitted,
            #[cfg(test)]
            delivery_pressure: pressure_observer,
            #[cfg(test)]
            presentation_queue,
        })
    }

    /// Close new actor admission while retaining existing invocations and their settlement delivery.
    pub(crate) fn close_admission(&self) {
        self.healthy.store(false, Ordering::Release);
    }

    /// Mark the native stream closure as intentional before shutting its provider down.
    pub(crate) fn begin_orderly_shutdown(&self) {
        self.healthy.store(false, Ordering::Release);
        self.orderly_shutdown.store(true, Ordering::Release);
    }

    /// Stop accepting work and join the driver after its bounded cleanup and delivery drain.
    pub(crate) async fn close(&self) -> Result<(), ProviderError> {
        self.close_inner(None).await
    }

    /// Collect intentional cancellation only after native termination is positively observed.
    #[cfg(test)]
    pub(crate) async fn close_with_reason(
        &self,
        reason: OwnerCloseReason,
    ) -> Result<(), ProviderError> {
        self.close_inner(Some(reason)).await
    }

    /// Retain the same driver handle when a collection waiter is cancelled.
    async fn close_inner(&self, reason: Option<OwnerCloseReason>) -> Result<(), ProviderError> {
        let mut task = self.task.lock().await;
        if let ExecutionTask::Failed(previous) = &*task {
            let report = collect_failed_execution(
                self.provider.as_ref(),
                &self.owner,
                previous.observation().clone(),
            )
            .await;
            if !report.collected() {
                let report = Arc::new(report);
                *task = ExecutionTask::Failed(report.clone());
                return Err(ProviderError::Cleanup(report));
            }
            *task = ExecutionTask::Closed;
            return Ok(());
        }
        let handle = match &mut *task {
            ExecutionTask::Running(handle) => handle,
            ExecutionTask::Closed => return Ok(()),
            ExecutionTask::Failed(report) => return Err(ProviderError::Cleanup(report.clone())),
        };
        let _ = self.commands.send(Command::Close(reason)).await;
        let result = handle.await;
        let result = match result {
            Ok(result) => result,
            Err(error) => Err(crate::provider::lifecycle_error::classify_source(
                forge_provider_api::error::ProviderOperation::InputSubmit,
                &error,
            )),
        };
        match result {
            Ok(()) => {
                *task = ExecutionTask::Closed;
                Ok(())
            }
            Err(ProviderError::Observation(failure)) => {
                // Resources were joined; the original unknown observation still
                // must be consumed before replacement may acquire this owner.
                *task = ExecutionTask::Closed;
                Err(ProviderError::Observation(failure))
            }
            Err(error) => {
                let report = match error {
                    ProviderError::Cleanup(report) => report,
                    error => Arc::new(forge_provider_api::error::ObservationCleanup::new(
                        ObservationFailure::Disconnected,
                        None,
                        None,
                        Some(forge_provider_api::error::CleanupFailure::Failed(Box::new(
                            error,
                        ))),
                    )),
                };
                *task = ExecutionTask::Failed(report.clone());
                Err(ProviderError::Cleanup(report))
            }
        }
    }

    /// Observe initialization and late events without taking ownership of native intake.
    #[cfg(test)]
    pub(crate) async fn session_events(
        &self,
    ) -> Result<crate::provider::events::ProviderEventStream, ProviderError> {
        Ok(self.session_observer().await?.1)
    }

    /// Share one ordered owner channel between active invocation and idle observation.
    pub(crate) async fn session_observer(
        &self,
    ) -> Result<
        (
            ProviderEventSink,
            crate::provider::events::ProviderEventStream,
        ),
        ProviderError,
    > {
        let (sink, stream) =
            crate::provider::events::channel(crate::provider::EventScope::Binding {
                owner: self.owner.clone(),
            });
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::ObserveSession {
                sink: sink.clone(),
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is no longer available"))?;
        response
            .await
            .map_err(|_| protocol("execution observation registration ended"))??;
        Ok((sink, stream))
    }

    /// Resolve an owner-targeted control against the engine's latest retained invocation.
    pub(crate) async fn steer_current(
        &self,
        input: crate::provider::SteeringInput,
    ) -> Result<(), ProviderError> {
        self.steer_current_authorized(input, None).await
    }

    /// Transfer callback authority to the native owner for checked control admission.
    pub(crate) async fn steer_current_authorized(
        &self,
        input: crate::provider::SteeringInput,
        authority: Option<crate::tools::TaskAuthority>,
    ) -> Result<(), ProviderError> {
        let submission = input
            .submission()
            .map(|receipt| SubmissionId::try_from(receipt.id().to_owned()))
            .transpose()
            .map_err(|error| protocol(&error.to_string()))?;
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::SteerCurrent {
                input,
                submission,
                authority,
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is no longer available"))?;
        response
            .await
            .map_err(|_| protocol("execution owner closed before steering acknowledgment"))?
    }

    /// Register a control on the original invocation before dispatching its native operation.
    pub(crate) async fn steer(
        &self,
        scope: InvocationScope,
        input: crate::provider::SteeringInput,
    ) -> Result<(), ProviderError> {
        if scope.owner != self.owner {
            return Err(protocol("steering belongs to another execution owner"));
        }
        let submission = input
            .submission()
            .map(|receipt| SubmissionId::try_from(receipt.id().to_owned()))
            .transpose()
            .map_err(|error| protocol(&error.to_string()))?;
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::Steer {
                scope,
                input,
                submission,
                authority: None,
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is no longer available"))?;
        response
            .await
            .map_err(|_| protocol("execution owner closed before steering acknowledgment"))?
    }

    /// Await native readiness and explicitly commit each attempt for one-shot callers.
    pub(crate) async fn run_once(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
        store: &Arc<crate::provider::submission::SubmissionStore>,
    ) -> Result<ProviderCompletion, ProviderError> {
        let scope = sink.invocation_scope()?.clone();
        let mut ready = self.run_until_ready(request, sink, store).await?;
        loop {
            match ready.ticket.commit().await {
                Ok(()) => return ready.outcome,
                Err(SettlementError::Stale) => ready = self.wait_ready(scope.clone()).await?,
                Err(SettlementError::Unavailable) => {
                    return Err(protocol("execution owner closed before settlement"));
                }
                Err(SettlementError::Provider(error)) => return Err(error),
            }
        }
    }

    /// Admit an attempt and return its native-ready preview without releasing host ownership.
    pub(crate) async fn run_until_ready(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
        store: &Arc<crate::provider::submission::SubmissionStore>,
    ) -> Result<NativeReady, ProviderError> {
        self.submit_until_ready(request, sink, store, None, None)
            .await
    }

    /// Preserve the prepared request when revision validation rejects before admission.
    pub(crate) async fn continue_attempt(
        &self,
        ticket: &SettlementTicket,
        request: ProviderRequest,
        sink: ProviderEventSink,
        store: &Arc<crate::provider::submission::SubmissionStore>,
    ) -> Result<ContinuationAttempt, ProviderError> {
        let supplied = sink.invocation_scope()?;
        if !self.commands.same_channel(&ticket.commands)
            || supplied.owner != ticket.scope.owner
            || supplied.exchange_id != ticket.scope.exchange_id
        {
            return Err(protocol(
                "continuation ticket belongs to another invocation or generation",
            ));
        }
        let scope =
            InvocationScope::new(ticket.scope.owner.clone(), ticket.scope.exchange_id.clone());
        let sink = sink.successor_invocation_sink(&scope)?;
        let (stale, mut rejected) = oneshot::channel();
        let result = self
            .submit_until_ready(
                request,
                sink,
                store,
                Some((
                    ticket.scope.clone(),
                    ticket.revision,
                    ticket.effects_revision,
                    ticket.nonce,
                )),
                Some(stale),
            )
            .await;
        if let Ok(request) = rejected.try_recv() {
            return Ok(ContinuationAttempt::Stale(Box::new(request)));
        }
        result.map(|ready| ContinuationAttempt::Ready(Box::new(ready)))
    }

    /// Prepare durable input before either initial or same-invocation admission.
    async fn submit_until_ready(
        &self,
        mut request: ProviderRequest,
        sink: ProviderEventSink,
        store: &Arc<crate::provider::submission::SubmissionStore>,
        continuation: Option<(InvocationScope, u64, u64, u64)>,
        stale: Option<oneshot::Sender<ProviderRequest>>,
    ) -> Result<NativeReady, ProviderError> {
        let scope = sink.invocation_scope()?.clone();
        if request.owner()? != self.owner || scope.owner != self.owner {
            return Err(protocol("execution request belongs to another owner"));
        }
        let id = {
            let submission = match request.submission.take() {
                Some(submission) => submission,
                None => store
                    .prepare(&request, self.provider.descriptor().kind)
                    .map_err(|_| protocol("submission preparation failed"))?,
            };
            submission
                .recover_owned(self.provider.as_ref(), &mut request, self)
                .await
                .map_err(|_| protocol("submission recovery failed"))?;
            let id = SubmissionId::try_from(submission.id().to_owned())
                .map_err(|error| protocol(&error.to_string()))?;
            request.submission = Some(submission);
            Some(id)
        };
        let context = request
            .control_context
            .get_or_insert_with(|| crate::tools::ControlTurnContext::inactive(request.mode));
        if continuation.is_none() {
            context.begin_provider_execution();
        }
        let completion = context.clone();
        let (reply, response) = oneshot::channel();
        let waiter = ReadinessWaiter::new();
        self.commands
            .send(Command::Execute {
                request: Box::new(request),
                scope,
                submission: id,
                sink,
                completion: Box::new(completion),
                continuation,
                stale,
                abandoned: waiter.ownership(),
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is no longer available"))?;
        let result = response
            .await
            .map_err(|_| protocol("execution owner closed before completion"))?;
        waiter.delivered();
        result
    }

    /// Await a fresh preview after a stale settlement token.
    pub(crate) async fn wait_ready(
        &self,
        scope: InvocationScope,
    ) -> Result<NativeReady, ProviderError> {
        let (reply, response) = oneshot::channel();
        let waiter = ReadinessWaiter::new();
        self.commands
            .send(Command::WaitReady {
                scope,
                abandoned: waiter.ownership(),
                reply,
            })
            .await
            .map_err(|_| protocol("execution owner is no longer available"))?;
        let result = response
            .await
            .map_err(|_| protocol("execution owner closed before readiness"))?;
        waiter.delivered();
        result
    }
}

impl Drop for SessionExecution {
    fn drop(&mut self) {
        if let ExecutionTask::Running(task) = self.task.get_mut() {
            task.abort();
        }
    }
}

impl Driver {
    /// Register the exact prepared input once while retaining only lightweight collected prevention evidence.
    fn reserve_input(
        &mut self,
        scope: InvocationScope,
        reservation: InputReservation,
        origin: ReservationOrigin,
    ) -> Result<(), ProviderError> {
        let invalid = || {
            ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::InputSubmit,
                forge_provider_api::ProviderFailureKind::WrongInvocation,
            ))
        };
        if scope.owner != self.owner || self.failed {
            return Err(invalid());
        }
        let record = reservation
            .submission
            .record()
            .map_err(crate::provider::submission::journal_failure)?;
        if record.request.owner()? != self.owner || record.attempt_id != reservation.attempt_id {
            return Err(invalid());
        }
        let mut admitted = self
            .admitted
            .lock()
            .map_err(|_| protocol("admitted scope snapshot poisoned"))?;
        if admitted.contains(&scope) {
            let reserved = match self.reservations.get(&scope) {
                Some(previous)
                    if previous.submission.receipt_id() == reservation.submission.receipt_id()
                        && previous.attempt_id == reservation.attempt_id =>
                {
                    true
                }
                Some(_) | None => false,
            };
            let prevented = self
                .prevented_invocations
                .get(&scope)
                .is_some_and(|previous| {
                    &previous.submission_id == reservation.submission.receipt_id()
                        && previous.attempt_id == reservation.attempt_id
                });
            return if reserved || prevented {
                Ok(())
            } else {
                Err(invalid())
            };
        }
        match record.delivery {
            crate::provider::submission::SubmissionState::Prepared
            | crate::provider::submission::SubmissionState::Cancelled => {}
            crate::provider::submission::SubmissionState::Sending { .. }
            | crate::provider::submission::SubmissionState::Accepted { .. } => {
                return Err(invalid());
            }
        }
        admitted.insert(scope.clone());
        let publish = match origin {
            ReservationOrigin::PreparedForeground => true,
            ReservationOrigin::Execution => {
                !self.cancelled_exchanges.contains_key(&scope.exchange_id)
                    && !self.cancelled_admissions.contains(&scope)
            }
        };
        if publish {
            self.latest_reservations
                .insert(scope.exchange_id.clone(), scope.clone());
        }
        if matches!(
            record.delivery,
            crate::provider::submission::SubmissionState::Cancelled
        ) {
            self.prevented_invocations.insert(
                scope,
                PreventedInput {
                    submission_id: reservation.submission.receipt_id().clone(),
                    attempt_id: reservation.attempt_id,
                },
            );
        } else {
            self.reservations.insert(scope, reservation);
        }
        Ok(())
    }
    /// Acknowledge task destruction before cancellation collects resources created during preparation.
    fn submission_finished(&mut self, id: tokio::task::Id) {
        if let Some(finished) = self.submission_exits.remove(&id) {
            finished.send_replace(true);
        }
    }
    /// Publish a read-only owner snapshot without transferring transition authority to readers.
    fn publish_activity(&self) {
        if let Ok(mut activity) = self.activity.lock() {
            *activity = (!self.failed && self.retiring.is_none())
                .then(|| {
                    self.state
                        .active_scope()
                        .filter(|scope| {
                            self.invocations
                                .get(&scope.invocation_id)
                                .is_some_and(|invocation| invocation.native_settlement.is_none())
                        })
                        .cloned()
                })
                .flatten();
        }
    }
    /// Reduce a binding event before acknowledging its stream position to operation waiters.
    async fn native_event(&mut self, envelope: crate::provider::bridge::HostEvent) {
        let crate::provider::bridge::HostEvent { event, native } = envelope;
        let category = event_category(&event);
        let scope = event.scope();
        if scope.owner() != &self.owner {
            self.fail_observation(ObservationFailure::OwnershipConflict);
            return;
        }
        let identity_first =
            matches!(&event, ProviderEvent::SessionIdentity { .. }) && native.is_some();
        if identity_first
            && scope
                .invocation()
                .is_some_and(|scope| !self.state.contains(scope))
        {
            self.fail_observation(ObservationFailure::OwnershipConflict);
            return;
        }
        if native.is_some()
            && let Err(failure) = self.validate_native_event(&event)
        {
            self.fail_observation(failure);
            return;
        }
        if let Some((binding, sequence)) = &native {
            let core = crate::exchange::core_log::CoreRecord::provider_event_metadata(
                binding.clone(),
                *sequence,
                &event,
            );
            if let Err(error) = self.provider.record_core_event(&self.owner, &core) {
                self.fail_observation_with_origin(
                    ObservationFailure::LifecycleUnavailable,
                    FailureOrigin::Error(error),
                );
                return;
            }
        }
        // Identity reducers consult consumed binding metadata; publish only after that metadata advances.
        // Native terminal and settlement events retain their reducer-before-watermark ordering.
        let after_consumption = if identity_first {
            Some(event)
        } else {
            self.event(event);
            None
        };
        if let Some((binding, sequence)) = native {
            if let Err(error) = self.provider.event_consumed(&binding, sequence).await {
                self.fail_observation_with_origin(
                    ObservationFailure::Protocol,
                    FailureOrigin::Error(error),
                );
                return;
            } else {
                self.trace.record(self.owner.session_id.as_str(), "provider.event.consumed", serde_json::json!({
                    "binding_id":binding, "sequence":sequence, "scope":scope, "category":category,
                }));
            }
        }
        if let Some(event) = after_consumption
            && !self.failed
        {
            self.event(event);
        }
    }

    /// Validate immutable lifecycle preconditions before durable consumption evidence or publication.
    fn validate_native_event(&self, event: &ProviderEvent) -> Result<(), ObservationFailure> {
        let scope = event.scope();
        let Some(scope) = scope.invocation() else {
            return Ok(());
        };
        if !self.state.contains(scope) {
            return Err(ObservationFailure::OwnershipConflict);
        }
        let invocation = self
            .invocations
            .get(&scope.invocation_id)
            .filter(|invocation| &invocation.scope == scope);
        if let ProviderEvent::Settled { settlement } = event {
            let invocation = invocation
                .or_else(|| {
                    self.committed_invocations
                        .get(&scope.invocation_id)
                        .filter(|invocation| &invocation.scope == scope)
                })
                .ok_or(ObservationFailure::OwnershipConflict)?;
            if invocation
                .native_settlement
                .as_ref()
                .is_some_and(|previous| {
                    previous.binding_id != settlement.binding_id
                        || previous.result != settlement.result
                })
                || matches!(&invocation.terminal, Some(Ok(result)) if result != &settlement.result)
            {
                return Err(ObservationFailure::Protocol);
            }
        }
        let ProviderEvent::Execution { observation } = event else {
            return Ok(());
        };
        if matches!(observation.update(), ExecutionUpdate::Started { .. })
            && (invocation.is_none()
                || invocation.is_some_and(|invocation| invocation.native_settlement.is_some()))
        {
            return Err(ObservationFailure::OwnershipConflict);
        }
        if invocation.is_some_and(|invocation| invocation.revision.checked_add(1).is_none()) {
            return Err(ObservationFailure::Protocol);
        }
        let state = self
            .state
            .invocations
            .get(&scope.invocation_id)
            .ok_or(ObservationFailure::OwnershipConflict)?;
        match observation.update() {
            ExecutionUpdate::Finished {
                address,
                covered_submissions,
                result,
                ..
            }
            | ExecutionUpdate::PublicationFailed {
                address,
                covered_submissions,
                result,
                ..
            } => {
                let previous = state
                    .executions
                    .get(&ExecutionState::key(address.clone()))
                    .ok_or(ObservationFailure::Protocol)?;
                if covered_submissions
                    .iter()
                    .any(|id| !state.submissions.contains_key(id))
                {
                    return Err(ObservationFailure::Protocol);
                }
                let publication = if let ExecutionUpdate::PublicationFailed { failure, .. } =
                    observation.update()
                {
                    Some(*failure)
                } else {
                    None
                };
                let current_covered: std::collections::HashSet<_> =
                    covered_submissions.iter().cloned().collect();
                if let super::NativeExecution::Finished {
                    result: prior,
                    covered,
                    publication_failure,
                } = previous
                    && (prior.as_ref() != result
                        || covered != &current_covered
                        || publication_failure != &publication)
                {
                    return Err(ObservationFailure::Protocol);
                }
            }
            ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions,
                result,
                ..
            }
            | ExecutionUpdate::ResolutionPublicationFailed {
                covered_submissions,
                result,
                ..
            } => {
                if covered_submissions
                    .iter()
                    .any(|id| !state.submissions.contains_key(id))
                {
                    return Err(ObservationFailure::Protocol);
                }
                let covered: std::collections::HashSet<_> =
                    covered_submissions.iter().cloned().collect();
                let publication =
                    if let ExecutionUpdate::ResolutionPublicationFailed { failure, .. } =
                        observation.update()
                    {
                        Some(*failure)
                    } else {
                        None
                    };
                for previous in &state.without_execution {
                    if previous.covered == covered {
                        if previous.result != *result || previous.publication_failure != publication
                        {
                            return Err(ObservationFailure::Protocol);
                        }
                    } else if !previous.covered.is_disjoint(&covered) {
                        return Err(ObservationFailure::Protocol);
                    }
                }
            }
            ExecutionUpdate::Started { .. } | ExecutionUpdate::ObservationLost { .. } => {}
        }
        Ok(())
    }
    /// Preserve a failed owned task while keeping the first observation cause immutable.
    fn retain_collection_failure(
        &mut self,
        operation: forge_provider_api::error::ProviderOperation,
        source: Arc<dyn std::error::Error + Send + Sync>,
    ) {
        let next = crate::provider::lifecycle_error::classify_source(operation, source.as_ref());
        let failure = self
            .first_failure
            .get_or_insert(ObservationFailure::LifecycleUnavailable)
            .clone();
        let report = self.cleanup_failure.get_or_insert_with(|| {
            Arc::new(forge_provider_api::error::ObservationCleanup::new(
                failure.clone(),
                None,
                None,
                None,
            ))
        });
        Arc::make_mut(report).record_collection_failure(
            forge_provider_api::error::CleanupFailure::Failed(Box::new(next)),
        );
        self.failed = true;
        match &self.idle {
            SessionObserver::Inbox(inbox) => inbox.fail(failure),
            SessionObserver::Stream(sink) => sink.fail(
                crate::provider::events::EventDeliveryError::Observation(failure),
            ),
        }
    }
    /// Retain a task or delivery failure as the origin of sticky observation loss.
    fn fail_operation(
        &mut self,
        failure: ObservationFailure,
        operation: forge_provider_api::error::ProviderOperation,
        source: Arc<dyn std::error::Error + Send + Sync>,
    ) {
        self.fail_observation_with_origin(
            failure,
            FailureOrigin::Error(crate::provider::lifecycle_error::classify_source(
                operation,
                source.as_ref(),
            )),
        );
    }
    /// Own deliberate cancellation while preserving independently observed terminal evidence.
    fn begin_retirement(&mut self, reason: OwnerCloseReason) {
        if self.retiring.is_some() || self.failed {
            return;
        }
        self.retiring = Some(reason);
        self.healthy.store(false, Ordering::Release);
        let provider = self.provider.clone();
        let owner = self.owner.clone();
        self.cleanup.spawn(async move {
            use forge_provider_api::error::{CleanupFailure, ObservationCleanup};
            let collection = provider.session_close_owner(&owner).await;
            let classify = |result: Result<(), ProviderError>| match result {
                Ok(()) => None,
                Err(error) => Some(CleanupFailure::Failed(Box::new(error))),
            };
            ObservationCleanup::new(
                ObservationFailure::LifecycleUnavailable,
                None,
                None,
                classify(collection),
            )
            .with_origin(ProviderError::Cancelled)
        });
    }
    fn command(&mut self, command: Command) {
        match command {
            Command::Close(_) => unreachable!("close is handled by the driver loop"),
            Command::ReserveInvocation { scope, reservation } => {
                if let Err(error) =
                    self.reserve_input(scope, reservation, ReservationOrigin::PreparedForeground)
                {
                    self.fail_observation_with_origin(
                        ObservationFailure::OwnershipConflict,
                        FailureOrigin::Error(error),
                    );
                }
            }
            Command::ResumeExchange { collection, reply } => {
                let scope = collection.scope.clone();
                let admitted = match self.admitted.lock() {
                    Ok(admitted) => admitted.contains(&scope),
                    Err(_) => {
                        let _ = reply.send(Err(protocol("admitted scope snapshot poisoned")));
                        return;
                    }
                };
                let collected = |invocation: &Invocation| {
                    invocation.pending_controls == 0
                        && invocation.terminal.is_some()
                        && invocation.native_settlement.is_some()
                        && matches!(invocation.host_collection, HostCollection::Settled)
                };
                let prior = self
                    .invocations
                    .get(&scope.invocation_id)
                    .or_else(|| self.committed_invocations.get(&scope.invocation_id))
                    .filter(|invocation| invocation.scope == scope);
                let previous_collected =
                    prior.is_some_and(collected) || self.prevented_invocations.contains_key(&scope);
                let all_collected = self
                    .invocations
                    .values()
                    .filter(|invocation| invocation.scope.exchange_id == scope.exchange_id)
                    .all(collected);
                let current_fence = self
                    .cancelled_exchanges
                    .get(&scope.exchange_id)
                    .is_some_and(|fence| {
                        fence.scope == scope
                            && fence.fence_id == collection.fence_id
                            && self.commands.same_channel(&collection.commands)
                    });
                let latest = self.latest_reservations.get(&scope.exchange_id) == Some(&scope);
                if self.failed
                    || scope.owner != self.owner
                    || !admitted
                    || !previous_collected
                    || !current_fence
                    || !latest
                {
                    self.trace.record(self.owner.session_id.as_str(), "provider.replacement.rejected", serde_json::json!({
                        "scope":scope, "fence_id":collection.fence_id, "owner_failed":self.failed,
                        "admitted":admitted, "previous_collected":previous_collected,
                        "current_fence":current_fence, "latest_reservation":latest,
                    }));
                    let _ = reply.send(Err(ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::SessionConfigure,
                            forge_provider_api::ProviderFailureKind::WrongInvocation,
                        ),
                    )));
                    return;
                }
                if self.state.active_scope().is_some() || !all_collected {
                    let _ = reply.send(Err(ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::SessionConfigure,
                            forge_provider_api::ProviderFailureKind::ExecutionActive,
                        ),
                    )));
                    return;
                }
                self.cancelled_exchanges.remove(&scope.exchange_id);
                let _ = reply.send(Ok(()));
            }
            Command::CancelExchange { scope, reply } => {
                let registered = self
                    .admitted
                    .lock()
                    .is_ok_and(|admitted| admitted.contains(&scope));
                let fence = if registered {
                    self.latest_reservations
                        .get(&scope.exchange_id)
                        .cloned()
                        .unwrap_or_else(|| scope.clone())
                } else {
                    scope.clone()
                };
                let collection = ExchangeCollection {
                    scope: fence.clone(),
                    fence_id: forge_provider_api::OperationId::new(
                        uuid::Uuid::new_v4().to_string(),
                    )
                    .expect("generated exchange fence identity is nonempty"),
                    commands: self.commands.clone(),
                };
                self.cancelled_exchanges
                    .insert(scope.exchange_id.clone(), collection.clone());
                let target = self
                    .state
                    .active_scope()
                    .filter(|active| {
                        active.owner == scope.owner && active.exchange_id == scope.exchange_id
                    })
                    .cloned()
                    .unwrap_or(fence);
                self.command(Command::Cancel {
                    scope: target,
                    reply: CancellationReply::Exchange { collection, reply },
                });
            }
            Command::HostEffectsCollected {
                scope,
                operation_id,
                result,
            } => {
                self.trace.record(self.owner.session_id.as_str(), "provider.operation.completed", serde_json::json!({
                    "operation_id":operation_id, "operation":forge_provider_api::ProviderOperation::InputSubmit,
                    "phase":forge_provider_api::ProviderPhase::Collection, "scope":scope, "success":result.is_ok(),
                }));
                let Some(invocation) =
                    self.invocations
                        .get_mut(&scope.invocation_id)
                        .filter(|invocation| {
                            invocation.scope == scope
                                && matches!(invocation.host_collection, HostCollection::Collecting)
                        })
                else {
                    return;
                };
                match result {
                    Ok(()) => invocation.host_collection = HostCollection::Settled,
                    Err(error) => {
                        self.fail_observation_with_origin(
                            ObservationFailure::LifecycleUnavailable,
                            FailureOrigin::Error(error),
                        );
                        return;
                    }
                }
                self.settle(&scope.invocation_id);
            }
            Command::ReconcileInput {
                submission,
                observation,
                reply,
            } => {
                let result = if self.failed {
                    Err(protocol("reconciliation has no live owner").into())
                } else {
                    submission.apply_reconciliation(*observation)
                };
                let _ = reply.send(result);
            }
            Command::Cancel { scope, reply } => {
                if self.failed || scope.owner != self.owner {
                    reply.complete(Err(protocol("cancellation has no live owner")));
                    return;
                }
                if self
                    .invocations
                    .get(&scope.invocation_id)
                    .or_else(|| self.committed_invocations.get(&scope.invocation_id))
                    .is_some_and(|invocation| {
                        invocation.scope == scope
                            && invocation.native_settlement.is_some()
                            && matches!(invocation.host_collection, HostCollection::Settled)
                    })
                {
                    reply.complete(Ok(()));
                    return;
                }
                let Some(attempt) = self.attempts.get(&scope) else {
                    if let Some(reservation) = self.reservations.remove(&scope) {
                        match reservation
                            .submission
                            .retire_prepared_attempt(&reservation.attempt_id)
                        {
                            Ok(crate::provider::submission::SubmissionRetirement::Cancelled) => {
                                self.prevented_invocations.insert(
                                    scope,
                                    PreventedInput {
                                        submission_id: reservation.submission.receipt_id().clone(),
                                        attempt_id: reservation.attempt_id,
                                    },
                                );
                                reply.complete(Ok(()));
                            }
                            Ok(
                                crate::provider::submission::SubmissionRetirement::Sending
                                | crate::provider::submission::SubmissionRetirement::Accepted,
                            ) => {
                                self.reservations.insert(scope, reservation);
                                reply.complete(Err(ProviderError::DeliveryUncertain));
                            }
                            Err(error) => {
                                self.reservations.insert(scope, reservation);
                                reply.complete(Err(error));
                            }
                        }
                        return;
                    }
                    self.cancelled_admissions.insert(scope);
                    reply.complete(Ok(()));
                    return;
                };
                let retirement = match attempt
                    .submission
                    .retire_prepared_attempt(&attempt.attempt_id)
                {
                    Ok(retirement) => retirement,
                    Err(error) => {
                        reply.complete(Err(error));
                        return;
                    }
                };
                let provider = self.provider.clone();
                let mut dispatch = attempt.dispatch.clone();
                let mut finished = attempt.finished.clone();
                let kind = match retirement {
                    crate::provider::submission::SubmissionRetirement::Cancelled => {
                        attempt.task.abort();
                        CancellationKind::Prevented(PreventedInput {
                            submission_id: attempt.submission.receipt_id().clone(),
                            attempt_id: attempt.attempt_id.clone(),
                        })
                    }
                    crate::provider::submission::SubmissionRetirement::Sending
                    | crate::provider::submission::SubmissionRetirement::Accepted => {
                        CancellationKind::Native
                    }
                };
                let commands = self.commands.clone();
                self.deliveries.spawn(async move {
                    let result = match &kind {
                        CancellationKind::Prevented(_) => {
                            let stopped = finished.wait_for(|finished| *finished).await.map(drop);
                            match stopped {
                                Ok(()) => {
                                    provider
                                        .cancel_prepared_invocation(&scope, retirement)
                                        .await
                                }
                                Err(_) => Err(protocol(
                                    "prevented submission lost task collection evidence",
                                )),
                            }
                        }
                        CancellationKind::Native => match dispatch.wait().await {
                            Ok(confirmation) if confirmation.scope() == &scope => {
                                provider.collect_invocation(scope.clone()).await.map(|_| ())
                            }
                            Ok(_) => Err(protocol("cancellation dispatch identity changed")),
                            Err(_) => Err(ProviderError::DeliveryUncertain),
                        },
                    };
                    let _ = commands
                        .send(Command::CancellationFinished {
                            scope,
                            outcome: kind,
                            result,
                            reply,
                        })
                        .await;
                    Ok(())
                });
            }
            Command::CancellationFinished {
                scope,
                outcome,
                result,
                reply,
            } => {
                if let CancellationKind::Prevented(prevented) = outcome
                    && result.is_ok()
                {
                    // Completion can remove the attempt while collection awaits its worker.
                    // The successful collection result owns the proof needed for replacement.
                    self.prevented_invocations.insert(scope.clone(), prevented);
                    if let Some(attempt) = self.attempts.get(&scope)
                        && self
                            .state
                            .delivery(
                                &scope,
                                attempt.submission.receipt_id(),
                                DeliveryState::NotConsumed,
                            )
                            .is_err()
                    {
                        reply.complete(Err(protocol(
                            "prevented cancellation conflicts with delivery",
                        )));
                        return;
                    }
                    if let Some(invocation) = self
                        .invocations
                        .get_mut(&scope.invocation_id)
                        .filter(|invocation| invocation.scope == scope)
                    {
                        invocation.terminal = Some(Err(ProviderError::Cancelled));
                    }
                    self.settle(&scope.invocation_id);
                }
                reply.complete(result);
            }
            #[cfg(test)]
            Command::TestPublishEffect {
                scope,
                completion,
                reply,
            } => {
                let result = self
                    .invocations
                    .get(&scope.invocation_id)
                    .or_else(|| self.committed_invocations.get(&scope.invocation_id))
                    .filter(|invocation| invocation.scope == scope)
                    .ok_or_else(|| protocol("test effect has no retained invocation"))
                    .and_then(|invocation| {
                        invocation
                            .completion
                            .provider_completion
                            .publish(*completion)
                    });
                let _ = reply.send(result);
            }
            Command::SteerCurrent {
                input,
                submission,
                authority,
                reply,
            } => {
                let Some(scope) = self.state.latest_scope().cloned() else {
                    let _ = reply.send(Err(not_admitted()));
                    return;
                };
                self.command(Command::Steer {
                    scope,
                    input,
                    submission,
                    authority,
                    reply,
                });
            }
            Command::Steer {
                scope,
                input,
                submission,
                authority,
                reply,
            } => {
                if self.failed
                    || self.state.active_scope() != Some(&scope)
                    || self
                        .invocations
                        .get(&scope.invocation_id)
                        .is_none_or(|invocation| invocation.native_settlement.is_some())
                {
                    let _ = reply.send(Err(not_admitted()));
                    return;
                }
                if self
                    .state
                    .active_scope()
                    .is_some_and(|active| active != &scope)
                {
                    let _ = reply.send(Err(not_admitted()));
                    return;
                }
                let mut reply = Some(reply);
                let dispatch_authority = authority.clone();
                let dispatch = || -> Result<(), ProviderError> {
                    let invocation = self
                        .invocations
                        .get_mut(&scope.invocation_id)
                        .or_else(|| self.committed_invocations.get_mut(&scope.invocation_id))
                        .ok_or_else(|| protocol("steering has no retained host authority"))?;
                    let revision = invocation
                        .revision
                        .checked_add(1)
                        .ok_or_else(|| protocol("settlement revision exhausted"))?;
                    let pending = invocation
                        .pending_controls
                        .checked_add(1)
                        .ok_or_else(|| protocol("pending control count exhausted"))?;
                    if let Some(id) = &submission {
                        self.state.admit(scope.clone(), id.clone())?;
                    }
                    invocation.pending_controls = pending;
                    invocation.revision = revision;
                    let provider = self.provider.clone();
                    let reply = reply
                        .take()
                        .expect("control reply is present before dispatch");
                    self.submissions.spawn(async move {
                        let result = match dispatch_authority
                            .as_ref()
                            .map(|authority| authority.validate_scope())
                            .transpose()
                        {
                            Err(error) => Err(ProviderError::PreparationFailed(Box::new(
                                crate::provider::lifecycle_error::classify(
                                    forge_provider_api::ProviderOperation::InputSubmit,
                                    error,
                                ),
                            ))),
                            Ok(_) => {
                                let result = provider
                                    .input_steer(&scope.owner, scope.clone(), input)
                                    .await;
                                if dispatch_authority
                                    .as_ref()
                                    .is_some_and(|authority| authority.validate_scope().is_err())
                                    && !matches!(&result, Err(ProviderError::NotConsumed))
                                {
                                    Err(ProviderError::DeliveryUncertain)
                                } else {
                                    result
                                }
                            }
                        };
                        SubmissionOutcome {
                            scope,
                            submission,
                            control_reply: Some(reply),
                            result,
                        }
                    });
                    Ok(())
                };
                let admitted = match authority {
                    Some(authority) => authority.commit_control(dispatch),
                    None => dispatch(),
                };
                if let Err(error) = admitted
                    && let Some(reply) = reply
                {
                    let _ = reply.send(Err(ProviderError::PreparationFailed(Box::new(error))));
                }
            }
            Command::ObserveSession { sink, reply } => {
                if self.failed || matches!(self.idle, SessionObserver::Stream(_)) {
                    let _ = reply.send(Err(protocol(
                        "session observer is already attached or closed",
                    )));
                    return;
                }
                let pending = match &self.idle {
                    SessionObserver::Inbox(inbox) => match inbox.drain() {
                        Ok(events) => events,
                        Err(error) => {
                            let _ = reply.send(Err(error));
                            return;
                        }
                    },
                    SessionObserver::Stream(_) => unreachable!("stream observer rejected above"),
                };
                self.idle = SessionObserver::Stream(Box::new(sink));
                for event in pending {
                    self.idle_event(event);
                }
                if self.failed {
                    let _ = reply.send(Err(ProviderError::Observation(
                        self.first_failure
                            .clone()
                            .unwrap_or(ObservationFailure::Disconnected),
                    )));
                    return;
                }
                let _ = reply.send(Ok(()));
            }
            Command::Execute {
                request,
                scope,
                submission,
                sink,
                completion,
                continuation,
                stale,
                abandoned,
                reply,
            } => {
                let mut request = *request;
                let completion = *completion;
                if self.failed {
                    let _ = reply.send(Err(protocol("execution owner is unavailable")));
                    return;
                }
                if !self.healthy.load(Ordering::Acquire) {
                    let _ = reply.send(Err(ProviderError::Cancelled));
                    return;
                }
                let Some(receipt) = request.submission.as_ref() else {
                    let _ = reply.send(Err(protocol("execution has no journal authority")));
                    return;
                };
                let record = match receipt.record() {
                    Ok(record) => record,
                    Err(error) => {
                        let _ =
                            reply.send(Err(crate::provider::submission::journal_failure(error)));
                        return;
                    }
                };
                if let Some(prevented) = self.prevented_invocations.get(&scope) {
                    let result = if prevented.submission_id == *receipt.receipt_id()
                        && prevented.attempt_id == record.attempt_id
                    {
                        ProviderError::Cancelled
                    } else {
                        ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::InputSubmit,
                            forge_provider_api::ProviderFailureKind::WrongInvocation,
                        ))
                    };
                    let _ = reply.send(Err(result));
                    return;
                }
                if !self.reservations.contains_key(&scope)
                    && let Err(error) = self.reserve_input(
                        scope.clone(),
                        InputReservation {
                            submission: receipt.clone(),
                            attempt_id: record.attempt_id.clone(),
                        },
                        ReservationOrigin::Execution,
                    )
                {
                    let _ = reply.send(Err(error));
                    return;
                }
                if self.prevented_invocations.contains_key(&scope) {
                    let _ = reply.send(Err(ProviderError::Cancelled));
                    return;
                }
                let reservation = self
                    .reservations
                    .remove(&scope)
                    .expect("execution reservation was installed above");
                if reservation.submission.receipt_id() != receipt.receipt_id()
                    || reservation.attempt_id != record.attempt_id
                {
                    let _ = reply.send(Err(ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::InputSubmit,
                            forge_provider_api::ProviderFailureKind::WrongInvocation,
                        ),
                    )));
                    return;
                }
                let attempt_id = reservation.attempt_id;
                if matches!(
                    record.delivery,
                    crate::provider::submission::SubmissionState::Cancelled
                ) {
                    self.prevented_invocations.insert(
                        scope.clone(),
                        PreventedInput {
                            submission_id: receipt.receipt_id().clone(),
                            attempt_id,
                        },
                    );
                    let _ = reply.send(Err(ProviderError::Cancelled));
                    return;
                }
                if !matches!(
                    record.delivery,
                    crate::provider::submission::SubmissionState::Prepared
                ) {
                    let _ = reply.send(Err(ProviderError::DeliveryUncertain));
                    return;
                }
                if self.cancelled_admissions.remove(&scope)
                    || self.cancelled_exchanges.contains_key(&scope.exchange_id)
                {
                    match receipt.retire_prepared_attempt(&attempt_id) {
                        Ok(crate::provider::submission::SubmissionRetirement::Cancelled) => {
                            self.prevented_invocations.insert(
                                scope.clone(),
                                PreventedInput {
                                    submission_id: receipt.receipt_id().clone(),
                                    attempt_id,
                                },
                            );
                        }
                        Ok(
                            crate::provider::submission::SubmissionRetirement::Sending
                            | crate::provider::submission::SubmissionRetirement::Accepted,
                        ) => {
                            let _ = reply.send(Err(ProviderError::DeliveryUncertain));
                            return;
                        }
                        Err(error) => {
                            let _ = reply.send(Err(error));
                            return;
                        }
                    }
                    let _ = reply.send(Err(ProviderError::Cancelled));
                    return;
                }
                if let Some((previous_scope, revision, effects_revision, nonce)) = continuation {
                    if scope.owner != previous_scope.owner
                        || scope.exchange_id != previous_scope.exchange_id
                        || scope.invocation_id == previous_scope.invocation_id
                    {
                        let _ = reply.send(Err(protocol(
                            "continuation requires a fresh scope within its exchange",
                        )));
                        return;
                    }
                    let Some(invocation) = self
                        .invocations
                        .get_mut(&previous_scope.invocation_id)
                        .filter(|invocation| invocation.scope == previous_scope)
                    else {
                        let _ = reply.send(Err(protocol(
                            "continuation invocation is no longer retained",
                        )));
                        return;
                    };
                    let effects_current = invocation
                        .completion
                        .provider_completion
                        .preview()
                        .map(|preview| preview.revision == effects_revision);
                    if invocation.revision != revision
                        || invocation.ticket_nonce != nonce
                        || !invocation.ticket_outstanding
                        || invocation.pending_controls != 0
                        || invocation.reply.is_some()
                        || !matches!(
                            self.state.readiness(&previous_scope),
                            Ok(ExecutionReadiness::Complete)
                        )
                        || !matches!(effects_current, Ok(true))
                    {
                        if let Some(stale) = stale {
                            let _ = stale.send(request);
                        }
                        let _ =
                            reply.send(Err(protocol("continuation settlement ticket is stale")));
                        return;
                    }
                    if let Some(submission) = &submission
                        && let Err(error) = self.state.admit(scope.clone(), submission.clone())
                    {
                        let _ = reply.send(Err(protocol(&error.to_string())));
                        return;
                    }
                    let Some(context) = request.control_context.as_mut() else {
                        let _ = reply.send(Err(protocol("continuation has no control context")));
                        self.fail_all("continuation has no control context");
                        return;
                    };
                    context.provider_completion = invocation.completion.provider_completion.clone();
                    if let Err(error) = context.provider_completion.begin_attempt() {
                        let _ = reply.send(Err(error));
                        self.fail_all("continuation completion slot failed");
                        return;
                    }
                    let completion = context.clone();
                    let previous = self
                        .invocations
                        .remove(&previous_scope.invocation_id)
                        .expect("continuation predecessor was validated under the owner");
                    self.attempts.remove(&previous_scope);
                    self.committed_invocations
                        .insert(previous_scope.invocation_id.clone(), previous);
                    self.invocations.insert(
                        scope.invocation_id.clone(),
                        Invocation {
                            scope: scope.clone(),
                            output: sink,
                            reply: Some(reply),
                            completion,
                            terminal: None,
                            native_settlement: None,
                            host_collection: HostCollection::Pending,
                            publication_failure: None,
                            pending_controls: 0,
                            revision: 0,
                            ticket_outstanding: false,
                            ticket_nonce: 0,
                            abandoned,
                            idle_observation: false,
                        },
                    );
                } else {
                    if !self.invocations.is_empty() || self.state.active_scope().is_some() {
                        let _ = reply.send(Err(protocol(
                            "execution owner is unavailable or already active",
                        )));
                        return;
                    }
                    if let Some(submission) = &submission
                        && let Err(error) = self.state.admit(scope.clone(), submission.clone())
                    {
                        let _ = reply.send(Err(protocol(&error.to_string())));
                        return;
                    }
                    self.invocations.insert(
                        scope.invocation_id.clone(),
                        Invocation {
                            scope: scope.clone(),
                            output: sink,
                            reply: Some(reply),
                            completion,
                            terminal: None,
                            native_settlement: None,
                            host_collection: HostCollection::Pending,
                            publication_failure: None,
                            pending_controls: 0,
                            revision: 0,
                            ticket_outstanding: false,
                            ticket_nonce: 0,
                            abandoned,
                            idle_observation: false,
                        },
                    );
                }
                let provider = self.provider.clone();
                let attempt_scope = scope.clone();
                let attempt_submission = request.submission.clone();
                let attempt_dispatch = request.dispatch.observer();
                let (finished, finish_observer) = tokio::sync::watch::channel(false);
                let task = self.submissions.spawn(async move {
                    let result = provider.input_submit(request, scope.clone()).await;
                    SubmissionOutcome {
                        scope,
                        submission,
                        control_reply: None,
                        result,
                    }
                });
                self.submission_exits.insert(task.id(), finished);
                if let Some(submission) = attempt_submission {
                    self.attempts.insert(
                        attempt_scope,
                        AttemptControl {
                            submission,
                            dispatch: attempt_dispatch,
                            task,
                            finished: finish_observer,
                            attempt_id,
                        },
                    );
                }
            }
            Command::WaitReady {
                scope,
                abandoned,
                reply,
            } => {
                let Some(invocation) = self
                    .invocations
                    .get_mut(&scope.invocation_id)
                    .filter(|invocation| invocation.scope == scope)
                else {
                    let _ =
                        reply.send(Err(protocol("settlement invocation is no longer retained")));
                    return;
                };
                if invocation.abandoned.load(Ordering::Acquire) {
                    invocation.reply.take();
                    invocation.ticket_outstanding = false;
                }
                if invocation.reply.is_some() || invocation.ticket_outstanding {
                    let _ = reply.send(Err(protocol("a readiness waiter is already registered")));
                    return;
                }
                invocation.reply = Some(reply);
                invocation.abandoned = abandoned;
                self.settle(&scope.invocation_id);
            }
            Command::CommitSettlement {
                scope,
                revision,
                effects_revision,
                nonce,
                reply,
            } => {
                if self
                    .committed_tickets
                    .get(&(scope.invocation_id.clone(), nonce))
                    == Some(&scope)
                {
                    let _ = reply.send(Ok(()));
                    return;
                }
                let Some(invocation) = self
                    .invocations
                    .get_mut(&scope.invocation_id)
                    .filter(|invocation| invocation.scope == scope)
                else {
                    let _ = reply.send(Err(SettlementError::Stale));
                    return;
                };
                if invocation.ticket_nonce != nonce || !invocation.ticket_outstanding {
                    let _ = reply.send(Err(SettlementError::Stale));
                    return;
                }
                if invocation.revision != revision
                    || invocation.pending_controls != 0
                    || invocation.terminal.is_none()
                    || !matches!(
                        self.state.readiness(&scope),
                        Ok(ExecutionReadiness::Complete)
                    )
                {
                    invocation.ticket_outstanding = false;
                    let _ = reply.send(Err(SettlementError::Stale));
                    return;
                }
                match invocation
                    .completion
                    .commit_provider_execution(effects_revision)
                {
                    Ok(true) => {
                        self.attempts.remove(&scope);
                        self.committed_tickets
                            .insert((scope.invocation_id.clone(), nonce), scope.clone());
                        if let Some(mut invocation) = self.invocations.remove(&scope.invocation_id)
                        {
                            invocation.ticket_outstanding = false;
                            self.committed_invocations
                                .insert(scope.invocation_id.clone(), invocation);
                        }
                        let _ = reply.send(Ok(()));
                    }
                    Ok(false) => {
                        invocation.ticket_outstanding = false;
                        let _ = reply.send(Err(SettlementError::Stale));
                    }
                    Err(error) => {
                        let message = error.to_string();
                        let _ = reply.send(Err(SettlementError::Provider(error)));
                        self.fail_all(&message);
                    }
                }
            }
        }
    }

    /// Reduce delivery independently, retaining a terminal candidate until all controls settle.
    fn acceptance(&mut self, outcome: SubmissionOutcome) {
        let SubmissionOutcome {
            scope,
            submission,
            control_reply,
            result,
        } = outcome;
        let Some(invocation) = self.invocations.get_mut(&scope.invocation_id) else {
            if control_reply.is_some()
                && let Some(invocation) = self.committed_invocations.get_mut(&scope.invocation_id)
            {
                let Some(pending) = invocation.pending_controls.checked_sub(1) else {
                    self.fail_all("idle control accounting underflow");
                    return;
                };
                invocation.pending_controls = pending;
            }
            let delivery = match &result {
                Ok(()) => DeliveryState::Accepted,
                Err(ProviderError::NotConsumed | ProviderError::PreparationFailed(_)) => {
                    DeliveryState::NotConsumed
                }
                Err(_) => DeliveryState::Uncertain,
            };
            if let Some(submission) = submission
                && let Err(error) = self.state.delivery(&scope, &submission, delivery)
            {
                self.fail_all(&error.to_string());
            }
            if delivery == DeliveryState::Uncertain {
                self.fail_observation(ObservationFailure::Disconnected);
            }
            if let Some(reply) = control_reply {
                let _ = reply.send(result);
            }
            return;
        };
        let is_control = control_reply.is_some();
        if is_control {
            invocation.pending_controls -= 1;
        }
        let delivery = match &result {
            Ok(()) => DeliveryState::Accepted,
            Err(ProviderError::NotConsumed | ProviderError::PreparationFailed(_)) => {
                DeliveryState::NotConsumed
            }
            Err(_) => DeliveryState::Uncertain,
        };
        if let Some(submission) = submission
            && let Err(error) = self.state.delivery(&scope, &submission, delivery)
        {
            invocation.terminal = Some(Err(protocol(&error.to_string())));
        }
        if let Some(reply) = control_reply {
            if result.is_err() && delivery != DeliveryState::NotConsumed {
                invocation.terminal = Some(Err(ProviderError::DeliveryUncertain));
            }
            let _ = reply.send(result);
        } else if let Err(error) = result {
            invocation.terminal = Some(Err(error));
        }
        self.settle(&scope.invocation_id);
    }

    fn event(&mut self, event: ProviderEvent) {
        if event.scope().owner() != &self.owner {
            self.fail_all("foreign owner in provider stream");
            return;
        }
        if let ProviderEvent::SettlementFailed { scope, failure } = &event {
            if !self.state.contains(scope) {
                self.fail_observation(ObservationFailure::OwnershipConflict);
                return;
            }
            self.fail_observation_with_origin(
                ObservationFailure::LifecycleUnavailable,
                FailureOrigin::Error(ProviderError::Failure(*failure)),
            );
            return;
        }
        let Some(scope) = event.scope().invocation().cloned() else {
            self.idle_event(event);
            return;
        };
        if let ProviderEvent::Settled { settlement } = &event {
            let Some(invocation) = self
                .invocations
                .get_mut(&scope.invocation_id)
                .filter(|invocation| invocation.scope == scope)
            else {
                if self
                    .committed_invocations
                    .contains_key(&scope.invocation_id)
                {
                    return;
                }
                self.fail_observation(ObservationFailure::OwnershipConflict);
                return;
            };
            if invocation
                .native_settlement
                .as_ref()
                .is_some_and(|previous| {
                    previous.binding_id != settlement.binding_id
                        || previous.result != settlement.result
                })
            {
                self.fail_observation(ObservationFailure::OwnershipConflict);
                return;
            }
            if let Some(Ok(terminal)) = &invocation.terminal
                && terminal != &settlement.result
            {
                self.fail_observation(ObservationFailure::Protocol);
                return;
            }
            invocation.native_settlement = Some(settlement.as_ref().clone());
            if matches!(invocation.host_collection, HostCollection::Pending) {
                invocation.host_collection = HostCollection::Collecting;
                let operation_id =
                    forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string())
                        .expect("generated host collection identity is nonempty");
                self.trace.record(self.owner.session_id.as_str(), "provider.operation.started", serde_json::json!({
                    "operation_id":operation_id, "operation":forge_provider_api::ProviderOperation::InputSubmit,
                    "phase":forge_provider_api::ProviderPhase::Collection, "scope":scope,
                }));
                let provider = self.provider.clone();
                let commands = self.commands.clone();
                self.deliveries.spawn(async move {
                    let result = provider.collect_host_effects(&scope).await;
                    let _ = commands
                        .send(Command::HostEffectsCollected {
                            scope,
                            operation_id,
                            result,
                        })
                        .await;
                    Ok(())
                });
            }
            return;
        }
        if !self.invocations.contains_key(&scope.invocation_id)
            && matches!(&event, ProviderEvent::Execution { observation } if matches!(observation.update(), ExecutionUpdate::Started { .. }))
            && let Some(invocation) = self.committed_invocations.remove(&scope.invocation_id)
        {
            self.idle_event(event);
            self.invocations
                .insert(invocation.scope.invocation_id.clone(), invocation);
            self.fail_observation(ObservationFailure::OwnershipConflict);
            return;
        }
        if !self.invocations.contains_key(&scope.invocation_id) {
            if let ProviderEvent::Execution { observation } = &event {
                if !self.state.contains(&scope) {
                    self.fail_all("unregistered execution in provider stream");
                    return;
                }
                if matches!(observation.update(), ExecutionUpdate::Started { .. }) {
                    self.fail_all("late execution has no retained host settlement authority");
                    return;
                }
                if matches!(observation.update(), ExecutionUpdate::Started { .. })
                    && !self.invocations.is_empty()
                {
                    self.fail_all("late execution conflicts with another admitted exchange");
                    return;
                }
                if self.state.observe(observation.clone()).is_err() {
                    self.fail_all("conflicting late execution observation");
                    return;
                }
                if let ExecutionUpdate::ObservationLost { failure, .. } = observation.update() {
                    let failure = failure.clone();
                    self.idle_event(event);
                    self.fail_observation(failure);
                    return;
                }
            }
            self.idle_event(event);
            return;
        }
        let invocation = self
            .invocations
            .get_mut(&scope.invocation_id)
            .expect("active invocation was checked");
        if let ProviderEvent::Execution { observation } = &event {
            if matches!(observation.update(), ExecutionUpdate::Started { .. })
                && invocation.native_settlement.is_some()
            {
                self.fail_observation(ObservationFailure::OwnershipConflict);
                return;
            }
            let Some(revision) = invocation.revision.checked_add(1) else {
                self.fail_all("settlement revision exhausted");
                return;
            };
            invocation.revision = revision;
            if let Err(error) = self.state.observe(observation.clone()) {
                invocation.terminal = Some(Err(protocol(&error.to_string())));
            } else {
                invocation.terminal = match observation.update() {
                    ExecutionUpdate::Started { .. } => {
                        invocation.native_settlement = None;
                        invocation.host_collection = HostCollection::Pending;
                        None
                    }
                    ExecutionUpdate::Finished { result, .. }
                    | ExecutionUpdate::ResolvedWithoutExecution { result, .. } => {
                        Some(Ok(result.clone()))
                    }
                    ExecutionUpdate::PublicationFailed {
                        failure, result, ..
                    }
                    | ExecutionUpdate::ResolutionPublicationFailed {
                        failure, result, ..
                    } => {
                        let kind = match failure {
                            forge_provider_api::HostEffectFailure::Rejected
                            | forge_provider_api::HostEffectFailure::Retired => {
                                forge_provider_api::ProviderFailureKind::Rejected
                            }
                            forge_provider_api::HostEffectFailure::Uncertain => {
                                forge_provider_api::ProviderFailureKind::Persistence
                            }
                        };
                        invocation.publication_failure = Some(ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::InputSubmit,
                                kind,
                            ),
                        ));
                        Some(Ok(result.clone()))
                    }
                    ExecutionUpdate::ObservationLost { failure, .. } => {
                        Some(Err(ProviderError::Observation(failure.clone())))
                    }
                };
            }
        }
        if invocation.idle_observation {
            self.idle_event(event);
            self.settle(&scope.invocation_id);
            return;
        }
        let output = invocation.output.clone();
        if !self.enqueue(Delivery::Event {
            sink: output,
            event: Box::new(event),
            fallback: Some(self.idle.clone()),
        }) {
            self.fail_observation(ObservationFailure::Disconnected);
            return;
        }
        self.settle(&scope.invocation_id);
    }

    /// Retain initialization and late output under the owner-wide observer.
    fn idle_event(&mut self, event: ProviderEvent) {
        match self.idle.clone() {
            SessionObserver::Inbox(inbox) => {
                if !self.enqueue(Delivery::Inbox { inbox, event }) {
                    self.fail_observation(ObservationFailure::Disconnected);
                }
            }
            SessionObserver::Stream(idle) => {
                let sink = match idle.bind_scope(event.scope()) {
                    Ok(sink) => sink,
                    Err(error) => {
                        self.fail_all(&error.to_string());
                        return;
                    }
                };
                if !self.enqueue(Delivery::Event {
                    sink,
                    event: Box::new(event),
                    fallback: None,
                }) {
                    self.fail_observation(ObservationFailure::Disconnected);
                }
            }
        }
    }

    /// Retain ordered publication while selecting capacity alongside owner control commands.
    fn enqueue(&mut self, delivery: Delivery) -> bool {
        if matches!(&delivery, Delivery::Complete { reply, .. } if reply.is_closed()) {
            return true;
        }
        let Some(sender) = self.delivery.as_ref() else {
            delivery.fail(ObservationFailure::Disconnected);
            return false;
        };
        let delivery = if self.pending_delivery.is_empty() {
            match sender.try_send(delivery) {
                Ok(()) => return true,
                Err(mpsc::error::TrySendError::Full(delivery)) => delivery,
                Err(mpsc::error::TrySendError::Closed(delivery)) => {
                    delivery.fail(ObservationFailure::Disconnected);
                    return false;
                }
            }
        } else {
            delivery
        };
        if self.pending_delivery.len() < forge_protocol::MAX_PENDING_FRAMES {
            self.pending_delivery.push_back(delivery);
            #[cfg(test)]
            self.delivery_pressure.send_replace(true);
            return true;
        }
        if !self.queue_loss_reported.swap(true, Ordering::AcqRel) {
            let (stage, source_scope, event_bytes) = match &delivery {
                Delivery::Inbox { event, .. } => (
                    "engine_idle",
                    Some(event.scope()),
                    serde_json::to_vec(event).ok().map(|encoded| encoded.len()),
                ),
                Delivery::Event { event, .. } => (
                    "engine_foreground",
                    Some(event.scope()),
                    serde_json::to_vec(event).ok().map(|encoded| encoded.len()),
                ),
                Delivery::Complete { result, .. } => (
                    "engine_completion",
                    result
                        .as_ref()
                        .as_ref()
                        .ok()
                        .map(|ready| ready.ticket.scope.clone().into()),
                    None,
                ),
            };
            self.trace.record(self.owner.session_id.as_str(), "provider.observation_queue_lost", serde_json::json!({
                "provider":self.provider.descriptor().kind, "stage":stage,
                "queued":self.pending_delivery.len(), "capacity":forge_protocol::MAX_PENDING_FRAMES,
                "queued_bytes":null, "event_bytes":event_bytes, "source_scope":source_scope, "owner":self.owner,
            }));
        }
        delivery.fail(ObservationFailure::Overflow);
        false
    }

    fn settle(&mut self, id: &InvocationId) {
        let Some(invocation) = self.invocations.get(id) else {
            return;
        };
        if invocation.terminal.is_none() || invocation.pending_controls > 0 {
            return;
        }
        let ready = self.state.readiness(&invocation.scope);
        if matches!(ready, Ok(ExecutionReadiness::Waiting)) {
            return;
        }
        let origin = match &ready {
            Ok(ExecutionReadiness::DeliveryUncertain) => {
                if let Some(Err(error @ ProviderError::PreparationCleanup { .. })) =
                    &invocation.terminal
                {
                    FailureOrigin::Error(error.clone())
                } else {
                    FailureOrigin::Error(ProviderError::DeliveryUncertain)
                }
            }
            Ok(ExecutionReadiness::ObservationLost(ObservationFailure::DeliveryUncertain)) => {
                FailureOrigin::Error(ProviderError::DeliveryUncertain)
            }
            _ => FailureOrigin::Observation,
        };
        let failure = match ready {
            Ok(ExecutionReadiness::Waiting) => return,
            Ok(ExecutionReadiness::Complete) => None,
            Ok(ExecutionReadiness::ObservationLost(failure)) => Some(failure),
            Ok(ExecutionReadiness::DeliveryUncertain) => {
                Some(ObservationFailure::DeliveryUncertain)
            }
            Err(_) => Some(ObservationFailure::Protocol),
        };
        if let Some(failure) = failure {
            self.fail_observation_with_origin(failure, origin);
            return;
        }
        if matches!(invocation.terminal, Some(Ok(_)))
            && (invocation.native_settlement.is_none()
                || !matches!(invocation.host_collection, HostCollection::Settled))
        {
            return;
        }
        if self.invocations.get(id).is_some_and(|invocation| {
            matches!(
                &invocation.terminal,
                Some(Err(ProviderError::PreparationFailed(_)
                    | ProviderError::NotConsumed
                    | ProviderError::Cancelled))
            )
        }) {
            let mut invocation = self
                .invocations
                .remove(id)
                .expect("invocation checked above");
            self.attempts.remove(&invocation.scope);
            if let (Some(reply), Some(Err(error))) =
                (invocation.reply.take(), invocation.terminal.take())
                && !self.enqueue(Delivery::Complete {
                    reply,
                    result: Box::new(Err(error)),
                })
            {
                self.fail_all("execution result delivery closed");
            }
            return;
        }
        let invocation = self
            .invocations
            .get_mut(id)
            .expect("invocation checked above");
        let Some(reply) = invocation.reply.take() else {
            return;
        };
        let Some(Ok(terminal)) = invocation.terminal.as_ref() else {
            let _ = reply.send(Err(protocol("invalid terminal observation")));
            self.fail_all("invalid terminal observation");
            return;
        };
        let crate::tools::ProviderExecutionPreview {
            revision: effects_revision,
            mut outcome,
            batches,
        } = match invocation.completion.preview_provider_execution(terminal) {
            Ok(preview) => preview,
            Err(error) => {
                let message = error.to_string();
                let _ = reply.send(Err(error));
                self.fail_all(&message);
                return;
            }
        };
        if let Some(failure) = &invocation.publication_failure {
            outcome = Err(failure.clone());
        }
        let Some(nonce) = self.next_ticket_nonce.checked_add(1) else {
            let _ = reply.send(Err(protocol("settlement ticket identity exhausted")));
            self.fail_all("settlement ticket identity exhausted");
            return;
        };
        self.next_ticket_nonce = nonce;
        invocation.ticket_nonce = nonce;
        let result = Ok(NativeReady {
            outcome,
            batches,
            ticket: SettlementTicket {
                scope: invocation.scope.clone(),
                revision: invocation.revision,
                effects_revision,
                nonce,
                commands: self.commands.clone(),
                abandoned: invocation.abandoned.clone(),
            },
        });
        invocation.ticket_outstanding = true;
        if !self.enqueue(Delivery::Complete {
            reply,
            result: Box::new(result),
        }) {
            self.fail_all("execution result delivery closed");
        }
    }

    /// Resolve failed execution waiters only after cancellation and collection have completed.
    fn cleanup_finished(
        &mut self,
        result: Option<
            Result<forge_provider_api::error::ObservationCleanup, tokio::task::JoinError>,
        >,
    ) {
        let report = match result {
            Some(Ok(report)) => Arc::new(match &self.first_failure {
                Some(failure) => report.with_observation(failure.clone()),
                None => report,
            }),
            Some(Err(error)) => {
                let report = forge_provider_api::error::ObservationCleanup::new(
                    self.first_failure
                        .clone()
                        .unwrap_or(ObservationFailure::Disconnected),
                    None,
                    None,
                    Some(forge_provider_api::error::CleanupFailure::Failed(Box::new(
                        crate::provider::lifecycle_error::classify_source(
                            forge_provider_api::error::ProviderOperation::SessionClose,
                            &error,
                        ),
                    ))),
                );
                let origin = self
                    .failed_waiters
                    .iter()
                    .find_map(|waiter| waiter.original.clone());
                Arc::new(match origin {
                    Some(origin) => report.with_origin(origin),
                    None => report,
                })
            }
            None => return,
        };
        if !report.collected() {
            if self.retiring.is_some() && !self.failed {
                self.failed = true;
                self.first_failure
                    .get_or_insert(ObservationFailure::LifecycleUnavailable);
                match &self.idle {
                    SessionObserver::Inbox(inbox) => inbox.fail(
                        self.first_failure
                            .clone()
                            .expect("failed retirement records its cause"),
                    ),
                    SessionObserver::Stream(sink) => {
                        sink.fail(crate::provider::events::EventDeliveryError::Observation(
                            self.first_failure
                                .clone()
                                .expect("failed retirement records its cause"),
                        ))
                    }
                }
            }
            self.cleanup_failure = Some(report.clone());
            self.idle.diagnostic(ProviderEvent::Diagnostic {
                scope: crate::provider::EventScope::Binding {
                    owner: self.owner.clone(),
                },
                diagnostic: crate::provider::ProviderDiagnostic {
                    kind: forge_provider_api::ProviderFailureKind::Runtime,
                    message: forge_provider_api::SanitizedMessage::new(
                        "Provider observation cleanup failed",
                    )
                    .expect("static diagnostic is nonempty"),
                    fatal: true,
                },
            });
        }
        for FailedWaiter { reply, original } in std::mem::take(&mut self.failed_waiters) {
            let error = if report.collected() {
                original.unwrap_or_else(|| ProviderError::Observation(report.observation().clone()))
            } else {
                ProviderError::Cleanup(report.clone())
            };
            let _ = reply.send(Err(error));
        }
    }

    fn fail_all(&mut self, _message: &str) {
        self.fail_observation(ObservationFailure::Protocol);
    }

    fn fail_observation(&mut self, failure: ObservationFailure) {
        let origin = match &failure {
            ObservationFailure::DeliveryUncertain => {
                FailureOrigin::Error(ProviderError::DeliveryUncertain)
            }
            _ => FailureOrigin::Observation,
        };
        self.fail_observation_with_origin(failure, origin);
    }

    fn fail_observation_with_origin(&mut self, failure: ObservationFailure, origin: FailureOrigin) {
        if self.failed {
            return;
        }
        self.healthy.store(false, Ordering::Release);
        self.failed = true;
        self.first_failure = Some(failure.clone());
        let provider = self.provider.clone();
        let owner = self.owner.clone();
        let observation = failure.clone();
        let cleanup_origin = match &origin {
            FailureOrigin::Observation => None,
            FailureOrigin::Error(error) => Some(error.clone()),
        };
        self.cleanup.spawn(async move {
            let report = collect_failed_execution(provider.as_ref(), &owner, observation).await;
            match cleanup_origin {
                Some(origin) => report.with_origin(origin),
                None => report,
            }
        });
        match &self.idle {
            SessionObserver::Inbox(inbox) => inbox.fail(failure.clone()),
            SessionObserver::Stream(sink) => sink.fail(
                crate::provider::events::EventDeliveryError::Observation(failure.clone()),
            ),
        }
        // Retain execution identity until close has consumed its terminal events.
        // Removing it here would reject settlement and strand the close watermark.
        let waiters: Vec<_> = self
            .invocations
            .values_mut()
            .map(|invocation| {
                (
                    invocation.scope.clone(),
                    invocation.output.clone(),
                    invocation.reply.take(),
                )
            })
            .collect();
        for (scope, output, reply) in waiters {
            if let Ok(observation) = forge_provider_api::lifecycle::ExecutionObservation::new(
                scope,
                ExecutionUpdate::ObservationLost {
                    failure: failure.clone(),
                    completion: None,
                },
            ) {
                self.enqueue(Delivery::Event {
                    sink: output,
                    event: Box::new(ProviderEvent::Execution { observation }),
                    fallback: Some(self.idle.clone()),
                });
            }
            if let Some(reply) = reply {
                let original = match &origin {
                    FailureOrigin::Observation => None,
                    FailureOrigin::Error(error) => Some(error.clone()),
                };
                self.failed_waiters.push(FailedWaiter { reply, original });
            }
        }
    }
}

/// Retry cleanup without admitting new work or replaying the failed invocation.
async fn collect_failed_execution(
    provider: &Provider,
    owner: &ExecutionOwner,
    failure: ObservationFailure,
) -> forge_provider_api::error::ObservationCleanup {
    use forge_provider_api::error::{CleanupFailure, ObservationCleanup};

    let collection = provider.session_close_owner(owner).await;
    let classify = |result: Result<(), ProviderError>| match result {
        Ok(()) => None,
        Err(error) => Some(CleanupFailure::Failed(Box::new(error))),
    };
    ObservationCleanup::new(failure, None, None, classify(collection))
}

impl Delivery {
    /// Resolve a rejected delivery without stranding the associated completion waiter.
    fn fail(self, failure: ObservationFailure) {
        match self {
            Self::Inbox { inbox, .. } => inbox.fail(failure),
            Self::Event { sink, .. } => sink.fail(
                crate::provider::events::EventDeliveryError::Observation(failure.clone()),
            ),
            Self::Complete { reply, .. } => {
                let _ = reply.send(Err(ProviderError::Observation(failure)));
            }
        }
    }
}

/// Preserve provider event order across foreground and idle presentation sinks.
async fn deliver(mut pending: mpsc::Receiver<Delivery>) -> Result<(), ProviderError> {
    while let Some(delivery) = pending.recv().await {
        let result = match delivery {
            Delivery::Inbox { inbox, event } => {
                let result = inbox.push(event);
                if let Err(ProviderError::Observation(failure)) = &result {
                    inbox.fail(failure.clone());
                }
                result
            }
            Delivery::Event {
                sink,
                event,
                fallback,
            } => {
                let event = *event;
                match fallback {
                    Some(observer) => match sink.send_wait(event.clone()).await {
                        Ok(()) => Ok(()),
                        Err(crate::provider::events::EventDeliveryError::Closed) => {
                            observer.retain(event).await
                        }
                        Err(error) => Err(error.into()),
                    },
                    None => sink.send_wait(event).await.map_err(ProviderError::from),
                }
            }
            Delivery::Complete { reply, result } => {
                let _ = reply.send(*result);
                Ok(())
            }
        };
        if let Err(error) = result {
            let failure = match &error {
                ProviderError::Observation(failure) => failure.clone(),
                _ => ObservationFailure::Disconnected,
            };
            pending.close();
            while let Some(delivery) = pending.recv().await {
                delivery.fail(failure.clone());
            }
            return Err(error);
        }
    }
    Ok(())
}

/// Classify observation logs without recording prompt text, tool payloads, or native details.
fn event_category(event: &ProviderEvent) -> &'static str {
    match event {
        ProviderEvent::Settled { .. } => "settled",
        ProviderEvent::SettlementFailed { .. } => "settlement_failed",
        ProviderEvent::CapabilitiesChanged { .. } => "capabilities_changed",
        ProviderEvent::SessionIdentity { .. } => "session_identity",
        ProviderEvent::Terminals { .. } => "terminals",
        ProviderEvent::Execution { .. } => "execution",
        ProviderEvent::Message { .. } => "message",
        ProviderEvent::Tool { .. } => "tool",
        ProviderEvent::Turn { .. } => "turn",
        ProviderEvent::Usage { .. } => "usage",
        ProviderEvent::Runtime { .. } => "runtime",
        ProviderEvent::Compaction { .. } => "compaction",
        ProviderEvent::Agent { .. } => "agent",
        ProviderEvent::Checklist { .. } => "checklist",
        ProviderEvent::Approval { .. } => "approval",
        ProviderEvent::DirectoryAccess { .. } => "directory_access",
        ProviderEvent::InputAccepted { .. } => "input_accepted",
        ProviderEvent::Retry { .. } => "retry",
        ProviderEvent::Diagnostic { .. } => "diagnostic",
        ProviderEvent::ParentBoundary { .. } => "parent_boundary",
    }
}

fn protocol(_message: &str) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::InputSubmit,
        forge_provider_api::ProviderFailureKind::Protocol,
    ))
}

fn not_admitted() -> ProviderError {
    ProviderError::PreparationFailed(Box::new(ProviderError::Failure(
        forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::InputSteer,
            forge_provider_api::ProviderFailureKind::Rejected,
        ),
    )))
}

#[cfg(test)]
#[path = "owner_conformance.rs"]
mod owner_conformance;

#[cfg(test)]
mod tests {
    use super::*;

    /// A failed required consumption record cannot publish an event or advance its native watermark.
    #[tokio::test]
    async fn core_persistence_failure_prevents_native_event_forwarding() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        drop(crate::storage::SqliteStore::open(directory.path())?);
        let database = rusqlite::Connection::open(directory.path().join("harness.sqlite3"))?;
        database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('session','workspace',1,'{}')", [])?;
        database.execute_batch("CREATE TRIGGER reject_consumption BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='provider_event_consumed' BEGIN SELECT RAISE(ABORT,'fixture consumption evidence failure'); END;")?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        trace.configure(false)?;
        let provider = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id().unwrap(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("session");
        let (delivery, mut pending) = mpsc::channel(8);
        let (commands, _receive) = mpsc::channel(1);
        let mut driver = Driver {
            owner: owner.clone(),
            provider: provider.clone(),
            trace,
            queue_loss_reported: AtomicBool::new(false),
            state: ExecutionState::new(owner.clone()),
            invocations: HashMap::new(),
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 1,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Default::default(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: HashMap::new(),
            prevented_invocations: HashMap::new(),
            reservations: HashMap::new(),
            latest_reservations: HashMap::new(),
            submission_exits: Default::default(),
        };
        driver
            .native_event(crate::provider::bridge::HostEvent {
                event: ProviderEvent::Diagnostic {
                    scope: forge_provider_api::EventScope::Binding { owner },
                    diagnostic: forge_provider_api::ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new("not published")?,
                        fatal: false,
                    },
                },
                native: Some((
                    forge_provider_api::BindingId::new("binding")?,
                    forge_provider_api::EventSequence::new(1),
                )),
            })
            .await;
        assert!(driver.failed);
        assert!(matches!(
            pending.try_recv(),
            Err(mpsc::error::TryRecvError::Empty)
        ));
        assert!(driver.pending_delivery.is_empty());
        assert_eq!(database.query_row("SELECT COUNT(*) FROM owner_core_log WHERE json_extract(record,'$.record')='provider_event_consumed'", [], |row| row.get::<_, i64>(0))?, 0);
        let cleanup = driver.cleanup.join_next().await;
        driver.cleanup_finished(cleanup);
        provider.shutdown().await?;
        Ok(())
    }
    use crate::provider::mock_response::MockResponseCommand;
    use crate::provider::{PromptMode, ProviderInput, ProviderLaunch};
    use anyhow::Context;
    use forge_provider_api::lifecycle::ExecutionObservation;

    /// Dropping a cancellable checkout restores the sole ticket to its host owner.
    #[test]
    fn settlement_checkout_restores_ticket_on_drop() -> anyhow::Result<()> {
        let cell = Arc::new(SettlementCell::default());
        let (commands, _receive) = mpsc::channel(1);
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
        {
            let mut checkout = cell.checkout()?;
            assert!(cell.checkout().is_err());
            checkout.replace(SettlementTicket {
                scope: scope.clone(),
                revision: 1,
                effects_revision: 2,
                nonce: 3,
                commands,
                abandoned: Arc::new(AtomicBool::new(false)),
            });
        }
        assert!(!cell.is_empty()?);
        let checkout = cell.checkout()?;
        assert_eq!(checkout.ticket().unwrap().scope(), &scope);
        drop(checkout);
        assert!(!cell.is_empty()?);
        Ok(())
    }

    /// Receipts prevent repeated edits, reject changed documents, and distinguish validation failure.
    #[test]
    fn plan_effect_receipts_fence_replay_and_ambiguous_mutation() -> anyhow::Result<()> {
        let cell = Arc::new(SettlementCell::default());
        let first = HostEffectId {
            batch_revision: 1,
            kind: HostEffectKind::PlanEdit,
            ordinal: 0,
        };
        let HostEffectAdmission::Apply(guard) =
            cell.begin_plan_effect(first, "plan", 1, "before")?
        else {
            panic!("first effect must be admitted")
        };
        guard.cancel_definite()?;
        let HostEffectAdmission::Apply(guard) =
            cell.begin_plan_effect(first, "plan", 1, "before")?
        else {
            panic!("validation failure must be retryable")
        };
        guard.commit("plan".into(), 2, "after".into(), None)?;
        assert!(matches!(
            cell.begin_plan_effect(first, "plan", 2, "after")?,
            HostEffectAdmission::Applied(_)
        ));
        assert!(cell.begin_plan_effect(first, "plan", 3, "foreign").is_err());
        let second = HostEffectId {
            batch_revision: 2,
            kind: HostEffectKind::DesignPatch,
            ordinal: 0,
        };
        let HostEffectAdmission::Apply(unstarted) =
            cell.begin_plan_effect(second, "plan", 2, "after")?
        else {
            panic!("second effect must be admitted")
        };
        drop(unstarted);
        let HostEffectAdmission::Apply(mut guard) =
            cell.begin_plan_effect(second, "plan", 2, "after")?
        else {
            panic!("second effect must be admitted")
        };
        guard.begin_mutation();
        drop(guard);
        assert!(cell.begin_plan_effect(second, "plan", 2, "after").is_err());
        Ok(())
    }

    /// Retain an effect's exact events until publication has drained and been acknowledged.
    #[test]
    fn result_effect_receipt_retains_presentation_until_acknowledged() -> anyhow::Result<()> {
        let cell = Arc::new(SettlementCell::default());
        let id = HostEffectId {
            batch_revision: 1,
            kind: HostEffectKind::QuestionAnswer,
            ordinal: 0,
        };
        let ResultEffectAdmission::Apply(guard) = cell.begin_result_effect(id)? else {
            panic!("new effect must be admitted")
        };
        guard.commit_output(HostEffectOutput {
            nodes: Vec::new(),
            events: vec![forge_protocol::message::SessionEvent {
                session_id: "session".into(),
                event: "question".into(),
                payload: serde_json::json!({"value":1}),
            }],
            elicitation: None,
            awaiting_input: false,
            publication_drained: false,
            plan_document: None,
            continuing: None,
            plan_record: None,
        })?;
        cell.seal_effects(&[id])?;
        assert!(!cell.effect_published(id)?);
        assert!(cell.validate_effects_settled().is_err());
        let ResultEffectAdmission::Applied(retained) = cell.begin_result_effect(id)? else {
            panic!("completed effect must retain its result")
        };
        assert_eq!(retained.events[0].payload["value"], 1);
        assert_eq!(cell.pending_effect_publications()?, vec![id]);
        cell.acknowledge_effect_publication(id)?;
        assert!(cell.effect_published(id)?);
        assert!(cell.pending_effect_publications()?.is_empty());
        cell.validate_effects_settled()?;
        Ok(())
    }

    /// Preserve actual mutation order when batch revisions and effect ordinals overlap.
    #[test]
    fn effect_publications_preserve_application_order() -> anyhow::Result<()> {
        let cell = Arc::new(SettlementCell::default());
        let ids = [
            HostEffectId {
                batch_revision: 2,
                kind: HostEffectKind::QuestionWithdrawal,
                ordinal: 0,
            },
            HostEffectId {
                batch_revision: 1,
                kind: HostEffectKind::QuestionAnswer,
                ordinal: 0,
            },
            HostEffectId {
                batch_revision: 2,
                kind: HostEffectKind::PlanQuestion,
                ordinal: 0,
            },
        ];
        for id in ids {
            let ResultEffectAdmission::Apply(guard) = cell.begin_result_effect(id)? else {
                panic!("new effect must be admitted")
            };
            guard.commit_output(HostEffectOutput {
                nodes: Vec::new(),
                events: Vec::new(),
                elicitation: None,
                awaiting_input: false,
                publication_drained: false,
                plan_document: None,
                continuing: None,
                plan_record: None,
            })?;
        }
        assert_eq!(cell.pending_effect_publications()?, ids);
        assert!(matches!(
            cell.begin_result_effect(ids[1])?,
            ResultEffectAdmission::Applied(_)
        ));
        assert_eq!(cell.pending_effect_publications()?, ids);
        cell.acknowledge_effect_publication(ids[1])?;
        assert_eq!(cell.pending_effect_publications()?, vec![ids[0], ids[2]]);
        Ok(())
    }

    /// A later published host-effect batch invalidates an earlier complete-effect seal.
    #[test]
    fn new_host_batch_invalidates_effect_seal() -> anyhow::Result<()> {
        let cell = SettlementCell::default();
        cell.seal_effects(&[])?;
        cell.validate_effects_settled()?;
        cell.set_batches(vec![crate::provider::HostEffectBatch::new(
            1,
            ProviderCompletion::default(),
        )])?;
        assert!(cell.validate_effects_settled().is_err());
        Ok(())
    }

    /// Retry a failed collection before releasing the old execution owner.
    #[tokio::test]
    async fn close_retries_incomplete_cleanup_before_releasing_owner() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let report = Arc::new(forge_provider_api::error::ObservationCleanup::new(
            ObservationFailure::Disconnected,
            None,
            None,
            Some(forge_provider_api::error::CleanupFailure::TimedOut),
        ));
        let expected = report.clone();
        let task = tokio::spawn(async move { Err(ProviderError::Cleanup(report)) });
        let (commands, _receive) = mpsc::channel(1);
        let engine = SessionExecution {
            owner: ExecutionOwner::root("failed-close"),
            provider,
            commands,
            healthy: Arc::new(AtomicBool::new(true)),
            orderly_shutdown: Arc::new(AtomicBool::new(false)),
            task: tokio::sync::Mutex::new(ExecutionTask::Running(task)),
            activity: Default::default(),
            admitted: Default::default(),
            delivery_pressure: tokio::sync::watch::channel(false).1,
            presentation_queue: mpsc::channel(1).0.downgrade(),
        };
        let Err(ProviderError::Cleanup(actual)) = engine.close().await else {
            panic!("incomplete cleanup must remain a failure");
        };
        assert!(Arc::ptr_eq(&actual, &expected));
        assert!(!actual.collected());
        engine.close().await?;
        assert!(matches!(*engine.task.lock().await, ExecutionTask::Closed));
        engine.close().await?;
        Ok(())
    }

    /// Retain host ownership, reject late steering, and admit ordinary successor input with fresh identity.
    #[tokio::test]
    async fn host_ticket_blocks_admission_and_rolls_ordinary_successor_scope() -> anyhow::Result<()>
    {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let store =
            crate::provider::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let owner = ExecutionOwner::root("host-ticket");
        let registry = SessionExecutionRegistry::new(provider.clone(), trace.clone());
        let engine = registry.owner(owner.clone()).await?;
        let mut request = ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: Some(owner.clone()),
            harness_session_id: "host-ticket".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("hello"),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let scope = InvocationScope::new(owner.clone(), "original");
        let (sink, events) = crate::provider::events::channel(scope.clone());
        drop(events);
        let ready = engine
            .run_until_ready(request.clone(), sink, &store)
            .await
            .context("first execution readiness")?;
        assert!(ready.outcome.is_ok());
        assert!(engine.is_healthy().await);
        assert!(!registry.observer(owner.clone())?.drain()?.is_empty());
        let native_revision = ready.ticket.revision;
        let effects_revision = ready.ticket.effects_revision;
        let previous_nonce = ready.ticket.nonce;
        drop(ready);
        let mut cancelled_waiter = Box::pin(engine.wait_ready(scope.clone()));
        assert!(futures_util::poll!(cancelled_waiter.as_mut()).is_pending());
        drop(cancelled_waiter);
        let ready = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.wait_ready(scope.clone()),
        )
        .await??;
        assert_eq!(ready.ticket.revision, native_revision);
        assert_eq!(ready.ticket.effects_revision, effects_revision);
        assert!(ready.ticket.nonce > previous_nonce);
        ready.ticket.retire_for_refresh();
        let renewed = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.wait_ready(scope.clone()),
        )
        .await??;
        assert!(matches!(
            ready.ticket.commit().await,
            Err(SettlementError::Stale)
        ));
        drop(ready);
        let ready = renewed;
        let (other, _events) =
            crate::provider::events::channel(InvocationScope::new(owner.clone(), "other"));
        assert!(
            engine
                .run_until_ready(request.clone(), other, &store)
                .await
                .is_err()
        );
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: Some(owner),
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            provider_session_id: None,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
        };
        let receipt = store.prepare_steering(
            &catalog,
            ProviderInput::from_text("continue"),
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
        )?;
        assert!(
            engine
                .steer(
                    scope.clone(),
                    crate::provider::SteeringInput::User {
                        text: "continue".into(),
                        attachments: Vec::new(),
                        submission: Some(receipt),
                    },
                )
                .await
                .is_err(),
            "late steering reopened settled work"
        );
        engine
            .publish_idle_test_effect(scope.clone(), ProviderCompletion::default())
            .await?;
        assert!(matches!(
            ready.ticket.commit().await,
            Err(SettlementError::Stale)
        ));
        request.dispatch = crate::provider::DispatchTracker::default();
        let (continuation_sink, _events) = crate::provider::events::channel(scope.clone());
        let stale = engine
            .continue_attempt(&ready.ticket, request.clone(), continuation_sink, &store)
            .await
            .context("reject stale continuation")?;
        let ContinuationAttempt::Stale(prepared) = stale else {
            panic!("stale continuation must not be admitted");
        };
        let submission = prepared.submission.context("prepared input was lost")?;
        assert!(matches!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        let current = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.wait_ready(scope.clone()),
        )
        .await??;
        assert!(current.outcome.is_ok());
        let (continuation_sink, _events) = crate::provider::events::channel(scope.clone());
        let next = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.continue_attempt(&current.ticket, request, continuation_sink, &store),
        )
        .await
        .context("continue current ticket deadline")?
        .context("continue current ticket")?;
        let ContinuationAttempt::Ready(next) = next else {
            panic!("current continuation must be admitted");
        };
        assert!(next.outcome.is_ok());
        assert_ne!(next.ticket.scope.invocation_id, scope.invocation_id);
        assert_eq!(next.ticket.scope.exchange_id, scope.exchange_id);
        assert!(engine.admitted_invocation(&next.ticket.scope)?);

        assert!(matches!(
            current.ticket.commit().await,
            Err(SettlementError::Stale)
        ));
        next.ticket.commit().await?;
        next.ticket.commit().await?;
        registry.close().await?;
        provider.shutdown().await?;
        Ok(())
    }

    /// Retain positive prevention even when completion removed the attempt before collection replied.
    #[tokio::test]
    async fn prevented_collection_after_attempt_removal_authorizes_exact_replacement()
    -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id().unwrap(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("collected-prevention");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let (commands, _receive) = mpsc::channel(1);
        let collection = ExchangeCollection {
            scope: scope.clone(),
            fence_id: forge_provider_api::OperationId::new("collection")?,
            commands: commands.clone(),
        };
        let mut driver = Driver {
            owner: owner.clone(),
            provider: provider.clone(),
            trace,
            queue_loss_reported: AtomicBool::new(false),
            state: ExecutionState::new(owner),
            invocations: HashMap::new(),
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: None,
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 1,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Arc::new(Mutex::new(std::collections::HashSet::from([scope.clone()]))),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: HashMap::from([(scope.exchange_id.clone(), collection.clone())]),
            prevented_invocations: HashMap::new(),
            reservations: HashMap::new(),
            latest_reservations: HashMap::from([(scope.exchange_id.clone(), scope.clone())]),
            submission_exits: Default::default(),
        };
        let submission_id = SubmissionId::try_from("prevented-input".to_owned())?;
        let attempt_id = forge_provider_api::DeliveryAttemptId::new("prevented-attempt")?;
        let (reply, result) = oneshot::channel();
        driver.command(Command::CancellationFinished {
            scope: scope.clone(),
            outcome: CancellationKind::Prevented(PreventedInput {
                submission_id: submission_id.clone(),
                attempt_id: attempt_id.clone(),
            }),
            result: Ok(()),
            reply: CancellationReply::Exchange {
                collection: collection.clone(),
                reply,
            },
        });
        let collected = result.await??;
        let proof = driver.prevented_invocations.get(&scope).unwrap();
        assert_eq!(proof.submission_id, submission_id);
        assert_eq!(proof.attempt_id, attempt_id);
        let (reply, result) = oneshot::channel();
        driver.command(Command::ResumeExchange {
            collection: collected,
            reply,
        });
        result.await??;
        assert!(!driver.cancelled_exchanges.contains_key(&scope.exchange_id));
        // Consuming collection once must still reject a delayed duplicate resume.
        let (reply, result) = oneshot::channel();
        driver.command(Command::ResumeExchange { collection, reply });
        assert!(matches!(result.await?, Err(ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::WrongInvocation));
        provider.shutdown().await?;
        Ok(())
    }

    /// A native terminal cannot release the owner before provider settlement and host-effect commit.
    #[tokio::test]
    async fn native_terminal_waits_for_settlement_and_host_effect_commit() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("idle-host-effects");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let submission = SubmissionId::try_from("original".to_owned())?;
        let first = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "first".into(),
        };
        let completed = forge_provider_api::lifecycle::ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        };
        let mut state = ExecutionState::new(owner.clone());
        state.admit(scope.clone(), submission.clone())?;
        state.delivery(&scope, &submission, DeliveryState::Accepted)?;
        state.observe(ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Started {
                address: first.clone(),
            },
        )?)?;
        state.observe(ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Finished {
                address: first,
                covered_submissions: vec![submission],
                completed_at_ms: 1,
                result: completed.clone(),
            },
        )?)?;
        let context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
        let slot = context.provider_completion.clone();
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let (reply, response) = oneshot::channel();
        let mut committed = HashMap::new();
        committed.insert(
            scope.invocation_id.clone(),
            Invocation {
                scope: scope.clone(),
                output: sink,
                reply: Some(reply),
                completion: context,
                terminal: Some(Ok(completed.clone())),
                native_settlement: None,
                host_collection: HostCollection::Pending,
                publication_failure: None,
                pending_controls: 0,
                revision: 1,
                ticket_outstanding: false,
                ticket_nonce: 1,
                abandoned: Arc::new(AtomicBool::new(false)),
                idle_observation: false,
            },
        );
        let (delivery, mut deliveries) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (commands, _receive) = mpsc::channel(1);
        let mut driver = Driver {
            trace: Arc::new(crate::trace::TraceStore::open(directory.path())?),
            queue_loss_reported: AtomicBool::new(false),
            owner,
            provider,
            state,
            invocations: committed,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 1,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Default::default(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: Default::default(),
            prevented_invocations: Default::default(),
            reservations: Default::default(),
            latest_reservations: Default::default(),
            submission_exits: Default::default(),
        };
        driver.settle(&scope.invocation_id);
        assert!(
            deliveries.try_recv().is_err(),
            "terminal bypassed provider settlement"
        );
        slot.publish(ProviderCompletion {
            plan_question: Some(crate::plan::PlanQuestionSet {
                id: "idle-question".into(),
                questions: Vec::new(),
            }),
            ..Default::default()
        })?;
        driver.event(ProviderEvent::Settled {
            settlement: Box::new(forge_provider_api::InvocationSettlement {
                binding_id: forge_provider_api::BindingId::new("binding")?,
                scope: scope.clone(),
                result: completed,
                watermark: forge_provider_api::EventSequence::new(2),
            }),
        });
        assert!(
            deliveries.try_recv().is_err(),
            "native settlement bypassed host-owned collection"
        );
        driver.command(Command::HostEffectsCollected {
            scope: scope.clone(),
            operation_id: forge_provider_api::OperationId::new("fixture-collection")?,
            result: Ok(()),
        });
        while let Some(delivery) = deliveries.recv().await {
            if let Delivery::Complete { reply, result } = delivery {
                reply.send(*result).ok();
                break;
            }
        }
        let ready = response.await??;
        assert_eq!(
            ready.batches[0]
                .completion
                .plan_question
                .as_ref()
                .unwrap()
                .id,
            "idle-question"
        );
        assert!(slot.preview()?.completion.is_some());
        let (reply, committed) = oneshot::channel();
        driver.command(Command::CommitSettlement {
            scope: scope.clone(),
            revision: ready.ticket.revision,
            effects_revision: ready.ticket.effects_revision,
            nonce: ready.ticket.nonce,
            reply,
        });
        committed.await??;
        assert!(slot.preview()?.completion.is_none());
        assert!(
            driver
                .committed_invocations
                .contains_key(&scope.invocation_id)
        );
        let (_task_root, mut task_store, controller) =
            crate::task::SessionTaskController::test_goal();
        let task = controller.current()?.context("task fixture missing")?;
        let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
        context.bind_test_task_controller(controller.clone());
        let authority =
            crate::tools::TaskAuthority::capture(&context).context("callback authority missing")?;
        controller.change(
            &mut task_store,
            Some(&task.revision),
            crate::task::TaskChange::Clear,
            3,
        )?;
        let (reply, response) = oneshot::channel();
        driver.command(Command::SteerCurrent {
            input: crate::provider::SteeringInput::AgentResult("stale child message".into()),
            submission: None,
            authority: Some(authority),
            reply,
        });
        let response = response.await?;
        let Err(ProviderError::PreparationFailed(cause)) = response else {
            panic!("collected owner admitted stale child control: {response:?}");
        };
        assert!(matches!(cause.as_ref(), ProviderError::Failure(failure)
            if failure.operation() == forge_provider_api::ProviderOperation::InputSteer
                && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected
                && failure.phase() == forge_provider_api::ProviderPhase::Admission
                && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged));
        assert!(
            driver.submissions.is_empty(),
            "revoked callback dispatched native work"
        );
        Ok(())
    }

    /// A proven preparation failure retains its cause without failing the observation owner.
    #[tokio::test]
    async fn preparation_failure_preserves_cause_without_delivery_or_fallback() -> anyhow::Result<()>
    {
        let directory = tempfile::tempdir()?;
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            Arc::new(crate::trace::TraceStore::open(directory.path())?),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("preparation-failure");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let submission = SubmissionId::try_from("input".to_owned())?;
        let mut state = ExecutionState::new(owner.clone());
        state.admit(scope.clone(), submission.clone())?;
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let (reply, response) = oneshot::channel();
        let mut invocations = HashMap::new();
        invocations.insert(
            scope.invocation_id.clone(),
            Invocation {
                scope: scope.clone(),
                output: sink,
                reply: Some(reply),
                completion: crate::tools::ControlTurnContext::inactive(PromptMode::Chat),
                terminal: None,
                native_settlement: None,
                host_collection: HostCollection::Pending,
                publication_failure: None,
                pending_controls: 0,
                revision: 0,
                ticket_outstanding: false,
                ticket_nonce: 0,
                abandoned: Arc::new(AtomicBool::new(false)),
                idle_observation: false,
            },
        );
        let (delivery, mut pending) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (commands, _receive) = mpsc::channel(1);
        let mut driver = Driver {
            trace: Arc::new(crate::trace::TraceStore::open(directory.path())?),
            queue_loss_reported: AtomicBool::new(false),
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Default::default(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: Default::default(),
            prevented_invocations: Default::default(),
            reservations: Default::default(),
            latest_reservations: Default::default(),
            submission_exits: Default::default(),
        };
        driver.acceptance(SubmissionOutcome {
            scope: scope.clone(),
            submission: Some(submission),
            control_reply: None,
            result: Err(ProviderError::PreparationFailed(Box::new(
                ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::SessionOpen,
                    forge_provider_api::ProviderFailureKind::Initialization,
                )),
            ))),
        });
        if let Some(Delivery::Complete { reply, result }) = pending.recv().await {
            reply.send(*result).ok();
        }
        let Err(ProviderError::PreparationFailed(cause)) = response.await? else {
            panic!("expected typed preparation failure")
        };
        assert!(
            matches!(cause.as_ref(), ProviderError::Failure(failure) if failure.kind() == forge_provider_api::ProviderFailureKind::Initialization)
        );
        assert!(!driver.failed);
        assert!(driver.cleanup.is_empty());
        assert!(!driver.invocations.contains_key(&scope.invocation_id));
        assert!(driver.state.active_scope().is_none());
        driver.state.admit(
            InvocationScope::new(scope.owner.clone(), "next-explicit-request"),
            SubmissionId::try_from("next-input".to_owned())?,
        )?;
        Ok(())
    }

    /// An uncertain delivery cannot return success or release its waiter before cleanup.
    #[tokio::test]
    async fn uncertain_terminal_waits_for_cleanup_and_preserves_failure() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("uncertain-cleanup");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let submission = SubmissionId::try_from("input".to_owned())?;
        let mut state = ExecutionState::new(owner.clone());
        state.admit(scope.clone(), submission.clone())?;
        state.delivery(&scope, &submission, DeliveryState::Uncertain)?;
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let (reply, mut response) = oneshot::channel();
        let mut invocations = HashMap::new();
        invocations.insert(
            scope.invocation_id.clone(),
            Invocation {
                scope: scope.clone(),
                output: sink,
                reply: Some(reply),
                completion: crate::tools::ControlTurnContext::inactive(PromptMode::Chat),
                terminal: Some(Ok(
                    forge_provider_api::lifecycle::ExecutionResult::Completed {
                        completion: forge_provider_api::completion::ProviderCompletion::default()
                            .into(),
                    },
                )),
                native_settlement: None,
                host_collection: HostCollection::Pending,
                publication_failure: None,
                pending_controls: 0,
                revision: 0,
                ticket_outstanding: false,
                ticket_nonce: 0,
                abandoned: Arc::new(AtomicBool::new(false)),
                idle_observation: false,
            },
        );
        let (delivery, _pending) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (commands, _receive) = mpsc::channel(1);
        let mut driver = Driver {
            trace: Arc::new(crate::trace::TraceStore::open(directory.path())?),
            queue_loss_reported: AtomicBool::new(false),
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Default::default(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: Default::default(),
            prevented_invocations: Default::default(),
            reservations: Default::default(),
            latest_reservations: Default::default(),
            submission_exits: Default::default(),
        };
        driver.settle(&scope.invocation_id);
        assert!(matches!(
            response.try_recv(),
            Err(oneshot::error::TryRecvError::Empty)
        ));
        // A failed cleanup task must retain the original uncertainty in its typed report.
        driver.cleanup.abort_all();
        let cleanup = driver.cleanup.join_next().await;
        driver.cleanup_finished(cleanup);
        let Err(ProviderError::Cleanup(report)) = response.await? else {
            panic!("cleanup failure must reach the execution waiter");
        };
        assert!(matches!(
            report.observation(),
            ObservationFailure::DeliveryUncertain
        ));
        assert!(!report.collection().is_empty());
        Ok(())
    }

    /// A typed observation can arrive before the separate acceptance task completes.
    #[tokio::test]
    async fn uncertain_observation_before_acceptance_preserves_typed_error() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let owner = ExecutionOwner::root("uncertain-cleanup");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let submission = SubmissionId::try_from("input".to_owned())?;
        let mut state = ExecutionState::new(owner.clone());
        state.admit(scope.clone(), submission.clone())?;
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        let (reply, mut response) = oneshot::channel();
        let mut invocations = HashMap::new();
        invocations.insert(
            scope.invocation_id.clone(),
            Invocation {
                scope: scope.clone(),
                output: sink,
                reply: Some(reply),
                completion: crate::tools::ControlTurnContext::inactive(PromptMode::Chat),
                terminal: None,
                pending_controls: 0,
                native_settlement: None,
                host_collection: HostCollection::Pending,
                publication_failure: None,
                revision: 0,
                ticket_outstanding: false,
                ticket_nonce: 0,
                abandoned: Arc::new(AtomicBool::new(false)),
                idle_observation: false,
            },
        );
        let (delivery, _pending) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (commands, _receive) = mpsc::channel(1);
        let mut driver = Driver {
            trace: Arc::new(crate::trace::TraceStore::open(directory.path())?),
            queue_loss_reported: AtomicBool::new(false),
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            pending_delivery: VecDeque::new(),
            delivery_pressure: tokio::sync::watch::channel(false).0,
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            retiring: None,
            idle: SessionObserver::Inbox(Arc::default()),
            activity: Default::default(),
            admitted: Default::default(),
            attempts: HashMap::new(),
            cancelled_admissions: Default::default(),
            cancelled_exchanges: Default::default(),
            prevented_invocations: Default::default(),
            reservations: Default::default(),
            latest_reservations: Default::default(),
            submission_exits: Default::default(),
        };
        let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::ObservationLost {
                failure: ObservationFailure::DeliveryUncertain,
                completion: None,
            },
        )?;
        driver.event(ProviderEvent::Execution { observation });
        assert!(matches!(
            response.try_recv(),
            Err(oneshot::error::TryRecvError::Empty)
        ));
        let cleanup = driver.cleanup.join_next().await;
        driver.cleanup_finished(cleanup);
        assert!(matches!(
            response.await?,
            Err(ProviderError::DeliveryUncertain)
        ));
        Ok(())
    }

    /// Closed presentation queues fail completion waiters instead of dropping them.
    #[tokio::test]
    async fn rejected_completion_resolves_with_observation_failure() {
        let (reply, response) = oneshot::channel();
        Delivery::Complete {
            reply,
            result: Box::new(Err(protocol("original"))),
        }
        .fail(ObservationFailure::Disconnected);
        assert!(matches!(
            response.await.unwrap(),
            Err(ProviderError::Observation(ObservationFailure::Disconnected))
        ));
    }

    /// Error cleanup must consume cancellation events before waiting for the provider close watermark.
    #[tokio::test]
    async fn failed_foreground_close_drains_native_cancellation_events() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        crate::exchange::core_log::persist_test_session(
            directory.path(),
            "failed-foreground-close",
        )?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let store =
            crate::provider::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let owner = ExecutionOwner::root("failed-foreground-close");
        let registry = SessionExecutionRegistry::new(provider.clone(), trace);
        let engine = registry.owner(owner.clone()).await?;
        let control = provider.mock_response_control().unwrap();
        let hold = control
            .dispatch(
                owner.session_id.as_str(),
                MockResponseCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_session: false,
                },
            )
            .await?;
        let request = ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: Some(owner.clone()),
            harness_session_id: owner.session_id.to_string(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("Wait until cancelled"),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let (sink, mut events) = crate::provider::events::channel(scope);
        let drain = tokio::spawn(async move { while let Ok(Some(_)) = events.recv().await {} });
        let running = engine.clone();
        let result = tokio::spawn(async move { running.run_once(request, sink, &store).await });
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            control.dispatch(
                owner.session_id.as_str(),
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().unwrap().into(),
                },
            ),
        )
        .await??;
        let close = tokio::time::timeout(std::time::Duration::from_secs(5), engine.close()).await;
        if close.is_err() {
            let artifacts = directory.keep();
            anyhow::bail!(
                "foreground close deadlocked; trace retained at {}",
                artifacts.display()
            );
        }
        close??;
        assert!(
            tokio::time::timeout(std::time::Duration::from_secs(5), result)
                .await??
                .is_err()
        );
        tokio::time::timeout(std::time::Duration::from_secs(5), drain).await??;
        Ok(())
    }

    /// Keep a host result pending after acceptance, and reuse one engine for the next exchange.
    #[tokio::test]
    async fn host_waits_for_execution_completion_and_reuses_session_intake() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?;
        let store =
            crate::provider::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let owner = ExecutionOwner::root("engine-fixture");
        let registry = SessionExecutionRegistry::new(provider.clone(), trace.clone());
        let engine = registry.owner(owner.clone()).await?;
        assert!(Arc::ptr_eq(&engine, &registry.owner(owner.clone()).await?));
        let _idle = engine.session_events().await?;
        let mut retained_scope = None;
        for exchange in ["one", "two"] {
            let mut request = ProviderRequest {
                dispatch: forge_provider_api::submission::DispatchTracker::default(),
                execution_owner: Some(owner.clone()),
                harness_session_id: "engine-fixture".into(),
                workspace: directory.path().to_string_lossy().into_owned(),
                input: ProviderInput::from_text("hello"),
                mode: PromptMode::Chat,
                model: "default".into(),
                effort: "default".into(),
                context_window: None,
                service_tier: Default::default(),
                access: Default::default(),
                approval_mode: crate::session::ApprovalMode::Ask,
                provider_session_id: None,
                submission: None,
                recovered_input: Vec::new(),
                control_context: None,
            };
            request.model = provider
                .model_list(forge_provider_api::QueryContext::Workspace(
                    forge_provider_api::WorkspaceContext {
                        workspace: request.workspace.clone(),
                    },
                ))
                .await?
                .into_iter()
                .next()
                .context("fixture provider advertises a model")?
                .id;
            let control = provider.mock_response_control().unwrap();
            let hold = control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Hold {
                        mode: Some(PromptMode::Chat),
                        any_session: false,
                    },
                )
                .await
                .context("arm prompt hold")?;
            let hold_id = hold["id"].as_str().unwrap().to_owned();
            let scope = InvocationScope::new(owner.clone(), exchange);
            retained_scope = Some(scope.clone());
            let (sink, mut events) = crate::provider::events::channel(scope);
            let running = engine.clone();
            let store = store.clone();
            let mut result =
                tokio::spawn(async move { running.run_once(request, sink, &store).await });
            let hold_wait = control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Wait {
                        id: hold_id.clone(),
                    },
                )
                .await;
            if let Err(error) = hold_wait {
                let outcome =
                    tokio::time::timeout(std::time::Duration::from_secs(2), &mut result).await;
                anyhow::bail!(
                    "wait for exchange {exchange} hold: {error}; prompt outcome: {outcome:?}"
                );
            }
            assert!(
                !result.is_finished(),
                "acceptance was mistaken for execution completion"
            );
            control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Release { id: hold_id },
                )
                .await
                .with_context(|| format!("release exchange {exchange} hold"))?;
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                loop {
                    tokio::select! {
                        value = &mut result => { value?.with_context(|| format!("complete exchange {exchange}"))?; return Ok::<_, anyhow::Error>(()); }
                        event = events.recv() => { event.with_context(|| format!("observe exchange {exchange}"))?; }
                    }
                }
            })
            .await.with_context(|| format!("complete exchange {exchange} before deadline"))??;
        }
        let scope = retained_scope.unwrap();
        let catalog = crate::provider::ProviderCatalogRequest {
            execution_owner: Some(owner),
            harness_session_id: "engine-fixture".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            provider_session_id: None,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
        };
        let receipt = store.prepare_steering(
            &catalog,
            ProviderInput::from_text("continue original work"),
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
        )?;
        let rejected = engine
            .steer(
                scope,
                crate::provider::SteeringInput::User {
                    text: "continue original work".into(),
                    attachments: Vec::new(),
                    submission: Some(receipt.clone()),
                },
            )
            .await;
        assert!(
            matches!(rejected,
            Err(ProviderError::PreparationFailed(error))
            if matches!(error.as_ref(), ProviderError::Failure(failure)
                if failure.operation() == forge_provider_api::ProviderOperation::InputSteer
                    && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)),
            "retired native invocation accepted new input"
        );
        assert!(
            matches!(
                receipt.record()?.delivery,
                crate::provider::submission::SubmissionState::Prepared
            ),
            "rejected late steering changed durable delivery evidence"
        );
        registry.close().await.context("close fixture registry")?;
        registry.close().await.context("close fixture registry")?;
        assert!(
            registry
                .owner(ExecutionOwner::root("closed"))
                .await
                .is_err()
        );
        provider
            .shutdown()
            .await
            .context("shutdown fixture provider")?;
        Ok(())
    }
}
