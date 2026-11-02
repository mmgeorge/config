//! Own session intake independently of submissions and scoped result waiters.

use std::collections::HashMap;
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
    RetainedWork(anyhow::Error),
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
            Self::RetainedWork(error) => Some(error.as_ref()),
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
    pub(crate) fn new(provider: Arc<Provider>) -> Self {
        Self {
            provider,
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
            owners: Mutex::new(owners),
            gate: tokio::sync::RwLock::new(()),
            closed: AtomicBool::new(false),
            cleanup: super::cleanup::CleanupLedger::default(),
        }
    }

    pub(crate) fn retain_cleanup<F>(
        &self,
        work: F,
    ) -> anyhow::Result<oneshot::Receiver<Result<(), super::RetainedCleanupFailure>>>
    where
        F: std::future::Future<Output = anyhow::Result<()>> + Send + 'static,
    {
        self.cleanup.start(work)
    }

    pub(crate) fn record_cleanup_failure(&self, error: anyhow::Error) {
        self.cleanup.record_failure(error);
    }

    pub(crate) async fn collect_cleanup(&self) -> anyhow::Result<()> {
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
                SessionExecution::open(self.provider.clone(), owner, slot.observer.clone())
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
        if !old
            .observer
            .ready_for_replacement()
            .map_err(ProviderError::from)?
        {
            return Err(protocol("old execution observations have not been consumed").into());
        }
        old.observer
            .retire_background()
            .await
            .map_err(ProviderError::from)?;
        self.provider
            .reset_session(&owner)
            .await
            .map_err(ProviderError::from)?;
        let observer = Arc::new(super::SessionObservationInbox::default());
        let engine = Arc::new(
            SessionExecution::open(self.provider.clone(), owner.clone(), observer.clone()).await?,
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
        self.cleanup.seal().map_err(RegistryError::RetainedWork)?;
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
                failure.get_or_insert_with(|| ProviderError::from(error));
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

enum Command {
    Close,
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
        submission: SubmissionId,
        sink: ProviderEventSink,
        completion: Box<crate::tools::ControlTurnContext>,
        continuation: Option<(u64, u64, u64)>,
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
        event: ProviderEvent,
        fallback: Option<SessionObserver>,
    },
    Complete {
        reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
        result: Result<NativeReady, ProviderError>,
    },
}

struct Invocation {
    scope: InvocationScope,
    output: ProviderEventSink,
    reply: Option<oneshot::Sender<Result<NativeReady, ProviderError>>>,
    completion: crate::tools::ControlTurnContext,
    terminal: Option<Result<forge_provider_api::lifecycle::ExecutionResult, ProviderError>>,
    pending_controls: usize,
    revision: u64,
    ticket_outstanding: bool,
    ticket_nonce: u64,
    abandoned: Arc<AtomicBool>,
    idle_observation: bool,
}

struct SubmissionOutcome {
    scope: InvocationScope,
    submission: Option<SubmissionId>,
    control_reply: Option<oneshot::Sender<Result<(), ProviderError>>>,
    result: Result<(), ProviderError>,
}

#[derive(Clone)]
enum SessionObserver {
    Inbox(Arc<super::SessionObservationInbox>),
    Stream(ProviderEventSink),
}

impl SessionObserver {
    /// Preserve an invocation event when its result observer has been dropped.
    async fn retain(&self, event: ProviderEvent) -> Result<(), ProviderError> {
        match self {
            Self::Inbox(inbox) => inbox.push(event).map_err(ProviderError::from),
            Self::Stream(sink) => {
                sink.bind_scope(event.scope())?.send_wait(event).await?;
                Ok(())
            }
        }
    }

    fn close(&self) {
        if let Self::Inbox(inbox) = self {
            inbox.close();
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
    DeliveryUncertain {
        message: String,
    },
    PreparationCleanup {
        cause: Arc<ProviderError>,
        cleanup: Arc<ProviderError>,
    },
}

struct FailedWaiter {
    reply: oneshot::Sender<Result<NativeReady, ProviderError>>,
    original: Option<ProviderError>,
}

struct Driver {
    owner: ExecutionOwner,
    provider: Arc<Provider>,
    state: ExecutionState,
    invocations: HashMap<InvocationId, Invocation>,
    /// Completed authority retained for positively correlated idle continuations.
    committed_invocations: HashMap<InvocationId, Invocation>,
    submissions: JoinSet<SubmissionOutcome>,
    deliveries: JoinSet<Result<(), ProviderError>>,
    delivery: Option<mpsc::Sender<Delivery>>,
    cleanup: JoinSet<forge_provider_api::error::ObservationCleanup>,
    failed_waiters: Vec<FailedWaiter>,
    commands: mpsc::Sender<Command>,
    next_ticket_nonce: u64,
    committed_tickets: HashMap<(InvocationId, u64), InvocationScope>,
    failed: bool,
    healthy: Arc<AtomicBool>,
    first_failure: Option<ObservationFailure>,
    cleanup_failure: Option<Arc<forge_provider_api::error::ObservationCleanup>>,
    idle: SessionObserver,
}

impl SessionExecution {
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
    ) -> Result<Self, ProviderError> {
        let mut events = provider.session_events(owner.clone()).await?;
        let (commands, mut receive) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let (delivery, output) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
        let mut deliveries = JoinSet::new();
        deliveries.spawn(deliver(output));
        let healthy = Arc::new(AtomicBool::new(true));
        let orderly_shutdown = Arc::new(AtomicBool::new(false));
        let driver_shutdown = orderly_shutdown.clone();
        let mut driver = Driver {
            owner: owner.clone(),
            provider: provider.clone(),
            state: ExecutionState::new(owner.clone()),
            invocations: HashMap::new(),
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries,
            delivery: Some(delivery),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands: commands.clone(),
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: healthy.clone(),
            first_failure: None,
            cleanup_failure: None,
            idle: SessionObserver::Inbox(observer),
        };
        let task = tokio::spawn(async move {
            loop {
                tokio::select! {
                    command = receive.recv() => match command {
                        Some(Command::Close) => {
                            loop {
                                match events.try_recv() {
                                    Ok(event) => {
                                        driver.event(event);
                                        // Let the bounded presentation worker collect each accepted frame.
                                        tokio::task::yield_now().await;
                                    },
                                    Err(crate::provider::events::EventDeliveryError::Empty | crate::provider::events::EventDeliveryError::Closed { .. }) => break,
                                    Err(crate::provider::events::EventDeliveryError::Observation(failure)) => { driver.fail_observation(failure); break; },
                                    Err(crate::provider::events::EventDeliveryError::OwnershipMismatch { .. }) => { driver.fail_observation(ObservationFailure::OwnershipConflict); break; },
                                    Err(error) => { driver.fail_all(&error.to_string()); break; },
                                }
                            }
                            break;
                        },
                        Some(command) => driver.command(command),
                        None => break,
                    },
                    event = events.recv() => match event {
                        Ok(Some(event)) => driver.event(event),
                        Ok(None) => {
                            if !driver_shutdown.load(Ordering::Acquire) {
                                driver.fail_observation(ObservationFailure::Disconnected);
                            }
                            break;
                        },
                        Err(crate::provider::events::EventDeliveryError::Closed { .. }) if driver_shutdown.load(Ordering::Acquire) => break,
                        Err(crate::provider::events::EventDeliveryError::Observation(failure)) => {
                            driver.fail_observation(failure); break;
                        },
                        Err(crate::provider::events::EventDeliveryError::OwnershipMismatch { .. }) => {
                            driver.fail_observation(ObservationFailure::OwnershipConflict); break;
                        },
                        Err(error) => { driver.fail_all(&error.to_string()); break; }
                    },
                    delivered = driver.submissions.join_next(), if !driver.submissions.is_empty() => {
                        match delivered {
                            Some(Ok(outcome)) => driver.acceptance(outcome),
                            Some(Err(_)) => driver.fail_all("provider submission task failed"),
                            None => {},
                        }
                    },
                    result = driver.deliveries.join_next(), if !driver.deliveries.is_empty() => {
                        match result {
                            Some(Ok(Ok(()))) | None => {},
                            Some(Ok(Err(ProviderError::Observation(failure)))) => driver.fail_observation(failure),
                            Some(Ok(Err(_))) | Some(Err(_)) => driver.fail_observation(ObservationFailure::Disconnected),
                        }
                    },
                    result = driver.cleanup.join_next(), if !driver.cleanup.is_empty() => { driver.cleanup_finished(result); },
                }
                if driver.failed && driver.cleanup.is_empty() {
                    break;
                }
            }
            if !driver.invocations.is_empty() || driver.state.active_scope().is_some() {
                driver.fail_observation(ObservationFailure::Disconnected);
            }
            driver.submissions.abort_all();
            while driver.submissions.join_next().await.is_some() {}
            while let Some(result) = driver.cleanup.join_next().await {
                driver.cleanup_finished(Some(result));
            }
            driver.delivery.take();
            let drained = tokio::time::timeout(std::time::Duration::from_secs(15), async {
                while driver.deliveries.join_next().await.is_some() {}
            })
            .await;
            if drained.is_err() {
                driver.deliveries.abort_all();
                while driver.deliveries.join_next().await.is_some() {}
            }
            driver.idle.close();
            if let Some(report) = driver.cleanup_failure {
                return Err(ProviderError::Cleanup(report));
            }
            if drained.is_err() {
                return Err(protocol("execution delivery drain exceeded its deadline"));
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
        })
    }

    /// Mark the native stream closure as intentional before shutting its provider down.
    pub(crate) fn begin_orderly_shutdown(&self) {
        self.healthy.store(false, Ordering::Release);
        self.orderly_shutdown.store(true, Ordering::Release);
    }

    /// Stop accepting work and join the driver after its bounded cleanup and delivery drain.
    pub(crate) async fn close(&self) -> Result<(), ProviderError> {
        let mut task = self.task.lock().await;
        let handle = match &mut *task {
            ExecutionTask::Running(handle) => handle,
            ExecutionTask::Closed => return Ok(()),
            ExecutionTask::Failed(report) => return Err(ProviderError::Cleanup(report.clone())),
        };
        let _ = self.commands.send(Command::Close).await;
        let result = handle.await;
        let result = match result {
            Ok(result) => result,
            Err(error) => Err(protocol(&format!("execution driver failed: {error}"))),
        };
        match result {
            Ok(()) => {
                *task = ExecutionTask::Closed;
                Ok(())
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
            crate::provider::events::channel(crate::provider::EventScope::SessionOperation {
                owner: self.owner.clone(),
                operation_id: uuid::Uuid::new_v4().to_string(),
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
        if !self.commands.same_channel(&ticket.commands)
            || sink.invocation_scope()? != &ticket.scope
        {
            return Err(protocol(
                "continuation ticket belongs to another invocation or generation",
            ));
        }
        let (stale, mut rejected) = oneshot::channel();
        let result = self
            .submit_until_ready(
                request,
                sink,
                store,
                Some((ticket.revision, ticket.effects_revision, ticket.nonce)),
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
        continuation: Option<(u64, u64, u64)>,
        stale: Option<oneshot::Sender<ProviderRequest>>,
    ) -> Result<NativeReady, ProviderError> {
        let scope = sink.invocation_scope()?.clone();
        if request.owner()? != self.owner || scope.owner != self.owner {
            return Err(protocol("execution request belongs to another owner"));
        }
        let submission = match request.submission.take() {
            Some(submission) => submission,
            None => store.prepare(&request, self.provider.descriptor().kind)?,
        };
        submission
            .recover(self.provider.as_ref(), &mut request)
            .await?;
        let id = SubmissionId::try_from(submission.id().to_owned())
            .map_err(|error| protocol(&error.to_string()))?;
        request.submission = Some(submission);
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
    fn command(&mut self, command: Command) {
        match command {
            Command::Close => unreachable!("close is handled by the driver loop"),
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
                    let _ = reply.send(Err(ProviderError::Rejected {
                        message: "owner has no admitted invocation for steering".into(),
                    }));
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
                if self.failed || !self.state.contains(&scope) {
                    let _ = reply.send(Err(ProviderError::Rejected {
                        message: "steering has no retained execution owner".into(),
                    }));
                    return;
                }
                if self
                    .state
                    .active_scope()
                    .is_some_and(|active| active != &scope)
                {
                    let _ = reply.send(Err(ProviderError::Rejected {
                        message: "another invocation owns this session".into(),
                    }));
                    return;
                }
                let mut reply = Some(reply);
                let dispatch_authority = authority.clone();
                let dispatch = || -> anyhow::Result<()> {
                    let invocation = self
                        .invocations
                        .get_mut(&scope.invocation_id)
                        .or_else(|| self.committed_invocations.get_mut(&scope.invocation_id))
                        .ok_or_else(|| {
                            anyhow::anyhow!("steering has no retained host authority")
                        })?;
                    let revision = invocation
                        .revision
                        .checked_add(1)
                        .ok_or_else(|| anyhow::anyhow!("settlement revision exhausted"))?;
                    let pending = invocation
                        .pending_controls
                        .checked_add(1)
                        .ok_or_else(|| anyhow::anyhow!("pending control count exhausted"))?;
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
                        let result = match dispatch_authority.as_ref().map(|authority| authority.validate_scope()).transpose() {
                            Err(error) => Err(ProviderError::NotConsumed { message: format!("callback authority was revoked before native dispatch: {error:#}") }),
                            Ok(_) => {
                                let result = provider.session_steer(&scope.owner, scope.clone(), input).await;
                                if dispatch_authority.as_ref().is_some_and(|authority| authority.validate_scope().is_err())
                                    && !matches!(&result, Err(ProviderError::NotConsumed { .. }))
                                {
                                    Err(ProviderError::DeliveryUncertain { message: "callback authority changed while native steering was in flight".into() })
                                } else { result }
                            }
                        };
                        SubmissionOutcome { scope, submission, control_reply: Some(reply), result }
                    });
                    Ok(())
                };
                let admitted = match authority {
                    Some(authority) => authority.commit(dispatch),
                    None => dispatch(),
                };
                if let Err(error) = admitted
                    && let Some(reply) = reply
                {
                    let _ = reply.send(Err(ProviderError::NotConsumed {
                        message: format!("control was not admitted: {error:#}"),
                    }));
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
                            let _ = reply.send(Err(error.into()));
                            return;
                        }
                    },
                    SessionObserver::Stream(_) => unreachable!("stream observer rejected above"),
                };
                self.idle = SessionObserver::Stream(sink);
                for event in pending {
                    self.idle_event(event);
                }
                if self.failed {
                    let _ = reply.send(Err(ProviderError::Observation(
                        ObservationFailure::Overflow,
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
                if let Some((revision, effects_revision, nonce)) = continuation {
                    let Some(invocation) = self
                        .invocations
                        .get_mut(&scope.invocation_id)
                        .filter(|invocation| invocation.scope == scope)
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
                            self.state.readiness(&scope),
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
                    if let Err(error) = self.state.admit(scope.clone(), submission.clone()) {
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
                    let Some(next_revision) = invocation.revision.checked_add(1) else {
                        let _ = reply.send(Err(protocol("settlement revision exhausted")));
                        self.fail_all("settlement revision exhausted");
                        return;
                    };
                    invocation.revision = next_revision;
                    invocation.ticket_outstanding = false;
                    invocation.terminal = None;
                    invocation.output = sink;
                    invocation.idle_observation = false;
                    invocation.completion = context.clone();
                    invocation.reply = Some(reply);
                    invocation.abandoned = abandoned;
                } else {
                    if !self.invocations.is_empty() || self.state.active_scope().is_some() {
                        let _ = reply.send(Err(protocol(
                            "execution owner is unavailable or already active",
                        )));
                        return;
                    }
                    if let Err(error) = self.state.admit(scope.clone(), submission.clone()) {
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
                self.submissions.spawn(async move {
                    let result = provider.session_prompt(request, scope.clone()).await;
                    SubmissionOutcome {
                        scope,
                        submission: Some(submission),
                        control_reply: None,
                        result,
                    }
                });
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
                if self.committed_invocations.len() >= forge_protocol::MAX_PENDING_FRAMES
                    && !self
                        .committed_invocations
                        .contains_key(&scope.invocation_id)
                {
                    let failure = ObservationFailure::Overflow;
                    let _ = reply.send(Err(SettlementError::Provider(ProviderError::Observation(
                        failure.clone(),
                    ))));
                    self.fail_observation(failure);
                    return;
                }
                match invocation
                    .completion
                    .commit_provider_execution(effects_revision)
                {
                    Ok(true) => {
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
                Err(ProviderError::NotConsumed { .. }) => DeliveryState::NotConsumed,
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
            Err(ProviderError::NotConsumed { .. }) => DeliveryState::NotConsumed,
            Err(ProviderError::PreparationFailed(_)) if !is_control => DeliveryState::NotConsumed,
            Err(_) => DeliveryState::Uncertain,
        };
        if let Some(submission) = submission
            && let Err(error) = self.state.delivery(&scope, &submission, delivery)
        {
            invocation.terminal = Some(Err(protocol(&error.to_string())));
        }
        if let Some(reply) = control_reply {
            if result.is_err() && delivery != DeliveryState::NotConsumed {
                invocation.terminal = Some(Err(ProviderError::DeliveryUncertain {
                    message: "steering delivery failed; native completion is unknown".into(),
                }));
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
        let Some(scope) = event.scope().invocation().cloned() else {
            self.idle_event(event);
            return;
        };
        if !self.invocations.contains_key(&scope.invocation_id)
            && matches!(&event, ProviderEvent::Execution { observation } if matches!(observation.update(), ExecutionUpdate::Started { .. }))
            && let Some(mut invocation) = self.committed_invocations.remove(&scope.invocation_id)
        {
            if invocation.scope != scope || !self.invocations.is_empty() {
                self.fail_all("late execution conflicts with another admitted exchange");
                return;
            }
            if let Err(error) = invocation.completion.provider_completion.begin_attempt() {
                self.fail_all(&error.to_string());
                return;
            }
            invocation.terminal = None;
            invocation.reply = None;
            invocation.ticket_outstanding = false;
            invocation.idle_observation = true;
            self.invocations
                .insert(scope.invocation_id.clone(), invocation);
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
            let Some(revision) = invocation.revision.checked_add(1) else {
                self.fail_all("settlement revision exhausted");
                return;
            };
            invocation.revision = revision;
            if let Err(error) = self.state.observe(observation.clone()) {
                invocation.terminal = Some(Err(protocol(&error.to_string())));
            } else {
                invocation.terminal = match observation.update() {
                    ExecutionUpdate::Started { .. } => None,
                    ExecutionUpdate::Finished { result, .. }
                    | ExecutionUpdate::ResolvedWithoutExecution { result, .. } => {
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
            event,
            fallback: Some(self.idle.clone()),
        }) {
            self.fail_observation(ObservationFailure::Overflow);
            return;
        }
        self.settle(&scope.invocation_id);
    }

    /// Retain initialization and late output under a bounded owner-wide observer.
    fn idle_event(&mut self, event: ProviderEvent) {
        match self.idle.clone() {
            SessionObserver::Inbox(inbox) => {
                if !self.enqueue(Delivery::Inbox { inbox, event }) {
                    self.fail_observation(ObservationFailure::Overflow);
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
                    event,
                    fallback: None,
                }) {
                    self.fail_observation(ObservationFailure::Overflow);
                }
            }
        }
    }

    fn enqueue(&self, delivery: Delivery) -> bool {
        let Some(sender) = self.delivery.as_ref() else {
            delivery.fail(ObservationFailure::Disconnected);
            return false;
        };
        match sender.try_send(delivery) {
            Ok(()) => true,
            Err(mpsc::error::TrySendError::Full(delivery)) => {
                delivery.fail(ObservationFailure::Overflow);
                false
            }
            Err(mpsc::error::TrySendError::Closed(delivery)) => {
                delivery.fail(ObservationFailure::Disconnected);
                false
            }
        }
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
                if let Some(Err(ProviderError::PreparationCleanup { cause, cleanup })) =
                    &invocation.terminal
                {
                    FailureOrigin::PreparationCleanup {
                        cause: cause.clone(),
                        cleanup: cleanup.clone(),
                    }
                } else {
                    let message = match &invocation.terminal {
                        Some(Err(ProviderError::DeliveryUncertain { message })) => message.clone(),
                        _ => "provider delivery is uncertain; native completion is unknown".into(),
                    };
                    FailureOrigin::DeliveryUncertain { message }
                }
            }
            Ok(ExecutionReadiness::ObservationLost(ObservationFailure::DeliveryUncertain {
                message,
            })) => FailureOrigin::DeliveryUncertain {
                message: message.clone(),
            },
            _ => FailureOrigin::Observation,
        };
        let failure = match ready {
            Ok(ExecutionReadiness::Waiting) => return,
            Ok(ExecutionReadiness::Complete) => None,
            Ok(ExecutionReadiness::ObservationLost(failure)) => Some(failure),
            Ok(ExecutionReadiness::DeliveryUncertain) => Some(ObservationFailure::Protocol {
                message: "provider delivery is uncertain; native completion is unknown".into(),
            }),
            Err(error) => Some(ObservationFailure::Protocol {
                message: error.to_string(),
            }),
        };
        if let Some(failure) = failure {
            self.fail_observation_with_origin(failure, origin);
            return;
        }
        if self.invocations.get(id).is_some_and(|invocation| {
            matches!(
                &invocation.terminal,
                Some(Err(ProviderError::PreparationFailed(_)))
            )
        }) {
            let mut invocation = self
                .invocations
                .remove(id)
                .expect("invocation checked above");
            if let (Some(reply), Some(Err(error))) =
                (invocation.reply.take(), invocation.terminal.take())
                && !self.enqueue(Delivery::Complete {
                    reply,
                    result: Err(error),
                })
            {
                self.fail_all("execution result delivery overflowed or closed");
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
            outcome,
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
        if !self.enqueue(Delivery::Complete { reply, result }) {
            self.fail_all("execution result delivery overflowed or closed");
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
            Some(Ok(report)) => Arc::new(report),
            Some(Err(error)) => {
                let report = forge_provider_api::error::ObservationCleanup::new(
                    self.first_failure
                        .clone()
                        .unwrap_or(ObservationFailure::Disconnected),
                    None,
                    None,
                    Some(forge_provider_api::error::CleanupFailure::Failed(Box::new(
                        protocol(&format!("cleanup task failed: {error}")),
                    ))),
                );
                let origin = self
                    .failed_waiters
                    .iter()
                    .find_map(|waiter| match &waiter.original {
                        Some(ProviderError::DeliveryUncertain { message }) => {
                            Some(ProviderError::DeliveryUncertain {
                                message: message.clone(),
                            })
                        }
                        Some(ProviderError::PreparationCleanup { cause, cleanup }) => {
                            Some(ProviderError::PreparationCleanup {
                                cause: cause.clone(),
                                cleanup: cleanup.clone(),
                            })
                        }
                        _ => None,
                    });
                Arc::new(match origin {
                    Some(origin) => report.with_origin(origin),
                    None => report,
                })
            }
            None => return,
        };
        if !report.collected() {
            self.cleanup_failure = Some(report.clone());
            self.idle.diagnostic(ProviderEvent::Diagnostic {
                scope: crate::provider::EventScope::SessionOperation {
                    owner: self.owner.clone(),
                    operation_id: uuid::Uuid::new_v4().to_string(),
                },
                diagnostic: crate::provider::ProviderDiagnostic {
                    code: Some("observation_cleanup_failed".into()),
                    message: report.to_string(),
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

    fn fail_all(&mut self, message: &str) {
        self.fail_observation(ObservationFailure::Protocol {
            message: message.to_owned(),
        });
    }

    fn fail_observation(&mut self, failure: ObservationFailure) {
        let origin = match &failure {
            ObservationFailure::DeliveryUncertain { message } => FailureOrigin::DeliveryUncertain {
                message: message.clone(),
            },
            _ => FailureOrigin::Observation,
        };
        self.fail_observation_with_origin(failure, origin);
    }

    fn fail_observation_with_origin(&mut self, failure: ObservationFailure, origin: FailureOrigin) {
        if !self.failed {
            self.healthy.store(false, Ordering::Release);
            self.failed = true;
            self.first_failure = Some(failure.clone());
            let provider = self.provider.clone();
            let owner = self.owner.clone();
            let key = self.owner.runtime_key();
            let observation = failure.clone();
            let cleanup_origin = match &origin {
                FailureOrigin::DeliveryUncertain { message } => {
                    Some(ProviderError::DeliveryUncertain {
                        message: message.clone(),
                    })
                }
                FailureOrigin::Observation => None,
                FailureOrigin::PreparationCleanup { cause, cleanup } => {
                    Some(ProviderError::PreparationCleanup {
                        cause: cause.clone(),
                        cleanup: cleanup.clone(),
                    })
                }
            };
            self.cleanup.spawn(async move {
                let deadline = std::time::Duration::from_secs(15);
                let fence = tokio::time::timeout(deadline, provider.fence_session(&owner)).await;
                let cancellation =
                    tokio::time::timeout(deadline, provider.cancel_session(&key)).await;
                // Collection still runs when the stop request fails or exceeds its deadline.
                let collection =
                    tokio::time::timeout(deadline, provider.cleanup_execution(&key)).await;
                use forge_provider_api::error::{CleanupFailure, ObservationCleanup};
                let classify = |result: Result<
                    Result<(), anyhow::Error>,
                    tokio::time::error::Elapsed,
                >| match result {
                    Ok(Ok(())) => None,
                    Ok(Err(error)) => {
                        Some(CleanupFailure::Failed(Box::new(ProviderError::from(error))))
                    }
                    Err(_) => Some(CleanupFailure::TimedOut),
                };
                let report = ObservationCleanup::new(
                    observation,
                    classify(fence),
                    classify(cancellation),
                    classify(collection),
                );
                match cleanup_origin {
                    Some(origin) => report.with_origin(origin),
                    None => report,
                }
            });
        }
        match &self.idle {
            SessionObserver::Inbox(inbox) => inbox.fail(failure.clone()),
            SessionObserver::Stream(sink) => sink.fail(
                crate::provider::events::EventDeliveryError::Observation(failure.clone()),
            ),
        }
        let invocations = std::mem::take(&mut self.invocations);
        for (_, invocation) in invocations {
            if let Ok(observation) = forge_provider_api::lifecycle::ExecutionObservation::new(
                invocation.scope,
                ExecutionUpdate::ObservationLost {
                    failure: failure.clone(),
                    completion: None,
                },
            ) {
                self.enqueue(Delivery::Event {
                    sink: invocation.output,
                    event: ProviderEvent::Execution { observation },
                    fallback: Some(self.idle.clone()),
                });
            }
            if let Some(reply) = invocation.reply {
                let original = match &origin {
                    FailureOrigin::DeliveryUncertain { message } => {
                        Some(match invocation.terminal {
                            Some(Err(error @ ProviderError::DeliveryUncertain { .. })) => error,
                            _ => ProviderError::DeliveryUncertain {
                                message: message.clone(),
                            },
                        })
                    }
                    FailureOrigin::Observation => None,
                    FailureOrigin::PreparationCleanup { cause, cleanup } => {
                        Some(ProviderError::PreparationCleanup {
                            cause: cause.clone(),
                            cleanup: cleanup.clone(),
                        })
                    }
                };
                self.failed_waiters.push(FailedWaiter { reply, original });
            }
        }
    }
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
                let result = inbox.push(event).map_err(ProviderError::from);
                if let Err(ProviderError::Observation(failure)) = &result {
                    inbox.fail(failure.clone());
                }
                result
            }
            Delivery::Event {
                sink,
                event,
                fallback,
            } => match fallback {
                Some(observer) => match sink.send_wait(event.clone()).await {
                    Ok(()) => Ok(()),
                    Err(crate::provider::events::EventDeliveryError::Closed { .. }) => {
                        observer.retain(event).await
                    }
                    Err(error) => Err(error.into()),
                },
                None => sink.send_wait(event).await.map_err(ProviderError::from),
            },
            Delivery::Complete { reply, result } => {
                let _ = reply.send(result);
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

fn protocol(message: &str) -> ProviderError {
    ProviderError::Protocol {
        message: message.to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        cell.set_batches(vec![crate::provider::HostEffectBatch {
            revision: 1,
            completion: ProviderCompletion::default(),
        }])?;
        assert!(cell.validate_effects_settled().is_err());
        Ok(())
    }

    /// A failed collection remains fenced on every subsequent close attempt.
    #[tokio::test]
    async fn close_retains_incomplete_cleanup_failure() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace,
            directory.path().into(),
        )?);
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
        };
        for _ in 0..2 {
            let Err(ProviderError::Cleanup(actual)) = engine.close().await else {
                panic!("incomplete cleanup must remain a failure");
            };
            assert!(Arc::ptr_eq(&actual, &expected));
            assert!(!actual.collected());
        }
        Ok(())
    }

    /// Retain the owner until host commit and invalidate a ticket when a steer reopens work.
    #[tokio::test]
    async fn host_ticket_blocks_admission_and_reopens_on_steering() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let owner = ExecutionOwner::root("host-ticket");
        let registry = SessionExecutionRegistry::new(provider.clone());
        let engine = registry.owner(owner.clone()).await?;
        let request = ProviderRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: "host-ticket".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("hello"),
            mode: PromptMode::Chat,
            model: "mock".into(),
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
            .await?;
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
            crate::provider::ProviderKind::Mock,
        )?;
        engine
            .steer(
                scope.clone(),
                crate::provider::SteeringInput::User {
                    text: "continue".into(),
                    attachments: Vec::new(),
                    submission: Some(receipt),
                },
            )
            .await?;
        assert!(matches!(
            ready.ticket.commit().await,
            Err(SettlementError::Stale)
        ));
        let (continuation_sink, _events) = crate::provider::events::channel(scope.clone());
        let stale = engine
            .continue_attempt(&ready.ticket, request.clone(), continuation_sink, &store)
            .await?;
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
        let (continuation_sink, _events) = crate::provider::events::channel(scope);
        let next = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            engine.continue_attempt(&current.ticket, request, continuation_sink, &store),
        )
        .await??;
        let ContinuationAttempt::Ready(next) = next else {
            panic!("current continuation must be admitted");
        };
        assert!(next.outcome.is_ok());
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

    /// Idle native work retains its new host effects until host acknowledgment.
    #[tokio::test]
    async fn idle_reopen_retains_host_effect_until_new_commit() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace,
            directory.path().into(),
        )?);
        let owner = ExecutionOwner::root("idle-host-effects");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let submission = SubmissionId::try_from("original".to_owned())?;
        let first = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "first".into(),
        };
        let later = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "later".into(),
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
        let mut committed = HashMap::new();
        committed.insert(
            scope.invocation_id.clone(),
            Invocation {
                scope: scope.clone(),
                output: sink,
                reply: None,
                completion: context,
                terminal: Some(Ok(completed.clone())),
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
            owner,
            provider,
            state,
            invocations: HashMap::new(),
            committed_invocations: committed,
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 1,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            idle: SessionObserver::Inbox(Arc::default()),
        };
        driver.event(ProviderEvent::Execution {
            observation: ExecutionObservation::new(
                scope.clone(),
                ExecutionUpdate::Started {
                    address: later.clone(),
                },
            )?,
        });
        slot.publish(ProviderCompletion {
            plan_question: Some(crate::plan::PlanQuestionSet {
                id: "idle-question".into(),
                questions: Vec::new(),
            }),
            ..Default::default()
        })?;
        driver.event(ProviderEvent::Execution {
            observation: ExecutionObservation::new(
                scope.clone(),
                ExecutionUpdate::Finished {
                    address: later,
                    covered_submissions: Vec::new(),
                    completed_at_ms: 2,
                    result: completed,
                },
            )?,
        });
        let (reply, response) = oneshot::channel();
        driver.command(Command::WaitReady {
            scope: scope.clone(),
            abandoned: Arc::new(AtomicBool::new(false)),
            reply,
        });
        while let Some(delivery) = deliveries.recv().await {
            if let Delivery::Complete { reply, result } = delivery {
                reply.send(result).ok();
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
        assert!(matches!(
            response.await?,
            Err(ProviderError::NotConsumed { .. })
        ));
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
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            Arc::new(crate::trace::TraceStore::open(directory.path())?),
            directory.path().into(),
        )?);
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
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            idle: SessionObserver::Inbox(Arc::default()),
        };
        driver.acceptance(SubmissionOutcome {
            scope: scope.clone(),
            submission: Some(submission),
            control_reply: None,
            result: Err(ProviderError::PreparationFailed(Arc::new(
                ProviderError::Rejected {
                    message: "catalog unavailable".into(),
                },
            ))),
        });
        if let Some(Delivery::Complete { reply, result }) = pending.recv().await {
            reply.send(result).ok();
        }
        let Err(ProviderError::PreparationFailed(cause)) = response.await? else {
            panic!("expected typed preparation failure")
        };
        assert!(
            matches!(cause.as_ref(), ProviderError::Rejected { message } if message == "catalog unavailable")
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
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace,
            directory.path().into(),
        )?);
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
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            idle: SessionObserver::Inbox(Arc::default()),
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
        assert!(
            matches!(report.observation(), ObservationFailure::Protocol { message }
            if message.contains("delivery is uncertain"))
        );
        assert!(report.collection().is_some());
        Ok(())
    }

    /// A typed observation can arrive before the separate acceptance task completes.
    #[tokio::test]
    async fn uncertain_observation_before_acceptance_preserves_typed_error() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace,
            directory.path().into(),
        )?);
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
            owner,
            provider,
            state,
            invocations,
            committed_invocations: HashMap::new(),
            submissions: JoinSet::new(),
            deliveries: JoinSet::new(),
            delivery: Some(delivery),
            cleanup: JoinSet::new(),
            failed_waiters: Vec::new(),
            commands,
            next_ticket_nonce: 0,
            committed_tickets: HashMap::new(),
            failed: false,
            healthy: Arc::new(AtomicBool::new(true)),
            first_failure: None,
            cleanup_failure: None,
            idle: SessionObserver::Inbox(Arc::default()),
        };
        let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::ObservationLost {
                failure: ObservationFailure::DeliveryUncertain {
                    message: "initial acknowledgment expired".into(),
                },
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
        assert!(
            matches!(response.await?, Err(ProviderError::DeliveryUncertain { message }) if message == "initial acknowledgment expired")
        );
        Ok(())
    }

    /// Saturated presentation queues fail completion waiters explicitly instead of dropping them.
    #[tokio::test]
    async fn rejected_completion_resolves_with_observation_failure() {
        let (reply, response) = oneshot::channel();
        Delivery::Complete {
            reply,
            result: Err(protocol("original")),
        }
        .fail(ObservationFailure::Overflow);
        assert!(matches!(
            response.await.unwrap(),
            Err(ProviderError::Observation(ObservationFailure::Overflow))
        ));
    }

    /// Keep a host result pending after acceptance, and reuse one engine for the next exchange.
    #[tokio::test]
    async fn host_waits_for_execution_completion_and_reuses_session_intake() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let provider: Arc<Provider> = Arc::from(crate::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace.clone(),
            directory.path().into(),
        )?);
        let store = crate::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let owner = ExecutionOwner::root("engine-fixture");
        let registry = SessionExecutionRegistry::new(provider.clone());
        let engine = registry.owner(owner.clone()).await?;
        assert!(Arc::ptr_eq(&engine, &registry.owner(owner.clone()).await?));
        let mut idle = engine.session_events().await?;
        let mut retained_scope = None;
        for exchange in ["one", "two"] {
            let request = ProviderRequest {
                execution_owner: Some(owner.clone()),
                harness_session_id: "engine-fixture".into(),
                workspace: directory.path().to_string_lossy().into_owned(),
                input: ProviderInput::from_text("hello"),
                mode: PromptMode::Chat,
                model: "mock".into(),
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
            let control = provider.mock_response_control().unwrap();
            let hold = control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Hold {
                        mode: Some(PromptMode::Chat),
                        any_session: false,
                    },
                )
                .await?;
            let hold_id = hold["id"].as_str().unwrap().to_owned();
            let scope = InvocationScope::new(owner.clone(), exchange);
            retained_scope = Some(scope.clone());
            let (sink, mut events) = crate::provider::events::channel(scope);
            let running = engine.clone();
            let store = store.clone();
            let mut result =
                tokio::spawn(async move { running.run_once(request, sink, &store).await });
            control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Wait {
                        id: hold_id.clone(),
                    },
                )
                .await?;
            assert!(
                !result.is_finished(),
                "acceptance was mistaken for execution completion"
            );
            control
                .dispatch(
                    "engine-fixture",
                    MockResponseCommand::Release { id: hold_id },
                )
                .await?;
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                loop {
                    tokio::select! {
                        value = &mut result => { value??; return Ok::<_, anyhow::Error>(()); }
                        event = events.recv() => { event?; }
                    }
                }
            })
            .await??;
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
            crate::provider::ProviderKind::Mock,
        )?;
        let input = SubmissionId::try_from(receipt.id().to_owned())?;
        engine
            .steer(
                scope.clone(),
                crate::provider::SteeringInput::User {
                    text: "continue original work".into(),
                    attachments: Vec::new(),
                    submission: Some(receipt),
                },
            )
            .await?;
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            let mut started = false;
            loop {
                if let Some(ProviderEvent::Execution { observation }) = idle.recv().await? {
                    assert_eq!(observation.scope(), &scope);
                    match observation.update() {
                        ExecutionUpdate::Started { .. } => started = true,
                        ExecutionUpdate::Finished {
                            covered_submissions,
                            ..
                        } => {
                            assert!(started);
                            assert!(covered_submissions.contains(&input));
                            return Ok::<_, anyhow::Error>(());
                        }
                        ExecutionUpdate::ObservationLost { failure, .. } => {
                            anyhow::bail!("idle continuation lost observation: {failure}")
                        }
                        ExecutionUpdate::ResolvedWithoutExecution { .. } => {
                            anyhow::bail!("mock continuation unexpectedly had no native execution")
                        }
                    }
                }
            }
        })
        .await??;

        registry.close().await?;
        registry.close().await?;
        assert!(
            registry
                .owner(ExecutionOwner::root("closed"))
                .await
                .is_err()
        );
        provider.shutdown().await?;
        Ok(())
    }
}
