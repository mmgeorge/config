//! Typed operation ownership shared by native adapters and normalized fixtures.

use std::{
    collections::HashMap,
    future::Future,
    panic::AssertUnwindSafe,
    sync::{Arc, Mutex},
};
use tokio::{
    sync::{Mutex as AsyncMutex, watch},
    task::JoinHandle,
};

use crate::{
    FailureDisposition, OperationContext, OperationId, OperationInterruption, ProviderError,
    ProviderFailure, ProviderFailureKind, ProviderOperation, ProviderPhase,
};

/// Keep a provisional operation observation separate from the native owner's concurrently polled cleanup.
/// The observation must describe the owner's actual dispatch phase and known effects.
/// Successful cleanup does not manufacture native settlement or undo mutation.
pub struct Interruption<Cleanup> {
    observation: ProviderError,
    cleanup: Cleanup,
}

impl<Cleanup> Interruption<Cleanup> {
    /// Create an interruption plan whose cleanup remains owned after every waiter disappears.
    /// The caller supplies actual interruption/revocation/collection work for the exact target.
    /// A successful cleanup future alone is not native terminal evidence or replay permission.
    pub fn new(observation: ProviderError, cleanup: Cleanup) -> Self {
        Self {
            observation,
            cleanup,
        }
    }
}

/// Retain one typed domain's immutable requests, native workers, and authoritative outcomes.
///
/// Reusing an ID compares its complete context, requested operation, payload, and observation
/// mode before any admission check. Completed outcomes therefore remain observable after
/// expiry. Dropping an observer or cancellation source never cancels work. The native owner
/// supplies real interruption cleanup and must check admission again immediately before its
/// first mutation. The interruption callback is not a finalizer: completed work wins observation,
/// including a native future which itself returns a cancellation error. Every worker error path
/// must therefore revoke unsafe authority and collect or retain its exact resources independently.
/// Work and interruption cleanup run concurrently, so neither blocks the other's progress.
/// Build the work future without starting effects; only an admitted record polls it. Store weak
/// request identity snapshots for authority-bearing requests, allowing normal collection to
/// release their leases and callback owners. Records and identity namespaces are in-process;
/// they do not establish crash-persistent exactly-once native mutation.
pub struct Store<Payload, Output> {
    state: Mutex<StoreState<Payload, Output>>,
    identities: Arc<IdentityRegistry>,
    domain: Arc<()>,
}

impl<Payload, Output> Store<Payload, Output>
where
    Payload: Eq + Send + Sync + 'static,
    Output: Send + 'static,
{
    /// Create an admitting store with an independent identity namespace and no native resources.
    /// Use registered with one shared IdentityRegistry for all semantic domains of a provider;
    /// independent namespaces are appropriate only for independently owned providers or fixtures.
    pub fn new() -> Self {
        Self::registered(Arc::new(IdentityRegistry::new()))
    }

    /// Create a typed domain store sharing its provider's operation identity namespace.
    /// The registry rejects cross-domain reuse without erasing or downcasting this store's payload.
    pub fn registered(identities: Arc<IdentityRegistry>) -> Self {
        Self {
            state: Mutex::new(StoreState {
                admitting: true,
                records: HashMap::new(),
            }),
            identities,
            domain: Arc::new(()),
        }
    }

    /// Observe a repeatable result while the store retains its independent worker and cleanup.
    /// An interruption is provisional: a later exact observation obtains the authoritative
    /// native result once known, without dispatching a second mutation. Changed context, operation,
    /// payload, or observation mode rejects as OperationConflict before work is polled. Exact
    /// retained outcomes remain observable after expiry or admission closure.
    pub async fn observe<Work, Handler, Cleanup>(
        &self,
        context: OperationContext,
        operation: ProviderOperation,
        payload: Payload,
        work: Work,
        on_interrupt: Handler,
    ) -> Result<Output, ProviderError>
    where
        Output: Clone,
        Work: Future<Output = Result<Output, ProviderError>> + Send + 'static,
        Handler: FnOnce(OperationInterruption) -> Interruption<Cleanup> + Send + 'static,
        Cleanup: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        let record = self.record(
            context,
            operation,
            payload,
            ObservationMode::Repeatable,
            work,
            on_interrupt,
        )?;
        let mut changed = record.changed.subscribe();
        loop {
            if let Some(result) = record.observe() {
                return result;
            }
            changed
                .changed()
                .await
                .expect("retained operation record owns its notification sender");
        }
    }

    /// Transfer a successful owned result exactly once, retaining errors for repeat observation.
    /// An exact second transfer reports OperationResultTaken, including after expiry; it never
    /// opens a replacement session or creates another reader. Dropping a pending waiter retains
    /// the untransferred result and work; another exact waiter may perform the sole transfer.
    pub async fn take<Work, Handler, Cleanup>(
        &self,
        context: OperationContext,
        operation: ProviderOperation,
        payload: Payload,
        work: Work,
        on_interrupt: Handler,
    ) -> Result<Output, ProviderError>
    where
        Work: Future<Output = Result<Output, ProviderError>> + Send + 'static,
        Handler: FnOnce(OperationInterruption) -> Interruption<Cleanup> + Send + 'static,
        Cleanup: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        let record = self.record(
            context,
            operation,
            payload,
            ObservationMode::TakeOnce,
            work,
            on_interrupt,
        )?;
        let mut changed = record.changed.subscribe();
        loop {
            if let Some(result) = record.take() {
                return result;
            }
            changed
                .changed()
                .await
                .expect("retained operation record owns its notification sender");
        }
    }

    /// Stop all new operations in this typed store while preserving exact cached observations.
    /// This local gate has no lifecycle exception. Close the shared IdentityRegistry instead
    /// when fresh cancel, close, or shutdown contexts must remain legal for cleanup repair.
    pub fn close_admission(&self) {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .admitting = false;
    }

    /// Join every retained worker after its native owner has stopped resource producers.
    /// Dropping this waiter preserves each join handle for the next collection attempt.
    /// Success proves these futures joined, not that native resources were released. The
    /// owner must inspect and repair retained resources separately. Historical operation and
    /// cleanup failures remain in their exact operation outcomes and do not poison later repair.
    pub async fn join_all(&self) -> Result<(), ProviderError> {
        self.join_selected(None).await
    }

    /// Join retained workers while excluding the shutdown operation which owns this await.
    /// As with join_all, success proves future ownership was joined, not native resource release;
    /// historical operation failures remain accessible through their exact recorded outcomes.
    pub async fn join_all_except(&self, operation: &OperationId) -> Result<(), ProviderError> {
        self.join_selected(Some(operation)).await
    }

    /// Install native ownership synchronously before publishing an operation to its observers.
    fn record<Work, Handler, Cleanup>(
        &self,
        context: OperationContext,
        operation: ProviderOperation,
        payload: Payload,
        mode: ObservationMode,
        work: Work,
        on_interrupt: Handler,
    ) -> Result<Arc<Record<Payload, Output>>, ProviderError>
    where
        Work: Future<Output = Result<Output, ProviderError>> + Send + 'static,
        Handler: FnOnce(OperationInterruption) -> Interruption<Cleanup> + Send + 'static,
        Cleanup: Future<Output = Result<(), ProviderError>> + Send + 'static,
    {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(record) = state.records.get(context.id()) {
            if record.context != context
                || record.operation != operation
                || record.payload != payload
                || record.mode != mode
            {
                return Err(failure(
                    operation,
                    ProviderFailureKind::OperationConflict,
                    ProviderPhase::Admission,
                    FailureDisposition::Unchanged,
                ));
            }
            return Ok(record.clone());
        }
        self.identities
            .reserve(&context, operation, mode, &self.domain)?;
        let admission = if state.admitting {
            context.check_admission(operation)
        } else {
            Err(failure(
                operation,
                ProviderFailureKind::BindingClosed,
                ProviderPhase::Admission,
                FailureDisposition::Invalidated,
            ))
        };
        let admitted = admission.is_ok();
        let record = Arc::new(Record {
            context: context.clone(),
            operation,
            payload,
            mode,
            outcome: Mutex::new(match admission.as_ref() {
                Ok(()) => Outcome::Pending { observation: None },
                Err(error) => Outcome::Ready(Err(error.clone())),
            }),
            changed: watch::channel(()).0,
            worker: AsyncMutex::new(match admission {
                Ok(()) => WorkerState::Starting,
                Err(_) => WorkerState::Collected(Ok(())),
            }),
        });
        if admitted {
            let retained = record.clone();
            let worker = tokio::spawn(async move { supervise(retained, work, on_interrupt).await });
            *record
                .worker
                .try_lock()
                .expect("unpublished operation has no concurrent collector") =
                WorkerState::Running(worker);
        }
        state.records.insert(context.id().clone(), record.clone());
        Ok(record)
    }

    /// Snapshot retained ownership before waiting so workers can publish their completion.
    async fn join_selected(&self, excluded: Option<&OperationId>) -> Result<(), ProviderError> {
        let records = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .records
            .iter()
            .filter(|(id, _)| excluded != Some(*id))
            .map(|(_, record)| record.clone())
            .collect::<Vec<_>>();
        let mut failures = Vec::new();
        for record in records {
            if let Err(error) = record.join().await {
                failures.push(error);
            }
        }
        match failures.split_first() {
            None => Ok(()),
            Some((primary, [])) => Err(primary.clone()),
            Some((primary, secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary.clone()),
                secondary: secondary.to_vec(),
            }),
        }
    }
}

/// Bind operation identities across independently typed stores without erasing their payloads.
/// Each provider or fixture owns one shared registry. Closing its admission preserves exact
/// outcomes already retained by individual stores. Exact cancellation, binding closure, and
/// provider shutdown remain admissible so failed cleanup can be retried with a fresh context.
pub struct IdentityRegistry {
    state: Mutex<IdentityState>,
}

impl IdentityRegistry {
    /// Create an empty admitting namespace for one provider's complete operation lifetime.
    pub fn new() -> Self {
        Self {
            state: Mutex::new(IdentityState {
                admitting: true,
                operations: HashMap::new(),
            }),
        }
    }

    /// Stop ordinary admission while keeping fresh cancellation and cleanup identities legal.
    pub fn close_admission(&self) {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .admitting = false;
    }

    /// Reject reuse across domains while payload equality remains with its typed store.
    fn reserve(
        &self,
        context: &OperationContext,
        operation: ProviderOperation,
        mode: ObservationMode,
        domain: &Arc<()>,
    ) -> Result<(), ProviderError> {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(original) = state.operations.get(context.id()) {
            if &original.context != context
                || original.operation != operation
                || original.mode != mode
                || !Arc::ptr_eq(&original.domain, domain)
            {
                return Err(failure(
                    operation,
                    ProviderFailureKind::OperationConflict,
                    ProviderPhase::Admission,
                    FailureDisposition::Unchanged,
                ));
            }
            return Ok(());
        }
        if !state.admitting
            && !matches!(
                operation,
                ProviderOperation::TurnInterrupt
                    | ProviderOperation::ThreadClose
                    | ProviderOperation::Shutdown
            )
        {
            return Err(failure(
                operation,
                ProviderFailureKind::BindingClosed,
                ProviderPhase::Admission,
                FailureDisposition::Invalidated,
            ));
        }
        state.operations.insert(
            context.id().clone(),
            IdentityRecord {
                context: context.clone(),
                operation,
                mode,
                domain: domain.clone(),
            },
        );
        Ok(())
    }
}

impl Default for IdentityRegistry {
    /// Create a new provider operation namespace.
    fn default() -> Self {
        Self::new()
    }
}

impl<Payload, Output> Default for Store<Payload, Output>
where
    Payload: Eq + Send + Sync + 'static,
    Output: Send + 'static,
{
    /// Create an admitting typed store with no native work.
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ObservationMode {
    Repeatable,
    TakeOnce,
}

enum Outcome<Output> {
    Pending { observation: Option<ProviderError> },
    Ready(Result<Output, ProviderError>),
    Taken,
}

enum WorkerState {
    Starting,
    Running(JoinHandle<Result<(), ProviderError>>),
    Collected(Result<(), ProviderError>),
}

struct StoreState<Payload, Output> {
    admitting: bool,
    records: HashMap<OperationId, Arc<Record<Payload, Output>>>,
}

struct IdentityState {
    admitting: bool,
    operations: HashMap<OperationId, IdentityRecord>,
}

struct IdentityRecord {
    context: OperationContext,
    operation: ProviderOperation,
    mode: ObservationMode,
    domain: Arc<()>,
}

struct Record<Payload, Output> {
    context: OperationContext,
    operation: ProviderOperation,
    payload: Payload,
    mode: ObservationMode,
    outcome: Mutex<Outcome<Output>>,
    changed: watch::Sender<()>,
    worker: AsyncMutex<WorkerState>,
}

impl<Payload, Output> Record<Payload, Output> {
    /// Read authoritative completion before any provisional interruption observation.
    fn observe(&self) -> Option<Result<Output, ProviderError>>
    where
        Output: Clone,
    {
        match &*self
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
        {
            Outcome::Pending { observation } => observation.clone().map(Err),
            Outcome::Ready(result) => Some(result.clone()),
            Outcome::Taken => Some(Err(failure(
                self.operation,
                ProviderFailureKind::OperationResultTaken,
                ProviderPhase::Observation,
                FailureDisposition::Unchanged,
            ))),
        }
    }

    /// Move an owned success once while retaining exact errors and provisional observations.
    fn take(&self) -> Option<Result<Output, ProviderError>> {
        let mut outcome = self
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &*outcome {
            Outcome::Pending { observation } => return observation.clone().map(Err),
            Outcome::Ready(Err(error)) => return Some(Err(error.clone())),
            Outcome::Taken => {
                return Some(Err(failure(
                    self.operation,
                    ProviderFailureKind::OperationResultTaken,
                    ProviderPhase::Observation,
                    FailureDisposition::Unchanged,
                )));
            }
            Outcome::Ready(Ok(_)) => {}
        }
        match std::mem::replace(&mut *outcome, Outcome::Taken) {
            Outcome::Ready(result) => Some(result),
            Outcome::Pending { .. } | Outcome::Taken => {
                unreachable!("owned result was checked under the same outcome lock")
            }
        }
    }

    /// Publish bounded observer interruption without replacing an authoritative result.
    fn interrupt(&self, observation: ProviderError) {
        let mut outcome = self
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Outcome::Pending {
            observation: pending,
        } = &mut *outcome
        {
            *pending = Some(observation.for_operation(self.operation));
        }
        drop(outcome);
        self.changed.send_replace(());
    }

    /// Publish the native owner's proven result after its concurrent cleanup has finished.
    fn complete(&self, result: Result<Output, ProviderError>) {
        *self
            .outcome
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Outcome::Ready(result);
        self.changed.send_replace(());
    }

    /// Keep the supervisor join handle retained even if a collector stops awaiting it.
    async fn join(&self) -> Result<(), ProviderError> {
        let mut worker = self.worker.lock().await;
        let result = match &mut *worker {
            WorkerState::Starting => {
                unreachable!("operation is published after its supervisor handle is installed")
            }
            WorkerState::Running(worker) => worker.await.unwrap_or_else(|_| {
                Err(failure(
                    self.operation,
                    ProviderFailureKind::Runtime,
                    ProviderPhase::Collection,
                    FailureDisposition::Invalidated,
                ))
            }),
            WorkerState::Collected(result) => return result.clone(),
        };
        *worker = WorkerState::Collected(result.clone());
        result
    }
}

/// Poll the original native worker and any real cleanup independently until both are collected.
async fn supervise<Payload, Output, Work, Handler, Cleanup>(
    record: Arc<Record<Payload, Output>>,
    work: Work,
    on_interrupt: Handler,
) -> Result<(), ProviderError>
where
    Payload: Eq + Send + Sync + 'static,
    Output: Send + 'static,
    Work: Future<Output = Result<Output, ProviderError>> + Send + 'static,
    Handler: FnOnce(OperationInterruption) -> Interruption<Cleanup> + Send + 'static,
    Cleanup: Future<Output = Result<(), ProviderError>> + Send + 'static,
{
    let mut worker = tokio::spawn(work);
    let (native, cleanup, unexpected) = tokio::select! {
        biased;
        result = &mut worker => (result, Ok(()), false),
        reason = record.context.interrupted() => {
            match std::panic::catch_unwind(AssertUnwindSafe(|| on_interrupt(reason))) {
                Ok(Interruption { observation, cleanup }) => {
                    record.interrupt(observation);
                    let cleanup = tokio::spawn(cleanup);
                    let (native, cleanup) = tokio::join!(&mut worker, cleanup);
                    let unexpected = cleanup.is_err();
                    let cleanup = cleanup.unwrap_or_else(|_| Err(failure(record.operation, ProviderFailureKind::Runtime, ProviderPhase::Collection, FailureDisposition::Invalidated)));
                    (native, cleanup, unexpected)
                }
                Err(_) => {
                    let failure = failure(record.operation, ProviderFailureKind::Runtime, ProviderPhase::Interruption, FailureDisposition::Invalidated);
                    record.interrupt(failure.clone());
                    ((&mut worker).await, Err(failure), true)
                }
            }
        }
    };
    let native_panicked = native.is_err();
    let native = native.unwrap_or_else(|_| {
        Err(failure(
            record.operation,
            ProviderFailureKind::Runtime,
            ProviderPhase::Collection,
            FailureDisposition::Invalidated,
        ))
    });
    let result = match (native, &cleanup) {
        (Ok(output), Ok(())) => Ok(output),
        (Err(primary), Ok(())) => Err(primary),
        (Ok(_), Err(error)) => Err(error.clone()),
        (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![secondary.clone()],
        }),
    };
    record.complete(result.map_err(|error| error.for_operation(record.operation)));
    if unexpected || native_panicked {
        return Err(failure(
            record.operation,
            ProviderFailureKind::Runtime,
            ProviderPhase::Collection,
            FailureDisposition::Invalidated,
        ));
    }
    Ok(())
}

/// Construct safe lifecycle diagnostics without retaining native payloads or panic text.
fn failure(
    operation: ProviderOperation,
    kind: ProviderFailureKind,
    phase: ProviderPhase,
    disposition: FailureDisposition,
) -> ProviderError {
    ProviderFailure::new(operation, kind)
        .at_phase(phase, disposition)
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OperationCancellation, OperationCancellationSource};
    use std::{
        sync::atomic::{AtomicUsize, Ordering},
        time::Duration,
    };

    /// Create one bounded host operation whose clones preserve identical cancellation authority.
    fn context(id: &str) -> (OperationCancellationSource, OperationContext) {
        let (source, cancellation) = OperationCancellation::channel();
        (
            source,
            OperationContext::new(
                OperationId::new(id).unwrap(),
                Duration::from_secs(2),
                cancellation,
            )
            .unwrap(),
        )
    }

    /// Preserve uncertainty while a native owner retains its already dispatched work.
    fn interrupted(
        reason: OperationInterruption,
    ) -> Interruption<impl Future<Output = Result<(), ProviderError>> + Send> {
        Interruption::new(
            reason.failure(
                ProviderOperation::ThreadFork,
                ProviderPhase::Observation,
                FailureDisposition::Uncertain,
            ),
            async { Ok(()) },
        )
    }

    /// Retain native work across dropped observers and resolve exact expired replay from evidence.
    #[tokio::test]
    async fn dropped_observer_does_not_cancel_or_repeat_native_work() {
        let store = Arc::new(Store::new());
        let (source, operation) = context("retained");
        let (entered, entry) = tokio::sync::oneshot::channel();
        let (release, released) = tokio::sync::oneshot::channel();
        let count = Arc::new(AtomicUsize::new(0));
        let native_count = count.clone();
        let interruptions = Arc::new(AtomicUsize::new(0));
        let observed_interruptions = interruptions.clone();
        let owner = store.clone();
        let observed_context = operation.clone();
        let observer = tokio::spawn(async move {
            owner
                .observe(
                    observed_context,
                    ProviderOperation::ThreadFork,
                    1_u8,
                    async move {
                        native_count.fetch_add(1, Ordering::AcqRel);
                        let _ = entered.send(());
                        let _ = released.await;
                        Ok(7_u8)
                    },
                    move |reason| {
                        observed_interruptions.fetch_add(1, Ordering::AcqRel);
                        interrupted(reason)
                    },
                )
                .await
        });
        entry.await.unwrap();
        observer.abort();
        drop(source);
        release.send(()).unwrap();
        store.join_all().await.unwrap();
        let result = store
            .observe(
                operation,
                ProviderOperation::ThreadFork,
                1_u8,
                async { panic!("exact replay dispatched twice") },
                interrupted,
            )
            .await
            .unwrap();
        assert_eq!(result, 7);
        assert_eq!(count.load(Ordering::Acquire), 1);
        assert_eq!(interruptions.load(Ordering::Acquire), 0);
    }
}
