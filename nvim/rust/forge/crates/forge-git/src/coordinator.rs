use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, ensure};

use crate::mutation::{MutationScope, OperationCompletion, OperationId};

const MAX_OPERATION_SCOPES: usize = 64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperationState {
    Queued,
    Running { cancellation_requested: bool },
    Abandoned,
    Finished(OperationCompletion),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AdmissionUsage {
    pub operations: usize,
    pub input_bytes: usize,
}

struct Operation {
    scopes: Vec<MutationScope>,
    state: OperationState,
    waiting: bool,
    input_bytes: usize,
    quarantined: bool,
}

struct CoordinatorState {
    operations: BTreeMap<OperationId, Operation>,
    next_id: u64,
    closed: bool,
    input_bytes: usize,
}

/// Bounds admitted operations and retained receipts while preserving order on intersecting scopes.
pub struct MutationCoordinator {
    state: Arc<Mutex<CoordinatorState>>,
    capacity: usize,
    input_capacity: usize,
    changed: tokio::sync::watch::Sender<()>,
}

/// Remains with the process owner until termination is known. Drop quarantines its scopes.
pub struct AdmissionGuard {
    state: Arc<Mutex<CoordinatorState>>,
    id: OperationId,
    finished: bool,
    changed: tokio::sync::watch::Sender<()>,
}

struct QueuedWait {
    state: Arc<Mutex<CoordinatorState>>,
    changed: tokio::sync::watch::Sender<()>,
    id: OperationId,
    active: bool,
}

impl MutationCoordinator {
    pub fn new(capacity: usize, input_capacity: usize) -> Result<Self> {
        ensure!(capacity > 0, "mutation admission capacity must be positive");
        ensure!(
            input_capacity > 0,
            "mutation input capacity must be positive"
        );
        Ok(Self {
            state: Arc::new(Mutex::new(CoordinatorState {
                operations: BTreeMap::new(),
                next_id: 0,
                closed: false,
                input_bytes: 0,
            })),
            capacity,
            input_capacity,
            changed: tokio::sync::watch::channel(()).0,
        })
    }

    /// Reserves queue, receipt, and declared retained input capacity before preparation allocates.
    /// Callers must include retained patch, path, and precondition inputs in the declared byte bound.
    pub fn admit(&self, scopes: Vec<MutationScope>, input_bytes: usize) -> Result<OperationId> {
        ensure!(
            !scopes.is_empty() && scopes.len() <= MAX_OPERATION_SCOPES,
            "mutation scope count must be between 1 and {MAX_OPERATION_SCOPES}"
        );
        for (index, scope) in scopes.iter().enumerate() {
            ensure!(!scopes[..index].contains(scope), "duplicate mutation scope");
        }
        let mut state = self.state.lock().expect("mutation coordinator lock");
        ensure!(!state.closed, "mutation admission is closed");
        ensure!(!state.operations.values().any(|previous| previous.quarantined && previous.scopes.iter().any(|scope| scopes.contains(scope))), "repository writes require read-only reconciliation");
        ensure!(
            state.operations.len() < self.capacity,
            "mutation admission is full"
        );
        ensure!(
            input_bytes <= self.input_capacity - state.input_bytes,
            "mutation retained input budget is full"
        );
        let id = OperationId::from_value(state.next_id)?;
        state.next_id = state
            .next_id
            .checked_add(1)
            .context("mutation identity exhausted")?;
        state.operations.insert(
            id,
            Operation {
                scopes,
                state: OperationState::Queued,
                waiting: false,
                input_bytes,
                quarantined: false,
            },
        );
        state.input_bytes += input_bytes;
        Ok(id)
    }

    pub fn usage(&self) -> AdmissionUsage {
        let state = self.state.lock().expect("mutation coordinator lock");
        AdmissionUsage {
            operations: state.operations.len(),
            input_bytes: state.input_bytes,
        }
    }

    pub fn resize(&self, id: OperationId, input_bytes: usize) -> Result<()> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        let operation = state.operations.get(&id).context("unknown queued mutation")?;
        ensure!(operation.state == OperationState::Queued, "running mutation cannot change its input reservation");
        let retained = state.input_bytes - operation.input_bytes;
        ensure!(input_bytes <= self.input_capacity - retained, "mutation retained input budget is full");
        state.operations.get_mut(&id).expect("validated mutation").input_bytes = input_bytes;
        state.input_bytes = retained + input_bytes;
        Ok(())
    }

    /// Waits for earlier writers before capturing disk preconditions without starting this write.
    pub async fn wait_ready(&self, id: OperationId) -> Result<()> {
        let mut changed = self.changed.subscribe();
        loop {
            let blocked = {
                let state = self.state.lock().expect("mutation coordinator lock");
                let operation = state.operations.get(&id).context("unknown queued mutation")?;
                ensure!(operation.state == OperationState::Queued, "mutation is no longer queued");
                let earlier: Vec<_> = state.operations.range(..id).filter(|(_, previous)| previous.scopes.iter().any(|scope| operation.scopes.contains(scope))).collect();
                ensure!(!earlier.iter().any(|(_, previous)| previous.quarantined), "repository writes require read-only reconciliation");
                earlier.iter().any(|(_, previous)| !matches!(previous.state, OperationState::Finished(_)))
            };
            if !blocked { return Ok(()); }
            changed.changed().await.context("mutation coordinator closed during preparation")?;
        }
    }

    /// Returns no guard while an earlier intersecting operation owns queue order or execution.
    /// The writer must validate source preconditions after obtaining this guard and before Git starts.
    pub fn start(&self, id: OperationId) -> Result<Option<AdmissionGuard>> {
        self.try_start(id, false)
    }

    /// Registers one queue owner immediately, including before the returned future is first polled.
    /// Dropping the future cancels queued work and leaves a terminal receipt for explicit adoption.
    pub fn wait_start(
        &self,
        id: OperationId,
    ) -> Result<impl std::future::Future<Output = Result<AdmissionGuard>> + Send + '_> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        let operation = state.operations.get_mut(&id).context("unknown mutation")?;
        ensure!(
            operation.state == OperationState::Queued && !operation.waiting,
            "mutation already has a queue owner or is not queued"
        );
        operation.waiting = true;
        drop(state);
        let mut changed = self.changed.subscribe();
        let mut owner = QueuedWait {
            state: Arc::clone(&self.state),
            changed: self.changed.clone(),
            id,
            active: true,
        };
        Ok(async move {
            loop {
                if let Some(guard) = self.try_start(id, true)? {
                    owner.active = false;
                    // Capture the complete queue owner so future cancellation drops its registration.
                    drop(owner);
                    return Ok(guard);
                }
                changed
                    .changed()
                    .await
                    .context("mutation coordinator notifications closed")?;
            }
        })
    }

    fn try_start(&self, id: OperationId, waiting: bool) -> Result<Option<AdmissionGuard>> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        let operation = state.operations.get(&id).context("unknown mutation")?;
        ensure!(
            operation.state == OperationState::Queued,
            "mutation is not queued"
        );
        ensure!(
            operation.waiting == waiting,
            "mutation has another queue owner"
        );
        let blocked = state.operations.range(..id).any(|(_, previous)| {
            (previous.quarantined || !matches!(previous.state, OperationState::Finished(_)))
                && previous
                    .scopes
                    .iter()
                    .any(|scope| operation.scopes.contains(scope))
        });
        if blocked {
            return Ok(None);
        }
        state.operations.get_mut(&id).expect("known mutation").state = OperationState::Running {
            cancellation_requested: false,
        };
        Ok(Some(AdmissionGuard {
            state: Arc::clone(&self.state),
            id,
            finished: false,
            changed: self.changed.clone(),
        }))
    }

    pub fn state(&self, id: OperationId) -> Option<OperationState> {
        self.state
            .lock()
            .expect("mutation coordinator lock")
            .operations
            .get(&id)
            .map(|operation| operation.state)
    }

    /// Cancels queued work immediately. Running owners retain their guard until termination is known.
    pub fn cancel(&self, id: OperationId) -> Result<OperationState> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        let operation = state.operations.get_mut(&id).context("unknown mutation")?;
        request_cancellation(operation);
        self.changed.send_replace(());
        Ok(operation.state)
    }

    /// Removes only terminal receipts, allowing callers to acknowledge durable adoption explicitly.
    pub fn take_receipt(&self, id: OperationId) -> Option<OperationCompletion> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        if state.operations.get(&id)?.quarantined { return None; }
        let OperationState::Finished(completion) = state.operations.get(&id)?.state else {
            return None;
        };
        let operation = state
            .operations
            .remove(&id)
            .expect("known terminal mutation");
        state.input_bytes -= operation.input_bytes;
        Some(completion)
    }

    pub fn quarantined(&self, scopes: &[MutationScope]) -> Vec<OperationId> {
        self.state.lock().expect("mutation coordinator lock").operations.iter()
            .filter(|(_, operation)| operation.quarantined && operation.scopes.iter().any(|scope| scopes.contains(scope)))
            .map(|(id, _)| *id).collect()
    }

    pub fn reconciled(&self, operation: &[OperationId]) {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        for id in operation { if let Some(operation) = state.operations.get_mut(id) { operation.quarantined = false; } }
        self.changed.send_replace(());
    }

    /// Closes admission and requests cancellation without releasing running or abandoned scopes.
    pub fn close(&self) -> Vec<OperationId> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        state.closed = true;
        for operation in state.operations.values_mut() {
            request_cancellation(operation);
        }
        self.changed.send_replace(());
        state
            .operations
            .iter()
            .filter_map(|(id, operation)| {
                (!matches!(operation.state, OperationState::Finished(_))).then_some(*id)
            })
            .collect()
    }
}

impl AdmissionGuard {
    pub fn finish_quarantined(mut self, completion: OperationCompletion) -> Result<()> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        let operation = state.operations.get_mut(&self.id).context("unknown running mutation")?;
        operation.state = OperationState::Finished(completion);
        operation.quarantined = true;
        self.finished = true;
        self.changed.send_replace(());
        Ok(())
    }

    /// Waits for cancellation without polling. The owner must still terminate and reap its child.
    pub async fn cancelled(&self) -> Result<()> {
        let mut changed = self.changed.subscribe();
        while !self.cancellation_requested() {
            changed
                .changed()
                .await
                .context("mutation coordinator notifications closed")?;
        }
        Ok(())
    }

    pub fn cancellation_requested(&self) -> bool {
        matches!(
            self.state
                .lock()
                .expect("mutation coordinator lock")
                .operations[&self.id]
                .state,
            OperationState::Running {
                cancellation_requested: true
            }
        )
    }

    /// Records known process termination before releasing scope ordering.
    pub fn finish(mut self, completion: OperationCompletion) -> Result<()> {
        ensure!(
            completion != OperationCompletion::CancelledBeforeStart,
            "a started mutation cannot finish as cancelled before start"
        );
        let mut state = self.state.lock().expect("mutation coordinator lock");
        state
            .operations
            .get_mut(&self.id)
            .context("unknown running mutation")?
            .state = OperationState::Finished(completion);
        self.finished = true;
        self.changed.send_replace(());
        Ok(())
    }
}

impl Drop for AdmissionGuard {
    fn drop(&mut self) {
        if !self.finished {
            let mut state = self.state.lock().expect("mutation coordinator lock");
            if let Some(operation) = state.operations.get_mut(&self.id) {
                operation.state = OperationState::Abandoned;
                self.changed.send_replace(());
            }
        }
    }
}

fn request_cancellation(operation: &mut Operation) {
    match operation.state {
        OperationState::Queued => {
            operation.state = OperationState::Finished(OperationCompletion::CancelledBeforeStart)
        }
        OperationState::Running { .. } => {
            operation.state = OperationState::Running {
                cancellation_requested: true,
            }
        }
        _ => {}
    }
}

impl Drop for QueuedWait {
    fn drop(&mut self) {
        if self.active {
            let mut state = self.state.lock().expect("mutation coordinator lock");
            if let Some(operation) = state.operations.get_mut(&self.id)
                && operation.state == OperationState::Queued
            {
                request_cancellation(operation);
                self.changed.send_replace(());
            }
        }
    }
}
