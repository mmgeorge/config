use std::collections::{BTreeMap, VecDeque};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use anyhow::{Context, Result, ensure};

use crate::mutation::{MutationScope, OperationCompletion, OperationId};

const MAX_OPERATION_SCOPES: usize = 64;
const TRACE_CAPACITY: usize = 1024;
const TRACE_PAGE: usize = 128;

#[derive(Clone, serde::Serialize)]
/// Metadata-only write lifecycle record. Identities are local to one host coordinator.
pub struct MutationTrace {
    sequence: u64,
    operation_id: u64,
    operation: &'static str,
    phase: &'static str,
    elapsed_us: u64,
    phase_us: u64,
    file_count: usize,
    command_count: usize,
    blocker_id: Option<u64>,
    blocker_operation: Option<&'static str>,
    blocker_phase: Option<&'static str>,
}

#[derive(serde::Serialize)]
/// Bounded trace page. `dropped` counts records overwritten before the requested cursor.
pub struct MutationTracePage {
    sequence: u64,
    dropped: u64,
    events: Vec<MutationTrace>,
}

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
    admitted: Instant,
    phase_started: Instant,
    phase: &'static str,
    kind: &'static str,
    file_count: usize,
    command_count: usize,
    blocker: Option<OperationId>,
}

struct CoordinatorState {
    operations: BTreeMap<OperationId, Operation>,
    next_id: u64,
    closed: bool,
    input_bytes: usize,
    trace: VecDeque<MutationTrace>,
    trace_sequence: u64,
}

impl CoordinatorState {
    fn record(&mut self, id: OperationId, phase: &'static str, blocker: Option<OperationId>) {
        let Some(operation) = self.operations.get(&id) else { return };
        let now = Instant::now();
        let blocked_by = blocker.and_then(|blocker| self.operations.get(&blocker));
        self.trace_sequence = self.trace_sequence.saturating_add(1);
        let event = MutationTrace {
            sequence: self.trace_sequence,
            operation_id: id.value(), operation: operation.kind, phase,
            elapsed_us: now.duration_since(operation.admitted).as_micros() as u64,
            phase_us: now.duration_since(operation.phase_started).as_micros() as u64,
            file_count: operation.file_count, command_count: operation.command_count,
            blocker_id: blocker.map(OperationId::value),
            blocker_operation: blocked_by.map(|previous| previous.kind),
            blocker_phase: blocked_by.map(|previous| previous.phase),
        };
        if self.trace.len() == TRACE_CAPACITY { self.trace.pop_front(); }
        self.trace.push_back(event);
        if blocker.is_none() {
            let operation = self.operations.get_mut(&id).expect("recorded operation");
            operation.phase = phase;
            operation.phase_started = now;
        }
    }
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
                trace: VecDeque::new(),
                trace_sequence: 0,
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
                admitted: Instant::now(),
                phase_started: Instant::now(),
                phase: "admitted",
                kind: "unknown",
                file_count: 0,
                command_count: 0,
                blocker: None,
            },
        );
        state.input_bytes += input_bytes;
        Ok(id)
    }

    /// Labels an admitted operation without retaining paths, commands, or message contents.
    pub fn describe(&self, id: OperationId, kind: &'static str, file_count: usize) {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        if let Some(operation) = state.operations.get_mut(&id) {
            operation.kind = kind;
            operation.file_count = file_count;
            state.record(id, "admitted", None);
        }
    }

    /// Records a lifecycle boundary without changing admission or queue ownership.
    pub fn phase(&self, id: OperationId, phase: &'static str) {
        self.state.lock().expect("mutation coordinator lock").record(id, phase, None);
    }

    /// Counts a command invocation independently of the number of selected files.
    pub fn command_started(&self, id: OperationId) {
        if let Some(operation) = self.state.lock().expect("mutation coordinator lock").operations.get_mut(&id) {
            operation.command_count += 1;
        }
    }

    /// Returns at most 128 records after a host-local cursor without consuming other readers' logs.
    pub fn trace(&self, after: Option<u64>) -> MutationTracePage {
        let state = self.state.lock().expect("mutation coordinator lock");
        let initial = after.is_none();
        let after = after.unwrap_or_else(|| state.trace_sequence.saturating_sub(TRACE_PAGE as u64));
        let first = state.trace.front().map_or(state.trace_sequence + 1, |event| event.sequence);
        let events: Vec<_> = state.trace.iter().filter(|event| event.sequence > after)
            .take(TRACE_PAGE).cloned().collect();
        MutationTracePage {
            sequence: events.last().map_or(after, |event| event.sequence),
            dropped: if initial { 0 } else { first.saturating_sub(after.saturating_add(1)) }, events,
        }
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
        let mut reported = Vec::new();
        loop {
            let blocked = {
                let mut state = self.state.lock().expect("mutation coordinator lock");
                let operation = state.operations.get(&id).context("unknown queued mutation")?;
                ensure!(operation.state == OperationState::Queued, "mutation is no longer queued");
                let earlier: Vec<_> = state.operations.range(..id).filter(|(_, previous)| previous.scopes.iter().any(|scope| operation.scopes.contains(scope))).collect();
                ensure!(!earlier.iter().any(|(_, previous)| previous.quarantined), "repository writes require read-only reconciliation");
                let blockers: Vec<_> = earlier.iter().filter(|(_, previous)| !matches!(previous.state, OperationState::Finished(_)))
                    .map(|(id, _)| **id).collect();
                for blocker in &blockers {
                    if !reported.contains(blocker) {
                        state.record(id, "blocked", Some(*blocker));
                        reported.push(*blocker);
                    }
                }
                !blockers.is_empty()
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
        let blocker = state.operations.range(..id).find(|(_, previous)| {
            (previous.quarantined || !matches!(previous.state, OperationState::Finished(_)))
                && previous
                    .scopes
                    .iter()
                    .any(|scope| operation.scopes.contains(scope))
        }).map(|(id, _)| *id);
        if let Some(blocker) = blocker {
            if operation.blocker != Some(blocker) {
                state.record(id, "blocked", Some(blocker));
                state.operations.get_mut(&id).expect("known mutation").blocker = Some(blocker);
            }
            return Ok(None);
        }
        state.operations.get_mut(&id).expect("known mutation").state = OperationState::Running {
            cancellation_requested: false,
        };
        state.record(id, "running", None);
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
        let outcome = operation.state;
        state.record(id, "cancel_requested", None);
        self.changed.send_replace(());
        Ok(outcome)
    }

    /// Removes only terminal receipts, allowing callers to acknowledge durable adoption explicitly.
    pub fn take_receipt(&self, id: OperationId) -> Option<OperationCompletion> {
        let mut state = self.state.lock().expect("mutation coordinator lock");
        if state.operations.get(&id)?.quarantined { return None; }
        let OperationState::Finished(completion) = state.operations.get(&id)?.state else {
            return None;
        };
        state.record(id, "acknowledged", None);
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
        state.record(self.id, "quarantined", None);
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
        state.record(self.id, match completion {
            OperationCompletion::Completed => "completed",
            OperationCompletion::Failed => "failed",
            OperationCompletion::CancelledBeforeStart => "cancelled",
            OperationCompletion::Uncertain => "uncertain",
        }, None);
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
                state.record(self.id, "abandoned", None);
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
