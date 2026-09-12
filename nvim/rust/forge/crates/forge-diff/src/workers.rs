//! Dedicated analysis threads retain job and input admission through native completion.

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex, Weak};
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use tokio::sync::Notify;

/// Queue selection order, with the highest priority first.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum WorkPriority {
    Foreground,
    Visible,
    Speculative,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WorkStop {
    Cancelled,
    Deadline,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PoolError {
    Closed,
    Busy,
    Oversized,
    WorkerUnavailable,
    Stopped(WorkStop),
}

#[derive(Clone, Copy, Debug)]
pub struct PoolLimits {
    pub workers: usize,
    pub jobs: usize,
    pub input_bytes: usize,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct PoolUsage {
    pub admitted_jobs: usize,
    pub running_jobs: usize,
    pub input_bytes: usize,
    pub live_workers: usize,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct PoolShutdown {
    pub usage: PoolUsage,
    pub unfinished_workers: usize,
}

/// Shares cancellation and an optional cooperative deadline with one native job.
#[derive(Clone)]
pub struct WorkBudget {
    input_bytes: usize,
    deadline: Option<Instant>,
    cancelled: Arc<AtomicBool>,
}

/// Identifies one submission within its owning pool. Dropping a ticket does not cancel work.
#[derive(Clone)]
pub struct WorkTicket {
    owner: Weak<PoolShared>,
    sequence: u64,
}

/// Reserves a job slot and declared input bytes before the caller transfers its input closure.
pub struct WorkPermit {
    priority: WorkPriority,
    lease: WorkLease,
}

/// Owns lazily started native threads and bounded FIFO queues ordered by priority.
///
/// Closing admission drains accepted work. Dropping the pool never waits for running native code.
/// Worker threads retain accepted jobs and their input charges until completion.
pub struct AnalysisPool {
    shared: Arc<PoolShared>,
    worker: Mutex<Vec<JoinHandle<()>>>,
}

struct PoolShared {
    limits: PoolLimits,
    state: Mutex<PoolState>,
    ready: Condvar,
    completed: Notify,
}

struct PoolState {
    queue: [VecDeque<WorkItem>; 3],
    job: HashMap<u64, WorkRecord>,
    usage: PoolUsage,
    next_sequence: u64,
    closed: bool,
}

struct WorkRecord {
    budget: WorkBudget,
    running: bool,
}

struct WorkItem {
    // Input-owning closures must drop before the lease releases admission.
    task: Option<Box<dyn FnOnce(WorkBudget) + Send>>,
    lease: WorkLease,
}

struct WorkLease {
    shared: Arc<PoolShared>,
    sequence: u64,
}

impl WorkBudget {
    pub fn new(input_bytes: usize, deadline: Option<Instant>) -> Self {
        Self {
            input_bytes,
            deadline,
            cancelled: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Returns a stop reason at a cooperative checkpoint without interrupting native code.
    pub fn check(&self) -> Result<(), WorkStop> {
        if self.cancelled.load(Ordering::Acquire) {
            Err(WorkStop::Cancelled)
        } else if self
            .deadline
            .is_some_and(|deadline| Instant::now() >= deadline)
        {
            Err(WorkStop::Deadline)
        } else {
            Ok(())
        }
    }
}

impl AnalysisPool {
    pub fn new(limits: PoolLimits) -> Self {
        Self {
            shared: Arc::new(PoolShared {
                limits,
                state: Mutex::new(PoolState {
                    queue: std::array::from_fn(|_| VecDeque::new()),
                    job: HashMap::new(),
                    usage: PoolUsage::default(),
                    next_sequence: 0,
                    closed: false,
                }),
                ready: Condvar::new(),
                completed: Notify::new(),
            }),
            worker: Mutex::new(Vec::new()),
        }
    }

    /// Charges declared input bytes and a job slot before queuing the closure.
    ///
    /// Rejection drops the closure without execution. Zero workers or jobs reject all work.
    /// Expired or cancelled queued work is dropped before execution. Native panics drop the
    /// closure and release admission while preserving the worker for subsequent jobs.
    pub fn submit(
        &self,
        priority: WorkPriority,
        budget: WorkBudget,
        task: impl FnOnce(WorkBudget) + Send + 'static,
    ) -> Result<WorkTicket, PoolError> {
        Ok(self.reserve(priority, budget)?.submit(task))
    }

    /// Reserves admission without retaining caller inputs. Dropping the permit releases admission.
    pub fn reserve(
        &self,
        priority: WorkPriority,
        budget: WorkBudget,
    ) -> Result<WorkPermit, PoolError> {
        budget.check().map_err(PoolError::Stopped)?;
        let mut state = self.shared.state.lock().expect("analysis pool poisoned");
        if state.closed {
            return Err(PoolError::Closed);
        }
        if budget.input_bytes > self.shared.limits.input_bytes {
            return Err(PoolError::Oversized);
        }
        if self.shared.limits.workers == 0 || self.shared.limits.jobs == 0 {
            return Err(PoolError::WorkerUnavailable);
        }
        if state.usage.admitted_jobs >= self.shared.limits.jobs
            || budget.input_bytes > self.shared.limits.input_bytes - state.usage.input_bytes
        {
            return Err(PoolError::Busy);
        }
        let sequence = state.next_sequence;
        let next_sequence = sequence.checked_add(1).ok_or(PoolError::WorkerUnavailable)?;

        // Start capacity before retaining a queued closure, with workers blocked on the state lock.
        if state.usage.live_workers < self.shared.limits.workers
            && state.usage.live_workers <= state.usage.admitted_jobs
        {
            let shared = Arc::clone(&self.shared);
            let worker = std::thread::Builder::new()
                .name(format!("forge-analysis-{}", state.usage.live_workers))
                .spawn(move || run_worker(shared))
                .map_err(|_| PoolError::WorkerUnavailable)?;
            self.worker
                .lock()
                .expect("analysis workers poisoned")
                .push(worker);
            state.usage.live_workers += 1;
        }
        state.next_sequence = next_sequence;
        state.usage.admitted_jobs += 1;
        state.usage.input_bytes += budget.input_bytes;
        state.job.insert(
            sequence,
            WorkRecord {
                budget,
                running: false,
            },
        );
        Ok(WorkPermit {
            priority,
            lease: WorkLease {
                shared: Arc::clone(&self.shared),
                sequence,
            },
        })
    }

    /// Drops queued inputs or signals an active job. Foreign and retired tickets return false.
    pub fn cancel(&self, ticket: &WorkTicket) -> bool {
        if !Weak::ptr_eq(&ticket.owner, &Arc::downgrade(&self.shared)) {
            return false;
        }
        let queued = {
            let mut state = self.shared.state.lock().expect("analysis pool poisoned");
            let Some(record) = state.job.get(&ticket.sequence) else {
                return false;
            };
            record.budget.cancelled.store(true, Ordering::Release);
            state.queue.iter_mut().find_map(|queue| {
                let position = queue
                    .iter()
                    .position(|item| item.lease.sequence == ticket.sequence)?;
                queue.remove(position)
            })
        };
        // Dropping feature-owned input can reenter its owner, so release the pool lock first.
        drop(queued);
        true
    }

    /// Raises queued work to the tail of a higher-priority queue without changing admission.
    ///
    /// Foreign, stopped, running, and retired tickets return false. Equal or lower priority
    /// requests do not reorder work. Promotion does not interrupt a running native operation.
    pub fn promote(&self, ticket: &WorkTicket, priority: WorkPriority) -> bool {
        if !Weak::ptr_eq(&ticket.owner, &Arc::downgrade(&self.shared)) {
            return false;
        }
        let mut state = self.shared.state.lock().expect("analysis pool poisoned");
        if state
            .job
            .get(&ticket.sequence)
            .is_none_or(|record| record.running || record.budget.check().is_err())
        {
            return false;
        }
        let promoted = state
            .queue
            .iter_mut()
            .skip(priority as usize + 1)
            .find_map(|queue| {
                let position = queue
                    .iter()
                    .position(|item| item.lease.sequence == ticket.sequence)?;
                queue.remove(position)
            });
        let Some(promoted) = promoted else {
            return false;
        };
        state.queue[priority as usize].push_back(promoted);
        self.shared.ready.notify_one();
        true
    }

    pub fn close(&self) {
        self.shared
            .state
            .lock()
            .expect("analysis pool poisoned")
            .closed = true;
        self.shared.ready.notify_all();
        self.wake_admission();
    }

    pub(crate) fn admission_changed(&self) -> tokio::sync::futures::Notified<'_> {
        self.shared.completed.notified()
    }

    pub(crate) fn wake_admission(&self) {
        self.shared.completed.notify_waiters();
    }

    pub fn usage(&self) -> PoolUsage {
        self.shared
            .state
            .lock()
            .expect("analysis pool poisoned")
            .usage
    }

    /// Closes admission and joins finished threads until the deadline, returning unfinished owners.
    pub async fn shutdown(&self, deadline: Duration) -> PoolShutdown {
        self.close();
        let expires = tokio::time::Instant::now() + deadline;
        loop {
            let completed = self.shared.completed.notified();
            tokio::pin!(completed);
            completed.as_mut().enable();
            let remaining = self.join_finished();
            let usage = self.usage();
            if remaining == 0 || tokio::time::Instant::now() >= expires {
                return PoolShutdown {
                    usage,
                    unfinished_workers: remaining,
                };
            }
            if usage.live_workers == 0 {
                tokio::task::yield_now().await;
            } else if tokio::time::timeout_at(expires, completed).await.is_err() {
                return PoolShutdown {
                    usage: self.usage(),
                    unfinished_workers: self.join_finished(),
                };
            }
        }
    }

    fn join_finished(&self) -> usize {
        let mut worker = self.worker.lock().expect("analysis workers poisoned");
        let mut position = 0;
        while position < worker.len() {
            if worker[position].is_finished() {
                let _ = worker.swap_remove(position).join();
            } else {
                position += 1;
            }
        }
        worker.len()
    }
}

impl Drop for AnalysisPool {
    fn drop(&mut self) {
        self.close();
        self.join_finished();
    }
}

impl WorkPermit {
    /// Captures completion identity before the reserved work can be published or queued.
    pub fn ticket(&self) -> WorkTicket {
        WorkTicket {
            owner: Arc::downgrade(&self.lease.shared),
            sequence: self.lease.sequence,
        }
    }

    /// Transfers accepted input ownership into its priority queue, including after admission closes.
    pub fn submit(self, task: impl FnOnce(WorkBudget) + Send + 'static) -> WorkTicket {
        let shared = Arc::clone(&self.lease.shared);
        let ticket = self.ticket();
        shared.state.lock().expect("analysis pool poisoned").queue[self.priority as usize]
            .push_back(WorkItem {
                task: Some(Box::new(task)),
                lease: self.lease,
            });
        shared.ready.notify_one();
        ticket
    }
}

impl WorkTicket {
    /// Waits until native inputs and their admission lease have been released.
    ///
    /// A dropped unsubmitted permit also completes this receipt. Waiting does not cancel work
    /// or require the executor that submitted it to remain alive.
    pub async fn completed(&self) {
        let Some(shared) = self.owner.upgrade() else {
            return;
        };
        loop {
            let completed = shared.completed.notified();
            tokio::pin!(completed);
            completed.as_mut().enable();
            if !shared
                .state
                .lock()
                .expect("analysis pool poisoned")
                .job
                .contains_key(&self.sequence)
            {
                return;
            }
            completed.await;
        }
    }
}

impl Drop for WorkLease {
    fn drop(&mut self) {
        let mut state = self.shared.state.lock().expect("analysis pool poisoned");
        if let Some(record) = state.job.remove(&self.sequence) {
            state.usage.admitted_jobs -= 1;
            state.usage.input_bytes -= record.budget.input_bytes;
            if record.running {
                state.usage.running_jobs -= 1;
            }
        }
        self.shared.completed.notify_waiters();
        self.shared.ready.notify_all();
    }
}

fn run_worker(shared: Arc<PoolShared>) {
    loop {
        let (mut work, budget) = {
            let mut state = shared.state.lock().expect("analysis pool poisoned");
            loop {
                if let Some(work) = state.queue.iter_mut().find_map(VecDeque::pop_front) {
                    let record = state
                        .job
                        .get_mut(&work.lease.sequence)
                        .expect("queued job missing");
                    record.running = true;
                    let budget = record.budget.clone();
                    state.usage.running_jobs += 1;
                    break (work, budget);
                }
                if state.closed && state.job.is_empty() {
                    state.usage.live_workers -= 1;
                    shared.completed.notify_waiters();
                    return;
                }
                state = shared.ready.wait(state).expect("analysis pool poisoned");
            }
        };
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
            if budget.check().is_ok() {
                let task = work.task.take().expect("queued task missing");
                task(budget);
            }
            drop(work);
        }));
    }
}
