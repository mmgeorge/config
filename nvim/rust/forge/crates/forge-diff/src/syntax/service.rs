use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use tokio::sync::watch;

use crate::source::{SourceIdentity, SourceVersion};
use crate::workers::{AnalysisPool, WorkBudget, WorkPriority, WorkStop, WorkTicket};

use super::tree::{ParsedSyntax, parse};
use super::{SyntaxError, SyntaxHandle, SyntaxLanguage};

#[derive(Clone, Copy, Debug)]
pub struct SyntaxLimits {
    /// Admission charge for pinned results and active jobs, not a native allocator RSS limit.
    pub retained_bytes: usize,
    pub cached_entries: usize,
    pub captures: usize,
    pub injection_depth: usize,
    pub injection_trees: usize,
}

impl Default for SyntaxLimits {
    fn default() -> Self {
        Self {
            retained_bytes: 64 * 1024 * 1024,
            cached_entries: 256,
            captures: 65_536,
            injection_depth: 4,
            injection_trees: 1024,
        }
    }
}

#[derive(Clone)]
pub struct SyntaxRequest {
    pub source: SourceVersion,
    pub language: SyntaxLanguage,
    pub priority: WorkPriority,
    pub deadline: Option<Instant>,
}

#[derive(Clone, Copy, Debug)]
pub struct SyntaxUsage {
    pub retained_bytes: usize,
    pub cached_entries: usize,
    pub active_jobs: usize,
}

pub struct SyntaxEngine {
    pool: Arc<AnalysisPool>,
    limits: SyntaxLimits,
    state: Mutex<SyntaxState>,
    retained: Arc<AtomicUsize>,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct SyntaxKey {
    source: SourceIdentity,
    language: SyntaxLanguage,
}

struct SyntaxState {
    cache: HashMap<SyntaxKey, (u64, SyntaxHandle)>,
    job: HashMap<SyntaxKey, Arc<SyntaxJob>>,
    sequence: u64,
    closed: bool,
}

struct SyntaxJob {
    consumer: AtomicUsize,
    result: watch::Sender<Option<Result<SyntaxHandle, SyntaxError>>>,
    ticket: WorkTicket,
}

struct SyntaxInterest {
    job: Arc<SyntaxJob>,
    pool: Arc<AnalysisPool>,
}

pub(super) struct SyntaxCharge {
    retained: Arc<AtomicUsize>,
    bytes: usize,
}

struct SyntaxCompletion {
    engine: Arc<SyntaxEngine>,
    key: SyntaxKey,
    job: Arc<SyntaxJob>,
    budget: WorkBudget,
    published: bool,
}

impl SyntaxEngine {
    /// Shares worker admission with repository comparisons and owns bounded exact-source retention.
    pub fn new(pool: Arc<AnalysisPool>, limits: SyntaxLimits) -> Arc<Self> {
        Arc::new(Self {
            pool,
            limits,
            retained: Arc::new(AtomicUsize::new(0)),
            state: Mutex::new(SyntaxState {
                cache: HashMap::new(),
                job: HashMap::new(),
                sequence: 0,
                closed: false,
            }),
        })
    }

    /// Coalesces one source/language identity and retains native ownership through cancellation.
    ///
    /// Captures, recursive injections, and retained charge have explicit limits. A saturated
    /// cache evicts unpinned entries before returning `MemoryLimit`. At most 64 consumers join
    /// one job. Unavailable injected languages remain explicit metadata on the parent result.
    pub async fn analyze(
        self: &Arc<Self>,
        request: SyntaxRequest,
    ) -> Result<SyntaxHandle, SyntaxError> {
        loop {
            let changed = self.pool.admission_changed();
            tokio::pin!(changed);
            changed.as_mut().enable();
            match self.analyze_admitted(request.clone()).await {
                Err(SyntaxError::Busy | SyntaxError::Pool(crate::workers::PoolError::Busy)) => {
                    if let Some(deadline) = request.deadline {
                        tokio::time::timeout_at(deadline.into(), changed)
                            .await
                            .map_err(|_| SyntaxError::Deadline)?;
                    } else {
                        changed.await;
                    }
                }
                result => return result,
            }
        }
    }

    async fn analyze_admitted(
        self: &Arc<Self>,
        request: SyntaxRequest,
    ) -> Result<SyntaxHandle, SyntaxError> {
        let key = SyntaxKey {
            source: request.source.identity(),
            language: request.language,
        };
        let (interest, mut receiver, work) = {
            let mut state = self.state.lock().expect("syntax state poisoned");
            if state.closed {
                return Err(SyntaxError::Closed);
            }
            state.sequence = state.sequence.saturating_add(1);
            let sequence = state.sequence;
            if let Some((used, result)) = state.cache.get_mut(&key) {
                *used = sequence;
                return Ok(result.clone());
            }
            if let Some(job) = state.job.get(&key) {
                job.consumer
                    .fetch_update(Ordering::AcqRel, Ordering::Acquire, |count| {
                        (count > 0 && count < 64).then_some(count + 1)
                    })
                    .map_err(|count| {
                        if count == 0 {
                            SyntaxError::Busy
                        } else {
                            SyntaxError::ConsumerLimit
                        }
                    })?;
                (
                    SyntaxInterest {
                        job: Arc::clone(job),
                        pool: Arc::clone(&self.pool),
                    },
                    job.result.subscribe(),
                    None,
                )
            } else {
                // Reserve source/tree allowance and complete capture capacity before native admission.
                let bytes = request
                    .source
                    .retained_bytes()
                    .saturating_mul(64)
                    .saturating_add(self.limits.captures.saturating_mul(
                        std::mem::size_of::<super::SyntaxCapture>()
                            + 2 * std::mem::size_of::<usize>(),
                    ));
                while bytes
                    > self
                        .limits
                        .retained_bytes
                        .saturating_sub(self.retained.load(Ordering::Acquire))
                {
                    let Some(oldest) = state
                        .cache
                        .iter()
                        .min_by_key(|(_, (used, _))| *used)
                        .map(|(key, _)| *key)
                    else {
                        return Err(SyntaxError::MemoryLimit);
                    };
                    state.cache.remove(&oldest);
                }
                self.retained.fetch_add(bytes, Ordering::AcqRel);
                let charge = SyntaxCharge {
                    retained: Arc::clone(&self.retained),
                    bytes,
                };
                let budget = WorkBudget::new(request.source.retained_bytes(), request.deadline);
                let permit = self
                    .pool
                    .reserve(request.priority, budget.clone())
                    .map_err(SyntaxError::Pool)?;
                let (sender, receiver) = watch::channel(None);
                let job = Arc::new(SyntaxJob {
                    consumer: AtomicUsize::new(1),
                    result: sender,
                    ticket: permit.ticket(),
                });
                state.job.insert(key, Arc::clone(&job));
                let completion = SyntaxCompletion {
                    engine: Arc::clone(self),
                    key,
                    job: Arc::clone(&job),
                    budget,
                    published: false,
                };
                (
                    SyntaxInterest {
                        job,
                        pool: Arc::clone(&self.pool),
                    },
                    receiver,
                    Some((permit, charge, completion)),
                )
            }
        };
        if let Some((permit, charge, mut completion)) = work {
            let limits = self.limits;
            permit.submit(move |budget| {
                let result = parse(request.source, request.language, limits, &budget, charge);
                completion.publish(result);
            });
        }
        self.pool.promote(&interest.job.ticket, request.priority);
        loop {
            let outcome = receiver.borrow_and_update().clone();
            if let Some(outcome) = outcome {
                interest.job.ticket.completed().await;
                return outcome;
            }
            receiver
                .changed()
                .await
                .map_err(|_| SyntaxError::WorkerFailed)?;
        }
    }

    /// Closes only syntax admission. The shared pool remains owned by the runtime.
    pub fn close(&self) {
        self.state.lock().expect("syntax state poisoned").closed = true;
        self.pool.wake_admission();
    }

    pub fn usage(&self) -> SyntaxUsage {
        let state = self.state.lock().expect("syntax state poisoned");
        SyntaxUsage {
            retained_bytes: self.retained.load(Ordering::Acquire),
            cached_entries: state.cache.len(),
            active_jobs: state.job.len(),
        }
    }
}

impl SyntaxCompletion {
    fn publish(&mut self, result: Result<ParsedSyntax, SyntaxError>) {
        if self.published {
            return;
        }
        self.published = true;
        let mut state = self.engine.state.lock().expect("syntax state poisoned");
        let result = match (self.budget.check(), result) {
            (Err(stopped), _) => Err(stop_error(stopped)),
            (Ok(()), result) => result.map(|parsed| SyntaxHandle(Arc::new(parsed))),
        };
        if let Ok(handle) = &result {
            while state.cache.len() >= self.engine.limits.cached_entries && !state.cache.is_empty()
            {
                let oldest = state
                    .cache
                    .iter()
                    .min_by_key(|(_, (used, _))| *used)
                    .map(|(key, _)| *key)
                    .unwrap();
                state.cache.remove(&oldest);
            }
            if self.engine.limits.cached_entries > 0
                && self.job.consumer.load(Ordering::Acquire) > 0
            {
                let sequence = state.sequence;
                state.cache.insert(self.key, (sequence, handle.clone()));
            }
        }
        self.job.result.send_replace(Some(result));
        state.job.remove(&self.key);
    }
}

impl Drop for SyntaxCompletion {
    fn drop(&mut self) {
        self.publish(Err(self
            .budget
            .check()
            .err()
            .map(stop_error)
            .unwrap_or(SyntaxError::WorkerFailed)));
    }
}

impl Drop for SyntaxInterest {
    fn drop(&mut self) {
        if self.job.consumer.fetch_sub(1, Ordering::AcqRel) == 1 {
            self.pool.cancel(&self.job.ticket);
        }
    }
}

impl SyntaxCharge {
    pub(super) fn release_unused(&mut self, bytes: usize) {
        let released = bytes.min(self.bytes);
        self.bytes -= released;
        self.retained.fetch_sub(released, Ordering::AcqRel);
    }
}

impl Drop for SyntaxCharge {
    fn drop(&mut self) {
        self.retained.fetch_sub(self.bytes, Ordering::AcqRel);
    }
}

pub(super) fn stop_error(stopped: WorkStop) -> SyntaxError {
    match stopped {
        WorkStop::Cancelled => SyntaxError::Cancelled,
        WorkStop::Deadline => SyntaxError::Deadline,
    }
}
