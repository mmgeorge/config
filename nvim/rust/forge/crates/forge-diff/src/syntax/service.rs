use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Instant;

use tokio::sync::watch;

use crate::source::{SourceIdentity, SourceVersion};
use crate::workers::{AnalysisPool, WorkBudget, WorkPriority, WorkStop, WorkTicket};

use super::tree::{ParsedSyntax, parse};
use super::{SyntaxError, SyntaxHandle, SyntaxLanguage};

/// Files above this line count retain source text without syntax analysis.
pub const SYNTAX_LINE_LIMIT: usize = 10_000;

const CACHE_ENTRIES: usize = 256;

#[derive(Clone)]
pub struct SyntaxRequest {
    pub source: SourceVersion,
    pub language: SyntaxLanguage,
    pub priority: WorkPriority,
    pub deadline: Option<Instant>,
}

#[derive(Clone, Copy, Debug)]
pub struct SyntaxUsage {
    pub cached_entries: usize,
    pub active_jobs: usize,
}

pub struct SyntaxEngine {
    pool: Arc<AnalysisPool>,
    state: Mutex<SyntaxState>,
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

struct SyntaxCompletion {
    engine: Arc<SyntaxEngine>,
    key: SyntaxKey,
    job: Arc<SyntaxJob>,
    budget: WorkBudget,
    published: bool,
}

impl SyntaxEngine {
    /// Shares parser workers and caches exact-source results without restricting live handles.
    pub fn new(pool: Arc<AnalysisPool>) -> Arc<Self> {
        Arc::new(Self {
            pool,
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
    /// Files exceeding `SYNTAX_LINE_LIMIT` return their source without trees or captures.
    /// Cache eviction never rejects analysis or invalidates handles held by documents.
    /// Unavailable injected languages remain explicit metadata on the parent result.
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
                        (count > 0).then(|| count + 1)
                    })
                    .map_err(|_| SyntaxError::Busy)?;
                (
                    SyntaxInterest {
                        job: Arc::clone(job),
                        pool: Arc::clone(&self.pool),
                    },
                    job.result.subscribe(),
                    None,
                )
            } else {
                // The source is already retained. Syntax eligibility depends only on its line count.
                let budget = WorkBudget::new(0, request.deadline);
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
                    Some((permit, completion)),
                )
            }
        };
        if let Some((permit, mut completion)) = work {
            permit.submit(move |budget| {
                let result = parse(request.source, request.language, &budget);
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
            while state.cache.len() >= CACHE_ENTRIES {
                let oldest = state
                    .cache
                    .iter()
                    .min_by_key(|(_, (used, _))| *used)
                    .map(|(key, _)| *key)
                    .unwrap();
                state.cache.remove(&oldest);
            }
            if self.job.consumer.load(Ordering::Acquire) > 0 {
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

pub(super) fn stop_error(stopped: WorkStop) -> SyntaxError {
    match stopped {
        WorkStop::Cancelled => SyntaxError::Cancelled,
        WorkStop::Deadline => SyntaxError::Deadline,
    }
}
