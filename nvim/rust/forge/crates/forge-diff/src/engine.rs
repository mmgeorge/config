//! Shared comparisons retain admission until native work exits, independently of waiting callers.

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::sync::{Notify, watch};

use crate::cache::{
    AnalysisBytes, AnalysisHandle, AnalysisKey, AnalysisReservation, AnalysisStore, CacheError,
    CacheLimits, CacheUsage,
};
use crate::raw::{RawDiff, RawHunk, compute_hunks};
use crate::source::SourcePair;
use crate::workers::{
    AnalysisPool, PoolError, PoolLimits, PoolUsage, WorkBudget, WorkPriority, WorkStop, WorkTicket,
};

pub const MAX_ANALYSIS_CONSUMERS: usize = 64;

/// Owns exact immutable sources and the caller's scheduling priority for their shared comparison.
#[derive(Clone)]
pub struct DiffRequest {
    pub source: SourcePair,
    pub priority: WorkPriority,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EngineError {
    Busy,
    ConsumerLimit,
    Closed,
    Cancelled,
    Deadline,
    WorkerFailed,
    Pool(PoolError),
    Cache(CacheError),
}

#[derive(Clone, Copy, Debug)]
pub struct EngineUsage {
    pub active_jobs: usize,
    pub cache: CacheUsage,
    pub pool: PoolUsage,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct EngineShutdown {
    pub unfinished_jobs: usize,
    pub unfinished_workers: usize,
}

pub struct DiffEngine {
    state: Mutex<EngineState>,
    completed: Notify,
    max_jobs: usize,
    pool: Arc<AnalysisPool>,
    compute: Arc<dyn Fn(SourcePair) -> RawDiff + Send + Sync>,
}

struct EngineState {
    cache: Arc<AnalysisStore>,
    job: HashMap<AnalysisKey, Arc<AnalysisJob>>,
    closed: bool,
}

struct AnalysisJob {
    consumer: AtomicUsize,
    result: watch::Sender<Option<Result<AnalysisHandle, EngineError>>>,
    schedule: Mutex<AnalysisSchedule>,
}

struct AnalysisSchedule {
    priority: WorkPriority,
    ticket: WorkTicket,
}

struct AnalysisInterest {
    job: Arc<AnalysisJob>,
    pool: Arc<AnalysisPool>,
}

struct AnalysisWork {
    // Drop input bytes before the completion guard releases their reservation.
    source: SourcePair,
    completion: AnalysisCompletion,
}

struct AnalysisCompletion {
    engine: Arc<DiffEngine>,
    key: AnalysisKey,
    job: Arc<AnalysisJob>,
    reservation: Option<AnalysisReservation>,
    budget: WorkBudget,
}

impl DiffEngine {
    /// Shares native admission with syntax analysis without starting another worker pool.
    pub fn analysis_pool(&self) -> Arc<AnalysisPool> {
        Arc::clone(&self.pool)
    }

    pub fn new(cache_limits: CacheLimits, max_jobs: usize) -> Arc<Self> {
        Self::with_cache(Arc::new(AnalysisStore::new(cache_limits)), max_jobs)
    }

    pub fn with_cache(cache: Arc<AnalysisStore>, max_jobs: usize) -> Arc<Self> {
        let pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: max_jobs.min(
                std::thread::available_parallelism()
                    .map_or(1, |count| count.get().saturating_sub(1).clamp(1, 4)),
            ),
            jobs: max_jobs,
        }));
        Arc::new(Self {
            state: Mutex::new(EngineState {
                cache,
                job: HashMap::new(),
                closed: false,
            }),
            completed: Notify::new(),
            max_jobs,
            pool,
            compute: Arc::new(compute_hunks),
        })
    }

    /// Coalesces exact source identities and runs native comparison off the async executor.
    ///
    /// At most 64 callers can wait on one analysis. Additional callers receive `ConsumerLimit`.
    /// Temporary capacity pressure waits without holding native or cache admission. A matching
    /// job that lost its final consumer must exit before a replacement can acquire admission.
    /// Higher-priority consumers promote an existing queued job without changing its source
    /// identity or reserving duplicate work. Observed priority is never lowered for that job.
    pub async fn compare(
        self: &Arc<Self>,
        request: DiffRequest,
    ) -> Result<AnalysisHandle, EngineError> {
        loop {
            let changed = self.pool.admission_changed();
            tokio::pin!(changed);
            changed.as_mut().enable();
            match self.compare_admitted(request.clone()).await {
                Err(EngineError::Busy) if self.max_jobs > 0 => changed.await,
                Err(EngineError::Pool(PoolError::Busy)) => changed.await,
                result => return result,
            }
        }
    }

    async fn compare_admitted(
        self: &Arc<Self>,
        request: DiffRequest,
    ) -> Result<AnalysisHandle, EngineError> {
        let DiffRequest { source, priority } = request;
        let key = AnalysisKey {
            old: source.old.identity(),
            new: source.new.identity(),
        };
        // Admit one immutable job or join its existing consumers.
        let (interest, mut result, work) = {
            let mut state = self.state.lock().expect("diff engine state poisoned");
            if state.closed {
                return Err(EngineError::Closed);
            }
            if let Some(handle) = state.cache.get(key) {
                return Ok(handle);
            }
            if let Some(job) = state.job.get(&key) {
                drop(source);
                job.consumer
                    .try_update(Ordering::AcqRel, Ordering::Acquire, |consumer| {
                        (consumer > 0 && consumer < MAX_ANALYSIS_CONSUMERS).then(|| consumer + 1)
                    })
                    .map_err(|consumer| {
                        if consumer == 0 {
                            EngineError::Busy
                        } else {
                            EngineError::ConsumerLimit
                        }
                    })?;
                (
                    AnalysisInterest {
                        job: Arc::clone(job),
                        pool: Arc::clone(&self.pool),
                    },
                    job.result.subscribe(),
                    None,
                )
            } else {
                if state.job.len() >= self.max_jobs {
                    return Err(EngineError::Busy);
                }
                let bytes = reservation_bytes(&source);
                let reservation = state.cache.reserve(bytes).map_err(EngineError::Cache)?;
                let budget = WorkBudget::new(bytes.source, None);
                let permit = self
                    .pool
                    .reserve(priority, budget.clone())
                    .map_err(EngineError::Pool)?;
                let (sender, receiver) = watch::channel(None);
                let job = Arc::new(AnalysisJob {
                    consumer: AtomicUsize::new(1),
                    result: sender,
                    schedule: Mutex::new(AnalysisSchedule {
                        priority,
                        ticket: permit.ticket(),
                    }),
                });
                state.job.insert(key, Arc::clone(&job));
                let work = AnalysisWork {
                    source,
                    completion: AnalysisCompletion {
                        engine: Arc::clone(self),
                        key,
                        job: Arc::clone(&job),
                        reservation: Some(reservation),
                        budget,
                    },
                };
                (
                    AnalysisInterest {
                        job,
                        pool: Arc::clone(&self.pool),
                    },
                    receiver,
                    Some((permit, work)),
                )
            }
        };
        if let Some((permit, work)) = work {
            permit.submit(move |_| work.run());
        }
        {
            // Publish scheduling demand outside the engine lock, which cancellation can reenter.
            let mut schedule = interest
                .job
                .schedule
                .lock()
                .expect("analysis schedule poisoned");
            schedule.priority = schedule.priority.min(priority);
            self.pool.promote(&schedule.ticket, schedule.priority);
        }
        loop {
            let outcome = result.borrow_and_update().clone();
            if let Some(outcome) = outcome {
                let ticket = interest
                    .job
                    .schedule
                    .lock()
                    .expect("analysis schedule poisoned")
                    .ticket
                    .clone();
                ticket.completed().await;
                drop(interest);
                return outcome;
            }
            result
                .changed()
                .await
                .map_err(|_| EngineError::WorkerFailed)?;
        }
    }

    /// Closes new admission. Accepted native work remains owned until it actually exits.
    pub fn close(&self) {
        self.state
            .lock()
            .expect("diff engine state poisoned")
            .closed = true;
        self.pool.wake_admission();
    }

    /// Closes admission and reports native jobs and unjoined threads remaining at the deadline.
    pub async fn shutdown(&self, deadline: Duration) -> EngineShutdown {
        self.close();
        let expires = tokio::time::Instant::now() + deadline;
        loop {
            let completed = self.completed.notified();
            tokio::pin!(completed);
            completed.as_mut().enable();
            let active = self.usage().active_jobs;
            if active == 0 {
                break;
            }
            if tokio::time::timeout_at(expires, completed).await.is_err() {
                break;
            }
        }
        let remaining = expires.saturating_duration_since(tokio::time::Instant::now());
        let pool = self.pool.shutdown(remaining).await;
        EngineShutdown {
            unfinished_jobs: self.usage().active_jobs.max(pool.usage.admitted_jobs),
            unfinished_workers: pool.unfinished_workers,
        }
    }

    pub fn usage(&self) -> EngineUsage {
        let state = self.state.lock().expect("diff engine state poisoned");
        EngineUsage {
            active_jobs: state.job.len(),
            cache: state.cache.usage(),
            pool: self.pool.usage(),
        }
    }
}

impl Drop for AnalysisInterest {
    fn drop(&mut self) {
        if self.job.consumer.fetch_sub(1, Ordering::AcqRel) == 1 {
            let ticket = self
                .job
                .schedule
                .lock()
                .expect("analysis schedule poisoned")
                .ticket
                .clone();
            self.pool.cancel(&ticket);
        }
    }
}

impl AnalysisWork {
    fn run(mut self) {
        let outcome = if self.completion.job.consumer.load(Ordering::Acquire) == 0 {
            drop(self.source);
            Err(EngineError::Cancelled)
        } else {
            Ok((self.completion.engine.compute)(self.source))
        };
        let outcome = match self.completion.budget.check() {
            Ok(()) => outcome,
            Err(reason) => {
                drop(outcome);
                Err(match reason {
                    WorkStop::Cancelled => EngineError::Cancelled,
                    WorkStop::Deadline => EngineError::Deadline,
                })
            }
        };
        self.completion.publish(outcome);
    }
}

impl AnalysisCompletion {
    fn publish(&mut self, outcome: Result<RawDiff, EngineError>) {
        let Some(reservation) = self.reservation.take() else {
            return;
        };
        let mut state = self
            .engine
            .state
            .lock()
            .expect("diff engine state poisoned");
        let outcome = match outcome {
            Ok(diff) if self.job.consumer.load(Ordering::Acquire) > 0 => state
                .cache
                .insert(self.key, reservation, diff)
                .map_err(EngineError::Cache),
            discarded => {
                let error = discarded.err().unwrap_or(EngineError::Cancelled);
                drop(reservation);
                Err(error)
            }
        };
        self.job.result.send_replace(Some(outcome));
        state.job.remove(&self.key);
        self.engine.completed.notify_waiters();
    }
}

impl Drop for AnalysisCompletion {
    fn drop(&mut self) {
        let error = match self.budget.check() {
            Ok(()) => EngineError::WorkerFailed,
            Err(WorkStop::Cancelled) => EngineError::Cancelled,
            Err(WorkStop::Deadline) => EngineError::Deadline,
        };
        self.publish(Err(error));
    }
}

pub fn reservation_bytes(source: &SourcePair) -> AnalysisBytes {
    let source_bytes = source.old.retained_bytes()
        + if source.old.shares_bytes(&source.new) {
            0
        } else {
            source.new.retained_bytes()
        };
    let lines = source.old.newline().line_count + source.new.newline().line_count;
    let hunk_capacity = if source.old.identity().content_hash == source.new.identity().content_hash
    {
        0
    } else if source.old.bytes().is_empty() || source.new.bytes().is_empty() {
        1
    } else {
        lines.max(4).next_power_of_two()
    };
    AnalysisBytes {
        source: source_bytes,
        result: std::mem::size_of::<RawDiff>() + hunk_capacity * std::mem::size_of::<RawHunk>(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Representation, SourceVersion};

    fn request(text: &str) -> DiffRequest {
        DiffRequest {
            source: SourcePair {
                old: SourceVersion::new(b"old\n".to_vec(), Representation::Raw).unwrap(),
                new: SourceVersion::new(text.as_bytes().to_vec(), Representation::Raw).unwrap(),
            },
            priority: WorkPriority::Foreground,
        }
    }

    fn blocked_engine() -> (
        Arc<DiffEngine>,
        Arc<tokio::sync::Notify>,
        std::sync::mpsc::Sender<()>,
    ) {
        let mut engine = DiffEngine::new(CacheLimits::default(), 1);
        let started = Arc::new(tokio::sync::Notify::new());
        let notification = Arc::clone(&started);
        let (release, receiver) = std::sync::mpsc::channel();
        let receiver = Mutex::new(receiver);
        Arc::get_mut(&mut engine).unwrap().compute = Arc::new(move |source| {
            notification.notify_one();
            receiver.lock().unwrap().recv().unwrap();
            compute_hunks(source)
        });
        (engine, started, release)
    }

    #[tokio::test]
    async fn cancelled_consumer_does_not_cancel_another_consumer_of_the_same_analysis() {
        let (engine, started, release) = blocked_engine();
        let first_engine = Arc::clone(&engine);
        let first = tokio::spawn(async move { first_engine.compare(request("new\n")).await });
        started.notified().await;
        let second_engine = Arc::clone(&engine);
        let second = tokio::spawn(async move { second_engine.compare(request("new\n")).await });
        async {
            loop {
                if engine
                    .state
                    .lock()
                    .unwrap()
                    .job
                    .values()
                    .next()
                    .unwrap()
                    .consumer
                    .load(Ordering::Acquire)
                    == 2
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
        }
        .await;
        assert_eq!(engine.usage().active_jobs, 1);
        let mut waiting = Box::pin(engine.compare(request("different\n")));
        assert!(
            std::future::poll_fn(|context| {
                std::task::Poll::Ready(std::future::Future::poll(waiting.as_mut(), context))
            })
            .await
            .is_pending()
        );
        drop(waiting);
        first.abort();
        assert!(first.await.is_err_and(|error| error.is_cancelled()));
        release.send(()).unwrap();
        let handle = second.await.unwrap().unwrap();
        assert_eq!(handle.source().new.bytes(), b"new\n");
        let cached = engine.compare(request("new\n")).await.unwrap();
        assert!(std::ptr::eq(&*handle, &*cached));
        assert_eq!(engine.usage().cache.admitted_entries, 1);
    }

    #[tokio::test]
    async fn final_consumer_cancellation_keeps_admission_until_native_exit() {
        let (engine, started, release) = blocked_engine();
        let worker_engine = Arc::clone(&engine);
        let caller = tokio::spawn(async move { worker_engine.compare(request("new\n")).await });
        started.notified().await;
        caller.abort();
        assert!(caller.await.is_err_and(|error| error.is_cancelled()));
        assert_eq!(engine.usage().active_jobs, 1);
        assert_eq!(engine.usage().cache.admitted_entries, 1);
        let mut waiting = Box::pin(engine.compare(request("new\n")));
        assert!(
            std::future::poll_fn(|context| {
                std::task::Poll::Ready(std::future::Future::poll(waiting.as_mut(), context))
            })
            .await
            .is_pending()
        );
        drop(waiting);
        release.send(()).unwrap();
        async {
            while {
                let usage = engine.usage();
                usage.active_jobs != 0 || usage.pool.admitted_jobs != 0
            } {
                tokio::task::yield_now().await;
            }
        }
        .await;
        assert_eq!(engine.usage().cache.admitted_entries, 0);
        assert_eq!(engine.usage().cache.cached_entries, 0);
        release.send(()).unwrap();
        assert!(engine.compare(request("new\n")).await.is_ok());
    }

    #[test]
    fn native_completion_releases_admission_after_its_executor_is_destroyed() {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        let (engine, started, release) = blocked_engine();
        let worker_engine = Arc::clone(&engine);
        runtime.block_on(async {
            tokio::spawn(async move { worker_engine.compare(request("new\n")).await });
            started.notified().await;
        });
        runtime.shutdown_timeout(Duration::from_millis(10));
        assert_eq!(engine.usage().active_jobs, 1);
        assert_eq!(engine.usage().cache.admitted_entries, 1);
        release.send(()).unwrap();
        let observer = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        assert_eq!(
            observer.block_on(engine.shutdown(Duration::from_secs(1))),
            EngineShutdown::default()
        );
        assert_eq!(engine.usage().cache.admitted_entries, 0);
        assert_eq!(engine.usage().cache.cached_entries, 0);
    }

    #[tokio::test]
    async fn worker_panic_releases_admission_and_allows_a_later_request() {
        let mut engine = DiffEngine::new(CacheLimits::default(), 1);
        let calls = AtomicUsize::new(0);
        Arc::get_mut(&mut engine).unwrap().compute = Arc::new(move |source| {
            assert_ne!(
                calls.fetch_add(1, Ordering::AcqRel),
                0,
                "injected worker failure"
            );
            compute_hunks(source)
        });
        assert!(matches!(
            engine.compare(request("new\n")).await,
            Err(EngineError::WorkerFailed)
        ));
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().cache.admitted_entries, 0);
        assert!(engine.compare(request("new\n")).await.is_ok());
    }
}
