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
use crate::source::{SourceError, SourcePair};
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
    Source(SourceError),
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
    compute: Arc<dyn Fn(SourcePair) -> Result<RawDiff, SourceError> + Send + Sync>,
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
            input_bytes: cache.limits().source_bytes,
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
                    .fetch_update(Ordering::AcqRel, Ordering::Acquire, |consumer| {
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
            (self.completion.engine.compute)(self.source).map_err(EngineError::Source)
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
            receiver
                .lock()
                .unwrap()
                .recv_timeout(Duration::from_secs(2))
                .unwrap();
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
        tokio::time::timeout(Duration::from_secs(1), async {
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
        })
        .await
        .unwrap();
        assert_eq!(engine.usage().active_jobs, 1);
        let mut waiting = Box::pin(engine.compare(request("different\n")));
        assert!(std::future::poll_fn(|context| {
            std::task::Poll::Ready(std::future::Future::poll(waiting.as_mut(), context))
        }).await.is_pending());
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
        assert!(std::future::poll_fn(|context| {
            std::task::Poll::Ready(std::future::Future::poll(waiting.as_mut(), context))
        }).await.is_pending());
        drop(waiting);
        release.send(()).unwrap();
        tokio::time::timeout(Duration::from_secs(1), async {
            while {
                let usage = engine.usage();
                usage.active_jobs != 0 || usage.pool.admitted_jobs != 0
            } {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert_eq!(engine.usage().cache.admitted_entries, 0);
        assert_eq!(engine.usage().cache.cached_entries, 0);
        release.send(()).unwrap();
        assert!(engine.compare(request("new\n")).await.is_ok());
    }

    #[tokio::test]
    async fn close_rejects_new_requests_without_releasing_running_work() {
        let (engine, started, release) = blocked_engine();
        let worker_engine = Arc::clone(&engine);
        let caller = tokio::spawn(async move { worker_engine.compare(request("new\n")).await });
        started.notified().await;
        engine.close();
        assert!(matches!(
            engine.compare(request("new\n")).await,
            Err(EngineError::Closed)
        ));
        assert_eq!(engine.usage().active_jobs, 1);
        release.send(()).unwrap();
        assert!(caller.await.unwrap().is_ok());
    }

    #[tokio::test]
    async fn shared_pool_pressure_waits_without_cache_charge_and_close_wakes_waiters() {
        use std::future::Future;
        use std::task::Poll;
        for close in [false, true] {
            let engine = DiffEngine::new(CacheLimits::default(), 1);
            let occupied = engine.pool.reserve(WorkPriority::Visible, WorkBudget::new(0, None)).unwrap();
            let mut pending = Box::pin(engine.compare(request("uncached\n")));
            assert!(std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context))).await.is_pending());
            assert_eq!(engine.usage().active_jobs, 0);
            assert_eq!(engine.usage().cache.admitted_entries, 0);
            if close {
                engine.close();
                assert!(matches!(tokio::time::timeout(Duration::from_secs(2), pending).await.unwrap(), Err(EngineError::Closed)));
                assert_eq!(engine.usage().pool.admitted_jobs, 1);
                drop(occupied);
            } else {
                drop(occupied);
                let result = tokio::time::timeout(Duration::from_secs(2), pending).await.unwrap().unwrap();
                assert_eq!(result.source().new.bytes(), b"uncached\n");
                assert_eq!(engine.usage().pool.admitted_jobs, 0);
            }
        }
    }

    #[tokio::test]
    async fn final_consumer_cancellation_releases_queued_work_before_the_running_job_exits() {
        let (mut engine, started, release) = blocked_engine();
        let owner = Arc::get_mut(&mut engine).unwrap();
        owner.max_jobs = 2;
        owner.pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 1024,
        }));
        let first_engine = Arc::clone(&engine);
        let first = tokio::spawn(async move { first_engine.compare(request("first\n")).await });
        started.notified().await;
        let second_engine = Arc::clone(&engine);
        let second = tokio::spawn(async move { second_engine.compare(request("second\n")).await });
        tokio::time::timeout(Duration::from_secs(1), async {
            while engine.usage().active_jobs != 2 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        second.abort();
        assert!(second.await.is_err_and(|error| error.is_cancelled()));
        assert_eq!(engine.usage().active_jobs, 1);
        assert_eq!(engine.usage().cache.admitted_entries, 1);
        assert_eq!(engine.usage().pool.admitted_jobs, 1);
        assert_eq!(engine.usage().pool.running_jobs, 1);
        release.send(()).unwrap();
        assert!(first.await.unwrap().is_ok());
        assert_eq!(
            engine.shutdown(Duration::from_secs(1)).await,
            EngineShutdown::default()
        );
    }

    #[tokio::test]
    async fn coalesced_consumer_limit_recovers_when_one_waiter_leaves() {
        use std::future::Future;
        use std::task::Poll;

        let (engine, started, release) = blocked_engine();
        let mut caller = Vec::new();
        for _ in 0..MAX_ANALYSIS_CONSUMERS {
            let mut pending = Box::pin(engine.compare(request("new\n")));
            let admission =
                std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context))).await;
            assert!(admission.is_pending());
            caller.push(pending);
        }
        started.notified().await;
        assert_eq!(engine.usage().active_jobs, 1);
        assert!(matches!(
            engine.compare(request("new\n")).await,
            Err(EngineError::ConsumerLimit)
        ));
        drop(caller.pop());
        let mut replacement = Box::pin(engine.compare(request("new\n")));
        assert!(
            std::future::poll_fn(|context| Poll::Ready(replacement.as_mut().poll(context)))
                .await
                .is_pending()
        );
        release.send(()).unwrap();
        assert!(replacement.await.is_ok());
        for pending in caller {
            assert!(pending.await.is_ok());
        }
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().cache.admitted_entries, 1);
    }

    #[tokio::test]
    async fn foreground_interest_promotes_shared_speculation_without_duplicate_comparison() {
        use std::future::Future;
        use std::task::Poll;

        let mut engine = DiffEngine::new(CacheLimits::default(), 4);
        let owner = Arc::get_mut(&mut engine).unwrap();
        owner.pool = Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 4,
            input_bytes: 1024,
        }));
        let started = Arc::new(Notify::new());
        let notification = Arc::clone(&started);
        let (release, blocked) = std::sync::mpsc::channel();
        let blocked = Mutex::new(blocked);
        let (output, received) = std::sync::mpsc::channel();
        owner.compute = Arc::new(move |source| {
            if source.new.text() == "block\n" {
                notification.notify_one();
                blocked
                    .lock()
                    .unwrap()
                    .recv_timeout(Duration::from_secs(5))
                    .unwrap();
            }
            output.send(source.new.text().to_owned()).unwrap();
            compute_hunks(source)
        });
        let first_engine = Arc::clone(&engine);
        let first = tokio::spawn(async move { first_engine.compare(request("block\n")).await });
        started.notified().await;

        let speculative_engine = Arc::clone(&engine);
        let speculative = tokio::spawn(async move {
            let mut requested = request("promoted\n");
            requested.priority = WorkPriority::Speculative;
            speculative_engine.compare(requested).await
        });
        let visible_engine = Arc::clone(&engine);
        let visible = tokio::spawn(async move {
            let mut requested = request("visible\n");
            requested.priority = WorkPriority::Visible;
            visible_engine.compare(requested).await
        });
        tokio::time::timeout(Duration::from_secs(1), async {
            while engine.usage().active_jobs != 3 {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        let mut foreground = Box::pin(engine.compare(request("promoted\n")));
        assert!(
            std::future::poll_fn(|context| Poll::Ready(foreground.as_mut().poll(context)))
                .await
                .is_pending()
        );
        assert_eq!(engine.usage().active_jobs, 3);
        assert_eq!(engine.usage().pool.admitted_jobs, 3);
        drop(foreground);

        release.send(()).unwrap();
        assert!(first.await.unwrap().is_ok());
        let handle = speculative.await.unwrap().unwrap();
        assert!(visible.await.unwrap().is_ok());
        let cached = engine.compare(request("promoted\n")).await.unwrap();
        assert!(std::ptr::eq(&*handle, &*cached));
        assert_eq!(
            received.try_iter().collect::<Vec<_>>(),
            ["block\n", "promoted\n", "visible\n"]
        );
        assert_eq!(
            engine.shutdown(Duration::from_secs(1)).await,
            EngineShutdown::default()
        );
    }

    #[tokio::test]
    async fn shutdown_deadline_preserves_ownership_until_native_exit() {
        let (engine, started, release) = blocked_engine();
        let worker_engine = Arc::clone(&engine);
        let caller = tokio::spawn(async move { worker_engine.compare(request("new\n")).await });
        started.notified().await;
        assert_eq!(
            engine.shutdown(Duration::ZERO).await,
            EngineShutdown {
                unfinished_jobs: 1,
                unfinished_workers: 1,
            }
        );
        assert_eq!(engine.usage().cache.admitted_entries, 1);
        let draining_engine = Arc::clone(&engine);
        let drain =
            tokio::spawn(async move { draining_engine.shutdown(Duration::from_secs(1)).await });
        tokio::task::yield_now().await;
        assert!(!drain.is_finished());
        release.send(()).unwrap();
        assert_eq!(drain.await.unwrap(), EngineShutdown::default());
        assert!(caller.await.unwrap().is_ok());
        assert_eq!(
            engine.shutdown(Duration::ZERO).await,
            EngineShutdown::default()
        );
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
            tokio::time::timeout(Duration::from_secs(1), started.notified())
                .await
                .unwrap();
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

    #[test]
    fn stopped_executor_does_not_own_native_analysis_execution() {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap();
        let executor = runtime.handle().clone();
        runtime.shutdown_timeout(Duration::ZERO);
        let engine = DiffEngine::new(CacheLimits::default(), 1);
        assert!(executor.block_on(engine.compare(request("new\n"))).is_ok());
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().cache.admitted_entries, 1);
        let replacement = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap();
        assert!(
            replacement
                .block_on(engine.compare(request("new\n")))
                .is_ok()
        );
    }

    #[tokio::test]
    async fn published_success_waits_for_native_admission_release() {
        published_outcome_waits_for_native_admission_release(false).await;
    }

    #[tokio::test]
    async fn published_failure_waits_for_native_admission_release() {
        published_outcome_waits_for_native_admission_release(true).await;
    }

    async fn published_outcome_waits_for_native_admission_release(failed: bool) {
        use std::future::Future;
        use std::task::Poll;

        let engine = DiffEngine::new(CacheLimits::default(), 1);
        let source = request("published\n").source;
        let key = AnalysisKey {
            old: source.old.identity(),
            new: source.new.identity(),
        };
        let bytes = reservation_bytes(&source);
        let budget = WorkBudget::new(bytes.source, None);
        let permit = engine
            .pool
            .reserve(WorkPriority::Foreground, budget.clone())
            .unwrap();
        let (sender, _) = watch::channel(None);
        let job = Arc::new(AnalysisJob {
            consumer: AtomicUsize::new(1),
            result: sender,
            schedule: Mutex::new(AnalysisSchedule {
                priority: WorkPriority::Foreground,
                ticket: permit.ticket(),
            }),
        });
        let reservation = {
            let mut state = engine.state.lock().unwrap();
            state.job.insert(key, Arc::clone(&job));
            state.cache.reserve(bytes).unwrap()
        };
        let mut completion = AnalysisCompletion {
            engine: Arc::clone(&engine),
            key,
            job,
            reservation: Some(reservation),
            budget,
        };
        let mut pending = Box::pin(engine.compare(request("published\n")));
        assert!(
            std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context)))
                .await
                .is_pending()
        );
        completion.publish(if failed {
            Err(EngineError::WorkerFailed)
        } else {
            compute_hunks(source).map_err(EngineError::Source)
        });
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().pool.admitted_jobs, 1);
        assert!(
            std::future::poll_fn(|context| Poll::Ready(pending.as_mut().poll(context)))
                .await
                .is_pending()
        );
        drop(permit);
        let outcome = pending.await;
        if failed {
            assert!(matches!(outcome, Err(EngineError::WorkerFailed)));
        } else {
            assert!(outcome.is_ok());
        }
        assert_eq!(engine.usage().pool.admitted_jobs, 0);
        assert!(
            engine
                .compare(request("different uncached\n"))
                .await
                .is_ok()
        );
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

    #[tokio::test]
    async fn exhausted_retention_budget_rejects_before_starting_native_work() {
        let mut engine = DiffEngine::new(
            CacheLimits {
                result_bytes: 0,
                ..CacheLimits::default()
            },
            1,
        );
        Arc::get_mut(&mut engine).unwrap().compute =
            Arc::new(|_| panic!("rejected work must not run"));
        assert!(matches!(
            engine.compare(request("new\n")).await,
            Err(EngineError::Cache(CacheError::Saturated))
        ));
        assert_eq!(engine.usage().active_jobs, 0);
        assert_eq!(engine.usage().cache.admitted_entries, 0);
    }
}
