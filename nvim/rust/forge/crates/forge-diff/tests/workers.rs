use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};

use forge_diff::workers::{
    AnalysisPool, PoolError, PoolLimits, PoolShutdown, PoolUsage, WorkBudget, WorkPriority,
    WorkStop,
};

struct InputOwner(Arc<AtomicBool>);

impl Drop for InputOwner {
    fn drop(&mut self) {
        self.0.store(true, Ordering::Release);
    }
}

#[tokio::test]
async fn priority_queues_preserve_fifo_and_cancelled_inputs_drop_before_capacity_returns() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 6,
        input_bytes: 60,
    });
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, blocked) = std::sync::mpsc::channel();
    pool.submit(
        WorkPriority::Foreground,
        WorkBudget::new(10, None),
        move |_| {
            started.send(()).unwrap();
            blocked.recv_timeout(Duration::from_secs(5)).unwrap();
        },
    )
    .unwrap();
    ready.await.unwrap();
    let dropped = Arc::new(AtomicBool::new(false));
    let owner = InputOwner(Arc::clone(&dropped));
    let cancelled = pool
        .submit(
            WorkPriority::Speculative,
            WorkBudget::new(10, None),
            move |_| {
                drop(owner);
                panic!("cancelled queued work must not execute");
            },
        )
        .unwrap();
    let foreign = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 10,
    });
    assert!(!foreign.cancel(&cancelled));
    assert!(pool.cancel(&cancelled));
    assert!(dropped.load(Ordering::Acquire));
    assert_eq!(pool.usage().admitted_jobs, 1);
    assert_eq!(pool.usage().input_bytes, 10);
    assert!(!pool.cancel(&cancelled));
    assert!(!pool.promote(&cancelled, WorkPriority::Foreground));
    let (output, received) = std::sync::mpsc::channel();
    for (priority, label) in [
        (WorkPriority::Speculative, "speculative"),
        (WorkPriority::Visible, "visible-first"),
        (WorkPriority::Foreground, "foreground"),
        (WorkPriority::Visible, "visible-second"),
    ] {
        let output = output.clone();
        pool.submit(priority, WorkBudget::new(10, None), move |_| {
            output.send(label).unwrap();
        })
        .unwrap();
    }
    release.send(()).unwrap();
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
    let actual: Vec<_> = received.try_iter().collect();
    assert_eq!(
        actual,
        [
            "foreground",
            "visible-first",
            "visible-second",
            "speculative"
        ]
    );
}

#[tokio::test]
async fn running_cancellation_and_shutdown_expiry_retain_native_admission() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 8,
    });
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, blocked) = std::sync::mpsc::channel();
    let (stopped, observed) = tokio::sync::oneshot::channel();
    let ticket = pool
        .submit(
            WorkPriority::Foreground,
            WorkBudget::new(8, None),
            move |budget| {
                assert!(
                    std::thread::current()
                        .name()
                        .unwrap()
                        .starts_with("forge-analysis-")
                );
                started.send(()).unwrap();
                blocked.recv_timeout(Duration::from_secs(5)).unwrap();
                stopped.send(budget.check()).unwrap();
            },
        )
        .unwrap();
    ready.await.unwrap();
    assert!(pool.cancel(&ticket));
    let unfinished = pool.shutdown(Duration::ZERO).await;
    assert_eq!(unfinished.usage.admitted_jobs, 1);
    assert_eq!(unfinished.usage.running_jobs, 1);
    assert_eq!(unfinished.usage.input_bytes, 8);
    assert_eq!(unfinished.usage.live_workers, 1);
    assert_eq!(unfinished.unfinished_workers, 1);
    release.send(()).unwrap();
    assert_eq!(observed.await.unwrap(), Err(WorkStop::Cancelled));
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
}

#[tokio::test]
async fn promotion_preserves_admission_and_appends_after_existing_foreground_work() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 5,
        input_bytes: 50,
    });
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, blocked) = std::sync::mpsc::channel();
    let running = pool
        .submit(
            WorkPriority::Speculative,
            WorkBudget::new(10, None),
            move |_| {
                started.send(()).unwrap();
                blocked.recv_timeout(Duration::from_secs(5)).unwrap();
            },
        )
        .unwrap();
    ready.await.unwrap();
    let (output, received) = std::sync::mpsc::channel();
    let promoted_output = output.clone();
    let promoted = pool
        .submit(
            WorkPriority::Speculative,
            WorkBudget::new(10, None),
            move |_| {
                promoted_output.send("promoted").unwrap();
            },
        )
        .unwrap();
    for (priority, label) in [
        (WorkPriority::Foreground, "foreground-first"),
        (WorkPriority::Visible, "visible"),
        (WorkPriority::Speculative, "remaining-speculative"),
    ] {
        let output = output.clone();
        pool.submit(priority, WorkBudget::new(10, None), move |_| {
            output.send(label).unwrap();
        })
        .unwrap();
    }
    let before = pool.usage();
    let foreign = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 10,
    });
    assert!(!foreign.promote(&promoted, WorkPriority::Foreground));
    assert!(!pool.promote(&running, WorkPriority::Foreground));
    assert!(pool.promote(&promoted, WorkPriority::Visible));
    assert!(!pool.promote(&promoted, WorkPriority::Visible));
    assert!(!pool.promote(&promoted, WorkPriority::Speculative));
    assert!(pool.promote(&promoted, WorkPriority::Foreground));
    assert_eq!(pool.usage(), before);
    release.send(()).unwrap();
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
    assert_eq!(
        received.try_iter().collect::<Vec<_>>(),
        [
            "foreground-first",
            "promoted",
            "visible",
            "remaining-speculative"
        ]
    );
    assert!(!pool.promote(&promoted, WorkPriority::Foreground));
}

#[tokio::test]
async fn reservations_bound_inputs_before_transfer_and_survive_admission_close() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
        input_bytes: 8,
    });
    assert_eq!(pool.usage(), PoolUsage::default());
    let accepted = pool
        .reserve(WorkPriority::Visible, WorkBudget::new(8, None))
        .unwrap();
    assert!(matches!(
        pool.reserve(WorkPriority::Foreground, WorkBudget::new(1, None)),
        Err(PoolError::Busy)
    ));
    assert!(matches!(
        pool.reserve(WorkPriority::Foreground, WorkBudget::new(9, None)),
        Err(PoolError::Oversized)
    ));
    drop(accepted);
    assert_eq!(pool.usage().input_bytes, 0);
    let accepted = pool
        .reserve(WorkPriority::Visible, WorkBudget::new(8, None))
        .unwrap();
    pool.close();
    assert!(matches!(
        pool.reserve(WorkPriority::Foreground, WorkBudget::new(0, None)),
        Err(PoolError::Closed)
    ));
    let (output, received) = tokio::sync::oneshot::channel();
    accepted.submit(move |_| {
        output.send(()).unwrap();
    });
    received.await.unwrap();
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
}

#[tokio::test]
async fn panicking_work_releases_its_slot_and_preserves_the_dedicated_worker() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
        input_bytes: 16,
    });
    pool.submit(WorkPriority::Foreground, WorkBudget::new(8, None), |_| {
        panic!("injected native analysis failure");
    })
    .unwrap();
    let (output, received) = tokio::sync::oneshot::channel();
    pool.submit(
        WorkPriority::Foreground,
        WorkBudget::new(8, None),
        move |_| {
            output.send(()).unwrap();
        },
    )
    .unwrap();
    received.await.unwrap();
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
}

#[tokio::test]
async fn a_deadline_that_expires_after_reservation_drops_input_without_execution() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 8,
    });
    let deadline = Instant::now() + Duration::from_millis(100);
    let permit = pool
        .reserve(WorkPriority::Visible, WorkBudget::new(8, Some(deadline)))
        .unwrap();
    let dropped = Arc::new(AtomicBool::new(false));
    let ran = Arc::new(AtomicBool::new(false));
    let executed = Arc::clone(&ran);
    let owner = InputOwner(Arc::clone(&dropped));
    std::thread::sleep(deadline.saturating_duration_since(Instant::now()));
    permit.submit(move |_| {
        executed.store(true, Ordering::Release);
        drop(owner);
    });
    assert_eq!(
        pool.shutdown(Duration::from_secs(1)).await,
        PoolShutdown::default()
    );
    assert!(dropped.load(Ordering::Acquire));
    assert!(!ran.load(Ordering::Acquire));
}

#[test]
fn expired_work_is_rejected_before_starting_a_worker() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 8,
    });
    assert!(matches!(
        pool.submit(
            WorkPriority::Foreground,
            WorkBudget::new(8, Some(Instant::now())),
            |_| panic!("expired work must not execute"),
        ),
        Err(PoolError::Stopped(WorkStop::Deadline))
    ));
    assert_eq!(pool.usage(), PoolUsage::default());
}
