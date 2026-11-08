use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use forge_diff::workers::{
    AnalysisPool, PoolLimits, PoolShutdown, WorkBudget, WorkPriority, WorkStop,
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
    });
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, blocked) = std::sync::mpsc::channel();
    pool.submit(
        WorkPriority::Foreground,
        WorkBudget::new(10, None),
        move |_| {
            started.send(()).unwrap();
            blocked.recv().unwrap();
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
                blocked.recv().unwrap();
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
async fn panicking_work_releases_its_slot_and_preserves_the_dedicated_worker() {
    let pool = AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
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
