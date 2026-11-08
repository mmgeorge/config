use std::time::Duration;

use forge_diff::workers::{AnalysisPool, PoolLimits, PoolShutdown, WorkBudget, WorkPriority};

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
