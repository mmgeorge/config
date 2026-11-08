use std::{sync::mpsc, time::Duration};

use forge_git::read_pool::{BlockingReadPool, ReadAdmissionError};

#[tokio::test]
async fn dropped_waiter_retains_capacity_until_native_read_exits() {
    let pool = BlockingReadPool::new(1).unwrap();
    let (started, ready) = tokio::sync::oneshot::channel();
    let (release, wait) = mpsc::channel();
    let task = pool
        .submit(8, move |cancellation| {
            started.send(()).unwrap();
            wait.recv().unwrap();
            assert!(cancellation.is_requested());
            Ok(())
        })
        .unwrap();
    let id = task.id();
    ready.await.unwrap();
    drop(task);
    assert_eq!(pool.status().active_jobs, 1);
    assert_eq!(pool.status().reserved_input_bytes, 8);
    assert_eq!(
        pool.submit(1, |_| Ok(())).err().unwrap(),
        ReadAdmissionError::Busy
    );
    let report = pool.shutdown(Duration::ZERO).await;
    assert_eq!(report.unfinished, vec![id]);
    assert_eq!(
        pool.submit(1, |_| Ok(())).err().unwrap(),
        ReadAdmissionError::Closed
    );
    release.send(()).unwrap();
    assert!(
        pool.shutdown(Duration::from_secs(2))
            .await
            .unfinished
            .is_empty()
    );
    assert_eq!(pool.status().reserved_input_bytes, 0);
}

#[tokio::test]
async fn worker_panic_releases_capacity_and_is_reported_to_its_waiter() {
    let pool = BlockingReadPool::new(1).unwrap();
    let task = pool
        .submit::<()>(8, |_| panic!("synthetic read panic"))
        .unwrap();
    assert!(task.finish().await.is_err());
    assert_eq!(pool.status().active_jobs, 0);
    assert_eq!(pool.status().reserved_input_bytes, 0);
    pool.submit(8, |_| Ok(())).unwrap().finish().await.unwrap();
}

#[test]
fn cancelled_queued_work_retains_admission_and_never_runs_its_operation() {
    use std::sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    };
    let runtime = tokio::runtime::Builder::new_current_thread()
        .max_blocking_threads(1)
        .enable_time()
        .build()
        .unwrap();
    runtime.block_on(async {
        let pool = BlockingReadPool::new(2).unwrap();
        let (started, ready) = tokio::sync::oneshot::channel();
        let (release, wait) = mpsc::channel();
        let first = pool
            .submit(4, move |_| {
                started.send(()).unwrap();
                wait.recv().unwrap();
                Ok(())
            })
            .unwrap();
        ready.await.unwrap();
        let executed = Arc::new(AtomicBool::new(false));
        let observed = Arc::clone(&executed);
        let queued = pool
            .submit(4, move |_| {
                observed.store(true, Ordering::Release);
                Ok(())
            })
            .unwrap();
        queued.cancel();
        assert_eq!(pool.status().active_jobs, 2);
        assert_eq!(
            pool.submit(0, |_| Ok(())).err().unwrap(),
            ReadAdmissionError::Busy
        );
        release.send(()).unwrap();
        first.finish().await.unwrap();
        assert!(queued.finish().await.is_err());
        assert!(!executed.load(Ordering::Acquire));
        assert_eq!(pool.status().active_jobs, 0);
        assert_eq!(pool.status().reserved_input_bytes, 0);
    });
}
