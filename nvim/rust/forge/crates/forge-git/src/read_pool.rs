use std::collections::BTreeMap;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use tokio::sync::watch;
use tokio::task::JoinHandle;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ReadId(u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReadAdmissionError {
    Closed,
    Busy,
    InputTooLarge,
    NoRuntime,
    IdExhausted,
}

impl fmt::Display for ReadAdmissionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Closed => "repository read pool is closed",
            Self::Busy => "repository read capacity is full",
            Self::InputTooLarge => "repository read input exceeds the pool budget",
            Self::NoRuntime => "repository reads require an active Tokio runtime",
            Self::IdExhausted => "repository read identities are exhausted",
        })
    }
}

impl std::error::Error for ReadAdmissionError {}

#[derive(Clone)]
pub struct ReadCancellation {
    requested: Arc<AtomicBool>,
}

impl ReadCancellation {
    pub fn is_requested(&self) -> bool {
        self.requested.load(Ordering::Acquire)
    }

    pub fn check(&self) -> Result<()> {
        ensure!(!self.is_requested(), "repository read cancelled");
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReadPoolStatus {
    /// Admitted jobs, including completed results awaiting collection or disposal.
    pub active_jobs: usize,
    pub reserved_input_bytes: usize,
    pub closed: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReadShutdown {
    /// Jobs still retaining admission, including uncollected completed results.
    pub unfinished: Vec<ReadId>,
}

/// Bounds admitted blocking reads, including queued work and completed, uncollected results.
pub struct BlockingReadPool {
    shared: Arc<PoolShared>,
}

struct PoolShared {
    max_jobs: usize,
    max_input_bytes: usize,
    state: Mutex<PoolState>,
    changed: watch::Sender<usize>,
}

#[derive(Default)]
struct PoolState {
    closed: bool,
    next_id: u64,
    input_bytes: usize,
    active: BTreeMap<ReadId, ReadCancellation>,
}

struct ReadReservation {
    shared: Arc<PoolShared>,
    id: ReadId,
    input_bytes: usize,
}

/// Owns one result waiter. Dropping it requests cancellation without releasing worker capacity.
pub struct ReadTask<Output> {
    id: ReadId,
    cancellation: ReadCancellation,
    handle: Option<JoinHandle<ReadCompletion<Output>>>,
}

struct ReadCompletion<Output> {
    // Dispose uncollected output before releasing its admission charge.
    result: Result<Output>,
    _reservation: ReadReservation,
}

impl BlockingReadPool {
    pub fn new(max_jobs: usize, max_input_bytes: usize) -> Result<Self> {
        ensure!(
            max_jobs > 0 && max_input_bytes > 0,
            "repository read limits must be positive"
        );
        let (changed, _) = watch::channel(0);
        Ok(Self {
            shared: Arc::new(PoolShared {
                max_jobs,
                max_input_bytes,
                state: Mutex::new(PoolState::default()),
                changed,
            }),
        })
    }

    /// Admits one job immediately or returns a capacity error without spawning it.
    ///
    /// `input_bytes` accounts for retained caller input. The worker must separately bound source
    /// allocation and result size, and check cancellation between interruptible stages.
    /// Admission remains charged until `finish` collects the result or the abandoned result drops.
    pub fn submit<Output: Send + 'static>(
        &self,
        input_bytes: usize,
        work: impl FnOnce(ReadCancellation) -> Result<Output> + Send + 'static,
    ) -> std::result::Result<ReadTask<Output>, ReadAdmissionError> {
        let runtime =
            tokio::runtime::Handle::try_current().map_err(|_| ReadAdmissionError::NoRuntime)?;
        let cancellation = ReadCancellation {
            requested: Arc::new(AtomicBool::new(false)),
        };
        let id = {
            let mut state = self
                .shared
                .state
                .lock()
                .expect("repository read state lock");
            if state.closed {
                return Err(ReadAdmissionError::Closed);
            }
            if input_bytes > self.shared.max_input_bytes {
                return Err(ReadAdmissionError::InputTooLarge);
            }
            if state.active.len() == self.shared.max_jobs
                || input_bytes > self.shared.max_input_bytes - state.input_bytes
            {
                return Err(ReadAdmissionError::Busy);
            }
            state.next_id = state
                .next_id
                .checked_add(1)
                .ok_or(ReadAdmissionError::IdExhausted)?;
            let id = ReadId(state.next_id);
            state.input_bytes += input_bytes;
            state.active.insert(id, cancellation.clone());
            self.shared.changed.send_replace(state.active.len());
            id
        };
        let reservation = ReadReservation {
            shared: Arc::clone(&self.shared),
            id,
            input_bytes,
        };
        let worker_cancellation = cancellation.clone();
        let handle = runtime.spawn_blocking(move || {
            let result = (move || {
                worker_cancellation.check()?;
                let result = work(worker_cancellation.clone());
                worker_cancellation.check()?;
                result
            })();
            ReadCompletion {
                result,
                _reservation: reservation,
            }
        });
        Ok(ReadTask {
            id,
            cancellation,
            handle: Some(handle),
        })
    }

    pub fn status(&self) -> ReadPoolStatus {
        let state = self
            .shared
            .state
            .lock()
            .expect("repository read state lock");
        ReadPoolStatus {
            active_jobs: state.active.len(),
            reserved_input_bytes: state.input_bytes,
            closed: state.closed,
        }
    }

    /// Stops admission, signals cancellation, and reports retained jobs at the deadline.
    ///
    /// A timeout does not terminate native work or discard uncollected results. Their reservations
    /// remain charged, and a later shutdown call can observe worker exit and result disposal.
    pub async fn shutdown(&self, deadline: Duration) -> ReadShutdown {
        let mut changed = self.shared.changed.subscribe();
        self.shared.close();
        let _ = tokio::time::timeout(deadline, async {
            loop {
                if *changed.borrow_and_update() == 0 {
                    break;
                }
                if changed.changed().await.is_err() {
                    break;
                }
            }
        })
        .await;
        let state = self
            .shared
            .state
            .lock()
            .expect("repository read state lock");
        ReadShutdown {
            unfinished: state.active.keys().copied().collect(),
        }
    }
}

impl PoolShared {
    fn close(&self) {
        let mut state = self.state.lock().expect("repository read state lock");
        state.closed = true;
        for cancellation in state.active.values() {
            cancellation.requested.store(true, Ordering::Release);
        }
    }
}

impl Drop for BlockingReadPool {
    fn drop(&mut self) {
        self.shared.close();
    }
}

impl Drop for ReadReservation {
    fn drop(&mut self) {
        let mut state = self
            .shared
            .state
            .lock()
            .expect("repository read state lock");
        state.active.remove(&self.id);
        state.input_bytes -= self.input_bytes;
        self.shared.changed.send_replace(state.active.len());
    }
}

impl<Output> ReadTask<Output> {
    pub fn id(&self) -> ReadId {
        self.id
    }

    pub fn cancel(&self) {
        self.cancellation.requested.store(true, Ordering::Release);
    }

    pub async fn finish(mut self) -> Result<Output> {
        let completion = self
            .handle
            .take()
            .expect("repository read result owner")
            .await
            .context("repository read worker did not complete normally")?;
        self.cancellation.check()?;
        completion.result
    }
}

impl<Output> Drop for ReadTask<Output> {
    fn drop(&mut self) {
        self.cancel();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn wait_for_native_completion<Output>(task: &ReadTask<Output>) {
        tokio::time::timeout(Duration::from_secs(2), async {
            while !task.handle.as_ref().unwrap().is_finished() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn completed_result_retains_admission_until_collected() {
        let pool = BlockingReadPool::new(1, 8).unwrap();
        let owner = std::thread::current().id();
        let task = pool.submit(8, |_| Ok(std::thread::current().id())).unwrap();
        wait_for_native_completion(&task).await;
        assert_eq!(pool.status().active_jobs, 1);
        assert_eq!(pool.status().reserved_input_bytes, 8);
        assert_eq!(
            pool.submit(0, |_| Ok(())).err(),
            Some(ReadAdmissionError::Busy)
        );
        assert_ne!(task.finish().await.unwrap(), owner);
        assert_eq!(pool.status().active_jobs, 0);
        assert_eq!(pool.status().reserved_input_bytes, 0);
        pool.submit(8, |_| Ok(())).unwrap().finish().await.unwrap();
    }

    #[tokio::test]
    async fn shutdown_reports_uncollected_results_and_cancellation_rejects_them() {
        let pool = BlockingReadPool::new(1, 8).unwrap();
        let task = pool.submit(4, |_| Ok("completed result")).unwrap();
        let id = task.id();
        wait_for_native_completion(&task).await;
        assert_eq!(pool.shutdown(Duration::ZERO).await.unfinished, vec![id]);
        assert!(
            task.finish()
                .await
                .unwrap_err()
                .to_string()
                .contains("cancelled")
        );
        assert_eq!(pool.status().active_jobs, 0);
        assert!(pool.shutdown(Duration::ZERO).await.unfinished.is_empty());
    }

    #[tokio::test]
    async fn abandoned_completed_output_drops_before_its_reservation() {
        struct ObservedOutput {
            pool: Arc<BlockingReadPool>,
            disposed: Arc<AtomicBool>,
        }

        impl Drop for ObservedOutput {
            fn drop(&mut self) {
                assert_eq!(self.pool.status().active_jobs, 1);
                assert_eq!(self.pool.status().reserved_input_bytes, 8);
                self.disposed.store(true, Ordering::Release);
            }
        }

        let pool = Arc::new(BlockingReadPool::new(1, 8).unwrap());
        let disposed = Arc::new(AtomicBool::new(false));
        let output = ObservedOutput {
            pool: Arc::clone(&pool),
            disposed: Arc::clone(&disposed),
        };
        let task = pool.submit(8, |_| Ok(output)).unwrap();
        wait_for_native_completion(&task).await;
        drop(task);
        assert!(disposed.load(Ordering::Acquire));
        assert_eq!(pool.status().active_jobs, 0);
        assert_eq!(pool.status().reserved_input_bytes, 0);
    }
}
