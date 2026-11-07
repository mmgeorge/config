//! Keep decoder fixtures behind the actual retained turn lifecycle capability.

use std::sync::Arc;

use async_trait::async_trait;
use forge_provider_api::{
    NativeTurnStart, ProviderError, ProviderEventSink, TurnAddress, TurnResult,
};
use tokio::sync::{Mutex, oneshot, watch};

/// Retain the real worker until the fixture supplies a result or drops its native producer.
pub(super) struct NativeTurnFixture {
    pub(super) start: NativeTurnStart,
    result: oneshot::Sender<TurnResult>,
    runner: crate::turn_runner::RetainedTurn,
}

impl NativeTurnFixture {
    /// Collect the actual fixture driver and its canonical native result.
    pub(super) async fn complete(self, result: TurnResult) {
        self.result
            .send(result)
            .expect("fixture driver retains its result receiver");
        self.runner
            .join()
            .await
            .expect("fixture native collection succeeds");
    }
}

struct Driver {
    start: Mutex<Option<oneshot::Sender<NativeTurnStart>>>,
    result: Mutex<Option<oneshot::Receiver<TurnResult>>>,
    cancelled: watch::Sender<bool>,
}

#[async_trait]
impl crate::turn_runner::NativeTurnDriver for Driver {
    /// Hand the positive-start capability to the native fixture while retaining its result producer.
    async fn run(&self, start: NativeTurnStart) -> Result<TurnResult, ProviderError> {
        let result = self
            .result
            .lock()
            .await
            .take()
            .expect("fixture driver runs once");
        let mut cancelled = self.cancelled.subscribe();
        self.start
            .lock()
            .await
            .take()
            .expect("fixture driver starts once")
            .send(start)
            .map_err(|_| ProviderError::Cancelled)?;
        tokio::select! {
            result = result => result.map_err(|_| ProviderError::Cancelled),
            _ = cancelled.changed() => Err(ProviderError::Cancelled),
        }
    }

    /// Interrupt the retained native fixture without cancelling its collection worker.
    async fn interrupt(&self) -> Result<(), ProviderError> {
        self.cancelled.send_replace(true);
        Ok(())
    }

    /// Finish the fixture's callback-free native collection boundary.
    async fn collect(&self) -> Result<(), ProviderError> {
        Ok(())
    }
}

/// Create a real retained driver with an owned producer that closes when its fixture is dropped.
pub(super) async fn native_turn(
    sink: ProviderEventSink,
    address: TurnAddress,
) -> NativeTurnFixture {
    let (start, started) = oneshot::channel();
    let (result, received) = oneshot::channel();
    let (cancelled, _) = watch::channel(false);
    let driver = Arc::new(Driver {
        start: Mutex::new(Some(start)),
        result: Mutex::new(Some(received)),
        cancelled,
    });
    let runner = crate::turn_runner::RetainedTurn::spawn(driver, sink, address)
        .expect("fixture scope admits one retained native driver");
    NativeTurnFixture {
        start: started
            .await
            .expect("fixture driver publishes its start capability"),
        result,
        runner,
    }
}
