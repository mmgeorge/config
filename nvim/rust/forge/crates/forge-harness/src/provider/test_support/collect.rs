//! Result-oriented test observers over the production session submission contract.
use crate::provider::ScopeOwner;

use forge_harness as harness;

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use harness::provider::session_execution::OwnerObservation;
use harness::provider::{
    HarnessEvent, HarnessEventSink, InvocationScope, ProviderCompletion, ProviderError,
    ProviderOutput, ProviderRequest, ThreadOwner,
};
use tokio::sync::Mutex;

/// Retain one observer per owner across complete-result assertions.
pub struct ProviderTestSession {
    provider: Arc<harness::provider::Provider>,
    registry: Arc<harness::provider::session_execution::SessionExecutionRegistry>,
    owners: Arc<Mutex<HashMap<ThreadOwner, Arc<TestOwner>>>>,
    store: Arc<harness::provider::submission::SubmissionStore>,
    _directory: Arc<tempfile::TempDir>,
}

struct TestOwner {
    engine: Arc<harness::provider::session_execution::SessionExecution>,
    idle: Arc<Mutex<IdleObservations>>,
    observer: Mutex<Option<tokio::task::JoinHandle<()>>>,
}

#[derive(Default)]
struct IdleObservations {
    events: Vec<HarnessEvent>,
    failure: Option<forge_provider_api::lifecycle::ObservationFailure>,
}

impl Drop for TestOwner {
    fn drop(&mut self) {
        if let Some(observer) = self.observer.get_mut().take() {
            observer.abort();
        }
    }
}

impl ProviderTestSession {
    /// Wrap a normalized native provider in the production bridge for isolated tests.
    pub fn from_native(native: Arc<super::fixture::EventFixtureProvider>) -> anyhow::Result<Self> {
        Self::from_registered(native.registration())
    }

    /// Attach an adapter and explicit immutable metadata without reconstructing its descriptor.
    pub fn from_registered(
        registration: forge_providers::registry::RegisteredProvider,
    ) -> anyhow::Result<Self> {
        let directory = Arc::new(tempfile::tempdir()?);
        let permission_store = Arc::new(std::sync::RwLock::new(
            harness::permissions::store::PermissionStore::load(
                directory.path().join("permissions.json"),
                directory.path(),
            )?,
        ));
        let permissions = Arc::new(harness::provider::approval::PermissionCoordinator::new(
            permission_store,
        ));
        let trace = Arc::new(harness::trace::TraceStore::open(directory.path())?);
        let provider = Arc::new(harness::provider::Provider::new(
            registration,
            permissions,
            trace.clone(),
            directory.path().to_path_buf(),
        ));
        Self::from_dyn_with_directory(provider, directory, trace)
    }

    fn from_dyn_with_directory(
        provider: Arc<harness::provider::Provider>,
        directory: Arc<tempfile::TempDir>,
        trace: Arc<harness::trace::TraceStore>,
    ) -> anyhow::Result<Self> {
        let store =
            harness::provider::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let registry = Arc::new(
            harness::provider::session_execution::SessionExecutionRegistry::new(
                provider.clone(),
                trace,
            ),
        );
        Ok(Self {
            provider,
            registry,
            owners: Arc::default(),
            store,
            _directory: directory,
        })
    }

    /// Get the production provider interface for journal reconciliation.
    pub fn as_provider(&self) -> &harness::provider::Provider {
        self.provider.as_ref()
    }

    /// Clone the production bridge for service or controller test constructors.
    pub fn provider_handle(&self) -> Arc<harness::provider::Provider> {
        self.provider.clone()
    }

    /// Run bounded retry-policy attempts through the production session engine and bridge.
    pub async fn session_prompt_with_retry(
        &self,
        request: ProviderRequest,
        sink: Option<HarnessEventSink>,
        trace: &harness::trace::TraceStore,
        store: &Arc<harness::provider::submission::SubmissionStore>,
    ) -> Result<ProviderOutput, ProviderError> {
        let owner = request.owner()?;
        let scope = match &sink {
            Some(sink) => sink.invocation_scope()?.clone(),
            None => InvocationScope::new(owner.clone(), uuid::Uuid::new_v4().to_string()),
        };
        if scope.owner() != owner {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ),
            ));
        }
        let execution = self.owner(owner).await?;
        let (output, mut stream) = harness::provider::events::channel(scope);
        let mut completion: std::pin::Pin<
            Box<
                dyn std::future::Future<Output = Result<ProviderCompletion, ProviderError>>
                    + Send
                    + '_,
            >,
        > = Box::pin(async {
            harness::provider::retry::execute(&execution.engine, request, output, trace, store)
                .await
                .map_err(|error| {
                    harness::provider::lifecycle_error::classify(
                        forge_provider_api::ProviderOperation::TurnStart,
                        error,
                    )
                })
        });
        let mut observed = Vec::new();
        let mut stream_open = true;
        // The retry owner admits fresh invocations in this exchange. Preserve each exact
        // scope when mirroring host output; a native sink never acquires broader authority.
        let relay_sink =
            |event: &OwnerObservation| -> Result<Option<HarnessEventSink>, ProviderError> {
                let Some(sink) = &sink else {
                    return Ok(None);
                };
                let event_scope = event.scope();
                if &event_scope == sink.scope() {
                    return Ok(Some(sink.clone()));
                }
                match event_scope.invocation() {
                    Some(invocation) => Ok(Some(sink.successor_invocation_sink(invocation)?)),
                    None => Ok(Some(sink.clone())),
                }
            };
        loop {
            tokio::select! {
                result = &mut completion => {
                    while let Ok(event) = stream.try_recv() {
                        if let Some(sink) = relay_sink(&event)? {
                            sink.send_observation(event.clone()).await?;
                        }
                        if let OwnerObservation::Event(event) = event { observed.push(event); }
                    }
                    return Ok(ProviderOutput { event: observed, completion: result? });
                }
                event = stream.recv(), if stream_open => {
                    let Some(event) = event? else { stream_open = false; continue; };
                    if let Some(sink) = relay_sink(&event)? {
                        sink.send_observation(event.clone()).await?;
                    }
                    if let OwnerObservation::Event(event) = event { observed.push(event); }
                }
            }
        }
    }

    /// Replace a failed owner through the production recovery path before another test request.
    pub async fn recover(&self, owner: ThreadOwner) -> anyhow::Result<()> {
        self.registry.owner(owner.clone()).await?.close().await?;
        let inbox = self.registry.observer(owner.clone())?;
        // This fixture replaces inbox delivery with its separately joined stream.
        inbox.close();
        loop {
            match inbox.drain() {
                Ok(events) if events.is_empty() => break,
                Ok(_) => {}
                Err(ProviderError::Observation(_)) => {
                    break;
                }
                Err(error) => return Err(error.into()),
            }
        }
        self.registry.replace(owner.clone()).await?;
        if let Some(previous) = self.owners.lock().await.remove(&owner)
            && let Some(observer) = previous.observer.lock().await.take()
        {
            observer.await?;
        }
        self.owner(owner).await?;
        Ok(())
    }

    /// Close all engines and join their independently retained observers.
    pub async fn close(&self) -> anyhow::Result<()> {
        self.registry.close().await?;
        let owners: Vec<_> = self.owners.lock().await.values().cloned().collect();
        let mut observer_failure = None;
        for owner in owners {
            let mut observer = owner.observer.lock().await;
            if let Some(handle) = observer.as_mut() {
                if let Err(error) = handle.await {
                    observer_failure.get_or_insert(error);
                }
                observer.take();
            }
        }
        match observer_failure {
            None => Ok(()),
            Some(error) => Err(anyhow::Error::new(error).context("test observer task failed")),
        }
    }

    /// Keep the engine and its idle observer alive independently of result waiters.
    async fn owner(&self, owner: ThreadOwner) -> Result<Arc<TestOwner>, ProviderError> {
        let mut owners = self.owners.lock().await;
        if let Some(existing) = owners.get(&owner) {
            if let Some(failure) = existing.idle.lock().await.failure.clone() {
                return Err(ProviderError::Observation(failure));
            }
            return Ok(existing.clone());
        }
        let engine = self.registry.owner(owner.clone()).await?;
        let (_, mut stream) = engine.session_observer().await?;
        let idle = Arc::new(Mutex::new(IdleObservations::default()));
        let retained = idle.clone();
        let observer = tokio::spawn(async move {
            loop {
                let event = match stream.recv().await {
                    Ok(Some(event)) => event,
                    Ok(None) => break,
                    Err(error) => {
                        retained.lock().await.failure = Some(match error {
                            harness::provider::events::EventDeliveryError::Observation(failure) => {
                                failure
                            }
                            harness::provider::events::EventDeliveryError::OwnershipMismatch {
                                ..
                            } => {
                                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict
                            }
                            _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                        });
                        break;
                    }
                };
                let mut idle = retained.lock().await;
                if let OwnerObservation::Event(event) = event {
                    idle.events.push(event);
                }
            }
        });
        let execution = Arc::new(TestOwner {
            engine,
            idle,
            observer: Mutex::new(Some(observer)),
        });
        owners.insert(owner, execution.clone());
        Ok(execution)
    }

    /// Collect active events while the production engine owns admission and settlement.
    async fn execute(
        &self,
        request: ProviderRequest,
        sink: Option<HarnessEventSink>,
    ) -> Result<ProviderOutput, ProviderError> {
        let owner = request.owner()?;
        let scope = match &sink {
            Some(sink) => sink.invocation_scope()?.clone(),
            None => InvocationScope::new(owner.clone(), uuid::Uuid::new_v4().to_string()),
        };
        if scope.owner() != owner {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ),
            ));
        }
        let execution = self.owner(owner).await?;
        let (output, mut stream) = harness::provider::events::channel(scope.clone());
        let mut completion: std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<
                            harness::provider::session_execution::NativeReady,
                            ProviderError,
                        >,
                    > + Send
                    + '_,
            >,
        > = Box::pin(
            execution
                .engine
                .run_until_ready(request, output, &self.store),
        );
        let mut observed = Vec::new();
        let mut stream_open = true;
        loop {
            tokio::select! {
                result = &mut completion => {
                    while let Ok(event) = stream.try_recv() {
                        if let Some(sink) = &sink {
                            sink.send_observation(event.clone()).await?;
                        }
                        if let OwnerObservation::Event(event) = event { observed.push(event); }
                    }
                    let ready = result?;
                    match ready.ticket.commit().await {
                        Ok(()) => return Ok(ProviderOutput { event: observed, completion: ready.outcome? }),
                        Err(harness::provider::session_execution::SettlementError::Stale) => {
                            completion = Box::pin(execution.engine.wait_ready(scope.clone()));
                        }
                        Err(harness::provider::session_execution::SettlementError::Unavailable) => {
                            return Err(ProviderError::Observation(
                                forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
                            ));
                        }
                        Err(harness::provider::session_execution::SettlementError::Provider(error)) => return Err(error),
                    }
                }
                event = stream.recv(), if stream_open => {
                    let Some(event) = event? else {
                        stream_open = false;
                        continue;
                    };
                    if let Some(sink) = &sink {
                        sink.send_observation(event.clone()).await?;
                    }
                    if let OwnerObservation::Event(event) = event { observed.push(event); }
                }
            }
        }
    }
}

impl Clone for ProviderTestSession {
    fn clone(&self) -> Self {
        Self {
            provider: self.provider.clone(),
            registry: self.registry.clone(),
            owners: self.owners.clone(),
            store: self.store.clone(),
            _directory: self._directory.clone(),
        }
    }
}

/// Collect complete output above the session API without taking over its native reader.
#[async_trait]
pub trait ProviderTestExt {
    async fn session_prompt_and_wait(
        &self,
        request: ProviderRequest,
        sink: HarnessEventSink,
    ) -> Result<ProviderCompletion, ProviderError>;
}

#[async_trait]
impl ProviderTestExt for ProviderTestSession {
    async fn session_prompt_and_wait(
        &self,
        request: ProviderRequest,
        sink: HarnessEventSink,
    ) -> Result<ProviderCompletion, ProviderError> {
        Ok(self.execute(request, Some(sink)).await?.completion)
    }
}
