use anyhow::{Context, Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_git::store::RepositoryStore;
use forge_protocol::{
    message::{Message, RequestEvent, SessionEvent},
    outbound::MessageSender,
};
use serde::Deserialize;
use serde_json::{Value, json};
use std::sync::atomic::{AtomicBool, Ordering};
use std::{collections::HashMap, fs, path::PathBuf, sync::Arc, time::Duration};
use tokio::sync::{Mutex, Semaphore, oneshot};
use tokio::task::JoinSet;

use super::{BackgroundContext, BackgroundIdentity};
use crate::{
    broker::{BrokerRuntime, HarnessBroker, InitializeRequest},
    service::TextGenerationConfig,
    session::SessionStore,
    storage::SqliteStore,
};

/// Captures all immutable inputs for a single Background exchange.
pub struct BackgroundRequest {
    /// Request cancellation retained through admission and provider cleanup.
    pub cancellation: super::CancellationToken,
    /// Current response contract, independent of the conversation's preceding task.
    pub purpose: crate::provider::TextGeneration,
    /// Explicit provider settings, independent of the selected chat.
    pub config: TextGenerationConfig,
    /// Repository and history captured with the current diff.
    pub context: BackgroundContext,
    /// Complete current task input.
    pub input: String,
    /// Request-correlated delivery channel for progress and session errors.
    pub delivery: Option<(MessageSender, u64)>,
}

#[derive(serde::Serialize)]
/// Returns generated text before optional background maintenance finishes.
pub struct BackgroundResult {
    /// Durable session owning the generation and its timeline.
    pub session_id: String,
    /// Validated final assistant message.
    pub text: String,
}

/// Retain the native owner and its observer between tool-free generation requests.
struct RetainedBackground {
    runtime: Arc<BrokerRuntime>,
    session_id: String,
    observer: Arc<crate::provider::session_execution::SessionObservationInbox>,
    leased: AtomicBool,
    last_used: std::sync::Mutex<std::time::Instant>,
}

struct BackgroundLease {
    retained: Arc<RetainedBackground>,
}

enum ObservationContinuity {
    Reliable,
    Collected,
    Lost,
}

impl BackgroundLease {
    fn acquire(retained: Arc<RetainedBackground>) -> Result<Self> {
        ensure!(
            !retained.leased.swap(true, Ordering::AcqRel),
            "Background runtime is already leased"
        );
        Ok(Self { retained })
    }
}

impl Drop for BackgroundLease {
    fn drop(&mut self) {
        *self
            .retained
            .last_used
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = std::time::Instant::now();
        self.retained.leased.store(false, Ordering::Release);
    }
}

/// Owns Background operation lifetimes independently of interactive Harness startup.
pub struct BackgroundManager {
    pub(super) repositories: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    syntax: Arc<forge_diff::syntax::SyntaxEngine>,
    admission: Mutex<HashMap<String, Arc<Mutex<()>>>>,
    tasks: Mutex<JoinSet<()>>,
    runtime_admission: Semaphore,
    preparation: Mutex<()>,
    shutdown: super::CancellationToken,
    retained: Mutex<HashMap<String, Arc<RetainedBackground>>>,
    cleanup_pending: std::sync::Mutex<Vec<Arc<RetainedBackground>>>,
    presentation:
        Mutex<HashMap<String, Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>>>,
}

impl BackgroundManager {
    /// Share native repository and diff ownership with the host.
    pub fn new(
        repositories: Arc<RepositoryStore>,
        diff: Arc<DiffEngine>,
        syntax: Arc<forge_diff::syntax::SyntaxEngine>,
    ) -> Arc<Self> {
        Arc::new(Self {
            repositories,
            diff,
            syntax,
            admission: Mutex::new(HashMap::new()),
            tasks: Mutex::new(JoinSet::new()),
            runtime_admission: Semaphore::new(4),
            preparation: Mutex::new(()),
            shutdown: super::CancellationToken::new(),
            retained: Mutex::new(HashMap::new()),
            cleanup_pending: std::sync::Mutex::new(Vec::new()),
            presentation: Mutex::new(HashMap::new()),
        })
    }

    /// Admit one persistent exchange and retain cleanup after returning its text.
    pub async fn generate(
        self: &Arc<Self>,
        request: BackgroundRequest,
    ) -> Result<BackgroundResult> {
        let (sender, receiver) = oneshot::channel();
        let owner = Arc::clone(self);
        let mut tasks = self.tasks.lock().await;
        ensure!(
            !self.shutdown.is_cancelled(),
            "Background manager is shutting down"
        );
        while tasks.try_join_next().is_some() {}
        tasks.spawn(async move {
            owner.run(request, sender).await;
        });
        drop(tasks);
        receiver
            .await
            .context("Background generation worker stopped")?
    }

    async fn run(
        &self,
        request: BackgroundRequest,
        sender: oneshot::Sender<Result<BackgroundResult>>,
    ) {
        let key = format!(
            "{}:{}",
            request.config.data_root.display(),
            request.context.workspace
        );
        let gate = self
            .admission
            .lock()
            .await
            .entry(key.clone())
            .or_default()
            .clone();
        let admitted = async {
            let admission = gate.lock().await;
            let runtime = self
                .runtime_admission
                .acquire()
                .await
                .expect("Background admission remains open while draining");
            (admission, runtime)
        };
        let (_admission, _runtime) = tokio::select! {
            biased;
            _ = request.cancellation.cancelled() => {
                let _ = sender.send(Err(anyhow::anyhow!("Background generation cancelled")));
                return;
            }
            _ = self.shutdown.cancelled() => {
                let _ = sender.send(Err(anyhow::anyhow!("Background generation cancelled")));
                return;
            }
            admitted = admitted => admitted,
        };
        let prepared = self.prepare(&request, &key).await;
        let (mut broker, runtime, _ownership, model) = match prepared {
            Ok(prepared) => prepared,
            Err(error) => {
                let _ = sender.send(Err(error));
                return;
            }
        };
        let session_id = broker
            .snapshot()
            .expect("prepared Background snapshot")
            .session
            .id;
        let (sink, events) = crate::broker::publication::channel();
        let delivery = request.delivery.clone();
        let progress_session = session_id.clone();
        let forward = tokio::spawn(forward_progress(events, delivery, progress_session));
        let policy = crate::settings::SettingsStore::open(&request.config.data_root)
            .and_then(|store| {
                store.get(
                    &request.context.workspace,
                    &session_id,
                    request.config.provider.kind.as_str(),
                )
            })
            .map(|snapshot| {
                snapshot
                    .desired
                    .auto_compaction
                    .unwrap_or_else(|| request.config.auto_compaction.clone())
            });
        let policy = match policy {
            Ok(policy) => policy,
            Err(error) => {
                forward.abort();
                let _ = forward.await;
                let error = match broker.release_lease() {
                    Ok(()) => error,
                    Err(release) => {
                        error.context(format!("Background lease release also failed: {release:#}"))
                    }
                };
                let _ = sender.send(Err(error));
                return;
            }
        };
        let identity = BackgroundIdentity {
            branch: request.context.branch.clone(),
            head: request.context.head.clone(),
            reason: broker
                .snapshot()
                .expect("prepared Background snapshot")
                .session
                .background
                .expect("Background identity")
                .reason,
        };
        let cancellation = broker.turn_cancellation();
        let input = request.purpose.background_input(&request.input);
        let result = {
            let generation = broker.generate_background(input, identity, model, policy, sink);
            tokio::pin!(generation);
            tokio::select! {
                biased;
                _ = request.cancellation.cancelled() => {
                    cancellation.request();
                    let _ = generation.await;
                    Err(anyhow::anyhow!("Background generation cancelled"))
                }
                _ = self.shutdown.cancelled() => {
                    cancellation.request();
                    let _ = generation.await;
                    Err(anyhow::anyhow!("Background generation cancelled"))
                }
                result = &mut generation => result.and_then(|text| request.purpose.validate(&text)),
            }
        };
        let forwarded = forward
            .await
            .context("Background publication worker failed")
            .and_then(|result| result);
        let result = match (result, forwarded) {
            (Ok(text), Ok(())) => Ok(text),
            (Err(error), Ok(())) => Err(error),
            (Ok(_), Err(error)) => Err(error),
            (Err(error), Err(publication)) => Err(error.context(format!(
                "Background publication also failed: {publication:#}"
            ))),
        };
        if let Err(error) = &result
            && let Err(storage) =
                broker.background_failure("background_generation", format!("{error:#}"))
        {
            report(
                &request.delivery,
                &session_id,
                format!("Could not retain Background failure: {storage:#}"),
            )
            .await;
        }
        let succeeded = result.is_ok();
        let _ = sender.send(result.map(|text| BackgroundResult {
            text,
            session_id: session_id.clone(),
        }));
        let mut native_compaction_observed = false;
        let mut observation = ObservationContinuity::Reliable;
        {
            let inbox = &runtime.retained.observer;
            let deadline = tokio::time::Instant::now() + Duration::from_secs(120);
            let mut pending: Option<String> = None;
            let root_thread = crate::provider::ThreadOwner::root(&session_id).thread_id();
            loop {
                let observations = async {
                    inbox.notified().await?;
                    if inbox.is_closed() && !inbox.has_pending()? {
                        return Ok(None);
                    }
                    inbox.drain().map(Some)
                };
                // Generation has settled through its event watermark; only explicit ongoing
                // compaction justifies waiting for another event on the persistent stream.
                let observed = if pending.is_some() {
                    tokio::time::timeout_at(deadline, observations).await
                } else if inbox.is_closed() || inbox.has_pending().unwrap_or(true) {
                    Ok(observations.await)
                } else {
                    break;
                };
                // Drain every accepted observation before interpreting the typed
                // owner end. Intentional collection is not a native terminal.
                let observed = match observed {
                    Ok(Ok(None)) => {
                        let end = inbox.end();
                        let retired_binding = match &end {
                            Some(
                                crate::provider::session_execution::ObservationEnd::Collected {
                                    retired_binding,
                                    ..
                                }
                                | crate::provider::session_execution::ObservationEnd::Lost {
                                    retired_binding,
                                    ..
                                },
                            ) => retired_binding.as_ref(),
                            None => None,
                        };
                        if let Some(binding) = retired_binding {
                            let retired = match broker.observed_binding_retired(binding) {
                                Ok(delivery) => delivery.deliver().await.map(|_| ()),
                                Err(error) => Err(error),
                            };
                            if let Err(error) = retired {
                                observation = ObservationContinuity::Lost;
                                report(
                                    &request.delivery,
                                    &session_id,
                                    format!(
                                        "Could not retain Background binding retirement: {error:#}"
                                    ),
                                )
                                .await;
                            }
                        }
                        match end {
                            Some(
                                crate::provider::session_execution::ObservationEnd::Collected {
                                    reason,
                                    ..
                                },
                            ) => {
                                let reason = match reason { crate::provider::session_execution::OwnerCloseReason::Cancelled => "cancelled", crate::provider::session_execution::OwnerCloseReason::Shutdown => "shutdown" };
                                broker.trace.record(
                                    &session_id,
                                    "background.observation_collected",
                                    json!({"reason":reason}),
                                );
                                if matches!(observation, ObservationContinuity::Reliable) {
                                    observation = ObservationContinuity::Collected;
                                }
                                Ok(Ok(None))
                            }
                            Some(crate::provider::session_execution::ObservationEnd::Lost {
                                failure,
                                ..
                            }) => Ok(Err(crate::provider::ProviderError::Observation(failure))),
                            None => Ok(Ok(None)),
                        }
                    }
                    observed => observed,
                };
                if let Ok(Err(error)) = &observed {
                    observation = ObservationContinuity::Lost;
                    let failure = format!("Background session observation failed: {error:#}");
                    if let Err(storage) =
                        broker.background_failure("observation_loss", failure.clone())
                    {
                        report(
                            &request.delivery,
                            &session_id,
                            format!("Could not retain Background observation loss: {storage:#}"),
                        )
                        .await;
                    }
                    report(&request.delivery, &session_id, failure).await;
                }
                match observed {
                    Ok(Ok(Some(events))) => {
                        for event in events {
                            if let crate::provider::session_execution::OwnerObservation::Event(
                                crate::provider::HarnessEvent::Native {
                                    event: boxed_native_event,
                                    ..
                                },
                            ) = &event
                                && let crate::provider::ProviderEvent::Thread {
                                    thread_id,
                                    event:
                                        forge_provider_api::ThreadEvent::CompactionUpdated(compaction),
                                } = boxed_native_event.as_ref()
                                && thread_id == &root_thread
                            {
                                native_compaction_observed = true;
                                pending = if compaction.state
                                    == crate::provider::CompactionState::Running
                                {
                                    Some(compaction.id.clone())
                                } else {
                                    None
                                };
                            }
                            if let Err(error) = broker.observe_session_event(event) {
                                observation = ObservationContinuity::Lost;
                                let _ = broker.background_failure(
                                    "observation_application",
                                    format!("{error:#}"),
                                );
                                report(
                                    &request.delivery,
                                    &session_id,
                                    format!("Background session observation failed: {error:#}"),
                                )
                                .await;
                            }
                        }
                    }
                    _ => {
                        if let Some(id) = pending.take() {
                            let marked = match broker.observed_compaction_observation_lost(&id) {
                                Ok(delivery) => delivery.deliver().await.map(|_| ()),
                                Err(error) => Err(error),
                            };
                            if let Err(error) = marked {
                                report(&request.delivery, &session_id, format!("Could not retain unconfirmed Background compaction: {error:#}")).await;
                            }
                            report(
                                &request.delivery,
                                &session_id,
                                "Background compaction completion was not confirmed".into(),
                            )
                            .await;
                        }
                        break;
                    }
                }
            }
        }
        let reusable = matches!(observation, ObservationContinuity::Reliable);
        if succeeded
            && reusable
            && !native_compaction_observed
            && let Err(error) = broker.auto_compact().await
        {
            let _ = broker.background_failure("auto_compaction", format!("{error:#}"));
            report(
                &request.delivery,
                &session_id,
                format!("Background compaction failed: {error}"),
            )
            .await;
        }
        if !succeeded || !reusable {
            let _preparation = self.preparation.lock().await;
            let removed = self.retained.lock().await.remove(&key);
            if let Some(removed) = removed
                && let Err(error) = self.retire(removed).await
            {
                let _ = broker.background_failure("provider_cleanup", format!("{error:#}"));
                report(
                    &request.delivery,
                    &session_id,
                    format!("Background provider cleanup failed: {error:#}"),
                )
                .await;
            }
        }
        if let Err(error) = broker.release_lease() {
            report(
                &request.delivery,
                &session_id,
                format!("Background ownership cleanup failed: {error:#}"),
            )
            .await;
        }
    }

    async fn prepare(
        &self,
        request: &BackgroundRequest,
        key: &str,
    ) -> Result<(
        HarnessBroker,
        BackgroundLease,
        fs::File,
        crate::settings::ValidatedModelSelection,
    )> {
        // Serialize cache selection, collection, and allocation so concurrent workspaces
        // cannot each observe an available native-runtime slot before publishing it.
        let _preparation = tokio::select! {
            biased;
            _ = request.cancellation.cancelled() => anyhow::bail!("Background generation cancelled"),
            _ = self.shutdown.cancelled() => anyhow::bail!("Background generation cancelled"),
            preparation = self.preparation.lock() => preparation,
        };
        let pending: Vec<_> = self
            .cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .iter()
            .cloned()
            .collect();
        for runtime in pending {
            runtime
                .runtime
                .shutdown()
                .await
                .context("Background runtime collection is incomplete; cleanup retry failed")?;
            self.cleanup_pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .retain(|pending| !Arc::ptr_eq(pending, &runtime));
        }
        ensure!(
            !request.config.model.trim().is_empty(),
            "Background model cannot be empty"
        );
        ensure!(
            forge_providers::registry::resolve(&forge_providers::registry::select(
                request.config.provider.kind.as_str()
            )?)?
            .capability
            .integration
            .background_generation,
            "This provider does not support tool-free persistent Background sessions"
        );
        let store = SqliteStore::open(&request.config.data_root)?;
        let directory =
            crate::storage::state::directory(&request.config.data_root).join("background-owner");
        fs::create_dir_all(&directory)?;
        let ownership = fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .read(true)
            .write(true)
            .open(directory.join(format!("{}.lock", crate::plan::digest(key.as_bytes()))))?;
        ownership.try_lock().map_err(|error| {
            anyhow::anyhow!(
                "Background session is busy in another editor. Retry after it finishes: {error}"
            )
        })?;
        let previous = store
            .list_session(Some(&request.context.workspace))?
            .into_iter()
            .find(|session| session.background.is_some());
        let reason = match &previous {
            None => "First Background request",
            Some(previous)
                if previous.provider != request.config.provider.kind.as_str()
                    || previous.model != request.config.model =>
            {
                "Background provider or model changed"
            }
            Some(previous)
                if previous.background.as_ref().unwrap().branch != request.context.branch =>
            {
                "Git branch changed"
            }
            Some(previous)
                if request.context.branch.is_none()
                    && previous.background.as_ref().unwrap().head != request.context.head =>
            {
                "Detached HEAD changed"
            }
            Some(previous)
                if !request
                    .context
                    .follows(
                        &self.repositories,
                        previous.background.as_ref().unwrap().head.as_deref(),
                    )
                    .await? =>
            {
                "Git history diverged"
            }
            Some(_) => "",
        };
        let session_id = if reason.is_empty() {
            previous.as_ref().map(|session| session.id.clone())
        } else {
            None
        };
        let identity = BackgroundIdentity {
            branch: request.context.branch.clone(),
            head: request.context.head.clone(),
            reason: reason.into(),
        };
        // Explicit generation settings are validated before the broker commits them.
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "data_root":request.config.data_root, "workspace":request.context.workspace,
            "client_id":format!("background-{}", uuid::Uuid::new_v4()), "provider":request.config.provider,
            "model":"default", "effort":"default",
            "session_id":session_id, "background":identity,
            "new_session_name":if reason.is_empty() { None } else { Some("Background") }
        }))?;
        let cached = {
            let cache = self.retained.lock().await;
            if let Some(retained) = cache.get(key) {
                if initialize.session_id.as_deref() == Some(retained.session_id.as_str()) {
                    Some(BackgroundLease::acquire(retained.clone())?)
                } else {
                    None
                }
            } else {
                None
            }
        };
        if let Some(retained) = cached {
            let mut broker = HarnessBroker::initialize_with_runtime(
                initialize,
                retained.retained.runtime.clone(),
            )?;
            let recovery = async {
                let registry = broker.execution_registry();
                let owner = crate::provider::ThreadOwner::root(&retained.retained.session_id);
                let engine = registry.owner(owner.clone()).await?;
                let mut retained = retained;
                if !engine.is_healthy().await {
                    engine.close().await?;
                    loop {
                        match retained.retained.observer.drain() {
                            Ok(events) if events.is_empty() => break,
                            Ok(events) => {
                                for event in events {
                                    broker.observe_session_event(event)?;
                                }
                            }
                            Err(error) => {
                                broker
                                    .background_failure("observation_loss", format!("{error:#}"))?;
                                break;
                            }
                        }
                    }
                    let observer = registry.replace(owner).await?;
                    broker.observer().replace_session_events(observer.clone());
                    let replacement = Arc::new(RetainedBackground {
                        runtime: retained.retained.runtime.clone(),
                        session_id: retained.retained.session_id.clone(),
                        observer,
                        leased: AtomicBool::new(false),
                        last_used: std::sync::Mutex::new(std::time::Instant::now()),
                    });
                    let replacement_lease = BackgroundLease::acquire(replacement.clone())?;
                    self.retained
                        .lock()
                        .await
                        .insert(key.to_owned(), replacement);
                    retained = replacement_lease;
                }
                request.context.verify(&self.repositories).await?;
                Ok::<_, anyhow::Error>(retained)
            }
            .await;
            let retained = match recovery {
                Ok(retained) => retained,
                Err(error) => {
                    return match broker.release_lease() {
                        Ok(()) => Err(error),
                        Err(release) => Err(error.context(format!(
                            "Background recovery lease release also failed: {release:#}"
                        ))),
                    };
                }
            };
            let model = broker.configure_background_model(&request.config).await?;
            return Ok((broker, retained, ownership, model));
        }
        let evicted = {
            let mut cache = self.retained.lock().await;
            let mut evicted = Vec::new();
            if let Some(previous) = cache.remove(key) {
                evicted.push(previous);
            }
            if cache.len() >= 4 {
                let idle = cache
                    .iter()
                    .filter(|(_, runtime)| !runtime.leased.load(Ordering::Acquire))
                    .min_by_key(|(_, runtime)| {
                        *runtime
                            .last_used
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                    })
                    .map(|(key, _)| key.clone())
                    .context("all retained Background runtimes are busy")?;
                if let Some(previous) = cache.remove(&idle) {
                    evicted.push(previous);
                }
            }
            evicted
        };
        self.cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .extend(evicted.iter().cloned());
        let mut cleanup_failure = None;
        for previous in evicted {
            match collect_retained_runtime(&previous).await {
                Ok(()) => {
                    self.cleanup_pending
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .retain(|pending| !Arc::ptr_eq(pending, &previous));
                }
                Err(error) => {
                    cleanup_failure.get_or_insert(error);
                }
            }
        }
        if let Some(error) = cleanup_failure {
            return Err(error);
        }
        let runtime = BrokerRuntime::initialize(
            &initialize,
            crate::workspace::WorkspaceKind::Git(PathBuf::from(&request.context.workspace)),
            Arc::clone(&self.repositories),
            Arc::clone(&self.diff),
        )?;
        let mut broker = HarnessBroker::initialize_with_runtime(initialize, Arc::clone(&runtime))?;
        let session_id = broker.snapshot()?.session.id;
        let registry = broker.execution_registry();
        let owner = crate::provider::ThreadOwner::root(&session_id);
        registry.owner(owner.clone()).await?;
        let observer = registry.observer(owner)?;
        let retained = Arc::new(RetainedBackground {
            runtime,
            session_id,
            observer,
            leased: AtomicBool::new(false),
            last_used: std::sync::Mutex::new(std::time::Instant::now()),
        });
        let lease = BackgroundLease::acquire(retained.clone())?;
        self.retained
            .lock()
            .await
            .insert(key.to_owned(), retained.clone());
        let prepared = async {
            let model = broker.configure_background_model(&request.config).await?;
            request.context.verify(&self.repositories).await?;
            Ok::<_, anyhow::Error>(model)
        }
        .await;
        match prepared {
            Ok(model) => Ok((broker, lease, ownership, model)),
            Err(error) => {
                drop(lease);
                self.retained.lock().await.remove(key);
                match self.retire(retained).await {
                    Ok(()) => Err(error),
                    Err(cleanup) => Err(error.context(format!(
                        "Background preparation cleanup failed: {cleanup:#}"
                    ))),
                }
            }
        }
    }

    /// Retain native cleanup ownership until collection succeeds, including cancelled shutdown waits.
    async fn retire(&self, runtime: Arc<RetainedBackground>) -> Result<()> {
        self.cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(runtime.clone());
        collect_retained_runtime(&runtime).await?;
        self.cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .retain(|pending| !Arc::ptr_eq(pending, &runtime));
        Ok(())
    }

    /// Drain admitted work and report every incomplete provider collection obligation.
    pub async fn shutdown(&self) -> Result<()> {
        self.shutdown.cancel();
        let mut failure = None;
        let mut tasks = self.tasks.lock().await;
        while let Some(result) = tasks.join_next().await {
            if let Err(error) = result {
                failure.get_or_insert_with(|| anyhow::Error::from(error));
            }
        }
        drop(tasks);
        let retained = std::mem::take(&mut *self.retained.lock().await);
        self.cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .extend(retained.into_values());
        let pending = self
            .cleanup_pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        for runtime in pending {
            match collect_retained_runtime(&runtime).await {
                Ok(()) => {
                    self.cleanup_pending
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .retain(|pending| !Arc::ptr_eq(pending, &runtime));
                }
                Err(error) => {
                    failure.get_or_insert(error);
                }
            }
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    /// Read or project Background history without acquiring provider ownership.
    pub async fn timeline(&self, params: Value) -> Result<Value> {
        #[derive(Deserialize)]
        struct Query {
            data_root: PathBuf,
            workspace: String,
            session_id: Option<String>,
            presentation: Option<Value>,
            auto_compaction: Option<crate::session::AutoCompaction>,
            expected_revision: Option<u64>,
        }
        let query: Query = serde_json::from_value(params)?;
        let store = SqliteStore::open(&query.data_root)?;
        let context = BackgroundContext::read(&self.repositories, &query.workspace).await?;
        let sessions = store
            .list_session(Some(&context.workspace))?
            .into_iter()
            .filter(|session| session.background.is_some())
            .collect::<Vec<_>>();
        let selected = if let Some(id) = &query.session_id {
            sessions.iter().find(|session| &session.id == id)
        } else {
            sessions.iter().find(|session| {
                session
                    .background
                    .as_ref()
                    .is_some_and(|identity| identity.branch == context.branch)
            })
        };
        ensure!(
            query.session_id.is_none() || selected.is_some(),
            "Background session not found"
        );
        let Some(selected) = selected.cloned() else {
            return Ok(json!({"sessions":sessions,"session":null}));
        };
        let mut settings = crate::settings::SettingsStore::open(&query.data_root)?;
        let mut snapshot = settings.get(&selected.workspace, &selected.id, &selected.provider)?;
        if let Some(policy) = query.auto_compaction {
            policy.validate(selected.context_usage.as_ref())?;
            let mut layer = settings.layer_settings(
                crate::settings::SettingScope::Session,
                &selected.workspace,
                &selected.id,
            )?;
            layer.auto_compaction = Some(policy);
            settings.update(
                query
                    .expected_revision
                    .context("Background policy requires expected revision")?,
                crate::settings::SettingScope::Session,
                &selected.workspace,
                &selected.id,
                &layer,
            )?;
            snapshot = settings.get(&selected.workspace, &selected.id, &selected.provider)?;
        }
        let plan_file = crate::plan::PlanFileStore::new(&query.data_root, &selected.workspace)?;
        let session_events = store.list_session_event(&selected.id)?;
        let entries =
            crate::timeline::TimelineProjector::build(crate::timeline::TimelineProjection {
                interaction_list: store.list_exchange(&selected.id)?,
                plan_list: &[],
                lifecycle_list: vec![],
                execution_list: vec![],
                deviation_list: vec![],
                audit_list: vec![],
                resolution_list: vec![],
                agent_run_list: vec![],
                agent_exchange_list: vec![],
                session_event_list: session_events.clone(),
                plan_file: &plan_file,
            })?;
        let entries = store.with_fork_history(&selected.id, entries, &plan_file)?;
        let presentation = {
            let mut presentations = self.presentation.lock().await;
            presentations
                .entry(selected.id.clone())
                .or_insert_with(|| {
                    Arc::new(std::sync::Mutex::new(
                        crate::buffer::session::SessionPresentation::new(selected.id.clone()),
                    ))
                })
                .clone()
        };
        presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("Background presentation lock poisoned"))?
            .reconcile(entries)?;
        let result = if let Some(request) = query.presentation {
            let request = serde_json::from_value(request)?;
            if let Some(result) = crate::buffer::session::diff_view::update(
                presentation.clone(),
                &request,
                query.data_root,
                self.diff.clone(),
                self.syntax.clone(),
            )
            .await?
            {
                Some(result)
            } else if let crate::buffer::session::PresentationRequest::Highlight { document } =
                request
            {
                let job = presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Background presentation lock poisoned"))?
                    .capture_syntax(&document)?;
                if let Some(job) = job {
                    let highlighted = job.analyze(&self.syntax).await?;
                    presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("Background presentation lock poisoned"))?
                        .apply_syntax(&document, &job, highlighted)?;
                }
                Some(json!({}))
            } else {
                Some(
                    presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("Background presentation lock poisoned"))?
                        .dispatch(request)?,
                )
            }
        } else {
            None
        };
        Ok(
            json!({"sessions":sessions,"session":selected,"settings":snapshot,"events":session_events,"result":result}),
        )
    }
}

/// Collect a retired Background owner's trace only after native execution cleanup succeeds.
async fn collect_retained_runtime(retained: &RetainedBackground) -> Result<()> {
    retained.runtime.shutdown().await?;
    // Background runtimes own separate trace leases even when they share a data
    // root with Main. Releasing this lease leaves other live owners usable.
    let trace = retained.runtime.trace();
    tokio::task::spawn_blocking(move || trace.close())
        .await
        .context("collect Background trace writer")??;
    Ok(())
}

/// Forward ordered Background progress and surface errors before a flush can acknowledge it.
async fn forward_progress(
    mut events: crate::broker::publication::SessionUpdateStream,
    delivery: Option<(MessageSender, u64)>,
    progress_session: String,
) -> Result<()> {
    while let Some(event) = events.recv().await? {
        let progress = match &event {
            crate::broker::publication::SessionUpdate::Runtime { .. } => {
                Some(crate::broker::publication::encode_session_update(&event)?)
            }
            crate::broker::publication::SessionUpdate::DocumentChanged { patch } => patch
                .notification()
                .get("status")
                .map(|status| json!({"kind":"status","data":status})),
            _ => None,
        };
        if let (Some((delivery, request_id)), Some(progress)) = (&delivery, progress) {
            delivery.send(Message::RequestEvent(RequestEvent {
                request_id: *request_id,
                event: forge_protocol::contract::RequestEventName::RepositoryGenerationProgress
                    .as_str()
                    .into(),
                payload: json!({"session_id":progress_session,"event":progress}),
            }))?;
        }
    }
    Ok::<_, anyhow::Error>(())
}

async fn report(delivery: &Option<(MessageSender, u64)>, session: &str, error: String) {
    if let Some((delivery, _)) = delivery {
        let _ = delivery
            .send_event(SessionEvent {
                session_id: session.into(),
                event: "background_error".into(),
                payload: json!({"error":error}),
            })
            .await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Refuse to acknowledge progress when the external receiver has closed.
    #[tokio::test]
    async fn failed_progress_delivery_cannot_acknowledge_flush() -> Result<()> {
        let (outbound, receiver) = forge_protocol::outbound::channel();
        drop(receiver);
        let (sink, events) = crate::broker::publication::channel();
        let forward = tokio::spawn(forward_progress(
            events,
            Some((outbound, 7)),
            "background".into(),
        ));
        sink.send_wait(crate::broker::publication::SessionUpdate::Runtime {
            session_id: "background".into(),
            runtime: crate::provider::ProviderRuntime::default(),
        })
        .await?;
        let error = tokio::time::timeout(Duration::from_secs(1), forward)
            .await??
            .unwrap_err();
        assert_eq!(
            error
                .downcast_ref::<std::io::Error>()
                .map(std::io::Error::kind),
            Some(std::io::ErrorKind::BrokenPipe)
        );
        assert_eq!(
            sink.flush().await.unwrap_err().kind(),
            std::io::ErrorKind::BrokenPipe
        );
        Ok(())
    }

    /// Supersession collects the old owner intentionally and reuses the completed successor.
    ///
    /// The superseded generation is cancelled before any model output, so cancellation retracts
    /// its output-free exchange as described under `session.turn.cancel` in
    /// `contributing/forge/architecture/harness.md`.
    #[tokio::test]
    async fn background_supersession_retracts_output_free_generation_and_reuses_completed_successor_without_loss_report()
    -> Result<()> {
        tokio::time::timeout(Duration::from_secs(10), async {
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().join("workspace");
            fs::create_dir(&workspace)?;
            let initialized = std::process::Command::new("git").arg("-C").arg(&workspace).args(["init", "-q"]).output()?;
            ensure!(initialized.status.success(), "fixture Git initialization failed");
            let diff = DiffEngine::new(Default::default(), 1);
            let manager = BackgroundManager::new(Arc::new(RepositoryStore::default()), diff.clone(), forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool()));
            let config = TextGenerationConfig {
                provider:crate::provider::ProviderLaunch { kind:forge_providers::registry::nonselectable_id().expect("registry supplies a nonselectable fixture identity"), command:Vec::new() },
                model:"mock-model".into(), reasoning_effort:None, service_tier:Default::default(),
                data_root:fixture.path().join("state"), auto_compaction:Default::default(),
            };
            let context = BackgroundContext::read(&manager.repositories, workspace.to_str().context("fixture path is not Unicode")?).await?;
            let (delivery, mut delivered) = forge_protocol::outbound::channel();
            let request = |input:&str, cancellation| BackgroundRequest {
                cancellation, purpose:crate::provider::TextGeneration::CommitMessage, config:config.clone(),
                context:context.clone(), input:input.into(), delivery:Some((delivery.clone(), 71)),
            };
            let cancellation = super::super::CancellationToken::new();
            let initial = request("cancelled-old-generation", cancellation.clone());
            let key = format!("{}:{}", config.data_root.display(), context.workspace);
            let (mut broker, retained, ownership, _model) = manager.prepare(&initial, &key).await?;
            let session_id = retained.retained.session_id.clone();
            let provider = retained.retained.runtime.provider_handle();
            let control = provider.mock_response_control().context("mock response barrier is unavailable")?;
            let hold = control.dispatch(&session_id, crate::provider::mock_response::MockResponseCommand::Hold {
                mode:Some(crate::provider::PromptMode::Background), any_session:false,
            }).await?;
            let hold = hold["id"].as_str().context("mock hold identity missing")?.to_owned();
            drop(retained);
            broker.release_lease()?;
            drop(broker);
            drop(ownership);
            let first = tokio::spawn({ let manager = manager.clone(); async move { manager.generate(initial).await } });
            control.dispatch(&session_id, crate::provider::mock_response::MockResponseCommand::Wait { id:hold }).await?;
            let store = SqliteStore::read_only(&config.data_root)?;
            loop {
                if store.list_exchange(&session_id)?.first().is_some_and(|exchange| !exchange.turn.is_empty()) { break; }
                tokio::task::yield_now().await;
            }
            cancellation.cancel();
            let error = first.await?.err().context("superseded generation returned a result")?;
            assert!(error.to_string().contains("Background generation cancelled"));
            let second = manager.generate(request("new-completed-generation", super::super::CancellationToken::new())).await?;
            assert_eq!(second.session_id, session_id);
            assert!(second.text.contains("new-completed-generation") && !second.text.contains("cancelled-old-generation"));
            let successor = manager.retained.lock().await.get(&key).context("successful successor was not retained")?.runtime.clone();
            let third = manager.generate(request("later-same-session-generation", super::super::CancellationToken::new())).await?;
            assert_eq!(third.session_id, session_id);
            assert!(third.text.contains("later-same-session-generation") && !third.text.contains("cancelled-old-generation"));
            assert!(Arc::ptr_eq(&successor, &manager.retained.lock().await.get(&key).context("reused owner missing")?.runtime));
            manager.shutdown().await?;
            let exchanges = store.list_exchange(&session_id)?;
            assert_eq!(exchanges.len(), 2, "the output-free superseded generation was not retracted");
            assert!(exchanges.iter().all(|exchange| exchange.state() == crate::exchange::ExchangeState::Complete
                && !exchange.prompt.contains("cancelled-old-generation")));
            let records = store.list_session_event(&session_id)?;
            assert!(!records.iter().any(|record| matches!(&record.detail,
                crate::timeline::SessionEventKind::Configuration { field, .. } if field.iter().any(|field| field == "observation_loss")
            )), "intentional supersession was retained as unreliable observation");
            while let Ok(frame) = delivered.try_recv() {
                let event:Value = serde_json::from_slice(frame.bytes())?;
                assert_ne!(event["event"], "background_error", "{event}");
            }
            Ok::<_, anyhow::Error>(())
        }).await.context("Background supersession did not collect and reuse its owner")?
    }

    /// Evict the oldest idle native runtime while preserving its durable logical conversation.
    #[tokio::test]
    async fn idle_runtime_lru_eviction_preserves_durable_background_history() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let diff = DiffEngine::new(Default::default(), 1);
        let manager = BackgroundManager::new(
            Arc::new(RepositoryStore::default()),
            diff.clone(),
            forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool()),
        );
        let config = TextGenerationConfig {
            provider: crate::provider::ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id().expect("fixture identity"),
                command: Vec::new(),
            },
            model: "mock-model".into(),
            reasoning_effort: None,
            service_tier: Default::default(),
            data_root: fixture.path().join("state"),
            auto_compaction: Default::default(),
        };
        let mut first = None;
        for index in 0..5 {
            let workspace = fixture.path().join(format!("workspace-{index}"));
            fs::create_dir(&workspace)?;
            let initialized = std::process::Command::new("git")
                .arg("-C")
                .arg(&workspace)
                .args(["init", "-q"])
                .output()?;
            ensure!(
                initialized.status.success(),
                "fixture Git initialization failed"
            );
            let context = BackgroundContext::read(
                &manager.repositories,
                workspace.to_str().context("fixture path is not Unicode")?,
            )
            .await?;
            let key = format!("{}:{}", config.data_root.display(), context.workspace);
            let request = BackgroundRequest {
                cancellation: super::super::CancellationToken::new(),
                purpose: crate::provider::TextGeneration::CommitMessage,
                config: config.clone(),
                context,
                input: "Generate fixture text".into(),
                delivery: None,
            };
            let result = manager.generate(request).await?;
            let mut tasks = manager.tasks.lock().await;
            while let Some(result) = tasks.join_next().await {
                result?;
            }
            drop(tasks);
            assert!(manager.retained.lock().await.len() <= 4);
            if index == 0 {
                first = Some((key, result.session_id));
            }
        }
        let (key, session_id) = first.context("first workspace was not generated")?;
        assert!(!manager.retained.lock().await.contains_key(&key));
        assert_eq!(manager.retained.lock().await.len(), 4);
        assert!(
            SqliteStore::read_only(&config.data_root)?
                .load_session(&session_id)?
                .is_some()
        );
        assert_eq!(
            SqliteStore::read_only(&config.data_root)?
                .list_exchange(&session_id)?
                .len(),
            1
        );
        manager.shutdown().await?;
        assert!(manager.retained.lock().await.is_empty());
        Ok(())
    }

    /// Recover a lost retained listener only when a new Background request arrives.
    #[tokio::test]
    async fn explicit_background_request_replaces_failed_observer() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().join("workspace");
        fs::create_dir(&workspace)?;
        let git = std::process::Command::new("git")
            .arg("-C")
            .arg(&workspace)
            .args(["init", "-q"])
            .output()?;
        ensure!(git.status.success(), "fixture Git initialization failed");
        let diff = DiffEngine::new(Default::default(), 1);
        let manager = BackgroundManager::new(
            Arc::new(RepositoryStore::default()),
            Arc::clone(&diff),
            forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool()),
        );
        let mut request = BackgroundRequest {
            cancellation: super::super::CancellationToken::new(),
            purpose: crate::provider::TextGeneration::CommitMessage,
            config: TextGenerationConfig {
                provider: crate::provider::ProviderLaunch {
                    kind: forge_providers::registry::nonselectable_id()
                        .expect("registry supplies a nonselectable fixture identity"),
                    command: Vec::new(),
                },
                model: "mock-model".into(),
                reasoning_effort: None,
                service_tier: Default::default(),
                data_root: fixture.path().join("state"),
                auto_compaction: Default::default(),
            },
            context: BackgroundContext::read(
                &manager.repositories,
                workspace.to_str().context("fixture path is not UTF-8")?,
            )
            .await?,
            input: "fixture".into(),
            delivery: None,
        };
        let key = format!(
            "{}:{}",
            request.config.data_root.display(),
            request.context.workspace
        );
        request.config.model = "unavailable-background-model".into();
        assert!(manager.prepare(&request, &key).await.is_err());
        assert!(manager.retained.lock().await.is_empty());
        assert!(manager.cleanup_pending.lock().unwrap().is_empty());
        let failed_sessions =
            SqliteStore::read_only(&request.config.data_root)?.list_session(None)?;
        assert!(
            failed_sessions
                .iter()
                .all(|session| session.model == "default" && session.provider_session_id.is_none())
        );
        request.config.model = "mock-model".into();
        let (mut broker, retained, ownership, _model) = manager.prepare(&request, &key).await?;
        let session_id = retained.retained.session_id.clone();
        let old_observer = retained.retained.observer.clone();
        let owner = crate::provider::ThreadOwner::root(&session_id);
        let registry = broker.execution_registry();
        let old_engine = registry.owner(owner.clone()).await?;
        old_observer.fail(forge_provider_api::lifecycle::ObservationFailure::Disconnected);
        registry.fence(&owner).await?;
        tokio::time::timeout(Duration::from_secs(2), async {
            while old_engine.is_healthy().await {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert!(matches!(
            old_observer.end(),
            Some(crate::provider::session_execution::ObservationEnd::Lost {
                failure: forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ..
            })
        ));
        drop(retained);
        broker.release_lease()?;
        drop(broker);
        drop(ownership);
        let (mut broker, replacement, ownership, _model) = manager.prepare(&request, &key).await?;
        assert_eq!(replacement.retained.session_id, session_id);
        assert!(!Arc::ptr_eq(&old_observer, &replacement.retained.observer));
        assert!(registry.owner(owner).await?.is_healthy().await);
        assert!(old_observer.is_closed());
        let records =
            SqliteStore::read_only(&request.config.data_root)?.list_session_event(&session_id)?;
        assert!(records.iter().any(|record| matches!(&record.detail,
            crate::timeline::SessionEventKind::Configuration { state, field, reason, .. }
            if state == "failed" && field.iter().any(|field| field == "observation_loss") && reason.is_some()
        )));
        let retiring = replacement.retained.clone();
        drop(replacement);
        broker.release_lease()?;
        drop(ownership);
        manager.retained.lock().await.remove(&key);
        registry.record_cleanup_failure(crate::provider::session_execution::CleanupError::Work {
            kind: crate::provider::session_execution::CleanupWorkKind::OwnerFence,
            source: crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::ThreadClose,
                    forge_provider_api::ProviderFailureKind::Runtime,
                ),
            )
            .into(),
        });
        assert!(manager.retire(retiring.clone()).await.is_err());
        registry.record_cleanup_failure(crate::provider::session_execution::CleanupError::Work {
            kind: crate::provider::session_execution::CleanupWorkKind::OwnerFence,
            source: crate::provider::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::ThreadClose,
                    forge_provider_api::ProviderFailureKind::Runtime,
                ),
            )
            .into(),
        });
        let failed = manager.prepare(&request, &key).await;
        assert!(failed.is_err(), "pending cleanup admitted a replacement");
        assert!(manager.retained.lock().await.get(&key).is_none());
        assert!(
            manager
                .cleanup_pending
                .lock()
                .unwrap()
                .iter()
                .any(|pending| Arc::ptr_eq(pending, &retiring))
        );
        let (mut broker, recovered, ownership, _model) = manager.prepare(&request, &key).await?;
        assert_eq!(recovered.retained.session_id, session_id);
        assert!(!Arc::ptr_eq(&recovered.retained.runtime, &retiring.runtime));
        assert!(manager.cleanup_pending.lock().unwrap().is_empty());
        drop(recovered);
        broker.release_lease()?;
        drop(ownership);
        manager.shutdown().await?;
        Ok(())
    }

    #[tokio::test]
    async fn shutdown_reports_background_worker_failure() {
        let diff = DiffEngine::new(Default::default(), 1);
        let manager = BackgroundManager::new(
            Arc::new(RepositoryStore::default()),
            Arc::clone(&diff),
            forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool()),
        );
        manager.tasks.lock().await.spawn(async {
            panic!("background worker failed before completing its request");
        });
        let error = manager.shutdown().await.unwrap_err();
        assert!(error.to_string().contains("background worker failed"));
    }

    /// Cancel queued generation on either caller interruption or shutdown before native preparation.
    #[tokio::test]
    async fn cancelled_or_shutdown_admission_never_starts_provider() {
        for shutdown in [false, true] {
            let diff = DiffEngine::new(Default::default(), 1);
            let manager = BackgroundManager::new(
                Arc::new(RepositoryStore::default()),
                Arc::clone(&diff),
                forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool()),
            );
            let permits = manager.runtime_admission.acquire_many(4).await.unwrap();
            let cancellation = super::super::CancellationToken::new();
            let request = BackgroundRequest {
                cancellation: cancellation.clone(),
                purpose: crate::provider::TextGeneration::CommitMessage,
                config: TextGenerationConfig {
                    provider: crate::provider::ProviderLaunch {
                        kind: forge_providers::registry::nonselectable_id()
                            .expect("registry supplies a nonselectable fixture identity"),
                        command: vec![],
                    },
                    model: "mock-model".into(),
                    reasoning_effort: None,
                    service_tier: Default::default(),
                    data_root: PathBuf::new(),
                    auto_compaction: Default::default(),
                },
                context: BackgroundContext {
                    workspace: "not-a-repository".into(),
                    branch: None,
                    head: None,
                    history: String::new(),
                },
                input: "cancel before preparation".into(),
                delivery: None,
            };
            let task = tokio::spawn({
                let manager = Arc::clone(&manager);
                async move { manager.generate(request).await }
            });
            tokio::task::yield_now().await;
            if shutdown {
                manager.shutdown().await.unwrap();
            } else {
                cancellation.cancel();
            }
            let error = task
                .await
                .unwrap()
                .err()
                .expect("cancelled request succeeded");
            assert_eq!(error.to_string(), "Background generation cancelled");
            drop(permits);
            manager.shutdown().await.unwrap();
        }
    }

    #[tokio::test]
    async fn background_failure_is_delivered_to_the_owner() {
        let (sender, mut receiver) = forge_protocol::outbound::channel();
        report(&Some((sender, 1)), "session", "retained failure".into()).await;
        let frame = tokio::time::timeout(std::time::Duration::from_secs(1), receiver.recv())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        let event: serde_json::Value = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(event["session_id"], "session");
        assert_eq!(event["event"], "background_error");
        assert_eq!(event["payload"]["error"], "retained failure");
    }
}
