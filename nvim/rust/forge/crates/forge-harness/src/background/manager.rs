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
use tracing::{Instrument, instrument::WithSubscriber};

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
    ownership: Arc<fs::File>,
    trace: Arc<crate::trace::TraceStore>,
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
    trace: Mutex<HashMap<PathBuf, Arc<crate::trace::TraceStore>>>,
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
            trace: Mutex::new(HashMap::new()),
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
        let trace = {
            let mut trace = self.trace.lock().await;
            match trace.entry(request.config.data_root.clone()) {
                std::collections::hash_map::Entry::Occupied(entry) => entry.get().clone(),
                std::collections::hash_map::Entry::Vacant(entry) => entry
                    .insert(Arc::new(crate::trace::TraceStore::open(
                        &request.config.data_root,
                    )?))
                    .clone(),
            }
        };
        let dispatch = trace.dispatch();
        let span = tracing::dispatcher::with_default(&dispatch, || {
            tracing::info_span!(
                "background.generation",
                request_id = request.delivery.as_ref().map(|(_, request_id)| *request_id),
                process_id = std::process::id(),
                session_id = tracing::field::Empty,
                method = "repository.generation",
                provider = request.config.provider.kind.as_str(),
            )
        });
        tasks.spawn(
            async move {
                owner.run(request, sender, trace).await;
            }
            .instrument(span)
            .with_subscriber(dispatch),
        );
        drop(tasks);
        receiver
            .await
            .context("Background generation worker stopped")?
    }

    async fn run(
        &self,
        request: BackgroundRequest,
        sender: oneshot::Sender<Result<BackgroundResult>>,
        trace: Arc<crate::trace::TraceStore>,
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
        let prepared = self.prepare(&request, &key, trace).await;
        let (mut broker, runtime, model) = match prepared {
            Ok(prepared) => prepared,
            Err(error) => {
                tracing::error!(target: "forge::provider", event_name = "background.preparation.failed",
                    error = %format_args!("{error:#}"));
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
        let forward = tokio::spawn(
            forward_progress(events, delivery, progress_session)
                .in_current_span()
                .with_current_subscriber(),
        );
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
                tracing::error!(target: "forge::provider", event_name = "background.preparation.failed",
                    error = %format_args!("{error:#}"));
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
        if let Err(error) = &result {
            tracing::error!(target: "forge::provider", event_name = "background.generation.failed",
                error = %format_args!("{error:#}"));
        }
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
                                let reason = match reason { crate::provider::session_execution::OwnerCloseReason::Shutdown => "shutdown" };
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
        trace: Arc<crate::trace::TraceStore>,
    ) -> Result<(
        HarnessBroker,
        BackgroundLease,
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
        #[expect(
            clippy::disallowed_methods,
            reason = "a Background run owns a store for its whole asynchronous preparation, and a writer lease cannot cross an await"
        )]
        let store = SqliteStore::open(&request.config.data_root)?;
        let ownership = self
            .retained
            .lock()
            .await
            .get(key)
            .map(|retained| retained.ownership.clone());
        // An idle retained runtime still owns its native writer. Keep the file lock
        // through reuse, replacement, and failed cleanup, not just one generation.
        let ownership = match ownership {
            Some(ownership) => ownership,
            None => {
                let directory = crate::storage::state::directory(&request.config.data_root)
                    .join("background-owner");
                fs::create_dir_all(&directory)?;
                let ownership = fs::OpenOptions::new()
                    .create(true)
                    .truncate(false)
                    .read(true)
                    .write(true)
                    .open(
                        directory.join(format!("{}.lock", crate::plan::digest(key.as_bytes()))),
                    )?;
                ownership.try_lock().map_err(|error| {
                    anyhow::anyhow!(
                        "Background session is owned by another editor. Close that editor before retrying: {error}"
                    )
                })?;
                Arc::new(ownership)
            }
        };
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
            tracing::Span::current().record("session_id", retained.retained.session_id.as_str());
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
                        ownership: retained.retained.ownership.clone(),
                        trace: retained.retained.trace.clone(),
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
            return Ok((broker, retained, model));
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
        tracing::Span::current().record("session_id", session_id.as_str());
        let registry = broker.execution_registry();
        let owner = crate::provider::ThreadOwner::root(&session_id);
        registry.owner(owner.clone()).await?;
        let observer = registry.observer(owner)?;
        let retained = Arc::new(RetainedBackground {
            runtime,
            ownership,
            trace,
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
            Ok(model) => Ok((broker, lease, model)),
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
            None => {
                let traces = std::mem::take(&mut *self.trace.lock().await);
                tokio::task::spawn_blocking(move || {
                    for trace in traces.into_values() {
                        trace.close()?;
                    }
                    Ok::<_, crate::trace::TraceError>(())
                })
                .await
                .context("collect Background trace writers")??;
                Ok(())
            }
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
        #[expect(
            clippy::disallowed_methods,
            reason = "a Background query owns a store for its whole asynchronous preparation, and a writer lease cannot cross an await"
        )]
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
    let dispatch = retained.trace.dispatch();
    let span = tracing::dispatcher::with_default(&dispatch, || {
        tracing::info_span!(
            "background.collection",
            session_id = retained.session_id.as_str()
        )
    });
    retained
        .runtime
        .shutdown()
        .instrument(span)
        .with_subscriber(dispatch)
        .await?;
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
            // Progress is advisory and the generated text arrives in the response, so a stalled
            // editor drops progress rather than closing its connection.
            delivery.send_progress(Message::RequestEvent(RequestEvent {
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
    tracing::error!(target: "forge::provider", event_name = "background.cleanup.failed",
        session_id = session, error = %error);
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
    use forge_diff::cache::CacheLimits;

    /// Keep idle native ownership exclusive until shutdown, while allowing reuse by its owner.
    #[tokio::test]
    async fn retained_background_keeps_writer_lock_until_shutdown() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().join("workspace");
        fs::create_dir_all(&workspace)?;
        forge_git::test_support::init_repository(&workspace);
        let repositories = Arc::new(RepositoryStore::default());
        let diff = DiffEngine::new(CacheLimits::default(), 1);
        let syntax = forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool());
        let first = BackgroundManager::new(repositories.clone(), diff.clone(), syntax.clone());
        let second = BackgroundManager::new(repositories.clone(), diff, syntax);
        let request = BackgroundRequest {
            cancellation: super::super::CancellationToken::new(),
            purpose: crate::provider::TextGeneration::CommitMessage,
            config: serde_json::from_value(json!({
                "data_root":fixture.path().join("data"),
                "provider":{"kind":"mock","command":[]},
                "model":"mock-model",
                "auto_compaction":{"enabled":false,"threshold_tokens":100000}
            }))?,
            context: BackgroundContext::read(&repositories, &workspace.to_string_lossy()).await?,
            input: "A test diff".into(),
            delivery: None,
        };
        let key = format!(
            "{}:{}",
            request.config.data_root.display(),
            request.context.workspace
        );
        let trace = Arc::new(crate::trace::TraceStore::open(&request.config.data_root)?);
        let (mut broker, lease, _) = first.prepare(&request, &key, trace.clone()).await?;
        let session_id = lease.retained.session_id.clone();
        broker.release_lease()?;
        drop(broker);
        drop(lease);

        let (mut broker, lease, _) = first.prepare(&request, &key, trace.clone()).await?;
        assert_eq!(lease.retained.session_id, session_id);
        broker.release_lease()?;
        drop(broker);
        drop(lease);
        let conflict = match second.prepare(&request, &key, trace.clone()).await {
            Err(error) => error,
            Ok(_) => panic!("an idle native writer must exclude another manager"),
        };
        assert!(conflict.to_string().contains("owned by another editor"));

        first.shutdown().await?;
        let (mut broker, lease, _) = second.prepare(&request, &key, trace).await?;
        assert_eq!(lease.retained.session_id, session_id);
        broker.release_lease()?;
        drop(broker);
        drop(lease);
        second.shutdown().await?;
        Ok(())
    }
}
