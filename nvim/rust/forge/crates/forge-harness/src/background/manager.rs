use anyhow::{Context, Result, ensure};
use forge_diff::engine::DiffEngine;
use forge_git::store::RepositoryStore;
use forge_protocol::{
    message::{Message, RequestEvent, SessionEvent},
    outbound::MessageSender,
};
use serde::Deserialize;
use serde_json::{Value, json};
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
    /// Current response contract, independent of the conversation's preceding task.
    pub purpose: crate::backend::TextGeneration,
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

/// Owns Background operation lifetimes independently of interactive Harness startup.
pub struct BackgroundManager {
    pub(super) repositories: Arc<RepositoryStore>,
    diff: Arc<DiffEngine>,
    syntax: Arc<forge_diff::syntax::SyntaxEngine>,
    admission: Mutex<HashMap<String, Arc<Mutex<()>>>>,
    tasks: Mutex<JoinSet<()>>,
    runtime_admission: Semaphore,
    presentation: Mutex<HashMap<String, crate::buffer::session::SessionPresentation>>,
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
        let _admission = gate.lock().await;
        let _runtime = self
            .runtime_admission
            .acquire()
            .await
            .expect("Background admission remains open while draining");
        let prepared = self.prepare(&request, &key).await;
        let (mut broker, runtime, _ownership) = match prepared {
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
        let (sink, mut events) = crate::backend::events::channel();
        let delivery = request.delivery.clone();
        let progress_session = session_id.clone();
        let forward = tokio::spawn(async move {
            while let Ok(Some(event)) = events.recv().await {
                let progress = match event.kind.as_str() {
                    "runtime_resolved" | "reasoning_summary" => Some(json!(event)),
                    "document_changed" => event
                        .data
                        .get("status")
                        .map(|status| json!({"kind":"status","data":status})),
                    _ => None,
                };
                if let (Some((delivery, request_id)), Some(progress)) = (&delivery, progress) {
                    let _ = delivery.send(Message::RequestEvent(RequestEvent {
                        request_id: *request_id,
                        event: "repository.generate.progress".into(),
                        payload: json!({"session_id":progress_session,"event":progress}),
                    }));
                }
            }
        });
        let mut compaction_stream = match runtime
            .backend_handle()
            .compaction_stream(&session_id)
            .await
        {
            Ok(stream) => stream,
            Err(error) => {
                report(
                    &request.delivery,
                    &session_id,
                    format!("Background compaction subscription failed: {error:#}"),
                );
                None
            }
        };
        let policy = crate::settings::SettingsStore::open(&request.config.data_root)
            .and_then(|store| {
                store.get(
                    &request.context.workspace,
                    &session_id,
                    &request.config.backend.kind,
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
                let _ = sender.send(Err(error));
                forward.abort();
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
        let timeout = tokio::spawn(async move {
            tokio::time::sleep(Duration::from_secs(120)).await;
            cancellation.request(false);
        });
        let input = request.purpose.background_input(&request.input);
        let result = broker
            .generate_background(
                input,
                identity,
                request.config.model,
                request
                    .config
                    .reasoning_effort
                    .unwrap_or_else(|| "low".into()),
                policy,
                sink,
            )
            .await
            .and_then(|text| request.purpose.validate(&text));
        timeout.abort();
        let _ = forward.await;
        if let Err(error) = &result {
            if let Err(storage) =
                broker.background_failure("background_generation", format!("{error:#}"))
            {
                report(
                    &request.delivery,
                    &session_id,
                    format!("Could not retain Background failure: {storage:#}"),
                );
            }
        }
        let succeeded = result.is_ok();
        let _ = sender.send(result.map(|text| BackgroundResult {
            text,
            session_id: session_id.clone(),
        }));
        let mut native_compaction_observed = false;
        if let Some(stream) = &mut compaction_stream {
            let deadline = tokio::time::Instant::now() + Duration::from_secs(120);
            let mut pending: Option<crate::backend::Compaction> = None;
            loop {
                let until = if pending.is_some() {
                    deadline
                } else {
                    tokio::time::Instant::now() + Duration::from_millis(10)
                };
                match tokio::time::timeout_at(until, stream.recv()).await {
                    Ok(Ok(Some(event))) => {
                        if let Ok(Some(compaction)) = event.compaction() {
                            native_compaction_observed = true;
                            pending =
                                if compaction.state == crate::backend::CompactionState::Running {
                                    Some(compaction)
                                } else {
                                    None
                                };
                        }
                        if let Err(error) = broker.observe_external_compaction(event) {
                            report(
                                &request.delivery,
                                &session_id,
                                format!("Background compaction event failed: {error:#}"),
                            );
                        }
                    }
                    _ => {
                        if let Some(mut compaction) = pending.take() {
                            compaction.state = crate::backend::CompactionState::Unconfirmed;
                            compaction.result.error =
                                Some("Provider compaction completion was not confirmed".into());
                            let _ = broker.observe_external_compaction(
                                crate::backend::BackendEvent::from_compaction(compaction, None),
                            );
                            report(
                                &request.delivery,
                                &session_id,
                                "Background compaction completion was not confirmed".into(),
                            );
                        }
                        break;
                    }
                }
            }
        }
        if succeeded
            && !native_compaction_observed
            && let Err(error) = broker.auto_compact().await
        {
            let _ = broker.background_failure("auto_compaction", format!("{error:#}"));
            report(
                &request.delivery,
                &session_id,
                format!("Background compaction failed: {error:#}"),
            );
        }
        if let Err(error) = runtime.backend_handle().shutdown().await {
            let _ = broker.background_failure("provider_cleanup", format!("{error:#}"));
            report(
                &request.delivery,
                &session_id,
                format!("Background provider cleanup failed: {error:#}"),
            );
        }
        if let Err(error) = broker.release_lease() {
            report(
                &request.delivery,
                &session_id,
                format!("Background ownership cleanup failed: {error:#}"),
            );
        }
    }

    async fn prepare(
        &self,
        request: &BackgroundRequest,
        key: &str,
    ) -> Result<(HarnessBroker, Arc<BrokerRuntime>, fs::File)> {
        ensure!(
            !request.config.model.trim().is_empty(),
            "Background model cannot be empty"
        );
        if let Some(effort) = &request.config.reasoning_effort {
            ensure!(
                ["none", "minimal", "low", "medium", "high", "xhigh", "max"]
                    .contains(&effort.as_str()),
                "Invalid Background thinking setting"
            );
        }
        request.config.auto_compaction.validate(None)?;
        ensure!(
            ["codex", "copilot", "mock"].contains(&request.config.backend.kind.as_str()),
            "This Harness provider does not support tool-free persistent Background sessions"
        );
        let directory = request.config.data_root.join("background-owner");
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
        let store = SqliteStore::open(&request.config.data_root)?;
        let previous = store
            .list_session(Some(&request.context.workspace))?
            .into_iter()
            .find(|session| session.background.is_some());
        let reason = match &previous {
            None => "First Background request",
            Some(previous)
                if previous.backend != request.config.backend.kind
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
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "data_root":request.config.data_root, "workspace":request.context.workspace,
            "client_id":format!("background-{}", uuid::Uuid::new_v4()), "backend":request.config.backend,
            "model":request.config.model, "effort":request.config.reasoning_effort.as_deref().unwrap_or("low"),
            "session_id":session_id, "background":identity,
            "new_session_name":if reason.is_empty() { None } else { Some("Background") }
        }))?;
        let runtime = BrokerRuntime::initialize(
            &initialize,
            crate::workspace::WorkspaceKind::Git(PathBuf::from(&request.context.workspace)),
            Arc::clone(&self.repositories),
            Arc::clone(&self.diff),
        )?;
        let broker = HarnessBroker::initialize_with_runtime(initialize, Arc::clone(&runtime))?;
        request.context.verify(&self.repositories).await?;
        Ok((broker, runtime, ownership))
    }

    /// Drain admitted generation and maintenance before shutting down the host.
    pub async fn shutdown(&self) {
        let mut tasks = self.tasks.lock().await;
        while tasks.join_next().await.is_some() {}
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
        let mut snapshot = settings.get(&selected.workspace, &selected.id, &selected.backend)?;
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
            snapshot = settings.get(&selected.workspace, &selected.id, &selected.backend)?;
        }
        let plan_file = crate::plan::PlanFileStore::new(&query.data_root, &selected.workspace);
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
        let mut presentations = self.presentation.lock().await;
        let presentation = presentations.entry(selected.id.clone()).or_insert_with(|| {
            crate::buffer::session::SessionPresentation::new(selected.id.clone())
        });
        presentation.reconcile(entries)?;
        let result = if let Some(request) = query.presentation {
            let request = serde_json::from_value(request)?;
            if let crate::buffer::session::PresentationRequest::Highlight { document } = request {
                let job = presentation.capture_syntax(&document)?;
                if let Some(job) = job {
                    drop(presentations);
                    let highlighted = job.analyze(&self.syntax).await?;
                    let mut current = self.presentation.lock().await;
                    current
                        .get_mut(&selected.id)
                        .context("Background view closed during syntax")?
                        .apply_syntax(&document, &job, highlighted)?;
                }
                Some(json!({}))
            } else {
                Some(presentation.dispatch(request)?)
            }
        } else {
            None
        };
        Ok(
            json!({"sessions":sessions,"session":selected,"settings":snapshot,"events":session_events,"result":result}),
        )
    }
}

fn report(delivery: &Option<(MessageSender, u64)>, session: &str, error: String) {
    if let Some((delivery, _)) = delivery {
        let _ = delivery.send_event(SessionEvent {
            session_id: session.into(),
            event: "background_error".into(),
            payload: json!({"error":error}),
        });
    }
}
