//! Keep one idle, read-only provider conversation for each source with an editable review open.
use super::*;
use crate::broker::review_question::ReviewAnswerModel;
use crate::provider::{PromptMode, ProviderInput, ThreadOwner};
use forge_buffer::identity::DocumentId;
use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};
use uuid::Uuid;

#[derive(Default)]
pub(super) struct ReviewStandbyPool {
    source: HashMap<String, OpenReviews>,
    recovered: HashSet<String>,
    workers: JoinSet<()>,
    closed: bool,
}

#[derive(Default)]
struct OpenReviews {
    views: HashSet<DocumentId>,
    standby: Option<Standby>,
}

struct Standby {
    id: Uuid,
    requirements: Requirements,
    ready: Arc<std::sync::OnceLock<ReadyStandby>>,
    /// A successful claim transfers the fork to its chain. Dropping this sender retires it.
    claim: tokio::sync::oneshot::Sender<()>,
}

struct ReadyStandby {
    child: Arc<SessionController>,
    provider: Arc<crate::provider::Provider>,
    binding: forge_provider_api::BindingId,
}

impl ReadyStandby {
    fn live(&self) -> bool {
        !self.child.input_stopping.load(Ordering::Acquire)
            && !self.child.execution_failed()
            && Arc::ptr_eq(&self.provider, &self.child.provider_handle())
            && self
                .provider
                .ready_binding(&ThreadOwner::root(self.child.observer.session_id()))
                .as_ref()
                == Some(&self.binding)
    }
}

#[derive(Clone)]
struct Requirements {
    model: ReviewAnswerModel,
    source: Value,
    provider: Arc<crate::provider::Provider>,
    resources: u64,
    executing: bool,
}

impl Requirements {
    async fn capture(controller: &SessionController) -> Result<Self> {
        let (model, source) = controller.broker.lock().await.review_settings()?;
        Ok(Self {
            model,
            source,
            provider: controller.provider_handle(),
            resources: controller.mcp_revision.load(Ordering::Acquire),
            executing: controller.coordinator.execution.available_permits() == 0,
        })
    }

    fn matches(&self, other: &Self) -> bool {
        self.model == other.model
            && self.source == other.source
            && self.resources == other.resources
            && self.executing == other.executing
            && Arc::ptr_eq(&self.provider, &other.provider)
    }
}

impl ReviewStandbyPool {
    /// Get the idle controller only after preparation published a live native binding.
    pub(super) fn ready_child(&self, source: &str) -> Option<&Arc<SessionController>> {
        self.source
            .get(source)?
            .standby
            .as_ref()?
            .ready
            .get()
            .filter(|ready| ready.live())
            .map(|ready| &ready.child)
    }

    pub(super) fn close_view(&mut self, source: &str, document: &DocumentId) {
        if let Some(open) = self.source.get_mut(source) {
            open.views.remove(document);
            if open.views.is_empty() {
                self.source.remove(source);
            }
        }
    }

    pub(super) fn close_session(&mut self, source: &str) {
        self.source.remove(source);
    }

    fn collect(&mut self) {
        while let Some(joined) = self.workers.try_join_next() {
            if let Err(error) = joined {
                tracing::warn!(%error, "review standby worker stopped");
            }
        }
    }
}

pub(super) async fn shutdown(registry: &SessionControllerRegistry) -> Result<()> {
    let mut workers = {
        let mut pool = registry.review_standby.lock().await;
        pool.closed = true;
        pool.source.clear();
        std::mem::take(&mut pool.workers)
    };
    while let Some(joined) = workers.join_next().await {
        joined.context("collect review standby worker")?;
    }
    Ok(())
}

pub(super) async fn activate(
    registry: &Arc<SessionControllerRegistry>,
    source: &Arc<SessionController>,
    document: DocumentId,
    sink: &MessageSender,
) {
    let mut pool = registry.review_standby.lock().await;
    if pool.closed || !source.provider_handle().descriptor().capability.native_fork {
        return;
    }
    pool.source
        .entry(source.observer.session_id().to_owned())
        .or_default()
        .views
        .insert(document);
    // Rendering the plan never waits for broker admission, a history read, or native startup.
    let registry = registry.clone();
    let source = source.clone();
    let sink = sink.clone();
    pool.workers
        .spawn(async move { refresh(&registry, &source, &sink).await });
}

/// Provider-local settings may have changed from a different tab.
pub(super) async fn refresh_open(registry: &Arc<SessionControllerRegistry>, sink: &MessageSender) {
    let sources = registry
        .review_standby
        .lock()
        .await
        .source
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    for id in sources {
        let controller = registry.controller_by_id.read().await.get(&id).cloned();
        if let Some(controller) = controller {
            refresh(registry, &controller, sink).await;
        }
    }
}

pub(super) async fn refresh(
    registry: &Arc<SessionControllerRegistry>,
    source: &Arc<SessionController>,
    sink: &MessageSender,
) {
    let source_id = source.observer.session_id();
    if !registry
        .review_standby
        .lock()
        .await
        .source
        .contains_key(source_id)
    {
        return;
    }
    let result = async {
        let requirements = Requirements::capture(source).await?;
        let mut pool = registry.review_standby.lock().await;
        ensure_standby(
            &mut pool,
            registry,
            source,
            requirements,
            sink,
            Duration::from_secs(1),
        )
    }
    .await;
    if let Err(error) = result {
        tracing::warn!(source_id, error = %format_args!("{error:#}"), "could not prepare review standby");
    }
}

fn directory(source: &SessionController) -> PathBuf {
    crate::storage::state::directory(&source.data_root)
        .join(source.observer.session_id())
        .join("review-forks")
}

fn ensure_standby(
    pool: &mut ReviewStandbyPool,
    registry: &Arc<SessionControllerRegistry>,
    source: &Arc<SessionController>,
    requirements: Requirements,
    sink: &MessageSender,
    retry_delay: Duration,
) -> Result<()> {
    let source_id = source.observer.session_id();
    let Some(open) = pool.source.get(source_id) else {
        return Ok(());
    };
    if let Some(slot) = &open.standby
        && slot.requirements.matches(&requirements)
        && !slot.claim.is_closed()
        && slot.ready.get().is_none_or(ReadyStandby::live)
    {
        return Ok(());
    }
    pool.collect();
    let directory = directory(source);
    let unused = directory.join("unused");
    std::fs::create_dir_all(&unused)?;
    std::fs::create_dir_all(directory.join("chains"))?;
    if !pool.recovered.contains(source_id) {
        let stale = std::fs::read_dir(&unused)?
            .map(|entry| {
                Ok(Uuid::parse_str(
                    entry?
                        .file_name()
                        .to_str()
                        .context("invalid standby file name")?,
                )?)
            })
            .collect::<Result<BTreeSet<_>>>()?;
        for id in stale {
            let registry = registry.clone();
            let marker = unused.join(id.to_string());
            let source_id = source_id.to_owned();
            pool.workers.spawn(async move {
                discard(&registry, &source_id, id, &marker).await;
            });
        }
        pool.recovered.insert(source_id.to_owned());
    }
    let id = Uuid::new_v4();
    let marker = unused.join(id.to_string());
    // The marker precedes fork creation. A host interrupted during creation can retire the orphan.
    std::fs::write(&marker, id.to_string())?;
    let (claim, mut disposition) = tokio::sync::oneshot::channel();
    let ready = Arc::new(std::sync::OnceLock::new());
    pool.source
        .get_mut(source_id)
        .expect("review is open")
        .standby = Some(Standby {
        id,
        requirements: requirements.clone(),
        ready: ready.clone(),
        claim,
    });
    let registry = registry.clone();
    let source = source.clone();
    let sink = sink.clone();
    pool.workers.spawn(async move {
        let source_id = source.observer.session_id();
        match prepare(&registry, &source, id, &requirements, &sink).await {
            Ok(child) => {
                let _ = ready.set(child);
                source.trace.record(source_id, "review.standby.ready", json!({"fork":id}));
                if disposition.await.is_err() {
                    discard(&registry, source_id, id, &marker).await;
                }
            }
            Err(error) => {
                tracing::warn!(%id, error = %format_args!("{error:#}"), "review standby unavailable");
                if !discard(&registry, source_id, id, &marker).await { return; }
                tokio::select! {
                    _ = &mut disposition => return,
                    () = tokio::time::sleep(retry_delay) => {}
                }
                // A tab close or a newer configuration can retire this retry while it reads the source.
                let result = async {
                    let current = Requirements::capture(&source).await?;
                    let mut pool = registry.review_standby.lock().await;
                    let Some(open) = pool.source.get_mut(source_id) else { return Ok(()) };
                    if open.standby.as_ref().is_none_or(|slot| slot.id != id) { return Ok(()); }
                    open.standby.take();
                    ensure_standby(&mut pool, &registry, &source, current, &sink,
                        (retry_delay * 2).min(Duration::from_secs(30)))
                }.await;
                if let Err(error) = result { tracing::warn!(source_id, %error, "could not retry review standby"); }
            }
        }
    });
    Ok(())
}

async fn prepare(
    registry: &Arc<SessionControllerRegistry>,
    source: &SessionController,
    id: Uuid,
    expected: &Requirements,
    sink: &MessageSender,
) -> Result<ReadyStandby> {
    let child = super::review_questions::open_answer_session(
        registry,
        source.observer.session_id(),
        id,
        sink,
    )
    .await?;
    let generation = child.provider_lane.read().await;
    let catalog = match &expected.model {
        ReviewAnswerModel::Override(_) => {
            let job = child.broker.lock().await.prepare_configuration_catalog()?;
            let result = job.run().await?;
            child
                .broker
                .lock()
                .await
                .commit_configuration_catalog(result)?
        }
        ReviewAnswerModel::Inherited { .. } => Vec::new(),
    };
    let request = {
        let mut broker = child.broker.lock().await;
        broker.apply_review_model(&expected.model, &catalog)?;
        broker.provider_request(ProviderInput::from_user_text(""), PromptMode::PlanQuestion)?
    };
    let provider = child.provider_handle();
    provider.thread_open(request).await?;
    let binding = provider
        .ready_binding(&ThreadOwner::root(child.observer.session_id()))
        .context("review fork binding is unavailable")?;
    drop(generation);
    ensure!(
        expected.matches(&Requirements::capture(source).await?),
        "source changed during review fork preparation"
    );
    Ok(ReadyStandby {
        child,
        provider,
        binding,
    })
}

async fn discard(
    registry: &SessionControllerRegistry,
    source: &str,
    id: Uuid,
    marker: &Path,
) -> bool {
    let result = async {
        let id = id.to_string();
        let exists = {
            let store = crate::storage::HistoryReader::current(Path::new(&registry.initialize.data_root))?;
            if let Some(saved) = store.load_session(&id)? {
                ensure!(saved.purpose == crate::session::SessionPurpose::PlanReviewAnswer &&
                    store.list_session_event(&id)?.iter().any(|event| matches!(&event.detail,
                        crate::timeline::SessionEventKind::Forked { source_session_id, .. } if source_session_id == source))
                    && store.latest_exchange(&id)?.is_none(), "marked fork is not an unused answer from this source");
                true
            } else { false }
        };
        if exists {
            let child = registry.resolve(&id).await?;
            require_idle_deletion(&child)?;
            child.input_stopping.store(true, Ordering::Release);
            child.coordinator.submissions.changed.notify_one();
            child.provider_handle().session_terminate_owner(&ThreadOwner::root(&id)).await?;
            delete_stopping_session(registry, &id, Some(&child)).await?;
        }
        std::fs::remove_file(marker)?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    if let Err(error) = result {
        tracing::warn!(%id, error = %format_args!("{error:#}"), "could not retire unused review fork");
        return false;
    }
    true
}

/// Claim only a matching live binding. An opening fork stays available for a later card.
pub(super) async fn claim(
    registry: &Arc<SessionControllerRegistry>,
    source: &Arc<SessionController>,
    chain: Uuid,
    selection: &ReviewAnswerModel,
    sink: &MessageSender,
) -> Result<Arc<SessionController>> {
    let source_id = source.observer.session_id();
    let directory = directory(source);
    let alias = directory.join("chains").join(chain.to_string());
    match std::fs::read_to_string(&alias) {
        Ok(id) => {
            return super::review_questions::open_answer_session(
                registry,
                source_id,
                Uuid::parse_str(&id)?,
                sink,
            )
            .await;
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }
    // Older and cold-started chains already own their deterministic session.
    let existing = crate::storage::HistoryReader::current(&source.data_root)?
        .load_session(&chain.to_string())?
        .is_some();
    if !existing
        && registry
            .review_standby
            .lock()
            .await
            .source
            .contains_key(source_id)
    {
        let current = match Requirements::capture(source).await {
            Ok(current) => current,
            Err(error) => {
                tracing::warn!(source_id, %error, "could not verify review standby");
                return super::review_questions::open_answer_session(
                    registry, source_id, chain, sink,
                )
                .await;
            }
        };
        let mut pool = registry.review_standby.lock().await;
        let ready = pool
            .ready_child(source_id)
            .filter(|_| {
                pool.source
                    .get(source_id)
                    .and_then(|open| open.standby.as_ref())
                    .is_some_and(|slot| {
                        current.model == *selection && current.matches(&slot.requirements)
                    })
                    && source.agent_coordinator.active_count() == 0
            })
            .cloned();
        if let Some(child) = ready {
            let open = pool
                .source
                .get_mut(source_id)
                .expect("ready review is open");
            let slot = open.standby.as_ref().expect("ready standby exists");
            // The rename both binds the chain across restarts and removes it from orphan recovery.
            if let Err(error) =
                std::fs::rename(directory.join("unused").join(slot.id.to_string()), alias)
            {
                tracing::warn!(source_id, %error, "could not bind review standby to its chain");
                drop(pool);
                return super::review_questions::open_answer_session(
                    registry, source_id, chain, sink,
                )
                .await;
            }
            let slot = open.standby.take().unwrap();
            slot.claim
                .send(())
                .map_err(|_| anyhow::anyhow!("review standby worker stopped before claim"))?;
            if let Err(error) = ensure_standby(
                &mut pool,
                registry,
                source,
                current,
                sink,
                Duration::from_secs(1),
            ) {
                tracing::warn!(source_id, %error, "could not replenish review standby");
            }
            source.trace.record(
                source_id,
                "review.standby.claimed",
                json!({"fork":child.observer.session_id(),"chain":chain}),
            );
            return Ok(child);
        }
        if let Err(error) = ensure_standby(
            &mut pool,
            registry,
            source,
            current,
            sink,
            Duration::from_secs(1),
        ) {
            tracing::warn!(source_id, %error, "could not refresh review standby");
        }
    }
    super::review_questions::open_answer_session(registry, source_id, chain, sink).await
}
