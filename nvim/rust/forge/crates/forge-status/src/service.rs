use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};
use base64::{Engine, engine::general_purpose::STANDARD};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock, TargetRange, TextPosition, TextRange},
    identity::{BlockId, DocumentId, TargetId},
    patch::{BufferPatch, BufferSnapshot},
    sequence::SequenceEdit,
    text::BufferText,
};
use forge_diff::{
    display::{ChunkLimits, DisplayCursor, DisplayState},
    engine::{DiffEngine, DiffRequest},
    syntax::{SyntaxEngine, SyntaxHandle, SyntaxRequest},
    workers::WorkPriority,
};
use forge_git::{
    store::RepositoryStore,
    writer::{
        DiscardSource, GitWriteAction, GitWriteService, GitWriteTicket, PatchDirection, PatchTarget,
    },
};
use serde::{Deserialize, Serialize};
use tokio::time::timeout;

use crate::{
    StatusSection,
    document::{DOCUMENT_BYTES, FileTarget, HunkTarget, StatusDocument, label},
    protocol::{
        StatusDelta, StatusInput, StatusLocation, StatusSelection, StatusSnapshot, file_target,
    },
    source::sources,
};

const OPEN_STAGE_DEADLINE: Duration = Duration::from_secs(8);

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag = "state", content = "diagnostic", rename_all = "snake_case")]
pub enum BodyState {
    Deferred,
    Loading,
    Partial,
    Ready,
    Unavailable(String),
    Failed(String),
}

#[derive(Clone, Debug, Serialize)]
pub struct BodyDelivery {
    pub patch: Option<BufferPatch>,
    pub document: DocumentId,
    pub file: u64,
    pub generation: u64,
    pub snapshot: Option<BufferSnapshot>,
    pub state: BodyState,
    pub more: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub syntax_diagnostic: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct StatusOpenTarget {
    #[serde(flatten)]
    pub input: StatusInput,
    pub id: String,
    pub kind: &'static str,
    pub path: String,
    pub row: usize,
    pub column: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_revision: Option<String>,
}

/// Reports bounded phase durations for one successful status document open.
#[derive(Debug, Serialize)]
pub struct StatusOpenTiming {
    pub admission_ms: u128,
    pub repository_discovery_ms: u128,
    pub repository_observation_ms: u128,
    pub repository_observation: StatusObservationTiming,
    pub context_collection_ms: u128,
    pub ignored_paths_ms: u128,
    pub document_build_ms: u128,
    pub snapshot_ms: u128,
    pub document_insert_ms: u128,
    pub total_ms: u128,
    pub changed_path_count: usize,
    pub file_count: usize,
}

/// Projects repository timing into the status transport without coupling Git state to Serde.
#[derive(Debug, Serialize)]
pub struct StatusObservationTiming {
    pub queue_wait_ms: u128,
    pub head_and_index_ms: u128,
    pub initial_status_ms: u128,
    pub initial_metadata_ms: u128,
    pub verification_status_ms: u128,
    pub line_stats_ms: u128,
    pub line_stats_skipped: bool,
    pub line_stats_eligible_pairs: usize,
    pub line_stats_skipped_pairs: usize,
    pub line_stats_preparation_us: u128,
    pub line_stats_verification_us: u128,
    pub line_stats_source_reads: usize,
    pub line_stats_fast_count_pairs: usize,
    pub line_stats_cache_hits: usize,
    pub line_stats_count_cache_hits: usize,
    pub line_stats_count_metadata_us: u128,
    pub line_stats_cache_retained: usize,
    pub line_stats_cache_skipped: usize,
    pub line_stats_retained_entries: usize,
    pub line_stats_retained_source_bytes: usize,
    pub line_stats_retained_result_bytes: usize,
    pub line_stats_staged_source_us: u128,
    pub line_stats_worktree_source_us: u128,
    pub line_stats_diff_us: u128,
    pub line_stats_compared_pairs: usize,
    pub line_stats_unchanged_side_pairs: usize,
    pub line_stats_source_bytes: usize,
    pub verification_metadata_ms: u128,
    pub identity_verification_ms: u128,
    pub collection_total_ms: u128,
    pub retry_ms: u128,
    pub retry_reason: Option<String>,
    pub overall_ms: u128,
    pub attempt_count: usize,
}

impl From<forge_git::snapshot::ObservationTiming> for StatusObservationTiming {
    fn from(timing: forge_git::snapshot::ObservationTiming) -> Self {
        Self {
            queue_wait_ms: timing.queue_wait_ms,
            head_and_index_ms: timing.head_and_index_ms,
            initial_status_ms: timing.initial_status_ms,
            initial_metadata_ms: timing.initial_metadata_ms,
            verification_status_ms: timing.verification_status_ms,
            line_stats_ms: timing.line_stats_ms,
            line_stats_skipped: timing.line_stats_detail.skipped,
            line_stats_eligible_pairs: timing.line_stats_detail.eligible_pairs,
            line_stats_skipped_pairs: timing.line_stats_detail.skipped_pairs,
            line_stats_preparation_us: timing.line_stats_detail.preparation_us,
            line_stats_verification_us: timing.line_stats_detail.verification_us,
            line_stats_source_reads: timing.line_stats_detail.source_reads,
            line_stats_fast_count_pairs: timing.line_stats_detail.fast_count_pairs,
            line_stats_cache_hits: timing.line_stats_detail.cache_hits,
            line_stats_count_cache_hits: timing.line_stats_detail.count_cache_hits,
            line_stats_count_metadata_us: timing.line_stats_detail.count_metadata_us,
            line_stats_cache_retained: timing.line_stats_detail.cache_retained,
            line_stats_cache_skipped: timing.line_stats_detail.cache_skipped,
            line_stats_retained_entries: timing.line_stats_detail.retained_entries,
            line_stats_retained_source_bytes: timing.line_stats_detail.retained_source_bytes,
            line_stats_retained_result_bytes: timing.line_stats_detail.retained_result_bytes,
            line_stats_staged_source_us: timing.line_stats_detail.staged_source_us,
            line_stats_worktree_source_us: timing.line_stats_detail.worktree_source_us,
            line_stats_diff_us: timing.line_stats_detail.diff_us,
            line_stats_compared_pairs: timing.line_stats_detail.compared_pairs,
            line_stats_unchanged_side_pairs: timing.line_stats_detail.unchanged_side_pairs,
            line_stats_source_bytes: timing.line_stats_detail.source_bytes,
            verification_metadata_ms: timing.verification_metadata_ms,
            identity_verification_ms: timing.identity_verification_ms,
            collection_total_ms: timing.collection_total_ms,
            retry_ms: timing.retry_ms,
            retry_reason: timing.retry_reason,
            overall_ms: timing.overall_ms,
            attempt_count: timing.attempt_count,
        }
    }
}

#[derive(Default)]
pub(crate) struct SelectedFile {
    pub whole: bool,
    pub hunk: std::collections::HashSet<forge_diff::raw::RawHunkId>,
}

/// Owns at most eight status documents and reuses the host's repository and analysis services.
#[derive(Clone)]
pub struct StatusService {
    pub(crate) store: Arc<RepositoryStore>,
    pub(crate) diff: Arc<DiffEngine>,
    pub(crate) syntax: Arc<SyntaxEngine>,
    pub(crate) writer: Arc<GitWriteService>,
    context: Arc<crate::context::StatusContextService>,
    pub(crate) document: Arc<Mutex<HashMap<DocumentId, Arc<tokio::sync::Mutex<StatusDocument>>>>>,
    admitted: Arc<crate::document::DocumentAdmissionStore>,
    pub(crate) closed: Arc<AtomicBool>,
    pub(crate) ignored_directory: Arc<Mutex<Option<PathBuf>>>,
    pub(crate) mutation: Arc<crate::mutation::MutationStore>,
}

struct LoadingLease(Arc<AtomicBool>);

impl Drop for LoadingLease {
    fn drop(&mut self) {
        self.0.store(false, Ordering::Release);
    }
}

impl StatusService {
    pub fn new(
        store: Arc<RepositoryStore>,
        diff: Arc<DiffEngine>,
        syntax: Arc<SyntaxEngine>,
        writer: Arc<GitWriteService>,
    ) -> Self {
        Self {
            context: Arc::new(crate::context::StatusContextService::new(Arc::clone(
                &store,
            ))),
            store,
            diff,
            syntax,
            writer,
            document: Arc::new(Mutex::new(HashMap::new())),
            admitted: Arc::new(crate::document::DocumentAdmissionStore::default()),
            closed: Arc::new(AtomicBool::new(false)),
            ignored_directory: Arc::new(Mutex::new(None)),
            mutation: Arc::new(crate::mutation::MutationStore::default()),
        }
    }

    pub fn configure_ignored_directory(&self, directory: PathBuf) -> Result<()> {
        ensure!(
            directory.is_absolute(),
            "ignored store directory must be absolute"
        );
        let mut configured = self
            .ignored_directory
            .lock()
            .expect("ignored directory lock");
        if let Some(current) = configured.as_ref() {
            ensure!(
                current == &directory,
                "ignored store directory is immutable after initialization"
            );
            return Ok(());
        }
        ensure!(
            self.document
                .lock()
                .expect("status document lock")
                .is_empty(),
            "ignored store must be configured before documents open"
        );
        *configured = Some(directory);
        Ok(())
    }

    async fn load_ignored(
        &self,
        repository: &forge_git::repository::RepositoryState,
    ) -> Result<crate::ignored::IgnoredPathSet> {
        let directory = self
            .ignored_directory
            .lock()
            .expect("ignored directory lock")
            .clone();
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("ignored paths require a worktree")?;
        self.store
            .reads
            .submit(1024 * 1024, move |cancellation| {
                cancellation.check()?;
                let result = crate::ignored::load(directory.as_deref(), &root)?;
                cancellation.check()?;
                Ok(result)
            })?
            .finish()
            .await
    }

    /// Publishes all section and file headers without acquiring diff bodies or syntax trees.
    pub async fn open(&self, id: DocumentId, path: PathBuf) -> Result<StatusSnapshot> {
        let (snapshot, _) = self.open_with_timing(id, path).await?;
        Ok(snapshot)
    }

    /// Publishes the initial status snapshot and reports the cost of each acquisition phase.
    pub async fn open_with_timing(
        &self,
        id: DocumentId,
        path: PathBuf,
    ) -> Result<(StatusSnapshot, StatusOpenTiming)> {
        let started = Instant::now();
        id.validate()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "status service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let admission_ms = started.elapsed().as_millis();
        let stage_started = Instant::now();
        let repository = timeout(OPEN_STAGE_DEADLINE, self.store.open(path.clone()))
            .await
            .context("status open timed out while discovering its repository")??
            .context("status path has no Git repository")?;
        let repository_discovery_ms = stage_started.elapsed().as_millis();
        let stage_started = Instant::now();
        let (observation, repository_observation) = timeout(
            OPEN_STAGE_DEADLINE,
            repository.observe_with_timing(&self.store),
        )
        .await
        .context("status open timed out while observing repository state")??;
        let repository_observation_ms = stage_started.elapsed().as_millis();
        let changed_path_count = observation.path.len();
        let stage_started = Instant::now();
        let context = timeout(
            OPEN_STAGE_DEADLINE,
            self.context.collect_repository(Arc::clone(&repository)),
        )
        .await
        .context("status open timed out while collecting repository context")??;
        let context_collection_ms = stage_started.elapsed().as_millis();
        let stage_started = Instant::now();
        let ignored = self.load_ignored(&repository).await?;
        let ignored_paths_ms = stage_started.elapsed().as_millis();
        let stage_started = Instant::now();
        let document = StatusDocument::new(
            id.clone(),
            repository,
            &observation,
            admission,
            context,
            ignored,
        )?;
        let document_build_ms = stage_started.elapsed().as_millis();
        let stage_started = Instant::now();
        let snapshot_ms = stage_started.elapsed().as_millis();
        let stage_started = Instant::now();
        let snapshot = self.register_document(document).await?;
        let file_count = snapshot.file.len();
        let document_insert_ms = stage_started.elapsed().as_millis();
        Ok((
            snapshot,
            StatusOpenTiming {
                admission_ms,
                repository_discovery_ms,
                repository_observation_ms,
                repository_observation: repository_observation.into(),
                context_collection_ms,
                ignored_paths_ms,
                document_build_ms,
                snapshot_ms,
                document_insert_ms,
                total_ms: started.elapsed().as_millis(),
                changed_path_count,
                file_count,
            },
        ))
    }

    pub async fn snapshot(&self, id: &DocumentId) -> Result<StatusSnapshot> {
        Ok(self.get(id)?.lock().await.snapshot())
    }

    pub async fn open_local(&self, id: DocumentId, filename: PathBuf) -> Result<StatusSnapshot> {
        id.validate()?;
        ensure!(
            filename.is_absolute(),
            "local diff requires an absolute filename"
        );
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "status service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let repository = self
            .store
            .open(
                filename
                    .parent()
                    .context("local file has no parent")?
                    .to_owned(),
            )
            .await?
            .context("local file has no repository")?;
        let root = repository
            .identity
            .worktree_root
            .as_ref()
            .context("local diff requires worktree")?;
        let relative = filename
            .strip_prefix(root)
            .context("local file is outside resolved worktree")?;
        let mut raw = Vec::new();
        for component in relative.components() {
            let std::path::Component::Normal(component) = component else {
                anyhow::bail!("local file path is not normalized")
            };
            if !raw.is_empty() {
                raw.push(b'/');
            }
            raw.extend_from_slice(component.as_encoded_bytes());
        }
        let path = forge_git::RepositoryPath::new(raw)?;
        forge_git::resolve_argument(&path)?;
        let observation = repository.observe(&self.store).await?;
        let ignored = self.load_ignored(&repository).await?;
        let document = StatusDocument::local(
            id.clone(),
            repository,
            &observation,
            path,
            admission,
            ignored,
        )?;
        self.register_document(document).await
    }

    pub async fn open_comparison(
        &self,
        id: DocumentId,
        workspace: PathBuf,
        mut request: forge_git::revision::comparison::ComparisonRequest,
    ) -> Result<StatusSnapshot> {
        id.validate()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "status service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let repository = self
            .store
            .open(workspace)
            .await?
            .context("comparison has no repository")?;
        let observation = forge_git::revision::comparison::observe(
            &self.store,
            Arc::clone(&repository),
            request.clone(),
        )
        .await?
        .value;
        let title = request.reference.clone();
        request.reference = observation.commit.to_string();
        let document = StatusDocument::comparison(
            id.clone(),
            repository,
            &observation,
            request,
            admission,
            title,
        )?;
        self.register_document(document).await
    }

    /// Reconciles changed file ranges while preserving unchanged body blocks and their identities.
    pub async fn refresh(&self, id: &DocumentId) -> Result<Option<StatusDelta>> {
        let owner = self.get(id)?;
        let repository = Arc::clone(&owner.lock().await.repository);
        self.writer.reconcile(&repository).await?;
        if self.has_pending_mutation(&repository).await {
            return Ok(None);
        }
        let comparison = owner.lock().await.comparison.clone();
        repository.invalidate()?;
        if let Some(comparison) = comparison {
            let generation = repository.generation();
            let observation = forge_git::revision::comparison::observe(
                &self.store,
                Arc::clone(&repository),
                comparison,
            )
            .await?
            .value;
            let mut document = owner.lock().await;
            ensure!(
                generation == repository.generation(),
                "status refresh was invalidated"
            );
            return crate::reconcile::reconcile(
                &mut document,
                &forge_git::snapshot::HeadState::Detached {
                    target: observation.commit,
                },
                &observation.path,
                None,
                Default::default(),
            );
        }
        let observation = repository.observe(&self.store).await?;
        let ignored = self.load_ignored(&repository).await?;
        let local_path = owner.lock().await.local_path.clone();
        if let Some(path) = local_path {
            let journal = self.lock_mutations(&repository).await?;
            if journal.has_pending() {
                return Ok(None);
            }
            let observed: Vec<_> = observation
                .path
                .iter()
                .filter(|observed| observed.change.affects(&path))
                .cloned()
                .collect();
            let mut document = owner.lock().await;
            ensure!(
                observation.generation == repository.generation(),
                "status refresh was invalidated"
            );
            return crate::reconcile::reconcile(
                &mut document,
                &observation.head,
                &observed,
                None,
                ignored,
            );
        }
        let context = self
            .context
            .collect_repository(Arc::clone(&repository))
            .await?;
        let journal = self.lock_mutations(&repository).await?;
        if journal.has_pending() {
            return Ok(None);
        }
        let mut document = owner.lock().await;
        ensure!(
            observation.generation == repository.generation(),
            "status refresh was invalidated"
        );
        crate::reconcile::reconcile(
            &mut document,
            &observation.head,
            &observation.path,
            Some(context),
            ignored,
        )
    }

    pub async fn context_action(
        &self,
        input: StatusInput,
    ) -> Result<crate::context::ContextAction> {
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        let target = validate_input(&mut document, &input)?;
        document
            .context
            .as_ref()
            .context("comparison has no repository context")?
            .action(&target)
            .context("context target is unavailable")
    }

    pub async fn context_commit_message(&self, workspace: PathBuf, oid: String) -> Result<String> {
        self.context.commit_message(workspace, oid).await
    }

    pub async fn context_issues(&self, input: StatusInput, text: &str) -> Result<GitWriteTicket> {
        let owner = self.get(&input.document)?;
        let (repository, action) = {
            let mut document = owner.lock().await;
            let target = validate_input(&mut document, &input)?;
            let context = document
                .context
                .as_ref()
                .context("comparison is read-only")?;
            ensure!(
                matches!(
                    context.action(&target),
                    Some(crate::context::ContextAction::Issues { .. })
                ),
                "input does not select Issues"
            );
            (
                Arc::clone(&document.repository),
                GitWriteAction::UpdateRepositoryConfig {
                    expected: context.config_source.clone(),
                    replacement: context.issues_replacement(text)?,
                },
            )
        };
        let intent = self.writer.prepare(repository, action).await?;
        self.writer.submit(intent)
    }

    /// Produces one bounded body delivery. `more` requests another visible-demand turn.
    /// Body blocks remain installed when demand stops or a window collapses its native fold.
    pub async fn navigate(&self, input: StatusInput, forward: bool) -> Result<NavigationDelivery> {
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        resolve_position(&document, &input)?;
        let destination = crate::navigation::destination(&document, &input.location, forward)?;
        match destination {
            crate::navigation::Destination::Demand(target) => {
                let mut demand = input;
                demand.location = StatusLocation::File {
                    id: document.file[&target].id,
                };
                demand.action = "demand".into();
                drop(document);
                let body = self.demand(demand).await?;
                Ok(NavigationDelivery {
                    effect: None,
                    body: Some(body),
                    more: true,
                })
            }
            crate::navigation::Destination::Header(location) => {
                document.input.insert(input.view.clone(), input.sequence);
                Ok(NavigationDelivery {
                    body: None,
                    more: false,
                    effect: Some(NavigationEffect {
                        document: input.document,
                        revision: document.revision,
                        view: input.view,
                        sequence: input.sequence,
                        id: document.next_id("navigation")?,
                        kind: "cursor",
                        location,
                    }),
                })
            }
            crate::navigation::Destination::End => {
                document.input.insert(input.view, input.sequence);
                Ok(NavigationDelivery {
                    effect: None,
                    body: None,
                    more: false,
                })
            }
        }
    }

    pub async fn demand(&self, mut input: StatusInput) -> Result<BodyDelivery> {
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        if matches!(input.location, StatusLocation::File { .. }) {
            ensure!(input.revision <= document.revision, "status demand uses a future revision");
            input.revision = document.revision;
        }
        let target = validate_input(&mut document, &input)?;
        let file_id = document
            .hunk
            .get(&target)
            .map_or(target.clone(), |hunk| hunk.file.clone());
        let file = document
            .file
            .get_mut(&file_id)
            .context("target is not a status file")?;
        if matches!(file.state, BodyState::Loading)
            && file
                .loading
                .as_ref()
                .is_some_and(|active| active.load(Ordering::Acquire))
        {
            return Ok(BodyDelivery {
                patch: None,
                document: input.document.clone(),
                file: file.id,
                generation: file.generation,
                snapshot: None,
                state: BodyState::Loading,
                more: false,
                syntax_diagnostic: None,
            });
        }
        if file.cursor.is_none()
            && !matches!(
                file.state,
                BodyState::Ready | BodyState::Unavailable(_) | BodyState::Failed(_)
            )
        {
            let active = Arc::new(AtomicBool::new(true));
            file.loading = Some(Arc::clone(&active));
            file.state = BodyState::Loading;
            let lease = LoadingLease(active);
            let captured = (
                file.record.clone(),
                file.stamp.clone(),
                file.section,
                file.prepared.clone(),
                file.projected.clone(),
            );
            let repository = Arc::clone(&document.repository);
            drop(document);
            let loaded = self
                .load(
                    &repository,
                    &captured.0,
                    captured.2,
                    captured.3.as_ref(),
                    captured.4.as_ref(),
                )
                .await;
            ensure!(
                self.get(&input.document)
                    .is_ok_and(|current| Arc::ptr_eq(&current, &owner)),
                "status demand document closed"
            );
            document = owner.lock().await;
            let file = document
                .file
                .get_mut(&file_id)
                .context("status demand target disappeared")?;
            ensure!(
                file.record == captured.0
                    && file.stamp == captured.1
                    && file
                        .loading
                        .as_ref()
                        .is_some_and(|active| Arc::ptr_eq(active, &lease.0)),
                "status demand was superseded"
            );
            match loaded {
                Ok((analysis, cursor, old_syntax, new_syntax, _diagnostic)) => {
                    file.analysis = Some(analysis);
                    file.cursor = Some(cursor);
                    file.old_syntax = old_syntax;
                    file.new_syntax = new_syntax;
                    file.state = BodyState::Partial;
                }
                Err(error) => {
                    file.state = if error
                        .downcast_ref::<crate::source::SourceUnavailable>()
                        .is_some()
                    {
                        BodyState::Unavailable(format!("{error:#}"))
                    } else {
                        BodyState::Failed(format!("{error:#}"))
                    };
                }
            }
            drop(lease);
        }
        let mut file = document
            .file
            .remove(&file_id)
            .context("status file disappeared")?;
        let result = self.deliver(&mut document, &file_id, &mut file);
        document.file.insert(file_id, file);
        self.queue_context(&mut document);
        result
    }

    /// Resolves stage, unstage, and discard from native file or raw-hunk identity.
    /// Discard confirmation remains an editor effect completed before this call is admitted.
    pub async fn input(&self, input: StatusInput) -> Result<crate::StatusOperationTicket> {
        match self.act(input, None).await? {
            StatusAction::Write(ticket) => Ok(crate::StatusOperationTicket::Write(ticket)),
            StatusAction::Accepted(_, ticket) => Ok(ticket),
            StatusAction::Projection(_) => {
                anyhow::bail!("status input completed without a Git operation")
            }
        }
    }

    pub async fn input_selection(
        &self,
        input: StatusInput,
        selection: StatusSelection,
    ) -> Result<crate::StatusOperationTicket> {
        match self.act(input, Some(selection)).await? {
            StatusAction::Write(ticket) => Ok(crate::StatusOperationTicket::Write(ticket)),
            StatusAction::Accepted(_, ticket) => Ok(ticket),
            StatusAction::Projection(_) => {
                anyhow::bail!("status selection completed without a Git operation")
            }
        }
    }

    pub async fn act(
        &self,
        input: StatusInput,
        selection: Option<StatusSelection>,
    ) -> Result<StatusAction> {
        let private_unignore = if input.action == "unstage" {
            let owner = self.get(&input.document)?;
            let document = owner.lock().await;
            match &input.location {
                StatusLocation::Section { section } => *section == StatusSection::Ignored,
                StatusLocation::File { id } | StatusLocation::Body { file: id, .. } => document
                    .file
                    .get(&file_target(*id))
                    .is_some_and(|file| file.section == StatusSection::Ignored),
                _ => false,
            }
        } else {
            false
        };
        if matches!(input.action.as_str(), "stage" | "unstage") && !private_unignore {
            let (accepted, ticket) = self.admit_mutation(input, selection).await?;
            return Ok(StatusAction::Accepted(accepted, ticket));
        }
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        ensure!(
            document.comparison.is_none(),
            "comparison documents are read-only"
        );
        let target = validate_input(&mut document, &input)?;
        ensure!(
            matches!(
                input.action.as_str(),
                "stage" | "unstage" | "discard" | "ignore"
            ),
            "unknown status action"
        );
        let selected = selected_files(&document, &input, selection.as_ref(), &target)?;
        let mut selected: Vec<_> = selected.into_iter().collect();
        selected.sort_by(|(left, _), (right, _)| {
            document.file[left]
                .record
                .path
                .raw()
                .cmp(document.file[right].record.path.raw())
        });
        let mut path_selection = HashMap::new();
        if input.action == "discard" {
            for (id, choice) in &selected {
                let entry = path_selection
                    .entry(document.file[id].record.path.clone())
                    .or_insert((0usize, false));
                entry.0 += 1;
                entry.1 |= !choice.whole;
            }
        }
        let mut captured = HashMap::new();
        let mut missing = Vec::new();
        for (id, _) in &selected {
            let file = &document.file[id];
            let mixed = path_selection
                .get(&file.record.path)
                .is_some_and(|(count, partial)| *count > 1 && *partial);
            if mixed {
                if let Some(analysis) = &file.analysis {
                    captured.insert(id.clone(), analysis.clone());
                } else {
                    missing.push((
                        id.clone(),
                        file.record.clone(),
                        file.section,
                        file.stamp.clone(),
                    ));
                }
            }
        }
        if !missing.is_empty() {
            let repository = Arc::clone(&document.repository);
            drop(document);
            let mut retained = 0usize;
            for (id, record, section, _) in &missing {
                let (source, _) = sources(&self.store, &repository, record, *section).await?;
                let analysis = self
                    .diff
                    .compare(DiffRequest {
                        source,
                        priority: WorkPriority::Visible,
                    })
                    .await
                    .map_err(|error| anyhow::anyhow!("discard analysis unavailable: {error:?}"))?;
                retained += analysis.retained_source_bytes() + analysis.retained_result_bytes();
                ensure!(
                    retained <= forge_git::writer::MAX_INPUT,
                    "discard sources exceed input budget"
                );
                captured.insert(id.clone(), analysis);
            }
            document = owner.lock().await;
            document._admission.check()?;
            ensure!(
                document.revision == input.revision
                    && document.input.get(&input.view) == Some(&input.sequence),
                "discard input changed while capturing sources"
            );
            for (id, record, _, stamp) in &missing {
                let current = document
                    .file
                    .get(id)
                    .context("discard target disappeared")?;
                ensure!(
                    current.record == *record && current.stamp == *stamp,
                    "discard source changed"
                );
            }
        }
        let mut action = Vec::new();
        let mut observed = Vec::new();
        let observed_head = document.observed_head.clone();
        let mut private_path = Vec::new();
        let mut ignored_unstage = std::collections::HashSet::new();
        for (id, selected) in selected {
            let file = &document.file[&id];
            if input.action == "ignore" {
                if file.section == StatusSection::Staged {
                    let mut path = vec![file.record.path.clone()];
                    if let forge_git::snapshot::PathState::Tracked {
                        relocation: Some(relocation), ..
                    } = &file.record.state
                        && relocation.kind == forge_git::snapshot::ChangeKind::Renamed
                    {
                        path.push(relocation.origin.clone());
                    }
                    ignored_unstage.extend(path.iter().cloned());
                    private_path.extend(path.iter().cloned());
                    action.push(whole_action(file.section, path, "unstage")?);
                } else if matches!(
                    file.section,
                    StatusSection::Unstaged | StatusSection::Untracked
                ) {
                    private_path.push(file.record.path.clone());
                }
                continue;
            }
            if file.section == StatusSection::Ignored
                && matches!(input.action.as_str(), "stage" | "unstage")
            {
                private_path.push(file.record.path.clone());
                if input.action == "unstage" {
                    continue;
                }
            }
            if (input.action == "stage" && file.section == StatusSection::Staged)
                || (input.action == "unstage" && file.section != StatusSection::Staged)
            {
                continue;
            }
            validate_action(file.section, &input.action)?;
            observed.push(forge_git::snapshot::ObservedPath {
                analysis: Default::default(),
                change: file.record.clone(),
                worktree: file.stamp.clone(),
                origin: file.origin_stamp.clone(),
            });
            if selected.whole && !captured.contains_key(&id) {
                let mut path = vec![file.record.path.clone()];
                if let forge_git::snapshot::PathState::Tracked {
                    relocation: Some(relocation),
                    ..
                } = &file.record.state
                    && relocation.kind == forge_git::snapshot::ChangeKind::Renamed
                {
                    path.push(relocation.origin.clone());
                }
                action.push(whole_action(file.section, path, &input.action)?);
            } else {
                let direction = match input.action.as_str() {
                    "stage" => PatchDirection::Stage,
                    "unstage" => PatchDirection::Unstage,
                    "discard" if file.section == StatusSection::Staged => {
                        PatchDirection::DiscardStaged
                    }
                    "discard" => PatchDirection::Discard,
                    _ => unreachable!("validated Git action"),
                };
                action.push(GitWriteAction::Patch {
                    direction,
                    target: PatchTarget {
                        path: file.record.path.clone(),
                        analysis: (**captured
                            .get(&id)
                            .or(file.analysis.as_ref())
                            .context("selection analysis expired")?)
                        .clone(),
                        selected: if selected.whole {
                            captured[&id].hunks().iter().map(|hunk| hunk.id).collect()
                        } else {
                            selected.hunk.into_iter().collect()
                        },
                    },
                });
            }
        }
        if input.action == "discard" {
            action = combine_discard(action)?;
        }
        private_path.sort_by(|left, right| left.raw().cmp(right.raw()));
        private_path.dedup();
        let expected: crate::ignored::IgnoredPathSet = private_path
            .iter()
            .filter(|path| document.ignored.contains(*path))
            .cloned()
            .collect();
        let repository = Arc::clone(&document.repository);
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("ignored paths require worktree")?;
        let directory = self
            .ignored_directory
            .lock()
            .expect("ignored directory lock")
            .clone();
        if !private_path.is_empty() {
            ensure!(
                directory.is_some(),
                "Forge ignored-path persistence is unavailable"
            );
        }
        drop(document);
        if action.is_empty() {
            ensure!(
                !private_path.is_empty(),
                "selection contains no applicable changes"
            );
            let directory = directory.context("ignored persistence unavailable")?;
            let ignored = input.action == "ignore";
            let retained = private_path
                .iter()
                .map(|path| path.retained_bytes() * 2 + 64)
                .sum::<usize>();
            ensure!(retained <= 1024 * 1024, "ignore input exceeds 1 MiB");
            self.store
                .reads
                .submit(retained, move |cancellation| {
                    cancellation.check()?;
                    crate::ignored::update(&directory, &root, &expected, &private_path, ignored)
                })?
                .finish()
                .await?;
            return Ok(StatusAction::Projection(
                self.refresh(&input.document).await?,
            ));
        }
        let action = if action.len() == 1 {
            action.pop().expect("one action")
        } else {
            GitWriteAction::Batch { action }
        };
        let mut intent = self.writer.prepare(repository, action).await?;
        intent.validate_observed_sources(&observed_head, &observed)?;
        if !private_path.is_empty() {
            let directory = directory.context("ignored persistence unavailable")?;
            let stage = input.action == "stage";
            let ignore = input.action == "ignore";
            let retained = private_path
                .iter()
                .map(|path| path.retained_bytes() * 2 + 64)
                .sum::<usize>()
                + directory.as_os_str().as_encoded_bytes().len()
                + root.as_os_str().as_encoded_bytes().len();
            intent.set_settlement_handler(
                Arc::new(move |outcome| {
                    let completed: Vec<_> = private_path
                        .iter()
                        .filter(|path| {
                            (!stage && !ignored_unstage.contains(*path))
                                || outcome.target.iter().any(|target| {
                                    target.path.as_ref() == Some(*path)
                                        && target.completion
                                            == forge_git::writer::TargetCompletion::Completed
                                })
                        })
                        .cloned()
                        .collect();
                    if !completed.is_empty() {
                        crate::ignored::update(&directory, &root, &expected, &completed, ignore)?;
                    }
                    Ok(())
                }),
                retained,
            )?;
        }
        Ok(StatusAction::Write(self.writer.submit(intent)?))
    }

    pub async fn open_target(&self, input: StatusInput) -> Result<StatusOpenTarget> {
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        let target = validate_input(&mut document, &input)?;
        let file_target = document
            .hunk
            .get(&target)
            .map_or(&target, |hunk| &hunk.file);
        let file = document
            .file
            .get(file_target)
            .context("status target has no source file")?;
        let coordinate = match &input.location {
            StatusLocation::Body {
                block, position, ..
            } => file
                .source_row
                .get(block)
                .and_then(|source| source.get(position.row)),
            _ => None,
        }
        .copied()
        .unwrap_or(forge_diff::source::SourceCoordinate {
            side: forge_diff::source::SourceSide::New,
            line: 0,
            byte_column: 0,
        });
        let root = document
            .repository
            .identity
            .worktree_root
            .as_ref()
            .context("source target requires a worktree")?;
        let path = forge_git::validate_path(root, &file.record.path)?;
        let historical = document
            .comparison
            .as_ref()
            .is_some_and(|comparison| !comparison.worktree);
        let old = coordinate.side == forge_diff::source::SourceSide::Old
            || (!historical && !path.try_exists()?);
        let (kind, path, workspace, source_revision) = if historical || old {
            let object = match &file.record.state {
                forge_git::snapshot::PathState::Tracked { head, index, .. } => {
                    if file.section == StatusSection::Staged {
                        if old || index.mode == 0 { head } else { index }
                    } else {
                        index
                    }
                }
                _ => anyhow::bail!("source target has no captured Git blob"),
            };
            ensure!(
                object.mode != 0 && !object.object.is_null(),
                "captured source blob is unavailable"
            );
            (
                "open_source",
                STANDARD.encode(file.record.path.raw()),
                Some(
                    root.to_str()
                        .context("source workspace is not UTF-8")?
                        .replace('\\', "/"),
                ),
                Some(format!("object:{}", object.object)),
            )
        } else {
            (
                "open_file",
                path.to_str()
                    .context("source path is not UTF-8")?
                    .replace('\\', "/"),
                None,
                None,
            )
        };
        let row = coordinate.line;
        let column = input.position().column;
        Ok(StatusOpenTarget {
            input,
            id: document.next_id("open")?,
            kind,
            path,
            row,
            column,
            workspace,
            source_revision,
        })
    }

    pub fn close(&self, id: &DocumentId) -> bool {
        let admitted = self.admitted.cancel(id);
        self.document
            .lock()
            .expect("status document lock")
            .remove(id)
            .is_some()
            || admitted
    }

    pub async fn close_collected(&self, id: &DocumentId) -> bool {
        let closed = self.close(id);
        self.admitted.wait_closed(id).await;
        closed
    }

    pub fn close_all(&self) {
        self.closed.store(true, Ordering::Release);
        self.admitted.cancel_all();
        self.document.lock().expect("status document lock").clear();
    }

    pub async fn close_view(
        &self,
        id: &DocumentId,
        view: &forge_buffer::identity::ViewId,
    ) -> Result<Option<StatusDelta>> {
        let owner = self.get(id)?;
        let mut document = owner.lock().await;
        document.input.remove(view);
        Ok(None)
    }

    pub async fn body_snapshot(
        &self,
        id: &DocumentId,
        file: u64,
        generation: u64,
    ) -> Result<BodyDelivery> {
        let owner = self.get(id)?;
        let document = owner.lock().await;
        document._admission.check()?;
        let file = document
            .file
            .get(&file_target(file))
            .context("status body file disappeared")?;
        ensure!(
            file.generation == generation,
            "status body generation changed"
        );
        Ok(BodyDelivery {
            document: id.clone(),
            file: file.id,
            generation,
            patch: None,
            snapshot: file.body.as_ref().map(|body| body.snapshot()),
            state: file.state.clone(),
            more: matches!(file.state, BodyState::Partial),
            syntax_diagnostic: None,
        })
    }

    pub(crate) fn get(&self, id: &DocumentId) -> Result<Arc<tokio::sync::Mutex<StatusDocument>>> {
        self.document
            .lock()
            .expect("status document lock")
            .get(id)
            .cloned()
            .context("unknown status document")
    }

    pub(crate) fn deliver(
        &self,
        document: &mut StatusDocument,
        target: &TargetId,
        file: &mut FileTarget,
    ) -> Result<BodyDelivery> {
        document._admission.check()?;
        file.body_mut()?;
        if matches!(
            file.state,
            BodyState::Ready | BodyState::Unavailable(_) | BodyState::Failed(_)
        ) {
            let patch = if !file.notice_delivered {
                match &file.state {
                    BodyState::Unavailable(diagnostic) | BodyState::Failed(diagnostic) => {
                        let diagnostic: String = diagnostic.chars().take(512).collect();
                        let block = label(
                            BlockId(document.next_id("notice")?),
                            &diagnostic.replace(['\n', '\r'], " "),
                            "Comment",
                            Some(target.clone()),
                        )?;
                        let body = file.body_mut()?;
                        let after = body.block_count();
                        let patch = body.edit(after..after, vec![block])?;
                        file.notice_delivered = true;
                        patch
                    }
                    _ => None,
                }
            } else {
                None
            };
            return Ok(body_delivery(document, file, patch));
        }
        if document.retained_bytes.saturating_add(1536 * 1024) > DOCUMENT_BYTES {
            file.state =
                BodyState::Unavailable("status document has no remaining body admission".into());
            return self.deliver(document, target, file);
        }
        let chunk = file
            .cursor
            .as_mut()
            .context("status body cursor missing")?
            .next_rows(ChunkLimits {
                rows: 128,
                ..ChunkLimits::default()
            });
        let mut rows = Vec::with_capacity(chunk.rows.len());
        let mut source_rows = Vec::with_capacity(chunk.rows.len());
        let mut metadata = BlockMetadata::default();
        for (row, display) in chunk.rows.iter().enumerate() {
            let range = TextRange {
                start: TextPosition { row, column: 0 },
                end: TextPosition {
                    row: row + 1,
                    column: 0,
                },
            };
            let row_target = if let Some(raw) = display.raw_id {
                let id = if let Some(id) = file.hunk.get(&raw) {
                    id.clone()
                } else {
                    let id = TargetId(document.next_id("hunk")?);
                    file.hunk.insert(raw, id.clone());
                    document.hunk.insert(
                        id.clone(),
                        HunkTarget {
                            file: target.clone(),
                            raw: vec![raw],
                        },
                    );
                    id
                };
                id
            } else {
                target.clone()
            };
            metadata.target.push(TargetRange {
                id: row_target,
                range: range.clone(),
            });
            if let Err(failure) = forge_diff::projection::append_display_row(
                &mut metadata,
                row,
                display,
                file.cursor
                    .as_ref()
                    .and_then(|cursor| cursor.group(display.group_index))
                    .context("diff display group is missing")?,
                file.old_syntax.as_ref(),
                file.new_syntax.as_ref(),
            ) {
                file.state = BodyState::Unavailable(failure.to_string());
                return self.deliver(document, target, file);
            }
            rows.push(display.text.clone());
            source_rows.push(
                display
                    .new
                    .or(display.old)
                    .context("diff row has no source coordinate")?,
            );
        }
        file.state = match &chunk.state {
            DisplayState::More => BodyState::Partial,
            DisplayState::Complete => BodyState::Ready,
            DisplayState::Unavailable(reason) => BodyState::Unavailable(format!("{reason:?}")),
        };
        let retained = chunk.text_bytes
            + (metadata.decoration.len() + metadata.visible_decoration.len()) * 160
            + metadata.target.len() * 160
            + metadata.gutter.len() * 128
            + source_rows.capacity() * std::mem::size_of::<forge_diff::source::SourceCoordinate>();
        ensure!(
            document.retained_bytes.saturating_add(retained) <= DOCUMENT_BYTES,
            "status body exceeds 16 MiB document budget"
        );
        let block = BufferBlock {
            id: BlockId(document.next_id("body")?),
            text: BufferText::from_rows(rows)?,
            metadata,
        };
        let projection = crate::hunks::project(
            document,
            file,
            target,
            block,
            source_rows,
            &chunk
                .rows
                .iter()
                .map(|row| row.group_index)
                .collect::<Vec<_>>(),
            retained,
        )?;
        let retained = retained + projection.retained;
        let patch = if projection.block.is_empty() {
            None
        } else {
            let body = file.body_mut()?;
            let after = body.block_count();
            let mut edits = projection.edit;
            edits.push(SequenceEdit {
                range: after..after,
                block: projection.block,
            });
            let patch = body.edit_many(edits)?;
            file.source_row.extend(projection.source);
            document.retained_bytes += retained;
            file.retained_bytes += retained;
            patch
        };
        Ok(body_delivery(document, file, patch))
    }

    async fn load(
        &self,
        repository: &Arc<forge_git::repository::RepositoryState>,
        record: &forge_git::snapshot::PathRecord,
        section: StatusSection,
        prepared: Option<&forge_git::snapshot::PreparedAnalysis>,
        projected: Option<&crate::mutation::ProjectedIndex>,
    ) -> Result<(
        forge_diff::cache::AnalysisHandle,
        DisplayCursor,
        Option<SyntaxHandle>,
        Option<SyntaxHandle>,
        Option<String>,
    )> {
        let generation = repository.generation();
        let (analysis, kind) = if let Some(projected) = projected {
            let (source, kind) = projected.sources(self, repository, section).await?;
            let analysis = self
                .diff
                .compare(DiffRequest {
                    source,
                    priority: WorkPriority::Visible,
                })
                .await
                .map_err(|error| anyhow::anyhow!("predicted diff unavailable: {error:?}"))?;
            (analysis, kind)
        } else {
            crate::source::analysis(
                &self.store,
                repository,
                record,
                section,
                prepared,
                &self.diff,
            )
            .await?
        };
        ensure!(
            projected.is_some() || generation == repository.generation(),
            "status body source generation changed"
        );
        let cursor = DisplayCursor::compact(analysis.clone(), 3, kind);
        Ok((analysis, cursor, None, None, None))
    }
}

pub(crate) async fn optional_syntax(
    engine: &Arc<SyntaxEngine>,
    request: SyntaxRequest,
) -> (Option<SyntaxHandle>, Option<String>) {
    match engine.analyze(request).await {
        Ok(syntax) => (Some(syntax), None),
        Err(error) => (
            None,
            Some(
                format!("Syntax highlighting unavailable: {error:?}")
                    .chars()
                    .take(256)
                    .collect(),
            ),
        ),
    }
}

fn body_delivery(
    document: &StatusDocument,
    file: &FileTarget,
    patch: Option<BufferPatch>,
) -> BodyDelivery {
    let first = patch.as_ref().is_some_and(|patch| patch.base.0 == 0);
    BodyDelivery {
        document: document.id.clone(),
        file: file.id,
        generation: file.generation,
        snapshot: first.then(|| file.body.as_ref().expect("delivered body").snapshot()),
        patch: if first { None } else { patch },
        state: file.state.clone(),
        more: matches!(file.state, BodyState::Partial),
        syntax_diagnostic: None,
    }
}

fn validate_input(document: &mut StatusDocument, input: &StatusInput) -> Result<TargetId> {
    let target = resolve_input(document, input)?;
    document.input.insert(input.view.clone(), input.sequence);
    Ok(target)
}

fn resolve_input(document: &StatusDocument, input: &StatusInput) -> Result<TargetId> {
    resolve_position(document, input)?.context("status input position has no target")
}

fn resolve_position(document: &StatusDocument, input: &StatusInput) -> Result<Option<TargetId>> {
    document._admission.check()?;
    input.validate()?;
    ensure!(
        input.revision == document.revision,
        "status input uses a stale revision"
    );
    ensure!(
        input.sequence.0 > 0
            && document
                .input
                .get(&input.view)
                .is_none_or(|sequence| *sequence < input.sequence),
        "status input sequence was already accepted"
    );
    ensure!(
        document.input.contains_key(&input.view) || document.input.len() < 64,
        "status view admission is full"
    );
    resolve_location(document, &input.location)
}

fn resolve_location(
    document: &StatusDocument,
    location: &StatusLocation,
) -> Result<Option<TargetId>> {
    match location {
        StatusLocation::Boundary { .. } => Ok(None),
        StatusLocation::File { id } => {
            let target = file_target(*id);
            ensure!(
                document.file.contains_key(&target),
                "status file disappeared"
            );
            Ok(Some(target))
        }
        StatusLocation::Section { section } => {
            ensure!(
                document.file.values().any(|file| file.section == *section
                    || (*section == StatusSection::Unstaged
                        && file.section == StatusSection::Untracked)),
                "status section disappeared"
            );
            Ok(Some(TargetId(format!("section:{section:?}"))))
        }
        StatusLocation::Context { role } => {
            ensure!(role.len() <= 128, "invalid context role");
            let target = TargetId(format!("status:context:{role}"));
            ensure!(
                document
                    .context
                    .as_ref()
                    .is_some_and(|context| context.action(&target).is_some()),
                "context target disappeared"
            );
            Ok(Some(target))
        }
        StatusLocation::Body {
            file,
            generation,
            revision,
            block,
            position,
            target,
        } => {
            let file = document
                .file
                .get(&file_target(*file))
                .context("status body file disappeared")?;
            ensure!(
                file.generation == *generation,
                "status body generation changed"
            );
            let body = file.body.as_ref().context("status body is not loaded")?;
            ensure!(body.revision() == *revision, "status body revision changed");
            let block = body.block(block).context("status body block disappeared")?;
            TextRange {
                start: *position,
                end: *position,
            }
            .validate(&block.text)?;
            let resolved = block.target_at(*position);
            ensure!(
                target
                    .as_ref()
                    .is_none_or(|target| Some(target) == resolved),
                "status target does not match position"
            );
            Ok(resolved.cloned())
        }
    }
}

fn section_target(target: &TargetId) -> Option<StatusSection> {
    [
        StatusSection::Staged,
        StatusSection::Unstaged,
        StatusSection::Untracked,
        StatusSection::Conflicted,
        StatusSection::Ignored,
    ]
    .into_iter()
    .find(|section| target.0 == format!("section:{section:?}"))
}

fn validate_action(section: StatusSection, action: &str) -> Result<()> {
    match action {
        "stage" => ensure!(section != StatusSection::Staged, "target is already staged"),
        "unstage" => ensure!(section == StatusSection::Staged, "target is not staged"),
        "discard" => {}
        _ => anyhow::bail!("unknown status action"),
    }
    Ok(())
}

fn whole_action(
    section: StatusSection,
    path: Vec<forge_git::RepositoryPath>,
    action: &str,
) -> Result<GitWriteAction> {
    validate_action(section, action)?;
    Ok(match action {
        "stage" => GitWriteAction::Stage { path },
        "unstage" => GitWriteAction::Unstage { path },
        "discard" => GitWriteAction::Discard {
            path,
            source: if section == StatusSection::Staged {
                DiscardSource::Head
            } else {
                DiscardSource::Index
            },
        },
        _ => unreachable!("validated status action"),
    })
}

pub(crate) fn syntax_decorations(
    syntax: &SyntaxHandle,
    source_row: usize,
    row: usize,
    text: &str,
    metadata: &mut BlockMetadata,
) -> bool {
    forge_diff::projection::append_syntax_row(metadata, syntax, source_row, row, text).is_ok()
}

#[derive(Serialize)]
pub struct NavigationDelivery {
    pub effect: Option<NavigationEffect>,
    pub body: Option<BodyDelivery>,
    pub more: bool,
}

#[derive(Serialize)]
pub struct NavigationEffect {
    pub document: DocumentId,
    pub revision: forge_buffer::identity::DocumentRevision,
    pub view: forge_buffer::identity::ViewId,
    pub sequence: forge_buffer::identity::InputSequence,
    pub id: String,
    pub kind: &'static str,
    pub location: StatusLocation,
}

fn selected_files(
    document: &StatusDocument,
    _input: &StatusInput,
    selection: Option<&StatusSelection>,
    target: &TargetId,
) -> Result<HashMap<TargetId, SelectedFile>> {
    let mut selected = HashMap::new();
    if let Some(selection) = selection {
        ensure!(
            !selection.target.is_empty() && selection.target.len() <= 65536,
            "invalid status selection size"
        );
        for location in &selection.target {
            let target =
                resolve_location(document, location)?.context("status selection has no target")?;
            select_target(document, &target, &mut selected);
        }
    } else {
        select_target(document, target, &mut selected);
    }
    Ok(selected)
}

fn select_target(
    document: &StatusDocument,
    target: &TargetId,
    selected: &mut HashMap<TargetId, SelectedFile>,
) {
    if let Some(section) = section_target(target) {
        for (id, file) in &document.file {
            if file.section == section
                || (section == StatusSection::Unstaged && file.section == StatusSection::Untracked)
            {
                selected.entry(id.clone()).or_default().whole = true;
            }
        }
    } else if document.file.contains_key(target) {
        selected.entry(target.clone()).or_default().whole = true;
    } else if let Some(hunk) = document.hunk.get(target) {
        selected
            .entry(hunk.file.clone())
            .or_default()
            .hunk
            .extend(hunk.raw.iter().copied());
    }
}

pub enum StatusAction {
    Accepted(crate::StatusActionAccepted, crate::StatusOperationTicket),
    Write(GitWriteTicket),
    Projection(Option<StatusDelta>),
}

fn combine_discard(action: Vec<GitWriteAction>) -> Result<Vec<GitWriteAction>> {
    let mut result: Vec<GitWriteAction> = Vec::new();
    for next in action {
        let path = match &next {
            GitWriteAction::Discard { path, .. } => &path[0],
            GitWriteAction::Patch { target, .. } => &target.path,
            _ => anyhow::bail!("invalid discard action"),
        };
        let existing = result
            .last()
            .filter(|action| match action {
                GitWriteAction::Discard { path: previous, .. } => previous.first() == Some(path),
                GitWriteAction::Patch { target, .. } => target.path == *path,
                _ => false,
            })
            .map(|_| result.len() - 1);
        let Some(index) = existing else {
            result.push(next);
            continue;
        };
        let previous = result.remove(index);
        let combined = match (previous, next) {
            (
                GitWriteAction::Discard { mut path, .. },
                GitWriteAction::Discard { path: other, .. },
            ) => {
                path.extend(other);
                path.sort_by(|left, right| left.raw().cmp(right.raw()));
                path.dedup();
                GitWriteAction::Discard {
                    path,
                    source: DiscardSource::Head,
                }
            }
            (
                GitWriteAction::Patch {
                    direction: PatchDirection::DiscardStaged,
                    target: staged,
                },
                GitWriteAction::Patch {
                    direction: PatchDirection::Discard,
                    target: unstaged,
                },
            )
            | (
                GitWriteAction::Patch {
                    direction: PatchDirection::Discard,
                    target: unstaged,
                },
                GitWriteAction::Patch {
                    direction: PatchDirection::DiscardStaged,
                    target: staged,
                },
            ) => GitWriteAction::DiscardCombined { staged, unstaged },
            _ => anyhow::bail!("incompatible discard selections for one path"),
        };
        result.insert(index, combined);
    }
    Ok(result)
}
