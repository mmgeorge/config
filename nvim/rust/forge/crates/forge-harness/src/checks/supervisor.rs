use super::model::{CheckCommand, CheckEventKind, CheckRunContext, PreparedCheckRun};
use super::publication::{CheckNotification, CheckPublication};
use super::{CheckRun, CheckSelection, CheckState, CommandCheck, ConfigSnapshot, RunRequest};
use crate::{
    broker::publication::{SessionUpdate, SessionUpdateSink},
    buffer::session::SessionPresentation,
};
use anyhow::{Context, Result, ensure};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

const CHECK_HISTORY_VERSION: u32 = crate::storage::state::VERSION;

/// Binds check history to one session, workspace, and storage location.
pub struct CheckSupervisorContext {
    pub session_id: String,
    pub data_root: PathBuf,
    pub workspace: PathBuf,
    pub root: PathBuf,
}

#[derive(Clone, Copy)]
struct CheckExecutionContext<'target> {
    sink: Option<&'target SessionUpdateSink>,
    publication: &'target CheckPublication,
    deadline: Option<CheckDeadline>,
    planned_inputs: Option<&'target super::discovery::DiscoveryInputs>,
    process_owner: &'target Arc<dyn forge_shell::process::ProcessOwner>,
}

/// Bound a planned run from admission through its final source validation.
#[derive(Clone, Copy)]
pub(crate) struct CheckDeadline {
    at: tokio::time::Instant,
    timeout: std::time::Duration,
}

impl CheckDeadline {
    /// Create an absolute budget that subprocesses cannot renew.
    fn new(timeout: std::time::Duration) -> Self {
        Self {
            at: tokio::time::Instant::now() + timeout,
            timeout,
        }
    }

    /// Get the shared expiration for discovery and adapter processes.
    pub(super) fn instant(self) -> tokio::time::Instant {
        self.at
    }

    /// Reject further work or successful completion after the captured budget expires.
    pub(crate) fn check(self) -> Result<()> {
        if tokio::time::Instant::now() >= self.at {
            return Err(super::failure::CheckFailure::classified(
                CheckState::Failed,
                CommandTimeout(self.timeout),
            ));
        }
        Ok(())
    }
}

/// Owns project check admission, durable run history, and process cancellation for one session.
pub struct CheckSupervisor {
    pub(crate) session_id: String,
    pub(crate) data_root: PathBuf,
    pub(crate) workspace: PathBuf,
    pub(crate) root: PathBuf,
    pub(super) changes: super::changes::CheckChanges,
    presentation: Arc<Mutex<SessionPresentation>>,
    state: Mutex<CheckStateStore>,
    pub(super) verification: Mutex<super::verification::VerificationRequirements>,
    notification: OnceLock<CheckNotification>,
    owner: OnceLock<super::owner::CheckOwnerSender>,
    trace: Arc<crate::trace::TraceStore>,
    changed: tokio::sync::Notify,
}

/// Check state and its matching presentation revision captured under one lock.
pub(crate) struct CheckSnapshot {
    /// Complete retained run history in start order.
    pub history: Vec<CheckRun>,
    /// Run that still owns process execution.
    pub active_run: Option<String>,
    /// Presentation revision corresponding to this check state.
    pub revision: u64,
}

struct CheckStateStore {
    records: BTreeMap<String, CheckRun>,
    active: Option<ActiveCheck>,
    discovery: Option<String>,
    task: BTreeMap<String, tokio::task::JoinHandle<()>>,
    notified: std::collections::BTreeSet<String>,
}

struct ActiveCheck {
    run_id: String,
    cancellation: Arc<CheckCancellation>,
}

/// Release exclusive discovery admission only after its worker has collected every process.
pub(super) struct DiscoveryReservation {
    supervisor: Arc<CheckSupervisor>,
    id: String,
    _workspace: Option<super::reservation::CheckReservation>,
}

impl Drop for DiscoveryReservation {
    fn drop(&mut self) {
        if let Ok(mut state) = self.supervisor.state.lock()
            && state.discovery.as_ref() == Some(&self.id)
        {
            state.discovery = None;
        }
    }
}

impl DiscoveryReservation {
    /// Keep exact discovery's source reservation alive through native collection.
    pub(super) fn process_owner(&self) -> Option<Arc<dyn forge_shell::process::ProcessOwner>> {
        self._workspace
            .as_ref()
            .map(super::reservation::CheckReservation::process_owner)
    }
}

struct AdmittedCheckRun {
    run: PreparedCheckRun,
    cancellation: Arc<CheckCancellation>,
    deadline: Option<CheckDeadline>,
    planned_inputs: Option<super::discovery::DiscoveryInputs>,
    _parent_permit: Option<crate::command::ExecutionPermit>,
    _workspace: super::reservation::CheckReservation,
}

pub(super) struct CheckCancellation {
    requested: Arc<AtomicBool>,
    changed: tokio::sync::Notify,
}

impl CheckCancellation {
    fn cancel(&self) {
        self.requested.store(true, Ordering::Release);
        self.changed.notify_waiters();
    }

    pub(super) async fn wait(&self, external: Option<tokio::sync::watch::Receiver<bool>>) {
        let local = async {
            loop {
                let changed = self.changed.notified();
                tokio::pin!(changed);
                changed.as_mut().enable();
                if self.requested.load(Ordering::Acquire) {
                    return;
                }
                changed.await;
            }
        };
        let external = async {
            if let Some(mut cancelled) = external
                && cancelled.wait_for(|cancelled| *cancelled).await.is_ok()
            {
                return;
            }
            std::future::pending::<()>().await;
        };
        tokio::select! { _ = local => {}, _ = external => {} }
    }

    async fn read<Output>(
        &self,
        external: Option<&tokio::sync::watch::Receiver<bool>>,
        read: impl std::future::Future<Output = Result<Output>>,
    ) -> Result<Option<Output>> {
        tokio::select! {
            biased;
            _ = self.wait(external.cloned()) => Ok(None),
            result = read => result.map(Some),
        }
    }
}

struct CheckProcess {
    worker: tokio::task::JoinHandle<Result<(std::process::ExitStatus, bool)>>,
    stop: Option<tokio::sync::oneshot::Sender<()>>,
    collected: bool,
}

/// Stop a check run when its captured execution deadline expires.
#[derive(Debug)]
struct CommandTimeout(std::time::Duration);

impl std::fmt::Display for CommandTimeout {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "Check execution exceeded its {} second timeout; check execution stopped",
            self.0.as_secs()
        )
    }
}

impl std::error::Error for CommandTimeout {}

impl CheckProcess {
    async fn wait(&mut self) -> Result<(std::process::ExitStatus, bool)> {
        let result = (&mut self.worker)
            .await
            .context("check process supervisor failed");
        self.collected = true;
        result?
    }

    async fn stop(&mut self) -> Result<()> {
        if let Some(stop) = self.stop.take() {
            let _ = stop.send(());
        }
        if !self.collected {
            self.wait().await?;
        }
        Ok(())
    }
}

impl Drop for CheckProcess {
    fn drop(&mut self) {
        if let Some(stop) = self.stop.take() {
            let _ = stop.send(());
        }
    }
}

/// Captures task authority and concrete selectors before command admission.
pub(crate) struct CheckAdmission {
    pub owner: crate::timeline::TimelineOwner,
    pub request: RunRequest,
    pub overrides: BTreeMap<String, bool>,
    /// Task settings before any reviewer exclusions are merged into enabled selections.
    pub task_overrides: super::CheckOverrides,
    /// Changed paths that select automatic checks and default path scopes.
    pub inventory: super::ChangeInventory,
    pub shell: crate::session::CheckShell,
    pub trigger: String,
    pub task_id: Option<String>,
    pub revision: u32,
}

impl CheckSupervisor {
    /// Keep completion scheduling active until existing mutation obligations can be inspected.
    pub(crate) fn has_verification_requirements(&self) -> Result<bool> {
        Ok(self
            .verification
            .lock()
            .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
            .has_requirements())
    }

    /// Check mandatory verification against the same source capture used by completion.
    pub(crate) fn verification_satisfied(
        &self,
        snapshot: &crate::checkpoint::CheckpointRecord,
    ) -> Result<bool> {
        Ok(self.pending_verification(snapshot)?.is_empty())
    }

    /// Get each check whose whole-check verification the source capture still needs, once.
    ///
    /// A definition comes from the mutation that created the obligation, so its name and ID
    /// remain available when the current configuration changed or removed it.
    pub(crate) fn pending_verification(
        &self,
        snapshot: &crate::checkpoint::CheckpointRecord,
    ) -> Result<Vec<super::CheckDefinition>> {
        if !self.has_verification_requirements()? {
            return Ok(Vec::new());
        }
        let config = ConfigSnapshot::load(&self.workspace)?;
        let pending = self
            .verification
            .lock()
            .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
            .pending_for_config(snapshot, &config)?;
        let mut definitions: Vec<super::CheckDefinition> = Vec::new();
        for requirement in pending {
            if !definitions
                .iter()
                .any(|definition| definition.id == requirement.check)
            {
                definitions.push(requirement.definition);
            }
        }
        Ok(definitions)
    }

    /// Recheck mutation obligations against fresh source after a completion check run.
    pub(crate) async fn completion_verification_satisfied(&self) -> Result<bool> {
        if !self.has_verification_requirements()? {
            return Ok(true);
        }
        let snapshot = self.changes.capture(&self.session_id).await?;
        self.verification_satisfied(&snapshot)
    }

    pub(crate) fn session_owner(&self) -> crate::timeline::TimelineOwner {
        crate::timeline::TimelineOwner::session(&self.session_id)
    }

    pub(crate) fn exchange_owner(
        &self,
        exchange_id: &str,
    ) -> Result<crate::timeline::TimelineOwner> {
        crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?
            .capture_timeline_owner(&self.session_id, exchange_id)
    }

    fn validate_owner(&self, run: &CheckRun) -> Result<()> {
        ensure!(
            run.owner.session_id() == self.session_id,
            "check ownership crosses sessions"
        );
        crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?
            .validate_timeline_owner(&run.owner)
    }

    /// Reopens current-format history and marks detached execution interrupted without replaying commands.
    pub fn open(
        scope: CheckSupervisorContext,
        presentation: Arc<Mutex<SessionPresentation>>,
        objects: crate::storage::objects::ObjectStore,
        repositories: Arc<forge_git::store::RepositoryStore>,
        trace: Arc<crate::trace::TraceStore>,
    ) -> Result<Arc<Self>> {
        let CheckSupervisorContext {
            session_id,
            data_root,
            workspace,
            root,
        } = scope;
        std::fs::create_dir_all(&root)?;
        let changes = super::changes::CheckChanges::open(
            &workspace,
            &data_root,
            &root,
            objects.clone(),
            repositories,
        )?;
        let verification = super::verification::VerificationRequirements::open(
            &root,
            &workspace,
            &session_id,
            objects,
        )?;
        let mut records = BTreeMap::new();
        for mut run in Self::saved_history(&root)? {
            ensure!(
                run.owner.session_id() == session_id,
                "saved check ownership crosses sessions"
            );
            let path = root.join(&run.id).join("record.json");
            if run.state == CheckState::Running {
                run.state = CheckState::Interrupted;
                for check in &mut run.commands {
                    if check.state == CheckState::Running {
                        check.state = CheckState::Interrupted;
                    }
                }
                run.record(
                    CheckEventKind::Interrupted,
                    None,
                    "Checks interrupted: previous process detached".into(),
                );
                run.completed_at_ms = Some(now_ms());
                write_record(&path, &run)?;
            }
            records.insert(run.id.clone(), run);
        }
        Ok(Arc::new(Self {
            session_id,
            data_root,
            workspace,
            root,
            changes,
            verification: Mutex::new(verification),
            presentation,
            state: Mutex::new(CheckStateStore {
                records,
                active: None,
                discovery: None,
                task: BTreeMap::new(),
                notified: Default::default(),
            }),
            notification: OnceLock::new(),
            owner: OnceLock::new(),
            trace,
            changed: Default::default(),
        }))
    }

    /// Captures the source boundary before provider edits can affect check applicability.
    pub(crate) fn begin_changes(
        &self,
        owner: String,
        checkpoint: &crate::checkpoint::CheckpointRecord,
    ) -> Result<()> {
        self.changes.begin(owner, checkpoint)
    }

    /// Updates applicability from current task changes without borrowing the active broker.
    pub(crate) async fn refresh_changes(&self) -> Result<()> {
        if self.active()?.is_none() {
            self.changes.refresh().await?;
        }
        Ok(())
    }

    /// Reads saved history for session previews without mutating another live runner's state.
    ///
    /// A record that cannot be read or decoded, such as one torn by a crash, is skipped and
    /// reported in the log. One damaged run must not block resume, preview, or fork of the whole
    /// session, and the skipped file is left in place for inspection.
    pub(crate) fn saved_history(root: &Path) -> Result<Vec<CheckRun>> {
        if !root.exists() {
            return Ok(Vec::new());
        }
        let mut records = Vec::new();
        for directory in std::fs::read_dir(root)? {
            let directory = directory?;
            if !directory.file_type()?.is_dir() {
                continue;
            }
            let path = directory.path().join("record.json");
            if !path.exists() {
                continue;
            }
            match saved_record(&path, &directory.file_name().to_string_lossy()) {
                Ok(Some(run)) => records.push(run),
                Ok(None) => {}
                Err(error) => tracing::warn!(
                    target: "forge::checks",
                    event_name = "checks.history_skipped",
                    path = %path.display(),
                    error = %format!("{error:#}"),
                    "skipping undecodable check history record"
                ),
            }
        }
        records.sort_by_key(|run| run.started_at_ms);
        Ok(records)
    }

    /// Lists persisted runs chronologically, including settled results from earlier hosts.
    pub fn history(&self) -> Result<Vec<CheckRun>> {
        let mut records: Vec<_> = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
            .records
            .values()
            .cloned()
            .collect();
        records.sort_by_key(|run| run.started_at_ms);
        Ok(records)
    }

    /// Returns the process-owning run without inferring activity from saved history.
    pub(crate) fn active(&self) -> Result<Option<String>> {
        Ok(self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
            .active
            .as_ref()
            .map(|active| active.run_id.clone()))
    }

    /// Reserve discovery atomically against both other discoveries and check runs.
    pub(super) fn discovery_reserve(
        self: &Arc<Self>,
        id: &str,
        mode: super::adapter::DiscoverMode,
    ) -> Result<DiscoveryReservation> {
        let workspace = match mode {
            super::adapter::DiscoverMode::Static => None,
            super::adapter::DiscoverMode::Exact => {
                Some(super::reservation::CheckReservation::acquire(
                    &self.data_root,
                    &self.workspace,
                    super::CheckPurpose::Verify,
                )?)
            }
        };
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        ensure!(
            state.active.is_none() && state.discovery.is_none(),
            "another check operation is already active"
        );
        state.discovery = Some(id.to_owned());
        Ok(DiscoveryReservation {
            supervisor: Arc::clone(self),
            id: id.to_owned(),
            _workspace: workspace,
        })
    }

    /// Reads admission and its current record under one state lock.
    pub(crate) fn snapshot(&self) -> Result<CheckSnapshot> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        let mut history: Vec<_> = state.records.values().cloned().collect();
        history.sort_by_key(|run| run.started_at_ms);
        let revision = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("check presentation lock poisoned"))?
            .revision();
        Ok(CheckSnapshot {
            history,
            active_run: state.active.as_ref().map(|active| active.run_id.clone()),
            revision,
        })
    }

    /// Reads one owned run, waiting for an update or completion within one fixed deadline.
    pub async fn status(
        &self,
        id: &str,
        wait_ms: u64,
        wait_for_completion: bool,
    ) -> Result<CheckRun> {
        let read = || {
            self.state
                .lock()
                .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
                .records
                .get(id)
                .cloned()
                .context("unknown check run")
        };
        let maximum_wait = if wait_for_completion { 60000 } else { 30000 };
        let deadline = tokio::time::Instant::now()
            + std::time::Duration::from_millis(wait_ms.min(maximum_wait));
        loop {
            let waiting = self.changed.notified();
            tokio::pin!(waiting);
            waiting.as_mut().enable();
            let run = read()?;
            if run.state != CheckState::Running || wait_ms == 0 {
                return Ok(run);
            }
            if tokio::time::timeout_at(deadline, waiting).await.is_err() || !wait_for_completion {
                return read();
            }
        }
    }

    pub(crate) fn running_summaries(&self) -> Result<serde_json::Value> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        let runs: Vec<_> = state
            .records
            .values()
            .filter(|run| run.state == CheckState::Running)
            .map(super::response::summary)
            .collect();
        Ok(serde_json::json!({"runs":runs}))
    }

    pub(crate) async fn status_summary(&self, id: &str, wait_ms: u64) -> Result<serde_json::Value> {
        let deadline =
            tokio::time::Instant::now() + std::time::Duration::from_millis(wait_ms.min(60_000));
        loop {
            let waiting = self.changed.notified();
            tokio::pin!(waiting);
            waiting.as_mut().enable();
            let (summary, running) = {
                let state = self
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
                let run = state.records.get(id).context("unknown check run")?;
                (
                    super::response::summary(run),
                    run.state == CheckState::Running,
                )
            };
            if !running || wait_ms == 0 {
                return Ok(summary);
            }
            if tokio::time::timeout_at(deadline, waiting).await.is_err() {
                return self.status_summary_now(id);
            }
        }
    }

    fn status_summary_now(&self, id: &str) -> Result<serde_json::Value> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        let run = state.records.get(id).context("unknown check run")?;
        Ok(super::response::summary(run))
    }

    /// Requests cancellation without releasing process ownership before collection finishes.
    pub fn cancel(&self, id: &str) -> Result<()> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        let active = state.active.as_ref().context("no check run is active")?;
        ensure!(
            active.run_id == id,
            "check run is not owned by this active request"
        );
        active.cancellation.cancel();
        Ok(())
    }

    /// Request collection of the selected run, accepting natural completion under the same lock.
    /// A different active run never inherits cancellation intended for this identity.
    pub(crate) fn cancel_for_collection(&self, id: &str) -> Result<()> {
        let state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        let run = state.records.get(id).context("unknown check run")?;
        if run.state != CheckState::Running {
            return Ok(());
        }
        let active = state
            .active
            .as_ref()
            .context("running check lost process ownership")?;
        ensure!(
            active.run_id == id,
            "running check ownership changed before collection"
        );
        active.cancellation.cancel();
        Ok(())
    }

    /// Builds exact commands from saved definitions and task selectors before acquiring execution ownership.
    pub(crate) fn prepare(
        &self,
        snapshot: ConfigSnapshot,
        admission: CheckAdmission,
    ) -> Result<PreparedCheckRun> {
        let CheckAdmission {
            owner,
            request,
            overrides,
            task_overrides,
            inventory,
            shell,
            trigger,
            task_id,
            revision,
        } = admission;
        ensure!(
            owner.session_id() == self.session_id,
            "check ownership crosses sessions"
        );
        let mut effective = snapshot.config.clone();
        if request.purpose == super::CheckPurpose::Prepare {
            ensure!(
                !request.ids.is_empty(),
                "apply requires explicitly named checks"
            );
            ensure!(
                request.include.is_empty(),
                "apply requires explicitly named checks without automatic inclusions"
            );
            for id in &request.ids {
                let definition = effective
                    .definitions
                    .iter()
                    .find(|definition| &definition.id == id)
                    .with_context(|| format!("Unknown check: {id}"))?;
                ensure!(
                    matches!(
                        definition.category,
                        super::CheckCategory::Format | super::CheckCategory::Lint
                    ) && matches!(definition.runner, super::CheckRunner::Adapter(_)),
                    "{id} cannot apply fixes; only format and lint adapters support apply"
                );
            }
        }
        for definition in &mut effective.definitions {
            if let Some(enabled) = overrides.get(&definition.id) {
                definition.enabled = *enabled;
            }
        }
        let id = uuid::Uuid::new_v4().to_string();
        let directory = self.root.join(&id);
        let mut run = CheckRun {
            id: id.clone(),
            owner: Box::new(owner),
            revision,
            context: CheckRunContext {
                automatic: false,
                trigger,
                task_id,
                snapshot,
                overrides,
                task_overrides,
                changed_files: inventory.files().to_vec(),
            },
            lifecycle: Vec::new(),
            shell,
            executable: shell.executable().ok(),
            workspace: self.workspace.to_string_lossy().into_owned(),
            environment_digest: crate::plan::digest(&serde_json::to_vec(
                &std::env::vars_os()
                    .map(|(key, value)| {
                        (
                            key.to_string_lossy().into_owned(),
                            value.to_string_lossy().into_owned(),
                        )
                    })
                    .collect::<BTreeMap<_, _>>(),
            )?),
            started_at_ms: now_ms(),
            completed_at_ms: None,
            cancelled_inputs: None,
            cancelled_run_id: None,
            state: CheckState::Running,
            progress: Default::default(),
            planned_tests: None,
            workspace_identity: None,
            workspace_digest: BTreeMap::new(),
            commands: Vec::new(),
        };
        let applicability = super::applicability::resolve(&request, &effective, &inventory)?;
        let requested: std::collections::BTreeSet<_> = applicability
            .iter()
            .filter(|item| item.selected)
            .map(|item| item.id.clone())
            .collect();
        let effective_request = RunRequest::Selected(super::SelectedChecks {
            ids: requested.iter().cloned().collect(),
            ..(*request).clone()
        });
        let selections = if requested.is_empty() {
            BTreeMap::new()
        } else if request.is_all() {
            requested
                .iter()
                .map(|id| (id.clone(), CheckSelection::All))
                .collect()
        } else {
            effective_request.selections(&effective, &inventory)?
        };
        let purposes: BTreeMap<_, _> = effective
            .definitions
            .iter()
            .map(|definition| {
                let purpose = if request.purpose == super::CheckPurpose::Prepare
                    && request.ids.contains(&definition.id)
                {
                    super::CheckPurpose::Prepare
                } else {
                    definition
                        .runner
                        .command()
                        .map_or(super::CheckPurpose::Verify, super::CheckOperation::purpose)
                };
                (definition.id.clone(), purpose)
            })
            .collect();
        let preparing = requested
            .iter()
            .filter(|id| purposes[*id] == super::CheckPurpose::Prepare)
            .cloned()
            .collect();
        for index in effective.order_preparing(&preparing)? {
            let definition = &effective.definitions[index];
            let applicability = &applicability[index];
            ensure!(
                definition.enabled
                    || request.ids.is_empty()
                    || (!request.ids.contains(&definition.id)
                        && !request.include.contains(&definition.id)),
                "{} is disabled; re-enable it in /checks",
                definition.id
            );
            let selection = selections.get(&definition.id).cloned().unwrap_or_default();
            let empty_default = !request.is_all()
                && !request.selections.contains_key(&definition.id)
                && selection.default_empty();
            let operation = definition.runner.command();
            let (state, error) = if !definition.enabled {
                (CheckState::Skipped, Some("Disabled for this task".into()))
            } else if !applicability.selected {
                (CheckState::Skipped, Some(applicability.reason.clone()))
            } else if selection.empty() && request.selections.contains_key(&definition.id) {
                (
                    CheckState::ConfigurationError,
                    Some(
                        "Explicit check selection is empty. Supply concrete declared inputs."
                            .into(),
                    ),
                )
            } else if empty_default && inventory == super::ChangeInventory::Unavailable {
                (CheckState::ConfigurationError, Some("Task change inventory is unavailable. Supply declared inputs or explicitly select full context.".into()))
            } else if inventory == super::ChangeInventory::Pending {
                // The initial scan replaces this attempt before any command starts.
                (CheckState::Pending, None)
            } else if empty_default {
                (CheckState::Skipped, Some("No matching task changes".into()))
            } else if operation.is_none()
                && matches!(definition.runner, super::CheckRunner::Command(_))
            {
                (
                    CheckState::ConfigurationError,
                    Some("Requested scope is unsupported. Enable /checks edit on and request a correction in chat.".into()),
                )
            } else {
                (CheckState::Pending, None)
            };
            let position = run.commands.len();
            run.commands.push(CommandCheck {
                selected: applicability.selected,
                evidence: None,
                reused_from: None,
                id: format!("{id}:{position}"),
                command: definition.runner.display(),
                context: CheckCommand {
                    purpose: purposes[&definition.id],
                    preparation: None,
                    operation: operation.cloned(),
                    invocation: None,
                    definition_id: definition.id.clone(),
                    name: definition.name.clone(),
                    area: definition.area.clone(),
                    reason: applicability.reason.clone(),
                    matched_files: applicability.matched_files.clone(),
                    category: definition.category,
                    directory: definition.directory.clone(),
                    selection,
                    resolved: None,
                    depends_on: definition.depends_on.clone(),
                },
                state,
                started_at_ms: None,
                completed_at_ms: None,
                exit_code: None,
                error,
                stdout: directory.join(format!("{position}.stdout")),
                stderr: directory.join(format!("{position}.stderr")),
                output: directory.join(format!("{position}.output")),
                output_bytes: 0,
                preview: String::new(),
                result: None,
                adapter_progress: None,
            });
        }
        run.record(
            CheckEventKind::Started,
            None,
            format!("Checks started · {} selected", requested.len()),
        );
        if request.purpose == super::CheckPurpose::Prepare {
            ensure!(
                run.commands.iter().all(|command| !command.selected
                    || request.ids.contains(&command.context.definition_id)
                    || command.context.purpose == super::CheckPurpose::Verify),
                "apply cannot launch a source-changing prerequisite"
            );
            for command in &run.commands {
                ensure!(
                    !command.selected || command.state != CheckState::ConfigurationError,
                    "{}: {}",
                    command.context.definition_id,
                    command
                        .error
                        .as_deref()
                        .unwrap_or("invalid apply selection")
                );
            }
        }
        Ok(run)
    }

    /// Preserve an admitted plan attempt's identity while resolving its scanned selection.
    pub(crate) fn continue_attempt(
        &self,
        previous: &CheckRun,
        mut prepared: PreparedCheckRun,
    ) -> Result<PreparedCheckRun> {
        let context = previous
            .context
            .as_ref()
            .context("admitted check context is missing")?;
        ensure!(
            previous.state == CheckState::Running
                && previous.planned_tests.is_some()
                && previous
                    .commands
                    .iter()
                    .all(|command| command.started_at_ms.is_none())
                && previous.owner == prepared.owner
                && previous.revision == prepared.revision
                && context.task_id == prepared.context.task_id
                && context.snapshot == prepared.context.snapshot
                && context.overrides == prepared.context.overrides
                && context.task_overrides == prepared.context.task_overrides
                && previous.workspace == prepared.workspace
                && previous.shell == prepared.shell,
            "initial scan cannot replace an admitted check attempt"
        );
        prepared.id = previous.id.clone();
        prepared.started_at_ms = previous.started_at_ms;
        prepared.lifecycle = previous.lifecycle.clone();
        let directory = self.root.join(&prepared.id);
        for (position, command) in prepared.commands.iter_mut().enumerate() {
            command.id = format!("{}:{position}", prepared.id);
            command.stdout = directory.join(format!("{position}.stdout"));
            command.stderr = directory.join(format!("{position}.stderr"));
            command.output = directory.join(format!("{position}.output"));
        }
        Ok(prepared)
    }

    /// Capture the configured budget before a plan's first scan begins.
    pub(crate) fn plan_deadline(&self) -> Result<CheckDeadline> {
        self.execution_timeout().map(CheckDeadline::new)
    }

    /// Freezes declared inputs before review, authorization, or process admission.
    pub(crate) async fn resolve(&self, mut run: PreparedCheckRun) -> Result<PreparedCheckRun> {
        if run.context.automatic && self.has_verification_requirements()? {
            let snapshot = self.changes.capture(&self.session_id).await?;
            super::discovery::DiscoveryInputs::for_run(self, &run)?;
            self.verification
                .lock()
                .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                .require_in_run(&mut run, &snapshot)?;
        }
        let mut commands = Vec::new();
        for mut check in std::mem::take(&mut run.commands) {
            let context = check.context.clone();
            if check.state != CheckState::Pending || context.invocation.is_some() {
                commands.push((check, context));
                continue;
            }
            let Some(operation) = &context.operation else {
                commands.push((check, context));
                continue;
            };
            match super::invocation::resolve(
                &self.workspace,
                &context.directory,
                operation,
                &context.selection,
                &context.matched_files,
            ) {
                Ok(invocations) => {
                    ensure!(
                        !invocations.is_empty(),
                        "nonempty check selection resolved to no commands"
                    );
                    for invocation in invocations {
                        let mut expanded = check.clone();
                        if operation.args.is_some() {
                            expanded.command = invocation.display();
                        }
                        let mut context = context.clone();
                        if !invocation.files.is_empty()
                            && matches!(context.selection, CheckSelection::Files { .. })
                        {
                            context.matched_files = invocation.files.clone();
                            context.reason = format!("Changed: {}", invocation.files.join(", "));
                        }
                        context.invocation = Some(invocation);
                        commands.push((expanded, context));
                    }
                }
                Err(error) => {
                    check.state = super::failure::classify(&error, CheckState::ConfigurationError);
                    check.error = Some(format!("{error:#}"));
                    commands.push((check, context));
                }
            }
        }
        for (position, (check, _)) in commands.iter_mut().enumerate() {
            check.id = format!("{}:{position}", run.id);
            let directory = self.root.join(&run.id);
            check.stdout = directory.join(format!("{position}.stdout"));
            check.stderr = directory.join(format!("{position}.stderr"));
            check.output = directory.join(format!("{position}.output"));
        }
        run.commands = commands
            .into_iter()
            .map(|(mut check, context)| {
                check.context = context;
                check
            })
            .collect();
        Ok(run)
    }

    /// Runs the same serial dependency-aware lifecycle for automatic gates and on-demand requests.
    pub(crate) async fn run(
        self: &Arc<Self>,
        run: PreparedCheckRun,
        sink: Option<SessionUpdateSink>,
        external_cancelled: Option<tokio::sync::watch::Receiver<bool>>,
        deadline: Option<CheckDeadline>,
    ) -> Result<CheckRun> {
        let (_, completed) = self
            .launch(run, sink, external_cancelled, None, deadline)
            .await?;
        completed
            .await
            .context("check worker did not report completion")?
            .map_err(anyhow::Error::msg)
    }

    /// Finds current evidence in this task without consuming or mutating prior runs.
    pub(crate) fn evidence(
        &self,
        run: &CheckRun,
        check: &CommandCheck,
        checkpoint: &crate::checkpoint::CheckpointRecord,
    ) -> Result<Option<(String, CommandCheck)>> {
        let Some(task) = run
            .context
            .as_ref()
            .and_then(|context| context.task_id.as_deref())
        else {
            return Ok(None);
        };
        for previous in self.history()?.into_iter().rev() {
            if previous.id == run.id
                || previous
                    .context
                    .as_ref()
                    .and_then(|context| context.task_id.as_deref())
                    != Some(task)
            {
                continue;
            }
            for command in previous.commands.iter().rev() {
                if !matches!(command.state, CheckState::Passed | CheckState::Failed) {
                    continue;
                }
                if let Some(evidence) = &command.evidence
                    && evidence.covers(run, check, checkpoint)?
                {
                    return Ok(Some((previous.id.clone(), command.clone())));
                }
            }
        }
        Ok(None)
    }

    fn admit(
        &self,
        run: PreparedCheckRun,
        admission: Option<crate::command::ExecutionPermit>,
        deadline: Option<CheckDeadline>,
    ) -> Result<AdmittedCheckRun> {
        self.validate_owner(&run.clone().into_record())?;
        let deadline = match deadline {
            Some(deadline) => {
                ensure!(
                    run.planned_tests.is_some(),
                    "a captured deadline requires a planned check attempt"
                );
                deadline.check()?;
                Some(deadline)
            }
            None => run
                .planned_tests
                .as_ref()
                .map(|_| self.plan_deadline())
                .transpose()?,
        };
        let planned_inputs = run
            .planned_tests
            .as_ref()
            .map(|_| super::discovery::DiscoveryInputs::for_run(self, &run))
            .transpose()?;
        let purpose = if run.commands.iter().any(|command| {
            command.selected && command.context.purpose == super::CheckPurpose::Prepare
        }) {
            super::CheckPurpose::Prepare
        } else {
            super::CheckPurpose::Verify
        };
        let workspace = super::reservation::CheckReservation::acquire(
            &self.data_root,
            &self.workspace,
            purpose,
        )?;
        if purpose == super::CheckPurpose::Prepare {
            super::discovery::DiscoveryInputs::for_run(self, &run)?;
        }
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        ensure!(
            state.active.is_none() && state.discovery.is_none(),
            "another check operation is already active"
        );
        ensure!(
            !state.task.contains_key(&run.id),
            "previous check run is still saving"
        );
        let cancelled = Arc::new(CheckCancellation {
            requested: Arc::new(AtomicBool::new(false)),
            changed: Default::default(),
        });
        state
            .records
            .insert(run.id.clone(), run.clone().into_record());
        state.active = Some(ActiveCheck {
            run_id: run.id.clone(),
            cancellation: Arc::clone(&cancelled),
        });
        Ok(AdmittedCheckRun {
            run,
            cancellation: cancelled,
            deadline,
            planned_inputs,
            _parent_permit: admission,
            _workspace: workspace,
        })
    }

    fn cancelled_attempt(
        &self,
        run: &CheckRun,
        check: &CommandCheck,
        checkpoint: Option<&crate::checkpoint::CheckpointRecord>,
    ) -> Result<Option<String>> {
        let owner = run
            .context
            .as_ref()
            .and_then(|context| context.task_id.as_ref());
        for previous in self.history()?.into_iter().rev() {
            if previous.id == run.id
                || previous
                    .context
                    .as_ref()
                    .and_then(|context| context.task_id.as_ref())
                    != owner
            {
                continue;
            }
            for command in &previous.commands {
                if previous.state != CheckState::Interrupted
                    && previous
                        .context
                        .as_ref()
                        .is_some_and(|context| !context.automatic)
                    && matches!(command.state, CheckState::Passed | CheckState::Failed)
                    && command.context.as_ref().map(|context| &context.selection)
                        == check.context.as_ref().map(|context| &context.selection)
                    && super::evidence::signature(&previous, command)?
                        == super::evidence::signature(run, check)?
                {
                    return Ok(None);
                }
                if matches!(command.state, CheckState::Passed | CheckState::Failed)
                    && let Some(evidence) = &command.evidence
                    && let Some(checkpoint) = checkpoint
                    && evidence.matches_inputs(run, check, checkpoint)?
                {
                    return Ok(None);
                }
                if previous.state == CheckState::Interrupted && command.state != CheckState::Skipped
                {
                    if let (Some(cancelled), Some(checkpoint)) =
                        (&previous.cancelled_inputs, checkpoint)
                    {
                        if let Some(evidence) = super::evidence::CheckEvidence::capture(
                            self.changes.objects(),
                            &previous,
                            command,
                            cancelled.clone(),
                            cancelled,
                        )? && evidence.matches_inputs(run, check, checkpoint)?
                        {
                            return Ok(Some(previous.id.clone()));
                        }
                    } else if command.context.as_ref().map(|context| &context.selection)
                        == check.context.as_ref().map(|context| &context.selection)
                        && previous.workspace_identity == run.workspace_identity
                        && previous.workspace_digest == run.workspace_digest
                        && super::evidence::signature(&previous, command)?
                            == super::evidence::signature(run, check)?
                    {
                        return Ok(Some(previous.id.clone()));
                    }
                }
            }
        }
        Ok(None)
    }

    async fn run_admitted(
        self: &Arc<Self>,
        admitted: AdmittedCheckRun,
        sink: Option<SessionUpdateSink>,
        external_cancelled: Option<tokio::sync::watch::Receiver<bool>>,
    ) -> Result<CheckRun> {
        let AdmittedCheckRun {
            mut run,
            cancellation: cancelled,
            deadline,
            planned_inputs,
            _parent_permit,
            _workspace,
        } = admitted;
        let publication = CheckPublication::new(
            self.root.join(&run.id).join("record.json"),
            &run.clone().into_record(),
        );
        let process_owner = _workspace.process_owner();
        use futures_util::FutureExt;
        let result = std::panic::AssertUnwindSafe(self.run_inner(
            &mut run,
            CheckExecutionContext {
                sink: sink.as_ref(),
                publication: &publication,
                deadline,
                planned_inputs: planned_inputs.as_ref(),
                process_owner: &process_owner,
            },
            &cancelled,
            external_cancelled.as_ref(),
        ))
        .catch_unwind()
        .await
        .unwrap_or_else(|payload| {
            let cause = payload
                .downcast_ref::<String>()
                .map(String::as_str)
                .or_else(|| payload.downcast_ref::<&str>().copied())
                .unwrap_or("non-text panic payload");
            Err(anyhow::anyhow!("check worker panicked: {cause}"))
        });
        if let Some(planned) = &mut run.planned_tests {
            planned.record_results(&run.commands);
        }
        if run.commands.iter().any(|command| {
            command.selected && command.context.purpose == super::CheckPurpose::Prepare
        }) {
            for command in &mut run.commands {
                if command.state == CheckState::Pending {
                    command.state = CheckState::Skipped;
                    command.error = Some(match &result {
                        Err(error) => format!("Not started: {error:#}"),
                        Ok(()) => "Not started: source-changing batch stopped".into(),
                    });
                }
            }
        }
        drop(_parent_permit);
        if let Err(error) = &result {
            run.state = super::failure::classify(error, CheckState::ExecutionError);
            for command in &mut run.commands {
                if command.state == CheckState::Running {
                    command.state = run.state;
                    command.completed_at_ms = Some(now_ms());
                    command.error = Some(format!("{error:#}"));
                } else if command.state == CheckState::Pending {
                    command.state = CheckState::Skipped;
                    command.error = Some(format!("Not started: {error:#}"));
                }
            }
            run.record(
                if run.state == CheckState::Failed {
                    CheckEventKind::Finished
                } else {
                    CheckEventKind::ExecutionError
                },
                None,
                format!("Check execution stopped: {error:#}"),
            );
        }
        if run.state != CheckState::Interrupted {
            run.cancelled_inputs = None;
        }
        run.completed_at_ms = Some(now_ms());
        // Save before publishing the terminal state, so the editor sees one terminal outcome
        // instead of a result that a failed save later replaces.
        let saved = publication.finish(&run.clone().into_record()).await;
        match &saved {
            Ok(()) => _workspace.published()?,
            Err(error) => _workspace.unresolved(format!(
                "check {} history could not be saved: {error:#}",
                run.id
            ))?,
        }
        if let Err(error) = &saved {
            run.state = CheckState::ExecutionError;
            run.record(
                CheckEventKind::ExecutionError,
                None,
                format!("Check history could not be saved: {error:#}"),
            );
        }
        self.update(&run.clone().into_record(), sink.as_ref())?;
        saved?;
        // A deadline is a retained failed check, so the plan owner must receive its run.
        if let Err(error) = result
            && run.state != CheckState::Failed
        {
            return Err(error);
        }
        Ok(run.into_record())
    }

    async fn run_inner(
        &self,
        run: &mut PreparedCheckRun,
        execution: CheckExecutionContext<'_>,
        cancelled: &Arc<CheckCancellation>,
        external_cancelled: Option<&tokio::sync::watch::Receiver<bool>>,
    ) -> Result<()> {
        let CheckExecutionContext {
            sink,
            publication,
            deadline,
            planned_inputs,
            process_owner,
        } = execution;
        if let Some(deadline) = deadline {
            deadline.check()?;
        }
        self.update(&run.clone().into_record(), sink)?;
        publication.update(&run.clone().into_record());
        if run.planned_tests.is_some() {
            super::planned::resolve_run(
                self,
                run,
                deadline.context("planned run deadline missing")?,
                planned_inputs.context("planned discovery inputs missing")?,
                cancelled,
                external_cancelled,
                process_owner,
            )
            .await?;
            if run.state == CheckState::Interrupted {
                return Ok(());
            }
            // Missing or ambiguous planned tests fail the gate after the run; they never stop
            // checks that do not depend on them. Only an attempt without one complete
            // resolution of its captured source starts nothing.
            if let Some(reason) = run
                .planned_tests
                .as_ref()
                .and_then(super::PlannedTestExecution::execution_blocker)
            {
                for command in &mut run.commands {
                    if command.state == CheckState::Pending {
                        command.state = CheckState::Skipped;
                        command.error = Some(format!("Not started: {reason}"));
                    }
                }
                run.state = CheckState::Failed;
                return Ok(());
            }
            // Mandatory verification takes precedence over the plan's case selections and exclusions.
            if self.has_verification_requirements()? {
                let snapshot = self.changes.capture(&self.session_id).await?;
                self.verification
                    .lock()
                    .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                    .require_in_run(run, &snapshot)?;
            }
        }
        let applying = run.commands.iter().any(|command| {
            command.selected
                && command.context.purpose == super::CheckPurpose::Prepare
                && matches!(
                    command.context.category,
                    super::CheckCategory::Format | super::CheckCategory::Lint
                )
        });
        for index in 0..run.commands.len() {
            if let Some(inputs) = planned_inputs {
                super::planned::validate_inputs(self, run, inputs)?;
            }
            if let Some(deadline) = deadline {
                deadline.check()?;
            }
            if applying
                && let Some(previous) = run.commands[..index]
                    .iter()
                    .find(|command| command.selected && command.state != CheckState::Passed)
            {
                let reason = format!(
                    "Not started: earlier operation {} ended {:?}",
                    previous.context.definition_id, previous.state
                );
                for command in run.commands.iter_mut().skip(index) {
                    if command.state == CheckState::Pending {
                        command.state = CheckState::Skipped;
                        command.error = Some(reason.clone());
                    }
                }
                break;
            }
            if cancelled.requested.load(Ordering::Acquire)
                || external_cancelled.is_some_and(|value| *value.borrow())
            {
                run.interrupt();
                return Ok(());
            }
            let mut context = run.commands[index].context.clone();
            if run.commands[index].state == CheckState::Pending {
                for dependency in &context.depends_on {
                    let prerequisites: Vec<_> = run
                        .commands
                        .iter()
                        .filter(|check| &check.context.definition_id == dependency)
                        .collect();
                    ensure!(!prerequisites.is_empty(), "missing prerequisite command");
                    if let Some(prerequisite) = prerequisites
                        .iter()
                        .find(|check| check.state != CheckState::Passed)
                    {
                        run.commands[index].state =
                            if prerequisite.error.as_deref() == Some("Disabled for this task") {
                                CheckState::ConfigurationError
                            } else {
                                CheckState::Skipped
                            };
                        run.commands[index].error =
                            Some(format!("Prerequisite {dependency} did not pass"));
                        break;
                    }
                }
            }
            match run.commands[index].state {
                CheckState::Skipped
                | CheckState::ConfigurationError
                | CheckState::ExecutionError
                | CheckState::Blocked
                | CheckState::Stale
                | CheckState::Stopped => {
                    let kind = if run.commands[index].state == CheckState::Skipped {
                        CheckEventKind::CheckSkipped
                    } else if run.commands[index].state == CheckState::ConfigurationError {
                        CheckEventKind::ConfigurationError
                    } else {
                        CheckEventKind::CheckFinished
                    };
                    run.record(
                        kind,
                        Some(context.definition_id),
                        format!(
                            "{} · {:?}: {}",
                            context.name,
                            run.commands[index].state,
                            run.commands[index].error.as_deref().unwrap_or("")
                        ),
                    );
                    self.update(&run.clone().into_record(), sink)?;
                    publication.update(&run.clone().into_record());
                    continue;
                }
                CheckState::Passed | CheckState::Failed => continue,
                _ => {}
            }
            let preparing = context.purpose == super::CheckPurpose::Prepare;
            if applying {
                super::discovery::DiscoveryInputs::for_run(self, run)?;
            }
            let verification_capture = self
                .verification
                .lock()
                .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                .capture();
            let Some(before) = cancelled
                .read(external_cancelled, async {
                    if preparing {
                        self.changes.capture(&self.session_id).await.map(Some)
                    } else {
                        self.changes.snapshot().await
                    }
                })
                .await?
            else {
                run.interrupt();
                return Ok(());
            };
            run.cancelled_inputs = before.clone();
            // A resumed plan Check owns fresh case outcomes; ordinary automatic checks retain
            // cancellation suppression and evidence reuse for unchanged inputs.
            if run.context.automatic
                && run.planned_tests.is_none()
                && let Some(previous) = self.cancelled_attempt(
                    &run.clone().into_record(),
                    &run.commands[index].clone().into_record(),
                    before.as_ref(),
                )?
            {
                run.commands[index].state = CheckState::Stopped;
                run.cancelled_run_id = Some(previous.clone());
                run.commands[index].error = Some(format!(
                    "Cancelled in run {previous}. Run this check explicitly to retry unchanged inputs."
                ));
                run.record(
                    CheckEventKind::CheckSkipped,
                    Some(context.definition_id),
                    "Automatic retry suppressed after cancellation".into(),
                );
                continue;
            }
            if run.context.automatic
                && run.planned_tests.is_none()
                && let Some(checkpoint) = before.as_ref()
                && let Some((run_id, previous)) = self.evidence(
                    &run.clone().into_record(),
                    &run.commands[index].clone().into_record(),
                    checkpoint,
                )?
            {
                let command = &mut run.commands[index];
                command.state = previous.state;
                command.exit_code = previous.exit_code;
                command.error = previous.error;
                command.evidence = previous.evidence;
                command.reused_from = Some((run_id.clone(), previous.id));
                command.stdout = previous.stdout;
                command.stderr = previous.stderr;
                command.output = previous.output;
                command.output_bytes = previous.output_bytes;
                command.preview = previous.preview;
                command.result = previous.result;
                command.adapter_progress = previous.adapter_progress;
                context.resolved = previous.context.and_then(|context| context.resolved);
                command.context = context.clone();
                run.record(
                    CheckEventKind::CheckFinished,
                    Some(context.definition_id),
                    format!("Reused current check evidence from {run_id}"),
                );
                self.update(&run.clone().into_record(), sink)?;
                publication.update(&run.clone().into_record());
                continue;
            }
            if let Some(deadline) = deadline {
                deadline.check()?;
            }
            if let Some(inputs) = planned_inputs {
                super::planned::validate_inputs(self, run, inputs)?;
            }
            let mut mutation = None;
            if preparing {
                context.preparation = Some(super::model::PreparationSnapshot::Started {
                    before: Box::new(before.clone().context("preparation snapshot missing")?),
                });
                run.commands[index].context = context.clone();
                if let Err(error) = publication.persist(&run.clone().into_record()).await {
                    run.commands[index].state = CheckState::ExecutionError;
                    run.commands[index].error = Some(format!(
                        "preparation did not start because its source snapshot could not be saved: {error:#}"
                    ));
                    return Err(error.context("persist source before preparation"));
                }
                if matches!(
                    context.category,
                    super::CheckCategory::Format | super::CheckCategory::Lint
                ) {
                    mutation = Some(
                        self.verification
                            .lock()
                            .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                            .prepare(
                                &run.clone().into_record(),
                                &run.commands[index].clone().into_record(),
                            )?,
                    );
                }
                if cancelled.requested.load(Ordering::Acquire)
                    || external_cancelled.is_some_and(|value| *value.borrow())
                {
                    if let Some(preparation) = mutation.take() {
                        self.verification
                            .lock()
                            .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                            .retire_unlaunched(preparation)?;
                    }
                    run.interrupt();
                    return Ok(());
                }
            }
            let mutation = mutation
                .map(|preparation| {
                    self.verification
                        .lock()
                        .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                        .launch(preparation)
                })
                .transpose()?;
            run.commands[index].state = CheckState::Running;
            run.commands[index].started_at_ms = Some(now_ms());
            run.record(
                CheckEventKind::CheckStarted,
                Some(context.definition_id.clone()),
                format!("Check started · {}", context.name),
            );
            self.update(&run.clone().into_record(), sink)?;
            publication.update(&run.clone().into_record());
            let execution_result = self
                .execute(
                    run,
                    index,
                    context.clone(),
                    execution,
                    cancelled,
                    external_cancelled,
                )
                .await;
            if let Err(error) = &execution_result {
                run.commands[index].state = if cancelled.requested.load(Ordering::Acquire)
                    || external_cancelled.is_some_and(|value| *value.borrow())
                {
                    CheckState::Interrupted
                } else {
                    super::failure::classify(error, CheckState::ExecutionError)
                };
                run.commands[index].error = Some(format!("{error:#}"));
            }
            run.commands[index].completed_at_ms = Some(now_ms());
            // Collection has finished. Caller cancellation must not erase partial source edits.
            let preparation_after = if preparing {
                let before = before.clone().context("preparation snapshot missing")?;
                match self.changes.capture(&self.session_id).await {
                    Ok(after) => {
                        run.commands[index].context.preparation =
                            Some(super::model::PreparationSnapshot::Captured {
                                before: Box::new(before),
                                after: Box::new(after.clone()),
                            });
                        Some(after)
                    }
                    Err(error) => {
                        let capture_error = format!("capture source after preparation: {error:#}");
                        run.commands[index].context.preparation =
                            Some(super::model::PreparationSnapshot::Unknown {
                                before: Box::new(before),
                                error: format!("{error:#}"),
                            });
                        let command = &mut run.commands[index];
                        command.state = CheckState::ExecutionError;
                        command.error = Some(match command.error.take() {
                            Some(previous) => format!("{previous}\n{capture_error}"),
                            None => capture_error,
                        });
                        return Err(error.context("capture source after preparation"));
                    }
                }
            } else {
                None
            };
            if let Some(mutation) = mutation {
                // Store the collected result and after-snapshot before retiring its obligation.
                publication.persist(&run.clone().into_record()).await?;
                self.verification
                    .lock()
                    .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                    .retire_unsupported(
                        mutation,
                        &run.commands[index].clone().into_record(),
                        before.as_ref().context("preparation snapshot missing")?,
                        preparation_after
                            .as_ref()
                            .context("preparation snapshot missing")?,
                    )?;
            }
            if let Err(error) = execution_result
                && error.is::<CommandTimeout>()
            {
                return Err(error);
            }
            if let Some(deadline) = deadline {
                deadline.check()?;
            }
            if matches!(
                run.commands[index].state,
                CheckState::Passed | CheckState::Failed
            ) {
                let after = if let Some(after) = preparation_after {
                    Some(after)
                } else {
                    let Some(after) = cancelled
                        .read(external_cancelled, self.changes.snapshot())
                        .await?
                    else {
                        run.interrupt();
                        return Ok(());
                    };
                    after
                };
                if let (Some(before), Some(after)) = (before, after) {
                    if let Some(deadline) = deadline {
                        deadline.check()?;
                    }
                    run.commands[index].evidence = super::evidence::CheckEvidence::capture(
                        self.changes.objects(),
                        &run.clone().into_record(),
                        &run.commands[index].clone().into_record(),
                        before.clone(),
                        &after,
                    )?;
                    if !preparing {
                        publication.persist(&run.clone().into_record()).await?;
                        if let Some(deadline) = deadline {
                            deadline.check()?;
                        }
                        if self.has_verification_requirements()? {
                            super::discovery::DiscoveryInputs::for_run(self, run)?;
                        }
                        self.verification
                            .lock()
                            .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                            .observe(
                                verification_capture,
                                &run.clone().into_record(),
                                &run.commands[index].clone().into_record(),
                                &before,
                                &after,
                            )?;
                    }
                }
            }
            let check = &run.commands[index];
            let kind = if check.state == CheckState::ConfigurationError {
                CheckEventKind::ConfigurationError
            } else {
                CheckEventKind::CheckFinished
            };
            let text = format!(
                "Check finished · {} · {:?} · {}ms{}",
                context.name,
                check.state,
                check.elapsed_ms(0).unwrap_or(0),
                check
                    .error
                    .as_ref()
                    .map(|error| format!(" · {error}"))
                    .unwrap_or_default()
            );
            run.record(kind, Some(context.definition_id), text);
            self.update(&run.clone().into_record(), sink)?;
            publication.update(&run.clone().into_record());
            if run.commands[index].state == CheckState::Interrupted {
                run.interrupt();
                return Ok(());
            }
            if preparing && run.commands[index].state != CheckState::Passed {
                for command in run.commands.iter_mut().skip(index + 1) {
                    if command.state == CheckState::Pending {
                        command.state = CheckState::Skipped;
                        command.error =
                            Some("Not started: an earlier source-changing operation failed".into());
                    }
                }
                break;
            }
        }
        if run.planned_tests.is_some() {
            super::planned::finish_run(
                self,
                run,
                planned_inputs.context("planned discovery inputs missing")?,
            )
            .await?;
        }
        if let Some(deadline) = deadline {
            deadline.check()?;
        }
        run.completed_at_ms = Some(now_ms());
        run.state = if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::ExecutionError)
        {
            CheckState::ExecutionError
        } else if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::Stopped)
        {
            CheckState::Stopped
        } else if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::Stale)
        {
            CheckState::Stale
        } else if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::ConfigurationError)
        {
            CheckState::ConfigurationError
        } else if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::Blocked)
        {
            CheckState::Blocked
        } else if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::Failed)
            || run
                .planned_tests
                .as_ref()
                .is_some_and(|planned| !planned.satisfied())
        {
            CheckState::Failed
        } else if run
            .commands
            .iter()
            .all(|check| check.state == CheckState::Skipped)
        {
            CheckState::Skipped
        } else {
            CheckState::Passed
        };
        run.record(
            CheckEventKind::Finished,
            None,
            format!("Checks · {} · {}ms", run.summary(), run.duration_ms()),
        );
        Ok(())
    }

    /// Load the configured tool budget before admitting or launching execution.
    fn execution_timeout(&self) -> Result<std::time::Duration> {
        Ok(crate::settings::SettingsStore::open(&self.data_root)?
            .get(&self.workspace.to_string_lossy(), &self.session_id, "")?
            .desired
            .tool_timeout
            .unwrap_or_default()
            .require_expiry()?
            .duration())
    }

    async fn execute(
        &self,
        run: &mut PreparedCheckRun,
        index: usize,
        context: super::model::CheckCommand,
        target: CheckExecutionContext<'_>,
        cancelled: &Arc<CheckCancellation>,
        external_cancelled: Option<&tokio::sync::watch::Receiver<bool>>,
    ) -> Result<()> {
        let CheckExecutionContext {
            sink,
            publication,
            deadline,
            planned_inputs: _,
            process_owner: _,
        } = target;
        let directory = self
            .workspace
            .join(super::config::relative_path(&context.directory)?)
            .canonicalize()
            .context("check working directory is unavailable")?;
        ensure!(
            directory.starts_with(self.workspace.canonicalize()?),
            "check directory escapes the project"
        );
        let adapter = run
            .context
            .snapshot
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == context.definition_id)
            .and_then(|definition| match &definition.runner {
                super::CheckRunner::Adapter(adapter) => Some(adapter.clone()),
                super::CheckRunner::Command(_) => None,
            });
        if let Some(adapter) = adapter {
            return self
                .execute_adapter(
                    run,
                    index,
                    context,
                    &directory,
                    adapter,
                    target,
                    cancelled,
                    external_cancelled,
                )
                .await;
        }
        let direct = context.invocation.as_ref().filter(|_| {
            context
                .operation
                .as_ref()
                .is_some_and(|operation| operation.args.is_some())
        });
        let mut command = if let Some(invocation) = direct {
            let mut command = crate::process::command(&invocation.program, &invocation.args);
            command.stdin(std::process::Stdio::null());
            command
        } else {
            let executable = run.executable.as_ref().ok_or_else(|| {
                super::failure::CheckFailure::classified(
                    CheckState::Blocked,
                    anyhow::anyhow!("configured check shell is unavailable"),
                )
            })?;
            run.shell
                .command(executable, &run.commands[index].command, false)?
        };
        command
            .current_dir(&directory)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        let deadline = match deadline {
            Some(deadline) => deadline,
            None => CheckDeadline::new(self.execution_timeout()?),
        };
        deadline.check()?;
        let process_owner = target.process_owner.clone();
        let mut child = process_owner
            .spawn(command)
            .map_err(super::failure::CheckFailure::launch)
            .context("start configured check")?;
        let mut stdout = child.stdout().take().context("check stdout unavailable")?;
        let mut stderr = child.stderr().take().context("check stderr unavailable")?;
        let process_cancelled = Arc::clone(cancelled);
        let process_external = external_cancelled.cloned();
        let (stop, stopped) = tokio::sync::oneshot::channel();
        let worker = tokio::spawn(async move {
            let _process_owner = process_owner;
            let cancelled = async {
                tokio::select! {
                    _ = process_cancelled.wait(process_external) => {},
                    _ = stopped => {},
                }
            };
            tokio::select! {
                biased;
                _ = cancelled => {
                    child.start_kill()?;
                    Ok((child.wait().await?, true))
                }
                status = child.wait() => Ok((status?, false)),
            }
        });
        let mut process = CheckProcess {
            worker,
            stop: Some(stop),
            collected: false,
        };
        let result = async {
            let mut output = tokio::fs::File::create(&run.commands[index].output).await?;
            let mut stdout_file = tokio::fs::File::create(&run.commands[index].stdout).await?;
            let mut stderr_file = tokio::fs::File::create(&run.commands[index].stderr).await?;
            let mut stdout_buffer = [0u8; 8192];
            let mut stderr_buffer = [0u8; 8192];
            let mut stdout_done = false;
            let mut stderr_done = false;
            let mut status = None;
            let mut killed = false;
            let mut publication_deadline = None;
            while !stdout_done || !stderr_done || status.is_none() {
                tokio::select! {
                    result = stdout.read(&mut stdout_buffer), if !stdout_done => {
                        let bytes = result?; stdout_done = bytes == 0;
                        if bytes > 0 { stdout_file.write_all(&stdout_buffer[..bytes]).await?; output.write_all(&stdout_buffer[..bytes]).await?;
                            run.commands[index].output_bytes += bytes as u64; append_preview(&mut run.commands[index].preview, &stdout_buffer[..bytes]);
                            publication_deadline.get_or_insert_with(|| tokio::time::Instant::now() + std::time::Duration::from_millis(250)); }
                    },
                    result = stderr.read(&mut stderr_buffer), if !stderr_done => {
                        let bytes = result?; stderr_done = bytes == 0;
                        if bytes > 0 { stderr_file.write_all(&stderr_buffer[..bytes]).await?; output.write_all(&stderr_buffer[..bytes]).await?;
                            run.commands[index].output_bytes += bytes as u64; append_preview(&mut run.commands[index].preview, &stderr_buffer[..bytes]);
                            publication_deadline.get_or_insert_with(|| tokio::time::Instant::now() + std::time::Duration::from_millis(250)); }
                    },
                    result = process.wait(), if status.is_none() => {
                        let (completed, interrupted) = result?;
                        status = Some(completed);
                        killed = interrupted;
                    },
                    _ = async {
                        match publication_deadline {
                            Some(deadline) => tokio::time::sleep_until(deadline).await,
                            None => std::future::pending::<()>().await,
                        }
                    } => {
                        publication_deadline = None;
                        output.flush().await?;
                        self.update(&run.clone().into_record(), sink)?;
                        publication.update(&run.clone().into_record());
                    },
                }
            }
            output.flush().await?;
            stdout_file.flush().await?;
            stderr_file.flush().await?;
            let status = status.context("check process status missing")?;
            let command = &mut run.commands[index];
            command.exit_code = status.code();
            command.state = if killed {
                CheckState::Interrupted
            } else if status.success() {
                CheckState::Passed
            } else {
                CheckState::Failed
            };
            command.context = context;
            if !killed && status.success() {
                command.context.resolved = Some(command.context.selection.clone());
            }
            Ok(())
        };
        let result = tokio::select! {
            biased;
            result = result => result,
            () = tokio::time::sleep_until(deadline.instant()) => {
                let timeout = CommandTimeout(deadline.timeout);
                Err(if target.deadline.is_some() {
                    super::failure::CheckFailure::classified(CheckState::Failed, timeout)
                } else {
                    timeout.into()
                })
            },
        };
        if result.is_err() {
            process.stop().await?;
        }
        result
    }

    /// Execute an adapter with the command's cancellation, deadline, and publication ownership.
    #[allow(clippy::too_many_arguments)]
    async fn execute_adapter(
        &self,
        run: &mut PreparedCheckRun,
        index: usize,
        mut context: super::model::CheckCommand,
        directory: &Path,
        config: super::adapter::AdapterConfig,
        target: CheckExecutionContext<'_>,
        cancelled: &Arc<CheckCancellation>,
        external_cancelled: Option<&tokio::sync::watch::Receiver<bool>>,
    ) -> Result<()> {
        use super::adapter::{
            AdapterContext, AdapterError, AdapterProcesses, AdapterProgress, AdapterResult,
            CategoryAdapter,
        };
        let adapter = config.build(context.category)?;
        let deadline = match target.deadline {
            Some(deadline) => deadline,
            None => CheckDeadline::new(self.execution_timeout()?),
        };
        deadline.check()?;
        let (stop, receiver) = tokio::sync::watch::channel(false);
        let processes = AdapterProcesses::new(
            receiver,
            deadline.instant(),
            run.commands[index].output.clone(),
        )
        .owner_bind(Some(target.process_owner.clone()));
        let scratch = run.commands[index].output.with_extension("adapter");
        let workspace = self.workspace.canonicalize()?;
        let adapter_context = AdapterContext {
            workspace: &workspace,
            directory,
            processes: &processes,
            scratch: &scratch,
        };
        match adapter {
            CategoryAdapter::Test(adapter) => {
                let resolved = match &context.selection {
                    CheckSelection::All => Ok(super::adapter::TestSelection::default()),
                    CheckSelection::Tests { tests } => Ok(tests.clone()),
                    CheckSelection::Files { .. }
                    | CheckSelection::Paths { .. }
                    | CheckSelection::Inputs { .. } => Err(AdapterError::Selection(
                        "test adapter requires test IDs, affected paths, or all".into(),
                    )),
                };
                let selection = resolved.as_ref().cloned().unwrap_or_default();
                let mut events = super::adapter::events::TestEventSink::new(
                    selection.clone(),
                    &scratch.join("cases"),
                )?;
                let mut progress = events.progress_subscribe();
                let outcome = if let Err(error) = resolved {
                    Err(error)
                } else {
                    let operation = adapter.run(&adapter_context, &selection, &mut events);
                    tokio::pin!(operation);
                    let mut interrupted = false;
                    loop {
                        tokio::select! {
                            result = &mut operation => break result,
                            _ = cancelled.wait(external_cancelled.cloned()), if !interrupted => {
                                interrupted = true;
                                stop.send_replace(true);
                            }
                            changed = progress.changed() => {
                                if changed.is_ok() {
                                    run.commands[index].adapter_progress = Some(super::adapter::AdapterProgress::Tests(progress.borrow_and_update().clone()));
                                    if let Err(error) = self.update(&run.clone().into_record(), target.sink) {
                                        stop.send_replace(true);
                                        let _ = operation.as_mut().await;
                                        return Err(error);
                                    }
                                    target.publication.update(&run.clone().into_record());
                                }
                            }
                        }
                    }
                };
                let interrupted = matches!(outcome, Err(AdapterError::Cancelled));
                let exit = match outcome {
                    Ok(exit) => Some(exit),
                    Err(error) => {
                        events.failure(&error);
                        None
                    }
                };
                run.commands[index].adapter_progress =
                    Some(super::adapter::AdapterProgress::Tests(events.progress()));
                let result = events.finish(exit, interrupted);
                run.commands[index].state = result.state();
                run.commands[index].exit_code = result.exit;
                run.commands[index].error = result
                    .protocol_error
                    .clone()
                    .or_else(|| result.error.clone())
                    .or_else(|| {
                        (!result.not_found.is_empty())
                            .then(|| "requested test items were not found".into())
                    })
                    .or_else(|| {
                        (result.cases.is_empty()).then(|| "the selection matched no tests".into())
                    });
                if run.commands[index].state == CheckState::Passed {
                    context.resolved = Some(CheckSelection::Tests { tests: selection });
                }
                run.commands[index].context = context;
                run.commands[index].result = Some(super::adapter::AdapterResult::Tests(result));
            }
            CategoryAdapter::Diagnostics(adapter) => {
                let mut selection = match &context.selection {
                    CheckSelection::All => super::adapter::PathSelection::default(),
                    CheckSelection::Files { files } => super::adapter::PathSelection {
                        paths: files.clone(),
                    },
                    CheckSelection::Paths { paths } => paths.clone(),
                    CheckSelection::Tests { .. } | CheckSelection::Inputs { .. } => {
                        anyhow::bail!("diagnostic adapter requires paths or all")
                    }
                };
                selection.normalize()?;
                let mut events = super::adapter::events::DiagnosticSink::new(
                    adapter.category(),
                    selection.clone(),
                )?;
                let mut progress = events.progress_subscribe();
                let outcome = {
                    let operation = async {
                        match context.purpose {
                            super::CheckPurpose::Verify => {
                                adapter.run(&adapter_context, &selection, &mut events).await
                            }
                            super::CheckPurpose::Prepare => {
                                adapter.fix(&adapter_context, &selection, &mut events).await
                            }
                        }
                    };
                    tokio::pin!(operation);
                    let mut interrupted = false;
                    loop {
                        tokio::select! {
                            result = &mut operation => break result,
                            _ = cancelled.wait(external_cancelled.cloned()), if !interrupted => {
                                interrupted = true;
                                stop.send_replace(true);
                            }
                            changed = progress.changed() => {
                                if changed.is_ok() {
                                    run.commands[index].adapter_progress = Some(AdapterProgress::Diagnostics(progress.borrow_and_update().clone()));
                                    if let Err(error) = self.update(&run.clone().into_record(), target.sink) {
                                        stop.send_replace(true);
                                        let _ = operation.as_mut().await;
                                        return Err(error);
                                    }
                                    target.publication.update(&run.clone().into_record());
                                }
                            }
                        }
                    }
                };
                let interrupted = matches!(outcome, Err(AdapterError::Cancelled));
                let exit = match outcome {
                    Ok(exit) => Some(exit),
                    Err(error) => {
                        events.failure(&error);
                        None
                    }
                };
                run.commands[index].adapter_progress =
                    Some(AdapterProgress::Diagnostics(events.progress()));
                let result = events.finish(exit, interrupted);
                run.commands[index].state = result.state();
                run.commands[index].exit_code = result.exit;
                run.commands[index].error = result
                    .protocol_error
                    .clone()
                    .or_else(|| result.error.clone())
                    .or_else(|| {
                        result.unsupported.then(|| match context.purpose {
                            super::CheckPurpose::Prepare => {
                                "this check's adapter has no fix operation".into()
                            }
                            super::CheckPurpose::Verify => {
                                "this check's adapter does not support the requested operation"
                                    .into()
                            }
                        })
                    });
                if run.commands[index].state == CheckState::Passed {
                    context.resolved = Some(CheckSelection::Paths { paths: selection });
                }
                run.commands[index].context = context;
                run.commands[index].result = Some(AdapterResult::Diagnostics(result));
            }
            CategoryAdapter::Format(adapter) => {
                let mut selection = match &context.selection {
                    CheckSelection::All => super::adapter::PathSelection::default(),
                    CheckSelection::Files { files } => super::adapter::PathSelection {
                        paths: files.clone(),
                    },
                    CheckSelection::Paths { paths } => paths.clone(),
                    CheckSelection::Tests { .. } | CheckSelection::Inputs { .. } => {
                        anyhow::bail!("format adapter requires paths or all")
                    }
                };
                selection.normalize()?;
                let mut events = super::adapter::events::FormatSink::new(
                    match context.purpose {
                        super::CheckPurpose::Verify => {
                            super::adapter::events::FormatOperation::Check
                        }
                        super::CheckPurpose::Prepare => {
                            super::adapter::events::FormatOperation::Apply
                        }
                    },
                    selection.clone(),
                )?;
                let mut progress = events.progress_subscribe();
                let outcome = {
                    let operation = async {
                        match context.purpose {
                            super::CheckPurpose::Verify => {
                                adapter
                                    .check(&adapter_context, &selection, &mut events)
                                    .await
                            }
                            super::CheckPurpose::Prepare => {
                                adapter
                                    .apply(&adapter_context, &selection, &mut events)
                                    .await
                            }
                        }
                    };
                    tokio::pin!(operation);
                    let mut interrupted = false;
                    loop {
                        tokio::select! {
                            result = &mut operation => break result,
                            _ = cancelled.wait(external_cancelled.cloned()), if !interrupted => {
                                interrupted = true;
                                stop.send_replace(true);
                            }
                            changed = progress.changed() => {
                                if changed.is_ok() {
                                    run.commands[index].adapter_progress = Some(AdapterProgress::Format(progress.borrow_and_update().clone()));
                                    if let Err(error) = self.update(&run.clone().into_record(), target.sink) {
                                        stop.send_replace(true);
                                        let _ = operation.as_mut().await;
                                        return Err(error);
                                    }
                                    target.publication.update(&run.clone().into_record());
                                }
                            }
                        }
                    }
                };
                let interrupted = matches!(outcome, Err(AdapterError::Cancelled));
                let exit = match outcome {
                    Ok(exit) => Some(exit),
                    Err(error) => {
                        events.failure(&error);
                        None
                    }
                };
                run.commands[index].adapter_progress =
                    Some(AdapterProgress::Format(events.progress()));
                let result = events.finish(exit, interrupted);
                run.commands[index].state = result.state();
                run.commands[index].exit_code = result.exit;
                run.commands[index].error = result
                    .protocol_error
                    .clone()
                    .or_else(|| result.error.clone())
                    .or_else(|| {
                        result.unsupported.then(|| match context.purpose {
                            super::CheckPurpose::Prepare => {
                                "this check's adapter has no fix operation".into()
                            }
                            super::CheckPurpose::Verify => {
                                "this check's adapter does not support the requested operation"
                                    .into()
                            }
                        })
                    });
                if run.commands[index].state == CheckState::Passed {
                    context.resolved = Some(CheckSelection::Paths { paths: selection });
                }
                run.commands[index].context = context;
                run.commands[index].result = Some(AdapterResult::Format(result));
            }
        }
        if let Ok(metadata) = tokio::fs::metadata(&run.commands[index].output).await {
            run.commands[index].output_bytes = metadata.len();
        }
        Ok(())
    }

    /// Persists a completed external update before publishing its live state.
    pub(crate) async fn publish(
        &self,
        run: &CheckRun,
        sink: Option<&SessionUpdateSink>,
    ) -> Result<()> {
        self.validate_owner(run)?;
        if let Some(previous) = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
            .records
            .get(&run.id)
        {
            ensure!(previous.owner == run.owner, "check ownership cannot change");
        }
        let path = self.root.join(&run.id).join("record.json");
        let record = run.clone();
        tokio::task::spawn_blocking(move || write_record(&path, &record))
            .await
            .context("check persistence worker failed")??;
        self.update(run, sink)
    }

    /// Persist a short check-record commit while the captured task authority remains current.
    pub(crate) fn publish_with_authority(
        &self,
        run: &CheckRun,
        sink: Option<&SessionUpdateSink>,
        authority: &crate::tools::TaskAuthority,
    ) -> Result<()> {
        authority.commit(|| {
            self.validate_owner(run)?;
            if let Some(previous) = self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
                .records
                .get(&run.id)
            {
                ensure!(previous.owner == run.owner, "check ownership cannot change");
            }
            let path = self.root.join(&run.id).join("record.json");
            write_record(&path, run)?;
            self.update(run, sink)
        })
    }

    fn update(&self, run: &CheckRun, sink: Option<&SessionUpdateSink>) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        if run.state == CheckState::Running
            && state.records.get(&run.id).is_some_and(|previous| {
                previous.state != CheckState::Running
                    && previous.lifecycle.len() >= run.lifecycle.len()
            })
        {
            return Ok(());
        }
        state.records.insert(run.id.clone(), run.clone());
        if run.state != CheckState::Running
            && state
                .active
                .as_ref()
                .is_some_and(|active| active.run_id == run.id)
        {
            state.active = None;
        }
        let patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("check presentation lock poisoned"))?
            .update_checks(run)?;
        let error = run
            .lifecycle
            .last()
            .filter(|event| {
                matches!(
                    event.kind,
                    CheckEventKind::ConfigurationError | CheckEventKind::ExecutionError
                )
            })
            .filter(|event| state.notified.insert(event.id.clone()))
            .cloned();
        drop(state);
        self.changed.notify_waiters();
        if !patch.is_empty()
            && let Some(sink) = sink
        {
            self.notification
                .get_or_init(|| CheckNotification::new(self.trace.clone(), self.session_id.clone()))
                .publish(sink, patch);
        }
        if let (Some(error), Some(sink)) = (error, sink) {
            self.notification
                .get_or_init(|| CheckNotification::new(self.trace.clone(), self.session_id.clone()))
                .error(
                    sink,
                    SessionUpdate::CheckFailure {
                        run_id: run.id.clone(),
                        error: error.text.clone(),
                    },
                );
        }
        Ok(())
    }

    /// Install the session owner's internal check route exactly once.
    pub(crate) fn install_owner(&self, owner: super::owner::CheckOwnerSender) -> Result<()> {
        self.owner
            .set(owner)
            .map_err(|_| anyhow::anyhow!("check owner already installed"))
    }

    /// Get the installed owner route without allowing unowned check admission.
    pub(crate) fn owner_sender(&self) -> Result<super::owner::CheckOwnerSender> {
        self.owner
            .get()
            .cloned()
            .context("check owner is unavailable")
    }

    /// Launch already resolved work only from an authenticated owner update step.
    pub(crate) fn launch_owned(
        self: &Arc<Self>,
        run: PreparedCheckRun,
        sink: Option<SessionUpdateSink>,
        external_cancelled: Option<tokio::sync::watch::Receiver<bool>>,
    ) -> Result<(
        String,
        tokio::sync::oneshot::Receiver<std::result::Result<CheckRun, String>>,
    )> {
        self.admit_and_launch(run, sink, external_cancelled, None, None)
    }

    async fn launch(
        self: &Arc<Self>,
        run: PreparedCheckRun,
        sink: Option<SessionUpdateSink>,
        external_cancelled: Option<tokio::sync::watch::Receiver<bool>>,
        admission: Option<crate::command::ExecutionPermit>,
        deadline: Option<CheckDeadline>,
    ) -> Result<(
        String,
        tokio::sync::oneshot::Receiver<std::result::Result<CheckRun, String>>,
    )> {
        let run = self.resolve(run).await?;
        self.admit_and_launch(run, sink, external_cancelled, admission, deadline)
    }

    fn admit_and_launch(
        self: &Arc<Self>,
        run: PreparedCheckRun,
        sink: Option<SessionUpdateSink>,
        external_cancelled: Option<tokio::sync::watch::Receiver<bool>>,
        admission: Option<crate::command::ExecutionPermit>,
        deadline: Option<CheckDeadline>,
    ) -> Result<(
        String,
        tokio::sync::oneshot::Receiver<std::result::Result<CheckRun, String>>,
    )> {
        std::fs::create_dir_all(self.root.join(&run.id))?;
        let admitted = self.admit(run, admission, deadline)?;
        let id = admitted.run.id.clone();
        if let Err(error) = self.update(&admitted.run.clone().into_record(), sink.as_ref()) {
            self.state
                .lock()
                .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?
                .active = None;
            return Err(error);
        }
        let supervisor = Arc::clone(self);
        let task_id = id.clone();
        let (completed, receiver) = tokio::sync::oneshot::channel();
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        state.task.retain(|_, task| !task.is_finished());
        let task = tokio::spawn(async move {
            let result = supervisor
                .run_admitted(admitted, sink, external_cancelled)
                .await
                .map_err(|error| format!("{error:#}"));
            if let Err(error) = &result {
                supervisor.trace.record(
                    &supervisor.session_id,
                    "checks.execution_failed",
                    serde_json::json!({"run_id":task_id,"error":error}),
                );
            }
            let _ = completed.send(result);
            if let Ok(mut state) = supervisor.state.lock() {
                state.task.remove(&task_id);
            }
        });
        state.task.insert(id.clone(), task);
        Ok((id, receiver))
    }
}

/// Decode one current-format run record, or `None` for a record from another history version.
pub(super) fn saved_record(path: &Path, directory_name: &str) -> Result<Option<CheckRun>> {
    let value: serde_json::Value = crate::codec::from_slice(&std::fs::read(path)?)
        .with_context(|| format!("decode check history {}", path.display()))?;
    if value["version"] != CHECK_HISTORY_VERSION {
        return Ok(None);
    }
    let run: CheckRun = crate::codec::from_value(value["run"].clone())
        .with_context(|| format!("decode current check history {}", path.display()))?;
    ensure!(
        directory_name == run.id,
        "check history identity differs from its directory"
    );
    Ok(Some(run))
}

/// Atomically replaces one run record on a blocking persistence worker.
///
/// The replacement is synced before the rename, so a crash leaves either the previous record or
/// the complete new one, never a renamed file whose contents had not reached the disk.
pub(super) fn write_record(path: &Path, run: &CheckRun) -> Result<()> {
    if path.exists() {
        let existing: serde_json::Value = crate::codec::from_slice(&std::fs::read(path)?)?;
        if existing["version"] == CHECK_HISTORY_VERSION {
            let previous: CheckRun = crate::codec::from_value(existing["run"].clone())?;
            ensure!(previous.owner == run.owner, "check ownership cannot change");
        }
    }
    std::fs::create_dir_all(path.parent().context("check history path has no parent")?)?;
    let temporary = path.with_extension("tmp");
    let encoded = crate::codec::to_vec(
        &serde_json::json!({"version":CHECK_HISTORY_VERSION,"run":crate::codec::to_value(run)?}),
    )?;
    let mut file = std::fs::File::create(&temporary)?;
    std::io::Write::write_all(&mut file, &encoded)?;
    file.sync_all()?;
    drop(file);
    std::fs::rename(temporary, path)?;
    Ok(())
}

fn append_preview(preview: &mut String, bytes: &[u8]) {
    preview.push_str(&String::from_utf8_lossy(bytes));
    let start = preview
        .match_indices('\n')
        .rev()
        .nth(4)
        .map_or(0, |(index, _)| index + 1);
    if start > 0 {
        preview.drain(..start);
    }
    if preview.len() > 8192 {
        let mut start = preview.len() - 8192;
        while !preview.is_char_boundary(start) {
            start += 1;
        }
        preview.drain(..start);
    }
}

pub(crate) fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}
