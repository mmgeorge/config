use super::model::{CheckCommand, CheckEventKind, CheckRunContext};
use super::publication::{CheckNotification, CheckPublication};
use super::{
    CheckCase, CheckDefinition, CheckReceipt, CheckRun, CheckSelection, CheckState, CommandCheck,
    ConfigSnapshot, RunRequest,
};
use crate::{
    backend::{BackendEvent, BackendEventSink},
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

const CHECK_HISTORY_VERSION: u32 = 5;

/// Owns project check admission, durable run history, and process cancellation for one session.
pub struct CheckSupervisor {
    pub(crate) session_id: String,
    pub(crate) data_root: PathBuf,
    pub(crate) workspace: PathBuf,
    pub(crate) root: PathBuf,
    pub(super) changes: super::changes::CheckChanges,
    presentation: Arc<Mutex<SessionPresentation>>,
    state: Mutex<CheckStateStore>,
    notification: OnceLock<CheckNotification>,
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
    task: BTreeMap<String, tokio::task::JoinHandle<()>>,
    notified: std::collections::BTreeSet<String>,
}

struct ActiveCheck {
    run_id: String,
    cancellation: Arc<CheckCancellation>,
    _admission: Option<tokio::sync::OwnedSemaphorePermit>,
}

struct CheckCancellation {
    requested: Arc<AtomicBool>,
    changed: tokio::sync::Notify,
}

impl CheckCancellation {
    fn cancel(&self) {
        self.requested.store(true, Ordering::Release);
        self.changed.notify_waiters();
    }

    async fn wait(&self, external: Option<&AtomicBool>) {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.requested.load(Ordering::Acquire)
                || external.is_some_and(|flag| flag.load(Ordering::Acquire))
            {
                return;
            }
            tokio::select! {
                _ = changed => {},
                _ = tokio::time::sleep(std::time::Duration::from_millis(25)), if external.is_some() => {},
            }
        }
    }

    async fn read<Output>(
        &self,
        external: Option<&Arc<AtomicBool>>,
        read: impl std::future::Future<Output = Result<Output>>,
    ) -> Result<Option<Output>> {
        tokio::select! {
            biased;
            _ = self.wait(external.map(AsRef::as_ref)) => Ok(None),
            result = read => result.map(Some),
        }
    }
}

struct CheckProcess {
    worker: tokio::task::JoinHandle<Result<(std::process::ExitStatus, bool)>>,
    stop: Option<tokio::sync::oneshot::Sender<()>>,
    collected: bool,
}

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
    /// None means the task has no complete change inventory.
    pub files: Option<Vec<String>>,
    /// Distinguishes an accepted empty case inventory from unavailable standalone discovery.
    pub cases: Option<Vec<CheckCase>>,
    /// Newly added cases only, distinct from new and modified inventory.
    pub new_cases: Option<Vec<CheckCase>>,
    pub has_scope: bool,
    pub shell: crate::session::CheckShell,
    pub trigger: String,
    pub task_id: Option<String>,
    pub revision: u32,
}

impl CheckSupervisor {
    pub(crate) fn session_owner(&self) -> crate::timeline::TimelineOwner {
        crate::timeline::TimelineOwner::session(&self.session_id)
    }

    pub(crate) fn exchange_owner(
        &self,
        exchange_id: &str,
    ) -> Result<crate::timeline::TimelineOwner> {
        crate::storage::SqliteStore::open(&self.data_root)?
            .capture_timeline_owner(&self.session_id, exchange_id)
    }

    fn validate_owner(&self, run: &CheckRun) -> Result<()> {
        ensure!(
            run.owner.session_id() == self.session_id,
            "check ownership crosses sessions"
        );
        crate::storage::SqliteStore::open(&self.data_root)?.validate_timeline_owner(&run.owner)
    }

    /// Reopens current-format history and marks detached execution interrupted without replaying commands.
    pub fn open(
        session_id: String,
        data_root: PathBuf,
        workspace: PathBuf,
        root: PathBuf,
        presentation: Arc<Mutex<SessionPresentation>>,
        objects: crate::storage::objects::ObjectStore,
        repositories: Arc<forge_git::store::RepositoryStore>,
        trace: Arc<crate::trace::TraceStore>,
    ) -> Result<Arc<Self>> {
        std::fs::create_dir_all(&root)?;
        let changes = super::changes::CheckChanges::open(&workspace, &root, objects, repositories)?;
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
            presentation,
            state: Mutex::new(CheckStateStore {
                records,
                active: None,
                task: BTreeMap::new(),
                notified: Default::default(),
            }),
            notification: OnceLock::new(),
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
            let value: serde_json::Value = crate::codec::from_slice(&std::fs::read(&path)?)
                .with_context(|| format!("decode check history {}", path.display()))?;
            if value["version"] != CHECK_HISTORY_VERSION {
                continue;
            }
            let mut run: CheckRun = crate::codec::from_value(value["run"].clone())
                .with_context(|| format!("decode current check history {}", path.display()))?;
            ensure!(
                directory.file_name().to_string_lossy() == run.id,
                "check history identity differs from its directory"
            );
            for command in &mut run.commands {
                if command.report.is_none() {
                    command.report = super::report::collect(command);
                }
            }
            records.push(run);
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

    /// Builds exact commands from saved definitions and task selectors before acquiring execution ownership.
    pub(crate) fn prepare(
        &self,
        snapshot: ConfigSnapshot,
        admission: CheckAdmission,
    ) -> Result<CheckRun> {
        let CheckAdmission {
            owner,
            request,
            overrides,
            files,
            cases,
            new_cases,
            has_scope,
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
            context: Some(CheckRunContext {
                automatic: false,
                trigger,
                task_id,
                snapshot,
                overrides,
                changed_files: files.clone().unwrap_or_default(),
                planned_cases: cases.clone().unwrap_or_default(),
            }),
            lifecycle: Vec::new(),
            completed_plan: None,
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
            workspace_identity: None,
            workspace_digest: BTreeMap::new(),
            commands: Vec::new(),
        };
        let applicability =
            super::applicability::resolve(&request, &effective, files.as_deref(), has_scope)?;
        let requested: std::collections::BTreeSet<_> = applicability
            .iter()
            .filter(|item| item.selected)
            .map(|item| item.id.clone())
            .collect();
        let mut effective_request = request.clone();
        effective_request.all = false;
        effective_request.ids = requested.iter().cloned().collect();
        let selections = if requested.is_empty() {
            BTreeMap::new()
        } else if request.all {
            requested
                .iter()
                .map(|id| (id.clone(), CheckSelection::All))
                .collect()
        } else {
            effective_request.selections(
                &effective,
                files.as_deref().unwrap_or_default(),
                cases.as_deref(),
                new_cases.as_deref(),
                has_scope,
            )?
        };
        for index in effective.order_selected(&requested)? {
            let definition = &effective.definitions[index];
            let applicability = applicability
                .iter()
                .find(|item| item.id == definition.id)
                .unwrap();
            ensure!(
                definition.enabled
                    || request.ids.is_empty()
                    || (!request.ids.contains(&definition.id)
                        && !request.include.contains(&definition.id)),
                "{} is disabled; re-enable it in /checks",
                definition.id
            );
            let selection = selections.get(&definition.id).cloned().unwrap_or_default();
            let operation = operation(definition, &selection);
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
            } else if selection.empty()
                && matches!(selection, CheckSelection::Cases { .. })
                && (if definition.scope == super::CheckScope::NewCases {
                    new_cases.is_none()
                } else {
                    cases.is_none()
                })
            {
                (CheckState::ConfigurationError, Some("Plan case inventory is unavailable outside plan execution. Supply declared inputs or explicitly select full context.".into()))
            } else if selection.empty() && files.is_none() && has_scope {
                (CheckState::ConfigurationError, Some("Task change inventory is unavailable. Supply declared inputs or explicitly select full context.".into()))
            } else if selection.empty() {
                (
                    CheckState::Skipped,
                    Some(
                        if definition.scope == super::CheckScope::NewCases {
                            "No new tests in the accepted plan"
                        } else {
                            "No matching task changes"
                        }
                        .into(),
                    ),
                )
            } else if operation.is_none() {
                (
                    CheckState::ConfigurationError,
                    Some("Requested scope is unsupported. Enable /checks setup on and request a correction in chat.".into()),
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
                command: operation.unwrap_or(&definition.all).display(),
                context: Some(CheckCommand {
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
                    receipt: directory.join(format!("{position}.selection.json")),
                }),
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
                report: None,
            });
        }
        run.record(
            CheckEventKind::Started,
            None,
            format!("Checks started · {} selected", requested.len()),
        );
        Ok(run)
    }

    /// Freezes declared inputs before review, authorization, or process admission.
    pub(crate) async fn resolve(&self, mut run: CheckRun) -> Result<CheckRun> {
        let mut commands = Vec::new();
        let planned_cases = run
            .context
            .as_ref()
            .map(|context| context.planned_cases.clone())
            .unwrap_or_default();
        for mut check in std::mem::take(&mut run.commands) {
            let context = check.context.as_ref().context("check context is missing")?;
            if check.state != CheckState::Pending
                || context.invocation.is_some()
                || context.operation.is_none()
            {
                commands.push(check);
                continue;
            }
            match super::invocation::resolve(
                &self.workspace,
                &context.directory,
                context.operation.as_ref().unwrap(),
                &context.selection,
                &context.matched_files,
                &planned_cases,
            ) {
                Ok(invocations) => {
                    ensure!(
                        !invocations.is_empty(),
                        "nonempty check selection resolved to no commands"
                    );
                    for invocation in invocations {
                        let mut expanded = check.clone();
                        if context
                            .operation
                            .as_ref()
                            .is_some_and(|operation| operation.args.is_some())
                        {
                            expanded.command = invocation.display();
                        }
                        let context = expanded.context.as_mut().unwrap();
                        if !invocation.files.is_empty()
                            && matches!(context.selection, CheckSelection::Files { .. })
                        {
                            context.matched_files = invocation.files.clone();
                            context.reason = format!("Changed: {}", invocation.files.join(", "));
                        }
                        if !invocation.cases.is_empty() {
                            context.reason = format!(
                                "Selected plan tests: {}",
                                invocation
                                    .cases
                                    .iter()
                                    .map(|case| case.name.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );
                        }
                        context.invocation = Some(invocation);
                        commands.push(expanded);
                    }
                }
                Err(error) => {
                    check.state = super::failure::classify(&error, CheckState::ConfigurationError);
                    check.error = Some(format!("{error:#}"));
                    commands.push(check);
                }
            }
        }
        for (position, check) in commands.iter_mut().enumerate() {
            check.id = format!("{}:{position}", run.id);
            let directory = self.root.join(&run.id);
            check.stdout = directory.join(format!("{position}.stdout"));
            check.stderr = directory.join(format!("{position}.stderr"));
            check.output = directory.join(format!("{position}.output"));
            check.context.as_mut().unwrap().receipt =
                directory.join(format!("{position}.selection.json"));
        }
        run.commands = commands;
        Ok(run)
    }

    /// Runs the same serial dependency-aware lifecycle for automatic gates and on-demand requests.
    pub(crate) async fn run(
        self: &Arc<Self>,
        run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
    ) -> Result<CheckRun> {
        let (_, completed) = self.launch(run, sink, external_cancelled, None).await?;
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
                if let Some(evidence) = &command.evidence {
                    if evidence.covers(run, check, checkpoint)? {
                        return Ok(Some((previous.id.clone(), command.clone())));
                    }
                }
            }
        }
        Ok(None)
    }

    fn admit(
        &self,
        run: &CheckRun,
        admission: Option<tokio::sync::OwnedSemaphorePermit>,
    ) -> Result<Arc<CheckCancellation>> {
        self.validate_owner(run)?;
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("check state lock poisoned"))?;
        ensure!(
            state.active.is_none(),
            "another check run is already active"
        );
        ensure!(
            !state.task.contains_key(&run.id),
            "previous check run is still saving"
        );
        let cancelled = Arc::new(CheckCancellation {
            requested: Arc::new(AtomicBool::new(false)),
            changed: Default::default(),
        });
        state.records.insert(run.id.clone(), run.clone());
        state.active = Some(ActiveCheck {
            run_id: run.id.clone(),
            cancellation: Arc::clone(&cancelled),
            _admission: admission,
        });
        Ok(cancelled)
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
                    && evidence.covers(run, check, checkpoint)?
                {
                    return Ok(None);
                }
                if previous.state == CheckState::Interrupted && command.state != CheckState::Skipped
                {
                    if let (Some(cancelled), Some(checkpoint)) =
                        (&previous.cancelled_inputs, checkpoint)
                    {
                        if let Some(evidence) = super::evidence::CheckEvidence::capture(
                            &previous,
                            command,
                            cancelled.clone(),
                            cancelled,
                        )? && evidence.covers(run, check, checkpoint)?
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
        mut run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
        cancelled: Arc<CheckCancellation>,
    ) -> Result<CheckRun> {
        let publication = CheckPublication::new(self.root.join(&run.id).join("record.json"), &run);
        use futures_util::FutureExt;
        let result = std::panic::AssertUnwindSafe(self.run_inner(
            &mut run,
            sink.as_ref(),
            &publication,
            &cancelled,
            external_cancelled.as_ref(),
        ))
        .catch_unwind()
        .await
        .unwrap_or_else(|_| Err(anyhow::anyhow!("check worker panicked")));
        if let Err(error) = &result {
            run.state = CheckState::ExecutionError;
            for command in &mut run.commands {
                if command.state == CheckState::Running {
                    command.state = CheckState::ExecutionError;
                    command.completed_at_ms = Some(now_ms());
                    command.error = Some(format!("{error:#}"));
                }
            }
            run.record(
                CheckEventKind::ExecutionError,
                None,
                format!("Check execution error: {error:#}"),
            );
        }
        if run.state != CheckState::Interrupted {
            run.cancelled_inputs = None;
        }
        run.completed_at_ms = Some(now_ms());
        self.update(&run, sink.as_ref())?;
        if let Err(error) = publication.finish(&run).await {
            run.state = CheckState::ExecutionError;
            run.record(
                CheckEventKind::ExecutionError,
                None,
                format!("Check history could not be saved: {error:#}"),
            );
            self.update(&run, sink.as_ref())?;
            return Err(error);
        }
        result?;
        Ok(run)
    }

    async fn run_inner(
        &self,
        run: &mut CheckRun,
        sink: Option<&BackendEventSink>,
        publication: &CheckPublication,
        cancelled: &Arc<CheckCancellation>,
        external_cancelled: Option<&Arc<AtomicBool>>,
    ) -> Result<()> {
        self.update(run, sink)?;
        publication.update(run);
        for index in 0..run.commands.len() {
            if cancelled.requested.load(Ordering::Acquire)
                || external_cancelled.is_some_and(|value| value.load(Ordering::Acquire))
            {
                run.interrupt();
                return Ok(());
            }
            let context = run.commands[index].context.as_ref().unwrap().clone();
            if run.commands[index].state == CheckState::Pending {
                for dependency in &context.depends_on {
                    let prerequisites: Vec<_> = run
                        .commands
                        .iter()
                        .filter(|check| {
                            check
                                .context
                                .as_ref()
                                .is_some_and(|value| &value.definition_id == dependency)
                        })
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
                    self.update(run, sink)?;
                    publication.update(run);
                    continue;
                }
                CheckState::Passed | CheckState::Failed => continue,
                _ => {}
            }
            let Some(before) = cancelled
                .read(external_cancelled, self.changes.snapshot())
                .await?
            else {
                run.interrupt();
                return Ok(());
            };
            run.cancelled_inputs = before.clone();
            if run
                .context
                .as_ref()
                .is_some_and(|context| context.automatic)
                && let Some(previous) =
                    self.cancelled_attempt(run, &run.commands[index], before.as_ref())?
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
            if run
                .context
                .as_ref()
                .is_some_and(|context| context.automatic)
                && let Some(checkpoint) = before.as_ref()
                && let Some((run_id, previous)) =
                    self.evidence(run, &run.commands[index], checkpoint)?
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
                command.report = previous.report;
                command.context.as_mut().unwrap().resolved =
                    previous.context.and_then(|context| context.resolved);
                run.record(
                    CheckEventKind::CheckFinished,
                    Some(context.definition_id),
                    format!("Reused current check evidence from {run_id}"),
                );
                self.update(run, sink)?;
                publication.update(run);
                continue;
            }
            run.commands[index].state = CheckState::Running;
            run.commands[index].started_at_ms = Some(now_ms());
            run.record(
                CheckEventKind::CheckStarted,
                Some(context.definition_id.clone()),
                format!("Check started · {}", context.name),
            );
            self.update(run, sink)?;
            publication.update(run);
            if let Err(error) = self
                .execute(run, index, sink, publication, cancelled, external_cancelled)
                .await
            {
                run.commands[index].state = if cancelled.requested.load(Ordering::Acquire)
                    || external_cancelled.is_some_and(|value| value.load(Ordering::Acquire))
                {
                    CheckState::Interrupted
                } else {
                    super::failure::classify(&error, CheckState::ExecutionError)
                };
                run.commands[index].error = Some(format!("{error:#}"));
            }
            run.commands[index].completed_at_ms = Some(now_ms());
            if matches!(
                run.commands[index].state,
                CheckState::Passed | CheckState::Failed
            ) {
                let Some(after) = cancelled
                    .read(external_cancelled, self.changes.snapshot())
                    .await?
                else {
                    run.interrupt();
                    return Ok(());
                };
                if let (Some(before), Some(after)) = (before, after) {
                    run.commands[index].evidence = super::evidence::CheckEvidence::capture(
                        run,
                        &run.commands[index],
                        before,
                        &after,
                    )?;
                    if run.commands[index].state == CheckState::Passed
                        && run.commands[index].evidence.is_none()
                    {
                        run.commands[index].state = CheckState::Stale;
                        run.commands[index].error = Some("Check inputs changed during execution. Verify again after source changes settle.".into());
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
            self.update(run, sink)?;
            publication.update(run);
            if run.commands[index].state == CheckState::Interrupted {
                run.interrupt();
                return Ok(());
            }
        }
        let Some(checkpoint) = cancelled
            .read(external_cancelled, self.changes.snapshot())
            .await?
        else {
            run.interrupt();
            return Ok(());
        };
        if let Some(checkpoint) = checkpoint {
            for index in 0..run.commands.len() {
                let command = &run.commands[index];
                if command.state == CheckState::Passed
                    && let Some(evidence) = &command.evidence
                    && !evidence.covers(run, command, &checkpoint)?
                {
                    run.commands[index].state = CheckState::Stale;
                    run.commands[index].error = Some("Later source changes superseded this check's evidence. Verify current inputs again.".into());
                }
            }
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
            format!(
                "Checks finished · {} · {}ms",
                run.summary(),
                run.duration_ms()
            ),
        );
        Ok(())
    }

    async fn execute(
        &self,
        run: &mut CheckRun,
        index: usize,
        sink: Option<&BackendEventSink>,
        publication: &CheckPublication,
        cancelled: &Arc<CheckCancellation>,
        external_cancelled: Option<&Arc<AtomicBool>>,
    ) -> Result<()> {
        let context = run.commands[index].context.as_ref().unwrap().clone();
        let directory = self
            .workspace
            .join(super::config::relative_path(&context.directory)?)
            .canonicalize()
            .context("check working directory is unavailable")?;
        ensure!(
            directory.starts_with(self.workspace.canonicalize()?),
            "check directory escapes the project"
        );
        let direct = context.invocation.as_ref().filter(|_| {
            context
                .operation
                .as_ref()
                .is_some_and(|operation| operation.args.is_some())
        });
        let input = super::supervisor::operation(
            run.context
                .as_ref()
                .context("check context unavailable")?
                .snapshot
                .config
                .definitions
                .iter()
                .find(|definition| definition.id == context.definition_id)
                .context("check definition unavailable")?,
            &context.selection,
        )
        .is_some_and(|operation| operation.input);
        let scoped = input;
        let source = if scoped && run.shell == crate::session::CheckShell::Nushell {
            format!("$in | {}", run.commands[index].command)
        } else {
            run.commands[index].command.clone()
        };
        let mut command = if let Some(invocation) = direct {
            let mut command = crate::process::command(&invocation.program, &invocation.args);
            command.stdin(if scoped {
                std::process::Stdio::piped()
            } else {
                std::process::Stdio::null()
            });
            command
        } else {
            let executable = run.executable.as_ref().ok_or_else(|| {
                super::failure::CheckFailure::classified(
                    CheckState::Blocked,
                    anyhow::anyhow!("configured check shell is unavailable"),
                )
            })?;
            run.shell.command(executable, &source, scoped)?
        };
        command
            .current_dir(directory)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        let mut child = crate::process::spawn_owned(command)
            .map_err(super::failure::CheckFailure::launch)
            .context("start configured check")?;
        let input = if scoped {
            Some(
                child
                    .stdin()
                    .take()
                    .context("scoped check stdin unavailable")?,
            )
        } else {
            None
        };
        let request = serde_json::to_vec(&serde_json::json!({
            "version":1,
            "check_id":context.definition_id,
            "workspace":self.workspace,
            "inputs":context.invocation.as_ref().map(|invocation| &invocation.inputs),
            "selection":context.selection,
            "changed_files":context.matched_files,
            "receipt_path":context.receipt,
            "report_path":context.receipt.with_extension("report.json")
        }))?;
        let input_delivery = async move {
            if let Some(mut input) = input {
                input.write_all(&request).await?;
                input.shutdown().await?;
            }
            Ok::<_, std::io::Error>(())
        };
        tokio::pin!(input_delivery);
        let mut stdout = child.stdout().take().context("check stdout unavailable")?;
        let mut stderr = child.stderr().take().context("check stderr unavailable")?;
        let process_cancelled = Arc::clone(cancelled);
        let process_external = external_cancelled.cloned();
        let (stop, stopped) = tokio::sync::oneshot::channel();
        let worker = tokio::spawn(async move {
            let cancelled = async {
                tokio::select! {
                    _ = process_cancelled.wait(process_external.as_deref()) => {},
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
        let result: Result<()> = async {
        let mut output = tokio::fs::File::create(&run.commands[index].output).await?;
        let mut stdout_file = tokio::fs::File::create(&run.commands[index].stdout).await?;
        let mut stderr_file = tokio::fs::File::create(&run.commands[index].stderr).await?;
        let mut stdout_buffer = [0u8; 8192];
        let mut stderr_buffer = [0u8; 8192];
        let mut stdout_done = false;
        let mut stderr_done = false;
        let mut status = None;
        let mut killed = false;
        let mut input_done = !scoped;
        let mut ticker = tokio::time::interval(std::time::Duration::from_millis(250));
        while !stdout_done || !stderr_done || status.is_none() || !input_done {
            tokio::select! {
                result = &mut input_delivery, if !input_done => {
                    input_done = true;
                    if !killed {result.context("deliver scoped selectors")?;}
                },
                result = stdout.read(&mut stdout_buffer), if !stdout_done => {
                    let bytes = result?; stdout_done = bytes == 0;
                    if bytes > 0 { stdout_file.write_all(&stdout_buffer[..bytes]).await?; output.write_all(&stdout_buffer[..bytes]).await?;
                        run.commands[index].output_bytes += bytes as u64; append_preview(&mut run.commands[index].preview, &stdout_buffer[..bytes]); }
                },
                result = stderr.read(&mut stderr_buffer), if !stderr_done => {
                    let bytes = result?; stderr_done = bytes == 0;
                    if bytes > 0 { stderr_file.write_all(&stderr_buffer[..bytes]).await?; output.write_all(&stderr_buffer[..bytes]).await?;
                        run.commands[index].output_bytes += bytes as u64; append_preview(&mut run.commands[index].preview, &stderr_buffer[..bytes]); }
                },
                result = process.wait(), if status.is_none() => {
                    let (completed, interrupted) = result?;
                    status = Some(completed);
                    killed = interrupted;
                },
                _ = ticker.tick() => {
                    output.flush().await?;
                    self.update(run, sink)?;
                    publication.update(run);
                },
            }
        }
        output.flush().await?;
        stdout_file.flush().await?;
        stderr_file.flush().await?;
        let status = status.context("check process status missing")?;
        run.commands[index].exit_code = status.code();
        run.commands[index].state = if killed {
            CheckState::Interrupted
        } else if status.success() {
            CheckState::Passed
        } else {
            CheckState::Failed
        };
        if scoped && !killed && status.success() {
            let receipt = (|| -> Result<CheckReceipt> {
                let source = std::fs::read(&context.receipt)
                    .context("scoped check did not produce a selection receipt; fix its definition")?;
                let mut receipt: CheckReceipt = serde_json::from_slice(&source).context("invalid selection receipt")?;
                receipt.validate_request(&context.selection)?;
                Ok(receipt)
            })().map_err(|error| super::failure::CheckFailure::classified(CheckState::ConfigurationError, error))?;
            run.commands[index].context.as_mut().unwrap().resolved = Some(receipt.selection);
        } else if !scoped && !killed && status.success() {
            run.commands[index].context.as_mut().unwrap().resolved = Some(context.selection.clone());
        }
        let command = run.commands[index].clone();
        run.commands[index].report = tokio::task::spawn_blocking(move || super::report::collect(&command))
            .await.context("collect check result report")?;
        if let Some(report) = &run.commands[index].report {
            if let Some(error) = &report.error {
                return Err(super::failure::CheckFailure::classified(
                    CheckState::ConfigurationError, anyhow::anyhow!("{error}")
                ));
            }
            if run.commands[index].state == CheckState::Passed
                && report.result.iter().any(|result| !matches!(result.state, CheckState::Passed | CheckState::Skipped))
            {
                return Err(super::failure::CheckFailure::classified(
                    CheckState::Failed, anyhow::anyhow!("callback reported a failed result")
                ));
            }
        }
        Ok(())
        }.await;
        if result.is_err() {
            process.stop().await?;
        }
        result
    }

    /// Persists a completed external update before publishing its live state.
    pub(crate) async fn publish(
        &self,
        run: &CheckRun,
        sink: Option<&BackendEventSink>,
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

    fn update(&self, run: &CheckRun, sink: Option<&BackendEventSink>) -> Result<()> {
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
                .publish(sink, patch.notification());
        }
        if let (Some(error), Some(sink)) = (error, sink) {
            self.notification
                .get_or_init(|| CheckNotification::new(self.trace.clone(), self.session_id.clone()))
                .error(
                    sink,
                    BackendEvent {
                        received_at_ms: None,
                        address: None,
                        turn_boundary: None,
                        kind: "checks_configuration_error".into(),
                        text: None,
                        data: serde_json::json!({"run_id":run.id,"error":error.text}),
                        activity: None,
                        summary: None,
                        task_update: None,
                    },
                );
        }
        Ok(())
    }

    /// Starts an owned check task without waiting for progress delivery.
    /// Retains an optional source-work permit until the process is collected.
    pub(crate) async fn start(
        self: &Arc<Self>,
        run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
        admission: Option<tokio::sync::OwnedSemaphorePermit>,
    ) -> Result<String> {
        let (id, _) = self
            .launch(run, sink, external_cancelled, admission)
            .await?;
        Ok(id)
    }

    async fn launch(
        self: &Arc<Self>,
        run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
        admission: Option<tokio::sync::OwnedSemaphorePermit>,
    ) -> Result<(
        String,
        tokio::sync::oneshot::Receiver<std::result::Result<CheckRun, String>>,
    )> {
        let run = self.resolve(run).await?;
        std::fs::create_dir_all(self.root.join(&run.id))?;
        let cancelled = self.admit(&run, admission)?;
        let id = run.id.clone();
        if let Err(error) = self.update(&run, sink.as_ref()) {
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
                .run_admitted(run, sink, external_cancelled, cancelled)
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

pub(super) fn operation<'definition>(
    definition: &'definition CheckDefinition,
    selection: &CheckSelection,
) -> Option<&'definition super::CheckOperation> {
    match selection {
        CheckSelection::All => Some(&definition.all),
        CheckSelection::Inputs { .. } => Some(&definition.all),
        CheckSelection::Files { .. } => definition.files.as_ref(),
        CheckSelection::Cases { .. } => definition.cases.as_ref(),
    }
}

/// Atomically replaces one run record on a blocking persistence worker.
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
    std::fs::write(
        &temporary,
        crate::codec::to_vec(
            &serde_json::json!({"version":CHECK_HISTORY_VERSION,"run":crate::codec::to_value(run)?}),
        )?,
    )?;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn cancellation_releases_a_pending_checkpoint_read() -> Result<()> {
        let cancellation = CheckCancellation {
            requested: Arc::new(AtomicBool::new(false)),
            changed: Default::default(),
        };
        let read = cancellation.read(None, std::future::pending::<Result<()>>());
        tokio::pin!(read);
        assert!(futures_util::poll!(read.as_mut()).is_pending());
        cancellation.cancel();
        assert!(
            tokio::time::timeout(std::time::Duration::from_secs(1), read)
                .await??
                .is_none()
        );
        Ok(())
    }
}
