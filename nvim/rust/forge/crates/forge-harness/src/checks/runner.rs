use super::model::{CheckCommand, CheckEventKind, CheckRunContext};
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
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

/// Owns project check admission, durable run history, and process cancellation for one session.
pub struct CheckRunner {
    pub(crate) workspace: PathBuf,
    pub(crate) root: PathBuf,
    pub(super) changes: super::changes::CheckChanges,
    presentation: Arc<Mutex<SessionPresentation>>,
    records: Mutex<BTreeMap<String, CheckRun>>,
    active: Mutex<Option<(String, Arc<AtomicBool>)>>,
    changed: tokio::sync::Notify,
    notified: Mutex<std::collections::BTreeSet<String>>,
}

/// Captures task authority and concrete selectors before command admission.
pub(crate) struct CheckAdmission {
    pub request: RunRequest,
    pub overrides: BTreeMap<String, bool>,
    pub files: Vec<String>,
    /// Distinguishes an accepted empty case inventory from unavailable standalone discovery.
    pub cases: Option<Vec<CheckCase>>,
    pub has_scope: bool,
    pub shell: crate::session::CheckShell,
    pub trigger: String,
    pub task_id: Option<String>,
    pub revision: u32,
}

impl CheckRunner {
    /// Reopens current-format history and marks detached execution interrupted without replaying commands.
    pub fn open(
        workspace: PathBuf,
        root: PathBuf,
        presentation: Arc<Mutex<SessionPresentation>>,
        objects: crate::storage::objects::ObjectStore,
        repositories: Arc<forge_git::store::RepositoryStore>,
    ) -> Result<Arc<Self>> {
        std::fs::create_dir_all(&root)?;
        let changes = super::changes::CheckChanges::open(&workspace, &root, objects, repositories)?;
        let mut records = BTreeMap::new();
        for mut run in Self::saved_history(&root)? {
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
                write_record(&path, &run)?;
            }
            records.insert(run.id.clone(), run);
        }
        Ok(Arc::new(Self {
            workspace,
            root,
            changes,
            presentation,
            records: Mutex::new(records),
            active: Mutex::new(None),
            changed: Default::default(),
            notified: Default::default(),
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
            let value: serde_json::Value = serde_json::from_slice(&std::fs::read(path)?)?;
            if value["version"] != 2 {
                continue;
            }
            let run: CheckRun = serde_json::from_value(value["run"].clone())?;
            ensure!(
                directory.file_name().to_string_lossy() == run.id,
                "check history identity differs from its directory"
            );
            records.push(run);
        }
        records.sort_by_key(|run| run.started_at_ms);
        Ok(records)
    }

    /// Lists persisted runs chronologically, including settled results from earlier hosts.
    pub fn history(&self) -> Result<Vec<CheckRun>> {
        let mut records: Vec<_> = self
            .records
            .lock()
            .map_err(|_| anyhow::anyhow!("check history lock poisoned"))?
            .values()
            .cloned()
            .collect();
        records.sort_by_key(|run| run.started_at_ms);
        Ok(records)
    }

    pub(crate) fn active(&self) -> Result<Option<String>> {
        Ok(self
            .active
            .lock()
            .map_err(|_| anyhow::anyhow!("check admission lock poisoned"))?
            .as_ref()
            .map(|(id, _)| id.clone()))
    }

    /// Reads one owned run, optionally waiting up to thirty seconds for a state update.
    pub async fn status(&self, id: &str, wait_ms: u64) -> Result<CheckRun> {
        let read = || {
            self.records
                .lock()
                .map_err(|_| anyhow::anyhow!("check history lock poisoned"))?
                .get(id)
                .cloned()
                .context("unknown check run")
        };
        let waiting = self.changed.notified();
        tokio::pin!(waiting);
        waiting.as_mut().enable();
        let run = read()?;
        if run.state == CheckState::Running && wait_ms > 0 {
            let _ = tokio::time::timeout(
                std::time::Duration::from_millis(wait_ms.min(30000)),
                waiting,
            )
            .await;
            return read();
        }
        Ok(run)
    }

    /// Requests cancellation without releasing process ownership before collection finishes.
    pub fn cancel(&self, id: &str) -> Result<()> {
        let active = self
            .active
            .lock()
            .map_err(|_| anyhow::anyhow!("check admission lock poisoned"))?;
        let (active_id, cancelled) = active.as_ref().context("no check run is active")?;
        ensure!(
            active_id == id,
            "check run is not owned by this active request"
        );
        cancelled.store(true, Ordering::Release);
        Ok(())
    }

    /// Builds exact commands from saved definitions and task selectors before acquiring execution ownership.
    pub(crate) fn prepare(
        &self,
        snapshot: ConfigSnapshot,
        admission: CheckAdmission,
    ) -> Result<CheckRun> {
        let CheckAdmission {
            request,
            overrides,
            files,
            cases,
            has_scope,
            shell,
            trigger,
            task_id,
            revision,
        } = admission;
        let mut effective = snapshot.config.clone();
        for definition in &mut effective.definitions {
            if let Some(enabled) = overrides.get(&definition.id) {
                definition.enabled = *enabled;
            }
        }
        let id = uuid::Uuid::new_v4().to_string();
        let directory = self.root.join(&id);
        std::fs::create_dir_all(&directory)?;
        let mut run = CheckRun {
            id: id.clone(),
            revision,
            context: Some(CheckRunContext {
                trigger,
                task_id,
                snapshot,
                overrides,
                changed_files: files.clone(),
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
            state: CheckState::Running,
            progress: Default::default(),
            workspace_identity: None,
            workspace_digest: BTreeMap::new(),
            commands: Vec::new(),
        };
        let applicability = super::applicability::resolve(&request, &effective, &files, has_scope)?;
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
            effective_request.selections(&effective, &files, cases.as_deref(), has_scope)?
        };
        for index in effective.order()? {
            let definition = &effective.definitions[index];
            let applicability = applicability
                .iter()
                .find(|item| item.id == definition.id)
                .unwrap();
            ensure!(
                definition.enabled
                    || request.ids.is_empty()
                    || !request.ids.contains(&definition.id),
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
                        "Explicit check selection is empty. Supply concrete files or cases.".into(),
                    ),
                )
            } else if selection.empty()
                && matches!(selection, CheckSelection::Cases { .. })
                && cases.is_none()
            {
                (CheckState::ConfigurationError, Some("Added/modified case inventory is unavailable outside plan execution. Use changed_files or all in /checks setup, or supply explicit cases.".into()))
            } else if selection.empty() {
                (CheckState::Skipped, Some("No matching task changes".into()))
            } else if operation.is_none() {
                (
                    CheckState::ConfigurationError,
                    Some("Requested scope is unsupported. Fix this check in /checks setup.".into()),
                )
            } else {
                (CheckState::Pending, None)
            };
            let position = run.commands.len();
            run.commands.push(CommandCheck {
                id: format!("{id}:{position}"),
                command: operation.unwrap_or(&definition.all).command.clone(),
                context: Some(CheckCommand {
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
            });
        }
        run.record(
            CheckEventKind::Started,
            None,
            format!(
                "Checks started · {} · {} selected",
                run.context.as_ref().unwrap().trigger,
                requested.len()
            ),
        );
        Ok(run)
    }

    /// Runs the same serial dependency-aware lifecycle for automatic gates and on-demand requests.
    pub(crate) async fn run(
        self: &Arc<Self>,
        run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
    ) -> Result<CheckRun> {
        let cancelled = self.admit(&run.id)?;
        self.run_admitted(run, sink, external_cancelled, cancelled)
            .await
    }

    fn admit(&self, id: &str) -> Result<Arc<AtomicBool>> {
        let mut active = self
            .active
            .lock()
            .map_err(|_| anyhow::anyhow!("check admission lock poisoned"))?;
        ensure!(active.is_none(), "another check run is already active");
        let cancelled = Arc::new(AtomicBool::new(false));
        *active = Some((id.into(), Arc::clone(&cancelled)));
        Ok(cancelled)
    }

    async fn run_admitted(
        self: &Arc<Self>,
        mut run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
        cancelled: Arc<AtomicBool>,
    ) -> Result<CheckRun> {
        let result = self
            .run_inner(
                &mut run,
                sink.as_ref(),
                &cancelled,
                external_cancelled.as_deref(),
            )
            .await;
        if let Err(error) = &result {
            run.state = CheckState::ConfigurationError;
            run.record(
                CheckEventKind::ConfigurationError,
                None,
                format!("Check configuration error: {error:#}"),
            );
            run.completed_at_ms = Some(now_ms());
        }
        let publication = self.publish(&run, sink.as_ref()).await;
        *self
            .active
            .lock()
            .map_err(|_| anyhow::anyhow!("check admission lock poisoned"))? = None;
        publication?;
        result?;
        Ok(run)
    }

    async fn run_inner(
        &self,
        run: &mut CheckRun,
        sink: Option<&BackendEventSink>,
        cancelled: &AtomicBool,
        external_cancelled: Option<&AtomicBool>,
    ) -> Result<()> {
        self.publish(run, sink).await?;
        for index in 0..run.commands.len() {
            if cancelled.load(Ordering::Acquire)
                || external_cancelled.is_some_and(|value| value.load(Ordering::Acquire))
            {
                run.state = CheckState::Interrupted;
                run.record(
                    CheckEventKind::Interrupted,
                    None,
                    "Checks interrupted".into(),
                );
                return Ok(());
            }
            let context = run.commands[index].context.as_ref().unwrap().clone();
            if run.commands[index].state == CheckState::Pending {
                for dependency in &context.depends_on {
                    let prerequisite = run
                        .commands
                        .iter()
                        .find(|check| {
                            check
                                .context
                                .as_ref()
                                .is_some_and(|value| &value.definition_id == dependency)
                        })
                        .context("missing prerequisite command")?;
                    if prerequisite.state != CheckState::Passed {
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
                CheckState::Skipped | CheckState::ConfigurationError => {
                    let kind = if run.commands[index].state == CheckState::Skipped {
                        CheckEventKind::CheckSkipped
                    } else {
                        CheckEventKind::ConfigurationError
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
                    self.publish(run, sink).await?;
                    continue;
                }
                CheckState::Passed | CheckState::Failed => continue,
                _ => {}
            }
            run.commands[index].state = CheckState::Running;
            run.commands[index].started_at_ms = Some(now_ms());
            run.record(
                CheckEventKind::CheckStarted,
                Some(context.definition_id.clone()),
                format!("Check started · {}", context.name),
            );
            self.publish(run, sink).await?;
            if let Err(error) = self
                .execute(run, index, sink, cancelled, external_cancelled)
                .await
            {
                run.commands[index].state = CheckState::ConfigurationError;
                run.commands[index].error = Some(format!("{error:#}"));
            }
            run.commands[index].completed_at_ms = Some(now_ms());
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
            self.publish(run, sink).await?;
            if run.commands[index].state == CheckState::Interrupted {
                run.state = CheckState::Interrupted;
                run.record(
                    CheckEventKind::Interrupted,
                    None,
                    "Checks interrupted".into(),
                );
                return Ok(());
            }
        }
        run.completed_at_ms = Some(now_ms());
        run.state = if run
            .commands
            .iter()
            .any(|check| check.state == CheckState::ConfigurationError)
        {
            CheckState::ConfigurationError
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
        cancelled: &AtomicBool,
        external_cancelled: Option<&AtomicBool>,
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
        let executable = run
            .executable
            .as_ref()
            .context("configured check shell is unavailable")?;
        let scoped = !matches!(context.selection, CheckSelection::All);
        let source = if scoped && run.shell == crate::session::CheckShell::Nushell {
            format!("$in | {}", run.commands[index].command)
        } else {
            run.commands[index].command.clone()
        };
        let mut command = run.shell.command(executable, &source)?;
        command
            .current_dir(directory)
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        if scoped {
            command.stdin(std::process::Stdio::piped());
            if run.shell == crate::session::CheckShell::Nushell {
                command.arg("--stdin");
            }
        }
        let mut child = crate::process::spawn_owned(command).context("start configured check")?;
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
        let request = serde_json::to_vec(
            &serde_json::json!({"version":1,"check_id":context.definition_id,"workspace":self.workspace,"selection":context.selection,"receipt_path":context.receipt}),
        )?;
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
                result = child.wait(), if status.is_none() => { status = Some(result?); },
                _ = ticker.tick() => {
                    if !killed && (cancelled.load(Ordering::Acquire) || external_cancelled.is_some_and(|value| value.load(Ordering::Acquire))) {
                        child.start_kill()?; killed = true;
                    }
                    output.flush().await?;
                    self.publish(run, sink).await?;
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
        if scoped && !killed {
            let source = std::fs::read(&context.receipt)
                .context("scoped check did not produce a selection receipt; fix its definition")?;
            ensure!(
                source.len() <= 1024 * 1024,
                "selection receipt exceeds 1 MiB"
            );
            let mut receipt: CheckReceipt =
                serde_json::from_slice(&source).context("invalid selection receipt")?;
            receipt.validate_request(&context.selection)?;
            run.commands[index].context.as_mut().unwrap().resolved = Some(receipt.selection);
        }
        Ok(())
    }

    pub(crate) async fn publish(
        &self,
        run: &CheckRun,
        sink: Option<&BackendEventSink>,
    ) -> Result<()> {
        let path = self.root.join(&run.id).join("record.json");
        std::fs::create_dir_all(path.parent().unwrap())?;
        write_record(&path, run)?;
        self.records
            .lock()
            .map_err(|_| anyhow::anyhow!("check history lock poisoned"))?
            .insert(run.id.clone(), run.clone());
        self.changed.notify_waiters();
        let patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("check presentation lock poisoned"))?
            .update_checks(run)?;
        if let Some(sink) = sink {
            if !patch.is_empty() {
                let _ = sink
                    .send_wait(BackendEvent {
                        received_at_ms: None,
                        address: None,
                        turn_boundary: None,
                        kind: "document_changed".into(),
                        text: None,
                        data: patch.notification(),
                        activity: None,
                        summary: None,
                        task_update: None,
                    })
                    .await;
            }
            let notification = run
                .lifecycle
                .last()
                .filter(|event| matches!(event.kind, CheckEventKind::ConfigurationError));
            let notify = if let Some(event) = notification {
                self.notified
                    .lock()
                    .map_err(|_| anyhow::anyhow!("check notification lock poisoned"))?
                    .insert(event.id.clone())
            } else {
                false
            };
            if notify {
                let _ = sink.send_wait(BackendEvent { received_at_ms:None,address:None,turn_boundary:None,kind:"checks_configuration_error".into(),text:None,
                    data:serde_json::json!({"run_id":run.id,"error":run.lifecycle.last().unwrap().text}),activity:None,summary:None,task_update:None }).await;
            }
        }
        Ok(())
    }

    /// Starts a durable background run without keeping the provider tool call open for compilation.
    pub(crate) async fn start(
        self: &Arc<Self>,
        run: CheckRun,
        sink: Option<BackendEventSink>,
        external_cancelled: Option<Arc<AtomicBool>>,
    ) -> Result<String> {
        let cancelled = self.admit(&run.id)?;
        let id = run.id.clone();
        if let Err(error) = self.publish(&run, sink.as_ref()).await {
            *self
                .active
                .lock()
                .map_err(|_| anyhow::anyhow!("check admission lock poisoned"))? = None;
            return Err(error);
        }
        let runner = Arc::clone(self);
        tokio::spawn(async move {
            let _ = runner
                .run_admitted(run, sink, external_cancelled, cancelled)
                .await;
        });
        Ok(id)
    }
}

fn operation<'definition>(
    definition: &'definition CheckDefinition,
    selection: &CheckSelection,
) -> Option<&'definition super::CheckOperation> {
    match selection {
        CheckSelection::All => Some(&definition.all),
        CheckSelection::Files { .. } => definition.files.as_ref(),
        CheckSelection::Cases { .. } => definition.cases.as_ref(),
    }
}

fn write_record(path: &Path, run: &CheckRun) -> Result<()> {
    let temporary = path.with_extension("tmp");
    std::fs::write(
        &temporary,
        serde_json::to_vec(&serde_json::json!({"version":2,"run":run}))?,
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
