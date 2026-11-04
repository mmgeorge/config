//! Capture execution progress without borrowing the broker and commit only current evidence.

use super::*;

/// Own the accepted design, observation fence, and workspace inputs for a completed turn.
pub(crate) struct PlanProgressJob {
    files: Arc<crate::plan::PlanFileStore>,
    session_id: String,
    workspace: PathBuf,
    before: crate::plan::PlanExecutionRecord,
    permit: crate::task::TaskPermit,
    previous: Option<crate::checkpoint::CheckpointRecord>,
    capture: execution::WorkspaceCaptureJob,
    commands: Vec<CommandEvidence>,
}

struct CommandEvidence {
    title: String,
    output: crate::turn::ToolOutputText,
}

/// Retain the captured authority with a fully computed progress observation.
pub(crate) struct PlanProgressResult {
    session_id: String,
    workspace: PathBuf,
    before: crate::plan::PlanExecutionRecord,
    execution: crate::plan::PlanExecutionRecord,
    permit: crate::task::TaskPermit,
    document: crate::plan::PlanDocument,
    checkpoint: Option<crate::checkpoint::CheckpointRecord>,
    changed: bool,
}

impl PlanProgressJob {
    /// Scan the accepted design and command evidence independently of broker ownership.
    pub(crate) async fn run(self) -> Result<PlanProgressResult> {
        let Self {
            files,
            session_id,
            workspace,
            before,
            permit,
            previous,
            capture,
            commands,
        } = self;
        let scan_files = files.clone();
        let scan_session = session_id.clone();
        let scan_workspace = workspace.clone();
        let scan_execution = before.clone();
        let (progress, document) = tokio::task::spawn_blocking(move || -> Result<_> {
            let submitted = scan_files.read_submitted_document(
                &scan_session,
                &scan_execution.plan_id,
                scan_execution.revision,
            )?;
            let design = submitted
                .design
                .as_ref()
                .context("execution requires a semantic design")?;
            let progress = if scan_execution.phase == crate::plan::PlanPhase::Verify {
                crate::plan::execution::SemanticProgress::scan(design, &scan_workspace)?
            } else {
                let mut progress = crate::plan::execution::SemanticProgress::default();
                for path in design.proposed.keys() {
                    if let Some(source) = crate::plan::workspace_source(&scan_workspace, path)? {
                        progress
                            .source_digest
                            .insert(path.clone(), crate::plan::digest(source.as_bytes()));
                    }
                }
                progress
            };
            Ok((
                progress,
                scan_files.read_working_document(&scan_session, &scan_execution.plan_id)?,
            ))
        })
        .await
        .context("execution progress scan worker failed")??;
        let checkpoint = if previous.is_some() {
            Some(capture.run().await?)
        } else {
            None
        };
        let mut changed = previous
            .as_ref()
            .zip(checkpoint.as_ref())
            .map(|(before, after)| before.changed_paths(after))
            .transpose()?
            .is_some_and(|paths| !paths.is_empty());
        let mut execution = before.clone();
        if let Some(checkpoint) = &checkpoint {
            execution.observed_checkpoint = Some(checkpoint.id.clone());
        }
        changed |= execution.observed_document_version != 0
            && execution.observed_document_version != document.version;
        if execution.phase == crate::plan::PlanPhase::Verify {
            let source = serde_json::to_string(&progress.source_digest)?;
            for command in commands {
                let identity = crate::plan::digest(
                    format!("{}\n{}\n{source}", command.title, command.output).as_bytes(),
                );
                changed |= execution.observed_check.insert(identity);
            }
        }
        execution.observed_source = progress.source_digest.clone();
        execution.observed_document_version = document.version;
        execution.progress = progress;
        if execution.baseline_checkpoint.is_none() {
            execution
                .progress
                .warning
                .push(execution::CHECKPOINT_WARNING.into());
        }
        Ok(PlanProgressResult {
            session_id,
            workspace,
            before,
            execution,
            permit,
            document,
            checkpoint,
            changed,
        })
    }
}

impl HarnessBroker {
    /// Capture the exact execution observation before scanning a completed turn.
    pub(crate) fn prepare_plan_progress(
        &self,
        interaction: &Exchange,
    ) -> Result<Option<PlanProgressJob>> {
        let Some(execution::ExecutionCapture { permit, record }) = self.capture_execution()? else {
            return Ok(None);
        };
        let previous = record
            .observed_checkpoint
            .as_ref()
            .or(record.baseline_checkpoint.as_ref())
            .map(|id| {
                self.store
                    .load_checkpoint(id)?
                    .context("execution progress checkpoint is missing")
            })
            .transpose()?;
        let commands = interaction
            .turn
            .iter()
            .flat_map(|turn| interaction.tools_for_turn(turn.id()))
            .filter(|tool| {
                matches!(tool.state(), crate::turn::ToolState::Completed) && tool.kind == "command"
            })
            .map(|tool| CommandEvidence {
                title: tool.title.clone(),
                output: tool.output.clone(),
            })
            .collect();
        Ok(Some(PlanProgressJob {
            files: self.plan_file.clone(),
            session_id: self.session.id.clone(),
            workspace: PathBuf::from(&self.session.workspace),
            before: record,
            permit,
            previous,
            capture: self.prepare_workspace_capture(),
            commands,
        }))
    }

    /// Commit progress only while task, execution observations, and working document remain current.
    pub(crate) fn commit_plan_progress(&mut self, result: PlanProgressResult) -> Result<bool> {
        let PlanProgressResult {
            session_id,
            workspace,
            before,
            execution,
            permit,
            document,
            checkpoint,
            changed,
        } = result;
        anyhow::ensure!(
            session_id == self.session.id
                && workspace.as_path() == Path::new(&self.session.workspace),
            "execution progress crossed session ownership"
        );
        let current = self
            .capture_execution()?
            .context("execution disappeared during progress capture")?;
        permit.validate(self.task_controller.current()?.as_ref())?;
        let record = &current.record;
        if !(record.id == before.id
            && record.generation == before.generation
            && record.revision == before.revision
            && record.phase == before.phase
            && record.state == before.state
            && record.baseline_checkpoint == before.baseline_checkpoint
            && record.observed_checkpoint == before.observed_checkpoint
            && record.observed_document_version == before.observed_document_version
            && record.observed_check == before.observed_check
            && record.observed_source == before.observed_source)
        {
            return Err(crate::task::StaleTask.into());
        }
        let effects = crate::storage::TaskEffects {
            checkpoint: checkpoint.into_iter().collect(),
            ..Default::default()
        };
        self.task_controller.apply_with_plan_document(
            &mut self.store,
            &permit,
            self.plan_file.guard_working_document(&document),
            crate::task::TaskEvent::ExecutionUpdated(Box::new(execution)),
            effects,
            self.clock.now_ms(),
        )?;
        Ok(changed)
    }
}
