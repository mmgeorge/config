use super::*;

pub(crate) struct InputReadiness {
    pub session: crate::storage::admission::AdmissionSession,
    pub task: Option<crate::storage::admission::AdmissionTask>,
    pub operation_state: Option<String>,
    pub has_elicitation: bool,
    pub has_approval: bool,
    pub checks_running: bool,
    pub native_steer: bool,
}

/// Shares durable queries and published transient state without acquiring the execution owner.
pub(crate) struct BrokerObserver {
    pub(crate) admission: Arc<crate::storage::admission::SessionAdmission>,
    pub(crate) input_schedule: tokio::sync::watch::Sender<crate::session::InputSchedule>,
    pub(super) lsp: Arc<crate::control_tools::lsp::LspService>,
    pub(crate) status_thoughts: AtomicBool,
    pub(crate) check_editing: Arc<AtomicBool>,
    pub(crate) completion_queue: crate::checks::CompletionQueue,
    pub(super) data_root: PathBuf,
    pub(super) session_id: String,
    pub(super) session_metadata: crate::storage::lifecycle::SessionMetadataWriter,
    pub(super) runtime_epoch: String,
    pub(super) snapshot_revision: AtomicU64,
    pub(super) metadata: RwLock<ObservationMetadata>,
    pub(super) plan_file: Arc<PlanFileStore>,
    pub(super) permission_coordinator: Arc<PermissionCoordinator>,
    pub(super) permission_store: Arc<RwLock<PermissionStore>>,
    pub(super) agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    pub(super) check_runner: Arc<crate::checks::CheckSupervisor>,
    pub(super) trace: Arc<TraceStore>,
}

#[derive(Clone)]
pub(super) struct ObservationMetadata {
    pub revision: u64,
    pub capability: BackendCapability,
    pub active_wait: Option<ActiveWait>,
    pub no_checkpoint: bool,
}

impl BrokerObserver {
    pub(crate) fn input_readiness(&self) -> Result<InputReadiness> {
        let state = self.admission.snapshot()?;
        let mut has_approval = false;
        for approval in self.permission_coordinator.pending_list()? {
            if let Some(id) = approval.exchange_id {
                has_approval |= self.admission.owns_exchange(&id)?;
            }
        }
        Ok(InputReadiness {
            session: state.session.clone(),
            task: state.task.clone(),
            has_elicitation: state.has_elicitation,
            has_approval,
            operation_state: state.operation_state.clone(),
            checks_running: self.check_runner.active()?.is_some(),
            native_steer: self
                .metadata
                .read()
                .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?
                .capability
                .native_steer,
        })
    }
    pub(crate) fn session_id(&self) -> &str {
        &self.session_id
    }
    /// Returns the selected task from the same committed projection used by dispatch.
    pub(crate) fn input_task_id(&self) -> Result<Option<String>> {
        Ok(self.admission.snapshot()?.session.current_task_id.clone())
    }
    /// Renames the owning session without waiting for the provider turn.
    pub(super) fn rename_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        anyhow::ensure!(
            session_id == self.session_id,
            "session rename must target the owning broker"
        );
        let name = required_text(&params, "name")?.trim().to_owned();
        let session = self.session_metadata.rename(
            &name,
            params.get("expected_name").and_then(Value::as_str),
            SystemClock.now_ms(),
        )?;
        Ok((serde_json::to_value(session)?, Vec::new()))
    }

    /// Deletes an inactive session while retaining its ownership lock through cleanup.
    pub(super) fn delete_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        anyhow::ensure!(
            session_id != self.session_id,
            "cannot delete the active session"
        );
        let mut store = SqliteStore::open(&self.data_root)?;
        let session = store
            .load_session(&session_id)?
            .context("session not found")?;
        store.delete_session_with(&session_id, || self.plan_file.delete_session(&session.id))?;
        Ok((
            json!({ "deleted": session_id, "provider_session_preserved": true }),
            Vec::new(),
        ))
    }

    /// Persists an annotation independently of provider execution after checking its session.
    pub(super) fn save_exchange_comment(
        &self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut store = SqliteStore::open(&self.data_root)?;
        let mut comment: ExchangeComment = serde_json::from_value(params)?;
        anyhow::ensure!(
            store
                .load_exchange(&comment.exchange_id)?
                .is_some_and(|interaction| interaction.session_id == self.session_id),
            "interaction comment does not belong to the active session"
        );
        comment.created_at_ms = SystemClock.now_ms();
        store.save_exchange_comment(&comment)?;
        Ok((serde_json::to_value(comment)?, Vec::new()))
    }

    /// Reads an immutable submitted declaration without acquiring execution ownership.
    pub(super) fn read_plan_declaration(
        &self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let plan_id = required_text(&params, "plan_id")?;
        let path = required_text(&params, "path")?;
        let revision: u32 =
            serde_json::from_value(params.get("revision").context("missing revision")?.clone())?;
        let baseline = params
            .get("baseline")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        let store = SqliteStore::read_only(&self.data_root)?;
        let plan = store
            .load_plan(&plan_id)?
            .context("plan artifact not found")?;
        anyhow::ensure!(
            plan.session_id == self.session_id,
            "plan belongs to another session"
        );
        let document =
            self.plan_file
                .read_submitted_document(&self.session_id, &plan_id, revision)?;
        let design = document
            .design
            .context("plan has no declaration snapshot")?;
        let text = if params
            .get("document")
            .and_then(Value::as_bool)
            .unwrap_or(false)
        {
            design
                .document
                .sections()
                .into_iter()
                .find(|section| section.title == path)
                .context("unknown plan overview section")?
                .text
        } else if baseline {
            crate::plan::calls::present(
                &path,
                &design
                    .baseline
                    .get(&path)
                    .context("declaration baseline not found")?
                    .text,
                design
                    .baseline_calls
                    .get(&path)
                    .map(Vec::as_slice)
                    .unwrap_or_default(),
            )?
            .declaration
            .text
            .clone()
        } else {
            crate::plan::calls::present(
                &path,
                design
                    .proposed
                    .get(&path)
                    .context("proposed declaration not found")?,
                design
                    .proposed_calls
                    .get(&path)
                    .map(Vec::as_slice)
                    .unwrap_or_default(),
            )?
            .declaration
            .text
            .clone()
        };
        Ok((
            json!({ "text": text, "path": path, "revision": revision, "baseline": baseline }),
            Vec::new(),
        ))
    }

    /// Persists conversation-scoped check editing without starting or interrupting a turn.
    pub(super) fn set_check_editing(&self, enabled: bool) -> Result<Value> {
        let session = SqliteStore::read_only(&self.data_root)?
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let snapshot = settings.get(&session.workspace, &session.id, &session.backend)?;
        let mut desired = settings.layer_settings(
            crate::settings::SettingScope::Session,
            &session.workspace,
            &session.id,
        )?;
        desired.check_editing = Some(enabled);
        settings.update(
            snapshot.revision,
            crate::settings::SettingScope::Session,
            &session.workspace,
            &session.id,
            &desired,
        )?;
        self.check_editing.store(enabled, Ordering::Release);
        Ok(json!({"check_editing": enabled}))
    }

    /// Reads current durable state together with the last published runtime metadata.
    pub(crate) fn snapshot(&self) -> Result<BrokerSnapshot> {
        let revision = self.snapshot_revision.fetch_add(1, Ordering::Relaxed) + 1;
        let store = SqliteStore::read_only(&self.data_root)?;
        store.read_snapshot(|| {
            let session = store
                .load_session(&self.session_id)?
                .context("observed session is missing")?;
            let metadata = self
                .metadata
                .read()
                .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?
                .clone();
            let agents = load_agent_registry(&store, &session.id)?.list();
            self.build(&store, &session, &metadata, agents, revision)
        })
    }

    pub(crate) fn runtime_epoch(&self) -> &str {
        &self.runtime_epoch
    }

    pub(super) fn publish(&self, broker: &HarnessBroker) -> Result<BrokerSnapshot> {
        let revision = self.snapshot_revision.fetch_add(1, Ordering::Relaxed) + 1;
        let metadata = ObservationMetadata {
            revision,
            capability: broker.capability.clone(),
            active_wait: broker.active_wait_projection.clone(),
            no_checkpoint: matches!(broker.workspace_kind, WorkspaceKind::Untracked(_)),
        };
        let mut session = broker.session.clone();
        session.name = broker
            .store
            .load_session(&session.id)?
            .context("observed session is missing")?
            .name;
        let result = self.build(
            &broker.store,
            &session,
            &metadata,
            broker.agent_registry.list(),
            revision,
        )?;
        if broker.session.id == self.session_id {
            let mut published = self
                .metadata
                .write()
                .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
            if metadata.revision >= published.revision {
                *published = metadata;
            }
        }
        Ok(result)
    }
    fn build(
        &self,
        store: &SqliteStore,
        source_session: &HarnessSession,
        metadata: &ObservationMetadata,
        agent_run_list: Vec<Agent>,
        revision: u64,
    ) -> Result<BrokerSnapshot> {
        let status_thoughts = self.status_thoughts.load(Ordering::Relaxed);
        let mut session = source_session.clone();
        let snapshot = crate::settings::SettingsStore::read_only(&self.data_root)?.get(
            &session.workspace,
            &session.id,
            &session.backend,
        )?;
        let ask_model = snapshot.ask_model()?;
        let settings = snapshot.desired;
        let check_timing = settings.check_timing.unwrap_or_default();
        let automatic_checks = settings.automatic_checks_enabled();
        (session.approval_timeout, session.answer_timeout) =
            self.permission_coordinator.input_timeouts(&session.id)?;
        let goal = source_session
            .goal_id
            .as_deref()
            .map(|id| store.load_goal(id))
            .transpose()?
            .flatten();
        let task_list = store.list_task(&source_session.id)?;
        let task = task_list
            .iter()
            .find(|task| Some(&task.id) == source_session.current_task_id.as_ref());
        let active_plan = task
            .as_ref()
            .and_then(|task| task.plan_id.as_deref())
            .map(|id| store.load_plan(id))
            .transpose()?
            .flatten();
        if let Some(plan) = active_plan.as_ref() {
            self.trace.record(
                &source_session.id,
                "snapshot_plan_state",
                plan_trace_fields(plan),
            );
        }
        let interaction = store.list_exchange_metadata(&source_session.id)?;
        let plan_input = active_plan.as_ref().filter(|_| {
            !task
                .as_ref()
                .is_some_and(|task| task.status == crate::task::TaskStatus::Paused)
        });
        let active_elicitation = plan_input
            .and_then(|plan| {
                plan.acceptance
                    .as_ref()
                    .map(|acceptance| ActiveElicitation {
                        owner: ElicitationOwner::PlanAcceptance,
                        plan_id: Some(plan.id.clone()),
                        exchange_id: None,
                        elicitation: acceptance.elicitation.clone(),
                    })
            })
            .or_else(|| {
                plan_input.and_then(|plan| {
                    plan.elicitation
                        .clone()
                        .map(|elicitation| ActiveElicitation {
                            owner: ElicitationOwner::Plan,
                            plan_id: Some(plan.id.clone()),
                            exchange_id: None,
                            elicitation,
                        })
                })
            })
            .or_else(|| {
                interaction
                    .iter()
                    .rev()
                    .filter(|interaction| interaction.awaiting_input())
                    .find_map(|interaction| {
                        interaction
                            .elicitation()
                            .cloned()
                            .map(|elicitation| ActiveElicitation {
                                owner: ElicitationOwner::Interaction,
                                plan_id: interaction.plan_id.clone(),
                                exchange_id: Some(interaction.id.clone()),
                                elicitation,
                            })
                    })
            });
        let plan_list = store.list_plan(&source_session.id)?;
        let lifecycle_list = store.list_plan_lifecycle(&source_session.id)?;
        let execution_list = store.list_plan_execution(&source_session.id)?;
        let goal_execution = goal.as_ref().and_then(|goal| {
            execution_list
                .iter()
                .rev()
                .find(|execution| execution.goal_id == goal.id)
                .cloned()
        });
        let active_wait = metadata.active_wait.clone();
        let session_event_list = store.list_session_event(&source_session.id)?;
        let mut status = crate::session::state_machine::SessionPhase::resolve(
            active_plan.as_ref(),
            active_wait.as_ref(),
            interaction.last(),
        );
        if task
            .as_ref()
            .is_some_and(|task| task.status == crate::task::TaskStatus::Paused)
        {
            status = crate::session::state_machine::SessionPhase::Paused;
        }
        if status == crate::session::state_machine::SessionPhase::Idle
            && self.agent_coordinator.active_count() > 0
        {
            status = crate::session::state_machine::SessionPhase::WaitingForAgent {
                agent_count: self.agent_coordinator.active_count(),
            };
        }
        if let Some(execution) = goal_execution.as_ref() {
            HarnessBroker::execution_status(&mut status, execution);
        }
        if let Some(started_at_ms) = session_event_list
            .iter()
            .rev()
            .find_map(|event| match &event.detail {
                SessionEventKind::Compaction { compaction }
                    if compaction.state == crate::backend::CompactionState::Running =>
                {
                    Some(compaction.started_at_ms.unwrap_or(event.created_at_ms))
                }
                _ => None,
            })
        {
            status = crate::session::state_machine::SessionPhase::Compacting { started_at_ms };
        }
        self.trace.record(
            &source_session.id,
            "status_projected",
            json!({ "session_id": source_session.id, "status": status }),
        );

        let agent_exchange_list =
            store.list_session_agent_exchange_metadata(&source_session.id, &agent_run_list)?;
        let timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: interaction.clone(),
            plan_list: &plan_list,
            lifecycle_list,
            execution_list,
            deviation_list: store.list_plan_deviation(&source_session.id)?,
            audit_list: store.list_plan_audit(&source_session.id)?,
            resolution_list: store.list_plan_resolution(&source_session.id)?,
            agent_run_list: agent_run_list.clone(),
            agent_exchange_list: agent_exchange_list.clone(),
            session_event_list,
            plan_file: &self.plan_file,
        })?;
        let check_snapshot = self.check_runner.snapshot()?;
        let check_history = check_snapshot.history;
        let active_check_run = check_snapshot.active_run;
        let checks = active_check_run
            .as_ref()
            .and_then(|id| check_history.iter().find(|run| &run.id == id))
            .and_then(crate::checks::CheckActivity::from_run);
        let checks_running = checks.is_some();
        let mut timeline = store.with_fork_history(
            &source_session.id,
            TimelineProjector::with_history(
                timeline,
                check_history,
                interaction.iter().chain(&agent_exchange_list),
            )?,
            &self.plan_file,
        )?;
        timeline.push(TimelineEntry::Status {
            checks,
            agent_count: self.agent_coordinator.active_count(),
            status_thoughts,
            id: format!("{}:status", source_session.id),
            created_at_ms: 0,
            status: status.clone(),
        });
        let approval = self
            .permission_coordinator
            .pending_list()?
            .into_iter()
            .filter(|approval| {
                interaction
                    .iter()
                    .chain(agent_exchange_list.iter())
                    .any(|exchange| approval.exchange_id.as_deref() == Some(exchange.id.as_str()))
            })
            .collect();
        Ok(BrokerSnapshot {
            ask_model,
            access_abbreviation: session.access.abbreviation().into(),
            subagent_tools: self.agent_coordinator.enabled(),
            lsp_tools: self.lsp.enabled(&source_session.id),
            session_tools: settings.session_tools.unwrap_or(true),
            status_thoughts,
            check_editing: self.check_editing.load(Ordering::Acquire),
            runtime_epoch: self.runtime_epoch.clone(),
            snapshot_revision: revision,
            checks_running,
            check_timing,
            automatic_checks,
            checks_deferred: self.completion_queue.deferred()?,
            active_check_run,
            task_operation: store
                .latest_task_operation(&source_session.id)?
                .filter(|operation| {
                    !matches!(
                        operation.state.as_str(),
                        "failed" | "cancelled" | "completed" | "superseded"
                    ) || operation.exchange_id.as_deref()
                        == interaction.last().map(|exchange| exchange.id.as_str())
                })
                .map(|operation| operation.view()),
            session,
            task: task_list,
            exchange: interaction,
            capability: metadata.capability.clone(),
            no_checkpoint: metadata.no_checkpoint,
            restore_recovery: crate::checkpoint::RestoreJournal::progress(
                &store.objects,
                &source_session.id,
            )?
            .filter(|progress| progress.state != crate::checkpoint::RestoreState::Complete)
            .map(|progress| json!({"applied":progress.applied,"files":progress.files})),
            goal,
            active_plan,
            active_elicitation,
            artifact: plan_list
                .iter()
                .filter(|plan| !plan.working_path.is_empty())
                .map(ArtifactSummary::from)
                .collect(),
            timeline,
            status,
            timeline_revision: check_snapshot.revision,
            goal_execution,
            active_wait,
            prompt_history: store.list_prompt_history()?,
            agent: AgentSnapshot {
                summary: agent_summaries(&agent_run_list, &agent_exchange_list),
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
            approval,
        })
    }

    /// Handles session queries and independent metadata writes without a provider turn lock.
    pub(crate) fn query(&self, method: HarnessMethod, params: Value) -> Result<Option<Value>> {
        let result = match method {
            HarnessMethod::ChecksEditing => self.set_check_editing(
                params["enabled"]
                    .as_bool()
                    .context("check editing requires enabled")?,
            )?,
            HarnessMethod::StateGet => serde_json::to_value(self.snapshot()?)?,
            HarnessMethod::TaskList => serde_json::to_value(
                SqliteStore::read_only(&self.data_root)?.list_task(&self.session_id)?,
            )?,
            HarnessMethod::AgentList => {
                let store = SqliteStore::read_only(&self.data_root)?;
                store.read_snapshot(|| {
                    let run = load_agent_registry(&store, &self.session_id)?.list();
                    let exchange =
                        store.list_session_agent_exchange_metadata(&self.session_id, &run)?;
                    Ok(serde_json::to_value(AgentSnapshot {
                        summary: agent_summaries(&run, &exchange),
                        run,
                        exchange,
                    })?)
                })?
            }
            HarnessMethod::SessionList => self.list_session(params)?.0,
            HarnessMethod::SessionPreview => self.preview_session(params)?.0,
            HarnessMethod::SessionRename => self.rename_session(params)?.0,
            HarnessMethod::SessionDelete => self.delete_session(params)?.0,
            HarnessMethod::ExchangeCommentSave => self.save_exchange_comment(params)?.0,
            HarnessMethod::ExchangeRollbackPreview => {
                let store = SqliteStore::read_only(&self.data_root)?;
                let id = required_text(&params, "preview_id")?;
                crate::checkpoint::RestorePreview::load(&store.objects, &self.session_id, &id)?
                    .summary(params.get("offset").and_then(Value::as_u64).unwrap_or(0) as usize)
            }
            HarnessMethod::PlanDeclaration => self.read_plan_declaration(params)?.0,
            HarnessMethod::PlanList => self.list_replanning_choices()?.0,
            HarnessMethod::PlanGet => self.plan(params)?,
            HarnessMethod::ExchangeList => serde_json::to_value(self.list_exchange_review()?)?,
            HarnessMethod::HistoryRecord => {
                self.record_prompt_history(params, SystemClock.now_ms())?.0
            }
            HarnessMethod::PermissionsOpen => self.open_permission_document()?.0,
            HarnessMethod::PermissionsSave => self.save_permission_document(params)?.0,
            _ => return Ok(None),
        };
        Ok(Some(result))
    }

    /// Reads a submitted plan without selecting or resuming its owning task.
    pub(super) fn plan(&self, params: Value) -> Result<Value> {
        let plan_id = required_text(&params, "plan_id")?;
        let store = SqliteStore::read_only(&self.data_root)?;
        let plan = store
            .load_plan(&plan_id)?
            .context("plan artifact not found")?;
        anyhow::ensure!(
            plan.session_id == self.session_id,
            "plan belongs to another session"
        );
        anyhow::ensure!(plan.review_digest.is_some(), "plan has no submitted review");
        if let Some(revision) = params.get("revision").filter(|value| !value.is_null()) {
            let revision: u32 = serde_json::from_value(revision.clone())?;
            let stranded = plan.state == PlanState::Accepted
                && !store
                    .list_plan_execution(&self.session_id)?
                    .iter()
                    .any(|execution| execution.plan_id == plan.id);
            if revision != plan.model_revision
                || (plan.state != PlanState::AwaitingReview && !stranded)
            {
                let source = self.capture_plan_revision(&plan.id, revision)?;
                let mut historical = serde_json::to_value(&plan)?;
                historical["title"] = json!(source.document.title);
                historical["working_path"] = json!(source.path);
                historical["document_version"] = json!(source.document.version);
                historical["model_revision"] = json!(revision);
                historical["historical_revision"] = json!(revision);
                historical["review_digest"] =
                    json!(crate::plan::digest(&serde_json::to_vec(&source.document)?));
                return Ok(historical);
            }
        }
        Ok(serde_json::to_value(plan)?)
    }

    pub(super) fn list_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let session = store
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let scope = params
            .get("scope")
            .and_then(Value::as_str)
            .unwrap_or("repo");
        let session = if scope == "all" {
            store.list_session(None)?
        } else {
            let workspace = params
                .get("workspace")
                .and_then(Value::as_str)
                .map(Path::new)
                .map(crate::workspace::resolve)
                .transpose()?
                .map(|kind| match kind {
                    WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                        path.to_string_lossy().into_owned()
                    }
                })
                .unwrap_or_else(|| session.workspace.clone());
            store.list_session(Some(&workspace))?
        };
        Ok((
            serde_json::to_value(
                session
                    .into_iter()
                    .filter(|session| session.background.is_none())
                    .collect::<Vec<_>>(),
            )?,
            Vec::new(),
        ))
    }

    pub(super) fn preview_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let session_id = required_text(&params, "session_id")?;
        let preview_session = store
            .load_session(&session_id)?
            .context("session not found")?;
        let interaction = store.list_exchange_metadata(&session_id)?;
        let plan_list = store.list_plan(&session_id)?;
        let agent_run_list = store
            .list_agent_run(&session_id)?
            .into_iter()
            .filter(|agent| !agent.is_primary())
            .collect::<Vec<_>>();
        let agent_exchange_list =
            store.list_session_agent_exchange_metadata(&session_id, &agent_run_list)?;
        let timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: interaction.clone(),
            plan_list: &plan_list,
            lifecycle_list: store.list_plan_lifecycle(&session_id)?,
            execution_list: store.list_plan_execution(&session_id)?,
            deviation_list: store.list_plan_deviation(&session_id)?,
            audit_list: store.list_plan_audit(&session_id)?,
            resolution_list: store.list_plan_resolution(&session_id)?,
            agent_run_list: agent_run_list.clone(),
            agent_exchange_list: agent_exchange_list.clone(),
            session_event_list: store.list_session_event(&session_id)?,
            plan_file: &self.plan_file,
        })?;
        let timeline = TimelineProjector::with_history(
            timeline,
            crate::checks::CheckSupervisor::saved_history(
                &self.data_root.join(&session_id).join("checks"),
            )?,
            interaction.iter().chain(&agent_exchange_list),
        )?;
        let timeline = store.with_fork_history(&session_id, timeline, &self.plan_file)?;
        let preview = SessionPreview {
            lines: preview_lines(&timeline)?,
            session: preview_session,
            exchange: interaction,
            timeline,
            agent: AgentSnapshot {
                summary: agent_summaries(&agent_run_list, &agent_exchange_list),
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
        };
        Ok((serde_json::to_value(preview)?, Vec::new()))
    }

    pub(super) fn list_exchange_review(&self) -> Result<Vec<Exchange>> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let session = store
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let mut interaction_list = store.list_exchange_metadata(&session.id)?;
        for interaction in &mut interaction_list {
            interaction.comment = store.list_exchange_comment(&interaction.id)?;
        }
        Ok(interaction_list)
    }

    pub(super) fn record_prompt_history(
        &self,
        params: Value,
        now_ms: i64,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut store = SqliteStore::open(&self.data_root)?;
        let draft: crate::attachment::PromptDraft = serde_json::from_value(params)?;
        store.record_prompt_history(&draft, now_ms)?;
        Ok((
            serde_json::to_value(store.list_prompt_history()?)?,
            Vec::new(),
        ))
    }

    pub(super) fn open_permission_document(&self) -> Result<(Value, Vec<SessionEvent>)> {
        let (path, source) = self
            .permission_store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .open();
        Ok((json!({ "path": path, "source": source }), Vec::new()))
    }

    pub(super) fn save_permission_document(
        &self,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let source = required_text(&params, "source")?;
        self.permission_store
            .write()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .save(&source)?;
        self.open_permission_document()
    }

    pub(super) fn list_replanning_choices(&self) -> Result<(Value, Vec<SessionEvent>)> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let session = store
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let mut choices = Vec::new();
        for session in [&session] {
            let execution = store.list_plan_execution(&session.id)?;
            for plan in store.list_plan(&session.id)? {
                choices.push(PlanChoice {
                    implemented: execution.iter().any(|record| {
                        record.plan_id == plan.id && record.state == PlanExecutionState::Complete
                    }),
                    task: store
                        .list_task(&session.id)?
                        .into_iter()
                        .find(|task| task.plan_id.as_deref() == Some(&plan.id)),
                    digest: plan.review_digest.clone(),
                    id: plan.id,
                    created_at_ms: plan.created_at_ms,
                    title: plan.title,
                    session_name: session.name.clone(),
                    revision_count: plan.model_revision,
                    state: plan.state,
                });
            }
        }
        Ok((serde_json::to_value(choices)?, Vec::new()))
    }

    pub(crate) fn capture_plan_review(
        &self,
        plan_id: &str,
        expected_digest: &str,
    ) -> Result<crate::plan::review_source::PlanReviewSource> {
        let store = SqliteStore::read_only(&self.data_root)?;
        anyhow::ensure!(
            !plan_id.is_empty() && plan_id.len() <= 256,
            "invalid plan identity"
        );
        let plan = store
            .load_plan(plan_id)?
            .context("plan review record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session_id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(expected_digest),
            "plan review digest changed"
        );
        let mut source = self.plan_file.capture_review_source(
            &self.session_id,
            &plan.id,
            (plan.submitted_version == Some(plan.document_version)).then_some(
                if plan.state == PlanState::Accepted {
                    plan.accepted_revision.unwrap_or(plan.model_revision)
                } else {
                    plan.model_revision
                },
            ),
            expected_digest,
        )?;
        source.execution_review = self.execution_review(plan_id)?;
        Ok(source)
    }

    pub(crate) fn capture_plan_revision(
        &self,
        plan_id: &str,
        revision: u32,
    ) -> Result<crate::plan::review_source::PlanReviewSource> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let plan = store
            .load_plan(plan_id)?
            .context("plan artifact not found")?;
        anyhow::ensure!(
            plan.session_id == self.session_id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            revision > 0 && revision <= plan.model_revision,
            "plan revision is unavailable"
        );
        let mut source =
            self.plan_file
                .capture_revision_source(&self.session_id, &plan.id, revision)?;
        source.execution_review = self.execution_review(plan_id)?;
        Ok(source)
    }

    pub(super) fn execution_review(&self, plan_id: &str) -> Result<Option<String>> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let Some(execution) = store
            .list_plan_execution(&self.session_id)?
            .into_iter()
            .find(|execution| execution.plan_id == plan_id)
        else {
            return Ok(None);
        };
        let mut report = format!(
            "## {}: {:?}\n\nOriginal approved revision: {}. Current accepted revision: {}.\n\n### Semantic comparison\n\n",
            execution.phase.label(),
            execution.state,
            execution.original_revision,
            execution.revision
        );
        for (label, paths) in [
            ("Matched", &execution.progress.matched),
            ("Missing", &execution.progress.missing),
            ("Different", &execution.progress.different),
            ("Unverified", &execution.progress.unverified),
            ("Analyzer warning", &execution.progress.warning),
        ] {
            for path in paths {
                report.push_str(&format!("- {label}: {path}\n"));
            }
        }
        if !execution.findings.is_empty() {
            report.push_str("\n### Unresolved findings\n\n");
            for finding in &execution.findings {
                report.push_str(&format!("- {finding}\n"));
            }
        }
        report.push_str("\n### Verification assessments\n\n");
        for evidence in &execution.verification {
            report.push_str(&format!(
                "- Revision {} · {:?}: {}\n  Tool evidence: {}\n",
                evidence.revision,
                evidence.report.outcome,
                evidence.summary,
                evidence.report.evidence.join(", ")
            ));
            if let Some(reason) = &evidence.report.reuse_reason {
                report.push_str(&format!("  Evidence reuse: {reason}\n"));
            }
        }
        report.push_str("\n### Revision history\n\n");
        for revision in &execution.revision_history {
            report.push_str(&format!(
                "- Revision {} · {} approval: {}\n",
                revision.revision, revision.approval, revision.reason
            ));
        }
        if let Some(reason) = &execution.pending_revision_reason {
            report.push_str(&format!("- Awaiting review: {reason}\n"));
        }
        let original = self.plan_file.read_submitted_document(
            &self.session_id,
            plan_id,
            execution.original_revision,
        )?;
        let current = self.plan_file.read_submitted_document(
            &self.session_id,
            plan_id,
            execution.revision,
        )?;
        let design = current
            .design
            .as_ref()
            .context("accepted execution plan has no declaration design")?;
        let delta = crate::plan::revision::DeclarationDelta::between(Some(&original), design)?;
        report.push_str("\n### Original approved to current accepted revision\n\n");
        if delta.document.is_empty() && delta.files.is_empty() {
            report.push_str("No revisions to the approved design.\n");
        } else {
            report.push_str(&format!(
                "```diff\n{}{}\n```\n",
                delta.document, delta.files
            ));
        }
        Ok(Some(report))
    }
}

#[derive(Serialize)]
struct PlanChoice {
    id: String,
    created_at_ms: i64,
    title: String,
    session_name: String,
    revision_count: u32,
    implemented: bool,
    state: PlanState,
    digest: Option<String>,
    task: Option<crate::task::TaskRecord>,
}
