use super::*;

pub(crate) struct InputReadiness {
    pub session: crate::storage::admission::AdmissionSession,
    pub has_elicitation: bool,
    pub has_approval: bool,
    pub checks_running: bool,
    pub native_steer: bool,
}

impl InputReadiness {
    /// Get the facts that hold a queued message back from starting a turn of its own.
    pub(crate) fn turn_offer_gates(&self) -> crate::exchange::TurnOfferGates {
        crate::exchange::TurnOfferGates {
            checks_running: self.checks_running,
            has_approval: self.has_approval,
            has_elicitation: self.has_elicitation,
        }
    }
}

/// Shares durable queries and published transient state without acquiring the execution owner.
pub(crate) struct BrokerObserver {
    pub(crate) input: Arc<crate::input::SessionInputQueue>,
    pub(crate) check_inbox: Arc<crate::input::SessionInputQueue>,
    pub(crate) activity: RwLock<crate::exchange::ActivitySnapshot>,
    pub(crate) questions: Arc<crate::question::QuestionController>,
    pub(crate) admission: Arc<crate::storage::admission::SessionAdmission>,
    pub(crate) input_schedule: tokio::sync::watch::Sender<crate::session::InputSchedule>,
    pub(super) lsp: Arc<crate::tools::lsp::LspService>,
    pub(crate) status_thoughts: AtomicBool,
    /// A caller's compaction is accepted and has not started its attempt. The session owner
    /// keeps this current; the idle status reads as a hand-off while it holds.
    pub(crate) compaction_waiting: AtomicBool,
    pub(crate) check_editing: Arc<AtomicBool>,
    pub(crate) completion_queue: crate::checks::CompletionQueue,
    pub(crate) session_events:
        RwLock<Arc<crate::provider::session_execution::SessionObservationInbox>>,
    pub(super) data_root: PathBuf,
    pub(super) session_id: String,
    pub(super) session_metadata: crate::storage::lifecycle::SessionMetadataWriter,
    pub(super) runtime_epoch: String,
    pub(super) snapshot_revision: AtomicU64,
    pub(super) metadata: RwLock<ObservationMetadata>,
    pub(crate) plan_file: Arc<PlanFileStore>,
    pub(super) permission_coordinator: Arc<PermissionCoordinator>,
    pub(super) permission_store: Arc<RwLock<PermissionStore>>,
    pub(super) agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    pub(super) check_runner: Arc<crate::checks::CheckSupervisor>,
    pub(super) trace: Arc<TraceStore>,
}

#[derive(Clone)]
pub(super) struct ObservationMetadata {
    pub revision: u64,
    pub capability: ProviderCapability,
    pub manifest: Option<forge_provider_api::CapabilityManifest>,
    pub active_wait: Option<ActiveWait>,
    pub no_checkpoint: bool,
}

impl BrokerObserver {
    /// Read negotiated native steering support without querying or configuring a provider.
    ///
    /// Steering is a provider fact; Harness admits steering of an active task of any kind
    /// whenever the provider reports native steering.
    pub(crate) fn native_steer_supported(&self) -> Result<bool> {
        let metadata = self
            .metadata
            .read()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
        Ok(metadata.capability.native_steer)
    }

    /// Get the bounded internal check route installed by the session exchange owner.
    pub(crate) fn check_owner_sender(&self) -> Result<crate::checks::owner::CheckOwnerSender> {
        self.check_runner.owner_sender()
    }

    /// Get the current owner observation generation.
    pub(crate) fn session_events(
        &self,
    ) -> Arc<crate::provider::session_execution::SessionObservationInbox> {
        self.session_events
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }

    /// Install a replacement only after the previous observer has drained.
    pub(crate) fn replace_session_events(
        &self,
        inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    ) {
        *self
            .session_events
            .write()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = inbox;
    }
    pub(crate) fn input_readiness(&self) -> Result<InputReadiness> {
        let state = self.admission.snapshot()?;
        let metadata = self
            .metadata
            .read()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?
            .clone();
        let native_steer = metadata.capability.native_steer;
        let mut has_approval = false;
        for approval in self.permission_coordinator.pending_list()? {
            if let Some(id) = approval.exchange_id {
                has_approval |= self.admission.owns_exchange(&id)?;
            }
        }
        Ok(InputReadiness {
            session: state.session.clone(),
            has_elicitation: state.has_elicitation,
            has_approval,
            checks_running: self.check_runner.active()?.is_some(),
            native_steer,
        })
    }
    /// Keep an idle phase working while the next turn or check run is about to start.
    ///
    /// The finished exchange resolves to idle a few owner steps before the next queued message
    /// is claimed and admitted, and before a task's reserved continuation becomes queued input or
    /// its verification checks start. Publishing that idle phase closed the status row and
    /// reopened it at every hand-off. The substitute carries no running clock, so it reads
    /// `Working` without one: the successor may continue an open exchange whose clock already
    /// stands well past zero, and publishes that clock itself once admitted. Its `HandingOff`
    /// activity tells the editor that the settled exchange is followed by another turn, so the
    /// editor does not reveal the finished exchange a moment before the successor replaces it at
    /// the tail. Pending verification checks read `Checking · 0s` instead, and only over an idle
    /// phase: no exchange replaces the finished one, and once the run is recorded its own
    /// `Checking` phase, timed by the run, follows. A `/compact` that waits for session control,
    /// such as one requested while a task's checks run, is a hand-off too: the work it waits for
    /// settles to idle before the compaction starts and publishes its own `Compacting` phase. An
    /// owner that cannot execute is excluded, because it would never start the work and the row
    /// would never clear.
    pub(crate) fn hand_off_phase(
        &self,
        store: &SqliteStore,
        task: Option<&crate::task::Task>,
        status: crate::session::state_machine::SessionPhase,
    ) -> Result<crate::session::state_machine::SessionPhase> {
        use crate::session::state_machine::{SessionPhase, WorkflowActivity};
        // A revision whose acceptance is already decided reaches review only for the instant
        // before execution starts: either the acceptance command waits for the revision's turn
        // to settle, or the reviewer's recorded decision runs as soon as the owner can issue
        // it. That instant is the hand-off to execution, not a review the user must act on.
        if matches!(status, SessionPhase::AwaitingPlanReview { .. })
            && let Some(task) = task
            && (crate::task::command::SessionCommand::recorded_acceptance(task).is_some()
                || store
                    .pending_terminal_requests(&self.session_id)?
                    .iter()
                    .any(|(id, owner)| {
                        id.starts_with("plan_acceptance:")
                            && owner.as_deref() == Some(task.id.as_str())
                    }))
        {
            return Ok(SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: None,
                activity: WorkflowActivity::HandingOff,
                reasoning_summary: None,
                execution: None,
            });
        }
        if status != SessionPhase::Idle {
            return Ok(status);
        }
        let unavailable = self
            .activity
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .phase
            == crate::exchange::ActivityPhase::Unavailable;
        if unavailable {
            return Ok(status);
        }
        let hand_off = |activity| SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: None,
            activity,
            reasoning_summary: None,
            execution: None,
        };
        if self.compaction_waiting.load(Ordering::Acquire) {
            return Ok(hand_off(WorkflowActivity::HandingOff));
        }
        let input = self.input.snapshot()?;
        if input.messages.is_empty() && task.is_none() {
            return Ok(status);
        }
        // Unreadable admission state fails the owner's own offer, which reports it; no turn
        // starts, so the idle phase stands.
        let Ok(readiness) = self.input_readiness() else {
            return Ok(status);
        };
        let gates = readiness.turn_offer_gates();
        let task_successor = crate::exchange::task_successor_pending(
            task,
            &input,
            &self.questions,
            gates,
            |turn| store.continuation_source_complete(&self.session_id, turn),
        )?;
        let activity = match task_successor {
            // Once the run is recorded the execution already reads `Checking` with the run's
            // clock; only the idle gap before it needs the clockless substitute.
            Some(crate::exchange::TaskSuccessor::Checks) if status != SessionPhase::Idle => {
                return Ok(status);
            }
            // No run record exists yet, so the rows it will show are unknown; the timeline
            // turns this into `Checking` when the run's first record attaches its rows.
            Some(crate::exchange::TaskSuccessor::Checks) => WorkflowActivity::ChecksStarting,
            Some(crate::exchange::TaskSuccessor::Turn) => WorkflowActivity::HandingOff,
            None if crate::exchange::successor_turn_pending(input, &self.questions, gates)? => {
                WorkflowActivity::HandingOff
            }
            None => return Ok(status),
        };
        Ok(hand_off(activity))
    }
    pub(crate) fn session_id(&self) -> &str {
        &self.session_id
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
        let mut store = crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?;
        let session =
            super::SessionNotFound::require(store.load_session(&session_id)?, &session_id)?;
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
        let mut store = crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
            let parsed = design.parsed()?;
            crate::plan::calls::present(
                &path,
                parsed
                    .proposed
                    .get(&path)
                    .context("proposed declaration not found")?,
                parsed
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
    pub(crate) fn set_check_editing(&self, enabled: bool) -> Result<Value> {
        let session = crate::storage::HistoryReader::current(&self.data_root)?
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
        settings.patch(
            crate::settings::SettingTarget {
                scope: crate::settings::SettingScope::Session,
                workspace: &session.workspace,
                session: &session.id,
            },
            &crate::settings::PortableSettings {
                check_editing: Some(enabled),
                ..Default::default()
            },
        )?;
        self.check_editing.store(enabled, Ordering::Release);
        Ok(json!({"check_editing": enabled}))
    }

    /// Reads current durable state together with the last published runtime metadata.
    pub(crate) fn snapshot(&self) -> Result<BrokerSnapshot> {
        let revision = self.snapshot_revision.fetch_add(1, Ordering::Relaxed) + 1;
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
            manifest: broker.latest_manifest.clone(),
            active_wait: broker.active_wait_projection.clone(),
            no_checkpoint: matches!(broker.workspace_kind, WorkspaceKind::Untracked(_)),
        };
        let session = broker.session_view()?;
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
        let settings_store = crate::settings::SettingsStore::read_only(&self.data_root)?;
        let snapshot = settings_store.get(&session.workspace, &session.id, &session.provider)?;
        // The Permissions tab shows this session's own choice: its session layer, or the access
        // it opened with. Another editor's change to the defaults never reaches it.
        let access_preference = settings_store
            .layer_settings(
                crate::settings::SettingScope::Session,
                &session.workspace,
                &session.id,
            )?
            .access
            .unwrap_or_else(|| session.access.clone());
        let ask_model = snapshot.ask_model()?;
        let settings = snapshot.desired;
        let check_timing = settings.check_timing.unwrap_or_default();
        let automatic_checks = settings.automatic_checks_enabled();
        session.approval_timeout = self.permission_coordinator.approval_timeout(&session.id)?;
        let task = source_session.active_task.as_ref();
        let active_plan = match task.map(|task| &task.workflow) {
            Some(crate::task::WorkflowSnapshot::Plan { plan }) => Some(plan.artifact.clone()),
            Some(crate::task::WorkflowSnapshot::Execute { execution }) => {
                store.load_plan(&execution.record.plan_id)?
            }
            _ => None,
        };
        if let Some(plan) = active_plan.as_ref() {
            self.trace.record(
                &source_session.id,
                "snapshot_plan_state",
                plan_trace_fields(plan),
            );
        }
        let mut interaction = store.list_exchange_metadata(&source_session.id)?;
        let questions = self.questions.snapshot()?;
        crate::question::project(&mut interaction, &questions)?;
        let plan_input = active_plan.as_ref().filter(|_| {
            !task
                .as_ref()
                .is_some_and(|task| task.status() == crate::task::TaskStatus::Paused)
        });
        let active_elicitation = plan_input
            .and_then(|plan| {
                plan.review_questions().map(|questions| ActiveElicitation {
                    owner: ElicitationOwner::PlanReview,
                    plan_id: Some(plan.id.clone()),
                    exchange_id: None,
                    elicitation: questions.clone(),
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
                                plan_id: active_plan.as_ref().map(|plan| plan.id.clone()),
                                exchange_id: Some(interaction.id.clone()),
                                elicitation,
                            })
                    })
            });
        let plan_list = store.list_plan(&source_session.id)?;
        let lifecycle_list = store.list_plan_lifecycle(&source_session.id)?;
        let execution_list = store.list_plan_execution(&source_session.id)?;
        let goal_execution = match task.map(|task| &task.workflow) {
            Some(crate::task::WorkflowSnapshot::Execute { execution }) => {
                Some(execution.record.clone())
            }
            _ => None,
        };
        let active_wait = metadata.active_wait.clone();
        let session_event_list = store.list_session_event(&source_session.id)?;
        let mut status = crate::session::state_machine::SessionPhase::resolve(
            active_plan.as_ref(),
            active_wait.as_ref(),
            interaction.last(),
        );
        if status == crate::session::state_machine::SessionPhase::Idle
            && self.agent_coordinator.active_count() > 0
        {
            status = crate::session::state_machine::SessionPhase::WaitingForAgent {
                agent_count: self.agent_coordinator.active_count(),
            };
        }
        // Children that still run keep the session from reading Paused, as in the live status,
        // which also applies the pause after it names the running agents.
        if task
            .as_ref()
            .is_some_and(|task| task.status() == crate::task::TaskStatus::Paused)
        {
            status = status.paused();
        }
        if let Some(execution) = goal_execution.as_ref() {
            HarnessBroker::execution_status(&mut status, execution);
        }
        if let Some(started_at_ms) = session_event_list
            .iter()
            .rev()
            .find_map(|event| match &event.detail {
                SessionEventKind::Compaction { compaction }
                    if compaction.state == crate::provider::CompactionState::Running =>
                {
                    Some(compaction.started_at_ms.unwrap_or(event.created_at_ms))
                }
                _ => None,
            })
        {
            status = crate::session::state_machine::SessionPhase::Compacting {
                started_at_ms,
                turn_waiting: interaction
                    .last()
                    .is_some_and(|exchange| !exchange.is_finished()),
            };
        }
        if questions.required_pending()
            && !matches!(
                status,
                crate::session::state_machine::SessionPhase::Paused { .. }
            )
            && !interaction
                .last()
                .is_some_and(|exchange| exchange.state() == crate::exchange::ExchangeState::Running)
        {
            status = crate::session::state_machine::SessionPhase::AwaitingInput {
                owner: ElicitationOwner::Interaction,
                plan_id: active_plan.as_ref().map(|plan| plan.id.clone()),
                exchange_id: questions
                    .questions
                    .iter()
                    .find(|question| {
                        question.pending()
                            && question.requirement
                                == crate::question::QuestionRequirement::Required
                    })
                    .map(|question| question.exchange_id.clone()),
            };
        }
        let cancellation = store.cancellation_progress(&source_session.id)?;
        // The owner commits the cancelled exchange inside a command step but records the settled
        // phase only when its next step begins. The exchange heading reads that committed record,
        // so a phase that only settles the target exchange yields to the record once it finishes;
        // provider recovery still stops the session after the exchange has settled.
        if let Some(stopping) = cancellation
            .as_ref()
            .and_then(|progress| cancellation_stopping(progress, &interaction))
        {
            status = stopping;
        }
        let status = self
            .hand_off_phase(store, task, status)?
            .blocked_by(task)
            .preparing_fork(&source_session.provider_fork_state);
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
        timeline = TimelineProjector::with_observed_agents(
            timeline,
            store.list_observed_agents(&source_session.id)?,
            interaction.iter().chain(&agent_exchange_list),
        )?;
        let agents = crate::timeline::running_agents(&timeline, status_thoughts);
        timeline.push(TimelineEntry::Status {
            checks,
            agents,
            status_thoughts,
            id: format!("{}:status", source_session.id),
            created_at_ms: 0,
            status: status.clone(),
        });
        let approval = self
            .permission_coordinator
            .pending_presentations()?
            .into_iter()
            .filter(|approval| {
                interaction
                    .iter()
                    .chain(agent_exchange_list.iter())
                    .any(|exchange| approval.exchange_id.as_deref() == Some(exchange.id.as_str()))
            })
            .collect();
        Ok(BrokerSnapshot {
            cancellation,
            activity: self
                .activity
                .read()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone(),
            questions: self.questions.snapshot()?,
            ask_model,
            access_preference,
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
            session,
            exchange: interaction,
            capability: metadata.capability.clone(),
            no_checkpoint: metadata.no_checkpoint,
            // A restore still running owns its incomplete journal; only a stopped one needs recovery.
            restore_recovery: crate::checkpoint::RestoreJournal::interrupted(
                &store.objects,
                &source_session.id,
            )?
            .map(|progress| json!({"applied":progress.applied,"files":progress.files})),
            active_plan,
            active_elicitation,
            // A cancelled or rejected plan is no longer current work; its transcript rows
            // still open it by identity.
            artifact: plan_list
                .iter()
                .filter(|plan| {
                    !plan.working_path.is_empty()
                        && !matches!(plan.state, PlanState::Cancelled | PlanState::Rejected)
                })
                .map(ArtifactSummary::from)
                .collect(),
            timeline,
            status,
            timeline_revision: check_snapshot.revision,
            active_wait,
            prompt_history: store.list_prompt_history()?,
            agent: AgentSnapshot {
                revision,
                runtime_epoch: self.runtime_epoch.clone(),
                summary: agent_summaries(&agent_run_list, &agent_exchange_list),
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
            approval,
            observed_agents: store.list_observed_agents(&source_session.id)?,
        })
    }

    /// Handles session queries and independent metadata writes without a provider turn lock.
    pub(crate) fn query(&self, method: RuntimeMethod, params: Value) -> Result<Option<Value>> {
        let result = match method {
            RuntimeMethod::SessionStateGet => serde_json::to_value(self.snapshot()?)?,
            RuntimeMethod::SessionTaskGet => {
                serde_json::to_value(self.snapshot()?.session.active_task)?
            }
            RuntimeMethod::SessionList => self.list_session(params)?.0,
            RuntimeMethod::SessionPreview => self.preview_session(params)?.0,
            RuntimeMethod::SessionRename => self.rename_session(params)?.0,
            RuntimeMethod::SessionDelete => self.delete_session(params)?.0,
            RuntimeMethod::SessionExchangeCommentSave => self.save_exchange_comment(params)?.0,
            RuntimeMethod::SessionExchangeRollbackPreview => {
                let store = crate::storage::HistoryReader::current(&self.data_root)?;
                let id = required_text(&params, "preview_id")?;
                crate::checkpoint::RestorePreview::load(&store.objects, &self.session_id, &id)?
                    .summary(params.get("offset").and_then(Value::as_u64).unwrap_or(0) as usize)
            }
            // Only the status read is a query; `continue` and `undo` run as owner commands.
            RuntimeMethod::SessionExchangeRecovery => {
                match params.get("action").and_then(Value::as_str) {
                    None | Some("status") => crate::checkpoint::RestoreJournal::status(
                        &crate::storage::HistoryReader::current(&self.data_root)?.objects,
                        &self.session_id,
                    )?,
                    Some("continue" | "undo") => return Ok(None),
                    Some(_) => anyhow::bail!("unknown restore recovery action"),
                }
            }
            RuntimeMethod::SessionPlanDeclarationGet => self.read_plan_declaration(params)?.0,
            RuntimeMethod::SessionPlanList => self.list_replanning_choices()?.0,
            RuntimeMethod::SessionPlanGet => self.plan(params)?,
            RuntimeMethod::SessionExchangeList => {
                serde_json::to_value(self.list_exchange_review()?)?
            }
            RuntimeMethod::SessionPromptHistoryRecord => {
                self.record_prompt_history(params, SystemClock.now_ms())?.0
            }
            RuntimeMethod::PermissionsDocumentOpen => self.open_permission_document()?.0,
            RuntimeMethod::PermissionsDocumentSave => self.save_permission_document(params)?.0,
            _ => return Ok(None),
        };
        Ok(Some(result))
    }

    /// Reads a submitted plan without selecting or resuming its owning task.
    pub(super) fn plan(&self, params: Value) -> Result<Value> {
        let plan_id = required_text(&params, "plan_id")?;
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let session = store
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let scope = params
            .get("scope")
            .and_then(Value::as_str)
            .unwrap_or("repo");
        let session = if scope == "all" {
            store.list_user_session(None)?
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
            store.list_user_session(Some(&workspace))?
        };
        Ok((serde_json::to_value(session)?, Vec::new()))
    }

    pub(super) fn preview_session(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let session_id = required_text(&params, "session_id")?;
        let preview_session =
            super::SessionNotFound::require(store.load_session(&session_id)?, &session_id)?;
        let mut interaction = store.list_exchange_metadata(&session_id)?;
        let plan_list = store.list_plan(&session_id)?;
        let agent_run_list = store
            .list_agent_run(&session_id)?
            .into_iter()
            .filter(|agent| !agent.is_primary())
            .collect::<Vec<_>>();
        let mut agent_exchange_list =
            store.list_session_agent_exchange_metadata(&session_id, &agent_run_list)?;
        // A session nobody owns keeps the running exchange its killed owner left in storage.
        // Show what resuming will show, not a clock that runs to the preview instant.
        if !store.session_owner_live(&session_id)? {
            let primary_agent_id = crate::session::HarnessSession::primary_agent_id(&session_id);
            for exchange in interaction.iter_mut().chain(agent_exchange_list.iter_mut()) {
                crate::storage::interrupt_detached_exchange(
                    exchange,
                    &primary_agent_id,
                    crate::storage::DetachedSettlement::Outcome,
                )?;
            }
        }
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
                &crate::storage::state::directory(&self.data_root)
                    .join(&session_id)
                    .join("checks"),
            )?,
            interaction.iter().chain(&agent_exchange_list),
        )?;
        let timeline = store.with_fork_history(&session_id, timeline, &self.plan_file)?;
        let preview = SessionPreview {
            lines: preview_lines(&timeline, Clock::now_ms(&SystemClock))?,
            session: preview_session,
            exchange: interaction,
            timeline,
            agent: AgentSnapshot {
                revision: 0,
                runtime_epoch: self.runtime_epoch.clone(),
                summary: agent_summaries(&agent_run_list, &agent_exchange_list),
                run: agent_run_list,
                exchange: agent_exchange_list,
            },
        };
        Ok((serde_json::to_value(preview)?, Vec::new()))
    }

    /// List the session's exchanges with the reason, if any, that rolling back to each is refused.
    pub(super) fn list_exchange_review(
        &self,
    ) -> Result<Vec<crate::exchange::rollback::ExchangeRollbackReview>> {
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let session = store
            .load_session(&self.session_id)?
            .context("observed session is missing")?;
        let mut interaction_list = store.list_exchange_metadata(&session.id)?;
        for interaction in &mut interaction_list {
            interaction.comment = store.list_exchange_comment(&interaction.id)?;
        }
        let capability = self
            .metadata
            .read()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?
            .capability
            .clone();
        let provider = crate::exchange::rollback::RewindProvider {
            label: &session.provider_label,
            capability: &capability,
            native_history: session.provider_session_id.is_some(),
            newest_session_compaction_ms:
                crate::exchange::rollback::RewindProvider::newest_session_compaction(
                    &store.list_session_event(&session.id)?,
                ),
        };
        Ok(crate::exchange::rollback::ExchangeRollbackReview::list(
            interaction_list,
            &store.list_plan(&session.id)?,
            &provider,
        ))
    }

    pub(super) fn record_prompt_history(
        &self,
        params: Value,
        now_ms: i64,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut store = crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
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
        for run in &execution.check_runs {
            if let Some(planned) = &run.planned_tests {
                report.push_str(&format!("\n### Planned test results · {}\n\n", run.id));
                for line in planned.review_lines() {
                    report.push_str(&format!("- {line}\n"));
                }
            }
        }
        report.push_str("\n### Verification assessments\n\n");
        for evidence in &execution.verification {
            report.push_str(&format!(
                "- Revision {} · {:?}: {}\n  Observed in: {}\n",
                evidence.revision,
                evidence.report.status,
                evidence.summary,
                evidence
                    .report
                    .cited()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ")
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
}

/// Get the stopping phase a cancellation operation imposes on the session, if any.
///
/// An operation that only settles its target exchange yields once that exchange has settled or
/// been withdrawn, as a cancelled output-free prompt is; provider recovery still stops the
/// session after the exchange has settled.
fn cancellation_stopping(
    progress: &crate::exchange::CancellationProgress,
    interaction: &[Exchange],
) -> Option<crate::session::state_machine::SessionPhase> {
    use crate::exchange::CancellationPhase;
    let target_settled = progress.exchange_id.as_ref().is_some_and(|target| {
        interaction
            .iter()
            .find(|exchange| exchange.id == target.as_str())
            .is_none_or(Exchange::is_finished)
    });
    (progress.phase.active()
        && (progress.phase == CancellationPhase::Recovering || !target_settled))
        .then_some(crate::session::state_machine::SessionPhase::Stopping {
            collection: match progress.phase {
                CancellationPhase::Acquiring | CancellationPhase::Collecting => {
                    crate::exchange::ExchangeCollection::Pending
                }
                _ => crate::exchange::ExchangeCollection::Settling,
            },
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn progress(
        phase: crate::exchange::CancellationPhase,
        exchange_id: &str,
    ) -> crate::exchange::CancellationProgress {
        crate::exchange::CancellationProgress {
            operation_id: crate::exchange::command::CommandId::new("cancel").unwrap(),
            exchange_id: Some(exchange_id.into()),
            revision: 1,
            phase,
            response_timed_out: false,
            started_at_ms: 1,
            updated_at_ms: 1,
            blockers: Default::default(),
        }
    }

    /// A cancellation that withdrew its output-free prompt stops nothing once the exchange is
    /// gone. The owner deletes the exchange inside a command step but records the settled phase
    /// only when its next step begins, and the missing exchange read as unfinished, so `Stopping`
    /// stayed for about half a second after the prompt had returned to the composer.
    #[test]
    fn withdrawn_cancellation_target_stops_nothing() {
        use crate::exchange::CancellationPhase;
        for phase in [CancellationPhase::Draining, CancellationPhase::Settling] {
            assert_eq!(
                cancellation_stopping(&progress(phase, "withdrawn"), &[]),
                None
            );
        }
        assert!(
            cancellation_stopping(&progress(CancellationPhase::Recovering, "withdrawn"), &[])
                .is_some(),
            "provider recovery outlives the exchange it settled"
        );
    }
}
