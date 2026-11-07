//! Transfer admitted provider work and every scoped resource to the foreground driver.

use super::foreground_dispatch::{ForegroundProviderContext, StagedInteraction};
use super::*;

impl HarnessBroker {
    /// Start one prepared native submission with owned observation, tools, and task provenance.
    pub(crate) fn construct_foreground_transaction(
        &mut self,
        staged: &StagedInteraction,
        engine: Arc<crate::provider::session_execution::SessionExecution>,
    ) -> Result<ForegroundTransaction> {
        anyhow::ensure!(
            self.pending_host_settlement.is_none(),
            "prior host settlement is still pending"
        );
        let ForegroundProviderContext {
            mut request,
            repository_lease,
            inspection_lease,
            lsp_lease,
            task_permit,
            execution_id: dispatched_execution_id,
            entry_id,
        } = self.prepare_foreground_provider_context(staged)?;
        let scope = match staged.reservation.as_ref() {
            Some(reservation) => {
                anyhow::ensure!(
                    Arc::ptr_eq(&reservation.engine, &engine),
                    "prepared continuation execution owner changed"
                );
                reservation.scope.clone()
            }
            None => {
                crate::provider::InvocationScope::new(request.owner()?, staged.exchange.id.clone())
            }
        };
        let (live_host_sender, live_host_receiver) =
            crate::provider::LiveHostEffectSender::channel();
        if let Some(context) = request.control_context.as_mut() {
            context.live_host_effects = Some(live_host_sender);
        }
        let attached_results = self.agent_coordinator.unreported_result();
        if !attached_results.is_empty() {
            let message = attached_results
                .iter()
                .map(|task| self.agent_result_message(task))
                .collect::<Result<Vec<_>>>()?
                .join("\n\n");
            match &mut request.input {
                ProviderInput::Text { text, .. } => {
                    text.push_str("\n\n");
                    text.push_str(&message);
                }
                ProviderInput::Skill { arguments, .. } => {
                    arguments.push_str("\n\n");
                    arguments.push_str(&message);
                }
                ProviderInput::QuestionReply { context, .. } => {
                    context.push_str("\n\n");
                    context.push_str(&message);
                }
            }
        }

        request.acknowledged_child_reports = attached_results
            .iter()
            .filter(|task| {
                task.source == crate::agent::AgentSource::Model
                    && task.parent_exchange_id == staged.exchange.id
                    && matches!(
                        task.state,
                        ExchangeState::Interrupted
                            | ExchangeState::Cancelled
                            | ExchangeState::Failed
                    )
            })
            .map(|task| task.id.clone().into())
            .collect();

        // Every scoped callback must share the same owner-confirmed revision handle,
        // including scopes captured before the provider bridge clones this context.
        if let Some(context) = &mut request.control_context {
            context.bind_task_authority_progress();
        }
        // Acquire all fallible tool scopes before transferring the exchange runtime or dispatching.
        let (agent_scope, agent_lease) = crate::agent::AgentToolScope::new(
            scope.owner.clone(),
            staged.exchange.id.clone(),
            self.agent_coordinator.clone(),
        )?;
        agent_scope.bind_task_authority(
            request
                .control_context
                .as_ref()
                .and_then(crate::tools::TaskAuthority::capture),
        )?;
        if let Some(context) = &mut request.control_context {
            context.agent = Some(agent_scope);
        }
        let task_authority = request
            .control_context
            .as_ref()
            .and_then(crate::tools::TaskAuthority::capture);
        repository_lease.bind_authority(task_authority.clone())?;
        inspection_lease.bind_authority(task_authority.clone())?;
        lsp_lease.bind_authority(task_authority.clone())?;
        let (events, stream) = crate::provider::events::channel(scope.clone());
        let tool_timeout = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired
            .tool_timeout
            .unwrap_or(self.session.tool_timeout);
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            tool_timeout,
        )?);
        let session_events = self.observer.session_events();
        session_events.watch_tools(&scope, &watchdog)?;
        session_events.start_tool_monitor(
            scope.clone(),
            watchdog.clone(),
            self.provider.clone(),
        )?;
        let provider_event_sink = events.with_observer(session_events.clone());
        let (execution_sender, execution_receiver) = tokio::sync::mpsc::channel(1);
        let execution = self
            .current_execution()?
            .filter(|execution| execution.state == PlanExecutionState::Active);
        if request.mode == PromptMode::ExecutePlan
            && let (Some(context), Some(execution)) =
                (&mut request.control_context, execution.as_ref())
        {
            context.execution = Some(crate::plan::execution::ExecutionControlSender {
                task_controller: self.task_controller.clone(),
                task_permit: context
                    .task_permit
                    .clone()
                    .context("execution callback has no dispatched task permit")?,
                phase: execution.phase,
                execution_id: execution.id.clone(),
                generation: execution.generation,
                sender: execution_sender,
            });
        }
        let (check_scope, check_lease) = crate::checks::CheckToolScope::open(
            self.check_runner.clone(),
            crate::checks::CheckAuthority {
                task_authority: task_authority.clone(),
                data_root: self.data_root.clone(),
                provider: self.session.provider.clone(),
                task_id: self.task_controller.current()?.map(|task| {
                    task.entry
                        .as_ref()
                        .map(|entry| entry.id.as_str())
                        .unwrap_or(task.revision.as_str())
                        .to_owned()
                }),
                shell: self.session.shell,
                mode: request.mode,
                permission: self.session.approval_mode,
                permissions: self.permission_coordinator.clone(),
                scope: scope.clone(),
                authorization_sink: provider_event_sink.clone(),
                editing: self.observer.check_editing.clone(),
                can_run: staged.exchange.check_setup.is_none()
                    && self.agent_coordinator.active_count() == 0
                    && execution.is_none(),
                agent: self.agent_coordinator.clone(),
                cancelled: self.turn_cancellation.subscribe(),
                sink: self.event_sink.clone(),
            },
        );
        if let Some(context) = &mut request.control_context {
            context.checks = Some(check_scope);
            if staged.exchange.check_setup.is_some() && request.mode == PromptMode::Chat {
                context.resolved_question_digest_set.clear();
                context.has_active_elicitation = staged.exchange.elicitation().is_some();
            }
        }

        // Install host settlement ownership before native callbacks can be dispatched.
        let submission = request
            .submission
            .as_ref()
            .context("prepared foreground has no input receipt")?;
        engine.reserve_invocation(scope.clone(), submission.clone())?;
        let active = self
            .parent_input
            .activate(Some(provider_event_sink.clone()))?;
        let state = self.begin_owned_foreground(staged.exchange.clone(), scope)?;
        let settlement = Arc::new(crate::provider::session_execution::SettlementCell::default());
        self.pending_host_settlement = Some(settlement.clone());
        // Prior-turn usage remains useful for display but cannot authorize maintenance
        // for a new submission until that submission reports fresh context usage.
        self.context_usage_fresh = false;
        let native = ForegroundWait::start(
            stream,
            ProviderSubmission {
                request: request.clone(),
                engine: engine.clone(),
                sink: provider_event_sink.clone(),
                trace: self.trace.clone(),
                store: self.submission_store.clone(),
                continuation: None,
                settlement: settlement.clone(),
                retry: Default::default(),
            }
            .run(),
            self.turn_cancellation.capture(),
            watchdog,
        )?;
        let mut scan_interval = tokio::time::interval_at(
            tokio::time::Instant::now() + std::time::Duration::from_secs(5),
            std::time::Duration::from_secs(5),
        );
        scan_interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
        let mut agent_revision = self.agent_coordinator.subscribe();
        agent_revision.mark_changed();
        Ok(ForegroundTransaction {
            live_host_receiver,
            live_host: None,
            state,
            request,
            mode: staged.mode,
            settlement,
            continuation_count: 0,
            dispatched_task_permit: task_permit,
            dispatched_execution_id,
            dispatched_entry_id: entry_id,
            _repository_lease: repository_lease,
            _inspection_lease: inspection_lease,
            _lsp_lease: lsp_lease,
            native,
            native_open: true,
            collection: None,
            provider_event_sink,
            engine,
            steering: ProviderSteering {
                active,
                deferred: std::collections::VecDeque::new(),
                pending_deliveries: Arc::new(SteeringDeliveries::default()),
                refresh_readiness: false,
            },
            agent_revision,
            _agent_foreground_claim: self.agent_coordinator.claim_foreground(),
            question_changes: self.questions.changed.subscribe(),
            execution_receiver,
            pending_revision: None,
            task_authority,
            revision_job_active: false,
            agent_command_active: false,
            result_delivery: tokio::task::JoinSet::new(),
            attached_results,
            attempted_results: std::collections::HashSet::new(),
            pending_agent_launch: std::collections::VecDeque::new(),
            pending_agent_reply: None,
            pending_agent_interrupt: None,
            user_delivery: tokio::task::JoinSet::new(),
            session_events,
            delivery_sink: self.event_sink.clone(),
            scan_interval,
            execution_id: execution.as_ref().map(|execution| execution.id.clone()),
            question_enabled: !self.question_discussion,
            question_stop_pending: false,
            question_override: None,
            pending_native_ready: None,
            retraction_boundary: None,
            scan_enabled: execution.is_some(),
            execution_open: true,
            _agent_lease: agent_lease,
            _check_lease: check_lease,
            pending_session_observations: std::collections::VecDeque::new(),
            observation_publication_in_flight: false,
            finalization: None,
            failure_cleanup_active: false,
            failure_inbox_open: false,
            failure: None,
            failure_secondary: Vec::new(),
        })
    }
}
