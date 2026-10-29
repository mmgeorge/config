use super::*;

impl BrokerObserver {
    pub(super) fn prepare_review_change(
        &self,
        action: &crate::task::command::SessionAction,
        current: Option<&crate::task::ActiveTask>,
        now_ms: i64,
    ) -> Result<super::plan_entry::PreparedTaskChange> {
        use crate::task::command::SessionAction;
        use crate::task::{ActiveTask, TaskChange, TaskWorkflow};
        let review = match action {
            SessionAction::BeginAcceptance { review }
            | SessionAction::CancelAcceptance { review }
            | SessionAction::RequestChanges { review }
            | SessionAction::CancelPlan { review } => review,
            _ => anyhow::bail!("command is not a plan review decision"),
        };
        let store = SqliteStore::read_only(&self.data_root)?;
        let plan_id = review
            .plan_id
            .as_deref()
            .or_else(|| {
                current.and_then(|task| match &task.workflow {
                    TaskWorkflow::Plan { plan } => Some(plan.artifact.id.as_str()),
                    TaskWorkflow::Execute { execution } => Some(execution.record.plan_id.as_str()),
                    _ => None,
                })
            })
            .context("plan_id is required")?;
        let mut plan = store
            .load_plan(plan_id)?
            .context("plan record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session_id,
            "plan belongs to another session"
        );
        if let Some(digest) = &review.digest {
            anyhow::ensure!(
                plan.review_digest.as_ref() == Some(digest),
                "plan changed after the review action began"
            );
        }
        let mut effects = crate::storage::TaskEffects::default();
        effects.plan_guard.push(crate::storage::PlanArtifactGuard {
            id: plan.id.clone(),
            document_version: plan.document_version,
            review_digest: plan.review_digest.clone(),
        });
        let anchor = store
            .latest_exchange(&self.session_id)?
            .as_ref()
            .map(crate::plan::ExchangeAnchor::capture);
        let mut task = current.cloned();
        let executing=task.as_ref().is_some_and(|task| matches!(&task.workflow,TaskWorkflow::Execute { execution } if execution.record.plan_id==plan.id && execution.record.pending_revision_reason.is_some()));
        if matches!(action, SessionAction::CancelAcceptance { .. }) {
            anyhow::ensure!(
                plan.acceptance.is_some(),
                "active plan has no pending acceptance"
            );
            plan.acceptance = None;
            plan.updated_at_ms = now_ms;
        } else if matches!(action, SessionAction::CancelPlan { .. }) {
            if executing {
                let task = task.as_mut().unwrap();
                let TaskWorkflow::Execute { execution } = &mut task.workflow else {
                    unreachable!()
                };
                let accepted = self.plan_file.restore_revision(
                    &self.session_id,
                    &plan.id,
                    execution.record.revision,
                )?;
                plan.state = PlanState::Accepted;
                plan.document_version = accepted.version;
                plan.title = accepted.title;
                plan.review_digest = plan.accepted_digest.clone();
                execution
                    .record
                    .revision_history
                    .push(crate::plan::execution::ExecutionRevision {
                        revision: plan.model_revision,
                        reason: execution.record.pending_revision_reason.take().unwrap(),
                        approval: "rejected".into(),
                        recorded_at_ms: now_ms,
                    });
                execution.record.findings.push(
                    "The user rejected the proposed revision. Match the current accepted design."
                        .into(),
                );
                self.finish_review_execution(
                    task,
                    &plan,
                    anchor.clone(),
                    "Plan revision rejected",
                    now_ms,
                )?;
            } else {
                PlanStateMachine::apply(&mut plan, PlanEvent::Cancelled, now_ms)?;
                plan.acceptance = None;
                effects.lifecycle.push(self.review_lifecycle(
                    &plan,
                    anchor.clone(),
                    PlanLifecycleKind::Cancelled,
                    None,
                    Vec::new(),
                    now_ms,
                ));
                effects.plan.push(plan.clone());
                let clearing=current.is_some_and(|task| matches!(&task.workflow,TaskWorkflow::Plan { plan:active } if active.artifact.id==plan.id));
                if clearing
                    && let Some(mut exchange) = store.latest_exchange(&self.session_id)?
                    && exchange.state() == ExchangeState::Running
                    && exchange.execution_started_at_ms().is_none()
                    && exchange
                        .turn
                        .iter()
                        .all(|turn| turn.state() != crate::turn::TurnState::Running)
                {
                    exchange.finish(crate::exchange::ExchangeOutcome::Cancelled, now_ms)?;
                    effects.exchange.push(exchange);
                }
                let change = if clearing {
                    TaskChange::Clear
                } else {
                    TaskChange::Preserve
                };
                return Ok(super::plan_entry::PreparedTaskChange { change, effects });
            }
        } else {
            anyhow::ensure!(
                matches!(plan.state, PlanState::AwaitingReview | PlanState::Accepted),
                "plan does not await review"
            );
            let digest = plan
                .review_digest
                .as_ref()
                .context("submitted plan digest is missing")?;
            let reviewed = self.capture_plan_review(&plan.id, digest)?;
            if let Some(expected) = &review.saved_source_digest {
                anyhow::ensure!(
                    expected == &reviewed.saved_digest,
                    "saved plan source changed after the review action"
                );
            }
            if let Some(design) = &reviewed.document.design {
                let session = store
                    .load_session(&self.session_id)?
                    .context("session is missing")?;
                design.check_workspace(Path::new(&session.workspace))?;
                if matches!(action, SessionAction::BeginAcceptance { .. }) {
                    let mut checks =
                        crate::checks::ConfigSnapshot::load(Path::new(&session.workspace))?;
                    let overrides = self.check_runner.overrides(
                        current
                            .and_then(|task| task.entry.as_ref())
                            .map(|entry| entry.id.as_str()),
                    )?;
                    for definition in &mut checks.config.definitions {
                        if let Some(enabled) = overrides.enabled.get(&definition.id) {
                            definition.enabled = *enabled;
                        }
                        if let Some(scope) = overrides.scopes.get(&definition.id) {
                            definition.scope = *scope;
                        }
                    }
                    anyhow::ensure!(
                        design.check_configuration.as_ref() == Some(&checks),
                        "Configured checks changed. Reopen /plan to review them before acceptance."
                    );
                }
            }
            if matches!(action, SessionAction::BeginAcceptance { .. }) && !executing {
                let mode = self
                    .metadata
                    .read()
                    .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?
                    .capability
                    .approval_mode_list
                    .clone();
                let mut acceptance = PlanAcceptance::new(digest.clone(), &mode)?;
                acceptance.saved_source_digest = Some(reviewed.saved_digest.clone());
                acceptance.overall_comment = review.comment.clone();
                plan.acceptance = Some(acceptance);
                plan.updated_at_ms = now_ms;
            } else {
                self.prepare_review_revision(
                    &mut plan,
                    &reviewed,
                    anchor.clone(),
                    &mut effects,
                    now_ms,
                )?;
                plan.user_revision += 1;
                if matches!(action, SessionAction::BeginAcceptance { .. }) {
                    let task = task.as_mut().unwrap();
                    let TaskWorkflow::Execute { execution } = &mut task.workflow else {
                        unreachable!()
                    };
                    plan.accepted_revision = Some(plan.model_revision);
                    plan.accepted_digest = plan.review_digest.clone();
                    plan.state = PlanState::Accepted;
                    execution.record.revision = plan.model_revision;
                    execution.record.revision_history.push(
                        crate::plan::execution::ExecutionRevision {
                            revision: plan.model_revision,
                            reason: execution.record.pending_revision_reason.take().unwrap(),
                            approval: "user".into(),
                            recorded_at_ms: now_ms,
                        },
                    );
                    if let Some(comment) = &review.comment {
                        execution.record.approval_comments.push(comment.clone());
                    }
                    execution.record.progress = Default::default();
                    self.finish_review_execution(
                        task,
                        &plan,
                        anchor.clone(),
                        "Plan revision accepted",
                        now_ms,
                    )?;
                    effects.lifecycle.push(self.review_lifecycle(
                        &plan,
                        anchor.clone(),
                        PlanLifecycleKind::Accepted,
                        review.comment.clone(),
                        Vec::new(),
                        now_ms,
                    ));
                } else {
                    let annotation = crate::plan::resolve_annotations(
                        &reviewed.rendered,
                        review.annotations.clone(),
                    )?;
                    let feedback =
                        crate::plan::render_review_feedback(&reviewed.document, &annotation)?;
                    let instruction = PlanPrompt::with_manual_changes(
                        PlanPrompt::revision(
                            &reviewed.document.model_json()?,
                            &feedback,
                            review.comment.as_deref(),
                        ),
                        &plan.manual_changes,
                    );
                    plan.acceptance = None;
                    if executing {
                        let task = task.as_mut().unwrap();
                        let TaskWorkflow::Execute { execution } = &mut task.workflow else {
                            unreachable!()
                        };
                        execution.record.findings.push(format!(
                            "Revise the pending design using this review feedback: {}\n{feedback}",
                            review.comment.as_deref().unwrap_or_default()
                        ));
                        execution.record.pending_revision_reason = None;
                        plan.state = PlanState::Revising;
                        self.finish_review_execution(
                            task,
                            &plan,
                            anchor.clone(),
                            "Plan revision changes requested",
                            now_ms,
                        )?;
                    } else {
                        PlanStateMachine::apply(&mut plan, PlanEvent::ChangesRequested, now_ms)?;
                        plan.generation.reset();
                        let mut revised=current.filter(|task|matches!(&task.workflow,TaskWorkflow::Plan { plan:active } if active.artifact.id==plan.id)).cloned().unwrap_or_else(||ActiveTask::plan(plan.clone(),now_ms));
                        if let TaskWorkflow::Plan { plan: active } = &mut revised.workflow {
                            active.artifact = plan.clone();
                            active.paused = false;
                        }
                        revised.request_entry(instruction, now_ms)?;
                        task = Some(revised);
                    }
                    effects.lifecycle.push(self.review_lifecycle(
                        &plan,
                        anchor.clone(),
                        PlanLifecycleKind::ChangesRequested,
                        review.comment.clone(),
                        annotation,
                        now_ms,
                    ));
                }
            }
        }
        effects.plan.push(plan.clone());
        let change = if executing {
            crate::task::TaskChange::Update(Box::new(task.unwrap()))
        } else if matches!(action, SessionAction::CancelAcceptance { .. }) {
            if let Some(mut task)=task.filter(|task|matches!(&task.workflow,TaskWorkflow::Plan { plan:active } if active.artifact.id==plan.id)) {
                if let TaskWorkflow::Plan { plan:active }=&mut task.workflow { active.artifact=plan; }
                TaskChange::Update(Box::new(task))
            } else { TaskChange::Preserve }
        } else {
            let mut prepared=task.filter(|task|matches!(&task.workflow,TaskWorkflow::Plan { plan:active } if active.artifact.id==plan.id)).unwrap_or_else(||ActiveTask::plan(plan.clone(),now_ms));
            if let TaskWorkflow::Plan { plan: active } = &mut prepared.workflow {
                active.artifact = plan;
                active.paused = false;
            }
            if current.is_some_and(|task|matches!(&task.workflow,TaskWorkflow::Plan { plan:active } if matches!(&prepared.workflow,TaskWorkflow::Plan { plan } if plan.artifact.id==active.artifact.id))) { TaskChange::Update(Box::new(prepared)) } else { TaskChange::Install(Box::new(prepared)) }
        };
        Ok(super::plan_entry::PreparedTaskChange { change, effects })
    }

    fn prepare_review_revision(
        &self,
        plan: &mut PlanRecord,
        reviewed: &crate::plan::review_source::PlanReviewSource,
        anchor: Option<crate::plan::ExchangeAnchor>,
        effects: &mut crate::storage::TaskEffects,
        now_ms: i64,
    ) -> Result<()> {
        if plan.submitted_version == Some(reviewed.document.version) {
            return Ok(());
        }
        anyhow::ensure!(
            plan.document_version == reviewed.document.version,
            "plan version changed before review decision"
        );
        plan.model_revision = plan
            .model_revision
            .checked_add(1)
            .context("plan revision exhausted")?;
        self.plan_file.submit_reviewed_revision(
            &self.session_id,
            &plan.id,
            plan.model_revision,
            reviewed,
        )?;
        plan.submitted_version = Some(reviewed.document.version);
        plan.updated_at_ms = now_ms;
        effects.lifecycle.push(self.review_lifecycle(
            plan,
            anchor,
            PlanLifecycleKind::RevisionCreated,
            Some(plan.manual_changes.join("\n")),
            Vec::new(),
            now_ms,
        ));
        Ok(())
    }

    fn review_lifecycle(
        &self,
        plan: &PlanRecord,
        anchor: Option<crate::plan::ExchangeAnchor>,
        kind: PlanLifecycleKind,
        comment: Option<String>,
        annotation: Vec<crate::plan::PlanAnnotation>,
        now_ms: i64,
    ) -> PlanLifecycleRecord {
        PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor,
            id: Uuid::new_v4().to_string(),
            session_id: self.session_id.clone(),
            plan_id: plan.id.clone(),
            kind,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: comment,
            annotation,
            question: None,
            answer: None,
            created_at_ms: now_ms,
        }
    }

    fn finish_review_execution(
        &self,
        task: &mut crate::task::ActiveTask,
        plan: &PlanRecord,
        anchor: Option<crate::plan::ExchangeAnchor>,
        title: &str,
        now_ms: i64,
    ) -> Result<()> {
        let crate::task::TaskWorkflow::Execute { execution } = &mut task.workflow else {
            anyhow::bail!("review no longer owns an execution task")
        };
        let resume = execution.record.review_paused;
        execution.record.review_paused = false;
        if resume {
            execution.record.state = PlanExecutionState::Active;
        }
        execution.record.generation += 1;
        if let Some(anchor) = anchor {
            execution.record.append_lifecycle(
                anchor,
                now_ms,
                PlanExecutionLifecycleEvent::Phase {
                    title: title.into(),
                    phase: execution.record.phase,
                    revision: execution.record.revision,
                    state: execution.record.state,
                },
            );
        }
        if resume {
            let document = self.plan_file.read_submitted_document(
                &self.session_id,
                &plan.id,
                execution.record.revision,
            )?;
            let instruction = PlanPrompt::with_manual_changes(
                PlanPrompt::execution(
                    &execution.record,
                    PlanExecutionPromptKind::Continue,
                    &document.execution_json(execution.record.phase)?,
                ),
                &plan.manual_changes,
            );
            task.request_entry(instruction, now_ms)?;
        }
        Ok(())
    }

    pub(crate) fn prepare_execution(
        &self,
        params: &Value,
        goal_max_turns: u32,
        now_ms: i64,
    ) -> Result<(crate::task::ActiveTask, crate::storage::TaskEffects)> {
        let store = SqliteStore::read_only(&self.data_root)?;
        let mut session = store
            .load_session(&self.session_id)?
            .context("session is missing")?;
        if let Some(executor) = params.get("plan_executor") {
            session.plan_executor = serde_json::from_value(executor.clone())?;
        }
        if let Some(compact) = params.get("plan_compact").and_then(Value::as_bool) {
            session.plan_compact = compact;
        }
        let selected = params["plan_id"].as_str().unwrap_or("last");
        let mut plan = if selected == "last" {
            store
                .list_plan(&session.id)?
                .into_iter()
                .rev()
                .find(|plan| plan.model_revision > 0)
        } else {
            store.load_plan(selected)?
        }
        .context("plan is missing")?;
        anyhow::ensure!(
            plan.session_id == session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            matches!(plan.state, PlanState::AwaitingReview | PlanState::Accepted),
            "plan does not await review"
        );
        let digest = plan
            .review_digest
            .clone()
            .context("submitted plan digest is missing")?;
        if let Some(expected) = params["digest"].as_str() {
            anyhow::ensure!(
                expected == digest,
                "plan changed after the accept action began"
            );
        }
        let reviewed = self.capture_plan_review(&plan.id, &digest)?;
        if let Some(expected) = params["saved_source_digest"].as_str() {
            anyhow::ensure!(
                expected == reviewed.saved_digest,
                "saved plan source changed during acceptance"
            );
        }
        let design = reviewed
            .document
            .design
            .as_ref()
            .context("execution requires a semantic plan")?;
        design.check_workspace(Path::new(&session.workspace))?;
        let mut checks = crate::checks::ConfigSnapshot::load(Path::new(&session.workspace))?;
        let scope = session
            .active_task
            .as_ref()
            .and_then(|task| task.entry.as_ref())
            .map(|entry| entry.id.as_str());
        let overrides = self.check_runner.overrides(scope)?;
        for definition in &mut checks.config.definitions {
            if let Some(enabled) = overrides.enabled.get(&definition.id) {
                definition.enabled = *enabled;
            }
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        anyhow::ensure!(
            design.check_configuration.as_ref() == Some(&checks),
            "Configured checks changed. Reopen /plan to review them before acceptance."
        );
        let approval_mode = params
            .get("approval_mode")
            .cloned()
            .map(serde_json::from_value::<ApprovalMode>)
            .transpose()?
            .unwrap_or(session.approval_mode);
        let metadata = self
            .metadata
            .read()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
        anyhow::ensure!(
            metadata
                .capability
                .approval_mode_list
                .contains(&approval_mode),
            "requested execution approval mode is unavailable"
        );
        if session.plan_compact {
            anyhow::ensure!(
                metadata.capability.native_compact && session.backend_session_id.is_some(),
                "current session cannot compact before execution"
            );
        }
        if session.plan_executor.enabled {
            anyhow::ensure!(
                session.plan_executor.model.is_some() && session.plan_executor.effort.is_some(),
                "plan executor model and thinking must be configured"
            );
        }
        drop(metadata);
        let anchor = store
            .latest_exchange(&session.id)?
            .as_ref()
            .map(crate::plan::ExchangeAnchor::capture);
        let mut effects = crate::storage::TaskEffects::default();
        effects.plan_guard.push(crate::storage::PlanArtifactGuard {
            id: plan.id.clone(),
            document_version: plan.document_version,
            review_digest: plan.review_digest.clone(),
        });
        if plan.submitted_version != Some(reviewed.document.version) {
            anyhow::ensure!(
                plan.document_version == reviewed.document.version,
                "plan version changed before the review decision"
            );
            plan.model_revision = plan
                .model_revision
                .checked_add(1)
                .context("plan revision exhausted")?;
            self.plan_file.submit_reviewed_revision(
                &session.id,
                &plan.id,
                plan.model_revision,
                &reviewed,
            )?;
            plan.submitted_version = Some(reviewed.document.version);
            effects.lifecycle.push(PlanLifecycleRecord {
                title: plan.title.clone(),
                anchor: anchor.clone(),
                id: Uuid::new_v4().to_string(),
                session_id: session.id.clone(),
                plan_id: plan.id.clone(),
                kind: PlanLifecycleKind::RevisionCreated,
                model_revision: plan.model_revision,
                user_revision: plan.user_revision + 1,
                overall_comment: Some(plan.manual_changes.join("\n")),
                annotation: Vec::new(),
                question: None,
                answer: None,
                created_at_ms: now_ms,
            });
        }
        plan.user_revision += 1;
        plan.accepted_digest = Some(digest);
        plan.accepted_revision = Some(plan.model_revision);
        plan.acceptance = None;
        if plan.state != PlanState::Accepted {
            PlanStateMachine::apply(&mut plan, PlanEvent::Accepted, now_ms)?;
        }
        let comment = params["comment"]
            .as_str()
            .map(str::trim)
            .filter(|comment| !comment.is_empty())
            .map(str::to_owned);
        effects.lifecycle.push(PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor,
            id: Uuid::new_v4().to_string(),
            session_id: session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::Accepted,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: comment.clone(),
            annotation: Vec::new(),
            question: None,
            answer: None,
            created_at_ms: now_ms,
        });
        let execution = PlanExecutionRecord {
            id: Uuid::new_v4().to_string(),
            session_id: session.id.clone(),
            plan_id: plan.id.clone(),
            baseline_checkpoint: None,
            state: PlanExecutionState::Active,
            implementation_report: None,
            phase: crate::plan::PlanPhase::Implement,
            original_revision: plan.model_revision,
            revision: plan.model_revision,
            generation: 0,
            planning_backend_session_id: session.backend_session_id.clone(),
            execution_backend_session_id: session.backend_session_id.clone(),
            lifecycle: Vec::new(),
            findings: Vec::new(),
            progress: Default::default(),
            verification: Vec::new(),
            check_runs: Vec::new(),
            revision_history: Vec::new(),
            approval_comments: comment.into_iter().collect(),
            pending_revision_reason: None,
            review_paused: false,
            observed_source: Default::default(),
            observed_generation: 0,
            observed_check: Default::default(),
            created_at_ms: now_ms,
            completed_at_ms: None,
        };
        let instruction = PlanPrompt::with_manual_changes(
            PlanPrompt::execution(
                &execution,
                PlanExecutionPromptKind::Start,
                &reviewed.document.execution_json(execution.phase)?,
            ),
            &plan.manual_changes,
        );
        let mut task = crate::task::ActiveTask::execute(
            plan.title.clone(),
            execution,
            crate::session::continuation::ContinuationBudget::new(goal_max_turns, 2),
            false,
            now_ms,
        );
        task.request_entry(instruction, now_ms)?;
        task.entry
            .as_mut()
            .context("execution entry is missing")?
            .execution = Some(crate::task::ExecutionEntry {
            approval_mode,
            executor: session.plan_executor.clone(),
            compact: session.plan_compact,
        });
        effects.plan.push(plan);
        Ok((task, effects))
    }
}
