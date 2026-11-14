use super::*;
use crate::plan::{check_edits, removed_test_edits};
use crate::session::SessionStore;
use forge_provider_api::forge::PlanEdit;

/// Describe review edits one per line for plan lifecycle records.
fn edit_summary(edits: &[PlanEdit]) -> String {
    edits
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}

/// Retain normalized execution configuration while approval decisions and owned collection settle.
pub(crate) struct ValidatedPlanExecution {
    request: crate::task::command::ExecuteSelection,
    approval_modes: Vec<ApprovalMode>,
}

impl ValidatedPlanExecution {
    /// Get the captured request; absent approval mode still requires an explicit review decision.
    pub(crate) fn request(&self) -> &crate::task::command::ExecuteSelection {
        &self.request
    }

    /// Get normalized approval choices without exposing provider protocol fields.
    #[cfg(test)]
    pub(crate) fn approval_modes(&self) -> &[ApprovalMode] {
        &self.approval_modes
    }
}

/// Separate a frozen executable request from a durable request for reviewer input.
pub(crate) enum PlanExecutionPreparation {
    Ready(ValidatedPlanExecution),
    AwaitingInput {
        change: Box<super::plan_entry::PreparedTaskChange>,
        question_set: crate::exchange::command::PlanQuestionSetSelection,
    },
}

/// Keep read-only semantic validation separate from artifact preparation and task mutation.
struct ExecutionCapture {
    session: crate::session::HarnessSession,
    plan: PlanRecord,
    reviewed: crate::plan::review_source::PlanReviewSource,
    approval_mode: ApprovalMode,
    approval_modes: Vec<ApprovalMode>,
    feedback: Option<(Vec<crate::plan::PlanAnnotationInput>, Option<String>)>,
}

impl BrokerObserver {
    /// Apply one captured plan-document edit and return guarded finite domain effects.
    ///
    /// `operation_id` correlates the edit's trace stages with the caller's request.
    pub(crate) async fn prepare_owner_plan_edit(
        &self,
        edit: &crate::exchange::command::PlanEdit,
        operation_id: &str,
        current: Option<&crate::task::Task>,
        now_ms: i64,
    ) -> std::result::Result<
        super::plan_entry::PreparedTaskChange,
        crate::exchange::command::TaskCommandFailure,
    > {
        use crate::exchange::command::{PlanEditChange, PlanEntitySelection, TaskCommandFailure};
        edit.validate_target(current)?;
        let store = crate::storage::HistoryReader::current(&self.data_root)
            .map_err(|_| TaskCommandFailure::StateUnavailable)?;
        let mut plan = store
            .load_plan(edit.source().id().as_str())
            .map_err(|_| TaskCommandFailure::StateUnavailable)?
            .ok_or(TaskCommandFailure::PlanMissing)?;
        edit.source().validate(&plan)?;
        if plan.session_id != self.session_id {
            return Err(TaskCommandFailure::PlanChanged);
        }
        let digest = plan
            .review_digest
            .clone()
            .ok_or(TaskCommandFailure::PlanChanged)?;
        let document = self
            .plan_file
            .read_working_document(&self.session_id, &plan.id)
            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?;
        let current_digest = crate::codec::to_vec(&document)
            .map(|bytes| crate::plan::digest(&bytes))
            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?;
        if document.version != plan.document_version || current_digest != digest {
            return Err(TaskCommandFailure::PlanChanged);
        }
        self.plan_file
            .authoring_require_current(&self.session_id, &document)
            .map_err(|_| TaskCommandFailure::PlanChanged)?;
        let changed: Result<_> = async {
            match &edit.request().change {
                PlanEditChange::RenameEntity {
                    target: PlanEntitySelection::Symbol(symbol),
                    name,
                } => {
                    let trace = crate::plan::edit_trace::PlanEditTrace::new(
                        Arc::clone(&self.trace),
                        &self.session_id,
                        "session.plan.entity.rename",
                        &json!({"operation_id": operation_id, "plan_id": plan.id,
                            "expected_version": document.version}),
                    );
                    let (previous, document, _, checksum) =
                        self.plan_file.rename_symbol_from_document(
                            &self.session_id,
                            &document,
                            symbol,
                            name,
                            Some(&trace),
                        )?;
                    Ok((
                        document,
                        checksum,
                        vec![PlanEdit::Rename {
                            from: previous.to_string(),
                            to: name.clone(),
                        }],
                    ))
                }
                PlanEditChange::RenameEntity {
                    target: PlanEntitySelection::AddedEntity(entity),
                    name,
                } => {
                    let result = self.plan_file.rename_added_entity_from_document(
                        &self.session_id,
                        &document,
                        entity,
                        name.clone(),
                    )?;
                    let checksum = crate::plan::digest(&crate::codec::to_vec(&result.document)?);
                    Ok((
                        result.document,
                        checksum,
                        vec![PlanEdit::Rename {
                            from: entity.to_string(),
                            to: name.clone(),
                        }],
                    ))
                }
                PlanEditChange::DeleteTests { tests } => {
                    let (document, _, checksum) = self.plan_file.remove_tests(
                        &self.session_id,
                        &plan.id,
                        plan.document_version,
                        &digest,
                        tests,
                        None,
                    )?;
                    Ok((document, checksum, removed_test_edits(tests)))
                }
                PlanEditChange::UpdateChecks {
                    enabled,
                    test_scopes,
                    tests,
                } => {
                    let (document, _, checksum) = self
                        .plan_file
                        .update_checks(
                            &self.session_id,
                            &plan.id,
                            plan.document_version,
                            &digest,
                            crate::plan::PlanCheckSelection {
                                enabled,
                                test_scopes,
                                removed_tests: tests,
                            },
                            None,
                        )
                        .await?;
                    let mut edits = check_edits(enabled);
                    edits.extend(crate::plan::test_scope_edits(test_scopes));
                    edits.extend(removed_test_edits(tests));
                    Ok((document, checksum, edits))
                }
            }
        }
        .await;
        let (document, checksum, edits) =
            changed.map_err(|_| TaskCommandFailure::PreparationUnavailable)?;
        plan.document_version = document.version;
        plan.title = document.title;
        plan.review_digest = Some(checksum);
        plan.review = None;
        plan.manual_changes.extend(edits);
        plan.updated_at_ms = now_ms;
        self.plan_file
            .seal_plan_artifacts(&plan)
            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?;
        let mut effects = crate::storage::TaskEffects::default();
        effects
            .source_plan
            .push(crate::exchange::command::PlanSourceGuard::Working(
                edit.source().clone(),
            ));
        effects.plan.push(plan.clone());
        let change = if let Some(task) = current.filter(|task| {
            matches!(&task.workflow,
            crate::task::WorkflowSnapshot::Plan { plan: active } if active.artifact.id == plan.id)
        }) {
            let mut draft = task.draft();
            if let crate::task::WorkflowSnapshot::Plan { plan: active } = &mut draft.workflow {
                active.artifact = plan;
            }
            crate::task::TaskChange::Update(Box::new(
                crate::task::Task::try_from(draft)
                    .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
            ))
        } else {
            crate::task::TaskChange::Preserve
        };
        Ok(super::plan_entry::PreparedTaskChange { change, effects })
    }

    /// Reconcile retained plan artifacts against the domain record committed by the owner.
    pub(crate) fn reconcile_plan_artifacts(&self, plan: &PlanRecord) -> Result<()> {
        self.plan_file.reconcile_plan_artifacts(plan)
    }

    pub(super) fn prepare_review_change(
        &self,
        action: &crate::task::command::SessionAction,
        current: Option<&crate::task::Task>,
        now_ms: i64,
    ) -> Result<super::plan_entry::PreparedTaskChange> {
        use crate::task::command::SessionAction;
        use crate::task::{Task, TaskChange, WorkflowSnapshot};
        let review = match action {
            SessionAction::RequestChanges { review } | SessionAction::CancelPlan { review } => {
                review
            }
            _ => anyhow::bail!("command is not a plan review decision"),
        };
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let plan_id = review
            .plan_id
            .as_deref()
            .or_else(|| {
                current.and_then(|task| match &task.workflow {
                    WorkflowSnapshot::Plan { plan } => Some(plan.artifact.id.as_str()),
                    WorkflowSnapshot::Execute { execution } => {
                        Some(execution.record.plan_id.as_str())
                    }
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
        let mut task = current.map(Task::draft);
        let executing=task.as_ref().is_some_and(|task| matches!(&task.workflow,WorkflowSnapshot::Execute { execution } if execution.record.plan_id==plan.id && execution.record.pending_revision_reason.is_some()));
        if matches!(action, SessionAction::CancelPlan { .. }) {
            if executing {
                let task = task.as_mut().unwrap();
                let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                    unreachable!()
                };
                let prior = plan.clone();
                let digest = plan
                    .review_digest
                    .as_deref()
                    .context("review digest is missing")?;
                let source = self.capture_plan_review(&plan.id, digest)?;
                if let Some(expected) = &review.saved_source_digest {
                    anyhow::ensure!(
                        expected == &source.saved_digest,
                        "saved plan source changed before rejection"
                    );
                }
                let accepted = self.plan_file.read_submitted_document(
                    &self.session_id,
                    &plan.id,
                    execution.record.revision,
                )?;
                plan.state = PlanState::Accepted;
                plan.document_version = accepted.version;
                plan.title = accepted.title;
                plan.review_digest = plan.accepted_digest.clone();
                plan.review = None;
                self.plan_file.restore_revision_guarded(
                    &prior,
                    &plan,
                    execution.record.revision,
                    &source,
                )?;
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
                plan.review = None;
                effects.lifecycle.push(self.review_lifecycle(
                    &plan,
                    anchor.clone(),
                    PlanLifecycleKind::Cancelled,
                    None,
                    Vec::new(),
                    now_ms,
                ));
                self.plan_file.seal_plan_artifacts(&plan)?;
                effects.plan.push(plan.clone());
                let clearing=current.is_some_and(|task| matches!(&task.workflow,WorkflowSnapshot::Plan { plan:active } if active.artifact.id==plan.id));
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
            }
            {
                self.prepare_review_revision(
                    &mut plan,
                    &reviewed,
                    anchor.clone(),
                    &mut effects,
                    now_ms,
                )?;
                plan.user_revision += 1;
                {
                    let annotation = crate::plan::resolve_annotations(
                        &reviewed.rendered,
                        review.annotations.clone(),
                    )?;
                    let feedback =
                        crate::plan::render_review_feedback(&reviewed.document, &annotation)?;
                    let instruction = PlanPrompt::revision(
                        feedback.clone(),
                        review.comment.as_deref(),
                        plan.manual_changes.clone(),
                    );
                    plan.review = None;
                    if executing {
                        let task = task.as_mut().unwrap();
                        let WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
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
                        plan.generation.reset_no_progress();
                        let mut revised=current.filter(|task|matches!(&task.workflow,WorkflowSnapshot::Plan { plan:active } if active.artifact.id==plan.id)).map(Task::draft).unwrap_or_else(||crate::task::TaskSnapshot::plan(plan.clone(),now_ms));
                        if let WorkflowSnapshot::Plan { plan: active } = &mut revised.workflow {
                            active.artifact = plan.clone();
                            active.paused = false;
                        }
                        revised.request_entry(instruction, now_ms)?;
                        revised
                            .entry
                            .as_mut()
                            .context("revision entry is missing")?
                            .review_exchange =
                            anchor.as_ref().map(|anchor| anchor.exchange_id.clone());
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
        self.plan_file.seal_plan_artifacts(&plan)?;
        effects.plan.push(plan.clone());
        let change = if executing {
            crate::task::TaskChange::Update(Box::new(Task::try_from(task.unwrap())?))
        } else {
            let mut prepared=task.filter(|task|matches!(&task.workflow,WorkflowSnapshot::Plan { plan:active } if active.artifact.id==plan.id)).unwrap_or_else(||crate::task::TaskSnapshot::plan(plan.clone(),now_ms));
            if let WorkflowSnapshot::Plan { plan: active } = &mut prepared.workflow {
                active.artifact = plan;
                active.paused = false;
            }
            if current.is_some_and(|task|matches!(&task.workflow,WorkflowSnapshot::Plan { plan:active } if matches!(&prepared.workflow,WorkflowSnapshot::Plan { plan } if plan.artifact.id==active.artifact.id))) { TaskChange::Update(Box::new(Task::try_from(prepared)?)) } else { TaskChange::Install(Box::new(Task::try_from(prepared)?)) }
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
        let prior = crate::storage::HistoryReader::current(&self.data_root)?
            .load_plan(&plan.id)?
            .context("plan record is missing before artifact preparation")?;
        self.plan_file
            .submit_reviewed_revision_guarded(&prior, plan.model_revision, reviewed)?;
        plan.submitted_version = Some(reviewed.document.version);
        plan.updated_at_ms = now_ms;
        effects.lifecycle.push(self.review_lifecycle(
            plan,
            anchor,
            PlanLifecycleKind::RevisionCreated,
            Some(edit_summary(&plan.manual_changes)),
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
        task: &mut crate::task::TaskSnapshot,
        _plan: &PlanRecord,
        anchor: Option<crate::plan::ExchangeAnchor>,
        title: &str,
        now_ms: i64,
    ) -> Result<()> {
        let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
            anyhow::bail!("review no longer owns an execution task")
        };
        let resume = execution.record.resume_after_review;
        execution.record.resume_after_review = false;
        if resume {
            execution.record.state = PlanExecutionState::Active;
        }
        execution.record.generation += 1;
        let review_exchange = anchor.as_ref().map(|anchor| anchor.exchange_id.clone());
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
            // The user's review edits were reported on the review decision itself.
            let instruction = PlanPrompt::execution(PlanExecutionPromptKind::Continue);
            task.request_entry(instruction, now_ms)?;
            task.entry
                .as_mut()
                .context("review entry is missing")?
                .review_exchange = review_exchange;
        }
        Ok(())
    }

    /// Validate immutable execution input and freeze its normalized configuration without writing artifacts.
    pub(crate) fn validate_plan_execution(
        &self,
        command: &crate::exchange::command::CommandId,
        selection: &crate::exchange::command::PlanExecutionSelection,
    ) -> std::result::Result<ValidatedPlanExecution, crate::exchange::command::TaskCommandFailure>
    {
        let request = selection.normalized_request();
        let captured = self.capture_execution(&request).map_err(|error| {
            tracing::dispatcher::with_default(&self.trace.dispatch(), || {
                tracing::warn!(target: "forge::plan", session_id = %self.session_id, error = %format!("{error:#}"), "Plan execution validation failed");
            });
            self.trace.record(&self.session_id, "exchange.plan_execution.validation_failed", json!({
                "command_id":command.as_str(),"plan_id":selection.source().id().as_str(),"reason":format!("{error:#}"),
            }));
            crate::exchange::command::TaskCommandFailure::PreparationUnavailable
        })?;
        selection.source().validate(&captured.plan)?;
        let store = crate::storage::HistoryReader::current(&self.data_root)
            .map_err(|_| crate::exchange::command::TaskCommandFailure::StateUnavailable)?;
        crate::exchange::command::PlanSourceGuard::Working(selection.source().clone())
            .validate_storage(&store, &self.session_id)?;
        let mut request = request;
        request.saved_source_digest = Some(captured.reviewed.saved_digest.clone());
        request.plan_executor = Some(captured.session.plan_executor.clone());
        request.plan_compact = Some(captured.session.plan_compact);
        Ok(ValidatedPlanExecution {
            request,
            approval_modes: captured.approval_modes,
        })
    }

    /// Prepare reviewer input without writing plan artifacts or replacing an accepted execution.
    pub(crate) fn prepare_plan_execution(
        &self,
        command: &crate::exchange::command::CommandId,
        selection: &crate::exchange::command::PlanExecutionSelection,
        now_ms: i64,
    ) -> std::result::Result<PlanExecutionPreparation, crate::exchange::command::TaskCommandFailure>
    {
        use crate::exchange::command::TaskCommandFailure;
        let mut validated = self.validate_plan_execution(command, selection)?;
        let store = crate::storage::HistoryReader::current(&self.data_root)
            .map_err(|_| TaskCommandFailure::StateUnavailable)?;
        let mut plan = store
            .load_plan(selection.source().id().as_str())
            .map_err(|_| TaskCommandFailure::StateUnavailable)?
            .ok_or(TaskCommandFailure::PlanChanged)?;
        let current = store
            .load_session(&self.session_id)
            .map_err(|_| TaskCommandFailure::StateUnavailable)?
            .ok_or(TaskCommandFailure::StateUnavailable)?
            .active_task;
        selection.validate_target(current.as_ref())?;
        let matching_review = plan.review.as_ref().filter(|review| {
            Some(&review.review_digest) == validated.request.digest.as_ref()
                && review.saved_source_digest == validated.request.saved_source_digest
                && review.approval_mode_list == validated.approval_modes
        });
        if let Some(review) = matching_review {
            if let Some(frozen) = &review.execution_request {
                validated.request.plan_executor = frozen.plan_executor.clone();
                validated.request.plan_compact = frozen.plan_compact;
            }
            if validated.request.comment.is_none() {
                validated.request.comment = review.overall_comment.clone();
            }
            if validated.request.annotations.is_empty() {
                validated.request.annotations = review.annotations.clone();
            }
            if validated.request.approval_mode.is_none()
                && !review.elicitation.has_pending_decisions()
            {
                validated.request.approval_mode = Some(
                    review
                        .approval_mode()
                        .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
                );
            }
        }
        if validated.request.approval_mode.is_some() {
            return Ok(PlanExecutionPreparation::Ready(validated));
        }
        let mut review = match matching_review {
            Some(review) => review.clone(),
            None => PlanReview::new(
                validated
                    .request
                    .digest
                    .clone()
                    .ok_or(TaskCommandFailure::PlanChanged)?,
                &validated.approval_modes,
            )
            .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
        };
        review.saved_source_digest = validated.request.saved_source_digest.clone();
        review.overall_comment = validated.request.comment.clone();
        review.annotations = validated.request.annotations.clone();
        review.execution_request = Some(validated.request);
        let question_set =
            crate::exchange::command::PlanQuestionSetSelection::capture(&review.elicitation)?;
        plan.review = Some(review);
        plan.updated_at_ms = now_ms;
        let mut effects = crate::storage::TaskEffects::default();
        effects.plan_guard.push(crate::storage::PlanArtifactGuard {
            id: plan.id.clone(),
            document_version: plan.document_version,
            review_digest: plan.review_digest.clone(),
        });
        effects.plan.push(plan.clone());
        let change = match current {
            Some(task) if matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan: active } if active.artifact.id == plan.id) =>
            {
                let mut draft = task.draft();
                if let crate::task::WorkflowSnapshot::Plan { plan: active } = &mut draft.workflow {
                    active.artifact = plan;
                }
                crate::task::TaskChange::Update(Box::new(
                    crate::task::Task::try_from(draft)
                        .map_err(|_| TaskCommandFailure::PreparationUnavailable)?,
                ))
            }
            None | Some(_) => crate::task::TaskChange::Preserve,
        };
        Ok(PlanExecutionPreparation::AwaitingInput {
            change: Box::new(super::plan_entry::PreparedTaskChange { change, effects }),
            question_set,
        })
    }

    /// Read execution prerequisites without preparing revisions, changing tasks, or dispatching native work.
    fn capture_execution(
        &self,
        selection: &crate::task::command::ExecuteSelection,
    ) -> Result<ExecutionCapture> {
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let mut session = store
            .load_session(&self.session_id)?
            .context("session is missing")?;
        if let Some(executor) = &selection.plan_executor {
            session.plan_executor = executor.clone();
        }
        if let Some(compact) = selection.plan_compact {
            session.plan_compact = compact;
        }
        let selected = selection.plan_id.as_deref().unwrap_or("last");
        let plan = if selected == "last" {
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
        if let Some(expected) = selection.digest.as_deref() {
            anyhow::ensure!(
                expected == digest,
                "plan changed after the accept action began"
            );
        }
        let reviewed = self.capture_plan_review(&plan.id, &digest)?;
        self.plan_file
            .authoring_require_current(&self.session_id, &reviewed.document)?;
        if let Some(expected) = selection.saved_source_digest.as_deref() {
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
        // The reviewed snapshot describes project defaults; task preferences apply at execution admission.
        let checks = crate::checks::ConfigSnapshot::load(Path::new(&session.workspace))?;
        anyhow::ensure!(
            design.check_configuration.as_ref() == Some(&checks),
            "Configured checks changed. Reopen /plan to review them before acceptance."
        );
        let approval_mode = selection.approval_mode.unwrap_or(session.approval_mode);
        let metadata = self
            .metadata
            .read()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
        anyhow::ensure!(
            !metadata.capability.approval_mode_list.is_empty(),
            "execution has no available approval mode"
        );
        if let Some(mode) = selection.approval_mode {
            anyhow::ensure!(
                metadata.capability.approval_mode_list.contains(&mode),
                "requested execution approval mode is unavailable"
            );
        }
        if session.plan_compact {
            anyhow::ensure!(
                metadata.capability.native_compact && session.provider_session_id.is_some(),
                "current session cannot compact before execution"
            );
        }
        if session.plan_executor.enabled {
            anyhow::ensure!(
                session.plan_executor.model.is_some() && session.plan_executor.effort.is_some(),
                "plan executor model and thinking must be configured"
            );
        }
        let approval_modes = metadata.capability.approval_mode_list.clone();
        drop(metadata);
        // Capture review feedback before any collection or artifact writes.
        let feedback = if !selection.annotations.is_empty() {
            Some((selection.annotations.clone(), selection.comment.clone()))
        } else if let Some(acceptance) = plan
            .review
            .as_ref()
            .filter(|acceptance| !acceptance.annotations.is_empty())
        {
            anyhow::ensure!(
                acceptance.review_digest == digest
                    && acceptance.saved_source_digest.as_deref()
                        == Some(reviewed.saved_digest.as_str()),
                "accepted feedback no longer matches the reviewed plan"
            );
            Some((
                acceptance.annotations.clone(),
                acceptance.overall_comment.clone(),
            ))
        } else {
            None
        };
        if let Some((annotations, _)) = &feedback {
            crate::plan::resolve_annotations(&reviewed.rendered, annotations.clone())?;
        }
        Ok(ExecutionCapture {
            session,
            plan,
            reviewed,
            approval_mode,
            approval_modes,
            feedback,
        })
    }

    pub(crate) fn prepare_execution(
        &self,
        selection: &crate::task::command::ExecuteSelection,
        goal_max_turns: u32,
        now_ms: i64,
    ) -> Result<(crate::task::Task, crate::storage::TaskEffects)> {
        let ExecutionCapture {
            session,
            mut plan,
            reviewed,
            approval_mode,
            approval_modes,
            feedback,
        } = self.capture_execution(selection)?;
        anyhow::ensure!(
            approval_modes.contains(&approval_mode),
            "execution requires an available approval decision"
        );
        let store = crate::storage::HistoryReader::current(&self.data_root)?;
        let digest = plan
            .review_digest
            .clone()
            .context("submitted plan digest is missing")?;
        // Preserve the accepted execution identity and its explicit pause policy when approving a revision.
        if feedback.is_none()
            && let Some(current) = session.active_task.as_ref()
            && matches!(&current.workflow, crate::task::WorkflowSnapshot::Execute { execution }
                if execution.record.plan_id == plan.id && execution.record.pending_revision_reason.is_some())
        {
            let mut effects = crate::storage::TaskEffects::default();
            effects.plan_guard.push(crate::storage::PlanArtifactGuard {
                id: plan.id.clone(),
                document_version: plan.document_version,
                review_digest: plan.review_digest.clone(),
            });
            let anchor = store
                .latest_exchange(&session.id)?
                .as_ref()
                .map(crate::plan::ExchangeAnchor::capture);
            self.prepare_review_revision(
                &mut plan,
                &reviewed,
                anchor.clone(),
                &mut effects,
                now_ms,
            )?;
            let mut task = current.draft();
            let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                unreachable!()
            };
            plan.user_revision += 1;
            plan.accepted_revision = Some(plan.model_revision);
            plan.accepted_digest = plan.review_digest.clone();
            plan.state = PlanState::Accepted;
            plan.review = None;
            execution.record.revision = plan.model_revision;
            execution.record.manual_verification = reviewed
                .document
                .design
                .as_ref()
                .context("accepted execution design is missing")?
                .document
                .manual_checks
                .clone();
            execution
                .record
                .revision_history
                .push(crate::plan::execution::ExecutionRevision {
                    revision: plan.model_revision,
                    reason: execution
                        .record
                        .pending_revision_reason
                        .take()
                        .context("revision reason missing")?,
                    approval: "user".into(),
                    recorded_at_ms: now_ms,
                });
            if let Some(comment) = &selection.comment {
                execution.record.approval_comments.push(comment.clone());
            }
            execution.record.progress = Default::default();
            self.finish_review_execution(
                &mut task,
                &plan,
                anchor.clone(),
                "Plan revision accepted",
                now_ms,
            )?;
            effects.lifecycle.push(self.review_lifecycle(
                &plan,
                anchor,
                PlanLifecycleKind::Accepted,
                selection.comment.clone(),
                Vec::new(),
                now_ms,
            ));
            self.plan_file.seal_plan_artifacts(&plan)?;
            effects.plan.push(plan);
            return Ok((crate::task::Task::try_from(task)?, effects));
        }
        // Prepare captured feedback before admitting implementation work.
        if let Some((annotations, comment)) = feedback {
            let prepared = self.prepare_review_change(
                &crate::task::command::SessionAction::RequestChanges {
                    review: crate::task::command::PlanReviewRequest {
                        plan_id: Some(plan.id.clone()),
                        digest: Some(digest),
                        saved_source_digest: Some(reviewed.saved_digest.clone()),
                        comment: comment.clone(),
                        annotations,
                        ..Default::default()
                    },
                },
                session.active_task.as_ref(),
                now_ms,
            )?;
            let mut task = match prepared.change {
                crate::task::TaskChange::Update(task) | crate::task::TaskChange::Install(task) => {
                    task.draft()
                }
                _ => anyhow::bail!("accepted feedback did not produce a revision task"),
            };
            task.plan_execution = Some(crate::task::PlanExecutionChoice {
                execution: crate::task::ExecutionEntry {
                    approval_mode,
                    executor: session.plan_executor,
                    compact: session.plan_compact,
                },
                comment,
            });
            return Ok((crate::task::Task::try_from(task)?, prepared.effects));
        }
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
            let prior = store
                .load_plan(&plan.id)?
                .context("plan record is missing before artifact preparation")?;
            self.plan_file.submit_reviewed_revision_guarded(
                &prior,
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
                overall_comment: Some(edit_summary(&plan.manual_changes)),
                annotation: Vec::new(),
                question: None,
                answer: None,
                created_at_ms: now_ms,
            });
        }
        plan.user_revision += 1;
        plan.accepted_digest = Some(digest);
        plan.accepted_revision = Some(plan.model_revision);
        plan.review = None;
        if plan.state != PlanState::Accepted {
            PlanStateMachine::apply(&mut plan, PlanEvent::Accepted, now_ms)?;
        }
        let comment = selection
            .comment
            .as_deref()
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
            manual_verification: reviewed
                .document
                .design
                .as_ref()
                .context("accepted execution design is missing")?
                .document
                .manual_checks
                .clone(),
            planning_provider_session_id: session.provider_session_id.clone(),
            execution_provider_session_id: session.provider_session_id.clone(),
            lifecycle: Vec::new(),
            findings: Vec::new(),
            progress: Default::default(),
            verification: Vec::new(),
            verification_draft: None,
            check_runs: Vec::new(),
            revision_history: Vec::new(),
            approval_comments: comment.into_iter().collect(),
            pending_revision_reason: None,
            resume_after_review: false,
            observed_source: Default::default(),
            observed_checkpoint: None,
            observed_document_version: reviewed.document.version,
            observed_check: Default::default(),
            created_at_ms: now_ms,
            completed_at_ms: None,
        };
        // Accepting the plan is the review decision that reports the user's edits.
        let instruction = PlanPrompt::accepted(plan.manual_changes.clone());
        let mut task = crate::task::TaskSnapshot::execute(
            plan.title.clone(),
            execution,
            crate::session::continuation::ContinuationBudget::new(goal_max_turns, 5),
            now_ms,
        );
        let planning_task = session.active_task.as_ref().filter(|task| {
            matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan: active }
                if active.artifact.id == plan.id)
        });
        if let Some(planning_task) = planning_task {
            task.id = planning_task.id.clone();
            task.created_at_ms = planning_task.created_at_ms;
            task.observed_turn = planning_task.observed_turn.clone();
            if let crate::task::WorkflowSnapshot::Plan { plan: active } = &planning_task.workflow
                && let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow
            {
                execution.continuation = active.artifact.generation.budget.clone();
                if execution.continuation.turn_count >= execution.continuation.max_turn_count
                    || execution.continuation.consecutive_no_progress
                        >= execution.continuation.max_consecutive_no_progress
                {
                    execution.record.state = PlanExecutionState::Stalled;
                }
            }
        }
        task.auto_accept_plan = planning_task.is_some_and(|task| task.auto_accept_plan);
        task.plan_execution = planning_task.and_then(|task| task.plan_execution.clone());
        task.request_entry(instruction, now_ms)?;
        task.entry
            .as_mut()
            .context("execution entry is missing")?
            .execution = Some(crate::task::ExecutionEntry {
            approval_mode,
            executor: session.plan_executor.clone(),
            compact: session.plan_compact,
        });
        self.plan_file.seal_plan_artifacts(&plan)?;
        effects.plan.push(plan);
        Ok((crate::task::Task::try_from(task)?, effects))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::execution::ExecutionOperation;

    /// Create a submitted semantic plan without executing it or changing its task ownership.
    fn reviewed_execution_fixture() -> Result<(
        tempfile::TempDir,
        tempfile::TempDir,
        HarnessBroker,
        PlanRecord,
    )> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.save_session()?;
        let mut document = crate::plan::test_fixture("review-plan", "Review ownership");
        let mut design = crate::plan::DeclarationDesign::open(workspace.path())?;
        design.document.objective = "Preserve ownership".into();
        design.document.background = "A review fixture".into();
        design.document.design = "Keep state in its owner.".into();
        design.proposed_source.insert(
            "src/owner.rs".into(),
            "/// Own application state.\npub struct Owner;\n".into(),
        );
        design.parsed =
            crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())?.parsed;
        document.design = Some(design);
        let mut plan = broker
            .plan_file
            .prepare_entry(
                &broker.session.id,
                "Review ownership".into(),
                Some(document),
                1,
            )?
            .record;
        let (document, _, digest) = broker.plan_file.submit_document_revision(
            &broker.session.id,
            &plan.id,
            1,
            plan.document_version,
        )?;
        plan.document_version = document.version;
        plan.submitted_version = Some(document.version);
        plan.model_revision = 1;
        plan.review_digest = Some(digest.clone());
        plan.state = PlanState::AwaitingReview;
        broker.store.save_plan(&plan)?;
        broker.snapshot()?;
        Ok((workspace, data, broker, plan))
    }

    /// Validation must leave artifacts/task state unchanged and guard the later preparation against file edits.
    #[test]
    fn execution_preflight_is_read_only_and_freezes_working_source_authority() -> Result<()> {
        let (_workspace, _data, broker, plan) = reviewed_execution_fixture()?;
        let current = broker.task_controller.current()?;
        let selection = crate::exchange::command::PlanExecutionSelection::capture(
            &plan,
            current.as_ref(),
            crate::task::command::ExecuteSelection {
                plan_compact: Some(false),
                ..Default::default()
            },
        )?;
        let before = serde_json::to_value(broker.store.load_plan(&plan.id)?)?;
        let task_before = serde_json::to_value(current)?;
        let lifecycle_before = broker.store.list_plan_lifecycle(&broker.session.id)?.len();
        let validated = broker.observer.validate_plan_execution(
            &crate::exchange::command::CommandId::new("execution-preflight")?,
            &selection,
        )?;
        assert!(
            validated.request().approval_mode.is_none(),
            "preflight manufactured the missing approval decision"
        );
        assert!(!validated.approval_modes().is_empty());
        assert!(validated.request().saved_source_digest.is_some());
        assert_eq!(validated.request().plan_compact, Some(false));
        assert_eq!(
            serde_json::to_value(broker.store.load_plan(&plan.id)?)?,
            before
        );
        assert_eq!(
            serde_json::to_value(broker.task_controller.current()?)?,
            task_before
        );
        assert_eq!(
            broker.store.list_plan_lifecycle(&broker.session.id)?.len(),
            lifecycle_before
        );
        let mut changed = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)?;
        changed.design.as_mut().unwrap().document.design = "Changed after validation".into();
        broker
            .plan_file
            .write_working_document(&broker.session.id, &plan.id, &changed)?;
        let mut approved = validated.request().clone();
        approved.approval_mode = Some(ApprovalMode::Ask);
        assert!(
            broker
                .observer
                .prepare_execution(&approved, 20, 100)
                .is_err()
        );
        assert_eq!(
            serde_json::to_value(broker.store.load_plan(&plan.id)?)?,
            before
        );
        assert!(
            broker
                .plan_file
                .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
                .is_err()
        );
        Ok(())
    }

    /// An execution in its Resolve phase with one accepted revision and a settled source exchange.
    struct ResolvingExecution {
        broker: HarnessBroker,
        plan: PlanRecord,
        exchange: Exchange,
        // Declared last: fields drop in order, and Windows cannot delete files still open.
        _workspace: tempfile::TempDir,
        _data: tempfile::TempDir,
    }

    /// Accept the reviewed fixture and enter Resolve, after writing `checkpointed_source` into the
    /// workspace so the execution checkpoint contains it.
    async fn resolving_execution(
        checkpointed_source: &[(&str, &str)],
    ) -> Result<ResolvingExecution> {
        let (workspace, data, mut broker, plan) = reviewed_execution_fixture()?;
        for (path, source) in checkpointed_source {
            let target = workspace.path().join(path);
            std::fs::create_dir_all(target.parent().context("source path has a parent")?)?;
            std::fs::write(target, source)?;
        }
        let digest = plan
            .review_digest
            .clone()
            .context("review digest missing")?;
        broker.dispatch_session_command(
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":digest,"approval_mode":"ask","plan_compact":false}),
        )?;
        broker.ensure_execution_baseline().await?;
        let task = broker.task_controller.current()?.unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 2, |task| {
                let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow
                else {
                    anyhow::bail!("expected execution");
                };
                execution.record.phase = crate::plan::PlanPhase::Resolve;
                task.entry.as_mut().unwrap().delivery = crate::task::TaskInputDelivery::Accepted;
                Ok(())
            })?;
        let exchange: Exchange = serde_json::from_value(json!({
            "id":"review-exchange","session_id":broker.session.id,
            "agent_id":broker.session.primary_agent_id,"ordinal":1,
            "prompt":"Revise ownership","state":"complete","kind":"plan_execution",
            "plan_id":plan.id,"created_at_ms":1,"completed_at_ms":2,
            "attributed_matches_checkpoint":true,"node_list":[]
        }))?;
        broker.store.save_exchange(&exchange)?;
        Ok(ResolvingExecution {
            _workspace: workspace,
            _data: data,
            broker,
            plan,
            exchange,
        })
    }

    /// Enter Checking with a settled source turn, without starting a scan or a process.
    async fn checking_execution() -> Result<ResolvingExecution> {
        let mut fixture = resolving_execution(&[]).await?;
        let broker = &mut fixture.broker;
        let task = broker.task_controller.current()?.unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 3, |task| {
                let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow
                else {
                    anyhow::bail!("expected execution");
                };
                execution.record.phase = crate::plan::PlanPhase::Checking;
                Ok(())
            })?;
        let task = broker.task_controller.current()?.unwrap();
        assert!(broker.task_controller.reserve_checks(
            &mut broker.store,
            &task.permit(),
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 0
            },
            4,
        )?);
        broker.reconcile_timeline()?;
        Ok(fixture)
    }

    /// Collect an empty configured gate after implementing the fixture's accepted declaration.
    async fn prepared_check_finalization() -> Result<(
        ResolvingExecution,
        crate::broker::checks::TaskCheckFinalization,
    )> {
        let mut fixture = checking_execution().await?;
        std::fs::create_dir_all(fixture._workspace.path().join("src"))?;
        std::fs::write(
            fixture._workspace.path().join("src/owner.rs"),
            "/// Own application state.\npub struct Owner;\n",
        )?;
        let broker = &mut fixture.broker;
        let (mut state, scan) = broker.begin_owned_task_checks()?;
        let scan = scan.run().await?;
        let run = broker.prepare_owned_task_check_run(&mut state, scan)?;
        // The fixture has no configured commands or test adapters; this launches no processes.
        let result = run.run().await?;
        let scan = broker.prepare_task_check_postscan(&state)?.run().await?;
        let finalization = broker.prepare_task_check_finalization(state, result, scan)?;
        Ok((fixture, finalization))
    }

    /// Keep other writers excluded through report capture and the durable completion transition.
    #[tokio::test]
    async fn check_finalization_retains_repository_access_through_commit() -> Result<()> {
        let (mut fixture, mut finalization) = prepared_check_finalization().await?;
        let report = finalization.capture_report().await?;
        assert!(
            crate::checks::CheckReservation::acquire(
                &(fixture._workspace.path()).join("harness-state"),
                fixture._workspace.path(),
                crate::checks::CheckPurpose::Prepare,
            )
            .is_err()
        );
        fixture
            .broker
            .commit_task_check_finalization(finalization, report)?;
        let _writer = crate::checks::CheckReservation::acquire(
            &(fixture._workspace.path()).join("harness-state"),
            fixture._workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        assert!(fixture.broker.task_controller.current()?.is_none());
        Ok(())
    }

    /// A failed completion write releases read access without marking the execution complete.
    #[tokio::test]
    async fn check_finalization_write_failure_releases_repository_access() -> Result<()> {
        let (mut fixture, mut finalization) = prepared_check_finalization().await?;
        let report = finalization.capture_report().await?;
        let database = crate::storage::test::raw_connection(fixture._data.path())?;
        database.execute_batch(
            "CREATE TRIGGER reject_check_completion BEFORE UPDATE ON session_record
             BEGIN SELECT RAISE(ABORT,'completion fixture write failure'); END;",
        )?;
        let error = fixture
            .broker
            .commit_task_check_finalization(finalization, report)
            .err()
            .context("completion ignored its failed write")?;
        assert!(format!("{error:#}").contains("completion fixture write failure"));
        assert_eq!(
            fixture.broker.current_execution()?.unwrap().phase,
            crate::plan::PlanPhase::Checking
        );
        let _writer = crate::checks::CheckReservation::acquire(
            &(fixture._workspace.path()).join("harness-state"),
            fixture._workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        Ok(())
    }

    /// Recheck configuration after report capture instead of publishing stale passing evidence.
    #[tokio::test]
    async fn check_finalization_rejects_configuration_changed_during_report() -> Result<()> {
        let (mut fixture, mut finalization) = prepared_check_finalization().await?;
        let report = finalization.capture_report().await?;
        let path = fixture._workspace.path().join(".forge.json");
        let mut config: serde_json::Value = serde_json::from_slice(&std::fs::read(&path)?)?;
        config["checks"]["late-check"] = json!({
            "trigger": {}, "action": { "category": "custom", "command": "must-not-run" }
        });
        std::fs::write(path, serde_json::to_vec(&config)?)?;
        let error = fixture
            .broker
            .commit_task_check_finalization(finalization, report)
            .err()
            .context("completion accepted changed configuration")?;
        assert!(error.to_string().contains("check configuration changed"));
        assert_eq!(
            fixture.broker.current_execution()?.unwrap().phase,
            crate::plan::PlanPhase::Checking
        );
        let _writer = crate::checks::CheckReservation::acquire(
            &(fixture._workspace.path()).join("harness-state"),
            fixture._workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        Ok(())
    }

    /// Require protected source evidence for ordinary goals and retain it through settlement.
    #[tokio::test]
    async fn goal_completion_requires_and_retains_reserved_workspace() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.dispatch_session_command(
            "session.goal.set",
            json!({"objective":"Complete with protected evidence"}),
        )?;
        let task = broker
            .task_controller
            .current()?
            .context("goal task missing")?;
        let entry = task.entry.as_ref().context("goal entry missing")?;
        broker.task_controller.acknowledge_entry(
            &mut broker.store,
            &task.revision,
            &entry.id,
            1,
        )?;
        let task = broker.task_controller.current()?.unwrap();
        let evidence = || crate::goal::TurnEvidence {
            structured_complete: true,
            ..Default::default()
        };
        let turn = crate::task::TaskTurnId {
            exchange_ordinal: 1,
            turn_ordinal: 1,
        };
        let error = broker
            .apply_goal_evidence_strict(
                evidence(),
                Some(task.permit()),
                turn.clone(),
                None,
                None,
                &mut Vec::new(),
            )
            .expect_err("completion accepted unprotected evidence");
        assert!(error.to_string().contains("reserved workspace evidence"));
        let snapshot = broker.prepare_workspace_capture().run_reserved().await?;
        assert!(
            crate::checks::CheckReservation::acquire(
                &(workspace.path()).join("harness-state"),
                workspace.path(),
                crate::checks::CheckPurpose::Prepare
            )
            .is_err()
        );
        assert!(!broker.apply_goal_evidence_strict(
            evidence(),
            Some(task.permit()),
            turn,
            None,
            Some((&snapshot, None)),
            &mut Vec::new()
        )?);
        assert!(broker.task_controller.current()?.is_none());
        drop(snapshot);
        let _writer = crate::checks::CheckReservation::acquire(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        Ok(())
    }

    /// Cancellation before the initial scan retains the admitted attempt in both histories.
    #[tokio::test]
    async fn check_admission_survives_cancellation_before_initial_scan() -> Result<()> {
        use super::super::owned_task_checks::{
            OwnedTaskCheckRun, TaskCheckEvent, TaskCheckProgress,
        };

        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (sink, mut stream) = super::super::publication::channel();
        let publication = tokio::spawn(async move {
            while stream.recv().await?.is_some() {}
            Ok::<_, anyhow::Error>(())
        });
        let mut owner = OwnedTaskCheckRun::prepare(broker, sink)?.context("check owner missing")?;
        let admitted = broker
            .current_execution()?
            .unwrap()
            .check_runs
            .pop()
            .unwrap();
        assert_eq!(admitted.state, crate::checks::CheckState::Running);
        assert!(admitted.completed_at_ms.is_none());
        assert!(
            admitted
                .commands
                .iter()
                .all(|command| command.started_at_ms.is_none())
        );
        let event = owner.next().await.context("admission event missing")??;
        assert!(matches!(event, TaskCheckEvent::Admitted(_)));
        owner.cancel()?;
        assert!(matches!(
            owner.update(broker, event)?,
            TaskCheckProgress::Pending
        ));
        let event = owner.next().await.context("interruption event missing")??;
        assert!(matches!(
            owner.update(broker, event)?,
            TaskCheckProgress::Finished { failure: None, .. }
        ));
        let retained = broker
            .current_execution()?
            .unwrap()
            .check_runs
            .pop()
            .unwrap();
        assert_eq!(retained.id, admitted.id);
        assert_eq!(retained.state, crate::checks::CheckState::Interrupted);
        assert!(retained.completed_at_ms.is_some());
        assert!(
            retained
                .commands
                .iter()
                .all(|command| command.started_at_ms.is_none())
        );
        let history = broker.check_runner.history()?;
        let published = history.iter().find(|run| run.id == admitted.id).unwrap();
        assert_eq!(
            serde_json::to_value(published)?,
            serde_json::to_value(&retained)?
        );
        drop(owner);
        publication.await??;
        Ok(())
    }

    /// A changed definition cannot replace the inputs of an already admitted attempt.
    #[tokio::test]
    async fn check_admission_rejects_configuration_changed_during_initial_scan() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (mut state, scan) = broker.begin_owned_task_checks()?;
        let admitted = state.attempt().clone();
        let scanned = scan.run().await?;
        let path = fixture._workspace.path().join(".forge.json");
        let mut config: serde_json::Value = serde_json::from_slice(&std::fs::read(&path)?)?;
        config["checks"]["late-check"] = json!({
            "trigger": {}, "action": { "category": "custom", "command": "must-not-run" }
        });
        std::fs::write(path, serde_json::to_vec(&config)?)?;
        let error = match broker.prepare_owned_task_check_run(&mut state, scanned) {
            Ok(_) => anyhow::bail!("changed configuration replaced admitted inputs"),
            Err(error) => error,
        };
        assert!(
            error
                .to_string()
                .contains("cannot replace an admitted check attempt")
        );
        let retained = broker
            .current_execution()?
            .unwrap()
            .check_runs
            .pop()
            .unwrap();
        assert_eq!(
            serde_json::to_value(&retained)?,
            serde_json::to_value(&admitted)?
        );
        Ok(())
    }

    /// Recovery terminates a pre-scan attempt and pauses execution without losing its identity.
    #[tokio::test]
    async fn check_admission_survives_restart_before_initial_scan() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (state, _scan) = broker.begin_owned_task_checks()?;
        let admitted = state.attempt().clone();
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)?;
        let execution = broker
            .store
            .load_plan_execution(&broker.current_execution()?.unwrap().id)?
            .context("recovered execution missing")?;
        assert_eq!(execution.state, crate::plan::PlanExecutionState::Paused);
        let retained = execution.check_runs.last().unwrap();
        assert_eq!(retained.id, admitted.id);
        assert_eq!(retained.state, crate::checks::CheckState::Interrupted);
        assert!(retained.completed_at_ms.is_some());
        assert!(
            retained
                .commands
                .iter()
                .all(|command| command.started_at_ms.is_none())
        );
        assert!(!retained.lifecycle.is_empty());
        let root = crate::storage::state::directory(fixture._data.path())
            .join(&broker.session.id)
            .join("checks");
        let history = crate::checks::CheckSupervisor::saved_history(&root)?;
        assert_eq!(
            serde_json::to_value(&history[0])?,
            serde_json::to_value(retained)?
        );
        Ok(())
    }

    /// A failed terminal history write cannot commit a successfully paused execution.
    #[tokio::test]
    async fn check_recovery_write_failure_rejects_restart_until_retry() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (state, _scan) = broker.begin_owned_task_checks()?;
        let root = crate::storage::state::directory(fixture._data.path())
            .join(&broker.session.id)
            .join("checks");
        let directory = root.join(&state.attempt().id);
        let obstruction = directory.join("record.tmp");
        std::fs::create_dir_all(&obstruction)?;
        let error = broker
            .store
            .interrupt_detached_execution(&broker.session.id)
            .unwrap_err();
        assert!(format!("{error:#}").contains("persist recovered check"));
        assert!(error.chain().any(|cause| cause.is::<std::io::Error>()));
        let execution = broker.current_execution()?.unwrap();
        assert_eq!(execution.state, crate::plan::PlanExecutionState::Active);
        assert_eq!(
            execution.check_runs[0].state,
            crate::checks::CheckState::Running
        );
        std::fs::remove_dir(&obstruction)?;
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)?;
        let recovered = broker.current_execution()?.unwrap();
        assert_eq!(recovered.state, crate::plan::PlanExecutionState::Paused);
        assert_eq!(
            recovered.check_runs[0].state,
            crate::checks::CheckState::Interrupted
        );
        assert_eq!(
            crate::checks::CheckSupervisor::saved_history(&root)?.len(),
            1
        );
        Ok(())
    }

    /// A rollback after file publication can safely repeat recovery from the old admission.
    #[tokio::test]
    async fn check_recovery_retries_after_sql_commit_failure() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        broker.begin_owned_task_checks()?;
        let database = crate::storage::state::database(fixture._data.path());
        let connection = rusqlite::Connection::open(&database)?;
        connection.execute_batch("CREATE TRIGGER reject_recovery BEFORE UPDATE ON session_record BEGIN SELECT RAISE(FAIL, 'recovery fixture SQL failure'); END;")?;
        drop(connection);
        let error = broker
            .store
            .interrupt_detached_execution(&broker.session.id)
            .unwrap_err();
        assert!(format!("{error:#}").contains("recovery fixture SQL failure"));
        assert_eq!(
            broker.current_execution()?.unwrap().check_runs[0].state,
            crate::checks::CheckState::Running
        );
        let root = crate::storage::state::directory(fixture._data.path())
            .join(&broker.session.id)
            .join("checks");
        let history = crate::checks::CheckSupervisor::saved_history(&root)?;
        assert_eq!(history[0].state, crate::checks::CheckState::Interrupted);
        let connection = rusqlite::Connection::open(database)?;
        connection.execute_batch("DROP TRIGGER reject_recovery;")?;
        drop(connection);
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)?;
        let execution = broker.current_execution()?.unwrap();
        assert_eq!(execution.state, crate::plan::PlanExecutionState::Paused);
        assert_eq!(
            serde_json::to_value(&history[0])?,
            serde_json::to_value(&execution.check_runs[0])?
        );
        Ok(())
    }

    /// Invalid referenced history blocks recovery without resetting the record or the task.
    #[tokio::test]
    async fn check_recovery_rejects_corrupt_and_mismatched_history() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (state, _scan) = broker.begin_owned_task_checks()?;
        broker.check_runner.publish(state.attempt(), None).await?;
        let path = crate::storage::state::directory(fixture._data.path())
            .join(&broker.session.id)
            .join("checks")
            .join(&state.attempt().id)
            .join("record.json");
        let original = std::fs::read(&path)?;
        let document: serde_json::Value = serde_json::from_slice(&original)?;
        let mut wrong_version = document.clone();
        wrong_version["version"] = (document["version"].as_u64().unwrap() + 1).into();
        let mut wrong_revision = document;
        wrong_revision["run"]["revision"] = (state.attempt().revision + 1).into();
        for (bytes, reason) in [
            (b"malformed history".to_vec(), "decode check history"),
            (serde_json::to_vec(&wrong_version)?, "incompatible version"),
            (serde_json::to_vec(&wrong_revision)?, "does not match"),
        ] {
            std::fs::write(&path, &bytes)?;
            let error = broker
                .store
                .interrupt_detached_execution(&broker.session.id)
                .unwrap_err();
            assert!(format!("{error:#}").contains(reason), "{error:#}");
            assert_eq!(std::fs::read(&path)?, bytes);
            assert_eq!(
                broker.current_execution()?.unwrap().state,
                crate::plan::PlanExecutionState::Active
            );
        }
        std::fs::write(&path, original)?;
        broker
            .store
            .interrupt_detached_execution(&broker.session.id)?;
        assert_eq!(
            broker.current_execution()?.unwrap().state,
            crate::plan::PlanExecutionState::Paused
        );
        Ok(())
    }

    /// Expiring the admission budget before scanning leaves a failed attempt and its cause.
    #[tokio::test(start_paused = true)]
    async fn check_admission_timeout_before_initial_scan_retains_failure() -> Result<()> {
        let mut fixture = checking_execution().await?;
        let broker = &mut fixture.broker;
        let (state, scan) = broker.begin_owned_task_checks()?;
        let admitted = state.attempt().clone();
        tokio::time::advance(crate::session::InputTimeout::default().duration()).await;
        let error = match scan.run().await {
            Ok(_) => anyhow::bail!("expired initial scan was admitted"),
            Err(error) => error,
        };
        broker.prepare_task_check_postscan(&state)?.run().await?;
        broker.prepare_owned_task_check_failure(&state, &error)?;
        let execution = broker.current_execution()?.unwrap();
        assert_eq!(execution.state, crate::plan::PlanExecutionState::Blocked);
        let retained = execution.check_runs.last().unwrap();
        assert_eq!(retained.id, admitted.id);
        assert_eq!(retained.state, crate::checks::CheckState::Failed);
        assert!(retained.completed_at_ms.is_some());
        assert!(
            retained
                .commands
                .iter()
                .all(|command| command.started_at_ms.is_none())
        );
        assert!(
            retained
                .lifecycle
                .iter()
                .any(|event| event.text.contains(&error.to_string()))
        );
        Ok(())
    }

    /// A phase tool acknowledges a proposal without changing the live execution.
    #[tokio::test]
    async fn phase_completion_remains_pending_without_normal_native_completion() -> Result<()> {
        phase_completion_case(false).await
    }

    /// A native completion cannot certify workspace content changed after validation.
    #[tokio::test]
    async fn phase_completion_rejects_workspace_changed_after_request() -> Result<()> {
        phase_completion_case(true).await
    }

    async fn phase_completion_case(changed: bool) -> Result<()> {
        let mut fixture = resolving_execution(&[]).await?;
        let address = crate::turn::test_support::address(&fixture.exchange, "phase-native-turn");
        fixture
            .exchange
            .turn
            .push(crate::turn::Turn::new("phase-turn".into(), address, 1));
        let broker = &mut fixture.broker;
        let task = broker.task_controller.current()?.unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 3, |task| {
                let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow
                else {
                    anyhow::bail!("expected execution");
                };
                execution.record.phase = crate::plan::PlanPhase::Implement;
                Ok(())
            })?;
        let task = broker.task_controller.current()?.unwrap();
        let execution = broker.current_execution()?.unwrap();
        let evidence = broker
            .prepare_execution_control(
                &execution.id,
                execution.generation,
                &task.permit(),
                ExecutionOperation::Phase(crate::plan::execution::PlanPhaseDone {
                    phase: crate::plan::PlanPhase::Implement,
                    revision: execution.revision,
                    summary: "Implementation is ready".into(),
                    verification: None,
                }),
                &fixture.exchange,
            )?
            .run()
            .await?;
        let crate::broker::execution::ExecutionControlStage::Pending(pending) =
            broker.apply_execution_control(evidence, &mut fixture.exchange)?
        else {
            anyhow::bail!("phase completion must remain pending");
        };
        assert_eq!(pending.acknowledgement()["result"], "requested");
        let current = broker.current_execution()?.unwrap();
        assert_eq!(current.phase, crate::plan::PlanPhase::Implement);
        assert_eq!(current.generation, execution.generation);
        assert!(
            broker
                .settle_execution_phase(
                    (*pending).clone(),
                    &broker.prepare_workspace_capture().run_reserved().await?,
                    &mut fixture.exchange
                )
                .is_err()
        );
        assert_eq!(
            broker.current_execution()?.unwrap().phase,
            crate::plan::PlanPhase::Implement
        );
        broker
            .store
            .record_terminal_request(&broker.session.id, pending.request_id(), &pending)?;
        fixture.exchange.turn.last_mut().unwrap().finish(
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            5,
        )?;
        if changed {
            std::fs::write(
                fixture._workspace.path().join("after-request.txt"),
                "late writer",
            )?;
        }
        let checkpoint = broker.prepare_workspace_capture().run_reserved().await?;
        assert!(
            crate::checks::CheckReservation::acquire(
                &(fixture._workspace.path()).join("harness-state"),
                fixture._workspace.path(),
                crate::checks::CheckPurpose::Prepare,
            )
            .is_err()
        );
        let result =
            broker.settle_execution_phase((*pending).clone(), &checkpoint, &mut fixture.exchange);
        if changed {
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("workspace changed")
            );
            assert_eq!(
                broker.current_execution()?.unwrap().phase,
                crate::plan::PlanPhase::Implement
            );
        } else {
            result?;
            assert_eq!(
                broker.current_execution()?.unwrap().phase,
                crate::plan::PlanPhase::Checking
            );
            let before = broker.task_controller.current()?.unwrap();
            let reservation = before
                .turn
                .as_ref()
                .unwrap()
                .continuation
                .as_ref()
                .unwrap()
                .id
                .clone();
            broker.settle_execution_phase(*pending, &checkpoint, &mut fixture.exchange)?;
            let after = broker.task_controller.current()?.unwrap();
            assert_eq!(
                after
                    .turn
                    .as_ref()
                    .unwrap()
                    .continuation
                    .as_ref()
                    .unwrap()
                    .id,
                reservation
            );
            assert_eq!(after.revision, before.revision);
        }
        drop(checkpoint);
        let _writer = crate::checks::CheckReservation::acquire(
            &(fixture._workspace.path()).join("harness-state"),
            fixture._workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        Ok(())
    }

    /// Draft and submit one execution-owned revision, returning the stored plan it produced.
    async fn submit_execution_revision(
        execution_fixture: &mut ResolvingExecution,
        revision: u32,
    ) -> Result<PlanRecord> {
        let ResolvingExecution {
            broker,
            plan,
            exchange,
            ..
        } = execution_fixture;
        let task = broker.task_controller.current()?.unwrap();
        let execution = broker.current_execution()?.unwrap();
        let mut document = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)?;
        document.version += 1;
        document.design.as_mut().unwrap().document.design =
            format!("Keep state in its owner, revision {revision}.");
        broker
            .handle_execution_control(
                &execution.id,
                execution.generation,
                &task.permit(),
                ExecutionOperation::Draft(Box::new(document.clone())),
                exchange,
            )
            .await?;
        let task = broker.task_controller.current()?.unwrap();
        let execution = broker.current_execution()?.unwrap();
        let baseline = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)?;
        broker
            .handle_execution_control(
                &execution.id,
                execution.generation,
                &task.permit(),
                ExecutionOperation::Submit {
                    baseline: Box::new(baseline),
                    document: Box::new(document),
                    reason: "Clarify ownership".into(),
                },
                exchange,
            )
            .await?;
        Ok(broker.store.load_plan(&plan.id)?.unwrap())
    }

    /// A file first captured during Resolve takes its baseline from the execution checkpoint
    /// through the same capture as planning, so its classified calls and editable text describe
    /// the checkpointed source rather than the edited workspace.
    #[tokio::test]
    async fn resolve_draft_recaptures_new_baselines_from_the_checkpoint() -> Result<()> {
        let checkpointed =
            "/// Help once.\nfn helper() {}\n/// Run the helper.\nfn run() {\n    helper();\n}\n";
        let execution_fixture = resolving_execution(&[("src/extra.rs", checkpointed)]).await?;
        let ResolvingExecution {
            broker,
            plan,
            exchange,
            _workspace: workspace,
            ..
        } = &mut { execution_fixture };
        std::fs::write(
            workspace.path().join("src/extra.rs"),
            "/// Help once.\nfn helper() {}\n/// Run the helper twice.\nfn run() {\n    helper();\n    helper();\n}\n",
        )?;
        let mut document = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)?;
        document.version += 1;
        let design = document.design.as_mut().unwrap();
        // A plan edit that captures a file removes the submitted structure, as this draft does.
        design.parsed = None;
        let (edited, edited_calls) = design.source(workspace.path(), "src/extra.rs")?;
        design
            .proposed_source
            .insert("src/extra.rs".into(), edited.editable_text.clone());
        design.baseline.insert("src/extra.rs".into(), edited);
        design
            .baseline_calls
            .insert("src/extra.rs".into(), edited_calls);
        let task = broker.task_controller.current()?.unwrap();
        let execution = broker.current_execution()?.unwrap();
        broker
            .handle_execution_control(
                &execution.id,
                execution.generation,
                &task.permit(),
                ExecutionOperation::Draft(Box::new(document)),
                exchange,
            )
            .await?;

        let saved = broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)?;
        let saved = saved.design.as_ref().unwrap();
        let (expected, expected_calls) = saved.capture_baseline("src/extra.rs", checkpointed)?;
        assert_eq!(
            saved.baseline["src/extra.rs"], expected,
            "the recaptured baseline describes the checkpointed source"
        );
        assert!(expected_calls.is_empty());
        assert!(!saved.baseline_calls.contains_key("src/extra.rs"));
        assert_ne!(
            saved.proposed_source["src/extra.rs"], expected.editable_text,
            "the proposal keeps the text the draft carried"
        );
        Ok(())
    }

    /// Review annotations on an execution revision request changes and keep acceptance explicit.
    #[tokio::test]
    async fn inline_execution_review_annotations_request_changes() -> Result<()> {
        let mut execution_fixture = resolving_execution(&[]).await?;
        let submitted = submit_execution_revision(&mut execution_fixture, 2).await?;
        let ResolvingExecution { broker, plan, .. } = &mut execution_fixture;
        assert_eq!(submitted.state, PlanState::AwaitingReview);
        let waiting = broker.task_controller.current()?.unwrap();
        assert_eq!(waiting.status(), crate::task::TaskStatus::Waiting);
        assert_eq!(waiting.phase(), crate::task::TaskPhase::Resolve);
        assert_eq!(waiting.next_action(), crate::task::TaskNextAction::Wait);
        assert!(!waiting.auto_accept_plan);
        let source = broker
            .observer
            .capture_plan_review(&plan.id, submitted.review_digest.as_deref().unwrap())?;
        let line = source.rendered.navigation.anchor.first().unwrap().line;
        let (_, events) = broker.dispatch_session_command(
            "session.plan.accept",
            json!({
                "plan_id":plan.id,"digest":submitted.review_digest,"approval_mode":"ask",
                "annotations":[{"start_line":line,"end_line":line,"body":"Clarify the owner."}]
            }),
        )?;
        assert!(
            events
                .iter()
                .any(|event| event.event == "plan_changes_requested")
        );
        let pending = broker.store.load_plan(&plan.id)?.unwrap();
        assert_eq!(pending.state, PlanState::Revising);
        assert!(!broker.task_controller.current()?.unwrap().auto_accept_plan);
        broker.dispatch_session_command(
            "session.command.apply",
            json!({"action":"plan_accept","enabled":true}),
        )?;
        let execution = broker.current_execution()?.unwrap();
        assert!(
            execution
                .findings
                .iter()
                .any(|finding| finding.contains("Clarify the owner."))
        );
        Ok(())
    }
}
