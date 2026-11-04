use super::*;
use crate::provider::session_execution::{
    HostEffectAdmission, HostEffectGuard, HostEffectId, HostEffectOutput, ResultEffectAdmission,
    SettlementCell,
};

/// An ordered semantic mutation, prepared against one admitted plan authority.
pub(crate) enum PlanMutation {
    Edit(crate::plan::PlanEditRequest),
    Design(crate::plan::DesignEditRequest),
}

/// Own a plan mutation and its receipt while the broker is available to other requests.
pub(crate) struct PlanMutationJob {
    pub(crate) store: Arc<crate::plan::PlanFileStore>,
    pub(crate) session_id: String,
    pub(crate) workspace: PathBuf,
    pub(crate) plan: PlanRecord,
    pub(crate) mutation: PlanMutation,
    pub(crate) id: HostEffectId,
    pub(crate) cell: Arc<SettlementCell>,
    pub(crate) task_controller: Arc<crate::task::SessionTaskController>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) now_ms: i64,
}

/// Retain a successful write's guard until durable plan metadata is committed.
pub(crate) enum PlanMutationResult {
    Applied(Box<Option<PlanRecord>>),
    Written {
        before: Box<PlanRecord>,
        plan: Box<PlanRecord>,
        document: Box<crate::plan::PlanDocument>,
        guard: HostEffectGuard,
    },
}

/// Validate and render a submitted plan without retaining broker or task authority locks.
pub(crate) struct PlanSubmissionJob {
    pub(crate) store: Arc<crate::plan::PlanFileStore>,
    pub(crate) session_id: String,
    pub(crate) workspace: PathBuf,
    pub(crate) plan: PlanRecord,
    pub(crate) request: crate::provider::PlanSubmitRequest,
    pub(crate) id: HostEffectId,
    pub(crate) cell: Arc<SettlementCell>,
    pub(crate) task_controller: Arc<crate::task::SessionTaskController>,
    pub(crate) permit: crate::task::TaskPermit,
}

/// Keep a submitted revision's receipt until its metadata and presentation are durable.
pub(crate) enum PlanSubmissionResult {
    Applied(Box<HostEffectOutput>),
    Written(Box<PlanSubmissionWrite>),
}

/// Retain one submitted revision and its receipt through host commit.
pub(crate) struct PlanSubmissionWrite {
    before: Box<PlanRecord>,
    plan: Box<PlanRecord>,
    document: Box<crate::plan::PlanDocument>,
    rendered: crate::plan::RenderedPlan,
    digest: String,
    diff_text: String,
    declaration: Option<crate::exchange::DeclarationRevision>,
    warnings: Vec<crate::plan::PlanViolation>,
    guard: HostEffectGuard,
}

/// Normalize a plan question and reserve its identity without retaining broker state.
pub(crate) struct PlanQuestionJob {
    pub(crate) store: Arc<crate::plan::PlanFileStore>,
    pub(crate) session_id: String,
    pub(crate) plan: PlanRecord,
    pub(crate) question: crate::plan::PlanQuestionSet,
    pub(crate) id: HostEffectId,
    pub(crate) cell: Arc<SettlementCell>,
}

pub(crate) enum PlanQuestionResult {
    Applied(Box<HostEffectOutput>),
    Prepared {
        before: Box<PlanRecord>,
        document: Box<crate::plan::PlanDocument>,
        question: crate::plan::PlanQuestionSet,
        guard: HostEffectGuard,
    },
}

impl PlanQuestionJob {
    pub(crate) async fn run(self) -> Result<PlanQuestionResult> {
        if let Some(output) = self.cell.applied_result(self.id)? {
            return Ok(PlanQuestionResult::Applied(output));
        }
        let document = self
            .store
            .read_working_document(&self.session_id, &self.plan.id)?;
        let digest = crate::plan::digest(&serde_json::to_vec(&document)?);
        let guard = match self.cell.begin_plan_result_effect(
            self.id,
            &self.plan.id,
            document.version,
            &digest,
        )? {
            ResultEffectAdmission::Applied(output) => {
                return Ok(PlanQuestionResult::Applied(output));
            }
            ResultEffectAdmission::Apply(guard) => guard,
        };
        let question = self.question.normalize()?;
        Ok(PlanQuestionResult::Prepared {
            before: Box::new(self.plan),
            document: Box::new(document),
            question,
            guard,
        })
    }
}

impl PlanSubmissionJob {
    /// Validate before fencing, then commit only the prepared CAS under task authority.
    pub(crate) async fn run(self) -> Result<PlanSubmissionResult> {
        let Self {
            store,
            session_id,
            workspace,
            mut plan,
            request,
            id,
            cell,
            task_controller,
            permit,
        } = self;
        if let Some(output) = cell.applied_result(id)? {
            return Ok(PlanSubmissionResult::Applied(output));
        }
        let before = plan.clone();
        anyhow::ensure!(
            request.plan_id == plan.id,
            "submitted plan id does not match active plan"
        );
        let original = store.read_working_document(&session_id, &plan.id)?;
        anyhow::ensure!(
            original.version == request.expected_version,
            "plan version changed before submission validation"
        );
        let digest = crate::plan::digest(&serde_json::to_vec(&original)?);
        let mut guard =
            match cell.begin_plan_result_effect(id, &plan.id, original.version, &digest)? {
                ResultEffectAdmission::Applied(output) => {
                    return Ok(PlanSubmissionResult::Applied(output));
                }
                ResultEffectAdmission::Apply(guard) => guard,
            };
        let mut document = original.clone();
        let warnings = if let Some(design) = &mut document.design {
            let validated = design.validated(&workspace).await?;
            let warnings = validated.warnings();
            *design = validated.into_design();
            warnings
        } else {
            Vec::new()
        };
        let previous = if plan.model_revision == 0 {
            None
        } else {
            Some(store.read_submitted_document(&session_id, &plan.id, plan.model_revision)?)
        };
        let revision = plan
            .model_revision
            .checked_add(1)
            .context("plan revision exhausted")?;
        let prepared = store.prepare_validated_submission(
            &session_id,
            &plan.id,
            revision,
            request.expected_version,
            &original,
            document,
        )?;
        guard.begin_mutation();
        let result = task_controller
            .commit_plan_write(&permit, || store.commit_prepared_submission(prepared));
        let (document, rendered, digest) = match result {
            Ok(result) => result,
            Err(error) => {
                if error
                    .downcast_ref::<crate::plan::WorkingDocumentChanged>()
                    .is_some()
                    || error.downcast_ref::<crate::task::StaleTask>().is_some()
                {
                    guard.cancel_definite()?;
                }
                return Err(error);
            }
        };
        plan.model_revision = revision;
        let (diff_text, declaration) = if let Some(design) = &document.design {
            let delta =
                crate::plan::revision::DeclarationDelta::between(previous.as_ref(), design)?;
            (
                delta.files,
                Some(crate::exchange::DeclarationRevision {
                    plan_id: plan.id.clone(),
                    revision,
                    document_diff: delta.document,
                    baseline_paths: delta.baseline_paths,
                }),
            )
        } else {
            let previous_markdown = previous
                .as_ref()
                .map(|document| crate::plan::render_plan_at(document, &workspace))
                .transpose()?
                .map(|rendered| rendered.markdown)
                .unwrap_or_default();
            (
                crate::plan::render_plan_delta(
                    &plan.working_path,
                    &previous_markdown,
                    &rendered.markdown,
                ),
                None,
            )
        };
        Ok(PlanSubmissionResult::Written(Box::new(
            PlanSubmissionWrite {
                before: Box::new(before),
                plan: Box::new(plan),
                document: Box::new(document),
                rendered,
                digest,
                diff_text,
                declaration,
                warnings,
                guard,
            },
        )))
    }
}

/// Retain ordered plan mutations and the task authority advanced by their own commits.
pub(crate) struct PlanMutationQueue {
    plan: PlanRecord,
    permit: crate::task::TaskPermit,
    entry_id: Option<crate::task::TaskInputId>,
    cell: Arc<SettlementCell>,
    pending: std::collections::VecDeque<(HostEffectId, PlanMutation)>,
    in_flight: bool,
}

impl PlanMutationQueue {
    pub(crate) fn permit(&self) -> &crate::task::TaskPermit {
        &self.permit
    }
    pub(crate) fn plan(&self) -> &PlanRecord {
        &self.plan
    }
    /// Consume only the edits represented by this completion's retained batch provenance.
    pub(crate) fn new(
        staged: &mut interaction::StagedHostCompletion,
        plan: PlanRecord,
        permit: crate::task::TaskPermit,
    ) -> Result<Self> {
        let cell = staged
            .settlement
            .clone()
            .context("plan mutation has no settlement owner")?;
        let batches = staged
            .batches
            .as_ref()
            .context("plan mutation has no batch provenance")?;
        let edits: Vec<_> = batches
            .iter()
            .flat_map(|batch| {
                (0..batch.completion.plan_edit.len()).map(move |ordinal| HostEffectId {
                    batch_revision: batch.revision,
                    kind: crate::provider::session_execution::HostEffectKind::PlanEdit,
                    ordinal,
                })
            })
            .collect();
        let designs: Vec<_> = batches
            .iter()
            .flat_map(|batch| {
                (0..batch.completion.design_patch.len()).map(move |ordinal| HostEffectId {
                    batch_revision: batch.revision,
                    kind: crate::provider::session_execution::HostEffectKind::DesignPatch,
                    ordinal,
                })
            })
            .collect();
        anyhow::ensure!(
            edits.len() == staged.output.plan_edit.len()
                && designs.len() == staged.output.design_patch.len(),
            "plan mutation batch provenance does not match completion"
        );
        let mut pending = std::collections::VecDeque::new();
        pending.extend(
            edits
                .into_iter()
                .zip(std::mem::take(&mut staged.output.plan_edit))
                .map(|(id, request)| (id, PlanMutation::Edit(request))),
        );
        pending.extend(
            designs
                .into_iter()
                .zip(std::mem::take(&mut staged.output.design_patch))
                .map(|(id, request)| (id, PlanMutation::Design(request))),
        );
        Ok(Self {
            plan,
            permit,
            entry_id: staged.output.dispatched_entry_id.clone(),
            cell,
            pending,
            in_flight: false,
        })
    }

    /// Prepare one job at a time so its receipt order matches the host mutation order.
    pub(crate) fn next_job(&mut self, broker: &HarnessBroker) -> Result<Option<PlanMutationJob>> {
        anyhow::ensure!(!self.in_flight, "plan mutation is already in flight");
        self.permit
            .validate(broker.task_controller.current()?.as_ref())?;
        let Some((id, mutation)) = self.pending.pop_front() else {
            return Ok(None);
        };
        let job = PlanMutationJob {
            store: broker.plan_file.clone(),
            session_id: broker.session.id.clone(),
            workspace: PathBuf::from(&broker.session.workspace),
            plan: self.plan.clone(),
            mutation,
            id,
            cell: self.cell.clone(),
            task_controller: broker.task_controller.clone(),
            permit: self.permit.clone(),
            now_ms: broker.clock.now_ms(),
        };
        self.in_flight = true;
        Ok(Some(job))
    }

    /// Persist one mutation and refresh only the authority advanced by that same commit.
    pub(crate) fn complete(
        &mut self,
        broker: &mut HarnessBroker,
        result: Result<PlanMutationResult>,
    ) -> Result<()> {
        anyhow::ensure!(self.in_flight, "plan mutation has no active job");
        self.in_flight = false;
        broker.commit_plan_mutation(
            &mut self.plan,
            &self.permit,
            self.entry_id.as_ref(),
            result?,
        )?;
        let current = broker
            .task_controller
            .current()?
            .context("planning task disappeared")?;
        self.permit.validate_observation(Some(&current))?;
        self.permit = current.permit();
        Ok(())
    }
}

impl PlanMutationJob {
    /// Compute first, then guard only the authority check and atomic document replacement.
    pub(crate) async fn run(self) -> Result<PlanMutationResult> {
        let Self {
            store,
            session_id,
            workspace,
            mut plan,
            mutation,
            id,
            cell,
            task_controller,
            permit,
            now_ms,
        } = self;
        if let Some(output) = cell.applied_result(id)? {
            return Ok(PlanMutationResult::Applied(Box::new(
                output.plan_record.clone(),
            )));
        }
        let before = plan.clone();
        let original = store.read_working_document(&session_id, &plan.id)?;
        let digest = crate::plan::digest(&serde_json::to_vec(&original)?);
        let mut guard = match cell.begin_plan_effect(id, &plan.id, original.version, &digest)? {
            HostEffectAdmission::Applied(snapshot) => {
                return Ok(PlanMutationResult::Applied(snapshot));
            }
            HostEffectAdmission::Apply(guard) => guard,
        };
        let updated = match mutation {
            PlanMutation::Edit(request) => {
                if request.plan_id != plan.id {
                    guard.cancel_definite()?;
                    anyhow::bail!("edited plan id does not match active plan");
                }
                match crate::plan::apply_plan_edit(&original, request) {
                    Ok(result) => result.document,
                    Err(error) => {
                        guard.cancel_definite()?;
                        return Err(error);
                    }
                }
            }
            PlanMutation::Design(request) => match original.patch_design(&workspace, request) {
                Ok(document) => document,
                Err(error) => {
                    guard.cancel_definite()?;
                    return Err(error);
                }
            },
        };
        if plan.state == PlanState::AwaitingReview {
            if let Err(error) =
                PlanStateMachine::apply(&mut plan, PlanEvent::ChangesRequested, now_ms)
            {
                guard.cancel_definite()?;
                return Err(error);
            }
            plan.review = None;
            plan.user_revision += 1;
            plan.generation.reset_no_progress();
        }
        plan.document_version = updated.version;
        plan.title = updated.title.clone();
        plan.updated_at_ms = now_ms;
        guard.begin_mutation();
        let result = task_controller.commit_plan_write(&permit, || {
            store.write_working_mutation_guarded(&before, &original, &updated)
        });
        if let Err(error) = result {
            if error
                .downcast_ref::<crate::plan::WorkingDocumentChanged>()
                .is_some()
                || error.downcast_ref::<crate::task::StaleTask>().is_some()
            {
                guard.cancel_definite()?;
            }
            return Err(error);
        }
        Ok(PlanMutationResult::Written {
            before: Box::new(before),
            plan: Box::new(plan),
            document: Box::new(updated),
            guard,
        })
    }
}

impl HarnessBroker {
    /// Commit a plan question only while its task and canonical document remain current.
    pub(crate) fn commit_plan_question(
        &mut self,
        interaction: &mut Exchange,
        current: &mut PlanRecord,
        permit: &crate::task::TaskPermit,
        entry_id: Option<&crate::task::TaskInputId>,
        result: PlanQuestionResult,
    ) -> Result<Vec<SessionEvent>> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        match result {
            PlanQuestionResult::Applied(output) => {
                let output = *output;
                interaction::reconcile_plan_effect(current, output.plan_record.clone())?;
                for node in output.nodes {
                    if !interaction
                        .node_list
                        .iter()
                        .any(|existing| existing.id() == node.id())
                    {
                        interaction.node_list.push(node);
                    }
                }
                if interaction.elicitation().is_none() && output.elicitation.is_some() {
                    interaction.set_elicitation(output.elicitation);
                    interaction.set_awaiting_input(output.awaiting_input);
                }
                Ok(if output.publication_drained {
                    Vec::new()
                } else {
                    output.events
                })
            }
            PlanQuestionResult::Prepared {
                before,
                document,
                question,
                mut guard,
            } => {
                let before = *before;
                let document = *document;
                anyhow::ensure!(
                    serde_json::to_vec(current)? == serde_json::to_vec(&before)?,
                    "plan metadata changed before question"
                );
                let task = self
                    .task_controller
                    .current()?
                    .context("planning task disappeared")?;
                permit.validate(Some(&task))?;
                anyhow::ensure!(
                    task.entry.as_ref().map(|entry| &entry.id) == entry_id,
                    "planning task entry changed"
                );
                let mut plan = before;
                let mut candidate = interaction.clone();
                let mut events = Vec::new();
                let mut lifecycle = Vec::new();
                if let Some(question) = plan.question_ledger.unresolved(question) {
                    match plan.elicitation.as_mut() {
                        Some(elicitation) => elicitation.replace_question_set(question.clone()),
                        None => plan.elicitation = Some(PlanElicitation::new(question.clone())),
                    }
                    PlanStateMachine::apply(
                        &mut plan,
                        PlanEvent::QuestionAsked,
                        self.clock.now_ms(),
                    )?;
                    candidate.set_awaiting_input(true);
                    let record = PlanLifecycleRecord {
                        title: plan.title.clone(),
                        anchor: Some(crate::plan::ExchangeAnchor::capture(&candidate)),
                        id: Uuid::new_v4().to_string(),
                        session_id: self.session.id.clone(),
                        plan_id: plan.id.clone(),
                        kind: PlanLifecycleKind::QuestionAsked,
                        model_revision: plan.model_revision,
                        user_revision: plan.user_revision,
                        overall_comment: None,
                        annotation: Vec::new(),
                        question: Some(question.clone()),
                        answer: None,
                        created_at_ms: self.clock.now_ms(),
                    };
                    events.push(self.event(
                        "plan_question",
                        json!({"plan":plan,"lifecycle":record,"question":question}),
                    )?);
                    lifecycle.push(record);
                } else {
                    plan.updated_at_ms = self.clock.now_ms();
                }
                guard.begin_mutation();
                let result = self.task_controller.apply_with_plan_document(
                    &mut self.store,
                    permit,
                    self.plan_file.guard_working_document(&document),
                    crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
                    crate::storage::TaskEffects {
                        lifecycle,
                        exchange: vec![candidate.clone()],
                        ..Default::default()
                    },
                    self.clock.now_ms(),
                );
                if let Err(error) = result {
                    if error
                        .downcast_ref::<crate::plan::WorkingDocumentChanged>()
                        .is_some()
                        || error.downcast_ref::<crate::task::StaleTask>().is_some()
                    {
                        guard.cancel_definite()?;
                    }
                    return Err(error);
                }
                guard.commit_output(HostEffectOutput {
                    nodes: Vec::new(),
                    events: events.clone(),
                    elicitation: candidate.elicitation().cloned(),
                    awaiting_input: candidate.awaiting_input(),
                    publication_drained: false,
                    plan_document: Some((
                        plan.id.clone(),
                        document.version,
                        crate::plan::digest(&serde_json::to_vec(&document)?),
                    )),
                    continuing: None,
                    plan_record: Some(plan.clone()),
                })?;
                *interaction = candidate;
                *current = plan;
                Ok(events)
            }
        }
    }

    /// Commit submitted plan state and retain its exact presentation in the receipt.
    pub(crate) fn commit_plan_submission(
        &mut self,
        interaction: &mut Exchange,
        current: &mut PlanRecord,
        permit: &crate::task::TaskPermit,
        entry_id: Option<&crate::task::TaskInputId>,
        result: PlanSubmissionResult,
    ) -> Result<Vec<SessionEvent>> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        match result {
            PlanSubmissionResult::Applied(output) => {
                let output = *output;
                interaction::reconcile_plan_effect(current, output.plan_record.clone())?;
                for node in output.nodes {
                    if !interaction
                        .node_list
                        .iter()
                        .any(|existing| existing.id() == node.id())
                    {
                        interaction.node_list.push(node);
                    }
                }
                if interaction.elicitation().is_none() && output.elicitation.is_some() {
                    interaction.set_elicitation(output.elicitation);
                    interaction.set_awaiting_input(output.awaiting_input);
                }
                Ok(if output.publication_drained {
                    Vec::new()
                } else {
                    output.events
                })
            }
            PlanSubmissionResult::Written(write) => {
                let PlanSubmissionWrite {
                    before,
                    plan,
                    document,
                    rendered,
                    digest,
                    diff_text,
                    declaration,
                    warnings,
                    guard,
                } = *write;
                let before = *before;
                let document = *document;
                let mut plan = *plan;
                anyhow::ensure!(
                    serde_json::to_vec(current)? == serde_json::to_vec(&before)?,
                    "plan metadata changed during submission"
                );
                let task = self
                    .task_controller
                    .current()?
                    .context("planning task disappeared")?;
                permit.validate(Some(&task))?;
                anyhow::ensure!(
                    task.entry.as_ref().map(|entry| &entry.id) == entry_id,
                    "planning task entry changed"
                );
                let lifecycle_kind = if before.model_revision == 0 {
                    PlanLifecycleKind::Created
                } else {
                    PlanLifecycleKind::RevisionCreated
                };
                plan.document_version = document.version;
                plan.submitted_version = Some(document.version);
                plan.review = None;
                plan.title = document.title.clone();
                plan.review_digest = Some(digest);
                plan.validation_warning = warnings;
                PlanStateMachine::apply(&mut plan, PlanEvent::PlanSubmitted, self.clock.now_ms())?;
                self.task_controller.apply_with_plan_document(
                    &mut self.store,
                    permit,
                    self.plan_file.guard_working_document(&document),
                    crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
                    Default::default(),
                    self.clock.now_ms(),
                )?;
                self.record_plan_trace("plan_submitted", plan_trace_fields(&plan));
                let node_start = interaction.node_list.len();
                if lifecycle_kind == PlanLifecycleKind::RevisionCreated {
                    let lifecycle = self
                        .store
                        .list_plan_lifecycle(&self.session.id)?
                        .into_iter()
                        .rev()
                        .find(|item| {
                            item.plan_id == plan.id
                                && item.kind == PlanLifecycleKind::ChangesRequested
                                && item.user_revision == plan.user_revision
                        });
                    if let Some(lifecycle) = lifecycle.filter(|item| !item.annotation.is_empty()) {
                        interaction
                            .node_list
                            .push(ExchangeNode::PlanCommentResolution {
                                resolution: crate::exchange::PlanCommentResolution {
                                    id: format!(
                                        "{}:resolved-comments:{}",
                                        interaction.id, plan.user_revision
                                    ),
                                    annotation: lifecycle.annotation,
                                    created_at_ms: self.clock.now_ms(),
                                },
                            });
                    }
                }
                interaction.node_list.push(ExchangeNode::ArtifactChange {
                    change: crate::exchange::ArtifactChange {
                        id: format!("{}:artifact:{}", interaction.id, plan.model_revision),
                        path: plan.working_path.clone(),
                        revision_request: false,
                        diff_text,
                        declaration,
                        created_at_ms: self.clock.now_ms(),
                    },
                });
                let lifecycle = PlanLifecycleRecord {
                    title: plan.title.clone(),
                    anchor: Some(crate::plan::ExchangeAnchor::capture(interaction)),
                    id: Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    plan_id: plan.id.clone(),
                    kind: lifecycle_kind,
                    model_revision: plan.model_revision,
                    user_revision: plan.user_revision,
                    overall_comment: None,
                    annotation: Vec::new(),
                    question: None,
                    answer: None,
                    created_at_ms: self.clock.now_ms(),
                };
                self.store.save_plan_lifecycle(&lifecycle)?;
                let event = self.event(if lifecycle_kind == PlanLifecycleKind::Created { "plan_created" } else { "plan_revision_created" },
                    json!({"plan":plan,"lifecycle":lifecycle,"document":document,"content":rendered.markdown,"navigation":rendered.navigation}))?;
                guard.commit_output(HostEffectOutput {
                    nodes: interaction.node_list[node_start..].to_vec(),
                    events: vec![event.clone()],
                    elicitation: interaction.elicitation().cloned(),
                    awaiting_input: interaction.awaiting_input(),
                    publication_drained: false,
                    plan_document: Some((
                        plan.id.clone(),
                        document.version,
                        crate::plan::digest(&serde_json::to_vec(&document)?),
                    )),
                    continuing: None,
                    plan_record: Some(plan.clone()),
                })?;
                *current = plan;
                Ok(vec![event])
            }
        }
    }

    /// Persist the authorized plan metadata before marking its document receipt applied.
    pub(crate) fn commit_plan_mutation(
        &mut self,
        current: &mut PlanRecord,
        permit: &crate::task::TaskPermit,
        entry_id: Option<&crate::task::TaskInputId>,
        result: PlanMutationResult,
    ) -> Result<()> {
        permit.validate(self.task_controller.current()?.as_ref())?;
        match result {
            PlanMutationResult::Applied(snapshot) => {
                let snapshot = *snapshot;
                interaction::reconcile_plan_effect(current, snapshot)
            }
            PlanMutationResult::Written {
                before,
                plan,
                document,
                guard,
            } => {
                let before = *before;
                let document = *document;
                let plan = *plan;
                anyhow::ensure!(
                    serde_json::to_vec(current)? == serde_json::to_vec(&before)?,
                    "plan metadata changed during the owned mutation"
                );
                let task = self
                    .task_controller
                    .current()?
                    .context("planning task disappeared")?;
                permit.validate(Some(&task))?;
                anyhow::ensure!(
                    task.entry.as_ref().map(|entry| &entry.id) == entry_id,
                    "planning task entry changed"
                );
                self.task_controller.apply_with_plan_document(
                    &mut self.store,
                    permit,
                    self.plan_file.guard_working_document(&document),
                    crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
                    Default::default(),
                    self.clock.now_ms(),
                )?;
                guard.commit(
                    plan.id.clone(),
                    document.version,
                    crate::plan::digest(&serde_json::to_vec(&document)?),
                    Some(plan.clone()),
                )?;
                *current = plan;
                Ok(())
            }
        }
    }
}
