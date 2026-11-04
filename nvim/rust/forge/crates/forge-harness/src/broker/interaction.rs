use super::*;

/// Retain validated completion metadata and its exact outstanding host effects.
pub(crate) struct StagedHostCompletion {
    pub(crate) output: crate::provider::ProviderCompletion,
    pub(crate) mode: PromptMode,
    pub(crate) settlement: Option<Arc<crate::provider::session_execution::SettlementCell>>,
    pub(crate) batches: Option<Vec<crate::provider::HostEffectBatch>>,
    pub(crate) expected_effects: Vec<crate::provider::session_execution::HostEffectId>,
    pub(crate) refresh_elicitation: bool,
}

pub(crate) struct AppliedQuestionEffect {
    pub(crate) id: crate::provider::session_execution::HostEffectId,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) needs_publication: bool,
}
enum QuestionEffect {
    Ask(crate::plan::PlanQuestionSet),
    Answer(crate::plan::PlanQuestionAnswer),
    Withdraw(crate::plan::PlanQuestionWithdrawal),
}

/// Apply rejected callback dispositions even to native snapshots captured before rejection.
fn discard_rejected_host_effects(
    output: &mut crate::provider::ProviderCompletion,
    batches: &mut Vec<crate::provider::HostEffectBatch>,
) {
    if !batches
        .iter()
        .any(crate::provider::HostEffectBatch::is_rejected)
    {
        return;
    }
    let mut retained =
        crate::provider::HostEffectBatch::active_completion(batches).unwrap_or_default();
    retained.native = std::mem::take(&mut output.native);
    retained.dispatched_task_permit = output.dispatched_task_permit.take();
    retained.dispatched_execution_id = output.dispatched_execution_id.take();
    retained.dispatched_entry_id = output.dispatched_entry_id.take();
    *output = retained;
    batches.retain(|batch| !batch.is_rejected());
}

/// Restore an unapplied in-memory plan snapshot without overwriting newer durable progress.
pub(super) fn reconcile_plan_effect(
    current: &mut PlanRecord,
    recorded: Option<PlanRecord>,
) -> Result<()> {
    let recorded = recorded.context("applied plan effect has no retained plan state")?;
    anyhow::ensure!(
        current.id == recorded.id,
        "plan effect receipt belongs to another plan"
    );
    let older = current.model_revision < recorded.model_revision
        || current.user_revision < recorded.user_revision
        || current.document_version < recorded.document_version;
    let newer = current.model_revision > recorded.model_revision
        || current.user_revision > recorded.user_revision
        || current.document_version > recorded.document_version;
    anyhow::ensure!(
        !(older && newer),
        "plan effect receipt conflicts with durable plan progress"
    );
    if older {
        *current = recorded;
    } else if !newer {
        anyhow::ensure!(
            current.state == recorded.state,
            "plan effect receipt conflicts with durable plan state"
        );
    }
    Ok(())
}

/// Identify every finalization mutation in the cumulative host completion.
fn applicable_host_effects(
    output: &crate::provider::ProviderCompletion,
    batches: &[crate::provider::HostEffectBatch],
    mode: PromptMode,
    discarded_planning: bool,
) -> Result<Vec<crate::provider::session_execution::HostEffectId>> {
    use crate::provider::session_execution::{HostEffectId, HostEffectKind};
    let crate::provider::ProviderCompletion {
        native,
        plan_edit,
        design_patch,
        plan_submit,
        plan_read,
        plan_question,
        question_answer,
        question_withdrawal,
        control_error,
        goal_control,
        dispatched_task_permit,
        dispatched_execution_id,
        dispatched_entry_id,
        structured_plan,
    } = output;
    // Native metadata and authorization fields are consumed by the existing idempotent state path.
    let _ = (
        native,
        plan_read,
        control_error,
        dispatched_task_permit,
        dispatched_execution_id,
        dispatched_entry_id,
        structured_plan,
    );
    anyhow::ensure!(
        matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            || (plan_edit.is_empty() && design_patch.is_empty() && plan_submit.is_none()),
        "planning effects are not authorized for this interaction"
    );
    anyhow::ensure!(
        usize::from(plan_submit.is_some())
            + usize::from(plan_question.is_some())
            + usize::from(question_answer.is_some())
            + usize::from(question_withdrawal.is_some())
            <= 1,
        "multiple terminal host controls cannot settle together"
    );
    let mut expected = Vec::new();
    for batch in batches.iter().filter(|_| !discarded_planning) {
        expected.extend(
            (0..batch.completion.plan_edit.len()).map(|ordinal| HostEffectId {
                batch_revision: batch.revision,
                kind: HostEffectKind::PlanEdit,
                ordinal,
            }),
        );
        expected.extend(
            (0..batch.completion.design_patch.len()).map(|ordinal| HostEffectId {
                batch_revision: batch.revision,
                kind: HostEffectKind::DesignPatch,
                ordinal,
            }),
        );
    }
    anyhow::ensure!(
        expected
            .iter()
            .filter(|id| id.kind == HostEffectKind::PlanEdit)
            .count()
            == plan_edit.len()
            && expected
                .iter()
                .filter(|id| id.kind == HostEffectKind::DesignPatch)
                .count()
                == design_patch.len(),
        "host effect batches do not match the cumulative completion"
    );
    let terminal = if plan_submit.is_some() {
        Some(HostEffectKind::PlanSubmit)
    } else if plan_question.is_some() {
        Some(HostEffectKind::PlanQuestion)
    } else if question_answer.is_some() {
        Some(HostEffectKind::QuestionAnswer)
    } else if question_withdrawal.is_some() {
        Some(HostEffectKind::QuestionWithdrawal)
    } else {
        None
    };
    if let Some(kind) = terminal {
        let batch = batches
            .iter()
            .rev()
            .find(|batch| match kind {
                HostEffectKind::PlanSubmit => batch.completion.plan_submit.is_some(),
                HostEffectKind::PlanQuestion => batch.completion.plan_question.is_some(),
                HostEffectKind::QuestionAnswer => batch.completion.question_answer.is_some(),
                HostEffectKind::QuestionWithdrawal => {
                    batch.completion.question_withdrawal.is_some()
                }
                HostEffectKind::PlanEdit
                | HostEffectKind::DesignPatch
                | HostEffectKind::GoalEvidence => false,
            })
            .context("terminal host effect has no batch provenance")?;
        expected.push(HostEffectId {
            batch_revision: batch.revision,
            kind,
            ordinal: 0,
        });
    }
    if goal_control.is_some() {
        let batch = batches
            .iter()
            .rev()
            .find(|batch| batch.completion.goal_control.is_some())
            .context("goal control has no host effect batch")?;
        expected.push(HostEffectId {
            batch_revision: batch.revision,
            kind: HostEffectKind::GoalEvidence,
            ordinal: 0,
        });
    }
    Ok(expected)
}

/// Omit only terminal effects whose exact retained batch has already been published.
fn discard_published_terminal_effects(
    output: &mut crate::provider::ProviderCompletion,
    batches: &[crate::provider::HostEffectBatch],
    cell: &crate::provider::session_execution::SettlementCell,
) -> Result<()> {
    use crate::provider::session_execution::{HostEffectId, HostEffectKind};
    let published = |kind| -> Result<bool> {
        let batch = batches.iter().rev().find(|batch| match kind {
            HostEffectKind::PlanSubmit => batch.completion.plan_submit.is_some(),
            HostEffectKind::PlanQuestion => batch.completion.plan_question.is_some(),
            HostEffectKind::QuestionAnswer => batch.completion.question_answer.is_some(),
            HostEffectKind::QuestionWithdrawal => batch.completion.question_withdrawal.is_some(),
            HostEffectKind::PlanEdit
            | HostEffectKind::DesignPatch
            | HostEffectKind::GoalEvidence => false,
        });
        match batch {
            Some(batch) => Ok(cell.effect_published(HostEffectId {
                batch_revision: batch.revision,
                kind,
                ordinal: 0,
            })?),
            None => Ok(false),
        }
    };
    if published(HostEffectKind::PlanSubmit)? {
        output.plan_submit.take();
    }
    if published(HostEffectKind::PlanQuestion)? {
        output.plan_question.take();
    }
    if published(HostEffectKind::QuestionAnswer)? {
        output.question_answer.take();
    }
    if published(HostEffectKind::QuestionWithdrawal)? {
        output.question_withdrawal.take();
    }
    if let Some(batch) = batches
        .iter()
        .rev()
        .find(|batch| batch.completion.goal_control.is_some())
    {
        let id = HostEffectId {
            batch_revision: batch.revision,
            kind: HostEffectKind::GoalEvidence,
            ordinal: 0,
        };
        if cell.effect_published(id)? {
            output.goal_control.take();
        }
    }
    Ok(())
}

/// Identify the batch that owns explicit goal control, or the latest native evidence batch.
pub(super) fn goal_evidence_id(
    staged: &StagedHostCompletion,
) -> Result<crate::provider::session_execution::HostEffectId> {
    use crate::provider::session_execution::{HostEffectId, HostEffectKind};
    let batches = staged
        .batches
        .as_ref()
        .context("goal evidence has no batch provenance")?;
    let batch = if staged.output.goal_control.is_some() {
        batches
            .iter()
            .rev()
            .find(|batch| batch.completion.goal_control.is_some())
    } else {
        batches.last()
    }
    .context("goal evidence has no batch provenance")?;
    Ok(HostEffectId {
        batch_revision: batch.revision,
        kind: HostEffectKind::GoalEvidence,
        ordinal: 0,
    })
}

impl HarnessBroker {
    /// Apply a non-planning question control once and retain its exact presentation.
    pub(crate) fn apply_owned_question_effect(
        &mut self,
        interaction: &mut Exchange,
        staged: &mut StagedHostCompletion,
    ) -> Result<Option<AppliedQuestionEffect>> {
        use crate::provider::session_execution::{
            HostEffectId, HostEffectKind, HostEffectOutput, ResultEffectAdmission,
        };
        let effect = if let Some(question) = staged.output.plan_question.take() {
            Some(QuestionEffect::Ask(question.normalize()?))
        } else if let Some(answer) = staged.output.question_answer.take() {
            Some(QuestionEffect::Answer(answer))
        } else {
            staged
                .output
                .question_withdrawal
                .take()
                .map(QuestionEffect::Withdraw)
        };
        let Some(effect) = effect else {
            return Ok(None);
        };
        let kind = match &effect {
            QuestionEffect::Ask(_) => HostEffectKind::PlanQuestion,
            QuestionEffect::Answer(_) => HostEffectKind::QuestionAnswer,
            QuestionEffect::Withdraw(_) => HostEffectKind::QuestionWithdrawal,
        };
        let batch = staged
            .batches
            .as_ref()
            .and_then(|batches| {
                batches.iter().rev().find(|batch| match kind {
                    HostEffectKind::PlanQuestion => batch.completion.plan_question.is_some(),
                    HostEffectKind::QuestionAnswer => batch.completion.question_answer.is_some(),
                    HostEffectKind::QuestionWithdrawal => {
                        batch.completion.question_withdrawal.is_some()
                    }
                    HostEffectKind::PlanEdit
                    | HostEffectKind::DesignPatch
                    | HostEffectKind::PlanSubmit
                    | HostEffectKind::GoalEvidence => false,
                })
            })
            .context("question effect has no batch provenance")?;
        let id = HostEffectId {
            batch_revision: batch.revision,
            kind,
            ordinal: 0,
        };
        let cell = staged
            .settlement
            .as_ref()
            .context("question effect has no settlement owner")?;
        let events = match cell.begin_result_effect(id)? {
            ResultEffectAdmission::Applied(output) => {
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
                if output.publication_drained {
                    Vec::new()
                } else {
                    output.events
                }
            }
            ResultEffectAdmission::Apply(mut guard) => {
                let node_start = interaction.node_list.len();
                let mut events = Vec::new();
                match effect {
                    QuestionEffect::Ask(question) => {
                        let plan_waiting = self.active_plan_awaits_input()?;
                        guard.begin_mutation();
                        if !plan_waiting {
                            interaction.node_list.push(ExchangeNode::QuestionPresented {
                                id: format!("{}:question:{}", interaction.id, Uuid::new_v4()),
                                question: question.clone(),
                                answer: None,
                            });
                        }
                        if !self.replace_active_elicitation(question.clone(), &mut events)? {
                            interaction.set_awaiting_input(true);
                            interaction.set_elicitation(Some(PlanElicitation::new(question)));
                            self.store.save_exchange(interaction)?;
                            events.push(self.event("question", json!({"owner":"interaction","plan_id":self.current_plan_id()?,
                                "exchange_id":interaction.id,"elicitation":interaction.elicitation()}))?);
                        }
                    }
                    QuestionEffect::Answer(answer) => {
                        guard.begin_mutation();
                        self.answer_active_elicitation_from_model(answer, &mut events)?;
                    }
                    QuestionEffect::Withdraw(withdrawal) => {
                        guard.begin_mutation();
                        self.withdraw_active_elicitation(withdrawal, &mut events)?;
                    }
                }
                guard.commit_output(HostEffectOutput {
                    nodes: interaction.node_list[node_start..].to_vec(),
                    events: events.clone(),
                    elicitation: interaction.elicitation().cloned(),
                    awaiting_input: interaction.awaiting_input(),
                    publication_drained: false,
                    plan_document: None,
                    continuing: None,
                    plan_record: None,
                })?;
                events
            }
        };
        if staged.refresh_elicitation {
            let owner = self
                .store
                .load_exchange_metadata(&interaction.id)?
                .context("question control lost its owning exchange")?;
            interaction.set_elicitation(owner.elicitation().cloned());
            interaction.set_awaiting_input(owner.awaiting_input());
        }
        let needs_publication = cell.pending_effect_publications()?.contains(&id);
        Ok(Some(AppliedQuestionEffect {
            id,
            events,
            needs_publication,
        }))
    }

    /// Apply interaction admission and retain all presentation for ordered delivery.
    pub(super) fn stage_admit_interaction(
        &mut self,
        text: &str,
        mode: PromptMode,
        admission: Option<&ExchangeAdmission>,
        input_message_id: Option<String>,
        selected: Option<&crate::provider::ExchangeId>,
    ) -> Result<(Exchange, bool, Vec<SessionEvent>, Vec<AppliedProviderEvent>)> {
        let now_ms = self.clock.now_ms();
        let admitted_prompt = admission.map(|value| value.prompt.as_str()).unwrap_or(text);
        let automatic_repair = admission.is_some_and(|value| value.automatic_owner.is_some());
        if automatic_repair {
            let mut previous = self
                .store
                .latest_exchange(&self.session.id)?
                .context("automatic repair has no originating exchange")?;
            anyhow::ensure!(
                previous.check_owner.as_ref()
                    == admission.and_then(|value| value.check_owner.as_ref()),
                "automatic repair check ownership changed"
            );
            previous.repair_checks(now_ms)?;
            self.store.save_exchange(&previous)?;
        }
        let (mut interaction, new_interaction, events, mut effects) = self
            .stage_interaction_for_turn(
                admitted_prompt,
                admission.is_some() && !automatic_repair,
                now_ms,
                selected,
            )?;
        // Internal continuations have their own journal record but no new user input identity.
        // Keep the originating input link when they resume the same retained exchange.
        interaction.input_message_id = input_message_id.or(interaction.input_message_id);
        interaction.approval_mode = Some(self.session.approval_mode);
        if new_interaction && let Some(admission) = admission {
            interaction.origin = admission.origin.clone();
            if self.check_setup {
                interaction.check_setup = Some(crate::checks::CheckSetupState::Discovery);
            } else if mode == PromptMode::Chat {
                interaction.check_setup = self.continued_check_setup(admission)?;
            }
            interaction.check_owner.clone_from(&admission.check_owner);
            if let Some(run_id) = admission.agent_run_id.as_deref()
                && let Some(run) = self.agent_registry.get(run_id).cloned()
            {
                self.agent_registry.execution_mut(run_id).parent_exchange_id =
                    Some(interaction.id.clone());
                self.store.save_agent_run(&run)?;
                self.agent_registry.insert(run);
            }
            if interaction.checklist.is_none() && self.current_execution()?.is_some() {
                interaction.checklist = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .and_then(|previous| previous.checklist);
            }
        }
        if new_interaction {
            if let Some(event_id) = admission.and_then(|value| value.plan_event_id.as_deref()) {
                let mut lifecycle = self
                    .store
                    .list_plan_lifecycle(&self.session.id)?
                    .into_iter()
                    .find(|record| record.id == event_id)
                    .context("admitted planning event is missing")?;
                lifecycle.anchor = Some(crate::plan::ExchangeAnchor::capture(&interaction));
                self.store.save_plan_lifecycle(&lifecycle)?;
            }
            if let Some(task) = self.task_controller.current()?
                && matches!(&task.workflow, crate::task::WorkflowSnapshot::Execute { execution } if execution.record.baseline_checkpoint.is_none())
                && let Some(checkpoint) = interaction.checkpoint_before.clone()
            {
                self.task_controller.apply(
                    &mut self.store,
                    &task.permit(),
                    crate::task::TaskEvent::ExecutionBaselineCaptured(checkpoint),
                    now_ms,
                )?;
            }
        }
        interaction.resume(self.clock.now_ms())?;
        self.store.save_exchange(&interaction)?;
        effects.push(self.prepare_prompt_admission(true)?);
        if let Some(owner) = admission.and_then(|value| value.automatic_owner.as_deref()) {
            let mut automatic =
                crate::checks::AutomaticChecks::open(&self.check_runner.root, owner)?;
            automatic.repair_exchange = Some(interaction.id.clone());
            automatic.save()?;
        }
        effects.push(
            self.prepare_live_interaction(publication::LiveUpdate::StateChanged, &interaction)?,
        );
        self.trace.record(
            &self.session.id,
            "turn.state",
            json!({
                "exchange_id":interaction.id,"message_id":interaction.input_message_id,
                "from_state":"queued","to_state":"working",
            }),
        );
        if mode == PromptMode::Plan || admission.is_some_and(|value| value.plan_event_id.is_some())
        {
            effects.push(AppliedProviderEvent::Timeline(self.reconcile_timeline()?));
        }
        self.session.active_task = self.task_controller.current()?;
        effects.push(AppliedProviderEvent::Update(
            SessionUpdate::ExecutionState {
                session: Box::new(self.session_view()?),
            },
        ));
        Ok((interaction, new_interaction, events, effects))
    }

    pub(super) async fn run_interaction_input_inner(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        input: ProviderInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let (sink, updates) = publication::channel();
        let dispatch = self.begin_local_interaction_dispatch(text, mode, admission, input, sink)?;
        let result = self.drive_local_foreground(dispatch, updates).await?;
        let value = result.response.result().cloned().ok_or_else(|| {
            anyhow::anyhow!(
                "local foreground dispatch failed: {:?}",
                result.response.error()
            )
        })?;
        Ok((value, result.event))
    }

    /// Apply completion metadata and derive the exact host effect set under broker ownership.
    pub(crate) fn stage_host_completion(
        &mut self,
        interaction: &mut Exchange,
        mut output: crate::provider::ProviderCompletion,
        mut mode: PromptMode,
    ) -> Result<Box<StagedHostCompletion>> {
        let settlement = self.host_settlement();
        let mut batches = settlement.as_ref().map(|cell| cell.batches()).transpose()?;
        if let Some(batches) = &mut batches {
            discard_rejected_host_effects(&mut output, batches);
        }
        let current = self.task_controller.current()?;
        let mut discarded_planning = false;
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && output
                .dispatched_task_permit
                .as_ref()
                .is_some_and(|permit| permit.validate(current.as_ref()).is_err())
        {
            discarded_planning = true;
            output.plan_edit.clear();
            output.design_patch.clear();
            output.plan_read = None;
            output.plan_submit = None;
            output.plan_question = None;
            output.question_answer = None;
            output.question_withdrawal = None;
            output.control_error = None;
            mode = PromptMode::Chat;
        }
        self.trace.record(
            &self.session.id,
            "model.completed",
            serde_json::to_value(&output)?,
        );
        interaction.pause(self.clock.now_ms());
        if let Some(context_usage) = output.metrics.context_usage.clone() {
            self.session.context_usage = Some(context_usage);
            self.context_usage_fresh = true;
        }
        if let Some(identity) = &output.provider_session_id {
            self.retain_session_identity(
                &crate::provider::ExecutionOwner::root(&self.session.id),
                identity,
            )?;
        }
        self.session.provider_checkpoint_id = output.provider_checkpoint_id.clone();
        if !output.runtime.provider.is_empty() {
            self.session.provider_label = output.runtime.provider.clone();
        }
        if output.runtime.model.is_some() {
            self.session.resolved_model = output.runtime.model.clone();
        }

        if let Some(control_error) = output.control_error.take() {
            anyhow::bail!(control_error);
        }

        if let (Some(cell), Some(batches)) = (&settlement, &batches) {
            discard_published_terminal_effects(&mut output, batches, cell)?;
        }
        let elicitation_control_count = usize::from(output.plan_question.is_some())
            + usize::from(output.question_answer.is_some())
            + usize::from(output.question_withdrawal.is_some());
        let refresh_elicitation =
            interaction.elicitation().is_some() && elicitation_control_count > 0;
        anyhow::ensure!(
            elicitation_control_count <= 1,
            "provider turn emitted multiple Harness question control actions"
        );
        if mode == PromptMode::Plan {
            anyhow::ensure!(
                output.question_answer.is_none() && output.question_withdrawal.is_none(),
                "planning generation cannot answer or withdraw a pending question"
            );
        }

        let expected_effects = match &batches {
            Some(batches) => applicable_host_effects(&output, batches, mode, discarded_planning)?,
            None => Vec::new(),
        };
        Ok(Box::new(StagedHostCompletion {
            output,
            mode,
            settlement,
            batches,
            expected_effects,
            refresh_elicitation,
        }))
    }
}

#[cfg(test)]
mod receipt_tests {
    use super::*;

    /// Terminal snapshots are historical observations, not a second negotiated capability authority.
    #[tokio::test]
    async fn completion_preserves_negotiated_capability_authority() -> Result<()> {
        use forge_provider_api::capability::{CapabilityId, Support};
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let (mut exchange, _, _) = broker
            .test_stage_interaction_for_turn("Preserve negotiated support", true, 100)
            .await?;
        for (revision, supported) in [(1, true), (2, false)] {
            let mut manifest = forge_provider_api::CapabilityManifest::from_descriptor(
                broker.provider.descriptor(),
            );
            manifest.effective_revision = revision;
            for capability in &mut manifest.capability {
                if matches!(
                    capability.id,
                    CapabilityId::NativeFork
                        | CapabilityId::NativeCompact
                        | CapabilityId::NativeSteer
                ) {
                    capability.support = if supported {
                        Support::Supported
                    } else {
                        Support::Unsupported
                    };
                }
            }
            broker.apply_effective_capabilities(manifest.clone())?;
            let expected = serde_json::to_value(&broker.capability)?;
            let mut stale = broker.capability.clone();
            stale.native_fork = !supported;
            stale.native_compact = !supported;
            stale.native_steer = !supported;
            broker.session.provider_checkpoint_id = Some("previous-turn-checkpoint".into());
            let completion = ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    capability: stale,
                    ..Default::default()
                },
                ..Default::default()
            };
            broker.stage_host_completion(&mut exchange, completion, PromptMode::Chat)?;
            assert_eq!(serde_json::to_value(&broker.capability)?, expected);
            assert_eq!(
                serde_json::to_value(&broker.latest_manifest)?,
                serde_json::to_value(Some(&manifest))?
            );
            {
                let metadata = broker.observer.metadata.read().unwrap();
                assert_eq!(serde_json::to_value(&metadata.capability)?, expected);
                assert_eq!(
                    serde_json::to_value(&metadata.manifest)?,
                    serde_json::to_value(Some(&manifest))?
                );
            }
            assert_eq!(broker.session.native_fork, supported);
            assert_eq!(broker.session.native_compact, supported);
            let stored = broker.store.load_session(&broker.session.id)?.unwrap();
            assert_eq!(stored.native_fork, supported);
            assert_eq!(stored.native_compact, supported);
            assert!(
                broker.session.provider_checkpoint_id.is_none(),
                "absent per-turn retraction eligibility was preserved"
            );
        }
        Ok(())
    }

    /// A routing thread is not durable history, and absent terminal metadata cannot erase known history.
    #[tokio::test]
    async fn turn_routes_and_absent_completion_preserve_native_history() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let (mut exchange, _, _) = broker
            .test_stage_interaction_for_turn("Retain native history", true, 100)
            .await?;
        let address = crate::provider::TurnAddress {
            scope: crate::provider::InvocationScope::new(
                crate::provider::ExecutionOwner::root(&broker.session.id),
                exchange.id.clone(),
            ),
            thread_id: "arbitrary-routing-thread".into(),
            turn_id: "routing-turn".into(),
        };
        broker.apply_turn_observation(
            &exchange,
            &address,
            &crate::provider::TurnUpdate::Started { started_at_ms: 100 },
        )?;
        assert!(
            broker.session.provider_session_id.is_none(),
            "turn routing created durable native history"
        );
        broker.session.provider_session_id = Some("known-native-history".into());
        broker.save_session()?;
        broker.apply_turn_observation(
            &exchange,
            &address,
            &crate::provider::TurnUpdate::Started { started_at_ms: 101 },
        )?;
        assert_eq!(
            broker.session.provider_session_id.as_deref(),
            Some("known-native-history")
        );
        let completion = ProviderCompletion {
            native: forge_provider_api::completion::ProviderCompletion {
                capability: broker.capability.clone(),
                ..Default::default()
            },
            ..Default::default()
        };
        broker.stage_host_completion(&mut exchange, completion, PromptMode::Chat)?;
        assert_eq!(
            broker.session.provider_session_id.as_deref(),
            Some("known-native-history")
        );
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)?
                .unwrap()
                .provider_session_id
                .as_deref(),
            Some("known-native-history")
        );
        Ok(())
    }

    #[test]
    fn rejected_callback_cannot_reappear_in_stale_terminal_effects() -> Result<()> {
        let rejected = crate::provider::HostEffectBatch::new(
            1,
            ProviderCompletion {
                plan_edit: vec![serde_json::from_value(
                    json!({"plan_id":"plan", "expected_version":1,
                "plan":{"title":"must not apply"}}),
                )?],
                ..Default::default()
            },
        );
        let retained = crate::provider::HostEffectBatch::new(
            2,
            ProviderCompletion {
                plan_submit: Some(crate::provider::PlanSubmitRequest {
                    plan_id: "plan".into(),
                    expected_version: 1,
                }),
                ..Default::default()
            },
        );
        let mut snapshot = vec![rejected.clone(), retained];
        let mut output = crate::provider::HostEffectBatch::active_completion(&snapshot).unwrap();
        rejected.reject();
        discard_rejected_host_effects(&mut output, &mut snapshot);
        assert!(output.plan_edit.is_empty());
        assert!(output.plan_submit.is_some());
        let expected = applicable_host_effects(&output, &snapshot, PromptMode::Plan, false)?;
        assert_eq!(expected.len(), 1);
        assert_eq!(expected[0].batch_revision, 2);
        assert_eq!(
            expected[0].kind,
            crate::provider::session_execution::HostEffectKind::PlanSubmit
        );
        Ok(())
    }

    #[test]
    fn rejected_goal_callback_does_not_become_native_terminal_evidence() -> Result<()> {
        let (_root, _store, controller) = crate::task::SessionTaskController::test_goal();
        let permit = controller.current()?.unwrap().permit();
        let invocation = crate::tools::ControlToolInvocation {
            call_id: None,
            task_permit: Some(permit),
            name: "goal_complete".into(),
            arguments: json!({"summary":"completed host callback"}),
        };
        let mut host = ProviderCompletion::default();
        crate::tools::apply_invocation(&invocation, &mut host)?;
        let rejected = crate::provider::HostEffectBatch::new(1, host);
        let surviving = crate::provider::HostEffectBatch::new(2, ProviderCompletion::default());
        let mut batches = vec![rejected.clone(), surviving];
        let mut output = crate::provider::HostEffectBatch::active_completion(&batches).unwrap();
        rejected.reject();
        discard_rejected_host_effects(&mut output, &mut batches);
        assert!(output.goal_control.is_none());
        let (evidence, _) = output.goal_evidence();
        assert!(!evidence.structured_complete);
        assert!(!evidence.structured_blocked);
        assert!(
            applicable_host_effects(&output, &batches, PromptMode::GoalContinuation, false)?
                .is_empty()
        );
        Ok(())
    }

    #[test]
    fn plan_receipt_restores_missing_progress_without_overwriting_newer_progress() -> Result<()> {
        let root = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let store = crate::plan::PlanFileStore::new(root.path(), workspace.path());
        let original = store
            .prepare_entry("session", "objective".into(), None, 1)?
            .record;
        let mut recorded = original.clone();
        recorded.user_revision += 1;
        recorded.document_version += 1;
        let mut current = original;
        reconcile_plan_effect(&mut current, Some(recorded.clone()))?;
        assert_eq!(current.user_revision, recorded.user_revision);
        assert_eq!(current.document_version, recorded.document_version);
        current.document_version += 1;
        reconcile_plan_effect(&mut current, Some(recorded.clone()))?;
        assert_eq!(current.document_version, recorded.document_version + 1);
        current.user_revision -= 1;
        assert!(reconcile_plan_effect(&mut current, Some(recorded)).is_err());
        Ok(())
    }

    #[test]
    fn published_terminal_effect_is_filtered_without_dropping_a_newer_control() -> Result<()> {
        use crate::provider::session_execution::{
            HostEffectId, HostEffectKind, HostEffectOutput, ResultEffectAdmission, SettlementCell,
        };
        let cell = Arc::new(SettlementCell::default());
        let old_id = HostEffectId {
            batch_revision: 1,
            kind: HostEffectKind::PlanSubmit,
            ordinal: 0,
        };
        let ResultEffectAdmission::Apply(guard) = cell.begin_result_effect(old_id)? else {
            panic!("new effect must be reserved")
        };
        guard.commit_output(HostEffectOutput {
            nodes: Vec::new(),
            events: Vec::new(),
            elicitation: None,
            awaiting_input: false,
            publication_drained: false,
            plan_document: None,
            continuing: None,
            plan_record: None,
        })?;
        cell.acknowledge_effect_publication(old_id)?;
        let first = ProviderCompletion {
            plan_submit: Some(crate::provider::PlanSubmitRequest {
                plan_id: "plan".into(),
                expected_version: 1,
            }),
            ..Default::default()
        };
        let second = ProviderCompletion {
            plan_question: Some(PlanQuestionSet {
                id: "question".into(),
                questions: Vec::new(),
            }),
            ..Default::default()
        };
        let mut output = first.clone();
        output.merge_execution(second.clone());
        let batches = vec![
            crate::provider::HostEffectBatch::new(1, first),
            crate::provider::HostEffectBatch::new(2, second),
        ];
        discard_published_terminal_effects(&mut output, &batches, &cell)?;
        assert!(output.plan_submit.is_none());
        assert!(output.plan_question.is_some());
        Ok(())
    }
}
