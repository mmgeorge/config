use super::*;

#[derive(Serialize)]
struct PlanChoice {
    id: String,
    created_at_ms: i64,
    title: String,
    session_name: String,
    revision_count: u32,
    implemented: bool,
    state: PlanState,
}

impl HarnessBroker {
    /// List submitted plans in the current workspace with durable completion evidence.
    pub(super) fn list_replanning_choices(&self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut choices = Vec::new();
        for session in self.store.list_session(Some(&self.session.workspace))? {
            let execution = self.store.list_plan_execution(&session.id)?;
            for plan in self.store.list_plan(&session.id)? {
                if plan.model_revision == 0 {
                    continue;
                }
                choices.push(PlanChoice {
                    implemented: execution.iter().any(|record| {
                        record.plan_id == plan.id && record.state == PlanExecutionState::Complete
                    }),
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

    /// Seed a new planning exchange from an immutable revision in the same workspace.
    pub(super) async fn replan(&mut self, selection: &str) -> Result<(Value, Vec<SessionEvent>)> {
        let fields = selection.split_whitespace().collect::<Vec<_>>();
        anyhow::ensure!(fields.len() == 2, "usage: /replan <plan-id> <revision>");
        let revision: u32 = fields[1].parse().context("invalid plan revision")?;
        let source = self
            .store
            .load_plan(fields[0])?
            .context("source plan not found")?;
        let owner = self
            .store
            .load_session(&source.session_id)?
            .context("source session not found")?;
        anyhow::ensure!(
            owner.workspace == self.session.workspace,
            "source plan belongs to another workspace"
        );
        anyhow::ensure!(
            revision > 0 && revision <= source.model_revision,
            "source revision is unavailable"
        );
        let mut document = self
            .plan_file
            .capture_revision_source(&source.session_id, &source.id, revision)?
            .document;
        let request = format!(
            "Replan {} from revision {revision}. Reassess this plan against the current workspace, preserve useful decisions, and ask about necessary changes. Original request: {}",
            source.title, source.request
        );
        document.plan_id = Uuid::new_v4().to_string();
        document.version = 1;
        document.prompt = request.clone();
        self.start_plan(
            request,
            format!("/replan {} · revision {revision}", source.title),
            document,
        )
        .await
    }

    /// Admit a fresh plan owner and pause prior work before invoking the planning provider.
    pub(super) async fn start_plan(
        &mut self,
        request: String,
        prompt: String,
        document: PlanDocument,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut leading_event = Vec::new();
        if let Some(active) = self
            .session
            .active_plan_id
            .as_deref()
            .map(|id| self.store.load_plan(id))
            .transpose()?
            .flatten()
            && matches!(
                active.state,
                PlanState::Generating
                    | PlanState::Revising
                    | PlanState::AwaitingInput
                    | PlanState::AwaitingReview
                    | PlanState::Failed
            )
        {
            let (_, mut cancelled) = self.cancel_plan()?;
            leading_event.append(&mut cancelled);
        }
        let now_ms = self.clock.now_ms();
        let plan_id = document.plan_id.clone();
        let working_path =
            self.plan_file
                .write_working_document(&self.session.id, &plan_id, &document)?;
        let plan = PlanRecord {
            id: plan_id.clone(),
            session_id: self.session.id.clone(),
            request: request.clone(),
            title: document.title.clone(),
            state: PlanState::Generating,
            working_path: working_path.to_string_lossy().into_owned(),
            document_version: document.version,
            model_revision: 0,
            submitted_version: None,
            accepted_revision: None,
            user_revision: 0,
            review_digest: None,
            accepted_digest: None,
            elicitation: None,
            acceptance: None,
            question_ledger: Default::default(),
            generation: Default::default(),
            validation_warning: Vec::new(),
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        };
        self.store.save_plan(&plan)?;
        self.session.mode = HarnessMode::Plan;
        self.session.active_plan_id = Some(plan_id.clone());
        self.save_session()?;
        if let Some(goal) = self.current_goal()?
            && goal.state == GoalState::Active
        {
            let (_, mut paused) = self.pause_goal().await?;
            leading_event.append(&mut paused);
        }
        let (result, mut event) = self
            .run_planning_interaction(
                PlanPrompt::with_active_document(
                    PlanPrompt::draft(&request),
                    &document.model_json()?,
                ),
                Some(ExchangeAdmission::plan(prompt, Some(plan_id), false)),
            )
            .await?;
        leading_event.append(&mut event);
        Ok((result, leading_event))
    }
}
