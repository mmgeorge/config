use super::*;

impl HarnessBroker {
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
        mut document: PlanDocument,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let mut leading_event = Vec::new();
        let checks_ready =
            crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace)).is_ok();
        if document.design.is_none() {
            if checks_ready {
                let workspace = PathBuf::from(&self.session.workspace);
                document.design = Some(
                    tokio::task::spawn_blocking(move || {
                        crate::plan::DeclarationDesign::open(&workspace)
                    })
                    .await??,
                );
            }
            document.title = request
                .lines()
                .next()
                .unwrap_or("Declaration design")
                .chars()
                .take(100)
                .collect();
            document.overview = request.clone();
        }
        self.pause_task(false).await?;
        if let Some(permission) = self.session.plan_permission {
            self.select_execution_mode(json!({"mode":permission}))?;
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
            state: if checks_ready {
                PlanState::Generating
            } else {
                PlanState::ConfiguringChecks
            },
            working_path: working_path.to_string_lossy().into_owned(),
            document_version: document.version,
            model_revision: 0,
            submitted_version: None,
            accepted_revision: None,
            user_revision: 0,
            manual_changes: Vec::new(),
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
        let mut task = crate::task::TaskRecord::new(
            self.session.id.clone(),
            plan.title.clone(),
            crate::task::TaskKind::Plan,
            self.session.execution_mode,
            now_ms,
        );
        task.plan_id = Some(plan.id.clone());
        if !checks_ready {
            task.phase = "checks_setup".into();
        }
        task.operation_id = self.active_operation_id.clone();
        task.begin(now_ms)?;
        self.session.current_task_id = Some(task.id.clone());
        self.store.select_task(
            &self.session,
            Some(&task),
            None,
            Some(&plan),
            &self.client_id,
        )?;
        if let Some(goal) = self.current_goal()?
            && goal.state == GoalState::Active
        {
            let (_, mut paused) = self.pause_goal().await?;
            leading_event.append(&mut paused);
        }
        let (result, mut event) = if !checks_ready {
            self.configure_plan_checks(prompt).await?
        } else {
            self.run_planning_interaction(
                PlanPrompt::with_active_document(
                    PlanPrompt::draft(&request),
                    &document.model_json()?,
                ),
                Some(ExchangeAdmission::plan(prompt, Some(plan_id), false)),
            )
            .await?
        };
        leading_event.append(&mut event);
        Ok((result, leading_event))
    }
}
