use super::*;

pub(crate) struct PreparedPlan {
    pub record: PlanRecord,
    pub instruction: String,
}

impl PlanFileStore {
    pub(crate) fn prepare_entry(
        &self,
        session_id: &str,
        request: String,
        document: Option<PlanDocument>,
        now_ms: i64,
    ) -> Result<PreparedPlan> {
        anyhow::ensure!(!request.trim().is_empty(), "task objective is required");
        let checks_ready = crate::checks::ConfigSnapshot::load(&self.workspace).is_ok();
        let seeded = document.is_some();
        let mut document = document.unwrap_or_else(|| PlanDocument {
            schema_version: PLAN_SCHEMA_VERSION,
            version: 1,
            plan_id: uuid::Uuid::new_v4().to_string(),
            title: PROVISIONAL_PLAN_TITLE.into(),
            prompt: request.clone(),
            overview: String::new(),
            design: None,
            usage: None,
            entity_changes: Vec::new(),
            dependencies: Vec::new(),
            flows: Vec::new(),
            stages: Vec::new(),
            assumptions: Vec::new(),
        });
        if document.design.is_none() {
            if checks_ready {
                document.design = Some(DeclarationDesign::open(&self.workspace)?);
            }
            document.title = request.lines().next().unwrap_or_default().into();
            document.overview = request.clone();
        }
        // Capture fresh evidence for an explicitly forked or replanned document.
        if seeded && let Some(design) = &mut document.design {
            let previous_source = design.source_fingerprint()?;
            let paths = design
                .baseline
                .keys()
                .chain(design.proposed_source.keys())
                .cloned()
                .collect::<std::collections::BTreeSet<_>>();
            let mut baseline = std::collections::BTreeMap::new();
            for path in paths {
                if let Some(source) = workspace_source(&self.workspace, &path)? {
                    let (file, _) = design.capture_baseline(&path, &source)?;
                    baseline.insert(path, file);
                }
            }
            design.baseline = baseline;
            design.baseline_calls.clear();
            design.moved.clear();
            design.cargo_lock.clear();
            design.check_configuration = None;
            design.check_preview.clear();
            design.check_preview_source.clear();
            design.authoring_fingerprint = None;
            if design.source_fingerprint()? != previous_source {
                design.parsed = None;
            }
        }
        let instruction = if checks_ready {
            PlanPrompt::with_active_plan(PlanPrompt::draft(&request), &document)
        } else {
            PlanPrompt::checks_setup(&request)
        };
        let working_path =
            self.create_working_document(session_id, &document.plan_id, &document)?;
        self.authoring_create(session_id, &document)?;
        let instruction = if checks_ready {
            format!(
                "Planning directory: {}\n\n{instruction}",
                self.authoring_directory(session_id, &document.plan_id)
                    .display()
            )
        } else {
            instruction
        };
        Ok(PreparedPlan {
            record: PlanRecord {
                id: document.plan_id,
                session_id: session_id.into(),
                request,
                title: document.title,
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
                review: None,
                question_ledger: Default::default(),
                generation: Default::default(),
                validation_warning: Vec::new(),
                created_at_ms: now_ms,
                updated_at_ms: now_ms,
            },
            instruction,
        })
    }
}
