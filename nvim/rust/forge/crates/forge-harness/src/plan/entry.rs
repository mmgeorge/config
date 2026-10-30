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
        let working_path = self.write_working_document(session_id, &document.plan_id, &document)?;
        let instruction = if checks_ready {
            PlanPrompt::with_active_document(PlanPrompt::draft(&request), &document.model_json()?)
        } else {
            PlanPrompt::checks_setup(&request)
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
                acceptance: None,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plan_entry_preserves_the_complete_objective() -> Result<()> {
        let root = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        crate::checks::configure_fixture(workspace.path());
        let store = PlanFileStore::new(root.path(), workspace.path());
        let request = format!(
            "{}\nKeep every requirement.",
            "Detailed objective ".repeat(256)
        );
        let prepared = store.prepare_entry("session", request.clone(), None, 1)?;
        assert_eq!(prepared.record.title, request.lines().next().unwrap());
        assert_eq!(prepared.record.request, request);
        let document = store.read_working_document("session", &prepared.record.id)?;
        assert_eq!(document.prompt, request);
        assert!(prepared.instruction.contains(&request));
        Ok(())
    }
}
