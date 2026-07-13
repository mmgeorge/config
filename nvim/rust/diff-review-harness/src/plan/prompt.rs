/// Builds the Harness-owned planning and revision contracts.
pub struct PlanPrompt;

impl PlanPrompt {
    /// Build a decision-complete read-only planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in READ mode. Do not modify files.

Explore the repository before asking questions. Resolve discoverable facts from the code. Ask only when a product decision materially changes the implementation. Produce a decision-complete reviewer-readable plan with Overview, Usage, Code flow, Tasks, Modularity and validation, and Test plan sections. Keep the overview, code flow, and task tree aligned. Do not parse the plan into an execution checklist. Provider task updates are advisory progress only.

When the plan is complete, call harness_plan_submit with the entire Markdown artifact. Do not treat ordinary prose or a provider checklist as plan submission.

User request:
{request}"#
        )
    }

    /// Build a complete replacement request from reviewed plan state.
    pub fn revision(
        edited: &str,
        edit_diff: &str,
        annotation_json: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        format!(
            r#"Revise the saved implementation plan in READ mode. Do not modify repository files. Preserve intentional user edits, resolve every annotation and overall comment, then call harness_plan_submit with the complete replacement Markdown artifact.

Overall review comment:
{comment}

Current edited plan:
{edited}

User edit diff against the last model revision:
{edit_diff}

Anchored annotations:
{annotation_json}"#
        )
    }

    /// Append the saved active-plan location without copying editor state into chat.
    pub fn with_active_artifact(prompt: String, plan_id: &str, path: &str) -> String {
        format!(
            "Active plan artifact: id={plan_id}, path={path}. Read this saved file when the question depends on the plan. Unsaved editor changes are not part of the request.\n\n{prompt}"
        )
    }
}

#[cfg(test)]
mod test {
    use super::PlanPrompt;

    #[test]
    fn planning_contract_requires_structured_submission_without_native_mode() {
        let prompt = PlanPrompt::draft("Refactor the renderer");
        assert!(prompt.contains("READ mode"));
        assert!(prompt.contains("harness_plan_submit"));
        assert!(prompt.contains("Refactor the renderer"));
        assert!(!prompt.contains("collaborationMode"));
    }

    #[test]
    fn active_artifact_context_exposes_only_the_saved_path() {
        let prompt = PlanPrompt::with_active_artifact("Why?".into(), "plan", "D:/plan/working.md");
        assert!(prompt.contains("D:/plan/working.md"));
        assert!(prompt.ends_with("Why?"));
    }
}
