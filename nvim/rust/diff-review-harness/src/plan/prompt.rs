/// Builds the Harness-owned planning and revision contracts.
pub struct PlanPrompt;

impl PlanPrompt {
    /// Build a decision-complete read-only planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in READ mode. Do not modify files.

Explore the repository before asking questions. Resolve discoverable facts from the code. Ask only when a product decision materially changes the implementation. When feedback is required, call harness_plan_question with one to three concise questions, two or three mutually exclusive choices per structured question, and a recommended choice first. End that turn after requesting feedback. Produce a decision-complete reviewer-readable plan with Overview, Usage, Code flow, Tasks, Modularity and validation, and Test plan sections. Keep the overview, code flow, and task tree aligned. Do not parse the plan into an execution checklist. Provider task updates are advisory progress only.

When the plan is complete, call harness_plan_submit with the entire Markdown artifact. Do not treat ordinary prose or a provider checklist as plan submission.

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing READ-mode planning conversation for the original request below. Incorporate the user's planning feedback. If another material product decision remains, call harness_plan_question and end the turn. Otherwise call harness_plan_submit with the complete Markdown plan.

Original request:
{request}

Planning feedback:
{answer}"#
        )
    }

    /// Build a follow-up turn that may preserve or explicitly mutate pending decisions.
    pub fn clarification(request: &str, elicitation_json: &str, question: &str) -> String {
        mutable_elicitation_prompt(Some(request), elicitation_json, question)
    }

    /// Build an ordinary follow-up turn against one pending Harness decision set.
    pub fn question_follow_up(elicitation_json: &str, question: &str) -> String {
        mutable_elicitation_prompt(None, elicitation_json, question)
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

fn mutable_elicitation_prompt(
    planning_request: Option<&str>,
    elicitation_json: &str,
    question: &str,
) -> String {
    let workflow_boundary = if planning_request.is_some() {
        "Do not continue or submit the plan during this turn."
    } else {
        "Do not continue the original request during this turn."
    };
    let planning_context = planning_request
        .map(|request| format!("\nOriginal planning request:\n{request}\n"))
        .unwrap_or_default();
    format!(
        r#"The user is responding while a Harness question set remains pending. Treat the pending elicitation as mutable decision state, not as a modal lock.

Stay in READ mode. Answer the user's follow-up directly, using repository evidence when relevant. {workflow_boundary}

After answering, choose exactly one outcome:

1. Preserve
   Make no control-tool call when the existing questions and options remain material and valid.

2. Answer
   Call harness_question_answer only when the user explicitly and unambiguously answers a pending question. Do not convert tentative language, discussion, or model preference into an answer.

3. Replace
   Call harness_plan_question with the complete revised question set when the user requests changes or when clarification changes which questions or options remain material. Preserve question IDs only when their meaning remains unchanged. End the turn after replacement.

4. Withdraw
   Call harness_question_withdraw when no material user decision remains. Provide a concise reason grounded in an explicit user instruction, delegated judgment, repository evidence, or a resolved requirement. End the turn after withdrawal.

Never select an option merely because you recommend it. Never retain a question that no longer affects the implementation. Never withdraw a question merely to avoid asking for input. "Choose for me" explicitly delegates the decision and may resolve the question. Tentative language such as "I'm leaning toward" does not resolve the question. If only part of the question set changes, replace the complete set while retaining unchanged questions and stable IDs. If a new material decision emerges, include it in the complete replacement set. After any control-tool call, let the Harness present or resume the resulting workflow.
{planning_context}
Pending elicitation:
{elicitation_json}

User follow-up:
{question}"#
    )
}

#[cfg(test)]
mod test {
    use super::PlanPrompt;

    #[test]
    fn planning_contract_requires_structured_submission_without_native_mode() {
        let prompt = PlanPrompt::draft("Refactor the renderer");
        assert!(prompt.contains("READ mode"));
        assert!(prompt.contains("harness_plan_submit"));
        assert!(prompt.contains("harness_plan_question"));
        assert!(prompt.contains("Refactor the renderer"));
        assert!(!prompt.contains("collaborationMode"));
    }

    #[test]
    fn feedback_continues_the_same_planning_contract() {
        let prompt = PlanPrompt::feedback("Refactor the renderer", "Use a staged migration");
        assert!(prompt.contains("Refactor the renderer"));
        assert!(prompt.contains("Use a staged migration"));
        assert!(prompt.contains("harness_plan_question"));
        assert!(prompt.contains("harness_plan_submit"));
    }

    #[test]
    fn active_artifact_context_exposes_only_the_saved_path() {
        let prompt = PlanPrompt::with_active_artifact("Why?".into(), "plan", "D:/plan/working.md");
        assert!(prompt.contains("D:/plan/working.md"));
        assert!(prompt.ends_with("Why?"));
    }

    #[test]
    fn clarification_preserves_the_pending_decision_boundary() {
        let prompt = PlanPrompt::clarification("Refactor", "{\"question\":\"Migration?\"}", "Why?");
        assert!(prompt.contains("mutable decision state"));
        assert!(prompt.contains("harness_question_answer"));
        assert!(prompt.contains("harness_plan_question"));
        assert!(prompt.contains("harness_question_withdraw"));
        assert!(prompt.contains("Tentative language"));
        assert!(prompt.contains("Why?"));
        assert!(prompt.contains("Migration?"));
    }

    #[test]
    fn ordinary_question_follow_up_omits_planning_language() {
        let prompt = PlanPrompt::question_follow_up("{\"question\":\"Format?\"}", "Use JSON");
        assert!(prompt.contains("Do not continue the original request"));
        assert!(!prompt.contains("Original planning request"));
        assert!(prompt.contains("harness_question_answer"));
    }
}
