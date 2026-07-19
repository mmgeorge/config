/// Builds the Harness-owned planning and revision contracts.
pub struct PlanPrompt;

impl PlanPrompt {
    /// Build a decision-complete planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in Harness Plan mode. The retained execution authorization still governs every command and file operation. Planning does not imply read-only access.

Explore the repository before asking questions. Resolve discoverable facts from the code. You may write supporting planning material when the retained authorization permits it. Ask only when a product decision materially changes the implementation. When feedback is required, call harness_plan_question with one to three concise questions, two or three mutually exclusive choices per structured question, and a recommended choice first. End that turn after requesting feedback.

Build the canonical PlanDocument with harness_plan_create and semantic harness_plan_edit operations. Keep its object definitions, independent flows, architectural tasks, file boundaries, subtasks, concrete code edits, tests, and assumptions aligned. Replace a definition's complete members or enum variants when those nested details change. Do not use provider task updates as the plan. When the document passes submission validation, call harness_plan_submit with the exact plan_id and expected_version. Ordinary prose and provider checklists do not submit a plan.

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan mode conversation. Incorporate the user's feedback through semantic harness_plan_edit operations. If another material product decision remains, call harness_plan_question and end the turn. Otherwise call harness_plan_submit with the exact canonical plan ID and version.

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

    /// Build a semantic revision request from reviewed plan state.
    pub fn revision(
        rendered: &str,
        annotation_json: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        format!(
            r#"Revise the saved canonical plan in Harness Plan mode. Resolve every annotation and overall comment with semantic harness_plan_edit operations, then call harness_plan_submit with the exact resulting plan ID and version.

Overall review comment:
{comment}

Current rendered plan:
{rendered}

Anchored annotations:
{annotation_json}"#
        )
    }

    /// Prepend the complete canonical plan so every provider can discover and edit it.
    pub fn with_active_document(prompt: String, document_json: &str) -> String {
        format!("Active canonical PlanDocument:\n```json\n{document_json}\n```\n\n{prompt}")
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

Remain in Harness Plan mode. The retained execution authorization governs repository access. Answer the user's follow-up directly, using repository evidence when relevant. {workflow_boundary}

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
        assert!(prompt.contains("Plan mode"));
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
    fn active_artifact_context_exposes_the_canonical_document() {
        let prompt = PlanPrompt::with_active_document("Why?".into(), "{\"plan_id\":\"plan\"}");
        assert!(prompt.contains("\"plan_id\":\"plan\""));
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
