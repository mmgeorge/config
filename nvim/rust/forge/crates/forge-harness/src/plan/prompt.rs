use serde::{Deserialize, Serialize};

/// Builds the Harness-owned planning and revision contracts.
pub struct PlanPrompt;

const PLANNING_CONTRACT: &str = include_str!("prompts/planning.md");

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanExecutionPromptKind {
    Start,
    Continue,
    ResumeAfterInterruption,
}

impl PlanPrompt {
    /// Build a decision-complete planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in the Harness Plan task. The effective permission for this turn governs repository access. Planning may inspect source but must not modify project files.

{PLANNING_CONTRACT}

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan task. The Harness already recorded and consumed the user's answers below. Do not call harness_question_answer or harness_question_withdraw for them. Incorporate the answers through harness_design_apply_patch edits. Resolve any remaining material decision under the user's question preferences, then call harness_plan_submit with the exact canonical plan ID and version.

{PLANNING_CONTRACT}

Original request:
{request}

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
        document_json: &str,
        review_feedback: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        format!(
            r#"Revise the saved canonical plan in the Harness Plan task. Address every annotation and overall comment. Apply requested design changes with harness_design_apply_patch. Explain when a comment needs no edit or conflicts with another requirement. Do not invent an edit merely to close a comment. Then call harness_plan_submit with the exact resulting plan ID and version.

{PLANNING_CONTRACT}

Overall review comment:
{comment}

Current declaration design:
```json
{document_json}
```

Review comments:
{review_feedback}"#
        )
    }

    /// Prepend the complete canonical plan so every provider can discover and edit it.
    pub fn with_active_document(prompt: String, document_json: &str) -> String {
        format!("Active declaration design:\n```json\n{document_json}\n```\n\n{prompt}")
    }

    /// Restore execution identity and phase without replaying completed work after interruption.
    pub(crate) fn execution(
        execution: &super::execution::PlanExecutionRecord,
        kind: PlanExecutionPromptKind,
        document_json: &str,
    ) -> String {
        let continuation = match kind {
            PlanExecutionPromptKind::Start => "Start the Execute task for this accepted plan. Inspect the current workspace before implementation.",
            PlanExecutionPromptKind::Continue => "Continue the same Execute task at the persisted phase below. Reuse confirmed work and check whether existing evidence still applies.",
            PlanExecutionPromptKind::ResumeAfterInterruption => "Resume the same Execute task after interruption. Preserve its plan, accepted revision, and phase. Inspect current files, completed tool results, and any background commands before retrying unfinished work. Do not assume interruption rolled back changes or stopped a command.",
        };
        format!(
            "{continuation}\nExecution ID: {}\nPlan ID: {}\n{}\nUnresolved findings: {}\nAccepted semantic design:\n{}",
            execution.id, execution.plan_id, execution.instructions(),
            execution.findings.join("\n"), document_json
        )
    }

    /// Keep review discussion read-only until the user requests a canonical plan revision.
    pub fn discussion(prompt: &str) -> String {
        format!(
            "Continue discussing the active canonical plan in the Harness Plan task. \
Answer questions without editing or resubmitting the plan. When the user requests changes, \
revise the existing plan through harness_design_apply_patch, then submit the resulting plan ID and version \
through harness_plan_submit. A successful edit starts a revision of the same plan. \
Resolve material uncertainty under the user's question preferences. Resolve pending questions before editing. \
Do not implement the plan or modify project files. The authoring procedure below applies only when the user requests a revision. For discussion alone, answer without editing or submitting.\n\n{PLANNING_CONTRACT}\n\nUser request:\n{prompt}"
        )
    }
}

fn mutable_elicitation_prompt(
    planning_request: Option<&str>,
    elicitation_json: &str,
    question: &str,
) -> String {
    let workflow_boundary = if planning_request.is_some() {
        "Remain in the Harness Plan task. Do not continue or submit the plan during this turn."
    } else {
        "Do not continue the original request during this turn."
    };
    let planning_context = planning_request
        .map(|request| format!("\nOriginal planning request:\n{request}\n"))
        .unwrap_or_default();
    format!(
        r#"The user is responding while a Harness question set remains pending. Treat the pending elicitation as mutable decision state, not as a modal lock.

The effective permission for this turn governs repository access. Answer the user's follow-up directly, using repository evidence when relevant. {workflow_boundary}

After answering, choose exactly one outcome:

1. Preserve
   Make no control-tool call when the existing questions and options remain material and valid.

2. Answer
   Call harness_question_answer only when the user explicitly and unambiguously answers a pending question. Do not convert tentative language, discussion, or model preference into an answer.

3. Replace
   Call harness_question_ask with the complete revised question set when the user requests changes or when clarification changes which questions or options remain material. Preserve question IDs only when their meaning remains unchanged. End the turn after replacement.

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
    use super::{PLANNING_CONTRACT, PlanPrompt};

    #[test]
    fn planning_modes_include_the_same_complete_contract_and_current_context() {
        let request = "Preserve pending edits";
        let answer = "Planning feedback: retain drafts after a failed save";
        let document_json = r#"{"plan_id":"active-plan","version":7}"#;
        let annotation_json = r#"[{"json_path":"/stages/1","text":"Split independent consumers"}]"#;
        let draft = PlanPrompt::draft(request);
        let feedback = PlanPrompt::feedback(request, answer);
        let revision = PlanPrompt::revision(
            document_json,
            annotation_json,
            Some("Keep file ownership distinct"),
        );
        for prompt in [&draft, &feedback, &revision] {
            assert_eq!(prompt.matches(PLANNING_CONTRACT).count(), 1);
            assert!(!prompt.contains("harness_plan_create"));
            assert!(!prompt.contains("{PLANNING_CONTRACT}"));
        }
        assert!(draft.ends_with(request));
        assert!(feedback.contains(request));
        assert!(feedback.ends_with(answer));
        assert!(feedback.contains("Do not call harness_question_answer"));
        assert!(revision.contains(document_json));
        assert!(revision.contains(annotation_json));
        assert!(revision.contains("Keep file ownership distinct"));
    }

    #[test]
    fn planning_patch_example_applies_to_virtual_declarations() {
        let workspace = tempfile::tempdir().unwrap();
        let contract = PLANNING_CONTRACT.replace("\r\n", "\n");
        let examples = contract.split("```text\n").skip(1)
            .map(|part| part.split_once("\n```").unwrap().0)
            .filter(|example| example.starts_with("*** Begin Patch\n")).collect::<Vec<_>>();
        let example = examples.last().unwrap();
        let mut design = crate::plan::DeclarationDesign::default();
        design.proposed.insert("src/textures.rs".into(), "impl Texture {\n  pub fn request(texture: TextureId) -> TextureHandle;\n\n  pub fn status(request: RequestId) -> RequestStatus;\n}\n".into());
        let described = design.patch(workspace.path(), &Default::default(), examples[0]).unwrap();
        assert!(described.document.usage.as_deref().is_some_and(|usage| !usage.is_empty()));
        assert!(!described.document.objective.is_empty());
        let changed = described.patch(workspace.path(), &Default::default(), example).unwrap();
        assert!(changed.proposed["src/textures.rs"].contains("-> TextureRequest;"));
        assert!(changed.proposed["src/textures.rs"].contains("pub fn cancel(request: RequestId) -> bool;"));
        assert_eq!(design.baseline,changed.baseline);
    }

    #[test]
    fn active_artifact_context_exposes_the_canonical_document() {
        let prompt = PlanPrompt::with_active_document("Why?".into(), "{\"plan_id\":\"plan\"}");
        assert!(prompt.contains("\"plan_id\":\"plan\""));
        assert!(prompt.ends_with("Why?"));
    }

    #[test]
    fn revision_sends_canonical_json_and_contextual_review_comments() {
        let prompt = PlanPrompt::revision(
            r#"{"plan_id":"plan","entity_changes":[]}"#,
            "src/controls.rs\n```text\n8  -   direction: Vec2,\n8  +   velocity: Vec2,\n```\n\n8: Why replace direction with velocity?",
            Some("Tighten ownership"),
        );

        assert!(prompt.contains(r#""plan_id":"plan""#));
        assert!(prompt.contains("Review comments"));
        assert!(prompt.contains("8: Why replace direction with velocity?"));
        assert!(!prompt.contains("Semantic annotations"));
        assert!(prompt.contains("harness_design_apply_patch"));
        assert!(!prompt.contains("Current rendered plan"));
    }

    #[test]
    fn clarification_preserves_the_pending_decision_boundary() {
        let prompt = PlanPrompt::clarification("Refactor", "{\"question\":\"Migration?\"}", "Why?");
        assert!(prompt.contains("mutable decision state"));
        assert!(prompt.contains("harness_question_answer"));
        assert!(prompt.contains("harness_question_ask"));
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
        assert!(!prompt.contains("Harness Plan task"));
        assert!(prompt.contains("harness_question_answer"));
    }

}
