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
    pub(crate) fn checks_setup(request: &str) -> String {
        format!(
            "The user asked for a plan, but the project's checks are missing or invalid. Configure them with the harness-checks skill before planning. Do not design or implement the requested feature yet, and use the original request only to identify the tooling it needs. Harness resumes the original planning request after the configuration is saved.\n\nOriginal planning request:\n{request}"
        )
    }

    /// Include persisted user edits when a provider accepts, revises, or resumes a reviewed plan.
    pub(crate) fn with_manual_changes(prompt: String, changes: &[String]) -> String {
        if changes.is_empty() {
            return prompt;
        }
        format!(
            "{prompt}\n\nManual plan edits made during review:\n{}\nThe current plan shows which of these edits remain after the review decision. List them when you acknowledge the decision.",
            changes
                .iter()
                .map(|change| format!("- {change}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }

    /// Build a decision-complete planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change as a Harness Plan task. The permission for this turn controls repository access. You may read source but must not change project files.

{PLANNING_CONTRACT}

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan task. Harness has already recorded the user's answers below. Do not call question_answer or question_withdraw for them. Apply the answers with design_patch edits, settle any remaining decision according to the user's question preferences, then call plan_submit with the exact plan ID and version.

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
            r#"Revise the saved plan in the Harness Plan task. Address every review comment and the overall comment, making the requested design changes with design_patch. When a comment needs no change or conflicts with another requirement, explain why instead of making an edit just to close it. Then call plan_submit with the resulting plan ID and version.

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
            PlanExecutionPromptKind::Start => {
                "Start the Execute task for this accepted plan. Check the current workspace before implementing."
            }
            PlanExecutionPromptKind::Continue => {
                "Continue the same Execute task at the saved phase below. Reuse confirmed work, and check whether existing evidence still applies."
            }
            PlanExecutionPromptKind::ResumeAfterInterruption => {
                "Resume the same Execute task after an interruption, keeping its plan, accepted revision, and phase. Check current files, completed tool results, and any background commands before retrying unfinished work. An interruption does not undo changes or stop a running command."
            }
        };
        format!(
            "{continuation}\nExecution ID: {}\nPlan ID: {}\n{}\nUnresolved findings: {}\nReviewer approval comments (context only, not plan edits):\n{}\nAccepted semantic design:\n{}",
            execution.id,
            execution.plan_id,
            execution.instructions(),
            execution.findings.join("\n"),
            execution.approval_comments.join("\n"),
            document_json
        )
    }

    /// Keep review discussion read-only until the user requests a canonical plan revision.
    pub fn discussion(prompt: &str) -> String {
        format!(
            "Continue discussing the active plan in the Harness Plan task. Answer questions without editing or resubmitting the plan. When the user asks for changes, revise the existing plan with design_patch and submit the resulting plan ID and version with plan_submit. A successful edit starts a new revision of the same plan. Settle pending questions before editing, according to the user's question preferences. Do not implement the plan or change project files. The planning contract below applies only when the user asks for a revision.\n\n{PLANNING_CONTRACT}\n\nUser request:\n{prompt}"
        )
    }
}

fn mutable_elicitation_prompt(
    planning_request: Option<&str>,
    elicitation_json: &str,
    question: &str,
) -> String {
    let workflow_boundary = if planning_request.is_some() {
        "Stay in the Harness Plan task, and do not continue or submit the plan during this turn."
    } else {
        "Do not continue the original request during this turn."
    };
    let planning_context = planning_request
        .map(|request| format!("\nOriginal planning request:\n{request}\n"))
        .unwrap_or_default();
    format!(
        r#"The user is replying while Harness questions are still pending. The pending questions are open decisions that can change, not a lock on the conversation.

The permission for this turn controls repository access. Answer the user's reply directly, using the repository when it helps. {workflow_boundary}

After answering, choose exactly one outcome:

1. Keep: make no tool call when the current questions and options still matter and are still valid.
2. Answer: call question_answer only when the user clearly answers a pending question. Discussion, tentative wording, or your own preference is not an answer.
3. Replace: call question_ask with the complete revised question set when the user asks for changes or the reply changes which questions or options matter. Keep a question's ID only when its meaning is unchanged. End the turn after replacing.
4. Withdraw: call question_withdraw when no decision remains for the user, with a short reason based on an explicit instruction, a delegated choice, repository evidence, or a settled requirement. End the turn after withdrawing.

Never pick an option because you recommend it, keep a question that no longer affects the implementation, or withdraw a question to avoid asking. "Choose for me" delegates the decision and can settle the question. Tentative language such as "I'm leaning toward" does not. When only part of the set changes, replace the whole set, keeping unchanged questions and their IDs, and include any new decision that has come up. After any tool call, let Harness present or resume the next step.
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
        assert!(feedback.contains("Do not call question_answer"));
        assert!(revision.contains(document_json));
        assert!(revision.contains(annotation_json));
        assert!(revision.contains("Keep file ownership distinct"));
    }

    #[test]
    fn planning_patch_example_applies_to_virtual_declarations() {
        let workspace = tempfile::tempdir().unwrap();
        let contract = PLANNING_CONTRACT.replace("\r\n", "\n");
        let examples = contract
            .split("```text\n")
            .skip(1)
            .map(|part| part.split_once("\n```").unwrap().0)
            .filter(|example| example.starts_with("*** Begin Patch\n"))
            .collect::<Vec<_>>();
        let example = examples.last().unwrap();
        let mut design = crate::plan::DeclarationDesign::default();
        design.proposed.insert("src/textures.rs".into(), "impl Texture {\n  pub fn request(texture: TextureId) -> TextureHandle;\n\n  pub fn status(request: RequestId) -> RequestStatus;\n}\n".into());
        let described = design
            .patch(workspace.path(), &Default::default(), examples[0])
            .unwrap();
        assert!(
            described
                .document
                .usage
                .as_deref()
                .is_some_and(|usage| !usage.is_empty())
        );
        assert!(!described.document.objective.is_empty());
        let changed = described
            .patch(workspace.path(), &Default::default(), example)
            .unwrap();
        assert!(changed.proposed["src/textures.rs"].contains("-> TextureRequest;"));
        assert!(
            changed.proposed["src/textures.rs"]
                .contains("pub fn cancel(request: RequestId) -> bool;")
        );
        assert_eq!(design.baseline, changed.baseline);
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
        assert!(prompt.contains("design_patch"));
        assert!(!prompt.contains("Current rendered plan"));
    }

    #[test]
    fn clarification_preserves_the_pending_decision_boundary() {
        let prompt = PlanPrompt::clarification("Refactor", "{\"question\":\"Migration?\"}", "Why?");
        assert!(prompt.contains("open decisions that can change"));
        assert!(prompt.contains("question_answer"));
        assert!(prompt.contains("question_ask"));
        assert!(prompt.contains("question_withdraw"));
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
        assert!(prompt.contains("question_answer"));
    }
}
