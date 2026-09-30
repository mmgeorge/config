use super::document::{PlanDocument, PlanSubtask, PlanTask};
use anyhow::Result;
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

/// Build one scheduler-aware accepted-plan execution prompt.
pub fn execution_prompt(
    kind: PlanExecutionPromptKind,
    execution_id: &str,
    active_task: Option<&PlanTask>,
    document: &PlanDocument,
) -> Result<String> {
    let boundary = match kind {
        PlanExecutionPromptKind::Start => "Begin accepted-plan execution.",
        PlanExecutionPromptKind::Continue => "Continue accepted-plan execution.",
        PlanExecutionPromptKind::ResumeAfterInterruption => {
            "Resume accepted-plan execution after an interruption. Preserve completed workspace changes and evidence. Do not repeat finished actions. Continue the same active task."
        }
    };
    let active_entity_name = active_task
        .into_iter()
        .flat_map(|task| &task.files)
        .flat_map(|file| &file.subtasks)
        .flat_map(PlanSubtask::owned_entities)
        .collect::<std::collections::HashSet<_>>();
    let active_entity_change = document
        .entity_changes
        .iter()
        .filter(|entity| active_entity_name.contains(&entity.name))
        .collect::<Vec<_>>();
    let active_work = serde_json::json!({
        "task_id": active_task.map(|task| &task.id),
        "plan_version": document.version,
        "label": active_task.and_then(|task| document.task_label(&task.id)),
        "task_path": active_task.and_then(|task| document.task_by_id(&task.id).map(|(path, _)| path)),
        "stage": active_task.and_then(|task| document.stages.iter().find(|stage| stage.tasks.iter().any(|child| child.id == task.id))).map(|stage| serde_json::json!({"id": stage.id, "title": stage.title})),
        "task": active_task,
        "entity_changes": active_entity_change,
    });
    let recovery = if active_task.is_some() {
        " The active task remains unfinished in Harness's persisted scheduler. If its workspace work and tests already finished, reuse that evidence and submit a fresh harness_plan_task_report for this task. A previous tool acknowledgment or final answer does not replace the persisted task state."
    } else {
        ""
    };
    Ok(format!(
        "{boundary} Execution ID: {execution_id}.{recovery} Complete the active whole task before calling harness_plan_task_report with the active task_id, current plan_version, and detailed subtask, entity, path, and test evidence. A stage completes only when all its lettered tasks have persisted completion. Execute only the selected task even when its siblings are independent. Address tasks, subtasks, entities, and tests by their JSON pointer paths in this exact plan version. Call harness_plan_deviation before departing from accepted intent. Call harness_goal_complete only after the scheduler has no incomplete tasks.\n\nActive task:\n```json\n{}\n```\n\nEffective canonical PlanDocument:\n```json\n{}\n```",
        serde_json::to_string_pretty(&active_work)?,
        serde_json::to_string_pretty(document)?,
    ))
}

impl PlanPrompt {
    /// Build a decision-complete planning request for one user objective.
    pub fn draft(request: &str) -> String {
        format!(
            r#"You are planning a software change in Harness Plan mode. The retained execution authorization governs every command and file operation. Planning does not imply read-only access.

{PLANNING_CONTRACT}

User request:
{request}"#
        )
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan mode conversation. The Harness already recorded and consumed the user's answers below. Do not call harness_question_answer or harness_question_withdraw for them. Incorporate the answers through semantic harness_plan_edit operations. If another material product decision remains, call harness_question_ask and end the turn. Otherwise call harness_plan_submit with the exact canonical plan ID and version.

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
        annotation_json: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        format!(
            r#"Revise the saved canonical plan in Harness Plan mode. Resolve every annotation and overall comment with semantic harness_plan_edit operations, then call harness_plan_submit with the exact resulting plan ID and version.

{PLANNING_CONTRACT}

Overall review comment:
{comment}

Current canonical PlanDocument:
```json
{document_json}
```

Semantic annotations:
{annotation_json}"#
        )
    }

    /// Prepend the complete canonical plan so every provider can discover and edit it.
    pub fn with_active_document(prompt: String, document_json: &str) -> String {
        format!("Active canonical PlanDocument:\n```json\n{document_json}\n```\n\n{prompt}")
    }

    /// Keep review discussion read-only until the user requests a canonical plan revision.
    pub fn discussion(prompt: &str) -> String {
        format!(
            "Continue discussing the active canonical plan in Harness Plan mode. \
Answer questions without editing or resubmitting the plan. When the user requests changes, \
revise the existing plan through harness_plan_edit, then submit the resulting plan ID and version \
through harness_plan_submit. A successful edit starts a revision of the same plan. \
Ask for clarification when the requested change is unclear. Resolve pending questions before editing. \
Do not implement the plan or modify project files.\n\n{prompt}"
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

Remain in Harness Plan mode. The retained execution authorization governs repository access. Answer the user's follow-up directly, using repository evidence when relevant. {workflow_boundary}

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
    use super::{PLANNING_CONTRACT, PlanExecutionPromptKind, PlanPrompt, execution_prompt};

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
    fn planning_example_applies_and_submits_with_independent_consumers() {
        use crate::plan::{PLAN_SCHEMA_VERSION, PlanDocument, PlanEditRequest, apply_plan_edit};
        let contract = PLANNING_CONTRACT.replace("\r\n", "\n");
        let example = contract
            .split_once("```json\n")
            .and_then(|(_, remainder)| remainder.split_once("\n```"))
            .map(|(example, _)| example)
            .expect("planning contract must contain a complete edit example");
        let request: PlanEditRequest = serde_json::from_str(example)
            .expect("example must match the advertised edit schema");
        let document = PlanDocument {
            schema_version: PLAN_SCHEMA_VERSION,
            version: request.expected_version,
            plan_id: request.plan_id.clone(),
            title: "Planning in progress".into(),
            prompt: "Preserve editor drafts".into(),
            overview: String::new(),
            usage: None,
            entity_changes: Vec::new(),
            dependencies: Vec::new(),
            flows: Vec::new(),
            stages: Vec::new(),
            assumptions: Vec::new(),
        };
        let edited = apply_plan_edit(&document, request).expect("example must apply atomically");
        let planned = edited.document;
        planned
            .validate_for_submission()
            .expect("example must pass submission validation");
        assert_eq!(planned.prompt, document.prompt);
        assert_eq!(planned.version, document.version + 1);
        assert_eq!(
            planned.stages.iter().map(|stage| stage.tasks.len()).collect::<Vec<_>>(),
            [1, 3, 1]
        );
        for task in &planned.stages[1].tasks {
            assert_eq!(task.requires, [planned.stages[0].tasks[0].id.clone()]);
        }
        assert_eq!(
            planned.stages[2].tasks[0].requires,
            planned.stages[1]
                .tasks
                .iter()
                .map(|task| task.id.clone())
                .collect::<Vec<_>>()
        );
        crate::plan::render_plan(&planned).expect("example must remain renderable");
    }

    #[test]
    fn active_artifact_context_exposes_the_canonical_document() {
        let prompt = PlanPrompt::with_active_document("Why?".into(), "{\"plan_id\":\"plan\"}");
        assert!(prompt.contains("\"plan_id\":\"plan\""));
        assert!(prompt.ends_with("Why?"));
    }

    #[test]
    fn revision_sends_canonical_json_and_semantic_annotations_without_markdown() {
        let prompt = PlanPrompt::revision(
            r#"{"plan_id":"plan","entity_changes":[]}"#,
            r#"[{"json_path":"/entity_changes/0","label":"PlanDocument","target":{"target_type":"entity","name":"PlanDocument"}}]"#,
            Some("Tighten ownership"),
        );

        assert!(prompt.contains(r#""plan_id":"plan""#));
        assert!(prompt.contains("Semantic annotations"));
        assert!(prompt.contains("PlanDocument"));
        assert!(prompt.contains("reviewer-readable implementation walkthrough"));
        assert!(prompt.contains("Validate the PlanDocument as one ownership model"));
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
        assert!(prompt.contains("harness_question_answer"));
    }

    #[test]
    fn execution_prompt_reanchors_start_and_resume_to_the_active_task() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let task = &document.stages[0].tasks[0];
        let start = execution_prompt(
            PlanExecutionPromptKind::Start,
            "execution",
            Some(task),
            &document,
        )
        .unwrap();
        assert!(start.contains("Begin accepted-plan execution"));
        assert!(start.contains(&task.title));

        let resumed = execution_prompt(
            PlanExecutionPromptKind::ResumeAfterInterruption,
            "execution",
            Some(task),
            &document,
        )
        .unwrap();
        assert!(resumed.contains("Preserve completed workspace changes"));
        assert!(resumed.contains("Do not repeat finished actions"));
        assert!(resumed.contains("Continue the same active task"));
        assert!(resumed.contains("unfinished in Harness's persisted scheduler"));
        assert!(resumed.contains("reuse that evidence and submit a fresh harness_plan_task_report"));
        let settled = execution_prompt(PlanExecutionPromptKind::Continue, "execution", None, &document).unwrap();
        assert!(!settled.contains("unfinished in Harness's persisted scheduler"));
    }
}
