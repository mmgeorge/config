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

    /// Get the planning workflow contract that Harness delivers as the tracked task section.
    ///
    /// Planning prompts refer to it instead of embedding it, so a conversation receives it once
    /// and again only after compaction or another history change.
    pub(crate) fn planning_contract() -> &'static str {
        PLANNING_CONTRACT.trim_end()
    }

    /// Build the first planning turn's input for one user objective.
    ///
    /// The input is the user's request unchanged: the tracked `task` section names the plan and
    /// says that each user message during the task is input to it.
    pub fn draft(request: &str) -> String {
        request.to_owned()
    }

    /// Continue one paused planning conversation with the user's selected feedback.
    pub fn feedback(request: &str, answer: &str) -> String {
        format!(
            r#"Continue the existing Harness Plan task. Harness has already recorded the user's answers below. Do not answer or withdraw those resolved questions. Apply the answers to the existing planning directory with your file-editing tool, settle remaining decisions according to the user's question preferences, then call plan_submit with the plan ID and no expected version. The Harness task instructions still apply.

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
        document: &super::PlanDocument,
        review_feedback: &str,
        overall_comment: Option<&str>,
    ) -> String {
        let comment = overall_comment
            .filter(|value| !value.trim().is_empty())
            .unwrap_or("None");
        let plan = active_plan_line(document);
        format!(
            r#"Revise the saved plan in the Harness Plan task. Address every review comment and the overall comment by editing the existing planning directory with your file-editing tool. When a comment needs no change or conflicts with another requirement, explain why instead of making an edit just to close it. Then call plan_submit with the plan ID and no expected version. The Harness task instructions still apply.

{plan}

Overall review comment:
{comment}

Review comments:
{review_feedback}"#
        )
    }

    /// Prepend the active identity without embedding a mutable draft or edit version.
    ///
    /// The interaction contract supplies current filesystem locations on every planning turn.
    pub fn with_active_plan(prompt: String, document: &super::PlanDocument) -> String {
        format!("{}\n\n{prompt}", active_plan_line(document))
    }

    /// Name the active plan in a note beside the user's text.
    pub(crate) fn active_plan(document: &super::PlanDocument) -> String {
        active_plan_line(document)
    }

    /// Report whether a prompt already carries the active plan's identity.
    pub(crate) fn has_active_plan(prompt: &str) -> bool {
        prompt.contains(ACTIVE_PLAN_PREFIX)
    }

    /// Start, continue, or resume an Execute task without repeating its accepted design.
    ///
    /// The phase instructions and design travel as the tracked `task` section
    /// ([`Self::execution_task`]), so this prompt names only the execution and its phase.
    pub(crate) fn execution(
        execution: &super::execution::PlanExecutionRecord,
        kind: PlanExecutionPromptKind,
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
            "{continuation}\nExecution ID: {}. Phase: {:?}. Accepted revision: {}. Follow the Harness task instructions; if they are missing, inspect the plan with plan_status and plan_read.",
            execution.id, execution.phase, execution.revision,
        )
    }

    /// Build an Execute task's phase instructions and accepted design for the `task` section.
    pub(crate) fn execution_task(
        execution: &super::execution::PlanExecutionRecord,
        document_json: &str,
    ) -> String {
        format!(
            "Execution ID: {}\nPlan ID: {}\n{}\nUnresolved findings: {}\nReviewer approval comments (context only, not plan edits):\n{}\nAccepted semantic design:\n{}",
            execution.id,
            execution.plan_id,
            execution.instructions(),
            execution.findings.join("\n"),
            execution.approval_comments.join("\n"),
            document_json
        )
    }

    /// Keep review discussion read-only until the user requests a canonical plan revision.
    ///
    /// Harness delivers it as the input's note, so the user's message follows it unchanged.
    pub fn discussion() -> &'static str {
        "Continue discussing the active plan in the Harness Plan task. Answer the user's message without editing or resubmitting the plan. When the user asks for changes, revise the existing planning directory with your file-editing tool, then call plan_submit with its plan ID and no expected version. Settle pending questions before editing, according to the user's question preferences. Do not implement the plan or change project files. The Harness task instructions apply only when the user asks for a revision."
    }
}

const ACTIVE_PLAN_PREFIX: &str = "Active plan `";

fn active_plan_line(document: &super::PlanDocument) -> String {
    format!(
        "{ACTIVE_PLAN_PREFIX}{}`. Edit the supplied planning directory with your file-editing tool; plan_status reports its location. Submit this plan ID with plan_submit without an expected version.",
        document.plan_id
    )
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

Only Harness records explicit user answers. Never choose an option on the user's behalf or treat a recommendation as consent. Tentative language such as "I'm leaning toward" is not an answer. This discussion does not resume work; Harness owns the continuation after required decisions resolve.
{planning_context}
Pending elicitation:
{elicitation_json}

User follow-up:
{question}"#
    )
}
