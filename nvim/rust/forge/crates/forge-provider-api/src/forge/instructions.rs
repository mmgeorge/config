//! Fixed `<instructions>` text for each turn kind and notice.
//!
//! Callers choose a [`TurnKind`]; this module alone decides what Harness tells the model to do
//! about it, so the instruction always matches the elements beside it.

use super::turn::{Notice, PendingScope, PlanFeedback, PlanReview, TaskStep, TurnKind};
use crate::input::QuestionReplyResponse;

const PLAN_DISCUSSION: &str = "Continue discussing the active plan in the Harness Plan task. Answer the user's message without editing or resubmitting the plan. When the user asks for changes, revise the existing planning directory with your file-editing tool, then call plan_submit with its plan ID and no expected version. Settle pending questions before editing, according to the user's question preferences. Do not implement the plan or change project files. The Harness task instructions apply only when the user asks for a revision.";

const CHECKS_SETUP: &str = "The user asked for a plan, but the project's checks are missing or invalid. Configure them with the harness-checks skill before planning. Do not design or implement the requested feature yet, and use the planning request in the user's message only to identify the tooling it needs. Harness resumes the planning request after the configuration is saved.";

const PENDING_QUESTIONS: &str = "The user is replying while Harness questions are still pending. The pending questions are open decisions that can change, not a lock on the conversation. The permission for this turn controls repository access. Answer the user's reply directly, using the repository when it helps.";

const PENDING_PLAN: &str =
    "Stay in the Harness Plan task, and do not continue or submit the plan during this turn.";

const PENDING_CONVERSATION: &str = "Do not continue the original request during this turn.";

const PENDING_ANSWERS: &str = "Only Harness records explicit user answers. Never choose an option on the user's behalf or treat a recommendation as consent. Tentative language such as \"I'm leaning toward\" is not an answer. This discussion does not resume work; Harness owns the continuation after required decisions resolve.";

const PLAN_REVIEW_QUESTION: &str = "Answer the user's plan review question in this forked session with a concise final response, which Harness saves on the original question. Do not edit, submit, implement, or ask follow-up questions.";

const QUESTIONS_RESOLVED: &str = "Continue the original request in the user's message using the recorded user answers. Do not repeat resolved questions.";

const MORE_REQUIRED: &str = "Other required questions remain unanswered. Record this {what} without resuming the original request, and wait for the remaining answers.";

const CLARIFICATION: &str = "This is a read-only question discussion. Explain the choices and give illustrative examples when asked, without selecting or recording an answer. Call question_reply with these exact target fields and your explanation as its text: {target}. Copy the complete opaque question_id, including its colon and index suffix, and leave the question unanswered. Honor any explicit constraint in the user's clarification. Do not implement or resume the original task; Harness owns its continuation.";

const PLAN_ANSWERS: &str = "Continue the existing Harness Plan task. Harness has already recorded the user's answers below; do not answer or withdraw those resolved questions. Apply the answers to the existing planning directory with your file-editing tool, settle remaining decisions according to the user's question preferences, then call plan_submit with the plan ID and no expected version.";

const ANSWERS_TIMED_OUT: &str = "The questions timed out, and the user may be unavailable. Harness kept the answers the user confirmed, selected defaults for the rest, and skipped questions without options. Automatic selections are not user answers. Continue cautiously with the available context and existing permissions.";

const PLAN_WITHDRAWN: &str = "Continue the existing Harness Plan task. The pending planning questions were withdrawn for the reason below. Continue the plan in the existing planning directory with your file-editing tool, then call plan_submit with the plan ID and no expected version.";

const WORKFLOW_ANSWERS: &str = "Harness resolved the pending questions and recorded their response sources. Continue the original request using these responses.";

const REVISION: &str = "Revise the saved plan in the Harness Plan task. Address every review comment and the overall comment by editing the existing planning directory with your file-editing tool. When a comment needs no change or conflicts with another requirement, explain why instead of making an edit just to close it. Then call plan_submit with the plan ID and no expected version.";

const ACCEPTED: &str = "The user accepted the plan. Start the Execute task for it, and check the current workspace before implementing.";

const PLAN_EDITS: &str = "The plan already includes the user's edits listed below. Keep them, and mention them when you acknowledge the decision.";

const CHECK_REPAIR: &str = "Automatic checks are blocking task completion. Fix the failures below and finish the repair turn, and Harness runs the checks again. You do not need to call check_run, though you may for earlier feedback. Do not run validation commands through terminal tools, and keep to the original task's scope. If the turn ends with the same failing inputs, automatic repair stops.";

const CHILD_RESULTS: &str = "Continue this same unfinished request using the child results below. Synthesize the results and complete the original request when its required work is settled.";

const PERMISSION_TIMEOUT: &str =
    "Approval requests timed out during the previous turn, and the user may be unavailable:";

const HISTORY_REPAIRED: &str = "Recovery notice: an interrupted native tool-call suffix was removed from provider history. Its actions may already have completed or partially changed the workspace. Inspect the workspace before repeating writes; do not replay the interrupted request automatically.";

const EXECUTE_CONTINUE: &str = "Continue the same Execute task at its saved phase. Reuse confirmed work, and check whether existing evidence still applies.";

const EXECUTE_RESUME: &str = "Resume the same Execute task after an interruption, keeping its plan, accepted revision, and phase. Check current files, completed tool results, and any background commands before retrying unfinished work. An interruption does not undo changes or stop a running command.";

const EXECUTE_START: &str = "Start the Execute task for the accepted plan. Check the current workspace before implementing.";

const GOAL_START: &str = "Start working toward the goal in the task section.";

const GOAL_CONTINUE: &str = "Continue working toward the goal in the task section.";

const GOAL_RESUME: &str = "Resume working toward the goal in the task section.";

const PLAN_RESUME: &str = "Resume the Harness Plan task for the request in the task section, keeping existing draft work in the planning directory.";

const PLAN_CONTINUE: &str = "Continue the active planning task in its existing directory. The previous turn ended without submitting the plan or asking a question. Use ordinary file tools for docs/ and files/, then call plan_submit with the plan ID and no expected version. Prose alone does not submit a plan. Use question_ask only for a new open decision, and only when the user's question preferences allow it. Do not repeat answered questions. If a tool keeps failing, report the failure instead of repeating the same rejected request.";

const CHECKS_SAVED: &str = "Project checks have been saved and validated at revision {revision}. Check setup is complete. Now begin the planning request in the task section.";

const ANSWERS_RESOLVED: &str = "Continue the current task at its preserved phase using the recorded user answers. Do not repeat resolved questions.";

const CONTINUE_REQUEST: &str = "Continue the previous request from its completed state.";

/// Get the instruction text a turn kind requires, if any.
pub(crate) fn for_kind(kind: &TurnKind) -> Option<String> {
    Some(match kind {
        TurnKind::UserMessage { .. } | TurnKind::Background { .. } => return None,
        TurnKind::PlanDiscussion { .. } => PLAN_DISCUSSION.into(),
        TurnKind::ChecksSetup { .. } => CHECKS_SETUP.into(),
        TurnKind::PendingQuestions { scope, .. } => format!(
            "{PENDING_QUESTIONS} {} {PENDING_ANSWERS}",
            match scope {
                PendingScope::Plan => PENDING_PLAN,
                PendingScope::Conversation => PENDING_CONVERSATION,
            }
        ),
        TurnKind::PlanReviewQuestion { .. } => PLAN_REVIEW_QUESTION.into(),
        TurnKind::QuestionsResolved { .. } => QUESTIONS_RESOLVED.into(),
        TurnKind::QuestionResponse { response } => match &response.response {
            QuestionReplyResponse::Answer {
                required_questions_remain: true,
                ..
            } => MORE_REQUIRED.replace("{what}", "answer"),
            QuestionReplyResponse::Correction {
                required_questions_remain: true,
                ..
            } => MORE_REQUIRED.replace("{what}", "correction"),
            QuestionReplyResponse::Answer { .. } | QuestionReplyResponse::Correction { .. } => {
                return None;
            }
            QuestionReplyResponse::Clarification { reply_to, .. } => {
                let target = serde_json::json!({
                    "question_id": response.question_id,
                    "revision": response.revision,
                    "message_id": response.message_id,
                    "reply_to": reply_to,
                });
                CLARIFICATION.replace("{target}", &target.to_string())
            }
        },
        TurnKind::PlanFeedback { feedback } => match feedback {
            PlanFeedback::Answers { answers } if answers.timed_out() => {
                format!("{PLAN_ANSWERS}\n\n{ANSWERS_TIMED_OUT}")
            }
            PlanFeedback::Answers { .. } => PLAN_ANSWERS.into(),
            PlanFeedback::Withdrawn { .. } => PLAN_WITHDRAWN.into(),
        },
        TurnKind::WorkflowAnswers { answers } if answers.timed_out() => {
            format!("{WORKFLOW_ANSWERS}\n\n{ANSWERS_TIMED_OUT}")
        }
        TurnKind::WorkflowAnswers { .. } => WORKFLOW_ANSWERS.into(),
        TurnKind::PlanReview { review } => {
            let decision = match review {
                PlanReview::Accepted { .. } => ACCEPTED,
                PlanReview::ChangesRequested { .. } => REVISION,
            };
            if review.has_edits() {
                format!("{decision}\n\n{PLAN_EDITS}")
            } else {
                decision.into()
            }
        }
        TurnKind::CheckRepair { .. } => CHECK_REPAIR.into(),
        TurnKind::ChildResults => CHILD_RESULTS.into(),
        TurnKind::PermissionTimeout { feedback } => format!("{PERMISSION_TIMEOUT}\n{feedback}"),
        TurnKind::TaskStep { step } => match step {
            TaskStep::ExecuteStart => EXECUTE_START.into(),
            TaskStep::ExecuteContinue => EXECUTE_CONTINUE.into(),
            TaskStep::ExecuteResume => EXECUTE_RESUME.into(),
            TaskStep::GoalStart => GOAL_START.into(),
            TaskStep::GoalContinue => GOAL_CONTINUE.into(),
            TaskStep::GoalResume => GOAL_RESUME.into(),
            TaskStep::PlanResume => PLAN_RESUME.into(),
            TaskStep::PlanContinue { turn, max_turns } => {
                format!("{PLAN_CONTINUE}\n\nPlanning continuation: turn {turn} of {max_turns}.")
            }
            TaskStep::ChecksSaved { revision } => CHECKS_SAVED.replace("{revision}", revision),
            TaskStep::AnswersResolved => ANSWERS_RESOLVED.into(),
            TaskStep::ContinueRequest => CONTINUE_REQUEST.into(),
        },
    })
}

/// Get the instruction text for one notice.
pub(crate) fn for_notice(notice: &Notice) -> String {
    match notice {
        Notice::TaskStarted { label, title } => format!("{label} started: {title}"),
        Notice::TaskResumed { step } => for_kind(&TurnKind::TaskStep { step: step.clone() })
            .expect("every task step has instructions"),
        Notice::PermissionTimeout { feedback } => format!("{PERMISSION_TIMEOUT}\n{feedback}"),
        Notice::HistoryRepaired => HISTORY_REPAIRED.into(),
    }
}
