//! Build typed planning and execution inputs.
//!
//! Each function returns a [`TurnInput`] whose kind decides the `<forge>` elements and the fixed
//! instruction text; no planning prompt is assembled from strings.

use forge_provider_api::PromptInput;
use forge_provider_api::forge::{
    AnswerValue, PendingQuestion, PendingQuestions, PendingScope, PlanEdit, PlanFeedback,
    PlanReview, QuestionOption, RecordedAnswer, ResolvedQuestions, ReviewFeedback, TaskStep,
    TurnInput, TurnKind,
};
use serde::{Deserialize, Serialize};

/// Builds the Harness-owned planning and execution inputs.
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
    /// Ask for check configuration before planning the user's request.
    pub(crate) fn checks_setup(request: &str) -> TurnInput {
        TurnInput::new(TurnKind::ChecksSetup {
            input: PromptInput::Text(request.to_owned()),
        })
    }

    /// Get the planning workflow contract that Harness delivers in the plan task section.
    pub(crate) fn planning_contract() -> &'static str {
        PLANNING_CONTRACT.trim_end()
    }

    /// Build the first planning turn's input: the user's request unchanged.
    pub fn draft(request: &str) -> TurnInput {
        TurnInput::user(PromptInput::Text(request.to_owned()))
    }

    /// Continue planning after a turn that neither submitted nor asked a question.
    pub(crate) fn continuation(turn: u32, max_turns: u32) -> TurnInput {
        step(TaskStep::PlanContinue { turn, max_turns })
    }

    /// Begin the planning request after check setup finished at `revision`.
    pub(crate) fn checks_saved(revision: &str) -> TurnInput {
        step(TaskStep::ChecksSaved {
            revision: revision.to_owned(),
        })
    }

    /// Resume planning with recorded answers.
    pub(crate) fn feedback(answers: ResolvedQuestions) -> TurnInput {
        TurnInput::new(TurnKind::PlanFeedback {
            feedback: PlanFeedback::Answers { answers },
        })
    }

    /// Resume planning after the model withdrew its questions.
    pub(crate) fn withdrawn(reason: &str) -> TurnInput {
        TurnInput::new(TurnKind::PlanFeedback {
            feedback: PlanFeedback::Withdrawn {
                reason: reason.to_owned(),
            },
        })
    }

    /// Answer the user's chat message while a plan's questions are still open.
    pub(crate) fn clarification(
        elicitation: &super::PlanElicitation,
        input: PromptInput,
    ) -> TurnInput {
        pending(elicitation, input, PendingScope::Plan)
    }

    /// Answer the user's chat message while a request's questions are still open.
    pub(crate) fn question_follow_up(
        elicitation: &super::PlanElicitation,
        input: PromptInput,
    ) -> TurnInput {
        pending(elicitation, input, PendingScope::Conversation)
    }

    /// Revise the saved plan for the user's review comments.
    pub(crate) fn revision(
        review_feedback: String,
        overall_comment: Option<&str>,
        edits: Vec<PlanEdit>,
    ) -> TurnInput {
        let feedback = ReviewFeedback::new(overall_comment.map(str::to_owned), review_feedback)
            .or_else(|| ReviewFeedback::new(None, "No review comments were attached.".into()))
            .expect("fallback review feedback is not empty");
        TurnInput::new(TurnKind::PlanReview {
            review: PlanReview::ChangesRequested { feedback, edits },
        })
    }

    /// Start execution after the user accepted the plan.
    pub(crate) fn accepted(edits: Vec<PlanEdit>) -> TurnInput {
        TurnInput::new(TurnKind::PlanReview {
            review: PlanReview::Accepted { edits },
        })
    }

    /// Start, continue, or resume an Execute task. The `<task>` section carries its design.
    pub(crate) fn execution(kind: PlanExecutionPromptKind) -> TurnInput {
        step(match kind {
            PlanExecutionPromptKind::Start => TaskStep::ExecuteStart,
            PlanExecutionPromptKind::Continue => TaskStep::ExecuteContinue,
            PlanExecutionPromptKind::ResumeAfterInterruption => TaskStep::ExecuteResume,
        })
    }
}

/// Build one Harness-started task step.
pub(crate) fn step(step: TaskStep) -> TurnInput {
    TurnInput::new(TurnKind::TaskStep { step })
}

/// Build a pending-question discussion from the open elicitation.
fn pending(
    elicitation: &super::PlanElicitation,
    input: PromptInput,
    scope: PendingScope,
) -> TurnInput {
    TurnInput::new(TurnKind::PendingQuestions {
        input,
        questions: pending_questions(elicitation),
        scope,
    })
}

/// Describe each question of an elicitation with any answer recorded so far.
fn pending_questions(elicitation: &super::PlanElicitation) -> PendingQuestions {
    PendingQuestions {
        questions: elicitation
            .question_set
            .questions
            .iter()
            .map(|question| PendingQuestion {
                id: question.id.clone(),
                header: question.header.clone(),
                question: question.question.clone(),
                options: question
                    .options
                    .iter()
                    .map(|option| QuestionOption {
                        label: option.label.clone(),
                        description: option.description.clone(),
                        recommended: option.recommended,
                    })
                    .collect(),
                allow_freeform: question.allow_freeform,
                answer: elicitation
                    .answer
                    .iter()
                    .find(|answer| answer.question_id == question.id)
                    .map(|answer| response_text(&answer.response)),
            })
            .collect(),
    }
}

/// Convert an elicitation's recorded answers, or `None` when it has no questions.
pub(crate) fn resolved_questions(
    elicitation: &super::PlanElicitation,
) -> Option<ResolvedQuestions> {
    let answers = elicitation
        .question_set
        .questions
        .iter()
        .map(|question| {
            let answer = elicitation
                .answer
                .iter()
                .find(|answer| answer.question_id == question.id);
            RecordedAnswer {
                question: question.header.clone(),
                answer: match answer.map(|answer| &answer.response) {
                    Some(super::PlanQuestionResponse::Selected { option, feedback }) => {
                        AnswerValue::Selected {
                            option: option.clone(),
                            feedback: feedback.clone(),
                        }
                    }
                    Some(super::PlanQuestionResponse::Other { text }) => {
                        AnswerValue::Other { text: text.clone() }
                    }
                    Some(super::PlanQuestionResponse::Skipped) | None => AnswerValue::Skipped,
                },
                automatic: answer.is_some_and(|answer| {
                    answer.source == crate::session::ResolutionSource::Timeout
                }),
            }
        })
        .collect();
    ResolvedQuestions::new(
        answers,
        elicitation.resolution_source == crate::session::ResolutionSource::Timeout,
    )
}

/// Describe a recorded response in one line.
fn response_text(response: &super::PlanQuestionResponse) -> String {
    match response {
        super::PlanQuestionResponse::Selected { option, feedback } => feedback
            .as_ref()
            .filter(|feedback| !feedback.trim().is_empty())
            .map(|feedback| format!("{option} — {feedback}"))
            .unwrap_or_else(|| option.clone()),
        super::PlanQuestionResponse::Other { text } => text.clone(),
        super::PlanQuestionResponse::Skipped => "[skipped]".into(),
    }
}

/// Record removed planned tests as review edits.
pub(crate) fn removed_test_edits(tests: &[super::DesignTestSelection]) -> Vec<PlanEdit> {
    tests
        .iter()
        .map(|test| PlanEdit::RemoveTest {
            file: test.file.clone(),
            name: test.name.clone(),
        })
        .collect()
}

/// Record enabled and disabled plan checks as review edits.
pub(crate) fn check_edits(enabled: &std::collections::BTreeMap<String, bool>) -> Vec<PlanEdit> {
    enabled
        .iter()
        .map(|(id, enabled)| PlanEdit::SetCheck {
            id: id.clone(),
            enabled: *enabled,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep the user's planning request as the input itself.
    #[test]
    fn draft_is_the_users_request() {
        let input = PlanPrompt::draft("Add retries.");
        assert_eq!(
            input.user_input(),
            Some(&PromptInput::Text("Add retries.".into()))
        );
        assert_eq!(
            forge_provider_api::forge::input_block(&input),
            "",
            "a draft carries no Harness instructions"
        );
    }

    /// Report review edits only on the review decision and never on continuations.
    #[test]
    fn review_decisions_carry_edits_and_steps_do_not() {
        let edits = vec![PlanEdit::Rename {
            from: "A".into(),
            to: "B".into(),
        }];
        let accepted = forge_provider_api::forge::input_block(&PlanPrompt::accepted(edits));
        assert!(accepted.contains("<rename from=\"A\" to=\"B\"/>"));
        let resumed = forge_provider_api::forge::input_block(&PlanPrompt::execution(
            PlanExecutionPromptKind::ResumeAfterInterruption,
        ));
        assert!(resumed.contains("Resume the same Execute task"));
        assert!(!resumed.contains("plan_edits"));
    }

    /// Keep user text only in inputs the user typed; every Harness-started input carries none.
    #[test]
    fn harness_started_inputs_carry_no_user_text() {
        let answers = ResolvedQuestions::new(
            vec![RecordedAnswer {
                question: "Backoff".into(),
                answer: AnswerValue::Other {
                    text: "Exponential".into(),
                },
                automatic: false,
            }],
            false,
        )
        .unwrap();
        for input in [
            PlanPrompt::continuation(2, 5),
            PlanPrompt::checks_saved("9c1e"),
            PlanPrompt::feedback(answers),
            PlanPrompt::withdrawn("no material decision remains"),
            PlanPrompt::revision("comment".into(), None, Vec::new()),
            PlanPrompt::accepted(Vec::new()),
            PlanPrompt::execution(PlanExecutionPromptKind::Start),
            PlanPrompt::execution(PlanExecutionPromptKind::Continue),
            step(TaskStep::GoalStart),
        ] {
            assert_eq!(input.user_input(), None, "{input:?}");
            assert!(
                forge_provider_api::forge::input_block(&input).contains("<instructions>"),
                "{input:?}"
            );
        }
        let checks = PlanPrompt::checks_setup("Add retries.");
        assert_eq!(
            checks.user_input(),
            Some(&PromptInput::Text("Add retries.".into())),
            "check setup keeps the user's planning request as their own words"
        );
    }
}
