//! Stage only the planning or exchange question explicitly captured by user input.

use super::foreground_dispatch::{ClarificationContext, StagedInteraction};
use super::*;
use crate::exchange::command::InputSubmissionTarget;

impl HarnessBroker {
    /// Revalidate selected question authority before preparing its clarification turn.
    pub(super) fn prepare_selected_clarification(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<Option<StagedInteraction>> {
        let crate::input::InputPurpose::Clarification { target } = &admission.purpose else {
            return Ok(None);
        };
        let target = target.clone();
        anyhow::ensure!(
            crate::exchange::input_target::is_current(
                &self.data_root,
                &self.session.id,
                &target,
                false
            )?,
            "selected clarification question changed before dispatch"
        );
        let discussion = self.prepare_question_discussion(admission)?;
        let text = admission.text.clone();
        let staged = match target {
            InputSubmissionTarget::Plan {
                id, question_id, ..
            } => {
                let mut plan = self
                    .store
                    .load_plan(id.as_str())?
                    .context("selected plan is missing")?;
                let questions = plan
                    .elicitation
                    .as_mut()
                    .context("selected plan questions are missing")?;
                questions.begin_clarification(question_id.as_str())?;
                let question = crate::exchange::QuestionInput {
                    set_id: questions.question_set.id.clone(),
                    question_id: Some(question_id.as_str().into()),
                    answer: Vec::new(),
                };
                let prompt =
                    PlanPrompt::clarification(questions, crate::provider::parse_user_input(&text));
                self.append_plan_feedback(
                    &plan.id,
                    text.clone(),
                    crate::exchange::InputIntent::Clarification,
                    Some(question),
                )?;
                plan.updated_at_ms = self.clock.now_ms();
                self.publish_plan(&plan)?;
                admission.clarification = Some(ClarificationContext::Plan { plan_id: plan.id });
                self.stage_foreground_interaction(
                    &text,
                    PromptMode::Chat,
                    None,
                    ProviderInput::new(prompt),
                )?
            }
            InputSubmissionTarget::Interaction {
                id, question_id, ..
            } => {
                let mut exchange = self
                    .store
                    .load_exchange(id.as_str())?
                    .context("selected question exchange is missing")?;
                let questions = exchange
                    .elicitation_mut()
                    .context("selected exchange questions are missing")?;
                questions.begin_clarification(question_id.as_str())?;
                let prompt = PlanPrompt::question_follow_up(
                    questions,
                    crate::provider::parse_user_input(&text),
                );
                let question = crate::exchange::QuestionInput {
                    set_id: questions.question_set.id.clone(),
                    question_id: Some(question_id.as_str().into()),
                    answer: Vec::new(),
                };
                exchange.append_question_input(
                    crate::exchange::InputIntent::Clarification,
                    text.clone(),
                    question,
                    self.clock.now_ms(),
                )?;
                self.store.save_exchange(&exchange)?;
                admission.clarification = Some(ClarificationContext::Interaction);
                self.stage_foreground_interaction(
                    &text,
                    self.task_prompt_mode()?,
                    None,
                    ProviderInput::new(prompt),
                )?
            }
            InputSubmissionTarget::PlanReview { id, .. } => {
                let plan = self
                    .store
                    .load_plan(id.as_str())?
                    .context("selected review plan is missing")?;
                let questions = plan
                    .review_questions()
                    .context("selected review questions are missing")?;
                let prompt = PlanPrompt::question_follow_up(
                    questions,
                    crate::provider::parse_user_input(&text),
                );
                // Approval evidence remains unchanged; only a later explicit command may execute the plan.
                self.stage_foreground_interaction(
                    &text,
                    PromptMode::Chat,
                    None,
                    ProviderInput::new(prompt),
                )?
            }
            InputSubmissionTarget::Conversation | InputSubmissionTarget::Question { .. } => {
                anyhow::bail!("clarification source is not supported")
            }
        };
        admission.question_discussion = discussion;
        Ok(Some(staged))
    }
}
