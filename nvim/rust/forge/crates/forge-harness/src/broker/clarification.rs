//! Stage only the planning or exchange question explicitly captured by user input.

use super::foreground_dispatch::{ClarificationContext, StagedInteraction};
use super::*;
use crate::exchange::command::InputSubmissionTarget;

impl HarnessBroker {
    /// Retire the questions of a stopped exchange and say in the transcript which ones a task lost.
    ///
    /// A question's waiting tool cannot return an answer once its turn has stopped, by an
    /// interrupt or a host stop: the provider keeps no pending tool call across either. The task
    /// pauses, so the row names the question and how to continue, instead of leaving a paused plan
    /// with no question to answer and no word of what became of it.
    pub(crate) fn retire_stopped_questions(&mut self, exchange_id: Option<&str>) -> Result<()> {
        let lost = self.questions.cancel_chat(exchange_id)?;
        self.report_lost_questions(&lost)
    }

    /// Retire the questions of the running or latest exchange without reporting them yet.
    ///
    /// A task command revokes delivery before it collects the turn, but whether its task
    /// survives is known only once the command commits; the caller passes the returned
    /// questions to [`Self::report_lost_questions`] then.
    pub(crate) fn revoke_question_delivery(
        &mut self,
    ) -> Result<Vec<crate::question::QuestionRecord>> {
        let active = match self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?
        {
            Some(active) => Some(active),
            None => self.store.latest_exchange(&self.session.id)?,
        };
        self.questions
            .cancel_chat(active.as_ref().map(|exchange| exchange.id.as_str()))
    }

    /// Add a `Question lost` row for each retired question whose task can still resume.
    ///
    /// A question whose task is no longer current, because a control such as `/plan cancel`
    /// cleared or replaced it, ends with that task and gets no row: its advice to resume would
    /// name a task that no longer exists.
    pub(crate) fn report_lost_questions(
        &mut self,
        lost: &[crate::question::QuestionRecord],
    ) -> Result<()> {
        let current_task = self.task_controller.current()?;
        let resumable = |question: &&crate::question::QuestionRecord| {
            current_task
                .as_ref()
                .is_some_and(|task| question.task_id.as_deref() == Some(task.id.as_str()))
        };
        let now_ms = self.clock.now_ms();
        for question in lost.iter().filter(resumable) {
            self.store
                .save_session_event(&crate::timeline::SessionEventRecord {
                    id: format!("{}:question-lost:{}", self.session.id, question.id.as_str()),
                    session_id: self.session.id.clone(),
                    owner: crate::timeline::TimelineOwner::session(&self.session.id),
                    created_at_ms: now_ms,
                    detail: crate::timeline::SessionEventKind::TaskAdvancement {
                        message: lost_question_message(&question.content.header),
                    },
                })?;
        }
        Ok(())
    }

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

/// Name a question whose turn stopped before it was answered, and how the task continues.
fn lost_question_message(header: &str) -> String {
    format!(
        "Question lost: {header}. Its turn stopped before you answered; type the answer as a \
         prompt, then run /task resume to continue"
    )
}
