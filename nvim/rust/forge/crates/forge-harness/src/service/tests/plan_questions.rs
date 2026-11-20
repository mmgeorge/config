//! Drive questions a planner asks through the service and the mock provider.
//!
//! Each case starts a plan whose first planning turn asks the user a question, then checks
//! that the question holds the planning loop, survives a host restart, or, once the loop has
//! stopped at its limit, that a typed answer still reaches the model.

use super::super::{HarnessService, SessionController};
use super::bootstrap::{initialize, service};
use crate::provider::mock_response::MockResponseCommand;
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use forge_provider_api::testing::{
    TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep, TestPromptTerminal,
};
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

/// The outer bound on each wait guards only a hung run.
const BOUND: Duration = Duration::from_secs(8);

/// Own one mock session and the output reader that keeps its publications flowing.
struct PlanSession {
    service: HarnessService,
    controller: Arc<SessionController>,
    session_id: String,
    sink: forge_protocol::outbound::MessageSender,
    reader: tokio::task::JoinHandle<Result<()>>,
}

impl PlanSession {
    /// Open the mock session `session_id`, or a new one, over the scenario directory.
    async fn open(root: &tempfile::TempDir, session_id: Option<String>) -> Result<Self> {
        let service = service();
        let mut request = initialize(root, "mock");
        request.session_id = session_id;
        let opened = service.open_session(1, request).await?;
        anyhow::ensure!(opened.error().is_none(), "{:?}", opened.error());
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        Ok(Self {
            service,
            controller,
            session_id,
            sink,
            reader,
        })
    }

    async fn prompt(&self, id: u64, text: &str) -> Result<()> {
        self.service
            .dispatch(
                Some(self.session_id.clone()),
                Request {
                    id,
                    method: "session.prompt.submit".into(),
                    params: json!({"operation_id":format!("prompt-{id}"),"text":text}),
                },
                &self.sink,
            )
            .await
    }

    /// Queue one provider turn that runs `steps` and completes.
    async fn script(&self, steps: Vec<TestPromptStep>) -> Result<()> {
        self.controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?
            .dispatch(
                &self.session_id,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::BeforeResponse,
                        steps,
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        Ok(())
    }

    /// Queue a planning turn that asks one question without `wait` and then ends its turn.
    async fn script_question(&self) -> Result<()> {
        self.script(vec![
            TestPromptStep::ToolCall {
                name: "question_ask".into(),
                arguments: json!({"questions":[{"header":"Points","question":"How are points computed?",
                    "options":[{"label":"Per unit of subtotal","description":"One point per currency unit."}]}]}),
            },
            TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: "I will plan once you answer.".into(),
            },
        ])
        .await
    }

    fn plan(&self) -> Result<Option<crate::plan::PlanRecord>> {
        Ok(self
            .controller
            .task_controller
            .current()?
            .and_then(|task| match &task.workflow {
                crate::task::WorkflowSnapshot::Plan { plan } => Some(plan.artifact.clone()),
                _ => None,
            }))
    }

    fn pending_question(&self) -> Result<Option<crate::question::QuestionRecord>> {
        Ok(self
            .controller
            .questions
            .snapshot()?
            .questions
            .into_iter()
            .find(crate::question::QuestionRecord::pending))
    }

    fn exchanges(&self) -> Result<Vec<crate::exchange::Exchange>> {
        SqliteStore::read_only(&self.controller.data_root)?.list_exchange(&self.session_id)
    }

    /// Wait until `condition` holds, describing the plan and the latest exchange if it never does.
    async fn until(&self, what: &str, condition: impl Fn(&Self) -> Result<bool>) -> Result<()> {
        tokio::time::timeout(BOUND, async {
            loop {
                if condition(self)? {
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .with_context(|| {
            let plan = self.plan().ok().flatten();
            let exchanges = self.exchanges().unwrap_or_default();
            format!(
                "{what} never happened: plan {:?} budget {:?}; {} exchanges, latest {:?}; questions {:?}; status {:?}",
                plan.as_ref().map(|plan| plan.state),
                plan.as_ref().map(|plan| &plan.generation.budget),
                exchanges.len(),
                exchanges.last().map(|exchange| (&exchange.opening, exchange.state())),
                self.controller.questions.snapshot().map(|snapshot| snapshot.questions.into_iter()
                    .map(|question| (question.requirement, question.delivery, question.resolution))
                    .collect::<Vec<_>>()).ok(),
                self.controller.observer.snapshot().map(|snapshot| snapshot.status).ok(),
            )
        })?
    }

    /// Wait until the planner's question is pending and the session reads as waiting for it.
    async fn question_waits(&self) -> Result<crate::question::QuestionRecord> {
        self.until("a waiting planning question", |session| {
            Ok(session.pending_question()?.is_some()
                && matches!(
                    session.controller.observer.snapshot()?.status,
                    crate::session::state_machine::SessionPhase::AwaitingInput { .. }
                ))
        })
        .await?;
        self.pending_question()?
            .context("the planning question disappeared")
    }

    async fn answer(&self, question: &crate::question::QuestionRecord, text: &str) -> Result<()> {
        self.controller
            .exchange_owner
            .question_events()
            .change(crate::exchange::QuestionChange::user(
                crate::question::QuestionCommand {
                    operation_id: format!("answer-{}", question.id.as_str()),
                    question_id: question.id.clone(),
                    revision: question.content.revision,
                    action: crate::question::QuestionAction::Answer {
                        answer: crate::question::QuestionAnswer::Other { text: text.into() },
                    },
                },
            )?)
            .await?;
        Ok(())
    }

    async fn plan_in_review(&self) -> Result<()> {
        self.until("a submitted plan", |session| {
            Ok(session.plan()?.is_some_and(|plan| {
                plan.state == crate::plan::PlanState::AwaitingReview && plan.model_revision == 1
            }) && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await
    }

    /// Count the continuation turns the planning loop started after a turn without a plan.
    fn continuation_turns(&self) -> Result<usize> {
        Ok(self
            .exchanges()?
            .iter()
            .filter(|exchange| {
                exchange.opening
                    == crate::exchange::ExchangeOpening::task_continuation(
                        "The planning turn ended without submitting a plan",
                    )
            })
            .count())
    }

    /// Read the transcript rows that report a question lost with its turn.
    fn lost_question_rows(&self) -> Result<Vec<String>> {
        Ok(SqliteStore::read_only(&self.controller.data_root)?
            .list_session_event(&self.session_id)?
            .into_iter()
            .filter_map(|row| match row.detail {
                crate::timeline::SessionEventKind::TaskAdvancement { message }
                    if message.starts_with("Question lost:") =>
                {
                    Some(message)
                }
                _ => None,
            })
            .collect())
    }

    async fn close(self) -> Result<()> {
        self.service.shutdown(Duration::from_secs(5)).await?;
        self.reader.abort();
        let _ = self.reader.await;
        Ok(())
    }
}

/// A question the planner asked holds the planning loop until the user answers it.
///
/// A planner that asked without `wait` and ended its turn got one continuation turn after
/// another, each ending without a plan, until the no-progress limit stopped planning while the
/// question still waited. The question now holds the turn that asked it: no continuation turn
/// starts, the no-progress count stays at zero, and the status reads `Waiting for your answer`.
#[tokio::test]
async fn planning_question_holds_the_loop_until_answered() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    session.script_question().await?;
    session
        .prompt(2, "/plan Add a loyalty points field")
        .await?;
    let question = session.question_waits().await?;
    let plan = session.plan()?.context("planning task disappeared")?;
    assert_eq!(plan.generation.budget.consecutive_no_progress, 0);
    assert_eq!(session.continuation_turns()?, 0);
    session.answer(&question, "One point per unit").await?;
    session.plan_in_review().await?;
    session.close().await
}

/// A planning question waiting when the host stops is lost with its turn, and the reopened
/// session says so and how to continue.
///
/// The provider keeps no pending tool call across a host stop, so the question cannot wait
/// again. The reopened plan is paused, the transcript names the lost question, a typed answer
/// reaches the model, and `/task resume` plans with it.
#[tokio::test]
async fn planning_question_lost_at_a_restart_says_how_to_continue() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    session.script_question().await?;
    session
        .prompt(2, "/plan Add a loyalty points field")
        .await?;
    session.question_waits().await?;
    let session_id = session.session_id.clone();
    session.close().await?;

    let reopened = PlanSession::open(&root, Some(session_id)).await?;
    reopened
        .until("the paused plan", |session| {
            Ok(session
                .controller
                .task_controller
                .current()?
                .is_some_and(|task| task.status() == crate::task::TaskStatus::Paused)
                && session.pending_question()?.is_none())
        })
        .await?;
    assert_eq!(
        reopened.lost_question_rows()?,
        [
            "Question lost: Points. Its turn stopped before you answered; type the answer as a \
          prompt, then run /task resume to continue"
        ],
        "the transcript does not say in one sentence that the question was lost"
    );
    let answer = "One point per unit of subtotal.";
    reopened.prompt(3, answer).await?;
    reopened
        .until("the typed answer ran", |session| {
            Ok(session.exchanges()?.iter().any(|exchange| {
                exchange.prompt == answer
                    && exchange.state() == crate::exchange::ExchangeState::Complete
            }) && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await?;
    reopened
        .service
        .dispatch(
            Some(reopened.session_id.clone()),
            Request {
                id: 4,
                method: "session.command.apply".into(),
                params: json!({"action":"resume","operation_id":"resume-4"}),
            },
            &reopened.sink,
        )
        .await?;
    reopened.plan_in_review().await?;
    reopened.close().await
}

/// Cancelling a plan while its question waits ends the question with the plan.
///
/// `/plan cancel` stops the planning turn and clears the task, and the question went with it
/// under a `Question lost` row whose advice, to answer and then run `/task resume`, named a task
/// that no longer existed. The question is retired and the transcript records no loss.
#[tokio::test]
async fn plan_cancel_while_a_question_waits_reports_no_lost_question() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    session.script_question().await?;
    session
        .prompt(2, "/plan Add a loyalty points field")
        .await?;
    session.question_waits().await?;
    session.prompt(3, "/plan cancel").await?;
    session
        .until("the cleared plan", |session| {
            Ok(session.controller.task_controller.current()?.is_none()
                && session.pending_question()?.is_none()
                && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await?;
    assert_eq!(session.lost_question_rows()?, Vec::<String>::new());
    session.close().await
}

/// A prompt typed after planning stopped at its no-progress limit reaches the model.
///
/// Planning that has stopped refused the prompt as a failed turn, which restarted the provider
/// and left the prompt in a paused queue as `Provider did not confirm message acceptance`,
/// although the provider was never asked to take it.
#[tokio::test]
async fn prompt_after_planning_stopped_reaches_the_model() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    let limit =
        crate::session::continuation::ContinuationBudget::default().max_consecutive_no_progress;
    for _ in 0..limit {
        session
            .script(vec![TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: "I need to know how points are computed.".into(),
            }])
            .await?;
    }
    session
        .prompt(2, "/plan Add a loyalty points field")
        .await?;
    session
        .until("planning stopped at its limit", |session| {
            Ok(session
                .plan()?
                .is_some_and(|plan| plan.state == crate::plan::PlanState::Failed)
                && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await?;
    let answer = "Points come from the subtotal.";
    session.prompt(3, answer).await?;
    session
        .until("the typed answer settled", |session| {
            let queue = session.controller.coordinator.submissions.snapshot()?;
            Ok(queue.messages.iter().all(|message| message.text != answer)
                && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await
        .map_err(|error| {
            let queue = session
                .controller
                .coordinator
                .submissions
                .snapshot()
                .map(|queue| {
                    queue
                        .messages
                        .iter()
                        .map(|message| (message.text.clone(), message.delivery()))
                        .collect::<Vec<_>>()
                });
            error.context(format!("queue {queue:?}"))
        })?;
    let delivered = session.exchanges()?.into_iter().any(|exchange| {
        exchange.prompt == answer && exchange.state() == crate::exchange::ExchangeState::Complete
    });
    assert!(delivered, "the typed answer never ran as a turn");
    session.close().await
}

/// A prompt Harness refuses before any provider work stays in the queue with the reason.
///
/// A refusal from prompt admission, such as a bare `/read`, became a failed turn: it restarted
/// the provider, cancelled the prepared submission, and left the entry failed with `Provider did
/// not confirm message acceptance`, although the provider was never asked to take it. The entry
/// now names the refusal, no exchange fails, and once the entry is removed the next prompt runs.
#[tokio::test]
async fn refused_prompt_keeps_its_reason_without_failing_a_turn() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    session.prompt(2, "/read").await?;
    session
        .until("the refused prompt settled", |session| {
            let queue = session.controller.coordinator.submissions.snapshot()?;
            Ok(queue.messages.iter().any(|message| {
                message.text == "/read" && message.delivery() == crate::input::InputDelivery::Failed
            }) && session.controller.coordinator.execution.available_permits() == 1)
        })
        .await?;
    let queue = session.controller.coordinator.submissions.snapshot()?;
    let refused = queue
        .messages
        .iter()
        .find(|message| message.text == "/read")
        .context("the refused prompt left the queue")?;
    assert_eq!(
        refused.error(),
        Some("Use /read full, /write, /write full, /ask, /deny, /allow, or /yolo")
    );
    assert!(
        session
            .exchanges()?
            .iter()
            .all(|exchange| exchange.state() != crate::exchange::ExchangeState::Failed),
        "the refusal failed a turn"
    );
    // A failed entry blocks later follow-ups until the user retries, edits, or removes it.
    session
        .service
        .dispatch(
            Some(session.session_id.clone()),
            Request {
                id: 3,
                method: "session.input.apply".into(),
                params: json!({"id":"remove-3","control_revision":queue.control_revision,
                    "action":{"kind":"remove","id":refused.id,"revision":refused.revision}}),
            },
            &session.sink,
        )
        .await?;
    session
        .script(vec![TestPromptStep::Message {
            content: TestMessageContent::Response,
            text: "ready".into(),
        }])
        .await?;
    session.prompt(4, "Reply with ready").await?;
    session
        .until("the next prompt completed", |session| {
            Ok(session.exchanges()?.iter().any(|exchange| {
                exchange.prompt == "Reply with ready"
                    && exchange.state() == crate::exchange::ExchangeState::Complete
            }))
        })
        .await
        .map_err(|error| {
            let queue = session
                .controller
                .coordinator
                .submissions
                .snapshot()
                .map(|queue| {
                    (
                        queue.suspended,
                        queue
                            .messages
                            .iter()
                            .map(|message| (message.text.clone(), message.delivery()))
                            .collect::<Vec<_>>(),
                    )
                });
            error.context(format!("queue {queue:?}"))
        })?;
    session.close().await
}

/// A question answered while the turn that asked it keeps running reads as answered in the live
/// transcript at once, not only when the turn ends.
///
/// Live updates rebuilt the presented exchange from its in-memory record and carried the question
/// rows over from the previous frame, so `Questions · 0/1 answered … Awaiting answer` stayed on
/// screen while the model worked with the answer, until the next full snapshot.
#[tokio::test]
async fn an_answer_accepted_during_its_turn_shows_in_the_live_transcript() -> Result<()> {
    use forge_provider_api::testing::TestHoldId;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let session = PlanSession::open(&root, None).await?;
    let transcript = forge_buffer::identity::DocumentId("transcript:live-answer".into());
    session.controller.presentation.lock().unwrap().open(
        transcript.clone(),
        forge_buffer::identity::ViewId("view:live-answer".into()),
        forge_buffer::width::WidthProfile::default(),
    )?;
    session
        .script(vec![
            TestPromptStep::ToolCall {
                name: "question_ask".into(),
                arguments: json!({"wait":true,"questions":[{"header":"Points","question":"How are points computed?",
                    "options":[{"label":"Per unit of subtotal","description":"One point per currency unit."}]}]}),
            },
            TestPromptStep::Barrier {
                id: TestHoldId::new("after-the-answer")?,
            },
            TestPromptStep::Message {
                content: TestMessageContent::Response,
                text: "I will plan with that rate.".into(),
            },
        ])
        .await?;
    session
        .prompt(2, "/plan Add a loyalty points field")
        .await?;
    let question = session.question_waits().await?;
    session.answer(&question, "One point per unit").await?;
    let control = session
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    tokio::time::timeout(
        BOUND,
        control.dispatch(
            &session.session_id,
            MockResponseCommand::Wait {
                id: "after-the-answer".into(),
            },
        ),
    )
    .await??;
    let rows = session
        .controller
        .presentation
        .lock()
        .unwrap()
        .snapshot(&transcript)?
        .block
        .iter()
        .flat_map(|block| block.text.rows().map(str::to_owned).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    control
        .dispatch(
            &session.session_id,
            MockResponseCommand::Release {
                id: "after-the-answer".into(),
            },
        )
        .await?;
    assert!(
        rows.iter().any(|row| row.contains("Answered 1 question")),
        "the live transcript does not show the accepted answer: {rows:#?}"
    );
    assert!(
        !rows.iter().any(|row| row.contains("Awaiting answer")),
        "the live transcript still waits for the accepted answer: {rows:#?}"
    );
    session.close().await
}
