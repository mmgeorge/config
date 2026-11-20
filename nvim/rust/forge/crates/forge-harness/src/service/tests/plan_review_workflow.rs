//! Drive plan review decisions through the service and the mock provider.
//!
//! Each case reaches a submitted plan through the mock planner, then exercises one review
//! decision, revision outcome, or cleanup boundary that the editor relies on.

use super::super::{HarnessService, SessionController};
use super::bootstrap::{initialize, service};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// Owns one mock session whose responses the cases read by request identity.
struct ReviewFixture {
    root: tempfile::TempDir,
    service: HarnessService,
    controller: Arc<SessionController>,
    session_id: String,
    sink: forge_protocol::outbound::MessageSender,
    frame_list: Arc<Mutex<Vec<Value>>>,
    reader: tokio::task::JoinHandle<Result<()>>,
}

impl ReviewFixture {
    /// Open a mock session, optionally without project checks so planning starts with setup.
    async fn open(configure_checks: bool) -> Result<Self> {
        let root = tempfile::tempdir()?;
        if configure_checks {
            crate::checks::configure_fixture(root.path());
        }
        let service = service();
        service.open_session(1, initialize(&root, "mock")).await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let frame_list = Arc::new(Mutex::new(Vec::new()));
        let received = Arc::clone(&frame_list);
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                received
                    .lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        Ok(Self {
            root,
            service,
            controller,
            session_id,
            sink,
            frame_list,
            reader,
        })
    }

    async fn dispatch(&self, id: u64, method: &str, params: Value) -> Result<()> {
        self.service
            .dispatch(
                Some(self.session_id.clone()),
                Request {
                    id,
                    method: method.into(),
                    params,
                },
                &self.sink,
            )
            .await
    }

    async fn prompt(&self, id: u64, text: &str) -> Result<()> {
        self.dispatch(
            id,
            "session.prompt.submit",
            json!({"operation_id":format!("prompt-{id}"),"text":text}),
        )
        .await
    }

    /// Wait for the response frame that answers `id`.
    async fn response(&self, id: u64) -> Result<Value> {
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                if let Some(frame) = self
                    .frame_list
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|frame| frame["id"] == id)
                {
                    break frame.clone();
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .with_context(|| format!("request {id} was never answered"))
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

    /// Wait until no turn holds the session and the condition holds.
    async fn settled(&self, what: &str, condition: impl Fn(&Self) -> Result<bool>) -> Result<()> {
        tokio::time::timeout(Duration::from_secs(8), async {
            loop {
                if self.controller.coordinator.execution.available_permits() == 1
                    && condition(self)?
                {
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .with_context(|| {
            let plan = self.plan().ok().flatten();
            let exchange = self
                .store()
                .ok()
                .and_then(|store| store.latest_exchange(&self.session_id).ok().flatten());
            let task = self.controller.task_controller.current().ok().flatten();
            let queue = self
                .controller
                .coordinator
                .submissions
                .snapshot()
                .ok()
                .map(|snapshot| {
                    snapshot
                        .messages
                        .iter()
                        .map(|message| (message.text.clone(), message.delivery()))
                        .collect::<Vec<_>>()
                });
            format!(
                "{what} never settled: queue {queue:?}; task turn {:?} budget {:?}; plan {:?} revision {:?}, latest exchange {:?} {:?}",
                task.as_ref().map(|task| (&task.turn, &task.observed_turn, task.status())),
                plan.as_ref().map(|plan| &plan.generation),
                plan.as_ref().map(|plan| plan.state),
                plan.as_ref().map(|plan| plan.model_revision),
                exchange.as_ref().map(|exchange| exchange.ordinal),
                exchange.as_ref().map(|exchange| exchange.state()),
            )
        })?
    }

    /// Plan through the mock planner until its first revision awaits review.
    async fn plan_in_review(&self) -> Result<crate::plan::PlanRecord> {
        self.prompt(3, "/plan Add an isolated callable").await?;
        self.settled("planning", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.state == crate::plan::PlanState::AwaitingReview && plan.model_revision == 1
            }))
        })
        .await?;
        self.plan()?.context("planning task disappeared")
    }

    /// Queue provider responses that answer with prose and never submit a plan.
    async fn script_prose_responses(&self, count: usize) -> Result<()> {
        use crate::provider::mock_response::MockResponseCommand;
        use forge_provider_api::testing::{
            TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
            TestPromptTerminal,
        };
        let control = self
            .controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?;
        for _ in 0..count {
            control
                .dispatch(
                    &self.session_id,
                    MockResponseCommand::Script {
                        script: TestPromptScript {
                            acceptance: TestInputAcceptance::BeforeResponse,
                            steps: vec![TestPromptStep::Message {
                                content: TestMessageContent::Response,
                                text: "The plan needs no change, so I did not resubmit it.".into(),
                            }],
                            terminal: TestPromptTerminal::Completed,
                        },
                    },
                )
                .await?;
        }
        Ok(())
    }

    fn task(&self) -> Result<crate::task::Task> {
        self.controller
            .task_controller
            .current()?
            .context("the task disappeared")
    }

    /// Resume the task the way `/task resume` does, and require that the command succeeded.
    async fn resume(&self, id: u64) -> Result<Value> {
        self.dispatch(
            id,
            "session.command.apply",
            json!({"action":"resume","operation_id":format!("resume-{id}")}),
        )
        .await?;
        let response = self.response(id).await?;
        assert!(response.get("error").is_none(), "{response}");
        Ok(response)
    }

    /// Count the transcript rows that announce execution, which only an Execute task's start writes.
    fn entered_execution_rows(&self) -> Result<usize> {
        Ok(self
            .store()?
            .list_session_event(&self.session_id)?
            .iter()
            .filter(|event| {
                matches!(&event.detail, crate::timeline::SessionEventKind::TaskAdvancement { message }
                    if message == "Approved plan entered execution")
            })
            .count())
    }

    /// Require that no plan acceptance was recorded as failed, which the transcript draws as an
    /// `Execution did not start` row, and that execution was announced exactly once.
    fn assert_execution_started_once(&self) -> Result<()> {
        let refused = self
            .store()?
            .list_session_event(&self.session_id)?
            .into_iter()
            .filter_map(|event| match event.detail {
                crate::timeline::SessionEventKind::TaskAdvancement { message }
                    if message.starts_with("Execution did not start") =>
                {
                    Some(message)
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(refused.is_empty(), "an acceptance was refused: {refused:?}");
        assert_eq!(self.entered_execution_rows()?, 1);
        Ok(())
    }

    /// Accept the plan in review and answer its execution questions.
    async fn accept_and_answer(&self, first_id: u64) -> Result<()> {
        let plan = self.plan()?.context("planning task disappeared")?;
        self.dispatch(
            first_id,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
        self.response(first_id).await?;
        self.settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
        self.answer_acceptance(first_id + 1).await?;
        Ok(())
    }

    /// Pause the task the way `/task pause` does, without a provider turn.
    fn pause(&self) -> Result<()> {
        let task = self.task()?;
        self.controller.task_controller.change(
            &mut self.store()?,
            Some(&task.revision),
            crate::task::TaskChange::Pause,
            1,
        )?;
        assert_eq!(self.task()?.status(), crate::task::TaskStatus::Paused);
        Ok(())
    }

    fn store(&self) -> Result<SqliteStore> {
        SqliteStore::open(&self.controller.data_root)
    }

    /// Save one inline comment on the reviewed plan the way the review document does: keyed by
    /// the saved working document, before the editor submits its decision.
    fn save_comment(&self, plan: &crate::plan::PlanRecord, body: &str) -> Result<()> {
        let digest = plan
            .review_digest
            .as_deref()
            .context("plan has no review digest")?;
        let source = self
            .controller
            .observer
            .capture_plan_review(&plan.id, digest)?;
        let anchor = source
            .rendered
            .navigation
            .anchor
            .first()
            .context("the reviewed plan has no anchored line")?;
        let line = anchor.line;
        // `save_annotations` anchors each comment to the semantic target of its rows, so the
        // comment survives a later re-render of the same document.
        let target = serde_json::to_value(&anchor.target)?;
        let working = std::path::Path::new(&plan.working_path).with_file_name("working.json");
        let working_digest = crate::plan::digest(&std::fs::read(&working)?);
        let saved = json!({"source_digest":working_digest,"annotation":[
            {"id":"comment-1","source":{"start_line":line,"end_line":line,"body":body},
                "anchor":{"start":target,"end":target}}
        ]});
        std::fs::write(
            working.with_file_name(format!("review-annotations-{working_digest}.json")),
            serde_json::to_vec(&saved)?,
        )?;
        Ok(())
    }

    /// Answer every pending acceptance question with its first option, then continue.
    /// Returns the editor reply to the continuation.
    async fn answer_acceptance(&self, first_id: u64) -> Result<Value> {
        let plan = self.plan()?.context("planning task disappeared")?;
        let questions = plan
            .review_questions()
            .context("acceptance raised no questions")?
            .clone();
        let mut id = first_id;
        for question in &questions.question_set.questions {
            let option = question.options.first().context("question has no option")?;
            self.dispatch(
                id,
                "session.question.answer",
                json!({
                    "operation_id":format!("answer-{id}"),
                    "target":{"owner":"plan_review","id":plan.id,"set_id":questions.question_set.id,"revision":questions.revision},
                    "question_id":question.id,
                    "response":{"kind":"selected","option":option.label},
                }),
            )
            .await?;
            self.response(id).await?;
            id += 1;
        }
        let questions = self
            .plan()?
            .context("planning task disappeared")?
            .review_questions()
            .context("acceptance questions disappeared")?
            .clone();
        self.dispatch(
            id,
            "session.question.continue",
            json!({"operation_id":format!("continue-{id}"),
                "target":{"owner":"plan_review","id":plan.id,"set_id":questions.question_set.id,"revision":questions.revision}}),
        )
        .await?;
        self.response(id).await
    }

    /// Require that the comment reached a revision before any acceptance: a `ChangesRequested`
    /// record carries it, no `Accepted` record precedes it, and the revision instruction quotes it.
    fn assert_revised_before_accepted(&self, body: &str, accepted: bool) -> Result<()> {
        let lifecycle = self.store()?.list_plan_lifecycle(&self.session_id)?;
        let kinds = lifecycle
            .iter()
            .map(|record| record.kind)
            .collect::<Vec<_>>();
        let requested = lifecycle
            .iter()
            .position(|record| {
                record.kind == crate::plan::PlanLifecycleKind::ChangesRequested
                    && serde_json::to_string(&record.annotation)
                        .is_ok_and(|annotation| annotation.contains(body))
            })
            .with_context(|| format!("no revision carried the saved comment: {kinds:?}"))?;
        let first_accepted = lifecycle
            .iter()
            .position(|record| record.kind == crate::plan::PlanLifecycleKind::Accepted);
        assert!(
            first_accepted.is_none_or(|accepted| accepted > requested),
            "the plan was accepted before its comment was revised: {kinds:?}"
        );
        assert_eq!(first_accepted.is_some(), accepted, "{kinds:?}");
        let instruction = self
            .store()?
            .list_exchange(&self.session_id)?
            .into_iter()
            .filter(|exchange| {
                exchange.opening
                    == crate::exchange::ExchangeOpening::task_continuation("Revision requested")
                    || serde_json::to_string(&exchange.turn).is_ok_and(|turn| turn.contains(body))
            })
            .count();
        assert!(
            instruction > 0,
            "no revision turn carried the saved comment"
        );
        Ok(())
    }

    async fn finish(self) -> Result<()> {
        self.controller.finish_exchange_owner().await?;
        self.reader.abort();
        let result = self.reader.await;
        assert!(result.is_ok() || result.is_err_and(|error| error.is_cancelled()));
        drop(self.root);
        Ok(())
    }
}

/// A change request with no overall comment, no inline comment, and no review edit gives
/// the model nothing to act on, so admission refuses it and the plan stays in review.
#[tokio::test]
async fn empty_change_request_is_refused_and_plan_stays_in_review() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let exchange_count = fixture.store()?.list_exchange(&fixture.session_id)?.len();
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"empty-change","plan_id":plan.id,"comment":"  "}),
        )
        .await?;
    let response = fixture.response(20).await?;
    assert_eq!(
        response["error"]["code"], "plan_revision_empty",
        "{response}"
    );
    let current = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(current.state, crate::plan::PlanState::AwaitingReview);
    assert_eq!(current.user_revision, plan.user_revision);
    assert_eq!(
        fixture.store()?.list_exchange(&fixture.session_id)?.len(),
        exchange_count
    );
    fixture.finish().await
}

/// A revision whose turns never resubmit returns the plan to review at its last submitted
/// revision, records why the revision stopped, and keeps the review comments it was sent.
#[tokio::test]
async fn abandoned_revision_returns_to_review_and_keeps_comments() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let working = std::path::Path::new(&plan.working_path).with_file_name("working.json");
    let working_digest = crate::plan::digest(&std::fs::read(&working)?);
    let comment_path = working.with_file_name(format!("review-annotations-{working_digest}.json"));
    let comments = json!({"source_digest":working_digest,"annotation":[
        {"id":"comment-1","source":{"start_line":1,"end_line":1,"body":"Rename the callable"}}
    ]});
    std::fs::write(&comment_path, serde_json::to_vec(&comments)?)?;
    let budget = plan.generation.budget.max_consecutive_no_progress;
    fixture.script_prose_responses(usize::from(budget)).await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Rename the callable"}),
        )
        .await?;
    fixture.response(20).await?;
    fixture
        .settled("abandoned revision", |fixture| {
            Ok(fixture.plan()?.is_some_and(|current| {
                current.user_revision > plan.user_revision
                    && !matches!(
                        current.state,
                        crate::plan::PlanState::Revising | crate::plan::PlanState::Generating
                    )
            }))
        })
        .await?;
    let current = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(current.state, crate::plan::PlanState::AwaitingReview);
    assert_eq!(current.model_revision, plan.model_revision);
    assert_eq!(current.review_digest, plan.review_digest);
    let stopped = fixture
        .store()?
        .list_plan_lifecycle(&fixture.session_id)?
        .into_iter()
        .filter(|lifecycle| lifecycle.kind == crate::plan::PlanLifecycleKind::RevisionStopped)
        .count();
    assert_eq!(stopped, 1);
    assert_eq!(
        crate::plan::digest(&std::fs::read(&working)?),
        working_digest
    );
    assert_eq!(
        serde_json::from_slice::<Value>(&std::fs::read(&comment_path)?)?,
        comments
    );
    fixture.finish().await
}

/// A revision requested after a later discussion starts its own exchange instead of
/// reopening the finished discussion.
#[tokio::test]
async fn revision_after_discussion_starts_its_own_exchange() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture.script_prose_responses(1).await?;
    fixture.prompt(10, "Explain the proposed callable.").await?;
    fixture
        .settled("discussion", |fixture| {
            Ok(fixture
                .store()?
                .latest_exchange(&fixture.session_id)?
                .is_some_and(|exchange| exchange.is_finished() && exchange.ordinal == 2))
        })
        .await?;
    let discussion = fixture
        .store()?
        .latest_exchange(&fixture.session_id)?
        .context("discussion exchange is missing")?;
    fixture
        .script_prose_responses(usize::from(
            plan.generation.budget.max_consecutive_no_progress,
        ))
        .await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Rename the callable"}),
        )
        .await?;
    fixture
        .settled("revision", |fixture| {
            Ok(fixture.plan()?.is_some_and(|current| {
                current.user_revision > plan.user_revision
                    && current.state != crate::plan::PlanState::Revising
            }))
        })
        .await?;
    let store = fixture.store()?;
    let reloaded = store
        .list_exchange(&fixture.session_id)?
        .into_iter()
        .find(|exchange| exchange.id == discussion.id)
        .context("discussion exchange disappeared")?;
    assert_eq!(reloaded.turn.len(), discussion.turn.len());
    assert_eq!(reloaded.state(), discussion.state());
    assert_eq!(reloaded.completed_at_ms(), discussion.completed_at_ms());
    let revision = store
        .list_exchange(&fixture.session_id)?
        .into_iter()
        .find(|exchange| exchange.ordinal == discussion.ordinal + 1)
        .context("the revision did not start its own exchange")?;
    assert_eq!(
        revision.opening,
        crate::exchange::ExchangeOpening::task_continuation("Revision requested")
    );
    fixture.finish().await
}

/// Turning automatic acceptance off withdraws the acceptance it started, so no hidden
/// execution question keeps the plan waiting for an answer.
#[tokio::test]
async fn automatic_acceptance_off_withdraws_its_pending_question() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    fixture.prompt(10, "/plan accept on").await?;
    fixture
        .settled("automatic acceptance", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.review_questions()
                    .is_some_and(|questions| questions.has_pending_decisions())
            }))
        })
        .await?;
    fixture.prompt(11, "/plan accept off").await?;
    fixture
        .settled("manual acceptance", |fixture| {
            Ok(fixture
                .controller
                .task_controller
                .current()?
                .is_some_and(|task| !task.auto_accept_plan))
        })
        .await?;
    let plan = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(plan.state, crate::plan::PlanState::AwaitingReview);
    assert!(
        plan.review_questions().is_none(),
        "a question stayed pending"
    );
    fixture.finish().await
}

/// An explicit pause holds through `oY`: the acceptance and its questions are recorded, nothing
/// starts, and `/task resume` presents the questions the acceptance raised.
#[tokio::test]
async fn review_acceptance_keeps_an_explicit_pause() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture.pause()?;
    fixture
        .dispatch(
            20,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
    let response = fixture.response(20).await?;
    assert!(response.get("error").is_none(), "{response}");
    fixture
        .settled("recorded acceptance", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.review
                    .as_ref()
                    .is_some_and(|review| review.execution_request.is_some())
            }))
        })
        .await?;
    let task = fixture.task()?;
    assert_eq!(task.status(), crate::task::TaskStatus::Paused);
    assert_eq!(task.kind(), crate::task::TaskKind::Plan);
    assert_eq!(
        fixture.entered_execution_rows()?,
        0,
        "a paused acceptance announced execution"
    );
    fixture.resume(21).await?;
    fixture
        .settled("resume", |fixture| {
            Ok(fixture.task()?.status() != crate::task::TaskStatus::Paused)
        })
        .await?;
    let plan = fixture.plan()?.context("planning task disappeared")?;
    assert!(
        plan.review_questions()
            .is_some_and(|questions| questions.has_pending_decisions()),
        "resume did not present the recorded acceptance's questions"
    );
    assert_eq!(fixture.entered_execution_rows()?, 0);
    fixture.answer_acceptance(30).await?;
    fixture
        .settled("execution", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    assert_eq!(fixture.entered_execution_rows()?, 1);
    fixture.finish().await
}

/// An acceptance whose questions were answered before the pause is carried out at once by
/// `/task resume`.
#[tokio::test]
async fn resume_carries_out_an_acceptance_recorded_while_paused() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture
        .dispatch(
            20,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
    fixture.response(20).await?;
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.pause()?;
    fixture.answer_acceptance(21).await?;
    fixture.settled("recorded acceptance", |_| Ok(true)).await?;
    let task = fixture.task()?;
    assert_eq!(task.status(), crate::task::TaskStatus::Paused);
    assert_eq!(
        task.kind(),
        crate::task::TaskKind::Plan,
        "a paused plan entered Execute"
    );
    fixture.resume(30).await?;
    fixture
        .settled("execution", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    fixture.finish().await
}

/// An interrupt with nothing running leaves a plan that waits for review waiting.
///
/// Cancellation paused every unfinished task, so a `Ctrl-C` after submission turned the plan
/// into `plan · review · paused` beside a status that still said it waited for review.
#[tokio::test]
async fn idle_interrupt_keeps_a_plan_in_review_waiting() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    fixture
        .dispatch(
            30,
            "session.turn.cancel",
            json!({"operation_id":"idle-cancel","restore_prompt_if_no_output":true}),
        )
        .await?;
    fixture.response(30).await?;
    fixture.settled("idle interrupt", |_| Ok(true)).await?;
    let task = fixture
        .controller
        .task_controller
        .current()?
        .context("planning task disappeared")?;
    assert_eq!(task.status(), crate::task::TaskStatus::Waiting);
    fixture.finish().await
}

/// Shutting the owner down leaves a plan that waits for review waiting, so a restarted session
/// opens it in review rather than as `plan · review · paused`.
#[tokio::test]
async fn shutdown_keeps_a_plan_in_review_waiting() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    let store = fixture.store()?;
    let session_id = fixture.session_id.clone();
    fixture.service.shutdown(Duration::from_secs(2)).await?;
    let task = store
        .load_active_task(&session_id)?
        .context("planning task disappeared")?;
    assert_eq!(task.status(), crate::task::TaskStatus::Waiting);
    fixture.reader.abort();
    Ok(())
}

/// A plan waiting for review after an abrupt close stays waiting once the next owner recovers
/// the session, as it does after a clean shutdown: recovery pauses only work that was running.
#[tokio::test]
async fn abrupt_close_keeps_a_plan_in_review_waiting() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    let mut store = fixture.store()?;
    let session_id = fixture.session_id.clone();
    // Startup recovery of the next owner, run against the state the abandoned owner left.
    store.interrupt_detached_execution(&session_id)?;
    let task = store
        .load_active_task(&session_id)?
        .context("planning task disappeared")?;
    assert_eq!(task.status(), crate::task::TaskStatus::Waiting);
    fixture.finish().await
}

/// `oY` with an empty decision comment and automatic acceptance off: the saved inline comment
/// is revised first, and the revision returns to review instead of entering Execute.
#[tokio::test]
async fn manual_acceptance_revises_a_saved_comment_first() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture.save_comment(&plan, "Rename the callable to compute_total")?;
    fixture
        .dispatch(
            20,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
    let accepted = fixture.response(20).await?;
    assert!(accepted.get("error").is_none(), "{accepted}");
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.answer_acceptance(21).await?;
    // Settle on either outcome, so a plan that skipped its revision fails the assertion below
    // instead of the wait.
    fixture
        .settled("revision", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute
                || fixture.plan()?.is_some_and(|current| {
                    current.model_revision > plan.model_revision
                        && current.state == crate::plan::PlanState::AwaitingReview
                }))
        })
        .await?;
    fixture.assert_revised_before_accepted("compute_total", false)?;
    assert_eq!(
        fixture.entered_execution_rows()?,
        0,
        "the revision announced execution"
    );
    fixture.accept_and_answer(40).await?;
    fixture
        .settled("execution", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    assert_eq!(fixture.entered_execution_rows()?, 1);
    fixture.finish().await
}

/// The provider thread keeps a cancelled plan's turns, so the next plan's first turn says the
/// cancelled plan must not be continued.
#[tokio::test]
async fn new_plan_after_a_cancelled_plan_says_so() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let cancelled = fixture.plan_in_review().await?;
    fixture.prompt(10, "/plan cancel").await?;
    fixture
        .settled("cancellation", |fixture| {
            Ok(fixture.controller.task_controller.current()?.is_none())
        })
        .await?;
    fixture.prompt(11, "/plan Add a second callable").await?;
    fixture
        .settled("second plan", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.id != cancelled.id && plan.state == crate::plan::PlanState::AwaitingReview
            }))
        })
        .await?;
    let task = fixture.task()?;
    let entry = serde_json::to_string(
        &task
            .entry
            .as_ref()
            .context("the new plan has no entry")?
            .instruction,
    )?;
    assert!(
        entry.contains("plan_abandoned") && entry.contains(&cancelled.title),
        "the new plan's first turn did not say the earlier plan was cancelled: {entry}"
    );
    fixture.finish().await
}

/// `/plan accept on` during review accepts at once; a saved inline comment is revised first,
/// and the revised submission is then accepted automatically.
#[tokio::test]
async fn automatic_acceptance_in_review_revises_a_saved_comment_first() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture.save_comment(&plan, "Rename the callable to compute_total")?;
    fixture.prompt(20, "/plan accept on").await?;
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.answer_acceptance(21).await?;
    fixture
        .settled("revised acceptance", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    fixture.assert_revised_before_accepted("compute_total", true)?;
    fixture.assert_execution_started_once()?;
    fixture.finish().await
}

/// With automatic acceptance already on, a later submission carrying a saved comment from its
/// review is revised before the automatic acceptance runs.
#[tokio::test]
async fn automatic_acceptance_of_a_later_submission_revises_a_saved_comment_first() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Add an overview"}),
        )
        .await?;
    fixture.response(20).await?;
    fixture
        .settled("first revision", |fixture| {
            Ok(fixture.plan()?.is_some_and(|current| {
                current.model_revision > plan.model_revision
                    && current.state == crate::plan::PlanState::AwaitingReview
            }))
        })
        .await?;
    let revised = fixture.plan()?.context("planning task disappeared")?;
    fixture.save_comment(&revised, "Rename the callable to compute_total")?;
    fixture.prompt(21, "/plan accept on").await?;
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.answer_acceptance(22).await?;
    fixture
        .settled("revised acceptance", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    fixture.assert_revised_before_accepted("compute_total", true)?;
    fixture.assert_execution_started_once()?;
    fixture.finish().await
}

/// A draft turn the provider fails leaves the plan failed, not drafting with nothing running,
/// so `/plan retry` starts the draft again and the planner can submit.
#[tokio::test]
async fn failed_draft_turn_is_retryable() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{TestInputAcceptance, TestPromptScript, TestPromptTerminal};
    let fixture = ReviewFixture::open(true).await?;
    fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: Vec::new(),
                    terminal: TestPromptTerminal::Failed(
                        forge_provider_api::error::ProviderFailureKind::Protocol,
                    ),
                },
            },
        )
        .await?;
    fixture.prompt(3, "/plan Add an isolated callable").await?;
    fixture
        .settled("failed draft", |fixture| {
            Ok(fixture.task()?.status() == crate::task::TaskStatus::Failed)
        })
        .await?;
    fixture.prompt(4, "/plan retry").await?;
    fixture
        .settled("retried draft", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.state == crate::plan::PlanState::AwaitingReview))
        })
        .await?;
    fixture.finish().await
}

/// `/task resume` on a task that is not paused is refused with a reason the editor shows.
#[tokio::test]
async fn resume_of_a_running_task_says_why_it_was_refused() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"resume","operation_id":"resume-20"}),
        )
        .await?;
    let response = fixture.response(20).await?;
    let message = response
        .pointer("/error/message")
        .and_then(Value::as_str)
        .unwrap_or_default();
    assert!(message.contains("nothing to resume"), "{response}");
    fixture.finish().await
}

/// Execution refuses an acceptance whose proposed file appeared in the workspace after review.
/// The editor's reply names that reason, and the refused decision is withdrawn: no acceptance
/// question stays pending and no recorded acceptance replays it later.
#[tokio::test]
async fn refused_acceptance_withdraws_its_decision_and_says_why() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture
        .dispatch(
            20,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
    fixture.response(20).await?;
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    let workspace = fixture.root.path().join("src");
    std::fs::create_dir_all(&workspace)?;
    std::fs::write(
        workspace.join("change.rs"),
        "pub fn requested_change() {}\n",
    )?;
    let reply = fixture.answer_acceptance(21).await?;
    let message = reply
        .pointer("/error/message")
        .and_then(Value::as_str)
        .unwrap_or_default();
    assert!(message.contains("already exists"), "{reply}");
    let current = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(current.state, crate::plan::PlanState::AwaitingReview);
    assert!(
        current.review_questions().is_none(),
        "an acceptance question stayed pending"
    );
    assert!(
        current
            .review
            .as_ref()
            .is_none_or(|review| review.execution_request.is_none()),
        "the refused acceptance stayed recorded"
    );
    fixture.finish().await
}

/// A check toggled on and off again during review leaves the submitted plan unchanged, so accepting
/// it creates no user revision and executes the revision the planner submitted.
#[tokio::test]
async fn acceptance_after_an_undone_check_toggle_creates_no_revision() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let config_path = fixture.root.path().join(".forge.json");
    let mut config: Value = serde_json::from_slice(&std::fs::read(&config_path)?)?;
    config["checks"]["build"] = json!({
        "trigger": {}, "action": { "category": "custom", "command": "must-not-run" }
    });
    std::fs::write(&config_path, serde_json::to_vec(&config)?)?;
    let mut plan = fixture.plan_in_review().await?;
    for (id, enabled) in [(20, true), (21, false)] {
        fixture
            .dispatch(
                id,
                "session.plan.check.update",
                json!({"plan_id":plan.id,"digest":plan.review_digest,
                    "expected_version":plan.document_version,"enabled":{"build":enabled}}),
            )
            .await?;
        let reply = fixture.response(id).await?;
        assert!(reply.get("error").is_none(), "{reply}");
        plan = fixture.plan()?.context("planning task disappeared")?;
    }
    fixture
        .dispatch(
            22,
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .await?;
    fixture.response(22).await?;
    fixture
        .settled("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.answer_acceptance(23).await?;
    fixture
        .settled("execution", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    let kinds = fixture
        .store()?
        .list_plan_lifecycle(&fixture.session_id)?
        .into_iter()
        .map(|event| (event.kind, event.model_revision))
        .collect::<Vec<_>>();
    assert!(
        !kinds
            .iter()
            .any(|(kind, _)| *kind == crate::plan::PlanLifecycleKind::RevisionCreated),
        "an undone check toggle created a revision: {kinds:?}"
    );
    assert!(
        kinds.contains(&(crate::plan::PlanLifecycleKind::Accepted, 1)),
        "{kinds:?}"
    );
    fixture.finish().await
}

/// Cancelling a plan hides its artifacts and removes its authoring directory, while the
/// submitted revisions that transcript rows open remain.
#[tokio::test]
async fn cancelled_plan_leaves_no_artifact_or_authoring_directory() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let authoring = files.authoring_directory(&fixture.session_id, &plan.id);
    assert!(authoring.exists());
    fixture.prompt(10, "/plan cancel").await?;
    fixture
        .settled("cancellation", |fixture| {
            Ok(fixture.controller.task_controller.current()?.is_none())
        })
        .await?;
    assert!(
        !authoring.exists(),
        "the cancelled plan kept its authoring directory"
    );
    assert!(std::path::Path::new(&plan.working_path).exists());
    let snapshot = fixture.controller.observer.snapshot()?;
    assert!(
        snapshot
            .artifact
            .iter()
            .all(|artifact| artifact.id != plan.id),
        "the cancelled plan stayed in the artifact list"
    );
    fixture.finish().await
}

/// Cancelling a plan while its draft turn runs settles that turn's exchange as cancelled before
/// the command answers, rather than parking it for an admission that never comes.
#[tokio::test]
async fn plan_cancel_during_a_draft_settles_its_exchange() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = ReviewFixture::open(true).await?;
    let control = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    fixture.prompt(2, "/yolo").await?;
    fixture.response(2).await?;
    let hold = control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
    fixture.prompt(3, "/plan Add an isolated callable").await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &fixture.session_id,
            MockResponseCommand::Wait { id: hold_id },
        ),
    )
    .await??;
    let drafting = fixture
        .store()?
        .latest_exchange(&fixture.session_id)?
        .context("the draft turn has no exchange")?
        .id;
    fixture.prompt(4, "/plan cancel").await?;
    let answer = fixture.response(4).await?;
    let exchange = fixture
        .store()?
        .load_exchange(&drafting)?
        .context("the draft exchange disappeared")?;
    assert_eq!(
        exchange.state(),
        crate::exchange::ExchangeState::Cancelled,
        "the draft exchange did not settle before the command answered: {answer}"
    );
    fixture.finish().await
}

/// Check setup grants check editing to its own exchange without changing the session
/// setting, so cancelling setup leaves the setting as the user chose it.
#[tokio::test]
async fn check_setup_leaves_the_check_editing_setting_unchanged() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    let fixture = ReviewFixture::open(false).await?;
    let control = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    let hold = control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
    fixture.prompt(3, "/plan Add an isolated callable").await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &fixture.session_id,
            MockResponseCommand::Wait {
                id: hold_id.clone(),
            },
        ),
    )
    .await??;
    assert_eq!(
        fixture.plan()?.map(|plan| plan.state),
        Some(crate::plan::PlanState::ConfiguringChecks)
    );
    let check_editing = || {
        fixture
            .controller
            .observer
            .check_editing
            .load(std::sync::atomic::Ordering::Acquire)
    };
    assert!(!check_editing(), "check setup changed the session setting");
    fixture.prompt(4, "/plan cancel").await?;
    fixture
        .settled("cancelled setup", |fixture| {
            Ok(fixture.controller.task_controller.current()?.is_none())
        })
        .await?;
    assert!(!check_editing());
    fixture.finish().await
}

/// A review edit that a plan discussion delivered retires with the revision the discussion led
/// to, so the next decision does not report it again. The discussion carries the edit and the
/// saved comment pending on the plan; the mock planner then rewrites its draft from scratch and
/// submits, as a model that writes whole files does.
#[tokio::test]
async fn edits_a_discussion_delivered_retire_with_the_revision_it_led_to() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let config_path = fixture.root.path().join(".forge.json");
    let mut config: Value = serde_json::from_slice(&std::fs::read(&config_path)?)?;
    config["checks"]["build"] = json!({
        "trigger": {}, "action": { "category": "custom", "command": "must-not-run" }
    });
    std::fs::write(&config_path, serde_json::to_vec(&config)?)?;
    let plan = fixture.plan_in_review().await?;
    fixture
        .dispatch(
            20,
            "session.plan.check.update",
            json!({"plan_id":plan.id,"digest":plan.review_digest,
                "expected_version":plan.document_version,"enabled":{"build":true}}),
        )
        .await?;
    let reply = fixture.response(20).await?;
    assert!(reply.get("error").is_none(), "{reply}");
    let plan = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(plan.manual_changes.len(), 1, "{:?}", plan.manual_changes);
    fixture.save_comment(&plan, "Say whether zero parts are rejected.")?;
    fixture.prompt(21, "Explain the design.").await?;
    fixture
        .settled("discussion revision", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.state == crate::plan::PlanState::AwaitingReview && plan.model_revision == 2
            }))
        })
        .await?;
    let plan = fixture.plan()?.context("planning task disappeared")?;
    assert!(
        plan.manual_changes.is_empty() && plan.unplaced_review_comments.is_empty(),
        "the delivered review stayed pending on the next revision: {:?} {:?}",
        plan.manual_changes,
        plan.unplaced_review_comments
    );
    fixture
        .dispatch(
            22,
            "session.plan.changes.request",
            json!({"plan_id":plan.id,"digest":plan.review_digest,"comment":"Tighten the design."}),
        )
        .await?;
    let reply = fixture.response(22).await?;
    assert!(reply.get("error").is_none(), "{reply}");
    fixture
        .settled("requested revision", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.state == crate::plan::PlanState::AwaitingReview && plan.model_revision == 3
            }))
        })
        .await?;
    let revision_turns = fixture
        .store()?
        .list_exchange(&fixture.session_id)?
        .into_iter()
        .map(|exchange| serde_json::to_string(&exchange.turn))
        .collect::<serde_json::Result<Vec<_>>>()?;
    let requested = revision_turns
        .iter()
        .find(|turn| turn.contains("Tighten the design."))
        .context("no turn carried the revision request")?;
    assert!(
        !requested.contains("set_check") && !requested.contains("zero parts"),
        "the revision request replayed review from an earlier revision: {requested}"
    );
    fixture.finish().await
}
