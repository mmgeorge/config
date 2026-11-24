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
        self.wait_until(what, true, condition).await
    }

    /// Wait until the condition holds while a held turn keeps the session busy.
    async fn settled_while_running(
        &self,
        what: &str,
        condition: impl Fn(&Self) -> Result<bool>,
    ) -> Result<()> {
        self.wait_until(what, false, condition).await
    }

    /// Wait until the condition holds, and also until no turn holds the session when `idle`.
    async fn wait_until(
        &self,
        what: &str,
        idle: bool,
        condition: impl Fn(&Self) -> Result<bool>,
    ) -> Result<()> {
        tokio::time::timeout(Duration::from_secs(8), async {
            loop {
                if (!idle || self.controller.coordinator.execution.available_permits() == 1)
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

    /// Count the editor notices that automatic acceptance stopped without submitting again.
    fn stopped_acceptance_notices(&self) -> usize {
        self.frame_list
            .lock()
            .unwrap()
            .iter()
            .filter(|frame| {
                frame["event"] == "provider_event"
                    && frame["payload"]["code"] == "automatic_acceptance_stopped"
            })
            .count()
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

    /// Count the times the exchange owner received the command `operation_id`, as the session
    /// trace records each submission. The trace writer appends on its own thread, so the file
    /// is read after a flush.
    fn command_receipts(&self, operation_id: &str) -> usize {
        self.trace_events("exchange.command.received")
            .iter()
            .filter(|record| record["payload"]["command_id"] == operation_id)
            .count()
    }

    /// Read the session trace records whose event name starts with `event_prefix`, oldest first.
    fn trace_events(&self, event_prefix: &str) -> Vec<Value> {
        self.controller
            .trace
            .flush()
            .expect("the session trace could not be flushed");
        let path = self
            .controller
            .trace
            .session_status(self.controller.observer.session_id())
            .path;
        std::fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .filter_map(|line| serde_json::from_str::<Value>(line).ok())
            .filter(|record| {
                record["event"]
                    .as_str()
                    .is_some_and(|event| event.starts_with(event_prefix))
            })
            .collect()
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
        let continue_id = self.answer_acceptance_questions(first_id).await?;
        self.continue_acceptance(continue_id).await?;
        self.response(continue_id).await
    }

    /// Answer every pending acceptance question with its first option, using request identities
    /// from `first_id`. Returns the next unused request identity.
    async fn answer_acceptance_questions(&self, first_id: u64) -> Result<u64> {
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
        Ok(id)
    }

    /// Submit the answered acceptance questions as request `id` without waiting for its reply.
    async fn continue_acceptance(&self, id: u64) -> Result<()> {
        let plan = self.plan()?.context("planning task disappeared")?;
        let questions = plan
            .review_questions()
            .context("acceptance questions disappeared")?
            .clone();
        self.dispatch(
            id,
            "session.question.continue",
            json!({"operation_id":format!("continue-{id}"),
                "target":{"owner":"plan_review","id":plan.id,"set_id":questions.question_set.id,"revision":questions.revision}}),
        )
        .await
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

    /// Roll the conversation back to before `exchange_id`, as `/undo` with Conversation only
    /// does, and return the response.
    async fn undo_conversation(&self, id: u64, exchange_id: &str) -> Result<Value> {
        let latest = self
            .store()?
            .latest_exchange(&self.session_id)?
            .context("the session has no exchange")?
            .id;
        self.dispatch(
            id,
            "session.command.apply",
            json!({"operation_id":format!("undo-{id}"),"action":"undo","rollback":{
                "scope":"conversation","exchange_id":exchange_id,"latest_exchange_id":latest}}),
        )
        .await?;
        self.response(id).await
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

/// Enabling automatic acceptance again after `/plan accept off` withdrew its questions is a new
/// acceptance: it asks the execution questions again, and answering them starts execution.
#[tokio::test]
async fn automatic_acceptance_enabled_again_asks_again() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture.plan_in_review().await?;
    let questions_pending = |fixture: &ReviewFixture| {
        Ok(fixture.plan()?.is_some_and(|plan| {
            plan.review_questions()
                .is_some_and(|questions| questions.has_pending_decisions())
        }))
    };
    fixture.prompt(10, "/plan accept on").await?;
    fixture
        .settled("automatic acceptance", questions_pending)
        .await?;
    fixture.prompt(11, "/plan accept off").await?;
    fixture
        .settled("withdrawn acceptance", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_none()))
        })
        .await?;
    fixture.prompt(12, "/plan accept on").await?;
    fixture
        .settled("second automatic acceptance", questions_pending)
        .await?;
    fixture.answer_acceptance(20).await?;
    fixture
        .settled("execution", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute)
        })
        .await?;
    fixture.assert_execution_started_once()?;
    assert_eq!(fixture.stopped_acceptance_notices(), 0);
    fixture.finish().await
}

/// An automatic acceptance whose operation the command journal already finished is not
/// submitted again and again: the journal can only replay the recorded reply, so recovery
/// submits it a bounded number of times and reports that it stopped.
///
/// A manual acceptance sent under the operation identity that automatic acceptance computes
/// reaches the state directly: its questions are withdrawn by `/plan accept off`, and the
/// following `/plan accept on` finds the plan eligible again under that finished identity.
#[tokio::test]
async fn journaled_automatic_acceptance_is_not_resubmitted_in_a_loop() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    fixture
        .controller
        .trace
        .configure_default(crate::trace::LogLevel::Trace)?;
    fixture.plan_in_review().await?;
    // The command `/plan accept on` will make eligible, computed from the task it will leave.
    let mut enabled = fixture.task()?.draft();
    enabled.auto_accept_plan = true;
    enabled.plan_acceptance_enablement += 1;
    let acceptance = crate::task::command::SessionCommand::approved_revision(
        &crate::task::Task::try_from(enabled)?,
    )?
    .context("an enabled acceptance would not be eligible")?;
    let mut request = acceptance.to_request(20)?;
    request.params["operation_id"] = json!(acceptance.operation_id);
    fixture
        .dispatch(20, &request.method, request.params.clone())
        .await?;
    fixture.response(20).await?;
    fixture
        .settled("manual acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    fixture.prompt(11, "/plan accept off").await?;
    fixture
        .settled("withdrawn acceptance", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_none()))
        })
        .await?;
    fixture.prompt(12, "/plan accept on").await?;
    fixture.response(12).await?;
    let reported = |fixture: &ReviewFixture| fixture.stopped_acceptance_notices() > 0;
    // Settle on either outcome, so a loop fails the count below instead of the wait.
    fixture
        .settled("reported stop", |fixture| {
            Ok(reported(fixture) || fixture.command_receipts(&acceptance.operation_id) > 2)
        })
        .await?;
    for _ in 0..200 {
        tokio::task::yield_now().await;
    }
    let submitted = fixture.command_receipts(&acceptance.operation_id);
    assert!(
        submitted <= 2,
        "the journaled acceptance was submitted {submitted} times"
    );
    assert!(
        reported(&fixture),
        "the stopped acceptance was not reported"
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
#[tokio::test(start_paused = true)]
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
    assert_eq!(fixture.stopped_acceptance_notices(), 0);
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

/// Submission refreshes stale captures and lets the planner retry in the same task.
#[tokio::test]
async fn submission_refreshes_changed_sources_before_resubmission() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let source = fixture.root.path().join("existing.rs");
    std::fs::write(&source, "pub fn existing() { configure_color(); }\n")?;
    files.authoring_capture(&fixture.session_id, &plan.id, "existing.rs")?;
    let proposal = files
        .authoring_directory(&fixture.session_id, &plan.id)
        .join("files/existing.rs");
    let proposed = "/// Existing callable.\npub fn existing();\n/// Add a callable.\npub fn added();\nAlgorithm\n  return true\n";
    std::fs::write(&proposal, proposed)?;
    std::fs::write(&source, "pub fn existing() {}\n")?;
    let config = fixture.root.path().join("settings.toml");
    std::fs::write(&config, "size = 1\n")?;
    files.authoring_capture(&fixture.session_id, &plan.id, "settings.toml")?;
    let deleted_proposal = files
        .authoring_directory(&fixture.session_id, &plan.id)
        .join("files/settings.toml");
    std::fs::remove_file(&deleted_proposal)?;
    std::fs::write(&config, "size = 2\n")?;
    let obsolete = fixture.root.path().join("obsolete.rs");
    std::fs::write(&obsolete, "pub fn obsolete() {}\n")?;
    files.authoring_capture(&fixture.session_id, &plan.id, "obsolete.rs")?;
    std::fs::remove_file(proposal.with_file_name("obsolete.rs"))?;
    std::fs::remove_file(obsolete)?;
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
                    steps: vec![
                        TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan.id}),
                        },
                        TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan.id}),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture.dispatch(20, "session.command.apply", json!({"action":"request_changes","operation_id":"resubmit","plan_id":plan.id,"comment":"Resubmit the draft."})).await?;
    fixture
        .settled("resubmission", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| plan.model_revision == 2))
        })
        .await?;
    let design = files
        .read_submitted_document(&fixture.session_id, &plan.id, 2)?
        .design
        .unwrap();
    design.check_workspace(fixture.root.path())?;
    assert_eq!(design.proposed_source["existing.rs"], proposed);
    assert!(!deleted_proposal.exists());
    assert!(!design.proposed_source.contains_key("settings.toml"));
    assert!(!design.baseline.contains_key("obsolete.rs"));
    let transcript = serde_json::to_string(&fixture.store()?.list_exchange(&fixture.session_id)?)?;
    for message in [
        "Draft not submitted",
        "No changes to captured declarations",
        "-size = 1",
        "+size = 2",
        "-pub fn obsolete();",
        "resubmit it as is",
    ] {
        assert!(
            transcript.contains(message),
            "missing submission feedback: {message}"
        );
    }
    fixture.finish().await
}

/// A package omitted from the plan leaves its dependencies unverified without recursively
/// qualifying the same missing name through a wildcard in the proposed crate root.
#[tokio::test]
async fn submission_with_an_uncaptured_package_and_wildcard_reaches_review() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let directory = files.authoring_directory(&fixture.session_id, &plan.id);
    let write = |path: &str, text: &str| TestPromptStep::FileWrite {
        path: directory.join(path).to_string_lossy().into_owned(),
        text: text.into(),
    };
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
                    steps: vec![
                        write("files/Cargo.toml", "[workspace]\nmembers = []\n"),
                        write(
                            "files/engine/src/lib.rs",
                            "pub use bridge::AppDefinition;\npub use runtime::{executor::*, fetch::*, macros::*};\n",
                        ),
                        TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan.id}),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes",
        "operation_id":"wildcard-revision","plan_id":plan.id,"comment":"Expose the bridge type."}),
        )
        .await?;
    fixture
        .settled("wildcard submission", |fixture| {
            Ok(fixture.plan()?.is_some_and(|plan| {
                plan.state == crate::plan::PlanState::AwaitingReview && plan.model_revision == 2
            }))
        })
        .await?;
    let document = files.read_submitted_document(&fixture.session_id, &plan.id, 2)?;
    let design = document.design.context("missing declaration design")?;
    let report = design
        .parsed()?
        .validation
        .as_ref()
        .context("missing validation report")?;
    let lookup = report
        .diagnostic
        .iter()
        .find(|entry| entry.reference == "bridge::AppDefinition")
        .context("missing warning for the uncaptured dependency")?;
    assert!(
        !lookup.error && lookup.reason.contains("unavailable"),
        "{lookup:?}"
    );
    fixture.finish().await
}

/// The live transcript draws `Plan submitted` as soon as the submission is committed, while the
/// turn that submitted it still runs, so it shows the same rows a restored transcript shows.
#[tokio::test]
async fn live_transcript_shows_a_submission_before_its_turn_settles() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let fixture = ReviewFixture::open(true).await?;
    let transcript = forge_buffer::identity::DocumentId("transcript:live-submission".into());
    fixture.controller.presentation.lock().unwrap().open(
        transcript.clone(),
        forge_buffer::identity::ViewId("view:live-submission".into()),
        forge_buffer::width::WidthProfile::default(),
    )?;
    let plan = fixture.plan_in_review().await?;
    let directory =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?
            .authoring_directory(&fixture.session_id, &plan.id);
    let control = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![
                        TestPromptStep::FileWrite {
                            path: directory.join("docs/overview.md").to_string_lossy().into_owned(),
                            text: "# Implement the requested change\n\n## Objective\nRename the callable.\n\n## Background\nThe review asked for a clearer name.\n\n## Design\nApply the rename at its owning boundary.\n\n## Manual checks\n- Confirm the renamed interface behaves as designed.\n".into(),
                        },
                        TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan.id}),
                        },
                        TestPromptStep::Barrier {
                            id: TestHoldId::new("revision-submitted")?,
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Submitted the revised plan.".into(),
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Rename the callable"}),
        )
        .await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &fixture.session_id,
            MockResponseCommand::Wait {
                id: "revision-submitted".into(),
            },
        ),
    )
    .await??;
    let submitted = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(
        submitted.model_revision, 2,
        "the revision was not submitted"
    );
    let rows = fixture
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
            &fixture.session_id,
            MockResponseCommand::Release {
                id: "revision-submitted".into(),
            },
        )
        .await?;
    assert!(
        rows.iter()
            .any(|row| row.contains("Plan submitted") && row.contains("revision 2")),
        "the live transcript omitted the submission: {rows:#?}"
    );
    fixture
        .settled("revision", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.state == crate::plan::PlanState::AwaitingReview))
        })
        .await?;
    fixture.finish().await
}

/// An acceptance that reaches the owner while the revision turn that submitted its revision has
/// not settled waits for that turn and then runs. The mock provider holds the revision turn after
/// its terminal event, and `/plan accept on` and the answered acceptance questions arrive in that
/// window. The continuation is answered only once execution starts, and no acceptance finishes
/// as failed, which would leave every later automatic recovery finding a finished command and
/// the revision stranded in review.
#[tokio::test(start_paused = true)]
async fn acceptance_during_an_unsettled_revision_turn_runs_after_it_settles() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let fixture = ReviewFixture::open(true).await?;
    fixture
        .controller
        .trace
        .configure_default(crate::trace::LogLevel::Trace)?;
    let plan = fixture.plan_in_review().await?;
    let directory =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?
            .authoring_directory(&fixture.session_id, &plan.id);
    let control = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![
                        TestPromptStep::FileWrite {
                            path: directory.join("docs/overview.md").to_string_lossy().into_owned(),
                            text: "# Implement the requested change\n\n## Objective\nRename the callable.\n\n## Background\nThe review asked for a clearer name.\n\n## Design\nApply the rename at its owning boundary.\n\n## Manual checks\n- Confirm the renamed interface behaves as designed.\n".into(),
                        },
                        TestPromptStep::ToolCall {
                            name: "plan_submit".into(),
                            arguments: json!({"plan_id":plan.id}),
                        },
                        TestPromptStep::Message {
                            content: TestMessageContent::Response,
                            text: "Submitted the revised plan.".into(),
                        },
                        TestPromptStep::CollectionBarrier {
                            id: TestHoldId::new("revision-unsettled")?,
                        },
                    ],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Rename the callable"}),
        )
        .await?;
    control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Wait {
                id: "revision-unsettled".into(),
            },
        )
        .await?;
    let revised = fixture.plan()?.context("planning task disappeared")?;
    assert_eq!(revised.model_revision, 2, "the revision was not submitted");
    fixture.prompt(21, "/plan accept on").await?;
    fixture.response(21).await?;
    fixture
        .settled_while_running("acceptance questions", |fixture| {
            Ok(fixture
                .plan()?
                .is_some_and(|plan| plan.review_questions().is_some()))
        })
        .await?;
    let continue_id = fixture.answer_acceptance_questions(22).await?;
    // The continuation's reply waits for the acceptance it starts, so the turn is released
    // while that request is still being answered.
    let operation_id = format!("continue-{continue_id}");
    let settle_revision = async {
        fixture
            .settled_while_running("acceptance reaching the owner", |fixture| {
                Ok(fixture
                    .trace_events("plan.acceptance_")
                    .iter()
                    .any(|record| record["payload"]["command_id"] == operation_id.as_str()))
            })
            .await?;
        assert_ne!(
            fixture.task()?.kind(),
            crate::task::TaskKind::Execute,
            "execution started before the revision turn settled"
        );
        control
            .dispatch(
                &fixture.session_id,
                MockResponseCommand::Release {
                    id: "revision-unsettled".into(),
                },
            )
            .await?;
        Ok::<(), anyhow::Error>(())
    };
    let (continuation, settlement) =
        tokio::join!(fixture.continue_acceptance(continue_id), settle_revision);
    continuation?;
    settlement?;
    let continued = fixture.response(continue_id).await?;
    assert!(
        continued.get("error").is_none(),
        "the acceptance failed while the revision turn settled: {continued}"
    );
    fixture
        .settled("accepted revision", |fixture| {
            Ok(fixture.task()?.kind() == crate::task::TaskKind::Execute
                || fixture.stopped_acceptance_notices() > 0)
        })
        .await?;
    assert_eq!(fixture.stopped_acceptance_notices(), 0);
    assert_eq!(
        fixture.task()?.kind(),
        crate::task::TaskKind::Execute,
        "the revision was never accepted"
    );
    fixture.assert_execution_started_once()?;
    fixture.finish().await
}

/// Rolling back the exchange that started a plan removes the plan from the session's artifacts
/// once the plan was accepted, as it does while the plan is in review: the winbar count and the
/// artifact picker no longer offer a plan whose start the transcript no longer shows.
#[tokio::test]
async fn rolling_back_an_accepted_plan_start_removes_its_artifact() -> Result<()> {
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    fixture.accept_and_answer(20).await?;
    fixture
        .settled("accepted plan execution settled", |fixture| {
            // Acceptance starts another exchange. Its history must settle before undo captures
            // the latest exchange and the rollback authority.
            let store = fixture.store()?;
            Ok(store
                .load_plan(&plan.id)?
                .is_some_and(|plan| plan.state == crate::plan::PlanState::Accepted)
                && fixture.entered_execution_rows()? == 1
                && store
                    .latest_exchange(&fixture.session_id)?
                    .is_some_and(|exchange| exchange.ordinal > 1 && exchange.is_finished()))
        })
        .await?;
    let start = fixture
        .store()?
        .list_exchange(&fixture.session_id)?
        .into_iter()
        .next()
        .context("the plan start exchange is missing")?;
    let response = fixture.undo_conversation(30, &start.id).await?;
    assert!(response.get("error").is_none(), "{response}");
    let snapshot = fixture.controller.observer.snapshot()?;
    assert!(
        snapshot
            .artifact
            .iter()
            .all(|artifact| artifact.id != plan.id),
        "the rolled-back plan stayed in the artifact list"
    );
    fixture.finish().await
}

/// Rolling back a discussion exchange while the plan waits in review, with nothing running,
/// leaves the plan in review as it was: a plan waiting for the user would not continue by itself,
/// so the rollback has nothing to pause.
#[tokio::test]
async fn rolling_back_a_discussion_keeps_the_plan_in_review() -> Result<()> {
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
    let response = fixture.undo_conversation(20, &discussion.id).await?;
    assert!(response.get("error").is_none(), "{response}");
    assert_eq!(fixture.task()?.status(), crate::task::TaskStatus::Waiting);
    let current = fixture.plan()?.context("the plan disappeared")?;
    assert_eq!(current.id, plan.id);
    assert_eq!(current.state, crate::plan::PlanState::AwaitingReview);
    assert_eq!(current.model_revision, plan.model_revision);
    fixture.finish().await
}

/// A `/compact` that stops a revision turn runs that turn again with the review it was given:
/// the change request, the saved comments, and the edits, together with the notice that says
/// why the turn runs again.
///
/// The turn that ran again once received only "Continue the interrupted request…" followed by
/// the task's first prompt, so the model revised without the review and resubmitted the plan
/// unchanged.
#[tokio::test]
async fn compaction_during_a_revision_runs_it_again_with_the_review() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let control = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    control
        .dispatch(
            &fixture.session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance: TestInputAcceptance::BeforeResponse,
                    steps: vec![TestPromptStep::Barrier {
                        id: TestHoldId::new("revision-before-compaction")?,
                    }],
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    fixture
        .dispatch(
            20,
            "session.command.apply",
            json!({"action":"request_changes","operation_id":"revise","plan_id":plan.id,"comment":"Name the leftover rule in the summary."}),
        )
        .await?;
    tokio::time::timeout(
        Duration::from_secs(2),
        control.dispatch(
            &fixture.session_id,
            MockResponseCommand::Wait {
                id: "revision-before-compaction".into(),
            },
        ),
    )
    .await??;
    fixture
        .dispatch(
            21,
            "session.command.apply",
            json!({"operation_id":"compact-revision","action":"compact"}),
        )
        .await?;
    let response = fixture.response(21).await?;
    assert!(response.get("error").is_none(), "{response}");
    fixture
        .settled("the revision that ran again", |fixture| {
            Ok(fixture.submitted_turns()?.len() > 2)
        })
        .await?;
    let turns = fixture.submitted_turns()?;
    let review = turns
        .iter()
        .filter(|turn| turn["kind"]["kind"] == "plan_review")
        .collect::<Vec<_>>();
    assert_eq!(
        review.len(),
        2,
        "the turn that ran again lost its review: {turns:#?}"
    );
    let stopped = review[0];
    let rerun = review[1];
    assert_eq!(rerun["kind"], stopped["kind"], "{turns:#?}");
    assert_eq!(
        rerun["kind"]["review"]["feedback"]["overall"],
        "Name the leftover rule in the summary."
    );
    assert!(
        rerun["notices"].as_array().is_some_and(|notices| notices
            .iter()
            .any(|notice| notice["kind"] == "turn_restarted")),
        "the turn that ran again does not say why: {rerun:#}"
    );
    fixture.finish().await
}

impl ReviewFixture {
    /// Read the typed input of every turn the session submitted, in submission order.
    fn submitted_turns(&self) -> Result<Vec<Value>> {
        let journal = rusqlite::Connection::open_with_flags(
            crate::storage::state::directory(&self.controller.data_root)
                .join("provider-submissions.sqlite3"),
            rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        let mut statement = journal.prepare(
            "SELECT json_extract(payload,'$.request.input.turn') FROM submission \
             WHERE json_extract(payload,'$.request.harness_session_id')=?1 ORDER BY rowid",
        )?;
        let turns = statement
            .query_map([&self.session_id], |row| row.get::<_, String>(0))?
            .map(|turn| Ok(serde_json::from_str(&turn?)?))
            .collect::<Result<Vec<Value>>>()?;
        Ok(turns)
    }
}

/// Unfold changed test declarations through the same file-body request the editor sends.
#[tokio::test]
async fn unfolded_rust_tests_follow_declaration_visibility() -> Result<()> {
    use crate::buffer::session::PresentationRequest;
    use crate::plan::review_document::ReadOnlyPlanReview;
    use crate::plan::review_files::ReviewFiles;
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::patch::BufferPatch;

    let fixture = ReviewFixture::open(true).await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let path = "tests/editor_ui_test.rs";
    let declaration = "#[testing::test]\nasync fn private_case();\n#[test]\npub fn public_case();\n#[cfg(test)]\nmod tests {\n  pub fn nested_case();\n}\n";
    let mut plan = crate::plan::test_fixture("review", "Review the editor tests.");
    let mut design =
        crate::plan::DeclarationDesign::scratch([(path.into(), declaration.into())].into())?;
    design.document.objective = "Review the editor tests.".into();
    design.document.background = "The editor has a test harness.".into();
    design.document.requirements = vec!["Exercise each visibility.".into()];
    design.document.design = "Add editor test cases.".into();
    plan.design = Some(design);
    files.write_working_document(&fixture.session_id, "review", &plan)?;
    let (_, _, digest) = files.submit_document_revision(&fixture.session_id, "review", 1, 1)?;

    for public_only in [false, true] {
        let mut source =
            files.capture_review_source(&fixture.session_id, "review", Some(1), &digest)?;
        source.public_only = public_only;
        source.files = Some(ReviewFiles::new(
            source.document.design.as_ref().unwrap(),
            &source.rendered,
            &Default::default(),
        )?);
        let saved_source_digest = source.saved_digest.clone();
        let document = DocumentId(format!("review-{public_only}"));
        let review = ReadOnlyPlanReview::new(
            document.clone(),
            ViewId("plan".into()),
            source,
            Default::default(),
            None,
        )?;
        let store = &fixture.controller.plan_review;
        store.insert(review, store.admit(document.clone())?)?;
        let result = fixture
            .service
            .document(
                Some(fixture.session_id.clone()),
                PresentationRequest::PlanBody {
                    document,
                    saved_source_digest,
                    file: format!("plan:design:file:{}", crate::plan::digest(path.as_bytes())),
                },
                &fixture.sink,
            )
            .await?;
        let patch: BufferPatch = serde_json::from_value(result["patch"].clone())?;
        let body = patch
            .text_edit
            .iter()
            .flat_map(|edit| edit.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        for (name, visible) in [
            ("public_case", true),
            ("private_case", !public_only),
            ("nested_case", !public_only),
        ] {
            assert_eq!(
                body.contains(name),
                visible,
                "public_only={public_only}, {name}: {body}"
            );
        }
    }
    fixture.finish().await
}

/// Review a behavior addition alongside a real change of signature and a removed callable.
#[tokio::test]
async fn behavior_only_edits_show_one_modified_signature() -> Result<()> {
    use crate::plan::review_document::ReadOnlyPlanReview;
    use crate::plan::{DeclarationDesign, DeclarationFile, FunctionBehavior, FunctionBody};
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::patch::BufferSnapshot;

    let fixture = ReviewFixture::open(true).await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let path = "app.rs";
    let old = "pub fn app() -> App;\npub fn changed() -> u32;\npub fn removed();\n";
    std::fs::write(fixture.root.path().join(path), old)?;
    let new = "pub fn app() -> App;\npub fn changed() -> u64;\n";
    let mut design = DeclarationDesign::default();
    design.baseline.insert(
        path.into(),
        DeclarationFile::new(path, old.into(), crate::plan::digest(old.as_bytes()), &[])?,
    );
    design.parsed_insert(
        path,
        new.into(),
        ["app", "changed"]
            .map(|owner| FunctionBody {
                owner: owner.into(),
                behavior: Some(FunctionBehavior::Change("Install the plugin.".into())),
            })
            .into(),
    )?;
    design.document.objective = "Update the application.".into();
    design.document.requirements = vec!["Install the plugin.".into()];
    design.document.background = "The application has an existing entry point.".into();
    design.document.design = "Update its behavior and change the counter type.".into();
    let mut plan = crate::plan::test_fixture("review", "Update the application.");
    plan.design = Some(design);
    files.write_working_document(&fixture.session_id, "review", &plan)?;
    let (_, _, digest) = files.submit_document_revision(&fixture.session_id, "review", 1, 1)?;
    let mut source =
        files.capture_review_source(&fixture.session_id, "review", Some(1), &digest)?;
    for name in ["app", "changed"] {
        let fold = format!(
            "plan:calls:{}:proposed:{}",
            crate::plan::digest(path.as_bytes()),
            crate::plan::digest(name.as_bytes())
        );
        source
            .collapse
            .insert(forge_buffer::identity::FoldId(fold), false);
    }
    let id = DocumentId("review".into());
    let review = ReadOnlyPlanReview::new(
        id.clone(),
        ViewId("plan".into()),
        source,
        Default::default(),
        None,
    )?;
    let store = &fixture.controller.plan_review;
    let result = store.insert(review, store.admit(id)?)?;
    let snapshot: BufferSnapshot = serde_json::from_value(result["snapshot"].clone())?;
    let visible = |text| {
        snapshot.block.iter().find(|block| {
            block.text.row(0) == Some(text)
                && !block.metadata.conceal.iter().any(|conceal| conceal.line)
        })
    };
    assert!(
        visible("pub fn app() -> App;").is_none(),
        "duplicate baseline signature"
    );
    let app = visible("pub fn app() -> App {").context("missing modified heading")?;
    let gutter = app
        .metadata
        .gutter
        .iter()
        .flat_map(|mark| &mark.chunk)
        .map(|chunk| chunk.text.as_str())
        .collect::<String>();
    assert_eq!(
        gutter.split_whitespace().collect::<Vec<_>>(),
        ["1", "1", "~"]
    );
    assert!(visible("  Change").is_some());
    assert!(visible("pub fn changed() -> u32;").is_some());
    assert!(visible("pub fn changed() -> u64 {").is_some());
    assert!(visible("pub fn removed();").is_some());
    let baseline = snapshot
        .block
        .iter()
        .find(|block| block.text.row(0) == Some("pub fn app() -> App;"))
        .context("missing baseline comment anchor")?;
    forge_recorded_fixtures::assert_matches_recording(
        "plan-review-behavior-signature.json",
        &json!({
            "baseline": baseline.metadata.conceal,
            "modified": app.metadata.gutter,
        }),
    );
    fixture.finish().await
}

/// Follow an intervening comment on one chain and start another chain at the same plan line.
#[tokio::test]
async fn review_asks_reuse_the_comment_chain_session() -> Result<()> {
    use crate::plan::review_document::ReadOnlyPlanReview;
    use forge_buffer::identity::{DocumentId, ViewId};

    let fixture = ReviewFixture::open(true).await?;
    let files =
        crate::plan::PlanFileStore::new(&fixture.controller.data_root, fixture.root.path())?;
    let mut plan = crate::plan::test_fixture("review", "Explain ownership.");
    let mut design = crate::plan::DeclarationDesign::scratch(
        [(
            "owner.rs".into(),
            "/// Own state.\npub struct Owner;\n".into(),
        )]
        .into(),
    )?;
    design.document.objective = "Explain ownership.".into();
    design.document.background = "The plan introduces an owner.".into();
    design.document.requirements = vec!["Preserve state.".into()];
    design.document.design = "Add an owner.".into();
    plan.design = Some(design);
    files.write_working_document(&fixture.session_id, "review", &plan)?;
    let (_, _, digest) = files.submit_document_revision(&fixture.session_id, "review", 1, 1)?;
    let store = &fixture.controller.plan_review;
    let open_review = |id: &DocumentId| -> Result<_> {
        let source =
            files.capture_review_source(&fixture.session_id, "review", Some(1), &digest)?;
        let saved_digest = source.saved_digest.clone();
        let line = source.rendered.navigation.anchor[0].line;
        let view = ViewId("plan".into());
        let document =
            ReadOnlyPlanReview::new(id.clone(), view.clone(), source, Default::default(), None)?;
        let snapshot = document.snapshot();
        let block = &snapshot.block[0];
        let target = &block.metadata.target[0];
        let review = json!({"document":id,"revision":snapshot.revision,"view":view,"sequence":1,
            "action":"session.plan.question.answer","block":block.id,"position":target.range.start,"target":target.id});
        store.insert(document, store.admit(id.clone())?)?;
        Ok((review, saved_digest, line))
    };
    let id = DocumentId("review".into());
    let (mut review, saved_digest, line) = open_review(&id)?;
    let mut first_binding = None;
    let mut annotations = Vec::new();
    for (index, (name, parent, kind)) in [
        ("first", None, "question"),
        ("feedback", Some("first"), "comment"),
        ("follow-up", Some("feedback"), "question"),
        ("inherit", Some("follow-up"), "question"),
        ("independent", None, "question"),
    ]
    .into_iter()
    .enumerate()
    {
        let configuration = match name {
            "first" => Some(json!({"model":"mock-model","effort":"low"})),
            "follow-up" => Some(
                json!({"ask_model":{"model":"mock-alternate","effort":"high","service_tier":"fast"}}),
            ),
            "inherit" => Some(json!({"ask_model":{}, "effort":"medium"})),
            _ => None,
        };
        if let Some(configuration) = configuration {
            let request = 20 + index as u64;
            fixture
                .dispatch(request, "session.configure", configuration)
                .await?;
            let response = fixture.response(request).await?;
            assert_eq!(response["result"]["state"], "completed", "{response}");
        }
        if name == "follow-up" {
            store.close(&id).await?;
            (review, _, _) = open_review(&DocumentId("reopened".into()))?;
        }
        annotations.push(json!({"id":name, "parent_id":parent, "kind":kind,
            "source":{"start_line":line,"end_line":line,"body":name}}));
        let request = 10 + index as u64;
        fixture.dispatch(request, "session.plan.question.answer", json!({"review":review,
            "draft_source_digest":saved_digest,"draft_annotation":annotations,"question_ids":[name]})).await?;
        let response = fixture.response(request).await?;
        annotations = serde_json::from_value(response["result"]["annotation"].clone())
            .with_context(|| format!("missing answered annotations: {response}"))?;
        assert_eq!(
            annotations.last().unwrap()["reply"]["body"].is_string(),
            kind == "question"
        );
        let registry = fixture.service.registry().await?;
        let children: Vec<_> = registry
            .controller_by_id
            .read()
            .await
            .iter()
            .filter(|(child, _)| **child != fixture.session_id)
            .map(|(child, _)| child.clone())
            .collect();
        assert_eq!(
            children.len(),
            if name == "independent" { 2 } else { 1 },
            "{name}"
        );
        if matches!(name, "first" | "follow-up" | "inherit") {
            use crate::session::SessionStore;
            let history = fixture.store()?;
            let session = history
                .load_session(&children[0])?
                .context("missing review session")?;
            let exchange = history
                .latest_exchange(&children[0])?
                .context("missing review exchange")?;
            let observed_model = exchange
                .turn
                .last()
                .and_then(|turn| turn.resolved_model())
                .context("mock provider did not report its invocation model")?;
            let (model, effort, tier) = match name {
                "follow-up" => ("mock-alternate", "high", crate::provider::ServiceTier::Fast),
                "inherit" => (
                    "mock-model",
                    "medium",
                    crate::provider::ServiceTier::Standard,
                ),
                _ => ("mock-model", "low", crate::provider::ServiceTier::Standard),
            };
            assert_eq!(observed_model.model_id.as_str(), model);
            assert_eq!(
                (session.effort.as_str(), session.service_tier),
                (effort, tier)
            );
            let binding = session
                .provider_session_id
                .context("missing native conversation")?;
            if name == "first" {
                first_binding = Some(binding);
            } else {
                assert_eq!(Some(binding), first_binding);
            }
        }
        if name == "follow-up" {
            assert_eq!(fixture.store()?.list_exchange(&children[0])?.len(), 2);
        }
    }
    fixture.finish().await
}

/// Read file review progress from the opening snapshot after replacing the session owner.
#[tokio::test]
async fn reopened_revision_restores_viewed_files_after_restart() -> Result<()> {
    use crate::buffer::session::PresentationRequest;
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::patch::BufferSnapshot;

    let mut fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let open = |name: &str, revision| PresentationRequest::PlanOpen {
        document: DocumentId(name.into()),
        view: ViewId("plan".into()),
        plan_id: plan.id.clone(),
        digest: plan.review_digest.clone().unwrap(),
        revision,
        width: Default::default(),
        expanded_files: Default::default(),
        focused_annotation: None,
        saved_source_digest: None,
    };
    for (index, (name, revision, expected, action)) in [
        ("first", None, false, Some("viewed")),
        ("history", Some(1), true, Some("unviewed")),
        ("reopened", None, false, None),
    ]
    .into_iter()
    .enumerate()
    {
        if index == 1 {
            fixture.service.shutdown(Duration::from_secs(2)).await?;
            drop(fixture.controller);
            fixture.service = service();
            let mut options = initialize(&fixture.root, "mock");
            options.session_id = Some(fixture.session_id.clone());
            fixture
                .service
                .open_session(2, options)
                .await?
                .result()
                .context("session restart failed")?;
            fixture.controller = fixture
                .service
                .registry()
                .await?
                .resolve(&fixture.session_id)
                .await?;
        }
        let opened = fixture
            .service
            .document(
                Some(fixture.session_id.clone()),
                open(name, revision),
                &fixture.sink,
            )
            .await?;
        let snapshot: BufferSnapshot = serde_json::from_value(opened["snapshot"].clone())?;
        let file = snapshot
            .block
            .iter()
            .position(|block| block.id.0.starts_with("plan:design:file:"))
            .context("missing changed file")?;
        let viewed = snapshot
            .block
            .iter()
            .position(|block| block.id.0 == "plan:section:changes:viewed")
            .context("missing Viewed Changes")?;
        assert_eq!(file > viewed, expected, "{name}");
        if let Some(action) = action {
            let heading = &snapshot.block[file];
            let target = &heading.metadata.target[0];
            let input = serde_json::from_value(
                json!({"document":name,"view":"plan","revision":snapshot.revision,
                "sequence":1,"action":action,"block":heading.id,"position":target.range.start,"target":target.id}),
            )?;
            fixture
                .service
                .document(
                    Some(fixture.session_id.clone()),
                    PresentationRequest::PlanAction { input },
                    &fixture.sink,
                )
                .await?;
        }
        fixture
            .service
            .document(
                Some(fixture.session_id.clone()),
                PresentationRequest::PlanClose {
                    document: DocumentId(name.into()),
                },
                &fixture.sink,
            )
            .await?;
    }
    fixture.finish().await
}

/// Open an editable review and observe its idle provider binding before any question is sent.
#[tokio::test]
async fn plan_tab_warms_an_idle_answer_fork() -> Result<()> {
    use crate::buffer::session::PresentationRequest;
    use forge_buffer::identity::{DocumentId, ViewId};
    let fixture = ReviewFixture::open(true).await?;
    let plan = fixture.plan_in_review().await?;
    let mut settings = crate::settings::SettingsStore::open(&fixture.controller.data_root)?;
    let workspace = fixture.root.path().to_string_lossy();
    let revision = settings
        .get(&workspace, &fixture.session_id, "mock")?
        .revision;
    settings.save_provider(
        revision,
        "mock",
        &json!({"ask_model":{"model":"mock-alternate","effort":"low"}}),
    )?;
    let document = DocumentId("warm-review".into());
    let view = ViewId("review".into());
    let opened = fixture
        .service
        .document(
            Some(fixture.session_id.clone()),
            PresentationRequest::PlanOpen {
                document: document.clone(),
                view: view.clone(),
                plan_id: plan.id.clone(),
                digest: plan.review_digest.clone().unwrap(),
                revision: None,
                width: Default::default(),
                expanded_files: Default::default(),
                focused_annotation: None,
                saved_source_digest: None,
            },
            &fixture.sink,
        )
        .await?;
    let registry = fixture.service.registry().await?;
    let ready_child = |excluded: Option<String>| {
        let registry = registry.clone();
        let source = fixture.session_id.clone();
        async move {
            tokio::time::timeout(Duration::from_secs(2), async {
                loop {
                    if let Some(child) = registry.review_standby.lock().await.ready_child(&source)
                        && Some(&child.observer.session_id().to_owned()) != excluded.as_ref()
                    {
                        return child.observer.session_id().to_owned();
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .context("review has no ready standby")
        }
    };
    let first = ready_child(None).await?;
    assert!(fixture.store()?.list_exchange(&first)?.is_empty());
    let snapshot: forge_buffer::patch::BufferSnapshot =
        serde_json::from_value(opened["snapshot"].clone())?;
    let block = &snapshot.block[0];
    let target = &block.metadata.target[0];
    let input = json!({"document":document,"view":view,"revision":snapshot.revision,"sequence":1,
        "action":"session.plan.question.answer","block":block.id,"position":target.range.start,"target":target.id});
    fixture.dispatch(44,"session.plan.question.answer",json!({"review":input,
        "draft_source_digest":opened["saved_source_digest"],"draft_annotation":[{"id":"first","kind":"question",
        "source":{"start_line":1,"end_line":1,"body":"Explain the objective."}}]})).await?;
    assert_eq!(fixture.store()?.list_exchange(&first)?.len(), 1);
    let spare = ready_child(Some(first.clone())).await?;
    assert!(fixture.store()?.list_exchange(&spare)?.is_empty());
    // Only thinking effort changes. This external save sends no configure event to this host.
    let revision = settings
        .get(&registry.initialize.workspace, &fixture.session_id, "mock")?
        .revision;
    settings.save_provider(
        revision,
        "mock",
        &json!({"ask_model":{"model":"mock-alternate","effort":"high"}}),
    )?;
    let mut annotations = fixture.response(44).await?["result"]["annotation"]
        .as_array()
        .unwrap()
        .clone();
    annotations.push(json!({"id":"second","kind":"question","source":{"start_line":1,"end_line":1,"body":"How does this work?"}}));
    fixture
        .dispatch(
            45,
            "session.plan.question.answer",
            json!({"review":input,"draft_source_digest":opened["saved_source_digest"],
        "draft_annotation":annotations,"question_ids":["second"]}),
        )
        .await?;
    let replacement = ready_child(Some(spare.clone())).await?;
    assert!(fixture.store()?.list_exchange(&spare)?.is_empty());
    let controllers = registry.controller_by_id.read().await.clone();
    let mut answering = Vec::new();
    for child in controllers.keys().filter(|id| *id != &fixture.session_id) {
        if let Some(exchange) = fixture.store()?.latest_exchange(child)? {
            answering.push(child.clone());
            if child != &first {
                let model = exchange.turn.last().unwrap().resolved_model().unwrap();
                assert_eq!(model.model_id.as_str(), "mock-alternate");
                assert_ne!(child, &replacement);
            }
        }
    }
    assert_eq!(answering.len(), 2);
    annotations.push(
        json!({"id":"followup","parent_id":"first","kind":"question",
        "source":{"start_line":1,"end_line":1,"body":"Explain the first answer."}}),
    );
    fixture
        .dispatch(
            46,
            "session.plan.question.answer",
            json!({"review":input,"draft_source_digest":opened["saved_source_digest"],
        "draft_annotation":annotations,"question_ids":["followup"]}),
        )
        .await?;
    assert_eq!(fixture.store()?.list_exchange(&first)?.len(), 2);
    assert_eq!(
        registry
            .review_standby
            .lock()
            .await
            .ready_child(&fixture.session_id)
            .unwrap()
            .observer
            .session_id(),
        replacement
    );
    fixture
        .service
        .document(
            Some(fixture.session_id.clone()),
            PresentationRequest::PlanClose { document },
            &fixture.sink,
        )
        .await?;
    super::super::review_standby::shutdown(&registry).await?;
    assert!(
        !registry
            .controller_by_id
            .read()
            .await
            .contains_key(&replacement)
    );
    assert!(!registry.controller_by_id.read().await.contains_key(&spare));
    assert!(registry.controller_by_id.read().await.contains_key(&first));
    fixture.finish().await
}

/// A request admitted before its standby worker is polled takes the ordinary fork path.
#[tokio::test]
async fn opening_standby_does_not_delay_a_new_chain() -> Result<()> {
    use super::super::review_standby;
    use forge_buffer::identity::DocumentId;
    let fixture = ReviewFixture::open(true).await?;
    let registry = fixture.service.registry().await?;
    let (selection, _) = fixture.controller.broker.lock().await.review_settings()?;
    let document = DocumentId("pending-review".into());
    review_standby::activate(
        &registry,
        &fixture.controller,
        document.clone(),
        &fixture.sink,
    )
    .await;
    // This current-thread test has not yielded to the spawned preparation worker.
    assert!(
        registry
            .review_standby
            .lock()
            .await
            .ready_child(&fixture.session_id)
            .is_none()
    );
    let chain = uuid::Uuid::new_v4();
    let child = review_standby::claim(
        &registry,
        &fixture.controller,
        chain,
        &selection,
        &fixture.sink,
    )
    .await?;
    assert_eq!(child.observer.session_id(), chain.to_string());
    registry
        .review_standby
        .lock()
        .await
        .close_view(&fixture.session_id, &document);
    review_standby::shutdown(&registry).await?;
    fixture.finish().await
}
