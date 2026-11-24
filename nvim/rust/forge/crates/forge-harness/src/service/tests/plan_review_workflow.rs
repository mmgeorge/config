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
        .settled("acceptance", |fixture| {
            Ok(fixture
                .store()?
                .load_plan(&plan.id)?
                .is_some_and(|plan| plan.state == crate::plan::PlanState::Accepted))
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
