use super::super::{
    ExchangeAdmission, ExchangeState, PromptMode, Request, SessionUpdate, SqliteStore,
    TurnRetracted, publication,
};
use super::test_support::{
    PreparationProbe, checkpoint_fixture, git_repository, planning_question_broker, repository,
};
use anyhow::{Context, Result};
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::Notify;

/// Preserve a required tool question's exchange when clarification has released its native waiter.
#[tokio::test]
async fn detached_required_answer_continues_original_exchange_before_or_after_completion()
-> Result<()> {
    use crate::question::{
        QuestionAction, QuestionAnswer, QuestionCommand, QuestionDraft, QuestionMessageKind,
        QuestionOrigin, QuestionRequirement,
    };
    for answer_before_completion in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut original, _, _) = broker
            .test_stage_interaction_for_turn("Choose a format then produce a sample", true, 1)
            .await?;
        broker.begin_exchange_runtime(&mut original, 1)?;
        broker.store.save_exchange(&original)?;
        let question_id = broker
            .questions
            .ask(
                "required-tool",
                &original.id,
                QuestionOrigin::Tool(None),
                vec![QuestionDraft {
                    header: "Format".into(),
                    question: "Which format?".into(),
                    requirement: QuestionRequirement::Required,
                    options: Vec::new(),
                }],
            )?
            .remove(0);
        let answer = QuestionCommand {
            operation_id: "answer".into(),
            question_id: question_id.clone(),
            revision: 1,
            action: QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "JSON".into(),
                },
            },
        };
        if answer_before_completion {
            broker.questions.apply(answer.clone())?;
        }
        broker
            .run_interaction(
                "Explain the two formats without selecting one".into(),
                PromptMode::Chat,
                None,
            )
            .await?;
        let waiting = broker.store.latest_exchange(&broker.session.id)?.unwrap();
        assert_eq!(waiting.id, original.id);
        assert_eq!(waiting.awaiting_input(), !answer_before_completion);
        if !answer_before_completion {
            assert!(
                waiting.execution_started_at_ms().is_none(),
                "question wait kept the execution clock active"
            );
        }
        assert_eq!(
            waiting.state(),
            if answer_before_completion {
                ExchangeState::Complete
            } else {
                ExchangeState::Running
            }
        );
        if !answer_before_completion {
            broker.questions.apply(answer)?;
        }
        let packet = broker
            .questions
            .outbox()?
            .into_iter()
            .find(|message| message.kind == QuestionMessageKind::Answer)
            .unwrap();
        broker.input_purpose = crate::input::InputPurpose::Question {
            additional_replies: Vec::new(),
            content_revision: None,
            question_id,
            task_id: None,
            message_kind: QuestionMessageKind::Answer,
            reply: packet.reply.clone(),
        };
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":packet.text}),
            })
            .await;
        assert!(
            result.response.error().is_none(),
            "answer_before_completion={answer_before_completion}: {:?}",
            result.response.error()
        );
        let exchanges = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchanges.len(), 1, "answer created another exchange");
        assert_eq!(exchanges[0].id, original.id);
        assert_eq!(exchanges[0].state(), ExchangeState::Complete);
        assert_eq!(exchanges[0].turn.len(), 2);
        assert!(
            !broker.question_discussion,
            "required continuation used the temporary Ask model"
        );
    }
    Ok(())
}

#[tokio::test]
async fn admission_precedes_preparation_and_settles_success_failure_and_cancellation() -> Result<()>
{
    for outcome in ["complete", "fail", "cancel"] {
        let repository = git_repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let preparation = Arc::new(PreparationProbe {
            started: Notify::new(),
            release: Notify::new(),
            fail: outcome == "fail",
        });
        let tool_ran = Arc::new(AtomicBool::new(false));
        let provider_fixture = checkpoint_fixture(
            repository.path().to_owned(),
            Some(preparation.clone()),
            Arc::new(Notify::new()),
            tool_ran.clone(),
            Default::default(),
            None,
            false,
        )?;
        broker.provider = provider_fixture.provider_handle();
        let (sink, mut stream) = publication::channel();
        broker.event_sink = Some(sink);
        let session_id = broker.session.id.clone();
        let cancellation = broker.turn_cancellation();
        cancellation.arm();
        let (settled, mut completion) = tokio::sync::watch::channel(false);
        let observe = async {
            let mut published = false;
            let preparing = preparation.started.notified();
            tokio::pin!(preparing);
            // Admission may flush its publication before native preparation begins.
            loop {
                tokio::select! {
                    () = &mut preparing => break,
                    changed = completion.changed() => {
                        changed?;
                        anyhow::ensure!(!*completion.borrow(), "turn settled before preparation was admitted");
                    }
                    update = stream.recv() => {
                        let update = update?.context("publication closed before preparation")?;
                        published |= matches!(update, SessionUpdate::ExecutionState { .. });
                    }
                }
            }
            while let Ok(event) = stream.try_recv() {
                published |= matches!(event, SessionUpdate::ExecutionState { .. });
            }
            assert!(published, "working publication waited for preparation");
            let store = SqliteStore::read_only(data.path())?;
            let exchange = store.list_exchange(&session_id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].state(), ExchangeState::Running);
            assert_eq!(exchange[0].prompt, "Visible before provider preparation");
            assert!(!tool_ran.load(Ordering::SeqCst));
            drop(store);
            if outcome == "cancel" {
                cancellation.request();
            } else {
                preparation.release.notify_one();
            }
            // Publication flushes require a consumer through the end of settlement.
            while !*completion.borrow() {
                tokio::select! {
                    changed = completion.changed() => { changed?; },
                    update = stream.recv() => {
                        if update?.is_none() {
                            break;
                        }
                    }
                }
            }
            Ok::<_, anyhow::Error>(())
        };
        let (result, observed) = async {
            tokio::join!(
                async {
                    let result = broker
                        .run_interaction(
                            "Visible before provider preparation".into(),
                            PromptMode::Chat,
                            Some(ExchangeAdmission::chat(
                                "Visible before provider preparation".into(),
                            )),
                        )
                        .await;
                    settled.send_replace(true);
                    result
                },
                observe
            )
        }
        .await;
        observed?;
        match outcome {
            "complete" => {
                result?;
            }
            "fail" => {
                let error = result.unwrap_err();
                assert!(error.chain().any(|cause| matches!(
                    cause.downcast_ref::<forge_provider_api::ProviderFailure>(),
                    Some(failure) if failure.kind() == forge_provider_api::ProviderFailureKind::Runtime
                )), "preparation lost its typed failure: {error:#}");
            }
            "cancel" => {
                // Cancellation before dispatch leaves the submission never consumed, so the
                // host retracts the output-free exchange instead of retaining it as cancelled.
                let error = result.unwrap_err();
                assert!(error.is::<TurnRetracted>(), "{error:#}");
                assert!(!tool_ran.load(Ordering::SeqCst));
                assert!(
                    broker.store.list_exchange(&session_id)?.is_empty(),
                    "retracted exchange remained in the transcript"
                );
                assert!(broker.exchange_runtime.is_none());
                continue;
            }
            _ => unreachable!(),
        }
        assert_eq!(tool_ran.load(Ordering::SeqCst), outcome == "complete");
        let exchange = broker.store.list_exchange(&session_id)?;
        assert_eq!(
            exchange.len(),
            1,
            "preparation created a duplicate exchange"
        );
        assert_eq!(
            exchange[0].state(),
            if outcome == "complete" {
                ExchangeState::Complete
            } else {
                ExchangeState::Failed
            }
        );
        assert!(broker.exchange_runtime.is_none());
    }
    Ok(())
}

/// Admit one root exchange, deliver its usage reports to a single turn, and settle it.
async fn usage_exchange(
    broker: &mut super::super::HarnessBroker,
    prompt: &str,
    report_list: &[forge_provider_api::TurnUsageReport],
    now_ms: i64,
) -> Result<forge_provider_api::usage::TokenUsage> {
    use crate::provider::{ProviderEvent, TurnEvent};
    let (mut exchange, _, _) = broker
        .test_stage_interaction_for_turn(prompt, true, now_ms)
        .await?;
    let scope = crate::provider::InvocationScope::new(
        crate::provider::ThreadOwner::root(&broker.session.id),
        exchange.id.clone(),
    );
    let address = crate::provider::test_support::address(&scope);
    let started = TurnEvent::Started(forge_provider_api::turn::TurnStarted {
        started_at_ms: now_ms,
    });
    let usage = report_list.iter().cloned().map(TurnEvent::Usage);
    for event in std::iter::once(started).chain(usage) {
        exchange.observe_provider_event(
            &address.scope.clone().into(),
            &ProviderEvent::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event,
            },
            now_ms,
        )?;
    }
    exchange.finish(crate::exchange::ExchangeOutcome::Interrupted, now_ms)?;
    broker.store.save_exchange(&exchange)?;
    Ok(broker
        .store
        .load_exchange(&exchange.id)?
        .context("settled exchange disappeared")?
        .usage())
}

/// Charge a cumulative token report to the exchange that first received it, across restarts.
///
/// Codex opens a turn on a resumed thread by repeating the thread's last usage notification:
/// the previous call's breakdown under an unchanged cumulative total. A host that restarted
/// between the two exchanges holds no memory of that total, so the later exchange, cancelled
/// before any model call, showed the earlier exchange's tokens in its heading. The baseline
/// must also pass through an exchange that reported nothing to the one after it.
#[tokio::test]
async fn replayed_cumulative_usage_charges_only_its_first_exchange() -> Result<()> {
    use forge_provider_api::usage::TokenUsage;
    use forge_provider_api::{TurnUsageReport, UsageReportId};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let first_call = TokenUsage {
        input: Some(1_200),
        cached_input: Some(200),
        reasoning: Some(40),
        output: Some(90),
    };
    let second_call = TokenUsage {
        input: Some(300),
        cached_input: Some(100),
        reasoning: Some(10),
        output: Some(25),
    };
    let mut thread_total = first_call.clone();
    thread_total.accumulate(&second_call);
    let report = |observed_at_ms: i64, call: &TokenUsage, cumulative: &TokenUsage, total: u64| {
        TurnUsageReport::tokens(
            UsageReportId::new(format!("total-{total}"))?,
            None,
            observed_at_ms,
            Some(call.clone()),
            Some(cumulative.clone()),
            Some(total),
        )
        .map_err(anyhow::Error::from)
    };

    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let session_id = broker.session.id.clone();
    assert_eq!(
        usage_exchange(
            &mut broker,
            "First prompt",
            &[report(100, &first_call, &first_call, 1_290)?],
            100,
        )
        .await?,
        first_call
    );
    drop(broker);

    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    assert_eq!(broker.session.id, session_id);
    assert_eq!(
        usage_exchange(
            &mut broker,
            "Cancelled before any model call",
            &[report(200, &first_call, &first_call, 1_290)?],
            200,
        )
        .await?,
        TokenUsage::default(),
        "a replayed cumulative total was charged to a second exchange"
    );
    drop(broker);

    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    assert_eq!(
        usage_exchange(
            &mut broker,
            "Third prompt",
            &[
                report(300, &first_call, &first_call, 1_290)?,
                report(301, &second_call, &thread_total, 1_615)?,
            ],
            300,
        )
        .await?,
        second_call,
        "the baseline did not pass through an exchange that reported nothing"
    );
    Ok(())
}

/// A user cancellation the provider cannot retract settles as cancelled without an error.
///
/// Claude Code declares no native rollback, so a prompt cancelled after the CLI accepted it stays
/// in history as cancelled, as does any turn that produced output; the cancellation succeeded
/// and is not an error to report.
#[tokio::test]
async fn unretractable_user_cancellation_publishes_no_error_diagnostic() -> Result<()> {
    let repository = git_repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (provider, signals) = super::cleanup_retry_provider(&broker, true, false);
    broker.provider = provider.clone();
    broker.capability.native_turn_rollback = false;
    let (sink, mut stream) = publication::channel();
    broker.event_sink = Some(sink);
    let cancellation = broker.turn_cancellation();
    cancellation.arm();
    let (settled, mut completion) = tokio::sync::watch::channel(false);
    let observe = async {
        let mut diagnostic = Vec::new();
        let mut cancelled = false;
        loop {
            tokio::select! {
                () = signals.started.notified(), if !cancelled => {
                    cancelled = true;
                    cancellation.block_submissions();
                    let scope = signals
                        .scope
                        .lock()
                        .unwrap()
                        .clone()
                        .context("fixture invocation scope missing")?;
                    provider.cancel_invocation(scope).await?;
                    cancellation.request();
                }
                changed = completion.changed() => {
                    changed?;
                    if *completion.borrow() {
                        break;
                    }
                }
                update = stream.recv() => {
                    if let SessionUpdate::Diagnostic { diagnostic: view } =
                        update?.context("publication closed before settlement")?
                    {
                        diagnostic.push(view.message.to_string());
                    }
                }
            }
        }
        while let Ok(update) = stream.try_recv() {
            if let SessionUpdate::Diagnostic { diagnostic: view } = update {
                diagnostic.push(view.message.to_string());
            }
        }
        Ok::<_, anyhow::Error>(diagnostic)
    };
    let execution = async {
        let result = broker
            .run_interaction(
                "Cancel an accepted prompt".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Cancel an accepted prompt".into())),
            )
            .await;
        settled.send_replace(true);
        result
    };
    let (result, diagnostic) = tokio::join!(execution, observe);
    let error = result.unwrap_err();
    assert!(error.is::<super::super::TurnCancelled>(), "{error:#}");
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
    assert_eq!(diagnostic?, Vec::<String>::new());
    Ok(())
}
