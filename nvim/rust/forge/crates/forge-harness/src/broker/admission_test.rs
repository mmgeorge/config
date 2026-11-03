struct PreparationProbe {
    started: Notify,
    release: Notify,
    fail: bool,
}

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

/// Refuse stale required answers instead of attaching them to newer source work.
#[tokio::test]
async fn detached_required_answer_rejects_cancelled_or_newer_exchange() -> Result<()> {
    use crate::question::{
        QuestionAction, QuestionAnswer, QuestionCommand, QuestionDraft, QuestionMessageKind,
        QuestionOrigin, QuestionRequirement,
    };
    for newer_exchange in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut original, _, _) = broker
            .test_stage_interaction_for_turn("Original", true, 1)
            .await?;
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
        broker.questions.apply(QuestionCommand {
            operation_id: "answer".into(),
            question_id: question_id.clone(),
            revision: 1,
            action: QuestionAction::Answer {
                answer: QuestionAnswer::Other {
                    text: "JSON".into(),
                },
            },
        })?;
        original.finish(
            if newer_exchange {
                crate::exchange::ExchangeOutcome::Complete
            } else {
                crate::exchange::ExchangeOutcome::Cancelled
            },
            2,
        )?;
        broker.store.save_exchange(&original)?;
        if newer_exchange {
            let (newer, _, _) = broker
                .test_stage_interaction_for_turn("Newer", true, 3)
                .await?;
            broker.store.save_exchange(&newer)?;
        }
        let count = broker.store.list_exchange(&broker.session.id)?.len();
        broker.input_purpose = crate::input::InputPurpose::Question {
            reply: crate::provider::test_support::question_reply(&question_id,
                forge_provider_api::QuestionReplyResponse::Answer { answer: "JSON".into(), required_questions_remain: false }),
            question_id,
            task_id: None,
            message_kind: QuestionMessageKind::Answer,
        };
        let result = broker
            .dispatch(Request {
                id: 2,
                method: "session.prompt.submit".into(),
                params: json!({"text":"JSON"}),
            })
            .await;
        assert!(
            result.response.error().is_some(),
            "stale answer was dispatched"
        );
        assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(), count);
        assert_eq!(
            broker.store.load_exchange(&original.id)?.unwrap().state(),
            original.state()
        );
    }
    Ok(())
}

/// Reject host completion when its presentation receiver has disappeared.
#[tokio::test]
async fn closed_publication_cannot_complete_an_exchange() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (sink, stream) = publication::channel();
    broker.event_sink = Some(sink);
    drop(stream);
    let error = broker
        .run_interaction(
            "Publication must finish before this result".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat(
                "Publication must finish before this result".into(),
            )),
        )
        .await
        .unwrap_err();
    assert_eq!(
        error
            .downcast_ref::<std::io::Error>()
            .map(std::io::Error::kind),
        Some(std::io::ErrorKind::BrokenPipe)
    );
    assert!(
        broker
            .store
            .list_exchange(&broker.session.id)?
            .iter()
            .all(|exchange| exchange.state() != ExchangeState::Complete)
    );
    Ok(())
}

#[tokio::test]
async fn admission_skips_completed_transcript_but_settles_running_exchange() -> Result<()> {
    for completed in [true, false] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut previous, _, _) = broker
            .test_stage_interaction_for_turn("previous", true, 10)
            .await?;
        if completed {
            previous.finish(crate::exchange::ExchangeOutcome::Complete, 20)?;
        }
        broker.store.save_exchange(&previous)?;
        if completed {
            let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3"))?;
            connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.node_list','invalid transcript') WHERE id=?1", [&previous.id])?;
        }
        let (next, new_runtime, events) = broker
            .test_stage_interaction_for_turn("next", true, 30)
            .await?;
        assert!(new_runtime);
        assert_ne!(next.id, previous.id);
        assert_eq!(next.ordinal, previous.ordinal + 1);
        if completed {
            assert!(events.is_empty(), "completed exchange was republished");
        } else {
            assert_eq!(
                broker.store.load_exchange(&previous.id)?.unwrap().state(),
                ExchangeState::Complete
            );
            assert!(!events.is_empty(), "required completion was not published");
        }
    }
    Ok(())
}

#[tokio::test]
async fn admission_precedes_preparation_and_settles_success_failure_and_cancellation() -> Result<()>
{
    for outcome in ["complete", "fail", "cancel"] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let preparation = Arc::new(PreparationProbe {
            started: Notify::new(),
            release: Notify::new(),
            fail: outcome == "fail",
        });
        let tool_ran = Arc::new(AtomicBool::new(false));
        let provider_fixture = checkpoint_fixture(
            repository.path().to_owned(), Some(preparation.clone()), Arc::new(Notify::new()),
            tool_ran.clone(), Default::default(), None, false,
        )?;
        broker.provider = provider_fixture.provider_handle();
        let (sink, mut stream) = publication::channel();
        broker.event_sink = Some(sink);
        let session_id = broker.session.id.clone();
        let cancellation = broker.turn_cancellation();
        cancellation.arm(true);
        let observe = async {
            preparation.started.notified().await;
            let mut published = false;
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
            if outcome == "cancel" {
                cancellation.request(false);
            } else {
                preparation.release.notify_one();
            }
            Ok::<_, anyhow::Error>(())
        };
        let (result, observed) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
            tokio::join!(
                broker.run_interaction(
                    "Visible before provider preparation".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat(
                        "Visible before provider preparation".into()
                    )),
                ),
                observe
            )
        })
        .await?;
        observed?;
        match outcome {
            "complete" => {
                result?;
            }
            "fail" => {
                assert!(format!("{:#}", result.unwrap_err()).contains("fixture preparation failed"))
            }
            "cancel" => assert!(result.unwrap_err().is::<TurnCancelled>()),
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
            match outcome {
                "complete" => ExchangeState::Complete,
                "fail" => ExchangeState::Failed,
                _ => ExchangeState::Cancelled,
            }
        );
        assert!(broker.exchange_runtime.is_none());
    }
    Ok(())
}

#[tokio::test]
async fn input_readiness_matches_pending_question_without_loading_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), true);
    assert!(!broker.observer().input_readiness()?.has_elicitation);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"prompt me with a multiple-choice question"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let snapshot = broker.snapshot()?;
    let ready = broker.observer().input_readiness()?;
    assert!(snapshot.active_elicitation.is_some());
    assert!(ready.has_elicitation);
    assert_eq!(
        broker
            .observer()
            .admission
            .snapshot()?
            .task_revision
            .as_ref(),
        snapshot
            .session
            .active_task
            .as_ref()
            .map(|task| &task.revision)
    );
    assert_eq!(ready.checks_running, snapshot.checks_running);
    assert_eq!(ready.has_approval, !snapshot.approval.is_empty());
    assert_eq!(ready.native_steer, snapshot.capability.native_steer);
    Ok(())
}
