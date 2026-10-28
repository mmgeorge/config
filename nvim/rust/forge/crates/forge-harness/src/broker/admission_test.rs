struct PreparationProbe {
    started: Notify,
    release: Notify,
    fail: bool,
}

#[tokio::test]
async fn admission_skips_completed_transcript_but_settles_running_exchange() -> Result<()> {
    for completed in [true, false] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut previous, _, _) = broker.interaction_for_turn("previous", true, 10).await?;
        if completed {
            previous.finish(ExchangeState::Complete, 20)?;
        }
        broker.store.save_exchange(&previous)?;
        if completed {
            let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3"))?;
            connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.node_list','invalid transcript') WHERE id=?1", [&previous.id])?;
        }
        let (next, new_runtime, events) = broker.interaction_for_turn("next", true, 30).await?;
        assert!(new_runtime);
        assert_ne!(next.id, previous.id);
        assert_eq!(next.ordinal, previous.ordinal + 1);
        if completed {
            assert!(events.is_empty(), "completed exchange was republished");
        } else {
            assert_eq!(broker.store.load_exchange(&previous.id)?.unwrap().state, ExchangeState::Complete);
            assert!(!events.is_empty(), "required completion was not published");
        }
    }
    Ok(())
}

#[tokio::test]
async fn admission_precedes_preparation_and_settles_success_failure_and_cancellation() -> Result<()> {
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
        broker.backend = Arc::new(CheckpointBackend {
            preparation: Some(preparation.clone()),
            started: Arc::new(Notify::new()),
            tool_ran: tool_ran.clone(),
            active: Default::default(),
        });
        let (sink, mut stream) = crate::backend::events::channel();
        broker.event_sink = Some(sink);
        let session_id = broker.session.id.clone();
        let cancellation = broker.turn_cancellation();
        cancellation.arm(true);
        let observe = async {
            preparation.started.notified().await;
            let mut published = false;
            while let Ok(event) = stream.try_recv() {
                published |= event.kind == "execution_state";
            }
            assert!(published, "working publication waited for preparation");
            let store = SqliteStore::read_only(data.path())?;
            let exchange = store.list_exchange(&session_id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].state, ExchangeState::Running);
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
            tokio::join!(broker.run_interaction(
                "Visible before provider preparation".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Visible before provider preparation".into())),
            ), observe)
        }).await?;
        observed?;
        match outcome {
            "complete" => { result?; }
            "fail" => assert!(format!("{:#}", result.unwrap_err()).contains("fixture preparation failed")),
            "cancel" => assert!(result.unwrap_err().is::<TurnCancelled>()),
            _ => unreachable!(),
        }
        assert_eq!(tool_ran.load(Ordering::SeqCst), outcome == "complete");
        let exchange = broker.store.list_exchange(&session_id)?;
        assert_eq!(exchange.len(), 1, "preparation created a duplicate exchange");
        assert_eq!(exchange[0].state, match outcome {
            "complete" => ExchangeState::Complete,
            "fail" => ExchangeState::Failed,
            _ => ExchangeState::Cancelled,
        });
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
    let result = broker.dispatch(Request {
        id: 1, method: "prompt.submit".into(),
        params: json!({"text":"prompt me with a multiple-choice question"}),
    }).await;
    assert!(result.response.error().is_none(), "{:?}", result.response.error());
    let snapshot = broker.snapshot()?;
    let ready = broker.observer().input_readiness()?;
    assert!(snapshot.active_elicitation.is_some());
    assert!(ready.has_elicitation);
    assert_eq!(ready.session.current_task_id, snapshot.session.current_task_id);
    assert_eq!(ready.checks_running, snapshot.checks_running);
    assert_eq!(ready.has_approval, !snapshot.approval.is_empty());
    assert_eq!(ready.native_steer, snapshot.capability.native_steer);
    Ok(())
}
