use super::*;

/// Create an unfinished, persisted source without introducing a native provider protocol into these tests.
pub(super) async fn fixture() -> Result<(tempfile::TempDir, Arc<SessionController>, String)> {
    let root = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"question-owner-events",
        "provider":{"kind":"mock","command":[]},
    }))?)?;
    let controller = SessionController::new(broker).await?;
    let session = controller.observer.session_id();
    let exchange = crate::exchange::Exchange {
        finalization_error: None,
        check_owner: None,
        output_store: None,
        archived_output: Default::default(),
        input_message_id: None,
        provider_retry: None,
        agent_id: crate::session::HarnessSession::primary_agent_id(session),
        id: "question-source".into(),
        session_id: session.into(),
        ordinal: 1,
        prompt: "Clarify the workspace".into(),
        origin: crate::exchange::ExchangeOrigin::User,
        approval_mode: None,
        phase: crate::exchange::ExchangePhase::Running,
        checkpoint_before: None,
        checkpoint_after: None,
        attributed_diff_text: None,
        checkpoint_diff_text: None,
        attributed_matches_checkpoint: false,
        approval_resolution: Vec::new(),
        host_file_changes: Vec::new(),
        check_setup: None,
        turn: Vec::new(),
        created_at_ms: 1,
        tool_counter: Default::default(),
        entry: Vec::new(),
        node_list: Vec::new(),
        metrics: Default::default(),
        comment: Vec::new(),
        checklist: None,
        question: crate::exchange::ExchangeQuestion::None,
        clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 0 },
    };
    SqliteStore::open(&controller.data_root)?.save_exchange(&exchange)?;
    Ok((root, controller, exchange.id))
}

/// Capture the real owner event handle and task authority used by a tool callback.
pub(super) fn context(
    controller: &Arc<SessionController>,
    exchange: &str,
) -> crate::tools::ControlTurnContext {
    let mut context = crate::tools::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
    context.bind_test_task_controller(controller.task_controller.clone());
    context.questions = Some(controller.questions.clone());
    context.question_events = Some(controller.exchange_owner.question_events());
    context.question_exchange_id = Some(exchange.into());
    context
}

/// Capture immutable creation input before async intake so a replacement task cannot retarget it.
pub(super) fn creation(
    context: &crate::tools::ControlTurnContext,
    exchange: &str,
    operation: &str,
) -> Result<crate::exchange::QuestionCreation> {
    Ok(crate::exchange::QuestionCreation::new(
        crate::exchange::QuestionOperationId::new(operation.into())?,
        exchange.into(),
        None,
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
        crate::exchange::QuestionCreationOrigin::Workflow,
        crate::tools::TaskAuthority::capture(context).unwrap(),
    )?)
}

/// Waiting publication must not hold the runtime lock, lose a fast answer, or manufacture input delivery.
#[tokio::test]
async fn waiting_question_owner_publishes_before_barrier_and_allows_revision_and_answer()
-> Result<()> {
    let (_root, controller, exchange) = fixture().await?;
    let mut context = context(&controller, &exchange);
    // Owner intake authenticates tool-created questions against a live native invocation,
    // which this fixture never starts. The local question controller exercises the same
    // publication barrier, revision, and answer delivery without that admission proof;
    // authenticated owner routing is covered by live session tests.
    context.question_events = None;
    let executor = crate::tools::ControlToolExecutor::new(context);
    let broker = controller.broker.lock().await;
    let (barrier, published) = tokio::sync::oneshot::channel();
    let mut waiting = Box::pin(executor.execute_with_barrier(crate::tools::ControlToolInvocation { call_id: None,
        task_permit: None, name: "question_ask".into(), arguments: json!({"wait":true,"questions":[{"header":"Scope","question":"Which scope?","options":[]}]}),
    }, Some(barrier)));
    async {
        tokio::select! {
            result = waiting.as_mut() => Err(anyhow::anyhow!("waiting question settled before an answer: {}", result.is_ok())),
            ready = published => ready.context("question publication barrier was lost"),
        }
    }.await?;
    let question = controller.questions.snapshot()?.questions[0].clone();
    assert!(question.pending());
    assert_eq!(
        question.delivery,
        crate::question::QuestionDelivery::ToolResult
    );
    {
        let revised = executor.execute(crate::tools::ControlToolInvocation { call_id: None,
            task_permit: None, name: "question_revise".into(), arguments: json!({"question_id":question.id,"revision":1,"draft":{"header":"Scope","question":"Which project scope?","options":[]}}),
        }).await?;
        assert!(revised.result.is_ok());
    }
    assert_eq!(
        controller.questions.snapshot()?.questions[0]
            .content
            .revision,
        2
    );
    controller
        .exchange_owner
        .question_events()
        .change(crate::exchange::QuestionChange::user(
            crate::question::QuestionCommand {
                operation_id: "fast-answer".into(),
                question_id: question.id,
                revision: 2,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Workspace".into(),
                    },
                },
            },
        )?)
        .await?;
    let outcome = waiting.as_mut().await?;
    let answer: Value = serde_json::from_str(&outcome.result?.message)?;
    assert_eq!(
        answer["answers"][0]["resolution"]["answer"]["text"],
        "Workspace"
    );
    assert!(controller.questions.outbox()?.is_empty());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    drop(broker);
    controller.finish_exchange_owner().await?;
    Ok(())
}
