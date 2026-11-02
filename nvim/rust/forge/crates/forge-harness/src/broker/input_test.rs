#[tokio::test]
async fn ordinary_input_does_not_activate_a_pending_plan_entry() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let prepared =
        broker
            .plan_file
            .prepare_entry(&broker.session.id, "Pending plan".into(), None, 1)?;
    let mut task = crate::task::Task::plan(prepared.record, 1)?;
    task.request_entry(prepared.instruction, 1)?;
    let pending = broker
        .task_controller
        .change(
            &mut broker.store,
            None,
            crate::task::TaskChange::Install(Box::new(task)),
            1,
        )?
        .unwrap();
    let response = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Explain the current approach"}),
        })
        .await;
    assert!(
        response.response.error().is_none(),
        "{:?}",
        response.response.error()
    );
    let current = broker.task_controller.current()?.unwrap();
    assert_eq!(current.revision, pending.revision);
    assert!(!current.entry_accepted());
    let crate::task::WorkflowSnapshot::Plan { plan } = current.workflow.clone() else {
        unreachable!()
    };
    assert_eq!(plan.artifact.state, crate::plan::PlanState::Generating);
    let exchange = broker.store.latest_exchange(&broker.session.id)?.unwrap();
    assert_eq!(exchange.prompt, "Explain the current approach");
    assert_eq!(exchange.origin, crate::exchange::ExchangeOrigin::User);
    Ok(())
}

struct BoundarySteeringProvider {
    initial_started: tokio::sync::Semaphore,
    initial_release: tokio::sync::Semaphore,
    steering_deferred: tokio::sync::Semaphore,
    continuation_started: tokio::sync::Semaphore,
    continuation_accept: tokio::sync::Semaphore,
    continuation_release: tokio::sync::Semaphore,
    input: std::sync::Mutex<Vec<String>>,
    fail_response: std::sync::atomic::AtomicBool,
}

struct TaskStartEventProvider {
    inner: Arc<Provider>,
    conflicting_replay: bool,
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for TaskStartEventProvider {
    type Host = crate::provider::HarnessProviderHost;
    fn descriptor(&self) -> crate::provider::ProviderDescriptor {
        self.inner.descriptor()
    }
    async fn shutdown(&self) -> Result<()> {
        self.inner.shutdown().await
    }
    async fn has_active_turn(&self, id: &str) -> bool {
        self.inner.has_active_turn(id).await
    }
    async fn cleanup_execution(&self, id: &str) -> Result<()> {
        self.inner.cleanup_execution(id).await
    }
    async fn active_session_id_for(&self, id: &str) -> Option<String> {
        self.inner.active_session_id_for(id).await
    }
    async fn prompt_stream(
        &self,
        request: ProviderRequest,
        sink: crate::provider::ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
        let scope = sink.invocation_scope()?.clone();
        let start = crate::exchange::TaskStart {
            kind: crate::task::TaskKind::Goal,
            title: "Reduce startup time".into(),
        };
        let mut acknowledgement = crate::provider::InputAcknowledgement::TaskStarted {
            input_id: Some("task-entry".into()),
            start,
            prompt: "Complete startup work.\nVerify the result.".into(),
        };
        sink.send_wait(ProviderEvent::InputAccepted {
            scope: scope.clone(),
            acknowledgement: acknowledgement.clone(),
        })
        .await?;
        if self.conflicting_replay
            && let crate::provider::InputAcknowledgement::TaskStarted { prompt, .. } =
                &mut acknowledgement
        {
            *prompt = "Conflicting instruction".into();
        }
        sink.send_wait(ProviderEvent::InputAccepted {
            scope,
            acknowledgement,
        })
        .await?;
        self.inner.prompt_stream(request, sink).await
    }
}

#[tokio::test]
async fn acknowledged_task_start_is_retained_once_inside_its_exchange() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.provider = Arc::new(TaskStartEventProvider {
        inner: broker.provider.clone(),
        conflicting_replay: false,
    });
    broker
        .run_interaction(
            "Inspect startup".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Inspect startup".into())),
        )
        .await?;
    let snapshot = broker.snapshot()?;
    assert_eq!(snapshot.exchange.len(), 1);
    assert_eq!(
        snapshot.exchange[0].origin,
        crate::exchange::ExchangeOrigin::User
    );
    assert_eq!(snapshot.exchange[0].prompt, "Inspect startup");
    let TimelineEntry::Exchange { exchange, .. } = &snapshot.timeline[0] else {
        panic!("task start created a separate timeline entry")
    };
    let events = exchange
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                crate::timeline::TimelineEvent::Session { event } => Some(event),
                _ => None,
            },
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(events.len(), 1);
    assert_eq!(events[0].owner.exchange_id(), Some(exchange.id.as_str()));
    assert!(
        matches!(&events[0].detail, SessionEventKind::TaskStarted { start, prompt }
        if start.title == "Reduce startup time" && prompt == "Complete startup work.\nVerify the result.")
    );
    assert!(
        !exchange
            .node_list
            .iter()
            .any(|node| matches!(node, ExchangeNode::ExchangeInput { .. }))
    );
    let restored = SqliteStore::open(data.path())?;
    let persisted = restored
        .load_session_event(
            &broker.session.id,
            &format!("{}:task-start:task-entry", broker.session.id),
        )?
        .unwrap();
    assert_eq!(persisted.owner, events[0].owner);
    Ok(())
}

#[tokio::test]
async fn task_start_updates_live_presentation_before_the_turn_finishes() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (mut exchange, _, mut notifications) = broker
        .interaction_for_turn("Inspect startup", true, 1)
        .await?;
    broker.store.save_exchange(&exchange)?;
    broker.reconcile_live_interaction(Some(&exchange), None)?;
    let document = forge_buffer::identity::DocumentId("task-start-live".into());
    broker.presentation.lock().unwrap().open(
        document.clone(),
        forge_buffer::identity::ViewId("task-start-view".into()),
        forge_buffer::width::WidthProfile::default(),
    )?;
    let mut runtime = ExchangeRuntime {
        task_permit: None,
        checkpoint: None,
        tool_gate: None,
        exchange_id: exchange.id.clone(),

        checklist: ChecklistTracker::default(),
        retraction_eligible: true,
        active_wait: None,
    };
    let task_event = ProviderEvent::InputAccepted {
        scope: crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root(&broker.session.id),
            exchange.id.clone(),
        ),
        acknowledgement: crate::provider::InputAcknowledgement::TaskStarted {
            input_id: Some("live-input".into()),
            start: crate::exchange::TaskStart {
                kind: crate::task::TaskKind::Goal,
                title: "Reduce startup time".into(),
            },
            prompt: "Complete startup work.\nVerify the result.".into(),
        },
    };
    broker
        .process_exchange_content(
            &mut runtime,
            &mut exchange,
            task_event.clone(),
            &mut notifications,
        )
        .await?;
    let assert_visible = |broker: &HarnessBroker| -> Result<()> {
        let snapshot = broker.presentation.lock().unwrap().snapshot(&document)?;
        let text = snapshot
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert_eq!(
            text.matches("Goal started: Reduce startup time").count(),
            1,
            "{text}"
        );
        assert!(
            text.contains("Reduce startup time"),
            "{text}"
        );
        Ok(())
    };
    assert_visible(&broker)?;
    broker
        .process_exchange_content(&mut runtime, &mut exchange, task_event, &mut notifications)
        .await?;
    broker.reconcile_live_interaction(Some(&exchange), None)?;
    assert_visible(&broker)?;
    assert_eq!(exchange.state(), ExchangeState::Running);
    assert!(
        broker
            .store
            .load_exchange(&exchange.id)?
            .unwrap()
            .node_list
            .is_empty(),
        "presentation event was copied into the persisted provider content"
    );
    Ok(())
}

#[tokio::test]
async fn task_start_replay_rejects_reused_identity_with_different_content() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.provider = Arc::new(TaskStartEventProvider {
        inner: broker.provider.clone(),
        conflicting_replay: true,
    });
    let result = broker
        .run_interaction(
            "Inspect startup".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Inspect startup".into())),
        )
        .await;
    assert!(
        format!("{:#}", result.unwrap_err())
            .contains("task entry identity was reused for different content")
    );
    let retained = broker
        .store
        .load_session_event(
            &broker.session.id,
            &format!("{}:task-start:task-entry", broker.session.id),
        )?
        .unwrap();
    assert!(
        matches!(retained.detail, SessionEventKind::TaskStarted { prompt, .. } if prompt == "Complete startup work.\nVerify the result.")
    );
    Ok(())
}

impl BoundarySteeringProvider {
    fn new() -> Self {
        Self {
            initial_started: tokio::sync::Semaphore::new(0),
            initial_release: tokio::sync::Semaphore::new(0),
            steering_deferred: tokio::sync::Semaphore::new(0),
            continuation_started: tokio::sync::Semaphore::new(0),
            continuation_accept: tokio::sync::Semaphore::new(0),
            continuation_release: tokio::sync::Semaphore::new(0),
            input: Default::default(),
            fail_response: Default::default(),
        }
    }
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for BoundarySteeringProvider {
    type Host = crate::provider::HarnessProviderHost;
    fn descriptor(&self) -> crate::provider::ProviderDescriptor {
        crate::provider::ProviderDescriptor {
            kind: crate::provider::ProviderKind::Codex,
            label: "Boundary steering fixture".into(),
            capability: crate::provider::mock_capability(),
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
    async fn has_active_turn(&self, _: &str) -> bool {
        true
    }
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        Some("boundary-thread".into())
    }

    async fn steer_session(&self, _: &str, _: crate::provider::SteeringInput) -> Result<()> {
        self.steering_deferred.add_permits(1);
        anyhow::bail!("provider turn ended before steering dispatch")
    }

    async fn prompt_stream(
        &self,
        request: ProviderRequest,
        sink: crate::provider::ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
        let fixture_scope = sink.invocation_scope()?.clone();
        let fixture = crate::provider::test_support::FixtureStream::new(sink);
        let sink = fixture.clone();
        let result: anyhow::Result<crate::provider::ProviderCompletion> = async {
            let output: anyhow::Result<crate::provider::ProviderCompletion> = async {
                let sink = Some(sink);
                let text = request.input.text();
                self.input.lock().unwrap().push(text.clone());
                let continuation = text == "boundary correction";
                if continuation {
                    self.continuation_started.add_permits(1);
                    self.continuation_accept.acquire().await?.forget();
                }
                let submission = request
                    .submission
                    .as_ref()
                    .context("missing tracked submission")?;
                submission.sending(crate::provider::submission::SubmissionTarget::Codex {
                    thread_id: "boundary-thread".into(),
                })?;
                submission.accepted("boundary provider receipt".into())?;
                if continuation {
                    self.continuation_release.acquire().await?.forget();
                    if self
                        .fail_response
                        .load(std::sync::atomic::Ordering::Acquire)
                    {
                        anyhow::bail!("Turn cancelled by user");
                    }
                } else {
                    self.initial_started.add_permits(1);
                    self.initial_release.acquire().await?.forget();
                }
                if let Some(sink) = sink {
                    sink.send_wait(completed_message(
                        &crate::provider::test_support::address(&fixture_scope, "fixture", "turn"),
                        "handled-response",
                        crate::provider::MessageRole::Response,
                        format!("handled {text}"),
                    ))
                    .await?;
                }
                Ok(crate::provider::ProviderCompletion { native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some("boundary-thread".into()),
                    capability: self.descriptor().capability, ..Default::default() },
                    ..Default::default()
                })
            }
            .await;
            Ok(output?)
        }
        .await;
        let result = result.map_err(crate::provider::ProviderError::from);
        fixture.finish(&result).await?;
        result
    }
}

#[tokio::test]
async fn deferred_steering_acknowledges_only_after_provider_acceptance() -> Result<()> {
    assert_deferred_steering_delivery(false).await
}

#[tokio::test]
async fn accepted_steering_survives_response_failure() -> Result<()> {
    assert_deferred_steering_delivery(true).await
}

#[tokio::test]
async fn deferred_steering_cancellation_preserves_acceptance_boundary() -> Result<()> {
    for accept_before_cancel in [false, true] {
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            let repository = repository();
            let data = tempfile::tempdir()?;
            let mut broker = planning_question_broker(repository.path(), data.path(), false);
            let provider = Arc::new(BoundarySteeringProvider::new());
            broker.provider = provider.clone();
            let lane = broker.parent_input.clone();
            let catalog = broker.provider_catalog_request();
            let cancellation = broker.turn_cancellation();
            cancellation.arm(true);
            let journal = crate::provider::submission::SubmissionStore::open(
                data.path(),
                broker.trace.clone(),
            )?;
            let running = tokio::spawn(async move {
                let result = broker
                    .run_interaction(
                        "initial request".into(),
                        PromptMode::Chat,
                        Some(ExchangeAdmission::chat("initial request".into())),
                    )
                    .await;
                (broker, result)
            });
            provider.initial_started.acquire().await?.forget();
            let submission = journal.prepare_input(
                &catalog,
                ProviderInput::from_text("boundary correction"),
                crate::provider::ProviderKind::Codex,
                Some("cancel-boundary-message".into()),
            )?;
            let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
                text: "boundary correction".into(),
                attachments: Vec::new(),
                submission: Some(submission.clone()),
            }));
            assert!(futures_util::poll!(steering.as_mut()).is_pending());
            provider.steering_deferred.acquire().await?.forget();
            provider.initial_release.add_permits(1);
            provider.continuation_started.acquire().await?.forget();
            if accept_before_cancel {
                provider.continuation_accept.add_permits(1);
                (&mut steering).await?;
            }
            cancellation.request(false);
            let (broker, result) = running.await?;
            assert!(result.is_err());
            if !accept_before_cancel {
                assert!(
                    steering.await.is_err(),
                    "unaccepted steering was acknowledged after cancellation"
                );
            }
            assert_eq!(
                matches!(
                    submission.record()?.delivery,
                    crate::provider::submission::SubmissionState::Accepted { .. }
                ),
                accept_before_cancel
            );
            let exchange = broker.store.list_exchange(&broker.session.id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(
                exchange[0]
                    .node_list
                    .iter()
                    .filter(|node| matches!(node,
                crate::exchange::ExchangeNode::ExchangeInput { prompt }
                    if prompt.id == "cancel-boundary-message"))
                    .count(),
                usize::from(accept_before_cancel)
            );
            Ok::<(), anyhow::Error>(())
        })
        .await??;
    }
    Ok(())
}

async fn assert_deferred_steering_delivery(fail_response: bool) -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(BoundarySteeringProvider::new());
        provider.fail_response.store(fail_response, std::sync::atomic::Ordering::Release);
        broker.provider = provider.clone();
        let lane = broker.parent_input.clone();
        let catalog = broker.provider_catalog_request();
        let journal = crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
        let running = tokio::spawn(async move {
            let result = broker.run_interaction("initial request".into(), PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into()))).await;
            (broker, result)
        });
        provider.initial_started.acquire().await?.forget();
        let submission = journal.prepare_input(&catalog,
            ProviderInput::from_text("boundary correction"), crate::provider::ProviderKind::Codex, Some("boundary-message".into()))?;
        let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
            text: "boundary correction".into(), attachments: Vec::new(), submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(steering.as_mut()).is_pending());
        provider.steering_deferred.acquire().await?.forget();
        provider.initial_release.add_permits(1);
        provider.continuation_started.acquire().await?.forget();
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Prepared));
        assert!(futures_util::poll!(steering.as_mut()).is_pending(),
            "a continuation was acknowledged while its journal still allowed requeueing");
        provider.continuation_accept.add_permits(1);
        steering.await?;
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Accepted { .. }));
        assert!(!running.is_finished(), "steering acknowledgement waited for the full provider response");
        provider.continuation_release.add_permits(1);
        let (broker, result) = running.await?;
        if fail_response { assert!(result.is_err()); } else { result?; }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1, "steering created a second exchange");
        assert_eq!(exchange[0].node_list.iter().filter(|node| matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text == "boundary correction" && prompt.id == "boundary-message")).count(), 1);
        assert_eq!(*provider.input.lock().unwrap(), ["initial request", "boundary correction"]);
        Ok::<(), anyhow::Error>(())
    }).await?
}

/// Hold native steering admission while the original provider turn keeps running.
struct SlowSteeringProvider {
    inner: BoundarySteeringProvider,
    steering_started: tokio::sync::Semaphore,
    steering_release: tokio::sync::Semaphore,
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for SlowSteeringProvider {
    type Host = crate::provider::HarnessProviderHost;

    fn descriptor(&self) -> crate::provider::ProviderDescriptor {
        self.inner.descriptor()
    }

    async fn shutdown(&self) -> Result<()> { Ok(()) }

    async fn cleanup_execution(&self, _: &str) -> Result<()> { Ok(()) }

    async fn has_active_turn(&self, _: &str) -> bool { true }

    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        Some("boundary-thread".into())
    }

    async fn prompt_stream(
        &self,
        request: ProviderRequest,
        sink: crate::provider::ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
        self.inner.prompt_stream(request, sink).await
    }

    async fn steer_session(&self, _: &str, input: crate::provider::SteeringInput) -> Result<()> {
        self.steering_started.add_permits(1);
        self.steering_release.acquire().await?.forget();
        let submission = input.submission().context("steering lost its journal")?;
        submission.sending(crate::provider::submission::SubmissionTarget::Codex {
            thread_id: "boundary-thread".into(),
        })?;
        submission.accepted("native steering receipt".into())?;
        Ok(())
    }
}

/// A slow native admission must not turn live steering into a deferred second prompt.
#[tokio::test]
async fn slow_native_steering_keeps_its_delivery_until_acknowledged() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = Arc::new(SlowSteeringProvider {
        inner: BoundarySteeringProvider::new(),
        steering_started: tokio::sync::Semaphore::new(0),
        steering_release: tokio::sync::Semaphore::new(0),
    });
    broker.provider = provider.clone();
    let lane = broker.parent_input.clone();
    let catalog = broker.provider_catalog_request();
    let cancellation = broker.turn_cancellation();
    let journal = crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
    let running = tokio::spawn(async move {
        broker.run_interaction("initial request".into(), PromptMode::Chat,
            Some(ExchangeAdmission::chat("initial request".into()))).await
    });
    provider.inner.initial_started.acquire().await?.forget();
    let submission = journal.prepare_input(&catalog,
        ProviderInput::from_text("keep working"), crate::provider::ProviderKind::Codex,
        Some("slow-steering".into()))?;
    let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
        text: "keep working".into(), attachments: Vec::new(), submission: Some(submission.clone()),
    }));
    assert!(futures_util::poll!(steering.as_mut()).is_pending());
    provider.steering_started.acquire().await?.forget();

    tokio::time::pause();
    tokio::time::advance(std::time::Duration::from_secs(31)).await;
    tokio::task::yield_now().await;
    tokio::task::yield_now().await;
    tokio::time::resume();
    provider.steering_release.add_permits(1);
    let delivered = tokio::time::timeout(std::time::Duration::from_secs(1), steering.as_mut()).await;
    cancellation.request(false);
    provider.inner.initial_release.add_permits(1);
    let _cancelled = running.await?;

    delivered.context("steering was abandoned at the old 30-second deadline")??;
    assert!(matches!(submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Accepted { .. }));
    assert_eq!(*provider.inner.input.lock().unwrap(), ["initial request"]);
    Ok(())
}
