struct BoundarySteeringBackend {
    initial_started: tokio::sync::Semaphore,
    initial_release: tokio::sync::Semaphore,
    steering_deferred: tokio::sync::Semaphore,
    continuation_started: tokio::sync::Semaphore,
    continuation_accept: tokio::sync::Semaphore,
    continuation_release: tokio::sync::Semaphore,
    input: std::sync::Mutex<Vec<String>>,
    fail_response: std::sync::atomic::AtomicBool,
}

impl BoundarySteeringBackend {
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
impl Backend for BoundarySteeringBackend {
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        crate::backend::BackendDescriptor {
            kind: crate::backend::BackendKind::Codex,
            label: "Boundary steering fixture".into(),
            capability: crate::backend::mock_capability(),
        }
    }

    async fn shutdown(&self) -> Result<()> { Ok(()) }
    async fn has_active_turn(&self, _: &str) -> bool { true }
    async fn cleanup_execution(&self, _: &str) -> Result<()> { Ok(()) }
    async fn active_session_id_for(&self, _: &str) -> Option<String> { Some("boundary-thread".into()) }

    async fn steer_session(&self, _: &str, _: crate::backend::SteeringInput) -> Result<()> {
        self.steering_deferred.add_permits(1);
        anyhow::bail!("provider turn ended before steering dispatch")
    }

    async fn prompt_stream(
        &self,
        request: BackendRequest,
        sink: Option<crate::backend::BackendEventSink>,
    ) -> Result<crate::backend::BackendOutput> {
        let text = request.input.text();
        self.input.lock().unwrap().push(text.clone());
        let continuation = text == "boundary correction";
        if continuation {
            self.continuation_started.add_permits(1);
            self.continuation_accept.acquire().await?.forget();
        }
        let submission = request.submission.as_ref().context("missing tracked submission")?;
        submission.sending(crate::backend::submission::SubmissionTarget::Codex {
            thread_id: "boundary-thread".into(),
        })?;
        submission.accepted("boundary provider receipt".into())?;
        if continuation {
            self.continuation_release.acquire().await?.forget();
            if self.fail_response.load(std::sync::atomic::Ordering::Acquire) {
                anyhow::bail!("Turn cancelled by user");
            }
        } else {
            self.initial_started.add_permits(1);
            self.initial_release.acquire().await?.forget();
        }
        if let Some(sink) = sink {
            sink.send_wait(BackendEvent {
                received_at_ms: None, address: None, turn_boundary: None,
                kind: "assistant_message".into(), text: Some(format!("handled {text}")),
                data: Value::Null, activity: None, summary: None, task_update: None,
            }).await?;
        }
        Ok(crate::backend::BackendOutput {
            backend_session_id: Some("boundary-thread".into()),
            capability: self.descriptor().capability,
            ..Default::default()
        })
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
            let backend = Arc::new(BoundarySteeringBackend::new());
            broker.backend = backend.clone();
            let lane = broker.parent_input.clone();
            let catalog = broker.backend_catalog_request();
            let cancellation = broker.turn_cancellation();
            cancellation.arm(true);
            let journal = crate::backend::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
            let running = tokio::spawn(async move {
                let result = broker.run_interaction("initial request".into(), PromptMode::Chat,
                    Some(ExchangeAdmission::chat("initial request".into()))).await;
                (broker, result)
            });
            backend.initial_started.acquire().await?.forget();
            let submission = journal.prepare_input(&catalog,
                BackendInput::from_text("boundary correction"), crate::backend::BackendKind::Codex,
                Some("cancel-boundary-message".into()))?;
            let mut steering = Box::pin(lane.steer(crate::backend::SteeringInput::User {
                text: "boundary correction".into(), attachments: Vec::new(), submission: Some(submission.clone()),
            }));
            assert!(futures_util::poll!(steering.as_mut()).is_pending());
            backend.steering_deferred.acquire().await?.forget();
            backend.initial_release.add_permits(1);
            backend.continuation_started.acquire().await?.forget();
            if accept_before_cancel {
                backend.continuation_accept.add_permits(1);
                (&mut steering).await?;
            }
            cancellation.request(false);
            let (broker, result) = running.await?;
            assert!(result.is_err());
            if !accept_before_cancel {
                assert!(steering.await.is_err(), "unaccepted steering was acknowledged after cancellation");
            }
            assert_eq!(matches!(submission.record()?.delivery,
                crate::backend::submission::SubmissionState::Accepted { .. }), accept_before_cancel);
            let exchange = broker.store.list_exchange(&broker.session.id)?;
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].node_list.iter().filter(|node| matches!(node,
                crate::exchange::ExchangeNode::ExchangeInput { prompt }
                    if prompt.id == "cancel-boundary-message")).count(), usize::from(accept_before_cancel));
            Ok::<(), anyhow::Error>(())
        }).await??;
    }
    Ok(())
}

async fn assert_deferred_steering_delivery(fail_response: bool) -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = Arc::new(BoundarySteeringBackend::new());
        backend.fail_response.store(fail_response, std::sync::atomic::Ordering::Release);
        broker.backend = backend.clone();
        let lane = broker.parent_input.clone();
        let catalog = broker.backend_catalog_request();
        let journal = crate::backend::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
        let running = tokio::spawn(async move {
            let result = broker.run_interaction("initial request".into(), PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into()))).await;
            (broker, result)
        });
        backend.initial_started.acquire().await?.forget();
        let submission = journal.prepare_input(&catalog,
            BackendInput::from_text("boundary correction"), crate::backend::BackendKind::Codex, Some("boundary-message".into()))?;
        let mut steering = Box::pin(lane.steer(crate::backend::SteeringInput::User {
            text: "boundary correction".into(), attachments: Vec::new(), submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(steering.as_mut()).is_pending());
        backend.steering_deferred.acquire().await?.forget();
        backend.initial_release.add_permits(1);
        backend.continuation_started.acquire().await?.forget();
        assert!(matches!(submission.record()?.delivery, crate::backend::submission::SubmissionState::Prepared));
        assert!(futures_util::poll!(steering.as_mut()).is_pending(),
            "a continuation was acknowledged while its journal still allowed requeueing");
        backend.continuation_accept.add_permits(1);
        steering.await?;
        assert!(matches!(submission.record()?.delivery, crate::backend::submission::SubmissionState::Accepted { .. }));
        assert!(!running.is_finished(), "steering acknowledgement waited for the full provider response");
        backend.continuation_release.add_permits(1);
        let (broker, result) = running.await?;
        if fail_response { assert!(result.is_err()); } else { result?; }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1, "steering created a second exchange");
        assert_eq!(exchange[0].node_list.iter().filter(|node| matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text == "boundary correction" && prompt.id == "boundary-message")).count(), 1);
        assert_eq!(*backend.input.lock().unwrap(), ["initial request", "boundary correction"]);
        Ok::<(), anyhow::Error>(())
    }).await?
}
