use std::{fmt, time::Duration};

use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::{
    Backend, BackendCompletion, BackendEvent, BackendEventSink, BackendInput, BackendRequest,
    TurnAddress,
};
use crate::trace::TraceStore;

const RETRY_DELAY_SECONDS: [u64; 5] = [1, 5, 10, 30, 60];

/// Current retry attempt within one admitted provider request.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderRetry {
    /// One-based retry number, excluding the initial attempt.
    pub attempt: usize,
    /// Maximum retries for this request.
    pub max_attempts: usize,
    /// Deadline for the delay, absent once the retry is running.
    pub retry_at_ms: Option<i64>,
}

/// Adapter proof that a terminal transient failure produced no replay-sensitive output.
#[derive(Debug)]
pub struct RetryableFailure {
    /// Completed native turn that retains the original request in provider history.
    pub address: TurnAddress,
    /// Structured provider error code used by the adapter's allowlist.
    pub code: String,
    /// Complete provider error payload retained in diagnostic logs.
    pub detail: Value,
    /// Original error displayed when retries stop.
    pub message: String,
}

impl fmt::Display for RetryableFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for RetryableFailure {}

/// Rejects replay after tools, control results, or model output have been observed.
pub(crate) fn replay_safe(output: &super::BackendCompletion) -> bool {
    !output.evidence.tool_called
        && !output.evidence.workspace_changed
        && output.plan_edit.is_empty()
        && output.design_patch.is_empty()
        && output.plan_submit.is_none()
        && output.plan_question.is_none()
        && output.plan_read.is_none()
        && output.plan_deviation.is_empty()
        && output.plan_task_report.is_empty()
        && output.question_answer.is_none()
        && output.question_withdrawal.is_none()
        && output.control_error.is_none()
        && !output.structured_plan
        && !output.evidence.structured_complete
        && !output.evidence.structured_blocked
}

/// Runs one request with bounded retries only after an adapter proves safe terminal failure.
/// Preserve broker cancellation identity when a provider terminates the request intentionally.
/// Dropping this future cancels its delay. Native execution cleanup remains with the caller.
pub(crate) async fn prompt(
    backend: &dyn Backend,
    mut request: BackendRequest,
    event_sink: Option<BackendEventSink>,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
) -> Result<BackendCompletion> {
    let mut progress = RetryProgress {
        trace,
        session_id: request.harness_session_id.clone(),
        agent_id: request.owner()?.agent_id.to_string(),
        attempt: 0,
        pending: false,
    };
    let mut address = None;
    loop {
        let result = if progress.attempt == 0 {
            submit(backend, &mut request, event_sink.clone(), submission_store).await
        } else {
            match event_sink.clone() {
                Some(sink) => backend.prompt_stream(request.clone(), sink).await,
                None => backend
                    .prompt(request.clone())
                    .await
                    .map(|output| output.completion),
            }
        };
        let error = match result {
            Ok(output) => {
                if progress.attempt > 0 {
                    progress.record("recovered", Value::Null);
                    publish(&event_sink, address.clone(), None).await?;
                }
                progress.pending = false;
                return Ok(output);
            }
            Err(error) => error,
        };
        let super::BackendError::Retryable(failure) = &error else {
            if progress.attempt > 0 {
                progress.record("stopped", json!({"error":format!("{error:#}")}));
                publish(&event_sink, address.clone(), None).await?;
            }
            progress.pending = false;
            if matches!(error, super::BackendError::Cancelled) {
                return Err(crate::broker::TurnCancelled.into());
            }
            return Err(error.into());
        };
        progress.record(
            "failed",
            json!({"code":failure.code,"error":failure.detail,"message":failure.message,
            "thread_id":failure.address.thread_id,"turn_id":failure.address.turn_id}),
        );
        let Some(delay_seconds) = RETRY_DELAY_SECONDS.get(progress.attempt).copied() else {
            progress.record(
                "exhausted",
                json!({"code":failure.code,"error":failure.detail,"message":failure.message}),
            );
            publish(&event_sink, address.clone(), None).await?;
            progress.pending = false;
            return Err(error.into());
        };
        address = Some(failure.address.clone());
        request.backend_session_id = Some(failure.address.thread_id.clone());
        // The native thread already contains the request. Do not append it a second time.
        request.input = BackendInput::from_text(
            "Continue the previous request after the temporary provider failure. Preserve the original instructions and complete only unfinished work.",
        );
        request.submission = None;
        request.recovered_input.clear();
        progress.attempt += 1;
        progress.pending = true;
        let retry_at_ms = now_ms().saturating_add((delay_seconds * 1000) as i64);
        progress.record(
            "scheduled",
            json!({"delay_ms":delay_seconds * 1000,"retry_at_ms":retry_at_ms}),
        );
        publish(
            &event_sink,
            address.clone(),
            Some(ProviderRetry {
                attempt: progress.attempt,
                max_attempts: RETRY_DELAY_SECONDS.len(),
                retry_at_ms: Some(retry_at_ms),
            }),
        )
        .await?;
        tokio::time::sleep(Duration::from_secs(delay_seconds)).await;
        progress.record("started", Value::Null);
        publish(
            &event_sink,
            address.clone(),
            Some(ProviderRetry {
                attempt: progress.attempt,
                max_attempts: RETRY_DELAY_SECONDS.len(),
                retry_at_ms: None,
            }),
        )
        .await?;
    }
}

async fn submit(
    backend: &dyn Backend,
    request: &mut BackendRequest,
    sink: Option<BackendEventSink>,
    store: &std::sync::Arc<super::submission::SubmissionStore>,
) -> Result<BackendCompletion, super::BackendError> {
    let submission = match request.submission.take() {
        Some(submission) => submission,
        None => store.prepare(request, backend.descriptor().kind)?,
    };
    submission.recover(backend, request).await?;
    request.submission = Some(submission.clone());
    let tracked = matches!(
        backend.descriptor().kind,
        super::BackendKind::Codex
            | super::BackendKind::Copilot
            | super::BackendKind::Claude
            | super::BackendKind::Antigravity
    );
    if !tracked {
        submission.sending(super::submission::SubmissionTarget::Untracked)?;
        if backend.descriptor().kind == super::BackendKind::Mock {
            submission.accepted("local mock admission".into())?;
        }
    }
    let result = match sink {
        Some(sink) => backend.prompt_stream(request.clone(), sink).await,
        None => backend
            .prompt(request.clone())
            .await
            .map(|output| output.completion),
    };
    if result.is_ok()
        || result
            .as_ref()
            .is_err_and(|error| matches!(error, super::BackendError::Retryable(_)))
    {
        submission.accepted("adapter completed submission".into())?;
    }
    if let Err(error) = &result {
        submission.failed(format!("{error:#}"))?;
    }
    result
}

struct RetryProgress<'trace> {
    trace: &'trace TraceStore,
    session_id: String,
    agent_id: String,
    attempt: usize,
    pending: bool,
}

impl RetryProgress<'_> {
    fn record(&self, status: &str, detail: Value) {
        self.trace.record(&self.session_id, "provider.retry", json!({
            "status":status,"agent_id":self.agent_id,"retry_attempt":self.attempt,"max_retries":RETRY_DELAY_SECONDS.len(),"detail":detail,
        }));
    }
}

impl Drop for RetryProgress<'_> {
    fn drop(&mut self) {
        if self.pending {
            self.record("cancelled", Value::Null);
        }
    }
}

async fn publish(
    sink: &Option<BackendEventSink>,
    _address: Option<TurnAddress>,
    retry: Option<ProviderRetry>,
) -> Result<()> {
    if let Some(sink) = sink {
        sink.send_wait(BackendEvent::Retry {
            scope: sink.invocation_scope()?.clone(),
            update: retry,
        })
        .await?;
    }
    Ok(())
}

pub(crate) fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{
        BackendDescriptor, BackendError, BackendKind, ExecutionOwner, InvocationScope, PromptMode,
    };
    use std::sync::Mutex;

    struct FailureBackend {
        failure_count: usize,
        terminal_error: Option<fn() -> BackendError>,
        request: Mutex<Vec<(BackendRequest, tokio::time::Instant)>>,
    }

    #[async_trait::async_trait]
    impl Backend for FailureBackend {
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }
        fn descriptor(&self) -> BackendDescriptor {
            BackendDescriptor {
                kind: BackendKind::Mock,
                label: "Retry fixture".into(),
                capability: Default::default(),
            }
        }
        async fn has_active_turn(&self, _: &str) -> bool {
            false
        }
        async fn cleanup_execution(&self, _: &str) -> Result<()> {
            Ok(())
        }
        async fn active_session_id_for(&self, _: &str) -> Option<String> {
            Some("retained-thread".into())
        }
        async fn prompt_stream(
            &self,
            request: BackendRequest,
            sink: BackendEventSink,
        ) -> Result<BackendCompletion, BackendError> {
            let output: anyhow::Result<BackendCompletion> = async {
                let mut requests = self.request.lock().unwrap();
                requests.push((request, tokio::time::Instant::now()));
                if requests.len() <= self.failure_count {
                    return Err(RetryableFailure {
                        address: TurnAddress {
                            scope: sink.invocation_scope()?.clone(),
                            thread_id: "retained-thread".into(),
                            turn_id: format!("turn-{}", requests.len()).into(),
                        },
                        code: "serverOverloaded".into(),
                        detail: json!({"message":format!("capacity failure {}", requests.len())}),
                        message: format!("capacity failure {}", requests.len()),
                    }
                    .into());
                }
                if let Some(error) = self.terminal_error {
                    return Err(error().into());
                }
                Ok(BackendCompletion::default())
            }
            .await;
            output.map_err(BackendError::from)
        }
    }

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ExecutionOwner::root("retry-fixture"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn request() -> BackendRequest {
        BackendRequest {
            execution_owner: None,
            harness_session_id: "retry-fixture".into(),
            workspace: "workspace".into(),
            input: BackendInput::from_text("original request"),
            mode: PromptMode::Chat,
            model: "selected-model".into(),
            effort: "high".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    fn backend(failure_count: usize) -> FailureBackend {
        FailureBackend {
            failure_count,
            terminal_error: None,
            request: Mutex::new(Vec::new()),
        }
    }

    #[test]
    fn replay_rejects_tools_and_control_mutations() {
        let mut output = BackendCompletion::default();
        assert!(replay_safe(&output));
        output.evidence.tool_called = true;
        assert!(!replay_safe(&output));
        output = BackendCompletion::default();
        output.plan_read = Some("plan".into());
        assert!(!replay_safe(&output));
        output = BackendCompletion::default();
        output.evidence.workspace_changed = true;
        assert!(!replay_safe(&output));
        output = BackendCompletion::default();
        output.evidence.structured_complete = true;
        assert!(!replay_safe(&output));
    }

    #[tokio::test(start_paused = true)]
    async fn retry_schedule_preserves_model_and_final_error() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        trace.configure(true)?;
        let backend = backend(6);
        let error = prompt(&backend, request(), None, &trace, &submission_store)
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "capacity failure 6");
        let requests = backend.request.lock().unwrap();
        assert_eq!(requests.len(), 6);
        for (index, delay) in RETRY_DELAY_SECONDS.iter().enumerate() {
            assert_eq!(
                requests[index + 1].1.duration_since(requests[index].1),
                Duration::from_secs(*delay)
            );
            assert_eq!(requests[index + 1].0.model, "selected-model");
            assert_eq!(
                requests[index + 1].0.backend_session_id.as_deref(),
                Some("retained-thread")
            );
            assert_ne!(requests[index + 1].0.input.text(), "original request");
        }
        let records = std::fs::read_to_string(trace.session_status("retry-fixture").path)?;
        assert!(records.contains("capacity failure 6"));
        assert!(records.contains("\"status\":\"exhausted\""));
        assert_eq!(
            records
                .lines()
                .filter(|line| line.contains("provider.retry"))
                .count(),
            17
        );
        Ok(())
    }

    #[tokio::test(start_paused = true)]
    async fn retry_recovers_clears_status_and_stops_on_permanent_error() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let recovered_backend = backend(1);
        let (sink, mut stream) = crate::backend::events::channel(scope());
        prompt(
            &recovered_backend,
            request(),
            Some(sink),
            &trace,
            &submission_store,
        )
        .await?;
        let waiting = stream.try_recv()?;
        let BackendEvent::Retry {
            update: Some(waiting),
            ..
        } = waiting
        else {
            panic!("expected scheduled retry");
        };
        assert_eq!(waiting.attempt, 1);
        assert!(waiting.retry_at_ms.is_some());
        assert!(matches!(
            stream.try_recv()?,
            BackendEvent::Retry {
                update: Some(ProviderRetry {
                    retry_at_ms: None,
                    ..
                }),
                ..
            }
        ));
        assert!(matches!(
            stream.try_recv()?,
            BackendEvent::Retry { update: None, .. }
        ));
        let backend = FailureBackend {
            terminal_error: Some(|| BackendError::Provider {
                code: None,
                message: "unauthorized".into(),
            }),
            ..backend(0)
        };
        assert_eq!(
            prompt(&backend, request(), None, &trace, &submission_store)
                .await
                .unwrap_err()
                .to_string(),
            "unauthorized"
        );
        assert_eq!(backend.request.lock().unwrap().len(), 1);
        Ok(())
    }

    /// Preserve cancellation identity on the first attempt and after transient recovery.
    #[tokio::test(start_paused = true)]
    async fn provider_cancellation_reaches_broker_without_another_retry() -> Result<()> {
        for failure_count in [0, 1] {
            let directory = tempfile::tempdir()?;
            let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
            let submission_store =
                super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
            let backend = FailureBackend {
                terminal_error: Some(|| BackendError::Cancelled),
                ..backend(failure_count)
            };
            let (sink, mut stream) = crate::backend::events::channel(scope());
            let error = prompt(&backend, request(), Some(sink), &trace, &submission_store)
                .await
                .unwrap_err();
            assert!(error.is::<crate::broker::TurnCancelled>(), "{error:#}");
            assert_eq!(backend.request.lock().unwrap().len(), failure_count + 1);
            let mut retry_updates = Vec::new();
            while let Some(event) = stream.try_next()? {
                if let BackendEvent::Retry { update, .. } = event {
                    retry_updates.push(update);
                }
            }
            if failure_count == 0 {
                assert!(retry_updates.is_empty());
            } else {
                assert_eq!(retry_updates.len(), 3);
                assert!(retry_updates[0].as_ref().unwrap().retry_at_ms.is_some());
                assert!(retry_updates[1].as_ref().unwrap().retry_at_ms.is_none());
                assert!(retry_updates[2].is_none());
            }
        }
        Ok(())
    }

    /// Keep provider failures distinct even when their rendered message describes cancellation.
    #[tokio::test]
    async fn provider_failure_text_does_not_become_broker_cancellation() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let backend = FailureBackend {
            terminal_error: Some(|| BackendError::Provider {
                code: Some("provider_failure".into()),
                message: "provider execution cancelled".into(),
            }),
            ..backend(0)
        };
        let error = prompt(&backend, request(), None, &trace, &submission_store)
            .await
            .unwrap_err();
        assert!(!error.is::<crate::broker::TurnCancelled>());
        assert!(matches!(error.downcast_ref::<BackendError>(),
            Some(BackendError::Provider { code: Some(code), message })
                if code == "provider_failure" && message == "provider execution cancelled"));
        assert_eq!(backend.request.lock().unwrap().len(), 1);
        Ok(())
    }

    #[tokio::test(start_paused = true)]
    async fn cancellation_during_delay_never_dispatches_another_attempt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        trace.configure(true)?;
        let backend = backend(6);
        let (sink, mut stream) = crate::backend::events::channel(scope());
        {
            let pending = prompt(&backend, request(), Some(sink), &trace, &submission_store);
            tokio::pin!(pending);
            tokio::select! {
                result = &mut pending => panic!("retry settled before cancellation: {result:?}"),
                event = stream.recv() => {
                    assert!(matches!(event?.unwrap(), BackendEvent::Retry { update: Some(_), .. }));
                }
            }
        }
        tokio::time::advance(Duration::from_secs(120)).await;
        assert_eq!(backend.request.lock().unwrap().len(), 1);
        let records = std::fs::read_to_string(trace.session_status("retry-fixture").path)?;
        assert!(records.contains("\"status\":\"cancelled\""));
        Ok(())
    }
}
