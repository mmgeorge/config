use std::{fmt, time::Duration};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use super::{
    Backend, BackendEvent, BackendEventSink, BackendInput, BackendOutput, BackendRequest,
    ProviderAddress,
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
pub(crate) struct RetryableFailure {
    /// Completed native turn that retains the original request in provider history.
    pub address: ProviderAddress,
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
pub(crate) fn replay_safe(output: &BackendOutput) -> bool {
    !output.evidence.tool_called
        && !output.evidence.workspace_changed
        && !output.event.iter().any(|event| {
            event.activity.is_some()
                || event.checklist_update.is_some()
                || matches!(
                    event.kind.as_str(),
                    "assistant_message" | "reasoning" | "reasoning_summary" | "plan"
                ) && event.text.as_ref().is_some_and(|text| !text.is_empty())
        })
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
/// Dropping this future cancels its delay. Native execution cleanup remains with the caller.
pub(crate) async fn prompt(
    backend: &dyn Backend,
    mut request: BackendRequest,
    event_sink: Option<BackendEventSink>,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
) -> Result<BackendOutput> {
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
                Some(sink) => backend
                    .prompt_stream(request.clone(), sink)
                    .await
                    .map(super::BackendCompletion::into_output),
                None => backend.prompt(request.clone()).await,
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
        let Some(failure) = error.downcast_ref::<RetryableFailure>() else {
            if progress.attempt > 0 {
                progress.record("stopped", json!({"error":format!("{error:#}")}));
                publish(&event_sink, address.clone(), None).await?;
            }
            progress.pending = false;
            return Err(error);
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
            return Err(error);
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
) -> Result<BackendOutput> {
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
        Some(sink) => backend
            .prompt_stream(request.clone(), sink)
            .await
            .map(super::BackendCompletion::into_output),
        None => backend.prompt(request.clone()).await,
    };
    if result.is_ok()
        || result
            .as_ref()
            .is_err_and(|error| error.is::<RetryableFailure>())
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
    address: Option<ProviderAddress>,
    retry: Option<ProviderRetry>,
) -> Result<()> {
    if let Some(sink) = sink {
        sink.send_wait(BackendEvent {
            message: None,
            received_at_ms: None,
            address,
            turn_boundary: None,
            kind: "provider_retry".into(),
            text: None,
            data: serde_json::to_value(retry).context("encode provider retry state")?,
            activity: None,
            summary: None,
            checklist_update: None,
        })
        .await?;
    }
    Ok(())
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{BackendDescriptor, BackendKind, PromptMode};
    use std::sync::Mutex;

    struct FailureBackend {
        failure_count: usize,
        permanent: bool,
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
            _: BackendEventSink,
        ) -> Result<crate::backend::BackendCompletion> {
            let output: anyhow::Result<crate::backend::BackendOutput> = async {
                let mut requests = self.request.lock().unwrap();
                requests.push((request, tokio::time::Instant::now()));
                if self.permanent {
                    anyhow::bail!("unauthorized");
                }
                if requests.len() <= self.failure_count {
                    return Err(RetryableFailure {
                        address: ProviderAddress {
                            owner: None,
                            thread_id: "retained-thread".into(),
                            turn_id: format!("turn-{}", requests.len()),
                        },
                        code: "serverOverloaded".into(),
                        detail: json!({"message":format!("capacity failure {}", requests.len())}),
                        message: format!("capacity failure {}", requests.len()),
                    }
                    .into());
                }
                Ok(BackendOutput::default())
            }
            .await;
            Ok(output?.into_completion())
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
            permanent: false,
            request: Mutex::new(Vec::new()),
        }
    }

    #[test]
    fn replay_rejects_model_output_tools_and_control_mutations() {
        let mut output = BackendOutput::default();
        assert!(replay_safe(&output));
        output.evidence.tool_called = true;
        assert!(!replay_safe(&output));
        output = BackendOutput::default();
        output.plan_read = Some("plan".into());
        assert!(!replay_safe(&output));
        output = BackendOutput::default();
        output.event.push(BackendEvent {
            message: None,
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("Partial response".into()),
            data: Value::Null,
            activity: None,
            summary: None,
            checklist_update: None,
        });
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
        let (sink, mut stream) = crate::backend::events::channel();
        prompt(
            &recovered_backend,
            request(),
            Some(sink),
            &trace,
            &submission_store,
        )
        .await?;
        let waiting = stream.try_recv()?;
        assert_eq!(waiting.kind, "provider_retry");
        assert_eq!(waiting.data["attempt"], 1);
        assert!(waiting.data["retry_at_ms"].is_i64());
        assert!(stream.try_recv()?.data["retry_at_ms"].is_null());
        assert!(stream.try_recv()?.data.is_null());
        let backend = FailureBackend {
            permanent: true,
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

    #[tokio::test(start_paused = true)]
    async fn cancellation_during_delay_never_dispatches_another_attempt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        trace.configure(true)?;
        let backend = backend(6);
        let (sink, mut stream) = crate::backend::events::channel();
        {
            let pending = prompt(&backend, request(), Some(sink), &trace, &submission_store);
            tokio::pin!(pending);
            tokio::select! {
                result = &mut pending => panic!("retry settled before cancellation: {result:?}"),
                event = stream.recv() => { assert_eq!(event?.unwrap().kind, "provider_retry"); }
            }
        }
        tokio::time::advance(Duration::from_secs(120)).await;
        assert_eq!(backend.request.lock().unwrap().len(), 1);
        let records = std::fs::read_to_string(trace.session_status("retry-fixture").path)?;
        assert!(records.contains("\"status\":\"cancelled\""));
        Ok(())
    }
}
