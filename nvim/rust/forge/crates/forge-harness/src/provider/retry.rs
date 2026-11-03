pub use forge_provider_api::retry::ProviderRetry;
use std::time::Duration;

use anyhow::Result;
use serde_json::{Value, json};

use super::{ProviderCompletion, ProviderEvent, ProviderEventSink, ProviderRequest, TurnAddress};
use crate::trace::TraceStore;

const RETRY_DELAY_SECONDS: [u64; 5] = [1, 5, 10, 30, 60];

/// Rejects replay after tools, control results, or model output have been observed.
pub(crate) fn replay_safe(output: &super::ProviderCompletion) -> bool {
    !output.evidence.tool_called
        && !output.evidence.workspace_changed
        && output.plan_edit.is_empty()
        && output.design_patch.is_empty()
        && output.plan_submit.is_none()
        && output.plan_question.is_none()
        && output.plan_read.is_none()
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
pub(crate) async fn execute(
    engine: &super::session_execution::SessionExecution,
    request: ProviderRequest,
    event_sink: ProviderEventSink,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
) -> Result<ProviderCompletion> {
    run_policy(request, event_sink, trace, |request, sink, _| async move {
        engine.run_once(request, sink, submission_store).await
    })
    .await
}

/// Retry safe native failures while retaining one host invocation and its effects.
pub(crate) enum RetryAttempt {
    Ready(Box<Result<ProviderCompletion, super::ProviderError>>),
    Stale(Box<ProviderRequest>, RetryState),
}

#[derive(Default)]
pub(crate) struct RetryState {
    attempt: usize,
    pending: bool,
    address: Option<TurnAddress>,
}

/// A stale attempt has not crossed the native admission boundary.
pub(crate) async fn execute_until_ready(
    engine: &super::session_execution::SessionExecution,
    mut request: ProviderRequest,
    event_sink: ProviderEventSink,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
    settlement: std::sync::Arc<super::session_execution::SettlementCell>,
    state: RetryState,
) -> Result<RetryAttempt> {
    let mut retained = settlement.checkout()?;
    let mut progress = RetryProgress {
        trace,
        session_id: request.harness_session_id.clone(),
        agent_id: request.owner()?.agent_id.to_string(),
        attempt: state.attempt,
        pending: state.pending,
    };
    let mut address = state.address;
    loop {
        let ready = match retained.ticket() {
            Some(ticket) => {
                match engine
                    .continue_attempt(
                        ticket,
                        request.clone(),
                        event_sink.clone(),
                        submission_store,
                    )
                    .await?
                {
                    super::session_execution::ContinuationAttempt::Ready(ready) => *ready,
                    super::session_execution::ContinuationAttempt::Stale(request) => {
                        let state = RetryState {
                            attempt: progress.attempt,
                            pending: progress.pending,
                            address,
                        };
                        progress.pending = false;
                        return Ok(RetryAttempt::Stale(request, state));
                    }
                }
            }
            None => {
                engine
                    .run_until_ready(request.clone(), event_sink.clone(), submission_store)
                    .await?
            }
        };
        let super::session_execution::NativeReady {
            outcome,
            ticket,
            batches,
        } = ready;
        settlement.set_batches(batches)?;
        retained.replace(ticket);
        let failure = match &outcome {
            Err(super::ProviderError::Retryable(failure)) => failure,
            _ => {
                if progress.attempt > 0 {
                    progress.record(
                        if outcome.is_ok() {
                            "recovered"
                        } else {
                            "stopped"
                        },
                        Value::Null,
                    );
                    publish(&event_sink, address, None).await?;
                }
                progress.pending = false;
                return Ok(RetryAttempt::Ready(Box::new(outcome)));
            }
        };
        progress.record("failed", json!({"category":failure.category}));
        let Some(delay_seconds) = RETRY_DELAY_SECONDS.get(progress.attempt).copied() else {
            progress.record("exhausted", json!({"category":failure.category}));
            publish(&event_sink, address, None).await?;
            progress.pending = false;
            return Ok(RetryAttempt::Ready(Box::new(outcome)));
        };
        address = Some(failure.address.clone());
        request
            .control_context
            .get_or_insert_with(|| crate::tools::ControlTurnContext::inactive(request.mode))
            .provider_continuation = Some(failure.continuation.clone());
        request.submission = None;
        request.recovered_input.clear();
        progress.attempt += 1;
        progress.pending = true;
        let retry_at_ms =
            now_ms().saturating_add(i64::try_from(delay_seconds.saturating_mul(1000))?);
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

pub(crate) async fn run_policy<F, Fut>(
    mut request: ProviderRequest,
    event_sink: ProviderEventSink,
    trace: &TraceStore,
    mut attempt: F,
) -> Result<ProviderCompletion>
where
    F: FnMut(ProviderRequest, ProviderEventSink, bool) -> Fut,
    Fut: std::future::Future<Output = Result<ProviderCompletion, super::ProviderError>>,
{
    let mut progress = RetryProgress {
        trace,
        session_id: request.harness_session_id.clone(),
        agent_id: request.owner()?.agent_id.to_string(),
        attempt: 0,
        pending: false,
    };
    let mut address = None;
    loop {
        let result = attempt(request.clone(), event_sink.clone(), progress.attempt == 0).await;
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
        let super::ProviderError::Retryable(failure) = &error else {
            if progress.attempt > 0 {
                progress.record("stopped", json!({"error":format!("{error:#}")}));
                publish(&event_sink, address.clone(), None).await?;
            }
            progress.pending = false;
            if matches!(error, super::ProviderError::Cancelled) {
                return Err(crate::broker::TurnCancelled.into());
            }
            return Err(error.into());
        };
        progress.record("failed", json!({"category":failure.category}));
        let Some(delay_seconds) = RETRY_DELAY_SECONDS.get(progress.attempt).copied() else {
            progress.record("exhausted", json!({"category":failure.category}));
            publish(&event_sink, address.clone(), None).await?;
            progress.pending = false;
            return Err(error.into());
        };
        address = Some(failure.address.clone());
        request
            .control_context
            .get_or_insert_with(|| crate::tools::ControlTurnContext::inactive(request.mode))
            .provider_continuation = Some(failure.continuation.clone());
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
    sink: &ProviderEventSink,
    _address: Option<TurnAddress>,
    retry: Option<ProviderRetry>,
) -> Result<()> {
    sink.send_wait(ProviderEvent::Retry {
        scope: sink.invocation_scope()?.clone(),
        update: retry,
    })
    .await?;
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
    use crate::provider::test_support::retry_prompt as prompt;
    use crate::provider::{ExecutionOwner, InvocationScope, PromptMode, ProviderInput};
    use forge_provider_api::{
        ProviderError, ProviderEvent, TurnAddress,
        completion::ProviderCompletion as ApiCompletion,
        events::ProviderEventSink as ApiEventSink,
        lifecycle::{ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate},
        retry::RetryCategory,
    };
    use std::sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    };

    #[derive(Clone, Copy)]
    enum Ending {
        Completed,
        Cancelled,
        Rejected,
    }

    struct ScriptLog {
        prompt_count: AtomicUsize,
        continuation_count: AtomicUsize,
        attempts: Mutex<Vec<(usize, tokio::time::Instant)>>,
        selected_model: Mutex<Option<String>>,
    }

    fn descriptor() -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a fixture identity"),
            label: "Generic fixture".into(),
            capability: Default::default(),
        }
    }

    async fn emit_attempt(
        scope: InvocationScope,
        sink: ApiEventSink,
        attempt: usize,
        failure_count: usize,
        ending: Ending,
        covered: Vec<forge_provider_api::SubmissionId>,
    ) -> Result<(), ProviderError> {
        let address = TurnAddress {
            scope: scope.clone(),
            thread_id: "fixture-session".into(),
            turn_id: format!("turn-{attempt}").into(),
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(
                scope.clone(),
                ExecutionUpdate::Started {
                    address: address.clone(),
                },
            )?,
        })
        .await?;
        let completion = ApiCompletion::default();
        let result = if attempt <= failure_count {
            ExecutionResult::Failed {
                completion: completion.into(),
                failure: ExecutionFailure::Retryable {
                    address: address.clone(),
                    category: RetryCategory::TemporaryCapacity,
                    continuation: forge_provider_api::ContinuationToken::new(
                        format!("retry-{attempt}").into_bytes(),
                    )?,
                },
            }
        } else {
            match ending {
                Ending::Completed => ExecutionResult::Completed {
                    completion: completion.into(),
                },
                Ending::Cancelled => ExecutionResult::Cancelled {
                    completion: completion.into(),
                },
                Ending::Rejected => ExecutionResult::Failed {
                    completion: completion.into(),
                    failure: ExecutionFailure::Terminal {
                        kind: forge_provider_api::ProviderFailureKind::Rejected,
                    },
                },
            }
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(
                scope,
                ExecutionUpdate::Finished {
                    address,
                    covered_submissions: covered,
                    completed_at_ms: now_ms(),
                    result,
                },
            )?,
        })
        .await?;
        Ok(())
    }

    fn provider(
        failure_count: usize,
        ending: Ending,
    ) -> (
        Arc<crate::provider::test_support::EventFixtureProvider>,
        Arc<ScriptLog>,
    ) {
        let log = Arc::new(ScriptLog {
            prompt_count: AtomicUsize::new(0),
            continuation_count: AtomicUsize::new(0),
            attempts: Mutex::new(Vec::new()),
            selected_model: Mutex::new(None),
        });
        let initial_log = log.clone();
        let provider = crate::provider::test_support::EventFixtureProvider::new(
            descriptor(),
            move |request, sink, _cancelled| {
                let log = initial_log.clone();
                async move {
                    let attempt = log.prompt_count.fetch_add(1, Ordering::SeqCst) + 1;
                    *log.selected_model.lock().unwrap() = request.configuration.model.model.clone();
                    log.attempts
                        .lock()
                        .unwrap()
                        .push((attempt, tokio::time::Instant::now()));
                    emit_attempt(
                        request.scope,
                        sink,
                        attempt,
                        failure_count,
                        ending,
                        vec![request.submission_id],
                    )
                    .await
                }
            },
        );
        let continuation_log = log.clone();
        provider.with_continuation(move |scope, _callbacks, sink, _cancelled| {
            let log = continuation_log.clone();
            async move {
                let attempt = log.continuation_count.fetch_add(1, Ordering::SeqCst) + 2;
                log.attempts
                    .lock()
                    .unwrap()
                    .push((attempt, tokio::time::Instant::now()));
                emit_attempt(scope, sink, attempt, failure_count, ending, Vec::new()).await
            }
        });
        (provider, log)
    }

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ExecutionOwner::root("retry-fixture"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn request() -> ProviderRequest {
        ProviderRequest {
            execution_owner: None,
            harness_session_id: "retry-fixture".into(),
            workspace: "workspace".into(),
            input: ProviderInput::from_text("original request"),
            mode: PromptMode::Chat,
            model: "selected-model".into(),
            effort: "high".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    #[test]
    fn replay_rejects_tools_and_control_mutations() {
        let mut output = ProviderCompletion::default();
        assert!(replay_safe(&output));
        output.evidence.tool_called = true;
        assert!(!replay_safe(&output));
        output = ProviderCompletion::default();
        output.plan_read = Some("plan".into());
        assert!(!replay_safe(&output));
        output = ProviderCompletion::default();
        output.evidence.workspace_changed = true;
        assert!(!replay_safe(&output));
        output = ProviderCompletion::default();
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
        let (native, log) = provider(6, Ending::Completed);
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let error = prompt(&provider, request(), None, &trace, &submission_store)
            .await
            .unwrap_err();
        assert!(
            matches!(error.downcast_ref::<ProviderError>(), Some(ProviderError::Retryable(failure))
            if failure.category == RetryCategory::TemporaryCapacity)
        );
        let attempts = log.attempts.lock().unwrap();
        assert_eq!(log.prompt_count.load(Ordering::SeqCst), 1);
        assert_eq!(log.continuation_count.load(Ordering::SeqCst), 5);
        assert_eq!(attempts.len(), 6);
        for (index, delay) in RETRY_DELAY_SECONDS.iter().enumerate() {
            assert_eq!(
                attempts[index + 1].1.duration_since(attempts[index].1),
                Duration::from_secs(*delay)
            );
        }
        assert_eq!(
            log.selected_model.lock().unwrap().as_deref(),
            Some("selected-model")
        );
        trace.flush()?;
        let records = std::fs::read_to_string(trace.session_status("retry-fixture").path)?;
        assert!(!records.contains("serverOverloaded"));
        assert!(!records.contains("capacity failure"));
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
        let (recovered_native, recovered_log) = provider(1, Ending::Completed);
        let recovered_provider =
            crate::provider::test_support::ProviderTestSession::from_native(recovered_native)?;
        let (sink, mut stream) = crate::provider::events::channel(scope());
        prompt(
            &recovered_provider,
            request(),
            Some(sink),
            &trace,
            &submission_store,
        )
        .await?;
        let mut retries = Vec::new();
        while let Some(event) = stream.try_next()? {
            if let ProviderEvent::Retry { update, .. } = event {
                retries.push(update);
            }
        }
        assert_eq!(retries.len(), 3);
        let waiting = retries[0].as_ref().expect("scheduled retry");
        assert_eq!(waiting.attempt, 1);
        assert!(waiting.retry_at_ms.is_some());
        assert!(
            retries[1]
                .as_ref()
                .is_some_and(|retry| retry.retry_at_ms.is_none())
        );
        assert!(retries[2].is_none());
        assert_eq!(recovered_log.continuation_count.load(Ordering::SeqCst), 1);
        let (native, log) = provider(0, Ending::Rejected);
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let error = prompt(&provider, request(), None, &trace, &submission_store)
            .await
            .unwrap_err();
        assert!(
            matches!(error.downcast_ref::<ProviderError>(), Some(ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)
        );
        assert_eq!(log.prompt_count.load(Ordering::SeqCst), 1);
        assert_eq!(log.continuation_count.load(Ordering::SeqCst), 0);
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
            let (native, log) = provider(failure_count, Ending::Cancelled);
            let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
            let (sink, mut stream) = crate::provider::events::channel(scope());
            let error = prompt(&provider, request(), Some(sink), &trace, &submission_store)
                .await
                .unwrap_err();
            assert!(error.is::<crate::broker::TurnCancelled>(), "{error:#}");
            assert_eq!(log.prompt_count.load(Ordering::SeqCst), 1);
            assert_eq!(log.continuation_count.load(Ordering::SeqCst), failure_count);
            let mut retry_updates = Vec::new();
            while let Some(event) = stream.try_next()? {
                if let ProviderEvent::Retry { update, .. } = event {
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
        let (native, log) = provider(0, Ending::Rejected);
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let error = prompt(&provider, request(), None, &trace, &submission_store)
            .await
            .unwrap_err();
        assert!(!error.is::<crate::broker::TurnCancelled>());
        assert!(
            matches!(error.downcast_ref::<ProviderError>(), Some(ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)
        );
        assert_eq!(log.prompt_count.load(Ordering::SeqCst), 1);
        Ok(())
    }

    #[tokio::test(start_paused = true)]
    async fn cancellation_during_delay_never_dispatches_another_attempt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        trace.configure(true)?;
        let (native, log) = provider(6, Ending::Completed);
        let provider = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let (sink, mut stream) = crate::provider::events::channel(scope());
        {
            let pending = prompt(&provider, request(), Some(sink), &trace, &submission_store);
            tokio::pin!(pending);
            loop {
                tokio::select! {
                    result = &mut pending => panic!("retry settled before cancellation: {result:?}"),
                    event = stream.recv() => {
                        if matches!(event?.expect("retry stream remains open"), ProviderEvent::Retry { update: Some(_), .. }) { break; }
                    }
                }
            }
        }
        tokio::time::advance(Duration::from_secs(120)).await;
        assert_eq!(log.prompt_count.load(Ordering::SeqCst), 1);
        assert_eq!(log.continuation_count.load(Ordering::SeqCst), 0);
        trace.flush()?;
        let records = std::fs::read_to_string(trace.session_status("retry-fixture").path)?;
        assert!(records.contains("\"status\":\"cancelled\""));
        Ok(())
    }
}
