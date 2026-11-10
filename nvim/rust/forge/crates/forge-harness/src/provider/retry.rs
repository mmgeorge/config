use crate::provider::ScopeOwner;
/// Host retry schedule for one admitted provider request.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
pub struct ProviderRetry {
    /// One-based retry number, excluding the initial attempt.
    pub attempt: usize,
    /// Maximum retries for this request.
    pub max_attempts: usize,
    /// Deadline for the delay, absent once the retry is running.
    pub retry_at_ms: Option<i64>,
}
use std::time::Duration;

use anyhow::Result;
use serde_json::{Value, json};

use super::{
    HarnessEventSink, HostEvent, ProviderCompletion, ProviderRequest, RetryEvent, TurnAddress,
};
use crate::trace::TraceStore;

const RETRY_DELAY_SECONDS: [u64; 5] = [1, 5, 30, 60, 300];
const CONTINUATION_PROMPT: &str = "Continue the previously accepted work from its current state. The preceding execution ended with a transient failure. Preserve completed work and tool effects; do not repeat the original user input or redo completed actions.";

/// Rejects replay after tools, control results, or model output have been observed.
pub(crate) fn replay_safe(output: &super::ProviderCompletion) -> bool {
    !output.descendant_work_admitted
        && output.host_file_changes.is_empty()
        && !output.evidence.tool_called
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

/// Close future retry admission independently of an already-owned native attempt.
#[derive(Clone)]
pub(crate) struct RetryAdmission {
    source: tokio::sync::watch::Receiver<bool>,
    closed: std::sync::Arc<std::sync::atomic::AtomicBool>,
    changed: std::sync::Arc<tokio::sync::Notify>,
    scope: std::sync::Arc<std::sync::Mutex<Option<super::InvocationScope>>>,
}

impl RetryAdmission {
    /// Create retry admission tied to the child cancellation owner.
    pub(crate) fn new(source: tokio::sync::watch::Receiver<bool>) -> Self {
        Self {
            source,
            closed: Default::default(),
            changed: Default::default(),
            scope: Default::default(),
        }
    }

    /// Prevent future attempts without discarding the currently admitted attempt's settlement waiter.
    pub(crate) fn close(&self) {
        self.closed
            .store(true, std::sync::atomic::Ordering::Release);
        self.changed.notify_waiters();
    }

    /// Get exact cancellation scope even while its transport handoff is still preparing.
    pub(crate) fn current_scope(&self) -> Option<super::InvocationScope> {
        self.scope
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
    }

    fn begin_attempt(&self, sink: &HarnessEventSink) -> Result<()> {
        let mut scope = self
            .scope
            .lock()
            .map_err(|_| anyhow::anyhow!("retry admission poisoned"))?;
        if self.is_closed() {
            return Err(crate::broker::TurnCancelled.into());
        }
        *scope = Some(sink.invocation_scope()?.clone());
        Ok(())
    }

    fn finish_attempt(&self) {
        self.scope
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
    }

    fn is_closed(&self) -> bool {
        self.closed.load(std::sync::atomic::Ordering::Acquire) || *self.source.borrow()
    }

    async fn wait_closed(&mut self) {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.is_closed() {
                return;
            }
            tokio::select! {
                () = changed => {},
                result = self.source.changed() => { if result.is_err() { return; } }
            }
        }
    }
}

/// Run child retry policy with closed admission observed before each attempt and during delays.
pub(crate) async fn execute_cancellable(
    engine: &super::session_execution::SessionExecution,
    request: ProviderRequest,
    event_sink: HarnessEventSink,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
    admission: RetryAdmission,
) -> Result<ProviderCompletion> {
    run_policy_guarded(
        request,
        event_sink,
        trace,
        Some(admission),
        |mut request, sink, initial| async move {
            if !initial {
                request.submission = Some(
                    submission_store
                        .prepare_continuation(&request, engine.provider_id())
                        .map_err(super::submission::journal_failure)?,
                );
            }
            engine.run_once(request, sink, submission_store).await
        },
    )
    .await
}

/// Runs one request with bounded retries only after an adapter proves safe terminal failure.
/// Preserve broker cancellation identity when a provider terminates the request intentionally.
/// Dropping this future cancels its delay. Native execution cleanup remains with the caller.
#[cfg(test)]
pub(crate) async fn execute(
    engine: &super::session_execution::SessionExecution,
    request: ProviderRequest,
    event_sink: HarnessEventSink,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
) -> Result<ProviderCompletion> {
    run_policy(
        request,
        event_sink,
        trace,
        |mut request, sink, initial| async move {
            if !initial {
                request.submission = Some(
                    submission_store
                        .prepare_continuation(&request, engine.provider_id())
                        .map_err(super::submission::journal_failure)?,
                );
            }
            engine.run_once(request, sink, submission_store).await
        },
    )
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
    event_sink: HarnessEventSink,
    trace: &TraceStore,
    submission_store: &std::sync::Arc<super::submission::SubmissionStore>,
    settlement: std::sync::Arc<super::session_execution::SettlementCell>,
    state: RetryState,
) -> Result<RetryAttempt> {
    let mut retained = settlement.checkout()?;
    let mut progress = RetryProgress {
        trace,
        session_id: request.harness_session_id.clone(),
        agent_id: request.owner()?.agent_id().to_string(),
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
        progress.record("failed", json!({"category":failure.category()}));
        let Some(delay) = retry_delay(progress.attempt, failure.retry_after()) else {
            progress.record("exhausted", json!({"category":failure.category()}));
            publish(&event_sink, address, None).await?;
            progress.pending = false;
            return Ok(RetryAttempt::Ready(Box::new(outcome)));
        };
        address = Some(failure.address().clone());
        author_continuation(&mut request);
        request.submission =
            Some(submission_store.prepare_continuation(&request, engine.provider_id())?);
        progress.attempt += 1;
        progress.pending = true;
        let retry_at_ms =
            now_ms().saturating_add(i64::try_from(delay.as_millis()).unwrap_or(i64::MAX));
        progress.record(
            "scheduled",
            json!({"delay_ms":u64::try_from(delay.as_millis()).unwrap_or(u64::MAX),"retry_at_ms":retry_at_ms}),
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
        tokio::time::sleep(delay).await;
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

#[cfg(test)]
pub(crate) async fn run_policy<F, Fut>(
    request: ProviderRequest,
    event_sink: HarnessEventSink,
    trace: &TraceStore,
    attempt: F,
) -> Result<ProviderCompletion>
where
    F: FnMut(ProviderRequest, HarnessEventSink, bool) -> Fut,
    Fut: std::future::Future<Output = Result<ProviderCompletion, super::ProviderError>>,
{
    run_policy_guarded(request, event_sink, trace, None, attempt).await
}

async fn run_policy_guarded<F, Fut>(
    mut request: ProviderRequest,
    mut event_sink: HarnessEventSink,
    trace: &TraceStore,
    mut admission: Option<RetryAdmission>,
    mut attempt: F,
) -> Result<ProviderCompletion>
where
    F: FnMut(ProviderRequest, HarnessEventSink, bool) -> Fut,
    Fut: std::future::Future<Output = Result<ProviderCompletion, super::ProviderError>>,
{
    let mut progress = RetryProgress {
        trace,
        session_id: request.harness_session_id.clone(),
        agent_id: request.owner()?.agent_id().to_string(),
        attempt: 0,
        pending: false,
    };
    let mut address = None;
    loop {
        if admission.as_ref().is_some_and(RetryAdmission::is_closed) {
            return Err(crate::broker::TurnCancelled.into());
        }
        if let Some(admission) = &admission {
            admission.begin_attempt(&event_sink)?;
        }
        let result = attempt(request.clone(), event_sink.clone(), progress.attempt == 0).await;
        if let Some(admission) = &admission {
            admission.finish_attempt();
        }
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
        progress.record("failed", json!({"category":failure.category()}));
        let Some(delay) = retry_delay(progress.attempt, failure.retry_after()) else {
            progress.record("exhausted", json!({"category":failure.category()}));
            publish(&event_sink, address.clone(), None).await?;
            progress.pending = false;
            return Err(error.into());
        };
        address = Some(failure.address().clone());
        author_continuation(&mut request);
        let previous_scope = event_sink.invocation_scope()?;
        let scope = super::InvocationScope::new(
            previous_scope.owner().clone(),
            previous_scope.exchange_id.clone(),
        );
        let successor_sink = event_sink.successor_invocation_sink(&scope)?;
        progress.attempt += 1;
        progress.pending = true;
        let retry_at_ms =
            now_ms().saturating_add(i64::try_from(delay.as_millis()).unwrap_or(i64::MAX));
        progress.record(
            "scheduled",
            json!({"delay_ms":u64::try_from(delay.as_millis()).unwrap_or(u64::MAX),"retry_at_ms":retry_at_ms}),
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
        if let Some(admission) = admission.as_mut() {
            tokio::select! {
                biased;
                () = admission.wait_closed() => return Err(crate::broker::TurnCancelled.into()),
                () = tokio::time::sleep(delay) => {}
            }
        } else {
            tokio::time::sleep(delay).await;
        }
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
        event_sink = successor_sink;
    }
}

/// Honor provider-required waits without shortening the host's retry schedule.
fn retry_delay(attempt: usize, required: Option<Duration>) -> Option<Duration> {
    RETRY_DELAY_SECONDS
        .get(attempt)
        .map(|seconds| Duration::from_secs(*seconds).max(required.unwrap_or_default()))
}

/// Author a new logical input while preserving effective configuration and existing task authority.
fn author_continuation(request: &mut ProviderRequest) {
    request.input = super::ProviderInput::from_text(CONTINUATION_PROMPT);
    request.submission = None;
    request.dispatch = super::DispatchTracker::default();
    request.recovered_input.clear();
    if let Some(context) = request.control_context.as_mut() {
        context.task_start = None;
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

/// Publish retry presentation only for the failed turn that owns the retained schedule.
async fn publish(
    sink: &HarnessEventSink,
    address: Option<TurnAddress>,
    retry: Option<ProviderRetry>,
) -> Result<()> {
    let Some(address) = address else {
        return Ok(());
    };
    sink.send_host(HostEvent::RetryUpdated {
        thread_id: address.thread_id,
        turn_id: address.turn_id,
        event: match retry {
            Some(retry) => RetryEvent::Scheduled(retry),
            None => RetryEvent::Cleared,
        },
    })
    .await?;
    Ok(())
}

pub(crate) fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
        .try_into()
        .unwrap_or(i64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::session_execution::OwnerObservation;
    use crate::provider::test_support::retry_prompt as prompt;
    use crate::provider::{HarnessEvent, HostEvent, RetryEvent};
    use crate::provider::{InvocationScope, PromptMode, ProviderInput, ThreadOwner};
    use forge_provider_api::ProviderError;
    use std::sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    };

    #[derive(Clone, Copy)]
    enum Ending {
        Completed,
        Cancelled,
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

    /// Return one positively observed native attempt result without publishing terminal events.
    async fn emit_attempt(
        request: forge_provider_api::PromptRequest,
        start: forge_provider_api::NativeTurnStart,
        attempt: usize,
        failure_count: usize,
        ending: Ending,
    ) -> Result<forge_provider_api::TurnResult, ProviderError> {
        use forge_provider_api::turn::{RetryDecision, TurnCompletionReason, TurnOutcome};
        start.observed(now_ms()).await?;
        let error = |kind| {
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                kind,
            )
        };
        let outcome = if attempt <= failure_count {
            TurnOutcome::Failed {
                error: error(forge_provider_api::ProviderFailureKind::Runtime),
                retry: RetryDecision::Allowed { retry_after: None },
            }
        } else {
            match ending {
                Ending::Completed => TurnOutcome::Completed {
                    stop_reason: TurnCompletionReason::Normal,
                },
                Ending::Cancelled => TurnOutcome::Interrupted,
            }
        };
        forge_provider_api::TurnResult::new(
            outcome,
            now_ms(),
            None,
            vec![forge_provider_api::CoveredInput::new(
                request.submission_id,
                request.attempt_id,
            )],
            vec![],
            None,
        )
        .map_err(|_| error(forge_provider_api::ProviderFailureKind::Protocol).into())
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
            move |request, _sink, start, _cancelled| {
                let log = initial_log.clone();
                async move {
                    let attempt = log.prompt_count.fetch_add(1, Ordering::SeqCst) + 1;
                    if attempt > 1 {
                        log.continuation_count.fetch_add(1, Ordering::SeqCst);
                        assert_eq!(
                            request.input,
                            forge_provider_api::forge::TurnInput::from(CONTINUATION_PROMPT)
                        );
                    }
                    *log.selected_model.lock().unwrap() = request.configuration.model.model.clone();
                    log.attempts
                        .lock()
                        .unwrap()
                        .push((attempt, tokio::time::Instant::now()));
                    emit_attempt(request, start, attempt, failure_count, ending).await
                }
            },
        );
        (provider, log)
    }

    fn scope() -> InvocationScope {
        InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(ThreadOwner::root("retry-fixture")),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn request() -> ProviderRequest {
        ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
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
            acknowledged_child_reports: Vec::new(),
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
            assert!(
                matches!(
                    error.downcast_ref::<ProviderError>(),
                    Some(ProviderError::Cancelled)
                ),
                "{error:#}"
            );
            assert_eq!(log.prompt_count.load(Ordering::SeqCst), failure_count + 1);
            assert_eq!(log.continuation_count.load(Ordering::SeqCst), failure_count);
            let mut retry_updates = Vec::new();
            let mut retry_scopes = Vec::new();
            while let Some(event) = stream.try_next()? {
                if let OwnerObservation::Event(HarnessEvent::Host {
                    scope,
                    event: HostEvent::RetryUpdated { event, .. },
                }) = event
                {
                    let scope = scope
                        .invocation()
                        .expect("retry retains invocation scope")
                        .clone();
                    let update = match event {
                        RetryEvent::Scheduled(retry) => Some(retry),
                        RetryEvent::Cleared => None,
                    };
                    retry_scopes.push(scope);
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
                assert_eq!(retry_scopes[0], scope());
                assert_eq!(retry_scopes[1], scope());
                assert_ne!(retry_scopes[2].invocation_id, scope().invocation_id);
                assert_eq!(retry_scopes[2].owner(), scope().owner());
                assert_eq!(retry_scopes[2].exchange_id, scope().exchange_id);
            }
        }
        Ok(())
    }

    #[tokio::test(start_paused = true)]
    async fn cancellation_during_delay_never_dispatches_another_attempt() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = std::sync::Arc::new(TraceStore::open(directory.path())?);
        let submission_store =
            super::super::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        trace.configure(crate::trace::LogLevel::Trace)?;
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
                        if matches!(event?.expect("retry stream remains open"), OwnerObservation::Event(HarnessEvent::Host { event: HostEvent::RetryUpdated { event: RetryEvent::Scheduled(_), .. }, .. })) { break; }
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
