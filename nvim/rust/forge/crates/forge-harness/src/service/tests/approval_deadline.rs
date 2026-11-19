use super::super::{HarnessBroker, RuntimeMethod, SessionController, prompt};
use super::bootstrap::initialize;
use crate::storage::SqliteStore;
use anyhow::{Context, Result, ensure};
use forge_protocol::message::Request;
use forge_provider_api::permissions::{
    PermissionRequest, PermissionResponse, PermissionSubject, PermissionTarget,
};
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

/// Native item identity of the command whose approval outlasts the tool timeout.
const COMMAND_ITEM: &str = "approval-gated-command";

/// Run the first turn's approval-gated command: announce it as running, as Codex does, then ask
/// to run it and complete it with the approval's outcome.
async fn run_gated_command(
    request: &forge_provider_api::PromptRequest,
    sink: &forge_provider_api::AdapterActivityWriter,
) -> std::result::Result<(), forge_provider_api::ProviderError> {
    run_command_requesting(
        request,
        sink,
        PermissionTarget::Tool {
            target: "fixture-approval".into(),
        },
    )
    .await
}

/// Start the fixture command, ask permission for `target`, and complete it with the answer.
async fn run_command_requesting(
    request: &forge_provider_api::PromptRequest,
    sink: &forge_provider_api::AdapterActivityWriter,
    target: PermissionTarget,
) -> std::result::Result<(), forge_provider_api::ProviderError> {
    let item_id = forge_provider_api::ItemId::new(COMMAND_ITEM).expect("fixture item ID is valid");
    let tool_event = |event| forge_provider_api::AdapterActivity::Item {
        thread_id: request.thread_id.clone(),
        turn_id: request.turn_id.clone(),
        item_id: item_id.clone(),
        event,
    };
    for event in [
        forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::CommandExecution(
            forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "shell".into(),
                title: "Approval-gated fixture command".into(),
                arguments: forge_provider_api::InspectableArguments::new(
                    json!({"command":"fixture"}),
                ),
            },
        )),
        forge_provider_api::ItemEvent::Tool {
            event: forge_provider_api::ToolItemEvent::ExecutionStarted,
        },
    ] {
        sink.send_wait(tool_event(event)).await?;
    }
    // Open the approval after the tool deadline starts, as Codex does, so the unsuspended tool
    // deadline would expire before the approval timeout.
    tokio::time::sleep(Duration::from_secs(1)).await;
    let response = request
        .permissions
        .authorize(
            &request.scope,
            PermissionRequest {
                id: "fixture-approval".into(),
                provider: forge_providers::registry::nonselectable_id().unwrap(),
                subject: PermissionSubject::Tool(item_id.clone()),
                reason: None,
                target_list: vec![target],
            },
        )
        .await;
    let outcome = match response {
        PermissionResponse::Allow => crate::provider::ToolOutcome::Succeeded,
        PermissionResponse::Deny { .. } | PermissionResponse::Cancelled => {
            crate::provider::ToolOutcome::Denied {
                reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                    "fixture approval was not granted",
                )
                .expect("static diagnostic is valid"),
            }
        }
    };
    sink.send_wait(tool_event(forge_provider_api::ItemEvent::Completed(
        forge_provider_api::ItemCompletion::CommandExecution(forge_provider_api::ToolCompletion {
            outcome,
            descriptor: None,
            output: None,
            changes: Default::default(),
        }),
    )))
    .await?;
    Ok(())
}

/// Complete a fixture turn normally, covering its own submission and any input steered into it.
fn completed_turn(
    request: &forge_provider_api::PromptRequest,
    steered: Vec<forge_provider_api::CoveredInput>,
) -> std::result::Result<forge_provider_api::TurnResult, forge_provider_api::ProviderError> {
    finished_turn(
        request,
        steered,
        forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        },
    )
}

/// End a fixture turn with `outcome`, covering its own submission and any input steered into it.
fn finished_turn(
    request: &forge_provider_api::PromptRequest,
    steered: Vec<forge_provider_api::CoveredInput>,
    outcome: forge_provider_api::turn::TurnOutcome,
) -> std::result::Result<forge_provider_api::TurnResult, forge_provider_api::ProviderError> {
    let mut coverage = vec![forge_provider_api::CoveredInput::new(
        request.submission_id.clone(),
        request.attempt_id.clone(),
    )];
    coverage.extend(steered);
    forge_provider_api::TurnResult::new(outcome, 2, None, coverage, Vec::new(), None).map_err(
        |_| {
            crate::provider::ProviderError::Observation(
                forge_provider_api::ObservationFailure::Protocol,
            )
        },
    )
}

/// Start a controller over the fixture provider, with the session services that include the
/// approval-timeout watcher that dispatches the continuation.
pub(super) async fn start_controller(
    root: &tempfile::TempDir,
    native: Arc<crate::provider::test_support::EventFixtureProvider>,
) -> Result<(
    Arc<SessionController>,
    crate::provider::test_support::ProviderTestSession,
    forge_protocol::outbound::MessageSender,
    tokio::task::JoinHandle<Result<()>>,
)> {
    native.with_models(vec![serde_json::from_value(json!({
        "id":"fixture-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let mut broker = HarnessBroker::initialize(initialize(root, "mock"))?;
    let provider =
        crate::provider::test_support::ProviderTestSession::from_native_with_permissions(
            native,
            broker.permission_coordinator(),
        )?;
    broker.set_provider_for_test(provider.provider_handle());
    let controller = SessionController::new(broker).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_session_events(&sink).await?;
    controller.attach_input(&sink).await?;
    Ok((controller, provider, sink, reader))
}

/// Stop the controller's owner, provider, and session services after a scenario.
pub(super) async fn stop_controller(
    controller: Arc<SessionController>,
    provider: crate::provider::test_support::ProviderTestSession,
    reader: tokio::task::JoinHandle<Result<()>>,
) -> Result<()> {
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    // The paused clock runs this bound out at once when a wedged owner never shuts down.
    let teardown = tokio::time::timeout(Duration::from_secs(3600), async {
        controller.finish_exchange_owner().await?;
        provider.provider_handle().shutdown().await?;
        controller.finish_session_events().await
    })
    .await
    .context("the session owner never shut down")
    .and_then(|teardown| teardown);
    reader.abort();
    drop(controller);
    teardown
}

/// Run one scenario whose first turn asks for the gated command and ends with `first_outcome`.
///
/// Returns the controller's settled exchanges, every later turn's input kind as the provider
/// received it, and the root timeline's projected rows.
async fn run_expiring_approval(
    first_outcome: forge_provider_api::turn::TurnOutcome,
) -> Result<(
    Vec<crate::exchange::Exchange>,
    Vec<forge_provider_api::forge::TurnKind>,
    Vec<String>,
)> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (continued, mut continuations) = tokio::sync::mpsc::unbounded_channel();
    let runs = Arc::new(AtomicUsize::new(0));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| {
            let first = runs.fetch_add(1, Ordering::AcqRel) == 0;
            let continued = continued.clone();
            async move {
                start.observed(1).await?;
                if first {
                    run_gated_command(&request, &sink).await?;
                    return finished_turn(&request, Vec::new(), first_outcome);
                }
                let _ = continued.send(request.input.kind().clone());
                completed_turn(&request, Vec::new())
            }
        },
    );
    let (controller, provider, sink, reader) = start_controller(&root, native).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 900,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"approval-deadline","text":"Run the gated command"}),
            },
            &sink,
        )
        .await?;
        // The paused clock runs both bounds out at once: the first waits for a continuation that
        // may never come, and the second lets the timeout watcher act on whatever feedback is
        // left before the scenario reads the settled history.
        let mut kinds = Vec::new();
        if let Ok(Some(kind)) =
            tokio::time::timeout(Duration::from_secs(3600), continuations.recv()).await
        {
            kinds.push(kind);
        }
        tokio::time::sleep(Duration::from_secs(3600)).await;
        while let Ok(kind) = continuations.try_recv() {
            kinds.push(kind);
        }
        let exchanges = SqliteStore::open(&controller.data_root)?
            .list_exchange(controller.observer.session_id())?;
        let mut rows = Vec::new();
        for entry in &controller.observer.snapshot()?.timeline {
            if matches!(entry, crate::timeline::TimelineEntry::Exchange { .. }) {
                rows.extend(crate::buffer::projection::tests::project_rows(entry)?);
            }
        }
        Ok::<_, anyhow::Error>((exchanges, kinds, rows))
    }
    .await;
    let teardown = stop_controller(controller, provider, reader).await;
    drop(root);
    let observed = scenario?;
    teardown?;
    Ok(observed)
}

/// A command whose approval outlasts the tool timeout times out the approval, not the turn, and
/// a turn that completes normally after the expiry starts no continuation.
///
/// Codex announces a command as running before it asks to run it, so the tool deadline starts
/// before the approval opens. Both timeouts default to ten minutes, and the tool deadline used to
/// fire first: it interrupted the turn, cancelled the approval as `TurnInterrupted`, and failed
/// the exchange. The approval's own timeout must decide instead. The provider receives the
/// expiry as the tool's rejection inside the running turn, and once that turn completes
/// normally the model has answered it: a permission-timeout continuation used to start anyway,
/// draw "An approval timed out" after the answer, and make the model repeat itself. The expiry
/// stays in the transcript beside the request instead.
#[tokio::test(start_paused = true)]
async fn approval_expiring_inside_a_completed_turn_starts_no_continuation() -> Result<()> {
    let (exchanges, kinds, rows) =
        run_expiring_approval(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await?;
    let exchange = exchanges
        .iter()
        .find(|exchange| exchange.ordinal == 1)
        .context("the gated command lost its exchange")?;
    ensure!(
        exchange.state() == crate::exchange::ExchangeState::Complete,
        "the tool timeout ended the gated turn as {:?}",
        exchange.state()
    );
    let resolution = serde_json::to_value(&exchange.approval_resolution)?;
    ensure!(
        resolution[0]["Resolved"]["targets"][0]["source"] == "Timeout",
        "the approval did not end through its own timeout: {resolution}"
    );
    ensure!(
        resolution[0]["Resolved"]["scope"]["Item"]["item_id"] == COMMAND_ITEM,
        "the approval was not scoped to its tool item: {resolution}"
    );
    ensure!(
        kinds.is_empty() && exchanges.len() == 1,
        "a completed turn's expiry started more turns: {kinds:?}"
    );
    let notice = rows
        .iter()
        .position(|row| row.contains("An approval timed out"))
        .with_context(|| format!("the expiry left no transcript row: {rows:#?}"))?;
    let tool = rows
        .iter()
        .position(|row| row.contains("Approval-gated fixture command"))
        .with_context(|| format!("the gated command left no tool row: {rows:#?}"))?;
    ensure!(
        tool < notice,
        "the expiry row is not beside its request: {rows:#?}"
    );
    Ok(())
}

/// A turn interrupted after the expiry continues, and the continuation says the operation did
/// not run, is not approved, and must not be performed by another route.
///
/// The guidance used to say only that the expiry "was not an explicit user rejection", and a
/// model read that as permission to run the refused command through a terminal MCP tool.
#[tokio::test(start_paused = true)]
async fn interrupted_turn_continues_with_guidance_that_forbids_other_routes() -> Result<()> {
    let (_, kinds, _) =
        run_expiring_approval(forge_provider_api::turn::TurnOutcome::Interrupted).await?;
    let [forge_provider_api::forge::TurnKind::PermissionTimeout { feedback }] = kinds.as_slice()
    else {
        anyhow::bail!("the interrupted turn's expiry dispatched {kinds:?}");
    };
    for statement in [
        "fixture-approval",
        "the operation did not run",
        "It is not approved",
        "another shell, a terminal or MCP tool",
        "Continue the parts of the user's request that do not depend on it",
        "still needs the user's approval",
    ] {
        ensure!(
            feedback.contains(statement),
            "the continuation omitted {statement:?}: {feedback}"
        );
    }
    ensure!(
        !feedback.contains("not an explicit user rejection") && !feedback.contains("\nRequest\n"),
        "the continuation kept the old guidance or the human notice: {feedback}"
    );
    Ok(())
}

/// One provider delivery the fixture observed, in arrival order.
#[derive(Debug)]
enum Delivery {
    /// A turn's prompt with this serialized input is about to be dispatched.
    Dispatching(String),
    /// A turn started with this serialized input.
    Turn(String),
    /// Steering reached the running turn with this serialized input.
    Steer(String),
}

impl Delivery {
    /// Report whether this delivery handed the provider input containing `text`.
    fn delivered(&self, text: &str) -> bool {
        match self {
            Self::Turn(input) | Self::Steer(input) => input.contains(text),
            Self::Dispatching(_) => false,
        }
    }
}

/// Text of the follow-up the user submits while the approval is pending.
const FOLLOW_UP: &str = "Follow-up written while the approval was pending";

/// Wait until no exchange is running or finalizing, then require the first exchange to have
/// ended as `first_state` and every later exchange to be complete.
async fn require_settled_history(
    controller: &SessionController,
    observed: &[Delivery],
    first_state: crate::exchange::ExchangeState,
) -> Result<()> {
    tokio::time::timeout(Duration::from_secs(3600), async {
        loop {
            let states = SqliteStore::open(&controller.data_root)?
                .list_exchange(controller.observer.session_id())?
                .into_iter()
                .map(|exchange| (exchange.ordinal, exchange.state()))
                .collect::<Vec<_>>();
            if !states.is_empty()
                && !states.iter().any(|(_, state)| {
                    matches!(
                        state,
                        crate::exchange::ExchangeState::Running
                            | crate::exchange::ExchangeState::Finalizing
                    )
                })
            {
                ensure!(
                    states.iter().all(|(ordinal, state)| *state
                        == if *ordinal == 1 {
                            first_state
                        } else {
                            crate::exchange::ExchangeState::Complete
                        }),
                    "an exchange did not settle as expected: {states:?} after {observed:?}"
                );
                return Ok(());
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
    })
    .await
    .with_context(|| format!("exchanges never settled after {observed:?}"))?
}

/// Receive deliveries until one hands the provider `text`, signalling one input-readiness
/// re-evaluation while the permission-timeout continuation waits to be dispatched.
async fn receive_until_delivered(
    controller: &SessionController,
    deliveries: &mut tokio::sync::mpsc::UnboundedReceiver<Delivery>,
    observed: &mut Vec<Delivery>,
    text: &str,
) -> Result<()> {
    while !observed.iter().any(|delivery| delivery.delivered(text)) {
        // The paused clock runs this bound out at once when the delivery never arrives.
        match tokio::time::timeout(Duration::from_secs(3600), deliveries.recv()).await {
            Ok(Some(delivery)) => {
                // Input readiness is re-evaluated whenever the queue or the editor's view of it
                // changes, so such a change can land before the continuation is sent.
                if matches!(&delivery, Delivery::Dispatching(input) if input.contains("permission_timeout"))
                {
                    controller.coordinator.submissions.changed.notify_one();
                }
                observed.push(delivery);
            }
            _ => anyhow::bail!("{text:?} never reached the provider: {observed:?}"),
        }
    }
    Ok(())
}

/// A follow-up submitted during a pending approval reaches the provider once after it expires.
///
/// The permission-timeout continuation used to run without the root execution permit, so the
/// follow-up was admitted as a second foreground delivery beside it. That delivery collected the
/// unsent continuation, the provider restarted after the lost dispatch, and the follow-up's own
/// turn then waited for execution while the restart waited for that turn: the session stayed
/// `Working` forever and the follow-up was never sent.
#[tokio::test(start_paused = true)]
async fn follow_up_submitted_during_an_approval_reaches_the_provider_after_it_expires() -> Result<()>
{
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let (delivered, mut deliveries) = tokio::sync::mpsc::unbounded_channel();
    let runs = Arc::new(AtomicUsize::new(0));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let turn_deliveries = delivered.clone();
    let dispatch_deliveries = delivered.clone();
    // Input the provider accepted as steering, reported as covered by the turn it joined.
    let steered = Arc::new(std::sync::Mutex::new(Vec::new()));
    let turn_steered = steered.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| {
            let first = runs.fetch_add(1, Ordering::AcqRel) == 0;
            let delivered = turn_deliveries.clone();
            let steered = turn_steered.clone();
            async move {
                start.observed(1).await?;
                if first {
                    // An interrupted turn keeps its expiry feedback for a continuation, which is
                    // the delivery the follow-up must not race.
                    run_gated_command(&request, &sink).await?;
                    return finished_turn(
                        &request,
                        std::mem::take(&mut *steered.lock().expect("steering lock")),
                        forge_provider_api::turn::TurnOutcome::Interrupted,
                    );
                } else {
                    let input =
                        serde_json::to_string(&request.input).expect("turn input serializes");
                    let _ = delivered.send(Delivery::Turn(input));
                    // Keep later turns open long enough for queued input to steer into them.
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
                let steered = std::mem::take(&mut *steered.lock().expect("steering lock"));
                completed_turn(&request, steered)
            }
        },
    )
    .with_pre_dispatch(move |request| {
        let delivered = dispatch_deliveries.clone();
        async move {
            let input = serde_json::to_string(&request.input).expect("turn input serializes");
            let continuation = input.contains("permission_timeout");
            let _ = delivered.send(Delivery::Dispatching(input));
            if continuation {
                // Hold the continuation before its send, where the readiness change lands.
                tokio::time::sleep(Duration::from_secs(1)).await;
            }
            Ok(())
        }
    })
    .with_steering(move |request| {
        let delivered = delivered.clone();
        let steered = steered.clone();
        async move {
            if let Some(receipt) = request.delivery.receipt() {
                steered
                    .lock()
                    .expect("steering lock")
                    .push(forge_provider_api::CoveredInput::new(
                        receipt.id().clone(),
                        receipt.attempt_id()?,
                    ));
            }
            let input = serde_json::to_string(&request.input).expect("steering input serializes");
            let _ = delivered.send(Delivery::Steer(input));
            Ok(())
        }
    });
    let (controller, provider, sink, reader) = start_controller(&root, native).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 900,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"approval-follow-up","text":"Run the gated command"}),
            },
            &sink,
        )
        .await?;
        tokio::time::timeout(Duration::from_secs(60), async {
            while controller.permission.pending_list()?.is_empty() {
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("the gated command opened no approval")??;
        controller
            .input_request(
                Request {
                    id: 901,
                    method: "session.input.apply".into(),
                    params: json!({"id":"follow-up","action":{"kind":"submit","message":{
                        "id":"follow-up","text":FOLLOW_UP,"intent":"auto","attachments":[],
                    }}}),
                },
                RuntimeMethod::SessionInputApply,
                &sink,
            )
            .await?;
        // The paused clock expires the approval once the owner waits on nothing else.
        let mut observed = Vec::new();
        receive_until_delivered(&controller, &mut deliveries, &mut observed, FOLLOW_UP).await?;
        require_settled_history(
            &controller,
            &observed,
            crate::exchange::ExchangeState::Interrupted,
        )
        .await?;
        // A later prompt runs only when the owner returned to idle rather than wedging.
        let final_prompt = "Prompt after the follow-up";
        tokio::time::timeout(
            Duration::from_secs(3600),
            prompt::apply(
                &controller,
                Request {
                    id: 902,
                    method: "session.prompt.submit".into(),
                    params: json!({"operation_id":"approval-final","text":final_prompt}),
                },
                &sink,
            ),
        )
        .await
        .with_context(|| format!("the owner never admitted a later prompt after {observed:?}"))??;
        receive_until_delivered(&controller, &mut deliveries, &mut observed, final_prompt).await?;
        require_settled_history(
            &controller,
            &observed,
            crate::exchange::ExchangeState::Interrupted,
        )
        .await?;
        let follow_up_count = observed
            .iter()
            .filter(|delivery| delivery.delivered(FOLLOW_UP))
            .count();
        ensure!(
            follow_up_count == 1,
            "the follow-up reached the provider {follow_up_count} times: {observed:?}"
        );
        ensure!(
            observed
                .iter()
                .any(|delivery| delivery.delivered("permission_timeout")),
            "the timed-out approval's feedback never reached the provider: {observed:?}"
        );
        println!("approval follow-up delivery order: {observed:?}");
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = stop_controller(controller, provider, reader).await;
    drop(root);
    scenario?;
    teardown
}

/// Switching to Allow while an approval waits approves that request without stopping the turn.
///
/// The approval mode is Harness policy; no adapter configures it natively. Changing it used to
/// stop the provider and reconfigure it: the pending request was rejected, the streamed turn was
/// discarded, and the provider ran the turn again.
#[tokio::test(start_paused = true)]
async fn allowing_a_pending_approval_approves_it_without_stopping_the_turn() -> Result<()> {
    let response = decide_pending_approval_by_mode("allow").await?;
    ensure!(
        matches!(response, PermissionResponse::Allow),
        "switching to Allow did not approve the pending request: {response:?}"
    );
    Ok(())
}

/// Switching to Deny rejects a pending request with the reason a new request gets under Deny.
#[tokio::test(start_paused = true)]
async fn denying_a_pending_approval_rejects_it_with_the_mode_reason() -> Result<()> {
    let response = decide_pending_approval_by_mode("deny").await?;
    let PermissionResponse::Deny {
        reason: Some(reason),
    } = &response
    else {
        anyhow::bail!("switching to Deny did not reject the pending request: {response:?}");
    };
    ensure!(
        reason.contains("current approval mode denies")
            && reason.contains(forge_provider_api::forge::REJECT_ALL_GUIDANCE),
        "the rejection did not carry the Deny mode reason: {reason}"
    );
    Ok(())
}

/// Change the approval mode while the first turn's request waits, and return how the provider's
/// request was answered once the turn settled without a restart.
async fn decide_pending_approval_by_mode(mode: &str) -> Result<PermissionResponse> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let runs = Arc::new(AtomicUsize::new(0));
    let (decided, mut decisions) = tokio::sync::mpsc::unbounded_channel();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let turn_runs = runs.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, _sink, start, _cancelled| {
            turn_runs.fetch_add(1, Ordering::AcqRel);
            let decided = decided.clone();
            async move {
                start.observed(1).await?;
                let response = request
                    .permissions
                    .authorize(
                        &request.scope,
                        PermissionRequest {
                            id: "fixture-approval".into(),
                            provider: forge_providers::registry::nonselectable_id().unwrap(),
                            subject: PermissionSubject::Unattached,
                            reason: None,
                            target_list: vec![PermissionTarget::Tool {
                                target: "fixture-approval".into(),
                            }],
                        },
                    )
                    .await;
                let _ = decided.send(response);
                completed_turn(&request, Vec::new())
            }
        },
    );
    let (controller, provider, sink, reader) = start_controller(&root, native).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 910,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"approval-mode","text":"Run the gated tool"}),
            },
            &sink,
        )
        .await?;
        tokio::time::timeout(Duration::from_secs(60), async {
            while controller.permission.pending_list()?.is_empty() {
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await
        .context("the gated tool opened no approval")??;
        let command = super::super::native_permission::decode_command(&Request {
            id: 911,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"mode-pending","action":"permission","change":{"kind":"set_approval","mode":mode}}),
        })?;
        controller
            .exchange_owner
            .execute_identified(command, sink.clone())
            .await
            .context("changing the approval mode failed")?;
        let response = tokio::time::timeout(Duration::from_secs(60), decisions.recv())
            .await
            .context("the pending approval was never decided")?
            .context("the provider stopped before the approval was decided")?;
        require_settled_history(&controller, &[], crate::exchange::ExchangeState::Complete)
            .await?;
        ensure!(
            runs.load(Ordering::Acquire) == 1,
            "the approval-mode change restarted the turn {} times",
            runs.load(Ordering::Acquire) - 1
        );
        ensure!(
            serde_json::to_value(controller.observer.snapshot()?.session.approval_mode)? == mode,
            "the session did not keep the selected mode"
        );
        Ok::<PermissionResponse, anyhow::Error>(response)
    }
    .await;
    let teardown = stop_controller(controller, provider, reader).await;
    drop(root);
    let response = scenario?;
    teardown?;
    Ok(response)
}

/// A tool that outlives its deadline fails the exchange and records why, beside the tool.
///
/// The provider reports the stopped call only as interrupted, so the tool row used to carry no
/// failure mark and the reason survived only in a transient notice that named the tool twice.
/// The exchange now keeps the expiry: its tool row is marked failed with the configured
/// duration, and the stop reason sits in the transcript where turn failures are shown.
#[tokio::test(start_paused = true)]
async fn tool_outliving_its_deadline_records_the_timeout_beside_the_tool() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, mut cancelled| async move {
            start.observed(1).await?;
            let item_id =
                forge_provider_api::ItemId::new("held-command").expect("fixture item ID is valid");
            let tool_event = |event| forge_provider_api::AdapterActivity::Item {
                thread_id: request.thread_id.clone(),
                turn_id: request.turn_id.clone(),
                item_id: item_id.clone(),
                event,
            };
            for event in [
                forge_provider_api::ItemEvent::Started(
                    forge_provider_api::ItemStart::CommandExecution(
                        forge_provider_api::ToolDescriptor {
                            semantics: Default::default(),
                            name: "shell".into(),
                            title: "Held fixture command".into(),
                            arguments: forge_provider_api::InspectableArguments::new(
                                json!({"command":"held"}),
                            ),
                        },
                    ),
                ),
                forge_provider_api::ItemEvent::Tool {
                    event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                },
            ] {
                sink.send_wait(tool_event(event)).await?;
            }
            while !*cancelled.borrow() {
                cancelled
                    .changed()
                    .await
                    .map_err(|_| crate::provider::ProviderError::Cancelled)?;
            }
            // Providers report a call their turn interrupt stopped as interrupted, not failed.
            sink.send_wait(tool_event(forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::CommandExecution(
                    forge_provider_api::ToolCompletion {
                        outcome: crate::provider::ToolOutcome::Interrupted,
                        descriptor: None,
                        output: None,
                        changes: Default::default(),
                    },
                ),
            )))
            .await?;
            finished_turn(
                &request,
                Vec::new(),
                forge_provider_api::turn::TurnOutcome::Interrupted,
            )
        },
    );
    let (controller, provider, sink, reader) = start_controller(&root, native).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 920,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"tool-deadline","text":"Run the held command"}),
            },
            &sink,
        )
        .await?;
        // Pass the ten-minute default deadline in one step of the paused clock; polling the
        // store each simulated second until then costs seconds of real time.
        tokio::time::sleep(Duration::from_secs(11 * 60)).await;
        require_settled_history(&controller, &[], crate::exchange::ExchangeState::Failed).await?;
        let exchange = SqliteStore::open(&controller.data_root)?
            .list_exchange(controller.observer.session_id())?
            .into_iter()
            .find(|exchange| exchange.ordinal == 1)
            .context("the held command lost its exchange")?;
        ensure!(
            exchange.tool_timeout.len() == 1
                && exchange.tool_timeout[0].item_id.as_str() == "held-command"
                && exchange.tool_timeout[0].timeout_label() == "10m",
            "the exchange did not keep the expiry: {:?}",
            exchange.tool_timeout
        );
        let mut rows = Vec::new();
        for entry in &controller.observer.snapshot()?.timeline {
            if matches!(entry, crate::timeline::TimelineEntry::Exchange { .. }) {
                rows.extend(crate::buffer::projection::tests::project_rows(entry)?);
            }
        }
        ensure!(
            rows.iter().any(|row| row
                .contains("Stopped: `Held fixture command` exceeded its 10m tool timeout.")),
            "the transcript did not keep the stop reason: {rows:#?}"
        );
        ensure!(
            !rows.iter().any(|row| row.contains("held-command")),
            "the stop reason repeated the tool's identifier: {rows:#?}"
        );
        let mut expanded = Vec::new();
        for entry in &controller.observer.snapshot()?.timeline {
            if matches!(entry, crate::timeline::TimelineEntry::Exchange { .. }) {
                expanded.extend(crate::buffer::projection::tests::project_rows_expanded(
                    entry,
                )?);
            }
        }
        ensure!(
            expanded
                .iter()
                .any(|row| row.contains("Failed: timed out after 10m; execution stopped.")),
            "the tool row did not record its timeout: {expanded:#?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = stop_controller(controller, provider, reader).await;
    drop(root);
    scenario?;
    teardown
}

/// A command Harness refuses before asking anyone shows Harness's reason on its tool row.
///
/// The shell-writer check answers the provider directly, so the host recorded no decision and
/// the row showed only the provider's generic denial, such as Copilot's fixed "Provider denied
/// this operation". The refusal is now recorded with its reason on the request's own item.
#[tokio::test(start_paused = true)]
async fn harness_refusal_before_any_approval_shows_its_reason_on_the_tool_row() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, start, _cancelled| async move {
            start.observed(1).await?;
            run_command_requesting(
                &request,
                &sink,
                PermissionTarget::Command {
                    command: "Set-Content notes.txt hello".into(),
                    shell: Some(forge_shell::shell::CommandShell::PowerShell),
                },
            )
            .await?;
            completed_turn(&request, Vec::new())
        },
    );
    let (controller, provider, sink, reader) = start_controller(&root, native).await?;
    let scenario = async {
        prompt::apply(
            &controller,
            Request {
                id: 930,
                method: "session.prompt.submit".into(),
                params: json!({"operation_id":"shell-writer","text":"Write the notes"}),
            },
            &sink,
        )
        .await?;
        require_settled_history(&controller, &[], crate::exchange::ExchangeState::Complete).await?;
        let exchange = SqliteStore::open(&controller.data_root)?
            .list_exchange(controller.observer.session_id())?
            .into_iter()
            .find(|exchange| exchange.ordinal == 1)
            .context("the refused command lost its exchange")?;
        let resolution = serde_json::to_value(&exchange.approval_resolution)?;
        ensure!(
            resolution[0]["Resolved"]["targets"][0]["source"] == "SavedPolicy"
                && resolution[0]["Resolved"]["scope"]["Item"]["item_id"] == COMMAND_ITEM,
            "the refusal was not recorded on its item: {resolution}"
        );
        let rows = {
            let mut rows = Vec::new();
            for entry in &controller.observer.snapshot()?.timeline {
                if matches!(entry, crate::timeline::TimelineEntry::Exchange { .. }) {
                    rows.extend(crate::buffer::projection::tests::project_rows_expanded(
                        entry,
                    )?);
                }
            }
            rows
        };
        let text = rows.join(
            "
",
        );
        ensure!(
            text.contains("Denied by Harness policy")
                && text.contains("writes files through the shell")
                && !text.contains("fixture approval was not granted"),
            "the row did not show Harness's reason: {rows:#?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let teardown = stop_controller(controller, provider, reader).await;
    drop(root);
    scenario?;
    teardown
}
