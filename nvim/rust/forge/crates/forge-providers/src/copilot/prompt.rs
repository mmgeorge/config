use super::event_decoder::CopilotEventDecoder;
use super::turn::{CopilotTurnEvent, CopilotTurnEventGuard};
use super::{rejection, retry};
use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::input::{MessageInput, PromptInput};
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::turn_control::TurnControl;
use forge_provider_api::{
    ProviderCapability, ProviderError, ProviderEvent, ProviderEventSink, ProviderRuntime,
    error::{ProviderFailure, ProviderFailureKind, ProviderOperation},
};
use github_copilot_sdk::rpc::{CommandsInvokeRequest, SlashCommandInvocationResult};
use github_copilot_sdk::session::Session;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::sync::Arc;

pub struct TurnOptions {
    pub scope: forge_provider_api::InvocationScope,
    pub binding_id: forge_provider_api::BindingId,
    pub host_effects: Arc<dyn forge_provider_api::ProviderHostEffects>,
    pub workspace: String,
    pub model: Option<String>,
    pub capability: ProviderCapability,
    pub input: PromptInput,
    pub task: Option<forge_provider_api::input::TaskStart>,
    pub goal: Option<forge_provider_api::native_goal::NativeGoalCommand>,
    pub recovered: Vec<forge_provider_api::session::RecoveredInput>,
    pub instructions: String,
    pub repair_caution: Option<String>,
    pub attachments: MessageInput,
    pub receipt: Option<Arc<dyn InputReceipt>>,
    pub submission_id: Option<forge_provider_api::SubmissionId>,
    pub background_compaction_observed: bool,
    pub accepted: Option<tokio::sync::oneshot::Sender<std::result::Result<(), ProviderError>>>,
}

pub struct TurnResult {
    #[cfg(test)]
    pub completion: ProviderCompletion,
}

pub async fn run(
    session: &Arc<super::handle::CopilotSession>,
    turn_event: &CopilotTurnEvent,
    turn_event_guard: &CopilotTurnEventGuard<'_>,
    mut options: TurnOptions,
    sink: &ProviderEventSink,
    control: &dyn TurnControl,
    native_callbacks: &dyn super::provider::NativeCallbackRouter,
) -> Result<TurnResult, ProviderError> {
    session.ensure_healthy()?;
    let owned_session = session;
    let session = &session.inner;
    let output = ProviderCompletion {
        provider_session_id: Some(session.id().to_string().try_into().map_err(|_| {
            native_failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Protocol,
            )
        })?),
        capability: options.capability,
        runtime: ProviderRuntime {
            provider: "Copilot CLI".into(),
            model: options.model.clone(),
        },
        ..Default::default()
    };
    let decoder =
        CopilotEventDecoder::new(sink.invocation_scope()?.clone(), options.workspace.clone());
    let retained_context = Arc::new(tokio::sync::Mutex::new(super::intake::TurnContext {
        decoder,
        output,
        sink: sink.clone(),
    }));
    let mut subscription = owned_session
        .events
        .attach(retained_context.clone())
        .await?;
    let mut active_context = retained_context.lock().await;
    let super::intake::TurnContext {
        decoder, output, ..
    } = &mut *active_context;
    let initial_task_snapshot = session
        .rpc()
        .plan()
        .read_sql_todos_with_dependencies()
        .await
        .map_err(|_| {
            native_failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            )
        })?;
    let initial_task_data = serde_json::to_value(initial_task_snapshot).unwrap_or(Value::Null);
    if initial_task_data
        .get("rows")
        .and_then(Value::as_array)
        .is_some_and(|row_list| !row_list.is_empty())
    {
        decoder
            .publish_task_snapshot(initial_task_data, "copilot-root".into(), output, sink)
            .await?;
    }
    let provider_text = input_text(session, &options.input).await?;
    let provider_text = match &options.goal {
        Some(forge_provider_api::native_goal::NativeGoalCommand::Set { objective }) => {
            format!(
                "Establish this goal for the current task and work toward it: {objective}\n\n{provider_text}"
            )
        }
        Some(forge_provider_api::native_goal::NativeGoalCommand::Resume { objective }) => {
            format!(
                "Resume work on the existing goal and continue from accepted conversation history: {objective}\n\n{provider_text}"
            )
        }
        None => provider_text,
    };
    let provider_text = with_attachment_context(provider_text, &options.attachments.attachments);
    let provider_text = match &options.task {
        Some(task) => format!("{}\n\n{provider_text}", task.heading()),
        None => provider_text,
    };
    let mut recovered = Vec::new();
    let mut recovered_attachments = Vec::new();
    for input in &options.recovered {
        let mut text = input.instructions.clone();
        if !text.is_empty() {
            text.push_str("\n\n");
        }
        text.push_str(&input_text(session, &input.input).await?);
        text = with_attachment_context(text, &input.attachments);
        recovered.push(text);
        recovered_attachments.extend(input.attachments.iter().cloned());
    }
    let provider_text = forge_provider_api::input::recovered_text(&provider_text, &recovered);
    let prompt = match &options.repair_caution {
        Some(caution) => format!(
            "{}\n\n{}\n\n{}",
            options.instructions, caution, provider_text
        ),
        None => format!("{}\n\n{}", options.instructions, provider_text),
    };
    let history = session.rpc().event_log().tail().await.map_err(|_| {
        native_failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Transport,
        )
    })?;
    let mut message_input = options.attachments;
    recovered_attachments.extend(message_input.attachments);
    message_input.attachments = recovered_attachments;
    message_input.prompt.push_str(&prompt);
    let message = super::input::message(message_input);
    let sending = turn_event_guard
        .send(
            history.cursor.clone(),
            options.submission_id.clone(),
            async {
                owned_session.ensure_healthy()?;
                control.before_send(&options.scope).await?;
                if let Some(submission) = &options.receipt {
                    submission
                        .clone()
                        .record_sending(Some(sending_recovery_token(
                            options.scope.owner.clone(),
                            options.submission_id.clone().ok_or_else(|| {
                                native_failure(
                                    ProviderOperation::AdmitInput,
                                    ProviderFailureKind::Rejected,
                                )
                            })?,
                            options.workspace.clone(),
                            session.id().to_string(),
                            history.cursor.clone(),
                            hex::encode(Sha256::digest(message.prompt.as_bytes())),
                        )?))
                        .await?;
                }
                owned_session.ensure_healthy()?;
                let receipt = session
                    .send(message)
                    .await
                    .map_err(|_| ProviderError::DeliveryUncertain)?;
                if let Some(submission) = &options.receipt {
                    submission
                        .clone()
                        .record_accepted(Some(accepted_recovery_token(
                            options.submission_id.clone().ok_or_else(|| {
                                native_failure(
                                    ProviderOperation::AdmitInput,
                                    ProviderFailureKind::Rejected,
                                )
                            })?,
                            options.scope.owner.clone(),
                            session.id().to_string(),
                            receipt.clone(),
                        )?))
                        .await
                        .map_err(|_| ProviderError::DeliveryUncertain)?;
                }
                Ok(receipt)
            },
        )
        .await;
    if let Err(error) = sending {
        let acceptance_error = error
            .downcast_ref::<ProviderError>()
            .and_then(|error| match error {
                ProviderError::NotConsumed { continuation } => Some(ProviderError::NotConsumed {
                    continuation: continuation.clone(),
                }),
                _ => None,
            })
            .unwrap_or(ProviderError::DeliveryUncertain);
        if let Some(accepted) = options.accepted.take() {
            let _ = accepted.send(Err(acceptance_error.clone()));
        }
        return Err(acceptance_error);
    }
    if let Some(accepted) = options.accepted.take() {
        let _ = accepted.send(Ok(()));
    }
    let mut tool_names = std::collections::HashMap::new();
    let mut execution_turn_stopped = false;
    let mut foreground_emitted = false;
    let mut transient_failure: Option<(
        forge_provider_api::TurnAddress,
        forge_provider_api::retry::RetryCategory,
    )> = None;
    let mut retry_idle_deadline = None;
    let mut rejection = rejection::RejectionState::default();
    let mut provider_aborted = false;
    let processing: std::result::Result<(), ProviderError> = async {
        loop {
            tokio::select! {
                settled = turn_event.idle_settled() => {
                    normalize_internal(settled, ProviderOperation::NativeTransport, ProviderFailureKind::Protocol)?;
                    break;
                },
                _ = async {
                    match retry_idle_deadline {
                        Some(deadline) => tokio::time::sleep_until(deadline).await,
                        None => std::future::pending::<()>().await,
                    }
                } => {
                    return Err(native_failure(ProviderOperation::NativeTransport, ProviderFailureKind::Transport));
                }
                provider_event = subscription.recv() => {
                    let provider_event = provider_event.map_err(|_| native_failure(ProviderOperation::CollectEventReader, ProviderFailureKind::Transport))?;
                    if let Some(call_id) = provider_event.data.get("toolCallId").and_then(Value::as_str) {
                        native_callbacks.bind_native_call(call_id, &options.scope).await;
                    }
                    normalize_internal(turn_event.observe(&provider_event), ProviderOperation::NativeTransport, ProviderFailureKind::Protocol)?;
                    rejection.observe(&provider_event);
                    if provider_event.agent_id.is_none()
                        && provider_event.event_type == "assistant.idle"
                    {
                        provider_aborted |= provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true);
                    }
                    if completes_exchange(&provider_event) {
                        let aborted = !execution_turn_stopped && (provider_aborted || provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true));
                        if decoder.root_address().is_none() {
                            continue;
                        }
                        decoder.decode(&provider_event, output, sink).await?;
                        output.provider_checkpoint_id = if provider_event.ephemeral == Some(true) {
                            provider_event.parent_id.clone()
                        } else {
                            Some(provider_event.id.clone())
                        };
                        normalize_internal(turn_event.observe_idle_at(&provider_event, if aborted { super::turn::IdleOutcome::Aborted } else { super::turn::IdleOutcome::Completed }), ProviderOperation::NativeTransport, ProviderFailureKind::Protocol)?;
                        if aborted {
                            let settled = tokio::time::timeout(std::time::Duration::from_secs(15), turn_event.idle_settled()).await
                                .map_err(|_| ProviderError::DeliveryUncertain)?;
                            normalize_internal(settled, ProviderOperation::NativeTransport, ProviderFailureKind::Protocol)?;
                            decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink).await?;
                            publish_terminal_executions(session, turn_event, sink, output).await?;
                            return Err(ProviderError::Cancelled);
                        }
                        continue;
                    }
                    if provider_event.event_type == "session.todos_changed" {
                        let task_snapshot = session
                            .rpc()
                            .plan()
                            .read_sql_todos_with_dependencies()
                            .await
                            .map_err(|_| native_failure(ProviderOperation::NativeTransport, ProviderFailureKind::Transport))?;
                        decoder.publish_task_snapshot(
                            serde_json::to_value(task_snapshot).unwrap_or(Value::Null),
                            provider_event
                                .agent_id
                                .clone()
                                .unwrap_or_else(|| "copilot-root".into()),
                            output,
                            sink,
                        ).await?;
                        continue;
                    }
                    let terminal_error = provider_event.event_type == "session.error"
                        && !provider_event.is_transient_error();
                    if matches!(provider_event.event_type.as_str(), "session.compaction_start" | "session.compaction_complete")
                        && options.background_compaction_observed {
                        continue;
                    }
                    if terminal_error
                        && let Some(category) = retry::transient_category(&provider_event.data)
                        && provider_event.agent_id.is_none()
                        && decoder.replay_safe()
                        && (native_replay_safe(output) && control.status(&options.scope).await?.replay_safe)
                        && let Some(address) = decoder.root_address()
                    {
                        transient_failure = Some((address, category));
                        retry_idle_deadline = Some(tokio::time::Instant::now() + std::time::Duration::from_secs(30));
                        continue;
                    }
                    decoder.decode(&provider_event, output, sink).await?;
                    if !foreground_emitted
                        && let (Some((_, event_id)), Some(root)) = (normalize_internal(turn_event.foreground_anchor(), ProviderOperation::NativeTransport, ProviderFailureKind::Protocol)?, decoder.root_address()) {
                            let address = forge_provider_api::TurnAddress { turn_id: event_id.into(), ..root };
                            let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                                address.scope.clone(), forge_provider_api::lifecycle::ExecutionUpdate::Started { address },
                            )?;
                            sink.send_wait(forge_provider_api::ProviderEvent::Execution { observation }).await?;
                            foreground_emitted = true;
                    }
                    if provider_event.agent_id.is_none() && !execution_turn_stopped {
                        let call_id = provider_event.data.get("toolCallId").and_then(Value::as_str);
                        if provider_event.event_type == "tool.execution_start"
                            && let (Some(call_id), Some(name)) = (call_id, provider_event.data.get("toolName").and_then(Value::as_str)) {
                            tool_names.insert(call_id.to_owned(), name.to_owned());
                        }
                        if provider_event.event_type == "tool.execution_complete" {
                            let name = call_id.and_then(|id| tool_names.remove(id));
                            if let Some(call_id) = call_id {
                                native_callbacks.release_native_call(call_id).await;
                            }
                        if control.tool_finished(&options.scope, name.as_deref()) {
                                session.abort().await.map_err(|_| native_failure(ProviderOperation::CancelSession, ProviderFailureKind::Transport))?;
                                execution_turn_stopped = true;
                            }
                        }
                    }
                    if terminal_error {
                        decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink).await?;
                        return Err(native_failure(ProviderOperation::NativeTransport, super::error::session_failure_kind(&provider_event.data)));
                    }
                }
                result = control.receive(&options.scope) => { result?; }

            }
        }
        Ok(())
        }.await;
    control.close(&options.scope).await?;
    let host_status = control.status(&options.scope).await?;
    let publication_failure = options
        .host_effects
        .settle_foreground(&options.binding_id, &options.scope)
        .await
        .err();
    if let Err(error) = processing {
        let outcome = if matches!(error, ProviderError::Cancelled) {
            forge_provider_api::turn::TurnOutcome::Cancelled
        } else {
            forge_provider_api::turn::TurnOutcome::Failed
        };
        decoder.finish_failure(outcome, sink).await?;
        if let Some(failure) = publication_failure {
            publish_failed_publication(session, turn_event, sink, output, failure).await?;
        } else {
            publish_observation_lost(sink, output).await?;
        }
        return Err(error);
    }
    subscription.ensure_healthy()?;
    if let Some(failure) = publication_failure {
        decoder.finish_success(sink).await?;
        publish_failed_publication(session, turn_event, sink, output, failure).await?;
        return Err(native_failure(
            ProviderOperation::HostEffectPublication,
            match failure {
                forge_provider_api::session::HostEffectFailure::Rejected => {
                    ProviderFailureKind::Rejected
                }
                forge_provider_api::session::HostEffectFailure::Uncertain => {
                    ProviderFailureKind::Persistence
                }
                forge_provider_api::session::HostEffectFailure::Retired => {
                    ProviderFailureKind::Authorization
                }
            },
        ));
    }
    if let Some((address, category)) = transient_failure {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        let continuation =
            forge_provider_api::ContinuationToken::new(uuid::Uuid::new_v4().as_bytes().to_vec())
                .map_err(|_| {
                    native_failure(
                        ProviderOperation::NativeTransport,
                        ProviderFailureKind::Unknown,
                    )
                })?;
        publish_retryable_execution(
            turn_event,
            sink,
            output,
            &address,
            category,
            continuation.clone(),
        )
        .await?;
        return Err(ProviderError::Retryable(Box::new(
            forge_provider_api::retry::RetryableFailure {
                address,
                category,
                continuation,
            },
        )));
    }
    if host_status.cancelled {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink)
            .await?;
        publish_terminal_executions(session, turn_event, sink, output).await?;
        return Err(ProviderError::Cancelled);
    }
    if !execution_turn_stopped && rejection.finish(host_status.control_handoff).is_err() {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(native_failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Rejected,
        ));
    }
    if host_status.error.is_some() {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(native_failure(
            ProviderOperation::HostEffectPublication,
            ProviderFailureKind::Rejected,
        ));
    }
    if execution_turn_stopped {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Interrupted, sink)
            .await?;
    } else {
        decoder.finish_success(sink).await?;
        publish_terminal_executions(session, turn_event, sink, output).await?;
    }
    Ok(TurnResult {
        #[cfg(test)]
        completion: output.clone(),
    })
}

fn with_attachment_context(
    mut prompt: String,
    attachments: &[forge_provider_api::FileAttachment],
) -> String {
    for attachment in attachments {
        if !prompt.is_empty() {
            prompt.push_str("\n\n");
        }
        prompt.push_str(&attachment.context);
        prompt.push_str("\nNative attachment: ");
        prompt.push_str(
            &attachment
                .label
                .replace('&', "&amp;")
                .replace('<', "&lt;")
                .replace('>', "&gt;"),
        );
    }
    prompt
}

/// Publish terminal execution observations after accepted inputs and live turns settle.
async fn publish_terminal_executions(
    session: &Session,
    turn_event: &CopilotTurnEvent,
    sink: &ProviderEventSink,
    completion: &ProviderCompletion,
) -> Result<(), ProviderError> {
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};

    let scope = sink.invocation_scope()?.clone();
    let completed_at_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
        .try_into()
        .unwrap_or(i64::MAX);
    for (native_event, covered_submissions, result) in normalize_internal(
        turn_event.terminal_executions(),
        ProviderOperation::NativeTransport,
        ProviderFailureKind::Protocol,
    )? {
        let address = forge_provider_api::TurnAddress {
            scope: scope.clone(),
            thread_id: session.id().to_string(),
            turn_id: native_event.into(),
        };
        let result = match result {
            ExecutionResult::Completed { .. } => ExecutionResult::Completed {
                completion: completion.clone().into(),
            },
            ExecutionResult::Cancelled { .. } => ExecutionResult::Cancelled {
                completion: completion.clone().into(),
            },
            ExecutionResult::Failed { failure, .. } => ExecutionResult::Failed {
                completion: completion.clone().into(),
                failure,
            },
        };
        let observation = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Finished {
                address,
                covered_submissions,
                completed_at_ms,
                result,
            },
        )?;
        sink.send_wait(ProviderEvent::Execution { observation })
            .await?;
    }
    Ok(())
}

/// Preserve native terminal evidence when required host effects fail to publish.
async fn publish_failed_publication(
    session: &Session,
    turn_event: &CopilotTurnEvent,
    sink: &ProviderEventSink,
    completion: &ProviderCompletion,
    failure: forge_provider_api::session::HostEffectFailure,
) -> Result<(), ProviderError> {
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};

    let scope = sink.invocation_scope()?.clone();
    let completed_at_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
        .try_into()
        .unwrap_or(i64::MAX);
    for (native_event, covered_submissions, result) in normalize_internal(
        turn_event.terminal_executions(),
        ProviderOperation::NativeTransport,
        ProviderFailureKind::Protocol,
    )? {
        let address = forge_provider_api::TurnAddress {
            scope: scope.clone(),
            thread_id: session.id().to_string(),
            turn_id: native_event.into(),
        };
        let result = match result {
            ExecutionResult::Completed { .. } => ExecutionResult::Completed {
                completion: completion.clone().into(),
            },
            ExecutionResult::Cancelled { .. } => ExecutionResult::Cancelled {
                completion: completion.clone().into(),
            },
            ExecutionResult::Failed { failure, .. } => ExecutionResult::Failed {
                completion: completion.clone().into(),
                failure,
            },
        };
        let observation = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::PublicationFailed {
                address,
                covered_submissions,
                completed_at_ms,
                result,
                failure,
            },
        )?;
        sink.send_wait(ProviderEvent::Execution { observation })
            .await?;
    }
    Ok(())
}

/// Publish retry eligibility while retaining the accepted inputs in native history.
async fn publish_retryable_execution(
    turn_event: &CopilotTurnEvent,
    sink: &ProviderEventSink,
    completion: &ProviderCompletion,
    address: &forge_provider_api::TurnAddress,
    category: forge_provider_api::retry::RetryCategory,
    continuation: forge_provider_api::ContinuationToken,
) -> Result<(), ProviderError> {
    use forge_provider_api::lifecycle::{
        ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate,
    };

    let (_, covered_submissions, _) = turn_event
        .terminal_executions()
        .map_err(|_| {
            native_failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?
        .into_iter()
        .find(|(native_event, _, _)| native_event == address.turn_id.as_str())
        .ok_or_else(|| {
            native_failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
    let scope = sink.invocation_scope()?.clone();
    let observation = ExecutionObservation::new(
        scope,
        ExecutionUpdate::Finished {
            address: address.clone(),
            covered_submissions,
            completed_at_ms: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis()
                .try_into()
                .unwrap_or(i64::MAX),
            result: ExecutionResult::Failed {
                completion: completion.clone().into(),
                failure: ExecutionFailure::Retryable {
                    address: address.clone(),
                    category,
                    continuation,
                },
            },
        },
    )?;
    sink.send_wait(ProviderEvent::Execution { observation })
        .await?;
    Ok(())
}

/// Report lost terminal observation without turning an operation error into native success.
async fn publish_observation_lost(
    sink: &ProviderEventSink,
    completion: &ProviderCompletion,
) -> Result<(), ProviderError> {
    let scope = sink.invocation_scope()?.clone();
    let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
        scope,
        forge_provider_api::lifecycle::ExecutionUpdate::ObservationLost {
            failure: forge_provider_api::lifecycle::ObservationFailure::Protocol,
            completion: Some(completion.clone().into()),
        },
    )?;
    sink.send_wait(ProviderEvent::Execution { observation })
        .await?;
    Ok(())
}

fn native_replay_safe(output: &ProviderCompletion) -> bool {
    !output.evidence.tool_called
        && !output.evidence.workspace_changed
        && !output.evidence.structured_complete
        && !output.evidence.structured_blocked
}

/// Convert one classified SDK failure into the normalized provider error contract.
fn native_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[derive(Deserialize, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub(super) enum NativeRecoveryEvidence {
    Sending {
        version: u8,
        submission_id: forge_provider_api::SubmissionId,
        owner: forge_provider_api::ExecutionOwner,
        workspace: String,
        session_id: String,
        cursor: String,
        content_digest: String,
    },
    Accepted {
        version: u8,
        submission_id: forge_provider_api::SubmissionId,
        owner: forge_provider_api::ExecutionOwner,
        session_id: String,
        message_id: String,
    },
}

pub(super) fn sending_recovery_token(
    owner: forge_provider_api::ExecutionOwner,
    submission_id: forge_provider_api::SubmissionId,
    workspace: String,
    session_id: String,
    cursor: String,
    content_digest: String,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    recovery_token(NativeRecoveryEvidence::Sending {
        version: 1,
        submission_id,
        owner,
        workspace,
        session_id,
        cursor,
        content_digest,
    })
}

pub(super) fn accepted_recovery_token(
    submission_id: forge_provider_api::SubmissionId,
    owner: forge_provider_api::ExecutionOwner,
    session_id: String,
    message_id: String,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    recovery_token(NativeRecoveryEvidence::Accepted {
        version: 1,
        submission_id,
        owner,
        session_id,
        message_id,
    })
}

pub(super) fn decode_recovery_token(
    token: &forge_provider_api::RecoveryToken,
) -> Result<NativeRecoveryEvidence, ProviderError> {
    serde_json::from_slice(token.as_bytes()).map_err(|_| {
        native_failure(
            ProviderOperation::RecoverSession,
            ProviderFailureKind::Protocol,
        )
    })
}

fn recovery_token(
    evidence: NativeRecoveryEvidence,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let bytes = serde_json::to_vec(&evidence).map_err(|_| {
        native_failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
    })?;
    forge_provider_api::RecoveryToken::new(bytes)
        .map_err(|_| native_failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol))
}

async fn input_text(session: &Arc<Session>, input: &PromptInput) -> Result<String, ProviderError> {
    Ok(match input {
        PromptInput::Text(text) => text.clone(),
        PromptInput::Skill { name, arguments } => {
            let skill_available = session
                .rpc()
                .skills()
                .list()
                .await
                .map_err(|_| {
                    native_failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol)
                })?
                .skills
                .into_iter()
                .any(|skill| skill.name == *name && skill.enabled && skill.user_invocable);
            if !skill_available {
                return Err(native_failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
            match session
                .rpc()
                .commands()
                .invoke(CommandsInvokeRequest {
                    origin: None,
                    name: name.clone(),
                    input: (!arguments.is_empty()).then(|| arguments.clone()),
                })
                .await
                .map_err(|_| {
                    native_failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected)
                })? {
                SlashCommandInvocationResult::AgentPrompt(result) => result.prompt,
                _ => {
                    return Err(native_failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Protocol,
                    ));
                }
            }
        }
        PromptInput::QuestionReply(reply) => {
            let answer = match &reply.response {
                forge_provider_api::input::QuestionReplyResponse::Answer { answer, .. }
                | forge_provider_api::input::QuestionReplyResponse::Correction { answer, .. } => {
                    answer
                }
                forge_provider_api::input::QuestionReplyResponse::Clarification {
                    text, ..
                } => text,
            };
            format!("Answer to the question {:?}: {answer}", reply.question)
        }
    })
}

fn normalize_internal<T>(
    result: anyhow::Result<T>,
    operation: ProviderOperation,
    kind: ProviderFailureKind,
) -> Result<T, ProviderError> {
    result.map_err(|error| {
        error
            .downcast_ref::<ProviderError>()
            .cloned()
            .unwrap_or_else(|| native_failure(operation, kind))
    })
}

pub fn completes_exchange(event: &github_copilot_sdk::SessionEvent) -> bool {
    event.agent_id.is_none() && event.event_type == "session.idle"
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Finish the main agent independently from attached background work.
    #[test]
    fn only_root_session_idle_finishes_the_provider_invocation() {
        let mut event: github_copilot_sdk::SessionEvent =
            serde_json::from_value(serde_json::json!({
                "id":"idle", "type":"assistant.idle", "timestamp":"2026-09-18T00:00:00Z", "data":{}
            }))
            .unwrap();
        assert!(!completes_exchange(&event));
        event.agent_id = Some("child".into());
        assert!(!completes_exchange(&event));
        event.agent_id = None;
        event.event_type = "assistant.turn_end".into();
        assert!(!completes_exchange(&event));
        event.event_type = "session.idle".into();
        assert!(completes_exchange(&event));
        event.agent_id = Some("child".into());
        assert!(!completes_exchange(&event));
    }
}
