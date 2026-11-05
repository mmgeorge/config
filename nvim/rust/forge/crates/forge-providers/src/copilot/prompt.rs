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
    pub workspace: String,
    pub model: Option<String>,
    pub capability: ProviderCapability,
    pub input: PromptInput,
    pub task: Option<forge_provider_api::input::TaskStart>,
    pub recovered: Vec<forge_provider_api::session::RecoveredInput>,
    pub instructions: String,
    pub repair_caution: Option<String>,
    pub attachments: MessageInput,
    pub receipt: Option<Arc<dyn InputReceipt>>,
    /// Confirm the exact journaled input transport handoff independently of acceptance.
    pub dispatch: Option<forge_provider_api::submission::DispatchReporter>,
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
                ProviderOperation::SessionOpen,
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
                ProviderOperation::InputSubmit,
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
    let (cursor, anchor) = owned_session.delivery_boundary().await?;
    let mut message_input = options.attachments;
    recovered_attachments.extend(message_input.attachments);
    message_input.attachments = recovered_attachments;
    message_input.prompt.push_str(&prompt);
    let message = super::input::message(message_input);
    let sending = turn_event_guard
        .send_native(
            cursor.clone(),
            options.submission_id.clone(),
            |authority| async {
                owned_session.ensure_healthy()?;
                control.before_send(&options.scope).await?;
                if let Some(submission) = &options.receipt {
                    submission
                        .clone()
                        .record_sending(Some(sending_recovery_token(
                            options.scope.owner.clone(),
                            options.submission_id.clone().ok_or_else(|| {
                                native_failure(
                                    ProviderOperation::InputSubmit,
                                    ProviderFailureKind::Rejected,
                                )
                            })?,
                            submission.attempt_id()?,
                            NativeDeliveryBoundary {
                                session_id: session.id().to_string(),
                                cursor: cursor.clone(),
                                anchor: anchor.clone(),
                            },
                            options.workspace.clone(),
                            hex::encode(Sha256::digest(message.prompt.as_bytes())),
                        )?))
                        .await?;
                }
                owned_session.ensure_healthy()?;
                let receipt =
                    super::input::send(session, message, options.dispatch.as_ref(), authority)
                        .await?;
                if let Some(submission) = &options.receipt {
                    submission
                        .clone()
                        .record_accepted(Some(accepted_recovery_token(
                            options.submission_id.clone().ok_or_else(|| {
                                native_failure(
                                    ProviderOperation::InputSubmit,
                                    ProviderFailureKind::Rejected,
                                )
                            })?,
                            submission.attempt_id()?,
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
                ProviderError::NotConsumed => Some(ProviderError::NotConsumed),
                ProviderError::Failure(failure)
                    if failure.phase() == forge_provider_api::ProviderPhase::Admission
                        && failure.disposition()
                            == forge_provider_api::FailureDisposition::Unchanged =>
                {
                    Some(error.clone())
                }
                ProviderError::Contract(_)
                | ProviderError::Failure(_)
                | ProviderError::Combined { .. }
                | ProviderError::Cleanup(_)
                | ProviderError::Unsupported(_)
                | ProviderError::Observation(_)
                | ProviderError::RuntimeVersion(_)
                | ProviderError::PreparationFailed(_)
                | ProviderError::PreparationCleanup { .. }
                | ProviderError::DeliveryUncertain
                | ProviderError::Cancelled
                | ProviderError::Retryable(_)
                | ProviderError::Delivery(_) => None,
            })
            .unwrap_or(ProviderError::DeliveryUncertain);
        let acceptance_error =
            if super::operation::current().is_some_and(|operation| operation.dispatched()) {
                super::error::after_input_dispatch(acceptance_error, ProviderOperation::InputSubmit)
            } else {
                acceptance_error
            };
        if super::operation::current().is_some_and(|operation| !operation.dispatched())
            && let Some(receipt) = &options.receipt
        {
            receipt
                .clone()
                .record_rejected(acceptance_error.clone())
                .await?;
        }
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
    let mut rejection = rejection::RejectionState::default();
    let mut provider_aborted = false;
    let processing: std::result::Result<super::turn::IdleOutcome, ProviderError> = async {
        loop {
            tokio::select! {
                settled = turn_event.idle_settled() => {
                    normalize_internal(settled, ProviderOperation::InputSubmit, ProviderFailureKind::Protocol)?;
                    return normalize_internal(turn_event.terminal_outcome(), ProviderOperation::InputSubmit, ProviderFailureKind::Protocol);
                },
                provider_event = subscription.recv() => {
                    let provider_event = provider_event.map_err(|_| native_failure(ProviderOperation::SessionClose, ProviderFailureKind::Transport))?;
                    if let Some(call_id) = provider_event.data.get("toolCallId").and_then(Value::as_str) {
                        native_callbacks.bind_native_call(call_id, &options.scope).await;
                    }
                    normalize_internal(turn_event.observe(&provider_event), ProviderOperation::InputSubmit, ProviderFailureKind::Protocol)?;
                    rejection.observe(&provider_event);
                    if provider_event.agent_id.is_none()
                        && provider_event.event_type == "assistant.idle"
                    {
                        provider_aborted |= provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true);
                    }
                    if completes_exchange(&provider_event) {
                        let aborted = !execution_turn_stopped && (provider_aborted || provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true));
                        if decoder.root_address().is_none() && !aborted {
                            continue;
                        }
                        decoder.decode(&provider_event, output, sink).await?;
                        output.provider_checkpoint_id = if provider_event.ephemeral == Some(true) {
                            provider_event.parent_id.clone()
                        } else {
                            Some(provider_event.id.clone())
                        };
                        let outcome = if execution_turn_stopped { super::turn::IdleOutcome::HostHandoff }
                            else if aborted { super::turn::IdleOutcome::Aborted }
                            else { super::turn::IdleOutcome::Completed };
                        normalize_internal(turn_event.observe_idle_at(&provider_event, outcome), ProviderOperation::InputSubmit, ProviderFailureKind::Protocol)?;
                        continue;
                    }
                    if provider_event.event_type == "session.todos_changed" {
                        let task_snapshot = session
                            .rpc()
                            .plan()
                            .read_sql_todos_with_dependencies()
                            .await
                            .map_err(|_| native_failure(ProviderOperation::InputSubmit, ProviderFailureKind::Transport))?;
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
                        continue;
                    }
                    decoder.decode(&provider_event, output, sink).await?;
                    if !foreground_emitted
                        && let (Some((_, event_id)), Some(root)) = (normalize_internal(turn_event.foreground_anchor(), ProviderOperation::InputSubmit, ProviderFailureKind::Protocol)?, decoder.root_address()) {
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
                                session.abort().await.map_err(|_| native_failure(ProviderOperation::Cancel, ProviderFailureKind::Transport))?;
                                execution_turn_stopped = true;
                            }
                        }
                    }
                    if terminal_error {
                        decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink).await?;
                        return Err(native_failure(ProviderOperation::InputSubmit, super::error::session_failure_kind(&provider_event.data)));
                    }
                }
                result = control.receive(&options.scope) => { result?; }

            }
        }
        }.await;
    let native_cancelled = matches!(processing, Ok(super::turn::IdleOutcome::Aborted));
    let host_status = control.status(&options.scope).await?;
    if let Err(error) = processing {
        let outcome = if matches!(error, ProviderError::Cancelled) {
            forge_provider_api::turn::TurnOutcome::Cancelled
        } else {
            forge_provider_api::turn::TurnOutcome::Failed
        };
        decoder.finish_failure(outcome, sink).await?;
        publish_observation_lost(sink, output).await?;
        return Err(super::error::after_input_dispatch(
            error,
            ProviderOperation::InputSubmit,
        ));
    }
    subscription.ensure_healthy()?;
    if let Some((address, category)) = transient_failure {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        publish_terminal_executions(
            session.id().as_ref(),
            turn_event,
            sink,
            output,
            TerminalPublication::Retryable {
                address: address.clone(),
                category,
            },
        )
        .await?;
        return Err(ProviderError::Retryable(Box::new(
            forge_provider_api::retry::RetryableFailure {
                address,
                category,
                retry_after: None,
            },
        )));
    }
    if native_cancelled || host_status.cancelled {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink)
            .await?;
        publish_terminal_executions(
            session.id().as_ref(),
            turn_event,
            sink,
            output,
            TerminalPublication::Settled,
        )
        .await?;
        return Err(ProviderError::Cancelled);
    }
    if !execution_turn_stopped && rejection.finish(host_status.control_handoff).is_err() {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(native_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ));
    }
    if host_status.error.is_some() {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(native_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        ));
    }
    if execution_turn_stopped {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Interrupted, sink)
            .await?;
    } else {
        decoder.finish_success(sink).await?;
    }
    publish_terminal_executions(
        session.id().as_ref(),
        turn_event,
        sink,
        output,
        TerminalPublication::Settled,
    )
    .await?;
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

/// Distinguish a collected foreground from one whose required host publication failed.
#[derive(Clone)]
enum TerminalPublication {
    Settled,
    Retryable {
        address: forge_provider_api::TurnAddress,
        category: forge_provider_api::retry::RetryCategory,
    },
}

/// Publish every consumed execution's start before any terminal can settle the invocation.
async fn publish_terminal_executions(
    thread_id: &str,
    turn_event: &CopilotTurnEvent,
    sink: &ProviderEventSink,
    completion: &ProviderCompletion,
    publication: TerminalPublication,
) -> Result<(), ProviderError> {
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};

    normalize_internal(
        turn_event.retain_completion(completion),
        ProviderOperation::InputSubmit,
        ProviderFailureKind::Protocol,
    )?;
    let scope = sink.invocation_scope()?.clone();
    let completed_at_ms = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis()
        .try_into()
        .unwrap_or(i64::MAX);
    let mut executions = normalize_internal(
        turn_event.terminal_executions(),
        ProviderOperation::InputSubmit,
        ProviderFailureKind::Protocol,
    )?;
    if let TerminalPublication::Retryable { address, .. } = &publication {
        let index = executions
            .iter()
            .position(|(event, _, _)| {
                address.scope == scope
                    && address.thread_id == thread_id
                    && address.turn_id.as_str() == event
            })
            .ok_or_else(|| {
                native_failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                )
            })?;
        // Publish retry eligibility last, after every other consumed execution is settled.
        let retry = executions.remove(index);
        executions.push(retry);
    }

    // Register steering's distinct native execution before the first input's terminal is visible.
    // Repeating the already-published foreground start preserves the same immutable identity.
    for (native_event, _, _) in &executions {
        let observation = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Started {
                address: forge_provider_api::TurnAddress {
                    scope: scope.clone(),
                    thread_id: thread_id.to_owned(),
                    turn_id: native_event.clone().into(),
                },
            },
        )?;
        sink.send_wait(ProviderEvent::Execution { observation })
            .await?;
    }
    for (native_event, covered_submissions, result) in executions {
        let address = forge_provider_api::TurnAddress {
            scope: scope.clone(),
            thread_id: thread_id.to_owned(),
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
        let result = match &publication {
            TerminalPublication::Retryable {
                address: retry,
                category,
            } if retry == &address => ExecutionResult::Failed {
                completion: completion.clone().into(),
                failure: forge_provider_api::lifecycle::ExecutionFailure::Retryable {
                    address: address.clone(),
                    category: *category,
                    retry_after: None,
                },
            },
            TerminalPublication::Settled | TerminalPublication::Retryable { .. } => result,
        };
        let update = ExecutionUpdate::Finished {
            address,
            covered_submissions,
            completed_at_ms,
            result,
        };
        let observation = ExecutionObservation::new(scope.clone(), update)?;
        sink.send_wait(ProviderEvent::Execution { observation })
            .await?;
    }
    if let Some(covered_submissions) = normalize_internal(
        turn_event.cancelled_without_execution(),
        ProviderOperation::InputSubmit,
        ProviderFailureKind::Protocol,
    )? {
        let result = ExecutionResult::Cancelled {
            completion: completion.clone().into(),
        };
        let update = ExecutionUpdate::ResolvedWithoutExecution {
            covered_submissions,
            completed_at_ms,
            result,
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(scope, update)?,
        })
        .await?;
    }
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
        attempt_id: forge_provider_api::DeliveryAttemptId,
        owner: forge_provider_api::ExecutionOwner,
        workspace: String,
        session_id: String,
        cursor: String,
        anchor: Option<String>,
        content_digest: String,
    },
    Accepted {
        version: u8,
        submission_id: forge_provider_api::SubmissionId,
        attempt_id: forge_provider_api::DeliveryAttemptId,
        owner: forge_provider_api::ExecutionOwner,
        session_id: String,
        message_id: String,
    },
}

/// Bind the volatile cursor and durable anchor to the native history that supplied both.
pub(super) struct NativeDeliveryBoundary {
    pub session_id: String,
    pub cursor: String,
    pub anchor: Option<String>,
}

pub(super) fn sending_recovery_token(
    owner: forge_provider_api::ExecutionOwner,
    submission_id: forge_provider_api::SubmissionId,
    attempt_id: forge_provider_api::DeliveryAttemptId,
    boundary: NativeDeliveryBoundary,
    workspace: String,
    content_digest: String,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let NativeDeliveryBoundary {
        session_id,
        cursor,
        anchor,
    } = boundary;
    recovery_token(NativeRecoveryEvidence::Sending {
        version: 2,
        submission_id,
        attempt_id,
        owner,
        workspace,
        session_id,
        cursor,
        anchor,
        content_digest,
    })
}

pub(super) fn accepted_recovery_token(
    submission_id: forge_provider_api::SubmissionId,
    attempt_id: forge_provider_api::DeliveryAttemptId,
    owner: forge_provider_api::ExecutionOwner,
    session_id: String,
    message_id: String,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    recovery_token(NativeRecoveryEvidence::Accepted {
        version: 2,
        submission_id,
        attempt_id,
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
            ProviderOperation::InputReconcile,
            ProviderFailureKind::Protocol,
        )
    })
}

fn recovery_token(
    evidence: NativeRecoveryEvidence,
) -> Result<forge_provider_api::RecoveryToken, ProviderError> {
    let bytes = serde_json::to_vec(&evidence).map_err(|_| {
        native_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Protocol,
        )
    })?;
    forge_provider_api::RecoveryToken::new(bytes).map_err(|_| {
        native_failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Protocol,
        )
    })
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
                    native_failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Protocol,
                    )
                })?
                .skills
                .into_iter()
                .any(|skill| skill.name == *name && skill.enabled && skill.user_invocable);
            if !skill_available {
                return Err(native_failure(
                    ProviderOperation::InputSubmit,
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
                    native_failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Rejected,
                    )
                })? {
                SlashCommandInvocationResult::AgentPrompt(result) => result.prompt,
                _ => {
                    return Err(native_failure(
                        ProviderOperation::InputSubmit,
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
        let cause = error
            .downcast_ref::<ProviderError>()
            .cloned()
            .unwrap_or_else(|| native_failure(operation, kind));
        // Every caller validates a live native turn or terminal observed after input handoff.
        super::error::after_input_dispatch(cause, operation)
    })
}

pub fn completes_exchange(event: &github_copilot_sdk::SessionEvent) -> bool {
    // Session-wide idle waits for background terminals. Root assistant idle
    // settles the foreground loop while those terminals retain their own owner.
    event.agent_id.is_none()
        && matches!(event.event_type.as_str(), "assistant.idle" | "session.idle")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Register steering's separate execution before publishing either input's terminal coverage.
    #[tokio::test]
    async fn steering_terminals_follow_all_matching_starts() -> anyhow::Result<()> {
        use forge_provider_api::lifecycle::ExecutionUpdate;
        let turn = CopilotTurnEvent::default();
        let guard = turn.activate(None, None)?;
        let first = forge_provider_api::SubmissionId::try_from("first".to_owned())?;
        let second = forge_provider_api::SubmissionId::try_from("second".to_owned())?;
        guard
            .send("cursor".into(), Some(first.clone()), async {
                Ok("receipt-first".into())
            })
            .await?;
        turn.steer(Some(second.clone()), async { Ok("receipt-second".into()) })
            .await?;
        for (id, kind, data) in [
            (
                "user-first",
                "user.message",
                serde_json::json!({"messageId":"receipt-first","turnId":"0"}),
            ),
            (
                "start-first",
                "assistant.turn_start",
                serde_json::json!({"turnId":"0"}),
            ),
            (
                "end-first",
                "assistant.turn_end",
                serde_json::json!({"turnId":"0"}),
            ),
            (
                "start-second",
                "assistant.turn_start",
                serde_json::json!({"turnId":"1"}),
            ),
            (
                "user-second",
                "user.message",
                serde_json::json!({"messageId":"receipt-second","turnId":"1"}),
            ),
            (
                "end-second",
                "assistant.turn_end",
                serde_json::json!({"turnId":"1"}),
            ),
        ] {
            turn.observe(&serde_json::from_value(serde_json::json!({"id":id,"type":kind,"timestamp":"2026-11-03T00:00:00Z","data":data}))?)?;
        }
        let idle = serde_json::from_value(
            serde_json::json!({"id":"idle","type":"session.idle","timestamp":"2026-11-03T00:00:00Z","data":{}}),
        )?;
        turn.observe(&idle)?;
        turn.observe_idle_at(&idle, super::super::turn::IdleOutcome::Completed)?;
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "invocation",
        );
        for publication in [
            TerminalPublication::Settled,
            TerminalPublication::Retryable {
                address: forge_provider_api::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "thread".into(),
                    turn_id: "start-first".into(),
                },
                category: forge_provider_api::retry::RetryCategory::TemporaryTransport,
            },
        ] {
            let retrying = matches!(publication, TerminalPublication::Retryable { .. });
            let (sink, mut stream) = forge_provider_api::events::channel(
                forge_provider_api::BindingId::new("binding")?,
                scope.clone(),
            );
            publish_terminal_executions(
                "thread",
                &turn,
                &sink,
                &ProviderCompletion::default(),
                publication,
            )
            .await?;
            let mut starts = std::collections::HashSet::new();
            let mut covered = std::collections::HashSet::new();
            for index in 0..4 {
                let event = stream.recv().await?.unwrap();
                let ProviderEvent::Execution { observation } = event.event else {
                    panic!("expected execution observation")
                };
                match observation.update() {
                    ExecutionUpdate::Started { address } => {
                        assert!(
                            index < 2,
                            "all executions must be admitted before terminal settlement"
                        );
                        starts.insert(address.turn_id.clone());
                    }
                    ExecutionUpdate::Finished {
                        address,
                        covered_submissions,
                        result,
                        ..
                    }
                    | ExecutionUpdate::PublicationFailed {
                        address,
                        covered_submissions,
                        result,
                        ..
                    } => {
                        assert!(starts.contains(&address.turn_id));
                        covered.extend(covered_submissions.iter().cloned());
                        assert_eq!(
                            matches!(result, forge_provider_api::ExecutionResult::Failed { .. }),
                            retrying && index == 3
                        );
                        if retrying && index == 3 {
                            assert_eq!(address.turn_id.as_str(), "start-first");
                        }
                    }
                    other => panic!("unexpected terminal sequence: {other:?}"),
                }
            }
            assert_eq!(starts.len(), 2);
            assert_eq!(
                covered,
                std::collections::HashSet::from([first.clone(), second.clone()])
            );
        }
        Ok(())
    }

    /// Abort after accepted consumption but before turn-start preserves coverage without inventing execution.
    #[tokio::test]
    async fn cancelled_before_turn_start_publishes_consumed_resolution_without_execution()
    -> anyhow::Result<()> {
        use forge_provider_api::lifecycle::ExecutionUpdate;
        for native_turn in [None, Some("not-started")] {
            let turn = CopilotTurnEvent::default();
            let guard = turn.activate(None, None)?;
            let submission = forge_provider_api::SubmissionId::try_from("accepted-before-start")?;
            guard
                .send("cursor".into(), Some(submission.clone()), async {
                    Ok("accepted-message".into())
                })
                .await?;
            let event = |id: &str,
                         parent: Option<&str>,
                         kind: &str,
                         data|
             -> anyhow::Result<github_copilot_sdk::SessionEvent> {
                Ok(serde_json::from_value(
                    serde_json::json!({ "id":id, "parentId":parent, "type":kind, "timestamp":"2026-11-04T00:00:00Z", "data":data }),
                )?)
            };
            turn.observe(&event(
                "user",
                None,
                "user.message",
                serde_json::json!({"messageId":"accepted-message", "turnId":native_turn}),
            )?)?;
            let unrelated = event(
                "unrelated-idle",
                None,
                "session.idle",
                serde_json::json!({"aborted":true}),
            )?;
            turn.observe(&unrelated)?;
            turn.observe_idle_at(&unrelated, super::super::turn::IdleOutcome::Aborted)?;
            assert!(
                turn.covered_submissions().is_err(),
                "an unrelated idle certified accepted-input cancellation"
            );
            let pending = turn.idle_settled();
            tokio::pin!(pending);
            assert!(futures_util::poll!(pending.as_mut()).is_pending());
            let cancelled = event(
                "cancelled-idle",
                Some("user"),
                "session.idle",
                serde_json::json!({"aborted":true}),
            )?;
            turn.observe(&cancelled)?;
            turn.observe_idle_at(&cancelled, super::super::turn::IdleOutcome::Aborted)?;
            pending.await?;
            assert_eq!(turn.covered_submissions()?, vec![submission.clone()]);
            assert!(turn.terminal_executions()?.is_empty());
            let scope = forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("no-start"),
                "invocation",
            );
            let (sink, mut stream) = forge_provider_api::events::channel(
                forge_provider_api::BindingId::new("binding")?,
                scope,
            );
            publish_terminal_executions(
                "native-session",
                &turn,
                &sink,
                &ProviderCompletion::default(),
                TerminalPublication::Settled,
            )
            .await?;
            let ProviderEvent::Execution { observation } = stream.recv().await?.unwrap().event
            else {
                panic!("missing consumed cancellation resolution");
            };
            match observation.update() {
                ExecutionUpdate::ResolvedWithoutExecution {
                    covered_submissions,
                    result,
                    ..
                } => {
                    assert_eq!(covered_submissions, &[submission]);
                    assert!(matches!(
                        result,
                        forge_provider_api::ExecutionResult::Cancelled { .. }
                    ));
                }
                other => panic!("fabricated a started native execution: {other:?}"),
            }
            assert!(stream.try_next()?.is_none());
            assert!(matches!(
                turn.settlement_result()?,
                forge_provider_api::ExecutionResult::Cancelled { .. }
            ));
        }
        Ok(())
    }

    /// Finish the main agent independently from attached background work.
    #[test]
    fn root_assistant_idle_finishes_without_waiting_for_background_terminals() {
        let mut event: github_copilot_sdk::SessionEvent =
            serde_json::from_value(serde_json::json!({
                "id":"idle", "type":"assistant.idle", "timestamp":"2026-09-18T00:00:00Z", "data":{}
            }))
            .unwrap();
        assert!(completes_exchange(&event));
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
