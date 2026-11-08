use super::event_decoder::CopilotEventDecoder;
use super::turn::{CopilotTurnEvent, CopilotTurnEventGuard};
use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::input::{MessageInput, PromptInput};
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::turn_control::TurnControl;
use forge_provider_api::{
    ProviderCapability, ProviderError, ProviderRuntime,
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

pub async fn run(
    session: &Arc<super::handle::CopilotSession>,
    turn_event_guard: &CopilotTurnEventGuard<'_>,
    native_start: &forge_provider_api::NativeTurnStart,
    mut options: TurnOptions,
    owner_sink: &forge_provider_api::ProviderEventSink,
    control: &dyn TurnControl,
    native_callbacks: Arc<dyn super::provider::NativeCallbackRouter>,
) -> Result<forge_provider_api::TurnResult, ProviderError> {
    let writer = owner_sink.activity_writer();
    let sink = &writer;
    let turn_event = turn_event_guard.events();
    match (&options.submission_id, &options.receipt) {
        (Some(submission), Some(receipt)) if submission == receipt.id() => {
            let coverage = options
                .recovered
                .iter()
                .map(|input| input.covered_input.clone())
                .chain(std::iter::once(forge_provider_api::CoveredInput::new(
                    submission.clone(),
                    receipt.attempt_id()?,
                )))
                .collect();
            normalize_internal(
                turn_event.register_combined_inputs(&options.scope, submission, coverage),
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            )?;
        }
        (None, None) => {}
        (Some(_), Some(_) | None) | (None, Some(_)) => {
            return Err(native_failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            ));
        }
    }
    session.ensure_healthy()?;
    let owned_session = session;
    let session = &session.inner;
    let output = ProviderCompletion {
        provider_session_id: Some(session.id().to_string().try_into().map_err(|_| {
            native_failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol)
        })?),
        capability: options.capability,
        runtime: ProviderRuntime {
            provider: "Copilot CLI".into(),
            model: options.model.clone(),
        },
        ..Default::default()
    };
    let decoder =
        CopilotEventDecoder::new(sink.invocation_scope()?.clone(), options.workspace.clone())
            .with_callbacks(native_callbacks)
            .with_turn_owner(owner_sink.clone());
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
            native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport)
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
                            options.scope.thread_id.clone(),
                            options.submission_id.clone().ok_or_else(|| {
                                native_failure(
                                    ProviderOperation::TurnStart,
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
                                    ProviderOperation::TurnStart,
                                    ProviderFailureKind::Rejected,
                                )
                            })?,
                            submission.attempt_id()?,
                            options.scope.thread_id.clone(),
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
    if sending.is_ok() {
        owned_session.release_mcp_loading().await;
    }
    if let Err(error) = sending {
        let acceptance_error =
            if super::operation::current().is_some_and(|operation| !operation.dispatched()) {
                error
                    .downcast_ref::<ProviderError>()
                    .cloned()
                    .unwrap_or_else(|| {
                        native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Unknown)
                    })
            } else {
                error
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
                    .unwrap_or(ProviderError::DeliveryUncertain)
            };
        let acceptance_error =
            if super::operation::current().is_some_and(|operation| operation.dispatched()) {
                super::error::after_input_dispatch(acceptance_error, ProviderOperation::TurnStart)
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
    let mut provider_aborted = false;
    let processing: std::result::Result<super::turn::IdleOutcome, ProviderError> = async {
        loop {
            tokio::select! {
                settled = turn_event.idle_settled() => {
                    normalize_internal(settled, ProviderOperation::TurnStart, ProviderFailureKind::Protocol)?;
                    return normalize_internal(turn_event.terminal_outcome(), ProviderOperation::TurnStart, ProviderFailureKind::Protocol);
                },
                provider_event = subscription.recv() => {
                    let provider_event = provider_event.map_err(|_| native_failure(ProviderOperation::ThreadClose, ProviderFailureKind::Transport))?;
                    let observed_at_ms = subscription.observed_at(&provider_event);
                    normalize_internal(turn_event.observe(&provider_event), ProviderOperation::TurnStart, ProviderFailureKind::Protocol)?;
                    // An abort that discards still-queued input is terminal: Copilot emits no idle.
                    if provider_event.agent_id.is_none()
                        && provider_event.event_type == "abort"
                        && normalize_internal(turn_event.accepted_inputs_unconsumed(), ProviderOperation::TurnStart, ProviderFailureKind::Protocol)?
                    {
                        return Ok(super::turn::IdleOutcome::Aborted);
                    }
                    if provider_event.agent_id.is_none()
                        && provider_event.event_type == "user.message"
                        && let Some(submission) = provider_event
                            .data
                            .get("messageId")
                            .and_then(Value::as_str)
                            .map(|message| turn_event.accepted_submission(message))
                            .transpose()
                            .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?
                            .flatten()
                    {
                        decoder.consumed_input(&provider_event, submission, sink).await?;
                    }
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
                        decoder.decode_at(&provider_event, observed_at_ms, output, sink).await?;
                        output.provider_checkpoint_id = if provider_event.ephemeral == Some(true) {
                            provider_event.parent_id.clone()
                        } else {
                            Some(provider_event.id.clone())
                        };
                        let outcome = if execution_turn_stopped { super::turn::IdleOutcome::HostHandoff }
                            else if aborted { super::turn::IdleOutcome::Aborted }
                            else { super::turn::IdleOutcome::Completed };
                        normalize_internal(turn_event.observe_idle_timestamp(&provider_event, outcome, observed_at_ms), ProviderOperation::TurnStart, ProviderFailureKind::Protocol)?;
                        continue;
                    }
                    if provider_event.event_type == "session.todos_changed" {
                        let task_snapshot = session
                            .rpc()
                            .plan()
                            .read_sql_todos_with_dependencies()
                            .await
                            .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))?;
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
                    if matches!(provider_event.event_type.as_str(), "session.compaction_start" | "session.compaction_complete")
                        && options.background_compaction_observed && provider_event.agent_id.is_none() {
                        continue;
                    }
                    if provider_event.agent_id.is_none() && provider_event.event_type == "assistant.turn_start" {
                        native_start.observed(observed_at_ms).await?;
                    }
                    decoder.decode_at(&provider_event, observed_at_ms, output, sink).await?;
                    if provider_event.agent_id.is_none() && !execution_turn_stopped {
                        let call_id = provider_event.data.get("toolCallId").and_then(Value::as_str);
                        if provider_event.event_type == "tool.execution_start"
                            && let (Some(call_id), Some(name)) = (call_id, provider_event.data.get("toolName").and_then(Value::as_str)) {
                            tool_names.insert(call_id.to_owned(), name.to_owned());
                        }
                        if provider_event.event_type == "tool.execution_complete" {
                            let name = call_id.and_then(|id| tool_names.remove(id));
                        if control.tool_finished(&options.scope, name.as_deref()) {
                                session.abort().await.map_err(|_| native_failure(ProviderOperation::TurnInterrupt, ProviderFailureKind::Transport))?;
                                execution_turn_stopped = true;
                            }
                        }
                    }

                }
                result = control.receive(&options.scope) => { result?; }

            }
        }
        }.await;
    let native_cancelled = matches!(processing, Ok(super::turn::IdleOutcome::Aborted));
    if let Err(error) = processing {
        decoder.interrupt_content(sink).await?;
        return Err(super::error::after_input_dispatch(
            error,
            ProviderOperation::TurnStart,
        ));
    }
    subscription.ensure_healthy()?;
    if native_cancelled && decoder.root_address().is_none() {
        return Err(ProviderError::Cancelled);
    }
    let native = normalized_result(turn_event, output)?;
    decoder.finish_native(native.outcome(), sink).await?;
    drop(active_context);
    Ok(native)
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

/// Freeze the retained native idle outcome and its exact accepted input attempts once.
fn normalized_result(
    turn_event: &CopilotTurnEvent,
    completion: &ProviderCompletion,
) -> Result<forge_provider_api::TurnResult, ProviderError> {
    let outcome = match normalize_internal(
        turn_event.terminal_outcome(),
        ProviderOperation::TurnStart,
        ProviderFailureKind::Protocol,
    )? {
        super::turn::IdleOutcome::Completed => forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        },
        super::turn::IdleOutcome::Aborted | super::turn::IdleOutcome::HostHandoff => {
            forge_provider_api::turn::TurnOutcome::Interrupted
        }
    };
    let inputs = normalize_internal(
        turn_event.covered_inputs(),
        ProviderOperation::TurnStart,
        ProviderFailureKind::Protocol,
    )?;
    let checkpoint = completion
        .provider_checkpoint_id
        .clone()
        .map(forge_provider_api::CheckpointId::new)
        .transpose()
        .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
    let completed_at_ms = normalize_internal(
        turn_event.completed_at_ms(),
        ProviderOperation::TurnStart,
        ProviderFailureKind::Protocol,
    )?;
    forge_provider_api::TurnResult::new(
        outcome,
        completed_at_ms,
        None,
        inputs,
        Vec::new(),
        checkpoint,
    )
    .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
}

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
        owner: forge_provider_api::ThreadId,
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
        owner: forge_provider_api::ThreadId,
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
    owner: forge_provider_api::ThreadId,
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
    owner: forge_provider_api::ThreadId,
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
    let bytes = serde_json::to_vec(&evidence)
        .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
    forge_provider_api::RecoveryToken::new(bytes)
        .map_err(|_| native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
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
                    native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                })?
                .skills
                .into_iter()
                .any(|skill| skill.name == *name && skill.enabled && skill.user_invocable);
            if !skill_available {
                return Err(native_failure(
                    ProviderOperation::TurnStart,
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
                    native_failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
                })? {
                SlashCommandInvocationResult::AgentPrompt(result) => result.prompt,
                _ => {
                    return Err(native_failure(
                        ProviderOperation::TurnStart,
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

    /// Publication cannot manufacture success before any correlated native terminal exists.
    #[tokio::test]
    async fn missing_native_terminal_does_not_publish_or_return_success() -> anyhow::Result<()> {
        let turn = CopilotTurnEvent::default();
        let _guard = turn.activate(None, None)?;
        let scope = forge_provider_api::InvocationScope::new(
            forge_provider_api::testing::root_thread("missing-terminal"),
            "exchange",
        );
        let (_sink, mut stream) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("binding")?,
            scope,
        );
        assert!(normalized_result(&turn, &ProviderCompletion::default()).is_err());
        assert!(stream.try_next()?.is_none());
        Ok(())
    }
}
