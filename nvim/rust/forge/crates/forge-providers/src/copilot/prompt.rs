use super::event_decoder::CopilotEventDecoder;
use super::turn::{CopilotTurnEvent, CopilotTurnEventGuard};
use super::{rejection, retry};
use anyhow::{Context, Result};
use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::input::{MessageInput, PromptInput};
use forge_provider_api::submission::{InputReceipt, SubmissionTarget};
use forge_provider_api::trace::ProviderTrace;
use forge_provider_api::turn_control::TurnControl;
use forge_provider_api::{ProviderCapability, ProviderError, ProviderEventSink, ProviderRuntime};
use github_copilot_sdk::rpc::{CommandsInvokeRequest, SlashCommandInvocationResult};
use github_copilot_sdk::session::Session;
use serde_json::{Value, json};
use std::sync::Arc;

pub struct TurnOptions {
    pub correlation_id: String,
    pub workspace: String,
    pub model: Option<String>,
    pub capability: ProviderCapability,
    pub input: PromptInput,
    pub recovered: Vec<PromptInput>,
    pub instructions: String,
    pub repair_caution: Option<String>,
    pub attachments: MessageInput,
    pub receipt: Option<Arc<dyn InputReceipt>>,
    pub background_compaction_observed: bool,
}

pub struct TurnResult {
    pub completion: ProviderCompletion,
    pub answered_rejection: bool,
}

pub async fn run(
    session: &Arc<super::handle::CopilotSession>,
    turn_event: &CopilotTurnEvent,
    turn_event_guard: &CopilotTurnEventGuard<'_>,
    options: TurnOptions,
    sink: &ProviderEventSink,
    control: &dyn TurnControl,
    trace: Arc<dyn ProviderTrace>,
) -> Result<TurnResult, ProviderError> {
    session.ensure_healthy()?;
    let owned_session = session;
    let session = &session.inner;
    let output = ProviderCompletion {
        provider_session_id: Some(session.id().to_string().try_into().map_err(
            |error: forge_provider_api::ProviderSessionIdError| ProviderError::Protocol {
                message: error.to_string(),
            },
        )?),
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
        .context("read initial Copilot task snapshot")?;
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
    let mut recovered = Vec::new();
    for input in &options.recovered {
        recovered.push(input_text(session, input).await?);
    }
    let provider_text = forge_provider_api::input::recovered_text(&provider_text, &recovered);
    let prompt = match &options.repair_caution {
        Some(caution) => format!(
            "{}\n\n{}\n\n{}",
            options.instructions, caution, provider_text
        ),
        None => format!("{}\n\n{}", options.instructions, provider_text),
    };
    let history = session
        .rpc()
        .event_log()
        .tail()
        .await
        .context("capture Copilot pre-submit history boundary")?;
    let ancestry_cursor = history.cursor.clone();
    let mut message_input = options.attachments;
    message_input.prompt.push_str(&prompt);
    let message = super::input::message(message_input);
    turn_event_guard
        .send(history.cursor.clone(), options.receipt.as_ref().map(|receipt| forge_provider_api::lifecycle::SubmissionId::try_from(receipt.id().to_owned())).transpose().map_err(|error| ProviderError::Protocol { message: error.to_string() })?, async {
            owned_session.ensure_healthy()?;
            trace.record(
                &options.correlation_id,
                "copilot.session.send",
                json!({"session_id":session.id().to_string(),"prompt":prompt}),
            );
            if let Some(submission) = &options.receipt {
                submission
                    .clone()
                    .record_sending(SubmissionTarget::native("copilot", session.id().to_string(), serde_json::json!({"cursor":history.cursor,"prompt":message.prompt.clone()})))
                    .await?;
            }
            control.before_send().await?;
            owned_session.ensure_healthy()?;
            let receipt = session.send(message).await.context("send Copilot prompt")?;
            if let Some(submission) = &options.receipt {
                submission.clone().record_accepted(receipt.clone()).await?;
            }
            Ok(receipt)
        })
        .await?;
    let mut tool_names = std::collections::HashMap::new();
    let mut execution_turn_stopped = false;
    let mut foreground_emitted = false;
    let mut retry_failure = None;
    let mut retry_idle_deadline = None;
    let mut rejection = rejection::RejectionState::default();
    let mut provider_aborted = false;
    let processing: std::result::Result<(), ProviderError> = async {
        loop {
            tokio::select! {
                settled = turn_event.idle_settled() => {
                    settled?;
                    break;
                },
                _ = async {
                    match retry_idle_deadline {
                        Some(deadline) => tokio::time::sleep_until(deadline).await,
                        None => std::future::pending::<()>().await,
                    }
                } => {
                    let failure: forge_provider_api::retry::RetryableFailure = retry_failure.take().context("missing failed Copilot turn")?;
                    return Err(ProviderError::Transport {
                        message: format!("{}: provider did not confirm idle within 30 seconds", failure.message),
                    });
                }
                provider_event = subscription.recv() => {
                    let provider_event = provider_event.context("receive Copilot session event")?;
                    turn_event.observe(&provider_event)?;
                    rejection.observe(&provider_event);
                    if provider_event.agent_id.is_none()
                        && provider_event.event_type == "assistant.idle"
                    {
                        provider_aborted |= provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true);
                    }
                    let mut trace_payload = serde_json::to_value(&provider_event).context("serialize Copilot event trace")?;
                    if let Some(fields) = trace_payload.get_mut("data").and_then(Value::as_object_mut) {
                        for (native, normalized) in [("toolCallId", "tool_call_id"), ("requestId", "request_id"), ("turnId", "turn_id")] {
                            if let Some(value) = fields.remove(native) {
                                fields.insert(normalized.into(), value);
                            }
                        }
                    }
                    trace.record(&options.correlation_id, "copilot.session.event", trace_payload);
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
                        turn_event.observe_idle_at(&provider_event, if aborted { super::turn::IdleOutcome::Aborted } else { super::turn::IdleOutcome::Completed })?;
                        if aborted {
                            tokio::time::timeout(std::time::Duration::from_secs(15), turn_event.idle_settled()).await
                                .map_err(|_| ProviderError::DeliveryUncertain { message: "Copilot aborted before delivery settled".into() })??;
                            decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink).await?;
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
                            .context("read Copilot task snapshot")?;
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
                        && let Some(code) = retry::transient_code(&provider_event.data)
                        && provider_event.agent_id.is_none()
                        && decoder.replay_safe()
                        && (native_replay_safe(output) && control.status().await?.replay_safe)
                        && let Some(address) = decoder.root_address()
                    {
                        retry_failure = Some(forge_provider_api::retry::RetryableFailure {
                            address, code, detail: provider_event.data.clone(),
                            message: super::decode::session_error(&provider_event, Some(session.id())),
                        });
                        retry_idle_deadline = Some(tokio::time::Instant::now() + std::time::Duration::from_secs(30));
                        continue;
                    }
                    decoder.decode(&provider_event, output, sink).await?;
                    if !foreground_emitted
                        && let (Some((_, event_id)), Some(root)) = (turn_event.foreground_anchor()?, decoder.root_address()) {
                            let address = forge_provider_api::TurnAddress { turn_id: event_id.into(), ..root };
                            let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                                address.scope.clone(), forge_provider_api::lifecycle::ExecutionUpdate::Started { address },
                            ).map_err(|error| ProviderError::Protocol { message: error.to_string() })?;
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
                            if control.tool_finished(name.as_deref()) {
                                session.abort().await.context("finish Copilot host-requested turn")?;
                                execution_turn_stopped = true;
                            }
                        }
                    }
                    if terminal_error {
                        let detail = super::decode::session_error(&provider_event, Some(session.id()));
                        decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink).await?;
                        return Err(ProviderError::Provider {
                            code: provider_event.data.get("errorType").and_then(Value::as_str).map(str::to_owned),
                            message: detail,
                        });
                    }
                }
                result = control.receive() => { result?; }

            }
        }
        Ok(())
        }.await;
    control.close().await?;
    let host_status = control.status().await?;
    if let Err(error) = processing {
        let outcome = if matches!(error, ProviderError::Cancelled) {
            forge_provider_api::turn::TurnOutcome::Cancelled
        } else {
            forge_provider_api::turn::TurnOutcome::Failed
        };
        decoder.finish_failure(outcome, sink).await?;
        return Err(error);
    }
    if turn_event.needs_ancestry()? {
        restore_durable_ancestry(
            session,
            turn_event,
            ancestry_cursor,
            trace.as_ref(),
            &options.correlation_id,
        )
        .await?;
    }
    subscription.ensure_healthy()?;
    if let Some(failure) = retry_failure {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        if decoder.replay_safe()
            && (native_replay_safe(output) && control.status().await?.replay_safe)
        {
            return Err(ProviderError::Retryable(failure.into()));
        }
        return Err(ProviderError::Provider {
            code: Some(failure.code),
            message: failure.message,
        });
    }
    if host_status.cancelled {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink)
            .await?;
        return Err(ProviderError::Cancelled);
    }
    if !execution_turn_stopped && let Err(error) = rejection.finish(host_status.control_handoff) {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(ProviderError::Rejected {
            message: format!("{error:#}"),
        });
    }
    if let Some(error) = &host_status.error {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        return Err(ProviderError::Protocol {
            message: error.clone(),
        });
    }
    if execution_turn_stopped {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Interrupted, sink)
            .await?;
    } else {
        decoder.finish_success(sink).await?;
    }
    Ok(TurnResult {
        completion: output.clone(),
        answered_rejection: rejection.answered,
    })
}

/// Fill filtered durable links while retaining the live idle as the terminal authority.
async fn restore_durable_ancestry(
    session: &Session,
    turn: &CopilotTurnEvent,
    mut cursor: String,
    trace: &dyn ProviderTrace,
    correlation: &str,
) -> Result<(), ProviderError> {
    use github_copilot_sdk::rpc::{
        EventLogReadRequest, EventsAgentScope, EventsCursorStatus, EventsReadDirection,
    };
    let read = async {
        let mut cursors = std::collections::HashSet::new();
        let mut count = 0usize;
        loop {
            anyhow::ensure!(
                cursors.insert(cursor.clone()),
                "Copilot ancestry cursor did not advance"
            );
            trace.record(
                correlation,
                "copilot.ancestry.lookup",
                json!({
                    "phase":"page_requested", "cursor":cursor, "page":cursors.len(),
                    "native_session_id":session.id(),
                }),
            );
            let page = session
                .rpc()
                .event_log()
                .read(EventLogReadRequest {
                    agent_ids: None,
                    agent_scope: Some(EventsAgentScope::All),
                    cursor: Some(cursor.clone()),
                    direction: Some(EventsReadDirection::Forward),
                    include_ephemeral: Some(false),
                    max: Some(1000),
                    types: Some(json!("*")),
                    wait_ms: Some(0),
                })
                .await
                .context("read Copilot durable terminal ancestry")?;
            anyhow::ensure!(
                page.cursor_status == EventsCursorStatus::Ok,
                "Copilot terminal ancestry cursor expired"
            );
            count = count
                .checked_add(page.events.len())
                .context("Copilot ancestry event count overflow")?;
            anyhow::ensure!(
                count <= 16_384 && cursors.len() <= 64,
                "Copilot terminal ancestry exceeds its read bound"
            );
            trace.record(correlation, "copilot.ancestry.lookup", json!({
                "phase":"page_received", "cursor":cursor, "next_cursor":page.cursor,
                "cursor_status":page.cursor_status, "has_more":page.has_more,
                "event_count":page.events.len(), "total_event_count":count,
                "event_types":page.events.iter().map(|event| event.event_type.as_str()).collect::<std::collections::BTreeSet<_>>(),
            }));
            turn.restore_ancestry(&page.events)?;
            if !page.has_more {
                trace.record(
                    correlation,
                    "copilot.ancestry.lookup",
                    json!({
                        "phase":"finished", "start_ancestry_missing":turn.needs_ancestry()?,
                        "total_event_count":count,
                    }),
                );
                return Ok::<_, anyhow::Error>(());
            }
            cursor = page.cursor;
        }
    };
    tokio::time::timeout(std::time::Duration::from_secs(30), read)
        .await
        .context("Copilot terminal ancestry lookup timed out")
        .and_then(|result| result)
        .map_err(|error| {
            trace.record(
                correlation,
                "copilot.ancestry.lookup",
                json!({
                    "phase":"failed", "error":format!("{error:#}"),
                }),
            );
            ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol {
                    message: format!("cannot certify Copilot terminal ancestry: {error:#}"),
                },
            )
        })
}

fn native_replay_safe(output: &ProviderCompletion) -> bool {
    !output.evidence.tool_called
        && !output.evidence.workspace_changed
        && !output.evidence.structured_complete
        && !output.evidence.structured_blocked
}

async fn input_text(session: &Arc<Session>, input: &PromptInput) -> Result<String> {
    Ok(match input {
        PromptInput::Text(text) => text.clone(),
        PromptInput::Skill { name, arguments } => {
            let skill_available = session
                .rpc()
                .skills()
                .list()
                .await
                .with_context(|| format!("resolve Copilot skill ${name}"))?
                .skills
                .into_iter()
                .any(|skill| skill.name == *name && skill.enabled && skill.user_invocable);
            anyhow::ensure!(
                skill_available,
                "Copilot skill ${name} is unavailable or disabled"
            );
            match session
                .rpc()
                .commands()
                .invoke(CommandsInvokeRequest {
                    origin: None,
                    name: name.clone(),
                    input: (!arguments.is_empty()).then(|| arguments.clone()),
                })
                .await
                .with_context(|| format!("invoke Copilot skill ${name}"))?
            {
                SlashCommandInvocationResult::AgentPrompt(result) => result.prompt,
                _ => anyhow::bail!("Copilot skill ${name} did not produce an agent prompt"),
            }
        }
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
