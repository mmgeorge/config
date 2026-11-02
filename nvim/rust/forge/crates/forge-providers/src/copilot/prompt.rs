use super::event_decoder::CopilotEventDecoder;
use super::turn::{CopilotTurnEvent, CopilotTurnEventGuard};
use super::{event_stream, rejection, retry};
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
    let session = &session.inner;
    let mut subscription = event_stream::SessionEventStream::new(
        session,
        |_| true,
        trace.clone(),
        options.correlation_id.clone(),
    );
    let mut output = ProviderCompletion {
        provider_session_id: Some(session.id().to_string()),
        capability: options.capability,
        runtime: ProviderRuntime {
            provider: "Copilot CLI".into(),
            model: options.model.clone(),
        },
        ..Default::default()
    };
    let mut decoder =
        CopilotEventDecoder::new(sink.invocation_scope()?.clone(), options.workspace.clone());
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
            .publish_task_snapshot(initial_task_data, "copilot-root".into(), &mut output, sink)
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
    let mut message_input = options.attachments;
    message_input.prompt.push_str(&prompt);
    let message = super::input::message(message_input);
    turn_event_guard
        .send(history.cursor.clone(), async {
            trace.record(
                &options.correlation_id,
                "copilot.session.send",
                json!({"session_id":session.id().to_string(),"prompt":prompt}),
            );
            if let Some(submission) = &options.receipt {
                submission
                    .clone()
                    .record_sending(SubmissionTarget::Copilot {
                        session_id: session.id().to_string(),
                        cursor: history.cursor,
                        prompt: message.prompt.clone(),
                    })
                    .await?;
            }
            control.before_send().await?;
            let receipt = session.send(message).await.context("send Copilot prompt")?;
            if let Some(submission) = &options.receipt {
                submission.clone().record_accepted(receipt.clone()).await?;
            }
            Ok(receipt)
        })
        .await?;
    let mut tool_names = std::collections::HashMap::new();
    let mut execution_turn_stopped = false;
    let mut retry_failure = None;
    let mut retry_idle_deadline = None;
    let mut rejection = rejection::RejectionState::default();
    let mut provider_aborted = false;
    let processing: std::result::Result<(), ProviderError> = async {
        loop {
            tokio::select! {
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
                    trace.record(&options.correlation_id, "copilot.session.event", serde_json::to_value(&provider_event).context("serialize Copilot event trace")?);
                    if completes_exchange(&provider_event) {
                        if !execution_turn_stopped && (provider_aborted || provider_event.data.get("aborted").and_then(Value::as_bool) == Some(true)) {
                            decoder.finish_failure(forge_provider_api::turn::TurnOutcome::Cancelled, sink).await?;
                            return Err(ProviderError::Cancelled);
                        }
                        if decoder.root_address().is_none() {
                            continue;
                        }
                        decoder.decode(&provider_event, &mut output, sink).await?;
                        output.provider_checkpoint_id = if provider_event.ephemeral == Some(true) {
                            provider_event.parent_id.clone()
                        } else {
                            Some(provider_event.id.clone())
                        };
                        break;
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
                            &mut output,
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
                        && (native_replay_safe(&output) && control.status().await?.replay_safe)
                        && let Some(address) = decoder.root_address()
                    {
                        retry_failure = Some(forge_provider_api::retry::RetryableFailure {
                            address, code, detail: provider_event.data.clone(),
                            message: super::decode::session_error(&provider_event, Some(session.id())),
                        });
                        retry_idle_deadline = Some(tokio::time::Instant::now() + std::time::Duration::from_secs(30));
                        continue;
                    }
                    decoder.decode(&provider_event, &mut output, sink).await?;
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
    if let Some(failure) = retry_failure {
        decoder
            .finish_failure(forge_provider_api::turn::TurnOutcome::Failed, sink)
            .await?;
        if decoder.replay_safe()
            && (native_replay_safe(&output) && control.status().await?.replay_safe)
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
        completion: output,
        answered_rejection: rejection.answered,
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
