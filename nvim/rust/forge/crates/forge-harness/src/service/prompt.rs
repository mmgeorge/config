//! Normalize legacy prompt transport into one durable user-input command.

use super::*;
use crate::exchange::command::{
    Command, CommandId, CommandRequest, InputCommand, InputControl, InputId,
};
use crate::input::{InputAction, InputIntent, InputMessage, InputOperation, InputPurpose};
use serde::Deserialize;

/// Retain presentation completion observers after their original RPC caller disconnects.
#[derive(Default)]
pub(super) struct PromptWorkers {
    tasks: Vec<tokio::task::JoinHandle<()>>,
    settlements: Vec<tokio::sync::oneshot::Receiver<()>>,
}

pub(super) struct PromptAdmission {
    pub(super) presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    pub(super) document: forge_buffer::identity::DocumentId,
    pub(super) token: u64,
}

impl Drop for PromptAdmission {
    fn drop(&mut self) {
        if let Ok(mut presentation) = self.presentation.lock() {
            let _ = presentation.complete_submission(&self.document, self.token, false);
        }
    }
}

/// Keep legacy composer identity outside the durable input domain.
#[derive(Deserialize)]
struct ComposerSubmission {
    document: Option<forge_buffer::identity::DocumentId>,
    token: u64,
}

/// Decode supported prompt fields without allowing callers to manufacture delivery state.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PromptFields {
    text: String,
    #[serde(default)]
    attachments: Vec<Value>,
    #[serde(default)]
    queued: bool,
    intent: Option<InputIntent>,
    input_id: Option<InputId>,
    operation_id: Option<CommandId>,
    config: Option<Value>,
    model: Option<String>,
    effort: Option<String>,
    service_tier: Option<crate::provider::ServiceTier>,
    context_window: Option<String>,
    submission: Option<ComposerSubmission>,
    target: Option<Value>,
}

/// Admit exactly one normalized user input and retain only its presentation observer.
pub(super) async fn apply(
    controller: &Arc<SessionController>,
    request: Request,
    sink: &MessageSender,
) -> Result<()> {
    let (command, composer) = normalize(controller.observer.session_id(), request.params)?;
    let input_id = match &command.command {
        Command::Input(InputCommand::Submit(input)) => input.id.as_str().to_owned(),
        _ => unreachable!("prompt normalization creates user input submission"),
    };
    let prior = controller
        .exchange_owner
        .command_status(command.id.clone())
        .await?;
    let mut workers = controller.prompt_workers.lock().await;
    let mut index = 0;
    while index < workers.tasks.len() {
        if workers.tasks[index].is_finished() {
            workers
                .tasks
                .swap_remove(index)
                .await
                .context("prompt presentation worker panicked")?;
        } else {
            index += 1;
        }
    }
    workers.settlements.retain_mut(|settled| {
        matches!(
            settled.try_recv(),
            Err(tokio::sync::oneshot::error::TryRecvError::Empty)
        )
    });
    let mut admission = None;
    if prior.is_none()
        && let Some(ComposerSubmission {
            document: Some(document),
            token,
        }) = composer
    {
        controller
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .begin_submission(
                &document,
                token,
                match &command.command {
                    Command::Input(InputCommand::Submit(input)) => &input.text,
                    _ => unreachable!("prompt normalization creates user input submission"),
                },
            )?;
        admission = Some(PromptAdmission {
            presentation: controller.presentation.clone(),
            document,
            token,
        });
    }
    let controller = Arc::clone(controller);
    let sink = sink.clone();
    let (completed, response) = tokio::sync::oneshot::channel();
    let (settled, settlement) = tokio::sync::oneshot::channel();
    workers.settlements.push(settlement);
    let caller_admission = crate::admission::take_notification();
    workers.tasks.push(tokio::spawn(async move {
        let result =
            crate::admission::observe(caller_admission, controller.execute_command(command, &sink))
                .await;
        let projected: Result<(Option<SessionEvent>, Response)> = (|| {
            let event = if let Some(admission) = admission {
                let transition = {
                    let mut presentation = admission
                        .presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?;
                    presentation.complete_submission(
                        &admission.document,
                        admission.token,
                        result.is_ok(),
                    )?
                };
                transition.map(|transition| SessionEvent {
                    session_id: controller.observer.session_id().into(),
                    event: "provider_event".into(),
                    payload: json!({"kind":"prompt_submission", "data":transition}),
                })
            } else {
                None
            };
            let response = match result {
                Ok(result) => {
                    let mut value = controller
                        .observer
                        .snapshot()
                        .ok()
                        .map(serde_json::to_value)
                        .transpose()?
                        .unwrap_or_else(|| json!({}));
                    value["input_id"] = json!(input_id);
                    value["accepted"] = json!(true);
                    value["command_result"] = serde_json::to_value(result)?;
                    Response::success(request.id, value)?
                }
                Err(error) => Response::failure(request.id, error.code(), error.to_string()),
            };
            Ok((event, response))
        })();
        let trace = controller.trace.clone();
        let session = controller.observer.session_id().to_owned();
        drop(controller);
        // Native/domain ownership has settled; outbound backpressure owns only these prepared UI messages.
        let _ = settled.send(());
        let _ = completed.send(());
        let delivered: Result<()> = async {
            let (event, response) = projected?;
            if let Some(event) = event {
                sink.send_event(event).await?;
            }
            sink.send_control_response(response).await?;
            Ok(())
        }
        .await;
        if let Err(error) = delivered {
            trace.record(
                &session,
                "prompt.presentation_failed",
                json!({"reason":format!("{error:#}")}),
            );
        }
    }));
    drop(workers);
    response
        .await
        .context("prompt presentation observer ended without a response")?;
    Ok(())
}

/// Decode model choices and immutable identity before handing input to the owner.
fn normalize(
    session_id: &str,
    mut params: Value,
) -> Result<(CommandRequest, Option<ComposerSubmission>)> {
    let object = params
        .as_object_mut()
        .context("prompt submission requires an object")?;
    if let Some(identity) = object.remove("_command") {
        let id = identity
            .get("id")
            .cloned()
            .context("command identity is required")?;
        if let Some(existing) = object.get("operation_id") {
            ensure!(existing == &id, "conflicting command identities");
        }
        object.insert("operation_id".into(), id);
    }
    let context_present = object.contains_key("context_window");
    let PromptFields {
        text,
        attachments,
        queued,
        intent,
        input_id,
        operation_id,
        config,
        model,
        effort,
        service_tier,
        context_window,
        submission,
        target,
    } = serde_json::from_value(params)?;
    ensure!(!text.trim().is_empty(), "prompt requires nonempty text");
    crate::protocol::validate_root_control(target.as_ref())?;
    let intent = match (queued, intent) {
        (true, Some(InputIntent::Auto)) => {
            anyhow::bail!("queued prompt conflicts with automatic input intent")
        }
        (true, _) => InputIntent::Queue,
        (false, intent) => intent.unwrap_or_default(),
    };
    let operation_id = operation_id.unwrap_or(CommandId::new(match (&input_id, &submission) {
        (Some(input), _) => format!("prompt:{}", input.as_str()),
        (
            _,
            Some(ComposerSubmission {
                document: Some(document),
                token,
            }),
        ) => format!("prompt:{session_id}:{}:{token}", document.0),
        _ => uuid::Uuid::new_v4().to_string(),
    })?);
    let input_id = input_id.unwrap_or(InputId::new(format!(
        "prompt-input:{}",
        operation_id.as_str()
    ))?);
    let mut config = match config {
        Some(Value::Object(config)) => config,
        Some(_) => anyhow::bail!("prompt config must be an object"),
        None => serde_json::Map::new(),
    };
    for (key, value) in [
        ("model", model.map(Value::String)),
        ("effort", effort.map(Value::String)),
        (
            "service_tier",
            service_tier.map(serde_json::to_value).transpose()?,
        ),
        (
            "context_window",
            context_present.then(|| context_window.map(Value::String).unwrap_or(Value::Null)),
        ),
    ] {
        if let Some(value) = value {
            if let Some(existing) = config.get(key) {
                ensure!(existing == &value, "conflicting prompt model choices");
            }
            config.insert(key.into(), value);
        }
    }
    let mut command = CommandRequest::from_input(InputOperation {
        id: operation_id.as_str().into(),
        control_revision: 0,
        after_control: None,
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(crate::input::InputDraft {
                id: input_id.as_str().into(),
                text,
                purpose: InputPurpose::User,
                attachments,
                config: (!config.is_empty()).then_some(Value::Object(config)),
                intent,
            })),
        },
    })?;
    command.input_control = InputControl::AtExecution;
    Ok((command, submission))
}

impl SessionController {
    /// Wait for domain/composer settlement while retaining backpressured UI delivery independently.
    pub(super) async fn finish_prompt_workers(&self) -> Result<()> {
        let mut workers = self.prompt_workers.lock().await;
        while let Some(settlement) = workers.settlements.last_mut() {
            settlement
                .await
                .context("prompt observer ended before domain settlement")?;
            workers.settlements.pop();
        }
        let mut index = 0;
        while index < workers.tasks.len() {
            if workers.tasks[index].is_finished() {
                workers
                    .tasks
                    .swap_remove(index)
                    .await
                    .context("prompt presentation worker panicked")?;
            } else {
                index += 1;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Prompt aliases cannot manufacture internal input purposes or native delivery metadata.
    #[test]
    fn prompt_normalization_rejects_forged_authority_and_conflicting_choices() {
        for params in [
            json!({"text":"work", "purpose":{"kind":"task_entry"}}),
            json!({"text":"work", "submission_id":"forged"}),
            json!({"text":"work", "queued":true, "intent":"auto"}),
            json!({"text":"work", "model":"one", "config":{"model":"two"}}),
            json!({"text":"work", "config":{"unsupported":true}}),
            json!({"text":" "}),
        ] {
            assert!(normalize("session", params).is_err());
        }
    }
}
