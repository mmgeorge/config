use forge_provider_api::input::{FileAttachment, MessageInput};
use github_copilot_sdk::types::{Attachment, MessageOptions};

/// Encode explicitly admitted files without discovering attachments in prompt text.
pub fn message(input: MessageInput) -> MessageOptions {
    let MessageInput {
        prompt,
        attachments,
    } = input;
    let message = MessageOptions::new(prompt)
        .with_agent_mode(github_copilot_sdk::types::AgentMode::Interactive);
    if attachments.is_empty() {
        return message;
    }
    message.with_attachments(
        attachments
            .into_iter()
            .map(|attachment| {
                let FileAttachment { path, label, .. } = attachment;
                Attachment::File {
                    path,
                    display_name: Some(label),
                    line_range: None,
                }
            })
            .collect(),
    )
}

/// Hand one tracked native input to the SDK writer before waiting for its acknowledgement.
pub async fn send(
    session: &github_copilot_sdk::session::Session,
    message: MessageOptions,
    dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
    authority: super::turn::NativeSendAuthority<'_>,
) -> Result<String, forge_provider_api::ProviderError> {
    use std::future::Future;
    use std::task::Poll;
    let mut params = serde_json::json!({ "sessionId": session.id(), "prompt": message.prompt, "agentMode": "interactive" });
    if let Some(attachments) = message.attachments {
        params["attachments"] = serde_json::to_value(attachments)
            .map_err(|_| forge_provider_api::ProviderError::NotConsumed)?;
    }
    if let Some(mode) = message.mode {
        params["mode"] = serde_json::to_value(mode)
            .map_err(|_| forge_provider_api::ProviderError::NotConsumed)?;
    }
    let sending = session.client().call("session.send", Some(params));
    tokio::pin!(sending);
    // Client::call commits its writer-actor frame before its first yield. Poll it
    // once before confirming dispatch; response latency cannot block cancellation.
    let immediate = std::future::poll_fn(|context| {
        Poll::Ready(
            authority
                .commit(|| {
                    super::operation::dispatch()?;
                    Ok(match sending.as_mut().poll(context) {
                        Poll::Ready(result) => Some(result),
                        Poll::Pending => None,
                    })
                })
                .and_then(|result| result),
        )
    })
    .await?;
    if immediate.as_ref().is_none_or(Result::is_ok)
        && let Some(dispatch) = dispatch
    {
        dispatch.confirm();
    }
    let reply = match immediate {
        Some(result) => result,
        None => sending.await,
    }
    .map_err(|_| forge_provider_api::ProviderError::DeliveryUncertain)?;
    reply
        .get("messageId")
        .and_then(serde_json::Value::as_str)
        .filter(|id| !id.is_empty())
        .map(str::to_owned)
        .ok_or(forge_provider_api::ProviderError::DeliveryUncertain)
}
