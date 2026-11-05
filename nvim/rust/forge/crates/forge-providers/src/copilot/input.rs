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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// A malformed native observation after a real SDK handoff cannot become preparation rejection.
    #[tokio::test]
    async fn protocol_failure_after_native_handoff_retains_uncertain_observation_effects() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (client, peer) = super::super::test_peer::client(|request| {
                let result = match request["method"].as_str().unwrap() {
                    "session.create" => json!({"sessionId":"post-dispatch", "workspacePath":std::env::temp_dir()}),
                    "session.options.update" | "session.detach" => json!({"success":true}),
                    "session.send" => json!({"messageId":"accepted-input"}),
                    method => panic!("unexpected handoff metadata fixture method: {method}"),
                };
                vec![super::super::test_peer::response(request, result)]
            });
            let session = client.create_session(github_copilot_sdk::SessionConfig::default().with_session_id(github_copilot_sdk::SessionId::new("post-dispatch"))).await.unwrap();
            let turn = super::super::turn::CopilotTurnEvent::default();
            let scope = forge_provider_api::InvocationScope::new(forge_provider_api::ExecutionOwner::root("post-dispatch"), "invocation");
            let (sink, _events) = forge_provider_api::events::channel(forge_provider_api::BindingId::new("binding").unwrap(), scope.clone());
            let guard = turn.activate(Some(sink.clone()), None).unwrap();
            turn.steer_native(None, None, |authority| async {
                send(&session, MessageOptions::new("accepted request"), None, authority).await.map_err(anyhow::Error::new)
            }).await.unwrap();
            let mut decoder = super::super::event_decoder::CopilotEventDecoder::new(scope, ".".into());
            let mut output = forge_provider_api::completion::ProviderCompletion {
                provider_session_id: Some("post-dispatch".to_owned().try_into().unwrap()), ..Default::default()
            };
            let malformed = serde_json::from_value(json!({"id":"malformed", "type":"assistant.message_delta", "timestamp":"2026-11-04T00:00:00Z", "data":{"deltaContent":"no message identity"}})).unwrap();
            let error = decoder.decode(&malformed, &mut output, &sink).await.unwrap_err();
            assert!(matches!(error, forge_provider_api::ProviderError::Failure(failure)
                if failure.operation() == forge_provider_api::ProviderOperation::InputSubmit
                && failure.kind() == forge_provider_api::ProviderFailureKind::Protocol
                && failure.phase() == forge_provider_api::ProviderPhase::Observation
                && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain));
            drop(guard);
            session.disconnect().await.unwrap(); client.stop().await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.send").count(), 1);
        }).await.expect("post-dispatch metadata fixture timed out");
    }

    /// Dispatch is observable and abort reaches the writer while native send acknowledgement is held.
    #[tokio::test]
    async fn tracked_handoff_does_not_wait_for_native_acknowledgement() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let mut held = None;
            let (client, peer) = super::super::test_peer::client(move |request| {
                let result = match request["method"].as_str().unwrap() {
                    "session.create" => {
                        json!({"sessionId":"handoff", "workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" | "session.detach" => json!({"success":true}),
                    "session.send" => {
                        held = Some(request.clone());
                        return Vec::new();
                    }
                    "session.abort" => {
                        let sending = held.take().expect("abort overtook native dispatch");
                        return vec![
                            super::super::test_peer::response(request, json!({})),
                            super::super::test_peer::response(
                                &sending,
                                json!({"messageId":"accepted"}),
                            ),
                        ];
                    }
                    method => panic!("unexpected native method: {method}"),
                };
                vec![super::super::test_peer::response(request, result)]
            });
            let session = client
                .create_session(
                    github_copilot_sdk::SessionConfig::default()
                        .with_session_id(github_copilot_sdk::SessionId::new("handoff")),
                )
                .await
                .unwrap();
            let turn = super::super::turn::CopilotTurnEvent::default();
            let guard = turn.activate(None, None).unwrap();
            let scope = forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("handoff"),
                "invocation",
            );
            let submission = forge_provider_api::SubmissionId::try_from("submission").unwrap();
            let (dispatch, observer) = forge_provider_api::submission::DispatchReporter::channel(
                forge_provider_api::BindingId::new("binding").unwrap(),
                scope,
                submission.clone(),
                forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            );
            let sending = turn.steer_native(None, Some(submission), |authority| async {
                send(
                    &session,
                    MessageOptions::new("request"),
                    Some(&dispatch),
                    authority,
                )
                .await
                .map_err(anyhow::Error::new)
            });
            tokio::pin!(sending);
            assert!(futures_util::poll!(sending.as_mut()).is_pending());
            assert!(
                observer.current().unwrap().is_some(),
                "writer handoff remained tied to native acknowledgement"
            );
            turn.request_stop().unwrap();
            session.abort().await.unwrap();
            sending.await.unwrap();
            turn.finish_pending_sends().await.unwrap();
            drop(guard);
            session.disconnect().await.unwrap();
            client.stop().await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(
                requests
                    .iter()
                    .filter(|request| request["method"] == "session.send")
                    .count(),
                1
            );
        })
        .await
        .expect("tracked SDK handoff fixture timed out");
    }
}
