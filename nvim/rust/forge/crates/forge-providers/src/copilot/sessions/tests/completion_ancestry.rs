use super::super::CopilotSessions;
use super::Trace;
use super::support::{RecoveryPeer, recovery_session};
use async_trait::async_trait;
use forge_provider_api::ProviderError;
use std::sync::Arc;

struct CompletionControl;

#[async_trait]
impl forge_provider_api::turn_control::TurnControl for CompletionControl {
    async fn before_send(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn receive(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        std::future::pending().await
    }
    async fn close(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn status(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::turn_control::HostTurnStatus, ProviderError> {
        Ok(forge_provider_api::turn_control::HostTurnStatus {
            replay_safe: true,
            control_handoff: false,
            cancelled: false,
            error: None,
        })
    }
    fn tool_finished(&self, _: &forge_provider_api::InvocationScope, _: Option<&str>) -> bool {
        false
    }
}

/// Complete repeated real SDK turns when live ancestry omits its durable usage node.
#[tokio::test]
async fn sdk_turn_completion_survives_omitted_ancestry_and_reused_zero_turn_id() {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
            let runtime = crate::copilot::runtime::CopilotRuntime::new(vec!["ancestry-peer".into()]);
            let peer = RecoveryPeer::attach_with_turns(&runtime, "1.0.103", true).await;
            let client = runtime.client(".").await.unwrap();
            let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
            let owner = forge_provider_api::ExecutionOwner::root("ancestry-owner");
            let session = recovery_session(&sessions, client.clone(), &owner).await;
            let turn = sessions.turn(&owner.clone()).await;
            let capability = crate::registry::descriptor(&crate::registry::select("copilot").unwrap()).unwrap().capability;
            let mut completed = Vec::new();
            for ordinal in 0..2 {
                let scope = forge_provider_api::InvocationScope::new(owner.clone(), format!("ancestry-{ordinal}"));
                let binding_id = forge_provider_api::BindingId::new(format!("ancestry-binding-{ordinal}")).unwrap();
                let (sink, mut stream) = forge_provider_api::events::channel(binding_id.clone(), scope.clone());
                let guard = turn.activate(Some(sink.clone()), None).unwrap();
                let submission_id = forge_provider_api::SubmissionId::try_from(format!("submission-{ordinal}")).unwrap();
                let options = crate::copilot::prompt::TurnOptions {
                    scope: scope.clone(),
                    workspace: std::env::temp_dir().to_string_lossy().into_owned(),
                    model: None,
                    capability: capability.clone(),
                    input: forge_provider_api::PromptInput::Text(format!("request {ordinal}")),
                    task: None,
                    recovered: Vec::new(),
                    instructions: String::new(),
                    repair_caution: None,
                    attachments: forge_provider_api::input::MessageInput { prompt: String::new(), attachments: Vec::new() },
                    receipt: None,
                    dispatch: None,
                    submission_id: Some(submission_id.clone()),
                    background_compaction_observed: true,
                    accepted: None,
                };
                let result = crate::copilot::prompt::run(
                    &session,
                    &turn,
                    &guard,
                    options,
                    &sink,
                    &CompletionControl,
                    &crate::copilot::provider::NoopNativeCallbacks,
                ).await.unwrap();
                drop(guard);
                assert_eq!(result.completion.provider_session_id.as_ref().unwrap().as_str(), session.id());
                let mut answer = None;
                let mut terminal = None;
                while let Ok(bound) = stream.try_recv() {
                    match bound.event {
                        forge_provider_api::ProviderEvent::Message { update: forge_provider_api::MessageUpdate::Completed { text }, .. } => answer = Some(text),
                        forge_provider_api::ProviderEvent::Execution { observation } => {
                            if let forge_provider_api::lifecycle::ExecutionUpdate::Finished { covered_submissions, result: forge_provider_api::lifecycle::ExecutionResult::Completed { .. }, .. } = observation.update() {
                                terminal = Some(covered_submissions.clone());
                            }
                        }
                        _ => {}
                    }
                }
                assert_eq!(answer.as_deref(), Some(format!("full answer {ordinal}").as_str()));
                assert_eq!(terminal.unwrap(), vec![submission_id]);
                completed.push(ordinal);
            }
            assert_eq!(completed, [0, 1]);
            let methods = peer.methods.lock().unwrap().clone();
            assert_eq!(methods.iter().filter(|method| method.as_str() == "session.send").count(), 2);
            assert!(!methods.iter().any(|method| method == "session.abort"));
            assert_eq!(methods.iter().filter(|method| method.as_str() == "session.eventLog.read").count(), 2, "completion unexpectedly read back output instead of using live ancestry");
            for index in methods.iter().enumerate().filter_map(|(index, method)| (method == "session.send").then_some(index)) {
                assert_eq!(&methods[index - 2..index], ["session.eventLog.tail", "session.eventLog.read"], "journal boundary was not captured before native input");
            }
            sessions.retire(&owner.clone()).await.unwrap();
            runtime.shutdown().await.unwrap();
            let _ = peer.stop.send(());
            peer.task.await.unwrap();
    }).await.expect("incomplete-ancestry SDK regression fixture timed out");
}
