use std::sync::Arc;

use anyhow::{Context, Result};
use tokio::sync::watch;

use super::{AgentCommand, AgentCommandSender};
use crate::backend::{Backend, BackendEvent, BackendOutput, BackendRequest, ExecutionOwner};

/// Runs one child turn without retaining a broker, storage, or presentation lock.
pub struct AgentRuntime {
    pub backend: Arc<dyn Backend>,
    /// Shared diagnostics for provider retry attempts and cancellation.
    pub(crate) trace: Arc<crate::trace::TraceStore>,
    /// Persistent submission journal shared by the child execution owner.
    pub(crate) submission_store: Arc<crate::backend::submission::SubmissionStore>,
    /// Shares timeout feedback without crossing child conversation ownership.
    pub permission: Arc<crate::backend::approval::PermissionCoordinator>,
    pub request: BackendRequest,
    pub task_id: String,
    pub cancellation: watch::Receiver<bool>,
    pub sender: AgentCommandSender,
    pub(crate) lsp_lease: Option<crate::control_tools::lsp::LspLease>,
}

impl AgentRuntime {
    pub async fn run(mut self) -> Result<()> {
        let owner = self.request.owner()?;
        anyhow::ensure!(
            !owner.is_root(),
            "child runtime cannot own the root conversation"
        );
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let event_sink = event_sink.owned(owner.clone());
        let mut request = self.request.clone();
        let mut stream_open = true;
        let mut cancelled = *self.cancellation.borrow();
        let mut output = loop {
            if request.submission.is_none() {
                request.submission = Some(
                    self.submission_store
                        .prepare(&request, self.backend.descriptor().kind)?,
                );
            }
            let result = async {
            let prompt = crate::backend::retry::prompt(self.backend.as_ref(), request.clone(), Some(event_sink.clone()), &self.trace, &self.submission_store);
            tokio::pin!(prompt);
            loop {
                if cancelled {break Err(anyhow::anyhow!("child task interrupted before dispatch"));}
                tokio::select! {
                    biased;
                    changed = self.cancellation.changed() => {
                        changed.context("child cancellation owner closed")?;
                        if *self.cancellation.borrow_and_update() {
                            cancelled = true;
                            break Err(anyhow::anyhow!("child task interrupted"));
                        }
                    }
                    event = event_stream.recv(), if stream_open => {
                        match event? {
                            Some(event) => self.publish(&owner, event).await?,
                            None => stream_open = false,
                        }
                    }
                    output = &mut prompt, if !cancelled => break output,
                    else => break Err(anyhow::anyhow!("child task interrupted before dispatch")),
                }
            }
          }.await;
            if cancelled && let Some(submission) = &request.submission {
                submission.cancel()?;
            }
            if cancelled || result.is_err() {
                break result;
            }
            let Some(feedback) = self.permission.take_timeout_feedback(&owner)? else {
                break result;
            };
            if let Ok(output) = &result
                && output.backend_session_id.is_some()
            {
                request.backend_session_id = output.backend_session_id.clone();
            }
            request.input = crate::backend::BackendInput::from_text(feedback);
            request.submission = None;
            request.recovered_input.clear();
        };
        drop(self.lsp_lease.take());
        let runtime_key = self.request.runtime_key()?;
        let retained = match tokio::time::timeout(
            std::time::Duration::from_secs(2),
            self.backend.active_session_id_for(&runtime_key),
        )
        .await
        {
            Ok(retained) => retained,
            Err(_) => {
                output = Err(anyhow::anyhow!(
                    "child provider identity lookup exceeded 2 seconds"
                ));
                None
            }
        };
        while let Ok(event) = event_stream.try_recv() {
            if let Err(error) = self.publish(&owner, event).await {
                output = Err(error);
                break;
            }
        }
        if let Some(provider_thread_id) = &retained {
            self.publish(
                &owner,
                BackendEvent {
                    message: None,
                    received_at_ms: None,
                    address: None,
                    turn_boundary: None,
                    kind: "agent_conversation_resolved".into(),
                    text: None,
                    data: serde_json::json!({"thread_id":provider_thread_id}),
                    activity: None,
                    summary: None,
                    checklist_update: None,
                },
            )
            .await?;
        }
        let cleanup = tokio::time::timeout(std::time::Duration::from_secs(30), async {
            let mut failure = Vec::new();
            if cancelled || output.is_err() {
                if let Err(error) = self.backend.cancel_session(&runtime_key).await {
                    failure.push(format!("cancel: {error:#}"));
                }
                if let Err(error) = self.backend.cleanup_execution(&runtime_key).await {
                    failure.push(format!("cleanup: {error:#}"));
                }
            }
            if let Err(error) = self.backend.shutdown().await {
                failure.push(format!("shutdown: {error:#}"));
            }
            failure
        })
        .await;
        let output = output.map(|mut output: BackendOutput| {
            if output.backend_session_id.is_none() {
                output.backend_session_id = retained;
            }
            output
        });
        let cleanup_error = match cleanup {
            Ok(failure) if failure.is_empty() => None,
            Ok(failure) => Some(failure.join("\n")),
            Err(_) => Some("child cleanup exceeded 30 seconds".to_owned()),
        };
        self.sender
            .send(AgentCommand::Completed {
                owner,
                task_id: self.task_id.clone(),
                output,
                cleanup_error,
            })
            .await
    }

    async fn publish(&self, owner: &ExecutionOwner, mut event: BackendEvent) -> Result<()> {
        if let Some(address) = &mut event.address {
            anyhow::ensure!(
                address
                    .owner
                    .as_ref()
                    .is_none_or(|observed| observed == owner),
                "provider event crosses child execution ownership"
            );
            address.owner = Some(owner.clone());
        }
        self.sender
            .send(AgentCommand::Event {
                owner: owner.clone(),
                task_id: self.task_id.clone(),
                event,
            })
            .await
    }
}
