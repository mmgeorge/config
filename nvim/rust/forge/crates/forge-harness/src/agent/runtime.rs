use std::{future::Future, sync::Arc};

use anyhow::{Context, Result};
use tokio::sync::watch;

use super::{AgentCommand, AgentCommandSender};
use crate::provider::{
    ExecutionOwner, Provider, ProviderCompletion, ProviderEvent, ProviderRequest,
};

/// Runs one child turn without retaining a broker, storage, or presentation lock.
pub struct AgentRuntime {
    pub provider: Arc<Provider>,
    /// Applies the root session's deadline to each child tool and shell invocation.
    pub(crate) tool_timeout: crate::session::InputTimeout,
    /// Shared diagnostics for provider retry attempts and cancellation.
    pub(crate) trace: Arc<crate::trace::TraceStore>,
    /// Persistent submission journal shared by the child execution owner.
    pub(crate) submission_store: Arc<crate::provider::submission::SubmissionStore>,
    /// Shares timeout feedback without crossing child conversation ownership.
    pub permission: Arc<crate::provider::approval::PermissionCoordinator>,
    pub request: ProviderRequest,
    pub task_id: String,
    pub cancellation: watch::Receiver<bool>,
    pub sender: AgentCommandSender,
    pub(crate) lsp_lease: Option<crate::tools::lsp::LspLease>,
}

impl AgentRuntime {
    /// Forward owned observations through shutdown before reporting the child's completion.
    pub async fn run(mut self) -> Result<()> {
        let owner = self.request.owner()?;
        anyhow::ensure!(
            !owner.is_root(),
            "child runtime cannot own the root conversation"
        );
        // Subscribe before dispatch so retained child observations keep their original owner.
        let scope = crate::provider::InvocationScope::new(owner.clone(), self.task_id.clone());
        let (event_sink, event_stream) = crate::provider::events::channel(scope.clone());
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            self.tool_timeout,
        )?);
        let event_sink = event_sink.with_observer(watchdog.clone());
        let (session_events, subscription_error) =
            match self.provider.session_event_stream(owner.clone()).await {
                Ok(stream) => (stream, None),
                Err(error) => (None, Some(error)),
            };
        let mut events = ChildEventDelivery {
            invocation: Some(event_stream),
            session: session_events,
        };
        let mut request = self.request.clone();
        let mut cancelled = *self.cancellation.borrow();
        // Run foreground work while consuming both observation lanes.
        let mut output = if let Some(error) = subscription_error {
            Err(error)
        } else {
            loop {
                if request.submission.is_none() {
                    request.submission = Some(
                        self.submission_store
                            .prepare(&request, self.provider.descriptor().kind)?,
                    );
                }
                let result = async {
            let prompt = crate::provider::retry::prompt(self.provider.as_ref(), request.clone(), Some(event_sink.clone()), &self.trace, &self.submission_store);
            tokio::pin!(prompt);
            loop {
                if cancelled {break Err(anyhow::anyhow!("child task interrupted before dispatch"));}
                tokio::select! {
                    biased;
                    expired = watchdog.expired() => {
                        let expired = expired?.context("tool watchdog ended during child execution")?;
                        self.publish(&owner, ProviderEvent::Tool {
                            address: expired.address.clone(),
                            update: crate::provider::ToolUpdate::Finished {
                                outcome: crate::provider::ToolOutcome::Failed {
                                    diagnostic: crate::provider::ToolDiagnostic {
                                        code: Some("tool_timeout".into()), message: expired.to_string(),
                                    },
                                },
                            },
                        }).await?;
                        break Err(expired.into());
                    }
                    changed = self.cancellation.changed() => {
                        changed.context("child cancellation owner closed")?;
                        if *self.cancellation.borrow_and_update() {
                            cancelled = true;
                            break Err(anyhow::anyhow!("child task interrupted"));
                        }
                    }
                    event = events.next(), if events.is_open() => {
                        if let Some(event) = event? {
                            watchdog.observe(&event)?;
                            self.publish(&owner, event).await?;
                        }
                    }
                    output = &mut prompt, if !cancelled => break output,
                    else => break Err(anyhow::anyhow!("child task interrupted before dispatch")),
                }
            }
          }.await;
                cancelled |= result
                    .as_ref()
                    .is_err_and(|error| error.is::<crate::broker::TurnCancelled>());
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
                    && output.provider_session_id.is_some()
                {
                    request.provider_session_id = output.provider_session_id.clone();
                }
                request.input = crate::provider::ProviderInput::from_text(feedback);
                request.submission = None;
                request.recovered_input.clear();
            }
        };
        drop(event_sink);
        drop(self.lsp_lease.take());
        // Resolve native identity without stopping delivery from a bounded provider channel.
        let runtime_key = self.request.runtime_key()?;
        let (retained, observation_error) = events
            .forward_until(
                &self,
                &owner,
                tokio::time::timeout(
                    std::time::Duration::from_secs(2),
                    self.provider.active_session_id_for(&runtime_key),
                ),
            )
            .await;
        if let Some(error) = observation_error {
            output = Err(error);
        }
        let retained = match retained {
            Ok(retained) => retained,
            Err(_) => {
                output = Err(anyhow::anyhow!(
                    "child provider identity lookup exceeded 2 seconds"
                ));
                None
            }
        };
        if let Err(error) = events.drain(&self, &owner).await {
            output = Err(error);
        }
        if let Some(provider_thread_id) = &retained
            && let Err(error) = self
                .publish(
                    &owner,
                    ProviderEvent::Agent {
                        scope: scope.clone(),
                        update: crate::provider::AgentUpdate::ConversationResolved {
                            thread_id: provider_thread_id.clone(),
                        },
                    },
                )
                .await
        {
            output = Err(error);
        }
        // Settle native work and publish its final observations before child completion.
        let cleanup = tokio::time::timeout(std::time::Duration::from_secs(30), async {
            let (failure, observation_error) = events
                .forward_until(&self, &owner, async {
                    let mut failure = Vec::new();
                    if cancelled || output.is_err() {
                        if let Err(error) = self.provider.cancel_session(&runtime_key).await {
                            failure.push(format!("cancel: {error:#}"));
                        }
                        if let Err(error) = self.provider.cleanup_execution(&runtime_key).await {
                            failure.push(format!("cleanup: {error:#}"));
                        }
                    }
                    if let Err(error) = self.provider.shutdown().await {
                        failure.push(format!("shutdown: {error:#}"));
                    }
                    failure
                })
                .await;
            let observation_error = observation_error.or(events.drain(&self, &owner).await.err());
            (failure, observation_error)
        })
        .await;
        let cleanup = match cleanup {
            Ok((failure, observation_error)) => {
                if let Some(error) = observation_error {
                    output = Err(error);
                }
                Ok(failure)
            }
            Err(error) => Err(error),
        };
        let output = output.map(|mut output: ProviderCompletion| {
            if output.provider_session_id.is_none() {
                output.provider_session_id = retained;
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
                output: Box::new(output),
                cleanup_error,
            })
            .await
    }

    async fn publish(&self, owner: &ExecutionOwner, event: ProviderEvent) -> Result<()> {
        anyhow::ensure!(
            event.scope().owner() == owner,
            "provider event crosses child execution ownership"
        );
        anyhow::ensure!(
            event
                .scope()
                .invocation()
                .is_none_or(|scope| scope.exchange_id == self.task_id),
            "provider event crosses child task ownership"
        );
        self.sender
            .send(AgentCommand::Event {
                owner: owner.clone(),
                task_id: self.task_id.clone(),
                event: Box::new(event),
            })
            .await
    }
}

/// Preserve invocation ordering while draining observations owned by the same child session.
struct ChildEventDelivery {
    invocation: Option<crate::provider::events::ProviderEventStream>,
    session: Option<crate::provider::events::ProviderEventStream>,
}

impl ChildEventDelivery {
    /// Identify whether either owned observation stream can still produce an event.
    fn is_open(&self) -> bool {
        self.invocation.is_some() || self.session.is_some()
    }

    /// Poll admitted invocation events before later session observations of those items.
    async fn next(&mut self) -> Result<Option<ProviderEvent>> {
        loop {
            tokio::select! {
                biased;
                event = async { self.invocation.as_mut().expect("invocation branch requires an open stream").recv().await }, if self.invocation.is_some() => {
                    match event {
                        Ok(Some(event)) => return Ok(Some(event)),
                        Ok(None) => self.invocation = None,
                        Err(error) => {
                            self.invocation = None;
                            return Err(error.into());
                        }
                    }
                }
                event = async { self.session.as_mut().expect("session branch requires an open stream").recv().await }, if self.session.is_some() => {
                    match event {
                        Ok(Some(event)) => return Ok(Some(event)),
                        Ok(None) => self.session = None,
                        Err(error) => {
                            self.session = None;
                            return Err(error.into());
                        }
                    }
                }
                else => return Ok(None),
            }
        }
    }

    /// Keep bounded provider channels moving while a lifecycle operation waits for completion.
    async fn forward_until<Operation: Future>(
        &mut self,
        runtime: &AgentRuntime,
        owner: &ExecutionOwner,
        operation: Operation,
    ) -> (Operation::Output, Option<anyhow::Error>) {
        tokio::pin!(operation);
        let mut failure = None;
        loop {
            tokio::select! {
                biased;
                result = &mut operation => return (result, failure),
                event = self.next(), if self.is_open() => {
                    match event {
                        Ok(Some(event)) => {
                            if let Err(error) = runtime.publish(owner, event).await {
                                failure.get_or_insert(error);
                            }
                        }
                        Ok(None) => {},
                        Err(error) => { failure.get_or_insert(error); }
                    }
                }
            }
        }
    }

    /// Publish every queued final observation after its producer has finished shutdown.
    async fn drain(&mut self, runtime: &AgentRuntime, owner: &ExecutionOwner) -> Result<()> {
        let mut failure = None;
        loop {
            if let Some(invocation) = self.invocation.as_mut() {
                match invocation.try_recv() {
                    Ok(event) => {
                        if let Err(error) = runtime.publish(owner, event).await {
                            failure.get_or_insert(error);
                        }
                        continue;
                    }
                    Err(crate::provider::events::EventDeliveryError::Empty) => {}
                    Err(crate::provider::events::EventDeliveryError::Closed { .. }) => {
                        self.invocation = None
                    }
                    Err(error) => {
                        self.invocation = None;
                        failure.get_or_insert(error.into());
                    }
                }
            }
            let Some(session) = self.session.as_mut() else {
                break;
            };
            match session.try_recv() {
                Ok(event) => {
                    if let Err(error) = runtime.publish(owner, event).await {
                        failure.get_or_insert(error);
                    }
                }
                Err(crate::provider::events::EventDeliveryError::Empty) => break,
                Err(crate::provider::events::EventDeliveryError::Closed { .. }) => {
                    self.session = None;
                    break;
                }
                Err(error) => {
                    self.session = None;
                    failure.get_or_insert(error.into());
                    break;
                }
            }
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }
}

#[cfg(test)]
#[path = "runtime_test.rs"]
mod test;
