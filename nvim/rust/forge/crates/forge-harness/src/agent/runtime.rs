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
    pub(crate) execution_registry:
        Arc<crate::provider::session_execution::SessionExecutionRegistry>,
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
        let engine = self.execution_registry.owner(owner.clone()).await?;
        let (session_sink, event_stream) = engine.session_observer().await?;
        let event_sink = session_sink.invocation_sink(&scope)?;
        let watchdog = Arc::new(crate::provider::tool_timeout::ToolWatchdog::new(
            self.tool_timeout,
        )?);
        let event_sink = event_sink.with_observer(watchdog.clone());
        let mut events = ChildObservation {
            stream: Some(event_stream),
        };
        let mut request = self.request.clone();
        let mut cancelled = *self.cancellation.borrow();
        // Run foreground work while consuming the owner's single ordered stream.
        let mut output = {
            loop {
                if request.submission.is_none() {
                    request.submission = Some(
                        self.submission_store
                            .prepare(&request, self.provider.descriptor().kind)?,
                    );
                }
                let result = async {
            let prompt = crate::provider::retry::execute(engine.as_ref(), request.clone(), event_sink.clone(), &self.trace, &self.submission_store);
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
                                        kind: forge_provider_api::ProviderFailureKind::Timeout,
                                        message: forge_provider_api::SanitizedMessage::new("Tool execution timed out")
                                            .expect("static diagnostic is nonempty"),
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
                if (cancelled || result.is_err())
                    && let Some(submission) = &request.submission
                {
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
            preserve_child_failure(&mut output, error);
        }
        let retained = match retained {
            Ok(Ok(retained)) => retained,
            Ok(Err(error)) => {
                preserve_child_failure(&mut output, error.into());
                None
            }
            Err(_) => {
                preserve_child_failure(
                    &mut output,
                    anyhow::anyhow!("child provider identity lookup exceeded 2 seconds"),
                );
                None
            }
        };
        if let Err(error) = events.drain(&self, &owner).await {
            preserve_child_failure(&mut output, error);
        }
        if let Some(provider_thread_id) = &retained
            && let Err(error) = self
                .publish(
                    &owner,
                    ProviderEvent::Agent {
                        scope: scope.clone(),
                        update: crate::provider::AgentUpdate::ConversationResolved {
                            thread_id: provider_thread_id.clone().into(),
                        },
                    },
                )
                .await
        {
            preserve_child_failure(&mut output, error);
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
                    if let Err(error) = self.execution_registry.begin_orderly_shutdown().await {
                        failure.push(format!("execution shutdown fence: {error:#}"));
                    }
                    if let Err(error) = self.provider.shutdown().await {
                        failure.push(format!("shutdown: {error:#}"));
                    }
                    if let Err(error) = self.execution_registry.close().await {
                        failure.push(format!("execution collection: {error:#}"));
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
                    preserve_child_failure(&mut output, error);
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
                .is_none_or(|scope| scope.exchange_id
                    == forge_provider_api::ExchangeId::from(self.task_id.as_str())),
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

/// Keep the original child failure while retaining later observation failures as context.
fn preserve_child_failure(output: &mut Result<ProviderCompletion>, additional: anyhow::Error) {
    let prior = std::mem::replace(
        output,
        Err(anyhow::anyhow!("child failure is being recorded")),
    );
    *output = Err(match prior {
        Ok(_) => additional,
        Err(original) => {
            let message =
                format!("{original:#}; additional child observation failure: {additional:#}");
            original.context(message)
        }
    });
}

/// Preserve invocation ordering while draining observations owned by the same child session.
struct ChildObservation {
    stream: Option<crate::provider::events::ProviderEventStream>,
}

impl ChildObservation {
    /// Identify whether the owner stream can still produce an observation.
    fn is_open(&self) -> bool {
        self.stream.is_some()
    }

    /// Receive foreground and retained events in the engine's delivery order.
    async fn next(&mut self) -> Result<Option<ProviderEvent>> {
        let Some(stream) = self.stream.as_mut() else {
            return Ok(None);
        };
        match stream.recv().await {
            Ok(Some(event)) => Ok(Some(event)),
            Ok(None) => {
                self.stream = None;
                Ok(None)
            }
            Err(error) => {
                self.stream = None;
                Err(child_delivery_error(error))
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
        while let Some(stream) = self.stream.as_mut() {
            match stream.try_recv() {
                Ok(event) => {
                    if let Err(error) = runtime.publish(owner, event).await {
                        failure.get_or_insert(error);
                    }
                }
                Err(crate::provider::events::EventDeliveryError::Empty) => break,
                Err(crate::provider::events::EventDeliveryError::Closed) => {
                    self.stream = None;
                    break;
                }
                Err(error) => {
                    self.stream = None;
                    failure.get_or_insert(child_delivery_error(error));
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

/// Preserve observation failures as provider errors across the child event boundary.
fn child_delivery_error(error: crate::provider::events::EventDeliveryError) -> anyhow::Error {
    match error {
        crate::provider::events::EventDeliveryError::Observation(failure) => {
            crate::provider::ProviderError::Observation(failure).into()
        }
        error => error.into(),
    }
}

#[cfg(test)]
#[path = "runtime_test.rs"]
mod test;
