use super::{
    ControlToolDefinition, ControlToolExecutor, ControlToolInvocation, ControlTurnContext,
    ToolError, ToolResult,
};
use crate::provider::{ProviderCompletion, ProviderDiagnostic, ProviderEvent, ProviderEventSink};
use async_trait::async_trait;
use forge_provider_api::control::ToolCall;
use serde::Serialize;
use std::sync::{Arc, Mutex as StdMutex};
use tokio::sync::{Mutex, Notify, RwLock};

/// The typed host boundary served by the private MCP endpoint.
#[async_trait]
pub(crate) trait ControlEndpoint: Send + Sync {
    async fn definitions(&self) -> Vec<ControlToolDefinition>;
    async fn call(&self, call: ToolCall) -> ToolResult<ControlReply>;
}

#[derive(Debug, Serialize)]
pub(crate) struct ControlReply {
    pub(crate) message: String,
}

struct ActiveControl {
    executor: Arc<ControlToolExecutor>,
    sink: ProviderEventSink,
    tools: std::collections::HashSet<String>,
    output: Mutex<ProviderCompletion>,
}

#[derive(Default)]
struct ControlState {
    accepting: bool,
    active: Option<Arc<ActiveControl>>,
    pending_calls: usize,
}

/// Owns Harness tool execution and effects independently of native provider events.
#[derive(Default)]
pub(crate) struct ControlSession {
    definitions: RwLock<Vec<ControlToolDefinition>>,
    state: StdMutex<ControlState>,
    changed: Notify,
    host: StdMutex<Option<HostDelegate>>,
}

#[derive(Clone)]
struct HostDelegate {
    scope: crate::provider::InvocationScope,
    tools: std::sync::Weak<dyn forge_provider_api::control::ProviderTools>,
}

struct PendingControlCall<'session> {
    session: &'session ControlSession,
}

impl Drop for PendingControlCall<'_> {
    fn drop(&mut self) {
        let mut state = self
            .session
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.pending_calls -= 1;
        drop(state);
        self.session.changed.notify_waiters();
    }
}

impl ControlSession {
    pub(crate) fn new(definitions: Vec<ControlToolDefinition>) -> Self {
        Self {
            definitions: RwLock::new(definitions),
            state: StdMutex::new(ControlState::default()),
            changed: Notify::new(),
            host: StdMutex::new(None),
        }
    }

    /// Route native MCP calls through the same scoped commit and replay boundary as direct callbacks.
    pub(crate) fn bind_host(
        &self,
        scope: crate::provider::InvocationScope,
        tools: std::sync::Weak<dyn forge_provider_api::control::ProviderTools>,
    ) {
        *self
            .host
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) =
            Some(HostDelegate { scope, tools });
    }

    pub(crate) async fn begin(&self, mut context: ControlTurnContext, sink: ProviderEventSink) {
        context.question_invocation = sink.invocation_scope().ok().cloned();
        self.close().await;
        self.host
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        let active = Arc::new(ActiveControl {
            executor: Arc::new(ControlToolExecutor::new(context)),
            sink,
            tools: self
                .definitions
                .read()
                .await
                .iter()
                .map(|tool| tool.name.to_owned())
                .collect(),
            output: Mutex::new(ProviderCompletion::default()),
        });
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.active = Some(active);
        state.accepting = true;
    }

    pub(crate) async fn take_output(&self) -> ProviderCompletion {
        let active = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .active
            .clone();
        match active {
            Some(active) => std::mem::take(&mut *active.output.lock().await),
            None => ProviderCompletion::default(),
        }
    }

    /// Revoke admission without waiting for an in-flight tool.
    pub(crate) fn cancel_admission(&self) -> Option<Arc<ControlToolExecutor>> {
        let active = {
            let mut state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.accepting = false;
            state.active.clone()
        };
        let executor = active.map(|active| active.executor.clone());
        if let Some(executor) = &executor {
            executor.cancel();
        }
        executor
    }

    pub(crate) async fn cancel(&self) -> Option<Arc<ControlToolExecutor>> {
        self.cancel_admission()
    }

    pub(crate) async fn close(&self) {
        if let Some(executor) = self.cancel().await {
            executor.close().await;
        }
        self.wait_for_calls().await;
    }

    /// Collect normal foreground effects without turning completion into tool cancellation.
    pub(crate) async fn finish(&self) {
        let active = {
            let mut state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.accepting = false;
            state.active.clone()
        };
        let executor = active.map(|active| active.executor.clone());
        if let Some(executor) = executor {
            executor.finish().await;
        }
        self.wait_for_calls().await;
    }

    async fn wait_for_calls(&self) {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .pending_calls
                == 0
            {
                return;
            }
            changed.await;
        }
    }

    fn admit_call(&self, name: &str) -> ToolResult<(Arc<ActiveControl>, PendingControlCall<'_>)> {
        let mut state = self.state.lock().map_err(|_| {
            ToolError::MissingState("Tool is unavailable in the active invocation".into())
        })?;
        if !state.accepting {
            return Err(ToolError::MissingState(
                "Tool is unavailable in the active invocation".into(),
            ));
        }
        let active = state
            .active
            .as_ref()
            .filter(|active| active.tools.contains(name))
            .cloned()
            .ok_or_else(|| {
                ToolError::MissingState("Tool is unavailable in the active invocation".into())
            })?;
        state.pending_calls = state.pending_calls.checked_add(1).ok_or_else(|| {
            ToolError::MissingState("Tool is unavailable in the active invocation".into())
        })?;
        Ok((active, PendingControlCall { session: self }))
    }
}

#[async_trait]
impl ControlEndpoint for ControlSession {
    async fn definitions(&self) -> Vec<ControlToolDefinition> {
        self.definitions.read().await.clone()
    }

    async fn call(&self, call: ToolCall) -> ToolResult<ControlReply> {
        let host = self
            .host
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        if let Some(host) = host {
            let tools = host
                .tools
                .upgrade()
                .ok_or_else(|| ToolError::MissingState("Host invocation has retired".into()))?;
            let message = tools
                .call(&host.scope, call)
                .await
                .map_err(|error| ToolError::Operation(Box::new(error)))?;
            return Ok(ControlReply { message });
        }
        let (active, _pending) = self.admit_call(&call.name)?;
        let executor = active.executor.clone();
        let sink = active.sink.clone();
        let invocation = ControlToolInvocation {
            call_id: Some(call.id),
            task_permit: None,
            name: call.name,
            arguments: call.arguments,
        };
        let outcome = executor.execute(invocation.clone()).await?;
        let result = match outcome.result {
            Ok(result) => result,
            Err(error) => {
                let failure = super::control_tool_failure_json(
                    &invocation,
                    &error,
                    outcome.plan_document.as_ref(),
                );
                sink.send_wait(ProviderEvent::Diagnostic {
                    scope: sink
                        .invocation_scope()
                        .map_err(|error| ToolError::Operation(Box::new(error)))?
                        .clone()
                        .into(),
                    diagnostic: ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Rejected,
                        message: forge_provider_api::SanitizedMessage::new(
                            "Host tool execution failed",
                        )
                        .expect("static diagnostic is nonempty"),
                        fatal: false,
                    },
                })
                .await
                .map_err(|error| ToolError::Operation(Box::new(error)))?;
                return Err(ToolError::Rejected(failure));
            }
        };
        if let Some(invocation) = result.invocation {
            super::apply_invocation(&invocation, &mut *active.output.lock().await)?;
        }
        Ok(ControlReply {
            message: result.message,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{ExecutionOwner, InvocationScope, PromptMode, events};
    use anyhow::Result;
    use serde_json::{Value, json};

    fn channel() -> (ProviderEventSink, events::ProviderEventStream) {
        events::channel(InvocationScope::new(
            ExecutionOwner::root("control"),
            "exchange",
        ))
    }

    fn definitions(names: &[&str]) -> Vec<ControlToolDefinition> {
        crate::tools::ControlToolRegistry
            .definition_list()
            .into_iter()
            .filter(|definition| names.contains(&definition.name))
            .collect()
    }

    #[tokio::test]
    async fn cleanup_cancels_tool_waits_before_replacement_turn() -> Result<()> {
        let state = ControlSession::new(definitions(&["goal_complete"]));
        let (sink, _stream) = channel();
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.bind_test_task_controller(task_controller);
        context.checkpoint = Some(crate::checkpoint::ToolGate::new());
        state.begin(context, sink.clone()).await;
        let mut pending = Box::pin(
            state.call(ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-1")
                    .expect("fixture call identity is nonempty"),
                name: "goal_complete".into(),
                arguments: json!({"summary":"old turn"}),
            }),
        );
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        let (outcome, ()) = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            tokio::join!(pending, state.close())
        })
        .await?;
        assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(!state.take_output().await.evidence.structured_complete);
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.bind_test_task_controller(task_controller);
        state.begin(context, sink.clone()).await;
        state
            .call(ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-2")
                    .expect("fixture call identity is nonempty"),
                name: "goal_complete".into(),
                arguments: json!({"summary":"replacement turn"}),
            })
            .await?;
        let output = state.take_output().await;
        assert!(!output.evidence.structured_complete);
        assert!(output.goal_evidence().0.structured_complete);
        assert!(output.goal_control.is_some());
        Ok(())
    }

    #[tokio::test]
    async fn malformed_agent_tool_returns_field_diagnostics_to_the_model_and_timeline() -> Result<()>
    {
        let state = ControlSession::new(definitions(&["agent_spawn"]));
        let (sink, mut stream) = channel();
        state
            .begin(
                ControlTurnContext::inactive(crate::provider::PromptMode::Chat),
                sink,
            )
            .await;
        let failure = state
            .call(ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-3")
                    .expect("fixture call identity is nonempty"),
                name: "agent_spawn".into(),
                arguments: json!({"provider":"incorrect"}),
            })
            .await
            .unwrap_err();
        let diagnostic: Value = serde_json::from_str(&failure.to_string())?;
        assert_eq!(diagnostic["code"], "invalid_arguments");
        let violation = diagnostic["violation"].as_array().unwrap();
        assert!(violation.iter().any(|item| item["code"] == "missing_field"
            && item["message"].as_str().unwrap().contains("text")));
        assert!(
            violation
                .iter()
                .any(|item| item["path"] == "provider" && item["code"] == "unknown_field")
        );
        assert!(
            matches!(stream.try_next()?, Some(ProviderEvent::Diagnostic { diagnostic, .. }) if diagnostic.kind == forge_provider_api::ProviderFailureKind::Rejected && diagnostic.message.as_str() == "Host tool execution failed")
        );
        Ok(())
    }

    #[tokio::test]
    async fn inactive_endpoint_rejects_calls() {
        let state = ControlSession::default();
        let failure = state
            .call(ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-4")
                    .expect("fixture call identity is nonempty"),
                name: "plan_file_read".into(),
                arguments: json!({}),
            })
            .await
            .err()
            .unwrap();
        assert!(matches!(failure, ToolError::MissingState(_)));
    }
}
