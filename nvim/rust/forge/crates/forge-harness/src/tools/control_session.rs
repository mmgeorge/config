use super::{
    ControlToolDefinition, ControlToolExecutor, ControlToolInvocation, ControlTurnContext,
    ToolError, ToolResult,
};
use crate::provider::{ProviderCompletion, ProviderDiagnostic, ProviderEvent, ProviderEventSink};
use async_trait::async_trait;
use forge_provider_api::control::ToolCall;
use serde::Serialize;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};

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
}

/// Owns Harness tool execution and effects independently of native provider events.
#[derive(Default)]
pub(crate) struct ControlSession {
    definitions: RwLock<Vec<ControlToolDefinition>>,
    active: Mutex<Option<ActiveControl>>,
    output: Mutex<ProviderCompletion>,
}

impl ControlSession {
    pub(crate) fn new(definitions: Vec<ControlToolDefinition>) -> Self {
        Self {
            definitions: RwLock::new(definitions),
            ..Self::default()
        }
    }

    pub(crate) async fn begin(&self, context: ControlTurnContext, sink: ProviderEventSink) {
        self.close().await;
        *self.output.lock().await = ProviderCompletion::default();
        *self.active.lock().await = Some(ActiveControl {
            executor: Arc::new(ControlToolExecutor::new(context)),
            sink,
        });
    }

    pub(crate) async fn take_output(&self) -> ProviderCompletion {
        std::mem::take(&mut *self.output.lock().await)
    }

    /// Revoke admission without waiting for an in-flight tool.
    pub(crate) async fn cancel(&self) -> Option<Arc<ControlToolExecutor>> {
        let executor = self
            .active
            .lock()
            .await
            .as_ref()
            .map(|active| active.executor.clone());
        if let Some(executor) = &executor {
            executor.cancel();
        }
        executor
    }

    pub(crate) async fn close(&self) {
        if let Some(executor) = self.cancel().await {
            executor.close().await;
            let mut active = self.active.lock().await;
            if active
                .as_ref()
                .is_some_and(|active| Arc::ptr_eq(&active.executor, &executor))
            {
                *active = None;
            }
        }
    }
}

#[async_trait]
impl ControlEndpoint for ControlSession {
    async fn definitions(&self) -> Vec<ControlToolDefinition> {
        self.definitions.read().await.clone()
    }

    async fn call(&self, call: ToolCall) -> ToolResult<ControlReply> {
        let (executor, sink) = self
            .active
            .lock()
            .await
            .as_ref()
            .map(|active| (active.executor.clone(), active.sink.clone()))
            .ok_or_else(|| {
                ToolError::MissingState("Harness control called outside an active turn".into())
            })?;
        let invocation = ControlToolInvocation {
            task_permit: None,
            name: call.name,
            arguments: call.arguments,
        };
        let outcome = executor.execute(invocation.clone()).await?;
        let active = self.active.lock().await;
        if !active
            .as_ref()
            .is_some_and(|active| Arc::ptr_eq(&active.executor, &executor))
        {
            return Err(ToolError::Rejected(
                "Harness control result belongs to a collected turn".into(),
            ));
        }
        let result = match outcome.result {
            Ok(result) => result,
            Err(error) => {
                drop(active);
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
                        code: None,
                        message: format!("Harness tool {} failed: {failure}", invocation.name),
                        fatal: false,
                    },
                })
                .await
                .map_err(|error| ToolError::Operation(Box::new(error)))?;
                return Err(ToolError::Rejected(failure));
            }
        };
        if let Some(invocation) = result.invocation {
            super::apply_invocation(&invocation, &mut *self.output.lock().await)?;
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

    #[tokio::test]
    async fn cleanup_cancels_tool_waits_before_replacement_turn() -> Result<()> {
        let state = ControlSession::default();
        let (sink, _stream) = channel();
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.bind_test_task_controller(task_controller);
        context.checkpoint = Some(crate::checkpoint::ToolGate::new()?);
        state.begin(context, sink.clone()).await;
        let mut pending = Box::pin(state.call(ToolCall {
            name: "goal_complete".into(),
            arguments: json!({"summary":"old turn"}),
        }));
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        let (outcome, ()) = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            tokio::join!(pending, state.close())
        })
        .await?;
        assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(!state.output.lock().await.evidence.structured_complete);
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.bind_test_task_controller(task_controller);
        state.begin(context, sink.clone()).await;
        state
            .call(ToolCall {
                name: "goal_complete".into(),
                arguments: json!({"summary":"replacement turn"}),
            })
            .await?;
        assert!(state.output.lock().await.evidence.structured_complete);
        assert!(state.output.lock().await.goal_control.is_some());
        Ok(())
    }

    #[tokio::test]
    async fn malformed_agent_tool_returns_field_diagnostics_to_the_model_and_timeline() -> Result<()>
    {
        let state = ControlSession::default();
        let (sink, mut stream) = channel();
        state
            .begin(
                ControlTurnContext::inactive(crate::provider::PromptMode::Chat),
                sink,
            )
            .await;
        let failure = state
            .call(ToolCall {
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
            matches!(stream.try_next()?, Some(ProviderEvent::Diagnostic { diagnostic, .. }) if diagnostic.message.contains("text"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn inactive_endpoint_rejects_calls() {
        let state = ControlSession::default();
        let failure = state
            .call(ToolCall {
                name: "plan_file_read".into(),
                arguments: json!({}),
            })
            .await
            .err()
            .unwrap();
        assert!(matches!(failure, ToolError::MissingState(_)));
    }
}
