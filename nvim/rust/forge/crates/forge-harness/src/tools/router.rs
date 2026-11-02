use super::{
    ControlToolExecutor, ControlToolInvocation, ControlTurnContext, control_tool_failure_json,
};
use anyhow::{Context, Result};
use async_trait::async_trait;
use forge_provider_api::ProviderError;
use forge_provider_api::control::{ProviderTools, ToolCall};
use std::sync::{Arc, Mutex as StandardMutex};
use tokio::sync::mpsc;

#[derive(Default)]
pub(crate) struct ControlToolRouter {
    execution_finished: std::sync::atomic::AtomicBool,
    sender: StandardMutex<Option<mpsc::Sender<ControlToolInvocation>>>,
    runtime: StandardMutex<Option<Arc<ControlToolExecutor>>>,
}

impl ControlToolRouter {
    pub(crate) fn activate(
        self: &Arc<Self>,
        context: ControlTurnContext,
    ) -> Result<ControlToolStream> {
        let mut sender = self
            .sender
            .lock()
            .map_err(|_| anyhow::anyhow!("Provider control-tool router lock poisoned"))?;
        anyhow::ensure!(
            sender.is_none(),
            "Provider control-tool router already active"
        );
        let (active_sender, receiver) = mpsc::channel(32);
        self.execution_finished
            .store(false, std::sync::atomic::Ordering::Release);
        *sender = Some(active_sender);
        *self
            .runtime
            .lock()
            .map_err(|_| anyhow::anyhow!("Provider control-tool runtime lock poisoned"))? =
            Some(Arc::new(ControlToolExecutor::new(context)));
        Ok(ControlToolStream {
            router: Arc::clone(self),
            receiver,
        })
    }

    pub(crate) async fn route(&self, invocation: ControlToolInvocation) -> Result<String> {
        let (active_sender, admission, runtime) = {
            let sender_guard = self
                .sender
                .lock()
                .map_err(|_| anyhow::anyhow!("Provider control-tool router lock poisoned"))?;
            let sender = sender_guard
                .clone()
                .context("Provider control tool ran without an active turn")?;
            let active_sender = sender.clone();
            let admission = sender
                .try_reserve_owned()
                .context("Provider control-tool admission is full or closed")?;
            let runtime = self
                .runtime
                .lock()
                .map_err(|_| anyhow::anyhow!("Provider control-tool runtime lock poisoned"))?
                .clone()
                .context("Provider control tool ran without an active turn")?;
            drop(sender_guard);
            (active_sender, admission, runtime)
        };
        {
            let sender = self
                .sender
                .lock()
                .map_err(|_| anyhow::anyhow!("Provider control-tool router lock poisoned"))?;
            anyhow::ensure!(
                sender
                    .as_ref()
                    .is_some_and(|current| current.same_channel(&active_sender)),
                "Provider control-tool invocation belongs to a collected turn"
            );
        }
        let outcome = runtime.execute(invocation.clone()).await?;
        let execution_finished = outcome.execution_finished;
        let result = outcome.result;
        let failure_json = result.as_ref().err().map(|error| {
            control_tool_failure_json(&invocation, error, outcome.plan_document.as_ref())
        });
        let sender_guard = self
            .sender
            .lock()
            .map_err(|_| anyhow::anyhow!("Provider control-tool router lock poisoned"))?;
        anyhow::ensure!(
            sender_guard
                .as_ref()
                .is_some_and(|current| current.same_channel(&active_sender)),
            "Provider control-tool result belongs to a collected turn"
        );
        drop(sender_guard);
        let result = match result {
            Ok(result) => result,
            Err(_) => {
                return Ok(
                    failure_json.expect("failed control-tool invocation must format one failure")
                );
            }
        };
        self.execution_finished
            .fetch_or(execution_finished, std::sync::atomic::Ordering::AcqRel);
        let Some(accepted) = result.invocation else {
            if execution_finished {
                admission.send(invocation);
            }
            return Ok(result.message);
        };
        let invocation = accepted;
        admission.send(invocation);
        Ok(result.message)
    }

    pub(crate) async fn close(&self) -> Result<()> {
        let runtime = self.cancel()?;
        if let Some(runtime) = runtime {
            runtime.close().await;
        }
        Ok(())
    }

    pub(crate) fn execution_finished(&self) -> bool {
        self.execution_finished
            .load(std::sync::atomic::Ordering::Acquire)
    }

    pub(crate) fn cancel(&self) -> Result<Option<Arc<ControlToolExecutor>>> {
        let runtime = self
            .runtime
            .lock()
            .map_err(|_| anyhow::anyhow!("Provider control-tool runtime lock poisoned"))?
            .clone();
        if let Some(runtime) = &runtime {
            runtime.cancel();
        }
        Ok(runtime)
    }
}

pub(crate) struct ControlToolStream {
    router: Arc<ControlToolRouter>,
    receiver: mpsc::Receiver<ControlToolInvocation>,
}

impl ControlToolStream {
    pub(crate) async fn receive(&mut self) -> Option<ControlToolInvocation> {
        self.receiver.recv().await
    }

    pub(crate) fn try_receive(
        &mut self,
    ) -> Result<ControlToolInvocation, mpsc::error::TryRecvError> {
        self.receiver.try_recv()
    }
}

impl Drop for ControlToolStream {
    fn drop(&mut self) {
        if let Ok(mut sender) = self.router.sender.lock() {
            *sender = None;
            if let Ok(mut runtime) = self.router.runtime.lock()
                && let Some(runtime) = runtime.take()
            {
                runtime.cancel();
            }
        }
    }
}

#[async_trait]
impl ProviderTools for ControlToolRouter {
    async fn call(&self, call: ToolCall) -> Result<String, ProviderError> {
        self.route(ControlToolInvocation {
            task_permit: None,
            name: call.name,
            arguments: call.arguments,
        })
        .await
        .map_err(ProviderError::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::PromptMode;
    use serde_json::{Value, json};

    #[tokio::test]
    async fn routes_tools_without_provider_specific_schemas() {
        let router = Arc::new(ControlToolRouter::default());
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.task_controller = Some(task_controller);
        let mut stream = router.activate(context).unwrap();
        router
            .call(ToolCall {
                name: "goal_complete".into(),
                arguments: serde_json::json!({ "summary": "Complete" }),
            })
            .await
            .unwrap();
        let invocation = stream.receive().await.unwrap();
        assert_eq!(invocation.name, "goal_complete");
        assert!(invocation.task_permit.is_some());
    }

    #[tokio::test]
    async fn collecting_control_stream_cancels_pending_runtime_work() -> Result<()> {
        let router = Arc::new(ControlToolRouter::default());
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.task_controller = Some(task_controller);
        context.checkpoint = Some(crate::checkpoint::ToolGate::new()?);
        let mut stream = router.activate(context)?;
        let mut pending = Box::pin(router.route(ControlToolInvocation {
            task_permit: None,
            name: "goal_complete".into(),
            arguments: json!({"summary":"complete"}),
        }));
        assert!(futures_util::poll!(pending.as_mut()).is_pending());
        let (outcome, closed) = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            tokio::join!(pending, router.close())
        })
        .await?;
        closed?;
        assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(stream.try_receive().is_err());
        drop(stream);
        let mut replacement = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        replacement.task_controller = Some(task_controller);
        let mut stream = router.activate(replacement)?;
        router
            .route(ControlToolInvocation {
                task_permit: None,
                name: "goal_complete".into(),
                arguments: json!({"summary":"replacement"}),
            })
            .await?;
        assert_eq!(stream.receive().await.unwrap().name, "goal_complete");
        Ok(())
    }

    #[tokio::test]
    async fn returns_control_failures_as_provider_neutral_json() {
        let router = Arc::new(ControlToolRouter::default());
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.task_controller = Some(task_controller);
        let mut stream = router.activate(context).unwrap();

        let response = router
            .route(ControlToolInvocation {
                task_permit: None,
                name: "goal_complete".into(),
                arguments: serde_json::json!({}),
            })
            .await
            .unwrap();
        let failure: Value = serde_json::from_str(&response).expect("valid failure JSON");

        assert_eq!(failure["ok"], false);
        assert_eq!(failure["tool"], "goal_complete");
        assert_eq!(failure["phase"], "argument_validation");
        assert_eq!(failure["code"], "invalid_arguments");
        assert_eq!(failure["violation"][0]["path"], "<arguments>");
        assert!(stream.try_receive().is_err());
    }

    #[tokio::test]
    async fn control_admission_rejects_before_terminal_mutation() {
        let router = Arc::new(ControlToolRouter::default());
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.task_controller = Some(task_controller);
        let mut stream = router.activate(context).unwrap();
        for _ in 0..32 {
            router
                .route(ControlToolInvocation {
                    task_permit: None,
                    name: "goal_status".into(),
                    arguments: serde_json::json!({ "status": "working" }),
                })
                .await
                .unwrap();
        }
        let terminal = ControlToolInvocation {
            task_permit: None,
            name: "goal_complete".into(),
            arguments: serde_json::json!({ "summary": "finished" }),
        };
        assert!(
            router
                .route(terminal.clone())
                .await
                .unwrap_err()
                .to_string()
                .contains("admission is full")
        );
        assert_eq!(stream.receive().await.unwrap().name, "goal_status");
        assert_eq!(
            router.route(terminal).await.unwrap(),
            "Goal control accepted"
        );
        for _ in 0..31 {
            assert_eq!(stream.receive().await.unwrap().name, "goal_status");
        }
        assert_eq!(stream.receive().await.unwrap().name, "goal_complete");
        assert!(stream.try_receive().is_err());
    }

    #[tokio::test]
    async fn large_control_invocation_preserves_runtime_and_content() {
        let router = Arc::new(ControlToolRouter::default());
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.task_controller = Some(task_controller);
        let mut stream = router.activate(context).unwrap();
        let summary = "λ".repeat(256 * 1024 + 1);
        assert_eq!(
            router
                .route(ControlToolInvocation {
                    task_permit: None,
                    name: "goal_complete".into(),
                    arguments: serde_json::json!({"summary":summary}),
                })
                .await
                .unwrap(),
            "Goal control accepted"
        );
        let invocation = stream.receive().await.unwrap();
        assert_eq!(invocation.name, "goal_complete");
        assert_eq!(invocation.arguments["summary"], summary);
    }
}
