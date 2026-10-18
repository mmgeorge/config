use super::runtime::ControlToolRuntime;
use super::{ControlToolInvocation, ControlToolResult, ControlTurnContext};
use crate::plan::PlanDocument;
use anyhow::Result;
use std::collections::BTreeMap;
use std::sync::Mutex;
use tokio::sync::{Notify, watch};

/// Owns tool execution and cancellation for one provider turn.
pub(crate) struct ControlToolExecutor {
    runtime: tokio::sync::Mutex<ControlToolRuntime>,
    admission: Mutex<Admission>,
    cancelled: watch::Sender<bool>,
    settled: Notify,
}

impl ControlToolExecutor {
    /// Creates a fresh execution generation from the broker's turn context.
    pub(crate) fn new(context: ControlTurnContext) -> Self {
        Self {
            runtime: tokio::sync::Mutex::new(ControlToolRuntime::new(context)),
            admission: Mutex::new(Admission::default()),
            cancelled: watch::channel(false).0,
            settled: Notify::new(),
        }
    }

    /// Serializes canonical mutations while allowing cancellation to release any tool wait.
    pub(crate) async fn execute(
        &self,
        invocation: ControlToolInvocation,
    ) -> Result<ControlToolOutcome<'_>> {
        let execution = {
            let mut admission = self
                .admission
                .lock()
                .expect("control admission lock poisoned");
            if admission.closed {
                return Err(crate::broker::TurnCancelled.into());
            }
            admission.active += 1;
            Execution { owner: self }
        };
        let mut cancelled = self.cancelled.subscribe();
        let mut runtime = tokio::select! {
            biased;
            _ = cancelled.wait_for(|closed| *closed) => return Err(crate::broker::TurnCancelled.into()),
            runtime = self.runtime.lock() => runtime,
        };
        if *cancelled.borrow() {
            return Err(crate::broker::TurnCancelled.into());
        }
        let result = tokio::select! {
            biased;
            result = runtime.invoke(invocation) => result,
            _ = cancelled.wait_for(|closed| *closed) => return Err(crate::broker::TurnCancelled.into()),
        };
        let source_digests = result
            .as_ref()
            .ok()
            .and_then(|result| result.invocation.as_ref())
            .and_then(|invocation| invocation.arguments["expected_version"].as_u64())
            .and_then(|version| runtime.patch_source_digests(version))
            .cloned();
        Ok(ControlToolOutcome {
            _execution: execution,
            result,
            plan_document: runtime.plan_document().cloned(),
            source_digests,
            execution_finished: runtime.execution_finished(),
        })
    }

    /// Revokes this generation without waiting for the runtime's execution lock.
    pub(crate) fn cancel(&self) {
        self.admission
            .lock()
            .expect("control admission lock poisoned")
            .closed = true;
        self.cancelled.send_replace(true);
    }

    /// Cancels and collects all admitted calls before the provider releases its turn.
    pub(crate) async fn close(&self) {
        self.cancel();
        loop {
            let settled = self.settled.notified();
            if self
                .admission
                .lock()
                .expect("control admission lock poisoned")
                .active
                == 0
            {
                return;
            }
            settled.await;
        }
    }
}

#[derive(Debug)]
/// Preserves the outcome and holds admission until its consumer publishes or discards it.
pub(crate) struct ControlToolOutcome<'owner> {
    _execution: Execution<'owner>,
    /// Contains the accepted tool result or its semantic error.
    pub(crate) result: Result<ControlToolResult>,
    /// Captures the canonical plan after execution, including error context.
    pub(crate) plan_document: Option<PlanDocument>,
    /// Binds a replayed patch to the source revision validated by the runtime.
    pub(crate) source_digests: Option<BTreeMap<String, String>>,
    /// Identifies a phase transition that requires ending the native turn.
    pub(crate) execution_finished: bool,
}

#[derive(Default)]
struct Admission {
    active: usize,
    closed: bool,
}

struct Execution<'owner> {
    owner: &'owner ControlToolExecutor,
}

impl std::fmt::Debug for Execution<'_> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Execution")
    }
}

impl Drop for Execution<'_> {
    fn drop(&mut self) {
        let mut admission = self
            .owner
            .admission
            .lock()
            .expect("control admission lock poisoned");
        admission.active -= 1;
        if admission.active == 0 {
            self.owner.settled.notify_waiters();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::PromptMode;
    use anyhow::Context;
    use serde_json::json;
    use std::sync::{Arc, atomic::AtomicBool};
    use std::time::Duration;

    #[tokio::test]
    async fn cancellation_releases_running_and_queued_tools_without_reusing_their_state()
    -> Result<()> {
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.has_active_goal = true;
        context.checkpoint = Some(crate::checkpoint::ToolGate::new()?);
        let executor = ControlToolExecutor::new(context);
        let invocation = || ControlToolInvocation {
            name: "goal_complete".into(),
            arguments: json!({"summary":"finished"}),
        };
        let mut running = Box::pin(executor.execute(invocation()));
        let mut queued = Box::pin(executor.execute(invocation()));
        assert!(futures_util::poll!(running.as_mut()).is_pending());
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        let close = executor.close();
        let (running, queued, ()) = tokio::time::timeout(Duration::from_secs(1), async {
            tokio::join!(running, queued, close)
        })
        .await?;
        assert!(running.unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(queued.unwrap_err().is::<crate::broker::TurnCancelled>());
        assert!(
            executor
                .execute(invocation())
                .await
                .unwrap_err()
                .is::<crate::broker::TurnCancelled>()
        );
        let mut replacement = ControlTurnContext::inactive(PromptMode::Chat);
        replacement.has_active_goal = true;
        assert!(
            ControlToolExecutor::new(replacement)
                .execute(invocation())
                .await?
                .result
                .is_ok()
        );
        Ok(())
    }

    #[tokio::test]
    async fn cleanup_retains_completed_results_until_their_consumer_releases_them() -> Result<()> {
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.has_active_goal = true;
        let executor = ControlToolExecutor::new(context);
        let outcome = executor
            .execute(ControlToolInvocation {
                name: "goal_status".into(),
                arguments: json!({"status":"working"}),
            })
            .await?;
        assert!(outcome.result.is_ok());
        let mut close = Box::pin(executor.close());
        assert!(futures_util::poll!(close.as_mut()).is_pending());
        drop(outcome);
        tokio::time::timeout(Duration::from_secs(1), close).await?;
        Ok(())
    }

    #[tokio::test]
    async fn cancellation_revokes_the_editor_request_and_rejects_its_late_response() -> Result<()> {
        let directory = tempfile::tempdir()?;
        std::fs::write(directory.path().join("source.rs"), "pub fn source() {}\n")?;
        let lsp = Arc::new(super::super::lsp::LspService::default());
        let (sender, mut editor) = forge_protocol::outbound::channel();
        lsp.attach(sender);
        lsp.set_enabled("session", true);
        let policy = Arc::new(
            crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::default_permission_document(),
                directory.path(),
            ),
        );
        let (scope, _lease) = lsp.scope(
            "session".into(),
            "exchange".into(),
            directory.path().into(),
            policy,
            crate::session::PermissionMode::Read,
            Arc::new(AtomicBool::new(false)),
        );
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.lsp = Some(scope);
        let executor = ControlToolExecutor::new(context);
        let pending = executor.execute(ControlToolInvocation {
            name: "lsp_context".into(),
            arguments: json!({"path":"source.rs","line":1,"column":1}),
        });
        let cancel = async {
            let query = editor.recv().await?.context("editor request is missing")?;
            let query: serde_json::Value = serde_json::from_slice(query.bytes())?;
            assert_eq!(query["event"], "lsp.request");
            executor.close().await;
            let event = editor
                .recv()
                .await?
                .context("editor cancellation is missing")?;
            let event: serde_json::Value = serde_json::from_slice(event.bytes())?;
            assert_eq!(event["event"], "lsp.cancel");
            assert!(
                lsp.respond(
                    "session",
                    query["payload"]["id"].as_str().unwrap(),
                    json!({}),
                    None
                )
                .is_err()
            );
            Ok::<_, anyhow::Error>(())
        };
        let (outcome, cancelled) = tokio::time::timeout(Duration::from_secs(1), async {
            tokio::join!(pending, cancel)
        })
        .await?;
        assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
        cancelled?;
        Ok(())
    }
}
