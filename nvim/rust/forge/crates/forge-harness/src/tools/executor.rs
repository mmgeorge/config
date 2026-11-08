use super::runtime::ControlToolRuntime;
use super::{ControlToolInvocation, ControlToolResult, ControlTurnContext};
use crate::plan::PlanDocument;
use crate::tools::ToolResult as Result;
use std::sync::Mutex;
use tokio::sync::{Notify, watch};

/// Owns tool execution and cancellation for one provider turn.
pub(crate) struct ControlToolExecutor {
    lsp_mutations: std::sync::Arc<super::lsp::MutationWork>,
    completion: std::sync::Arc<crate::provider::CompletionSlot>,
    task_controller: Option<std::sync::Arc<crate::task::SessionTaskController>>,
    task_permit: Option<crate::task::TaskPermit>,
    authority: Option<super::TaskAuthority>,
    mode: crate::provider::PromptMode,
    observed_lsp: Option<std::sync::Arc<super::lsp::LspScope>>,
    observed_repository: Option<std::sync::Arc<super::repository::RepositoryToolScope>>,
    runtime: tokio::sync::Mutex<ControlToolRuntime>,
    admission: Mutex<Admission>,
    cancelled: watch::Sender<bool>,
    settled: Notify,
}

impl ControlToolExecutor {
    /// Get whether original mutation collection retained a failure despite a lost tool waiter.
    pub(crate) fn mutation_failed(&self) -> bool {
        self.lsp_mutations.failed()
    }

    /// Creates a fresh execution generation from the broker's turn context.
    pub(crate) fn new(context: ControlTurnContext) -> Self {
        let completion = context.provider_completion.clone();
        let runtime = ControlToolRuntime::new(context.clone());
        Self {
            lsp_mutations: runtime.lsp_mutations.clone(),
            completion,
            task_controller: context.task_controller.clone(),
            task_permit: context.task_permit.clone(),
            authority: super::TaskAuthority::capture(&context),
            mode: context.mode,
            observed_lsp: context
                .observed_file_changes
                .as_ref()
                .and(context.lsp.clone()),
            observed_repository: context
                .observed_file_changes
                .as_ref()
                .and(context.repository.clone()),
            runtime: tokio::sync::Mutex::new(runtime),
            admission: Mutex::new(Admission::default()),
            cancelled: watch::channel(false).0,
            settled: Notify::new(),
        }
    }

    /// Serializes canonical mutations while allowing cancellation to release any tool wait.
    pub(crate) async fn execute(
        &self,
        invocation: ControlToolInvocation,
    ) -> anyhow::Result<ControlToolOutcome<'_>> {
        self.execute_with_barrier(invocation, None).await
    }

    /// Signal after a waiting question is committed, allowing later ordered mutations.
    pub(crate) async fn execute_with_barrier(
        &self,
        mut invocation: ControlToolInvocation,
        barrier: Option<tokio::sync::oneshot::Sender<()>>,
    ) -> anyhow::Result<ControlToolOutcome<'_>> {
        let mut barrier = QuestionBarrier(barrier);
        if let Some(authority) = &self.authority {
            authority.validate_scope()?;
        }
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
        let permit = self
            .authority
            .as_ref()
            .map(|authority| authority.current_permit())
            .transpose()?
            .flatten()
            .or_else(|| self.task_permit.clone());
        invocation.admit_turn_control(
            self.task_controller.as_deref(),
            permit.as_ref(),
            self.mode,
        )?;
        let mut cancelled = self.cancelled.subscribe();
        let mut runtime = tokio::select! {
            biased;
            _ = cancelled.wait_for(|closed| *closed) => return Err(crate::broker::TurnCancelled.into()),
            runtime = self.runtime.lock() => runtime,
        };
        if *cancelled.borrow() {
            return Err(crate::broker::TurnCancelled.into());
        }
        if let Some(authority) = &self.authority {
            authority.validate_scope()?;
        }
        if invocation.name == "question_ask"
            && invocation.arguments["wait"].as_bool() == Some(true)
            && runtime.context.questions.is_some()
        {
            let prepared = super::questions::prepare_wait(&runtime, &invocation);
            let plan_document = runtime.plan_document().cloned();
            let execution_finished = runtime.execution_finished();
            drop(runtime);
            // Question creation and wait retirement are owned by the session queue.
            // Publish cancellation as a settled tool result so host-effect collection
            // does not mistake this read wait for an abandoned mutation.
            let prepared = match prepared {
                Ok(prepared) => tokio::select! {
                    biased;
                    _ = cancelled.wait_for(|closed| *closed) => Err(crate::tools::ToolError::from(crate::broker::TurnCancelled)),
                    wait = prepared.publish() => wait,
                },
                Err(error) => Err(error),
            };
            barrier.release();
            let result = match prepared {
                Ok(wait) => {
                    let answers = tokio::select! {
                        biased;
                        _ = cancelled.wait_for(|closed| *closed) => Err(crate::tools::ToolError::from(crate::broker::TurnCancelled)),
                        answers = wait.answers() => answers.map_err(crate::tools::ToolError::from),
                    };
                    match answers {
                        Ok(answers) => {
                            if let Some(authority) = &self.authority {
                                authority.validate_scope()?;
                            }
                            Ok(ControlToolResult {
                                invocation: None,
                                message: serde_json::json!({"answers":answers}).to_string(),
                            })
                        }
                        Err(error) => Err(error),
                    }
                }
                Err(error) => Err(error),
            };
            return Ok(ControlToolOutcome {
                _execution: execution,
                result,
                plan_document,
                execution_finished,
            });
        }
        let result = tokio::select! {
            biased;
            result = runtime.invoke(invocation) => result,
            _ = cancelled.wait_for(|closed| *closed) => return Err(crate::broker::TurnCancelled.into()),
        };
        Ok(ControlToolOutcome {
            _execution: execution,
            result,
            plan_document: runtime.plan_document().cloned(),
            execution_finished: runtime.execution_finished(),
        })
    }

    /// Revokes this generation without waiting for the runtime's execution lock.
    pub(crate) fn cancel(&self) {
        if let Some(lsp) = &self.observed_lsp {
            lsp.retire();
        }
        if let Some(repository) = &self.observed_repository {
            repository.retire();
        }
        self.admission
            .lock()
            .expect("control admission lock poisoned")
            .closed = true;
        self.cancelled.send_replace(true);
    }

    /// Cancels and collects all admitted calls before the provider releases its turn.
    pub(crate) async fn close(&self) {
        self.cancel();
        self.finish().await;
    }

    /// Close admission and collect published effects without cancelling admitted tool work.
    pub(crate) async fn finish(&self) {
        self.admission
            .lock()
            .expect("control admission lock poisoned")
            .closed = true;
        loop {
            let settled = self.settled.notified();
            tokio::pin!(settled);
            settled.as_mut().enable();
            if self
                .admission
                .lock()
                .expect("control admission lock poisoned")
                .active
                == 0
            {
                if let Some(lsp) = &self.observed_lsp {
                    lsp.retire();
                }
                if let Some(repository) = &self.observed_repository {
                    repository.retire();
                }
                if let Some(failure) = self.lsp_mutations.finish().await {
                    let _ = self
                        .completion
                        .publish(crate::provider::ProviderCompletion {
                            control_error: Some(failure.to_string()),
                            ..Default::default()
                        });
                }
                return;
            }
            settled.await;
        }
    }
}

struct QuestionBarrier(Option<tokio::sync::oneshot::Sender<()>>);

impl QuestionBarrier {
    fn release(&mut self) {
        if let Some(sender) = self.0.take() {
            let _ = sender.send(());
        }
    }
}

impl Drop for QuestionBarrier {
    fn drop(&mut self) {
        self.release();
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
    use crate::provider::PromptMode;
    use anyhow::{Context, Result};

    #[tokio::test]
    async fn task_replacement_invalidates_running_and_queued_goal_controls() -> Result<()> {
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.bind_test_task_controller(controller.clone());
        let checkpoint = crate::checkpoint::ToolGate::new();
        context.checkpoint = Some(checkpoint.clone());
        let executor = ControlToolExecutor::new(context);
        let invocation = || ControlToolInvocation {
            call_id: None,
            name: "goal_complete".into(),
            arguments: serde_json::json!({"summary":"Finish the old goal"}),
            task_permit: None,
        };
        let mut running = Box::pin(executor.execute(invocation()));
        let mut queued = Box::pin(executor.execute(invocation()));
        assert!(futures_util::poll!(running.as_mut()).is_pending());
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        let previous = controller.current()?.unwrap();
        let replacement = controller
            .change(
                &mut store,
                Some(&previous.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                    "Replacement".into(),
                    Default::default(),
                    2,
                ))),
                2,
            )?
            .unwrap();
        checkpoint.complete(Ok(()));
        let running = running.await?.result.unwrap_err();
        let queued = queued.await.unwrap_err();
        assert!(running.is::<crate::task::StaleTask>());
        assert!(queued.is::<crate::task::StaleTask>());
        assert_eq!(
            controller.current()?.unwrap().revision,
            replacement.revision
        );
        assert!(
            executor
                .execute(invocation())
                .await
                .unwrap_err()
                .is::<crate::task::StaleTask>()
        );
        let mut fresh = ControlTurnContext::inactive(PromptMode::Chat);
        fresh.bind_test_task_controller(controller.clone());
        let fresh_executor = ControlToolExecutor::new(fresh);
        let accepted = fresh_executor.execute(invocation()).await?.result?;
        accepted
            .invocation
            .unwrap()
            .task_permit
            .unwrap()
            .validate(Some(&replacement))?;
        Ok(())
    }
    use serde_json::json;
    use std::sync::Arc;

    #[tokio::test]
    async fn cancellation_releases_running_and_queued_tools_without_reusing_their_state()
    -> Result<()> {
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        context.bind_test_task_controller(task_controller);
        context.checkpoint = Some(crate::checkpoint::ToolGate::new());
        let executor = ControlToolExecutor::new(context);
        let invocation = || ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "goal_complete".into(),
            arguments: json!({"summary":"finished"}),
        };
        let mut running = Box::pin(executor.execute(invocation()));
        let mut queued = Box::pin(executor.execute(invocation()));
        assert!(futures_util::poll!(running.as_mut()).is_pending());
        assert!(futures_util::poll!(queued.as_mut()).is_pending());
        let close = executor.close();
        let (running, queued, ()) = async { tokio::join!(running, queued, close) }.await;
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
        let (_task_root, _task_store, task_controller) =
            crate::task::SessionTaskController::test_goal();
        replacement.bind_test_task_controller(task_controller);
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
            tokio::sync::watch::channel(false).1,
        );
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.lsp = Some(scope);
        let executor = ControlToolExecutor::new(context);
        let pending = executor.execute(ControlToolInvocation {
            call_id: None,
            task_permit: None,
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
        let (outcome, cancelled) = async { tokio::join!(pending, cancel) }.await;
        assert!(outcome.unwrap_err().is::<crate::broker::TurnCancelled>());
        cancelled?;
        Ok(())
    }
}
