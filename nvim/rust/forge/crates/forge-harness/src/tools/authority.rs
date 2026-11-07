use anyhow::{Context, Result, ensure};
use std::sync::Arc;

use crate::task::{SessionTaskController, TaskPermit};

/// Retain a callback's captured evaluator and its revocation authority.
#[derive(Clone)]
pub(crate) struct PermissionAuthoritySnapshot {
    store: Arc<std::sync::RwLock<crate::permissions::store::PermissionStore>>,
    compiled: Arc<crate::permissions::matcher::CompiledPermissionDocument>,
    epoch: crate::permissions::store::PermissionRevocationEpoch,
}

impl PermissionAuthoritySnapshot {
    pub(crate) fn capture(
        store: Arc<std::sync::RwLock<crate::permissions::store::PermissionStore>>,
        current: &crate::permissions::store::PermissionStore,
    ) -> Self {
        Self {
            store,
            compiled: current.compiled(),
            epoch: current.revocation_epoch(),
        }
    }

    /// Get the evaluator captured with the callback authority.
    pub(crate) fn compiled(&self) -> Arc<crate::permissions::matcher::CompiledPermissionDocument> {
        self.compiled.clone()
    }
}

/// Share only owner-confirmed revision advancement within one immutable invocation.
#[derive(Debug)]
pub(crate) struct TaskAuthorityProgress {
    current: std::sync::Mutex<TaskPermit>,
}

impl TaskAuthorityProgress {
    pub(crate) fn new(permit: TaskPermit) -> Self {
        Self {
            current: std::sync::Mutex::new(permit),
        }
    }
    pub(crate) fn permit(&self) -> Result<TaskPermit> {
        Ok(self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("task authority progress lock poisoned"))?
            .clone())
    }
}

/// Preserves the task contract that admitted a provider callback across asynchronous waits.
#[derive(Clone)]
pub(crate) struct TaskAuthority {
    controller: Option<Arc<SessionTaskController>>,
    permit: Option<TaskPermit>,
    progress: Option<Arc<TaskAuthorityProgress>>,
    generation: Option<u64>,
    permission_snapshot: Option<PermissionAuthoritySnapshot>,
    permission_required: bool,
}

impl TaskAuthority {
    /// Check that a callback is being committed by the exact session task controller that admitted it.
    pub(crate) fn belongs_to(&self, controller: &Arc<SessionTaskController>) -> bool {
        self.controller
            .as_ref()
            .is_some_and(|captured| Arc::ptr_eq(captured, controller))
    }

    /// Get the captured task attribution without granting authority to a replacement task.
    pub(crate) fn task_id(&self) -> Option<&crate::task::TaskId> {
        self.permit.as_ref().map(TaskPermit::task_id)
    }

    pub(crate) fn capture(context: &super::ControlTurnContext) -> Option<Self> {
        context.task_controller.as_ref().map(|_| Self {
            controller: context.task_controller.clone(),
            permit: context.task_permit.clone(),
            progress: context.task_authority_progress.clone(),
            generation: context.task_generation,
            permission_snapshot: context.permission_snapshot.clone(),
            permission_required: context.permission_required,
        })
    }

    pub(crate) fn current_permit(&self) -> Result<Option<TaskPermit>> {
        self.progress
            .as_ref()
            .map(|progress| progress.permit())
            .transpose()
            .map(|permit| permit.or_else(|| self.permit.clone()))
    }

    /// Capture the last owner-confirmed permit without following later parent advancement.
    pub(crate) fn snapshot(&self) -> Result<Self> {
        let mut snapshot = self.clone();
        snapshot.permit = self.current_permit()?;
        snapshot.progress = None;
        Ok(snapshot)
    }

    /// Adopt only a revision explicitly returned by this invocation's live owner commit.
    pub(crate) fn advance(&self, next: TaskPermit) -> Result<()> {
        let progress = self
            .progress
            .as_ref()
            .context("invocation has no task authority progress")?;
        let previous = progress.permit()?;
        ensure!(
            previous.receipt() == next.receipt(),
            "live task authority changed observation generation"
        );
        let controller = self
            .controller
            .as_ref()
            .context("task authority has no controller")?;
        let generation = self
            .generation
            .context("task callback generation was not captured")?;
        controller.with_callback_authority(generation, Some(&next), || {
            if self.permission_required && self.permission_snapshot.is_none() {
                anyhow::bail!("permission authority was not captured");
            }
            let permission = self
                .permission_snapshot
                .as_ref()
                .map(|snapshot| -> Result<_> {
                    let guard = snapshot
                        .store
                        .read()
                        .map_err(|_| anyhow::anyhow!("permission authority lock poisoned"))?;
                    ensure!(
                        guard.revocation_epoch() == snapshot.epoch,
                        "permission authority changed before task advancement"
                    );
                    Ok(guard)
                })
                .transpose()?;
            let mut current = progress
                .current
                .lock()
                .map_err(|_| anyhow::anyhow!("task authority progress lock poisoned"))?;
            ensure!(
                current.receipt() == next.receipt(),
                "task authority advancement lost its invocation"
            );
            *current = next.clone();
            drop(permission);
            Ok(())
        })
    }

    /// Rejects callbacks whose task was replaced, suspended, cleared, or not acknowledged.
    pub(crate) fn validate(&self) -> Result<()> {
        self.commit(|| {
            if self.permit.is_some() {
                let current = self
                    .controller
                    .as_ref()
                    .context("task callback has no session controller")?
                    .current()?;
                ensure!(
                    current.as_ref().is_some_and(|task| task.entry_accepted()),
                    "task entry has not been acknowledged by the provider"
                );
            }
            Ok(())
        })
    }

    /// Fence task and taskless scopes without imposing an entry-acknowledgment policy.
    pub(crate) fn validate_scope(&self) -> Result<()> {
        self.commit(|| Ok(()))
    }

    /// Commit typed provider control under the same atomic task and permission authority.
    pub(crate) fn commit_control<T>(
        &self,
        commit: impl FnOnce() -> std::result::Result<T, crate::provider::ProviderError>,
    ) -> std::result::Result<T, crate::provider::ProviderError> {
        // Keep callback errors as values inside the existing gate. Only the
        // authority layer's own legacy validation failures cross its adapter.
        self.commit(|| Ok(commit())).map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::TurnStart,
                source,
            )
        })?
    }

    /// Run a short synchronous section while the admitting task generation and permit remain current.
    fn with_task_gate<T>(&self, section: impl FnOnce() -> Result<T>) -> Result<T> {
        let controller = self
            .controller
            .as_ref()
            .context("task callback has no session controller")?;
        let generation = self
            .generation
            .context("task callback generation was not captured")?;
        let permit = self
            .progress
            .as_ref()
            .map(|progress| progress.permit())
            .transpose()?
            .or_else(|| self.permit.clone());
        controller.with_callback_authority(generation, permit.as_ref(), section)
    }

    /// Fence a new permission request by its task alone.
    ///
    /// The request is evaluated against the current permission policy, so a rule saved earlier
    /// in the turn, such as an exact denial of another command, must not reject it. The captured
    /// permission epoch still fences commits that relied on the earlier policy.
    pub(crate) fn validate_task_for_new_request(&self) -> Result<()> {
        self.with_task_gate(|| Ok(()))
    }

    /// Validate under the shared task gate and perform only a short synchronous commit.
    pub(crate) fn commit<T>(&self, commit: impl FnOnce() -> Result<T>) -> Result<T> {
        self.with_task_gate(|| {
            if self.permission_required && self.permission_snapshot.is_none() {
                anyhow::bail!("permission authority was not captured");
            }
            let permission = self
                .permission_snapshot
                .as_ref()
                .map(|snapshot| -> Result<_> {
                    let guard = snapshot
                        .store
                        .read()
                        .map_err(|_| anyhow::anyhow!("permission authority lock poisoned"))?;
                    anyhow::ensure!(
                        guard.revocation_epoch() == snapshot.epoch,
                        "permission authority changed before callback"
                    );
                    Ok(guard)
                })
                .transpose()?;
            let result = commit();
            drop(permission);
            result
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::provider::ScopeOwner;
    use crate::task::{Task, TaskChange};

    /// Share the persisted baseline receipt across parent collection while retaining the child's original task fence.
    #[tokio::test]
    async fn child_checkpoint_survives_parent_collection_but_task_revocation_denies_mutation()
    -> Result<()> {
        let (directory, mut store, controller) = SessionTaskController::test_goal();
        let parent_scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root("session"),
            "exchange",
        );
        let child_scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::new(
                "session".into(),
                forge_provider_api::AgentId::new("native-child-owner")?,
            )?,
            "exchange",
        );
        let child_address = forge_provider_api::TurnAddress {
            thread_id: child_scope.owner().thread_id(),
            turn_id: child_scope.turn_id()?,
            scope: child_scope.clone(),
        };
        let public_agent = forge_provider_api::AgentId::new("native-child")?;
        let proof = crate::provider::test_support::observed_agent_fixture(
            forge_provider_api::BindingId::new("binding")?,
            parent_scope.clone(),
            vec![(
                public_agent.clone(),
                forge_provider_api::agent::AgentDescription {
                    parent_agent_id: Some(parent_scope.owner().agent_id().clone()),
                    delegation: Some(forge_provider_api::agent::AgentDelegation {
                        parent_thread_id: parent_scope.owner().thread_id(),
                        parent_turn_id: parent_scope.turn_id()?,
                        parent_item_id: None,
                    }),
                    thread_id: Some(child_address.thread_id.clone()),
                    definition: None,
                    nickname: None,
                    task: None,
                },
                Some(child_address.clone()),
            )],
            public_agent,
            Some(child_address.turn_id),
        )?;
        let checkpoint = crate::checkpoint::ToolGate::new();
        checkpoint.complete(Ok(()));
        let mut parent =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        parent.bind_test_task_controller(controller.clone());
        parent.question_invocation = Some(parent_scope);
        parent.checkpoint = Some(checkpoint.clone());
        let (sink, _events) = crate::provider::events::channel(child_scope);
        let child = parent.fork_observed(&proof, sink)?;
        assert!(!Arc::ptr_eq(
            &parent.provider_completion,
            &child.provider_completion
        ));
        let authority = TaskAuthority::capture(&child).context("child task authority missing")?;
        crate::tools::ControlToolExecutor::new(parent)
            .finish()
            .await;
        checkpoint.complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
        child.checkpoint.as_ref().unwrap().wait().await?;
        let path = directory.path().join("child-write");
        authority.commit(|| {
            std::fs::write(&path, "committed child change")?;
            Ok(())
        })?;
        let task = controller
            .current()?
            .context("task disappeared before revocation")?;
        controller.change(&mut store, Some(&task.revision), TaskChange::Clear, 3)?;
        assert!(
            authority
                .commit(|| {
                    std::fs::write(&path, "revoked write")?;
                    Ok(())
                })
                .is_err()
        );
        assert_eq!(std::fs::read_to_string(path)?, "committed child change");
        Ok(())
    }

    /// Only an owner-confirmed revision within the original observation fence reaches sibling callbacks.
    #[test]
    fn owner_confirmation_advances_shared_task_authority_without_following_user_control()
    -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let initial = controller.current()?.unwrap();
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(initial.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        context.bind_task_authority_progress();
        let tools = TaskAuthority::capture(&context).unwrap();
        let permissions = tools.clone();
        controller.update(&mut store, &initial.permit(), 2, |task| {
            let crate::task::WorkflowSnapshot::Goal { goal } = &mut task.workflow else {
                unreachable!()
            };
            goal.objective.push_str(" with committed detail");
            Ok(())
        })?;
        assert!(
            tools.validate_scope().is_err(),
            "state changes alone cannot advance callback authority"
        );
        let advanced = controller.current()?.unwrap();
        tools.advance(advanced.permit())?;
        permissions.validate_scope()?;
        assert_eq!(
            context.current_task_permit()?.unwrap().revision(),
            &advanced.revision
        );
        let child = tools.snapshot()?;
        controller.update(&mut store, &advanced.permit(), 3, |task| {
            let crate::task::WorkflowSnapshot::Goal { goal } = &mut task.workflow else {
                unreachable!()
            };
            goal.objective.push_str(" with later parent detail");
            Ok(())
        })?;
        let latest = controller.current()?.unwrap();
        tools.advance(latest.permit())?;
        permissions.validate_scope()?;
        assert_eq!(
            child.current_permit()?.unwrap().revision(),
            &advanced.revision
        );
        assert!(
            child.validate_scope().is_err(),
            "child policy followed later parent advancement"
        );
        controller.change(&mut store, Some(&latest.revision), TaskChange::Pause, 4)?;
        let paused = controller.current()?.unwrap();
        assert!(
            tools.advance(paused.permit()).is_err(),
            "user control must not be adopted as owner progress"
        );
        assert!(permissions.validate_scope().is_err());
        Ok(())
    }

    /// Keep typed callback failures intact and reject revoked authority before invoking control.
    #[test]
    fn typed_control_commit_preserves_error_and_revocation_atomicity() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(task.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        let authority = TaskAuthority::capture(&context).unwrap();
        let invoked = std::cell::Cell::new(false);
        let error = authority
            .commit_control(|| {
                invoked.set(true);
                Err::<(), _>(crate::provider::ProviderError::Contract(
                    forge_provider_api::lifecycle::LifecycleError::UnknownExecution,
                ))
            })
            .unwrap_err();
        assert!(invoked.get());
        assert!(matches!(
            error,
            crate::provider::ProviderError::Contract(
                forge_provider_api::lifecycle::LifecycleError::UnknownExecution
            )
        ));
        controller.change(&mut store, Some(&task.revision), TaskChange::Clear, 2)?;
        invoked.set(false);
        let error = authority
            .commit_control(|| {
                invoked.set(true);
                Ok(())
            })
            .unwrap_err();
        assert!(
            !invoked.get(),
            "revoked authority invoked the native control callback"
        );
        let crate::provider::ProviderError::Failure(failure) = error else {
            panic!("authority failure lost its typed boundary");
        };
        assert_eq!(
            failure.operation(),
            forge_provider_api::error::ProviderOperation::TurnStart
        );
        Ok(())
    }

    #[test]
    fn approval_authority_rejects_replacement_and_unacknowledged_entry() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let mut context = super::super::ControlTurnContext::inactive(
            crate::provider::PromptMode::GoalContinuation,
        );
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(task.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        let authority = TaskAuthority::capture(&context).unwrap();
        authority.validate()?;
        let mut replacement = Task::goal("Replacement".into(), Default::default(), 2);
        replacement.request_entry("New instructions".into(), 2)?;
        let replacement = controller
            .change(
                &mut store,
                Some(&task.revision),
                TaskChange::Install(Box::new(replacement)),
                2,
            )?
            .unwrap();
        assert!(
            authority
                .validate()
                .unwrap_err()
                .is::<crate::task::StaleTask>()
        );
        context.task_permit = Some(replacement.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        let pending = TaskAuthority::capture(&context).unwrap();
        assert!(pending.validate().is_err());
        controller.acknowledge_entry(
            &mut store,
            &replacement.revision,
            &replacement.entry.as_ref().unwrap().id,
            3,
        )?;
        pending.validate()?;
        Ok(())
    }

    /// Dispatched callbacks may precede native acknowledgment without granting unscoped control.
    #[test]
    fn dispatched_callback_preserves_pending_receipt_and_rejects_revocation() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let original = controller.current()?.unwrap();
        let mut pending = Task::goal("Pending native receipt".into(), Default::default(), 2);
        pending.request_entry("Run the admitted task".into(), 2)?;
        let pending = controller
            .change(
                &mut store,
                Some(&original.revision),
                TaskChange::Install(Box::new(pending)),
                2,
            )?
            .unwrap();
        let mut context = super::super::ControlTurnContext::inactive(
            crate::provider::PromptMode::GoalContinuation,
        );
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(pending.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        let authority = TaskAuthority::capture(&context).unwrap();
        assert!(controller.admit(crate::task::TaskKind::Goal).is_err());
        assert!(authority.validate().is_err());
        authority.validate_scope()?;
        let mut invocation = super::super::ControlToolInvocation {
            call_id: None,
            name: "goal_status".into(),
            arguments: serde_json::json!({}),
            task_permit: None,
        };
        invocation.admit_turn_control(Some(&controller), Some(&pending.permit()), context.mode)?;
        assert!(
            !controller.current()?.unwrap().entry_accepted(),
            "callback must not fabricate a native receipt"
        );
        controller.change(&mut store, Some(&pending.revision), TaskChange::Pause, 3)?;
        assert!(authority.validate_scope().is_err());
        assert!(
            invocation
                .admit_turn_control(Some(&controller), Some(&pending.permit()), context.mode)
                .is_err()
        );
        Ok(())
    }

    #[test]
    fn taskless_callback_cannot_survive_task_install_and_clear() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let original = controller.current()?.unwrap();
        controller.change(&mut store, Some(&original.revision), TaskChange::Clear, 2)?;
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.task_controller = Some(controller.clone());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        let authority = TaskAuthority::capture(&context).unwrap();
        authority.validate()?;
        let next = controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(Task::goal(
                    "new goal".into(),
                    Default::default(),
                    3,
                ))),
                3,
            )?
            .unwrap();
        controller.change(&mut store, Some(&next.revision), TaskChange::Clear, 4)?;
        let mut mutated = false;
        assert!(
            authority
                .commit(|| {
                    mutated = true;
                    Ok(())
                })
                .is_err()
        );
        assert!(
            !mutated,
            "revoked taskless callback committed after task replacement"
        );
        Ok(())
    }

    /// A grant made by an approval does not revoke the same turn's control tools.
    #[tokio::test]
    async fn approved_allow_grant_preserves_followup_tool_but_deny_revokes_it() -> Result<()> {
        use crate::permissions::document::PermissionDecision;
        let (root, _store, controller) = SessionTaskController::test_goal();
        let permissions = Arc::new(std::sync::RwLock::new(
            crate::permissions::store::PermissionStore::load(
                root.path().join("permissions.json"),
                root.path(),
            )?,
        ));
        let captured = {
            let store = permissions.read().unwrap();
            PermissionAuthoritySnapshot::capture(permissions.clone(), &store)
        };
        let old = captured.compiled();
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.bind_test_task_controller(controller);
        let questions = Arc::new(crate::question::QuestionController::open(
            root.path(),
            "session",
        )?);
        context.questions = Some(questions.clone());
        context.question_exchange_id = Some("exchange".into());
        context.permission_snapshot = Some(captured);
        context.permission_required = true;
        let executor = super::super::ControlToolExecutor::new(context);
        let coordinator = Arc::new(crate::provider::approval::PermissionCoordinator::new(
            permissions.clone(),
        ));
        let (sink, mut stream) =
            crate::provider::events::channel(crate::provider::InvocationScope::new(
                crate::provider::ThreadOwner::root("approval"),
                "exchange",
            ));
        let waiting_coordinator = coordinator.clone();
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    crate::session::ApprovalMode::Ask,
                    crate::permissions::matcher::PermissionRequest {
                        id: "rg".into(),
                        provider: forge_provider_api::ProviderId::new("test")
                            .expect("static provider identity is valid"),
                        reason: None,
                        target_list: vec![crate::permissions::matcher::PermissionTarget::Command {
                            command: "rg --files".into(),
                            shell: None,
                        }],
                    },
                    None,
                    Some(&sink),
                )
                .await
        });
        let event = stream.recv().await?.context("approval event missing")?;
        let crate::provider::session_execution::OwnerObservation::Event(
            crate::provider::HarnessEvent::Host {
                event:
                    crate::provider::HostEvent::PermissionApproval {
                        execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                        event: crate::provider::PermissionApprovalEvent::Requested(view),
                    },
                ..
            },
        ) = event
        else {
            anyhow::bail!("expected approval request")
        };
        coordinator
            .resolve(
                view.request_id.as_str(),
                &[crate::provider::approval::ApprovalAnswer {
                    item_id: view.targets[0].target_id.as_str().to_owned(),
                    choice_id: "allow_broad".into(),
                }],
                None,
            )
            .await?;
        waiting.await??;
        assert!(!Arc::ptr_eq(&old, &permissions.read().unwrap().compiled()));
        let question = executor.execute(super::super::ControlToolInvocation { call_id: None,
            task_permit: None,
            name: "question_ask".into(),
            arguments: serde_json::json!({"questions":[{"header":"Choice", "question":"What should happen?", "options":[]}]}),
        }).await?;
        assert!(question.result.is_ok());
        drop(question);
        assert_eq!(questions.snapshot()?.questions.len(), 1);
        let invocation = || super::super::ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "goal_status".into(),
            arguments: serde_json::json!({"status":"working"}),
        };
        assert!(executor.execute(invocation()).await?.result.is_ok());
        permissions.write().unwrap().set_rule_list(&[(
            "read".into(),
            "private/**".into(),
            PermissionDecision::Deny,
        )])?;
        let error = executor.execute(invocation()).await.unwrap_err();
        assert!(error.to_string().contains("permission authority changed"));
        Ok(())
    }

    /// A saved denial fences commits under the captured policy but not new permission requests.
    ///
    /// Saving "always deny" for one command during a turn must not reject the turn's later,
    /// unrelated permission requests, which the coordinator evaluates against the current policy.
    #[test]
    fn saved_denial_admits_new_permission_requests_but_fences_captured_commits() -> Result<()> {
        use crate::permissions::document::PermissionDecision;
        let (root, _store, controller) = SessionTaskController::test_goal();
        let permissions = Arc::new(std::sync::RwLock::new(
            crate::permissions::store::PermissionStore::load(
                root.path().join("permissions.json"),
                root.path(),
            )?,
        ));
        let captured = {
            let store = permissions.read().unwrap();
            PermissionAuthoritySnapshot::capture(permissions.clone(), &store)
        };
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.bind_test_task_controller(controller);
        context.permission_snapshot = Some(captured);
        context.permission_required = true;
        let authority = TaskAuthority::capture(&context).unwrap();
        permissions.write().unwrap().set_rule_list(&[(
            "bash".into(),
            "whoami".into(),
            PermissionDecision::Deny,
        )])?;
        assert!(authority.validate_scope().is_err());
        authority.validate_task_for_new_request()?;
        Ok(())
    }

    /// Replacing a store cannot recycle an old permission generation.
    #[test]
    fn replacing_permission_store_revokes_captured_callback() -> Result<()> {
        let (root, _store, controller) = SessionTaskController::test_goal();
        let path = root.path().join("permissions.json");
        let permissions = Arc::new(std::sync::RwLock::new(
            crate::permissions::store::PermissionStore::load(&path, root.path())?,
        ));
        let captured = {
            let store = permissions.read().unwrap();
            PermissionAuthoritySnapshot::capture(permissions.clone(), &store)
        };
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.bind_test_task_controller(controller);
        context.permission_snapshot = Some(captured);
        context.permission_required = true;
        let authority = TaskAuthority::capture(&context).unwrap();
        authority.validate_scope()?;
        *permissions.write().unwrap() =
            crate::permissions::store::PermissionStore::load(&path, root.path())?;
        assert!(authority.validate_scope().is_err());
        Ok(())
    }

    #[test]
    fn policy_replacement_revokes_captured_callback_before_mutation() -> Result<()> {
        let (root, _store, controller) = SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let permissions = Arc::new(std::sync::RwLock::new(
            crate::permissions::store::PermissionStore::load(
                root.path().join("permissions.json"),
                root.path(),
            )?,
        ));
        let captured = {
            let current = permissions.read().unwrap();
            PermissionAuthoritySnapshot::capture(permissions.clone(), &current)
        };
        let mut context = super::super::ControlTurnContext::inactive(
            crate::provider::PromptMode::GoalContinuation,
        );
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(task.permit());
        context.task_generation = Some(controller.callback_snapshot()?.0);
        context.permission_snapshot = Some(captured);
        context.permission_required = true;
        let authority = TaskAuthority::capture(&context).unwrap();
        authority.validate_scope()?;
        permissions.write().unwrap().set_rule_list(&[(
            "read".into(),
            "private/**".into(),
            crate::permissions::document::PermissionDecision::Deny,
        )])?;
        let mut mutated = false;
        assert!(
            authority
                .commit(|| {
                    mutated = true;
                    Ok(())
                })
                .is_err()
        );
        assert!(
            !mutated,
            "old policy authority committed after policy replacement"
        );
        Ok(())
    }
}
