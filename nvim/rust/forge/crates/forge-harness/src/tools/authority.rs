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
    admission: Arc<std::sync::Mutex<MutationAdmission>>,
    execution_review: std::sync::Mutex<Option<ExecutionRevisionReview>>,
}

/// An execution revision this invocation submitted, and the review decision that answered it.
///
/// Submitting the revision, and later the review decision, renew the task's revision while
/// the submitting turn is still running. The invocation adopts exactly those renewals of its own
/// Resolve execution, so its plan reads stay available while the revision awaits review and its
/// later plan tools stay admitted after the decision, as the revision workflow promises. Any
/// other change, such as a pause, a phase change, or a replaced task, still makes it stale.
#[derive(Clone, Debug)]
struct ExecutionRevisionReview {
    execution_id: String,
    generation: u64,
    decided: bool,
}

/// Keep terminal sealing separate from task revocation and provider collection.
#[derive(Debug, Default)]
enum MutationAdmission {
    #[default]
    Open,
    Sealed,
}

impl TaskAuthorityProgress {
    pub(crate) fn new(permit: TaskPermit) -> Self {
        Self {
            current: std::sync::Mutex::new(permit),
            admission: Arc::default(),
            execution_review: std::sync::Mutex::default(),
        }
    }
    /// Freeze child revision authority while retaining the original invocation's mutation seal.
    pub(crate) fn fork(&self, permit: TaskPermit) -> Self {
        Self {
            current: std::sync::Mutex::new(permit),
            admission: self.admission.clone(),
            execution_review: std::sync::Mutex::default(),
        }
    }

    /// Record that this invocation submitted an execution revision now awaiting review.
    pub(crate) fn await_execution_review(&self, execution_id: &str, generation: u64) -> Result<()> {
        *self
            .execution_review
            .lock()
            .map_err(|_| anyhow::anyhow!("execution review lock poisoned"))? =
            Some(ExecutionRevisionReview {
                execution_id: execution_id.into(),
                generation,
                decided: false,
            });
        Ok(())
    }

    /// Adopt the task renewal of this invocation's own revision submission or its decision.
    ///
    /// The current task must be the same Execute task, still running Resolve in the same
    /// execution. While the revision awaits review the generation is the submitted one; the
    /// decision clears the pending revision and keeps that generation, or advances it once when
    /// a later turn carries the decision. After the decision nothing further is adopted.
    pub(crate) fn adopt_execution_review(&self, controller: &SessionTaskController) -> Result<()> {
        let mut review = self
            .execution_review
            .lock()
            .map_err(|_| anyhow::anyhow!("execution review lock poisoned"))?;
        let Some(pending) = review.as_mut().filter(|review| !review.decided) else {
            return Ok(());
        };
        let mut current = self
            .current
            .lock()
            .map_err(|_| anyhow::anyhow!("task authority progress lock poisoned"))?;
        let Some(task) = controller.current()? else {
            return Ok(());
        };
        if &task.id != current.task_id() || task.revision == *current.revision() {
            return Ok(());
        }
        let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow else {
            return Ok(());
        };
        let record = &execution.record;
        if record.id != pending.execution_id
            || record.state != crate::plan::PlanExecutionState::Active
            || record.phase != crate::plan::PlanPhase::Resolve
        {
            return Ok(());
        }
        let awaiting =
            record.pending_revision_reason.is_some() && record.generation == pending.generation;
        let decided = record.pending_revision_reason.is_none()
            && (record.generation == pending.generation
                || pending.generation.checked_add(1) == Some(record.generation));
        if awaiting || decided {
            *current = task.permit();
            pending.generation = record.generation;
            pending.decided = decided;
        }
        Ok(())
    }

    /// Get the execution generation this invocation adopted with its own revision, if any.
    pub(crate) fn adopted_execution_generation(&self, execution_id: &str) -> Result<Option<u64>> {
        Ok(self
            .execution_review
            .lock()
            .map_err(|_| anyhow::anyhow!("execution review lock poisoned"))?
            .as_ref()
            .filter(|review| review.execution_id == execution_id)
            .map(|review| review.generation))
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
    admission: Option<Arc<std::sync::Mutex<MutationAdmission>>>,
    generation: Option<u64>,
    permission_snapshot: Option<PermissionAuthoritySnapshot>,
    permission_required: bool,
}

impl TaskAuthority {
    /// Admit new work only while this invocation has no terminal request.
    pub(crate) fn admit_mutation(&self) -> Result<()> {
        if let Some(admission) = &self.admission {
            let state = admission
                .lock()
                .map_err(|_| anyhow::anyhow!("mutation admission lock poisoned"))?;
            ensure!(
                matches!(*state, MutationAdmission::Open),
                "This invocation has a pending terminal request. Finish the response; new mutations require a new turn."
            );
        }
        Ok(())
    }

    /// Record terminal intent and close mutation admission under one serialized boundary.
    pub(crate) fn seal_mutations<T>(&self, record: impl FnOnce() -> Result<T>) -> Result<T> {
        self.with_task_gate(|| {
            let admission = self
                .admission
                .as_ref()
                .context("invocation mutation admission is missing")?;
            let mut state = admission
                .lock()
                .map_err(|_| anyhow::anyhow!("mutation admission lock poisoned"))?;
            ensure!(
                matches!(*state, MutationAdmission::Open),
                "a terminal request is already pending for this invocation"
            );
            let result = record()?;
            *state = MutationAdmission::Sealed;
            Ok(result)
        })
    }

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
            admission: context
                .task_authority_progress
                .as_ref()
                .map(|progress| progress.admission.clone()),
            generation: context.task_generation,
            permission_snapshot: context.permission_snapshot.clone(),
            permission_required: context.permission_required,
        })
    }

    /// Adopt a review decision on a revision this invocation submitted, before admission.
    pub(crate) fn adopt_execution_review(&self) -> Result<()> {
        if let (Some(progress), Some(controller)) = (&self.progress, &self.controller) {
            progress.adopt_execution_review(controller)?;
        }
        Ok(())
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

    /// Adopt only the revision this invocation's own live owner commit produced.
    ///
    /// The session owner calls this in the same step as the commit, so no later owner step,
    /// such as sealing an acceptance of the committed revision, can observe the new task
    /// revision while this invocation still holds the one before it.
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
    use crate::task::TaskChange;

    /// Sealing rejects new mutations, including snapshots, without revoking settlement authority.
    #[test]
    fn terminal_seal_preserves_observation_and_rejects_new_mutation() -> Result<()> {
        let (_root, _store, controller) = SessionTaskController::test_goal();
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.bind_test_task_controller(controller);
        let authority = TaskAuthority::capture(&context).unwrap();
        let snapshot = authority.snapshot()?;
        authority.admit_mutation()?;
        let mut recorded = false;
        authority.seal_mutations(|| {
            recorded = true;
            Ok(())
        })?;
        assert!(recorded);
        assert!(authority.admit_mutation().is_err());
        assert!(snapshot.admit_mutation().is_err());
        authority.validate_scope()?;
        snapshot.validate_scope()?;
        assert!(authority.seal_mutations(|| Ok(())).is_err());
        Ok(())
    }

    /// Failed request recording leaves subsequent mutation admission available.
    #[test]
    fn rejected_terminal_record_does_not_seal_mutations() -> Result<()> {
        let (_root, _store, controller) = SessionTaskController::test_goal();
        let mut context =
            super::super::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        context.bind_test_task_controller(controller);
        let authority = TaskAuthority::capture(&context).unwrap();
        assert!(
            authority
                .seal_mutations::<()>(|| anyhow::bail!("invalid request"))
                .is_err()
        );
        authority.admit_mutation()?;
        Ok(())
    }

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
