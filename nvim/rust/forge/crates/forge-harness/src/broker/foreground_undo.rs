//! Own conversation rollback teardown outside broker ownership.
use super::*;

/// Retain the old native generation and replacement until teardown has completed.
pub(crate) struct UndoTeardown {
    session_id: String,
    expected_latest: String,
    exchanges: Vec<String>,
    expected_task: Option<crate::task::Task>,
    provider: Arc<Provider>,
    replacement: Arc<Provider>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    goal_controller: Arc<crate::task::SessionTaskController>,
}

impl UndoTeardown {
    /// Collect the captured provider generation before restoring host history.
    pub(crate) async fn run(&mut self) -> Result<()> {
        anyhow::ensure!(
            foreground_failure::same_retraction_task(
                self.goal_controller.current()?.as_ref(),
                self.expected_task.as_ref()
            ),
            "task changed before undo provider teardown"
        );
        // The registry can serve other sessions; rollback retires only its captured binding.
        self.registry
            .retire(
                &crate::provider::ExecutionOwner::root(&self.session_id),
                crate::provider::session_execution::OwnerCloseReason::Cancelled,
            )
            .await?;
        self.inbox
            .close_collected(crate::provider::session_execution::OwnerCloseReason::Cancelled);
        self.inbox.retire_background().await?;
        Ok(())
    }
}

impl HarnessBroker {
    /// Capture teardown while allowing an already-authorized interrupted file restore.
    pub(super) fn prepare_rollback_teardown(
        &mut self,
        action: &Value,
        recovering: bool,
    ) -> Result<UndoTeardown> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before conversation rollback"
        );
        if !recovering {
            self.require_restore_recovered()?;
        }
        let rollback = action
            .get("rollback")
            .context("undo requires rollback selection")?;
        anyhow::ensure!(
            rollback["scope"] == "conversation",
            "undo requires conversation rollback scope"
        );
        let requested = required_text(rollback, "exchange_id")?;
        let target = self.store.checkpoint_owner(&self.session.id, &requested)?;
        let expected_latest = required_text(rollback, "latest_exchange_id")?;
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == expected_latest),
            "conversation changed; select the rollback interaction again"
        );
        let position = exchanges
            .iter()
            .position(|exchange| exchange.id == target)
            .context("rollback interaction disappeared")?;
        anyhow::ensure!(
            exchanges[position].disposition() == crate::exchange::HistoryDisposition::Current,
            "selected interaction has already been rolled back"
        );
        anyhow::ensure!(
            exchanges.iter().all(|exchange| exchange.is_finished()),
            "cancel or finish active work before conversation rollback"
        );
        let ids = exchanges[position..]
            .iter()
            .map(|exchange| exchange.id.clone())
            .collect::<Vec<_>>();
        self.store
            .require_settled_exchange_tree(&self.session.id, &ids)?;
        let expected_task = self.task_controller.current()?;
        let replacement = crate::provider::build(
            self.provider_launch.clone(),
            self.permission_coordinator.clone(),
            self.trace.clone(),
            self.data_root.clone(),
        )?;
        Ok(UndoTeardown {
            session_id: self.session.id.clone(),
            expected_latest,
            exchanges: ids,
            expected_task,
            provider: self.provider.clone(),
            replacement,
            registry: self.execution_registry(),
            inbox: self.observer.session_events(),
            goal_controller: self.task_controller.clone(),
        })
    }

    /// Publish a fresh observer and commit rollback only if the retired generation still owns it.
    pub(crate) fn commit_undo_teardown(
        &mut self,
        teardown: UndoTeardown,
    ) -> Result<Vec<SessionEvent>> {
        anyhow::ensure!(
            self.session.id == teardown.session_id
                && Arc::ptr_eq(&self.provider, &teardown.provider)
                && Arc::ptr_eq(&self.observer.session_events(), &teardown.inbox)
                && foreground_failure::same_retraction_task(
                    self.task_controller.current()?.as_ref(),
                    teardown.expected_task.as_ref()
                ),
            "rollback owner changed during provider teardown"
        );
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == teardown.expected_latest)
                && exchanges.iter().all(|exchange| exchange.is_finished()),
            "conversation changed during provider teardown"
        );
        self.store
            .require_settled_exchange_tree(&self.session.id, &teardown.exchanges)?;
        let mut session = self.session.clone();
        session.active_task = None;
        session.provider_session_id = None;
        session.provider_checkpoint_id = None;
        session.provider_fork_state = ProviderForkState::Ready;
        session.context_usage = None;
        session.last_compaction_turn = None;
        session.acquire_lease(&self.client_id)?;
        session.updated_at_ms = self.clock.now_ms();
        self.task_controller.clear_for_rollback(
            &mut self.store,
            teardown.expected_task.as_ref().map(|task| &task.revision),
            &session,
            &teardown.exchanges,
            self.clock.now_ms(),
        )?;
        self.session = session;
        self.provider = teardown.replacement;
        self.clear_effective_capabilities()?;
        self.context_usage_fresh = false;
        self.observer
            .replace_session_events(Arc::new(Default::default()));
        let exchange = self
            .store
            .load_exchange(&teardown.exchanges[0])?
            .filter(|exchange| exchange.session_id == self.session.id)
            .context("rolled-back interaction disappeared")?;
        Ok(vec![self.event(
            "interaction_rolled_back",
            serde_json::to_value(exchange)?,
        )?])
    }
}
