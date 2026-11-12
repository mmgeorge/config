//! Own conversation rollback's native rewind outside broker ownership.
//!
//! Rollback keeps the provider conversation and rewinds it natively to the checkpoint of the last
//! retained exchange. A provider without native rewind makes conversation rollback unsupported;
//! Harness never reconstructs a conversation by replaying retained history into a new prompt.
use super::*;

/// Retain one native rewind request built while the broker still owns the session.
#[derive(Clone)]
pub(crate) struct NativeRewind {
    pub(super) request: ProviderRequest,
    pub(super) after: Option<String>,
}

/// Retain the native rewind and the owner identity its commit must still match.
pub(crate) struct ConversationRewind {
    session_id: String,
    expected_latest: String,
    exchanges: Vec<String>,
    expected_task: Option<crate::task::Task>,
    provider: Arc<Provider>,
    inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    goal_controller: Arc<crate::task::SessionTaskController>,
    native: Option<NativeRewind>,
}

impl ConversationRewind {
    /// Rewind the provider conversation before host history records the rollback.
    ///
    /// The native request is idempotent, so a failed or uncertain attempt leaves the rollback
    /// uncommitted and a later attempt repeats the same rewind.
    pub(crate) async fn run(&mut self) -> Result<()> {
        anyhow::ensure!(
            foreground_failure::same_retraction_task(
                self.goal_controller.current()?.as_ref(),
                self.expected_task.as_ref()
            ),
            "task changed before conversation rewind"
        );
        if let Some(native) = &self.native {
            self.provider
                .thread_rewind(native.request.clone(), native.after.clone())
                .await
                .map_err(|error| {
                    anyhow::Error::new(error).context("provider conversation rewind failed")
                })?;
        }
        Ok(())
    }
}

impl HarnessBroker {
    /// Select the checkpoint that ends the retained native conversation.
    ///
    /// Retained exchanges without provider turns never reached native history and are skipped.
    /// A retained exchange with provider turns but no recorded checkpoint has no safe boundary,
    /// so rollback rejects it rather than discarding that exchange's native turns.
    pub(super) fn native_rewind(
        &self,
        exchanges: &[Exchange],
        first_removed: usize,
    ) -> Result<Option<NativeRewind>> {
        anyhow::ensure!(
            self.capability.native_conversation_rewind,
            "Conversation rollback is unsupported: {} cannot rewind its conversation",
            self.session.provider_label
        );
        if self.session.provider_session_id.is_none() {
            return Ok(None);
        }
        let mut after = None;
        for exchange in exchanges[..first_removed].iter().rev().filter(|exchange| {
            exchange.disposition() == crate::exchange::HistoryDisposition::Current
        }) {
            if let Some(checkpoint) = &exchange.provider_checkpoint_id {
                after = Some(checkpoint.clone());
                break;
            }
            anyhow::ensure!(
                exchange.turn.is_empty(),
                "Conversation rollback is unsupported here: interaction {} has no recorded provider checkpoint",
                exchange.ordinal
            );
        }
        Ok(Some(NativeRewind {
            request: self.provider_request(ProviderInput::from_user_text(""), PromptMode::Chat)?,
            after,
        }))
    }

    /// Validate the selection and capture its native rewind before any history or file change.
    pub(super) fn prepare_conversation_rewind(
        &mut self,
        action: &Value,
        recovering: bool,
    ) -> Result<ConversationRewind> {
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
        let native = self.native_rewind(&exchanges, position)?;
        Ok(ConversationRewind {
            session_id: self.session.id.clone(),
            expected_latest,
            exchanges: ids,
            expected_task: self.task_controller.current()?,
            provider: self.provider.clone(),
            inbox: self.observer.session_events(),
            goal_controller: self.task_controller.clone(),
            native,
        })
    }

    /// Commit rollback only if the rewound provider and conversation still own the session.
    pub(crate) fn commit_conversation_rewind(
        &mut self,
        rewind: ConversationRewind,
    ) -> Result<Vec<SessionEvent>> {
        anyhow::ensure!(
            self.session.id == rewind.session_id
                && Arc::ptr_eq(&self.provider, &rewind.provider)
                && Arc::ptr_eq(&self.observer.session_events(), &rewind.inbox)
                && foreground_failure::same_retraction_task(
                    self.task_controller.current()?.as_ref(),
                    rewind.expected_task.as_ref()
                ),
            "rollback owner changed during conversation rewind"
        );
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == rewind.expected_latest)
                && exchanges.iter().all(|exchange| exchange.is_finished()),
            "conversation changed during conversation rewind"
        );
        self.store
            .require_settled_exchange_tree(&self.session.id, &rewind.exchanges)?;
        let mut session = self.session.clone();
        session.active_task = None;
        self.apply_rewound_conversation(&mut session, rewind.native.as_ref())?;
        self.task_controller.clear_for_rollback(
            &mut self.store,
            rewind.expected_task.as_ref().map(|task| &task.revision),
            &session,
            &rewind.exchanges,
            self.clock.now_ms(),
        )?;
        self.session = session;
        self.context_usage_fresh = false;
        self.rolled_back_events(&rewind.exchanges[0])
    }

    /// Record that native history now ends at the rewind checkpoint.
    ///
    /// Context usage and the compaction marker describe the discarded history, so the next
    /// provider report replaces them.
    pub(super) fn apply_rewound_conversation(
        &self,
        session: &mut crate::session::HarnessSession,
        native: Option<&NativeRewind>,
    ) -> Result<()> {
        if let Some(native) = native {
            session.provider_checkpoint_id = native.after.clone();
        }
        session.context_usage = None;
        session.last_compaction_turn = None;
        session.acquire_lease(&self.client_id)?;
        session.updated_at_ms = self.clock.now_ms();
        Ok(())
    }
}
