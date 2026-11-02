//! Own manual and automatic compaction outside the broker lock.
use super::*;

/// Retain why compaction was requested and the automatic turn that authorized it.
#[derive(Clone, Debug)]
pub(crate) enum CompactionProvenance {
    Manual,
    Automatic { turn_id: String },
}

pub(crate) struct OwnedCompaction {
    provenance: CompactionProvenance,
    task: Option<crate::task::TaskPermit>,
    observer: Arc<crate::provider::session_execution::SessionObservationInbox>,
    compaction: crate::provider::Compaction,
    record: SessionEventRecord,
}

pub(crate) struct CompactionJob {
    provider: Arc<Provider>,
    request: crate::provider::ProviderCompactionRequest,
    state: OwnedCompaction,
}

pub(crate) struct CompactionResult {
    state: OwnedCompaction,
    output: Result<crate::provider::ProviderOutput>,
}

impl CompactionJob {
    pub(crate) async fn run(self) -> CompactionResult {
        use futures_util::FutureExt;
        let output = std::panic::AssertUnwindSafe(self.provider.compact(self.request))
            .catch_unwind()
            .await
            .unwrap_or_else(|_| Err(anyhow::anyhow!("provider compaction panicked")));
        CompactionResult {
            state: self.state,
            output,
        }
    }
}

impl HarnessBroker {
    /// Settle a recorded start when its publication fails before provider dispatch.
    pub(crate) fn abort_owned_compaction(
        &mut self,
        job: CompactionJob,
        error: anyhow::Error,
    ) -> Result<(
        PendingProviderDelivery,
        Option<anyhow::Error>,
        CompactionProvenance,
    )> {
        self.commit_owned_compaction(CompactionResult {
            state: job.state,
            output: Err(error),
        })
    }
    /// Select an automatic compaction only once for the latest native turn.
    pub(crate) fn prepare_automatic_compaction(
        &mut self,
    ) -> Result<Option<(CompactionJob, PendingProviderDelivery)>> {
        let policy = &self.session.auto_compaction;
        if !policy.enabled || !self.context_usage_fresh {
            return Ok(None);
        }
        anyhow::ensure!(
            self.capability.native_compact,
            "This provider does not support automatic compaction"
        );
        policy.validate(self.session.context_usage.as_ref())?;
        if self.store.list_session_event(&self.session.id)?.iter().any(|event| matches!(&event.detail,
            SessionEventKind::Compaction { compaction } if compaction.state == crate::provider::CompactionState::Running
                && Some(&compaction.thread_id) == self.session.provider_session_id.as_ref())) { return Ok(None); }
        if !policy.due(
            self.session.context_usage.as_ref(),
            self.capability.native_compact,
        ) {
            return Ok(None);
        }
        let latest = self.store.latest_exchange(&self.session.id)?;
        let turn_id = latest
            .as_ref()
            .and_then(|exchange| exchange.turn.last())
            .map(|turn| turn.id().to_owned());
        let native_compacted = latest.as_ref().is_some_and(|exchange| exchange.node_list.iter().any(|node|
            matches!(node, crate::exchange::ExchangeNode::Compaction { compaction, .. }
                if matches!(compaction.state, crate::provider::CompactionState::Completed | crate::provider::CompactionState::Running))));
        let Some(turn_id) = turn_id else {
            return Ok(None);
        };
        if self.session.last_compaction_turn.as_deref() == Some(turn_id.as_str()) {
            return Ok(None);
        }
        self.session.last_compaction_turn = Some(turn_id.clone());
        self.save_session()?;
        if native_compacted {
            return Ok(None);
        }
        self.prepare_owned_compaction(CompactionProvenance::Automatic { turn_id })
            .map(Some)
    }

    /// Capture a compaction start and its provider job under the session owner.
    pub(crate) fn prepare_owned_compaction(
        &mut self,
        provenance: CompactionProvenance,
    ) -> Result<(CompactionJob, PendingProviderDelivery)> {
        anyhow::ensure!(
            self.capability.native_compact,
            "current provider does not support context compaction"
        );
        let thread_id = self
            .session
            .provider_session_id
            .clone()
            .context("current session has no provider conversation to compact")?;
        let now = self.clock.now_ms();
        let compaction = crate::provider::Compaction {
            id: Uuid::new_v4().to_string(),
            thread_id,
            state: crate::provider::CompactionState::Running,
            manual: matches!(&provenance, CompactionProvenance::Manual),
            started_at_ms: Some(now),
            completed_at_ms: None,
            result: crate::provider::CompactionResult {
                context_tokens_before: self.session.context_usage.as_ref().map(|usage| usage.used),
                ..Default::default()
            },
        };
        let record = SessionEventRecord {
            id: format!("{}:compaction:{}", self.session.id, compaction.id),
            session_id: self.session.id.clone(),
            owner: crate::timeline::TimelineOwner::session(&self.session.id),
            created_at_ms: now,
            detail: SessionEventKind::Compaction {
                compaction: compaction.clone().into(),
            },
        };
        let request = crate::provider::ProviderCompactionRequest {
            id: compaction.id.clone(),
            request: self.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
        };
        self.store.save_session_event(&record)?;
        let task = self.task_controller.current()?.map(|task| task.permit());
        let mut state = OwnedCompaction {
            provenance,
            task,
            observer: self.observer.session_events(),
            compaction,
            record,
        };
        let delivery = (|| {
            let patch = self.reconcile_timeline()?;
            self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))
        })();
        let delivery =
            match delivery {
                Ok(delivery) => delivery,
                Err(error) => {
                    state.compaction.state = crate::provider::CompactionState::Failed;
                    state.compaction.completed_at_ms = Some(self.clock.now_ms());
                    state.compaction.result.error = Some(format!("{error:#}"));
                    state.record.detail = SessionEventKind::Compaction {
                        compaction: state.compaction.into(),
                    };
                    return match self.store.save_session_event(&state.record) {
                        Ok(()) => Err(error),
                        Err(persistence) => Err(error
                            .context(format!("record failed compaction start: {persistence:#}"))),
                    };
                }
            };
        Ok((
            CompactionJob {
                provider: self.provider.clone(),
                request,
                state,
            },
            delivery,
        ))
    }

    /// Save terminal evidence and apply metadata only while the owner remains current.
    pub(crate) fn commit_owned_compaction(
        &mut self,
        result: CompactionResult,
    ) -> Result<(
        PendingProviderDelivery,
        Option<anyhow::Error>,
        CompactionProvenance,
    )> {
        let CompactionResult { mut state, output } = result;
        state.compaction.completed_at_ms = Some(self.clock.now_ms());
        let output = match output {
            Ok(output) => output,
            Err(error) => {
                state.compaction.state = crate::provider::CompactionState::Failed;
                state.compaction.result.error = Some(format!("{error:#}"));
                state.record.detail = SessionEventKind::Compaction {
                    compaction: state.compaction.into(),
                };
                self.store.save_session_event(&state.record)?;
                let patch = self.reconcile_timeline()?;
                return Ok((
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                    Some(error),
                    state.provenance,
                ));
            }
        };
        let current_task = self.task_controller.current()?;
        let authority = match &state.task {
            Some(permit) => permit.validate(current_task.as_ref()),
            None if current_task.is_none() => Ok(()),
            None => Err(anyhow::anyhow!("task changed during compaction")),
        }
        .and_then(|()| {
            anyhow::ensure!(
                Arc::ptr_eq(&state.observer, &self.observer.session_events())
                    && self.session.provider_session_id.as_deref()
                        == Some(state.compaction.thread_id.as_str()),
                "provider generation changed during compaction"
            );
            if let CompactionProvenance::Automatic { turn_id } = &state.provenance {
                anyhow::ensure!(
                    self.session.last_compaction_turn.as_deref() == Some(turn_id.as_str()),
                    "automatic compaction turn changed"
                );
            }
            Ok(())
        });
        let observed = output.event.iter().rev().find_map(|event| match event {
            ProviderEvent::Compaction { update, .. } => Some(update.clone()),
            _ => None,
        });
        let completion = output.completion;
        let before = state.compaction.result.context_tokens_before;
        if let Some(observed) = observed {
            state.compaction.state = observed.state;
            state.compaction.result = observed.result;
        } else {
            state.compaction.state = crate::provider::CompactionState::Completed;
        }
        state.compaction.result.context_tokens_before = before;
        if state.compaction.state == crate::provider::CompactionState::Completed {
            state.compaction.result.context_tokens_after = completion
                .native
                .metrics
                .context_usage
                .as_ref()
                .map(|usage| usage.used);
        }
        let failed = state.compaction.state == crate::provider::CompactionState::Failed;
        let message = state
            .compaction
            .result
            .error
            .clone()
            .unwrap_or_else(|| "Provider compaction failed".into());
        state.record.detail = SessionEventKind::Compaction {
            compaction: state.compaction.into(),
        };
        self.store.save_session_event(&state.record)?;
        if let Err(error) = authority {
            let patch = self.reconcile_timeline()?;
            return Ok((
                self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                Some(error),
                state.provenance,
            ));
        }
        self.capability = completion.native.capability.clone();
        self.session.native_fork = completion.native.capability.native_fork;
        self.session.native_compact = completion.native.capability.native_compact;
        if let Some(id) = completion.native.provider_session_id {
            self.session.provider_session_id = Some(id.into());
        }
        if let Some(usage) = completion.native.metrics.context_usage {
            let size = self
                .session
                .context_usage
                .as_ref()
                .map_or(usage.size, |current| current.size);
            self.session.context_usage = ContextUsage::reported(usage.used, size);
        }
        if !completion.native.runtime.provider.is_empty() {
            self.session.provider_label = completion.native.runtime.provider;
        }
        if completion.native.runtime.model.is_some() {
            self.session.resolved_model = completion.native.runtime.model;
        }
        self.save_session()?;
        let snapshot = self.snapshot()?;
        let patch = self.reconcile_timeline()?;
        let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?;
        if failed {
            return Ok((
                delivery,
                Some(anyhow::anyhow!("{message}")),
                state.provenance,
            ));
        }
        delivery
            .events
            .push(self.event("context_compacted", serde_json::to_value(snapshot)?)?);
        Ok((delivery, None, state.provenance))
    }
}
