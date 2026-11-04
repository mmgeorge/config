//! Own manual and automatic compaction outside the broker lock.
use super::*;

/// Retain why compaction was requested and the automatic turn that authorized it.
#[derive(Clone, Debug)]
pub(crate) enum CompactionProvenance {
    Manual,
    Automatic { turn_id: String },
}

#[derive(Clone)]
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

#[derive(Clone)]
pub(crate) struct CompactionResult {
    state: OwnedCompaction,
    output: Result<crate::provider::Compaction, crate::provider::ProviderError>,
}

impl CompactionJob {
    /// Get the host-created attempt identity before moving this job into its retained worker.
    pub(crate) fn id(&self) -> &str {
        &self.state.compaction.id
    }

    pub(crate) async fn run(self) -> CompactionResult {
        use futures_util::FutureExt;
        let output = std::panic::AssertUnwindSafe(self.provider.compact(self.request))
            .catch_unwind()
            .await
            .unwrap_or_else(|_| {
                Err(crate::provider::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::CompactSession,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    ),
                ))
            });
        CompactionResult {
            state: self.state,
            output,
        }
    }
}

impl HarnessBroker {
    /// Mark a started attempt unconfirmed after losing its worker without inventing successful evidence.
    pub(crate) fn owner_compaction_observation_lost(
        &mut self,
        id: &crate::exchange::command::ContextCompactionId,
    ) -> Result<PendingProviderDelivery> {
        let mut record = self.store.list_session_event(&self.session.id)?.into_iter().find(|record| {
            matches!(&record.detail, SessionEventKind::Compaction { compaction } if compaction.id == id.as_str())
        }).context("retained compaction attempt is missing")?;
        let SessionEventKind::Compaction { compaction } = &mut record.detail else {
            unreachable!()
        };
        if compaction.state == crate::provider::CompactionState::Running {
            compaction.state = crate::provider::CompactionState::Unconfirmed;
            compaction.completed_at_ms = Some(self.clock.now_ms());
            compaction.result.diagnostic = Some(crate::provider::ProviderDiagnostic {
                kind: forge_provider_api::ProviderFailureKind::Transport,
                message: forge_provider_api::SanitizedMessage::new(
                    "compaction worker observation was lost".to_owned(),
                )
                .expect("host diagnostic is nonempty"),
                fatal: false,
            });
            self.store.save_session_event(&record)?;
        }
        let patch = self.reconcile_timeline()?;
        self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))
    }

    /// Validate native compaction prerequisites before interrupting current exchange work.
    pub(crate) fn owner_compaction_available(
        &self,
    ) -> Result<(), crate::exchange::command::ContextCompactionFailure> {
        if !self.capability.native_compact {
            return Err(crate::exchange::command::ContextCompactionFailure::Unsupported);
        }
        if self.session.provider_session_id.is_none() {
            return Err(crate::exchange::command::ContextCompactionFailure::NoConversation);
        }
        Ok(())
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
        self.trace.record(
            &self.session.id,
            "compaction.started",
            json!({
                "operation_id":compaction.id,
                "manual":compaction.manual,
                "background":self.session.background.is_some(),
                "context_tokens":compaction.result.context_tokens_before,
                "context_capacity":self.session.context_usage.as_ref().map(|usage| usage.size),
                "threshold_tokens":self.session.auto_compaction.threshold_tokens,
            }),
        );
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
                    state.compaction.result.diagnostic =
                        Some(crate::provider::lifecycle_error::diagnostic(
                            forge_provider_api::ProviderOperation::HostEffectPublication,
                            error.as_ref(),
                        ));
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
        let evidence = match output {
            Ok(evidence) => evidence,
            Err(error) => {
                state.compaction.state = crate::provider::CompactionState::Failed;
                state.compaction.result.diagnostic =
                    Some(crate::provider::lifecycle_error::diagnostic(
                        forge_provider_api::ProviderOperation::CompactSession,
                        &error,
                    ));
                state.record.detail = SessionEventKind::Compaction {
                    compaction: state.compaction.into(),
                };
                self.store.save_session_event(&state.record)?;
                let patch = self.reconcile_timeline()?;
                return Ok((
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                    Some(error.into()),
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
        let settlement = apply_compaction_evidence(&mut state.compaction, evidence);
        state.record.detail = SessionEventKind::Compaction {
            compaction: state.compaction.clone().into(),
        };
        self.store.save_session_event(&state.record)?;
        let failure = authority
            .err()
            .map(|error| error.context(CompactionEvidenceError::AuthorityChanged))
            .or_else(|| settlement.err().map(anyhow::Error::from));
        if let Some(error) = failure {
            let patch = self.reconcile_timeline()?;
            return Ok((
                self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                Some(error),
                state.provenance,
            ));
        }
        self.apply_compaction_context(&state.compaction);
        self.save_session()?;
        let snapshot = self.snapshot()?;
        let patch = self.reconcile_timeline()?;
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Effects(vec![
            AppliedProviderEvent::Timeline(patch),
            AppliedProviderEvent::Update(SessionUpdate::ContextCompacted {
                snapshot: Box::new(snapshot),
            }),
        ]))?;
        Ok((delivery, None, state.provenance))
    }
}

/// Distinguish unconfirmed or invalid evidence from a provider-reported compaction failure.
#[derive(Clone, Copy, Debug, thiserror::Error)]
pub(crate) enum CompactionEvidenceError {
    /// A completed fact is historical only after its task or binding authority changes.
    #[error("compaction task or binding authority changed")]
    AuthorityChanged,
    /// Evidence for another conversation cannot mutate this compaction or its session.
    #[error("compaction evidence belongs to a different conversation")]
    ConversationChanged,
    /// Returning from the native request does not establish completed context replacement.
    #[error("context compaction completion was not confirmed")]
    Unconfirmed,
    /// Preserve the normalized failure category without copying native error text.
    #[error("context compaction failed ({kind:?})")]
    Failed {
        kind: forge_provider_api::ProviderFailureKind,
    },
}

/// Settle only the identified conversation and preserve every normalized compaction result field.
pub(super) fn apply_compaction_evidence(
    target: &mut crate::provider::Compaction,
    evidence: crate::provider::Compaction,
) -> Result<(), CompactionEvidenceError> {
    let before = target.result.context_tokens_before;
    let failure = if target.thread_id != evidence.thread_id {
        target.state = crate::provider::CompactionState::Unconfirmed;
        Some(CompactionEvidenceError::ConversationChanged)
    } else {
        target.state = evidence.state;
        target.result = evidence.result;
        match target.state {
            crate::provider::CompactionState::Completed => None,
            crate::provider::CompactionState::Running
            | crate::provider::CompactionState::Unconfirmed => {
                target.state = crate::provider::CompactionState::Unconfirmed;
                Some(CompactionEvidenceError::Unconfirmed)
            }
            crate::provider::CompactionState::Failed => Some(CompactionEvidenceError::Failed {
                kind: target.result.diagnostic.as_ref().map_or(
                    forge_provider_api::ProviderFailureKind::Unknown,
                    |diagnostic| diagnostic.kind,
                ),
            }),
        }
    };
    target.result.context_tokens_before = before;
    if target.state == crate::provider::CompactionState::Completed {
        target.result.context_tokens_after = target.result.context_tokens_after.or_else(|| {
            target
                .result
                .context_window
                .as_ref()
                .and_then(|context| context.current_tokens)
        });
    }
    if let Some(error) = failure {
        let kind = match error {
            CompactionEvidenceError::AuthorityChanged
            | CompactionEvidenceError::ConversationChanged => {
                forge_provider_api::ProviderFailureKind::Protocol
            }
            CompactionEvidenceError::Unconfirmed => {
                forge_provider_api::ProviderFailureKind::Transport
            }
            CompactionEvidenceError::Failed { kind } => kind,
        };
        if target.result.diagnostic.is_none() {
            target.result.diagnostic = Some(crate::provider::ProviderDiagnostic {
                kind,
                message: forge_provider_api::SanitizedMessage::new(error.to_string())
                    .expect("host compaction diagnostics are nonempty"),
                fatal: false,
            });
        }
        return Err(error);
    }
    Ok(())
}

impl HarnessBroker {
    /// Apply reported total context usage without replacing unrelated capability or runtime state.
    pub(super) fn apply_compaction_context(&mut self, compaction: &crate::provider::Compaction) {
        if compaction.state != crate::provider::CompactionState::Completed {
            return;
        }
        let Some(used) = compaction.result.context_tokens_after else {
            return;
        };
        let size = compaction
            .result
            .context_window
            .as_ref()
            .and_then(|context| context.token_limit)
            .or_else(|| {
                self.session
                    .context_usage
                    .as_ref()
                    .map(|context| context.size)
            });
        if let Some(size) = size
            && let Some(context) = ContextUsage::reported(used, size)
        {
            self.session.context_usage = Some(context);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve capabilities and reported context, and never convert missing completion into success.
    #[tokio::test]
    async fn compaction_preserves_capabilities_and_rejects_unconfirmed_evidence() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.capability.native_compact = true;
        broker.session.native_compact = true;
        broker.session.provider_session_id = Some("context-conversation".into());
        broker.session.context_usage = ContextUsage::reported(900, 4000);
        let capability = serde_json::to_value(&broker.capability)?;
        let resolved_model = broker.session.resolved_model.clone();
        let (job, _started) = broker.prepare_owned_compaction(CompactionProvenance::Manual)?;
        let mut completed = job.state.compaction.clone();
        completed.state = crate::provider::CompactionState::Completed;
        completed.result.context_tokens_after = Some(100);
        let (_terminal, failure, _) = broker.commit_owned_compaction(CompactionResult {
            state: job.state,
            output: Ok(completed),
        })?;
        assert!(failure.is_none());
        assert_eq!(serde_json::to_value(&broker.capability)?, capability);
        assert!(broker.session.native_compact);
        assert_eq!(broker.session.resolved_model, resolved_model);
        assert_eq!(broker.session.context_usage.as_ref().unwrap().used, 100);
        let (job, _started) = broker.prepare_owned_compaction(CompactionProvenance::Manual)?;
        let mut unconfirmed = job.state.compaction.clone();
        unconfirmed.state = crate::provider::CompactionState::Unconfirmed;
        unconfirmed.result.context_tokens_after = Some(1);
        let (terminal, failure, _) = broker.commit_owned_compaction(CompactionResult {
            state: job.state,
            output: Ok(unconfirmed),
        })?;
        assert!(matches!(
            failure
                .as_ref()
                .and_then(|error| error.downcast_ref::<CompactionEvidenceError>()),
            Some(CompactionEvidenceError::Unconfirmed)
        ));
        assert!(
            !terminal
                .into_updates()
                .iter()
                .any(|update| matches!(update, SessionUpdate::ContextCompacted { .. }))
        );
        assert_eq!(broker.session.context_usage.as_ref().unwrap().used, 100);
        assert_eq!(serde_json::to_value(&broker.capability)?, capability);
        Ok(())
    }
}
