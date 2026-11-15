use super::services::OwnerServices;
use crate::input::{
    InputAction, InputDraft, InputIntent, InputMessage, InputOperation, InputPurpose,
};
use anyhow::{Result, ensure};
use forge_provider_api::forge::{TurnInput, TurnKind};
use tokio::time::Instant;

/// Recover and admit check completion messages through the durable input queue.
pub(super) struct CheckResults {
    requested: bool,
    retry_at: Option<Instant>,
}

impl Default for CheckResults {
    /// Reconcile unacknowledged completions when an owner starts.
    fn default() -> Self {
        Self {
            requested: true,
            retry_at: None,
        }
    }
}

impl CheckResults {
    /// Schedule reconciliation after actual process collection.
    pub(super) fn request(&mut self) {
        self.requested = true;
    }

    /// Admit each completed tool run once, retaining a retry on storage failure.
    pub(super) fn update(
        &mut self,
        services: &OwnerServices,
        runs: &crate::checks::owner::CheckOwnerRuns,
        messages: Option<&forge_protocol::outbound::MessageSender>,
    ) -> Result<()> {
        retire_obsolete(services, messages)?;
        if !self.requested
            || self
                .retry_at
                .is_some_and(|deadline| Instant::now() < deadline)
        {
            return Ok(());
        }
        let result = reconcile(services, runs, messages);
        match &result {
            Ok(()) => {
                self.requested = false;
                self.retry_at = None;
            }
            Err(_) => {
                self.retry_at = Some(Instant::now() + std::time::Duration::from_millis(250));
            }
        }
        result
    }

    /// Wake for pending work without polling from the provider.
    pub(super) async fn next(&self) {
        if !self.requested {
            std::future::pending::<()>().await;
        }
        if let Some(deadline) = self.retry_at {
            tokio::time::sleep_until(deadline).await;
        }
    }
}

/// Remove never-delivered results when their originating exchange has been cancelled or retired.
fn retire_obsolete(
    services: &OwnerServices,
    messages: Option<&forge_protocol::outbound::MessageSender>,
) -> Result<()> {
    let mut retired = Vec::new();
    for message in services.input.snapshot()?.messages {
        if matches!(message.purpose, InputPurpose::CheckResult { .. })
            && crate::input::task_input_retirable(&message)
            && !crate::input::input_purpose_is_current(
                &services.tasks,
                &services.questions,
                &message.purpose,
                &services.root,
                services.observer.session_id(),
            )?
        {
            retired.push((message.id, message.revision));
        }
    }
    if let Some(update) = services.input.retire_internal_input(&retired)? {
        services.publications.publish_once(
            super::publication::PublicationOrigin::OwnerUpdate(format!(
                "check-result-retired:{}",
                update.revision
            )),
            forge_protocol::message::SessionEvent {
                session_id: services.observer.session_id().into(),
                event: "input_changed".into(),
                payload: serde_json::to_value(update)?,
            },
            messages,
        )?;
    }
    Ok(())
}

/// Commit result input before acknowledging delivery; operation receipts survive removal and restart.
fn reconcile(
    services: &OwnerServices,
    runs: &crate::checks::owner::CheckOwnerRuns,
    messages: Option<&forge_protocol::outbound::MessageSender>,
) -> Result<()> {
    let history = services.checks.history()?;
    for (run_id, exchange_id) in runs.result_sources()? {
        let operation_id = format!("check-result:{run_id}");
        if let Some(receipt) = services
            .input
            .admission_receipt(&operation_id)?
            .filter(|receipt| receipt.error.is_none())
        {
            publish(services, messages, &operation_id, receipt)?;
            continue;
        }
        let Some(run) = history
            .iter()
            .find(|run| run.id == run_id && run.completed_at_ms.is_some())
        else {
            continue;
        };
        let source =
            crate::storage::HistoryReader::current(&services.root)?.load_exchange(&exchange_id)?;
        if !source.is_some_and(|source| {
            source.session_id == services.observer.session_id()
                && source.disposition() == crate::exchange::HistoryDisposition::Current
                && matches!(
                    source.state(),
                    crate::exchange::ExchangeState::Running
                        | crate::exchange::ExchangeState::Finalizing
                        | crate::exchange::ExchangeState::Complete
                )
        }) {
            continue;
        }
        let result = crate::checks::completion::result(run, &exchange_id);
        let turn = TurnInput::new(TurnKind::CheckResult { result });
        let snapshot = services.input.snapshot()?;
        let update = services.input.apply(InputOperation {
            id: operation_id.clone(),
            control_revision: snapshot.control_revision,
            after_control: snapshot.control_operation_id,
            action: InputAction::Submit {
                message: Box::new(
                    InputMessage::queued(InputDraft {
                        id: operation_id.clone(),
                        text: turn.label(),
                        purpose: InputPurpose::CheckResult {
                            exchange_id,
                            run_id,
                        },
                        attachments: Vec::new(),
                        config: None,
                        intent: InputIntent::Auto,
                    })
                    .with_turn(turn),
                ),
            },
        })?;
        ensure!(
            update.error.is_none(),
            "check result admission failed: {:?}",
            update.error
        );
        publish(services, messages, &operation_id, update)?;
    }
    Ok(())
}

/// Keep queue admission visible through the same durable publication path as question replies.
fn publish(
    services: &OwnerServices,
    messages: Option<&forge_protocol::outbound::MessageSender>,
    id: &str,
    update: crate::input::InputUpdate,
) -> Result<()> {
    services.publications.publish_once(
        super::publication::PublicationOrigin::CheckResult(super::command::InputId::new(id)?),
        forge_protocol::message::SessionEvent {
            session_id: services.observer.session_id().into(),
            event: "input_changed".into(),
            payload: serde_json::to_value(update)?,
        },
        messages,
    )?;
    Ok(())
}
