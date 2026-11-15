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
    // Recover immutable admissions before selecting new evidence.
    let history = services.checks.history()?;
    let mut delivered = std::collections::HashSet::new();
    let mut reported_failures = std::collections::HashSet::new();
    for receipt in services.input.check_result_receipts()? {
        for message in &receipt.upsert {
            if let Some(input) = message.turn() {
                for result in input.check_results() {
                    if result.complete {
                        delivered.insert(result.run_id.clone());
                    } else {
                        reported_failures.extend(
                            result
                                .checks
                                .iter()
                                .map(|check| (result.run_id.clone(), check.id.clone())),
                        );
                    }
                }
            }
        }
        if let Some(id) = receipt.operation_id.clone() {
            publish(services, messages, &id, receipt)?;
        }
    }
    // Group runs by the exchange that requested them, including unfinished workers.
    let mut groups = std::collections::BTreeMap::<String, Vec<&crate::checks::CheckRun>>::new();
    for (run_id, exchange_id) in runs.result_sources()? {
        if let Some(run) = history.iter().find(|run| run.id == run_id) {
            groups.entry(exchange_id).or_default().push(run);
        }
    }
    for (exchange_id, mut group) in groups {
        group.sort_by(|left, right| {
            (left.started_at_ms, &left.id).cmp(&(right.started_at_ms, &right.id))
        });
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
        let mut results =
            ready_results(&group, &exchange_id, &delivered, |id| runs.is_collected(id));
        for run in &group {
            if let Some(result) = partial_failure(run, &exchange_id, &reported_failures) {
                results.push(result);
            }
        }
        let mut results = results.into_iter();
        let Some(result) = results.next() else {
            continue;
        };
        let run_id = result.run_id.clone();
        let operation_id = if result.complete {
            format!("check-result:{run_id}")
        } else {
            format!(
                "check-result:{run_id}:failure:{}",
                crate::plan::digest(&serde_json::to_vec(&result)?)
            )
        };
        let mut turn = TurnInput::new(TurnKind::CheckResult { result });
        turn.push_check_results(results);
        // Commit the grouped input and its publication under one journal transaction.
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

/// Report newly collected failed commands while retaining the unfinished run for final delivery.
fn partial_failure(
    run: &crate::checks::CheckRun,
    exchange_id: &str,
    reported: &std::collections::HashSet<(String, String)>,
) -> Option<forge_provider_api::forge::CheckResult> {
    if run.completed_at_ms.is_some() {
        return None;
    }
    let mut partial = run.clone();
    partial.commands.retain(|command| {
        let id = command
            .context
            .as_ref()
            .map_or(command.id.as_str(), |context| {
                context.definition_id.as_str()
            });
        command.selected
            && command.completed_at_ms.is_some()
            && !reported.contains(&(run.id.clone(), id.to_owned()))
            && matches!(
                command.state,
                crate::checks::CheckState::Failed
                    | crate::checks::CheckState::Blocked
                    | crate::checks::CheckState::ConfigurationError
                    | crate::checks::CheckState::ExecutionError
                    | crate::checks::CheckState::Stale
            )
    });
    if partial.commands.is_empty() {
        return None;
    }
    partial.state = crate::checks::CheckState::Failed;
    Some(crate::checks::completion::result(&partial, exchange_id))
}

/// Hold successful runs until collection finishes for the group; release failures immediately.
fn ready_results(
    group: &[&crate::checks::CheckRun],
    exchange_id: &str,
    delivered: &std::collections::HashSet<String>,
    collected: impl Fn(&str) -> bool,
) -> Vec<forge_provider_api::forge::CheckResult> {
    let pending = group
        .iter()
        .any(|run| run.completed_at_ms.is_none() || !collected(&run.id));
    group
        .iter()
        .filter(|run| {
            !delivered.contains(&run.id) && run.completed_at_ms.is_some() && collected(&run.id)
        })
        .map(|run| crate::checks::completion::result(run, exchange_id))
        .filter(|result| {
            !pending
                || matches!(
                    result.state,
                    forge_provider_api::forge::CheckResultState::Failed
                        | forge_provider_api::forge::CheckResultState::Blocked
                )
        })
        .collect()
}

/// Keep queue admission visible through the same durable publication path as question replies.
fn publish(
    services: &OwnerServices,
    messages: Option<&forge_protocol::outbound::MessageSender>,
    id: &str,
    mut update: crate::input::InputUpdate,
) -> Result<()> {
    // Initial delivery and recovery must publish the same immutable receipt bytes.
    update.snapshot = None;
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A collected failure bypasses the success batch while unfinished commands retain ownership.
    #[test]
    fn failed_command_reports_once_before_run_completion() -> Result<()> {
        let run: crate::checks::CheckRun = serde_json::from_value(serde_json::json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":".","environment_digest":"fixture",
            "started_at_ms":1,"completed_at_ms":null,"state":"running",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":[{"id":"failed","command":"fixture","state":"failed","selected":true,
                "started_at_ms":1,"completed_at_ms":2,"exit_code":1,"error":"failure",
                "stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":""}]
        }))?;
        let mut reported = std::collections::HashSet::new();
        let result = partial_failure(&run, "exchange", &reported).unwrap();
        assert!(!result.complete);
        assert_eq!(result.checks.len(), 1);
        assert_eq!(result.checks[0].error.as_deref(), Some("failure"));
        let message = InputMessage::queued(InputDraft {
            id: "failure".into(),
            text: "failed".into(),
            purpose: InputPurpose::CheckResult {
                exchange_id: "exchange".into(),
                run_id: "run".into(),
            },
            attachments: Vec::new(),
            config: None,
            intent: InputIntent::Auto,
        })
        .with_turn(TurnInput::new(TurnKind::CheckResult { result }));
        assert!(message.reports_check_failure());
        reported.insert(("run".into(), "failed".into()));
        assert!(partial_failure(&run, "exchange", &reported).is_none());
        Ok(())
    }

    /// Model overlapping check runs without starting check commands or advancing a real clock.
    #[test]
    fn successes_wait_for_collection_and_failures_are_immediate_once() -> Result<()> {
        let make_run = |id: &str,
                        state: &str,
                        completed: Option<i64>|
         -> Result<crate::checks::CheckRun> {
            Ok(serde_json::from_value(serde_json::json!({
                "id":id,"owner":{"kind":"session","session_id":"session"},"revision":0,
                "shell":"system","executable":null,"workspace":".","environment_digest":"fixture",
                "started_at_ms":1,"completed_at_ms":completed,"state":state,
                "progress":crate::plan::execution::SemanticProgress::default(),
                "workspace_identity":null,"workspace_digest":{},"commands":[]
            }))?)
        };
        let success = make_run("success", "passed", Some(2))?;
        let failure = make_run("failure", "failed", Some(2))?;
        let mut running = make_run("running", "running", None)?;
        let mut delivered = std::collections::HashSet::new();
        assert!(ready_results(&[&success, &running], "exchange", &delivered, |_| true).is_empty());
        assert!(ready_results(&[&success, &failure], "exchange", &delivered, |_| false).is_empty());
        let immediate = ready_results(
            &[&success, &failure, &running],
            "exchange",
            &delivered,
            |_| true,
        );
        assert_eq!(
            immediate
                .iter()
                .map(|result| result.run_id.as_str())
                .collect::<Vec<_>>(),
            ["failure"]
        );
        delivered.insert("failure".into());
        assert!(
            ready_results(
                &[&success, &failure, &running],
                "exchange",
                &delivered,
                |_| true
            )
            .is_empty()
        );
        running.state = crate::checks::CheckState::Passed;
        running.completed_at_ms = Some(3);
        let batch = ready_results(
            &[&success, &failure, &running],
            "exchange",
            &delivered,
            |_| true,
        );
        assert_eq!(
            batch
                .iter()
                .map(|result| result.run_id.as_str())
                .collect::<Vec<_>>(),
            ["success", "running"]
        );
        Ok(())
    }
}
