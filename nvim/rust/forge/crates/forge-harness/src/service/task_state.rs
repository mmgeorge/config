use super::*;
use crate::task::TaskOperation;
use crate::task::command::{CommandResponse, SessionAction, SessionCommand};

pub(super) fn handles(_controller: &SessionController, request: &Request) -> Result<bool> {
    Ok(SessionCommand::parse_request(request)?.is_some())
}

pub(super) async fn apply(
    controller: &SessionController,
    session_id: &str,
    request: &Request,
    goal_max_turns: u32,
    sink: &MessageSender,
) -> Result<bool> {
    let Some(command) = SessionCommand::parse_request(request)? else {
        return Ok(false);
    };
    apply_command(
        controller,
        session_id,
        &command,
        Some(request.id),
        goal_max_turns,
        sink,
    )
    .await?;
    Ok(true)
}

pub(super) async fn apply_command(
    controller: &SessionController,
    session_id: &str,
    command: &SessionCommand,
    request_id: Option<u64>,
    goal_max_turns: u32,
    sink: &MessageSender,
) -> Result<()> {
    let current = controller.task_controller.current()?;
    let expected_revision = current.as_ref().map(|task| task.revision.clone());
    let now_ms = SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as i64;
    let normalized = command.action.to_params()?;
    let mut receipt = TaskOperation {
        revision: 0,
        id: command.operation_id.clone(),
        session_id: session_id.into(),
        action: normalized.clone(),
        permission_selection: None,
        state: "completed".into(),
        error: None,
        exchange_id: None,
        created_at_ms: now_ms,
        resume_after_transition: false,
    };
    let mut store = SqliteStore::open(&controller.data_root)?;
    if let Some(previous) = store.load_task_operation(session_id, &command.operation_id)? {
        ensure!(
            previous.action == normalized,
            "operation ID was reused for a different action"
        );
        let _ = publish_response(controller, command, request_id, previous.view(), sink).await?;
        return Ok(());
    }
    let observer = controller.observer.clone();
    let action = command.action.clone();
    let prepared = tokio::task::spawn_blocking(move || {
        observer.prepare_task_change(&action, current.as_ref(), goal_max_turns, now_ms)
    })
    .await
    .context("session command preparation worker failed")??;
    let plan = prepared.effects.plan.last().cloned();
    let lifecycle = prepared.effects.lifecycle.last().cloned();
    controller.task_controller.command_with_effects(
        &mut store,
        &receipt,
        expected_revision.as_ref(),
        prepared.change,
        prepared.effects,
        now_ms,
    )?;
    receipt = store
        .load_task_operation(session_id, &command.operation_id)?
        .context("session command receipt is missing")?;
    if command.action.admits_entry() || matches!(command.action, SessionAction::Pause) {
        let update = controller.coordinator.submissions.suspend(
            matches!(command.action, SessionAction::Pause),
            Some(&receipt.id),
        )?;
        controller.publish_input(update, sink).await?;
    }
    let active_elicitation =
        publish_response(controller, command, request_id, receipt.view(), sink).await?;
    if let Some(event) = command
        .action
        .event_name(lifecycle.as_ref().map(|record| record.kind))
    {
        let execution =
            controller
                .task_controller
                .current()?
                .and_then(|task| match task.workflow {
                    crate::task::TaskWorkflow::Execute { execution } => Some(execution.record),
                    _ => None,
                });
        let payload = if event == "plan_acceptance_started" {
            active_elicitation.unwrap_or(Value::Null)
        } else {
            json!({"plan":plan,"lifecycle":lifecycle,"execution":execution})
        };
        sink.send_event(SessionEvent {
            session_id: session_id.into(),
            event: event.into(),
            payload,
        })
        .await?;
    }
    sink.send_event(SessionEvent {
        session_id: session_id.into(),
        event: "task_operation".into(),
        payload: receipt.view(),
    })
    .await?;
    controller.coordinator.submissions.changed.notify_one();
    Ok(())
}

/// Reconcile committed task state before publishing its session notification and response.
async fn publish_response(
    controller: &SessionController,
    command: &SessionCommand,
    request_id: Option<u64>,
    receipt: Value,
    sink: &MessageSender,
) -> Result<Option<Value>> {
    let mut snapshot = controller.observer.snapshot()?;
    let patch = controller
        .presentation
        .lock()
        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
        .reconcile_with_status_thoughts(
            std::mem::take(&mut snapshot.timeline),
            controller.observer.status_thoughts.load(Ordering::Relaxed),
        )?;
    snapshot.timeline_revision = patch.revision;
    if !patch.is_empty() {
        sink.send_event(SessionEvent {
            session_id: snapshot.session.id.clone(),
            event: "document_changed".into(),
            payload: patch.notification(),
        })
        .await?;
    }
    let mut session = SqliteStore::read_only(&controller.data_root)?
        .load_session(controller.observer.session_id())?
        .context("session command owner is missing")?;
    session.active_task = controller.task_controller.current()?;
    sink.send_event(SessionEvent {
        session_id: session.id.clone(),
        event: "session_changed".into(),
        payload: serde_json::to_value(session)?,
    })
    .await?;
    let result = if command.response == CommandResponse::Snapshot {
        let mut snapshot = serde_json::to_value(snapshot)?;
        snapshot["task_operation"] = receipt;
        snapshot
    } else {
        receipt
    };
    let active_elicitation = result.get("active_elicitation").cloned();
    if let Some(request_id) = request_id {
        sink.send_control_response(Response::success(request_id, result)?)
            .await?;
    }
    Ok(active_elicitation)
}
