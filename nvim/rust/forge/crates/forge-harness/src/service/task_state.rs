use super::*;
use crate::task::command::SessionCommand;

pub(super) fn handles(_controller: &SessionController, request: &Request) -> Result<bool> {
    Ok(is_plan_edit_method(&request.method) || SessionCommand::parse_request(request)?.is_some())
}

pub(super) async fn apply(
    controller: &SessionController,
    session_id: &str,
    request: &Request,
    goal_max_turns: u32,
    sink: &MessageSender,
) -> Result<bool> {
    if is_plan_edit_method(&request.method) {
        apply_plan_edit(controller, request, sink).await?;
        return Ok(true);
    }
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
    _session_id: &str,
    command: &SessionCommand,
    request_id: Option<u64>,
    _goal_max_turns: u32,
    sink: &MessageSender,
) -> Result<()> {
    if matches!(
        command.action,
        crate::task::command::SessionAction::ReserveContinuation { .. }
    ) {
        return apply_legacy_continuation(controller, command, request_id, sink).await;
    }
    apply_owned_command(controller, command, request_id, sink).await
}

/// Normalize legacy task controls once, then await the owner's durable command result.
async fn apply_owned_command(
    controller: &SessionController,
    command: &SessionCommand,
    request_id: Option<u64>,
    sink: &MessageSender,
) -> Result<()> {
    use crate::exchange::command::{Command, CommandId, CommandRequest, TaskCommand};
    let id = CommandId::new(command.operation_id.clone())?;
    let result = async {
        use crate::exchange::command::{CommandFailure, ExecuteCommandError, TaskCommandFailure};
        let previous = controller.exchange_owner.command_status(id.clone()).await?;
        let current = if previous.is_none() {
            controller.task_controller.current().map_err(|_| {
                ExecuteCommandError::Failed(CommandFailure::Task(
                    TaskCommandFailure::StateUnavailable,
                ))
            })?
        } else {
            None
        };
        let source_plan = if previous.is_none() {
            match (
                &command.action,
                TaskCommand::legacy_plan_id(&command.action),
            ) {
                (crate::task::command::SessionAction::Execute { selection }, _) => {
                    let store = SqliteStore::read_only(&controller.data_root).map_err(|_| {
                        ExecuteCommandError::Failed(CommandFailure::Task(
                            TaskCommandFailure::StateUnavailable,
                        ))
                    })?;
                    match selection.plan_id.as_deref().filter(|id| *id != "last") {
                        Some(id) => store.load_plan(id),
                        None => store
                            .list_plan(controller.observer.session_id())
                            .map(|plans| {
                                plans.into_iter().rev().find(|plan| plan.model_revision > 0)
                            }),
                    }
                    .map_err(|_| {
                        ExecuteCommandError::Failed(CommandFailure::Task(
                            TaskCommandFailure::StateUnavailable,
                        ))
                    })?
                }

                (
                    crate::task::command::SessionAction::RequestChanges { review }
                    | crate::task::command::SessionAction::CancelPlan { review },
                    _,
                ) => {
                    let selected = review.plan_id.as_deref().or_else(|| {
                        current.as_ref().and_then(|task| match &task.workflow {
                            crate::task::WorkflowSnapshot::Plan { plan } => {
                                Some(plan.artifact.id.as_str())
                            }
                            crate::task::WorkflowSnapshot::Execute { execution } => {
                                Some(execution.record.plan_id.as_str())
                            }
                            crate::task::WorkflowSnapshot::Goal { .. } => None,
                        })
                    });
                    match selected {
                        Some(id) => SqliteStore::read_only(&controller.data_root)
                            .and_then(|store| store.load_plan(id))
                            .map_err(|_| {
                                ExecuteCommandError::Failed(CommandFailure::Task(
                                    TaskCommandFailure::StateUnavailable,
                                ))
                            })?,
                        None => None,
                    }
                }
                (_, Some(plan)) => SqliteStore::read_only(&controller.data_root)
                    .and_then(|store| store.load_plan(plan))
                    .map_err(|_| {
                        ExecuteCommandError::Failed(CommandFailure::Task(
                            TaskCommandFailure::StateUnavailable,
                        ))
                    })?,
                (_, None) => None,
            }
        } else {
            None
        };
        let intent = TaskCommand::from_session_action(
            &command.action,
            current.as_ref(),
            source_plan.as_ref(),
            previous.as_ref().map(|record| &record.request.command),
        )
        .map_err(|error| ExecuteCommandError::Failed(CommandFailure::Task(error)))?
        .ok_or(ExecuteCommandError::Failed(
            CommandFailure::PrerequisiteFailed,
        ))?;
        let owned_command = Command::Task(intent);
        controller
            .execute_command(
                CommandRequest {
                    id,
                    command: owned_command.clone(),
                    input_control: crate::exchange::command::InputControl::AtExecution,
                },
                sink,
            )
            .await
            .map(|result| (owned_command, result))
    }
    .await;
    let (owned_command, result) = match result {
        Ok(result) => result,
        Err(error) => {
            if let Some(request_id) = request_id {
                sink.send_control_response(Response::failure(
                    request_id,
                    error.code(),
                    error.to_string(),
                ))
                .await?;
                return Ok(());
            }
            return Err(error.into());
        }
    };
    if let Some(request_id) = request_id {
        let response = controller.command_response(&owned_command, &result)?;
        sink.send_control_response(Response::success(request_id, response)?)
            .await?;
    }
    Ok(())
}

/// Normalize explicit question continuation before the root service chooses execution admission.
pub(super) async fn apply_continuation(
    controller: &SessionController,
    request: &Request,
    sink: &MessageSender,
) -> Result<bool> {
    if request.method != "session.question.continue" {
        return Ok(false);
    }
    let mut intent: ContinuationIngress = serde_json::from_value(request.params.clone())?;
    if intent.operation_id.is_none() {
        intent.operation_id = request
            .params
            .pointer("/_command/id")
            .and_then(Value::as_str)
            .map(str::to_owned);
    }
    submit_continuation(controller, intent, Some(request.id), sink).await?;
    Ok(true)
}

/// Route legacy resume and continue aliases through one immutable continuation envelope.
async fn apply_legacy_continuation(
    controller: &SessionController,
    command: &SessionCommand,
    request_id: Option<u64>,
    sink: &MessageSender,
) -> Result<()> {
    let completed_turn = match &command.action {
        crate::task::command::SessionAction::ReserveContinuation { completed_turn } => {
            completed_turn.clone()
        }
        _ => unreachable!("continuation routing selects explicit continue"),
    };
    submit_continuation(
        controller,
        ContinuationIngress {
            operation_id: Some(command.operation_id.clone()),
            target: None,
            message: None,
            completed_turn,
        },
        request_id,
        sink,
    )
    .await
}

#[derive(serde::Deserialize)]
struct ContinuationIngress {
    operation_id: Option<String>,
    target: Option<ContinuationTargetIngress>,
    message: Option<String>,
    completed_turn: Option<crate::task::TaskTurnId>,
}

#[derive(serde::Deserialize)]
#[serde(tag = "owner", rename_all = "snake_case")]
enum ContinuationTargetIngress {
    #[serde(alias = "plan_acceptance")]
    PlanReview {
        id: String,
        set_id: String,
        revision: u32,
    },
    Plan {
        id: String,
        set_id: String,
        revision: u32,
    },
    Interaction {
        id: String,
        set_id: String,
        revision: u32,
    },
}

/// Capture once for fresh admission; retries compare caller intent before reading mutable sources.
async fn submit_continuation(
    controller: &SessionController,
    ingress: ContinuationIngress,
    request_id: Option<u64>,
    sink: &MessageSender,
) -> Result<()> {
    use crate::exchange::command::{Command, CommandId, CommandRequest, InputControl};
    let id = CommandId::new(
        ingress
            .operation_id
            .clone()
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
    )?;
    let previous = controller.exchange_owner.command_status(id.clone()).await?;
    let request = if let Some(previous) = previous {
        let Command::ContinueWorkflow(request) = previous.request.command else {
            anyhow::bail!("command identity was reused for a different intent");
        };
        ensure!(
            continuation_matches(&ingress, &request),
            "command identity was reused for a different continuation"
        );
        request
    } else {
        capture_continuation(controller, &ingress)?
    };
    let command = Command::ContinueWorkflow(request);
    let result = controller
        .execute_command(
            CommandRequest {
                id,
                command: command.clone(),
                input_control: InputControl::AtExecution,
            },
            sink,
        )
        .await;
    match result {
        Ok(result) => {
            if let Some(request_id) = request_id {
                sink.send_control_response(Response::success(
                    request_id,
                    controller.command_response(&command, &result)?,
                )?)
                .await?;
            }
        }
        Err(error) => {
            if let Some(request_id) = request_id {
                sink.send_control_response(Response::failure(
                    request_id,
                    error.code(),
                    error.to_string(),
                ))
                .await?;
            } else {
                return Err(error.into());
            }
        }
    }
    Ok(())
}

/// Compare only caller-supplied intent while retaining the original captured authority on retries.
fn continuation_matches(
    ingress: &ContinuationIngress,
    request: &crate::exchange::command::WorkflowContinuationRequest,
) -> bool {
    use crate::exchange::command::{QuestionWorkflow, WorkflowContinuationTarget};
    if ingress.message != request.message {
        return false;
    }
    if let Some(completed) = &ingress.completed_turn {
        let WorkflowContinuationTarget::Exchange(selection) = &request.target else {
            return false;
        };
        if selection.completed_turn() != completed {
            return false;
        }
    }
    match (&ingress.target, &request.target) {
        (None, WorkflowContinuationTarget::Plan(_) | WorkflowContinuationTarget::Exchange(_)) => {
            true
        }
        (Some(target), WorkflowContinuationTarget::Questions(selection)) => {
            let (id, set_id, revision, matches_owner) = match target {
                ContinuationTargetIngress::PlanReview {
                    id,
                    set_id,
                    revision,
                } => (
                    id,
                    set_id,
                    revision,
                    matches!(selection.workflow, QuestionWorkflow::PlanReview(_)),
                ),
                ContinuationTargetIngress::Plan {
                    id,
                    set_id,
                    revision,
                } => (
                    id,
                    set_id,
                    revision,
                    matches!(selection.workflow, QuestionWorkflow::Plan { .. }),
                ),
                ContinuationTargetIngress::Interaction {
                    id,
                    set_id,
                    revision,
                } => (
                    id,
                    set_id,
                    revision,
                    matches!(selection.workflow, QuestionWorkflow::Exchange(_)),
                ),
            };
            let source_id = match &selection.workflow {
                QuestionWorkflow::PlanReview(execution) => execution.source().id().as_str(),
                QuestionWorkflow::Plan { source, .. } => source.id().as_str(),
                QuestionWorkflow::Exchange(exchange) => exchange.id().as_str(),
            };
            matches_owner
                && id == source_id
                && set_id == selection.questions.id().as_str()
                && *revision == selection.questions.revision()
        }
        _ => false,
    }
}

/// Bind the current source once while preserving explicit plan, question, and turn constraints.
fn capture_continuation(
    controller: &SessionController,
    ingress: &ContinuationIngress,
) -> Result<crate::exchange::command::WorkflowContinuationRequest> {
    use crate::exchange::command::{
        ExchangeContinuationSelection, PlanExecutionSelection, PlanQuestionSetSelection,
        PlanSelection, QuestionContinuationSelection, QuestionWorkflow, TaskSelection,
        WorkflowContinuationRequest, WorkflowContinuationTarget,
    };
    ensure!(
        ingress
            .message
            .as_ref()
            .is_none_or(|message| !message.trim().is_empty()),
        "continuation message must be nonempty"
    );
    let snapshot = controller.observer.snapshot()?;
    let current = snapshot.session.active_task.as_ref();
    let store = SqliteStore::read_only(&controller.data_root)?;
    let target = if let Some(target) = &ingress.target {
        let (id, set_id, revision) = match target {
            ContinuationTargetIngress::PlanReview {
                id,
                set_id,
                revision,
            }
            | ContinuationTargetIngress::Plan {
                id,
                set_id,
                revision,
            }
            | ContinuationTargetIngress::Interaction {
                id,
                set_id,
                revision,
            } => (id, set_id, *revision),
        };
        let (questions, workflow) = match target {
            ContinuationTargetIngress::PlanReview { .. } => {
                let plan = store
                    .load_plan(id)?
                    .context("selected review plan is missing")?;
                ensure!(
                    plan.session_id == snapshot.session.id,
                    "plan belongs to another session"
                );
                let questions = PlanQuestionSetSelection::capture(
                    plan.review_questions()
                        .context("selected review questions are missing")?,
                )?;
                let execution = PlanExecutionSelection::capture(
                    &plan,
                    current,
                    crate::task::command::ExecuteSelection {
                        plan_id: Some(id.clone()),
                        ..Default::default()
                    },
                )?;
                (questions, QuestionWorkflow::PlanReview(Box::new(execution)))
            }
            ContinuationTargetIngress::Plan { .. } => {
                let plan = store.load_plan(id)?.context("selected plan is missing")?;
                let task = current.context("selected plan task is missing")?;
                ensure!(
                    matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan: selected } if selected.artifact.id == *id),
                    "selected plan task changed"
                );
                let questions = PlanQuestionSetSelection::capture(
                    plan.elicitation
                        .as_ref()
                        .context("selected planning questions are missing")?,
                )?;
                (
                    questions,
                    QuestionWorkflow::Plan {
                        source: PlanSelection::capture(&plan)?,
                        task: TaskSelection::capture(task),
                    },
                )
            }
            ContinuationTargetIngress::Interaction { .. } => {
                let exchange = snapshot
                    .exchange
                    .iter()
                    .find(|exchange| exchange.id == *id)
                    .context("selected question exchange is missing")?;
                let questions = PlanQuestionSetSelection::capture(
                    exchange
                        .elicitation()
                        .context("selected exchange questions are missing")?,
                )?;
                (
                    questions,
                    QuestionWorkflow::Exchange(ExchangeContinuationSelection::capture(
                        exchange, current,
                    )?),
                )
            }
        };
        ensure!(
            questions.id().as_str() == set_id && questions.revision() == revision,
            "selected question set changed"
        );
        WorkflowContinuationTarget::Questions(QuestionContinuationSelection {
            questions,
            workflow,
        })
    } else if let Some(crate::task::WorkflowSnapshot::Plan { plan }) =
        current.map(|task| &task.workflow)
        && matches!(
            plan.artifact.state,
            crate::plan::PlanState::AwaitingReview | crate::plan::PlanState::Accepted
        )
    {
        WorkflowContinuationTarget::Plan(PlanExecutionSelection::capture(
            &plan.artifact,
            current,
            Default::default(),
        )?)
    } else {
        let exchange = snapshot
            .exchange
            .last()
            .context("continuation source exchange is missing")?;
        let selection = ExchangeContinuationSelection::capture(exchange, current)?;
        ensure!(
            exchange.is_finished() || exchange.awaiting_input(),
            "continuation source has not settled"
        );
        ensure!(
            ingress
                .completed_turn
                .as_ref()
                .is_none_or(|turn| turn == selection.completed_turn()),
            "selected completed turn changed"
        );
        WorkflowContinuationTarget::Exchange(selection)
    };
    Ok(WorkflowContinuationRequest {
        target,
        message: ingress.message.clone(),
    })
}

/// Identify the direct document edits owned by the task command FIFO.
fn is_plan_edit_method(method: &str) -> bool {
    matches!(
        method,
        "session.plan.entity.rename" | "session.plan.test.delete" | "session.plan.check.update"
    )
}

/// Normalize a direct review edit and retain its selected source before command admission.
async fn apply_plan_edit(
    controller: &SessionController,
    request: &Request,
    sink: &MessageSender,
) -> Result<()> {
    apply_plan_edit_inner(controller, request, sink, None).await
}

/// Execute direct plan edits and optionally refresh the existing read-only review projection.
pub(super) async fn apply_plan_edit_review(
    controller: &SessionController,
    request: &Request,
    sink: &MessageSender,
    syntax: &Arc<SyntaxEngine>,
) -> Result<bool> {
    if !is_plan_edit_method(&request.method) {
        return Ok(false);
    }
    apply_plan_edit_inner(controller, request, sink, Some(syntax)).await?;
    Ok(true)
}

/// Preserve the edit's durable result independently of an optional presentation refresh.
async fn apply_plan_edit_inner(
    controller: &SessionController,
    request: &Request,
    sink: &MessageSender,
    syntax: Option<&Arc<SyntaxEngine>>,
) -> Result<()> {
    use crate::exchange::command::{
        Command, CommandId, CommandRequest, InputControl, PlanEdit, TaskCommand,
    };
    let intent = decode_plan_edit(request)?;
    let id = CommandId::new(
        request
            .params
            .get("operation_id")
            .or_else(|| request.params.pointer("/_command/id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
    )?;
    let previous = controller.exchange_owner.command_status(id.clone()).await?;
    let edit = if let Some(previous) = previous {
        let Command::Task(TaskCommand::EditPlan(edit)) = previous.request.command else {
            anyhow::bail!("command identity was reused for a different edit");
        };
        ensure!(
            edit.request() == &intent,
            "command identity was reused for a different edit"
        );
        edit
    } else {
        let plan = SqliteStore::read_only(&controller.data_root)?
            .load_plan(intent.plan.as_str())?
            .context("selected plan is missing")?;
        ensure!(
            plan.session_id == controller.observer.session_id(),
            "plan belongs to another session"
        );
        let current = controller.task_controller.current()?;
        PlanEdit::capture(&plan, current.as_ref(), intent)?
    };
    let command = Command::Task(TaskCommand::EditPlan(edit));
    match controller
        .execute_command(
            CommandRequest {
                id,
                command: command.clone(),
                input_control: InputControl::AtExecution,
            },
            sink,
        )
        .await
    {
        Ok(result) => {
            let mut response = controller.command_response(&command, &result)?;
            if let (Some(document), Some(syntax)) = (request.params.get("review_document"), syntax)
            {
                let document = serde_json::from_value(document.clone())?;
                match refresh_edited_review(controller, &result, &document, syntax).await {
                    Ok(review) => {
                        response["review"] = review;
                        response
                            .as_object_mut()
                            .unwrap()
                            .remove("review_reload_required");
                    }
                    Err(error) => {
                        response["review_error"] = json!(format!(
                            "Plan changed, but the open review could not update: {error:#}"
                        ))
                    }
                }
            }
            sink.send_control_response(Response::success(request.id, response)?)
                .await?;
        }
        Err(error) => {
            sink.send_control_response(Response::failure(
                request.id,
                error.code(),
                error.to_string(),
            ))
            .await?
        }
    }
    Ok(())
}

/// Parse the legacy RPC vocabulary only at its transport boundary.
fn decode_plan_edit(request: &Request) -> Result<crate::exchange::command::PlanEditRequest> {
    use crate::exchange::command::{PlanEditChange, PlanEditRequest, PlanEntitySelection, PlanId};
    #[derive(serde::Deserialize)]
    struct Fields {
        plan_id: String,
        expected_version: Option<u64>,
        digest: Option<String>,
        symbol: Option<String>,
        entity_name: Option<String>,
        name: Option<String>,
        #[serde(default)]
        tests: Vec<crate::plan::DesignTestSelection>,
        #[serde(default)]
        enabled: std::collections::BTreeMap<String, bool>,
    }
    let Fields {
        plan_id,
        expected_version,
        digest,
        symbol,
        entity_name,
        name,
        tests,
        enabled,
    } = serde_json::from_value(request.params.clone())?;
    let change = match request.method.as_str() {
        "session.plan.entity.rename" => PlanEditChange::RenameEntity {
            target: match symbol {
                Some(symbol) => PlanEntitySelection::Symbol(symbol),
                None => PlanEntitySelection::AddedEntity(
                    entity_name.context("entity_name is required")?,
                ),
            },
            name: name.context("new entity name is required")?,
        },
        "session.plan.test.delete" => PlanEditChange::DeleteTests { tests },
        "session.plan.check.update" => PlanEditChange::UpdateChecks { enabled, tests },
        _ => anyhow::bail!("unsupported plan edit method"),
    };
    Ok(PlanEditRequest {
        plan: PlanId::new(plan_id)?,
        expected_version,
        digest,
        change,
    })
}

/// Rehydrate an identified review retry from its original intent before reading or saving UI documents.
pub(super) async fn normalize_review_retry(
    controller: &SessionController,
    request: &mut Request,
) -> Result<bool> {
    use crate::exchange::command::{
        Command, CommandId, PlanEditChange, PlanEntitySelection, TaskCommand,
    };
    let Some(id) = request
        .params
        .get("operation_id")
        .or_else(|| request.params.pointer("/_command/id"))
        .and_then(Value::as_str)
    else {
        return Ok(false);
    };
    let Some(record) = controller
        .exchange_owner
        .command_status(CommandId::new(id)?)
        .await?
    else {
        return Ok(false);
    };
    let (method, original) = match &record.request.command {
        Command::Task(TaskCommand::ExecutePlan(selection)) => (
            "session.plan.accept",
            serde_json::to_value(selection.request())?,
        ),
        Command::Task(TaskCommand::RequestPlanChanges(selection)) => (
            "session.plan.changes.request",
            serde_json::to_value(selection.selection().request())?,
        ),
        Command::Task(TaskCommand::CancelPlan(selection)) => (
            "session.plan.cancel",
            serde_json::to_value(selection.selection().request())?,
        ),
        Command::Task(TaskCommand::EditPlan(edit)) => {
            let intent = edit.request();
            let mut original = json!({"plan_id":intent.plan,"expected_version":intent.expected_version,"digest":intent.digest});
            let method = match &intent.change {
                PlanEditChange::RenameEntity { target, name } => {
                    original["name"] = json!(name);
                    match target {
                        PlanEntitySelection::Symbol(symbol) => original["symbol"] = json!(symbol),
                        PlanEntitySelection::AddedEntity(name) => {
                            original["entity_name"] = json!(name)
                        }
                    }
                    "session.plan.entity.rename"
                }
                PlanEditChange::DeleteTests { tests } => {
                    original["tests"] = json!(tests);
                    "session.plan.test.delete"
                }
                PlanEditChange::UpdateChecks { enabled, tests } => {
                    original["enabled"] = json!(enabled);
                    original["tests"] = json!(tests);
                    "session.plan.check.update"
                }
            };
            (method, original)
        }
        _ => anyhow::bail!("command identity was reused for a different intent"),
    };
    ensure!(
        request.method == method,
        "command identity was reused for a different review action"
    );
    let params = request
        .params
        .as_object_mut()
        .context("review request requires an object")?;
    let presentation = params
        .remove("review")
        .map(serde_json::from_value::<forge_buffer::input::DocumentInput>)
        .transpose()?;
    params.remove("review_document");
    if let Some(presentation) = presentation {
        params.insert(
            "review_document".into(),
            serde_json::to_value(presentation.document)?,
        );
    }
    params.remove("draft_annotation");
    params.remove("draft_source_digest");
    for (key, value) in original
        .as_object()
        .context("stored review intent requires an object")?
    {
        params.entry(key.clone()).or_insert_with(|| value.clone());
    }
    Ok(true)
}

/// Rebuild only the retained edit revision; a newer plan cannot be reported as its completion.
async fn refresh_edited_review(
    controller: &SessionController,
    result: &crate::exchange::command::CommandResult,
    document: &forge_buffer::identity::DocumentId,
    syntax: &Arc<SyntaxEngine>,
) -> Result<Value> {
    let crate::exchange::command::CommandOutcome::Task(
        crate::exchange::command::TaskCommandOutcome::PlanEdited {
            source: selected,
            plan,
            ..
        },
    ) = &result.outcome
    else {
        anyhow::bail!("plan edit returned another command outcome");
    };
    let validate = || -> Result<()> {
        let current = SqliteStore::read_only(&controller.data_root)?
            .load_plan(&plan.id)?
            .context("edited plan is missing")?;
        selected.validate(&current)?;
        Ok(())
    };
    validate()?;
    let source = controller.observer.capture_plan_review(
        &plan.id,
        plan.review_digest
            .as_deref()
            .context("edited plan has no review digest")?,
    )?;
    let mut source = crate::plan::review_source::EditableReviewSource::try_from(source)?;
    let expected_version = controller
        .plan_review
        .prepare_update(document, &mut source)?;
    analyze_plan_source(&mut source, syntax, &validate, None).await?;
    validate()?;
    controller
        .plan_review
        .update(document, source, expected_version, None)
}
