use std::collections::BTreeMap;

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use forge_protocol::message::Request;

#[derive(Clone, Debug)]
pub(crate) struct SessionCommand {
    pub operation_id: String,
    pub action: SessionAction,
    pub response: CommandResponse,
    pub display_text: Option<String>,
    pub metadata: BTreeMap<String, Value>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CommandResponse {
    #[default]
    Receipt,
    Snapshot,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action")]
pub(crate) enum SessionAction {
    #[serde(rename = "goal")]
    NewGoal { text: String },
    #[serde(rename = "plan")]
    NewPlan { text: String },
    /// Change acceptance policy without replacing or interrupting the task.
    #[serde(rename = "plan_accept")]
    PlanAccept { enabled: bool },
    #[serde(rename = "execute")]
    Execute {
        #[serde(flatten)]
        selection: ExecuteSelection,
    },
    #[serde(rename = "attach_plan")]
    Attach { plan_id: String },
    #[serde(rename = "fork")]
    Fork { plan_id: String },
    #[serde(rename = "replan")]
    Replan { plan_id: String, revision: u32 },
    #[serde(rename = "pause")]
    Pause,
    #[serde(rename = "resume")]
    Resume {
        #[serde(flatten)]
        request: ResumeRequest,
    },
    #[serde(rename = "clear")]
    Clear,
    #[serde(rename = "continue")]
    ReserveContinuation {
        #[serde(
            default,
            alias = "completed_turn_id",
            alias = "source",
            skip_serializing_if = "Option::is_none"
        )]
        completed_turn: Option<crate::task::TaskTurnId>,
    },
    #[serde(rename = "begin_acceptance")]
    BeginAcceptance {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "cancel_acceptance")]
    CancelAcceptance {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "request_changes")]
    RequestChanges {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "cancel_plan")]
    CancelPlan {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "complete_acceptance")]
    CompleteAcceptance { plan_id: String, digest: String },
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct ExecuteSelection {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub saved_source_digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(default, alias = "executor", skip_serializing_if = "Option::is_none")]
    pub plan_executor: Option<crate::session::PlanExecutor>,
    #[serde(default, alias = "compact", skip_serializing_if = "Option::is_none")]
    pub plan_compact: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct PlanReviewRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub saved_source_digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub annotations: Vec<crate::plan::PlanAnnotationInput>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct ResumeRequest {
    #[serde(default, alias = "text", skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub continuation: Option<String>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}

impl SessionCommand {
    /// Promote a current full submission when the task enables automatic acceptance.
    pub(crate) fn approved_revision(task: &super::ActiveTask) -> Result<Option<Self>> {
        let super::TaskWorkflow::Plan { plan } = &task.workflow else {
            return Ok(None);
        };
        if !task.auto_accept_plan
            || plan.paused
            || plan.artifact.acceptance.is_some()
            || plan.artifact.state != crate::plan::PlanState::AwaitingReview
            || plan.artifact.submitted_version != Some(plan.artifact.document_version)
            || !task.entry_accepted()
        {
            return Ok(None);
        }
        let choice = task.plan_execution.as_ref();
        Ok(Some(Self {
            operation_id: format!(
                "review-revision:{}:{}:{}",
                plan.artifact.id, plan.artifact.user_revision, plan.artifact.model_revision
            ),
            action: SessionAction::Execute {
                selection: ExecuteSelection {
                    plan_id: Some(plan.artifact.id.clone()),
                    digest: Some(
                        plan.artifact
                            .review_digest
                            .clone()
                            .context("approved revision has no submitted digest")?,
                    ),
                    approval_mode: choice.map(|choice| choice.execution.approval_mode),
                    plan_executor: choice.map(|choice| choice.execution.executor.clone()),
                    plan_compact: choice.map(|choice| choice.execution.compact),
                    comment: choice.and_then(|choice| choice.comment.clone()),
                    ..Default::default()
                },
            },
            response: CommandResponse::Snapshot,
            display_text: None,
            metadata: BTreeMap::new(),
        }))
    }

    pub(crate) fn queued_input(message: &crate::input::InputMessage) -> Result<Option<Self>> {
        if message.delivery != crate::input::InputDelivery::Queued
            || message.reservation.is_some()
            || !matches!(message.purpose, crate::input::InputPurpose::User)
        {
            return Ok(None);
        }
        Self::parse_text(
            &message.text,
            Some(&format!(
                "input-command:{}:{}",
                message.id, message.revision
            )),
        )
    }

    pub(crate) fn parse_request(request: &Request) -> Result<Option<Self>> {
        if request.method == "session.turn.steer" {
            crate::protocol::validate_root_control(request.params.get("target"))?;
        }
        if request.method == "session.input.apply" {
            let operation: crate::input::InputOperation =
                serde_json::from_value(request.params.clone())?;
            return match operation.action {
                crate::input::InputAction::Submit { message }
                    if matches!(message.purpose, crate::input::InputPurpose::User) =>
                {
                    let id = operation_id(&request.params).unwrap_or(&operation.id);
                    let mut command = Self::parse_text(&message.text, Some(id))?;
                    if let Some(command) = &mut command {
                        preserve_command_metadata(command, &request.params);
                    }
                    Ok(command)
                }
                _ => Ok(None),
            };
        }
        if matches!(
            request.method.as_str(),
            "session.prompt.submit" | "session.turn.steer"
        ) {
            let Some(text) = request.params["text"].as_str() else {
                return Ok(None);
            };
            let mut command = Self::parse_text(text, operation_id(&request.params))?;
            if let Some(command) = &mut command {
                preserve_command_metadata(command, &request.params);
            }
            return Ok(command);
        }
        let action = match request.method.as_str() {
            "session.command.apply" => request.params["action"]
                .as_str()
                .context("task action is required")?,
            "session.goal.set" => "goal",
            "session.goal.pause" => "pause",
            "session.goal.resume" => "resume",
            "session.goal.clear" => "clear",
            "session.plan.accept" => "execute",
            "session.plan.acceptance.begin" => "begin_acceptance",
            "session.plan.acceptance.cancel" => "cancel_acceptance",
            "session.plan.changes.request" => "request_changes",
            "session.plan.cancel" => "cancel_plan",
            _ => return Ok(None),
        };
        if !matches!(
            action,
            "goal"
                | "plan"
                | "plan_accept"
                | "execute"
                | "attach_plan"
                | "fork"
                | "replan"
                | "pause"
                | "resume"
                | "clear"
                | "continue"
                | "begin_acceptance"
                | "cancel_acceptance"
                | "request_changes"
                | "cancel_plan"
                | "complete_acceptance"
        ) {
            return Ok(None);
        }
        let mut params = if request.params.is_null() {
            serde_json::Map::new()
        } else {
            request
                .params
                .as_object()
                .cloned()
                .context("session command parameters must be an object")?
        };
        let id = operation_id(&request.params)
            .map(str::to_owned)
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
        let display_text = params
            .remove("display_text")
            .and_then(|value| value.as_str().map(str::to_owned));
        let response = params
            .remove("_command_response")
            .map(serde_json::from_value)
            .transpose()?
            .unwrap_or(
                if request.method == "session.plan.accept"
                    || matches!(
                        action,
                        "begin_acceptance"
                            | "cancel_acceptance"
                            | "request_changes"
                            | "cancel_plan"
                            | "complete_acceptance"
                    )
                {
                    CommandResponse::Snapshot
                } else {
                    CommandResponse::Receipt
                },
            );
        params.remove("operation_id");
        params.insert("action".into(), json!(action));
        if action == "goal"
            && !params.contains_key("text")
            && let Some(objective) = params.remove("objective")
        {
            params.insert("text".into(), objective);
        }
        if action == "execute" && params.get("plan_id").and_then(Value::as_str) == Some("last") {
            params.remove("plan_id");
        }
        let metadata = params
            .remove("_command")
            .map(|value| BTreeMap::from([("_command".into(), value)]))
            .unwrap_or_default();
        let action: SessionAction = serde_json::from_value(Value::Object(params))?;
        action.validate()?;
        Ok(Some(Self {
            operation_id: id,
            action,
            response,
            display_text,
            metadata,
        }))
    }

    pub(crate) fn question_continue(
        request: &Request,
        prior: Option<&crate::task::TaskOperation>,
        target: Option<(String, String)>,
    ) -> Result<Option<Self>> {
        ensure!(
            request.method == "session.question.continue",
            "acceptance continuation requires question.continue"
        );
        if let Some(prior) = prior {
            ensure!(
                prior.action["action"] == "complete_acceptance",
                "operation ID was reused for a different action"
            );
            if let Some(id) = operation_id(&request.params) {
                ensure!(
                    id == prior.id,
                    "continuation receipt does not match operation identity"
                );
            }
            let mut params = prior.action.clone();
            params["operation_id"] = json!(prior.id);
            params["_command_response"] = json!(CommandResponse::Snapshot);
            if let Some(identity) = request.params.get("_command") {
                params["_command"] = identity.clone();
            }
            return Self::parse_request(&Request {
                id: request.id,
                method: "session.command.apply".into(),
                params,
            });
        }
        let Some((plan_id, digest)) = target else {
            return Ok(None);
        };
        let action = SessionAction::CompleteAcceptance { plan_id, digest };
        action.validate()?;
        let mut command = Self {
            operation_id: operation_id(&request.params)
                .map(str::to_owned)
                .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
            action,
            response: CommandResponse::Snapshot,
            display_text: None,
            metadata: BTreeMap::new(),
        };
        preserve_command_metadata(&mut command, &request.params);
        Ok(Some(command))
    }

    pub(crate) fn parse_text(text: &str, operation_id: Option<&str>) -> Result<Option<Self>> {
        let trimmed = text.trim();
        ensure!(
            trimmed != "/replan",
            "select a plan and revision with the /replan picker"
        );
        let action = match trimmed {
            "/plan accept on" => SessionAction::PlanAccept { enabled: true },
            "/plan accept off" => SessionAction::PlanAccept { enabled: false },
            _ if trimmed == "/plan accept" || trimmed.starts_with("/plan accept ") => {
                anyhow::bail!("use /plan accept on or /plan accept off")
            }
            "/goal pause" => SessionAction::Pause,
            "/goal clear" | "/plan cancel" => SessionAction::Clear,
            "/goal resume" | "/plan retry" => SessionAction::Resume {
                request: ResumeRequest::default(),
            },
            "/goal continue" => SessionAction::ReserveContinuation {
                completed_turn: None,
            },
            "/execute" => SessionAction::Execute {
                selection: ExecuteSelection::default(),
            },
            _ if trimmed.starts_with("/goal resume ") => SessionAction::Resume {
                request: ResumeRequest {
                    message: Some(trimmed[13..].trim().to_owned()),
                    ..Default::default()
                },
            },
            _ if trimmed.starts_with("/goal ") => SessionAction::NewGoal {
                text: trimmed[6..].trim().to_owned(),
            },
            _ if trimmed.starts_with("/plan ") => SessionAction::NewPlan {
                text: trimmed[6..].trim().to_owned(),
            },
            _ if trimmed.starts_with("/execute ") => SessionAction::Execute {
                selection: ExecuteSelection {
                    plan_id: (trimmed[9..].trim() != "last")
                        .then(|| trimmed[9..].trim().to_owned()),
                    ..Default::default()
                },
            },
            _ if trimmed.starts_with("/replan ") => {
                let mut fields = trimmed[8..].split_whitespace();
                let plan_id = fields
                    .next()
                    .context("replan requires a plan identity and revision")?
                    .to_owned();
                let revision = fields
                    .next()
                    .context("replan requires a selected revision")?
                    .parse()
                    .context("invalid selected plan revision")?;
                ensure!(
                    fields.next().is_none(),
                    "replan requires exactly one plan identity and revision"
                );
                SessionAction::Replan { plan_id, revision }
            }
            _ => return Ok(None),
        };
        action.validate()?;
        Ok(Some(Self {
            operation_id: operation_id
                .filter(|id| !id.is_empty())
                .map(str::to_owned)
                .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
            action,
            response: CommandResponse::Receipt,
            display_text: Some(text.to_owned()),
            metadata: BTreeMap::new(),
        }))
    }

    pub(crate) fn to_request(&self, request_id: u64) -> Result<Request> {
        let mut params = self.action.to_params()?;
        let object = params
            .as_object_mut()
            .context("session command is not an object")?;
        object.extend(self.metadata.clone());
        object.insert("operation_id".into(), json!(self.operation_id));
        object.insert(
            "_command_response".into(),
            serde_json::to_value(self.response)?,
        );
        if let Some(display_text) = &self.display_text {
            object.insert("display_text".into(), json!(display_text));
        }
        Ok(Request {
            id: request_id,
            method: self.action.method().into(),
            params,
        })
    }
}

impl SessionAction {
    pub(crate) fn to_params(&self) -> Result<Value> {
        Ok(serde_json::to_value(self)?)
    }

    pub(crate) fn is_review(&self) -> bool {
        matches!(
            self,
            Self::BeginAcceptance { .. }
                | Self::CancelAcceptance { .. }
                | Self::RequestChanges { .. }
                | Self::CancelPlan { .. }
                | Self::CompleteAcceptance { .. }
        )
    }

    /// Report the committed review outcome rather than the requested acceptance action.
    pub(crate) fn event_name(
        &self,
        lifecycle: Option<crate::plan::PlanLifecycleKind>,
    ) -> Option<&'static str> {
        if lifecycle == Some(crate::plan::PlanLifecycleKind::ChangesRequested) {
            return Some("plan_changes_requested");
        }
        match self {
            Self::BeginAcceptance { .. } => Some("plan_acceptance_started"),
            Self::CancelAcceptance { .. } => Some("plan_acceptance_cancelled"),
            Self::RequestChanges { .. } => Some("plan_changes_requested"),
            Self::CancelPlan { .. } => Some("plan_cancelled"),
            Self::Execute { .. } | Self::CompleteAcceptance { .. } => Some("plan_accepted"),
            _ => None,
        }
    }

    pub(crate) fn admits_entry(&self) -> bool {
        matches!(
            self,
            Self::NewGoal { .. }
                | Self::NewPlan { .. }
                | Self::Execute { .. }
                | Self::Attach { .. }
                | Self::Fork { .. }
                | Self::Replan { .. }
                | Self::Resume { .. }
                | Self::RequestChanges { .. }
                | Self::BeginAcceptance { .. }
                | Self::CompleteAcceptance { .. }
        )
    }

    fn method(&self) -> &'static str {
        match self {
            Self::BeginAcceptance { .. } => "session.plan.acceptance.begin",
            Self::CancelAcceptance { .. } => "session.plan.acceptance.cancel",
            Self::RequestChanges { .. } => "session.plan.changes.request",
            Self::CancelPlan { .. } => "session.plan.cancel",
            _ => "session.command.apply",
        }
    }

    fn validate(&self) -> Result<()> {
        match self {
            Self::NewGoal { text } | Self::NewPlan { text } => {
                ensure!(!text.trim().is_empty(), "task objective is required")
            }
            Self::Attach { plan_id } | Self::Fork { plan_id } => {
                ensure!(!plan_id.trim().is_empty(), "plan identity is required")
            }
            Self::Replan { plan_id, revision } => ensure!(
                !plan_id.trim().is_empty() && *revision > 0,
                "replan requires a plan identity and positive selected revision"
            ),
            Self::CompleteAcceptance { plan_id, digest } => ensure!(
                !plan_id.trim().is_empty() && !digest.is_empty(),
                "acceptance completion requires the reviewed plan and digest"
            ),
            _ => {}
        }
        Ok(())
    }
}

fn operation_id(params: &Value) -> Option<&str> {
    params["operation_id"]
        .as_str()
        .or_else(|| params.pointer("/_command/id").and_then(Value::as_str))
        .filter(|id| !id.is_empty())
}

fn preserve_command_metadata(command: &mut SessionCommand, params: &Value) {
    if let Some(metadata) = params.get("_command") {
        command.metadata.insert("_command".into(), metadata.clone());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn task_aliases_and_slash_commands_have_identical_typed_payloads() -> Result<()> {
        for (method, params, text, canonical) in [
            (
                "session.goal.set",
                json!({"objective":"Finish the task"}),
                "/goal Finish the task",
                json!({"action":"goal","text":"Finish the task"}),
            ),
            (
                "session.goal.pause",
                json!({}),
                "/goal pause",
                json!({"action":"pause"}),
            ),
            (
                "session.goal.resume",
                json!({}),
                "/goal resume",
                json!({"action":"resume"}),
            ),
            (
                "session.goal.clear",
                json!({}),
                "/goal clear",
                json!({"action":"clear"}),
            ),
            (
                "session.command.apply",
                json!({"action":"continue"}),
                "/goal continue",
                json!({"action":"continue"}),
            ),
            (
                "session.command.apply",
                json!({"action":"plan","text":"Plan the work"}),
                "/plan Plan the work",
                json!({"action":"plan","text":"Plan the work"}),
            ),
            (
                "session.command.apply",
                json!({"action":"resume"}),
                "/plan retry",
                json!({"action":"resume"}),
            ),
            (
                "session.command.apply",
                json!({"action":"clear"}),
                "/plan cancel",
                json!({"action":"clear"}),
            ),
            (
                "session.command.apply",
                json!({"action":"replan","plan_id":"selected","revision":3}),
                "/replan selected 3",
                json!({"action":"replan","plan_id":"selected","revision":3}),
            ),
        ] {
            let command = SessionCommand::parse_request(&Request {
                id: 1,
                method: method.into(),
                params,
            })?
            .unwrap();
            let slash = SessionCommand::parse_text(text, Some("same-operation"))?.unwrap();
            assert_eq!(command.action.to_params()?, canonical);
            assert_eq!(slash.action.to_params()?, canonical);
        }
        Ok(())
    }

    #[test]
    fn execution_alias_preserves_review_and_execution_settings() -> Result<()> {
        let parameters = json!({
            "operation_id":"accept-once","plan_id":"reviewed","digest":"review-digest",
            "saved_source_digest":"saved-digest","approval_mode":"allow", "comment":"Proceed",
            "plan_executor":{"enabled":true,"model":"executor","effort":"low"},
            "plan_compact":true,"review":{"document":"review-view","sequence":9}
        });
        let alias = SessionCommand::parse_request(&Request {
            id: 1,
            method: "session.plan.accept".into(),
            params: parameters.clone(),
        })?
        .unwrap();
        let mut task_parameters = parameters;
        task_parameters["action"] = json!("execute");
        let canonical = SessionCommand::parse_request(&Request {
            id: 2,
            method: "session.command.apply".into(),
            params: task_parameters,
        })?
        .unwrap();
        assert_eq!(alias.operation_id, "accept-once");
        assert_eq!(alias.response, CommandResponse::Snapshot);
        assert_eq!(canonical.response, CommandResponse::Receipt);
        assert_eq!(alias.action.to_params()?, canonical.action.to_params()?);
        let request = alias.to_request(3)?;
        assert_eq!(
            SessionCommand::parse_request(&request)?.unwrap().response,
            CommandResponse::Snapshot
        );
        assert_eq!(request.params["saved_source_digest"], "saved-digest");
        assert_eq!(request.params["review"]["sequence"], 9);
        assert_eq!(request.params["plan_compact"], true);
        Ok(())
    }

    /// Keep automatic acceptance across submissions while honoring pause and explicit disable.
    #[test]
    fn approved_revision_requires_current_submission_and_preserves_execution_choices() -> Result<()>
    {
        let record: crate::plan::PlanRecord = serde_json::from_value(json!({
            "id":"plan","session_id":"session","request":"Review","title":"Review",
            "state":"revising","working_path":"","document_version":2,
            "model_revision":1,"user_revision":1,"created_at_ms":1,"updated_at_ms":1
        }))?;
        let mut task = super::super::ActiveTask::plan(record, 1);
        task.auto_accept_plan = true;
        task.plan_execution = Some(crate::task::PlanExecutionChoice {
            execution: super::super::ExecutionEntry {
                approval_mode: crate::session::ApprovalMode::Deny,
                executor: crate::session::PlanExecutor::default(),
                compact: true,
            },
            comment: Some("Keep public behavior.".into()),
        });
        task.request_entry("Apply feedback".into(), 1)?;
        task.entry.as_mut().unwrap().delivery = super::super::TaskInputDelivery::Accepted;
        assert!(SessionCommand::approved_revision(&task)?.is_none());
        let super::super::TaskWorkflow::Plan { plan } = &mut task.workflow else {
            unreachable!()
        };
        plan.artifact.state = crate::plan::PlanState::AwaitingReview;
        plan.artifact.model_revision = 2;
        plan.artifact.review_digest = Some("revised".into());
        assert!(SessionCommand::approved_revision(&task)?.is_none());
        let super::super::TaskWorkflow::Plan { plan } = &mut task.workflow else {
            unreachable!()
        };
        plan.artifact.submitted_version = Some(2);
        plan.paused = true;
        assert!(SessionCommand::approved_revision(&task)?.is_none());
        let super::super::TaskWorkflow::Plan { plan } = &mut task.workflow else {
            unreachable!()
        };
        plan.paused = false;
        let recovered = serde_json::from_value(serde_json::to_value(&task)?)?;
        let command = SessionCommand::approved_revision(&recovered)?.unwrap();
        assert_eq!(command.action.to_params()?["approval_mode"], "deny");
        assert_eq!(command.action.to_params()?["plan_compact"], true);
        assert_eq!(command.action.to_params()?["digest"], "revised");
        assert_eq!(
            command.action.to_params()?["comment"],
            "Keep public behavior."
        );
        assert_eq!(
            command.operation_id,
            SessionCommand::approved_revision(&task)?
                .unwrap()
                .operation_id
        );
        let super::super::TaskWorkflow::Plan { plan } = &mut task.workflow else {
            unreachable!()
        };
        plan.artifact.model_revision = 3;
        assert!(SessionCommand::approved_revision(&task)?.is_some());
        task.auto_accept_plan = false;
        assert!(SessionCommand::approved_revision(&task)?.is_none());
        task.auto_accept_plan = true;
        let super::super::TaskWorkflow::Plan { plan } = &mut task.workflow else {
            unreachable!()
        };
        crate::plan::state_machine::PlanStateMachine::apply(
            &mut plan.artifact,
            crate::plan::state_machine::PlanEvent::Cancelled,
            3,
        )?;
        assert!(SessionCommand::approved_revision(&task)?.is_none());
        Ok(())
    }

    /// Keep policy commands distinct from plan creation, including invalid arguments.
    #[test]
    fn plan_accept_commands_do_not_start_planning() -> Result<()> {
        for (text, enabled) in [("/plan accept on", true), ("/plan accept off", false)] {
            let command = SessionCommand::parse_text(text, None)?.unwrap();
            assert!(
                matches!(command.action, SessionAction::PlanAccept { enabled: value } if value == enabled)
            );
            assert!(!command.action.admits_entry());
            let request = command.to_request(1)?;
            assert_eq!(request.params["action"], "plan_accept");
            assert_eq!(request.params["enabled"], enabled);
            assert!(SessionCommand::parse_request(&request)?.is_some());
        }
        for text in ["/plan accept", "/plan accept yes", "/plan accept on extra"] {
            assert!(SessionCommand::parse_text(text, None).is_err());
        }
        Ok(())
    }

    #[test]
    fn command_normalization_preserves_receipt_identity_and_metadata() -> Result<()> {
        let request = Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({
                "text":"/goal Finish", "_command":{"id":"stable-command","source":"rpc"}
            }),
        };
        let command = SessionCommand::parse_request(&request)?.unwrap();
        let normalized = command.to_request(2)?;
        let repeated = SessionCommand::parse_request(&normalized)?.unwrap();
        assert_eq!(command.operation_id, "stable-command");
        assert_eq!(repeated.operation_id, command.operation_id);
        assert_eq!(normalized.params["_command"]["source"], "rpc");
        assert_eq!(normalized.params["display_text"], "/goal Finish");
        assert_eq!(command.action.to_params()?, repeated.action.to_params()?);
        assert!(SessionCommand::parse_text("ordinary discussion", None)?.is_none());
        assert!(SessionCommand::parse_text("/replan selected 0", None).is_err());
        Ok(())
    }

    #[test]
    fn raw_input_and_steering_parse_before_delivery() -> Result<()> {
        let input = Request {
            id: 1,
            method: "session.input.apply".into(),
            params: json!({
                "id":"input-command","action":{"kind":"submit","message":{
                    "id":"message","text":"/plan Draft design","intent":"auto","attachments":[],
                    "config":null,"reservation":null,"submission_id":null,"error":null
                }}
            }),
        };
        let command = SessionCommand::parse_request(&input)?.unwrap();
        assert_eq!(command.operation_id, "input-command");
        assert_eq!(
            command.action.to_params()?,
            json!({"action":"plan","text":"Draft design"})
        );
        let steering = SessionCommand::parse_request(&Request {
            id: 2,
            method: "session.turn.steer".into(),
            params: json!({"text":"/goal pause"}),
        })?
        .unwrap();
        assert!(matches!(steering.action, SessionAction::Pause));
        assert!(
            SessionCommand::parse_request(&Request {
                id: 3,
                method: "session.turn.steer".into(),
                params: json!({"text":"/goal pause","target":{"thread_id":"child"}}),
            })
            .is_err()
        );
        Ok(())
    }

    #[test]
    fn acceptance_replay_keeps_original_review_identity() -> Result<()> {
        let request = Request {
            id: 1,
            method: "session.question.continue".into(),
            params: json!({"operation_id":"complete-once"}),
        };
        let command = SessionCommand::question_continue(
            &request,
            None,
            Some(("original-plan".into(), "original-digest".into())),
        )?
        .unwrap();
        let prior = crate::task::TaskOperation {
            revision: 1,
            id: command.operation_id.clone(),
            session_id: "session".into(),
            action: command.action.to_params()?,
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
            resume_after_transition: false,
        };
        for target in [
            None,
            Some(("replacement-plan".into(), "replacement-digest".into())),
        ] {
            let replay =
                SessionCommand::question_continue(&request, Some(&prior), target)?.unwrap();
            assert_eq!(replay.operation_id, "complete-once");
            assert_eq!(replay.action.to_params()?, command.action.to_params()?);
            assert_eq!(replay.response, CommandResponse::Snapshot);
        }
        Ok(())
    }

    #[test]
    fn review_aliases_preserve_arguments_and_dedicated_review_methods() -> Result<()> {
        for (method, action) in [
            ("session.plan.acceptance.begin", "begin_acceptance"),
            ("session.plan.acceptance.cancel", "cancel_acceptance"),
            ("session.plan.changes.request", "request_changes"),
            ("session.plan.cancel", "cancel_plan"),
        ] {
            let payload = json!({"operation_id":"review-command","plan_id":"plan","digest":"review-digest",
                "saved_source_digest":"source-digest","comment":"Keep the existing behavior",
                "annotations":[{"start_line":2,"end_line":4,"body":"Clarify this part"}],
                "review":{"document":"review-document","sequence":7}});
            let alias = SessionCommand::parse_request(&Request {
                id: 1,
                method: method.into(),
                params: payload.clone(),
            })?
            .unwrap();
            let mut canonical = payload;
            canonical["action"] = json!(action);
            let canonical = SessionCommand::parse_request(&Request {
                id: 2,
                method: "session.command.apply".into(),
                params: canonical,
            })?
            .unwrap();
            assert!(alias.action.is_review());
            assert_eq!(alias.action.to_params()?, canonical.action.to_params()?);
            let normalized = alias.to_request(3)?;
            assert_eq!(normalized.method, method);
            assert_eq!(normalized.params["annotations"][0]["end_line"], 4);
            assert_eq!(normalized.params["review"]["sequence"], 7);
        }
        Ok(())
    }
}
