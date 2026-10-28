use super::approval::{PreparedApproval, codex_response, permission_from_provider};
use super::json_rpc::{DynamicToolTraceContext, control_invocation, rejects_question_resolution};
use crate::backend::approval::{ApprovalResolution, PermissionCoordinator, protected_target};
use crate::backend::{BackendEventSink, BackendOutput, ExecutionOwner};
use crate::control_tools::{
    ControlToolExecutor, ControlToolInvocation, ControlTurnContext, apply_invocation,
    control_tool_failure_json,
};
use crate::plan::{PlanDocument, render_plan};
use crate::session::ApprovalMode;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use std::time::Instant;

/// Owns ordered tool state independently of the provider socket.
pub(super) struct RequestExecutor {
    workspace: String,
    approval_mode: ApprovalMode,
    planning_feedback: bool,
    plan_document: Option<PlanDocument>,
    control_runtime: Option<Arc<ControlToolExecutor>>,
    replay_invocation: Option<ControlToolInvocation>,
    source_digests: Option<BTreeMap<String, String>>,
    execution_finished: bool,
    permission_coordinator: Arc<PermissionCoordinator>,
    event_sink: Option<BackendEventSink>,
    trace: Arc<TraceStore>,
    session_id: String,
    execution_owner: Option<ExecutionOwner>,
    approval: Option<Result<PreparedApproval>>,
}

impl RequestExecutor {
    /// Creates isolated tool state for one provider connection.
    pub(super) fn new(
        workspace: String,
        approval_mode: ApprovalMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<BackendEventSink>,
        trace: Arc<TraceStore>,
        session_id: String,
    ) -> Self {
        Self {
            workspace,
            approval_mode,
            planning_feedback: false,
            plan_document: None,
            control_runtime: None,
            replay_invocation: None,
            source_digests: None,
            execution_finished: false,
            permission_coordinator,
            event_sink,
            trace,
            session_id,
            execution_owner: None,
            approval: None,
        }
    }

    /// Associates permission callbacks with the execution that owns this connection.
    pub(super) fn set_owner(&mut self, owner: Option<ExecutionOwner>) {
        self.execution_owner = owner;
    }

    pub(super) fn set_approval(&mut self, approval: Result<PreparedApproval>) {
        self.approval = Some(approval);
    }

    /// Resets turn-local tool state before a settled connection is reused.
    pub(super) fn reset(
        &mut self,
        workspace: String,
        mode: ApprovalMode,
        sink: Option<BackendEventSink>,
    ) {
        self.workspace = workspace;
        self.approval_mode = mode;
        self.event_sink = sink;
        self.control_runtime = None;
        self.plan_document = None;
        self.planning_feedback = false;
        self.approval = None;
    }

    /// Installs the canonical plan and tool services for the next turn.
    pub(super) fn configure(&mut self, context: ControlTurnContext) -> Arc<ControlToolExecutor> {
        self.plan_document = context.plan_document.clone();
        self.planning_feedback = context.planning_feedback;
        let executor = Arc::new(ControlToolExecutor::new(context));
        self.control_runtime = Some(executor.clone());
        executor
    }

    /// Produces a response and replay effects without borrowing the transport.
    pub(super) async fn execute(
        &mut self,
        message: Value,
        received_at: Instant,
    ) -> ExecutedRequest {
        self.replay_invocation = None;
        self.source_digests = None;
        self.execution_finished = false;
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let trace_context = DynamicToolTraceContext::from_message(&message);
        if let Some(context) = &trace_context {
            self.trace.record(
                &self.session_id,
                "dynamic_tool_started",
                context.payload(json!({"phase":"provider_request"})),
            );
            self.trace.record(
                &self.session_id,
                "dynamic_tool_received",
                context
                    .payload(json!({"handler_queue_ms":received_at.elapsed().as_millis() as u64})),
            );
        }
        let result = self
            .provider_request_result(method, &message, trace_context.as_ref())
            .await;
        if let (Some(context), Err(error)) = (&trace_context, &result) {
            self.trace.record(&self.session_id, "dynamic_tool_failed", context.payload(json!({
                "phase":"provider_request", "error_category":"internal", "error":format!("{error:#}")
            })));
        }
        let response = match result {
            Ok(result) => json!({"jsonrpc":"2.0", "id":message["id"], "result":result}),
            Err(error) => {
                json!({"jsonrpc":"2.0", "id":message["id"], "error":{"code":-32001,"message":format!("{error:#}")}})
            }
        };
        ExecutedRequest {
            control_completion: None,
            message,
            response,
            trace_context,
            approval: self.approval.take().and_then(Result::ok),
            invocation: self.replay_invocation.take(),
            source_digests: self.source_digests.take(),
            has_runtime: self.control_runtime.is_some(),
            execution_finished: self.execution_finished,
        }
    }
    async fn provider_request_result(
        &mut self,
        method: &str,
        message: &Value,
        trace_context: Option<&DynamicToolTraceContext>,
    ) -> Result<Value> {
        match method {
            "item/commandExecution/requestApproval"
            | "item/fileChange/requestApproval"
            | "item/permissions/requestApproval"
            | "mcpServer/elicitation/request" => {
                if method == "mcpServer/elicitation/request"
                    && message
                        .pointer("/params/meta/codex_approval_kind")
                        .and_then(Value::as_str)
                        != Some("mcp_tool_call")
                {
                    return Ok(json!({ "action": "decline", "content": Value::Null }));
                }
                let resolution = self.authorize_provider_request(method, message).await?;
                Ok(codex_response(message, resolution))
            }
            "item/tool/call"
                if rejects_question_resolution(self.planning_feedback, method, message) =>
            {
                Ok(json!({
                    "contentItems": [{
                        "type": "inputText",
                        "text": "Harness already recorded and consumed this planning feedback. Continue with design_patch, question_ask, or plan_submit."
                    }],
                    "success": false
                }))
            }
            "item/tool/call" => {
                let params = message.get("params").unwrap_or(message);
                if let Some(invocation) = control_invocation(method, params)? {
                    let validation_started_at = Instant::now();
                    let mut decoded_output = BackendOutput::default();
                    if let Err(error) = apply_invocation(&invocation, &mut decoded_output) {
                        let failure_json = control_tool_failure_json(
                            &invocation,
                            &error,
                            self.plan_document.as_ref(),
                        );
                        if let Some(context) = trace_context {
                            self.trace.record(
                                &self.session_id,
                                "dynamic_tool_failed",
                                context.payload(json!({
                                    "phase": "argument_validation",
                                    "error_category": "schema",
                                    "error": format!("{error:#}"),
                                    "validation_ms": validation_started_at.elapsed().as_millis() as u64,
                                })),
                            );
                        }
                        return Ok(json!({
                            "contentItems": [{
                                "type": "inputText",
                                "text": failure_json
                            }],
                            "success": false
                        }));
                    }
                    if let Some(context) = trace_context {
                        self.trace.record(
                            &self.session_id,
                            "dynamic_tool_validated",
                            context.payload(json!({
                                "phase": "argument_validation",
                                "validation_ms": validation_started_at.elapsed().as_millis() as u64,
                            })),
                        );
                    }
                    let execution_started_at = Instant::now();
                    let execution_message = match self
                        .execute_control_invocation(&invocation, decoded_output)
                        .await
                    {
                        Ok(message) => message,
                        Err(error) => {
                            let failure_json = control_tool_failure_json(
                                &invocation,
                                &error,
                                self.plan_document.as_ref(),
                            );
                            if let Some(context) = trace_context {
                                self.trace.record(
                                    &self.session_id,
                                    "dynamic_tool_failed",
                                    context.payload(json!({
                                        "phase": "semantic_execution",
                                        "error_category": "semantic",
                                        "error": format!("{error:#}"),
                                        "execution_ms": execution_started_at.elapsed().as_millis() as u64,
                                    })),
                                );
                            }
                            return Ok(json!({
                                "contentItems": [{
                                    "type": "inputText",
                                    "text": failure_json
                                }],
                                "success": false
                            }));
                        }
                    };
                    if let Some(context) = trace_context {
                        self.trace.record(
                            &self.session_id,
                            "dynamic_tool_executed",
                            context.payload(json!({
                                "phase": "semantic_execution",
                                "execution_ms": execution_started_at.elapsed().as_millis() as u64,
                                "resulting_version": self.plan_document.as_ref().map(|document| document.version),
                            })),
                        );
                    }
                    let response_text = if let Some(message) = execution_message {
                        message
                    } else {
                        match invocation.name.as_str() {
                            "harness_plan_edit" => format!(
                                "Plan edit accepted. The active canonical version is {}. Submission validation has not run.",
                                self.plan_document
                                    .as_ref()
                                    .map(|document| document.version)
                                    .unwrap_or_default()
                            ),
                            "plan_read" => self
                                .plan_document
                                .as_ref()
                                .map(serde_json::to_string_pretty)
                                .transpose()?
                                .context("plan read lost its active canonical document")?,
                            "plan_submit" => format!(
                                "Plan version {} passed canonical submission validation.",
                                self.plan_document
                                    .as_ref()
                                    .map(|document| document.version)
                                    .unwrap_or_default()
                            ),
                            _ => "Recorded by Forge Harness".into(),
                        }
                    };
                    return Ok(json!({
                        "contentItems": [{ "type": "inputText", "text": response_text }],
                        "success": true
                    }));
                }
                Ok(json!({
                    "contentItems": [{ "type": "inputText", "text": "Unknown Harness dynamic tool" }],
                    "success": false
                }))
            }
            _ => Ok(Value::Null),
        }
    }

    async fn execute_control_invocation(
        &mut self,
        invocation: &ControlToolInvocation,
        mut output: BackendOutput,
    ) -> Result<Option<String>> {
        if let Some(runtime) = self.control_runtime.as_ref() {
            let outcome = runtime.execute(invocation.clone()).await?;
            self.plan_document = outcome.plan_document;
            self.source_digests = outcome.source_digests;
            self.execution_finished = outcome.execution_finished;
            let result = outcome.result?;
            self.replay_invocation = result.invocation;
            return Ok(Some(result.message));
        }
        match invocation.name.as_str() {
            name if crate::control_tools::is_agent_tool(name) => {
                anyhow::bail!("agent tools require an authenticated active root turn");
            }
            "plan_phase_done" => {
                anyhow::bail!("phase completion requires an active execution control context");
            }
            "harness_plan_edit" => {
                anyhow::bail!("obsolete plan edit tool. Use design_patch");
            }
            "design_patch" => {
                let request = output
                    .design_patch
                    .pop()
                    .context("design patch has no request")?;
                self.plan_document = Some(
                    self.plan_document
                        .as_ref()
                        .context("design patch has no active design")?
                        .patch_design(Path::new(&self.workspace), request)?,
                );
                return Ok(Some(format!(
                    "Declaration patch accepted. Active version: {}",
                    self.plan_document.as_ref().unwrap().version
                )));
            }
            "plan_read" => {
                let requested_plan_id = output
                    .plan_read
                    .as_deref()
                    .context("plan read did not produce a plan id")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan read has no active canonical document")?;
                anyhow::ensure!(
                    requested_plan_id == document.plan_id,
                    "requested plan id does not match the active plan"
                );
                if let Some(design) = &document.design {
                    let value = design.inspect(
                        Path::new(&self.workspace),
                        invocation.arguments.get("path").and_then(Value::as_str),
                        invocation
                            .arguments
                            .get("baseline")
                            .and_then(Value::as_bool)
                            .unwrap_or(false),
                    )?;
                    return Ok(Some(serde_json::json!({"plan_id":document.plan_id,"version":document.version,"declarations":value}).to_string()));
                }
            }
            "plan_submit" => {
                let submission = output
                    .plan_submit
                    .as_ref()
                    .context("plan submit did not produce a submission")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan submit has no active canonical document")?;
                anyhow::ensure!(
                    submission.plan_id == document.plan_id,
                    "submitted plan id does not match the active plan"
                );
                anyhow::ensure!(
                    submission.expected_version == document.version,
                    "submitted version {} does not match active version {}",
                    submission.expected_version,
                    document.version
                );
                document.validate_for_submission()?;
                let mut document = document.clone();
                if let Some(design) = &document.design {
                    document.design = Some(design.formatted()?);
                }
                render_plan(&document)?;
            }
            _ => {}
        }
        Ok(None)
    }

    async fn authorize_provider_request(
        &mut self,
        method: &str,
        message: &Value,
    ) -> Result<ApprovalResolution> {
        let prepared = self.approval.take().unwrap_or_else(|| {
            permission_from_provider(method, message, &self.workspace).map(PreparedApproval::direct)
        })?;
        prepared.validate()?;
        let request = prepared.request.clone();
        let protected = {
            let store = self.permission_coordinator.store();
            let store = store
                .read()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?;
            protected_target(&request, &store)
        };
        if protected {
            return Ok(ApprovalResolution::DenyOnce);
        }
        let resolution = self
            .permission_coordinator
            .authorize(
                self.approval_mode,
                request,
                Some(crate::backend::ProviderAddress {
                    owner: self.execution_owner.clone(),
                    thread_id: message
                        .pointer("/params/threadId")
                        .and_then(Value::as_str)
                        .context("approval request omitted its provider thread")?
                        .to_owned(),
                    turn_id: message
                        .pointer("/params/turnId")
                        .and_then(Value::as_str)
                        .context("approval request omitted its provider turn")?
                        .to_owned(),
                }),
                self.event_sink.as_ref(),
            )
            .await?;
        if matches!(
            resolution,
            ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad
        ) {
            prepared.validate()?;
        }
        self.trace.record(&self.session_id, "permission_decision", json!({
            "provider":"codex", "thread_id":message.pointer("/params/threadId"),
            "turn_id":message.pointer("/params/turnId"), "item_id":message.pointer("/params/itemId"),
            "target_count":prepared.request.target_list.len(), "resolution":resolution
        }));
        self.approval = Some(Ok(prepared));
        Ok(resolution)
    }
}

/// Carries a completed tool result back to the transport for ordered publication.
pub(super) struct ExecutedRequest {
    /// Releases the next canonical request after this result is published.
    pub control_completion: Option<tokio::sync::oneshot::Sender<()>>,
    /// Retains provider identity and arguments for event normalization.
    pub message: Value,
    /// Contains the JSON-RPC success or failure response.
    pub response: Value,
    /// Proposal snapshot revalidated immediately before approval publication.
    pub approval: Option<PreparedApproval>,
    /// Correlates execution and transport timing for a dynamic tool.
    pub trace_context: Option<DynamicToolTraceContext>,
    /// Replays accepted canonical state into the turn output after delivery.
    pub invocation: Option<ControlToolInvocation>,
    /// Binds replayed patches to the source version validated by the tool.
    pub source_digests: Option<BTreeMap<String, String>>,
    /// Distinguishes canonical execution from decoding-only callbacks.
    pub has_runtime: bool,
    /// Marks a phase completion that requires ending the native turn.
    pub execution_finished: bool,
}

impl ExecutedRequest {
    /// Rejects queued work when its predecessor failed before publication.
    pub(super) fn cancelled(message: Value) -> Self {
        Self {
            response: json!({"jsonrpc":"2.0", "id":message["id"],
                "error":{"code":-32001,"message":"Previous provider request did not complete"}}),
            message,
            control_completion: None,
            trace_context: None,
            approval: None,
            invocation: None,
            source_digests: None,
            has_runtime: false,
            execution_finished: false,
        }
    }
}
