use crate::agent::{AgentLifecycleEvent, AgentRunStatus};
use crate::backend::approval::{
    ApprovalResolution, PermissionCoordinator, codex_response, permission_from_provider,
    protected_target,
};
use crate::backend::{
    BackendEvent, BackendEventSink, BackendOutput, ProviderChangeKind, ProviderChangeSet,
    ProviderFileChange, ProviderTaskEntry, ProviderTaskUpdate, TaskStatus, ToolActivity,
    ToolActivityKind,
};
use crate::control_tools::{
    ControlToolInvocation, ControlToolRuntime, ControlTurnContext, apply_invocation,
};
use crate::plan::{PlanDocument, apply_plan_edit, render_plan};
use crate::session::{ContextUsage, ExecutionMode};
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use futures_util::{SinkExt, StreamExt};
use serde_json::{Value, json};
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::net::TcpStream;
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, connect_async};

const DYNAMIC_TOOL_RESPONSE_TIMEOUT: Duration = Duration::from_secs(30);

#[derive(Clone, Debug)]
struct DynamicToolTraceContext {
    invocation_id: String,
    request_id: Value,
    item_id: Option<String>,
    thread_id: Option<String>,
    turn_id: Option<String>,
    tool_name: String,
    argument_bytes: usize,
    mutation_count: usize,
    mutation_summary: Value,
    plan_id: Option<String>,
    expected_version: Option<u64>,
    started_at: Instant,
}

impl DynamicToolTraceContext {
    fn from_message(message: &Value) -> Option<Self> {
        let method = message.get("method").and_then(Value::as_str)?;
        let params = message.get("params").unwrap_or(message);
        let tool_name = control_tool_name(method, params)?.to_owned();
        let arguments = find_control_arguments(params, &tool_name).unwrap_or(Value::Null);
        let request_id = message.get("id").cloned().unwrap_or(Value::Null);
        let item_id = [
            "/params/item/id",
            "/params/itemId",
            "/params/item_id",
            "/params/callId",
            "/params/call_id",
        ]
        .into_iter()
        .find_map(|pointer| message.pointer(pointer).and_then(Value::as_str))
        .map(str::to_owned);
        let invocation_id = item_id.clone().unwrap_or_else(|| {
            let request_label = match &request_id {
                Value::String(value) => value.clone(),
                value => value.to_string(),
            };
            format!("rpc-{request_label}")
        });
        let mutation_summary = plan_mutation_summary(&arguments);
        let mutation_count = mutation_summary
            .as_object()
            .map(|resource_map| {
                resource_map
                    .values()
                    .filter_map(Value::as_object)
                    .flat_map(|action_map| action_map.values())
                    .filter_map(Value::as_u64)
                    .sum::<u64>() as usize
            })
            .unwrap_or_default();
        Some(Self {
            invocation_id,
            request_id,
            item_id,
            thread_id: message
                .pointer("/params/threadId")
                .or_else(|| message.pointer("/params/thread_id"))
                .and_then(Value::as_str)
                .map(str::to_owned),
            turn_id: message
                .pointer("/params/turnId")
                .or_else(|| message.pointer("/params/turn_id"))
                .and_then(Value::as_str)
                .map(str::to_owned),
            tool_name,
            argument_bytes: serde_json::to_vec(&arguments).map_or(0, |encoded| encoded.len()),
            mutation_count,
            mutation_summary,
            plan_id: arguments
                .get("plan_id")
                .and_then(Value::as_str)
                .map(str::to_owned),
            expected_version: arguments.get("expected_version").and_then(Value::as_u64),
            started_at: Instant::now(),
        })
    }

    fn payload(&self, fields: Value) -> Value {
        let mut payload = json!({
            "invocation_id": self.invocation_id,
            "request_id": self.request_id,
            "item_id": self.item_id,
            "thread_id": self.thread_id,
            "turn_id": self.turn_id,
            "tool_name": self.tool_name,
            "argument_bytes": self.argument_bytes,
            "mutation_count": self.mutation_count,
            "mutation_summary": self.mutation_summary,
            "plan_id": self.plan_id,
            "expected_version": self.expected_version,
            "elapsed_ms": self.started_at.elapsed().as_millis() as u64,
        });
        if let (Some(payload), Some(fields)) = (payload.as_object_mut(), fields.as_object()) {
            payload.extend(fields.clone());
        }
        payload
    }
}

fn plan_mutation_summary(arguments: &Value) -> Value {
    let mut resource_map = serde_json::Map::new();
    for resource_name in [
        "plan",
        "entity_changes",
        "dependencies",
        "flows",
        "tasks",
        "assumptions",
    ] {
        let Some(resource) = arguments.get(resource_name).and_then(Value::as_object) else {
            continue;
        };
        let mut action_map = serde_json::Map::new();
        for action_name in ["add", "modify", "remove"] {
            let count = resource
                .get(action_name)
                .and_then(Value::as_array)
                .map_or(0, Vec::len);
            if count > 0 {
                action_map.insert(action_name.into(), json!(count));
            }
        }
        if resource_name == "plan" {
            let count = resource
                .get("modify")
                .and_then(Value::as_object)
                .map_or(0, serde_json::Map::len);
            if count > 0 {
                action_map.insert("modify".into(), json!(count));
            }
        }
        if !action_map.is_empty() {
            resource_map.insert(resource_name.into(), Value::Object(action_map));
        }
    }
    Value::Object(resource_map)
}

/// Owns one session-scoped JSON-RPC connection to the shared Codex app-server.
pub struct CodexJsonRpc {
    socket: WebSocketStream<MaybeTlsStream<TcpStream>>,
    next_id: u64,
    workspace: String,
    execution_mode: ExecutionMode,
    planning_feedback: bool,
    plan_document: Option<PlanDocument>,
    control_runtime: Option<ControlToolRuntime>,
    control_invocation_key_set: HashSet<String>,
    permission_coordinator: Arc<PermissionCoordinator>,
    event_sink: Option<BackendEventSink>,
    trace: Arc<TraceStore>,
    session_id: String,
}

impl CodexJsonRpc {
    /// Connect one session channel to the shared provider process.
    pub async fn connect(
        endpoint: &str,
        workspace: &str,
        execution_mode: ExecutionMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<BackendEventSink>,
        trace: Arc<TraceStore>,
        session_id: String,
    ) -> Result<Self> {
        let (socket, _) = connect_async(endpoint)
            .await
            .with_context(|| format!("connect to shared Codex app-server at {endpoint}"))?;
        Ok(Self {
            socket,
            next_id: 1,
            workspace: workspace.to_owned(),
            execution_mode,
            planning_feedback: false,
            plan_document: None,
            control_runtime: None,
            control_invocation_key_set: HashSet::new(),
            permission_coordinator,
            event_sink,
            trace,
            session_id,
        })
    }

    /// Update request context and reset control-call identity before reusing this process.
    pub fn set_request_context(
        &mut self,
        workspace: &str,
        execution_mode: ExecutionMode,
        event_sink: Option<BackendEventSink>,
    ) {
        self.workspace = workspace.to_owned();
        self.execution_mode = execution_mode;
        self.event_sink = event_sink;
        self.control_invocation_key_set.clear();
        self.control_runtime = None;
    }

    /// Seed provider-visible control validation from one broker-owned turn snapshot.
    pub fn set_control_context(&mut self, context: ControlTurnContext) {
        self.plan_document = context.plan_document.clone();
        self.planning_feedback = context.planning_feedback;
        self.control_runtime = Some(ControlToolRuntime::new(context));
    }

    /// Send one JSON-RPC notification without allocating a response identifier.
    pub async fn notify(&mut self, method: &str, params: Value) -> Result<()> {
        self.write_message(&json!({ "jsonrpc": "2.0", "method": method, "params": params }))
            .await
    }

    /// Send one JSON-RPC request and collect normalized events until its response arrives.
    pub async fn request(
        &mut self,
        method: &str,
        params: Value,
        output: &mut BackendOutput,
    ) -> Result<Value> {
        let id = self.send_request(method, params).await?;
        loop {
            let message = self.read_message(output).await?;
            if let Some(result) = Self::request_result(&message, id, method) {
                return result;
            }
        }
    }

    /// Start one JSON-RPC request without taking ownership of the response loop.
    pub async fn send_request(&mut self, method: &str, params: Value) -> Result<u64> {
        let id = self.next_id;
        self.next_id += 1;
        self.write_message(
            &json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }),
        )
        .await?;
        Ok(id)
    }

    /// Read and normalize one provider message while preserving request routing metadata.
    pub async fn read_message(&mut self, output: &mut BackendOutput) -> Result<Value> {
        let encoded = loop {
            let frame = self
                .socket
                .next()
                .await
                .context("shared Codex app-server closed before completing the request")??;
            match frame {
                Message::Text(text) => break text.to_string(),
                Message::Binary(bytes) => {
                    break String::from_utf8(bytes.to_vec())
                        .context("decode shared Codex app-server binary frame")?;
                }
                Message::Ping(payload) => {
                    self.socket.send(Message::Pong(payload)).await?;
                }
                Message::Pong(_) => {}
                Message::Close(close) => {
                    anyhow::bail!("shared Codex app-server closed the session channel: {close:?}")
                }
                Message::Frame(_) => {}
            }
        };
        self.trace.record(
            &self.session_id,
            "model.received_raw",
            Value::String(encoded.clone()),
        );
        let message: Value = serde_json::from_str(&encoded)
            .with_context(|| format!("decode backend JSON-RPC message: {encoded}"))?;
        self.trace
            .record(&self.session_id, "model.received", message.clone());
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let rejects_question_resolution =
            rejects_question_resolution(self.planning_feedback, method, &message);
        let repeats_control_invocation = control_invocation_key(method, &message)
            .is_some_and(|key| !self.control_invocation_key_set.insert(key));
        if message.get("id").is_some() && message.get("method").is_some() {
            self.respond_to_provider_request(&message).await?;
        }
        let completed_control_lifecycle = repeats_control_invocation
            && control_tool_name(method, message.get("params").unwrap_or(&Value::Null)).is_some()
            && method.eq_ignore_ascii_case("item/completed");
        if !rejects_question_resolution
            && (!repeats_control_invocation || completed_control_lifecycle)
        {
            normalize_event_in_workspace(
                &message,
                output,
                self.event_sink.as_ref(),
                &self.workspace,
                !repeats_control_invocation,
            );
        }
        Ok(message)
    }

    /// Resolve one matching JSON-RPC response without consuming unrelated messages.
    pub fn request_result(message: &Value, id: u64, method: &str) -> Option<Result<Value>> {
        if message.get("id").and_then(Value::as_u64) != Some(id) || message.get("method").is_some()
        {
            return None;
        }
        if let Some(error) = message.get("error") {
            return Some(Err(anyhow::anyhow!("backend {method} failed: {error}")));
        }
        Some(Ok(message.get("result").cloned().unwrap_or(Value::Null)))
    }

    /// Collect provider events until a named terminal notification arrives.
    pub async fn read_until(
        &mut self,
        terminal_method: &str,
        output: &mut BackendOutput,
    ) -> Result<Value> {
        loop {
            let message = self.read_message(output).await?;
            if message.get("method").and_then(Value::as_str) == Some(terminal_method) {
                return Ok(message.get("params").cloned().unwrap_or(Value::Null));
            }
        }
    }

    async fn write_message(&mut self, message: &Value) -> Result<()> {
        let encoded = serde_json::to_string(message)?;
        self.write_encoded_message(message, encoded).await
    }

    async fn write_encoded_message(&mut self, message: &Value, encoded: String) -> Result<()> {
        self.trace
            .record(&self.session_id, "model.sent", message.clone());
        self.socket
            .send(Message::Text(encoded.into()))
            .await
            .context("write shared Codex app-server JSON-RPC request")
    }

    async fn respond_to_provider_request(&mut self, message: &Value) -> Result<()> {
        let id = message.get("id").cloned().unwrap_or(Value::Null);
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let trace_context = DynamicToolTraceContext::from_message(message);
        if let Some(context) = trace_context.as_ref() {
            self.trace.record(
                &self.session_id,
                "dynamic_tool_started",
                context.payload(json!({ "phase": "provider_request" })),
            );
            self.trace.record(
                &self.session_id,
                "dynamic_tool_received",
                context.payload(
                    json!({ "handler_queue_ms": context.started_at.elapsed().as_millis() as u64 }),
                ),
            );
        }
        let result = self
            .provider_request_result(method, message, trace_context.as_ref())
            .await;
        if let (Some(context), Err(error)) = (trace_context.as_ref(), &result) {
            self.trace.record(
                &self.session_id,
                "dynamic_tool_failed",
                context.payload(json!({
                    "phase": "provider_request",
                    "error_category": "internal",
                    "error": format!("{error:#}"),
                })),
            );
        }
        let response = match result {
            Ok(result) => json!({ "jsonrpc": "2.0", "id": id, "result": result }),
            Err(error) => json!({
                "jsonrpc": "2.0",
                "id": id,
                "error": { "code": -32001, "message": format!("{error:#}") }
            }),
        };
        let serialization_started_at = Instant::now();
        let encoded_response = match serde_json::to_string(&response) {
            Ok(encoded) => encoded,
            Err(error) => {
                if let Some(context) = trace_context.as_ref() {
                    self.trace.record(
                        &self.session_id,
                        "dynamic_tool_failed",
                        context.payload(json!({
                            "phase": "response_serialization",
                            "error_category": "serialization",
                            "error": format!("{error:#}"),
                            "response_serialization_ms": serialization_started_at.elapsed().as_millis() as u64,
                        })),
                    );
                }
                return Err(error.into());
            }
        };
        let response_serialization_ms = serialization_started_at.elapsed().as_millis() as u64;
        let response_bytes = encoded_response.len();
        let response_success = response
            .pointer("/result/success")
            .and_then(Value::as_bool)
            .unwrap_or_else(|| response.get("error").is_none());
        let response_started_at = Instant::now();
        let (write_result, response_timed_out) = match tokio::time::timeout(
            DYNAMIC_TOOL_RESPONSE_TIMEOUT,
            self.write_encoded_message(&response, encoded_response),
        )
        .await
        {
            Ok(result) => (result, false),
            Err(_) => (
                Err(anyhow::anyhow!(
                    "dynamic tool response exceeded the 30 second transport deadline"
                )),
                true,
            ),
        };
        if let Some(context) = trace_context.as_ref() {
            if response_timed_out {
                self.trace.record(
                    &self.session_id,
                    "dynamic_tool_timed_out",
                    context.payload(json!({
                        "phase": "socket_write",
                        "response_bytes": response_bytes,
                        "response_serialization_ms": response_serialization_ms,
                        "response_write_ms": response_started_at.elapsed().as_millis() as u64,
                        "timeout_ms": DYNAMIC_TOOL_RESPONSE_TIMEOUT.as_millis() as u64,
                    })),
                );
            } else {
                match &write_result {
                    Ok(()) => self.trace.record(
                        &self.session_id,
                        "dynamic_tool_responded",
                        context.payload(json!({
                            "phase": "socket_write",
                            "response_bytes": response_bytes,
                            "response_success": response_success,
                            "response_serialization_ms": response_serialization_ms,
                            "response_write_ms": response_started_at.elapsed().as_millis() as u64,
                        })),
                    ),
                    Err(error) => self.trace.record(
                        &self.session_id,
                        "dynamic_tool_failed",
                        context.payload(json!({
                            "phase": "socket_write",
                            "error_category": "transport",
                            "error": format!("{error:#}"),
                            "response_bytes": response_bytes,
                            "response_serialization_ms": response_serialization_ms,
                            "response_write_ms": response_started_at.elapsed().as_millis() as u64,
                        })),
                    ),
                }
            }
        }
        write_result
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
                        "text": "Harness already recorded and consumed this planning feedback. Continue with harness_plan_edit, harness_question_ask, or harness_plan_submit."
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
                                "text": format!(
                                    "{} received invalid arguments: {error:#}. Use the exact tool schema. Group mutations by resource, then use add, modify, or remove inside that resource. Nested resources follow the same shape. Never use JSON Patch op, path, or value fields.",
                                    invocation.name
                                )
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
                    if let Err(error) = self.execute_control_invocation(&invocation, decoded_output)
                    {
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
                                "text": format!(
                                    "{} failed canonical validation: {error:#}. The plan remains editable. Correct the referenced resource and retry harness_plan_submit with the current version.",
                                    invocation.name
                                )
                            }],
                            "success": false
                        }));
                    }
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
                    let response_text = match invocation.name.as_str() {
                        "harness_plan_edit" => format!(
                            "Plan edit accepted. The active canonical version is {}.",
                            self.plan_document
                                .as_ref()
                                .map(|document| document.version)
                                .unwrap_or_default()
                        ),
                        "harness_plan_read" => self
                            .plan_document
                            .as_ref()
                            .map(serde_json::to_string_pretty)
                            .transpose()?
                            .context("plan read lost its active canonical document")?,
                        "harness_plan_submit" => format!(
                            "Plan version {} passed canonical submission validation.",
                            self.plan_document
                                .as_ref()
                                .map(|document| document.version)
                                .unwrap_or_default()
                        ),
                        _ => "Recorded by DiffReview Harness".into(),
                    };
                    return Ok(json!({
                        "contentItems": [{ "type": "inputText", "text": response_text }],
                        "success": true
                    }));
                }
                Ok(json!({
                    "contentItems": [{ "type": "inputText", "text": "Recorded by DiffReview Harness" }],
                    "success": true
                }))
            }
            _ => Ok(Value::Null),
        }
    }

    fn execute_control_invocation(
        &mut self,
        invocation: &ControlToolInvocation,
        mut output: BackendOutput,
    ) -> Result<()> {
        if let Some(runtime) = self.control_runtime.as_mut() {
            let result = runtime.invoke_decoded(invocation.clone(), output)?;
            if result.invocation.is_none() {
                return Ok(());
            }
            self.plan_document = runtime.plan_document().cloned();
            return Ok(());
        }
        match invocation.name.as_str() {
            "harness_plan_edit" => {
                let request = output
                    .plan_edit
                    .pop()
                    .context("plan edit did not produce an edit request")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan edit has no active canonical document")?;
                self.plan_document = Some(apply_plan_edit(document, request)?.document);
            }
            "harness_plan_read" => {
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
            }
            "harness_plan_submit" => {
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
                render_plan(document)?;
            }
            _ => {}
        }
        Ok(())
    }

    async fn authorize_provider_request(
        &mut self,
        method: &str,
        message: &Value,
    ) -> Result<ApprovalResolution> {
        let request = permission_from_provider(method, message, &self.workspace);
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
            .authorize(self.execution_mode, request, self.event_sink.as_ref())
            .await?;
        Ok(resolution)
    }
}

fn rejects_question_resolution(planning_feedback: bool, method: &str, value: &Value) -> bool {
    let params = value.get("params").unwrap_or(value);
    planning_feedback
        && matches!(
            control_tool_name(method, params),
            Some("harness_question_answer" | "harness_question_withdraw")
        )
}

fn control_invocation_key(method: &str, value: &Value) -> Option<String> {
    let params = value.get("params").unwrap_or(value);
    let control_name = control_tool_name(method, params)?;
    let arguments = find_control_arguments(params, control_name)?;
    Some(format!("{control_name}:{arguments}"))
}

#[cfg(test)]
fn normalize_event(
    message: &Value,
    output: &mut BackendOutput,
    event_sink: Option<&BackendEventSink>,
) {
    normalize_event_in_workspace(message, output, event_sink, "", true);
}

fn normalize_event_in_workspace(
    message: &Value,
    output: &mut BackendOutput,
    event_sink: Option<&BackendEventSink>,
    workspace: &str,
    apply_control_invocation: bool,
) {
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let params = message.get("params").cloned().unwrap_or(Value::Null);
    let text = first_text(&params);
    let method_lower = method.to_ascii_lowercase();
    let encoded = params.to_string().to_ascii_lowercase();
    if method_lower == "turn/moderationmetadata" {
        return;
    }
    if matches!(method_lower.as_str(), "turn/started" | "turn/completed")
        && pointer_string(
            &params,
            &[
                "/threadId",
                "/thread_id",
                "/turn/threadId",
                "/turn/thread_id",
            ],
        )
        .is_some()
    {
        let event = BackendEvent {
            kind: method_lower.replace('/', "_"),
            text: None,
            data: json!({ "method": method, "params": params }),
            activity: None,
            summary: None,
            task_update: None,
        };
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(event.clone());
        }
        append_output_event(output, event);
        return;
    }
    let agent_lifecycle_list = normalize_agent_lifecycle(method, &params);
    if !agent_lifecycle_list.is_empty() {
        for lifecycle in agent_lifecycle_list {
            let event = BackendEvent {
                kind: "agent_lifecycle".into(),
                text: None,
                data: serde_json::to_value(lifecycle).unwrap_or(Value::Null),
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(event.clone());
            }
            append_output_event(output, event);
        }
        return;
    }
    let usage_event = method_lower.contains("tokenusage");
    if let Some(native_compact) = available_compact_update(&params) {
        output.metrics.native_compact_update = Some(native_compact);
    }
    if let Some(token_count) = turn_token_count(&params) {
        output.metrics.token_count = Some(token_count);
    }
    if usage_event {
        if let Some(context_usage) = context_usage(&params) {
            output.metrics.context_usage = Some(context_usage.clone());
            let event = BackendEvent {
                kind: "context_usage".into(),
                text: None,
                data: serde_json::to_value(context_usage).unwrap_or(Value::Null),
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(event_sink) = event_sink {
                let _ = event_sink.send(event.clone());
            }
            append_output_event(output, event);
        }
        return;
    }
    let control_tool = control_tool_name(method, &params);
    let tool_event = is_tool_lifecycle_event(&method_lower, &encoded);
    let plan_event = method_lower.contains("plan");
    let reasoning_event = method_lower.contains("reason") || encoded.contains("\"thought\"");
    let assistant_event = method_lower.contains("agentmessage");
    let error_event = method_lower.contains("error") || encoded.contains("\"type\":\"error\"");
    let kind = if error_event {
        "error"
    } else if tool_event {
        output.evidence.tool_called = true;
        "tool"
    } else if plan_event {
        "plan"
    } else if reasoning_event {
        "reasoning"
    } else {
        "assistant_message"
    };

    if apply_control_invocation
        && tool_event
        && let Ok(Some(invocation)) = control_invocation(method, &params)
    {
        let _ = apply_invocation(&invocation, output);
    }
    if method_lower.contains("goal")
        && params
            .pointer("/goal/status")
            .or_else(|| params.get("status"))
            .and_then(Value::as_str)
            .is_some_and(|status| status.eq_ignore_ascii_case("complete"))
    {
        output.evidence.native_complete = true;
    }
    let user_message_item = params
        .pointer("/item/type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("userMessage"));
    if !user_message_item
        && (tool_event || plan_event || reasoning_event || assistant_event || error_event)
    {
        let activity = tool_event
            .then(|| normalize_tool_activity(method, &params, workspace))
            .flatten();
        if control_tool.is_some() && activity.is_none() {
            return;
        }
        if activity.as_ref().is_some_and(successful_file_change) {
            output.evidence.workspace_changed = true;
        }
        let task_update = plan_event
            .then(|| normalize_task_update(method, &params))
            .flatten();
        let event = BackendEvent {
            kind: kind.into(),
            text: activity.is_none().then_some(text).flatten(),
            data: json!({ "method": method, "params": params }),
            activity,
            summary: None,
            task_update,
        };
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(event.clone());
        }
        append_output_event(output, event);
    }
}

fn normalize_agent_status(status: &str) -> Option<AgentRunStatus> {
    match status.to_ascii_lowercase().as_str() {
        "pendinginit" => Some(AgentRunStatus::Starting),
        "running" => Some(AgentRunStatus::Running),
        "interrupted" | "cancelled" | "canceled" => Some(AgentRunStatus::Interrupted),
        "completed" | "complete" | "done" => Some(AgentRunStatus::Completed),
        "errored" | "failed" | "error" => Some(AgentRunStatus::Failed),
        "shutdown" | "closed" | "notfound" => Some(AgentRunStatus::Closed),
        "waiting" | "idle" => Some(AgentRunStatus::Waiting),
        _ => None,
    }
}

fn normalize_agent_lifecycle(method: &str, params: &Value) -> Vec<AgentLifecycleEvent> {
    let encoded = params.to_string().to_ascii_lowercase();
    let method_lower = method.to_ascii_lowercase();
    if !method_lower.contains("subagent")
        && !method_lower.contains("collabagent")
        && !encoded.contains("collabagenttoolcall")
        && !encoded.contains("subagentactivity")
    {
        return Vec::new();
    }
    let item = params.pointer("/item").unwrap_or(params);
    let operation = first_string(item, &["tool", "operation", "action", "type"])
        .unwrap_or_else(|| "agentActivity".into());
    let parent_thread_id = pointer_string(
        params,
        &[
            "/item/senderThreadId",
            "/item/sender_thread_id",
            "/threadId",
            "/thread_id",
            "/senderThreadId",
            "/sender_thread_id",
        ],
    );
    let turn_id = pointer_string(
        params,
        &["/turnId", "/turn_id", "/item/turnId", "/item/turn_id"],
    );
    let definition = first_string(item, &["agentType", "agent_type", "role"]);
    let nickname = first_string(item, &["agentNickname", "agent_nickname", "nickname"]);
    let task = first_string(item, &["prompt", "task", "input"]);
    if item
        .get("type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("subAgentActivity"))
    {
        let kind = first_string(item, &["kind"]).unwrap_or_default();
        let starts_child = kind.eq_ignore_ascii_case("started");
        let status = if starts_child {
            AgentRunStatus::Running
        } else if let Some(status) = normalize_agent_status(&kind) {
            status
        } else {
            return Vec::new();
        };
        let provider_thread_id = pointer_string(
            params,
            &[
                "/item/agentThreadId",
                "/item/agent_thread_id",
                "/agentThreadId",
            ],
        );
        return provider_thread_id
            .map(|provider_thread_id| {
                vec![AgentLifecycleEvent {
                    operation,
                    starts_child,
                    parent_thread_id,
                    provider_thread_id: Some(provider_thread_id),
                    turn_id,
                    definition,
                    nickname,
                    task,
                    status,
                }]
            })
            .unwrap_or_default();
    }

    let receiver_thread_id_list = item
        .get("receiverThreadIds")
        .or_else(|| item.get("receiver_thread_ids"))
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
        .map(str::to_owned)
        .collect::<Vec<_>>();
    let agent_state_map = item
        .get("agentsStates")
        .or_else(|| item.get("agents_states"))
        .and_then(Value::as_object);
    let mut lifecycle_list = Vec::new();
    for receiver_thread_id in receiver_thread_id_list {
        let status = agent_state_map
            .and_then(|state_map| state_map.get(&receiver_thread_id))
            .and_then(|state| first_string(state, &["status", "state"]))
            .and_then(|status| normalize_agent_status(&status))
            .or_else(|| {
                if operation.eq_ignore_ascii_case("spawnAgent") {
                    Some(AgentRunStatus::Running)
                } else if operation.eq_ignore_ascii_case("closeAgent")
                    && method_lower.ends_with("/completed")
                {
                    Some(AgentRunStatus::Closed)
                } else {
                    None
                }
            });
        let Some(status) = status else { continue };
        lifecycle_list.push(AgentLifecycleEvent {
            operation: operation.clone(),
            starts_child: operation.eq_ignore_ascii_case("spawnAgent"),
            parent_thread_id: parent_thread_id.clone(),
            provider_thread_id: Some(receiver_thread_id),
            turn_id: turn_id.clone(),
            definition: definition.clone(),
            nickname: nickname.clone(),
            task: task.clone(),
            status,
        });
    }
    if lifecycle_list.is_empty() && operation.eq_ignore_ascii_case("spawnAgent") {
        let status_text = first_string(item, &["status", "state"]).unwrap_or_default();
        lifecycle_list.push(AgentLifecycleEvent {
            operation,
            starts_child: true,
            parent_thread_id,
            provider_thread_id: None,
            turn_id,
            definition,
            nickname,
            task,
            status: if status_text.eq_ignore_ascii_case("inProgress") {
                AgentRunStatus::Starting
            } else {
                AgentRunStatus::Running
            },
        });
    }
    lifecycle_list
}

/// Resolve child-agent lifecycle state from one raw provider message.
pub(crate) fn agent_lifecycle_list(message: &Value) -> Vec<AgentLifecycleEvent> {
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let params = message.get("params").unwrap_or(&Value::Null);
    normalize_agent_lifecycle(method, params)
}

fn pointer_string(value: &Value, pointer_list: &[&str]) -> Option<String> {
    pointer_list
        .iter()
        .find_map(|pointer| value.pointer(pointer).and_then(Value::as_str))
        .map(str::to_owned)
}

fn first_string(value: &Value, field_list: &[&str]) -> Option<String> {
    field_list
        .iter()
        .find_map(|field| value.get(field).and_then(Value::as_str))
        .map(str::to_owned)
}

fn context_usage(params: &Value) -> Option<ContextUsage> {
    if let Some(token_usage) = params
        .get("tokenUsage")
        .or_else(|| params.get("token_usage"))
    {
        let used = token_usage
            .pointer("/last/totalTokens")
            .or_else(|| token_usage.pointer("/last/total_tokens"))
            .and_then(Value::as_u64)?;
        let size = token_usage
            .get("modelContextWindow")
            .or_else(|| token_usage.get("model_context_window"))
            .and_then(Value::as_u64)?;
        return ContextUsage::codex(used, size);
    }
    let update = params.get("update").unwrap_or(params);
    let used = update.get("used").and_then(Value::as_u64)?;
    let size = update.get("size").and_then(Value::as_u64)?;
    ContextUsage::reported(used, size)
}

fn available_compact_update(params: &Value) -> Option<bool> {
    let update = params.get("update").unwrap_or(params);
    let command_list = update
        .get("availableCommands")
        .or_else(|| update.get("available_commands"))?
        .as_array()?;
    Some(command_list.iter().any(|command| {
        command
            .get("name")
            .and_then(Value::as_str)
            .is_some_and(|name| name.trim_start_matches('/').eq_ignore_ascii_case("compact"))
    }))
}

fn turn_token_count(params: &Value) -> Option<u64> {
    for pointer in [
        "/tokenUsage/last/totalTokens",
        "/tokenUsage/last/total_tokens",
        "/turn/tokenUsage/totalTokens",
        "/turn/tokenUsage/total_tokens",
        "/usage/totalTokens",
        "/usage/total_tokens",
        "/update/used",
        "/used",
    ] {
        if let Some(token_count) = params.pointer(pointer).and_then(Value::as_u64) {
            return Some(token_count);
        }
    }
    let last_usage = params
        .pointer("/tokenUsage/last")
        .or_else(|| params.pointer("/turn/tokenUsage"))
        .or_else(|| params.get("usage"))?;
    let token_count = [
        "inputTokens",
        "input_tokens",
        "cachedInputTokens",
        "cached_input_tokens",
        "outputTokens",
        "output_tokens",
        "reasoningOutputTokens",
        "reasoning_output_tokens",
    ]
    .into_iter()
    .filter_map(|field| last_usage.get(field).and_then(Value::as_u64))
    .sum::<u64>();
    (token_count > 0).then_some(token_count)
}

fn is_tool_lifecycle_event(method_lower: &str, encoded: &str) -> bool {
    method_lower.contains("tool")
        || method_lower.contains("commandexecution")
        || method_lower.contains("filechange")
        || encoded.contains("\"command_execution\"")
        || encoded.contains("\"type\":\"commandexecution\"")
        || encoded.contains("\"type\":\"dynamictoolcall\"")
        || encoded.contains("\"type\":\"filechange\"")
        || encoded.contains("\"type\":\"mcptoolcall\"")
}

fn control_tool_name<'value>(method: &str, value: &'value Value) -> Option<&'value str> {
    let method_lower = method.to_ascii_lowercase();
    let envelope = value.get("update").unwrap_or(value);
    let item = envelope.get("item").unwrap_or(envelope);
    let dynamic_tool_call = method_lower == "item/tool/call"
        || item
            .get("type")
            .and_then(Value::as_str)
            .is_some_and(|item_type| item_type.eq_ignore_ascii_case("dynamicToolCall"));
    if !dynamic_tool_call {
        return None;
    }
    ["tool", "name", "toolName", "tool_name"]
        .into_iter()
        .find_map(|key| item.get(key).and_then(Value::as_str))
        .filter(|name| {
            matches!(
                *name,
                "harness_plan_edit"
                    | "harness_plan_read"
                    | "harness_plan_submit"
                    | "harness_plan_deviation"
                    | "harness_plan_task_report"
                    | "harness_question_ask"
                    | "harness_question_answer"
                    | "harness_question_withdraw"
                    | "harness_goal_complete"
                    | "harness_goal_blocked"
                    | "harness_goal_status"
            )
        })
}

fn find_control_arguments(value: &Value, control_name: &str) -> Option<Value> {
    if value
        .get("tool")
        .or_else(|| value.get("name"))
        .or_else(|| value.get("toolName"))
        .or_else(|| value.get("tool_name"))
        .and_then(Value::as_str)
        == Some(control_name)
        && let Some(arguments) = value.get("arguments")
    {
        if let Some(arguments) = arguments.as_str() {
            return serde_json::from_str(arguments).ok();
        }
        return Some(arguments.clone());
    }
    match value {
        Value::Array(item) => item
            .iter()
            .find_map(|item| find_control_arguments(item, control_name)),
        Value::Object(map) => map
            .values()
            .find_map(|item| find_control_arguments(item, control_name)),
        _ => None,
    }
}

fn control_invocation(method: &str, value: &Value) -> Result<Option<ControlToolInvocation>> {
    let Some(control_name) = control_tool_name(method, value) else {
        return Ok(None);
    };
    let arguments = find_control_arguments(value, control_name)
        .with_context(|| format!("{control_name} omitted its arguments object"))?;
    Ok(Some(ControlToolInvocation {
        name: control_name.to_owned(),
        arguments,
    }))
}

fn append_output_event(output: &mut BackendOutput, event: BackendEvent) {
    if let Some(update) = event.task_update.as_ref()
        && let Some(index) = output.event.iter().rposition(|previous| {
            previous
                .task_update
                .as_ref()
                .is_some_and(|previous_update| previous_update.scope_id == update.scope_id)
        })
    {
        let mut previous = output.event.remove(index);
        merge_task_update(&mut previous, event);
        output.event.push(previous);
        return;
    }
    if let Some(activity) = event.activity.as_ref() {
        let exact_index = output.event.iter().rposition(|previous| {
            previous
                .activity
                .as_ref()
                .is_some_and(|previous_activity| previous_activity.id == activity.id)
        });
        let fallback_index = exact_index.or_else(|| {
            let meaningful_title =
                !matches!(activity.title.as_str(), "command" | "file changes" | "tool");
            if !meaningful_title
                || (activity.output.is_none() && activity.status.as_deref() == Some("inProgress"))
            {
                return None;
            }
            let matching_index = output
                .event
                .iter()
                .enumerate()
                .filter_map(|(index, previous)| {
                    let previous_activity = previous.activity.as_ref()?;
                    (previous_activity.kind == activity.kind
                        && previous_activity.title == activity.title
                        && matches!(
                            previous_activity.status.as_deref(),
                            Some("inProgress" | "in_progress")
                        ))
                    .then_some(index)
                })
                .collect::<Vec<_>>();
            if matching_index.len() == 1 {
                Some(matching_index[0])
            } else {
                None
            }
        });
        if let Some(index) = fallback_index {
            merge_tool_activity(&mut output.event[index], event);
            return;
        }
    }
    let mergeable = matches!(event.kind.as_str(), "assistant_message" | "reasoning");
    if mergeable
        && let Some(text) = event.text.as_deref()
        && let Some(previous) = output.event.last_mut()
        && previous.kind == event.kind
        && let Some(previous_text) = previous.text.as_mut()
    {
        previous_text.push_str(text);
        previous.data = event.data;
        return;
    }
    output.event.push(event);
}

fn merge_task_update(previous: &mut BackendEvent, incoming: BackendEvent) {
    let Some(incoming_update) = incoming.task_update else {
        return;
    };
    let Some(previous_update) = previous.task_update.as_mut() else {
        return;
    };
    if incoming_update.name.is_some() {
        previous_update.name = incoming_update.name;
    }
    if incoming_update.replace_entries {
        previous_update.entry_list = incoming_update.entry_list;
    }
    previous_update.replace_entries = incoming_update.replace_entries;
    previous_update.complete = incoming_update.complete;
    previous.data = incoming.data;
}

fn normalize_task_update(method: &str, params: &Value) -> Option<ProviderTaskUpdate> {
    let envelope = params.get("update").unwrap_or(params);
    let entry_list = envelope
        .get("plan")
        .or_else(|| envelope.get("entries"))
        .and_then(Value::as_array);
    let replace_entries = entry_list.is_some();
    let entry_list = entry_list
        .into_iter()
        .flatten()
        .enumerate()
        .filter_map(|(provider_ordinal, entry)| {
            let content = entry
                .get("step")
                .or_else(|| entry.get("content"))
                .and_then(Value::as_str)?;
            let status = match entry
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or("pending")
                .to_ascii_lowercase()
                .as_str()
            {
                "inprogress" | "in_progress" | "active" => TaskStatus::InProgress,
                "completed" | "complete" | "done" => TaskStatus::Completed,
                _ => TaskStatus::Pending,
            };
            Some(ProviderTaskEntry {
                provider_id: entry
                    .get("id")
                    .or_else(|| entry.get("taskId"))
                    .or_else(|| entry.get("task_id"))
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                content: content.to_owned(),
                priority: entry
                    .get("priority")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                status,
                provider_ordinal,
            })
        })
        .collect::<Vec<_>>();
    let method_lower = method.to_ascii_lowercase();
    let completed = method_lower.ends_with("/completed")
        || envelope
            .get("status")
            .and_then(Value::as_str)
            .is_some_and(|status| matches!(status, "completed" | "complete"));
    if entry_list.is_empty() && !completed {
        return None;
    }
    let scope_id = envelope
        .get("turnId")
        .or_else(|| envelope.get("turn_id"))
        .or_else(|| envelope.get("threadId"))
        .or_else(|| envelope.get("thread_id"))
        .and_then(Value::as_str)
        .unwrap_or("plan")
        .to_owned();
    let name = envelope
        .get("planName")
        .or_else(|| envelope.get("plan_name"))
        .or_else(|| envelope.get("name"))
        .or_else(|| envelope.get("title"))
        .and_then(Value::as_str)
        .filter(|name| !name.trim().is_empty())
        .map(str::to_owned);
    Some(ProviderTaskUpdate {
        scope_id,
        name,
        complete: completed,
        replace_entries,
        entry_list,
    })
}

fn normalize_tool_activity(method: &str, params: &Value, workspace: &str) -> Option<ToolActivity> {
    let envelope = params.get("update").unwrap_or(params);
    let item = envelope.get("item").unwrap_or(envelope);
    let id = item
        .get("id")
        .or_else(|| envelope.get("itemId"))
        .or_else(|| envelope.get("item_id"))
        .or_else(|| envelope.get("toolCallId"))
        .or_else(|| envelope.get("tool_call_id"))
        .or_else(|| envelope.get("callId"))
        .and_then(Value::as_str)?
        .to_owned();
    let item_type = item
        .get("type")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_ascii_lowercase();
    let method_lower = method.to_ascii_lowercase();
    let kind = if item_type.contains("commandexecution")
        || item_type.contains("command_execution")
        || method_lower.contains("commandexecution")
    {
        ToolActivityKind::Command
    } else if item_type.contains("filechange")
        || item_type.contains("file_change")
        || method_lower.contains("filechange")
    {
        ToolActivityKind::FileChange
    } else {
        ToolActivityKind::ToolCall
    };
    let title = mcp_tool_title(item)
        .or_else(|| {
            [
                item.get("command"),
                envelope.get("command"),
                item.get("title"),
                envelope.get("title"),
                item.get("tool"),
                envelope.get("tool"),
                item.get("name"),
                envelope.get("name"),
            ]
            .into_iter()
            .flatten()
            .find_map(tool_title)
        })
        .unwrap_or_else(|| match kind {
            ToolActivityKind::Command => "command".into(),
            ToolActivityKind::FileChange => "file changes".into(),
            ToolActivityKind::ToolCall => "tool".into(),
        });
    let output = mcp_tool_output(item)
        .or_else(|| {
            item.get("contentItems")
                .and_then(first_text)
                .map(strip_ansi_escapes::strip_str)
        })
        .or_else(|| {
            item.get("aggregatedOutput")
                .or_else(|| item.get("aggregated_output"))
                .or_else(|| item.get("output"))
                .or_else(|| envelope.get("output"))
                .or_else(|| envelope.get("delta"))
                .or_else(|| envelope.get("message"))
                .and_then(Value::as_str)
                .map(strip_ansi_escapes::strip_str)
                .or_else(|| {
                    envelope
                        .get("content")
                        .and_then(first_text)
                        .map(strip_ansi_escapes::strip_str)
                })
        });
    let status = item
        .get("status")
        .or_else(|| envelope.get("status"))
        .and_then(Value::as_str)
        .map(str::to_owned)
        .or_else(|| {
            method_lower
                .ends_with("/completed")
                .then(|| "completed".to_owned())
        })
        .or_else(|| {
            method_lower
                .ends_with("/started")
                .then(|| "inProgress".to_owned())
        })
        .or_else(|| {
            method_lower
                .eq("item/mcptoolcall/progress")
                .then(|| "inProgress".to_owned())
        });
    Some(ToolActivity {
        id,
        kind,
        title,
        output,
        status,
        change: normalize_file_change_set(item, envelope, workspace),
        output_delta: method_lower.ends_with("/outputdelta")
            || method_lower.ends_with("/output_delta"),
    })
}

fn normalize_file_change_set(item: &Value, envelope: &Value, workspace: &str) -> ProviderChangeSet {
    let change_list = item
        .get("changes")
        .or_else(|| item.get("change_list"))
        .or_else(|| envelope.get("changes"))
        .or_else(|| envelope.get("change_list"))
        .and_then(Value::as_array);
    let Some(change_list) = change_list else {
        return ProviderChangeSet::default();
    };
    let file = change_list
        .iter()
        .filter_map(|change| {
            let path = change
                .get("path")
                .or_else(|| change.get("filePath"))
                .or_else(|| change.get("file_path"))
                .and_then(Value::as_str)?;
            let path = provider_change_path(path, workspace);
            let move_path = change
                .get("movePath")
                .or_else(|| change.get("move_path"))
                .and_then(Value::as_str)
                .map(|path| provider_change_path(path, workspace));
            let kind = if move_path.is_some() {
                ProviderChangeKind::Move
            } else {
                match change
                    .get("kind")
                    .or_else(|| change.get("operation"))
                    .and_then(|kind| {
                        kind.as_str()
                            .or_else(|| kind.get("type").and_then(Value::as_str))
                    })
                    .unwrap_or("update")
                    .to_ascii_lowercase()
                    .as_str()
                {
                    "add" | "create" | "created" => ProviderChangeKind::Add,
                    "delete" | "deleted" | "remove" => ProviderChangeKind::Delete,
                    "move" | "rename" => ProviderChangeKind::Move,
                    _ => ProviderChangeKind::Update,
                }
            };
            let diff = change
                .get("diff")
                .or_else(|| change.get("patch"))
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned();
            Some(ProviderFileChange {
                path,
                move_path,
                kind,
                diff,
            })
        })
        .collect();
    ProviderChangeSet { file }
}

fn provider_change_path(path: &str, workspace: &str) -> String {
    if workspace.is_empty() {
        return path.to_owned();
    }
    Path::new(path)
        .strip_prefix(Path::new(workspace))
        .unwrap_or_else(|_| Path::new(path))
        .to_string_lossy()
        .replace('\\', "/")
}

fn successful_file_change(activity: &ToolActivity) -> bool {
    activity.kind == ToolActivityKind::FileChange
        && !activity.change.is_empty()
        && activity.status.as_deref().is_some_and(|status| {
            matches!(
                status.to_ascii_lowercase().as_str(),
                "completed" | "complete" | "success" | "succeeded"
            )
        })
}

fn tool_title(value: &Value) -> Option<String> {
    match value {
        Value::String(title) => compact_tool_title(title),
        Value::Array(part_list) => {
            let title = part_list
                .iter()
                .filter_map(Value::as_str)
                .collect::<Vec<_>>()
                .join(" ");
            compact_tool_title(&title)
        }
        _ => None,
    }
}

fn mcp_tool_title(item: &Value) -> Option<String> {
    item.get("type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("mcpToolCall"))
        .then_some(())?;
    let server = item.get("server").and_then(Value::as_str)?;
    let tool = item.get("tool").and_then(Value::as_str)?;
    let arguments = item
        .get("arguments")
        .cloned()
        .unwrap_or_else(|| Value::Object(Default::default()));
    let arguments = redact_mcp_arguments(&arguments);
    let arguments = serde_json::to_string(&arguments).ok()?;
    compact_tool_title(&format!("{server}.{tool}({arguments})"))
}

fn mcp_tool_output(item: &Value) -> Option<String> {
    item.get("type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("mcpToolCall"))
        .then_some(())?;
    if let Some(message) = item.pointer("/error/message").and_then(Value::as_str) {
        return Some(strip_ansi_escapes::strip_str(message));
    }
    let result = item.get("result")?;
    let text_list = result
        .get("content")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|content| content.get("text").and_then(Value::as_str))
        .map(format_mcp_text)
        .collect::<Vec<_>>();
    if !text_list.is_empty() {
        return Some(text_list.join("\n"));
    }
    result
        .get("structuredContent")
        .and_then(|content| serde_json::to_string_pretty(content).ok())
}

fn redact_mcp_arguments(value: &Value) -> Value {
    match value {
        Value::Array(value_list) => {
            Value::Array(value_list.iter().map(redact_mcp_arguments).collect())
        }
        Value::Object(value_map) => Value::Object(
            value_map
                .iter()
                .map(|(key, value)| {
                    let value = if sensitive_mcp_argument_key(key) {
                        Value::String("[REDACTED]".into())
                    } else {
                        redact_mcp_arguments(value)
                    };
                    (key.clone(), value)
                })
                .collect(),
        ),
        _ => value.clone(),
    }
}

fn sensitive_mcp_argument_key(key: &str) -> bool {
    matches!(
        key.to_ascii_lowercase().replace(['-', '_'], "").as_str(),
        "token"
            | "apikey"
            | "key"
            | "secret"
            | "password"
            | "passphrase"
            | "authorization"
            | "bearer"
            | "cookie"
            | "session"
            | "credential"
            | "accesstoken"
            | "refreshtoken"
            | "clientsecret"
            | "privatekey"
    )
}

fn format_mcp_text(text: &str) -> String {
    let text = strip_ansi_escapes::strip_str(text);
    serde_json::from_str::<Value>(&text)
        .and_then(|value| serde_json::to_string_pretty(&value))
        .unwrap_or(text)
}

fn compact_tool_title(title: &str) -> Option<String> {
    let compact = title.split_whitespace().collect::<Vec<_>>().join(" ");
    (!compact.is_empty()).then_some(compact)
}

fn merge_tool_activity(previous: &mut BackendEvent, incoming: BackendEvent) {
    let Some(incoming_activity) = incoming.activity else {
        return;
    };
    let Some(previous_activity) = previous.activity.as_mut() else {
        return;
    };
    if !incoming_activity.title.is_empty()
        && !matches!(
            incoming_activity.title.as_str(),
            "command" | "file changes" | "tool"
        )
    {
        previous_activity.title = incoming_activity.title;
    }
    if let Some(output) = incoming_activity.output {
        if incoming_activity.output_delta {
            previous_activity
                .output
                .get_or_insert_default()
                .push_str(&output);
        } else {
            previous_activity.output = Some(output);
        }
    }
    if incoming_activity.status.is_some() {
        previous_activity.status = incoming_activity.status;
    }
    if !incoming_activity.change.is_empty() {
        previous_activity.change = incoming_activity.change;
    }
    previous_activity.output_delta = false;
    previous.data = incoming.data;
}

fn first_text(value: &Value) -> Option<String> {
    for pointer in [
        "/delta",
        "/text",
        "/content/text",
        "/update/content/text",
        "/item/content/text",
        "/msg/message",
    ] {
        if let Some(text) = value.pointer(pointer).and_then(Value::as_str) {
            return Some(text.to_owned());
        }
    }
    match value {
        Value::Array(item) => item.iter().find_map(first_text),
        Value::Object(map) => map.values().find_map(first_text),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn extracts_structured_questions_from_dynamic_tool_arguments() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "type": "dynamicToolCall",
                        "tool": "harness_question_ask",
                        "arguments": {
                            "questions": [{
                                "header": "Migration",
                                "question": "Which migration should the plan use?",
                                "options": [
                                    { "label": "Staged", "description": "Support both formats temporarily." },
                                    { "label": "Immediate", "description": "Remove the old format now." }
                                ],
                        "allow_freeform": true
                            }]
                        }
                    }
                }
            }),
            &mut output,
            None,
        );
        let question = output.plan_question.expect("structured planning question");
        assert_eq!(question.questions[0].header, "Migration");
        assert_eq!(question.questions[0].options[0].label, "Staged");
        assert!(output.event.is_empty());
    }

    #[test]
    fn extracts_answer_and_withdrawal_without_rendering_control_tools() {
        let mut answer_output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": { "item": {
                    "type": "dynamicToolCall",
                    "tool": "harness_question_answer",
                    "arguments": {
                        "question_id": "migration",
                        "response": { "kind": "selected", "option": "Staged" }
                    }
                } }
            }),
            &mut answer_output,
            None,
        );
        let answer = answer_output
            .question_answer
            .expect("structured question answer");
        assert_eq!(answer.question_id, "migration");
        assert!(answer_output.event.is_empty());

        let mut withdrawal_output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": { "item": {
                    "type": "dynamicToolCall",
                    "tool": "harness_question_withdraw",
                    "arguments": { "reason": "Repository policy determines the choice." }
                } }
            }),
            &mut withdrawal_output,
            None,
        );
        assert_eq!(
            withdrawal_output.question_withdrawal.unwrap().reason,
            "Repository policy determines the choice."
        );
        assert!(withdrawal_output.event.is_empty());
    }

    #[test]
    fn replaces_task_state_across_lifecycle_events_without_creating_an_artifact() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "turn/plan/updated",
                "params": { "plan": [{ "step": "Inspect the change" }] }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "turn/plan/completed",
                "params": { "status": "completed" }
            }),
            &mut output,
            None,
        );
        assert!(output.plan_edit.is_empty());
        assert_eq!(output.event.len(), 1);
        let update = output.event[0].task_update.as_ref().unwrap();
        assert!(update.complete);
        assert_eq!(update.entry_list.len(), 1);
        assert_eq!(update.entry_list[0].content, "Inspect the change");
    }

    #[test]
    fn ignores_control_tool_names_inside_command_output() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/commandExecution/completed",
                "params": { "output": "printed harness_goal_complete as documentation" }
            }),
            &mut output,
            None,
        );
        assert!(output.evidence.tool_called);
        assert!(!output.evidence.structured_complete);
    }

    #[test]
    fn recognizes_exact_control_tool_calls() {
        let mut output = BackendOutput::default();
        let (event_sink, event_stream) = tokio::sync::mpsc::unbounded_channel();
        normalize_event(
            &json!({
                "method": "item/tool/call",
                "params": {
                    "name": "harness_goal_complete",
                    "arguments": { "summary": "Complete" }
                }
            }),
            &mut output,
            Some(&event_sink),
        );
        assert!(output.evidence.structured_complete);
        assert!(output.event.is_empty());
        assert!(event_stream.is_empty());
    }

    #[test]
    fn correlates_dynamic_tool_trace_metadata_without_copying_arguments() {
        let message = json!({
            "jsonrpc": "2.0",
            "id": 42,
            "method": "item/tool/call",
            "params": {
                "threadId": "thread-1",
                "turnId": "turn-1",
                "callId": "call-1",
                "tool": "harness_plan_edit",
                "arguments": {
                    "plan_id": "plan-1",
                    "expected_version": 3,
                    "plan": {
                        "modify": {
                            "overview": "Updated overview",
                            "usage": null
                        }
                    }
                }
            }
        });

        let context = DynamicToolTraceContext::from_message(&message).unwrap();
        let payload = context.payload(json!({ "phase": "argument_validation" }));

        assert_eq!(context.invocation_id, "call-1");
        assert_eq!(context.mutation_count, 2);
        assert_eq!(context.mutation_summary["plan"]["modify"], 2);
        assert_eq!(context.plan_id.as_deref(), Some("plan-1"));
        assert_eq!(context.expected_version, Some(3));
        assert_eq!(payload["thread_id"], "thread-1");
        assert_eq!(payload["turn_id"], "turn-1");
        assert_eq!(payload["phase"], "argument_validation");
        assert!(payload.get("arguments").is_none());
        assert!(context.argument_bytes > 0);
    }

    #[test]
    fn falls_back_to_the_json_rpc_id_for_dynamic_tool_trace_correlation() {
        let message = json!({
            "jsonrpc": "2.0",
            "id": "request-7",
            "method": "item/tool/call",
            "params": {
                "name": "harness_plan_submit",
                "arguments": {
                    "plan_id": "plan-1",
                    "expected_version": 4
                }
            }
        });

        let context = DynamicToolTraceContext::from_message(&message).unwrap();

        assert_eq!(context.invocation_id, "rpc-request-7");
        assert_eq!(context.request_id, "request-7");
    }

    #[test]
    fn renders_completed_control_calls_without_reapplying_their_semantic_effect() {
        let mut output = BackendOutput::default();
        let completed = json!({
            "method": "item/completed",
            "params": { "item": {
                "id": "plan-edit-1",
                "type": "dynamicToolCall",
                "tool": "harness_plan_edit",
                "status": "completed",
                "success": true,
                "contentItems": [{ "type": "inputText", "text": "Plan edit accepted." }],
                "arguments": {
                    "plan_id": "plan-1",
                    "expected_version": 1,
                    "plan": { "modify": { "overview": "Updated overview" } }
                }
            } }
        });

        normalize_event_in_workspace(&completed, &mut output, None, "", false);

        assert!(output.plan_edit.is_empty());
        let activity = output.event[0]
            .activity
            .as_ref()
            .expect("control tool activity");
        assert_eq!(activity.id, "plan-edit-1");
        assert_eq!(activity.title, "harness_plan_edit");
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(activity.output.as_deref(), Some("Plan edit accepted."));
    }

    #[test]
    fn gives_replayed_control_lifecycle_and_request_messages_one_identity() {
        let arguments = json!({
            "plan_id": "plan-id",
            "expected_version": 1,
            "plan": { "modify": { "overview": "Updated overview" } }
        });
        let completed = json!({
            "method": "item/completed",
            "params": {
                "item": {
                    "id": "dynamic-tool-1",
                    "type": "dynamicToolCall",
                    "tool": "harness_plan_edit",
                    "arguments": arguments
                }
            }
        });
        let request = json!({
            "method": "item/tool/call",
            "params": {
                "name": "harness_plan_edit",
                "arguments": arguments
            }
        });

        assert_eq!(
            control_invocation_key("item/completed", &completed),
            control_invocation_key("item/tool/call", &request)
        );
        assert!(
            control_invocation_key(
                "item/started",
                &json!({
                    "method": "item/started",
                    "params": {
                        "item": {
                            "type": "dynamicToolCall",
                            "tool": "harness_plan_edit"
                        }
                    }
                })
            )
            .is_none()
        );
    }

    #[test]
    fn rejects_question_resolution_only_after_planning_feedback_is_consumed() {
        let answer = json!({
            "method": "item/tool/call",
            "params": {
                "name": "harness_question_answer",
                "arguments": {
                    "question_id": "reader_mode",
                    "option_id": "preview_cli"
                }
            }
        });
        let withdraw = json!({
            "method": "item/tool/call",
            "params": {
                "name": "harness_question_withdraw",
                "arguments": {
                    "question_id": "reader_mode"
                }
            }
        });
        let plan_edit = json!({
            "method": "item/tool/call",
            "params": {
                "name": "harness_plan_edit",
                "arguments": {}
            }
        });

        assert!(rejects_question_resolution(true, "item/tool/call", &answer));
        assert!(rejects_question_resolution(
            true,
            "item/tool/call",
            &withdraw
        ));
        assert!(!rejects_question_resolution(
            false,
            "item/tool/call",
            &answer
        ));
        assert!(!rejects_question_resolution(
            true,
            "item/tool/call",
            &plan_edit
        ));
        assert!(!rejects_question_resolution(
            true,
            "item/tool/result",
            &answer
        ));
    }

    #[test]
    fn coalesces_message_deltas_without_treating_prose_as_control_tools() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/agentMessage/delta",
                "params": { "delta": "I will call harness_goal_complete " }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "item/agentMessage/delta",
                "params": { "delta": "after the plan finishes." }
            }),
            &mut output,
            None,
        );
        assert_eq!(output.event.len(), 1);
        assert_eq!(
            output.event[0].text.as_deref(),
            Some("I will call harness_goal_complete after the plan finishes.")
        );
        assert!(!output.evidence.structured_complete);
        assert_eq!(output.event[0].kind, "assistant_message");
    }

    #[test]
    fn ignores_codex_user_message_lifecycle_events() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/started",
                "params": {
                    "item": {
                        "type": "userMessage",
                        "content": [{ "type": "text", "text": "Explain this repository" }]
                    }
                }
            }),
            &mut output,
            None,
        );
        assert!(output.event.is_empty());
    }

    #[test]
    fn correlates_codex_command_lifecycle_into_one_activity() {
        let mut output = BackendOutput::default();
        let (event_sink, event_stream) = tokio::sync::mpsc::unbounded_channel();
        normalize_event(
            &json!({
                "method": "item/started",
                "params": {
                    "item": {
                        "id": "command-1",
                        "type": "commandExecution",
                        "command": "Get-Content README.md",
                        "status": "inProgress"
                    }
                }
            }),
            &mut output,
            Some(&event_sink),
        );
        normalize_event(
            &json!({
                "method": "item/commandExecution/outputDelta",
                "params": { "itemId": "command-1", "delta": "# Harness\nline two\n" }
            }),
            &mut output,
            Some(&event_sink),
        );
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "command-completed-with-different-provider-id",
                        "type": "commandExecution",
                        "command": "Get-Content README.md",
                        "status": "completed",
                        "aggregatedOutput": "# Harness\nline two\n"
                    }
                }
            }),
            &mut output,
            Some(&event_sink),
        );

        assert_eq!(
            event_stream.len(),
            3,
            "each lifecycle update must stream live"
        );
        assert_eq!(
            output.event.len(),
            1,
            "one command must persist as one activity"
        );
        let activity = output.event[0].activity.as_ref().unwrap();
        assert_eq!(activity.id, "command-1");
        assert_eq!(activity.title, "Get-Content README.md");
        assert_eq!(activity.output.as_deref(), Some("# Harness\nline two\n"));
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert!(!activity.output_delta);
    }

    #[test]
    fn correlates_codex_mcp_lifecycle_into_one_activity() {
        let mut output = BackendOutput::default();
        let (event_sink, event_stream) = tokio::sync::mpsc::unbounded_channel();
        normalize_event(
            &json!({
                "method": "item/started",
                "params": {
                    "item": {
                        "id": "mcp-1",
                        "type": "mcpToolCall",
                        "server": "docs-mcp",
                        "tool": "crate_get",
                        "arguments": {
                            "crate": "bevy",
                            "api_key": "must-not-persist",
                            "nested": { "clientSecret": "also-secret" }
                        },
                        "status": "inProgress"
                    }
                }
            }),
            &mut output,
            Some(&event_sink),
        );
        normalize_event(
            &json!({
                "method": "item/mcpToolCall/progress",
                "params": {
                    "itemId": "mcp-1",
                    "message": "Fetching crate metadata"
                }
            }),
            &mut output,
            Some(&event_sink),
        );
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "mcp-1",
                        "type": "mcpToolCall",
                        "server": "docs-mcp",
                        "tool": "crate_get",
                        "arguments": {
                            "crate": "bevy",
                            "api_key": "must-not-persist",
                            "nested": { "clientSecret": "also-secret" }
                        },
                        "status": "completed",
                        "result": {
                            "content": [
                                { "type": "text", "text": "crate metadata" },
                                { "type": "text", "text": "release metadata" }
                            ]
                        }
                    }
                }
            }),
            &mut output,
            Some(&event_sink),
        );

        assert_eq!(
            event_stream.len(),
            3,
            "each MCP lifecycle update must stream live"
        );
        assert_eq!(
            output.event.len(),
            1,
            "one MCP call must persist as one activity"
        );
        let activity = output.event[0].activity.as_ref().expect("MCP activity");
        assert_eq!(activity.id, "mcp-1");
        assert_eq!(activity.kind, ToolActivityKind::ToolCall);
        assert_eq!(
            activity.title,
            "docs-mcp.crate_get({\"api_key\":\"[REDACTED]\",\"crate\":\"bevy\",\"nested\":{\"clientSecret\":\"[REDACTED]\"}})"
        );
        assert!(!activity.title.contains("must-not-persist"));
        assert!(!activity.title.contains("also-secret"));
        assert_eq!(
            activity.output.as_deref(),
            Some("crate metadata\nrelease metadata")
        );
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert!(output.evidence.tool_called);
    }

    #[test]
    fn normalizes_codex_mcp_failure_output() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "mcp-failed",
                        "type": "mcpToolCall",
                        "server": "docs-mcp",
                        "tool": "crate_get",
                        "status": "failed",
                        "error": { "message": "server unavailable" }
                    }
                }
            }),
            &mut output,
            None,
        );

        let activity = output.event[0]
            .activity
            .as_ref()
            .expect("failed MCP activity");
        assert_eq!(activity.output.as_deref(), Some("server unavailable"));
        assert_eq!(activity.status.as_deref(), Some("failed"));
    }

    #[test]
    fn prettifies_codex_mcp_json_results() {
        let output = mcp_tool_output(&json!({
            "type": "mcpToolCall",
            "result": {
                "content": [{
                    "type": "text",
                    "text": "{\"login\":\"mmgeorge\",\"details\":{\"followers\":22}}"
                }]
            }
        }));
        assert_eq!(
            output.as_deref(),
            Some("{\n  \"details\": {\n    \"followers\": 22\n  },\n  \"login\": \"mmgeorge\"\n}")
        );
    }

    #[test]
    fn does_not_classify_mcp_names_as_harness_control_tools() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "mcp-control-name",
                        "type": "mcpToolCall",
                        "server": "external-mcp",
                        "tool": "harness_goal_complete",
                        "status": "completed",
                        "result": { "content": [{ "type": "text", "text": "ordinary result" }] }
                    }
                }
            }),
            &mut output,
            None,
        );

        assert!(!output.evidence.structured_complete);
        let activity = output.event[0]
            .activity
            .as_ref()
            .expect("external MCP activity");
        assert_eq!(activity.title, "external-mcp.harness_goal_complete({})");
    }

    #[test]
    fn replaces_codex_patch_revisions_with_the_completed_change_set() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/fileChange/patchUpdated",
                "params": {
                    "itemId": "patch-1",
                    "changes": [{
                        "path": "src/main.rs",
                        "kind": "update",
                        "diff": "@@ -1 +1 @@\n-old\n+draft"
                    }]
                }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "patch-1",
                        "type": "fileChange",
                        "status": "completed",
                        "changes": [{
                            "path": "src/main.rs",
                            "kind": "update",
                            "diff": "@@ -1 +1 @@\n-old\n+final"
                        }]
                    }
                }
            }),
            &mut output,
            None,
        );

        assert_eq!(output.event.len(), 1);
        let activity = output.event[0].activity.as_ref().expect("file change");
        assert_eq!(activity.change.file.len(), 1);
        assert!(activity.change.file[0].diff.contains("+final"));
        assert!(!activity.change.file[0].diff.contains("+draft"));
        assert!(output.evidence.workspace_changed);
    }

    #[test]
    fn normalizes_codex_absolute_paths_and_structured_change_kinds() {
        let mut output = BackendOutput::default();
        normalize_event_in_workspace(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "patch-1",
                        "type": "fileChange",
                        "status": "completed",
                        "changes": [{
                            "path": "C:\\workspace\\src\\main.rs",
                            "kind": { "type": "add" },
                            "diff": "@@ -0,0 +1 @@\n+created"
                        }]
                    }
                }
            }),
            &mut output,
            None,
            "C:\\workspace",
            true,
        );

        let change = &output.event[0].activity.as_ref().unwrap().change.file[0];
        assert_eq!(change.path, "src/main.rs");
        assert_eq!(change.kind, ProviderChangeKind::Add);
    }

    #[test]
    fn declined_file_changes_do_not_count_as_workspace_progress() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "patch-1",
                        "type": "fileChange",
                        "status": "declined",
                        "changes": [{
                            "path": "src/main.rs",
                            "kind": "update",
                            "diff": "@@ -1 +1 @@\n-old\n+rejected"
                        }]
                    }
                }
            }),
            &mut output,
            None,
        );

        assert!(!output.evidence.workspace_changed);
        assert_eq!(
            output.event[0].activity.as_ref().unwrap().change.file.len(),
            1
        );
    }

    #[test]
    fn derives_codex_command_status_from_lifecycle_method() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/started",
                "params": {
                    "item": {
                        "id": "command-1",
                        "type": "commandExecution",
                        "command": "Get-Content rulesync.jsonc"
                    }
                }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "command-1",
                        "type": "commandExecution",
                        "command": "Get-Content rulesync.jsonc",
                        "aggregatedOutput": "{\"$schema\": \"rulesync\"}"
                    }
                }
            }),
            &mut output,
            None,
        );

        let activity = output.event[0].activity.as_ref().unwrap();
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(
            activity.output.as_deref(),
            Some("{\"$schema\": \"rulesync\"}")
        );
    }

    #[test]
    fn strips_terminal_controls_from_command_output() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "command-1",
                        "type": "commandExecution",
                        "command": "Get-ChildItem -Force",
                        "aggregatedOutput": "\u{1b}[32;1mMode\u{1b}[0m  \u{1b}[32;1mName\u{1b}[0m"
                    }
                }
            }),
            &mut output,
            None,
        );

        let activity = output.event[0].activity.as_ref().unwrap();
        assert_eq!(activity.output.as_deref(), Some("Mode  Name"));
    }

    #[test]
    fn ignores_codex_moderation_metadata_with_tool_named_fields() {
        let mut output = BackendOutput::default();
        let (event_sink, event_stream) = tokio::sync::mpsc::unbounded_channel();
        normalize_event(
            &json!({
                "method": "turn/moderationMetadata",
                "params": {
                    "metadata": {
                        "generation": {},
                        "prompt": {},
                        "tool_call": {},
                        "tool_response": {}
                    },
                    "threadId": "thread-1",
                    "turnId": "turn-1"
                }
            }),
            &mut output,
            Some(&event_sink),
        );
        assert!(output.event.is_empty());
        assert!(event_stream.is_empty());
    }

    #[test]
    fn compacts_multiline_command_titles_for_one_line_previews() {
        assert_eq!(
            compact_tool_title("pwsh -Command\n  Get-Content README.md\r\n").as_deref(),
            Some("pwsh -Command Get-Content README.md")
        );
    }

    #[test]
    fn records_token_usage_without_emitting_interaction_noise() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "thread/tokenUsage/updated",
                "params": {
                    "threadId": "thread-1",
                    "tokenUsage": {
                        "last": { "totalTokens": 2700 },
                        "modelContextWindow": 353000
                    }
                }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "turn/started",
                "params": { "turn": { "id": "turn-1", "status": "inProgress" } }
            }),
            &mut output,
            None,
        );

        assert_eq!(output.metrics.token_count, Some(2700));
        assert_eq!(
            output.metrics.context_usage,
            Some(ContextUsage {
                used: 2700,
                size: 353000,
                remaining_percent: 100,
            })
        );
        assert_eq!(output.event.len(), 1);
        assert_eq!(output.event[0].kind, "context_usage");
    }

    #[test]
    fn normalizes_codex_spawn_activity_without_rendering_it_as_a_tool() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "turnId": "parent-turn",
                    "item": {
                        "id": "spawn-1",
                        "type": "collabAgentToolCall",
                        "tool": "spawnAgent",
                        "agentType": "explorer",
                        "agentNickname": "Bevy scout",
                        "prompt": "Inspect Bevy",
                        "receiverThreadIds": ["child-thread"],
                        "status": "completed"
                    }
                }
            }),
            &mut output,
            None,
        );

        assert_eq!(output.event.len(), 1);
        assert_eq!(output.event[0].kind, "agent_lifecycle");
        let lifecycle: AgentLifecycleEvent =
            serde_json::from_value(output.event[0].data.clone()).unwrap();
        assert_eq!(
            lifecycle.provider_thread_id.as_deref(),
            Some("child-thread")
        );
        assert_eq!(lifecycle.definition.as_deref(), Some("explorer"));
        assert_eq!(lifecycle.status, AgentRunStatus::Running);
        assert!(output.event[0].activity.is_none());
    }

    #[test]
    fn normalizes_each_codex_child_state_instead_of_the_wait_tool_state() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "turnId": "parent-turn",
                    "item": {
                        "type": "collabAgentToolCall",
                        "tool": "wait",
                        "senderThreadId": "parent-thread",
                        "receiverThreadIds": ["complete-child", "running-child"],
                        "agentsStates": {
                            "complete-child": { "status": "completed" },
                            "running-child": { "status": "running" }
                        },
                        "status": "completed"
                    }
                }
            }),
            &mut output,
            None,
        );

        assert_eq!(output.event.len(), 2);
        let lifecycle_list = output
            .event
            .iter()
            .map(|event| serde_json::from_value::<AgentLifecycleEvent>(event.data.clone()).unwrap())
            .collect::<Vec<_>>();
        assert_eq!(lifecycle_list[0].status, AgentRunStatus::Completed);
        assert_eq!(lifecycle_list[1].status, AgentRunStatus::Running);
        assert_eq!(
            lifecycle_list[0].parent_thread_id.as_deref(),
            Some("parent-thread")
        );
    }

    #[test]
    fn ignores_a_codex_wait_without_child_state() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/completed",
                "params": {
                    "threadId": "parent-thread",
                    "item": {
                        "type": "collabAgentToolCall",
                        "tool": "wait",
                        "receiverThreadIds": [],
                        "agentsStates": {},
                        "status": "completed"
                    }
                }
            }),
            &mut output,
            None,
        );

        assert!(output.event.is_empty());
    }

    #[test]
    fn normalizes_codex_subagent_activity_with_its_child_identity() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/updated",
                "params": {
                    "threadId": "parent-thread",
                    "item": {
                        "type": "subAgentActivity",
                        "agentThreadId": "child-thread",
                        "agentPath": "explorer",
                        "kind": "interrupted"
                    }
                }
            }),
            &mut output,
            None,
        );

        let lifecycle: AgentLifecycleEvent =
            serde_json::from_value(output.event[0].data.clone()).unwrap();
        assert_eq!(
            lifecycle.provider_thread_id.as_deref(),
            Some("child-thread")
        );
        assert_eq!(lifecycle.status, AgentRunStatus::Interrupted);
    }

    #[test]
    fn ignores_codex_subagent_interaction_directed_at_the_parent() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "item/updated",
                "params": {
                    "threadId": "child-thread",
                    "item": {
                        "type": "subAgentActivity",
                        "agentThreadId": "parent-thread",
                        "agentPath": "/root",
                        "kind": "interacted"
                    }
                }
            }),
            &mut output,
            None,
        );

        assert!(output.event.is_empty());
    }
}
