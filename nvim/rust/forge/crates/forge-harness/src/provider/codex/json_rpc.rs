use super::approval::ApprovalContext;
#[cfg(test)]
use super::approval::permission_from_provider;
use super::request_execution::{ExecutedRequest, RequestExecutor};
#[cfg(test)]
use crate::agent::AgentExecutionState;
#[cfg(test)]
use crate::provider::ProviderEvent;
use crate::provider::approval::PermissionCoordinator;
#[cfg(test)]
use crate::provider::{ProviderChangeKind, ProviderChangeSet, ToolActivityKind};
use crate::provider::{ProviderEventSink, ProviderOutput};
use crate::session::ApprovalMode;
#[cfg(test)]
use crate::session::ContextUsage;
use crate::tools::{
    ControlToolExecutor, ControlToolInvocation, ControlToolRegistry, ControlTurnContext,
    apply_invocation,
};
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use forge_providers::codex::CodexConnection;
use forge_providers::codex::protocol::{self, McpContext};
#[cfg(test)]
use futures_util::{SinkExt, StreamExt};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};
#[cfg(test)]
use tokio_tungstenite::tungstenite::Message;

const DYNAMIC_TOOL_RESPONSE_TIMEOUT: Duration = Duration::from_secs(30);

#[cfg(test)]
fn fixture_scope() -> crate::provider::InvocationScope {
    crate::provider::InvocationScope {
        owner: crate::provider::ExecutionOwner::root("session"),
        exchange_id: "exchange".into(),
        invocation_id: "fixture-invocation".into(),
    }
}

#[cfg(test)]
fn fixture_normalization() -> super::normalization::CodexNormalization {
    let mut normalization = super::normalization::CodexNormalization::default();
    normalization.bind(Some(fixture_scope()));
    normalization
}

#[cfg(test)]
async fn normalize_event(
    normalization: &mut super::normalization::CodexNormalization,
    message: &Value,
    output: &mut ProviderOutput,
    sink: Option<&ProviderEventSink>,
) {
    normalize_event_in_workspace(normalization, message, output, sink, "", true).await;
}

#[cfg(test)]
async fn normalize_event_in_workspace(
    normalization: &mut super::normalization::CodexNormalization,
    message: &Value,
    output: &mut ProviderOutput,
    sink: Option<&ProviderEventSink>,
    workspace: &str,
    apply_control: bool,
) {
    let mut message = message.clone();
    let method = message["method"].as_str().unwrap_or_default();
    if method.starts_with("item/") || method.starts_with("turn/plan") {
        let params = &mut message["params"];
        if params.get("threadId").is_none() {
            params["threadId"] = json!("thread");
        }
        if params.get("turnId").is_none() {
            params["turnId"] = json!("turn");
        }
    }
    if message["method"] == "item/agentMessage/delta" && message["params"]["itemId"].is_null() {
        message["params"]["itemId"] = json!("fixture-message");
    }
    let previous = output.event.len();
    normalization
        .normalize(&message, output, None, workspace, apply_control)
        .await
        .unwrap();
    if let Some(sink) = sink {
        for event in &output.event[previous..] {
            sink.send_wait(event.clone()).await.unwrap();
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct DynamicToolTraceContext {
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
    pub(super) fn from_message(message: &Value) -> Option<Self> {
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

    pub(super) fn payload(&self, fields: Value) -> Value {
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
    if let Some(plan) = arguments.get("plan").and_then(Value::as_object)
        && !plan.is_empty()
    {
        resource_map.insert("plan".into(), json!({ "patch": plan.len() }));
    }
    if arguments.get("assumptions").is_some_and(Value::is_array) {
        resource_map.insert("assumptions".into(), json!({ "replace": 1 }));
    }
    for resource_name in ["entity_changes", "dependencies", "flows", "tasks"] {
        let Some(operation_list) = arguments.get(resource_name).and_then(Value::as_array) else {
            continue;
        };
        let mut action_map = serde_json::Map::new();
        for action_name in ["create", "replace", "delete"] {
            let count = operation_list
                .iter()
                .filter(|operation| {
                    operation.get("operation").and_then(Value::as_str) == Some(action_name)
                })
                .count();
            if count > 0 {
                action_map.insert(action_name.into(), json!(count));
            }
        }
        if !action_map.is_empty() {
            resource_map.insert(resource_name.into(), Value::Object(action_map));
        }
    }
    Value::Object(resource_map)
}

/// Separates provider frames from completed local tool work.
pub(super) enum ReceivedMessage {
    /// A frame read from the provider socket.
    Provider(Value),
    /// A local request ready for response publication.
    Request(Box<ExecutedRequest>),
}

impl ReceivedMessage {
    /// Returns provider routing metadata regardless of execution stage.
    pub(super) fn message(&self) -> &Value {
        match self {
            Self::Provider(message) => message,
            Self::Request(request) => &request.message,
        }
    }
}

/// Owns one session-scoped JSON-RPC connection to the shared Codex app-server.
pub struct CodexJsonRpc {
    socket: CodexConnection,
    next_id: u64,
    workspace: String,
    approval_mode: ApprovalMode,
    planning_feedback: bool,
    request_executor: Arc<tokio::sync::Mutex<RequestExecutor>>,
    control_executor: Option<Arc<ControlToolExecutor>>,
    request_tasks: tokio::task::JoinSet<ExecutedRequest>,
    pending_requests: HashMap<tokio::task::Id, Value>,
    control_completion: Option<tokio::sync::oneshot::Receiver<()>>,
    accepted_control_invocation_key_set: HashSet<String>,
    mcp: McpContext,
    approval: ApprovalContext,
    permission_coordinator: Arc<PermissionCoordinator>,
    event_sink: Option<ProviderEventSink>,
    background_sink: Option<ProviderEventSink>,
    activity_publication: bool,
    requests_cancelled: bool,
    trace: Arc<TraceStore>,
    session_id: String,
    execution_owner: Option<crate::provider::ExecutionOwner>,
    pub(super) normalization: super::normalization::CodexNormalization,
}

impl CodexJsonRpc {
    /// Connect one session channel to the shared provider process.
    #[cfg(test)]
    pub async fn connect(
        endpoint: &str,
        workspace: &str,
        approval_mode: ApprovalMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<ProviderEventSink>,
        trace: Arc<TraceStore>,
        session_id: String,
    ) -> Result<Self> {
        let socket = CodexConnection::connect(endpoint)
            .await
            .map_err(crate::provider::ProviderError::from)?;
        Self::from_connection(
            socket,
            workspace,
            approval_mode,
            permission_coordinator,
            event_sink,
            trace,
            session_id,
        )
    }

    /// Attach Harness callbacks to an already connected native provider channel.
    pub fn from_connection(
        socket: CodexConnection,
        workspace: &str,
        approval_mode: ApprovalMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<ProviderEventSink>,
        trace: Arc<TraceStore>,
        session_id: String,
    ) -> Result<Self> {
        Ok(Self {
            socket,
            next_id: 1,
            workspace: workspace.to_owned(),
            approval_mode,
            planning_feedback: false,
            request_executor: Arc::new(tokio::sync::Mutex::new(RequestExecutor::new(
                workspace.to_owned(),
                approval_mode,
                permission_coordinator.clone(),
                event_sink.clone(),
                trace.clone(),
                session_id.clone(),
            ))),
            request_tasks: tokio::task::JoinSet::new(),
            control_executor: None,
            pending_requests: HashMap::new(),
            control_completion: None,
            accepted_control_invocation_key_set: HashSet::new(),
            mcp: McpContext::default(),
            approval: ApprovalContext::default(),
            permission_coordinator,
            event_sink: event_sink.clone(),
            background_sink: None,
            activity_publication: true,
            requests_cancelled: false,
            trace,
            session_id,
            execution_owner: None,
            normalization: {
                let mut normalization = super::normalization::CodexNormalization::default();
                normalization.bind(
                    event_sink
                        .as_ref()
                        .map(|sink| sink.invocation_scope().cloned())
                        .transpose()?,
                );
                normalization
            },
        })
    }

    /// Bind the same-owner observer that survives foreground invocation completion.
    pub(super) fn set_background_sink(&mut self, sink: Option<ProviderEventSink>) {
        self.background_sink = sink;
    }

    /// Deliver only exact retained tool evidence while the primary connection is idle.
    pub(super) async fn publish_idle_received(&mut self, received: ReceivedMessage) -> Result<()> {
        let mut output = ProviderOutput::default();
        match received {
            ReceivedMessage::Provider(message) => {
                if self.publish_background(&message, &mut output).await? {
                    return Ok(());
                }
                if message.get("id").is_some() && message.get("method").is_some() {
                    self.write_message(&json!({"jsonrpc":"2.0", "id":message["id"],
                        "error":{"code":-32001,"message":"No active provider invocation"}}))
                        .await?;
                }
            }
            ReceivedMessage::Request(request) => {
                self.complete_provider_request(*request, &mut output)
                    .await?;
            }
        }
        Ok(())
    }

    /// Route a retained native identity without borrowing the current invocation's sink.
    async fn publish_background(
        &mut self,
        message: &Value,
        output: &mut ProviderOutput,
    ) -> Result<bool> {
        let Some(events) = self
            .normalization
            .normalize_background(message, &self.workspace)
            .await?
        else {
            return Ok(false);
        };
        self.publish_background_events(events, output).await?;
        Ok(true)
    }

    /// Deliver retained updates through their original invocation-scoped observer.
    async fn publish_background_events(
        &self,
        events: Vec<crate::provider::ProviderEvent>,
        output: &mut ProviderOutput,
    ) -> Result<()> {
        for event in events {
            if let Some(base) = &self.background_sink {
                let scope = event.scope();
                let invocation = scope
                    .invocation()
                    .context("background tool omitted invocation ownership")?;
                base.invocation_sink(invocation)?.send_wait(event).await?;
            } else {
                output.event.push(event);
            }
        }
        Ok(())
    }

    /// Stop only admitted native background commands and publish their verified settlement.
    pub(super) async fn shutdown_background(&mut self) -> Result<()> {
        let addresses = self.normalization.pending_background_addresses();
        let threads: std::collections::BTreeSet<_> = addresses
            .iter()
            .map(|address| address.turn.thread_id.clone())
            .collect();
        let mut output = ProviderOutput::default();
        for thread_id in threads {
            // Preserve completion frames encountered while collecting exact owned processes.
            let terminals = self.background_inventory(&thread_id, &mut output).await?;
            for (item_id, process_id) in terminals {
                if !addresses.iter().any(|address| {
                    address.turn.thread_id == thread_id && address.tool_id == item_id
                }) {
                    continue;
                }
                let result = self
                    .request(
                        "thread/backgroundTerminals/terminate",
                        json!({"threadId":thread_id,"processId":process_id}),
                        &mut output,
                    )
                    .await?;
                anyhow::ensure!(
                    result.get("terminated").and_then(Value::as_bool).is_some(),
                    "native terminal termination omitted its outcome"
                );
            }
            // A second inventory proves absence even when natural completion won the first scan.
            let remaining = self.background_inventory(&thread_id, &mut output).await?;
            anyhow::ensure!(
                !remaining
                    .iter()
                    .any(|(item_id, _)| addresses.iter().any(|address| {
                        address.turn.thread_id == thread_id && &address.tool_id == item_id
                    })),
                "native background command remains running after shutdown cleanup"
            );
            let events = addresses
                .iter()
                .filter(|address| address.turn.thread_id == thread_id)
                .filter_map(|address| self.normalization.interrupt_background(address))
                .collect();
            self.publish_background_events(events, &mut output).await?;
        }
        Ok(())
    }

    /// Collect the complete native inventory without granting ownership to unrecognized items.
    async fn background_inventory(
        &mut self,
        thread_id: &str,
        output: &mut ProviderOutput,
    ) -> Result<Vec<(crate::provider::ToolId, String)>> {
        let mut terminals = Vec::new();
        let mut cursor: Option<String> = None;
        let mut seen = HashSet::new();
        loop {
            let result = self
                .request(
                    "thread/backgroundTerminals/list",
                    json!({"threadId":thread_id,"cursor":cursor}),
                    output,
                )
                .await?;
            for terminal in result
                .get("data")
                .and_then(Value::as_array)
                .context("native terminal inventory is missing")?
            {
                let item = terminal
                    .get("itemId")
                    .and_then(Value::as_str)
                    .context("native terminal item identity is missing")?;
                let process = terminal
                    .get("processId")
                    .and_then(Value::as_str)
                    .context("native terminal process identity is missing")?;
                terminals.push((item.into(), process.to_owned()));
            }
            let Some(next) = result.get("nextCursor").and_then(Value::as_str) else {
                return Ok(terminals);
            };
            anyhow::ensure!(
                seen.insert(next.to_owned()),
                "native terminal inventory repeated a pagination cursor"
            );
            cursor = Some(next.to_owned());
        }
    }

    /// Bind provider permission callbacks before starting or resuming a conversation.
    pub async fn set_execution_owner(&mut self, owner: crate::provider::ExecutionOwner) {
        self.request_executor
            .lock()
            .await
            .set_owner(Some(owner.clone()));
        if self.execution_owner.as_ref() != Some(&owner) {
            self.approval.clear();
        }
        self.execution_owner = Some(owner);
    }

    /// Update request context and reset control-call identity before reusing this process.
    pub async fn set_request_context(
        &mut self,
        workspace: &str,
        approval_mode: ApprovalMode,
        event_sink: Option<ProviderEventSink>,
    ) -> Result<()> {
        self.workspace = workspace.to_owned();
        self.approval_mode = approval_mode;
        self.normalization.bind(
            event_sink
                .as_ref()
                .map(|sink| sink.invocation_scope().cloned())
                .transpose()?,
        );
        self.event_sink = event_sink;
        self.requests_cancelled = false;
        self.control_executor = None;
        self.accepted_control_invocation_key_set.clear();
        self.mcp.clear_calls();
        self.approval.clear();
        self.request_executor.lock().await.reset(
            workspace.to_owned(),
            approval_mode,
            self.event_sink.clone(),
        );
        Ok(())
    }

    /// Isolate preparation traffic until the current provider turn is admitted.
    pub fn set_activity_publication(&mut self, enabled: bool) {
        self.activity_publication = enabled;
    }

    /// Keep approval targets and timeline tool names in the shared resource namespace.
    pub(super) fn set_mcp_bindings(&mut self, binding: HashMap<String, String>) {
        self.mcp.set_bindings(binding);
    }

    /// Seed provider-visible control validation from one broker-owned turn snapshot.
    pub async fn set_control_context(&mut self, context: ControlTurnContext) {
        self.planning_feedback = context.planning_feedback;
        self.control_executor = Some(self.request_executor.lock().await.configure(context));
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
        output: &mut ProviderOutput,
    ) -> Result<Value> {
        let id = self.send_request(method, params).await?;
        loop {
            let message = self.read_message(output).await?;
            if let Some(result) = protocol::request_result(&message, id, method) {
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
    pub async fn read_message(&mut self, output: &mut ProviderOutput) -> Result<Value> {
        let message = self.receive_message().await?;
        self.publish_received(message, output).await
    }

    /// Reads a provider frame without starting delivery that a competing command can cancel.
    pub async fn receive_message(&mut self) -> Result<ReceivedMessage> {
        tokio::select! {
            biased;
            completed = self.request_tasks.join_next_with_id(), if !self.request_tasks.is_empty() => {
                let (task_id, result) = completed.context("provider request worker disappeared")?
                    .context("provider request worker failed")?;
                self.pending_requests.remove(&task_id);
                Ok(ReceivedMessage::Request(Box::new(result)))
            }
            message = protocol::receive(&mut self.socket, self.trace.as_ref(), &self.session_id) => {
                message.map(ReceivedMessage::Provider)
            }
        }
    }

    /// Applies completed request effects without stopping the provider frame reader.
    pub async fn publish_received(
        &mut self,
        received: ReceivedMessage,
        output: &mut ProviderOutput,
    ) -> Result<Value> {
        match received {
            ReceivedMessage::Provider(message) => self.publish_message(message, output).await,
            ReceivedMessage::Request(result) => {
                let message = result.message.clone();
                self.complete_provider_request(*result, output).await?;
                if self.activity_publication
                    && !rejects_question_resolution(
                        self.planning_feedback,
                        message["method"].as_str().unwrap_or_default(),
                        &message,
                    )
                {
                    self.normalization
                        .normalize(
                            &message,
                            output,
                            self.event_sink.as_ref(),
                            &self.workspace,
                            false,
                        )
                        .await?;
                }
                Ok(message)
            }
        }
    }

    /// Cancels local request waits before releasing the provider execution owner.
    pub async fn cancel_requests(&mut self, output: &mut ProviderOutput) -> Result<()> {
        self.cancel_request_work();
        self.collect_requests(output).await
    }

    /// Revokes local tool authority without delaying the native interrupt message.
    pub(super) fn cancel_request_work(&mut self) {
        self.requests_cancelled = true;
        self.approval.clear();
        if let Some(executor) = &self.control_executor {
            executor.cancel();
        }
        self.control_completion = None;
        self.request_tasks.abort_all();
    }

    /// Publishes completed results and collects cancelled callbacks after signalling the provider.
    pub(super) async fn collect_requests(&mut self, output: &mut ProviderOutput) -> Result<()> {
        while let Some(completed) = self.request_tasks.join_next_with_id().await {
            match completed {
                Ok((task_id, result)) => {
                    self.pending_requests.remove(&task_id);
                    self.complete_provider_request(result, output).await?;
                }
                Err(error) if error.is_cancelled() => {}
                Err(error) => {
                    return Err(error).context("provider request worker failed during cleanup");
                }
            }
        }
        let pending = std::mem::take(&mut self.pending_requests);
        for (_, message) in pending {
            self.write_message(&json!({"jsonrpc":"2.0", "id":message["id"],
                "error":{"code":-32001,"message":"Provider execution was cancelled"}}))
                .await?;
        }
        if let Some(executor) = &self.control_executor {
            executor.close().await;
        }
        Ok(())
    }

    /// Publishes one consumed frame completely before processing another provider command.
    pub async fn publish_message(
        &mut self,
        mut message: Value,
        output: &mut ProviderOutput,
    ) -> Result<Value> {
        self.mcp.normalize(&mut message);
        if self.publish_background(&message, output).await? {
            return Ok(message);
        }
        self.approval.observe(&message);
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        if !self.activity_publication && message.get("id").is_some() && !method.is_empty() {
            return Ok(message);
        }
        let rejects_question_resolution =
            rejects_question_resolution(self.planning_feedback, method, &message);
        if message.get("id").is_some() && message.get("method").is_some() {
            if self.requests_cancelled {
                self.write_message(&json!({"jsonrpc":"2.0", "id":message["id"],
                    "error":{"code":-32001,"message":"Provider execution was cancelled"}}))
                    .await?;
                return Ok(message);
            }
            if self
                .pending_requests
                .values()
                .any(|pending| pending["id"] == message["id"])
            {
                anyhow::ensure!(
                    self.pending_requests
                        .values()
                        .any(|pending| pending == &message),
                    "provider reused a pending request ID with different arguments"
                );
                return Ok(message);
            }
            let executor = self.request_executor.clone();
            if method == "item/tool/call" && !rejects_question_resolution {
                self.normalization
                    .normalize(
                        &message,
                        output,
                        self.event_sink.as_ref(),
                        &self.workspace,
                        false,
                    )
                    .await?;
            }
            let request = message.clone();
            let approval = method != "item/tool/call";
            let ordered = if approval {
                None
            } else {
                let (completed, next) = tokio::sync::oneshot::channel();
                let previous = self.control_completion.replace(next);
                Some((previous, completed))
            };
            let mut independent = RequestExecutor::new(
                self.workspace.clone(),
                self.approval_mode,
                self.permission_coordinator.clone(),
                self.event_sink.clone(),
                self.trace.clone(),
                self.session_id.clone(),
            );
            independent.set_owner(self.execution_owner.clone());
            if approval {
                independent.set_approval(self.approval.prepare(&message));
            }
            let received_at = Instant::now();
            let task = self.request_tasks.spawn(async move {
                if let Some((previous, completed)) = ordered {
                    if let Some(previous) = previous
                        && previous.await.is_err()
                    {
                        return ExecutedRequest::cancelled(request);
                    }
                    let mut result = executor.lock().await.execute(request, received_at).await;
                    result.control_completion = Some(completed);
                    result
                } else {
                    independent.execute(request, received_at).await
                }
            });
            self.pending_requests.insert(task.id(), message.clone());
            return Ok(message);
        }
        if self.activity_publication && !rejects_question_resolution {
            self.normalization
                .normalize(
                    &message,
                    output,
                    self.event_sink.as_ref(),
                    &self.workspace,
                    false,
                )
                .await?;
        }
        Ok(message)
    }

    /// Collect provider events until a named terminal notification arrives.
    pub async fn read_until(
        &mut self,
        terminal_method: &str,
        output: &mut ProviderOutput,
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
            .record_ref(&self.session_id, "model.sent", message);
        self.socket
            .send(encoded)
            .await
            .map_err(|error| crate::provider::ProviderError::from(error).into())
    }

    async fn complete_provider_request(
        &mut self,
        completed: ExecutedRequest,
        output: &mut ProviderOutput,
    ) -> Result<bool> {
        let ExecutedRequest {
            message,
            mut response,
            approval,
            trace_context,
            invocation,
            source_digests,
            has_runtime,
            execution_finished,
            control_completion,
        } = completed;
        if response.pointer("/result/decision").and_then(Value::as_str) == Some("accept")
            && let Some(approval) = approval
            && let Err(error) = approval.validate()
        {
            response = json!({"jsonrpc":"2.0","id":message["id"],
                "error":{"code":-32001,"message":format!("{error:#}")}});
        }
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
        write_result?;
        if response_success
            && matches!(
                control_tool_name(
                    message
                        .get("method")
                        .and_then(Value::as_str)
                        .unwrap_or_default(),
                    message.get("params").unwrap_or(&message)
                ),
                Some("plan_phase_done")
            )
            && execution_finished
        {
            let params = message.get("params").unwrap_or(&message);
            if let (Some(thread), Some(turn)) = (
                params.get("threadId").and_then(Value::as_str),
                params.get("turnId").and_then(Value::as_str),
            ) {
                self.send_request("turn/interrupt", json!({"threadId":thread,"turnId":turn}))
                    .await?;
            }
        }
        if self.activity_publication
            && should_apply_control_request_semantics(
                &message,
                &mut self.accepted_control_invocation_key_set,
                response_success,
            )
        {
            if has_runtime {
                if let Some(invocation) = invocation {
                    apply_invocation(&invocation, &mut output.completion)?;
                }
            } else {
                apply_control_request_result(&message, output, response_success)?;
            }
            if let (Some(digests), Some(crate::plan::DesignEditRequest::File(request))) =
                (source_digests, output.completion.design_patch.last_mut())
            {
                request.source_digests = digests;
            }
        }
        if let Some(completed) = control_completion {
            let _ = completed.send(());
        }
        Ok(response_success)
    }

    #[cfg(test)]
    async fn respond_to_provider_request(&mut self, message: &Value) -> Result<bool> {
        let result = self
            .request_executor
            .lock()
            .await
            .execute(message.clone(), Instant::now())
            .await;
        self.complete_provider_request(result, &mut ProviderOutput::default())
            .await
    }

    #[cfg(test)]
    async fn finish_requests(&mut self, output: &mut ProviderOutput) -> Result<()> {
        while !self.request_tasks.is_empty() {
            let message = self.receive_message().await?;
            self.publish_received(message, output).await?;
        }
        Ok(())
    }
}

fn apply_control_request_result(
    message: &Value,
    output: &mut ProviderOutput,
    response_success: bool,
) -> Result<()> {
    if !response_success {
        return Ok(());
    }
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let params = message.get("params").unwrap_or(&Value::Null);
    let Some(invocation) = control_invocation(method, params)? else {
        return Ok(());
    };
    Ok(apply_invocation(&invocation, &mut output.completion)?)
}

fn should_apply_control_request_semantics(
    message: &Value,
    accepted_control_invocation_key_set: &mut HashSet<String>,
    response_success: bool,
) -> bool {
    response_success
        && control_invocation_key(
            message
                .get("method")
                .and_then(Value::as_str)
                .unwrap_or_default(),
            message,
        )
        .is_none_or(|key| accepted_control_invocation_key_set.insert(key))
}

pub(super) fn rejects_question_resolution(
    planning_feedback: bool,
    method: &str,
    value: &Value,
) -> bool {
    let params = value.get("params").unwrap_or(value);
    planning_feedback
        && matches!(
            control_tool_name(method, params),
            Some("question_answer" | "question_withdraw")
        )
}

fn control_invocation_key(method: &str, value: &Value) -> Option<String> {
    let params = value.get("params").unwrap_or(value);
    control_tool_name(method, params)?;
    let call_id = pointer_string(
        params,
        &[
            "/item/id",
            "/itemId",
            "/item_id",
            "/toolCallId",
            "/tool_call_id",
            "/callId",
        ],
    )?;
    Some(json!([params.get("threadId"), params.get("turnId"), call_id]).to_string())
}

/// Decode only the owning thread's native goal lifecycle notification.
pub(super) fn native_goal_state(
    message: &Value,
    thread_id: &str,
) -> Option<crate::goal::GoalState> {
    use crate::goal::GoalState;
    if message.pointer("/params/threadId").and_then(Value::as_str) != Some(thread_id) {
        return None;
    }
    match message.get("method").and_then(Value::as_str)? {
        "thread/goal/cleared" => Some(GoalState::Cleared),
        "thread/goal/updated" => match message
            .pointer("/params/goal/status")
            .or_else(|| message.pointer("/params/status"))?
            .as_str()?
        {
            "active" => Some(GoalState::Active),
            "complete" => Some(GoalState::Complete),
            "paused" => Some(GoalState::Paused),
            "blocked" => Some(GoalState::Blocked),
            "usageLimited" => Some(GoalState::UsageLimited),
            "budgetLimited" => Some(GoalState::BudgetLimited),
            _ => None,
        },
        _ => None,
    }
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
            *name == "harness_plan_edit"
                || ControlToolRegistry
                    .definition_list()
                    .iter()
                    .any(|definition| definition.name == *name)
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

pub(super) fn control_invocation(
    method: &str,
    value: &Value,
) -> Result<Option<ControlToolInvocation>> {
    let Some(control_name) = control_tool_name(method, value) else {
        return Ok(None);
    };
    let arguments = find_control_arguments(value, control_name)
        .with_context(|| format!("{control_name} omitted its arguments object"))?;
    Ok(Some(ControlToolInvocation {
        task_permit: None,
        name: control_name.to_owned(),
        arguments,
    }))
}

#[cfg(test)]
mod test {
    /// Keep both directions' numeric IDs separate while rejecting malformed client responses.

    /// Distinguish missing history from permission, transient, and unrelated RPC failures.
    use crate::provider::{
        MessageAddress, MessageClassification, MessageRole, MessageUpdate, TextUpdate, ToolAddress,
        ToolOutcome, ToolStatus, ToolUpdate,
    };

    struct MessageRecord {
        address: MessageAddress,
        role: Option<MessageRole>,
        text: String,
        completed: bool,
    }

    fn message_records(events: &[ProviderEvent]) -> Vec<MessageRecord> {
        let mut records: Vec<MessageRecord> = Vec::new();
        for event in events {
            let ProviderEvent::Message { address, update } = event else {
                continue;
            };
            let index = records
                .iter()
                .position(|record| record.address == *address)
                .unwrap_or_else(|| {
                    records.push(MessageRecord {
                        address: address.clone(),
                        role: None,
                        text: String::new(),
                        completed: false,
                    });
                    records.len() - 1
                });
            let record = &mut records[index];
            match update {
                MessageUpdate::Started {
                    classification,
                    text,
                } => {
                    record.role = match classification {
                        MessageClassification::Known(role) => Some(*role),
                        MessageClassification::Unclassified => None,
                    };
                    record.text = text.clone();
                }
                MessageUpdate::Delta { text } => record.text.push_str(text),
                MessageUpdate::Completed { text } => {
                    record.text = text.clone();
                    record.completed = true;
                }
                MessageUpdate::Classified { role } => record.role = Some(*role),
                MessageUpdate::Interrupted => record.completed = true,
            }
        }
        records
    }

    fn message_text(events: &[ProviderEvent], index: usize) -> Option<String> {
        message_records(events)
            .get(index)
            .map(|record| record.text.clone())
    }

    fn message_id(events: &[ProviderEvent], index: usize) -> Option<String> {
        message_records(events)
            .get(index)
            .map(|record| record.address.message_id.as_str().into())
    }

    fn message_role(events: &[ProviderEvent], index: usize) -> Option<MessageRole> {
        message_records(events)
            .get(index)
            .and_then(|record| record.role)
    }

    fn tool_records(events: &[ProviderEvent]) -> Vec<CodexToolSnapshot> {
        let mut records: Vec<(ToolAddress, CodexToolSnapshot)> = Vec::new();
        for event in events {
            let ProviderEvent::Tool { address, update } = event else {
                continue;
            };
            let index = records
                .iter()
                .position(|(existing, _)| existing == address)
                .unwrap_or_else(|| {
                    records.push((
                        address.clone(),
                        CodexToolSnapshot {
                            id: address.tool_id.to_string(),
                            kind: ToolActivityKind::ToolCall,
                            name: String::new(),
                            arguments: Value::Null,
                            title: String::new(),
                            output: None,
                            status: None,
                            change: ProviderChangeSet::default(),
                            output_delta: false,
                        },
                    ));
                    records.len() - 1
                });
            let record = &mut records[index].1;
            match update {
                ToolUpdate::Requested { invocation } => {
                    record.kind = invocation.kind;
                    record.name = invocation.name.clone();
                    record.arguments = invocation.arguments.clone();
                    record.title = invocation.title.clone();
                }
                ToolUpdate::TitleChanged { title } => record.title.clone_from(title),
                ToolUpdate::Started if record.status.is_none() => {
                    record.status = Some(ToolStatus::Running)
                }
                ToolUpdate::Output {
                    update: TextUpdate::Append(text),
                } => record.output.get_or_insert_default().push_str(text),
                ToolUpdate::Output {
                    update: TextUpdate::Replace(text),
                } => record.output = Some(text.clone()),
                ToolUpdate::Changes { changes } => record.change = changes.clone(),
                ToolUpdate::Finished { outcome } => {
                    record.status = Some(match outcome {
                        ToolOutcome::Succeeded => ToolStatus::Succeeded,
                        ToolOutcome::Failed { .. } => ToolStatus::Failed,
                        ToolOutcome::Denied { .. } => ToolStatus::Denied,
                        ToolOutcome::Cancelled => ToolStatus::Cancelled,
                        ToolOutcome::Interrupted => ToolStatus::Interrupted,
                    })
                }
                _ => {}
            }
        }
        records.into_iter().map(|(_, record)| record).collect()
    }

    fn record_count(events: &[ProviderEvent]) -> usize {
        let mut identities = HashSet::new();
        for event in events {
            let identity = match event {
                ProviderEvent::Message { address, .. } => format!("message:{address:?}"),
                ProviderEvent::Tool { address, .. } => format!("tool:{address:?}"),
                ProviderEvent::Compaction { scope, update } => {
                    format!("compaction:{scope:?}:{}:{}", update.thread_id, update.id)
                }
                ProviderEvent::Checklist { address, update } => {
                    format!("checklist:{address:?}:{}", update.scope_id)
                }
                _ => format!("event:{event:?}"),
            };
            identities.insert(identity);
        }
        identities.len()
    }

    #[tokio::test]
    async fn manual_compaction_preserves_operation_scope_without_message_admission() -> Result<()> {
        let scope = crate::provider::EventScope::SessionOperation {
            owner: crate::provider::ExecutionOwner::root("session"),
            operation_id: "compact".into(),
        };
        let mut normalization = super::super::normalization::CodexNormalization::default();
        normalization.bind(Some(scope.clone()));
        let mut output = ProviderOutput::default();
        for (method, item) in [
            (
                "item/started",
                json!({"type":"contextCompaction","id":"compact"}),
            ),
            (
                "item/completed",
                json!({"type":"agentMessage","id":"ignored","phase":"final_answer","text":"Not a response"}),
            ),
            (
                "item/completed",
                json!({"type":"contextCompaction","id":"compact"}),
            ),
        ] {
            normalization
                .normalize(
                    &json!({"method":method,"params":{"threadId":"native","item":item}}),
                    &mut output,
                    None,
                    "",
                    false,
                )
                .await?;
        }
        assert_eq!(output.event.len(), 2);
        assert!(output.event.iter().all(
            |event| matches!(event, ProviderEvent::Compaction { update, .. } if update.manual)
                && event.scope() == scope
        ));
        Ok(())
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn selected_checks_replay_before_later_design_edits() -> anyhow::Result<()> {
        use super::*;
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        std::fs::write(
            fixture.path().join(".forge.json"),
            json!({"checks":{"manual":{"trigger":{"auto":false},"action":{"command":"print selected","label":"Manual","category":"test"}}}})
            .to_string(),
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for id in 1..=3 {
                let response: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(response["id"], id);
                assert_eq!(response["result"]["success"], true, "{response}");
            }
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            PermissionCoordinator::transient(&workspace)?,
            None,
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.normalization.bind(Some(fixture_scope()));
        let mut document = crate::plan::test_fixture("plan", "Initial");
        document.version = 4;
        document.design = Some(crate::plan::DeclarationDesign::default());
        let mut context = ControlTurnContext::inactive(crate::provider::PromptMode::Plan);
        context.plan_state = Some(crate::plan::PlanState::Revising);
        context.plan_document = Some(document.clone());
        context.workspace_root = Some(fixture.path().to_owned());
        process.set_control_context(context).await;
        let mut output = ProviderOutput::default();
        for (id, tool, arguments) in [
            (
                1,
                "plan_overview_edit",
                json!({"plan_id":"plan","expected_version":4,"checks":["manual"]}),
            ),
            (
                2,
                "plan_overview_edit",
                json!({"plan_id":"plan","expected_version":5,"objective":"Restart fixture"}),
            ),
            (
                3,
                "plan_overview_edit",
                json!({"plan_id":"plan","expected_version":6,"checks":["manual"]}),
            ),
        ] {
            process.publish_message(json!({"id":id,"method":"item/tool/call","params":{
                "threadId":"thread","turnId":"turn","callId":format!("call-{id}"),"tool":tool,"arguments":arguments
            }}), &mut output).await?;
        }
        process.finish_requests(&mut output).await?;
        assert_eq!(output.completion.design_patch.len(), 2);
        for patch in output.completion.design_patch {
            document = document.patch_design(fixture.path(), patch)?;
        }
        assert_eq!(document.version, 6);
        assert_eq!(
            document.design.as_ref().unwrap().document.checks,
            ["manual"]
        );
        assert_eq!(
            document.design.as_ref().unwrap().document.objective,
            "Restart fixture"
        );
        tokio::time::timeout(Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[tokio::test]
    async fn shell_parse_errors_reach_provider_and_corrected_calls_reuse_connection() -> Result<()>
    {
        use super::*;
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let failure: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(failure["id"], 1);
            assert!(
                failure["error"]["message"]
                    .as_str()
                    .unwrap()
                    .contains("shell_command_parse_error")
            );
            assert!(failure.get("result").is_none());
            let inner_failure: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(inner_failure["id"], 2);
            assert!(
                inner_failure["error"]["message"]
                    .as_str()
                    .unwrap()
                    .contains("shell_command_parse_error")
            );
            let corrected: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(corrected["id"], 3);
            assert_eq!(corrected["result"]["decision"], "accept");
        });
        let coordinator = PermissionCoordinator::transient(&workspace)?;
        let (sink, mut stream) = crate::provider::events::channel(fixture_scope());
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Allow,
            coordinator.clone(),
            Some(sink),
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.normalization.bind(Some(fixture_scope()));
        for (id, command, accepted) in [
            (1, "pwsh -c 'unfinished".to_owned(), false),
            (
                2,
                shlex::try_join(["pwsh", "-c", "probe 'unfinished"]).unwrap(),
                false,
            ),
            (
                3,
                shlex::try_join(["pwsh", "-c", r#"probe "timeline|markdown|table|wrap""#]).unwrap(),
                true,
            ),
        ] {
            let message = json!({"id":id, "method":"item/commandExecution/requestApproval",
                "params":{"threadId":"thread","turnId":"turn","command":command,"shell":"powershell"}});
            assert_eq!(
                process.respond_to_provider_request(&message).await?,
                accepted
            );
            assert!(coordinator.pending_list()?.is_empty());
            let event = stream.try_recv();
            if id == 2 {
                let event = event?;
                assert_eq!(event.scope(), fixture_scope().into());
                assert!(
                    matches!(event, ProviderEvent::Diagnostic { diagnostic, .. } if diagnostic.message.contains("shell_command_parse_error"))
                );
                assert!(stream.try_recv().is_err());
            } else {
                assert!(event.is_err(), "{event:?}");
            }
        }
        tokio::time::timeout(Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[test]
    fn every_registered_dynamic_tool_reaches_control_dispatch() {
        for definition in crate::tools::ControlToolRegistry.definition_list() {
            let request = serde_json::json!({"tool":definition.name,"arguments":{}});
            let invocation = super::control_invocation("item/tool/call", &request)
                .unwrap()
                .unwrap();
            assert_eq!(invocation.name, definition.name);
        }
    }
    #[test]
    fn mcp_approval_uses_current_metadata_and_canonical_tool_identity() {
        use super::*;
        let mut context = McpContext::default();
        context.set_bindings(HashMap::from([(
            "native_alias".into(),
            "shared-server".into(),
        )]));
        let mut started = json!({"method":"item/started", "params":{
            "threadId":"thread", "turnId":"turn", "item":{
                "type":"mcpToolCall", "id":"call", "server":"native_alias", "tool":"verify"
            }
        }});
        context.normalize(&mut started);
        assert_eq!(started["params"]["item"]["server"], "shared-server");
        let request = json!({"method":"mcpServer/elicitation/request", "params":{
            "threadId":"thread", "turnId":"turn", "serverName":"native_alias",
            "_meta":{"codex_approval_kind":"mcp_tool_call"}
        }});
        let mut normalized = request.clone();
        context.normalize(&mut normalized);
        assert_eq!(
            normalized["params"]["meta"]["codex_approval_kind"],
            "mcp_tool_call"
        );
        assert_eq!(
            permission_from_provider("mcpServer/elicitation/request", &normalized, ".")
                .unwrap()
                .target_list,
            vec![crate::permissions::matcher::PermissionTarget::Mcp {
                target: "shared-server/verify".into()
            }]
        );
        let mut completed = started.clone();
        completed["method"] = json!("item/completed");
        context.normalize(&mut completed);
        let mut later = request;
        context.normalize(&mut later);
        assert!(
            later.pointer("/params/meta/toolName").is_none(),
            "completed calls cannot authorize later tools"
        );
    }

    #[tokio::test]
    async fn compaction_lifecycle_is_neither_assistant_text_nor_a_tool() {
        let mut normalization = fixture_normalization();
        let mut output = super::ProviderOutput::default();
        for method in [
            "item/started",
            "item/completed",
            "item/started",
            "item/completed",
        ] {
            super::normalize_event(
                &mut normalization,
                &serde_json::json!({
                    "method":method, "params":{"threadId":"thread", "turnId":"turn",
                        "item":{"type":"contextCompaction","id":"compact"}}
                }),
                &mut output,
                None,
            )
            .await;
        }
        assert_eq!(record_count(&output.event), 1);
        let ProviderEvent::Compaction {
            update: compaction, ..
        } = output.event.last().unwrap()
        else {
            panic!("expected compaction")
        };
        assert_eq!(
            compaction.state,
            crate::provider::CompactionState::Completed
        );
        assert_eq!(compaction.result.summary_content, None);
        assert!(!matches!(output.event[0], ProviderEvent::Tool { .. }));
        assert!(!matches!(output.event[0], ProviderEvent::Message { .. }));
    }

    #[tokio::test]
    async fn passive_catalog_connection_does_not_answer_broadcast_controls() -> anyhow::Result<()> {
        use super::*;
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server =
            tokio::spawn(async move {
                let (stream, _) = listener.accept().await.unwrap();
                let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "thread/backgroundTerminals/list");
                socket.send(Message::Text(json!({
                "id": 90, "method": "item/tool/call",
                "params": {"threadId":"thread", "turnId":"turn", "callId":"phase",
                    "tool":"plan_phase_done",
                    "arguments":{"phase":"implement","revision":1,"summary":"Implemented"}}
            }).to_string().into())).await.unwrap();
                socket
                    .send(Message::Text(
                        json!({
                            "id": request["id"], "result":{"terminals":[]}
                        })
                        .to_string()
                        .into(),
                    ))
                    .await
                    .unwrap();
                assert!(
                    tokio::time::timeout(Duration::from_millis(100), socket.next())
                        .await
                        .is_err(),
                    "a catalog connection must leave broadcast controls to the turn owner"
                );
            });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            PermissionCoordinator::transient(&workspace)?,
            None,
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.normalization.bind(Some(fixture_scope()));
        process.set_activity_publication(false);
        let mut output = ProviderOutput::default();
        process
            .request(
                "thread/backgroundTerminals/list",
                json!({"threadId":"thread"}),
                &mut output,
            )
            .await?;
        tokio::time::timeout(Duration::from_secs(2), server).await??;
        let failure = process
            .request_executor
            .lock()
            .await
            .execute(
                json!({
                    "id":91, "method":"item/tool/call", "params":{"tool":"plan_phase_done",
                        "arguments":{"phase":"implement","revision":1,"summary":"Implemented"}}
                }),
                Instant::now(),
            )
            .await;
        assert_eq!(failure.response["result"]["success"], false);
        assert!(
            failure
                .response
                .to_string()
                .contains("active execution control context")
        );
        Ok(())
    }

    #[tokio::test]
    async fn native_goal_state_ignores_child_and_unknown_notifications() {
        let mut normalization = fixture_normalization();
        use crate::goal::GoalState;
        let mut output = crate::provider::ProviderOutput {
            event: Vec::new(),
            completion: crate::provider::ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some("parent".into()),
                    ..Default::default()
                },
                ..Default::default()
            },
        };
        for (thread, status, expected) in [
            ("child", "blocked", None),
            ("parent", "unknown", None),
            ("parent", "usageLimited", Some(GoalState::UsageLimited)),
            ("child", "complete", Some(GoalState::UsageLimited)),
            ("parent", "active", Some(GoalState::Active)),
            ("parent", "budgetLimited", Some(GoalState::BudgetLimited)),
        ] {
            super::normalize_event(
                &mut normalization,
                &serde_json::json!({
                    "method":"thread/goal/updated", "params":{"threadId":thread,"status":status}
                }),
                &mut output,
                None,
            )
            .await;
            assert_eq!(output.completion.evidence.native_state, expected);
        }
    }

    use super::*;

    #[tokio::test]
    async fn reasoning_snapshots_reconcile_sections_without_losing_or_duplicating_text() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        for message in [
            json!({"method":"item/started","params":{"threadId":"thread","turnId":"turn",
                "item":{"id":"reasoning","type":"reasoning","summary":[],"content":[]}}}),
            json!({"method":"item/reasoning/summaryTextDelta","params":{"threadId":"thread","turnId":"turn",
                "itemId":"reasoning","summaryIndex":0,"delta":"Partial"}}),
            json!({"method":"item/reasoning/summaryPartAdded","params":{"threadId":"thread","turnId":"turn",
                "itemId":"reasoning","summaryIndex":1}}),
            json!({"method":"item/completed","params":{"threadId":"thread","turnId":"turn",
                "item":{"id":"reasoning","type":"reasoning","summary":["Corrected first section","Snapshot only section"],
                    "content":["raw reasoning must not become a summary"]}}}),
        ] {
            normalize_event(&mut normalization, &message, &mut output, None).await;
        }
        assert_eq!(record_count(&output.event), 3);
        assert_eq!(
            message_text(&output.event, 0).as_deref(),
            Some("Corrected first section")
        );
        assert_eq!(
            message_text(&output.event, 1).as_deref(),
            Some("Snapshot only section")
        );
        assert_eq!(
            message_id(&output.event, 0).as_deref(),
            Some("reasoning:summary:0")
        );
        assert_eq!(
            message_id(&output.event, 1).as_deref(),
            Some("reasoning:summary:1")
        );
        assert!(
            message_records(&output.event)
                .iter()
                .all(|message| message.completed)
        );
        assert_eq!(message_role(&output.event, 2), Some(MessageRole::Thought));
    }

    #[tokio::test]
    async fn dynamic_calls_retain_arguments_all_output_and_failure_status() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        for method in ["item/started", "item/completed"] {
            normalize_event(&mut normalization, &json!({"method":method,"params":{"threadId":"thread","turnId":"turn",
                "item":{"id":"dynamic","type":"dynamicToolCall","tool":"fixture_read",
                    "namespace":"fixture","arguments":"{\"path\":\"README.md\",\"api_key\":\"secret\"}",
                    "status":if method == "item/started" { "inProgress" } else { "completed" },
                    "success":if method == "item/completed" { json!(false) } else { Value::Null },
                    "contentItems":[{"type":"inputText","text":"first block"},{"type":"inputText","text":"second block"}]}}}),
                &mut output, None).await;
        }
        assert_eq!(record_count(&output.event), 1);
        let activity = &tool_records(&output.event)[0];
        assert_eq!(
            activity.title,
            "fixture.fixture_read({\"api_key\":\"[REDACTED]\",\"path\":\"README.md\"})"
        );
        assert_eq!(
            activity.output.as_deref(),
            Some("first block\nsecond block")
        );
        assert_eq!(activity.status.as_deref(), Some("failed"));
    }

    #[tokio::test]
    async fn separates_readable_reasoning_summaries_from_raw_reasoning() {
        let mut normalization = fixture_normalization();
        let event = |method: &str, summary_index: u64, delta: &str| {
            json!({
                "method": method,
                "params": {
                    "threadId": "thread",
                    "turnId": "turn",
                    "itemId": "reasoning-1",
                    "summaryIndex": summary_index,
                    "delta": delta,
                }
            })
        };
        let mut output = ProviderOutput::default();

        normalize_event_in_workspace(
            &mut normalization,
            &event("item/reasoning/summaryTextDelta", 0, "Inspecting "),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        normalize_event_in_workspace(
            &mut normalization,
            &event("item/reasoning/summaryTextDelta", 0, "ownership"),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        normalize_event_in_workspace(
            &mut normalization,
            &event("item/reasoning/textDelta", 0, "private chain of thought"),
            &mut output,
            None,
            "",
            false,
        )
        .await;

        assert_eq!(record_count(&output.event), 2);
        assert_eq!(
            message_role(&output.event, 0),
            Some(MessageRole::ThoughtSummary)
        );
        assert_eq!(
            message_text(&output.event, 0).as_deref(),
            Some("Inspecting ownership")
        );
        assert_eq!(
            message_id(&output.event, 0).as_deref(),
            Some("reasoning-1:summary:0")
        );
        assert_eq!(message_role(&output.event, 1), Some(MessageRole::Thought));
        assert_eq!(
            message_text(&output.event, 1).as_deref(),
            Some("private chain of thought")
        );
    }

    #[tokio::test]
    async fn lifecycle_events_cannot_poison_a_corrected_control_request() {
        let mut normalization = fixture_normalization();
        let invocation = |method: &str, overview: &str| {
            json!({
                "method": method,
                "params": {
                    "item": {
                        "id": overview,
                        "type": "dynamicToolCall",
                        "tool": "plan_file_edit",
                        "arguments": {
                            "plan_id": "plan-1",
                            "expected_version": 1,
                            "patch": overview
                        }
                    }
                }
            })
        };
        let request = |overview: &str| {
            let mut request = invocation("item/started", overview);
            request["id"] = Value::String(overview.to_owned());
            request["method"] = Value::String("item/tool/call".into());
            request
        };
        let mut output = ProviderOutput::default();

        normalize_event_in_workspace(
            &mut normalization,
            &invocation("item/started", "rejected"),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        normalize_event_in_workspace(
            &mut normalization,
            &invocation("item/completed", "rejected"),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        assert!(output.completion.design_patch.is_empty());

        normalize_event_in_workspace(
            &mut normalization,
            &invocation("item/started", "corrected"),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        apply_control_request_result(&request("rejected"), &mut output, false).unwrap();
        assert!(output.completion.design_patch.is_empty());
        apply_control_request_result(&request("corrected"), &mut output, true).unwrap();
        normalize_event_in_workspace(
            &mut normalization,
            &invocation("item/completed", "corrected"),
            &mut output,
            None,
            "",
            false,
        )
        .await;

        assert_eq!(output.completion.design_patch.len(), 1);
        let crate::plan::DesignEditRequest::File(request) = &output.completion.design_patch[0]
        else {
            panic!("expected file edit")
        };
        assert_eq!(request.patch, "corrected");
    }

    #[tokio::test]
    async fn accepted_submit_remains_authoritative_across_lifecycle_replays() {
        let mut normalization = fixture_normalization();
        let lifecycle = |method: &str| {
            json!({
                "method": method,
                "params": {
                    "item": {
                        "id": "submit-1",
                        "type": "dynamicToolCall",
                        "tool": "plan_submit",
                        "arguments": {
                            "plan_id": "plan-1",
                            "expected_version": 5
                        }
                    }
                }
            })
        };
        let request = json!({
            "id": 42,
            "method": "item/tool/call",
            "params": {
                "itemId": "submit-1",
                "tool": "plan_submit",
                "arguments": {
                    "plan_id": "plan-1",
                    "expected_version": 5
                }
            }
        });
        let mut output = ProviderOutput::default();

        normalize_event_in_workspace(
            &mut normalization,
            &lifecycle("item/started"),
            &mut output,
            None,
            "",
            false,
        )
        .await;
        assert!(output.completion.plan_submit.is_none());

        apply_control_request_result(&request, &mut output, true).unwrap();
        normalize_event_in_workspace(
            &mut normalization,
            &lifecycle("item/completed"),
            &mut output,
            None,
            "",
            false,
        )
        .await;

        let submit = output
            .completion
            .plan_submit
            .expect("accepted submit request");
        assert_eq!(submit.plan_id, "plan-1");
        assert_eq!(submit.expected_version, 5);
    }

    #[test]
    fn accepted_control_request_identity_ignores_rejections_and_deduplicates_success() {
        let request = json!({
            "id": 42,
            "method": "item/tool/call",
            "params": {
                "itemId": "edit-1",
                "tool": "harness_plan_edit",
                "arguments": {
                    "plan_id": "plan-1",
                    "expected_version": 1,
                    "plan": { "overview": "corrected" }
                }
            }
        });
        let mut accepted_control_invocation_key_set = HashSet::new();

        assert!(!should_apply_control_request_semantics(
            &request,
            &mut accepted_control_invocation_key_set,
            false
        ));
        assert!(should_apply_control_request_semantics(
            &request,
            &mut accepted_control_invocation_key_set,
            true
        ));
        assert!(!should_apply_control_request_semantics(
            &request,
            &mut accepted_control_invocation_key_set,
            true
        ));
        let mut next_request = request.clone();
        next_request["params"]["itemId"] = json!("edit-2");
        assert!(should_apply_control_request_semantics(
            &next_request,
            &mut accepted_control_invocation_key_set,
            true
        ));
    }

    #[tokio::test]
    async fn extracts_structured_questions_from_dynamic_tool_arguments() {
        let mut output = ProviderOutput::default();
        apply_control_request_result(
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "type": "dynamicToolCall",
                        "tool": "question_ask",
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
            true,
        ).unwrap();
        let question = output
            .completion
            .plan_question
            .expect("structured planning question");
        assert_eq!(question.questions[0].header, "Migration");
        assert_eq!(question.questions[0].options[0].label, "Staged");
        assert!(output.event.is_empty());
    }

    #[tokio::test]
    async fn ignores_removed_answer_tool_and_records_withdrawal_without_rendering_tools() {
        let mut answer_output = ProviderOutput::default();
        apply_control_request_result(
            &json!({
                "method": "item/completed",
                "params": { "item": {
                    "type": "dynamicToolCall",
                    "tool": "question_answer",
                    "arguments": {
                        "question_id": "migration",
                        "response": { "kind": "selected", "option": "Staged" }
                    }
                } }
            }),
            &mut answer_output,
            true,
        )
        .unwrap();
        assert!(
            answer_output.completion.question_answer.is_none(),
            "only user input may answer a question"
        );
        assert!(answer_output.event.is_empty());

        let mut withdrawal_output = ProviderOutput::default();
        apply_control_request_result(
            &json!({
                "method": "item/completed",
                "params": { "item": {
                    "type": "dynamicToolCall",
                    "tool": "question_withdraw",
                    "arguments": { "question_id":"migration", "revision":1, "reason": "Repository policy determines the choice." }
                } }
            }),
            &mut withdrawal_output,
            true,
        )
        .unwrap();
        assert_eq!(
            withdrawal_output
                .completion
                .question_withdrawal
                .unwrap()
                .reason,
            "Repository policy determines the choice."
        );
        assert!(withdrawal_output.event.is_empty());
    }

    #[tokio::test]
    async fn replaces_task_state_across_lifecycle_events_without_creating_an_artifact() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
            &json!({
                "method": "turn/plan/updated",
                "params": { "plan": [{ "step": "Inspect the change" }] }
            }),
            &mut output,
            None,
        )
        .await;
        normalize_event(
            &mut normalization,
            &json!({
                "method": "turn/plan/completed",
                "params": { "status": "completed" }
            }),
            &mut output,
            None,
        )
        .await;
        assert!(output.completion.plan_edit.is_empty());
        assert_eq!(record_count(&output.event), 1);
        let ProviderEvent::Checklist { update, .. } = output.event.last().unwrap() else {
            panic!("expected checklist")
        };
        assert!(update.complete);
        assert_eq!(update.entry_list.len(), 1);
        assert_eq!(update.entry_list[0].content, "Inspect the change");
    }

    #[tokio::test]
    async fn ignores_control_tool_names_inside_command_output() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(&mut normalization,
            &json!({
                "method": "item/commandExecution/completed",
                "params": { "itemId":"command-doc", "output": "printed goal_complete as documentation" }
            }),
            &mut output,
            None,
        )
        .await;
        assert!(output.completion.evidence.tool_called);
        assert!(!output.completion.evidence.structured_complete);
    }

    #[tokio::test]
    async fn recognizes_exact_control_tool_calls() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(fixture_scope());
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/tool/call",
                "params": {
                    "callId":"complete-goal",
                    "name": "goal_complete",
                    "arguments": { "summary": "Complete" }
                }
            }),
            &mut output,
            Some(&event_sink),
        )
        .await;
        assert!(output.completion.evidence.structured_complete);
        assert_eq!(tool_records(&output.event).len(), 1);
        assert_eq!(tool_records(&output.event)[0].name, "goal_complete");
        assert!(
            std::iter::from_fn(|| event_stream.try_recv().ok()).any(|event| matches!(event, ProviderEvent::Tool { update: ToolUpdate::Requested { invocation }, .. } if invocation.name == "goal_complete"))
        );
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
                        "overview": "Updated overview",
                        "usage": null
                    }
                }
            }
        });

        let context = DynamicToolTraceContext::from_message(&message).unwrap();
        let payload = context.payload(json!({ "phase": "argument_validation" }));

        assert_eq!(context.invocation_id, "call-1");
        assert_eq!(context.mutation_count, 2);
        assert_eq!(context.mutation_summary["plan"]["patch"], 2);
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
                "name": "plan_submit",
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

    #[tokio::test]
    async fn repeated_control_calls_preserve_independent_tool_lifetimes() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            tokio_tungstenite::accept_async(stream).await.unwrap()
        });
        let (sink, mut stream) = crate::provider::events::channel(fixture_scope());
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            PermissionCoordinator::transient(&workspace)?,
            Some(sink),
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        process.normalization.bind(Some(fixture_scope()));
        let _server_socket = server.await?;
        let mut output = ProviderOutput::default();
        let mut turn = crate::turn::test_support::TestTurn::new(
            "turn".into(),
            crate::provider::TurnAddress {
                scope: fixture_scope(),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            0,
        );
        for (call_id, method, status, now_ms) in [
            ("read-1", "item/started", "inProgress", 1000),
            ("read-1", "item/completed", "failed", 2000),
            ("read-2", "item/started", "inProgress", 3000),
            ("read-2", "item/started", "inProgress", 4000),
            ("read-2", "item/completed", "completed", 5000),
            ("read-2", "item/completed", "completed", 6000),
        ] {
            process
                .publish_message(
                    json!({
                        "method": method,
                        "params": {
                            "threadId": "thread",
                            "turnId": "turn",
                            "item": {
                                "id": call_id,
                                "type": "dynamicToolCall",
                                "tool": "plan_overview_read",
                                "arguments": { "plan_id": "plan" },
                                "status": status
                            }
                        }
                    }),
                    &mut output,
                )
                .await?;
            while let Some(event) = stream.try_next()? {
                let ProviderEvent::Tool { address, update } = event else {
                    panic!("expected tool lifecycle")
                };
                assert_eq!(address.tool_id, call_id);
                turn.record_tool(address.tool_id.as_str(), update, now_ms)?;
            }
            assert_eq!(
                turn.tools().find(|tool| tool.id == call_id).unwrap().status,
                crate::provider::ToolStatus::parse(status).unwrap().as_str()
            );
        }
        let tool = turn.tools().collect::<Vec<_>>();
        assert_eq!(tool.len(), 2);
        for (call_id, started, completed) in [("read-1", 1000, 2000), ("read-2", 3000, 5000)] {
            let tool = tool.iter().find(|tool| tool.id == call_id).unwrap();
            assert_eq!(tool.started_at_ms, Some(started));
            assert_eq!(tool.completed_at_ms, Some(completed));
            assert_eq!(tool.elapsed_ms(9000), Some((completed - started) as u64));
        }
        assert!(output.event.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn renders_completed_control_calls_without_reapplying_their_semantic_effect() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
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
                    "plan": { "overview": "Updated overview" }
                }
            } }
        });

        normalize_event_in_workspace(&mut normalization, &completed, &mut output, None, "", false)
            .await;

        assert!(output.completion.plan_edit.is_empty());
        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.id, "plan-edit-1");
        assert_eq!(
            activity.title,
            "harness_plan_edit({\"expected_version\":1,\"plan\":{\"overview\":\"Updated overview\"},\"plan_id\":\"plan-1\"})"
        );
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(activity.output.as_deref(), Some("Plan edit accepted."));
    }

    #[test]
    fn gives_replayed_control_lifecycle_and_request_messages_one_identity() {
        let arguments = json!({
            "plan_id": "plan-id",
            "expected_version": 1,
            "plan": { "overview": "Updated overview" }
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
                "callId": "dynamic-tool-1",
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
                "name": "question_answer",
                "arguments": {
                    "question_id": "reader_mode",
                    "option_id": "preview_cli"
                }
            }
        });
        let withdraw = json!({
            "method": "item/tool/call",
            "params": {
                "name": "question_withdraw",
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

        assert!(
            !rejects_question_resolution(true, "item/tool/call", &answer),
            "removed answer tools are not executable controls"
        );
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

    #[tokio::test]
    async fn message_lifecycle_preserves_phase_identity_and_authoritative_text() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (sink, mut stream) = crate::provider::events::channel(fixture_scope());
        for (method, item) in [
            (
                "item/started",
                json!({"item":{"type":"agentMessage","id":"one","phase":"commentary","text":""}}),
            ),
            (
                "item/agentMessage/delta",
                json!({"itemId":"one","delta":"Checking "}),
            ),
            (
                "item/agentMessage/delta",
                json!({"itemId":"one","delta":"files"}),
            ),
            (
                "item/completed",
                json!({"item":{"type":"agentMessage","id":"one","phase":"commentary","text":"Checking files."}}),
            ),
            (
                "item/started",
                json!({"item":{"type":"agentMessage","id":"two","phase":"final_answer","text":""}}),
            ),
            (
                "item/agentMessage/delta",
                json!({"itemId":"two","delta":"Done"}),
            ),
            (
                "item/completed",
                json!({"item":{"type":"agentMessage","id":"two","phase":"final_answer","text":"Done."}}),
            ),
        ] {
            let mut params = item;
            params["threadId"] = json!("parent");
            params["turnId"] = json!("turn");
            normalize_event(
                &mut normalization,
                &json!({"method":method,"params":params}),
                &mut output,
                Some(&sink),
            )
            .await;
        }
        assert_eq!(record_count(&output.event), 2);
        assert_eq!(
            message_text(&output.event, 0).as_deref(),
            Some("Checking files.")
        );
        assert_eq!(
            message_role(&output.event, 0),
            Some(crate::provider::MessageRole::Commentary)
        );
        assert_eq!(message_text(&output.event, 1).as_deref(), Some("Done."));
        assert_eq!(message_id(&output.event, 1).as_deref(), Some("two"));
        assert!(
            message_records(&output.event)
                .iter()
                .all(|message| message.completed)
        );
        assert!(output.event.iter().all(|event| !matches!(event, ProviderEvent::Message { update: MessageUpdate::Started { text, .. }, .. } if text.is_empty())));
        let mut delta = Vec::new();
        while let Ok(event) = stream.try_recv() {
            if let ProviderEvent::Message { update, .. } = event {
                match update {
                    MessageUpdate::Started {
                        text,
                        classification: MessageClassification::Known(role),
                    } => delta.push((text, Some(role))),
                    MessageUpdate::Delta { text } => {
                        delta.push((text, Some(MessageRole::Commentary)))
                    }
                    _ => {}
                }
            }
        }
        assert_eq!(
            delta,
            vec![
                (
                    "Checking ".into(),
                    Some(crate::provider::MessageRole::Commentary)
                ),
                (
                    "files".into(),
                    Some(crate::provider::MessageRole::Commentary)
                ),
                ("Done".into(), Some(crate::provider::MessageRole::Response)),
            ]
        );
        // The same provider item ID in another agent must not inherit the parent's phase.
        normalize_event(
            &mut normalization,
            &json!({"method":"item/agentMessage/delta", "params":{
                "threadId":"child","turnId":"turn","itemId":"one","delta":"Child"
            }}),
            &mut output,
            None,
        )
        .await;
        assert_eq!(record_count(&output.event), 3);
        assert_eq!(message_role(&output.event, 2), None);
        let mut next_scope = fixture_scope();
        next_scope.invocation_id = "next-invocation".into();
        normalization.bind(Some(next_scope.clone()));
        normalize_event(
            &mut normalization,
            &json!({"method":"item/agentMessage/delta", "params":{
                "threadId":"parent","turnId":"turn","itemId":"one","delta":"Next invocation"
            }}),
            &mut output,
            None,
        )
        .await;
        assert_eq!(
            message_records(&output.event)[3].address.turn.scope,
            next_scope
        );
        assert_eq!(message_role(&output.event, 3), None);
    }

    #[tokio::test]
    async fn usage_retains_native_categories_and_streams_addressed_snapshots() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (sink, mut stream) = crate::provider::events::channel(fixture_scope());
        normalize_event(&mut normalization, &json!({"method":"thread/tokenUsage/updated", "params":{
            "threadId":"parent", "turnId":"turn", "tokenUsage":{
                "last":{"inputTokens":76000,"cachedInputTokens":68400,"outputTokens":4200,"reasoningOutputTokens":3000,"totalTokens":80200},
                "total":{"inputTokens":176000,"cachedInputTokens":148400,"outputTokens":14200,"reasoningOutputTokens":9000,"totalTokens":190200},
                "modelContextWindow":258400
            }
        }}), &mut output, Some(&sink)).await;
        let event = output
            .event
            .iter()
            .find(|event| matches!(event, ProviderEvent::Usage { .. }))
            .unwrap();
        let ProviderEvent::Usage {
            address,
            update: native,
        } = event
        else {
            unreachable!()
        };
        assert_eq!(address.turn_id, "turn");
        assert_eq!(native.usage.cached_percent(), Some(90));
        assert_eq!(native.usage.non_reasoning_output(), Some(1200));
        assert_eq!(native.cumulative.as_ref().unwrap().input, Some(176000));
        assert!(matches!(
            stream.try_recv().unwrap(),
            ProviderEvent::Usage { .. }
        ));
        assert!(output.event.iter().any(|event| matches!(
            event,
            ProviderEvent::Runtime {
                update: crate::provider::RuntimeUpdate::ContextUsage { .. },
                ..
            }
        )));
    }

    #[tokio::test]
    async fn coalesces_message_deltas_without_treating_prose_as_tools() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/agentMessage/delta",
                "params": { "delta": "I will call goal_complete " }
            }),
            &mut output,
            None,
        )
        .await;
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/agentMessage/delta",
                "params": { "delta": "after the plan finishes." }
            }),
            &mut output,
            None,
        )
        .await;
        assert_eq!(record_count(&output.event), 1);
        assert_eq!(
            message_text(&output.event, 0).as_deref(),
            Some("I will call goal_complete after the plan finishes.")
        );
        assert!(!output.completion.evidence.structured_complete);
        assert!(matches!(output.event[0], ProviderEvent::Message { .. }));
    }

    #[tokio::test]
    async fn ignores_codex_user_message_lifecycle_events() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;
        assert!(output.event.is_empty());
    }

    #[tokio::test]
    async fn keeps_different_command_call_ids_separate() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(fixture_scope());
        normalize_event(
            &mut normalization,
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
        )
        .await;
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/commandExecution/outputDelta",
                "params": { "itemId": "command-1", "delta": "# Harness\nline two\n" }
            }),
            &mut output,
            Some(&event_sink),
        )
        .await;
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert_eq!(
            std::iter::from_fn(|| event_stream.try_recv().ok()).count(),
            7,
            "each lifecycle update must stream live"
        );
        assert_eq!(
            record_count(&output.event),
            2,
            "different call IDs must remain separate even with identical commands"
        );
        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.id, "command-1");
        assert_eq!(activity.title, "Get-Content README.md");
        assert_eq!(activity.output.as_deref(), Some("# Harness\nline two\n"));
        assert_eq!(activity.status, Some(crate::provider::ToolStatus::Running));
        let completed = &tool_records(&output.event)[1];
        assert_eq!(completed.id, "command-completed-with-different-provider-id");
        assert_eq!(completed.status.as_deref(), Some("completed"));
        assert!(!activity.output_delta);
    }

    #[tokio::test]
    async fn codex_sparse_tool_progress_preserves_one_request_and_final_titles() -> Result<()> {
        let mut normalization = fixture_normalization();
        let address = crate::provider::TurnAddress {
            scope: fixture_scope(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let mut turn = crate::turn::test_support::TestTurn::new("turn".into(), address, 0);
        let mut output = ProviderOutput::default();
        let mut request_count = 0;
        for (method, params) in [
            (
                "item/started",
                json!({"item":{"id":"command", "type":"commandExecution", "status":"inProgress"}}),
            ),
            (
                "item/commandExecution/outputDelta",
                json!({"itemId":"command", "delta":"partial output"}),
            ),
            (
                "item/completed",
                json!({"item":{"id":"command", "type":"commandExecution", "command":"Get-Content README.md", "cwd":"D:/fixture", "status":"completed", "aggregatedOutput":"final output"}}),
            ),
            (
                "item/started",
                json!({"item":{"id":"mcp", "type":"mcpToolCall", "server":"fixture", "tool":"inspect", "arguments":{"path":"README.md"}, "status":"inProgress"}}),
            ),
            (
                "item/mcpToolCall/progress",
                json!({"itemId":"mcp", "message":"Inspecting README.md"}),
            ),
            (
                "item/completed",
                json!({"item":{"id":"mcp", "type":"mcpToolCall", "server":"fixture", "tool":"inspect", "arguments":{"path":"README.md"}, "status":"completed", "result":{"content":[{"type":"text","text":"inspected"}]}}}),
            ),
        ] {
            let previous_count = output.event.len();
            normalize_event(
                &mut normalization,
                &json!({"method":method,"params":params}),
                &mut output,
                None,
            )
            .await;
            for event in &output.event[previous_count..] {
                let ProviderEvent::Tool { address, update } = event else {
                    panic!("expected tool lifecycle");
                };
                request_count += usize::from(matches!(update, ToolUpdate::Requested { .. }));
                turn.record_tool(address.tool_id.as_str(), update.clone(), 1000)?;
            }
        }
        assert_eq!(
            request_count, 2,
            "progress must never replay a changed request"
        );
        let command = turn.tools().find(|tool| tool.id == "command").unwrap();
        assert_eq!(command.title, "Get-Content README.md");
        assert_eq!(command.status, "completed");
        assert_eq!(&*command.output, "final output");
        let mcp = turn.tools().find(|tool| tool.id == "mcp").unwrap();
        assert_eq!(mcp.title, "fixture.inspect({\"path\":\"README.md\"})");
        assert_eq!(mcp.status, "completed");
        assert!(mcp.output.contains("inspected"));
        Ok(())
    }

    #[tokio::test]
    async fn codex_conflicting_request_fields_fail_without_replacing_prior_identity() -> Result<()>
    {
        for (field, replacement) in [
            ("tool", json!("other")),
            ("server", json!("other-server")),
            ("namespace", json!("other-namespace")),
            ("arguments", json!({"path":"other.md"})),
            ("command", json!("Remove-Item README.md")),
            ("cwd", json!("D:/other")),
            ("type", json!("dynamicToolCall")),
        ] {
            let mut normalization = fixture_normalization();
            let mut output = ProviderOutput::default();
            let mut original = json!({"id":"call", "type":"mcpToolCall", "server":"fixture", "tool":"inspect",
                "arguments":{"path":"README.md"}, "command":"Get-Content README.md", "cwd":"D:/fixture"});
            if field == "namespace" {
                original.as_object_mut().unwrap().remove("server");
                original["namespace"] = json!("fixture");
                original["type"] = json!("dynamicToolCall");
            }
            normalize_event(
                &mut normalization,
                &json!({"method":"item/started", "params":{"item":original}}),
                &mut output,
                None,
            )
            .await;
            let previous_count = output.event.len();
            let mut changed = original.clone();
            changed[field] = replacement;
            let failure = normalization
                .normalize(
                    &json!({"method":"item/completed", "params":{
                "threadId":"thread", "turnId":"turn", "item":changed}}),
                    &mut output,
                    None,
                    "",
                    false,
                )
                .await
                .unwrap_err();
            assert!(
                failure.to_string().contains("changed its invocation"),
                "{field}: {failure}"
            );
            assert_eq!(
                output.event.len(),
                previous_count,
                "{field} published a partial update"
            );
            normalize_event(
                &mut normalization,
                &json!({"method":"item/completed", "params":{"item":original}}),
                &mut output,
                None,
            )
            .await;
            assert!(matches!(
                output.event.last(),
                Some(ProviderEvent::Tool {
                    update: ToolUpdate::Finished { .. },
                    ..
                })
            ));
        }
        Ok(())
    }

    #[tokio::test]
    async fn codex_null_arguments_remain_distinct_from_missing_arguments() -> Result<()> {
        for (original, changed) in [(Value::Null, json!({})), (json!({}), Value::Null)] {
            let mut normalization = fixture_normalization();
            let mut output = ProviderOutput::default();
            let item = json!({"id":"call", "type":"mcpToolCall", "server":"fixture", "tool":"inspect", "arguments":original});
            normalize_event(
                &mut normalization,
                &json!({"method":"item/started", "params":{"item":item}}),
                &mut output,
                None,
            )
            .await;
            let previous_count = output.event.len();
            normalize_event(&mut normalization, &json!({"method":"item/mcpToolCall/progress", "params":{"itemId":"call", "message":"Working"}}), &mut output, None).await;
            assert!(
                output.event.len() > previous_count,
                "missing arguments rejected valid progress"
            );
            let previous_count = output.event.len();
            let mut contradictory = item.clone();
            contradictory["arguments"] = changed;
            let failure = normalization
                .normalize(
                    &json!({"method":"item/completed", "params":{
                "threadId":"thread", "turnId":"turn", "item":contradictory}}),
                    &mut output,
                    None,
                    "",
                    false,
                )
                .await
                .unwrap_err();
            assert!(failure.to_string().contains("changed its invocation"));
            assert_eq!(output.event.len(), previous_count);
            normalize_event(
                &mut normalization,
                &json!({"method":"item/completed", "params":{"item":item}}),
                &mut output,
                None,
            )
            .await;
            assert!(matches!(
                output.event.last(),
                Some(ProviderEvent::Tool {
                    update: ToolUpdate::Finished { .. },
                    ..
                })
            ));
        }
        Ok(())
    }

    #[tokio::test]
    async fn codex_builtin_activity_preserves_web_results_in_child_turns() -> Result<()> {
        let mut normalization = fixture_normalization();
        let address = crate::provider::TurnAddress {
            scope: fixture_scope(),
            thread_id: "child-thread".into(),
            turn_id: "child-turn".into(),
        };
        let mut turn =
            crate::turn::test_support::TestTurn::new("child-turn".into(), address.clone(), 0);
        let mut output = ProviderOutput::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(fixture_scope());
        for (id, action, expected_title) in [
            (
                "search-1",
                json!({ "type": "search", "queries": ["engine documentation", "engine pricing"] }),
                "Web Search · engine documentation · +1 query",
            ),
            (
                "open-1",
                json!({ "type": "openPage", "url": "https://example.com/docs" }),
                "Open web · example.com/docs",
            ),
            (
                "find-1",
                json!({ "type": "findInPage", "url": "https://example.com/docs", "pattern": "royalty" }),
                "Find in page · \"royalty\" · example.com/docs",
            ),
            (
                "find-2",
                json!({ "type": "findInPage", "url": null, "pattern": "royalty" }),
                "Find in page · \"royalty\"",
            ),
        ] {
            let result = json!({
                "type": "webSearch", "id": id, "query": "engine documentation",
                "action": action,
                "results": [{ "type": "text_result", "title": "Documentation",
                    "url": "https://example.com/docs", "snippet": "Tool documentation" }]
            });
            for (method, item, now_ms) in [
                (
                    "item/started",
                    json!({ "type": "webSearch", "id": id,
                    "query": "", "action": null, "results": null }),
                    1000,
                ),
                ("item/completed", result.clone(), 2000),
            ] {
                normalize_event(
                    &mut normalization,
                    &json!({ "method": method, "params": {
                        "threadId": "child-thread", "turnId": "child-turn", "item": item,
                        "startedAtMs": 1000,
                        "completedAtMs": (method == "item/completed").then_some(now_ms)
                    }}),
                    &mut output,
                    Some(&event_sink),
                )
                .await;
                while let Some(event) = event_stream.try_next()? {
                    let ProviderEvent::Tool {
                        address: tool_address,
                        update,
                    } = event
                    else {
                        panic!("expected tool lifecycle")
                    };
                    assert_eq!(&tool_address.turn, &address);
                    turn.record_tool(tool_address.tool_id.as_str(), update, now_ms)?;
                }
                let tool = turn.tools().find(|tool| tool.id == id).unwrap();
                if method == "item/started" {
                    assert_eq!(tool.title, "Web Search");
                } else {
                    assert_eq!(tool.title, expected_title);
                }
            }
            let tool = turn
                .tools()
                .find(|tool| tool.id == id)
                .expect("persisted web activity");
            assert_eq!(tool.title, expected_title);
            assert_eq!(tool.status, "completed");
            assert_eq!(tool.started_at_ms, Some(1000));
            assert_eq!(tool.completed_at_ms, Some(2000));
            assert_eq!(serde_json::from_str::<Value>(&tool.output)?, result);
        }
        assert_eq!(record_count(&output.event), 4);
        assert_eq!(turn.tools().count(), 4);
        assert!(output.completion.evidence.tool_called);
        let restored: crate::turn::test_support::TestTurn =
            serde_json::from_value(serde_json::to_value(&turn)?)?;
        assert_eq!(restored.tools().count(), 4);
        assert!(
            restored
                .tools()
                .all(|tool| tool.output.contains("Tool documentation")
                    && !tool.title.starts_with("webSearch("))
        );
        Ok(())
    }

    #[tokio::test]
    async fn builtin_titles_handle_sleep_and_web_fallbacks_without_aliasing_mcp() {
        let mut normalization = fixture_normalization();
        let event = json!({"method":"item/completed", "params":{"item":{
            "type":"webSearch", "id":"unknown-web", "action":{"type":"other"}, "results":[{"title":"Retained result"}]
        }}});
        let mut output = ProviderOutput::default();
        normalize_event(&mut normalization, &event, &mut output, None).await;
        let activity = &tool_records(&output.event)[0];
        assert!(activity.title.starts_with("webSearch("));
        assert!(activity.title.contains("Retained result"));
        assert!(
            activity
                .output
                .as_ref()
                .unwrap()
                .contains("Retained result")
        );
    }

    #[tokio::test]
    async fn codex_builtin_activity_preserves_unknown_types_and_terminal_status() {
        for status in ["completed", "failed", "cancelled"] {
            let mut normalization = fixture_normalization();
            let mut output = ProviderOutput::default();
            let item = json!({ "type": "futureBuiltin", "id": "future-1", "status": status,
                "arguments": { "api_key": "private-value", "text": "preserve  spacing" },
                "result": { "type": "error", "message": "structured result", "items": [1, 2],
                    "credentials": [{ "access_token": "nested-private-value" }] }
            });
            normalize_event(
                &mut normalization,
                &json!({ "method": "item/completed", "params": {
                    "item": item
                }}),
                &mut output,
                None,
            )
            .await;
            assert_eq!(record_count(&output.event), 1);
            assert!(matches!(output.event[0], ProviderEvent::Tool { .. }));
            let activity = &tool_records(&output.event)[0];
            let title_payload = activity
                .title
                .strip_prefix("futureBuiltin(")
                .unwrap()
                .strip_suffix(')')
                .unwrap();
            let mut expected = item.clone();
            expected["arguments"]["api_key"] = json!("[REDACTED]");
            expected["result"]["credentials"][0]["access_token"] = json!("[REDACTED]");
            assert_eq!(
                serde_json::from_str::<Value>(title_payload).unwrap(),
                expected
            );
            assert_eq!(activity.status.as_deref(), Some(status));
            assert_eq!(
                serde_json::from_str::<Value>(activity.output.as_deref().unwrap()).unwrap(),
                item
            );
            assert!(output.completion.evidence.tool_called);
        }
    }

    #[tokio::test]
    async fn codex_builtin_activity_excludes_non_tools_and_nested_payload_types() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        for item_type in [
            "userMessage",
            "agentMessage",
            "reasoning",
            "plan",
            "enteredReviewMode",
            "exitedReviewMode",
            "collabAgentToolCall",
            "subAgentActivity",
        ] {
            normalize_event(
                &mut normalization,
                &json!({ "method": "item/completed", "params": {
                    "item": { "type": item_type, "id": "non-tool", "text": "Tool documentation",
                        "payload": { "type": "mcpToolCall", "tool": "nested" } }
                }}),
                &mut output,
                None,
            )
            .await;
        }
        for params in [
            json!({ "item": { "type": "futureBuiltin" } }),
            json!({ "item": { "type": "futureBuiltin", "id": "" } }),
            json!({ "item": { "id": "untyped", "payload": { "type": "mcpToolCall" } } }),
        ] {
            normalize_event(
                &mut normalization,
                &json!({ "method": "item/completed", "params": params }),
                &mut output,
                None,
            )
            .await;
        }
        assert!(!output.completion.evidence.tool_called);
        assert!(
            output
                .event
                .iter()
                .all(|event| !matches!(event, ProviderEvent::Tool { .. }))
        );
    }

    #[tokio::test]
    async fn correlates_codex_mcp_lifecycle_into_one_activity() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(fixture_scope());
        normalize_event(
            &mut normalization,
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
        )
        .await;
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/mcpToolCall/progress",
                "params": {
                    "itemId": "mcp-1",
                    "message": "Fetching crate metadata"
                }
            }),
            &mut output,
            Some(&event_sink),
        )
        .await;
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert_eq!(
            std::iter::from_fn(|| event_stream.try_recv().ok()).count(),
            5,
            "each MCP lifecycle update must stream live"
        );
        assert_eq!(
            record_count(&output.event),
            1,
            "one MCP call must persist as one activity"
        );
        let activity = &tool_records(&output.event)[0];
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
        assert!(output.completion.evidence.tool_called);
    }

    #[tokio::test]
    async fn normalizes_codex_mcp_failure_output() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.output.as_deref(), Some("server unavailable"));
        assert_eq!(activity.status.as_deref(), Some("failed"));
    }

    #[tokio::test]
    async fn does_not_classify_mcp_names_as_harness_tools() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
            &json!({
                "method": "item/completed",
                "params": {
                    "item": {
                        "id": "mcp-control-name",
                        "type": "mcpToolCall",
                        "server": "external-mcp",
                        "tool": "goal_complete",
                        "status": "completed",
                        "result": { "content": [{ "type": "text", "text": "ordinary result" }] }
                    }
                }
            }),
            &mut output,
            None,
        )
        .await;

        assert!(!output.completion.evidence.structured_complete);
        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.title, "external-mcp.goal_complete({})");
    }

    #[tokio::test]
    async fn replaces_codex_patch_revisions_with_the_completed_change_set() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert_eq!(record_count(&output.event), 1);
        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.change.file.len(), 1);
        assert!(activity.change.file[0].diff.contains("+final"));
        assert!(!activity.change.file[0].diff.contains("+draft"));
        assert!(output.completion.evidence.workspace_changed);
    }

    #[tokio::test]
    async fn normalizes_codex_absolute_paths_and_structured_change_kinds() {
        let mut normalization = fixture_normalization();
        for (path, workspace, expected) in [
            (
                "C:\\workspace\\src\\main.rs",
                "C:\\workspace",
                "src/main.rs",
            ),
            ("/workspace/src/main.rs", "/workspace", "src/main.rs"),
            ("src/main.rs", "/workspace", "src/main.rs"),
            (
                "C:\\workspace-other\\src\\main.rs",
                "C:\\workspace",
                "C:/workspace-other/src/main.rs",
            ),
        ] {
            let mut output = ProviderOutput::default();
            normalize_event_in_workspace(
                &mut normalization,
                &json!({
                    "method": "item/completed",
                    "params": {
                        "item": {
                            "id": "patch-1",
                            "type": "fileChange",
                            "status": "completed",
                            "changes": [{
                                "path": path,
                                "kind": { "type": "add" },
                                "diff": "@@ -0,0 +1 @@\n+created"
                            }]
                        }
                    }
                }),
                &mut output,
                None,
                workspace,
                true,
            )
            .await;

            let change = &&tool_records(&output.event)[0].change.file[0];
            assert_eq!(change.path, expected);
            assert_eq!(change.kind, ProviderChangeKind::Add);
        }
    }

    #[tokio::test]
    async fn declined_file_changes_do_not_count_as_workspace_progress() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert!(!output.completion.evidence.workspace_changed);
        assert_eq!(tool_records(&output.event)[0].change.file.len(), 1);
    }

    #[tokio::test]
    async fn derives_codex_command_status_from_lifecycle_method() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;
        normalize_event(
            &mut normalization,
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
        )
        .await;

        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.status.as_deref(), Some("completed"));
        assert_eq!(
            activity.output.as_deref(),
            Some("{\"$schema\": \"rulesync\"}")
        );
    }

    #[tokio::test]
    async fn strips_terminal_controls_from_command_output() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        let activity = &tool_records(&output.event)[0];
        assert_eq!(activity.output.as_deref(), Some("Mode  Name"));
    }

    #[tokio::test]
    async fn ignores_codex_moderation_metadata_with_tool_named_fields() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        let (event_sink, mut event_stream) = crate::provider::events::channel(fixture_scope());
        normalize_event(
            &mut normalization,
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
        )
        .await;
        assert!(output.event.is_empty());
        assert!(event_stream.try_recv().is_err());
    }

    #[tokio::test]
    async fn records_token_usage_without_emitting_interaction_noise() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;
        let error = normalization
            .normalize(
                &json!({
                    "method": "turn/started",
                    "params": { "turn": { "id": "turn-1", "status": "inProgress" } }
                }),
                &mut output,
                None,
                "",
                false,
            )
            .await
            .unwrap_err();
        assert!(matches!(
            crate::provider::ProviderError::from(error),
            crate::provider::ProviderError::Protocol { .. }
        ));

        assert!(
            !output
                .event
                .iter()
                .any(|event| matches!(event, ProviderEvent::Usage { .. }))
        );
        assert_eq!(
            output.completion.metrics.context_usage,
            Some(ContextUsage {
                used: 2700,
                size: 353000,
                remaining_percent: 100,
            })
        );
        assert_eq!(record_count(&output.event), 1);
        assert!(matches!(
            output.event[0],
            ProviderEvent::Runtime {
                update: crate::provider::RuntimeUpdate::ContextUsage { .. },
                ..
            }
        ));
    }

    #[tokio::test]
    async fn normalizes_codex_spawn_activity_without_rendering_it_as_a_tool() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert_eq!(record_count(&output.event), 1);
        let ProviderEvent::Agent {
            update: crate::provider::AgentUpdate::Lifecycle { event: lifecycle },
            ..
        } = &output.event[0]
        else {
            panic!("expected lifecycle")
        };
        assert_eq!(
            lifecycle.provider_thread_id.as_deref(),
            Some("child-thread")
        );
        assert_eq!(lifecycle.definition.as_deref(), Some("explorer"));
        assert_eq!(lifecycle.status, AgentExecutionState::Running);
        assert!(!matches!(output.event[0], ProviderEvent::Tool { .. }));
    }

    #[tokio::test]
    async fn normalizes_each_codex_child_state_instead_of_the_wait_tool_state() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert_eq!(record_count(&output.event), 2);
        let lifecycle_list = output
            .event
            .iter()
            .map(|event| match event {
                ProviderEvent::Agent {
                    update: crate::provider::AgentUpdate::Lifecycle { event },
                    ..
                } => event,
                _ => panic!("expected lifecycle"),
            })
            .collect::<Vec<_>>();
        assert_eq!(lifecycle_list[0].status, AgentExecutionState::Completed);
        assert_eq!(lifecycle_list[1].status, AgentExecutionState::Running);
        assert_eq!(
            lifecycle_list[0].parent_thread_id.as_deref(),
            Some("parent-thread")
        );
    }

    #[tokio::test]
    async fn ignores_a_codex_wait_without_child_state() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert!(output.event.is_empty());
    }

    #[tokio::test]
    async fn normalizes_codex_subagent_activity_with_its_child_identity() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        let ProviderEvent::Agent {
            update: crate::provider::AgentUpdate::Lifecycle { event: lifecycle },
            ..
        } = &output.event[0]
        else {
            panic!("expected lifecycle")
        };
        assert_eq!(
            lifecycle.provider_thread_id.as_deref(),
            Some("child-thread")
        );
        assert_eq!(lifecycle.status, AgentExecutionState::Interrupted);
    }

    #[tokio::test]
    async fn ignores_codex_subagent_interaction_directed_at_the_parent() {
        let mut normalization = fixture_normalization();
        let mut output = ProviderOutput::default();
        normalize_event(
            &mut normalization,
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
        )
        .await;

        assert!(output.event.is_empty());
    }
}

#[cfg(test)]
mod approval_tests {
    use super::*;
    use crate::permissions::document::{CATEGORY_EDIT, PermissionDecision};
    use crate::provider::{PromptMode, ProviderInput, ProviderRequest, ServiceTier};
    use crate::session::{AccessPolicy, DirectoryScope, WriteScope};

    #[tokio::test]
    async fn file_approvals_use_started_paths_and_existing_access_without_prompting() -> Result<()>
    {
        for (scope, external, grant, deny, mode, expected) in [
            (
                WriteScope::Full,
                false,
                false,
                false,
                ApprovalMode::Ask,
                "accept",
            ),
            (
                WriteScope::Workspace,
                false,
                false,
                false,
                ApprovalMode::Ask,
                "accept",
            ),
            (
                WriteScope::Off,
                false,
                false,
                false,
                ApprovalMode::Allow,
                "decline",
            ),
            (
                WriteScope::Full,
                false,
                false,
                true,
                ApprovalMode::Allow,
                "decline",
            ),
            (
                WriteScope::Workspace,
                true,
                true,
                false,
                ApprovalMode::Ask,
                "accept",
            ),
            (
                WriteScope::Workspace,
                true,
                false,
                false,
                ApprovalMode::Deny,
                "decline",
            ),
        ] {
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().join("workspace");
            let outside = fixture.path().join("outside");
            std::fs::create_dir_all(&workspace)?;
            std::fs::create_dir_all(&outside)?;
            let changed = if external {
                outside.join("file.rs")
            } else {
                workspace.join("file.rs")
            };
            let coordinator = PermissionCoordinator::transient(&workspace)?;
            if deny {
                coordinator.store().write().unwrap().set_rule_list(&[(
                    CATEGORY_EDIT.into(),
                    changed.to_string_lossy().into_owned(),
                    PermissionDecision::Deny,
                )])?;
            }
            let request = ProviderRequest {
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace: workspace.to_string_lossy().into_owned(),
                input: ProviderInput::from_text("edit"),
                mode: PromptMode::Chat,
                model: "default".into(),
                effort: "default".into(),
                context_window: None,
                service_tier: ServiceTier::Standard,
                access: AccessPolicy {
                    sandbox: false,
                    read_access: DirectoryScope::Full,
                    write_access: scope,
                    writable_directory: if grant {
                        vec![outside.to_string_lossy().into_owned()]
                    } else {
                        vec![]
                    },
                    ..Default::default()
                },
                approval_mode: mode,
                provider_session_id: None,
                submission: None,
                recovered_input: Vec::new(),
                control_context: None,
            };
            let _policy = coordinator.directories(None, &request)?;
            let before = coordinator.store().read().unwrap().open().1;
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
            let endpoint = format!("ws://{}", listener.local_addr()?);
            let server = tokio::spawn(async move {
                let (stream, _) = listener.accept().await?;
                tokio_tungstenite::accept_async(stream).await
            });
            let (sink, _events) = crate::provider::events::channel(fixture_scope());
            let mut process = CodexJsonRpc::connect(
                &endpoint,
                &request.workspace,
                mode,
                coordinator.clone(),
                Some(sink),
                Arc::new(TraceStore::open(fixture.path())?),
                "session".into(),
            )
            .await?;
            process.normalization.bind(Some(fixture_scope()));
            process.set_execution_owner(request.owner()?).await;
            let mut socket = server.await??;
            let mut output = ProviderOutput::default();
            process.publish_message(json!({"method":"item/started","params":{
                "threadId":"thread","turnId":"turn","item":{"id":"edit","type":"fileChange",
                "changes":[{"path":changed,"kind":{"type":"update","move_path":null},"diff":"new"}]}
            }}), &mut output).await?;
            let approval = json!({"id":10,"method":"item/fileChange/requestApproval","params":{
                "threadId":"thread","turnId":"turn","itemId":"edit","grantRoot":null
            }});
            process
                .publish_message(approval.clone(), &mut output)
                .await?;
            process.publish_message(approval, &mut output).await?;
            assert_eq!(process.pending_requests.len(), 1);
            tokio::time::timeout(
                Duration::from_secs(3),
                process.collect_requests(&mut output),
            )
            .await??;
            let response = tokio::time::timeout(Duration::from_secs(3), socket.next())
                .await?
                .context("approval socket closed")??;
            let response: Value = serde_json::from_str(response.to_text()?)?;
            assert_eq!(response["result"]["decision"], expected, "{response}");
            assert!(coordinator.pending_list()?.is_empty());
            if expected == "accept" {
                let next = json!({"id":11,"method":"item/fileChange/requestApproval","params":{
                    "threadId":"thread","turnId":"turn","itemId":"edit","grantRoot":null
                }});
                process.publish_message(next, &mut output).await?;
                let completed =
                    tokio::time::timeout(Duration::from_secs(3), process.receive_message())
                        .await??;
                assert!(matches!(completed, ReceivedMessage::Request(_)));
                process.publish_message(json!({"method":"item/started","params":{
                    "threadId":"thread","turnId":"turn","item":{"id":"edit","type":"fileChange",
                    "changes":[{"path":changed,"kind":{"type":"update"},"diff":"different proposal"}]}
                }}), &mut output).await?;
                process.publish_received(completed, &mut output).await?;
                let rejected = tokio::time::timeout(Duration::from_secs(3), socket.next())
                    .await?
                    .context("approval socket closed")??;
                let rejected: Value = serde_json::from_str(rejected.to_text()?)?;
                assert!(
                    rejected["error"]["message"]
                        .as_str()
                        .is_some_and(|text| text.contains("changed or retired"))
                );
            }
            assert_eq!(coordinator.store().read().unwrap().open().1, before);
        }
        Ok(())
    }
}

pub(super) use forge_providers::codex::decode::{agent_lifecycle_list, pointer_string};

#[cfg(test)]
use forge_providers::codex::decode::CodexToolSnapshot;
