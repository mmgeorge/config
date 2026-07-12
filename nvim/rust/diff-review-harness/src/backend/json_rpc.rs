use crate::backend::{
    BackendEvent, BackendEventSink, BackendOutput, PlanProgress, PlanStep, ToolActivity,
    ToolActivityKind, TrustPolicy,
};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Stdio;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout};

/// Owns one JSON-RPC subprocess for an ACP or Codex turn.
pub struct JsonRpcProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: tokio::io::Lines<BufReader<ChildStdout>>,
    next_id: u64,
    write_allowed: bool,
    workspace: String,
    trust_policy: TrustPolicy,
    event_sink: Option<BackendEventSink>,
    terminal: super::acp::terminal::TerminalStore,
}

impl JsonRpcProcess {
    /// Start one provider process with isolated stdin, stdout, and stderr pipes.
    pub async fn start(
        command: &[String],
        write_allowed: bool,
        workspace: &str,
        trust_policy: TrustPolicy,
        event_sink: Option<BackendEventSink>,
    ) -> Result<Self> {
        let (program, args) = command
            .split_first()
            .context("backend launch command is empty")?;
        let mut provider = super::process::command(program, args);
        let mut child = provider
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .kill_on_drop(true)
            .spawn()
            .with_context(|| format!("start Harness backend {program}"))?;
        let stdin = child
            .stdin
            .take()
            .context("backend stdin pipe was not created")?;
        let stdout = child
            .stdout
            .take()
            .context("backend stdout pipe was not created")?;
        Ok(Self {
            child,
            stdin,
            stdout: BufReader::new(stdout).lines(),
            next_id: 1,
            write_allowed,
            workspace: workspace.to_owned(),
            trust_policy,
            event_sink,
            terminal: super::acp::terminal::TerminalStore::default(),
        })
    }

    /// Configure per-turn workspace, permission, and live-event state.
    pub fn configure_request(
        &mut self,
        write_allowed: bool,
        workspace: &str,
        trust_policy: TrustPolicy,
        event_sink: Option<BackendEventSink>,
    ) {
        self.write_allowed = write_allowed;
        self.workspace = workspace.to_owned();
        self.trust_policy = trust_policy;
        self.event_sink = event_sink;
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
        let id = self.next_id;
        self.next_id += 1;
        self.write_message(
            &json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }),
        )
        .await?;
        loop {
            let line = self
                .stdout
                .next_line()
                .await
                .context("read backend JSONL")?
                .context("backend closed stdout before completing the request")?;
            let message: Value = serde_json::from_str(&line)
                .with_context(|| format!("decode backend JSON-RPC line: {line}"))?;
            if message.get("id").and_then(Value::as_u64) == Some(id)
                && message.get("method").is_none()
            {
                if let Some(error) = message.get("error") {
                    anyhow::bail!("backend {method} failed: {error}");
                }
                return Ok(message.get("result").cloned().unwrap_or(Value::Null));
            }
            if message.get("id").is_some() && message.get("method").is_some() {
                self.respond_to_provider_request(&message).await?;
                normalize_event(&message, output, self.event_sink.as_ref());
                continue;
            }
            normalize_event(&message, output, self.event_sink.as_ref());
        }
    }

    /// Collect provider events until a named terminal notification arrives.
    pub async fn read_until(
        &mut self,
        terminal_method: &str,
        output: &mut BackendOutput,
    ) -> Result<Value> {
        loop {
            let line = self
                .stdout
                .next_line()
                .await
                .context("read backend JSONL")?
                .context("backend closed stdout before the turn completed")?;
            let message: Value = serde_json::from_str(&line)
                .with_context(|| format!("decode backend JSON-RPC line: {line}"))?;
            if message.get("id").is_some() && message.get("method").is_some() {
                self.respond_to_provider_request(&message).await?;
            }
            normalize_event(&message, output, self.event_sink.as_ref());
            if message.get("method").and_then(Value::as_str) == Some(terminal_method) {
                return Ok(message.get("params").cloned().unwrap_or(Value::Null));
            }
        }
    }

    async fn write_message(&mut self, message: &Value) -> Result<()> {
        let mut encoded = serde_json::to_vec(message)?;
        encoded.push(b'\n');
        self.stdin
            .write_all(&encoded)
            .await
            .context("write backend JSON-RPC request")?;
        self.stdin
            .flush()
            .await
            .context("flush backend JSON-RPC request")
    }

    async fn respond_to_provider_request(&mut self, message: &Value) -> Result<()> {
        let id = message.get("id").cloned().unwrap_or(Value::Null);
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let result = self.provider_request_result(method, message).await;
        let response = match result {
            Ok(result) => json!({ "jsonrpc": "2.0", "id": id, "result": result }),
            Err(error) => json!({
                "jsonrpc": "2.0",
                "id": id,
                "error": { "code": -32001, "message": format!("{error:#}") }
            }),
        };
        self.write_message(&response).await
    }

    async fn provider_request_result(&mut self, method: &str, message: &Value) -> Result<Value> {
        let params = message.get("params").unwrap_or(&Value::Null);
        match method {
            "item/commandExecution/requestApproval" | "item/fileChange/requestApproval" => {
                Ok(codex_permission_result(
                    self.write_allowed
                        && workspace_profile_allows(message, &self.workspace, &self.trust_policy),
                ))
            }
            "item/permissions/requestApproval" => Ok(codex_permission_result(false)),
            "item/tool/call" => Ok(json!({
                "contentItems": [{ "type": "inputText", "text": "Recorded by DiffReview Harness" }],
                "success": true
            })),
            "session/request_permission" => Ok(acp_permission_result(
                message,
                self.write_allowed
                    && workspace_profile_allows(message, &self.workspace, &self.trust_policy),
            )),
            "fs/read_text_file" => self.read_text_file(params),
            "fs/write_text_file" => self.write_text_file(params),
            "terminal/create" => {
                anyhow::ensure!(
                    workspace_profile_allows(message, &self.workspace, &self.trust_policy),
                    "trust profile denied ACP terminal command"
                );
                let cwd = params
                    .get("cwd")
                    .and_then(Value::as_str)
                    .unwrap_or(&self.workspace);
                let cwd = provider_path(
                    cwd,
                    &self.workspace,
                    self.trust_policy.allow_outside_workspace,
                )?;
                anyhow::ensure!(cwd.is_dir(), "ACP terminal cwd is not a directory");
                self.terminal.create(params, cwd).await
            }
            "terminal/output" => self.terminal.output(params).await,
            "terminal/wait_for_exit" => self.terminal.wait(params).await,
            "terminal/kill" => self.terminal.kill(params).await,
            "terminal/release" => self.terminal.release(params).await,
            _ => Ok(Value::Null),
        }
    }

    fn read_text_file(&self, params: &Value) -> Result<Value> {
        let path = required_provider_path(params, &self.workspace, &self.trust_policy)?;
        let content = fs::read_to_string(&path)
            .with_context(|| format!("read ACP file {}", path.display()))?;
        let line = params.get("line").and_then(Value::as_u64).unwrap_or(1);
        let limit = params.get("limit").and_then(Value::as_u64);
        Ok(json!({ "content": select_line(&content, line, limit) }))
    }

    fn write_text_file(&self, params: &Value) -> Result<Value> {
        anyhow::ensure!(self.write_allowed, "READ mode denies ACP file writes");
        anyhow::ensure!(
            self.trust_policy.allow_workspace_write,
            "trust profile denies ACP workspace writes"
        );
        let path = required_provider_path(params, &self.workspace, &self.trust_policy)?;
        let content = params
            .get("content")
            .and_then(Value::as_str)
            .context("ACP file content is required")?;
        fs::write(&path, content).with_context(|| format!("write ACP file {}", path.display()))?;
        Ok(json!({}))
    }
}

impl Drop for JsonRpcProcess {
    fn drop(&mut self) {
        let _ = self.child.start_kill();
    }
}

fn acp_permission_result(message: &Value, write_allowed: bool) -> Value {
    if !write_allowed {
        return json!({ "outcome": { "outcome": "cancelled" } });
    }
    let option_id = message
        .pointer("/params/options")
        .and_then(Value::as_array)
        .and_then(|option| {
            option
                .iter()
                .find(|entry| {
                    let kind = entry
                        .get("kind")
                        .and_then(Value::as_str)
                        .unwrap_or_default();
                    kind.contains("allow") || kind.contains("once")
                })
                .or_else(|| option.first())
        })
        .and_then(|entry| entry.get("optionId").or_else(|| entry.get("option_id")))
        .cloned()
        .unwrap_or_else(|| Value::String("allow_once".into()));
    json!({ "outcome": { "outcome": "selected", "optionId": option_id } })
}

fn codex_permission_result(write_allowed: bool) -> Value {
    json!({ "decision": if write_allowed { "acceptForSession" } else { "decline" } })
}

fn required_provider_path(
    params: &Value,
    workspace: &str,
    policy: &TrustPolicy,
) -> Result<PathBuf> {
    let path = params
        .get("path")
        .and_then(Value::as_str)
        .context("ACP file path is required")?;
    provider_path(path, workspace, policy.allow_outside_workspace)
}

fn provider_path(path: &str, workspace: &str, allow_outside_workspace: bool) -> Result<PathBuf> {
    let path = normalize_absolute(Path::new(path))?;
    if allow_outside_workspace {
        return Ok(path);
    }
    let workspace = fs::canonicalize(workspace)
        .or_else(|_| normalize_absolute(Path::new(workspace)))
        .context("resolve ACP workspace boundary")?;
    let mut existing = path.as_path();
    while !existing.exists() {
        existing = existing
            .parent()
            .context("ACP path has no existing ancestor")?;
    }
    let resolved = fs::canonicalize(existing)
        .with_context(|| format!("resolve ACP path boundary {}", existing.display()))?;
    anyhow::ensure!(
        path_starts_with(&resolved, &workspace),
        "ACP path escaped the workspace: {}",
        path.display()
    );
    Ok(path)
}

fn normalize_absolute(path: &Path) -> Result<PathBuf> {
    anyhow::ensure!(
        path.is_absolute(),
        "ACP path must be absolute: {}",
        path.display()
    );
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                anyhow::ensure!(normalized.pop(), "ACP path escaped its filesystem root");
            }
            _ => normalized.push(component.as_os_str()),
        }
    }
    Ok(normalized)
}

#[cfg(windows)]
fn path_starts_with(path: &Path, root: &Path) -> bool {
    let path = path
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase();
    let root = root
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase();
    path == root || path.starts_with(&(root + "/"))
}

#[cfg(not(windows))]
fn path_starts_with(path: &Path, root: &Path) -> bool {
    path.starts_with(root)
}

fn select_line(content: &str, line: u64, limit: Option<u64>) -> String {
    let skip_count = usize::try_from(line.saturating_sub(1)).unwrap_or(usize::MAX);
    let take_count = limit
        .and_then(|value| usize::try_from(value).ok())
        .unwrap_or(usize::MAX);
    content
        .split_inclusive('\n')
        .skip(skip_count)
        .take(take_count)
        .collect()
}

fn workspace_profile_allows(message: &Value, workspace: &str, policy: &TrustPolicy) -> bool {
    let encoded = message.to_string().to_ascii_lowercase();
    let contains_command = message.pointer("/params/command").is_some()
        || encoded.contains("commandexecution")
        || encoded.contains("terminal");
    if contains_command && !policy.allow_command {
        return false;
    }
    if encoded.contains("filechange") && !policy.allow_workspace_write {
        return false;
    }
    let git_index = ["git add", "git reset", "git restore --staged"]
        .iter()
        .any(|pattern| encoded.contains(pattern));
    let git_history = [
        "git commit",
        "git checkout",
        "git switch",
        "git rebase",
        "git merge",
        "git cherry-pick",
        "git push",
        "git fetch",
        "git pull",
    ]
    .iter()
    .any(|pattern| encoded.contains(pattern));
    let network = ["curl ", "wget ", "invoke-webrequest", "invoke-restmethod"]
        .iter()
        .any(|pattern| encoded.contains(pattern));
    let elevation = ["sudo ", "runas "]
        .iter()
        .any(|pattern| encoded.contains(pattern));
    let denied_command = (git_index && !policy.allow_git_index)
        || (git_history && !policy.allow_git_history)
        || (network && !policy.allow_network)
        || (elevation && !policy.allow_elevation);
    if denied_command {
        return false;
    }

    if policy.allow_outside_workspace {
        return true;
    }
    let Ok(workspace) = normalize_absolute(Path::new(workspace)) else {
        return false;
    };
    absolute_path_list(message).into_iter().all(|path| {
        normalize_absolute(Path::new(&path)).is_ok_and(|path| path_starts_with(&path, &workspace))
    })
}

fn absolute_path_list(value: &Value) -> Vec<String> {
    let mut result = Vec::new();
    match value {
        Value::Object(map) => {
            for (key, child) in map {
                if (key.eq_ignore_ascii_case("path") || key.to_ascii_lowercase().ends_with("path"))
                    && child.as_str().is_some_and(is_absolute_path)
                {
                    result.push(child.as_str().unwrap_or_default().to_owned());
                }
                result.extend(absolute_path_list(child));
            }
        }
        Value::Array(item) => {
            for child in item {
                result.extend(absolute_path_list(child));
            }
        }
        _ => {}
    }
    result
}

fn is_absolute_path(path: &str) -> bool {
    path.starts_with('/')
        || path.starts_with("\\\\")
        || path.as_bytes().get(1).is_some_and(|byte| *byte == b':')
}

fn normalize_event(
    message: &Value,
    output: &mut BackendOutput,
    event_sink: Option<&BackendEventSink>,
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
    let usage_event = method_lower.contains("tokenusage")
        || encoded.contains("\"sessionupdate\":\"usage_update\"")
        || encoded.contains("\"session_update\":\"usage_update\"");
    if let Some(token_count) = turn_token_count(&params) {
        output.metrics.token_count = Some(token_count);
    }
    if usage_event {
        return;
    }
    let control_tool = control_tool_name(&params);
    let tool_event = method_lower.contains("tool")
        || method_lower.contains("commandexecution")
        || method_lower.contains("filechange")
        || encoded.contains("\"tool_call\"")
        || encoded.contains("\"tool_call_update\"")
        || encoded.contains("\"command_execution\"")
        || encoded.contains("\"type\":\"commandexecution\"")
        || encoded.contains("\"type\":\"dynamictoolcall\"")
        || encoded.contains("\"type\":\"filechange\"");
    let plan_event = method_lower.contains("plan")
        || encoded.contains("\"sessionupdate\":\"plan\"")
        || encoded.contains("\"session_update\":\"plan\"");
    let reasoning_event = method_lower.contains("reason")
        || encoded.contains("\"agent_thought_chunk\"")
        || encoded.contains("\"thought\"");
    let assistant_event = method_lower.contains("agentmessage")
        || encoded.contains("\"sessionupdate\":\"agent_message_chunk\"")
        || encoded.contains("\"session_update\":\"agent_message_chunk\"");
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

    if tool_event && control_tool == Some("harness_plan_submit") {
        if let Some(plan) = find_plan(&params) {
            output.plan_markdown = Some(plan);
            output.structured_plan = true;
        }
    } else if !output.structured_plan
        && plan_event
        && let Some(plan) = find_plan(&params)
    {
        output.plan_markdown = Some(plan);
    }
    if tool_event && control_tool == Some("harness_goal_complete") {
        output.evidence.structured_complete = true;
    }
    if tool_event && control_tool == Some("harness_goal_blocked") {
        output.evidence.structured_blocked = true;
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
    if control_tool.is_some() {
        return;
    }
    let user_message_item = params
        .pointer("/item/type")
        .and_then(Value::as_str)
        .is_some_and(|item_type| item_type.eq_ignore_ascii_case("userMessage"));
    if !user_message_item
        && (tool_event || plan_event || reasoning_event || assistant_event || error_event)
    {
        let activity = tool_event
            .then(|| normalize_tool_activity(method, &params))
            .flatten();
        let plan_progress = plan_event
            .then(|| normalize_plan_progress(method, &params))
            .flatten();
        let event = BackendEvent {
            kind: kind.into(),
            text: activity.is_none().then_some(text).flatten(),
            data: json!({ "method": method, "params": params }),
            activity,
            summary: None,
            plan_progress,
        };
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(event.clone());
        }
        append_output_event(output, event);
    }
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

fn control_tool_name(value: &Value) -> Option<&str> {
    match value {
        Value::Array(item) => item.iter().find_map(control_tool_name),
        Value::Object(map) => {
            for key in ["tool", "name", "toolName", "tool_name"] {
                if let Some(name) = map.get(key).and_then(Value::as_str)
                    && matches!(
                        name,
                        "harness_plan_submit"
                            | "harness_goal_complete"
                            | "harness_goal_blocked"
                            | "harness_goal_status"
                    )
                {
                    return Some(name);
                }
            }
            map.values().find_map(control_tool_name)
        }
        _ => None,
    }
}

fn append_output_event(output: &mut BackendOutput, event: BackendEvent) {
    if let Some(progress) = event.plan_progress.as_ref()
        && let Some(index) = output.event.iter().rposition(|previous| {
            previous
                .plan_progress
                .as_ref()
                .is_some_and(|previous_progress| previous_progress.id == progress.id)
        })
    {
        let mut previous = output.event.remove(index);
        merge_plan_progress(&mut previous, event);
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

fn merge_plan_progress(previous: &mut BackendEvent, incoming: BackendEvent) {
    let Some(incoming_progress) = incoming.plan_progress else {
        return;
    };
    let Some(previous_progress) = previous.plan_progress.as_mut() else {
        return;
    };
    if incoming_progress.name.is_some() {
        previous_progress.name = incoming_progress.name;
    }
    if !incoming_progress.step_list.is_empty() {
        previous_progress.step_list = incoming_progress.step_list;
    }
    previous_progress.status = incoming_progress.status;
    previous.data = incoming.data;
}

fn normalize_plan_progress(method: &str, params: &Value) -> Option<PlanProgress> {
    let envelope = params.get("update").unwrap_or(params);
    let entry_list = envelope
        .get("plan")
        .or_else(|| envelope.get("entries"))
        .and_then(Value::as_array);
    let step_list = entry_list
        .into_iter()
        .flatten()
        .filter_map(|entry| {
            let text = entry
                .get("step")
                .or_else(|| entry.get("content"))
                .and_then(Value::as_str)?;
            let status = entry
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or("pending");
            Some(PlanStep {
                text: text.to_owned(),
                status: status.to_owned(),
            })
        })
        .collect::<Vec<_>>();
    let method_lower = method.to_ascii_lowercase();
    let completed = method_lower.ends_with("/completed")
        || envelope
            .get("status")
            .and_then(Value::as_str)
            .is_some_and(|status| matches!(status, "completed" | "complete"));
    if step_list.is_empty() && !completed {
        return None;
    }
    let id = envelope
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
    Some(PlanProgress {
        id,
        name,
        status: if completed { "completed" } else { "inProgress" }.to_owned(),
        step_list,
    })
}

fn normalize_tool_activity(method: &str, params: &Value) -> Option<ToolActivity> {
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
        .or_else(|| envelope.get("sessionUpdate"))
        .or_else(|| envelope.get("session_update"))
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
    let title = [
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
    .unwrap_or_else(|| match kind {
        ToolActivityKind::Command => "command".into(),
        ToolActivityKind::FileChange => "file changes".into(),
        ToolActivityKind::ToolCall => "tool".into(),
    });
    let output = item
        .get("aggregatedOutput")
        .or_else(|| item.get("aggregated_output"))
        .or_else(|| item.get("output"))
        .or_else(|| envelope.get("output"))
        .or_else(|| envelope.get("delta"))
        .and_then(Value::as_str)
        .map(strip_ansi_escapes::strip_str)
        .or_else(|| {
            envelope
                .get("content")
                .and_then(first_text)
                .map(strip_ansi_escapes::strip_str)
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
        });
    Some(ToolActivity {
        id,
        kind,
        title,
        output,
        status,
        output_delta: method_lower.ends_with("/outputdelta")
            || method_lower.ends_with("/output_delta"),
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

fn find_plan(value: &Value) -> Option<String> {
    if value.get("tool").and_then(Value::as_str) == Some("harness_plan_submit")
        && let Some(markdown) = value.pointer("/arguments/markdown").and_then(Value::as_str)
    {
        return Some(markdown.to_owned());
    }
    if let Some(markdown) = value.get("markdown").and_then(Value::as_str) {
        let encoded = value.to_string().to_ascii_lowercase();
        if encoded.contains("harness_plan_submit") || encoded.contains("plan") {
            return Some(markdown.to_owned());
        }
    }
    if let Some(plan) = value.get("plan").and_then(Value::as_str) {
        return Some(plan.to_owned());
    }
    if let Some(step_list) = value
        .get("plan")
        .or_else(|| value.get("entries"))
        .and_then(Value::as_array)
    {
        return plan_entry_markdown(step_list);
    }
    match value {
        Value::Array(item) => item.iter().find_map(find_plan),
        Value::Object(map) => map.values().find_map(find_plan),
        _ => None,
    }
}

fn plan_entry_markdown(entry_list: &[Value]) -> Option<String> {
    let mut markdown = String::from("# Plan\n\n");
    let mut count = 0;
    for entry in entry_list {
        let Some(text) = entry
            .get("step")
            .or_else(|| entry.get("content"))
            .and_then(Value::as_str)
        else {
            continue;
        };
        count += 1;
        let status = entry.get("status").and_then(Value::as_str);
        let marker = match status {
            Some("completed" | "complete") => "[x] ",
            Some("in_progress" | "inProgress") => "[-] ",
            _ => "[ ] ",
        };
        markdown.push_str(&format!("{count}. {marker}{text}\n"));
    }
    (count > 0).then_some(markdown)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn workspace_profile_denies_index_network_and_outside_paths() {
        let root = tempfile::tempdir().unwrap();
        let workspace = root.path().join("work");
        let sibling = root.path().join("work-other");
        fs::create_dir_all(workspace.join("src")).unwrap();
        fs::create_dir_all(&sibling).unwrap();
        let workspace = workspace.to_string_lossy().into_owned();
        assert!(!workspace_profile_allows(
            &json!({ "params": { "command": "git add ." } }),
            &workspace,
            &TrustPolicy::default()
        ));
        assert!(!workspace_profile_allows(
            &json!({ "params": { "command": "curl https://example.com" } }),
            &workspace,
            &TrustPolicy::default()
        ));
        assert!(!workspace_profile_allows(
            &json!({ "params": { "path": sibling.join("file.txt") } }),
            &workspace,
            &TrustPolicy::default()
        ));
        assert!(workspace_profile_allows(
            &json!({ "params": { "path": Path::new(&workspace).join("src/main.rs"), "command": "cargo test" } }),
            &workspace,
            &TrustPolicy::default()
        ));
        assert!(
            provider_path(
                Path::new(&workspace)
                    .join("new/file.txt")
                    .to_string_lossy()
                    .as_ref(),
                &workspace,
                false
            )
            .is_ok()
        );
        assert!(
            provider_path(
                sibling.join("file.txt").to_string_lossy().as_ref(),
                &workspace,
                false
            )
            .is_err()
        );
    }

    #[test]
    fn selects_acp_file_lines_without_rewriting_line_endings() {
        assert_eq!(select_line("one\r\ntwo\r\nthree", 2, Some(1)), "two\r\n");
        assert_eq!(select_line("one\ntwo", 3, None), "");
    }

    #[test]
    fn extracts_complete_plan_from_dynamic_tool_arguments() {
        let value = json!({
            "tool": "harness_plan_submit",
            "arguments": { "markdown": "# Plan\n\nComplete" }
        });
        assert_eq!(find_plan(&value).as_deref(), Some("# Plan\n\nComplete"));
    }

    #[test]
    fn preserves_native_plan_across_lifecycle_events_without_plan_content() {
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
        assert_eq!(
            output.plan_markdown.as_deref(),
            Some("# Plan\n\n1. [ ] Inspect the change\n")
        );
        assert_eq!(output.event.len(), 1);
        let progress = output.event[0].plan_progress.as_ref().unwrap();
        assert_eq!(progress.status, "completed");
        assert_eq!(progress.step_list.len(), 1);
        assert_eq!(progress.step_list[0].text, "Inspect the change");
    }

    #[test]
    fn extracts_stable_acp_plan_entries() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "session/update",
                "params": {
                    "sessionUpdate": "plan",
                    "entries": [
                        { "content": "Inspect the change", "status": "completed" },
                        { "content": "Apply the fix", "status": "in_progress" }
                    ]
                }
            }),
            &mut output,
            None,
        );
        assert_eq!(
            output.plan_markdown.as_deref(),
            Some("# Plan\n\n1. [x] Inspect the change\n2. [-] Apply the fix\n")
        );
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
                "params": { "name": "harness_goal_complete", "arguments": {} }
            }),
            &mut output,
            Some(&event_sink),
        );
        assert!(output.evidence.structured_complete);
        assert!(output.event.is_empty());
        assert!(event_stream.is_empty());
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
    fn correlates_acp_tool_call_updates_into_one_activity() {
        let mut output = BackendOutput::default();
        normalize_event(
            &json!({
                "method": "session/update",
                "params": {
                    "update": {
                        "sessionUpdate": "tool_call",
                        "toolCallId": "tool-1",
                        "title": "Read README.md",
                        "status": "in_progress"
                    }
                }
            }),
            &mut output,
            None,
        );
        normalize_event(
            &json!({
                "method": "session/update",
                "params": {
                    "update": {
                        "sessionUpdate": "tool_call_update",
                        "toolCallId": "tool-1",
                        "status": "completed",
                        "content": [{
                            "type": "content",
                            "content": { "type": "text", "text": "# Harness" }
                        }]
                    }
                }
            }),
            &mut output,
            None,
        );

        assert_eq!(output.event.len(), 1);
        let activity = output.event[0].activity.as_ref().unwrap();
        assert_eq!(activity.title, "Read README.md");
        assert_eq!(activity.output.as_deref(), Some("# Harness"));
        assert_eq!(activity.status.as_deref(), Some("completed"));
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
                    "tokenUsage": { "last": { "totalTokens": 2700 } }
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
        assert!(output.event.is_empty());
    }
}
