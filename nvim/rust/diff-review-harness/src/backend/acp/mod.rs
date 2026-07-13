use crate::backend::json_rpc::JsonRpcProcess;
use crate::backend::{
    Backend, BackendCapability, BackendEventSink, BackendModel, BackendOutput, BackendRequest,
};
use crate::session::WriteMode;
use agent_client_protocol::schema::v1::SessionModeState;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::path::Path;
use tokio::sync::Mutex;

pub(crate) mod terminal;

/// Owns ACP initialization, session reuse, prompting, and native fork negotiation.
pub struct AcpBackend {
    command: Vec<String>,
    connection: Mutex<Option<AcpConnection>>,
}

/// Owns one long-lived ACP agent process and its known provider sessions.
struct AcpConnection {
    process: JsonRpcProcess,
    initialized: Value,
    session: HashMap<String, Value>,
    preview_session_id: Option<String>,
    native_compact: bool,
}

impl AcpBackend {
    /// Build an ACP backend from its configured executable and arguments.
    pub fn new(command: Vec<String>) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "ACP backend requires harness.backends.acp.command"
        );
        Ok(Self {
            command,
            connection: Mutex::new(None),
        })
    }

    async fn open_connection(
        &self,
        request: &BackendRequest,
        output: &mut BackendOutput,
        event_sink: Option<BackendEventSink>,
    ) -> Result<AcpConnection> {
        let mut process = JsonRpcProcess::start(
            &self.command,
            request.write_mode == WriteMode::Write,
            &request.workspace,
            request.trust_policy.clone(),
            event_sink,
        )
        .await?;
        let initialized = process.request("initialize", json!({
            "protocolVersion": 1,
            "clientCapabilities": {
                "fs": { "readTextFile": true, "writeTextFile": request.write_mode == WriteMode::Write },
                "terminal": true
            },
            "clientInfo": { "name": "diff-review-harness", "version": env!("CARGO_PKG_VERSION") }
        }), output).await?;
        Ok(AcpConnection {
            process,
            initialized,
            session: HashMap::new(),
            preview_session_id: None,
            native_compact: output.metrics.native_compact_update.unwrap_or(false),
        })
    }

    fn capability(initialized: &Value) -> BackendCapability {
        let encoded = initialized.to_string().to_ascii_lowercase();
        BackendCapability {
            native_fork: initialized
                .pointer("/agentCapabilities/sessionCapabilities/fork")
                .or_else(|| initialized.pointer("/agent_capabilities/session_capabilities/fork"))
                .and_then(Value::as_bool)
                .unwrap_or(false)
                || encoded.contains("session/fork")
                || encoded.contains("forksession")
                || encoded.contains("fork_session"),
            native_compact: false,
            native_steer: false,
            native_goal: false,
            model_selection: false,
            effort_selection: false,
            permission_control: true,
        }
    }

    fn config_option<'a>(session: &'a Value, category: &str) -> Option<&'a Value> {
        session
            .get("configOptions")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .find(|option| {
                let declared_category = option.get("category").and_then(Value::as_str);
                declared_category == Some(category)
                    || declared_category.is_none()
                        && option
                            .get("id")
                            .and_then(Value::as_str)
                            .is_some_and(|id| id.to_ascii_lowercase().contains(category))
            })
    }

    fn mode_state(session: &Value) -> Option<SessionModeState> {
        serde_json::from_value(session.get("modes")?.clone()).ok()
    }

    fn mode_id(session: &Value, preferred: &[&str]) -> Option<String> {
        let mode_state = Self::mode_state(session)?;
        for needle in preferred {
            if let Some(mode) = mode_state.available_modes.iter().find(|mode| {
                mode.id.to_string().eq_ignore_ascii_case(needle)
                    || mode.name.eq_ignore_ascii_case(needle)
            }) {
                return Some(mode.id.to_string());
            }
        }
        for needle in preferred {
            if let Some(mode) = mode_state.available_modes.iter().find(|mode| {
                let descriptor = format!(
                    "{} {} {}",
                    mode.id,
                    mode.name,
                    mode.description.as_deref().unwrap_or_default()
                )
                .to_ascii_lowercase();
                descriptor.contains(needle)
            }) {
                return Some(mode.id.to_string());
            }
        }
        None
    }

    fn desired_mode(session: &Value, request: &BackendRequest) -> Option<String> {
        if request.mode == crate::backend::PromptMode::Plan {
            None
        } else {
            Self::mode_id(session, &["code", "agent", "default", "build", "write"])
        }
    }

    fn provider_label(&self) -> String {
        let executable = self
            .command
            .first()
            .map(String::as_str)
            .unwrap_or("ACP agent");
        let stem = Path::new(executable)
            .file_stem()
            .and_then(|value| value.to_str())
            .unwrap_or(executable);
        if stem.eq_ignore_ascii_case("copilot") {
            "Copilot CLI (ACP)".into()
        } else {
            format!("{stem} (ACP)")
        }
    }

    fn runtime_model(session: &Value, request: &BackendRequest) -> Option<String> {
        if request.model != "default" {
            return Some(request.model.clone());
        }
        Self::config_option(session, "model")
            .and_then(|option| option.get("currentValue"))
            .and_then(Value::as_str)
            .filter(|model| !model.is_empty())
            .map(str::to_owned)
    }

    async fn configure_mode(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
        session: &mut Value,
        session_id: &str,
        request: &BackendRequest,
    ) -> Result<()> {
        let Some(mode_id) = Self::desired_mode(session, request) else {
            return Ok(());
        };
        let current_mode = session
            .pointer("/modes/currentModeId")
            .and_then(Value::as_str);
        if current_mode == Some(&mode_id) {
            return Ok(());
        }
        process
            .request(
                "session/set_mode",
                json!({ "sessionId": session_id, "modeId": mode_id }),
                output,
            )
            .await?;
        if let Some(current_mode) = session.pointer_mut("/modes/currentModeId") {
            *current_mode = Value::String(mode_id);
        }
        Ok(())
    }

    fn option_value_list(option: &Value) -> Vec<(String, String)> {
        fn collect(value: &Value, result: &mut Vec<(String, String)>) {
            if let Some(value_id) = value.get("value").and_then(Value::as_str) {
                result.push((
                    value_id.to_owned(),
                    value
                        .get("name")
                        .and_then(Value::as_str)
                        .unwrap_or(value_id)
                        .to_owned(),
                ));
            }
            if let Some(child_list) = value.as_array() {
                for child in child_list {
                    collect(child, result);
                }
            } else if let Some(map) = value.as_object() {
                for child in map.values() {
                    collect(child, result);
                }
            }
        }

        let mut result = Vec::new();
        if let Some(options) = option.get("options") {
            collect(options, &mut result);
        }
        result
    }

    async fn configure_option(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
        session_id: &str,
        option: Option<&Value>,
        value: &str,
    ) -> Result<()> {
        let Some(option) = option else { return Ok(()) };
        let current = option.get("currentValue").and_then(Value::as_str);
        if current == Some(value) {
            return Ok(());
        }
        let available = Self::option_value_list(option);
        anyhow::ensure!(
            available.iter().any(|(candidate, _)| candidate == value),
            "ACP config value is unavailable: {value}"
        );
        let config_id = option
            .get("id")
            .and_then(Value::as_str)
            .context("ACP config option omitted id")?;
        process
            .request(
                "session/set_config_option",
                json!({ "sessionId": session_id, "configId": config_id, "value": value }),
                output,
            )
            .await?;
        Ok(())
    }

    async fn configure_session(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
        session: &mut Value,
        session_id: &str,
        request: &BackendRequest,
    ) -> Result<()> {
        Self::configure_mode(process, output, session, session_id, request).await?;
        if request.model != "default" {
            Self::configure_option(
                process,
                output,
                session_id,
                Self::config_option(session, "model"),
                &request.model,
            )
            .await?;
        }
        Self::configure_option(
            process,
            output,
            session_id,
            Self::config_option(session, "thought_level"),
            &request.effort,
        )
        .await
    }

    fn configure_connection(
        connection: &mut AcpConnection,
        request: &BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) {
        connection.process.configure_request(
            request.write_mode == WriteMode::Write,
            &request.workspace,
            request.trust_policy.clone(),
            event_sink,
        );
    }

    fn session_id(session: &Value, fallback: Option<&str>) -> Result<String> {
        session
            .get("sessionId")
            .or_else(|| session.get("session_id"))
            .and_then(Value::as_str)
            .or(fallback)
            .map(str::to_owned)
            .context("ACP session response omitted sessionId")
    }

    async fn create_session(
        connection: &mut AcpConnection,
        request: &BackendRequest,
        output: &mut BackendOutput,
    ) -> Result<(String, Value)> {
        let session = connection
            .process
            .request(
                "session/new",
                json!({
                    "cwd": request.workspace,
                    "mcpServers": [Self::control_server()]
                }),
                output,
            )
            .await
            .context("create ACP session")?;
        let session_id = Self::session_id(&session, None)?;
        connection
            .session
            .insert(session_id.clone(), session.clone());
        Ok((session_id, session))
    }

    async fn load_session(
        connection: &mut AcpConnection,
        request: &BackendRequest,
        output: &mut BackendOutput,
        session_id: &str,
    ) -> Result<Value> {
        if let Some(session) = connection.session.get(session_id) {
            return Ok(session.clone());
        }
        anyhow::ensure!(
            connection
                .initialized
                .pointer("/agentCapabilities/loadSession")
                .or_else(|| connection
                    .initialized
                    .pointer("/agent_capabilities/load_session"))
                .and_then(Value::as_bool)
                .unwrap_or(false),
            "ACP agent cannot reload this persisted session; start a new Harness session"
        );
        let session = connection
            .process
            .request(
                "session/load",
                json!({
                    "sessionId": session_id,
                    "cwd": request.workspace,
                    "mcpServers": [Self::control_server()]
                }),
                output,
            )
            .await
            .with_context(|| format!("load ACP session {session_id}"))?;
        connection
            .session
            .insert(session_id.to_owned(), session.clone());
        Ok(session)
    }

    async fn prompt_session(
        connection: &mut AcpConnection,
        request: &BackendRequest,
        output: &mut BackendOutput,
    ) -> Result<(String, Value)> {
        if let Some(session_id) = request.backend_session_id.as_deref() {
            let session = Self::load_session(connection, request, output, session_id).await?;
            return Ok((session_id.to_owned(), session));
        }
        if let Some(session_id) = connection.preview_session_id.take()
            && let Some(session) = connection.session.get(&session_id)
        {
            return Ok((session_id, session.clone()));
        }
        Self::create_session(connection, request, output).await
    }

    async fn inspect_session(
        connection: &mut AcpConnection,
        request: &BackendRequest,
        output: &mut BackendOutput,
    ) -> Result<Value> {
        if let Some(session_id) = request.backend_session_id.as_deref() {
            return Self::load_session(connection, request, output, session_id).await;
        }
        if let Some(session_id) = connection.preview_session_id.as_deref()
            && let Some(session) = connection.session.get(session_id)
        {
            return Ok(session.clone());
        }
        let (session_id, session) = Self::create_session(connection, request, output).await?;
        connection.preview_session_id = Some(session_id);
        Ok(session)
    }

    fn control_server() -> Value {
        let executable = std::env::current_exe()
            .map(|path| path.to_string_lossy().into_owned())
            .unwrap_or_else(|_| "diff-review-harness".into());
        json!({
            "name": "diff-review-harness-control",
            "command": executable,
            "args": ["mcp"],
            "env": []
        })
    }

    fn prompt_text(request: &BackendRequest) -> String {
        let contract = match request.mode {
            crate::backend::PromptMode::Plan => return request.text.clone(),
            crate::backend::PromptMode::GoalContinuation
            | crate::backend::PromptMode::ExecutePlan => {
                "Continue until the active goal reaches a terminal state. Call harness_goal_complete or harness_goal_blocked. Ordinary prose does not complete the goal."
            }
            _ => {
                "Use harness_plan_question whenever the user explicitly requests interactive or multiple-choice questions. The question tool works outside planning. Do not claim questions were sent through prose. Use the other Harness control tools for plan and goal terminal state."
            }
        };
        format!("{contract}\n\n{}", request.text)
    }

    fn unsupported_model_error(error: &anyhow::Error) -> bool {
        let message = format!("{error:#}").to_ascii_lowercase();
        message.contains("requested model is not supported")
            || message.contains("requested model is unsupported")
    }

    fn should_retry_unsupported_model(
        request: &BackendRequest,
        already_retried: bool,
        error: &anyhow::Error,
    ) -> bool {
        !already_retried
            && request.backend_session_id.is_none()
            && Self::unsupported_model_error(error)
    }
}

#[async_trait]
impl Backend for AcpBackend {
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let mut retried_unsupported_model = false;
        let attempt_request = request;
        loop {
            let mut output = BackendOutput::default();
            let mut connection_guard = self.connection.lock().await;
            if connection_guard.is_none() {
                *connection_guard = Some(
                    self.open_connection(&attempt_request, &mut output, event_sink.clone())
                        .await?,
                );
            }
            let connection = connection_guard
                .as_mut()
                .context("ACP connection initialization failed")?;
            Self::configure_connection(connection, &attempt_request, event_sink.clone());
            let result = async {
                output.capability = Self::capability(&connection.initialized);
                output.capability.native_compact = connection.native_compact;
                let (session_id, mut session) =
                    Self::prompt_session(connection, &attempt_request, &mut output).await?;
                output.backend_session_id = Some(session_id.clone());
                output.capability.model_selection =
                    Self::config_option(&session, "model").is_some();
                output.capability.effort_selection =
                    Self::config_option(&session, "thought_level").is_some();
                output.runtime.provider = self.provider_label();
                output.runtime.model = Self::runtime_model(&session, &attempt_request);
                Self::configure_session(
                    &mut connection.process,
                    &mut output,
                    &mut session,
                    &session_id,
                    &attempt_request,
                )
                .await?;
                connection.session.insert(session_id.clone(), session);
                connection
                    .process
                    .request(
                        "session/prompt",
                        json!({
                            "sessionId": session_id,
                            "prompt": [{ "type": "text", "text": Self::prompt_text(&attempt_request) }]
                        }),
                        &mut output,
                    )
                    .await
                    .context("run ACP prompt")?;
                if let Some(native_compact) = output.metrics.native_compact_update {
                    connection.native_compact = native_compact;
                    output.capability.native_compact = native_compact;
                }
                Ok(output)
            }
            .await;
            match result {
                Ok(output) => return Ok(output),
                Err(error)
                    if Self::should_retry_unsupported_model(
                        &attempt_request,
                        retried_unsupported_model,
                        &error,
                    ) =>
                {
                    *connection_guard = None;
                    drop(connection_guard);
                    retried_unsupported_model = true;
                }
                Err(error) => {
                    *connection_guard = None;
                    if Self::unsupported_model_error(&error) {
                        return Err(error).context(
                            "ACP rejected its configured model after a fresh-session retry; select a supported model in the provider CLI",
                        );
                    }
                    return Err(error);
                }
            }
        }
    }

    async fn fork(&self, request: BackendRequest) -> Result<String> {
        let source = request
            .backend_session_id
            .clone()
            .context("ACP session has not started")?;
        let mut output = BackendOutput::default();
        let mut connection_guard = self.connection.lock().await;
        if connection_guard.is_none() {
            *connection_guard = Some(self.open_connection(&request, &mut output, None).await?);
        }
        let connection = connection_guard
            .as_mut()
            .context("ACP connection initialization failed")?;
        Self::configure_connection(connection, &request, None);
        let result = async {
            anyhow::ensure!(
                Self::capability(&connection.initialized).native_fork,
                "ACP agent does not advertise session/fork"
            );
            Self::load_session(connection, &request, &mut output, &source).await?;
            let fork = connection
                .process
                .request(
                    "session/fork",
                    json!({
                        "sessionId": source,
                        "cwd": request.workspace,
                        "mcpServers": [Self::control_server()]
                    }),
                    &mut output,
                )
                .await?;
            let session_id = Self::session_id(&fork, None)?;
            connection.session.insert(session_id.clone(), fork);
            Ok(session_id)
        }
        .await;
        if result.is_err() {
            *connection_guard = None;
        }
        result
    }

    async fn compact(&self, mut request: BackendRequest) -> Result<BackendOutput> {
        anyhow::ensure!(
            request.backend_session_id.is_some(),
            "ACP session has not started"
        );
        request.text = "/compact".into();
        self.prompt_stream(request, None).await
    }

    async fn cancel(&self) -> Result<()> {
        let mut connection_guard = self.connection.lock().await;
        *connection_guard = None;
        Ok(())
    }

    async fn model_list(&self, request: BackendRequest) -> Result<Vec<BackendModel>> {
        let mut output = BackendOutput::default();
        let mut connection_guard = self.connection.lock().await;
        if connection_guard.is_none() {
            *connection_guard = Some(self.open_connection(&request, &mut output, None).await?);
        }
        let connection = connection_guard
            .as_mut()
            .context("ACP connection initialization failed")?;
        Self::configure_connection(connection, &request, None);
        let result = async {
            let session = Self::inspect_session(connection, &request, &mut output).await?;
            let effort = Self::config_option(&session, "thought_level")
                .map(Self::option_value_list)
                .unwrap_or_default()
                .into_iter()
                .map(|(value, _)| value)
                .collect::<Vec<_>>();
            Ok(Self::config_option(&session, "model")
                .map(Self::option_value_list)
                .unwrap_or_default()
                .into_iter()
                .map(|(id, label)| BackendModel {
                    id,
                    label,
                    effort: effort.clone(),
                    is_default: false,
                })
                .collect())
        }
        .await;
        if result.is_err() {
            *connection_guard = None;
        }
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::{PromptMode, TrustPolicy};

    fn request(mode: PromptMode) -> BackendRequest {
        BackendRequest {
            workspace: "workspace".into(),
            text: "prompt".into(),
            mode,
            model: "default".into(),
            effort: "medium".into(),
            fast_mode: false,
            write_mode: WriteMode::Read,
            trust_profile: "workspace".into(),
            trust_policy: TrustPolicy::default(),
            backend_session_id: None,
        }
    }

    fn session() -> Value {
        json!({
            "modes": {
                "currentModeId": "code",
                "availableModes": [
                    { "id": "architect", "name": "Architect", "description": "Plan changes" },
                    { "id": "code", "name": "Code", "description": "Write changes" }
                ]
            }
        })
    }

    #[test]
    fn selects_plan_and_execution_modes_from_acp_session_state() {
        let session = session();
        assert_eq!(
            AcpBackend::desired_mode(&session, &request(PromptMode::Plan)),
            None
        );
        assert_eq!(
            AcpBackend::desired_mode(&session, &request(PromptMode::ExecutePlan)).as_deref(),
            Some("code")
        );
    }

    #[test]
    fn rejects_cosmetic_plan_capability_without_a_plan_mode() {
        let session = json!({
            "modes": {
                "currentModeId": "code",
                "availableModes": [
                    { "id": "code", "name": "Code", "description": "Write changes" }
                ]
            }
        });
        assert!(AcpBackend::mode_id(&session, &["plan", "architect"]).is_none());
    }

    #[test]
    fn recognizes_provider_model_rejection_for_one_fresh_session_retry() {
        let error = anyhow::anyhow!(
            "Execution failed: CAPIError: 400 The requested model is not supported."
        );
        assert!(AcpBackend::unsupported_model_error(&error));
        assert!(!AcpBackend::unsupported_model_error(&anyhow::anyhow!(
            "permission denied"
        )));
        let initial = request(PromptMode::Chat);
        assert!(AcpBackend::should_retry_unsupported_model(
            &initial, false, &error
        ));
        assert!(!AcpBackend::should_retry_unsupported_model(
            &initial, true, &error
        ));
        let mut established = initial;
        established.backend_session_id = Some("provider-session".into());
        assert!(!AcpBackend::should_retry_unsupported_model(
            &established,
            false,
            &error
        ));
    }
}
