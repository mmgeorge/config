use super::AcpKind as ProviderKind;
use super::session::AcpSessionState;
use super::{AcpConnection, AcpConnectionError, AcpLaunch};
use anyhow::{Context, Result, ensure};
use forge_provider_api::catalog::{McpDefinition, McpStatus};
use forge_provider_api::{ExecutionMode, ProviderCapability, ProviderError, ProviderModel};
use serde_json::{Value, json};
use std::path::PathBuf;
use std::sync::Arc;

/// Replace only the host control server while preserving admitted provider MCP launches.
pub(super) fn configuration_with_tool_endpoint(
    configuration: &serde_json::Value,
    endpoint: Option<&forge_provider_api::session::HostToolEndpoint>,
) -> Result<serde_json::Value, ProviderError> {
    let mut configuration = configuration.clone();
    let servers = configuration["mcpServers"]
        .as_array_mut()
        .ok_or_else(|| acp_failure(forge_provider_api::ProviderFailureKind::Protocol))?;
    servers.retain(|server| server["name"] != super::permission::HOST_TOOL_SERVER);
    if let Some(endpoint) = endpoint {
        servers.insert(0, json!({
            "type":"http",
            "name":super::permission::HOST_TOOL_SERVER,
            "url":endpoint.url.clone(),
            "headers":endpoint.headers.iter().map(|(name, value)| json!({"name":name,"value":value})).collect::<Vec<_>>(),
        }));
    }
    Ok(configuration)
}

/// Native session configuration prepared by the host's resource owner.
pub struct ConversationOptions {
    pub kind: ProviderKind,
    pub workspace: String,
    pub session_id: Option<String>,
    pub control_endpoint: Option<forge_provider_api::session::HostToolEndpoint>,
    pub mcp_servers: Vec<Value>,
    pub resources: Vec<McpDefinition>,
    pub profile: PathBuf,
    pub skills: Vec<String>,
    /// Preserve host base instructions independently from per-invocation guidance.
    pub system_instructions: String,
}

/// Prepare the complete native target configuration for one durable fork.
pub(super) struct ForkConfiguration<'a> {
    pub workspace: &'a str,
    pub mcp_servers: Vec<Value>,
    pub skills: &'a [String],
    pub profile: &'a std::path::Path,
    pub model: Option<&'a str>,
    pub effort: Option<&'a str>,
    /// Replace source instructions with the separately owned target's instructions.
    pub system_instructions: &'a str,
}

enum InstructionDelivery {
    NativeAppend { resumed_prelude: Option<String> },
    PromptPrelude(String),
}

/// Own the connection, negotiated capabilities, and native session configuration.
pub struct AcpConversation {
    id: String,
    transport: AcpConnection,
    state: Arc<AcpSessionState>,
    initialized: Value,
    configuration: tokio::sync::Mutex<Value>,
    instructions: InstructionDelivery,
    fork_targets:
        tokio::sync::Mutex<std::collections::HashMap<forge_provider_api::OperationId, String>>,
}

impl AcpConversation {
    /// Negotiate and open native history without owning host tools or policy.
    pub(super) async fn open(
        launch: AcpLaunch<'_>,
        state: Arc<AcpSessionState>,
        options: ConversationOptions,
        cleanup: &super::connection::AcpCleanup,
    ) -> Result<Self, ProviderError> {
        let runtime = PathBuf::from(launch.command.first().map(String::as_str).unwrap_or("ACP"));
        let requirement = runtime_requirement(options.kind, runtime)?;
        let transport = AcpConnection::launch_retained(launch, state.clone(), cleanup)
            .await
            .map_err(|source| initialization_failure(&requirement, source))?;
        let result: Result<(String, Value, Value, InstructionDelivery)> = async {
        let initialized = transport.request("initialize", json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","title":"Forge Harness","version":"0.1.0"},
            "clientCapabilities":{"session":{"compaction":{},"configOptions":{"boolean":{}},"notices":{}},"subagents":{}}})).await.map_err(|source| initialization_failure(&requirement, source))?;
        validate_initialization(&requirement, &initialized)?;
        ensure!(
            initialized
                .pointer("/agentCapabilities/loadSession")
                .and_then(Value::as_bool)
                == Some(true),
            "ACP agent cannot resume conversations, which Forge requires"
        );
        let mut servers = Vec::new();
        if let Some(endpoint) = &options.control_endpoint {
            let headers = endpoint.headers.iter().map(|(name, value)| json!({"name":name,"value":value})).collect::<Vec<_>>();
            servers.push(json!({"type":"http","name":super::permission::HOST_TOOL_SERVER,"url":endpoint.url,"headers":headers}));
        }
        let mut parameter = json!({"cwd":options.workspace,"mcpServers":servers});
        let instructions = if initialized.pointer("/agentCapabilities/sessionCapabilities/systemPrompt").is_some_and(Value::is_object) {
            append_system_instructions(&mut parameter, &options.system_instructions);
            // Claude retains its original system prompt on resume until compaction.
            // Supply current instructions as input too so resume cannot defer the host contract.
            InstructionDelivery::NativeAppend {
                resumed_prelude: options.session_id.as_ref().map(|_| options.system_instructions.clone()),
            }
        } else {
            InstructionDelivery::PromptPrelude(options.system_instructions)
        };
        parameter["mcpServers"]
            .as_array_mut()
            .context("ACP MCP list missing")?
            .extend(options.mcp_servers);
        let resource_status: Vec<_> = options.resources.iter().map(|server| {
            json!({"id":server.resource_id,"name":server.name,
                "state":if server.status == McpStatus::Loading { "requested" } else if !server.enabled { "disabled" } else { "unavailable" },
                "reason":server.status_detail})
        }).collect();
        state
            .update_catalog(json!({"mcpResources": resource_status}))
            .await;
        if options.kind == ProviderKind::Claude {
            // ACP 0.99.1 passes SDK 0.3.312 options through on new/load/fork.
            // Verbatim input blocks slash-command dispatch; denied tools cannot start native goals.
            parameter["_meta"] = json!({"claudeCode":{"emitRawSDKMessages":true,"options":{
                "strictMcpConfig":true,"verbatimPrompts":true,
                "disallowedTools":["Agent","Task","ProposeGoal","CronCreate","RemoteTrigger","ScheduleWakeup"]}}});
            parameter["_meta"]["claudeCode"]["options"]["settings"] =
                json!({"permissions":{"ask":["*"]}});
            parameter["_meta"]["claudeCode"]["options"]["plugins"] = json!([
                {"type":"local","path":options.profile}
            ]);
            parameter["_meta"]["claudeCode"]["options"]["skills"] = json!(
                options.skills.iter().map(|name| format!("forge-managed:{name}"))
                    .collect::<Vec<_>>()
            );
        } else {
            // ACP 1.4.5 exposes schedule by default, including through read-only policy.
            // Its persisted built-in filter keeps continuation authority with Harness.
            parameter["_meta"] = json!({"antigravity":{"disabledTools":["schedule"]}});
        }
        let response = if let Some(id) = &options.session_id {
            parameter["sessionId"] = json!(id);
            ensure!(
                initialized
                    .pointer("/agentCapabilities/loadSession")
                    .and_then(Value::as_bool)
                    == Some(true),
                "ACP agent cannot load the saved conversation"
            );
            transport.request("session/load", parameter.clone()).await?
        } else {
            transport.request("session/new", parameter.clone()).await?
        };
        let id = response["sessionId"]
            .as_str()
            .filter(|id| !id.trim().is_empty())
            .map(str::to_owned)
            .or_else(|| options.session_id.clone())
            .context("ACP session response has no conversation identity")?;
        if let Some(expected) = &options.session_id { ensure!(&id == expected, "ACP loaded a different conversation identity"); }
        state.update_catalog(response.clone()).await;
        Ok((id, initialized, parameter, instructions))
        }.await;
        match result {
            Ok((id, initialized, configuration, instructions)) => Ok(Self {
                id,
                transport,
                state,
                initialized,
                configuration: tokio::sync::Mutex::new(configuration),
                instructions,
                fork_targets: tokio::sync::Mutex::new(std::collections::HashMap::new()),
            }),
            Err(error) => {
                // Keep a login-required rejection distinct so the user learns to sign in
                // through the provider CLI instead of seeing a generic startup failure.
                let kind = match error.downcast_ref::<AcpConnectionError>() {
                    Some(AcpConnectionError::Remote {
                        failure: Some(super::connection::AcpRemoteFailure::AuthenticationRequired),
                        ..
                    }) => forge_provider_api::ProviderFailureKind::Authentication,
                    _ => forge_provider_api::ProviderFailureKind::Initialization,
                };
                let cause = acp_failure(kind);
                state.invalidate_binding(cause.clone()).await;
                transport.retain_failure(cause.clone()).await;
                match transport.shutdown().await {
                    Ok(()) => Err(cause),
                    Err(cleanup) => Err(ProviderError::PreparationCleanup {
                        cause: Box::new(cause),
                        cleanup: Box::new(crate::error::acp(
                            cleanup,
                            forge_provider_api::ProviderOperation::ThreadOpen,
                        )),
                    }),
                }
            }
        }
    }

    /// Get the negotiated provider conversation identity.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Report the native adapter version from the initialization handshake.
    pub fn version(&self) -> Option<String> {
        self.initialized
            .pointer("/agentInfo/version")
            .and_then(Value::as_str)
            .map(str::to_owned)
    }

    /// Report native image support before the host prepares attachments.
    pub fn supports_images(&self) -> bool {
        self.initialized
            .pointer("/agentCapabilities/promptCapabilities/image")
            .and_then(Value::as_bool)
            == Some(true)
    }

    /// Report whether the native adapter supports resuming conversation history.
    pub fn supports_resume(&self) -> bool {
        self.initialized
            .pointer("/agentCapabilities/loadSession")
            .and_then(Value::as_bool)
            == Some(true)
    }

    /// Get immediate prompt guidance when native delivery is unavailable or deferred on resume.
    pub(super) fn prompt_instructions(&self) -> Option<&str> {
        match &self.instructions {
            InstructionDelivery::NativeAppend { resumed_prelude } => {
                resumed_prelude.as_deref().filter(|text| !text.is_empty())
            }
            InstructionDelivery::PromptPrelude(text) => (!text.is_empty()).then_some(text),
        }
    }

    /// Reject a fork whose target instructions cannot be retained by the native history.
    pub(super) fn validate_fork_instructions(
        &self,
        instructions: &str,
    ) -> Result<(), ProviderError> {
        if !instructions.is_empty()
            && matches!(self.instructions, InstructionDelivery::PromptPrelude(_))
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeFork,
            )
            .into());
        }
        Ok(())
    }

    /// Send a native operation while preserving its transport error category.
    pub async fn request(&self, method: &str, parameter: Value) -> Result<Value, ProviderError> {
        self.transport
            .request(method, parameter)
            .await
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadCompact)
            })
    }

    /// Reload retained history with the exact invocation-owned host MCP endpoint.
    ///
    /// ACP exposes MCP launches when a session is created or loaded, so replacing the
    /// endpoint requires reloading the existing provider session before accepting input.
    /// Resource-owned MCP launches remain intact while the host callback server is swapped.
    pub async fn bind_tool_endpoint(
        &self,
        endpoint: Option<&forge_provider_api::session::HostToolEndpoint>,
    ) -> Result<(), ProviderError> {
        let configuration = {
            let current_configuration = self.configuration.lock().await;
            let current = current_configuration["mcpServers"]
                .as_array()
                .and_then(|servers| {
                    servers
                        .iter()
                        .find(|server| server["name"] == super::permission::HOST_TOOL_SERVER)
                });
            let desired = endpoint.map(|endpoint| json!({
                "type":"http",
                "name":super::permission::HOST_TOOL_SERVER,
                "url":endpoint.url.clone(),
                "headers":endpoint.headers.iter().map(|(name, value)| json!({"name":name,"value":value})).collect::<Vec<_>>(),
            }));
            if current == desired.as_ref() {
                return Ok(());
            }
            if !self.supports_resume() {
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::McpLaunchRequirements,
                )
                .into());
            }

            configuration_with_tool_endpoint(&current_configuration, endpoint)?
        };
        let mut configuration = configuration;
        configuration["sessionId"] = json!(self.id);
        let _history_replay = self.state.suppress_history_replay()?;
        let response = self
            .transport
            .request("session/load", configuration.clone())
            .await
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::TurnStart)
            })
            .and_then(|response| {
                if response["sessionId"].as_str() == Some(self.id.as_str()) {
                    Ok(response)
                } else {
                    Err(ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Protocol,
                        ),
                    ))
                }
            });
        let response = match response {
            Ok(response) => response,
            Err(error) => {
                let kind = match &error {
                    ProviderError::Failure(failure) => failure.kind(),
                    _ => forge_provider_api::ProviderFailureKind::Unknown,
                };
                // session/load may have installed this callback URL even when its response is lost or invalid.
                let cause = ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        kind,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Preparation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
                );
                self.state.invalidate_binding(cause.clone()).await;
                self.transport.retain_failure(cause.clone()).await;
                return Err(cause);
            }
        };
        *self.configuration.lock().await = configuration;
        self.state.update_catalog(response).await;
        Ok(())
    }

    /// Reload the same durable history with a complete resource set and preserved host endpoint.
    pub(super) async fn configure_resources(
        &self,
        servers: Vec<Value>,
        skills: &[String],
        profile: &std::path::Path,
    ) -> Result<(), ProviderError> {
        let mut configuration = self.configuration.lock().await.clone();
        let endpoint = configuration["mcpServers"]
            .as_array()
            .and_then(|servers| {
                servers
                    .iter()
                    .find(|server| server["name"] == super::permission::HOST_TOOL_SERVER)
            })
            .cloned();
        let mut servers = servers;
        if let Some(endpoint) = endpoint {
            servers.insert(0, endpoint);
        }
        configuration["mcpServers"] = json!(servers);
        configuration["sessionId"] = json!(self.id);
        if let Some(options) = configuration.pointer_mut("/_meta/claudeCode/options") {
            options["plugins"] = json!([{"type":"local","path":profile}]);
            options["skills"] = json!(
                skills
                    .iter()
                    .map(|name| format!("forge-managed:{name}"))
                    .collect::<Vec<_>>()
            );
        }
        let _history_replay = self.state.suppress_history_replay()?;
        let response = self
            .transport
            .request("session/load", configuration.clone())
            .await
            .map_err(|error| {
                crate::error::acp(
                    error,
                    forge_provider_api::ProviderOperation::ThreadConfigure,
                )
            })?;
        if response["sessionId"].as_str() != Some(self.id.as_str()) {
            return Err(acp_failure(
                forge_provider_api::ProviderFailureKind::Protocol,
            ));
        }
        *self.configuration.lock().await = configuration;
        self.state.update_catalog(response).await;
        Ok(())
    }

    /// Commit native prompt or steering evidence through the host's receipt.
    pub async fn submit(
        &self,
        method: super::submission::AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn forge_provider_api::submission::InputReceipt>>,
        dispatch: &forge_provider_api::submission::DispatchReporter,
        cancellation: &tokio_util::sync::CancellationToken,
    ) -> Result<Value, ProviderError> {
        self.transport
            .submit(
                method,
                parameter,
                receipt,
                Some(dispatch),
                Some(cancellation),
            )
            .await
    }

    /// Get native response evidence retained independently of receipt persistence.
    pub(super) fn prompt_end(
        &self,
        input: &forge_provider_api::CoveredInput,
    ) -> Option<super::response::NativePromptEnd> {
        self.transport.prompt_end(input)
    }

    /// Submit steering and retain the adapter's typed lifecycle limitation.
    pub async fn steer(
        &self,
        parameter: Value,
        receipt: Option<Arc<dyn forge_provider_api::submission::InputReceipt>>,
    ) -> Result<super::submission::SteeringAcceptance, ProviderError> {
        self.transport.submit_steering(parameter, receipt).await
    }

    /// Interrupt this conversation while retaining the outstanding prompt's terminal response.
    pub fn cancel(&self) -> Result<(), ProviderError> {
        self.transport.cancel(&self.id).map_err(|error| {
            crate::error::acp(error, forge_provider_api::ProviderOperation::TurnInterrupt)
        })
    }

    /// Stop and collect the owned native process.
    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.transport.shutdown().await.map_err(|error| {
            crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadClose)
        })
    }

    /// Fork the negotiated configuration and reject a reused source identity.
    pub(super) async fn fork(
        &self,
        operation_id: &forge_provider_api::OperationId,
        target: ForkConfiguration<'_>,
    ) -> Result<String, ProviderError> {
        let ForkConfiguration {
            workspace,
            mcp_servers,
            skills,
            profile,
            model,
            effort,
            system_instructions,
        } = target;
        self.validate_fork_instructions(system_instructions)?;
        if !self
            .initialized
            .pointer("/agentCapabilities/sessionCapabilities/fork")
            .is_some_and(Value::is_object)
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeFork,
            )
            .into());
        }
        async {
            let mut configuration = self.configuration.lock().await.clone();
            configuration["sessionId"] = json!(self.id);
            configuration["cwd"] = json!(workspace);
            configuration["mcpServers"] = json!(mcp_servers);
            if matches!(self.instructions, InstructionDelivery::NativeAppend { .. }) {
                append_system_instructions(&mut configuration, system_instructions);
            }
            if let Some(options) = configuration.pointer_mut("/_meta/claudeCode/options") {
                options["plugins"] = json!([{"type":"local","path":profile}]);
                options["skills"] = json!(
                    skills
                        .iter()
                        .map(|name| format!("forge-managed:{name}"))
                        .collect::<Vec<_>>()
                );
            }
            let response = self
                .transport
                .request("session/fork", configuration)
                .await
                .map_err(|error| {
                    crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadFork)
                })?;
            let id = response["sessionId"]
                .as_str()
                .filter(|id| !id.trim().is_empty())
                .ok_or_else(|| acp_failure(forge_provider_api::ProviderFailureKind::Protocol))?
                .to_owned();
            if id == self.id {
                return Err(acp_failure(
                    forge_provider_api::ProviderFailureKind::Protocol,
                ));
            }
            self.fork_targets
                .lock()
                .await
                .insert(operation_id.clone(), id.clone());
            let catalog = self.state.catalog().await.clone();
            if let Some(model) = model {
                if let Some(option) = find_option(&catalog, "model") {
                    self.transport
                        .request(
                            "session/set_config_option",
                            json!({
                                "sessionId":id,"configId":option["id"],"value":model
                            }),
                        )
                        .await
                        .map_err(|error| {
                            crate::error::acp(
                                error,
                                forge_provider_api::ProviderOperation::ThreadFork,
                            )
                        })?;
                } else {
                    self.transport
                        .request("session/set_model", json!({"sessionId":id,"modelId":model}))
                        .await
                        .map_err(|error| {
                            crate::error::acp(
                                error,
                                forge_provider_api::ProviderOperation::ThreadFork,
                            )
                        })?;
                }
            }
            if let Some(effort) = effort {
                let option = find_option(&catalog, "thought_level")
                    .or_else(|| find_option(&catalog, "effort"))
                    .ok_or({
                        forge_provider_api::capability::UnsupportedCapability(
                            forge_provider_api::capability::ProviderFeature::EffortSelection,
                        )
                    })?;
                self.transport
                    .request(
                        "session/set_config_option",
                        json!({
                            "sessionId":id,"configId":option["id"],"value":effort
                        }),
                    )
                    .await
                    .map_err(|error| {
                        crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadFork)
                    })?;
            }
            Ok(id)
        }
        .await
    }

    /// Apply native model, reasoning, and plan mode settings.
    pub async fn configure(&self, model: &str, effort: &str, mode: ExecutionMode) -> Result<()> {
        let catalog = self.state.catalog().await.clone();
        if model != "default" && !model.is_empty() {
            if let Some(option) = find_option(&catalog, "model") {
                let response = self
                    .transport
                    .request(
                        "session/set_config_option",
                        json!({"sessionId":self.id,
                    "configId":option["id"],"value":model}),
                    )
                    .await?;
                if let Some(options) = response.get("configOptions") {
                    self.state
                        .update_catalog(json!({"configOptions": options}))
                        .await;
                }
            } else {
                self.transport
                    .request(
                        "session/set_model",
                        json!({"sessionId":self.id,"modelId":model}),
                    )
                    .await?;
            }
        }
        if let Some(option) =
            find_option(&catalog, "thought_level").or_else(|| find_option(&catalog, "effort"))
            && !effort.is_empty()
            && effort != "default"
        {
            self.transport
                .request(
                    "session/set_config_option",
                    json!({"sessionId":self.id,
                "configId":option["id"],"value":effort}),
                )
                .await?;
        } else if effort != "default" {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::EffortSelection,
            )
            .into());
        }
        let modes = catalog
            .pointer("/modes/availableModes")
            .and_then(Value::as_array);
        let planning = mode == ExecutionMode::Planning;
        let plan_mode = if planning {
            ["plan", "architect"]
                .into_iter()
                .find(|id| modes.is_some_and(|modes| modes.iter().any(|mode| mode["id"] == *id)))
        } else {
            None
        };
        let desired = plan_mode.unwrap_or("default");
        if modes.is_some_and(|modes| modes.iter().any(|mode| mode["id"] == desired)) {
            self.transport
                .request(
                    "session/set_mode",
                    json!({"sessionId":self.id,"modeId":desired}),
                )
                .await?;
        }
        Ok(())
    }
    /// Refine the host descriptor using negotiated native capabilities.
    pub async fn capabilities(&self, mut capability: ProviderCapability) -> ProviderCapability {
        capability.native_fork &=
            matches!(self.instructions, InstructionDelivery::NativeAppend { .. })
                && self
                    .initialized
                    .pointer("/agentCapabilities/sessionCapabilities/fork")
                    .is_some_and(Value::is_object);
        capability.native_steer &= self
            .initialized
            .pointer("/_meta/steering/supported")
            .and_then(Value::as_bool)
            == Some(true);
        capability.agent.observe = self
            .initialized
            .pointer("/agentCapabilities/sessionCapabilities/subagents")
            .is_some_and(Value::is_object);
        let catalog = self.state.catalog().await;
        capability.effort_selection = find_option(&catalog, "thought_level")
            .or_else(|| find_option(&catalog, "effort"))
            .is_some();
        capability
    }

    /// Capture concrete native defaults before task-specific model overrides can replace them.
    pub(super) async fn initial_model(&self) -> forge_provider_api::ModelSelection {
        let catalog = self.state.catalog().await;
        forge_provider_api::ModelSelection {
            model: current_model_id(&catalog).map(str::to_owned),
            reasoning: find_option(&catalog, "thought_level")
                .or_else(|| find_option(&catalog, "effort"))
                .and_then(|option| option["currentValue"].as_str())
                .map(|effort| {
                    forge_provider_api::generation::ReasoningSelection::Level(effort.to_owned())
                })
                .unwrap_or_default(),
            ..Default::default()
        }
    }

    /// Decode native model and reasoning choices for the host catalog.
    pub async fn models(&self) -> Result<Vec<ProviderModel>> {
        let catalog = self.state.catalog().await;
        let vision = self
            .initialized
            .pointer("/agentCapabilities/promptCapabilities/image")
            .and_then(Value::as_bool)
            == Some(true);
        Ok(decode_model_catalog(&catalog, vision))
    }
}

/// Decode a session catalog's model entries from the legacy model state or the `model` config option.
///
/// Both shapes carry a human-readable `name` beside the selection value. Claude's adapter versions
/// that name from the alias's resolved model (`opus` is named `Opus 5.5`), so it becomes the label
/// while the alias stays the `id` that model selection sends back.
pub(super) fn decode_model_catalog(catalog: &Value, vision: bool) -> Vec<ProviderModel> {
    let model_list = catalog
        .pointer("/models/availableModels")
        .and_then(Value::as_array)
        .or_else(|| find_option(catalog, "model").and_then(|option| option["options"].as_array()));
    let current_model = current_model_id(catalog);
    let reasoning: Vec<String> = find_option(catalog, "thought_level")
        .or_else(|| find_option(catalog, "effort"))
        .and_then(|option| option["options"].as_array())
        .into_iter()
        .flatten()
        .filter_map(|option| option["value"].as_str().map(str::to_owned))
        .collect();
    let non_empty = |value: &Value| {
        value
            .as_str()
            .filter(|text| !text.trim().is_empty())
            .map(str::to_owned)
    };
    model_list
        .into_iter()
        .flatten()
        .filter_map(|model| {
            let id = model["modelId"]
                .as_str()
                .or_else(|| model["value"].as_str())?;
            Some(ProviderModel {
                service_tiers: vec![forge_provider_api::ServiceTier::Standard],
                id: id.into(),
                label: non_empty(&model["name"]),
                reasoning: reasoning.clone(),
                default_reasoning: None,
                selected_reasoning: None,
                context_window: Vec::new(),
                default_context_window: None,
                selected_context_window: None,
                vision,
                description: non_empty(&model["description"]),
                is_default: current_model == Some(id),
            })
        })
        .collect()
}

/// Read the session's selected model from the legacy model state or the newer `model` config option.
pub(super) fn current_model_id(catalog: &Value) -> Option<&str> {
    catalog
        .pointer("/models/currentModelId")
        .and_then(Value::as_str)
        .or_else(|| {
            find_option(catalog, "model").and_then(|option| option["currentValue"].as_str())
        })
}

/// Create the adapter-owned requirement shared by setup and native session initialization.
pub fn runtime_requirement(
    kind: ProviderKind,
    runtime: PathBuf,
) -> Result<forge_provider_api::version::RuntimeRequirement, ProviderError> {
    let minimum = match kind {
        ProviderKind::Claude => semver::Version::new(0, 99, 1),
        ProviderKind::Antigravity => semver::Version::new(1, 4, 5),
    };
    Ok(forge_provider_api::version::RuntimeRequirement::new(
        kind.id(),
        runtime,
        minimum,
    ))
}

/// Preserve the native initialize failure with its version-probe classification and requirement.
pub fn initialization_failure(
    requirement: &forge_provider_api::version::RuntimeRequirement,
    source: AcpConnectionError,
) -> ProviderError {
    match source {
        AcpConnectionError::Configuration(_)
        | AcpConnectionError::Process { .. }
        | AcpConnectionError::Startup { .. }
        | AcpConnectionError::Protocol { .. }
        | AcpConnectionError::Remote { .. }
        | AcpConnectionError::Callback(_)
        | AcpConnectionError::Collection { .. } => requirement
            .probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable)
            .into(),
    }
}

/// Validate the reported runtime and selected ACP protocol before session or authentication use.
pub fn validate_initialization(
    requirement: &forge_provider_api::version::RuntimeRequirement,
    initialized: &Value,
) -> Result<forge_provider_api::version::ValidatedRuntime, ProviderError> {
    let validated = requirement.validate(
        initialized
            .pointer("/agentInfo/version")
            .and_then(Value::as_str),
    )?;
    if initialized["protocolVersion"] != 1 {
        return Err(acp_failure(
            forge_provider_api::ProviderFailureKind::Protocol,
        ));
    }
    Ok(validated)
}

/// Encode negotiated ACP append semantics while preserving the native provider preset.
fn append_system_instructions(configuration: &mut Value, instructions: &str) {
    configuration["systemPrompt"] = json!({
        "mode": "append", "content": [{ "type": "text", "text": instructions }],
    });
}

fn acp_failure(kind: forge_provider_api::ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::ThreadOpen,
        kind,
    ))
}

fn find_option<'catalog>(catalog: &'catalog Value, category: &str) -> Option<&'catalog Value> {
    catalog["configOptions"]
        .as_array()?
        .iter()
        .find(|option| option["category"] == category || option["id"] == category)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn legacy_model_state_labels_aliases_with_versioned_names() {
        let catalog = json!({
            "models":{
                "currentModelId":"opus",
                "availableModels":[
                    {"modelId":"opus","name":"Opus 5.5","description":"Most capable for complex work"},
                    {"modelId":"haiku","name":" ","description":""},
                ],
            },
            "configOptions":[{"id":"effort","options":[{"value":"low"},{"value":"high"}]}],
        });

        let models = decode_model_catalog(&catalog, true);

        assert_eq!(models.len(), 2);
        assert_eq!(models[0].id, "opus");
        assert_eq!(models[0].label.as_deref(), Some("Opus 5.5"));
        assert_eq!(
            models[0].description.as_deref(),
            Some("Most capable for complex work")
        );
        assert!(models[0].is_default);
        assert!(models[0].vision);
        assert_eq!(models[0].reasoning, ["low", "high"]);
        assert_eq!(models[1].id, "haiku");
        assert_eq!(models[1].label, None);
        assert_eq!(models[1].description, None);
        assert!(!models[1].is_default);
    }

    #[test]
    fn model_config_option_labels_values_with_option_names() {
        let catalog = json!({
            "configOptions":[{
                "id":"model","category":"model","currentValue":"sonnet",
                "options":[{"value":"sonnet","name":"Sonnet 5.5"},{"value":"custom-model"}],
            }],
        });

        let models = decode_model_catalog(&catalog, false);

        assert_eq!(models[0].id, "sonnet");
        assert_eq!(models[0].label.as_deref(), Some("Sonnet 5.5"));
        assert!(models[0].is_default);
        assert_eq!(models[1].id, "custom-model");
        assert_eq!(models[1].label, None);
    }

    #[test]
    fn absent_label_is_omitted_from_the_host_payload() {
        let models = decode_model_catalog(
            &json!({"models":{"availableModels":[{"modelId":"plain"}]}}),
            false,
        );

        let payload = serde_json::to_value(&models[0]).expect("model serializes");

        assert!(payload.get("label").is_none());
    }
}
