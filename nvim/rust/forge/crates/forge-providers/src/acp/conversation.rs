use super::session::AcpSessionState;
use super::{AcpConnection, AcpLaunch};
use anyhow::{Context, Result, ensure};
use forge_provider_api::catalog::{McpDefinition, McpStatus};
use forge_provider_api::{
    PromptMode, ProviderCapability, ProviderError, ProviderKind, ProviderModel,
};
use serde_json::{Value, json};
use std::path::PathBuf;
use std::sync::Arc;

/// Native session configuration prepared by the host's resource owner.
pub struct ConversationOptions {
    pub kind: ProviderKind,
    pub workspace: String,
    pub session_id: Option<String>,
    pub control_url: String,
    pub mcp_servers: Vec<Value>,
    pub resources: Vec<McpDefinition>,
    pub profile: PathBuf,
    pub skills: Vec<String>,
}

/// Own the connection, negotiated capabilities, and native session configuration.
pub struct AcpConversation {
    id: String,
    transport: AcpConnection,
    state: Arc<AcpSessionState>,
    initialized: Value,
    configuration: Value,
}

impl AcpConversation {
    /// Negotiate and open native history without owning host tools or policy.
    pub async fn open(
        launch: AcpLaunch<'_>,
        state: Arc<AcpSessionState>,
        options: ConversationOptions,
    ) -> Result<Self, ProviderError> {
        let transport = AcpConnection::launch(launch, state.clone()).await?;
        let result: Result<Self> = async {
        let initialized = transport.request("initialize", json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","title":"Forge Harness","version":"0.1.0"},
            "clientCapabilities":{"session":{"compaction":{},"configOptions":{"boolean":{}}},"subagents":{}}})).await?;
        ensure!(
            initialized["protocolVersion"] == 1,
            "ACP agent did not negotiate protocol v1"
        );
        ensure!(
            initialized
                .pointer("/agentCapabilities/loadSession")
                .and_then(Value::as_bool)
                == Some(true),
            "ACP agent cannot resume conversations, which Forge requires"
        );
        let mut parameter = json!({"cwd":options.workspace,"mcpServers":[{"type":"http",
            "name":"forge-harness-control","url":options.control_url,"headers":[]}]});
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
            parameter["_meta"] = json!({"claudeCode":{"emitRawSDKMessages":true,"options":{
                "strictMcpConfig":true,"disallowedTools":["Agent","Task"]}}});
            parameter["_meta"]["claudeCode"]["options"]["settings"] =
                json!({"permissions":{"ask":["*"]}});
            parameter["_meta"]["claudeCode"]["options"]["plugins"] = json!([
                {"type":"local","path":options.profile}
            ]);
            parameter["_meta"]["claudeCode"]["options"]["skills"] = json!(
                options.skills.iter().map(|name| format!("forge-managed:{name}"))
                    .collect::<Vec<_>>()
            );
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
            .map(str::to_owned)
            .or(options.session_id)
            .context("ACP session response has no conversation identity")?;
        state.update_catalog(response.clone()).await;
        Ok(Self { id, transport, state, initialized, configuration: parameter })
        }.await;
        result.map_err(ProviderError::from)
    }

    /// Get the negotiated provider conversation identity.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Get the native event owner without exposing its internal state.
    pub fn state(&self) -> &Arc<AcpSessionState> {
        &self.state
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

    /// Send a native operation while preserving its transport error category.
    pub async fn request(&self, method: &str, parameter: Value) -> Result<Value, ProviderError> {
        Ok(self.transport.request(method, parameter).await?)
    }

    /// Commit native prompt or steering evidence through the host's receipt.
    pub async fn submit(
        &self,
        method: super::submission::AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn forge_provider_api::submission::InputReceipt>>,
    ) -> Result<Value, ProviderError> {
        self.transport.submit(method, parameter, receipt).await
    }

    /// Stop and collect the owned native process.
    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        Ok(self.transport.shutdown().await?)
    }

    /// Fork the negotiated configuration and reject a reused source identity.
    pub async fn fork(&self, workspace: &str) -> Result<String, ProviderError> {
        let result: Result<String> = async {
            ensure!(
                self.initialized
                    .pointer("/agentCapabilities/sessionCapabilities/fork")
                    .is_some(),
                "Claude ACP did not advertise fork"
            );
            let mut configuration = self.configuration.clone();
            configuration["sessionId"] = json!(self.id);
            configuration["cwd"] = json!(workspace);
            let response = self
                .transport
                .request("session/fork", configuration)
                .await?;
            let id = response["sessionId"]
                .as_str()
                .context("ACP fork did not create a conversation")?;
            ensure!(id != self.id, "ACP fork returned the source conversation");
            Ok(id.to_owned())
        }
        .await;
        result.map_err(ProviderError::from)
    }

    /// Apply native model, reasoning, and plan mode settings.
    pub async fn configure(&self, model: &str, effort: &str, mode: PromptMode) -> Result<()> {
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
        }
        let modes = catalog
            .pointer("/modes/availableModes")
            .and_then(Value::as_array);
        let planning = matches!(
            mode,
            PromptMode::Plan | PromptMode::PlanDiscussion | PromptMode::PlanQuestion
        );
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
        capability.native_fork &= self
            .initialized
            .pointer("/agentCapabilities/sessionCapabilities/fork")
            .is_some();
        capability.native_steer &= self
            .initialized
            .pointer("/_meta/steering/supported")
            .and_then(Value::as_bool)
            == Some(true);
        capability.agent.observe = self
            .initialized
            .pointer("/agentCapabilities/sessionCapabilities/subagents")
            .is_some();
        let catalog = self.state.catalog().await;
        capability.effort_selection = find_option(&catalog, "thought_level")
            .or_else(|| find_option(&catalog, "effort"))
            .is_some();
        capability
    }

    /// Decode native model and reasoning choices for the host catalog.
    pub async fn models(&self) -> Result<Vec<ProviderModel>> {
        let catalog = self.state.catalog().await;
        let model_list = catalog
            .pointer("/models/availableModels")
            .and_then(Value::as_array)
            .or_else(|| {
                find_option(&catalog, "model").and_then(|option| option["options"].as_array())
            });
        let mut result = Vec::new();
        for model in model_list.into_iter().flatten() {
            let Some(id) = model["modelId"]
                .as_str()
                .or_else(|| model["value"].as_str())
            else {
                continue;
            };
            let reasoning: Vec<String> = find_option(&catalog, "thought_level")
                .or_else(|| find_option(&catalog, "effort"))
                .and_then(|option| option["options"].as_array())
                .into_iter()
                .flatten()
                .filter_map(|option| option["value"].as_str().map(str::to_owned))
                .collect();
            result.push(ProviderModel {
                service_tiers: vec![forge_provider_api::ServiceTier::Standard],
                id: id.into(),
                reasoning,
                default_reasoning: None,
                selected_reasoning: None,
                context_window: Vec::new(),
                default_context_window: None,
                selected_context_window: None,
                vision: self
                    .initialized
                    .pointer("/agentCapabilities/promptCapabilities/image")
                    .and_then(Value::as_bool)
                    == Some(true),
                description: model["description"]
                    .as_str()
                    .or_else(|| model["name"].as_str())
                    .map(str::to_owned),
                is_default: catalog
                    .pointer("/models/currentModelId")
                    .and_then(Value::as_str)
                    == Some(id),
            });
        }
        Ok(result)
    }
}

fn find_option<'catalog>(catalog: &'catalog Value, category: &str) -> Option<&'catalog Value> {
    catalog["configOptions"]
        .as_array()?
        .iter()
        .find(|option| option["category"] == category || option["id"] == category)
}
