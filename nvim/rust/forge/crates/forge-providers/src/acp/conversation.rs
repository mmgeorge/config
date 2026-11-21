use super::AcpKind as ProviderKind;
use super::connection::PromptCancellation;
use super::session::AcpSessionState;
use super::{AcpConnection, AcpConnectionError, AcpLaunch};
use anyhow::{Context, Result, ensure};
use forge_provider_api::error::RuntimeLaunchFailure;
use forge_provider_api::generation::ReasoningSelection;
use forge_provider_api::version::RuntimeRequirement;
use forge_provider_api::{
    ExecutionMode, ModelSelection, ProviderCapability, ProviderError, ProviderFailure,
    ProviderFailureKind, ProviderModel, ProviderOperation, SanitizedMessage,
};
use serde_json::{Value, json};
use std::path::PathBuf;
use std::sync::Arc;

/// Native session configuration prepared by the host's resource owner.
pub struct ConversationOptions {
    pub kind: ProviderKind,
    pub workspace: String,
    pub session_id: Option<String>,
    pub control_endpoint: Option<forge_provider_api::session::HostToolEndpoint>,
    pub mcp_servers: Vec<Value>,
    /// Preserve host base instructions independently from per-invocation guidance.
    pub system_instructions: String,
}

enum InstructionDelivery {
    NativeAppend { resumed_prelude: Option<String> },
    PromptPrelude(String),
}

/// Describe exactly which selection writes completed before the prompt stopped.
pub(super) enum ConfigurationOutcome {
    /// Model and effort are confirmed. Stopping may have prevented a later mode write.
    Configured,
    /// The stop prevented all model and effort writes.
    Cancelled,
    /// The model setter completed before the stop prevented the effort setter.
    ModelOnly,
}

/// Own the connection, negotiated capabilities, and native session configuration.
pub struct AcpConversation {
    id: String,
    transport: AcpConnection,
    state: Arc<AcpSessionState>,
    initialized: Value,
    initial_model: ModelSelection,
    instructions: InstructionDelivery,
    transcript: Option<PathBuf>,
}

impl AcpConversation {
    /// Negotiate and open native history without owning host tools or policy.
    pub(super) async fn open(
        launch: AcpLaunch<'_>,
        state: Arc<AcpSessionState>,
        options: ConversationOptions,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<Self, ProviderError> {
        let requirement = runtime_requirement(options.kind, launch.command);
        let profile = launch.profile;
        let transport = AcpConnection::launch_retained(launch, state.clone(), cleanup)
            .await
            .map_err(|source| launch_failure(&requirement, ProviderOperation::ThreadOpen, source))?;
        let result: Result<(String, Value, ModelSelection, InstructionDelivery)> = async {
        let initialized = initialize(&transport, &requirement, ProviderOperation::ThreadOpen).await?;
        let mut servers = Vec::new();
        if let Some(endpoint) = &options.control_endpoint {
            let headers = endpoint.headers.iter().map(|(name, value)| json!({"name":name,"value":value})).collect::<Vec<_>>();
            servers.push(json!({"type":"http","name":super::permission::HOST_TOOL_SERVER,"url":endpoint.url,"headers":headers}));
        }
        servers.extend(options.mcp_servers);
        let server_names = servers.iter().map(|server| {
            server["name"].as_str().map(str::to_owned).context("ACP MCP launch omitted its server name")
        }).collect::<Result<Vec<_>>>()?;
        state.set_mcp_server_names(server_names)?;
        let mut parameter = json!({"cwd":options.workspace,"mcpServers":servers});
        let instructions = if initialized.pointer("/agentCapabilities/_meta/antigravity/systemPrompt/append").and_then(Value::as_bool) == Some(true) {
            append_system_instructions(&mut parameter, &options.system_instructions);
            // Native adapters can retain their original system prompt on resume until compaction.
            // Supply current instructions as input too so resume cannot defer the host contract.
            InstructionDelivery::NativeAppend {
                resumed_prelude: options.session_id.as_ref().map(|_| options.system_instructions.clone()),
            }
        } else {
            InstructionDelivery::PromptPrelude(options.system_instructions)
        };
        let response = if let Some(id) = &options.session_id {
            parameter["sessionId"] = json!(id);
            transport
                .request("session/resume", parameter.clone())
                .await
                .map_err(|error| resume_failure(error, id))?
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
        let initial_model = ModelSelection {
            model: current_model_id(&response).map(str::to_owned),
            reasoning: effort_option(&response)
                .and_then(|option| option["currentValue"].as_str())
                .map(|effort| ReasoningSelection::Level(effort.to_owned()))
                .unwrap_or_default(),
            ..Default::default()
        };
        state.update_catalog(response).await;
        Ok((id, initialized, initial_model, instructions))
        }.await;
        match result {
            Ok((id, initialized, initial_model, instructions)) => Ok(Self {
                transcript: profile.and_then(|profile| super::transcript::path(profile, &id)),
                id,
                transport,
                state,
                initialized,
                initial_model,
                instructions,
            }),
            Err(error) => {
                let cause = match error.downcast::<ProviderError>() {
                    Ok(cause) => cause,
                    Err(error) => match error.downcast::<AcpConnectionError>() {
                        Ok(source) => crate::error::acp(source, ProviderOperation::ThreadOpen),
                        Err(_) => acp_failure(ProviderFailureKind::Initialization),
                    },
                };
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

    /// Observe the CLI's transcript from the point before this invocation starts.
    pub(super) async fn transcript_cursor(&self) -> Option<super::transcript::TranscriptCursor> {
        super::transcript::TranscriptCursor::capture(self.transcript.as_deref()?).await
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

    /// Get the base instructions for each ordinary primary prompt that relies on a prelude.
    ///
    /// Bundled Antigravity can compact without reporting it, so a prior prompt cannot establish
    /// that instructions delivered as user content remain visible in the next turn.
    pub(super) fn prompt_instructions(&self) -> Option<&str> {
        match &self.instructions {
            InstructionDelivery::NativeAppend { resumed_prelude } => resumed_prelude.as_deref(),
            InstructionDelivery::PromptPrelude(text) => Some(text.as_str()),
        }
        .filter(|text| !text.trim().is_empty())
    }

    /// Commit native prompt or steering evidence through the host's receipt.
    pub(super) async fn submit(
        &self,
        method: super::submission::AcpSubmission,
        parameter: Value,
        receipt: Option<Arc<dyn forge_provider_api::submission::InputReceipt>>,
        dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
        cancellation: Option<&super::connection::PromptCancellation>,
    ) -> Result<super::response::NativePromptEnd, ProviderError> {
        self.transport
            .submit(method, parameter, receipt, dispatch, cancellation)
            .await
    }

    /// Ask the worker to flush and release its conversation before process collection.
    pub(super) async fn close_native(&self) -> Result<(), ProviderError> {
        self.transport
            .request("session/close", json!({"sessionId": self.id}))
            .await
            .map(|_| ())
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadClose)
            })
    }

    /// Get native response evidence retained independently of receipt persistence.
    pub(super) fn prompt_end(
        &self,
        input: &forge_provider_api::CoveredInput,
    ) -> Option<super::response::NativePromptEnd> {
        self.transport.prompt_end(input)
    }

    /// Interrupt this conversation while retaining the outstanding prompt's terminal response.
    pub(super) fn cancel(
        &self,
        cancellation: &super::connection::PromptCancellation,
    ) -> Result<(), ProviderError> {
        cancellation
            .notify(&self.transport, &self.id)
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::TurnInterrupt)
            })
    }

    /// Apply native model, reasoning, and plan mode settings.
    pub(super) async fn configure(
        &self,
        model: Option<&str>,
        effort: Option<&str>,
        mode: ExecutionMode,
        cancellation: Option<&PromptCancellation>,
    ) -> Result<ConfigurationOutcome> {
        // Confirm the model before using the replacement effort control it advertises.
        let catalog = self.state.catalog().await;
        if let Some(model) = model {
            if let Some(option) = find_option(&catalog, "model") {
                let Some(response) = self.set_config_option(option, model, cancellation).await?
                else {
                    return Ok(ConfigurationOutcome::Cancelled);
                };
                confirm_model_selection(&response, &option["id"], model)?;
                self.state
                    .update_catalog(json!({"configOptions": response["configOptions"]}))
                    .await;
            } else if self
                .transport
                .request_configuration(
                    "session/set_model",
                    json!({"sessionId":self.id,"modelId":model}),
                    cancellation,
                )
                .await?
                .is_none()
            {
                return Ok(ConfigurationOutcome::Cancelled);
            }
        }

        // Confirm effort against the selected model before changing plan mode.
        if let Some(effort) = effort {
            let catalog = self.state.catalog().await;
            let option = effort_option(&catalog).ok_or(
                forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::EffortSelection,
                ),
            )?;
            if option["currentValue"] != effort {
                let Some(response) = self.set_config_option(option, effort, cancellation).await?
                else {
                    return Ok(if model.is_some() {
                        ConfigurationOutcome::ModelOnly
                    } else {
                        ConfigurationOutcome::Cancelled
                    });
                };
                ensure!(
                    option_selection(&response, &option["id"]) == Some(effort)
                        && find_option(&catalog, "model").is_none_or(|model| {
                            option_selection(&response, &model["id"]) == model["currentValue"].as_str()
                        }),
                    "ACP effort setter did not confirm the requested selection for the current model",
                );
                self.state
                    .update_catalog(json!({"configOptions": response["configOptions"]}))
                    .await;
            }
        }

        // A stopped invocation keeps its confirmed selection and skips an unstarted mode write.
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
            let _ = self
                .transport
                .request_configuration(
                    "session/set_mode",
                    json!({"sessionId":self.id,"modeId":desired}),
                    cancellation,
                )
                .await?;
        }
        Ok(ConfigurationOutcome::Configured)
    }

    /// Parse a complete option snapshot after the admitted setter request settles.
    async fn set_config_option(
        &self,
        option: &Value,
        selection: &str,
        cancellation: Option<&PromptCancellation>,
    ) -> Result<Option<Value>> {
        let response = self
            .transport
            .request_configuration(
                "session/set_config_option",
                json!({"sessionId":self.id,"configId":option["id"],"value":selection}),
                cancellation,
            )
            .await?;
        if let Some(response) = &response {
            validate_config_options(response)?;
        }
        Ok(response)
    }

    /// Refine the host descriptor using negotiated native capabilities.
    pub async fn capabilities(&self, mut capability: ProviderCapability) -> ProviderCapability {
        capability.native_fork &=
            matches!(self.instructions, InstructionDelivery::NativeAppend { .. })
                && self
                    .initialized
                    .pointer("/agentCapabilities/sessionCapabilities/fork")
                    .is_some_and(Value::is_object);
        capability.agent.observe = self
            .initialized
            .pointer("/agentCapabilities/sessionCapabilities/subagents")
            .is_some_and(Value::is_object);
        let catalog = self.state.catalog().await;
        capability.effort_selection = supports_effort(&catalog);
        capability
    }

    /// Get the native selection captured before Forge applies session or task settings.
    pub(super) fn initial_model(&self) -> &ModelSelection {
        &self.initial_model
    }

    /// Decode native model and reasoning choices for the host catalog.
    pub async fn models(&self) -> Result<Vec<ProviderModel>> {
        let catalog = self.state.catalog().await;
        let vision = self
            .initialized
            .pointer("/agentCapabilities/promptCapabilities/image")
            .and_then(Value::as_bool)
            == Some(true);
        let mut models = decode_model_catalog(&catalog, vision);
        for model in &mut models {
            model.is_default = self.initial_model.model.as_deref() == Some(model.id.as_str());
        }
        Ok(models)
    }
}

/// Decode selectable model IDs and the reasoning levels offered for each model.
///
/// The bundled CLI lists families in configuration updates and concrete variants in the opening
/// model snapshot. Keep the concrete IDs for saved selections and use the current configuration
/// to determine available families and the active family's reasoning choices.
pub(super) fn decode_model_catalog(catalog: &Value, vision: bool) -> Vec<ProviderModel> {
    let variants = catalog["models"]["availableModels"]
        .as_array()
        .map(Vec::as_slice)
        .unwrap_or_default();
    let families = find_option(catalog, "model").and_then(|option| option["options"].as_array());
    let current_model = current_model_id(catalog);
    let selectable_variants = variants.iter().filter(|variant| {
        families.is_none_or(|families| {
            variant["modelId"]
                .as_str()
                .is_some_and(|id| model_family(id, families).is_some())
        })
    });
    let families_without_variants = families.into_iter().flat_map(|families| {
        families.iter().filter(move |family| {
            family["value"].as_str().is_some_and(|family_id| {
                !variants.iter().any(|variant| {
                    variant["modelId"].as_str().is_some_and(|id| {
                        if current_model == Some(family_id) {
                            id == family_id
                        } else {
                            model_family(id, families) == Some(family_id)
                        }
                    })
                })
            })
        })
    });
    let current_family =
        find_option(catalog, "model").and_then(|option| option["currentValue"].as_str());
    let current_efforts: Vec<&str> = effort_option(catalog)
        .and_then(|option| option["options"].as_array())
        .into_iter()
        .flatten()
        .filter_map(|option| option["value"].as_str())
        .collect();
    let non_empty = |value: &Value| {
        value
            .as_str()
            .filter(|text| !text.trim().is_empty())
            .map(str::to_owned)
    };
    selectable_variants
        .chain(families_without_variants)
        .filter_map(|model| {
            let id = model["modelId"]
                .as_str()
                .or_else(|| model["value"].as_str())?;
            let family = families.and_then(|families| model_family(id, families));
            let variant_level = family.and_then(|family| variant_effort(id, family));
            let mut efforts = match (family, families) {
                (Some(family), Some(families)) if Some(family) != current_family => variants
                    .iter()
                    .filter_map(|variant| variant["modelId"].as_str())
                    .filter(|variant| model_family(variant, families) == Some(family))
                    .filter_map(|variant| variant_effort(variant, family))
                    .collect(),
                _ => current_efforts.clone(),
            };
            if family.is_some()
                && family == current_family
                && variant_level.is_some_and(|level| !efforts.contains(&level))
            {
                return None;
            }
            efforts.sort_by_key(|effort| match *effort {
                "low" => 0,
                "medium" => 1,
                "high" => 2,
                "xhigh" => 3,
                _ => 4,
            });
            efforts.dedup();
            let default_reasoning = variant_level.filter(|effort| efforts.contains(effort));
            Some(ProviderModel {
                service_tiers: vec![forge_provider_api::ServiceTier::Standard],
                id: id.into(),
                label: non_empty(&model["name"]),
                reasoning: efforts.into_iter().map(str::to_owned).collect(),
                default_reasoning: default_reasoning.map(str::to_owned),
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

/// Check whether any available model accepts a reasoning effort.
pub(super) fn supports_effort(catalog: &Value) -> bool {
    decode_model_catalog(catalog, false)
        .iter()
        .any(|model| !model.reasoning.is_empty())
}

/// Resolve the selected bundled model and effort before consulting the legacy snapshot.
pub(super) fn current_model_id(catalog: &Value) -> Option<&str> {
    if let Some(family) =
        find_option(catalog, "model").and_then(|option| option["currentValue"].as_str())
    {
        if let Some(level) =
            effort_option(catalog).and_then(|option| option["currentValue"].as_str())
        {
            let selected = format!("{family}-{level}");
            if let Some(variant) = catalog
                .pointer("/models/availableModels")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(|model| model["modelId"].as_str())
                .find(|model| *model == selected)
            {
                return Some(variant);
            }
        }
        return Some(family);
    }
    catalog
        .pointer("/models/currentModelId")
        .and_then(Value::as_str)
}

/// Get the advertised family that owns a concrete model identifier.
fn model_family<'catalog>(id: &str, families: &'catalog [Value]) -> Option<&'catalog str> {
    families
        .iter()
        .filter_map(|family| family["value"].as_str())
        .filter(|family| id == *family || variant_effort(id, family).is_some())
        .max_by_key(|family| family.len())
}

/// Get the level encoded by a concrete model identifier.
fn variant_effort<'catalog>(id: &'catalog str, family: &str) -> Option<&'catalog str> {
    id.strip_prefix(family)?.strip_prefix('-')
}

/// Create the version requirement for the selected executable.
pub(super) fn runtime_requirement(kind: ProviderKind, command: &[String]) -> RuntimeRequirement {
    let minimum = match kind {
        ProviderKind::Antigravity => semver::Version::new(1, 5, 13),
    };
    let runtime = PathBuf::from(command.first().map(String::as_str).unwrap_or("ACP"));
    RuntimeRequirement::new(kind.id(), runtime, minimum)
}

/// Negotiate ACP before either model discovery or session attachment.
pub(super) async fn initialize(
    transport: &AcpConnection,
    requirement: &RuntimeRequirement,
    operation: ProviderOperation,
) -> Result<Value, ProviderError> {
    let initialized = transport.request_initialization(json!({
        "protocolVersion":1,
        "clientInfo":{"name":"forge-harness","title":"Forge Harness","version":env!("CARGO_PKG_VERSION")},
        "clientCapabilities":{"session":{"compaction":{},"configOptions":{"boolean":{}},"notices":{}},"subagents":{}}
    })).await.map_err(|source| crate::error::acp(source, operation))?;
    validate_initialization(requirement, operation, &initialized)?;
    Ok(initialized)
}

/// Preserve an OS launch failure without exposing arguments or third-party error text.
pub(super) fn launch_failure(
    requirement: &RuntimeRequirement,
    operation: ProviderOperation,
    error: AcpConnectionError,
) -> ProviderError {
    let AcpConnectionError::Process { source, .. } = error else {
        return crate::error::acp(error, operation);
    };
    let failure = match source.kind() {
        std::io::ErrorKind::NotFound => RuntimeLaunchFailure::NotFound,
        std::io::ErrorKind::PermissionDenied => RuntimeLaunchFailure::PermissionDenied,
        _ => RuntimeLaunchFailure::Unavailable,
    };
    tracing::error!(target: "forge::provider", event_name = "provider.startup.failed",
        provider = "antigravity", operation = ?operation, failure = ?failure,
        os_error = source.raw_os_error());
    ProviderError::RuntimeLaunch {
        provider: requirement.provider().clone(),
        runtime: requirement.runtime().to_owned(),
        failure,
    }
}

/// Explain the bundled CLI's saved-history read failure without exposing native text.
fn resume_failure(error: AcpConnectionError, id: &str) -> anyhow::Error {
    let AcpConnectionError::Remote { source, .. } = &error else {
        return error.into();
    };
    let Some(native) = source.downcast_ref::<agent_client_protocol::Error>() else {
        return error.into();
    };
    if native.code != agent_client_protocol::ErrorCode::InternalError
        || native.message != format!("Failed to read session {id}; retry, or load it with session/load")
    {
        return error.into();
    }
    ProviderError::Explained {
        cause: Box::new(acp_failure(forge_provider_api::ProviderFailureKind::Initialization)),
        explanation: forge_provider_api::diagnostic::SanitizedMessage::new(
            "Antigravity could not read the saved conversation.",
        )
        .expect("static resume explanation is nonempty"),
    }
    .into()
}

/// Validate the reported runtime and selected ACP protocol before session or authentication use.
fn validate_initialization(
    requirement: &RuntimeRequirement,
    operation: ProviderOperation,
    initialized: &Value,
) -> Result<(), ProviderError> {
    requirement.validate(
        initialized
            .pointer("/agentInfo/version")
            .and_then(Value::as_str),
    )?;
    let capabilities = &initialized["agentCapabilities"];
    let explanation = if initialized["protocolVersion"] != 1 {
        "Antigravity did not negotiate ACP version 1. Check the configured runtime."
    } else if !capabilities
            .pointer("/sessionCapabilities/resume")
            .is_some_and(Value::is_object)
        || !capabilities
            .pointer("/sessionCapabilities/close")
            .is_some_and(Value::is_object)
    {
        "Antigravity did not advertise ACP session resume and close. Check the configured runtime."
    } else if capabilities
            .pointer("/mcpCapabilities/http")
            .and_then(Value::as_bool)
            != Some(true)
    {
        "Antigravity did not advertise HTTP MCP. Launch it with agy acp --process-per-session."
    } else {
        return Ok(());
    };
    tracing::error!(target: "forge::provider", event_name = "provider.startup.failed",
        provider = "antigravity", operation = ?operation, explanation);
    Err(ProviderError::Explained {
        cause: Box::new(ProviderError::Failure(ProviderFailure::new(
            operation,
            ProviderFailureKind::Protocol,
        ))),
        explanation: SanitizedMessage::new(explanation)
            .expect("static ACP handshake explanation is nonempty"),
    })
}

/// Encode negotiated ACP append semantics while preserving the native provider preset.
fn append_system_instructions(configuration: &mut Value, instructions: &str) {
    configuration["_meta"]["antigravity"]["systemPrompt"] = json!({"append": instructions});
}

fn acp_failure(kind: forge_provider_api::ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::ThreadOpen,
        kind,
    ))
}

/// Get the native config control for a category or identifier.
fn find_option<'catalog>(catalog: &'catalog Value, category: &str) -> Option<&'catalog Value> {
    catalog["configOptions"]
        .as_array()?
        .iter()
        .find(|option| option["category"] == category || option["id"] == category)
}

/// Get the current model's native effort control when the CLI publishes one.
fn effort_option(catalog: &Value) -> Option<&Value> {
    find_option(catalog, "thought_level").or_else(|| find_option(catalog, "effort"))
}

/// Require a complete, unambiguous setter response before accepting its current selection.
fn validate_config_options(response: &Value) -> Result<()> {
    let options = response["configOptions"]
        .as_array()
        .context("ACP setter omitted its configuration options")?;
    let parsed: agent_client_protocol::schema::v1::SetSessionConfigOptionResponse =
        serde_json::from_value(response.clone())?;
    let mut ids = std::collections::HashSet::new();
    ensure!(
        !options.is_empty()
            && options.len() == parsed.config_options.len()
            && options.iter().all(|option| {
                option["id"].as_str().is_some_and(|id| !id.is_empty() && ids.insert(id))
            }),
        "ACP setter returned invalid configuration options",
    );
    Ok(())
}

/// Read the select value for the exact configuration control that was written.
fn option_selection<'catalog>(catalog: &'catalog Value, id: &Value) -> Option<&'catalog str> {
    let id = id.as_str()?;
    catalog["configOptions"]
        .as_array()?
        .iter()
        .find(|option| option["id"] == id && option["type"] == "select")?["currentValue"]
        .as_str()
}

/// Accept either an exact model value or Antigravity's family and effort acknowledgement.
fn confirm_model_selection(response: &Value, id: &Value, requested: &str) -> Result<()> {
    let family = option_selection(response, id)
        .context("ACP model setter omitted its model selection")?;
    let effort = effort_option(response)
        .and_then(|option| option_selection(response, &option["id"]));
    ensure!(
        family == requested
            || effort.is_some_and(|effort| variant_effort(requested, family) == Some(effort)),
        "ACP model setter did not confirm the requested selection",
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Attribute an incompatible handshake to the requested operation and explain the remedy.
    #[test]
    fn initialization_reports_missing_http_mcp_for_model_discovery() {
        let requirement = runtime_requirement(ProviderKind::Antigravity, &["agy".into()]);
        let operation = ProviderOperation::ModelList;
        let mut initialized = json!({
            "protocolVersion": 1,
            "agentInfo": {"version": "1.5.13"},
            "agentCapabilities": {"sessionCapabilities": {"resume": {}, "close": {}}}
        });
        let error = validate_initialization(&requirement, operation, &initialized).unwrap_err();
        let ProviderError::Explained { cause, explanation } = error else {
            panic!("expected an explained protocol failure, got {error}");
        };
        assert!(matches!(*cause, ProviderError::Failure(failure)
            if failure.operation() == operation && failure.kind() == ProviderFailureKind::Protocol));
        assert!(explanation.as_str().contains("agy acp --process-per-session"));

        initialized["agentCapabilities"]["mcpCapabilities"] = json!({"http": true});
        validate_initialization(&requirement, operation, &initialized).unwrap();
    }

    /// Reject an empty or stale `set_config_option` result before applying effort or mode.
    #[tokio::test]
    async fn configuration_rejects_unconfirmed_model_response() {
        for response in [json!({}), configuration_options("neon", "high")] {
            let directory = tempfile::tempdir().unwrap();
            let peer = super::super::in_memory_peer::InMemoryPeer::register(
                directory.path(),
                Box::new(move || {
                    let response = response.clone();
                    Box::new(move |request| {
                        if request["method"] == "session/set_config_option" {
                            vec![json!({"id":request["id"],"result":response})]
                        } else {
                            configuration_fixture_reply(request)
                        }
                    })
                }),
            );
            let conversation = configuration_fixture(&peer, directory.path()).await;
            assert!(conversation.configure(Some("flash-low"), Some("high"), ExecutionMode::Default, None).await.is_err());
            assert_eq!(current_model_id(&conversation.state.catalog().await), Some("neon"));
            let requests = peer.requests();
            assert_eq!(requests.iter().filter(|request| request["method"] == "session/set_config_option").count(), 1);
            assert!(requests.iter().all(|request| request["method"] != "session/set_mode"));
            conversation.transport.shutdown().await.unwrap();
        }
    }

    /// Confirm an effort change against the control the new model advertised in its reply.
    #[tokio::test]
    async fn configuration_checks_replacement_effort_response() {
        for (reported_effort, confirmed) in [("high", true), ("low", false)] {
            let directory = tempfile::tempdir().unwrap();
            let peer = super::super::in_memory_peer::InMemoryPeer::register(
                directory.path(),
                Box::new(move || {
                    Box::new(move |request| {
                        if request["method"] != "session/set_config_option" {
                            return configuration_fixture_reply(request);
                        }
                        let effort = match request["params"]["configId"].as_str() {
                            Some("model") => "low",
                            Some("replacement-effort") => reported_effort,
                            _ => panic!("stale effort control used after switching models"),
                        };
                        let mut selection = configuration_options("flash", effort);
                        selection["configOptions"][1]["id"] = json!("replacement-effort");
                        vec![json!({"id":request["id"],"result":selection})]
                    })
                }),
            );
            let conversation = configuration_fixture(&peer, directory.path()).await;
            let result = conversation.configure(Some("flash-low"), Some("high"), ExecutionMode::Default, None).await;
            assert_eq!(result.is_ok(), confirmed);
            let catalog = conversation.state.catalog().await;
            assert_eq!(effort_option(&catalog).unwrap()["currentValue"], reported_effort);
            let requests = peer.requests();
            assert_eq!(requests.iter().any(|request| request["method"] == "session/set_mode"), confirmed);
            conversation.transport.shutdown().await.unwrap();
        }
    }

    /// Hold the first setter on the wire and prove stop fences the next setter after collection.
    #[tokio::test]
    async fn configuration_stop_collects_model_without_publishing_followup_setters() {
        let directory = tempfile::tempdir().unwrap();
        let model_published = Arc::new(tokio::sync::Notify::new());
        let published = Arc::clone(&model_published);
        let peer = super::super::in_memory_peer::InMemoryPeer::register(
            directory.path(),
            Box::new(move || {
                let published = Arc::clone(&published);
                let mut model_request = None;
                Box::new(move |request| match request["method"].as_str() {
                    Some("session/set_config_option") if request["params"]["configId"] == "model" => {
                        model_request = Some(request["id"].clone());
                        published.notify_one();
                        Vec::new()
                    }
                    Some("fixture/release") => vec![
                        json!({"id":model_request.take().unwrap(),"result":configuration_options("flash", "low")}),
                        json!({"id":request["id"],"result":{}}),
                    ],
                    Some("session/set_config_option") => {
                        vec![json!({"id":request["id"],"result":configuration_options("flash", "high")})]
                    }
                    _ => configuration_fixture_reply(request),
                })
            }),
        );
        let conversation = configuration_fixture(&peer, directory.path()).await;
        let cancellation = PromptCancellation::default();
        let configuration = conversation.configure(Some("flash-low"), Some("high"), ExecutionMode::Default, Some(&cancellation));
        tokio::pin!(configuration);
        tokio::select! {
            _ = model_published.notified() => {},
            _ = &mut configuration => panic!("held model setter completed before release"),
        }
        cancellation.stop();
        assert!(futures_util::poll!(&mut configuration).is_pending());
        conversation.transport.request("fixture/release", json!({})).await.unwrap();
        assert!(matches!(configuration.await.unwrap(), ConfigurationOutcome::ModelOnly));
        assert_eq!(current_model_id(&conversation.state.catalog().await), Some("flash"));
        let requests = peer.requests();
        assert_eq!(requests.iter().filter(|request| request["method"] == "session/set_config_option").count(), 1);
        assert!(requests.iter().all(|request| request["method"] != "session/set_mode"));
        conversation.transport.shutdown().await.unwrap();
    }

    /// Open ordinary ACP conversation state through the deterministic native-wire fixture.
    async fn configuration_fixture(peer: &super::super::in_memory_peer::InMemoryPeer, directory: &std::path::Path) -> AcpConversation {
        let command = [peer.program()];
        let workspace = directory.to_str().unwrap();
        let kind = ProviderKind::Antigravity;
        AcpConversation::open(
            AcpLaunch {
                process_owner: None,
                command: &command,
                environment: &Default::default(),
                workspace,
                profile: None,
            },
            Arc::new(AcpSessionState::new_native(kind, workspace.to_owned())),
            ConversationOptions {
                kind,
                workspace: workspace.to_owned(),
                session_id: None,
                control_endpoint: None,
                mcp_servers: Vec::new(),
                system_instructions: String::new(),
            },
            &Default::default(),
        ).await.unwrap()
    }

    /// Supply the model and effort selectors advertised by the fixture peer.
    fn configuration_options(family: &str, effort: &str) -> Value {
        json!({"configOptions":[
            {"id":"model","category":"model","type":"select","name":"Model","currentValue":family,
             "options":[{"value":"neon","name":"Neon"},{"value":"flash","name":"Flash"}]},
            {"id":"thought_level","category":"thought_level","type":"select","name":"Thinking","currentValue":effort,
             "options":[{"value":"low","name":"Low"},{"value":"high","name":"High"}]},
        ]})
    }

    /// Reply to setup and legacy mode requests for the configuration fixture.
    fn configuration_fixture_reply(request: &Value) -> Vec<Value> {
        let result = match request["method"].as_str() {
            Some("initialize") => json!({
                "protocolVersion":1,"agentInfo":{"version":"99.0.0"},
                "agentCapabilities":{"sessionCapabilities":{"resume":{},"close":{}},"mcpCapabilities":{"http":true}},
            }),
            Some("session/new") => {
                let mut result = configuration_options("neon", "high");
                result["sessionId"] = json!("configuration-fixture");
                result["modes"] = json!({"availableModes":[{"id":"default"}]});
                result
            }
            Some("session/set_mode") => json!({}),
            _ => panic!("unexpected fixture request: {request}"),
        };
        vec![json!({"id":request["id"],"result":result})]
    }

    /// Keep generic RPC errors distinct from the reported failure to read the requested history.
    #[test]
    fn resume_explains_only_the_observed_history_read_failure() {
        let remote_error = |message: &str| AcpConnectionError::Remote {
            method: "session/resume".into(),
            failure: Some(super::super::connection::AcpRemoteFailure::Protocol),
            source: Box::new(agent_client_protocol::Error::new(-32603, message)),
        };
        let error = resume_failure(
            remote_error("Failed to read session history-id; retry, or load it with session/load"),
            "history-id",
        );
        assert!(matches!(error.downcast_ref::<ProviderError>(), Some(ProviderError::Explained { explanation, .. })
            if explanation.as_str() == "Antigravity could not read the saved conversation."));

        for message in [
            "Internal error",
            "Failed to read session other-id; retry, or load it with session/load",
        ] {
            let error = resume_failure(remote_error(message), "history-id");
            assert!(error.downcast_ref::<AcpConnectionError>().is_some());
        }
    }

    /// Bundled ACP keeps its opening model snapshot after configuration changes.
    #[test]
    fn selected_model_uses_the_latest_config_options() {
        let catalog = json!({
            "models": {
                "currentModelId": "gemini-4-neon-high",
                "availableModels": [
                    {"modelId": "gemini-4-neon-high"},
                    {"modelId": "gemini-3.8-flash-low"}
                ]
            },
            "configOptions": [
                {"id": "model", "currentValue": "gemini-3.8-flash"},
                {"id": "thought_level", "currentValue": "low"}
            ]
        });
        assert_eq!(current_model_id(&catalog), Some("gemini-3.8-flash-low"));
    }

    /// Preserve concrete picker IDs while limiting effort to the advertised model family.
    #[test]
    fn bundled_catalog_keeps_reasoning_scoped_to_each_family() {
        let mut catalog = json!({
            "models": {
                "currentModelId": "gemini-4-neon-high",
                "availableModels": [
                    {"modelId": "gemini-4-neon-high"},
                    {"modelId": "gemini-4-neon-xhigh"},
                    {"modelId": "gemini-4-neon-low"},
                    {"modelId": "gemini-3.8-flash-high"},
                    {"modelId": "gemini-3.8-flash-low"},
                    {"modelId": "orchestrator"},
                    {"modelId": "retired-high"}
                ]
            },
            "configOptions": [
                {"id": "model", "currentValue": "gemini-4-neon", "options": [
                    {"value": "gemini-4-neon"}, {"value": "gemini-3.8-flash"},
                    {"value": "orchestrator"}, {"value": "new-family"}
                ]},
                {"id": "thought_level", "currentValue": "high", "options": [
                    {"value": "low"}, {"value": "high"}, {"value": "xhigh"}
                ]}
            ]
        });

        let models = decode_model_catalog(&catalog, false);
        let flash_low = models
            .iter()
            .find(|model| model.id == "gemini-3.8-flash-low")
            .unwrap();
        assert_eq!(flash_low.reasoning, ["low", "high"]);
        assert_eq!(flash_low.default_reasoning.as_deref(), Some("low"));
        assert!(models.iter().any(|model| model.id == "new-family"));
        assert!(models.iter().all(|model| model.id != "retired-high"));

        catalog["configOptions"][0]["currentValue"] = json!("orchestrator");
        catalog["configOptions"].as_array_mut().unwrap().pop();
        let models = decode_model_catalog(&catalog, false);
        let fixed = models.iter().find(|model| model.id == "orchestrator").unwrap();
        assert!(fixed.reasoning.is_empty());
        assert!(supports_effort(&catalog));
        let neon = models
            .iter()
            .find(|model| model.id == "gemini-4-neon-high")
            .unwrap();
        assert_eq!(neon.reasoning, ["low", "high", "xhigh"]);
    }

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
