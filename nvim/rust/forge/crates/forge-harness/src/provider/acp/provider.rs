use super::runtime::AcpRuntime;
use super::transport::AcpTransport;
use crate::permissions::acp::AcpPermissionContext;
use crate::provider::approval::PermissionCoordinator;
use crate::provider::*;
use crate::session::ApprovalMode;
use crate::settings::ResourceProfile;
use crate::tools::ControlTurnContext;
use crate::tools::control_server::ControlServer;
use crate::tools::control_session::{ControlEndpoint, ControlSession};
use anyhow::{Context, Result, ensure};
use async_trait::async_trait;
use forge_providers::acp::conversation::{AcpConversation, ConversationOptions};
use forge_providers::acp::session::AcpSessionState;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::Mutex;

/// Owns ACP root-session processes and adapts provider-specific configuration.
pub(crate) struct AcpProvider {
    session_events: Arc<crate::provider::session_events::SessionEvents>,
    kind: ProviderKind,
    runtime: AcpRuntime,
    permission: Arc<PermissionCoordinator>,
    session: Mutex<HashMap<String, Arc<AcpSession>>>,
    preparation: Mutex<HashMap<String, Arc<Mutex<()>>>>,
    cancellation: Mutex<HashMap<String, Arc<AtomicBool>>>,
    conversation: Mutex<HashMap<String, String>>,
    settings_root: std::path::PathBuf,
}

struct AcpSession {
    native: AcpConversation,
    permission: Arc<AcpPermissionContext>,
    _control: ControlServer,
    control: Arc<ControlSession>,
    turn: Mutex<PromptEvidence>,
    _profile: Option<ResourceProfile>,
}

/// Retain validated native terminal evidence under the same lock as its prompt worker.
enum PromptEvidence {
    Unavailable,
    Validated,
}

impl AcpProvider {
    /// Validate the launch descriptor without launching an unaffiliated provider process.
    pub(crate) fn new(
        kind: ProviderKind,
        command: Vec<String>,
        permission: Arc<PermissionCoordinator>,
        settings_root: std::path::PathBuf,
    ) -> Result<Self> {
        ensure!(!command.is_empty(), "ACP command is required");
        let runtime = AcpRuntime::new(kind, command, &settings_root);
        Ok(Self {
            session_events: Default::default(),
            kind,
            runtime,
            permission,
            session: Mutex::new(HashMap::new()),
            preparation: Mutex::new(HashMap::new()),
            cancellation: Mutex::new(HashMap::new()),
            conversation: Mutex::new(HashMap::new()),
            settings_root,
        })
    }

    async fn session(
        &self,
        mut request: ProviderCatalogRequest,
        tools: Option<Vec<crate::tools::ControlToolDefinition>>,
        cancelled: Option<&AtomicBool>,
    ) -> Result<Arc<AcpSession>> {
        crate::provider::registered_capability(self.kind).validate_access(&request.access)?;
        if cancelled.is_some_and(|cancelled| cancelled.load(Ordering::Acquire)) {
            return Err(crate::broker::TurnCancelled.into());
        }
        let owner = request.owner()?;
        let runtime_key = owner.runtime_key();
        let preparation = self
            .preparation
            .lock()
            .await
            .entry(runtime_key.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone();
        let _preparation = preparation.lock().await;
        if cancelled.is_some_and(|cancelled| cancelled.load(Ordering::Acquire)) {
            return Err(crate::broker::TurnCancelled.into());
        }
        let existing = self.session.lock().await.get(&runtime_key).cloned();
        if let Some(session) = existing {
            let changed = if let Some(tools) = &tools {
                let previous = session.control.definitions().await;
                tools
                    .iter()
                    .map(|definition| definition.mcp_value())
                    .collect::<Vec<_>>()
                    != previous
                        .iter()
                        .map(|definition| definition.mcp_value())
                        .collect::<Vec<_>>()
            } else {
                false
            };
            if !changed {
                return Ok(session);
            }
            ensure!(
                !session.native.state().has_active_work().await,
                "Tool profile changes require active execution cleanup"
            );
            request.provider_session_id = Some(session.native.id().to_owned().try_into()?);
            session.native.shutdown().await?;
            self.session.lock().await.remove(&runtime_key);
        }
        if request.provider_session_id.is_none() {
            request.provider_session_id = self
                .conversation
                .lock()
                .await
                .get(&request.runtime_key()?)
                .cloned()
                .map(TryInto::try_into)
                .transpose()?;
        }
        let has_history = request.provider_session_id.is_some();
        let permission = Arc::new(AcpPermissionContext::new(self.permission.clone()));
        let state = Arc::new(AcpSessionState::new(self.kind, permission.clone()));
        let setting = crate::settings::SettingsStore::open(&self.settings_root)?.get(
            &request.workspace,
            &request.harness_session_id,
            if self.kind == ProviderKind::Claude {
                "claude"
            } else {
                "antigravity"
            },
        )?;
        let definitions = tools.unwrap_or_else(|| {
            crate::tools::ControlToolRegistry
                .definition_list_for_owner(
                    &owner,
                    setting.desired.subagent_tools.unwrap_or(false),
                    setting.desired.lsp_tools.unwrap_or(false),
                )
                .into_iter()
                .filter(|tool| {
                    !tool.name.starts_with("harness_session_")
                        || setting.desired.session_tools.unwrap_or(true)
                })
                .collect()
        });
        let controls = Arc::new(ControlSession::new(definitions));
        let control = ControlServer::start(controls.clone()).await?;
        let target = if self.kind == ProviderKind::Claude {
            "claudecode"
        } else {
            "antigravity-cli"
        };
        let resources = crate::settings::SessionResources::load(
            &self.settings_root,
            &request,
            if self.kind == ProviderKind::Claude {
                "claude"
            } else {
                "antigravity"
            },
            target,
        )?;
        let selected_skill = &resources.selected_skill;
        let profile = if self.kind == ProviderKind::Antigravity {
            let excluded: Vec<_> = resources
                .skill
                .iter()
                .filter(|skill| !skill.enabled)
                .collect();
            if !excluded.is_empty() {
                let workspace = std::path::Path::new(&request.workspace);
                let native = crate::settings::ResourceCatalog::native_skills(vec![
                    workspace.join(".gemini/skills"),
                    workspace.join(".agents/skills"),
                ])?;
                for skill in excluded {
                    ensure!(
                        !native.iter().any(|candidate| candidate.name == skill.name),
                        "Antigravity cannot exclude project-native skill {} without changing project configuration",
                        skill.name
                    );
                }
            }
            ResourceProfile::antigravity(
                &self.settings_root,
                &ResourceProfile::native_home()?,
                selected_skill,
            )?
        } else {
            ResourceProfile::claude(&self.settings_root, selected_skill)?
        };
        let environment_profile = (self.kind == ProviderKind::Antigravity).then(|| profile.path());
        let runtime = self.runtime.resolve()?;
        let native = AcpConversation::open(
            forge_providers::acp::AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace: &request.workspace,
                profile: environment_profile,
            },
            state,
            ConversationOptions {
                kind: self.kind,
                workspace: request.workspace.clone(),
                session_id: request.provider_session_id.map(Into::into),
                control_url: control.url.clone(),
                mcp_servers: resources.acp()?,
                resources: resources.mcp.clone(),
                profile: profile.path().to_owned(),
                skills: selected_skill
                    .iter()
                    .map(|skill| skill.name.clone())
                    .collect(),
            },
        )
        .await?;
        let session = Arc::new(AcpSession {
            native,
            permission,
            _control: control,
            control: controls,
            turn: Mutex::new(PromptEvidence::Unavailable),
            _profile: Some(profile),
        });
        if has_history {
            self.conversation
                .lock()
                .await
                .insert(runtime_key.clone(), session.native.id().to_owned());
        }
        let mut published = self.session.lock().await;
        if cancelled.is_some_and(|cancelled| cancelled.load(Ordering::Acquire)) {
            return Err(crate::broker::TurnCancelled.into());
        }
        published.insert(runtime_key, session.clone());
        Ok(session)
    }
}

#[async_trait]
impl crate::provider::ProviderAdapter for AcpProvider {
    type Host = crate::provider::HarnessProviderHost;
    async fn setup(&self, request: ProviderCatalogRequest) -> Result<Value> {
        ensure!(
            !self.has_active_turn(&request.runtime_key()?).await,
            "Finish active provider work before setup"
        );
        let status = self.runtime.status();
        let mut result = serde_json::to_value(&status)?;
        result["auth_methods"] = json!([]);
        match status.provider_version().await {
            Ok(version) => result["provider_version"] = json!(version),
            Err(error) => result["version_error"] = json!(format!("{error:#}")),
        }
        if !status.available {
            return Ok(result);
        }
        self.cleanup_execution(&request.runtime_key()?).await?;
        let permission = Arc::new(AcpPermissionContext::new(self.permission.clone()));
        let state = Arc::new(AcpSessionState::new(self.kind, permission.clone()));
        let profile = if self.kind == ProviderKind::Antigravity {
            Some(ResourceProfile::antigravity(
                &self.settings_root,
                &ResourceProfile::native_home()?,
                &[],
            )?)
        } else {
            None
        };
        let transport = AcpTransport::launch(
            &status,
            &request.workspace,
            state,
            profile.as_ref().map(ResourceProfile::path),
        )
        .await?;
        let initialized = transport
            .initialize(
                self.kind,
                status
                    .command
                    .first()
                    .map(std::path::PathBuf::from)
                    .unwrap_or_else(|| "ACP".into()),
                json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","version":env!("CARGO_PKG_VERSION")},
            "clientCapabilities":{"auth":{"terminal":true}}}),
            )
            .await;
        let collected = transport.shutdown().await;
        let initialized = match (initialized, collected) {
            (Ok(initialized), Ok(())) => initialized,
            (Err(cause), Err(cleanup)) => {
                return Err(ProviderError::PreparationCleanup {
                    cause: Arc::new(cause),
                    cleanup: Arc::new(cleanup),
                }
                .into());
            }
            (Err(error), Ok(())) => return Err(error.into()),
            (Ok(_), Err(error)) => return Err(error.into()),
        };
        result["adapter_version"] = initialized
            .pointer("/agentInfo/version")
            .cloned()
            .unwrap_or(Value::Null);
        result["auth_methods"] = json!(initialized.get("authMethods").and_then(Value::as_array)
            .into_iter().flatten().map(|method| {
                let terminal = method["type"] == "terminal";
                let mut command = status.command.clone();
                if terminal { command.extend(method.get("args").and_then(Value::as_array).into_iter().flatten()
                    .filter_map(Value::as_str).map(str::to_owned)); }
                json!({"id":method["id"],"label":method["name"],"description":method["description"],
                    "terminal":terminal,"command":if terminal { json!(command) } else { Value::Null },
                    "environment":status.environment})
            }).collect::<Vec<_>>());
        Ok(result)
    }

    async fn reconcile_settings(
        &self,
        request: ProviderCatalogRequest,
        _setting: &crate::settings::PortableSettings,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.cleanup_execution(&request.runtime_key()?).await?;
            self.session(request, None, None).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::ReconcileSettings,
                source,
            )
        })
    }

    async fn authenticate(&self, request: ProviderCatalogRequest, method: String) -> Result<()> {
        ensure!(
            !self.has_active_turn(&request.runtime_key()?).await,
            "Authentication cannot change during active provider work"
        );
        let permission = Arc::new(AcpPermissionContext::new(self.permission.clone()));
        let state = Arc::new(AcpSessionState::new(self.kind, permission.clone()));
        let profile = if self.kind == ProviderKind::Antigravity {
            Some(ResourceProfile::antigravity(
                &self.settings_root,
                &ResourceProfile::native_home()?,
                &[],
            )?)
        } else {
            None
        };
        let transport = AcpTransport::launch(
            &self.runtime.resolve()?,
            &request.workspace,
            state,
            profile.as_ref().map(ResourceProfile::path),
        )
        .await?;
        let initialized = transport
            .request(
                "initialize",
                json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","version":env!("CARGO_PKG_VERSION")},
            "clientCapabilities":{"auth":{"terminal":true}}}),
            )
            .await?;
        let selected = initialized["authMethods"]
            .as_array()
            .and_then(|method_list| {
                method_list
                    .iter()
                    .find(|candidate| candidate["id"] == method)
            })
            .with_context(|| {
                format!("ACP agent did not advertise authentication method {method}")
            })?;
        ensure!(
            selected["type"] != "terminal",
            "This sign-in method requires the terminal action in /config → Provider setup"
        );
        let result = transport
            .request("authenticate", json!({"methodId":method}))
            .await;
        transport.shutdown().await?;
        result?;
        self.cleanup_execution(&request.runtime_key()?).await?;
        Ok(())
    }

    async fn capabilities(&self, request: ProviderCatalogRequest) -> Result<CapabilityManifest> {
        let mut manifest = CapabilityManifest::from_descriptor(self.descriptor());
        if let Some(session) = self
            .session
            .lock()
            .await
            .get(&request.runtime_key()?)
            .cloned()
        {
            let mut descriptor = self.descriptor();
            descriptor.capability = session
                .native
                .capabilities(self.descriptor().capability)
                .await;
            let native_fork = descriptor.capability.native_fork;
            let native_steer = descriptor.capability.native_steer;
            manifest = CapabilityManifest::from_descriptor(descriptor);
            if self.kind != ProviderKind::Claude {
                manifest.provider_version = session.native.version();
            }
            let catalog = session.native.state().catalog().await;
            manifest.notice = catalog["mcpResources"]
                .as_array()
                .into_iter()
                .flatten()
                .filter(|resource| resource["state"] == "unavailable")
                .filter_map(|resource| resource["reason"].as_str().map(str::to_owned))
                .collect();
            for capability in &mut manifest.capability {
                let supported = match capability.id {
                    "native_fork" => Some(native_fork),
                    "native_steer" => Some(native_steer),
                    "resume" => Some(session.native.supports_resume()),
                    "effort_selection" => Some(
                        find_option(&catalog, "thought_level")
                            .or_else(|| find_option(&catalog, "effort"))
                            .is_some(),
                    ),
                    _ => None,
                };
                if let Some(supported) = supported {
                    capability.support = if supported {
                        super::super::capability::Support::Supported
                    } else {
                        super::super::capability::Support::Unsupported
                    };
                }
            }
        } else {
            manifest.availability = "discovering";
            manifest.availability_reason =
                Some("Agent startup and authentication have not completed".into());
        }
        let status = self.runtime.status();
        if self.kind == ProviderKind::Claude {
            match status.provider_version().await {
                Ok(version) => manifest.provider_version = version,
                Err(error) => manifest.notice.push(format!("{error:#}")),
            }
        }
        if !status.available {
            manifest.availability = "blocked";
            manifest.availability_reason =
                Some("ACP runtime is not installed. Configure an installed ACP adapter in harness.providers".into());
        }
        Ok(manifest)
    }
    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: self.kind,
            label: if self.kind == ProviderKind::Claude {
                "Claude Code"
            } else {
                "Antigravity"
            }
            .into(),
            capability: capability(self.kind),
        }
    }

    async fn shutdown(&self) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            let mut failures = Vec::new();
            if let Err(error) = self.session_events.shutdown().await {
                failures.push(error);
            }
            let sessions: Vec<_> = self
                .session
                .lock()
                .await
                .drain()
                .map(|(_, session)| session)
                .collect();
            for session in sessions {
                if let Err(source) = session.native.shutdown().await {
                    failures.push(source);
                }
            }
            crate::provider::lifecycle_error::combine(failures).map_err(anyhow::Error::from)
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::Shutdown,
                source,
            )
        })
    }

    async fn skill_list(&self, request: ProviderCatalogRequest) -> Result<Vec<SkillDefinition>> {
        let settings = crate::settings::SettingsStore::open(&self.settings_root)?;
        let catalog = settings.catalog(
            &request.workspace,
            &request.harness_session_id,
            if self.kind == ProviderKind::Claude {
                "claude"
            } else {
                "antigravity"
            },
        )?;
        let target = if self.kind == ProviderKind::Claude {
            "claudecode"
        } else {
            "antigravity-cli"
        };
        Ok(catalog
            .skill
            .into_iter()
            .filter(|skill| {
                crate::settings::ResourceCatalog::supports_target(&skill.targets, target)
            })
            .map(|skill| SkillDefinition {
                scope: skill.scope,
                resource_id: Some(skill.id.clone()),
                enabled: catalog.settings.desired.skill.get(&skill.id) != Some(&false),
                source: Some(
                    if skill.id.starts_with("import:") {
                        "imported"
                    } else {
                        "rulesync"
                    }
                    .into(),
                ),
                name: skill.name,
                description: skill.description,
                user_invocable: skill.user_invocable,
                path: Some(skill.source),
                argument_hint: None,
            })
            .collect())
    }

    async fn mcp_configuration(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<McpDefinition>> {
        let settings = crate::settings::SettingsStore::open(&self.settings_root)?;
        let catalog = settings.catalog(
            &request.workspace,
            &request.harness_session_id,
            if self.kind == ProviderKind::Claude {
                "claude"
            } else {
                "antigravity"
            },
        )?;
        let target = if self.kind == ProviderKind::Claude {
            "claudecode"
        } else {
            "antigravity-cli"
        };
        Ok(catalog
            .server
            .into_iter()
            .filter(|server| {
                crate::settings::ResourceCatalog::supports_target(&server.targets, target)
            })
            .map(|server| {
                let enabled = catalog.settings.desired.mcp.get(&server.id) != Some(&false);
                McpDefinition {
                    resource_id: Some(server.id),
                    scope: server.scope,
                    source_path: Some(server.source),
                    name: server.name,
                    transport: server.transport.as_str().into(),
                    enabled,
                    status: if enabled {
                        McpStatus::Unavailable
                    } else {
                        McpStatus::Disabled
                    },
                    status_detail: enabled.then(|| {
                        "Configured selection. ACP does not expose server readiness".into()
                    }),
                    token_count: None,
                    token_estimated: false,
                    tools: Vec::new(),
                    tool_error: None,
                }
            })
            .collect())
    }

    async fn mcp_list(&self, request: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
        let mut definition = self.mcp_configuration(request.clone()).await?;
        if let Some(session) = self
            .session
            .lock()
            .await
            .get(&request.runtime_key()?)
            .cloned()
        {
            let catalog = session.native.state().catalog().await;
            for server in &mut definition {
                if let Some(resource) = catalog["mcpResources"]
                    .as_array()
                    .into_iter()
                    .flatten()
                    .find(|resource| {
                        resource["name"] == server.name && resource["state"] == "unavailable"
                    })
                {
                    server.status_detail = resource["reason"].as_str().map(str::to_owned);
                }
            }
        }
        Ok(definition)
    }

    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> Result<crate::provider::events::ProviderEventStream, crate::provider::ProviderError> {
        self.session_events.subscribe(owner)
    }

    async fn session_steer(
        self: std::sync::Arc<Self>,
        owner: &crate::provider::ExecutionOwner,
        scope: crate::provider::InvocationScope,
        input: crate::provider::SteeringInput,
    ) -> Result<(), crate::provider::ProviderError> {
        if scope.owner != *owner {
            return Err(crate::provider::ProviderError::Rejected {
                message: "steering scope belongs to another owner".into(),
            });
        }
        if !self.session_events.running(&scope).await? {
            let request = self.session_events.continuation(&scope, input)?;
            return self.session_prompt(request, scope).await;
        }
        let events = self.session_events.clone();
        let provider = self.clone();
        let continuation = input.clone();
        let result = events
            .steer(&scope, input, |input| async move {
                provider
                    .steer_native(&owner.runtime_key(), input)
                    .await
                    .map_err(crate::provider::ProviderError::from)
            })
            .await;
        if matches!(
            &result,
            Err(crate::provider::ProviderError::NotConsumed { .. })
        ) {
            events.wait_closed(&scope).await?;
            let request = events.continuation(&scope, continuation)?;
            return self.session_prompt(request, scope).await;
        }
        result
    }

    async fn session_prompt(
        self: Arc<Self>,
        request: ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> Result<(), crate::provider::ProviderError> {
        let events = self.session_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::TerminalBoundary::AcpPrompt,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }

    async fn model_list(&self, request: ProviderRequest) -> Result<Vec<ProviderModel>> {
        let session = self.session(catalog_request(&request), None, None).await?;
        session.native.models().await
    }

    async fn fork(&self, request: ProviderForkRequest) -> Result<ProviderForkResult> {
        ensure!(
            self.kind == ProviderKind::Claude,
            "Antigravity does not implement native session fork"
        );
        let session = self
            .session(catalog_request(&request.source), None, None)
            .await?;
        Ok(ProviderForkResult::unprofiled(
            session
                .native
                .fork(&request.source.workspace)
                .await?
                .try_into()?,
        ))
    }

    async fn compact(&self, request: ProviderCompactionRequest) -> Result<ProviderOutput> {
        ensure!(
            self.kind == ProviderKind::Claude,
            "Antigravity has no manual compaction operation"
        );
        let scope = EventScope::SessionOperation {
            owner: request.request.owner()?,
            operation_id: request.id,
        };
        let request = request.request;
        let session = self.session(catalog_request(&request), None, None).await?;
        let mut turn = session
            .turn
            .try_lock()
            .context("ACP session already owns active work")?;
        *turn = PromptEvidence::Unavailable;
        session
            .native
            .configure(&request.model, &request.effort, request.mode)
            .await?;
        let cancelled = Arc::new(AtomicBool::new(false));
        self.cancellation
            .lock()
            .await
            .insert(request.runtime_key()?, cancelled.clone());
        let (sink, mut stream) = events::channel(scope);
        session
            .native
            .state()
            .begin_compaction(session.native.id(), sink)
            .await?;
        self.conversation
            .lock()
            .await
            .insert(request.runtime_key()?, session.native.id().to_owned());
        let mut event = Vec::new();
        let execution = session.native.request(
            "session/prompt",
            json!({"sessionId":session.native.id(),"prompt":[{"type":"text","text":"/compact"}]}),
        );
        tokio::pin!(execution);
        let mut stream_open = true;
        let result = loop {
            tokio::select! {
                update = stream.recv(), if stream_open => match update {
                    Ok(Some(update)) => event.push(update),
                    Ok(None) => stream_open = false,
                    Err(error) => {
                        let cleanup = session.native.shutdown().await;
                        let error: anyhow::Error = error.into();
                        break Err(match cleanup {
                            Ok(()) => error,
                            Err(cleanup) => error.context(format!("ACP compaction cleanup also failed: {cleanup:#}")),
                        });
                    }
                },
                result = &mut execution => break result.map_err(anyhow::Error::from),
            }
        };
        session.native.state().finish_compaction().await;
        while let Some(update) = stream.try_next()? {
            event.push(update);
        }
        let response = result?;
        forge_providers::acp::validate_prompt_response(response.clone()).map_err(|error| {
            ProviderError::Protocol {
                message: format!(
                    "ACP compaction response does not confirm a completed turn: {error}"
                ),
            }
        })?;
        *turn = PromptEvidence::Validated;
        if cancelled.load(Ordering::Acquire) || response["stopReason"] == "cancelled" {
            return Err(crate::broker::TurnCancelled.into());
        }
        let mut completion =
            ProviderCompletion::from(session.native.state().take_completion().await);
        completion.provider_session_id = Some(session.native.id().to_owned().try_into()?);
        completion.capability = session
            .native
            .capabilities(self.descriptor().capability)
            .await;
        let catalog = session.native.state().catalog().await;
        completion.runtime = ProviderRuntime {
            provider: self.descriptor().label,
            model: catalog
                .pointer("/models/currentModelId")
                .and_then(Value::as_str)
                .map(str::to_owned)
                .or_else(|| {
                    find_option(&catalog, "model")
                        .and_then(|option| option["currentValue"].as_str())
                        .map(str::to_owned)
                }),
        };
        Ok(ProviderOutput { event, completion })
    }

    async fn has_active_turn(&self, id: &str) -> bool {
        let session = self.session.lock().await.get(id).cloned();
        match session {
            Some(session) => session.native.state().has_active_work().await,
            None => false,
        }
    }

    /// Interrupt native work and collect its validated terminal response without retiring the reader.
    async fn cancel_session(
        &self,
        id: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            if let Some(cancelled) = self.cancellation.lock().await.get(id) {
                cancelled.store(true, Ordering::Release);
            }
            let session = self.session.lock().await.get(id).cloned();
            if let Some(session) = session {
                session.control.cancel().await;
                session.permission.close().await;
                if session.native.state().has_active_work().await {
                    let terminal = async {
                        session.native.cancel()?;
                        let turn = tokio::time::timeout(
                            std::time::Duration::from_secs(10),
                            session.turn.lock(),
                        )
                        .await
                        .map_err(|_| {
                            ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
                        )
                        })?;
                        match &*turn {
                        PromptEvidence::Validated => Ok(()),
                        PromptEvidence::Unavailable => Err(ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
                        )),
                    }
                    }
                    .await;
                    if let Err(failure) = terminal {
                        return match self.cleanup_execution(id).await {
                            Ok(()) => Err(failure.into()),
                            Err(cleanup) => Err(ProviderError::Combined {
                                primary: Box::new(failure),
                                secondary: vec![cleanup],
                            }
                            .into()),
                        };
                    }
                }
            }
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CancelSession,
                source,
            )
        })
    }

    async fn cleanup_execution(
        &self,
        id: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            if let Some(cancelled) = self.cancellation.lock().await.get(id) {
                cancelled.store(true, Ordering::Release);
            }
            let session = self.session.lock().await.get(id).cloned();
            if let Some(session) = session {
                session.control.cancel().await;
                session.permission.close().await;
                let result = session.native.shutdown().await;
                session.control.close().await;
                let finalized = session
                    .native
                    .state()
                    .finish(crate::turn::TurnOutcome::Cancelled)
                    .await;
                finalized?;
                result?;
                self.session.lock().await.remove(id);
            }
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
                source,
            )
        })
    }

    async fn reset_session(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.cleanup_execution(&owner.runtime_key()).await?;
            self.session_events.reset_owner(owner).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::ResetSession,
                source,
            )
        })
    }

    async fn fence_session(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> =
            async move { Ok(self.cleanup_execution(&owner.runtime_key()).await?) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::FenceSession,
                source,
            )
        })
    }

    async fn active_session_id_for(
        &self,
        id: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(self
            .conversation
            .lock()
            .await
            .get(id)
            .cloned()
            .map(TryInto::try_into)
            .transpose()?)
    }
}

impl AcpProvider {
    async fn steer_native(&self, id: &str, input: crate::provider::SteeringInput) -> Result<()> {
        let text = input.text();
        let session = self
            .session
            .lock()
            .await
            .get(id)
            .cloned()
            .context("ACP conversation is not active")?;
        ensure!(
            self.kind == ProviderKind::Claude
                && session
                    .native
                    .capabilities(self.descriptor().capability)
                    .await
                    .native_steer,
            "ACP agent did not advertise steering support"
        );
        let image_supported = session.native.supports_images();
        let prompt = crate::provider::acp::attachments::input(
            text.to_owned(),
            input.attachments(),
            image_supported,
        )
        .await?;
        let submission = input.submission();
        let scope = session.native.state().scope().await?;
        let acceptance = session
            .native
            .steer(
                json!({"sessionId":session.native.id(),
            "prompt":prompt,"_meta":{"steering":{"idleBehavior":"promptRequired"}}}),
                submission.as_ref().map(|receipt| {
                    receipt.clone() as Arc<dyn forge_provider_api::submission::InputReceipt>
                }),
            )
            .await?;
        if acceptance == forge_providers::acp::submission::SteeringAcceptance::UnobservableExecution
        {
            let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::ObservationLost {
                    failure:
                        forge_provider_api::lifecycle::ObservationFailure::LifecycleUnavailable,
                    completion: None,
                },
            )
            .map_err(|error| ProviderError::Protocol {
                message: error.to_string(),
            })?;
            let _ = session
                .native
                .state()
                .emit(ProviderEvent::Execution { observation })
                .await;
            if let Err(error) = self.cleanup_execution(id).await {
                let _ = session
                    .native
                    .state()
                    .emit(ProviderEvent::Diagnostic {
                        scope: scope.into(),
                        diagnostic: ProviderDiagnostic {
                            code: Some("lifecycle_cleanup_failed".into()),
                            message: format!("{error:#}"),
                            fatal: true,
                        },
                    })
                    .await;
            }
            return Ok(());
        }
        if let Some(start) = input.task_start() {
            let input_id = submission
                .as_ref()
                .map(|submission| submission.record())
                .transpose()?
                .and_then(|record| record.input_message_id);
            let event = ProviderEvent::InputAccepted {
                scope: session.native.state().scope().await?,
                acknowledgement: InputAcknowledgement::TaskStarted {
                    input_id,
                    start: start.clone(),
                    prompt: input.text().to_owned(),
                },
            };
            session.native.state().emit(event).await?;
        }
        Ok(())
    }
}

impl AcpProvider {
    /// Drive admitted native work inside the session-owned task.
    async fn run_native(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
    ) -> std::result::Result<crate::provider::ProviderCompletion, ProviderError> {
        if request.owner().map_err(ProviderError::from)? != sink.invocation_scope()?.owner {
            return Err(ProviderError::Rejected {
                message: "ACP invocation scope does not match the requested owner".into(),
            });
        }
        let output: anyhow::Result<crate::provider::ProviderCompletion> = async {
            let sink = Some(sink);
            let cancelled = request
                .control_context
                .as_ref()
                .and_then(|context| context.submission_cancelled.clone())
                .unwrap_or_else(|| Arc::new(AtomicBool::new(false)));
            let skills = self
                .permission
                .skills(Some(&self.settings_root), &request, self.kind)?;
            let _directories = self
                .permission
                .directories(Some(&self.settings_root), &request)?;
            let sink = skills.sink(sink);
            self.cancellation
                .lock()
                .await
                .insert(request.runtime_key()?, cancelled.clone());
            let tools = crate::tools::ControlToolRegistry.definition_list_for_context(
                request.control_context.as_ref(),
                request.owner()?.is_root(),
            );
            let session = self
                .session(catalog_request(&request), Some(tools), Some(&cancelled))
                .await?;
            let mut turn = session
                .turn
                .try_lock()
                .context("ACP session already owns active work")?;
            *turn = PromptEvidence::Unavailable;
            session
                .native
                .configure(&request.model, &request.effort, request.mode)
                .await?;
            if cancelled.load(Ordering::Acquire) {
                return Err(crate::broker::TurnCancelled.into());
            }
            let text = match &request.input {
                ProviderInput::Text { text, .. } => text.clone(),
                ProviderInput::Skill {
                    name, arguments, ..
                } => format!("/{name} {arguments}"),
            };
            let recovered = request
                .recovered_input
                .iter()
                .map(ProviderInput::text)
                .collect::<Vec<_>>();
            let text = request.recovered_text(&text, &recovered);
            let prompt = if text == "/compact" {
                text
            } else {
                format!("{}\n\n{}", request.system_message(), text)
            };
            let image_supported = session.native.supports_images();
            let prompt = crate::provider::acp::attachments::input(
                prompt,
                &request.input_attachments(),
                image_supported,
            )
            .await?;
            session
                .permission
                .configure(
                    request.approval_mode,
                    request
                        .control_context
                        .as_ref()
                        .and_then(crate::tools::TaskAuthority::capture),
                    sink.clone()
                        .context("ACP execution requires an event sink")?,
                )
                .await;
            session
                .native
                .state()
                .begin(
                    session.native.id(),
                    sink.clone()
                        .context("ACP execution requires an event sink")?,
                )
                .await?;
            session
                .control
                .begin(
                    request
                        .control_context
                        .clone()
                        .unwrap_or_else(|| ControlTurnContext::inactive(request.mode)),
                    sink.context("ACP execution requires an event sink")?,
                )
                .await;
            if let Some(address) = session.native.state().address(session.native.id()).await {
                session
                    .native
                    .state()
                    .emit(ProviderEvent::Turn {
                        address,
                        update: TurnUpdate::Started {
                            started_at_ms: super::children::timestamp(),
                        },
                    })
                    .await?;
            }
            self.conversation
                .lock()
                .await
                .insert(request.runtime_key()?, session.native.id().to_owned());
            let _dispatch = request
                .submission
                .as_ref()
                .map(|submission| submission.begin_prompt())
                .transpose()?;
            let response = session
                .native
                .submit(
                    forge_providers::acp::submission::AcpSubmission::Prompt,
                    json!({"sessionId":session.native.id(),
            "prompt":prompt}),
                    request.submission.as_ref().map(|receipt| {
                        receipt.clone() as Arc<dyn forge_provider_api::submission::InputReceipt>
                    }),
                )
                .await;
            if response.is_ok() {
                *turn = PromptEvidence::Validated;
            }
            if response.is_ok()
                && let Some(address) = session.native.state().address(session.native.id()).await
            {
                let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                    address.scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started { address },
                )?;
                session
                    .native
                    .state()
                    .emit(ProviderEvent::Execution { observation })
                    .await?;
            }
            let outcome = if response.is_ok()
                && (cancelled.load(Ordering::Acquire)
                    || response
                        .as_ref()
                        .is_ok_and(|response| response["stopReason"] == "cancelled"))
            {
                crate::turn::TurnOutcome::Cancelled
            } else if response.is_ok() {
                crate::turn::TurnOutcome::Completed
            } else {
                crate::turn::TurnOutcome::Failed
            };
            session.control.close().await;
            let cleanup = session.native.state().finish(outcome).await;
            session.permission.close().await;
            if outcome == crate::turn::TurnOutcome::Cancelled {
                cleanup?;
                return Err(crate::broker::TurnCancelled.into());
            }
            response.context("ACP prompt failed")?;
            cleanup?;
            let mut output = session.control.take_output().await;
            let evidence = std::mem::take(&mut output.native.evidence);
            output.native = session.native.state().take_completion().await;
            output.native.evidence.structured_complete |= evidence.structured_complete;
            output.native.evidence.structured_blocked |= evidence.structured_blocked;
            output.native.evidence.tool_called |= evidence.tool_called;
            output.native.evidence.workspace_changed |= evidence.workspace_changed;
            output.provider_session_id = Some(session.native.id().to_owned().try_into()?);
            output.capability = session
                .native
                .capabilities(self.descriptor().capability)
                .await;
            let catalog = session.native.state().catalog().await;
            output.runtime = ProviderRuntime {
                provider: self.descriptor().label,
                model: catalog
                    .pointer("/models/currentModelId")
                    .and_then(Value::as_str)
                    .map(str::to_owned)
                    .or_else(|| {
                        find_option(&catalog, "model")
                            .and_then(|option| option["currentValue"].as_str())
                            .map(str::to_owned)
                    }),
            };
            Ok(output)
        }
        .await;
        output.map_err(ProviderError::from)
    }
}

fn catalog_request(request: &ProviderRequest) -> ProviderCatalogRequest {
    ProviderCatalogRequest {
        execution_owner: request.execution_owner.clone(),
        harness_session_id: request.harness_session_id.clone(),
        workspace: request.workspace.clone(),
        access: request.access.clone(),
        approval_mode: request.approval_mode,
        provider_session_id: request.provider_session_id.clone(),
    }
}

fn find_option<'catalog>(catalog: &'catalog Value, category: &str) -> Option<&'catalog Value> {
    catalog["configOptions"]
        .as_array()?
        .iter()
        .find(|option| option["category"] == category || option["id"] == category)
}

/// Declare integration support and limitations before native runtime discovery.
pub(crate) fn capability(kind: ProviderKind) -> ProviderCapability {
    let claude = kind == ProviderKind::Claude;
    ProviderCapability {
        integration: super::super::ProviderIntegration {
            runtime_setup: true,
            authentication: true,
            tracked_submission: true,
            native_agent_suppression: claude,
            resource_target: Some(
                if claude {
                    "claudecode"
                } else {
                    "antigravity-cli"
                }
                .into(),
            ),
            limitations: if claude {
                vec![]
            } else {
                vec![
 forge_provider_api::capability::FeatureLimitation { id: "file_diff".into(), support: forge_provider_api::capability::Support::Limited, reason: "Edit tools can complete without structured diffs; checkpoint review remains available".into() },
 forge_provider_api::capability::FeatureLimitation { id: "approval_rules".into(), support: forge_provider_api::capability::Support::Limited, reason: "Enterprise terminal policy may execute without approval callbacks".into() },
 forge_provider_api::capability::FeatureLimitation { id: "skill_catalog".into(), support: forge_provider_api::capability::Support::Limited, reason: "Project-native skills cannot be excluded by this integration".into() },
 forge_provider_api::capability::FeatureLimitation { id: "skill_mutation".into(), support: forge_provider_api::capability::Support::Limited, reason: "Project-native skills cannot be excluded by this integration".into() },
]
            },
            ..Default::default()
        },
        native_fork: claude,
        native_compact: claude,
        native_steer: claude,
        model_selection: true,
        effort_selection: claude,
        permission_control: true,
        catalog: CatalogCapability {
            skill: true,
            mcp: true,
            live_mcp_mutation: false,
        },
        approval_mode_list: vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow],
        compaction: CompactionCapability {
            lifecycle: claude,
            summary: claude,
            ..Default::default()
        },
        ..Default::default()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::settings::{ImportKind, SettingScope, SettingsStore};

    #[tokio::test]
    async fn cleanup_does_not_wait_for_session_preparation() -> Result<()> {
        for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
            let fixture = tempfile::tempdir()?;
            let provider = AcpProvider::new(
                kind,
                vec!["unused".into()],
                PermissionCoordinator::transient(fixture.path())?,
                fixture.path().join("settings"),
            )?;
            let preparation = Arc::new(Mutex::new(()));
            provider
                .preparation
                .lock()
                .await
                .insert("preparing".into(), preparation.clone());
            let cancelled = Arc::new(AtomicBool::new(false));
            provider
                .cancellation
                .lock()
                .await
                .insert("preparing".into(), cancelled.clone());
            let _preparation = preparation.lock().await;
            tokio::time::timeout(
                std::time::Duration::from_millis(100),
                provider.cleanup_execution("preparing"),
            )
            .await??;
            assert!(cancelled.load(Ordering::Acquire));
            let request = ProviderCatalogRequest {
                execution_owner: None,
                harness_session_id: "preparing".into(),
                workspace: fixture.path().to_string_lossy().into_owned(),
                access: crate::provider::registered_capability(kind)
                    .resolve_access(Default::default()),
                approval_mode: ApprovalMode::Ask,
                provider_session_id: None,
            };
            let failed = provider.session(request, None, Some(&cancelled)).await;
            assert!(failed.err().unwrap().is::<crate::broker::TurnCancelled>());
        }
        Ok(())
    }
    #[tokio::test]
    async fn catalog_includes_disabled_imports_and_refreshes_selection_without_provider_startup()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().join("workspace");
        std::fs::create_dir(&workspace)?;
        let server_source = fixture.path().join("native.json");
        std::fs::write(&server_source, json!({"mcpServers":{"imported-server":{
            "command":std::env::current_exe()?,"targets":["*"],"env":{"TOKEN":"native-credential-fixture"}
        }}}).to_string())?;
        let skill_source = fixture.path().join("native-skill");
        std::fs::create_dir(&skill_source)?;
        std::fs::write(
            skill_source.join("SKILL.md"),
            "---\nname: imported-skill\ndescription: Imported skill\ntargets: ['*']\n---\nPrivate instructions",
        )?;
        let workspace = workspace.to_string_lossy().into_owned();
        let mut settings = SettingsStore::open(fixture.path())?;
        for (revision, kind, source) in [
            (0, ImportKind::Mcp, &server_source),
            (1, ImportKind::Skills, &skill_source),
        ] {
            let preview = settings.preview_import(
                kind,
                source,
                SettingScope::Workspace,
                &workspace,
                "session",
                "claude",
            )?;
            let selected =
                preview.select(&preview.fingerprint, &[preview.candidate[0].id.clone()])?;
            settings.import_operation(
                revision,
                crate::settings::SettingTarget {
                    scope: SettingScope::Workspace,
                    workspace: &workspace,
                    session: "session",
                },
                &selected,
                &format!("import-{revision}"),
                &json!({"fingerprint":preview.fingerprint}),
            )?;
        }
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
        };
        for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
            let provider = AcpProvider::new(
                kind,
                vec!["fixture-must-not-launch".into()],
                PermissionCoordinator::transient(fixture.path())?,
                fixture.path().to_owned(),
            )?;
            let server = provider.mcp_configuration(request.clone()).await?;
            let skill = provider.skill_list(request.clone()).await?;
            assert_eq!(server.len(), 1);
            assert_eq!(skill.len(), 1);
            assert!(!server[0].enabled && !skill[0].enabled);
            assert_eq!(server[0].status, McpStatus::Disabled);
            let mut foreign = request.clone();
            foreign.workspace = "another-workspace".into();
            assert!(
                provider
                    .mcp_configuration(foreign.clone())
                    .await?
                    .is_empty()
            );
            assert!(provider.skill_list(foreign).await?.is_empty());
        }
        let mut desired = settings.get(&workspace, "session", "claude")?.desired;
        for enabled in desired.mcp.values_mut().chain(desired.skill.values_mut()) {
            *enabled = true;
        }
        settings.update(2, SettingScope::Workspace, &workspace, "session", &desired)?;
        std::fs::remove_file(server_source)?;
        for kind in [ProviderKind::Claude, ProviderKind::Antigravity] {
            let provider = AcpProvider::new(
                kind,
                vec!["fixture-must-not-launch".into()],
                PermissionCoordinator::transient(fixture.path())?,
                fixture.path().to_owned(),
            )?;
            let server = provider.mcp_list(request.clone()).await?;
            let skill = provider.skill_list(request.clone()).await?;
            assert!(server[0].enabled && skill[0].enabled);
            assert_eq!(server[0].status, McpStatus::Unavailable);
            assert!(
                server[0]
                    .status_detail
                    .as_ref()
                    .unwrap()
                    .contains("readiness")
            );
            assert!(!serde_json::to_string(&server)?.contains("native-credential-fixture"));
            assert!(provider.session.lock().await.is_empty());
            assert!(provider.conversation.lock().await.is_empty());
        }
        Ok(())
    }
}
