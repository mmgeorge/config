use anyhow::{Context, Result};
use forge_provider_api::control::{ControlToolDefinition, ProviderTools};
use forge_provider_api::permissions::ProviderPermissions;
use forge_provider_api::trace::ProviderTrace;
use forge_provider_api::{
    PromptMode, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};
use github_copilot_sdk::session_events::ReasoningSummary;
use github_copilot_sdk::types::ToolSearchConfig;
use github_copilot_sdk::{ResumeSessionConfig, SessionConfig, SessionId, SystemMessageConfig};
use std::path::PathBuf;
use std::sync::Arc;

/// Select native discovery or the resource directories prepared by the host.
pub enum ResourceDiscovery {
    Native,
    Managed {
        skill_directories: Vec<PathBuf>,
        selected_mcp: Vec<String>,
    },
}

/// Prepared session settings without an SDK configuration or host workflow state.
pub struct SessionOptions {
    /// Suppress every inherited source and persist no temporary history.
    pub isolated_text: bool,
    pub session_id: Option<String>,
    pub model: Option<String>,
    pub reasoning_effort: Option<String>,
    pub context_tier: Option<String>,
    pub workspace: PathBuf,
    pub mode: PromptMode,
    pub system_message: String,
    pub resources: ResourceDiscovery,
    pub excluded_tools: Vec<String>,
    pub tools: Vec<ControlToolDefinition>,
    pub native_callbacks: Option<Arc<dyn super::provider::NativeCallbackRouter>>,
}

/// Create or resume through the SDK while host callbacks retain policy and tools.
pub async fn open(
    client: &super::handle::CopilotClient,
    options: SessionOptions,
    _permissions: Arc<dyn ProviderPermissions>,
    _tools: Arc<dyn ProviderTools>,
    trace: Arc<dyn ProviderTrace>,
    correlation_id: &str,
) -> Result<super::handle::CopilotSession, ProviderError> {
    super::operation::check()?;
    client.ensure_healthy()?;
    let generation = client.generation.clone();
    // Discover inherited sources before creating a session that could start or authenticate them.
    let selection = if options.isolated_text {
        Some(&[][..])
    } else {
        match &options.resources {
            ResourceDiscovery::Managed { selected_mcp, .. } => Some(
                if options.isolated_text || options.mode == PromptMode::Background {
                    &[][..]
                } else {
                    selected_mcp.as_slice()
                },
            ),
            ResourceDiscovery::Native if options.mode == PromptMode::Background => Some(&[][..]),
            ResourceDiscovery::Native => None,
        }
    };
    let disabled_mcp = if let Some(selection) = selection {
        let discovered = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            client.discover_mcp(&options.workspace.to_string_lossy()),
        )
        .await
        .map_err(|_| {
            ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::SessionOpen,
                forge_provider_api::ProviderFailureKind::Transport,
            ))
        })??;
        let mut disabled = discovered
            .into_iter()
            .filter(|server| !selection.iter().any(|selected| selected == &server.name))
            .map(|server| server.name)
            .collect::<Vec<_>>();
        disabled.sort();
        disabled.dedup();
        Some(disabled)
    } else {
        None
    };
    let client = &client.inner;
    let native_id = options
        .session_id
        .clone()
        .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    // Install the subscription before native session startup and retain its cleanup owner.
    let result: Result<super::handle::CopilotSession> = async {
        let managed = matches!(&options.resources, ResourceDiscovery::Managed { .. });
        let skill_directories = match &options.resources {
            ResourceDiscovery::Native => None,
            ResourceDiscovery::Managed {
                skill_directories, ..
            } => Some(skill_directories.clone()),
        };
        let reasoning_effort = options.reasoning_effort.clone();
        let callbacks: Arc<dyn super::provider::NativeCallbackRouter> = options
            .native_callbacks
            .clone()
            .unwrap_or_else(|| Arc::new(super::provider::NoopNativeCallbacks));
        let tool_list = super::control::tool_list(options.tools, callbacks.clone());
        let system_message = SystemMessageConfig::new()
            .with_mode(
                if options.isolated_text || options.mode == PromptMode::Background {
                    "replace"
                } else {
                    "append"
                },
            )
            .with_content(options.system_message.clone());
        let permission_handler: Arc<dyn github_copilot_sdk::handler::PermissionHandler> =
            if options.isolated_text || options.mode == PromptMode::Background {
                Arc::new(github_copilot_sdk::handler::DenyAllHandler)
            } else {
                super::permission::handler(callbacks.clone())
            };
        let checkpoint_hooks = super::permission::checkpoint_hooks(callbacks);
        super::operation::check()?;
        let prepared = if let Some(session_id) = options.session_id.as_deref() {
            let mut config = ResumeSessionConfig::new(SessionId::new(session_id));
            config.suppress_resume_event = Some(true);
            config.continue_pending_work = Some(false);
            config.excluded_tools = Some(options.excluded_tools.clone());
            config.autopilot = Some(github_copilot_sdk::types::AutopilotOptions {
                continuation_enabled: Some(false),
                max_continues: Some(0),
            });
            config.model = options.model.clone();
            config.reasoning_effort = reasoning_effort.clone();
            config.reasoning_summary = Some(ReasoningSummary::Concise);
            config.context_tier = options.context_tier.clone();
            config.streaming = Some(true);
            config.system_message = Some(system_message);
            config.tools = Some(tool_list);
            config.tool_search = Some(ToolSearchConfig::new().with_enabled(false));
            config.working_directory = Some(options.workspace.clone());
            config.include_sub_agent_streaming_events = Some(true);
            config.enable_config_discovery = Some(!managed);
            config.enable_skills = Some(true);
            config.mcp_servers = managed.then(Default::default);
            config.skill_directories = skill_directories.clone();
            config.disabled_skills = managed.then(Vec::new);
            config.disabled_mcp_servers = disabled_mcp.clone();
            if options.isolated_text || options.mode == PromptMode::Background {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(!options.isolated_text);
            }
            let config = config.with_permission_handler(permission_handler);
            let config = if options.isolated_text {
                config
            } else {
                config.with_hooks(checkpoint_hooks)
            };
            client
                .prepare_resume_session(config)
                .map_err(|error| match error.kind() {
                    github_copilot_sdk::ErrorKind::Session(
                        github_copilot_sdk::SessionErrorKind::NotFound(_),
                    ) => anyhow::Error::new(native_failure(
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::SessionMissing,
                    )),
                    _ => error.into(),
                })
                .context("resume Copilot session")?
        } else {
            let mut config = SessionConfig::default();
            config.session_id = Some(SessionId::new(native_id.clone()));
            config.excluded_tools = Some(options.excluded_tools.clone());
            config.autopilot = Some(github_copilot_sdk::types::AutopilotOptions {
                continuation_enabled: Some(false),
                max_continues: Some(0),
            });
            config.model = options.model.clone();
            config.reasoning_effort = reasoning_effort;
            config.reasoning_summary = Some(ReasoningSummary::Concise);
            config.context_tier = options.context_tier.clone();
            config.streaming = Some(true);
            config.system_message = Some(system_message);
            config.tools = Some(tool_list);
            config.tool_search = Some(ToolSearchConfig::new().with_enabled(false));
            config.working_directory = Some(options.workspace.clone());
            config.include_sub_agent_streaming_events = Some(true);
            config.enable_config_discovery = Some(!managed);
            config.enable_skills = Some(true);
            config.mcp_servers = managed.then(Default::default);
            config.skill_directories = skill_directories;
            config.disabled_skills = managed.then(Vec::new);
            config.disabled_mcp_servers = disabled_mcp.clone();
            if options.isolated_text || options.mode == PromptMode::Background {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(!options.isolated_text);
            }
            let config = config.with_permission_handler(permission_handler);
            let config = if options.isolated_text {
                config
            } else {
                config.with_hooks(checkpoint_hooks)
            };
            client
                .prepare_session(config)
                .context("prepare Copilot session")?
        };
        let mut events = super::event_stream::SessionEventStream::from_subscription(
            prepared.subscribe(),
            native_id,
            |_| true,
            trace.clone(),
            correlation_id.to_owned(),
            super::event_stream::StartupState::Pending,
            generation.clone(),
        );
        let start = || prepared.start();
        let started = async {
            super::operation::dispatch()?;
            start()
                .await
                .map_err(|error| match error.kind() {
                    github_copilot_sdk::ErrorKind::Session(
                        github_copilot_sdk::SessionErrorKind::NotFound(_),
                    ) => anyhow::Error::new(native_failure(
                        ProviderOperation::SessionOpen,
                        ProviderFailureKind::SessionMissing,
                    )),
                    _ => error.into(),
                })
                .context("start prepared Copilot session")
        }
        .await;
        events.finish_startup(match &started {
            Ok(_) => super::event_stream::StartupOutcome::Started,
            Err(_) => super::event_stream::StartupOutcome::Failed,
        });
        let session = match started {
            Ok(session) => session,
            Err(error) => {
                let cause = error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    generation
                        .failure()
                        .map(ProviderError::Observation)
                        .unwrap_or_else(|| {
                            native_failure(
                                ProviderOperation::SessionOpen,
                                ProviderFailureKind::Initialization,
                            )
                        })
                });
                if let Err(_cleanup) = events.close().await {
                    return Err(ProviderError::PreparationCleanup {
                        cause: Box::new(cause),
                        cleanup: Box::new(super::error::native_failure(
                            ProviderOperation::SessionClose,
                            ProviderFailureKind::Runtime,
                        )),
                    }
                    .into());
                }
                return Err(cause.into());
            }
        };
        Ok(super::handle::CopilotSession {
            inner: Arc::new(session),
            events: super::intake::SessionIntake::new(events, generation.clone()),
            generation,
        })
    }
    .await;
    result.map_err(|error| {
        error.downcast::<ProviderError>().unwrap_or_else(|_| {
            native_failure(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::Initialization,
            )
        })
    })
}

/// Confirm the native instruction policy before publishing an isolated binding.
/// The SDK applies this flag after create, but does not check the response's success field.
pub(super) async fn confirm_isolated_instructions(
    session: &super::handle::CopilotSession,
) -> Result<(), ProviderError> {
    super::operation::check()?;
    let patch = github_copilot_sdk::rpc::SessionUpdateOptionsParams {
        skip_custom_instructions: Some(true),
        ..Default::default()
    };
    let result = session
        .inner
        .rpc()
        .options()
        .update(patch)
        .await
        .map_err(|_| {
            ProviderError::Failure(
                ProviderFailure::new(ProviderOperation::SessionOpen, ProviderFailureKind::Policy)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Preparation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
            )
        })?;
    if !result.success {
        return Err(ProviderError::Failure(
            ProviderFailure::new(ProviderOperation::SessionOpen, ProviderFailureKind::Policy)
                .at_phase(
                    forge_provider_api::ProviderPhase::Preparation,
                    forge_provider_api::FailureDisposition::Invalidated,
                ),
        ));
    }
    Ok(())
}

fn native_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    struct Authority;
    #[async_trait::async_trait]
    impl forge_provider_api::permissions::ProviderPermissions for Authority {
        /// Reject native permissions in the isolated fixture.
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: forge_provider_api::permissions::PermissionRequest,
        ) -> forge_provider_api::permissions::PermissionResponse {
            forge_provider_api::permissions::PermissionResponse::Cancelled
        }
        /// Reject tool admission in the isolated fixture.
        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Err(ProviderError::Cancelled)
        }
    }
    #[async_trait::async_trait]
    impl forge_provider_api::control::ProviderTools for Authority {
        /// Detect any unexpected host tool dispatch from the isolated session.
        async fn call(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: forge_provider_api::control::ToolCall,
        ) -> Result<String, ProviderError> {
            panic!("isolated session called a host tool")
        }
    }

    struct Trace;
    impl forge_provider_api::trace::ProviderTrace for Trace {
        /// Discard fixture diagnostics while recording actual SDK requests at the peer.
        fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
    }

    /// Ordinary isolated open suppresses inherited configuration at the actual native boundary.
    #[tokio::test]
    async fn isolated_open_disables_inherited_authority_and_native_continuation() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (native, peer) = super::super::test_peer::client(|request| {
                let result = match request["method"].as_str().unwrap() {
                    "mcp.discover" => json!({"servers":[]}),
                    "session.create" => {
                        let params = &request["params"];
                        for key in ["enableConfigDiscovery", "enableSkills", "enableFileHooks", "hooks", "enableSessionStore"] {
                            assert_eq!(params[key], false, "isolation failed for {key}");
                        }
                        assert_eq!(params["availableTools"], json!([]));
                        assert_eq!(params["tools"], json!([]));
                        assert_eq!(params["mcpServers"], json!({}));
                        assert_eq!(params["autopilot"]["continuationEnabled"], false);
                        assert_eq!(params["autopilot"]["maxContinues"], 0);
                        assert_eq!(params["systemMessage"]["mode"], "replace");
                        assert_eq!(params["systemMessage"]["content"], "supplied instructions");
                        json!({"sessionId":params["sessionId"], "workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" => {
                        assert_eq!(request["params"]["skipCustomInstructions"], true);
                        json!({"success":true})
                    }
                    "session.detach" => json!({"success":true}),
                    method => panic!("unexpected native method during isolated open: {method}"),
                };
                vec![super::super::test_peer::response(request, result)]
            });
            let client = super::super::handle::CopilotClient::new(native);
            let session = open(&client, SessionOptions {
                isolated_text: true,
                session_id: None, model: None, reasoning_effort: None, context_tier: None,
                workspace: std::env::temp_dir(), mode: PromptMode::Background,
                system_message: "supplied instructions".into(), resources: ResourceDiscovery::Native,
                excluded_tools: Vec::new(), tools: Vec::new(), native_callbacks: None,
            }, Arc::new(Authority), Arc::new(Authority), Arc::new(Trace), "isolated").await.unwrap();
            confirm_isolated_instructions(&session).await.unwrap();
            session.disconnect().await.unwrap();
            client.stop().await.unwrap();
            session.disconnect().await.unwrap();
            let requests = peer.collected().await;
            assert_eq!(requests.iter().filter(|request| request["method"] == "session.detach").count(), 1, "collected SDK generation attempted a dead native detach");
            assert!(requests.iter().all(|request| request["method"] != "session.resume"));
            let methods = requests.iter().map(|request| request["method"].as_str().unwrap()).collect::<Vec<_>>();
            assert_eq!(methods, ["mcp.discover", "session.create", "session.options.update", "session.options.update", "session.detach"]);
        }).await.expect("isolated ordinary session fixture timed out");
    }
}
