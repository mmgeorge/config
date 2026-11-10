use anyhow::{Context, Result};
use forge_provider_api::control::{ControlToolDefinition, ProviderTools};
use forge_provider_api::permissions::ProviderPermissions;
use forge_provider_api::trace::ProviderTrace;
use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use github_copilot_sdk::session_events::ReasoningSummary;
use github_copilot_sdk::types::ToolSearchConfig;
use github_copilot_sdk::{ResumeSessionConfig, SessionConfig, SessionId, SystemMessageConfig};
use std::path::PathBuf;
use std::sync::Arc;

/// Retain the complete host-selected resources without implicit native discovery.
#[derive(Default)]
pub struct SessionResources {
    pub skill_directories: Vec<PathBuf>,
    pub selected_mcp: Vec<String>,
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
    pub system_message: String,
    pub resources: SessionResources,
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
        &[][..]
    } else {
        &options.resources.selected_mcp
    };
    let discovered =
        super::operation::wait(client.discover_mcp(&options.workspace.to_string_lossy())).await??;
    let mut disabled_mcp = discovered
        .into_iter()
        .filter(|server| !selection.iter().any(|selected| selected == &server.name))
        .map(|server| server.name)
        .collect::<Vec<_>>();
    disabled_mcp.sort();
    disabled_mcp.dedup();
    let shell_client = client.clone();
    let client = &client.inner;
    let native_id = options
        .session_id
        .clone()
        .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    // Install the subscription before native session startup and retain its cleanup owner.
    let result: Result<super::handle::CopilotSession> = async {
        let skill_directories = options.resources.skill_directories.clone();
        let reasoning_effort = options.reasoning_effort.clone();
        let callbacks: Arc<dyn super::provider::NativeCallbackRouter> = options
            .native_callbacks
            .clone()
            .unwrap_or_else(|| Arc::new(super::provider::NoopNativeCallbacks));
        let mut tool_list = super::control::tool_list(options.tools, callbacks.clone());
        if !options.isolated_text
            && let Some(shell) = callbacks.shell()
        {
            shell
                .prepare(shell_client, options.workspace.clone(), &native_id)
                .await?;
            tool_list.extend(super::shell_tools::tools(shell));
        }
        let system_message = SystemMessageConfig::new()
            .with_mode(if options.isolated_text {
                "replace"
            } else {
                "append"
            })
            .with_content(options.system_message.clone());
        let permission_handler: Arc<dyn github_copilot_sdk::handler::PermissionHandler> =
            if options.isolated_text {
                Arc::new(github_copilot_sdk::handler::DenyAllHandler)
            } else {
                super::permission::handler(callbacks.clone())
            };
        let checkpoint_hooks = super::permission::checkpoint_hooks(callbacks);
        super::operation::check()?;
        let prepared = if let Some(session_id) = options.session_id.as_deref() {
            let mut config = ResumeSessionConfig::new(SessionId::new(session_id));
            config.event_buffer_capacity = Some(forge_protocol::MAX_PENDING_FRAMES);
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
            config.enable_config_discovery = Some(false);
            config.enable_skills = Some(true);
            config.mcp_servers = Some(Default::default());
            config.skill_directories = Some(skill_directories.clone());
            config.disabled_skills = Some(Vec::new());
            config.disabled_mcp_servers = Some(disabled_mcp.clone());
            if options.isolated_text {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(false);
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
                        ProviderOperation::ThreadOpen,
                        ProviderFailureKind::SessionMissing,
                    )),
                    _ => error.into(),
                })
                .context("resume Copilot session")?
        } else {
            let mut config = SessionConfig::default();
            config.event_buffer_capacity = Some(forge_protocol::MAX_PENDING_FRAMES);
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
            config.enable_config_discovery = Some(false);
            config.enable_skills = Some(true);
            config.mcp_servers = Some(Default::default());
            config.skill_directories = Some(skill_directories);
            config.disabled_skills = Some(Vec::new());
            config.disabled_mcp_servers = Some(disabled_mcp.clone());
            if options.isolated_text {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(false);
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
                        ProviderOperation::ThreadOpen,
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
                tracing::error!(target: "forge::provider", event_name = "provider.session.failed",
                    provider = "copilot", error = %format_args!("{error:#}"));
                let cause = error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    generation
                        .failure()
                        .map(ProviderError::Observation)
                        .unwrap_or_else(|| {
                            native_failure(
                                ProviderOperation::ThreadOpen,
                                ProviderFailureKind::Initialization,
                            )
                        })
                });
                if let Err(_cleanup) = events.close().await {
                    return Err(ProviderError::PreparationCleanup {
                        cause: Box::new(cause),
                        cleanup: Box::new(super::error::native_failure(
                            ProviderOperation::ThreadClose,
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
            mcp_loading: Default::default(),
        })
    }
    .await;
    result.map_err(|error| {
        tracing::error!(target: "forge::provider", event_name = "provider.session.failed",
            provider = "copilot", error = %format_args!("{error:#}"));
        error.downcast::<ProviderError>().unwrap_or_else(|_| {
            native_failure(
                ProviderOperation::ThreadOpen,
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
                ProviderFailure::new(ProviderOperation::ThreadOpen, ProviderFailureKind::Policy)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Preparation,
                        forge_provider_api::FailureDisposition::Invalidated,
                    ),
            )
        })?;
    if !result.success {
        return Err(ProviderError::Failure(
            ProviderFailure::new(ProviderOperation::ThreadOpen, ProviderFailureKind::Policy)
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
