use anyhow::{Context, Result};
use forge_provider_api::control::{ControlToolDefinition, ProviderTools};
use forge_provider_api::permissions::ProviderPermissions;
use forge_provider_api::trace::ProviderTrace;
use forge_provider_api::{PromptMode, ProviderError};
use github_copilot_sdk::session::Session;
use github_copilot_sdk::session_events::ReasoningSummary;
use github_copilot_sdk::types::ToolSearchConfig;
use github_copilot_sdk::{ResumeSessionConfig, SessionConfig, SessionId, SystemMessageConfig};
use serde_json::json;
use std::path::PathBuf;
use std::sync::Arc;

/// Select native discovery or the resource directories prepared by the host.
pub enum ResourceDiscovery {
    Native,
    Managed { skill_directories: Vec<PathBuf> },
}

/// Prepared session settings without an SDK configuration or host workflow state.
pub struct SessionOptions {
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
}

/// Create or resume through the SDK while host callbacks retain policy and tools.
pub async fn open(
    client: &super::handle::CopilotClient,
    options: SessionOptions,
    permissions: Arc<dyn ProviderPermissions>,
    tools: Arc<dyn ProviderTools>,
    trace: &dyn ProviderTrace,
    correlation_id: &str,
) -> Result<super::handle::CopilotSession, ProviderError> {
    let client = &client.inner;
    let result: Result<Session> = async {
        let managed = matches!(&options.resources, ResourceDiscovery::Managed { .. });
        let skill_directories = match &options.resources {
            ResourceDiscovery::Native => None,
            ResourceDiscovery::Managed { skill_directories } => Some(skill_directories.clone()),
        };
        let reasoning_effort = options.reasoning_effort.clone();
        let tool_list = super::control::tool_list(options.tools, tools);
        let system_message = SystemMessageConfig::new()
            .with_mode(if options.mode == PromptMode::Background { "replace" } else { "append" })
            .with_content(options.system_message.clone());
        let permission_handler: Arc<dyn github_copilot_sdk::handler::PermissionHandler> =
            if options.mode == PromptMode::Background {
                Arc::new(github_copilot_sdk::handler::DenyAllHandler)
            } else { super::permission::handler(permissions.clone()) };
        let checkpoint_hooks = super::permission::checkpoint_hooks(permissions);
        let session = if let Some(session_id) = options.session_id.as_deref() {
            let mut config = ResumeSessionConfig::new(SessionId::new(session_id));
            config.suppress_resume_event = Some(true);
            config.excluded_tools = Some(options.excluded_tools.clone());
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
            trace.record(correlation_id, "copilot.session.resume", json!({
                "session_id":session_id, "model":config.model, "reasoning_effort":config.reasoning_effort,
                "context_tier":config.context_tier, "system_message":options.system_message,
                "workspace":options.workspace, "streaming":true, "enable_config_discovery":!managed, "enable_skills":true,
            }));
            if options.mode == PromptMode::Background {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(true);
            }
            client
                .resume_session(
                    config
                        .with_permission_handler(permission_handler)
                        .with_hooks(checkpoint_hooks),
                )
                .await
                .map_err(|error| match error.kind() {
                    github_copilot_sdk::ErrorKind::Session(
                        github_copilot_sdk::SessionErrorKind::NotFound(_),
                    ) => anyhow::Error::new(ProviderError::SessionMissing {
                        message: error.to_string(),
                    }),
                    _ => error.into(),
                })
                .context("resume Copilot session")?
        } else {
            let mut config = SessionConfig::default();
            config.excluded_tools = Some(options.excluded_tools.clone());
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
            trace.record(correlation_id, "copilot.session.create", json!({
                "model":config.model, "reasoning_effort":config.reasoning_effort,
                "context_tier":config.context_tier, "system_message":options.system_message,
                "workspace":options.workspace, "streaming":true, "enable_config_discovery":!managed, "enable_skills":true,
            }));
            if options.mode == PromptMode::Background {
                config.available_tools = Some(Vec::new());
                config.tools = Some(Vec::new());
                config.enable_config_discovery = Some(false);
                config.enable_skills = Some(false);
                config.enable_file_hooks = Some(false);
                config.hooks = Some(false);
                config.skip_custom_instructions = Some(true);
                config.mcp_servers = Some(Default::default());
                config.enable_session_store = Some(true);
            }
            client
                .create_session(
                    config
                        .with_permission_handler(permission_handler)
                        .with_hooks(checkpoint_hooks),
                )
                .await
                .context("create Copilot session")?
        };
        Ok(session)
    }.await;
    result
        .map(|session| super::handle::CopilotSession {
            inner: Arc::new(session),
        })
        .map_err(ProviderError::from)
}
