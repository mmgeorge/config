//! One live CLI process and the native configuration it was launched with.
//!
//! A conversation keeps its session id across process restarts: rewind and launch-time
//! configuration changes close the idle process and resume the same session in a new one.
//!
//! The CLI writes a conversation to disk when its first turn runs, so a session id that has only
//! ever launched names nothing to resume. `UnstartedSessions` records those ids, and
//! `launch_conversation` starts such a conversation again when its resume finds nothing. It also
//! records the fork targets that have run no turn, whose copied history ends in the note that
//! materialized them, and `launch_conversation` resumes such a target before that note.

use super::process::{ClaudeLauncher, LaunchSpec, McpConfig, SessionStart};
use super::session::ClaudeSessionState;
use super::transport::{ClaudeTransport, FrameHandler, TransportFailure};
use super::wire::content::InputBlock;
use super::wire::control::{
    InboundControlRequest, Initialize, InitializeResponse, OutboundControl,
};
use super::wire::stdin::UserInput;
use super::wire::stdout::StdoutFrame;
use async_trait::async_trait;
use forge_provider_api::{
    FailureDisposition, ProviderError, ProviderFailure, ProviderFailureKind, ProviderModel,
    ProviderOperation, ProviderPhase,
};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::RwLock;
use tokio_util::sync::CancellationToken;

/// How long a `get_context_usage` answer may take before the adapter goes without it.
///
/// The CLI answers from local state in milliseconds when healthy. The figure is a status-line
/// decoration and the instruction sources are optional rows, so a few seconds is generous, and
/// a turn never waits longer than that for either after the CLI has ended it.
const CONTEXT_USAGE_DEADLINE: std::time::Duration = std::time::Duration::from_secs(3);

/// Record the session ids this provider minted under which the CLI has run no turn.
///
/// Harness learns a session id when the session opens and resumes it after every provider
/// restart, but the CLI answers `--resume` of an id it never ran a turn under with "No
/// conversation found" and exits. An id held here may therefore be started again under
/// `--session-id` when its resume fails. An id leaves when a turn the CLI started ends, because
/// starting that conversation again would replace history Harness still shows. An id whose
/// session never ran a turn stays for the provider's lifetime.
///
/// A fork target is written by a note the adapter appends without a model call, so its newest
/// entry is that note. Resuming it plainly would hand the model the note followed by the
/// "No response requested." reply the CLI adds after a trailing user entry. A target held here
/// therefore resumes at the entry its fork copied up to, which discards the note when the first
/// prompt is appended. The record lives in the provider, so a target first opened after an editor
/// restart resumes plainly.
#[derive(Clone, Default)]
pub(crate) struct UnstartedSessions {
    session_ids: Arc<std::sync::Mutex<HashSet<String>>>,
    fork_points: Arc<std::sync::Mutex<HashMap<String, String>>>,
}

impl UnstartedSessions {
    /// Record a session id minted for a conversation the CLI has not run.
    pub(crate) fn mint(&self, session_id: &str) {
        self.session_ids
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(session_id.to_owned());
    }

    /// Report whether the CLI has run no turn under a session id this provider minted.
    fn holds(&self, session_id: &str) -> bool {
        self.session_ids
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .contains(session_id)
    }

    /// Record a fork target whose copied history ends at `fork_point`, before its note.
    pub(crate) fn fork(&self, session_id: &str, fork_point: &str) {
        self.fork_points
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(session_id.to_owned(), fork_point.to_owned());
    }

    /// Name the entry a fork target that has run no turn resumes at.
    fn fork_point(&self, session_id: &str) -> Option<String> {
        self.fork_points
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(session_id)
            .cloned()
    }

    /// Forget a session id whose conversation the CLI has run a turn under.
    fn started(&self, session_id: &str) {
        self.session_ids
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(session_id);
        self.fork_points
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(session_id);
    }
}

/// Deliver one process's frames to its conversation, and its exit only once it serves the
/// conversation.
///
/// A process that exits before its initialize handshake never served the conversation. `launch`
/// returns that failure to its caller, which may launch another process for the same
/// conversation, so the exit must not invalidate the session state both processes share.
struct ProcessFrames {
    state: Arc<ClaudeSessionState>,
    serving: AtomicBool,
    /// The errors of a `result` the CLI printed before it served the conversation.
    ///
    /// A CLI that refuses its command line, such as a guarded resume whose discarded range holds
    /// entries of another turn, prints its reason in such a `result` and exits; the closed
    /// output alone does not say why.
    startup_errors: std::sync::Mutex<Vec<String>>,
}

impl ProcessFrames {
    /// Join the reasons the CLI gave before it served the conversation, if it gave any.
    fn startup_refusal(&self) -> Option<String> {
        let errors = self
            .startup_errors
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        (!errors.is_empty()).then(|| errors.join("; "))
    }
}

#[async_trait]
impl FrameHandler for ProcessFrames {
    async fn frame(&self, frame: StdoutFrame) {
        if !self.serving.load(Ordering::Acquire)
            && let StdoutFrame::Result(result) = &frame
        {
            self.startup_errors
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .extend(result.errors.iter().cloned());
        }
        FrameHandler::frame(&*self.state, frame).await;
    }

    async fn control(
        &self,
        request: InboundControlRequest,
        cancelled: CancellationToken,
    ) -> Result<Value, String> {
        FrameHandler::control(&*self.state, request, cancelled).await
    }

    async fn closed(&self, failure: Option<TransportFailure>) {
        if self.serving.load(Ordering::Acquire) {
            FrameHandler::closed(&*self.state, failure).await;
        }
    }
}

/// Native configuration a conversation applies at every launch.
#[derive(Clone)]
pub(crate) struct ConversationOptions {
    pub process_owner: Option<Arc<dyn forge_shell::process::ProcessOwner>>,
    pub workspace: String,
    pub system_instructions: String,
    pub mcp_servers: Map<String, Value>,
    pub plugin_dir: Option<std::path::PathBuf>,
    pub model: Option<String>,
    pub effort: Option<String>,
    pub planning: bool,
}

/// The process currently serving a conversation.
struct LiveProcess {
    transport: Arc<ClaudeTransport>,
    initialized: InitializeResponse,
}

/// Own the conversation's process and native session id.
pub(crate) struct ClaudeConversation {
    session_id: String,
    launcher: Arc<dyn ClaudeLauncher>,
    state: Arc<ClaudeSessionState>,
    unstarted: UnstartedSessions,
    options: RwLock<ConversationOptions>,
    live: RwLock<LiveProcess>,
}

impl ClaudeConversation {
    /// Launch a process for the conversation and initialize it.
    pub(crate) async fn open(
        launcher: Arc<dyn ClaudeLauncher>,
        state: Arc<ClaudeSessionState>,
        session_id: String,
        start: SessionStart,
        options: ConversationOptions,
        unstarted: UnstartedSessions,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<Self, ProviderError> {
        let live =
            launch_conversation(&*launcher, &state, &options, start, &unstarted, cleanup).await?;
        state
            .update_catalog(|catalog| {
                catalog.models = live.initialized.models.clone();
                catalog.version = live.initialized.claude_code_version.clone();
                if catalog.selected_model.is_none() {
                    catalog.selected_model = options.model.clone().or(Some("default".into()));
                }
            })
            .await;
        Ok(Self {
            session_id,
            launcher,
            state,
            unstarted,
            options: RwLock::new(options),
            live: RwLock::new(live),
        })
    }

    /// Get the durable native session id.
    pub(crate) fn id(&self) -> &str {
        &self.session_id
    }

    /// Record that a turn the CLI started has ended, so the conversation is on disk.
    pub(crate) fn observe_turn(&self) {
        self.unstarted.started(&self.session_id);
    }

    /// Report the CLI version from the initialize handshake.
    pub(crate) async fn version(&self) -> Option<String> {
        self.live
            .read()
            .await
            .initialized
            .claude_code_version
            .clone()
    }

    async fn transport(&self) -> Arc<ClaudeTransport> {
        self.live.read().await.transport.clone()
    }

    /// Write one user input frame.
    pub(crate) async fn write(&self, input: &UserInput) -> Result<(), ProviderError> {
        let frame = serde_json::to_value(input)
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))?;
        self.transport()
            .await
            .send(&frame)
            .await
            .map_err(|error| transport_error(ProviderOperation::TurnStart, error))
    }

    /// Send one control request under the requesting operation.
    pub(crate) async fn request(
        &self,
        operation: ProviderOperation,
        request: OutboundControl,
    ) -> Result<Value, ProviderError> {
        self.transport()
            .await
            .request(request)
            .await
            .map_err(|error| transport_error(operation, error))
    }

    /// Interrupt the running turn and discard input queued behind it.
    pub(crate) async fn interrupt(&self) -> Result<(), ProviderError> {
        self.request(
            ProviderOperation::TurnInterrupt,
            OutboundControl::Interrupt {
                cancel_queued: Some(true),
            },
        )
        .await
        .map(|_| ())
    }

    /// Ask the CLI to list the host tools again after the invocation's set changed.
    pub(crate) async fn host_tools_changed(&self) -> Result<(), ProviderError> {
        self.request(
            ProviderOperation::TurnStart,
            OutboundControl::McpMessage {
                server_name: super::tool_targets::HOST_TOOL_SERVER.to_owned(),
                message: super::host_tools::list_changed(),
            },
        )
        .await
        .map(|_| ())
    }

    /// Ask the CLI for its context accounting, giving up when `cancellation` fires or the answer
    /// outlives `CONTEXT_USAGE_DEADLINE`.
    ///
    /// Both readers of `get_context_usage` treat the answer as optional: the window figure only
    /// decorates the status line, and missing instruction sources only omit initial read rows.
    /// The CLI computes the answer itself and has once left it unanswered for ten minutes after
    /// a turn's `result`, which held the finished turn, and every interrupt of it, until the
    /// answer came. An abandoned request leaves its waiter registered, and the late answer then
    /// finds no receiver.
    async fn context_accounting(&self, cancellation: &CancellationToken) -> Option<Value> {
        let request = self.request(
            ProviderOperation::TurnStart,
            OutboundControl::GetContextUsage {},
        );
        let answer = tokio::select! {
            biased;
            _ = cancellation.cancelled() => return None,
            answer = tokio::time::timeout(CONTEXT_USAGE_DEADLINE, request) => answer,
        };
        match answer {
            Ok(Ok(usage)) => Some(usage),
            Ok(Err(error)) => {
                tracing::warn!(?error, "Claude context usage unavailable");
                None
            }
            Err(_) => {
                tracing::warn!(
                    deadline_ms = CONTEXT_USAGE_DEADLINE.as_millis() as u64,
                    "Claude context usage did not arrive before its deadline"
                );
                None
            }
        }
    }

    /// Read the conversation's context-window usage, or nothing when the CLI does not answer.
    pub(crate) async fn context_usage(
        &self,
        cancellation: &CancellationToken,
    ) -> Option<forge_provider_api::context::ContextUsage> {
        let usage = self.context_accounting(cancellation).await?;
        let used = usage["totalTokens"].as_u64()?;
        let size = usage["maxTokens"].as_u64()?;
        forge_provider_api::context::ContextUsage::reported(used, size)
    }

    /// Return the native session's instruction-path base directory.
    pub(crate) async fn workspace(&self) -> String {
        self.options.read().await.workspace.clone()
    }

    /// Query files already included before SDK instruction callbacks could be registered, or
    /// none when the CLI does not answer.
    pub(crate) async fn instruction_sources(
        &self,
        cancellation: &CancellationToken,
    ) -> Vec<String> {
        let Some(usage) = self.context_accounting(cancellation).await else {
            return Vec::new();
        };
        usage
            .get("memoryFiles")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(|file| {
                file.get("path")
                    .and_then(serde_json::Value::as_str)
                    .map(str::to_owned)
            })
            .collect()
    }

    /// Apply the model, effort, and permission mode of the next turn.
    pub(crate) async fn configure(
        &self,
        model: Option<&str>,
        effort: Option<&str>,
        planning: bool,
    ) -> Result<(), ProviderError> {
        let operation = ProviderOperation::ThreadConfigure;
        let mut options = self.options.write().await;
        let model = model.map(str::to_owned);
        if options.model != model {
            self.request(
                operation,
                OutboundControl::SetModel {
                    model: model.clone().filter(|model| model != "default"),
                },
            )
            .await?;
            options.model = model.clone();
            self.state
                .update_catalog(|catalog| {
                    catalog.selected_model = model.clone().or(Some("default".into()));
                })
                .await;
        }
        let effort = effort.map(str::to_owned);
        if options.effort != effort {
            self.request(
                operation,
                OutboundControl::ApplyFlagSettings {
                    settings: json!({"effortLevel": effort}),
                },
            )
            .await?;
            options.effort = effort;
        }
        if options.planning != planning {
            self.request(
                operation,
                OutboundControl::SetPermissionMode {
                    mode: if planning { "plan" } else { "default" }.to_owned(),
                },
            )
            .await?;
            options.planning = planning;
        }
        Ok(())
    }

    /// Replace the launch-time resources and restart the idle process on the same session.
    pub(crate) async fn reconfigure(
        &self,
        mcp_servers: Map<String, Value>,
        plugin_dir: Option<std::path::PathBuf>,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<(), ProviderError> {
        {
            let mut options = self.options.write().await;
            options.mcp_servers = mcp_servers;
            options.plugin_dir = plugin_dir;
        }
        self.restart(
            SessionStart::Resume {
                session_id: self.session_id.clone(),
                resume_at: None,
                drops_turn: None,
            },
            cleanup,
        )
        .await
    }

    /// Close the idle process and resume the session in a new one.
    pub(crate) async fn restart(
        &self,
        start: SessionStart,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<(), ProviderError> {
        let mut live = self.live.write().await;
        live.transport.retire().await?;
        let options = self.options.read().await.clone();
        *live = launch_conversation(
            &*self.launcher,
            &self.state,
            &options,
            start,
            &self.unstarted,
            cleanup,
        )
        .await?;
        Ok(())
    }

    /// Launch a disposable process that copies this conversation into a new session id.
    pub(crate) async fn fork(
        &self,
        target_id: &str,
        fork_point: Option<String>,
        options: ConversationOptions,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<(), ProviderError> {
        fork_session(
            &*self.launcher,
            &self.unstarted,
            &self.session_id,
            target_id,
            fork_point,
            options,
            cleanup,
        )
        .await
    }

    /// Get the models the CLI offers.
    pub(crate) async fn models(&self) -> Vec<ProviderModel> {
        let selected = self.state.catalog().await.selected_model;
        provider_models(
            &self.live.read().await.initialized,
            selected.as_deref().unwrap_or("default"),
        )
    }

    /// Report whether the current process's output has ended.
    pub(crate) async fn is_closed(&self) -> bool {
        self.transport().await.is_closed()
    }
}

/// Launch a process for a retained conversation, starting the conversation again when the CLI
/// never ran it.
///
/// The resume is attempted first because only the CLI knows whether it wrote the conversation: a
/// prompt it received before it froze or was replaced may or may not have reached disk.
async fn launch_conversation(
    launcher: &dyn ClaudeLauncher,
    state: &Arc<ClaudeSessionState>,
    options: &ConversationOptions,
    start: SessionStart,
    unstarted: &UnstartedSessions,
    cleanup: &crate::resource_cleanup::ResourceCleanup,
) -> Result<LiveProcess, ProviderError> {
    let start = match start {
        SessionStart::Resume {
            session_id,
            resume_at: None,
            drops_turn: None,
        } => SessionStart::Resume {
            resume_at: unstarted.fork_point(&session_id),
            session_id,
            drops_turn: None,
        },
        start => start,
    };
    let restart = match &start {
        SessionStart::Resume {
            session_id,
            resume_at: None,
            ..
        } if unstarted.holds(session_id) => Some(SessionStart::Fresh {
            session_id: session_id.clone(),
        }),
        _ => None,
    };
    match (
        launch(launcher, state, options, start, cleanup).await,
        restart,
    ) {
        (Err(ProviderError::Failure(failure)), Some(restart))
            if failure.kind() == ProviderFailureKind::Initialization =>
        {
            launch(launcher, state, options, restart, cleanup).await
        }
        (launched, _) => launched,
    }
}

/// Launch a process and complete its initialize handshake.
async fn launch(
    launcher: &dyn ClaudeLauncher,
    state: &Arc<ClaudeSessionState>,
    options: &ConversationOptions,
    start: SessionStart,
    cleanup: &crate::resource_cleanup::ResourceCleanup,
) -> Result<LiveProcess, ProviderError> {
    if !cleanup.is_admitting() {
        return Err(failure(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::BindingClosed,
        ));
    }
    let spec = launch_spec(options, start)?;
    let io = launcher.launch(&spec)?;
    let frames = Arc::new(ProcessFrames {
        state: state.clone(),
        serving: AtomicBool::new(false),
        startup_errors: std::sync::Mutex::new(Vec::new()),
    });
    let transport = Arc::new(ClaudeTransport::start(io, frames.clone(), cleanup));
    let initialized = transport
        .request(OutboundControl::Initialize(initialize(
            options,
            &spec.start,
        )))
        .await;
    let initialized = match initialized {
        Ok(initialized) => initialized,
        Err(error) => {
            let _ = transport.shutdown().await;
            // The reader delivers every frame before it reports the closed output, so a reason
            // the CLI printed before it exited is already recorded.
            if let Some(reason) = frames.startup_refusal() {
                tracing::error!(target: "forge::provider", event_name = "provider.startup.refused",
                    provider = "claude", reason = %reason);
            }
            return Err(initialization_failure(error));
        }
    };
    let initialized: InitializeResponse = serde_json::from_value(initialized).map_err(|_| {
        failure(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Initialization,
        )
    })?;
    if let Err(error) = super::process::runtime_requirement(launcher.executable())
        .validate(initialized.claude_code_version.as_deref())
    {
        let cause = ProviderError::from(error);
        return Err(match transport.shutdown().await {
            Ok(()) => cause,
            Err(cleanup) => ProviderError::Combined {
                primary: Box::new(cause),
                secondary: vec![cleanup],
            },
        });
    }
    frames.serving.store(true, Ordering::Release);
    // The reader marks the transport closed before it reports the exit, so an exit that raced the
    // line above and went unreported is still visible here.
    if transport.is_closed() {
        let _ = transport.shutdown().await;
        return Err(initialization_failure(TransportFailure::Closed));
    }
    Ok(LiveProcess {
        transport,
        initialized,
    })
}

/// Launch, materialize, and close a fork of `source_id` under `target_id`.
///
/// `fork_point` names the last entry the fork copies; without it the CLI chooses the source's
/// leaf. A named point is also where the target's first launch resumes, before the note.
pub(crate) async fn fork_session(
    launcher: &dyn ClaudeLauncher,
    unstarted: &UnstartedSessions,
    source_id: &str,
    target_id: &str,
    fork_point: Option<String>,
    options: ConversationOptions,
    cleanup: &crate::resource_cleanup::ResourceCleanup,
) -> Result<(), ProviderError> {
    let state = Arc::new(ClaudeSessionState::new(Arc::new(
        std::sync::atomic::AtomicBool::new(true),
    )));
    let start = SessionStart::Fork {
        source_id: source_id.to_owned(),
        target_id: target_id.to_owned(),
        resume_at: fork_point.clone(),
    };
    let live = launch(launcher, &state, &options, start, cleanup).await?;
    // A fork is written only when an entry is appended; a note appends one without a model call.
    let note = UserInput::note(
        uuid::Uuid::new_v4().to_string(),
        "[Forge: this conversation was forked from an earlier conversation]",
    );
    let note_id = note.uuid.clone();
    let delivered = state.track_command(&note_id, false).await;
    let result = async {
        let mut delivered = delivered?;
        let frame = serde_json::to_value(&note)
            .map_err(|_| failure(ProviderOperation::ThreadFork, ProviderFailureKind::Protocol))?;
        live.transport
            .send(&frame)
            .await
            .map_err(|error| transport_error(ProviderOperation::ThreadFork, error))?;
        wait_command(
            &mut delivered,
            |state| state == super::wire::stdout::CommandState::Completed,
            ProviderOperation::ThreadFork,
        )
        .await
        .map(|_| ())
    }
    .await;
    let collected = live.transport.shutdown().await;
    result.and(collected)?;
    if let Some(fork_point) = &fork_point {
        unstarted.fork(target_id, fork_point);
    }
    Ok(())
}

/// Wait until the CLI reports that a command reached a delivery state.
pub(crate) async fn wait_command(
    observer: &mut tokio::sync::watch::Receiver<super::wire::stdout::CommandState>,
    reached: impl Fn(super::wire::stdout::CommandState) -> bool,
    operation: ProviderOperation,
) -> Result<super::wire::stdout::CommandState, ProviderError> {
    observer
        .wait_for(|state| reached(*state))
        .await
        .map(|state| *state)
        .map_err(|_| failure(operation, ProviderFailureKind::Transport))
}

fn launch_spec(
    options: &ConversationOptions,
    start: SessionStart,
) -> Result<LaunchSpec, ProviderError> {
    let mut servers = options.mcp_servers.clone();
    if !matches!(start, SessionStart::Ephemeral) {
        servers.insert(
            super::tool_targets::HOST_TOOL_SERVER.to_owned(),
            json!({"type": "sdk", "name": super::tool_targets::HOST_TOOL_SERVER}),
        );
    }
    let config = json!({"mcpServers": servers});
    // Selected servers can carry credentials; only the built-in host server may stay inline.
    let mcp_config = match &options.plugin_dir {
        Some(profile) => {
            let path = profile.join("forge-mcp-config.json");
            std::fs::write(&path, config.to_string()).map_err(|_| {
                failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Runtime)
            })?;
            McpConfig::File(path)
        }
        None if options.mcp_servers.is_empty() => McpConfig::Inline(config),
        None => {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Rejected,
            ));
        }
    };
    Ok(LaunchSpec {
        process_owner: options.process_owner.clone(),
        workspace: options.workspace.clone(),
        start,
        model: options.model.clone().filter(|model| model != "default"),
        effort: options.effort.clone(),
        planning: options.planning,
        mcp_config,
        plugin_dir: options.plugin_dir.clone(),
    })
}

fn initialize(options: &ConversationOptions, start: &SessionStart) -> Initialize {
    let isolated = matches!(start, SessionStart::Ephemeral);
    Initialize {
        hooks: (!isolated).then(|| {
            serde_json::json!({
                "InstructionsLoaded": [{"hookCallbackIds": ["forge-instructions"]}]
            })
        }),
        sdk_mcp_servers: if isolated {
            Vec::new()
        } else {
            vec![super::tool_targets::HOST_TOOL_SERVER.to_owned()]
        },
        system_prompt: isolated.then(|| options.system_instructions.clone()),
        append_system_prompt: (!isolated && !options.system_instructions.is_empty())
            .then(|| options.system_instructions.clone()),
        // Render current host instructions on every request, including after resume.
        system_prompt_snapshot: Some(false),
        tool_aliases: Map::new(),
    }
}

/// Convert the initialize model list to the Harness catalog.
pub(crate) fn provider_models(
    initialized: &InitializeResponse,
    selected: &str,
) -> Vec<ProviderModel> {
    initialized
        .models
        .iter()
        .map(|model| ProviderModel {
            service_tiers: vec![forge_provider_api::ServiceTier::Standard],
            id: model.value.clone(),
            label: model
                .display_name
                .clone()
                .filter(|label| !label.trim().is_empty()),
            reasoning: model.supported_effort_levels.clone(),
            default_reasoning: None,
            selected_reasoning: None,
            context_window: Vec::new(),
            default_context_window: None,
            selected_context_window: None,
            vision: true,
            description: model
                .description
                .clone()
                .filter(|description| !description.trim().is_empty()),
            is_default: model.value == selected,
        })
        .collect()
}

/// Read the model list with a disposable process that writes no transcript.
pub(crate) async fn workspace_models(
    launcher: &dyn ClaudeLauncher,
    workspace: &str,
    cleanup: &crate::resource_cleanup::ResourceCleanup,
) -> Result<Vec<ProviderModel>, ProviderError> {
    let state = Arc::new(ClaudeSessionState::new(Arc::new(
        std::sync::atomic::AtomicBool::new(true),
    )));
    let options = ConversationOptions {
        process_owner: None,
        workspace: workspace.to_owned(),
        system_instructions: String::new(),
        mcp_servers: Map::new(),
        plugin_dir: None,
        model: None,
        effort: None,
        planning: false,
    };
    let live = launch(launcher, &state, &options, SessionStart::Ephemeral, cleanup).await;
    let result = live.map(|live| {
        let models = provider_models(&live.initialized, "default");
        (models, live.transport)
    });
    match result {
        Ok((models, transport)) => {
            transport.shutdown().await?;
            Ok(models)
        }
        Err(error) => Err(error),
    }
}

/// Normalize a transport failure at its requesting operation.
pub(crate) fn transport_error(
    operation: ProviderOperation,
    error: TransportFailure,
) -> ProviderError {
    tracing::error!(target: "forge::provider", event_name = "provider.failure",
        provider = "claude", operation = ?operation, error = %error);
    let (kind, disposition) = match error {
        TransportFailure::Rejected { code, .. } => (
            match code.as_deref() {
                Some("not_found") => ProviderFailureKind::SessionMissing,
                Some("restricted_by_org") => ProviderFailureKind::Policy,
                _ => ProviderFailureKind::Rejected,
            },
            FailureDisposition::Unchanged,
        ),
        TransportFailure::Decode(_) => {
            (ProviderFailureKind::Protocol, FailureDisposition::Uncertain)
        }
        TransportFailure::Closed | TransportFailure::Write => (
            ProviderFailureKind::Transport,
            FailureDisposition::Uncertain,
        ),
    };
    ProviderError::Failure(
        ProviderFailure::new(operation, kind).at_phase(ProviderPhase::Observation, disposition),
    )
}

fn initialization_failure(error: TransportFailure) -> ProviderError {
    tracing::error!(target: "forge::provider", event_name = "provider.startup.failed",
        provider = "claude", error = %error);
    let kind = match error {
        TransportFailure::Rejected { .. } => ProviderFailureKind::Initialization,
        TransportFailure::Closed | TransportFailure::Write => ProviderFailureKind::Initialization,
        TransportFailure::Decode(_) => ProviderFailureKind::Protocol,
    };
    ProviderError::Failure(
        ProviderFailure::new(ProviderOperation::ThreadOpen, kind)
            .at_phase(ProviderPhase::Preparation, FailureDisposition::Unchanged),
    )
}

pub(crate) fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Encode Harness input text and image attachments as user content blocks.
pub(crate) fn input_blocks(
    texts: Vec<String>,
    attachments: &[forge_provider_api::input::FileAttachment],
) -> Vec<InputBlock> {
    use base64::Engine;
    let mut blocks = Vec::new();
    for attachment in attachments {
        blocks.push(InputBlock::Text {
            text: attachment.context.clone(),
        });
        if attachment.media_type.starts_with("image/") {
            blocks.push(InputBlock::Image {
                source: super::wire::content::ImageSource {
                    kind: "base64",
                    media_type: attachment.media_type.clone(),
                    data: base64::engine::general_purpose::STANDARD.encode(&attachment.contents),
                },
            });
        }
    }
    blocks.extend(
        texts
            .into_iter()
            .filter(|text| !text.is_empty())
            .map(|text| InputBlock::Text { text }),
    );
    blocks
}
