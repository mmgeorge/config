//! Keep native shell tasks outside the model session's autonomous notification queue.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Weak};
use std::time::Duration;

use forge_provider_api::terminal::{BackgroundTerminal, TerminalId, TerminalInventory};
use forge_provider_api::{
    ProviderError, ProviderEventSink, ProviderFailureKind, ProviderOperation, ThreadId,
};
use futures_util::FutureExt;
use futures_util::future::{BoxFuture, Shared};
use github_copilot_sdk::{SessionConfig, SessionId};
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use tokio::sync::{Mutex, watch};

use super::handle::CopilotClient;
use super::provider::{NativeCallbackRouter, failure};

const OPEN_TIMEOUT: Duration = Duration::from_secs(30);

type StartupResult = Shared<BoxFuture<'static, Result<Arc<PausedShell>, ProviderError>>>;
type CallResult = Shared<BoxFuture<'static, Result<NativeToolResult, ProviderError>>>;
/// Retain one helper collection independently of the public close awaiter.
pub(super) type CloseResult = Shared<BoxFuture<'static, Result<(), ProviderError>>>;

#[derive(Clone)]
struct Startup {
    result: StartupResult,
    id: SessionId,
    client: CopilotClient,
    deadline: tokio::time::Instant,
    workspace: PathBuf,
}

#[derive(Clone)]
struct Preparation {
    client: CopilotClient,
    workspace: PathBuf,
}

enum Admission {
    Open,
    Closed,
}

#[derive(Default)]
enum CollectionPhase {
    #[default]
    Pending,
    Complete,
}

#[derive(Default)]
struct CleanupProgress {
    tasks: CollectionPhase,
    reader: CollectionPhase,
    disconnect: CollectionPhase,
    workers: CollectionPhase,
}

#[derive(Clone)]
struct RetainedCall {
    id: uuid::Uuid,
    result: CallResult,
    abort: tokio::task::AbortHandle,
    tool: super::shell_tools::ShellTool,
    launch: watch::Receiver<LaunchDisposition>,
}

#[derive(Clone, Copy, PartialEq)]
enum LaunchDisposition {
    NotDispatched,
    InFlight,
    Acknowledged,
    Uncertain,
}

enum HistoryCleanup {
    Pending,
    Deleting(Arc<super::runtime::CopilotRuntime>),
    Stopping {
        runtime: Arc<super::runtime::CopilotRuntime>,
        deletion: Result<(), ProviderError>,
    },
    Complete,
}

struct State {
    admission: Admission,
    preparation: Option<Preparation>,
    startup: Option<Startup>,
    calls: Vec<RetainedCall>,
    helper_id: Option<SessionId>,
    cleanup: Option<CloseResult>,
}

/// Bind every issued shell handle to its immutable auxiliary session generation.
#[derive(Clone)]
struct ShellTarget {
    session: SessionId,
    native_id: String,
    launch_call: String,
    output_epoch: u64,
    read_epoch: u64,
}

/// Retain a binding's native shells independently of model turns and configuration replacement.
pub(super) struct ShellSession {
    state: Mutex<State>,
    callbacks: Weak<dyn NativeCallbackRouter>,
    publisher: Arc<super::terminal::TerminalPublisher>,
    publication: super::terminal::TerminalSourceHandle,
    handles: Mutex<HashMap<TerminalId, ShellTarget>>,
    interruption: watch::Sender<u64>,
    cancellation: watch::Sender<()>,
    cleanup_factory: std::sync::OnceLock<Arc<super::runtime::CopilotRuntime>>,
    history_cleanup: Mutex<HistoryCleanup>,
}

/// Expose only tool execution on a session whose model queue remains permanently paused.
struct PausedShell {
    session: Arc<github_copilot_sdk::session::Session>,
    client: CopilotClient,
    reader: Mutex<Option<tokio::task::JoinHandle<()>>>,
    shutdown: watch::Sender<bool>,
    inventory: watch::Sender<TerminalInventory>,
    activity: watch::Sender<u64>,
    admitted: Result<(), ProviderError>,
    cleanup: Mutex<CleanupProgress>,
    refreshing: Mutex<()>,
    workspace: PathBuf,
}

/// Avoid a binding -> helper -> SDK callbacks -> binding ownership cycle.
struct HelperCallbacks {
    target: Weak<dyn NativeCallbackRouter>,
}

#[async_trait::async_trait]
impl NativeCallbackRouter for HelperCallbacks {
    /// Retain the binding only while servicing this one callback.
    async fn call_native(
        &self,
        call_id: &str,
        call: forge_provider_api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        self.target
            .upgrade()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                )
            })?
            .call_native(call_id, call)
            .await
    }

    /// Resolve native permission against the original router while the request is live.
    async fn authorize_native(
        &self,
        call_id: &str,
        request: forge_provider_api::permissions::PermissionRequest,
    ) -> forge_provider_api::permissions::PermissionResponse {
        match self.target.upgrade() {
            Some(target) => target.authorize_native(call_id, request).await,
            None => forge_provider_api::permissions::PermissionResponse::Cancelled,
        }
    }

    /// Reject native pre-tool work once its binding owner has been collected.
    async fn before_tool_native(&self, call_id: Option<&str>) -> Result<(), ProviderError> {
        self.target
            .upgrade()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                )
            })?
            .before_tool_native(call_id)
            .await
    }

    /// Preserve the exact-call cancellation guard for helper permissions.
    async fn before_shell_native(&self, call_id: &str) -> Result<(), ProviderError> {
        self.target
            .upgrade()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                )
            })?
            .before_shell_native(call_id)
            .await
    }

    /// Forward observed identities without extending the router's idle lifetime.
    async fn bind_native_call(
        &self,
        call_id: &str,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        match self.target.upgrade() {
            Some(target) => target.bind_native_call(call_id, scope).await,
            None => Err(ProviderError::Cancelled),
        }
    }

    async fn release_native_call(&self, call_id: &str) {
        if let Some(target) = self.target.upgrade() {
            target.release_native_call(call_id).await;
        }
    }
}

#[derive(Clone, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
enum NativeOutcome {
    Success,
    Failure,
}

/// Retain actual command output separately from its native success/failure classification.
#[derive(Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(super) struct NativeToolResult {
    text_result_for_llm: String,
    result_type: NativeOutcome,
}

impl NativeToolResult {
    /// Preserve native command failures as failures in the model-facing tool response.
    pub(super) fn into_sdk(self) -> github_copilot_sdk::ToolResult {
        let kind = match self.result_type {
            NativeOutcome::Success => "success",
            NativeOutcome::Failure => "failure",
        };
        github_copilot_sdk::ToolResult::Expanded(github_copilot_sdk::ToolResultExpanded::new(
            self.text_result_for_llm,
            kind,
        ))
    }
}

impl ShellSession {
    /// Create a dormant owner before the model session can expose shell tools.
    pub(super) fn new(
        callbacks: Weak<dyn NativeCallbackRouter>,
        owner: ThreadId,
        sink: ProviderEventSink,
    ) -> Self {
        let (interruption, _) = watch::channel(0);
        let (cancellation, _) = watch::channel(());
        let publisher = super::terminal::TerminalPublisher::new(owner, sink);
        let publication = publisher.source(super::terminal::TerminalSource::Helper);
        Self {
            state: Mutex::new(State {
                admission: Admission::Open,
                preparation: None,
                startup: None,
                calls: Vec::new(),
                helper_id: None,
                cleanup: None,
            }),
            callbacks,
            publisher,
            publication,
            handles: Mutex::new(HashMap::new()),
            interruption,
            cancellation,
            cleanup_factory: std::sync::OnceLock::new(),
            history_cleanup: Mutex::new(HistoryCleanup::Pending),
        }
    }

    /// Share the binding's combined terminal publisher with its main native reader.
    pub(super) fn publisher(&self) -> Arc<super::terminal::TerminalPublisher> {
        self.publisher.clone()
    }

    /// Retain the transport without allocating a helper until the first shell operation.
    pub(super) async fn prepare(
        &self,
        client: CopilotClient,
        workspace: PathBuf,
        main_session_id: &str,
    ) -> Result<(), ProviderError> {
        let mut state = self.state.lock().await;
        if matches!(state.admission, Admission::Closed) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let id = helper_identity(main_session_id);
        if state.startup.is_none() {
            state.helper_id = Some(id);
        } else if state.helper_id.as_ref() != Some(&id) {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        state.preparation = Some(Preparation { client, workspace });
        Ok(())
    }

    /// Retain a dormant independent transport factory for history cleanup after native process loss.
    pub(super) fn set_cleanup_runtime(&self, runtime: super::runtime::CopilotRuntime) {
        let _ = self.cleanup_factory.set(Arc::new(runtime));
    }

    /// Release foreground waits when steering or cancellation gives the model another instruction.
    pub(super) fn interrupt_waits(&self) {
        self.interruption
            .send_modify(|epoch| *epoch = epoch.wrapping_add(1));
    }

    /// Revoke a pending native permission independently of its host response future.
    pub(super) fn cancel_waits(&self) {
        self.interrupt_waits();
        self.cancellation.send_replace(());
    }

    /// Observe cancellation without lending a callback authority from another invocation.
    pub(super) fn cancellation(&self) -> watch::Receiver<()> {
        self.cancellation.subscribe()
    }

    /// Identify a private history already tied to the current main native identity.
    pub(super) async fn started(&self) -> bool {
        self.state.lock().await.startup.is_some()
    }

    /// Read the last observed native task inventory without creating a helper.
    pub(super) async fn snapshot(&self) -> Result<TerminalInventory, ProviderError> {
        let startup = self.state.lock().await.startup.clone();
        let Some(startup) = startup else {
            return Ok(TerminalInventory::observed(Vec::new()));
        };
        let shell = tokio::time::timeout_at(startup.deadline, startup.result)
            .await
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Timeout))??;
        shell.admitted.clone()?;
        let snapshot = shell.inventory.borrow().clone();
        Ok(snapshot)
    }

    /// Stop only a terminal handle issued by this binding's auxiliary generation.
    pub(super) async fn terminate(self: &Arc<Self>, id: &TerminalId) -> Result<(), ProviderError> {
        let shell = self.ready().await?;
        let target = self
            .target(&shell, id, ProviderOperation::TerminalTerminate)
            .await?;
        if !shell
            .inventory
            .borrow()
            .terminal
            .iter()
            .any(|terminal| &terminal.id == id)
        {
            return Err(failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            ));
        }
        shell
            .session
            .rpc()
            .tasks()
            .cancel(github_copilot_sdk::rpc::TasksCancelRequest {
                id: target.native_id,
            })
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::TerminalTerminate,
                    ProviderFailureKind::Runtime,
                )
            })?;
        self.refresh(&shell).await?;
        Ok(())
    }

    /// Check only this helper's issued handles before resolving a terminal operation.
    pub(super) async fn owns(&self, id: &TerminalId) -> bool {
        self.handles.lock().await.contains_key(id)
    }

    /// Revoke allocation synchronously with its state, then collect retained startup and native tasks.
    #[cfg(test)]
    pub(super) async fn close(self: &Arc<Self>) -> Result<(), ProviderError> {
        self.start_close().await.await
    }

    /// Start retained helper cleanup before a caller begins independent main-session collection.
    pub(super) async fn start_close(self: &Arc<Self>) -> CloseResult {
        self.cancel_waits();
        let mut state = self.state.lock().await;
        state.admission = Admission::Closed;
        if let Some(cleanup) = &state.cleanup
            && !matches!(cleanup.peek(), Some(Err(_)))
        {
            return cleanup.clone();
        }
        let owner = self.clone();
        let task = tokio::spawn(async move { owner.close_owned().await });
        let cleanup = async move {
            task.await.map_err(|_| {
                failure(ProviderOperation::ThreadClose, ProviderFailureKind::Runtime)
            })?
        }
        .boxed()
        .shared();
        state.cleanup = Some(cleanup.clone());
        cleanup
    }

    /// Collect each independent resource phase without abandoning siblings after one failure.
    async fn close_owned(&self) -> Result<(), ProviderError> {
        self.cancel_waits();
        let (startup, mut calls) = {
            let mut state = self.state.lock().await;
            state.admission = Admission::Closed;
            (state.startup.clone(), state.calls.clone())
        };
        let Some(startup) = startup else {
            for call in calls {
                let _ = call.result.await;
            }
            return Ok(());
        };
        let shell = match startup.result.await {
            Ok(shell) => shell,
            Err(_) => {
                for call in calls {
                    let _ = call.result.await;
                }
                self.delete_history(&startup.client, &startup.id, &startup.workspace)
                    .await?;
                return Ok(());
            }
        };
        shell.shutdown.send_replace(true);
        // Keep independent completion proofs; history deletion is never a substitute for teardown.
        let mut cleanup = shell.cleanup.lock().await;
        let mut failures = Vec::new();
        let pending_launch = calls.iter().any(|call| {
            call.tool.is_command()
                && matches!(
                    *call.launch.borrow(),
                    LaunchDisposition::InFlight | LaunchDisposition::Uncertain
                )
        });
        if matches!(cleanup.tasks, CollectionPhase::Pending) {
            if shell.client.generation.is_collected() {
                cleanup.tasks = CollectionPhase::Complete;
            } else {
                let mut task_failures = Vec::new();
                match native_tasks(&shell).await {
                    Ok(tasks) => {
                        for terminal in tasks {
                            if shell
                                .session
                                .rpc()
                                .tasks()
                                .cancel(github_copilot_sdk::rpc::TasksCancelRequest {
                                    id: terminal.id.as_str().to_owned(),
                                })
                                .await
                                .is_err()
                            {
                                task_failures.push(failure(
                                    ProviderOperation::ThreadClose,
                                    ProviderFailureKind::Runtime,
                                ));
                            }
                        }
                    }
                    Err(error) => task_failures.push(error),
                }
                match native_tasks(&shell).await {
                    Ok(tasks) if tasks.is_empty() && !pending_launch => {
                        cleanup.tasks = CollectionPhase::Complete;
                        failures.extend(task_failures);
                    }
                    Ok(tasks) if tasks.is_empty() => {
                        failures.extend(task_failures);
                    }
                    Ok(_) => {
                        failures.extend(task_failures);
                        failures.push(failure(
                            ProviderOperation::ThreadClose,
                            ProviderFailureKind::ResourceConflict,
                        ));
                    }
                    Err(error) => {
                        failures.extend(task_failures);
                        failures.push(error);
                    }
                }
            }
        }
        if matches!(cleanup.reader, CollectionPhase::Pending) {
            let mut reader = shell.reader.lock().await;
            if let Some(reader) = reader.as_mut()
                && reader.await.is_err()
            {
                failures.push(failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::Runtime,
                ));
            }
            reader.take();
            cleanup.reader = CollectionPhase::Complete;
        }
        if pending_launch && !shell.client.generation.is_collected() {
            for call in calls.iter_mut().filter(|call| call.tool.is_command()) {
                loop {
                    let disposition = *call.launch.borrow_and_update();
                    match disposition {
                        LaunchDisposition::NotDispatched | LaunchDisposition::Acknowledged => break,
                        LaunchDisposition::Uncertain => {
                            failures.push(ProviderError::Observation(
                                forge_provider_api::ObservationFailure::DeliveryUncertain,
                            ));
                            break;
                        }
                        LaunchDisposition::InFlight => {
                            if call.launch.changed().await.is_err() {
                                failures.push(ProviderError::Observation(
                                    forge_provider_api::ObservationFailure::DeliveryUncertain,
                                ));
                                break;
                            }
                        }
                    }
                }
            }
            if shell.client.generation.is_collected() {
                cleanup.tasks = CollectionPhase::Complete;
            } else if calls.iter().all(|call| {
                !call.tool.is_command()
                    || matches!(
                        *call.launch.borrow(),
                        LaunchDisposition::NotDispatched | LaunchDisposition::Acknowledged
                    )
            }) {
                match collect_native_tasks(&shell).await {
                    Ok(()) => cleanup.tasks = CollectionPhase::Complete,
                    Err(error) => failures.push(error),
                }
            }
        }
        if matches!(cleanup.disconnect, CollectionPhase::Pending)
            && matches!(cleanup.tasks, CollectionPhase::Complete)
        {
            if shell.client.generation.is_collected() {
                shell.session.stop_event_loop().await;
                cleanup.disconnect = CollectionPhase::Complete;
            } else {
                match shell.session.disconnect().await {
                    Ok(()) => cleanup.disconnect = CollectionPhase::Complete,
                    Err(_) => failures.push(failure(
                        ProviderOperation::ThreadClose,
                        ProviderFailureKind::Transport,
                    )),
                }
            }
        }
        if matches!(cleanup.tasks, CollectionPhase::Complete)
            && matches!(cleanup.disconnect, CollectionPhase::Complete)
        {
            // Only detach and positive task collection make a withheld native reply safe to abandon.
            for call in calls {
                if call.result.peek().is_none() {
                    call.abort.abort();
                }
                let _ = call.result.await;
            }
            self.state.lock().await.calls.clear();
            cleanup.workers = CollectionPhase::Complete;
        }
        if matches!(cleanup.tasks, CollectionPhase::Complete)
            && matches!(cleanup.reader, CollectionPhase::Complete)
            && matches!(cleanup.disconnect, CollectionPhase::Complete)
            && matches!(cleanup.workers, CollectionPhase::Complete)
        {
            if let Err(error) = self
                .delete_history(&shell.client, shell.session.id(), &shell.workspace)
                .await
            {
                failures.push(error);
            }
        } else if failures.is_empty() {
            failures.push(failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::ResourceConflict,
            ));
        }
        match failures.len() {
            0 => Ok(()),
            1 => Err(failures.remove(0)),
            _ => Err(ProviderError::Combined {
                primary: Box::new(failures.remove(0)),
                secondary: failures,
            }),
        }
    }

    /// Delete only this helper's history, collecting any independent cleanup process on every outcome.
    async fn delete_history(
        &self,
        original: &CopilotClient,
        id: &SessionId,
        workspace: &std::path::Path,
    ) -> Result<(), ProviderError> {
        let mut state = self.history_cleanup.lock().await;
        if matches!(*state, HistoryCleanup::Complete) {
            return Ok(());
        }
        if !original.generation.is_collected() {
            delete_native_history(&original.inner, id).await?;
            *state = HistoryCleanup::Complete;
            return Ok(());
        }
        if matches!(*state, HistoryCleanup::Pending) {
            let factory = self.cleanup_factory.get().ok_or_else(|| {
                failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::Transport,
                )
            })?;
            *state = HistoryCleanup::Deleting(Arc::new(factory.cleanup_runtime()));
        }
        if let HistoryCleanup::Deleting(runtime) = &*state {
            let runtime = runtime.clone();
            let deletion = match runtime.client(&workspace.to_string_lossy()).await {
                Ok(client) => delete_native_history(&client.inner, id).await,
                Err(error) => Err(error),
            };
            *state = HistoryCleanup::Stopping { runtime, deletion };
        }
        if let HistoryCleanup::Stopping { runtime, deletion } = &*state {
            let deletion = deletion.clone();
            if let Err(collection) = runtime.shutdown().await {
                return Err(match deletion {
                    Ok(()) => collection,
                    Err(deletion) => ProviderError::Combined {
                        primary: Box::new(deletion),
                        secondary: vec![collection],
                    },
                });
            }
            if deletion.is_ok() {
                *state = HistoryCleanup::Complete;
            } else {
                *state = HistoryCleanup::Pending;
            }
            return deletion;
        }
        Ok(())
    }

    /// Route a shell tool through native permissions while keeping model notification work paused.
    pub(super) async fn execute(
        self: &Arc<Self>,
        call_id: &str,
        tool: super::shell_tools::ShellTool,
        arguments: Value,
    ) -> Result<NativeToolResult, ProviderError> {
        tool.validate(&arguments)?;
        let call = {
            let mut state = self.state.lock().await;
            if matches!(state.admission, Admission::Closed) {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if state.calls.iter().any(|call| {
                let disposition = *call.launch.borrow();
                disposition == LaunchDisposition::Uncertain
                    || (disposition == LaunchDisposition::InFlight
                        && call.launch.has_changed().is_err())
            }) {
                return Err(ProviderError::Observation(
                    forge_provider_api::ObservationFailure::DeliveryUncertain,
                ));
            }
            let owner = self.clone();
            let call_id = call_id.to_owned();
            let id = uuid::Uuid::new_v4();
            let (launch, disposition) = watch::channel(LaunchDisposition::NotDispatched);
            let task = tokio::spawn(async move {
                let result = owner
                    .execute_owned(&call_id, tool, arguments, &launch)
                    .await;
                if *launch.borrow() != LaunchDisposition::Uncertain {
                    owner
                        .state
                        .lock()
                        .await
                        .calls
                        .retain(|current| current.id != id);
                }
                result
            });
            let abort = task.abort_handle();
            let call = async move {
                task.await.map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime)
                })?
            }
            .boxed()
            .shared();
            state.calls.push(RetainedCall {
                id,
                result: call.clone(),
                abort,
                tool,
                launch: disposition,
            });
            call
        };
        call.await
    }

    /// Retain native handoff and its response independently of the SDK callback awaiter's lifetime.
    async fn execute_owned(
        self: &Arc<Self>,
        call_id: &str,
        tool: super::shell_tools::ShellTool,
        mut arguments: Value,
        launch: &watch::Sender<LaunchDisposition>,
    ) -> Result<NativeToolResult, ProviderError> {
        let callbacks = self.callbacks.upgrade().ok_or_else(|| {
            failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            )
        })?;
        callbacks.before_shell_native(call_id).await?;
        let mut interrupted = self.interruption.subscribe();
        let shell = self.ready().await?;
        callbacks.before_shell_native(call_id).await?;
        let name = tool.name();
        let command = tool.is_command();
        let mut wait = Duration::ZERO;
        if command {
            if arguments.get("mode").and_then(Value::as_str) != Some("async") {
                let seconds = arguments
                    .get("initial_wait")
                    .and_then(Value::as_f64)
                    .unwrap_or(30.0);
                wait = Duration::try_from_secs_f64(seconds).map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
                })?;
            }
            // Initial waiting is owned by this invocation, never by the native
            // asynchronous launch (whose parser rejects a zero initial_wait).
            arguments
                .as_object_mut()
                .expect("validated shell arguments are objects")
                .remove("initial_wait");
            arguments["mode"] = json!("async");
            let id = TerminalId::new(
                arguments
                    .get("shellId")
                    .and_then(Value::as_str)
                    .map(str::to_owned)
                    .unwrap_or_else(|| uuid::Uuid::new_v4().to_string()),
            )?;
            arguments["shellId"] = json!(id.as_str());
            {
                let mut handles = self.handles.lock().await;
                if handles.contains_key(&id) {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::OperationConflict,
                    ));
                }
                handles.insert(
                    id.clone(),
                    ShellTarget {
                        session: shell.session.id().clone(),
                        native_id: id.as_str().to_owned(),
                        launch_call: call_id.to_owned(),
                        output_epoch: 0,
                        read_epoch: 0,
                    },
                );
            }
        } else if let Some(id) = arguments.get("shellId").and_then(Value::as_str) {
            let id = TerminalId::new(id)?;
            let target = self
                .target(&shell, &id, ProviderOperation::TurnStart)
                .await?;
            if tool.is_read() {
                let delay = arguments
                    .get("delay")
                    .and_then(Value::as_f64)
                    .unwrap_or(0.0);
                let delay = Duration::try_from_secs_f64(delay).map_err(|_| {
                    failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
                })?;
                if target.output_epoch == target.read_epoch && !delay.is_zero() {
                    let mut activity = shell.activity.subscribe();
                    let active = shell
                        .inventory
                        .borrow()
                        .terminal
                        .iter()
                        .any(|terminal| terminal.id == id);
                    if active {
                        tokio::select! {
                            _ = activity.changed() => {},
                            _ = tokio::time::sleep(delay) => {},
                            _ = interrupted.changed() => {},
                        }
                    }
                }
                arguments["delay"] = json!(0);
            }
            arguments["shellId"] = json!(target.native_id);
        }
        if let Err(error) = callbacks.before_shell_native(call_id).await {
            if command && let Some(id) = arguments.get("shellId").and_then(Value::as_str) {
                self.handles.lock().await.remove(&TerminalId::new(id)?);
            }
            return Err(error);
        }
        let result = self
            .invoke(
                &shell,
                call_id,
                name,
                arguments.clone(),
                command.then_some(launch),
            )
            .await?;
        self.refresh(&shell).await?;
        if command && !wait.is_zero() && result.result_type == NativeOutcome::Success {
            let id = arguments
                .get("shellId")
                .and_then(Value::as_str)
                .expect("shell launch installs its opaque handle");
            let mut inventory = shell.inventory.subscribe();
            let complete = async {
                loop {
                    if !inventory
                        .borrow()
                        .terminal
                        .iter()
                        .any(|task| task.id.as_str() == id)
                    {
                        break;
                    }
                    if inventory.changed().await.is_err() {
                        break;
                    }
                }
            };
            tokio::select! {
                _ = complete => {},
                _ = tokio::time::sleep(wait) => {},
                _ = interrupted.changed() => {},
            }
            callbacks.before_shell_native(call_id).await?;
            let result = self
                .invoke(
                    &shell,
                    call_id,
                    tool.read_name(),
                    json!({"shellId":id,"delay":0}),
                    None,
                )
                .await?;
            self.mark_read(id).await;
            return Ok(result);
        }
        if tool.is_read()
            && let Some(id) = arguments.get("shellId").and_then(Value::as_str)
        {
            self.mark_read(id).await;
        }
        Ok(result)
    }

    /// Record which native output prefix the explicit read consumed without retaining its text.
    async fn mark_read(&self, native_id: &str) {
        for target in self.handles.lock().await.values_mut() {
            if target.native_id == native_id {
                target.read_epoch = target.output_epoch;
            }
        }
    }

    /// Get or start one retained helper allocation under the binding's admission fence.
    async fn ready(self: &Arc<Self>) -> Result<Arc<PausedShell>, ProviderError> {
        let startup = {
            let mut state = self.state.lock().await;
            if matches!(state.admission, Admission::Closed) {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if let Some(startup) = &state.startup {
                startup.clone()
            } else {
                let preparation = state.preparation.clone().ok_or_else(|| {
                    failure(
                        ProviderOperation::ThreadOpen,
                        ProviderFailureKind::Initialization,
                    )
                })?;
                let owner = Arc::downgrade(self);
                let callbacks = self.callbacks.clone();
                let cancellation = self.cancellation.subscribe();
                let id = state
                    .helper_id
                    .clone()
                    .expect("prepared shell owns its deterministic private history identity");
                let client = preparation.client.clone();
                let workspace = preparation.workspace.clone();
                let native_id = id.clone();
                let task = tokio::spawn(async move {
                    open(preparation, native_id, owner, callbacks, cancellation).await
                });
                let result = async move {
                    task.await.map_err(|_| {
                        failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Runtime)
                    })?
                }
                .boxed()
                .shared();
                let startup = Startup {
                    result,
                    id,
                    client,
                    workspace,
                    deadline: tokio::time::Instant::now() + OPEN_TIMEOUT,
                };
                state.startup = Some(startup.clone());
                startup
            }
        };
        let shell = tokio::time::timeout_at(startup.deadline, startup.result)
            .await
            .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Timeout))??;
        shell.admitted.clone()?;
        if matches!(self.state.lock().await.admission, Admission::Closed) {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            ));
        }
        shell.client.ensure_healthy()?;
        Ok(shell)
    }

    /// Resolve an issued handle without allowing native task IDs from another helper generation.
    async fn target(
        &self,
        shell: &PausedShell,
        id: &TerminalId,
        operation: ProviderOperation,
    ) -> Result<ShellTarget, ProviderError> {
        self.handles
            .lock()
            .await
            .get(id)
            .filter(|target| target.session == shell.session.id())
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::Rejected))
    }

    /// Invoke only the native shell tool set, preserving its structured permission pipeline.
    async fn invoke(
        &self,
        shell: &PausedShell,
        call_id: &str,
        name: &str,
        arguments: Value,
        launch: Option<&watch::Sender<LaunchDisposition>>,
    ) -> Result<NativeToolResult, ProviderError> {
        shell.client.ensure_healthy()?;
        {
            let state = self.state.lock().await;
            if matches!(state.admission, Admission::Closed) {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            if let Some(launch) = launch {
                launch.send_replace(LaunchDisposition::InFlight);
            }
        }
        let result = async {
            let value = shell.client.inner.call("session.tools.execute", Some(json!({"sessionId":shell.session.id(),"name":name,"arguments":arguments,"toolCallId":call_id}))).await
                .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Transport))?;
            serde_json::from_value(value).map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
        }.await;
        if let Some(launch) = launch {
            launch.send_replace(if result.is_ok() {
                LaunchDisposition::Acknowledged
            } else {
                LaunchDisposition::Uncertain
            });
        }
        result
    }

    /// Publish binding-owned task changes without admitting a model execution.
    async fn refresh(&self, shell: &PausedShell) -> Result<(), ProviderError> {
        let _refreshing = shell.refreshing.lock().await;
        let mut shutdown = shell.shutdown.subscribe();
        if *shutdown.borrow() {
            return Ok(());
        }
        let tasks_api = shell.session.rpc().tasks();
        let tasks = tokio::select! {
            _ = shutdown.changed() => return Ok(()),
            result = tasks_api.list() => result.map_err(|_| failure(ProviderOperation::TerminalList, ProviderFailureKind::Transport))?,
        };
        let native = crate::terminal::parse_snapshot(
            &serde_json::to_value(tasks).map_err(|_| {
                failure(
                    ProviderOperation::TerminalList,
                    ProviderFailureKind::Protocol,
                )
            })?,
            true,
        )
        .map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Protocol,
            )
        })?;
        let handles = self.handles.lock().await;
        let terminal = native
            .terminal
            .into_iter()
            .filter_map(|task| {
                handles
                    .iter()
                    .find(|(_, target)| {
                        target.session == *shell.session.id()
                            && target.native_id == task.id.as_str()
                    })
                    .map(|(id, _)| BackgroundTerminal {
                        id: id.clone(),
                        command: task.command,
                    })
            })
            .collect();
        drop(handles);
        let snapshot = TerminalInventory::observed(terminal);
        if *shell.inventory.borrow() != snapshot {
            shell.inventory.send_replace(snapshot.clone());
            tokio::select! {
                _ = shutdown.changed() => {},
                result = self.publication.publish(snapshot) => { result?; }
            }
        }
        Ok(())
    }
}

/// Allocate private history and acquire its permanent queue pause before exposing execution.
async fn open(
    preparation: Preparation,
    id: SessionId,
    owner: Weak<ShellSession>,
    callbacks: Weak<dyn NativeCallbackRouter>,
    cancellation: watch::Receiver<()>,
) -> Result<Arc<PausedShell>, ProviderError> {
    if callbacks.upgrade().is_none() {
        return Err(failure(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::BindingClosed,
        ));
    }
    let callbacks: Arc<dyn NativeCallbackRouter> = Arc::new(HelperCallbacks { target: callbacks });
    // Main-history exclusivity is established before a tool can reach this lazy allocation.
    // A fresh runtime may collect only this deterministic, adapter-owned stale helper history.
    if let Err(error) = preparation.client.inner.delete_session(&id).await
        && !matches!(
            error.kind(),
            github_copilot_sdk::ErrorKind::Session(github_copilot_sdk::SessionErrorKind::NotFound(
                _
            ))
        )
    {
        return Err(failure(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Transport,
        ));
    }
    let mut config = SessionConfig::default();
    let inherited_mcp = preparation
        .client
        .discover_mcp(&preparation.workspace.to_string_lossy())
        .await?;
    config.session_id = Some(id.clone());
    config.working_directory = Some(preparation.workspace.clone());
    let family = if cfg!(windows) {
        [
            "powershell",
            "read_powershell",
            "stop_powershell",
            "list_powershell",
        ]
    } else {
        ["bash", "read_bash", "stop_bash", "list_bash"]
    };
    config.available_tools = Some(family.into_iter().map(str::to_owned).collect());
    config.enable_config_discovery = Some(false);
    config.enable_skills = Some(false);
    // Pin the tools-only skill selection. Runtime 1.0.103 materializes the native
    // offered tool set when this post-create option is applied in CLI mode.
    config.included_builtin_skills = Some(Vec::new());
    config.enable_file_hooks = Some(false);
    config.skip_custom_instructions = Some(true);
    config.mcp_servers = Some(Default::default());
    config.disabled_mcp_servers = Some(
        inherited_mcp
            .into_iter()
            .map(|server| server.name)
            .collect(),
    );
    config.autopilot = Some(github_copilot_sdk::types::AutopilotOptions {
        continuation_enabled: Some(false),
        max_continues: Some(0),
    });
    config.event_buffer_capacity = Some(forge_protocol::MAX_PENDING_FRAMES);
    let config = config
        .with_permission_handler(super::permission::shell_handler(
            callbacks.clone(),
            cancellation,
        ))
        .with_hooks(super::permission::checkpoint_hooks(callbacks));
    let session = Arc::new(
        preparation
            .client
            .inner
            .create_session(config)
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadOpen,
                    ProviderFailureKind::Initialization,
                )
            })?,
    );
    let admitted = preparation
        .client
        .inner
        .call(
            "session.queue.setDrainPaused",
            Some(json!({"sessionId":id,"paused":true})),
        )
        .await
        .map(|_| ())
        .map_err(|_| failure(ProviderOperation::ThreadOpen, ProviderFailureKind::Protocol));
    let mut subscription = session.subscribe();
    let (shutdown, mut stopped) = watch::channel(false);
    let (inventory, _) = watch::channel(TerminalInventory::observed(Vec::new()));
    let (activity, _) = watch::channel(0);
    let shell = Arc::new(PausedShell {
        session,
        client: preparation.client,
        reader: Mutex::new(None),
        shutdown,
        inventory,
        activity,
        admitted,
        cleanup: Mutex::new(CleanupProgress::default()),
        refreshing: Mutex::new(()),
        workspace: preparation.workspace,
    });
    let weak_shell = Arc::downgrade(&shell);
    let reader = tokio::spawn(async move {
        loop {
            let event = tokio::select! { _ = stopped.changed() => break, event = subscription.recv() => event };
            let Some(shell) = weak_shell.upgrade() else {
                break;
            };
            let Ok(event) = event else {
                shell
                    .client
                    .generation
                    .fail(forge_provider_api::ObservationFailure::Disconnected);
                break;
            };
            if event.event_type == "assistant.turn_start" {
                shell
                    .client
                    .generation
                    .fail(forge_provider_api::ObservationFailure::UnexpectedExecution);
                break;
            }
            if event.event_type == "session.background_tasks_changed" {
                let Some(owner) = owner.upgrade() else {
                    break;
                };
                if owner.refresh(&shell).await.is_err() {
                    shell
                        .client
                        .generation
                        .fail(forge_provider_api::ObservationFailure::Protocol);
                    break;
                }
                shell
                    .activity
                    .send_modify(|epoch| *epoch = epoch.wrapping_add(1));
            }
            if event.event_type == "tool.shell_output" {
                let Some(owner) = owner.upgrade() else {
                    break;
                };
                if let Some(call) = event.data.get("toolCallId").and_then(Value::as_str) {
                    for target in owner.handles.lock().await.values_mut() {
                        if target.launch_call == call {
                            target.output_epoch = target.output_epoch.wrapping_add(1);
                        }
                    }
                    shell
                        .activity
                        .send_modify(|epoch| *epoch = epoch.wrapping_add(1));
                }
            }
        }
    });
    *shell.reader.lock().await = Some(reader);
    Ok(shell)
}

/// Read authoritative live native tasks for collection without relying on a presentation cache.
async fn native_tasks(shell: &PausedShell) -> Result<Vec<BackgroundTerminal>, ProviderError> {
    let tasks = shell.session.rpc().tasks().list().await.map_err(|_| {
        failure(
            ProviderOperation::ThreadClose,
            ProviderFailureKind::Transport,
        )
    })?;
    let tasks = serde_json::to_value(tasks).map_err(|_| {
        failure(
            ProviderOperation::ThreadClose,
            ProviderFailureKind::Protocol,
        )
    })?;
    crate::terminal::parse_snapshot(&tasks, true)
        .map(|snapshot| snapshot.terminal)
        .map_err(|_| {
            failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Protocol,
            )
        })
}

/// Collect every currently visible task and require a fresh absence proof after cancellation.
async fn collect_native_tasks(shell: &PausedShell) -> Result<(), ProviderError> {
    let mut failures = Vec::new();
    for terminal in native_tasks(shell).await? {
        if shell
            .session
            .rpc()
            .tasks()
            .cancel(github_copilot_sdk::rpc::TasksCancelRequest {
                id: terminal.id.as_str().to_owned(),
            })
            .await
            .is_err()
        {
            failures.push(failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Runtime,
            ));
        }
    }
    if native_tasks(shell).await?.is_empty() {
        if failures.is_empty() {
            return Ok(());
        }
        let primary = failures.remove(0);
        return if failures.is_empty() {
            Err(primary)
        } else {
            Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: failures,
            })
        };
    }
    failures.push(failure(
        ProviderOperation::ThreadClose,
        ProviderFailureKind::ResourceConflict,
    ));
    let primary = failures.remove(0);
    if failures.is_empty() {
        Err(primary)
    } else {
        Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: failures,
        })
    }
}

/// Accept only the native not-found proof as successful idempotent history deletion.
async fn delete_native_history(
    client: &github_copilot_sdk::Client,
    id: &SessionId,
) -> Result<(), ProviderError> {
    match client.delete_session(id).await {
        Ok(()) => Ok(()),
        Err(error)
            if matches!(
                error.kind(),
                github_copilot_sdk::ErrorKind::Session(
                    github_copilot_sdk::SessionErrorKind::NotFound(_)
                )
            ) =>
        {
            Ok(())
        }
        Err(_) => Err(failure(
            ProviderOperation::ThreadClose,
            ProviderFailureKind::Transport,
        )),
    }
}

/// Derive reserved private history from the main native session without persisting another schema.
fn helper_identity(main_session_id: &str) -> SessionId {
    let mut hash = Sha256::new();
    hash.update(b"forge-copilot-shell-v1\0");
    hash.update(main_session_id.as_bytes());
    let digest = hash.finalize();
    let mut bytes = [0; 16];
    bytes.copy_from_slice(&digest[..16]);
    SessionId::new(uuid::Uuid::from_bytes(bytes).to_string())
}

#[cfg(test)]
mod tests;
