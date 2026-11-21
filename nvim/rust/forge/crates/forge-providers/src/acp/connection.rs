use agent_client_protocol::{ByteStreams, ConnectionTo, UntypedMessage, UntypedRole};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::io::{AsyncRead, AsyncReadExt};
use tokio::sync::{Mutex, oneshot, watch};
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

/// Order native prompt publication, replies, and cancellation for one invocation.
#[derive(Clone, Default)]
pub(super) struct PromptCancellation {
    pub(super) token: tokio_util::sync::CancellationToken,
    replies_drained: tokio_util::sync::CancellationToken,
    state: Arc<std::sync::Mutex<PromptPublication>>,
}

#[derive(Default)]
struct PromptPublication {
    pending_count: usize,
    sealed: bool,
    notified: bool,
}

impl PromptCancellation {
    /// Fence future sends while leaving admitted requests alive until their replies arrive.
    pub(super) fn seal(&self) {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.sealed = true;
        if state.pending_count == 0 {
            self.replies_drained.cancel();
        }
    }

    /// Prevent any further native prompt publication for this invocation.
    pub(super) fn stop(&self) {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.sealed = true;
        self.token.cancel();
        if state.pending_count == 0 {
            self.replies_drained.cancel();
        }
    }

    /// Wait for the last native reply to close prompt publication.
    pub(super) async fn wait_for_replies(&self) {
        self.replies_drained.cancelled().await;
    }

    /// Send the native notification once even when multiple requests are awaiting replies.
    pub(super) fn notify(
        &self,
        connection: &AcpConnection,
        session_id: &str,
    ) -> Result<(), AcpConnectionError> {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.sealed = true;
        self.token.cancel();
        if state.pending_count == 0 {
            self.replies_drained.cancel();
            return Ok(());
        }
        if state.notified {
            return Ok(());
        }
        connection.cancel(session_id)?;
        state.notified = true;
        Ok(())
    }

    /// Publish while the invocation still owns native work, or when publishing its first prompt.
    fn publish(
        &self,
        send: impl FnOnce() -> Result<(), agent_client_protocol::Error>,
    ) -> Result<bool, agent_client_protocol::Error> {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if state.sealed {
            return Ok(false);
        }
        // Hold the gate through SDK publication so the ordered callback and cancel
        // cannot observe a request that was enqueued but has yet to be counted.
        send()?;
        state.pending_count += 1;
        Ok(true)
    }

    /// Enqueue a native setter only while the invocation remains open to publication.
    fn publish_configuration<Sent>(&self, send: impl FnOnce() -> Sent) -> Option<Sent> {
        let state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        (!state.sealed).then(send)
    }

    /// Retire one published request as its ordered reply arrives or its response channel closes.
    fn reply_finished(&self) {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        state.pending_count = state
            .pending_count
            .checked_sub(1)
            .expect("a reply retires its published request once");
        if state.pending_count == 0 {
            state.sealed = true;
            self.replies_drained.cancel();
        }
    }
}

type SourceError = Box<dyn std::error::Error + Send + Sync>;

/// Describes an ACP connection failure without depending on host workflow errors.
#[derive(Debug, thiserror::Error)]
pub enum AcpConnectionError {
    /// The launch descriptor does not identify a program or required stream.
    #[error("{0}")]
    Configuration(&'static str),
    /// An owned provider process could not be launched or collected.
    #[error("{operation}: {source}")]
    Process {
        operation: &'static str,
        #[source]
        source: std::io::Error,
    },
    /// The native driver could not establish a connection.
    #[error("{message}: {source}")]
    Startup {
        message: &'static str,
        #[source]
        source: SourceError,
    },
    /// The provider exited during initialization with a sanitized startup diagnosis.
    #[error("{reason}")]
    StartupExit { reason: AcpStartupFailure },
    /// A native message could not be encoded.
    #[error("ACP {method} protocol error: {source}")]
    Protocol {
        method: String,
        #[source]
        source: SourceError,
    },
    /// The remote provider rejected or failed a request.
    #[error("ACP {method} failed: {source}")]
    Remote {
        method: String,
        failure: Option<AcpRemoteFailure>,
        #[source]
        source: SourceError,
    },
    /// Independent process and reader collection phases failed; retain both typed causes.
    #[error("ACP resource collection failed")]
    Collection {
        process: Option<std::io::Error>,
        reader: Option<tokio::task::JoinError>,
    },
    /// The host rejected an incoming callback.
    #[error("{0}")]
    Callback(#[source] SourceError),
}

/// Preserve only recognized, provider-neutral ACP remote error classes.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AcpRemoteFailure {
    AuthenticationRequired,
    RequestCancelled,
    ResourceNotFound,
    Rejected,
    Protocol,
}

/// Classify launch output with fixed text that excludes provider paths and arguments.
#[derive(Clone, Copy, Debug, thiserror::Error)]
pub enum AcpStartupFailure {
    #[error("Antigravity rejected a launch option. Check the configured command and run agy acp --help.")]
    UnsupportedOption,
    #[error("Antigravity exited before completing ACP initialization.")]
    ExitedBeforeInitialize,
}

fn remote_failure(source: &(dyn std::error::Error + 'static)) -> Option<AcpRemoteFailure> {
    let mut cause = Some(source);
    while let Some(current) = cause {
        if let Some(error) = current.downcast_ref::<agent_client_protocol::Error>() {
            use agent_client_protocol::ErrorCode;
            return match &error.code {
                ErrorCode::AuthRequired => Some(AcpRemoteFailure::AuthenticationRequired),
                ErrorCode::RequestCancelled => Some(AcpRemoteFailure::RequestCancelled),
                ErrorCode::ResourceNotFound => Some(AcpRemoteFailure::ResourceNotFound),
                ErrorCode::InvalidRequest | ErrorCode::InvalidParams => {
                    Some(AcpRemoteFailure::Rejected)
                }
                ErrorCode::ParseError | ErrorCode::MethodNotFound | ErrorCode::InternalError => {
                    Some(AcpRemoteFailure::Protocol)
                }
                _ => None,
            };
        }
        cause = current.source();
    }
    None
}

/// Handles native callbacks while the connection continues reading responses.
#[async_trait]
pub trait AcpHandler: Send + Sync {
    /// Deliver one provider notification to its host.
    async fn notification(&self, method: &str, parameter: Value) -> Result<(), AcpConnectionError>;
    /// Resolve one provider request through the host's policy and tools.
    async fn request(&self, method: &str, parameter: Value) -> Result<Value, AcpConnectionError>;
}

/// Supplies resolved launch settings without owning host configuration or permissions.
pub struct AcpLaunch<'a> {
    pub process_owner: Option<Arc<dyn forge_shell::process::ProcessOwner>>,
    pub command: &'a [String],
    pub environment: &'a BTreeMap<String, String>,
    pub workspace: &'a str,
    pub profile: Option<&'a Path>,
}

/// Retain reader collection across cancelled waiters and process-cleanup retries.
enum DriverState {
    Starting,
    Running(DriverTask),
    Collected,
}

/// Own native process and reader handles independently of successful protocol initialization.
struct AcpTransportResources {
    state: Mutex<TransportState>,
    first_failure: Mutex<Option<forge_provider_api::ProviderError>>,
    collected: std::sync::atomic::AtomicBool,
}

struct TransportState {
    process: Box<dyn process_wrap::tokio::ChildWrapper>,
    driver: DriverState,
    stderr: Option<tokio::task::JoinHandle<()>>,
    process_collected: bool,
}

#[async_trait]
impl crate::resource_cleanup::NativeResource for AcpTransportResources {
    async fn shutdown(&self) -> Result<(), forge_provider_api::ProviderError> {
        self.collect().await.map_err(|error| {
            crate::error::acp(error, forge_provider_api::ProviderOperation::Shutdown)
        })
    }

    fn is_collected(&self) -> bool {
        self.collected.load(Ordering::Acquire)
    }
}

impl AcpTransportResources {
    /// Collect independent native phases while retaining handles for cancellation and retry.
    async fn collect(&self) -> Result<(), AcpConnectionError> {
        let mut state = self.state.lock().await;
        let process_failure = if state.process_collected {
            None
        } else {
            let failure = Box::into_pin(state.process.kill()).await.err();
            if failure.is_none() {
                state.process_collected = true;
            }
            failure
        };
        let reader_failure = match &mut state.driver {
            DriverState::Running(task) => {
                task.abort();
                let failure = match task.await {
                    Ok(_) => None,
                    Err(error) if error.is_cancelled() => None,
                    Err(error) => Some(error),
                };
                state.driver = DriverState::Collected;
                failure
            }
            DriverState::Starting => {
                state.driver = DriverState::Collected;
                None
            }
            DriverState::Collected => None,
        };
        if let Some(stderr) = state.stderr.as_mut() {
            stderr.abort();
            let _ = stderr.await;
            state.stderr = None;
        }
        if process_failure.is_some() || reader_failure.is_some() {
            return Err(AcpConnectionError::Collection {
                process: process_failure,
                reader: reader_failure,
            });
        }
        self.collected.store(true, Ordering::Release);
        Ok(())
    }
}

impl Drop for AcpTransportResources {
    fn drop(&mut self) {
        if let DriverState::Running(driver) = &mut self.state.get_mut().driver {
            driver.abort();
        }
        if let Some(stderr) = &self.state.get_mut().stderr {
            stderr.abort();
        }
    }
}

type DriverTask = tokio::task::JoinHandle<Result<(), agent_client_protocol::Error>>;

/// Launch the provider as an owned process with piped stdio.
fn spawn_provider_process(
    launch: &AcpLaunch<'_>,
    program: &str,
) -> Result<Box<dyn process_wrap::tokio::ChildWrapper>, AcpConnectionError> {
    let arguments = &launch.command[1..];
    let separator = arguments
        .iter()
        .position(|argument| argument == "--")
        .unwrap_or(arguments.len());
    let (options, trailing) = arguments.split_at(separator);
    let mut command = forge_shell::process::command(program, options);
    command.envs(launch.environment);
    if let Some(profile) = launch.profile {
        command
            .arg("--gemini-dir")
            .arg(profile)
            .arg("--cli_dir_name")
            .arg(super::profile::cli_data_argument())
            .arg("--agent")
            .arg(super::profile::agent_name(profile));
    }
    command
        .args(trailing)
        .current_dir(launch.workspace)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped());
    match &launch.process_owner {
        Some(owner) => owner.spawn(command),
        None => forge_shell::process::spawn_owned(command),
    }
    .map_err(|source| AcpConnectionError::Process {
        operation: "launch ACP provider",
        source,
    })
}

/// Drain stderr without retaining native text past a diagnostic's fixed prefix.
async fn read_startup_diagnostics(
    mut stderr: impl AsyncRead + Unpin,
    diagnostics: watch::Sender<Option<AcpStartupFailure>>,
) {
    const OPTION_ERRORS: [&[u8]; 2] = [
        b"Error: flags provided but not defined:",
        b"Error: unexpected arguments:",
    ];
    let mut buffer = [0; 1024];
    let mut prefix = Vec::new();
    let mut prefix_complete = false;
    while let Ok(length) = stderr.read(&mut buffer).await {
        if length == 0 {
            break;
        }
        for byte in &buffer[..length] {
            if *byte == b'\n' {
                prefix.clear();
                prefix_complete = false;
            } else if !prefix_complete {
                prefix.push(*byte);
                if OPTION_ERRORS.contains(&prefix.as_slice()) {
                    diagnostics.send_replace(Some(AcpStartupFailure::UnsupportedOption));
                }
                prefix_complete = !OPTION_ERRORS.iter().any(|error| error.starts_with(&prefix));
            }
        }
    }
}

/// Run the native driver over one byte transport until it closes or is aborted.
fn start_driver<Outgoing, Incoming>(
    outgoing: Outgoing,
    incoming: Incoming,
    handler: Arc<dyn AcpHandler>,
) -> (DriverTask, oneshot::Receiver<ConnectionTo<UntypedRole>>)
where
    Outgoing: futures_util::io::AsyncWrite + Send + 'static,
    Incoming: futures_util::io::AsyncRead + Send + 'static,
{
    let transport = ByteStreams::new(outgoing, incoming);
    let (connected, ready) = oneshot::channel();
    let notification_handler = Arc::clone(&handler);
    let driver = tokio::spawn(async move {
        UntypedRole
            .builder()
            .name("forge-harness")
            .on_receive_notification(
                async move |notification: UntypedMessage, _context| {
                    notification_handler
                        .notification(notification.method(), notification.params().clone())
                        .await
                        .map_err(|error| {
                            agent_client_protocol::util::internal_error(format!("{error:#}"))
                        })
                },
                agent_client_protocol::on_receive_notification!(),
            )
            .on_receive_request(
                async move |request: UntypedMessage, responder, context| {
                    let handler = Arc::clone(&handler);
                    context.spawn(async move {
                        let work = handler.request(request.method(), request.params().clone());
                        let response = if request.method() == "session/request_permission" {
                            let cancellation = responder.cancellation();
                            tokio::select! {
                                biased;
                                _ = cancellation.cancelled() => Ok(serde_json::json!({
                                    "outcome": {"outcome": "cancelled"}
                                })),
                                result = work => result,
                            }
                        } else {
                            work.await
                        }
                        .map_err(|error| {
                            agent_client_protocol::util::internal_error(format!("{error:#}"))
                        });
                        match response {
                            Ok(value) => responder.respond(value),
                            Err(error) => responder.respond_with_error(error),
                        }
                    })
                },
                agent_client_protocol::on_receive_request!(),
            )
            .connect_with(transport, async move |context| {
                let _ = connected.send(context.clone());
                context.incoming_closed().await;
                Ok(())
            })
            .await
    });
    (driver, ready)
}

/// Own the native connection while the provider cleanup registry retains its process and reader.
pub struct AcpConnection {
    connection: ConnectionTo<UntypedRole>,
    resources: Arc<AcpTransportResources>,
    startup_diagnostics: watch::Receiver<Option<AcpStartupFailure>>,
    prompt_end_count: Arc<AtomicU64>,
    prompt_ends: std::sync::Mutex<
        std::collections::HashMap<
            (
                forge_provider_api::SubmissionId,
                forge_provider_api::DeliveryAttemptId,
            ),
            super::response::NativePromptEnd,
        >,
    >,
}

impl AcpConnection {
    /// Launch a standalone deterministic wire fixture using the production retained lifecycle.
    #[cfg(test)]
    pub async fn launch(
        launch: AcpLaunch<'_>,
        handler: Arc<dyn AcpHandler>,
    ) -> Result<Self, AcpConnectionError> {
        let cleanup = crate::resource_cleanup::ResourceCleanup::default();
        Self::launch_retained(launch, handler, &cleanup).await
    }

    /// Register native ownership before launching the reader or awaiting startup negotiation.
    pub(super) async fn launch_retained(
        launch: AcpLaunch<'_>,
        handler: Arc<dyn AcpHandler>,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Result<Self, AcpConnectionError> {
        if !cleanup.is_admitting() {
            return Err(AcpConnectionError::Configuration(
                "ACP launch operation is closed",
            ));
        }
        let program = launch
            .command
            .first()
            .ok_or(AcpConnectionError::Configuration(
                "ACP provider command is empty",
            ))?;
        #[cfg(test)]
        let (process, peer_streams) = match super::in_memory_peer::attach(program) {
            Some(attached) => (attached.process, Some(attached.streams)),
            None => (spawn_provider_process(&launch, program)?, None),
        };
        #[cfg(not(test))]
        let process = spawn_provider_process(&launch, program)?;
        let (diagnostics, startup_diagnostics) = watch::channel(None);
        let resources = Arc::new(AcpTransportResources {
            state: Mutex::new(TransportState {
                process,
                driver: DriverState::Starting,
                stderr: None,
                process_collected: false,
            }),
            first_failure: Mutex::new(None),
            collected: std::sync::atomic::AtomicBool::new(false),
        });
        cleanup.retain_native(resources.clone());
        let mut retained = resources.state.lock().await;
        if retained.process_collected || !cleanup.is_admitting() {
            drop(retained);
            let _ = resources.collect().await;
            return Err(AcpConnectionError::Configuration(
                "ACP startup was collected",
            ));
        }
        retained.stderr = retained
            .process
            .stderr()
            .take()
            .map(|stderr| tokio::spawn(read_startup_diagnostics(stderr, diagnostics)));
        #[cfg(test)]
        let attached_driver = peer_streams.map(|streams| {
            start_driver(
                streams.outgoing.compat_write(),
                streams.incoming.compat(),
                Arc::clone(&handler),
            )
        });
        #[cfg(not(test))]
        let attached_driver: Option<(
            DriverTask,
            oneshot::Receiver<ConnectionTo<UntypedRole>>,
        )> = None;
        let (driver, ready) = match attached_driver {
            Some(attached) => attached,
            None => {
                let outgoing = retained
                    .process
                    .stdin()
                    .take()
                    .ok_or(AcpConnectionError::Configuration("ACP stdin missing"))?;
                let incoming = retained
                    .process
                    .stdout()
                    .take()
                    .ok_or(AcpConnectionError::Configuration("ACP stdout missing"))?;
                start_driver(outgoing.compat_write(), incoming.compat(), handler)
            }
        };
        retained.driver = DriverState::Running(driver);
        drop(retained);
        // The retained operation owns the single deadline and keeps this driver collectible.
        let connection = match ready.await {
            Ok(connection) => connection,
            Err(source) => {
                let _ = resources.collect().await;
                return Err(AcpConnectionError::Startup {
                    message: "ACP driver exited before connection startup",
                    source: Box::new(source),
                });
            }
        };
        Ok(Self {
            connection,
            resources,
            startup_diagnostics,
            prompt_end_count: Arc::default(),
            prompt_ends: Default::default(),
        })
    }

    /// Preserve native terminal evidence before receipt persistence can fail or wait.
    pub(super) fn retain_prompt_end(
        &self,
        input: &forge_provider_api::CoveredInput,
        end: super::response::NativePromptEnd,
    ) -> Result<(), forge_provider_api::ProviderError> {
        let mut ends = self
            .prompt_ends
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match ends.entry((input.submission_id().clone(), input.attempt_id().clone())) {
            std::collections::hash_map::Entry::Occupied(entry)
                if entry.get().outcome() != end.outcome() =>
            {
                Err(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
                .into())
            }
            std::collections::hash_map::Entry::Occupied(_) => Ok(()),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(end);
                Ok(())
            }
        }
    }

    /// Get the original native terminal fact for one exact journaled delivery attempt.
    pub(super) fn prompt_end(
        &self,
        input: &forge_provider_api::CoveredInput,
    ) -> Option<super::response::NativePromptEnd> {
        self.prompt_ends
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(&(input.submission_id().clone(), input.attempt_id().clone()))
            .copied()
    }

    /// Retain the first normalized open failure independently of the connection facade.
    pub(super) async fn retain_failure(&self, cause: forge_provider_api::ProviderError) {
        self.resources
            .first_failure
            .lock()
            .await
            .get_or_insert(cause);
    }

    /// Send a native request under the enclosing retained operation deadline.
    pub async fn request(
        &self,
        method: &str,
        parameter: Value,
    ) -> Result<Value, AcpConnectionError> {
        let message = request_message(method, parameter)?;
        response_value(method, self.connection.send_request(message).block_task().await)
    }

    /// Preserve the safe launch diagnosis when the native process closes the protocol at startup.
    pub(super) async fn request_initialization(
        &self,
        parameter: Value,
    ) -> Result<Value, AcpConnectionError> {
        match self.request("initialize", parameter).await {
            Err(AcpConnectionError::Remote {
                failure,
                ref source,
                ..
            }) if source
                .downcast_ref::<agent_client_protocol::Error>()
                .is_some_and(agent_client_protocol::is_incoming_transport_closed)
                || (failure.is_none() && self.connection.is_incoming_closed()) =>
            {
                let mut diagnostics = self.startup_diagnostics.clone();
                let reason = diagnostics
                    .wait_for(Option::is_some)
                    .await
                    .ok()
                    .and_then(|reason| *reason)
                    .unwrap_or(AcpStartupFailure::ExitedBeforeInitialize);
                Err(AcpConnectionError::StartupExit { reason })
            }
            result => result,
        }
    }

    /// Publish a setter only while preparation is admitted and retain every published reply.
    pub(super) async fn request_configuration(
        &self,
        method: &str,
        parameter: Value,
        cancellation: Option<&PromptCancellation>,
    ) -> Result<Option<Value>, AcpConnectionError> {
        let message = request_message(method, parameter)?;
        let send = || self.connection.send_request(message);
        let request = match cancellation {
            Some(cancellation) => match cancellation.publish_configuration(send) {
                Some(request) => request,
                None => return Ok(None),
            },
            None => send(),
        };
        response_value(method, request.block_task().await).map(Some)
    }

    /// Hand a prompt to the native driver and stamp its reply before later replies can pass it.
    /// A missing response means prompt admission closed before the request published.
    pub(super) async fn request_prompt(
        &self,
        method: &str,
        parameter: Value,
        session_id: &str,
        dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
        cancellation: Option<&PromptCancellation>,
    ) -> Result<Option<super::response::NativePromptEnd>, AcpConnectionError> {
        let message = request_message(method, parameter)?;
        let prepared = self.connection.prepare_request(message);
        let response_method = method.to_owned();
        let order = self.prompt_end_count.clone();
        let publication = cancellation.cloned();
        let (sender, mut receiver) = oneshot::channel();
        // The SDK acknowledges an ordered callback before dispatching the next message.
        // Keep journal work outside the callback so it cannot delay other inbound traffic.
        let send = || {
            prepared.on_receiving_result(move |response| async move {
                let result = response_value(&response_method, response).and_then(|response| {
                    super::response::NativePromptEnd::parse(
                        &response,
                        super::children::timestamp(),
                        order.fetch_add(1, Ordering::Relaxed),
                    )
                    .map_err(|source| AcpConnectionError::Protocol {
                        method: response_method,
                        source: Box::new(source),
                    })
                });
                if let Some(publication) = publication {
                    publication.reply_finished();
                }
                let _ = sender.send(result);
                Ok(())
            })
        };
        let published = if let Some(cancellation) = cancellation {
            cancellation.publish(send)
        } else {
            send().map(|()| true)
        }
        .map_err(|source| AcpConnectionError::Remote {
            method: method.into(),
            failure: remote_failure(&source),
            source: Box::new(source),
        })?;
        if !published {
            return Ok(None);
        }
        if let Some(dispatch) = dispatch {
            dispatch.confirm();
        }
        let mut cancel_error = None;
        let result = if let Some(cancellation) = cancellation {
            tokio::select! {
                biased;
                result = &mut receiver => result,
                _ = cancellation.token.cancelled() => {
                    cancel_error = cancellation.notify(self, session_id).err();
                    receiver.await
                }
            }
        } else {
            receiver.await
        };
        match result {
            Ok(result) => result.map(Some),
            Err(source) => {
                if let Some(cancellation) = cancellation {
                    cancellation.reply_finished();
                }
                Err(cancel_error.unwrap_or_else(|| AcpConnectionError::Remote {
                    method: method.into(),
                    failure: None,
                    source: Box::new(source),
                }))
            }
        }
    }

    /// Send the session's native interruption without closing its response reader.
    fn cancel(&self, session_id: &str) -> Result<(), AcpConnectionError> {
        let method = "session/cancel";
        let notification = UntypedMessage::new(method, serde_json::json!({"sessionId":session_id}))
            .map_err(|source| AcpConnectionError::Protocol {
                method: method.into(),
                source: Box::new(source),
            })?;
        self.connection
            .send_notification(notification)
            .map_err(|source| AcpConnectionError::Remote {
                method: method.into(),
                failure: None,
                source: Box::new(source),
            })
    }

    /// Collect the process tree and stop the native driver.
    pub async fn shutdown(&self) -> Result<(), AcpConnectionError> {
        self.resources.collect().await
    }
}

/// Encode the request before taking the invocation's publication gate.
fn request_message(method: &str, parameter: Value) -> Result<UntypedMessage, AcpConnectionError> {
    UntypedMessage::new(method, parameter).map_err(|source| AcpConnectionError::Protocol {
        method: method.into(),
        source: Box::new(source),
    })
}

/// Preserve the SDK's typed remote failure at the ACP boundary.
fn response_value(
    method: &str,
    response: Result<Value, agent_client_protocol::Error>,
) -> Result<Value, AcpConnectionError> {
    response.map_err(|source| {
        let failure = remote_failure(&source);
        AcpConnectionError::Remote {
            method: method.into(),
            failure,
            source: Box::new(source),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Classify only the fixed native diagnostic prefix, even when stderr contains private text.
    #[tokio::test]
    async fn startup_diagnostics_keep_only_a_fixed_explanation() {
        let (sender, diagnostics) = watch::channel(None);
        let stderr = b"private local path\r\nError: flags provided but not defined: private-option\r\n";
        read_startup_diagnostics(&stderr[..], sender).await;
        assert!(matches!(
            *diagnostics.borrow(),
            Some(AcpStartupFailure::UnsupportedOption),
        ));
    }
}
