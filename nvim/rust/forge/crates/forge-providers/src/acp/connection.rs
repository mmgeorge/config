use agent_client_protocol::{ByteStreams, ConnectionTo, UntypedMessage, UntypedRole};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use tokio::sync::{Mutex, oneshot};
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

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
    /// A native message could not be encoded.
    #[error("encode ACP {method}: {source}")]
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
    }
}

type DriverTask = tokio::task::JoinHandle<Result<(), agent_client_protocol::Error>>;

/// Launch the provider as an owned process with piped stdio.
fn spawn_provider_process(
    launch: &AcpLaunch<'_>,
    program: &str,
) -> Result<Box<dyn process_wrap::tokio::ChildWrapper>, AcpConnectionError> {
    let mut command = forge_shell::process::command(program, &launch.command[1..]);
    command.envs(launch.environment);
    if let Some(profile) = launch.profile {
        command.env("GEMINI_HOME", profile);
    }
    command
        .current_dir(launch.workspace)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null());
    forge_shell::process::spawn_owned(command).map_err(|source| AcpConnectionError::Process {
        operation: "launch ACP provider",
        source,
    })
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
                        let response = handler
                            .request(request.method(), request.params().clone())
                            .await
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
        let resources = Arc::new(AcpTransportResources {
            state: Mutex::new(TransportState {
                process,
                driver: DriverState::Starting,
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
        self.request_tracked(method, parameter, None, None).await
    }

    /// Hand a tracked request to the native driver before waiting for its response.
    /// Prompt callers have already installed immutable configuration and collection
    /// ownership. The SDK retains response/cancellation routing at publication.
    pub(super) async fn request_tracked(
        &self,
        method: &str,
        parameter: Value,
        dispatch: Option<&forge_provider_api::submission::DispatchReporter>,
        cancellation: Option<&tokio_util::sync::CancellationToken>,
    ) -> Result<Value, AcpConnectionError> {
        let session_id = parameter["sessionId"].as_str().map(str::to_owned);
        let message = UntypedMessage::new(method, parameter).map_err(|source| {
            AcpConnectionError::Protocol {
                method: method.into(),
                source: Box::new(source),
            }
        })?;
        let request = self.connection.send_request(message).block_task();
        tokio::pin!(request);
        // Drain synchronous publication errors before confirming the tracked handoff.
        // Pending means the SDK owns the request and response/cancellation routing;
        // it does not prove remote delivery or receipt acceptance.
        let response = if let std::task::Poll::Ready(response) = futures_util::poll!(&mut request) {
            if response.is_ok()
                && let Some(dispatch) = dispatch
            {
                dispatch.confirm();
            }
            response
        } else {
            if let Some(dispatch) = dispatch {
                dispatch.confirm();
            }
            if method == "session/prompt" {
                if let (Some(cancellation), Some(session_id)) = (cancellation, session_id) {
                    tokio::select! {
                        biased;
                        result = &mut request => result,
                        _ = cancellation.cancelled() => {
                            self.cancel(&session_id)?;
                            request.await
                        }
                    }
                } else {
                    request.await
                }
            } else {
                // A native subrequest must not reset or shorten its retained operation's budget.
                request.await
            }
        };
        response.map_err(|source| {
            let failure = remote_failure(&source);
            AcpConnectionError::Remote {
                method: method.into(),
                failure,
                source: Box::new(source),
            }
        })
    }

    /// Send the session's native interruption without closing its response reader.
    pub fn cancel(&self, session_id: &str) -> Result<(), AcpConnectionError> {
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
