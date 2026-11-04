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
    /// A bounded non-prompt request did not finish.
    #[error("ACP {method} exceeded {seconds} seconds")]
    Timeout {
        method: String,
        seconds: u64,
        #[source]
        source: tokio::time::error::Elapsed,
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
    Running(tokio::task::JoinHandle<Result<(), agent_client_protocol::Error>>),
    Collected,
}

/// Retain native resources from spawn through successful collection, including failed opens.
#[derive(Clone)]
pub(super) struct AcpCleanup {
    resources: Arc<std::sync::Mutex<Vec<AcpResource>>>,
    parent: Option<Arc<std::sync::Mutex<Vec<AcpResource>>>>,
    admission: Arc<std::sync::atomic::AtomicBool>,
    parent_admission: Option<Arc<std::sync::atomic::AtomicBool>>,
    #[cfg(test)]
    injected_collection_failures: Arc<std::sync::atomic::AtomicUsize>,
}

impl Default for AcpCleanup {
    fn default() -> Self {
        Self {
            resources: Arc::new(std::sync::Mutex::new(Vec::new())),
            parent: None,
            admission: Arc::new(std::sync::atomic::AtomicBool::new(true)),
            parent_admission: None,
            #[cfg(test)]
            injected_collection_failures: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
        }
    }
}

impl AcpCleanup {
    /// Create an operation-local cleanup scope while preserving provider shutdown ownership.
    pub(super) fn scope(&self) -> Self {
        Self {
            resources: Arc::new(std::sync::Mutex::new(Vec::new())),
            parent: Some(self.parent.as_ref().unwrap_or(&self.resources).clone()),
            admission: Arc::new(std::sync::atomic::AtomicBool::new(true)),
            parent_admission: Some(
                self.parent_admission
                    .as_ref()
                    .unwrap_or(&self.admission)
                    .clone(),
            ),
            #[cfg(test)]
            injected_collection_failures: self.injected_collection_failures.clone(),
        }
    }

    /// Retain a native profile before a connection can load it or suspend startup.
    pub(super) fn retain_profile(
        &self,
        profile: crate::profile::ResourceProfile,
    ) -> Arc<AcpProfile> {
        let profile = Arc::new(AcpProfile {
            path: profile.path().to_owned(),
            profile: Mutex::new(Some(profile)),
            collected: std::sync::atomic::AtomicBool::new(false),
            #[cfg(test)]
            injected_failures: std::sync::atomic::AtomicUsize::new(0),
        });
        self.retain(AcpResource::Profile(profile.clone()));
        profile
    }

    /// Retain callback/server authority until its native resources and profile have collected.
    pub(super) fn retain_lease(
        &self,
        lease: Option<Arc<dyn forge_provider_api::ResourceLease>>,
    ) -> Arc<AcpLease> {
        let collected = lease.is_none();
        let lease = Arc::new(AcpLease {
            lease: Mutex::new(lease),
            collected: std::sync::atomic::AtomicBool::new(collected),
        });
        self.retain(AcpResource::Lease(lease.clone()));
        lease
    }

    /// Get whether every resource registered to this operation completed collection.
    pub(super) fn is_collected(&self) -> bool {
        self.resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .iter()
            .all(AcpResource::is_collected)
    }

    /// Stop native launch admission before collecting this operation's pending resources.
    pub(super) fn close_admission(&self) {
        self.admission.store(false, Ordering::Release);
    }

    /// Require both this operation and the provider shutdown owner to admit native launch.
    fn is_admitting(&self) -> bool {
        self.admission.load(Ordering::Acquire)
            && self
                .parent_admission
                .as_ref()
                .is_none_or(|parent| parent.load(Ordering::Acquire))
    }

    /// Register ownership before startup can suspend or fail.
    fn retain(&self, resources: AcpResource) {
        if let Some(parent) = &self.parent {
            parent
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(resources.clone());
        }
        self.resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(resources);
    }

    /// Inject a cleanup failure on the next native process without releasing its resources.
    #[cfg(test)]
    pub(super) fn fail_next_collection(&self, count: usize) {
        self.injected_collection_failures
            .store(count, Ordering::Release);
    }

    /// Count retained native processes or readers whose collection has not completed.
    #[cfg(test)]
    pub(super) fn pending_count(&self) -> usize {
        self.resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .iter()
            .filter(|resource| {
                matches!(resource, AcpResource::Native(_)) && !resource.is_collected()
            })
            .count()
    }

    /// Retry process/readers first, then profiles and callback leases only after native collection.
    pub(super) async fn shutdown(&self) -> Result<(), forge_provider_api::ProviderError> {
        self.close_admission();
        let resources = self
            .resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let mut errors = Vec::new();
        for resource in &resources {
            if let AcpResource::Native(native) = resource
                && let Err(error) = native.shutdown().await
            {
                errors.push(crate::error::acp(
                    error,
                    forge_provider_api::ProviderOperation::Shutdown,
                ));
            }
        }
        let native_collected = resources.iter().all(|resource| match resource {
            AcpResource::Native(_) => resource.is_collected(),
            AcpResource::Profile(_) | AcpResource::Lease(_) => true,
        });
        if native_collected {
            for resource in &resources {
                if let AcpResource::Profile(profile) = resource
                    && let Err(error) = profile
                        .close(forge_provider_api::ProviderOperation::Shutdown)
                        .await
                {
                    errors.push(error);
                }
            }
        }
        let prerequisites_collected = resources.iter().all(|resource| match resource {
            AcpResource::Native(_) | AcpResource::Profile(_) => resource.is_collected(),
            AcpResource::Lease(_) => true,
        });
        if prerequisites_collected {
            for resource in &resources {
                if let AcpResource::Lease(lease) = resource {
                    lease.release().await;
                }
            }
        }
        let mut errors = errors.into_iter();
        let Some(primary) = errors.next() else {
            return Ok(());
        };
        let secondary = errors.collect::<Vec<_>>();
        if secondary.is_empty() {
            Err(primary)
        } else {
            Err(forge_provider_api::ProviderError::Combined {
                primary: Box::new(primary),
                secondary,
            })
        }
    }
}

/// Keep native startup, profile, and callback ownership in one retained cleanup graph.
#[derive(Clone)]
enum AcpResource {
    Native(Arc<AcpTransportResources>),
    Profile(Arc<AcpProfile>),
    Lease(Arc<AcpLease>),
}

impl AcpResource {
    /// Get positive collection evidence without awaiting a resource currently being collected.
    fn is_collected(&self) -> bool {
        match self {
            Self::Native(native) => native.collected.load(Ordering::Acquire),
            Self::Profile(profile) => profile.collected.load(Ordering::Acquire),
            Self::Lease(lease) => lease.collected.load(Ordering::Acquire),
        }
    }
}

/// Retain one profile across native load/configuration failures and cleanup retries.
pub(super) struct AcpProfile {
    path: std::path::PathBuf,
    profile: Mutex<Option<crate::profile::ResourceProfile>>,
    collected: std::sync::atomic::AtomicBool,
    #[cfg(test)]
    injected_failures: std::sync::atomic::AtomicUsize,
}

impl AcpProfile {
    /// Get the unchanged native profile path while its retained owner controls collection.
    pub(super) fn path(&self) -> &Path {
        &self.path
    }

    /// Inject a profile collection failure without discarding its retriable owner.
    #[cfg(test)]
    pub(super) fn fail_next_close(&self, count: usize) {
        self.injected_failures.store(count, Ordering::Release);
    }

    /// Close the profile once and retain a failed close for the next cleanup attempt.
    pub(super) async fn close(
        &self,
        operation: forge_provider_api::ProviderOperation,
    ) -> Result<(), forge_provider_api::ProviderError> {
        let mut profile = self.profile.lock().await;
        #[cfg(test)]
        if self
            .injected_failures
            .try_update(Ordering::AcqRel, Ordering::Acquire, |count| {
                count.checked_sub(1)
            })
            .is_ok()
        {
            return Err(forge_provider_api::ProviderFailure::new(
                operation,
                forge_provider_api::ProviderFailureKind::Runtime,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Collection,
                forge_provider_api::FailureDisposition::Invalidated,
            )
            .into());
        }
        if let Some(profile) = profile.as_mut() {
            profile
                .close()
                .map_err(|error| match error.into_provider_error(operation) {
                    forge_provider_api::ProviderError::Failure(failure) => {
                        forge_provider_api::ProviderFailure::new(operation, failure.kind())
                            .at_phase(
                                forge_provider_api::ProviderPhase::Collection,
                                forge_provider_api::FailureDisposition::Invalidated,
                            )
                            .into()
                    }
                    error => error,
                })?;
        }
        profile.take();
        self.collected.store(true, Ordering::Release);
        Ok(())
    }
}

/// Retain host callback authority until all native users have been collected.
pub(super) struct AcpLease {
    lease: Mutex<Option<Arc<dyn forge_provider_api::ResourceLease>>>,
    collected: std::sync::atomic::AtomicBool,
}

impl AcpLease {
    /// Release callback/server ownership after native collection, idempotently across retries.
    pub(super) async fn release(&self) {
        self.lease.lock().await.take();
        self.collected.store(true, Ordering::Release);
    }
}

/// Own native process and reader handles independently of successful protocol initialization.
struct AcpTransportResources {
    state: Mutex<TransportState>,
    first_failure: Mutex<Option<forge_provider_api::ProviderError>>,
    collected: std::sync::atomic::AtomicBool,
    #[cfg(test)]
    injected_failures: std::sync::atomic::AtomicUsize,
}

struct TransportState {
    process: Box<dyn process_wrap::tokio::ChildWrapper>,
    driver: DriverState,
    process_collected: bool,
}

impl AcpTransportResources {
    /// Collect independent native phases while retaining handles for cancellation and retry.
    async fn shutdown(&self) -> Result<(), AcpConnectionError> {
        #[cfg(test)]
        if self
            .injected_failures
            .try_update(Ordering::AcqRel, Ordering::Acquire, |count| {
                count.checked_sub(1)
            })
            .is_ok()
        {
            return Err(AcpConnectionError::Collection {
                process: Some(std::io::Error::other("injected process collection failure")),
                reader: None,
            });
        }
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

/// Own the native connection while the provider cleanup registry retains its process and reader.
pub struct AcpConnection {
    connection: ConnectionTo<UntypedRole>,
    resources: Arc<AcpTransportResources>,
}

impl AcpConnection {
    /// Launch a standalone deterministic wire fixture using the production retained lifecycle.
    #[cfg(test)]
    pub async fn launch(
        launch: AcpLaunch<'_>,
        handler: Arc<dyn AcpHandler>,
    ) -> Result<Self, AcpConnectionError> {
        let cleanup = AcpCleanup::default();
        Self::launch_retained(launch, handler, &cleanup).await
    }

    /// Register native ownership before launching the reader or awaiting startup negotiation.
    pub(super) async fn launch_retained(
        launch: AcpLaunch<'_>,
        handler: Arc<dyn AcpHandler>,
        cleanup: &AcpCleanup,
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
        let process = forge_shell::process::spawn_owned(command).map_err(|source| {
            AcpConnectionError::Process {
                operation: "launch ACP provider",
                source,
            }
        })?;
        let resources = Arc::new(AcpTransportResources {
            state: Mutex::new(TransportState {
                process,
                driver: DriverState::Starting,
                process_collected: false,
            }),
            first_failure: Mutex::new(None),
            collected: std::sync::atomic::AtomicBool::new(false),
            #[cfg(test)]
            injected_failures: std::sync::atomic::AtomicUsize::new(
                cleanup
                    .injected_collection_failures
                    .swap(0, Ordering::AcqRel),
            ),
        });
        cleanup.retain(AcpResource::Native(resources.clone()));
        let mut retained = resources.state.lock().await;
        if retained.process_collected || !cleanup.is_admitting() {
            drop(retained);
            let _ = resources.shutdown().await;
            return Err(AcpConnectionError::Configuration(
                "ACP startup was collected",
            ));
        }
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
        let transport = ByteStreams::new(outgoing.compat_write(), incoming.compat());
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
                                    agent_client_protocol::util::internal_error(format!(
                                        "{error:#}"
                                    ))
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
        retained.driver = DriverState::Running(driver);
        drop(retained);
        let connection = match tokio::time::timeout(std::time::Duration::from_secs(10), ready).await
        {
            Ok(Ok(connection)) => connection,
            Ok(Err(source)) => {
                let _ = resources.shutdown().await;
                return Err(AcpConnectionError::Startup {
                    message: "ACP driver exited before connection startup",
                    source: Box::new(source),
                });
            }
            Err(source) => {
                let _ = resources.shutdown().await;
                return Err(AcpConnectionError::Startup {
                    message: "ACP connection startup exceeded 10 seconds",
                    source: Box::new(source),
                });
            }
        };
        Ok(Self {
            connection,
            resources,
        })
    }

    /// Retain the first normalized open failure independently of the connection facade.
    pub(super) async fn retain_failure(&self, cause: forge_provider_api::ProviderError) {
        self.resources
            .first_failure
            .lock()
            .await
            .get_or_insert(cause);
    }

    /// Send a native request; interactive prompts remain unbounded.
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
                let seconds = if method == "authenticate" { 120 } else { 30 };
                tokio::time::timeout(std::time::Duration::from_secs(seconds), request)
                    .await
                    .map_err(|source| AcpConnectionError::Timeout {
                        method: method.into(),
                        seconds,
                        source,
                    })?
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
        self.resources.shutdown().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct UnexpectedCallback;

    #[async_trait]
    impl AcpHandler for UnexpectedCallback {
        async fn notification(&self, _: &str, _: Value) -> Result<(), AcpConnectionError> {
            panic!("empty command must not launch a provider")
        }

        async fn request(&self, _: &str, _: Value) -> Result<Value, AcpConnectionError> {
            panic!("empty command must not launch a provider")
        }
    }

    #[tokio::test]
    async fn missing_command_fails_before_launch() {
        let environment = BTreeMap::new();
        let launch = AcpLaunch {
            command: &[],
            environment: &environment,
            workspace: ".",
            profile: None,
        };
        let result = AcpConnection::launch(launch, Arc::new(UnexpectedCallback)).await;
        assert!(matches!(
            result,
            Err(AcpConnectionError::Configuration(
                "ACP provider command is empty"
            ))
        ));
    }
}
