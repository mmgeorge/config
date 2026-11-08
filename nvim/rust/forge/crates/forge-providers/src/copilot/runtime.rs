use forge_provider_api::ProviderError;
use github_copilot_sdk::{CliProgram, Client, ClientOptions};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tokio::sync::Mutex;

use super::handle::CopilotClient;

enum Shutdown {
    Idle,
    Running(tokio::task::JoinHandle<Result<(), std::sync::Arc<ProviderError>>>),
    Complete(Result<(), std::sync::Arc<ProviderError>>),
}

#[derive(Clone)]
enum RuntimeClient {
    Unvalidated(CopilotClient),
    Ready {
        client: super::handle::CopilotClient,
        _validation: forge_provider_api::version::ValidatedRuntime,
    },
}

impl RuntimeClient {
    /// Get the transport for resource collection regardless of validation state.
    fn client(&self) -> &CopilotClient {
        match self {
            Self::Unvalidated(client) => client,
            Self::Ready { client, .. } => client,
        }
    }
}

/// Owns the native client process and retains its collection task across waiter cancellation.
pub struct CopilotRuntime {
    command: Vec<String>,
    client: Mutex<Option<RuntimeClient>>,
    initializing: Mutex<()>,
    starts: AtomicU64,
    closed: AtomicBool,
    shutdown: Mutex<Shutdown>,
}

impl CopilotRuntime {
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            client: Mutex::new(None),
            initializing: Mutex::new(()),
            starts: AtomicU64::new(0),
            closed: AtomicBool::new(false),
            shutdown: Mutex::new(Shutdown::Idle),
        }
    }

    pub fn close_admission(&self) {
        self.closed.store(true, Ordering::Release);
    }

    /// Bind an externally supplied transport before this runtime starts a client.
    #[cfg(test)]
    pub async fn attach_streams<R, W>(
        &self,
        reader: R,
        writer: W,
        workspace: PathBuf,
    ) -> Result<(), ProviderError>
    where
        R: tokio::io::AsyncRead + Unpin + Send + 'static,
        W: tokio::io::AsyncWrite + Unpin + Send + 'static,
    {
        let _initializing = self.initializing.lock().await;
        let mut client = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) || client.is_some() {
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        *client = Some(RuntimeClient::Unvalidated(CopilotClient::new(
            Client::from_streams(reader, writer, workspace).map_err(|_| {
                provider_failure(
                    forge_provider_api::error::ProviderOperation::TurnStart,
                    forge_provider_api::error::ProviderFailureKind::Transport,
                )
            })?,
        )));
        Ok(())
    }

    /// Start and validate one runtime while keeping transport ownership visible to shutdown.
    pub async fn client(
        &self,
        workspace: &str,
    ) -> Result<super::handle::CopilotClient, ProviderError> {
        let _initializing = self.initializing.lock().await;
        if self.closed.load(Ordering::Acquire) {
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        let current = self.client.lock().await.clone();
        let transport = match current {
            Some(RuntimeClient::Ready { client, .. }) => {
                client.ensure_healthy()?;
                return Ok(client);
            }
            Some(RuntimeClient::Unvalidated(client)) => client,
            None => {
                let mut options = ClientOptions::new().with_cwd(workspace);
                if let Some(program) = self.command.first() {
                    options = options.with_program(CliProgram::Path(PathBuf::from(program)));
                    if self.command.len() > 1 {
                        options = options.with_prefix_args(self.command.iter().skip(1));
                    }
                }
                let started =
                    CopilotClient::new(Client::start(options).await.map_err(|error| {
                        let kind = if matches!(
                            error.kind(),
                            github_copilot_sdk::ErrorKind::BinaryNotFound { .. }
                        ) {
                            forge_provider_api::error::ProviderFailureKind::Runtime
                        } else {
                            forge_provider_api::error::ProviderFailureKind::Transport
                        };
                        provider_failure(
                            forge_provider_api::error::ProviderOperation::ThreadOpen,
                            kind,
                        )
                    })?);
                self.starts.fetch_add(1, Ordering::Relaxed);
                if self.closed.load(Ordering::Acquire) {
                    started.force_stop();
                    return Err(provider_failure(
                        forge_provider_api::error::ProviderOperation::ThreadOpen,
                        forge_provider_api::error::ProviderFailureKind::Rejected,
                    ));
                }
                *self.client.lock().await = Some(RuntimeClient::Unvalidated(started.clone()));
                started
            }
        };
        let validation = match validate_version(&transport.inner, &self.command).await {
            Ok(validation) => validation,
            Err(error) => {
                transport.force_stop();
                self.client.lock().await.take();
                return Err(error);
            }
        };
        let mut state = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) {
            transport.force_stop();
            state.take();
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        let client = transport;
        *state = Some(RuntimeClient::Ready {
            client: client.clone(),
            _validation: validation,
        });
        Ok(client)
    }

    /// Identify the failed cached client without starting or replacing any native work.
    pub async fn failed_generation(&self) -> Option<super::client_generation::FailedGeneration> {
        let client = self.client.lock().await.clone();
        match client {
            Some(RuntimeClient::Ready { client, .. }) => {
                client.generation.failed_after_fencing().await
            }
            Some(RuntimeClient::Unvalidated(_)) | None => None,
        }
    }

    /// Collect only the failed cached client after its affected callback owners are fenced.
    pub async fn collect_failed_client(
        &self,
        failed: &super::client_generation::FailedGeneration,
    ) -> Result<super::client_generation::CollectedClient, ProviderError> {
        let current = {
            let _initializing = self.initializing.lock().await;
            self.client.lock().await.clone()
        };
        match current {
            Some(RuntimeClient::Ready { client, .. }) if failed.matches(&client.generation) => {
                client.generation.collect(failed).await
            }
            Some(RuntimeClient::Ready { .. }) | Some(RuntimeClient::Unvalidated(_)) | None => {
                Err(provider_failure(
                    forge_provider_api::error::ProviderOperation::InputReconcile,
                    forge_provider_api::error::ProviderFailureKind::Rejected,
                ))
            }
        }
    }

    /// Retire the stopped cache entry only after all of its registered session work is joined.
    pub async fn finish_recovery(
        &self,
        sessions: super::client_generation::CollectedSessions,
    ) -> Result<(), ProviderError> {
        let _initializing = self.initializing.lock().await;
        let mut current = self.client.lock().await;
        match current.as_ref() {
            Some(RuntimeClient::Ready { client, .. })
                if sessions.failed_generation().matches(&client.generation) =>
            {
                current.take();
                Ok(())
            }
            Some(RuntimeClient::Ready { .. }) | Some(RuntimeClient::Unvalidated(_)) | None => {
                Err(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Rejected,
                ))
            }
        }
    }

    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.close_admission();
        let _initializing = self.initializing.lock().await;
        let mut shutdown = self.shutdown.lock().await;
        if matches!(*shutdown, Shutdown::Idle | Shutdown::Complete(Err(_))) {
            let client = self.client.lock().await.clone();
            *shutdown = Shutdown::Running(tokio::spawn(async move {
                match client {
                    Some(client) => client.client().stop().await.map_err(|_| {
                        std::sync::Arc::new(provider_failure(
                            forge_provider_api::error::ProviderOperation::Shutdown,
                            forge_provider_api::error::ProviderFailureKind::Transport,
                        ))
                    }),
                    None => Ok(()),
                }
            }));
        }
        if let Shutdown::Running(task) = &mut *shutdown {
            let result = task.await.unwrap_or_else(|_| {
                Err(std::sync::Arc::new(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                )))
            });
            *shutdown = Shutdown::Complete(result);
        }
        if let Shutdown::Complete(Err(error)) = &*shutdown {
            let kind = match error.as_ref() {
                ProviderError::Failure(failure) => failure.kind(),
                _ => forge_provider_api::error::ProviderFailureKind::Unknown,
            };
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::Shutdown,
                kind,
            ));
        }
        self.client.lock().await.take();
        Ok(())
    }

    /// Create an independent cleanup owner using the same native command.
    ///
    /// The caller must collect this owner's client even when history deletion fails.
    pub(super) fn cleanup_runtime(&self) -> Self {
        Self::new(self.command.clone())
    }
}

fn provider_failure(
    operation: forge_provider_api::error::ProviderOperation,
    kind: forge_provider_api::error::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
        operation, kind,
    ))
}

/// Validate the actual native client before retained or isolated session creation.
pub(crate) async fn validate_version(
    client: &Client,
    command: &[String],
) -> Result<forge_provider_api::version::ValidatedRuntime, ProviderError> {
    let requirement = forge_provider_api::version::RuntimeRequirement::new(
        crate::registry::select("copilot").map_err(|_| {
            ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Unknown,
            ))
        })?,
        command
            .first()
            .map(PathBuf::from)
            .unwrap_or_else(|| "SDK-managed Copilot".into()),
        semver::Version::new(1, 0, 103),
    );
    let status = super::operation::wait(client.call("status.get", None))
        .await?
        .map_err(|_| {
            requirement.probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable)
        })?;
    Ok(requirement.validate(status.get("version").and_then(serde_json::Value::as_str))?)
}
