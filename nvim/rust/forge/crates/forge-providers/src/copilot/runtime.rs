use forge_provider_api::ProviderError;
use github_copilot_sdk::{CliProgram, Client, ClientOptions};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tokio::sync::Mutex;

use super::handle::CopilotClient;

enum Shutdown {
    Idle,
    Running(tokio::task::JoinHandle<Result<(), String>>),
    Complete(Result<(), String>),
}

#[derive(Clone)]
enum RuntimeClient {
    Unvalidated(CopilotClient),
    Ready {
        client: CopilotClient,
        _validation: forge_provider_api::version::ValidatedRuntime,
    },
}

impl RuntimeClient {
    /// Get the transport for resource collection regardless of validation state.
    fn client(&self) -> &CopilotClient {
        match self {
            Self::Unvalidated(client) | Self::Ready { client, .. } => client,
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
    recovery: Mutex<Shutdown>,
}

impl CopilotRuntime {
    /// Report SDK startup future storage without starting a native client.
    pub fn sdk_start_future_bytes() -> usize {
        fn size<F, Fut>(_: &F) -> usize
        where
            F: FnOnce() -> Fut,
            Fut: std::future::Future,
        {
            std::mem::size_of::<Fut>()
        }
        size(&|| Client::start(ClientOptions::new()))
    }

    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            client: Mutex::new(None),
            initializing: Mutex::new(()),
            starts: AtomicU64::new(0),
            closed: AtomicBool::new(false),
            shutdown: Mutex::new(Shutdown::Idle),
            recovery: Mutex::new(Shutdown::Idle),
        }
    }

    pub fn command(&self) -> &[String] {
        &self.command
    }

    pub fn start_count(&self) -> u64 {
        self.starts.load(Ordering::Relaxed)
    }

    pub fn close_admission(&self) {
        self.closed.store(true, Ordering::Release);
    }

    /// Bind an externally supplied transport before this runtime starts a client.
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
            return Err(ProviderError::Rejected {
                message: "Copilot transport must be attached before runtime startup".into(),
            });
        }
        *client = Some(RuntimeClient::Unvalidated(CopilotClient::new(
            Client::from_streams(reader, writer, workspace).map_err(|error| {
                ProviderError::Transport {
                    message: error.to_string(),
                }
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
            return Err(ProviderError::Rejected {
                message: "Copilot runtime is shut down".into(),
            });
        }
        self.collect_stopped().await?;
        let current = self.client.lock().await.clone();
        let transport = match current {
            Some(RuntimeClient::Ready { client, .. }) => {
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
                let started = CopilotClient::new(Client::start(options).await.map_err(|error| {
                    let message = if matches!(error.kind(), github_copilot_sdk::ErrorKind::BinaryNotFound { .. }) {
                        format!("Copilot CLI is unavailable. Restore the SDK's pinned CLI cache by rebuilding Forge, or set harness.providers.copilot.command to an installed CLI executable: {error}")
                    } else { format!("start GitHub Copilot SDK client: {error}") };
                    ProviderError::Transport { message }
                })?);
                self.starts.fetch_add(1, Ordering::Relaxed);
                if self.closed.load(Ordering::Acquire) {
                    started.force_stop();
                    return Err(ProviderError::Rejected {
                        message: "Copilot runtime closed during startup".into(),
                    });
                }
                *self.client.lock().await = Some(RuntimeClient::Unvalidated(started.clone()));
                started
            }
        };
        let validation = match self.validate_version(&transport.inner).await {
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
            return Err(ProviderError::Rejected {
                message: "Copilot runtime closed during validation".into(),
            });
        }
        *state = Some(RuntimeClient::Ready {
            client: transport.clone(),
            _validation: validation,
        });
        Ok(transport)
    }

    /// Collect a stopped generation before an explicit recovery attaches another transport.
    pub async fn recover(&self) -> Result<(), ProviderError> {
        let _initializing = self.initializing.lock().await;
        self.collect_stopped().await
    }

    /// Release only a force-stopped client after its SDK workers have been collected.
    async fn collect_stopped(&self) -> Result<(), ProviderError> {
        let current = self.client.lock().await.clone();
        if let Some(current) = current
            && current.client().is_stopped()
        {
            let mut recovery = self.recovery.lock().await;
            if matches!(*recovery, Shutdown::Idle | Shutdown::Complete(Err(_))) {
                *recovery = Shutdown::Running(tokio::spawn(async move {
                    current
                        .client()
                        .stop()
                        .await
                        .map_err(|error| error.to_string())
                }));
            }
            if let Shutdown::Running(task) = &mut *recovery {
                let result = task.await.unwrap_or_else(|error| Err(error.to_string()));
                *recovery = Shutdown::Complete(result);
            }
            if let Shutdown::Complete(Err(error)) = &*recovery {
                return Err(ProviderError::Transport {
                    message: format!("collect stopped Copilot runtime: {error}"),
                });
            }
            self.client.lock().await.take();
            *recovery = Shutdown::Idle;
        }
        Ok(())
    }

    /// Validate the actual process before decoding optional status metadata.
    async fn validate_version(
        &self,
        client: &Client,
    ) -> Result<forge_provider_api::version::ValidatedRuntime, ProviderError> {
        let status =
            client
                .call("status.get", None)
                .await
                .map_err(|error| ProviderError::Transport {
                    message: format!("query Copilot runtime version: {error}"),
                })?;
        let requirement = forge_provider_api::version::RuntimeRequirement::new(
            forge_provider_api::ProviderKind::Copilot,
            self.command
                .first()
                .map(PathBuf::from)
                .unwrap_or_else(|| "SDK-managed Copilot".into()),
            semver::Version::new(1, 0, 103),
        );
        Ok(requirement.validate(status.get("version").and_then(serde_json::Value::as_str))?)
    }

    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.close_admission();
        let _initializing = self.initializing.lock().await;
        self.collect_stopped().await?;
        let mut shutdown = self.shutdown.lock().await;
        if matches!(*shutdown, Shutdown::Idle | Shutdown::Complete(Err(_))) {
            let client = self.client.lock().await.clone();
            *shutdown = Shutdown::Running(tokio::spawn(async move {
                match client {
                    Some(client) => client
                        .client()
                        .stop()
                        .await
                        .map_err(|error| error.to_string()),
                    None => Ok(()),
                }
            }));
        }
        if let Shutdown::Running(task) = &mut *shutdown {
            let result = task.await.unwrap_or_else(|error| Err(error.to_string()));
            *shutdown = Shutdown::Complete(result);
        }
        if let Shutdown::Complete(Err(error)) = &*shutdown {
            return Err(ProviderError::Transport {
                message: format!("Copilot runtime collection failed: {error}"),
            });
        }
        self.client.lock().await.take();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Prevent retained session handles from issuing detach RPCs after successful shutdown.
    #[tokio::test]
    async fn shutdown_marks_collected_generation_unavailable() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (reader, _peer_writer) = tokio::io::duplex(64);
        let (_peer_reader, writer) = tokio::io::duplex(64);
        runtime
            .attach_streams(reader, writer, PathBuf::from("."))
            .await
            .unwrap();
        let client = runtime
            .client
            .lock()
            .await
            .as_ref()
            .unwrap()
            .client()
            .clone();
        runtime.shutdown().await.unwrap();
        assert!(client.is_stopped());
        assert!(runtime.client.lock().await.is_none());
    }

    /// Keep stopped-generation collection alive when its first recovery waiter disappears.
    #[tokio::test]
    async fn cancelled_recovery_retains_collection_until_shutdown() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (reader, _writer) = tokio::io::duplex(64);
        let (_reader, writer) = tokio::io::duplex(64);
        runtime
            .attach_streams(reader, writer, PathBuf::from("."))
            .await
            .unwrap();
        runtime
            .client
            .lock()
            .await
            .as_ref()
            .unwrap()
            .client()
            .force_stop();
        let (release, released) = tokio::sync::oneshot::channel();
        *runtime.recovery.lock().await = Shutdown::Running(tokio::spawn(async move {
            released.await.map_err(|error| error.to_string())
        }));
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(1), runtime.recover())
                .await
                .is_err()
        );
        assert!(matches!(
            *runtime.recovery.lock().await,
            Shutdown::Running(_)
        ));
        assert!(runtime.client.lock().await.is_some());
        release.send(()).unwrap();
        runtime.shutdown().await.unwrap();
        assert!(runtime.client.lock().await.is_none());
    }

    /// Retry a failed stop instead of caching its error forever.
    #[tokio::test]
    async fn failed_collection_can_be_retried_without_starting_a_client() {
        let runtime = CopilotRuntime::new(Vec::new());
        *runtime.shutdown.lock().await = Shutdown::Complete(Err("prior stop failed".into()));
        runtime.shutdown().await.unwrap();
        assert!(matches!(
            *runtime.shutdown.lock().await,
            Shutdown::Complete(Ok(()))
        ));
        assert_eq!(runtime.start_count(), 0);
    }

    #[tokio::test]
    async fn shutdown_waiter_cancellation_retains_the_same_collection_task() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (release, released) = tokio::sync::oneshot::channel();
        *runtime.shutdown.lock().await = Shutdown::Running(tokio::spawn(async move {
            released.await.map_err(|error| error.to_string())
        }));
        assert!(
            tokio::time::timeout(std::time::Duration::ZERO, runtime.shutdown())
                .await
                .is_err()
        );
        assert!(matches!(
            *runtime.shutdown.lock().await,
            Shutdown::Running(_)
        ));
        release.send(()).unwrap();
        runtime.shutdown().await.unwrap();
        runtime.shutdown().await.unwrap();
        assert_eq!(runtime.start_count(), 0);
        assert!(runtime.client(".").await.is_err());
    }
}
