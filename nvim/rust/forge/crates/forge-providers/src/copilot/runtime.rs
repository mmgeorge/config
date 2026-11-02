use forge_provider_api::ProviderError;
use github_copilot_sdk::{CliProgram, Client, ClientOptions};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tokio::sync::Mutex;

enum Shutdown {
    Idle,
    Running(tokio::task::JoinHandle<Result<(), String>>),
    Complete(Result<(), String>),
}

/// Owns the native client process and retains its collection task across waiter cancellation.
pub struct CopilotRuntime {
    command: Vec<String>,
    client: Mutex<Option<Client>>,
    starts: AtomicU64,
    closed: AtomicBool,
    shutdown: Mutex<Shutdown>,
}

impl CopilotRuntime {
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            client: Mutex::new(None),
            starts: AtomicU64::new(0),
            closed: AtomicBool::new(false),
            shutdown: Mutex::new(Shutdown::Idle),
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
        let mut client = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) || client.is_some() {
            return Err(ProviderError::Rejected {
                message: "Copilot transport must be attached before runtime startup".into(),
            });
        }
        *client = Some(
            Client::from_streams(reader, writer, workspace).map_err(|error| {
                ProviderError::Transport {
                    message: error.to_string(),
                }
            })?,
        );
        Ok(())
    }

    pub async fn client(
        &self,
        workspace: &str,
    ) -> Result<super::handle::CopilotClient, ProviderError> {
        let mut client = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) {
            return Err(ProviderError::Rejected {
                message: "Copilot runtime is shut down".into(),
            });
        }
        if let Some(client) = client.as_ref() {
            return Ok(super::handle::CopilotClient {
                inner: client.clone(),
            });
        }
        let mut options = ClientOptions::new().with_cwd(workspace);
        if let Some(program) = self.command.first() {
            options = options.with_program(CliProgram::Path(PathBuf::from(program)));
            if self.command.len() > 1 {
                options = options.with_prefix_args(self.command.iter().skip(1));
            }
        }
        let started = Client::start(options).await.map_err(|error| {
            let message = if matches!(error.kind(), github_copilot_sdk::ErrorKind::BinaryNotFound { .. }) {
                format!("Copilot CLI is unavailable. Restore the SDK's pinned CLI cache by rebuilding Forge, or set harness.providers.copilot.command to an installed CLI executable. Explicit configured commands retain precedence: {error}")
            } else { format!("start GitHub Copilot SDK client: {error}") };
            ProviderError::Transport { message }
        })?;
        self.starts.fetch_add(1, Ordering::Relaxed);
        *client = Some(started.clone());
        Ok(super::handle::CopilotClient { inner: started })
    }

    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.close_admission();
        let mut shutdown = self.shutdown.lock().await;
        if matches!(*shutdown, Shutdown::Idle) {
            let client = self.client.lock().await.clone();
            *shutdown = Shutdown::Running(tokio::spawn(async move {
                match client {
                    Some(client) => client.stop().await.map_err(|error| error.to_string()),
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
