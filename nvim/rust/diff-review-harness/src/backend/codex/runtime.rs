use super::json_rpc::CodexJsonRpc;
use super::process;
use crate::backend::BackendEventSink;
use crate::backend::approval::PermissionCoordinator;
use crate::session::ExecutionMode;
use anyhow::{Context, Result};
use std::net::TcpListener;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tokio::process::Child;
use tokio::sync::Mutex;

struct CodexAppServer {
    child: Child,
    endpoint: String,
}

/// Owns the single Codex app-server shared by every Harness session.
pub struct CodexRuntime {
    command: Vec<String>,
    server: Mutex<Option<CodexAppServer>>,
    start_count: AtomicU64,
}

impl CodexRuntime {
    /// Build a lazy provider runtime from the configured Codex command.
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            server: Mutex::new(None),
            start_count: AtomicU64::new(0),
        }
    }

    /// Connect one session channel without starting another app-server.
    pub async fn connect(
        &self,
        workspace: &str,
        execution_mode: ExecutionMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<BackendEventSink>,
    ) -> Result<CodexJsonRpc> {
        let mut server = self.server.lock().await;
        let running = match server.as_mut() {
            Some(server) => server.child.try_wait()?.is_none(),
            None => false,
        };
        if !running {
            if let Some(mut stopped) = server.take() {
                let _ = stopped.child.start_kill();
                let _ = stopped.child.wait().await;
            }
            *server = Some(self.start()?);
        }
        let endpoint = server
            .as_ref()
            .map(|server| server.endpoint.clone())
            .context("shared Codex app-server did not retain its endpoint")?;
        let started = Instant::now();
        loop {
            match CodexJsonRpc::connect(
                &endpoint,
                workspace,
                execution_mode,
                Arc::clone(&permission_coordinator),
                event_sink.clone(),
            )
            .await
            {
                Ok(connection) => return Ok(connection),
                Err(error) if started.elapsed() < Duration::from_secs(5) => {
                    if let Some(status) = server
                        .as_mut()
                        .and_then(|server| server.child.try_wait().ok().flatten())
                    {
                        anyhow::bail!(
                            "shared Codex app-server exited during startup with {status}: {error:#}"
                        );
                    }
                    tokio::time::sleep(Duration::from_millis(25)).await;
                }
                Err(error) => return Err(error),
            }
        }
    }

    /// Return how many singleton process generations this runtime launched.
    pub fn start_count(&self) -> u64 {
        self.start_count.load(Ordering::Relaxed)
    }

    /// Return the operating-system process identifier for the shared app-server.
    pub async fn process_id(&self) -> Option<u32> {
        self.server
            .lock()
            .await
            .as_ref()
            .and_then(|server| server.child.id())
    }

    fn start(&self) -> Result<CodexAppServer> {
        let listener = TcpListener::bind(("127.0.0.1", 0))
            .context("reserve a loopback port for the shared Codex app-server")?;
        let address = listener.local_addr()?;
        drop(listener);
        let endpoint = format!("ws://{address}");
        let (program, argument_list) = self
            .command
            .split_first()
            .context("Codex backend launch command is empty")?;
        let mut command = process::command(program, argument_list);
        command
            .arg("--listen")
            .arg(&endpoint)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::inherit())
            .kill_on_drop(true);
        let child = command
            .spawn()
            .with_context(|| format!("start shared Codex app-server {program}"))?;
        self.start_count.fetch_add(1, Ordering::Relaxed);
        Ok(CodexAppServer { child, endpoint })
    }
}

impl Drop for CodexRuntime {
    fn drop(&mut self) {
        if let Some(server) = self.server.get_mut().take() {
            let mut child = server.child;
            let _ = child.start_kill();
        }
    }
}
