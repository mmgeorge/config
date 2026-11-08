use super::connection::CodexConnection;
use anyhow::{Context, Result};
use forge_shell::process;
use process_wrap::tokio::ChildWrapper;
use std::net::TcpListener;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Duration;
use tokio::io::AsyncReadExt;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

struct CodexAppServer {
    child: Box<dyn ChildWrapper>,
    endpoint: String,
    stderr: Arc<std::sync::Mutex<Vec<u8>>>,
    stderr_task: Option<JoinHandle<()>>,
}

impl CodexAppServer {
    /// Stop the native process and join its owned diagnostic reader before releasing the server.
    async fn collect(&mut self) -> Result<()> {
        self.child
            .start_kill()
            .context("terminate shared Codex app-server")?;
        self.child
            .wait()
            .await
            .context("collect shared Codex app-server")?;
        if let Some(task) = self.stderr_task.as_mut() {
            let result = task.await;
            self.stderr_task.take();
            result.context("collect Codex diagnostic reader")?;
        }
        Ok(())
    }

    fn diagnostic(&self) -> String {
        let bytes: Vec<u8> = self
            .stderr
            .lock()
            .unwrap_or_else(|error| error.into_inner())
            .iter()
            .copied()
            .collect();
        String::from_utf8_lossy(&bytes).trim().to_owned()
    }
}

impl Drop for CodexAppServer {
    fn drop(&mut self) {
        let _ = self.child.start_kill();
        if let Some(task) = &self.stderr_task {
            task.abort();
        }
    }
}

/// Owns one Codex app-server shared by connections using the same resource configuration.
pub struct CodexRuntime {
    command: Vec<String>,
    isolated_home: Option<std::path::PathBuf>,
    server: Mutex<Option<CodexAppServer>>,
    start_count: AtomicU64,
    closed: AtomicBool,
    #[cfg(test)]
    fixture_endpoint: Option<String>,
}

impl CodexRuntime {
    /// Build a lazy provider runtime from the configured Codex command.
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            isolated_home: None,
            server: Mutex::new(None),
            start_count: AtomicU64::new(0),
            closed: AtomicBool::new(false),
            #[cfg(test)]
            fixture_endpoint: None,
        }
    }

    /// Create an exclusive runtime whose home cannot load the user's global instructions or hooks.
    pub(super) fn isolated(command: Vec<String>, home: std::path::PathBuf) -> Self {
        let mut runtime = Self::new(command);
        runtime.isolated_home = Some(home);
        runtime
    }

    /// Connect one session channel without starting another app-server.
    pub async fn connect(&self) -> Result<CodexConnection> {
        let mut server = self.server.lock().await;
        anyhow::ensure!(
            !self.closed.load(Ordering::Acquire),
            "shared Codex runtime is shut down"
        );
        #[cfg(test)]
        if let Some(endpoint) = &self.fixture_endpoint {
            return CodexConnection::connect(endpoint)
                .await
                .map_err(anyhow::Error::from);
        }
        let running = match server.as_mut() {
            Some(server) => server.child.try_wait()?.is_none(),
            None => false,
        };
        if !running {
            if let Some(previous) = server.as_mut() {
                previous.collect().await?;
            }
            *server = Some(self.start()?);
        }
        let endpoint = server
            .as_ref()
            .map(|server| server.endpoint.clone())
            .context("shared Codex app-server did not retain its endpoint")?;
        loop {
            super::operations::check().map_err(anyhow::Error::from)?;
            match CodexConnection::connect(&endpoint)
                .await
                .map_err(anyhow::Error::from)
            {
                Ok(connection) => return Ok(connection),
                Err(error) => {
                    if let Some(running) = server.as_mut()
                        && let Some(status) = running
                            .child
                            .try_wait()
                            .context("collect Codex app-server during startup")?
                    {
                        anyhow::bail!(
                            "shared Codex app-server exited during startup with {status}: {error:#}\n{}",
                            running.diagnostic()
                        );
                    }
                    // The websocket is not listening yet. Retry a failed connection inside
                    // the adapter; established sessions use pushed native observations.
                    tokio::time::sleep(Duration::from_millis(25)).await;
                }
            }
        }
    }

    /// Return the operating-system process identifier for the shared app-server.
    #[cfg(test)]
    pub async fn process_id(&self) -> Option<u32> {
        self.server
            .lock()
            .await
            .as_ref()
            .and_then(|server| server.child.id())
    }

    /// Terminate and collect the shared provider after session work has drained.
    pub async fn shutdown(&self) -> Result<()> {
        self.closed.store(true, Ordering::Release);
        let mut server = self.server.lock().await;
        if let Some(running) = server.as_mut() {
            running.collect().await?;
        }
        *server = None;
        Ok(())
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
            .context("Codex provider launch command is empty")?;
        let mut command = process::command(program, argument_list);
        if let Some(home) = &self.isolated_home {
            command.env("CODEX_HOME", home);
        }
        command
            .arg("--listen")
            .arg(&endpoint)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped());
        let mut child = process::spawn_owned(command)
            .with_context(|| format!("start shared Codex app-server {program}"))?;
        // SAFETY: Taking stderr preserves the process identity and lifecycle owned by the wrappers.
        let mut stderr_pipe = unsafe { child.try_inner_child_mut() }
            .context("owned Codex app-server has no native child")?
            .stderr
            .take()
            .context("owned Codex app-server has no stderr pipe")?;
        let stderr = Arc::new(std::sync::Mutex::new(Vec::new()));
        let stderr_output = Arc::clone(&stderr);
        let stderr_task = tokio::spawn(async move {
            let mut chunk = [0_u8; 4096];
            while let Ok(length) = stderr_pipe.read(&mut chunk).await {
                if length == 0 {
                    break;
                }
                let mut retained = stderr_output
                    .lock()
                    .unwrap_or_else(|error| error.into_inner());
                retained.extend(&chunk[..length]);
            }
        });
        self.start_count.fetch_add(1, Ordering::Relaxed);
        Ok(CodexAppServer {
            child,
            endpoint,
            stderr,
            stderr_task: Some(stderr_task),
        })
    }
}
