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

#[cfg(test)]
mod tests {
    use super::CodexRuntime;

    #[cfg(windows)]
    fn process_alive(process_id: u32) -> bool {
        use windows::Win32::Foundation::CloseHandle;
        use windows::Win32::System::Threading::{
            GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION,
        };
        // SAFETY: The query-only handle is closed after reading its exit code and never transferred.
        unsafe {
            let Ok(handle) = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, process_id)
            else {
                return false;
            };
            let mut exit_code = 0;
            let running = GetExitCodeProcess(handle, &mut exit_code).is_ok() && exit_code == 259;
            let _ = CloseHandle(handle);
            running
        }
    }

    #[cfg(windows)]
    fn descendant_runtime(directory: &std::path::Path) -> CodexRuntime {
        let parent = directory.join("parent.nu");
        let child = directory.join("child.nu");
        std::fs::write(&parent, "def main [child_script: string, pid_file: string, --listen: string] { ^nu --no-config-file $child_script $pid_file }").unwrap();
        std::fs::write(&child, "def main [pid_file: string] { $nu.pid | into string | save -f $pid_file; sleep 30sec }").unwrap();
        CodexRuntime::new(vec![
            "nu".into(),
            "--no-config-file".into(),
            parent.display().to_string(),
            child.display().to_string(),
            directory.join("descendant.pid").display().to_string(),
        ])
    }

    #[cfg(windows)]
    async fn read_process_id(path: &std::path::Path) -> u32 {
        async {
            loop {
                if let Ok(text) = std::fs::read_to_string(path)
                    && let Ok(process_id) = text.trim().parse()
                {
                    break process_id;
                }
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        }
        .await
    }

    #[cfg(windows)]
    #[tokio::test]
    #[ignore = "subprocess entry point for abrupt-owner collection"]
    async fn owned_host_fixture() {
        let directory =
            std::path::PathBuf::from(std::env::var_os("FORGE_CODEX_OWNER_FIXTURE").unwrap());
        let runtime = descendant_runtime(&directory);
        *runtime.server.lock().await = Some(runtime.start().unwrap());
        std::fs::write(
            directory.join("provider.pid"),
            runtime.process_id().await.unwrap().to_string(),
        )
        .unwrap();
        tokio::time::sleep(std::time::Duration::from_secs(30)).await;
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn abrupt_owner_exit_collects_descendants_and_releases_stderr() {
        let directory = tempfile::tempdir().unwrap();
        let mut owner = tokio::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "codex::runtime::tests::owned_host_fixture",
                "--ignored",
                "--nocapture",
            ])
            .env("FORGE_CODEX_OWNER_FIXTURE", directory.path())
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let provider = read_process_id(&directory.path().join("provider.pid")).await;
        let descendant = read_process_id(&directory.path().join("descendant.pid")).await;
        assert!(process_alive(provider) && process_alive(descendant));
        owner.start_kill().unwrap();
        owner.wait_with_output().await.unwrap();
        async {
            while process_alive(provider) || process_alive(descendant) {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        }
        .await;
    }
}
