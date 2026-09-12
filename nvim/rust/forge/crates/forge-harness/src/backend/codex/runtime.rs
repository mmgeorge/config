use super::json_rpc::CodexJsonRpc;
use super::process;
use crate::backend::BackendEventSink;
use crate::backend::approval::PermissionCoordinator;
use crate::session::ExecutionMode;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use process_wrap::tokio::ChildWrapper;
use std::collections::VecDeque;
use std::net::TcpListener;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tokio::io::AsyncReadExt;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;

const STDERR_LIMIT: usize = 16 * 1024;

struct CodexAppServer {
    child: Box<dyn ChildWrapper>,
    endpoint: String,
    stderr: Arc<std::sync::Mutex<VecDeque<u8>>>,
    stderr_task: JoinHandle<()>,
}

impl CodexAppServer {
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
        self.stderr_task.abort();
    }
}

/// Owns the single Codex app-server shared by every Harness session.
pub struct CodexRuntime {
    command: Vec<String>,
    server: Mutex<Option<CodexAppServer>>,
    start_count: AtomicU64,
    closed: AtomicBool,
}

impl CodexRuntime {
    /// Build a lazy provider runtime from the configured Codex command.
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            server: Mutex::new(None),
            start_count: AtomicU64::new(0),
            closed: AtomicBool::new(false),
        }
    }

    /// Connect one session channel without starting another app-server.
    pub async fn connect(
        &self,
        workspace: &str,
        execution_mode: ExecutionMode,
        permission_coordinator: Arc<PermissionCoordinator>,
        event_sink: Option<BackendEventSink>,
        trace: Arc<TraceStore>,
        session_id: String,
    ) -> Result<CodexJsonRpc> {
        let mut server = self.server.lock().await;
        anyhow::ensure!(
            !self.closed.load(Ordering::Acquire),
            "shared Codex runtime is shut down"
        );
        let running = match server.as_mut() {
            Some(server) => server.child.try_wait()?.is_none(),
            None => false,
        };
        if !running {
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
                Arc::clone(&trace),
                session_id.clone(),
            )
            .await
            {
                Ok(connection) => return Ok(connection),
                Err(error) if started.elapsed() < Duration::from_secs(5) => {
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
                    tokio::time::sleep(Duration::from_millis(25)).await;
                }
                Err(error) => {
                    return Err(error.context(format!(
                        "connect shared Codex app-server: {}",
                        server
                            .as_ref()
                            .map(CodexAppServer::diagnostic)
                            .unwrap_or_default()
                    )));
                }
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

    /// Terminate and collect the shared provider after session work has drained.
    pub async fn shutdown(&self) -> Result<()> {
        self.closed.store(true, Ordering::Release);
        let mut server = self.server.lock().await;
        if let Some(running) = server.as_mut() {
            running
                .child
                .start_kill()
                .context("terminate shared Codex app-server")?;
            running
                .child
                .wait()
                .await
                .context("collect shared Codex app-server")?;
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
            .context("Codex backend launch command is empty")?;
        let mut command = process::command(program, argument_list);
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
        let stderr = Arc::new(std::sync::Mutex::new(VecDeque::with_capacity(STDERR_LIMIT)));
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
                let overflow = (retained.len() + length).saturating_sub(STDERR_LIMIT);
                retained.drain(..overflow);
                retained.extend(&chunk[..length]);
            }
        });
        self.start_count.fetch_add(1, Ordering::Relaxed);
        Ok(CodexAppServer {
            child,
            endpoint,
            stderr,
            stderr_task,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{CodexAppServer, CodexRuntime};
    use std::sync::atomic::Ordering;

    #[tokio::test]
    async fn stderr_is_privately_drained_with_a_bounded_tail() {
        let directory = tempfile::tempdir().unwrap();
        let script = directory.path().join("stderr.nu");
        std::fs::write(&script, "def main [--listen: string] { print -e ('x' | fill -w 40000); print -e 'final stderr diagnostic'; sleep 30sec }").unwrap();
        let runtime = CodexRuntime::new(vec![
            "nu".into(),
            "--no-config-file".into(),
            script.display().to_string(),
        ]);
        let server = runtime.start().unwrap();
        let collector = server.stderr_task.abort_handle();
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            while !server.diagnostic().ends_with("final stderr diagnostic") {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await
        .unwrap();
        assert_eq!(server.stderr.lock().unwrap().len(), super::STDERR_LIMIT);
        *runtime.server.lock().await = Some(server);
        runtime.shutdown().await.unwrap();
        tokio::task::yield_now().await;
        assert!(collector.is_finished());
    }

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
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                if let Ok(text) = std::fs::read_to_string(path)
                    && let Ok(process_id) = text.trim().parse()
                {
                    break process_id;
                }
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await
        .unwrap()
    }

    #[cfg(windows)]
    #[tokio::test]
    async fn shutdown_collects_provider_descendants() {
        let directory = tempfile::tempdir().unwrap();
        let runtime = descendant_runtime(directory.path());
        *runtime.server.lock().await = Some(runtime.start().unwrap());
        let provider = runtime.process_id().await.unwrap();
        let descendant = read_process_id(&directory.path().join("descendant.pid")).await;
        assert!(process_alive(provider) && process_alive(descendant));
        tokio::time::timeout(std::time::Duration::from_secs(5), runtime.shutdown())
            .await
            .unwrap()
            .unwrap();
        assert!(!process_alive(provider) && !process_alive(descendant));
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
                "backend::codex::runtime::tests::owned_host_fixture",
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
        tokio::time::timeout(std::time::Duration::from_secs(5), owner.wait_with_output())
            .await
            .expect("provider inherited the owner's output pipe")
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            while process_alive(provider) || process_alive(descendant) {
                tokio::time::sleep(std::time::Duration::from_millis(10)).await;
            }
        })
        .await
        .expect("provider descendants outlived their owner's job handle");
    }

    #[tokio::test]
    async fn shutdown_collects_owned_process_and_remains_closed() {
        let runtime = CodexRuntime::new(Vec::new());
        let mut command = tokio::process::Command::new("nu");
        command.args(["--no-config-file", "-c", "sleep 30sec"]);
        let child = super::process::spawn_owned(command).unwrap();
        *runtime.server.lock().await = Some(CodexAppServer {
            child,
            endpoint: String::new(),
            stderr: Default::default(),
            stderr_task: tokio::spawn(async {}),
        });
        assert!(runtime.process_id().await.is_some());
        runtime.shutdown().await.unwrap();
        assert!(runtime.process_id().await.is_none());
        assert!(runtime.closed.load(Ordering::Acquire));
        runtime.shutdown().await.unwrap();
    }
}
