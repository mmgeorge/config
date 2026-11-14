use super::{AdapterError, RunExit};
use std::{
    path::{Path, PathBuf},
    process::Stdio,
};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    sync::{mpsc, watch},
    task::JoinHandle,
    time::Instant,
};

const READ_SIZE: usize = 8192;
const LINE_LIMIT: usize = 8 * 1024 * 1024;
const STDERR_LIMIT: usize = 16 * 1024;

/// Launch child trees with one command's cancellation, absolute deadline, and log.
pub struct AdapterProcesses {
    cancelled: watch::Receiver<bool>,
    deadline: Instant,
    log: PathBuf,
    owner: Option<std::sync::Arc<dyn forge_shell::process::ProcessOwner>>,
}

impl AdapterProcesses {
    /// Create a launcher from the supervisor's execution resources.
    pub fn new(cancelled: watch::Receiver<bool>, deadline: Instant, log: PathBuf) -> Self {
        Self {
            cancelled,
            deadline,
            log,
            owner: None,
        }
    }

    /// Attach the admitted source owner before any native process can start.
    pub(crate) fn owner_bind(
        mut self,
        owner: Option<std::sync::Arc<dyn forge_shell::process::ProcessOwner>>,
    ) -> Self {
        self.owner = owner;
        self
    }

    /// Append adapter event output to the command log owned by this launcher.
    pub(super) async fn log_append(&self, text: &str) -> Result<(), AdapterError> {
        let mut log = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log)
            .await
            .map_err(AdapterError::Storage)?;
        log.write_all(text.as_bytes())
            .await
            .map_err(AdapterError::Storage)?;
        log.flush().await.map_err(AdapterError::Storage)
    }

    /// Start an owned process with concurrent stdin delivery and stdout/stderr drainage.
    pub async fn spawn(&self, spec: ProcessSpec<'_>) -> Result<AdapterProcess, AdapterError> {
        if *self.cancelled.borrow() {
            return Err(AdapterError::Cancelled);
        }
        if Instant::now() >= self.deadline {
            return Err(AdapterError::Timeout);
        }
        let mut log = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.log)
            .await
            .map_err(AdapterError::Storage)?;
        let mut stdout_log = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(self.log.with_extension("stdout"))
            .await
            .map_err(AdapterError::Storage)?;
        let mut stderr_log = tokio::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(self.log.with_extension("stderr"))
            .await
            .map_err(AdapterError::Storage)?;
        let mut command = crate::process::command(spec.program, spec.args);
        command
            .current_dir(spec.directory)
            .envs(spec.env.iter().cloned())
            .stdin(if spec.stdin.is_some() {
                Stdio::piped()
            } else {
                Stdio::null()
            })
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        // Opening logs may consume the remaining budget; never launch after that boundary.
        if *self.cancelled.borrow() {
            return Err(AdapterError::Cancelled);
        }
        if Instant::now() >= self.deadline {
            return Err(AdapterError::Timeout);
        }
        let owner = self.owner.clone();
        let mut child = match &owner {
            Some(owner) => owner.spawn(command),
            None => crate::process::spawn_owned(command),
        }
        .map_err(AdapterError::Launch)?;
        let stdin = child.stdin().take();
        let mut stdout = child
            .stdout()
            .take()
            .ok_or_else(|| AdapterError::Protocol("process stdout unavailable".into()))?;
        let mut stderr = child
            .stderr()
            .take()
            .ok_or_else(|| AdapterError::Protocol("process stderr unavailable".into()))?;
        let request = spec.stdin;
        let log_stdout = spec.log_stdout;
        let mut cancelled = self.cancelled.clone();
        let deadline = self.deadline;
        let (stop, mut stopped) = watch::channel(false);
        let (send, recv) = mpsc::channel(16);
        let worker = tokio::spawn(async move {
            let input = async move {
                if let (Some(mut stdin), Some(request)) = (stdin, request) {
                    stdin.write_all(&request).await?;
                    stdin.shutdown().await?;
                }
                Ok::<_, std::io::Error>(())
            };
            tokio::pin!(input);
            let operation = async {
                let mut stdout_buffer = [0; READ_SIZE];
                let mut stderr_buffer = [0; READ_SIZE];
                let mut stderr_tail = StderrTail::default();
                let mut stdout_done = false;
                let mut stderr_done = false;
                let mut input_done = false;
                let mut status = None;
                let mut pending = None;
                while !stdout_done
                    || !stderr_done
                    || !input_done
                    || status.is_none()
                    || pending.is_some()
                {
                    tokio::select! {
                        biased;
                        _ = cancellation(&mut cancelled, &mut stopped) => return Err(AdapterError::Cancelled),
                        _ = tokio::time::sleep_until(deadline) => return Err(AdapterError::Timeout),
                        result = &mut input, if !input_done => {
                            result.map_err(AdapterError::Process)?;
                            input_done = true;
                        }
                        permit = send.reserve(), if pending.is_some() => {
                            permit.map_err(|_| AdapterError::Cancelled)?.send(pending.take().expect("pending stdout chunk"));
                        }
                        result = stdout.read(&mut stdout_buffer), if !stdout_done && pending.is_none() => {
                            let count = result.map_err(AdapterError::Process)?;
                            stdout_done = count == 0;
                            if count > 0 {
                                stdout_log.write_all(&stdout_buffer[..count]).await.map_err(AdapterError::Storage)?;
                                if log_stdout { log.write_all(&stdout_buffer[..count]).await.map_err(AdapterError::Storage)?; }
                                pending = Some(stdout_buffer[..count].to_vec());
                            }
                        }
                        result = stderr.read(&mut stderr_buffer), if !stderr_done => {
                            let count = result.map_err(AdapterError::Process)?;
                            stderr_done = count == 0;
                            if count > 0 {
                                stderr_tail.append(&stderr_buffer[..count]);
                                stderr_log.write_all(&stderr_buffer[..count]).await.map_err(AdapterError::Storage)?;
                                log.write_all(&stderr_buffer[..count]).await.map_err(AdapterError::Storage)?;
                            }
                        }
                        result = child.wait(), if status.is_none() => {
                            status = Some(result.map_err(AdapterError::Process)?);
                        }
                    }
                }
                log.flush().await.map_err(AdapterError::Storage)?;
                stdout_log.flush().await.map_err(AdapterError::Storage)?;
                stderr_log.flush().await.map_err(AdapterError::Storage)?;
                let status = status.expect("process exit collected before loop finishes");
                Ok(ProcessOutput {
                    exit: RunExit {
                        code: status.code(),
                        success: status.success(),
                    },
                    stderr_tail,
                })
            };
            let result = operation.await;
            if result.is_err() {
                // Collect the owned tree before exposing an interrupted or failed command.
                child.start_kill().map_err(AdapterError::Process)?;
                child.wait().await.map_err(AdapterError::Process)?;
                let _ = log.flush().await;
            }
            drop(owner);
            result
        });
        Ok(AdapterProcess {
            worker: Some(worker),
            stop,
            stdout: recv,
            pending: Vec::new(),
        })
    }
}

/// Supply fixed argv and protocol input for an owned adapter invocation.
pub struct ProcessSpec<'a> {
    pub program: &'a str,
    pub args: &'a [String],
    pub directory: &'a Path,
    pub env: &'a [(String, String)],
    pub stdin: Option<Vec<u8>>,
    pub log_stdout: bool,
}

/// Own a running process; dropping it requests tree cleanup by its worker.
pub struct AdapterProcess {
    worker: Option<JoinHandle<Result<ProcessOutput, AdapterError>>>,
    stop: watch::Sender<bool>,
    stdout: mpsc::Receiver<Vec<u8>>,
    pending: Vec<u8>,
}

impl AdapterProcess {
    /// Read one UTF-8 stdout line, bounded independently of command log retention.
    pub async fn next_line(&mut self) -> Result<Option<String>, AdapterError> {
        loop {
            if let Some(end) = self.pending.iter().position(|byte| *byte == b'\n') {
                if end > LINE_LIMIT {
                    return Err(AdapterError::Protocol(
                        "process stdout line exceeds 8 MiB".into(),
                    ));
                }
                let mut bytes: Vec<_> = self.pending.drain(..=end).collect();
                bytes.pop();
                if bytes.last() == Some(&b'\r') {
                    bytes.pop();
                }
                return String::from_utf8(bytes).map(Some).map_err(|error| {
                    AdapterError::Protocol(format!("process stdout is not UTF-8: {error}"))
                });
            }
            if self.pending.len() > LINE_LIMIT {
                return Err(AdapterError::Protocol(
                    "process stdout line exceeds 8 MiB".into(),
                ));
            }
            match self.stdout.recv().await {
                Some(bytes) => self.pending.extend(bytes),
                None if self.pending.is_empty() => return Ok(None),
                None => {
                    return String::from_utf8(std::mem::take(&mut self.pending))
                        .map(Some)
                        .map_err(|error| {
                            AdapterError::Protocol(format!("process stdout is not UTF-8: {error}"))
                        });
                }
            }
        }
    }

    /// Request cancellation; `wait` still collects the process tree before returning.
    pub fn cancel(&self) {
        self.stop.send_replace(true);
    }

    /// Drain remaining stdout and collect the process result exactly once.
    pub async fn wait(&mut self) -> Result<RunExit, AdapterError> {
        self.collect().await.map(|output| output.exit)
    }

    /// Drain stdout and collect the exit and this process's bounded stderr tail exactly once.
    pub(super) async fn collect(&mut self) -> Result<ProcessOutput, AdapterError> {
        while self.stdout.recv().await.is_some() {}
        let worker = self
            .worker
            .as_mut()
            .ok_or_else(|| AdapterError::Protocol("process already collected".into()))?;
        let result = worker
            .await
            .map_err(|error| AdapterError::Protocol(format!("process worker failed: {error}")))?;
        self.worker = None;
        result
    }
}

/// Retain an invocation's exit and diagnostic output separately from the shared command log.
pub(super) struct ProcessOutput {
    pub exit: RunExit,
    stderr_tail: StderrTail,
}

impl ProcessOutput {
    /// Get diagnostic text with explicit omitted-byte accounting and valid UTF-8.
    pub(super) fn stderr(&self) -> String {
        self.stderr_tail.display()
    }
}

#[derive(Default)]
struct StderrTail {
    bytes: Vec<u8>,
    omitted: usize,
}

impl StderrTail {
    /// Retain the last bounded bytes even when one read exceeds the capacity.
    fn append(&mut self, bytes: &[u8]) {
        let overflow = self
            .bytes
            .len()
            .saturating_add(bytes.len())
            .saturating_sub(STDERR_LIMIT);
        self.omitted = self.omitted.saturating_add(overflow);
        if overflow >= self.bytes.len() {
            self.bytes.clear();
            self.bytes
                .extend_from_slice(&bytes[bytes.len().saturating_sub(STDERR_LIMIT)..]);
        } else {
            self.bytes.drain(..overflow);
            self.bytes.extend_from_slice(bytes);
        }
    }

    /// Render arbitrary process bytes without allowing invalid UTF-8 through the transport.
    fn display(&self) -> String {
        let text = String::from_utf8_lossy(&self.bytes);
        if self.omitted == 0 {
            text.into_owned()
        } else {
            format!(
                "[{} earlier stderr bytes omitted; full output in command log]\n{text}",
                self.omitted
            )
        }
    }
}

impl Drop for AdapterProcess {
    /// Keep cleanup active even if the adapter future is abandoned.
    fn drop(&mut self) {
        self.stop.send_replace(true);
    }
}

/// Observe owner cancellation or an abandoned process handle without polling.
async fn cancellation(cancelled: &mut watch::Receiver<bool>, stopped: &mut watch::Receiver<bool>) {
    loop {
        if *cancelled.borrow_and_update() || *stopped.borrow_and_update() {
            return;
        }
        tokio::select! {
            result = cancelled.changed() => if result.is_err() { return; },
            result = stopped.changed() => if result.is_err() { return; },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep only this invocation's latest diagnostics and count discarded raw bytes.
    #[test]
    fn stderr_tail_bounds_bytes_and_handles_split_utf8() {
        let mut tail = StderrTail::default();
        for byte in "é\n".as_bytes() {
            tail.append(&[*byte]);
        }
        assert_eq!(tail.display(), "é\n");
        tail.append(&vec![b'x'; STDERR_LIMIT + 7]);
        assert_eq!(tail.bytes.len(), STDERR_LIMIT);
        assert_eq!(tail.omitted, 10);
        tail.append("é".as_bytes());
        assert_eq!(tail.omitted, 12);
        assert!(tail.display().ends_with('é'));
        tail.append(&[0xff]);
        assert!(tail.display().ends_with('\u{fffd}'));
    }

    /// Collect stderr and exit once without spawning an operating-system process.
    #[tokio::test]
    async fn collection_retains_stderr_and_rejects_a_second_wait() {
        let mut process = output(vec![b"unconsumed stdout".to_vec()]);
        process.worker = Some(tokio::spawn(async {
            let mut stderr_tail = StderrTail::default();
            stderr_tail.append(b"compiler failure");
            Ok(ProcessOutput {
                exit: RunExit {
                    code: Some(101),
                    success: false,
                },
                stderr_tail,
            })
        }));
        let collected = process.collect().await.unwrap();
        assert_eq!(collected.exit.code, Some(101));
        assert_eq!(collected.stderr(), "compiler failure");
        assert!(matches!(
            process.wait().await,
            Err(AdapterError::Protocol(_))
        ));
    }

    /// Assemble ordinary process output state without starting an operating-system process.
    fn output(chunks: Vec<Vec<u8>>) -> AdapterProcess {
        let (sender, stdout) = mpsc::channel(16);
        for chunk in chunks {
            sender.try_send(chunk).unwrap();
        }
        drop(sender);
        let (stop, _) = watch::channel(false);
        AdapterProcess {
            worker: None,
            stop,
            stdout,
            pending: Vec::new(),
        }
    }

    #[tokio::test]
    async fn stdout_preserves_split_utf8_crlf_and_final_unterminated_line() {
        let bytes = "é\r\nnext\nfinal".as_bytes();
        let mut process = output(bytes.iter().map(|byte| vec![*byte]).collect());
        assert_eq!(process.next_line().await.unwrap().as_deref(), Some("é"));
        assert_eq!(process.next_line().await.unwrap().as_deref(), Some("next"));
        assert_eq!(process.next_line().await.unwrap().as_deref(), Some("final"));
        assert_eq!(process.next_line().await.unwrap(), None);
    }

    #[tokio::test]
    async fn stdout_rejects_invalid_utf8_and_oversized_lines() {
        let mut invalid = output(vec![vec![0xff, b'\n']]);
        assert!(matches!(
            invalid.next_line().await,
            Err(AdapterError::Protocol(_))
        ));
        let mut oversized = output(vec![vec![b'x'; LINE_LIMIT + 1]]);
        assert!(matches!(
            oversized.next_line().await,
            Err(AdapterError::Protocol(_))
        ));
    }

    #[tokio::test]
    async fn cancellation_observes_already_requested_and_later_owner_stop() {
        let (sender, mut receiver) = watch::channel(true);
        let (_stop, mut stopped) = watch::channel(false);
        cancellation(&mut receiver, &mut stopped).await;
        sender.send_replace(false);
        let (stop, mut stopped) = watch::channel(false);
        let cancelled = cancellation(&mut receiver, &mut stopped);
        tokio::pin!(cancelled);
        tokio::select! { biased; _ = &mut cancelled => panic!("owner has not cancelled"), _ = std::future::ready(()) => {} }
        stop.send_replace(true);
        cancelled.await;
    }
}
