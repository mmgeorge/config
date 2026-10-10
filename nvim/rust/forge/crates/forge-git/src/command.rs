//! Bounded native commands own their process group until output collection completes.

use std::io;
use std::process::{Command, ExitStatus, Output, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::{Duration, Instant};

use anyhow::{Context, Result, ensure};
use process_wrap::tokio::{CommandWrap, KillOnDrop};
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};

/// Independent stream capacities and the command deadline.
#[derive(Clone, Copy, Debug)]
pub struct CommandLimits {
    pub stdout_bytes: usize,
    pub stderr_bytes: usize,
    pub timeout: Duration,
}

/// A native output stream.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommandStream {
    Stdout,
    Stderr,
}

/// One bounded progress update from a native command.
#[derive(Debug)]
pub struct CommandProgress {
    pub stream: CommandStream,
    pub sequence: u64,
    pub bytes: Vec<u8>,
}

/// Receives admitted progress chunks.
pub type CommandProgressSink = Arc<dyn Fn(CommandProgress) -> Result<()> + Send + Sync>;

/// Collects a command from a blocking repository worker.
///
/// The caller owns admission and must not call this function on a Tokio executor thread.
/// Cancellation, timeout, and stream failures terminate the group or job before releasing it.
pub fn read_command(
    command: &mut Command,
    limits: CommandLimits,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    progress_command(command, limits, None, None, check)
}

/// Writes input while collecting bounded output.
pub fn write_command(
    command: &mut Command,
    limits: CommandLimits,
    input: &[u8],
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    progress_command(command, limits, Some(input), None, check)
}

/// Publishes bounded chunks in independent stream order.
pub fn progress_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    collect_command(command, limits, input, progress, false, None, check)
}

/// Retains a bounded diagnostic tail while draining both streams to completion.
pub fn diagnostic_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    collect_command(command, limits, input, progress, true, None, check)
}

/// Drains stdout directly to an owned file while retaining bounded stderr and process deadlines.
pub fn file_command(
    command: &mut Command,
    destination: std::fs::File,
    limits: CommandLimits,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    collect_command(command, limits, None, None, false, Some(destination), check)
}

fn collect_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    diagnostic: bool,
    destination: Option<std::fs::File>,
    mut check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    ensure!(
        !limits.timeout.is_zero(),
        "command timeout must be positive"
    );
    check()?;
    static RUNTIME: OnceLock<std::result::Result<tokio::runtime::Runtime, String>> =
        OnceLock::new();
    let current = tokio::runtime::Handle::try_current().ok();
    let runtime = match current {
        Some(handle) => handle,
        None => RUNTIME
            .get_or_init(|| {
                tokio::runtime::Builder::new_multi_thread()
                    .worker_threads(1)
                    .enable_all()
                    .build()
                    .map_err(|error| error.to_string())
            })
            .as_ref()
            .map_err(|error| anyhow::anyhow!("start native command runtime: {error}"))?
            .handle()
            .clone(),
    };
    let command = std::mem::replace(command, Command::new(""));
    let input = input.map(ToOwned::to_owned);
    let progress = progress.cloned();
    runtime.block_on(async move {
        collect_async(
            command,
            limits,
            input,
            progress,
            diagnostic,
            destination,
            &mut check,
        )
        .await
    })
}

async fn collect_async(
    command: Command,
    limits: CommandLimits,
    input: Option<Vec<u8>>,
    progress: Option<CommandProgressSink>,
    diagnostic: bool,
    destination: Option<std::fs::File>,
    check: &mut impl FnMut() -> Result<()>,
) -> Result<Output> {
    let started = Instant::now();
    let mut wrapped = CommandWrap::from(tokio::process::Command::from(command));
    wrapped
        .command_mut()
        .stdin(if input.is_some() {
            Stdio::piped()
        } else {
            Stdio::null()
        })
        .stdout(destination.map(Stdio::from).unwrap_or_else(Stdio::piped))
        .stderr(Stdio::piped());
    wrapped.wrap(KillOnDrop);
    #[cfg(windows)]
    {
        wrapped.wrap(process_wrap::tokio::JobObject);
        wrapped.wrap(process_wrap::tokio::CreationFlags(
            windows::Win32::System::Threading::PROCESS_CREATION_FLAGS(0x0800_0000),
        ));
    }
    #[cfg(unix)]
    wrapped.wrap(process_wrap::tokio::ProcessGroup::leader());

    let mut child = wrapped.spawn().context("start bounded command")?;
    let stdout = child.stdout().take();
    let stderr = child.stderr().take().context("command stderr is missing")?;
    let failed = Arc::new(AtomicBool::new(false));
    let stdout_reader = match stdout {
        Some(stdout) => spawn_reader(
            stdout,
            limits.stdout_bytes,
            CommandStream::Stdout,
            progress.clone(),
            diagnostic,
            Arc::clone(&failed),
        ),
        None => tokio::spawn(async { Ok(Vec::new()) }),
    };
    let stderr_reader = spawn_reader(
        stderr,
        limits.stderr_bytes,
        CommandStream::Stderr,
        progress,
        diagnostic,
        Arc::clone(&failed),
    );
    let input_writer = input.map(|input| {
        let mut stdin = child.stdin().take().expect("piped command stdin");
        let failed = Arc::clone(&failed);
        tokio::spawn(async move {
            let result = stdin.write_all(&input).await.context("write command input");
            if result.is_err() {
                failed.store(true, Ordering::Release);
            }
            result
        })
    });

    let mut exit_status: Option<ExitStatus> = None;
    let stopped = loop {
        if let Err(error) = check() {
            break Some(error);
        }
        if started.elapsed() >= limits.timeout {
            break Some(anyhow::anyhow!(
                "command exceeded {:?} deadline",
                limits.timeout
            ));
        }
        if failed.load(Ordering::Acquire) {
            break None;
        }
        let streams_finished = stdout_reader.is_finished() && stderr_reader.is_finished();
        if exit_status.is_none() && (cfg!(windows) || streams_finished) {
            match child.try_wait().context("observe command exit") {
                Ok(status) => exit_status = status,
                Err(error) => break Some(error),
            }
        }
        if exit_status.is_some()
            && streams_finished
            && input_writer
                .as_ref()
                .is_none_or(tokio::task::JoinHandle::is_finished)
        {
            break None;
        }
        tokio::time::sleep(Duration::from_millis(5)).await;
    };

    if exit_status.is_none() || failed.load(Ordering::Acquire) || stopped.is_some() {
        #[cfg(unix)]
        let _ = child.signal(libc::SIGKILL);
        #[cfg(windows)]
        let _ = child.start_kill();
        child.wait().await.context("reap bounded command")?;
    }
    let stdout = stdout_reader
        .await
        .context("command stdout reader panicked")?;
    let stderr = stderr_reader
        .await
        .context("command stderr reader panicked")?;
    let written = match input_writer {
        Some(writer) => Some(writer.await.context("command input writer panicked")?),
        None => None,
    };
    if let Some(error) = stopped {
        return Err(error);
    }
    let stdout = stdout?;
    let stderr = stderr?;
    let status = exit_status.context("command stopped before exit")?;
    check()?;
    if status.success() {
        written.transpose()?;
    }
    Ok(Output {
        status,
        stdout,
        stderr,
    })
}

fn spawn_reader(
    source: impl AsyncRead + Unpin + Send + 'static,
    limit: usize,
    stream: CommandStream,
    progress: Option<CommandProgressSink>,
    diagnostic: bool,
    failed: Arc<AtomicBool>,
) -> tokio::task::JoinHandle<Result<Vec<u8>>> {
    tokio::spawn(async move {
        let result = read_stream(source, limit, stream, progress.as_ref(), diagnostic).await;
        if result.is_err() {
            failed.store(true, Ordering::Release);
        }
        result
    })
}

async fn read_stream(
    mut source: impl AsyncRead + Unpin,
    limit: usize,
    stream: CommandStream,
    progress: Option<&CommandProgressSink>,
    diagnostic: bool,
) -> Result<Vec<u8>> {
    let notice = b"\n[Command output truncated]\n";
    let notice = &notice[..notice.len().min(limit)];
    let capacity = if diagnostic {
        limit - notice.len()
    } else {
        limit
    };
    let mut output = if diagnostic {
        Vec::with_capacity(capacity)
    } else {
        Vec::new()
    };
    let mut chunk = [0; 8192];
    let mut published = 0;
    let mut sequence = 0;
    let mut truncated = false;
    loop {
        let available = capacity.saturating_sub(output.len());
        let requested = if diagnostic {
            chunk.len()
        } else {
            chunk.len().min(available.saturating_add(1)).max(1)
        };
        let received = match source.read(&mut chunk[..requested]).await {
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            result => result.context("read command output")?,
        };
        if received == 0 {
            if diagnostic && truncated {
                let mut retained = Vec::with_capacity(limit);
                retained.extend_from_slice(notice);
                retained.extend(output);
                return Ok(retained);
            }
            return Ok(output);
        }
        if !diagnostic {
            let name = match stream {
                CommandStream::Stdout => "stdout",
                CommandStream::Stderr => "stderr",
            };
            ensure!(
                received <= available,
                "command {name} exceeds {limit} bytes"
            );
            let required = output.len() + received;
            if required > output.capacity() {
                let reserved = output.capacity().saturating_mul(2).max(required).min(limit);
                output
                    .try_reserve_exact(reserved - output.len())
                    .context("reserve command output")?;
                ensure!(
                    output.capacity() <= limit,
                    "command {name} allocation exceeds {limit} bytes"
                );
            }
            output.extend_from_slice(&chunk[..received]);
        } else {
            let retained = received.min(capacity);
            let removed = (output.len() + retained).saturating_sub(capacity);
            output.drain(..removed);
            output.extend_from_slice(&chunk[received - retained..received]);
        }
        let visible = if diagnostic {
            received.min(capacity.saturating_sub(published))
        } else {
            received
        };
        if let Some(progress) = progress {
            if visible > 0 {
                publish_progress(
                    progress,
                    CommandProgress {
                        stream,
                        sequence,
                        bytes: chunk[..visible].to_vec(),
                    },
                )
                .await?;
                sequence += 1;
            }
            if diagnostic && visible < received && !truncated && !notice.is_empty() {
                publish_progress(
                    progress,
                    CommandProgress {
                        stream,
                        sequence,
                        bytes: notice.to_vec(),
                    },
                )
                .await?;
                sequence += 1;
            }
        }
        published += visible;
        truncated |= diagnostic && visible < received;
    }
}

async fn publish_progress(progress: &CommandProgressSink, update: CommandProgress) -> Result<()> {
    let progress = Arc::clone(progress);
    tokio::task::spawn_blocking(move || progress(update))
        .await
        .context("command progress callback panicked")?
}
