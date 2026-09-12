//! Bounded command output remains owned until the direct child exits and both readers finish.

use std::io::{self, Read, Write};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{Context, Result, ensure};

/// Independent stream capacities and the deadline for requesting direct-child termination.
#[derive(Clone, Copy, Debug)]
pub struct CommandLimits {
    pub stdout_bytes: usize,
    pub stderr_bytes: usize,
    pub timeout: Duration,
}

struct CommandOwner {
    child: Child,
    reaped: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CommandStream {
    Stdout,
    Stderr,
}

#[derive(Debug)]
pub struct CommandProgress {
    pub stream: CommandStream,
    pub sequence: u64,
    pub bytes: Vec<u8>,
}

pub type CommandProgressSink = Arc<dyn Fn(CommandProgress) -> Result<()> + Send + Sync>;

/// Reads bounded stdout and stderr concurrently without invoking a shell.
///
/// The caller owns job admission and must run this blocking function outside an async executor.
/// `check` rejects pre-start or active cancellation. A stream overflow, cancellation, timeout,
/// reader failure, or callback unwind terminates and reaps the direct child before releasing
/// ownership. Nonzero exit status remains available in `Output` for caller classification.
///
/// The command receives null stdin and piped output. A zero timeout rejects before spawning.
/// The timeout requests termination, but cannot bound an operating-system wait or inherited pipe
/// held by a descendant. This function does not establish process-tree containment.
pub fn read_command(
    command: &mut Command,
    limits: CommandLimits,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    progress_command(command, limits, None, None, check)
}

/// Writes admitted input concurrently with bounded output collection and retains it until reaping.
pub fn write_command(
    command: &mut Command,
    limits: CommandLimits,
    input: &[u8],
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    progress_command(command, limits, Some(input), None, check)
}

/// Publishes bounded chunks in independent stream order while retaining complete bounded output.
pub fn progress_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    collect_command(command, limits, input, progress, false, check)
}

/// Drains diagnostic output to EOF without treating capture truncation as command failure.
/// Retains a bounded tail and publishes a bounded prefix with an explicit truncation notice.
pub fn diagnostic_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    collect_command(command, limits, input, progress, true, check)
}

fn collect_command(
    command: &mut Command,
    limits: CommandLimits,
    input: Option<&[u8]>,
    progress: Option<&CommandProgressSink>,
    diagnostic: bool,
    mut check: impl FnMut() -> Result<()>,
) -> Result<Output> {
    ensure!(
        !limits.timeout.is_zero(),
        "command timeout must be positive"
    );
    check()?;
    let started = Instant::now();
    let reader_failed = AtomicBool::new(false);
    thread::scope(|scope| {
        // Drop the child owner before the scope joins readers on any early-return or unwind path.
        let mut owner = CommandOwner {
            child: command
                .stdin(if input.is_some() {
                    Stdio::piped()
                } else {
                    Stdio::null()
                })
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .context("start bounded command")?,
            reaped: false,
        };
        let stdout = owner
            .child
            .stdout
            .take()
            .context("command stdout is missing")?;
        let stderr = owner
            .child
            .stderr
            .take()
            .context("command stderr is missing")?;
        let failure = &reader_failed;
        let input_writer = if let Some(input) = input {
            let mut stdin = owner
                .child
                .stdin
                .take()
                .context("command stdin is missing")?;
            Some(
                thread::Builder::new()
                    .name("forge-git-stdin".into())
                    .spawn_scoped(scope, move || {
                        let result = stdin.write_all(input).context("write command input");
                        if result.is_err() {
                            failure.store(true, Ordering::Release);
                        }
                        result
                    })
                    .context("start command input writer")?,
            )
        } else {
            None
        };
        let stdout_reader = thread::Builder::new()
            .name("forge-git-stdout".into())
            .spawn_scoped(scope, move || {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let read = if diagnostic {
                        read_diagnostic_stream
                    } else {
                        read_stream_progress
                    };
                    read(stdout, limits.stdout_bytes, CommandStream::Stdout, progress)
                }))
                .unwrap_or_else(|_| {
                    Err(anyhow::anyhow!("command stdout progress reader panicked"))
                });
                if result.is_err() {
                    failure.store(true, Ordering::Release);
                }
                result
            })
            .context("start command stdout reader")?;
        let stderr_reader = thread::Builder::new()
            .name("forge-git-stderr".into())
            .spawn_scoped(scope, move || {
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    let read = if diagnostic {
                        read_diagnostic_stream
                    } else {
                        read_stream_progress
                    };
                    read(stderr, limits.stderr_bytes, CommandStream::Stderr, progress)
                }))
                .unwrap_or_else(|_| {
                    Err(anyhow::anyhow!("command stderr progress reader panicked"))
                });
                if result.is_err() {
                    failure.store(true, Ordering::Release);
                }
                result
            })
            .context("start command stderr reader")?;

        let (status, stopped) = loop {
            let stopped = check().err().or_else(|| {
                (started.elapsed() >= limits.timeout)
                    .then(|| anyhow::anyhow!("command exceeded {:?} deadline", limits.timeout))
            });
            if stopped.is_some() || reader_failed.load(Ordering::Acquire) {
                break (owner.terminate_and_reap()?, stopped);
            }
            if let Some(status) = owner.child.try_wait().context("observe command exit")? {
                owner.reaped = true;
                break (status, None);
            }
            thread::sleep(Duration::from_millis(5));
        };
        let stdout = stdout_reader
            .join()
            .map_err(|_| anyhow::anyhow!("command stdout reader panicked"))?;
        let stderr = stderr_reader
            .join()
            .map_err(|_| anyhow::anyhow!("command stderr reader panicked"))?;
        let written = input_writer
            .map(|writer| {
                writer
                    .join()
                    .map_err(|_| anyhow::anyhow!("command input writer panicked"))
            })
            .transpose()?;
        if let Some(error) = stopped {
            return Err(error);
        }
        check()?;
        if status.success() {
            written.transpose()?;
        }
        Ok(Output {
            status,
            stdout: stdout?,
            stderr: stderr?,
        })
    })
}

impl CommandOwner {
    fn terminate_and_reap(&mut self) -> Result<ExitStatus> {
        // A concurrent natural exit can make kill fail. wait still establishes the final status.
        let _ = self.child.kill();
        let status = self.child.wait().context("reap bounded command")?;
        self.reaped = true;
        Ok(status)
    }
}

impl Drop for CommandOwner {
    fn drop(&mut self) {
        if !self.reaped {
            let _ = self.terminate_and_reap();
        }
    }
}

fn read_stream_progress(
    mut source: impl Read,
    limit: usize,
    stream: CommandStream,
    progress: Option<&CommandProgressSink>,
) -> Result<Vec<u8>> {
    let stream_name = match stream {
        CommandStream::Stdout => "stdout",
        CommandStream::Stderr => "stderr",
    };
    let mut output = Vec::new();
    let mut chunk = [0; 8192];
    let mut sequence = 0;
    loop {
        let available = limit - output.len();
        let requested = chunk.len().min(available.saturating_add(1));
        let received = match source.read(&mut chunk[..requested]) {
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            result => result.with_context(|| format!("read command {stream_name}"))?,
        };
        if received == 0 {
            return Ok(output);
        }
        ensure!(
            received <= available,
            "command {stream_name} exceeds {limit} bytes"
        );
        let required = output.len() + received;
        if required > output.capacity() {
            let capacity = output.capacity().saturating_mul(2).max(required).min(limit);
            output
                .try_reserve_exact(capacity - output.len())
                .context("reserve command output")?;
            ensure!(
                output.capacity() <= limit,
                "command {stream_name} allocation exceeds {limit} bytes"
            );
        }
        output.extend_from_slice(&chunk[..received]);
        if let Some(progress) = progress {
            progress(CommandProgress {
                stream,
                sequence,
                bytes: chunk[..received].to_vec(),
            })?;
            sequence += 1;
        }
    }
}

fn read_diagnostic_stream(
    mut source: impl Read,
    limit: usize,
    stream: CommandStream,
    progress: Option<&CommandProgressSink>,
) -> Result<Vec<u8>> {
    let notice = b"\n[Command output truncated]\n";
    let notice = &notice[..notice.len().min(limit)];
    let capacity = limit - notice.len();
    let mut output = Vec::with_capacity(capacity);
    let mut chunk = [0; 8192];
    let mut published = 0;
    let mut sequence = 0;
    let mut truncated = false;
    loop {
        let received = match source.read(&mut chunk) {
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            result => result.context("read command diagnostics")?,
        };
        if received == 0 {
            if truncated {
                let mut retained = Vec::with_capacity(limit);
                retained.extend_from_slice(notice);
                retained.extend(output);
                return Ok(retained);
            }
            return Ok(output);
        }
        let retained = received.min(capacity);
        let removed = (output.len() + retained).saturating_sub(capacity);
        output.drain(..removed);
        output.extend_from_slice(&chunk[received - retained..received]);
        let visible = received.min(capacity - published);
        if let Some(progress) = progress {
            if visible > 0 {
                progress(CommandProgress {
                    stream,
                    sequence,
                    bytes: chunk[..visible].to_vec(),
                })?;
                sequence += 1;
            }
            if visible < received && !truncated && !notice.is_empty() {
                progress(CommandProgress {
                    stream,
                    sequence,
                    bytes: notice.to_vec(),
                })?;
                sequence += 1;
            }
        }
        published += visible;
        truncated |= visible < received;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn progress_is_bounded_and_preserves_stream_bytes_in_sequence() {
        let observed = Arc::new(std::sync::Mutex::new(Vec::new()));
        let captured = Arc::clone(&observed);
        let progress: CommandProgressSink = Arc::new(move |chunk| {
            captured.lock().unwrap().push(chunk);
            Ok(())
        });
        let expected = vec![b'x'; 20_000];
        let result = read_stream_progress(
            &expected[..],
            expected.len(),
            CommandStream::Stderr,
            Some(&progress),
        )
        .unwrap();
        assert_eq!(result, expected);
        let chunks = observed.lock().unwrap();
        for (sequence, chunk) in chunks.iter().enumerate() {
            assert_eq!(chunk.sequence, sequence as u64);
            assert_eq!(chunk.stream, CommandStream::Stderr);
            assert!(chunk.bytes.len() <= 8192);
        }
        assert_eq!(
            chunks
                .iter()
                .flat_map(|chunk| chunk.bytes.iter().copied())
                .collect::<Vec<_>>(),
            expected
        );
    }

    #[test]
    fn stream_accepts_exact_limit_and_rejects_the_next_byte() {
        assert_eq!(
            read_stream_progress(&b"1234"[..], 4, CommandStream::Stdout, None).unwrap(),
            b"1234"
        );
        assert!(
            read_stream_progress(&b"12345"[..], 4, CommandStream::Stdout, None)
                .unwrap_err()
                .to_string()
                .contains("stdout exceeds 4 bytes")
        );
        assert!(read_stream_progress(&b"x"[..], 0, CommandStream::Stderr, None).is_err());
        assert!(
            read_stream_progress(&b""[..], 0, CommandStream::Stderr, None)
                .unwrap()
                .is_empty()
        );
    }
}
