use std::io::Write;
use std::process::Command;
use std::time::{Duration, Instant};

use forge_git::command::{
    CommandLimits, read_command, read_command_pair, read_command_with_expiry,
};

fn fixture(name: &str) -> Command {
    let mut command = Command::new(std::env::current_exe().unwrap());
    command.args(["--exact", name, "--ignored", "--nocapture"]);
    command
}

fn limits() -> CommandLimits {
    CommandLimits {
        stderr_bytes: 4096,
        timeout: Duration::from_secs(5),
    }
}

#[test]
fn preserves_both_streams_and_nonzero_exit_status() {
    let output = read_command(
        &mut fixture("command_child_failure"),
        CommandLimits {
            stderr_bytes: 128 * 1024,
            ..limits()
        },
        || Ok(()),
    )
    .unwrap();
    assert_eq!(output.status.code(), Some(7));
    assert!(output.stdout.len() >= 96 * 1024);
    assert!(output.stdout.ends_with(b"saved stdout\n"));
    assert_eq!(output.stderr.len(), 96 * 1024 + b"saved stderr\n".len());
    assert!(output.stderr.ends_with(b"saved stderr\n"));
}

#[test]
fn paired_reads_preserve_both_failures_and_streams() {
    let (first, second) = read_command_pair(
        fixture("command_child_failure"),
        fixture("command_child_failure"),
        CommandLimits {
            stderr_bytes: 128 * 1024,
            ..limits()
        },
        || Ok(()),
    )
    .unwrap();
    for output in [first, second] {
        assert_eq!(output.status.code(), Some(7));
        assert!(output.stdout.ends_with(b"saved stdout\n"));
        assert!(output.stderr.ends_with(b"saved stderr\n"));
    }
}

#[test]
fn paired_reads_cancel_both_children_before_returning() {
    let started = Instant::now();
    let error = read_command_pair(
        fixture("command_child_wait"),
        fixture("command_child_wait"),
        limits(),
        || {
            anyhow::ensure!(
                started.elapsed() < Duration::from_millis(100),
                "cancel pair"
            );
            Ok(())
        },
    )
    .unwrap_err();
    assert!(error.to_string().contains("cancel pair"));
    assert!(started.elapsed() < Duration::from_secs(2));
}

#[test]
fn complete_stdout_and_stderr_cross_former_stream_caps_without_truncation() {
    for (child, stream) in [
        ("command_child_stdout", "stdout"),
        ("command_child_stderr", "stderr"),
    ] {
        let started = Instant::now();
        let output = read_command(&mut fixture(child), limits(), || Ok(())).unwrap();
        assert!(output.status.success());
        let bytes = if stream == "stdout" {
            &output.stdout
        } else {
            &output.stderr
        };
        assert!(bytes.iter().filter(|byte| **byte == b'x').count() >= 512 * 1024);
        assert!(started.elapsed() < Duration::from_secs(5));
    }
}

#[test]
fn deadline_terminates_and_reaps_a_sleeping_child() {
    let fixture_dir = tempfile::tempdir().unwrap();
    let ready = fixture_dir.path().join("sleeping-child.ready");
    let mut command = fixture("command_child_wait");
    command.env("FORGE_TEST_READY", &ready);
    let started = Instant::now();
    // The deadline expires once the child confirms it is sleeping, not after a wall-clock delay.
    let error =
        read_command_with_expiry(&mut command, limits(), || ready.exists(), || Ok(())).unwrap_err();
    assert!(error.to_string().contains("deadline"));
    // Returning before the child's 10-second sleep ends proves termination and reaping.
    assert!(started.elapsed() < Duration::from_secs(5));
}

#[test]
fn active_cancellation_terminates_a_spawned_child() {
    let mut checked = false;
    let started = Instant::now();
    let error = read_command(&mut fixture("command_child_wait"), limits(), || {
        anyhow::ensure!(!checked, "cancelled after spawn");
        checked = true;
        Ok(())
    })
    .unwrap_err();
    assert!(error.to_string().contains("cancelled after spawn"));
    assert!(started.elapsed() < Duration::from_secs(5));
}

#[test]
fn cancellation_terminates_a_descendant_holding_inherited_pipes() {
    let fixture_dir = tempfile::tempdir().unwrap();
    let marker = fixture_dir.path().join("descendant-marker");
    let ready = marker.with_extension("ready");
    let mut command = fixture("command_child_descendant");
    command.env("FORGE_TEST_MARKER", &marker);
    // The descendant writes `ready` itself, so cancellation fires while it holds the pipes.
    let error = read_command(&mut command, limits(), || {
        anyhow::ensure!(!ready.exists(), "cancel descendant tree");
        Ok(())
    })
    .unwrap_err();
    assert!(error.to_string().contains("cancel descendant tree"));
    // Collection returns only after every holder of the output pipes closes them, so the
    // descendant has exited and the marker state is final without waiting past its sleep.
    assert!(!marker.exists(), "descendant survived command cancellation");
}

#[test]
fn deadline_terminates_descendants_after_the_direct_child_exits() {
    let fixture_dir = tempfile::tempdir().unwrap();
    let marker = fixture_dir.path().join("exited-parent-marker");
    let ready = marker.with_extension("ready");
    let mut command = fixture("command_child_descendant_exits");
    command.env("FORGE_TEST_MARKER", &marker);
    // The orphan writes `ready` only after its parent exits, so expiry follows the parent's exit.
    let error =
        read_command_with_expiry(&mut command, limits(), || ready.exists(), || Ok(())).unwrap_err();
    assert!(error.to_string().contains("deadline"));
    // Collection returns only after every holder of the output pipes closes them, so the orphan
    // has exited and the marker state is final without waiting past the orphan's sleep.
    assert!(
        !marker.exists(),
        "descendant survived after its parent exited"
    );
}

#[cfg(windows)]
#[test]
fn job_object_terminates_descendants_after_owner_abort() {
    use std::io::Read;
    use std::os::windows::io::{AsRawHandle, FromRawHandle, OwnedHandle};
    use windows::Win32::Foundation::{ERROR_PIPE_CONNECTED, HANDLE, INVALID_HANDLE_VALUE};
    use windows::Win32::Storage::FileSystem::{FILE_FLAG_FIRST_PIPE_INSTANCE, PIPE_ACCESS_INBOUND};
    use windows::Win32::System::Pipes::{
        ConnectNamedPipe, CreateNamedPipeW, PIPE_REJECT_REMOTE_CLIENTS, PIPE_TYPE_BYTE, PIPE_WAIT,
    };
    use windows::core::PCWSTR;

    let fixture_dir = tempfile::tempdir().unwrap();
    let marker = fixture_dir.path().join("owner-abort-marker");
    // The orphan's output pipes belong to the aborted owner, so the test serves its own pipe that
    // the descendant holds open until it exits.
    let liveness_pipe = format!(r"\\.\pipe\forge-git-owner-abort-{}", std::process::id());
    let wide_pipe_name: Vec<u16> = liveness_pipe.encode_utf16().chain([0]).collect();
    // SAFETY: the name is NUL-terminated and outlives the call. Passing no security attributes
    // keeps the server handle out of the owner fixture's inherited handles.
    let server = unsafe {
        CreateNamedPipeW(
            PCWSTR(wide_pipe_name.as_ptr()),
            PIPE_ACCESS_INBOUND | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_TYPE_BYTE | PIPE_WAIT | PIPE_REJECT_REMOTE_CLIENTS,
            1,
            0,
            0,
            0,
            None,
        )
    };
    assert_ne!(server, INVALID_HANDLE_VALUE, "create liveness pipe");
    // SAFETY: CreateNamedPipeW returned a valid handle that nothing else owns.
    let server = unsafe { OwnedHandle::from_raw_handle(server.0) };
    let mut owner = fixture("command_owner_abort");
    owner
        .env("FORGE_TEST_MARKER", &marker)
        .env("FORGE_TEST_LIVENESS_PIPE", &liveness_pipe)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    let mut owner = owner.spawn().unwrap();
    let owner_exit = std::thread::spawn({
        let liveness_pipe = liveness_pipe.clone();
        move || {
            let status = owner.wait().unwrap();
            // Unblock the connection wait when the owner exits before its descendant connects.
            // A connected descendant keeps the single pipe instance busy, so this open then fails.
            let _ = std::fs::OpenOptions::new().write(true).open(&liveness_pipe);
            status
        }
    });
    // SAFETY: `server` keeps the pipe handle open for the duration of the call.
    match unsafe { ConnectNamedPipe(HANDLE(server.as_raw_handle()), None) } {
        Ok(()) => {}
        Err(error) if error.code() == ERROR_PIPE_CONNECTED.to_hresult() => {}
        Err(error) => panic!("connect liveness pipe: {error}"),
    }
    let mut handshake = Vec::new();
    // Reading ends only when the client handle closes, which the descendant defers until exit.
    std::fs::File::from(server)
        .read_to_end(&mut handshake)
        .unwrap();
    assert!(!owner_exit.join().unwrap().success());
    assert_eq!(
        handshake, b"ready",
        "descendant never held the liveness pipe"
    );
    assert!(!marker.exists(), "descendant survived owner abort");
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_descendant() {
    let marker = std::path::PathBuf::from(std::env::var_os("FORGE_TEST_MARKER").unwrap());
    let mut descendant = fixture("command_descendant_marker");
    descendant.env("FORGE_TEST_MARKER", &marker);
    let mut descendant = descendant.spawn().unwrap();
    std::thread::sleep(Duration::from_secs(10));
    descendant.wait().unwrap();
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_descendant_exits() {
    let marker = std::path::PathBuf::from(std::env::var_os("FORGE_TEST_MARKER").unwrap());
    let mut descendant = fixture("command_descendant_marker");
    descendant
        .env("FORGE_TEST_MARKER", &marker)
        .stdin(std::process::Stdio::piped());
    // The leaked stdin writer closes only when this process exits.
    std::mem::forget(descendant.spawn().unwrap());
    // This fixture exits while its child retains the output pipes, exercising orphan collection.
    std::process::exit(0);
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_descendant_marker() {
    // Stdin reaches end of file once its writer closes: immediately for inherited null input, or
    // when a parent holding a piped writer exits, so readiness then implies the parent is gone.
    std::io::copy(&mut std::io::stdin(), &mut std::io::sink()).unwrap();
    let marker = std::path::PathBuf::from(std::env::var_os("FORGE_TEST_MARKER").unwrap());
    // Held until exit, so the owning test observes this process's termination as pipe closure.
    let _liveness = std::env::var_os("FORGE_TEST_LIVENESS_PIPE").map(|liveness_pipe| {
        let mut client = std::fs::OpenOptions::new()
            .write(true)
            .open(liveness_pipe)
            .unwrap();
        client.write_all(b"ready").unwrap();
        client
    });
    std::fs::write(marker.with_extension("ready"), b"ready").unwrap();
    std::thread::sleep(Duration::from_secs(10));
    std::fs::write(marker, b"survived").unwrap();
}

#[cfg(windows)]
#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_owner_abort() {
    let marker = std::path::PathBuf::from(std::env::var_os("FORGE_TEST_MARKER").unwrap());
    let mut command = fixture("command_child_descendant");
    command.env("FORGE_TEST_MARKER", &marker);
    let _ = read_command(&mut command, limits(), || {
        if marker.with_extension("ready").exists() {
            // Terminate without unwinding or waiting for Windows crash-report processing.
            // SAFETY: the pseudo-handle identifies this isolated fixture process.
            unsafe {
                windows::Win32::System::Threading::TerminateProcess(
                    windows::Win32::System::Threading::GetCurrentProcess(),
                    1,
                )
            }
            .expect("terminate command owner fixture");
            unreachable!("owner termination returned");
        }
        Ok(())
    });
}

#[test]
fn callback_unwind_disposes_the_child_before_returning() {
    let started = Instant::now();
    let outcome = std::panic::catch_unwind(|| {
        let mut checked = false;
        let _ = read_command(&mut fixture("command_child_wait"), limits(), || {
            assert!(!checked, "synthetic callback unwind after spawn");
            checked = true;
            Ok(())
        });
    });
    assert!(outcome.is_err());
    assert!(started.elapsed() < Duration::from_secs(5));
}

#[test]
fn invalid_deadline_and_prestart_cancellation_do_not_launch() {
    let mut missing = Command::new("forge-nonexistent-command-fixture");
    let error = read_command(
        &mut missing,
        CommandLimits {
            timeout: Duration::ZERO,
            ..limits()
        },
        || panic!("invalid limits must reject before cancellation callback"),
    )
    .unwrap_err();
    assert!(error.to_string().contains("timeout must be positive"));
    let error = read_command(&mut missing, limits(), || {
        anyhow::bail!("cancelled before spawn")
    })
    .unwrap_err();
    assert!(error.to_string().contains("cancelled before spawn"));
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_failure() {
    std::io::stdout().write_all(&[b'o'; 96 * 1024]).unwrap();
    std::io::stderr().write_all(&[b'e'; 96 * 1024]).unwrap();
    std::io::stdout().write_all(b"saved stdout\n").unwrap();
    std::io::stderr().write_all(b"saved stderr\n").unwrap();
    std::process::exit(7);
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_stdout() {
    for _ in 0..16 {
        let _ = std::io::stdout().write_all(&[b'x'; 32768]);
    }
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_stderr() {
    for _ in 0..16 {
        let _ = std::io::stderr().write_all(&[b'x'; 32768]);
    }
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_wait() {
    if let Some(ready) = std::env::var_os("FORGE_TEST_READY") {
        std::fs::write(ready, b"ready").unwrap();
    }
    std::thread::sleep(Duration::from_secs(10));
}

#[test]
fn diagnostic_overflow_preserves_exit_status_and_bounded_output() {
    use forge_git::command::{CommandProgressSink, CommandStream, diagnostic_command};
    use std::sync::{Arc, Mutex};
    for (child, expected) in [("command_child_failure", 7), ("command_child_success", 0)] {
        let captured = Arc::new(Mutex::new(Vec::new()));
        let observed = Arc::clone(&captured);
        let progress: CommandProgressSink = Arc::new(move |chunk| {
            observed.lock().unwrap().push(chunk);
            Ok(())
        });
        let output =
            diagnostic_command(&mut fixture(child), limits(), None, Some(&progress), || {
                Ok(())
            })
            .unwrap();
        assert_eq!(output.status.code(), Some(expected));
        assert!(output.stdout.len() <= 4096 && output.stderr.len() <= 4096);
        assert!(output.stdout.ends_with(b"saved stdout\n"));
        assert!(output.stderr.ends_with(b"saved stderr\n"));
        for stream in [CommandStream::Stdout, CommandStream::Stderr] {
            let chunks = captured.lock().unwrap();
            let selected: Vec<_> = chunks
                .iter()
                .filter(|chunk| chunk.stream == stream)
                .collect();
            for (sequence, chunk) in selected.iter().enumerate() {
                assert_eq!(chunk.sequence, sequence as u64);
            }
            let bytes: Vec<_> = selected
                .iter()
                .flat_map(|chunk| chunk.bytes.iter().copied())
                .collect();
            assert!(bytes.len() <= 4096);
            assert!(String::from_utf8_lossy(&bytes).contains("Command output truncated"));
        }
    }
}

#[test]
#[ignore = "subprocess fixture invoked by diagnostic ownership tests"]
fn command_child_success() {
    std::io::stdout().write_all(&[b'o'; 96 * 1024]).unwrap();
    std::io::stderr().write_all(&[b'e'; 96 * 1024]).unwrap();
    std::io::stdout().write_all(b"saved stdout\n").unwrap();
    std::io::stderr().write_all(b"saved stderr\n").unwrap();
    std::process::exit(0);
}
