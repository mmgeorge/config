use std::io::Write;
use std::process::Command;
use std::time::{Duration, Instant};

use forge_git::command::{CommandLimits, read_command};

fn fixture(name: &str) -> Command {
    let mut command = Command::new(std::env::current_exe().unwrap());
    command.args(["--exact", name, "--ignored", "--nocapture"]);
    command
}

fn limits() -> CommandLimits {
    CommandLimits {
        stdout_bytes: 4096,
        stderr_bytes: 4096,
        timeout: Duration::from_secs(5),
    }
}

#[test]
fn preserves_both_streams_and_nonzero_exit_status() {
    let output = read_command(
        &mut fixture("command_child_failure"),
        CommandLimits {
            stdout_bytes: 128 * 1024,
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
fn stdout_and_stderr_overflow_stop_without_returning_partial_success() {
    for (child, stream) in [
        ("command_child_stdout", "stdout"),
        ("command_child_stderr", "stderr"),
    ] {
        let started = Instant::now();
        let error = read_command(&mut fixture(child), limits(), || Ok(())).unwrap_err();
        assert!(
            error
                .to_string()
                .contains(&format!("{stream} exceeds 4096 bytes")),
            "{error:#}"
        );
        assert!(started.elapsed() < Duration::from_secs(5));
    }
}

#[test]
fn deadline_terminates_and_reaps_a_sleeping_child() {
    let started = Instant::now();
    let error = read_command(
        &mut fixture("command_child_wait"),
        CommandLimits {
            timeout: Duration::from_millis(100),
            ..limits()
        },
        || Ok(()),
    )
    .unwrap_err();
    assert!(error.to_string().contains("deadline"));
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
    std::thread::sleep(Duration::from_secs(10));
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_stderr() {
    for _ in 0..16 {
        let _ = std::io::stderr().write_all(&[b'x'; 32768]);
    }
    std::thread::sleep(Duration::from_secs(10));
}

#[test]
#[ignore = "subprocess fixture invoked by command ownership tests"]
fn command_child_wait() {
    std::thread::sleep(Duration::from_secs(10));
}
