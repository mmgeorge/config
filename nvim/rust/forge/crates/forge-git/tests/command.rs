use std::io::Write;
use std::process::Command;
use std::time::Duration;

use forge_git::command::{CommandLimits, read_command};

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
fn complete_stdout_and_stderr_cross_former_stream_caps_without_truncation() {
    for (child, stream) in [
        ("command_child_stdout", "stdout"),
        ("command_child_stderr", "stderr"),
    ] {
        let output = read_command(&mut fixture(child), limits(), || Ok(())).unwrap();
        assert!(output.status.success());
        let bytes = if stream == "stdout" {
            &output.stdout
        } else {
            &output.stderr
        };
        assert!(bytes.iter().filter(|byte| **byte == b'x').count() >= 512 * 1024);
    }
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
    #[expect(
        clippy::disallowed_methods,
        reason = "this fixture is the child process whose termination the ownership tests verify"
    )]
    std::thread::sleep(Duration::from_secs(10));
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
