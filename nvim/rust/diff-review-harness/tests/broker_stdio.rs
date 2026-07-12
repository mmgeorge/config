use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

struct BrokerProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl BrokerProcess {
    fn start() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_diff-review-harness"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        Self {
            child,
            stdin,
            stdout,
        }
    }

    fn request(&mut self, request: Value) {
        serde_json::to_writer(&mut self.stdin, &request).unwrap();
        self.stdin.write_all(b"\n").unwrap();
        self.stdin.flush().unwrap();
    }

    fn read_response(&mut self, request_id: u64) -> Vec<Value> {
        let mut message_list = Vec::new();
        loop {
            let mut line = String::new();
            assert_ne!(self.stdout.read_line(&mut line).unwrap(), 0);
            let message: Value = serde_json::from_str(&line).unwrap();
            let complete = message.get("id").and_then(Value::as_u64) == Some(request_id);
            message_list.push(message);
            if complete {
                return message_list;
            }
        }
    }
}

impl Drop for BrokerProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn git(workspace: &Path, args: &[&str]) {
    assert!(
        Command::new("git")
            .args(args)
            .current_dir(workspace)
            .status()
            .unwrap()
            .success()
    );
}

#[test]
fn streams_mock_backend_events_before_the_jsonl_response() {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    git(
        repository.path(),
        &["config", "user.email", "harness@example.invalid"],
    );
    git(
        repository.path(),
        &["config", "user.name", "Harness Integration"],
    );
    std::fs::write(repository.path().join("README.md"), "# Harness\n").unwrap();
    git(repository.path(), &["add", "."]);
    git(repository.path(), &["commit", "-qm", "seed"]);
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "stdio-test",
            "backend": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "trust_profile": "workspace",
            "goal_max_turns": 20
        }
    }));
    let initialized = broker.read_response(1);
    assert!(initialized.last().unwrap().get("error").is_none());

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "/plan build the feature" }
    }));
    let planned = broker.read_response(2);
    let event_name = planned
        .iter()
        .filter_map(|message| message.get("event").and_then(Value::as_str))
        .collect::<Vec<_>>();
    assert!(event_name.contains(&"backend_event"));
    assert_eq!(
        event_name
            .iter()
            .filter(|event| **event == "backend_event")
            .count(),
        3
    );
    let backend_kind = planned
        .iter()
        .filter(|message| message.get("event").and_then(Value::as_str) == Some("backend_event"))
        .filter_map(|message| message.pointer("/payload/kind").and_then(Value::as_str))
        .collect::<Vec<_>>();
    assert_eq!(
        backend_kind,
        vec!["user_message", "assistant_message", "assistant_summary"]
    );
    assert!(event_name.contains(&"plan_review"));
    assert!(planned.last().unwrap().get("error").is_none());

    broker.request(json!({ "id": 3, "method": "shutdown", "params": {} }));
    let stopped = broker.read_response(3);
    assert_eq!(
        stopped
            .last()
            .unwrap()
            .pointer("/result/shutdown")
            .and_then(Value::as_bool),
        Some(true)
    );
}
