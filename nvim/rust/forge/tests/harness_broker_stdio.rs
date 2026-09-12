use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};

static BROKER_TEST_LOCK: Mutex<()> = Mutex::new(());

#[test]
fn repository_discovery_failure_returns_a_correlated_initialize_error() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    std::fs::write(repository.path().join(".git/config"), "[invalid\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({"id": 41, "method": "harness.initialize", "params": {
        "data_root": data.path(), "workspace": repository.path(), "client_id": "invalid-repository",
        "backend": { "kind": "mock", "command": ["mock"] }
    }}));
    let response = broker.read_response(41);
    assert_eq!(
        response.last().unwrap()["error"]["code"],
        "initialize_failed"
    );
    assert!(
        !response.last().unwrap()["error"]["message"]
            .as_str()
            .unwrap()
            .is_empty()
    );
    assert_eq!(std::fs::read_dir(data.path()).unwrap().count(), 0);
}

#[test]
fn nested_workspace_initialization_preserves_the_canonical_repository_root() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    let nested = repository.path().join("nested");
    std::fs::create_dir(&nested).unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({"id": 42, "method": "harness.initialize", "params": {
        "data_root": data.path(), "workspace": nested, "client_id": "nested-repository",
        "backend": { "kind": "mock", "command": ["mock"] }
    }}));
    let response = broker.read_response(42);
    let workspace = response.last().unwrap()["result"]["session"]["workspace"]
        .as_str()
        .unwrap();
    assert_eq!(
        std::fs::canonicalize(workspace).unwrap(),
        std::fs::canonicalize(repository.path()).unwrap()
    );
    broker.request(json!({"id": 43, "method": "shutdown", "params": {}}));
    assert!(
        broker
            .read_response(43)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );
}

#[tokio::test]
async fn rejects_missing_or_mismatched_wire_version_before_initialization() {
    use tokio::io::AsyncWriteExt;
    for version in [Value::Null, json!(0), json!(3), json!("2")] {
        let root = tempfile::tempdir().unwrap();
        let data_root = root.path().join("must-not-be-created");
        let mut child = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let mut request =
            json!({"id": 7, "method": "initialize", "params": {"data_root": data_root}});
        if !version.is_null() {
            request["params"]["protocol_version"] = version;
        }
        let mut encoded = serde_json::to_vec(&request).unwrap();
        encoded.push(b'\n');
        let mut input = child.stdin.take().unwrap();
        input.write_all(&encoded).await.unwrap();
        drop(input);
        let output = tokio::time::timeout(Duration::from_secs(5), child.wait_with_output())
            .await
            .unwrap()
            .unwrap();
        assert!(output.status.success());
        let response: Value = serde_json::from_slice(&output.stdout).unwrap();
        assert_eq!(response["id"], 7);
        assert_eq!(response["error"]["code"], "protocol_mismatch");
        assert_eq!(
            response["error"]["data"]["expected_version"],
            forge_protocol::WIRE_VERSION
        );
        assert!(!data_root.exists());
    }
}

#[tokio::test]
async fn writer_waits_for_native_consumption_after_the_frame_window_fills() {
    use tokio::io::{AsyncBufReadExt, AsyncWriteExt};

    async fn send(input: &mut tokio::process::ChildStdin, request: Value) {
        let mut bytes = serde_json::to_vec(&request).unwrap();
        bytes.push(b'\n');
        input.write_all(&bytes).await.unwrap();
    }

    let mut child = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .kill_on_drop(true)
        .spawn()
        .unwrap();
    let mut input = child.stdin.take().unwrap();
    let mut output = tokio::io::BufReader::new(child.stdout.take().unwrap());
    send(
        &mut input,
        json!({"id": 1, "method": "initialize", "params": {
            "protocol_version": forge_protocol::WIRE_VERSION,
        }}),
    )
    .await;
    let mut line = Vec::new();
    tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
        .await
        .unwrap()
        .unwrap();
    let initialized: Value = serde_json::from_slice(&line).unwrap();
    assert!(initialized.get("error").is_none(), "{initialized}");
    let mut consumed_bytes = line.len();
    send(&mut input, json!({"id": 0, "method": "transport.consumed", "params": {"bytes": consumed_bytes, "frames": 1}})).await;
    for request_id in 2..130 {
        send(
            &mut input,
            json!({"id": request_id, "method": "turn.cancel", "params": {}}),
        )
        .await;
        line.clear();
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
            .await
            .unwrap()
            .unwrap();
        let message: Value = serde_json::from_slice(&line).unwrap();
        assert_eq!(message["id"], request_id);
        consumed_bytes += line.len();
    }
    send(
        &mut input,
        json!({"id": 130, "method": "turn.cancel", "params": {}}),
    )
    .await;
    line.clear();
    assert!(
        tokio::time::timeout(
            Duration::from_millis(50),
            output.read_until(b'\n', &mut line)
        )
        .await
        .is_err()
    );
    assert!(
        line.is_empty(),
        "writer emitted a partial frame without credit"
    );
    send(&mut input, json!({"id": 0, "method": "transport.consumed", "params": {"bytes": consumed_bytes, "frames": 129}})).await;
    tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(serde_json::from_slice::<Value>(&line).unwrap()["id"], 130);
    send(
        &mut input,
        json!({"id": 131, "method": "shutdown", "params": {}}),
    )
    .await;
    loop {
        line.clear();
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
            .await
            .unwrap()
            .unwrap();
        let message: Value = serde_json::from_slice(&line).unwrap();
        if message["id"] == 131 {
            assert!(message.get("error").is_none(), "shutdown failed: {message}");
            break;
        }
    }
    assert!(
        tokio::time::timeout(Duration::from_secs(5), child.wait())
            .await
            .unwrap()
            .unwrap()
            .success()
    );
}

#[tokio::test]
async fn rejects_oversized_unterminated_request_before_initializing_a_provider() {
    use tokio::io::AsyncWriteExt;

    let mut child = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"))
        .arg("nvim")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .unwrap();
    let mut input = child.stdin.take().unwrap();
    let payload = vec![b'x'; forge_protocol::MAX_FRAME_BYTES + 1];
    tokio::time::timeout(Duration::from_secs(5), input.write_all(&payload))
        .await
        .expect("host stopped reading before the frame limit")
        .unwrap();
    drop(input);
    let output = tokio::time::timeout(Duration::from_secs(5), child.wait_with_output())
        .await
        .expect("host waited for a newline after exceeding its limit")
        .unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("Forge JSONL frame exceeds its byte limit")
    );
}

struct BrokerProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    consumed_bytes: usize,
    consumed_frames: usize,
}

impl BrokerProcess {
    fn start() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_forge"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        let mut broker = Self {
            child,
            stdin,
            stdout,
            consumed_bytes: 0,
            consumed_frames: 0,
        };
        broker.request(json!({"id": 0, "method": "initialize", "params": {"protocol_version": forge_protocol::WIRE_VERSION}}));
        let handshake = broker.read_response(0);
        assert_eq!(
            handshake.last().unwrap()["result"]["protocol_version"],
            forge_protocol::WIRE_VERSION
        );
        broker
    }

    fn request(&mut self, request: Value) {
        serde_json::to_writer(&mut self.stdin, &request).unwrap();
        self.stdin.write_all(b"\n").unwrap();
        self.stdin.flush().unwrap();
    }

    fn read_response(&mut self, request_id: u64) -> Vec<Value> {
        let mut message_list = Vec::new();
        loop {
            let message = self.read_message();
            let complete = message.get("id").and_then(Value::as_u64) == Some(request_id);
            message_list.push(message);
            if complete {
                return message_list;
            }
        }
    }

    fn read_message(&mut self) -> Value {
        let mut line = String::new();
        assert_ne!(self.stdout.read_line(&mut line).unwrap(), 0);
        let message = serde_json::from_str(&line).unwrap();
        self.consumed_bytes += line.len();
        self.consumed_frames += 1;
        let credit = json!({"id": 0, "method": "transport.consumed", "params": {
            "bytes": self.consumed_bytes, "frames": self.consumed_frames,
        }});
        let encoded = serde_json::to_string(&credit).unwrap() + "\n";
        let _ = self.stdin.write_all(encoded.as_bytes());
        let _ = self.stdin.flush();
        message
    }

    fn read_backend_event(&mut self, expected_kind: &str) -> Value {
        loop {
            let message = self.read_message();
            if message.get("event").and_then(Value::as_str) == Some("backend_event")
                && message.pointer("/payload/kind").and_then(Value::as_str) == Some(expected_kind)
            {
                return message;
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

fn serial_test() -> MutexGuard<'static, ()> {
    BROKER_TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

#[test]
fn streams_mock_backend_events_before_the_jsonl_response() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "stdio-test",
            "backend": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
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
        2
    );
    let backend_kind = planned
        .iter()
        .filter(|message| message.get("event").and_then(Value::as_str) == Some("backend_event"))
        .filter_map(|message| message.pointer("/payload/kind").and_then(Value::as_str))
        .collect::<Vec<_>>();
    assert_eq!(
        backend_kind,
        vec!["timeline_interaction_started", "timeline_node_updated"]
    );
    assert!(event_name.contains(&"interaction_complete"));
    assert!(event_name.contains(&"plan_created"));
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

#[test]
fn opens_a_fork_before_provider_preparation_and_queues_only_the_child_prompt() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "async-fork-test",
            "backend": { "kind": "mock", "command": ["fork-blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let initialized = broker.read_response(1);
    let source_session_id = initialized
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap()
        .to_owned();

    let fork_started = Instant::now();
    broker.request(json!({
        "id": 2,
        "session_id": source_session_id,
        "method": "session.fork",
        "params": { "name": "concurrent child" }
    }));
    let forked = broker.read_response(2);
    assert!(
        fork_started.elapsed() < Duration::from_secs(2),
        "fork response waited for provider preparation"
    );
    let child_session_id = forked
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap()
        .to_owned();
    assert_eq!(
        forked
            .last()
            .unwrap()
            .pointer("/result/session/provider_fork_state/state")
            .and_then(Value::as_str),
        Some("preparing")
    );

    broker.request(json!({
        "id": 3,
        "session_id": child_session_id,
        "method": "prompt.submit",
        "params": { "text": "queued child prompt" }
    }));
    let parent_started = Instant::now();
    broker.request(json!({
        "id": 4,
        "session_id": source_session_id,
        "method": "prompt.submit",
        "params": { "text": "independent parent prompt" }
    }));
    let parent_messages = broker.read_response(4);
    let parent_elapsed = parent_started.elapsed();
    assert!(
        parent_elapsed < Duration::from_secs(2),
        "parent prompt queued behind child provider preparation: {parent_elapsed:?}"
    );
    assert!(parent_messages.last().unwrap().get("error").is_none());
    let child_completed = parent_messages
        .iter()
        .any(|message| message.get("id").and_then(Value::as_u64) == Some(3));
    if !child_completed {
        let child_messages = broker.read_response(3);
        assert!(child_messages.last().unwrap().get("error").is_none());
    }
}

#[test]
fn creates_a_new_session_while_the_source_turn_continues() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "concurrent-new-test",
            "backend": { "kind": "mock", "command": ["session-blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20,
            "new_session_name": ""
        }
    }));
    let initialized = broker.read_response(1);
    let source_session_id = initialized
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap()
        .to_owned();

    broker.request(json!({
        "id": 2,
        "session_id": source_session_id,
        "method": "prompt.submit",
        "params": { "text": "keep the source busy" }
    }));
    let turn_started = broker.read_message();
    assert_eq!(
        turn_started.get("event").and_then(Value::as_str),
        Some("backend_event")
    );

    let new_started = Instant::now();
    broker.request(json!({
        "id": 3,
        "session_id": source_session_id,
        "method": "session.new",
        "params": { "name": "parallel work" }
    }));
    let created = broker.read_response(3);
    let new_elapsed = new_started.elapsed();
    assert!(
        new_elapsed < Duration::from_secs(2),
        "new session response queued behind the source turn: {new_elapsed:?}"
    );
    assert_eq!(
        created
            .last()
            .unwrap()
            .pointer("/result/session/name")
            .and_then(Value::as_str),
        Some("parallel work")
    );
    assert!(
        created
            .last()
            .unwrap()
            .pointer("/result/session/backend_session_id")
            .and_then(Value::as_str)
            .is_none()
    );
    let child_session_id = created
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap()
        .to_owned();

    let source_completed = broker.read_response(2);
    assert!(source_completed.last().unwrap().get("error").is_none());

    broker.request(json!({
        "id": 4,
        "session_id": child_session_id,
        "method": "state.get",
        "params": {}
    }));
    let child_snapshot = broker.read_response(4);
    assert_eq!(
        child_snapshot
            .last()
            .unwrap()
            .pointer("/result/session/id")
            .and_then(Value::as_str),
        Some(child_session_id.as_str())
    );
}

#[test]
fn cancels_a_running_turn_through_the_out_of_band_request_lane() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "cancel-test",
            "backend": { "kind": "mock", "command": ["blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    assert!(
        broker
            .read_response(1)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "keep working until cancelled" }
    }));
    broker.request(json!({ "id": 3, "method": "turn.cancel", "params": {} }));

    let cancellation = broker.read_response(3);
    assert_eq!(
        cancellation
            .last()
            .unwrap()
            .pointer("/result/cancel_requested")
            .and_then(Value::as_bool),
        Some(true)
    );
    let cancelled_turn = broker.read_response(2);
    assert_eq!(
        cancelled_turn
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_cancelled")
    );
    assert!(cancelled_turn.iter().any(|message| {
        message.get("event").and_then(Value::as_str) == Some("backend_event")
            && message.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_interaction_cancelled")
    }));

    broker.request(json!({ "id": 4, "method": "state.get", "params": {} }));
    let snapshot = broker.read_response(4);
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/interaction/0/state")
            .and_then(Value::as_str),
        Some("cancelled")
    );
    broker.request(json!({ "id": 5, "method": "shutdown", "params": {} }));
    broker.read_response(5);
}

#[test]
fn shutdown_cancels_all_sessions_before_its_terminal_response() {
    let _serial_guard = serial_test();
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id":1,"method":"harness.initialize","params":{
            "workspace":workspace.path(),"data_root":data.path(),"client_id":"shutdown-test",
            "backend":{"kind":"mock","command":["visible-blocking"]}
        }
    }));
    let initialized = broker.read_response(1);
    assert!(initialized.last().unwrap().get("error").is_none());
    broker.request(json!({"id":2,"method":"prompt.submit","params":{"text":"first active turn"}}));
    loop {
        let event = broker.read_message();
        assert_ne!(
            event["id"].as_u64(),
            Some(2),
            "first turn ended before shutdown: {event}"
        );
        if event.pointer("/payload/kind").and_then(Value::as_str) == Some("timeline_node_updated") {
            break;
        }
    }
    broker
        .request(json!({"id":3,"method":"session.new","params":{"name":"second active session"}}));
    let created = broker.read_response(3);
    let child = created
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap();
    broker.request(json!({"id":4,"session_id":child,"method":"prompt.submit","params":{"text":"second active turn"}}));
    loop {
        let event = broker.read_message();
        assert_ne!(
            event["id"].as_u64(),
            Some(4),
            "second turn ended before shutdown: {event}"
        );
        if event["session_id"].as_str() == Some(child)
            && event.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_node_updated")
        {
            break;
        }
    }
    broker.request(json!({"id":5,"method":"shutdown","params":{}}));
    let stopped = broker.read_response(5);
    assert_eq!(
        stopped.last().unwrap().pointer("/result/shutdown"),
        Some(&Value::Bool(true))
    );
    for request_id in [2, 4] {
        let response = stopped
            .iter()
            .find(|message| message["id"].as_u64() == Some(request_id))
            .unwrap();
        assert_eq!(
            response.pointer("/error/code").and_then(Value::as_str),
            Some("turn_cancelled")
        );
    }
    assert!(broker.child.wait().unwrap().success());
}

#[test]
fn restarts_a_running_turn_in_write_mode_without_creating_another_interaction() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "restart-test",
            "backend": { "kind": "mock", "command": ["steering"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    assert!(
        broker
            .read_response(1)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "create the requested file" }
    }));
    let started = broker.read_message();
    assert_eq!(
        started.pointer("/payload/kind").and_then(Value::as_str),
        Some("timeline_interaction_started")
    );

    broker.request(json!({
        "id": 3,
        "method": "turn.restart",
        "params": { "mode": "write" }
    }));
    let restart = broker.read_response(3);
    assert_eq!(
        restart
            .last()
            .unwrap()
            .pointer("/result/restart_requested")
            .and_then(Value::as_bool),
        Some(true)
    );
    let cancelled = broker.read_response(2);
    assert_eq!(
        cancelled
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_cancelled")
    );

    broker.request(json!({
        "id": 4,
        "method": "session.execution_mode",
        "params": { "mode": "write" }
    }));
    assert!(
        broker
            .read_response(4)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );
    broker.request(json!({
        "id": 5,
        "method": "interaction.resume",
        "params": { "text": "continue in write mode" }
    }));
    assert!(
        broker
            .read_response(5)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    broker.request(json!({ "id": 6, "method": "state.get", "params": {} }));
    let snapshot = broker.read_response(6);
    let interaction = snapshot
        .last()
        .unwrap()
        .pointer("/result/interaction")
        .and_then(Value::as_array)
        .unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0]["state"], "complete");
    assert_eq!(interaction[0]["node_list"].as_array().unwrap().len(), 1);
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/session/execution_mode")
            .and_then(Value::as_str),
        Some("write")
    );
}

#[test]
fn retracts_an_output_free_planning_turn_and_restores_control_state() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "retract-test",
            "backend": { "kind": "mock", "command": ["blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let initialized = broker.read_response(1);
    assert_eq!(
        initialized
            .last()
            .unwrap()
            .pointer("/result/capability/native_turn_rollback")
            .and_then(Value::as_bool),
        Some(true)
    );

    broker.request(json!({
        "id": 2,
        "method": "session.execution_mode",
        "params": { "mode": "write" }
    }));
    broker.read_response(2);
    let prompt = "/plan reconsider this\nbefore doing anything";
    broker.request(json!({
        "id": 3,
        "method": "prompt.submit",
        "params": { "text": prompt }
    }));
    broker.request(json!({
        "id": 4,
        "method": "turn.cancel",
        "params": { "restore_prompt_if_no_output": true }
    }));

    let cancellation = broker.read_response(4);
    assert_eq!(
        cancellation
            .last()
            .unwrap()
            .pointer("/result/cancel_requested")
            .and_then(Value::as_bool),
        Some(true)
    );
    let retracted = broker.read_response(3);
    assert_eq!(
        retracted
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_retracted")
    );
    assert_eq!(
        retracted
            .last()
            .unwrap()
            .pointer("/error/data/prompt")
            .and_then(Value::as_str),
        Some(prompt)
    );
    assert!(retracted.iter().any(|message| {
        message.get("event").and_then(Value::as_str) == Some("backend_event")
            && message.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_interaction_retracted")
    }));

    broker.request(json!({ "id": 5, "method": "state.get", "params": {} }));
    let snapshot = broker.read_response(5);
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/interaction")
            .and_then(Value::as_array)
            .map(Vec::len),
        Some(0)
    );
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/session/execution_mode")
            .and_then(Value::as_str),
        Some("write")
    );
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/artifact")
            .and_then(Value::as_array)
            .map(Vec::len),
        Some(0)
    );
    broker.request(json!({ "id": 6, "method": "shutdown", "params": {} }));
    broker.read_response(6);
}

#[test]
fn visible_output_prevents_cancelled_turn_retraction() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "visible-cancel-test",
            "backend": { "kind": "mock", "command": ["visible-blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    broker.read_response(1);

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "show output before cancellation" }
    }));
    broker.read_backend_event("timeline_interaction_started");
    broker.read_backend_event("timeline_node_updated");
    broker.request(json!({
        "id": 3,
        "method": "turn.cancel",
        "params": { "restore_prompt_if_no_output": true }
    }));
    broker.read_response(3);
    let cancelled = broker.read_response(2);
    assert_eq!(
        cancelled
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_cancelled")
    );
    assert!(cancelled.iter().any(|message| {
        message.pointer("/payload/kind").and_then(Value::as_str)
            == Some("timeline_interaction_cancelled")
    }));

    broker.request(json!({ "id": 4, "method": "shutdown", "params": {} }));
    broker.read_response(4);
}

#[test]
fn workspace_changes_prevent_cancelled_turn_retraction() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "changed-cancel-test",
            "backend": { "kind": "mock", "command": ["writing-blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    broker.read_response(1);

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "change the workspace" }
    }));
    let started = broker.read_message();
    assert_eq!(
        started.pointer("/payload/kind").and_then(Value::as_str),
        Some("timeline_interaction_started")
    );
    let changed_path = repository.path().join("mock-provider-change.txt");
    let deadline = std::time::Instant::now() + Duration::from_secs(2);
    while !changed_path.exists() && std::time::Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    assert!(changed_path.exists());
    broker.request(json!({
        "id": 3,
        "method": "turn.cancel",
        "params": { "restore_prompt_if_no_output": true }
    }));
    broker.read_response(3);
    let cancelled = broker.read_response(2);
    assert_eq!(
        cancelled
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_cancelled")
    );

    broker.request(json!({ "id": 4, "method": "shutdown", "params": {} }));
    broker.read_response(4);
}

#[test]
fn persists_acknowledged_steering_on_the_active_interaction() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "steer-test",
            "backend": { "kind": "mock", "command": ["visible-blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let initialized = broker.read_response(1);
    assert_eq!(
        initialized
            .last()
            .unwrap()
            .pointer("/result/capability/native_steer")
            .and_then(Value::as_bool),
        Some(true)
    );

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "/plan refactor X" }
    }));
    broker.read_backend_event("timeline_node_updated");
    broker.request(json!({
        "id": 3,
        "method": "turn.steer",
        "params": { "text": "And be sure to modify Y" }
    }));
    let steered = broker.read_response(3);
    assert_eq!(
        steered
            .last()
            .unwrap()
            .pointer("/result/steered")
            .and_then(Value::as_bool),
        Some(true)
    );
    let mut event_list: Vec<_> = steered
        .iter()
        .filter(|message| message.get("id").is_none())
        .cloned()
        .collect();
    while !event_list.iter().any(|message| {
        message.pointer("/payload/kind").and_then(Value::as_str) == Some("timeline_node_updated")
            && message
                .pointer("/payload/data/node/kind")
                .and_then(Value::as_str)
                == Some("steering_prompt")
    }) {
        event_list.push(broker.read_message());
    }
    let steering_event = event_list
        .iter()
        .find(|message| {
            message.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_node_updated")
                && message
                    .pointer("/payload/data/node/kind")
                    .and_then(Value::as_str)
                    == Some("steering_prompt")
        })
        .unwrap();
    assert_eq!(
        steering_event
            .pointer("/payload/kind")
            .and_then(Value::as_str),
        Some("timeline_node_updated")
    );
    assert_eq!(
        steering_event
            .pointer("/payload/data/node/prompt/text")
            .and_then(Value::as_str),
        Some("And be sure to modify Y")
    );

    broker.request(json!({ "id": 4, "method": "turn.cancel", "params": {} }));
    broker.read_response(4);
    broker.read_response(2);
    broker.request(json!({ "id": 5, "method": "state.get", "params": {} }));
    let snapshot = broker.read_response(5);
    let interaction = snapshot
        .last()
        .unwrap()
        .pointer("/result/interaction")
        .and_then(Value::as_array)
        .unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0]["prompt"], "/plan refactor X");
    assert_eq!(
        interaction[0]
            .pointer("/node_list/1/prompt/text")
            .and_then(Value::as_str),
        Some("And be sure to modify Y")
    );
    assert_eq!(
        interaction[0]
            .pointer("/node_list/1/prompt/id")
            .and_then(Value::as_str),
        Some(format!("{}:steering:1", interaction[0]["id"].as_str().unwrap()).as_str())
    );

    broker.request(json!({ "id": 6, "method": "shutdown", "params": {} }));
    broker.read_response(6);
}

#[test]
fn reports_structured_lease_recovery_and_allows_a_new_session() {
    let _serial_guard = serial_test();
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
    let initialize = |id: u64, client_id: &str, action: Option<&str>| {
        json!({
            "id": id,
            "method": "harness.initialize",
            "params": {
                "data_root": data.path(),
                "workspace": repository.path(),
                "client_id": client_id,
                "backend": { "kind": "mock", "command": ["mock"] },
                "model": "mock-model",
                "effort": "low",
                "goal_max_turns": 20,
                "lease_conflict_action": action
            }
        })
    };

    let mut owner = BrokerProcess::start();
    owner.request(initialize(1, "owner", None));
    let owner_snapshot = owner.read_response(1);
    let owner_session_id = owner_snapshot
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap()
        .to_owned();
    owner.request(
        json!({ "id": 2, "method": "prompt.submit", "params": { "text": "prime fork support" } }),
    );
    assert!(
        owner
            .read_response(2)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    let mut blocked = BrokerProcess::start();
    blocked.request(initialize(3, "blocked", None));
    let conflict = blocked.read_response(3);
    let error = conflict.last().unwrap().get("error").unwrap();
    assert_eq!(error["code"], "session_lease_conflict");
    assert_eq!(error["data"]["session_id"], owner_session_id);
    assert_eq!(error["data"]["native_fork"], true);

    let mut replacement = BrokerProcess::start();
    replacement.request(initialize(4, "replacement", Some("new")));
    let replacement_snapshot = replacement.read_response(4);
    let replacement_session_id = replacement_snapshot
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap();
    assert_ne!(replacement_session_id, owner_session_id);

    replacement.request(json!({ "id": 5, "method": "shutdown", "params": {} }));
    replacement.read_response(5);
    owner.request(json!({ "id": 6, "method": "shutdown", "params": {} }));
    owner.read_response(6);
}

#[test]
fn full_request_admission_returns_busy_and_preserves_cancellation() {
    let _serial_guard = serial_test();
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
        "method": "harness.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "cancel-test",
            "backend": { "kind": "mock", "command": ["blocking"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    assert!(
        broker
            .read_response(1)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    broker.request(json!({
        "id": 2,
        "method": "prompt.submit",
        "params": { "text": "keep working until cancelled" }
    }));
    for request_id in 100..164 {
        broker.request(json!({ "id": request_id, "method": "state.get", "params": {} }));
    }
    let rejected = broker.read_response(163);
    assert_eq!(
        rejected
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("busy")
    );
    broker.request(json!({ "id": 3, "method": "turn.cancel", "params": {} }));

    let cancellation = broker.read_response(3);
    assert_eq!(
        cancellation
            .last()
            .unwrap()
            .pointer("/result/cancel_requested")
            .and_then(Value::as_bool),
        Some(true)
    );
    let cancelled_turn = broker.read_response(2);
    assert_eq!(
        cancelled_turn
            .last()
            .unwrap()
            .pointer("/error/code")
            .and_then(Value::as_str),
        Some("turn_cancelled")
    );
    assert!(cancelled_turn.iter().any(|message| {
        message.get("event").and_then(Value::as_str) == Some("backend_event")
            && message.pointer("/payload/kind").and_then(Value::as_str)
                == Some("timeline_interaction_cancelled")
    }));

    broker.request(json!({ "id": 4, "method": "state.get", "params": {} }));
    let snapshot = broker.read_response(4);
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/interaction/0/state")
            .and_then(Value::as_str),
        Some("cancelled")
    );
    broker.request(json!({ "id": 5, "method": "shutdown", "params": {} }));
    broker.read_response(5);
}
