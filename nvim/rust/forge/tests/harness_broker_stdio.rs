use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, ChildStdin, Command, Stdio};
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
    broker.request(json!({"id": 41, "method": "session.initialize", "params": {
        "data_root": data.path(), "workspace": repository.path(), "client_id": "invalid-repository",
        "provider": { "kind": "mock", "command": ["mock"] }
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
    broker.request(json!({"id": 42, "method": "session.initialize", "params": {
        "data_root": data.path(), "workspace": nested, "client_id": "nested-repository",
        "provider": { "kind": "mock", "command": ["mock"] }
    }}));
    let response = broker.read_response(42);
    let workspace = response.last().unwrap()["result"]["session"]["workspace"]
        .as_str()
        .unwrap();
    assert_eq!(
        std::fs::canonicalize(workspace).unwrap(),
        std::fs::canonicalize(repository.path()).unwrap()
    );
    broker.request(json!({"id": 43, "method": "host.shutdown", "params": {}}));
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
            json!({"id": 7, "method": "host.initialize", "params": {"data_root": data_root}});
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
        json!({"id": 1, "method": "host.initialize", "params": {
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
    send(&mut input, json!({"id": 0, "method": "host.transport.consumed", "params": {"bytes": consumed_bytes, "frames": 1}})).await;
    // The first request id after the window is the one that must wait for consumer credit.
    let blocked_request_id = 2 + forge_protocol::MAX_PENDING_FRAMES as u64;
    for request_id in 2..blocked_request_id {
        send(
            &mut input,
            json!({"id": request_id, "method": "session.turn.cancel", "params": {}}),
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
        json!({"id": blocked_request_id, "method": "session.turn.cancel", "params": {}}),
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
    send(&mut input, json!({"id": 0, "method": "host.transport.consumed", "params": {"bytes": consumed_bytes, "frames": 1 + forge_protocol::MAX_PENDING_FRAMES}})).await;
    tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(
        serde_json::from_slice::<Value>(&line).unwrap()["id"],
        blocked_request_id
    );
    let shutdown_request_id = blocked_request_id + 1;
    send(
        &mut input,
        json!({"id": shutdown_request_id, "method": "host.shutdown", "params": {}}),
    )
    .await;
    loop {
        line.clear();
        tokio::time::timeout(Duration::from_secs(5), output.read_until(b'\n', &mut line))
            .await
            .unwrap()
            .unwrap();
        let message: Value = serde_json::from_slice(&line).unwrap();
        if message["id"] == shutdown_request_id {
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
async fn large_invalid_unterminated_request_fails_json_decoding_after_eof() {
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
        .expect("host stopped reading the complete input")
        .unwrap();
    drop(input);
    let output = tokio::time::timeout(Duration::from_secs(5), child.wait_with_output())
        .await
        .expect("host did not finish decoding after EOF")
        .unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("decode Forge initialize request"));
}

struct BrokerProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: std::sync::mpsc::Receiver<std::io::Result<String>>,
    pending_response: std::collections::HashMap<u64, Value>,
    consumed_bytes: usize,
    consumed_frames: usize,
    next_query_id: u64,
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
        let reader = BufReader::new(child.stdout.take().unwrap());
        let (sender, stdout) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            for line in reader.lines() {
                if sender.send(line).is_err() {
                    break;
                }
            }
        });
        let mut broker = Self {
            child,
            stdin,
            stdout,
            pending_response: Default::default(),
            consumed_bytes: 0,
            consumed_frames: 0,
            next_query_id: 10_000,
        };
        broker.request(json!({"id": 0, "method": "host.initialize", "params": {"protocol_version": forge_protocol::WIRE_VERSION}}));
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
        if let Some(response) = self.pending_response.remove(&request_id) {
            return vec![response];
        }
        let mut message_list = Vec::new();
        loop {
            let message = self.read_message();
            let complete = message.get("id").and_then(Value::as_u64) == Some(request_id);
            if !complete && let Some(id) = message.get("id").and_then(Value::as_u64) {
                self.pending_response.insert(id, message.clone());
            }
            message_list.push(message);
            if complete {
                return message_list;
            }
        }
    }

    fn capture_session(&mut self, request_id: u64) -> String {
        let response = self.read_response(request_id).pop().unwrap();
        let session_id = response["result"]["session"]["id"]
            .as_str()
            .unwrap_or_else(|| panic!("session initialization failed: {response}"))
            .to_owned();
        self.pending_response.insert(request_id, response);
        session_id
    }

    fn read_message(&mut self) -> Value {
        let line = self
            .stdout
            .recv_timeout(Duration::from_secs(30))
            .expect("broker did not produce a JSONL message within 30 seconds")
            .unwrap();
        let message = serde_json::from_str(&line).unwrap();
        self.consumed_bytes += line.len() + 1;
        self.consumed_frames += 1;
        let credit = json!({"id": 0, "method": "host.transport.consumed", "params": {
            "bytes": self.consumed_bytes, "frames": self.consumed_frames,
        }});
        let encoded = serde_json::to_string(&credit).unwrap() + "\n";
        let _ = self.stdin.write_all(encoded.as_bytes());
        let _ = self.stdin.flush();
        message
    }

    /// Arm a mock response hold so the session's next prompt stays running until the test
    /// cancels it. The hold replaces fixed blocking scenarios that mock commands once selected.
    fn hold_next_prompt(&mut self, session_id: &str) -> String {
        let query_id = self.next_query_id;
        self.next_query_id += 1;
        self.request(json!({"id": query_id, "session_id": session_id,
            "method": "session.provider.mock_response", "params": {"operation": "hold"}}));
        let response = self.read_response(query_id).pop().unwrap();
        response["result"]["id"]
            .as_str()
            .unwrap_or_else(|| panic!("mock hold was not armed: {response}"))
            .to_owned()
    }

    /// Block until a prompt reaches the armed hold, which proves its provider turn is running.
    fn wait_for_held_prompt(&mut self, session_id: &str, hold_id: &str) -> Value {
        let query_id = self.next_query_id;
        self.next_query_id += 1;
        self.request(json!({"id": query_id, "session_id": session_id,
            "method": "session.provider.mock_response",
            "params": {"operation": "wait", "id": hold_id}}));
        let response = self.read_response(query_id).pop().unwrap();
        assert!(response.get("error").is_none(), "{response}");
        response["result"].clone()
    }

    fn wait_for_exchanges(
        &mut self,
        turn_request_id: Option<u64>,
        session_id: Option<&str>,
        ready: impl Fn(&Value) -> bool,
    ) -> Value {
        let deadline = Instant::now() + Duration::from_secs(5);
        loop {
            let query_id = self.next_query_id;
            self.next_query_id += 1;
            self.request(
                json!({"id":query_id,"session_id":session_id,"method":"session.exchange.list","params":{}}),
            );
            let messages = self.read_response(query_id);
            let response = messages.last().unwrap();
            assert!(response.get("error").is_none(), "{response}");
            let snapshot = &response["result"];
            if let Some(turn_request_id) = turn_request_id {
                // Prompt submission answers once the owner accepts the input, so the turn's
                // liveness comes from the exchange record rather than from that response.
                if let Some(accepted) = self.pending_response.get(&turn_request_id) {
                    assert!(accepted.get("error").is_none(), "{accepted}");
                }
                assert!(
                    exchanges_unsettled(snapshot),
                    "turn settled before the expected live state: {snapshot}"
                );
            }
            if ready(snapshot) {
                return snapshot.clone();
            }
            assert!(
                Instant::now() < deadline,
                "live state did not arrive: {snapshot}"
            );
            std::thread::sleep(Duration::from_millis(10));
        }
    }
}

/// Report whether every listed exchange is still queued or running its provider turn.
fn exchanges_unsettled(snapshot: &Value) -> bool {
    snapshot.as_array().is_some_and(|exchanges| {
        exchanges
            .iter()
            .all(|exchange| matches!(exchange["state"].as_str(), Some("queued" | "running")))
    })
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

/// Select the failing-runtime fixture through the same registry available to every client.
fn registered_provider(broker: &mut BrokerProcess) -> String {
    broker.request(json!({"id": 900, "method": "provider.registry.list", "params": {}}));
    broker.read_response(900).last().unwrap()["result"]["default_provider"]
        .as_str()
        .expect("registry supplies a default provider")
        .to_owned()
}

fn serial_test() -> MutexGuard<'static, ()> {
    BROKER_TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// Controller construction resolves inherited model tuning against the provider's workspace
/// catalog, so an unavailable provider executable rejects initialization itself. The host must
/// report that rejection on the request and keep serving later sessions.
#[test]
fn unavailable_provider_rejects_initialization_without_stopping_host() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    let provider = registered_provider(&mut broker);
    broker.request(json!({
        "id": 1, "method": "session.initialize", "params": {
            "data_root": data.path(), "workspace": repository.path(),
            "client_id": "unavailable-provider-test",
            "provider": { "kind": provider, "command": ["forge-unavailable-provider-fixture"] },
            "model": "fixture", "effort": "medium"
        }
    }));
    let rejected = broker.read_response(1).pop().unwrap();
    assert_eq!(rejected["error"]["code"], "initialize_failed", "{rejected}");
    broker.request(json!({
        "id": 2, "method": "session.initialize", "params": {
            "data_root": data.path(), "workspace": repository.path(),
            "client_id": "available-provider-test",
            "provider": { "kind": "mock", "command": ["mock"] }
        }
    }));
    let session_id = broker.capture_session(2);
    broker.request(
        json!({"session_id": session_id, "id": 3, "method": "session.state.get", "params": {} }),
    );
    assert!(
        broker
            .read_response(3)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );
    broker.request(json!({ "id": 4, "method": "host.shutdown", "params": {} }));
    assert_eq!(
        broker.read_response(4).last().unwrap()["result"]["shutdown"],
        true
    );
}

#[test]
fn streams_plan_document_changes_after_command_admission() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    std::fs::write(repository.path().join(".forge.json"), br#"{"checks":{}}"#).unwrap();
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
    std::fs::write(repository.path().join(".forge.json"), r#"{"checks":{}}"#).unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "stdio-test",
            "provider": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    let initialized = broker.read_response(1);
    assert!(initialized.last().unwrap().get("error").is_none());

    broker.request(json!({"session_id": fixture_session_id,
        "id": 2,
        "method": "session.prompt.submit",
        "params": { "text": "/plan build the feature" }
    }));
    let mut planned = broker.read_response(2);
    assert_eq!(planned.last().unwrap()["result"]["state"], "completed");
    while !planned
        .iter()
        .any(|message| message["event"] == "plan_created")
        || !planned
            .iter()
            .any(|message| message["event"] == "exchange_complete")
    {
        planned.push(broker.read_message());
    }
    let notifications = planned
        .iter()
        .filter(|message| message["event"] == "document_changed")
        .collect::<Vec<_>>();
    assert!(
        !notifications.is_empty(),
        "plan changes must stream after admission: {planned:?}"
    );
    let mut previous_revision = 0;
    for notification in notifications {
        let revision = notification["payload"]["revision"].as_u64().unwrap();
        assert!(revision > previous_revision, "{notification}");
        previous_revision = revision;
    }
    assert!(
        !planned.iter().any(|message| message
            .pointer("/payload/kind")
            .and_then(Value::as_str)
            .is_some_and(|kind| kind == "assistant_message" || kind.starts_with("timeline_"))),
        "Rust-owned source content must stay behind document notifications: {planned:?}"
    );
    assert!(
        planned
            .iter()
            .any(|message| message["event"] == "exchange_complete")
    );
    assert!(
        planned
            .iter()
            .any(|message| message["event"] == "plan_created")
    );
    assert_eq!(
        planned
            .iter()
            .find(|message| message["event"] == "plan_created")
            .unwrap()
            .pointer("/payload/plan/state")
            .and_then(Value::as_str),
        Some("awaiting_review")
    );

    broker.request(json!({ "id": 3, "method": "host.shutdown", "params": {} }));
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
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "async-fork-test",
            "provider": { "kind": "mock", "command": ["fork-blocking"] },
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
        "method": "session.prompt.submit",
        "params": { "text": "queued child prompt" }
    }));
    let parent_started = Instant::now();
    broker.request(json!({
        "id": 4,
        "session_id": source_session_id,
        "method": "session.prompt.submit",
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
        assert!(
            child_messages.last().unwrap().get("error").is_none(),
            "{:?}",
            child_messages.last()
        );
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
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "concurrent-new-test",
            "provider": { "kind": "mock", "command": ["mock"] },
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

    let hold_id = broker.hold_next_prompt(&source_session_id);
    broker.request(json!({
        "id": 2,
        "session_id": source_session_id,
        "method": "session.prompt.submit",
        "params": { "text": "keep the source busy" }
    }));
    broker.wait_for_held_prompt(&source_session_id, &hold_id);
    broker.wait_for_exchanges(Some(2), Some(&source_session_id), |snapshot| {
        snapshot.pointer("/0/state").and_then(Value::as_str) == Some("running")
    });

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
            .pointer("/result/session/provider_session_id")
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
        "method": "session.state.get",
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
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "cancel-test",
            "provider": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    assert!(
        broker
            .read_response(1)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    let hold_id = broker.hold_next_prompt(&fixture_session_id);
    broker.request(json!({"session_id": fixture_session_id,
        "id": 2,
        "method": "session.prompt.submit",
        "params": { "text": "keep working until cancelled" }
    }));
    broker.wait_for_held_prompt(&fixture_session_id, &hold_id);
    broker.request(json!({"session_id": fixture_session_id,  "id": 3, "method": "session.turn.cancel", "params": {} }));

    let cancellation = broker.read_response(3);
    assert_eq!(
        cancellation
            .last()
            .unwrap()
            .pointer("/result/cancel_requested")
            .and_then(Value::as_bool),
        Some(true)
    );
    // Prompt submission answers at owner acceptance; the exchange record carries the cancellation.
    let accepted_turn = broker.read_response(2);
    assert!(
        accepted_turn.last().unwrap().get("error").is_none(),
        "{accepted_turn:?}"
    );
    assert!(
        cancellation
            .iter()
            .chain(&accepted_turn)
            .any(|message| message["event"] == "document_changed")
    );
    broker.wait_for_exchanges(None, Some(&fixture_session_id), |snapshot| {
        snapshot.pointer("/0/state").and_then(Value::as_str) == Some("cancelled")
    });
    broker.request(json!({ "id": 5, "method": "host.shutdown", "params": {} }));
    broker.read_response(5);
}

#[test]
fn shutdown_cancels_all_sessions_before_its_terminal_response() {
    let _serial_guard = serial_test();
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id":1,"method":"session.initialize","params":{
            "workspace":workspace.path(),"data_root":data.path(),"client_id":"shutdown-test",
            "provider":{"kind":"mock","command":["mock"]}
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    let initialized = broker.read_response(1);
    assert!(initialized.last().unwrap().get("error").is_none());
    let first_hold_id = broker.hold_next_prompt(&fixture_session_id);
    broker.request(
        json!({"id":2,"session_id":fixture_session_id,"method":"session.prompt.submit","params":{"text":"first active turn"}}),
    );
    broker.wait_for_held_prompt(&fixture_session_id, &first_hold_id);
    broker
        .request(json!({"id":3,"method":"session.new","params":{"name":"second active session"}}));
    let created = broker.read_response(3);
    let child = created
        .last()
        .unwrap()
        .pointer("/result/session/id")
        .and_then(Value::as_str)
        .unwrap();
    let second_hold_id = broker.hold_next_prompt(child);
    broker.request(json!({"id":4,"session_id":child,"method":"session.prompt.submit","params":{"text":"second active turn"}}));
    broker.wait_for_held_prompt(child, &second_hold_id);
    broker.request(json!({"id":5,"method":"host.shutdown","params":{}}));
    let stopped = broker.read_response(5);
    assert_eq!(
        stopped.last().unwrap().pointer("/result/shutdown"),
        Some(&Value::Bool(true))
    );
    assert!(broker.child.wait().unwrap().success());
    // Both prompts were acknowledged at acceptance, so the durable exchange records carry the
    // cancellation that shutdown committed before its terminal response.
    let store = forge_harness::storage::SqliteStore::open(data.path()).unwrap();
    for session_id in [fixture_session_id.as_str(), child] {
        let exchanges = store.list_exchange(session_id).unwrap();
        assert_eq!(exchanges.len(), 1, "{session_id}");
        let exchange = serde_json::to_value(&exchanges[0]).unwrap();
        assert_eq!(exchange["state"], "cancelled", "{exchange}");
    }
}

#[test]
fn restarts_a_running_turn_with_updated_permissions_without_creating_another_interaction() {
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
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "restart-test",
            "provider": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    assert!(
        broker
            .read_response(1)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );

    let hold_id = broker.hold_next_prompt(&fixture_session_id);
    broker.request(json!({"session_id": fixture_session_id,
        "id": 2,
        "method": "session.prompt.submit",
        "params": { "text": "create the requested file" }
    }));
    broker.wait_for_held_prompt(&fixture_session_id, &hold_id);

    broker.request(json!({"session_id": fixture_session_id,
        "id": 3,
        "method": "session.command.apply",
        "params": {
            "operation_id": "restart-with-updated-permissions",
            "action": "permission",
            "change":{"kind":"set_approval","mode":"allow"}
        }
    }));
    let restart = broker.read_response(3);
    assert_eq!(
        restart
            .last()
            .unwrap()
            .pointer("/result/state")
            .and_then(Value::as_str),
        Some("completed"),
        "{restart:?}"
    );
    // Prompt submission answers at owner acceptance rather than at turn settlement.
    let accepted = broker.read_response(2);
    assert!(
        accepted.last().unwrap().get("error").is_none(),
        "{accepted:?}"
    );
    // The restarted turn finds no armed hold, so it completes inside the same interaction.
    broker.wait_for_exchanges(None, Some(&fixture_session_id), |snapshot| {
        snapshot.pointer("/0/state").and_then(Value::as_str) == Some("complete")
    });

    broker.request(json!({"session_id": fixture_session_id,  "id": 6, "method": "session.state.get", "params": {} }));
    let snapshot = broker.read_response(6);
    broker.request(json!({"session_id": fixture_session_id,  "id": 7, "method": "session.exchange.list", "params": {} }));
    let exchanges = broker.read_response(7);
    let interaction = exchanges.last().unwrap()["result"].as_array().unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0]["state"], "complete");
    assert_eq!(interaction[0]["node_list"].as_array().unwrap().len(), 1);
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/session/approval_mode")
            .and_then(Value::as_str),
        Some("allow")
    );
}

#[test]
fn retracts_an_output_free_planning_turn_and_restores_control_state() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    std::fs::write(repository.path().join(".forge.json"), br#"{"checks":{}}"#).unwrap();
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
    std::fs::write(repository.path().join(".forge.json"), r#"{"checks":{}}"#).unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "retract-test",
            "provider": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    let initialized = broker.read_response(1);
    assert_eq!(
        initialized
            .last()
            .unwrap()
            .pointer("/result/capability/native_turn_rollback")
            .and_then(Value::as_bool),
        Some(true)
    );

    broker.request(json!({"session_id": fixture_session_id,
        "id": 2,
        "method": "session.command.apply",
        "params": { "operation_id":"retract-permission-setup", "action":"permission", "change":{"kind":"set_approval","mode":"ask"} }
    }));
    // Owner commands reply once, after the permission change has completed.
    let permission = broker.read_response(2);
    assert_eq!(
        permission.last().unwrap()["result"]["state"],
        "completed",
        "{permission:?}"
    );
    let hold_id = broker.hold_next_prompt(&fixture_session_id);
    let prompt = "/plan reconsider this\nbefore doing anything";
    broker.request(json!({"session_id": fixture_session_id,
        "id": 3,
        "method": "session.prompt.submit",
        "params": { "text": prompt }
    }));
    assert_eq!(
        broker.read_response(3).last().unwrap()["result"]["state"],
        "completed"
    );
    broker.wait_for_held_prompt(&fixture_session_id, &hold_id);
    broker.request(
        json!({"session_id":fixture_session_id,"id":8,"method":"session.input.get","params":{}}),
    );
    let retained_input = broker.read_response(8).last().unwrap()["result"]["messages"][0].clone();
    assert!(retained_input["id"].is_string(), "{retained_input}");
    // Cancellation pauses the plan itself. Pausing first would revoke the running turn's task
    // authority and persist it as an ordinary cancelled exchange, which is not a retraction.
    broker.request(json!({"session_id": fixture_session_id,
        "id": 4,
        "method": "session.turn.cancel",
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
    // Retraction settles after the cancellation response. The owner publishes it as an input
    // update that removes the prompt from the queue and leaves the queue suspended, so wait for
    // that event instead of polling.
    let is_retraction = |message: &Value| {
        message["event"] == "input_changed"
            && message["payload"]["retracted"] == retained_input["id"]
    };
    let mut retraction = cancellation
        .iter()
        .rev()
        .find(|message| is_retraction(message))
        .cloned();
    while retraction.is_none() {
        let message = broker.read_message();
        if is_retraction(&message) {
            retraction = Some(message);
        }
    }
    let retraction = retraction.unwrap();
    assert_eq!(retraction["payload"]["suspended"], true, "{retraction}");
    assert_eq!(
        retraction["payload"]["snapshot"]["messages"]
            .as_array()
            .unwrap()
            .len(),
        0,
        "{retraction}"
    );

    broker.request(json!({"session_id": fixture_session_id,  "id": 5, "method": "session.state.get", "params": {} }));
    let snapshot = broker.read_response(5);
    broker.request(json!({"session_id": fixture_session_id,  "id": 7, "method": "session.exchange.list", "params": {} }));
    let exchanges = broker.read_response(7);
    assert_eq!(
        exchanges.last().unwrap()["result"]
            .as_array()
            .unwrap()
            .len(),
        0
    );
    assert_eq!(
        snapshot
            .last()
            .unwrap()
            .pointer("/result/session/approval_mode")
            .and_then(Value::as_str),
        Some("ask")
    );
    assert_eq!(
        snapshot.last().unwrap()["result"]["session"]["active_task"]["kind"],
        "plan"
    );
    assert_eq!(
        snapshot.last().unwrap()["result"]["session"]["active_task"]["plan"]["paused"],
        true
    );
    broker.request(json!({ "id": 6, "method": "host.shutdown", "params": {} }));
    broker.read_response(6);
}

#[test]
fn persists_acknowledged_steering_on_the_active_interaction() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    std::fs::write(repository.path().join(".forge.json"), br#"{"checks":{}}"#).unwrap();
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
    std::fs::write(repository.path().join(".forge.json"), r#"{"checks":{}}"#).unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = BrokerProcess::start();
    broker.request(json!({
        "id": 1,
        "method": "session.initialize",
        "params": {
            "data_root": data.path(),
            "workspace": repository.path(),
            "client_id": "steer-test",
            "provider": { "kind": "mock", "command": ["mock"] },
            "model": "mock-model",
            "effort": "low",
            "goal_max_turns": 20
        }
    }));
    let fixture_session_id = broker.capture_session(1);
    let initialized = broker.read_response(1);
    assert_eq!(
        initialized
            .last()
            .unwrap()
            .pointer("/result/capability/native_steer")
            .and_then(Value::as_bool),
        Some(true)
    );

    let hold_id = broker.hold_next_prompt(&fixture_session_id);
    broker.request(json!({"session_id": fixture_session_id,
        "id": 2,
        "method": "session.prompt.submit",
        "params": { "text": "/plan refactor X" }
    }));
    broker.wait_for_held_prompt(&fixture_session_id, &hold_id);
    broker.request(json!({"session_id": fixture_session_id,
        "id": 3,
        "method": "session.turn.steer",
        "params": { "text": "And be sure to modify Y" }
    }));
    let steered = broker.read_response(3);
    assert_eq!(
        steered
            .last()
            .unwrap()
            .pointer("/result/outcome/result/change/change")
            .and_then(Value::as_str),
        Some("submitted")
    );
    let snapshot = broker.wait_for_exchanges(None, Some(&fixture_session_id), |snapshot| {
        snapshot[0]["node_list"].as_array().is_some_and(|nodes| {
            nodes.iter().any(|node| {
                node["kind"] == "exchange_input"
                    && node["prompt"]["text"] == "And be sure to modify Y"
            })
        })
    });
    let steering = snapshot[0]["node_list"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| {
            node["kind"] == "exchange_input" && node["prompt"]["text"] == "And be sure to modify Y"
        })
        .unwrap()
        .clone();

    broker.request(json!({"session_id": fixture_session_id,  "id": 4, "method": "session.turn.cancel", "params": {} }));
    broker.read_response(4);
    broker.read_response(2);
    broker.request(json!({"session_id": fixture_session_id,  "id": 5, "method": "session.exchange.list", "params": {} }));
    let snapshot = broker.read_response(5);
    let interaction = snapshot
        .last()
        .unwrap()
        .pointer("/result")
        .and_then(Value::as_array)
        .unwrap();
    assert_eq!(interaction.len(), 1);
    assert!(
        interaction[0]["prompt"]
            .as_str()
            .unwrap()
            .contains("refactor X")
    );
    assert!(
        interaction[0]["node_list"]
            .as_array()
            .unwrap()
            .contains(&steering)
    );

    broker.request(json!({ "id": 6, "method": "host.shutdown", "params": {} }));
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
            "method": "session.initialize",
            "params": {
                "data_root": data.path(),
                "workspace": repository.path(),
                "client_id": client_id,
                "provider": { "kind": "mock", "command": ["mock"] },
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
        json!({ "id": 2, "session_id": owner_session_id, "method": "session.prompt.submit", "params": { "text": "prime fork support" } }),
    );
    assert!(
        owner
            .read_response(2)
            .last()
            .unwrap()
            .get("error")
            .is_none()
    );
    // Prompt submission answers at acceptance. Native fork support follows from the native
    // conversation that the first turn opens, so wait for that turn to finish.
    owner.wait_for_exchanges(None, Some(&owner_session_id), |snapshot| {
        snapshot.pointer("/0/state").and_then(Value::as_str) == Some("complete")
    });

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

    replacement.request(json!({ "id": 5, "method": "host.shutdown", "params": {} }));
    replacement.read_response(5);
    owner.request(json!({ "id": 6, "method": "host.shutdown", "params": {} }));
    owner.read_response(6);
}
