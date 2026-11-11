use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::{Mutex, MutexGuard};

static BROKER_TEST_LOCK: Mutex<()> = Mutex::new(());

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
            .recv()
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

/// Lay out an empty repository on disk, which the host reads in-process, without launching Git.
fn initialize_repository(workspace: &Path) {
    forge_git::test_support::init_repository_with_branch(workspace, "main");
}

fn serial_test() -> MutexGuard<'static, ()> {
    BROKER_TEST_LOCK
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// Keep Background preparation diagnostics after its failed runtime releases the last writer.
#[test]
fn background_model_failure_retains_native_cause_and_request_identity() {
    verify_background_failure_logs(None, BackgroundFailureProvider::Copilot);
}

/// Route diagnostics to their owning Background root rather than the interactive writer.
#[test]
fn background_model_failure_isolates_logging_roots() {
    let interactive_data = tempfile::tempdir().unwrap();
    verify_background_failure_logs(
        Some(interactive_data.path()),
        BackgroundFailureProvider::Copilot,
    );
}

/// Explain a missing configured executable instead of reporting model discovery as a transport loss.
#[test]
fn background_missing_codex_names_executable_and_remedy() {
    let interactive_data = tempfile::tempdir().unwrap();
    verify_background_failure_logs(
        Some(interactive_data.path()),
        BackgroundFailureProvider::Codex,
    );
}

/// Select the real native startup boundary exercised by the missing-executable fixture.
#[derive(Clone, Copy)]
enum BackgroundFailureProvider {
    Copilot,
    Codex,
}

/// Exercise native launch failure through IPC without contacting a live provider.
fn verify_background_failure_logs(
    interactive_data: Option<&Path>,
    provider: BackgroundFailureProvider,
) {
    let provider_name = match provider {
        BackgroundFailureProvider::Copilot => "copilot",
        BackgroundFailureProvider::Codex => "codex",
    };
    let workspace = tempfile::tempdir().unwrap();
    initialize_repository(workspace.path());
    std::fs::write(workspace.path().join("change.txt"), "Background fixture\n").unwrap();
    let data = tempfile::tempdir().unwrap();
    let missing_program = data.path().join(format!("missing-{provider_name}"));
    let mut broker = BrokerProcess::start();
    if let Some(interactive_data) = interactive_data {
        broker.request(json!({
            "id": 40, "method": "session.initialize", "params": {
                "workspace": workspace.path(), "data_root": interactive_data,
                "client_id": "diagnostic-fixture", "provider": {"kind": "mock", "command": ["mock"]}
            }
        }));
        assert!(
            broker
                .read_response(40)
                .pop()
                .unwrap()
                .get("error")
                .is_none()
        );
    }
    let params = json!({
            "operation": "generate", "workspace": workspace.path(), "comparison": "head",
            "generation": {
                "provider": {"kind": provider_name, "command": [missing_program]},
                "model": "default", "data_root": data.path()
            }
    });
    for request_id in [41, 43] {
        broker.request(json!({
            "id": request_id, "method": "repository.generation", "params": params
        }));
        let response = broker.read_response(request_id).pop().unwrap();
        let message = response["error"]["message"].as_str().unwrap();
        match provider {
            BackgroundFailureProvider::Codex => {
                assert!(
                    message.contains("cannot launch provider codex"),
                    "{message}"
                );
                assert!(
                    message.contains(missing_program.to_string_lossy().as_ref()),
                    "{message}"
                );
                assert!(message.contains("executable not found"), "{message}");
                assert!(
                    message.contains("installation or provider settings"),
                    "{message}"
                );
                assert!(!message.contains("Transport"), "{message}");
            }
            BackgroundFailureProvider::Copilot => {
                assert!(message.contains("ModelList"), "{message}");
            }
        }
        assert!(!message.contains("os error"), "{message}");
        assert!(!message.contains("request_id"), "{message}");
        assert!(!message.contains("operation_id"), "{message}");
    }
    broker.request(json!({"id": 42, "method": "host.shutdown", "params": {}}));
    assert!(
        broker
            .read_response(42)
            .pop()
            .unwrap()
            .get("error")
            .is_none()
    );
    assert!(broker.child.wait().unwrap().success());
    let path = forge_harness::storage::state::directory(data.path()).join("harness-trace.jsonl");
    let contents = std::fs::read_to_string(path).unwrap();
    let records = contents
        .lines()
        .map(|line| serde_json::from_str::<Value>(line).unwrap())
        .collect::<Vec<_>>();
    for request_id in [41, 43] {
        let failure = records
            .iter()
            .find(|record| {
                record["event"] == "provider.startup.failed"
                    && record["payload"]["request_id"] == request_id
            })
            .unwrap_or_else(|| panic!("missing native startup diagnostic: {contents}"));
        assert!(
            !failure["payload"]["error"].as_str().unwrap().is_empty(),
            "{failure}"
        );
        if let BackgroundFailureProvider::Codex = provider {
            let native_error = failure["payload"]["error"].as_str().unwrap();
            assert!(native_error.contains("os error"), "{native_error}");
            assert!(
                native_error.contains(missing_program.to_string_lossy().as_ref()),
                "{native_error}"
            );
        }
        assert!(
            failure["payload"]["process_id"].as_u64().unwrap() > 0,
            "{failure}"
        );
        assert!(failure["payload"]["operation_id"].is_string(), "{failure}");
        assert_ne!(failure["session_id"], "global", "{failure}");
        assert!(
            records
                .iter()
                .any(|record| record["event"] == "background.preparation.failed"
                    && record["payload"]["request_id"] == request_id),
            "{contents}"
        );
    }
    assert!(!contents.contains("Background fixture"));
    if let Some(interactive_data) = interactive_data {
        let path =
            forge_harness::storage::state::directory(interactive_data).join("harness-trace.jsonl");
        let contents = std::fs::read_to_string(path).unwrap();
        assert!(!contents.contains("provider.startup.failed"), "{contents}");
    } else {
        for request_id in [41, 43] {
            assert!(
                records.iter().any(|record| record["event"] == "ipc.failed"
                    && record["payload"]["request_id"] == request_id),
                "{contents}"
            );
        }
    }
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
    let path = forge_harness::storage::state::directory(data.path()).join("harness-trace.jsonl");
    let contents = std::fs::read_to_string(path).unwrap();
    let records = contents
        .lines()
        .map(|line| serde_json::from_str::<Value>(line).unwrap())
        .collect::<Vec<_>>();
    assert!(records.iter().any(|record| record["event"] == "ipc.started"
        && record["payload"]["method"] == "session.prompt.submit"));
    assert!(
        records
            .iter()
            .any(|record| record["event"] == "ipc.completed"
                && record["payload"]["elapsed_ms"].is_number())
    );
    assert!(
        records
            .iter()
            .all(|record| record["payload"]["level"] != "trace")
    );
    assert!(!contents.contains("first active turn") && !contents.contains("second active turn"));
}

#[test]
fn reports_structured_lease_recovery_and_allows_a_new_session() {
    let _serial_guard = serial_test();
    let repository = tempfile::tempdir().unwrap();
    initialize_repository(repository.path());
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
