// This fixture builds storage through a handle of its own, outside any running host.
#![allow(clippy::disallowed_methods)]
use base64::{Engine, engine::general_purpose::STANDARD};
use serde_json::{Value, json};
use std::process::Stdio;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

struct Host {
    process: tokio::process::Child,
    input: tokio::process::ChildStdin,
    output: BufReader<tokio::process::ChildStdout>,
    consumed_bytes: usize,
    consumed_frames: usize,
    progress: Vec<Value>,
    terminal_request: Option<u64>,
    recovery: tempfile::TempDir,
}

impl Host {
    async fn start() -> Self {
        let mut command = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"));
        let mut process = command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let input = process.stdin.take().unwrap();
        let output = BufReader::new(process.stdout.take().unwrap());
        let mut host = Self {
            process,
            input,
            output,
            consumed_bytes: 0,
            consumed_frames: 0,
            progress: Vec::new(),
            terminal_request: None,
            recovery: tempfile::tempdir().unwrap(),
        };
        let recovery_directory = host.recovery.path().join("forge/recovery/github/v1");
        let response = host
            .request(
                1,
                "host.initialize",
                json!({"protocol_version": forge_protocol::WIRE_VERSION,
                    "recovery_directory":recovery_directory}),
            )
            .await;
        assert_eq!(
            response["result"]["protocol_version"],
            forge_protocol::WIRE_VERSION,
            "{response}"
        );
        host
    }

    async fn request(&mut self, id: u64, method: &str, params: Value) -> Value {
        self.exchange(json!({"id":id,"method":method,"params":params}))
            .await
    }

    async fn request_for(
        &mut self,
        session_id: &str,
        id: u64,
        method: &str,
        params: Value,
    ) -> Value {
        self.exchange(json!({"id":id,"method":method,"params":params,"session_id":session_id}))
            .await
    }

    async fn exchange(&mut self, request: Value) -> Value {
        let id = request["id"].as_u64().unwrap();
        let method = request["method"].as_str().unwrap();
        if method == "host.shutdown" {
            self.terminal_request = Some(id);
        }
        let encoded = serde_json::to_vec(&request).unwrap();
        self.input.write_all(&encoded).await.unwrap();
        self.input.write_all(b"\n").await.unwrap();
        self.response(id).await
    }

    async fn response(&mut self, id: u64) -> Value {
        let mut assembled = String::new();
        let mut sequence = 0;
        let mut part_count = 0;
        let mut total_bytes = 0;
        loop {
            let mut bytes = Vec::new();
            self.output.read_until(b'\n', &mut bytes).await.unwrap();
            assert!(!bytes.is_empty());
            assert!(bytes.len() <= forge_protocol::MAX_FRAME_BYTES);
            let response: Value = serde_json::from_slice(&bytes).unwrap();
            self.consumed_bytes += bytes.len();
            self.consumed_frames += 1;
            let credit = serde_json::to_vec(&json!({"id":0,"method":"host.transport.consumed","params":{"bytes":self.consumed_bytes,"frames":self.consumed_frames}})).unwrap();
            let terminal = self.terminal_request == Some(id)
                && response["id"].as_u64() == Some(id)
                && response.get("error").is_none();
            if !terminal {
                self.input
                    .write_all(&credit)
                    .await
                    .expect("host closed before consuming response credit");
                self.input
                    .write_all(b"\n")
                    .await
                    .expect("host closed during response credit");
            }
            if response.get("request_id").is_some() {
                assert_eq!(response["request_id"], id);
                if response["event"] == "result.part" {
                    let part = &response["payload"];
                    if sequence == 0 {
                        part_count = part["part_count"].as_u64().unwrap();
                        total_bytes = part["total_bytes"].as_u64().unwrap();
                    }
                    assert_eq!(part["sequence"], sequence);
                    assert_eq!(part["part_count"], part_count);
                    assert_eq!(part["total_bytes"], total_bytes);
                    assembled.push_str(part["payload"].as_str().unwrap());
                    sequence += 1;
                    continue;
                }
                if response["event"] == "result.complete" {
                    assert_eq!(sequence, part_count);
                    assert_eq!(assembled.len() as u64, total_bytes);
                    assert_eq!(response["payload"]["part_count"], part_count);
                    assert_eq!(response["payload"]["total_bytes"], total_bytes);
                    let result: Value = serde_json::from_str(&assembled).unwrap();
                    assert_eq!(result["id"], id);
                    return result;
                }
                self.progress.push(response);
                continue;
            }
            if response.get("session_id").is_some() && response.get("event").is_some() {
                self.progress.push(response);
                continue;
            }
            assert_eq!(response["id"], id);
            return response;
        }
    }

    async fn stop(mut self) {
        let response = self.request(1000, "host.shutdown", json!({})).await;
        assert!(response.get("error").is_none(), "{response}");
        drop(self.input);
        assert!(self.process.wait().await.unwrap().success());
    }
}

#[tokio::test]
async fn reopening_large_harness_history_transfers_document_without_poisoning_host() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let params = json!({
        "workspace": workspace.path(), "data_root": data.path(), "client_id": "large-history",
        "provider": {"kind":"mock", "command":[]}
    });
    let mut host = Host::start().await;
    let initialized = host.request(2, "session.initialize", params.clone()).await;
    let session = &initialized["result"]["session"];
    assert!(session["id"].is_string(), "{initialized}");
    let session_id = session["id"].clone();
    let agent_id = session["primary_agent_id"].clone();
    host.stop().await;

    let prompt = "Saved history ".repeat(50_000);
    let exchange: forge_harness::exchange::Exchange = serde_json::from_value(json!({
        "id":"large-history", "session_id":session_id, "agent_id":agent_id,
        "ordinal":1, "prompt":prompt, "kind":"chat", "state":"complete",
        "created_at_ms":1, "completed_at_ms":2,
        "attributed_matches_checkpoint":false, "node_list":[]
    }))
    .unwrap();
    {
        let mut store = forge_harness::storage::SqliteStore::open(data.path()).unwrap();
        store.save_exchange(&exchange).unwrap();
    }
    let mut host = Host::start().await;
    let mut params = params;
    params["session_id"] = session_id.clone();
    let reopened = host.request(2, "session.initialize", params).await;
    assert_eq!(reopened["result"]["session"]["id"], session_id);
    let document = host
        .request_for(
            session_id.as_str().unwrap(),
            3,
            "session.document",
            json!({
                "operation":"open", "document":"large-history", "view":"history-view",
                "width":forge_buffer::width::WidthProfile::default(),
            }),
        )
        .await;
    let snapshot: forge_buffer::patch::BufferSnapshot =
        serde_json::from_value(document["result"]["transcript"].clone())
            .expect("history document response");
    let preview = snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "large-history:prompt")
        .expect("reopened prompt preview");
    assert!(preview.text.row_count() <= 3);
    let node = preview.metadata.node.as_ref().expect("prompt node");
    assert_eq!(node.display, forge_buffer::node::NodeDisplay::Preview);
    let expanded = host
        .request_for(
            session_id.as_str().unwrap(),
            30,
            "session.document",
            json!({
                "operation":"node", "document":"large-history", "view":"history-view",
                "sequence":1, "node":node.id.0, "generation":node.generation,
                "action":"set_expansion", "expanded":true, "rows":48,
                "width":forge_buffer::width::WidthProfile::default(),
            }),
        )
        .await;
    assert!(expanded.get("error").is_none(), "{expanded}");
    let document = host
        .request_for(
            session_id.as_str().unwrap(),
            31,
            "session.document",
            json!({
                "operation":"snapshot", "document":"large-history",
            }),
        )
        .await;
    let snapshot: forge_buffer::patch::BufferSnapshot =
        serde_json::from_value(document["result"].clone())
            .expect("expanded history document response");
    let displayed = snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "large-history:prompt")
        .expect("reopened prompt block")
        .text
        .wire_rows()
        .join("\n");
    assert!(
        displayed
            .split_whitespace()
            .filter(|word| *word != "●")
            .eq(prompt.split_whitespace()),
        "reopened transcript must preserve every saved prompt word"
    );
    assert!(
        serde_json::to_vec(&document).unwrap().len() > forge_protocol::MAX_SNAPSHOT_PART_BYTES,
        "the reopened history must exceed one snapshot transfer part"
    );
    let subsequent = host
        .request(
            4,
            "repository.revision.list",
            json!({"workspace":workspace.path()}),
        )
        .await;
    assert_eq!(
        subsequent["error"]["message"],
        "revision completion requires a Git repository"
    );
    host.stop().await;
}

/// A compiled commit-editor fixture whose directory lives as long as the test that uses it.
struct CommitEditorExecutable {
    directory: tempfile::TempDir,
}

impl CommitEditorExecutable {
    /// Create the editor by compiling the fixture source into a test-owned directory.
    fn compile() -> Self {
        let directory = tempfile::tempdir().unwrap();
        let source = directory.path().join("commit_editor.rs");
        std::fs::write(&source, include_str!("fixtures/commit_editor.rs")).unwrap();
        let editor = Self { directory };
        let compiled = std::process::Command::new("rustup")
            .args(["run", "1.94.0", "rustc", "--edition=2024"])
            .arg(source)
            .arg("-o")
            .arg(editor.path())
            .output()
            .unwrap();
        assert!(
            compiled.status.success(),
            "{}",
            String::from_utf8_lossy(&compiled.stderr)
        );
        editor
    }

    /// Get the compiled editor's path.
    fn path(&self) -> std::path::PathBuf {
        self.directory.path().join(if cfg!(windows) {
            "commit-editor.exe"
        } else {
            "commit-editor"
        })
    }
}

/// Git evaluates GIT_EDITOR through its shell, including on Git for Windows.
/// Quote one executable path as a POSIX shell word so Windows separators and spaces reach it.
fn git_editor_command(path: &std::path::Path) -> String {
    let path = path.to_string_lossy();
    format!("'{}'", path.replace('\'', "'\"'\"'"))
}

#[tokio::test]
async fn commit_editor_options_and_progress_receipts_use_the_copied_host_fixture() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "user.name", "Forge Fixture"]);
    git(root, &["config", "user.email", "forge@example.test"]);
    std::fs::write(root.join("commit.txt"), "commit through editor\n").unwrap();
    git(root, &["add", "commit.txt"]);

    let editor_executable = CommitEditorExecutable::compile();
    let editor = git_editor_command(&editor_executable.path());
    let nvim_server = "fixture-nvim-server";
    let mut host = Host::start().await;
    let prepared = host
        .request(
            2,
            "repository.write",
            json!({
                "operation":"prepare",
                "workspace":root,
                "action":{
                    "kind":"commit_editor",
                    "command":editor,
                    "nvim_server":nvim_server,
                },
            }),
        )
        .await;
    let intent = prepared["result"]["intent"]
        .as_str()
        .expect("commit editor intent");
    let submitted = host
        .request(
            3,
            "repository.write",
            json!({"operation":"submit","intent":intent}),
        )
        .await;
    assert_eq!(submitted["result"]["success"], true, "{submitted}");
    assert_eq!(
        submitted["result"]["target"][0]["completion"], "completed",
        "{submitted}"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("commit-editor-nvim")).unwrap(),
        nvim_server
    );
    let target = std::fs::read_to_string(root.join("commit-editor-target")).unwrap();
    assert!(target.ends_with("COMMIT_EDITMSG"), "{target}");
    assert!(
        host.progress.iter().any(|event| {
            event["request_id"] == 3
                && event["event"] == "repository.write.progress"
                && matches!(
                    event["payload"]["stream"].as_str(),
                    Some("stdout") | Some("stderr")
                )
                && event["payload"]["sequence"].is_u64()
                && event["payload"]["bytes"].as_str().is_some_and(|bytes| {
                    STANDARD
                        .decode(bytes)
                        .is_ok_and(|decoded| !decoded.is_empty())
                })
        }),
        "missing bounded commit progress receipt: {:#?}",
        host.progress
            .iter()
            .map(|event| (
                event["event"].clone(),
                event["payload"]["kind"].clone(),
                event["payload"]["data"].clone()
            ))
            .collect::<Vec<_>>()
    );
    let operation_id = submitted["result"]["operation_id"]
        .as_u64()
        .expect("operation receipt");
    let acknowledged = host
        .request(
            4,
            "repository.write",
            json!({"operation":"acknowledge","operation_id":operation_id}),
        )
        .await;
    assert_eq!(
        acknowledged["result"]["acknowledged"], true,
        "{acknowledged}"
    );
    let original_tree = git(root, &["rev-parse", "HEAD^{tree}"]);
    std::fs::write(
        root.join("commit.txt"),
        "staged after the original commit\n",
    )
    .unwrap();
    git(root, &["add", "commit.txt"]);
    let prepared_amend = host
        .request(
            5,
            "repository.write",
            json!({
                "operation":"prepare",
                "workspace":root,
                "action":{
                    "kind":"commit_editor",
                    "command":editor,
                    "nvim_server":nvim_server,
                    "amend":true,
                },
            }),
        )
        .await;
    let amend_intent = prepared_amend["result"]["intent"]
        .as_str()
        .expect("amend editor intent");
    let amended = host
        .request(
            6,
            "repository.write",
            json!({"operation":"submit","intent":amend_intent}),
        )
        .await;
    assert_eq!(amended["result"]["success"], true, "{amended}");
    assert_eq!(git(root, &["rev-parse", "HEAD^{tree}"]), original_tree);
    assert_eq!(
        git(root, &["diff", "--cached", "--name-only"]),
        b"commit.txt\n"
    );
    host.stop().await;
}

#[tokio::test]
async fn harness_submits_complete_text_with_document_lifetime_admission() {
    let fixture = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    git(fixture.path(), &["init", "--quiet"]);
    let mut host = Host::start().await;
    let unavailable = host
        .request_for(
            "uninitialized",
            2,
            "session.document",
            json!({
                "operation":"snapshot", "document":"transcript"
            }),
        )
        .await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    let initialized = host
        .request(
            3,
            "session.initialize",
            json!({
                "workspace":fixture.path(), "data_root":data.path(),
                "client_id":"document-host-test", "provider":{"kind":"mock","command":[]}
            }),
        )
        .await;
    assert!(
        initialized["result"]["session"]["id"].is_string(),
        "{initialized}"
    );
    let session_id = initialized["result"]["session"]["id"].as_str().unwrap();
    let opened = host
        .request_for(
            session_id,
            4,
            "session.document",
            json!({
                "operation":"open", "document":"transcript", "view":"view",
                "width":{"columns":80,"tabstop":4}
            }),
        )
        .await;
    assert!(opened["result"].get("composer").is_none(), "{opened}");
    let transcript_revision = opened["result"]["transcript"]["revision"].clone();
    let complete_text = format!("{}complete tail", "complete λ\n".repeat(6000));
    let armed = host
        .request_for(
            session_id,
            10,
            "session.provider.mock_response",
            json!({"operation":"hold"}),
        )
        .await;
    let hold_id = armed["result"]["id"].as_str().unwrap().to_owned();
    let submitted_large = host
        .request_for(
            session_id,
            5,
            "session.prompt.submit",
            json!({"text":complete_text,
                "submission":{"document":"transcript","token":1}}),
        )
        .await;
    assert!(submitted_large.get("error").is_none(), "{submitted_large}");
    // Prompt submission answers at owner acceptance. The transcript gains the prompt once the
    // provider turn starts, so wait for the armed hold instead of polling.
    let waited = host
        .request_for(
            session_id,
            11,
            "session.provider.mock_response",
            json!({"operation":"wait","id":hold_id}),
        )
        .await;
    assert!(waited.get("error").is_none(), "{waited}");
    let synchronized = host
        .request_for(
            session_id,
            7,
            "session.document",
            json!({
                "operation":"sync","document":"transcript","revision":transcript_revision
            }),
        )
        .await;
    assert!(
        synchronized["result"]["patch"]
            .as_array()
            .is_some_and(|patch| !patch.is_empty()),
        "{synchronized}"
    );
    let submitted = host
        .request_for(
            session_id,
            21,
            "session.prompt.submit",
            json!({
                "text":"newer λ\n\ndraft", "submission":{"document":"transcript","token":2}
            }),
        )
        .await;
    assert!(submitted.get("error").is_none(), "{submitted}");
    // The admission event follows the owner's acceptance on the same ordered stream as the
    // response. A later request response proves the stream has delivered it.
    let flushed = host
        .request_for(session_id, 23, "session.state.get", json!({}))
        .await;
    assert!(flushed.get("error").is_none(), "{flushed}");
    assert!(
        host.progress
            .iter()
            .any(|event| event["event"] == "provider_event"
                && event["payload"]["kind"] == "prompt_submission"
                && event["payload"]["data"]
                    == json!({"document":"transcript","token":2,"state":"accepted"})),
        "missing prompt admission event"
    );
    let duplicate = host
        .request_for(
            session_id,
            22,
            "session.prompt.submit",
            json!({"text":"duplicate",
        "submission":{"document":"transcript","token":2}}),
        )
        .await;
    assert!(duplicate.get("error").is_some(), "{duplicate}");
    let closed = host
        .request_for(
            session_id,
            8,
            "session.document",
            json!({
                "operation":"close","document":"transcript"
            }),
        )
        .await;
    assert_eq!(closed["result"]["closed"], true, "{closed}");
    let stale = host
        .request_for(
            session_id,
            9,
            "session.prompt.submit",
            json!({"text":"late prompt",
        "submission":{"document":"transcript","token":3}}),
        )
        .await;
    assert!(
        stale["error"]["message"]
            .as_str()
            .unwrap()
            .contains("not open"),
        "{stale}"
    );
    let state = host
        .request_for(session_id, 10, "session.state.get", json!({}))
        .await;
    let exchanges = host
        .request_for(session_id, 11, "session.exchange.list", json!({}))
        .await;
    assert_eq!(exchanges["result"][0]["prompt"], complete_text);
    assert_eq!(
        state["result"]["session"]["id"],
        initialized["result"]["session"]["id"]
    );
    host.stop().await;
}

fn git(root: &std::path::Path, arguments: &[&str]) -> Vec<u8> {
    let output = std::process::Command::new("git")
        .arg("-C")
        .arg(root)
        .args(arguments)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}
