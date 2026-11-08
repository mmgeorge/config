use base64::{Engine, engine::general_purpose::STANDARD};
use serde_json::{Value, json};
use std::{process::Stdio, time::Duration};
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
        Self::start_with_gh(None).await
    }

    async fn start_with_gh(directory: Option<&std::path::Path>) -> Self {
        Self::start_with_fixture(directory, false).await
    }

    async fn start_with_gh_and_recovery(
        directory: Option<&std::path::Path>,
        recovery_directory: std::path::PathBuf,
    ) -> Self {
        Self::start_with_fixture_and_recovery(directory, false, Some(recovery_directory)).await
    }

    async fn start_with_fixture(
        directory: Option<&std::path::Path>,
        inherit_executables: bool,
    ) -> Self {
        Self::start_with_fixture_and_recovery(directory, inherit_executables, None).await
    }

    async fn start_with_fixture_and_recovery(
        directory: Option<&std::path::Path>,
        inherit_executables: bool,
        configured_recovery_directory: Option<std::path::PathBuf>,
    ) -> Self {
        let mut command = tokio::process::Command::new(env!("CARGO_BIN_EXE_forge"));
        if let Some(directory) = directory {
            if inherit_executables {
                let mut path = vec![directory.to_path_buf()];
                path.extend(std::env::split_paths(
                    &std::env::var_os("PATH").unwrap_or_default(),
                ));
                command.env("PATH", std::env::join_paths(path).unwrap());
            } else {
                command.env("PATH", directory);
            }
        }
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
        let recovery_directory = configured_recovery_directory
            .unwrap_or_else(|| host.recovery.path().join("forge/recovery/github/v1"));
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

struct GhExecutable {
    _directory: tempfile::TempDir,
    search_path: std::path::PathBuf,
}

fn gh_executable() -> &'static std::path::Path {
    static EXECUTABLE: std::sync::OnceLock<GhExecutable> = std::sync::OnceLock::new();
    &EXECUTABLE
        .get_or_init(|| {
            let directory = tempfile::tempdir().unwrap();
            let source = directory.path().join("gh.rs");
            let output = directory
                .path()
                .join(if cfg!(windows) { "gh.exe" } else { "gh" });
            std::fs::write(
                &source,
                include_str!("../crates/forge-github/tests/fixtures/gh.rs"),
            )
            .unwrap();
            let compiled = std::process::Command::new("rustup")
                .args(["run", "1.94.0", "rustc", "--edition=2024"])
                .arg(source)
                .arg("-o")
                .arg(output)
                .output()
                .unwrap();
            assert!(
                compiled.status.success(),
                "{}",
                String::from_utf8_lossy(&compiled.stderr)
            );
            GhExecutable {
                search_path: directory.path().to_owned(),
                _directory: directory,
            }
        })
        .search_path
}

struct CommitEditorExecutable {
    _directory: tempfile::TempDir,
    path: std::path::PathBuf,
}

fn commit_editor_executable() -> &'static std::path::Path {
    static EXECUTABLE: std::sync::OnceLock<CommitEditorExecutable> = std::sync::OnceLock::new();
    &EXECUTABLE
        .get_or_init(|| {
            let directory = tempfile::tempdir().unwrap();
            let source = directory.path().join("commit_editor.rs");
            let output = directory.path().join(if cfg!(windows) {
                "commit-editor.exe"
            } else {
                "commit-editor"
            });
            std::fs::write(&source, include_str!("fixtures/commit_editor.rs")).unwrap();
            let compiled = std::process::Command::new("rustup")
                .args(["run", "1.94.0", "rustc", "--edition=2024"])
                .arg(source)
                .arg("-o")
                .arg(&output)
                .output()
                .unwrap();
            assert!(
                compiled.status.success(),
                "{}",
                String::from_utf8_lossy(&compiled.stderr)
            );
            CommitEditorExecutable {
                path: output,
                _directory: directory,
            }
        })
        .path
}

/// Git evaluates GIT_EDITOR through its shell, including on Git for Windows.
/// Quote one executable path as a POSIX shell word so Windows separators and spaces reach it.
fn git_editor_command(path: &std::path::Path) -> String {
    let path = path.to_string_lossy();
    format!("'{}'", path.replace('\'', "'\"'\"'"))
}

fn sync_fixture(root: &std::path::Path, mode: &str) -> Value {
    std::fs::write(root.join("host"), "enterprise.example").unwrap();
    std::fs::write(root.join("mode"), mode).unwrap();
    std::fs::write(root.join("response.json"), serde_json::to_vec(&json!({"data":{"repository":{"issues":{
        "nodes":[{"number":7,"title":"Hosted sync","state":"OPEN","url":"https://enterprise.example/owner/repo/issues/7",
            "createdAt":"2026-09-01T00:00:00Z","updatedAt":"2026-09-07T00:00:00Z","labels":{"nodes":[]}}],
        "totalCount":1,"pageInfo":{"hasNextPage":false,"endCursor":null}
    }}}})).unwrap()).unwrap();
    json!({"database":root.join("repo/issues/issues.redb"),"directory":root,"request":{
        "repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"scope":"open","manual":true,
        "snapshot":root.join("repo/issues/open-snapshot.json")
    }})
}

#[tokio::test]
async fn pr_state_transition_and_uncertain_recovery_use_the_host_without_harness() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "pr_success");
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let mut params = json!({"directory":root.path(), "request": {
        "target":{"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"},
        "request":{"operation":"transition","desired":"DRAFT"}
    }});
    let result = host.request(2, "github.pull_request", params.clone()).await;
    assert_eq!(result["result"]["outcome"], "confirmed", "{result}");
    assert_eq!(result["result"]["is_draft"], true);
    std::fs::write(root.path().join("mode"), "pr_uncertain").unwrap();
    params["request"]["request"]["desired"] = json!("CLOSED");
    let result = host.request(3, "github.pull_request", params.clone()).await;
    assert_eq!(result["result"]["outcome"], "outcome_unknown", "{result}");
    assert!(result["result"].get("state").is_none());
    let result = host.request(4, "github.pull_request", params.clone()).await;
    assert!(
        result["error"].to_string().contains("OutcomeUnknown"),
        "{result}"
    );
    params["request"]["request"] = json!({"operation":"reconcile"});
    let result = host.request(5, "github.pull_request", params).await;
    assert_eq!(result["result"]["outcome"], "reconciled", "{result}");
    assert_eq!(result["result"]["state"], "CLOSED");
    std::fs::write(root.path().join("mode"), "pr_success").unwrap();
    std::fs::write(
        root.path().join("submitted-title.json"),
        "\"Host-owned title\"",
    )
    .unwrap();
    let params = json!({"directory":root.path(), "request": {
        "target":{"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"},
        "request":{"operation":"edit","edit":{"title":"Host-owned title"}}
    }});
    let result = host.request(6, "github.pull_request", params).await;
    assert_eq!(result["result"]["outcome"], "confirmed", "{result}");
    assert_eq!(result["result"]["matches_submission"], true);
    assert!(host.progress.is_empty());
    host.stop().await;
}

#[tokio::test]
async fn issue_document_persists_edits_across_close_and_confirms_native_save() {
    let directory = tempfile::tempdir().unwrap();
    let sync = sync_fixture(directory.path(), "success");
    let detail: Value = serde_json::from_str(include_str!(
        "../crates/forge-github/tests/fixtures/detail.json"
    ))
    .unwrap();
    std::fs::write(
        directory.path().join("response.json"),
        serde_json::to_vec(&detail).unwrap(),
    )
    .unwrap();
    let current = json!({"node_id":"ISSUE_7","number":7,"title":detail["title"],
        "body":detail["body"],"assignees":[{"login":"bob"}]});
    let saved_title = "Saved native issue title";
    let mut after = current.clone();
    after["title"] = json!(saved_title);
    std::fs::write(
        directory.path().join("issue-edit-current.json"),
        serde_json::to_vec(&current).unwrap(),
    )
    .unwrap();
    std::fs::write(
        directory.path().join("issue-edit-fields-expected.json"),
        serde_json::to_vec(&json!({"title":saved_title})).unwrap(),
    )
    .unwrap();
    std::fs::write(
        directory.path().join("issue-edit-fields-result.json"),
        serde_json::to_vec(&after).unwrap(),
    )
    .unwrap();
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let mut open = json!({"operation":"open","document":"native-issue-1","directory":directory.path(),
        "database":sync["database"],"repository":sync["request"]["repository"],"number":7});
    let opened = host.request(2, "github.issue.document", open.clone()).await;
    assert!(opened.get("error").is_none(), "{opened}");
    assert_eq!(opened["result"]["snapshot"]["document"], "native-issue-1");
    let blocks = opened["result"]["snapshot"]["block"].as_array().unwrap();
    assert_eq!(
        blocks
            .iter()
            .filter(|block| block["metadata"]["editable_region"]
                .as_array()
                .is_some_and(|region| !region.is_empty()))
            .count(),
        3
    );
    std::fs::write(
        directory.path().join("actor-error"),
        "fixture actor temporarily unavailable",
    )
    .unwrap();
    let deferred = host
        .request(
            3,
            "github.issue.document",
            json!({"operation":"save","document":"native-issue-1","capture":[{
                "document":"native-issue-1","region":"title","base":0,"sequence":1,"text":saved_title
            }]}),
        )
        .await;
    assert!(deferred.get("error").is_none(), "{deferred}");
    assert!(
        deferred["result"]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .any(|field| field["region"] == "title"
                && field["revision"] == 1
                && field["sequence"] == 1
                && field["dirty"] == true
                && field["uncertain"] == false),
        "{deferred}"
    );
    assert!(
        deferred["result"]["diagnostic"]
            .as_str()
            .unwrap()
            .contains("fixture actor temporarily unavailable"),
        "{deferred}"
    );
    assert!(!directory.path().join("issue-edit-log").exists());
    std::fs::remove_file(directory.path().join("actor-error")).unwrap();
    let closed = host
        .request(
            4,
            "github.issue.document",
            json!({"operation":"close","document":"native-issue-1"}),
        )
        .await;
    assert_eq!(closed["result"]["collected"], true, "{closed}");
    open["document"] = json!("native-issue-2");
    let reopened = host.request(5, "github.issue.document", open).await;
    assert!(reopened.get("error").is_none(), "{reopened}");
    let title = reopened["result"]["snapshot"]["block"]
        .as_array()
        .unwrap()
        .iter()
        .find(|block| block["id"] == "region:title")
        .unwrap();
    assert_eq!(title["text"][0], saved_title);
    let saved = host
        .request(
            6,
            "github.issue.document",
            json!({"operation":"save","document":"native-issue-2"}),
        )
        .await;
    assert!(saved.get("error").is_none(), "{saved}");
    assert!(
        saved["result"]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .all(|field| field["dirty"] == false && field["uncertain"] == false),
        "{saved}"
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("issue-edit-log")).unwrap(),
        "fields\n"
    );
    let closed = host
        .request(
            7,
            "github.issue.document",
            json!({"operation":"close","document":"native-issue-2"}),
        )
        .await;
    assert_eq!(closed["result"]["collected"], true, "{closed}");
    host.stop().await;
}

#[tokio::test]
async fn host_restart_retains_issue_notification_and_pr_recovery_without_replaying_writes() {
    let directory = tempfile::tempdir().unwrap();
    let recovery_directory = directory.path().join("forge/recovery/github/v1");
    let sync = sync_fixture(directory.path(), "issue_edit_uncertain");
    let detail: Value = serde_json::from_str(include_str!(
        "../crates/forge-github/tests/fixtures/detail.json"
    ))
    .unwrap();
    std::fs::write(
        directory.path().join("response.json"),
        serde_json::to_vec(&detail).unwrap(),
    )
    .unwrap();
    let current = json!({"node_id":"ISSUE_7","number":7,"title":detail["title"],
        "body":detail["body"],"assignees":[{"login":"bob"}]});
    let saved_title = "Uncertain native issue title";
    let mut after = current.clone();
    after["title"] = json!(saved_title);
    std::fs::write(
        directory.path().join("issue-edit-current.json"),
        serde_json::to_vec(&current).unwrap(),
    )
    .unwrap();
    std::fs::write(
        directory.path().join("issue-edit-fields-expected.json"),
        serde_json::to_vec(&json!({"title":saved_title})).unwrap(),
    )
    .unwrap();
    std::fs::write(
        directory.path().join("issue-edit-fields-result.json"),
        serde_json::to_vec(&after).unwrap(),
    )
    .unwrap();

    let mut first =
        Host::start_with_gh_and_recovery(Some(gh_executable()), recovery_directory.clone()).await;
    let opened = first
        .request(
            2,
            "github.issue.document",
            json!({"operation":"open","document":"restart-issue-first","directory":directory.path(),
                "database":sync["database"],"repository":sync["request"]["repository"],"number":7}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let uncertain = first
        .request(
            4,
            "github.issue.document",
            json!({"operation":"save","document":"restart-issue-first","capture":[{
                "document":"restart-issue-first","region":"title","base":0,"sequence":1,"text":saved_title
            }]}),
        )
        .await;
    assert!(
        uncertain["result"]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .any(|field| field["region"] == "title"
                && field["revision"] == 1
                && field["sequence"] == 1),
        "{uncertain}"
    );
    let operation_id = uncertain["result"]["recovery"]["capture"]["operation_id"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_eq!(
        uncertain["result"]["recovery"]["state"]["phase"], "outcome_unknown",
        "{uncertain}"
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("issue-edit-log")).unwrap(),
        "fields\n"
    );
    first.stop().await;

    let mut restarted =
        Host::start_with_gh_and_recovery(Some(gh_executable()), recovery_directory.clone()).await;
    let reopened = restarted
        .request(
            5,
            "github.issue.document",
            json!({"operation":"open","document":"restart-issue-second","directory":directory.path(),
                "database":sync["database"],"repository":sync["request"]["repository"],"number":7}),
        )
        .await;
    assert_eq!(
        reopened["result"]["recovery"]["capture"]["operation_id"], operation_id,
        "{reopened}"
    );
    assert!(
        reopened["result"]["fields"]
            .as_array()
            .unwrap()
            .iter()
            .any(|field| field["uncertain"] == true)
    );
    let blocked = restarted
        .request(
            6,
            "github.issue.document",
            json!({"operation":"save","document":"restart-issue-second"}),
        )
        .await;
    assert!(blocked.get("error").is_some(), "{blocked}");
    assert_eq!(
        std::fs::read_to_string(directory.path().join("issue-edit-log")).unwrap(),
        "fields\n"
    );
    let resolved = restarted
        .request(
            7,
            "github.issue.document",
            json!({"operation":"resolve","document":"restart-issue-second","operation_id":operation_id,
                "resolution":{"resolution":"close_unknown"}}),
        )
        .await;
    assert_eq!(resolved["result"]["fresh_required"], true, "{resolved}");
    assert_eq!(
        resolved["result"]["recovery"]["state"]["phase"], "user_closed_unknown",
        "{resolved}"
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("issue-edit-log")).unwrap(),
        "fields\n"
    );

    for fixture in [
        "issue-edit-current.json",
        "issue-edit-fields-expected.json",
        "issue-edit-fields-result.json",
    ] {
        std::fs::remove_file(directory.path().join(fixture)).unwrap();
    }
    let notification = json!({
        "id":"1","repository":{"full_name":"other/second"},"unread":true,
        "reason":"participating","updated_at":"2026-09-07T00:00:00Z",
        "subject":{"title":"Restart-captured notification","type":"Issue",
            "url":"https://enterprise.example/api/v3/repos/other/second/issues/7","latest_comment_url":null}
    });
    std::fs::write(
        directory.path().join("notification-page.json"),
        serde_json::to_vec(&vec![notification]).unwrap(),
    )
    .unwrap();
    let page = restarted
        .request(8, "github.notification.list", json!({"directory":directory.path(),"request":{"hostname":"enterprise.example","cursor":null}}))
        .await;
    assert_eq!(
        page["result"]["record"][0]["repository"]["owner"], "other",
        "{page}"
    );
    let opened = restarted
        .request(9, "github.notification.document", json!({"operation":"open","document":"restart-notifications","workspace":directory.path(),"hostname":"enterprise.example"}))
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let view = restarted
        .request(
            10,
            "github.notification.document",
            json!({"operation":"view","document":"restart-notifications","view":"restart-notification-view",
                "width":forge_buffer::width::WidthProfile::default()}),
        )
        .await;
    assert!(view.get("error").is_none(), "{view}");
    let snapshot = restarted
        .request(
            11,
            "github.notification.document",
            json!({"operation":"snapshot","document":"restart-notifications"}),
        )
        .await;
    let block = snapshot["result"]["block"]
        .as_array()
        .unwrap()
        .iter()
        .find(|block| block["id"] == "notification:1")
        .unwrap();
    let action = restarted
        .request(
            12,
            "github.notification.document",
            json!({"operation":"act","input":{
            "document":"restart-notifications","revision":snapshot["result"]["revision"],
            "view":"restart-notification-view","sequence":1,"action":"open","block":block["id"],
            "position":{"row":0,"column":0},"target":block["metadata"]["target"][0]["id"]}}),
        )
        .await;
    assert_eq!(
        action["result"]["effect"]["repository"]["name"], "second",
        "{action}"
    );
    assert_eq!(action["result"]["effect"]["number"], 7, "{action}");
    restarted.stop().await;

    std::fs::remove_file(directory.path().join("notification-page.json")).unwrap();
    std::fs::write(directory.path().join("mode"), "pr_uncertain").unwrap();
    let target = json!({"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"});
    let saved_title = "Restart-reconciled review title";
    std::fs::write(
        directory.path().join("submitted-title.json"),
        serde_json::to_vec(saved_title).unwrap(),
    )
    .unwrap();
    let mut pr_host =
        Host::start_with_gh_and_recovery(Some(gh_executable()), recovery_directory).await;
    let opened = pr_host
        .request(
            13,
            "github.review.document.open",
            json!({"directory":directory.path(),"target":target}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let document = opened["result"]["document"].clone();
    let uncertain = pr_host
        .request(
            15,
            "github.review.document.save",
            json!({"document":document,"capture":[{
                "document":document,"region":"title","base":0,"sequence":1,"text":saved_title
            }]}),
        )
        .await;
    assert_eq!(
        uncertain["result"]["snapshot"]["field"][0]["revision"], 1,
        "{uncertain}"
    );
    assert_eq!(
        uncertain["result"]["snapshot"]["uncertain"], true,
        "{uncertain}"
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("mutation-log"))
            .unwrap()
            .lines()
            .count(),
        1
    );
    pr_host.stop().await;

    let mut pr_restarted = Host::start_with_gh_and_recovery(
        Some(gh_executable()),
        directory.path().join("forge/recovery/github/v1"),
    )
    .await;
    let opened = pr_restarted
        .request(
            16,
            "github.review.document.open",
            json!({"directory":directory.path(),"target":target}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let restarted_document = opened["result"]["document"].clone();
    let reconciled = pr_restarted
        .request(
            17,
            "github.review.document.reconcile",
            json!({"document":restarted_document}),
        )
        .await;
    assert_eq!(reconciled["result"]["uncertain"], false, "{reconciled}");
    assert_eq!(
        reconciled["result"]["field"][0]["baseline"], saved_title,
        "{reconciled}"
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("mutation-log"))
            .unwrap()
            .lines()
            .count(),
        1
    );
    pr_restarted.stop().await;
}

#[tokio::test]
async fn host_shutdown_reaps_a_blocked_gh_read_before_its_terminal_response() {
    let directory = tempfile::tempdir().unwrap();
    let params = sync_fixture(directory.path(), "block");
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    host.input
        .write_all(
            &serde_json::to_vec(&json!({"id":2,"method":"github.issue.sync","params":params}))
                .unwrap(),
        )
        .await
        .unwrap();
    host.input.write_all(b"\n").await.unwrap();
    let marker = loop {
        if let Some(marker) = std::fs::read_dir(directory.path())
            .unwrap()
            .filter_map(Result::ok)
            .find(|entry| entry.file_name().to_string_lossy().starts_with("started."))
        {
            break marker.path();
        }
        tokio::time::sleep(Duration::from_millis(5)).await;
    };
    let id = marker
        .file_name()
        .unwrap()
        .to_string_lossy()
        .replace("started.", "");
    let alive = std::fs::File::options()
        .read(true)
        .write(true)
        .open(directory.path().join(format!("alive.{id}")))
        .unwrap();
    assert!(alive.try_lock().is_err());
    host.input
        .write_all(b"{\"id\":3,\"method\":\"host.shutdown\",\"params\":{}}\n")
        .await
        .unwrap();
    assert!(host.response(2).await.get("error").is_some());
    let terminal = host.response(3).await;
    assert!(terminal.get("error").is_none(), "{terminal}");
    alive.try_lock().unwrap();
    assert!(host.process.wait().await.unwrap().success());
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

    let editor = git_editor_command(commit_editor_executable());
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

#[tokio::test]
async fn repository_creation_context_and_durable_post_preserve_captured_identity() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "pr_success");
    git(root.path(), &["init", "--quiet", "-b", "feature-create"]);
    git(
        root.path(),
        &[
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.test",
            "commit",
            "--quiet",
            "--allow-empty",
            "-m",
            "creation context",
        ],
    );
    let mut host = Host::start_with_fixture(Some(gh_executable()), true).await;
    let context = host
        .request(
            2,
            "github.pull_request.creation.context",
            json!({"directory":root.path()}),
        )
        .await;
    assert!(context.get("error").is_none(), "{context}");
    assert_eq!(context["result"]["branch"], "feature-create");
    assert_eq!(
        context["result"]["repository"]["hostname"],
        "enterprise.example"
    );
    let head = context["result"]["head_commit"].as_str().unwrap();
    assert_eq!(head.len(), 40);
    std::fs::write(root.path().join("creation-head"), head).unwrap();
    let body = "captured λ\r\nbody\n";
    std::fs::write(root.path().join("expected-pr-creation.json"), serde_json::to_vec(&json!({"title":"Captured PR", "body":body,"base":"main","head":"feature-create","draft":true})).unwrap()).unwrap();
    let created = json!({"number":42,"node_id":"PR_created","title":"Captured PR","body":body,"draft":true,
        "user":{"node_id":"ACTOR_fixture"},"base":{"ref":"main","repo":{"node_id":"REPOSITORY_fixture"}},
        "head":{"ref":"feature-create","sha":head},"html_url":"https://enterprise.example/owner/repo/pull/42"});
    std::fs::write(
        root.path().join("pr-creation-result.json"),
        serde_json::to_vec(&created).unwrap(),
    )
    .unwrap();
    let resource =
        json!({"repository":context["result"]["repository"],"kind":"repository","number":0});
    let capture = json!({"resource":resource,"operation_id":"fixture-create","actor_node_id":"ACTOR_fixture","edit_sequence":1,"draft_target":"pr:create",
        "mutation":{"operation":"pull_request_create","repository_node_id":"REPOSITORY_fixture","title":"Captured PR","body":body,"base":"main","head":"feature-create","head_commit":head,"draft":true}});
    let result = host
        .request(
            3,
            "github.review.mutate",
            json!({"directory":root.path(),"request":capture}),
        )
        .await;
    assert_eq!(result["result"]["state"]["phase"], "confirmed", "{result}");
    assert_eq!(result["result"]["state"]["result"], created);
    let rejected_ack = host
        .request(
            4,
            "github.recovery.acknowledge",
            json!({"resource":resource,"operation_id":"fixture-create"}),
        )
        .await;
    assert!(rejected_ack.get("error").is_some());
    let settled = host.request(5,"github.recovery.draft.settle",json!({"resource":resource,"operation_id":"fixture-create","draft":{"repo":"owner/repo","number":0,"creation":{"sequence":2,"title":"newer retained generation"}}})).await;
    assert!(settled.get("error").is_none(), "{settled}");
    let stored = host
        .request(6, "github.review.draft.get", json!({"resource":resource}))
        .await;
    assert_eq!(
        stored["result"]["creation_receipt"]["capture"]["submitted"]["mutation"]["body"],
        body
    );
    assert_eq!(
        stored["result"]["creation"]["title"],
        "newer retained generation"
    );
    assert_eq!(
        std::fs::read_to_string(root.path().join("pr-creation-log")).unwrap(),
        "write\n"
    );
    host.stop().await;
}

fn hosted_comment(body: &str) -> Value {
    json!({"__typename":"PullRequestReviewComment","id":"COMMENT_fixture","databaseId":"4000000000","body":body,
        "url":"https://enterprise.example/owner/repo/pull/7#discussion_r4000000000","viewerDidAuthor":true})
}

fn hosted_rest_comment(body: &str) -> Value {
    json!({"node_id":"COMMENT_fixture","id":4_000_000_000u64,"body":body,
        "html_url":"https://enterprise.example/owner/repo/pull/7#discussion_r4000000000",
        "pull_request_url":"https://enterprise.example/api/v3/repos/owner/repo/pulls/7",
        "issue_url":"https://enterprise.example/api/v3/repos/owner/repo/issues/7",
        "user":{"login":"fixture","node_id":"ACTOR_fixture"}})
}

fn comment_params(root: &std::path::Path, request: Value) -> Value {
    json!({"directory":root,"target":{"pull_request":{"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"},
        "kind":"PullRequestReviewComment","node_id":"COMMENT_fixture","database_id":4_000_000_000u64},"request":request})
}

#[tokio::test]
async fn review_comment_route_owns_native_baseline_and_retains_unknown_submission() {
    let root = tempfile::tempdir().unwrap();
    for (name, body) in [
        ("rest-read.json", "remote old"),
        ("rest-after.json", "submitted"),
    ] {
        std::fs::write(
            root.path().join(name),
            serde_json::to_vec(&hosted_rest_comment(body)).unwrap(),
        )
        .unwrap();
    }
    sync_fixture(root.path(), "pr_success");
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let target = comment_params(root.path(), json!({}))["target"].clone();
    let opened = host
        .request(
            2,
            "github.review.document.open",
            json!({"directory":root.path(),"target":target["pull_request"]}),
        )
        .await;
    let document = opened["result"]["document"].clone();
    assert!(document.is_string(), "{opened}");
    std::fs::write(root.path().join("mode"), "comment_ok").unwrap();
    std::fs::write(
        root.path().join("expected-mutation"),
        "updatePullRequestReviewComment",
    )
    .unwrap();
    std::fs::write(root.path().join("comment-echo-receipt"), "yes").unwrap();
    std::fs::write(
        root.path().join("comment-read.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("remote old")}})).unwrap(),
    )
    .unwrap();
    std::fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("submitted")}})).unwrap(),
    )
    .unwrap();
    std::fs::write(root.path().join("comment-result.json"),serde_json::to_vec(&json!({"data":{"mutation":{"clientMutationId":"__receipt__","comment":hosted_comment("submitted")}}})).unwrap()).unwrap();
    let loaded = host
        .request(
            3,
            "github.review.document.comment",
            json!({"document":document,"command":{"operation":"load","target":target}}),
        )
        .await;
    let snapshot = &loaded["result"]["snapshot"];
    assert_eq!(snapshot["baseline"], "remote old", "{loaded}");
    let comment = snapshot["comment"].clone();
    let region = snapshot["region"].clone();
    let saved = host.request(5,"github.review.document.comment",json!({
        "document":document,
        "capture":[{"document":document,"region":region,"base":0,"sequence":1,"text":"submitted"}],
        "command":{"operation":"save","comment":comment,"action":"save"}
    })).await;
    assert_eq!(saved["result"]["snapshot"]["revision"], 1, "{saved}");
    assert_eq!(
        saved["result"]["snapshot"]["baseline"], "submitted",
        "{saved}"
    );
    assert_eq!(saved["result"]["snapshot"]["dirty"], false);
    std::fs::write(root.path().join("mode"), "comment_uncertain").unwrap();
    std::fs::write(
        root.path().join("rest-after.json"),
        serde_json::to_vec(&hosted_rest_comment("uncertain")).unwrap(),
    )
    .unwrap();
    std::fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("uncertain")}})).unwrap(),
    )
    .unwrap();
    let saved = host.request(7,"github.review.document.comment",json!({
        "document":document,
        "capture":[{"document":document,"region":region,"base":1,"sequence":2,"text":"uncertain"}],
        "command":{"operation":"save","comment":comment,"action":"save"}
    })).await;
    assert_eq!(saved["result"]["snapshot"]["revision"], 2, "{saved}");
    assert_eq!(saved["result"]["snapshot"]["uncertain"], true, "{saved}");
    assert_eq!(saved["result"]["snapshot"]["baseline"], "submitted");
    let reconciled = host
        .request(
            9,
            "github.review.document.comment",
            json!({
                "document":document,
                "capture":[{"document":document,"region":region,"base":2,"sequence":3,"text":"newer"}],
                "command":{"operation":"reconcile","comment":comment}
            }),
        )
        .await;
    assert_eq!(
        reconciled["result"]["snapshot"]["baseline"], "uncertain",
        "{reconciled}"
    );
    assert_eq!(reconciled["result"]["snapshot"]["text"], "newer");
    assert_eq!(reconciled["result"]["snapshot"]["revision"], 3);
    assert_eq!(reconciled["result"]["snapshot"]["sequence"], 3);
    assert_eq!(reconciled["result"]["snapshot"]["dirty"], true);
    assert_eq!(reconciled["result"]["snapshot"]["uncertain"], false);
    assert_eq!(
        std::fs::read_to_string(root.path().join("comment-mutation-log"))
            .unwrap()
            .lines()
            .count(),
        2
    );
    host.stop().await;
}
