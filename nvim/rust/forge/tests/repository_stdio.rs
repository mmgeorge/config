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
                "initialize",
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
        if method == "shutdown" {
            self.terminal_request = Some(id);
        }
        let encoded =
            serde_json::to_vec(&json!({"id": id, "method": method, "params": params})).unwrap();
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
            tokio::time::timeout(
                Duration::from_secs(10),
                self.output.read_until(b'\n', &mut bytes),
            )
            .await
            .unwrap()
            .unwrap();
            assert!(!bytes.is_empty());
            assert!(bytes.len() <= forge_protocol::MAX_FRAME_BYTES);
            let response: Value = serde_json::from_slice(&bytes).unwrap();
            self.consumed_bytes += bytes.len();
            self.consumed_frames += 1;
            let credit = serde_json::to_vec(&json!({"id":0,"method":"transport.consumed","params":{"bytes":self.consumed_bytes,"frames":self.consumed_frames}})).unwrap();
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
                    assert!(assembled.len() <= forge_protocol::MAX_SNAPSHOT_BYTES);
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
        let response = self.request(1000, "shutdown", json!({})).await;
        assert!(response.get("error").is_none(), "{response}");
        drop(self.input);
        assert!(
            tokio::time::timeout(Duration::from_secs(10), self.process.wait())
                .await
                .unwrap()
                .unwrap()
                .success()
        );
    }
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
            let compiled = forge_git::command::read_command(
                std::process::Command::new("rustup")
                    .args(["run", "1.94.0", "rustc", "--edition=2024"])
                    .arg(source)
                    .arg("-o")
                    .arg(output),
                forge_git::command::CommandLimits {
                    stdout_bytes: 64 * 1024,
                    stderr_bytes: 64 * 1024,
                    timeout: Duration::from_secs(30),
                },
                || Ok(()),
            )
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
            let compiled = forge_git::command::read_command(
                std::process::Command::new("rustup")
                    .args(["run", "1.94.0", "rustc", "--edition=2024"])
                    .arg(source)
                    .arg("-o")
                    .arg(&output),
                forge_git::command::CommandLimits {
                    stdout_bytes: 64 * 1024,
                    stderr_bytes: 64 * 1024,
                    timeout: Duration::from_secs(30),
                },
                || Ok(()),
            )
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
async fn review_document_edits_save_and_reconcile_through_the_shared_host() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "pr_success");
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let target = json!({"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"});
    let opened = host
        .request(
            2,
            "review.open_pr",
            json!({"directory":root.path(),"target":target}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let document = opened["result"]["document"].clone();
    let text = "λ\n  retained body\n";
    let materialized = host
        .request(
            20,
            "review.materialize",
            json!({
                "document":document,"width":{"columns":80,"tabstop":4}
            }),
        )
        .await;
    assert!(materialized.get("error").is_none(), "{materialized}");
    let projection_revision = materialized["result"]["snapshot"]["revision"].clone();
    std::fs::write(
        root.path().join("submitted-body.json"),
        serde_json::to_vec(text).unwrap(),
    )
    .unwrap();
    let accepted = host
        .request(
            3,
            "review.region_edit",
            json!({"document":document,"region":"body","base":0,"sequence":1,"text":text}),
        )
        .await;
    assert_eq!(accepted["result"]["revision"], 1, "{accepted}");
    assert_eq!(
        accepted["result"]["patch"]["base"], projection_revision,
        "{accepted}"
    );
    assert_eq!(
        accepted["result"]["patch"]["metadata_edit"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    let saved = host
        .request(4, "review.save", json!({"document":document}))
        .await;
    assert_eq!(saved["result"]["remote"]["outcome"], "confirmed", "{saved}");
    assert_eq!(saved["result"]["snapshot"]["field"][1]["baseline"], text);
    assert_eq!(saved["result"]["snapshot"]["field"][1]["dirty"], false);
    let stale = host
        .request(
            5,
            "review.region_edit",
            json!({"document":document,"region":"body","base":0,"sequence":2,"text":"stale"}),
        )
        .await;
    assert!(stale.get("error").is_some());
    std::fs::write(root.path().join("mode"), "pr_uncertain").unwrap();
    std::fs::write(
        root.path().join("submitted-title.json"),
        "\"Uncertain title\"",
    )
    .unwrap();
    let accepted = host.request(6, "review.region_edit", json!({"document":document,"region":"title","base":0,"sequence":2,"text":"Uncertain title"})).await;
    assert_eq!(accepted["result"]["revision"], 1);
    let saved = host
        .request(7, "review.save", json!({"document":document}))
        .await;
    assert_eq!(saved["result"]["snapshot"]["uncertain"], true, "{saved}");
    let reconciled = host
        .request(8, "review.reconcile", json!({"document":document}))
        .await;
    assert_eq!(reconciled["result"]["uncertain"], false, "{reconciled}");
    assert_eq!(
        reconciled["result"]["field"][0]["baseline"],
        "Uncertain title"
    );
    let snapshot = host
        .request(9, "review.snapshot", json!({"document":document}))
        .await;
    assert_eq!(snapshot["result"], reconciled["result"]);
    let closed = host
        .request(10, "review.close", json!({"document":document}))
        .await;
    assert_eq!(closed["result"]["closed"], true);
    assert!(
        host.request(11, "review.snapshot", json!({"document":document}))
            .await
            .get("error")
            .is_some()
    );
    assert!(host.progress.is_empty());
    host.stop().await;
}

#[tokio::test]
async fn large_review_open_transfers_exact_remote_baseline() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "pr_success");
    let body = "\"".repeat(200 * 1024);
    std::fs::write(
        root.path().join("pr-body.json"),
        serde_json::to_vec(&body).unwrap(),
    )
    .unwrap();
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let target = json!({"repository":{"hostname":"enterprise.example","owner":"Owner","name":"Repo"},"number":7,"node_id":"PR_fixture"});
    let opened = host
        .request(
            2,
            "review.open_pr",
            json!({"directory":root.path(),"target":target}),
        )
        .await;
    assert_eq!(
        opened["result"]["field"][1]["baseline"], body,
        "large review baseline missing"
    );
    assert_eq!(opened["result"]["field"][1]["text"], body);
    assert!(host.progress.is_empty());
    host.stop().await;
}

#[tokio::test]
async fn issue_sync_routes_through_the_native_owner_without_harness_initialization() {
    let directory = tempfile::tempdir().unwrap();
    let mut params = sync_fixture(directory.path(), "success");
    params["progress"] = json!(true);
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let synced = host.request(2, "github.sync", params.clone()).await;
    assert_eq!(synced["result"]["fetched"], 1, "{synced}");
    assert_eq!(synced["result"]["pages"], 1);
    assert!(!host.progress.is_empty());
    assert!(host.progress.iter().all(|event| event["request_id"] == 2
        && event["event"] == "github.sync.progress"
        && event["payload"]["phase"].is_string()));
    let snapshot: Value = serde_json::from_slice(
        &std::fs::read(directory.path().join("repo/issues/open-snapshot.json")).unwrap(),
    )
    .unwrap();
    assert_eq!(snapshot["issues"][0]["title"], "Hosted sync");
    let unavailable = host.request(3, "state.get", json!({})).await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    params["request"]["manual"] = json!(false);
    let fresh = host.request(4, "github.sync", params.clone()).await;
    assert_eq!(fresh["result"]["refreshed"], false, "{fresh}");
    params["request"]["unexpected"] = json!(true);
    assert!(
        host.request(5, "github.sync", params)
            .await
            .get("error")
            .is_some()
    );
    let deleted = host.request(6, "github.issues", json!({"database":directory.path().join("repo/issues/issues.redb"),"repo":"owner/repo","request":{"operation":"delete_cache"}})).await;
    assert_eq!(deleted["result"]["deleted"], true, "{deleted}");
    host.stop().await;
}

#[tokio::test]
async fn issue_detail_fetch_is_persisted_before_the_host_response() {
    let directory = tempfile::tempdir().unwrap();
    let sync = sync_fixture(directory.path(), "success");
    let mut detail: Value = serde_json::from_str(include_str!(
        "../crates/forge-github/tests/fixtures/detail.json"
    ))
    .unwrap();
    std::fs::write(
        directory.path().join("response.json"),
        serde_json::to_vec(&detail).unwrap(),
    )
    .unwrap();
    let params = json!({"database":sync["database"], "directory":directory.path(),
        "request":{"repository":sync["request"]["repository"],"number":7}});
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let fetched = host.request(2, "github.detail", params.clone()).await;
    assert_eq!(
        fetched["result"]["item"]["title"], "Unicode é issue",
        "{fetched}"
    );
    assert!(fetched["result"]["fetched_at"].as_u64().unwrap() > 0);
    let cached = host.request(3, "github.issues", json!({"database":sync["database"],"repo":"owner/repo", "request":{"operation":"detail","number":7}})).await;
    assert_eq!(cached["result"]["item"], fetched["result"]["item"]);
    let unavailable = host.request(4, "state.get", json!({})).await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    detail["body"] = json!("x".repeat(forge_protocol::MAX_FRAME_BYTES));
    std::fs::write(
        directory.path().join("response.json"),
        serde_json::to_vec(&detail).unwrap(),
    )
    .unwrap();
    let oversized = host.request(5, "github.detail", params).await;
    assert_eq!(
        oversized["result"]["item"]["body"].as_str().unwrap().len(),
        forge_protocol::MAX_FRAME_BYTES
    );
    let deleted = host.request(6, "github.issues", json!({"database":sync["database"],"repo":"owner/repo","request":{"operation":"delete_cache"}})).await;
    assert_eq!(deleted["result"]["deleted"], true, "{deleted}");
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
    let opened = host.request(2, "issue.document", open.clone()).await;
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
    let edited = host
        .request(
            3,
            "issue.document",
            json!({"operation":"edit","edit":{
        "document":"native-issue-1","region":"title","base":0,"sequence":1,"text":saved_title}}),
        )
        .await;
    assert_eq!(edited["result"]["revision"], 1, "{edited}");
    assert_eq!(
        edited["result"]["patch"]["metadata_edit"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    let closed = host
        .request(
            4,
            "issue.document",
            json!({"operation":"close","document":"native-issue-1"}),
        )
        .await;
    assert_eq!(closed["result"]["collected"], true, "{closed}");
    open["document"] = json!("native-issue-2");
    let reopened = host.request(5, "issue.document", open).await;
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
            "issue.document",
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
            "issue.document",
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
            "issue.document",
            json!({"operation":"open","document":"restart-issue-first","directory":directory.path(),
                "database":sync["database"],"repository":sync["request"]["repository"],"number":7}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let edited = first
        .request(
            3,
            "issue.document",
            json!({"operation":"edit","edit":{"document":"restart-issue-first",
                "region":"title","base":0,"sequence":1,"text":saved_title}}),
        )
        .await;
    assert_eq!(edited["result"]["revision"], 1, "{edited}");
    let uncertain = first
        .request(
            4,
            "issue.document",
            json!({"operation":"save","document":"restart-issue-first"}),
        )
        .await;
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
            "issue.document",
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
            "issue.document",
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
            "issue.document",
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
        .request(8, "github.notifications.page", json!({"directory":directory.path(),"request":{"hostname":"enterprise.example","cursor":null}}))
        .await;
    assert_eq!(
        page["result"]["record"][0]["repository"]["owner"], "other",
        "{page}"
    );
    let opened = restarted
        .request(9, "notifications.document", json!({"operation":"open","document":"restart-notifications","workspace":directory.path(),"hostname":"enterprise.example"}))
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let view = restarted
        .request(
            10,
            "notifications.document",
            json!({"operation":"view","document":"restart-notifications","view":"restart-notification-view",
                "width":forge_buffer::width::WidthProfile::default()}),
        )
        .await;
    assert!(view.get("error").is_none(), "{view}");
    let snapshot = restarted
        .request(
            11,
            "notifications.document",
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
            "notifications.document",
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
            "review.open_pr",
            json!({"directory":directory.path(),"target":target}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let document = opened["result"]["document"].clone();
    let edited = pr_host
        .request(
            14,
            "review.region_edit",
            json!({"document":document,"region":"title","base":0,"sequence":1,"text":saved_title}),
        )
        .await;
    assert_eq!(edited["result"]["revision"], 1, "{edited}");
    let uncertain = pr_host
        .request(15, "review.save", json!({"document":document}))
        .await;
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
            "review.open_pr",
            json!({"directory":directory.path(),"target":target}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let restarted_document = opened["result"]["document"].clone();
    let reconciled = pr_restarted
        .request(
            17,
            "review.reconcile",
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
            &serde_json::to_vec(&json!({"id":2,"method":"github.sync","params":params})).unwrap(),
        )
        .await
        .unwrap();
    host.input.write_all(b"\n").await.unwrap();
    let marker = tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            if let Some(marker) = std::fs::read_dir(directory.path())
                .unwrap()
                .filter_map(Result::ok)
                .find(|entry| entry.file_name().to_string_lossy().starts_with("started."))
            {
                break marker.path();
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .unwrap();
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
        .write_all(b"{\"id\":3,\"method\":\"shutdown\",\"params\":{}}\n")
        .await
        .unwrap();
    assert!(host.response(2).await.get("error").is_some());
    let terminal = host.response(3).await;
    assert!(terminal.get("error").is_none(), "{terminal}");
    alive.try_lock().unwrap();
    assert!(
        tokio::time::timeout(Duration::from_secs(3), host.process.wait())
            .await
            .unwrap()
            .unwrap()
            .success()
    );
}

#[tokio::test]
async fn repository_deletion_obeys_cross_process_leases_and_retains_the_host() {
    let root = tempfile::tempdir().unwrap();
    let repository = root.path().join("repo");
    let database = repository.join("issues/issues.redb");
    let mut first = Host::start().await;
    let mut second = Host::start().await;
    let params = |operation| json!({"database":database,"repo":"owner/repo","request":{"operation":operation}});
    assert!(
        first
            .request(2, "github.issues", params("state"))
            .await
            .get("result")
            .is_some()
    );
    let operation = forge_github::lease::RepositoryLease::operation(&repository).unwrap();
    let busy = second
        .request(2, "github.issues", params("delete_cache"))
        .await;
    assert!(
        busy["error"]["message"].as_str().unwrap().contains("Busy"),
        "{busy}"
    );
    assert!(database.exists());
    drop(operation);
    let deletion = forge_github::lease::RepositoryLease::deletion(&repository).unwrap();
    let blocked = first.request(3, "github.issues", params("state")).await;
    assert!(
        blocked["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Busy"),
        "{blocked}"
    );
    drop(deletion);
    let deleted = second
        .request(3, "github.issues", params("delete_cache"))
        .await;
    assert_eq!(deleted["result"]["deleted"], true, "{deleted}");
    assert!(!repository.exists());
    let absent = second
        .request(4, "github.issues", params("delete_cache"))
        .await;
    assert_eq!(absent["result"]["deleted"], false, "{absent}");
    let restored = first.request(4, "github.issues", params("state")).await;
    assert_eq!(restored["result"]["repo"], "owner/repo", "{restored}");
    assert!(database.exists());
    first.stop().await;
    second.stop().await;
}

#[tokio::test]
async fn separate_forge_processes_share_page_state_snapshot_and_detail_records() {
    let directory = tempfile::tempdir().unwrap();
    let database = directory.path().join("issues.redb");
    let snapshot_path = directory.path().join("open-snapshot.json");
    let mut first = Host::start().await;
    let mut second = Host::start().await;
    let page = first.request(2, "github.issues", json!({
        "database":database,"repo":"Owner/Repo","request":{"operation":"upsert_page","scope":"open","page":{
            "issues":[{"repo":"owner/repo","number":7,"title":"Shared record","state":"OPEN",
                "url":"https://github.com/owner/repo/issues/7","body":"Not completion metadata"}],
            "completed":true,"checked_at":123
        }}
    })).await;
    assert_eq!(page["result"]["upserted"], 1, "{page}");
    let state = second
        .request(
            2,
            "github.issues",
            json!({
                "database":database,"repo":"owner/repo","request":{"operation":"state"}
            }),
        )
        .await;
    assert_eq!(state["result"]["issue_count"], 1, "{state}");
    assert_eq!(state["result"]["open_historical_complete"], true);
    let published = second.request(3, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"publish_snapshot","state":"open","output":snapshot_path}
    })).await;
    assert_eq!(published["result"]["issue_count"], 1, "{published}");
    let snapshot: Value = serde_json::from_slice(&std::fs::read(&snapshot_path).unwrap()).unwrap();
    assert_eq!(snapshot["issues"][0]["number"], 7);
    assert!(snapshot["issues"][0].get("body").is_none());
    let detail = first.request(3, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"upsert_detail","number":7,
            "detail":{"repo":"ignored/repo","number":1,"fetched_at":456,"item":{"body":"Cached body"}}}
    })).await;
    assert_eq!(detail["result"]["repo"], "owner/repo", "{detail}");
    assert_eq!(detail["result"]["number"], 7);
    let details = second.request(4, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"details","number":[8,7]}
    })).await;
    assert_eq!(details["result"]["details"][0]["found"], false, "{details}");
    assert_eq!(
        details["result"]["details"][1]["item"]["body"],
        "Cached body"
    );
    let single = first
        .request(
            4,
            "github.issues",
            json!({
                "database":database,"repo":"owner/repo","request":{"operation":"detail","number":7}
            }),
        )
        .await;
    assert_eq!(single["result"], details["result"]["details"][1]);
    let committed = first.request(5, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"upsert_page","scope":"open","page":{
            "issues":[{"repo":"owner/repo","number":7,"title":"Recovered after process death","state":"OPEN",
                "url":"https://github.com/owner/repo/issues/7"}],"completed":true
        }}
    })).await;
    assert_eq!(committed["result"]["state"]["revision"], 2);
    first.process.kill().await.unwrap();
    let stale: Value = serde_json::from_slice(&std::fs::read(&snapshot_path).unwrap()).unwrap();
    assert_eq!(stale["revision"], 1);
    let recovered = second.request(5, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"reconcile_snapshot","state":"open","output":snapshot_path}
    })).await;
    assert_eq!(recovered["result"]["republished"], true, "{recovered}");
    let current: Value = serde_json::from_slice(&std::fs::read(&snapshot_path).unwrap()).unwrap();
    assert_eq!(current["revision"], 2);
    assert_eq!(
        current["issues"][0]["title"],
        "Recovered after process death"
    );
    let checked = second.request(6, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"reconcile_snapshot","state":"open","output":snapshot_path}
    })).await;
    assert_eq!(checked["result"]["republished"], false, "{checked}");
    second.stop().await;
}

#[tokio::test]
async fn issue_storage_uses_the_host_without_initializing_harness() {
    let directory = tempfile::tempdir().unwrap();
    let database = directory.path().join("issues.redb");
    let mut host = Host::start().await;
    let stored = host.request(2, "github.issues", json!({
        "database":database,"repo":"Owner/Repo","request":{
            "operation":"upsert_detail","number":7,
            "detail":{"repo":"ignored/repo","number":0,"fetched_at":123,"item":{"title":"Host storage"}}
        }
    })).await;
    assert_eq!(stored["result"]["number"], 7, "{stored}");
    assert_eq!(stored["result"]["repo"], "owner/repo");
    let detail = host
        .request(
            3,
            "github.issues",
            json!({
                "database":database,"repo":"owner/repo","request":{"operation":"detail","number":7}
            }),
        )
        .await;
    assert_eq!(
        detail["result"]["item"]["title"], "Host storage",
        "{detail}"
    );
    let unavailable = host.request(4, "state.get", json!({})).await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    let invalid = host.request(5, "github.issues", json!({
        "database":database,"repo":"owner/repo","request":{"operation":"state","unexpected":true}
    })).await;
    assert!(invalid.get("error").is_some(), "{invalid}");
    host.stop().await;
}

#[tokio::test]
async fn large_cached_detail_transfers_with_credit_and_preserves_the_host() {
    let directory = tempfile::tempdir().unwrap();
    let database = directory.path().join("issues.redb");
    let store =
        forge_github::issue_store::IssueStore::new(database.clone(), "owner/repo", Duration::ZERO)
            .unwrap();
    store
        .upsert_detail(
            7,
            forge_github::model::DetailRecord {
                repo: String::new(),
                number: 0,
                fetched_at: 123,
                item: json!({"body":"x".repeat(9 * 1024 * 1024)}),
            },
        )
        .unwrap();
    let mut host = Host::start().await;
    let detail = host
        .request(
            2,
            "github.issues",
            json!({
                "database":database,"repo":"owner/repo","request":{"operation":"detail","number":7}
            }),
        )
        .await;
    assert_eq!(
        detail["result"]["item"]["body"].as_str().unwrap().len(),
        9 * 1024 * 1024
    );
    let state = host
        .request(
            3,
            "github.issues",
            json!({
                "database":database,"repo":"owner/repo","request":{"operation":"state"}
            }),
        )
        .await;
    assert!(state.get("result").is_some(), "{state}");
    store
        .upsert_detail(
            7,
            forge_github::model::DetailRecord {
                repo: "owner/repo".into(),
                number: 7,
                fetched_at: 123,
                item: json!({"body":"x".repeat(forge_protocol::MAX_SNAPSHOT_BYTES)}),
            },
        )
        .unwrap();
    let refused = host.request(4, "github.issues", json!({"database":database,"repo":"owner/repo","request":{"operation":"detail","number":7}})).await;
    assert_eq!(refused["error"]["code"], "result_too_large");
    assert_eq!(refused["error"]["data"]["operation_completed"], true);
    host.stop().await;
}

#[tokio::test]
async fn repository_reads_do_not_require_harness_initialization() {
    let fixture = tempfile::tempdir().unwrap();
    git(fixture.path(), &["init", "--quiet"]);
    let data_root = fixture.path().join("harness-data");
    let mut host = Host::start().await;
    let response = host
        .request(
            2,
            "repository.revisions",
            json!({"workspace":fixture.path()}),
        )
        .await;
    assert_eq!(response["result"]["count"], 0, "{response}");
    assert!(!data_root.exists());
    let unknown = host.request(30, "state.unknown", json!({})).await;
    assert_eq!(unknown["id"], 30);
    assert_eq!(unknown["error"]["message"], "unknown Harness broker method");
    assert!(!data_root.exists());
    let unavailable = host.request(3, "state.get", json!({})).await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    assert!(!data_root.exists());
    let initialized = host
        .request(
            4,
            "harness.initialize",
            json!({
                "workspace":fixture.path(),"data_root":data_root,"client_id":"shared-host-test",
                "backend":{"kind":"mock","command":[]}
            }),
        )
        .await;
    assert!(
        initialized["result"]["session"]["id"].is_string(),
        "{initialized}"
    );
    let repeated = host
        .request(
            5,
            "repository.revisions",
            json!({"workspace":fixture.path()}),
        )
        .await;
    assert_eq!(repeated["result"]["count"], 0);
    let unknown = host.request(31, "state.unknown", json!({})).await;
    assert_eq!(unknown["id"], 31);
    assert_eq!(unknown["error"]["message"], "unknown Harness broker method");
    let state = host.request(32, "state.get", json!({})).await;
    assert_eq!(
        state["result"]["session"]["id"],
        initialized["result"]["session"]["id"]
    );
    host.stop().await;
}

#[tokio::test]
async fn revision_pages_preserve_arguments_and_reject_superseded_snapshots() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    std::fs::write(root.join("file"), b"content").unwrap();
    let object = git(root, &["hash-object", "-w", "file"]);
    let object = std::str::from_utf8(&object).unwrap().trim();
    let mut packed = String::from("# pack-refs with: sorted\n");
    let mut expected = Vec::new();
    for index in 0..5000 {
        let name = format!("branch-{index:05}-with-a-long-name-for-paging");
        packed.push_str(&format!("{object} refs/heads/{name}\n"));
        expected.extend_from_slice(name.as_bytes());
        expected.push(0);
    }
    std::fs::write(root.join(".git/packed-refs"), packed).unwrap();
    let mut host = Host::start().await;
    let first = host
        .request(2, "repository.revisions", json!({"workspace":root}))
        .await;
    let first = &first["result"];
    assert_eq!(first["count"], 5000, "{first}");
    let revision = first["revision"].as_u64().unwrap();
    let mut offset = first["next_offset"].as_u64().unwrap();
    assert!(offset < first["total_bytes"].as_u64().unwrap());
    let mut received = STANDARD.decode(first["data"].as_str().unwrap()).unwrap();
    let mut request_id = 3;
    while offset < expected.len() as u64 {
        let response = host
            .request(
                request_id,
                "repository.revisions",
                json!({"workspace":root,"revision":revision,"offset":offset,"repository":first["repository"],"reference_digest":first["reference_digest"]}),
            )
            .await;
        let page = &response["result"];
        assert_eq!(page["revision"], revision, "{response}");
        received.extend(STANDARD.decode(page["data"].as_str().unwrap()).unwrap());
        offset = page["next_offset"].as_u64().unwrap();
        request_id += 1;
    }
    assert_eq!(received, expected);
    let refreshed = host
        .request(
            request_id,
            "repository.revisions",
            json!({"workspace":root}),
        )
        .await;
    assert!(refreshed["result"]["revision"].as_u64().unwrap() > revision);
    let stale = host
        .request(
            request_id + 1,
            "repository.revisions",
            json!({"workspace":root,"revision":revision,"offset":0}),
        )
        .await;
    assert!(
        stale["error"]["message"]
            .as_str()
            .unwrap()
            .contains("superseded")
    );
    host.stop().await;
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
    host.stop().await;
}

#[tokio::test]
async fn status_context_and_source_documents_share_the_host_without_harness() {
    let fixture = tempfile::tempdir().unwrap();
    git(fixture.path(), &["init", "--quiet"]);
    git(fixture.path(), &["config", "user.name", "Forge Fixture"]);
    git(
        fixture.path(),
        &["config", "user.email", "forge@example.test"],
    );
    std::fs::write(fixture.path().join("sample.txt"), "original source\n").unwrap();
    git(fixture.path(), &["add", "sample.txt"]);
    git(
        fixture.path(),
        &[
            "commit",
            "--quiet",
            "-m",
            "Fixture subject",
            "-m",
            "Complete commit body.",
        ],
    );
    let mut host = Host::start().await;
    let opened = host
        .request(
            2,
            "status",
            json!({
                "operation":"open", "document":"status-fixture", "workspace":fixture.path()
            }),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let snapshot = &opened["result"];
    assert!(snapshot.get("block").is_none(), "{snapshot}");
    assert_eq!(snapshot["context"]["head"]["subject"], "Fixture subject");
    let captured = json!({
        "document":"status-fixture", "revision":snapshot["revision"], "view":"status-view",
        "sequence":1, "action":"open", "location":{"kind":"context","role":"head"}
    });
    let action = host
        .request(
            3,
            "status.context",
            json!({"operation":"action","input":captured}),
        )
        .await;
    assert_eq!(action["result"]["kind"], "commit", "{action}");
    let repeated = host
        .request(
            4,
            "status.context",
            json!({"operation":"action","input":captured}),
        )
        .await;
    assert!(
        repeated["error"]["message"]
            .as_str()
            .unwrap()
            .contains("already accepted"),
        "{repeated}"
    );
    let message = host.request(5, "status.context", json!({
        "operation":"commit_message", "document":"commit-fixture", "workspace":fixture.path(),
        "oid":action["result"]["oid"]
    })).await;
    assert!(message.get("error").is_none(), "{message}");
    let rendered = serde_json::to_string(&message["result"]["snapshot"]["block"]).unwrap();
    assert!(
        rendered.contains("Fixture subject") && rendered.contains("Complete commit body."),
        "{message}"
    );
    assert_eq!(message["result"]["more"], false, "{message}");
    let source = host
        .request(
            6,
            "source.document",
            json!({
                "operation":"open", "document":"source-fixture", "workspace":fixture.path(),
                "revision":action["result"]["oid"], "path":"c2FtcGxlLnR4dA=="
            }),
        )
        .await;
    assert!(source.get("error").is_none(), "{source}");
    assert!(
        serde_json::to_string(&source["result"]["snapshot"]["block"])
            .unwrap()
            .contains("original source"),
        "{source}"
    );
    let unavailable = host.request(7, "state.get", json!({})).await;
    assert!(
        unavailable["error"]["message"]
            .as_str()
            .unwrap()
            .contains("Harness is not initialized")
    );
    for (request, document) in [(8, "source-fixture"), (9, "commit-fixture")] {
        let closed = host
            .request(
                request,
                "source.document",
                json!({"operation":"close","document":document}),
            )
            .await;
        assert_eq!(closed["result"]["closed"], true, "{closed}");
    }
    let closed = host
        .request(
            10,
            "status",
            json!({"operation":"close","document":"status-fixture"}),
        )
        .await;
    assert_eq!(closed["result"]["closed"], true, "{closed}");
    host.stop().await;
}

#[tokio::test]
async fn harness_documents_keep_composer_revisions_and_close_lifetimes_separate() {
    let fixture = tempfile::tempdir().unwrap();
    git(fixture.path(), &["init", "--quiet"]);
    let mut host = Host::start().await;
    let unavailable = host
        .request(
            2,
            "harness.document",
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
            "harness.initialize",
            json!({
                "workspace":fixture.path(), "data_root":fixture.path().join("harness-data"),
                "client_id":"document-host-test", "backend":{"kind":"mock","command":[]}
            }),
        )
        .await;
    assert!(
        initialized["result"]["session"]["id"].is_string(),
        "{initialized}"
    );
    let opened = host
        .request(
            4,
            "harness.document",
            json!({
                "operation":"open", "document":"transcript", "composer":"composer", "view":"view",
                "width":{"columns":80,"tabstop":4}, "initial":["startup draft"]
            }),
        )
        .await;
    assert_eq!(
        opened["result"]["composer"]["block"][0]["text"],
        json!(["startup draft"]),
        "{opened}"
    );
    assert_eq!(opened["result"]["composer"]["revision"], 0, "{opened}");
    let transcript_revision = opened["result"]["transcript"]["revision"].clone();
    let edited = host
        .request(
            5,
            "harness.document",
            json!({
                "operation":"edit_composer", "edit":{"document":"composer","region":"composer",
                    "base":0,"sequence":1,"text":["newer λ", "", "draft"]}
            }),
        )
        .await;
    assert_eq!(edited["result"]["accepted"], true, "{edited}");
    assert_eq!(
        edited["result"]["acknowledgement"]["revision"], 1,
        "{edited}"
    );
    assert_eq!(edited["result"]["patch"]["base"], 0, "{edited}");
    assert_eq!(edited["result"]["patch"]["next"], 1, "{edited}");
    let composer = host
        .request(
            6,
            "harness.document",
            json!({
                "operation":"snapshot","document":"composer"
            }),
        )
        .await;
    assert_eq!(
        composer["result"]["block"][0]["text"],
        json!(["newer λ", "", "draft"]),
        "{composer}"
    );
    let synchronized = host
        .request(
            7,
            "harness.document",
            json!({
                "operation":"sync","document":"transcript","revision":transcript_revision
            }),
        )
        .await;
    assert_eq!(synchronized["result"]["patch"], json!([]), "{synchronized}");
    let submitted = host
        .request(
            21,
            "prompt.submit",
            json!({
                "mode":"chat", "composer":{"document":"composer","revision":1}
            }),
        )
        .await;
    assert!(submitted.get("error").is_none(), "{submitted}");
    assert!(
        host.progress
            .iter()
            .any(|event| event["event"] == "backend_event"
                && event["payload"]["kind"] == "composer_patch"),
        "missing native composer admission event"
    );
    let cleared = host
        .request(
            22,
            "harness.document",
            json!({
                "operation":"snapshot","document":"composer"
            }),
        )
        .await;
    assert_eq!(
        cleared["result"]["block"][0]["text"],
        json!([""]),
        "{cleared}"
    );
    let closed = host
        .request(
            8,
            "harness.document",
            json!({
                "operation":"close","document":"transcript"
            }),
        )
        .await;
    assert_eq!(closed["result"]["closed"], true, "{closed}");
    let stale = host
        .request(
            9,
            "harness.document",
            json!({
                "operation":"snapshot","document":"composer"
            }),
        )
        .await;
    assert!(
        stale["error"]["message"]
            .as_str()
            .unwrap()
            .contains("not open"),
        "{stale}"
    );
    let state = host.request(10, "state.get", json!({})).await;
    assert_eq!(
        state["result"]["session"]["id"],
        initialized["result"]["session"]["id"]
    );
    host.stop().await;
}

fn git(root: &std::path::Path, arguments: &[&str]) -> Vec<u8> {
    let output = forge_git::command::read_command(
        std::process::Command::new("git")
            .arg("-C")
            .arg(root)
            .args(arguments),
        forge_git::command::CommandLimits {
            stdout_bytes: 4096,
            stderr_bytes: 4096,
            timeout: Duration::from_secs(10),
        },
        || Ok(()),
    )
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
            "github.creation.context",
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
            "github.recovery.ack",
            json!({"resource":resource,"operation_id":"fixture-create"}),
        )
        .await;
    assert!(rejected_ack.get("error").is_some());
    let settled = host.request(5,"github.recovery.settle_draft",json!({"resource":resource,"operation_id":"fixture-create","draft":{"repo":"owner/repo","number":0,"creation":{"sequence":2,"title":"newer retained generation"}}})).await;
    assert!(settled.get("error").is_none(), "{settled}");
    let stored = host
        .request(6, "github.review.draft", json!({"resource":resource}))
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
async fn comment_route_preserves_directory_context_and_unknown_outcomes_without_harness() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "comment_ok");
    std::fs::write(
        root.path().join("expected-mutation"),
        "updatePullRequestReviewComment",
    )
    .unwrap();
    std::fs::write(
        root.path().join("comment-read.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("old")}})).unwrap(),
    )
    .unwrap();
    std::fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("saved λ\n")}})).unwrap(),
    )
    .unwrap();
    std::fs::write(root.path().join("comment-result.json"),serde_json::to_vec(&json!({"data":{"mutation":{"clientMutationId":"operation-1","comment":hosted_comment("saved λ\n")}}})).unwrap()).unwrap();
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let params = comment_params(
        root.path(),
        json!({"operation":"edit","body":"saved λ\n","receipt":"operation-1"}),
    );
    let response = host.request(2, "github.comment", params).await;
    assert_eq!(response["result"]["outcome"], "confirmed", "{response}");
    assert_eq!(response["result"]["state"]["body"], "saved λ\n");
    std::fs::write(root.path().join("mode"), "comment_uncertain").unwrap();
    std::fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment("newer")}})).unwrap(),
    )
    .unwrap();
    let params = comment_params(
        root.path(),
        json!({"operation":"edit","body":"newer","receipt":"operation-2"}),
    );
    let response = host.request(3, "github.comment", params.clone()).await;
    assert_eq!(
        response["result"]["outcome"], "outcome_unknown",
        "{response}"
    );
    let response = host.request(4, "github.comment", params).await;
    assert!(
        response["error"].to_string().contains("OutcomeUnknown"),
        "{response}"
    );
    let response = host
        .request(
            5,
            "github.comment",
            comment_params(root.path(), json!({"operation":"reconcile"})),
        )
        .await;
    assert_eq!(response["result"]["outcome"], "reconciled", "{response}");
    assert_eq!(response["result"]["state"]["body"], "newer");
    let mut invalid = comment_params(
        root.path(),
        json!({"operation":"delete","receipt":"operation-3"}),
    );
    invalid["unexpected"] = json!(true);
    assert!(
        host.request(6, "github.comment", invalid)
            .await
            .get("error")
            .is_some()
    );
    assert_eq!(
        std::fs::read_to_string(root.path().join("comment-mutation-log"))
            .unwrap()
            .lines()
            .count(),
        2
    );
    assert!(host.progress.is_empty());
    host.stop().await;
}

#[tokio::test]
async fn large_comment_observation_uses_correlated_result_transfer() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "comment_ok");
    let body = "\"".repeat(200 * 1024);
    std::fs::write(
        root.path().join("comment-read.json"),
        serde_json::to_vec(&json!({"data":{"node":hosted_comment(&body)}})).unwrap(),
    )
    .unwrap();
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let response = host
        .request(
            2,
            "github.comment",
            comment_params(root.path(), json!({"operation":"reconcile"})),
        )
        .await;
    assert_eq!(response["result"]["outcome"], "reconciled", "{response}");
    assert_eq!(response["result"]["state"]["body"], body);
    assert!(!root.path().join("comment-mutation-log").exists());
    host.stop().await;
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
            "review.open_pr",
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
            "review.comment",
            json!({"document":document,"command":{"operation":"load","target":target}}),
        )
        .await;
    let snapshot = &loaded["result"]["snapshot"];
    assert_eq!(snapshot["baseline"], "remote old", "{loaded}");
    let comment = snapshot["comment"].clone();
    let region = snapshot["region"].clone();
    let changed = host
        .request(
            4,
            "review.region_edit",
            json!({"document":document,"region":region,"base":0,"sequence":1,"text":"submitted"}),
        )
        .await;
    assert!(changed.get("error").is_none(), "{changed}");
    let saved = host.request(5,"review.comment",json!({"document":document,"command":{"operation":"save","comment":comment,"action":"save"}})).await;
    assert_eq!(
        saved["result"]["snapshot"]["baseline"], "submitted",
        "{saved}"
    );
    assert_eq!(saved["result"]["snapshot"]["dirty"], false);
    let changed = host
        .request(
            6,
            "review.region_edit",
            json!({"document":document,"region":region,"base":1,"sequence":2,"text":"uncertain"}),
        )
        .await;
    assert!(changed.get("error").is_none());
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
    let saved = host.request(7,"review.comment",json!({"document":document,"command":{"operation":"save","comment":comment,"action":"save"}})).await;
    assert_eq!(saved["result"]["snapshot"]["uncertain"], true, "{saved}");
    assert_eq!(saved["result"]["snapshot"]["baseline"], "submitted");
    let changed = host
        .request(
            8,
            "review.region_edit",
            json!({"document":document,"region":region,"base":2,"sequence":3,"text":"newer"}),
        )
        .await;
    assert!(changed.get("error").is_none());
    let reconciled = host
        .request(
            9,
            "review.comment",
            json!({"document":document,"command":{"operation":"reconcile","comment":comment}}),
        )
        .await;
    assert_eq!(
        reconciled["result"]["snapshot"]["baseline"], "uncertain",
        "{reconciled}"
    );
    assert_eq!(reconciled["result"]["snapshot"]["text"], "newer");
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

#[tokio::test]
async fn review_conversation_draft_creates_through_native_host() {
    let root = tempfile::tempdir().unwrap();
    sync_fixture(root.path(), "pr_success");
    let mut host = Host::start_with_gh(Some(gh_executable())).await;
    let target = comment_params(root.path(), json!({}))["target"]["pull_request"].clone();
    let opened = host
        .request(
            2,
            "review.open_pr",
            json!({"directory":root.path(),"target":target}),
        )
        .await;
    let document = opened["result"]["document"].clone();
    assert!(document.is_string(), "{opened}");
    let draft = host
        .request(
            3,
            "review.comment",
            json!({"document":document,"command":{"operation":"draft_conversation"}}),
        )
        .await;
    let comment = draft["result"]["snapshot"]["comment"].clone();
    let region = draft["result"]["snapshot"]["region"].clone();
    assert!(region.is_string(), "{draft}");
    let body = "created λ\n  body\n\n";
    std::fs::write(
        root.path().join("rest-result.json"),
        serde_json::to_vec(&hosted_rest_comment(body)).unwrap(),
    )
    .unwrap();
    let edited = host
        .request(
            4,
            "review.region_edit",
            json!({"document":document,"region":region,"base":0,"sequence":1,"text":body}),
        )
        .await;
    assert!(edited.get("error").is_none(), "{edited}");
    std::fs::write(root.path().join("mode"), "creation_ok").unwrap();
    std::fs::write(root.path().join("creation-echo-receipt"), "yes").unwrap();
    for (field, value) in [("subjectId", "PR_fixture"), ("body", body)] {
        std::fs::write(
            root.path().join(format!("expected-{field}")),
            serde_json::to_vec(value).unwrap(),
        )
        .unwrap();
    }
    std::fs::write(root.path().join("creation-parent.json"),serde_json::to_vec(&json!({"data":{"repository":{"pullRequest":{"id":"PR_fixture","number":7,"state":"OPEN","isDraft":false}}}})).unwrap()).unwrap();
    let mut state = hosted_comment(body);
    state["__typename"] = json!("IssueComment");
    std::fs::write(root.path().join("creation-result.json"),serde_json::to_vec(&json!({"data":{"mutation":{"clientMutationId":"__receipt__","subject":{"id":"PR_fixture"},"commentEdge":{"node":state}}}})).unwrap()).unwrap();
    let saved = host.request(5,"review.comment",json!({"document":document,"command":{"operation":"save","comment":comment,"action":"save"}})).await;
    assert_eq!(saved["result"]["snapshot"]["baseline"], body, "{saved}");
    assert_eq!(saved["result"]["snapshot"]["dirty"], false);
    assert_eq!(saved["result"]["snapshot"]["comment"], comment);
    assert_eq!(saved["result"]["snapshot"]["region"], region);
    assert_eq!(saved["result"]["remote"]["outcome"], "confirmed");
    assert_eq!(
        std::fs::read_to_string(root.path().join("creation-log"))
            .unwrap()
            .lines()
            .count(),
        1
    );
    host.stop().await;
}

#[tokio::test]
async fn notification_pages_preserve_global_scope_and_reject_malformed_results() {
    let root = tempfile::tempdir().unwrap();
    let record = |repository: &str, id: &str| {
        json!({
            "id": id, "repository": {"full_name": repository}, "unread": true,
            "reason": "participating", "updated_at": "2026-09-07T00:00:00Z",
            "subject": {"title": "Captured notification", "type": "Issue",
                "url": format!("https://enterprise.example/api/v3/repos/{repository}/issues/7"),
                "latest_comment_url": null}
        })
    };
    std::fs::write(
        root.path().join("notification-page.json"),
        serde_json::to_vec(&vec![
            record("owner/first", "1"),
            record("other/second", "2"),
        ])
        .unwrap(),
    )
    .unwrap();
    let mut host = Host::start_with_fixture(Some(gh_executable()), false).await;
    let params =
        json!({"directory":root.path(),"request":{"hostname":"enterprise.example","cursor":null}});
    let page = host
        .request(2, "github.notifications.page", params.clone())
        .await;
    assert!(page.get("error").is_none(), "{page}");
    assert_eq!(page["result"]["record"][0]["repository"]["name"], "first");
    assert_eq!(page["result"]["record"][1]["repository"]["owner"], "other");
    assert_eq!(page["result"]["complete"], true);
    let opened = host
        .request(
            3,
            "notifications.document",
            json!({"operation":"open",
        "document":"global-notifications","workspace":root.path(),"hostname":"enterprise.example"}),
        )
        .await;
    assert!(opened.get("error").is_none(), "{opened}");
    let view = host.request(4, "notifications.document", json!({"operation":"view",
        "document":"global-notifications","view":"notification-window","width":forge_buffer::width::WidthProfile::default()})).await;
    assert!(view.get("error").is_none(), "{view}");
    let snapshot = host
        .request(
            5,
            "notifications.document",
            json!({"operation":"snapshot","document":"global-notifications"}),
        )
        .await;
    let block = snapshot["result"]["block"]
        .as_array()
        .unwrap()
        .iter()
        .find(|block| block["id"] == "notification:2")
        .unwrap();
    let input = json!({"document":"global-notifications","revision":snapshot["result"]["revision"],
        "view":"notification-window","sequence":1,"action":"open","block":block["id"],
        "position":{"row":0,"column":0},"target":block["metadata"]["target"][0]["id"]});
    let action = host
        .request(
            6,
            "notifications.document",
            json!({"operation":"act","input":input}),
        )
        .await;
    assert!(action.get("error").is_none(), "{action}");
    assert_eq!(action["result"]["effect"]["repository"]["owner"], "other");
    assert_eq!(
        action["result"]["effect"]["repository"]["hostname"],
        "enterprise.example"
    );
    let duplicate = host
        .request(
            7,
            "notifications.document",
            json!({"operation":"act","input":input}),
        )
        .await;
    assert!(duplicate.get("error").is_some(), "{duplicate}");
    let closed = host
        .request(
            8,
            "notifications.document",
            json!({"operation":"close","document":"global-notifications"}),
        )
        .await;
    assert_eq!(closed["result"]["closed"], true);
    std::fs::write(root.path().join("notification-page.json"), "{}").unwrap();
    let failure = host.request(9, "github.notifications.page", params).await;
    assert!(failure.get("error").is_some(), "{failure}");
    host.stop().await;
}
