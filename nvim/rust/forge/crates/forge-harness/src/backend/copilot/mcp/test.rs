use super::*;
use crate::backend::copilot::CopilotBackend;
use crate::backend::{
    Backend, BackendCatalogRequest, BackendInput, BackendRequest, McpStatus, PromptMode,
};
use crate::session::ApprovalMode;
use github_copilot_sdk::Client;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

async fn read_request(reader: &mut DuplexStream) -> Option<Value> {
    let mut header = Vec::new();
    while !header.ends_with(b"\r\n\r\n") {
        header.push(reader.read_u8().await.ok()?);
        assert!(header.len() < 1024);
    }
    let length: usize = std::str::from_utf8(&header)
        .unwrap()
        .trim()
        .strip_prefix("Content-Length: ")
        .unwrap()
        .parse()
        .unwrap();
    assert!(length < 1024 * 1024);
    let mut body = vec![0; length];
    reader.read_exact(&mut body).await.unwrap();
    Some(serde_json::from_slice(&body).unwrap())
}

async fn write_message(writer: &mut DuplexStream, message: Value) {
    let body = serde_json::to_vec(&message).unwrap();
    writer
        .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
        .await
        .unwrap();
    writer.write_all(&body).await.unwrap();
    writer.flush().await.unwrap();
}

struct Fixture {
    directory: tempfile::TempDir,
    backend: CopilotBackend,
    recovered: Arc<AtomicBool>,
    request: Arc<Mutex<Vec<String>>>,
    server: tokio::task::JoinHandle<()>,
}

impl Fixture {
    async fn new(names: &[&str]) -> Self {
        let directory = tempfile::tempdir().unwrap();
        let config = directory.path().join("config");
        let source = config.join("rulesync-global/.rulesync");
        std::fs::create_dir_all(&source).unwrap();
        let servers: serde_json::Map<String, Value> = names
            .iter()
            .map(|name| {
                (
                    (*name).into(),
                    json!({"command":std::env::current_exe().unwrap(), "args":[]}),
                )
            })
            .collect();
        std::fs::write(
            source.join("mcp.json"),
            json!({"mcpServers":servers}).to_string(),
        )
        .unwrap();
        let data = directory.path().join("data");
        crate::settings::SettingsStore::open(&data)
            .unwrap()
            .configure_catalog(&config)
            .unwrap();
        let trace = Arc::new(TraceStore::open(&data).unwrap());
        trace.configure(true).unwrap();
        let backend = CopilotBackend::new_with_permission_coordinator(
            Vec::new(),
            crate::backend::approval::PermissionCoordinator::transient(directory.path()).unwrap(),
            trace,
            Some(data),
        )
        .unwrap();
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        let (mut server_write, client_read) = tokio::io::duplex(65536);
        *backend.client.lock().await =
            Some(Client::from_streams(client_read, client_write, directory.path().into()).unwrap());
        let recovered = Arc::new(AtomicBool::new(false));
        let recovery = Arc::clone(&recovered);
        let request = Arc::new(Mutex::new(Vec::new()));
        let requests = Arc::clone(&request);
        let names = names
            .iter()
            .map(|name| (*name).to_owned())
            .collect::<Vec<_>>();
        let server = tokio::spawn(async move {
            let mut provider_session = String::new();
            while let Some(message) = read_request(&mut server_read).await {
                let method = message["method"].as_str().unwrap();
                requests.lock().unwrap().push(method.into());
                if method == "session.mcp.startServer"
                    && message["params"]["serverName"] == "stalled"
                {
                    continue;
                }
                let result = match method {
                    "session.create" => {
                        provider_session =
                            message["params"]["sessionId"].as_str().unwrap().to_owned();
                        json!({"sessionId":provider_session,"workspacePath":"workspace"})
                    }
                    "session.skills.list" => json!({"skills":[]}),
                    "session.mcp.startServer" if message["params"]["serverName"] == "broken" => {
                        write_message(
                            &mut server_write,
                            json!({"jsonrpc":"2.0","id":message["id"],
                            "error":{"code":-32000,"message":"MCP process exited"}}),
                        )
                        .await;
                        continue;
                    }
                    "session.mcp.disable" if message["params"]["serverName"] == "rogue" => {
                        write_message(
                            &mut server_write,
                            json!({"jsonrpc":"2.0","id":message["id"],
                            "error":{"code":-32000,"message":"Cannot exclude unselected server"}}),
                        )
                        .await;
                        continue;
                    }
                    "session.mcp.startServer" | "session.mcp.disable" | "session.destroy" => {
                        json!({})
                    }
                    "session.mcp.list" => {
                        json!({"servers":names.iter().filter(|name| name.as_str() != "broken").map(|name|
                        json!({"name":name,"status":if recovery.load(Ordering::SeqCst) { "connected" }
                            else { match name.as_str() { "disabled" => "disabled", "stalled" => "pending", _ => "connected" } }}))
                        .collect::<Vec<_>>()})
                    }
                    "session.mcp.listTools" => json!({"tools":[]}),
                    "session.metadata.getContextAttribution" => json!({"contextAttribution":null}),
                    "session.plan.readSqlTodosWithDependencies" => {
                        json!({"rows":[],"dependencies":[]})
                    }
                    "session.send" => json!({"messageId":"reply"}),
                    "session.eventLog.tail" => json!({"cursor":"before-prompt"}),
                    other => panic!("unexpected Copilot request {other}"),
                };
                write_message(
                    &mut server_write,
                    json!({"jsonrpc":"2.0","id":message["id"],"result":result}),
                )
                .await;
                if method == "session.send" {
                    for index in 0..4096 {
                        write_message(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event",
                            "params":{"sessionId":provider_session,"event":{"id":format!("stream-{index}"),
                                "timestamp":"2026-10-14T00:00:00Z","type":if index % 2 == 0 {
                                    "assistant.streaming_delta"
                                } else { "assistant.tool_call_delta" },"ephemeral":true,
                                "data":{"totalResponseSizeBytes":index}}}})).await;
                    }
                    for (event_type, data) in [
                        ("assistant.message", json!({"content":"Repository answer"})),
                        ("assistant.idle", json!({})),
                    ] {
                        write_message(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event",
                            "params":{"sessionId":provider_session,"event":{"id":event_type,
                                "timestamp":"2026-10-14T00:00:00Z","type":event_type,"data":data}}})).await;
                    }
                }
            }
        });
        Self {
            directory,
            backend,
            recovered,
            request,
            server,
        }
    }

    fn catalog(&self) -> BackendCatalogRequest {
        BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "mcp-failure".into(),
            workspace: self.directory.path().to_string_lossy().into_owned(),
            access: crate::session::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
        }
    }

    fn prompt(&self) -> BackendRequest {
        BackendRequest {
            execution_owner: None,
            harness_session_id: "mcp-failure".into(),
            workspace: self.directory.path().to_string_lossy().into_owned(),
            input: BackendInput::Text {
                attachments: Vec::new(),
                text: "what is this repo?".into(),
            },
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "auto".into(),
            context_window: None,
            service_tier: Default::default(),
            access: crate::session::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        self.server.abort();
    }
}

#[tokio::test]
async fn mcp_failures_keep_prompt_and_catalog_available() {
    tokio::time::timeout(Duration::from_secs(5), async {
        let fixture = Fixture::new(&["broken", "disabled", "healthy"]).await;
        let output = fixture
            .backend
            .prompt_stream(fixture.prompt(), None)
            .await
            .unwrap();
        assert!(
            output
                .event
                .iter()
                .any(|event| event.kind == "assistant_message"
                    && event.text.as_deref() == Some("Repository answer"))
        );
        assert!(!output.event.iter().any(|event| event.kind == "error"));
        let catalog = fixture.backend.mcp_list(fixture.catalog()).await.unwrap();
        assert_eq!(
            catalog
                .iter()
                .find(|server| server.name == "broken")
                .unwrap()
                .status,
            McpStatus::Failed
        );
        assert!(
            catalog
                .iter()
                .find(|server| server.name == "broken")
                .unwrap()
                .status_detail
                .as_deref()
                .unwrap()
                .contains("MCP process exited")
        );
        assert_eq!(
            catalog
                .iter()
                .find(|server| server.name == "disabled")
                .unwrap()
                .status,
            McpStatus::Failed
        );
        assert_eq!(
            catalog
                .iter()
                .find(|server| server.name == "healthy")
                .unwrap()
                .status,
            McpStatus::Connected
        );
        let trace =
            std::fs::read_to_string(fixture.backend.trace.session_status("mcp-failure").path)
                .unwrap();
        assert_eq!(
            trace
                .lines()
                .filter(|line| line.contains("copilot.mcp.start_failed"))
                .count(),
            2
        );
        assert!(trace.contains("MCP process exited"));
        assert!(
            !fixture
                .request
                .lock()
                .unwrap()
                .iter()
                .any(|method| method == "session.destroy")
        );
        fixture.recovered.store(true, Ordering::SeqCst);
        let recovered = fixture.backend.mcp_list(fixture.catalog()).await.unwrap();
        let recovered = recovered
            .iter()
            .find(|server| server.name == "disabled")
            .unwrap();
        assert_eq!(recovered.status, McpStatus::Connected);
        assert!(recovered.status_detail.is_none());
        assert_eq!(
            fixture
                .request
                .lock()
                .unwrap()
                .iter()
                .filter(|method| method.as_str() == "session.create")
                .count(),
            1
        );
        fixture.backend.shutdown().await.unwrap();
    })
    .await
    .expect("MCP failure regression timed out");
}

#[tokio::test]
async fn mcp_timeout_preserves_healthy_servers_and_session() {
    tokio::time::timeout(Duration::from_secs(5), async {
        let fixture = Fixture::new(&["healthy"]).await;
        let session = fixture.backend.session(&fixture.prompt()).await.unwrap();
        let launch = vec![
            ("stalled".into(), json!({"command":"unused","args":[]})),
            ("healthy".into(), json!({"command":"unused","args":[]})),
        ];
        let failed = start(
            &session,
            &launch,
            &fixture.backend.trace,
            "mcp-timeout",
            Duration::from_millis(50),
        )
        .await
        .unwrap();
        assert!(failed["stalled"].contains("timed out"));
        assert!(!failed.contains_key("healthy"));
        assert_eq!(
            fixture
                .request
                .lock()
                .unwrap()
                .iter()
                .filter(|method| method.as_str() == "session.mcp.startServer")
                .count(),
            3
        );
        assert!(
            !fixture
                .request
                .lock()
                .unwrap()
                .iter()
                .any(|method| method == "session.destroy")
        );
        let output = fixture
            .backend
            .prompt_stream(fixture.prompt(), None)
            .await
            .unwrap();
        assert!(
            output
                .event
                .iter()
                .any(|event| event.text.as_deref() == Some("Repository answer"))
        );
        let trace =
            std::fs::read_to_string(fixture.backend.trace.session_status("mcp-timeout").path)
                .unwrap();
        assert!(trace.contains("copilot.mcp.start_failed") && trace.contains("timed out"));
        fixture.backend.shutdown().await.unwrap();
    })
    .await
    .expect("MCP timeout regression timed out");
}

#[tokio::test]
async fn mcp_selection_failure_still_rejects_unselected_tools() {
    tokio::time::timeout(Duration::from_secs(5), async {
        let fixture = Fixture::new(&["rogue"]).await;
        let session = fixture.backend.session(&fixture.prompt()).await.unwrap();
        let failure = start(
            &session,
            &[],
            &fixture.backend.trace,
            "mcp-selection",
            Duration::from_secs(1),
        )
        .await
        .unwrap_err();
        assert!(format!("{failure:#}").contains("Cannot exclude unselected server"));
        fixture.backend.shutdown().await.unwrap();
    })
    .await
    .expect("MCP selection regression timed out");
}
