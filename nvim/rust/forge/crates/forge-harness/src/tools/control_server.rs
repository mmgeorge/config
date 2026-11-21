use super::ControlToolDefinition;
use super::control_session::{ControlEndpoint, ControlReply};
use crate::tools::mcp::{CallResult, Empty, Initialization, Response};
use anyhow::{Context, Result, ensure};
use forge_provider_api::control::{ToolCall, ToolCallId, ToolOutcome};
use serde::Serialize;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::sync::OnceCell;
use tokio_util::sync::CancellationToken;

const ACCEPT_RETRY_DELAY: Duration = Duration::from_millis(250);
const SOCKET_IO_TIMEOUT: Duration = Duration::from_secs(10);

type SessionRegistry = Mutex<HashMap<uuid::Uuid, Arc<McpSession>>>;

enum HttpCommand {
    Post(Value),
    Delete,
    Unsupported,
}

/// Keep MCP envelopes typed around the open payload supplied by a control tool.
#[derive(Serialize)]
#[serde(untagged)]
enum Payload {
    Initialization(Initialization),
    Empty(Empty),
    Tools(ToolList),
    Call(CallResult),
}

#[derive(Serialize)]
struct ToolList {
    tools: Vec<ControlToolDefinition>,
}

struct HttpRequest {
    command: HttpCommand,
    session_id: Option<String>,
}

struct HttpResponse {
    status: &'static str,
    body: Vec<u8>,
    opened_session: Option<uuid::Uuid>,
}

impl HttpResponse {
    /// Create a reply without a JSON-RPC response body.
    fn empty(status: &'static str) -> Self {
        Self {
            status,
            body: Vec::new(),
            opened_session: None,
        }
    }
}

#[derive(Default)]
struct McpSession {
    calls: Mutex<HashMap<ToolCallId, Arc<McpCall>>>,
}

struct McpCall {
    name: String,
    arguments: Value,
    reply: OnceCell<ControlReply>,
}

impl McpSession {
    /// Preserve the first dispatch and its result while this protocol session owns the ID.
    async fn call(&self, handler: &dyn ControlEndpoint, call: ToolCall) -> Result<ControlReply> {
        let record = self
            .calls
            .lock()
            .map_err(|_| anyhow::anyhow!("MCP call registry poisoned"))?
            .entry(call.id.clone())
            .or_insert_with(|| {
                Arc::new(McpCall {
                    name: call.name.clone(),
                    arguments: call.arguments.clone(),
                    reply: OnceCell::new(),
                })
            })
            .clone();
        ensure!(
            record.name == call.name && record.arguments == call.arguments,
            "MCP request identity was reused with different tool input"
        );
        Ok(record
            .reply
            .get_or_init(|| async {
                match handler.call(call).await {
                    Ok(reply) => reply,
                    Err(error) => ControlReply {
                        message: format!("{error:#}"),
                        outcome: ToolOutcome::Failed,
                    },
                }
            })
            .await
            .clone())
    }
}

/// Owns the loopback control endpoint for one provider binding.
pub(crate) struct ControlServer {
    /// Retains the unguessable endpoint advertised to the provider for this binding.
    pub(crate) url: String,
    stop: CancellationToken,
    driver: tokio::sync::Mutex<Option<tokio::task::JoinHandle<()>>>,
}

impl ControlServer {
    /// Bind before provider startup so required Harness tools cannot race session creation.
    pub(crate) async fn start(handler: Arc<dyn ControlEndpoint>) -> Result<Self> {
        let listener = tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 0)).await?;
        let path = format!("/{}", uuid::Uuid::new_v4());
        let url = format!("http://{}{}", listener.local_addr()?, path);
        let stop = CancellationToken::new();
        let stopping = stop.clone();
        let driver = tokio::spawn(async move {
            let admission = Arc::new(tokio::sync::Semaphore::new(16));
            let sessions = Arc::new(SessionRegistry::default());
            let mut connection = tokio::task::JoinSet::new();
            let mut retry_at = None;
            loop {
                tokio::select! {
                    biased;
                    _ = stopping.cancelled() => break,
                    _ = connection.join_next(), if !connection.is_empty() => {}
                    accepted = async {
                        if let Some(retry_at) = retry_at {
                            tokio::time::sleep_until(retry_at).await;
                        }
                        let permit = admission.clone().acquire_owned().await.ok()?;
                        Some(listener.accept().await.map(|(socket, _)| (socket, permit)))
                    } => {
                        let Some(accepted) = accepted else { break };
                        let (socket, permit) = match accepted {
                            Ok(accepted) => accepted,
                            Err(error) => {
                                // Keep the provider's advertised URL live through transient accept errors.
                                tracing::debug!(%error, "Retrying Forge control endpoint accept");
                                retry_at = Some(tokio::time::Instant::now() + ACCEPT_RETRY_DELAY);
                                continue;
                            }
                        };
                        retry_at = None;
                        let handler = handler.clone();
                        let path = path.clone();
                        let sessions = sessions.clone();
                        let stopping = stopping.clone();
                        connection.spawn(async move {
                            let _permit = permit;
                            let _ = serve(socket, &path, handler, sessions, stopping).await;
                        });
                    }
                }
            }
            // Close TCP admission before waiting for already dispatched tools to finish.
            drop(listener);
            while connection.join_next().await.is_some() {}
        });
        Ok(Self {
            url,
            stop,
            driver: tokio::sync::Mutex::new(Some(driver)),
        })
    }

    /// Stop admission and collect every connection already accepted by this endpoint.
    pub(crate) async fn close(&self) -> std::result::Result<(), tokio::task::JoinError> {
        self.stop.cancel();
        let mut driver = self.driver.lock().await;
        if let Some(task) = driver.as_mut() {
            let result = task.await;
            driver.take();
            result?;
        }
        Ok(())
    }
}

impl Drop for ControlServer {
    fn drop(&mut self) {
        self.stop.cancel();
        if let Some(driver) = self.driver.get_mut().take() {
            driver.abort();
        }
    }
}

/// Finish admitted tool effects even if shutdown interrupts reading or writing their HTTP exchange.
async fn serve(
    socket: tokio::net::TcpStream,
    path: &str,
    handler: Arc<dyn ControlEndpoint>,
    sessions: Arc<SessionRegistry>,
    stopping: CancellationToken,
) -> Result<()> {
    let mut reader = BufReader::new(socket);
    let request = tokio::select! {
        biased;
        _ = stopping.cancelled() => return Ok(()),
        request = tokio::time::timeout(SOCKET_IO_TIMEOUT, read_request(&mut reader, path)) => {
            request.context("control request input timed out")?
        }
    };
    let response = match request {
        Ok(request) => respond(request, handler.as_ref(), &sessions, &stopping).await?,
        Err(_) => HttpResponse::empty("400 Bad Request"),
    };
    if let Some(identity) = response.opened_session {
        sessions
            .lock()
            .map_err(|_| anyhow::anyhow!("MCP session registry poisoned"))?
            .insert(identity, Arc::new(McpSession::default()));
        if let Err(error) = write_response(reader.get_mut(), &response, &stopping).await {
            sessions
                .lock()
                .map_err(|_| anyhow::anyhow!("MCP session registry poisoned"))?
                .remove(&identity);
            return Err(error);
        }
        return Ok(());
    }
    write_response(reader.get_mut(), &response, &stopping).await
}

/// Parse the endpoint's single HTTP request without allocating from an untrusted declared length.
async fn read_request(
    reader: &mut (impl AsyncBufReadExt + Unpin),
    path: &str,
) -> Result<HttpRequest> {
    let mut line = read_header_line(reader, 8192).await?;
    ensure!(
        line.split_whitespace().nth(1) == Some(path),
        "control endpoint mismatch"
    );
    let method = line
        .split_whitespace()
        .next()
        .unwrap_or_default()
        .to_owned();
    let mut length = None;
    let mut chunked = false;
    let mut session_id = None;
    let mut header_bytes = line.len();
    loop {
        line = read_header_line(reader, 8192 - header_bytes).await?;
        let bytes = line.len();
        ensure!(bytes > 0, "control request ended before headers");
        header_bytes += bytes;
        ensure!(header_bytes <= 8192, "control headers exceed 8192 bytes");
        if line == "\r\n" || line == "\n" {
            break;
        }
        if let Some((name, value)) = line.split_once(':') {
            if name.eq_ignore_ascii_case("content-length") {
                ensure!(length.is_none(), "duplicate HTTP content length");
                length = Some(value.trim().parse::<u64>()?);
            } else if name.eq_ignore_ascii_case("transfer-encoding") {
                ensure!(
                    !chunked && value.trim().eq_ignore_ascii_case("chunked"),
                    "unsupported HTTP transfer encoding"
                );
                chunked = true;
            } else if name.eq_ignore_ascii_case("mcp-session-id") {
                ensure!(session_id.is_none(), "duplicate MCP session identity");
                session_id = Some(value.trim().to_owned());
            }
        }
    }
    ensure!(
        !chunked || length.is_none(),
        "conflicting HTTP body framing"
    );
    let command = match method.as_str() {
        "POST" => {
            let mut body = Vec::new();
            if chunked {
                read_chunked(reader, &mut body).await?;
            } else {
                read_body_bytes(
                    reader,
                    &mut body,
                    length.context("control request requires body framing")?,
                )
                .await?;
            }
            HttpCommand::Post(serde_json::from_slice(&body)?)
        }
        "DELETE" => HttpCommand::Delete,
        _ => HttpCommand::Unsupported,
    };
    Ok(HttpRequest {
        command,
        session_id,
    })
}

/// Append only bytes the client sent, and verify it fulfilled this part of the HTTP framing.
async fn read_body_bytes(
    reader: &mut (impl AsyncBufReadExt + Unpin),
    body: &mut Vec<u8>,
    length: u64,
) -> Result<()> {
    let mut framed = reader.take(length);
    framed.read_to_end(body).await?;
    ensure!(framed.limit() == 0, "control request ended before its body");
    Ok(())
}

/// Decode HTTP/1.1 chunked framing while keeping trailers outside the request's authority headers.
async fn read_chunked(
    reader: &mut (impl AsyncBufReadExt + Unpin),
    body: &mut Vec<u8>,
) -> Result<()> {
    let mut line = String::new();
    loop {
        line.clear();
        reader.read_line(&mut line).await?;
        let header = line
            .strip_suffix("\r\n")
            .context("invalid HTTP chunk header")?;
        let length = header.split(';').next().unwrap_or_default().trim_end();
        ensure!(
            !length.is_empty() && length.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "invalid HTTP chunk length"
        );
        let length = u64::from_str_radix(length, 16)?;
        if length == 0 {
            break;
        }
        read_body_bytes(reader, body, length).await?;
        let mut end = [0; 2];
        reader.read_exact(&mut end).await?;
        ensure!(&end == b"\r\n", "invalid HTTP chunk terminator");
    }
    loop {
        line.clear();
        reader.read_line(&mut line).await?;
        ensure!(line.ends_with("\r\n"), "invalid HTTP trailer");
        if line == "\r\n" {
            return Ok(());
        }
    }
}

/// Read one metadata line within the remaining header budget.
async fn read_header_line(
    reader: &mut (impl AsyncBufReadExt + Unpin),
    remaining: usize,
) -> Result<String> {
    let mut bytes = Vec::new();
    (&mut *reader)
        .take((remaining + 1) as u64)
        .read_until(b'\n', &mut bytes)
        .await?;
    ensure!(
        bytes.len() <= remaining,
        "control headers exceed 8192 bytes"
    );
    Ok(String::from_utf8(bytes)?)
}

/// Resolve the HTTP session and run only requests that belong to that session.
async fn respond(
    request: HttpRequest,
    handler: &dyn ControlEndpoint,
    sessions: &SessionRegistry,
    stopping: &CancellationToken,
) -> Result<HttpResponse> {
    let HttpRequest {
        command,
        session_id,
    } = request;
    let session = if let Some(session_id) = session_id {
        let Ok(identity) = uuid::Uuid::parse_str(&session_id) else {
            return Ok(HttpResponse::empty("404 Not Found"));
        };
        let mut known = sessions
            .lock()
            .map_err(|_| anyhow::anyhow!("MCP session registry poisoned"))?;
        if matches!(command, HttpCommand::Delete) {
            return Ok(HttpResponse::empty(if known.remove(&identity).is_some() {
                "200 OK"
            } else {
                "404 Not Found"
            }));
        }
        let Some(session) = known.get(&identity) else {
            return Ok(HttpResponse::empty("404 Not Found"));
        };
        Some((identity, session.clone()))
    } else {
        None
    };
    let payload = match command {
        HttpCommand::Post(payload) => payload,
        HttpCommand::Delete => return Ok(HttpResponse::empty("400 Bad Request")),
        HttpCommand::Unsupported => return Ok(HttpResponse::empty("405 Method Not Allowed")),
    };
    if payload["method"] == "initialize" {
        let Some(id) = payload
            .get("id")
            .filter(|id| id.is_number() || id.is_string())
        else {
            return Ok(HttpResponse::empty("400 Bad Request"));
        };
        if session.is_some() {
            return Ok(HttpResponse::empty("400 Bad Request"));
        }
        return Ok(HttpResponse {
            status: "200 OK",
            body: crate::codec::to_vec(&Response::success(
                id,
                Payload::Initialization(Initialization::new("1")),
            ))?,
            opened_session: Some(uuid::Uuid::new_v4()),
        });
    }
    let Some((identity, session)) = session else {
        return Ok(HttpResponse::empty("400 Bad Request"));
    };
    let body = if let Value::Array(batch) = &payload {
        if batch.is_empty() {
            crate::codec::to_vec(&Response::failure(
                &Value::Null,
                -32600,
                "Empty MCP request batch".into(),
            ))?
        } else {
            let mut responses = Vec::new();
            for message in batch {
                ensure!(!stopping.is_cancelled(), "control endpoint retired");
                if let Some(response) = dispatch(message, handler, identity, &session).await? {
                    responses.push(response);
                }
            }
            if responses.is_empty() {
                return Ok(HttpResponse::empty("202 Accepted"));
            }
            crate::codec::to_vec(&responses)?
        }
    } else {
        ensure!(!stopping.is_cancelled(), "control endpoint retired");
        let Some(response) = dispatch(&payload, handler, identity, &session).await? else {
            return Ok(HttpResponse::empty("202 Accepted"));
        };
        crate::codec::to_vec(&response)?
    };
    Ok(HttpResponse {
        status: "200 OK",
        body,
        opened_session: None,
    })
}

/// Dispatch one message from an established MCP session and encode its JSON-RPC response.
async fn dispatch(
    request: &Value,
    handler: &dyn ControlEndpoint,
    identity: uuid::Uuid,
    session: &McpSession,
) -> Result<Option<Value>> {
    if !request.is_object() {
        return Ok(Some(serde_json::to_value(Response::failure(
            &Value::Null,
            -32600,
            "Invalid MCP request".into(),
        ))?));
    }
    let Some(id) = request.get("id") else {
        return Ok(None);
    };
    if !id.is_string() && !id.is_number() {
        return Ok(Some(serde_json::to_value(Response::failure(
            &Value::Null,
            -32600,
            "Invalid MCP request identity".into(),
        ))?));
    }
    let result = match request["method"].as_str().unwrap_or_default() {
        "ping" => Payload::Empty(Empty {}),
        "tools/list" => Payload::Tools(ToolList {
            tools: handler.definitions().await,
        }),
        "tools/call" => Payload::Call(tool_result(request, id, handler, identity, session).await?),
        _ => {
            return Ok(Some(serde_json::to_value(Response::failure(
                id,
                -32601,
                "Unsupported MCP control method".into(),
            ))?));
        }
    };
    Ok(Some(serde_json::to_value(Response::success(id, result))?))
}

/// Keep the existing success payload and expose host tool failure as MCP `isError`.
async fn tool_result(
    request: &Value,
    id: &Value,
    handler: &dyn ControlEndpoint,
    identity: uuid::Uuid,
    session: &McpSession,
) -> Result<CallResult> {
    let Some(name) = request["params"]["name"].as_str() else {
        return Ok(CallResult::failure("Control tool name missing".into()));
    };
    let reply = session
        .call(
            handler,
            ToolCall {
                id: ToolCallId::new(format!("mcp:{identity}:{id}"))?,
                name: name.into(),
                arguments: request["params"]
                    .get("arguments")
                    .cloned()
                    .unwrap_or_else(|| json!({})),
            },
        )
        .await;
    match reply {
        Ok(reply) if reply.outcome == ToolOutcome::Succeeded => {
            Ok(CallResult::text(crate::codec::to_string(&reply)?))
        }
        Ok(reply) => Ok(CallResult::failure(reply.message)),
        Err(error) => Ok(CallResult::failure(error.to_string())),
    }
}

/// Bound socket output independently of the admitted effects that produced it.
async fn write_response(
    socket: &mut (impl AsyncWriteExt + Unpin),
    response: &HttpResponse,
    stopping: &CancellationToken,
) -> Result<()> {
    let session_header = response
        .opened_session
        .map(|id| format!("Mcp-Session-Id: {id}\r\n"))
        .unwrap_or_default();
    let header = format!(
        "HTTP/1.1 {}\r\nContent-Type: application/json\r\n{session_header}Content-Length: {}\r\nConnection: close\r\n\r\n",
        response.status,
        response.body.len(),
    );
    tokio::select! {
        biased;
        _ = stopping.cancelled() => anyhow::bail!("control endpoint retired"),
        result = tokio::time::timeout(SOCKET_IO_TIMEOUT, async {
            socket.write_all(header.as_bytes()).await?;
            socket.write_all(&response.body).await
        }) => {
            result.context("control response output timed out")??;
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::ToolResult;
    use std::pin::Pin;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::task::{Context, Poll};

    struct FailingTool(AtomicUsize);

    #[async_trait::async_trait]
    impl ControlEndpoint for FailingTool {
        async fn definitions(&self) -> Vec<ControlToolDefinition> {
            Vec::new()
        }

        async fn call(&self, _: ToolCall) -> ToolResult<ControlReply> {
            self.0.fetch_add(1, Ordering::Relaxed);
            Ok(ControlReply {
                message: "refused by tool".into(),
                outcome: ToolOutcome::Failed,
            })
        }
    }

    struct StalledWriter;

    impl tokio::io::AsyncWrite for StalledWriter {
        fn poll_write(
            self: Pin<&mut Self>,
            _: &mut Context<'_>,
            _: &[u8],
        ) -> Poll<std::io::Result<usize>> {
            Poll::Pending
        }

        fn poll_flush(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<std::io::Result<()>> {
            Poll::Pending
        }

        fn poll_shutdown(self: Pin<&mut Self>, _: &mut Context<'_>) -> Poll<std::io::Result<()>> {
            Poll::Pending
        }
    }

    /// Retire an already stalled response without waiting for the socket deadline.
    #[tokio::test]
    async fn retirement_releases_stalled_output() {
        let stop = CancellationToken::new();
        let response = HttpResponse::empty("200 OK");
        let mut socket = StalledWriter;
        let mut write = Box::pin(write_response(&mut socket, &response, &stop));
        assert!(futures_util::poll!(&mut write).is_pending());
        stop.cancel();
        match futures_util::poll!(&mut write) {
            Poll::Ready(Err(error)) => assert_eq!(error.to_string(), "control endpoint retired"),
            other => panic!("socket output stayed live after retirement: {other:?}"),
        }
    }

    /// Release a live binding's socket permit when a peer stops reading.
    #[tokio::test(start_paused = true)]
    async fn live_socket_output_has_a_deadline() {
        let stop = CancellationToken::new();
        let response = HttpResponse::empty("200 OK");
        let mut socket = StalledWriter;
        let mut write = Box::pin(write_response(&mut socket, &response, &stop));
        assert!(futures_util::poll!(&mut write).is_pending());
        tokio::time::advance(SOCKET_IO_TIMEOUT).await;
        assert_eq!(
            write.await.unwrap_err().to_string(),
            "control response output timed out"
        );
    }

    /// Confirm a batch replays an exact failed tool request and deletes its session's replay state.
    #[tokio::test]
    async fn tool_replay_and_termination_follow_the_protocol_session() -> Result<()> {
        let handler = FailingTool(AtomicUsize::new(0));
        let stop = CancellationToken::new();
        let identity = uuid::Uuid::new_v4();
        let sessions =
            SessionRegistry::new(HashMap::from([(identity, Arc::new(McpSession::default()))]));
        let request = |command| HttpRequest {
            command,
            session_id: Some(identity.to_string()),
        };
        let call = json!({"jsonrpc": "2.0", "method": "tools/call", "id": 2, "params": {"name": "fixture"}});
        let mut changed = call.clone();
        changed["params"]["arguments"] = json!({"change": true});
        let batch =
            json!([call, call, changed, {"jsonrpc": "2.0", "method": "notifications/initialized"}]);
        let result = respond(
            request(HttpCommand::Post(batch)),
            &handler,
            &sessions,
            &stop,
        )
        .await?;
        let replies: Vec<Value> = serde_json::from_slice(&result.body)?;
        assert_eq!(replies.len(), 3);
        assert_eq!(replies[0], replies[1]);
        assert_eq!(replies[0]["result"]["isError"], true);
        assert_eq!(
            replies[0]["result"]["content"][0]["text"],
            "refused by tool"
        );
        assert_eq!(replies[2]["result"]["isError"], true);
        assert_eq!(handler.0.load(Ordering::Relaxed), 1);

        let deleted = respond(request(HttpCommand::Delete), &handler, &sessions, &stop).await?;
        assert_eq!(deleted.status, "200 OK");
        assert!(sessions.lock().unwrap().is_empty());
        let old = respond(request(HttpCommand::Post(call)), &handler, &sessions, &stop).await?;
        assert_eq!(old.status, "404 Not Found");
        Ok(())
    }

    /// Decode split JSON bytes and ignore framing trailers as request headers.
    #[tokio::test]
    async fn chunked_requests_preserve_their_envelope() -> Result<()> {
        let wire = b"POST /fixture HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n6\r\n{\"id\":\r\n2\r\n1}\r\n0\r\nMcp-Session-Id: trailer\r\n\r\n";
        let mut reader = BufReader::new(wire.as_slice());
        let request = read_request(&mut reader, "/fixture").await?;
        assert!(request.session_id.is_none());
        let HttpCommand::Post(payload) = request.command else {
            panic!("expected POST");
        };
        assert_eq!(payload, json!({"id": 1}));
        Ok(())
    }
}
