use super::ControlToolDefinition;
use super::control_session::ControlEndpoint;
#[cfg(test)]
use super::{ToolResult, control_session::ControlReply};
use crate::tools::mcp::{CallResult, Empty, Initialization, Response};
use anyhow::{Context, Result, ensure};
use forge_provider_api::control::ToolCall;
use serde::Serialize;
use serde_json::{Value, json};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};

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

/// Owns the loopback control endpoint for one Harness root session.
pub(crate) struct ControlServer {
    /// Retains the unguessable session-scoped endpoint used only by its provider.
    pub(crate) url: String,
    driver: tokio::task::JoinHandle<()>,
}

impl ControlServer {
    /// Bind before provider startup so required Harness tools cannot race session creation.
    pub(crate) async fn start(handler: Arc<dyn ControlEndpoint>) -> Result<Self> {
        let listener = tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 0)).await?;
        let path = format!("/{}", uuid::Uuid::new_v4());
        let url = format!("http://{}{}", listener.local_addr()?, path);
        let driver = tokio::spawn(async move {
            let admission = Arc::new(tokio::sync::Semaphore::new(16));
            let mut connection = tokio::task::JoinSet::new();
            loop {
                tokio::select! {
                    accepted = listener.accept() => {
                        let Ok((socket, _)) = accepted else { break };
                        let Ok(permit) = admission.clone().try_acquire_owned() else { continue };
                        let handler = handler.clone();
                        let path = path.clone();
                        connection.spawn(async move {
                            let _permit = permit;
                            let _ = serve(socket, &path, handler).await;
                        });
                    }
                    _ = connection.join_next(), if !connection.is_empty() => {}
                }
            }
        });
        Ok(Self { url, driver })
    }
}

impl Drop for ControlServer {
    fn drop(&mut self) {
        self.driver.abort();
    }
}

async fn serve(
    socket: tokio::net::TcpStream,
    path: &str,
    handler: Arc<dyn ControlEndpoint>,
) -> Result<()> {
    let (mut reader, request) = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        read_request(socket, path),
    )
    .await
    .context("control request input exceeded 10 seconds")??;
    let Some(request) = request else {
        return Ok(());
    };
    respond(&mut reader, request, handler).await
}

async fn read_request(
    socket: tokio::net::TcpStream,
    path: &str,
) -> Result<(BufReader<tokio::net::TcpStream>, Option<Value>)> {
    let mut reader = BufReader::new(socket);
    let mut line = read_header_line(&mut reader, 8192).await?;
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
    let mut header_bytes = line.len();
    loop {
        line = read_header_line(&mut reader, 8192 - header_bytes).await?;
        let bytes = line.len();
        ensure!(bytes > 0, "control request ended before headers");
        header_bytes += bytes;
        ensure!(header_bytes <= 8192, "control headers exceed 8192 bytes");
        if line == "\r\n" || line == "\n" {
            break;
        }
        if let Some((name, value)) = line.split_once(':')
            && name.eq_ignore_ascii_case("content-length")
        {
            length = Some(value.trim().parse::<usize>()?);
        }
    }
    if method != "POST" {
        reader.get_mut().write_all(b"HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n").await?;
        return Ok((reader, None));
    }
    let length = length.context("control request requires content length")?;
    let mut body = vec![0; length];
    reader.read_exact(&mut body).await?;
    let request: Value = serde_json::from_slice(&body)?;
    Ok((reader, Some(request)))
}

async fn read_header_line(
    reader: &mut BufReader<tokio::net::TcpStream>,
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

async fn respond(
    reader: &mut BufReader<tokio::net::TcpStream>,
    request: Value,
    handler: Arc<dyn ControlEndpoint>,
) -> Result<()> {
    let Some(id) = request.get("id") else {
        reader
            .get_mut()
            .write_all(b"HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
            .await?;
        return Ok(());
    };
    let result = match request["method"].as_str().unwrap_or_default() {
        "initialize" => Ok(Payload::Initialization(Initialization::new("1"))),
        "ping" => Ok(Payload::Empty(Empty {})),
        "tools/list" => Ok(Payload::Tools(ToolList {
            tools: handler.definitions().await,
        })),
        "tools/call" => {
            let outcome = match request["params"]["name"].as_str() {
                Some(name) => {
                    handler
                        .call(ToolCall {
                            name: name.into(),
                            arguments: request["params"]
                                .get("arguments")
                                .cloned()
                                .unwrap_or_else(|| json!({})),
                        })
                        .await
                }
                None => Err(super::ToolError::Rejected(
                    "control tool name missing".into(),
                )),
            };
            match outcome {
                Ok(value) => Ok(Payload::Call(CallResult::text(crate::codec::to_string(
                    &value,
                )?))),
                Err(error) => Ok(Payload::Call(CallResult::failure(format!("{error:#}")))),
            }
        }
        _ => Err(anyhow::anyhow!("unsupported MCP control method")),
    };
    let body = match result {
        Ok(result) => crate::codec::to_vec(&Response::success(id, result))?,
        Err(error) => crate::codec::to_vec(&Response::failure(id, -32601, error.to_string()))?,
    };
    let header = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
        body.len()
    );
    reader.get_mut().write_all(header.as_bytes()).await?;
    reader.get_mut().write_all(&body).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Handler;

    #[async_trait::async_trait]
    impl ControlEndpoint for Handler {
        async fn definitions(&self) -> Vec<ControlToolDefinition> {
            Vec::new()
        }
        async fn call(&self, call: ToolCall) -> ToolResult<ControlReply> {
            if call.arguments["fail"] == true {
                return Err(super::super::ToolError::Rejected(
                    "tool rejected the request".into(),
                ));
            }
            Ok(ControlReply {
                message: call.arguments["text"].as_str().unwrap_or_default().into(),
            })
        }
    }

    #[tokio::test]
    async fn typed_responses_preserve_http_lengths_and_mcp_outcomes() -> Result<()> {
        let server = ControlServer::start(Arc::new(Handler)).await?;
        let client = reqwest::Client::builder().no_proxy().build()?;
        let content = "λ\n\"".repeat(150_000);
        let requests = [
            json!({"id":"init","method":"initialize"}),
            json!({"id":1,"method":"ping"}),
            json!({"id":2,"method":"tools/list"}),
            json!({"id":3,"method":"tools/call","params":{"name":"echo","arguments":{"text":content}}}),
            json!({"id":4,"method":"tools/call","params":{"name":"echo","arguments":{"fail":true}}}),
            json!({"id":5,"method":"unknown"}),
        ];
        let mut responses = Vec::new();
        for request in requests {
            let response = client.post(&server.url).json(&request).send().await?;
            assert_eq!(response.status(), reqwest::StatusCode::OK);
            let length = response.content_length().unwrap();
            let bytes = response.bytes().await?;
            assert_eq!(length, u64::try_from(bytes.len())?);
            let response: Value = serde_json::from_slice(&bytes)?;
            assert_eq!(response["id"], request["id"]);
            assert_eq!(response["jsonrpc"], "2.0");
            responses.push(response);
        }
        assert_eq!(responses[0]["result"]["serverInfo"]["version"], "1");
        assert_eq!(responses[1]["result"], json!({}));
        assert_eq!(responses[2]["result"], json!({"tools":[]}));
        let result = &responses[3]["result"];
        let payload: Value = serde_json::from_str(result["content"][0]["text"].as_str().unwrap())?;
        assert_eq!(payload["message"], content);
        assert!(result.get("isError").is_none());
        assert!(result.get("structuredContent").is_none());
        assert_eq!(responses[4]["result"]["isError"], true);
        assert!(responses[4]["error"].is_null());
        assert_eq!(responses[5]["error"]["code"], -32601);
        assert!(responses[5].get("result").is_none());
        let response = client
            .post(&server.url)
            .json(&json!({"method":"notifications/initialized"}))
            .send()
            .await?;
        assert_eq!(response.status(), reqwest::StatusCode::ACCEPTED);
        assert!(response.bytes().await?.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn control_request_preserves_json_larger_than_one_mib() -> Result<()> {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let address = listener.local_addr()?;
        let content = "λ".repeat(1024 * 1024);
        let body =
            serde_json::to_vec(&json!({"method":"tools/call","params":{"content":content}}))?;
        let sender = tokio::spawn(async move {
            let mut socket = tokio::net::TcpStream::connect(address).await?;
            let header = format!(
                "POST /control HTTP/1.1\r\nContent-Length: {}\r\n\r\n",
                body.len()
            );
            socket.write_all(header.as_bytes()).await?;
            socket.write_all(&body).await?;
            Ok::<_, anyhow::Error>(())
        });
        let (socket, _) = listener.accept().await?;
        let (_, request) = read_request(socket, "/control").await?;
        sender.await??;
        assert_eq!(request.unwrap()["params"]["content"], content);
        Ok(())
    }
}
