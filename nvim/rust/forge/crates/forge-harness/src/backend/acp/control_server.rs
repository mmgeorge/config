use super::transport::AcpHandler;
use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};

/// Owns the loopback control endpoint for one Harness root session.
pub(super) struct ControlServer {
    /// Retains the unguessable session-scoped endpoint used only by its provider.
    pub(super) url: String,
    driver: tokio::task::JoinHandle<()>,
}

impl ControlServer {
    /// Bind before provider startup so required Harness tools cannot race session creation.
    pub(super) async fn start(handler: Arc<dyn AcpHandler>) -> Result<Self> {
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
    fn drop(&mut self) { self.driver.abort(); }
}

async fn serve(socket: tokio::net::TcpStream, path: &str, handler: Arc<dyn AcpHandler>) -> Result<()> {
    let (mut reader, request) = tokio::time::timeout(std::time::Duration::from_secs(10), read_request(socket, path))
        .await.context("control request input exceeded 10 seconds")??;
    let Some(request) = request else { return Ok(()) };
    respond(&mut reader, request, handler).await
}

async fn read_request(socket: tokio::net::TcpStream, path: &str) -> Result<(BufReader<tokio::net::TcpStream>, Option<Value>)> {
    let mut reader = BufReader::new(socket);
    let mut line = read_header_line(&mut reader, 8192).await?;
    ensure!(line.split_whitespace().nth(1) == Some(path), "control endpoint mismatch");
    let method = line.split_whitespace().next().unwrap_or_default().to_owned();
    let mut length = None;
    let mut header_bytes = line.len();
    loop {
        line = read_header_line(&mut reader, 8192 - header_bytes).await?;
        let bytes = line.len();
        ensure!(bytes > 0, "control request ended before headers");
        header_bytes += bytes;
        ensure!(header_bytes <= 8192, "control headers exceed 8192 bytes");
        if line == "\r\n" || line == "\n" { break; }
        if let Some((name, value)) = line.split_once(':')
            && name.eq_ignore_ascii_case("content-length") { length = Some(value.trim().parse::<usize>()?); }
    }
    if method != "POST" {
        reader.get_mut().write_all(b"HTTP/1.1 405 Method Not Allowed\r\nContent-Length: 0\r\nConnection: close\r\n\r\n").await?;
        return Ok((reader, None));
    }
    let length = length.context("control request requires content length")?;
    ensure!(length <= 1024 * 1024, "control payload exceeds 1 MiB");
    let mut body = vec![0; length];
    reader.read_exact(&mut body).await?;
    let request: Value = serde_json::from_slice(&body)?;
    Ok((reader, Some(request)))
}

async fn read_header_line(reader: &mut BufReader<tokio::net::TcpStream>, remaining: usize) -> Result<String> {
    let mut bytes = Vec::new();
    (&mut *reader).take((remaining + 1) as u64).read_until(b'\n', &mut bytes).await?;
    ensure!(bytes.len() <= remaining, "control headers exceed 8192 bytes");
    Ok(String::from_utf8(bytes)?)
}

async fn respond(reader: &mut BufReader<tokio::net::TcpStream>, request: Value, handler: Arc<dyn AcpHandler>) -> Result<()> {
    let Some(id) = request.get("id") else {
        reader.get_mut().write_all(b"HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n").await?;
        return Ok(());
    };
    let result = match request["method"].as_str().unwrap_or_default() {
        "initialize" => Ok(json!({"protocolVersion":"2025-03-26", "capabilities":{"tools":{}},
            "serverInfo":{"name":"forge-harness-control","version":"1"}})),
        "ping" => Ok(json!({})),
        "tools/list" => handler.request("forge/tools",json!({})).await,
        "tools/call" => {
            match handler.request("forge/control", request["params"].clone()).await {
                Ok(value) => Ok(json!({"content":[{"type":"text","text":serde_json::to_string(&value)?}]})),
                Err(error) => Ok(json!({"isError":true,"content":[{"type":"text","text":format!("{error:#}")}]})),
            }
        }
        _ => Err(anyhow::anyhow!("unsupported MCP control method")),
    };
    let response = match result {
        Ok(result) => json!({"jsonrpc":"2.0","id":id,"result":result}),
        Err(error) => json!({"jsonrpc":"2.0","id":id,"error":{"code":-32601,"message":error.to_string()}}),
    };
    let body = serde_json::to_vec(&response)?;
    let header = format!("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n", body.len());
    reader.get_mut().write_all(header.as_bytes()).await?;
    reader.get_mut().write_all(&body).await?;
    Ok(())
}
