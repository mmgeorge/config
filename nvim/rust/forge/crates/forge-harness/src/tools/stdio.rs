use super::ControlToolRegistry;
use super::mcp::{CallResult, Empty, Initialization, Response, Tools};
use anyhow::{Context, Result};
use serde::Serialize;
use serde_json::Value;
use tokio::io::{AsyncBufRead, AsyncWrite, AsyncWriteExt, BufReader};

/// Preserve the method-specific payload until the complete response is encoded.
#[derive(Serialize)]
#[serde(untagged)]
enum Payload<'a> {
    Initialization(Initialization),
    Tools(Tools<'a>),
    Call(CallResult<CallEcho<'a>>),
    Empty(Empty),
}

/// Borrow the standalone endpoint's echoed arguments without cloning their JSON tree.
#[derive(Serialize)]
struct CallEcho<'a> {
    tool: &'a str,
    arguments: &'a Value,
}

/// Run the Harness control-tool MCP server over JSONL stdio.
pub async fn run_stdio() -> Result<()> {
    serve(
        BufReader::new(forge_protocol::input::ThreadInput::new(std::io::stdin())?),
        tokio::io::stdout(),
    )
    .await
}

/// Process complete MCP frames and encode each response once at the output boundary.
async fn serve(
    input: impl AsyncBufRead + Unpin,
    mut output: impl AsyncWrite + Unpin,
) -> Result<()> {
    let definitions = ControlToolRegistry.definition_list();
    let mut input = forge_protocol::frame::JsonLineReader::new(input);
    let mut encoded = Vec::new();
    while let Some(line) = input.next_frame().await? {
        let request: Value =
            crate::codec::from_slice(&line).context("decode Harness control MCP request")?;
        let id = request.get("id").unwrap_or(&Value::Null);
        let method = request
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let result = match method {
            "initialize" => Payload::Initialization(Initialization::new(env!("CARGO_PKG_VERSION"))),
            "tools/list" => Payload::Tools(Tools {
                tools: &definitions,
            }),
            "tools/call" => {
                let name = request
                    .pointer("/params/name")
                    .and_then(Value::as_str)
                    .unwrap_or_default();
                let arguments = request.pointer("/params/arguments").unwrap_or(&Value::Null);
                Payload::Call(CallResult::structured(CallEcho {
                    tool: name,
                    arguments,
                })?)
            }
            "notifications/initialized" => continue,
            _ => Payload::Empty(Empty {}),
        };
        // Publish only complete frames; a serialization failure must not leave a partial response.
        encoded.clear();
        crate::codec::to_writer(&mut encoded, &Response::success(id, result))?;
        encoded.push(b'\n');
        output.write_all(&encoded).await?;
        output.flush().await?;
    }
    Ok(())
}
