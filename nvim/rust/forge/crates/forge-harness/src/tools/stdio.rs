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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn typed_responses_preserve_framing_ids_and_embedded_json() -> Result<()> {
        let requests = [
            json!({"id":"init","method":"initialize"}),
            json!({"method":"notifications/initialized"}),
            json!({"id":2,"method":"tools/list"}),
            json!({"id":null,"method":"tools/call","params":{"name":"inspect","arguments":{"text":"line\n\"quote\" λ"}}}),
            json!({"id":4,"method":"unknown"}),
        ];
        let mut input = Vec::new();
        for request in requests {
            serde_json::to_writer(&mut input, &request)?;
            input.push(b'\n');
        }
        let mut output = Vec::new();
        serve(BufReader::with_capacity(3, input.as_slice()), &mut output).await?;
        let text = String::from_utf8(output)?;
        let responses = text
            .lines()
            .map(serde_json::from_str::<Value>)
            .collect::<serde_json::Result<Vec<_>>>()?;
        assert_eq!(responses.len(), 4);
        assert_eq!(responses[0]["id"], "init");
        assert_eq!(responses[0]["result"]["capabilities"], json!({"tools":{}}));
        assert_eq!(responses[1]["id"], 2);
        assert!(
            responses[1]["result"]["tools"][0]
                .get("inputSchema")
                .is_some()
        );
        assert!(
            responses[1]["result"]["tools"][0]
                .get("input_schema")
                .is_none()
        );
        assert!(responses[2]["id"].is_null());
        let result = &responses[2]["result"];
        let embedded: Value = serde_json::from_str(result["content"][0]["text"].as_str().unwrap())?;
        assert_eq!(embedded, result["structuredContent"]);
        assert_eq!(embedded["arguments"]["text"], "line\n\"quote\" λ");
        assert!(result.get("isError").is_none());
        assert_eq!(responses[3], json!({"jsonrpc":"2.0","id":4,"result":{}}));
        Ok(())
    }
}
