use std::io::Read;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result, bail, ensure};
use reqwest::{Client, RequestBuilder};
use serde_json::{Value, json};

use super::{GenerationPermit, ModelSpec};

const SYSTEM: &str = "You are a factual Conventional Commit message generator. Return only the commit message, without code fences or explanations. Use <type>: <description>, imperative mood, lowercase first letter, no final period, maximum 50 characters. Describe concrete changes, not benefits or intentions. Default to no body. Never add a body for chore commits. If essential API, behavior, migration, or test facts require a body, use at most two lines wrapped at 72 characters.";

pub async fn generate(
    client: &Client,
    model: &ModelSpec,
    prompt: &str,
    ownership: &Arc<GenerationPermit>,
) -> Result<String> {
    let response = match model.provider.as_str() {
        "gemini" => {
            let key = std::env::var("GEMINI_API_KEY").context("GEMINI_API_KEY is not set")?;
            let mut body = json!({ "contents": [{ "role": "user", "parts": [{ "text": prompt }] }], "systemInstruction": { "parts": [{ "text": SYSTEM }] } });
            if let Some(thinking) = &model.thinking {
                let field = if thinking.is_number() {
                    "thinkingBudget"
                } else {
                    "thinkingLevel"
                };
                body["generationConfig"] = json!({ "thinkingConfig": { field: thinking } });
            }
            let mut url =
                reqwest::Url::parse("https://generativelanguage.googleapis.com/v1beta/models/")?;
            url.path_segments_mut()
                .map_err(|_| anyhow::anyhow!("invalid Gemini endpoint"))?
                .pop_if_empty()
                .push(&format!("{}:generateContent", model.model));
            read_json(client.post(url).header("x-goog-api-key", key).json(&body)).await?
        }
        "copilot" => {
            let ownership = Arc::clone(ownership);
            let oauth = tokio::task::spawn_blocking(move || {
                let _retained = ownership;
                oauth_token()
            })
            .await??;
            let session = read_json(
                client
                    .get("https://api.github.com/copilot_internal/v2/token")
                    .header("authorization", format!("Token {oauth}"))
                    .header("user-agent", "Forge/1.0"),
            )
            .await?;
            let token = session["token"]
                .as_str()
                .context("Copilot token exchange returned no token")?;
            let base = session["endpoints"]["api"]
                .as_str()
                .unwrap_or("https://api.githubcopilot.com");
            let url =
                reqwest::Url::parse(&format!("{}/chat/completions", base.trim_end_matches('/')))?;
            ensure!(
                url.scheme() == "https" && url.username().is_empty() && url.password().is_none(),
                "invalid Copilot completion endpoint"
            );
            read_json(client.post(url).bearer_auth(token).header("editor-version", "Neovim/0.11").header("editor-plugin-version", "ai.nvim/1.0").header("copilot-integration-id", "vscode-chat").header("x-github-api-version", "2025-10-01").json(&json!({ "model": model.model, "messages": [{ "role": "system", "content": SYSTEM }, { "role": "user", "content": prompt }], "stream": false }))).await?
        }
        "openai" => {
            let key = std::env::var("OPENAI_API_KEY").context("OPENAI_API_KEY is not set")?;
            let mut body = json!({ "model": model.model, "instructions": SYSTEM, "input": prompt });
            if let Some(thinking) = &model.thinking {
                body["reasoning"] = json!({ "effort": thinking });
            }
            read_json(
                client
                    .post("https://api.openai.com/v1/responses")
                    .bearer_auth(key)
                    .json(&body),
            )
            .await?
        }
        provider => bail!("unsupported detached generation provider {provider}"),
    };
    extract(&model.provider, &response)
}

async fn read_json(request: RequestBuilder) -> Result<Value> {
    let mut response = request
        .send()
        .await
        .context("detached model request failed")?;
    let status = response.status();
    let mut bytes = Vec::new();
    while let Some(chunk) = response.chunk().await? {
        ensure!(
            bytes.len() + chunk.len() <= 256 * 1024,
            "model response exceeds 256 KiB"
        );
        bytes.extend_from_slice(&chunk);
    }
    if !status.is_success() {
        bail!(
            "model request failed with HTTP {status}: {}",
            String::from_utf8_lossy(&bytes[..bytes.len().min(2048)])
        );
    }
    serde_json::from_slice(&bytes).context("model returned invalid JSON")
}

fn extract(provider: &str, response: &Value) -> Result<String> {
    let mut text = String::new();
    let mut append = |part: &str| -> Result<()> {
        ensure!(
            text.len() + part.len() <= 16 * 1024,
            "generated commit message exceeds 16 KiB"
        );
        text.push_str(part);
        Ok(())
    };
    match provider {
        "copilot" => append(
            response["choices"][0]["message"]["content"]
                .as_str()
                .context("Copilot returned no message")?,
        )?,
        "gemini" => {
            for part in response["candidates"][0]["content"]["parts"]
                .as_array()
                .context("Gemini returned no content")?
            {
                if part["thought"] != true {
                    if let Some(part) = part["text"].as_str() {
                        append(part)?;
                    }
                }
            }
        }
        "openai" => {
            for item in response["output"]
                .as_array()
                .context("OpenAI returned no output")?
            {
                if item["type"] == "message" {
                    for part in item["content"].as_array().into_iter().flatten() {
                        if part["type"] == "output_text" {
                            if let Some(part) = part["text"].as_str() {
                                append(part)?;
                            }
                        }
                    }
                }
            }
        }
        _ => bail!("unknown provider response"),
    }
    let mut text = text.trim();
    if text.starts_with("```") {
        text = text.split_once('\n').map(|(_, body)| body).unwrap_or(text);
        text = text.strip_suffix("```").unwrap_or(text).trim();
    }
    ensure!(!text.is_empty(), "model returned an empty commit message");
    Ok(text.to_owned())
}

fn oauth_token() -> Result<String> {
    let mut directory = Vec::new();
    for name in ["XDG_CONFIG_HOME", "LOCALAPPDATA"] {
        if let Some(value) = std::env::var_os(name).filter(|value| !value.is_empty()) {
            directory.push(PathBuf::from(value));
        }
    }
    if let Some(home) = std::env::var_os("HOME").filter(|value| !value.is_empty()) {
        directory.push(PathBuf::from(home).join(".config"));
    }
    for directory in directory {
        for name in ["hosts.json", "apps.json"] {
            let path = directory.join("github-copilot").join(name);
            let Ok(file) = std::fs::File::open(path) else {
                continue;
            };
            let mut bytes = Vec::new();
            file.take(256 * 1024 + 1).read_to_end(&mut bytes)?;
            ensure!(
                bytes.len() <= 256 * 1024,
                "Copilot credential file exceeds bound"
            );
            let Ok(Value::Object(record)) = serde_json::from_slice(&bytes) else {
                continue;
            };
            for (host, record) in record {
                if host.contains("github.com") {
                    if let Some(token) = record["oauth_token"].as_str() {
                        ensure!(
                            token.len() <= 16 * 1024,
                            "Copilot OAuth token exceeds bound"
                        );
                        return Ok(token.to_owned());
                    }
                }
            }
        }
    }
    bail!("no Copilot OAuth token found. Sign in with :Copilot auth first")
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn extracts_provider_text_without_thinking_or_tool_output() {
        assert_eq!(extract("gemini", &json!({"candidates":[{"content":{"parts":[{"thought":true,"text":"private reasoning"},{"text":"fix: preserve edits"}]}}]})).unwrap(), "fix: preserve edits");
        assert_eq!(
            extract(
                "copilot",
                &json!({"choices":[{"message":{"content":"```text\nfix: preserve edits\n```"}}]})
            )
            .unwrap(),
            "fix: preserve edits"
        );
        assert_eq!(extract("openai", &json!({"output":[{"type":"reasoning"},{"type":"message","content":[{"type":"output_text","text":"fix: preserve edits"}]}]})).unwrap(), "fix: preserve edits");
        assert!(
            extract(
                "copilot",
                &json!({"choices":[{"message":{"content":"x".repeat(16385)}}]})
            )
            .is_err()
        );
    }
}
