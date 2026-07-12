use crate::backend::process;
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::{ExitStatus, Stdio};
use std::sync::Arc;
use tokio::io::{AsyncRead, AsyncReadExt};
use tokio::process::Child;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use uuid::Uuid;

const DEFAULT_OUTPUT_BYTE_LIMIT: usize = 1_000_000;

/// Stores retained output for one ACP terminal command.
struct TerminalOutput {
    text: String,
    truncated: bool,
    byte_limit: usize,
}

impl TerminalOutput {
    fn append(&mut self, chunk: &[u8]) {
        self.text.push_str(&String::from_utf8_lossy(chunk));
        if self.text.len() <= self.byte_limit {
            return;
        }
        let mut remove_count = self.text.len() - self.byte_limit;
        while remove_count < self.text.len() && !self.text.is_char_boundary(remove_count) {
            remove_count += 1;
        }
        self.text.drain(..remove_count);
        self.truncated = true;
    }
}

/// Owns one command process and its asynchronous output readers.
struct TerminalProcess {
    child: Child,
    output: Arc<Mutex<TerminalOutput>>,
    reader: Vec<JoinHandle<()>>,
    exit_status: Option<ExitStatus>,
}

impl TerminalProcess {
    async fn refresh_exit(&mut self) -> Result<()> {
        if self.exit_status.is_none()
            && let Some(status) = self.child.try_wait().context("poll ACP terminal")?
        {
            self.exit_status = Some(status);
            self.finish_reader().await;
        }
        Ok(())
    }

    async fn wait(&mut self) -> Result<ExitStatus> {
        if let Some(status) = self.exit_status {
            return Ok(status);
        }
        let status = self.child.wait().await.context("wait for ACP terminal")?;
        self.exit_status = Some(status);
        self.finish_reader().await;
        Ok(status)
    }

    async fn finish_reader(&mut self) {
        for reader in self.reader.drain(..) {
            let _ = reader.await;
        }
    }
}

/// Owns ACP terminal processes for the lifetime of one provider connection.
#[derive(Default)]
pub(crate) struct TerminalStore {
    process: HashMap<String, TerminalProcess>,
}

impl TerminalStore {
    /// Start one ACP terminal command with retained stdout and stderr.
    pub(crate) async fn create(&mut self, params: &Value, cwd: PathBuf) -> Result<Value> {
        let program = required_text(params, "command")?;
        let argument = params
            .get("args")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(Value::as_str)
            .map(str::to_owned)
            .collect::<Vec<_>>();
        let mut command = process::command(&program, &argument);
        command
            .current_dir(cwd)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true);
        for variable in params
            .get("env")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
        {
            let Some(name) = variable.get("name").and_then(Value::as_str) else {
                continue;
            };
            let value = variable
                .get("value")
                .and_then(Value::as_str)
                .unwrap_or_default();
            command.env(name, value);
        }
        let mut child = command
            .spawn()
            .with_context(|| format!("start ACP terminal command {program}"))?;
        let stdout = child
            .stdout
            .take()
            .context("ACP terminal stdout was not piped")?;
        let stderr = child
            .stderr
            .take()
            .context("ACP terminal stderr was not piped")?;
        let byte_limit = params
            .get("outputByteLimit")
            .and_then(Value::as_u64)
            .and_then(|value| usize::try_from(value).ok())
            .unwrap_or(DEFAULT_OUTPUT_BYTE_LIMIT)
            .max(1);
        let output = Arc::new(Mutex::new(TerminalOutput {
            text: String::new(),
            truncated: false,
            byte_limit,
        }));
        let reader = vec![
            tokio::spawn(capture_output(stdout, output.clone())),
            tokio::spawn(capture_output(stderr, output.clone())),
        ];
        let terminal_id = Uuid::new_v4().to_string();
        self.process.insert(
            terminal_id.clone(),
            TerminalProcess {
                child,
                output,
                reader,
                exit_status: None,
            },
        );
        Ok(json!({ "terminalId": terminal_id }))
    }

    /// Read retained output and current exit status for one ACP terminal.
    pub(crate) async fn output(&mut self, params: &Value) -> Result<Value> {
        let terminal = self.terminal_mut(params)?;
        terminal.refresh_exit().await?;
        let output = terminal.output.lock().await;
        Ok(json!({
            "output": output.text,
            "truncated": output.truncated,
            "exitStatus": terminal.exit_status.map(exit_value)
        }))
    }

    /// Wait for one ACP terminal command and return its exit status.
    pub(crate) async fn wait(&mut self, params: &Value) -> Result<Value> {
        let status = self.terminal_mut(params)?.wait().await?;
        Ok(exit_value(status))
    }

    /// Terminate one ACP terminal without releasing its retained output.
    pub(crate) async fn kill(&mut self, params: &Value) -> Result<Value> {
        let terminal = self.terminal_mut(params)?;
        if terminal.exit_status.is_none() {
            terminal.child.kill().await.context("kill ACP terminal")?;
        }
        Ok(json!({}))
    }

    /// Release one ACP terminal and terminate it when it still runs.
    pub(crate) async fn release(&mut self, params: &Value) -> Result<Value> {
        let terminal_id = required_text(params, "terminalId")?;
        let mut terminal = self
            .process
            .remove(&terminal_id)
            .with_context(|| format!("ACP terminal not found: {terminal_id}"))?;
        terminal.refresh_exit().await?;
        if terminal.exit_status.is_none() {
            terminal
                .child
                .kill()
                .await
                .context("release ACP terminal")?;
            let _ = terminal.wait().await?;
        }
        terminal.finish_reader().await;
        Ok(json!({}))
    }

    fn terminal_mut(&mut self, params: &Value) -> Result<&mut TerminalProcess> {
        let terminal_id = required_text(params, "terminalId")?;
        self.process
            .get_mut(&terminal_id)
            .with_context(|| format!("ACP terminal not found: {terminal_id}"))
    }
}

async fn capture_output(mut reader: impl AsyncRead + Unpin, output: Arc<Mutex<TerminalOutput>>) {
    let mut buffer = [0_u8; 8192];
    loop {
        let Ok(count) = reader.read(&mut buffer).await else {
            return;
        };
        if count == 0 {
            return;
        }
        output.lock().await.append(&buffer[..count]);
    }
}

fn exit_value(status: ExitStatus) -> Value {
    json!({
        "exitCode": status.code().and_then(|code| u32::try_from(code).ok()),
        "signal": Value::Null
    })
}

fn required_text(value: &Value, field: &str) -> Result<String> {
    value
        .get(field)
        .and_then(Value::as_str)
        .map(str::to_owned)
        .with_context(|| format!("ACP terminal {field} is required"))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn truncates_output_on_a_utf8_boundary() {
        let mut output = TerminalOutput {
            text: String::new(),
            truncated: false,
            byte_limit: 3,
        };
        output.append("aébc".as_bytes());
        assert_eq!(output.text, "bc");
        assert!(output.truncated);
    }

    #[tokio::test]
    async fn runs_waits_reads_and_releases_a_terminal() {
        let workspace = tempfile::tempdir().unwrap();
        let executable = std::env::current_exe().unwrap();
        let mut store = TerminalStore::default();
        let created = store
            .create(
                &json!({
                    "command": executable,
                    "args": ["--list"],
                    "outputByteLimit": 100_000
                }),
                workspace.path().to_path_buf(),
            )
            .await
            .unwrap();
        let terminal_id = created.get("terminalId").and_then(Value::as_str).unwrap();
        let request = json!({ "terminalId": terminal_id });
        let status = store.wait(&request).await.unwrap();
        assert_eq!(status.get("exitCode").and_then(Value::as_u64), Some(0));
        let output = store.output(&request).await.unwrap();
        assert!(
            output
                .get("output")
                .and_then(Value::as_str)
                .is_some_and(|text| text.contains("truncates_output_on_a_utf8_boundary"))
        );
        store.release(&request).await.unwrap();
    }
}
