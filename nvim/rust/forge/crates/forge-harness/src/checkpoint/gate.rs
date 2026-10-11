use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::path::Path;
use std::sync::Arc;

#[derive(Clone)]
/// Shares a checkpoint barrier with provider tools and native hook processes.
pub struct ToolGate {
    state: tokio::sync::watch::Sender<Option<std::result::Result<(), String>>>,
    directory: Arc<tempfile::TempDir>,
}

impl ToolGate {
    /// Creates a closed barrier. Dropping its owner before release denies waiting tools.
    pub(crate) fn new() -> Result<Self> {
        let directory = Arc::new(tempfile::Builder::new().prefix("forge-checkpoint-").tempdir()?);
        std::fs::write(directory.path().join("state"), "pending")?;
        Ok(Self { state: tokio::sync::watch::channel(None).0, directory })
    }

    /// Releases tools only after the broker durably stores the baseline.
    pub(crate) fn complete(&self, result: std::result::Result<(), String>) -> Result<()> {
        if self.state.borrow().is_some() { return Ok(()); }
        let content = match &result { Ok(()) => "ready".to_owned(), Err(error) => format!("failed:{error}") };
        let mut update = tempfile::NamedTempFile::new_in(self.directory.path())?;
        std::io::Write::write_all(&mut update, content.as_bytes())?;
        update.persist(self.directory.path().join("state")).map_err(|error| error.error)?;
        self.state.send_replace(Some(result));
        Ok(())
    }

    /// Reports whether baseline persistence already released the barrier.
    pub(crate) fn ready(&self) -> bool { matches!(self.state.borrow().as_ref(), Some(Ok(()))) }

    /// Waits for durable baseline persistence and propagates checkpoint failure.
    pub async fn wait(&self) -> Result<()> {
        let mut state = self.state.subscribe();
        tokio::time::timeout(std::time::Duration::from_secs(25), async {
            loop {
                if let Some(result) = state.borrow_and_update().clone() {
                    return result.map_err(anyhow::Error::msg);
                }
                state.changed().await.context("checkpoint owner disappeared")?;
            }
        }).await.context("checkpoint barrier exceeded 25 seconds")?
    }

    /// Builds a native PreToolUse hook without granting or changing tool permissions.
    pub(crate) fn hook(&self) -> Result<Value> {
        let executable = std::env::current_exe()?;
        let command = format!("{} checkpoint-wait {}", quote(&executable)?, quote(&self.directory.path().join("state"))?);
        Ok(json!({"matcher":".*", "hooks":[{"type":"command", "command":command, "timeout":30}]}))
    }
}

fn quote(path: &Path) -> Result<String> {
    let path = path.to_str().context("checkpoint hook path is not Unicode")?;
    #[cfg(windows)]
    { Ok(format!("\"{}\"", path.replace('\\', "/"))) }
    #[cfg(not(windows))]
    { shlex::try_quote(path).map(|value|value.into_owned()).context("invalid checkpoint hook path") }
}

/// Drains the native hook input and returns a supported deny response on failure.
pub async fn wait_hook(path: &Path, input: &mut impl std::io::Read) -> Value {
    let result: Result<()> = tokio::time::timeout(std::time::Duration::from_secs(25), async {
        std::io::copy(input, &mut std::io::sink()).context("read checkpoint hook input")?;
        loop {
            let state = std::fs::read_to_string(path).context("checkpoint barrier is unavailable")?;
            match state.as_str() {
                "ready" => return Ok(()),
                "pending" => tokio::time::sleep(std::time::Duration::from_millis(5)).await,
                failure => anyhow::bail!("{failure}"),
            }
        }
    }).await.context("checkpoint barrier exceeded 25 seconds").and_then(|result|result);
    match result {
        Ok(()) => json!({}),
        Err(error) => json!({"hookSpecificOutput":{"hookEventName":"PreToolUse",
            "permissionDecision":"deny", "permissionDecisionReason":format!("{error:#}")}}),
    }
}

/// Retains and cancels asynchronous baseline acquisition with its exchange.
pub(crate) struct CheckpointTask(pub(crate) tokio::task::JoinHandle<Result<super::CheckpointRecord>>);

impl Drop for CheckpointTask {
    fn drop(&mut self) { self.0.abort(); }
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test]
    async fn tools_wait_for_persistence_and_failure_remains_denied() {
        let gate = ToolGate::new().unwrap();
        let path = gate.directory.path().join("state");
        let mut input = std::io::empty();
        let mut native = Box::pin(wait_hook(&path, &mut input));
        assert!(futures_util::poll!(&mut native).is_pending());
        let mut local = Box::pin(gate.wait());
        assert!(futures_util::poll!(&mut local).is_pending());
        gate.complete(Err("capture failed".into())).unwrap();
        gate.complete(Ok(())).unwrap();
        assert!(local.await.unwrap_err().to_string().contains("capture failed"));
        assert_eq!(native.await["hookSpecificOutput"]["permissionDecision"], "deny");
        let released = ToolGate::new().unwrap();
        released.complete(Ok(())).unwrap();
        released.wait().await.unwrap();
        assert_eq!(wait_hook(&released.directory.path().join("state"), &mut std::io::empty()).await, json!({}));
    }
}
