use super::{
    AdapterContext, AdapterError, BuildAdapter, CheckAdapter, DiscoverRequest, Discovery,
    FormatAdapter, LintAdapter, PathSelection, ProcessSpec, RunExit, TestAdapter, TestSelection,
    TypeCheckAdapter,
    events::{
        DiagnosticEvent, DiagnosticSink, FormatEvent, FormatSink, JsonLines, TestEvent,
        TestEventSink,
    },
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::path::Path;
use tokio::io::AsyncReadExt;

const DISCOVERY_LIMIT: u64 = 32 * 1024 * 1024;

/// Select the script protocol operation without exposing wire strings to callers.
#[derive(Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
enum ScriptOperation {
    Run,
    Typecheck,
    Lint,
    LintFix,
    Build,
    FormatCheck,
    FormatApply,
}

/// Extract command-wide output before the validating sink consumes an event.
trait StreamEvent: serde::de::DeserializeOwned + Send {
    fn output(&self) -> Option<&str>;
}

impl StreamEvent for TestEvent {
    /// Keep case output separate while preserving command-wide messages in the log.
    fn output(&self) -> Option<&str> {
        match self {
            Self::Output { id: None, text } => Some(text),
            Self::Error { message } => Some(message),
            Self::Output { id: Some(_), .. }
            | Self::Phase { .. }
            | Self::Plan { .. }
            | Self::Started { .. }
            | Self::Finished { .. }
            | Self::Complete
            | Self::Unsupported
            | Self::Unknown => None,
        }
    }
}

impl StreamEvent for DiagnosticEvent {
    /// Preserve tool-rendered diagnostic text and explicit output in the command log.
    fn output(&self) -> Option<&str> {
        match self {
            Self::Output { text } => Some(text),
            Self::Error { message } => Some(message),
            Self::Diagnostic(diagnostic) => diagnostic.rendered.as_deref(),
            Self::Phase { .. }
            | Self::Plan { .. }
            | Self::TargetStarted { .. }
            | Self::TargetFinished { .. }
            | Self::Artifact { .. }
            | Self::Complete
            | Self::Unsupported
            | Self::Unknown => None,
        }
    }
}

impl StreamEvent for FormatEvent {
    /// Preserve command output without duplicating retained per-file diffs.
    fn output(&self) -> Option<&str> {
        match self {
            Self::Output { text } => Some(text),
            Self::Error { message } => Some(message),
            Self::Phase { .. }
            | Self::Plan { .. }
            | Self::File { .. }
            | Self::Complete
            | Self::Unsupported
            | Self::Unknown => None,
        }
    }
}

/// Configure the fixed command implementing the repository script protocol.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ScriptAdapterConfig {
    /// Executable followed by fixed arguments, without shell interpolation.
    pub command: Vec<String>,
}

/// Run repository-owned implementations through the common protocol.
pub struct ScriptAdapter {
    config: ScriptAdapterConfig,
}

impl ScriptAdapter {
    /// Validate a fixed argv before permitting execution or catalog publication.
    pub fn new(config: ScriptAdapterConfig) -> Result<Self, AdapterError> {
        super::contract::validate_command("script command", &config.command)?;
        Ok(Self { config })
    }

    /// Run a category stream and drain final records after process termination.
    async fn stream<Selection, Event, Accept>(
        &self,
        context: &AdapterContext<'_>,
        operation: ScriptOperation,
        selection: &Selection,
        accept: &mut Accept,
    ) -> Result<RunExit, AdapterError>
    where
        Selection: Serialize + Sync,
        Event: StreamEvent,
        Accept: FnMut(Event) -> Result<(), AdapterError> + Send,
    {
        tokio::fs::create_dir_all(context.scratch)
            .await
            .map_err(AdapterError::Storage)?;
        let path = context.scratch.join("events.jsonl");
        let mut file = tokio::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&path)
            .await
            .map_err(AdapterError::Storage)?;
        let request = RunRequest {
            version: 1,
            op: operation,
            workspace: context.workspace,
            directory: directory(context)?,
            select: selection,
            events: &path,
        };
        let request = serde_json::to_vec(&request)
            .map_err(|error| AdapterError::Protocol(error.to_string()))?;
        let mut process = context
            .processes
            .spawn(ProcessSpec {
                program: &self.config.command[0],
                args: &self.config.command[1..],
                directory: context.workspace,
                env: &[],
                stdin: Some(request),
                log_stdout: true,
            })
            .await?;
        let mut lines = JsonLines::new();
        let mut offset = 0;
        let mut tick = tokio::time::interval(std::time::Duration::from_millis(100));
        let exit = loop {
            tokio::select! {
                result = process.wait() => break result,
                _ = tick.tick() => {
                    if let Err(error) = tail(&mut file, &mut offset, &mut lines, context.processes, accept).await {
                        process.cancel();
                        let _ = process.wait().await;
                        return Err(error);
                    }
                }
            }
        };
        // Preserve complete records written immediately before exit, including after cancellation.
        let drained = tail(
            &mut file,
            &mut offset,
            &mut lines,
            context.processes,
            accept,
        )
        .await;
        match exit {
            Err(AdapterError::Cancelled) => Err(AdapterError::Cancelled),
            Err(AdapterError::Timeout) => Err(AdapterError::Timeout),
            result => {
                drained?;
                lines.finish()?;
                result
            }
        }
    }

    /// Normalize diagnostic path selections before writing the operation request.
    async fn diagnostic_stream(
        &self,
        context: &AdapterContext<'_>,
        operation: ScriptOperation,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        let mut selection = selection.clone();
        selection.normalize()?;
        self.stream(context, operation, &selection, &mut |event| {
            events.accept(event)
        })
        .await
    }
}

impl CheckAdapter for ScriptAdapter {
    /// Get the script adapter's configuration key.
    fn kind(&self) -> &'static str {
        "script"
    }

    /// Include fixed argv in the check's evidence identity.
    fn evidence_identity(&self) -> serde_json::Value {
        serde_json::json!({ "command": self.config.command })
    }
}

#[async_trait::async_trait]
impl TestAdapter for ScriptAdapter {
    /// Run discovery under the requested restrictions and validate the bounded result.
    async fn discover(
        &self,
        context: &AdapterContext<'_>,
        request: &DiscoverRequest,
    ) -> Result<Discovery, AdapterError> {
        for path in &request.files {
            super::contract::validate_path(path)?;
        }
        tokio::fs::create_dir_all(context.scratch)
            .await
            .map_err(AdapterError::Storage)?;
        let result = context.scratch.join("discover.json");
        if tokio::fs::try_exists(&result)
            .await
            .map_err(AdapterError::Storage)?
        {
            return Err(AdapterError::Protocol(
                "discovery scratch file already exists".into(),
            ));
        }
        let input = DiscoveryRequest {
            version: 1,
            op: "discover",
            workspace: context.workspace,
            directory: directory(context)?,
            within: &request.within,
            files: &request.files,
            mode: request.mode,
            result: &result,
        };
        let stdin = serde_json::to_vec(&input)
            .map_err(|error| AdapterError::Protocol(error.to_string()))?;
        let mut process = context
            .processes
            .spawn(ProcessSpec {
                program: &self.config.command[0],
                args: &self.config.command[1..],
                directory: context.workspace,
                env: &[],
                stdin: Some(stdin),
                log_stdout: true,
            })
            .await?;
        let exit = process.wait().await?;
        if !exit.success {
            return Err(AdapterError::Protocol(format!(
                "discovery command failed with exit {:?}",
                exit.code
            )));
        }
        let mut file = tokio::fs::File::open(&result)
            .await
            .map_err(AdapterError::Storage)?
            .take(DISCOVERY_LIMIT + 1);
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .await
            .map_err(AdapterError::Storage)?;
        if u64::try_from(bytes.len()).expect("buffer length fits u64") > DISCOVERY_LIMIT {
            return Err(AdapterError::Protocol("discovery exceeds 32 MiB".into()));
        }
        decode_discovery(&bytes)?.scoped(request)
    }

    /// Deliver script-produced test events through the validating sink.
    async fn run(
        &self,
        context: &AdapterContext<'_>,
        selection: &TestSelection,
        events: &mut TestEventSink,
    ) -> Result<RunExit, AdapterError> {
        let mut selection = selection.clone();
        selection.normalize()?;
        self.stream(context, ScriptOperation::Run, &selection, &mut |event| {
            events.accept(event)
        })
        .await
    }
}

#[async_trait::async_trait]
impl TypeCheckAdapter for ScriptAdapter {
    /// Deliver typecheck diagnostics through the common owned stream transport.
    async fn typecheck(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        self.diagnostic_stream(context, ScriptOperation::Typecheck, selection, events)
            .await
    }
}

#[async_trait::async_trait]
impl LintAdapter for ScriptAdapter {
    /// Deliver lint diagnostics without requesting source mutation.
    async fn lint(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        self.diagnostic_stream(context, ScriptOperation::Lint, selection, events)
            .await
    }

    /// Request the script's lint fixes and collect remaining diagnostics.
    async fn fix(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        self.diagnostic_stream(context, ScriptOperation::LintFix, selection, events)
            .await
    }
}

#[async_trait::async_trait]
impl BuildAdapter for ScriptAdapter {
    /// Deliver build diagnostics and artifacts through the common owned stream transport.
    async fn build(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        self.diagnostic_stream(context, ScriptOperation::Build, selection, events)
            .await
    }
}

#[async_trait::async_trait]
impl FormatAdapter for ScriptAdapter {
    /// Stream read-only per-file formatting outcomes through the owned launcher.
    async fn check(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut FormatSink,
    ) -> Result<RunExit, AdapterError> {
        let mut selection = selection.clone();
        selection.normalize()?;
        self.stream(
            context,
            ScriptOperation::FormatCheck,
            &selection,
            &mut |event| events.accept(event),
        )
        .await
    }

    /// Stream formatter mutations after the caller has admitted source-changing work.
    async fn apply(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut FormatSink,
    ) -> Result<RunExit, AdapterError> {
        let mut selection = selection.clone();
        selection.normalize()?;
        self.stream(
            context,
            ScriptOperation::FormatApply,
            &selection,
            &mut |event| events.accept(event),
        )
        .await
    }
}

#[derive(Serialize)]
struct RunRequest<'a, Selection> {
    version: u8,
    op: ScriptOperation,
    workspace: &'a Path,
    directory: String,
    select: &'a Selection,
    events: &'a Path,
}

#[derive(Serialize)]
struct DiscoveryRequest<'a> {
    version: u8,
    op: &'static str,
    workspace: &'a Path,
    directory: String,
    within: &'a [super::TestItemId],
    files: &'a [String],
    mode: super::DiscoverMode,
    result: &'a Path,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum DiscoveryReply {
    Items(Discovery),
    Unsupported { unsupported: bool },
}

/// Express the validated working directory relative to the repository root.
fn directory(context: &AdapterContext<'_>) -> Result<String, AdapterError> {
    let path = context
        .directory
        .strip_prefix(context.workspace)
        .map(|path| {
            if path.as_os_str().is_empty() {
                ".".to_owned()
            } else {
                path.to_string_lossy().replace('\\', "/")
            }
        })
        .map_err(|_| AdapterError::Protocol("adapter directory is outside the workspace".into()))?;
    super::contract::validate_path(&path)?;
    Ok(path)
}

/// Read every currently complete event record from an append-only file.
async fn tail<Event, Accept>(
    file: &mut tokio::fs::File,
    offset: &mut u64,
    lines: &mut JsonLines,
    processes: &super::AdapterProcesses,
    accept: &mut Accept,
) -> Result<(), AdapterError>
where
    Event: StreamEvent,
    Accept: FnMut(Event) -> Result<(), AdapterError>,
{
    if file.metadata().await.map_err(AdapterError::Storage)?.len() < *offset {
        return Err(AdapterError::Protocol(
            "adapter truncated its event file".into(),
        ));
    }
    let mut bytes = [0; 8192];
    loop {
        let count = file.read(&mut bytes).await.map_err(AdapterError::Storage)?;
        if count == 0 {
            return Ok(());
        }
        *offset += u64::try_from(count).expect("read size fits u64");
        let mut output = String::new();
        let accepted = lines.push::<Event>(&bytes[..count], |event| {
            if let Some(text) = event.output() {
                output.push_str(text);
                if !text.ends_with('\n') {
                    output.push('\n');
                }
            }
            accept(event)
        });
        if !output.is_empty() {
            processes.log_append(&output).await?;
        }
        accepted?;
    }
}

/// Decode a discovery response while keeping unsupported distinct from a malformed tree.
fn decode_discovery(bytes: &[u8]) -> Result<Discovery, AdapterError> {
    let reply: DiscoveryReply = serde_json::from_slice(bytes)
        .map_err(|error| AdapterError::Protocol(format!("invalid discovery result: {error}")))?;
    match reply {
        DiscoveryReply::Items(items) => Ok(items),
        DiscoveryReply::Unsupported { unsupported: true } => {
            Err(AdapterError::Unsupported("test discovery".into()))
        }
        DiscoveryReply::Unsupported { unsupported: false } => Err(AdapterError::Protocol(
            "unsupported discovery response must be true".into(),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn discovery_replies_distinguish_unsupported_and_invalid_trees() {
        assert!(matches!(
            decode_discovery(br#"{"unsupported":true}"#),
            Err(AdapterError::Unsupported(_))
        ));
        for reply in [
            br#"{"unsupported":false}"#.as_slice(),
            br#"{"mode":"static","items":[{"id":"a","label":"a","parent":"missing"}]}"#,
            br#"{}"#,
        ] {
            assert!(matches!(
                decode_discovery(reply),
                Err(AdapterError::Protocol(_))
            ));
        }
        assert!(decode_discovery(br#"{"mode":"exact","items":[]}"#).is_ok());
    }

    #[test]
    fn command_validation_preserves_fixed_argument_boundaries() {
        for command in [vec![], vec!["".into()], vec!["nu".into(), "a\0b".into()]] {
            assert!(ScriptAdapter::new(ScriptAdapterConfig { command }).is_err());
        }
        let adapter = ScriptAdapter::new(ScriptAdapterConfig {
            command: vec!["nu".into(), "path with spaces.nu".into()],
        })
        .unwrap();
        assert_eq!(
            adapter.evidence_identity()["command"][1],
            "path with spaces.nu"
        );
    }
}
