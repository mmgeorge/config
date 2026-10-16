use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, AtomicU64, Ordering},
};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use tokio::sync::{Semaphore, oneshot};

use crate::permissions::{
    document::PermissionDecision,
    matcher::{CompiledPermissionDocument, PermissionRequest, PermissionTarget},
};
use crate::session::PermissionMode;
use forge_protocol::{message::SessionEvent, outbound::MessageSender};

const REPLY_BYTES: usize = 8192;
const HOVER_BYTES: usize = 4096;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
enum Operation {
    Status,
    Definition,
    TypeDefinition,
    Implementation,
    References,
    Hover,
    DocumentSymbols,
    WorkspaceSymbols,
    Calls,
    Diagnostics,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Arguments {
    operation: Operation,
    #[serde(skip_serializing_if = "Option::is_none")]
    path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    line: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    column: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    query: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    direction: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    limit: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    offset: Option<u32>,
}

impl Arguments {
    fn validate(&self) -> Result<()> {
        let position = matches!(
            self.operation,
            Operation::Definition
                | Operation::TypeDefinition
                | Operation::Implementation
                | Operation::References
                | Operation::Hover
                | Operation::Calls
        );
        ensure!(
            matches!(
                self.operation,
                Operation::Status | Operation::WorkspaceSymbols
            ) || self.path.is_some(),
            "operation requires path"
        );
        ensure!(
            !position || (self.line.is_some() && self.column.is_some()),
            "position operation requires line and column"
        );
        ensure!(
            position || (self.line.is_none() && self.column.is_none()),
            "line and column only apply to position operations"
        );
        ensure!(
            self.line.is_none_or(|value| value > 0) && self.column.is_none_or(|value| value > 0),
            "line and column are one-based"
        );
        ensure!(
            self.operation != Operation::WorkspaceSymbols || self.query.is_some(),
            "workspace_symbols requires query"
        );
        ensure!(
            self.query.is_none()
                || matches!(
                    self.operation,
                    Operation::WorkspaceSymbols | Operation::DocumentSymbols
                ),
            "query only applies to symbol operations"
        );
        ensure!(
            self.query
                .as_ref()
                .is_none_or(|value| !value.trim().is_empty() && value.len() <= 256),
            "query must contain 1-256 bytes"
        );
        ensure!(
            self.path
                .as_ref()
                .is_none_or(|value| !value.is_empty() && value.len() <= 4096),
            "path must contain 1-4096 bytes"
        );
        ensure!(
            if self.operation == Operation::Calls {
                matches!(self.direction.as_deref(), Some("incoming" | "outgoing"))
            } else {
                self.direction.is_none()
            },
            "calls requires direction incoming or outgoing; other operations reject direction"
        );
        ensure!(
            self.limit.is_none_or(|value| (1..=50).contains(&value)),
            "limit must be between 1 and 50"
        );
        ensure!(
            self.operation != Operation::Hover || (self.limit.is_none() && self.offset.is_none()),
            "hover does not accept pagination"
        );
        Ok(())
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Row {
    path: String,
    line: u32,
    column: u32,
    text: String,
    modified: Option<bool>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Reply {
    rows: Vec<Row>,
    source: String,
    next_offset: Option<u32>,
    warnings: Option<Vec<String>>,
    hover: Option<String>,
    freshness: Option<String>,
}

#[derive(Default)]
struct Selection {
    enabled: AtomicBool,
    generation: AtomicU64,
}

struct Pending {
    session: String,
    scope: Arc<LspScope>,
    result: oneshot::Sender<Result<Reply>>,
}

struct Flight {
    service: Arc<LspService>,
    session: String,
    id: String,
    sender: MessageSender,
}

impl Drop for Flight {
    fn drop(&mut self) {
        if self
            .service
            .pending
            .lock()
            .unwrap()
            .remove(&self.id)
            .is_some()
        {
            let _ = self
                .sender
                .send_control(forge_protocol::message::Message::Event(SessionEvent {
                    session_id: self.session.clone(),
                    event: "lsp.cancel".into(),
                    payload: json!({"id":self.id}),
                }));
        }
    }
}

/// Owns bounded editor requests independently of provider and broker locks.
pub(crate) struct LspService {
    sender: Mutex<Option<MessageSender>>,
    pending: Mutex<HashMap<String, Pending>>,
    selection: Mutex<HashMap<String, Arc<Selection>>>,
    slots: Semaphore,
}

impl Default for LspService {
    fn default() -> Self {
        Self {
            sender: Mutex::new(None),
            pending: Mutex::new(HashMap::new()),
            selection: Mutex::new(HashMap::new()),
            slots: Semaphore::new(4),
        }
    }
}

impl LspService {
    /// Bind the current host connection without acquiring session execution ownership.
    pub(crate) fn attach(&self, sender: MessageSender) {
        *self.sender.lock().unwrap() = Some(sender);
    }

    /// Revoke old scopes whenever the effective preference changes.
    pub(crate) fn set_enabled(&self, session: &str, enabled: bool) {
        let selection = self
            .selection
            .lock()
            .unwrap()
            .entry(session.into())
            .or_default()
            .clone();
        if selection.enabled.swap(enabled, Ordering::AcqRel) != enabled {
            selection.generation.fetch_add(1, Ordering::AcqRel);
        }
    }

    /// Return the current preference for snapshots and transport initialization.
    pub(crate) fn enabled(&self, session: &str) -> bool {
        self.selection
            .lock()
            .unwrap()
            .get(session)
            .is_some_and(|selection| selection.enabled.load(Ordering::Acquire))
    }

    /// Cancel editor work before the host connection drains.
    pub(crate) fn close(&self) {
        self.sender.lock().unwrap().take();
        self.pending.lock().unwrap().clear();
    }

    /// Create one request authority from the live interaction and its read policy.
    pub(crate) fn scope(
        self: &Arc<Self>,
        session: String,
        interaction: String,
        workspace: PathBuf,
        policy: Arc<CompiledPermissionDocument>,
        mode: PermissionMode,
        cancelled: Arc<AtomicBool>,
    ) -> (Arc<LspScope>, LspLease) {
        let selection = self
            .selection
            .lock()
            .unwrap()
            .entry(session.clone())
            .or_default()
            .clone();
        let generation = selection.generation.load(Ordering::Acquire);
        let scope = Arc::new(LspScope {
            service: self.clone(),
            session,
            interaction,
            workspace,
            policy,
            mode,
            cancelled,
            active: AtomicBool::new(true),
            selection,
            generation,
        });
        (scope.clone(), LspLease(scope))
    }

    /// Authorize target paths before the editor reads their contents.
    pub(crate) async fn authorize(
        &self,
        session: &str,
        id: &str,
        paths: Vec<String>,
    ) -> Result<Value> {
        ensure!(
            paths.len() <= 64,
            "at most 64 target paths may be authorized per batch"
        );
        let scope = {
            let pending = self.pending.lock().unwrap();
            let request = pending.get(id).context("LSP request is no longer active")?;
            ensure!(
                request.session == session,
                "LSP response belongs to another session"
            );
            request.scope.clone()
        };
        let mut allowed = serde_json::Map::new();
        for path in paths {
            allowed.insert(path.clone(), json!(scope.authorize(&path).await.is_ok()));
        }
        scope.check()?;
        Ok(Value::Object(allowed))
    }

    /// Settle exactly one matching editor response while preserving its scope validation.
    pub(crate) fn respond(
        &self,
        session: &str,
        id: &str,
        result: Value,
        failure: Option<String>,
    ) -> Result<()> {
        ensure!(
            serde_json::to_vec(&result)?.len() <= 64 * 1024,
            "LSP response exceeded transport limit"
        );
        let mut pending = self.pending.lock().unwrap();
        let request = pending.get(id).context("LSP request is no longer active")?;
        ensure!(
            request.session == session,
            "LSP response belongs to another session"
        );
        request.scope.check()?;
        let result = match failure {
            Some(failure) => Err(anyhow::anyhow!(failure)),
            None => serde_json::from_value(result).context("invalid editor LSP response"),
        };
        let request = pending.remove(id).unwrap();
        let _ = request.result.send(result);
        Ok(())
    }
}

/// Limits LSP access to one authenticated live interaction and preference generation.
pub(crate) struct LspScope {
    service: Arc<LspService>,
    session: String,
    interaction: String,
    workspace: PathBuf,
    policy: Arc<CompiledPermissionDocument>,
    mode: PermissionMode,
    cancelled: Arc<AtomicBool>,
    active: AtomicBool,
    selection: Arc<Selection>,
    generation: u64,
}

impl LspScope {
    /// Reports whether this scope can advertise and invoke editor intelligence.
    pub(crate) fn available(&self) -> bool {
        self.check().is_ok() && self.service.sender.lock().unwrap().is_some()
    }

    fn check(&self) -> Result<()> {
        ensure!(
            self.active.load(Ordering::Acquire) && !self.cancelled.load(Ordering::Acquire),
            "LSP interaction is no longer active"
        );
        ensure!(
            self.selection.enabled.load(Ordering::Acquire)
                && self.selection.generation.load(Ordering::Acquire) == self.generation,
            "LSP tools are disabled or their configuration changed"
        );
        Ok(())
    }

    async fn authorize(&self, path: &str) -> Result<PathBuf> {
        self.check()?;
        let path = Path::new(path);
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.workspace.join(path)
        };
        let absolute = match tokio::fs::canonicalize(&absolute).await {
            Ok(path) => path,
            Err(_) => tokio::fs::canonicalize(absolute.parent().context("path has no parent")?)
                .await?
                .join(absolute.file_name().context("path has no file name")?),
        };
        let path = absolute
            .to_str()
            .context("path is not UTF-8")?
            .trim_start_matches("\\\\?\\")
            .replace('\\', "/");
        let decision = self.policy.evaluate(
            self.mode,
            &PermissionRequest {
                id: self.interaction.clone(),
                provider: "harness-lsp".into(),
                reason: None,
                target_list: vec![PermissionTarget::Read { path }],
            },
        )?;
        ensure!(
            decision.decision == PermissionDecision::Allow,
            "read policy does not authorize this LSP path"
        );
        self.check()?;
        Ok(absolute)
    }

    /// Execute an allowlisted query and emit only bounded, authorized text.
    pub(crate) async fn invoke(self: &Arc<Self>, value: Value) -> Result<String> {
        self.check()?;
        let mut arguments: Arguments = serde_json::from_value(value)?;
        arguments.validate()?;
        if let Some(path) = &arguments.path {
            arguments.path = Some(
                self.authorize(path)
                    .await?
                    .to_string_lossy()
                    .trim_start_matches("\\\\?\\")
                    .replace('\\', "/"),
            );
        }
        let _slot = self
            .service
            .slots
            .try_acquire()
            .context("busy: four LSP requests are already active")?;
        let sender = self
            .service
            .sender
            .lock()
            .unwrap()
            .clone()
            .context("unavailable: no editor connection")?;
        let id = uuid::Uuid::new_v4().to_string();
        let (complete, result) = oneshot::channel();
        self.service.pending.lock().unwrap().insert(
            id.clone(),
            Pending {
                session: self.session.clone(),
                scope: self.clone(),
                result: complete,
            },
        );
        let flight = Flight {
            service: self.service.clone(),
            session: self.session.clone(),
            id: id.clone(),
            sender: sender.clone(),
        };
        let event = SessionEvent {
            session_id: self.session.clone(),
            event: "lsp.request".into(),
            payload: json!({"id":id,"workspace":self.workspace,"arguments":arguments}),
        };
        let received = tokio::time::timeout(Duration::from_secs(75), async {
            sender.send_event(event).await?;
            tokio::pin!(result);
            let mut tick = tokio::time::interval(Duration::from_millis(25));
            loop { tokio::select! {
                result = &mut result => break result.context("editor connection closed during LSP request")?,
                _ = tick.tick() => self.check()?,
            } }
        }).await.context("timeout: editor did not finish LSP startup and query within 75 seconds").and_then(|result| result);
        drop(flight);
        self.check()?;
        let reply = received?;
        ensure!(
            reply.rows.len() <= 50 && reply.source.len() <= 256,
            "invalid LSP result size"
        );
        let operation = serde_json::to_value(arguments.operation)?
            .as_str()
            .unwrap()
            .to_owned();
        let mut output = if arguments.operation == Operation::Hover {
            format!("{operation}: source={}\n", single_line(&reply.source, 256))
        } else {
            format!(
                "{operation}: {} results | source={}\n",
                reply.rows.len(),
                single_line(&reply.source, 256)
            )
        };
        if let Some(freshness) = reply.freshness {
            ensure!(
                matches!(freshness.as_str(), "current" | "stale" | "unknown"),
                "invalid diagnostics freshness"
            );
            output.push_str(&format!(
                "freshness={freshness}; LSP findings are not a full typecheck\n"
            ));
        }
        if let Some(hover) = reply.hover {
            append(&mut output, &truncate(&hover, HOVER_BYTES));
        }
        let mut next_offset = reply.next_offset;
        for (index, row) in reply.rows.iter().enumerate() {
            ensure!(row.path.len() <= 4096, "LSP result path exceeds 4096 bytes");
            if self.authorize(&row.path).await.is_err() {
                append(&mut output, "Results omitted by read policy\n");
                continue;
            }
            ensure!(
                row.line > 0 && row.column > 0,
                "invalid LSP result position"
            );
            let display = display_path(&row.path, &self.workspace);
            let line = format!(
                "{}:{}:{} | {}{}\n",
                single_line(&display, 4096),
                row.line,
                row.column,
                single_line(&row.text, 160),
                if row.modified == Some(true) {
                    " [unsaved]"
                } else {
                    ""
                }
            );
            if output.len() + line.len() > REPLY_BYTES - 1024 {
                next_offset = Some(arguments.offset.unwrap_or(0) + index as u32);
                break;
            }
            output.push_str(&line);
        }
        if let Some(offset) = next_offset {
            append(
                &mut output,
                &format!("next_offset={offset} (fresh query)\n"),
            );
        }
        if let Some(warnings) = reply.warnings {
            for warning in warnings.into_iter().take(3) {
                append(
                    &mut output,
                    &format!("partial: {}\n", single_line(&warning, 160)),
                );
            }
        }
        self.check()?;
        Ok(output)
    }
}

/// Revokes a scope when its owner stops awaiting the provider turn.
pub(crate) struct LspLease(Arc<LspScope>);
impl Drop for LspLease {
    fn drop(&mut self) {
        self.0.active.store(false, Ordering::Release);
    }
}

fn truncate(text: &str, maximum: usize) -> String {
    if text.len() <= maximum {
        return text.into();
    }
    let suffix = if maximum >= 13 { "\n[truncated]\n" } else { "" };
    let mut end = maximum.saturating_sub(suffix.len());
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    format!("{}{suffix}", &text[..end])
}

fn display_path(path: &str, workspace: &Path) -> String {
    let path = path.trim_start_matches("\\\\?\\").replace('\\', "/");
    let root = workspace
        .to_string_lossy()
        .trim_start_matches("\\\\?\\")
        .replace('\\', "/");
    let root = root.trim_end_matches('/');
    let prefix_matches = path.get(..root.len()).is_some_and(|prefix| {
        if cfg!(windows) {
            prefix.eq_ignore_ascii_case(root)
        } else {
            prefix == root
        }
    });
    if prefix_matches && path.as_bytes().get(root.len()) == Some(&b'/') {
        path[root.len() + 1..].into()
    } else {
        path
    }
}
fn single_line(text: &str, maximum: usize) -> String {
    text.split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .chars()
        .take(maximum)
        .collect()
}
fn append(output: &mut String, text: &str) {
    output.push_str(&truncate(text, REPLY_BYTES.saturating_sub(output.len())));
}

/// Defines a compact provider-neutral schema without protocol-specific objects.
pub(crate) fn definition() -> super::ControlToolDefinition {
    super::ControlToolDefinition {
        name: "lsp",
        description: "Query project language servers using live editor contents, waiting for initialization when needed. Positions are one-based Unicode characters. Position operations require path/line/column. Symbols use query; calls require direction. Diagnostics inspect one file and do not prove a full typecheck. Lists default to 20 rows; use next_offset for more. Read-only operations.",
        input_schema: json!({"type":"object","additionalProperties":false,"required":["operation"],"properties":{
            "operation":{"type":"string","enum":["status","definition","type_definition","implementation","references","hover","document_symbols","workspace_symbols","calls","diagnostics"]},
            "path":{"type":"string","minLength":1,"maxLength":4096,"description":"Source file, relative to workspace or absolute. Optional for status/workspace_symbols to select a server."},
            "line":{"type":"integer","minimum":1},"column":{"type":"integer","minimum":1},
            "query":{"type":"string","minLength":1,"maxLength":256,"description":"Required for workspace_symbols; optional literal name filter for document_symbols."},
            "direction":{"type":"string","enum":["incoming","outgoing"]},
            "limit":{"type":"integer","minimum":1,"maximum":50},"offset":{"type":"integer","minimum":0}
        }}),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contract_rejects_misleading_arguments_and_stays_small() {
        assert!(serde_json::to_vec(&definition().mcp_value()).unwrap().len() <= 3072);
        for value in [
            json!({"operation":"definition","path":"a.rs"}),
            json!({"operation":"status","line":1}),
            json!({"operation":"calls","path":"a","line":1,"column":1}),
            json!({"operation":"hover","path":"a","line":1,"column":1,"offset":0}),
            json!({"operation":"diagnostics","path":"a","query":"name"}),
            json!({"operation":"workspace_symbols","query":""}),
            json!({"operation":"status","limit":51}),
        ] {
            assert!(
                serde_json::from_value::<Arguments>(value)
                    .unwrap()
                    .validate()
                    .is_err()
            );
        }
        assert!(
            serde_json::from_value::<Arguments>(json!({"operation":"status","client_id":1}))
                .is_err()
        );
        for maximum in 0..100 {
            assert!(truncate(&"😀".repeat(100), maximum).len() <= maximum);
        }
        assert_eq!(
            display_path("/repo/src/main.rs", Path::new("/repo")),
            "src/main.rs"
        );
        assert_eq!(
            display_path("/repository/main.rs", Path::new("/repo")),
            "/repository/main.rs"
        );
        if cfg!(windows) {
            assert_eq!(
                display_path("d:/repo/src/main.rs", Path::new("D:/Repo")),
                "src/main.rs"
            );
        }
    }

    async fn fixture() -> (
        tempfile::TempDir,
        Arc<LspService>,
        Arc<LspScope>,
        LspLease,
        forge_protocol::outbound::MessageReceiver,
    ) {
        let directory = tempfile::tempdir().unwrap();
        std::fs::write(directory.path().join("source.rs"), "pub fn source() {}\n").unwrap();
        let service = Arc::new(LspService::default());
        let (sender, receiver) = forge_protocol::outbound::channel();
        service.attach(sender);
        service.set_enabled("session", true);
        let policy = Arc::new(CompiledPermissionDocument::compile(
            crate::permissions::document::default_permission_document(),
            directory.path(),
        ));
        let (scope, lease) = service.scope(
            "session".into(),
            "interaction".into(),
            directory.path().into(),
            policy,
            PermissionMode::Read,
            Arc::new(AtomicBool::new(false)),
        );
        (directory, service, scope, lease, receiver)
    }

    #[tokio::test]
    async fn reply_is_owned_authorized_and_bounded() {
        let (directory, service, scope, _lease, mut receiver) = fixture().await;
        let work = tokio::spawn(async move {
            scope
                .invoke(json!({"operation":"hover","path":"source.rs","line":1,"column":1}))
                .await
        });
        let frame = receiver.recv().await.unwrap().unwrap();
        let event: Value = serde_json::from_slice(frame.bytes()).unwrap();
        let id = event["payload"]["id"].as_str().unwrap();
        let path = directory
            .path()
            .join("source.rs")
            .to_string_lossy()
            .into_owned();
        assert!(
            service
                .authorize("other", id, vec![path.clone()])
                .await
                .is_err()
        );
        assert!(
            service
                .authorize("session", id, vec![path.clone()])
                .await
                .unwrap()[&path]
                .as_bool()
                .unwrap()
        );
        assert!(service.respond("other", id, json!({}), None).is_err());
        service
            .respond(
                "session",
                id,
                json!({"rows":[],"source":"buffer (unsaved) version=2","hover":"😀".repeat(10000)}),
                None,
            )
            .unwrap();
        let output = work.await.unwrap().unwrap();
        assert!(output.starts_with("hover: source=") && !output.contains("0 results"));
        assert!(
            output.len() <= REPLY_BYTES
                && output.contains("unsaved")
                && output.contains("truncated")
        );
        assert!(service.respond("session", id, json!({}), None).is_err());
    }

    #[tokio::test]
    async fn dropping_a_query_and_disabling_tools_release_authority() {
        let (_directory, service, scope, lease, mut receiver) = fixture().await;
        let active = scope.clone();
        let work = tokio::spawn(async move { active.invoke(json!({"operation":"status"})).await });
        receiver.recv().await.unwrap().unwrap();
        work.abort();
        assert!(work.await.unwrap_err().is_cancelled());
        assert!(service.pending.lock().unwrap().is_empty());
        assert_eq!(service.slots.available_permits(), 4);
        service.set_enabled("session", false);
        service.set_enabled("session", true);
        assert!(
            !scope.available(),
            "reenabling revived old request authority"
        );
        drop(lease);
    }
}
