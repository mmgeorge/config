use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, AtomicU64, Ordering},
};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use tokio::sync::{Semaphore, oneshot};

use crate::permissions::{
    document::PermissionDecision,
    matcher::{CompiledPermissionDocument, PermissionRequest, PermissionTarget},
};
use crate::session::PermissionMode;
use forge_protocol::{message::SessionEvent, outbound::MessageSender};

mod contract;
mod render;
use contract::Arguments;
pub(crate) use contract::definitions;
use render::Reply;

pub(crate) fn is_tool(name: &str) -> bool {
    contract::NAMES.contains(&name)
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
    upload: Option<AnalysisUpload>,
}

struct AnalysisUpload {
    path: String,
    total: usize,
    source: String,
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

    /// Receives ordered source chunks within the active request lifetime.
    pub(crate) async fn analyze_part(
        &self,
        session: &str,
        id: &str,
        path: &str,
        source: String,
        position: Option<(usize, usize)>,
        syntax: &Arc<forge_diff::syntax::SyntaxEngine>,
    ) -> Result<Value> {
        let Some((offset, total)) = position else {
            return self.analyze(session, id, path, source, syntax).await;
        };
        let scope = {
            let pending = self.pending.lock().unwrap();
            let request = pending.get(id).context("cancelled: LSP request is no longer active.")?;
            ensure!(request.session == session, "invalid_request: LSP session mismatch.");
            request.scope.clone()
        };
        scope.authorize(path).await?;
        let assembled = {
            let mut pending = self.pending.lock().unwrap();
            let request = pending.get_mut(id).context("cancelled: LSP request is no longer active.")?;
            scope.check()?;
            if offset == 0 {
                ensure!(request.upload.is_none(), "invalid_request: Source transfer already active.");
                request.upload = Some(AnalysisUpload { path: path.into(), total, source: String::new() });
            }
            let upload = request.upload.as_mut().context("invalid_request: Source transfer is missing.")?;
            ensure!(upload.path == path && upload.total == total && upload.source.len() == offset,
                "invalid_request: Source transfer identity or offset mismatch.");
            ensure!(source.len() <= total.saturating_sub(offset) && (!source.is_empty() || total == 0),
                "invalid_request: Source chunk exceeds declared size or is empty.");
            upload.source.push_str(&source);
            if upload.source.len() == total { request.upload.take().map(|upload| upload.source) } else { None }
        };
        match assembled {
            Some(source) => self.analyze(session, id, path, source, syntax).await,
            None => Ok(json!({"pending":true})),
        }
    }

    /// Analyze an authorized live snapshot on the host's shared syntax workers.
    pub(crate) async fn analyze(
        &self,
        session: &str,
        id: &str,
        path: &str,
        source: String,
        syntax: &Arc<forge_diff::syntax::SyntaxEngine>,
    ) -> Result<Value> {
        let scope = {
            let pending = self.pending.lock().unwrap();
            let request = pending.get(id).context("LSP request is no longer active")?;
            ensure!(
                request.session == session,
                "LSP request belongs to another session"
            );
            request.scope.clone()
        };
        scope.authorize(path).await?;
        let language = forge_diff::syntax::SyntaxLanguage::for_path(path)
            .context("unsupported source language")?;
        ensure!(
            matches!(
                language,
                forge_diff::syntax::SyntaxLanguage::Rust
                    | forge_diff::syntax::SyntaxLanguage::Typescript
                    | forge_diff::syntax::SyntaxLanguage::Tsx
                    | forge_diff::syntax::SyntaxLanguage::Javascript
                    | forge_diff::syntax::SyntaxLanguage::Lua
            ),
            "unsupported structural language"
        );
        let request = forge_diff::syntax::SyntaxRequest {
            source: forge_diff::source::SourceVersion::new(
                source.into_bytes(),
                forge_diff::source::Representation::Raw,
            )?,
            language,
            priority: forge_diff::workers::WorkPriority::Foreground,
            deadline: Some(std::time::Instant::now() + Duration::from_secs(10)),
        };
        let analysis = syntax.analyze_full(request);
        tokio::pin!(analysis);
        let mut tick = tokio::time::interval(Duration::from_millis(25));
        let handle = loop {
            tokio::select! {
                result = &mut analysis => break result.map_err(|failure| anyhow::anyhow!("source analysis failed: {failure:?}"))?,
                _ = tick.tick() => {
                    scope.check()?;
                    ensure!(self.pending.lock().unwrap().contains_key(id), "LSP request is no longer active");
                }
            }
        };
        scope.check()?;
        let declarations: Vec<_> = handle.declarations().into_iter().map(|declaration| json!({
            "name":declaration.name,"kind":declaration.kind,"signature":declaration.signature,
            "visibility":declaration.visibility,"line":declaration.line,"column":declaration.column,
            "start_line":declaration.start_line,"end_line":declaration.end_line,"parent":declaration.parent,"depth":declaration.depth,
                "range_start_line":declaration.range_start_line,"range_start_column":declaration.range_start_column,
                "range_end_line":declaration.range_end_line,"range_end_column":declaration.range_end_column,
        })).collect();
        Ok(json!({"declarations":declarations, "complete":handle.tree_count() > 0}))
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
            "cancelled: LSP request cancelled."
        );
        ensure!(
            self.selection.enabled.load(Ordering::Acquire)
                && self.selection.generation.load(Ordering::Acquire) == self.generation,
            "cancelled: LSP tools were disabled or their configuration changed."
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
            Err(_) => tokio::fs::canonicalize(absolute.parent().context("invalid_path: Path has no parent.")?)
                .await.map_err(|_| anyhow::anyhow!("path_not_found: {}", path.display()))?
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
            "access_denied: {}", absolute.display()
        );
        self.check()?;
        Ok(absolute)
    }

    /// Execute an allowlisted query and emit only bounded, authorized text.
    pub(crate) async fn invoke(self: &Arc<Self>, name: &str, value: Value) -> Result<String> {
        self.check()?;
        let mut arguments = Arguments::parse(name, value).map_err(|failure| anyhow::anyhow!("invalid_argument: {failure}"))?;
        if let Some(path) = &arguments.path {
            arguments.path = Some(
                self.authorize(path)
                    .await?
                    .to_string_lossy()
                    .trim_start_matches("\\\\?\\")
                    .replace('\\', "/"),
            );
        }
        for target in [&mut arguments.from, &mut arguments.to].into_iter().flatten() {
            if let Some(path) = &target.path {
                target.path = Some(self.authorize(path).await?.to_string_lossy().trim_start_matches("\\\\?\\").replace('\\', "/"));
            }
        }
        let _slot = self
            .service
            .slots
            .try_acquire()
            .context("busy: All LSP request slots are occupied.")?;
        let sender = self
            .service
            .sender
            .lock()
            .unwrap()
            .clone()
            .context("disconnected: No editor connection.")?;
        let id = uuid::Uuid::new_v4().to_string();
        let (complete, result) = oneshot::channel();
        self.service.pending.lock().unwrap().insert(
            id.clone(),
            Pending {
                session: self.session.clone(),
                scope: self.clone(),
                result: complete,
                upload: None,
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
        let timeout = if arguments.operation == contract::Operation::Paths { 105 } else { 75 };
        let received = tokio::time::timeout(Duration::from_secs(timeout), async {
            sender.send_event(event).await?;
            tokio::pin!(result);
            let mut tick = tokio::time::interval(Duration::from_millis(25));
            loop { tokio::select! {
                result = &mut result => break result.context("disconnected: Editor connection closed during LSP request.")?,
                _ = tick.tick() => self.check()?,
            } }
        }).await.context("timeout: Editor did not complete LSP startup and query within its deadline.").and_then(|result| result);
        drop(flight);
        self.check()?;
        let reply = received?;
        for row in reply.rows.iter().chain(&reply.omitted_visibility) {
            ensure!(row.path.len() <= 4096, "LSP result path exceeds 4096 bytes");
            self.authorize(&row.path).await?;
            if let Some(path) = &row.excerpt_path {
                self.authorize(path).await?;
            }
        }
        self.check()?;
        render::render(reply, &arguments, &self.workspace)
    }
}

/// Revokes a scope when its owner stops awaiting the provider turn.
pub(crate) struct LspLease(Arc<LspScope>);
impl Drop for LspLease {
    fn drop(&mut self) {
        self.0.active.store(false, Ordering::Release);
    }
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
    if prefix_matches && path.len() == root.len() {
        ".".into()
    } else if prefix_matches && path.as_bytes().get(root.len()) == Some(&b'/') {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn focused_contract_rejects_irrelevant_arguments() {
        for (name, value) in [
            ("lsp_definition", json!({"path":"a.rs"})),
            ("lsp_status", json!({"line":1})),
            ("lsp_calls", json!({"path":"a","line":1,"column":1})),
            (
                "lsp_context",
                json!({"path":"a","line":1,"column":1,"offset":0}),
            ),
            ("lsp_diagnostics", json!({"path":"a","query":"name"})),
            ("lsp_symbols", json!({"query":""})),
            ("lsp_symbols", json!({"limit":0})),
        ] {
            assert!(Arguments::parse(name, value).is_err());
        }
        assert!(Arguments::parse("lsp", json!({})).is_err());
        assert_eq!(definitions().len(), 9);
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
                .invoke(
                    "lsp_context",
                    json!({"path":"source.rs","line":1,"column":1}),
                )
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
                json!({"rows":[{"path":path,"line":1,"column":8,"text":"pub fn source()","modified":true}],"source":"buffer"}),
                None,
            )
            .unwrap();
        let output = work.await.unwrap().unwrap();
        assert!(output.contains("source.rs (unsaved)") && output.contains("pub fn source()"));
        assert!(!output.contains('|'));
        assert!(service.respond("session", id, json!({}), None).is_err());
    }

    #[tokio::test]
    async fn dropping_a_query_and_disabling_tools_release_authority() {
        let (_directory, service, scope, lease, mut receiver) = fixture().await;
        let active = scope.clone();
        let work = tokio::spawn(async move { active.invoke("lsp_status", json!({})).await });
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

    #[tokio::test]
    async fn chunked_analysis_preserves_large_source_and_request_ownership() {
        let (directory, service, scope, _lease, mut receiver) = fixture().await;
        let task = tokio::spawn(async move { scope.invoke("lsp_status", json!({})).await });
        let frame = receiver.recv().await.unwrap().unwrap();
        let event: Value = serde_json::from_slice(frame.bytes()).unwrap();
        let id = event["payload"]["id"].as_str().unwrap();
        let path = directory.path().join("source.rs").to_string_lossy().into_owned();
        let syntax = forge_diff::syntax::SyntaxEngine::new(Arc::new(forge_diff::workers::AnalysisPool::new(
            forge_diff::workers::PoolLimits { workers: 1, jobs: 2, input_bytes: 1024 * 1024 },
        )));
        let source = format!("{}pub fn beyond_previous_limits() {{}}\n", "// Unicode α and source padding for a large file\n".repeat(11000));
        assert!(source.len() > 512 * 1024);
        let rendering = syntax.analyze(forge_diff::syntax::SyntaxRequest {
            source: forge_diff::source::SourceVersion::new(source.as_bytes().to_vec(), forge_diff::source::Representation::Raw).unwrap(),
            language: forge_diff::syntax::SyntaxLanguage::Rust,
            priority: forge_diff::workers::WorkPriority::Foreground,
            deadline: None,
        }).await.unwrap();
        assert_eq!(rendering.tree_count(), 0, "rendering cutoff changed");
        let mut offset = 0;
        let mut result = json!(null);
        while offset < source.len() {
            let mut end = (offset + 32768).min(source.len());
            while !source.is_char_boundary(end) { end -= 1; }
            result = service.analyze_part("session", id, &path, source[offset..end].into(), Some((offset, source.len())), &syntax).await.unwrap();
            if end < source.len() { assert_eq!(result, json!({"pending":true})); }
            offset = end;
        }
        assert_eq!(result["complete"], true);
        assert_eq!(result["declarations"][0]["name"], "beyond_previous_limits");
        assert_eq!(result["declarations"][0]["line"], 11001);
        assert!(service.analyze_part("other", id, &path, "x".into(), Some((0, 2)), &syntax).await.is_err());
        assert!(service.analyze_part("session", id, &path, "x".into(), Some((1, 2)), &syntax).await.is_err());
        service.analyze_part("session", id, &path, "x".into(), Some((0, 2)), &syntax).await.unwrap();
        task.abort();
        assert!(task.await.unwrap_err().is_cancelled());
        assert!(service.pending.lock().unwrap().is_empty());
    }
}
