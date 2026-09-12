use std::{
    collections::HashMap,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock},
    document::BufferDocument,
    identity::{BlockId, DocumentId, InputSequence, ViewId},
    input::DocumentInput,
    patch::{BufferPatch, BufferSnapshot},
    text::BufferText,
};
use forge_diff::{
    source::SourceVersion,
    syntax::{SyntaxEngine, SyntaxHandle, SyntaxRequest},
    workers::WorkPriority,
};
use forge_git::{RepositoryPath, content::ContentSource, store::RepositoryStore};
use serde::Serialize;

use crate::{
    BodyState,
    document::{DOCUMENT_BYTES, DocumentAdmission},
    service::{optional_syntax, syntax_decorations},
    source::{SourceUnavailable, acquire, language},
};

#[derive(Debug, Serialize)]
pub struct SourceOpen {
    pub snapshot: BufferSnapshot,
    pub state: BodyState,
    pub more: bool,
    pub title: String,
    pub object: String,
    pub revision: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub syntax_diagnostic: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct SourceDelivery {
    pub patch: Option<BufferPatch>,
    pub state: BodyState,
    pub more: bool,
}

pub struct SourceDocumentService {
    store: Arc<RepositoryStore>,
    syntax: Arc<SyntaxEngine>,
    document: Mutex<HashMap<DocumentId, Arc<tokio::sync::Mutex<SourceDocument>>>>,
    admitted: Arc<crate::document::DocumentAdmissionStore>,
    closed: AtomicBool,
}

struct SourceDocument {
    buffer: BufferDocument,
    source: Option<SourceVersion>,
    syntax: Option<SyntaxHandle>,
    offset: usize,
    row: usize,
    retained: usize,
    state: BodyState,
    input: HashMap<ViewId, InputSequence>,
    _admission: DocumentAdmission,
}

impl SourceDocumentService {
    pub fn new(store: Arc<RepositoryStore>, syntax: Arc<SyntaxEngine>) -> Self {
        Self {
            store,
            syntax,
            document: Mutex::new(HashMap::new()),
            admitted: Arc::new(crate::document::DocumentAdmissionStore::default()),
            closed: AtomicBool::new(false),
        }
    }

    pub fn open_text(&self, id: DocumentId, title: String, text: Vec<u8>) -> Result<SourceOpen> {
        self.open_literal(id, title, text, 1024 * 1024, "text")
    }

    pub fn open_diff_text(
        &self,
        id: DocumentId,
        title: String,
        text: Vec<u8>,
    ) -> Result<SourceOpen> {
        self.open_literal(id, title, text, 8 * 1024 * 1024, "diff")
    }

    fn open_literal(
        &self,
        id: DocumentId,
        title: String,
        text: Vec<u8>,
        limit: usize,
        kind: &str,
    ) -> Result<SourceOpen> {
        id.validate()?;
        ensure!(
            title.len() <= 4096 && !title.contains(['\0', '\n', '\r']),
            "invalid source title"
        );
        ensure!(text.len() <= limit, "literal source exceeds its byte limit");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let source = SourceVersion::new(text, forge_diff::source::Representation::Raw)
            .map_err(|error| anyhow::anyhow!("invalid literal source: {error:?}"))?;
        self.adopt_version(id, title, source, None, None, kind, admission)
    }

    pub async fn open_version(
        &self,
        id: DocumentId,
        path: RepositoryPath,
        source: SourceVersion,
    ) -> Result<SourceOpen> {
        id.validate()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let (syntax, syntax_diagnostic) = if let Some(language) = language(&path) {
            optional_syntax(
                &self.syntax,
                SyntaxRequest {
                    source: source.clone(),
                    language,
                    priority: WorkPriority::Visible,
                    deadline: Some(std::time::Instant::now() + std::time::Duration::from_secs(30)),
                },
            )
            .await
        } else {
            (None, None)
        };
        admission.check()?;
        self.adopt_version(
            id,
            path.display_label(),
            source,
            syntax,
            syntax_diagnostic,
            "captured",
            admission,
        )
    }

    fn adopt_version(
        &self,
        id: DocumentId,
        title: String,
        source: SourceVersion,
        syntax: Option<SyntaxHandle>,
        syntax_diagnostic: Option<String>,
        kind: &str,
        admission: DocumentAdmission,
    ) -> Result<SourceOpen> {
        let object = source
            .identity()
            .content_hash
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect();
        let mut document = SourceDocument {
            buffer: BufferDocument::new(id.clone(), Vec::new())?,
            retained: source.retained_bytes() + title.len(),
            source: Some(source),
            syntax,
            offset: 0,
            row: 0,
            state: BodyState::Partial,
            input: HashMap::new(),
            _admission: admission,
        };
        document.deliver()?;
        let output = SourceOpen {
            snapshot: document.buffer.snapshot(),
            state: document.state.clone(),
            more: document.state == BodyState::Partial,
            title,
            object,
            revision: kind.into(),
            syntax_diagnostic,
        };
        let mut documents = self.document.lock().expect("source document lock");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service closed during open"
        );
        ensure!(
            !documents.contains_key(&id),
            "source document identity is already open"
        );
        document._admission.check()?;
        documents.insert(id, Arc::new(tokio::sync::Mutex::new(document)));
        Ok(output)
    }

    pub async fn open(
        &self,
        id: DocumentId,
        workspace: PathBuf,
        revision: String,
        path: Vec<u8>,
    ) -> Result<SourceOpen> {
        id.validate()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let path = RepositoryPath::new(path)?;
        let repository = self
            .store
            .open(workspace)
            .await?
            .context("source document path has no Git repository")?;
        let resolved = forge_git::revision::resolve_file(
            &self.store,
            Arc::clone(&repository),
            revision,
            path.clone(),
        )
        .await?
        .value;
        let revision = match resolved.origin {
            forge_git::revision::RevisionOrigin::Commit(commit) => {
                commit.to_string()[..12].to_owned()
            }
            forge_git::revision::RevisionOrigin::Index => "index".into(),
            forge_git::revision::RevisionOrigin::Object(blob) => {
                format!("blob-{}", &blob.to_string()[..12])
            }
        };
        let title = format!("{} @ {revision}", path.display_label());
        let mut document = SourceDocument {
            buffer: BufferDocument::new(id.clone(), Vec::new())?,
            source: None,
            syntax: None,
            offset: 0,
            row: 0,
            retained: title.len(),
            state: BodyState::Ready,
            input: HashMap::new(),
            _admission: admission,
        };
        let mut syntax_diagnostic = None;
        match acquire(
            &self.store,
            &repository,
            Some(ContentSource::Object(resolved.blob)),
        )
        .await
        {
            Ok(source) => {
                if let Some(language) = language(&path) {
                    let (syntax, diagnostic) = optional_syntax(
                        &self.syntax,
                        SyntaxRequest {
                            source: source.clone(),
                            language,
                            priority: WorkPriority::Visible,
                            deadline: None,
                        },
                    )
                    .await;
                    document.syntax = syntax;
                    syntax_diagnostic = diagnostic;
                }
                document.retained += source.retained_bytes();
                document.source = Some(source);
                document.state = BodyState::Partial;
                document.deliver()?;
            }
            Err(error) => {
                document.state = if error.downcast_ref::<SourceUnavailable>().is_some() {
                    BodyState::Unavailable(format!("{error:#}"))
                } else {
                    BodyState::Failed(format!("{error:#}"))
                }
            }
        }
        let output = SourceOpen {
            snapshot: document.buffer.snapshot(),
            state: document.state.clone(),
            more: document.state == BodyState::Partial,
            title,
            object: resolved.blob.to_string(),
            revision,
            syntax_diagnostic,
        };
        let mut documents = self.document.lock().expect("source document lock");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service closed during open"
        );
        ensure!(
            !documents.contains_key(&id),
            "source document identity is already open"
        );
        document._admission.check()?;
        documents.insert(id, Arc::new(tokio::sync::Mutex::new(document)));
        Ok(output)
    }

    pub async fn demand(&self, input: DocumentInput) -> Result<SourceDelivery> {
        input.validate()?;
        ensure!(
            input.action == "demand",
            "source document accepts only demand input"
        );
        ensure!(
            input.target.is_none(),
            "plain source input cannot name a semantic target"
        );
        let owner = self.get(&input.document)?;
        let mut document = owner.lock().await;
        ensure!(
            document.buffer.revision() == input.revision,
            "source document input revision is stale"
        );
        let block = document
            .buffer
            .block(&input.block)
            .context("source input block disappeared")?;
        let row = block
            .text
            .row(input.position.row)
            .context("source input row is outside block")?;
        ensure!(
            input.position.column <= row.len() && row.is_char_boundary(input.position.column),
            "source input column is invalid"
        );
        ensure!(
            document.input.contains_key(&input.view) || document.input.len() < 64,
            "source document view admission is full"
        );
        ensure!(
            document
                .input
                .get(&input.view)
                .is_none_or(|previous| input.sequence > *previous),
            "source input sequence is stale"
        );
        document.input.insert(input.view, input.sequence);
        document.deliver()
    }

    pub async fn snapshot(&self, id: &DocumentId) -> Result<BufferSnapshot> {
        Ok(self.get(id)?.lock().await.buffer.snapshot())
    }

    pub async fn close_view(&self, id: &DocumentId, view: &ViewId) -> Result<()> {
        self.get(id)?.lock().await.input.remove(view);
        Ok(())
    }

    pub fn close(&self, id: &DocumentId) -> bool {
        let admitted = self.admitted.cancel(id);
        self.document
            .lock()
            .expect("source document lock")
            .remove(id)
            .is_some()
            || admitted
    }

    pub async fn close_collected(&self, id: &DocumentId) -> bool {
        let closed = self.close(id);
        self.admitted.wait_closed(id).await;
        closed
    }

    pub fn close_all(&self) {
        self.closed.store(true, Ordering::Release);
        self.admitted.cancel_all();
        self.document.lock().expect("source document lock").clear();
    }

    fn get(&self, id: &DocumentId) -> Result<Arc<tokio::sync::Mutex<SourceDocument>>> {
        self.document
            .lock()
            .expect("source document lock")
            .get(id)
            .cloned()
            .context("unknown source document")
    }
}

impl SourceDocument {
    fn deliver(&mut self) -> Result<SourceDelivery> {
        self._admission.check()?;
        if self.state != BodyState::Partial {
            return Ok(SourceDelivery {
                patch: None,
                state: self.state.clone(),
                more: false,
            });
        }
        let source = self
            .source
            .as_ref()
            .context("source document content missing")?;
        let mut rows = Vec::new();
        let mut metadata = BlockMetadata::default();
        let mut bytes = 0;
        let mut offset = self.offset;
        while offset < source.bytes().len() && rows.len() < 256 {
            let remaining = &source.text()[offset..];
            let end = remaining.find('\n').unwrap_or(remaining.len());
            let text = if end < remaining.len() {
                remaining[..end]
                    .strip_suffix('\r')
                    .unwrap_or(&remaining[..end])
            } else {
                &remaining[..end]
            };
            if bytes + text.len() > 128 * 1024 {
                if rows.is_empty() {
                    self.state =
                        BodyState::Unavailable("source row exceeds 128 KiB delivery limit".into());
                }
                break;
            }
            let mut row_metadata = BlockMetadata::default();
            if let Some(syntax) = &self.syntax {
                if !syntax_decorations(
                    syntax,
                    self.row + rows.len(),
                    rows.len(),
                    text,
                    &mut row_metadata,
                ) {
                    self.state =
                        BodyState::Unavailable("source row exceeds decoration admission".into());
                    break;
                }
            }
            if metadata.visible_decoration.len() + row_metadata.visible_decoration.len() > 8192 {
                break;
            }
            metadata
                .visible_decoration
                .extend(row_metadata.visible_decoration);
            rows.push(text.to_owned());
            bytes += text.len();
            offset += end + usize::from(end < remaining.len());
        }
        let retained = bytes
            + rows.len() * 32
            + metadata
                .visible_decoration
                .iter()
                .map(|decoration| 160 + decoration.capture.capacity())
                .sum::<usize>();
        if self.retained.saturating_add(retained) > DOCUMENT_BYTES {
            self.state = BodyState::Unavailable("source document exceeds 16 MiB admission".into());
            return Ok(SourceDelivery {
                patch: None,
                state: self.state.clone(),
                more: false,
            });
        }
        let patch = if rows.is_empty() {
            None
        } else {
            let count = rows.len();
            let block = BufferBlock {
                id: BlockId(format!("source:{}", self.row)),
                text: BufferText::from_rows(rows)?,
                metadata,
            };
            let end = self.buffer.block_count();
            let patch = self.buffer.edit(end..end, vec![block])?;
            self.offset = offset;
            self.row += count;
            self.retained += retained;
            patch
        };
        if self.offset == source.bytes().len() {
            self.state = BodyState::Ready;
        }
        Ok(SourceDelivery {
            patch,
            state: self.state.clone(),
            more: self.state == BodyState::Partial,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::TextPosition;
    use forge_diff::{cache::CacheLimits, engine::DiffEngine, syntax::SyntaxLimits};

    #[tokio::test]
    async fn saved_diff_retains_large_read_only_source_through_bounded_delivery() -> Result<()> {
        let store = Arc::new(RepositoryStore::default());
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let service = SourceDocumentService::new(
            store,
            SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default()),
        );
        let text = format!("+{}\n", "x".repeat(254)).repeat(8192).into_bytes();
        assert!(
            service
                .open_text(
                    DocumentId("literal-limit".into()),
                    "message".into(),
                    text.clone()
                )
                .is_err()
        );
        let id = DocumentId("saved-diff".into());
        let opened = service.open_diff_text(id.clone(), "Tool changes".into(), text)?;
        assert_eq!(opened.revision, "diff");
        assert_eq!(opened.snapshot.block[0].text.row_count(), 256);
        let mut revision = opened.snapshot.revision;
        let mut more = opened.more;
        let mut rows = 256;
        let mut sequence = 0;
        while more {
            sequence += 1;
            let delivered = service
                .demand(DocumentInput {
                    document: id.clone(),
                    revision,
                    view: ViewId("source-view".into()),
                    sequence: InputSequence(sequence),
                    action: "demand".into(),
                    block: opened.snapshot.block[0].id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                    target: None,
                })
                .await?;
            if let Some(patch) = delivered.patch {
                assert!(patch.next_rows - patch.base_rows <= 256);
                rows = patch.next_rows;
                revision = patch.next;
            }
            more = delivered.more;
        }
        assert_eq!(rows, 8192);
        assert!(service.close(&id));
        assert!(
            service
                .open_diff_text(
                    DocumentId("diff-limit".into()),
                    "large".into(),
                    vec![b'x'; 8 * 1024 * 1024 + 1]
                )
                .is_err()
        );
        Ok(())
    }
}
