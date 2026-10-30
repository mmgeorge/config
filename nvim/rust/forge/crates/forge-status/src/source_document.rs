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

mod saved;

use crate::{
    BodyState,
    document::DocumentAdmission,
    service::{optional_syntax, syntax_decorations},
    source::{SourceUnavailable, acquire, language},
};

#[derive(Debug, Serialize)]
pub struct SourceOpen {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub patch: Option<BufferPatch>,
    pub state: BodyState,
    pub more: bool,
}

#[derive(Debug, Serialize)]
pub struct SourceSnapshot {
    #[serde(flatten)]
    pub snapshot: BufferSnapshot,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub more: bool,
}

pub struct SourceDocumentService {
    store: Arc<RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    syntax: Arc<SyntaxEngine>,
    document: Mutex<HashMap<DocumentId, Arc<tokio::sync::Mutex<SourceDocument>>>>,
    admitted: Arc<crate::document::DocumentAdmissionStore>,
    closed: AtomicBool,
}

struct SourceDocument {
    saved: Option<saved::SavedDocument>,
    saved_diagnostic: Option<String>,
    diff_view: Option<forge_diff::view::DiffViewState>,
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

struct PreparedSource {
    source: SourceVersion,
    syntax: Option<SyntaxHandle>,
    syntax_diagnostic: Option<String>,
}

impl SourceDocumentService {
    pub fn new(
        store: Arc<RepositoryStore>,
        diff: Arc<forge_diff::engine::DiffEngine>,
        syntax: Arc<SyntaxEngine>,
    ) -> Self {
        Self {
            store,
            diff,
            syntax,
            document: Mutex::new(HashMap::new()),
            admitted: Arc::new(crate::document::DocumentAdmissionStore::default()),
            closed: AtomicBool::new(false),
        }
    }

    pub fn open_text(&self, id: DocumentId, title: String, text: Vec<u8>) -> Result<SourceOpen> {
        self.open_literal(id, title, text, "text")
    }

    pub fn open_diff_text(
        &self,
        id: DocumentId,
        title: String,
        text: Vec<u8>,
    ) -> Result<SourceOpen> {
        self.open_literal(id, title, text, "diff")
    }

    fn open_literal(
        &self,
        id: DocumentId,
        title: String,
        text: Vec<u8>,
        kind: &str,
    ) -> Result<SourceOpen> {
        id.validate()?;
        ensure!(!title.contains(['\0', '\n', '\r']), "invalid source title");
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "source document service is closed"
        );
        let admission = self.admitted.admit(id.clone())?;
        let source = SourceVersion::new(text, forge_diff::source::Representation::Raw)
            .map_err(|error| anyhow::anyhow!("invalid literal source: {error:?}"))?;
        self.adopt_version(
            id,
            title,
            PreparedSource {
                source,
                syntax: None,
                syntax_diagnostic: None,
            },
            kind,
            admission,
        )
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
            PreparedSource {
                source,
                syntax,
                syntax_diagnostic,
            },
            "captured",
            admission,
        )
    }

    fn adopt_version(
        &self,
        id: DocumentId,
        title: String,
        prepared: PreparedSource,
        kind: &str,
        admission: DocumentAdmission,
    ) -> Result<SourceOpen> {
        let PreparedSource {
            source,
            syntax,
            syntax_diagnostic,
        } = prepared;
        let object = source
            .identity()
            .content_hash
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect();
        let mut document = SourceDocument {
            saved: None,
            saved_diagnostic: None,
            diff_view: (kind == "diff").then(|| {
                forge_diff::view::DiffViewState::new(
                    forge_diff::view::DiffCapabilities::Source,
                    forge_diff::view::DiffVisibility::All,
                )
            }),
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
        if kind == "diff" && document.buffer.block_count() == 0 {
            document.buffer.edit(
                0..0,
                vec![BufferBlock {
                    id: BlockId("source:0".into()),
                    text: BufferText::from_rows(["No changed files"])?,
                    metadata: BlockMetadata::default(),
                }],
            )?;
        }
        let output = SourceOpen {
            diff_view: document.diff_view.as_ref().map(|view| view.committed()),
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
            saved: None,
            saved_diagnostic: None,
            diff_view: None,
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
            diff_view: document.diff_view.as_ref().map(|view| view.committed()),
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
            matches!(
                input.action.as_str(),
                "demand"
                    | "toggle_visibility"
                    | "toggle_presentation"
                    | "source_expand"
                    | "source_collapse"
            ),
            "unsupported source document input"
        );
        let owner = self.get(&input.document)?;
        if owner.lock().await.saved.is_some() {
            return self.saved_input(owner, input).await;
        }
        ensure!(
            !matches!(input.action.as_str(), "source_expand" | "source_collapse"),
            "plain source has no semantic sections"
        );
        ensure!(
            input.target.is_none(),
            "plain source input cannot name a semantic target"
        );
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
            document
                .input
                .get(&input.view)
                .is_none_or(|previous| input.sequence > *previous),
            "source input sequence is stale"
        );
        if input.action != "demand" {
            let result = document.change_diff_view(if input.action == "toggle_visibility" {
                forge_diff::view::DiffViewAction::ToggleVisibility
            } else {
                forge_diff::view::DiffViewAction::TogglePresentation
            })?;
            document.input.insert(input.view, input.sequence);
            return Ok(result);
        }
        document.input.insert(input.view, input.sequence);
        document.deliver()
    }

    pub async fn snapshot_state(&self, id: &DocumentId) -> Result<SourceSnapshot> {
        let owner = self.get(id)?;
        let document = owner.lock().await;
        Ok(SourceSnapshot {
            snapshot: document.buffer.snapshot(),
            diff_view: document.diff_view.as_ref().map(|view| view.committed()),
            more: document.state == BodyState::Partial,
        })
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
    fn change_diff_view(
        &mut self,
        action: forge_diff::view::DiffViewAction,
    ) -> Result<SourceDelivery> {
        self._admission.check()?;
        let request = self
            .diff_view
            .as_mut()
            .context("plain sources have no diff controls")?
            .request(action)
            .map_err(|error| anyhow::anyhow!("source diff control failed: {error:?}"))?;
        let prepared = (|| -> Result<_> {
            let fallback = BlockId("source:diff:fallback".into());
            let previous = usize::from(self.buffer.block_index(&fallback).is_some());
            let block = if request.options == forge_diff::view::DiffViewOptions::default() {
                Vec::new()
            } else {
                vec![BufferBlock {
                    id: fallback,
                    text: BufferText::from_rows(
                        format!(
                            "Unclassified source changes: {}",
                            self.saved_diagnostic
                                .as_deref()
                                .unwrap_or("complete captured sources are unavailable")
                        )
                        .lines(),
                    )?,
                    metadata: BlockMetadata::default(),
                }]
            };
            let mut buffer = self.buffer.clone();
            let patch = buffer.splice(0..previous, block)?;
            Ok((buffer, patch))
        })();
        let view = self.diff_view.as_mut().expect("validated diff controls");
        let (buffer, patch) = match prepared {
            Ok(prepared) => prepared,
            Err(failure) => {
                view.fail(request);
                return Err(failure);
            }
        };
        ensure!(view.commit(request), "source diff control was superseded");
        self.buffer = buffer;
        Ok(SourceDelivery {
            patch,
            state: self.state.clone(),
            more: self.state == BodyState::Partial,
            diff_view: self.diff_view.as_ref().map(|view| view.committed()),
        })
    }

    fn deliver(&mut self) -> Result<SourceDelivery> {
        self._admission.check()?;
        if self.state != BodyState::Partial {
            return Ok(SourceDelivery {
                diff_view: self.diff_view.as_ref().map(|view| view.committed()),
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
            if !rows.is_empty() && bytes + text.len() > 128 * 1024 {
                break;
            }
            let mut row_metadata = BlockMetadata::default();
            if let Some(syntax) = &self.syntax {
                syntax_decorations(
                    syntax,
                    self.row + rows.len(),
                    rows.len(),
                    text,
                    &mut row_metadata,
                )?;
            }
            if !rows.is_empty()
                && metadata.visible_decoration.len() + row_metadata.visible_decoration.len() > 8192
            {
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
            diff_view: self.diff_view.as_ref().map(|view| view.committed()),
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
    use forge_diff::{cache::CacheLimits, engine::DiffEngine};

    #[tokio::test]
    async fn saved_diff_retains_large_read_only_source_through_bounded_delivery() -> Result<()> {
        let store = Arc::new(RepositoryStore::default());
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let service = SourceDocumentService::new(
            store,
            diff.clone(),
            SyntaxEngine::new(diff.analysis_pool()),
        );
        let text = format!("+{}\n", "x".repeat(254))
            .repeat(70_000)
            .into_bytes();
        assert!(
            service
                .open_text(
                    DocumentId("literal-limit".into()),
                    "message".into(),
                    text.clone()
                )
                .is_ok()
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
        assert_eq!(rows, 70_000);
        assert!(service.close(&id));
        assert!(
            service
                .open_diff_text(
                    DocumentId("diff-limit".into()),
                    "large".into(),
                    vec![b'x'; 8 * 1024 * 1024 + 1]
                )
                .is_ok()
        );
        Ok(())
    }
    #[tokio::test]
    async fn saved_diff_controls_preserve_unclassified_patch_and_independent_options() -> Result<()>
    {
        use forge_diff::view::{DiffPresentation, DiffVisibility};
        let store = Arc::new(RepositoryStore::default());
        let diff = DiffEngine::new(CacheLimits::default(), 2);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let service = SourceDocumentService::new(store, diff.clone(), syntax.clone());
        let id = DocumentId("saved-control".into());
        let text = "--- a/source.rs\n+++ b/source.rs\n@@ -20 +20 @@\n-before();\n+after();\n";
        let opened =
            service.open_diff_text(id.clone(), "Saved diff".into(), text.as_bytes().to_vec())?;
        assert_eq!(
            opened.diff_view.unwrap().presentation,
            DiffPresentation::Full
        );
        assert_eq!(opened.diff_view.unwrap().visibility, DiffVisibility::All);
        let mut frame = opened.snapshot.clone();
        let mut transition = Vec::new();
        for (sequence, action) in [
            "toggle_presentation",
            "toggle_visibility",
            "toggle_presentation",
            "toggle_visibility",
        ]
        .into_iter()
        .enumerate()
        {
            let delivery = service
                .demand(DocumentInput {
                    document: id.clone(),
                    revision: frame.revision,
                    view: ViewId("saved-control-view".into()),
                    sequence: InputSequence(sequence as u64 + 1),
                    action: action.into(),
                    block: frame.block[0].id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                    target: None,
                })
                .await?;
            if let Some(patch) = &delivery.patch {
                patch.validate()?;
            }
            frame = service.snapshot(&id).await?;
            let raw = frame
                .block
                .iter()
                .filter(|block| block.id.0 != "source:diff:fallback")
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n")
                + "\n";
            assert_eq!(raw, text);
            assert_eq!(
                frame
                    .block
                    .iter()
                    .any(|block| block.id.0 == "source:diff:fallback"),
                sequence != 3
            );
            let recovered = service.snapshot_state(&id).await?;
            assert_eq!(recovered.snapshot, frame);
            assert_eq!(recovered.diff_view, delivery.diff_view);
            assert_eq!(recovered.more, delivery.more);
            let options = delivery.diff_view.unwrap();
            assert_eq!(
                options.presentation,
                if sequence < 2 {
                    DiffPresentation::Semantic
                } else {
                    DiffPresentation::Full
                }
            );
            assert_eq!(
                options.visibility,
                if sequence == 1 || sequence == 2 {
                    DiffVisibility::Public
                } else {
                    DiffVisibility::All
                }
            );
            transition
                .push(serde_json::json!({"action":action,"delivery":delivery,"expected":frame}));
        }
        let empty = service.open_diff_text(
            DocumentId("empty-control".into()),
            "Empty diff".into(),
            Vec::new(),
        )?;
        assert_eq!(
            empty.snapshot.block[0].text.row(0),
            Some("No changed files")
        );
        let empty_control = service
            .demand(DocumentInput {
                document: empty.snapshot.document,
                revision: empty.snapshot.revision,
                view: ViewId("empty-view".into()),
                sequence: InputSequence(1),
                action: "toggle_visibility".into(),
                block: empty.snapshot.block[0].id.clone(),
                position: TextPosition { row: 0, column: 0 },
                target: None,
            })
            .await?;
        assert_eq!(
            empty_control.diff_view.unwrap().visibility,
            DiffVisibility::Public
        );
        let plain = service.open_text(
            DocumentId("plain-control".into()),
            "plain".into(),
            b"retained text".to_vec(),
        )?;
        assert!(
            service
                .demand(DocumentInput {
                    document: plain.snapshot.document.clone(),
                    revision: plain.snapshot.revision,
                    view: ViewId("plain-view".into()),
                    sequence: InputSequence(1),
                    action: "toggle_presentation".into(),
                    block: plain.snapshot.block[0].id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                    target: None
                })
                .await
                .is_err()
        );
        assert_eq!(
            service.snapshot(&plain.snapshot.document).await?,
            plain.snapshot
        );
        assert_eq!(syntax.usage().active_jobs, 0);
        assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
        std::fs::write(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("../../target/saved-diff-controls-fixture.json"),
            serde_json::to_vec(&serde_json::json!({"initial":opened,"transition":transition}))?,
        )?;
        Ok(())
    }
}
