use crate::inventory::SemInventory;
use anyhow::{Context, Result, ensure};
use forge_buffer::{
    admission::{DocumentAdmission, DocumentAdmissionStore},
    block::{
        BlockAnchor, BufferBlock, Decoration, FoldRange, TargetRange, TextPosition, TextRange,
    },
    document::BufferDocument,
    identity::{BlockId, DocumentId, FoldId, InputSequence, TargetId, ViewId},
    input::DocumentInput,
    markdown::MarkdownRenderer,
    patch::{BufferPatch, BufferSnapshot},
    text::BufferText,
    view::DocumentViews,
    width::WidthProfile,
};
use forge_git::{
    content::{ContentOrigin, ContentSource, WorktreeConversion},
    repository::RepositoryState,
    snapshot::HeadState,
    store::RepositoryStore,
};
use serde::Serialize;
use std::{
    collections::HashMap,
    io::Read,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};
mod review;
pub mod schema;
use schema::Artifact;

pub struct WalkthroughService {
    store: Arc<RepositoryStore>,
    admission: Arc<DocumentAdmissionStore>,
    document: Mutex<HashMap<DocumentId, Arc<Mutex<WalkthroughDocument>>>>,
    closed: AtomicBool,
}
struct WalkthroughDocument {
    admission: Arc<DocumentAdmission>,
    buffer: BufferDocument,
    repository: Arc<RepositoryState>,
    observation: forge_git::repository::RepositorySnapshot,
    artifact: Artifact,
    stale: bool,
    inventory: Option<std::collections::BTreeMap<(String, String), usize>>,
    inventory_diagnostic: Option<String>,
    views: DocumentViews,
    input: HashMap<ViewId, InputSequence>,
    target: HashMap<TargetId, (usize, usize, usize)>,
    annotation: HashMap<DocumentId, WalkthroughAnnotation>,
}
struct WalkthroughAnnotation {
    buffer: BufferDocument,
    content: schema::Annotation,
    anchor: String,
    stale: bool,
    views: DocumentViews,
    review: Option<review::ReviewExcerpt>,
    input: HashMap<ViewId, InputSequence>,
}
#[derive(Serialize)]
pub struct WalkthroughOpen {
    pub snapshot: BufferSnapshot,
    pub stale: bool,
    pub inventory_state: &'static str,
    pub inventory_diagnostic: Option<String>,
}
pub struct WalkthroughChange {
    pub source: forge_diff::source::SourceVersion,
    pub path: forge_git::RepositoryPath,
    pub row: usize,
    pub title: String,
    pub annotation: BufferSnapshot,
    pub stale: bool,
}
impl WalkthroughService {
    pub fn new(store: Arc<RepositoryStore>) -> Self {
        Self {
            store,
            admission: Arc::default(),
            document: Mutex::default(),
            closed: AtomicBool::new(false),
        }
    }
    pub async fn open(
        &self,
        id: DocumentId,
        workspace: PathBuf,
        inventory_enabled: bool,
    ) -> Result<WalkthroughOpen> {
        id.validate()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "Walkthrough service is closed"
        );
        let admission = Arc::new(self.admission.admit(id.clone())?);
        let repository = self
            .store
            .open(workspace)
            .await?
            .context("Walkthrough requires a repository")?;
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("Walkthrough requires a worktree")?;
        let artifact = self
            .store
            .reads
            .submit(schema::MAX_ARTIFACT_BYTES, move |cancellation| {
                cancellation.check()?;
                let path = root.join(".walkthrough.json");
                let metadata = std::fs::symlink_metadata(&path)
                    .context("Walkthrough artifact is unavailable")?;
                ensure!(
                    metadata.is_file() && !metadata.file_type().is_symlink(),
                    "Walkthrough artifact must be a regular file"
                );
                ensure!(
                    metadata.len() <= schema::MAX_ARTIFACT_BYTES as u64,
                    "Walkthrough artifact exceeds 2 MiB"
                );
                let mut bytes = Vec::new();
                std::fs::File::open(path)?
                    .take(schema::MAX_ARTIFACT_BYTES as u64 + 1)
                    .read_to_end(&mut bytes)?;
                cancellation.check()?;
                Artifact::parse(&bytes)
            })?
            .finish()
            .await?;
        admission.check()?;
        let observation = repository.snapshot(&self.store).await?;
        let head = match &observation.head {
            HeadState::Attached { target, .. } | HeadState::Detached { target } => {
                Some(target.to_string())
            }
            _ => None,
        };
        let stale = head.as_deref() != Some(artifact.commit.as_str());
        let (inventory, inventory_diagnostic) = if inventory_enabled {
            let inventory_admission = Arc::clone(&admission);
            let inventory = SemInventory::collect(
                &self.store,
                Arc::clone(&repository),
                &observation.path,
                Arc::new(move || Ok(inventory_admission.check()?)),
            )
            .await;
            admission.check()?;
            match inventory {
                Ok(inventory) => {
                    let mut count = std::collections::BTreeMap::new();
                    for entry in inventory.entry {
                        *count.entry((entry.action, entry.kind)).or_insert(0usize) += 1;
                    }
                    (Some(count), None)
                }
                Err(failure) => (None, Some(format!("{failure:#}"))),
            }
        } else {
            (None, None)
        };
        let mut document = WalkthroughDocument {
            admission,
            buffer: BufferDocument::new(id.clone(), Vec::new())?,
            repository,
            observation,
            artifact,
            stale,
            inventory,
            inventory_diagnostic,
            views: DocumentViews::default(),
            input: HashMap::new(),
            target: HashMap::new(),
            annotation: HashMap::new(),
        };
        let (blocks, targets) = project(&document, &WidthProfile::default())?;
        document.buffer = BufferDocument::new(id.clone(), blocks)?;
        document.target = targets;
        let result = WalkthroughOpen {
            snapshot: document.buffer.snapshot(),
            stale,
            inventory_state: if document.inventory.is_some() {
                "ready"
            } else if document.inventory_diagnostic.is_some() {
                "unavailable"
            } else {
                "disabled"
            },
            inventory_diagnostic: document.inventory_diagnostic.clone(),
        };
        let mut owner = self.document.lock().expect("walkthrough registry");
        document.admission.check()?;
        ensure!(
            !self.closed.load(Ordering::Acquire),
            "Walkthrough service closed before adoption"
        );
        owner.insert(id, Arc::new(Mutex::new(document)));
        Ok(result)
    }
    pub fn snapshot(&self, id: &DocumentId) -> Result<BufferSnapshot> {
        let owner = self.get(id)?;
        let document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        Ok(document.buffer.snapshot())
    }
    pub fn view(
        &self,
        id: &DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let mut document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        let mut views = document.views.clone();
        let changed = views.open(view.clone(), width)?;
        let patch = if changed {
            reflow(&mut document, views.profile().cloned().unwrap_or_default())?
        } else {
            None
        };
        document.views = views;
        document.input.entry(view).or_insert(InputSequence(0));
        Ok(patch)
    }
    pub fn close_view(&self, id: &DocumentId, view: &ViewId) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let mut document = owner.lock().expect("walkthrough document");
        document.input.remove(view);
        let mut views = document.views.clone();
        let changed = views.close(view);
        let patch = if changed {
            reflow(&mut document, views.profile().cloned().unwrap_or_default())?
        } else {
            None
        };
        document.views = views;
        Ok(patch)
    }
    pub async fn close_collected(&self, id: &DocumentId) -> bool {
        let existed = self.admission.cancel(id);
        self.document
            .lock()
            .expect("walkthrough registry")
            .remove(id);
        self.admission.wait_closed(id).await;
        existed
    }
    pub fn close_all(&self) {
        self.closed.store(true, Ordering::Release);
        self.admission.cancel_all();
        self.document.lock().expect("walkthrough registry").clear();
    }
    pub async fn resolve_change(
        &self,
        input: DocumentInput,
        annotation_document: DocumentId,
    ) -> Result<WalkthroughChange> {
        input.validate()?;
        annotation_document.validate()?;
        ensure!(
            input.action == "open",
            "Walkthrough has no Git mutation targets"
        );
        let owner = self.get(&input.document)?;
        let (repository, change, commit, stale, observed, width, step) = {
            let mut document = owner.lock().expect("walkthrough document");
            validate_input(&document, &input)?;
            let target = input
                .target
                .as_ref()
                .context("Walkthrough input has no target")?;
            let &(task, subtask, change) = document
                .target
                .get(target)
                .context("unknown Walkthrough change")?;
            let step = format!("{}.{}.{}", task + 1, subtask + 1, change + 1);
            let change = document.artifact.tasks[task].subtasks[subtask].changes[change].clone();
            document.input.insert(input.view.clone(), input.sequence);
            (
                Arc::clone(&document.repository),
                change,
                document.artifact.commit.clone(),
                document.stale,
                document.observation.path.clone(),
                document.views.profile().cloned().unwrap_or_default(),
                step,
            )
        };
        let (path, row) = change.source()?;
        let worktree_stamp = if !stale && !matches!(change.action, schema::Action::Remove) {
            observed
                .iter()
                .find(|observed| observed.change.path == path)
                .map(|observed| &observed.worktree)
        } else {
            None
        };
        let source = if worktree_stamp.is_some() {
            ContentSource::Worktree {
                path: path.clone(),
                conversion: WorktreeConversion::Raw,
            }
        } else {
            let resolved = forge_git::revision::resolve_file(
                &self.store,
                Arc::clone(&repository),
                commit,
                path.clone(),
            )
            .await?;
            ContentSource::Object(resolved.value.blob)
        };
        let acquired = repository
            .content(
                &self.store,
                forge_git::content::ContentRequest {
                    source,
                    limits: Default::default(),
                    expected: None,
                },
            )
            .await?
            .value;
        let content = match acquired {
            forge_git::content::ContentResult::Ready(content) => content,
            forge_git::content::ContentResult::Failed(failure) => {
                return Err(failure).context("Walkthrough source acquisition failed");
            }
            other => anyhow::bail!("Walkthrough source is unavailable: {other:?}"),
        };
        if let Some(expected) = worktree_stamp {
            ensure!(
                matches!(&content.origin, ContentOrigin::Worktree { stamp, .. } if stamp == expected),
                "Walkthrough source changed after opening; reopen the walkthrough before navigating"
            );
        }
        {
            let document = owner.lock().expect("walkthrough document");
            document.admission.check()?;
            ensure!(
                document.buffer.revision() == input.revision
                    && document.input.get(&input.view) == Some(&input.sequence),
                "Walkthrough source input was superseded"
            );
        }
        let annotation = change
            .annotation
            .context("Walkthrough change lacks annotation")?;
        let anchor = format!(
            "{step} · {}:{} · {:?} {:?}: {}",
            path.display_label(),
            row + 1,
            change.action,
            change.kind,
            change.target
        );
        let blocks = project_annotation(&annotation, &anchor, stale, &width)?;
        let annotation_buffer = BufferDocument::new(annotation_document.clone(), blocks)?;
        let annotation_snapshot = annotation_buffer.snapshot();
        {
            let mut document = owner.lock().expect("walkthrough document");
            document.admission.check()?;
            ensure!(
                document.buffer.revision() == input.revision
                    && document.input.get(&input.view) == Some(&input.sequence),
                "Walkthrough source input was superseded"
            );
            ensure!(
                document.annotation.len() < 8,
                "Close a Walkthrough source before opening another"
            );
            ensure!(
                !document.annotation.contains_key(&annotation_document),
                "Walkthrough annotation already exists"
            );
            document.annotation.insert(
                annotation_document,
                WalkthroughAnnotation {
                    buffer: annotation_buffer,
                    content: annotation,
                    anchor,
                    stale,
                    views: DocumentViews::default(),
                    review: None,
                    input: HashMap::new(),
                },
            );
        }
        Ok(WalkthroughChange {
            source: content.source,
            title: change.target,
            path,
            row,
            annotation: annotation_snapshot,
            stale,
        })
    }
    /// Returns the current read-only annotation snapshot, rejecting closed parents or unknown annotations.
    pub fn annotation_snapshot(
        &self,
        parent: &DocumentId,
        annotation: &DocumentId,
    ) -> Result<BufferSnapshot> {
        let owner = self.get(parent)?;
        let document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        Ok(document
            .annotation
            .get(annotation)
            .context("unknown Walkthrough annotation")?
            .buffer
            .snapshot())
    }
    /// Updates an annotation view's width, or closes the view when `width` is absent.
    ///
    /// Only the first registered view controls reflow. Returns its resulting patch, or `None`
    /// when layout is unchanged. Invalid widths, closed parents, unknown annotations, and
    /// rendering failures leave view ownership unchanged.
    pub fn annotation_view(
        &self,
        parent: &DocumentId,
        annotation: &DocumentId,
        view: ViewId,
        width: Option<WidthProfile>,
    ) -> Result<Option<BufferPatch>> {
        let owner = self.get(parent)?;
        let mut document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        let annotation = document
            .annotation
            .get_mut(annotation)
            .context("unknown Walkthrough annotation")?;
        let mut views = annotation.views.clone();
        let opening = width.is_some();
        let changed = if let Some(width) = width {
            views.open(view.clone(), width)?
        } else {
            views.close(&view)
        };
        let patch = if changed {
            let mut blocks = project_annotation(
                &annotation.content,
                &annotation.anchor,
                annotation.stale,
                &views.profile().cloned().unwrap_or_default(),
            )?;
            if let Some(review) = &annotation.review {
                review.prepend(&mut blocks)?;
            }
            annotation
                .buffer
                .edit_many(vec![forge_buffer::sequence::SequenceEdit {
                    range: 0..annotation.buffer.block_count(),
                    block: blocks,
                }])?
        } else {
            None
        };
        if opening {
            annotation
                .input
                .entry(view.clone())
                .or_insert(InputSequence(0));
        } else {
            annotation.input.remove(&view);
        }
        annotation.views = views;
        Ok(patch)
    }
    /// Releases one annotation and all its views. Repeated closure returns false while the parent is open.
    pub fn close_annotation(&self, parent: &DocumentId, annotation: &DocumentId) -> Result<bool> {
        let owner = self.get(parent)?;
        let mut document = owner.lock().expect("walkthrough document");
        document.admission.check()?;
        Ok(document.annotation.remove(annotation).is_some())
    }
    fn get(&self, id: &DocumentId) -> Result<Arc<Mutex<WalkthroughDocument>>> {
        self.document
            .lock()
            .expect("walkthrough registry")
            .get(id)
            .cloned()
            .context("unknown Walkthrough document")
    }
}
fn project_annotation(
    annotation: &schema::Annotation,
    anchor: &str,
    stale: bool,
    width: &WidthProfile,
) -> Result<Vec<BufferBlock>> {
    let mut blocks = Vec::new();
    let mut inner_width = width.clone();
    inner_width.columns = width.columns.saturating_sub(4).max(1);
    let content_width = if width.columns >= 6 {
        &inner_width
    } else {
        width
    };
    push(
        &mut blocks,
        "annotation:title",
        &format!("# {}", annotation.title),
        content_width,
    )?;
    push(&mut blocks, "annotation:anchor", anchor, content_width)?;
    color_block(blocks.last_mut().unwrap(), "ForgeWalkthroughLocation");
    if stale {
        push(
            &mut blocks,
            "annotation:stale",
            "**Stale source**: this read-only annotation may refer to a different source revision.",
            content_width,
        )?;
    }
    push(
        &mut blocks,
        "annotation:comment",
        &annotation.comment,
        content_width,
    )?;
    if width.columns >= 6 {
        frame_annotation(&mut blocks, width)?;
    }
    Ok(blocks)
}

fn color_block(block: &mut BufferBlock, capture: &str) {
    for row in 0..block.text.row_count() {
        let length = block.text.row(row).unwrap().len();
        if length > 0 {
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition { row, column: 0 },
                    end: TextPosition {
                        row,
                        column: length,
                    },
                },
                capture: capture.into(),
                priority: 140,
            });
        }
    }
}

fn frame_annotation(blocks: &mut Vec<BufferBlock>, width: &WidthProfile) -> Result<()> {
    for block in blocks.iter_mut() {
        let row_count = block.text.row_count();
        for decoration in &mut block.metadata.decoration {
            decoration.range.start.column += "│ ".len();
            if decoration.range.end.row < row_count {
                decoration.range.end.column += "│ ".len();
            }
        }
        let rows = (0..row_count)
            .map(|row| {
                let text = block.text.row(row).unwrap();
                let padding = width.columns.saturating_sub(width.cells(text, 0)? + 4);
                Ok(format!("│ {text}{} │", " ".repeat(padding)))
            })
            .collect::<Result<Vec<_>>>()?;
        block.text = BufferText::from_rows(rows)?;
        for row in 0..row_count {
            let length = block.text.row(row).unwrap().len();
            for (start, end) in [(0, "│".len()), (length - "│".len(), length)] {
                block.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row, column: start },
                        end: TextPosition { row, column: end },
                    },
                    capture: "ForgeReviewCommentHeader".into(),
                    priority: 160,
                });
            }
        }
    }
    let mut edge = Vec::new();
    push(
        &mut edge,
        "annotation:top",
        &format!("╭{}╮", "─".repeat(width.columns - 2)),
        width,
    )?;
    color_block(&mut edge[0], "ForgeReviewCommentHeader");
    blocks.insert(0, edge.remove(0));
    push(
        blocks,
        "annotation:bottom",
        &format!("╰{}╯", "─".repeat(width.columns - 2)),
        width,
    )?;
    color_block(blocks.last_mut().unwrap(), "ForgeReviewCommentHeader");
    Ok(())
}
fn reflow(document: &mut WalkthroughDocument, width: WidthProfile) -> Result<Option<BufferPatch>> {
    let (block, target) = project(document, &width)?;
    let patch = document
        .buffer
        .edit_many(vec![forge_buffer::sequence::SequenceEdit {
            range: 0..document.buffer.block_count(),
            block,
        }])?;
    document.target = target;
    Ok(patch)
}
fn project(
    document: &WalkthroughDocument,
    width: &WidthProfile,
) -> Result<(Vec<BufferBlock>, HashMap<TargetId, (usize, usize, usize)>)> {
    let mut block = Vec::new();
    let mut target = HashMap::new();
    push(
        &mut block,
        "title",
        &format!("# {}", document.artifact.root),
        width,
    )?;
    if document.stale {
        push(
            &mut block,
            "stale",
            &format!(
                "**Stale walkthrough**: captured commit {} differs from current HEAD. Source navigation remains read-only.",
                document.artifact.commit
            ),
            width,
        )?;
    }
    push(&mut block, "overview", &document.artifact.overview, width)?;
    let mut flow = String::from("## Flow\n");
    fn flow_text(nodes: &[schema::Flow], depth: usize, text: &mut String) {
        for node in nodes {
            text.push_str(&format!("{}{}", "  ".repeat(depth), node.text));
            let mut current = node;
            while current.children.len() == 1 {
                current = &current.children[0];
                text.push_str(&format!(" → {}", current.text));
            }
            text.push('\n');
            flow_text(&current.children, depth + 1, text);
        }
    }
    flow_text(&document.artifact.flow, 0, &mut flow);
    push(&mut block, "flow", &flow, width)?;
    match &document.inventory {
        Some(inventory) => {
            let text = inventory
                .iter()
                .map(|((action, kind), count)| {
                    format!(
                        "{kind} {}{count}",
                        match action.as_str() {
                            "added" | "Add" => "+",
                            "removed" | "Remove" => "-",
                            _ => "~",
                        }
                    )
                })
                .collect::<Vec<_>>()
                .join("   ·   ");
            push(
                &mut block,
                "inventory",
                &format!("## Semantic inventory\n{text}"),
                width,
            )?;
            let inventory = block.last_mut().unwrap();
            for row in 0..inventory.text.row_count() {
                let text = inventory.text.row(row).unwrap();
                for (column, character) in text.char_indices() {
                    let capture = match character {
                        '+' => "ForgeWalkthroughActionAdd",
                        '-' => "ForgeWalkthroughActionRemove",
                        '~' => "ForgeWalkthroughActionModify",
                        _ => continue,
                    };
                    let length = text[column + 1..]
                        .bytes()
                        .take_while(u8::is_ascii_digit)
                        .count();
                    if length == 0 {
                        continue;
                    }
                    inventory.metadata.decoration.push(Decoration {
                        range: TextRange {
                            start: TextPosition { row, column },
                            end: TextPosition {
                                row,
                                column: column + length + 1,
                            },
                        },
                        capture: capture.into(),
                        priority: 140,
                    });
                }
            }
        }
        None if document.inventory_diagnostic.is_none() => {}
        None => push(
            &mut block,
            "inventory",
            &format!(
                "## Semantic inventory unavailable\n{}",
                document
                    .inventory_diagnostic
                    .as_deref()
                    .unwrap_or("Sem did not return an inventory")
            ),
            width,
        )?,
    }
    for (task_index, task) in document.artifact.tasks.iter().enumerate() {
        let task_id = format!("task:{task_index}");
        let task_header = block.len();
        push(
            &mut block,
            &task_id,
            &format!("## {}. {}", task_index + 1, task.title),
            width,
        )?;
        color_block(block.last_mut().unwrap(), "ForgeWalkthroughItemTitle");
        if let Some(text) = &task.justification {
            push(&mut block, &format!("{task_id}:why"), text, width)?;
        }
        for (subtask_index, subtask) in task.subtasks.iter().enumerate() {
            let subtask_id = format!("{task_id}:subtask:{subtask_index}");
            let subtask_header = block.len();
            push(
                &mut block,
                &subtask_id,
                &format!(
                    "### {}.{}. {}",
                    task_index + 1,
                    subtask_index + 1,
                    subtask.title
                ),
                width,
            )?;
            color_block(block.last_mut().unwrap(), "ForgeWalkthroughItemTitle");
            if let Some(text) = &subtask.justification {
                push(&mut block, &format!("{subtask_id}:why"), text, width)?;
            }
            for (change_index, change) in subtask.changes.iter().enumerate() {
                let change_id = format!("{subtask_id}:change:{change_index}");
                push(
                    &mut block,
                    &change_id,
                    &format!(
                        "**{}.{}.{} · {:?} {:?}: {}**\n{}{}{}",
                        task_index + 1,
                        subtask_index + 1,
                        change_index + 1,
                        change.action,
                        change.kind,
                        change.target,
                        change
                            .role
                            .as_ref()
                            .map(|role| format!("{role}\n"))
                            .unwrap_or_default(),
                        change.note,
                        if change.annotation.is_some() {
                            let (path, row) = change.source()?;
                            format!(
                                "\n\n{}:{} · open source annotation",
                                path.display_label(),
                                row + 1
                            )
                        } else {
                            String::new()
                        }
                    ),
                    width,
                )?;
                let current = block.last_mut().expect("change block");
                current.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: 0,
                            column: current.text.row(0).unwrap().len(),
                        },
                    },
                    capture: match change.action {
                        schema::Action::Add => "ForgeWalkthroughActionAdd",
                        schema::Action::Modify => "ForgeWalkthroughActionModify",
                        schema::Action::Remove => "ForgeWalkthroughActionRemove",
                    }
                    .into(),
                    priority: 140,
                });
                if change.annotation.is_some() {
                    let current = block.last_mut().expect("change block");
                    let id = TargetId(format!("walkthrough:{change_id}"));
                    current.metadata.target.clear();
                    current.metadata.target.push(TargetRange {
                        id: id.clone(),
                        range: TextRange {
                            start: TextPosition { row: 0, column: 0 },
                            end: TextPosition {
                                row: current.text.row_count(),
                                column: 0,
                            },
                        },
                    });
                    target.insert(id, (task_index, subtask_index, change_index));
                }
            }
            fold(&mut block, subtask_header, &subtask_id, true);
        }
        fold(&mut block, task_header, &task_id, false);
    }
    let retained = schema::MAX_ARTIFACT_BYTES * 3
        + block
            .iter()
            .map(|block| block.text.byte_count() * 2 + 4096 + block.metadata.decoration.len() * 256)
            .sum::<usize>();
    ensure!(
        retained <= 16 * 1024 * 1024,
        "Walkthrough generated document exceeds 16 MiB"
    );
    Ok((block, target))
}
fn push(block: &mut Vec<BufferBlock>, id: &str, source: &str, width: &WidthProfile) -> Result<()> {
    let mut rendered =
        MarkdownRenderer::render(BlockId(format!("walkthrough:{id}")), source, width)?.block;
    rendered.metadata.target.clear();
    block.push(rendered);
    Ok(())
}
fn fold(block: &mut [BufferBlock], header: usize, id: &str, closed: bool) {
    let last = block.last().expect("fold body");
    let end = BlockAnchor {
        block: last.id.clone(),
        position: TextPosition {
            row: last.text.row_count(),
            column: 0,
        },
    };
    let header = &mut block[header];
    header.metadata.fold.push(FoldRange {
        id: FoldId(format!("walkthrough:{id}")),
        start: TextPosition {
            row: header.text.row_count().saturating_sub(1),
            column: 0,
        },
        end,
        closed,
    });
}

fn validate_input(document: &WalkthroughDocument, input: &DocumentInput) -> Result<()> {
    document.admission.check()?;
    ensure!(
        document.buffer.revision() == input.revision,
        "Walkthrough input uses a stale revision"
    );
    let previous = document
        .input
        .get(&input.view)
        .context("Walkthrough input view is not registered")?;
    ensure!(
        *previous < input.sequence,
        "Walkthrough input sequence was superseded"
    );
    let block = document
        .buffer
        .block(&input.block)
        .context("Walkthrough input block is unavailable")?;
    let row = block
        .text
        .row(input.position.row)
        .context("Walkthrough input row is unavailable")?;
    ensure!(
        row.is_char_boundary(input.position.column),
        "Walkthrough input byte position is invalid"
    );
    let target = input
        .target
        .as_ref()
        .context("Walkthrough input lacks target")?;
    ensure!(
        block.metadata.target.iter().any(|range| &range.id == target
            && range.range.start <= input.position
            && input.position < range.range.end),
        "Walkthrough target is not at the captured position"
    );
    Ok(())
}

#[cfg(test)]
mod tests;
