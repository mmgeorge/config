use super::*;
use crate::buffer::{
    nodes::{NodeAction, NodeRequest},
    projection::SavedCheckpoint,
};
use forge_buffer::{
    block::{BlockAnchor, BufferBlock, TextPosition},
    identity::{BlockId, FoldId},
    node::{NodeDisplay, NodeKind, NodeState},
};
use forge_diff::{
    display::ChunkLimits,
    engine::DiffEngine,
    saved_projection::{SavedDiffProjection, SavedDiffTarget},
    syntax::SyntaxEngine,
    view::{DiffCapabilities, DiffViewAction, DiffViewRequest, DiffViewState, DiffVisibility},
    workers::{AnalysisPool, WorkBudget, WorkPriority},
};
use std::{
    collections::BTreeMap,
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub(super) struct TimelineDiffStore {
    pub view: DiffViewState,
    pub(super) root: BTreeMap<TargetId, RetainedDiff>,
    operation: Arc<tokio::sync::Mutex<()>>,
}

#[derive(Clone, Eq, PartialEq)]
struct CapturedDiff {
    text: String,
    checkpoint: Option<SavedCheckpoint>,
}

#[derive(Clone)]
pub(super) struct RetainedDiff {
    source: CapturedDiff,
    model: SavedDiffProjection,
    block: Arc<Vec<BufferBlock>>,
    target: BTreeMap<TargetId, SavedDiffTarget>,
}

enum Transition {
    Control(DiffViewRequest),
    Node(Box<NodeRequest>, TargetId, Option<String>),
    Refresh,
}

impl Default for TimelineDiffStore {
    fn default() -> Self {
        Self {
            view: DiffViewState::new(DiffCapabilities::Source, DiffVisibility::All),
            root: BTreeMap::new(),
            operation: Arc::default(),
        }
    }
}

impl CapturedDiff {
    fn matches(&self, action: &TranscriptAction) -> bool {
        matches!(action, TranscriptAction::Diff { text, checkpoint, .. } if text == &self.text && checkpoint == &self.checkpoint)
    }

    fn from_action(action: &TranscriptAction) -> Option<Self> {
        match action {
            TranscriptAction::Diff {
                text, checkpoint, ..
            } => Some(Self {
                text: text.clone(),
                checkpoint: checkpoint.clone(),
            }),
            _ => None,
        }
    }
}

impl TimelineDiffStore {
    pub(super) fn overlay(&self, entry: &mut ProjectedEntry) -> Result<()> {
        for id in entry.action.keys() {
            if let Some(retained) = self.root.get(id) {
                replace_tree(&mut entry.entry.block, id, &retained.block)?;
            }
        }
        Ok(())
    }

    pub(super) fn action(&self, target: &TargetId) -> Option<TranscriptAction> {
        let retained = self
            .root
            .iter()
            .find(|(id, retained)| *id == target || retained.target.contains_key(target))?
            .1;
        Some(TranscriptAction::Diff {
            text: retained.source.text.clone(),
            checkpoint: retained.source.checkpoint.clone(),
            syntax: false,
        })
    }
}

/// Prepare captured timeline sources outside the presentation lock and publish only current work.
/// Non-diff node requests remain owned by the ordinary timeline expansion lifecycle.
pub(crate) async fn update(
    presentation: Arc<Mutex<SessionPresentation>>,
    request: &PresentationRequest,
    data_root: PathBuf,
    diff: Arc<DiffEngine>,
    syntax: Arc<SyntaxEngine>,
) -> Result<Option<serde_json::Value>> {
    let (document, transition, operation) = {
        let mut presentation = presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?;
        match request {
            PresentationRequest::DiffInput { input } => {
                let open = presentation.validate_input(input)?;
                let action = match input.action.as_str() {
                    "toggle_visibility" => DiffViewAction::ToggleVisibility,
                    "toggle_presentation" => DiffViewAction::TogglePresentation,
                    _ => anyhow::bail!("unsupported timeline diff control"),
                };
                let request = open.diff.view.request(action).map_err(|failure| {
                    anyhow::anyhow!("timeline diff control failed: {failure:?}")
                })?;
                (
                    input.document.clone(),
                    Transition::Control(request),
                    open.diff.operation.clone(),
                )
            }
            PresentationRequest::Node { request } => {
                let open =
                    OpenPresentation::document_mut(&mut presentation.open, &request.document)?;
                let Some(root) = node_root(open, &request.node) else {
                    return Ok(None);
                };
                if !open.sections.admit(
                    &request.view,
                    request.sequence,
                    &request.node,
                    request.generation,
                )? {
                    return Ok(Some(serde_json::json!({})));
                }
                request.width.validate()?;
                (
                    request.document.clone(),
                    Transition::Node(
                        Box::new(request.clone()),
                        root,
                        open.action
                            .get(&TargetId(request.node.clone()))
                            .and_then(|action| match action {
                                TranscriptAction::File { path, .. } => Some(path.clone()),
                                _ => None,
                            }),
                    ),
                    open.diff.operation.clone(),
                )
            }
            PresentationRequest::Sync { document, .. }
            | PresentationRequest::Snapshot { document }
            | PresentationRequest::Highlight { document }
            | PresentationRequest::Open { document, .. }
            | PresentationRequest::OpenView { document, .. }
            | PresentationRequest::Resize { document, .. } => {
                let Some(open) = presentation
                    .open
                    .as_ref()
                    .filter(|open| &open.transcript_id == document)
                else {
                    return Ok(None);
                };
                if !open.diff.root.iter().any(|(id, retained)| {
                    open.action
                        .get(id)
                        .is_some_and(|action| !retained.source.matches(action))
                }) {
                    return Ok(None);
                }
                (
                    document.clone(),
                    Transition::Refresh,
                    open.diff.operation.clone(),
                )
            }
            _ => return Ok(None),
        }
    };
    let refreshing = matches!(&transition, Transition::Refresh);
    let (sender, receiver) = tokio::sync::oneshot::channel();
    tokio::spawn(async move {
        let _operation = operation.lock().await;
        let result = async {
            let (options, source) = {
                let mut presentation = presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?;
                let open = OpenPresentation::document_mut(&mut presentation.open, &document)?;
                let options = match &transition {
                    Transition::Control(request) => {
                        if open.diff.view.requested() != *request {
                            return Ok(serde_json::json!({"diff_view":open.diff.view.committed()}));
                        }
                        request.options
                    }
                    Transition::Node(request, _, _) => {
                        ensure!(
                            open.sections.nodes.generation(&request.node)
                                == Some(request.generation),
                            "timeline diff node was retired"
                        );
                        open.diff.view.committed()
                    }
                    Transition::Refresh => open.diff.view.committed(),
                };
                let source = open
                    .action
                    .iter()
                    .filter_map(|(id, action)| {
                        if !matches!(action, TranscriptAction::Diff { .. }) {
                            return None;
                        }
                        let retained = open
                            .diff
                            .root
                            .get(id)
                            .filter(|retained| retained.source.matches(action));
                        let selected = match &transition {
                            Transition::Control(_) => {
                                retained.is_some() || open.sections.expansion(&id.0) == Some(true)
                            }
                            Transition::Node(_, root, _) => root == id,
                            Transition::Refresh => open
                                .diff
                                .root
                                .get(id)
                                .is_some_and(|previous| !previous.source.matches(action)),
                        };
                        selected.then(|| {
                            (
                                id.clone(),
                                CapturedDiff::from_action(action).expect("selected diff action"),
                                retained.cloned(),
                                open.diff
                                    .root
                                    .get(id)
                                    .filter(|previous| !previous.source.matches(action))
                                    .cloned(),
                            )
                        })
                    })
                    .collect::<Vec<_>>();
                (options, source)
            };
            let mut prepared = Vec::new();
            for (id, source, retained, previous) in source {
                let mut retained = match retained {
                    Some(retained) => retained,
                    None => {
                        let captured = source.clone();
                        let previous_model =
                            previous.as_ref().map(|previous| previous.model.clone());
                        let data_root = data_root.clone();
                        let model = worker(diff.analysis_pool(), source.text.len(), move || {
                            let source = crate::buffer::saved_diff::capture(
                                &data_root,
                                captured.checkpoint.as_ref(),
                                captured.text,
                            );
                            let mut model =
                                SavedDiffProjection::new(source).map_err(anyhow::Error::msg)?;
                            model.collapse_files();
                            if let Some(previous) = previous_model {
                                model.inherit_expansion(&previous);
                            }
                            Ok(model)
                        })
                        .await?;
                        RetainedDiff {
                            source,
                            model,
                            block: Arc::default(),
                            target: BTreeMap::new(),
                        }
                    }
                };
                let target = match &transition {
                    Transition::Node(request, _, path) => retained
                        .target
                        .get(&TargetId(request.node.clone()))
                        .cloned()
                        .or_else(|| {
                            path.as_deref()
                                .and_then(|path| retained.model.key_for_path(path))
                                .map(|key| SavedDiffTarget {
                                    file: key.to_owned(),
                                    declaration: None,
                                })
                        }),
                    _ => None,
                };
                let expanded = match &transition {
                    Transition::Node(request, _, _) => {
                        !matches!(request.action, NodeAction::SetExpansion { expanded: false })
                    }
                    _ => false,
                };
                let mut analysis = Vec::new();
                let restore_file = if previous.is_some() {
                    retained
                        .model
                        .expanded_files()
                        .map(str::to_owned)
                        .collect::<HashSet<_>>()
                } else {
                    HashSet::new()
                };
                for key in &restore_file {
                    if let Some(prepared) = retained
                        .model
                        .analyze_file(key, &diff, &syntax, WorkPriority::Visible)
                        .await
                        .map_err(|failure| {
                            anyhow::anyhow!("timeline source analysis failed: {failure:?}")
                        })?
                    {
                        analysis.push(prepared);
                    }
                }
                if expanded
                    && let Some(target) = &target
                    && !restore_file.contains(&target.file)
                    && let Some(prepared) = retained
                        .model
                        .analyze_file(&target.file, &diff, &syntax, WorkPriority::Visible)
                        .await
                        .map_err(|failure| {
                            anyhow::anyhow!("timeline source analysis failed: {failure:?}")
                        })?
                {
                    analysis.push(prepared);
                }
                let prefix = format!("{}:shared", id.0);
                let prepared_root = worker(diff.analysis_pool(), 0, move || {
                    for analysis in analysis {
                        retained.model.install(
                            &analysis.key,
                            analysis.projection,
                            analysis.diagnostic,
                        )?;
                    }
                    let limits = ChunkLimits {
                        rows: 128,
                        ..Default::default()
                    };
                    if retained.model.options() != options {
                        retained.model = retained.model.reconfigure(options, limits)?;
                    }
                    for key in restore_file {
                        retained.model.demand(&key, limits)?;
                    }
                    if let Some(target) = target {
                        retained.model.section(&target, expanded, limits)?;
                    }
                    let frame = retained.model.render(&prefix)?;
                    let mut block = frame.block;
                    native_nodes(&mut block, &id, &frame.target);
                    retained.block = Arc::new(block);
                    retained.target = frame.target;
                    Ok((id, retained))
                })
                .await?;
                prepared.push(prepared_root);
            }
            let mut presentation = presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?;
            let open = OpenPresentation::document_mut(&mut presentation.open, &document)?;
            if let Transition::Control(request) = &transition
                && open.diff.view.requested() != *request
            {
                return Ok(serde_json::json!({"diff_view":open.diff.view.committed()}));
            }
            for (id, retained) in &prepared {
                ensure!(
                    open.action
                        .get(id)
                        .and_then(CapturedDiff::from_action)
                        .as_ref()
                        == Some(&retained.source),
                    "timeline captured comparison changed during preparation"
                );
            }
            let mut entries = Vec::new();
            let mut changed_owner = HashSet::new();
            for (id, _) in &prepared {
                if let Some(owner) = open.transcript.block_owner(&BlockId(id.0.clone())) {
                    changed_owner.insert(owner.to_owned());
                }
            }
            for identity in changed_owner {
                let source = open.transcript.source_entry(&identity)?;
                let mut block = source.block.into_iter().cloned().collect::<Vec<_>>();
                for (id, retained) in &prepared {
                    replace_tree(&mut block, id, &retained.block)?;
                }
                entries.push(super::super::document::TranscriptEntry {
                    id: identity,
                    block,
                });
            }
            let patch = open.transcript.replace_entries(entries)?;
            for (id, retained) in prepared {
                open.diff.root.insert(id, retained);
            }
            let extending = matches!(
                &transition,
                Transition::Node(request, _, _) if matches!(request.action, NodeAction::LoadMore)
            );
            if !extending {
                retain_patches(open, patch)?;
            }
            match &transition {
                Transition::Control(request) => {
                    ensure!(
                        open.diff.view.commit(*request),
                        "timeline diff control was superseded"
                    );
                }
                Transition::Node(request, root, path) => {
                    let translated = if open
                        .transcript
                        .document
                        .block(&BlockId(request.node.clone()))
                        .is_some()
                    {
                        None
                    } else if let Some(path) = path {
                        let retained = open
                            .diff
                            .root
                            .get(root)
                            .context("timeline diff disappeared")?;
                        let key = retained
                            .model
                            .key_for_path(path)
                            .context("captured file identity is ambiguous")?;
                        retained
                            .block
                            .iter()
                            .find(|block| {
                                block
                                    .metadata
                                    .node
                                    .as_ref()
                                    .is_some_and(|node| node.kind == NodeKind::File)
                                    && retained
                                        .target
                                        .get(&TargetId(block.id.0.clone()))
                                        .is_some_and(|target| target.file == key)
                            })
                            .map(|block| block.id.0.clone())
                    } else {
                        None
                    };
                    let node = translated.as_deref().unwrap_or(&request.node);
                    let (expanded, more) = match request.action {
                        NodeAction::SetExpansion { expanded } => (expanded, false),
                        NodeAction::LoadMore => (true, true),
                        NodeAction::RetryLoading => (true, false),
                    };
                    open.sections.set(
                        &mut open.transcript,
                        crate::buffer::sections::SectionRequest {
                            view: request.view.clone(),
                            sequence: request.sequence,
                            id: node,
                            expanded,
                            more,
                            page: Some((request.rows, request.width.clone())),
                        },
                    )?;
                    retain_patches(open, Vec::new())?;
                }
                Transition::Refresh => {}
            }
            Ok(serde_json::json!({"diff_view":open.diff.view.committed()}))
        }
        .await;
        if result.is_err()
            && let Transition::Control(request) = transition
            && let Ok(mut presentation) = presentation.lock()
            && let Some(open) = presentation
                .open
                .as_mut()
                .filter(|open| open.transcript_id == document)
        {
            open.diff.view.fail(request);
        }
        let _ = sender.send(result);
    });
    let result = receiver
        .await
        .context("timeline diff preparation task ended")??;
    Ok((!refreshing).then_some(result))
}

fn node_root(open: &OpenPresentation, node: &str) -> Option<TargetId> {
    if let Some((id, _)) = open
        .diff
        .root
        .iter()
        .find(|(_, retained)| retained.target.contains_key(&TargetId(node.into())))
    {
        return Some(id.clone());
    }
    let id = TargetId(node.into());
    if matches!(open.action.get(&id), Some(TranscriptAction::Diff { .. })) {
        return Some(id);
    }
    let position = open
        .transcript
        .document
        .block_index(&BlockId(node.into()))?;
    open.action
        .iter()
        .filter_map(|(id, action)| {
            if !matches!(action, TranscriptAction::Diff { .. }) {
                return None;
            }
            let start = open
                .transcript
                .document
                .block_index(&BlockId(id.0.clone()))?;
            let heading = open.transcript.document.block(&BlockId(id.0.clone()))?;
            let end = heading
                .metadata
                .fold
                .iter()
                .find(|fold| fold.id.0 == id.0)
                .and_then(|fold| open.transcript.document.block_index(&fold.end.block))?;
            (start <= position && position <= end).then_some((start, id.clone()))
        })
        .max_by_key(|(start, _)| *start)
        .map(|(_, id)| id)
}

pub(super) fn captured_file_action(
    open: &OpenPresentation,
    block: &BlockId,
) -> Option<TranscriptAction> {
    let root = node_root(open, &block.0)?;
    open.action.get(&root).cloned()
}

fn native_nodes(
    block: &mut Vec<BufferBlock>,
    root: &TargetId,
    target: &BTreeMap<TargetId, SavedDiffTarget>,
) {
    // Native sections own continuation markers and compare the final materialized source row.
    block.retain(|block| !block.id.0.ends_with(":more"));
    let position = block
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect::<HashMap<_, _>>();
    let mut parent = Vec::<(usize, FoldId)>::new();
    for (index, block) in block.iter_mut().enumerate() {
        while parent.last().is_some_and(|(end, _)| *end <= index) {
            parent.pop();
        }
        let section = block.metadata.section.first().cloned();
        let descriptor = section
            .as_ref()
            .map(|section| (section.id.clone(), section.open, section.more))
            .or_else(|| {
                block
                    .metadata
                    .fold
                    .first()
                    .map(|fold| (fold.id.clone(), !fold.closed, false))
            });
        if let Some((id, expanded, more)) = descriptor {
            let kind = match target.get(&TargetId(id.0.clone())) {
                Some(target) if target.declaration.is_some() => NodeKind::Group,
                Some(_) => NodeKind::File,
                None => NodeKind::Hunk,
            };
            let mut node = NodeState::new(
                id.clone(),
                kind,
                if expanded {
                    NodeDisplay::Full
                } else {
                    NodeDisplay::Heading
                },
            );
            node.parent = Some(
                parent
                    .last()
                    .map_or_else(|| FoldId(root.0.clone()), |(_, id)| id.clone()),
            );
            node.more = more;
            block.metadata.node = Some(node);
            if let Some(end) = block
                .metadata
                .fold
                .first()
                .and_then(|fold| position.get(&fold.end.block))
            {
                parent.push((*end, id));
            }
        }
        block.metadata.section.clear();
    }
}

fn replace_tree(
    block: &mut Vec<BufferBlock>,
    id: &TargetId,
    replacement: &[BufferBlock],
) -> Result<()> {
    let Some(start) = block.iter().position(|block| block.id.0 == id.0) else {
        return Ok(());
    };
    let Some(fold) = block[start]
        .metadata
        .fold
        .iter()
        .find(|fold| fold.id.0 == id.0)
    else {
        return Ok(());
    };
    let end = block
        .iter()
        .position(|block| block.id == fold.end.block)
        .context("timeline diff boundary disappeared")?;
    ensure!(end >= start, "timeline diff boundary precedes its heading");
    let removed = block[start + 1..=end]
        .iter()
        .map(|block| block.id.clone())
        .collect::<HashSet<_>>();
    let boundary = BufferBlock {
        id: BlockId(format!("{}:shared:boundary", id.0)),
        text: Default::default(),
        metadata: Default::default(),
    };
    let anchor = BlockAnchor {
        block: boundary.id.clone(),
        position: TextPosition { row: 0, column: 0 },
    };
    block.splice(
        start + 1..=end,
        replacement.iter().cloned().chain(std::iter::once(boundary)),
    );
    let replacement_end = start + replacement.len() + 1;
    for (index, block) in block.iter_mut().enumerate() {
        if index > start && index <= replacement_end {
            continue;
        }
        for fold in &mut block.metadata.fold {
            if removed.contains(&fold.end.block) {
                fold.end = anchor.clone();
            }
        }
    }
    Ok(())
}

async fn worker<Value: Send + 'static>(
    pool: Arc<AnalysisPool>,
    bytes: usize,
    operation: impl FnOnce() -> Result<Value> + Send + 'static,
) -> Result<Value> {
    let permit = pool
        .reserve(WorkPriority::Visible, WorkBudget::new(bytes, None))
        .map_err(|failure| anyhow::anyhow!("timeline projection admission failed: {failure:?}"))?;
    let ticket = permit.ticket();
    let (sender, receiver) = tokio::sync::oneshot::channel();
    permit.submit(move |_| {
        let _ = sender.send(operation());
    });
    let result = receiver.await.context("timeline projection worker ended");
    ticket.completed().await;
    result?
}
