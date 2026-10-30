use std::{collections::HashMap, sync::Arc, time::Instant};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{TargetRange, TextPosition, TextRange},
    document::BufferDocument,
    identity::{BlockId, DocumentId, DocumentRevision, TargetId},
};
use forge_diff::{
    cache::AnalysisHandle,
    display::ChunkLimits,
    raw::RawHunkId,
    source::{SourceCoordinate, SourceSide},
    source_projection::{SourceDiffElement, SourceDiffProjection},
    syntax::SyntaxHandle,
    view::{DiffPresentation, DiffViewAction, DiffViewOptions, DiffViewRequest},
    workers::{WorkBudget, WorkPriority},
};

use crate::{
    BodyDelivery, BodyState, StatusInput, StatusService, StatusUpdate,
    document::{DisplayGroupTarget, FileTarget, HunkTarget, StatusDocument},
    mutation::{SelectionVersion, remember},
};

struct SourcePreparation {
    target: TargetId,
    generation: u64,
    base: Option<DocumentRevision>,
    analysis: AnalysisHandle,
    old: Option<SyntaxHandle>,
    new: Option<SyntaxHandle>,
    projection: Option<SourceDiffProjection>,
    body: BufferDocument,
    raw_target: HashMap<RawHunkId, TargetId>,
    group_target: HashMap<Vec<RawHunkId>, DisplayGroupTarget>,
    hunks_closed: bool,
}

struct PreparedSource {
    target: TargetId,
    generation: u64,
    base: Option<DocumentRevision>,
    projection: SourceDiffProjection,
    body: BufferDocument,
    delivery: BodyDelivery,
    hunk: HashMap<TargetId, HunkTarget>,
    raw_target: HashMap<RawHunkId, TargetId>,
    group: HashMap<usize, DisplayGroupTarget>,
    source: HashMap<BlockId, Vec<Option<SourceCoordinate>>>,
    retained: usize,
}

struct DiffOwner {
    id: DocumentId,
    owner: Arc<tokio::sync::Mutex<StatusDocument>>,
    request: DiffViewRequest,
}

pub(crate) enum SourceViewChange {
    Options(DiffViewOptions),
    Restore {
        options: DiffViewOptions,
        rows: usize,
    },
    Demand,
    Declaration {
        identity: String,
        expanded: bool,
    },
}

impl StatusService {
    /// Publish one document's controls with all prepared source bodies.
    pub async fn change_diff_view(&self, input: StatusInput) -> Result<Option<StatusUpdate>> {
        Ok(self
            .change_diff_views(input, Vec::new())
            .await?
            .into_iter()
            .next())
    }

    /// Commit linked inline comparisons with their owning status view in one native transaction.
    pub async fn change_diff_views(
        &self,
        input: StatusInput,
        related: Vec<DocumentId>,
    ) -> Result<Vec<StatusUpdate>> {
        let action = match input.action.as_str() {
            "toggle_visibility" => DiffViewAction::ToggleVisibility,
            "toggle_presentation" => DiffViewAction::TogglePresentation,
            _ => anyhow::bail!("unknown diff control"),
        };
        let parent = self.get(&input.document)?;
        let mut selected = Vec::new();
        for id in related {
            if id != input.document && !selected.iter().any(|(previous, _)| previous == &id) {
                selected.push((id.clone(), self.get(&id)?));
            }
        }
        let repository = parent.lock().await.repository.identity.worktree.clone();
        for (_, owner) in &selected {
            let related = owner.lock().await;
            ensure!(
                related.repository.identity.worktree == repository,
                "linked diff belongs to another worktree"
            );
            ensure!(
                related.comparison.is_some(),
                "linked diff is not a read-only comparison"
            );
        }
        let request = {
            let mut document = parent.lock().await;
            crate::service::resolve_position(&document, &input)?;
            let request = document
                .diff_view
                .request(action)
                .map_err(|error| anyhow::anyhow!("diff control failed: {error:?}"))?;
            document.input.insert(input.view.clone(), input.sequence);
            request
        };
        let mut owner = vec![DiffOwner {
            id: input.document.clone(),
            owner: parent,
            request,
        }];
        for (id, related) in selected {
            let result = {
                let mut document = related.lock().await;
                if document.repository.identity.worktree != repository {
                    Err(anyhow::anyhow!("linked diff belongs to another worktree"))
                } else {
                    document
                        .diff_view
                        .request_options(request.options)
                        .map_err(|error| anyhow::anyhow!("linked diff control failed: {error:?}"))
                }
            };
            match result {
                Ok(request) => owner.push(DiffOwner {
                    id,
                    owner: related,
                    request,
                }),
                Err(error) => {
                    for entry in owner {
                        entry.owner.lock().await.diff_view.fail(entry.request);
                    }
                    return Err(error);
                }
            }
        }
        owner.sort_by(|left, right| left.id.0.cmp(&right.id.0));
        let service = self.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        tokio::spawn(async move {
            let started = Instant::now();
            let result = async {
                loop {
                    let mut revision = Vec::with_capacity(owner.len());
                    let mut prepared = Vec::with_capacity(owner.len());
                    for entry in &owner {
                        let (current, work) = {
                            let document = entry.owner.lock().await;
                            document._admission.check()?;
                            if document.diff_view.requested() != entry.request {
                                return Ok(Vec::new());
                            }
                            let work = document
                                .file
                                .iter()
                                .filter(|(_, file)| file.projection.analysis().is_some())
                                .map(|(target, file)| {
                                    SourcePreparation::capture(&document, target.clone(), file)
                                })
                                .collect::<Result<Vec<_>>>()?;
                            (document.revision, work)
                        };
                        revision.push(current);
                        let mut source = Vec::with_capacity(work.len());
                        for work in work {
                            source.push(
                                service
                                    .prepare_source_view(
                                        &entry.id,
                                        work,
                                        SourceViewChange::Options(entry.request.options),
                                    )
                                    .await?,
                            );
                        }
                        prepared.push(source);
                    }
                    let mut document = Vec::with_capacity(owner.len());
                    for entry in &owner {
                        document.push(entry.owner.lock().await);
                    }
                    for (index, current) in document.iter().enumerate() {
                        current._admission.check()?;
                        if current.diff_view.requested() != owner[index].request {
                            return Ok(Vec::new());
                        }
                    }
                    if document.iter().enumerate().any(|(index, current)| {
                        current.revision != revision[index]
                            || prepared[index]
                                .iter()
                                .any(|source| !source.current(current))
                    }) {
                        continue;
                    }
                    let next = document
                        .iter()
                        .map(|document| document.revision.next())
                        .collect::<Result<Vec<_>, _>>()?;
                    let mut update = Vec::with_capacity(document.len());
                    for (index, (current, source)) in document.iter_mut().zip(prepared).enumerate()
                    {
                        let before = current.snapshot();
                        let version = SelectionVersion::capture(current);
                        let body = source
                            .into_iter()
                            .map(|source| source.adopt(current))
                            .collect();
                        assert!(
                            current.diff_view.commit(owner[index].request),
                            "validated diff request"
                        );
                        current.revision = next[index];
                        remember(current, version);
                        update.push(StatusUpdate {
                            document: current.id.clone(),
                            delta: crate::reconcile::delta(&before, &current.snapshot()),
                            body,
                            operation_id: 0,
                            phase: "diff",
                            diagnostic: Vec::new(),
                            elapsed_us: started.elapsed().as_micros(),
                            resync: false,
                        });
                    }
                    return Ok(update);
                }
            }
            .await;
            if result.is_err() {
                for entry in &owner {
                    entry.owner.lock().await.diff_view.fail(entry.request);
                }
            }
            let _ = sender.send(result);
        });
        receiver.await.context("status diff control task ended")?
    }

    /// Resolve expansion from the captured native declaration target.
    pub async fn source_section(&self, input: StatusInput) -> Result<BodyDelivery> {
        let expanded = match input.action.as_str() {
            "source_expand" => true,
            "source_collapse" => false,
            _ => anyhow::bail!("unknown source section action"),
        };
        let owner = self.get(&input.document)?;
        let (target, identity) = {
            let mut document = owner.lock().await;
            let target = crate::service::resolve_position(&document, &input)?
                .context("source section target is missing")?;
            let target = document
                .hunk
                .get(&target)
                .context("source section expired")?;
            let identity = target
                .declaration
                .clone()
                .context("target is not a semantic declaration")?;
            let target = target.file.clone();
            document.input.insert(input.view.clone(), input.sequence);
            (target, identity)
        };
        self.demand_source_view(
            &input.document,
            &target,
            SourceViewChange::Declaration { identity, expanded },
        )
        .await
    }

    pub(crate) async fn render_source_file(
        &self,
        document: &mut StatusDocument,
        target: &TargetId,
        file: &mut FileTarget,
        rows: usize,
    ) -> Result<BodyDelivery> {
        let mut diagnostic = Vec::new();
        if let (Some(language), Some(analysis)) = (
            crate::source::language(&file.record.path),
            file.projection.analysis(),
        ) {
            let parse = |source, cached: Option<SyntaxHandle>| async move {
                if let Some(cached) = cached {
                    (Some(cached), None)
                } else {
                    crate::service::optional_syntax(
                        &self.syntax,
                        forge_diff::syntax::SyntaxRequest {
                            source,
                            language,
                            priority: WorkPriority::Visible,
                            deadline: None,
                        },
                    )
                    .await
                }
            };
            let ((old, old_diagnostic), (new, new_diagnostic)) = tokio::join!(
                parse(analysis.source().old.clone(), file.old_syntax.clone()),
                parse(analysis.source().new.clone(), file.new_syntax.clone())
            );
            file.old_syntax = old;
            file.new_syntax = new;
            diagnostic.extend(old_diagnostic);
            diagnostic.extend(new_diagnostic);
            file.context_requested = true;
        }
        let analysis = file
            .projection
            .analysis()
            .cloned()
            .context("restored source analysis is unavailable")?;
        ensure!(
            file.body.is_none(),
            "source restoration requires a reset body"
        );
        file.source_view = Some(match file.source_view.take() {
            Some(previous) => {
                previous.replace_source(analysis, file.old_syntax.clone(), file.new_syntax.clone())
            }
            None => SourceDiffProjection::new(
                analysis,
                file.old_syntax.clone(),
                file.new_syntax.clone(),
                Default::default(),
            ),
        });
        let work = SourcePreparation::capture(document, target.clone(), file)?;
        let prepared = self
            .prepare_source_view(
                &document.id,
                work,
                SourceViewChange::Restore {
                    options: document.diff_view.committed(),
                    rows,
                },
            )
            .await?;
        let mut delivery = prepared.adopt_file(document, file);
        if !diagnostic.is_empty() {
            delivery.syntax_diagnostic = Some(diagnostic.join("\n"));
        }
        Ok(delivery)
    }

    pub(crate) async fn demand_source_view(
        &self,
        document_id: &DocumentId,
        target: &TargetId,
        change: SourceViewChange,
    ) -> Result<BodyDelivery> {
        let owner = self.get(document_id)?;
        let (revision, options, work) = {
            let document = owner.lock().await;
            let file = document
                .file
                .get(target)
                .context("source file disappeared")?;
            (
                document.revision,
                document.diff_view.committed(),
                SourcePreparation::capture(&document, target.clone(), file)?,
            )
        };
        let mut work = work;
        if work
            .projection
            .as_ref()
            .is_none_or(|projection| projection.options() != options)
        {
            let projection = work.projection.take().unwrap_or_else(|| {
                SourceDiffProjection::new(
                    work.analysis.clone(),
                    work.old.clone(),
                    work.new.clone(),
                    Default::default(),
                )
            });
            work.projection = Some(projection);
            let prepared = self
                .prepare_source_view(document_id, work, SourceViewChange::Options(options))
                .await?;
            let mut document = owner.lock().await;
            ensure!(
                document.revision == revision && prepared.current(&document),
                "source demand was superseded"
            );
            return Ok(prepared.adopt(&mut document));
        }
        let prepared = self.prepare_source_view(document_id, work, change).await?;
        let mut document = owner.lock().await;
        document._admission.check()?;
        ensure!(
            document.revision == revision && prepared.current(&document),
            "source demand was superseded"
        );
        Ok(prepared.adopt(&mut document))
    }

    async fn prepare_source_view(
        &self,
        document: &DocumentId,
        work: SourcePreparation,
        change: SourceViewChange,
    ) -> Result<PreparedSource> {
        let document = document.clone();
        let retained =
            work.analysis.retained_source_bytes() + work.analysis.retained_result_bytes();
        let permit = self
            .diff
            .analysis_pool()
            .reserve(WorkPriority::Visible, WorkBudget::new(retained, None))
            .map_err(|error| {
                anyhow::anyhow!("status source presentation admission failed: {error:?}")
            })?;
        let ticket = permit.ticket();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        permit.submit(move |_| {
            let _ = sender.send(work.prepare(document, change));
        });
        let result = receiver
            .await
            .context("status source presentation worker ended");
        ticket.completed().await;
        result?
    }
}

impl SourcePreparation {
    fn capture(document: &StatusDocument, target: TargetId, file: &FileTarget) -> Result<Self> {
        let analysis = file
            .projection
            .analysis()
            .cloned()
            .context("source analysis is not acquired")?;
        let projection = file
            .source_view
            .as_ref()
            .filter(|projection| {
                projection.analysis().source().old.identity() == analysis.source().old.identity()
                    && projection.analysis().source().new.identity()
                        == analysis.source().new.identity()
            })
            .cloned();
        let group_target = file
            .group
            .values()
            .filter_map(|group| {
                let action = document.hunk.get(&group.target)?;
                action
                    .declaration
                    .is_none()
                    .then(|| (action.raw.clone(), group.clone()))
            })
            .collect();
        Ok(Self {
            raw_target: file.hunk.clone(),
            group_target,
            hunks_closed: document.local_path.is_some()
                && file.section == crate::StatusSection::Staged,
            target,
            generation: file.generation,
            base: file.body.as_ref().map(BufferDocument::revision),
            analysis,
            old: file.old_syntax.clone(),
            new: file.new_syntax.clone(),
            projection,
            body: match &file.body {
                Some(body) => body.clone(),
                None => BufferDocument::new(
                    DocumentId(format!("body:{}:{}", file.id, file.generation)),
                    Vec::new(),
                )?,
            },
        })
    }

    fn prepare(self, document: DocumentId, change: SourceViewChange) -> Result<PreparedSource> {
        let mut projection = self.projection.unwrap_or_else(|| {
            SourceDiffProjection::new(self.analysis, self.old, self.new, Default::default())
        });
        let limits = ChunkLimits {
            rows: 128,
            ..Default::default()
        };
        let restored_rows = match &change {
            SourceViewChange::Restore { rows, .. } => *rows,
            _ => 0,
        };
        match change {
            SourceViewChange::Options(options) | SourceViewChange::Restore { options, .. } => {
                projection = projection.reconfigure(options)?;
                if !projection.materialized() {
                    projection.demand(limits)?;
                }
            }
            SourceViewChange::Demand => projection.demand(limits)?,
            SourceViewChange::Declaration { identity, expanded } => {
                projection.declaration(&identity, expanded, limits)?
            }
        }
        while projection.options().presentation == DiffPresentation::Full
            && projection.retained_source_rows() < restored_rows
            && projection.more()
        {
            projection.demand(limits)?;
        }
        let file = self
            .target
            .0
            .strip_prefix("file:")
            .context("invalid source file target")?
            .parse::<u64>()?;
        let mut hunk = HashMap::new();
        let mut raw_target = self.raw_target;
        let raw_counts = projection
            .analysis()
            .hunks()
            .iter()
            .map(|hunk| (hunk.id, (hunk.new_lines.len(), hunk.old_lines.len())))
            .collect::<HashMap<_, _>>();
        let mut projected_group = HashMap::new();
        let mut source = HashMap::new();
        let mut group = Vec::new();
        let mut group_action = None;
        let mut declaration = HashMap::new();
        let prefix = format!("status:{file}");
        let mut blocks = projection.render(&prefix, |element, block| {
            let group_heading = matches!(&element, SourceDiffElement::Hunk(_));
            let raw_identity = match &element {
                SourceDiffElement::Row(row) => row.raw_id,
                _ => None,
            };
            let owner = block
                .metadata
                .content_node
                .as_ref()
                .and_then(|owner| declaration.get(owner))
                .cloned();
            let (raw, scope, coordinate) = match element {
                SourceDiffElement::Hunk(display) => {
                    let (added, removed) = display
                        .raw_ids
                        .iter()
                        .filter_map(|identity| raw_counts.get(identity))
                        .fold((0, 0), |(added, removed), (next_added, next_removed)| {
                            (added + next_added, removed + next_removed)
                        });
                    if owner.is_none()
                        && let Some(previous) = self.group_target.get(&display.raw_ids)
                    {
                        block.id = previous.header.clone();
                    }
                    let content_node = block.metadata.content_node.clone();
                    let source_anchor = block.metadata.source_anchor.clone();
                    *block = forge_diff::presentation::DiffPresenter::new(Default::default())
                        .hunk(
                            block.id.clone(),
                            display,
                            forge_diff::presentation::HunkHeading::Counts { added, removed },
                        )?;
                    block.metadata.content_node = content_node;
                    block.metadata.source_anchor = source_anchor;
                    group = display.raw_ids.clone();
                    let coordinate = if display.new_lines.is_empty() {
                        SourceCoordinate {
                            side: SourceSide::Old,
                            line: display.old_lines.start,
                            byte_column: 0,
                        }
                    } else {
                        SourceCoordinate {
                            side: SourceSide::New,
                            line: display.new_lines.start,
                            byte_column: 0,
                        }
                    };
                    (group.clone(), owner, Some(coordinate))
                }
                SourceDiffElement::Row(row) => {
                    if row.is_gap() {
                        return Ok(());
                    }
                    (
                        row.raw_id
                            .map_or_else(|| group.clone(), |identity| vec![identity]),
                        owner,
                        row.new_coordinate().or(row.old_coordinate()),
                    )
                }
                SourceDiffElement::Declaration(signature) => {
                    if let Some(owner) = &block.metadata.content_node {
                        declaration.insert(owner.clone(), signature.identity.clone());
                    }
                    (
                        projection.declaration_hunks(&signature.identity)?,
                        Some(signature.identity.clone()),
                        Some(signature.origin),
                    )
                }
            };
            if !raw.is_empty() {
                let target = if scope.is_none()
                    && let Some(identity) = raw_identity
                {
                    raw_target
                        .entry(identity)
                        .or_insert_with(|| TargetId(format!("action:{}", block.id.0)))
                        .clone()
                } else if scope.is_none() && group_heading {
                    self.group_target.get(&raw).map_or_else(
                        || TargetId(format!("action:{}", block.id.0)),
                        |group| group.target.clone(),
                    )
                } else if scope.is_none() {
                    group_action
                        .clone()
                        .unwrap_or_else(|| TargetId(format!("action:{}", block.id.0)))
                } else {
                    TargetId(format!("action:{}", block.id.0))
                };
                if group_heading {
                    group_action = Some(target.clone());
                    projected_group.insert(
                        projected_group.len(),
                        DisplayGroupTarget {
                            header: block.id.clone(),
                            target: target.clone(),
                        },
                    );
                }

                hunk.insert(
                    target.clone(),
                    HunkTarget {
                        file: self.target.clone(),
                        raw,
                        declaration: scope,
                    },
                );
                block.metadata.target.push(TargetRange {
                    id: target,
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: block.text.row_count(),
                            column: 0,
                        },
                    },
                });
            }
            source.insert(
                block.id.clone(),
                (0..block.text.row_count())
                    .map(|row| {
                        coordinate.map(|coordinate| SourceCoordinate {
                            line: coordinate.line + row,
                            ..coordinate
                        })
                    })
                    .collect(),
            );
            Ok(())
        })?;
        if self.hunks_closed && projection.options().presentation == DiffPresentation::Full {
            for block in &mut blocks {
                for fold in &mut block.metadata.fold {
                    fold.closed = true;
                }
            }
        }
        let retained = blocks
            .iter()
            .map(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .map(|row| row.len() + 1)
                    .sum::<usize>()
                    + (block.metadata.target.len()
                        + block.metadata.decoration.len()
                        + block.metadata.visible_decoration.len()
                        + block.metadata.fold.len())
                        * 192
                    + block.metadata.gutter.len() * 128
                    + 256
            })
            .sum::<usize>();
        let mut body = self.body;
        let count = body.block_count();
        let patch = body.splice(0..count, blocks)?;
        let first = self.base.is_none() || self.base == Some(DocumentRevision(0));
        let more = projection.more();
        let delivery = BodyDelivery {
            document,
            file,
            generation: self.generation,
            snapshot: first.then(|| body.snapshot()),
            patch: if first { None } else { patch },
            state: if more {
                BodyState::Partial
            } else {
                BodyState::Ready
            },
            more,
            syntax_diagnostic: None,
        };
        Ok(PreparedSource {
            target: self.target,
            generation: self.generation,
            base: self.base,
            projection,
            body,
            delivery,
            hunk,
            raw_target,
            group: projected_group,
            source,
            retained,
        })
    }
}

impl PreparedSource {
    fn current(&self, document: &StatusDocument) -> bool {
        document.file.get(&self.target).is_some_and(|file| {
            file.generation == self.generation
                && file.body.as_ref().map(BufferDocument::revision) == self.base
                && file.projection.analysis().is_some_and(|analysis| {
                    analysis.source().old.identity()
                        == self.projection.analysis().source().old.identity()
                        && analysis.source().new.identity()
                            == self.projection.analysis().source().new.identity()
                })
        })
    }

    fn adopt(self, document: &mut StatusDocument) -> BodyDelivery {
        let target = self.target.clone();
        let mut file = document
            .file
            .remove(&target)
            .expect("validated source file");
        let delivery = self.adopt_file(document, &mut file);
        document.file.insert(target, file);
        delivery
    }

    fn adopt_file(self, document: &mut StatusDocument, file: &mut FileTarget) -> BodyDelivery {
        document.retained_bytes =
            document.retained_bytes.saturating_sub(file.retained_bytes) + self.retained;
        file.retained_bytes = self.retained;
        file.body = Some(self.body);
        file.source_view = Some(self.projection);
        file.source_row = self.source;
        file.hunk = self.raw_target;
        file.group = self.group;
        if let crate::document::SourceProjection::Loaded { complete, .. } = &mut file.projection {
            *complete = !self.delivery.more;
        }
        document.hunk.retain(|_, hunk| hunk.file != self.target);
        document.hunk.extend(self.hunk);
        self.delivery
    }
}
