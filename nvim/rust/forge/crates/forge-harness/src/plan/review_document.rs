use super::PlanNavigationAnchor;
use super::review_annotation::{ReviewAnnotation, ReviewAnnotationKind, ReviewAnnotationStore, ReviewQuestionReply};
use super::review_source::PlanReviewSource;
use anyhow::{Context, Result, ensure};
use forge_buffer::admission::{DocumentAdmission, DocumentAdmissionStore};
use forge_buffer::document::BufferDocument;
use forge_buffer::block::TextPosition;
use forge_buffer::identity::{DocumentId, InputSequence, TargetId, ViewId};
use forge_buffer::input::DocumentInput;
use forge_buffer::patch::BufferSnapshot;
use forge_buffer::view::DocumentViews;
use forge_buffer::width::WidthProfile;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Default)]
pub(crate) struct PlanReviewStore {
    admission: Arc<DocumentAdmissionStore>,
    document: Mutex<HashMap<DocumentId, (PlanReviewDocument, DocumentAdmission)>>,
}

/// Captures one unanswered question and the immutable design context used to answer it.
pub(crate) struct PlanReviewQuestion {
    /// Stable identity of the question's annotation.
    pub id: String,
    /// Exact editable text captured at submission.
    pub body: String,
    /// Saved declaration design and anchored review excerpt supplied to the model.
    pub prompt: String,
}

impl PlanReviewStore {
    pub(crate) fn save_annotations(
        &self,
        id: &DocumentId,
        digest: &str,
        annotation: Vec<ReviewAnnotation>,
    ) -> Result<()> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        ensure!(
            !document.source.historical,
            "historical plan revisions are read-only"
        );
        ensure!(
            digest == document.source.saved_digest,
            "saved plan source changed"
        );
        let mut captured = annotation;
        for annotation in &mut captured {
            annotation.reply = document.annotation.annotation().iter()
                .find(|saved| saved.id == annotation.id && saved.kind == annotation.kind
                    && saved.source.body == annotation.source.body)
                .and_then(|saved| saved.reply.clone());
            let start = document
                .source
                .rendered
                .navigation
                .resolve_line(annotation.source.start_line)
                .context("annotation start has no saved target")?;
            let end = document
                .source
                .rendered
                .navigation
                .resolve_line(annotation.source.end_line)
                .context("annotation end has no saved target")?;
            annotation.anchor = document.source.document.design.as_ref().map(|_| {
                super::review_annotation::ReviewAnnotationAnchor {
                    start: start.target.clone(),
                    end: end.target.clone(),
                }
            });
        }
        document.annotation.replace(captured)
    }

    pub(crate) fn submission(&self, input: DocumentInput) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.validate_input(input)?;
        ensure!(
            !document.source.historical,
            "historical plan revisions are read-only"
        );
        Ok(
            serde_json::json!({"plan_id":document.source.document.plan_id,
            "digest":super::digest(&serde_json::to_vec(&document.source.document)?),
            "saved_source_digest":document.source.saved_digest,
            "annotations":document.annotation.annotation().iter().filter(|annotation| annotation.kind == ReviewAnnotationKind::Comment && !annotation.source.body.trim().is_empty())
                .map(|annotation| {
                    let mut source = annotation.source.clone();
                    if annotation.parent_id.is_some() {
                        let thread = super::review_annotation::thread_context(document.annotation.annotation(), annotation);
                        source.body = format!("Conversation context:\n{thread}Requested change:\n{}", source.body);
                    }
                    source
                }).collect::<Vec<_>>() }),
        )
    }

    /// Capture unanswered questions after validating their native document identity.
    pub(crate) fn questions(&self, input: DocumentInput) -> Result<Vec<PlanReviewQuestion>> {
        let mut store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get_mut(&input.document).context("plan review document is closed")?;
        admission.check()?;
        document.validate_input(input)?;
        ensure!(!document.source.historical, "historical plan revisions are read-only");
        let design = document.source.document.design.as_ref().context("plan questions require a declaration design")?;
        let context = serde_json::to_string_pretty(&serde_json::json!({"document":design.document,"proposed":design.proposed}))?;
        let mut question = Vec::new();
        for annotation in document.annotation.annotation().iter().filter(|annotation|
            annotation.kind == ReviewAnnotationKind::Question && annotation.reply.is_none() && !annotation.source.body.trim().is_empty()) {
            let resolved = super::resolve_annotations(&document.source.rendered, vec![annotation.source.clone()])?;
            let excerpt = super::render_review_feedback(&document.source.document, &resolved)?;
            let thread = super::review_annotation::thread_context(document.annotation.annotation(), annotation);
            let prompt = format!("Saved declaration design:\n{context}\n\nPlan review question and source context:\n{excerpt}\n\nConversation thread:\n{thread}Answer this question:\n{}", annotation.source.body);
            ensure!(prompt.len() <= 8 * 1024 * 1024, "plan question context exceeds 8 MiB");
            question.push(PlanReviewQuestion { id:annotation.id.clone(), body:annotation.source.body.clone(), prompt });
        }
        Ok(question)
    }

    /// Persist an answer only while its original question and source snapshot remain current.
    pub(crate) fn answer(&self, id: &DocumentId, question: &PlanReviewQuestion, body: String, duration_ms: u64) -> Result<Vec<ReviewAnnotation>> {
        let mut store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get_mut(id).context("plan review document is closed")?;
        admission.check()?;
        let mut annotation = document.annotation.annotation().to_vec();
        let selected = annotation.iter_mut().find(|annotation| annotation.id == question.id
            && annotation.kind == ReviewAnnotationKind::Question && annotation.source.body == question.body)
            .context("plan question changed before its answer arrived")?;
        selected.reply = Some(ReviewQuestionReply { question_body:question.body.clone(), body, duration_ms: Some(duration_ms) });
        document.annotation.replace(annotation.clone())?;
        Ok(annotation)
    }

    /// Return persisted comments and answers independently of the visible source filter.
    pub(crate) fn annotations(&self, id: &DocumentId) -> Result<Vec<ReviewAnnotation>> {
        let store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get(id).context("plan review document is closed")?;
        admission.check()?;
        Ok(document.annotation.annotation().to_vec())
    }

    pub(crate) fn admit(&self, id: DocumentId) -> Result<DocumentAdmission> {
        Ok(self.admission.admit(id)?)
    }

    pub(crate) fn insert(
        &self,
        document: PlanReviewDocument,
        admission: DocumentAdmission,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        admission.check()?;
        let opened = serde_json::json!({"snapshot":document.snapshot(), "saved_source_digest":document.saved_digest(),
            "plan_id":document.source.document.plan_id, "version":document.source.document.version,
            "path":document.source.path, "public_only":document.source.public_only,
            "annotation":document.annotation.annotation(), "source_row":document.source_rows()});
        store.insert(document.id.clone(), (document, admission));
        Ok(opened)
    }

    /// Resolve snapshot declarations before acquiring external evidence for an unresolved jump.
    pub(crate) async fn prepare_declaration_jump(
        &self,
        input: &DocumentInput,
        trace: Option<&crate::declaration::trace::DeclarationTrace>,
    ) -> Result<()> {
        let captured = {
            let lock_stage = trace.map(|trace| trace.stage("review_lock", None));
            let store = self
                .document
                .lock()
                .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
            if let Some(stage) = lock_stage {
                stage.complete(serde_json::json!({}));
            }
            let (document, admission) = store
                .get(&input.document)
                .context("plan review document is closed")?;
            admission.check()?;
            let target = document
                .check_input(input)?
                .context("plan review input has no source target")?;
            let Some(design) = &document.source.document.design else {
                return Ok(());
            };
            if document.source.resolver.get().is_none() {
                let stage = trace.map(|trace| trace.stage("prepare_local", None));
                let proposed = crate::declaration::DeclarationResolver::local(
                    &document.source.workspace,
                    design,
                    false,
                )?;
                let baseline = crate::declaration::DeclarationResolver::local(
                    &document.source.workspace,
                    design,
                    true,
                )?;
                ensure!(
                    document
                        .source
                        .resolver
                        .set(Mutex::new((proposed, baseline)))
                        .is_ok(),
                    "declaration resolver was already initialized"
                );
                if let Some(stage) = stage {
                    stage.complete(serde_json::json!({}));
                }
            }
            {
                let mut resolver = document
                    .source
                    .resolver
                    .get()
                    .unwrap()
                    .lock()
                    .map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
                resolver.0.trace = trace.cloned();
                resolver.1.trace = trace.cloned();
            }
            let anchor = document
                .target
                .get(&target)
                .context("plan review source target is missing")?;
            let row = document
                .document
                .block(&input.block)
                .and_then(|block| block.text.row(input.position.row))
                .context("plan review input row is missing")?;
            let stage = trace.map(|trace| trace.stage("preflight", None));
            let result = document.resolve_declaration(anchor, row, input.position.column)?;
            if let Some(stage) = stage {
                stage.complete(serde_json::json!({"cached":document.source.resolver_sources.get().is_some(),"resolution":result}));
            }
            if matches!(
                result,
                crate::declaration::DeclarationResolution::Resolved { .. }
                    | crate::declaration::DeclarationResolution::Intrinsic
            ) {
                return Ok(());
            }
            let resolver = document
                .source
                .resolver
                .get()
                .unwrap()
                .lock()
                .map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
            let requires_fetch = (
                resolver.0.requires_source_fetch(),
                resolver.1.requires_source_fetch(),
            );
            if resolver.0.library_sources_checked()
                && resolver.1.library_sources_checked()
                && !requires_fetch.0
                && !requires_fetch.1
            {
                return Ok(());
            }
            (
                Arc::clone(&document.source.resolver),
                Arc::clone(&document.source.resolver_sources),
                design.clone(),
                (resolver.0.clone(), resolver.1.clone()),
                requires_fetch,
            )
        };
        let stage = trace.map(|trace| trace.stage("prepare_external", None));
        let prepare = async {
            let (mut proposed, mut baseline) = captured.3;
            proposed.prepare_sources(&captured.2).await?;
            baseline.prepare_sources(&captured.2).await?;
            *captured
                .0
                .get()
                .context("declaration resolver is unavailable")?
                .lock()
                .map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))? =
                (proposed, baseline);
            Ok::<_, anyhow::Error>(())
        };
        let cargo = captured.4.0 || captured.4.1;
        prepare.await?;
        if cargo {
            let _ = captured.1.set(());
        }
        if let Some(stage) = stage {
            stage.complete(serde_json::json!({"cargo":cargo}));
        }
        Ok(())
    }

    pub(crate) fn action(&self, input: DocumentInput) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        let action = input.action.clone();
        if action == "references" || action == "rename_entity" {
            let column = input.position.column;
            let row = document.document.block(&input.block).and_then(|block| block.text.row(input.position.row)).unwrap_or("").to_owned();
            let anchor = document.action(input)?;
            return if action == "rename_entity" { document.rename(&anchor, &row, column) } else { document.references(&anchor, &row, column) };
        }
        if let Some(id) = action.strip_prefix("reveal_reference:") {
            document.validate_input(input)?;
            return document.reveal_reference(id);
        }
        if action == "schema" {
            document.validate_input(input)?;
            let text = serde_json::to_string_pretty(&document.source.document)?;
            ensure!(text.len() <= 8 * 1024 * 1024, "plan schema exceeds 8 MiB");
            let text = forge_buffer::text::BufferText::from_rows(text.lines())?;
            ensure!(text.row_count() <= 65536, "plan schema exceeds 65536 rows");
            let snapshot = BufferDocument::new(
                DocumentId(format!("plan:schema:{}", uuid::Uuid::new_v4())),
                vec![forge_buffer::block::BufferBlock {
                    id: forge_buffer::identity::BlockId("schema".into()),
                    text,
                    metadata: Default::default(),
                }],
            )?
            .snapshot();
            return Ok(serde_json::json!({"schema":snapshot}));
        }
        if action == "toggle_public" {
            document.validate_input(input)?;
            return document.toggle_public();
        }
        if action == "toggle_declaration" {
            let owner = input.block.clone();
            document.validate_input(input)?;
            return document.toggle_declaration(&owner);
        }
        let column = input.position.column;
        let row = document
            .document
            .block(&input.block)
            .and_then(|block| block.text.row(input.position.row))
            .unwrap_or("")
            .to_owned();
        let anchor = document.action(input)?;
        document.describe(anchor, &action, &row, column)
    }

    pub(crate) fn view(
        &self,
        id: &DocumentId,
        view: ViewId,
        width: Option<WidthProfile>,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        document.update_view(view, width)
    }

    pub(crate) async fn close(&self, id: &DocumentId) -> Result<()> {
        self.admission.cancel(id);
        self.document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?
            .remove(id);
        self.admission.wait_closed(id).await;
        Ok(())
    }

    pub(crate) fn close_all(&self) -> Result<()> {
        self.admission.cancel_all();
        self.document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?
            .clear();
        Ok(())
    }
}

pub(crate) struct PlanReviewDocument {
    reference: Mutex<HashMap<bool, super::references::PlanReferenceIndex>>,
    focused_annotation: Option<String>,
    annotation_revision: HashMap<
        String,
        (
            forge_buffer::identity::RegionRevision,
            forge_buffer::identity::EditSequence,
        ),
    >,
    annotation: ReviewAnnotationStore,
    width: WidthProfile,
    view_width: DocumentViews,
    id: DocumentId,
    source: PlanReviewSource,
    document: BufferDocument,
    target: HashMap<TargetId, PlanNavigationAnchor>,
    view: HashMap<ViewId, InputSequence>,
}

impl PlanReviewDocument {
    fn rename(
        &mut self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        ensure!(
            !self.source.historical,
            "historical plan revisions are read-only"
        );
        ensure!(
            !matches!(&anchor.target, super::PlanReviewTarget::Declaration { side, .. } | super::PlanReviewTarget::Call { side, .. } if side != "proposed"),
            "baseline symbols cannot be renamed"
        );
        let design = self
            .source
            .document
            .design
            .as_ref()
            .context("rename requires a declaration plan")?;
        let index = super::references::PlanReferenceIndex::planned(&self.source.document, &self.source.workspace)?;
        let mut selected_anchor = anchor.clone();
        let saved_column = if let super::PlanReviewTarget::Declaration {
            path,
            line,
            column: saved,
            ..
        } = &anchor.target
        {
            let position = forge_diff::syntax::DeclarationOverview::token_position(
                path,
                &design.proposed[path],
                forge_diff::syntax::DeclarationPosition {
                    line: *line,
                    column: saved.unwrap_or(0),
                },
                row,
                column,
            )
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            if let super::PlanReviewTarget::Declaration { line, .. } = &mut selected_anchor.target {
                *line = position.line;
            }
            position.column
        } else {
            column as u32
        };
        let identity = index
            .selected(&selected_anchor, saved_column)
            .context("select a resolved symbol defined in the plan")?;
        let (_, symbol) =
            index.rename_definition(&self.source.document, identity)?;
        let mut preview = Vec::new();
        let mut resolver =
            crate::declaration::DeclarationResolver::planned(&self.source.workspace, design, false)?;
        resolver.bound_reference_files();
        for block in self.document.snapshot().block {
            for target in &block.metadata.target {
                let Some(anchor) = self.target.get(&target.id) else {
                    continue;
                };
                let Some(row) = block.text.row(0) else {
                    continue;
                };
                for (column, _) in row.match_indices(&symbol.name) {
                    let mut selected = anchor.clone();
                    let saved_column = match &anchor.target {
                        super::PlanReviewTarget::Declaration {
                            path,
                            side,
                            line,
                            column: saved,
                        } if side == "proposed" => {
                            let position = forge_diff::syntax::DeclarationOverview::token_position(
                                path,
                                &design.proposed[path],
                                forge_diff::syntax::DeclarationPosition {
                                    line: *line,
                                    column: saved.unwrap_or(0),
                                },
                                row,
                                column,
                            )
                            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                            if let super::PlanReviewTarget::Declaration { line, .. } =
                                &mut selected.target
                            {
                                *line = position.line;
                            }
                            position.column
                        }
                        super::PlanReviewTarget::Call {
                            path,
                            side,
                            owner,
                            name,
                            kind: _,
                        } if side == "proposed" => {
                            let separator = if path.ends_with(".rs") { "::" } else { "." };
                            let normalized = if separator == "." {
                                name.replace(':', ".")
                            } else {
                                name.clone()
                            };
                            let parts = normalized
                                .split(separator)
                                .map(str::to_owned)
                                .collect::<Vec<_>>();
                            let start = row.find(name).context("Calls row has no target text")?;
                            let mut offset = start;
                            for (position, part) in parts.iter().enumerate() {
                                if offset == column && *part == symbol.name {
                                    let selected = if position + 1 == parts.len() {
                                        index.selected(anchor, column as u32) == Some(identity)
                                    } else {
                                        matches!(resolver.type_target(path, &owner.split(separator).map(str::to_owned).collect::<Vec<_>>(), &parts[..=position]), crate::declaration::DeclarationResolution::Resolved {destination} if format!("{}:{}:{}",destination.path.replace('\\', "/"),destination.line,destination.column) == identity)
                                    };
                                    if selected {
                                        preview.push(serde_json::json!({"block":block.id,"column":column,"length":symbol.name.len()}));
                                    }
                                }
                                offset += part.len() + separator.len();
                            }
                            continue;
                        }
                        _ => continue,
                    };
                    if index.selected(&selected, saved_column) == Some(identity) {
                        preview.push(serde_json::json!({"block":block.id,"column":column,"length":symbol.name.len()}));
                    }
                }
            }
        }
        Ok(
            serde_json::json!({"rename":{"symbol":identity,"name":symbol.name,"expected_version":self.source.document.version,"preview":preview}}),
        )
    }

    fn references(&mut self, anchor: &PlanNavigationAnchor, row: &str, column: usize) -> Result<serde_json::Value> {
        let baseline = matches!(&anchor.target, super::PlanReviewTarget::Declaration { side, .. } | super::PlanReviewTarget::Call { side, .. } if side == "baseline");
        let mut reference = self.reference.lock().map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?;
        if !reference.contains_key(&baseline) {
            reference.insert(baseline, super::references::PlanReferenceIndex::build(&self.source.document, &self.source.workspace, baseline)?);
        }
        let index = reference.get(&baseline).unwrap();
        let mut selected_anchor = anchor.clone();
        let saved_column = if let super::PlanReviewTarget::Declaration { path, side, line, column: saved } = &anchor.target {
            let design = self.source.document.design.as_ref().context("declaration design is unavailable")?;
            let text = if side == "baseline" { design.baseline.get(path).map(|file| &file.text) } else { design.proposed.get(path) }.context("declaration file is unavailable")?;
            let position = forge_diff::syntax::DeclarationOverview::token_position(path, text,
                forge_diff::syntax::DeclarationPosition { line: *line, column: saved.unwrap_or(0) }, row, column)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            if let super::PlanReviewTarget::Declaration { line, .. } = &mut selected_anchor.target { *line = position.line; }
            position.column
        } else { column as u32 };
        let Some(symbol) = index.selected(&selected_anchor, saved_column) else {
            if let super::PlanReviewTarget::Call { path, owner, name, kind, .. } = &anchor.target {
                if let Some(reason) = index.unresolved.get(&(path.clone(), owner.clone(), name.clone(), *kind)) { return Ok(serde_json::json!({"message":reason})); }
            }
            return Ok(serde_json::json!({"message":"The selected symbol has no resolved plan identity."}));
        };
        let occurrences = index.occurrence.iter().filter(|reference| reference.symbol == symbol).collect::<Vec<_>>();
        if occurrences.is_empty() { return Ok(serde_json::json!({"message":"No references in this plan snapshot."})); }
        Ok(serde_json::json!({"references":occurrences,"revision":self.document.revision()}))
    }

    fn reveal_reference(&mut self, id: &str) -> Result<serde_json::Value> {
        let reference = self.reference.lock().map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?.values().flat_map(|index| &index.occurrence).find(|reference| reference.id == id)
            .context("plan reference selection is unavailable")?.clone();
        let locate = |blocks: &[forge_buffer::block::BufferBlock], targets: &HashMap<TargetId, PlanNavigationAnchor>| {
            blocks.iter().enumerate().find_map(|(index, block)| {
                block.metadata.target.iter().find_map(|range| {
                    let anchor = targets.get(&range.id)?;
                    let selected = match &anchor.target {
                        super::PlanReviewTarget::Call { path, side, owner, name, kind } => reference.kind == kind.label() && path == &reference.path && side == &reference.side && owner == &reference.owner && name == &reference.name,
                        super::PlanReviewTarget::Declaration { path, side, line, .. } => !matches!(reference.kind.as_str(), "call" | "property") && path == &reference.path && side == &reference.side && *line == reference.line,
                        _ => reference.anchor.as_ref().is_some_and(|saved| saved.json_path == anchor.json_path),
                    };
                    selected.then(|| (index, forge_buffer::block::BlockAnchor {
                        block: block.id.clone(), position: TextPosition { row: range.range.start.row,
                            column: if matches!(reference.kind.as_str(), "call" | "property") { block.text.row(range.range.start.row).unwrap_or("").find(&reference.name).unwrap_or(0) } else { reference.column as usize } },
                    }))
                })
            })
        };
        if let Some((_, jump)) = locate(&self.document.snapshot().block, &self.target) {
            return Ok(serde_json::json!({"jump":jump}));
        }
        self.retain_annotation_revision();
        let previous_revealed = self.source.revealed.clone();
        let previous_collapse = self.source.collapse.clone();
        self.source.revealed.retain(|(path, side)| path != &reference.path || side == &reference.side);
        self.source.revealed.insert((reference.path.clone(), reference.side.clone()));
        let projection = (|| -> Result<_> {
            let (mut blocks, mut targets) = super::design_review::project(
                &self.source.document, &self.width, &[], &self.annotation_revision, None,
                &self.source.declaration_syntax, self.source.public_only, self.source.trace.as_ref(), &self.source.revealed,
            )?;
            let (selected, jump) = locate(&blocks, &targets).context("reference no longer has a display occurrence")?;
            let endpoint: HashMap<_, _> = blocks.iter().enumerate().map(|(index, block)| (block.id.clone(), index)).collect();
            for (index, block) in blocks.iter_mut().enumerate() {
                for fold in &mut block.metadata.fold {
                    let end = endpoint[&fold.end.block] + usize::from(fold.end.position.row > 0);
                    if index <= selected && selected < end {
                        self.source.collapse.insert(fold.id.clone(), false);
                        fold.closed = false;
                    }
                }
            }
            let blocks = forge_buffer::collapse::project(blocks, &self.source.collapse)?;
            let visible: std::collections::HashSet<_> = blocks.iter().flat_map(|block| &block.metadata.target).map(|range| range.id.clone()).collect();
            targets.retain(|id, _| visible.contains(id));
            ensure!(blocks.iter().any(|block| block.id == jump.block), "reference destination remains collapsed");
            let patch = self.document.edit(0..self.document.block_count(), blocks)?;
            Ok((patch, targets, jump))
        })();
        if projection.is_err() {
            self.source.revealed = previous_revealed;
            self.source.collapse = previous_collapse;
        }
        let (patch, targets, jump) = projection?;
        self.target = targets;
        self.focused_annotation = None;
        Ok(serde_json::json!({"patch":patch,"snapshot":self.snapshot(),"source_row":self.source_rows(),"annotation":self.annotation.annotation(),"jump":jump}))
    }

    fn describe(
        &self,
        anchor: PlanNavigationAnchor,
        action: &str,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        use super::PlanReviewTarget;
        if let Some(design) = &self.source.document.design {
            if action == "jump_entity" {
                return self.jump_declaration(&anchor, row, column);
            }
            ensure!(action == "open", "this design action is unavailable");
            let (path, side) = match &anchor.target {
                PlanReviewTarget::Declaration { path, side, .. } | PlanReviewTarget::Call { path, side, .. } | PlanReviewTarget::Change { path, side, .. } => (path, side.as_str()),
                PlanReviewTarget::File { path } => (path, "proposed"),
                _ => anyhow::bail!("select a declaration file or line"),
            };
            let text = if side == "baseline" {
                design.baseline.get(path).map(|file| &file.text)
            } else {
                design
                    .proposed
                    .get(path)
                    .or_else(|| design.baseline.get(path).map(|file| &file.text))
            }
            .context("declaration file is unavailable")?;
            let calls = if side == "baseline" { &design.baseline_calls } else { &design.proposed_calls };
            let presentation = super::calls::present(path, text, calls.get(path).map(Vec::as_slice).unwrap_or_default())?.declaration;
            let snapshot = BufferDocument::new(
                DocumentId(format!("plan:declarations:{}", uuid::Uuid::new_v4())),
                vec![forge_buffer::block::BufferBlock {
                    id: forge_buffer::identity::BlockId("declarations".into()),
                    text: forge_buffer::text::BufferText::from_rows(presentation.text.lines())?,
                    metadata: Default::default(),
                }],
            )?
            .snapshot();
            let filetype = forge_diff::syntax::DeclarationOverview::filetype(path);
            return Ok(serde_json::json!({"declarations":snapshot,"filetype":filetype}));
        }
        let callable = match &anchor.target {
            PlanReviewTarget::FlowEdge {
                callable_name: Some(name),
                ..
            } if row
                .match_indices(name)
                .any(|(start, _)| start <= column && column < start + name.len()) =>
            {
                Some(name.as_str())
            }
            _ => None,
        };
        let cursor_entity = (action == "jump_entity")
            .then(|| {
                self.source
                    .document
                    .entity_changes
                    .iter()
                    .filter(|entity| {
                        !entity.name.is_empty()
                            && row.match_indices(&entity.name).any(|(start, name)| {
                                let end = start + name.len();
                                let identifier = |character: char| {
                                    character.is_alphanumeric() || character == '_'
                                };
                                start <= column
                                    && column < end
                                    && !row[..start].chars().next_back().is_some_and(identifier)
                                    && !row[end..].chars().next().is_some_and(identifier)
                            })
                    })
                    .max_by_key(|entity| entity.name.len())
            })
            .flatten();
        let entity_name =
            cursor_entity
                .map(|entity| entity.name.as_str())
                .or_else(|| match &anchor.target {
                    PlanReviewTarget::Entity { name }
                    | PlanReviewTarget::FileTreeEntity { name, .. } => Some(name.as_str()),
                    PlanReviewTarget::EntityMember { entity, .. }
                    | PlanReviewTarget::EnumVariant { entity, .. }
                    | PlanReviewTarget::EnumVariantField { entity, .. }
                        if action != "jump_entity" =>
                    {
                        Some(entity.as_str())
                    }
                    PlanReviewTarget::FlowStep { target_name, .. }
                    | PlanReviewTarget::FlowEdge { target_name, .. } => Some(target_name.as_str()),
                    _ => None,
                });
        let entity = entity_name.and_then(|name| {
            self.source
                .document
                .entity_changes
                .iter()
                .find(|entity| entity.name == name)
        });
        let source_path = match &anchor.target {
            PlanReviewTarget::FlowStep { workspace_path: Some(path), workspace_line, .. }
                | PlanReviewTarget::FlowEdge { workspace_path: Some(path), workspace_line, .. } => Some((path.as_str(), workspace_line.unwrap_or(1))),
            _ => anchor.path.as_deref().map(|path| (path, 1)),
        }.map(|(path, line)| serde_json::json!({"path":self.source.workspace.join(path),"line":line,"column":1}));
        let entity_anchor = entity.and_then(|entity| self.source.rendered.navigation.anchor.iter().find(|candidate| {
            if let Some(callable) = callable.filter(|_| cursor_entity.is_none()) {
                matches!(&candidate.target, PlanReviewTarget::EntityMember { entity: name, member } if name == &entity.name && member == callable)
            } else { matches!(&candidate.target, PlanReviewTarget::Entity { name } if name == &entity.name) }
        }));
        let mut jump = None;
        if let Some(entity_anchor) = entity_anchor {
            for block in self.document.snapshot().block {
                if let Some(target) = block.metadata.target.iter().find(|target| {
                    self.target
                        .get(&target.id)
                        .is_some_and(|candidate| candidate.json_path == entity_anchor.json_path)
                }) {
                    let mut position = target.range.start;
                    if let Some(name) = entity_name {
                        if let Some(column) = block.text.row(position.row).and_then(|row| {
                            row.find(callable.filter(|_| cursor_entity.is_none()).unwrap_or(name))
                        }) {
                            position.column = column;
                        }
                    }
                    jump = Some(forge_buffer::block::BlockAnchor {
                        block: block.id,
                        position,
                    });
                    break;
                }
            }
        }
        let info = if let Some(entity) = entity.filter(|_| action == "entity_info") {
            let description = format!(
                "# {}\n\n{}\n\n```json\n{}\n```",
                entity.name,
                entity.description,
                serde_json::to_string_pretty(entity)?
            );
            ensure!(
                description.len() <= 65536,
                "plan entity information exceeds 64 KiB"
            );
            let id = format!("plan:info:{}", uuid::Uuid::new_v4());
            let rendered = forge_buffer::markdown::MarkdownRenderer::render(
                forge_buffer::identity::BlockId("info".into()),
                &description,
                &self.width,
            )?;
            Some(BufferDocument::new(DocumentId(id), vec![rendered.block])?.snapshot())
        } else {
            None
        };
        Ok(
            serde_json::json!({"anchor":anchor,"source":source_path,"jump":jump,"info":info,
            "entity_name":entity.map(|entity| &entity.name), "rename_allowed":entity.is_some_and(|entity| entity.action == super::EntityChangeAction::Add),
            "version":self.source.document.version, "rustdoc_selection":if callable.is_some() { "callable" } else { "receiver" } }),
        )
    }

    fn resolve_declaration(
        &self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<crate::declaration::DeclarationResolution> {
        if let super::PlanReviewTarget::Call { path, side, owner, name, kind } = &anchor.target {
            let design = self.source.document.design.as_ref().context("declaration design is unavailable")?;
            let calls = if side == "baseline" { &design.baseline_calls } else { &design.proposed_calls };
            if calls.get(path).into_iter().flatten().any(|function| function.owner == *owner && function.call.iter().flatten().any(|call| call.name == *name && call.kind == *kind && call.unresolved)) {
                return Ok(crate::declaration::DeclarationResolution::Unverified { reason: "This target includes an opaque local binding in the captured source.".into() });
            }
            let baseline = side == "baseline";
            let mut reference = self.reference.lock().map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?;
            if !reference.contains_key(&baseline) {
                reference.insert(baseline, super::references::PlanReferenceIndex::build(&self.source.document, &self.source.workspace, baseline)?);
            }
            let index = reference.get(&baseline).unwrap();
            if let Some(identity) = index.selected(anchor, column as u32)
                && let Some(destination) = index.destination(&self.source.document, &self.source.workspace, identity, side == "baseline")? {
                return Ok(crate::declaration::DeclarationResolution::Resolved {destination});
            }
            if let Some(destination) = super::references::PlanReferenceIndex::lua_source_call(&self.source.document, &self.source.workspace, path, name, side == "baseline")? {
                return Ok(crate::declaration::DeclarationResolution::Resolved {destination});
            }
            let resolver = self.source.resolver.get().context("declaration resolver is unavailable")?;
            let mut resolver = resolver.lock().map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
            let resolver = if side == "baseline" { &mut resolver.1 } else { &mut resolver.0 };
            return Ok(match kind { super::CallKind::Call => resolver.callable(path, owner, name), super::CallKind::Property => resolver.property(path, owner, name) });
        }
        let super::PlanReviewTarget::Declaration {
            path,
            side,
            line,
            column: saved_column,
        } = &anchor.target
        else {
            anyhow::bail!("select a declaration type or import");
        };
        let design = self
            .source
            .document
            .design
            .as_ref()
            .context("declaration design is unavailable")?;
        let text = if side == "baseline" {
            design.baseline.get(path).map(|file| &file.text)
        } else {
            design.proposed.get(path)
        }
        .context("declaration snapshot is unavailable")?;
        let position = forge_diff::syntax::DeclarationOverview::token_position(
            path,
            text,
            forge_diff::syntax::DeclarationPosition {
                line: *line,
                column: saved_column.unwrap_or(0),
            },
            row,
            column,
        )
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        let resolver = self
            .source
            .resolver
            .get()
            .context("declaration resolver is unavailable")?;
        let mut resolver = resolver
            .lock()
            .map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
        Ok(if side == "baseline" {
            resolver.1.at(path, position.line, position.column)
        } else {
            resolver.0.at(path, position.line, position.column)
        })
    }

    fn jump_declaration(
        &self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        use crate::declaration::DeclarationResolution;
        let result = self.resolve_declaration(anchor, row, column)?;
        let side = match &anchor.target {
            super::PlanReviewTarget::Declaration { side, .. } | super::PlanReviewTarget::Call { side, .. } => side,
            _ => anyhow::bail!("select a declaration type, import, or call"),
        };
        let design = self
            .source
            .document
            .design
            .as_ref()
            .context("declaration design is unavailable")?;
        let destination = match result {
            DeclarationResolution::Resolved { destination } => destination,
            DeclarationResolution::Intrinsic => {
                return Ok(
                    serde_json::json!({"message":"This is a language intrinsic with no source declaration."}),
                );
            }
            DeclarationResolution::Invalid { reason }
            | DeclarationResolution::Ambiguous { reason }
            | DeclarationResolution::Unverified { reason } => {
                return Ok(serde_json::json!({"message":reason}));
            }
        };
        if destination.module_file {
            let relative = std::path::Path::new(&destination.path)
                .strip_prefix(&self.source.workspace)
                .ok()
                .map(|path| path.to_string_lossy().replace('\\', "/"));
            if let Some(relative) = &relative {
                for block in self
                    .document
                    .blocks(self.document.revision(), 0..self.document.block_count())?
                {
                    if block.metadata.target.iter().any(|target| self.target.get(&target.id).is_some_and(|anchor| matches!(&anchor.target, super::PlanReviewTarget::File { path } if path == relative))) {
                        return Ok(serde_json::json!({"jump":{"block":block.id,"position":{"row":0,"column":0}}}));
                    }
                }
                let text = if side == "baseline" {
                    design.baseline.get(relative).map(|file| &file.text)
                } else {
                    design.proposed.get(relative)
                };
                if destination.proposed || !std::path::Path::new(&destination.path).is_file() {
                    if let Some(text) = text {
                        let presentation =
                            forge_diff::syntax::DeclarationOverview::present(relative, text)
                                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                        let snapshot = BufferDocument::new(
                            DocumentId(format!("plan:declaration:{}", uuid::Uuid::new_v4())),
                            vec![forge_buffer::block::BufferBlock {
                                id: forge_buffer::identity::BlockId("declarations".into()),
                                text: forge_buffer::text::BufferText::from_rows(
                                    presentation.text.lines(),
                                )?,
                                metadata: Default::default(),
                            }],
                        )?
                        .snapshot();
                        return Ok(
                            serde_json::json!({"declarations":snapshot,"filetype":forge_diff::syntax::DeclarationOverview::filetype(relative),"selection":{"row":0,"column":0}}),
                        );
                    }
                }
            }
            return Ok(serde_json::json!({"source":{"path":destination.path,"line":1,"column":0}}));
        }
        if !destination.proposed && side != "baseline" {
            return Ok(
                serde_json::json!({"source": {"path":destination.path, "line":destination.line, "column":destination.column}}),
            );
        }
        let relative = std::path::Path::new(&destination.path)
            .strip_prefix(&self.source.workspace)
            .ok()
            .map(|path| path.to_string_lossy().replace('\\', "/"));
        if let Some(relative) = relative {
            let text = if side == "baseline" {
                design.baseline.get(&relative).map(|file| &file.text)
            } else {
                design.proposed.get(&relative)
            };
            let Some(text) = text else {
                anyhow::ensure!(
                    !destination.proposed,
                    "resolved declaration snapshot is unavailable"
                );
                return Ok(serde_json::json!({"source":{"path":destination.path,"line":destination.line,"column":destination.column}}));
            };
            let presentation = forge_diff::syntax::DeclarationOverview::present(&relative, text)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let display_position = forge_diff::syntax::DeclarationOverview::display_position(
                &relative,
                text,
                forge_diff::syntax::DeclarationPosition {
                    line: destination.line,
                    column: destination.column,
                },
            )
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let row_anchor = presentation.source[(display_position.line - 1) as usize];
            for block in self
                .document
                .blocks(self.document.revision(), 0..self.document.block_count())?
            {
                for target in &block.metadata.target {
                    let Some(anchor) = self.target.get(&target.id) else {
                        continue;
                    };
                    if matches!(&anchor.target, super::PlanReviewTarget::Declaration { path, side: target_side, line, column } if path == &relative && target_side == side && row_anchor.is_some_and(|position| position.line == *line && Some(position.column) == *column))
                    {
                        if let Some(column) = block
                            .text
                            .row(0)
                            .filter(|row| {
                                row.get(display_position.column as usize..)
                                    .is_some_and(|rest| rest.starts_with(&destination.name))
                            })
                            .map(|_| display_position.column)
                        {
                            return Ok(
                                serde_json::json!({"jump":{"block":block.id,"position":{"row":0,"column":column}}}),
                            );
                        }
                    }
                }
            }
            let selected = display_position.line - 1;
            let column = display_position.column;
            let snapshot = BufferDocument::new(
                DocumentId(format!("plan:declaration:{}", uuid::Uuid::new_v4())),
                vec![forge_buffer::block::BufferBlock {
                    id: forge_buffer::identity::BlockId("declarations".into()),
                    text: forge_buffer::text::BufferText::from_rows(presentation.text.lines())?,
                    metadata: Default::default(),
                }],
            )?
            .snapshot();
            return Ok(
                serde_json::json!({"declarations":snapshot,"filetype":forge_diff::syntax::DeclarationOverview::filetype(&relative),"selection":{"row":selected,"column":column}}),
            );
        }
        Ok(
            serde_json::json!({"source":{"path":destination.path,"line":destination.line,"column":destination.column}}),
        )
    }

    /// Retain edit counters while compact comments have no editable regions.
    fn retain_annotation_revision(&mut self) {
        for block in &self.document.snapshot().block {
            for region in &block.metadata.editable_region {
                self.annotation_revision
                    .insert(region.id.0.clone(), (region.revision, region.sequence));
            }
        }
    }

    pub(crate) fn new(
        id: DocumentId,
        view: ViewId,
        source: PlanReviewSource,
        width: WidthProfile,
        focused_annotation: Option<String>,
    ) -> Result<Self> {
        view.validate()?;
        let annotation_path = if source.historical {
            source
                .path
                .parent()
                .and_then(|directory| directory.parent())
                .context("plan revision parent is missing")?
                .join("working.json")
        } else {
            source.path.with_extension("json")
        };
        let mut annotation =
            ReviewAnnotationStore::open(annotation_path, source.saved_digest.clone())?;
        if source.saved_navigation.is_some() {
            annotation.reanchor(&source.rendered.navigation)?;
        }
        let (block, target) = super::review_projection::project(
            &source,
            &width,
            &[],
            &HashMap::new(),
            focused_annotation.as_deref(),
        )?;
        let document = BufferDocument::new(id.clone(), block)?;
        let mut view_width = DocumentViews::default();
        view_width.open(view.clone(), width.clone())?;
        Ok(Self {
            reference: Default::default(),
            focused_annotation,
            annotation_revision: HashMap::new(),
            id,
            source,
            document,
            target,
            annotation,
            width,
            view_width,
            view: HashMap::from([(view, InputSequence(0))]),
        })
    }

    fn source_rows(&self) -> Vec<serde_json::Value> {
        let mut row = Vec::new();
        for block in self.document.snapshot().block {
            for (position, text) in block.text.wire_rows().iter().enumerate() {
                let target = block.metadata.target.iter().find(|target| {
                    target.range.start.row <= position && position < target.range.end.row
                });
                let line = target
                    .and_then(|target| self.target.get(&target.id))
                    .map(|anchor| anchor.line)
                    .unwrap_or(row.len() as u32 + 1);
                row.push(
                    serde_json::json!({"id":format!("{}:{}",block.id.0,position), "text":text,
                    "source_line":line, "block":block.id, "position":{"row":position,"column":0},
                    "target":target.map(|target| &target.id), "metadata":block.metadata}),
                );
            }
        }
        row
    }

    pub(crate) fn snapshot(&self) -> BufferSnapshot {
        self.document.snapshot()
    }

    pub(crate) fn saved_digest(&self) -> &str {
        &self.source.saved_digest
    }

    pub(crate) fn action(&mut self, input: DocumentInput) -> Result<PlanNavigationAnchor> {
        let target = self
            .validate_input(input)?
            .context("plan review input has no source target")?;
        Ok(self
            .target
            .get(&target)
            .context("plan review source target is missing")?
            .clone())
    }

    fn check_input(&self, input: &DocumentInput) -> Result<Option<TargetId>> {
        input.validate()?;
        ensure!(
            input.document == self.id && input.revision == self.document.revision(),
            "plan review input revision changed"
        );
        let sequence = self
            .view
            .get(&input.view)
            .context("plan review view is closed")?;
        ensure!(
            input.sequence.0 > sequence.0,
            "plan review input was superseded"
        );
        let block = self
            .document
            .block(&input.block)
            .context("plan review input block is missing")?;
        let line = block
            .text
            .row(input.position.row)
            .context("plan review input row is missing")?;
        ensure!(
            line.is_char_boundary(input.position.column),
            "plan review input column is invalid"
        );
        let target = block
            .metadata
            .target
            .iter()
            .find(|target| {
                target.range.start <= input.position && input.position < target.range.end
            })
            .map(|target| target.id.clone());
        ensure!(input.target == target, "plan review input target changed");
        Ok(target)
    }

    fn validate_input(&mut self, input: DocumentInput) -> Result<Option<TargetId>> {
        let target = self.check_input(&input)?;
        *self
            .view
            .get_mut(&input.view)
            .context("plan review view is closed")? = input.sequence;
        Ok(target)
    }

    fn toggle_public(&mut self) -> Result<serde_json::Value> {
        ensure!(
            self.source.document.design.is_some(),
            "public visibility requires a declaration design"
        );
        self.retain_annotation_revision();
        let public_only = !self.source.public_only;
        self.source.public_only = public_only;
        let projection = super::review_projection::project(
            &self.source,
            &self.width,
            &[],
            &self.annotation_revision,
            None,
        ).and_then(|(block, target)| {
            let patch = self.document.edit(0..self.document.block_count(), block)?;
            Ok((patch, target))
        });
        if projection.is_err() { self.source.public_only = !public_only; }
        let (patch, target) = projection?;
        self.source.public_only = public_only;
        self.focused_annotation = None;
        self.target = target;
        Ok(
            serde_json::json!({"patch":patch, "snapshot":self.snapshot(), "source_row":self.source_rows(), "annotation":self.annotation.annotation(), "public_only":public_only}),
        )
    }

    fn toggle_declaration(&mut self, owner: &forge_buffer::identity::BlockId) -> Result<serde_json::Value> {
        let collapse = self.document.block(owner)
            .and_then(|block| block.metadata.collapse.first()).cloned()
            .context("cursor is not inside a collapsible declaration")?;
        self.retain_annotation_revision();
        let previous = self.source.collapse.insert(collapse.id.clone(), !collapse.closed);
        let projection = super::review_projection::project(
            &self.source, &self.width, &[], &self.annotation_revision, None,
        ).and_then(|(block, target)| {
            let patch = self.document.edit(0..self.document.block_count(), block)?;
            Ok((patch, target))
        });
        if projection.is_err() {
            if let Some(previous) = previous { self.source.collapse.insert(collapse.id.clone(), previous); }
            else { self.source.collapse.remove(&collapse.id); }
        }
        let (patch, target) = projection?;
        self.target = target;
        self.focused_annotation = None;
        Ok(serde_json::json!({"patch":patch,"snapshot":self.snapshot(),"source_row":self.source_rows(),
            "annotation":self.annotation.annotation(),"jump":collapse.opening}))
    }

    fn update_view(
        &mut self,
        view: ViewId,
        width: Option<WidthProfile>,
    ) -> Result<serde_json::Value> {
        let mut views = self.view_width.clone();
        if let Some(width) = &width {
            views.open(view.clone(), width.clone())?;
        } else {
            views.close(&view);
        }
        let profile = views
            .profile()
            .cloned()
            .unwrap_or_else(|| self.width.clone());
        let patch = if profile != self.width {
            let revision = self
                .document
                .snapshot()
                .block
                .iter()
                .flat_map(|block| &block.metadata.editable_region)
                .map(|region| (region.id.0.clone(), (region.revision, region.sequence)))
                .collect();
            let (block, target) = super::review_projection::project(
                &self.source,
                &profile,
                &[],
                &revision,
                self.focused_annotation.as_deref(),
            )?;
            let patch = self.document.edit(0..self.document.block_count(), block)?;
            self.target = target;
            self.width = profile;
            Some(patch)
        } else {
            None
        };
        if width.is_some() {
            self.view.entry(view).or_insert(InputSequence(0));
        } else {
            self.view.remove(&view);
        }
        self.view_width = views;
        Ok(
            serde_json::json!({"patch":patch, "snapshot":self.snapshot(), "source_row":self.source_rows(), "annotation":self.annotation.annotation()}),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declaration_collapse_preserves_source_and_visibility_choices() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Inspect containers.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Expose configuration.".into();
        design.document.description = "Reject invalid configuration.".into();
        design.proposed.insert("src/lib.rs".into(), "#[derive(Debug)]\n/// Rejects invalid dimensions.\npub enum ConfigError {\n  /// Invalid arena.\n  ArenaSize,\n}\n\n/// Stores configuration.\npub struct Config {\n  /// Internal limit.\n  limit: u32,\n  /// Public count.\n  pub count: u32,\n}\n".into());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", 1, &digest).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("review".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let find = |document: &PlanReviewDocument, needle: &str| {
            document.snapshot().block.into_iter().find(|block| block.text.row(0).is_some_and(|row| row.contains(needle))).unwrap()
        };
        let opening = find(&document, "pub enum ConfigError");
        assert!(opening.text.row(0).unwrap().ends_with("{...}"));
        assert!(opening.metadata.fold.is_empty());
        assert!(document.snapshot().block.iter().any(|block| block.text.row(0) == Some("#[derive(Debug)]")));
        assert!(!document.snapshot().block.iter().any(|block| block.text.row(0).is_some_and(|row| row.contains("ArenaSize"))));
        let identity = opening.metadata.target.clone();
        document.toggle_declaration(&opening.id).unwrap();
        let expanded = find(&document, "pub enum ConfigError");
        assert_eq!(expanded.id, opening.id);
        assert_eq!(expanded.metadata.target, identity);
        let member = find(&document, "ArenaSize");
        let result = document.toggle_declaration(&member.id).unwrap();
        assert_eq!(result["jump"]["block"], opening.id.0);
        document.toggle_public().unwrap();
        assert!(find(&document, "pub enum ConfigError").text.row(0).unwrap().ends_with("{...}"));
        assert!(find(&document, "limit: u32").metadata.collapse.len() == 1);
        let config = find(&document, "pub struct Config");
        document.toggle_declaration(&config.id).unwrap();
        document.toggle_public().unwrap();
        assert!(find(&document, "pub struct Config").text.row(0).unwrap().ends_with("{...}"));
        document.toggle_declaration(&config.id).unwrap();
        assert!(find(&document, "pub count: u32").metadata.collapse.len() == 1);
        assert!(!document.snapshot().block.iter().any(|block| block.text.row(0).is_some_and(|row| row.contains("limit: u32"))));
        document.update_view(ViewId("view".into()), Some(WidthProfile { columns: 60, ..WidthProfile::default() })).unwrap();
        assert!(find(&document, "pub enum ConfigError").text.row(0).unwrap().ends_with("{...}"));
    }

    #[test]
    fn questions_preserve_design_and_persist_answers_bound_to_current_text() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Question the API.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Expose shared state.".into();
        design.document.description = "Use a focused state owner.".into();
        design.proposed.insert("state.rs".into(), "/// Owns shared state.\npub struct State;\n".into());
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = file.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = file.capture_review_source("session", "plan", 1, &checksum).unwrap();
        let source_path = source.path.with_extension("json");
        let original = std::fs::read(&source_path).unwrap();
        let digest = source.saved_digest.clone();
        let id = DocumentId("questions".into());
        let document = PlanReviewDocument::new(id.clone(), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let anchor = document.source.rendered.navigation.anchor.iter().find(|anchor| matches!(&anchor.target, crate::plan::PlanReviewTarget::Declaration {..})).unwrap();
        let line = anchor.line;
        let snapshot = document.snapshot();
        let target = snapshot.block[0].metadata.target[0].clone();
        let mut input = DocumentInput { document:id.clone(), revision:snapshot.revision, view:ViewId("view".into()),
            sequence:InputSequence(1), action:"plan.questions.answer".into(), block:snapshot.block[0].id.clone(),
            position:target.range.start, target:Some(target.id) };
        let store = PlanReviewStore::default();
        let admission = store.admit(id.clone()).unwrap();
        store.insert(document, admission).unwrap();
        let mut question = ReviewAnnotation { parent_id: None, id:"question".into(), kind:ReviewAnnotationKind::Question, reply:None, anchor:None,
            source:crate::plan::PlanAnnotationInput { start_line:line, end_line:line, body:"Why use this owner?".into() } };
        let mut comment = question.clone();
        comment.id = "comment".into();
        comment.kind = ReviewAnnotationKind::Comment;
        comment.source.body = "Keep this interface.".into();
        store.save_annotations(&id, &digest, vec![question.clone(), comment.clone()]).unwrap();
        let questions = store.questions(input.clone()).unwrap();
        assert_eq!(questions.len(), 1);
        assert!(questions[0].prompt.contains("pub struct State;") && questions[0].prompt.contains("Why use this owner?"));
        let captured = store.answer(&id, &questions[0], "It isolates ownership.".into(), 3210).unwrap();
        assert_eq!(captured[0].reply.as_ref().unwrap().body, "It isolates ownership.");
        store.save_annotations(&id, &digest, vec![question.clone(), comment.clone()]).unwrap();
        input.sequence = InputSequence(2);
        assert!(store.questions(input.clone()).unwrap().is_empty(), "answered questions were submitted twice");
        input.sequence = InputSequence(3);
        let feedback = store.submission(input.clone()).unwrap();
        assert_eq!(feedback["annotations"].as_array().unwrap().len(), 1, "questions became revision instructions");
        assert_eq!(feedback["annotations"][0]["body"], "Keep this interface.");
        let reopened = file.capture_review_source("session", "plan", 1, &checksum).unwrap();
        let reopened = PlanReviewDocument::new(DocumentId("reopened".into()), ViewId("other".into()), reopened, WidthProfile::default(), None).unwrap();
        assert_eq!(reopened.annotation.annotation()[0].reply.as_ref().unwrap().body, "It isolates ownership.");
        assert_eq!(reopened.annotation.annotation()[0].reply.as_ref().unwrap().duration_ms, Some(3210));
        let mut followup = question.clone();
        followup.id = "followup".into();
        followup.parent_id = Some(question.id.clone());
        followup.source.body = "How can callers observe it?".into();
        store.save_annotations(&id, &digest, vec![question.clone(), comment.clone(), followup.clone()]).unwrap();
        input.sequence = InputSequence(4);
        let followup_question = store.questions(input.clone()).unwrap();
        assert_eq!(followup_question.len(), 1);
        assert!(followup_question[0].prompt.contains("Why use this owner?")
            && followup_question[0].prompt.contains("It isolates ownership.")
            && followup_question[0].prompt.contains("pub struct State;"));
        store.answer(&id, &followup_question[0], "Use a getter.".into(), 1000).unwrap();
        let mut change = comment.clone();
        change.id = "change".into();
        change.parent_id = Some(followup.id.clone());
        change.source.body = "Add that getter.".into();
        store.save_annotations(&id, &digest, vec![question.clone(), comment.clone(), followup, change]).unwrap();
        input.sequence = InputSequence(5);
        let feedback = store.submission(input.clone()).unwrap();
        let body = feedback["annotations"][1]["body"].as_str().unwrap();
        for expected in ["Why use this owner?", "It isolates ownership.", "How can callers observe it?", "Use a getter.", "Add that getter."] {
            assert!(body.contains(expected), "missing thread entry: {expected}");
        }
        assert!(!body.contains("Keep this interface."), "unrelated feedback entered the thread");
        let reopened = file.capture_review_source("session", "plan", 1, &checksum).unwrap();
        let reopened = PlanReviewDocument::new(DocumentId("thread-reopened".into()), ViewId("thread-view".into()), reopened, WidthProfile::default(), None).unwrap();
        assert_eq!(reopened.annotation.annotation()[3].parent_id.as_deref(), Some("followup"));
        assert_eq!(reopened.annotation.annotation()[2].reply.as_ref().unwrap().body, "Use a getter.");
        question.source.body = "Why this interface?".into();
        store.save_annotations(&id, &digest, vec![question.clone(), comment]).unwrap();
        assert!(store.annotations(&id).unwrap()[0].reply.is_none(), "edited question retained a previous answer");
        assert!(store.answer(&id, &questions[0], "Stale answer.".into(), 0).is_err());
        input.sequence = InputSequence(6);
        assert_eq!(store.questions(input).unwrap()[0].body, question.source.body);
        assert_eq!(std::fs::read(source_path).unwrap(), original, "answering changed the declaration artifact");
    }
    use crate::plan::PlanFileStore;

    #[test]
    fn historical_revision_retains_comments_after_working_plan_changes() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Original overview");
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let mut annotation =
            ReviewAnnotationStore::open(source.path.with_extension("json"), source.saved_digest)
                .unwrap();
        annotation
            .replace(vec![ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
                anchor: None,
                id: "original-comment".into(),
                source: crate::plan::PlanAnnotationInput {
                    start_line: 1,
                    end_line: 1,
                    body: "Rename this configuration".into(),
                },
            }])
            .unwrap();
        canonical.overview = "Revised overview".into();
        canonical.version = 2;
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        store
            .submit_document_revision("session", "plan", 2, 2)
            .unwrap();
        let source = store.capture_revision_source("session", "plan", 1).unwrap();
        assert!(
            source.public_only,
            "historical review did not default to Public"
        );
        assert_eq!(source.document.overview, "Original overview");
        let historical = PlanReviewDocument::new(
            DocumentId("history".into()),
            ViewId("history-view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert_eq!(
            historical.annotation.annotation()[0].source.body,
            "Rename this configuration"
        );
        let snapshot = historical.snapshot();
        assert!(
            snapshot
                .block
                .iter()
                .all(|block| block.metadata.editable_region.is_empty())
        );
        assert!(!snapshot.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .join("\n")
                .contains("Rename this configuration")
        }));
        let target = snapshot.block[0].metadata.target[0].clone();
        let input = DocumentInput {
            document: snapshot.document,
            revision: snapshot.revision,
            view: ViewId("history-view".into()),
            sequence: InputSequence(1),
            action: "comment".into(),
            block: snapshot.block[0].id.clone(),
            position: target.range.start,
            target: Some(target.id),
        };
        let review = PlanReviewStore::default();
        let admission = review.admit(historical.id.clone()).unwrap();
        let opened = review.insert(historical, admission).unwrap();
        assert_eq!(opened["annotation"][0]["source"]["body"], "Rename this configuration");
        assert!(
            review
                .save_annotations(&DocumentId("history".into()), "unused", vec![])
                .unwrap_err()
                .to_string()
                .contains("read-only")
        );
        assert!(
            review
                .submission(input)
                .unwrap_err()
                .to_string()
                .contains("read-only")
        );
        let latest = store.capture_revision_source("session", "plan", 2).unwrap();
        let latest = PlanReviewDocument::new(
            DocumentId("latest".into()),
            ViewId("latest-view".into()),
            latest,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert!(latest.annotation.annotation().is_empty());
    }

    #[tokio::test]
    async fn declaration_jumps_use_token_positions_and_filtered_snapshots() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Declarations");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Define the API.".into();
        design.document.description = "Expose users through the API.".into();
        design.proposed.insert(
            "tsconfig.json".into(),
            "{\"compilerOptions\":{\"noLib\":true}}".into(),
        );
        design.proposed.insert(
            "model.ts".into(),
            "export interface User<T> { value: T; }\n".into(),
        );
        design.proposed.insert("api.ts".into(), "import type { User } from './model';\ninterface Hidden {}\nexport interface Api { user: User<string>; hidden: Hidden; }\n".into());
        canonical.design = Some(design);
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, digest) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let mut source = store
            .capture_review_source("session", "plan", 1, &digest)
            .unwrap();
        assert!(source.public_only);
        source.public_only = false;
        let document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let resolver = Arc::clone(&document.source.resolver);
        assert!(
            resolver.get().is_none(),
            "opening acquired dependency sources"
        );
        let snapshot = document.snapshot();
        let block = snapshot
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|row| row.contains("user: User"))
            })
            .unwrap();
        let input = DocumentInput {
            document: document.id.clone(),
            revision: snapshot.revision,
            view: ViewId("view".into()),
            sequence: InputSequence(1),
            action: "jump_entity".into(),
            block: block.id.clone(),
            position: forge_buffer::block::TextPosition {
                row: 0,
                column: block.text.row(0).unwrap().find("User").unwrap(),
            },
            target: block
                .metadata
                .target
                .first()
                .map(|target| target.id.clone()),
        };
        let review = PlanReviewStore::default();
        review
            .insert(document, review.admit(input.document.clone()).unwrap())
            .unwrap();
        let mut stale = input.clone();
        stale.sequence = InputSequence(0);
        assert!(review.prepare_declaration_jump(&stale, None).await.is_err());
        assert!(resolver.get().is_none(), "invalid input acquired sources");
        review.prepare_declaration_jump(&input, None).await.unwrap();
        assert!(
            resolver.get().is_some(),
            "first jump did not acquire sources"
        );
        assert!(
            review
                .document
                .lock()
                .unwrap()
                .get(&input.document)
                .unwrap()
                .0
                .source
                .resolver_sources
                .get()
                .is_none(),
            "a local declaration jump acquired external sources"
        );
        let resolved = review.action(input.clone()).unwrap();
        assert!(resolved["jump"].is_object(), "{resolved}");
        assert!(
            review.prepare_declaration_jump(&input, None).await.is_err(),
            "superseded input was accepted"
        );
        let (mut document, _admission) = review
            .document
            .lock()
            .unwrap()
            .remove(&input.document)
            .unwrap();
        let select = |document: &PlanReviewDocument, needle: &str| {
            let snapshot = document.snapshot();
            let block = snapshot
                .block
                .iter()
                .find(|block| block.text.row(0).is_some_and(|row| row.contains(needle)))
                .unwrap();
            let row = block.text.row(0).unwrap().to_owned();
            let anchor = block
                .metadata
                .target
                .iter()
                .find_map(|target| document.target.get(&target.id))
                .unwrap()
                .clone();
            (anchor, row)
        };
        let (anchor, row) = select(&document, "user: User");
        let result = document
            .describe(anchor, "jump_entity", &row, row.find("User").unwrap())
            .unwrap();
        let jump: forge_buffer::block::BlockAnchor =
            serde_json::from_value(result["jump"].clone()).unwrap();
        assert!(
            document
                .document
                .block(&jump.block)
                .unwrap()
                .text
                .row(0)
                .unwrap()[jump.position.column..]
                .starts_with("User")
        );
        document.toggle_public().unwrap();
        let (anchor, row) = select(&document, "hidden: Hidden");
        let result = document
            .describe(anchor, "jump_entity", &row, row.find("Hidden").unwrap())
            .unwrap();
        assert!(result["declarations"].is_object(), "{result}");
        assert!(result["selection"]["row"].is_number());
        assert!(document.source.public_only);
    }

    #[test]
    fn module_declaration_jump_selects_the_file_diff() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Module navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Expose the assets module.".into();
        design.document.description = "Keep asset declarations in their own module.".into();
        design
            .proposed
            .insert("lib.rs".into(), "pub mod assets;\npub mod stable;\n".into());
        design
            .proposed
            .insert("assets.rs".into(), "pub struct Asset;\n".into());
        design.baseline.insert(
            "stable.rs".into(),
            crate::plan::DeclarationFile {
                text: "pub struct Stable;\n".into(),
                source_digest: String::new(),
            },
        );
        design
            .proposed
            .insert("stable.rs".into(), "pub struct Stable;\n".into());
        std::fs::write(temporary.path().join("stable.rs"), "pub struct Stable;\n").unwrap();
        canonical.design = Some(design.clone());
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, digest) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &digest)
            .unwrap();
        let captured_design = source.document.design.as_ref().unwrap();
        source
            .resolver
            .set(Mutex::new((
                crate::declaration::DeclarationResolver::local(
                    temporary.path(),
                    captured_design,
                    false,
                )
                .unwrap(),
                crate::declaration::DeclarationResolver::local(
                    temporary.path(),
                    captured_design,
                    true,
                )
                .unwrap(),
            )))
            .ok()
            .unwrap();
        let document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let snapshot = document.snapshot();
        let block = snapshot
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|row| row.contains("pub mod assets"))
            })
            .unwrap();
        let row = block.text.row(0).unwrap();
        let anchor = block
            .metadata
            .target
            .iter()
            .find_map(|target| document.target.get(&target.id))
            .unwrap()
            .clone();
        let result = document
            .describe(anchor, "jump_entity", row, row.find("assets").unwrap())
            .unwrap();
        let jump: forge_buffer::block::BlockAnchor =
            serde_json::from_value(result["jump"].clone()).unwrap();
        assert!(
            document
                .document
                .block(&jump.block)
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .contains("assets.rs"),
            "{result}"
        );
        let block = snapshot
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|row| row.contains("pub mod stable"))
            })
            .unwrap();
        let row = block.text.row(0).unwrap();
        let anchor = block
            .metadata
            .target
            .iter()
            .find_map(|target| document.target.get(&target.id))
            .unwrap()
            .clone();
        let result = document
            .describe(
                anchor.clone(),
                "jump_entity",
                row,
                row.find("stable").unwrap(),
            )
            .unwrap();
        assert!(
            result["source"]["path"]
                .as_str()
                .is_some_and(|path| path.ends_with("stable.rs")),
            "{result}, {anchor:?}, {row:?}"
        );
    }

    #[test]
    fn signature_type_jump_resolves_cursor_entity_and_preserves_flow_navigation() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Initial");
        let mut summary = canonical.entity_changes[0].clone();
        summary.name = "DiagnosticSummary".into();
        canonical.entity_changes.push(summary);
        if let crate::plan::document::PlanSubtask::Work(subtask) =
            &mut canonical.stages[0].tasks[0].files[0].subtasks[0]
        {
            subtask.entities.push("DiagnosticSummary".into());
        }
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let mut anchor = document.source.rendered.navigation.anchor.iter().find(|anchor|
            matches!(&anchor.target, super::super::PlanReviewTarget::Entity { name } if name == "PlanDocument")
        ).unwrap().clone();
        anchor.target = super::super::PlanReviewTarget::EntityMember {
            entity: "PlanDocument".into(),
            member: "run_diagnostics".into(),
        };
        for row in [
            "  + run_diagnostics(): DiagnosticSummary",
            "  + run(value: &DiagnosticSummary)",
            "  - summary: DiagnosticSummary",
        ] {
            let result = document
                .describe(
                    anchor.clone(),
                    "jump_entity",
                    row,
                    row.find("DiagnosticSummary").unwrap() + 4,
                )
                .unwrap();
            assert_eq!(result["entity_name"], "DiagnosticSummary");
            let jump: forge_buffer::block::BlockAnchor =
                serde_json::from_value(result["jump"].clone()).unwrap();
            let target = document
                .document
                .block(&jump.block)
                .unwrap()
                .text
                .row(jump.position.row)
                .unwrap();
            assert!(target[jump.position.column..].starts_with("DiagnosticSummary"));
        }
        let row = "  + run(): DiagnosticSummaryExtra";
        let result = document
            .describe(
                anchor.clone(),
                "jump_entity",
                row,
                row.find("DiagnosticSummary").unwrap(),
            )
            .unwrap();
        assert!(result["entity_name"].is_null());
        assert!(result["jump"].is_null());
        let row = "  + complete(message_id: &str): Result<(), QueueError>";
        for name in ["QueueError", "Result", "str", "complete"] {
            let result = document
                .describe(anchor.clone(), "jump_entity", row, row.find(name).unwrap())
                .unwrap();
            assert!(result["jump"].is_null(), "unexpected jump for {name}");
        }
        let mut flow = anchor;
        flow.target = super::super::PlanReviewTarget::FlowStep {
            reference_kind: super::super::PlanReviewReferenceKind::PlannedEntity,
            target_name: "PlanDocument".into(),
            target_is_type: true,
            workspace_path: None,
            workspace_line: None,
        };
        assert!(
            !document
                .describe(flow, "jump_entity", "Capture", 1)
                .unwrap()["jump"]
                .is_null()
        );
    }

    #[tokio::test]
    async fn proposed_rust_nested_and_imported_types_jump_without_dependency_acquisition() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Movement declarations");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Define movement interfaces.".into();
        design.document.description = "Separate movement intent from entity transforms.".into();
        design.proposed.insert("Cargo.toml".into(), "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n[dependencies]\nbevy = \"=0.19.1\"\n".into());
        design.proposed.insert(
            "src/lib.rs".into(),
            "/// Owns movement input.\nmod controls;\n/// Consumes movement input.\nmod arena;\n"
                .into(),
        );
        design.proposed.insert("src/controls.rs".into(), "use bevy::prelude::*;\n\n/// Holds normalized movement intent.\n#[derive(Resource, Default)]\npub(crate) struct MovementInput {\n  /// Direction limited to unit length.\n  pub(crate) direction: Vec2,\n}\n\n/// Samples keyboard movement.\npub(crate) fn movement_input(\n  keys: Res<ButtonInput<KeyCode>>,\n  mut movement: ResMut<MovementInput>,\n);\n".into());
        design.proposed.insert("src/arena.rs".into(), "use bevy::prelude::*;\nuse crate::controls::MovementInput;\n\n/// Advances the player from sampled input.\npub(crate) fn move_player(\n  movement: Res<MovementInput>,\n);\n".into());
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, digest) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let mut source = file
            .capture_review_source("session", "plan", 1, &digest)
            .unwrap();
        assert!(source.public_only);
        source.public_only = false;
        let dependency_sources = Arc::clone(&source.resolver_sources);
        let document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let review = PlanReviewStore::default();
        let id = document.id.clone();
        review
            .insert(document, review.admit(id.clone()).unwrap())
            .unwrap();
        for (index, needle) in [
            "ResMut<MovementInput>",
            "Res<MovementInput>",
            "use crate::controls::MovementInput",
            "pub(crate) struct MovementInput",
        ]
        .into_iter()
        .enumerate()
        {
            let input = {
                let store = review.document.lock().unwrap();
                let document = &store.get(&id).unwrap().0;
                let snapshot = document.snapshot();
                let block = snapshot
                    .block
                    .iter()
                    .find(|block| block.text.row(0).is_some_and(|row| row.contains(needle)))
                    .unwrap();
                DocumentInput {
                    document: id.clone(),
                    revision: snapshot.revision,
                    view: ViewId("view".into()),
                    sequence: InputSequence(index as u64 + 1),
                    action: "jump_entity".into(),
                    block: block.id.clone(),
                    position: forge_buffer::block::TextPosition {
                        row: 0,
                        column: block.text.row(0).unwrap().find("MovementInput").unwrap() + 4,
                    },
                    target: block
                        .metadata
                        .target
                        .first()
                        .map(|target| target.id.clone()),
                }
            };
            review.prepare_declaration_jump(&input, None).await.unwrap();
            let result = review.action(input).unwrap();
            let jump: forge_buffer::block::BlockAnchor =
                serde_json::from_value(result["jump"].clone()).unwrap();
            let store = review.document.lock().unwrap();
            let destination = store
                .get(&id)
                .unwrap()
                .0
                .document
                .block(&jump.block)
                .unwrap()
                .text
                .row(0)
                .unwrap();
            assert!(
                destination.contains("pub(crate) struct MovementInput")
                    && destination[jump.position.column..].starts_with("MovementInput"),
                "{needle}: {result}"
            );
            assert!(
                dependency_sources.get().is_none(),
                "{needle} acquired unavailable Bevy sources"
            );
        }
    }

    #[test]
    fn annotation_capture_saves_exact_text_and_rejects_changed_source_atomically() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path(), temporary.path());
        let canonical = crate::plan::document::test_fixture("plan", "Initial");
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = file
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let source_path = source.path.with_extension("json");
        let original = std::fs::read(&source_path).unwrap();
        let digest = source.saved_digest.clone();
        let document_id = DocumentId("review".into());
        let document = PlanReviewDocument::new(
            document_id.clone(),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let line = document.source.rendered.navigation.anchor[0].line;
        let store = PlanReviewStore::default();
        let admission = store.admit(document_id.clone()).unwrap();
        store.insert(document, admission).unwrap();
        let annotation = ReviewAnnotation {
                parent_id: None,
                kind: Default::default(),
                reply: None,
            id: "local-comment".into(),
            anchor: None,
            source: super::super::PlanAnnotationInput {
                start_line: line,
                end_line: line,
                body: "literal **comment**\nλ\r\n".into(),
            },
        };
        store
            .save_annotations(&document_id, &digest, vec![annotation.clone()])
            .unwrap();
        assert_eq!(std::fs::read(&source_path).unwrap(), original);
        std::fs::write(&source_path, b"changed").unwrap();
        let mut newer = annotation.clone();
        newer.source.body = "not durable".into();
        assert!(
            store
                .save_annotations(&document_id, &digest, vec![newer])
                .is_err()
        );
        assert_eq!(
            store.document.lock().unwrap()[&document_id]
                .0
                .annotation
                .annotation()[0]
                .source
                .body,
            annotation.source.body
        );
        std::fs::write(&source_path, original).unwrap();
        let reopened = file
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let reopened = PlanReviewDocument::new(
            DocumentId("reopened".into()),
            ViewId("other".into()),
            reopened,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert_eq!(
            reopened.annotation.annotation()[0].source.body,
            annotation.source.body
        );
        store
            .save_annotations(&document_id, &digest, vec![])
            .unwrap();
        let deleted = file
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let deleted = PlanReviewDocument::new(
            DocumentId("deleted".into()),
            ViewId("deleted-view".into()),
            deleted,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert!(deleted.annotation.annotation().is_empty());
    }

    #[tokio::test]
    async fn closing_pending_plan_review_retains_admission_until_capture_is_collected() {
        let store = Arc::new(PlanReviewStore::default());
        let id = DocumentId("pending-review".into());
        let admission = store.admit(id.clone()).unwrap();
        let close = tokio::spawn({
            let store = Arc::clone(&store);
            let id = id.clone();
            async move { store.close(&id).await }
        });
        tokio::task::yield_now().await;
        assert!(admission.check().is_err());
        assert!(store.admit(id.clone()).is_err());
        assert!(!close.is_finished());
        drop(admission);
        tokio::time::timeout(std::time::Duration::from_secs(1), close)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        assert!(store.admit(id).is_ok());
    }

    #[test]
    fn physical_projection_preserves_source_and_rejects_stale_or_forged_targets() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let canonical = crate::plan::document::test_fixture("plan", "Initial");
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let mut document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("first".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let snapshot = document.snapshot();
        assert!(!snapshot.block.is_empty());
        assert_eq!(document.saved_digest().len(), 64);
        let target = snapshot.block[0].metadata.target[0].clone();
        let mut input = DocumentInput {
            document: snapshot.document,
            revision: snapshot.revision,
            view: ViewId("first".into()),
            sequence: InputSequence(1),
            action: "open".into(),
            block: snapshot.block[0].id.clone(),
            position: target.range.start,
            target: Some(target.id),
        };
        let anchor = document.action(input.clone()).unwrap();
        assert_eq!(anchor.line as usize, input.position.row + 1);
        assert!(document.action(input.clone()).is_err());
        input.sequence = InputSequence(2);
        input.target = Some(TargetId("forged".into()));
        assert!(document.action(input.clone()).is_err());
        document
            .update_view(ViewId("second".into()), Some(WidthProfile::default()))
            .unwrap();
        document.update_view(ViewId("first".into()), None).unwrap();
        assert!(document.action(input).is_err());
    }

    #[test]
    fn baseline_property_jump_opens_uncaptured_source_without_changing_the_plan() {
        let temporary = tempfile::tempdir().unwrap();
        std::fs::write(
            temporary.path().join("client.ts"),
            "export interface Client { count: number; }\n",
        )
        .unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical =
            crate::plan::document::test_fixture("plan", "Uncaptured property navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Review a captured caller".into();
        design.document.description = "Navigate to an uncaptured property definition".into();
        let caller = "import { Client } from './client';\nexport function run(client: Client): void { client.count; }\n";
        std::fs::write(temporary.path().join("run.ts"), caller).unwrap();
        let text = forge_diff::syntax::DeclarationOverview::extract("run.ts", caller).unwrap();
        design.baseline.insert(
            "run.ts".into(),
            crate::plan::DeclarationFile {
                text: text.clone(),
                source_digest: crate::plan::digest(caller.as_bytes()),
            },
        );
        design.proposed.insert("run.ts".into(), text);
        design.baseline_calls.insert(
            "run.ts".into(),
            crate::plan::calls::extract(
                "run.ts",
                "export function run(client: Client) { client.count; }",
            )
            .unwrap(),
        );
        canonical.design = Some(design);
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let document = PlanReviewDocument::new(
            DocumentId("uncaptured".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let saved = serde_json::to_vec(&document.source.document).unwrap();
        let design = document.source.document.design.as_ref().unwrap();
        assert!(
            document
                .source
                .resolver
                .set(std::sync::Mutex::new((
                    crate::declaration::DeclarationResolver::local(temporary.path(), design, false)
                        .unwrap(),
                    crate::declaration::DeclarationResolver::local(temporary.path(), design, true)
                        .unwrap(),
                )))
                .is_ok()
        );
        let mut anchor = document.target.values().next().unwrap().clone();
        anchor.target = crate::plan::PlanReviewTarget::Call {
            path: "run.ts".into(),
            side: "baseline".into(),
            owner: "run".into(),
            name: "Client.count".into(),
            kind: crate::plan::CallKind::Property,
        };
        let result = document
            .jump_declaration(&anchor, "  Client.count", 10)
            .unwrap();
        assert_eq!(
            std::path::Path::new(result["source"]["path"].as_str().unwrap()),
            temporary.path().join("client.ts")
        );
        assert_eq!(result["source"]["line"], 1);
        assert_eq!(result["source"]["column"], 26);
        assert!(result.get("declarations").is_none());
        assert_eq!(
            serde_json::to_vec(&document.source.document).unwrap(),
            saved
        );
    }
    #[test]
    fn property_navigation_reveals_filtered_uses_and_previews_planned_rename() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Property navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Add a client count property.".into();
        design.document.description = "Expose property uses for review.".into();
        design.proposed.insert("client.ts".into(), "/// Tracks requests.\nexport interface Client {\n  /// Current request count.\n  count: number;\n}\n".into());
        design.proposed.insert("run.ts".into(), "import { Client } from './client';\n/// Updates request count.\nfunction run(client: Client): void;\n".into());
        design.proposed_calls.insert("run.ts".into(), crate::plan::calls::extract("run.ts", "import { Client } from './client'; function run(client: Client) { client.count++; client.count; }").unwrap());
        canonical.design = Some(design);
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let mut document = PlanReviewDocument::new(
            DocumentId("properties".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let saved = serde_json::to_vec(&document.source.document).unwrap();
        let block = document
            .snapshot()
            .block
            .into_iter()
            .find(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|row| row.contains("count: number"))
            })
            .unwrap();
        let anchor = block
            .metadata
            .target
            .iter()
            .filter_map(|target| document.target.get(&target.id))
            .find(|anchor| {
                matches!(
                    &anchor.target,
                    crate::plan::PlanReviewTarget::Declaration { .. }
                )
            })
            .unwrap()
            .clone();
        let row = block.text.row(0).unwrap();
        let references = document
            .references(&anchor, row, row.find("count").unwrap())
            .unwrap();
        let property = references["references"]
            .as_array()
            .unwrap()
            .iter()
            .find(|reference| reference["kind"] == "property")
            .unwrap();
        let result = document
            .reveal_reference(property["id"].as_str().unwrap())
            .unwrap();
        let jump: forge_buffer::block::BlockAnchor =
            serde_json::from_value(result["jump"].clone()).unwrap();
        let use_block = document.document.block(&jump.block).unwrap();
        assert_eq!(use_block.text.row(0), Some("    Client.count"));
        let use_anchor = use_block
            .metadata
            .target
            .iter()
            .filter_map(|target| document.target.get(&target.id))
            .next()
            .unwrap()
            .clone();
        let renamed = document
            .rename(&use_anchor, "    Client.count", 12)
            .unwrap();
        assert_eq!(renamed["rename"]["name"], "count");
        assert!(renamed["rename"]["preview"].as_array().unwrap().len() >= 2);
        let definition = document
            .jump_declaration(&use_anchor, "    Client.count", 12)
            .unwrap();
        let destination: forge_buffer::block::BlockAnchor =
            serde_json::from_value(definition["jump"].clone()).unwrap();
        assert!(
            document
                .document
                .block(&destination.block)
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .contains("count: number")
        );
        assert_eq!(
            serde_json::to_vec(&document.source.document).unwrap(),
            saved
        );
    }

    #[test]
    fn references_reveal_filtered_calls_in_the_same_snapshot_without_editing_design() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Call navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Define call relationships.".into();
        design.document.description = "Expose sender relationships for review.".into();
        design.proposed.insert("client.ts".into(), "/// Sends a request.\nexport function send(): void;\n".into());
        design.proposed.insert("run.ts".into(), "import { send } from './client';\n/// Dispatches work.\nclass Runner { private run(): void; }\n".into());
        design.proposed_calls.insert("run.ts".into(), vec![crate::plan::FunctionBody { change: None, owner: "Runner.run".into(), call: Some(vec![crate::plan::CallSite { kind: crate::plan::CallKind::Call, name: "send".into(), source: None, unresolved: false }]) }]);
        design.baseline.insert("run.ts".into(), crate::plan::DeclarationFile { text: design.proposed["run.ts"].clone(), source_digest: String::new() });
        design.baseline_calls.insert("run.ts".into(), design.proposed_calls["run.ts"].clone());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", 1, &checksum).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("references".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let saved = serde_json::to_vec(&document.source.document).unwrap();
        assert!(document.source.public_only);
        assert!(!document.snapshot().block.iter().any(|block| block.text.row(0) == Some("function run(): void;")));
        let source_line: HashMap<_, _> = document.target.iter().map(|(id, anchor)| (id.clone(), anchor.line)).collect();
        let block = document.snapshot().block.into_iter().find(|block| block.text.row(0).is_some_and(|row| row.contains("export function send"))).unwrap();
        let anchor = block.metadata.target.iter().filter_map(|target| document.target.get(&target.id)).find(|anchor| matches!(&anchor.target, super::super::PlanReviewTarget::Declaration { .. })).unwrap().clone();
        let row = block.text.row(0).unwrap();
        let references = document.references(&anchor, row, row.find("send").unwrap()).unwrap();
        let call = references["references"].as_array().unwrap().iter().find(|reference| reference["kind"] == "call").unwrap();
        let result = document.reveal_reference(call["id"].as_str().unwrap()).unwrap();
        let jump: forge_buffer::block::BlockAnchor = serde_json::from_value(result["jump"].clone()).unwrap();
        assert_eq!(document.document.block(&jump.block).unwrap().text.row(0).unwrap().trim(), "send");
        assert!(!document.document.block(&jump.block).unwrap().metadata.gutter.is_empty());
        assert!(!document.snapshot().block.iter().any(|block| block.id.0.starts_with("plan:reference:")));
        assert_eq!(document.source_rows().iter().find(|row| row["block"] == jump.block.0).unwrap()["source_line"], 0);
        for (id, line) in source_line {
            assert_eq!(document.target.get(&id).unwrap().line, line);
        }
        assert!(document.document.block(&jump.block).unwrap().metadata.collapse.iter().any(|fold| !fold.closed && fold.id.0.starts_with("plan:calls:")));
        assert!(document.document.block(&jump.block).unwrap().metadata.collapse.iter().all(|fold| !fold.closed));
        assert_eq!(serde_json::to_vec(&document.source.document).unwrap(), saved);
        assert!(document.source.public_only);
        let revision = document.document.revision();
        document.reveal_reference(call["id"].as_str().unwrap()).unwrap();
        assert_eq!(document.document.revision(), revision);
    }
}
