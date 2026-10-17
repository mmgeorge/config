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
    /// Plan that owns the captured annotation and design.
    pub plan_id: String,
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
            question.push(PlanReviewQuestion { plan_id:document.source.document.plan_id.clone(), id:annotation.id.clone(), body:annotation.source.body.clone(), prompt });
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
        mut document: PlanReviewDocument,
        admission: DocumentAdmission,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        admission.check()?;
        let source_edit = document.source_edit();
        let opened = serde_json::json!({"snapshot":document.snapshot(), "saved_source_digest":document.saved_digest(),
            "plan_id":document.source.document.plan_id, "version":document.source.document.version,
            "path":document.source.path, "public_only":document.source.public_only,
            "annotation":document.annotation.annotation(), "source_edit":source_edit,
            "checks":document.source.document.design.as_ref().map(|design| design.check_options()).unwrap_or_default()});
        store.insert(document.id.clone(), (document, admission));
        Ok(opened)
    }

    /// Reuse analysis for unchanged file sides before preparing a newer plan revision.
    pub(crate) fn prepare_update(&self, id: &DocumentId, source: &mut PlanReviewSource) -> Result<u64> {
        let store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get(id).context("plan review document is closed")?;
        admission.check()?;
        ensure!(!document.source.historical && !source.historical, "historical plan revisions are read-only");
        ensure!(source.document.plan_id == document.source.document.plan_id
            && source.document.version > document.source.document.version, "plan revision is stale");
        if let (Some(previous), Some(current)) = (&document.source.document.design, &source.document.design) {
            for ((path, side), handle) in &document.source.declaration_syntax {
                let (before, after) = if side == "baseline" {
                    (previous.baseline.get(path).map(|file| &file.text), current.baseline.get(path).map(|file| &file.text))
                } else { (previous.proposed.get(path), current.proposed.get(path)) };
                let (before_calls, after_calls) = if side == "baseline" {
                    (previous.baseline_calls.get(path), current.baseline_calls.get(path))
                } else { (previous.proposed_calls.get(path), current.proposed_calls.get(path)) };
                if before.is_some() && before == after && before_calls == after_calls {
                    source.declaration_syntax.insert((path.clone(), side.clone()), handle.clone());
                }
            }
            if previous.proposed == current.proposed && previous.baseline == current.baseline
                && previous.proposed_calls == current.proposed_calls && previous.baseline_calls == current.baseline_calls {
                source.resolver = Arc::clone(&document.source.resolver);
            }
        }
        Ok(document.source.document.version)
    }

    /// Publish a prepared revision while retaining the document identity and current view choices.
    pub(crate) fn update(&self, id: &DocumentId, source: PlanReviewSource, expected_version: u64, trace: Option<&super::edit_trace::PlanEditTrace>) -> Result<serde_json::Value> {
        let wait = trace.map(|trace| trace.stage("review.lock_wait"));
        let mut store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        if let Some(stage) = wait { stage.complete(serde_json::json!({})); }
        let (document, admission) = store.get_mut(id).context("plan review document is closed")?;
        admission.check()?;
        ensure!(document.source.document.version == expected_version, "plan changed while preparing its projection");
        document.update(source, trace)
    }

    /// Prepare only the saved proposal and baseline declaration indexes.
    pub(crate) async fn prepare_declaration_jump(
        &self,
        input: &DocumentInput,
        trace: Option<&crate::declaration::trace::DeclarationTrace>,
    ) -> Result<()> {
        let store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get(&input.document).context("plan review document is closed")?;
        admission.check()?;
        document.check_input(input)?;
        if document.input_target(input)?.and_then(|id| document.target.get(&id))
            .is_some_and(|anchor| matches!(anchor.target, super::PlanReviewTarget::InlineSymbol { .. })) {
            return Ok(());
        }
        let Some(design) = &document.source.document.design else { return Ok(()); };
        if document.source.resolver.get().is_none() {
            let mut proposed = crate::declaration::DeclarationResolver::planned(&document.source.workspace, design, false)?;
            let mut baseline = crate::declaration::DeclarationResolver::planned(&document.source.workspace, design, true)?;
            proposed.trace = trace.cloned();
            baseline.trace = trace.cloned();
            document.source.resolver.set(Mutex::new((proposed, baseline)))
                .map_err(|_| anyhow::anyhow!("declaration resolver was already initialized"))?;
        }
        Ok(())
    }

    /// Accept a definition only while its original action and source remain current.
    pub(crate) fn definition(&self, input: &DocumentInput, source: &super::WorkspaceDefinition, target: &super::DefinitionLocation) -> Result<serde_json::Value> {
        let mut store = self.document.lock().map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get_mut(&input.document).context("plan review document is closed")?;
        admission.check()?;
        ensure!(matches!(input.action.as_str(), "jump_entity" | "open") && document.view.get(&input.view) == Some(&input.sequence), "definition action was superseded");
        let identity = document.input_target(input)?.context("definition action has no target")?;
        let anchor = document.target.get(&identity).context("definition target is unavailable")?.clone();
        let row = document.document.block(&input.block).and_then(|block| block.text.row(input.position.row)).context("definition row is unavailable")?;
        ensure!(document.workspace_definition(&anchor, row, input.position.column)?.as_ref() == Some(source), "definition source changed");
        ensure!(target.line > 0 && std::path::Path::new(&target.path).is_absolute(), "invalid definition location");
        let side = match &anchor.target {
            super::PlanReviewTarget::Call { side, .. } | super::PlanReviewTarget::Declaration { side, .. } => side.as_str(),
            _ => anyhow::bail!("definition target is unavailable"),
        };
        let design = document.source.document.design.as_ref().unwrap();
        let absolute = dunce::canonicalize(&target.path).context("definition file is unavailable")?;
        ensure!(absolute.is_file(), "definition target is not a file");
        let workspace = dunce::canonicalize(&document.source.workspace)?;
        if !target.modified && let Ok(relative) = absolute.strip_prefix(&workspace) {
            let relative = relative.to_string_lossy().replace('\\', "/");
            if let Some(baseline) = design.baseline.get(&relative)
                && let Some(real) = super::design::workspace_source(&workspace, &relative)?
                && super::digest(real.as_bytes()) == baseline.source_digest
            {
                let saved_path = if side == "baseline" { &relative } else { design.moved.get(&relative).unwrap_or(&relative) };
                let saved = if side == "baseline" { Some(&baseline.text) } else { design.proposed.get(saved_path) };
                if let Some(saved) = saved
                    && let Some(position) = super::workspace_definition::map_position(&relative, &real, saved, forge_diff::syntax::DeclarationPosition { line: target.line, column: target.column })?
                {
                    let name = saved.lines().nth(position.line.saturating_sub(1) as usize)
                        .and_then(|line| line.get(position.column as usize..)).unwrap_or("")
                        .chars().take_while(|character| character.is_alphanumeric() || *character == '_' || *character == '$').collect();
                    return document.declaration_destination(side, crate::declaration::DeclarationDestination {
                        path: document.source.workspace.join(saved_path).to_string_lossy().into_owned(),
                        line: position.line, column: position.column, name, proposed: true, module_file: false,
                    });
                }
            }
        }
        Ok(serde_json::json!({"source":target}))
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
            let text = forge_buffer::text::BufferText::from_rows(text.lines())?;
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
    reference_context: Mutex<HashMap<bool, super::reference_context::ReferenceContext>>,
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
    source_anchor: HashMap<String, serde_json::Value>,
    target: HashMap<TargetId, PlanNavigationAnchor>,
    view: HashMap<ViewId, InputSequence>,
}

impl PlanReviewDocument {
    fn update(&mut self, mut source: PlanReviewSource, trace: Option<&super::edit_trace::PlanEditTrace>) -> Result<serde_json::Value> {
        let total = trace.map(|trace| trace.stage("review.update"));
        ensure!(!self.source.historical && !source.historical, "historical plan revisions are read-only");
        ensure!(source.document.plan_id == self.source.document.plan_id
            && source.document.version > self.source.document.version, "plan revision is stale");
        source.public_only = self.source.public_only;
        source.revealed = self.source.revealed.iter().filter(|(path, side, _)| {
            let (Some(previous), Some(current)) = (&self.source.document.design, &source.document.design) else { return false; };
            if side == "baseline" {
                previous.baseline.get(path) == current.baseline.get(path)
                    && previous.baseline_calls.get(path) == current.baseline_calls.get(path)
            } else {
                previous.proposed.get(path) == current.proposed.get(path)
                    && previous.proposed_calls.get(path) == current.proposed_calls.get(path)
            }
        }).cloned().collect();
        source.collapse = self.source.collapse.clone();
        let annotation_stage = trace.map(|trace| trace.stage("review.load_reanchor_annotations"));
        let mut annotation = ReviewAnnotationStore::open(source.path.with_extension("json"), source.saved_digest.clone())?;
        if source.saved_navigation.is_some() { annotation.reanchor(&source.rendered.navigation)?; }
        self.retain_annotation_revision();
        if let Some(stage) = annotation_stage { stage.complete(serde_json::json!({"count":annotation.annotation().len()})); }
        let projection = trace.map(|trace| trace.stage("review.project"));
        let mut expected = self.source.document.clone();
        expected.version = source.document.version;
        let tests_only = match (&mut expected.design, &source.document.design) {
            (Some(previous), Some(current)) => {
                previous.document.tests.clone_from(&current.document.tests);
                expected == source.document
            }
            _ => false,
        };
        let (block, target) = if tests_only {
            super::review_projection::update_tests(&source, &self.source.rendered,
                self.document.snapshot().block, self.target.clone())?
        } else {
            super::review_projection::project(&source, &self.width, &[],
                &self.annotation_revision, self.focused_annotation.as_deref())?
        };
        if let Some(stage) = projection { stage.complete(serde_json::json!({"count":block.len(),"cached":tests_only})); }
        let reanchor = trace.map(|trace| trace.stage("review.retain_annotations"));
        let previous = self.document.snapshot();
        let remap = |line| {
            let anchor = self.source.rendered.navigation.resolve_line(line)?;
            if let Some(current) = source.rendered.navigation.anchor.iter().find(|current| current.target == anchor.target) {
                return Some(current.clone());
            }
            for previous_block in &previous.block {
                for previous_range in &previous_block.metadata.target {
                    if self.target.get(&previous_range.id).is_none_or(|candidate| candidate.line != line) { continue; }
                    let current_block = block.iter().find(|current| current.id == previous_block.id)?;
                    let current_range = current_block.metadata.target.iter()
                        .find(|current| current.range.start.row == previous_range.range.start.row)?;
                    return target.get(&current_range.id).cloned();
                }
            }
            None
        };
        let mut retained = annotation.annotation().to_vec();
        for original in self.annotation.annotation() {
            if retained.iter().any(|saved| saved.id == original.id) { continue; }
            let (Some(start), Some(end)) = (remap(original.source.start_line), remap(original.source.end_line)) else { continue; };
            if original.parent_id.as_ref().is_some_and(|parent| !retained.iter().any(|saved| &saved.id == parent)) { continue; }
            let mut saved = original.clone();
            saved.source.start_line = start.line.min(end.line);
            saved.source.end_line = start.line.max(end.line);
            saved.anchor = Some(super::ReviewAnnotationAnchor { start: start.target, end: end.target });
            retained.push(saved);
        }
        if retained.len() != annotation.annotation().len() { annotation.replace(retained)?; }
        if let Some(stage) = reanchor { stage.complete(serde_json::json!({"count":annotation.annotation().len()})); }
        let edit = trace.map(|trace| trace.stage("review.edit_document"));
        let patch = super::review_delivery::replace(&mut self.document, block)?;
        if let Some(stage) = edit { stage.complete(serde_json::json!({})); }
        self.source = source;
        if !tests_only {
            self.reference = Default::default();
            self.reference_context = Default::default();
        }
        self.target = target;
        self.annotation = annotation;
        let response = trace.map(|trace| trace.stage("review.build_delivery"));
        let source_edit = self.source_edit();
        let result = serde_json::json!({"patch":patch, "source_edit":source_edit,
            "annotation":self.annotation.annotation(), "public_only":self.source.public_only,
            "saved_source_digest":self.saved_digest(), "version":self.source.document.version,
            "checks":self.source.document.design.as_ref().map(|design| design.check_options()).unwrap_or_default()});
        if let Some(stage) = response { stage.complete(serde_json::json!({})); }
        if let Some(stage) = total { stage.complete(serde_json::json!({"revision":self.source.document.version})); }
        Ok(result)
    }

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
        resolver.discard_snapshot_originals();
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
        let occurrences = index.occurrence.iter().filter(|reference| {
            reference.symbol == symbol && !(reference.kind == "import" && reference.path.ends_with(".rs"))
        }).collect::<Vec<_>>();
        if occurrences.is_empty() { return Ok(serde_json::json!({"message":"No references in this plan snapshot."})); }
        let mut context = self.reference_context.lock().map_err(|_| anyhow::anyhow!("plan reference context cache lock poisoned"))?;
        if !context.contains_key(&baseline) {
            context.insert(baseline, super::reference_context::ReferenceContext::build(&self.source, baseline)?);
        }
        let context = context.get(&baseline).unwrap();
        let entries = occurrences.into_iter().map(|reference| context.entry(reference)).collect::<Result<Vec<_>>>()?;
        Ok(serde_json::json!({"references":entries,"revision":self.document.revision()}))
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
                        super::PlanReviewTarget::Declaration { path, side, line, .. } => !matches!(reference.kind.as_str(), "call" | "property" | "callback" | "value" | "construct") && path == &reference.path && side == &reference.side && *line == reference.line,
                        _ => reference.anchor.as_ref().is_some_and(|saved| saved.json_path == anchor.json_path),
                    };
                    selected.then(|| (index, forge_buffer::block::BlockAnchor {
                        block: block.id.clone(), position: TextPosition { row: range.range.start.row,
                            column: if matches!(reference.kind.as_str(), "call" | "property" | "callback" | "value" | "construct") { block.text.row(range.range.start.row).unwrap_or("").find(&reference.name).unwrap_or(0) } else { reference.column as usize } },
                    }))
                })
            })
        };
        if let Some((_, jump)) = locate(&self.document.snapshot().block, &self.target) {
            return Ok(serde_json::json!({"jump":jump}));
        }
        let design = self.source.document.design.as_ref().context("reference has no declaration design")?;
        let (text, calls) = if reference.side == "baseline" {
            (design.baseline.get(&reference.path).map(|file| file.text.as_str()), design.baseline_calls.get(&reference.path))
        } else {
            (design.proposed.get(&reference.path).map(String::as_str), design.proposed_calls.get(&reference.path))
        };
        let presentation = super::calls::present(&reference.path, text.context("reference file is missing")?, calls.map(Vec::as_slice).unwrap_or_default())?;
        let selected_row = if matches!(reference.kind.as_str(), "call" | "property" | "callback" | "value" | "construct") {
            presentation.call_row.iter().find_map(|(row, (owner, name, kind))| {
                (owner == &reference.owner && name == &reference.name && kind.label() == reference.kind).then_some(*row)
            })
        } else { None }.or_else(|| presentation.declaration.source.iter().position(|position| position.is_some_and(|position| position.line == reference.line)))
            .context("reference has no declaration row")?;
        self.reveal_target(&reference.path, &reference.side, selected_row, locate)
    }

    fn reveal_target(
        &mut self,
        path: &str,
        side: &str,
        selected_row: usize,
        locate: impl Fn(&[forge_buffer::block::BufferBlock], &HashMap<TargetId, PlanNavigationAnchor>) -> Option<(usize, forge_buffer::block::BlockAnchor)>,
    ) -> Result<serde_json::Value> {
        self.retain_annotation_revision();
        let previous_revealed = self.source.revealed.clone();
        let previous_collapse = self.source.collapse.clone();
        self.source.revealed.retain(|(revealed_path, _, _)| revealed_path != path);
        self.source.revealed.insert((path.to_owned(), side.to_owned(), selected_row as u32));
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
            let patch = super::review_delivery::replace(&mut self.document, blocks)?;
            Ok((patch, targets, jump))
        })();
        if projection.is_err() {
            self.source.revealed = previous_revealed;
            self.source.collapse = previous_collapse;
        }
        let (patch, targets, jump) = projection?;
        self.target = targets;
        self.focused_annotation = None;
        let source_edit = self.source_edit();
        Ok(serde_json::json!({"patch":patch,"source_edit":source_edit,"annotation":self.annotation.annotation(),"jump":jump}))
    }

    fn describe(
        &mut self,
        anchor: PlanNavigationAnchor,
        action: &str,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        use super::PlanReviewTarget;
        if let PlanReviewTarget::InlineSymbol { name } = &anchor.target {
            let design = self.source.document.design.as_ref().context("declaration design is unavailable")?;
            let symbols = super::review_symbols::NarrativeSymbols::new(design, &self.source.workspace);
            let matching = symbols.matching(name);
            if action == "jump_entity" || action == "open" || action.starts_with("jump_symbol:") {
                if matching.len() == 1 {
                    return self.declaration_destination("proposed", matching[0].destination.clone());
                }
                if let Some(index) = action.strip_prefix("jump_symbol:") {
                    let index = index.parse::<usize>().context("invalid symbol selection")?;
                    let symbol = matching.get(index).context("symbol selection is no longer available")?;
                    return self.declaration_destination("proposed", symbol.destination.clone());
                }
                if !matching.is_empty() {
                    let choices = matching.iter().enumerate().map(|(index, symbol)| serde_json::json!({
                        "index": index, "label": format!("{}:{} — {}", symbol.destination.path, symbol.destination.line, symbol.destination.name)
                    })).collect::<Vec<_>>();
                    return Ok(serde_json::json!({"symbol_choices": choices}));
                }
                if symbols.removed(name) {
                    return Ok(serde_json::json!({"message":format!("{name} is removed by this plan")}));
                }
                let excluded = design.baseline.keys().chain(design.proposed.keys()).map(|path| self.source.workspace.join(path)).collect::<Vec<_>>();
                return Ok(serde_json::json!({"workspace_symbol":{"name":name,"workspace":self.source.workspace,"excluded":excluded}}));
            }
            return Ok(serde_json::json!({"message":"Use go to definition on this inline reference"}));
        }
        if self.source.document.design.is_some() {
            ensure!(matches!(action, "jump_entity" | "open"), "this design action is unavailable");
            if let PlanReviewTarget::File { path } = &anchor.target {
                return Ok(serde_json::json!({"source":{"path":self.source.workspace.join(path),"line":1,"column":0}}));
            }
            return self.jump_declaration(&anchor, row, column);
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
            let resolver = self.source.resolver.get().context("declaration resolver is unavailable")?;
            let mut resolver = resolver.lock().map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
            let resolver = if side == "baseline" { &mut resolver.1 } else { &mut resolver.0 };
            return Ok(match kind { super::CallKind::Call | super::CallKind::Callback => resolver.callable(path, owner, name), super::CallKind::Property => resolver.property(path, owner, name), super::CallKind::Value => resolver.value(path, owner, name), super::CallKind::Construct => resolver.construct(path, owner, name) });
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

    fn workspace_definition(&self, anchor: &PlanNavigationAnchor, row: &str, column: usize) -> Result<Option<super::WorkspaceDefinition>> {
        use forge_diff::syntax::{DeclarationCalls, DeclarationOverview, DeclarationPosition};
        let Some(design) = &self.source.document.design else { return Ok(None); };
        let (path, side) = match &anchor.target {
            super::PlanReviewTarget::Call { path, side, .. } | super::PlanReviewTarget::Declaration { path, side, .. } => (path, side),
            _ => return Ok(None),
        };
        let source_path = if side == "baseline" { path } else {
            design.moved.iter().find(|(_, destination)| *destination == path).map(|(original, _)| original).unwrap_or(path)
        };
        let Some(baseline) = design.baseline.get(source_path) else { return Ok(None); };
        let Some(source) = super::design::workspace_source(&self.source.workspace, source_path)? else { return Ok(None); };
        if super::digest(source.as_bytes()) != baseline.source_digest { return Ok(None); }
        let position = match &anchor.target {
            super::PlanReviewTarget::Call { owner, name, kind, .. } => {
                let calls = if side == "baseline" { &design.baseline_calls } else { &design.proposed_calls };
                let Some(function) = calls.get(path).into_iter().flatten().find(|function| function.owner == *owner) else { return Ok(None); };
                let captured = function.call.iter().flatten().filter(|call| call.name == *name && call.kind == *kind)
                    .filter_map(|call| call.source.as_ref()).collect::<Vec<_>>();
                let Some(first) = captured.first() else { return Ok(None); };
                // One displayed target may represent several occurrences. Distinct receivers must not be guessed.
                if captured.iter().any(|candidate| candidate.expression != first.expression) { return Ok(None); }
                let extracted = DeclarationCalls::extract(source_path, &source, false).map_err(|error| anyhow::anyhow!("{error:?}"))?;
                let originals = extracted.iter().find(|function| function.owner == *owner)
                    .into_iter().flat_map(|function| &function.call).filter(|call| {
                        call.line == first.line && call.column == first.column
                            && (first.expression.is_empty() || call.expression == first.expression)
                            || first.expression.is_empty() && call.name == *name
                    }).collect::<Vec<_>>();
                let Some(original) = originals.first() else { return Ok(None); };
                if originals.iter().any(|candidate| candidate.expression != original.expression) { return Ok(None); }
                Some(DeclarationPosition { line: original.line, column: original.column })
            }
            super::PlanReviewTarget::Declaration { line, column: saved_column, .. } => {
                let saved = if side == "baseline" { &baseline.text } else {
                    let Some(saved) = design.proposed.get(path) else { return Ok(None); };
                    saved
                };
                let position = DeclarationOverview::token_position(path, saved, DeclarationPosition {
                    line: *line, column: saved_column.unwrap_or(0),
                }, row, column).map_err(|error| anyhow::anyhow!("{error:?}"))?;
                if super::workspace_definition::map_position(path, saved, &baseline.text, position)?.is_none() { return Ok(None); }
                super::workspace_definition::map_position(path, saved, &source, position)?
            }
            _ => None,
        };
        Ok(position.map(|position| super::WorkspaceDefinition {
            path: self.source.workspace.join(source_path).to_string_lossy().into_owned(),
            digest: baseline.source_digest.clone(), line: position.line, column: position.column,
            filetype: DeclarationOverview::filetype(path).into(),
        }))
    }

    fn jump_declaration(
        &mut self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        let mut selected = anchor.clone();
        let algorithm = matches!(anchor.target, super::PlanReviewTarget::Behavior { .. });
        if let super::PlanReviewTarget::Behavior { path, side, owner, offset } = &anchor.target {
            let design = self.source.document.design.as_ref().context("declaration design is unavailable")?;
            let (declarations, bodies) = if side == "baseline" {
                (design.baseline.get(path).map(|file| file.text.as_str()), &design.baseline_calls)
            } else { (design.proposed.get(path).map(String::as_str), &design.proposed_calls) };
            let body = bodies.get(path).into_iter().flatten().find(|body| body.owner == *owner)
                .context("Algorithm callable is unavailable")?;
            let reference = match super::calls::algorithm_reference(path, declarations.context("Algorithm declarations are unavailable")?, body, *offset, row, column) {
                Ok(Some(reference)) => reference,
                Ok(None) => return Ok(serde_json::json!({"message":"Select a parsed identifier in the Algorithm."})),
                Err(error) => return Ok(serde_json::json!({"message":error.to_string()})),
            };
            match reference {
                super::calls::AlgorithmTarget::Binding { name, position } => {
                    return self.declaration_destination(side, crate::declaration::DeclarationDestination {
                        path: self.source.workspace.join(path).to_string_lossy().into_owned(),
                        line: position.line, column: position.column, proposed: true, name, module_file: false,
                    });
                }
                super::calls::AlgorithmTarget::Reference(reference) => {
                    selected.target = super::PlanReviewTarget::Call {
                        path: path.clone(), side: side.clone(), owner: owner.clone(), name: reference.name, kind: reference.kind,
                    };
                }
            }
        }
        let anchor = &selected;
        if !algorithm && let Some(source) = self.workspace_definition(anchor, row, column)? {
            return Ok(serde_json::json!({"workspace_definition":source}));
        }
        use crate::declaration::DeclarationResolution;
        let result = self.resolve_declaration(anchor, row, column)?;
        let side = match &anchor.target {
            super::PlanReviewTarget::Declaration { side, .. } | super::PlanReviewTarget::Call { side, .. } => side,
            _ => anyhow::bail!("select a declaration type, import, or call"),
        };
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
        self.declaration_destination(side, destination)
    }

    fn declaration_destination(&mut self, side: &str, destination: crate::declaration::DeclarationDestination) -> Result<serde_json::Value> {
        let design = self.source.document.design.as_ref().context("declaration design is unavailable")?;
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
                let captured = if side == "baseline" {
                    design.baseline.contains_key(relative)
                } else {
                    design.proposed.contains_key(relative)
                };
                if captured {
                    return self.reveal_target(relative, side, 0, |blocks, targets| {
                        blocks.iter().enumerate().find_map(|(index, block)| {
                            block.metadata.target.iter().any(|range| targets.get(&range.id).is_some_and(|anchor|
                                matches!(&anchor.target, super::PlanReviewTarget::File { path } if path == relative)))
                                .then(|| (index, forge_buffer::block::BlockAnchor {
                                    block: block.id.clone(), position: TextPosition { row: 0, column: 0 },
                                }))
                        })
                    });
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
            let calls = if side == "baseline" { &design.baseline_calls } else { &design.proposed_calls };
            let combined = super::calls::present(&relative, text, calls.get(&relative).map(Vec::as_slice).unwrap_or_default())?;
            let selected_row = *combined.declaration_row.get(&(display_position.line as usize - 1))
                .context("definition has no captured display row")?;
            let column = display_position.column as usize;
            return self.reveal_target(&relative, side, selected_row, |blocks, targets| {
                blocks.iter().enumerate().find_map(|(index, block)| {
                    block.metadata.target.iter().find_map(|range| {
                        let anchor = targets.get(&range.id)?;
                        let selected = matches!(&anchor.target,
                            super::PlanReviewTarget::Declaration { path, side: target_side, line, column: saved_column }
                                if path == &relative && target_side == side
                                && row_anchor.is_some_and(|position| position.line == *line && Some(position.column) == *saved_column));
                        (selected && block.text.row(0).and_then(|row| row.get(column..)).is_some_and(|rest| rest.starts_with(&destination.name)))
                            .then(|| (index, forge_buffer::block::BlockAnchor {
                                block: block.id.clone(), position: TextPosition { row: 0, column },
                            }))
                    })
                })
            });
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
            reference_context: Default::default(),
            focused_annotation,
            annotation_revision: HashMap::new(),
            id,
            source,
            document,
            source_anchor: HashMap::new(),
            target,
            annotation,
            width,
            view_width,
            view: HashMap::from([(view, InputSequence(0))]),
        })
    }

    fn source_edit(&mut self) -> serde_json::Value {
        let mut row = Vec::new();
        let mut current = HashMap::new();
        let mut row_count = 0;
        let tests = self.source.document.design.iter().flat_map(|design| &design.document.tests)
            .flat_map(|file| file.cases.iter().map(move |case| ((file.file.as_str(), case.name.as_str()), case.change)))
            .collect::<HashMap<_, _>>();
        let mut test_summary = serde_json::json!({"new":0,"modified":0,"removed":0,"reused":0});
        for change in tests.values() {
            let name = serde_json::to_value(change).expect("test change");
            let count = &mut test_summary[name.as_str().unwrap()];
            *count = serde_json::json!(count.as_u64().unwrap() + 1);
        }
        for block in self.document.blocks(self.document.revision(), 0..self.document.block_count()).expect("current document range") {
            for position in 0..block.text.row_count() {
                let target = block.metadata.target.iter().find(|target| {
                    target.range.start.row <= position && position < target.range.end.row
                });
                let line = target
                    .and_then(|target| self.target.get(&target.id))
                    .map(|anchor| anchor.line)
                    .unwrap_or(row_count + 1);
                let test = target.and_then(|target| self.target.get(&target.id)).and_then(|anchor| {
                    match &anchor.target {
                        super::PlanReviewTarget::TestCase { path, name } => Some(super::DesignTestSelection { file: path.clone(), name: name.clone() }),
                        _ => None,
                    }
                });
                let check = target.and_then(|target| self.target.get(&target.id)).and_then(|anchor| {
                    if let super::PlanReviewTarget::Check { id } = &anchor.target { Some(id) } else { None }
                });
                let test_change = test.as_ref().and_then(|test| tests.get(&(test.file.as_str(), test.name.as_str())));
                let test_file = target.and_then(|target| self.target.get(&target.id)).and_then(|anchor| {
                    matches!(anchor.target, super::PlanReviewTarget::Section { section: super::PlanSection::Tests })
                        .then_some(anchor.path.as_ref()).flatten()
                });
                let id = format!("{}:{}", block.id.0, position);
                let anchor = serde_json::json!({"id":id,"source_line":line,"test":test,"check":check,
                    "test_change":test_change,"test_file":test_file,
                    "test_summary":(block.id.0 == "plan:section:tests").then_some(&test_summary)});
                if self.source_anchor.get(&id) != Some(&anchor) { row.push(anchor.clone()); }
                current.insert(id, anchor);
                row_count += 1;
            }
        }
        let mut removed = self.source_anchor.keys().filter(|id| !current.contains_key(*id)).cloned().collect::<Vec<_>>();
        removed.sort();
        self.source_anchor = current;
        serde_json::json!({"upsert":row,"remove":removed})
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
        let sequence = self
            .view
            .get(&input.view)
            .context("plan review view is closed")?;
        ensure!(
            input.sequence.0 > sequence.0,
            "plan review input was superseded"
        );
        self.input_target(input)
    }

    fn input_target(&self, input: &DocumentInput) -> Result<Option<TargetId>> {
        input.validate()?;
        ensure!(
            input.document == self.id && input.revision == self.document.revision(),
            "plan review input revision changed"
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
            let patch = super::review_delivery::replace(&mut self.document, block)?;
            Ok((patch, target))
        });
        if projection.is_err() { self.source.public_only = !public_only; }
        let (patch, target) = projection?;
        self.source.public_only = public_only;
        self.focused_annotation = None;
        self.target = target;
        let source_edit = self.source_edit();
        Ok(
            serde_json::json!({"patch":patch, "source_edit":source_edit, "annotation":self.annotation.annotation(), "public_only":public_only}),
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
            let patch = super::review_delivery::replace(&mut self.document, block)?;
            Ok((patch, target))
        });
        if projection.is_err() {
            if let Some(previous) = previous { self.source.collapse.insert(collapse.id.clone(), previous); }
            else { self.source.collapse.remove(&collapse.id); }
        }
        let (patch, target) = projection?;
        self.target = target;
        self.focused_annotation = None;
        let source_edit = self.source_edit();
        Ok(serde_json::json!({"patch":patch,"source_edit":source_edit,
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
            let patch = super::review_delivery::replace(&mut self.document, block)?;
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
        let source_edit = self.source_edit();
        Ok(
            serde_json::json!({"patch":patch, "source_edit":source_edit, "annotation":self.annotation.annotation()}),
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn property_definition_reveals_private_field_in_the_plan() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Property definition");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Update the client counter.".into();
        design.document.background = "The counter is private to its module.".into();
        design.document.requirements = vec!["Keep the field private.".into()];
        design.document.design = "The update function changes the counter.".into();
        design.proposed.insert("src/lib.rs".into(), "pub struct Client { count: u32 }\npub fn update(client: &mut Client);\n".into());
        design.proposed_calls.insert("src/lib.rs".into(), crate::plan::calls::extract("src/lib.rs", "pub fn update(client: &mut Client) { client.count += 1; }").unwrap());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("review".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let saved = document.source.document.clone();
        assert!(!document.snapshot().block.iter().any(|block| block.text.row(0).is_some_and(|row| row.contains("count: u32"))));
        let text = &document.source.document.design.as_ref().unwrap().proposed["src/lib.rs"];
        let symbol = forge_diff::syntax::DeclarationIndex::extract("src/lib.rs", text).unwrap().symbol.into_iter().find(|symbol| symbol.name == "count").unwrap();
        let result = document.declaration_destination("proposed", crate::declaration::DeclarationDestination {
            path: store.workspace.join("src/lib.rs").to_string_lossy().into_owned(), line: symbol.position.line,
            column: symbol.position.column, proposed: true, name: "count".into(), module_file: false,
        }).unwrap();
        assert!(result.get("declarations").is_none(), "{result}");
        assert!(result["patch"].is_object(), "{result}");
        let jump: forge_buffer::block::BlockAnchor = serde_json::from_value(result["jump"].clone()).unwrap();
        assert!(document.document.block(&jump.block).unwrap().text.row(0).unwrap()[jump.position.column..].starts_with("count"));
        assert!(document.source.public_only);
        assert_eq!(document.source.document, saved);
    }

    #[tokio::test]
    async fn narrative_symbols_prefer_proposals_and_keep_exact_targets() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Narrative references");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Describe the declarations.".into();
        design.document.background = "Review proposed declarations.".into();
        design.document.requirements = vec!["Keep exact symbol identities.".into()];
        design.document.design = "Use `Health` in `configure_game` with `External`. Remove `Retired`.".into();
        design.proposed.insert("model.ts".into(), "interface Health {}\nexport function configure_game(health: Health): void;\n".into());
        design.baseline.insert("model.ts".into(), crate::plan::DeclarationFile {
            text: "interface Retired {}\n".into(), source_digest: super::super::digest(b"interface Retired {}\n"),
        });
        std::fs::write(temporary.path().join("model.ts"), "interface Retired {}\n").unwrap();
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
        let document = PlanReviewDocument::new(DocumentId("review".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        assert!(document.source.public_only);
        let snapshot = document.snapshot();
        let block = snapshot.block.iter().find(|block| block.text.row(0).is_some_and(|row| row.starts_with("Use `Health`"))).unwrap();
        assert!(block.metadata.decoration.iter().any(|span| span.capture == "@type"));
        assert!(block.metadata.decoration.iter().any(|span| span.capture == "@function"));
        println!("NARRATIVE_REVIEW_FIXTURE:{}", serde_json::to_string(&snapshot).unwrap());
        let input = |name: &str, sequence| {
            let position = forge_buffer::block::TextPosition { row: 0, column: block.text.row(0).unwrap().find(name).unwrap() };
            DocumentInput {
                document: document.id.clone(), revision: snapshot.revision, view: ViewId("view".into()),
                sequence: InputSequence(sequence), action: "jump_entity".into(), block: block.id.clone(), position,
                target: block.metadata.target.iter().find(|target| target.range.start <= position && position < target.range.end).map(|target| target.id.clone()),
            }
        };
        let health = input("Health", 3);
        let external = input("External", 1);
        let retired = input("Retired", 2);
        let review = PlanReviewStore::default();
        review.insert(document, review.admit(health.document.clone()).unwrap()).unwrap();
        let result = review.action(external).unwrap();
        assert_eq!(result["workspace_symbol"]["name"], "External");
        assert_eq!(result["workspace_symbol"]["excluded"].as_array().unwrap().len(), 2);
        let result = review.action(retired).unwrap();
        assert_eq!(result["message"], "Retired is removed by this plan");
        review.prepare_declaration_jump(&health, None).await.unwrap();
        let result = review.action(health.clone()).unwrap();
        assert!(result["jump"].is_object() && result["patch"].is_object(), "private proposed symbol must reveal its definition in the review: {result}");
        assert!(result.get("declarations").is_none());
        assert!(review.document.lock().unwrap().get(&health.document).unwrap().0.source.resolver.get().is_none(), "narrative lookup acquired unrelated dependency sources");
    }

    use super::*;

    #[test]
    fn updates_send_only_changed_blocks_and_sparse_source_anchors() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Inspect configuration.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose configuration.".into();
        design.document.background = "Captured declarations.".into();
        design.document.requirements = vec!["Preserve behavior.".into()];
        design.document.design = "Change one field.".into();
        let baseline = (0..1000).map(|index| format!("pub struct State{index:04};\n")).collect::<String>();
        std::fs::create_dir_all(store.workspace.join("src")).unwrap();
        std::fs::write(store.workspace.join("src/lib.rs"), &baseline).unwrap();
        design.baseline.insert("src/lib.rs".into(), crate::plan::DeclarationFile { text: baseline.clone(), source_digest: super::super::digest(baseline.as_bytes()) });
        design.proposed.insert("src/lib.rs".into(), baseline.replace("State0500;", "State0500 { pub changed: bool }"));
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("review".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let initial = serde_json::json!({"snapshot":document.snapshot(),"source_edit":document.source_edit()});
        let encoded = serde_json::to_string(&initial).unwrap();
        assert!(encoded.len() < 80_000, "compact open grew to {} bytes", encoded.len());
        assert!(!encoded.contains("State0900"));
        for row in initial["source_edit"]["upsert"].as_array().unwrap() {
            assert!(row.get("text").is_none() && row.get("metadata").is_none());
        }
        let idle = document.update_view(ViewId("view".into()), Some(WidthProfile::default())).unwrap();
        assert!(idle.get("snapshot").is_none() && idle.get("source_row").is_none());
        assert_eq!(idle["source_edit"]["upsert"], serde_json::json!([]));
        let mut source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
        source.document.version += 1;
        source.document.design.as_mut().unwrap().document.objective = "Updated objective.".into();
        source.rendered = super::super::render_plan_at(&source.document, &store.workspace).unwrap();
        let updated = document.update(source, None).unwrap();
        assert!(updated.get("snapshot").is_none() && updated.get("source_row").is_none());
        let encoded = serde_json::to_string(&updated).unwrap();
        assert!(!encoded.contains("State0500"), "unchanged declaration leaked into update");
        assert!(encoded.len() < 12_000, "single metadata edit grew to {} bytes", encoded.len());
        assert_eq!(updated["source_edit"]["upsert"], serde_json::json!([]));
    }

    #[test]
    fn test_inventory_revision_preserves_document_and_declaration_state() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Inspect configuration.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose configuration.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Reject invalid configuration.".into();
        design.proposed.insert("src/lib.rs".into(), "pub struct Config { pub count: u32 }\n".into());
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"tests/config.rs","cases":[
                {"name":"first","change":"new","description":"Reject invalid input."},
                {"name":"keep","change":"new","description":"Accept valid input."}
            ]}
        ])).unwrap();
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("review".into()), ViewId("view".into()),
            source, WidthProfile::default(), None).unwrap();
        let opening = document.snapshot().block.into_iter()
            .find(|block| block.text.row(0).is_some_and(|row| row.contains("pub struct Config"))).unwrap();
        document.toggle_declaration(&opening.id).unwrap();
        document.toggle_public().unwrap();
        let resolver = Arc::clone(&document.source.resolver);
        let collapse = document.source.collapse.clone();
        let revision = document.snapshot().revision;
        let (_, _, next_digest) = store.remove_tests("session", "plan", 1, &digest,
            &[crate::plan::DesignTestSelection { file: "tests/config.rs".into(), name: "first".into() }], None).unwrap();
        let mut source = store.capture_review_source("session", "plan", None, &next_digest).unwrap();
        source.resolver = Arc::clone(&resolver);
        let result = document.update(source, None).unwrap();
        let rebuilt = super::super::review_projection::project(&document.source, &document.width, &[],
            &document.annotation_revision, document.focused_annotation.as_deref()).unwrap();
        assert_eq!(document.snapshot().block, rebuilt.0, "test fast path differs from a full projection");
        assert_eq!(document.target, rebuilt.1, "test fast path shifted a semantic target incorrectly");
        assert_eq!(document.source.rendered, super::super::render_plan_at(&document.source.document, temporary.path()).unwrap());
        assert!(document.snapshot().revision > revision);
        assert_eq!(document.snapshot().document, DocumentId("review".into()));
        assert_eq!(document.source.collapse, collapse);
        assert!(!document.source.public_only);
        assert!(Arc::ptr_eq(&document.source.resolver, &resolver));
        assert_eq!(document.source.document.design.as_ref().unwrap().document.tests[0].cases[0].name, "keep");
        assert_eq!(result["version"], 2);
        let target = document.document.block(&opening.id).unwrap().metadata.target.first().unwrap();
        let captured = DocumentInput { document: DocumentId("review".into()), view: ViewId("view".into()),
            revision: document.snapshot().revision, sequence: InputSequence(1), block: opening.id,
            position: target.range.start, target: Some(target.id.clone()), action: "delete_tests".into() };
        document.validate_input(captured).unwrap();
        let stale = store.capture_review_source("session", "plan", None, &next_digest).unwrap();
        assert!(document.update(stale, None).unwrap_err().to_string().contains("stale"));
        let (_, _, empty_digest) = store.remove_tests("session", "plan", 2, &next_digest,
            &[crate::plan::DesignTestSelection { file: "tests/config.rs".into(), name: "keep".into() }], None).unwrap();
        let source = store.capture_review_source("session", "plan", None, &empty_digest).unwrap();
        document.update(source, None).unwrap();
        let rebuilt = super::super::review_projection::project(&document.source, &document.width, &[],
            &document.annotation_revision, document.focused_annotation.as_deref()).unwrap();
        assert_eq!(document.snapshot().block, rebuilt.0);
        assert_eq!(document.target, rebuilt.1);
        assert!(document.source.rendered.markdown.contains("Tests · None planned"));
        assert!(!document.snapshot().block.iter().any(|block| block.id.0.starts_with("plan:tests:file:")));
        assert_eq!(document.source.rendered, super::super::render_plan_at(&document.source.document, temporary.path()).unwrap());
    }

    #[test]
    fn declaration_collapse_preserves_source_and_visibility_choices() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Inspect containers.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose configuration.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Reject invalid configuration.".into();
        design.proposed.insert("src/lib.rs".into(), "#[derive(Debug)]\n/// Rejects invalid dimensions.\npub enum ConfigError {\n  /// Invalid arena.\n  ArenaSize,\n}\n\n/// Stores configuration.\npub struct Config {\n  /// Internal limit.\n  limit: u32,\n  /// Public count.\n  pub count: u32,\n}\n".into());
        design.proposed.get_mut("src/lib.rs").unwrap().push_str("pub fn inspect(config: &Config);\n");
        design.proposed_calls.insert("src/lib.rs".into(), crate::plan::calls::extract("src/lib.rs", "pub fn inspect(config: &Config) { config.limit; }").unwrap());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &digest).unwrap();
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
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Question the API.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose shared state.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Use a focused state owner.".into();
        design.proposed.insert("state.rs".into(), "/// Owns shared state.\npub struct State;\n".into());
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = file.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = file.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
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
        let reopened = file.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
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
        let reopened = file.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
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
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Original overview");
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", Some(1), &checksum)
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
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Declarations");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Define the API.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Expose users through the API.".into();
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
            .capture_review_source("session", "plan", Some(1), &digest)
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
            .describe(anchor, "open", &row, row.find("Hidden").unwrap())
            .unwrap();
        assert!(result["jump"].is_object() && result["patch"].is_object(), "{result}");
        assert!(result.get("declarations").is_none());
        assert!(document.source.public_only);
        let anchor = document.target.values().find(|anchor| {
            matches!(&anchor.target, super::super::PlanReviewTarget::File { path } if path == "api.ts")
        }).unwrap().clone();
        let result = document.describe(anchor, "open", "", 0).unwrap();
        assert!(result["source"].is_object(), "{result}");
        assert_eq!(result["source"]["line"], 1);
        assert!(result.get("declarations").is_none());
    }

    #[test]
    fn module_declaration_jump_selects_the_file_diff() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Module navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose the assets module.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Keep asset declarations in their own module.".into();
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
            .capture_review_source("session", "plan", Some(1), &digest)
            .unwrap();
        let captured_design = source.document.design.as_ref().unwrap();
        source
            .resolver
            .set(Mutex::new((
                crate::declaration::DeclarationResolver::planned(
                    temporary.path(),
                    captured_design,
                    false,
                )
                .unwrap(),
                crate::declaration::DeclarationResolver::planned(
                    temporary.path(),
                    captured_design,
                    true,
                )
                .unwrap(),
            )))
            .ok()
            .unwrap();
        let mut document = PlanReviewDocument::new(
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
        let jump: forge_buffer::block::BlockAnchor = serde_json::from_value(result["jump"].clone()).unwrap();
        assert!(document.document.block(&jump.block).unwrap().text.row(0).unwrap().contains("stable.rs"));
        assert!(result.get("declarations").is_none());
    }

    #[test]
    fn signature_type_jump_resolves_cursor_entity_and_preserves_flow_navigation() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
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
            .capture_review_source("session", "plan", Some(1), &checksum)
            .unwrap();
        let mut document = PlanReviewDocument::new(
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
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Movement declarations");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Define movement interfaces.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Separate movement intent from entity transforms.".into();
        design.proposed.insert("Cargo.toml".into(), "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n[dependencies]\nbevy = \"=0.19.1\"\n".into());
        design.proposed.insert(
            "src/lib.rs".into(),
            "/// Owns movement input.\nmod controls;\n/// Consumes movement input.\nmod arena;\n"
                .into(),
        );
        design.proposed.insert("src/controls.rs".into(), "use bevy::prelude::*;\n\n/// Holds normalized movement intent.\n#[derive(Resource, Default)]\npub(crate) struct MovementInput {\n  /// Direction limited to unit length.\n  pub(crate) direction: Vec2,\n}\n\n/// Samples keyboard movement.\npub(crate) fn movement_input(\n  keys: Res<ButtonInput<KeyCode>>,\n  mut movement: ResMut<MovementInput>,\n);\n".into());
        design.proposed.insert("src/arena.rs".into(), "use bevy::prelude::*;\nuse crate::controls::MovementInput;\n\n/// Advances the player from sampled input.\npub(crate) fn move_player(\n  movement: Res<MovementInput>,\n);\n".into());
        design.proposed.get_mut("src/lib.rs").unwrap().push_str("pub fn install();\n");
        design.proposed_calls.insert("src/lib.rs".into(), crate::plan::calls::extract("src/lib.rs", "pub fn install() { crate::controls::movement_input(); crate::arena::move_player(); }").unwrap());
        design.proposed_calls.insert("src/controls.rs".into(), crate::plan::calls::extract("src/controls.rs", "fn movement_input(movement: &mut MovementInput) { movement.direction; }").unwrap());
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, digest) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let mut source = file
            .capture_review_source("session", "plan", Some(1), &digest)
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

        }
    }

    #[test]
    fn annotation_capture_saves_exact_text_and_rejects_changed_source_atomically() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path(), temporary.path());
        crate::checks::configure_fixture(&file.workspace);
        let canonical = crate::plan::document::test_fixture("plan", "Initial");
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = file
            .capture_review_source("session", "plan", Some(1), &checksum)
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
            .capture_review_source("session", "plan", Some(1), &checksum)
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
            .capture_review_source("session", "plan", Some(1), &checksum)
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
        crate::checks::configure_fixture(&store.workspace);
        let canonical = crate::plan::document::test_fixture("plan", "Initial");
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", Some(1), &checksum)
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
    fn baseline_property_jump_requests_the_real_member_without_changing_the_plan() {
        let temporary = tempfile::tempdir().unwrap();
        std::fs::write(
            temporary.path().join("client.ts"),
            "export interface Client { count: number; }\n",
        )
        .unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical =
            crate::plan::document::test_fixture("plan", "Uncaptured property navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Review a captured caller".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Navigate to an uncaptured property definition".into();
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
                caller,
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
            .capture_review_source("session", "plan", Some(1), &checksum)
            .unwrap();
        let mut document = PlanReviewDocument::new(
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
                    crate::declaration::DeclarationResolver::planned(temporary.path(), design, false)
                        .unwrap(),
                    crate::declaration::DeclarationResolver::planned(temporary.path(), design, true)
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
            std::path::Path::new(result["workspace_definition"]["path"].as_str().unwrap()),
            temporary.path().join("run.ts")
        );
        assert_eq!(result["workspace_definition"]["line"], 2);
        assert_eq!(result["workspace_definition"]["column"], caller.lines().nth(1).unwrap().find("count").unwrap());
        assert!(result.get("declarations").is_none());
        assert_eq!(
            serde_json::to_vec(&document.source.document).unwrap(),
            saved
        );
        std::fs::write(temporary.path().join("run.ts"), caller.replace("client.count", "client.other")).unwrap();
        assert!(document.workspace_definition(&anchor, "  Client.count", 10).unwrap().is_none());
    }
    #[test]
    fn construction_navigation_reveals_type_uses_and_returns_to_definition() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Construction navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Construct a weapon.".into();
        design.document.background = "The setup function needs a ready weapon.".into();
        design.document.design = "Setup constructs the weapon and initializes its cooldown.".into();
        let (text, bodies) = crate::plan::calls::parse("lib.rs", "/// Player weapon.\npub struct Weapon {\n  /// Remaining cooldown.\n  pub cooldown: u32,\n}\nimpl Weapon {\n  /// Creates a ready weapon.\n  pub fn ready() -> Self;\n}\n/// Creates a ready weapon.\npub fn setup();\nConstructs\n  Weapon\n  Weapon::ready\nAccesses\n  Weapon::cooldown\n", &[]).unwrap();
        design.proposed.insert("lib.rs".into(), text);
        design.proposed_calls.insert("lib.rs".into(), bodies);
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("constructs".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let saved = serde_json::to_vec(&document.source.document).unwrap();
        let block = document.snapshot().block.into_iter().find(|block| block.text.row(0).is_some_and(|row| row.contains("struct Weapon"))).unwrap();
        let anchor = block.metadata.target.iter().filter_map(|target| document.target.get(&target.id)).find(|anchor| matches!(anchor.target, crate::plan::PlanReviewTarget::Declaration { .. })).unwrap().clone();
        let row = block.text.row(0).unwrap();
        let references = document.references(&anchor, row, row.find("Weapon").unwrap()).unwrap();
        let construction = references["references"].as_array().unwrap().iter().find(|reference| reference["kind"] == "construct").unwrap();
        assert_eq!(construction["text"].as_str().unwrap().trim(), "Weapon");
        let revealed = document.reveal_reference(construction["id"].as_str().unwrap()).unwrap();
        let jump: forge_buffer::block::BlockAnchor = serde_json::from_value(revealed["jump"].clone()).unwrap();
        let block = document.document.block(&jump.block).unwrap();
        let row = block.text.row(0).unwrap().to_owned();
        assert_eq!(row.trim(), "Weapon");
        assert!(block.metadata.visible_decoration.iter().any(|span| span.capture == "ForgeWalkthroughType"));
        let anchor = block.metadata.target.iter().filter_map(|target| document.target.get(&target.id)).next().unwrap().clone();
        let definition = document.jump_declaration(&anchor, &row, row.find("Weapon").unwrap()).unwrap();
        assert!(definition.get("declarations").is_none());
        let destination: forge_buffer::block::BlockAnchor = serde_json::from_value(definition["jump"].clone()).unwrap();
        assert!(document.document.block(&destination.block).unwrap().text.row(0).unwrap().contains("struct Weapon"));
        assert_eq!(serde_json::to_vec(&document.source.document).unwrap(), saved);
        println!("CONSTRUCT_FIXTURE:{}", serde_json::to_string(&document.snapshot()).unwrap());
    }

    #[test]
    fn property_navigation_reveals_filtered_uses_and_previews_planned_rename() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Property navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Add a client count property.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Expose property uses for review.".into();
        design.proposed.insert("client.ts".into(), "/// Tracks requests.\nexport interface Client {\n  /// Current request count.\n  count: number;\n}\n".into());
        design.proposed.insert("run.ts".into(), "import { Client } from './client';\n/// Updates request count.\nfunction run(client: Client): void;\n".into());
        design.proposed_calls.insert("run.ts".into(), crate::plan::calls::extract("run.ts", "import { Client } from './client'; function run(client: Client) { client.count++; client.count; }").unwrap());
        design.proposed.get_mut("run.ts").unwrap().push_str("export function install(): void;\n");
        design.proposed_calls.get_mut("run.ts").unwrap().extend(crate::plan::calls::extract("run.ts", "export function install() { run(); }").unwrap());
        canonical.design = Some(design);
        store
            .write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = store
            .capture_review_source("session", "plan", Some(1), &checksum)
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
    fn references_hide_plain_rust_imports_without_losing_exports_or_rename_uses() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Import visibility");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Define shared record declarations.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Expose records through public exports.".into();
        design.proposed.insert("Cargo.toml".into(), "[package]\nname = 'records'\nversion = '0.1.0'\nedition = '2021'\n".into());
        design.proposed.insert("src/lib.rs".into(), "pub mod model;\npub mod consumer;\npub use crate::model::Record;\npub(crate) use crate::model::Record as CrateRecord;\n".into());
        design.proposed.insert("src/model.rs".into(), "/// Holds a shared record.\npub struct Record {}\n".into());
        design.proposed.insert("src/consumer.rs".into(), "use crate::model::{\n    Record as LocalRecord,\n};\nimpl LocalRecord {}\n".into());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
        let mut document = PlanReviewDocument::new(DocumentId("imports".into()), ViewId("view".into()), source, WidthProfile::default(), None).unwrap();
        let block = document.snapshot().block.into_iter().find(|block| block.text.row(0).is_some_and(|row| row.contains("pub struct Record"))).unwrap();
        let anchor = block.metadata.target.iter().filter_map(|target| document.target.get(&target.id)).find(|anchor| matches!(&anchor.target, super::super::PlanReviewTarget::Declaration { .. })).unwrap().clone();
        let row = block.text.row(0).unwrap();
        let result = document.references(&anchor, row, row.find("Record").unwrap()).unwrap();
        let references = result["references"].as_array().unwrap();
        assert!(references.iter().all(|reference| reference["kind"] != "import"));
        assert_eq!(references.iter().filter(|reference| reference["kind"] == "export").count(), 2);
        assert!(references.iter().any(|reference| reference["text"].as_str().unwrap().starts_with("pub use ")));
        assert!(references.iter().any(|reference| reference["text"].as_str().unwrap().starts_with("pub(crate) use ")));
        let index = document.reference.lock().unwrap();
        let index = index.get(&false).unwrap();
        assert!(index.occurrence.iter().any(|reference| reference.kind == "import" && reference.path == "src/consumer.rs"));
        let identity = &index.definition.get(&("src/model.rs".into(), 2, 11)).unwrap().0;
        let renamed = index.renamed(&document.source.document, identity, "Entry").unwrap();
        let renamed = renamed.design.unwrap();
        assert!(renamed.proposed["src/consumer.rs"].contains("Entry as LocalRecord"));
        assert!(renamed.proposed["src/lib.rs"].contains("pub use crate::model::Entry;"));
        assert!(renamed.proposed["src/lib.rs"].contains("pub(crate) use crate::model::Entry as CrateRecord;"));
    }

    #[test]
    fn references_reveal_filtered_calls_in_the_same_snapshot_without_editing_design() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        crate::checks::configure_fixture(&store.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Call navigation");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Define call relationships.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Expose sender relationships for review.".into();
        design.proposed.insert("client.ts".into(), "/// Sends a request.\nexport function send(): void;\n".into());
        design.proposed.insert("run.ts".into(), "import { send } from './client';\n/// Dispatches work.\nclass Runner { private run(): void; }\n".into());
        design.proposed_calls.insert("run.ts".into(), vec![crate::plan::FunctionBody { evidence: None, behavior: None, owner: "Runner.run".into(), call: Some(vec![crate::plan::CallSite { kind: crate::plan::CallKind::Call, name: "send".into(), source: None, unresolved: false }]) }]);
        design.baseline.insert("run.ts".into(), crate::plan::DeclarationFile { text: design.proposed["run.ts"].clone(), source_digest: String::new() });
        design.baseline_calls.insert("run.ts".into(), design.proposed_calls["run.ts"].clone());
        canonical.design = Some(design);
        store.write_working_document("session", "plan", &canonical).unwrap();
        let (_, _, checksum) = store.submit_document_revision("session", "plan", 1, 1).unwrap();
        let source = store.capture_review_source("session", "plan", Some(1), &checksum).unwrap();
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
        assert_eq!(call["text"].as_str().unwrap().trim(), "send");
        assert!(call["text_highlight"].as_array().unwrap().iter().any(|capture| capture["capture"] == "@function.call"));
        let baseline_context = super::super::reference_context::ReferenceContext::build(&document.source, true).unwrap();
        let mut baseline_call = document.reference.lock().unwrap().get(&false).unwrap().occurrence.iter().find(|reference| reference.kind == "call").unwrap().clone();
        baseline_call.side = "baseline".into();
        let baseline_entry = serde_json::to_value(baseline_context.entry(&baseline_call).unwrap()).unwrap();
        assert_eq!(baseline_entry["text"].as_str().unwrap().trim(), "send");
        assert_eq!(baseline_entry["side"], "baseline");
        let result = document.reveal_reference(call["id"].as_str().unwrap()).unwrap();
        let jump: forge_buffer::block::BlockAnchor = serde_json::from_value(result["jump"].clone()).unwrap();
        assert_eq!(document.document.block(&jump.block).unwrap().text.row(0).unwrap().trim(), "send");
        assert!(!document.document.block(&jump.block).unwrap().metadata.gutter.is_empty());
        assert!(!document.snapshot().block.iter().any(|block| block.id.0.starts_with("plan:reference:")));
        assert_eq!(document.source_anchor[&format!("{}:0", jump.block.0)]["source_line"], 0);
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
