use super::PlanNavigationAnchor;
use super::review_annotation::{
    ReviewAnnotation, ReviewAnnotationKind, ReviewAnnotationStore, ReviewQuestionReply,
};
use super::review_source::PlanReviewSource;
use anyhow::{Context, Result, ensure};
use forge_buffer::admission::{DocumentAdmission, DocumentAdmissionStore};
use forge_buffer::block::TextPosition;
use forge_buffer::document::BufferDocument;
use forge_buffer::identity::{BlockId, DocumentId, InputSequence, TargetId, ViewId};
use forge_buffer::input::DocumentInput;
use forge_buffer::patch::BufferSnapshot;
use forge_buffer::view::DocumentViews;
use forge_buffer::width::WidthProfile;
use forge_diff::view::{DiffCapabilities, DiffViewAction, DiffViewState, DiffVisibility};
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashMap};
use std::sync::{Arc, Mutex};

#[derive(Default)]
pub(crate) struct PlanReviewStore {
    admission: Arc<DocumentAdmissionStore>,
    document: Mutex<HashMap<DocumentId, (ReadOnlyPlanReview, DocumentAdmission)>>,
    recent_symbols: Mutex<Option<(String, super::review_files::ReviewSymbolCache)>>,
}

/// Captures one unanswered question and the immutable design context used to answer it.
pub(crate) struct PlanReviewQuestion {
    /// Durable answer session shared by this annotation chain on this saved review.
    pub answer_session: uuid::Uuid,
    /// Stable identity of the question's annotation.
    pub id: String,
    /// Exact editable text captured at submission.
    pub body: String,
    /// Saved declaration design and anchored review excerpt supplied to the model.
    pub prompt: String,
}

#[derive(Default, serde::Serialize)]
struct TestChangeCounts {
    new: usize,
    modified: usize,
    removed: usize,
    reused: usize,
}

impl TestChangeCounts {
    fn add(&mut self, change: super::DesignTestChange) {
        let count = match change {
            super::DesignTestChange::New => &mut self.new,
            super::DesignTestChange::Modified => &mut self.modified,
            super::DesignTestChange::Removed => &mut self.removed,
            super::DesignTestChange::Reused => &mut self.reused,
        };
        *count += 1;
    }
}

impl PlanReviewStore {
    /// An unfold names the saved revision and file, independently of its current section.
    /// Release the store during syntax analysis and publish against the latest file location.
    pub(crate) async fn load_body(
        &self,
        id: &DocumentId,
        digest: &str,
        file: &str,
        syntax: &Arc<forge_diff::syntax::SyntaxEngine>,
    ) -> Result<serde_json::Value> {
        let started = std::time::Instant::now();
        let sides = {
            let store = self
                .document
                .lock()
                .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
            let (document, admission) = store.get(id).context("plan review document is closed")?;
            admission.check()?;
            ensure!(
                document.saved_digest() == digest,
                "plan file revision changed"
            );
            let inventory = document
                .source
                .files
                .as_ref()
                .context("plan has no file inventory")?;
            if inventory.loaded.contains(file) {
                return Ok(serde_json::json!({}));
            }
            let heading = inventory.files.iter().find(|heading| heading.id == file);
            let paths = heading
                .map(|heading| (&heading.before, &heading.after))
                .or_else(|| {
                    document.source.revealed.iter().find_map(|(path, _, _)| {
                        (file == format!("plan:design:file:{}", super::digest(path.as_bytes())))
                            .then_some((path, path))
                    })
                });
            let design = document
                .source
                .document
                .design
                .as_ref()
                .context("plan has no design")?;
            let parsed = design.parsed()?;
            let mut sides = Vec::new();
            if let Some((before, after)) = paths {
                for (side, path, text, calls) in [
                    (
                        "baseline",
                        before,
                        design.baseline.get(before).map(|file| &file.text),
                        &design.baseline_calls,
                    ),
                    (
                        "proposed",
                        after,
                        parsed.proposed.get(after),
                        &parsed.proposed_calls,
                    ),
                ] {
                    if let Some(text) = text {
                        let presentation = super::calls::present(
                            path,
                            text,
                            calls.get(path).map(Vec::as_slice).unwrap_or_default(),
                        )?;
                        sides.push((side, path.clone(), presentation));
                    }
                }
            } else {
                ensure!(
                    design.document.tests.iter().any(|test| file
                        == format!("plan:tests:file:{}", super::digest(test.file.as_bytes()))),
                    "plan file is missing"
                );
            }
            sides
        };
        let mut handles = Vec::new();
        for (side, path, presentation) in sides {
            if let Some(handle) =
                crate::service::analyze_review_side(&path, &presentation, syntax, || Ok(())).await?
            {
                handles.push(((path, side.to_owned()), handle));
            }
        }
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        ensure!(
            document.saved_digest() == digest,
            "plan file revision changed"
        );
        if document
            .document
            .block_index(&BlockId(file.to_owned()))
            .is_none()
        {
            // A visibility toggle can dismiss an unchanged contextual destination during analysis.
            return Ok(serde_json::json!({}));
        }
        document.source.declaration_syntax.extend(handles);
        let result = document.materialize_file(file)?;
        if let Some(trace) = &document.source.trace {
            trace.record("plan.review.file_body", started.elapsed(), 1);
        }
        Ok(result)
    }

    /// Reuse the visible prose index for an unchanged revision.
    pub(crate) fn reuse_symbols(&self, source: &mut PlanReviewSource) -> Result<()> {
        let digest = source.saved_digest.clone();
        if let Some(files) = source.files.as_mut() {
            let mut recent = self
                .recent_symbols
                .lock()
                .map_err(|_| anyhow::anyhow!("plan symbol cache lock poisoned"))?;
            match recent.as_ref() {
                Some((previous, symbols)) if previous == &digest => files.symbols = symbols.clone(),
                _ => *recent = Some((digest, files.symbols.clone())),
            }
        }
        Ok(())
    }

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
        document.editable()?.save_annotations(digest, annotation)
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
        document.editable()?.submission(input)
    }

    /// Capture unanswered questions after validating their native document identity.
    pub(crate) fn questions(
        &self,
        source_session: &str,
        input: DocumentInput,
    ) -> Result<Vec<PlanReviewQuestion>> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.editable()?.questions(source_session, input)
    }

    /// Validate the captured question against the current admitted review document.
    pub(crate) fn question_pending(
        &self,
        id: &DocumentId,
        question: &PlanReviewQuestion,
    ) -> Result<bool> {
        let store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get(id).context("plan review document is closed")?;
        admission.check()?;
        let annotation = document
            .annotation
            .annotation()
            .iter()
            .find(|annotation| {
                annotation.id == question.id
                    && annotation.kind == ReviewAnnotationKind::Question
                    && annotation.source.body == question.body
            })
            .context("plan question changed before its answer arrived")?;
        Ok(annotation.reply.is_none())
    }

    /// Persist an answer only while its original question and source snapshot remain current.
    pub(crate) fn answer(
        &self,
        id: &DocumentId,
        question: &PlanReviewQuestion,
        body: String,
        duration_ms: u64,
    ) -> Result<Vec<ReviewAnnotation>> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        document.editable()?.answer(question, body, duration_ms)
    }

    /// Return persisted comments and answers independently of the visible source filter.
    pub(crate) fn annotations(&self, id: &DocumentId) -> Result<Vec<ReviewAnnotation>> {
        let store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store.get(id).context("plan review document is closed")?;
        admission.check()?;
        Ok(document.annotation.annotation().to_vec())
    }

    pub(crate) fn admit(&self, id: DocumentId) -> Result<DocumentAdmission> {
        Ok(self.admission.admit(id)?)
    }

    pub(crate) fn insert(
        &self,
        mut document: ReadOnlyPlanReview,
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
            "diff_view":document.diff_view.committed(), "diff_capabilities":DiffCapabilities::SemanticOnly,
            "annotation":document.annotation.annotation(), "source_edit":source_edit,
            "checks":document.source.document.design.as_ref().map(|design| design.check_options()).transpose()?.unwrap_or_default(),
            "submission_warnings":document.source.document.design.as_ref().and_then(|design| design.submission_warnings.as_ref()),
            "submission_warning_status":null});
        store.insert(document.id.clone(), (document, admission));
        Ok(opened)
    }

    /// Reuse analysis for unchanged file sides before preparing a newer plan revision.
    pub(crate) fn prepare_update(
        &self,
        id: &DocumentId,
        source: &mut super::review_source::EditableReviewSource,
    ) -> Result<u64> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        document.editable()?.prepare_update(source)
    }

    /// Publish a prepared revision while retaining the document identity and current view choices.
    pub(crate) fn update(
        &self,
        id: &DocumentId,
        source: super::review_source::EditableReviewSource,
        expected_version: u64,
        trace: Option<&super::edit_trace::PlanEditTrace>,
    ) -> Result<serde_json::Value> {
        let wait = trace.map(|trace| trace.stage("review.lock_wait"));
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        if let Some(stage) = wait {
            stage.complete(serde_json::json!({}));
        }
        let (document, admission) = store
            .get_mut(id)
            .context("plan review document is closed")?;
        admission.check()?;
        ensure!(
            document.source.document.version == expected_version,
            "plan changed while preparing its projection"
        );
        document.editable()?.update(source, trace)
    }

    /// Prepare only the saved proposal and baseline declaration indexes.
    pub(crate) async fn prepare_declaration_jump(
        &self,
        input: &DocumentInput,
        trace: Option<&crate::declaration::trace::DeclarationTrace>,
    ) -> Result<()> {
        let store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.check_input(input)?;
        if document
            .input_target(input)?
            .and_then(|id| document.target.get(&id))
            .is_some_and(|anchor| {
                matches!(anchor.target, super::PlanReviewTarget::InlineSymbol { .. })
            })
        {
            return Ok(());
        }
        let Some(design) = &document.source.document.design else {
            return Ok(());
        };
        if document.source.resolver.get().is_none() {
            let mut proposed = crate::declaration::DeclarationResolver::planned(
                &document.source.workspace,
                design,
                false,
            )?;
            let mut baseline = crate::declaration::DeclarationResolver::planned(
                &document.source.workspace,
                design,
                true,
            )?;
            proposed.trace = trace.cloned();
            baseline.trace = trace.cloned();
            document
                .source
                .resolver
                .set(Mutex::new((proposed, baseline)))
                .map_err(|_| anyhow::anyhow!("declaration resolver was already initialized"))?;
        }
        Ok(())
    }

    /// Accept a definition only while its original action and source remain current.
    pub(crate) fn definition(
        &self,
        input: &DocumentInput,
        source: &super::WorkspaceDefinition,
        target: &super::DefinitionLocation,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        ensure!(
            matches!(input.action.as_str(), "jump_entity" | "open")
                && document.view.get(&input.view) == Some(&input.sequence),
            "definition action was superseded"
        );
        let identity = document
            .input_target(input)?
            .context("definition action has no target")?;
        let anchor = document
            .target
            .get(&identity)
            .context("definition target is unavailable")?
            .clone();
        let row = document
            .document
            .block(&input.block)
            .and_then(|block| block.text.row(input.position.row))
            .context("definition row is unavailable")?;
        let design = document
            .source
            .document
            .design
            .as_ref()
            .context("definition source has no semantic design")?;
        ensure!(
            document
                .workspace_definition(&anchor, row, input.position.column)?
                .as_ref()
                == Some(source),
            "definition source changed"
        );
        ensure!(
            target.line > 0 && std::path::Path::new(&target.path).is_absolute(),
            "invalid definition location"
        );
        let side = match &anchor.target {
            super::PlanReviewTarget::Call { side, .. }
            | super::PlanReviewTarget::Declaration { side, .. } => side.as_str(),
            _ => anyhow::bail!("definition target is unavailable"),
        };
        let absolute =
            dunce::canonicalize(&target.path).context("definition file is unavailable")?;
        ensure!(absolute.is_file(), "definition target is not a file");
        let workspace = dunce::canonicalize(&document.source.workspace)?;
        if !target.modified
            && let Ok(relative) = absolute.strip_prefix(&workspace)
        {
            let relative = relative.to_string_lossy().replace('\\', "/");
            if let Some(baseline) = design.baseline.get(&relative)
                && let Some(real) = super::design::workspace_source(&workspace, &relative)?
                && super::digest(real.as_bytes()) == baseline.source_digest
            {
                let saved_path = if side == "baseline" {
                    &relative
                } else {
                    design.moved.get(&relative).unwrap_or(&relative)
                };
                let saved = if side == "baseline" {
                    Some(&baseline.text)
                } else {
                    design.parsed()?.proposed.get(saved_path)
                };
                if let Some(saved) = saved
                    && let Some(position) = super::workspace_definition::map_position(
                        &relative,
                        &real,
                        saved,
                        forge_diff::syntax::DeclarationPosition {
                            line: target.line,
                            column: target.column,
                        },
                    )?
                {
                    let name = saved
                        .lines()
                        .nth(position.line.saturating_sub(1) as usize)
                        .and_then(|line| line.get(position.column as usize..))
                        .unwrap_or("")
                        .chars()
                        .take_while(|character| {
                            character.is_alphanumeric() || *character == '_' || *character == '$'
                        })
                        .collect();
                    return document.declaration_destination(
                        side,
                        crate::declaration::DeclarationDestination {
                            path: document
                                .source
                                .workspace
                                .join(saved_path)
                                .to_string_lossy()
                                .into_owned(),
                            line: position.line,
                            column: position.column,
                            name,
                            proposed: true,
                            module_file: false,
                        },
                    );
                }
            }
        }
        Ok(serde_json::json!({"source":target}))
    }

    pub(crate) fn action(
        &self,
        input: DocumentInput,
        selected_files: Option<&[BlockId]>,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        let action = input.action.clone();
        if matches!(action.as_str(), "viewed" | "unviewed") {
            let block = input.block.clone();
            document.validate_input(input)?;
            let cursor_file;
            let selected = match selected_files {
                Some(files) => files,
                None => {
                    let Some(file) = super::review_projection::change_file(&block) else {
                        return Ok(serde_json::json!({}));
                    };
                    cursor_file = BlockId(file.to_owned());
                    std::slice::from_ref(&cursor_file)
                }
            };
            let mut files = document.source.read_viewed_files()?;
            let mut changed = false;
            for file in selected {
                ensure!(
                    super::review_projection::change_file(file) == Some(file.0.as_str())
                        && document.document.block(file).is_some(),
                    "selected plan file is no longer available: {}",
                    file.0
                );
                changed |= forge_diff::presentation::file_review::set_viewed(
                    &mut files,
                    &file.0,
                    action == "viewed",
                );
            }
            if changed {
                super::write_json_atomically(&document.source.viewed_path()?, &files)?;
            }
            let result = if files == document.source.viewed_files {
                serde_json::json!({})
            } else {
                document.group_review_files(&files)?
            };
            document.source.viewed_files = files;
            return Ok(result);
        }
        if action == "references" || action == "rename_entity" {
            let column = input.position.column;
            let row = document
                .document
                .block(&input.block)
                .and_then(|block| block.text.row(input.position.row))
                .unwrap_or("")
                .to_owned();
            let anchor = document.action(input)?;
            return if action == "rename_entity" {
                document
                    .editable()?
                    .rename(ProposedReviewTarget::try_from(&anchor)?, &row, column)
            } else {
                document.references(&anchor, &row, column)
            };
        }
        if let Some(id) = action.strip_prefix("reveal_reference:") {
            document.validate_input(input)?;
            return document.reveal_reference(id);
        }
        if action == "schema" {
            document.validate_input(input)?;
            let text = crate::plan::codec::to_string_pretty(&document.source.document)?;
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
        if action == "toggle_visibility" {
            document.validate_input(input)?;
            return document.toggle_visibility();
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

pub(crate) struct ReadOnlyPlanReview {
    diff_view: DiffViewState,
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

struct EditablePlanReview<'document>(&'document mut ReadOnlyPlanReview);

struct ProposedReviewTarget<'target>(&'target PlanNavigationAnchor);

impl<'target> TryFrom<&'target PlanNavigationAnchor> for ProposedReviewTarget<'target> {
    type Error = anyhow::Error;
    fn try_from(anchor: &'target PlanNavigationAnchor) -> Result<Self> {
        match &anchor.target {
            super::PlanReviewTarget::Declaration { side, .. }
            | super::PlanReviewTarget::Call { side, .. }
            | super::PlanReviewTarget::Behavior { side, .. }
                if side == "proposed" =>
            {
                Ok(Self(anchor))
            }
            _ => anyhow::bail!("rename requires a proposed declaration target"),
        }
    }
}

impl EditablePlanReview<'_> {
    fn save_annotations(self, digest: &str, annotation: Vec<ReviewAnnotation>) -> Result<()> {
        let document = self.0;
        ensure!(
            digest == document.source.saved_digest,
            "saved plan source changed"
        );
        let mut captured = annotation;
        for annotation in &mut captured {
            annotation.reply = document
                .annotation
                .annotation()
                .iter()
                .find(|saved| {
                    saved.id == annotation.id
                        && saved.kind == annotation.kind
                        && saved.source.body == annotation.source.body
                })
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

    fn submission(self, input: DocumentInput) -> Result<serde_json::Value> {
        let document = self.0;
        document.validate_input(input)?;
        Ok(
            serde_json::json!({"plan_id":document.source.document.plan_id,
            "digest":super::digest(&crate::plan::codec::to_vec(&document.source.document)?),
            "saved_source_digest":document.source.saved_digest,
            "annotations":document.annotation.annotation().iter().filter(|annotation| annotation.kind == ReviewAnnotationKind::Comment && !annotation.source.body.trim().is_empty())
                .map(|annotation| {
                    let mut source = annotation.source.clone();
                    if annotation.parent_id.is_some() {
                        let (_, thread) = document.annotation.thread_context(annotation)?;
                        source.body = format!("Conversation context:\n{thread}Requested change:\n{}", source.body);
                    }
                    Ok::<_, anyhow::Error>(source)
                }).collect::<Result<Vec<_>>>()? }),
        )
    }

    /// Bind each question to exact numbered text from the captured review snapshot.
    fn questions(
        self,
        source_session: &str,
        input: DocumentInput,
    ) -> Result<Vec<PlanReviewQuestion>> {
        let document = self.0;
        // Independent answer forks read the same snapshot without consuming each other's input sequence.
        ensure!(
            document.view.contains_key(&input.view),
            "plan review view is closed"
        );
        document.input_target(&input)?;
        let design = document
            .source
            .document
            .design
            .as_ref()
            .context("plan questions require a declaration design")?;
        let mut context = serde_json::json!({"proposed":design.parsed()?.proposed});
        context["document"] = super::codec::to_value(&design.document)?;
        let context = super::codec::to_string_pretty(&context)?;
        let mut question = Vec::new();
        for annotation in document
            .annotation
            .annotation()
            .iter()
            .filter(|annotation| {
                annotation.kind == ReviewAnnotationKind::Question
                    && annotation.reply.is_none()
                    && !annotation.source.body.trim().is_empty()
            })
        {
            let resolved = super::resolve_annotations(
                &document.source.rendered,
                vec![annotation.source.clone()],
            )?;
            let excerpt = super::render_review_feedback(&document.source.document, &resolved)?;
            let selection = super::review_feedback::selected_lines(
                &document.source.rendered,
                &annotation.source,
            )?;
            let (root, thread) = document.annotation.thread_context(annotation)?;
            // A saved review can reopen under a different editor document ID.
            let key = serde_json::to_vec(&(
                "plan-review-answer",
                source_session,
                &document.source.document.plan_id,
                root,
            ))?;
            let mut bytes = [0; 16];
            bytes.copy_from_slice(&Sha256::digest(key)[..16]);
            let answer_session = uuid::Builder::from_custom_bytes(bytes).into_uuid();
            let prompt = format!(
                "Reviewed plan: {}\nReviewed plan version: {}\nSaved source digest: {}\n\
                 \nSaved declaration design:\n{context}\n\
                 \nExact selection from the reviewed plan (numbers identify canonical plan lines, not workspace file lines):\n{selection}\n\
                 Semantic source context (declaration excerpts use their own file line numbers):\n{excerpt}\n\
                 Conversation thread:\n{thread}Answer this question:\n{}\n\
                 \nAnswer specifically about the selected text. Context before, context after, and the saved design are supporting context, not the question's target.",
                document.source.document.plan_id,
                document.source.document.version,
                document.source.saved_digest,
                annotation.source.body
            );
            question.push(PlanReviewQuestion {
                answer_session,
                id: annotation.id.clone(),
                body: annotation.source.body.clone(),
                prompt,
            });
        }
        Ok(question)
    }

    fn answer(
        self,
        question: &PlanReviewQuestion,
        body: String,
        duration_ms: u64,
    ) -> Result<Vec<ReviewAnnotation>> {
        let document = self.0;
        let mut annotation = document.annotation.annotation().to_vec();
        let selected = annotation
            .iter_mut()
            .find(|annotation| {
                annotation.id == question.id
                    && annotation.kind == ReviewAnnotationKind::Question
                    && annotation.source.body == question.body
            })
            .context("plan question changed before its answer arrived")?;
        selected.reply = Some(ReviewQuestionReply {
            question_body: question.body.clone(),
            body,
            duration_ms: Some(duration_ms),
        });
        document.annotation.replace(annotation.clone())?;
        Ok(annotation)
    }

    fn prepare_update(
        self,
        source: &mut super::review_source::EditableReviewSource,
    ) -> Result<u64> {
        let document = self.0;
        let source: &mut super::review_source::ReviewSourceData = source;
        if let (Some(files), Some(design)) = (&document.source.files, &source.document.design) {
            source.files = Some(super::review_files::ReviewFiles::new(
                design,
                &source.rendered,
                &files.loaded,
            )?);
        }
        ensure!(
            source.document.plan_id == document.source.document.plan_id
                && source.document.version > document.source.document.version,
            "plan revision is stale"
        );
        if let (Some(previous), Some(current)) = (
            &document.source.document.design,
            &source.revision.document.design,
        ) {
            let previous_parsed = previous.parsed()?;
            let current_parsed = current.parsed()?;
            for ((path, side), handle) in &document.source.declaration_syntax {
                let (before, after) = if side == "baseline" {
                    (
                        previous.baseline.get(path).map(|file| &file.text),
                        current.baseline.get(path).map(|file| &file.text),
                    )
                } else {
                    (
                        previous_parsed.proposed.get(path),
                        current_parsed.proposed.get(path),
                    )
                };
                let (before_calls, after_calls) = if side == "baseline" {
                    (
                        previous.baseline_calls.get(path),
                        current.baseline_calls.get(path),
                    )
                } else {
                    (
                        previous_parsed.proposed_calls.get(path),
                        current_parsed.proposed_calls.get(path),
                    )
                };
                if before.is_some() && before == after && before_calls == after_calls {
                    source
                        .declaration_syntax
                        .insert((path.clone(), side.clone()), handle.clone());
                }
            }
            if previous_parsed.proposed == current_parsed.proposed
                && previous.baseline == current.baseline
                && previous_parsed.proposed_calls == current_parsed.proposed_calls
                && previous.baseline_calls == current.baseline_calls
            {
                source.resolver = Arc::clone(&document.source.resolver);
            }
        }
        Ok(document.source.document.version)
    }

    fn update(
        self,
        source: super::review_source::EditableReviewSource,
        trace: Option<&super::edit_trace::PlanEditTrace>,
    ) -> Result<serde_json::Value> {
        let document = self.0;
        let mut source = source.into_source();
        source.viewed_files = source.read_viewed_files()?;
        let total = trace.map(|trace| trace.stage("review.update"));
        ensure!(
            source.document.plan_id == document.source.document.plan_id
                && source.document.version > document.source.document.version,
            "plan revision is stale"
        );
        source.public_only = document.source.public_only;
        source.revealed = document
            .source
            .revealed
            .iter()
            .filter(|(path, side, _)| {
                let (Some(previous), Some(current)) =
                    (&document.source.document.design, &source.document.design)
                else {
                    return false;
                };
                if side == "baseline" {
                    previous.baseline.get(path) == current.baseline.get(path)
                        && previous.baseline_calls.get(path) == current.baseline_calls.get(path)
                } else {
                    match (&previous.parsed, &current.parsed) {
                        (Some(previous), Some(current)) => {
                            previous.proposed.get(path) == current.proposed.get(path)
                                && previous.proposed_calls.get(path)
                                    == current.proposed_calls.get(path)
                        }
                        _ => false,
                    }
                }
            })
            .cloned()
            .collect();
        source.collapse = document.source.collapse.clone();
        let annotation_stage = trace.map(|trace| trace.stage("review.load_reanchor_annotations"));
        let mut annotation = ReviewAnnotationStore::open(
            source.path.with_extension("json"),
            source.saved_digest.clone(),
        )?;
        if source.document.design.is_some() {
            annotation.reanchor(&source.rendered.navigation)?;
        }
        document.retain_annotation_revision();
        if let Some(stage) = annotation_stage {
            stage.complete(serde_json::json!({"count":annotation.annotation().len()}));
        }
        let projection = trace.map(|trace| trace.stage("review.project"));
        let mut expected = document.source.document.clone();
        expected.version = source.document.version;
        let tests_only = match (&mut expected.design, &source.document.design) {
            (Some(previous), Some(current)) => {
                previous.document.tests.clone_from(&current.document.tests);
                expected == source.document
            }
            _ => false,
        };
        let (block, target) = if tests_only {
            super::review_projection::update_tests(
                &source,
                &document.source.rendered,
                document.document.snapshot().block,
                document.target.clone(),
            )?
        } else {
            super::review_projection::project(
                &source,
                &document.width,
                &[],
                &document.annotation_revision,
                document.focused_annotation.as_deref(),
            )?
        };
        if let Some(stage) = projection {
            stage.complete(serde_json::json!({"count":block.len(),"cached":tests_only}));
        }
        let reanchor = trace.map(|trace| trace.stage("review.retain_annotations"));
        let previous = document.document.snapshot();
        // A comment keeps its row inside its section, as a review edit's own re-anchoring does;
        // the first row with the same target would be the section heading.
        let remap = |line| {
            if let Some(current) = super::review_annotation::relocate_row(
                &document.source.rendered,
                &source.rendered,
                line,
            ) {
                return Some(current.clone());
            }
            for previous_block in &previous.block {
                for previous_range in &previous_block.metadata.target {
                    if document
                        .target
                        .get(&previous_range.id)
                        .is_none_or(|candidate| candidate.line != line)
                    {
                        continue;
                    }
                    let current_block = block
                        .iter()
                        .find(|current| current.id == previous_block.id)?;
                    let current_range = current_block.metadata.target.iter().find(|current| {
                        current.range.start.row == previous_range.range.start.row
                    })?;
                    return target.get(&current_range.id).cloned();
                }
            }
            None
        };
        let mut retained = annotation.annotation().to_vec();
        for original in document.annotation.annotation() {
            if retained.iter().any(|saved| saved.id == original.id) {
                continue;
            }
            let (Some(start), Some(end)) = (
                remap(original.source.start_line),
                remap(original.source.end_line),
            ) else {
                continue;
            };
            if original
                .parent_id
                .as_ref()
                .is_some_and(|parent| !retained.iter().any(|saved| &saved.id == parent))
            {
                continue;
            }
            let mut saved = original.clone();
            saved.source.start_line = start.line.min(end.line);
            saved.source.end_line = start.line.max(end.line);
            saved.anchor = Some(super::ReviewAnnotationAnchor {
                start: start.target,
                end: end.target,
            });
            retained.push(saved);
        }
        if retained.len() != annotation.annotation().len() {
            annotation.replace(retained)?;
        }
        if let Some(stage) = reanchor {
            stage.complete(serde_json::json!({"count":annotation.annotation().len()}));
        }
        let edit = trace.map(|trace| trace.stage("review.edit_document"));
        let patch = document
            .document
            .splice(0..document.document.block_count(), block)?;
        if let Some(stage) = edit {
            stage.complete(serde_json::json!({}));
        }
        document.source = source;
        if !tests_only {
            document.reference = Default::default();
            document.reference_context = Default::default();
        }
        document.target = target;
        document.annotation = annotation;
        let response = trace.map(|trace| trace.stage("review.build_delivery"));
        let source_edit = document.source_edit();
        let result = serde_json::json!({"patch":patch, "source_edit":source_edit,
            "annotation":document.annotation.annotation(), "public_only":document.source.public_only,
            "saved_source_digest":document.saved_digest(), "version":document.source.document.version,
            "checks":document.source.document.design.as_ref().map(|design| design.check_options()).transpose()?.unwrap_or_default(),
            "submission_warnings":document.source.document.design.as_ref().and_then(|design| design.submission_warnings.as_ref()),
            "submission_warning_status":null});
        if let Some(stage) = response {
            stage.complete(serde_json::json!({}));
        }
        if let Some(stage) = total {
            stage.complete(serde_json::json!({"revision":document.source.document.version}));
        }
        Ok(result)
    }

    fn rename(
        self,
        anchor: ProposedReviewTarget<'_>,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        let document = self.0;
        let anchor = anchor.0;
        let design = document
            .source
            .document
            .design
            .as_ref()
            .context("rename requires a declaration plan")?;
        let index = super::references::PlanReferenceIndex::planned(
            &document.source.document,
            &document.source.workspace,
        )?;
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
                &design.parsed()?.proposed[path],
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
            .selected_algorithm(anchor, row, column)
            .or_else(|| index.selected(&selected_anchor, saved_column))
            .context("select a resolved symbol defined in the plan")?;
        let (definition_path, symbol) =
            index.rename_definition(&document.source.document, identity)?;
        let mut preview = Vec::new();
        // Narrative rows, test descriptions, and flows carry the inline-code mentions the rename
        // rewrites; declaration rows, doc comments included, change only through their tokens.
        let narrative =
            super::review_symbols::NarrativeSymbols::new(design, std::path::Path::new(""))?;
        // Narrative blocks hold one row each, so the open-fence state carries from row to row
        // through consecutive narrative blocks, as it does through the text they render.
        let mut fence = false;
        for block in document.document.snapshot().block {
            let kind = if block.id.0.contains(":description:") {
                super::references::NarrativeText::TestDescription
            } else if block.id.0.starts_with("plan:metadata:") {
                super::references::NarrativeText::Prose
            } else {
                fence = false;
                continue;
            };
            let Some(row) = block.text.row(0) else {
                continue;
            };
            let mut description_fence = false;
            let fence = if kind == super::references::NarrativeText::TestDescription {
                &mut description_fence
            } else {
                &mut fence
            };
            for column in super::references::narrative_mentions(
                row,
                &narrative,
                &definition_path,
                &symbol,
                kind,
                fence,
            ) {
                preview.push(serde_json::json!({"block":block.id,"column":column,"length":symbol.name.len()}));
            }
        }
        let mut resolver = crate::declaration::DeclarationResolver::planned(
            &document.source.workspace,
            design,
            false,
        )?;
        resolver.discard_snapshot_originals();
        for block in document.document.snapshot().block {
            for target in &block.metadata.target {
                let Some(anchor) = document.target.get(&target.id) else {
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
                                &design.parsed()?.proposed[path],
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
                        super::PlanReviewTarget::Behavior { side, .. } if side == "proposed" => {
                            if index.selected_algorithm(anchor, row, column) == Some(identity) {
                                preview.push(serde_json::json!({"block":block.id,"column":column,"length":symbol.name.len()}));
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
        // The editor refuses a name in this list as soon as it is entered, before it asks for
        // confirmation; the rename itself refuses the same names through the same function.
        let existing_names = super::references::names_in_scope(
            &definition_path,
            &design.parsed()?.proposed[&definition_path],
            &symbol,
        )?;
        Ok(
            serde_json::json!({"rename":{"symbol":identity,"name":symbol.name,"existing_names":existing_names,"expected_version":document.source.document.version,"preview":preview}}),
        )
    }
}

impl ReadOnlyPlanReview {
    fn editable(&mut self) -> Result<EditablePlanReview<'_>> {
        ensure!(
            !self.source.is_historical(),
            "historical plan revisions are read-only"
        );
        Ok(EditablePlanReview(self))
    }

    fn references(
        &mut self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        let baseline = matches!(&anchor.target, super::PlanReviewTarget::Declaration { side, .. } | super::PlanReviewTarget::Call { side, .. } if side == "baseline");
        let mut reference = self
            .reference
            .lock()
            .map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?;
        let index = match reference.entry(baseline) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(super::references::PlanReferenceIndex::build(
                    &self.source.document,
                    &self.source.workspace,
                    baseline,
                )?)
            }
        };
        let mut selected_anchor = anchor.clone();
        let saved_column = if let super::PlanReviewTarget::Declaration {
            path,
            side,
            line,
            column: saved,
        } = &anchor.target
        {
            let design = self
                .source
                .document
                .design
                .as_ref()
                .context("declaration design is unavailable")?;
            let text = if side == "baseline" {
                design.baseline.get(path).map(|file| &file.text)
            } else {
                design.parsed()?.proposed.get(path)
            }
            .context("declaration file is unavailable")?;
            let position = forge_diff::syntax::DeclarationOverview::token_position(
                path,
                text,
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
        let Some(symbol) = index.selected(&selected_anchor, saved_column) else {
            return Ok(
                serde_json::json!({"message":"The selected symbol has no resolved plan identity."}),
            );
        };
        let occurrences = index
            .occurrence
            .iter()
            .filter(|reference| {
                reference.symbol == symbol
                    && !(reference.kind == "import" && reference.path.ends_with(".rs"))
            })
            .collect::<Vec<_>>();
        if occurrences.is_empty() {
            return Ok(serde_json::json!({"message":"No references in this plan snapshot."}));
        }
        let mut context = self
            .reference_context
            .lock()
            .map_err(|_| anyhow::anyhow!("plan reference context cache lock poisoned"))?;
        let context = match context.entry(baseline) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => entry.insert(
                super::reference_context::ReferenceContext::build(&self.source, baseline)?,
            ),
        };
        let entries = occurrences
            .into_iter()
            .map(|reference| context.entry(reference))
            .collect::<Result<Vec<_>>>()?;
        Ok(serde_json::json!({"references":entries,"revision":self.document.revision()}))
    }

    fn reveal_reference(&mut self, id: &str) -> Result<serde_json::Value> {
        let reference = self
            .reference
            .lock()
            .map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?
            .values()
            .flat_map(|index| &index.occurrence)
            .find(|reference| reference.id == id)
            .context("plan reference selection is unavailable")?
            .clone();
        let locate = |blocks: &[forge_buffer::block::BufferBlock],
                      targets: &HashMap<TargetId, PlanNavigationAnchor>| {
            blocks.iter().enumerate().find_map(|(index, block)| {
                block.metadata.target.iter().find_map(|range| {
                    let anchor = targets.get(&range.id)?;
                    let selected = match &anchor.target {
                        super::PlanReviewTarget::Call {
                            path,
                            side,
                            owner,
                            name,
                            kind,
                        } => {
                            reference.kind == kind.label()
                                && path == &reference.path
                                && side == &reference.side
                                && owner == &reference.owner
                                && name == &reference.name
                        }
                        super::PlanReviewTarget::Declaration {
                            path, side, line, ..
                        } => {
                            !matches!(
                                reference.kind.as_str(),
                                "call" | "property" | "callback" | "value" | "construct"
                            ) && path == &reference.path
                                && side == &reference.side
                                && *line == reference.line
                        }
                        _ => reference
                            .anchor
                            .as_ref()
                            .is_some_and(|saved| saved.json_path == anchor.json_path),
                    };
                    selected.then(|| {
                        (
                            index,
                            forge_buffer::block::BlockAnchor {
                                block: block.id.clone(),
                                position: TextPosition {
                                    row: range.range.start.row,
                                    column: if matches!(
                                        reference.kind.as_str(),
                                        "call" | "property" | "callback" | "value" | "construct"
                                    ) {
                                        block
                                            .text
                                            .row(range.range.start.row)
                                            .unwrap_or("")
                                            .find(&reference.name)
                                            .unwrap_or(0)
                                    } else {
                                        reference.column as usize
                                    },
                                },
                            },
                        )
                    })
                })
            })
        };
        if let Some((_, jump)) = locate(&self.document.snapshot().block, &self.target) {
            return Ok(serde_json::json!({"jump":jump}));
        }
        let design = self
            .source
            .document
            .design
            .as_ref()
            .context("reference has no declaration design")?;
        let (text, calls) = if reference.side == "baseline" {
            (
                design
                    .baseline
                    .get(&reference.path)
                    .map(|file| file.text.as_str()),
                design.baseline_calls.get(&reference.path),
            )
        } else {
            (
                design
                    .parsed()?
                    .proposed
                    .get(&reference.path)
                    .map(String::as_str),
                design.parsed()?.proposed_calls.get(&reference.path),
            )
        };
        let presentation = super::calls::present(
            &reference.path,
            text.context("reference file is missing")?,
            calls.map(Vec::as_slice).unwrap_or_default(),
        )?;
        let selected_row = presentation
            .declaration
            .source
            .iter()
            .position(|position| position.is_some_and(|position| position.line == reference.line))
            .context("reference has no declaration row")?;
        self.reveal_target(&reference.path, &reference.side, selected_row, locate)
    }

    fn reveal_target(
        &mut self,
        path: &str,
        side: &str,
        selected_row: usize,
        locate: impl Fn(
            &[forge_buffer::block::BufferBlock],
            &HashMap<TargetId, PlanNavigationAnchor>,
        ) -> Option<(usize, forge_buffer::block::BlockAnchor)>,
    ) -> Result<serde_json::Value> {
        self.retain_annotation_revision();
        let previous_revealed = self.source.revealed.clone();
        let previous_collapse = self.source.collapse.clone();
        self.source
            .revealed
            .retain(|(revealed_path, _, _)| revealed_path != path);
        self.source
            .revealed
            .insert((path.to_owned(), side.to_owned(), selected_row as u32));
        let projection = (|| -> Result<_> {
            let (mut blocks, mut targets) = super::design_review::project(
                &self.source.document,
                &self.width,
                crate::plan::design_review::ReviewAnnotationView {
                    annotation: &[],
                    revision: &self.annotation_revision,
                    focused: None,
                },
                &self.source.declaration_syntax,
                self.source.public_only,
                self.source.trace.as_ref(),
                &self.source.revealed,
                self.source.files.as_ref(),
                None,
            )?;
            let (selected, jump) = locate(&blocks, &targets)
                .context("reference no longer has a display occurrence")?;
            let endpoint: HashMap<_, _> = blocks
                .iter()
                .enumerate()
                .map(|(index, block)| (block.id.clone(), index))
                .collect();
            for (index, block) in blocks.iter_mut().enumerate() {
                for fold in &mut block.metadata.fold {
                    let end = endpoint[&fold.end.block] + usize::from(fold.end.position.row > 0);
                    if index <= selected && selected < end {
                        self.source.collapse.insert(fold.id.clone(), false);
                        fold.closed = false;
                    }
                }
            }
            let mut blocks = forge_buffer::collapse::project(blocks, &self.source.collapse)?;
            super::review_projection::group_changes(
                &mut blocks,
                &targets,
                &self.source.viewed_files,
            )?;
            let visible: std::collections::HashSet<_> = blocks
                .iter()
                .flat_map(|block| &block.metadata.target)
                .map(|range| range.id.clone())
                .collect();
            targets.retain(|id, _| visible.contains(id));
            ensure!(
                blocks.iter().any(|block| block.id == jump.block),
                "reference destination remains collapsed"
            );
            let patch = self
                .document
                .splice(0..self.document.block_count(), blocks)?;
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
        let demand_file = super::review_projection::change_file(&jump.block).filter(|file| {
            self.source
                .files
                .as_ref()
                .is_some_and(|inventory| !inventory.loaded.contains(*file))
        });
        Ok(
            serde_json::json!({"patch":patch,"source_edit":source_edit,"annotation":self.annotation.annotation(),"jump":jump,"demand_file":demand_file}),
        )
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
            let design = self
                .source
                .document
                .design
                .as_ref()
                .context("declaration design is unavailable")?;
            let symbols =
                super::review_symbols::NarrativeSymbols::new(design, &self.source.workspace)?;
            let matching = symbols.matching(name);
            if action == "jump_entity" || action == "open" || action.starts_with("jump_symbol:") {
                if matching.len() == 1 {
                    return self
                        .declaration_destination("proposed", matching[0].destination.clone());
                }
                if let Some(index) = action.strip_prefix("jump_symbol:") {
                    let index = index.parse::<usize>().context("invalid symbol selection")?;
                    let symbol = matching
                        .get(index)
                        .context("symbol selection is no longer available")?;
                    return self.declaration_destination("proposed", symbol.destination.clone());
                }
                if !matching.is_empty() {
                    let choices = matching.iter().enumerate().map(|(index, symbol)| serde_json::json!({
                        "index": index, "label": format!("{}:{} — {}", symbol.destination.path, symbol.destination.line, symbol.destination.name)
                    })).collect::<Vec<_>>();
                    return Ok(serde_json::json!({"symbol_choices": choices}));
                }
                if symbols.removed(name) {
                    return Ok(
                        serde_json::json!({"message":format!("{name} is removed by this plan")}),
                    );
                }
                let excluded = design
                    .baseline
                    .keys()
                    .chain(design.parsed()?.proposed.keys())
                    .map(|path| self.source.workspace.join(path))
                    .collect::<Vec<_>>();
                return Ok(
                    serde_json::json!({"workspace_symbol":{"name":name,"workspace":self.source.workspace,"excluded":excluded}}),
                );
            }
            return Ok(
                serde_json::json!({"message":"Use go to definition on this inline reference"}),
            );
        }
        if self.source.document.design.is_some() {
            ensure!(
                matches!(action, "jump_entity" | "open"),
                "this design action is unavailable"
            );
            if let PlanReviewTarget::File { path } = &anchor.target {
                return Ok(
                    serde_json::json!({"source":{"path":self.source.workspace.join(path),"line":1,"column":0}}),
                );
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
                    if let Some(name) = entity_name
                        && let Some(column) = block.text.row(position.row).and_then(|row| {
                            row.find(callable.filter(|_| cursor_entity.is_none()).unwrap_or(name))
                        })
                    {
                        position.column = column;
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
                crate::plan::codec::to_string_pretty(entity)?
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
        if let super::PlanReviewTarget::Call {
            path,
            side,
            owner,
            name,
            kind,
        } = &anchor.target
        {
            let baseline = side == "baseline";
            let mut reference = self
                .reference
                .lock()
                .map_err(|_| anyhow::anyhow!("plan reference cache lock poisoned"))?;
            let index = match reference.entry(baseline) {
                std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(super::references::PlanReferenceIndex::build(
                        &self.source.document,
                        &self.source.workspace,
                        baseline,
                    )?)
                }
            };
            if let Some(identity) = index.selected(anchor, column as u32)
                && let Some(destination) = index.destination(
                    &self.source.document,
                    &self.source.workspace,
                    identity,
                    side == "baseline",
                )?
            {
                return Ok(crate::declaration::DeclarationResolution::Resolved { destination });
            }
            let resolver = self
                .source
                .resolver
                .get()
                .context("declaration resolver is unavailable")?;
            let mut resolver = resolver
                .lock()
                .map_err(|_| anyhow::anyhow!("declaration resolver lock poisoned"))?;
            let resolver = if side == "baseline" {
                &mut resolver.1
            } else {
                &mut resolver.0
            };
            return Ok(match kind {
                super::CallKind::Call | super::CallKind::Callback => {
                    resolver.callable(path, owner, name)
                }
                super::CallKind::Property => resolver.property(path, owner, name),
                super::CallKind::Value => resolver.value(path, owner, name),
                super::CallKind::Construct => resolver.construct(path, owner, name),
            });
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
            design.parsed()?.proposed.get(path)
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

    fn workspace_definition(
        &self,
        anchor: &PlanNavigationAnchor,
        row: &str,
        column: usize,
    ) -> Result<Option<super::WorkspaceDefinition>> {
        use forge_diff::syntax::{DeclarationOverview, DeclarationPosition};
        let Some(design) = &self.source.document.design else {
            return Ok(None);
        };
        let (path, side) = match &anchor.target {
            super::PlanReviewTarget::Call { path, side, .. }
            | super::PlanReviewTarget::Declaration { path, side, .. } => (path, side),
            _ => return Ok(None),
        };
        let source_path = if side == "baseline" {
            path
        } else {
            design
                .moved
                .iter()
                .find(|(_, destination)| *destination == path)
                .map(|(original, _)| original)
                .unwrap_or(path)
        };
        let Some(baseline) = design.baseline.get(source_path) else {
            return Ok(None);
        };
        let Some(source) = super::design::workspace_source(&self.source.workspace, source_path)?
        else {
            return Ok(None);
        };
        if super::digest(source.as_bytes()) != baseline.source_digest {
            return Ok(None);
        }
        let position = match &anchor.target {
            super::PlanReviewTarget::Declaration {
                line,
                column: saved_column,
                ..
            } => {
                let saved = if side == "baseline" {
                    &baseline.text
                } else {
                    let Some(saved) = design.parsed()?.proposed.get(path) else {
                        return Ok(None);
                    };
                    saved
                };
                let position = DeclarationOverview::token_position(
                    path,
                    saved,
                    DeclarationPosition {
                        line: *line,
                        column: saved_column.unwrap_or(0),
                    },
                    row,
                    column,
                )
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                if super::workspace_definition::map_position(path, saved, &baseline.text, position)?
                    .is_none()
                {
                    return Ok(None);
                }
                super::workspace_definition::map_position(path, saved, &source, position)?
            }
            _ => None,
        };
        Ok(position.map(|position| super::WorkspaceDefinition {
            path: self
                .source
                .workspace
                .join(source_path)
                .to_string_lossy()
                .into_owned(),
            digest: baseline.source_digest.clone(),
            line: position.line,
            column: position.column,
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
        if let super::PlanReviewTarget::Behavior {
            path,
            side,
            owner,
            offset,
        } = &anchor.target
        {
            let design = self
                .source
                .document
                .design
                .as_ref()
                .context("declaration design is unavailable")?;
            let (declarations, bodies) = if side == "baseline" {
                (
                    design.baseline.get(path).map(|file| file.text.as_str()),
                    &design.baseline_calls,
                )
            } else {
                (
                    design.parsed()?.proposed.get(path).map(String::as_str),
                    &design.parsed()?.proposed_calls,
                )
            };
            let body = bodies
                .get(path)
                .into_iter()
                .flatten()
                .find(|body| body.owner == *owner)
                .context("Algorithm callable is unavailable")?;
            let reference = match super::calls::algorithm_reference(
                path,
                declarations.context("Algorithm declarations are unavailable")?,
                body,
                *offset,
                row,
                column,
            ) {
                Ok(Some(reference)) => reference,
                Ok(None) => {
                    return Ok(
                        serde_json::json!({"message":"Select a parsed identifier in the Algorithm."}),
                    );
                }
                Err(error) => return Ok(serde_json::json!({"message":error.to_string()})),
            };
            match reference {
                super::calls::AlgorithmTarget::Binding { name, position } => {
                    return self.declaration_destination(
                        side,
                        crate::declaration::DeclarationDestination {
                            path: self
                                .source
                                .workspace
                                .join(path)
                                .to_string_lossy()
                                .into_owned(),
                            line: position.line,
                            column: position.column,
                            proposed: true,
                            name,
                            module_file: false,
                        },
                    );
                }
                super::calls::AlgorithmTarget::Reference { name, kind } => {
                    selected.target = super::PlanReviewTarget::Call {
                        path: path.clone(),
                        side: side.clone(),
                        owner: owner.clone(),
                        name,
                        kind,
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
            super::PlanReviewTarget::Declaration { side, .. }
            | super::PlanReviewTarget::Call { side, .. } => side,
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

    fn declaration_destination(
        &mut self,
        side: &str,
        destination: crate::declaration::DeclarationDestination,
    ) -> Result<serde_json::Value> {
        let design = self
            .source
            .document
            .design
            .as_ref()
            .context("declaration design is unavailable")?;
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
                    design.parsed()?.proposed.contains_key(relative)
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
                design.parsed()?.proposed.get(&relative)
            };
            let Some(text) = text else {
                anyhow::ensure!(
                    !destination.proposed,
                    "resolved declaration snapshot is unavailable"
                );
                return Ok(
                    serde_json::json!({"source":{"path":destination.path,"line":destination.line,"column":destination.column}}),
                );
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
                        && let Some(column) = block
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
            let calls = if side == "baseline" {
                &design.baseline_calls
            } else {
                &design.parsed()?.proposed_calls
            };
            let combined = super::calls::present(
                &relative,
                text,
                calls.get(&relative).map(Vec::as_slice).unwrap_or_default(),
            )?;
            let selected_row = *combined
                .declaration_row
                .get(&(display_position.line as usize - 1))
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
        mut source: PlanReviewSource,
        width: WidthProfile,
        focused_annotation: Option<String>,
    ) -> Result<Self> {
        view.validate()?;
        source.viewed_files = source.read_viewed_files()?;
        let annotation_path = source.plan_directory()?.join("working.json");
        let mut annotation =
            ReviewAnnotationStore::open(annotation_path, source.saved_digest.clone())?;
        if source.document.design.is_some() {
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
            diff_view: DiffViewState::new(
                DiffCapabilities::SemanticOnly,
                if source.public_only {
                    DiffVisibility::Public
                } else {
                    DiffVisibility::All
                },
            ),
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
        let tests = self
            .source
            .document
            .design
            .iter()
            .flat_map(|design| &design.document.tests)
            .flat_map(|file| {
                file.cases
                    .iter()
                    .map(move |case| ((file.file.as_str(), case.name.as_str()), case.change))
            })
            .collect::<HashMap<_, _>>();
        let mut test_summary = TestChangeCounts::default();
        let mut file_summary = HashMap::<_, TestChangeCounts>::new();
        for ((path, _), change) in &tests {
            let file = file_summary.entry(*path).or_default();
            for summary in [&mut test_summary, file] {
                summary.add(*change);
            }
        }
        for block in &self.document {
            for position in 0..block.text.row_count() {
                let target = block.metadata.target.iter().find(|target| {
                    target.range.start.row <= position && position < target.range.end.row
                });
                let line = target
                    .and_then(|target| self.target.get(&target.id))
                    .map(|anchor| anchor.line)
                    .unwrap_or_else(|| {
                        if self.source.document.design.is_some() {
                            0
                        } else {
                            row_count + 1
                        }
                    });
                let test = target
                    .and_then(|target| self.target.get(&target.id))
                    .and_then(|anchor| match &anchor.target {
                        super::PlanReviewTarget::TestCase { path, name } => {
                            Some(super::DesignTestSelection {
                                file: path.clone(),
                                name: name.clone(),
                            })
                        }
                        _ => None,
                    });
                let check = target
                    .and_then(|target| self.target.get(&target.id))
                    .and_then(|anchor| {
                        if let super::PlanReviewTarget::Check { id } = &anchor.target {
                            Some(id)
                        } else {
                            None
                        }
                    });
                let test_change = test
                    .as_ref()
                    .and_then(|test| tests.get(&(test.file.as_str(), test.name.as_str())));
                let test_file = target
                    .and_then(|target| self.target.get(&target.id))
                    .and_then(|anchor| {
                        matches!(
                            anchor.target,
                            super::PlanReviewTarget::Section {
                                section: super::PlanSection::Tests
                            }
                        )
                        .then_some(anchor.path.as_ref())
                        .flatten()
                    });
                let id = format!("{}:{}", block.id.0, position);
                let anchor = serde_json::json!({"id":id,"source_line":line,"test":test,"check":check,
                    "test_change":test_change,"test_file":test_file,
                    "test_summary":(block.id.0 == "plan:section:tests").then_some(&test_summary)
                        .or_else(|| test_file.and_then(|path| file_summary.get(path.as_str())))});
                if self.source_anchor.get(&id) != Some(&anchor) {
                    row.push(anchor.clone());
                }
                current.insert(id, anchor);
                row_count += 1;
            }
        }
        let mut removed = self
            .source_anchor
            .keys()
            .filter(|id| !current.contains_key(*id))
            .cloned()
            .collect::<Vec<_>>();
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

    fn toggle_visibility(&mut self) -> Result<serde_json::Value> {
        ensure!(
            self.source.document.design.is_some(),
            "public visibility requires a declaration design"
        );
        self.retain_annotation_revision();
        let request = self
            .diff_view
            .request(DiffViewAction::ToggleVisibility)
            .map_err(|error| anyhow::anyhow!("plan visibility request failed: {error:?}"))?;
        let public_only = request.options.visibility == DiffVisibility::Public;
        self.source.public_only = public_only;
        // Revealing a hidden declaration lifts Public filtering for its file side. An explicit
        // visibility toggle supersedes that per-navigation override in both directions.
        let previous_revealed = std::mem::take(&mut self.source.revealed);
        let projection = super::review_projection::project(
            &self.source,
            &self.width,
            &[],
            &self.annotation_revision,
            None,
        )
        .and_then(|(block, target)| {
            let patch = self
                .document
                .splice(0..self.document.block_count(), block)?;
            Ok((patch, target))
        });
        if projection.is_err() {
            self.diff_view.fail(request);
            self.source.revealed = previous_revealed;
            self.source.public_only =
                self.diff_view.committed().visibility == DiffVisibility::Public;
        }
        let (patch, target) = projection?;
        ensure!(
            self.diff_view.commit(request),
            "plan visibility request was superseded"
        );
        self.source.public_only = public_only;
        self.focused_annotation = None;
        self.target = target;
        let source_edit = self.source_edit();
        Ok(
            serde_json::json!({"patch":patch, "source_edit":source_edit, "annotation":self.annotation.annotation(), "public_only":public_only, "diff_view": self.diff_view.committed(), "diff_capabilities": DiffCapabilities::SemanticOnly}),
        )
    }

    /// Splice the requested body while preserving every other cached subtree.
    fn materialize_file(&mut self, file: &str) -> Result<serde_json::Value> {
        let inventory = self
            .source
            .files
            .as_ref()
            .context("plan has no deferred files")?;
        if inventory.loaded.contains(file) {
            return Ok(serde_json::json!({}));
        }
        let heading = self
            .document
            .block(&BlockId(file.to_owned()))
            .context("plan file heading is missing")?;
        let anchor = heading
            .metadata
            .target
            .iter()
            .find_map(|range| self.target.get(&range.id))
            .context("plan file anchor is missing")?;
        let path = anchor
            .path
            .as_deref()
            .context("plan file path is missing")?;
        let (body, targets) = super::design_review::project(
            &self.source.document,
            &self.width,
            super::design_review::ReviewAnnotationView {
                annotation: &[],
                revision: &self.annotation_revision,
                focused: None,
            },
            &self.source.declaration_syntax,
            self.source.public_only,
            self.source.trace.as_ref(),
            &self.source.revealed,
            self.source.files.as_ref(),
            Some(super::design_review::FileBody {
                id: file,
                path,
                first_line: anchor.line,
            }),
        )?;
        let body = forge_buffer::collapse::project(body, &self.source.collapse)?;
        let owns = |id: &str| {
            id == file
                || id
                    .strip_prefix(file)
                    .is_some_and(|suffix| suffix.starts_with(':'))
        };
        ensure!(!body.is_empty(), "expanded plan file is missing");
        let start = self
            .document
            .block_index(&BlockId(file.to_owned()))
            .context("plan file heading is missing")?;
        let mut removed = Vec::new();
        let mut end = start;
        for block in self
            .document
            .blocks(self.document.revision(), start..self.document.block_count())?
            .take_while(|block| owns(&block.id.0))
        {
            removed.extend(block.metadata.target.iter().map(|range| range.id.clone()));
            end += 1;
        }
        let mut added = Vec::new();
        for range in body.iter().flat_map(|block| &block.metadata.target) {
            if let Some(anchor) = targets.get(&range.id) {
                added.push((range.id.clone(), anchor.clone()));
            }
        }
        // BufferDocument rebases enclosing section folds when their endpoint is replaced.
        let patch = self.document.splice(start..end, body)?;
        for id in removed {
            self.target.remove(&id);
        }
        self.target.extend(added);
        self.source
            .files
            .as_mut()
            .unwrap()
            .loaded
            .insert(file.to_owned());
        Ok(
            serde_json::json!({"patch":patch, "source_edit":self.source_edit(), "annotation":self.annotation.annotation()}),
        )
    }

    /// Move the cached whole-file subtree, including an unloaded heading.
    fn group_review_files(&mut self, viewed: &BTreeSet<String>) -> Result<serde_json::Value> {
        let mut blocks = self.document.snapshot().block;
        super::review_projection::group_changes(&mut blocks, &self.target, viewed)?;
        let patch = self
            .document
            .splice(0..self.document.block_count(), blocks)?;
        Ok(
            serde_json::json!({"patch":patch, "source_edit":self.source_edit(), "annotation":self.annotation.annotation()}),
        )
    }

    fn toggle_declaration(
        &mut self,
        owner: &forge_buffer::identity::BlockId,
    ) -> Result<serde_json::Value> {
        let collapse = self
            .document
            .block(owner)
            .and_then(|block| block.metadata.collapse.first())
            .cloned()
            .context("cursor is not inside a collapsible declaration")?;
        self.retain_annotation_revision();
        let previous = self
            .source
            .collapse
            .insert(collapse.id.clone(), !collapse.closed);
        let projection = super::review_projection::project(
            &self.source,
            &self.width,
            &[],
            &self.annotation_revision,
            None,
        )
        .and_then(|(block, target)| {
            let patch = self
                .document
                .splice(0..self.document.block_count(), block)?;
            Ok((patch, target))
        });
        if projection.is_err() {
            if let Some(previous) = previous {
                self.source.collapse.insert(collapse.id.clone(), previous);
            } else {
                self.source.collapse.remove(&collapse.id);
            }
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
            let patch = self
                .document
                .splice(0..self.document.block_count(), block)?;
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

    use super::*;

    #[test]
    fn moving_a_file_preserves_proposal_order_without_requesting_navigation() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path())?;
        crate::checks::configure_fixture(&file.workspace);
        let mut plan = crate::plan::references::tests::scoring_plan();
        let design = plan.design.as_mut().unwrap();
        design.proposed_source.insert(
            "State.rs".into(),
            "/// Tracks a round.\npub struct State;\n".into(),
        );
        design.parsed =
            crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())?.parsed;
        file.write_working_document("session", "plan", &plan)?;
        let (_, _, checksum) = file.submit_document_revision("session", "plan", 1, 1)?;
        let source = file.capture_review_source("session", "plan", None, &checksum)?;
        let mut review = ReadOnlyPlanReview::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )?;
        let original = review.snapshot().block;
        let files: Vec<_> = original
            .iter()
            .filter(|block| {
                super::super::review_projection::change_file(&block.id) == Some(&block.id.0)
            })
            .map(|block| block.id.0.clone())
            .collect();
        assert_eq!(files.len(), 2);
        let viewed = BTreeSet::from([files[0].clone()]);
        let result = review.group_review_files(&viewed)?;
        assert!(result.get("jump").is_none());
        let moved = review.snapshot().block;
        let position = |id: &str| moved.iter().position(|block| block.id.0 == id).unwrap();
        let separator = &moved[position("plan:section:changes:viewed") - 1];
        assert_eq!(separator.id.0, "plan:section:changes:unviewed:separator");
        assert_eq!(separator.text.row(0), Some(""));
        assert!(position(&files[1]) < position("plan:section:changes:viewed"));
        assert!(position("plan:section:changes:viewed") < position(&files[0]));
        review.source.viewed_files = viewed;
        review.group_review_files(&BTreeSet::new())?;
        assert_eq!(review.snapshot().block, original);
        Ok(())
    }

    #[test]
    fn lazy_files_hide_empty_headings_and_preserve_canonical_comment_lines() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path())?;
        crate::checks::configure_fixture(&store.workspace);
        let mut plan = crate::plan::references::tests::scoring_plan();
        let design = plan.design.as_mut().unwrap();
        design
            .proposed_source
            .insert("private.rs".into(), "fn private_change();\n".into());
        design.parsed =
            crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())?.parsed;
        store.write_working_document("session", "plan", &plan)?;
        let (_, _, digest) = store.submit_document_revision("session", "plan", 1, 1)?;
        let open = |lazy, legacy| -> Result<_> {
            let mut source = store.capture_review_source("session", "plan", None, &digest)?;
            if lazy {
                let mut rendered = source.rendered.clone();
                if legacy {
                    rendered.navigation.file_visibility.clear();
                }
                source.files = Some(crate::plan::review_files::ReviewFiles::new(
                    source.document.design.as_ref().unwrap(),
                    &rendered,
                    &BTreeSet::new(),
                )?);
            }
            ReadOnlyPlanReview::new(
                DocumentId("review".into()),
                ViewId("view".into()),
                source,
                WidthProfile::default(),
                None,
            )
        };
        let mut review = open(true, false)?;
        let full = open(false, false)?;
        assert_eq!(review.snapshot().block, open(true, true)?.snapshot().block);
        let file_id =
            |path: &str| format!("plan:design:file:{}", crate::plan::digest(path.as_bytes()));
        let private = file_id("private.rs");
        let public = file_id("Rounds.rs");
        let tests = format!("plan:tests:file:{}", crate::plan::digest(b"tests.rs"));
        let deferred = |review: &ReadOnlyPlanReview, file: &str| {
            (&review.document)
                .into_iter()
                .any(|row| row.id.0 == format!("{file}:hunk:deferred"))
        };
        assert!(review.document.block(&BlockId(private.clone())).is_none());
        for file in [&public, &tests] {
            assert!(deferred(&review, file));
            let id = BlockId(file.clone());
            assert_eq!(
                review.document.block(&id).unwrap().text,
                full.document.block(&id).unwrap().text
            );
        }
        let mut jumped = open(true, false)?;
        let destination = crate::declaration::DeclarationDestination {
            path: jumped
                .source
                .workspace
                .join("Rounds.rs")
                .to_string_lossy()
                .into_owned(),
            name: "score".into(),
            line: 2,
            column: 7,
            proposed: true,
            module_file: false,
        };
        let result = jumped.declaration_destination("proposed", destination)?;
        assert_eq!(result["demand_file"], public);
        jumped.materialize_file(&public)?;
        let destination = BlockId(
            result["jump"]["block"]
                .as_str()
                .context("missing jump")?
                .to_owned(),
        );
        assert!(jumped.document.block(&destination).is_some());
        assert!(jumped.document.block(&BlockId(private.clone())).is_none());
        assert!(deferred(&jumped, &tests));
        for file in [&tests, &public] {
            review.materialize_file(file)?;
            assert!(!deferred(&review, file));
            for (id, anchor) in &review.target {
                if let Some(expected) = full.target.get(id) {
                    assert_eq!(anchor.line, expected.line, "{id:?}");
                }
            }
            if file != &public {
                assert!(deferred(&review, &public));
            }
            assert_eq!(review.materialize_file(file)?, serde_json::json!({}));
        }
        assert_eq!(review.snapshot().block, full.snapshot().block);
        assert!(
            (&review.document)
                .into_iter()
                .all(|row| row.id.0 != private)
        );
        assert!(
            !(&review.document)
                .into_iter()
                .any(|row| row.id.0.starts_with(&format!("{private}:")))
        );
        review.toggle_visibility()?;
        assert!(deferred(&review, &private));
        review.materialize_file(&private)?;
        assert!((&review.document).into_iter().any(|row| {
            row.text
                .wire_rows()
                .iter()
                .any(|text| text.contains("fn private_change"))
        }));
        Ok(())
    }

    /// A comment on prose rows, which share their section's navigation target, keeps its rows
    /// by their rendered text when a review edit moves later rows.
    #[tokio::test]
    async fn comment_on_shared_section_rows_keeps_its_text_after_test_deletion() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Review prose.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose shared state.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior.".into()];
        design.document.design =
            "Use a focused state owner.\n\nReturn every share in order.\n\nReject an empty part count."
                .into();
        design.document.tests = vec![crate::plan::design_tests::DesignTestFile {
            file: "tests.rs".into(),
            cases: ["first_case", "second_case"]
                .map(|name| crate::plan::design_tests::DesignTestCase {
                    name: name.into(),
                    target: None,
                    change: crate::plan::design_tests::DesignTestChange::New,
                    description: "covers one case".into(),
                })
                .into(),
        }];
        design.proposed_source.insert(
            "state.rs".into(),
            "/// Owns shared state.\npub struct State;\n".into(),
        );
        design.parsed = crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())
            .unwrap()
            .parsed;
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (submitted, rendered, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let rows = rendered.markdown.lines().collect::<Vec<_>>();
        let line_of = |rows: &[&str], text: &str| {
            rows.iter()
                .position(|row| row.contains(text))
                .map(|index| index as u32 + 1)
                .unwrap()
        };
        let start = line_of(&rows, "Return every share in order.");
        let end = line_of(&rows, "Reject an empty part count.");
        let working = file.plan_dir("session", "plan").join("working.json");
        ReviewAnnotationStore::open(
            working.clone(),
            crate::plan::digest(&std::fs::read(&working).unwrap()),
        )
        .unwrap()
        .replace(vec![ReviewAnnotation {
            parent_id: None,
            id: "comment".into(),
            kind: ReviewAnnotationKind::Comment,
            reply: None,
            anchor: None,
            source: crate::plan::PlanAnnotationInput {
                start_line: start,
                end_line: end,
                body: "Explain both rules.".into(),
            },
        }])
        .unwrap();
        let (_, rendered, _) = file
            .remove_tests(
                "session",
                "plan",
                submitted.version,
                &checksum,
                &[crate::plan::DesignTestSelection {
                    file: "tests.rs".into(),
                    name: "first_case".into(),
                }],
                None,
            )
            .await
            .unwrap();
        let saved = ReviewAnnotationStore::open(
            working.clone(),
            crate::plan::digest(&std::fs::read(&working).unwrap()),
        )
        .unwrap()
        .annotation()
        .to_vec();
        let rows = rendered.markdown.lines().collect::<Vec<_>>();
        let source = &saved[0].source;
        assert_eq!(
            (
                rows[source.start_line as usize - 1],
                rows[source.end_line as usize - 1]
            ),
            (
                rows[line_of(&rows, "Return every share in order.") as usize - 1],
                rows[line_of(&rows, "Reject an empty part count.") as usize - 1]
            )
        );
    }

    /// Open the review of the submitted plan and request the rename preview at the declaration
    /// row containing `declaration`, `offset` characters into it.
    fn rename_preview(
        file: &PlanFileStore,
        checksum: &str,
        declaration: &str,
        offset: usize,
    ) -> (BufferSnapshot, serde_json::Value) {
        let source = file
            .capture_review_source("session", "plan", None, checksum)
            .unwrap();
        let id = DocumentId("rename".into());
        let document = ReadOnlyPlanReview::new(
            id.clone(),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let snapshot = document.snapshot();
        let rendered = snapshot
            .block
            .iter()
            .map(|block| block.text.wire_rows())
            .collect::<Vec<_>>();
        let (block, row, column) = snapshot
            .block
            .iter()
            .find_map(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .enumerate()
                    .find_map(|(row, text)| {
                        text.find(declaration)
                            .map(|column| (block.clone(), row, column + offset))
                    })
            })
            .unwrap_or_else(|| panic!("no declaration row in {rendered:#?}"));
        let target = block
            .metadata
            .target
            .iter()
            .find(|target| {
                (target.range.start.row, target.range.start.column) <= (row, column)
                    && (row, column) < (target.range.end.row, target.range.end.column)
            })
            .or_else(|| block.metadata.target.first())
            .cloned();
        let store = PlanReviewStore::default();
        let admission = store.admit(id.clone()).unwrap();
        store.insert(document, admission).unwrap();
        let result = store
            .action(
                DocumentInput {
                    document: id,
                    revision: snapshot.revision,
                    view: ViewId("view".into()),
                    sequence: InputSequence(1),
                    action: "rename_entity".into(),
                    block: block.id.clone(),
                    position: forge_buffer::block::TextPosition { row, column },
                    target: target.map(|target| target.id),
                },
                None,
            )
            .unwrap();
        (snapshot, result)
    }

    /// The rename preview lists every inline-code mention the rename rewrites, in narrative rows,
    /// test descriptions, flow text, and Mermaid labels, beside the declaration itself.
    #[test]
    fn rename_preview_lists_narrative_mentions() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let canonical = crate::plan::references::tests::scoring_plan();
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let (snapshot, result) = rename_preview(&file, &checksum, "fn score(", 3);
        let rows = snapshot
            .block
            .iter()
            .map(|block| {
                (
                    block.id.0.clone(),
                    block.text.row(0).unwrap_or("").to_owned(),
                )
            })
            .collect::<std::collections::HashMap<_, _>>();
        let mut previewed = result["rename"]["preview"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|edit| {
                let block = edit["block"].as_str()?;
                let column = edit["column"].as_u64()? as usize;
                let row = &rows[block];
                (block.starts_with("plan:metadata:") || block.contains(":description:"))
                    .then(|| (row.trim().to_owned(), row[column..column + 5].to_owned()))
            })
            .collect::<Vec<_>>();
        previewed.sort();
        let mut expected = [
            "A[\"`play`\"] --> B[\"`score`\"]",
            "Call `score` once per round; `play` drives it.",
            "Each round calls `score`.",
            "- Keep `score` pure and leave `scored` alone.",
            "`Rounds::score` returns the round.",
            "`score` returns its round.",
        ]
        .map(|row| (row.to_owned(), "score".to_owned()))
        .to_vec();
        expected.sort();
        assert_eq!(previewed, expected);
    }

    /// The rename preview names every declaration already in the renamed symbol's scope, those the
    /// modified workspace file already declares as well as those the plan adds, so the editor can
    /// refuse a colliding name before it asks for confirmation; a rename to such a name sent anyway
    /// is refused and leaves the plan unchanged.
    #[test]
    fn rename_preview_lists_the_names_in_scope_and_apply_refuses_them() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let canonical = crate::plan::references::tests::existing_module_allocation_plan();
        let existing = &canonical.design.as_ref().unwrap().baseline["src/money.ts"].text;
        std::fs::create_dir_all(file.workspace.join("src")).unwrap();
        std::fs::write(file.workspace.join("src/money.ts"), existing).unwrap();
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let (_, result) = rename_preview(&file, &checksum, "function moneyAllocate(", 9);
        let existing_names = result["rename"]["existing_names"]
            .as_array()
            .unwrap_or_else(|| panic!("the preview names no existing names: {result}"))
            .iter()
            .filter_map(serde_json::Value::as_str)
            .collect::<Vec<_>>();
        assert_eq!(
            (result["rename"]["name"].as_str(), existing_names),
            (
                Some("moneyAllocate"),
                vec!["moneyAllocate", "moneyShare", "moneySplit"]
            )
        );
        let identity = result["rename"]["symbol"].as_str().unwrap();
        let working = file.read_working_document("session", "plan").unwrap();
        let refusal = file
            .rename_symbol_from_document("session", &working, identity, "moneyShare", None)
            .err()
            .map(|error| error.to_string());
        assert_eq!(
            refusal.as_deref(),
            Some("`moneyShare` already exists in this scope")
        );
        let unchanged = file.read_working_document("session", "plan").unwrap();
        assert_eq!(unchanged, working);
    }

    /// A rename rewrites rows of a section that holds a saved multi-row comment; the comment keeps
    /// its rows, matched by their text or their place in the section, rather than moving to the
    /// section's first row, which shares the rows' navigation target.
    #[test]
    fn rename_keeps_a_saved_comment_on_its_rows() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::references::tests::scoring_plan();
        canonical.design.as_mut().unwrap().document.design =
            "Each round has a total.\n\nKeep the totals.\n\n`score` returns the round.\n\nRecord the winner."
                .into();
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (submitted, rendered, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let line_of = |rendered: &crate::plan::RenderedPlan, text: &str| {
            rendered
                .markdown
                .lines()
                .position(|row| row.contains(text))
                .map(|index| index as u32 + 1)
                .unwrap()
        };
        let (start, end) = (
            line_of(&rendered, "Keep the totals."),
            line_of(&rendered, "`score` returns the round."),
        );
        let target = |line| {
            rendered
                .navigation
                .resolve_line(line)
                .unwrap()
                .target
                .clone()
        };
        let working = file.plan_dir("session", "plan").join("working.json");
        ReviewAnnotationStore::open(
            working.clone(),
            crate::plan::digest(&std::fs::read(&working).unwrap()),
        )
        .unwrap()
        .replace(vec![ReviewAnnotation {
            parent_id: None,
            id: "comment".into(),
            kind: ReviewAnnotationKind::Comment,
            reply: None,
            anchor: Some(super::super::ReviewAnnotationAnchor {
                start: target(start),
                end: target(end),
            }),
            source: crate::plan::PlanAnnotationInput {
                start_line: start,
                end_line: end,
                body: "Explain both rules.".into(),
            },
        }])
        .unwrap();
        let id = DocumentId("rename".into());
        let document = ReadOnlyPlanReview::new(
            id.clone(),
            ViewId("view".into()),
            file.capture_review_source("session", "plan", None, &checksum)
                .unwrap(),
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let store = PlanReviewStore::default();
        let admission = store.admit(id.clone()).unwrap();
        store.insert(document, admission).unwrap();
        let index =
            crate::plan::references::PlanReferenceIndex::planned(&submitted, &file.workspace)
                .unwrap();
        let identity = index
            .definition
            .iter()
            .find(|((path, line, _), _)| path == "Rounds.rs" && *line == 2)
            .map(|(_, (identity, _))| identity.clone())
            .unwrap();
        let working_document = file.read_working_document("session", "plan").unwrap();
        assert_eq!(working_document.version, submitted.version);
        let (_, _, renamed, digest) = file
            .rename_symbol_from_document("session", &working_document, &identity, "tally", None)
            .unwrap();
        let mut source = super::super::review_source::EditableReviewSource::try_from(
            file.capture_review_source("session", "plan", None, &digest)
                .unwrap(),
        )
        .unwrap();
        store.prepare_update(&id, &mut source).unwrap();
        store.update(&id, source, submitted.version, None).unwrap();
        let saved = ReviewAnnotationStore::open(
            working.clone(),
            crate::plan::digest(&std::fs::read(&working).unwrap()),
        )
        .unwrap()
        .annotation()
        .to_vec();
        let rows = renamed.markdown.lines().collect::<Vec<_>>();
        let source = &saved
            .first()
            .expect("the rename dropped the comment")
            .source;
        assert_eq!(
            (
                rows[source.start_line as usize - 1],
                rows[source.end_line as usize - 1]
            ),
            ("Keep the totals.", "`tally` returns the round.")
        );
    }

    /// Open a stored plan whose flows still carry CRLF line endings from an earlier writer.
    #[test]
    fn stored_crlf_flows_open_in_review() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Review a flow.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose shared state.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior.".into()];
        design.document.design = "Use a focused state owner.".into();
        design.document.flows =
            "## Flow\r\nRuns once.\r\n\r\n```mermaid\r\nflowchart TD\r\n  a --> b\r\n```".into();
        design.proposed_source.insert(
            "state.rs".into(),
            "/// Owns shared state.\npub struct State;\n".into(),
        );
        design.parsed = crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())
            .unwrap()
            .parsed;
        canonical.design = Some(design);
        file.write_working_document("session", "plan", &canonical)
            .unwrap();
        let (_, _, checksum) = file
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let source = file
            .capture_review_source("session", "plan", Some(1), &checksum)
            .unwrap();
        let document = ReadOnlyPlanReview::new(
            DocumentId("flows".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let flow_heading = document
            .snapshot()
            .block
            .into_iter()
            .find(|block| block.id.0 == "plan:metadata:flows:0")
            .unwrap();
        assert_eq!(flow_heading.text.wire_rows(), ["## Flow"]);
        assert!(
            flow_heading
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.range.end.column == "## Flow".len())
        );
    }

    /// Keep code and same-section prose questions bound to their own saved selections.
    #[test]
    fn questions_preserve_design_and_persist_answers_bound_to_current_text() {
        let temporary = tempfile::tempdir().unwrap();
        let file = PlanFileStore::new(temporary.path().join("data"), temporary.path()).unwrap();
        crate::checks::configure_fixture(&file.workspace);
        let mut canonical = crate::plan::document::test_fixture("plan", "Question the API.");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose shared state.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design =
            "Use a focused state owner.\nPreserve independent readers.\nKeep writes explicit.\nReport failures."
                .into();
        design.proposed_source.insert(
            "state.rs".into(),
            "/// Owns shared state.\npub struct State;\n".into(),
        );
        design.parsed = crate::plan::DeclarationDesign::scratch(design.proposed_source.clone())
            .unwrap()
            .parsed;
        canonical.design = Some(design);
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
        let id = DocumentId("questions".into());
        let document = ReadOnlyPlanReview::new(
            id.clone(),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let anchor = document
            .source
            .rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| {
                matches!(
                    &anchor.target,
                    crate::plan::PlanReviewTarget::Declaration { .. }
                )
            })
            .unwrap();
        let line = anchor.line;
        let prose_start = document
            .source
            .rendered
            .markdown
            .lines()
            .position(|row| row == "Use a focused state owner.")
            .unwrap();
        let prose_line = u32::try_from(prose_start + 1).unwrap();
        let snapshot = document.snapshot();
        let target = snapshot.block[0].metadata.target[0].clone();
        let mut input = DocumentInput {
            document: id.clone(),
            revision: snapshot.revision,
            view: ViewId("view".into()),
            sequence: InputSequence(1),
            action: "session.plan.question.answer".into(),
            block: snapshot.block[0].id.clone(),
            position: target.range.start,
            target: Some(target.id),
        };
        let store = PlanReviewStore::default();
        let admission = store.admit(id.clone()).unwrap();
        store.insert(document, admission).unwrap();
        let mut question = ReviewAnnotation {
            parent_id: None,
            id: "question".into(),
            kind: ReviewAnnotationKind::Question,
            reply: None,
            anchor: None,
            source: crate::plan::PlanAnnotationInput {
                start_line: line,
                end_line: line,
                body: "Why use this owner?".into(),
            },
        };
        let mut comment = question.clone();
        comment.id = "comment".into();
        comment.kind = ReviewAnnotationKind::Comment;
        comment.source.body = "Keep this interface.".into();
        let prose_questions = [
            (prose_line, prose_line),
            (prose_line + 1, prose_line + 1),
            (prose_line + 2, prose_line + 3),
        ]
        .into_iter()
        .enumerate()
        .map(|(index, (start_line, end_line))| {
            let mut annotation = question.clone();
            annotation.id = format!("prose-{index}");
            annotation.source = crate::plan::PlanAnnotationInput {
                start_line,
                end_line,
                body: "why?".into(),
            };
            annotation
        })
        .collect();
        store
            .save_annotations(&id, &digest, prose_questions)
            .unwrap();
        let captured = store.questions("session", input.clone()).unwrap();
        assert_eq!(captured.len(), 3);
        for (question, expected) in captured.iter().zip([
            format!("{prose_line}: Use a focused state owner.\n"),
            format!("{}: Preserve independent readers.\n", prose_line + 1),
            format!(
                "{}: Keep writes explicit.\n{}: Report failures.\n",
                prose_line + 2,
                prose_line + 3
            ),
        ]) {
            let selected = question
                .prompt
                .split_once("\nSelected text:\n")
                .unwrap()
                .1
                .split_once("\nContext after:\n")
                .unwrap()
                .0;
            assert_eq!(selected, expected);
            assert!(
                question
                    .prompt
                    .contains("Reviewed plan: plan\nReviewed plan version: 1\n")
            );
            assert!(
                question
                    .prompt
                    .contains(&format!("Saved source digest: {digest}"))
            );
        }
        store
            .save_annotations(&id, &digest, vec![question.clone(), comment.clone()])
            .unwrap();
        let questions = store.questions("session", input.clone()).unwrap();
        assert_eq!(questions.len(), 1);
        assert!(
            questions[0].prompt.contains("pub struct State;")
                && questions[0].prompt.contains("Why use this owner?")
        );
        assert!(
            questions[0]
                .prompt
                .contains(&format!("Selected plan lines: {line}-{line}"))
        );
        assert!(questions[0].prompt.contains("Semantic source context"));
        let captured = store
            .answer(&id, &questions[0], "It isolates ownership.".into(), 3210)
            .unwrap();
        assert_eq!(
            captured[0].reply.as_ref().unwrap().body,
            "It isolates ownership."
        );
        store
            .save_annotations(&id, &digest, vec![question.clone(), comment.clone()])
            .unwrap();
        input.sequence = InputSequence(2);
        assert!(
            store
                .questions("session", input.clone())
                .unwrap()
                .is_empty(),
            "answered questions were submitted twice"
        );
        input.sequence = InputSequence(3);
        let feedback = store.submission(input.clone()).unwrap();
        assert_eq!(
            feedback["annotations"].as_array().unwrap().len(),
            1,
            "questions became revision instructions"
        );
        assert_eq!(feedback["annotations"][0]["body"], "Keep this interface.");
        let reopened = file
            .capture_review_source("session", "plan", Some(1), &checksum)
            .unwrap();
        let reopened = ReadOnlyPlanReview::new(
            DocumentId("reopened".into()),
            ViewId("other".into()),
            reopened,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert_eq!(
            reopened.annotation.annotation()[0]
                .reply
                .as_ref()
                .unwrap()
                .body,
            "It isolates ownership."
        );
        assert_eq!(
            reopened.annotation.annotation()[0]
                .reply
                .as_ref()
                .unwrap()
                .duration_ms,
            Some(3210)
        );
        let mut followup = question.clone();
        followup.id = "followup".into();
        followup.parent_id = Some(question.id.clone());
        followup.source.body = "How can callers observe it?".into();
        store
            .save_annotations(
                &id,
                &digest,
                vec![question.clone(), comment.clone(), followup.clone()],
            )
            .unwrap();
        input.sequence = InputSequence(4);
        let followup_question = store.questions("session", input.clone()).unwrap();
        assert_eq!(followup_question.len(), 1);
        assert!(
            followup_question[0].prompt.contains("Why use this owner?")
                && followup_question[0]
                    .prompt
                    .contains("It isolates ownership.")
                && followup_question[0].prompt.contains("pub struct State;")
        );
        store
            .answer(&id, &followup_question[0], "Use a getter.".into(), 1000)
            .unwrap();
        let mut change = comment.clone();
        change.id = "change".into();
        change.parent_id = Some(followup.id.clone());
        change.source.body = "Add that getter.".into();
        store
            .save_annotations(
                &id,
                &digest,
                vec![question.clone(), comment.clone(), followup, change],
            )
            .unwrap();
        input.sequence = InputSequence(5);
        let feedback = store.submission(input.clone()).unwrap();
        let body = feedback["annotations"][1]["body"].as_str().unwrap();
        for expected in [
            "Why use this owner?",
            "It isolates ownership.",
            "How can callers observe it?",
            "Use a getter.",
            "Add that getter.",
        ] {
            assert!(body.contains(expected), "missing thread entry: {expected}");
        }
        assert_eq!(
            body.matches("Add that getter.").count(),
            1,
            "the follow-up comment reached the revision twice: {body}"
        );
        assert!(
            !body.contains("Keep this interface."),
            "unrelated feedback entered the thread"
        );
        let reopened = file
            .capture_review_source("session", "plan", Some(1), &checksum)
            .unwrap();
        let reopened = ReadOnlyPlanReview::new(
            DocumentId("thread-reopened".into()),
            ViewId("thread-view".into()),
            reopened,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert_eq!(
            reopened.annotation.annotation()[3].parent_id.as_deref(),
            Some("followup")
        );
        assert_eq!(
            reopened.annotation.annotation()[2]
                .reply
                .as_ref()
                .unwrap()
                .body,
            "Use a getter."
        );
        question.source.body = "Why this interface?".into();
        store
            .save_annotations(&id, &digest, vec![question.clone(), comment])
            .unwrap();
        assert!(
            store.annotations(&id).unwrap()[0].reply.is_none(),
            "edited question retained a previous answer"
        );
        assert!(
            store
                .answer(&id, &questions[0], "Stale answer.".into(), 0)
                .is_err()
        );
        input.sequence = InputSequence(6);
        assert_eq!(
            store.questions("session", input).unwrap()[0].body,
            question.source.body
        );
        assert_eq!(
            std::fs::read(source_path).unwrap(),
            original,
            "answering changed the declaration artifact"
        );
    }
    use crate::plan::PlanFileStore;
}
