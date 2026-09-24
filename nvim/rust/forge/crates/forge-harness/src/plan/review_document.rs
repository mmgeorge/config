use super::PlanNavigationAnchor;
use super::review_annotation::{ReviewAnnotation, ReviewAnnotationStore};
use super::review_source::PlanReviewSource;
use anyhow::{Context, Result, ensure};
use forge_buffer::admission::{DocumentAdmission, DocumentAdmissionStore};
use forge_buffer::document::{BufferDocument, LocalEditPreparation};
use forge_buffer::editable::{LocalEdit, LocalEditResult};
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

impl PlanReviewStore {
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
            "annotations":document.annotation.annotation().iter().filter(|annotation| !annotation.source.body.trim().is_empty())
                .map(|annotation| &annotation.source).collect::<Vec<_>>() }),
        )
    }

    pub(crate) fn add_annotation(
        &self,
        input: DocumentInput,
        end: Option<DocumentInput>,
    ) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.add_annotation(input, end)
    }

    /// Change the focused annotation only after validating its document input.
    pub(crate) fn focus_annotation(&self, input: DocumentInput) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.focus_annotation(input)
    }

    /// Delete the annotation selected by a validated document input.
    pub(crate) fn delete_annotation(&self, input: DocumentInput) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.delete_annotation(input)
    }

    pub(crate) fn edit(&self, edit: LocalEdit) -> Result<LocalEditResult> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&edit.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.edit(edit)
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
            "path":document.source.path});
        store.insert(document.id.clone(), (document, admission));
        Ok(opened)
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
    fn describe(
        &self,
        anchor: PlanNavigationAnchor,
        action: &str,
        row: &str,
        column: usize,
    ) -> Result<serde_json::Value> {
        use super::PlanReviewTarget;
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

    fn add_annotation(
        &mut self,
        input: DocumentInput,
        end: Option<DocumentInput>,
    ) -> Result<serde_json::Value> {
        ensure!(
            !self.source.historical,
            "historical plan revisions are read-only"
        );
        if let Some(end) = &end {
            ensure!(
                end.document == input.document
                    && end.revision == input.revision
                    && end.view == input.view,
                "plan selection belongs to another document or view"
            );
        }
        let anchor = self.action(input)?;
        let end_line = match end {
            Some(end) => self.action(end)?.line,
            None => anchor.line,
        };
        let mut annotation = self.annotation.annotation().to_vec();
        annotation.retain(|annotation| !annotation.source.body.trim().is_empty());
        let id = uuid::Uuid::new_v4().to_string();
        annotation.push(ReviewAnnotation {
            id: id.clone(),
            source: super::PlanAnnotationInput {
                start_line: anchor.line.min(end_line),
                end_line: anchor.line.max(end_line),
                body: String::new(),
            },
        });
        self.retain_annotation_revision();
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
            &self.width,
            &annotation,
            &revision,
            Some(&id),
        )?;
        let mut candidate = self.document.clone();
        let patch = candidate.edit(0..candidate.block_count(), block)?;
        self.annotation.replace(annotation)?;
        self.document = candidate;
        self.target = target;
        self.focused_annotation = Some(id.clone());
        Ok(
            serde_json::json!({"patch":patch, "region":id, "block":format!("plan:annotation:{id}"), "row":1}),
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

    /// Expand the selected comment or compact it and discard an abandoned empty draft.
    fn focus_annotation(&mut self, input: DocumentInput) -> Result<serde_json::Value> {
        ensure!(
            !self.source.historical,
            "historical plan revisions are read-only"
        );
        let block_id = input.block.clone();
        self.validate_input(input)?;
        let focused = self
            .annotation
            .annotation()
            .iter()
            .find(|annotation| block_id.0 == format!("plan:annotation:{}", annotation.id))
            .map(|annotation| annotation.id.clone());
        if focused == self.focused_annotation {
            return Ok(serde_json::json!({"patch":null,"focused":focused}));
        }
        self.retain_annotation_revision();
        let mut annotation = self.annotation.annotation().to_vec();
        annotation.retain(|annotation| {
            Some(&annotation.id) == focused.as_ref() || !annotation.source.body.trim().is_empty()
        });
        let (block, target) = super::review_projection::project(
            &self.source,
            &self.width,
            &annotation,
            &self.annotation_revision,
            focused.as_deref(),
        )?;
        let mut candidate = self.document.clone();
        let patch = candidate.edit(0..candidate.block_count(), block)?;
        self.annotation.replace(annotation)?;
        self.document = candidate;
        self.target = target;
        self.focused_annotation = focused.clone();
        Ok(serde_json::json!({"patch":patch,"focused":focused}))
    }

    /// Remove the selected annotation from durable review state and its projection.
    fn delete_annotation(&mut self, input: DocumentInput) -> Result<serde_json::Value> {
        ensure!(
            !self.source.historical,
            "historical plan revisions are read-only"
        );
        let block_id = input.block.clone();
        self.validate_input(input)?;
        let id = self
            .annotation
            .annotation()
            .iter()
            .find(|annotation| block_id.0 == format!("plan:annotation:{}", annotation.id))
            .map(|annotation| annotation.id.clone())
            .context("selected plan row is not a comment")?;
        self.retain_annotation_revision();
        let annotation = self
            .annotation
            .annotation()
            .iter()
            .filter(|annotation| annotation.id != id)
            .cloned()
            .collect::<Vec<_>>();
        self.annotation_revision.remove(&id);
        let (block, target) = super::review_projection::project(
            &self.source,
            &self.width,
            &annotation,
            &self.annotation_revision,
            None,
        )?;
        let mut candidate = self.document.clone();
        let patch = candidate.edit(0..candidate.block_count(), block)?;
        self.annotation.replace(annotation)?;
        self.document = candidate;
        self.target = target;
        self.focused_annotation = None;
        Ok(serde_json::json!({"patch":patch,"focused":null}))
    }

    fn edit(&mut self, edit: LocalEdit) -> Result<LocalEditResult> {
        ensure!(
            !self.source.historical,
            "historical plan revisions are read-only"
        );
        ensure!(
            edit.text.byte_count() <= 65536 && edit.text.row_count() <= 4096,
            "plan annotation edit exceeds 64 KiB or 4096 rows"
        );
        let mut annotation = self.annotation.annotation().to_vec();
        let Some(changed) = annotation
            .iter_mut()
            .find(|annotation| annotation.id == edit.region.0)
        else {
            return Ok(LocalEditResult::UnknownRegion);
        };
        changed.source.body = edit.text.wire_rows().join("\n");
        match self.document.prepare_local_edit(edit)? {
            LocalEditPreparation::Rejected(result) => Ok(result),
            LocalEditPreparation::Prepared(prepared) => {
                self.annotation.replace(annotation)?;
                Ok(prepared.commit())
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
        let annotation = ReviewAnnotationStore::open(annotation_path, source.saved_digest.clone())?;
        let (block, target) = super::review_projection::project(
            &source,
            &width,
            annotation.annotation(),
            &HashMap::new(),
            focused_annotation.as_deref(),
        )?;
        let document = BufferDocument::new(id.clone(), block)?;
        let mut view_width = DocumentViews::default();
        view_width.open(view.clone(), width.clone())?;
        Ok(Self {
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

    fn validate_input(&mut self, input: DocumentInput) -> Result<Option<TargetId>> {
        input.validate()?;
        ensure!(
            input.document == self.id && input.revision == self.document.revision(),
            "plan review input revision changed"
        );
        let sequence = self
            .view
            .get_mut(&input.view)
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
        *sequence = input.sequence;
        Ok(target)
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
                self.annotation.annotation(),
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
        Ok(serde_json::json!({"patch":patch}))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::PlanFileStore;
    use forge_buffer::identity::{BlockId, EditSequence, RegionId, RegionRevision};
    use forge_buffer::text::BufferText;

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
        assert_eq!(source.document.overview, "Original overview");
        let mut historical = PlanReviewDocument::new(
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
        assert!(snapshot.block.iter().any(|block| {
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
        assert!(
            historical
                .add_annotation(input.clone(), None)
                .unwrap_err()
                .to_string()
                .contains("read-only")
        );
        assert!(historical.delete_annotation(input.clone()).is_err());
        assert!(historical.focus_annotation(input.clone()).is_err());
        let review = PlanReviewStore::default();
        let admission = review.admit(historical.id.clone()).unwrap();
        review.insert(historical, admission).unwrap();
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

    #[test]
    fn signature_type_jump_resolves_cursor_entity_and_preserves_flow_navigation() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let mut canonical = crate::plan::document::test_fixture("plan", "Initial");
        let mut summary = canonical.entity_changes[0].clone();
        summary.name = "DiagnosticSummary".into();
        canonical.entity_changes.push(summary);
        if let crate::plan::document::PlanSubtask::Work(subtask) =
            &mut canonical.tasks[0].files[0].subtasks[0]
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

    #[test]
    fn annotation_edit_publishes_only_after_durable_save_and_reopens_exact_text() {
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
        let path = source.path.with_extension("json");
        let original = std::fs::read(&path).unwrap();
        let mut document = PlanReviewDocument::new(
            DocumentId("review".into()),
            ViewId("view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let snapshot = document.snapshot();
        let target = snapshot.block[0].metadata.target[0].clone();
        let created = document
            .add_annotation(
                DocumentInput {
                    document: snapshot.document,
                    revision: snapshot.revision,
                    view: ViewId("view".into()),
                    sequence: InputSequence(1),
                    action: "comment".into(),
                    block: snapshot.block[0].id.clone(),
                    position: target.range.start,
                    target: Some(target.id),
                },
                None,
            )
            .unwrap();
        let region = RegionId(created["region"].as_str().unwrap().into());
        let edit = LocalEdit {
            document: DocumentId("review".into()),
            region: region.clone(),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: BufferText::from_rows(["literal **comment**", ""]).unwrap(),
        };
        assert!(matches!(
            document.edit(edit.clone()).unwrap(),
            LocalEditResult::Accepted { .. }
        ));
        let accepted = document.document.revision();
        std::fs::write(&path, b"changed").unwrap();
        let mut failed = edit;
        failed.base = RegionRevision(1);
        failed.sequence = EditSequence(2);
        failed.text = BufferText::from_rows(["not durable"]).unwrap();
        assert!(document.edit(failed).is_err());
        assert_eq!(document.document.revision(), accepted);
        std::fs::write(path, original).unwrap();
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let mut reopened = PlanReviewDocument::new(
            DocumentId("reopened".into()),
            ViewId("other".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        let block = BlockId(format!("plan:annotation:{}", region.0));
        assert!(
            reopened
                .document
                .block(&block)
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .contains("╭─")
        );
        assert!(
            reopened
                .document
                .block(&block)
                .unwrap()
                .metadata
                .editable_region
                .is_empty()
        );
        reopened
            .focus_annotation(DocumentInput {
                document: reopened.id.clone(),
                revision: reopened.document.revision(),
                view: ViewId("other".into()),
                sequence: InputSequence(1),
                action: "focus_annotation".into(),
                block: block.clone(),
                position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                target: None,
            })
            .unwrap();
        assert_eq!(
            &reopened.document.block(&block).unwrap().text.wire_rows()[1..3],
            &["literal **comment**", ""]
        );
        reopened
            .delete_annotation(DocumentInput {
                document: reopened.id.clone(),
                revision: reopened.document.revision(),
                view: ViewId("other".into()),
                sequence: InputSequence(2),
                action: "delete".into(),
                block: block.clone(),
                position: forge_buffer::block::TextPosition { row: 1, column: 0 },
                target: None,
            })
            .unwrap();
        assert!(reopened.document.block(&block).is_none());
        let source = store
            .capture_review_source("session", "plan", 1, &checksum)
            .unwrap();
        let deleted = PlanReviewDocument::new(
            DocumentId("deleted".into()),
            ViewId("deleted-view".into()),
            source,
            WidthProfile::default(),
            None,
        )
        .unwrap();
        assert!(deleted.document.block(&block).is_none());
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
}
