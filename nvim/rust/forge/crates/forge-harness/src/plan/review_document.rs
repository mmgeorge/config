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
        Ok(
            serde_json::json!({"plan_id":document.source.document.plan_id,
            "digest":super::digest(&serde_json::to_vec(&document.source.document)?),
            "saved_source_digest":document.source.saved_digest,
            "annotations":document.annotation.annotation().iter().filter(|annotation| !annotation.source.body.trim().is_empty())
                .map(|annotation| &annotation.source).collect::<Vec<_>>() }),
        )
    }

    pub(crate) fn add_annotation(&self, input: DocumentInput) -> Result<serde_json::Value> {
        let mut store = self
            .document
            .lock()
            .map_err(|_| anyhow::anyhow!("plan review store lock poisoned"))?;
        let (document, admission) = store
            .get_mut(&input.document)
            .context("plan review document is closed")?;
        admission.check()?;
        document.add_annotation(input)
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
        let entity_name = match &anchor.target {
            PlanReviewTarget::Entity { name } | PlanReviewTarget::FileTreeEntity { name, .. } => {
                Some(name.as_str())
            }
            PlanReviewTarget::EntityMember { entity, .. }
            | PlanReviewTarget::EnumVariant { entity, .. }
            | PlanReviewTarget::EnumVariantField { entity, .. } => Some(entity.as_str()),
            PlanReviewTarget::FlowStep { target_name, .. }
            | PlanReviewTarget::FlowEdge { target_name, .. } => Some(target_name.as_str()),
            _ => None,
        };
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
            if let Some(callable) = callable {
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
                    jump = Some(forge_buffer::block::BlockAnchor {
                        block: block.id,
                        position: target.range.start,
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

    fn add_annotation(&mut self, input: DocumentInput) -> Result<serde_json::Value> {
        let anchor = self.action(input)?;
        let mut annotation = self.annotation.annotation().to_vec();
        annotation.retain(|annotation| !annotation.source.body.trim().is_empty());
        let id = uuid::Uuid::new_v4().to_string();
        annotation.push(ReviewAnnotation {
            id: id.clone(),
            source: super::PlanAnnotationInput {
                start_line: anchor.line,
                end_line: anchor.line,
                body: String::new(),
            },
        });
        let revision = self
            .document
            .snapshot()
            .block
            .iter()
            .flat_map(|block| &block.metadata.editable_region)
            .map(|region| (region.id.0.clone(), (region.revision, region.sequence)))
            .collect();
        let (block, target) =
            super::review_projection::project(&self.source, &self.width, &annotation, &revision)?;
        let mut candidate = self.document.clone();
        let patch = candidate.edit(0..candidate.block_count(), block)?;
        self.annotation.replace(annotation)?;
        self.document = candidate;
        self.target = target;
        Ok(
            serde_json::json!({"patch":patch, "region":id, "block":format!("plan:annotation:{id}"), "row":1}),
        )
    }

    fn edit(&mut self, edit: LocalEdit) -> Result<LocalEditResult> {
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
    ) -> Result<Self> {
        view.validate()?;
        let annotation = ReviewAnnotationStore::open(
            source.path.with_extension("json"),
            source.saved_digest.clone(),
        )?;
        let (block, target) = super::review_projection::project(
            &source,
            &width,
            annotation.annotation(),
            &HashMap::new(),
        )?;
        let document = BufferDocument::new(id.clone(), block)?;
        let mut view_width = DocumentViews::default();
        view_width.open(view.clone(), width.clone())?;
        Ok(Self {
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
        )
        .unwrap();
        let snapshot = document.snapshot();
        let target = snapshot.block[0].metadata.target[0].clone();
        let created = document
            .add_annotation(DocumentInput {
                document: snapshot.document,
                revision: snapshot.revision,
                view: ViewId("view".into()),
                sequence: InputSequence(1),
                action: "comment".into(),
                block: snapshot.block[0].id.clone(),
                position: target.range.start,
                target: Some(target.id),
            })
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
        let reopened = PlanReviewDocument::new(
            DocumentId("reopened".into()),
            ViewId("other".into()),
            source,
            WidthProfile::default(),
        )
        .unwrap();
        assert_eq!(
            reopened
                .document
                .block(&BlockId(format!("plan:annotation:{}", region.0)))
                .unwrap()
                .text
                .wire_rows(),
            ["Comment", "literal **comment**", ""]
        );
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
