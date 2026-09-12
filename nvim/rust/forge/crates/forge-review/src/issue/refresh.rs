use super::*;
use crate::{edit::PreparedFieldMerge, issue_presentation::IssueProjection};
use forge_buffer::{
    document::PreparedDocumentEdit, identity::DocumentRevision, sequence::SequenceEdit,
};

pub(crate) struct RefreshCapture {
    pub revision: DocumentRevision,
    pub fields: Vec<BufferBlock>,
    pub width: WidthProfile,
}
pub(crate) struct PreparedIssueRefresh<'document> {
    fields: PreparedFieldMerge<'document>,
    buffer: Option<PreparedDocumentEdit<'document>>,
    detail: &'document mut IssueDetail,
    target: &'document mut std::collections::HashMap<forge_buffer::identity::TargetId, String>,
    fresh_required: &'document mut bool,
    source_generation: &'document mut u64,
    next_source_generation: u64,
    incoming: IssueDetail,
    incoming_target: std::collections::HashMap<forge_buffer::identity::TargetId, String>,
    payload: serde_json::Value,
}
impl PreparedIssueRefresh<'_> {
    pub fn take_draft(&mut self) -> serde_json::Value {
        std::mem::take(&mut self.payload)
    }
    pub fn commit(self) -> Option<BufferPatch> {
        self.fields.commit();
        let patch = self.buffer.map(PreparedDocumentEdit::commit);
        *self.detail = self.incoming;
        *self.target = self.incoming_target;
        *self.fresh_required = false;
        *self.source_generation = self.next_source_generation;
        patch
    }
}
impl IssueDocument {
    pub(crate) fn capture_refresh(&mut self, detail: &IssueDetail) -> Result<RefreshCapture> {
        validate_detail(&self.repository, &self.node_id, detail)?;
        ensure!(detail.node_id == self.node_id, "issue refresh node changed");
        let revision = self.buffer.revision();
        let merged = self.edits.prepare_merge(observed(detail))?;
        let fields = ISSUE_FIELD
            .into_iter()
            .map(|name| {
                let region = RegionId(name.into());
                field_snapshot_block(region.clone(), merged.snapshot(&region)?)
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(RefreshCapture {
            revision,
            fields,
            width: self.width.clone(),
        })
    }
    pub(crate) fn prepare_refresh(
        &mut self,
        revision: DocumentRevision,
        width: WidthProfile,
        detail: IssueDetail,
        projection: IssueProjection,
    ) -> Result<PreparedIssueRefresh<'_>> {
        ensure!(
            self.buffer.revision() == revision && self.width == width,
            "issue changed during refresh projection"
        );
        let next_source_generation = self
            .source_generation
            .checked_add(1)
            .context("issue source generation exhausted")?;
        let mut draft = self.draft_state()?;
        draft.fresh_required = false;
        let merged = self.edits.prepare_merge(observed(&detail))?;
        for name in ISSUE_FIELD {
            let field = merged.snapshot(&RegionId(name.into()))?;
            let saved = draft
                .field
                .get_mut(name)
                .expect("canonical issue draft field");
            saved.text = field.text.to_owned();
            saved.baseline = field.baseline.to_owned();
            saved.revision = field.revision.0;
            saved.sequence = field.sequence.0;
            saved.remote = field.remote.map(str::to_owned);
            saved.uncertain = false;
            saved.pending = None;
        }
        let payload = serde_json::json!({"repo":self.repository.repository_name(),"number":self.number,"issue_document":draft});
        ensure!(
            serde_json::to_vec(&payload)?.len() <= 8 * 1024 * 1024,
            "issue draft exceeds 8 MiB"
        );
        let edits = refresh_edits(&self.buffer, projection.block)?;
        let replacement = self.buffer.prepare_edits(edits)?;
        Ok(PreparedIssueRefresh {
            fields: merged,
            buffer: replacement,
            detail: &mut self.detail,
            target: &mut self.target,
            fresh_required: &mut self.fresh_required,
            source_generation: &mut self.source_generation,
            next_source_generation,
            incoming: detail,
            incoming_target: projection.target,
            payload,
        })
    }
}
fn observed(detail: &IssueDetail) -> Vec<(RegionId, String)> {
    vec![
        (RegionId("title".into()), detail.title.clone()),
        (
            RegionId("assignees".into()),
            detail
                .assignees
                .iter()
                .map(|login| format!("@{login}"))
                .collect::<Vec<_>>()
                .join(" "),
        ),
        (RegionId("body".into()), detail.body.clone()),
    ]
}

fn refresh_edits(buffer: &BufferDocument, blocks: Vec<BufferBlock>) -> Result<Vec<SequenceEdit>> {
    let mut incoming = blocks.into_iter();
    let mut cursor = 0;
    let mut edits = Vec::new();
    for name in ISSUE_FIELD {
        let id = BlockId(format!("region:{name}"));
        let position = buffer
            .block_index(&id)
            .context("issue refresh field missing")?;
        let mut generated = Vec::new();
        let field = loop {
            let block = incoming.next().context("issue projection field missing")?;
            if block.id == id {
                break block;
            }
            generated.push(block);
        };
        if cursor != position || !generated.is_empty() {
            edits.push(SequenceEdit {
                range: cursor..position,
                block: generated,
            });
        }
        if buffer.block(&id) != Some(&field) {
            edits.push(SequenceEdit {
                range: position..position + 1,
                block: vec![field],
            });
        }
        cursor = position + 1;
    }
    let tail: Vec<_> = incoming.collect();
    if cursor != buffer.block_count() || !tail.is_empty() {
        edits.push(SequenceEdit {
            range: cursor..buffer.block_count(),
            block: tail,
        });
    }
    Ok(edits)
}
