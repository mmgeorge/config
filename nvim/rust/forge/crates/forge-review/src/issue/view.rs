use super::{ISSUE_FIELD, IssueDocument, field_block};
use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::TextRange,
    identity::{InputSequence, ViewId},
    input::DocumentInput,
    patch::BufferPatch,
    sequence::SequenceEdit,
    view::DocumentViews,
    width::WidthProfile,
};
use serde::Serialize;

#[derive(Serialize)]
pub struct IssueEffect {
    pub id: String,
    #[serde(flatten)]
    pub input: DocumentInput,
    pub kind: &'static str,
    pub url: String,
}

pub(crate) struct IssueViewCapture {
    views: DocumentViews,
    view: ViewId,
    close: bool,
    source_generation: u64,
    pub width: WidthProfile,
    pub detail: forge_github::model::IssueDetail,
    pub fields: Vec<forge_buffer::block::BufferBlock>,
}

impl IssueDocument {
    pub(crate) fn prepare_view(
        &mut self,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<Option<IssueViewCapture>> {
        let mut candidate = self.views.clone();
        candidate.open(view.clone(), width)?;
        self.capture_view(candidate, view, false)
    }
    pub(crate) fn prepare_close_view(&mut self, view: &ViewId) -> Result<Option<IssueViewCapture>> {
        let mut candidate = self.views.clone();
        candidate.close(view);
        self.capture_view(candidate, view.clone(), true)
    }
    fn capture_view(
        &mut self,
        views: DocumentViews,
        view: ViewId,
        close: bool,
    ) -> Result<Option<IssueViewCapture>> {
        let width = views.profile().cloned();
        if width.as_ref().is_none_or(|width| width == &self.width) {
            self.views = views;
            if close {
                self.input.remove(&view);
            } else {
                self.input.entry(view).or_insert(InputSequence(0));
            }
            return Ok(None);
        }
        let fields = ISSUE_FIELD
            .iter()
            .map(|name| field_block(&self.edits, name))
            .collect::<Result<Vec<_>>>()?;
        Ok(Some(IssueViewCapture {
            views,
            view,
            close,
            width: width.expect("changed profile"),
            source_generation: self.source_generation,
            detail: self.detail.clone(),
            fields,
        }))
    }
    pub(crate) fn adopt_view(
        &mut self,
        capture: IssueViewCapture,
        projection: crate::issue_presentation::IssueProjection,
    ) -> Result<Option<BufferPatch>> {
        ensure!(
            self.source_generation == capture.source_generation,
            "issue source changed during view analysis"
        );
        let mut changes = Vec::new();
        for block in projection.block {
            let current = self
                .buffer
                .block(&block.id)
                .context("issue reflow changed block identity")?;
            if !current.metadata.editable_region.is_empty() {
                ensure!(
                    !block.metadata.editable_region.is_empty(),
                    "issue reflow removed field identity"
                );
                continue;
            }
            ensure!(
                block.metadata.editable_region.is_empty(),
                "issue reflow introduced editable field"
            );
            if current != &block {
                let index = self
                    .buffer
                    .block_index(&block.id)
                    .context("issue reflow omitted block position")?;
                changes.push(SequenceEdit {
                    range: index..index + 1,
                    block: vec![block],
                });
            }
        }
        let patch = if changes.is_empty() {
            None
        } else {
            self.buffer.edit_many(changes)?
        };
        self.target = projection.target;
        self.width = capture.width;
        self.views = capture.views;
        if capture.close {
            self.input.remove(&capture.view);
        } else {
            self.input.entry(capture.view).or_insert(InputSequence(0));
        }
        Ok(patch)
    }
    pub fn act(&mut self, input: DocumentInput) -> Result<IssueEffect> {
        input.validate()?;
        ensure!(
            &input.document == self.edits.document_id() && input.revision == self.buffer.revision(),
            "stale issue document input"
        );
        let previous = self.input.get(&input.view).context("unknown issue view")?;
        ensure!(input.sequence > *previous, "replayed issue input");
        ensure!(input.action == "browse", "unknown issue action");
        let block = self
            .buffer
            .block(&input.block)
            .context("unknown issue block")?;
        TextRange {
            start: input.position,
            end: input.position,
        }
        .validate(&block.text)?;
        let url = if let Some(target) = &input.target {
            ensure!(
                block.target_at(input.position) == Some(target),
                "issue target differs from captured position"
            );
            self.target
                .get(target)
                .context("unknown issue source target")?
                .clone()
        } else {
            self.detail.url.clone()
        };
        ensure!(
            url.starts_with("https://") || url.starts_with("http://"),
            "issue source is not a browser URL"
        );
        ensure!(
            !url.chars().any(char::is_control),
            "issue source URL contains control characters"
        );
        self.input.insert(input.view.clone(), input.sequence);
        Ok(IssueEffect {
            id: format!("issue:browse:{}", input.sequence.0),
            input,
            kind: "browser",
            url,
        })
    }
}
