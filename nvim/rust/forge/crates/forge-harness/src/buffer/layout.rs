use anyhow::{Context, Result, ensure};
use forge_buffer::block::{BufferBlock, TextPosition, TextRange};
use forge_buffer::text::BufferText;

use crate::exchange::{Exchange, ExchangeNode};
use crate::turn::{MessageDelivery, TurnItem};

use super::question::QuestionHistory;

pub(super) struct ExchangeLayout<'exchange> {
    pub questions: QuestionHistory<'exchange>,
    pub opening: Vec<&'exchange ExchangeNode>,
    pub conclusion: Vec<&'exchange ExchangeNode>,
    pub results: Vec<&'exchange ExchangeNode>,
    pub activity: Vec<&'exchange ExchangeNode>,
    pub continuation: Vec<&'exchange ExchangeNode>,
}

impl<'exchange> ExchangeLayout<'exchange> {
    /// Places review admission first and generated results before the outer response.
    /// Question clarifications retain their branch ownership. Missing turn or message references fail.
    pub fn new(exchange: &'exchange Exchange) -> Result<Self> {
        let questions = QuestionHistory::new(exchange);
        let mut opening = Vec::new();
        let mut results = Vec::new();
        let mut conclusion = Vec::new();
        let mut activity = exchange
            .node_list
            .iter()
            .filter(|node| !questions.nested.contains(node.id()))
            .collect::<Vec<_>>();
        activity.retain(|node| {
            match node {
                ExchangeNode::PlanEvent { event } => match &event.content {
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if event.node_count == 0 && matches!(lifecycle.kind,
                            crate::plan::PlanLifecycleKind::Accepted | crate::plan::PlanLifecycleKind::ChangesRequested) => {
                        opening.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if matches!(lifecycle.kind, crate::plan::PlanLifecycleKind::Created
                            | crate::plan::PlanLifecycleKind::RevisionCreated) => {
                        results.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Execution { event }
                        if matches!(event, crate::plan::PlanExecutionLifecycleEvent::Completed { .. }
                            | crate::plan::PlanExecutionLifecycleEvent::Failed { .. }) => {
                        conclusion.push(*node);
                        false
                    }
                    crate::plan::PlanEventContent::Execution { .. } | crate::plan::PlanEventContent::Resolution { .. } => {
                        results.push(*node);
                        false
                    }
                    _ => true,
                },
                ExchangeNode::ArtifactChange { .. } | ExchangeNode::ImplementationReport { .. } => {
                    results.push(*node);
                    false
                }
                _ => true,
            }
        });
        let mut response_position = activity.len();
        for (position, node) in activity.iter().enumerate() {
            let ExchangeNode::TurnContent {
                turn_id,
                item: TurnItem::Message { id },
                ..
            } = node
            else {
                continue;
            };
            let turn = exchange
                .turn
                .iter()
                .find(|turn| turn.id() == turn_id)
                .context("timeline content references a missing provider turn")?;
            let message = turn
                .messages()
                .iter()
                .find(|message| message.id() == id)
                .context("timeline content references a missing message")?;
            if message.delivery() == MessageDelivery::Final {
                response_position = position;
                break;
            }
        }
        let continuation = activity.split_off(response_position);
        Ok(Self {
            questions,
            opening,
            results,
            conclusion,
            activity,
            continuation,
        })
    }
}

/// Materializes Markdown padding once so native wrapping shares the parser's content origin.
pub(super) fn materialize(block: &mut BufferBlock) -> Result<()> {
    if !block.metadata.markdown {
        return Ok(());
    }
    let Some(layout) = block.metadata.layout.as_mut() else {
        return Ok(());
    };
    let padding = layout.indent - 2;
    if padding == layout.source_indent {
        return Ok(());
    }
    ensure!(
        layout.source_indent == 0,
        "content layout was materialized before resolution completed"
    );
    let prefix = " ".repeat(padding);
    let count = block.text.row_count();
    block.text = BufferText::from_rows(
        block
            .text
            .wire_rows()
            .into_iter()
            .map(|line| format!("{prefix}{line}"))
            .collect::<Vec<_>>(),
    )?;
    layout.source_indent = padding;
    let position = |position: &mut TextPosition| {
        if position.row < count {
            position.column += padding;
        }
    };
    let range = |range: &mut TextRange| {
        position(&mut range.start);
        position(&mut range.end);
    };
    for target in &mut block.metadata.target {
        range(&mut target.range);
    }
    for decoration in block
        .metadata
        .decoration
        .iter_mut()
        .chain(&mut block.metadata.visible_decoration)
        .chain(&mut block.metadata.source_highlight)
    {
        range(&mut decoration.range);
    }
    for conceal in &mut block.metadata.conceal {
        range(&mut conceal.range);
    }
    for overlay in &mut block.metadata.source_overlay {
        range(&mut overlay.range);
    }
    for gutter in &mut block.metadata.gutter {
        position(&mut gutter.position);
    }
    block.validate()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::identity::BlockId;
    use forge_buffer::width::WidthProfile;

    #[test]
    fn nested_markdown_moves_navigation_once_without_changing_its_payload() -> Result<()> {
        let profile = WidthProfile::default();
        let mut rendered = crate::buffer::transcript::TranscriptRenderer::new(&profile)?.response(
            BlockId("nested".into()),
            "[Details](https://example.test)\n\n## Heading",
        )?;
        let original_column = rendered.block.metadata.target[0].range.start.column;
        rendered.block.metadata.layout.as_mut().unwrap().indent = 6;
        materialize(&mut rendered.block)?;
        assert_eq!(
            rendered.block.text.row(0),
            Some("    [Details](https://example.test)")
        );
        assert_eq!(
            rendered.block.metadata.target[0].range.start.column,
            original_column + 4
        );
        assert_eq!(rendered.link[0].destination, "https://example.test");
        let once = rendered.block.clone();
        materialize(&mut rendered.block)?;
        assert_eq!(rendered.block, once);
        Ok(())
    }
}
