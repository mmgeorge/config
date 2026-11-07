use anyhow::{Context, Result};
use forge_buffer::block::{BlockAnchor, BufferBlock, FoldRange, TextPosition, TextRange};
use forge_buffer::node::{NodeDisplay, NodeKind, NodeLifecycle};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

/// Marks long prompts and final responses with the source boundary after three display rows.
pub(super) fn prepare(block: &mut BufferBlock, width: &WidthProfile) -> Result<()> {
    let Some(node) = &block.metadata.node else {
        return Ok(());
    };
    if node.kind != NodeKind::Message
        || (node.default_display != NodeDisplay::Preview && node.lifecycle != NodeLifecycle::Live)
    {
        return Ok(());
    }
    let identity = node.id.clone();
    let closed = node.default_display == NodeDisplay::Preview;
    block.metadata.fold.retain(|fold| fold.id != identity);
    let mut remaining = 3;
    let mut boundary = None;
    for (row, text) in block.text.rows().enumerate() {
        if remaining == 0 {
            boundary = Some(TextPosition { row, column: 0 });
            break;
        }
        let skip = block
            .metadata
            .conceal
            .iter()
            .filter(|conceal| {
                conceal.range.start == TextPosition { row, column: 0 }
                    && conceal.range.end.row == row
            })
            .map(|conceal| conceal.range.end.column)
            .max()
            .unwrap_or(0);
        let text = &text[skip..];
        let indentation = width.cells(&text[..text.len() - text.trim_start().len()], 0)?;
        let (retained, rows) = width.wrapped_prefix(
            text,
            remaining,
            indentation.min(width.columns.saturating_sub(20)),
        )?;
        remaining -= rows;
        if retained < text.len() {
            boundary = Some(TextPosition {
                row,
                column: skip + retained,
            });
            break;
        }
    }
    if let Some(start) = boundary {
        block.metadata.fold.push(FoldRange {
            id: identity,
            start,
            end: BlockAnchor {
                block: block.id.clone(),
                position: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            closed,
            heading_start: Some(BlockAnchor {
                block: block.id.clone(),
                position: TextPosition { row: 0, column: 0 },
            }),
            collapsed_suffix: None,
            collapse_children: false,
            expand_children: false,
        });
        super::transcript::TranscriptRenderer::fold_marker(block);
    }
    Ok(())
}

/// Collapses a response after it leaves the retained recent-response window.
pub(super) fn settle(block: &mut BufferBlock) {
    if let Some(node) = &mut block.metadata.node
        && node.kind == NodeKind::Message
        && node.lifecycle == NodeLifecycle::Live
    {
        node.lifecycle = NodeLifecycle::Settled;
        if !block.metadata.fold.is_empty() {
            node.default_display = NodeDisplay::Preview;
            node.resolve(None);
            for fold in &mut block.metadata.fold {
                fold.closed = true;
            }
        }
    }
}

/// Keeps full source in the transcript while projecting a message or event preview.
pub(super) fn project(
    source: &BufferBlock,
    node: &forge_buffer::node::NodeState,
    expanded: Option<bool>,
) -> Result<BufferBlock> {
    let mut block = source.clone();
    let mut node = node.clone();
    node.resolve(expanded);
    let preview = node.display != NodeDisplay::Full;
    block.metadata.node = Some(node.clone());
    let Some(fold) = block.metadata.fold.first().cloned() else {
        return Ok(block);
    };
    if !preview {
        for fold in &mut block.metadata.fold {
            fold.closed = false;
        }
        return Ok(block);
    }
    let end = fold.start;
    let mut rows = source.text.slice(0..end.row)?;
    if end.column > 0 {
        rows.push(
            source
                .text
                .row(end.row)
                .and_then(|row| row.get(..end.column))
                .context("message preview boundary is outside its source")?,
        );
    }
    block.text = BufferText::from_rows(rows)?;
    let clip = |range: &mut TextRange| {
        range.end = range.end.min(end);
        range.start < range.end
    };
    for decorations in [
        &mut block.metadata.decoration,
        &mut block.metadata.visible_decoration,
        &mut block.metadata.source_highlight,
    ] {
        decorations.retain_mut(|decoration| clip(&mut decoration.range));
    }
    block
        .metadata
        .target
        .retain_mut(|target| clip(&mut target.range));
    block
        .metadata
        .conceal
        .retain_mut(|conceal| clip(&mut conceal.range));
    block
        .metadata
        .source_overlay
        .retain(|overlay| overlay.range.end <= end);
    block.metadata.gutter.retain(|gutter| gutter.position < end);
    block.metadata.fold[0].start = TextPosition { row: 0, column: 0 };
    block.metadata.fold[0].end.position = TextPosition {
        row: block.text.row_count(),
        column: 0,
    };
    block.metadata.fold[0].closed = true;
    block.metadata.fold[0].heading_start = None;
    node.loaded_rows = block.text.row_count();
    node.loaded_bytes = block.text.byte_count();
    node.more = true;
    block.metadata.node = Some(node);
    Ok(block)
}
