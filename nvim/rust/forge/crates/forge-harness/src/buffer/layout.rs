use anyhow::{Result, ensure};
use forge_buffer::block::{BufferBlock, TextPosition, TextRange};
use forge_buffer::text::BufferText;

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
    ensure!(
        block.text.byte_count() + padding * block.text.row_count() <= 32 * 1024 * 1024,
        "materialized Markdown exceeds native capacity"
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
