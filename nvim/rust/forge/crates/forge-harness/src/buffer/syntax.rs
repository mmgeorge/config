use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::{Result, ensure};
use forge_buffer::block::{BlockMetadata, BufferBlock};
use forge_buffer::identity::{BlockId, TargetId};
use forge_buffer::markdown::MarkdownCode;
use forge_buffer::text::BufferText;
use forge_diff::patch::{PatchHunk, UnifiedPatch};
use forge_diff::source::{Representation, SourceVersion};
use forge_diff::syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest};
use forge_diff::workers::WorkPriority;

/// Captures one saved diff independently of the mutable presentation and its lock.
pub(crate) struct SavedDiffSyntax {
    pub target: TargetId,
    pub text: String,
}

/// Captured syntax work shares one asynchronous service admission path.
pub(crate) enum TranscriptSyntax {
    Diff(SavedDiffSyntax),
    Markdown(MarkdownSyntax),
}

impl TranscriptSyntax {
    /// Identify the source whose completion is retained by the presentation.
    pub fn target(&self) -> &TargetId {
        match self {
            Self::Diff(job) => &job.target,
            Self::Markdown(job) => &job.target,
        }
    }

    /// Analyze immutable source outside the presentation lock.
    pub async fn analyze(&self, engine: &Arc<SyntaxEngine>) -> Result<Vec<BufferBlock>> {
        match self {
            Self::Diff(job) => job.analyze(engine).await,
            Self::Markdown(job) => job.analyze(engine).await,
        }
    }
}

/// Retain literal fenced source and its exact rendered positions for stale-result checks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MarkdownSyntax {
    pub target: TargetId,
    pub block: BlockId,
    pub text: BufferText,
    pub code: Vec<MarkdownCode>,
}

impl MarkdownSyntax {
    /// Include duplicated source and coordinate maps in presentation admission.
    pub fn retained_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
            + self.target.0.capacity() * 2
            + self.block.0.capacity()
            + self.text.allocated_bytes()
            + self.code.capacity() * std::mem::size_of::<MarkdownCode>()
            + self
                .code
                .iter()
                .map(MarkdownCode::retained_bytes)
                .sum::<usize>()
    }

    /// Map code-language captures onto literal rows without altering displayed Markdown.
    pub async fn analyze(&self, engine: &Arc<SyntaxEngine>) -> Result<Vec<BufferBlock>> {
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut block = BufferBlock {
            id: self.block.clone(),
            text: self.text.clone(),
            metadata: BlockMetadata::default(),
        };
        for code in &self.code {
            let Some(language) = SyntaxLanguage::from_name(&code.language) else {
                continue;
            };
            let source = code
                .row
                .iter()
                .map(|row| row.text.as_str())
                .collect::<Vec<_>>()
                .join("\n");
            let syntax = engine
                .analyze(SyntaxRequest {
                    source: SourceVersion::new(source.into_bytes(), Representation::Raw)?,
                    language,
                    priority: WorkPriority::Visible,
                    deadline: Some(deadline),
                })
                .await
                .map_err(|error| {
                    anyhow::anyhow!("Markdown code syntax analysis failed: {error:?}")
                })?;
            for (index, row) in code.row.iter().enumerate() {
                let first = block.metadata.visible_decoration.len();
                forge_diff::projection::append_syntax_row(
                    &mut block.metadata,
                    &syntax,
                    index,
                    row.position.row,
                    &row.text,
                )?;
                for span in &mut block.metadata.visible_decoration[first..] {
                    span.range.start.column += row.position.column;
                    span.range.end.column += row.position.column;
                    // Preserve query precedence above the 110-priority Markdown code style.
                    span.priority = span.priority.saturating_add(20);
                }
            }
            ensure!(
                block.retained_bytes() <= 16 * 1024 * 1024,
                "Markdown code syntax exceeds 16 MiB"
            );
        }
        Ok(vec![block])
    }
}

impl SavedDiffSyntax {
    /// Parse only included source fragments, then project captures into unchanged diff rows.
    pub async fn analyze(&self, engine: &Arc<SyntaxEngine>) -> Result<Vec<BufferBlock>> {
        let patch = UnifiedPatch::parse(&self.text)?;
        let deadline = Instant::now() + Duration::from_secs(10);
        let mut block = Vec::new();
        let mut retained = 0;
        for (file_index, file) in patch.file.iter().enumerate() {
            let Some(language) = file
                .new_path
                .as_deref()
                .or(file.old_path.as_deref())
                .and_then(SyntaxLanguage::for_path)
            else {
                continue;
            };
            for (hunk_index, hunk) in file.hunk.iter().enumerate() {
                let old_request = SyntaxRequest {
                    source: fragment(hunk, true)?,
                    language,
                    priority: WorkPriority::Visible,
                    deadline: Some(deadline),
                };
                let new_request = SyntaxRequest {
                    source: fragment(hunk, false)?,
                    language,
                    priority: WorkPriority::Visible,
                    deadline: Some(deadline),
                };
                let (old, new) =
                    tokio::try_join!(engine.analyze(old_request), engine.analyze(new_request))
                        .map_err(|error| {
                            anyhow::anyhow!("Saved diff syntax analysis failed: {error:?}")
                        })?;
                let hunk_id = format!("{}:file:{file_index}:hunk:{hunk_index}", self.target.0);
                for (batch, rows) in hunk.row.chunks(128).enumerate() {
                    let mut result = BufferBlock {
                        id: BlockId(format!("{hunk_id}:rows:{batch}")),
                        text: BufferText::from_rows(
                            rows.iter().map(|row| row.text.trim_end_matches('\r')),
                        )?,
                        metadata: BlockMetadata::default(),
                    };
                    for (index, row) in rows.iter().enumerate() {
                        let (syntax, source_row) = if let Some(line) = row.new_line {
                            (&new, line - hunk.new_lines.start)
                        } else {
                            (
                                &old,
                                row.old_line.expect("removed source line") - hunk.old_lines.start,
                            )
                        };
                        forge_diff::projection::append_syntax_row(
                            &mut result.metadata,
                            syntax,
                            source_row,
                            index,
                            row.text.trim_end_matches('\r'),
                        )?;
                    }
                    retained += result.retained_bytes();
                    ensure!(
                        retained <= 16 * 1024 * 1024,
                        "saved diff syntax exceeds 16 MiB"
                    );
                    block.push(result);
                }
            }
        }
        Ok(block)
    }
}

/// Reconstruct one hunk side without filling absent file ranges or mixing replacement sides.
fn fragment(hunk: &PatchHunk<'_>, old: bool) -> Result<SourceVersion> {
    let mut text = Vec::new();
    for row in &hunk.row {
        if (old && row.old_line.is_some()) || (!old && row.new_line.is_some()) {
            text.extend_from_slice(row.text.as_bytes());
            if !row.no_newline {
                text.push(b'\n');
            }
        }
    }
    Ok(SourceVersion::new(text, Representation::Raw)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_diff::syntax::SyntaxLimits;
    use forge_diff::workers::{AnalysisPool, PoolLimits};

    #[tokio::test]
    async fn fenced_markdown_syntax_preserves_languages_quotes_and_unicode_coordinates()
    -> Result<()> {
        let rendered = forge_buffer::markdown::MarkdownRenderer::render(
            BlockId("markdown".into()),
            "# Heading\n\n> ```ts\n> const café: string = '東京';\n>\n> console.log(café);\n> ```\n\n```rust\nfn example() {}\n```\n\n```unknown-language\nplain text\n```",
            &forge_buffer::width::WidthProfile {
                columns: 8,
                ..Default::default()
            },
        )?;
        let engine = SyntaxEngine::new(
            Arc::new(AnalysisPool::new(PoolLimits {
                workers: 1,
                jobs: 2,
                input_bytes: 1024 * 1024,
            })),
            SyntaxLimits::default(),
        );
        let job = MarkdownSyntax {
            target: TargetId("syntax".into()),
            block: rendered.block.id,
            text: rendered.block.text,
            code: rendered.code,
        };
        assert_eq!(
            job.code
                .iter()
                .map(|code| code.language.as_str())
                .collect::<Vec<_>>(),
            vec!["ts", "rust", "unknown-language"]
        );
        assert!(job.code[0].row.iter().any(|row| row.text.is_empty()));
        let block = job.analyze(&engine).await?.pop().unwrap();
        assert_eq!(block.text, job.text);
        assert!(
            block
                .metadata
                .visible_decoration
                .iter()
                .any(|span| span.capture.ends_with(".typescript"))
        );
        assert!(
            block
                .metadata
                .visible_decoration
                .iter()
                .any(|span| span.capture.ends_with(".rust"))
        );
        for span in &block.metadata.visible_decoration {
            assert!(
                span.priority > 110,
                "generic Markdown code styling hides syntax"
            );
            let row = block.text.row(span.range.start.row).unwrap();
            assert!(span.range.end.column <= row.len());
            assert!(
                row.is_char_boundary(span.range.start.column)
                    && row.is_char_boundary(span.range.end.column)
            );
            if row.starts_with("│ ") {
                assert!(span.range.start.column >= "│ ".len());
            }
            assert!(!row.contains("Heading") && row != "plain text");
        }
        block.validate()?;
        Ok(())
    }

    #[tokio::test]
    async fn sparse_saved_diff_keeps_side_bytes_and_local_capture_coordinates() -> Result<()> {
        let text = "--- a/example.mjs\n+++ b/example.mjs\n@@ -900001,2 +700001,2 @@\n const shared = 1;\r\n-const old = 2;\r\n\\ No newline at end of file\n+const next = 'value';\r\n\\ No newline at end of file\n";
        let patch = UnifiedPatch::parse(text)?;
        let hunk = &patch.file[0].hunk[0];
        assert_eq!(
            fragment(hunk, true)?.bytes(),
            b"const shared = 1;\r\nconst old = 2;\r"
        );
        assert_eq!(
            fragment(hunk, false)?.bytes(),
            b"const shared = 1;\r\nconst next = 'value';\r"
        );
        let engine = SyntaxEngine::new(
            Arc::new(AnalysisPool::new(PoolLimits {
                workers: 1,
                jobs: 2,
                input_bytes: 1024 * 1024,
            })),
            SyntaxLimits::default(),
        );
        let job = SavedDiffSyntax {
            target: TargetId("diff".into()),
            text: text.into(),
        };
        let blocks = job.analyze(&engine).await?;
        assert_eq!(blocks.len(), 1);
        let block = &blocks[0];
        assert_eq!(block.text.row_count(), 3);
        for row in 0..3 {
            assert!(
                block
                    .metadata
                    .visible_decoration
                    .iter()
                    .any(|span| span.range.start.row == row)
            );
        }
        for span in &block.metadata.visible_decoration {
            assert!(span.range.end.column <= block.text.row(span.range.end.row).unwrap().len());
        }
        assert!(block.retained_bytes() < 16 * 1024);
        Ok(())
    }
}
