use std::collections::HashMap;

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{
        BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, Gutter, TargetRange,
        TextChunk, TextPosition, TextRange,
    },
    identity::{BlockId, FoldId, TargetId},
    text::BufferText,
    width::WidthProfile,
};
use forge_github::model::IssueDetail;

pub struct IssueProjection {
    pub block: Vec<BufferBlock>,
    pub target: HashMap<TargetId, String>,
}

pub fn project(
    detail: &IssueDetail,
    fields: Vec<BufferBlock>,
    width: &WidthProfile,
) -> Result<IssueProjection> {
    width.validate()?;
    ensure!(
        fields.len() == 3,
        "issue projection requires three canonical fields"
    );
    let mut field: HashMap<_, _> = fields
        .into_iter()
        .map(|block| (block.id.clone(), block))
        .collect();
    let mut block = Vec::new();
    let mut target = HashMap::new();
    let mut title = field
        .remove(&BlockId("region:title".into()))
        .context("missing issue title")?;
    field_label(&mut title, "Title:  ");
    source_target(&mut title, "issue:browse", &detail.url, &mut target);
    block.push(title);
    let state = detail
        .state
        .split('_')
        .map(|word| {
            let mut letters = word.chars();
            letters.next().map_or_else(String::new, |first| {
                first.to_uppercase().collect::<String>() + &letters.as_str().to_lowercase()
            })
        })
        .collect::<Vec<_>>()
        .join(" ");
    let activity =
        crate::presentation_time::latest_activity(
            [&detail.updated_at, &detail.created_at]
                .into_iter()
                .map(String::as_str)
                .chain(detail.comments.iter().flat_map(|comment| {
                    [comment.updated_at.as_str(), comment.created_at.as_str()]
                })),
        );
    let release = if detail.milestone.is_empty() {
        String::new()
    } else {
        format!("◆ {}", detail.milestone)
    };
    let mut metadata = readonly("issue:metadata", "")?;
    let mut rows = Vec::new();
    for (name, value, capture) in [
        ("Author", detail.author.as_str(), "Normal"),
        (
            "State",
            state.as_str(),
            if state == "Open" {
                "ForgeStatusOpen"
            } else if state == "Closed" {
                "ForgeStatusClosed"
            } else {
                "Normal"
            },
        ),
        ("Activity", activity.as_str(), "ForgeStatusDate"),
        ("Release", release.as_str(), "Normal"),
        ("Projects", &detail.projects.join(", "), "Normal"),
        ("Subscription", detail.subscription.as_str(), "Normal"),
        ("Labels", &detail.labels.join(", "), "Normal"),
    ] {
        if value.is_empty() {
            continue;
        }
        let row = rows.len();
        let text = format!("{:<14}{value}", format!("{name}:"));
        decorate(&mut metadata, row, 14, text.len(), capture);
        rows.push(text);
    }
    metadata.text = BufferText::from_rows(rows)?;
    block.push(metadata);
    let mut assignees = field
        .remove(&BlockId("region:assignees".into()))
        .context("missing issue assignees")?;
    field_label(&mut assignees, "Assignees:    ");
    block.push(assignees);
    block.push(readonly("issue:body-gap", "")?);
    block.push(label("issue:body-label", "Description:")?);
    block.push(
        field
            .remove(&BlockId("region:body".into()))
            .context("missing issue body")?,
    );
    block.push(readonly("issue:comments-gap", "")?);
    let mut comments = label(
        "issue:comments-label",
        &format!("Comments ({}):", detail.comments_count),
    )?;
    if !detail.comments.is_empty() {
        comments.metadata.fold.push(FoldRange {
            id: FoldId("issue:comments".into()),
            start: TextPosition { row: 0, column: 0 },
            end: BlockAnchor {
                block: BlockId("issue:comments-end".into()),
                position: TextPosition { row: 0, column: 0 },
            },
            closed: false,
        });
    }
    block.push(comments);
    let mut bytes = block
        .iter()
        .map(|block| block.text.byte_count())
        .sum::<usize>();
    let date: Vec<_> = detail
        .comments
        .iter()
        .map(|comment| {
            crate::presentation_time::latest_activity([
                comment.updated_at.as_str(),
                comment.created_at.as_str(),
            ])
        })
        .collect();
    let author_width = detail
        .comments
        .iter()
        .map(|comment| width.cells(&comment.author, 0))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .max()
        .unwrap_or(0);
    let date_width = date
        .iter()
        .map(|text| width.cells(text, 0))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .max()
        .unwrap_or(0);
    for (index, comment) in detail.comments.iter().enumerate() {
        let heading_id = format!("issue:comment:{index}:label");
        let body_id = BlockId(format!("issue:comment:{index}:body"));
        let displayed_date = if index > 0 && date[index] == date[index - 1] {
            ""
        } else {
            date[index].as_str()
        };
        let prefix = format!(
            "󰅺 {}{} {}{}  ",
            comment.author,
            " ".repeat(author_width.saturating_sub(width.cells(&comment.author, 0)?)),
            displayed_date,
            " ".repeat(date_width.saturating_sub(width.cells(displayed_date, 0)?))
        );
        let preview = comment_preview(&comment.body);
        let available = width.columns.saturating_sub(width.cells(&prefix, 0)?);
        let preview = truncate_comment_preview(&preview, available + 4, width)?;
        let heading_rows = wrap_comment_heading(&format!("{prefix}{preview}"), width)?;
        let mut heading = readonly(&heading_id, &heading_rows.join("\n"))?;
        decorate_rows(&mut heading, "ForgeReviewComment");
        source_target(
            &mut heading,
            &format!("issue:comment:{index}:browse"),
            &comment.url,
            &mut target,
        );
        let mut body = raw_comment_body(body_id.clone(), &comment.body)?;
        decorate_rows(&mut body, "ForgeReviewComment");
        heading.metadata.fold.push(FoldRange {
            id: FoldId(format!("issue:comment:{index}")),
            start: TextPosition {
                row: heading.text.row_count() - 1,
                column: 0,
            },
            end: BlockAnchor {
                block: body_id,
                position: TextPosition {
                    row: body.text.row_count(),
                    column: 0,
                },
            },
            closed: true,
        });
        bytes = bytes
            .checked_add(heading.text.byte_count())
            .and_then(|bytes| bytes.checked_add(body.text.byte_count()))
            .context("issue projection size overflow")?;
        ensure!(bytes <= 8 * 1024 * 1024, "issue projection exceeds 8 MiB");
        block.push(heading);
        block.push(body);
    }
    block.push(readonly("issue:comments-end", "")?);
    ensure!(bytes <= 8 * 1024 * 1024, "issue projection exceeds 8 MiB");
    Ok(IssueProjection { block, target })
}

fn readonly(id: &str, text: &str) -> Result<BufferBlock> {
    Ok(BufferBlock {
        id: BlockId(id.into()),
        text: BufferText::from_rows(text.split('\n'))?,
        metadata: BlockMetadata::default(),
    })
}

fn field_label(block: &mut BufferBlock, label: &str) {
    block.metadata.gutter.push(Gutter {
        position: TextPosition { row: 0, column: 0 },
        chunk: vec![TextChunk {
            text: label.into(),
            capture: "Normal".into(),
        }],
        priority: 110,
    });
}

fn decorate(block: &mut BufferBlock, row: usize, start: usize, end: usize, capture: &str) {
    if start >= end {
        return;
    }
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition { row, column: start },
            end: TextPosition { row, column: end },
        },
        capture: capture.into(),
        priority: 110,
    });
}

fn decorate_rows(block: &mut BufferBlock, capture: &str) {
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
        capture: capture.into(),
        priority: 110,
    });
}

fn raw_comment_body(id: BlockId, source: &str) -> Result<BufferBlock> {
    let normalized = source.replace("\r\n", "\n").replace('\r', "\n");
    Ok(BufferBlock {
        id,
        text: BufferText::from_rows(normalized.split('\n'))?,
        metadata: BlockMetadata::default(),
    })
}

fn truncate_preview(source: &str, columns: usize, width: &WidthProfile) -> Result<String> {
    if width.cells(source, 0)? <= columns {
        return Ok(source.into());
    }
    if columns < 3 {
        return Ok(".".repeat(columns));
    }
    let mut start = 0;
    let mut end = source.len();
    while start < end {
        let middle = source.ceil_char_boundary(start + (end - start).div_ceil(2));
        if width.cells(&source[..middle], 0)? <= columns - 3 {
            start = middle;
        } else {
            end = source.floor_char_boundary(middle - 1);
        }
    }
    let mut result = source[..start].to_owned();
    result.push_str("...");
    Ok(result)
}

fn truncate_comment_preview(source: &str, columns: usize, width: &WidthProfile) -> Result<String> {
    if width.cells(source, 0)? <= columns || columns <= 4 {
        return truncate_preview(source, columns, width);
    }
    let truncated = truncate_preview(source, columns - 1, width)?;
    Ok(format!("{} ...", truncated.trim_end_matches('.')))
}

fn wrap_comment_heading(source: &str, width: &WidthProfile) -> Result<Vec<String>> {
    if width.cells(source, 0)? <= width.columns {
        return Ok(vec![source.into()]);
    }
    let mut split = None;
    let mut previous_split = None;
    for (offset, character) in source.char_indices() {
        if character.is_whitespace() && width.cells(&source[..offset], 0)? <= width.columns {
            previous_split = split;
            split = Some(offset);
        }
    }
    let mut split = split.context("comment heading has no wrap boundary")?;
    if source[split..].trim_start().starts_with("...") {
        split = previous_split.context("comment preview suffix has no word boundary")?;
    }
    Ok(vec![
        source[..split].into(),
        source[split..].trim_start().into(),
    ])
}

fn comment_preview(body: &str) -> String {
    let mut in_fence = false;
    body.replace("\r\n", "\n")
        .replace('\r', "\n")
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.starts_with("```") {
                in_fence = !in_fence;
                return None;
            }
            (!in_fence && !line.is_empty()).then_some(line)
        })
        .collect::<Vec<_>>()
        .join(" ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn label(id: &str, text: &str) -> Result<BufferBlock> {
    let mut block = readonly(id, text)?;
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
        capture: "ForgeStatusHeader".into(),
        priority: 200,
    });
    Ok(block)
}

fn source_target(
    block: &mut BufferBlock,
    id: &str,
    url: &str,
    target: &mut HashMap<TargetId, String>,
) {
    if url.is_empty() {
        return;
    }
    let id = TargetId(id.into());
    target.insert(id.clone(), url.into());
    block.metadata.target.push(TargetRange {
        id,
        range: TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::{document::BufferDocument, identity::DocumentId};
    use forge_github::model::ConversationComment;

    fn issue_with_comment(body: &str) -> IssueDetail {
        IssueDetail {
            kind: "issue".into(),
            node_id: "ISSUE_7".into(),
            repo: "owner/repo".into(),
            number: 7,
            title: "Title".into(),
            body: "raw **editable**\r\n".into(),
            url: "https://enterprise.example/owner/repo/issues/7".into(),
            state: "OPEN".into(),
            author: "author".into(),
            created_at: "created".into(),
            updated_at: "updated".into(),
            labels: vec!["bug".into()],
            assignees: vec!["author".into()],
            milestone: "v1".into(),
            projects: Vec::new(),
            comments_count: 1,
            subscription: String::new(),
            is_draft: false,
            comments: vec![ConversationComment {
                body: body.into(),
                author: "commenter".into(),
                created_at: "timestamp".into(),
                updated_at: String::new(),
                url: "https://enterprise.example/owner/repo/issues/7#issuecomment-8".into(),
            }],
        }
    }

    fn issue_fields(detail: &IssueDetail) -> Vec<BufferBlock> {
        vec![
            readonly("region:title", &detail.title).unwrap(),
            readonly("region:assignees", "@author").unwrap(),
            readonly("region:body", &detail.body).unwrap(),
        ]
    }

    #[test]
    fn expanded_comments_preserve_raw_markdown_crlf_utf8_and_width_independence() {
        let detail = issue_with_comment(
            "First **raw** row\r\n\r\n```rust\r\nlet value = \"界🙂\";\r\n```\rBare CR",
        );
        let fields = issue_fields(&detail);
        let wide = project(&detail, fields.clone(), &WidthProfile::default()).unwrap();
        let mut narrow_width = WidthProfile::default();
        narrow_width.columns = 12;
        let narrow = project(&detail, fields.clone(), &narrow_width).unwrap();
        let expected = [
            "First **raw** row",
            "",
            "```rust",
            "let value = \"界🙂\";",
            "```",
            "Bare CR",
        ];
        for projection in [&wide, &narrow] {
            let body = projection
                .block
                .iter()
                .find(|block| block.id.0 == "issue:comment:0:body")
                .unwrap();
            assert_eq!(body.text.row_count(), expected.len());
            for (row, expected) in expected.iter().enumerate() {
                assert_eq!(body.text.row(row), Some(*expected));
            }
        }
        assert_eq!(
            wide.block
                .iter()
                .find(|block| block.id.0 == "issue:comment:0:body")
                .unwrap()
                .text,
            narrow
                .block
                .iter()
                .find(|block| block.id.0 == "issue:comment:0:body")
                .unwrap()
                .text
        );
    }

    #[test]
    fn comment_rows_use_review_styling_and_retain_only_old_navigation_targets() {
        let mut detail = issue_with_comment("[Source](https://example.test/source)\rsecond row");
        detail.comments.push(detail.comments[0].clone());
        detail.comments_count = 2;
        let fields = issue_fields(&detail);
        let projection = project(&detail, fields.clone(), &WidthProfile::default()).unwrap();
        let fields = vec!["issue:comment:0:label", "issue:comment:0:body"];
        for id in fields {
            let block = projection
                .block
                .iter()
                .find(|block| block.id.0 == id)
                .unwrap();
            assert!(block.metadata.decoration.iter().any(|decoration| {
                decoration.capture == "ForgeReviewComment"
                    && decoration.range.start == TextPosition { row: 0, column: 0 }
                    && decoration.range.end
                        == TextPosition {
                            row: block.text.row_count(),
                            column: 0,
                        }
            }));
        }
        let heading = projection
            .block
            .iter()
            .find(|block| block.id.0 == "issue:comment:0:label")
            .unwrap();
        assert_eq!(heading.metadata.decoration.len(), 1);
        assert_eq!(heading.metadata.decoration[0].capture, "ForgeReviewComment");
        let repeated = projection
            .block
            .iter()
            .find(|block| block.id.0 == "issue:comment:1:label")
            .unwrap()
            .text
            .row(0)
            .unwrap();
        assert!(repeated.starts_with("󰅺 commenter            "));
        assert_eq!(
            projection.target[&TargetId("issue:browse".into())],
            detail.url
        );
        assert_eq!(projection.target.len(), 3);
        assert!(
            projection
                .target
                .values()
                .all(|url| url != "https://example.test/source")
        );
        assert_eq!(
            projection
                .block
                .iter()
                .flat_map(|block| &block.metadata.fold)
                .count(),
            3
        );
        BufferDocument::new(DocumentId("issue-projection".into()), projection.block).unwrap();
    }

    #[test]
    fn comment_preview_bounds_preserve_unicode_and_source_independence() {
        let width = WidthProfile::default();
        assert_eq!(
            comment_preview("Intro\r\n```lua\r\nx()\r\n```\r\nEnd"),
            "Intro End"
        );
        for columns in [1, 2, 3, 4, 12, 24, 80] {
            let preview = truncate_preview(
                "界🙂 first **paragraph** followed by a second paragraph",
                columns,
                &width,
            )
            .unwrap();
            assert!(width.cells(&preview, 0).unwrap() <= columns);
            assert!(!preview.contains('\n'));
        }
    }

    #[test]
    fn closed_long_comment_materializes_a_visible_wrap_before_the_folded_body() {
        let detail = issue_with_comment(
            "The source identity must remain stable. This second paragraph stays behind the fold.",
        );
        let mut width = WidthProfile::default();
        width.columns = 48;
        let projection = project(&detail, issue_fields(&detail), &width).unwrap();
        let heading = projection
            .block
            .iter()
            .find(|block| block.id.0 == "issue:comment:0:label")
            .unwrap();
        assert_eq!(heading.text.row_count(), 2);
        assert!(!heading.text.row(1).unwrap().is_empty());
        assert_eq!(
            heading.metadata.fold[0].start,
            TextPosition { row: 1, column: 0 }
        );
        assert!(heading.metadata.fold[0].closed);
        assert_eq!(
            heading.metadata.target[0].range.end,
            TextPosition { row: 2, column: 0 }
        );

        width.columns = 95;
        let prefix = "󰅺 maintainer 2 days ago  ";
        let preview = "The **source identity** must remain stable. This second paragraph stays behind the comment fold.";
        let available = width.columns - width.cells(prefix, 0).unwrap();
        let truncated = truncate_comment_preview(preview, available + 4, &width).unwrap();
        assert_eq!(
            wrap_comment_heading(&format!("{prefix}{truncated}"), &width).unwrap()[1],
            "stay ..."
        );
    }
}
