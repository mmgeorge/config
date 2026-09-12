use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, TargetRange, TextPosition,
    TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::markdown::MarkdownLink;
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::thread::ReviewThread;

#[derive(Clone, Debug)]
pub(crate) enum ReviewTarget {
    Lifecycle {
        available: Vec<forge_github::pull_request::DesiredPullRequestState>,
    },
    FileDiff {
        path: String,
    },
    WorkspaceFile {
        path: String,
    },
    CommitMessage {
        oid: String,
    },
    CommitDiff {
        oid: String,
        anchor: forge_buffer::identity::BlockId,
    },
    InlineComment {
        anchor: crate::comments::CommentAnchor,
    },
    ThreadComment {
        thread: String,
        comment: String,
        reply: bool,
    },
    ThreadMore {
        thread: String,
        cursor: String,
    },
    Browser {
        url: String,
    },
}

pub(super) struct ThreadProjection {
    pub block: Vec<BufferBlock>,
    pub link: BTreeMap<BlockId, Vec<MarkdownLink>>,
    pub target: BTreeMap<TargetId, ReviewTarget>,
}

pub(super) fn render(thread: &ReviewThread, width: &WidthProfile) -> Result<ThreadProjection> {
    render_range(thread, width, 0, true)
}

pub(super) fn render_range(
    thread: &ReviewThread,
    width: &WidthProfile,
    start: usize,
    complete: bool,
) -> Result<ThreadProjection> {
    let mut projection = ThreadProjection {
        block: Vec::new(),
        link: BTreeMap::new(),
        target: BTreeMap::new(),
    };
    let prefix = identity("thread", &thread.node_id);
    if complete {
        projection.boundary(format!("{prefix}:start"))?;
    }
    for (comment_offset, comment) in thread.comment[start..].iter().enumerate() {
        let prefix = identity(&prefix, &comment.node_id);
        let comment_start = projection.block.len();
        let author = comment.author.as_deref().unwrap_or("Deleted user");
        let is_reply = start + comment_offset > 0;
        let icon = if is_reply { "↳" } else { "󰅺" };
        let action = if is_reply { "replied" } else { "commented" };
        let timestamp = comment
            .updated_at
            .as_deref()
            .or(comment.created_at.as_deref());
        let date = timestamp
            .map(crate::presentation_time::relative)
            .unwrap_or_default();
        let left = format!(
            "{icon} {author} {action}{} ",
            if date.is_empty() {
                String::new()
            } else {
                format!(" {date}")
            }
        );
        let line = comment.line.or(thread.line);
        let right = if is_reply {
            String::new()
        } else {
            format!(
                " L{}",
                line.map_or_else(|| "?".into(), |line| line.to_string())
            )
        };
        projection.comment_rule(format!("{prefix}:author"), &left, &right, width, timestamp)?;
        projection.comment_body(format!("{prefix}:body"), &comment.body)?;
        if comment.viewer_did_author {
            projection.action(
                format!("{prefix}:edit"),
                "Edit comment",
                ReviewTarget::ThreadComment {
                    thread: thread.node_id.clone(),
                    comment: comment.node_id.clone(),
                    reply: false,
                },
            )?;
        }
        if thread.can_reply {
            projection.action(
                format!("{prefix}:reply"),
                "Reply",
                ReviewTarget::ThreadComment {
                    thread: thread.node_id.clone(),
                    comment: comment.node_id.clone(),
                    reply: true,
                },
            )?;
        }
        if !comment.url.is_empty() {
            projection.action(
                format!("{prefix}:browser"),
                "Open comment in browser",
                ReviewTarget::Browser {
                    url: comment.url.clone(),
                },
            )?;
        }
        projection.comment_footer(format!("{prefix}:footer"), width)?;
        let endpoint = projection
            .block
            .last()
            .expect("thread comment has a rendered endpoint")
            .clone();
        attach_fold(
            &mut projection.block[comment_start],
            format!("review:{prefix}"),
            &endpoint,
            true,
        );
    }
    if let Some(cursor) = &thread.next_cursor {
        projection.action(
            format!("{prefix}:more"),
            "Load additional thread comments",
            ReviewTarget::ThreadMore {
                thread: thread.node_id.clone(),
                cursor: cursor.clone(),
            },
        )?;
    }
    if complete {
        projection.boundary(format!("{prefix}:end"))?;
    }
    Ok(projection)
}

fn attach_fold(owner: &mut BufferBlock, identity: String, endpoint: &BufferBlock, closed: bool) {
    owner.metadata.fold.push(FoldRange {
        id: FoldId(identity),
        start: TextPosition { row: 0, column: 0 },
        end: BlockAnchor {
            block: endpoint.id.clone(),
            position: TextPosition {
                row: endpoint.text.row_count(),
                column: 0,
            },
        },
        closed,
    });
}

impl crate::review::ReviewDocument {
    pub(super) fn project_thread(
        &mut self,
        thread: &str,
        continuation: bool,
        projected: ThreadProjection,
    ) -> Result<Option<forge_buffer::patch::BufferPatch>> {
        let Some(projection) = self.projection.as_mut() else {
            return Ok(None);
        };
        let prefix = identity("thread", thread);
        let range = if continuation {
            let index = projection
                .block_index(&BlockId(format!("{prefix}:more")))
                .context("thread continuation marker is no longer presented")?;
            index..index + 1
        } else {
            let start = projection
                .block_index(&BlockId(format!("{prefix}:start")))
                .context("thread presentation start is missing")?;
            let end = projection
                .block_index(&BlockId(format!("{prefix}:end")))
                .context("thread presentation end is missing")?;
            ensure!(start <= end, "thread presentation anchors are reversed");
            start..end + 1
        };
        let mut removed_bytes = 0;
        let mut removed_rows = 0;
        let mut removed_block = Vec::new();
        let mut removed_target = std::collections::BTreeSet::new();
        for block in projection.blocks(projection.revision(), range.clone())? {
            removed_bytes += block.text.byte_count();
            removed_rows += block.text.row_count();
            removed_block.push(block.id.clone());
            removed_target.extend(block.metadata.target.iter().map(|target| target.id.clone()));
        }
        let added_bytes: usize = projected
            .block
            .iter()
            .map(|block| block.text.byte_count())
            .sum();
        let added_rows: usize = projected
            .block
            .iter()
            .map(|block| block.text.row_count())
            .sum();
        ensure!(
            projection.text_bytes() - removed_bytes + added_bytes <= 16 * 1024 * 1024,
            "review presentation byte limit exceeded"
        );
        ensure!(
            projection.row_count() - removed_rows + added_rows <= 65_536,
            "review presentation row limit exceeded"
        );
        ensure!(
            projected
                .target
                .keys()
                .all(|target| !self.projection_target.contains_key(target)
                    || removed_target.contains(target)),
            "review target identity collision"
        );
        let patch = projection.edit(range, projected.block)?;
        for block in removed_block {
            self.projection_link.remove(&block);
        }
        for target in removed_target {
            self.projection_target.remove(&target);
        }
        self.projection_link.extend(projected.link);
        self.projection_target.extend(projected.target);
        Ok(patch)
    }
}

impl ThreadProjection {
    fn boundary(&mut self, identity: String) -> Result<()> {
        self.block.push(BufferBlock {
            id: BlockId(identity),
            text: BufferText::from_rows([""])?,
            metadata: BlockMetadata::default(),
        });
        Ok(())
    }
    fn comment_rule(
        &mut self,
        identity: String,
        left: &str,
        right: &str,
        width: &WidthProfile,
        timestamp: Option<&str>,
    ) -> Result<()> {
        let text = rule_line(left, right, width)?;
        let mut metadata = full_row_style(&text, "ForgeReviewCommentHeader");
        if let Some(timestamp) = timestamp {
            let date = crate::presentation_time::relative(timestamp);
            if !date.is_empty() {
                append_term(&mut metadata, &text, &date, "ForgeStatusDate");
            }
        }
        self.block.push(BufferBlock {
            id: BlockId(identity),
            text: BufferText::from_rows([text])?,
            metadata,
        });
        Ok(())
    }

    fn comment_body(&mut self, identity: String, source: &str) -> Result<()> {
        let rows = source
            .replace("\r\n", "\n")
            .replace('\r', "\n")
            .split('\n')
            .map(str::to_owned)
            .collect::<Vec<_>>();
        let text = BufferText::from_rows(rows)?;
        let end = TextPosition {
            row: text.row_count() - 1,
            column: text
                .row(text.row_count() - 1)
                .context("comment body row is missing")?
                .len(),
        };
        self.block.push(BufferBlock {
            id: BlockId(identity),
            text,
            metadata: BlockMetadata {
                visible_decoration: vec![Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end,
                    },
                    capture: "ForgeReviewComment".into(),
                    priority: 200,
                }],
                ..Default::default()
            },
        });
        Ok(())
    }

    fn comment_footer(&mut self, identity: String, width: &WidthProfile) -> Result<()> {
        let text = "-".repeat(width.columns);
        self.block.push(BufferBlock {
            id: BlockId(identity),
            text: BufferText::from_rows([text.clone()])?,
            metadata: full_row_style(&text, "ForgeReviewCommentHeader"),
        });
        Ok(())
    }

    fn action(&mut self, identity: String, label: &str, target: ReviewTarget) -> Result<()> {
        let target_id = TargetId(identity.clone());
        self.target.insert(target_id.clone(), target);
        self.block.push(BufferBlock {
            id: BlockId(identity),
            text: BufferText::from_rows([label])?,
            metadata: BlockMetadata {
                target: vec![TargetRange {
                    id: target_id,
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: 0,
                            column: label.len(),
                        },
                    },
                }],
                visible_decoration: full_row_style(label, "ForgeReviewComment").visible_decoration,
                ..BlockMetadata::default()
            },
        });
        Ok(())
    }
}

fn rule_line(left: &str, right: &str, width: &WidthProfile) -> Result<String> {
    let mut left = left.to_owned();
    let mut right = right.to_owned();
    let mut fixed = width.cells(&format!("{left}{right}"), 0)?;
    if fixed > width.columns {
        let available = width.columns.saturating_sub(width.cells(&right, 0)?);
        left = truncate_display(&left, available, width)?;
        fixed = width.cells(&format!("{left}{right}"), 0)?;
    }
    if fixed > width.columns {
        let available = width.columns.saturating_sub(width.cells(&left, 0)?);
        right = truncate_display(&right, available, width)?;
        fixed = width.cells(&format!("{left}{right}"), 0)?;
    }
    Ok(format!(
        "{left}{}{right}",
        "-".repeat(width.columns.saturating_sub(fixed))
    ))
}

fn truncate_display(text: &str, available: usize, width: &WidthProfile) -> Result<String> {
    let mut rendered = String::new();
    for character in text.chars() {
        let candidate = format!("{rendered}{character}");
        if width.cells(&candidate, 0)? > available {
            break;
        }
        rendered.push(character);
    }
    Ok(rendered)
}

fn full_row_style(text: &str, capture: &str) -> BlockMetadata {
    BlockMetadata {
        visible_decoration: vec![Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: 0,
                    column: text.len(),
                },
            },
            capture: capture.into(),
            priority: 200,
        }],
        ..Default::default()
    }
}

fn append_term(metadata: &mut BlockMetadata, text: &str, term: &str, capture: &str) {
    let Some(column) = text.find(term) else {
        return;
    };
    metadata.visible_decoration.push(Decoration {
        range: TextRange {
            start: TextPosition { row: 0, column },
            end: TextPosition {
                row: 0,
                column: column + term.len(),
            },
        },
        capture: capture.into(),
        priority: 210,
    });
}

fn identity(prefix: &str, source: &str) -> String {
    let mut hash = std::hash::DefaultHasher::new();
    source.hash(&mut hash);
    format!("{prefix}:{:016x}", hash.finish())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn thread_presentation_assigns_stable_closed_comment_folds() {
        let record = serde_json::json!({
            "id":"THREAD_1","path":"src/lib.rs","isResolved":false,"isOutdated":false,
            "viewerCanReply":true,"viewerCanResolve":true,"viewerCanUnresolve":false,
            "diffSide":"RIGHT","line":5,
            "comments":{"totalCount":2,"pageInfo":{"hasNextPage":true,"endCursor":"next"},
                "nodes":[{"id":"COMMENT_1","databaseId":42,"body":"**Native body**",
                    "viewerDidAuthor":false,"url":"https://github.com/owner/repo/pull/7#discussion_r42",
                    "author":null,"line":5,"diffHunk":"@@ -5 +5 @@\n+source",
                    "pullRequestReview":{"id":"REVIEW_1","state":"PENDING"}}]}
        });
        let thread = super::super::thread::decode(&record).unwrap();
        let projected = render(&thread, &WidthProfile::default()).unwrap();
        assert!(
            projected
                .block
                .iter()
                .all(|block| block.metadata.editable_region.is_empty())
        );
        assert!(projected.target.values().any(|target| matches!(target,
            ReviewTarget::ThreadComment { thread, comment, reply: true }
                if thread == "THREAD_1" && comment == "COMMENT_1")));
        assert!(
            !projected
                .target
                .values()
                .any(|target| matches!(target, ReviewTarget::ThreadComment { reply: false, .. }))
        );
        assert!(projected.target.values().any(|target| matches!(target,
            ReviewTarget::ThreadMore { thread, cursor } if thread == "THREAD_1" && cursor == "next")));
        let author = projected
            .block
            .iter()
            .find(|block| block.id.0.ends_with(":author"))
            .expect("thread author presentation");
        let fold = author.metadata.fold.first().expect("thread comment fold");
        assert!(fold.closed);
        assert!(fold.id.0.starts_with("review:thread:"));
        assert!(fold.end.block.0.ends_with(":footer"));
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("thread-test".into()),
            projected.block,
        )
        .unwrap();
        let snapshot = document.snapshot();
        let text = snapshot
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("󰅺 Deleted user commented"));
        assert!(text.contains("Native body"));
    }

    #[test]
    fn comment_boxes_match_width_header_body_reply_and_footer_contracts() {
        let record = serde_json::json!({
            "id":"THREAD_1","path":"src/lib.rs","isResolved":false,"isOutdated":false,
            "viewerCanReply":true,"viewerCanResolve":true,"viewerCanUnresolve":false,
            "diffSide":"RIGHT","line":5,
            "comments":{"totalCount":2,"pageInfo":{"hasNextPage":false,"endCursor":null},
                "nodes":[
                    {"id":"COMMENT_1","databaseId":42,"body":"**literal markdown**\r\nsecond","viewerDidAuthor":true,
                        "url":"https://github.com/owner/repo/pull/7#discussion_r42",
                        "author":{"login":"author-with-a-long-name"},"line":5,
                        "createdAt":"2026-09-08T10:00:00Z","updatedAt":"2026-09-08T11:00:00Z",
                        "diffHunk":"@@ -5 +5 @@\n+source","pullRequestReview":{"id":"REVIEW_1","state":"PENDING"}},
                    {"id":"COMMENT_2","databaseId":43,"body":"reply body","viewerDidAuthor":false,
                        "url":"https://github.com/owner/repo/pull/7#discussion_r43",
                        "author":null,"line":5,"createdAt":"2026-09-08T12:00:00Z",
                        "diffHunk":"@@ -5 +5 @@\n+source","pullRequestReview":{"id":"REVIEW_2","state":"COMMENTED"}}
                ]}
        });
        let thread = super::super::thread::decode(&record).unwrap();
        for columns in [40, 80] {
            let width = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let projected = render(&thread, &width).unwrap();
            let header = projected
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":author"))
                .unwrap();
            let header_text = header.text.row(0).unwrap();
            assert_eq!(width.cells(header_text, 0).unwrap(), columns);
            assert!(header_text.starts_with("󰅺 author-with-a-long-name commented"));
            assert!(header_text.ends_with(" L5"));
            assert!(header.metadata.visible_decoration.iter().any(|style| {
                style.capture == "ForgeReviewCommentHeader"
                    && style.range.end.column == header_text.len()
            }));
            let body = projected
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":body"))
                .unwrap();
            assert_eq!(body.text.row(0), Some("**literal markdown**"));
            assert_eq!(body.text.row(1), Some("second"));
            assert!(
                body.metadata
                    .visible_decoration
                    .iter()
                    .any(|style| style.capture == "ForgeReviewComment")
            );
            let reply = projected
                .block
                .iter()
                .filter(|block| block.id.0.ends_with(":author"))
                .nth(1)
                .unwrap();
            assert!(
                reply
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with("↳ Deleted user replied")
            );
            assert_eq!(width.cells(reply.text.row(0).unwrap(), 0).unwrap(), columns);
            assert!(
                projected
                    .block
                    .iter()
                    .filter(|block| block.id.0.ends_with(":footer"))
                    .all(|footer| footer.text.row(0).unwrap() == "-".repeat(columns))
            );
            assert_eq!(
                projected
                    .target
                    .values()
                    .filter(|target| matches!(
                        target,
                        ReviewTarget::ThreadComment { reply: true, .. }
                    ))
                    .count(),
                2
            );
            assert_eq!(
                projected
                    .target
                    .values()
                    .filter(|target| matches!(
                        target,
                        ReviewTarget::ThreadComment { reply: false, .. }
                    ))
                    .count(),
                1
            );
            let continuation = render_range(&thread, &width, 1, false).unwrap();
            assert!(
                continuation
                    .block
                    .iter()
                    .find(|block| block.id.0.ends_with(":author"))
                    .unwrap()
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with("↳ Deleted user replied")
            );
        }
    }
}
