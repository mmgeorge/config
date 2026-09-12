use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{
        BlockAnchor, BlockMetadata, BufferBlock, Decoration, FoldRange, TextPosition, TextRange,
    },
    identity::{BlockId, FoldId, TargetId},
    sequence::SequenceEdit,
    text::BufferText,
};
use forge_diff::source::{SourceCoordinate, SourceSide};

use crate::{
    StatusSection,
    document::{DOCUMENT_BYTES, DisplayGroupTarget, FileTarget, HunkTarget, StatusDocument, label},
};

pub(crate) struct HunkProjection {
    pub block: Vec<BufferBlock>,
    pub edit: Vec<SequenceEdit>,
    pub source: Vec<(BlockId, Vec<SourceCoordinate>)>,
    pub retained: usize,
}

pub(crate) fn project(
    document: &mut StatusDocument,
    file: &mut FileTarget,
    target: &TargetId,
    body: BufferBlock,
    source: Vec<SourceCoordinate>,
    group: &[usize],
    body_retained: usize,
) -> Result<HunkProjection> {
    ensure!(
        group.len() == body.text.row_count() && source.len() == group.len(),
        "hunk batch coordinate count differs"
    );
    let mut ranges = Vec::new();
    let mut start = 0;
    while start < group.len() {
        let index = group[start];
        let end = start
            + group[start..]
                .iter()
                .take_while(|candidate| **candidate == index)
                .count();
        ranges.push((index, start..end));
        start = end;
    }
    let mut retained = 0usize;
    for (index, _) in &ranges {
        if !file.group.contains_key(index) {
            let display = file
                .cursor
                .as_ref()
                .and_then(|cursor| cursor.group(*index))
                .context("display group missing")?;
            retained = retained
                .checked_add(4096 + display.raw_ids.len() * size_of::<forge_diff::raw::RawHunkId>())
                .context("hunk accounting overflow")?;
        }
    }
    ensure!(
        document
            .retained_bytes
            .saturating_add(body_retained)
            .saturating_add(retained)
            <= DOCUMENT_BYTES,
        "hunk projection exceeds document budget"
    );
    let mut projection = HunkProjection {
        block: Vec::new(),
        edit: Vec::new(),
        source: Vec::new(),
        retained,
    };
    for (part, (index, range)) in ranges.into_iter().enumerate() {
        let body_id = if part == 0 {
            body.id.clone()
        } else {
            BlockId(document.next_id("body")?)
        };
        let display = file
            .cursor
            .as_ref()
            .and_then(|cursor| cursor.group(index))
            .context("display group missing")?;
        let mut header = if let Some(header) = file.group.get(&index).and_then(|group| file.body.as_ref().and_then(|body| body.block(&group.header))) {
            header.clone()
        } else {
            let (header, group_target) = if let Some(group) = file.group.get(&index) {
                (group.header.clone(), group.target.clone())
            } else {
                (BlockId(document.next_id("hunk-header")?), TargetId(document.next_id("group")?))
            };
            document.hunk.insert(
                group_target.clone(),
                HunkTarget {
                    file: target.clone(),
                    raw: display.raw_ids.clone(),
                },
            );
            let (added, deleted) = file
                .analysis
                .as_ref()
                .context("hunk analysis missing")?
                .hunks()
                .iter()
                .filter(|hunk| display.raw_ids.contains(&hunk.id))
                .fold((0usize, 0usize), |(added, deleted), hunk| {
                    (added + hunk.new_lines.len(), deleted + hunk.old_lines.len())
                });
            let addition = format!("+{added}");
            let deletion = format!("-{deleted}");
            let context = file.cursor.as_ref().and_then(|cursor| cursor.context_label(index));
            let text = match context {
                Some(context) => format!("@@ {addition} {deletion} {context}"),
                None => format!("@@ {addition} {deletion}"),
            };
            let coordinate = if display.new_lines.is_empty() {
                SourceCoordinate {
                    side: SourceSide::Old,
                    line: display.old_lines.start,
                    byte_column: 0,
                }
            } else {
                SourceCoordinate {
                    side: SourceSide::New,
                    line: display.new_lines.start,
                    byte_column: 0,
                }
            };
            projection.source.push((header.clone(), vec![coordinate]));
            file.group.insert(
                index,
                DisplayGroupTarget {
                    header: header.clone(),
                    target: group_target.clone(),
                },
            );
            let mut header = label(header, &text, "ForgeHunkHeader", Some(group_target))?;
            for (column, value, capture) in [
                (3, addition.as_str(), "ForgeAddRange"),
                (4 + addition.len(), deletion.as_str(), "ForgeDeleteRange"),
            ] {
                header.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column },
                        end: TextPosition {
                            row: 0,
                            column: column + value.len(),
                        },
                    },
                    capture: capture.into(),
                    priority: 110,
                });
            }
            header
        };
        let group_target = &file.group[&index].target;
        let mut metadata = BlockMetadata::default();
        let translate = |value: TextRange| TextRange {
            start: TextPosition {
                row: value.start.row - range.start,
                ..value.start
            },
            end: TextPosition {
                row: value.end.row - range.start,
                ..value.end
            },
        };
        for item in body
            .metadata
            .target
            .iter()
            .filter(|item| range.contains(&item.range.start.row))
        {
            let mut item = item.clone();
            item.range = translate(item.range);
            if item.id == *target {
                item.id = group_target.clone();
            }
            metadata.target.push(item);
        }
        for item in body
            .metadata
            .decoration
            .iter()
            .filter(|item| range.contains(&item.range.start.row))
        {
            let mut item = item.clone();
            item.range = translate(item.range);
            metadata.decoration.push(item);
        }
        for item in body
            .metadata
            .visible_decoration
            .iter()
            .filter(|item| range.contains(&item.range.start.row))
        {
            let mut item = item.clone();
            item.range = translate(item.range);
            metadata.visible_decoration.push(item);
        }
        for item in body
            .metadata
            .gutter
            .iter()
            .filter(|item| range.contains(&item.position.row))
        {
            let mut item = item.clone();
            item.position.row -= range.start;
            metadata.gutter.push(item);
        }
        let text = BufferText::from_rows(body.text.slice(range.clone())?)?;
        header.metadata.fold = vec![FoldRange {
            id: FoldId(header.id.0.clone()),
            start: TextPosition { row: 0, column: 0 },
            end: BlockAnchor {
                block: body_id.clone(),
                position: TextPosition {
                    row: text.row_count(),
                    column: 0,
                },
            },
            closed: document.local_path.is_some() && file.section == StatusSection::Staged,
        }];
        if let Some(position) = file
            .body
            .as_ref()
            .expect("initialized status body")
            .block_index(&header.id)
        {
            projection.edit.push(SequenceEdit {
                range: position..position + 1,
                block: vec![header],
            });
        } else {
            projection.block.push(header);
        }
        projection
            .source
            .push((body_id.clone(), source[range].to_vec()));
        projection.block.push(BufferBlock {
            id: body_id,
            text,
            metadata,
        });
    }
    Ok(projection)
}
