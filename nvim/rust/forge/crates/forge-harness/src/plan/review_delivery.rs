use anyhow::Result;
use forge_buffer::block::BufferBlock;
use forge_buffer::document::BufferDocument;
use forge_buffer::patch::BufferPatch;
use forge_buffer::sequence::SequenceEdit;
use std::collections::HashMap;

#[cfg(test)]
pub(crate) fn reconstruct(
    previous: Option<&serde_json::Value>,
    mut delivery: serde_json::Value,
) -> serde_json::Value {
    use forge_buffer::patch::{BufferSnapshot, apply_snapshot_reference};
    let snapshot: BufferSnapshot = if let Some(previous) = previous {
        assert!(
            delivery.get("snapshot").is_none(),
            "updates must not carry snapshots"
        );
        let mut snapshot = serde_json::from_value(previous["snapshot"].clone()).unwrap();
        if !delivery["patch"].is_null() {
            let patch = serde_json::from_value(delivery["patch"].clone()).unwrap();
            snapshot = apply_snapshot_reference(&snapshot, &patch).unwrap();
        }
        snapshot
    } else {
        serde_json::from_value(delivery["snapshot"].clone()).unwrap()
    };
    let mut anchor = previous
        .into_iter()
        .flat_map(|previous| previous["source_row"].as_array().unwrap())
        .map(|row| (row["id"].as_str().unwrap().to_owned(), row.clone()))
        .collect::<HashMap<_, _>>();
    for id in delivery["source_edit"]["remove"].as_array().unwrap() {
        anchor.remove(id.as_str().unwrap());
    }
    for row in delivery["source_edit"]["upsert"].as_array().unwrap() {
        anchor.insert(row["id"].as_str().unwrap().to_owned(), row.clone());
    }
    let mut rows = Vec::new();
    for block in &snapshot.block {
        for (position, text) in block.text.wire_rows().iter().enumerate() {
            let mut row = anchor[&format!("{}:{position}", block.id.0)].clone();
            row["text"] = serde_json::json!(text);
            row["block"] = serde_json::json!(block.id);
            row["position"] = serde_json::json!({"row":position,"column":0});
            row["target"] = serde_json::json!(
                block
                    .metadata
                    .target
                    .iter()
                    .find(|target| target.range.start.row <= position
                        && position < target.range.end.row)
                    .map(|target| &target.id)
            );
            rows.push(row);
        }
    }
    delivery["snapshot"] = serde_json::to_value(snapshot).unwrap();
    delivery["source_row"] = serde_json::json!(rows);
    delivery
}

pub(super) fn replace(
    document: &mut BufferDocument,
    replacement: Vec<BufferBlock>,
) -> Result<Option<BufferPatch>> {
    let previous = document
        .blocks(document.revision(), 0..document.block_count())?
        .enumerate()
        .map(|(index, block)| (block.id.clone(), (index, block)))
        .collect::<HashMap<_, _>>();
    let mut edits = Vec::new();
    let mut old_start = 0;
    let mut new_start = 0;
    for (new_index, block) in replacement.iter().enumerate() {
        let Some(&(old_index, old_block)) = previous.get(&block.id) else {
            continue;
        };
        if old_index < old_start || old_block != block {
            continue;
        }
        if old_index != old_start || new_index != new_start {
            edits.push(SequenceEdit {
                range: old_start..old_index,
                block: replacement[new_start..new_index].to_vec(),
            });
        }
        old_start = old_index + 1;
        new_start = new_index + 1;
    }
    if old_start != document.block_count() || new_start != replacement.len() {
        edits.push(SequenceEdit {
            range: old_start..document.block_count(),
            block: replacement[new_start..].to_vec(),
        });
    }
    document.edit_many(edits).map_err(Into::into)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::BlockMetadata;
    use forge_buffer::identity::{BlockId, DocumentId};
    use forge_buffer::text::BufferText;

    fn block(index: usize, text: &str) -> BufferBlock {
        BufferBlock {
            id: BlockId(format!("row:{index}")),
            text: BufferText::from_rows([text]).unwrap(),
            metadata: BlockMetadata::default(),
        }
    }

    #[test]
    fn distant_changes_do_not_resend_retained_rows() {
        for count in [100, 10_000] {
            let original = (0..count)
                .map(|index| block(index, "retained declaration"))
                .collect::<Vec<_>>();
            let mut document =
                BufferDocument::new(DocumentId("review".into()), original.clone()).unwrap();
            let mut changed = original.clone();
            changed[1] = block(1, "first change");
            changed[count - 2] = block(count - 2, "last change");
            let patch = replace(&mut document, changed.clone()).unwrap().unwrap();
            assert_eq!(patch.text_edit.len(), 2);
            assert_eq!(
                patch
                    .text_edit
                    .iter()
                    .map(|edit| edit.text.row_count())
                    .sum::<usize>(),
                2
            );
            assert!(
                !serde_json::to_string(&patch)
                    .unwrap()
                    .contains("retained declaration")
            );
            assert_eq!(document.snapshot().block, changed);
            assert!(replace(&mut document, changed).unwrap().is_none());
            replace(&mut document, original.clone()).unwrap();
            assert_eq!(document.snapshot().block, original);
        }
    }

    #[test]
    fn insert_delete_reorder_and_empty_replacements_match_the_projection() {
        let original = (0..6)
            .map(|index| block(index, "declaration"))
            .collect::<Vec<_>>();
        let mut document =
            BufferDocument::new(DocumentId("review".into()), original.clone()).unwrap();
        for replacement in [
            vec![
                original[3].clone(),
                block(7, "inserted"),
                original[1].clone(),
            ],
            vec![],
            original,
        ] {
            replace(&mut document, replacement.clone()).unwrap();
            assert_eq!(document.snapshot().block, replacement);
        }
    }
}
