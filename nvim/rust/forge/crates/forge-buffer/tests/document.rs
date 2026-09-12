use forge_buffer::MAX_COUNTER;
use forge_buffer::block::{BlockMetadata, BufferBlock, TargetRange, TextPosition, TextRange};
use forge_buffer::document::BufferDocument;
use forge_buffer::identity::{BlockId, DocumentId, DocumentRevision, TargetId};
use forge_buffer::patch::{BufferPatch, TextEdit, apply_reference, apply_snapshot_reference};
use forge_buffer::sequence::{BlockSequence, SequenceEdit};
use forge_buffer::text::BufferText;

fn block(id: usize, rows: &[&str]) -> BufferBlock {
    BufferBlock {
        id: BlockId(format!("block-{id}")),
        text: BufferText::from_rows(rows).unwrap(),
        metadata: BlockMetadata::default(),
    }
}

#[test]
fn packed_text_roundtrips_empty_and_multibyte_rows_without_internal_offsets() {
    for rows in [vec![], vec![""], vec!["", ""], vec!["λ🙂", "", "last", ""]] {
        let text = BufferText::from_rows(&rows).unwrap();
        let encoded = serde_json::to_string(&text).unwrap();
        assert_eq!(encoded, serde_json::to_string(&rows).unwrap());
        assert_eq!(serde_json::from_str::<BufferText>(&encoded).unwrap(), text);
        assert_eq!(text.wire_rows(), rows);
        assert!(text.row(rows.len()).is_none());
        assert!(text.slice(0..rows.len() + 1).is_err());
    }
    assert!(BufferText::from_rows(["embedded\nnewline"]).is_err());
    assert!(BufferText::from_rows(["embedded\0NUL"]).is_err());
}

#[test]
fn metadata_rejects_columns_inside_utf8_and_accepts_end_of_document() {
    let mut entry = block(1, &["a🙂z", ""]);
    entry.metadata.target.push(TargetRange {
        id: TargetId("target".into()),
        range: TextRange {
            start: TextPosition { row: 0, column: 1 },
            end: TextPosition { row: 0, column: 5 },
        },
    });
    entry.validate().unwrap();
    assert_eq!(
        entry.target_at(TextPosition { row: 0, column: 1 }),
        Some(&TargetId("target".into()))
    );
    assert_eq!(entry.target_at(TextPosition { row: 0, column: 5 }), None);
    entry.metadata.target[0].range.end.column = 3;
    assert!(entry.validate().is_err());
    entry.metadata.target[0].range.end = TextPosition { row: 2, column: 0 };
    entry.validate().unwrap();
    entry.metadata.target[0].range.end.column = 1;
    assert!(entry.validate().is_err());
}

#[test]
fn randomized_splices_match_reference_rows_and_retained_coordinates() {
    let mut sequence = BlockSequence::default();
    let mut expected = Vec::new();
    let mut random = 89123u64;
    for identity in 0..2000 {
        random = random.wrapping_mul(6364136223846793005).wrapping_add(1);
        let start = random as usize % (expected.len() + 1);
        let removed = if start < expected.len() && random & 1 == 0 {
            1
        } else {
            0
        };
        let entry = block(identity, &["λ", "", "tail"]);
        sequence
            .splice(start..start + removed, vec![entry.clone()])
            .unwrap();
        expected.splice(start..start + removed, [entry]);
        assert_eq!(sequence.blocks().cloned().collect::<Vec<_>>(), expected);
        assert_eq!(sequence.row_count(), expected.len() * 3);
        assert_eq!(sequence.byte_count(), expected.len() * 9);
        for (index, entry) in expected.iter().enumerate() {
            assert_eq!(sequence.position(&entry.id), Some(index * 3));
        }
        let row = random as usize % sequence.row_count();
        let (found, relative) = sequence.locate(row).unwrap();
        assert_eq!(found, &expected[row / 3]);
        assert_eq!(relative, row % 3);
    }
}

#[test]
fn document_patches_match_reference_across_insert_delete_move_and_metadata() {
    let document_id = DocumentId("document".into());
    let mut document = BufferDocument::new(document_id.clone(), vec![]).unwrap();
    let mut revision = DocumentRevision(0);
    let mut rows = Vec::new();
    let mut expected = Vec::new();
    let mut random = 271828u64;
    for identity in 0..500 {
        random = random.wrapping_mul(6364136223846793005).wrapping_add(1);
        let start = random as usize % (expected.len() + 1);
        expected.insert(start, block(identity, &["", "🙂", "last"]));
        if expected.len() > 5 && random & 1 == 0 {
            expected.remove(0);
        }
        if expected.len() > 3 && random & 2 == 0 {
            expected.swap(0, 2);
        }
        if let Some(patch) = document
            .edit(0..document.block_count(), expected.clone())
            .unwrap()
        {
            rows = apply_reference(&document_id, revision, &rows, &patch).unwrap();
            revision = patch.next;
        }
        let expected_rows: Vec<String> = expected
            .iter()
            .flat_map(|entry| entry.text.wire_rows().into_iter().map(str::to_owned))
            .collect();
        assert_eq!(rows, expected_rows);
        assert_eq!(document.snapshot().revision, revision);
        assert!(
            document
                .edit(0..document.block_count(), expected.clone())
                .unwrap()
                .is_none()
        );
        assert!(document.locate(DocumentRevision(0), 0).is_err());
    }
    expected[0].metadata.target.push(TargetRange {
        id: TargetId("target".into()),
        range: TextRange {
            start: TextPosition { row: 1, column: 0 },
            end: TextPosition { row: 1, column: 4 },
        },
    });
    let patch = document
        .edit(0..document.block_count(), expected)
        .unwrap()
        .unwrap();
    assert!(patch.text_edit.is_empty());
    assert_eq!(patch.metadata_edit.len(), 1);
}

#[test]
fn invalid_candidate_does_not_publish_revision_or_change_text() {
    let mut document =
        BufferDocument::new(DocumentId("document".into()), vec![block(1, &["original"])]).unwrap();
    let snapshot = document.snapshot();
    assert!(
        document
            .edit(0..1, vec![block(1, &["changed"]), block(1, &["duplicate"])])
            .is_err()
    );
    assert_eq!(document.snapshot(), snapshot);
    assert!(DocumentRevision(MAX_COUNTER).next().is_err());
    assert!(DocumentRevision(MAX_COUNTER + 1).validate().is_err());
}

#[test]
fn malformed_patches_are_rejected_before_reference_mutation() {
    let mut patch = BufferPatch {
        document: DocumentId("document".into()),
        base: DocumentRevision(0),
        next: DocumentRevision(1),
        base_rows: 3,
        next_rows: 3,
        text_edit: vec![TextEdit {
            start_row: 2,
            removed_rows: 1,
            text: BufferText::from_rows(["changed"]).unwrap(),
        }],
        metadata_edit: vec![],
        removed_block: vec![],
        block_edit: vec![],
        base_blocks: 0,
        next_blocks: 0,
    };
    let rows = vec!["a".into(), "b".into(), "c".into()];
    assert_eq!(
        apply_reference(&patch.document, patch.base, &rows, &patch).unwrap(),
        ["a", "b", "changed"]
    );
    assert!(apply_reference(&patch.document, DocumentRevision(1), &rows, &patch).is_err());
    patch.text_edit.push(patch.text_edit[0].clone());
    assert!(patch.validate().is_err());
    patch.text_edit.pop();
    patch.text_edit[0].start_row = usize::MAX;
    assert!(patch.validate().is_err());
    patch.text_edit[0].start_row = 2;
    patch.next_rows = 4;
    assert!(patch.validate().is_err());
}

#[test]
fn local_edits_publish_only_affected_blocks_and_preserve_replica_coordinates() {
    let mut document = BufferDocument::new(
        DocumentId("large".into()),
        (0..20_000)
            .map(|identity| block(identity, &["first", "last"]))
            .collect(),
    )
    .unwrap();
    let before = document.snapshot();
    let patch = document
        .edit(
            10_000..10_001,
            vec![block(10_000, &["first", "changed", "last"])],
        )
        .unwrap()
        .unwrap();
    assert_eq!(patch.text_edit.len(), 1);
    assert_eq!(patch.text_edit[0].start_row, 20_001);
    assert_eq!(patch.text_edit[0].removed_rows, 0);
    assert_eq!(patch.text_edit[0].text.wire_rows(), ["changed"]);
    assert!(patch.block_edit.is_empty());
    assert_eq!(patch.metadata_edit.len(), 1);
    assert!(serde_json::to_vec(&patch).unwrap().len() < 1024);
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
    let before = document.snapshot();
    let patch = document
        .edit(10_001..10_001, vec![block(30_000, &["new"])])
        .unwrap()
        .unwrap();
    assert_eq!(patch.block_edit.len(), 1);
    assert_eq!(patch.block_edit[0].start_block, 10_001);
    assert_eq!(patch.block_edit[0].removed_blocks, 0);
    assert_eq!(
        patch.block_edit[0].inserted,
        [BlockId("block-30000".into())]
    );
    assert!(serde_json::to_vec(&patch).unwrap().len() < 1024);
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
    let before = document.snapshot();
    let patch = document.edit(10_000..10_002, vec![]).unwrap().unwrap();
    assert_eq!(patch.removed_block.len(), 2);
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
}

#[test]
fn replica_rejects_inconsistent_metadata_order_and_retirement() {
    let mut document =
        BufferDocument::new(DocumentId("document".into()), vec![block(1, &["base"])]).unwrap();
    let before = document.snapshot();
    let patch = document
        .edit(0..1, vec![block(2, &["changed"])])
        .unwrap()
        .unwrap();
    let mut invalid = patch.clone();
    invalid.metadata_edit[0].row_count = 2;
    assert!(apply_snapshot_reference(&before, &invalid).is_err());
    let mut invalid = patch.clone();
    invalid.metadata_edit.clear();
    assert!(apply_snapshot_reference(&before, &invalid).is_err());
    let mut invalid = patch.clone();
    invalid.removed_block.clear();
    assert!(apply_snapshot_reference(&before, &invalid).is_err());
    let mut invalid = patch.clone();
    invalid.block_edit[0]
        .inserted
        .push(BlockId("block-2".into()));
    invalid.next_blocks += 1;
    assert!(apply_snapshot_reference(&before, &invalid).is_err());
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
}

#[test]
fn disjoint_transactions_preserve_base_coordinates_and_move_identities() {
    let original = vec![
        block(1, &["first"]),
        block(2, &["middle"]),
        block(3, &["last"]),
    ];
    let mut document =
        BufferDocument::new(DocumentId("transaction".into()), original.clone()).unwrap();
    let before = document.snapshot();
    let patch = document
        .edit_many(vec![
            SequenceEdit {
                range: 0..1,
                block: vec![original[2].clone()],
            },
            SequenceEdit {
                range: 2..3,
                block: vec![original[0].clone(), block(4, &["extra", "row"])],
            },
        ])
        .unwrap()
        .unwrap();
    assert_eq!(patch.next, DocumentRevision(1));
    assert!(patch.removed_block.is_empty());
    assert_eq!(patch.text_edit.len(), 2);
    assert_eq!(patch.text_edit[0].start_row, 2);
    assert_eq!(patch.text_edit[1].start_row, 0);
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
}

#[test]
fn invalid_transaction_preserves_all_blocks_and_revision() {
    let mut document = BufferDocument::new(
        DocumentId("transaction".into()),
        vec![
            block(1, &["first"]),
            block(2, &["middle"]),
            block(3, &["last"]),
        ],
    )
    .unwrap();
    let before = document.snapshot();
    for edits in [
        vec![
            SequenceEdit {
                range: 2..3,
                block: vec![block(4, &["valid"])],
            },
            SequenceEdit {
                range: 0..1,
                block: vec![block(2, &["duplicate retained identity"])],
            },
        ],
        vec![
            SequenceEdit {
                range: 0..2,
                block: vec![],
            },
            SequenceEdit {
                range: 1..3,
                block: vec![],
            },
        ],
        vec![
            SequenceEdit {
                range: 0..0,
                block: vec![block(4, &["insert"])],
            },
            SequenceEdit {
                range: 0..1,
                block: vec![],
            },
        ],
    ] {
        assert!(document.edit_many(edits).is_err());
        assert_eq!(document.snapshot(), before);
    }
}

#[test]
fn adjacent_empty_blocks_coalesce_text_without_losing_block_order() {
    let mut document = BufferDocument::new(
        DocumentId("empty".into()),
        vec![block(1, &[]), block(2, &[]), block(3, &["tail"])],
    )
    .unwrap();
    let before = document.snapshot();
    let patch = document
        .edit_many(vec![
            SequenceEdit {
                range: 0..1,
                block: vec![block(1, &["first"])],
            },
            SequenceEdit {
                range: 1..2,
                block: vec![block(2, &["second"])],
            },
        ])
        .unwrap()
        .unwrap();
    assert_eq!(patch.text_edit.len(), 1);
    assert_eq!(patch.text_edit[0].text.wire_rows(), ["first", "second"]);
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
}

#[test]
fn randomized_transactions_match_independent_block_model() {
    let mut expected: Vec<_> = (0..30).map(|identity| block(identity, &["base"])).collect();
    let mut document = BufferDocument::new(DocumentId("random".into()), expected.clone()).unwrap();
    let mut random = 78123u64;
    for iteration in 0..1000 {
        random = random.wrapping_mul(6364136223846793005).wrapping_add(1);
        let lower = random as usize % (expected.len() / 2);
        let upper = expected.len() / 2 + random as usize % (expected.len() - expected.len() / 2);
        let mut lower_block = expected[upper].clone();
        let mut upper_block = expected[lower].clone();
        lower_block.text = BufferText::from_rows(vec!["lower"; iteration % 4]).unwrap();
        upper_block.text = BufferText::from_rows(vec!["upper"; iteration % 3]).unwrap();
        let mut replacement = vec![upper_block];
        if iteration % 7 == 0 {
            replacement.push(block(1000 + iteration, &["inserted"]));
        }
        let before = document.snapshot();
        let patch = document
            .edit_many(vec![
                SequenceEdit {
                    range: lower..lower + 1,
                    block: vec![lower_block.clone()],
                },
                SequenceEdit {
                    range: upper..upper + 1,
                    block: replacement.clone(),
                },
            ])
            .unwrap()
            .unwrap();
        expected.splice(upper..upper + 1, replacement);
        expected.splice(lower..lower + 1, [lower_block]);
        assert_eq!(
            apply_snapshot_reference(&before, &patch).unwrap(),
            document.snapshot()
        );
        let reference = BufferDocument::new(DocumentId("random".into()), expected.clone())
            .unwrap()
            .snapshot();
        let mut actual = document.snapshot();
        actual.revision = reference.revision;
        assert_eq!(actual, reference);
    }
}
