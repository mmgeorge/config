use forge_buffer::block::{BlockMetadata, BufferBlock, TargetRange, TextPosition, TextRange};
use forge_buffer::document::BufferDocument;
use forge_buffer::identity::{BlockId, DocumentId, DocumentRevision, TargetId};
use forge_buffer::patch::{apply_reference, apply_snapshot_reference};
use forge_buffer::sequence::SequenceEdit;
use forge_buffer::text::BufferText;

fn block(id: usize, rows: &[&str]) -> BufferBlock {
    BufferBlock {
        id: BlockId(format!("block-{id}")),
        text: BufferText::from_rows(rows).unwrap(),
        metadata: BlockMetadata::default(),
    }
}

#[test]
fn node_enum_spelling_matches_the_editor_contract() {
    use forge_buffer::node::{NodeDisplay, NodeKind, NodeLifecycle};
    let contract: serde_json::Value = serde_json::from_str(include_str!(
        "../../../../../lua/forge/protocol_contract.json"
    ))
    .unwrap();
    let mut kind = serde_json::json!([
        NodeKind::Exchange,
        NodeKind::Message,
        NodeKind::Thought,
        NodeKind::ToolGroup,
        NodeKind::Tool,
        NodeKind::Changes,
        NodeKind::File,
        NodeKind::Hunk,
        NodeKind::Group
    ]);
    let mut lifecycle = serde_json::json!([NodeLifecycle::Live, NodeLifecycle::Settled]);
    let mut display = serde_json::json!([
        NodeDisplay::Heading,
        NodeDisplay::Preview,
        NodeDisplay::Full
    ]);
    for (name, actual) in [
        ("kind", &mut kind),
        ("lifecycle", &mut lifecycle),
        ("display", &mut display),
    ] {
        let mut expected: Vec<_> = contract["node"][name]
            .as_array()
            .unwrap()
            .iter()
            .map(|value| value.as_str().unwrap())
            .collect();
        let mut actual: Vec<_> = actual
            .as_array_mut()
            .unwrap()
            .iter()
            .map(|value| value.as_str().unwrap())
            .collect();
        expected.sort_unstable();
        actual.sort_unstable();
        assert_eq!(actual, expected, "{name}");
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
