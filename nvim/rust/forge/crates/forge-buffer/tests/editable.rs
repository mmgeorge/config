use forge_buffer::block::{
    BlockMetadata, BufferBlock, Decoration, EditableRegion, SourceOverlay, TargetRange,
    TextPosition, TextRange,
};
use forge_buffer::document::BufferDocument;
use forge_buffer::editable::{LocalEdit, LocalEditResult};
use forge_buffer::identity::{
    BlockId, DocumentId, EditSequence, RegionId, RegionRevision, TargetId,
};
use forge_buffer::patch::apply_snapshot_reference;
use forge_buffer::text::BufferText;

fn range(row: usize, start: usize, end: usize) -> TextRange {
    TextRange {
        start: TextPosition { row, column: start },
        end: TextPosition { row, column: end },
    }
}

fn block() -> BufferBlock {
    BufferBlock {
        id: BlockId("body".into()),
        text: BufferText::from_rows(["prefix editable suffix", "other"]).unwrap(),
        metadata: BlockMetadata {
            editable_region: vec![
                EditableRegion {
                    id: RegionId("body".into()),
                    revision: RegionRevision(3),
                    sequence: EditSequence(0),
                    range: range(0, 7, 15),
                },
                EditableRegion {
                    id: RegionId("other".into()),
                    revision: RegionRevision(0),
                    sequence: EditSequence(0),
                    range: range(1, 0, 5),
                },
            ],
            target: vec![TargetRange {
                id: TargetId("suffix".into()),
                range: range(0, 16, 22),
            }],
            decoration: vec![Decoration {
                range: range(0, 7, 15),
                capture: "String".into(),
                priority: 10,
            }],
            ..BlockMetadata::default()
        },
    }
}

fn edit(sequence: u64, base: u64, rows: &[&str]) -> LocalEdit {
    LocalEdit {
        document: DocumentId("document".into()),
        region: RegionId("body".into()),
        base: RegionRevision(base),
        sequence: EditSequence(sequence),
        text: BufferText::from_rows(rows).unwrap(),
    }
}

#[test]
fn acceptance_ignores_unrelated_layout_revision_and_rebases_retained_metadata() {
    let mut source = block();
    source.metadata.decoration.push(Decoration {
        range: range(0, 6, 10),
        capture: "Keyword".into(),
        priority: 20,
    });
    source.metadata.source_highlight = vec![
        Decoration {
            range: range(0, 7, 8),
            capture: "String".into(),
            priority: 4096,
        },
        Decoration {
            range: range(1, 0, 1),
            capture: "String".into(),
            priority: 4096,
        },
    ];
    source.metadata.source_overlay = vec![
        SourceOverlay {
            range: range(0, 7, 8),
            text: "●".into(),
            capture: "String".into(),
            priority: 120,
        },
        SourceOverlay {
            range: range(1, 0, 1),
            text: "●".into(),
            capture: "String".into(),
            priority: 120,
        },
    ];
    let mut document = BufferDocument::new(DocumentId("document".into()), vec![source]).unwrap();
    document
        .edit(
            0..0,
            vec![BufferBlock {
                id: BlockId("header".into()),
                text: BufferText::from_rows(["header"]).unwrap(),
                metadata: BlockMetadata::default(),
            }],
        )
        .unwrap();
    let before = document.snapshot();
    let LocalEditResult::Accepted {
        acknowledgement,
        patch,
    } = document
        .accept_local_edit(edit(1, 3, &["λ", "🙂"]))
        .unwrap()
    else {
        panic!("edit was not accepted");
    };
    assert_eq!(acknowledgement.sequence, EditSequence(1));
    assert_eq!(acknowledgement.revision, RegionRevision(4));
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
    let snapshot = document.snapshot();
    let block = &snapshot.block[1];
    assert_eq!(block.text.wire_rows(), ["prefix λ", "🙂 suffix", "other"]);
    assert_eq!(
        block.metadata.editable_region[0].range.end,
        TextPosition { row: 1, column: 4 }
    );
    assert_eq!(block.metadata.editable_region[1].range, range(2, 0, 5));
    assert_eq!(block.metadata.target[0].range, range(1, 5, 11));
    assert_eq!(block.metadata.decoration.len(), 1);
    assert_eq!(block.metadata.decoration[0].capture, "String");
    assert_eq!(block.metadata.decoration[0].range, range(0, 7, 9));
    assert_eq!(block.metadata.source_overlay.len(), 1);
    assert_eq!(block.metadata.source_overlay[0].range, range(2, 0, 1));
    assert_eq!(block.metadata.source_highlight.len(), 1);
    assert_eq!(block.metadata.source_highlight[0].range, range(2, 0, 1));
}

#[test]
fn conflicts_and_stale_sequences_never_mutate_document() {
    let mut document = BufferDocument::new(DocumentId("document".into()), vec![block()]).unwrap();
    assert!(matches!(
        document
            .accept_local_edit(edit(4, 3, &["accepted"]))
            .unwrap(),
        LocalEditResult::Accepted { .. }
    ));
    let before = document.snapshot();
    assert!(matches!(
        document
            .accept_local_edit(edit(5, 3, &["conflicting"]))
            .unwrap(),
        LocalEditResult::Conflict {
            current: RegionRevision(4)
        }
    ));
    assert!(matches!(
        document
            .accept_local_edit(edit(4, 4, &["replayed"]))
            .unwrap(),
        LocalEditResult::StaleSequence
    ));
    assert!(matches!(
        document.accept_local_edit(edit(3, 4, &["older"])).unwrap(),
        LocalEditResult::StaleSequence
    ));
    let mut unknown = edit(5, 4, &["unknown"]);
    unknown.region = RegionId("missing".into());
    assert!(matches!(
        document.accept_local_edit(unknown).unwrap(),
        LocalEditResult::UnknownRegion
    ));
    assert!(
        document
            .accept_local_edit(edit(0, 4, &["invalid"]))
            .is_err()
    );
    assert_eq!(document.snapshot(), before);
}
