use forge_buffer::block::{
    BlockAnchor, BlockMetadata, BufferBlock, Decoration, EditableRegion, FoldRange, SourceOverlay,
    TargetRange, TextPosition, TextRange,
};
use forge_buffer::document::{BufferDocument, LocalEditPreparation};
use forge_buffer::editable::{LocalEdit, LocalEditResult};
use forge_buffer::identity::{
    BlockId, DocumentId, EditSequence, FoldId, RegionId, RegionRevision, TargetId,
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

#[test]
fn restored_region_sequence_rejects_replay_and_survives_snapshot_reopening() {
    let mut original = block();
    original.metadata.editable_region[0].sequence = EditSequence(100);
    let mut document = BufferDocument::new(DocumentId("document".into()), vec![original]).unwrap();
    assert!(matches!(
        document
            .accept_local_edit(edit(100, 3, &["replayed"]))
            .unwrap(),
        LocalEditResult::StaleSequence
    ));
    assert!(matches!(
        document
            .accept_local_edit(edit(101, 3, &["accepted"]))
            .unwrap(),
        LocalEditResult::Accepted { .. }
    ));
    let snapshot = document.snapshot();
    assert_eq!(
        snapshot.block[0].metadata.editable_region[0].sequence,
        EditSequence(101)
    );
    let mut reopened = BufferDocument::new(DocumentId("document".into()), snapshot.block).unwrap();
    assert!(matches!(
        reopened
            .accept_local_edit(edit(101, 4, &["replayed again"]))
            .unwrap(),
        LocalEditResult::StaleSequence
    ));
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
fn local_edit_rebases_external_and_same_block_folds_in_one_atomic_patch() {
    let mut body = block();
    body.metadata.fold.push(FoldRange {
        id: FoldId("trailing".into()),
        start: TextPosition { row: 1, column: 0 },
        end: BlockAnchor {
            block: body.id.clone(),
            position: TextPosition { row: 2, column: 0 },
        },
        closed: false,
    });
    let header = BufferBlock {
        id: BlockId("header".into()),
        text: BufferText::from_rows(["Comment"]).unwrap(),
        metadata: BlockMetadata {
            fold: vec![FoldRange {
                id: FoldId("comment".into()),
                start: TextPosition { row: 0, column: 0 },
                end: BlockAnchor {
                    block: body.id.clone(),
                    position: TextPosition { row: 2, column: 0 },
                },
                closed: true,
            }],
            ..BlockMetadata::default()
        },
    };
    let mut document =
        BufferDocument::new(DocumentId("document".into()), vec![header, body]).unwrap();
    let before = document.snapshot();
    drop(
        document
            .prepare_local_edit(edit(1, 3, &["grown", "", "tail"]))
            .unwrap(),
    );
    assert_eq!(document.snapshot(), before);
    for (sequence, revision, rows, expected_rows) in [
        (1, 3, vec!["grown", "", "tail"], 4),
        (2, 4, vec!["short"], 2),
    ] {
        let before = document.snapshot();
        let LocalEditResult::Accepted { patch, .. } = document
            .accept_local_edit(edit(sequence, revision, &rows))
            .unwrap()
        else {
            panic!("valid fold edit was rejected")
        };
        let after = document.snapshot();
        assert_eq!(patch.metadata_edit.len(), 2);
        assert_eq!(patch.text_edit.len(), 1);
        assert_eq!(patch.next.0, patch.base.0 + 1);
        assert_eq!(apply_snapshot_reference(&before, &patch).unwrap(), after);
        assert_eq!(
            after.block[0].metadata.fold[0].end.position.row,
            expected_rows
        );
        assert_eq!(after.block[1].metadata.fold[0].start.row, expected_rows - 1);
        assert_eq!(
            after.block[1].metadata.fold[0].end.position.row,
            expected_rows
        );
        assert!(after.block[0].metadata.fold[0].closed);
        assert!(!after.block[1].metadata.fold[0].closed);
        assert!(document.edit(0..2, after.block).unwrap().is_none());
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

#[test]
fn whole_block_and_empty_region_edits_preserve_trailing_rows() {
    for rows in [vec![], vec![""], vec!["first", ""], vec!["", "last"]] {
        let mut original = block();
        original.metadata = BlockMetadata::default();
        original.metadata.editable_region.push(EditableRegion {
            id: RegionId("body".into()),
            revision: RegionRevision(3),
            sequence: EditSequence(0),
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition { row: 2, column: 0 },
            },
        });
        let mut document =
            BufferDocument::new(DocumentId("document".into()), vec![original]).unwrap();
        let before = document.snapshot();
        let LocalEditResult::Accepted { patch, .. } =
            document.accept_local_edit(edit(1, 3, &rows)).unwrap()
        else {
            panic!("not accepted")
        };
        assert_eq!(
            apply_snapshot_reference(&before, &patch).unwrap(),
            document.snapshot()
        );
        let snapshot = document.snapshot();
        let current = &snapshot.block[0];
        let end = current.metadata.editable_region[0].range.end;
        let mut accepted = String::new();
        for row in 0..end.row {
            accepted.push_str(current.text.row(row).unwrap());
            accepted.push('\n');
        }
        if end.column > 0 {
            accepted.push_str(&current.text.row(end.row).unwrap()[..end.column]);
        }
        assert_eq!(accepted, rows.join("\n"));
        let before = document.snapshot();
        let LocalEditResult::Accepted { patch, .. } = document
            .accept_local_edit(edit(2, 4, &["replacement"]))
            .unwrap()
        else {
            panic!("not accepted")
        };
        assert_eq!(
            apply_snapshot_reference(&before, &patch).unwrap(),
            document.snapshot()
        );
        assert_eq!(
            document.snapshot().block[0].text.wire_rows(),
            ["replacement"]
        );
    }
}

#[test]
fn exhausted_region_revision_rejects_without_consuming_sequence() {
    let mut original = block();
    original.metadata.editable_region[0].revision = RegionRevision(forge_buffer::MAX_COUNTER);
    let mut document = BufferDocument::new(DocumentId("document".into()), vec![original]).unwrap();
    let before = document.snapshot();
    assert!(
        document
            .accept_local_edit(edit(1, forge_buffer::MAX_COUNTER, &["new"]))
            .is_err()
    );
    assert_eq!(document.snapshot(), before);
    document.edit(0..1, vec![block()]).unwrap();
    assert!(matches!(
        document.accept_local_edit(edit(1, 3, &["new"])).unwrap(),
        LocalEditResult::Accepted { .. }
    ));
}

#[test]
fn independent_regions_accept_out_of_order_document_sequences() {
    let mut document = BufferDocument::new(DocumentId("document".into()), vec![block()]).unwrap();
    let mut other = edit(10, 0, &["second region"]);
    other.region = RegionId("other".into());
    assert!(matches!(
        document.accept_local_edit(other).unwrap(),
        LocalEditResult::Accepted { .. }
    ));
    assert!(matches!(
        document
            .accept_local_edit(edit(9, 3, &["first region"]))
            .unwrap(),
        LocalEditResult::Accepted { .. }
    ));
    assert_eq!(
        document.snapshot().block[0].text.wire_rows(),
        ["prefix first region suffix", "second region"]
    );
}

#[test]
fn prepared_edit_can_abort_then_commit_without_replacing_unrelated_storage() {
    let mut block_list = vec![block()];
    for index in 0..1000 {
        block_list.push(BufferBlock {
            id: BlockId(format!("unrelated:{index}")),
            text: BufferText::from_rows([format!("unrelated source {index}")]).unwrap(),
            metadata: BlockMetadata::default(),
        });
    }
    let mut document = BufferDocument::new(DocumentId("document".into()), block_list).unwrap();
    let untouched = BlockId("unrelated:999".into());
    let storage = document
        .block(&untouched)
        .unwrap()
        .text
        .row(0)
        .unwrap()
        .as_ptr();
    let before = document.snapshot();
    {
        let prepared = document
            .prepare_local_edit(edit(1, 3, &["candidate"]))
            .unwrap();
        assert!(matches!(prepared, LocalEditPreparation::Prepared(_)));
    }
    assert_eq!(document.snapshot(), before);
    let result = match document
        .prepare_local_edit(edit(1, 3, &["accepted λ", "tail"]))
        .unwrap()
    {
        LocalEditPreparation::Prepared(prepared) => prepared.commit(),
        LocalEditPreparation::Rejected(_) => panic!("aborted preparation consumed the edit"),
    };
    let LocalEditResult::Accepted {
        acknowledgement,
        patch,
    } = result
    else {
        panic!("prepared edit did not commit")
    };
    assert_eq!(acknowledgement.sequence, EditSequence(1));
    assert_eq!(acknowledgement.revision, RegionRevision(4));
    assert_eq!(patch.metadata_edit.len(), 1);
    assert_eq!(
        document
            .block(&untouched)
            .unwrap()
            .text
            .row(0)
            .unwrap()
            .as_ptr(),
        storage
    );
    assert_eq!(
        apply_snapshot_reference(&before, &patch).unwrap(),
        document.snapshot()
    );
}
