use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use forge_review::edit::{EditBudget, EditError, EditLimits, EditStore, RegionEdit, SaveOutcome};

#[test]
fn layout_change_does_not_invalidate_region_edit() {
    use forge_buffer::block::{
        BlockMetadata, BufferBlock, EditableRegion, TextPosition, TextRange,
    };
    use forge_buffer::document::BufferDocument;
    use forge_buffer::editable::{LocalEdit, LocalEditResult};
    use forge_buffer::identity::BlockId;
    use forge_buffer::text::BufferText;

    let mut store = store(&EditBudget::new(1024).unwrap());
    let body = BufferBlock {
        id: BlockId("body".into()),
        text: BufferText::from_rows(["old"]).unwrap(),
        metadata: BlockMetadata {
            editable_region: vec![EditableRegion {
                id: region(),
                revision: RegionRevision(0),
                sequence: EditSequence(0),
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition { row: 0, column: 3 },
                },
            }],
            ..BlockMetadata::default()
        },
    };
    let mut document = BufferDocument::new(DocumentId("review".into()), vec![body]).unwrap();
    document
        .edit(
            0..0,
            vec![BufferBlock {
                id: BlockId("header".into()),
                text: BufferText::from_rows(["generated", "header"]).unwrap(),
                metadata: BlockMetadata::default(),
            }],
        )
        .unwrap();
    let text = "λ\n  body\n";
    let acknowledgement = store
        .accept(RegionEdit {
            document: DocumentId("review".into()),
            region: region(),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: text.into(),
        })
        .unwrap();
    let LocalEditResult::Accepted {
        acknowledgement: document_acknowledgement,
        ..
    } = document
        .accept_local_edit(LocalEdit {
            document: DocumentId("review".into()),
            region: region(),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: BufferText::from_rows(text.split('\n')).unwrap(),
        })
        .unwrap()
    else {
        panic!("region edit rejected after layout change")
    };
    assert_eq!(acknowledgement, document_acknowledgement);
    assert_eq!(
        document.snapshot().block[1].text.wire_rows().join("\n"),
        store.snapshot(&region()).unwrap().text
    );
    assert_eq!(document.snapshot().revision.0, 2);
    assert_eq!(store.snapshot(&region()).unwrap().revision.0, 1);
}

fn store(budget: &EditBudget) -> EditStore {
    let mut store = EditStore::new(
        DocumentId("review".into()),
        EditLimits::default(),
        budget.clone(),
    )
    .unwrap();
    store
        .insert(region(), RegionRevision(0), "old".into())
        .unwrap();
    store
}

fn region() -> RegionId {
    RegionId("body".into())
}

fn change(store: &mut EditStore, text: &str) {
    let snapshot = store.snapshot(&region()).unwrap();
    let edit = RegionEdit {
        document: DocumentId("review".into()),
        region: region(),
        base: snapshot.revision,
        sequence: snapshot.sequence.next().unwrap(),
        text: text.into(),
    };
    store.accept(edit).unwrap();
}

#[test]
fn older_save_preserves_newer_local_text() {
    let budget = EditBudget::new(1024).unwrap();
    let mut store = store(&budget);
    change(&mut store, "submitted");
    let save = store.begin_save(&region()).unwrap().unwrap();
    change(&mut store, "newer");
    let result = store.complete_save(&save, SaveOutcome::Confirmed).unwrap();
    assert!(result.dirty);
    assert_eq!(result.current_sequence, EditSequence(2));
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!((snapshot.text, snapshot.baseline), ("newer", "submitted"));
    assert_eq!(save.text(), "submitted");
    assert_eq!(save.revision(), RegionRevision(1));
    assert_eq!(save.sequence(), EditSequence(1));
    assert_eq!(save.region(), &region());
    assert_eq!(
        store.complete_save(&save, SaveOutcome::Confirmed),
        Err(EditError::UnknownSave)
    );
}

#[test]
fn out_of_order_confirmation_cannot_roll_back_baseline() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    change(&mut store, "first");
    let first = store.begin_save(&region()).unwrap().unwrap();
    change(&mut store, "second");
    let second = store.begin_save(&region()).unwrap().unwrap();
    assert!(
        !store
            .complete_save(&second, SaveOutcome::Confirmed)
            .unwrap()
            .dirty
    );
    assert!(
        store
            .complete_save(&first, SaveOutcome::Confirmed)
            .unwrap()
            .superseded
    );
    assert_eq!(store.snapshot(&region()).unwrap().baseline, "second");
}

#[test]
fn local_revert_still_saves_after_pending_write() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    change(&mut store, "first");
    let first = store.begin_save(&region()).unwrap().unwrap();
    change(&mut store, "old");
    assert!(!store.snapshot(&region()).unwrap().dirty);
    let revert = store.begin_save(&region()).unwrap().unwrap();
    assert!(
        store
            .complete_save(&first, SaveOutcome::Confirmed)
            .unwrap()
            .dirty
    );
    assert!(
        !store
            .complete_save(&revert, SaveOutcome::Confirmed)
            .unwrap()
            .dirty
    );
    assert!(store.begin_save(&region()).unwrap().is_none());
}

#[test]
fn uncertainty_requires_observation_and_preserves_local_edits() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    change(&mut store, "submitted");
    let save = store.begin_save(&region()).unwrap().unwrap();
    assert!(
        store
            .complete_save(&save, SaveOutcome::Uncertain)
            .unwrap()
            .uncertain
    );
    change(&mut store, "newer");
    assert!(matches!(
        store.begin_save(&region()),
        Err(EditError::UncertainSave)
    ));
    assert_eq!(store.remove(&region()), Err(EditError::DirtyRegion));
    assert!(
        store
            .reconcile_save(&save, "observed".into())
            .unwrap()
            .dirty
    );
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!((snapshot.text, snapshot.baseline), ("newer", "observed"));
    assert!(!snapshot.uncertain);
    assert_eq!(snapshot.pending_saves, 0);
}

#[test]
fn late_uncertainty_reconciliation_cannot_replace_newer_confirmation() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    change(&mut store, "first");
    let first = store.begin_save(&region()).unwrap().unwrap();
    change(&mut store, "second");
    let second = store.begin_save(&region()).unwrap().unwrap();
    store.complete_save(&first, SaveOutcome::Uncertain).unwrap();
    store
        .complete_save(&second, SaveOutcome::Confirmed)
        .unwrap();
    assert!(
        store
            .reconcile_save(&first, "first".into())
            .unwrap()
            .superseded
    );
    assert_eq!(store.snapshot(&region()).unwrap().baseline, "second");
}

#[test]
fn rejected_save_keeps_baseline_and_releases_admission() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    change(&mut store, "submitted");
    let save = store.begin_save(&region()).unwrap().unwrap();
    assert!(matches!(
        store.begin_save(&region()),
        Err(EditError::AlreadySaving)
    ));
    change(&mut store, "newer");
    assert!(
        store
            .complete_save(&save, SaveOutcome::Rejected)
            .unwrap()
            .dirty
    );
    assert_eq!(store.snapshot(&region()).unwrap().baseline, "old");
    assert!(store.begin_save(&region()).unwrap().is_some());
}

#[test]
fn submissions_keep_budget_after_store_drop_and_reject_foreign_owner() {
    let budget = EditBudget::new(9).unwrap();
    let mut owner = store(&budget);
    change(&mut owner, "new");
    let save = owner.begin_save(&region()).unwrap().unwrap();
    let mut other = store(&budget);
    assert_eq!(budget.retained_bytes(), 9);
    assert_eq!(
        other.complete_save(&save, SaveOutcome::Confirmed),
        Err(EditError::WrongOwner)
    );
    drop(owner);
    assert_eq!(budget.retained_bytes(), 6);
    drop(other);
    assert_eq!(budget.retained_bytes(), 3);
    drop(save);
    assert_eq!(budget.retained_bytes(), 0);
}

#[test]
fn invalid_or_over_budget_edit_is_atomic() {
    let budget = EditBudget::new(3).unwrap();
    let mut store = store(&budget);
    let make = |base, sequence, text: &str| RegionEdit {
        document: DocumentId("review".into()),
        region: region(),
        base: RegionRevision(base),
        sequence: EditSequence(sequence),
        text: text.into(),
    };
    assert!(matches!(
        store.accept(make(0, 1, "new")),
        Err(EditError::Capacity(_))
    ));
    assert!(matches!(
        store.accept(make(1, 1, "old")),
        Err(EditError::Conflict { .. })
    ));
    assert!(matches!(
        store.accept(make(0, 0, "old")),
        Err(EditError::Invalid(_))
    ));
    assert!(matches!(
        store.accept(make(0, 1, "bad\0")),
        Err(EditError::Invalid(_))
    ));
    assert_eq!(
        store.snapshot(&region()).unwrap().revision,
        RegionRevision(0)
    );
    assert_eq!(budget.retained_bytes(), 3);
    store.accept(make(0, 1, "old")).unwrap();
    assert!(matches!(
        store.accept(make(1, 1, "old")),
        Err(EditError::StaleSequence { .. })
    ));
    assert_eq!(budget.retained_bytes(), 3);
}

#[test]
fn refresh_invalidates_old_region_revision_and_refuses_dirty_text() {
    let mut store = store(&EditBudget::new(1024).unwrap());
    assert_eq!(
        store
            .refresh(&region(), RegionRevision(0), "remote".into())
            .unwrap(),
        RegionRevision(1)
    );
    let stale = RegionEdit {
        document: DocumentId("review".into()),
        region: region(),
        base: RegionRevision(0),
        sequence: EditSequence(1),
        text: "stale".into(),
    };
    assert!(matches!(
        store.accept(stale),
        Err(EditError::Conflict { .. })
    ));
    change(&mut store, "local");
    assert!(matches!(
        store.refresh(&region(), RegionRevision(2), "remote".into()),
        Err(EditError::Conflict { .. })
    ));
    assert_eq!(store.snapshot(&region()).unwrap().text, "local");
}

#[test]
fn cardinality_and_retired_identity_limits_preserve_fields() {
    let limits = EditLimits {
        active_fields: 1,
        region_lifetimes: 2,
        pending_saves: 1,
        field_bytes: 3,
    };
    let mut store = EditStore::new(
        DocumentId("review".into()),
        limits,
        EditBudget::new(32).unwrap(),
    )
    .unwrap();
    store
        .insert(region(), RegionRevision(0), "old".into())
        .unwrap();
    let other = RegionId("other".into());
    assert!(matches!(
        store.insert(other.clone(), RegionRevision(0), "".into()),
        Err(EditError::Capacity(_))
    ));
    change(&mut store, "one");
    let first = store.begin_save(&region()).unwrap().unwrap();
    change(&mut store, "two");
    assert!(matches!(
        store.begin_save(&region()),
        Err(EditError::Capacity(_))
    ));
    store.complete_save(&first, SaveOutcome::Rejected).unwrap();
    change(&mut store, "old");
    store.remove(&region()).unwrap();
    assert!(matches!(
        store.insert(region(), RegionRevision(0), "".into()),
        Err(EditError::Invalid(_))
    ));
    store
        .insert(other.clone(), RegionRevision(0), "".into())
        .unwrap();
    store.remove(&other).unwrap();
    assert!(matches!(
        store.insert(RegionId("third".into()), RegionRevision(0), "".into()),
        Err(EditError::Capacity(_))
    ));
}

#[test]
fn region_counter_exhaustion_leaves_text_unchanged() {
    let mut store = EditStore::new(
        DocumentId("review".into()),
        EditLimits::default(),
        EditBudget::new(32).unwrap(),
    )
    .unwrap();
    store
        .insert(
            region(),
            RegionRevision(forge_buffer::MAX_COUNTER),
            "old".into(),
        )
        .unwrap();
    let result = store.accept(RegionEdit {
        document: DocumentId("review".into()),
        region: region(),
        base: RegionRevision(forge_buffer::MAX_COUNTER),
        sequence: EditSequence(1),
        text: "new".into(),
    });
    assert!(matches!(result, Err(EditError::Contract(_))));
    assert_eq!(store.snapshot(&region()).unwrap().text, "old");
}

#[test]
fn wire_edit_rejects_layout_revision_and_unknown_fields() {
    let edit = r#"{"document":"review","region":"body","base":0,"sequence":1,"text":"λ\n"}"#;
    let decoded: RegionEdit = serde_json::from_str(edit).unwrap();
    assert_eq!(decoded.text, "λ\n");
    assert!(
        serde_json::from_str::<RegionEdit>(
            &edit.replace("\"base\":0", "\"base\":0,\"layout_revision\":9")
        )
        .is_err()
    );
}

#[test]
fn merge_conflict_retains_all_versions_and_rejects_over_budget_replacement() {
    use forge_review::edit::{ConflictResolution, MergeOutcome};
    let budget = EditBudget::new(14).unwrap();
    let mut store = store(&budget);
    change(&mut store, "local");
    assert_eq!(
        store.merge(&region(), "remote".into()).unwrap(),
        MergeOutcome::Conflict
    );
    assert_eq!(budget.retained_bytes(), 14);
    assert!(matches!(
        store.merge(&region(), "replacement".into()),
        Err(EditError::Capacity(_))
    ));
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("local", "old", Some("remote"))
    );
    let revision = snapshot.revision;
    assert!(matches!(
        store.resolve(&region(), RegionRevision(0), ConflictResolution::TakeRemote),
        Err(EditError::Conflict { .. })
    ));
    assert_eq!(
        store
            .resolve(&region(), revision, ConflictResolution::TakeRemote)
            .unwrap(),
        RegionRevision(2)
    );
    assert_eq!(budget.retained_bytes(), 6);
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("remote", "remote", None)
    );
    assert!(!snapshot.dirty);
    drop(store);
    assert_eq!(budget.retained_bytes(), 0);
}

#[test]
fn matching_local_edit_resolves_remote_conflict_without_duplicate_retention() {
    use forge_review::edit::MergeOutcome;
    let budget = EditBudget::new(14).unwrap();
    let mut store = store(&budget);
    change(&mut store, "local");
    assert_eq!(
        store.merge(&region(), "remote".into()).unwrap(),
        MergeOutcome::Conflict
    );
    change(&mut store, "remote");
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("remote", "remote", None)
    );
    assert_eq!(snapshot.revision, RegionRevision(2));
    assert_eq!(snapshot.sequence, EditSequence(2));
    assert!(!snapshot.dirty);
    assert_eq!(budget.retained_bytes(), 6);
}

#[test]
fn remote_reversion_clears_conflict_and_preserves_local_draft() {
    use forge_review::edit::MergeOutcome;
    let budget = EditBudget::new(14).unwrap();
    let mut store = store(&budget);
    change(&mut store, "local");
    store.merge(&region(), "remote".into()).unwrap();
    assert_eq!(
        store.merge(&region(), "old".into()).unwrap(),
        MergeOutcome::Unchanged
    );
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("local", "old", None)
    );
    assert_eq!(budget.retained_bytes(), 8);
    assert!(store.begin_save(&region()).unwrap().is_some());
}

#[test]
fn merge_and_resolution_counter_exhaustion_preserve_retained_state() {
    use forge_review::edit::ConflictResolution;
    let budget = EditBudget::new(14).unwrap();
    let mut store = EditStore::new(
        DocumentId("review".into()),
        EditLimits::default(),
        budget.clone(),
    )
    .unwrap();
    store
        .insert(
            region(),
            RegionRevision(forge_buffer::MAX_COUNTER - 1),
            "old".into(),
        )
        .unwrap();
    change(&mut store, "local");
    store.merge(&region(), "remote".into()).unwrap();
    assert!(matches!(
        store.resolve(
            &region(),
            RegionRevision(forge_buffer::MAX_COUNTER),
            ConflictResolution::TakeRemote
        ),
        Err(EditError::Contract(_))
    ));
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("local", "old", Some("remote"))
    );
    assert_eq!(budget.retained_bytes(), 14);
    store.merge(&region(), "local".into()).unwrap();
    assert!(matches!(
        store.merge(&region(), "fresh".into()),
        Err(EditError::Contract(_))
    ));
    assert_eq!(store.snapshot(&region()).unwrap().text, "local");
    assert_eq!(budget.retained_bytes(), 5);
}
