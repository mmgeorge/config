use forge_buffer::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use forge_review::edit::{EditBudget, EditError, EditLimits, EditStore, RegionEdit, SaveOutcome};

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
    let budget = EditBudget::default();
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
    let mut store = store(&EditBudget::default());
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
    let mut store = store(&EditBudget::default());
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
    let mut store = store(&EditBudget::default());
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
    let mut store = store(&EditBudget::default());
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
fn refresh_invalidates_old_region_revision_and_refuses_dirty_text() {
    let mut store = store(&EditBudget::default());
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
fn explicit_capture_validates_all_fields_before_adopting_any() {
    let budget = EditBudget::default();
    let mut store = store(&budget);
    let title = RegionId("title".into());
    store
        .insert(title.clone(), RegionRevision(0), "original title".into())
        .unwrap();
    let body = RegionEdit {
        document: DocumentId("review".into()),
        region: region(),
        base: RegionRevision(0),
        sequence: EditSequence(1),
        text: "captured body".into(),
    };
    let invalid_title = RegionEdit {
        document: DocumentId("review".into()),
        region: title.clone(),
        base: RegionRevision(9),
        sequence: EditSequence(2),
        text: "captured title".into(),
    };
    let retained = budget.retained_bytes();
    assert!(
        store
            .prepare_capture(vec![copy_capture(&body), copy_capture(&invalid_title)], &[])
            .is_err()
    );
    assert_eq!(store.snapshot(&region()).unwrap().text, "old");
    assert_eq!(store.snapshot(&title).unwrap().text, "original title");
    assert_eq!(budget.retained_bytes(), retained);
    let valid_title = RegionEdit {
        base: RegionRevision(0),
        ..invalid_title
    };
    store
        .prepare_capture(vec![copy_capture(&body), copy_capture(&valid_title)], &[])
        .unwrap()
        .commit();
    assert_eq!(store.snapshot(&region()).unwrap().text, "captured body");
    assert_eq!(store.snapshot(&title).unwrap().text, "captured title");
    let retained = budget.retained_bytes();
    store
        .prepare_capture(vec![copy_capture(&body), valid_title], &[])
        .unwrap()
        .commit();
    assert_eq!(
        store.snapshot(&region()).unwrap().revision,
        RegionRevision(1)
    );
    assert_eq!(budget.retained_bytes(), retained);
    assert!(
        store
            .prepare_capture(
                vec![RegionEdit {
                    document: DocumentId("foreign".into()),
                    ..copy_capture(&body)
                }],
                &[]
            )
            .is_err()
    );
    assert!(
        store
            .prepare_capture(vec![copy_capture(&body), body], &[])
            .is_err()
    );
    assert_eq!(
        store.snapshot(&region()).unwrap().revision,
        RegionRevision(1)
    );
}

#[test]
fn merge_conflict_retains_replacement_versions() {
    use forge_review::edit::{ConflictResolution, MergeOutcome};
    let budget = EditBudget::default();
    let mut store = store(&budget);
    change(&mut store, "local");
    assert_eq!(
        store.merge(&region(), "remote".into()).unwrap(),
        MergeOutcome::Conflict
    );
    assert_eq!(budget.retained_bytes(), 14);
    assert_eq!(
        store.merge(&region(), "replacement".into()).unwrap(),
        MergeOutcome::Conflict
    );
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("local", "old", Some("replacement"))
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
    assert_eq!(budget.retained_bytes(), "replacement".len());
    let snapshot = store.snapshot(&region()).unwrap();
    assert_eq!(
        (snapshot.text, snapshot.baseline, snapshot.remote),
        ("replacement", "replacement", None)
    );
    assert!(!snapshot.dirty);
    drop(store);
    assert_eq!(budget.retained_bytes(), 0);
}

fn copy_capture(edit: &RegionEdit) -> RegionEdit {
    serde_json::from_value(serde_json::to_value(edit).unwrap()).unwrap()
}
