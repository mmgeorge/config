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
