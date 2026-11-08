use forge_diff::{
    edit::{EditDirection, EditSelection},
    raw::compute_hunks,
    source::{Representation, SourcePair, SourceVersion},
};

#[test]
fn selected_edits_preserve_output_beyond_former_source_limit() {
    let bytes = b"retained\n".repeat(1_000_000);
    let diff = compute_hunks(SourcePair {
        old: SourceVersion::new(Vec::new(), Representation::Raw).unwrap(),
        new: SourceVersion::new(bytes.clone(), Representation::Raw).unwrap(),
    });
    let selected = diff.hunks().iter().map(|hunk| hunk.id).collect::<Vec<_>>();
    let applied = EditSelection::capture(&diff, &selected, EditDirection::Forward)
        .unwrap()
        .apply()
        .unwrap();
    assert_eq!(applied.after().bytes(), bytes);
}
