use forge_diff::raw::{LineAnchor, compute_anchored_hunks};
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn pair(old: &str, new: &str) -> SourcePair {
    SourcePair {
        old: SourceVersion::new(old.as_bytes().to_vec(), Representation::DisplayOnly).unwrap(),
        new: SourceVersion::new(new.as_bytes().to_vec(), Representation::DisplayOnly).unwrap(),
    }
}

#[test]
fn anchored_histogram_preserves_complete_bytes_and_absolute_ranges() {
    let old = "head\nfn main() {\n  old();\n}\nlast";
    let new = "head\nfn inserted() {\n  old();\n}\nfn main() {\n  new_λ();\n}\nlast";
    let diff = compute_anchored_hunks(
        pair(old, new),
        &[LineAnchor { old: 1, new: 4 }, LineAnchor { old: 4, new: 7 }],
    )
    .unwrap();
    let mut rebuilt = Vec::new();
    let mut previous = 0;
    for hunk in diff.hunks() {
        assert!(hunk.old_bytes.start >= previous);
        rebuilt.extend_from_slice(&old.as_bytes()[previous..hunk.old_bytes.start]);
        rebuilt.extend_from_slice(&new.as_bytes()[hunk.new_bytes.clone()]);
        previous = hunk.old_bytes.end;
    }
    rebuilt.extend_from_slice(&old.as_bytes()[previous..]);
    assert_eq!(rebuilt, new.as_bytes());
    assert_eq!(diff.hunks()[0].old_lines, 1..1);
    assert_eq!(diff.hunks()[0].new_lines, 1..4);
    assert_eq!(diff.hunks()[1].old_lines, 2..3);
    assert_eq!(diff.hunks()[1].new_lines, 5..6);
    assert!(compute_anchored_hunks(pair(old, new), &[LineAnchor { old: 99, new: 0 }]).is_err());
    assert!(
        compute_anchored_hunks(
            pair(old, new),
            &[LineAnchor { old: 2, new: 5 }, LineAnchor { old: 1, new: 6 },]
        )
        .is_err()
    );
}
