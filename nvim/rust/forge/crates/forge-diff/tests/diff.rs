use forge_diff::raw::{PatchError, RawDiff, compute_hunks, patch_body, synthetic_hunk};
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn pair(old: &[u8], new: &[u8], representation: Representation) -> SourcePair {
    SourcePair {
        old: SourceVersion::new(old.to_vec(), representation).unwrap(),
        new: SourceVersion::new(new.to_vec(), representation).unwrap(),
    }
}

fn reconstruct(diff: &RawDiff, reverse: bool) -> Vec<u8> {
    let source = if reverse {
        &diff.source().new
    } else {
        &diff.source().old
    };
    let target = if reverse {
        &diff.source().old
    } else {
        &diff.source().new
    };
    let mut output = source.bytes().to_vec();
    for hunk in diff.hunks().iter().rev() {
        let (removed, inserted) = if reverse {
            (&hunk.new_bytes, &hunk.old_bytes)
        } else {
            (&hunk.old_bytes, &hunk.new_bytes)
        };
        output.splice(
            removed.clone(),
            target.bytes()[inserted.clone()].iter().copied(),
        );
    }
    output
}

#[test]
fn applying_and_reversing_raw_hunks_reconstruct_exact_canonical_bytes() {
    let mut fixture = vec![Vec::new()];
    for first in [b"a\n".as_slice(), b"a\r\n", "🙂\n".as_bytes(), b"\n"] {
        for last in [b"".as_slice(), b"a", b"b\r", b"b\n", b"b\r\n"] {
            fixture.push([first, last].concat());
        }
    }
    for old in &fixture {
        for new in &fixture {
            let diff = compute_hunks(pair(old, new, Representation::GitCanonical)).unwrap();
            assert_eq!(
                forge_diff::raw::compute_counts(diff.source()),
                diff.line_counts()
            );
            assert_eq!(reconstruct(&diff, false), *new);
            assert_eq!(reconstruct(&diff, true), *old);
            for adjacent in diff.hunks().windows(2) {
                assert!(adjacent[0].old_bytes.end <= adjacent[1].old_bytes.start);
                assert!(adjacent[0].new_bytes.end <= adjacent[1].new_bytes.start);
            }
        }
    }
}

#[test]
fn added_and_deleted_sources_have_exact_synthetic_ranges() {
    for (old, new) in [
        (b"".as_slice(), b"a\r\nlast".as_slice()),
        (b"a\r\nlast", b""),
    ] {
        let source = pair(old, new, Representation::GitCanonical);
        let synthetic = synthetic_hunk(source.clone()).unwrap();
        assert_eq!(synthetic.hunks().len(), 1);
        assert_eq!(synthetic.hunks()[0].old_bytes, 0..old.len());
        assert_eq!(synthetic.hunks()[0].new_bytes, 0..new.len());
        assert_eq!(synthetic.hunks(), compute_hunks(source).unwrap().hunks());
        assert_eq!(reconstruct(&synthetic, false), new);
    }
    assert!(
        synthetic_hunk(pair(b"", b"", Representation::Raw))
            .unwrap()
            .hunks()
            .is_empty()
    );
    assert!(synthetic_hunk(pair(b"old", b"new", Representation::Raw)).is_none());
}

#[test]
fn patch_bodies_preserve_crlf_and_missing_final_newline_markers() {
    let diff = compute_hunks(pair(
        b"before\r\nlast",
        b"after\r\nfinal",
        Representation::GitCanonical,
    ))
    .unwrap();
    assert_eq!(diff.hunks().len(), 1);
    assert_eq!(patch_body(&diff, diff.hunks()[0].id).unwrap().bytes,
        b"-before\r\n-last\n\\ No newline at end of file\n+after\r\n+final\n\\ No newline at end of file\n");
    let newline_only =
        compute_hunks(pair(b"same", b"same\n", Representation::GitCanonical)).unwrap();
    assert_eq!(newline_only.hunks().len(), 1);
    assert_eq!(reconstruct(&newline_only, false), b"same\n");
}

#[test]
fn patch_body_rejects_foreign_hunks_and_noncanonical_sources() {
    let first = compute_hunks(pair(b"old", b"new", Representation::GitCanonical)).unwrap();
    let other = compute_hunks(pair(b"old", b"different", Representation::GitCanonical)).unwrap();
    assert_eq!(
        patch_body(&other, first.hunks()[0].id),
        Err(PatchError::UnknownHunk)
    );
    for representation in [Representation::Raw, Representation::DisplayOnly] {
        let diff = compute_hunks(pair(b"old", b"new", representation)).unwrap();
        assert_eq!(
            patch_body(&diff, diff.hunks()[0].id),
            Err(PatchError::NonCanonicalSource)
        );
        assert_ne!(diff.hunks()[0].id, first.hunks()[0].id);
    }
    let repeated = compute_hunks(pair(b"old", b"new", Representation::GitCanonical)).unwrap();
    assert_eq!(first.hunks(), repeated.hunks());
}

#[test]
fn raw_changes_do_not_truncate_at_preview_line_limits() {
    let new = b"added\n".repeat(1200);
    let diff = compute_hunks(pair(b"", &new, Representation::GitCanonical)).unwrap();
    assert_eq!(diff.hunks()[0].new_lines, 0..1200);
    assert_eq!(
        patch_body(&diff, diff.hunks()[0].id).unwrap().bytes,
        b"+added\n".repeat(1200)
    );
}
