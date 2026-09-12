mod support;
use support::retain;

use forge_diff::body::{BodyKind, BodyPolicy, UnavailableReason};
use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayState};
use forge_diff::raw::{compute_hunks, patch_body};
use forge_diff::source::{MAX_SOURCE_BYTES, Representation, SourcePair, SourceVersion};

#[test]
fn unknown_or_100_line_delta_never_enters_prewarm() {
    let policy = BodyPolicy::default();
    for kind in [BodyKind::Modified, BodyKind::Added] {
        assert!(policy.may_prewarm(kind, Some(99), Some((100, 100)), true));
        for delta in [None, Some(100), Some(101)] {
            assert!(!policy.may_prewarm(kind, delta, Some((100, 100)), true));
        }
        assert!(!policy.may_prewarm(kind, Some(1), None, true));
        assert!(!policy.may_prewarm(kind, Some(1), Some((100, 100)), false));
        assert!(!policy.may_prewarm(kind, Some(99), Some((MAX_SOURCE_BYTES + 1, 100)), true));
        assert!(!policy.may_prewarm(kind, Some(99), Some((100, MAX_SOURCE_BYTES + 1)), true));
        assert!(policy.may_prewarm(
            kind,
            Some(1),
            Some((MAX_SOURCE_BYTES, MAX_SOURCE_BYTES)),
            true
        ));
    }
    assert!(!policy.may_prewarm(BodyKind::Deleted, Some(0), Some((0, 0)), true));
}

#[test]
fn permissive_settings_cannot_raise_the_plan_limits() {
    let policy = BodyPolicy {
        full_file_lines: usize::MAX,
        prewarm_delta_exclusive: usize::MAX,
    };
    assert!(policy.may_preview_full_file(BodyKind::Added, 1001).is_err());
    assert!(!policy.may_prewarm(BodyKind::Added, Some(100), Some((0, 100)), true));
    let strict = BodyPolicy {
        full_file_lines: 10,
        prewarm_delta_exclusive: 5,
    };
    assert!(strict.may_preview_full_file(BodyKind::Deleted, 11).is_err());
    assert!(!strict.may_prewarm(BodyKind::Modified, Some(5), Some((10, 10)), true));
}

#[test]
fn full_file_preview_accepts_1000_lines_and_rejects_1001_without_losing_raw_actions() {
    for kind in [BodyKind::Added, BodyKind::Deleted] {
        for line_count in [0, 1000, 1001] {
            for final_newline in [false, true] {
                let mut content = b"line\n".repeat(line_count);
                if !final_newline && !content.is_empty() {
                    content.pop();
                }
                let (old, new) = if kind == BodyKind::Added {
                    (Vec::new(), content)
                } else {
                    (content, Vec::new())
                };
                let raw = retain(
                    compute_hunks(SourcePair {
                        old: SourceVersion::new(old, Representation::GitCanonical).unwrap(),
                        new: SourceVersion::new(new, Representation::GitCanonical).unwrap(),
                    })
                    .unwrap(),
                );
                let original = raw.hunks().to_vec();
                let mut cursor = DisplayCursor::new(raw.clone(), 3, kind);
                if line_count > 1000 {
                    let chunk = cursor.next_rows(ChunkLimits::default());
                    assert!(chunk.rows.is_empty());
                    assert_eq!(
                        chunk.state,
                        DisplayState::Unavailable(UnavailableReason::FullFileLines {
                            lines: 1001,
                            limit: 1000
                        })
                    );
                    assert_eq!(cursor.next_rows(ChunkLimits::default()).state, chunk.state);
                    assert!(
                        !patch_body(&raw, raw.hunks()[0].id)
                            .unwrap()
                            .bytes
                            .is_empty()
                    );
                } else {
                    let mut delivered = 0;
                    for iteration in 0..10 {
                        let chunk = cursor.next_rows(ChunkLimits::default());
                        delivered += chunk.rows.len();
                        if chunk.state == DisplayState::Complete {
                            break;
                        }
                        assert_eq!(chunk.state, DisplayState::More);
                        assert!(iteration < 9);
                    }
                    assert_eq!(delivered, line_count);
                }
                assert_eq!(raw.hunks(), original);
            }
        }
    }
}

#[test]
fn modified_file_expansion_is_independent_of_the_full_file_gate() {
    let raw = retain(
        compute_hunks(SourcePair {
            old: SourceVersion::new(b"old\n".repeat(1200), Representation::Raw).unwrap(),
            new: SourceVersion::new(b"new\n".repeat(1200), Representation::Raw).unwrap(),
        })
        .unwrap(),
    );
    let mut cursor = DisplayCursor::new(raw, 3, BodyKind::Modified);
    let chunk = cursor.next_rows(ChunkLimits::default());
    assert_eq!(chunk.rows.len(), 256);
    assert_eq!(chunk.state, DisplayState::More);
}
