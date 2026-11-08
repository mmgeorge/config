mod support;
use support::retain;

use forge_diff::body::BodyKind;
use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayState};
use forge_diff::raw::{compute_hunks, patch_body};
use forge_diff::source::{Representation, SourcePair, SourceVersion};

#[test]
fn full_file_preview_delivers_every_line_beyond_former_cutoff_without_losing_raw_actions() {
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
                let raw = retain(compute_hunks(SourcePair {
                    old: SourceVersion::new(old, Representation::GitCanonical).unwrap(),
                    new: SourceVersion::new(new, Representation::GitCanonical).unwrap(),
                }));
                let original = raw.hunks().to_vec();
                let mut cursor = DisplayCursor::new(raw.clone(), 3);
                {
                    let mut delivered = 0;
                    for iteration in 0..10 {
                        let chunk = cursor.next_rows(ChunkLimits::default());
                        delivered += chunk.rows.len();
                        if chunk.state == DisplayState::Complete {
                            break;
                        }
                        if line_count > 0 {
                            assert!(
                                !patch_body(&raw, raw.hunks()[0].id)
                                    .unwrap()
                                    .bytes
                                    .is_empty()
                            );
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
