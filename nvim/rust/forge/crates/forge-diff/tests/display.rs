mod support;
use support::retain;

use forge_diff::cache::AnalysisHandle;
use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayRow, DisplayState, group_hunks};
use forge_diff::raw::compute_hunks;
use forge_diff::source::{Representation, SourcePair, SourceVersion};

fn diff(old: &str, new: &str) -> AnalysisHandle {
    retain(compute_hunks(SourcePair {
        old: SourceVersion::new(old.as_bytes().to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(new.as_bytes().to_vec(), Representation::Raw).unwrap(),
    }))
}

fn drain(cursor: &mut DisplayCursor, limits: ChunkLimits) -> Vec<DisplayRow> {
    let mut rows = Vec::new();
    for _ in 0..10000 {
        let chunk = cursor.next_rows(limits);
        assert!(chunk.rows.len() <= limits.rows.min(256).min(limits.decorations));
        assert!(chunk.rows.len() == 1 || chunk.text_bytes <= limits.text_bytes.min(128 * 1024));
        assert!(chunk.decorations <= limits.decorations.min(8192));
        assert_eq!(
            chunk.decorations,
            chunk
                .rows
                .iter()
                .map(|row| row.emphasis.len() + 1)
                .sum::<usize>()
        );
        assert_eq!(
            chunk.text_bytes,
            chunk
                .rows
                .iter()
                .map(|row| row.text.len() + 1)
                .sum::<usize>()
        );
        rows.extend(chunk.rows);
        match chunk.state {
            DisplayState::Complete => return rows,
            DisplayState::More => {}
        }
    }
    panic!("cursor did not terminate");
}

#[test]
fn display_context_merges_without_duplicate_source_lines() {
    let source = diff("a\nb\nc\nd\ne\nf\ng\n", "a\nB\nc\nd\ne\nF\ng\n");
    assert_eq!(source.hunks().len(), 2);
    let group = group_hunks(&source, 2);
    assert_eq!(group.len(), 1);
    assert_eq!(group[0].raw_ids.len(), 2);
    let rows = drain(
        &mut DisplayCursor::new(source, 2),
        ChunkLimits {
            rows: 2,
            text_bytes: 10,
            decorations: 2,
        },
    );
    assert_eq!(
        rows.iter().map(|row| row.text.as_str()).collect::<Vec<_>>(),
        ["b", "B", "c", "d", "e", "f", "F"]
    );
    assert_eq!(
        rows.iter()
            .filter_map(|row| row.old_coordinate().map(|coordinate| coordinate.line))
            .collect::<Vec<_>>(),
        (1..6).collect::<Vec<_>>()
    );
    assert_eq!(
        rows.iter()
            .filter_map(|row| row.new_coordinate().map(|coordinate| coordinate.line))
            .collect::<Vec<_>>(),
        (1..6).collect::<Vec<_>>()
    );
}

#[test]
fn compact_context_preserves_changed_spans_across_insertions_and_deletions() {
    let fixture = [
        "",
        "a\n",
        "b\r\n",
        "a\nb\n",
        "b\na\n",
        "a\r\nb",
        "\n\na\n",
        "🙂\na\n",
    ];
    for old in fixture {
        for new in fixture {
            let source = diff(old, new);
            if source.hunks().is_empty() {
                continue;
            }
            let old_range = source.hunks().first().unwrap().old_lines.start
                ..source.hunks().last().unwrap().old_lines.end;
            let new_range = source.hunks().first().unwrap().new_lines.start
                ..source.hunks().last().unwrap().new_lines.end;
            let rows = drain(
                &mut DisplayCursor::new(source, usize::MAX),
                ChunkLimits {
                    rows: 1,
                    ..ChunkLimits::default()
                },
            );
            let old_lines = old.split_inclusive('\n').collect::<Vec<_>>();
            let new_lines = new.split_inclusive('\n').collect::<Vec<_>>();
            assert_eq!(
                rows.iter()
                    .filter_map(|row| row.old_coordinate().map(|coordinate| coordinate.line))
                    .collect::<Vec<_>>(),
                old_range.collect::<Vec<_>>()
            );
            assert_eq!(
                rows.iter()
                    .filter_map(|row| row.new_coordinate().map(|coordinate| coordinate.line))
                    .collect::<Vec<_>>(),
                new_range.collect::<Vec<_>>()
            );
            for row in rows {
                for (coordinate, lines) in [
                    (row.old_coordinate(), &old_lines),
                    (row.new_coordinate(), &new_lines),
                ] {
                    if let Some(coordinate) = coordinate {
                        let line = lines[coordinate.line];
                        let text = line
                            .strip_suffix('\n')
                            .map(|text| text.strip_suffix('\r').unwrap_or(text))
                            .unwrap_or(line);
                        assert_eq!(row.text, text);
                    }
                }
            }
        }
    }
}
