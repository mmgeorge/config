use forge_diff::body::{BodyKind, UnavailableReason};
use forge_diff::intraline::FallbackReason;
mod support;
use support::retain;

use forge_diff::cache::AnalysisHandle;
use forge_diff::display::{
    ChunkLimits, DisplayCursor, DisplayRow, DisplayState, RowKind, group_hunks,
};
use forge_diff::raw::compute_hunks;
use forge_diff::source::{Representation, SourcePair, SourceSide, SourceVersion};

fn diff(old: &str, new: &str) -> AnalysisHandle {
    retain(
        compute_hunks(SourcePair {
            old: SourceVersion::new(old.as_bytes().to_vec(), Representation::Raw).unwrap(),
            new: SourceVersion::new(new.as_bytes().to_vec(), Representation::Raw).unwrap(),
        })
        .unwrap(),
    )
}

fn drain(cursor: &mut DisplayCursor, limits: ChunkLimits) -> Vec<DisplayRow> {
    let mut rows = Vec::new();
    for _ in 0..10000 {
        let chunk = cursor.next_rows(limits);
        assert!(chunk.rows.len() <= limits.rows.min(256).min(limits.decorations));
        assert!(chunk.text_bytes <= limits.text_bytes.min(128 * 1024));
        assert!(chunk.decorations <= limits.decorations.min(8192));
        assert_eq!(
            chunk.decorations,
            chunk.rows.iter().map(|row| row.emphasis.len() + 1).sum()
        );
        assert_eq!(
            chunk.text_bytes,
            chunk.rows.iter().map(|row| row.text.len() + 1).sum()
        );
        rows.extend(chunk.rows);
        match chunk.state {
            DisplayState::Complete => return rows,
            DisplayState::More => {}
            state => panic!("unexpected terminal state: {state:?}"),
        }
    }
    panic!("cursor did not terminate");
}

#[test]
fn first_giant_hunk_obeys_the_same_chunk_limit_as_later_hunks() {
    let source = diff(&"old\n".repeat(800), &"new\n".repeat(900));
    let identity = source.hunks()[0].id;
    let mut cursor = DisplayCursor::new(source, 3, BodyKind::Modified);
    let first = cursor.next_rows(ChunkLimits::default());
    assert_eq!(first.rows.len(), 256);
    assert_eq!(first.state, DisplayState::More);
    let mut rows = first.rows;
    rows.extend(drain(&mut cursor, ChunkLimits::default()));
    assert_eq!(rows.len(), 1700);
    assert!(rows.iter().all(|row| row.raw_id == Some(identity)));
    assert_eq!(
        rows.iter()
            .filter(|row| row.kind == RowKind::Removed)
            .count(),
        800
    );
    assert_eq!(
        rows.iter().filter(|row| row.kind == RowKind::Added).count(),
        900
    );
}

#[test]
fn display_context_merges_without_duplicate_source_lines() {
    let source = diff("a\nb\nc\nd\ne\nf\ng\n", "a\nB\nc\nd\ne\nF\ng\n");
    assert_eq!(source.hunks().len(), 2);
    let group = group_hunks(&source, 2);
    assert_eq!(group.len(), 1);
    assert_eq!(group[0].raw_ids.len(), 2);
    let rows = drain(
        &mut DisplayCursor::new(source, 2, BodyKind::Modified),
        ChunkLimits {
            rows: 2,
            text_bytes: 10,
            decorations: 2,
        },
    );
    assert_eq!(
        rows.iter().map(|row| row.text.as_str()).collect::<Vec<_>>(),
        ["a", "b", "B", "c", "d", "e", "f", "F", "g"]
    );
    assert_eq!(
        rows.iter()
            .filter_map(|row| row.old.map(|coordinate| coordinate.line))
            .collect::<Vec<_>>(),
        (0..7).collect::<Vec<_>>()
    );
    assert_eq!(
        rows.iter()
            .filter_map(|row| row.new.map(|coordinate| coordinate.line))
            .collect::<Vec<_>>(),
        (0..7).collect::<Vec<_>>()
    );
}

#[test]
fn group_identity_survives_one_row_deliveries_and_context_gaps() {
    let source = diff("a\nb\nc\nd\ne\nf\ng\n", "A\nb\nc\nd\ne\nf\nG\n");
    let mut cursor = DisplayCursor::new(source, 1, BodyKind::Modified);
    assert_eq!(cursor.group(0).unwrap().old_lines, 0..2);
    assert_eq!(cursor.group(1).unwrap().old_lines, 5..7);
    let rows = drain(
        &mut cursor,
        ChunkLimits {
            rows: 1,
            ..Default::default()
        },
    );
    assert_eq!(
        rows.iter().map(|row| row.group_index).collect::<Vec<_>>(),
        [0, 0, 0, 1, 1, 1]
    );
    assert!(cursor.group(2).is_none());
    for row in rows {
        let group = cursor.group(row.group_index).unwrap();
        if let Some(raw) = row.raw_id {
            assert!(group.raw_ids.contains(&raw));
        }
    }
}

#[test]
fn byte_and_decoration_limits_preserve_unemitted_rows() {
    let source = diff("", "é🙂\r\nsecond\nlast\r");
    let mut cursor = DisplayCursor::new(source, 0, BodyKind::Modified);
    let paused = cursor.next_rows(ChunkLimits {
        rows: 0,
        ..ChunkLimits::default()
    });
    assert!(paused.rows.is_empty());
    assert_eq!(paused.state, DisplayState::More);
    let rows = drain(
        &mut cursor,
        ChunkLimits {
            rows: 99,
            text_bytes: 7,
            decorations: 1,
        },
    );
    assert_eq!(
        rows.iter().map(|row| row.text.as_str()).collect::<Vec<_>>(),
        ["é🙂", "second", "last\r"]
    );
    assert!(rows.iter().all(|row| row.old.is_none()));
    assert_eq!(rows[2].new.unwrap().line, 2);
}

#[test]
fn one_very_long_line_is_unavailable_before_allocating_a_display_row() {
    let source = diff("", &"x".repeat(128 * 1024));
    let mut cursor = DisplayCursor::new(source, 0, BodyKind::Modified);
    let chunk = cursor.next_rows(ChunkLimits::default());
    assert!(chunk.rows.is_empty());
    assert_eq!(chunk.text_bytes, 0);
    assert!(
        matches!(chunk.state, DisplayState::Unavailable(UnavailableReason::LineBytes { coordinate, line_bytes: 131072, limit: 131072 }) if coordinate.side == SourceSide::New && coordinate.line == 0)
    );
    assert_eq!(cursor.next_rows(ChunkLimits::default()).state, chunk.state);
}

#[test]
fn separate_context_windows_skip_unchanged_gaps_and_keep_raw_targets() {
    let source = diff("a\nb\nc\nd\ne\nf\ng\nh\ni\n", "a\nB\nc\nd\ne\nf\ng\nH\ni\n");
    assert_eq!(group_hunks(&source, 1).len(), 2);
    let original = source.hunks().to_vec();
    let rows = drain(
        &mut DisplayCursor::new(source.clone(), 1, BodyKind::Modified),
        ChunkLimits::default(),
    );
    assert_eq!(
        rows.iter().map(|row| row.text.as_str()).collect::<Vec<_>>(),
        ["a", "b", "B", "c", "g", "h", "H", "i"]
    );
    assert_eq!(source.hunks(), original);
    let no_context = drain(
        &mut DisplayCursor::new(source, 0, BodyKind::Modified),
        ChunkLimits::default(),
    );
    assert_eq!(no_context.len(), 4);
}

#[test]
fn full_context_preserves_both_source_coordinate_sequences_across_insertions_and_deletions() {
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
            let rows = drain(
                &mut DisplayCursor::new(source, usize::MAX, BodyKind::Modified),
                ChunkLimits {
                    rows: 1,
                    ..ChunkLimits::default()
                },
            );
            let old_lines = old.split_inclusive('\n').collect::<Vec<_>>();
            let new_lines = new.split_inclusive('\n').collect::<Vec<_>>();
            assert_eq!(
                rows.iter()
                    .filter_map(|row| row.old.map(|coordinate| coordinate.line))
                    .collect::<Vec<_>>(),
                (0..old_lines.len()).collect::<Vec<_>>()
            );
            assert_eq!(
                rows.iter()
                    .filter_map(|row| row.new.map(|coordinate| coordinate.line))
                    .collect::<Vec<_>>(),
                (0..new_lines.len()).collect::<Vec<_>>()
            );
            for row in rows {
                for (coordinate, lines) in [(row.old, &old_lines), (row.new, &new_lines)] {
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

#[test]
fn emphasis_survives_batch_boundaries_with_exact_display_byte_ranges() {
    let source = diff("é🙂old終\r\n", "é🙂new終\r\n");
    let mut cursor = DisplayCursor::new(source, 0, BodyKind::Modified);
    let rows = drain(
        &mut cursor,
        ChunkLimits {
            decorations: 2,
            ..ChunkLimits::default()
        },
    );
    assert_eq!(rows.len(), 2);
    for row in &rows {
        assert_eq!(row.emphasis, vec![6..9]);
        assert_eq!(row.emphasis_fallback, None);
        assert!(!row.text.ends_with('\r'));
    }
    assert_eq!(&rows[0].text[rows[0].emphasis[0].clone()], "old");
    assert_eq!(&rows[1].text[rows[1].emphasis[0].clone()], "new");
}

#[test]
fn a_decoration_budget_that_cannot_fit_one_emphasized_row_uses_base_styles() {
    let mut cursor = DisplayCursor::new(diff("old\n", "new\n"), 0, BodyKind::Modified);
    let rows = drain(
        &mut cursor,
        ChunkLimits {
            decorations: 1,
            ..ChunkLimits::default()
        },
    );
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0].kind, RowKind::Removed);
    assert_eq!(rows[1].kind, RowKind::Added);
    assert!(rows.iter().all(|row| row.emphasis.is_empty()
        && row.emphasis_fallback == Some(FallbackReason::DecorationLimit)));
}

#[test]
fn rejected_intraline_work_preserves_the_entire_large_replacement() {
    let source = diff(&"old\n".repeat(65), &"new\n".repeat(65));
    let rows = drain(
        &mut DisplayCursor::new(source, 0, BodyKind::Modified),
        ChunkLimits::default(),
    );
    assert_eq!(rows.len(), 130);
    assert!(
        rows.iter().all(|row| row.emphasis.is_empty()
            && row.emphasis_fallback == Some(FallbackReason::PairLimit))
    );
}
