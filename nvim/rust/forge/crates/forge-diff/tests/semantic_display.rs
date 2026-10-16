use std::{collections::BTreeSet, sync::Arc};

use forge_diff::{
    cache::AnalysisHandle,
    display::{ChunkLimits, DisplayCursor, DisplayRow, DisplayState, RowKind, group_hunks},
    raw::compute_hunks,
    source::{Representation, SourcePair, SourceVersion},
    syntax::{SyntaxEngine, SyntaxHandle, SyntaxLanguage, SyntaxRequest},
    workers::{AnalysisPool, PoolLimits, WorkPriority},
};

mod support;

async fn analysis(
    language: SyntaxLanguage,
    old: &str,
    new: &str,
) -> (AnalysisHandle, SyntaxHandle, SyntaxHandle) {
    let source = SourcePair {
        old: SourceVersion::new(old.as_bytes().to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(new.as_bytes().to_vec(), Representation::Raw).unwrap(),
    };
    let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
    })));
    let mut syntax = Vec::new();
    for source in [source.old.clone(), source.new.clone()] {
        syntax.push(
            engine
                .analyze(SyntaxRequest {
                    source,
                    language,
                    priority: WorkPriority::Foreground,
                    deadline: None,
                })
                .await
                .unwrap(),
        );
    }
    let new = syntax.pop().unwrap();
    let old = syntax.pop().unwrap();
    (support::retain(compute_hunks(source).unwrap()), old, new)
}

fn drain(cursor: &mut DisplayCursor, rows: usize) -> Vec<DisplayRow> {
    let mut result = Vec::new();
    for _ in 0..1000 {
        let chunk = cursor.next_rows(ChunkLimits {
            rows,
            ..Default::default()
        });
        assert!(chunk.rows.len() <= rows);
        result.extend(chunk.rows);
        match chunk.state {
            DisplayState::Complete => return result,
            DisplayState::More => {}
        }
    }
    panic!("display did not finish");
}

fn function(header: &str, binding: &str) -> Vec<String> {
    let mut source = vec![header.into()];
    source.extend((1..=44).map(|line| format!("    {binding} value_{line} = {line};")));
    source.push("    return value_44;".into());
    source.push("}".into());
    source
}

#[tokio::test]
async fn semantic_windows_merge_and_shared_boundaries_render_once_in_typescript_and_rust() {
    for (language, header, binding) in [
        (
            SyntaxLanguage::Typescript,
            "export function foo(): number {",
            "const",
        ),
        (
            SyntaxLanguage::Typescript,
            "export const foo = (): number => {",
            "const",
        ),
        (SyntaxLanguage::Rust, "pub fn foo() -> i32 {", "let"),
    ] {
        let original = function(header, binding);
        let mut modified = original.clone();
        for row in [8, 13, 22, 36] {
            modified[row] = format!("    {binding} value_{row} = {};", 900 + row);
        }
        let (diff, old, new) = analysis(
            language,
            &(original.join("\n") + "\n"),
            &(modified.join("\n") + "\n"),
        )
        .await;
        let raw = diff.hunks().to_vec();
        assert_eq!(raw.len(), 4);
        assert_eq!(group_hunks(&diff, 3).len(), 3);
        for compact in [false, true] {
            let make_cursor = || {
                let cursor = if compact {
                    DisplayCursor::compact(diff.clone(), 3)
                } else {
                    DisplayCursor::new(diff.clone(), 3)
                };
                cursor.with_syntax_context(Some(&old), Some(&new))
            };
            let mut cursor = make_cursor();
            assert_eq!(
                cursor.group(0).unwrap().raw_ids,
                raw[..3].iter().map(|hunk| hunk.id).collect::<Vec<_>>()
            );
            assert_eq!(cursor.group(1).unwrap().raw_ids, vec![raw[3].id]);
            assert!(cursor.group(2).is_none());
            assert_eq!(cursor.context_label(0), Some(header));
            assert_eq!(cursor.context_label(1), Some(header));
            let rows = drain(&mut cursor, 1);
            assert_eq!(rows, drain(&mut make_cursor(), 256));
            assert_eq!(rows.iter().filter(|row| row.text == header).count(), 1);
            assert_eq!(rows.iter().filter(|row| row.text == "}").count(), 1);
            for select in [|row: &DisplayRow| row.old, |row: &DisplayRow| row.new] {
                let coordinate: Vec<_> = rows
                    .iter()
                    .filter_map(|row| select(row).map(|coordinate| coordinate.line))
                    .collect();
                assert!(
                    coordinate.windows(2).all(|pair| pair[0] < pair[1]),
                    "duplicated or reordered context: {coordinate:?}"
                );
            }
            for row in &rows {
                if let Some(identity) = row.raw_id {
                    assert!(
                        cursor
                            .group(row.group_index)
                            .unwrap()
                            .raw_ids
                            .contains(&identity)
                    );
                }
            }
            let bridge = rows
                .iter()
                .find(|row| row.new.is_some_and(|coordinate| coordinate.line == 18))
                .unwrap();
            assert_eq!(bridge.kind, RowKind::Context);
            assert_eq!(bridge.group_index, 0);
            assert!(
                rows.iter()
                    .all(|row| row.new.is_none_or(|coordinate| coordinate.line != 29))
            );
            assert_eq!(diff.hunks(), raw);
        }
    }
}

#[tokio::test]
async fn semantic_bridge_accepts_six_hidden_lines_but_rejects_seven() {
    let original = function("function foo() {", "const");
    for (second, groups) in [(21, 1), (22, 2)] {
        let mut modified = original.clone();
        for row in [8, second] {
            modified[row] = format!("    const value_{row} = {};", 900 + row);
        }
        let (diff, old, new) = analysis(
            SyntaxLanguage::Typescript,
            &original.join("\n"),
            &modified.join("\n"),
        )
        .await;
        let cursor = DisplayCursor::compact(diff, 3)
            .with_syntax_context(Some(&old), Some(&new));
        assert!(cursor.group(groups - 1).is_some());
        assert!(
            cursor.group(groups).is_none(),
            "gap threshold at source row {second}"
        );
    }
}

#[tokio::test]
async fn semantic_bridge_does_not_join_unrelated_functions() {
    let mut original = vec!["function first() {".to_owned()];
    original.extend((1..=12).map(|line| format!("  const first_{line} = {line};")));
    original.extend(["}".into(), "function second() {".into()]);
    original.extend((1..=12).map(|line| format!("  const second_{line} = {line};")));
    original.push("}".into());
    let mut modified = original.clone();
    modified[9] = "  const first_9 = 900;".into();
    modified[19] = "  const second_5 = 500;".into();
    let (diff, old, new) = analysis(
        SyntaxLanguage::Typescript,
        &original.join("\n"),
        &modified.join("\n"),
    )
    .await;
    let mut cursor = DisplayCursor::compact(diff, 3)
        .with_syntax_context(Some(&old), Some(&new));
    assert!(cursor.group(1).is_some());
    assert!(cursor.group(2).is_none());
    let rows = drain(&mut cursor, 1);
    assert_eq!(
        rows.iter()
            .filter(|row| row.text == "function first() {")
            .count(),
        1
    );
    assert_eq!(
        rows.iter()
            .filter(|row| row.text == "function second() {")
            .count(),
        1
    );
}

#[tokio::test]
async fn nested_rust_methods_share_ancestor_boundaries_without_merging_sibling_scopes() {
    let mut original = vec!["impl Engine {".into()];
    for name in ["first", "second"] {
        original.push(format!("  fn {name}() {{"));
        original.extend((1..=20).map(|line| format!("    let {name}_{line} = {line};")));
        original.push("  }".into());
    }
    original.push("}".into());
    let mut modified = original.clone();
    for row in [8, 23 + 8] {
        modified[row] = "    let changed = 900;".into();
    }
    let (diff, old, new) = analysis(
        SyntaxLanguage::Rust,
        &original.join("\n"),
        &modified.join("\n"),
    )
    .await;
    let mut cursor = DisplayCursor::compact(diff, 3)
        .with_syntax_context(Some(&old), Some(&new));
    assert_eq!(cursor.context_label(0), Some("fn first() {"));
    assert_eq!(cursor.context_label(1), Some("fn second() {"));
    let rows = drain(&mut cursor, 1);
    assert_eq!(
        rows.iter()
            .filter(|row| row.text == "impl Engine {")
            .count(),
        1
    );
    assert_eq!(rows.iter().filter(|row| row.text == "}").count(), 1);
    let coordinate: Vec<_> = rows
        .iter()
        .filter_map(|row| row.new.map(|coordinate| coordinate.line))
        .collect();
    assert_eq!(
        coordinate.len(),
        coordinate.iter().collect::<BTreeSet<_>>().len()
    );
    assert!(coordinate.windows(2).all(|pair| pair[0] < pair[1]));
}

#[tokio::test]
async fn deletion_only_context_uses_the_old_declaration_and_preserves_line_mapping() {
    let original = function("pub fn foo() -> i32 {", "let");
    let modified: Vec<_> = original
        .iter()
        .enumerate()
        .filter(|(row, _)| ![8, 17].contains(row))
        .map(|(_, text)| text.clone())
        .collect();
    let (diff, old, new) = analysis(
        SyntaxLanguage::Rust,
        &original.join("\n"),
        &modified.join("\n"),
    )
    .await;
    let raw = diff.hunks().to_vec();
    let mut cursor = DisplayCursor::compact(diff.clone(), 3)
        .with_syntax_context(Some(&old), Some(&new));
    assert_eq!(cursor.group(0).unwrap().raw_ids.len(), 2);
    assert!(cursor.group(1).is_none());
    assert_eq!(cursor.context_label(0), Some("pub fn foo() -> i32 {"));
    for row in drain(&mut cursor, 1) {
        if let Some(coordinate) = row.old {
            assert_eq!(row.text, original[coordinate.line]);
        }
        if let Some(coordinate) = row.new {
            assert_eq!(row.text, modified[coordinate.line]);
        }
    }
    assert_eq!(diff.hunks(), raw);
}
