use std::sync::Arc;

use forge_diff::{
    cache::AnalysisHandle,
    display::{ChunkLimits, DisplayCursor, DisplayRow, DisplayState, RowKind, group_hunks},
    raw::compute_hunks,
    source::{Representation, SourcePair, SourceVersion},
    syntax::{SyntaxEngine, SyntaxHandle, SyntaxLanguage, SyntaxRequest},
    workers::{AnalysisPool, PoolLimits, WorkPriority},
};

mod support;

#[tokio::test]
async fn malformed_private_declarations_keep_complete_public_fallback_and_raw_edit_capture() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "fn broken() {\n    previous(;\n}\n";
    let new = old.replace("previous", "updated");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    assert!(comparison.change.is_empty());
    let captured = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        None,
        DiffVisibility::Public,
    )
    .unwrap();
    assert_eq!(captured.apply().unwrap().after().text(), new);
    let mut projection =
        SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
            .reconfigure(DiffViewOptions {
                visibility: DiffVisibility::Public,
                presentation: DiffPresentation::Semantic,
            })
            .unwrap();
    while projection.more() {
        projection.demand(ChunkLimits::default()).unwrap();
    }
    let frame = source_frame(&projection);
    assert!(
        frame
            .iter()
            .any(|block| block.text.row(0) == Some("Unclassified source changes"))
    );
    assert!(frame.iter().any(|block| {
        block
            .text
            .row(0)
            .is_some_and(|row| row.contains("previous(;"))
    }));
    assert!(frame.iter().any(|block| {
        block
            .text
            .row(0)
            .is_some_and(|row| row.contains("updated(;"))
    }));
}

#[tokio::test]
async fn scoped_edits_rebase_independent_symbols_from_the_same_raw_hunk() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    let old = "pub fn visible() { old_public(); }\nfn hidden() { old_private(); }\n";
    let new = old
        .replace("old_public", "new_public_longer")
        .replace("old_private", "new_private_longer");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
    assert_eq!(diff.hunks().len(), 1);
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let public = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        None,
        DiffVisibility::Public,
    )
    .unwrap();
    let private = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        Some(&["/function_item:hidden"]),
        DiffVisibility::All,
    )
    .unwrap();
    for (first, second) in [(&public, &private), (&private, &public)] {
        let first = first.apply().unwrap();
        let rebased = second.rebase_over(&first).unwrap();
        assert_eq!(rebased.apply().unwrap().after().text(), new);
    }
}

#[tokio::test]
async fn scoped_edits_union_declarations_and_combine_disjoint_regions_without_expanding_selection()
{
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    let old = "pub fn first() { first_old(); }\npub fn second() { second_old(); }\npub fn third() { third_old(); }\n\n\n\n\npub fn distant() { distant_old(); }\n";
    let new = old
        .replace("first_old", "first_new")
        .replace("second_old", "second_new")
        .replace("third_old", "third_new")
        .replace("distant_old", "distant_new");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
    assert_eq!(diff.hunks().len(), 2);
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let first = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        Some(&["/function_item:first", "/function_item:third"]),
        DiffVisibility::All,
    )
    .unwrap();
    let distant =
        EditSelection::capture(&diff, &[diff.hunks()[1].id], EditDirection::Forward).unwrap();
    let combined = first.clone().combine(distant).unwrap();
    assert_eq!(
        combined.apply().unwrap().after().text(),
        new.replace("second_new", "second_old")
    );
    assert_eq!(
        first
            .clone()
            .combine(first)
            .unwrap()
            .apply()
            .unwrap()
            .after()
            .text(),
        old.replace("first_old", "first_new")
            .replace("third_old", "third_new")
    );
}

#[tokio::test]
async fn public_edits_preserve_nested_private_bodies_between_two_changed_owner_regions() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    let old = "pub fn run() {\n    before_old();\n    fn hidden() { private_old(); }\n    after_old();\n}\n";
    let new = old
        .replace("before_old", "before_new")
        .replace("after_old", "after_new")
        .replace("private_old", "private_new");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
    assert_eq!(diff.hunks().len(), 1);
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let captured = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        None,
        DiffVisibility::Public,
    )
    .unwrap();
    assert_eq!(
        captured.apply().unwrap().after().text(),
        new.replace("private_new", "private_old")
    );
}

#[tokio::test]
async fn public_edits_preserve_private_bytes_in_shared_raw_hunks_and_invert_exactly() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    for newline in ["\n", "\r\n"] {
        for private_first in [false, true] {
            let public = "pub fn visible() { old_public(); }";
            let private = "fn hidden() { old_private(); }";
            let source = if private_first {
                [private, public]
            } else {
                [public, private]
            };
            let old = source.join(newline);
            let new = old
                .replace("old_public", "new_public")
                .replace("old_private", "new_private");
            let (diff, before, after) = analysis(SyntaxLanguage::Rust, &old, &new).await;
            assert_eq!(diff.hunks().len(), 1);
            let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
            let selected = [diff.hunks()[0].id];
            for (direction, expected, baseline) in [
                (
                    EditDirection::Forward,
                    old.replace("old_public", "new_public"),
                    old.as_str(),
                ),
                (
                    EditDirection::Reverse,
                    new.replace("new_public", "old_public"),
                    new.as_str(),
                ),
            ] {
                let captured = EditSelection::capture_scoped(
                    &diff,
                    &selected,
                    direction,
                    &comparison,
                    None,
                    DiffVisibility::Public,
                )
                .unwrap();
                let applied = captured.apply().unwrap();
                assert_eq!(applied.after().text(), expected);
                assert_eq!(applied.inverse().apply().unwrap().after().text(), baseline);
                assert_eq!(diff.hunks()[0].id, selected[0]);
            }
        }
    }
}

#[tokio::test]
async fn public_creation_and_deletion_preserve_unselected_private_declarations() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    let source = "pub fn visible() { public_body(); }\nfn hidden() { private_body(); }\n";
    for (old, new, expected) in [
        ("", source, "pub fn visible() { public_body(); }\n"),
        (source, "", "fn hidden() { private_body(); }\n"),
    ] {
        let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        let captured = EditSelection::capture_scoped(
            &diff,
            &[diff.hunks()[0].id],
            EditDirection::Forward,
            &comparison,
            None,
            DiffVisibility::Public,
        )
        .unwrap();
        let applied = captured.apply().unwrap();
        assert_eq!(applied.after().text(), expected);
        assert_eq!(applied.inverse().apply().unwrap().after().text(), old);
    }
}

#[tokio::test]
async fn source_projection_delivers_lazy_bodies_and_restores_cached_presentations() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let (diff, before, after) = analysis(
        SyntaxLanguage::Rust,
        "pub fn visible() {\n    previous();\n}\nfn hidden() { old_private(); }\n",
        "pub fn visible() {\n    updated();\n}\nfn hidden() { new_private(); }\n",
    )
    .await;
    let mut full = SourceDiffProjection::new(diff, Some(before), Some(after), Default::default());
    assert!(!full.materialized());
    full.demand(ChunkLimits::default()).unwrap();
    let original = source_frame(&full);
    let semantic_options = DiffViewOptions {
        visibility: DiffVisibility::All,
        presentation: DiffPresentation::Semantic,
    };
    let mut semantic = full.reconfigure(semantic_options).unwrap();
    semantic.demand(ChunkLimits::default()).unwrap();
    let collapsed = source_frame(&semantic);
    assert!(collapsed.iter().all(|block| {
        !block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("updated()") || row.contains("new_private()"))
    }));
    let heading = collapsed
        .iter()
        .find(|block| block.text.row(0) == Some("pub fn visible() {"))
        .unwrap();
    assert!(heading.metadata.fold[0].closed);
    assert_eq!(
        heading.metadata.fold[0].collapsed_suffix.as_deref(),
        Some(" … 1 change }")
    );
    let identity = "/function_item:visible";
    semantic
        .declaration(
            identity,
            true,
            ChunkLimits {
                rows: 1,
                ..Default::default()
            },
        )
        .unwrap();
    let partial = source_frame(&semantic);
    semantic
        .declaration(identity, false, ChunkLimits::default())
        .unwrap();
    semantic
        .declaration(identity, true, ChunkLimits::default())
        .unwrap();
    assert_eq!(
        source_frame(&semantic),
        partial,
        "reopening consumed additional source rows"
    );
    loop {
        semantic
            .declaration(
                identity,
                true,
                ChunkLimits {
                    rows: 1,
                    ..Default::default()
                },
            )
            .unwrap();
        let frame = source_frame(&semantic);
        let heading = frame.iter().find(|block| block.id == heading.id).unwrap();
        if !heading.metadata.section[0].more {
            break;
        }
    }
    let expanded = source_frame(&semantic);
    assert!(expanded.iter().any(|block| {
        block
            .text
            .row(0)
            .is_some_and(|row| row.contains("updated()"))
    }));
    assert!(expanded.iter().all(|block| {
        !block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("new_private()"))
    }));
    let restored_full = semantic.reconfigure(DiffViewOptions::default()).unwrap();
    assert!(restored_full.materialized());
    assert_eq!(source_frame(&restored_full), original);
    let restored_semantic = restored_full.reconfigure(semantic_options).unwrap();
    assert!(restored_semantic.materialized());
    assert_eq!(source_frame(&restored_semantic), expanded);
}

#[tokio::test]
async fn source_projection_visibility_is_independent_and_unknown_changes_remain_available() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let (diff, before, after) = analysis(
        SyntaxLanguage::Rust,
        "pub fn visible() { previous(); }\nfn hidden() { old_private(); }\n",
        "pub fn visible() { updated(); }\nfn hidden() { new_private(); }\n",
    )
    .await;
    let mut all = SourceDiffProjection::new(diff, Some(before), Some(after), Default::default());
    all.demand(ChunkLimits::default()).unwrap();
    let original = source_frame(&all);
    let mut public = all
        .reconfigure(DiffViewOptions {
            visibility: DiffVisibility::Public,
            presentation: DiffPresentation::Full,
        })
        .unwrap();
    public.demand(ChunkLimits::default()).unwrap();
    let public_frame = source_frame(&public);
    assert!(public_frame.iter().any(|block| {
        block
            .text
            .row(0)
            .is_some_and(|row| row.contains("updated()"))
    }));
    assert!(public_frame.iter().all(|block| {
        !block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("private()"))
    }));
    let restored = public.reconfigure(DiffViewOptions::default()).unwrap();
    assert_eq!(source_frame(&restored), original);
    let (diff, before, after) =
        analysis(SyntaxLanguage::Json, "{\"value\":1}\n", "{\"value\":2}\n").await;
    let mut unknown =
        SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
            .reconfigure(DiffViewOptions {
                visibility: DiffVisibility::Public,
                presentation: DiffPresentation::Semantic,
            })
            .unwrap();
    unknown.demand(ChunkLimits::default()).unwrap();
    let frame = source_frame(&unknown);
    assert!(
        frame
            .iter()
            .any(|block| block.text.row(0) == Some("Unclassified source changes"))
    );
    assert!(
        frame
            .iter()
            .any(|block| block.text.row(0) == Some("{\"value\":2}"))
    );
    assert!(!unknown.more());
}

fn source_frame(
    projection: &forge_diff::source_projection::SourceDiffProjection,
) -> Vec<forge_buffer::block::BufferBlock> {
    let block = projection.render("source", |_, _| Ok(())).unwrap();
    forge_buffer::document::BufferDocument::new(
        forge_buffer::identity::DocumentId("source-contract".into()),
        block.clone(),
    )
    .unwrap();
    block
}

#[tokio::test]
async fn semantic_tree_assigns_nested_edits_once_and_keeps_local_bindings_in_function_details() {
    use forge_diff::{
        semantic_view::SourceComparison, source_view::SourceViewCursor, view::DiffVisibility,
    };
    let old = "import { item } from './old';\nexport class Store {\n  value() {\n    const count = 1;\n    function nested() { return 3; }\n    return count;\n  }\n  private hidden() { return 5; }\n}\n";
    let new = old
        .replace("'./old'", "'./new'")
        .replace("count = 1", "count = 2")
        .replace("return 3", "return 4")
        .replace("return 5", "return 6");
    let (diff, before, after) = analysis(SyntaxLanguage::Typescript, old, &new).await;
    let comparison =
        Arc::new(SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap());
    let tree = comparison.declaration_tree(DiffVisibility::All);
    let name = |change: usize| {
        comparison.after.declaration[comparison.change[change].after.unwrap()]
            .name
            .as_str()
    };
    assert_eq!(
        tree.iter()
            .map(|node| name(node.change))
            .collect::<Vec<_>>(),
        ["Store", "value", "nested", "hidden"]
    );
    let value = tree
        .iter()
        .find(|node| name(node.change) == "value")
        .unwrap()
        .change;
    let nested = tree
        .iter()
        .find(|node| name(node.change) == "nested")
        .unwrap();
    assert_eq!(nested.parent, Some(value));
    let mut partition = Vec::new();
    for node in &tree {
        let mut cursor = SourceViewCursor::new(
            diff.clone(),
            comparison.clone(),
            Some(node.change),
            DiffVisibility::All,
        )
        .unwrap();
        let rows = drain_source(&mut cursor);
        if node.change == value {
            assert!(rows.iter().any(|row| row.text.contains("count = 2")));
            assert!(rows.iter().all(|row| !row.text.contains("function nested")));
        }
        partition.extend(rows);
    }
    let mut fallback =
        SourceViewCursor::unclassified(diff.clone(), comparison.clone(), DiffVisibility::All)
            .unwrap();
    let fallback = drain_source(&mut fallback);
    assert!(fallback.iter().any(|row| row.text.contains("'./new'")));
    partition.extend(fallback);
    for row in drain(&mut DisplayCursor::new(diff, 3), 16)
        .into_iter()
        .filter(|row| !row.is_gap())
    {
        assert_eq!(
            partition
                .iter()
                .filter(|candidate| **candidate == row)
                .count(),
            1,
            "row must have exactly one owner: {row:?}"
        );
    }
    let public = comparison.declaration_tree(DiffVisibility::Public);
    assert!(public.iter().any(|node| name(node.change) == "value"));
    assert!(public.iter().all(|node| name(node.change) != "hidden"));
}

fn drain_source(cursor: &mut forge_diff::source_view::SourceViewCursor) -> Vec<DisplayRow> {
    let mut rows = Vec::new();
    loop {
        let chunk = cursor.next_rows(ChunkLimits {
            rows: 2,
            ..Default::default()
        });
        rows.extend(chunk.rows);
        if chunk.state == DisplayState::Complete {
            return rows;
        }
    }
}

#[tokio::test]
async fn source_visibility_keeps_public_bodies_imports_and_both_sides_of_visibility_transitions() {
    use forge_diff::{semantic_view::SourceComparison, view::DiffVisibility};
    let old = "use crate::Old;\npub fn value() {\n    old_public();\n}\nfn hidden() {\n    old_private();\n}\n";
    let new = "use crate::New;\npub fn value() {\n    new_public();\n}\nfn hidden() {\n    new_private();\n}\n";
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let rows = drain(&mut DisplayCursor::new(diff.clone(), 3), 128);
    let visible = rows
        .iter()
        .filter(|row| comparison.visible_row(row, DiffVisibility::Public))
        .map(|row| row.text.as_str())
        .collect::<Vec<_>>()
        .join("\n");
    assert!(visible.contains("crate::New"));
    assert!(visible.contains("old_public"));
    assert!(visible.contains("new_public"));
    assert!(!visible.contains("old_private"));
    assert!(!visible.contains("new_private"));
    let (diff, before, after) = analysis(
        SyntaxLanguage::Rust,
        "pub fn value() { run(); }\n",
        "fn value() { run(); }\n",
    )
    .await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let rows = drain(&mut DisplayCursor::new(diff, 3), 128);
    assert!(
        rows.iter()
            .filter(|row| !row.is_gap())
            .all(|row| comparison.visible_row(row, DiffVisibility::Public))
    );
}

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
    (support::retain(compute_hunks(source)), old, new)
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
        {
            let make_cursor = || {
                let cursor = DisplayCursor::new(diff.clone(), 3);
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
            for select in [
                |row: &DisplayRow| row.old_coordinate(),
                |row: &DisplayRow| row.new_coordinate(),
            ] {
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
                .find(|row| {
                    row.new_coordinate()
                        .is_some_and(|coordinate| coordinate.line == 18)
                })
                .unwrap();
            assert_eq!(bridge.kind(), RowKind::Context);
            assert_eq!(bridge.group_index, 0);
            assert!(rows.iter().all(|row| {
                row.new_coordinate()
                    .is_none_or(|coordinate| coordinate.line != 29)
            }));
            assert_eq!(diff.hunks(), raw);
        }
    }
}

#[tokio::test]
async fn structural_gaps_preserve_syntax_and_source_coordinates_across_delivery_limits() {
    for (language, opening, method, binding) in [
        (
            SyntaxLanguage::Rust,
            "impl Foo {",
            "  fn foobar(&self) {",
            "let",
        ),
        (
            SyntaxLanguage::Typescript,
            "class Foo {",
            "  foobar() {",
            "const",
        ),
    ] {
        for newline in ["\n", "\r\n"] {
            let mut original = vec![opening.to_owned(), method.to_owned()];
            original.extend((0..40).map(|index| format!("    {binding} value_{index} = {index};")));
            original.extend(["  }".into(), "}".into()]);
            let mut proposed = original.clone();
            proposed[12] = format!("    {binding} value_10 = 999;");
            let (diff, old, new) =
                analysis(language, &original.join(newline), &proposed.join(newline)).await;
            {
                let cursor = || {
                    DisplayCursor::new(diff.clone(), 3).with_syntax_context(Some(&old), Some(&new))
                };
                let expected = drain(&mut cursor(), 256);
                assert_eq!(expected, drain(&mut cursor(), 1));
                assert!(expected.iter().any(DisplayRow::is_gap));
                assert_eq!(expected.iter().filter(|row| row.text == opening).count(), 1);
                assert_eq!(expected.iter().filter(|row| row.text == method).count(), 1);
                for row in &expected {
                    let mut metadata = forge_buffer::block::BlockMetadata::default();
                    forge_diff::projection::append_display_row(
                        &mut metadata,
                        0,
                        row,
                        cursor().group(row.group_index).unwrap(),
                        forge_diff::projection::RowSyntax {
                            old: Some(&old),
                            new: Some(&new),
                        },
                        None,
                        0,
                    )
                    .unwrap();
                    if row.is_gap() {
                        assert_eq!(row.text.trim(), "...");
                        assert!(row.raw_id.is_none());
                        assert!(
                            metadata
                                .visible_decoration
                                .iter()
                                .all(|capture| !capture.capture.ends_with(language.name()))
                        );
                    } else {
                        if let Some(coordinate) = row.old_coordinate() {
                            assert_eq!(row.text, original[coordinate.line]);
                        }
                        if let Some(coordinate) = row.new_coordinate() {
                            assert_eq!(row.text, proposed[coordinate.line]);
                        }
                        if row.text == opening || row.text == method {
                            assert!(
                                metadata
                                    .visible_decoration
                                    .iter()
                                    .any(|capture| capture.capture.ends_with(language.name())),
                                "missing syntax: {row:?}"
                            );
                        }
                    }
                }
                let mut streamed = cursor();
                let mut actual = Vec::new();
                loop {
                    let chunk = streamed.next_rows(ChunkLimits {
                        rows: 2,
                        text_bytes: 3,
                        decorations: 1,
                    });
                    assert!(chunk.rows.len() <= 1);
                    assert!(!chunk.rows.is_empty() || chunk.state == DisplayState::Complete);
                    actual.extend(chunk.rows);
                    if chunk.state == DisplayState::Complete {
                        break;
                    }
                }
                assert_eq!(
                    actual
                        .iter()
                        .map(|row| (&row.text, row.old_coordinate(), row.new_coordinate()))
                        .collect::<Vec<_>>(),
                    expected
                        .iter()
                        .map(|row| (&row.text, row.old_coordinate(), row.new_coordinate()))
                        .collect::<Vec<_>>()
                );
            }
        }
    }
}
