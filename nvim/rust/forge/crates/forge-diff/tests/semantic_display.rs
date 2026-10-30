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
async fn scoped_edits_keep_neighboring_public_declarations_and_unselected_hunks_unchanged() {
    use forge_diff::{
        edit::{EditDirection, EditSelection},
        semantic_view::SourceComparison,
        view::DiffVisibility,
    };
    let old = "pub fn first() { first_old(); }\npub fn second() { second_old(); }\n\n\n\n\npub fn distant() { distant_old(); }\n";
    let new = old
        .replace("first_old", "first_new")
        .replace("second_old", "second_new")
        .replace("distant_old", "distant_new");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
    assert_eq!(diff.hunks().len(), 2);
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let captured = EditSelection::capture_scoped(
        &diff,
        &[diff.hunks()[0].id],
        EditDirection::Forward,
        &comparison,
        Some(&["/function_item:first"]),
        DiffVisibility::All,
    )
    .unwrap();
    assert_eq!(
        captured.apply().unwrap().after().text(),
        old.replace("first_old", "first_new")
    );
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
async fn removed_children_remain_beneath_their_parent_after_source_line_shifts() {
    use forge_diff::{semantic_view::SourceComparison, view::DiffVisibility};
    let old = "export class Engine {\n  removed() { previous(); }\n  retained() { keep(); }\n}\n";
    let new = format!(
        "{}export class Engine {{\n  retained() {{ keep(); }}\n}}\n",
        "\n".repeat(30)
    );
    let (diff, before, after) = analysis(SyntaxLanguage::Typescript, old, &new).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let tree = comparison.declaration_tree(DiffVisibility::All);
    let mut published = BTreeSet::new();
    for node in tree {
        if let Some(parent) = node.parent {
            assert!(
                published.contains(&parent),
                "a removed child preceded its retained parent"
            );
        }
        published.insert(node.change);
    }
    assert!(published.len() >= 2);
}

#[tokio::test]
async fn container_visibility_transitions_preserve_private_child_filtering() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "export class Engine {\n  run() {\n    public_before();\n  }\n  private hidden() {\n    private_before();\n  }\n}\n";
    let new = old
        .replace("export class", "class")
        .replace("public_before", "public_after")
        .replace("private_before", "private_after");
    for (old, new) in [(old, new.as_str()), (new.as_str(), old)] {
        let (diff, before, after) = analysis(SyntaxLanguage::Typescript, old, new).await;
        let mut projection =
            SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
                .reconfigure(DiffViewOptions {
                    visibility: DiffVisibility::Public,
                    presentation: DiffPresentation::Full,
                })
                .unwrap();
        while projection.more() {
            projection.demand(ChunkLimits::default()).unwrap();
        }
        let frame = source_frame(&projection);
        let rows = frame
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>();
        assert!(rows.iter().any(|row| row.contains("public_after")));
        assert!(rows.iter().any(|row| row.contains("public_before")));
        assert!(
            rows.iter()
                .all(|row| !row.contains("private_") && !row.contains("hidden"))
        );
    }
}

#[tokio::test]
async fn public_container_counts_only_visible_raw_edit_regions() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "export class Engine {\n  run() {\n    public_before();\n  }\n  private hidden() {\n    private_before();\n  }\n}\n";
    let new = old
        .replace("public_before", "public_after")
        .replace("private_before", "private_after");
    let (diff, before, after) = analysis(SyntaxLanguage::Typescript, old, &new).await;
    let mut all = SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
        .reconfigure(DiffViewOptions {
            visibility: DiffVisibility::All,
            presentation: DiffPresentation::Semantic,
        })
        .unwrap();
    all.demand(ChunkLimits::default()).unwrap();
    let frame = source_frame(&all);
    let container = frame
        .iter()
        .find(|block| block.text.row(0) == Some("export class Engine {"))
        .unwrap();
    assert_eq!(
        container.metadata.fold[0].collapsed_suffix.as_deref(),
        Some(" … 2 changes }")
    );
    let mut public = all
        .reconfigure(DiffViewOptions {
            visibility: DiffVisibility::Public,
            presentation: DiffPresentation::Semantic,
        })
        .unwrap();
    public.demand(ChunkLimits::default()).unwrap();
    let frame = source_frame(&public);
    let container = frame
        .iter()
        .find(|block| block.text.row(0) == Some("export class Engine {"))
        .unwrap();
    assert_eq!(
        container.metadata.fold[0].collapsed_suffix.as_deref(),
        Some(" … 1 change }")
    );
    assert!(frame.iter().all(|block| {
        !block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("hidden"))
    }));
}

#[tokio::test]
async fn public_semantic_projection_omits_containers_with_only_private_edits() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "export class Engine {\n  private hidden() {\n    previous();\n  }\n}\n";
    let new = old.replace("previous()", "updated()");
    let (diff, before, after) = analysis(SyntaxLanguage::Typescript, old, &new).await;
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
    assert_eq!(frame.len(), 1);
    assert_eq!(
        frame[0].text.row(0),
        Some("No changes for the selected visibility")
    );
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

#[tokio::test]
async fn semantic_projection_pages_headings_without_splitting_large_signatures_or_loading_bodies() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let parameters = (0..300)
        .map(|index| format!("    value_{index}: u32,\n"))
        .collect::<String>();
    let old = format!(
        "pub fn first(\n{parameters}) {{ previous(); }}\npub fn second() {{ old_second(); }}\n"
    );
    let new = old
        .replace("previous()", "updated()")
        .replace("old_second()", "new_second()");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, &old, &new).await;
    let mut projection =
        SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
            .reconfigure(DiffViewOptions {
                visibility: DiffVisibility::All,
                presentation: DiffPresentation::Semantic,
            })
            .unwrap();
    projection
        .demand(ChunkLimits {
            rows: 1,
            ..Default::default()
        })
        .unwrap();
    let first = source_frame(&projection);
    assert_eq!(first[0].text.row_count(), 302);
    assert_eq!(first[0].metadata.fold[0].start.row, 301);
    assert!(first.iter().all(|block| {
        !block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("second") || row.contains("updated()"))
    }));
    assert!(projection.more());
    projection
        .demand(ChunkLimits {
            rows: 1,
            ..Default::default()
        })
        .unwrap();
    assert!(
        source_frame(&projection)
            .iter()
            .any(|block| block.text.row(0) == Some("pub fn second() {"))
    );
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

#[tokio::test]
async fn semantic_tree_keeps_ambiguous_siblings_in_raw_fallback() {
    use forge_diff::{
        semantic_view::SourceComparison, source_view::SourceViewCursor, view::DiffVisibility,
    };
    let (diff, before, after) = analysis(
        SyntaxLanguage::Typescript,
        "export const first = 1, second = 2;\n",
        "export const first = 3, second = 4;\n",
    )
    .await;
    let comparison =
        Arc::new(SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap());
    for node in comparison.declaration_tree(DiffVisibility::All) {
        let mut cursor = SourceViewCursor::new(
            diff.clone(),
            comparison.clone(),
            Some(node.change),
            DiffVisibility::All,
        )
        .unwrap();
        assert!(drain_source(&mut cursor).is_empty());
    }
    let mut fallback =
        SourceViewCursor::unclassified(diff, comparison, DiffVisibility::All).unwrap();
    let rows = drain_source(&mut fallback);
    assert!(
        rows.iter()
            .any(|row| row.text.contains("first = 3, second = 4"))
    );
}

#[tokio::test]
async fn semantic_tree_does_not_assign_duplicate_declaration_identities_to_new_or_removed_symbols()
{
    use forge_diff::{
        semantic_view::SourceComparison, source_view::SourceViewCursor, view::DiffVisibility,
    };
    for new in [
        "pub fn repeated() { updated(); }\npub fn repeated() { second(); }\n",
        "pub fn repeated() { updated(); }\n",
    ] {
        let (diff, before, after) = analysis(
            SyntaxLanguage::Rust,
            "pub fn repeated() { first(); }\npub fn repeated() { second(); }\n",
            new,
        )
        .await;
        let comparison =
            Arc::new(SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap());
        assert!(comparison.declaration_tree(DiffVisibility::All).is_empty());
        let mut fallback =
            SourceViewCursor::unclassified(diff, comparison, DiffVisibility::All).unwrap();
        assert!(
            drain_source(&mut fallback)
                .iter()
                .any(|row| row.text.contains("updated()"))
        );
    }
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
async fn semantic_signature_retains_export_bindings_and_projects_indented_method_tokens() {
    use forge_diff::{presentation::DiffPresenter, semantic_view::SourceComparison};
    for (language, old, new, name, prefix, column) in [
        (
            SyntaxLanguage::Typescript,
            "export const value = (): number => { return 1; };\n",
            "export const value = (): number => { return 2; };\n",
            "value",
            "export const value",
            0,
        ),
        (
            SyntaxLanguage::Rust,
            "pub struct State;\nimpl State {\n    pub fn value(&self) -> u32 { 1 }\n}\n",
            "pub struct State;\nimpl State {\n    pub fn value(&self) -> u32 { 2 }\n}\n",
            "value",
            "pub fn value",
            4,
        ),
    ] {
        let (diff, before, after) = analysis(language, old, new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        let index = comparison
            .change
            .iter()
            .position(|change| {
                change
                    .after
                    .is_some_and(|index| comparison.after.declaration[index].name == name)
            })
            .unwrap();
        let signature = comparison.signature(index).unwrap();
        assert!(signature.text.starts_with(prefix), "{}", signature.text);
        assert_eq!(signature.origin.byte_column, column);
        let block = DiffPresenter::new(Default::default())
            .declaration_signature(
                forge_buffer::identity::BlockId("signature".into()),
                &signature,
                Some(&before),
                Some(&after),
            )
            .unwrap();
        assert!(
            block.metadata.visible_decoration.iter().any(|span| {
                span.capture.contains("function")
                    && signature
                        .text
                        .get(span.range.start.column..span.range.end.column)
                        == Some(name)
            }),
            "function token did not align in {:?}",
            signature.text
        );
    }
}

#[tokio::test]
async fn full_source_view_preserves_existing_diff_rows_without_semantic_preparation() {
    let (diff, _, _) = analysis(
        SyntaxLanguage::Rust,
        "fn value() { previous(); }\n",
        "fn value() { updated(); }\n",
    )
    .await;
    let expected = drain(&mut DisplayCursor::new(diff.clone(), 3), 16);
    let mut cursor = forge_diff::source_view::SourceViewCursor::full(diff, None, None);
    let mut actual = Vec::new();
    loop {
        let chunk = cursor.next_rows(ChunkLimits {
            rows: 16,
            ..Default::default()
        });
        actual.extend(chunk.rows);
        if chunk.state == DisplayState::Complete {
            break;
        }
    }
    assert_eq!(actual, expected);
}

#[tokio::test]
async fn semantic_signatures_preserve_multiline_contracts_beyond_delivery_row_budgets() {
    use forge_diff::semantic_view::SourceComparison;
    let parameters = (0..300)
        .map(|index| format!("    value_{index}: u32,\n"))
        .collect::<String>();
    let old = format!("pub fn value(\n{parameters}) -> u32 {{ 1 }}\n");
    let new = format!("pub fn value(\n{parameters}) -> u32 {{ 2 }}\n");
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, &old, &new).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let signature = comparison.signature(0).unwrap();
    let block = forge_diff::presentation::DiffPresenter::new(Default::default())
        .declaration_signature(
            forge_buffer::identity::BlockId("large-signature".into()),
            &signature,
            Some(&before),
            Some(&after),
        )
        .unwrap();
    assert_eq!(block.text.row_count(), 302);
    assert_eq!(block.metadata.gutter.len(), 302);
    assert!(
        block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("value_299"))
    );
    assert!(!signature.signature_changed);
}

#[tokio::test]
async fn declaration_details_are_lazy_scoped_and_preserve_raw_coordinates() {
    use forge_diff::{
        presentation::RowChange, semantic_view::SourceComparison, source_view::SourceViewCursor,
        view::DiffVisibility,
    };
    let old = "pub fn first() {\n    old_first();\n}\nfn second() {\n    old_second();\n}\n";
    let new = "pub fn first() {\n    new_first();\n}\nfn second() {\n    new_second();\n}\n";
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
    let comparison =
        Arc::new(SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap());
    let declaration = comparison
        .change
        .iter()
        .position(|change| {
            change
                .after
                .is_some_and(|index| comparison.after.declaration[index].name == "first")
        })
        .unwrap();
    let signature = comparison.signature(declaration).unwrap();
    assert_eq!(signature.text, "pub fn first() {");
    assert_eq!(signature.change, RowChange::Modified);
    assert!(!signature.signature_changed);
    assert_eq!(signature.detail_changes, 1);
    let presenter = forge_diff::presentation::DiffPresenter::new(Default::default());
    let heading = presenter
        .declaration_signature(
            forge_buffer::identity::BlockId("first:heading".into()),
            &signature,
            Some(&before),
            Some(&after),
        )
        .unwrap();
    assert_eq!(heading.text.wire_rows(), vec!["pub fn first() {"]);
    assert!(
        heading
            .metadata
            .visible_decoration
            .iter()
            .any(|span| span.capture == "@function.rust")
    );
    assert!(
        heading
            .metadata
            .gutter
            .iter()
            .flat_map(|gutter| &gutter.chunk)
            .any(|chunk| chunk.text == "~")
    );
    assert!(
        heading
            .metadata
            .visible_decoration
            .iter()
            .all(|span| !span.capture.starts_with("ForgeInline"))
    );
    assert!(
        presenter
            .declaration_signature(
                forge_buffer::identity::BlockId("foreign".into()),
                &signature,
                Some(&before),
                Some(&before)
            )
            .is_err()
    );
    let mut cursor = SourceViewCursor::new(
        diff.clone(),
        comparison.clone(),
        Some(declaration),
        DiffVisibility::All,
    )
    .unwrap();
    let mut rows = Vec::new();
    loop {
        let chunk = cursor.next_rows(ChunkLimits {
            rows: 1,
            ..Default::default()
        });
        assert!(chunk.rows.len() <= 1);
        assert_eq!(
            chunk.text_bytes,
            chunk
                .rows
                .iter()
                .map(|row| row.text.len() + 1)
                .sum::<usize>()
        );
        for row in &chunk.rows {
            if let Some(raw) = row.raw_id {
                assert!(
                    cursor
                        .group(row.group_index)
                        .unwrap()
                        .raw_ids
                        .contains(&raw)
                );
            }
        }
        rows.extend(chunk.rows);
        if chunk.state == DisplayState::Complete {
            break;
        }
    }
    assert!(rows.iter().any(|row| row.text.contains("new_first")));
    assert!(rows.iter().all(|row| !row.text.contains("second")));
    assert!(
        rows.iter()
            .filter_map(|row| row.new_coordinate())
            .all(|coordinate| coordinate.line < 3)
    );
    assert!(matches!(
        SourceViewCursor::new(diff, comparison, Some(usize::MAX), DiffVisibility::All),
        Err(forge_diff::source::SourceError::InconsistentMetadata)
    ));
}

#[tokio::test]
async fn public_detail_delivery_retains_continuation_through_empty_filtered_chunks() {
    use forge_diff::{
        semantic_view::SourceComparison, source_view::SourceViewCursor, view::DiffVisibility,
    };
    let body = |name: &str| {
        (0..300)
            .map(|index| format!("    let {name}_{index} = {index};\n"))
            .collect::<String>()
    };
    let old = format!(
        "fn hidden() {{\n{}}}\npub fn visible() {{ old(); }}\n",
        body("previous")
    );
    let new = format!(
        "fn hidden() {{\n{}}}\npub fn visible() {{ revised(); }}\n",
        body("updated")
    );
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, &old, &new).await;
    let comparison =
        Arc::new(SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap());
    let mut cursor = SourceViewCursor::new(diff, comparison, None, DiffVisibility::Public).unwrap();
    let mut empty_continuation = false;
    let mut visible = Vec::new();
    loop {
        let chunk = cursor.next_rows(ChunkLimits {
            rows: 16,
            ..Default::default()
        });
        empty_continuation |= chunk.rows.is_empty() && chunk.state == DisplayState::More;
        visible.extend(chunk.rows);
        if chunk.state == DisplayState::Complete {
            break;
        }
    }
    assert!(empty_continuation);
    assert!(visible.iter().any(|row| row.text.contains("revised")));
    assert!(
        visible
            .iter()
            .all(|row| !row.text.contains("previous_") && !row.text.contains("updated_"))
    );
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

#[tokio::test]
async fn source_semantics_separates_inline_body_signature_and_visibility_changes() {
    use forge_diff::semantic_view::SourceComparison;
    for (language, old, new, signature, details) in [
        (
            SyntaxLanguage::Rust,
            "/// Previous contract.\npub fn value() -> i32 { 1 }\n",
            "/// Updated contract.\npub fn value() -> i32 { 1 }\n",
            true,
            false,
        ),
        (
            SyntaxLanguage::Rust,
            "pub fn value() -> i32 { 1 }\n",
            "pub fn value() -> i32 { 2 }\n",
            false,
            true,
        ),
        (
            SyntaxLanguage::Rust,
            "pub fn value() -> i32 { 1 }\n",
            "pub fn value() -> i64 { 1 }\n",
            true,
            false,
        ),
        (
            SyntaxLanguage::Rust,
            "pub fn value() -> i32 { 1 }\n",
            "fn value() -> i32 { 1 }\n",
            true,
            false,
        ),
        (
            SyntaxLanguage::Typescript,
            "export function value(): number { return 1; }\n",
            "export function value(): number { return 2; }\n",
            false,
            true,
        ),
        (
            SyntaxLanguage::Lua,
            "function value() return 1 end\n",
            "function value() return 2 end\n",
            false,
            true,
        ),
    ] {
        let (diff, before, after) = analysis(language, old, new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        assert!(comparison.classified);
        let changed = comparison
            .change
            .iter()
            .find(|entry| {
                entry
                    .after
                    .is_some_and(|index| comparison.after.declaration[index].name == "value")
            })
            .expect("changed function remains present");
        assert!(changed.before.is_some());
        assert_eq!(changed.signature_changed, signature, "{language:?}");
        assert_eq!(changed.details_changed, details, "{language:?}");
        assert_eq!(
            changed.detail_region.len(),
            usize::from(details),
            "{language:?}"
        );
        assert_eq!(comparison.source.old.identity(), before.source_identity());
    }
}

#[tokio::test]
async fn source_semantics_keeps_unknown_sources_unclassified_and_rejects_foreign_syntax() {
    use forge_diff::{semantic_view::SourceComparison, source::SourceError};
    let (diff, before, after) = analysis(
        SyntaxLanguage::Rust,
        "fn value() { old(); }\n",
        "fn value() { new(); }\n",
    )
    .await;
    let unknown = SourceComparison::analyze(&diff, Some(&before), None).unwrap();
    assert!(!unknown.classified);
    assert!(unknown.change.is_empty());
    assert!(matches!(
        SourceComparison::analyze(&diff, Some(&after), Some(&after)),
        Err(SourceError::InconsistentIdentity)
    ));
}

#[tokio::test]
async fn source_semantics_matches_same_named_methods_by_owner_and_retains_creation_deletion() {
    use forge_diff::semantic_view::SourceComparison;
    let duplicate = "fn repeated() { first(); }\nfn repeated() { second(); }\n";
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, duplicate, duplicate).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    assert!(
        comparison.change.is_empty(),
        "unmatched ambiguity cannot manufacture changes"
    );
    let old = "struct First;\nstruct Second;\nimpl First { fn value() { old(); } }\nimpl Second { fn value() { same(); } }\n";
    let new = "struct First;\nstruct Second;\nimpl Second { fn value() { same(); } }\nimpl First { fn value() { new(); } }\n";
    let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
    let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
    let changed = comparison
        .change
        .iter()
        .filter(|entry| {
            entry
                .after
                .is_some_and(|index| comparison.after.declaration[index].name == "value")
        })
        .collect::<Vec<_>>();
    assert_eq!(changed.len(), 1);
    assert!(changed[0].before.is_some());
    for (old, new) in [
        ("", "fn added() { run(); }\n"),
        ("fn removed() { run(); }\n", ""),
    ] {
        let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        assert_eq!(comparison.change.len(), 1);
        assert_eq!(comparison.change[0].before.is_none(), old.is_empty());
        assert_eq!(comparison.change[0].after.is_none(), new.is_empty());
    }
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
        let cursor = DisplayCursor::new(diff, 3).with_syntax_context(Some(&old), Some(&new));
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
    let mut cursor = DisplayCursor::new(diff, 3).with_syntax_context(Some(&old), Some(&new));
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
    let mut cursor = DisplayCursor::new(diff, 3).with_syntax_context(Some(&old), Some(&new));
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
        .filter_map(|row| row.new_coordinate().map(|coordinate| coordinate.line))
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
    let mut cursor =
        DisplayCursor::new(diff.clone(), 3).with_syntax_context(Some(&old), Some(&new));
    assert_eq!(cursor.group(0).unwrap().raw_ids.len(), 2);
    assert!(cursor.group(1).is_none());
    assert_eq!(cursor.context_label(0), Some("pub fn foo() -> i32 {"));
    for row in drain(&mut cursor, 1) {
        if let Some(coordinate) = row.old_coordinate() {
            assert_eq!(row.text, original[coordinate.line]);
        }
        if let Some(coordinate) = row.new_coordinate() {
            assert_eq!(row.text, modified[coordinate.line]);
        }
    }
    assert_eq!(diff.hunks(), raw);
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

#[tokio::test]
async fn one_omitted_source_line_is_shown_instead_of_an_ellipsis() {
    let mut original = vec!["impl Foo {".to_owned(), "  fn foobar(&self) {".to_owned()];
    original.extend((0..12).map(|index| format!("    let value_{index} = {index};")));
    original.extend(["  }".into(), "}".into()]);
    let mut proposed = original.clone();
    proposed[3] = "    let value_1 = 999;".into();
    let (diff, old, new) = analysis(
        SyntaxLanguage::Rust,
        &original.join("\n"),
        &proposed.join("\n"),
    )
    .await;
    let mut cursor = DisplayCursor::new(diff, 3).with_syntax_context(Some(&old), Some(&new));
    let rows = drain(&mut cursor, 1);
    let hidden = rows
        .iter()
        .find(|row| {
            row.new_coordinate()
                .is_some_and(|coordinate| coordinate.line == 2)
        })
        .unwrap();
    assert_eq!(hidden.text, original[2]);
    assert!(!hidden.is_gap());
    assert_eq!(hidden.old_coordinate().unwrap().line, 2);
}

#[tokio::test]
async fn an_omitted_method_separates_the_impl_header_from_the_changed_method() {
    let mut original = vec![
        "impl Foo {".to_owned(),
        "    fn untouched(&self) {".to_owned(),
    ];
    original.extend((0..12).map(|index| format!("        let untouched_{index} = {index};")));
    original.extend(["    }".into(), "    fn foobar(&self) {".into()]);
    original.extend((0..24).map(|index| format!("        let changed_{index} = {index};")));
    original.extend(["    }".into(), "}".into()]);
    let modified = original
        .join("\n")
        .replace("changed_12 = 12", "changed_12 = 999");
    let (diff, old, new) = analysis(SyntaxLanguage::Rust, &original.join("\n"), &modified).await;
    let mut cursor = DisplayCursor::new(diff, 3).with_syntax_context(Some(&old), Some(&new));
    let rows = drain(&mut cursor, 1);
    assert_eq!(rows[0].text, "impl Foo {");
    assert!(rows[1].is_gap());
    assert_eq!(rows[2].text, "    fn foobar(&self) {");
    assert!(rows.iter().all(|row| !row.text.contains("untouched")));
    assert_eq!(
        rows.iter().filter(|row| row.text == "impl Foo {").count(),
        1
    );
    assert!(
        rows.iter()
            .any(|row| row.kind() == RowKind::Added && row.text.contains("changed_12 = 999"))
    );
}

#[tokio::test]
async fn ambiguous_physical_rows_keep_public_code_raw_without_private_symbol_headings() {
    use forge_diff::{semantic_view::SourceComparison, view::DiffVisibility};
    for old in [
        "pub fn run() { fn hidden() { private_old(); } public_old(); }\n",
        "pub fn run() { public_old(); } fn hidden() { private_old(); }\n",
    ] {
        let new = old
            .replace("public_old", "public_new")
            .replace("private_old", "private_new");
        let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        assert_eq!(comparison.before.row_owner, vec![None]);
        assert_eq!(comparison.after.row_owner, vec![None]);
        let rows = drain(&mut DisplayCursor::new(diff, 3), 128);
        assert!(
            rows.iter()
                .all(|row| comparison.visible_row(row, DiffVisibility::Public))
        );
        let hidden = comparison
            .change
            .iter()
            .find(|change| {
                change
                    .after
                    .is_some_and(|index| comparison.after.declaration[index].name == "hidden")
            })
            .unwrap();
        assert!(!hidden.public_changed);
        assert!(
            comparison
                .declaration_tree(DiffVisibility::Public)
                .iter()
                .all(|node| {
                    comparison.change[node.change]
                        .after
                        .is_none_or(|index| comparison.after.declaration[index].name != "hidden")
                })
        );
        assert!(rows.iter().filter(|row| !row.is_gap()).all(|row| {
            row.old_coordinate()
                .is_none_or(|coordinate| comparison.row_change(true, coordinate.line).is_none())
                && row.new_coordinate().is_none_or(|coordinate| {
                    comparison.row_change(false, coordinate.line).is_none()
                })
        }));
    }
}

#[tokio::test]
async fn public_visibility_transitions_retain_both_body_revisions_and_entry_point() {
    use forge_diff::{semantic_view::SourceComparison, view::DiffVisibility};
    for (old, new) in [
        (
            "pub fn value() {\n    before();\n}\n",
            "fn value() {\n    after();\n    inserted();\n}\n",
        ),
        (
            "fn value() {\n    before();\n}\n",
            "pub fn value() {\n    after();\n    inserted();\n}\n",
        ),
        (
            "fn main() {\n    before();\n}\n",
            "fn main() {\n    after();\n    inserted();\n}\n",
        ),
    ] {
        let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, new).await;
        let comparison = SourceComparison::analyze(&diff, Some(&before), Some(&after)).unwrap();
        let rows = drain(&mut DisplayCursor::new(diff, 3), 128);
        assert!(
            rows.iter()
                .all(|row| comparison.visible_row(row, DiffVisibility::Public))
        );
        assert_eq!(comparison.declaration_tree(DiffVisibility::Public).len(), 1);
        assert!(comparison.change[0].public_changed);
    }
}

#[tokio::test]
async fn semantic_fallback_requires_unclassified_edits_instead_of_unchanged_context() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "use crate::Fixed;\n\npub fn visible() { before(); }\n\nfn hidden() { private_before(); }\n";
    let new = old.replace("before", "after");
    for visibility in [DiffVisibility::All, DiffVisibility::Public] {
        let (diff, before, after) = analysis(SyntaxLanguage::Rust, old, &new).await;
        let mut projection =
            SourceDiffProjection::new(diff, Some(before), Some(after), Default::default())
                .reconfigure(DiffViewOptions {
                    visibility,
                    presentation: DiffPresentation::Semantic,
                })
                .unwrap();
        projection.demand(ChunkLimits::default()).unwrap();
        assert!(
            !projection.more(),
            "classified headings must not advertise an empty continuation"
        );
        let frame = source_frame(&projection);
        assert!(
            !frame
                .iter()
                .any(|block| block.text.row(0) == Some("Unclassified source changes"))
        );
        assert!(frame.iter().any(|block| {
            block
                .text
                .row(0)
                .is_some_and(|row| row.contains("pub fn visible()"))
        }));
        assert!(!frame.iter().any(|block| {
            block
                .text
                .row(0)
                .is_some_and(|row| row.contains("before();") || row.contains("after();"))
        }));
    }
    let (diff, before, after) = analysis(
        SyntaxLanguage::Rust,
        old,
        &new.replace("crate::Fixed", "crate::Changed"),
    )
    .await;
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
            .is_some_and(|row| row.contains("crate::Changed"))
    }));
}

#[tokio::test]
async fn public_semantic_fallback_does_not_publish_context_around_hidden_private_edits() {
    use forge_diff::{
        source_projection::SourceDiffProjection,
        view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    };
    let old = "pub fn visible() { unchanged(); }\n\nfn hidden() { before(); }\n";
    let (diff, before, after) =
        analysis(SyntaxLanguage::Rust, old, &old.replace("before", "after")).await;
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
    assert_eq!(frame.len(), 1);
    assert_eq!(
        frame[0].text.row(0),
        Some("No changes for the selected visibility")
    );
}
