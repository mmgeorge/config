use std::sync::Arc;

use forge_diff::{
    display::ChunkLimits,
    raw::compute_hunks,
    saved_projection::{SavedDiffProjection, SavedDiffTarget},
    saved_source::{SavedDiffSource, SavedFileSource},
    source::{Representation, SourcePair, SourceVersion},
    source_projection::SourceDiffProjection,
    syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest},
    view::{DiffPresentation, DiffViewOptions, DiffVisibility},
    workers::{AnalysisPool, PoolLimits, WorkPriority},
};

mod support;

#[tokio::test]
async fn saved_semantics_retains_body_evidence_visibility_and_cached_full_rows() {
    let before = "pub fn visible() {\n    previous();\n}\nfn hidden() { old_private(); }\n";
    let after = before
        .replace("previous", "updated")
        .replace("old_private", "new_private");
    let patch = format!(
        "diff --git a/source.rs b/source.rs\nold mode 100644\nnew mode 100755\n--- a/source.rs\n+++ b/source.rs\n@@ -1,4 +1,4 @@\n{}{}",
        before
            .lines()
            .map(|line| format!("-{line}\n"))
            .collect::<String>(),
        after
            .lines()
            .map(|line| format!("+{line}\n"))
            .collect::<String>()
    );
    let source = pair(before, &after);
    let mut model = SavedDiffProjection::new(captured(patch, source.clone())).unwrap();
    let key = model.next_file().unwrap().to_owned();
    assert!(model.pending_source(&key).is_some());
    let syntax = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
    })));
    let mut parsed = Vec::new();
    for source in [source.old.clone(), source.new.clone()] {
        parsed.push(
            syntax
                .analyze(SyntaxRequest {
                    source,
                    language: SyntaxLanguage::Rust,
                    priority: WorkPriority::Foreground,
                    deadline: None,
                })
                .await
                .unwrap(),
        );
    }
    let after = parsed.pop().unwrap();
    let before = parsed.pop().unwrap();
    model
        .install(
            &key,
            SourceDiffProjection::new(
                support::retain(compute_hunks(source)),
                Some(before),
                Some(after),
                Default::default(),
            ),
            None,
        )
        .unwrap();
    model.demand(&key, ChunkLimits::default()).unwrap();
    assert!(!model.more());
    let full = rows(&model);
    assert!(full.contains("new mode 100755"));
    assert!(full.contains("updated();"));
    let options = DiffViewOptions {
        visibility: DiffVisibility::All,
        presentation: DiffPresentation::Semantic,
    };
    model = model.reconfigure(options, ChunkLimits::default()).unwrap();
    assert!(!rows(&model).contains("updated();"));
    let frame = model.render("test").unwrap();
    let heading = frame
        .block
        .iter()
        .find(|block| block.text.row(0) == Some("pub fn visible() {"))
        .unwrap();
    assert!(heading.metadata.fold[0].closed);
    assert!(
        heading.metadata.fold[0]
            .collapsed_suffix
            .as_ref()
            .unwrap()
            .contains("1 change")
    );
    let target = frame
        .target
        .get(&heading.metadata.target[0].id)
        .unwrap()
        .clone();
    assert_eq!(
        target.declaration.as_deref(),
        Some("/function_item:visible")
    );
    model
        .section(&target, true, ChunkLimits::default())
        .unwrap();
    assert!(rows(&model).contains("updated();"));
    model
        .section(&target, false, ChunkLimits::default())
        .unwrap();
    assert!(!rows(&model).contains("updated();"));
    model
        .section(&target, true, ChunkLimits::default())
        .unwrap();
    assert!(rows(&model).contains("updated();"));
    model = model
        .reconfigure(
            DiffViewOptions {
                visibility: DiffVisibility::Public,
                ..options
            },
            ChunkLimits::default(),
        )
        .unwrap();
    assert!(!rows(&model).contains("hidden"));
    assert!(rows(&model).contains("updated();"));
    model = model
        .reconfigure(DiffViewOptions::default(), ChunkLimits::default())
        .unwrap();
    assert_eq!(rows(&model), full);
    model = model.reconfigure(options, ChunkLimits::default()).unwrap();
    assert!(rows(&model).contains("updated();"));
    model
        .section(
            &SavedDiffTarget {
                file: key,
                declaration: None,
            },
            false,
            ChunkLimits::default(),
        )
        .unwrap();
    assert!(!model.more());
    assert!(!model.render("test").unwrap().block[0].metadata.section[0].open);
}

#[test]
fn saved_raw_fallback_pages_every_original_metadata_and_binary_row() {
    let patch = format!(
        "diff --git a/image.bin b/image.bin\nold mode 100644\nnew mode 100755\nGIT binary patch\nliteral 80000\n{}\ndiff --git a/old.rs b/new.rs\nsimilarity index 100%\nrename from old.rs\nrename to new.rs\n",
        "payload-without-invented-source\n".repeat(70_000)
    );
    let mut model = SavedDiffProjection::new(SavedDiffSource {
        text: Arc::new(patch.clone()),
        file: Vec::new(),
        diagnostic: None,
    })
    .unwrap();
    let limits = ChunkLimits {
        rows: 127,
        ..Default::default()
    };
    while let Some(key) = model.next_file().map(str::to_owned) {
        model.demand(&key, limits).unwrap();
    }
    let original = raw_rows(&model);
    assert_eq!(original, patch);
    for presentation in [DiffPresentation::Semantic, DiffPresentation::Full] {
        model = model
            .reconfigure(
                DiffViewOptions {
                    visibility: DiffVisibility::Public,
                    presentation,
                },
                limits,
            )
            .unwrap();
        assert_eq!(raw_rows(&model), patch);
        assert!(rows(&model).contains("Unclassified source changes"));
    }
}

#[test]
fn saved_evidence_rejects_mismatched_sources_and_distinguishes_sparse_context() {
    let patch = "--- a/source.rs\n+++ b/source.rs\n@@ -2 +2 @@\n-before\n+after\n";
    let first = SavedDiffProjection::new(captured(
        patch.into(),
        pair("context-one\nbefore\n", "context-one\nafter\n"),
    ))
    .unwrap();
    let second = SavedDiffProjection::new(captured(
        patch.into(),
        pair("context-two\nbefore\n", "context-two\nafter\n"),
    ))
    .unwrap();
    assert_ne!(first.identity(), second.identity());
    let mut mismatch = SavedDiffProjection::new(captured(
        patch.into(),
        pair("stale\nbefore\n", "different\nafter\n"),
    ))
    .unwrap();
    let key = mismatch.next_file().unwrap().to_owned();
    assert!(mismatch.pending_source(&key).is_none());
    mismatch.demand(&key, ChunkLimits::default()).unwrap();
    assert_eq!(raw_rows(&mismatch), patch);
    mismatch = mismatch
        .reconfigure(
            DiffViewOptions {
                visibility: DiffVisibility::Public,
                presentation: DiffPresentation::Semantic,
            },
            ChunkLimits::default(),
        )
        .unwrap();
    assert!(rows(&mismatch).contains("do not match"));
    assert!(
        SavedDiffProjection::new(SavedDiffSource {
            text: Arc::new("invalid patch".into()),
            file: Vec::new(),
            diagnostic: None
        })
        .is_err()
    );
}

fn pair(before: &str, after: &str) -> SourcePair {
    SourcePair {
        old: SourceVersion::new(before.as_bytes().to_vec(), Representation::Raw).unwrap(),
        new: SourceVersion::new(after.as_bytes().to_vec(), Representation::Raw).unwrap(),
    }
}

fn captured(text: String, source: SourcePair) -> SavedDiffSource {
    SavedDiffSource {
        text: Arc::new(text),
        file: vec![SavedFileSource {
            old_path: Some("source.rs".into()),
            new_path: Some("source.rs".into()),
            source: Ok(source),
        }],
        diagnostic: None,
    }
}

fn rows(model: &SavedDiffProjection) -> String {
    model
        .render("test")
        .unwrap()
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>()
        .join("\n")
}

fn raw_rows(model: &SavedDiffProjection) -> String {
    model
        .render("test")
        .unwrap()
        .block
        .iter()
        .filter(|block| block.id.0.contains(":raw:"))
        .flat_map(|block| block.text.wire_rows())
        .map(|row| format!("{row}\n"))
        .collect()
}
