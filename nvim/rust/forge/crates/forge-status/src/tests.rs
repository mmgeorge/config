use std::{future::Future, sync::Arc, time::Duration};

use forge_buffer::{
    block::TextPosition,
    identity::{DocumentId, InputSequence, ViewId},
    patch::BufferSnapshot,
};
use forge_diff::{engine::DiffEngine, syntax::SyntaxEngine};
use forge_git::{store::RepositoryStore, writer::GitWriteService};

use forge_git::test_support::{
    head_commit, init_repository, lfs_pointer, set_config, set_identity, stage, stage_and_commit,
};

use crate::protocol::{StatusFile, StatusStatistics};
use crate::{BodyState, StatusInput, StatusLocation, StatusSection, StatusService, StatusSnapshot};

async fn fixture() -> (
    tempfile::TempDir,
    StatusService,
    Arc<DiffEngine>,
    Arc<GitWriteService>,
) {
    let directory = tempfile::tempdir().unwrap();
    init_repository(directory.path());
    set_identity(directory.path());
    set_config(directory.path(), "core", "autocrlf", "false");
    std::fs::write(directory.path().join("sample.txt"), "old\n").unwrap();
    stage_and_commit(directory.path(), &["sample.txt"], "initial");
    std::fs::write(directory.path().join("sample.txt"), "new\n").unwrap();
    // The edit keeps the committed size, so a write within the commit's second leaves stat data
    // identical at whole-second precision and status can report the file clean. Moving its mtime
    // a full hour makes the modification visible to stat comparison on every run.
    std::fs::OpenOptions::new()
        .write(true)
        .open(directory.path().join("sample.txt"))
        .unwrap()
        .set_modified(std::time::SystemTime::now() + Duration::from_secs(3600))
        .unwrap();
    let store = Arc::new(RepositoryStore::default());
    let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
    let writer = Arc::new(GitWriteService::new(Arc::clone(&store)));
    let service = StatusService::new(store, Arc::clone(&diff), syntax, Arc::clone(&writer));
    (directory, service, diff, writer)
}

/// Render a hydrated LFS worktree as plain text without authorizing mutations.
#[tokio::test]
async fn text_lfs_diff_is_plain_and_read_only() {
    let (directory, service, _, _) = fixture().await;
    let root = directory.path();
    let (old, _) = lfs_pointer(&root.join(".git/lfs"), b"export const value = 1;\n");
    std::fs::write(root.join(".gitattributes"), "*.ts filter=lfs -text\n").unwrap();
    std::fs::write(root.join("sample.spec.ts"), &old).unwrap();
    stage_and_commit(root, &[".gitattributes", "sample.spec.ts"], "LFS source");
    set_config(root, "filter \"lfs\"", "process", "");
    set_config(root, "filter \"lfs\"", "clean", "");
    set_config(root, "filter \"lfs\"", "required", "true");
    {
        let content = "export const value = 2;\n";
        std::fs::write(root.join("sample.spec.ts"), content).unwrap();
        let snapshot = service
            .open(DocumentId("lfs-text".into()), root.to_owned())
            .await
            .unwrap();
        set_config(
            root,
            "filter \"lfs\"",
            "process",
            "forge-test-filter-must-not-run",
        );
        let index = std::fs::read(root.join(".git/index")).unwrap();
        let collapsed_error = service
            .act(input(&snapshot, "sample.spec.ts", 1, "stage"), None)
            .await
            .err()
            .expect("collapsed LFS files must remain read-only");
        assert!(
            collapsed_error.to_string().contains("LFS"),
            "{collapsed_error:#}"
        );
        let delivery = service
            .demand(input(&snapshot, "sample.spec.ts", 2, "demand"))
            .await
            .unwrap();
        assert_eq!(delivery.state, BodyState::Ready);
        let text = rows(delivery.snapshot.as_ref().unwrap()).join("\n");
        assert!(
            text.contains("export const value = 1;") && text.contains("export const value = 2;"),
            "{text}"
        );
        assert!(!text.contains("oid sha256:"));
        let owner = service.get(&snapshot.document).unwrap();
        {
            let document = owner.lock().await;
            let file = document
                .file
                .values()
                .find(|file| file.record.path.display_label() == "sample.spec.ts")
                .unwrap();
            assert!(file.old_syntax.is_none() && file.new_syntax.is_none());
            assert_eq!(
                file.projection
                    .analysis()
                    .unwrap()
                    .source()
                    .old
                    .identity()
                    .representation,
                forge_diff::source::Representation::DisplayOnly
            );
        }
        let loaded = delivery.snapshot.as_ref().unwrap();
        let (block, row) = loaded
            .block
            .iter()
            .find_map(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .position(|row| *row == "export const value = 2;")
                    .map(|row| (block, row))
            })
            .unwrap();
        let mut hunk = input(&snapshot, "sample.spec.ts", 3, "stage");
        hunk.location = body_location(file(&snapshot, "sample.spec.ts", None), loaded, block, row);
        let hunk_error = service
            .act(hunk, None)
            .await
            .err()
            .expect("LFS hunks must remain read-only");
        assert!(hunk_error.to_string().contains("LFS"), "{hunk_error:#}");
        for (sequence, action) in [(4, "stage"), (5, "discard")] {
            let error = service
                .act(input(&snapshot, "sample.spec.ts", sequence, action), None)
                .await
                .err()
                .expect("LFS actions must be read-only");
            assert!(error.to_string().contains("LFS"), "{error:#}");
        }
        assert_eq!(
            std::fs::read_to_string(root.join("sample.spec.ts")).unwrap(),
            content
        );
        assert_eq!(std::fs::read(root.join(".git/index")).unwrap(), index);
        service
            .change_diff_views(
                input(&snapshot, "sample.spec.ts", 6, "toggle_presentation"),
                vec![],
            )
            .await
            .unwrap();
        let current = service.snapshot(&snapshot.document).await.unwrap();
        let displayed = body(&service, &current, file(&current, "sample.spec.ts", None)).await;
        assert!(rows(&displayed).contains(&"export const value = 2;"));
        let document = owner.lock().await;
        let source = document
            .file
            .values()
            .find(|file| file.record.path.display_label() == "sample.spec.ts")
            .unwrap();
        assert!(source.old_syntax.is_none() && source.new_syntax.is_none());
        drop(document);
        service.close(&snapshot.document);
    }
}

/// Bypass cached pointer comparisons for staged LFS content and reject collapsed unstaging.
#[tokio::test]
async fn staged_lfs_diff_uses_payloads_instead_of_cached_pointers() {
    let (directory, service, _, _) = fixture().await;
    let root = directory.path();
    let (old, _) = lfs_pointer(&root.join(".git/lfs"), b"old text\n");
    let (new, _) = lfs_pointer(&root.join(".git/lfs"), b"new text\n");
    std::fs::write(root.join(".gitattributes"), "*.ts filter=lfs -text\n").unwrap();
    std::fs::write(root.join("source.ts"), old).unwrap();
    stage_and_commit(root, &[".gitattributes", "source.ts"], "LFS baseline");
    std::fs::write(root.join("source.ts"), new).unwrap();
    stage(root, &["source.ts"]);
    std::fs::write(root.join("source.ts"), "new text\n").unwrap();
    set_config(
        root,
        "filter \"lfs\"",
        "process",
        "forge-test-filter-must-not-run",
    );
    set_config(root, "filter \"lfs\"", "required", "true");
    let snapshot = service
        .open(DocumentId("staged-lfs".into()), root.to_owned())
        .await
        .unwrap();
    let selected = file(&snapshot, "source.ts", Some(StatusSection::Staged));
    let index = std::fs::read(root.join(".git/index")).unwrap();
    let error = service
        .act(
            captured(
                &snapshot,
                StatusLocation::File { id: selected.id },
                1,
                "unstage",
            ),
            None,
        )
        .await
        .err()
        .expect("LFS unstaging must remain read-only");
    assert!(error.to_string().contains("LFS"), "{error:#}");
    let delivery = service
        .demand(captured(
            &snapshot,
            StatusLocation::File { id: selected.id },
            2,
            "demand",
        ))
        .await
        .unwrap();
    assert_eq!(delivery.state, BodyState::Ready);
    let text = rows(delivery.snapshot.as_ref().unwrap());
    assert!(
        text.contains(&"old text") && text.contains(&"new text"),
        "{text:?}"
    );
    assert_eq!(std::fs::read(root.join(".git/index")).unwrap(), index);
}

fn file<'a>(
    snapshot: &'a StatusSnapshot,
    filename: &str,
    section: Option<StatusSection>,
) -> &'a StatusFile {
    snapshot
        .file
        .iter()
        .find(|file| file.path == filename && section.is_none_or(|section| file.section == section))
        .unwrap()
}

fn captured(
    snapshot: &StatusSnapshot,
    location: StatusLocation,
    sequence: u64,
    action: &str,
) -> StatusInput {
    StatusInput {
        document: snapshot.document.clone(),
        revision: snapshot.revision,
        view: ViewId("window".into()),
        sequence: InputSequence(sequence),
        action: action.into(),
        location,
    }
}

fn input(snapshot: &StatusSnapshot, filename: &str, sequence: u64, action: &str) -> StatusInput {
    captured(
        snapshot,
        StatusLocation::File {
            id: file(snapshot, filename, None).id,
        },
        sequence,
        action,
    )
}

async fn body(
    service: &StatusService,
    snapshot: &StatusSnapshot,
    file: &StatusFile,
) -> BufferSnapshot {
    service
        .body_snapshot(&snapshot.document, file.id, file.generation)
        .await
        .unwrap()
        .snapshot
        .unwrap()
}

fn body_location(
    file: &StatusFile,
    body: &BufferSnapshot,
    block: &forge_buffer::block::BufferBlock,
    row: usize,
) -> StatusLocation {
    let position = TextPosition { row, column: 0 };
    StatusLocation::Body {
        file: file.id,
        generation: file.generation,
        revision: body.revision,
        block: block.id.clone(),
        position,
        target: block.target_at(position).cloned(),
    }
}

fn rows(body: &BufferSnapshot) -> Vec<&str> {
    body.block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect()
}

#[tokio::test]
async fn opening_publishes_all_headers_without_bodies_then_demand_adds_exact_rows() {
    let (directory, service, diff, _) = fixture().await;
    let (snapshot, timing) = service
        .open_with_timing(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    assert_eq!(snapshot.file.len(), 1);
    assert_eq!(timing.file_count, snapshot.file.len());
    assert_eq!(diff.usage().active_jobs, 0);
    let record = file(&snapshot, "sample.txt", None);
    assert_eq!(record.change, "modified");
    assert_eq!(
        record.stats,
        StatusStatistics::Exact {
            added: 1,
            deleted: 1
        }
    );
    assert!(
        service
            .body_snapshot(&snapshot.document, record.id, record.generation)
            .await
            .unwrap()
            .snapshot
            .is_none()
    );
    let encoded = serde_json::to_value(&snapshot).unwrap();
    assert!(encoded.get("block").is_none());
    assert!(encoded["context"].get("config_source").is_none());
    assert!(encoded["file"][0].get("metadata").is_none());
    let delivery = service
        .demand(input(&snapshot, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    assert_eq!(delivery.state, BodyState::Ready);
    assert!(delivery.patch.is_none());
    assert_eq!(
        rows(&delivery.snapshot.unwrap()),
        vec!["@@ +1 -1", "old", "new"]
    );
    assert_eq!(
        service.snapshot(&snapshot.document).await.unwrap(),
        snapshot
    );
    let warm = service
        .demand(input(&snapshot, "sample.txt", 2, "demand"))
        .await
        .unwrap();
    assert!(warm.patch.is_none() && warm.snapshot.is_none());
}

#[tokio::test]
async fn changed_file_refresh_retires_old_body_before_reloading() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let loaded = service
        .demand(input(&snapshot, "sample.txt", 1, "demand"))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    let stale = body_location(&snapshot.file[0], &loaded, &loaded.block[1], 0);
    std::fs::write(directory.path().join("sample.txt"), "newer content\n").unwrap();
    let delta = service.refresh(&snapshot.document).await.unwrap().unwrap();
    assert_eq!(delta.file[0].id, snapshot.file[0].id);
    assert_eq!(delta.file[0].generation, snapshot.file[0].generation + 1);
    let current = service.snapshot(&snapshot.document).await.unwrap();
    assert!(
        service
            .open_target(captured(&current, stale, 2, "open"))
            .await
            .is_err()
    );
    assert!(
        service
            .body_snapshot(
                &current.document,
                snapshot.file[0].id,
                snapshot.file[0].generation
            )
            .await
            .is_err()
    );
    let loaded = service
        .demand(input(&current, "sample.txt", 3, "demand"))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    assert!(rows(&loaded).contains(&"newer content"));
}

/// Retry both cached-analysis validation and fresh content reads after repository invalidation.
#[test]
fn invalidated_body_demand_remains_retryable_after_unchanged_refresh() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(1)
        .build()
        .unwrap();
    runtime.block_on(async {
        for cached_analysis in [true, false] {
            let (directory, service, _, _) = fixture().await;
            let snapshot = service
                .open(DocumentId("retry-body".into()), directory.path().to_owned())
                .await
                .unwrap();
            let owner = service.get(&snapshot.document).unwrap();
            let repository = Arc::clone(&owner.lock().await.repository);
            if !cached_analysis {
                repository.invalidate().unwrap();
            }

            // Hold the only blocking worker until demand has captured its repository generation.
            let (started, waiting) = tokio::sync::oneshot::channel();
            let (release, held) = std::sync::mpsc::channel();
            let blocker = tokio::task::spawn_blocking(move || {
                started.send(()).unwrap();
                held.recv().unwrap();
            });
            waiting.await.unwrap();
            let mut pending = Box::pin(service.demand(input(&snapshot, "sample.txt", 1, "demand")));
            std::future::poll_fn(|context| {
                assert!(pending.as_mut().poll(context).is_pending());
                std::task::Poll::Ready(())
            })
            .await;
            repository.invalidate().unwrap();
            release.send(()).unwrap();
            blocker.await.unwrap();

            let deferred = pending.await.unwrap();
            assert_eq!(deferred.state, BodyState::Deferred);
            assert!(deferred.more);
            assert!(deferred.snapshot.is_none() && deferred.patch.is_none());
            assert!(service.refresh(&snapshot.document).await.unwrap().is_none());
            let loaded = service
                .demand(input(&snapshot, "sample.txt", 2, "demand"))
                .await
                .unwrap();
            assert_eq!(loaded.state, BodyState::Ready);
            assert!(!loaded.more);
            assert_eq!(
                rows(&loaded.snapshot.unwrap()),
                vec!["@@ +1 -1", "old", "new"]
            );
        }
    });
}

/// Keep genuine object-read failures terminal instead of repeatedly demanding the same broken source.
#[tokio::test]
async fn corrupt_body_source_remains_a_visible_terminal_failure() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open(
            DocumentId("failed-body".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let owner = service.get(&snapshot.document).unwrap();
    let (repository, object) = {
        let document = owner.lock().await;
        let record = &document.file.values().next().unwrap().record;
        let forge_git::snapshot::PathState::Tracked { index, .. } = &record.state else {
            panic!("fixture source must be tracked");
        };
        (Arc::clone(&document.repository), index.object.to_string())
    };
    let object_path = directory
        .path()
        .join(".git/objects")
        .join(&object[..2])
        .join(&object[2..]);
    let original = std::fs::read(&object_path).unwrap();
    std::fs::remove_file(&object_path).unwrap();
    std::fs::write(&object_path, b"invalid object").unwrap();
    repository.invalidate().unwrap();

    let failed = service
        .demand(input(&snapshot, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let BodyState::Failed(diagnostic) = &failed.state else {
        panic!("corrupt content must remain a failure: {:?}", failed.state);
    };
    assert!(!failed.more);
    assert_eq!(
        rows(failed.snapshot.as_ref().unwrap()),
        vec![diagnostic.as_str()]
    );
    std::fs::write(&object_path, original).unwrap();
    let retained = service
        .demand(input(&snapshot, "sample.txt", 2, "demand"))
        .await
        .unwrap();
    assert_eq!(retained.state, failed.state);
    assert!(!retained.more);
}

#[tokio::test]
async fn body_continuation_is_bounded_and_loaded_blocks_remain() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("added.txt"), "line\n".repeat(1000)).unwrap();
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let mut first = None;
    for sequence in 1..=8 {
        let delivery = service
            .demand(input(&snapshot, "added.txt", sequence, "demand"))
            .await
            .unwrap();
        if sequence == 1 {
            let snapshot = delivery.snapshot.unwrap();
            assert!(rows(&snapshot).len() <= 129);
            first = Some(snapshot.block[1].clone());
        } else {
            let patch = delivery.patch.unwrap();
            patch.validate().unwrap();
            assert!(
                patch
                    .text_edit
                    .iter()
                    .map(|edit| edit.text.row_count())
                    .sum::<usize>()
                    <= 129
            );
            assert!(delivery.snapshot.is_none());
        }
        assert_eq!(delivery.more, sequence < 8);
    }
    let loaded = body(&service, &snapshot, file(&snapshot, "added.txt", None)).await;
    assert_eq!(rows(&loaded).len(), 1001);
    // The projection renders every source row as its own block, so all eight continuation chunks
    // remain loaded exactly when the body holds one source block per added line.
    assert_eq!(
        loaded
            .block
            .iter()
            .filter(|block| block.id.0.contains(":source:"))
            .count(),
        1000
    );
    assert_eq!(loaded.block[1], first.unwrap());
    assert_eq!(
        loaded.block[0].metadata.fold[0].end.block,
        loaded.block.last().unwrap().id
    );
    assert_eq!(
        service.snapshot(&snapshot.document).await.unwrap().revision,
        snapshot.revision
    );
}

#[cfg(unix)]
#[tokio::test]
async fn symlinked_worktree_preserves_local_deleted_files_and_ignored_identity() {
    let (directory, service, _, _) = fixture().await;
    let storage = tempfile::tempdir().unwrap();
    let alias = storage.path().join("worktree-link");
    std::os::unix::fs::symlink(directory.path(), &alias).unwrap();
    let selected = vec![forge_git::RepositoryPath::new(b"sample.txt".to_vec()).unwrap()];
    crate::ignored::update(
        storage.path(),
        directory.path(),
        &Default::default(),
        &selected,
        true,
    )
    .unwrap();
    assert_eq!(
        crate::ignored::load(Some(storage.path()), &alias).unwrap(),
        selected.into_iter().collect(),
    );
    for deleted in [false, true] {
        if deleted {
            std::fs::remove_file(directory.path().join("sample.txt")).unwrap();
        }
        let snapshot = service
            .open_local(
                DocumentId(format!("linked-local-{deleted}")),
                alias.join("sample.txt"),
            )
            .await
            .unwrap();
        assert_eq!(snapshot.file.len(), 1);
        assert!(service.close(&snapshot.document));
    }
}

#[tokio::test]
async fn stale_ignore_rejects_changed_sources_and_does_not_relax_discard() {
    let (directory, service, _, _) = fixture().await;
    let storage = tempfile::tempdir().unwrap();
    service
        .configure_ignored_directory(storage.path().to_owned())
        .unwrap();
    let opened = service
        .open(
            DocumentId("changed-ignore".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let ignore = input(&opened, "sample.txt", 1, "ignore");
    let discard = input(&opened, "sample.txt", 2, "discard");
    std::fs::write(
        directory.path().join("sample.txt"),
        "changed after capture\n",
    )
    .unwrap();
    service.refresh(&opened.document).await.unwrap();
    let error = match service.act(ignore, None).await {
        Err(error) => error,
        Ok(_) => panic!("changed source must not be silently ignored"),
    };
    assert!(
        error
            .to_string()
            .contains("selected file changed during refresh"),
        "{error}"
    );
    let error = match service.act(discard, None).await {
        Err(error) => error,
        Ok(_) => panic!("stale discard must remain rejected"),
    };
    assert!(error.to_string().contains("stale revision"), "{error}");
    assert!(
        crate::ignored::load(Some(storage.path()), directory.path())
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"changed after capture\n"
    );
}

async fn semantic_status_fixture(
    staged: bool,
) -> (
    tempfile::TempDir,
    StatusService,
    StatusSnapshot,
    String,
    String,
) {
    let (directory, service, _, _) = fixture().await;
    let old = "pub fn visible() { before(); }\nfn hidden() { private_before(); }\n".to_owned();
    let new = old.replace("before", "after");
    std::fs::write(directory.path().join("sample.rs"), &old).unwrap();
    stage_and_commit(directory.path(), &["sample.rs"], "semantic baseline");
    std::fs::write(directory.path().join("sample.rs"), &new).unwrap();
    if staged {
        stage(directory.path(), &["sample.rs"]);
    }
    let opened = service
        .open(
            DocumentId(format!("semantic-status-{staged}")),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    (directory, service, opened, old, new)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn linked_status_diff_controls_publish_matching_bodies_and_protocol_fixture() {
    use forge_diff::view::{DiffPresentation, DiffVisibility};
    let (directory, service, initial, _, _) = semantic_status_fixture(false).await;
    let revision = head_commit(directory.path()).to_string();
    let child_initial = service
        .open_comparison(
            DocumentId(format!("{}:commit:{revision}", initial.document.0)),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: revision,
                head: None,
                worktree: false,
                path: None,
            },
            initial.diff_view,
        )
        .await
        .unwrap();
    let parent_full = service
        .demand(input(&initial, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    let child_full = service
        .demand(input(&child_initial, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    let current = service.snapshot(&initial.document).await.unwrap();
    let semantic = service
        .change_diff_views(
            input(&current, "sample.rs", 2, "toggle_presentation"),
            vec![child_initial.document.clone()],
        )
        .await
        .unwrap();
    assert_eq!(semantic.len(), 2);
    assert!(semantic.iter().all(|update| {
        update
            .delta
            .diff_view
            .is_some_and(|options| options.presentation == DiffPresentation::Semantic)
    }));
    let current = service.snapshot(&initial.document).await.unwrap();
    let selected = file(&current, "sample.rs", None);
    let loaded = body(&service, &current, selected).await;
    let heading = loaded
        .block
        .iter()
        .find(|block| {
            block
                .text
                .row(0)
                .is_some_and(|row| row.contains("pub fn visible()"))
        })
        .unwrap();
    let mut expand = input(&current, "sample.rs", 3, "source_expand");
    expand.location = body_location(selected, &loaded, heading, 0);
    let expanded = service.source_section(expand).await.unwrap();
    let public = service
        .change_diff_views(
            input(&current, "sample.rs", 4, "toggle_visibility"),
            vec![child_initial.document.clone()],
        )
        .await
        .unwrap();
    for document in [&initial.document, &child_initial.document] {
        let snapshot = service.snapshot(document).await.unwrap();
        assert_eq!(snapshot.diff_view.visibility, DiffVisibility::Public);
        let text =
            rows(&body(&service, &snapshot, file(&snapshot, "sample.rs", None)).await).join("\n");
        assert!(
            !text.contains("private_") && !text.contains("hidden"),
            "{text}"
        );
    }
    let current = service.snapshot(&initial.document).await.unwrap();
    let inherited = service
        .open_comparison(
            DocumentId("inherited-controls".into()),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: "HEAD".into(),
                head: None,
                worktree: false,
                path: None,
            },
            current.diff_view,
        )
        .await
        .unwrap();
    assert_eq!(inherited.diff_view, current.diff_view);
    let inherited_body = service
        .demand(input(&inherited, "sample.rs", 1, "demand"))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    assert!(
        inherited_body
            .block
            .iter()
            .any(|block| !block.metadata.section.is_empty())
    );
    assert!(!rows(&inherited_body).join("\n").contains("hidden"));
    assert!(service.close(&inherited.document));
    let public_full = service
        .change_diff_views(
            input(&current, "sample.rs", 5, "toggle_presentation"),
            vec![child_initial.document.clone()],
        )
        .await
        .unwrap();
    let current = service.snapshot(&initial.document).await.unwrap();
    let all_full = service
        .change_diff_views(
            input(&current, "sample.rs", 6, "toggle_visibility"),
            vec![child_initial.document.clone()],
        )
        .await
        .unwrap();
    for document in [&initial.document, &child_initial.document] {
        let snapshot = service.snapshot(document).await.unwrap();
        assert_eq!(snapshot.diff_view, Default::default());
        let text =
            rows(&body(&service, &snapshot, file(&snapshot, "sample.rs", None)).await).join("\n");
        assert!(text.contains("private_"), "{text}");
    }
    let current = service.snapshot(&initial.document).await.unwrap();
    assert!(
        service
            .change_diff_views(
                input(&current, "sample.rs", 7, "toggle_visibility"),
                vec![DocumentId("missing-related".into())]
            )
            .await
            .is_err()
    );
    assert_eq!(
        service.snapshot(&initial.document).await.unwrap().diff_view,
        current.diff_view
    );
    let fixture = serde_json::json!({
        "initial": initial, "child_initial": child_initial, "full": [parent_full, child_full],
        "semantic": semantic, "expanded": expanded, "public": public, "public_full": public_full, "all_full": all_full,
    });
    forge_recorded_fixtures::assert_matches_recording("status-semantic.json", &fixture);
}
