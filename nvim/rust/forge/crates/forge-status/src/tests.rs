use std::{path::Path, sync::Arc, time::Duration};

use forge_buffer::{
    block::TextPosition,
    identity::{DocumentId, InputSequence, ViewId},
    patch::BufferSnapshot,
};
use forge_diff::{engine::DiffEngine, syntax::SyntaxEngine};
use forge_git::{
    command::{CommandLimits, read_command},
    store::RepositoryStore,
    writer::{GitWriteService, TargetCompletion},
};

use forge_git::test_support::{
    head_commit, init_repository, set_config, set_identity, stage, stage_and_commit,
};

use crate::protocol::{StatusFile, StatusStatistics};
use crate::{
    BodyState, StatusInput, StatusLocation, StatusSection, StatusSelection, StatusService,
    StatusSnapshot,
};

fn git(root: &Path, arguments: &[&str]) -> Vec<u8> {
    let mut command = std::process::Command::new("git");
    command.arg("-C").arg(root).args(arguments);
    if arguments.first() == Some(&"commit") {
        command
            .env("GIT_AUTHOR_DATE", "2000-01-01T00:00:00Z")
            .env("GIT_COMMITTER_DATE", "2000-01-01T00:00:00Z");
    }
    let output = std::thread::scope(|scope| {
        scope
            .spawn(|| {
                read_command(
                    &mut command,
                    CommandLimits {
                        stderr_bytes: 4096,
                        timeout: Duration::from_secs(10),
                    },
                    || Ok(()),
                )
            })
            .join()
            .expect("Git fixture command thread")
    })
    .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

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
    let store = Arc::new(RepositoryStore::default());
    let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
    let writer = Arc::new(GitWriteService::new(Arc::clone(&store)));
    let service = StatusService::new(store, Arc::clone(&diff), syntax, Arc::clone(&writer));
    (directory, service, diff, writer)
}

#[test]
fn closing_pending_document_invalidates_adoption_without_releasing_live_charge() {
    let store = Arc::new(crate::document::DocumentAdmissionStore::default());
    let mut admission = Vec::new();
    for index in 0..8 {
        admission.push(store.admit(DocumentId(format!("pending:{index}"))).unwrap());
    }
    assert!(store.cancel(&DocumentId("pending:0".into())));
    assert!(admission[0].check().is_err());
    assert!(
        store.admit(DocumentId("replacement".into())).is_err(),
        "cancel released a still-owned document charge"
    );
    drop(admission.remove(0));
    let replacement = store.admit(DocumentId("replacement".into())).unwrap();
    replacement.check().unwrap();
    store.cancel_all();
    assert!(replacement.check().is_err());
    assert!(admission.iter().all(|admission| admission.check().is_err()));
}

#[tokio::test]
async fn unstage_invalidates_cached_index_when_replacement_timestamp_moves_backwards() {
    let (directory, service, _, writer) = fixture().await;
    stage(directory.path(), &["sample.txt"]);
    let index = directory.path().join(".git/index");
    std::fs::OpenOptions::new()
        .write(true)
        .open(&index)
        .unwrap()
        .set_times(
            std::fs::FileTimes::new()
                .set_modified(std::time::SystemTime::now() + Duration::from_secs(3600)),
        )
        .unwrap();
    let opened = service
        .open(
            DocumentId("unstage-index-clock".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    assert_eq!(opened.file.len(), 1);
    assert_eq!(opened.file[0].section, StatusSection::Staged);
    let outcome = service
        .input(input(&opened, "sample.txt", 1, "unstage"))
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert!(outcome.settlement_diagnostic.is_none());
    assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
    let settled = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(settled.file.len(), 1);
    assert_eq!(settled.file[0].section, StatusSection::Unstaged);
    let delivery = service
        .demand(input(&settled, "sample.txt", 2, "demand"))
        .await
        .unwrap();
    let text = rows(delivery.snapshot.as_ref().unwrap()).join("\n");
    assert!(text.contains("old") && text.contains("new"), "{text}");
    service.refresh(&opened.document).await.unwrap();
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(refreshed.file.len(), 1);
    assert_eq!(refreshed.file[0].section, StatusSection::Unstaged);
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"new\n"
    );
    writer.acknowledge(outcome.operation);
}

#[tokio::test]
async fn forge_ignore_requires_configured_persistence_and_preserves_invalid_store() {
    let (directory, service, _, _) = fixture().await;
    let opened = service
        .open(
            DocumentId("unconfigured-ignore".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let failure = service
        .act(input(&opened, "sample.txt", 1, "ignore"), None)
        .await
        .err()
        .unwrap()
        .to_string();
    assert!(failure.contains("persistence is unavailable"));
    assert!(!git(directory.path(), &["diff", "--name-only"]).is_empty());
    service.close(&opened.document);
    let storage = tempfile::tempdir().unwrap();
    service
        .configure_ignored_directory(storage.path().to_owned())
        .unwrap();
    let expected = Default::default();
    let selected = vec![forge_git::RepositoryPath::new(b"sample.txt".to_vec()).unwrap()];
    crate::ignored::update(storage.path(), directory.path(), &expected, &selected, true).unwrap();
    let file = std::fs::read_dir(storage.path())
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .find(|path| {
            path.extension()
                .is_some_and(|extension| extension == "json")
        })
        .unwrap();
    let payload: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&file).unwrap()).unwrap();
    assert_eq!(payload["version"], 1);
    assert_eq!(payload["ignored_paths"], serde_json::json!(["sample.txt"]));
    std::fs::write(&file, b"{broken").unwrap();
    assert!(
        service
            .open(
                DocumentId("invalid-ignore".into()),
                directory.path().to_owned()
            )
            .await
            .is_err()
    );
    assert_eq!(std::fs::read(file).unwrap(), b"{broken");
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
async fn retired_file_handles_cannot_address_a_recreated_path() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("temporary.txt"), "first\n").unwrap();
    let opened = service
        .open(DocumentId("retired".into()), directory.path().to_owned())
        .await
        .unwrap();
    let retired = file(&opened, "temporary.txt", None).id;
    std::fs::remove_file(directory.path().join("temporary.txt")).unwrap();
    let removed = service.refresh(&opened.document).await.unwrap().unwrap();
    assert!(removed.removed.contains(&retired));
    std::fs::write(directory.path().join("temporary.txt"), "second\n").unwrap();
    service.refresh(&opened.document).await.unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    assert_ne!(file(&current, "temporary.txt", None).id, retired);
    assert!(
        service
            .demand(captured(
                &current,
                StatusLocation::File { id: retired },
                1,
                "demand"
            ))
            .await
            .is_err()
    );
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

#[tokio::test]
async fn read_demand_survives_unrelated_revision_but_rejects_forged_or_replayed_input() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    assert!(
        service
            .demand(captured(
                &snapshot,
                StatusLocation::File { id: 99999 },
                1,
                "demand"
            ))
            .await
            .is_err()
    );
    let loaded = service
        .demand(input(&snapshot, "sample.txt", 1, "demand"))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    let mut location = body_location(&snapshot.file[0], &loaded, &loaded.block[1], 0);
    if let StatusLocation::Body { target, .. } = &mut location {
        *target = Some(forge_buffer::identity::TargetId("forged".into()));
    }
    assert!(
        service
            .open_target(captured(&snapshot, location, 2, "open"))
            .await
            .is_err()
    );
    assert!(
        service
            .demand(input(&snapshot, "sample.txt", 1, "demand"))
            .await
            .is_err()
    );
    std::fs::write(directory.path().join("other"), "new\n").unwrap();
    service.refresh(&snapshot.document).await.unwrap();
    assert!(
        service
            .demand(input(&snapshot, "sample.txt", 2, "demand"))
            .await
            .is_ok()
    );
    let mut future = input(&snapshot, "sample.txt", 3, "demand");
    future.revision = forge_buffer::identity::DocumentRevision(999);
    assert!(service.demand(future).await.is_err());
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

#[tokio::test]
async fn physical_selection_stages_multiple_native_file_targets() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join("first.txt"), "first\n").unwrap();
    std::fs::write(directory.path().join("second.txt"), "second\n").unwrap();
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let selected = StatusSelection {
        target: ["first.txt", "second.txt"]
            .into_iter()
            .map(|name| input(&snapshot, name, 1, "stage").location)
            .collect(),
    };
    let outcome = service
        .input_selection(input(&snapshot, "first.txt", 1, "stage"), selected)
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    assert_eq!(git(directory.path(), &["show", ":first.txt"]), b"first\n");
    assert_eq!(git(directory.path(), &["show", ":second.txt"]), b"second\n");
    writer.acknowledge(outcome.operation);
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
async fn whole_section_stages_current_contents_and_only_selected_paths() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join("first.txt"), "first\n").unwrap();
    std::fs::write(directory.path().join("second.txt"), "second\n").unwrap();
    let snapshot = service
        .open(
            DocumentId("section-stale".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let mut captured = input(&snapshot, "sample.txt", 1, "stage");
    captured.location = StatusLocation::Section {
        section: StatusSection::Unstaged,
    };
    std::fs::write(
        directory.path().join("second.txt"),
        "changed before section action\n",
    )
    .unwrap();
    std::fs::write(directory.path().join("unselected.txt"), "not selected\n").unwrap();
    let outcome = service
        .input(captured)
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    assert_eq!(
        git(directory.path(), &["show", ":second.txt"]),
        b"changed before section action\n"
    );
    assert!(git(directory.path(), &["ls-files", "unselected.txt"]).is_empty());
    writer.acknowledge(outcome.operation);
}

#[tokio::test]
async fn queued_ignore_and_unignore_preserve_unchanged_file_targets() {
    let (directory, service, _, _) = fixture().await;
    let storage = tempfile::tempdir().unwrap();
    service
        .configure_ignored_directory(storage.path().to_owned())
        .unwrap();
    std::fs::write(directory.path().join("second.txt"), "second\n").unwrap();
    let opened = service
        .open(
            DocumentId("queued-ignore".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let second = input(&opened, "second.txt", 2, "ignore");
    service
        .act(input(&opened, "sample.txt", 1, "ignore"), None)
        .await
        .unwrap();
    assert!(service.snapshot(&opened.document).await.unwrap().revision > opened.revision);
    service.act(second, None).await.unwrap();
    let ignored = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(
        file(&ignored, "sample.txt", None).section,
        StatusSection::Ignored
    );
    assert_eq!(
        file(&ignored, "second.txt", None).section,
        StatusSection::Ignored
    );

    let second = input(&ignored, "second.txt", 4, "unstage");
    service
        .act(input(&ignored, "sample.txt", 3, "unstage"), None)
        .await
        .unwrap();
    service.act(second, None).await.unwrap();
    assert!(
        crate::ignored::load(Some(storage.path()), directory.path())
            .unwrap()
            .is_empty()
    );
    assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"new\n"
    );
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

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn optimistic_pending_index_is_shared_with_reopened_view_and_survives_close() {
    let (directory, service, _, _) = fixture().await;
    let repository = service
        .store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let scope =
        forge_git::mutation::MutationScope::Index(repository.identity.worktree.clone().unwrap());
    let gate = service.store.writes.admit(vec![scope], 1).unwrap();
    let guard = service.store.writes.start(gate).unwrap().unwrap();
    let first = service
        .open(DocumentId("first-view".into()), directory.path().to_owned())
        .await
        .unwrap();
    let ticket = service
        .input(input(&first, "sample.txt", 1, "stage"))
        .await
        .unwrap();
    let second = service
        .open(
            DocumentId("second-view".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    assert_eq!(second.pending.len(), 1);
    assert_eq!(
        file(&second, "sample.txt", None).section,
        StatusSection::Staged
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    assert!(service.close(&first.document));
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    let outcome = ticket.finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    let current = service.snapshot(&second.document).await.unwrap();
    assert!(current.pending.is_empty());
    assert_eq!(
        file(&current, "sample.txt", None).section,
        StatusSection::Staged
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    assert_eq!(service.store.writes.usage().operations, 0);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn changed_hunk_source_rejects_queued_stage_and_refreshes_body() {
    let (directory, service, _, _) = fixture().await;
    let opened = service
        .open(
            DocumentId("changed-hunk".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let selected = file(&current, "sample.txt", Some(StatusSection::Unstaged));
    let loaded = body(&service, &current, selected).await;
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| *row == "new")
                .map(|row| (block, row))
        })
        .unwrap();
    let repository = service
        .store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let gate = service
        .store
        .writes
        .admit(
            vec![forge_git::mutation::MutationScope::Index(
                repository.identity.worktree.clone().unwrap(),
            )],
            0,
        )
        .unwrap();
    let guard = service.store.writes.start(gate).unwrap().unwrap();
    let mut capture = input(&current, "sample.txt", 2, "stage");
    capture.location = body_location(selected, &loaded, block, row);
    let mut updates = service.subscribe_updates();
    let ticket = service.input(capture).await.unwrap();
    let optimistic = service.snapshot(&opened.document).await.unwrap();
    assert!(!optimistic.pending.is_empty());
    assert!(
        optimistic
            .section
            .iter()
            .any(|section| section.kind == StatusSection::Staged)
    );
    std::fs::write(
        directory.path().join("sample.txt"),
        b"external replacement\n",
    )
    .unwrap();
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    let outcome = ticket.finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Rejected),
        "{outcome:?}"
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"external replacement\n"
    );
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    assert!(refreshed.pending.is_empty());
    assert!(
        !refreshed
            .section
            .iter()
            .any(|section| section.kind == StatusSection::Staged)
    );
    let settled = std::iter::from_fn(|| updates.try_recv().ok())
        .find(|update| update.phase == "settled")
        .unwrap();
    assert!(!settled.delta.pending.contains(&outcome.operation.value()));
    service
        .demand(input(&refreshed, "sample.txt", 3, "demand"))
        .await
        .unwrap();
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    let loaded = body(
        &service,
        &refreshed,
        file(&refreshed, "sample.txt", Some(StatusSection::Unstaged)),
    )
    .await;
    assert!(rows(&loaded).contains(&"external replacement"));
    assert!(!rows(&loaded).contains(&"new"));
}

async fn selected_change(
    service: &StatusService,
    snapshot: &StatusSnapshot,
    sequence: u64,
    text: &str,
) -> StatusInput {
    let selected = file(snapshot, "sample.txt", Some(StatusSection::Unstaged));
    let loaded = body(service, snapshot, selected).await;
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| *row == text)
                .map(|row| (block, row))
        })
        .unwrap();
    let mut capture = input(snapshot, "sample.txt", sequence, "stage");
    capture.location = body_location(selected, &loaded, block, row);
    capture
}

#[tokio::test]
async fn hunk_rejects_deleted_source_then_whole_file_stage_records_deletion() {
    let (directory, service, _, _) = fixture().await;
    let opened = service
        .open(
            DocumentId("deleted-hunk-source".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let hunk = selected_change(&service, &snapshot, 2, "new").await;
    std::fs::remove_file(directory.path().join("sample.txt")).unwrap();
    let rejected = service.input(hunk).await.unwrap().finish().await.unwrap();
    assert!(
        rejected
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Rejected),
        "{rejected:?}"
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    assert!(refreshed.pending.is_empty());
    assert_eq!(
        file(&refreshed, "sample.txt", Some(StatusSection::Unstaged)).change,
        "deleted"
    );
    let completed = service
        .input(input(&snapshot, "sample.txt", 3, "stage"))
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert!(
        completed
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{completed:?}"
    );
    assert!(git(directory.path(), &["ls-files", "sample.txt"]).is_empty());
    assert!(!directory.path().join("sample.txt").exists());
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
async fn shared_status_controls_keep_semantic_bodies_lazy_and_visibility_independent() {
    use forge_diff::view::{DiffPresentation, DiffVisibility};
    let (_directory, service, opened, _, _) = semantic_status_fixture(false).await;
    assert_eq!(opened.diff_view.presentation, DiffPresentation::Full);
    assert_eq!(opened.diff_view.visibility, DiffVisibility::All);
    service
        .demand(input(&opened, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let delivery = service
        .change_diff_view(input(&current, "sample.rs", 2, "toggle_presentation"))
        .await
        .unwrap()
        .unwrap();
    assert_eq!(
        delivery.delta.diff_view.unwrap().presentation,
        DiffPresentation::Semantic
    );
    let current = service.snapshot(&opened.document).await.unwrap();
    let selected = file(&current, "sample.rs", None);
    let loaded = body(&service, &current, selected).await;
    let text = rows(&loaded).join("\n");
    assert!(text.contains("pub fn visible()"), "{text}");
    assert!(text.contains("fn hidden()"), "{text}");
    assert!(!text.contains("before()"), "{text}");
    assert!(!text.contains("after()"), "{text}");
    assert!(
        loaded
            .block
            .iter()
            .flat_map(|block| &block.metadata.fold)
            .any(|fold| fold
                .collapsed_suffix
                .as_ref()
                .is_some_and(|suffix| suffix.contains("1 change")))
    );
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
    service.source_section(expand).await.unwrap();
    let loaded = body(&service, &current, selected).await;
    let text = rows(&loaded).join("\n");
    assert!(
        text.contains("before()") && text.contains("after()"),
        "{text}"
    );
    service
        .change_diff_view(input(&current, "sample.rs", 4, "toggle_visibility"))
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let loaded = body(&service, &current, file(&current, "sample.rs", None)).await;
    let text = rows(&loaded).join("\n");
    assert!(
        !text.contains("hidden") && !text.contains("private_"),
        "{text}"
    );
    assert!(text.contains("after()"), "{text}");
    service
        .change_diff_view(input(&current, "sample.rs", 5, "toggle_presentation"))
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(current.diff_view.presentation, DiffPresentation::Full);
    assert_eq!(current.diff_view.visibility, DiffVisibility::Public);
    let text = rows(&body(&service, &current, file(&current, "sample.rs", None)).await).join("\n");
    assert!(
        text.contains("after()") && !text.contains("private_"),
        "{text}"
    );
    service.refresh(&opened.document).await.unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(current.diff_view.visibility, DiffVisibility::Public);
    service
        .change_diff_view(input(&current, "sample.rs", 6, "toggle_visibility"))
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let text = rows(&body(&service, &current, file(&current, "sample.rs", None)).await).join("\n");
    assert!(
        text.contains("private_before") && text.contains("private_after"),
        "{text}"
    );
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
    let artifact = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/status-semantic-fixture.json");
    std::fs::write(&artifact, serde_json::to_vec(&fixture).unwrap()).unwrap();
    println!("semantic status fixture: {}", artifact.display());
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn failed_linked_diff_preparation_preserves_every_usable_body_and_setting() {
    let (directory, service, initial, _, _) = semantic_status_fixture(false).await;
    let child = service
        .open_comparison(
            DocumentId("failed-linked-child".into()),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: "HEAD".into(),
                head: None,
                worktree: false,
                path: None,
            },
            Default::default(),
        )
        .await
        .unwrap();
    service
        .demand(input(&initial, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    service
        .demand(input(&child, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    let parent_before = body(&service, &initial, file(&initial, "sample.rs", None)).await;
    let child_before = body(&service, &child, file(&child, "sample.rs", None)).await;
    service.diff.analysis_pool().close();
    let current = service.snapshot(&initial.document).await.unwrap();
    assert!(
        service
            .change_diff_views(
                input(&current, "sample.rs", 2, "toggle_presentation"),
                vec![child.document.clone()]
            )
            .await
            .is_err()
    );
    assert_eq!(
        service.snapshot(&initial.document).await.unwrap().diff_view,
        initial.diff_view
    );
    assert_eq!(
        service.snapshot(&child.document).await.unwrap().diff_view,
        child.diff_view
    );
    assert_eq!(
        body(&service, &initial, file(&initial, "sample.rs", None)).await,
        parent_before
    );
    assert_eq!(
        body(&service, &child, file(&child, "sample.rs", None)).await,
        child_before
    );
}
