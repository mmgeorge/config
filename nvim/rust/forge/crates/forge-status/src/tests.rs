use std::{path::Path, sync::Arc, time::Duration};

use forge_buffer::{
    block::TextPosition,
    identity::{DocumentId, InputSequence, ViewId},
    input::DocumentInput,
    patch::BufferSnapshot,
};
use forge_diff::{
    cache::CacheLimits,
    engine::DiffEngine,
    syntax::{SyntaxEngine, SyntaxLimits},
};
use forge_git::{
    command::{CommandLimits, read_command},
    store::RepositoryStore,
    writer::{GitWriteService, TargetCompletion},
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
    let output = read_command(
        &mut command,
        CommandLimits {
            stdout_bytes: 64 * 1024,
            stderr_bytes: 4096,
            timeout: Duration::from_secs(10),
        },
        || Ok(()),
    )
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
    git(directory.path(), &["init", "--initial-branch=main"]);
    git(directory.path(), &["config", "user.name", "Forge"]);
    git(
        directory.path(),
        &["config", "user.email", "forge@example.invalid"],
    );
    git(directory.path(), &["config", "core.autocrlf", "false"]);
    std::fs::write(directory.path().join("sample.txt"), "old\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "initial"]);
    std::fs::write(directory.path().join("sample.txt"), "new\n").unwrap();
    let store = Arc::new(RepositoryStore::default());
    let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let writer = Arc::new(GitWriteService::new(Arc::clone(&store)));
    let service = StatusService::new(store, Arc::clone(&diff), syntax, Arc::clone(&writer));
    (directory, service, diff, writer)
}

#[test]
fn literal_source_uses_shared_admission_and_bounded_first_delivery() {
    let store = Arc::new(RepositoryStore::default());
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let service = crate::source_document::SourceDocumentService::new(store, syntax);
    let text = (0..300)
        .map(|line| format!("message {line}\n"))
        .collect::<String>();
    let opened = service
        .open_text(
            DocumentId("message".into()),
            "Commit message".into(),
            text.into_bytes(),
        )
        .unwrap();
    assert_eq!(opened.snapshot.block[0].text.row_count(), 256);
    assert!(opened.more);
    assert!(
        service
            .open_text(
                DocumentId("large".into()),
                "too large".into(),
                vec![b'a'; 1024 * 1024 + 1]
            )
            .is_err()
    );
    assert!(service.close(&DocumentId("message".into())));
}

#[tokio::test]
async fn staging_with_many_open_syntax_bodies_preserves_highlighting() {
    let (directory, service, _, _) = fixture().await;
    for ordinal in 0..8 {
        let filename = format!("source_{ordinal}.rs");
        std::fs::write(
            directory.path().join(filename),
            format!("fn source_{ordinal}() {{}}\n"),
        )
        .unwrap();
    }
    git(directory.path(), &["add", "--all"]);
    git(directory.path(), &["commit", "-m", "syntax fixtures"]);
    for ordinal in 0..8 {
        let filename = format!("source_{ordinal}.rs");
        std::fs::write(
            directory.path().join(filename),
            format!("fn source_{ordinal}() {{ let changed = {ordinal}; }}\n"),
        )
        .unwrap();
    }
    let opened = service
        .open(
            DocumentId("syntax-pressure".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let mut updates = service.subscribe_updates();
    for ordinal in 0..8 {
        let snapshot = service.snapshot(&opened.document).await.unwrap();
        service
            .demand(input(
                &snapshot,
                &format!("source_{ordinal}.rs"),
                ordinal + 1,
                "demand",
            ))
            .await
            .unwrap();
        let update = tokio::time::timeout(Duration::from_secs(5), updates.recv())
            .await
            .unwrap()
            .unwrap();
        assert_eq!(update.phase, "context");
        assert!(update.diagnostic.is_empty(), "{:?}", update.diagnostic);
    }
    for (ordinal, action) in ["stage", "unstage", "stage", "unstage"]
        .into_iter()
        .enumerate()
    {
        let snapshot = service.snapshot(&opened.document).await.unwrap();
        let ticket = service
            .input(input(&snapshot, "source_0.rs", 20 + ordinal as u64, action))
            .await
            .unwrap();
        let outcome = tokio::time::timeout(Duration::from_secs(10), ticket.finish())
            .await
            .unwrap()
            .unwrap();
        assert!(
            outcome
                .target
                .iter()
                .all(|target| target.completion == TargetCompletion::Completed)
        );
        while let Ok(update) = updates.try_recv() {
            assert!(update.diagnostic.is_empty(), "{:?}", update.diagnostic);
        }
    }
    let owner = service.get(&opened.document).unwrap();
    let document = owner.lock().await;
    for file in document.file.values() {
        assert!(file.old_syntax.is_some(), "{} old syntax absent", file.id);
        assert!(file.new_syntax.is_some(), "{} new syntax absent", file.id);
    }
    assert!(service.syntax.usage().retained_bytes < 1024 * 1024);
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
async fn stale_discard_and_unstage_reject_external_source_and_index_changes() {
    for action in ["discard"] {
        let (directory, service, _, _) = fixture().await;
        let snapshot = service
            .open(DocumentId(action.into()), directory.path().to_owned())
            .await
            .unwrap();
        std::fs::write(
            directory.path().join("sample.txt"),
            "external unseen content\n",
        )
        .unwrap();
        let failure = rejected_input(&service, input(&snapshot, "sample.txt", 1, action)).await;
        assert!(
            failure.to_string().contains("displayed observation"),
            "{failure}"
        );
        assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
        assert_eq!(
            std::fs::read(directory.path().join("sample.txt")).unwrap(),
            b"external unseen content\n"
        );
    }
    let (directory, service, _, _) = fixture().await;
    git(directory.path(), &["add", "sample.txt"]);
    let snapshot = service
        .open(
            DocumentId("unstage-stale".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    std::fs::write(directory.path().join("sample.txt"), "externally staged\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    let failure = rejected_input(&service, input(&snapshot, "sample.txt", 1, "unstage")).await;
    assert!(
        failure.to_string().contains("selected index changed"),
        "{failure}"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"externally staged\n"
    );
}

#[tokio::test]
async fn observed_intent_to_add_remains_stageable() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join("intent.txt"), "intent content\n").unwrap();
    git(directory.path(), &["add", "-N", "intent.txt"]);
    let snapshot = service
        .open(
            DocumentId("intent-to-add".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let outcome = service
        .input(input(&snapshot, "intent.txt", 1, "stage"))
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
    assert_eq!(
        git(directory.path(), &["show", ":intent.txt"]),
        b"intent content\n"
    );
    writer.acknowledge(outcome.operation);
}

#[tokio::test]
async fn observed_rename_rejects_recreated_index_origin() {
    for changed in [false, true] {
        let (directory, service, _, writer) = fixture().await;
        git(directory.path(), &["mv", "sample.txt", "renamed.txt"]);
        let snapshot = service
            .open(
                DocumentId(format!("rename-{changed}")),
                directory.path().to_owned(),
            )
            .await
            .unwrap();
        if changed {
            std::fs::write(directory.path().join("sample.txt"), "new origin\n").unwrap();
            git(directory.path(), &["add", "sample.txt"]);
            let failure = rejected_input(
                &service,
                section_input(&snapshot, "Staged", "renamed.txt", 1, "unstage"),
            )
            .await;
            assert!(
                failure.to_string().contains("rename index changed"),
                "{failure}"
            );
            assert_eq!(
                git(directory.path(), &["show", ":sample.txt"]),
                b"new origin\n"
            );
        } else {
            let outcome = service
                .input(section_input(
                    &snapshot,
                    "Staged",
                    "renamed.txt",
                    1,
                    "unstage",
                ))
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
            writer.acknowledge(outcome.operation);
        }
    }
}

#[tokio::test]
async fn native_file_action_stages_without_lua_patch_reconstruction() {
    let (directory, service, _, writer) = fixture().await;
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let outcome = service
        .input(input(&snapshot, "sample.txt", 1, "stage"))
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    writer.acknowledge(outcome.operation);
}

#[tokio::test]
async fn source_document_retains_exact_revision_and_bounded_continuation() {
    let (directory, _, diff, _) = fixture().await;
    let original = (0..300)
        .map(|row| format!("line {row}\n"))
        .collect::<String>();
    std::fs::write(directory.path().join("historical file.txt"), &original).unwrap();
    git(directory.path(), &["add", "historical file.txt"]);
    git(directory.path(), &["commit", "-m", "historical source"]);
    let service = crate::source_document::SourceDocumentService::new(
        Arc::new(RepositoryStore::default()),
        SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default()),
    );
    let opened = service
        .open(
            DocumentId("revision".into()),
            directory.path().to_owned(),
            "HEAD".into(),
            b"historical file.txt".to_vec(),
        )
        .await
        .unwrap();
    assert!(opened.more);
    assert_eq!(opened.snapshot.block[0].text.row_count(), 256);
    assert_eq!(opened.snapshot.block[0].text.row(0), Some("line 0"));
    std::fs::write(
        directory.path().join("historical file.txt"),
        "new worktree\n",
    )
    .unwrap();
    let delivered = service
        .demand(DocumentInput {
            document: opened.snapshot.document.clone(),
            revision: opened.snapshot.revision,
            view: ViewId("revision-window".into()),
            sequence: InputSequence(1),
            action: "demand".into(),
            block: opened.snapshot.block[0].id.clone(),
            position: TextPosition { row: 0, column: 0 },
            target: None,
        })
        .await
        .unwrap();
    assert_eq!(delivered.state, BodyState::Ready);
    assert!(!delivered.more);
    let snapshot = service.snapshot(&opened.snapshot.document).await.unwrap();
    assert_eq!(snapshot.block.len(), 2);
    assert_eq!(snapshot.block[1].text.row_count(), 44);
    assert_eq!(snapshot.block[1].text.row(43), Some("line 299"));
    service.close_all();
    assert!(service.snapshot(&opened.snapshot.document).await.is_err());
}

#[tokio::test]
async fn source_document_index_revision_is_immutable_and_distinct_from_later_index() {
    let (directory, _, diff, _) = fixture().await;
    let service = crate::source_document::SourceDocumentService::new(
        Arc::new(RepositoryStore::default()),
        SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default()),
    );
    let first = service
        .open(
            DocumentId("index-first".into()),
            directory.path().to_owned(),
            ":0".into(),
            b"sample.txt".to_vec(),
        )
        .await
        .unwrap();
    assert_eq!(first.revision, "index");
    assert_eq!(first.snapshot.block[0].text.row(0), Some("old"));
    git(directory.path(), &["add", "sample.txt"]);
    let second = service
        .open(
            DocumentId("index-second".into()),
            directory.path().to_owned(),
            ":0".into(),
            b"sample.txt".to_vec(),
        )
        .await
        .unwrap();
    assert_ne!(first.object, second.object);
    assert_eq!(second.snapshot.block[0].text.row(0), Some("new"));
    assert_eq!(
        service
            .snapshot(&first.snapshot.document)
            .await
            .unwrap()
            .block[0]
            .text
            .row(0),
        Some("old")
    );
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

fn section_input(
    snapshot: &StatusSnapshot,
    section: &str,
    filename: &str,
    sequence: u64,
    action: &str,
) -> StatusInput {
    let section =
        serde_json::from_value(serde_json::Value::String(section.to_lowercase())).unwrap();
    captured(
        snapshot,
        StatusLocation::File {
            id: file(snapshot, filename, Some(section)).id,
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

fn raw_location(file: &StatusFile, body: &BufferSnapshot) -> Vec<StatusLocation> {
    let mut seen = std::collections::HashSet::new();
    body.block
        .iter()
        .flat_map(|block| {
            block
                .metadata
                .target
                .iter()
                .filter_map(|target| {
                    if target.id.0.starts_with("hunk:") && seen.insert(target.id.clone()) {
                        Some(body_location(file, body, block, target.range.start.row))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect()
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
async fn navigation_boundaries_do_not_become_mutation_targets() {
    let (directory, service, _, _) = fixture().await;
    let opened = service
        .open(
            DocumentId("navigation-boundary".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let boundary = StatusLocation::Boundary { after_files: false };
    assert!(
        service
            .input(captured(&opened, boundary.clone(), 1, "stage"))
            .await
            .is_err()
    );
    let delivery = service
        .navigate(captured(&opened, boundary, 2, "navigate"), true)
        .await
        .unwrap();
    assert!(delivery.body.is_some());
    let delivery = service
        .navigate(
            captured(
                &opened,
                StatusLocation::Boundary { after_files: true },
                3,
                "navigate",
            ),
            false,
        )
        .await
        .unwrap();
    assert!(delivery.effect.is_some());
    let delivery = service
        .navigate(
            captured(
                &opened,
                StatusLocation::Boundary { after_files: true },
                4,
                "navigate",
            ),
            true,
        )
        .await
        .unwrap();
    assert!(delivery.effect.is_none() && !delivery.more);
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
async fn unrelated_refresh_preserves_loaded_rows_and_only_changes_affected_headers() {
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
    std::fs::write(directory.path().join("new.txt"), "untracked\n").unwrap();
    let delta = service.refresh(&snapshot.document).await.unwrap().unwrap();
    assert!(delta.removed.is_empty());
    assert_eq!(delta.file.len(), 1);
    assert_eq!(delta.file[0].path, "new.txt");
    assert_eq!(body(&service, &snapshot, &snapshot.file[0]).await, loaded);
    let current = service.snapshot(&snapshot.document).await.unwrap();
    assert_eq!(file(&current, "sample.txt", None).id, snapshot.file[0].id);
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
async fn refresh_replaces_counts_without_expanding_the_file() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open(DocumentId("counts".into()), directory.path().to_owned())
        .await
        .unwrap();
    std::fs::write(
        directory.path().join("sample.txt"),
        "first\nsecond\nthird\n",
    )
    .unwrap();
    service.refresh(&snapshot.document).await.unwrap();
    let current = service.snapshot(&snapshot.document).await.unwrap();
    assert_eq!(
        current.file[0].stats,
        StatusStatistics::Exact {
            added: 3,
            deleted: 1
        }
    );
    assert!(
        service
            .body_snapshot(
                &current.document,
                current.file[0].id,
                current.file[0].generation
            )
            .await
            .unwrap()
            .snapshot
            .is_none()
    );
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
    assert_eq!(
        loaded
            .block
            .iter()
            .filter(|block| block.id.0.starts_with("body:"))
            .count(),
        8
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
async fn oversized_added_file_is_explicitly_unavailable() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("added.txt"), "line\n".repeat(1001)).unwrap();
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let delivery = service
        .demand(input(&snapshot, "added.txt", 1, "demand"))
        .await
        .unwrap();
    assert!(matches!(delivery.state, BodyState::Unavailable(_)));
    assert!(delivery.snapshot.is_some());
    assert!(!delivery.more);
}

#[tokio::test]
async fn section_input_uses_all_native_section_paths() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join("added.txt"), "added\n").unwrap();
    let snapshot = service
        .open(DocumentId("status".into()), directory.path().to_owned())
        .await
        .unwrap();
    let target = StatusLocation::Section {
        section: StatusSection::Unstaged,
    };
    let outcome = service
        .input(captured(&snapshot, target, 1, "stage"))
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
    assert_eq!(git(directory.path(), &["show", ":added.txt"]), b"added\n");
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    writer.acknowledge(outcome.operation);
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

#[tokio::test]
async fn staged_body_survives_worktree_only_change() {
    let (directory, service, _, _) = fixture().await;
    git(directory.path(), &["add", "sample.txt"]);
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
    std::fs::write(directory.path().join("sample.txt"), "unrelated worktree\n").unwrap();
    service.refresh(&snapshot.document).await.unwrap();
    let current = service.snapshot(&snapshot.document).await.unwrap();
    let staged = file(&current, "sample.txt", Some(StatusSection::Staged));
    assert_eq!(staged.id, snapshot.file[0].id);
    assert_eq!(staged.generation, snapshot.file[0].generation);
    assert_eq!(body(&service, &current, staged).await, loaded);
}

#[tokio::test]
async fn body_gutters_and_folds_keep_source_rows_unchanged() {
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
    assert_eq!(rows(&loaded), vec!["@@ +1 -1", "old", "new"]);
    assert!(!loaded.block[1].metadata.gutter.is_empty());
    assert_eq!(
        loaded.block[0].metadata.fold[0].end.block,
        loaded.block[1].id
    );
    assert!(
        loaded
            .block
            .iter()
            .all(|block| !block.id.0.starts_with("file:"))
    );
}

#[tokio::test]
async fn open_target_resolves_native_source_row_and_view_identity() {
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
    let location = body_location(&snapshot.file[0], &loaded, &loaded.block[1], 1);
    let effect = service
        .open_target(captured(&snapshot, location, 2, "open"))
        .await
        .unwrap();
    assert_eq!(effect.input.view, ViewId("window".into()));
    assert_eq!(effect.input.sequence, InputSequence(2));
    assert_eq!(effect.kind, "open_file");
    assert_eq!(effect.row, 0);
}

#[tokio::test]
async fn navigation_demands_one_batch_and_returns_exact_current_cursor_effect() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open(DocumentId("navigation".into()), directory.path().to_owned())
        .await
        .unwrap();
    let first = service
        .navigate(input(&snapshot, "sample.txt", 1, "navigate"), true)
        .await
        .unwrap();
    assert!(first.more && first.effect.is_none() && first.body.unwrap().snapshot.is_some());
    let next = service
        .navigate(input(&snapshot, "sample.txt", 2, "navigate"), true)
        .await
        .unwrap();
    let effect = next.effect.unwrap();
    assert!(!next.more);
    assert_eq!(effect.sequence, InputSequence(2));
    assert!(
        matches!(effect.location, StatusLocation::Body { file, .. } if file == snapshot.file[0].id)
    );
}

#[tokio::test]
async fn context_input_rejects_refreshed_revision_and_issues_preserve_config() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join(".forge.json"), b"{\"other\":true}").unwrap();
    let snapshot = service
        .open(DocumentId("context".into()), directory.path().to_owned())
        .await
        .unwrap();
    let location = StatusLocation::Context {
        role: "issues".into(),
    };
    let outcome = service
        .context_issues(captured(&snapshot, location.clone(), 1, "open"), "#42")
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    writer.acknowledge(outcome.operation);
    let config: serde_json::Value =
        serde_json::from_slice(&std::fs::read(directory.path().join(".forge.json")).unwrap())
            .unwrap();
    assert_eq!(config["other"], true);
    assert_eq!(config["issues"], serde_json::json!([42]));
    service.refresh(&snapshot.document).await.unwrap();
    assert!(
        service
            .context_action(captured(&snapshot, location, 2, "open"))
            .await
            .unwrap_err()
            .to_string()
            .contains("stale revision")
    );
}

#[tokio::test]
async fn local_document_filters_neighbors_and_stages_with_shared_writer() {
    let (directory, service, _, writer) = fixture().await;
    std::fs::write(directory.path().join("neighbor.txt"), b"neighbor\n").unwrap();
    let snapshot = service
        .open_local(
            DocumentId("local".into()),
            directory.path().join("sample.txt"),
        )
        .await
        .unwrap();
    assert_eq!(snapshot.file.len(), 1);
    assert!(snapshot.context.is_none());
    let outcome = service
        .input(input(&snapshot, "sample.txt", 1, "stage"))
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert_eq!(outcome.target[0].completion, TargetCompletion::Completed);
    writer.acknowledge(outcome.operation);
    service.refresh(&snapshot.document).await.unwrap();
    let current = service.snapshot(&snapshot.document).await.unwrap();
    assert_eq!(current.file.len(), 1);
    assert_eq!(current.file[0].section, StatusSection::Staged);
    assert_ne!(current.file[0].id, snapshot.file[0].id);
}

#[tokio::test]
async fn comparisons_reuse_native_bodies_and_reject_mutations() {
    let (directory, service, _, _) = fixture().await;
    let snapshot = service
        .open_comparison(
            DocumentId("comparison".into()),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: "main".into(),
                worktree: true,
                path: None,
            },
        )
        .await
        .unwrap();
    assert!(snapshot.context.is_none());
    assert_eq!(snapshot.file.len(), 1);
    assert!(
        matches!(&snapshot.view, crate::protocol::StatusView::Comparison { title, worktree: true, .. } if title == "main")
    );
    assert!(
        service
            .input(input(&snapshot, "sample.txt", 1, "stage"))
            .await
            .is_err()
    );
    let loaded = service
        .demand(input(&snapshot, "sample.txt", 2, "demand"))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    assert!(rows(&loaded).contains(&"new"));
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "advance branch"]);
    service.refresh(&snapshot.document).await.unwrap();
    assert_eq!(
        service
            .snapshot(&snapshot.document)
            .await
            .unwrap()
            .file
            .len(),
        1,
        "comparison followed moving branch"
    );
    let empty = service
        .open_comparison(
            DocumentId("empty".into()),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: "HEAD".into(),
                worktree: true,
                path: None,
            },
        )
        .await
        .unwrap();
    assert!(empty.file.is_empty());
    let historical = service
        .open_comparison(
            DocumentId("historical".into()),
            directory.path().to_owned(),
            forge_git::revision::comparison::ComparisonRequest {
                reference: "HEAD".into(),
                worktree: false,
                path: None,
            },
        )
        .await
        .unwrap();
    let effect = service
        .open_target(input(&historical, "sample.txt", 1, "open"))
        .await
        .unwrap();
    assert_eq!(effect.kind, "open_source");
    assert!(effect.source_revision.unwrap().starts_with("object:"));
}

#[tokio::test]
async fn compound_discard_selection_supports_whole_and_mixed_hunks() {
    for mixed in [false, true] {
        let (directory, service, _, writer) = fixture().await;
        git(directory.path(), &["add", "sample.txt"]);
        std::fs::write(directory.path().join("sample.txt"), b"newer\n").unwrap();
        let snapshot = service
            .open(DocumentId("compound".into()), directory.path().to_owned())
            .await
            .unwrap();
        let unstaged = file(&snapshot, "sample.txt", Some(StatusSection::Unstaged));
        let staged = file(&snapshot, "sample.txt", Some(StatusSection::Staged));
        let first = if mixed {
            let loaded = service
                .demand(input(&snapshot, "sample.txt", 1, "demand"))
                .await
                .unwrap()
                .snapshot
                .unwrap();
            raw_location(unstaged, &loaded).remove(0)
        } else {
            StatusLocation::File { id: unstaged.id }
        };
        let outcome = service
            .input_selection(
                captured(&snapshot, first.clone(), 2, "discard"),
                StatusSelection {
                    target: vec![first, StatusLocation::File { id: staged.id }],
                },
            )
            .await
            .unwrap()
            .finish()
            .await
            .unwrap();
        assert_eq!(outcome.target.len(), 1);
        assert_eq!(
            outcome.target[0].completion,
            TargetCompletion::Completed,
            "{outcome:?}"
        );
        assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
        assert_eq!(
            std::fs::read(directory.path().join("sample.txt")).unwrap(),
            b"old\n"
        );
        writer.acknowledge(outcome.operation);
    }
}

#[tokio::test]
async fn compound_discard_selection_keeps_unselected_staged_and_unstaged_hunks() {
    let (directory, service, _, writer) = fixture().await;
    let original = b"one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\neleven\ntwelve\nthirteen\nfourteen\nfifteen\nsixteen\n";
    let staged = b"ONE\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\neleven\ntwelve\nthirteen\nfourteen\nfifteen\nSIXTEEN\n";
    let worktree = b"ONE\ntwo\nthree\nfour\nFIVE\nsix\nseven\neight\nnine\nten\neleven\nTWELVE\nthirteen\nfourteen\nfifteen\nSIXTEEN\n";
    std::fs::write(directory.path().join("sample.txt"), original).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "baseline"]);
    std::fs::write(directory.path().join("sample.txt"), staged).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    std::fs::write(directory.path().join("sample.txt"), worktree).unwrap();

    let snapshot = service
        .open(
            DocumentId("compound-hunks".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let staged_body = service
        .demand(section_input(
            &snapshot,
            "Staged",
            "sample.txt",
            1,
            "demand",
        ))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    let unstaged_body = service
        .demand(section_input(
            &snapshot,
            "Unstaged",
            "sample.txt",
            2,
            "demand",
        ))
        .await
        .unwrap()
        .snapshot
        .unwrap();
    let staged = raw_location(
        file(&snapshot, "sample.txt", Some(StatusSection::Staged)),
        &staged_body,
    );
    let unstaged = raw_location(
        file(&snapshot, "sample.txt", Some(StatusSection::Unstaged)),
        &unstaged_body,
    );
    assert_eq!(staged.len(), 2);
    assert_eq!(unstaged.len(), 2);
    let captured = captured(&snapshot, unstaged[1].clone(), 3, "discard");
    let selection = StatusSelection {
        target: vec![unstaged[1].clone(), staged[0].clone()],
    };
    let outcome = service
        .input_selection(captured, selection)
        .await
        .unwrap()
        .finish()
        .await
        .unwrap();
    assert_eq!(outcome.target.len(), 1);
    assert_eq!(
        outcome.target[0].completion,
        TargetCompletion::Completed,
        "{outcome:?}"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        b"one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\neleven\ntwelve\nthirteen\nfourteen\nfifteen\nSIXTEEN\n"
    );
    assert_eq!(
        std::fs::read(directory.path().join("sample.txt")).unwrap(),
        b"one\ntwo\nthree\nfour\nFIVE\nsix\nseven\neight\nnine\nten\neleven\ntwelve\nthirteen\nfourteen\nfifteen\nSIXTEEN\n"
    );
    writer.acknowledge(outcome.operation);
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
async fn syntax_capture_failure_preserves_status_and_source_text() {
    let (directory, _, diff, _) = fixture().await;
    let old = "fn before() {}\nfn retained() {}\n";
    let new = "fn after() {}\nfn retained() {}\n";
    std::fs::write(directory.path().join("syntax.rs"), old).unwrap();
    git(directory.path(), &["add", "syntax.rs"]);
    git(directory.path(), &["commit", "-m", "syntax source"]);
    std::fs::write(directory.path().join("syntax.rs"), new).unwrap();
    let store = Arc::new(RepositoryStore::default());
    let syntax = SyntaxEngine::new(
        diff.analysis_pool(),
        SyntaxLimits {
            captures: 1,
            ..SyntaxLimits::default()
        },
    );
    let writer = Arc::new(GitWriteService::new(Arc::clone(&store)));
    let service = StatusService::new(Arc::clone(&store), diff, Arc::clone(&syntax), writer);
    let (opened, timing) = service
        .open_with_timing(
            DocumentId("syntax-status".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    assert!(timing.changed_path_count >= 1);
    assert_eq!(timing.repository_observation.attempt_count, 1);
    assert!(
        timing.repository_observation.overall_ms
            >= timing.repository_observation.collection_total_ms
    );
    assert_eq!(timing.file_count, opened.file.len());
    let mut updates = service.subscribe_updates();
    let delivery = service
        .demand(input(&opened, "syntax.rs", 1, "demand"))
        .await
        .unwrap();
    assert_eq!(delivery.state, BodyState::Ready);
    assert!(delivery.syntax_diagnostic.is_none());
    let update = tokio::time::timeout(std::time::Duration::from_secs(5), updates.recv())
        .await
        .unwrap()
        .unwrap();
    assert_eq!(update.phase, "context");
    assert!(
        update
            .diagnostic
            .iter()
            .any(|diagnostic| diagnostic.contains("CaptureLimit"))
    );
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    let loaded = body(&service, &refreshed, file(&refreshed, "syntax.rs", None)).await;
    let rows: Vec<_> = loaded
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect();
    assert!(rows.contains(&"fn before() {}") && rows.contains(&"fn after() {}"));
    let body = loaded
        .block
        .iter()
        .find(|block| block.text.wire_rows().contains(&"fn after() {}"))
        .unwrap();
    let row = body
        .text
        .wire_rows()
        .iter()
        .position(|text| *text == "fn after() {}")
        .unwrap();
    let captured = captured(
        &refreshed,
        body_location(file(&refreshed, "syntax.rs", None), &loaded, body, row),
        2,
        "open",
    );
    let destination = service.open_target(captured).await.unwrap();
    assert_eq!(destination.row, 0);
    assert_eq!(destination.kind, "open_file");
    let repeated = service
        .demand(input(&refreshed, "syntax.rs", 3, "demand"))
        .await
        .unwrap();
    assert!(repeated.syntax_diagnostic.is_none());

    let source = crate::source_document::SourceDocumentService::new(store, syntax);
    let historical = source
        .open(
            DocumentId("syntax-source".into()),
            directory.path().to_owned(),
            "HEAD".into(),
            b"syntax.rs".to_vec(),
        )
        .await
        .unwrap();
    assert_eq!(historical.state, BodyState::Ready);
    assert!(
        historical
            .syntax_diagnostic
            .unwrap()
            .contains("CaptureLimit")
    );
    assert_eq!(
        historical.snapshot.block[0].text.wire_rows(),
        vec!["fn before() {}", "fn retained() {}"]
    );
    let captured = source
        .open_version(
            DocumentId("syntax-captured".into()),
            forge_git::RepositoryPath::new(b"syntax.rs".to_vec()).unwrap(),
            forge_diff::source::SourceVersion::new(
                new.as_bytes().to_vec(),
                forge_diff::source::Representation::Raw,
            )
            .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(captured.state, BodyState::Ready);
    assert!(captured.syntax_diagnostic.unwrap().contains("CaptureLimit"));
    assert_eq!(
        captured.snapshot.block[0].text.wire_rows(),
        vec!["fn after() {}", "fn retained() {}"]
    );
}
#[tokio::test]
async fn removed_status_row_opens_captured_blob_after_index_changes() {
    use base64::{Engine, engine::general_purpose::STANDARD};
    let (directory, service, diff, _) = fixture().await;
    let snapshot = service
        .open(
            DocumentId("removed-source".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&snapshot, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let snapshot = service.snapshot(&snapshot.document).await.unwrap();
    let loaded = body(&service, &snapshot, &snapshot.file[0]).await;
    let body = loaded
        .block
        .iter()
        .find(|block| block.id.0.starts_with("body:"))
        .unwrap();
    let mut captured = input(&snapshot, "sample.txt", 2, "open");
    captured.location = body_location(&snapshot.file[0], &loaded, body, 0);
    let effect = service.open_target(captured).await.unwrap();
    assert_eq!(effect.kind, "open_source");
    assert_eq!(effect.row, 0);
    git(directory.path(), &["add", "sample.txt"]);
    let source = crate::source_document::SourceDocumentService::new(
        Arc::new(RepositoryStore::default()),
        SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default()),
    );
    let opened = source
        .open(
            DocumentId("captured-source".into()),
            effect.workspace.unwrap().into(),
            effect.source_revision.unwrap(),
            STANDARD.decode(effect.path).unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(opened.snapshot.block[0].text.row(0), Some("old"));
}

#[tokio::test]
async fn display_group_context_stages_nearby_raw_hunks_only() {
    let (directory, service, _, writer) = fixture().await;
    let original: Vec<_> = (0..40).map(|line| format!("line {line}")).collect();
    std::fs::write(
        directory.path().join("sample.txt"),
        original.join("\n") + "\n",
    )
    .unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "many lines"]);
    let mut modified = original.clone();
    modified[5] = "first change".into();
    modified[9] = "nearby change".into();
    modified[30] = "separate change".into();
    std::fs::write(
        directory.path().join("sample.txt"),
        modified.join("\n") + "\n",
    )
    .unwrap();
    let opened = service
        .open(
            DocumentId("group-selection".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let loaded = body(&service, &snapshot, &snapshot.file[0]).await;
    let headers: Vec<_> = loaded
        .block
        .iter()
        .filter(|block| block.id.0.starts_with("hunk-header:"))
        .collect();
    assert_eq!(headers.len(), 2);
    let (body, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| *row == "line 7")
                .map(|row| (block, row))
        })
        .unwrap();
    let target = body.target_at(TextPosition { row, column: 0 }).unwrap();
    assert_eq!(target, &headers[0].metadata.target[0].id);
    let mut captured = input(&snapshot, "sample.txt", 2, "stage");
    captured.location = body_location(&snapshot.file[0], &loaded, body, row);
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
    writer.acknowledge(outcome.operation);
    let index = String::from_utf8(git(directory.path(), &["show", ":sample.txt"])).unwrap();
    assert!(index.contains("first change\n") && index.contains("nearby change\n"));
    assert!(!index.contains("separate change"));
    assert!(
        std::fs::read_to_string(directory.path().join("sample.txt"))
            .unwrap()
            .contains("separate change")
    );
}

#[tokio::test]
async fn ignoring_staged_changes_unstages_and_persists_without_changing_worktree() {
    for kind in ["modified", "new", "deleted", "renamed", "partial"] {
        let (directory, service, _, writer) = fixture().await;
        let storage = tempfile::tempdir().unwrap();
        service.configure_ignored_directory(storage.path().to_owned()).unwrap();
        let selected = match kind {
            "new" => {
                std::fs::write(directory.path().join("added.txt"), "added\n").unwrap();
                "added.txt"
            }
            "deleted" => {
                std::fs::remove_file(directory.path().join("sample.txt")).unwrap();
                "sample.txt"
            }
            "renamed" => {
                std::fs::write(directory.path().join("sample.txt"), "old\n").unwrap();
                std::fs::rename(directory.path().join("sample.txt"), directory.path().join("renamed.txt")).unwrap();
                "renamed.txt"
            }
            _ => "sample.txt",
        };
        git(directory.path(), &["add", "--all"]);
        if kind == "partial" {
            std::fs::write(directory.path().join("sample.txt"), "newer worktree\n").unwrap();
        }
        let before = std::fs::read(directory.path().join(selected)).ok();
        let opened = service.open(DocumentId(format!("ignore-staged-{kind}")), directory.path().to_owned()).await.unwrap();
        let request = section_input(&opened, "staged", selected, 1, "ignore");
        let crate::StatusAction::Write(ticket) = service.act(request, None).await.unwrap() else {
            panic!("staged ignore must own an unstage operation: {kind}")
        };
        let outcome = ticket.finish().await.unwrap();
        assert!(outcome.target.iter().all(|target| target.completion == TargetCompletion::Completed), "{kind}: {outcome:?}");
        assert!(outcome.settlement_diagnostic.is_none(), "{kind}: {:?}", outcome.settlement_diagnostic);
        writer.acknowledge(outcome.operation);
        assert_eq!(std::fs::read(directory.path().join(selected)).ok(), before, "{kind}");
        service.refresh(&opened.document).await.unwrap();
        let current = service.snapshot(&opened.document).await.unwrap();
        assert_eq!(file(&current, selected, None).section, StatusSection::Ignored, "{kind}");
        assert!(git(directory.path(), &["diff", "--cached", "--name-only", "--", selected]).is_empty(), "{kind}");
        assert!(crate::ignored::load(Some(storage.path()), directory.path()).unwrap().iter().any(|path| path.raw() == selected.as_bytes()), "{kind}");
        if kind == "renamed" {
            assert_eq!(file(&current, "sample.txt", None).section, StatusSection::Ignored);
            assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
        }
        assert!(!directory.path().join(".gitignore").exists());
        service.close(&opened.document);
    }
}

#[tokio::test]
async fn ignoring_staged_changes_does_not_hide_a_failed_unstage() {
    let (directory, service, _, writer) = fixture().await;
    let storage = tempfile::tempdir().unwrap();
    service.configure_ignored_directory(storage.path().to_owned()).unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    let opened = service.open(DocumentId("ignore-staged-failure".into()), directory.path().to_owned()).await.unwrap();
    let lock = directory.path().join(".git/index.lock");
    std::fs::write(&lock, "fixture lock").unwrap();
    match service.act(input(&opened, "sample.txt", 1, "ignore"), None).await {
        Err(_) => {}
        Ok(crate::StatusAction::Write(ticket)) => {
            let outcome = ticket.finish().await.unwrap();
            assert!(outcome.target.iter().all(|target| target.completion != TargetCompletion::Completed));
            writer.acknowledge(outcome.operation);
        }
        _ => panic!("failed unstage must not report a projection"),
    }
    std::fs::remove_file(lock).unwrap();
    assert!(crate::ignored::load(Some(storage.path()), directory.path()).unwrap().is_empty());
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    service.refresh(&opened.document).await.unwrap();
    assert_eq!(file(&service.snapshot(&opened.document).await.unwrap(), "sample.txt", None).section, StatusSection::Staged);
    service.close(&opened.document);
}

#[tokio::test]
async fn forge_only_ignore_unignore_and_collected_stage_preserve_git_state() {
    let (directory, service, _, writer) = fixture().await;
    let storage = tempfile::tempdir().unwrap();
    service
        .configure_ignored_directory(storage.path().to_owned())
        .unwrap();
    let opened = service
        .open(DocumentId("ignored".into()), directory.path().to_owned())
        .await
        .unwrap();
    let before = git(directory.path(), &["diff", "--binary"]);
    let ignored = service
        .act(input(&opened, "sample.txt", 1, "ignore"), None)
        .await
        .unwrap();
    assert!(matches!(ignored, crate::StatusAction::Projection(Some(_))));
    assert_eq!(git(directory.path(), &["diff", "--binary"]), before);
    assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
    let current = service.snapshot(&opened.document).await.unwrap();
    let ignored_input = input(&current, "sample.txt", 2, "unstage");
    assert_eq!(
        file(&current, "sample.txt", None).section,
        StatusSection::Ignored
    );
    assert!(matches!(
        service.act(ignored_input, None).await.unwrap(),
        crate::StatusAction::Projection(Some(_))
    ));
    assert_eq!(git(directory.path(), &["diff", "--binary"]), before);
    let current = service.snapshot(&opened.document).await.unwrap();
    service
        .act(input(&current, "sample.txt", 3, "ignore"), None)
        .await
        .unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let crate::StatusAction::Accepted(_, ticket) = service
        .act(input(&current, "sample.txt", 4, "stage"), None)
        .await
        .unwrap()
    else {
        panic!("ignored stage needs Git ownership")
    };
    service.close(&opened.document);
    let outcome = ticket.finish().await.unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert!(outcome.settlement_diagnostic.is_none());
    writer.acknowledge(outcome.operation);
    assert!(
        crate::ignored::load(Some(storage.path()), directory.path())
            .unwrap()
            .is_empty()
    );
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
    assert!(!directory.path().join(".gitignore").exists());
    assert!(!directory.path().join(".forge.json").exists());
}

async fn rejected_input(service: &StatusService, input: StatusInput) -> String {
    match service.input(input).await {
        Err(error) => error.to_string(),
        Ok(ticket) => {
            let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), ticket.finish())
                .await
                .unwrap()
                .unwrap();
            assert!(
                outcome
                    .target
                    .iter()
                    .all(|target| target.completion != TargetCompletion::Completed)
            );
            service.writer.acknowledge(outcome.operation);
            outcome
                .target
                .iter()
                .filter_map(|target| target.diagnostic.clone())
                .collect::<Vec<_>>()
                .join("\n")
        }
    }
}
#[tokio::test]
async fn optimistic_hunks_accept_stage_stage_unstage_before_git_and_preserve_context() {
    let (directory, service, _, writer) = fixture().await;
    let mut updates = service.subscribe_updates();
    let mut original = vec![
        "impl Engine {".to_owned(),
        "    fn run() {".to_owned(),
        "        if ready {".to_owned(),
    ];
    original.extend((0..40).map(|line| format!("            let line_{line} = {line};")));
    original.extend(["        }".to_owned(), "    }".to_owned(), "}".to_owned()]);
    std::fs::write(
        directory.path().join("sample.rs"),
        original.join("\n") + "\n",
    )
    .unwrap();
    git(directory.path(), &["add", "--all"]);
    git(directory.path(), &["commit", "-m", "context fixture"]);
    let mut changed = original.clone();
    changed[7] = "            let line_4 = 904;".into();
    changed[27] = "            let line_24 = 924;".into();
    std::fs::write(
        directory.path().join("sample.rs"),
        changed.join("\n") + "\n",
    )
    .unwrap();
    let opened = service
        .open(
            DocumentId("optimistic-context".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.rs", 1, "demand"))
        .await
        .unwrap();
    let update = tokio::time::timeout(std::time::Duration::from_secs(5), updates.recv())
        .await
        .unwrap()
        .unwrap();
    assert_eq!(update.phase, "context");
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let selected = file(&snapshot, "sample.rs", Some(StatusSection::Unstaged));
    let loaded = body(&service, &snapshot, selected).await;
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| row.trim() == "let line_4 = 904;")
                .map(|row| (block, row))
        })
        .unwrap();
    let mut captured = input(&snapshot, "sample.rs", 2, "stage");
    captured.location = body_location(selected, &loaded, block, row);
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
            1,
        )
        .unwrap();
    let guard = service.store.writes.start(gate).unwrap().unwrap();
    let first = service.input(captured).await.unwrap();
    assert_eq!(
        git(directory.path(), &["show", ":sample.rs"]),
        (original.join("\n") + "\n").as_bytes()
    );
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(snapshot.pending.len(), 1);
    let selected = file(&snapshot, "sample.rs", Some(StatusSection::Unstaged));
    let loaded = body(&service, &snapshot, selected).await;
    assert!(!loaded.block.iter().any(|block| {
        block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.trim() == "let line_4 = 904;")
    }));
    assert!(loaded.block.iter().any(|block| {
        block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("Engine.run"))
    }));
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| row.trim() == "let line_24 = 924;")
                .map(|row| (block, row))
        })
        .unwrap();
    let mut captured = input(&snapshot, "sample.rs", 3, "stage");
    captured.location = body_location(selected, &loaded, block, row);
    let second = service.input(captured).await.unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    assert_eq!(snapshot.pending.len(), 2);
    assert!(
        !snapshot
            .file
            .iter()
            .any(|file| file.path == "sample.rs" && file.section == StatusSection::Unstaged)
    );
    let selected = file(&snapshot, "sample.rs", Some(StatusSection::Staged));
    let loaded = body(&service, &snapshot, selected).await;
    assert_eq!(
        loaded
            .block
            .iter()
            .filter(|block| block.id.0.starts_with("hunk-header:"))
            .count(),
        2
    );
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| row.trim() == "let line_4 = 904;")
                .map(|row| (block, row))
        })
        .unwrap();
    let mut captured = input(&snapshot, "sample.rs", 4, "unstage");
    captured.location = body_location(selected, &loaded, block, row);
    let third = service.input(captured).await.unwrap();
    assert_eq!(
        service
            .snapshot(&opened.document)
            .await
            .unwrap()
            .pending
            .len(),
        3
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.rs"]),
        (original.join("\n") + "\n").as_bytes()
    );
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    for ticket in [first, second, third] {
        let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), ticket.finish())
            .await
            .unwrap()
            .unwrap();
        assert!(
            outcome
                .target
                .iter()
                .all(|target| target.completion == TargetCompletion::Completed),
            "{outcome:?}"
        );
        writer.acknowledge(outcome.operation);
    }
    let index = String::from_utf8(git(directory.path(), &["show", ":sample.rs"])).unwrap();
    assert!(index.contains("let line_4 = 4;"));
    assert!(index.contains("let line_24 = 924;"));
    assert_eq!(
        std::fs::read_to_string(directory.path().join("sample.rs")).unwrap(),
        changed.join("\n") + "\n"
    );
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    assert!(snapshot.pending.is_empty());
    for section in [StatusSection::Staged, StatusSection::Unstaged] {
        let loaded = body(
            &service,
            &snapshot,
            file(&snapshot, "sample.rs", Some(section)),
        )
        .await;
        assert!(loaded.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("Engine.run"))
        }));
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn optimistic_failed_insertion_rebases_later_hunk_without_staging_failed_bytes() {
    let (directory, service, _, _) = fixture().await;
    let original: Vec<_> = (0..60).map(|index| format!("line {index}")).collect();
    std::fs::write(
        directory.path().join("sample.txt"),
        original.join("\n") + "\n",
    )
    .unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "rebase baseline"]);
    let mut changed = original.clone();
    changed[40] = "later selected change".into();
    changed.splice(4..4, ["inserted A".into(), "inserted B".into()]);
    std::fs::write(
        directory.path().join("sample.txt"),
        changed.join("\n") + "\n",
    )
    .unwrap();
    let opened = service
        .open(
            DocumentId("failed-insertion".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.txt", 1, "demand"))
        .await
        .unwrap();
    let repository = service
        .store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let scope =
        forge_git::mutation::MutationScope::Index(repository.identity.worktree.clone().unwrap());
    let first_gate = service.store.writes.admit(vec![scope.clone()], 1).unwrap();
    let first_guard = service.store.writes.start(first_gate).unwrap().unwrap();
    let selected = file(&opened, "sample.txt", Some(StatusSection::Unstaged));
    let loaded = body(&service, &opened, selected).await;
    let (block, row) = loaded
        .block
        .iter()
        .find_map(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .position(|row| *row == "inserted A")
                .map(|row| (block, row))
        })
        .unwrap();
    let mut capture = input(&opened, "sample.txt", 2, "stage");
    capture.location = body_location(selected, &loaded, block, row);
    let first = service.input(capture).await.unwrap();
    let second_gate = service.store.writes.admit(vec![scope], 1).unwrap();
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
                .position(|row| *row == "later selected change")
                .map(|row| (block, row))
        })
        .unwrap();
    let mut capture = input(&current, "sample.txt", 3, "stage");
    capture.location = body_location(selected, &loaded, block, row);
    let second = service.input(capture).await.unwrap();
    let index_lock = repository.identity.index.with_extension("lock");
    std::fs::write(&index_lock, b"fixture lock").unwrap();
    first_guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(first_gate).unwrap();
    let failed = tokio::time::timeout(std::time::Duration::from_secs(20), first.finish())
        .await
        .unwrap()
        .unwrap();
    assert!(
        failed
            .target
            .iter()
            .any(|target| target.completion == TargetCompletion::OutcomeUnknown),
        "{failed:?}"
    );
    assert!(failed.settled.is_some());
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        (original.join("\n") + "\n").as_bytes()
    );
    std::fs::remove_file(index_lock).unwrap();
    let second_guard = service.store.writes.start(second_gate).unwrap().unwrap();
    second_guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(second_gate).unwrap();
    let completed = tokio::time::timeout(std::time::Duration::from_secs(20), second.finish())
        .await
        .unwrap()
        .unwrap();
    assert!(
        completed
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{completed:?}"
    );
    let mut expected = original;
    expected[40] = "later selected change".into();
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        (expected.join("\n") + "\n").as_bytes()
    );
    assert_eq!(
        std::fs::read_to_string(directory.path().join("sample.txt")).unwrap(),
        changed.join("\n") + "\n"
    );
    assert!(
        service
            .snapshot(&opened.document)
            .await
            .unwrap()
            .pending
            .is_empty()
    );
    assert_eq!(service.store.writes.usage().operations, 0);
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
    let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), ticket.finish())
        .await
        .unwrap()
        .unwrap();
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
    let opened = service.open(DocumentId("changed-hunk".into()), directory.path().to_owned()).await.unwrap();
    service.demand(input(&opened, "sample.txt", 1, "demand")).await.unwrap();
    let current = service.snapshot(&opened.document).await.unwrap();
    let selected = file(&current, "sample.txt", Some(StatusSection::Unstaged));
    let loaded = body(&service, &current, selected).await;
    let (block, row) = loaded.block.iter().find_map(|block| {
        block.text.wire_rows().iter().position(|row| *row == "new").map(|row| (block, row))
    }).unwrap();
    let repository = service.store.open(directory.path().to_owned()).await.unwrap().unwrap();
    let gate = service.store.writes.admit(vec![forge_git::mutation::MutationScope::Index(repository.identity.worktree.clone().unwrap())], 0).unwrap();
    let guard = service.store.writes.start(gate).unwrap().unwrap();
    let mut capture = input(&current, "sample.txt", 2, "stage");
    capture.location = body_location(selected, &loaded, block, row);
    let mut updates = service.subscribe_updates();
    let ticket = service.input(capture).await.unwrap();
    let optimistic = service.snapshot(&opened.document).await.unwrap();
    assert!(!optimistic.pending.is_empty());
    assert!(optimistic.section.iter().any(|section| section.kind == StatusSection::Staged));
    std::fs::write(directory.path().join("sample.txt"), b"external replacement\n").unwrap();
    guard.finish(forge_git::mutation::OperationCompletion::Completed).unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), ticket.finish()).await.unwrap().unwrap();
    assert!(outcome.target.iter().all(|target| target.completion == TargetCompletion::Rejected), "{outcome:?}");
    assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"old\n");
    assert_eq!(std::fs::read(directory.path().join("sample.txt")).unwrap(), b"external replacement\n");
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    assert!(refreshed.pending.is_empty());
    assert!(!refreshed.section.iter().any(|section| section.kind == StatusSection::Staged));
    let settled = std::iter::from_fn(|| updates.try_recv().ok()).find(|update| update.phase == "settled").unwrap();
    assert!(!settled.delta.pending.contains(&outcome.operation.value()));
    service.demand(input(&refreshed, "sample.txt", 3, "demand")).await.unwrap();
    let refreshed = service.snapshot(&opened.document).await.unwrap();
    let loaded = body(&service, &refreshed, file(&refreshed, "sample.txt", Some(StatusSection::Unstaged))).await;
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

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn queued_hunks_reject_edit_outside_selection_and_allow_independent_whole_file() {
    let (directory, service, _, _) = fixture().await;
    let original: Vec<_> = (0..60).map(|ordinal| format!("line {ordinal}")).collect();
    let path = directory.path().join("sample.txt");
    std::fs::write(&path, original.join("\n") + "\n").unwrap();
    git(directory.path(), &["add", "sample.txt"]);
    git(directory.path(), &["commit", "-m", "two hunks"]);
    let mut changed = original.clone();
    changed[4] = "first change".into();
    changed[40] = "second change".into();
    std::fs::write(&path, changed.join("\n") + "\n").unwrap();
    std::fs::write(directory.path().join("other.txt"), b"before queue\n").unwrap();
    let opened = service
        .open(
            DocumentId("queued-changed-hunks".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    service
        .demand(input(&opened, "sample.txt", 1, "demand"))
        .await
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
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let first = service
        .input(selected_change(&service, &snapshot, 2, "first change").await)
        .await
        .unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let second = service
        .input(selected_change(&service, &snapshot, 3, "second change").await)
        .await
        .unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let whole_file = service
        .input(input(&snapshot, "other.txt", 4, "stage"))
        .await
        .unwrap();
    let before = std::fs::metadata(&path).unwrap().modified().unwrap();
    changed[20] = "edit 20".into();
    std::fs::write(&path, changed.join("\n") + "\n").unwrap();
    let source = std::fs::OpenOptions::new().write(true).open(&path).unwrap();
    source
        .set_times(
            std::fs::FileTimes::new().set_modified(before + std::time::Duration::from_secs(2)),
        )
        .unwrap();
    std::fs::write(directory.path().join("other.txt"), b"execution contents\n").unwrap();
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    for ticket in [first, second] {
        let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), ticket.finish())
            .await
            .unwrap()
            .unwrap();
        assert!(!outcome.target.is_empty());
        assert!(
            outcome
                .target
                .iter()
                .all(|target| target.completion != TargetCompletion::Completed),
            "{outcome:?}"
        );
    }
    let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), whole_file.finish())
        .await
        .unwrap()
        .unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{outcome:?}"
    );
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        (original.join("\n") + "\n").as_bytes()
    );
    assert_eq!(
        git(directory.path(), &["show", ":other.txt"]),
        b"execution contents\n"
    );
    assert_eq!(
        std::fs::read(&path).unwrap(),
        (changed.join("\n") + "\n").as_bytes()
    );
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    assert!(snapshot.pending.is_empty());
    service
        .demand(input(&snapshot, "sample.txt", 5, "demand"))
        .await
        .unwrap();
    let snapshot = service.snapshot(&opened.document).await.unwrap();
    let retry = service
        .input(selected_change(&service, &snapshot, 6, "first change").await)
        .await
        .unwrap();
    let outcome = tokio::time::timeout(std::time::Duration::from_secs(20), retry.finish())
        .await
        .unwrap()
        .unwrap();
    assert!(
        outcome
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed),
        "{outcome:?}"
    );
    let mut expected = original;
    expected[4] = "first change".into();
    assert_eq!(
        git(directory.path(), &["show", ":sample.txt"]),
        (expected.join("\n") + "\n").as_bytes()
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn mixed_hunk_and_whole_file_selection_validates_each_target_policy() {
    for stale_hunk in [false, true] {
        let (directory, service, _, _) = fixture().await;
        std::fs::write(directory.path().join("other.txt"), b"initial\n").unwrap();
        let opened = service
            .open(
                DocumentId(format!("mixed-source-{stale_hunk}")),
                directory.path().to_owned(),
            )
            .await
            .unwrap();
        service
            .demand(input(&opened, "sample.txt", 1, "demand"))
            .await
            .unwrap();
        let snapshot = service.snapshot(&opened.document).await.unwrap();
        let capture = selected_change(&service, &snapshot, 2, "new").await;
        let selection = StatusSelection {
            target: vec![
                capture.location.clone(),
                input(&snapshot, "other.txt", 2, "stage").location,
            ],
        };
        std::fs::write(directory.path().join("other.txt"), b"current whole file\n").unwrap();
        if stale_hunk {
            std::fs::write(
                directory.path().join("sample.txt"),
                b"new\nexternal addition\n",
            )
            .unwrap();
        }
        let outcome = service
            .input_selection(capture, selection)
            .await
            .unwrap()
            .finish()
            .await
            .unwrap();
        if stale_hunk {
            assert!(
                outcome
                    .target
                    .iter()
                    .all(|target| target.completion == TargetCompletion::Rejected),
                "{outcome:?}"
            );
            assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
        } else {
            assert!(
                outcome
                    .target
                    .iter()
                    .all(|target| target.completion == TargetCompletion::Completed),
                "{outcome:?}"
            );
            assert_eq!(git(directory.path(), &["show", ":sample.txt"]), b"new\n");
            assert_eq!(
                git(directory.path(), &["show", ":other.txt"]),
                b"current whole file\n"
            );
        }
        assert!(
            service
                .snapshot(&opened.document)
                .await
                .unwrap()
                .pending
                .is_empty()
        );
    }
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

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn whole_section_projects_untracked_files_before_git_execution() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("new.txt"), b"new file\n").unwrap();
    std::fs::write(directory.path().join("empty.txt"), b"").unwrap();
    std::fs::write(directory.path().join("binary.bin"), b"binary\0data").unwrap();
    let opened = service
        .open(
            DocumentId("pending-untracked".into()),
            directory.path().to_owned(),
        )
        .await
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
    let mut capture = input(&opened, "sample.txt", 1, "stage");
    capture.location = StatusLocation::Section {
        section: StatusSection::Unstaged,
    };
    let ticket = service.input(capture).await.unwrap();
    let pending = service.snapshot(&opened.document).await.unwrap();
    assert!(!pending.pending.is_empty());
    assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
    assert_eq!(pending.file.len(), 4);
    assert!(
        pending
            .file
            .iter()
            .all(|file| file.section == StatusSection::Staged),
        "pending inventory: {:?}",
        pending.file
    );
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    let completed = ticket.finish().await.unwrap();
    assert!(
        completed
            .target
            .iter()
            .all(|target| target.completion == TargetCompletion::Completed)
    );
    assert_eq!(
        git(directory.path(), &["diff", "--cached", "--name-only"]),
        b"binary.bin\nempty.txt\nnew.txt\nsample.txt\n"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn collapsed_file_counts_survive_pending_stage_and_unstage() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("deleted.txt"), b"removed\n").unwrap();
    git(directory.path(), &["add", "deleted.txt"]);
    git(directory.path(), &["commit", "-m", "deletion fixture"]);
    std::fs::remove_file(directory.path().join("deleted.txt")).unwrap();
    std::fs::write(directory.path().join("new.txt"), b"one\ntwo\n").unwrap();
    let opened = service
        .open(
            DocumentId("pending-counts".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let expected_change: std::collections::HashMap<_, _> = opened
        .file
        .iter()
        .map(|file| (file.path.clone(), file.change))
        .collect();
    let expected: std::collections::HashMap<_, _> = opened
        .file
        .iter()
        .map(|file| (file.path.clone(), file.stats))
        .collect();
    assert_eq!(
        expected["sample.txt"],
        crate::protocol::StatusStatistics::Exact {
            added: 1,
            deleted: 1
        }
    );
    assert_eq!(
        expected["new.txt"],
        crate::protocol::StatusStatistics::Exact {
            added: 2,
            deleted: 0
        }
    );
    let repository = service
        .store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    for (sequence, action, section) in [
        (1, "stage", StatusSection::Unstaged),
        (2, "unstage", StatusSection::Staged),
    ] {
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
        let snapshot = service.snapshot(&opened.document).await.unwrap();
        let mut capture = input(&snapshot, "sample.txt", sequence, action);
        capture.location = StatusLocation::Section { section };
        let ticket = service.input(capture).await.unwrap();
        let pending = service.snapshot(&opened.document).await.unwrap();
        assert!(!pending.pending.is_empty());
        for file in &pending.file {
            assert_eq!(
                file.change, expected_change[&file.path],
                "{} during {action}",
                file.path
            );
            assert_eq!(
                file.stats, expected[&file.path],
                "{} during {action}",
                file.path
            );
        }
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
        let settled = service.snapshot(&opened.document).await.unwrap();
        assert!(settled.pending.is_empty());
        for file in &settled.file {
            assert_eq!(
                file.change, expected_change[&file.path],
                "{} after {action}",
                file.path
            );
            assert_eq!(
                file.stats, expected[&file.path],
                "{} after {action}",
                file.path
            );
        }
    }
    assert!(git(directory.path(), &["diff", "--cached", "--name-only"]).is_empty());
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn collapsed_file_counts_survive_queued_stage_unstage_stage() {
    let (directory, service, _, _) = fixture().await;
    std::fs::write(directory.path().join("deleted.txt"), b"removed\n").unwrap();
    git(directory.path(), &["add", "deleted.txt"]);
    git(directory.path(), &["commit", "-m", "deletion fixture"]);
    std::fs::remove_file(directory.path().join("deleted.txt")).unwrap();
    std::fs::write(directory.path().join("new.txt"), b"one\ntwo\n").unwrap();
    let opened = service
        .open(
            DocumentId("queued-counts".into()),
            directory.path().to_owned(),
        )
        .await
        .unwrap();
    let expected_change: std::collections::HashMap<_, _> = opened
        .file
        .iter()
        .map(|file| (file.path.clone(), file.change))
        .collect();
    let expected: std::collections::HashMap<_, _> = opened
        .file
        .iter()
        .map(|file| (file.path.clone(), file.stats))
        .collect();
    assert_eq!(
        expected["sample.txt"],
        crate::protocol::StatusStatistics::Exact {
            added: 1,
            deleted: 1
        }
    );
    assert_eq!(
        expected["new.txt"],
        crate::protocol::StatusStatistics::Exact {
            added: 2,
            deleted: 0
        }
    );
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
    let mut tickets = Vec::new();
    for (sequence, action, section) in [
        (1, "stage", StatusSection::Unstaged),
        (2, "unstage", StatusSection::Staged),
        (3, "stage", StatusSection::Unstaged),
    ] {
        let snapshot = service.snapshot(&opened.document).await.unwrap();
        let mut capture = input(&snapshot, "sample.txt", sequence, action);
        capture.location = StatusLocation::Section { section };
        let ticket = service.input(capture).await.unwrap();
        let pending = service.snapshot(&opened.document).await.unwrap();
        assert!(!pending.pending.is_empty());
        for file in &pending.file {
            assert_eq!(
                file.change, expected_change[&file.path],
                "{} during {action}",
                file.path
            );
            assert_eq!(
                file.stats, expected[&file.path],
                "{} during {action}",
                file.path
            );
        }
        tickets.push(ticket);
    }
    guard
        .finish(forge_git::mutation::OperationCompletion::Completed)
        .unwrap();
    service.store.writes.take_receipt(gate).unwrap();
    for ticket in tickets {
        let outcome = ticket.finish().await.unwrap();
        assert!(
            outcome
                .target
                .iter()
                .all(|target| target.completion == TargetCompletion::Completed)
        );
    }
    let action = "queued actions";
    {
        let settled = service.snapshot(&opened.document).await.unwrap();
        assert!(settled.pending.is_empty());
        for file in &settled.file {
            assert_eq!(
                file.change, expected_change[&file.path],
                "{} after {action}",
                file.path
            );
            assert_eq!(
                file.stats, expected[&file.path],
                "{} after {action}",
                file.path
            );
        }
    }
    assert_eq!(
        git(directory.path(), &["diff", "--cached", "--name-only"]),
        b"deleted.txt\nnew.txt\nsample.txt\n"
    );
}
