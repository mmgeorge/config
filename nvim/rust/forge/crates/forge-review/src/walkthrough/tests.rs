use super::*;
use forge_git::command::{CommandLimits, read_command};

#[tokio::test]
async fn composite_review_preserves_diff_source_coordinates_and_annotation_reflow() {
    let (directory, service, document) = fixture(false).await;
    service.close_collected(&document).await;
    std::fs::write(directory.path().join("source.rs"), "fn current() {}\n").unwrap();
    service
        .open(document.clone(), directory.path().to_owned(), false)
        .await
        .unwrap();
    let input = source_input(&service, &document);
    let annotation = DocumentId("review:composite".into());
    let mut change = service
        .resolve_change(input.clone(), annotation.clone())
        .await
        .unwrap();
    let diff = forge_diff::engine::DiffEngine::new(Default::default(), 1);
    let syntax = forge_diff::syntax::SyntaxEngine::new(
        Arc::new(forge_diff::workers::AnalysisPool::new(
            forge_diff::workers::PoolLimits {
                workers: 1,
                jobs: 2,
                input_bytes: 8 * 1024 * 1024,
            },
        )),
        Default::default(),
    );
    service
        .project_review(&input, &annotation, &mut change, &diff, &syntax)
        .await
        .unwrap();
    let rows = change
        .annotation
        .block
        .iter()
        .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
        .collect::<Vec<_>>();
    assert!(rows.contains(&"fn captured() {}"));
    assert!(rows.contains(&"fn current() {}"));
    assert!(rows.iter().any(|row| row.starts_with('╭')));
    assert!(rows.iter().any(|row| row.contains("Captured contract")));
    assert!(
        change
            .annotation
            .block
            .iter()
            .any(|block| !block.metadata.visible_decoration.is_empty())
    );
    let view = ViewId("composite:view".into());
    service
        .annotation_view(
            &document,
            &annotation,
            view.clone(),
            Some(WidthProfile {
                columns: 24,
                ..Default::default()
            }),
        )
        .unwrap();
    let snapshot = service.annotation_snapshot(&document, &annotation).unwrap();
    let block = snapshot
        .block
        .iter()
        .find(|block| block.text.row(0) == Some("fn current() {}"))
        .unwrap();
    let action = DocumentInput {
        document: annotation.clone(),
        revision: snapshot.revision,
        view: view.clone(),
        sequence: InputSequence(1),
        block: block.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(block.metadata.target[0].id.clone()),
        action: "open".into(),
    };
    assert_eq!(
        service
            .annotation_source(&document, action.clone())
            .unwrap(),
        0
    );
    assert!(
        service
            .annotation_source(&document, action.clone())
            .is_err()
    );
    service
        .annotation_view(
            &document,
            &annotation,
            view,
            Some(WidthProfile {
                columns: 50,
                ..Default::default()
            }),
        )
        .unwrap();
    let mut stale = action;
    stale.sequence = InputSequence(2);
    assert!(service.annotation_source(&document, stale).is_err());
    assert_eq!(change.source.text(), "fn current() {}\n");
    service.close_collected(&document).await;
}
fn git(root: &std::path::Path, arguments: &[&str]) -> String {
    let output = read_command(
        std::process::Command::new("git")
            .arg("-C")
            .arg(root)
            .args(arguments),
        CommandLimits {
            stdout_bytes: 8192,
            stderr_bytes: 8192,
            timeout: std::time::Duration::from_secs(10),
        },
        || Ok(()),
    )
    .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap().trim().to_owned()
}
async fn fixture(stale: bool) -> (tempfile::TempDir, WalkthroughService, DocumentId) {
    let directory = tempfile::tempdir().unwrap();
    git(directory.path(), &["init", "--initial-branch=main"]);
    git(directory.path(), &["config", "user.name", "Forge Test"]);
    git(
        directory.path(),
        &["config", "user.email", "forge@example.invalid"],
    );
    git(directory.path(), &["config", "core.autocrlf", "false"]);
    std::fs::write(directory.path().join("source.rs"), "fn captured() {}\n").unwrap();
    git(directory.path(), &["add", "source.rs"]);
    git(directory.path(), &["commit", "-m", "base"]);
    let head = git(directory.path(), &["rev-parse", "HEAD"]);
    let store = Arc::new(RepositoryStore::default());
    let repository = store
        .open(directory.path().to_owned())
        .await
        .unwrap()
        .unwrap();
    let observation = repository.snapshot(&store).await.unwrap();
    let service = WalkthroughService::new(store);
    let id = DocumentId("walkthrough:test".into());
    let artifact_bytes = serde_json::to_vec(&serde_json::json!({"version":12,"root":"Project","overview":"Overview","commit":head,"flow":[{"text":"Flow"}],"tasks":[{"title":"Complete task heading wraps across several physical rows","subtasks":[{"title":"Complete subtask heading wraps across several physical rows","changes":[{"action":"Modify","kind":"Function","target":"captured","note":"Description","file":"source.rs","line":1,"annotation":{"title":"Read-only annotation","comment":"**Captured contract**"}}]}]}]})).unwrap();
    std::fs::write(directory.path().join(".walkthrough.json"), &artifact_bytes).unwrap();
    let artifact = Artifact::parse(&artifact_bytes).unwrap();
    let mut document = WalkthroughDocument {
        admission: Arc::new(service.admission.admit(id.clone()).unwrap()),
        buffer: BufferDocument::new(id.clone(), Vec::new()).unwrap(),
        repository,
        observation,
        artifact,
        stale,
        inventory: None,
        inventory_diagnostic: Some("Sem unavailable in isolated projection fixture".into()),
        views: DocumentViews::default(),
        input: HashMap::new(),
        target: HashMap::new(),
        annotation: HashMap::new(),
    };
    let (blocks, targets) = project(&document, &WidthProfile::default()).unwrap();
    document.buffer = BufferDocument::new(id.clone(), blocks).unwrap();
    document.target = targets;
    service
        .document
        .lock()
        .unwrap()
        .insert(id.clone(), Arc::new(Mutex::new(document)));
    (directory, service, id)
}
#[tokio::test]
async fn disabled_inventory_opens_without_a_semantic_inventory_projection() {
    let (directory, service, id) = fixture(false).await;
    assert!(service.close_collected(&id).await);
    let opened = service
        .open(id.clone(), directory.path().to_owned(), false)
        .await
        .unwrap();
    assert_eq!(opened.inventory_state, "disabled");
    assert!(opened.inventory_diagnostic.is_none());
    assert!(
        !serde_json::to_string(&opened.snapshot)
            .unwrap()
            .contains("Semantic inventory")
    );
    assert!(service.close_collected(&id).await);
}

#[tokio::test]
async fn numbered_outline_and_bordered_annotation_keep_the_source_anchor() {
    let (_directory, service, id) = fixture(false).await;
    let owner = service.get(&id).unwrap();
    {
        let mut document = owner.lock().unwrap();
        document.artifact.flow[0].children.push(schema::Flow {
            text: "Next boundary".into(),
            children: vec![],
        });
        document.inventory = Some(std::collections::BTreeMap::from([(
            ("added".into(), "function".into()),
            2,
        )]));
        document.inventory_diagnostic = None;
        let (blocks, _) = project(&document, &WidthProfile::default()).unwrap();
        let rows = blocks
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(rows.contains("Flow → Next boundary"));
        assert!(rows.contains("1. Complete task"));
        assert!(rows.contains("1.1. Complete subtask"));
        assert!(rows.contains("1.1.1 · Modify Function"));
        assert!(rows.contains("source.rs:1 · open source annotation"));
        assert!(rows.contains("function +2"));
        assert!(
            blocks
                .iter()
                .flat_map(|block| &block.metadata.decoration)
                .any(|decoration| decoration.capture == "ForgeWalkthroughActionAdd")
        );
    }
    let input = source_input(&service, &id);
    let change = service
        .resolve_change(input, DocumentId("numbered-annotation".into()))
        .await
        .unwrap();
    let rows = change
        .annotation
        .block
        .iter()
        .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
        .collect::<Vec<_>>();
    assert!(rows.first().unwrap().starts_with('╭'));
    assert!(rows.last().unwrap().starts_with('╰'));
    assert!(rows.iter().any(|row| row.contains("1.1.1 · source.rs:1")));
    assert!(rows.iter().any(|row| row.contains("Captured contract")));
    assert_eq!(change.row, 0);
    for block in &change.annotation.block {
        block.validate().unwrap();
    }
}

#[tokio::test]
async fn folded_headings_keep_every_wrapped_row_and_width_has_one_owner() {
    let (_directory, service, id) = fixture(false).await;
    let first = ViewId("first".into());
    service
        .view(
            &id,
            first.clone(),
            WidthProfile {
                columns: 20,
                ..Default::default()
            },
        )
        .unwrap();
    let before = service.snapshot(&id).unwrap();
    for block in &before.block {
        for fold in &block.metadata.fold {
            assert_eq!(fold.closed, fold.id.0.contains(":subtask:"));
            assert!(block.text.row_count() > 1);
            assert_eq!(fold.start.row, block.text.row_count() - 1);
        }
        assert!(block.metadata.editable_region.is_empty());
    }
    assert!(
        service
            .view(
                &id,
                ViewId("second".into()),
                WidthProfile {
                    columns: 100,
                    ..Default::default()
                }
            )
            .unwrap()
            .is_none()
    );
    assert!(service.close_view(&id, &first).unwrap().is_some());
    assert!(service.close_collected(&id).await);
}
#[tokio::test]
async fn current_walkthrough_navigation_resolves_the_captured_object() {
    let (directory, service, id) = fixture(false).await;
    std::fs::write(directory.path().join("source.rs"), "fn changed() {}\n").unwrap();
    let view = ViewId("source-view".into());
    service
        .view(&id, view.clone(), WidthProfile::default())
        .unwrap();
    let snapshot = service.snapshot(&id).unwrap();
    let block = snapshot
        .block
        .iter()
        .find(|block| !block.metadata.target.is_empty())
        .unwrap();
    let input = DocumentInput {
        document: id.clone(),
        revision: snapshot.revision,
        view,
        sequence: InputSequence(1),
        action: "open".into(),
        block: block.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(block.metadata.target[0].id.clone()),
    };
    let changed = service
        .resolve_change(input, DocumentId("annotation".into()))
        .await
        .unwrap();
    assert_eq!(changed.source.bytes(), b"fn captured() {}\n");
    assert!(service.close_collected(&id).await);
}

fn source_input(service: &WalkthroughService, document: &DocumentId) -> DocumentInput {
    let view = ViewId("source-choice".into());
    service
        .view(document, view.clone(), WidthProfile::default())
        .unwrap();
    let snapshot = service.snapshot(document).unwrap();
    let block = snapshot
        .block
        .iter()
        .find(|block| !block.metadata.target.is_empty())
        .unwrap();
    DocumentInput {
        document: document.clone(),
        revision: snapshot.revision,
        view,
        sequence: InputSequence(1),
        action: "open".into(),
        block: block.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(block.metadata.target[0].id.clone()),
    }
}

#[tokio::test]
async fn current_annotations_select_new_worktree_content_and_removed_baseline_content() {
    for action in ["Add", "Modify", "Remove"] {
        let (directory, service, document) = fixture(false).await;
        assert!(service.close_collected(&document).await);
        let path = if action == "Add" {
            "added.rs"
        } else {
            "source.rs"
        };
        let expected = if action == "Remove" {
            std::fs::remove_file(directory.path().join(path)).unwrap();
            "fn captured() {}\n"
        } else {
            let content = "fn current() {\n    changed();\n}\n";
            std::fs::write(directory.path().join(path), content).unwrap();
            content
        };
        let artifact_path = directory.path().join(".walkthrough.json");
        let mut artifact: serde_json::Value =
            serde_json::from_slice(&std::fs::read(&artifact_path).unwrap()).unwrap();
        let change = &mut artifact["tasks"][0]["subtasks"][0]["changes"][0];
        change["action"] = action.into();
        change["file"] = path.into();
        change["line"] = if action == "Remove" { 1 } else { 2 }.into();
        std::fs::write(&artifact_path, serde_json::to_vec(&artifact).unwrap()).unwrap();
        service
            .open(document.clone(), directory.path().to_owned(), false)
            .await
            .unwrap();
        let selected = service
            .resolve_change(
                source_input(&service, &document),
                DocumentId("annotation".into()),
            )
            .await
            .unwrap();
        assert_eq!(selected.source.bytes(), expected.as_bytes(), "{action}");
        assert_eq!(selected.row, if action == "Remove" { 0 } else { 1 });
        assert!(
            !selected.stale,
            "fresh {action} annotation was marked stale"
        );
        assert!(
            selected
                .annotation
                .block
                .iter()
                .all(|block| block.metadata.editable_region.is_empty())
        );
        assert!(service.close_collected(&document).await);
    }
}

#[tokio::test]
async fn changed_worktree_after_walkthrough_open_rejects_navigation() {
    let (directory, service, document) = fixture(false).await;
    assert!(service.close_collected(&document).await);
    std::fs::write(directory.path().join("source.rs"), "fn current() {}\n").unwrap();
    service
        .open(document.clone(), directory.path().to_owned(), false)
        .await
        .unwrap();
    std::fs::write(
        directory.path().join("source.rs"),
        "fn changed_after_open() {}\n",
    )
    .unwrap();
    let failure = service
        .resolve_change(
            source_input(&service, &document),
            DocumentId("annotation".into()),
        )
        .await
        .err()
        .expect("changed worktree was accepted at an obsolete annotation");
    assert!(
        failure.to_string().contains("reopen the walkthrough"),
        "{failure:#}"
    );
    assert!(service.close_collected(&document).await);
}

#[tokio::test]
async fn annotation_views_reflow_independently_and_release_with_their_owner() {
    let (_directory, service, document) = fixture(false).await;
    {
        let owner = service.get(&document).unwrap();
        let mut document = owner.lock().unwrap();
        document.artifact.tasks[0].subtasks[0].changes[0].annotation.as_mut().unwrap().comment =
            "The annotation explains the current source boundary with complete words across narrow and wide panes.".into();
    }
    let mut input = source_input(&service, &document);
    for index in 0..8 {
        input.sequence = InputSequence(index + 1);
        service
            .resolve_change(input.clone(), DocumentId(format!("annotation:{index}")))
            .await
            .unwrap();
    }
    input.sequence = InputSequence(9);
    assert!(
        service
            .resolve_change(input, DocumentId("annotation:overflow".into()))
            .await
            .is_err()
    );
    let annotation = DocumentId("annotation:0".into());
    let primary = ViewId("annotation-primary".into());
    let secondary = ViewId("annotation-secondary".into());
    let width = |columns| WidthProfile {
        columns,
        ..Default::default()
    };
    assert!(
        service
            .annotation_view(&document, &annotation, primary.clone(), Some(width(20)))
            .unwrap()
            .is_some()
    );
    let narrow = service.annotation_snapshot(&document, &annotation).unwrap();
    assert!(
        narrow
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .all(|row| width(20).cells(row, 0).unwrap() <= 20)
    );
    assert!(
        service
            .annotation_view(&document, &annotation, primary.clone(), Some(width(80)))
            .unwrap()
            .is_some()
    );
    let wide = service.annotation_snapshot(&document, &annotation).unwrap();
    assert!(
        wide.block
            .iter()
            .map(|block| block.text.row_count())
            .sum::<usize>()
            < narrow
                .block
                .iter()
                .map(|block| block.text.row_count())
                .sum::<usize>()
    );
    assert!(
        service
            .annotation_view(&document, &annotation, secondary.clone(), Some(width(12)))
            .unwrap()
            .is_none()
    );
    assert!(
        service
            .annotation_view(&document, &annotation, primary, None)
            .unwrap()
            .is_some()
    );
    let transferred = service.annotation_snapshot(&document, &annotation).unwrap();
    assert!(
        transferred
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .all(|row| width(12).cells(row, 0).unwrap() <= 12)
    );
    assert_eq!(
        service
            .annotation_snapshot(&document, &DocumentId("annotation:1".into()))
            .unwrap()
            .revision,
        forge_buffer::identity::DocumentRevision(0)
    );
    assert!(service.close_annotation(&document, &annotation).unwrap());
    assert!(service.annotation_snapshot(&document, &annotation).is_err());
    assert!(service.close_collected(&document).await);
    assert!(
        service
            .annotation_snapshot(&document, &DocumentId("annotation:1".into()))
            .is_err()
    );
}

#[tokio::test]
async fn stale_annotation_resolves_captured_object_and_rejects_mutation_input() {
    let (directory, service, id) = fixture(true).await;
    std::fs::write(directory.path().join("source.rs"), "fn changed() {}\n").unwrap();
    let view = ViewId("source-view".into());
    service
        .view(&id, view.clone(), WidthProfile::default())
        .unwrap();
    let snapshot = service.snapshot(&id).unwrap();
    let block = snapshot
        .block
        .iter()
        .find(|block| !block.metadata.target.is_empty())
        .unwrap();
    let input = DocumentInput {
        document: id.clone(),
        revision: snapshot.revision,
        view,
        sequence: InputSequence(1),
        action: "open".into(),
        block: block.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(block.metadata.target[0].id.clone()),
    };
    let changed = service
        .resolve_change(input.clone(), DocumentId("annotation".into()))
        .await
        .unwrap();
    assert!(changed.stale);
    assert_eq!(changed.source.bytes(), b"fn captured() {}\n");
    assert!(
        changed
            .annotation
            .block
            .iter()
            .all(|block| block.metadata.editable_region.is_empty())
    );
    let mut mutation = input;
    mutation.action = "stage".into();
    mutation.sequence = InputSequence(2);
    assert!(
        service
            .resolve_change(mutation, DocumentId("forbidden".into()))
            .await
            .is_err()
    );
    assert!(service.close_collected(&id).await);
}
