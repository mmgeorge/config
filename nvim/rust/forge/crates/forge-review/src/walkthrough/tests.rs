use super::*;
use forge_git::test_support::{init_repository, set_config, set_identity, stage_and_commit};

#[tokio::test]
async fn composite_review_drains_all_chunks_with_an_anchor_after_the_first_chunk() {
    let (directory, service, document) = fixture(false).await;
    service.close_collected(&document).await;
    let source_rows = (0..600)
        .map(|row| format!("// row {row} {}", "x".repeat(512)))
        .collect::<Vec<_>>();
    let source = format!("{}\n", source_rows.join("\n"));
    assert!(source.len() > 128 * 1024);
    std::fs::write(directory.path().join("source.rs"), source).unwrap();
    let artifact_path = directory.path().join(".walkthrough.json");
    let mut artifact: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&artifact_path).unwrap()).unwrap();
    artifact["tasks"][0]["subtasks"][0]["changes"][0]["line"] = serde_json::json!(500);
    std::fs::write(artifact_path, serde_json::to_vec(&artifact).unwrap()).unwrap();
    service
        .open(document.clone(), directory.path().to_owned(), false)
        .await
        .unwrap();
    let input = source_input(&service, &document);
    let annotation = DocumentId("review:complete".into());
    let mut change = service
        .resolve_change(input.clone(), annotation.clone())
        .await
        .unwrap();
    let diff = forge_diff::engine::DiffEngine::new(Default::default(), 1);
    let syntax = forge_diff::syntax::SyntaxEngine::new(Arc::new(
        forge_diff::workers::AnalysisPool::new(forge_diff::workers::PoolLimits {
            workers: 1,
            jobs: 2,
        }),
    ));
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
    for source_row in &source_rows {
        assert!(rows.contains(&source_row.as_str()));
    }
    assert!(!rows.iter().any(|row| row.contains("excerpt is limited")));
    assert_eq!(change.row, 499);
    service.close_collected(&document).await;
}

async fn fixture(stale: bool) -> (tempfile::TempDir, WalkthroughService, DocumentId) {
    let (directory, head) = tokio::task::spawn_blocking(|| {
        let directory = tempfile::tempdir().unwrap();
        init_repository(directory.path());
        set_identity(directory.path());
        set_config(directory.path(), "core", "autocrlf", "false");
        std::fs::write(directory.path().join("source.rs"), "fn captured() {}\n").unwrap();
        let head = stage_and_commit(directory.path(), &["source.rs"], "base").to_string();
        (directory, head)
    })
    .await
    .unwrap();
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
    let WalkthroughProjection {
        block: blocks,
        target: targets,
    } = project(&document, &WidthProfile::default()).unwrap();
    document.buffer = BufferDocument::new(id.clone(), blocks).unwrap();
    document.target = targets;
    service
        .document
        .lock()
        .unwrap()
        .insert(id.clone(), Arc::new(Mutex::new(document)));
    (directory, service, id)
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
