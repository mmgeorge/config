use super::*;
use forge_git::test_support::{init_repository, set_config, set_identity, stage_and_commit};

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
