use forge_git::test_support as support;

use forge_git::{
    RepositoryPath,
    content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion},
    store::RepositoryStore,
};
use std::fs;
use support::git;

fn canonical_request() -> ContentRequest {
    ContentRequest {
        source: ContentSource::Worktree {
            path: RepositoryPath::new(b"file".to_vec()).unwrap(),
            conversion: WorktreeConversion::GitCanonical,
        },
        expected: None,
    }
}

#[tokio::test]
async fn canonical_conversion_preserves_index_crlf_history() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "core.autocrlf", "false"]);
    git(root, &["config", "core.safecrlf", "false"]);
    fs::write(root.join("file"), b"one\r\n").unwrap();
    git(root, &["add", "file"]);
    git(root, &["config", "core.autocrlf", "true"]);
    fs::write(root.join("file"), b"one\r\ntwo\r\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let result = repository
        .content(&store, canonical_request())
        .await
        .unwrap()
        .value;
    let ContentResult::Ready(content) = result else {
        panic!("{result:?}")
    };
    git(root, &["add", "file"]);
    let staged = git(root, &["show", ":file"]);
    assert_eq!(content.source.bytes(), staged);
}

#[tokio::test]
async fn canonical_conversion_refreshes_attribute_precedence() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "core.autocrlf", "false"]);
    git(root, &["config", "core.safecrlf", "false"]);
    fs::create_dir(root.join("sub")).unwrap();
    fs::write(
        root.join(".gitattributes"),
        b"* text\n[attr]canonical text\n",
    )
    .unwrap();
    fs::write(root.join("sub/.gitattributes"), b"file -text\n").unwrap();
    fs::write(root.join("sub/file"), b"one\r\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    for (override_attributes, expected) in [
        (b"".as_slice(), b"one\r\n".as_slice()),
        (b"sub/file text\n".as_slice(), b"one\n".as_slice()),
        (b"sub/file !text\n".as_slice(), b"one\r\n".as_slice()),
        (b"sub/file canonical\n".as_slice(), b"one\n".as_slice()),
        (b"sub/file -text\n".as_slice(), b"one\r\n".as_slice()),
    ] {
        fs::write(root.join(".git/info/attributes"), override_attributes).unwrap();
        let mut request = canonical_request();
        request.source = ContentSource::Worktree {
            path: RepositoryPath::new(b"sub/file".to_vec()).unwrap(),
            conversion: WorktreeConversion::GitCanonical,
        };
        let result = repository.content(&store, request).await.unwrap().value;
        let ContentResult::Ready(content) = result else {
            panic!("{override_attributes:?}: {result:?}")
        };
        assert_eq!(content.source.bytes(), expected);
    }
}
