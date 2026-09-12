mod support;

use forge_diff::source::Representation;
use forge_git::{
    RepositoryPath,
    content::{
        ContentLimits, ContentRequest, ContentResult, ContentSource, ContentUnavailable,
        WorktreeConversion,
    },
    store::RepositoryStore,
};
use std::{fs, io::Read};
use support::git;

#[tokio::test]
async fn native_builtin_conversion_matches_git_object_identity() {
    let case = [
        ("", b"plain\n".as_slice()),
        ("file text", b"one\r\ntwo\r\n"),
        ("file -text", b"one\r\ntwo\r\n"),
        ("file text=auto", b"one\r\ntwo\n"),
        ("file text eol=lf", b"one\r\ntwo"),
        ("file ident", b"$Id: abcdef $\n"),
        (
            "file working-tree-encoding=UTF-16LE text",
            b"o\0n\0e\0\r\0\n\0",
        ),
        (
            "file working-tree-encoding=UTF-16BE text",
            b"\0o\0n\0e\0\r\0\n",
        ),
        ("file working-tree-encoding=UTF-8 text", b"one\r\n"),
    ];
    for (attributes, input) in case {
        let fixture = tempfile::tempdir().unwrap();
        let root = fixture.path();
        git(root, &["init", "--quiet"]);
        git(root, &["config", "core.autocrlf", "false"]);
        git(root, &["config", "core.safecrlf", "false"]);
        fs::write(root.join(".gitattributes"), attributes).unwrap();
        fs::write(root.join("file"), input).unwrap();
        let expected = gix::ObjectId::from_hex(
            git(root, &["hash-object", "--path=file", "file"]).trim_ascii(),
        )
        .unwrap();
        let local = gix::open(root).unwrap();
        let (mut pipeline, index) = local.filter_pipeline(None).unwrap();
        let mut converted = Vec::new();
        pipeline
            .convert_to_git(input, std::path::Path::new("file"), &index)
            .unwrap()
            .read_to_end(&mut converted)
            .unwrap();
        assert_eq!(
            gix::objs::compute_hash(local.object_hash(), gix::objs::Kind::Blob, &converted)
                .unwrap(),
            expected,
            "{attributes}"
        );
        let store = RepositoryStore::default();
        let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
        let result = repository
            .content(&store, canonical_request())
            .await
            .unwrap()
            .value;
        let ContentResult::Ready(content) = result else {
            panic!("{attributes}: {result:?}")
        };
        assert_eq!(content.source.bytes(), converted);
        assert_eq!(
            content.source.identity().representation,
            Representation::GitCanonical
        );
        assert!(matches!(
            content.origin,
            forge_git::content::ContentOrigin::Worktree {
                conversion: Some(_),
                ..
            }
        ));
    }
}

#[tokio::test]
async fn canonical_read_reloads_configuration_after_store_discovery() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "core.autocrlf", "false"]);
    git(root, &["config", "core.safecrlf", "false"]);
    fs::write(root.join("file"), b"one\r\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    git(root, &["config", "core.autocrlf", "true"]);
    let result = repository
        .content(&store, canonical_request())
        .await
        .unwrap()
        .value;
    let ContentResult::Ready(content) = result else {
        panic!("{result:?}")
    };
    assert_eq!(content.source.bytes(), b"one\n");
}

#[tokio::test]
async fn external_filters_and_unproven_encodings_remain_explicitly_unavailable() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "filter.blocked.clean", "exit 9"]);
    git(root, &["config", "filter.blocked.required", "true"]);
    fs::write(root.join(".gitattributes"), "file filter=blocked\n").unwrap();
    fs::write(root.join("file"), b"one\r\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    assert!(matches!(
        repository
            .content(&store, canonical_request())
            .await
            .unwrap()
            .value,
        ContentResult::Unavailable(ContentUnavailable::ExternalFilter)
    ));
    fs::write(
        root.join(".gitattributes"),
        "file working-tree-encoding=ISO-8859-1\n",
    )
    .unwrap();
    assert!(matches!(
        repository
            .content(&store, canonical_request())
            .await
            .unwrap()
            .value,
        ContentResult::Unavailable(ContentUnavailable::UnsupportedEncoding)
    ));
}

#[tokio::test]
async fn encoding_capacity_and_byte_order_marks_are_checked_before_conversion() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    fs::write(
        root.join(".gitattributes"),
        "file working-tree-encoding=UTF-16LE text\n",
    )
    .unwrap();
    fs::write(root.join("file"), b"o\0n\0e\0\r\0\n\0").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let mut request = canonical_request();
    request.limits = ContentLimits::new(10, 10).unwrap();
    assert!(matches!(
        repository.content(&store, request).await.unwrap().value,
        ContentResult::TooLarge {
            kind: forge_git::content::ContentLimit::ConversionCapacity,
            ..
        }
    ));
    fs::write(root.join("file"), b"\xff\xfeo\0").unwrap();
    let result = repository
        .content(&store, canonical_request())
        .await
        .unwrap()
        .value;
    let ContentResult::Failed(error) = result else {
        panic!("{result:?}")
    };
    assert!(error.to_string().contains("byte-order mark"));
}

fn canonical_request() -> ContentRequest {
    ContentRequest {
        source: ContentSource::Worktree {
            path: RepositoryPath::new(b"file".to_vec()).unwrap(),
            conversion: WorktreeConversion::GitCanonical,
        },
        limits: ContentLimits::default(),
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

#[tokio::test]
async fn invalid_conversion_configuration_is_not_silently_defaulted() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    fs::write(root.join("file"), b"one\r\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    git(root, &["config", "core.autocrlf", "invalid"]);
    assert!(matches!(
        repository
            .content(&store, canonical_request())
            .await
            .unwrap()
            .value,
        ContentResult::Failed(_)
    ));
}
