use forge_git::test_support as support;

use std::fs;

use forge_diff::source::{Representation, SourceVersion};
use forge_git::{
    RepositoryPath,
    content::{
        ContentOrigin, ContentRequest, ContentResult, ContentSource, ContentUnavailable,
        FileContent, IndexStage,
    },
    store::RepositoryStore,
};
use support::{
    commit_index, git, init_repository, init_sha256_repository, resolve_revision, set_config,
    set_reference, stage, stage_conflict, stage_gitlink, write_blob,
};

#[tokio::test]
async fn staged_unstaged_and_head_sources_remain_distinct() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    set_config(root, "core", "autocrlf", "false");
    fs::write(root.join("tracked"), b"head\r\n").unwrap();
    stage(root, &["tracked"]);
    commit_index(root, "fixture");
    let head = resolve_revision(root, "HEAD:tracked");
    fs::write(root.join("tracked"), b"staged\r\n").unwrap();
    stage(root, &["tracked"]);
    fs::write(root.join("tracked"), b"worktree without newline").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let head = ready(
        repository
            .content(&store, request(ContentSource::Object(head)))
            .await
            .unwrap()
            .value,
    );
    let index = ready(
        repository
            .content(
                &store,
                request(ContentSource::IndexStage {
                    path: path("tracked"),
                    stage: IndexStage::Normal,
                }),
            )
            .await
            .unwrap()
            .value,
    );
    let worktree = ready(
        repository
            .content(
                &store,
                request(ContentSource::Worktree {
                    path: path("tracked"),
                    conversion: forge_git::content::WorktreeConversion::Raw,
                }),
            )
            .await
            .unwrap()
            .value,
    );
    assert_eq!(head.source.bytes(), b"head\r\n");
    assert_eq!(index.source.bytes(), b"staged\r\n");
    assert_eq!(worktree.source.bytes(), b"worktree without newline");
    assert_eq!(
        head.source.identity().representation,
        Representation::GitCanonical
    );
    assert_eq!(
        index.source.identity().representation,
        Representation::GitCanonical
    );
    assert_eq!(
        worktree.source.identity().representation,
        Representation::Raw
    );
    assert_eq!(head.source.newline().crlf_count, 1);
    assert!(!worktree.source.newline().has_final_newline);
    assert!(matches!(
        index.origin,
        ContentOrigin::IndexStage {
            stage: IndexStage::Normal,
            ..
        }
    ));
}

#[tokio::test]
async fn missing_binary_encoding_size_and_source_identity_have_distinct_outcomes() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    fs::write(root.join("binary"), b"a\0b").unwrap();
    fs::write(root.join("encoding"), b"a\xffb").unwrap();
    fs::write(root.join("text"), b"original").unwrap();
    fs::create_dir(root.join("directory")).unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    for (name, expected) in [
        ("binary", 0),
        ("encoding", 1),
        ("missing", 2),
        ("directory", 3),
    ] {
        let result = repository
            .content(
                &store,
                request(ContentSource::Worktree {
                    path: path(name),
                    conversion: forge_git::content::WorktreeConversion::Raw,
                }),
            )
            .await
            .unwrap()
            .value;
        assert!(matches!(
            (expected, result),
            (0, ContentResult::Binary)
                | (
                    1,
                    ContentResult::Unavailable(ContentUnavailable::UnsupportedEncoding)
                )
                | (2, ContentResult::Missing)
                | (3, ContentResult::Unavailable(ContentUnavailable::Directory))
        ));
    }
    let original = ready(
        repository
            .content(
                &store,
                request(ContentSource::Worktree {
                    path: path("text"),
                    conversion: forge_git::content::WorktreeConversion::Raw,
                }),
            )
            .await
            .unwrap()
            .value,
    );
    fs::write(root.join("text"), b"replaced").unwrap();
    let mut expected = request(ContentSource::Worktree {
        path: path("text"),
        conversion: forge_git::content::WorktreeConversion::Raw,
    });
    expected.expected = Some(original.source.identity());
    let result = repository.content(&store, expected).await.unwrap().value;
    let ContentResult::Failed(error) = result else {
        panic!("{result:?}")
    };
    assert!(error.to_string().contains("identity differs"));
}

#[tokio::test]
async fn object_reads_ignore_replacement_refs_and_support_sha256() {
    for format in ["sha1", "sha256"] {
        let fixture = tempfile::tempdir().unwrap();
        let root = fixture.path();
        if format == "sha256" {
            init_sha256_repository(root);
        } else {
            init_repository(root);
        }
        let original = write_blob(root, b"original");
        let replacement = write_blob(root, b"replacement");
        set_reference(root, &format!("refs/replace/{original}"), replacement);
        let store = RepositoryStore::default();
        let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
        let loaded = ready(
            repository
                .content(&store, request(ContentSource::Object(original)))
                .await
                .unwrap()
                .value,
        );
        assert_eq!(loaded.source.bytes(), b"original");
        assert!(matches!(loaded.origin, ContentOrigin::Object(object) if object == original));
    }
}

#[tokio::test]
async fn packed_blob_and_worktree_beyond_former_byte_limit_preserve_complete_content() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    let bytes = b"content\r\n".repeat(1_000_000);
    fs::write(root.join("large"), &bytes).unwrap();
    set_config(root, "core", "autocrlf", "false");
    stage(root, &["large"]);
    commit_index(root, "fixture");
    let large = resolve_revision(root, "HEAD:large");
    git(root, &["gc", "--prune=now"]);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    for source in [
        ContentSource::Object(large),
        ContentSource::Worktree {
            path: path("large"),
            conversion: forge_git::content::WorktreeConversion::Raw,
        },
    ] {
        let result = repository
            .content(&store, request(source))
            .await
            .unwrap()
            .value;
        assert_eq!(ready(result).source.bytes(), bytes);
    }
}

#[tokio::test]
async fn supplied_sources_preserve_identity_lines_and_spare_capacity() {
    let fixture = tempfile::tempdir().unwrap();
    init_repository(fixture.path());
    let store = RepositoryStore::default();
    let repository = store
        .open(fixture.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let source =
        SourceVersion::new(vec![b'a'; 8 * 1024 * 1024 + 1], Representation::DisplayOnly).unwrap();
    let identity = source.identity();
    let loaded = ready(
        repository
            .content(&store, request(ContentSource::Supplied(source)))
            .await
            .unwrap()
            .value,
    );
    assert_eq!(loaded.source.identity(), identity);
    let source =
        SourceVersion::new(b"one\r\ntwo\nthree".repeat(10_001), Representation::Raw).unwrap();
    let result = repository
        .content(&store, request(ContentSource::Supplied(source.clone())))
        .await
        .unwrap()
        .value;
    assert_eq!(ready(result).source.bytes(), source.bytes());
    let constrained = RepositoryStore::new(1, 1).unwrap();
    let repository = constrained
        .open(fixture.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let mut spare = Vec::with_capacity(4096);
    spare.push(b'x');
    let source = SourceVersion::new(spare, Representation::Raw).unwrap();
    let result = repository
        .content(&constrained, request(ContentSource::Supplied(source)))
        .await
        .unwrap()
        .value;
    assert_eq!(ready(result).source.bytes(), b"x");
}

#[tokio::test]
async fn conflict_stage_reads_select_exact_stage_and_literal_path() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    stage_conflict(root, "[conflict]", b"base\n", b"ours\n", b"theirs\n");
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    for (stage, text) in [
        (IndexStage::Base, b"base\n".as_slice()),
        (IndexStage::Ours, b"ours\n"),
        (IndexStage::Theirs, b"theirs\n"),
    ] {
        let result = repository
            .content(
                &store,
                request(ContentSource::IndexStage {
                    path: path("[conflict]"),
                    stage,
                }),
            )
            .await
            .unwrap()
            .value;
        assert_eq!(ready(result).source.bytes(), text);
    }
    assert!(matches!(
        repository
            .content(
                &store,
                request(ContentSource::IndexStage {
                    path: path("[conflict]"),
                    stage: IndexStage::Normal
                })
            )
            .await
            .unwrap()
            .value,
        ContentResult::Missing
    ));
}

#[tokio::test]
async fn empty_missing_and_non_blob_objects_are_not_conflated() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    fs::write(root.join("empty"), b"").unwrap();
    stage(root, &["empty"]);
    commit_index(root, "fixture");
    let empty = resolve_revision(root, "HEAD:empty");
    let commit = resolve_revision(root, "HEAD");
    stage_gitlink(root, "link", commit);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let loaded = ready(
        repository
            .content(&store, request(ContentSource::Object(empty)))
            .await
            .unwrap()
            .value,
    );
    assert!(loaded.source.bytes().is_empty());
    assert_eq!(loaded.source.newline().line_count, 0);
    assert!(matches!(
        repository
            .content(&store, request(ContentSource::Object(commit)))
            .await
            .unwrap()
            .value,
        ContentResult::Unavailable(ContentUnavailable::NotBlob(gix::objs::Kind::Commit))
    ));
    assert!(matches!(
        repository
            .content(
                &store,
                request(ContentSource::Object(gix::ObjectId::null(
                    gix::hash::Kind::Sha1
                )))
            )
            .await
            .unwrap()
            .value,
        ContentResult::Missing
    ));
    assert!(matches!(
        repository
            .content(
                &store,
                request(ContentSource::IndexStage {
                    path: path("link"),
                    stage: IndexStage::Normal
                })
            )
            .await
            .unwrap()
            .value,
        ContentResult::Unavailable(ContentUnavailable::Gitlink)
    ));
}

#[cfg(unix)]
#[tokio::test]
async fn symlink_worktree_content_is_the_target_text() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    fs::write(root.join("target"), b"must not be read").unwrap();
    std::os::unix::fs::symlink("target", root.join("link")).unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let loaded = ready(
        repository
            .content(
                &store,
                request(ContentSource::Worktree {
                    path: path("link"),
                    conversion: forge_git::content::WorktreeConversion::Raw,
                }),
            )
            .await
            .unwrap()
            .value,
    );
    assert_eq!(loaded.source.bytes(), b"target");
}

fn request(source: ContentSource) -> ContentRequest {
    ContentRequest {
        source,
        expected: None,
    }
}

fn path(name: &str) -> RepositoryPath {
    RepositoryPath::new(name.as_bytes().to_vec()).unwrap()
}

fn ready(result: ContentResult) -> FileContent {
    match result {
        ContentResult::Ready(content) => content,
        other => panic!("{other:?}"),
    }
}
