use forge_git::test_support as support;

use std::fs;

use forge_diff::source::Representation;
use forge_git::{
    RepositoryPath,
    content::{
        ContentOrigin, ContentRequest, ContentResult, ContentSource, ContentUnavailable,
        FileContent, IndexStage,
    },
    store::RepositoryStore,
};
use support::{commit_index, init_repository, resolve_revision, set_config, stage, stage_conflict};

/// Resolve both pointer sides and pointer-only worktrees while leaving canonical Git reads intact.
#[tokio::test]
async fn local_lfs_diff_uses_shared_and_configured_storage_without_filters() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    init_repository(root);
    fs::write(root.join(".gitattributes"), "*.ts filter=lfs -text\n").unwrap();
    let (old, _) = support::lfs_pointer(&root.join(".git/lfs"), b"const value = 1;\n");
    let (new, _) = support::lfs_pointer(&root.join(".git/lfs"), b"const value = 2;\n");
    fs::write(root.join("source.ts"), &old).unwrap();
    stage(root, &[".gitattributes", "source.ts"]);
    let commit = commit_index(root, "LFS source");
    let old_object = resolve_revision(root, "HEAD:source.ts");
    let linked = root.join("linked");
    support::add_detached_worktree(root, &linked, commit);
    set_config(
        root,
        "filter \"lfs\"",
        "process",
        "forge-test-filter-must-not-run",
    );
    set_config(root, "filter \"lfs\"", "required", "true");
    fs::write(linked.join("source.ts"), &new).unwrap();
    stage(&linked, &["source.ts"]);
    let store = RepositoryStore::default();
    let repository = store.open(linked.clone()).await.unwrap().unwrap();
    for configured in [false, true] {
        if configured {
            fs::rename(root.join(".git/lfs"), root.join(".git/custom-lfs")).unwrap();
            set_config(root, "lfs", "storage", "custom-lfs");
        }
        for current in [
            ContentSource::IndexStage {
                path: path("source.ts"),
                stage: IndexStage::Normal,
            },
            ContentSource::Worktree {
                path: path("source.ts"),
                conversion: forge_git::content::WorktreeConversion::GitCanonical,
            },
        ] {
            let source = repository
                .lfs_diff_sources(
                    &store,
                    path("source.ts"),
                    [Some(ContentSource::Object(old_object)), Some(current)],
                )
                .await
                .unwrap()
                .value
                .unwrap();
            assert_eq!(source.old.text(), "const value = 1;\n");
            assert_eq!(source.new.text(), "const value = 2;\n");
            assert_eq!(
                source.old.identity().representation,
                Representation::DisplayOnly
            );
            assert_eq!(
                source.new.identity().representation,
                Representation::DisplayOnly
            );
        }
    }
    let canonical = ready(
        repository
            .content(&store, request(ContentSource::Object(old_object)))
            .await
            .unwrap()
            .value,
    );
    assert_eq!(canonical.source.text(), old);
    assert_eq!(
        canonical.source.identity().representation,
        Representation::GitCanonical
    );
    let canonical_worktree = repository
        .content(
            &store,
            request(ContentSource::Worktree {
                path: path("source.ts"),
                conversion: forge_git::content::WorktreeConversion::GitCanonical,
            }),
        )
        .await
        .unwrap()
        .value;
    assert!(matches!(
        canonical_worktree,
        ContentResult::Unavailable(ContentUnavailable::ExternalFilter)
    ));
    assert!(
        repository
            .lfs_diff_sources(&store, path("normal.txt"), [None, None])
            .await
            .unwrap()
            .value
            .is_none()
    );
}

/// Report missing, corrupt, binary, and malformed LFS inputs rather than rendering pointer text.
#[tokio::test]
async fn local_lfs_diff_reports_unavailable_and_invalid_payloads() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    let local = init_repository(root);
    fs::write(root.join(".gitattributes"), "*.ts filter=lfs -text\n").unwrap();
    set_config(
        root,
        "filter \"lfs\"",
        "process",
        "forge-test-filter-must-not-run",
    );
    let store = RepositoryStore::default();
    let repository = store.open(root.to_owned()).await.unwrap().unwrap();
    for (scenario, diagnostic) in [
        ("missing", "LFS content not downloaded"),
        ("hash", "SHA-256 mismatch"),
        ("size", "size mismatch"),
        ("binary", "binary NUL"),
        ("encoding", "not valid UTF-8"),
        ("pointer", "invalid LFS SHA-256"),
        ("extension", "unsupported LFS extensions"),
    ] {
        let payload = match scenario {
            "binary" => b"a\0b".as_slice(),
            "encoding" => b"a\xffb".as_slice(),
            _ => b"old".as_slice(),
        };
        let (mut pointer, object_path) = support::lfs_pointer(&root.join(".git/lfs"), payload);
        match scenario {
            "missing" => fs::remove_file(object_path).unwrap(),
            "hash" => fs::write(object_path, b"bad").unwrap(),
            "size" => fs::write(object_path, b"too long").unwrap(),
            "pointer" => {
                pointer =
                    "version https://git-lfs.github.com/spec/v1\noid sha256:../../escape\nsize 3\n"
                        .into()
            }
            "extension" => {
                pointer = pointer.replacen("\noid ", "\next-0-custom sha256:unused\noid ", 1)
            }
            _ => {}
        }
        let object = local.write_blob(pointer.as_bytes()).unwrap().detach();
        let error = repository
            .lfs_diff_sources(
                &store,
                path("source.ts"),
                [Some(ContentSource::Object(object)), None],
            )
            .await
            .err()
            .expect("invalid LFS content must fail");
        assert!(
            format!("{error:#}").contains(diagnostic),
            "{scenario}: {error:#}"
        );
    }
    let (empty, _) = support::lfs_pointer(&root.join(".git/lfs"), b"");
    let object = local.write_blob(empty.as_bytes()).unwrap().detach();
    let source = repository
        .lfs_diff_sources(
            &store,
            path("source.ts"),
            [None, Some(ContentSource::Object(object))],
        )
        .await
        .unwrap()
        .value
        .unwrap();
    assert!(source.old.text().is_empty() && source.new.text().is_empty());
}

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
