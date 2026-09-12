mod support;

use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Seek, SeekFrom, Write},
    path::Path,
};

use forge_diff::source::{MAX_SOURCE_BYTES, Representation, SourceVersion};
use forge_git::{
    RepositoryPath,
    content::{
        ContentLimit, ContentLimits, ContentOrigin, ContentRequest, ContentResult, ContentSource,
        ContentUnavailable, FileContent, IndexStage,
    },
    store::RepositoryStore,
};
use support::git;

#[tokio::test]
async fn staged_unstaged_and_head_sources_remain_distinct() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "core.autocrlf", "false"]);
    fs::write(root.join("tracked"), b"head\r\n").unwrap();
    git(root, &["add", "tracked"]);
    commit(root);
    let head = object(&git(root, &["rev-parse", "HEAD:tracked"]));
    fs::write(root.join("tracked"), b"staged\r\n").unwrap();
    git(root, &["add", "tracked"]);
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
    git(root, &["init", "--quiet"]);
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
    let mut limited = request(ContentSource::Worktree {
        path: path("text"),
        conversion: forge_git::content::WorktreeConversion::Raw,
    });
    limited.limits = ContentLimits::new(3, usize::MAX).unwrap();
    assert!(matches!(
        repository.content(&store, limited).await.unwrap().value,
        ContentResult::TooLarge {
            kind: ContentLimit::Bytes,
            observed: 8,
            limit: 3
        }
    ));
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
        git(
            root,
            &["init", "--quiet", &format!("--object-format={format}")],
        );
        fs::write(root.join("original"), b"original").unwrap();
        fs::write(root.join("replacement"), b"replacement").unwrap();
        let original = object(&git(root, &["hash-object", "-w", "original"]));
        let replacement = object(&git(root, &["hash-object", "-w", "replacement"]));
        git(
            root,
            &["replace", &original.to_string(), &replacement.to_string()],
        );
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
async fn oversized_packed_blob_is_rejected_without_decoding_its_corrupt_payload() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    File::create(root.join("large"))
        .unwrap()
        .set_len(MAX_SOURCE_BYTES as u64 + 1)
        .unwrap();
    git(root, &["add", "large"]);
    commit(root);
    let large = object(&git(root, &["rev-parse", "HEAD:large"]));
    git(root, &["gc", "--prune=now"]);
    let pack_directory = root.join(".git/objects/pack");
    let index = fs::read_dir(&pack_directory)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .find(|path| path.extension().is_some_and(|extension| extension == "idx"))
        .unwrap();
    let listing =
        String::from_utf8(git(root, &["verify-pack", "-v", index.to_str().unwrap()])).unwrap();
    let line = listing
        .lines()
        .find(|line| line.starts_with(&large.to_string()))
        .unwrap();
    let offset = line
        .split_whitespace()
        .nth(4)
        .unwrap()
        .parse::<u64>()
        .unwrap();
    let pack_path = index.with_extension("pack");
    let mut permissions = fs::metadata(&pack_path).unwrap().permissions();
    #[cfg(windows)]
    #[expect(
        clippy::permissions_set_readonly_false,
        reason = "Windows clears a DOS attribute. Unix uses an explicit owner-only mode below."
    )]
    permissions.set_readonly(false);
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        permissions.set_mode(0o600);
    }
    fs::set_permissions(&pack_path, permissions).unwrap();
    let mut pack = OpenOptions::new()
        .read(true)
        .write(true)
        .open(pack_path)
        .unwrap();
    pack.seek(SeekFrom::Start(offset)).unwrap();
    let mut header = [0];
    loop {
        pack.read_exact(&mut header).unwrap();
        if header[0] & 0x80 == 0 {
            break;
        }
    }
    pack.write_all(&[0]).unwrap();
    drop(pack);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let result = repository
        .content(&store, request(ContentSource::Object(large)))
        .await
        .unwrap()
        .value;
    assert!(
        matches!(result, ContentResult::TooLarge { kind: ContentLimit::Bytes, observed, limit } if observed == MAX_SOURCE_BYTES as u64 + 1 && limit == MAX_SOURCE_BYTES as u64)
    );
    assert!(gix::open(root).unwrap().find_object(large).is_err());
}

#[tokio::test]
async fn supplied_sources_keep_identity_and_respect_line_and_input_admission() {
    let fixture = tempfile::tempdir().unwrap();
    git(fixture.path(), &["init", "--quiet"]);
    let store = RepositoryStore::default();
    let repository = store
        .open(fixture.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let source =
        SourceVersion::new(vec![b'a'; MAX_SOURCE_BYTES], Representation::DisplayOnly).unwrap();
    let identity = source.identity();
    let loaded = ready(
        repository
            .content(&store, request(ContentSource::Supplied(source)))
            .await
            .unwrap()
            .value,
    );
    assert_eq!(loaded.source.identity(), identity);
    let source = SourceVersion::new(b"one\r\ntwo\nthree".to_vec(), Representation::Raw).unwrap();
    let mut limited = request(ContentSource::Supplied(source));
    limited.limits = ContentLimits::new(100, 2).unwrap();
    assert!(matches!(
        repository.content(&store, limited).await.unwrap().value,
        ContentResult::TooLarge {
            kind: ContentLimit::Lines,
            observed: 3,
            limit: 2
        }
    ));
    assert!(ContentLimits::new(MAX_SOURCE_BYTES + 1, 1).is_err());

    let constrained = RepositoryStore::new(1, 1, 1024).unwrap();
    let repository = constrained
        .open(fixture.path().to_path_buf())
        .await
        .unwrap()
        .unwrap();
    let mut spare = Vec::with_capacity(4096);
    spare.push(b'x');
    let source = SourceVersion::new(spare, Representation::Raw).unwrap();
    assert!(
        repository
            .content(&constrained, request(ContentSource::Supplied(source)))
            .await
            .is_err()
    );
}

#[tokio::test]
async fn conflict_stage_reads_select_exact_stage_and_literal_path() {
    let fixture = tempfile::tempdir().unwrap();
    let root = fixture.path();
    git(root, &["init", "--quiet"]);
    let mut tree = Vec::new();
    for text in ["base\n", "ours\n", "theirs\n"] {
        fs::write(root.join("[conflict]"), text).unwrap();
        git(root, &["--literal-pathspecs", "add", "--", "[conflict]"]);
        tree.push(
            String::from_utf8(git(root, &["write-tree"]))
                .unwrap()
                .trim()
                .to_owned(),
        );
    }
    git(root, &["read-tree", "--empty"]);
    git(
        root,
        &["read-tree", "-i", "-m", &tree[0], &tree[1], &tree[2]],
    );
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
    git(root, &["init", "--quiet"]);
    fs::write(root.join("empty"), b"").unwrap();
    git(root, &["add", "empty"]);
    commit(root);
    let empty = object(&git(root, &["rev-parse", "HEAD:empty"]));
    let commit = object(&git(root, &["rev-parse", "HEAD"]));
    git(
        root,
        &[
            "update-index",
            "--add",
            "--cacheinfo",
            &format!("160000,{commit},link"),
        ],
    );
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
    git(root, &["init", "--quiet"]);
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
        limits: ContentLimits::default(),
        expected: None,
    }
}

fn path(name: &str) -> RepositoryPath {
    RepositoryPath::new(name.as_bytes().to_vec()).unwrap()
}

fn object(output: &[u8]) -> gix::ObjectId {
    gix::ObjectId::from_hex(output.trim_ascii()).unwrap()
}

fn ready(result: ContentResult) -> FileContent {
    match result {
        ContentResult::Ready(content) => content,
        other => panic!("{other:?}"),
    }
}

fn commit(root: &Path) {
    git(
        root,
        &[
            "-c",
            "user.name=Forge",
            "-c",
            "user.email=forge@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "fixture",
        ],
    );
}
