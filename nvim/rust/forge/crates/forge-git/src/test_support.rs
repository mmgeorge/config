//! Fixture repositories for forge-git tests.
//!
//! Launching `git` costs 50 to 150 ms per call on Windows, which dominated these suites. The
//! helpers here build the same on-disk state in-process through `gix`: `git init`, `git config`,
//! `git add`, `git commit`, `git branch`, `git tag`, and `git update-ref`. Tests that verify real
//! Git behavior (clean/smudge filters, object identity, the writer, `merge-tree`) keep calling
//! [`git`], which still launches the executable.
//!
//! The library's unit tests use it directly. Integration tests and other crates' tests reach it as
//! `forge_git::test_support` by enabling the `test-fixtures` feature in their dev-dependencies.
#![allow(dead_code)]

use std::path::Path;

use gix::{ObjectId, bstr::ByteSlice, objs::tree};

/// Fixed commit time so fixtures are deterministic.
const FIXTURE_COMMIT_SECONDS: i64 = 1_700_000_000;
const FIXTURE_NAME: &str = "Forge Test";
const FIXTURE_EMAIL: &str = "forge@example.invalid";

/// Runs the real `git` executable, for tests whose subject is real Git behavior.
pub fn git(root: &Path, arguments: &[&str]) -> Vec<u8> {
    let output = std::process::Command::new("git")
        .arg("--no-optional-locks")
        .arg("-C")
        .arg(root)
        .args(["-c", "core.fsmonitor=false"])
        .args(arguments)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

/// Initializes a non-bare SHA-1 repository whose unborn branch is `main`.
pub fn init_repository(root: &Path) -> gix::Repository {
    init_repository_with_branch(root, "main")
}

/// Initializes a non-bare SHA-1 repository whose unborn branch is `initial_branch`.
pub fn init_repository_with_branch(root: &Path, initial_branch: &str) -> gix::Repository {
    std::fs::create_dir_all(root).unwrap();
    let repository = gix::init(root).unwrap();
    point_head_at_branch(&repository, initial_branch);
    repository
}

/// Points `HEAD` at `branch` without touching any reference, like `git symbolic-ref HEAD
/// refs/heads/<branch>`.
pub fn set_head_branch(root: &Path, branch: &str) {
    point_head_at_branch(&gix::open(root).unwrap(), branch);
}

/// Initializes a bare SHA-1 repository.
pub fn init_bare_repository(root: &Path) -> gix::Repository {
    std::fs::create_dir_all(root).unwrap();
    gix::init_bare(root).unwrap()
}

/// Initializes a non-bare repository that stores SHA-256 object names, as `git init
/// --object-format=sha256` does: format version 1 plus the `objectformat` extension.
pub fn init_sha256_repository(root: &Path) -> gix::Repository {
    std::fs::create_dir_all(root).unwrap();
    gix::init(root).unwrap();
    let config_path = root.join(".git").join("config");
    let config = std::fs::read_to_string(&config_path)
        .unwrap()
        .replace("repositoryformatversion = 0", "repositoryformatversion = 1");
    std::fs::write(
        &config_path,
        format!("{config}[extensions]\n\tobjectformat = sha256\n"),
    )
    .unwrap();
    gix::open(root).unwrap()
}

fn point_head_at_branch(repository: &gix::Repository, branch: &str) {
    std::fs::write(
        repository.git_dir().join("HEAD"),
        format!("ref: refs/heads/{branch}\n"),
    )
    .unwrap();
}

/// Appends `[section]\n\tkey = value` to the repository's own configuration file. Git accepts
/// repeated sections, so no existing text is rewritten.
pub fn set_config(root: &Path, section: &str, key: &str, value: &str) {
    use std::io::Write;
    let repository = gix::open(root).unwrap();
    let mut config = std::fs::OpenOptions::new()
        .append(true)
        .open(repository.git_dir().join("config"))
        .unwrap();
    write!(config, "[{section}]\n\t{key} = {value}\n").unwrap();
}

/// Gives the repository the fixture author identity, so later real `git commit` calls succeed.
pub fn set_identity(root: &Path) {
    set_config(root, "user", "name", FIXTURE_NAME);
    set_config(root, "user", "email", FIXTURE_EMAIL);
}

fn open_index(repository: &gix::Repository) -> gix::index::File {
    let index_path = repository.index_path();
    if index_path.exists() {
        repository.open_index().unwrap()
    } else {
        gix::index::File::from_state(gix::index::State::new(repository.object_hash()), index_path)
    }
}

fn index_path_bytes(relative: &str) -> Vec<u8> {
    relative.replace('\\', "/").into_bytes()
}

/// Stages worktree files byte for byte (no text conversion), like `git add` with
/// `core.autocrlf=false`. A missing file removes its index entry, like `git add` on a deletion.
pub fn stage(root: &Path, relative_paths: &[&str]) {
    let repository = gix::open(root).unwrap();
    let worktree = repository.workdir().unwrap().to_owned();
    let mut index = open_index(&repository);
    for relative in relative_paths {
        let path_bytes = index_path_bytes(relative);
        index.remove_entries(|_, entry_path, _| entry_path.as_bytes() == path_bytes.as_slice());
        let absolute = worktree.join(relative);
        let Ok(metadata) = std::fs::symlink_metadata(&absolute) else {
            continue;
        };
        let content = std::fs::read(&absolute).unwrap();
        let blob = repository.write_blob(&content).unwrap().detach();
        let stat = gix::index::entry::Stat::from_fs(
            &gix::index::fs::Metadata::from_path_no_follow(&absolute).unwrap(),
        )
        .unwrap();
        assert!(metadata.is_file(), "fixture staging supports regular files");
        index.dangerously_push_entry(
            stat,
            blob,
            gix::index::entry::Flags::empty(),
            gix::index::entry::Mode::FILE,
            path_bytes.as_slice().as_bstr(),
        );
    }
    index.sort_entries();
    index.write(Default::default()).unwrap();
}

/// Replaces the index entry for `relative` with base, ours, and theirs entries at stages 1 to 3, like
/// `git update-index --index-info` fed a conflicted path. Each stage's content is written as a blob.
pub fn stage_conflict(root: &Path, relative: &str, base: &[u8], ours: &[u8], theirs: &[u8]) {
    let repository = gix::open(root).unwrap();
    let mut index = open_index(&repository);
    let path_bytes = index_path_bytes(relative);
    index.remove_entries(|_, entry_path, _| entry_path.as_bytes() == path_bytes.as_slice());
    for (stage_number, content) in [(1_u32, base), (2, ours), (3, theirs)] {
        let blob = repository.write_blob(content).unwrap().detach();
        index.dangerously_push_entry(
            gix::index::entry::Stat::default(),
            blob,
            gix::index::entry::Flags::from_bits_retain(stage_number << 12),
            gix::index::entry::Mode::FILE,
            path_bytes.as_slice().as_bstr(),
        );
    }
    index.sort_entries();
    index.write(Default::default()).unwrap();
}

/// Stages a gitlink (submodule) entry at `relative` pointing at `commit`, like `git update-index
/// --add --cacheinfo 160000,<commit>,<path>`.
pub fn stage_gitlink(root: &Path, relative: &str, commit: ObjectId) {
    let repository = gix::open(root).unwrap();
    let mut index = open_index(&repository);
    let path_bytes = index_path_bytes(relative);
    index.remove_entries(|_, entry_path, _| entry_path.as_bytes() == path_bytes.as_slice());
    index.dangerously_push_entry(
        gix::index::entry::Stat::default(),
        commit,
        gix::index::entry::Flags::empty(),
        gix::index::entry::Mode::COMMIT,
        path_bytes.as_slice().as_bstr(),
    );
    index.sort_entries();
    index.write(Default::default()).unwrap();
}

/// Stages `source` as `destination` and removes `source` from the worktree and index, like
/// `git mv`.
pub fn move_tracked(root: &Path, source: &str, destination: &str) {
    std::fs::rename(root.join(source), root.join(destination)).unwrap();
    stage(root, &[source, destination]);
}

/// Stages every regular file below the worktree except `.git` and removes index entries whose file
/// is gone, like `git add .` with `core.autocrlf=false`.
pub fn stage_all(root: &Path) {
    let mut relative_paths = Vec::new();
    collect_worktree_files(root, root, &mut relative_paths);
    let repository = gix::open(root).unwrap();
    let index = open_index(&repository);
    for entry in index.entries() {
        let path = entry.path(&index).to_str().unwrap().to_owned();
        if !relative_paths.contains(&path) {
            relative_paths.push(path);
        }
    }
    let borrowed = relative_paths
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>();
    stage(root, &borrowed);
}

/// Resets the index to the `HEAD` tree and leaves the worktree alone, like `git reset --quiet`.
pub fn reset_index_to_head(root: &Path) {
    let repository = gix::open(root).unwrap();
    let tree = repository.head_commit().unwrap().tree_id().unwrap();
    let mut state =
        gix::index::State::from_tree(&tree, &repository.objects, Default::default()).unwrap();
    let paths = state
        .entries()
        .iter()
        .map(|entry| entry.path(&state).to_str().unwrap().to_owned())
        .collect::<Vec<_>>();
    // Like `git reset`, record the worktree stat only for files whose content equals the `HEAD`
    // blob. A modified file keeps the zero stat, so status rehashes it instead of trusting a stat
    // that describes different content.
    for (entry, path) in state.entries_mut().iter_mut().zip(paths) {
        let absolute = root.join(path);
        let Ok(content) = std::fs::read(&absolute) else {
            continue;
        };
        if repository.find_blob(entry.id).unwrap().data != content {
            continue;
        }
        if let Ok(metadata) = gix::index::fs::Metadata::from_path_no_follow(&absolute) {
            entry.stat = gix::index::entry::Stat::from_fs(&metadata).unwrap();
        }
    }
    let mut index = gix::index::File::from_state(state, repository.index_path());
    index.write(Default::default()).unwrap();
}

fn collect_worktree_files(root: &Path, directory: &Path, found: &mut Vec<String>) {
    for entry in std::fs::read_dir(directory).unwrap() {
        let entry = entry.unwrap();
        if entry.file_name() == ".git" {
            continue;
        }
        if entry.file_type().unwrap().is_dir() {
            collect_worktree_files(root, &entry.path(), found);
        } else {
            let relative = entry.path().strip_prefix(root).unwrap().to_owned();
            found.push(relative.to_string_lossy().replace('\\', "/"));
        }
    }
}

#[derive(Default)]
struct TreeNode {
    files: std::collections::BTreeMap<Vec<u8>, (tree::EntryMode, ObjectId)>,
    directories: std::collections::BTreeMap<Vec<u8>, TreeNode>,
}

fn write_tree(repository: &gix::Repository, node: &TreeNode) -> ObjectId {
    let mut entries = Vec::new();
    for (name, (mode, id)) in &node.files {
        entries.push(tree::Entry {
            mode: *mode,
            filename: name.clone().into(),
            oid: *id,
        });
    }
    for (name, child) in &node.directories {
        entries.push(tree::Entry {
            mode: tree::EntryKind::Tree.into(),
            filename: name.clone().into(),
            oid: write_tree(repository, child),
        });
    }
    entries.sort();
    repository
        .write_object(gix::objs::Tree { entries })
        .unwrap()
        .detach()
}

/// Writes the tree object for the current index, like `git write-tree`.
pub fn write_index_tree(root: &Path) -> ObjectId {
    let repository = gix::open(root).unwrap();
    let index = open_index(&repository);
    let mut top = TreeNode::default();
    for entry in index.entries() {
        let path = entry.path(&index);
        let mut components = path.split(|byte| *byte == b'/').peekable();
        let mut node = &mut top;
        while let Some(component) = components.next() {
            if components.peek().is_some() {
                node = node.directories.entry(component.to_vec()).or_default();
            } else {
                node.files
                    .insert(component.to_vec(), (tree::EntryKind::Blob.into(), entry.id));
            }
        }
    }
    write_tree(&repository, &top)
}

fn fixture_signature() -> gix::actor::Signature {
    gix::actor::Signature {
        name: FIXTURE_NAME.into(),
        email: FIXTURE_EMAIL.into(),
        time: gix::date::Time::new(FIXTURE_COMMIT_SECONDS, 0),
    }
}

/// Commits the index on the current branch (or detached HEAD) with the fixture identity, like
/// `git commit -m`. Returns the new commit.
pub fn commit_index(root: &Path, message: &str) -> ObjectId {
    let tree = write_index_tree(root);
    let repository = gix::open(root).unwrap();
    let parents = repository
        .head_id()
        .ok()
        .map(|id| vec![id.detach()])
        .unwrap_or_default();
    let commit = repository
        .write_object(gix::objs::Commit {
            tree,
            parents: parents.into(),
            author: fixture_signature(),
            committer: fixture_signature(),
            encoding: None,
            message: message.into(),
            extra_headers: Vec::new(),
        })
        .unwrap()
        .detach();
    let reference_name = repository
        .head_name()
        .unwrap()
        .map(|name| name.as_bstr().to_string())
        .unwrap_or_else(|| "HEAD".to_owned());
    set_reference(root, &reference_name, commit);
    commit
}

/// Stages `relative_paths` and commits them, the common `git add` plus `git commit` pair.
pub fn stage_and_commit(root: &Path, relative_paths: &[&str], message: &str) -> ObjectId {
    stage(root, relative_paths);
    commit_index(root, message)
}

/// Creates or moves the reference `full_name` (for example `refs/heads/topic`) to `target`.
pub fn set_reference(root: &Path, full_name: &str, target: ObjectId) {
    let repository = gix::open(root).unwrap();
    repository
        .reference(
            full_name,
            target,
            gix::refs::transaction::PreviousValue::Any,
            "forge-git test fixture",
        )
        .unwrap();
}

/// Points the symbolic reference `full_name` at `target_name`, like `git symbolic-ref`.
pub fn set_symbolic_reference(root: &Path, full_name: &str, target_name: &str) {
    use gix::refs::{
        Target,
        transaction::{Change, LogChange, PreviousValue, RefEdit},
    };
    let repository = gix::open(root).unwrap();
    repository
        .edit_reference(RefEdit {
            change: Change::Update {
                log: LogChange::default(),
                expected: PreviousValue::Any,
                new: Target::Symbolic(target_name.try_into().unwrap()),
            },
            name: full_name.try_into().unwrap(),
            deref: false,
        })
        .unwrap();
}

/// Returns the commit `HEAD` names.
pub fn head_commit(root: &Path) -> ObjectId {
    gix::open(root).unwrap().head_id().unwrap().detach()
}

/// Creates branch `name` at `HEAD`, like `git branch name`.
pub fn create_branch(root: &Path, name: &str) {
    set_reference(root, &format!("refs/heads/{name}"), head_commit(root));
}

/// Creates lightweight tag `name` at `HEAD`, like `git tag name`.
pub fn create_tag(root: &Path, name: &str) {
    set_reference(root, &format!("refs/tags/{name}"), head_commit(root));
}

/// Writes `content` as a loose blob and returns its object id, like `git hash-object -w`.
pub fn write_blob(root: &Path, content: &[u8]) -> ObjectId {
    gix::open(root)
        .unwrap()
        .write_blob(content)
        .unwrap()
        .detach()
}

/// Resolves a revision such as `HEAD:path`, like `git rev-parse`.
pub fn resolve_revision(root: &Path, specification: &str) -> ObjectId {
    gix::open(root)
        .unwrap()
        .rev_parse_single(specification)
        .unwrap()
        .detach()
}

/// Creates `linked` as a detached linked worktree at `commit`, with the administrative layout
/// `git worktree add --detach` writes: `.git/worktrees/<name>/{HEAD,commondir,gitdir,index}` and a
/// `.git` file in the linked directory. The linked index and checkout match the commit's tree.
pub fn add_detached_worktree(root: &Path, linked: &Path, commit: ObjectId) {
    let repository = gix::open(root).unwrap();
    let name = linked.file_name().unwrap().to_string_lossy().into_owned();
    let admin = repository.common_dir().join("worktrees").join(&name);
    std::fs::create_dir_all(&admin).unwrap();
    std::fs::create_dir_all(linked).unwrap();
    std::fs::write(admin.join("HEAD"), format!("{commit}\n")).unwrap();
    std::fs::write(admin.join("commondir"), "../..\n").unwrap();
    let linked_git_file = linked.join(".git");
    std::fs::write(
        admin.join("gitdir"),
        format!("{}\n", slash_path(&linked_git_file)),
    )
    .unwrap();
    std::fs::write(
        &linked_git_file,
        format!("gitdir: {}\n", slash_path(&admin)),
    )
    .unwrap();
    let tree = repository.find_commit(commit).unwrap().tree_id().unwrap();
    let mut state =
        gix::index::State::from_tree(&tree, &repository.objects, Default::default()).unwrap();
    let mut files = Vec::new();
    for entry in state.entries() {
        files.push((entry.path(&state).to_owned(), entry.id));
    }
    for (relative, id) in files {
        let destination = linked.join(relative.to_str().unwrap());
        std::fs::create_dir_all(destination.parent().unwrap()).unwrap();
        let blob = repository.find_blob(id).unwrap();
        std::fs::write(&destination, &blob.data).unwrap();
    }
    let stats = state
        .entries()
        .iter()
        .map(|entry| {
            let path = linked.join(entry.path(&state).to_str().unwrap());
            gix::index::entry::Stat::from_fs(
                &gix::index::fs::Metadata::from_path_no_follow(&path).unwrap(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    for (entry, stat) in state.entries_mut().iter_mut().zip(stats) {
        entry.stat = stat;
    }
    let mut index = gix::index::File::from_state(state, admin.join("index"));
    index.write(Default::default()).unwrap();
}

fn slash_path(path: &Path) -> String {
    path.to_string_lossy().replace('\\', "/")
}
