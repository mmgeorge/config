use std::{
    collections::{BTreeMap, BTreeSet},
    io::{Read, Seek, Write},
    path::Path,
};

use anyhow::{Result, ensure};
use gix::bstr::ByteSlice;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// Captures checkout conversion without retaining mutable repository configuration.
pub struct CheckoutRule {
    /// Expands LF to CRLF for text content.
    pub crlf: bool,
    /// Leaves binary content unchanged when text detection is automatic.
    pub automatic: bool,
    /// Explains a conversion that cannot be reproduced without an external dependency.
    pub unsupported: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies existing committed content and its working-tree representation.
pub struct BaseFile {
    /// Git object identity, including the repository's hash format.
    pub object_id: String,
    /// Git file mode.
    pub mode: u32,
    /// Conversion sampled at capture time.
    pub checkout: CheckoutRule,
}

/// Supplies metadata-only candidates without using content-comparing status.
#[derive(Debug, Eq, PartialEq)]
pub struct CheckpointInventory {
    /// Existing commit, or no commit in an unborn repository.
    pub head: Option<String>,
    /// Existing tree, or no tree in an unborn repository.
    pub tree: Option<String>,
    /// Existing committed files. Trees are read, file blobs are not.
    pub base: BTreeMap<String, BaseFile>,
    /// Path and whether Git's index proves it unchanged from the base.
    pub candidate: BTreeMap<String, bool>,
    /// Working-file observations used to reject concurrent capture changes.
    pub observation: BTreeMap<String, crate::snapshot::WorktreeStamp>,
    /// Index version sampled without hashing file contents.
    pub index: crate::snapshot::IndexStamp,
    /// Case-sensitive baseline names replaced by differently cased directory entries.
    pub missing: BTreeSet<String>,
}

/// Reads existing trees and eligible paths without writing any Git object, ref, or index.
pub fn inventory(
    root: &Path,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CheckpointInventory> {
    let repository = gix::open(root)?;
    let identity = crate::discover_identity(root)?
        .ok_or_else(|| anyhow::anyhow!("missing repository identity"))?;
    let info = crate::content::conversion::read_info(&identity, check)?;
    let head = repository.head()?.id().map(|id| id.to_string());
    let tree = repository.head_tree_id_or_empty()?;
    let mut base = BTreeMap::new();
    let (pipeline, _) = repository.filter_pipeline(None)?;
    let (_, mut attributes) = pipeline.into_parts();
    if head.is_some() {
        for entry in repository
            .find_tree(tree.detach())?
            .traverse()
            .breadthfirst
            .files()?
        {
            check()?;
            if entry.mode.is_tree() {
                continue;
            }
            let path = path_key(entry.filepath.as_ref());
            base.insert(
                path.clone(),
                BaseFile {
                    object_id: entry.oid.to_string(),
                    mode: entry.mode.value() as u32,
                    checkout: checkout_rule(
                        &repository,
                        &identity,
                        &mut attributes,
                        info.as_deref(),
                        &path,
                        check,
                    )?,
                },
            );
        }
    }
    let index_stamp = crate::snapshot::read_index_stamp(&repository.index_path(), check)?;
    let index = repository.index_or_empty()?;
    let mut candidate: BTreeMap<_, _> = base.keys().map(|path| (path.clone(), false)).collect();
    let options = repository.stat_options()?;
    for entry in index.entries() {
        check()?;
        let path = path_key(entry.path(&index));
        if entry.mode.bits() == 0o040000
            && entry
                .flags
                .contains(gix::index::entry::Flags::SKIP_WORKTREE)
        {
            let prefix = format!("{}/", path.trim_end_matches('/'));
            for (name, clean) in &mut candidate {
                if name.starts_with(&prefix) {
                    *clean = true;
                }
            }
            continue;
        }
        let native = root.join(native_path(&path)?);
        let skipped = entry
            .flags
            .contains(gix::index::entry::Flags::SKIP_WORKTREE)
            && std::fs::symlink_metadata(&native)
                .is_err_and(|error| error.kind() == std::io::ErrorKind::NotFound);
        let clean = if let Some(base) = base.get_mut(&path) {
            let matched = entry.id.to_string() == base.object_id
                && entry.mode.bits() == base.mode
                && !entry.stat.is_racy(index.timestamp(), options)
                && gix::index::fs::Metadata::from_path_no_follow(&native)
                    .ok()
                    .and_then(|stat| gix::index::entry::Stat::from_fs(&stat).ok())
                    .is_some_and(|stat| entry.stat.matches(&stat, options));
            if skipped {
                true
            } else if matched
                && base.checkout.unsupported.is_none()
                && !(cfg!(windows) && base.mode == 0o120000)
            {
                let blob_size = repository.find_header(entry.id)?.size();
                let physical_size = u64::from(entry.stat.size);
                if physical_size >= blob_size && physical_size <= blob_size.saturating_mul(2) {
                    // Index size records the checked-out representation even after eol configuration changes.
                    base.checkout = CheckoutRule {
                        crlf: physical_size > blob_size,
                        automatic: false,
                        unsupported: None,
                    };
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };
        candidate.insert(path, clean);
    }
    let mut present = BTreeSet::new();
    let mut walk = repository.dirwalk_iter(
        index,
        Vec::<gix::bstr::BString>::new(),
        Default::default(),
        repository
            .dirwalk_options()?
            .emit_untracked(gix::dir::walk::EmissionMode::Matching)
            .emit_tracked(true),
    )?;
    for item in walk.by_ref() {
        check()?;
        let item = item?;
        if matches!(
            item.entry.status,
            gix::dir::entry::Status::Untracked | gix::dir::entry::Status::Tracked
        ) && item.entry.disk_kind != Some(gix::dir::entry::Kind::Directory)
        {
            let path = path_key(item.entry.rela_path.as_ref());
            present.insert(path.clone());
            candidate.entry(path).or_insert(false);
        }
    }
    ensure!(
        walk.into_outcome().is_some(),
        "checkpoint directory enumeration did not complete"
    );
    #[allow(unused_mut)]
    let mut missing = BTreeSet::new();
    #[cfg(windows)]
    {
        let spelling = present
            .iter()
            .map(|path| (path.to_lowercase(), path))
            .collect::<BTreeMap<_, _>>();
        for path in base.keys() {
            if spelling
                .get(&path.to_lowercase())
                .is_some_and(|current| *current != path)
            {
                missing.insert(path.clone());
                candidate.insert(path.clone(), false);
            }
        }
    }
    let mut observation = BTreeMap::new();
    for path in candidate.keys() {
        check()?;
        observation.insert(
            path.clone(),
            crate::snapshot::read_worktree_stamp(
                root,
                &crate::RepositoryPath::new(path_bytes(path)?)?,
            )?,
        );
    }
    ensure!(
        index_stamp == crate::snapshot::read_index_stamp(&repository.index_path(), check)?,
        "index changed during checkpoint discovery"
    );
    Ok(CheckpointInventory {
        head: head.clone(),
        tree: head.map(|_| tree.to_string()),
        base,
        candidate,
        observation,
        index: index_stamp,
        missing,
    })
}

/// Reads a Git tree without consulting the current index or worktree.
pub fn tree_files(root: &Path, tree: Option<&str>) -> Result<BTreeMap<String, BaseFile>> {
    let Some(tree) = tree else {
        return Ok(BTreeMap::new());
    };
    let repository = gix::open(root)?;
    let tree = repository.find_tree(gix::ObjectId::from_hex(tree.as_bytes())?)?;
    let mut files = BTreeMap::new();
    for entry in tree.traverse().breadthfirst.files()? {
        if entry.mode.is_tree() {
            continue;
        }
        files.insert(
            path_key(entry.filepath.as_ref()),
            BaseFile {
                object_id: entry.oid.to_string(),
                mode: entry.mode.value() as u32,
                checkout: CheckoutRule::default(),
            },
        );
    }
    Ok(files)
}

/// Resolves an existing blob with the captured conversion, without invoking external filters.
pub fn read_blob(root: &Path, file: &BaseFile, limit: usize) -> Result<Option<Vec<u8>>> {
    ensure!(
        file.mode != 0o160000,
        "submodule checkpoint restoration is unavailable"
    );
    ensure!(
        file.checkout.unsupported.is_none(),
        "checkpoint conversion unavailable: {}",
        file.checkout.unsupported.as_deref().unwrap_or_default()
    );
    let repository = gix::open(root)?;
    let id = gix::ObjectId::from_hex(file.object_id.as_bytes())?;
    if repository.find_header(id)?.size() > limit as u64 {
        return Ok(None);
    }
    let mut blob = repository.find_blob(id)?;
    ensure!(
        gix::objs::compute_hash(id.kind(), gix::objs::Kind::Blob, &blob.data)? == id,
        "Git baseline object digest mismatch: {id}"
    );
    let mut converted = Vec::new();
    if file.checkout.crlf && file.mode != 0o120000 {
        use gix::filter::plumbing::eol::{self, AttributesDigest, Configuration};
        let digest = if file.checkout.automatic {
            AttributesDigest::TextAutoCrlf
        } else {
            AttributesDigest::TextCrlf
        };
        if eol::convert_to_worktree(&blob.data, digest, &mut converted, Configuration::default())? {
            if converted.len() > limit {
                return Ok(None);
            }
            return Ok(Some(converted));
        }
    }
    Ok(Some(std::mem::take(&mut blob.data)))
}

/// Copies an immutable Git source with bounded memory, including large packed objects.
pub fn write_blob(root: &Path, file: &BaseFile, output: &mut impl Write) -> Result<()> {
    let repository = gix::open(root)?;
    let object = gix::ObjectId::from_hex(file.object_id.as_bytes())?;
    let size = repository.find_header(object)?.size();
    if size <= 8 * 1024 * 1024 {
        output.write_all(
            &read_blob(root, file, usize::MAX)?
                .ok_or_else(|| anyhow::anyhow!("missing baseline"))?,
        )?;
        return Ok(());
    }
    ensure!(
        file.mode != 0o160000 && file.checkout.unsupported.is_none(),
        "checkpoint conversion unavailable"
    );
    let mut spool = tempfile::tempfile()?;
    let mut command = std::process::Command::new("git");
    command
        .current_dir(root)
        .args(["cat-file", "blob", &file.object_id]);
    let result = crate::command::file_command(
        &mut command,
        spool.try_clone()?,
        crate::command::CommandLimits {
            stdout_bytes: 0,
            stderr_bytes: 64 * 1024,
            timeout: std::time::Duration::from_secs(30),
        },
        || Ok(()),
    )?;
    ensure!(
        result.status.success(),
        "read Git baseline failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    ensure!(spool.metadata()?.len() == size, "Git baseline size changed");
    spool.rewind()?;
    let mut digest = gix::hash::hasher(object.kind());
    digest.update(format!("blob {size}\0").as_bytes());
    let mut stats = gix::filter::plumbing::eol::Stats::default();
    let mut previous = None;
    let mut buffer = [0; 64 * 1024];
    loop {
        let count = spool.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        digest.update(&buffer[..count]);
        if !file.checkout.crlf {
            continue;
        }
        for byte in &buffer[..count] {
            if previous == Some(b'\r') {
                if *byte == b'\n' {
                    stats.crlf += 1;
                    previous = Some(*byte);
                    continue;
                }
                stats.lone_cr += 1;
            }
            match *byte {
                b'\r' => {}
                b'\n' => stats.lone_lf += 1,
                0 => {
                    stats.null += 1;
                    stats.non_printable += 1;
                }
                8 | 9 | 12 | 27 => stats.printable += 1,
                1..=31 | 127 => stats.non_printable += 1,
                _ => stats.printable += 1,
            }
            previous = Some(*byte);
        }
    }
    if previous == Some(b'\r') {
        stats.lone_cr += 1;
    }
    if previous == Some(0x1a) {
        stats.non_printable = stats.non_printable.saturating_sub(1);
    }
    ensure!(
        digest.try_finalize()? == object,
        "Git baseline object digest mismatch: {object}"
    );
    spool.rewind()?;
    use gix::filter::plumbing::eol::{AttributesDigest, Configuration};
    let digest = if file.checkout.automatic {
        AttributesDigest::TextAutoCrlf
    } else {
        AttributesDigest::TextCrlf
    };
    let convert =
        file.checkout.crlf && stats.will_convert_lf_to_crlf(digest, Configuration::default());
    previous = None;
    loop {
        let count = spool.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        if !convert {
            output.write_all(&buffer[..count])?;
            continue;
        }
        let mut start = 0;
        for position in 0..count {
            let byte = buffer[position];
            if byte == b'\n' && previous != Some(b'\r') {
                output.write_all(&buffer[start..position])?;
                output.write_all(b"\r")?;
                start = position;
            }
            previous = Some(byte);
        }
        output.write_all(&buffer[start..count])?;
    }
    Ok(())
}

fn checkout_rule(
    repository: &gix::Repository,
    identity: &crate::RepositoryIdentity,
    attributes: &mut gix::worktree::Stack,
    info: Option<&[u8]>,
    path: &str,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CheckoutRule> {
    use gix::attrs::State;
    use gix::filter::plumbing::eol::{AttributesDigest as Digest, AutoCrlf, Configuration, Mode};
    let selection = crate::content::conversion::resolve_attributes(
        repository,
        identity,
        &crate::RepositoryPath::new(path_bytes(path)?)?,
        attributes,
        info,
        check,
    )?;
    let state = selection.attribute;
    let extract = |state: &State| match state {
        State::Set => Some(Digest::Text),
        State::Unset => Some(Digest::Binary),
        State::Value(value) if value.as_ref().as_bstr() == b"auto".as_bstr() => {
            Some(Digest::TextAuto)
        }
        State::Value(value) if value.as_ref().as_bstr() == b"input".as_bstr() => {
            Some(Digest::TextInput)
        }
        _ => None,
    };
    let config = repository.config_snapshot();
    let auto = config
        .string("core.autocrlf")
        .map(|value| value.to_ascii_lowercase())
        .unwrap_or_default();
    let auto_crlf = match auto.as_slice() {
        b"true" => AutoCrlf::Enabled,
        b"input" => AutoCrlf::Input,
        _ => AutoCrlf::Disabled,
    };
    let eol = config
        .string("core.eol")
        .and_then(|value| match value.as_bstr() {
            value if value == b"lf".as_bstr() => Some(Mode::Lf),
            value if value == b"crlf".as_bstr() => Some(Mode::CrLf),
            _ => None,
        });
    let configuration = Configuration { auto_crlf, eol };
    let mut digest = extract(&state[4]).or_else(|| extract(&state[0]));
    if digest != Some(Digest::Binary)
        && let State::Value(value) = &state[3]
    {
        digest = match value.as_ref().as_bstr() {
            value if value == b"crlf".as_bstr() => Some(if digest == Some(Digest::TextAuto) {
                Digest::TextAutoCrlf
            } else {
                Digest::TextCrlf
            }),
            value if value == b"lf".as_bstr() => Some(if digest == Some(Digest::TextAuto) {
                Digest::TextAutoInput
            } else {
                Digest::TextInput
            }),
            _ => digest,
        };
    }
    let digest = digest.unwrap_or_else(|| auto_crlf.into());
    let mut rule = CheckoutRule {
        crlf: digest.to_eol(configuration) == Some(Mode::CrLf),
        automatic: digest.is_auto_text(),
        unsupported: None,
    };
    if matches!(state[2], State::Value(_)) {
        rule.unsupported = Some("external content filter".into());
    }
    if matches!(state[5], State::Value(_)) {
        rule.unsupported = Some("working-tree encoding".into());
    }
    if matches!(state[1], State::Set) {
        rule.unsupported = Some("ident expansion".into());
    }
    if !rule.crlf {
        rule.automatic = false;
    }
    Ok(rule)
}

/// Finds one baseline entry without traversing unrelated directories.
pub fn tree_file(root: &Path, tree: Option<&str>, path: &str) -> Result<Option<BaseFile>> {
    let Some(tree) = tree else { return Ok(None) };
    let repository = gix::open(root)?;
    let tree = repository.find_tree(gix::ObjectId::from_hex(tree.as_bytes())?)?;
    Ok(tree
        .lookup_entry_by_path(native_path(path)?)?
        .map(|entry| BaseFile {
            object_id: entry.object_id().to_string(),
            mode: entry.mode().value() as u32,
            checkout: CheckoutRule::default(),
        }))
}

/// Reads only the current commit identity, without scanning the working tree.
pub fn head(root: &Path) -> Result<Option<String>> {
    Ok(gix::open(root)?.head()?.id().map(|id| id.to_string()))
}

/// Encodes non-UTF-8 Git paths without losing identity or colliding with valid file names.
pub fn path_key(raw: &[u8]) -> String {
    match std::str::from_utf8(raw) {
        Ok(path) if !path.starts_with(":raw:") => path.to_owned(),
        _ => {
            let mut encoded = String::from(":raw:");
            for byte in raw {
                use std::fmt::Write;
                write!(&mut encoded, "{byte:02x}").unwrap();
            }
            encoded
        }
    }
}

/// Returns the exact bytes of a checkpoint path key.
pub fn path_bytes(key: &str) -> Result<Vec<u8>> {
    let Some(encoded) = key.strip_prefix(":raw:") else {
        return Ok(key.as_bytes().to_vec());
    };
    ensure!(encoded.len() % 2 == 0, "invalid encoded checkpoint path");
    encoded
        .as_bytes()
        .chunks_exact(2)
        .map(|pair| Ok(u8::from_str_radix(std::str::from_utf8(pair)?, 16)?))
        .collect()
}

/// Resolves a persisted path key without a lossy Unicode conversion.
pub fn native_path(key: &str) -> Result<std::ffi::OsString> {
    crate::resolve_argument(&crate::RepositoryPath::new(path_bytes(key)?)?)
}
