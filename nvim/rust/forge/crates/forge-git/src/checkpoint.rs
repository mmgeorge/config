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

/// Supplies changed paths from Git status and baseline metadata without scanning clean files.
#[derive(Debug, Eq, PartialEq)]
pub struct CheckpointInventory {
    /// Existing commit, or no commit in an unborn repository.
    pub head: Option<String>,
    /// Existing tree, or no tree in an unborn repository.
    pub tree: Option<String>,
    /// Existing committed files. Trees are read, file blobs are not.
    pub base: BTreeMap<String, BaseFile>,
    /// Changed paths and non-LFS conversions requiring raw capture.
    pub candidate: BTreeSet<String>,
    /// Paths outside checkpoint ownership because their filter is Git LFS.
    pub excluded: BTreeSet<String>,
    /// Baseline spellings replaced by case-only renames on Windows.
    pub missing: BTreeSet<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Identifies a worktree using native Git status and metadata for its changed paths.
pub struct CheckpointSignature {
    status: Vec<u8>,
    index: Option<(u64, std::time::SystemTime)>,
    observation: Vec<crate::snapshot::WorktreeStamp>,
    paths: Vec<crate::snapshot::PathRecord>,
    names: Vec<u8>,
    attributes: Vec<crate::snapshot::WorktreeStamp>,
    config: Vec<u8>,
    checkout: BTreeMap<String, [gix::attrs::State; 6]>,
    checkout_size: BTreeMap<String, u32>,
}

/// Checks checkpoint reuse without enumerating committed trees or creating checkpoint objects.
///
/// Runs a bounded, read-only Git status under the caller's repository admission. HEAD,
/// index identity, untracked paths, and metadata of already-dirty files all participate.
pub fn signature(
    root: &Path,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CheckpointSignature> {
    let started = std::time::Instant::now();
    let repository = gix::open(root)?;
    let index = match std::fs::metadata(repository.index_path()) {
        Ok(metadata) => Some((metadata.len(), metadata.modified()?)),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
        Err(error) => return Err(error.into()),
    };
    let temporary = tempfile::tempdir()?;
    let temporary_index = temporary.path().join("index");
    if index.is_some() {
        std::fs::copy(repository.index_path(), &temporary_index)?;
    }
    let mut status = std::process::Command::new("git");
    #[cfg(windows)]
    status.args(["-c", "core.ignoreCase=false"]);
    status
        .current_dir(root)
        .env("GIT_INDEX_FILE", &temporary_index)
        .args([
            "-c",
            "core.splitIndex=false",
            "status",
            "--porcelain=v2",
            "-z",
            "--branch",
            "--untracked-files=all",
            "--ignore-submodules=none",
            "--no-renames",
            "--",
            ".",
            ":(exclude,attr:filter=lfs)",
        ]);
    let mut names = std::process::Command::new("git");
    #[cfg(windows)]
    names.args(["-c", "core.ignoreCase=false"]);
    names.current_dir(root).args([
        "ls-files",
        "--cached",
        "--others",
        "--exclude-standard",
        "-z",
    ]);
    let (output, names) = crate::command::read_command_pair(
        status,
        names,
        crate::command::CommandLimits {
            stderr_bytes: 64 * 1024,
            timeout: std::time::Duration::from_secs(20),
        },
        &mut *check,
    )?;
    ensure!(
        output.status.success(),
        "checkpoint status failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    ensure!(
        names.status.success(),
        "checkpoint path inventory failed: {}",
        String::from_utf8_lossy(&names.stderr)
    );
    let paths = crate::snapshot::parse_porcelain_v2(&output.stdout, repository.object_hash())?;
    let mut observation = Vec::with_capacity(paths.len());
    for path in &paths {
        check()?;
        observation.push(crate::snapshot::read_worktree_stamp(root, &path.path)?);
    }
    let mut attributes = Vec::new();
    for name in names
        .stdout
        .split(|byte| *byte == 0)
        .filter(|name| *name == b".gitattributes" || name.ends_with(b"/.gitattributes"))
    {
        attributes.push(crate::snapshot::read_worktree_stamp(
            root,
            &crate::RepositoryPath::new(name.to_vec())?,
        )?);
    }
    let mut config = Vec::new();
    repository.config_snapshot().write_to(&mut config)?;
    let identity = crate::discover_identity(root)?
        .ok_or_else(|| anyhow::anyhow!("checkpoint repository disappeared"))?;
    config.extend(crate::content::conversion::read_info(&identity, check)?.unwrap_or_default());
    let command_elapsed = started.elapsed();
    let checkout = checkout_attributes(
        root,
        &names
            .stdout
            .split(|byte| *byte == 0)
            .filter(|path| !path.is_empty())
            .map(path_key)
            .collect(),
        check,
    )?;
    let refreshed = gix::index::File::at_or_default(
        &temporary_index,
        repository.object_hash(),
        false,
        Default::default(),
    )?;
    let checkout_size = refreshed
        .entries()
        .iter()
        .map(|entry| (path_key(entry.path(&refreshed)), entry.stat.size))
        .collect();
    let _ = writeln!(
        std::io::stderr(),
        "checkpoint.signature commands_ms={} attributes_ms={} names={} attributes={}",
        command_elapsed.as_millis(),
        (started.elapsed() - command_elapsed).as_millis(),
        names.stdout.len(),
        checkout.len()
    );
    Ok(CheckpointSignature {
        status: output.stdout,
        index,
        observation,
        paths,
        names: names.stdout,
        attributes,
        config,
        checkout,
        checkout_size,
    })
}

/// Reads existing trees and eligible paths without writing any Git object, ref, or index.
pub fn inventory(
    root: &Path,
    signature: &CheckpointSignature,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<CheckpointInventory> {
    let repository = gix::open(root)?;
    let head = repository.head()?.id().map(|id| id.to_string());
    let tree = repository.head_tree_id_or_empty()?;
    let tree_id = head.as_ref().map(|_| tree.to_string());
    let mut base = tree_files(root, tree_id.as_deref())?;
    let mut candidate = signature
        .paths
        .iter()
        .map(|entry| path_key(entry.path.raw()))
        .collect::<BTreeSet<_>>();
    let paths = base
        .keys()
        .chain(candidate.iter())
        .cloned()
        .chain(
            signature
                .names
                .split(|byte| *byte == 0)
                .filter(|path| !path.is_empty())
                .map(path_key),
        )
        .collect::<BTreeSet<_>>();
    let listed = signature
        .names
        .split(|byte| *byte == 0)
        .filter(|path| !path.is_empty())
        .map(path_key)
        .collect::<BTreeSet<_>>();
    let missing = paths.difference(&listed).cloned().collect::<BTreeSet<_>>();
    let additional = if missing.is_empty() {
        BTreeMap::new()
    } else {
        checkout_attributes(root, &missing, check)?
    };
    let empty = std::array::from_fn(|_| gix::attrs::State::Unspecified);
    let mut excluded = BTreeSet::new();
    #[allow(unused_mut)]
    let mut missing = BTreeSet::new();
    let auto_crlf = repository
        .config_snapshot()
        .string("core.autocrlf")
        .is_some_and(|value| {
            value.eq_ignore_ascii_case(b"true") || value.eq_ignore_ascii_case(b"input")
        });
    for path in &paths {
        check()?;
        let state = signature
            .checkout
            .get(path)
            .or_else(|| additional.get(path))
            .unwrap_or(&empty);
        let Some(mut rule) = checkout_rule(&repository, state) else {
            excluded.insert(path.clone());
            candidate.remove(path);
            base.remove(path);
            continue;
        };
        if let Some(file) = base.get_mut(path) {
            if rule.unsupported.is_some() || (cfg!(windows) && file.mode == 0o120000) {
                candidate.insert(path.clone());
            } else if !candidate.contains(path)
                && file.mode != 0o120000
                && file.mode != 0o160000
                && (auto_crlf
                    || [0, 3, 4].into_iter().any(|index| {
                        !matches!(
                            state[index],
                            gix::attrs::State::Unspecified | gix::attrs::State::Unset
                        )
                    }))
            {
                if let Some(size) = signature.checkout_size.get(path) {
                    let physical_size = u64::from(*size);
                    let blob_size = repository
                        .find_header(gix::ObjectId::from_hex(file.object_id.as_bytes())?)?
                        .size();
                    if physical_size >= blob_size && physical_size <= blob_size.saturating_mul(2) {
                        rule.crlf = physical_size > blob_size;
                        rule.automatic = false;
                    } else {
                        candidate.insert(path.clone());
                    }
                }
            }
            file.checkout = rule;
        }
    }
    #[cfg(windows)]
    {
        let spelling = base
            .keys()
            .map(|path| (path.to_lowercase(), path))
            .collect::<BTreeMap<_, _>>();
        for path in &candidate {
            if let Some(original) = spelling.get(&path.to_lowercase())
                && *original != path
            {
                missing.insert((*original).clone());
            }
        }
        for path in &missing {
            candidate.remove(path);
        }
    }
    Ok(CheckpointInventory {
        head,
        tree: tree_id,
        base,
        candidate,
        excluded,
        missing,
    })
}

/// Resolve LFS exclusions from attributes without opening asset contents or invoking filters.
pub fn lfs_paths(root: &Path, paths: &BTreeSet<String>) -> Result<BTreeSet<String>> {
    if crate::discover_identity(root)?.is_none() {
        return Ok(BTreeSet::new());
    }
    Ok(checkout_attributes(root, paths, &mut || Ok(()))?
        .into_iter()
        .filter_map(|(path, state)| {
            matches!(&state[2], gix::attrs::State::Value(value)
            if value.as_ref().as_bstr() == b"lfs".as_bstr())
            .then_some(path)
        })
        .collect())
}

fn checkout_attributes(
    root: &Path,
    paths: &BTreeSet<String>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<BTreeMap<String, [gix::attrs::State; 6]>> {
    let identity = crate::discover_identity(root)?
        .ok_or_else(|| anyhow::anyhow!("checkpoint repository disappeared"))?;
    let repository = gix::open(root)?;
    let info = crate::content::conversion::read_info(&identity, check)?;
    let (pipeline, _) = repository.filter_pipeline(None)?;
    let (_, mut attributes) = pipeline.into_parts();
    let mut result = BTreeMap::new();
    for path in paths {
        let state = crate::content::conversion::resolve_attribute_states(
            &repository,
            &identity,
            &crate::RepositoryPath::new(path_bytes(path)?)?,
            &mut attributes,
            info.as_deref(),
            check,
        )?;
        if state
            .iter()
            .any(|state| *state != gix::attrs::State::Unspecified)
        {
            result.insert(
                path.clone(),
                state
                    .try_into()
                    .map_err(|_| anyhow::anyhow!("checkpoint attribute selection changed shape"))?,
            );
        }
    }
    Ok(result)
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
pub fn read_blob(root: &Path, file: &BaseFile) -> Result<Vec<u8>> {
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
            return Ok(converted);
        }
    }
    Ok(std::mem::take(&mut blob.data))
}

/// Copies an immutable Git source with bounded memory, including large packed objects.
pub fn write_blob(root: &Path, file: &BaseFile, output: &mut impl Write) -> Result<()> {
    let repository = gix::open(root)?;
    let object = gix::ObjectId::from_hex(file.object_id.as_bytes())?;
    let size = repository.find_header(object)?.size();
    if size <= 8 * 1024 * 1024 {
        output.write_all(&read_blob(root, file)?)?;
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
    state: &[gix::attrs::State; 6],
) -> Option<CheckoutRule> {
    use gix::attrs::State;
    use gix::filter::plumbing::eol::{AttributesDigest as Digest, AutoCrlf, Configuration, Mode};
    if matches!(&state[2], State::Value(value) if value.as_ref().as_bstr() == b"lfs".as_bstr()) {
        return None;
    }
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
    Some(rule)
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
