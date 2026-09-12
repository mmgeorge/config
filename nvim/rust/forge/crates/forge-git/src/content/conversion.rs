use std::{
    io::{self, Write},
    path::PathBuf,
    sync::Arc,
};

use anyhow::{Context, Result, ensure};
use forge_diff::source::MAX_SOURCE_BYTES;
use gix::bstr::ByteSlice;
use gix::{
    attrs::{State, StateRef},
    filter::plumbing::pipeline::convert::ToGitOutcome,
};

use super::ContentUnavailable;
use crate::{
    RepositoryIdentity, RepositoryPath, resolve_argument,
    snapshot::{IndexStamp, read_index_stamp},
};

const ATTRIBUTE_NAMES: [&str; 6] = [
    "crlf",
    "ident",
    "filter",
    "eol",
    "text",
    "working-tree-encoding",
];
const FILTER_ATTRIBUTE: usize = 2;
const ENCODING_ATTRIBUTE: usize = 5;
const MAX_CONFIGURATION_BYTES: usize = 1024 * 1024;
const MAX_ATTRIBUTE_BYTES: usize = 64 * 1024;

/// Retains sampled conversion inputs for later verification of cached canonical content.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConversionIdentity {
    pub configuration: gix::ObjectId,
    pub attributes: gix::ObjectId,
    pub index: IndexStamp,
}

pub(super) enum ConversionResult {
    Ready {
        bytes: Vec<u8>,
        identity: Arc<ConversionIdentity>,
    },
    Unavailable(ContentUnavailable),
    TooLarge {
        capacity: usize,
    },
}

struct DigestWriter {
    hasher: gix::hash::Hasher,
    remaining: usize,
}

pub(super) struct ConversionSession {
    configuration: gix::ObjectId,
    index_stamp: IndexStamp,
    index: gix::worktree::IndexPersistedOrInMemory,
    pipeline: gix::filter::plumbing::Pipeline,
    cache: gix::worktree::Stack,
    info: Option<Vec<u8>>,
    used: Vec<(RepositoryPath, gix::ObjectId)>,
}

impl ConversionSession {
    pub(super) fn new(
        local: &mut gix::Repository,
        repository: &RepositoryIdentity,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<Self> {
        reload(local, repository, check)?;
        let configuration = configuration_digest(local)?;
        let index_stamp = read_index_stamp(&repository.index, check)?;
        let (wrapper, index) = local
            .filter_pipeline(None)
            .context("prepare canonical conversion")?;
        let (mut pipeline, cache) = wrapper.into_parts();
        pipeline.options_mut().drivers.clear();
        Ok(Self {
            configuration,
            index_stamp,
            index,
            pipeline,
            cache,
            info: read_info(repository, check)?,
            used: Vec::new(),
        })
    }

    pub(super) fn verify(
        &self,
        local: &mut gix::Repository,
        repository: &RepositoryIdentity,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<()> {
        let mut verified = Self::new(local, repository, check)?;
        ensure!(
            self.configuration == verified.configuration,
            "configuration changed during canonical conversion"
        );
        ensure!(
            self.index_stamp == verified.index_stamp,
            "index changed during canonical conversion"
        );
        for (path, expected) in &self.used {
            check()?;
            let selection = resolve_attributes(
                local,
                repository,
                path,
                &mut verified.cache,
                verified.info.as_deref(),
                check,
            )?;
            ensure!(
                *expected == attribute_digest(&selection.attribute)?,
                "attributes changed during canonical conversion"
            );
        }
        check()
    }
}

pub(super) fn convert(
    local: &mut gix::Repository,
    repository: &RepositoryIdentity,
    path: &RepositoryPath,
    bytes: Vec<u8>,
    limit: usize,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<ConversionResult> {
    let mut session = ConversionSession::new(local, repository, check)?;
    let result = session.convert(local, repository, path, bytes, limit, check)?;
    session.verify(local, repository, check)?;
    Ok(result)
}

impl ConversionSession {
    pub(super) fn convert(
        &mut self,
        local: &gix::Repository,
        repository: &RepositoryIdentity,
        path: &RepositoryPath,
        bytes: Vec<u8>,
        limit: usize,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<ConversionResult> {
        let relative = PathBuf::from(resolve_argument(path)?);
        let selection = resolve_attributes(
            local,
            repository,
            path,
            &mut self.cache,
            self.info.as_deref(),
            check,
        )?;
        let attribute = &selection.attribute;
        let attributes = attribute_digest(attribute)?;
        self.used.push((path.clone(), attributes));
        if let State::Value(name) = &attribute[FILTER_ATTRIBUTE]
            && has_external_filter(local, name.as_ref().as_bstr())?
        {
            return Ok(ConversionResult::Unavailable(
                ContentUnavailable::ExternalFilter,
            ));
        }
        if let State::Value(name) = &attribute[ENCODING_ATTRIBUTE] {
            let label = name.as_ref().as_bstr();
            let supported = [b"UTF-8".as_slice(), b"UTF-16LE", b"UTF-16BE"]
                .iter()
                .any(|supported| label.eq_ignore_ascii_case(supported));
            if !supported {
                return Ok(ConversionResult::Unavailable(
                    ContentUnavailable::UnsupportedEncoding,
                ));
            }
            if !label.eq_ignore_ascii_case(b"UTF-8") {
                ensure!(
                    !bytes.starts_with(&[0xff, 0xfe]) && !bytes.starts_with(&[0xfe, 0xff]),
                    "explicit UTF-16 endianness forbids a byte-order mark"
                );
                let encoding = gix::filter::plumbing::worktree::encoding::for_label(label)?;
                let capacity = encoding
                    .new_decoder_without_bom_handling()
                    .max_utf8_buffer_length(bytes.len())
                    .context("conversion capacity overflow")?;
                if capacity > limit {
                    return Ok(ConversionResult::TooLarge { capacity });
                }
            }
        }
        check()?;
        let converted = {
            let outcome = self.pipeline.convert_to_git(
                bytes.as_slice(),
                &relative,
                &mut |_, selected| {
                    // Each resolved selection owns different assignment maps, even at the same size.
                    *selected = selection.outcome.clone();
                },
                &mut |buffer| {
                    let result: Result<Option<()>> = (|| {
                        check()?;
                        let Some(index_entry) = self.index.entry_by_path(path.raw().as_bstr())
                        else {
                            return Ok(None);
                        };
                        if index_entry.stage() != gix::index::entry::Stage::Unconflicted {
                            return Ok(None);
                        }
                        let header = local.find_header(index_entry.id)?;
                        if header.kind() != gix::objs::Kind::Blob {
                            return Ok(None);
                        }
                        ensure!(
                            header.size() <= MAX_SOURCE_BYTES as u64,
                            "index source exceeds canonical conversion byte limit"
                        );
                        let object = local.find_object(index_entry.id)?;
                        ensure!(
                            object.data.len() as u64 == header.size()
                                && object.data.capacity() <= MAX_SOURCE_BYTES,
                            "index conversion source exceeds admitted capacity"
                        );
                        ensure!(
                            gix::objs::compute_hash(
                                index_entry.id.kind(),
                                object.kind,
                                &object.data
                            )? == index_entry.id,
                            "index conversion source checksum mismatch"
                        );
                        *buffer = object.detach().data;
                        Ok(Some(()))
                    })();
                    result.map_err(anyhow::Error::into_boxed_dyn_error)
                },
            )?;
            match outcome {
                ToGitOutcome::Unchanged(_) => None,
                ToGitOutcome::Buffer(converted) => {
                    ensure!(
                        converted.len() <= limit,
                        "canonical conversion exceeded admitted output size"
                    );
                    Some(converted.to_vec())
                }
                ToGitOutcome::Process(_) => {
                    anyhow::bail!("canonical conversion unexpectedly selected an external process")
                }
            }
        };
        let bytes = converted.unwrap_or(bytes);
        let identity = ConversionIdentity {
            configuration: self.configuration,
            attributes,
            index: self.index_stamp.clone(),
        };
        Ok(ConversionResult::Ready {
            bytes,
            identity: Arc::new(identity),
        })
    }
}

struct AttributeSelection {
    outcome: gix::attrs::search::Outcome,
    attribute: Vec<State>,
}

fn resolve_attributes(
    local: &gix::Repository,
    repository: &RepositoryIdentity,
    path: &RepositoryPath,
    cache: &mut gix::worktree::Stack,
    info: Option<&[u8]>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<AttributeSelection> {
    let relative = PathBuf::from(resolve_argument(path)?);
    let mut selected = gix::attrs::search::Outcome::default();
    selected.initialize_with_selection(&Default::default(), ATTRIBUTE_NAMES);
    cache
        .at_path(relative.as_path(), None, &local.objects)?
        .matching_attributes(&mut selected);
    let mut attribute = selected
        .iter_selected()
        .map(|item| item.assignment.state.to_owned())
        .collect::<Vec<_>>();
    // gix-worktree 0.56 places info attributes below nested worktree rules.
    // Reapply this highest-priority source with the collected macro definitions.
    let info_path = repository.common_directory.join("info/attributes");
    if let Some(bytes) = info {
        let mut collection = cache.attributes_collection().clone();
        let mut overrides = gix::attrs::Search::default();
        overrides.add_patterns_buffer(bytes, info_path, None, &mut collection, true);
        selected.initialize_with_selection(&collection, ATTRIBUTE_NAMES);
        let case = if local
            .config_snapshot()
            .boolean("core.ignoreCase")
            .unwrap_or(false)
        {
            gix::attrs::glob::pattern::Case::Fold
        } else {
            gix::attrs::glob::pattern::Case::Sensitive
        };
        overrides.pattern_matching_relative_path(
            path.raw().as_bstr(),
            case,
            Some(false),
            &mut selected,
        );
        for (position, item) in selected.iter_selected().enumerate() {
            if item.location.source.is_some() {
                attribute[position] = item.assignment.state.to_owned();
            }
        }
    }
    attribute_digest(&attribute)?;
    let mut serialized = b"*".to_vec();
    for (name, state) in ATTRIBUTE_NAMES.iter().zip(&attribute) {
        serialized.push(b' ');
        match state {
            State::Unset => serialized.push(b'-'),
            State::Unspecified => serialized.push(b'!'),
            _ => {}
        }
        serialized.extend_from_slice(name.as_bytes());
        if let State::Value(value) = state {
            serialized.push(b'=');
            serialized.extend_from_slice(value.as_ref().as_bstr());
        }
        ensure!(
            serialized.len() <= MAX_ATTRIBUTE_BYTES,
            "resolved attributes exceed conversion limit"
        );
    }
    serialized.push(b'\n');
    let mut collection = gix::attrs::search::MetadataCollection::default();
    let mut resolved = gix::attrs::Search::default();
    resolved.add_patterns_buffer(
        &serialized,
        "<canonical-selection>".into(),
        None,
        &mut collection,
        false,
    );
    let mut selected = gix::attrs::search::Outcome::default();
    selected.initialize_with_selection(&collection, ATTRIBUTE_NAMES);
    resolved.pattern_matching_relative_path(
        b"file".as_bstr(),
        gix::attrs::glob::pattern::Case::Sensitive,
        Some(false),
        &mut selected,
    );
    ensure!(
        selected
            .iter_selected()
            .map(|item| item.assignment.state.to_owned())
            .eq(attribute.iter().cloned()),
        "canonical attribute serialization changed a selected value"
    );
    check()?;
    Ok(AttributeSelection {
        outcome: selected,
        attribute,
    })
}

pub(super) fn verify_context(
    local: &mut gix::Repository,
    repository: &RepositoryIdentity,
    path: &RepositoryPath,
    identity: &ConversionIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    reload(local, repository, check)?;
    ensure!(
        identity.configuration == configuration_digest(local)?,
        "configuration changed during canonical conversion"
    );
    ensure!(
        identity.index == read_index_stamp(&repository.index, check)?,
        "index changed during canonical conversion"
    );
    let (pipeline, _) = local.filter_pipeline(None)?;
    let (_, mut cache) = pipeline.into_parts();
    let info = read_info(repository, check)?;
    let selection =
        resolve_attributes(local, repository, path, &mut cache, info.as_deref(), check)?;
    let attribute = &selection.attribute;
    ensure!(
        identity.attributes == attribute_digest(attribute)?,
        "attributes changed during canonical conversion"
    );
    check()
}

fn read_info(
    repository: &RepositoryIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<Vec<u8>>> {
    match std::fs::File::open(repository.common_directory.join("info/attributes")) {
        Ok(mut file) => {
            let metadata = file.metadata()?;
            ensure!(
                metadata.is_file() && metadata.len() <= MAX_CONFIGURATION_BYTES as u64,
                "info attributes exceed conversion input limit"
            );
            Ok(Some(super::read_bounded(
                &mut file,
                metadata.len() as usize,
                MAX_CONFIGURATION_BYTES,
                check,
            )?))
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(error) => Err(error).context("read canonical info attributes"),
    }
}

fn reload(
    local: &mut gix::Repository,
    repository: &RepositoryIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    check()?;
    let options = local
        .open_options()
        .clone()
        .strict_config(true)
        .open_path_as_is(true);
    *local = gix::ThreadSafeRepository::open_opts(&repository.git_directory, options)
        .context("refresh canonical conversion configuration")?
        .to_thread_local();
    local.objects.ignore_replacements = true;
    ensure!(
        local.workdir().map(dunce::canonicalize).transpose()? == repository.worktree_root,
        "worktree location changed during canonical conversion"
    );
    ensure!(
        local.index_path() == repository.index,
        "index location changed during canonical conversion"
    );
    check()
}

fn configuration_digest(local: &gix::Repository) -> Result<gix::ObjectId> {
    let mut writer = DigestWriter {
        hasher: gix::hash::hasher(gix::hash::Kind::Sha256),
        remaining: MAX_CONFIGURATION_BYTES,
    };
    local.config_snapshot().plumbing().write_to(&mut writer)?;
    Ok(writer.hasher.try_finalize()?)
}

fn has_external_filter(local: &gix::Repository, name: &gix::bstr::BStr) -> Result<bool> {
    let snapshot = local.config_snapshot();
    for section in snapshot
        .plumbing()
        .sections_by_name("filter")
        .into_iter()
        .flatten()
    {
        if section.header().subsection_name() != Some(name) {
            continue;
        }
        let required = section
            .value("required")
            .map(|value| gix::config::Boolean::try_from(value.as_bstr()).map(bool::from))
            .transpose()?
            .unwrap_or(false);
        if section.value("clean").is_some() || section.value("process").is_some() || required {
            return Ok(true);
        }
    }
    Ok(false)
}

fn attribute_digest(attribute: &[State]) -> Result<gix::ObjectId> {
    ensure!(
        attribute.len() == ATTRIBUTE_NAMES.len(),
        "canonical attribute selection is incomplete"
    );
    let mut writer = DigestWriter {
        hasher: gix::hash::hasher(gix::hash::Kind::Sha256),
        remaining: MAX_ATTRIBUTE_BYTES,
    };
    for state in attribute {
        match state.as_ref() {
            StateRef::Set => writer.write_all(&[0])?,
            StateRef::Unset => writer.write_all(&[1])?,
            StateRef::Unspecified => writer.write_all(&[2])?,
            StateRef::Value(value) => {
                writer.write_all(&[3])?;
                writer.write_all(&(value.as_bstr().len() as u64).to_le_bytes())?;
                writer.write_all(value.as_bstr())?;
            }
        }
    }
    Ok(writer.hasher.try_finalize()?)
}

impl Write for DigestWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        if bytes.len() > self.remaining {
            return Err(io::Error::other(
                "canonical configuration or attribute byte limit exceeded",
            ));
        }
        self.remaining -= bytes.len();
        self.hasher.update(bytes);
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::git;

    #[test]
    fn batch_rejects_changed_inputs_after_multiple_conversions() {
        for change in ["configuration", "attributes", "index"] {
            let fixture = tempfile::tempdir().unwrap();
            let root = fixture.path();
            git(root, &["init", "--quiet"]);
            git(root, &["config", "core.autocrlf", "false"]);
            git(root, &["config", "core.safecrlf", "false"]);
            std::fs::write(root.join(".gitattributes"), "* text\n").unwrap();
            std::fs::write(root.join("first"), b"one\r\n").unwrap();
            std::fs::write(root.join("second"), b"two\r\n").unwrap();
            let (repository, shared) = crate::identity::discover_repository(root).unwrap().unwrap();
            let mut local = shared.to_thread_local();
            let mut session =
                ConversionSession::new(&mut local, &repository, &mut || Ok(())).unwrap();
            for name in ["first", "second"] {
                let path = RepositoryPath::new(name.as_bytes().to_vec()).unwrap();
                let result = session
                    .convert(
                        &local,
                        &repository,
                        &path,
                        b"one\r\n".to_vec(),
                        MAX_SOURCE_BYTES,
                        &mut || Ok(()),
                    )
                    .unwrap();
                let ConversionResult::Ready { bytes, .. } = result else {
                    panic!()
                };
                assert_eq!(bytes, b"one\n");
            }
            session
                .verify(&mut local, &repository, &mut || Ok(()))
                .unwrap();
            match change {
                "configuration" => {
                    git(root, &["config", "core.autocrlf", "true"]);
                }
                "attributes" => {
                    std::fs::write(root.join(".gitattributes"), "first -text\nsecond text\n")
                        .unwrap();
                }
                "index" => {
                    git(root, &["add", "first"]);
                }
                _ => unreachable!(),
            }
            let error = session
                .verify(&mut local, &repository, &mut || Ok(()))
                .unwrap_err();
            assert!(error.to_string().contains(change), "{error:#}");
        }
    }

    #[test]
    fn changed_configuration_attributes_and_index_reject_conversion_identity() {
        for change in ["configuration", "attributes", "index"] {
            let fixture = tempfile::tempdir().unwrap();
            let root = fixture.path();
            git(root, &["init", "--quiet"]);
            git(root, &["config", "core.autocrlf", "false"]);
            git(root, &["config", "core.safecrlf", "false"]);
            std::fs::write(root.join(".gitattributes"), "file text\n").unwrap();
            std::fs::write(root.join("file"), b"one\r\n").unwrap();
            let (repository, shared) = crate::identity::discover_repository(root).unwrap().unwrap();
            let mut local = shared.to_thread_local();
            let path = RepositoryPath::new(b"file".to_vec()).unwrap();
            let result = convert(
                &mut local,
                &repository,
                &path,
                b"one\r\n".to_vec(),
                MAX_SOURCE_BYTES,
                &mut || Ok(()),
            )
            .unwrap();
            let ConversionResult::Ready { identity, .. } = result else {
                panic!()
            };
            match change {
                "configuration" => {
                    git(root, &["config", "core.autocrlf", "true"]);
                }
                "attributes" => {
                    std::fs::write(root.join(".gitattributes"), "file -text\n").unwrap()
                }
                "index" => {
                    git(root, &["add", "file"]);
                }
                _ => unreachable!(),
            }
            let error = verify_context(&mut local, &repository, &path, &identity, &mut || Ok(()))
                .unwrap_err();
            assert!(error.to_string().contains(change), "{error:#}");
        }
    }
}
