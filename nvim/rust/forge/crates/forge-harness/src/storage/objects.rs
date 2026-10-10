use std::fs;
use std::io::{Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use sha2::{Digest, Sha256};

/// Shares immutable checkpoint bytes without sharing a SQLite connection.
#[derive(Clone)]
pub struct ObjectStore {
    root: Arc<PathBuf>,
    cache: Arc<
        std::sync::Mutex<std::collections::HashMap<(PathBuf, String), (FileStamp, String, bool)>>,
    >,
    baseline: Arc<std::sync::Mutex<std::collections::HashMap<String, String>>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct FileStamp {
    length: u64,
    modified: std::time::SystemTime,
    created: Option<std::time::SystemTime>,
    readonly: bool,
    #[cfg(windows)]
    identity: Option<(u32, u64, i64)>,
    #[cfg(unix)]
    identity: (u64, u64, u32, i64, i64),
}

impl ObjectStore {
    pub fn open(data_root: &Path) -> Result<Self> {
        let root = data_root.join("objects").join("sha256");
        fs::create_dir_all(&root)
            .with_context(|| format!("create Harness object directory {}", root.display()))?;
        Ok(Self {
            root: Arc::new(root),
            cache: Arc::default(),
            baseline: Arc::default(),
        })
    }

    /// Publishes complete bytes without replacing an existing content identity.
    pub fn put(&self, content: &[u8]) -> Result<String> {
        let object_id = crate::plan::digest(content);
        let path = self.path(&object_id)?;
        let directory = path.parent().context("object directory is missing")?;
        fs::create_dir_all(directory)?;
        if path.exists() {
            self.verify(&object_id)?;
            return Ok(object_id);
        }
        let mut temporary = tempfile::NamedTempFile::new_in(directory)?;
        temporary.write_all(content)?;
        self.publish(temporary, object_id)
    }

    /// Captures a regular file with bounded buffers and rejects observed changes during acquisition.
    pub fn put_file(&self, source: &Path) -> Result<String> {
        let mut file = fs::File::open(source)
            .with_context(|| format!("open checkpoint source {}", source.display()))?;
        let before = handle_stamp(&file)?;
        ensure!(
            before == source_stamp(source)?,
            "checkpoint source changed before acquisition"
        );
        let mut temporary = tempfile::NamedTempFile::new_in(self.root.as_ref())?;
        let object_id = copy_exact(&mut file, &mut temporary, before.length)?;
        ensure!(
            before == handle_stamp(&file)? && before == source_stamp(source)?,
            "checkpoint source changed during acquisition"
        );
        self.publish(temporary, object_id)
    }

    /// Acquires uncertain content once and publishes only deviations from the Git baseline.
    pub(crate) fn capture_file(
        &self,
        source: &Path,
        root: &Path,
        baseline: Option<&forge_git::checkpoint::BaseFile>,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<(Option<String>, bool)> {
        let stamp = source_stamp(source)?;
        let baseline_key = baseline
            .map(serde_json::to_string)
            .transpose()?
            .unwrap_or_default();
        let cache_key = (source.to_owned(), baseline_key);
        if let Some((previous, identity, unchanged)) = self
            .cache
            .lock()
            .map_err(|_| anyhow::anyhow!("checkpoint cache poisoned"))?
            .get(&cache_key)
            && previous == &stamp
            && (*unchanged || self.path(identity)?.is_file())
        {
            return Ok(((!unchanged).then(|| identity.clone()), true));
        }
        let mut input = fs::File::open(source)?;
        ensure!(
            stamp == handle_stamp(&input)?,
            "checkpoint source changed before acquisition"
        );
        let mut temporary = tempfile::NamedTempFile::new_in(self.root.as_ref())?;
        let identity = copy_checked(&mut input, &mut temporary, stamp.length, check)?;
        ensure!(
            stamp == handle_stamp(&input)? && stamp == source_stamp(source)?,
            "checkpoint source changed during acquisition"
        );
        let unchanged = match baseline {
            Some(baseline) => self.matches_baseline(root, baseline, &identity)?,
            None => false,
        };
        if !unchanged {
            self.publish(temporary, identity.clone())?;
        }
        let mut cache = self
            .cache
            .lock()
            .map_err(|_| anyhow::anyhow!("checkpoint cache poisoned"))?;
        if cache.len() >= 65536 {
            cache.clear();
        }
        cache.insert(cache_key, (stamp, identity.clone(), unchanged));
        Ok(((!unchanged).then_some(identity), false))
    }

    /// Compares a captured identity with an immutable Git blob without repeatedly reading that blob.
    pub(crate) fn matches_baseline(
        &self,
        root: &Path,
        baseline: &forge_git::checkpoint::BaseFile,
        identity: &str,
    ) -> Result<bool> {
        Ok(self.baseline_identity(root, baseline)? == identity)
    }

    /// Returns the raw working-content digest for an immutable Git source.
    pub(crate) fn baseline_identity(
        &self,
        root: &Path,
        baseline: &forge_git::checkpoint::BaseFile,
    ) -> Result<String> {
        let key = serde_json::to_string(baseline)?;
        if let Some(digest) = self
            .baseline
            .lock()
            .map_err(|_| anyhow::anyhow!("baseline cache poisoned"))?
            .get(&key)
        {
            return Ok(digest.clone());
        }
        let mut digest_writer = DigestWriter {
            output: std::io::sink(),
            digest: Sha256::new(),
        };
        forge_git::checkpoint::write_blob(root, baseline, &mut digest_writer)?;
        let digest = hex::encode(digest_writer.digest.finalize());
        let mut cache = self
            .baseline
            .lock()
            .map_err(|_| anyhow::anyhow!("baseline cache poisoned"))?;
        if cache.len() >= 65536 {
            cache.clear();
        }
        cache.insert(key, digest.clone());
        Ok(digest)
    }

    /// Publishes an existing Git source through a bounded writer without an object-sized copy.
    pub(crate) fn put_git(
        &self,
        root: &Path,
        source: &forge_git::checkpoint::BaseFile,
    ) -> Result<String> {
        let temporary = tempfile::NamedTempFile::new_in(self.root.as_ref())?;
        let mut writer = DigestWriter {
            output: temporary,
            digest: Sha256::new(),
        };
        forge_git::checkpoint::write_blob(root, source, &mut writer)?;
        self.publish(writer.output, hex::encode(writer.digest.finalize()))
    }

    /// Locates session-owned recovery records beside immutable content objects.
    pub(crate) fn recovery_root(&self) -> PathBuf {
        self.root
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("restore")
    }

    fn publish(&self, temporary: tempfile::NamedTempFile, object_id: String) -> Result<String> {
        let path = self.path(&object_id)?;
        fs::create_dir_all(path.parent().context("object directory is missing")?)?;
        temporary.as_file().sync_all()?;
        match temporary.persist_noclobber(&path) {
            Ok(_) => {}
            Err(error) if error.error.kind() == std::io::ErrorKind::AlreadyExists => {
                self.verify(&object_id)?;
            }
            Err(error) => {
                return Err(error.error)
                    .with_context(|| format!("publish Harness object {}", path.display()));
            }
        }
        Ok(object_id)
    }

    /// Returns verified bytes, or `None` when the stored length exceeds the caller's byte limit.
    /// Oversized objects are not read or hash-verified. Accepted objects reject observed changes.
    pub fn get(&self, object_id: &str, byte_limit: usize) -> Result<Option<Vec<u8>>> {
        let path = self.path(object_id)?;
        let mut source = fs::File::open(&path)
            .with_context(|| format!("open Harness object {}", path.display()))?;
        let before = file_stamp(&source.metadata()?)?;
        if before.length > byte_limit as u64 {
            return Ok(None);
        }
        let length =
            usize::try_from(before.length).context("object length exceeds address space")?;
        let mut content = Vec::new();
        content.try_reserve_exact(length)?;
        ensure!(
            copy_exact(&mut source, &mut content, before.length)?.eq_ignore_ascii_case(object_id),
            "Harness object digest mismatch: {object_id}"
        );
        ensure!(
            before == file_stamp(&source.metadata()?)?,
            "Harness object changed during retrieval"
        );
        Ok(Some(content))
    }

    /// Verifies bytes before atomically replacing the destination from a same-directory temporary file.
    /// The caller owns path validation, mutation admission, and recovery journaling.
    pub fn restore_file(&self, object_id: &str, destination: &Path) -> Result<()> {
        let path = self.path(object_id)?;
        let mut source = fs::File::open(&path)
            .with_context(|| format!("open Harness object {}", path.display()))?;
        let before = file_stamp(&source.metadata()?)?;
        let mut verified = tempfile::NamedTempFile::new_in(self.root.as_ref())?;
        ensure!(
            copy_exact(&mut source, &mut verified, before.length)?.eq_ignore_ascii_case(object_id),
            "Harness object digest mismatch: {object_id}"
        );
        ensure!(
            before == file_stamp(&source.metadata()?)?,
            "Harness object changed during restoration preparation"
        );
        verified.rewind()?;
        let parent = destination
            .parent()
            .context("rollback destination has no parent")?;
        fs::create_dir_all(parent)?;
        let mut output = tempfile::NamedTempFile::new_in(parent)?;
        ensure!(
            copy_exact(&mut verified, &mut output, before.length)?.eq_ignore_ascii_case(object_id),
            "verified rollback bytes changed during restoration"
        );
        output.as_file().sync_all()?;
        output
            .persist(destination)
            .map_err(|error| error.error)
            .with_context(|| format!("replace rollback destination {}", destination.display()))?;
        Ok(())
    }

    fn path(&self, object_id: &str) -> Result<PathBuf> {
        ensure!(
            object_id.len() == 64 && object_id.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "invalid Harness object identifier"
        );
        Ok(self.root.join(&object_id[..2]).join(&object_id[2..]))
    }

    /// Validates stored bytes without allocating an object-sized buffer or changing workspace files.
    pub fn verify(&self, object_id: &str) -> Result<()> {
        let path = self.path(object_id)?;
        let mut file = fs::File::open(&path)?;
        let before = file_stamp(&file.metadata()?)?;
        ensure!(
            copy_exact(&mut file, &mut std::io::sink(), before.length)?
                .eq_ignore_ascii_case(object_id),
            "Harness object digest mismatch: {object_id}"
        );
        ensure!(
            before == file_stamp(&file.metadata()?)?,
            "Harness object changed during verification"
        );
        Ok(())
    }
}

struct DigestWriter<Output> {
    output: Output,
    digest: Sha256,
}

impl<Output: Write> Write for DigestWriter<Output> {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let count = self.output.write(bytes)?;
        self.digest.update(&bytes[..count]);
        Ok(count)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.output.flush()
    }
}

fn file_stamp(metadata: &fs::Metadata) -> Result<FileStamp> {
    ensure!(
        metadata.is_file(),
        "checkpoint source is not a regular file"
    );
    Ok(FileStamp {
        length: metadata.len(),
        modified: metadata.modified()?,
        created: metadata.created().ok(),
        readonly: metadata.permissions().readonly(),
        #[cfg(windows)]
        identity: None,
        #[cfg(unix)]
        identity: {
            use std::os::unix::fs::MetadataExt;
            (
                metadata.dev(),
                metadata.ino(),
                metadata.mode(),
                metadata.ctime(),
                metadata.ctime_nsec(),
            )
        },
    })
}

fn source_stamp(path: &Path) -> Result<FileStamp> {
    ensure!(
        fs::symlink_metadata(path)?.is_file(),
        "checkpoint source is no longer a regular file"
    );
    let stamp = handle_stamp(&fs::File::open(path)?)?;
    ensure!(
        fs::symlink_metadata(path)?.is_file(),
        "checkpoint source changed kind during acquisition"
    );
    Ok(stamp)
}

fn handle_stamp(file: &fs::File) -> Result<FileStamp> {
    #[allow(unused_mut)]
    let mut stamp = file_stamp(&file.metadata()?)?;
    #[cfg(windows)]
    {
        use std::os::windows::io::AsRawHandle;
        use windows::Win32::{
            Foundation::HANDLE,
            Storage::FileSystem::{
                BY_HANDLE_FILE_INFORMATION, FILE_BASIC_INFO, FileBasicInfo,
                GetFileInformationByHandle, GetFileInformationByHandleEx,
            },
        };
        let mut identity = BY_HANDLE_FILE_INFORMATION::default();
        let mut basic = FILE_BASIC_INFO::default();
        unsafe {
            let handle = HANDLE(file.as_raw_handle());
            GetFileInformationByHandle(handle, &mut identity)?;
            GetFileInformationByHandleEx(
                handle,
                FileBasicInfo,
                (&mut basic as *mut FILE_BASIC_INFO).cast(),
                std::mem::size_of::<FILE_BASIC_INFO>() as u32,
            )?;
        }
        stamp.identity = Some((
            identity.dwVolumeSerialNumber,
            (u64::from(identity.nFileIndexHigh) << 32) | u64::from(identity.nFileIndexLow),
            basic.ChangeTime,
        ));
    }
    Ok(stamp)
}

fn copy_exact(reader: &mut impl Read, writer: &mut impl Write, length: u64) -> Result<String> {
    copy_checked(reader, writer, length, &mut || Ok(()))
}

fn copy_checked(
    reader: &mut impl Read,
    writer: &mut impl Write,
    length: u64,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<String> {
    let mut buffer = [0; 64 * 1024];
    let mut remaining = length;
    let mut digest = Sha256::new();
    while remaining > 0 {
        check()?;
        let count = remaining.min(buffer.len() as u64) as usize;
        reader
            .read_exact(&mut buffer[..count])
            .context("checkpoint source shortened during acquisition")?;
        writer.write_all(&buffer[..count])?;
        digest.update(&buffer[..count]);
        remaining -= count as u64;
    }
    loop {
        match reader.read(&mut buffer[..1]) {
            Ok(0) => break,
            Ok(_) => anyhow::bail!("checkpoint source grew during acquisition"),
            Err(error) if error.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error.into()),
        }
    }
    Ok(hex::encode(digest.finalize()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concurrent_publication_retains_one_complete_object() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let content = vec![42; 256 * 1024];
        let source = directory.path().join("source.bin");
        fs::write(&source, &content).unwrap();
        let barrier = std::sync::Barrier::new(4);
        let identities = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..4)
                .map(|_| {
                    scope.spawn(|| {
                        barrier.wait();
                        objects.put_file(&source).unwrap()
                    })
                })
                .collect();
            workers
                .into_iter()
                .map(|worker| worker.join().unwrap())
                .collect::<Vec<_>>()
        });
        assert!(identities.iter().all(|identity| identity == &identities[0]));
        assert_eq!(
            objects.get(&identities[0], content.len()).unwrap().unwrap(),
            content
        );
        let path = objects.path(&identities[0]).unwrap();
        assert_eq!(fs::read_dir(path.parent().unwrap()).unwrap().count(), 1);
    }

    #[test]
    fn corrupt_content_and_invalid_identifiers_are_rejected() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let identity = objects.put(b"checkpoint").unwrap();
        fs::write(objects.path(&identity).unwrap(), b"corrupt").unwrap();
        assert!(objects.get(&identity, 1024).is_err());
        assert!(objects.put(b"checkpoint").is_err());
        assert!(objects.get("../outside", 1024).is_err());
        assert!(objects.get(&"g".repeat(64), 1024).is_err());
    }

    #[test]
    fn streamed_files_match_supplied_bytes_including_empty_and_binary_content() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let source = directory.path().join("source.bin");
        for content in [Vec::new(), b"first\r\nsecond\n\0binary\xff".repeat(20_000)] {
            fs::write(&source, &content).unwrap();
            let streamed = objects.put_file(&source).unwrap();
            assert_eq!(streamed, objects.put(&content).unwrap());
            assert_eq!(
                objects.get(&streamed, content.len()).unwrap().unwrap(),
                content
            );
            assert_eq!(objects.put_file(&source).unwrap(), streamed);
        }
        assert!(objects.put_file(directory.path()).is_err());
        assert!(objects.put_file(&directory.path().join("missing")).is_err());
    }

    #[test]
    fn source_copy_bounds_read_requests_and_rejects_size_changes() {
        struct Source {
            remaining: usize,
            maximum_request: usize,
        }
        impl Read for Source {
            fn read(&mut self, buffer: &mut [u8]) -> std::io::Result<usize> {
                self.maximum_request = self.maximum_request.max(buffer.len());
                let count = buffer.len().min(self.remaining);
                buffer[..count].fill(7);
                self.remaining -= count;
                Ok(count)
            }
        }
        let mut source = Source {
            remaining: 2 * 1024 * 1024,
            maximum_request: 0,
        };
        copy_exact(&mut source, &mut std::io::sink(), 2 * 1024 * 1024).unwrap();
        assert_eq!(source.maximum_request, 64 * 1024);
        let mut growing = std::io::repeat(7).take(100);
        assert!(copy_exact(&mut growing, &mut std::io::sink(), 9).is_err());
        assert_eq!(growing.limit(), 90);
        let mut shortened = std::io::Cursor::new([7; 8]);
        assert!(copy_exact(&mut shortened, &mut std::io::sink(), 9).is_err());
    }

    #[test]
    fn retrieval_enforces_the_byte_limit_before_reading_and_accepts_exact_limits() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let identity = objects.put(b"exact").unwrap();
        assert_eq!(objects.get(&identity, 5).unwrap().unwrap(), b"exact");
        assert!(objects.get(&identity, 4).unwrap().is_none());
        fs::write(objects.path(&identity).unwrap(), b"wrong").unwrap();
        assert!(objects.get(&identity, 4).unwrap().is_none());
        assert!(objects.get(&identity, 5).is_err());
        let empty = objects.put(b"").unwrap();
        assert!(objects.get(&empty, 0).unwrap().unwrap().is_empty());
    }

    #[test]
    fn streamed_restore_preserves_exact_bytes_and_truncates_old_content() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let destination = directory.path().join("restored.bin");
        for content in [
            b"first\r\nlast\0\xff".repeat(30_000),
            Vec::new(),
            b"no final newline".to_vec(),
        ] {
            let identity = objects.put(&content).unwrap();
            fs::write(&destination, vec![8; content.len() + 300]).unwrap();
            objects.restore_file(&identity, &destination).unwrap();
            assert_eq!(fs::read(&destination).unwrap(), content);
            assert!(
                fs::read_dir(objects.root.as_ref())
                    .unwrap()
                    .all(|entry| entry.unwrap().file_type().unwrap().is_dir())
            );
        }
    }

    #[test]
    fn corrupt_objects_leave_restore_destinations_untouched_and_remove_temporary_files() {
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let identity = objects.put(b"valid").unwrap();
        fs::write(objects.path(&identity).unwrap(), b"wrong").unwrap();
        let destination = directory.path().join("destination");
        fs::write(&destination, b"keep this").unwrap();
        assert!(objects.restore_file(&identity, &destination).is_err());
        assert_eq!(fs::read(&destination).unwrap(), b"keep this");
        let absent = directory.path().join("absent");
        assert!(objects.restore_file(&identity, &absent).is_err());
        assert!(!absent.exists());
        assert!(
            fs::read_dir(objects.root.as_ref())
                .unwrap()
                .all(|entry| entry.unwrap().file_type().unwrap().is_dir())
        );
    }

    #[cfg(unix)]
    #[test]
    fn streamed_restore_preserves_existing_executable_permissions() {
        use std::os::unix::fs::PermissionsExt;
        let directory = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(directory.path()).unwrap();
        let identity = objects.put(b"restored executable\n").unwrap();
        let destination = directory.path().join("executable");
        fs::write(&destination, b"previous").unwrap();
        fs::set_permissions(&destination, fs::Permissions::from_mode(0o755)).unwrap();
        objects.restore_file(&identity, &destination).unwrap();
        assert_eq!(
            fs::metadata(destination).unwrap().permissions().mode() & 0o777,
            0o755
        );
    }
}
