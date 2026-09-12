use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};
use serde::Serialize;

pub(crate) const MAX_CACHE_JSON_BYTES: usize = 16 * 1024 * 1024;

/// Publishes bounded JSON through a synced temporary file while the caller retains writer ownership.
pub(crate) fn publish_json(output: &Path, value: &impl Serialize) -> Result<()> {
    let parent = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    let mut temporary = tempfile::NamedTempFile::new_in(parent)?;
    let mut writer = PublicationWriter {
        file: BufWriter::with_capacity(64 * 1024, temporary.as_file_mut()),
        remaining: MAX_CACHE_JSON_BYTES,
    };
    serde_json::to_writer(&mut writer, value)?;
    writer.flush()?;
    drop(writer);
    temporary.as_file().sync_all()?;
    temporary
        .persist(output)
        .with_context(|| format!("publish {}", output.display()))?;
    Ok(())
}

struct PublicationWriter<'file> {
    file: BufWriter<&'file mut File>,
    remaining: usize,
}

impl Write for PublicationWriter<'_> {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        if bytes.len() > self.remaining {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "JSON publication exceeds its byte limit",
            ));
        }
        let written = self.file.write(bytes)?;
        self.remaining -= written;
        Ok(written)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.file.flush()
    }
}
