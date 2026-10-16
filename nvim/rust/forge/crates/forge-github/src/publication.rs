use std::fs;
use std::io::{BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};
use serde::Serialize;

/// Publish complete JSON through a synced file while the caller retains writer ownership.
pub(crate) fn publish_json(output: &Path, value: &impl Serialize) -> Result<()> {
    let parent = output
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    let mut temporary = tempfile::NamedTempFile::new_in(parent)?;
    let mut writer = BufWriter::with_capacity(64 * 1024, temporary.as_file_mut());
    serde_json::to_writer(&mut writer, value)?;
    writer.flush()?;
    drop(writer);
    temporary.as_file().sync_all()?;
    temporary
        .persist(output)
        .with_context(|| format!("publish {}", output.display()))?;
    Ok(())
}
