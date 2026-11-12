use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use anyhow::{Context, Result, ensure};
use serde::Serialize;

const INLINE_CHUNK_BYTES: usize = 16 * 1024;
pub(super) const INITIAL_INLINE_BYTES: usize = 64 * 1024;

#[derive(Debug, Serialize)]
pub struct ToolOutputPreview<'a> {
    pub row: Vec<&'a str>,
    pub hidden_rows: usize,
    pub total_rows: usize,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ToolOutputBatch {
    pub call_id: super::identity::DisplayId,
    pub start_row: usize,
    pub row: Vec<String>,
    pub complete: bool,
    byte_limit: usize,
}

pub struct ToolOutputView {
    call_id: super::identity::DisplayId,
    file: Option<(PathBuf, u64, u64)>,
    database: Option<StoredOutput>,
    saved: Arc<String>,
    parsed: OnceLock<std::result::Result<ParsedOutput, String>>,
    /// Source heading fields and the tool-clock reading captured with them, so an inline
    /// activation renders the same elapsed time as the projection that produced it.
    heading: Option<(crate::turn::ToolCall, crate::turn::ToolInstant)>,
    owner: String,
    pub(super) group: String,
}

/// Locates retained output without keeping its bytes in the timeline.
#[derive(Clone, PartialEq)]
struct StoredOutput {
    root: PathBuf,
    exchange: String,
    turn: String,
    tool: String,
    total: Option<u64>,
    loaded: u64,
}

/// Owns one incremental terminal parser and its normalized row index.
struct ParsedOutput {
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    parser: strip_ansi_escapes::Writer<OutputCollector>,
    collected: Arc<Mutex<Vec<u8>>>,
    total_rows: usize,
}

impl ParsedOutput {
    fn new(saved: &str) -> Result<Self> {
        let collected = Arc::new(Mutex::new(Vec::new()));
        let mut output = Self {
            display: Arc::new(String::new()),
            row: Arc::new(Vec::new()),
            parser: strip_ansi_escapes::Writer::new(OutputCollector(collected.clone())),
            collected,
            total_rows: 0,
        };
        output.append(saved)?;
        Ok(output)
    }

    fn append(&mut self, delta: &str) -> Result<()> {
        self.parser.write_all(delta.as_bytes())?;
        self.parser.flush()?;
        let normalized = String::from_utf8(std::mem::take(
            &mut *self
                .collected
                .lock()
                .map_err(|_| anyhow::anyhow!("tool output collector poisoned"))?,
        ))?;
        let display = Arc::make_mut(&mut self.display);
        let row = Arc::make_mut(&mut self.row);
        let mut start = display.len();
        if !display.ends_with('\n')
            && let Some(previous) = row.pop()
        {
            start = previous.start;
        }
        let offset = display.len();
        display.push_str(&normalized);
        for (relative, byte) in normalized.bytes().enumerate() {
            if byte == b'\n' {
                let end = offset + relative;
                row.push(start..end);
                if end > start {
                    self.total_rows = row.len();
                }
                start = end + 1;
            }
        }
        if start < display.len() {
            row.push(start..display.len());
            self.total_rows = row.len();
        }
        Ok(())
    }

    fn inline_bytes(&self) -> usize {
        self.total_rows
            .checked_sub(1)
            .map_or(0, |index| self.row[index].end)
    }
}

/// A fixed output version shares parsed bytes without owning a streaming parser.
pub struct ToolOutputSnapshot {
    call_id: super::identity::DisplayId,
    saved: Arc<String>,
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    total_rows: usize,
    loaded_rows: usize,
    expanded: bool,
    deferred: Option<Box<ToolOutputView>>,
}

struct OutputCollector(Arc<Mutex<Vec<u8>>>);

impl Write for OutputCollector {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.0
            .lock()
            .map_err(|_| std::io::Error::other("tool output collector poisoned"))?
            .extend_from_slice(bytes);
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl ToolOutputView {
    pub(crate) fn stored(
        call_id: super::identity::DisplayId,
        root: &Path,
        exchange: &str,
        turn: &str,
        tool: &str,
    ) -> Self {
        let mut output = Self::new(call_id, "");
        output.database = Some(StoredOutput {
            root: root.to_owned(),
            exchange: exchange.into(),
            turn: turn.into(),
            tool: tool.into(),
            total: None,
            loaded: 0,
        });
        output
    }

    pub(super) fn file(call_id: super::identity::DisplayId, path: PathBuf, bytes: u64) -> Self {
        let mut source = Self::new(call_id, "");
        source.file = Some((path, bytes, 0));
        source
    }

    pub(super) fn has_unloaded_output(&self) -> bool {
        self.database
            .as_ref()
            .is_some_and(|source| source.total.is_none_or(|total| source.loaded < total))
            || self
                .file
                .as_ref()
                .is_some_and(|(_, total, loaded)| loaded < total)
    }

    pub(super) fn load_prefix(&mut self, limit: usize) -> Result<()> {
        if let Some(mut source) = self.database.clone() {
            let store = crate::storage::HistoryReader::current(&source.root)?;
            let wanted = (limit as u64).saturating_sub(source.loaded);
            if wanted == 0 {
                return Ok(());
            }
            let (bytes, total) = store
                .tool_output_bytes(
                    &source.exchange,
                    &source.turn,
                    &source.tool,
                    source.loaded,
                    wanted,
                )
                .with_context(|| format!("read stored output for tool {}", source.tool))?;
            let total = source.total.unwrap_or(total);
            let bytes = &bytes[..bytes
                .len()
                .min(total.saturating_sub(source.loaded) as usize)];
            let length = match std::str::from_utf8(bytes) {
                Ok(_) => bytes.len(),
                Err(error)
                    if error.error_len().is_none()
                        && source.loaded + (bytes.len() as u64) < total =>
                {
                    error.valid_up_to()
                }
                Err(error) => return Err(error).context("stored tool output is not UTF-8"),
            };
            self.append(std::str::from_utf8(&bytes[..length])?)?;
            source.total = Some(total);
            source.loaded += length as u64;
            self.database = Some(source);
            return Ok(());
        }
        let Some((path, total, loaded)) = self.file.clone() else {
            return Ok(());
        };
        let end = total.min(limit as u64);
        if end <= loaded {
            return Ok(());
        }
        let mut file =
            File::open(&path).with_context(|| format!("open check output {}", path.display()))?;
        file.seek(SeekFrom::Start(loaded))?;
        let mut bytes = Vec::new();
        file.take(end - loaded).read_to_end(&mut bytes)?;
        let length = match std::str::from_utf8(&bytes) {
            Ok(_) => bytes.len(),
            Err(error) if error.error_len().is_none() && end < total => error.valid_up_to(),
            Err(_) => bytes.len(),
        };
        self.append(&String::from_utf8_lossy(&bytes[..length]))?;
        self.file = Some((path, total, loaded + length as u64));
        Ok(())
    }

    fn parsed(&self) -> Result<&ParsedOutput> {
        self.parsed
            .get_or_init(|| ParsedOutput::new(&self.saved).map_err(|error| format!("{error:#}")))
            .as_ref()
            .map_err(|error| anyhow::anyhow!(error.clone()))
    }

    pub fn snapshot(&self) -> Result<ToolOutputSnapshot> {
        let parsed = self.parsed()?;
        Ok(ToolOutputSnapshot {
            call_id: self.call_id,
            saved: Arc::clone(&self.saved),
            display: Arc::clone(&parsed.display),
            row: Arc::clone(&parsed.row),
            total_rows: parsed.total_rows,
            loaded_rows: 0,
            expanded: false,
            deferred: None,
        })
    }

    /// Captures a deferred source for an explicitly opened output document.
    pub(crate) fn complete_snapshot(&self) -> Result<ToolOutputSnapshot> {
        if let Some(source) = &self.database {
            let deferred = Self::stored(
                self.call_id,
                &source.root,
                &source.exchange,
                &source.turn,
                &source.tool,
            );
            let mut snapshot = deferred.snapshot()?;
            snapshot.deferred = Some(Box::new(deferred));
            return Ok(snapshot);
        }
        let Some((path, total, _)) = &self.file else {
            return self.snapshot();
        };
        let deferred = Self::file(self.call_id, path.clone(), *total);
        let mut snapshot = deferred.snapshot()?;
        snapshot.deferred = Some(Box::new(deferred));
        Ok(snapshot)
    }
    /// Retains source heading fields without copying the saved output or change tree.
    pub(super) fn heading(&mut self, call: &crate::turn::ToolCall, now: crate::turn::ToolInstant) {
        self.heading = Some((call.heading(), now));
    }

    /// Associates inline chunks with their containing projected entry.
    pub(super) fn own(&mut self, owner: &str) {
        self.owner = owner.to_owned();
    }

    /// Resolves the indexed entry owner for inline activation.
    pub(super) fn owner(&self) -> &str {
        &self.owner
    }

    /// Renders activation headings from source metadata rather than truncated native text.
    pub(super) fn header(&self) -> Option<(&crate::turn::ToolCall, crate::turn::ToolInstant)> {
        self.heading.as_ref().map(|(call, now)| (call, *now))
    }

    /// Keeps the parser and saved bytes while adopting newly projected heading ownership.
    pub(super) fn retain(&mut self, replacement: &Self) -> bool {
        if let (Some(source), Some(next)) = (&self.database, &replacement.database) {
            if source.root != next.root
                || source.exchange != next.exchange
                || source.turn != next.turn
                || source.tool != next.tool
            {
                return false;
            }
        } else if self.database.is_some() != replacement.database.is_some() {
            return false;
        }
        if let (Some((path, total, _)), Some((next_path, next_total, _))) =
            (&mut self.file, &replacement.file)
        {
            if path != next_path || next_total < total {
                return false;
            }
            *total = *next_total;
        } else if self.saved != replacement.saved
            || self.file.is_some() != replacement.file.is_some()
        {
            return false;
        }
        self.heading = replacement.heading.clone();
        self.owner.clone_from(&replacement.owner);
        self.group.clone_from(&replacement.group);
        true
    }

    /// Counts independent inline chunks without walking retained output rows.
    pub(super) fn inline_chunks(&self) -> Result<usize> {
        Ok(self
            .parsed()?
            .inline_bytes()
            .div_ceil(INLINE_CHUNK_BYTES)
            .max(1))
    }

    /// Limits formatted source chunks independently of the retained raw output.
    pub(super) fn inline_prefix_chunks(&self, byte_limit: usize) -> Result<usize> {
        Ok(self
            .inline_chunks()?
            .min(byte_limit.div_ceil(INLINE_CHUNK_BYTES).max(1)))
    }

    /// Returns the first chunk whose mutable tail can change after an append.
    pub(super) fn inline_tail(&self) -> Result<usize> {
        Ok(self.parsed()?.inline_bytes() / INLINE_CHUNK_BYTES)
    }

    /// Borrows at most one byte window with UTF-8 boundaries retained across appends.
    pub(super) fn inline_chunk(&self, index: usize) -> Result<Vec<&str>> {
        let parsed = self.parsed()?;
        let bytes = parsed.inline_bytes();
        let boundary = |offset: usize| {
            let mut offset = offset.min(bytes);
            while !parsed.display.is_char_boundary(offset) {
                offset += 1;
            }
            offset
        };
        let start = boundary(index * INLINE_CHUNK_BYTES);
        let end = boundary((index + 1) * INLINE_CHUNK_BYTES);
        Ok(parsed.display[start..end].split_terminator('\n').collect())
    }

    /// Retains raw output without parsing until a preview, expansion, or output view needs it.
    pub fn new(call_id: super::identity::DisplayId, saved: &str) -> Self {
        Self {
            call_id,
            file: None,
            database: None,
            saved: Arc::new(saved.to_owned()),
            parsed: OnceLock::new(),
            heading: None,
            owner: String::new(),
            group: String::new(),
        }
    }

    pub fn append(&mut self, delta: &str) -> Result<()> {
        let Some(parsed) = self.parsed.get_mut() else {
            Arc::make_mut(&mut self.saved).push_str(delta);
            return Ok(());
        };
        let parsed = parsed
            .as_mut()
            .map_err(|error| anyhow::anyhow!(error.clone()))?;
        parsed.append(delta)?;
        Arc::make_mut(&mut self.saved).push_str(delta);
        Ok(())
    }

    pub fn preview(&self, expanded: bool) -> Result<ToolOutputPreview<'_>> {
        let parsed = self.parsed()?;
        let visible = if expanded {
            parsed.total_rows
        } else {
            4.min(parsed.total_rows)
        };
        let start = parsed.total_rows - visible;
        Ok(ToolOutputPreview {
            row: parsed.row[start..parsed.total_rows]
                .iter()
                .map(|range| &parsed.display[range.clone()])
                .collect(),
            hidden_rows: parsed.total_rows.saturating_sub(visible),
            total_rows: parsed.total_rows,
        })
    }
}

impl ToolOutputSnapshot {
    /// Loads only enough source to produce another complete-line delivery batch.
    pub(super) fn prepare_batch(&mut self) -> Result<()> {
        let Some(source) = &mut self.deferred else {
            return Ok(());
        };
        loop {
            let parsed = source.parsed()?;
            let complete_rows = parsed.total_rows.saturating_sub(usize::from(
                source.has_unloaded_output() && !parsed.display.ends_with('\n'),
            ));
            if complete_rows > self.loaded_rows || !source.has_unloaded_output() {
                break;
            }
            let previous = source.saved.len();
            // Release this snapshot's old views before extending the shared source.
            self.saved = Arc::new(String::new());
            self.display = Arc::new(String::new());
            self.row = Arc::new(Vec::new());
            source.load_prefix(previous.saturating_add(65536))?;
            ensure!(
                source.saved.len() > previous || !source.has_unloaded_output(),
                "saved output ended before its recorded length"
            );
        }
        let parsed = source.parsed()?;
        self.saved = source.saved.clone();
        self.display = parsed.display.clone();
        self.row = parsed.row.clone();
        self.total_rows = parsed.total_rows.saturating_sub(usize::from(
            source.has_unloaded_output() && !parsed.display.ends_with('\n'),
        ));
        Ok(())
    }

    #[cfg(test)]
    pub fn retained_bytes(&self) -> usize {
        self.saved.len()
            + self.display.capacity()
            + self.row.capacity() * std::mem::size_of::<Range<usize>>()
    }

    pub fn expand(&mut self) {
        self.expanded = true;
    }

    pub fn collapse(&mut self) {
        self.expanded = false;
    }

    /// Prepares the next bounded batch without advancing its native document cursor.
    pub fn next_batch(
        &self,
        row_limit: usize,
        byte_limit: usize,
    ) -> Result<Option<ToolOutputBatch>> {
        let row_limit = row_limit.clamp(1, 256);
        let byte_limit = byte_limit.clamp(128, 65536);
        if !self.expanded || self.loaded_rows == self.total_rows {
            return Ok(None);
        }
        let mut row = Vec::new();
        let mut bytes = 0;
        for range in self.row[self.loaded_rows..self.total_rows]
            .iter()
            .take(row_limit)
        {
            let text = &self.display[range.clone()];
            if !row.is_empty() && bytes + text.len() + 1 > byte_limit {
                break;
            }
            bytes += text.len() + 1;
            row.push(text.to_owned());
        }
        Ok(Some(ToolOutputBatch {
            call_id: self.call_id,
            start_row: self.loaded_rows,
            complete: self.loaded_rows + row.len() == self.total_rows
                && !self
                    .deferred
                    .as_ref()
                    .is_some_and(|source| source.has_unloaded_output()),
            byte_limit,
            row,
        }))
    }

    /// Advances only after the owning native document accepts this exact batch.
    pub fn accept_batch(&mut self, batch: &ToolOutputBatch) -> Result<()> {
        let expected = self.next_batch(batch.row.len(), batch.byte_limit)?;
        ensure!(
            expected.as_ref() == Some(batch),
            "tool batch differs from saved output or cursor"
        );
        self.loaded_rows += batch.row.len();
        Ok(())
    }

    pub fn export_saved_output(
        &mut self,
        directory: &Path,
        trace: Arc<crate::trace::TraceStore>,
        session_id: &str,
    ) -> Result<OwnedToolExport> {
        if let Some(source) = &mut self.deferred {
            source.load_prefix(usize::MAX)?;
            self.saved = source.saved.clone();
        }
        OwnedToolExport::create(directory, &self.saved, trace, session_id)
    }
}

pub struct OwnedToolExport {
    file: Option<File>,
    path: Option<PathBuf>,
    trace: Arc<crate::trace::TraceStore>,
    session_id: String,
}

impl OwnedToolExport {
    pub fn create(
        directory: &Path,
        saved: &str,
        trace: Arc<crate::trace::TraceStore>,
        session_id: &str,
    ) -> Result<Self> {
        std::fs::create_dir_all(directory).context("create tool export directory")?;
        let directory = directory
            .canonicalize()
            .context("resolve tool export directory")?;
        let path = directory.join(format!("{}.txt", uuid::Uuid::new_v4()));
        let mut options = OpenOptions::new();
        options.read(true).write(true).create_new(true);
        #[cfg(unix)]
        {
            use std::os::unix::fs::OpenOptionsExt;
            options.mode(0o600);
        }
        let file = options
            .open(&path)
            .context("create owned tool output export")?;
        let mut export = Self {
            file: None,
            path: Some(path),
            trace,
            session_id: session_id.into(),
        };
        let output = export.file.insert(file);
        for chunk in saved.as_bytes().chunks(65536) {
            output.write_all(chunk)?;
        }
        output
            .sync_all()
            .context("persist complete tool output export")?;
        Ok(export)
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    pub fn close(&mut self) -> Result<()> {
        drop(self.file.take());
        if let Some(path) = self.path.as_ref() {
            match std::fs::remove_file(path) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error).context("remove owned tool output export"),
            }
        }
        self.path = None;
        Ok(())
    }
}

impl Drop for OwnedToolExport {
    fn drop(&mut self) {
        if let Err(error) = self.close() {
            self.trace.record(
                &self.session_id,
                "tool.export_cleanup_failed",
                serde_json::json!({"error":format!("{error:#}")}),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_preserves_fragmented_ansi_partial_rows_and_trailing_blanks() {
        let mut output = ToolOutputView::new(crate::buffer::identity::DisplayId::test(), "");
        for chunk in [
            "\u{1b}[3",
            "1mλ",
            "\r",
            "\n\n",
            "tail",
            " continued\u{1b}[",
            "0m\n\n",
        ] {
            output.append(chunk).unwrap();
        }
        assert_eq!(
            output.preview(false).unwrap().row,
            vec!["λ", "", "tail continued"]
        );
        assert_eq!(output.preview(false).unwrap().total_rows, 3);
        output.append("last").unwrap();
        assert_eq!(
            output.preview(true).unwrap().row,
            vec!["λ", "", "tail continued", "", "last"]
        );
        assert_eq!(output.preview(false).unwrap().hidden_rows, 1);
    }
}
