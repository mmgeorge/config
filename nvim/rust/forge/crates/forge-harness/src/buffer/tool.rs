use std::fs::{File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

use anyhow::{Context, Result, ensure};
use serde::Serialize;

const DISPLAY_ROW_LIMIT: usize = 262_144;
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
    pub call_id: String,
    pub start_row: usize,
    pub row: Vec<String>,
    pub complete: bool,
    byte_limit: usize,
}

pub struct ToolOutputView {
    call_id: String,
    file: Option<(PathBuf, u64, u64)>,
    saved: Arc<String>,
    parsed: OnceLock<std::result::Result<ParsedOutput, String>>,
    heading: Option<crate::turn::ToolCall>,
    owner: String,
    pub(super) group: String,
}

/// Owns one incremental terminal parser and its normalized row index.
struct ParsedOutput {
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    parser: strip_ansi_escapes::Writer<OutputCollector>,
    collected: Arc<Mutex<Vec<u8>>>,
    total_rows: usize,
    truncated: bool,
}

impl ParsedOutput {
    fn new(saved: &str) -> Result<Self> {
        let collected = Arc::new(Mutex::new(Vec::new()));
        let mut output = Self {
            display: Arc::new(String::new()), row: Arc::new(Vec::new()),
            parser: strip_ansi_escapes::Writer::new(OutputCollector(collected.clone())),
            collected, total_rows: 0, truncated: false,
        };
        output.append(saved)?;
        Ok(output)
    }

    fn append(&mut self, delta: &str) -> Result<()> {
        if self.truncated { return Ok(()); }
        self.parser.write_all(delta.as_bytes())?;
        self.parser.flush()?;
        let normalized = String::from_utf8(std::mem::take(&mut *self.collected.lock()
            .map_err(|_| anyhow::anyhow!("tool output collector poisoned"))?))?;
        let display = Arc::make_mut(&mut self.display);
        let row = Arc::make_mut(&mut self.row);
        let mut start = display.len();
        if !display.ends_with('\n') && let Some(previous) = row.pop() { start = previous.start; }
        let offset = display.len();
        display.push_str(&normalized);
        for (relative, byte) in normalized.bytes().enumerate() {
            if byte == b'\n' {
                let end = offset + relative;
                row.push(start..end);
                if end > start { self.total_rows = row.len(); }
                start = end + 1;
                if row.len() > DISPLAY_ROW_LIMIT { break; }
            }
        }
        if row.len() <= DISPLAY_ROW_LIMIT && start < display.len() {
            row.push(start..display.len());
            self.total_rows = row.len();
        }
        if row.len() > DISPLAY_ROW_LIMIT {
            display.truncate(row[DISPLAY_ROW_LIMIT - 1].end);
            row.truncate(DISPLAY_ROW_LIMIT);
            display.push('\n');
            let start = display.len();
            display.push_str("… Display truncated; export output for the complete response.");
            row.push(start..display.len());
            display.push('\n');
            self.total_rows = row.len();
            self.truncated = true;
        }
        Ok(())
    }

    fn inline_bytes(&self) -> usize {
        self.total_rows.checked_sub(1).map_or(0, |index| self.row[index].end)
    }
}

/// A fixed output version shares parsed bytes without owning a streaming parser.
pub struct ToolOutputSnapshot {
    call_id: String,
    saved: Arc<String>,
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    total_rows: usize,
    loaded_rows: usize,
    expanded: bool,
}

struct OutputCollector(Arc<Mutex<Vec<u8>>>);

impl Write for OutputCollector {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.0.lock().map_err(|_| std::io::Error::other("tool output collector poisoned"))?.extend_from_slice(bytes);
        Ok(bytes.len())
    }
    fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
}

impl ToolOutputView {
    pub(super) fn file(call_id: String, path: PathBuf, bytes: u64) -> Result<Self> {
        let mut source = Self::new(call_id, "")?;
        source.file = Some((path, bytes, 0));
        Ok(source)
    }

    pub(super) fn has_unloaded_output(&self) -> bool {
        self.file.as_ref().is_some_and(|(_, total, loaded)| loaded < total)
    }

    pub(super) fn load_prefix(&mut self, limit: usize) -> Result<()> {
        let Some((path, total, loaded)) = self.file.clone() else { return Ok(()); };
        let end = total.min(limit as u64);
        if end <= loaded { return Ok(()); }
        let mut file = File::open(&path).with_context(|| format!("open check output {}", path.display()))?;
        file.seek(SeekFrom::Start(loaded))?;
        let mut bytes = Vec::new();
        file.take(end - loaded).read_to_end(&mut bytes)?;
        let length = match std::str::from_utf8(&bytes) {
            Ok(_) => bytes.len(),
            Err(error) if error.error_len().is_none() && end < total => error.valid_up_to(),
            Err(_) => bytes.len(),
        };
        self.append(&String::from_utf8_lossy(&bytes[..length]))?;
        self.file = Some((path,total,loaded + length as u64));
        Ok(())
    }

    fn parsed(&self) -> Result<&ParsedOutput> {
        self.parsed.get_or_init(|| ParsedOutput::new(&self.saved).map_err(|error| format!("{error:#}")))
            .as_ref().map_err(|error| anyhow::anyhow!(error.clone()))
    }

    pub fn snapshot(&self) -> Result<ToolOutputSnapshot> {
        let parsed = self.parsed()?;
        Ok(ToolOutputSnapshot {
            call_id: self.call_id.clone(), saved: Arc::clone(&self.saved),
            display: Arc::clone(&parsed.display), row: Arc::clone(&parsed.row),
            total_rows: parsed.total_rows, loaded_rows: 0, expanded: false,
        })
    }
    /// Retains source heading fields without copying the saved output or change tree.
    pub(super) fn heading(&mut self, call: &crate::turn::ToolCall) {
        self.heading = Some(crate::turn::ToolCall {
            started_at_ms:call.started_at_ms,completed_at_ms:call.completed_at_ms,task_id:None,
            id:call.id.clone(),kind:call.kind.clone(),title:call.title.clone(),output:String::new(),
            status:call.status.clone(),failed:call.failed,change:Default::default(),
        });
    }

    /// Associates inline chunks with their containing projected entry.
    pub(super) fn own(&mut self, owner: &str) { self.owner = owner.to_owned(); }

    /// Resolves the indexed entry owner for inline activation.
    pub(super) fn owner(&self) -> &str { &self.owner }

    /// Renders activation headings from source metadata rather than truncated native text.
    pub(super) fn header(&self) -> Option<&crate::turn::ToolCall> {
        self.heading.as_ref()
    }

    /// Keeps the parser and saved bytes while adopting newly projected heading ownership.
    pub(super) fn retain(&mut self, replacement: &Self) -> bool {
        if let (Some((path,total,_)), Some((next_path,next_total,_))) = (&mut self.file,&replacement.file) {
            if path != next_path || next_total < total { return false; }
            *total = *next_total;
        } else if self.saved != replacement.saved || self.file.is_some() != replacement.file.is_some() { return false; }
        self.heading = replacement.heading.clone();
        self.owner.clone_from(&replacement.owner);
        self.group.clone_from(&replacement.group);
        true
    }

    /// Counts independent inline chunks without walking retained output rows.
    pub(super) fn inline_chunks(&self) -> Result<usize> {
        Ok(self.parsed()?.inline_bytes().div_ceil(INLINE_CHUNK_BYTES).max(1))
    }

    /// Limits formatted source chunks independently of the retained raw output.
    pub(super) fn inline_prefix_chunks(&self, byte_limit: usize) -> Result<usize> {
        Ok(self.inline_chunks()?.min(byte_limit.div_ceil(INLINE_CHUNK_BYTES).max(1)))
    }

    /// Returns the first chunk whose mutable tail can change after an append.
    pub(super) fn inline_tail(&self) -> Result<usize> {
        Ok(self.parsed()?.inline_bytes() / INLINE_CHUNK_BYTES)
    }

    pub(super) fn truncated(&self) -> Result<bool> {
        Ok(self.parsed()?.truncated)
    }

    /// Borrows at most one byte window with UTF-8 boundaries retained across appends.
    pub(super) fn inline_chunk(&self, index: usize) -> Result<Vec<&str>> {
        let parsed = self.parsed()?;
        let bytes = parsed.inline_bytes();
        let boundary = |offset: usize| {
            let mut offset = offset.min(bytes);
            while !parsed.display.is_char_boundary(offset) { offset += 1; }
            offset
        };
        let start = boundary(index * INLINE_CHUNK_BYTES);
        let end = boundary((index + 1) * INLINE_CHUNK_BYTES);
        Ok(parsed.display[start..end].split_terminator('\n').collect())
    }

    /// Retains raw output without parsing until a preview, expansion, or output view needs it.
    pub fn new(call_id: String, saved: &str) -> Result<Self> {
        ensure!(!call_id.is_empty() && call_id.len() <= 256, "invalid tool call identity");
        Ok(Self {
            call_id, file: None, saved: Arc::new(saved.to_owned()), parsed: OnceLock::new(),
            heading: None, owner: String::new(), group: String::new(),
        })
    }

    pub fn append(&mut self, delta: &str) -> Result<()> {
        if self.parsed.get().is_none() {
            Arc::make_mut(&mut self.saved).push_str(delta);
            return Ok(());
        }
        let parsed = self.parsed.get_mut().expect("initialized parser").as_mut()
            .map_err(|error| anyhow::anyhow!(error.clone()))?;
        parsed.append(delta)?;
        Arc::make_mut(&mut self.saved).push_str(delta);
        Ok(())
    }

    pub fn preview(&self, expanded: bool) -> Result<ToolOutputPreview<'_>> {
        let parsed = self.parsed()?;
        let visible = if expanded { parsed.total_rows } else { 4.min(parsed.total_rows) };
        let start = parsed.total_rows - visible;
        Ok(ToolOutputPreview {
            row: parsed.row[start..parsed.total_rows].iter()
                .map(|range| &parsed.display[range.clone()]).collect(),
            hidden_rows: parsed.total_rows.saturating_sub(visible), total_rows: parsed.total_rows,
        })
    }

}

impl ToolOutputSnapshot {
    #[cfg(test)]
    pub fn retained_bytes(&self) -> usize {
        self.saved.len() + self.display.capacity()
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
        for range in self.row[self.loaded_rows..self.total_rows].iter().take(row_limit) {
            let text = &self.display[range.clone()];
            if bytes + text.len() + 1 > byte_limit {
                if row.is_empty() {
                    let notice = "… [line truncated; export for full output]";
                    let mut end = byte_limit - notice.len() - 1;
                    while !text.is_char_boundary(end) { end -= 1; }
                    row.push(format!("{}{notice}", &text[..end]));
                }
                break;
            }
            bytes += text.len() + 1;
            row.push(text.to_owned());
        }
        Ok(Some(ToolOutputBatch {
            call_id: self.call_id.clone(),
            start_row: self.loaded_rows,
            complete: self.loaded_rows + row.len() == self.total_rows,
            byte_limit,
            row,
        }))
    }

    /// Advances only after the owning native document accepts this exact batch.
    pub fn accept_batch(&mut self, batch: &ToolOutputBatch) -> Result<()> {
        let expected = self.next_batch(batch.row.len(), batch.byte_limit)?;
        ensure!(expected.as_ref() == Some(batch), "tool batch differs from saved output or cursor");
        self.loaded_rows += batch.row.len();
        Ok(())
    }

    pub fn export_saved_output(&self, directory: &Path) -> Result<OwnedToolExport> {
        OwnedToolExport::create(directory, &self.saved)
    }
}

pub struct OwnedToolExport {
    file: Option<File>,
    path: Option<PathBuf>,
}

impl OwnedToolExport {
    pub fn create(directory: &Path, saved: &str) -> Result<Self> {
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
            file: Some(file),
            path: Some(path),
        };
        let output = export.file.as_mut().expect("created export");
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
            eprintln!("Forge tool export cleanup failed: {error:#}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_output_loads_only_requested_prefix_and_retains_unicode_boundaries() -> Result<()> {
        let directory=tempfile::tempdir()?;
        let path=directory.path().join("check.output");
        let saved="αβγ output\n".repeat(20_000);
        std::fs::write(&path,&saved)?;
        let mut source=ToolOutputView::file("check".into(),path,saved.len() as u64)?;
        assert!(source.saved.is_empty());
        assert!(source.has_unloaded_output());
        source.load_prefix(1024)?;
        assert!(source.saved.len()<=1024);
        assert!(saved.starts_with(source.saved.as_str()));
        source.load_prefix(2048)?;
        assert!(source.saved.len()<=2048);
        assert!(saved.starts_with(source.saved.as_str()));
        source.load_prefix(saved.len())?;
        assert_eq!(source.saved.as_str(),saved);
        assert!(!source.has_unloaded_output());
        Ok(())
    }

    #[test]
    fn oversized_line_is_abbreviated_in_a_batch_without_losing_saved_output() -> Result<()> {
        let saved = format!("{}\nlast\n", "λ".repeat(80_000));
        let source = ToolOutputView::new("call".into(), &saved)?;
        let mut output = source.snapshot()?;
        output.expand();
        let batch = output.next_batch(256, 65536)?.unwrap();
        assert!(batch.row[0].ends_with("[line truncated; export for full output]"));
        assert!(batch.row.iter().map(|row| row.len() + 1).sum::<usize>() <= 65536);
        output.accept_batch(&batch)?;
        let last = output.next_batch(256, 65536)?.unwrap();
        assert_eq!(last.row, ["last"]);
        output.accept_batch(&last)?;
        assert!(output.next_batch(256, 65536)?.is_none());
        let directory = tempfile::tempdir()?;
        let export = output.export_saved_output(directory.path())?;
        assert_eq!(std::fs::read_to_string(export.path().unwrap())?, saved);
        Ok(())
    }

    #[test]
    fn large_output_truncates_display_rows_and_preserves_complete_export() -> Result<()> {
        let saved = format!("{}\n", "x".repeat(128)).repeat(DISPLAY_ROW_LIMIT + 1);
        assert!(saved.len() > 32 * 1024 * 1024);
        let mut source = ToolOutputView::new("call".into(), &saved)?;
        assert!(source.truncated()?);
        source.append("last output after truncation\n")?;
        let preview = source.preview(true)?;
        assert_eq!(preview.total_rows, DISPLAY_ROW_LIMIT + 1);
        assert!(preview.row.last().unwrap().contains("Display truncated"));
        let directory = tempfile::tempdir()?;
        let snapshot = source.snapshot()?;
        let export = snapshot.export_saved_output(directory.path())?;
        let complete = std::fs::read_to_string(export.path().unwrap())?;
        assert!(complete.starts_with(&saved));
        assert!(complete.ends_with("last output after truncation\n"));
        Ok(())
    }

    #[test]
    fn deferred_output_parses_once_on_demand_and_retains_incremental_state() {
        let mut output = ToolOutputView::new("call".into(), "first\n\u{1b}[3").unwrap();
        output.append("1msecond").unwrap();
        assert!(output.parsed.get().is_none(), "closed output must retain only raw bytes");
        assert_eq!(output.preview(true).unwrap().row, ["first", "second"]);
        output.append("\u{1b}[0m third\n").unwrap();
        assert_eq!(output.preview(true).unwrap().row, ["first", "second third"]);
    }

    #[test]
    fn output_snapshot_shares_storage_and_survives_fragmented_live_appends() {
        let mut output = ToolOutputView::new("call".into(), "first\n\u{1b}[3").unwrap();
        let mut snapshot = output.snapshot().unwrap();
        assert!(Arc::ptr_eq(&output.saved, &snapshot.saved));
        assert!(Arc::ptr_eq(&output.parsed().unwrap().display, &snapshot.display));
        assert!(Arc::ptr_eq(&output.parsed().unwrap().row, &snapshot.row));
        output.append("1msecond\u{1b}[0m\n").unwrap();
        snapshot.expand();
        assert_eq!(snapshot.next_batch(256, 65536).unwrap().unwrap().row, ["first"]);
        assert_eq!(output.preview(true).unwrap().row, ["first", "second"]);
        assert_eq!(snapshot.saved.as_str(), "first\n\u{1b}[3");
    }

    #[test]
    fn append_preserves_fragmented_ansi_partial_rows_and_trailing_blanks() {
        let mut output = ToolOutputView::new("call".into(), "").unwrap();
        for chunk in ["\u{1b}[3", "1mλ", "\r", "\n\n", "tail", " continued\u{1b}[", "0m\n\n"] {
            output.append(chunk).unwrap();
        }
        assert_eq!(output.preview(false).unwrap().row,vec!["λ","","tail continued"]);
        assert_eq!(output.preview(false).unwrap().total_rows,3);
        output.append("last").unwrap();
        assert_eq!(output.preview(true).unwrap().row,vec!["λ","","tail continued","","last"]);
        assert_eq!(output.preview(false).unwrap().hidden_rows,1);
    }

    #[test]
    fn preview_keeps_latest_four_lines_and_counts_earlier_lines() {
        for count in 0usize..=6 {
            let saved = (0..count).map(|index| format!("line {index}\n")).collect::<String>();
            let view = ToolOutputView::new("call".into(), &saved).unwrap();
            let preview = view.preview(false).unwrap();
            assert_eq!(preview.row, (count.saturating_sub(4)..count).map(|index| format!("line {index}")).collect::<Vec<_>>());
            assert_eq!(preview.hidden_rows, count.saturating_sub(4));
        }
    }

    #[test]
    fn preview_advances_with_fragmented_output_without_reparsing_history() -> Result<()> {
        let mut output = ToolOutputView::new("call".into(), "one\ntwo\nthree\nfour\nfive")?;
        assert_eq!(output.preview(false)?.row, ["two", "three", "four", "five"]);
        let parser = output.parsed()? as *const ParsedOutput;
        output.append(" continued\n\u{1b}[3")?;
        output.append("2msix\u{1b}[0m\n")?;
        assert_eq!(output.parsed()? as *const ParsedOutput, parser);
        assert_eq!(output.preview(false)?.row, ["three", "four", "five continued", "six"]);
        assert_eq!(output.preview(false)?.hidden_rows, 2);
        assert_eq!(output.preview(true)?.row, ["one", "two", "three", "four", "five continued", "six"]);
        Ok(())
    }

    #[test]
    fn one_expansion_delivers_complete_output_and_collapse_preserves_loaded_rows() {
        let saved = (0..10000)
            .map(|row| format!("output {row}\n"))
            .collect::<String>();
        let view = ToolOutputView::new("call".into(), &saved).unwrap();
        assert_eq!(view.preview(false).unwrap().hidden_rows, 9996);
        let mut view = view.snapshot().unwrap();
        view.expand();
        let first = view.next_batch(256, 65536).unwrap().unwrap();
        assert_eq!(view.next_batch(256, 65536).unwrap().unwrap().start_row, 0);
        view.accept_batch(&first).unwrap();
        view.collapse();
        assert!(view.next_batch(256, 65536).unwrap().is_none());
        view.expand();
        let mut delivered = first.row;
        while let Some(batch) = view.next_batch(256, 65536).unwrap() {
            assert!(batch.row.len() <= 256);
            assert!(batch.row.iter().map(|row| row.len() + 1).sum::<usize>() <= 65536);
            view.accept_batch(&batch).unwrap();
            delivered.extend(batch.row);
        }
        assert_eq!(delivered.join("\n") + "\n", saved);
    }

    #[test]
    fn export_keeps_complete_saved_bytes_and_cleans_its_owned_artifact() {
        let directory = tempfile::tempdir().unwrap();
        let saved = "\u{1b}[31mred\u{1b}[0m\r\n\0tail\n";
        let view = ToolOutputView::new("call".into(), saved).unwrap();
        let mut export = view.snapshot().unwrap().export_saved_output(directory.path()).unwrap();
        let path = export.path().unwrap().to_owned();
        assert_eq!(std::fs::read(&path).unwrap(), saved.as_bytes());
        export.close().unwrap();
        assert!(!path.exists());
        export.close().unwrap();
    }

    #[test]
    fn display_strips_ansi_and_normalizes_terminal_controls_without_changing_export() {
        let saved = "\u{1b}[31mred\u{1b}[0m\r\nprogress\rdone\n\0tail\n";
        let view = ToolOutputView::new("call".into(), saved).unwrap();
        let collapsed = view.preview(false).unwrap();

        assert_eq!(collapsed.row, vec!["red", "progressdone", "tail"]);
        assert_eq!(collapsed.hidden_rows, 0);
        assert_eq!(view.saved.as_ref(), saved);
    }

    #[test]
    fn rejected_delivery_does_not_advance_the_output_cursor() {
        let mut view = ToolOutputView::new("call".into(), "first\nlast\n").unwrap().snapshot().unwrap();
        view.expand();
        let mut batch = view.next_batch(1, 65536).unwrap().unwrap();
        batch.complete = true;
        assert!(view.accept_batch(&batch).is_err());
        assert_eq!(view.next_batch(1, 65536).unwrap().unwrap().start_row, 0);
        batch.complete = false;
        view.accept_batch(&batch).unwrap();
        assert_eq!(view.next_batch(1, 65536).unwrap().unwrap().start_row, 1);
    }
}
