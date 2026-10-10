use std::fs::{File, OpenOptions};
use std::io::Write;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, ensure};
use serde::Serialize;

const MAX_OUTPUT_BYTES: usize = 32 * 1024 * 1024;
const MAX_OUTPUT_ROWS: usize = 262_144;
const INLINE_CHUNK_BYTES: usize = 16 * 1024;

#[derive(Debug, Serialize)]
pub struct ToolOutputPreview<'a> {
    pub row: Vec<&'a str>,
    pub hidden_rows: usize,
    pub total_rows: usize,
}

#[derive(Debug, Serialize)]
pub struct ToolOutputBatch {
    pub call_id: String,
    pub start_row: usize,
    pub row: Vec<String>,
    pub complete: bool,
}

pub struct ToolOutputView {
    call_id: String,
    saved: Arc<String>,
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    parser: strip_ansi_escapes::Writer<OutputCollector>,
    collected: Arc<Mutex<Vec<u8>>>,
    total_rows: usize,
    heading: Option<crate::turn::ToolCall>,
    owner: String,
    pub(super) group: String,
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
    pub fn snapshot(&self) -> ToolOutputSnapshot {
        ToolOutputSnapshot {
            call_id: self.call_id.clone(),
            saved: Arc::clone(&self.saved),
            display: Arc::clone(&self.display),
            row: Arc::clone(&self.row),
            total_rows: self.total_rows,
            loaded_rows: 0,
            expanded: false,
        }
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
    pub(super) fn retain(&mut self, replacement: &Self) {
        self.heading = replacement.heading.clone();
        self.owner.clone_from(&replacement.owner);
        self.group.clone_from(&replacement.group);
    }

    /// Counts independent inline chunks without walking retained output rows.
    pub(super) fn inline_chunks(&self) -> usize {
        self.inline_bytes().div_ceil(INLINE_CHUNK_BYTES).max(1)
    }

    /// Returns the first chunk whose mutable tail can change after an append.
    pub(super) fn inline_tail(&self) -> usize {
        self.inline_bytes() / INLINE_CHUNK_BYTES
    }

    fn inline_bytes(&self) -> usize {
        self.total_rows.checked_sub(1).map_or(0, |index| self.row[index].end)
    }

    /// Borrows at most one byte window with UTF-8 boundaries retained across appends.
    pub(super) fn inline_chunk(&self, index: usize) -> Vec<&str> {
        let bytes = self.inline_bytes();
        let boundary = |offset: usize| {
            let mut offset = offset.min(bytes);
            while !self.display.is_char_boundary(offset) { offset += 1; }
            offset
        };
        let start = boundary(index * INLINE_CHUNK_BYTES);
        let end = boundary((index + 1) * INLINE_CHUNK_BYTES);
        self.display[start..end].split_terminator('\n').collect()
    }

    pub fn retained_bytes(&self) -> usize {
        self.group.len() + self.saved.len()
            + self.display.capacity()
            + self.row.capacity() * std::mem::size_of::<Range<usize>>()
    }

    pub fn new(call_id: String, saved: &str) -> Result<Self> {
        ensure!(
            !call_id.is_empty() && call_id.len() <= 256,
            "invalid tool call identity"
        );
        ensure!(
            saved.len() <= MAX_OUTPUT_BYTES,
            "tool output exceeds the 32 MiB view limit"
        );
        let collected = Arc::new(Mutex::new(Vec::new()));
        let mut view = Self {
            call_id,
            saved: Arc::new(String::new()),
            display: Arc::new(String::new()),
            row: Arc::new(Vec::new()),
            parser: strip_ansi_escapes::Writer::new(OutputCollector(collected.clone())),
            collected,
            total_rows: 0,
            heading: None,
            owner: String::new(),
            group: String::new(),
        };
        view.append(saved)?;
        Ok(view)
    }

    pub fn append(&mut self, delta: &str) -> Result<()> {
        ensure!(self.saved.len() + delta.len() <= MAX_OUTPUT_BYTES, "tool output exceeds the 32 MiB view limit");
        self.parser.write_all(delta.as_bytes())?;
        self.parser.flush()?;
        let normalized = String::from_utf8(std::mem::take(&mut *self.collected.lock()
            .map_err(|_| anyhow::anyhow!("tool output collector poisoned"))?))?;
        ensure!(self.display.len() + normalized.len() <= MAX_OUTPUT_BYTES, "tool display exceeds the 32 MiB view limit");
        Arc::make_mut(&mut self.saved).push_str(delta);
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
            }
        }
        if start < display.len() {
            row.push(start..display.len());
            self.total_rows = row.len();
        }
        ensure!(row.len() <= MAX_OUTPUT_ROWS, "tool output requires a complete file export beyond 262144 rows");
        Ok(())
    }

    pub fn preview(&self, expanded: bool) -> ToolOutputPreview<'_> {
        let visible = if expanded { self.total_rows } else { 4 };
        ToolOutputPreview {
            row: self.row.iter().take(visible.min(self.total_rows))
                .map(|range| &self.display[range.clone()]).collect(),
            hidden_rows: self.total_rows.saturating_sub(visible),
            total_rows: self.total_rows,
        }
    }
}

impl ToolOutputSnapshot {
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
        ensure!(
            (1..=256).contains(&row_limit) && (1..=65536).contains(&byte_limit),
            "invalid tool batch limits"
        );
        if !self.expanded || self.loaded_rows == self.total_rows {
            return Ok(None);
        }
        let mut row = Vec::new();
        let mut bytes = 0;
        for range in self.row[self.loaded_rows..self.total_rows].iter().take(row_limit) {
            let text = &self.display[range.clone()];
            if bytes + text.len() + 1 > byte_limit {
                ensure!(
                    !row.is_empty(),
                    "tool output row exceeds delivery capacity and requires complete file export"
                );
                break;
            }
            bytes += text.len() + 1;
            row.push(text.to_owned());
        }
        Ok(Some(ToolOutputBatch {
            call_id: self.call_id.clone(),
            start_row: self.loaded_rows,
            complete: self.loaded_rows + row.len() == self.total_rows,
            row,
        }))
    }

    /// Advances only after the owning native document accepts this exact batch.
    pub fn accept_batch(&mut self, batch: &ToolOutputBatch) -> Result<()> {
        ensure!(
            batch.call_id == self.call_id && batch.start_row == self.loaded_rows,
            "tool batch belongs to another cursor"
        );
        ensure!(
            !batch.row.is_empty()
                && batch.row.len() <= 256
                && batch.row.iter().map(|row| row.len() + 1).sum::<usize>() <= 65536
                && self.loaded_rows + batch.row.len() <= self.total_rows,
            "tool batch exceeds source rows"
        );
        ensure!(
            batch.complete == (self.loaded_rows + batch.row.len() == self.total_rows),
            "tool batch completion differs from saved output"
        );
        for (offset, text) in batch.row.iter().enumerate() {
            ensure!(
                text == &self.display[self.row[self.loaded_rows + offset].clone()],
                "tool batch differs from saved output"
            );
        }
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
        ensure!(
            saved.len() <= MAX_OUTPUT_BYTES,
            "tool export exceeds the 32 MiB artifact limit"
        );
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
    fn output_snapshot_shares_storage_and_survives_fragmented_live_appends() {
        let mut output = ToolOutputView::new("call".into(), "first\n\u{1b}[3").unwrap();
        let mut snapshot = output.snapshot();
        assert!(Arc::ptr_eq(&output.saved, &snapshot.saved));
        assert!(Arc::ptr_eq(&output.display, &snapshot.display));
        assert!(Arc::ptr_eq(&output.row, &snapshot.row));
        output.append("1msecond\u{1b}[0m\n").unwrap();
        snapshot.expand();
        assert_eq!(snapshot.next_batch(256, 65536).unwrap().unwrap().row, ["first"]);
        assert_eq!(output.preview(true).row, ["first", "second"]);
        assert_eq!(snapshot.saved.as_str(), "first\n\u{1b}[3");
    }

    #[test]
    fn append_preserves_fragmented_ansi_partial_rows_and_trailing_blanks() {
        let mut output = ToolOutputView::new("call".into(), "").unwrap();
        for chunk in ["\u{1b}[3", "1mλ", "\r", "\n\n", "tail", " continued\u{1b}[", "0m\n\n"] {
            output.append(chunk).unwrap();
        }
        assert_eq!(output.preview(false).row,vec!["λ","","tail continued"]);
        assert_eq!(output.preview(false).total_rows,3);
        output.append("last").unwrap();
        assert_eq!(output.preview(true).row,vec!["λ","","tail continued","","last"]);
        assert_eq!(output.preview(false).hidden_rows,1);
    }

    #[test]
    fn preview_keeps_first_four_lines_and_counts_only_remaining_lines() {
        for count in 0usize..=6 {
            let saved = (0..count).map(|index| format!("line {index}\n")).collect::<String>();
            let view = ToolOutputView::new("call".into(), &saved).unwrap();
            let preview = view.preview(false);
            assert_eq!(preview.row, (0..count.min(4)).map(|index| format!("line {index}")).collect::<Vec<_>>());
            assert_eq!(preview.hidden_rows, count.saturating_sub(4));
        }
    }

    #[test]
    fn one_expansion_delivers_complete_output_and_collapse_preserves_loaded_rows() {
        let saved = (0..10000)
            .map(|row| format!("output {row}\n"))
            .collect::<String>();
        let view = ToolOutputView::new("call".into(), &saved).unwrap();
        assert_eq!(view.preview(false).hidden_rows, 9996);
        let mut view = view.snapshot();
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
        let mut export = view.snapshot().export_saved_output(directory.path()).unwrap();
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
        let collapsed = view.preview(false);

        assert_eq!(collapsed.row, vec!["red", "progressdone", "tail"]);
        assert_eq!(collapsed.hidden_rows, 0);
        assert_eq!(view.saved.as_ref(), saved);
    }

    #[test]
    fn rejected_delivery_does_not_advance_the_output_cursor() {
        let mut view = ToolOutputView::new("call".into(), "first\nlast\n").unwrap().snapshot();
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
