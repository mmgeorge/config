use std::fs::{File, OpenOptions};
use std::io::Write;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use serde::Serialize;

const MAX_OUTPUT_BYTES: usize = 32 * 1024 * 1024;
const MAX_OUTPUT_ROWS: usize = 262_144;

#[derive(Debug, Serialize)]
pub struct ToolOutputPreview<'a> {
    pub first: Option<&'a str>,
    pub last: Option<&'a str>,
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

#[derive(Clone)]
pub struct ToolOutputView {
    call_id: String,
    saved: Arc<str>,
    display: Arc<String>,
    row: Arc<Vec<Range<usize>>>,
    loaded_rows: usize,
    expanded: bool,
}

impl ToolOutputView {
    pub fn retained_bytes(&self) -> usize {
        self.saved.len()
            + self.display.capacity()
            + self.row.capacity() * std::mem::size_of::<Range<usize>>()
    }

    pub fn new(call_id: String, saved: Arc<str>) -> Result<Self> {
        ensure!(
            !call_id.is_empty() && call_id.len() <= 256,
            "invalid tool call identity"
        );
        ensure!(
            saved.len() <= MAX_OUTPUT_BYTES,
            "tool output exceeds the 32 MiB view limit"
        );
        let display = strip_ansi_escapes::strip_str(&saved)
            .replace("\r\n", "\n")
            .replace('\r', "")
            .replace('\0', "\\0");
        ensure!(
            display.len() <= MAX_OUTPUT_BYTES,
            "tool display exceeds the 32 MiB view limit"
        );
        let mut row = Vec::new();
        let mut start = 0;
        for (offset, byte) in display.bytes().enumerate() {
            if byte == b'\n' {
                ensure!(
                    row.len() < MAX_OUTPUT_ROWS,
                    "tool output requires a complete file export beyond 262144 rows"
                );
                row.push(start..offset);
                start = offset + 1;
            }
        }
        if start < display.len() {
            ensure!(
                row.len() < MAX_OUTPUT_ROWS,
                "tool output requires a complete file export beyond 262144 rows"
            );
            row.push(start..display.len());
        }
        while row.last().is_some_and(|range| range.is_empty()) {
            row.pop();
        }
        Ok(Self {
            call_id,
            saved,
            display: Arc::new(display),
            row: Arc::new(row),
            loaded_rows: 0,
            expanded: false,
        })
    }

    pub fn collapsed(&self) -> ToolOutputPreview<'_> {
        ToolOutputPreview {
            first: self.row.first().map(|range| &self.display[range.clone()]),
            last: (self.row.len() > 1)
                .then(|| &self.display[self.row.last().expect("nonempty rows").clone()]),
            hidden_rows: self.row.len().saturating_sub(2),
            total_rows: self.row.len(),
        }
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
        if !self.expanded || self.loaded_rows == self.row.len() {
            return Ok(None);
        }
        let mut row = Vec::new();
        let mut bytes = 0;
        for range in self.row[self.loaded_rows..].iter().take(row_limit) {
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
            complete: self.loaded_rows + row.len() == self.row.len(),
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
                && self.loaded_rows + batch.row.len() <= self.row.len(),
            "tool batch exceeds source rows"
        );
        ensure!(
            batch.complete == (self.loaded_rows + batch.row.len() == self.row.len()),
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
    fn one_expansion_delivers_complete_output_and_collapse_preserves_loaded_rows() {
        let saved = (0..10000)
            .map(|row| format!("output {row}\n"))
            .collect::<String>();
        let mut view = ToolOutputView::new("call".into(), Arc::from(saved.as_str())).unwrap();
        assert_eq!(view.collapsed().hidden_rows, 9998);
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
        let view = ToolOutputView::new("call".into(), Arc::from(saved)).unwrap();
        let mut export = view.export_saved_output(directory.path()).unwrap();
        let path = export.path().unwrap().to_owned();
        assert_eq!(std::fs::read(&path).unwrap(), saved.as_bytes());
        export.close().unwrap();
        assert!(!path.exists());
        export.close().unwrap();
    }

    #[test]
    fn display_strips_ansi_and_normalizes_terminal_controls_without_changing_export() {
        let saved = "\u{1b}[31mred\u{1b}[0m\r\nprogress\rdone\n\0tail\n";
        let view = ToolOutputView::new("call".into(), Arc::from(saved)).unwrap();
        let collapsed = view.collapsed();

        assert_eq!(collapsed.first, Some("red"));
        assert_eq!(collapsed.last, Some("tail"));
        assert_eq!(collapsed.hidden_rows, 1);
        assert_eq!(view.saved.as_ref(), saved);
    }

    #[test]
    fn rejected_delivery_does_not_advance_the_output_cursor() {
        let mut view = ToolOutputView::new("call".into(), Arc::from("first\nlast\n")).unwrap();
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
