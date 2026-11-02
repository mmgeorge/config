use super::logs::LogSnapshot;
use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use std::collections::BTreeSet;
use std::io::{BufWriter, Read, Seek, SeekFrom, Write};
use std::num::NonZeroUsize;

const PAGE_BYTES: usize = 24 * 1024;

/// Retains an immutable result independently of later database writes and log rotation.
pub(super) struct InspectionPage {
    file: BufWriter<tempfile::NamedTempFile>,
    log: Option<LogPages>,
    pub(super) identity: String,
    pub(super) workspace: Vec<String>,
}

/// Owns unread log segments and immutable boundaries of batches already captured.
struct LogPages {
    snapshot: LogSnapshot,
    limit: NonZeroUsize,
    boundaries: BTreeSet<u64>,
}

impl InspectionPage {
    /// Create a buffered snapshot without issuing one file write per JSON token.
    pub(super) fn new(identity: String) -> Result<Self> {
        Ok(Self {
            file: BufWriter::new(tempfile::NamedTempFile::new()?),
            log: None,
            identity,
            workspace: Vec::new(),
        })
    }

    /// Retain the log reader so each cursor request captures only its next batch.
    pub(super) fn attach_log(&mut self, snapshot: LogSnapshot, limit: NonZeroUsize) {
        self.log = Some(LogPages {
            snapshot,
            limit,
            boundaries: BTreeSet::new(),
        });
    }

    /// Append one complete record without truncating its contents.
    pub(super) fn push(&mut self, value: Value) -> Result<()> {
        serde_json::to_writer(&mut self.file, &value)?;
        self.file.write_all(b"\n")?;
        Ok(())
    }

    /// Flush the snapshot before reading a complete page or a Unicode-safe fragment.
    pub(super) fn read(
        &mut self,
        offset: u64,
        mut check_active: impl FnMut() -> Result<()>,
    ) -> Result<Value> {
        self.file.flush()?;
        let mut length = self.file.get_ref().as_file().metadata()?.len();
        ensure!(offset <= length, "inspection cursor exceeds snapshot");
        if let Some(log) = &self.log {
            ensure!(
                offset == 0 || log.boundaries.contains(&offset),
                "invalid log page boundary"
            );
        }
        if offset == length
            && let Some(log) = &mut self.log
        {
            self.file.seek(SeekFrom::End(0))?;
            for _ in 0..log.limit.get() {
                let Some(record) = log.snapshot.next(&mut check_active)? else {
                    break;
                };
                serde_json::to_writer(&mut self.file, &crate::trace::inspection_record(&record))?;
                self.file.write_all(b"\n")?;
            }
            self.file.flush()?;
            length = self.file.get_ref().as_file().metadata()?.len();
            log.boundaries.insert(length);
        }
        let unread = self
            .log
            .as_ref()
            .is_some_and(|log| log.snapshot.has_remaining());
        let end = self
            .log
            .as_ref()
            .and_then(|log| {
                log.boundaries
                    .range((
                        std::ops::Bound::Excluded(offset),
                        std::ops::Bound::Unbounded,
                    ))
                    .next()
                    .copied()
            })
            .unwrap_or(length);
        let file = self.file.get_mut();
        let continuing = if offset > 0 {
            file.seek(SeekFrom::Start(offset - 1))?;
            let mut previous = [0];
            file.read_exact(&mut previous)?;
            previous[0] != b'\n'
        } else {
            false
        };
        file.seek(SeekFrom::Start(offset))?;
        let mut bytes = Vec::new();
        let page_bytes = if self.log.is_some() {
            usize::try_from(end - offset)?
        } else {
            PAGE_BYTES
        };
        file.as_file_mut()
            .take(u64::try_from(page_bytes.saturating_add(4))?.min(end - offset))
            .read_to_end(&mut bytes)?;
        let mut end = bytes.len().min(page_bytes);
        while std::str::from_utf8(&bytes[..end]).is_err() {
            end -= 1;
        }
        ensure!(
            bytes.is_empty() || end > 0,
            "inspection snapshot is not UTF-8"
        );
        let text =
            std::str::from_utf8(&bytes[..end]).context("inspection snapshot is not UTF-8")?;
        if continuing || (!text.is_empty() && !text.contains('\n')) {
            let fragment_end = text.find('\n').map_or(text.len(), |position| position + 1);
            let next = offset + fragment_end as u64;
            return Ok(
                json!({"fragment":&text[..fragment_end],"encoding":"json-record",
                "record_complete":text[..fragment_end].ends_with('\n'),
                "next_offset":(next < length || unread).then_some(next)}),
            );
        }
        let mut items = Vec::new();
        let mut position = offset;
        for line in text.split_inclusive('\n') {
            if !line.ends_with('\n') {
                break;
            }
            items.push(serde_json::from_str::<Value>(line)?);
            position += line.len() as u64;
        }
        Ok(json!({"items":items,"next_offset":(position < length || unread).then_some(position)}))
    }
}
