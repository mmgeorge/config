use anyhow::{Context, Result, ensure};
use serde::Deserialize;
use serde_json::Value;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{ErrorKind, Read, Seek, SeekFrom};
use std::path::Path;

/// Captures every retained segment's extent before reading a growing session log.
pub(super) struct LogSnapshot {
    segments: VecDeque<LogSegment>,
}

/// Reads captured bytes backwards without indexing or decoding the whole file.
struct LogSegment {
    file: File,
    remaining: u64,
    buffer: Vec<u8>,
    trailing_newline: bool,
    pending: bool,
}

/// Decode the physical envelope used when one logical record spans log segments.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TraceChunk {
    id: String,
    sequence: usize,
    part_count: usize,
    payload_hex: String,
}

impl LogSnapshot {
    /// Open the current log before older rotations, retaining their captured byte lengths.
    pub(super) fn open(current: &Path) -> Result<Option<Self>> {
        let mut segments = VecDeque::new();
        for path in std::iter::once(current.to_owned())
            .chain((1..=10).map(|generation| current.with_extension(format!("jsonl.{generation}"))))
        {
            let file = match File::open(&path) {
                Ok(file) => file,
                Err(error) if error.kind() == ErrorKind::NotFound => continue,
                Err(error) => return Err(error).context("open session log"),
            };
            let length = file.metadata()?.len();
            segments.push_back(LogSegment {
                file,
                remaining: length,
                buffer: Vec::new(),
                trailing_newline: true,
                pending: length > 0,
            });
        }
        Ok((!segments.is_empty()).then_some(Self { segments }))
    }

    /// Read the newest remaining record without following later appends.
    pub(super) fn next(
        &mut self,
        mut check_active: impl FnMut() -> Result<()>,
    ) -> Result<Option<Value>> {
        let Some(mut record) = self.next_physical(&mut check_active)? else {
            return Ok(None);
        };
        let Some(envelope) = record.get_mut("trace_chunk") else {
            return Ok(Some(record));
        };
        let chunk: TraceChunk =
            serde_json::from_value(envelope.take()).context("decode session log chunk")?;
        ensure!(
            chunk.part_count > 0 && chunk.sequence == chunk.part_count - 1,
            "session log record is incomplete after rotation"
        );
        let mut fragments =
            vec![hex::decode(&chunk.payload_hex).context("decode session log chunk bytes")?];
        for expected in (0..chunk.sequence).rev() {
            check_active()?;
            let mut next = self
                .next_physical(&mut check_active)?
                .context("session log record is incomplete after rotation")?;
            let envelope = next
                .get_mut("trace_chunk")
                .context("session log chunk sequence was interrupted")?;
            let next: TraceChunk =
                serde_json::from_value(envelope.take()).context("decode session log chunk")?;
            ensure!(
                next.id == chunk.id
                    && next.part_count == chunk.part_count
                    && next.sequence == expected,
                "session log chunk sequence does not match its retained record"
            );
            fragments
                .push(hex::decode(&next.payload_hex).context("decode session log chunk bytes")?);
        }
        let bytes: Vec<u8> = fragments.into_iter().rev().flatten().collect();
        crate::codec::from_str(
            std::str::from_utf8(&bytes).context("session log record is not UTF-8")?,
        )
        .context("decode reassembled session log record")
        .map(Some)
    }

    /// Read one captured physical line, advancing across rotated segments.
    fn next_physical(
        &mut self,
        mut check_active: impl FnMut() -> Result<()>,
    ) -> Result<Option<Value>> {
        while let Some(segment) = self.segments.front_mut() {
            check_active()?;
            let Some(line) = segment.next_line(&mut check_active)? else {
                self.segments.pop_front();
                continue;
            };
            return crate::codec::from_str(
                std::str::from_utf8(&line).context("session log is not UTF-8")?,
            )
            .context("decode session log record")
            .map(Some);
        }
        Ok(None)
    }

    /// Detect unread captured bytes without decoding or consuming another entry.
    pub(super) fn has_remaining(&self) -> bool {
        self.segments.iter().any(|segment| segment.pending)
    }
}

impl LogSegment {
    /// Assemble one reverse-read line while preserving UTF-8 across bounded I/O blocks.
    fn next_line(
        &mut self,
        mut check_active: impl FnMut() -> Result<()>,
    ) -> Result<Option<Vec<u8>>> {
        if !self.pending {
            return Ok(None);
        }
        let mut fragments = Vec::new();
        loop {
            check_active()?;
            if let Some(delimiter) = self.buffer.iter().rposition(|byte| *byte == b'\n') {
                fragments.push(self.buffer.split_off(delimiter + 1));
                self.buffer.truncate(delimiter);
                break;
            }
            if self.remaining == 0 {
                fragments.push(std::mem::take(&mut self.buffer));
                self.pending = false;
                break;
            }
            if !self.buffer.is_empty() {
                fragments.push(std::mem::take(&mut self.buffer));
            }
            let start = self.remaining.saturating_sub(64 * 1024);
            let mut block = vec![0; usize::try_from(self.remaining - start)?];
            self.file.seek(SeekFrom::Start(start))?;
            if let Err(error) = self.file.read_exact(&mut block) {
                ensure!(
                    error.kind() != ErrorKind::UnexpectedEof,
                    "session log was truncated during inspection; retry the read"
                );
                return Err(error).context("read session log");
            }
            self.remaining = start;
            if self.trailing_newline {
                if block.last() == Some(&b'\n') {
                    block.pop();
                }
                self.trailing_newline = false;
            }
            self.buffer = block;
        }
        Ok(Some(fragments.into_iter().rev().flatten().collect()))
    }
}
