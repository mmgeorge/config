use anyhow::{Context, Result, ensure};
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

impl LogSnapshot {
    /// Open the current log before older rotations, retaining their captured byte lengths.
    pub(super) fn open(current: &Path) -> Result<Option<Self>> {
        let mut segments = VecDeque::new();
        for path in std::iter::once(current.to_owned())
            .chain((1..=3).map(|generation| current.with_extension(format!("jsonl.{generation}"))))
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::io::Write;

    /// Exclude appends to every segment, including segments not yet visited.
    #[test]
    fn snapshot_does_not_follow_appends() -> Result<()> {
        let root = tempfile::tempdir()?;
        let current = root.path().join("session.jsonl");
        std::fs::write(current.with_extension("jsonl.1"), "{\"count\":1}\n")?;
        std::fs::write(&current, "{\"count\":2}\n")?;
        let mut snapshot = LogSnapshot::open(&current)?.unwrap();
        let mut records = Vec::new();
        while let Some(record) = snapshot.next(|| Ok(()))? {
            records.push(record);
            writeln!(
                std::fs::OpenOptions::new().append(true).open(&current)?,
                "{{\"count\":3}}"
            )?;
        }
        assert_eq!(records, vec![json!({"count":2}), json!({"count":1})]);
        Ok(())
    }

    /// Reject a shortened segment instead of claiming its captured contents were complete.
    #[test]
    fn snapshot_reports_truncation() -> Result<()> {
        let root = tempfile::tempdir()?;
        let current = root.path().join("session.jsonl");
        std::fs::write(&current, "{\"count\":1}\n")?;
        let mut snapshot = LogSnapshot::open(&current)?.unwrap();
        std::fs::write(&current, "")?;
        let failure = snapshot.next(|| Ok(())).unwrap_err();
        assert!(failure.to_string().contains("truncated during inspection"));
        Ok(())
    }

    /// Stop before consuming another record when the owning interaction is cancelled.
    #[test]
    fn snapshot_honors_cancellation_between_records() -> Result<()> {
        let root = tempfile::tempdir()?;
        let current = root.path().join("session.jsonl");
        std::fs::write(&current, "{}\n{}\n")?;
        let mut snapshot = LogSnapshot::open(&current)?.unwrap();
        assert_eq!(snapshot.next(|| Ok(()))?, Some(json!({})));
        let failure = snapshot.next(|| anyhow::bail!("cancelled")).unwrap_err();
        assert_eq!(failure.to_string(), "cancelled");
        assert_eq!(snapshot.next(|| Ok(()))?, Some(json!({})));
        Ok(())
    }

    /// Read newest segments first and reconstruct records spanning multiple I/O blocks.
    #[test]
    fn snapshot_reads_newest_first_across_blocks_and_rotations() -> Result<()> {
        let root = tempfile::tempdir()?;
        let current = root.path().join("session.jsonl");
        for generation in 1..=3 {
            std::fs::write(
                current.with_extension(format!("jsonl.{generation}")),
                format!("{{\"generation\":{generation}}}\n"),
            )?;
        }
        let long = "界🙂".repeat(40_000);
        let records = [json!({"text":long}), json!({"event":"newest"})];
        std::fs::write(&current, format!("{}\n{}", records[0], records[1]))?;
        let mut snapshot = LogSnapshot::open(&current)?.unwrap();
        assert_eq!(snapshot.next(|| Ok(()))?, Some(records[1].clone()));
        assert_eq!(snapshot.next(|| Ok(()))?, Some(records[0].clone()));
        for generation in 1..=3 {
            assert_eq!(
                snapshot.next(|| Ok(()))?,
                Some(json!({"generation":generation}))
            );
        }
        assert!(!snapshot.has_remaining());
        assert_eq!(snapshot.next(|| Ok(()))?, None);
        Ok(())
    }

    /// Keep decoding lazy when an older retained segment is malformed.
    #[test]
    fn snapshot_defers_malformed_older_segments() -> Result<()> {
        let root = tempfile::tempdir()?;
        let current = root.path().join("session.jsonl");
        std::fs::write(current.with_extension("jsonl.1"), "invalid\n")?;
        std::fs::write(&current, "{\"event\":\"newest\"}\n")?;
        let mut snapshot = LogSnapshot::open(&current)?.unwrap();
        assert_eq!(snapshot.next(|| Ok(()))?, Some(json!({"event":"newest"})));
        assert!(snapshot.next(|| Ok(())).is_err());
        Ok(())
    }
}
