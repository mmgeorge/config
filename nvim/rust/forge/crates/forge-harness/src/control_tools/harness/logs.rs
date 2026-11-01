use anyhow::{Context, Result, ensure};
use serde_json::Value;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Read, Take};
use std::path::Path;

/// Captures every retained segment's extent before reading a growing session log.
pub(super) struct LogSnapshot {
    segments: VecDeque<BufReader<Take<File>>>,
}

impl LogSnapshot {
    /// Open retained segments in chronological order with their current byte lengths.
    pub(super) fn open(current: &Path) -> Result<Option<Self>> {
        let mut segments = VecDeque::new();
        for path in (1..=3)
            .rev()
            .map(|generation| current.with_extension(format!("jsonl.{generation}")))
            .chain([current.to_owned()])
        {
            let file = match File::open(&path) {
                Ok(file) => file,
                Err(error) if error.kind() == ErrorKind::NotFound => continue,
                Err(error) => return Err(error).context("open session log"),
            };
            let length = file.metadata()?.len();
            segments.push_back(BufReader::new(file.take(length)));
        }
        Ok((!segments.is_empty()).then_some(Self { segments }))
    }

    /// Read one captured record without scanning later entries or following appends.
    pub(super) fn next(
        &mut self,
        mut check_active: impl FnMut() -> Result<()>,
    ) -> Result<Option<Value>> {
        let mut line = String::new();
        while let Some(segment) = self.segments.front_mut() {
            check_active()?;
            if segment.read_line(&mut line)? == 0 {
                ensure!(
                    segment.get_ref().limit() == 0,
                    "session log was truncated during inspection; retry the read"
                );
                self.segments.pop_front();
                continue;
            }
            return crate::codec::from_str(&line)
                .context("decode session log record")
                .map(Some);
        }
        Ok(None)
    }

    /// Detect unread captured bytes without decoding or consuming another entry.
    pub(super) fn has_remaining(&self) -> bool {
        self.segments
            .iter()
            .any(|segment| !segment.buffer().is_empty() || segment.get_ref().limit() > 0)
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
        assert_eq!(records, vec![json!({"count":1}), json!({"count":2})]);
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
}
