use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use std::io::{Read, Seek, SeekFrom, Write};

const PAGE_BYTES: usize = 24 * 1024;

/// Retains an immutable result independently of later database writes and log rotation.
pub(super) struct InspectionPage {
    file: tempfile::NamedTempFile,
    pub(super) identity: String,
    pub(super) workspace: Vec<String>,
}

impl InspectionPage {
    pub(super) fn new(identity: String) -> Result<Self> {
        Ok(Self {
            file: tempfile::NamedTempFile::new()?,
            identity,
            workspace: Vec::new(),
        })
    }

    pub(super) fn push(&mut self, value: Value) -> Result<()> {
        serde_json::to_writer(self.file.as_file_mut(), &value)?;
        self.file.write_all(b"\n")?;
        Ok(())
    }

    pub(super) fn read(&mut self, offset: u64) -> Result<Value> {
        let length = self.file.as_file().metadata()?.len();
        ensure!(offset <= length, "inspection cursor exceeds snapshot");
        let continuing = if offset > 0 {
            self.file.seek(SeekFrom::Start(offset - 1))?;
            let mut previous = [0];
            self.file.read_exact(&mut previous)?;
            previous[0] != b'\n'
        } else {
            false
        };
        self.file.seek(SeekFrom::Start(offset))?;
        let mut bytes = Vec::new();
        self.file
            .as_file_mut()
            .take((PAGE_BYTES + 4) as u64)
            .read_to_end(&mut bytes)?;
        let mut end = bytes.len().min(PAGE_BYTES);
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
                "next_offset":(next < length).then_some(next)}),
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
        Ok(json!({"items":items,"next_offset":(position < length).then_some(position)}))
    }
}
