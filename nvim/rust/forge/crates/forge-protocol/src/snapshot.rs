//! Snapshot framing retains one encoding and produces bounded parts on demand.

use std::io;

use serde::{Deserialize, Serialize};

use crate::transfer::JsonTransfer;

const MAX_COUNTER: u64 = 9_007_199_254_740_991;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnapshotPart {
    pub document: String,
    pub revision: u64,
    pub transfer: u64,
    pub sequence: usize,
    pub part_count: usize,
    pub total_bytes: usize,
    pub payload: String,
}

pub struct SnapshotTransfer {
    document: String,
    revision: u64,
    transfer: u64,
    parts: JsonTransfer,
}

impl SnapshotTransfer {
    pub fn new(
        document: String,
        revision: u64,
        transfer: u64,
        snapshot: &impl Serialize,
    ) -> io::Result<Self> {
        if document.is_empty()
            || document.len() > 256
            || document.chars().any(char::is_control)
            || revision > MAX_COUNTER
            || transfer == 0
            || transfer > MAX_COUNTER
        {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid snapshot transfer identity",
            ));
        }
        Ok(Self {
            document,
            revision,
            transfer,
            parts: JsonTransfer::new(snapshot)?,
        })
    }
}

impl Iterator for SnapshotTransfer {
    type Item = SnapshotPart;

    fn next(&mut self) -> Option<Self::Item> {
        let part = self.parts.next()?;
        let part = SnapshotPart {
            document: self.document.clone(),
            revision: self.revision,
            transfer: self.transfer,
            sequence: part.sequence,
            part_count: part.part_count,
            total_bytes: part.total_bytes,
            payload: part.payload,
        };
        Some(part)
    }
}
