//! Snapshot framing retains one bounded encoding and produces parts on demand.

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{MAX_SNAPSHOT_BYTES, MAX_SNAPSHOT_PART_BYTES, outbound};

    #[test]
    fn escaped_multibyte_payload_roundtrips_with_bounded_frames() {
        let snapshot = serde_json::json!({"document": "document", "revision": 7, "text": "🙂\\\"".repeat(100_000)});
        let transfer = SnapshotTransfer::new("document".into(), 7, 1, &snapshot).unwrap();
        let mut assembled = String::new();
        let mut total = 0;
        let mut count = 0;
        for (sequence, part) in transfer.enumerate() {
            assert_eq!(part.sequence, sequence);
            assert!(part.payload.len() <= MAX_SNAPSHOT_PART_BYTES);
            assert!(outbound::encode(&part, crate::MAX_FRAME_BYTES).is_ok());
            assembled.push_str(&part.payload);
            total = part.total_bytes;
            count = part.part_count;
        }
        assert!(count > 1);
        assert_eq!(assembled.len(), total);
        assert_eq!(
            serde_json::from_str::<serde_json::Value>(&assembled).unwrap(),
            snapshot
        );
    }

    #[test]
    fn rejects_transfer_over_total_byte_limit() {
        assert!(
            SnapshotTransfer::new("document".into(), 0, 1, &"x".repeat(MAX_SNAPSHOT_BYTES))
                .is_err()
        );
        assert!(SnapshotTransfer::new("document".into(), 0, 0, &()).is_err());
    }
}
