//! JSON encodings split into bounded UTF-8 parts and produced one part at a time.

use std::io;

use serde::{Deserialize, Serialize};

use crate::MAX_SNAPSHOT_PART_BYTES;

const PAYLOAD_BYTES: usize = MAX_SNAPSHOT_PART_BYTES;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct JsonPart {
    pub sequence: usize,
    pub part_count: usize,
    pub total_bytes: usize,
    pub payload: String,
}

pub struct JsonTransfer {
    encoded: String,
    offset: usize,
    part_count: usize,
    sequence: usize,
}

impl JsonTransfer {
    pub fn new(value: &impl Serialize) -> io::Result<Self> {
        let mut encoded = serde_json::to_string(value).map_err(io::Error::other)?;
        encoded.push('\n');
        let mut part_count = 0;
        let mut start = 0;
        while start < encoded.len() {
            let mut end = (start + PAYLOAD_BYTES).min(encoded.len());
            while !encoded.is_char_boundary(end) {
                end -= 1;
            }
            part_count += 1;
            start = end;
        }
        Ok(Self {
            encoded,
            offset: 0,
            part_count,
            sequence: 0,
        })
    }

    pub fn total_bytes(&self) -> usize {
        self.encoded.len()
    }

    pub fn part_count(&self) -> usize {
        self.part_count
    }
}

impl Iterator for JsonTransfer {
    type Item = JsonPart;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset == self.encoded.len() {
            return None;
        }
        let start = self.offset;
        let mut end = (start + PAYLOAD_BYTES).min(self.encoded.len());
        while !self.encoded.is_char_boundary(end) {
            end -= 1;
        }
        let part = JsonPart {
            sequence: self.sequence,
            part_count: self.part_count(),
            total_bytes: self.total_bytes(),
            payload: self.encoded[start..end].to_owned(),
        };
        self.sequence += 1;
        self.offset = end;
        Some(part)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve exact chunk boundaries and multibyte content beyond one wire frame.
    #[test]
    fn oversized_json_uses_full_chunks_and_preserves_utf8() {
        let source = "\u{03bb}\u{1f642}".repeat(crate::MAX_FRAME_BYTES / 4);
        let transfer = JsonTransfer::new(&source).unwrap();
        assert!(transfer.total_bytes() > crate::MAX_FRAME_BYTES);
        let expected_count = transfer.part_count();
        let mut encoded = String::new();
        let mut actual_count = 0;
        for part in transfer {
            assert_eq!(part.sequence, actual_count);
            assert!(part.payload.len() <= MAX_SNAPSHOT_PART_BYTES);
            if actual_count + 1 < expected_count {
                assert!(part.payload.len() >= MAX_SNAPSHOT_PART_BYTES - 3);
            }
            assert!(crate::outbound::encode(&part, crate::MAX_FRAME_BYTES).is_ok());
            encoded.push_str(&part.payload);
            actual_count += 1;
        }
        assert_eq!(actual_count, expected_count);
        assert_eq!(serde_json::from_str::<String>(&encoded).unwrap(), source);
    }
}
