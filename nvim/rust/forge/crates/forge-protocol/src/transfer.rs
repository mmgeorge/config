//! Bounded JSON encodings split at UTF-8 boundaries and produced one part at a time.

use std::io;

use serde::{Deserialize, Serialize};

use crate::{MAX_SNAPSHOT_BYTES, MAX_SNAPSHOT_PART_BYTES, outbound};

const PAYLOAD_BYTES: usize = MAX_SNAPSHOT_PART_BYTES / 2;

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
    boundary: Vec<usize>,
    sequence: usize,
}

impl JsonTransfer {
    pub fn new(value: &impl Serialize) -> io::Result<Self> {
        let encoded = String::from_utf8(outbound::encode(value, MAX_SNAPSHOT_BYTES)?)
            .map_err(io::Error::other)?;
        let mut boundary = vec![0];
        let mut start = 0;
        while start < encoded.len() {
            let mut end = (start + PAYLOAD_BYTES).min(encoded.len());
            while !encoded.is_char_boundary(end) {
                end -= 1;
            }
            boundary.push(end);
            start = end;
        }
        Ok(Self {
            encoded,
            boundary,
            sequence: 0,
        })
    }

    pub fn total_bytes(&self) -> usize {
        self.encoded.len()
    }

    pub fn part_count(&self) -> usize {
        self.boundary.len() - 1
    }
}

impl Iterator for JsonTransfer {
    type Item = JsonPart;

    fn next(&mut self) -> Option<Self::Item> {
        let start = *self.boundary.get(self.sequence)?;
        let end = *self.boundary.get(self.sequence + 1)?;
        let part = JsonPart {
            sequence: self.sequence,
            part_count: self.part_count(),
            total_bytes: self.total_bytes(),
            payload: self.encoded[start..end].to_owned(),
        };
        self.sequence += 1;
        Some(part)
    }
}
