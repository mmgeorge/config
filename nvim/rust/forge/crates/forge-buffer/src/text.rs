//! Packed generated rows preserve zero rows and every trailing empty row.

use std::ops::Range;
use std::sync::Arc;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::ser::SerializeSeq;

use crate::ContractError;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BufferText {
    storage: Arc<TextStorage>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct TextStorage {
    text: String,
    row_offset: Vec<usize>,
}

impl BufferText {
    pub fn from_rows<Row: AsRef<str>>(
        rows: impl IntoIterator<Item = Row>,
    ) -> Result<Self, ContractError> {
        let mut result = TextStorage::default();
        for row in rows {
            let row = row.as_ref();
            if row.contains(['\n', '\0']) {
                return Err(ContractError("generated row contains a newline or NUL"));
            }
            result.row_offset.push(result.text.len());
            result.text.push_str(row);
            result.text.push('\n');
        }
        Ok(Self { storage: Arc::new(result) })
    }

    pub fn row_count(&self) -> usize {
        self.storage.row_offset.len()
    }

    pub fn byte_count(&self) -> usize {
        self.storage.text.len()
    }

    /// Charge the full retained storage even when immutable snapshots share it.
    pub fn allocated_bytes(&self) -> usize {
        self.storage.text.capacity() + self.storage.row_offset.capacity() * std::mem::size_of::<usize>()
    }

    pub fn row(&self, row: usize) -> Option<&str> {
        let start = *self.storage.row_offset.get(row)?;
        let end = self
            .storage.row_offset
            .get(row + 1)
            .copied()
            .unwrap_or(self.storage.text.len());
        Some(&self.storage.text[start..end - 1])
    }

    pub fn slice(&self, rows: Range<usize>) -> Result<Vec<&str>, ContractError> {
        if rows.start > rows.end || rows.end > self.row_count() {
            return Err(ContractError("row slice is outside text"));
        }
        Ok(rows
            .map(|row| self.row(row).expect("validated row"))
            .collect())
    }

    pub fn wire_rows(&self) -> Vec<&str> {
        (0..self.row_count())
            .map(|row| self.row(row).expect("stored row"))
            .collect()
    }
}

impl Serialize for BufferText {
    fn serialize<SerializerType: Serializer>(
        &self,
        serializer: SerializerType,
    ) -> Result<SerializerType::Ok, SerializerType::Error> {
        let mut sequence = serializer.serialize_seq(Some(self.row_count()))?;
        for row in 0..self.row_count() {
            sequence.serialize_element(self.row(row).expect("stored row"))?;
        }
        sequence.end()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snapshots_share_large_text_and_replacement_keeps_the_previous_rows() {
        let source = BufferText::from_rows(["x".repeat(2 * 1024 * 1024), String::new()]).unwrap();
        let snapshot = source.clone();
        assert!(Arc::ptr_eq(&source.storage, &snapshot.storage));
        assert_eq!(source.row(0).unwrap().as_ptr(), snapshot.row(0).unwrap().as_ptr());
        let replacement = BufferText::from_rows(["replacement"]).unwrap();
        drop(source);
        assert_eq!(snapshot.row_count(), 2);
        assert_eq!(snapshot.row(0).unwrap().len(), 2 * 1024 * 1024);
        assert_eq!(snapshot.row(1), Some(""));
        assert_eq!(replacement.row(0), Some("replacement"));
    }

    #[test]
    fn serialization_preserves_empty_and_trailing_rows() {
        for rows in [vec![], vec![""], vec!["hello", "", "world", ""]] {
            let text = BufferText::from_rows(&rows).unwrap();
            let encoded = serde_json::to_string(&text).unwrap();
            assert_eq!(encoded, serde_json::to_string(&rows).unwrap());
            let decoded: BufferText = serde_json::from_str(&encoded).unwrap();
            assert_eq!(decoded, text);
        }
    }
}

impl<'de> Deserialize<'de> for BufferText {
    fn deserialize<DeserializerType: Deserializer<'de>>(
        deserializer: DeserializerType,
    ) -> Result<Self, DeserializerType::Error> {
        let rows = Vec::<String>::deserialize(deserializer)?;
        Self::from_rows(rows).map_err(serde::de::Error::custom)
    }
}
