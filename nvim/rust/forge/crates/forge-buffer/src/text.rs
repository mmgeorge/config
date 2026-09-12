//! Packed generated rows preserve zero rows and every trailing empty row.

use std::ops::Range;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::ContractError;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BufferText {
    text: String,
    row_offset: Vec<usize>,
}

impl BufferText {
    pub fn from_rows<Row: AsRef<str>>(
        rows: impl IntoIterator<Item = Row>,
    ) -> Result<Self, ContractError> {
        let mut result = Self::default();
        for row in rows {
            let row = row.as_ref();
            if row.contains(['\n', '\0']) {
                return Err(ContractError("generated row contains a newline or NUL"));
            }
            result.row_offset.push(result.text.len());
            result.text.push_str(row);
            result.text.push('\n');
        }
        Ok(result)
    }

    pub fn row_count(&self) -> usize {
        self.row_offset.len()
    }

    pub fn byte_count(&self) -> usize {
        self.text.len()
    }

    pub fn row(&self, row: usize) -> Option<&str> {
        let start = *self.row_offset.get(row)?;
        let end = self
            .row_offset
            .get(row + 1)
            .copied()
            .unwrap_or(self.text.len());
        Some(&self.text[start..end - 1])
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
        self.wire_rows().serialize(serializer)
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
