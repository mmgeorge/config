use super::super::AdapterError;
use serde::de::DeserializeOwned;

/// Maximum UTF-8 byte length of one event record, excluding its newline.
pub const RECORD_LIMIT: usize = 8 * 1024 * 1024;

/// Decode incrementally appended UTF-8 JSON Lines without accepting partial records.
#[derive(Default)]
pub struct JsonLines {
    pending: Vec<u8>,
}

impl JsonLines {
    /// Create an empty decoder for one append-only event stream.
    pub fn new() -> Self {
        Self::default()
    }

    /// Deliver complete records in order, bounding allocation before copying bytes.
    pub fn push<T: DeserializeOwned>(
        &mut self,
        bytes: &[u8],
        mut accept: impl FnMut(T) -> Result<(), AdapterError>,
    ) -> Result<(), AdapterError> {
        for segment in bytes.split_inclusive(|byte| *byte == b'\n') {
            let complete = segment.last() == Some(&b'\n');
            let fragment = if complete {
                &segment[..segment.len() - 1]
            } else {
                segment
            };
            if self.pending.len().saturating_add(fragment.len()) > RECORD_LIMIT + 1 {
                return Err(AdapterError::Protocol("event record exceeds 8 MiB".into()));
            }
            self.pending.extend_from_slice(fragment);
            if complete {
                let record = self.pending.strip_suffix(b"\r").unwrap_or(&self.pending);
                accept(decode(record)?)?;
                self.pending.clear();
            }
        }
        Ok(())
    }

    /// Reject an unterminated final record after the owner has drained the file.
    pub fn finish(self) -> Result<(), AdapterError> {
        if !self.pending.is_empty() {
            return Err(AdapterError::Protocol(
                "unterminated final event record".into(),
            ));
        }
        Ok(())
    }
}

/// Decode only JSON objects and preserve typed event validation errors.
pub(super) fn decode<T: DeserializeOwned>(record: &[u8]) -> Result<T, AdapterError> {
    if record.len() > RECORD_LIMIT {
        return Err(AdapterError::Protocol("event record exceeds 8 MiB".into()));
    }
    let first = record.iter().find(|byte| !byte.is_ascii_whitespace());
    if first != Some(&b'{') {
        return Err(AdapterError::Protocol(
            "event record must be a JSON object".into(),
        ));
    }
    serde_json::from_slice(record)
        .map_err(|error| AdapterError::Protocol(format!("invalid event record: {error}")))
}

/// Keep the largest valid UTF-8 prefix allowed by the remaining text budget.
pub(super) fn text_retain(text: &mut String, remaining: &mut usize) -> u64 {
    let original = text.len();
    let mut keep = original.min(*remaining);
    while !text.is_char_boundary(keep) {
        keep -= 1;
    }
    text.truncate(keep);
    *remaining -= keep;
    u64::try_from(original - keep).expect("retained text size fits u64")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::adapter::events::TestEvent;

    #[test]
    fn partial_utf8_and_crlf_records_are_delivered_once() {
        let bytes =
            "{\"event\":\"error\",\"message\":\"é\"}\r\n{\"event\":\"complete\"}\n".as_bytes();
        let mut decoder = JsonLines::new();
        let mut events = Vec::new();
        for byte in bytes {
            decoder
                .push::<TestEvent>(&[*byte], |event| {
                    events.push(event);
                    Ok(())
                })
                .unwrap();
        }
        decoder.finish().unwrap();
        assert_eq!(events.len(), 2);
        assert_eq!(
            events[0],
            TestEvent::Error {
                message: "é".into()
            }
        );
    }

    #[test]
    fn malformed_and_incomplete_streams_fail() {
        for record in [
            b"[]".as_slice(),
            b"null",
            b"{}",
            b"{\"event\":\"started\"}",
            b"{\"event\":\"error\",\"message\":\"\xff\"}",
        ] {
            assert!(decode::<TestEvent>(record).is_err());
        }
        let mut decoder = JsonLines::new();
        decoder.push::<TestEvent>(b"{", |_| Ok(())).unwrap();
        assert!(decoder.finish().is_err());
    }

    #[test]
    fn oversized_unterminated_records_fail_before_unbounded_accumulation() {
        let mut decoder = JsonLines::new();
        let block = vec![b' '; 1024 * 1024];
        for _ in 0..8 {
            decoder.push::<TestEvent>(&block, |_| Ok(())).unwrap();
        }
        assert!(decoder.push::<TestEvent>(&block, |_| Ok(())).is_err());
    }
}
