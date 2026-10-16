use std::io;

use tokio::io::{AsyncBufRead, AsyncBufReadExt};

pub struct JsonLineReader<Input> {
    input: Input,
    frame: Vec<u8>,
    failed: bool,
}

impl<Input: AsyncBufRead + Unpin> JsonLineReader<Input> {
    pub fn new(input: Input) -> Self {
        Self {
            input,
            frame: Vec::new(),
            failed: false,
        }
    }

    /// Reads one frame without retaining bytes from subsequent frames.
    ///
    /// A final frame at EOF is
    /// returned once even without a newline. Cancellation retains a partial
    /// frame in this reader. An I/O error terminates its lifetime.
    pub async fn next_frame(&mut self) -> io::Result<Option<Vec<u8>>> {
        if self.failed {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Forge input is desynchronized",
            ));
        }
        loop {
            let available = match self.input.fill_buf().await {
                Ok(available) => available,
                Err(error) => {
                    self.failed = true;
                    self.frame.clear();
                    return Err(error);
                }
            };
            if available.is_empty() {
                return if self.frame.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(std::mem::take(&mut self.frame)))
                };
            }
            let newline = available.iter().position(|byte| *byte == b'\n');
            let count = newline.unwrap_or(available.len());
            self.frame.extend_from_slice(&available[..count]);
            self.input.consume(count + usize::from(newline.is_some()));
            if newline.is_some() {
                return Ok(Some(std::mem::take(&mut self.frame)));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tokio::io::{AsyncWriteExt, BufReader};

    use super::*;

    #[tokio::test]
    async fn separates_coalesced_frames_and_preserves_utf8_bytes() {
        let encoded = "{\"text\":\"🙂\"}\n{}\nlast".as_bytes();
        let mut reader = JsonLineReader::new(BufReader::with_capacity(1, encoded));
        assert_eq!(
            reader.next_frame().await.unwrap().unwrap(),
            "{\"text\":\"🙂\"}".as_bytes()
        );
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"{}");
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"last");
        assert!(reader.next_frame().await.unwrap().is_none());
        assert!(reader.next_frame().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn preserves_newline_delimited_and_empty_records() {
        let mut reader = JsonLineReader::new(BufReader::with_capacity(2, &b"1234\n\n"[..]));
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"1234");
        assert!(reader.next_frame().await.unwrap().unwrap().is_empty());
        assert!(reader.next_frame().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn preserves_complete_requests_beyond_output_frame_and_queue_budgets() {
        let body = "λ".repeat(8 * 1024 * 1024 + 1);
        let encoded = format!("{{\"text\":\"{body}\"}}\n{{}}\n");
        let mut reader = JsonLineReader::new(BufReader::with_capacity(64 * 1024, encoded.as_bytes()));
        let frame = reader.next_frame().await.unwrap().unwrap();
        let decoded: serde_json::Value = serde_json::from_slice(&frame).unwrap();
        assert_eq!(decoded["text"], body);
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"{}");
        assert!(reader.next_frame().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn cancelled_read_retains_partial_frame() {
        let (mut writer, stream) = tokio::io::duplex(64);
        let mut reader = JsonLineReader::new(BufReader::with_capacity(2, stream));
        writer.write_all(b"part").await.unwrap();
        tokio::select! {
            biased;
            result = reader.next_frame() => panic!("partial frame completed: {result:?}"),
            _ = tokio::task::yield_now() => {}
        }
        writer.write_all(b"ial\n").await.unwrap();
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"partial");
    }
}
