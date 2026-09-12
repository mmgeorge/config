use std::io;

use tokio::io::{AsyncBufRead, AsyncBufReadExt};

use crate::MAX_FRAME_BYTES;

pub struct JsonLineReader<Input> {
    input: Input,
    frame: Vec<u8>,
    limit: usize,
    failed: bool,
}

impl<Input: AsyncBufRead + Unpin> JsonLineReader<Input> {
    pub fn new(input: Input) -> Self {
        Self::with_limit(input, MAX_FRAME_BYTES)
    }

    pub fn with_limit(input: Input, limit: usize) -> Self {
        Self {
            input,
            frame: Vec::new(),
            limit,
            failed: false,
        }
    }

    /// Reads one frame without retaining bytes from subsequent frames.
    ///
    /// The newline does not count toward the limit. A final frame at EOF is
    /// returned once even without a newline. Cancellation retains a partial
    /// frame in this reader. An I/O or size error terminates its lifetime.
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
            if count > self.limit.saturating_sub(self.frame.len()) {
                self.failed = true;
                self.frame.clear();
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Forge JSONL frame exceeds its byte limit",
                ));
            }
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
    async fn accepts_exact_limit_without_counting_the_newline() {
        let mut reader =
            JsonLineReader::with_limit(BufReader::with_capacity(2, &b"1234\n\n"[..]), 4);
        assert_eq!(reader.next_frame().await.unwrap().unwrap(), b"1234");
        assert!(reader.next_frame().await.unwrap().unwrap().is_empty());
        assert!(reader.next_frame().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn rejects_oversized_unterminated_input_and_poisoned_followup() {
        let mut reader =
            JsonLineReader::with_limit(BufReader::with_capacity(2, &b"12345\n{}\n"[..]), 4);
        assert_eq!(
            reader.next_frame().await.unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
        assert!(reader.frame.is_empty());
        assert_eq!(
            reader.next_frame().await.unwrap_err().kind(),
            io::ErrorKind::InvalidData
        );
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
