//! A dedicated bounded input thread does not join Tokio's blocking shutdown pool.

use std::io::{self, Read};
use std::pin::Pin;
use std::task::{Context, Poll};

use tokio::io::{AsyncRead, ReadBuf};
use tokio::sync::mpsc;

pub struct ThreadInput {
    receiver: mpsc::Receiver<io::Result<Vec<u8>>>,
    current: Vec<u8>,
    offset: usize,
}

impl ThreadInput {
    /// Owns one process-input reader thread with two queued 64 KiB chunks.
    /// An OS read can remain blocked until process exit, but cannot delay runtime teardown.
    pub fn new(mut reader: impl Read + Send + 'static) -> io::Result<Self> {
        let (sender, receiver) = mpsc::channel(2);
        std::thread::Builder::new()
            .name("forge-input".into())
            .spawn(move || {
                loop {
                    let mut bytes = vec![0; 64 * 1024];
                    match reader.read(&mut bytes) {
                        Ok(0) => break,
                        Ok(count) => {
                            bytes.truncate(count);
                            if sender.blocking_send(Ok(bytes)).is_err() {
                                break;
                            }
                        }
                        Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                        Err(error) => {
                            let _ = sender.blocking_send(Err(error));
                            break;
                        }
                    }
                }
            })?;
        Ok(Self {
            receiver,
            current: Vec::new(),
            offset: 0,
        })
    }
}

impl AsyncRead for ThreadInput {
    fn poll_read(
        mut self: Pin<&mut Self>,
        context: &mut Context<'_>,
        output: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        if output.remaining() == 0 {
            return Poll::Ready(Ok(()));
        }
        if self.offset == self.current.len() {
            match self.receiver.poll_recv(context) {
                Poll::Pending => return Poll::Pending,
                Poll::Ready(None) => return Poll::Ready(Ok(())),
                Poll::Ready(Some(Err(error))) => return Poll::Ready(Err(error)),
                Poll::Ready(Some(Ok(bytes))) => {
                    self.current = bytes;
                    self.offset = 0;
                }
            }
        }
        let count = output.remaining().min(self.current.len() - self.offset);
        output.put_slice(&self.current[self.offset..self.offset + count]);
        self.offset += count;
        Poll::Ready(Ok(()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncReadExt;

    #[tokio::test]
    async fn preserves_bytes_across_bounded_chunks_and_eof() {
        let expected: Vec<u8> = (0..200_000).map(|index| (index % 251) as u8).collect();
        let mut input = ThreadInput::new(io::Cursor::new(expected.clone())).unwrap();
        let mut actual = Vec::new();
        input.read_to_end(&mut actual).await.unwrap();
        assert_eq!(actual, expected);
    }
}
