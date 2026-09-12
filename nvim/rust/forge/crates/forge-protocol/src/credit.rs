//! Consumer credit acknowledges an exact cumulative prefix of published frames.

use std::collections::VecDeque;
use std::io;
use std::sync::Mutex;

use serde::Deserialize;
use tokio::sync::Notify;

use crate::{MAX_FRAME_BYTES, MAX_PENDING_FRAMES, MAX_QUEUED_BYTES};

const MAX_COUNTER: u64 = 9_007_199_254_740_991;

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Consumption {
    pub bytes: u64,
    pub frames: u64,
}

#[derive(Default)]
struct Window {
    outstanding: VecDeque<usize>,
    bytes: usize,
    consumed_bytes: u64,
    consumed_frames: u64,
    closed: bool,
}

#[derive(Default)]
pub struct ReceiveCredit {
    window: Mutex<Window>,
    available: Notify,
}

impl ReceiveCredit {
    pub async fn reserve(&self, bytes: usize) -> io::Result<()> {
        if bytes == 0 || bytes > MAX_FRAME_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid output credit size",
            ));
        }
        loop {
            {
                let mut window = self.window.lock().expect("receive credit lock");
                if window.closed {
                    return Err(io::Error::new(
                        io::ErrorKind::BrokenPipe,
                        "consumer credit closed",
                    ));
                }
                if window.consumed_bytes + window.bytes as u64 + bytes as u64 > MAX_COUNTER
                    || window.consumed_frames + window.outstanding.len() as u64 + 1 > MAX_COUNTER
                {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "consumer credit counter exhausted",
                    ));
                }
                if window.outstanding.len() < MAX_PENDING_FRAMES
                    && window.bytes + bytes <= MAX_QUEUED_BYTES
                {
                    window.outstanding.push_back(bytes);
                    window.bytes += bytes;
                    return Ok(());
                }
            }
            self.available.notified().await;
        }
    }

    pub fn grant(&self, consumed: Consumption) -> io::Result<()> {
        let mut window = self.window.lock().expect("receive credit lock");
        let invalid = || {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "consumer credit does not match published prefix",
            )
        };
        if consumed.bytes > MAX_COUNTER
            || consumed.frames > MAX_COUNTER
            || consumed.bytes < window.consumed_bytes
            || consumed.frames < window.consumed_frames
        {
            return Err(invalid());
        }
        let frames = consumed.frames - window.consumed_frames;
        if frames > window.outstanding.len() as u64 {
            return Err(invalid());
        }
        let bytes: usize = window.outstanding.iter().take(frames as usize).sum();
        if consumed.bytes - window.consumed_bytes != bytes as u64 {
            return Err(invalid());
        }
        for _ in 0..frames {
            window.outstanding.pop_front();
        }
        window.bytes -= bytes;
        window.consumed_bytes = consumed.bytes;
        window.consumed_frames = consumed.frames;
        drop(window);
        self.available.notify_one();
        Ok(())
    }

    pub fn close(&self) {
        self.window.lock().expect("receive credit lock").closed = true;
        self.available.notify_one();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn exhausted_record_credit_waits_for_exact_consumption() {
        let credit = ReceiveCredit::default();
        for _ in 0..MAX_PENDING_FRAMES {
            credit.reserve(10).await.unwrap();
        }
        let pending = credit.reserve(20);
        tokio::pin!(pending);
        assert!(matches!(
            std::future::poll_fn(|context| std::task::Poll::Ready(pending.as_mut().poll(context)))
                .await,
            std::task::Poll::Pending
        ));
        assert!(
            credit
                .grant(Consumption {
                    bytes: 11,
                    frames: 1
                })
                .is_err()
        );
        credit
            .grant(Consumption {
                bytes: 10,
                frames: 1,
            })
            .unwrap();
        pending.await.unwrap();
        credit
            .grant(Consumption {
                bytes: 10,
                frames: 1,
            })
            .unwrap();
        assert_eq!(
            credit.window.lock().unwrap().outstanding.len(),
            MAX_PENDING_FRAMES
        );
    }

    #[tokio::test]
    async fn byte_window_and_close_bound_a_slow_consumer() {
        let credit = ReceiveCredit::default();
        for _ in 0..MAX_QUEUED_BYTES / MAX_FRAME_BYTES {
            credit.reserve(MAX_FRAME_BYTES).await.unwrap();
        }
        let pending = credit.reserve(1);
        tokio::pin!(pending);
        assert!(matches!(
            std::future::poll_fn(|context| std::task::Poll::Ready(pending.as_mut().poll(context)))
                .await,
            std::task::Poll::Pending
        ));
        credit.close();
        assert!(pending.await.is_err());
    }
}
