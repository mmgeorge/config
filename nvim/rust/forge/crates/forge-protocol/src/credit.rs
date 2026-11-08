//! Consumer credit acknowledges an exact cumulative prefix of published frames.

use std::collections::VecDeque;
use std::io;
use std::sync::Mutex;

use serde::Deserialize;
use tokio::sync::Notify;

use crate::{MAX_FRAME_BYTES, MAX_PENDING_FRAMES};

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
                if window.outstanding.len() < MAX_PENDING_FRAMES {
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
