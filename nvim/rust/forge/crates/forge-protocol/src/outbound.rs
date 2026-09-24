//! Output admission bounds encoded storage before serialization starts.

use std::io::{self, Write};
use std::sync::{Arc, Mutex};

use crate::message::{Message, RequestEvent, Response};
use serde::Serialize;
use serde_json::json;
use tokio::sync::{mpsc, watch};

use crate::{MAX_FRAME_BYTES, MAX_PENDING_FRAMES, MAX_QUEUED_BYTES, RESERVED_CONTROL_RECORDS};

pub const MAX_CONTROL_BYTES: usize = 4096;

#[derive(Default)]
struct Budget {
    bytes: usize,
    frames: usize,
    normal: usize,
}

#[derive(Clone)]
pub struct MessageSender {
    sender: mpsc::Sender<EncodedFrame>,
    budget: Arc<Mutex<Budget>>,
    failure: watch::Sender<Option<String>>,
    capacity: watch::Sender<()>,
    transfer: Arc<tokio::sync::Semaphore>,
}

pub struct MessageReceiver {
    receiver: mpsc::Receiver<EncodedFrame>,
    failure: watch::Receiver<Option<String>>,
}

pub struct EncodedFrame {
    bytes: Vec<u8>,
    terminal: bool,
    reservation: usize,
    control: bool,
    budget: Arc<Mutex<Budget>>,
    capacity: watch::Sender<()>,
}

struct LimitedWriter {
    bytes: Vec<u8>,
    limit: usize,
}

pub fn channel() -> (MessageSender, MessageReceiver) {
    let (sender, receiver) = mpsc::channel(MAX_PENDING_FRAMES);
    let (failure, failed) = watch::channel(None);
    let (capacity, _) = watch::channel(());
    (
        MessageSender {
            sender,
            budget: Arc::new(Mutex::new(Budget::default())),
            failure,
            capacity,
            transfer: Arc::new(tokio::sync::Semaphore::new(2)),
        },
        MessageReceiver {
            receiver,
            failure: failed,
        },
    )
}

pub fn encode(message: &impl Serialize, limit: usize) -> io::Result<Vec<u8>> {
    let mut output = LimitedWriter {
        bytes: Vec::new(),
        limit,
    };
    serde_json::to_writer(&mut output, message).map_err(io::Error::other)?;
    output.write_all(b"\n")?;
    Ok(output.bytes)
}

impl MessageSender {
    /// Transfer a complete response without exceeding individual frame limits.
    pub async fn send_response(&self, response: Response) -> io::Result<()> {
        let request_id = response.id;
        let response = Message::Response(response);
        if crate::outbound::encode(&response, crate::MAX_FRAME_BYTES).is_ok() {
            self.send_wait(response).await?;
            return Ok(());
        }
        let prepared = self.begin_transfer().and_then(|permit| {
            crate::transfer::JsonTransfer::new(&response).map(|transfer| (permit, transfer))
        });
        let (_permit, transfer) =
            match prepared {
                Ok(prepared) => prepared,
                Err(error) => {
                    self.send_wait(Message::Response(Response::failure_with_data(
                request_id,
                if error.kind() == std::io::ErrorKind::WouldBlock {
                    "result_transfer_busy"
                } else {
                    "result_too_large"
                },
                format!("Forge operation completed, but its result cannot be transferred: {error}"),
                json!({"operation_completed": true}),
            )))
            .await?;
                    return Ok(());
                }
            };
        let complete =
            json!({"part_count":transfer.part_count(), "total_bytes":transfer.total_bytes()});
        for part in transfer {
            self.send_wait(Message::RequestEvent(RequestEvent {
                request_id,
                event: "result.part".into(),
                payload: serde_json::to_value(part).map_err(io::Error::other)?,
            }))
            .await?;
            tokio::task::yield_now().await;
        }
        self.send_wait(Message::RequestEvent(RequestEvent {
            request_id,
            event: "result.complete".into(),
            payload: complete,
        }))
        .await?;
        Ok(())
    }

    /// Reserves one of two encoded transfers before retaining up to 16 MiB of JSON.
    pub fn begin_transfer(&self) -> io::Result<tokio::sync::OwnedSemaphorePermit> {
        self.transfer.clone().try_acquire_owned().map_err(|_| {
            io::Error::new(
                io::ErrorKind::WouldBlock,
                "result transfer admission is full",
            )
        })
    }

    /// Waits for output capacity while the caller retains ownership of the source value.
    ///
    /// Saturation does not poison the connection. Cancellation before admission sends nothing.
    pub async fn send_wait(&self, message: impl Serialize) -> io::Result<()> {
        let mut capacity = self.capacity.subscribe();
        loop {
            match self.try_admit(&message, false, false) {
                Ok(()) => return Ok(()),
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => {}
                Err(error) => {
                    self.record_failure(&error);
                    return Err(error);
                }
            }
            tokio::select! {
                _ = capacity.changed() => {},
                error = self.failed() => return Err(error),
                _ = self.sender.closed() => return Err(io::Error::new(io::ErrorKind::BrokenPipe, "output receiver closed")),
            }
        }
    }

    pub fn send(&self, message: impl Serialize) -> io::Result<()> {
        self.admit(message, false, false)
    }

    pub fn send_control(&self, message: impl Serialize) -> io::Result<()> {
        self.admit(message, true, false)
    }

    pub fn send_terminal(&self, message: impl Serialize) -> io::Result<()> {
        self.admit(message, false, true)
    }

    pub async fn failed(&self) -> io::Error {
        let mut failure = self.failure.subscribe();
        loop {
            if let Some(message) = failure.borrow().clone() {
                return io::Error::new(io::ErrorKind::BrokenPipe, message);
            }
            if failure.changed().await.is_err() {
                return io::Error::new(io::ErrorKind::BrokenPipe, "output failure channel closed");
            }
        }
    }

    fn admit(&self, message: impl Serialize, control: bool, terminal: bool) -> io::Result<()> {
        let result = self.try_admit(message, control, terminal);
        if let Err(error) = &result {
            self.record_failure(error);
        }
        result
    }

    fn record_failure(&self, error: &io::Error) {
        self.failure.send_if_modified(|failure| {
            if failure.is_some() {
                return false;
            }
            *failure = Some(error.to_string());
            true
        });
    }

    fn try_admit(&self, message: impl Serialize, control: bool, terminal: bool) -> io::Result<()> {
        if self.failure.borrow().is_some() {
            return Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "output connection is poisoned",
            ));
        }
        let limit = if control {
            MAX_CONTROL_BYTES
        } else {
            MAX_FRAME_BYTES
        };
        {
            let mut budget = self.budget.lock().expect("output budget lock");
            let byte_limit = if control {
                MAX_QUEUED_BYTES
            } else {
                MAX_QUEUED_BYTES - RESERVED_CONTROL_RECORDS * MAX_CONTROL_BYTES
            };
            if budget.frames == MAX_PENDING_FRAMES
                || (!control && budget.normal == MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS)
                || budget.bytes + limit > byte_limit
            {
                return Err(io::Error::new(
                    io::ErrorKind::WouldBlock,
                    "output admission budget exhausted",
                ));
            }
            budget.bytes += limit;
            budget.frames += 1;
            budget.normal += usize::from(!control);
        }
        let mut frame = EncodedFrame {
            bytes: Vec::new(),
            terminal,
            reservation: limit,
            control,
            budget: Arc::clone(&self.budget),
            capacity: self.capacity.clone(),
        };
        frame.bytes = encode(&message, limit)?;
        {
            let mut budget = self.budget.lock().expect("output budget lock");
            budget.bytes -= limit - frame.bytes.len();
            frame.reservation = frame.bytes.len();
        }
        self.sender
            .try_send(frame)
            .map_err(|error| io::Error::new(io::ErrorKind::BrokenPipe, error.to_string()))
    }
}

impl MessageReceiver {
    pub fn check(&self) -> io::Result<()> {
        match self.failure.borrow().clone() {
            Some(failure) => Err(io::Error::new(io::ErrorKind::BrokenPipe, failure)),
            None => Ok(()),
        }
    }

    pub fn try_recv(&mut self) -> io::Result<EncodedFrame> {
        self.check()?;
        self.receiver.try_recv().map_err(|error| {
            let kind = match error {
                mpsc::error::TryRecvError::Empty => io::ErrorKind::WouldBlock,
                mpsc::error::TryRecvError::Disconnected => io::ErrorKind::UnexpectedEof,
            };
            io::Error::new(kind, error)
        })
    }

    pub async fn recv(&mut self) -> io::Result<Option<EncodedFrame>> {
        if let Some(failure) = self.failure.borrow().clone() {
            return Err(io::Error::new(io::ErrorKind::BrokenPipe, failure));
        }
        tokio::select! {
            biased;
            changed = self.failure.changed() => {
                if changed.is_ok() {
                    return Err(io::Error::new(io::ErrorKind::BrokenPipe,
                        self.failure.borrow().clone().unwrap_or_else(|| "output failed".into())));
                }
                Ok(self.receiver.recv().await)
            }
            frame = self.receiver.recv() => Ok(frame),
        }
    }
}

impl EncodedFrame {
    pub fn is_terminal(&self) -> bool {
        self.terminal
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}

impl Drop for EncodedFrame {
    fn drop(&mut self) {
        let mut budget = self.budget.lock().expect("output budget lock");
        budget.bytes -= self.reservation;
        budget.frames -= 1;
        budget.normal -= usize::from(!self.control);
        drop(budget);
        self.capacity.send_replace(());
    }
}

impl Write for LimitedWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        if bytes.len() > self.limit - self.bytes.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "encoded frame exceeds its limit",
            ));
        }
        self.bytes.extend_from_slice(bytes);
        Ok(bytes.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::{Future, poll_fn};
    use std::task::Poll;

    #[tokio::test]
    async fn large_responses_transfer_losslessly_and_leave_the_connection_usable() {
        let (sender, mut receiver) = channel();
        let payload = "preview界\n".repeat(MAX_FRAME_BYTES / 8);
        let expected = Response::success(169, &payload).unwrap();
        sender.send_response(expected.clone()).await.unwrap();
        let mut encoded = String::new();
        loop {
            let frame = receiver.recv().await.unwrap().unwrap();
            assert!(frame.bytes().len() <= MAX_FRAME_BYTES);
            let event: RequestEvent = serde_json::from_slice(frame.bytes()).unwrap();
            assert_eq!(event.request_id, 169);
            if event.event == "result.complete" {
                break;
            }
            let part: crate::transfer::JsonPart = serde_json::from_value(event.payload).unwrap();
            encoded.push_str(&part.payload);
        }
        let restored: Response = serde_json::from_str(&encoded).unwrap();
        assert_eq!(restored.result(), expected.result());
        sender
            .send_response(Response::success(170, "next preview").unwrap())
            .await
            .unwrap();
        let next = receiver.recv().await.unwrap().unwrap();
        let response: Response = serde_json::from_slice(next.bytes()).unwrap();
        assert_eq!(response.id, 170);
        receiver.check().unwrap();
    }

    #[tokio::test]
    async fn waiting_sender_retains_fifo_and_waits_until_writer_releases_the_frame() {
        let (sender, mut receiver) = channel();
        for sequence in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(sequence).unwrap();
        }
        let waiting = sender.send_wait("after capacity");
        tokio::pin!(waiting);
        assert!(
            poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context)))
                .await
                .is_pending()
        );
        let held = receiver.recv().await.unwrap().unwrap();
        assert!(
            poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context)))
                .await
                .is_pending()
        );
        drop(held);
        waiting.await.unwrap();
        receiver.check().unwrap();
        for sequence in 1..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            let frame = receiver.recv().await.unwrap().unwrap();
            assert_eq!(
                serde_json::from_slice::<usize>(frame.bytes()).unwrap(),
                sequence
            );
        }
        let frame = receiver.recv().await.unwrap().unwrap();
        assert_eq!(
            serde_json::from_slice::<String>(frame.bytes()).unwrap(),
            "after capacity"
        );
    }

    #[tokio::test]
    async fn waiting_sender_fails_when_the_receiver_closes() {
        let (sender, receiver) = channel();
        for sequence in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(sequence).unwrap();
        }
        let waiting = sender.send_wait("pending");
        tokio::pin!(waiting);
        assert!(
            poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context)))
                .await
                .is_pending()
        );
        drop(receiver);
        assert_eq!(waiting.await.unwrap_err().kind(), io::ErrorKind::BrokenPipe);
    }

    #[test]
    fn encoded_transfer_admission_is_shared_by_sender_clones() {
        let (sender, _receiver) = channel();
        let first = sender.begin_transfer().unwrap();
        let _second = sender.clone().begin_transfer().unwrap();
        assert_eq!(
            sender.begin_transfer().unwrap_err().kind(),
            io::ErrorKind::WouldBlock
        );
        drop(first);
        let _permit = sender.begin_transfer().unwrap();
    }

    #[tokio::test]
    async fn reserved_control_capacity_preserves_fifo_order() {
        let (sender, mut receiver) = channel();
        for sequence in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(sequence).unwrap();
        }
        for sequence in 0..RESERVED_CONTROL_RECORDS {
            sender.send_control(sequence + 96).unwrap();
        }
        for sequence in 0..MAX_PENDING_FRAMES {
            let frame = receiver.recv().await.unwrap().unwrap();
            assert_eq!(frame.bytes(), format!("{sequence}\n").as_bytes());
        }
        assert_eq!(sender.budget.lock().unwrap().bytes, 0);
    }

    #[tokio::test]
    async fn oversized_serialization_releases_reservation_and_poison_connection() {
        let (sender, mut receiver) = channel();
        assert!(sender.send("x".repeat(MAX_FRAME_BYTES)).is_err());
        assert_eq!(sender.budget.lock().unwrap().bytes, 0);
        assert_eq!(sender.budget.lock().unwrap().frames, 0);
        assert!(receiver.recv().await.is_err());
        assert!(sender.send_control("later terminal response").is_err());
    }

    #[tokio::test]
    async fn received_frame_retains_budget_until_writer_finishes() {
        let (sender, mut receiver) = channel();
        sender.send("payload").unwrap();
        let frame = receiver.recv().await.unwrap().unwrap();
        assert_eq!(sender.budget.lock().unwrap().bytes, frame.bytes().len());
        drop(frame);
        assert_eq!(sender.budget.lock().unwrap().bytes, 0);
    }

    #[tokio::test]
    async fn byte_budget_rejects_admission_before_the_record_limit() {
        let (sender, mut receiver) = channel();
        let payload = "x".repeat(MAX_FRAME_BYTES - 3);
        for _ in 0..15 {
            sender.send(&payload).unwrap();
        }
        assert_eq!(sender.budget.lock().unwrap().bytes, 15 * MAX_FRAME_BYTES);
        assert!(sender.send(&payload).is_err());
        assert!(receiver.recv().await.is_err());
        drop(receiver);
        assert_eq!(sender.budget.lock().unwrap().bytes, 0);
    }

    #[tokio::test]
    async fn normal_record_saturation_poison_connection_before_control_can_overtake() {
        let (sender, mut receiver) = channel();
        for _ in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(0).unwrap();
        }
        assert!(sender.send(0).is_err());
        assert!(receiver.recv().await.is_err());
        assert!(sender.send_control("terminal").is_err());
    }
}
