//! Output admission bounds encoded storage before serialization starts.

use std::io::{self, Write};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicU64, Ordering},
};

use crate::message::{Message, RequestEvent, Response, SessionEvent};
use serde::Serialize;
use serde_json::json;
use tokio::sync::{mpsc, watch};

use crate::{MAX_FRAME_BYTES, MAX_PENDING_FRAMES, RESERVED_CONTROL_RECORDS};

#[derive(Default)]
struct Budget {
    bytes: usize,
    frames: usize,
    normal: usize,
}

type ResponseObserver = Arc<dyn Fn(&Response) -> io::Result<()> + Send + Sync>;

#[derive(Clone)]
pub struct MessageSender {
    sender: mpsc::Sender<EncodedFrame>,
    control_sender: mpsc::Sender<EncodedFrame>,
    budget: Arc<Mutex<Budget>>,
    failure: watch::Sender<Option<String>>,
    capacity: watch::Sender<()>,
    transfer: Arc<tokio::sync::Semaphore>,
    event_sequence: Arc<AtomicU64>,
    response_observer: Option<ResponseObserver>,
}

pub struct MessageReceiver {
    receiver: mpsc::Receiver<EncodedFrame>,
    control_receiver: mpsc::Receiver<EncodedFrame>,
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
    let (control_sender, control_receiver) = mpsc::channel(RESERVED_CONTROL_RECORDS);
    let (failure, failed) = watch::channel(None);
    let (capacity, _) = watch::channel(());
    (
        MessageSender {
            sender,
            control_sender,
            budget: Arc::new(Mutex::new(Budget::default())),
            failure,
            capacity,
            transfer: Arc::new(tokio::sync::Semaphore::new(2)),
            event_sequence: Arc::new(AtomicU64::new(0)),
            response_observer: None,
        },
        MessageReceiver {
            receiver,
            control_receiver,
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
    /// Get whether this output can no longer admit frames, without sending or recording a failure.
    pub fn is_closed(&self) -> bool {
        self.sender.is_closed() || self.failure.borrow().is_some()
    }

    /// Attempt one session-event frame without poisoning the connection when output capacity is unavailable.
    /// A caller retaining the event may retry; this method never waits or begins a multipart transfer.
    pub fn try_send_event(&self, event: &SessionEvent) -> io::Result<()> {
        crate::contract::validate("session", &event.event, &event.payload)
            .map_err(io::Error::other)?;
        self.try_admit(Message::Event(event.clone()), false, false)
    }

    /// Records a response before transport can fail, without changing event delivery.
    pub fn observe_responses(
        &self,
        observer: impl Fn(&Response) -> io::Result<()> + Send + Sync + 'static,
    ) -> Self {
        let mut sender = self.clone();
        sender.response_observer = Some(Arc::new(observer));
        sender
    }

    /// Use reserved capacity up to the frame ceiling and transfer larger responses losslessly.
    pub async fn send_control_response(&self, response: Response) -> io::Result<()> {
        let encoded = match encode(&response, MAX_FRAME_BYTES) {
            Ok(encoded) => encoded,
            Err(_) => return self.send_response(response).await,
        };
        if let Some(observer) = &self.response_observer {
            observer(&response)?;
        }
        self.send_encoded_wait(encoded, true).await
    }
    /// Transfers a session event atomically through bounded frames without discarding its payload.
    pub async fn send_event(&self, event: SessionEvent) -> io::Result<()> {
        crate::contract::validate("session", &event.event, &event.payload)
            .map_err(io::Error::other)?;
        let session_id = event.session_id.clone();
        let message = Message::Event(event);
        if let Ok(encoded) = encode(&message, MAX_FRAME_BYTES) {
            return self.send_encoded_wait(encoded, false).await;
        }
        let _permit = self.begin_transfer().await?;
        let transfer = crate::transfer::JsonTransfer::new(&message)?;
        let transfer_id = self
            .event_sequence
            .fetch_add(1, Ordering::Relaxed)
            .to_string();
        let completion =
            json!({"part_count":transfer.part_count(), "total_bytes":transfer.total_bytes()});
        for part in transfer {
            self.send_wait(Message::Event(SessionEvent {
                session_id: session_id.clone(),
                event: "event.part".into(),
                payload: json!({"transfer_id":transfer_id, "part":part}),
            }))
            .await?;
        }
        self.send_wait(Message::Event(SessionEvent {
            session_id,
            event: "event.complete".into(),
            payload: json!({"transfer_id":transfer_id, "part":completion}),
        }))
        .await
    }

    /// Records a terminal response before attempting synchronous transport admission.
    pub fn send_terminal_response(&self, response: Response) -> io::Result<()> {
        if let Some(observer) = &self.response_observer {
            observer(&response)?;
        }
        self.send_terminal(Message::Response(response))
    }

    /// Transfers a complete response without exceeding individual frame limits.
    pub async fn send_response(&self, response: Response) -> io::Result<()> {
        if let Some(observer) = &self.response_observer {
            observer(&response)?;
        }
        let request_id = response.id;
        let response = Message::Response(response);
        if let Ok(encoded) = encode(&response, MAX_FRAME_BYTES) {
            self.send_encoded_wait(encoded, false).await?;
            return Ok(());
        }
        let prepared = self.begin_transfer().await.and_then(|permit| {
            crate::transfer::JsonTransfer::new(&response).map(|transfer| (permit, transfer))
        });
        let (_permit, transfer) =
            match prepared {
                Ok(prepared) => prepared,
                Err(error) => {
                    self.send_wait(Message::Response(Response::failure_with_data(
                request_id,
                "result_encoding_failed",
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

    /// Waits for an encoding slot while preserving transport shutdown handling.
    pub async fn begin_transfer(&self) -> io::Result<tokio::sync::OwnedSemaphorePermit> {
        tokio::select! {
            permit = self.transfer.clone().acquire_owned() => permit.map_err(|_| {
                io::Error::new(io::ErrorKind::BrokenPipe, "result transfer admission closed")
            }),
            _ = self.sender.closed() => Err(io::Error::new(io::ErrorKind::BrokenPipe, "output receiver closed")),
            error = self.failed() => Err(error),
        }
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

    /// Admit one ordinary frame, or return `WouldBlock` without closing the transport.
    pub fn send(&self, message: impl Serialize) -> io::Result<()> {
        self.admit(message, false, false)
    }

    /// Admit one advisory frame, such as request progress, that a later frame supersedes.
    ///
    /// Saturation discards the frame instead of waiting, because a stalled consumer must not hold
    /// the producing work. Encoding and transport failures still return an error.
    pub fn send_progress(&self, message: impl Serialize) -> io::Result<()> {
        match self.send(message) {
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => Ok(()),
            result => result,
        }
    }

    /// Admit one reserved control frame, or return `WouldBlock` without closing the transport.
    pub fn send_control(&self, message: impl Serialize) -> io::Result<()> {
        self.admit(message, true, false)
    }

    /// Waits for reserved control capacity without treating congestion as transport failure.
    pub async fn send_control_wait(&self, message: impl Serialize) -> io::Result<()> {
        let mut capacity = self.capacity.subscribe();
        loop {
            match self.try_admit(&message, true, false) {
                Ok(()) => return Ok(()),
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => {}
                Err(error) => return Err(error),
            }
            tokio::select! {
                _ = capacity.changed() => {},
                error = self.failed() => return Err(error),
                _ = self.control_sender.closed() => return Err(io::Error::new(io::ErrorKind::BrokenPipe, "control output closed")),
            }
        }
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
        if let Err(error) = &result
            && error.kind() != io::ErrorKind::WouldBlock
        {
            self.record_failure(error);
        }
        result
    }

    /// Publish a terminal producer failure to waiting senders and the receiver.
    pub fn record_failure(&self, error: &io::Error) {
        self.failure.send_if_modified(|failure| {
            if failure.is_some() {
                return false;
            }
            *failure = Some(error.to_string());
            true
        });
    }

    /// Wait for a lane reservation while retaining the single successful encoding.
    async fn send_encoded_wait(&self, bytes: Vec<u8>, control: bool) -> io::Result<()> {
        let mut capacity = self.capacity.subscribe();
        loop {
            match self.reserve_frame(control, false) {
                Ok(mut frame) => {
                    self.set_frame_bytes(&mut frame, bytes);
                    return self.publish_frame(frame);
                }
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => {}
                Err(error) => return Err(error),
            }
            tokio::select! {
                _ = capacity.changed() => {},
                error = self.failed() => return Err(error),
                _ = self.sender.closed() => return Err(io::Error::new(io::ErrorKind::BrokenPipe, "output receiver closed")),
            }
        }
    }

    /// Reserve one queue entry before allocating its encoded contents.
    fn reserve_frame(&self, control: bool, terminal: bool) -> io::Result<EncodedFrame> {
        if let Some(failure) = self.failure.borrow().clone() {
            return Err(io::Error::new(io::ErrorKind::BrokenPipe, failure));
        }
        let limit = MAX_FRAME_BYTES;
        {
            let mut budget = self.budget.lock().expect("output budget lock");
            if budget.frames == MAX_PENDING_FRAMES
                || (control && budget.frames - budget.normal == RESERVED_CONTROL_RECORDS)
                || (!control && budget.normal == MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS)
            {
                return Err(io::Error::new(
                    io::ErrorKind::WouldBlock,
                    format!(
                        "output admission capacity exhausted: {} queued frames ({} normal), {} queued bytes, requested frame reservation {limit}",
                        budget.frames, budget.normal, budget.bytes
                    ),
                ));
            }
            budget.bytes += limit;
            budget.frames += 1;
            budget.normal += usize::from(!control);
        }
        Ok(EncodedFrame {
            bytes: Vec::new(),
            terminal,
            reservation: limit,
            control,
            budget: Arc::clone(&self.budget),
            capacity: self.capacity.clone(),
        })
    }

    /// Replace the provisional maximum reservation with the actual encoded length.
    fn set_frame_bytes(&self, frame: &mut EncodedFrame, bytes: Vec<u8>) {
        let mut budget = self.budget.lock().expect("output budget lock");
        budget.bytes -= frame.reservation - bytes.len();
        frame.reservation = bytes.len();
        frame.bytes = bytes;
    }

    /// Publish an already reserved frame to its FIFO lane.
    fn publish_frame(&self, frame: EncodedFrame) -> io::Result<()> {
        let sender = if frame.control {
            &self.control_sender
        } else {
            &self.sender
        };
        sender
            .try_send(frame)
            .map_err(|error| io::Error::new(io::ErrorKind::BrokenPipe, error.to_string()))
    }

    /// Encode only after queue admission succeeds, and release failed reservations by drop.
    fn try_admit(&self, message: impl Serialize, control: bool, terminal: bool) -> io::Result<()> {
        let mut frame = self.reserve_frame(control, terminal)?;
        let bytes = encode(&message, MAX_FRAME_BYTES)?;
        self.set_frame_bytes(&mut frame, bytes);
        self.publish_frame(frame)
    }
}

impl MessageReceiver {
    pub fn check(&self) -> io::Result<()> {
        match self.failure.borrow().clone() {
            Some(failure) => Err(io::Error::new(io::ErrorKind::BrokenPipe, failure)),
            None => Ok(()),
        }
    }

    /// Drain accepted frames after a producer failure without admitting more writes.
    pub fn try_drain(&mut self) -> io::Result<EncodedFrame> {
        if let Ok(frame) = self.control_receiver.try_recv() {
            return Ok(frame);
        }
        self.receiver.try_recv().map_err(|error| {
            let kind = match error {
                mpsc::error::TryRecvError::Empty => io::ErrorKind::WouldBlock,
                mpsc::error::TryRecvError::Disconnected => io::ErrorKind::UnexpectedEof,
            };
            io::Error::new(kind, error)
        })
    }

    pub fn try_recv(&mut self) -> io::Result<EncodedFrame> {
        self.check()?;
        if let Ok(frame) = self.control_receiver.try_recv() {
            return Ok(frame);
        }
        self.receiver.try_recv().map_err(|error| {
            if let Err(failure) = self.check() {
                return failure;
            }
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
        if let Ok(frame) = self.control_receiver.try_recv() {
            return Ok(Some(frame));
        }
        let frame = tokio::select! {
            biased;
            changed = self.failure.changed() => {
                if changed.is_ok() {
                    self.check()?;
                }
                self.receiver.recv().await
            }
            frame = self.control_receiver.recv() => match frame {
                Some(frame) => Some(frame),
                None => self.receiver.recv().await,
            },
            frame = self.receiver.recv() => frame,
        };
        self.check()?;
        Ok(frame)
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
                format!(
                    "protocol frame encoding requires at least {} bytes, frame budget is {} bytes",
                    self.bytes.len() + bytes.len(),
                    self.limit
                ),
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

    /// Large control records retain reserved progress when ordinary events fill their lane.
    #[tokio::test]
    async fn full_frame_control_response_overtakes_saturated_bulk_lane() {
        let (sender, mut receiver) = channel();
        for sequence in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(sequence).unwrap();
        }
        let expected = Response::success(73, "control".repeat(4096)).unwrap();
        sender
            .send_control_response(expected.clone())
            .await
            .unwrap();
        let frame = receiver.recv().await.unwrap().unwrap();
        assert!(frame.bytes().len() > 4096);
        let actual: Response = serde_json::from_slice(frame.bytes()).unwrap();
        assert_eq!(actual.result(), expected.result());
        receiver.check().unwrap();
    }

    /// Progress under saturation is discarded without poisoning the connection for later responses.
    #[tokio::test]
    async fn saturated_progress_is_discarded_without_failing_the_transport() {
        let (sender, mut receiver) = channel();
        for sequence in 0..MAX_PENDING_FRAMES - RESERVED_CONTROL_RECORDS {
            sender.send(sequence).unwrap();
        }
        sender.send_progress("discarded progress").unwrap();
        assert!(!sender.is_closed());
        let response = sender.send_response(Response::success(7, "answer").unwrap());
        tokio::pin!(response);
        assert!(
            poll_fn(|context| Poll::Ready(response.as_mut().poll(context)))
                .await
                .is_pending()
        );
        drop(receiver.recv().await.unwrap().unwrap());
        response.await.unwrap();
        receiver.check().unwrap();
        let mut last = None;
        while let Ok(frame) = receiver.try_recv() {
            last = Some(frame.bytes().to_vec());
        }
        let last: Response = serde_json::from_slice(&last.unwrap()).unwrap();
        assert_eq!(last.result(), Some(&serde_json::json!("answer")));
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
}
