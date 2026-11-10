//! Line-framed stream-json transport with control-request correlation.
//!
//! The reader task delivers ordinary frames to the handler in stdout order, because item
//! normalization depends on that order. Inbound control requests are answered on separate tasks,
//! because a permission request can wait for the user while the CLI keeps streaming. Outbound
//! requests carry adapter-chosen ids and resolve when the matching `control_response` arrives or
//! the transport closes.

use super::wire::control::{
    ControlResponse, InboundControlRequest, OutboundControl, request_frame, response_frame,
};
use super::wire::stdout::{FrameDecodeError, StdoutFrame};
use async_trait::async_trait;
use forge_provider_api::ProviderError;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tokio::io::{AsyncBufReadExt, AsyncWrite, AsyncWriteExt, BufReader};
use tokio::sync::{Mutex, oneshot};
use tokio_util::sync::CancellationToken;

/// Readable and writable halves of one CLI process.
pub(crate) struct ProcessIo {
    pub stdout: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    pub stdin: Box<dyn AsyncWrite + Send + Unpin>,
    /// The owned process tree, absent for an in-memory peer.
    pub process: Option<Box<dyn process_wrap::tokio::ChildWrapper>>,
}

/// Receive frames and inbound requests from one transport.
#[async_trait]
pub(crate) trait FrameHandler: Send + Sync {
    /// Handle one ordinary frame; the reader waits for this before reading the next line.
    async fn frame(&self, frame: StdoutFrame);
    /// Answer one inbound request; `cancelled` fires when the CLI withdraws it.
    async fn control(
        &self,
        request: InboundControlRequest,
        cancelled: CancellationToken,
    ) -> Result<Value, String>;
    /// Observe the end of stdout, with the decode failure that ended it, if any.
    async fn closed(&self, failure: Option<TransportFailure>);
}

/// Classify why the transport could not complete an exchange.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub(crate) enum TransportFailure {
    #[error("Claude Code closed its output")]
    Closed,
    #[error("Claude Code input could not be written")]
    Write,
    #[error("Claude Code wrote an undecodable frame: {0}")]
    Decode(String),
    #[error("Claude Code rejected `{subtype}`: {message}")]
    Rejected {
        subtype: &'static str,
        message: String,
        code: Option<String>,
    },
}

impl From<FrameDecodeError> for TransportFailure {
    fn from(error: FrameDecodeError) -> Self {
        Self::Decode(error.to_string())
    }
}

/// Shared transport state owned by the reader task, request callers, and inbound handlers.
struct TransportShared {
    writer: Mutex<Option<Box<dyn AsyncWrite + Send + Unpin>>>,
    pending: std::sync::Mutex<HashMap<String, oneshot::Sender<ControlResponse>>>,
    inbound: std::sync::Mutex<HashMap<String, CancellationToken>>,
    closed: CancellationToken,
    /// Set when the owner replaces this process on purpose, so its exit is not a failure.
    retired: AtomicBool,
    next_request: AtomicU64,
}

impl TransportShared {
    /// Write one frame as a single line.
    async fn write(&self, frame: &Value) -> Result<(), TransportFailure> {
        let mut line = serde_json::to_vec(frame).map_err(|_| TransportFailure::Write)?;
        line.push(b'\n');
        let mut writer = self.writer.lock().await;
        let writer = writer.as_mut().ok_or(TransportFailure::Closed)?;
        writer.write_all(&line).await.map_err(|error| {
            tracing::error!(target: "forge::provider", event_name = "provider.write.failed",
                    provider = "claude", error = %error);
            TransportFailure::Write
        })?;
        writer.flush().await.map_err(|error| {
            tracing::error!(target: "forge::provider", event_name = "provider.flush.failed",
                provider = "claude", error = %error);
            TransportFailure::Write
        })
    }
}

/// One live stream-json connection.
pub(crate) struct ClaudeTransport {
    shared: Arc<TransportShared>,
    resources: Arc<TransportResources>,
}

impl ClaudeTransport {
    /// Start reading the process output and register its collection with the cleanup graph.
    pub(crate) fn start(
        io: ProcessIo,
        handler: Arc<dyn FrameHandler>,
        cleanup: &crate::resource_cleanup::ResourceCleanup,
    ) -> Self {
        let shared = Arc::new(TransportShared {
            writer: Mutex::new(Some(io.stdin)),
            pending: std::sync::Mutex::new(HashMap::new()),
            inbound: std::sync::Mutex::new(HashMap::new()),
            closed: CancellationToken::new(),
            retired: AtomicBool::new(false),
            next_request: AtomicU64::new(1),
        });
        let reader = tokio::spawn(read_frames(io.stdout, shared.clone(), handler));
        let resources = Arc::new(TransportResources {
            state: Mutex::new(ResourceState {
                process: io.process,
                reader: Some(reader),
                process_collected: false,
            }),
            shared: shared.clone(),
            collected: AtomicBool::new(false),
        });
        cleanup.retain_native(resources.clone());
        Self { shared, resources }
    }

    /// Write one user or control frame.
    pub(crate) async fn send(&self, frame: &Value) -> Result<(), TransportFailure> {
        self.shared.write(frame).await
    }

    /// Send one control request and wait for its response.
    pub(crate) async fn request(
        &self,
        request: OutboundControl,
    ) -> Result<Value, TransportFailure> {
        let subtype = request.subtype();
        let request_id = format!(
            "forge-{}",
            self.shared.next_request.fetch_add(1, Ordering::Relaxed)
        );
        let (sender, receiver) = oneshot::channel();
        self.shared
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(request_id.clone(), sender);
        if let Err(error) = self
            .shared
            .write(&request_frame(&request_id, &request))
            .await
        {
            self.shared
                .pending
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .remove(&request_id);
            return Err(error);
        }
        let response = tokio::select! {
            response = receiver => response.map_err(|_| TransportFailure::Closed)?,
            _ = self.shared.closed.cancelled() => return Err(TransportFailure::Closed),
        };
        response
            .outcome
            .map_err(|error| TransportFailure::Rejected {
                subtype,
                message: error.message,
                code: error.code,
            })
    }

    /// Report whether stdout has ended.
    pub(crate) fn is_closed(&self) -> bool {
        self.shared.closed.is_cancelled()
    }

    /// Shut down a process its owner is replacing, without reporting its exit to the handler.
    ///
    /// A restart for rewind or a resource change ends the old process deliberately; reporting
    /// that exit would invalidate the binding the replacement process continues.
    pub(crate) async fn retire(&self) -> Result<(), ProviderError> {
        self.shared.retired.store(true, Ordering::Release);
        self.shutdown().await
    }

    /// Close stdin so the CLI finishes and exits, then collect the process tree.
    pub(crate) async fn shutdown(&self) -> Result<(), ProviderError> {
        use crate::resource_cleanup::NativeResource;
        self.resources.shutdown().await
    }
}

/// Read stdout lines until the stream ends or a frame cannot be decoded.
async fn read_frames(
    stdout: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    shared: Arc<TransportShared>,
    handler: Arc<dyn FrameHandler>,
) {
    let mut lines = BufReader::new(stdout).lines();
    let failure = loop {
        let line = match lines.next_line().await {
            Ok(Some(line)) => line,
            Ok(None) => break None,
            Err(error) => {
                tracing::error!(target: "forge::provider", event_name = "provider.read.failed",
                    provider = "claude", error = %error);
                break Some(TransportFailure::Closed);
            }
        };
        if line.trim().is_empty() {
            continue;
        }
        let frame = match StdoutFrame::decode(&line) {
            Ok(frame) => frame,
            Err(error) => break Some(error.into()),
        };
        match frame {
            StdoutFrame::ControlResponse(response) => {
                let waiter = shared
                    .pending
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .remove(&response.request_id);
                if let Some(waiter) = waiter {
                    let _ = waiter.send(response);
                }
            }
            StdoutFrame::ControlRequest(request) => {
                let cancelled = CancellationToken::new();
                shared
                    .inbound
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .insert(request.request_id.clone(), cancelled.clone());
                let shared = shared.clone();
                let handler = handler.clone();
                tokio::spawn(async move {
                    let request_id = request.request_id.clone();
                    let outcome = handler.control(request, cancelled.clone()).await;
                    let withdrawn = shared
                        .inbound
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .remove(&request_id)
                        .is_none_or(|token| token.is_cancelled());
                    if !withdrawn {
                        let _ = shared.write(&response_frame(&request_id, outcome)).await;
                    }
                });
            }
            StdoutFrame::ControlCancelRequest { request_id } => {
                if let Some(token) = shared
                    .inbound
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .get(&request_id)
                {
                    token.cancel();
                }
            }
            frame => handler.frame(frame).await,
        }
    };
    shared.closed.cancel();
    shared
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clear();
    for (_, token) in shared
        .inbound
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .drain()
    {
        token.cancel();
    }
    if !shared.retired.load(Ordering::Acquire) {
        handler.closed(failure).await;
    }
}

/// Own the process tree and reader task until collection succeeds.
struct TransportResources {
    state: Mutex<ResourceState>,
    shared: Arc<TransportShared>,
    collected: AtomicBool,
}

struct ResourceState {
    process: Option<Box<dyn process_wrap::tokio::ChildWrapper>>,
    reader: Option<tokio::task::JoinHandle<()>>,
    process_collected: bool,
}

/// Bound the wait for a CLI to finish writing its transcript after stdin closes.
const EXIT_GRACE: std::time::Duration = std::time::Duration::from_secs(5);

#[async_trait]
impl crate::resource_cleanup::NativeResource for TransportResources {
    async fn shutdown(&self) -> Result<(), ProviderError> {
        let mut state = self.state.lock().await;
        // Closing stdin lets the CLI flush its transcript before the tree is terminated.
        if let Some(mut writer) = self.shared.writer.lock().await.take() {
            let _ = writer.shutdown().await;
        }
        if !state.process_collected {
            if let Some(process) = state.process.as_mut() {
                let exited = tokio::time::timeout(EXIT_GRACE, process.wait()).await;
                if !matches!(exited, Ok(Ok(_))) {
                    Box::into_pin(process.kill()).await.map_err(|_| {
                        ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::Shutdown,
                                forge_provider_api::ProviderFailureKind::Runtime,
                            )
                            .at_phase(
                                forge_provider_api::ProviderPhase::Collection,
                                forge_provider_api::FailureDisposition::Invalidated,
                            ),
                        )
                    })?;
                }
            }
            state.process_collected = true;
        }
        if let Some(mut reader) = state.reader.take() {
            // The exited process closes stdout, so the reader delivers its trailing frames and
            // reaches end of stream. A descendant that inherited the pipe can hold it open, so
            // the reader is aborted after the same grace.
            if tokio::time::timeout(EXIT_GRACE, &mut reader).await.is_err() {
                reader.abort();
                let _ = reader.await;
            }
        }
        self.shared.closed.cancel();
        self.collected.store(true, Ordering::Release);
        Ok(())
    }

    fn is_collected(&self) -> bool {
        self.collected.load(Ordering::Acquire)
    }
}

impl Drop for TransportResources {
    fn drop(&mut self) {
        if let Some(reader) = self.state.get_mut().reader.take() {
            reader.abort();
        }
    }
}
