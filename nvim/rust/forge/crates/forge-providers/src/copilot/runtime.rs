use forge_provider_api::ProviderError;
use github_copilot_sdk::{CliProgram, Client, ClientOptions};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use tokio::sync::Mutex;

use super::handle::CopilotClient;

enum Shutdown {
    Idle,
    Running(tokio::task::JoinHandle<Result<(), std::sync::Arc<ProviderError>>>),
    Complete(Result<(), std::sync::Arc<ProviderError>>),
}

#[derive(Clone)]
enum RuntimeClient {
    Unvalidated(CopilotClient),
    Ready {
        client: super::handle::CopilotClient,
        _validation: forge_provider_api::version::ValidatedRuntime,
    },
}

impl RuntimeClient {
    /// Get the transport for resource collection regardless of validation state.
    fn client(&self) -> &CopilotClient {
        match self {
            Self::Unvalidated(client) => client,
            Self::Ready { client, .. } => client,
        }
    }
}

/// Owns the native client process and retains its collection task across waiter cancellation.
pub struct CopilotRuntime {
    command: Vec<String>,
    client: Mutex<Option<RuntimeClient>>,
    initializing: Mutex<()>,
    starts: AtomicU64,
    closed: AtomicBool,
    shutdown: Mutex<Shutdown>,
}

impl CopilotRuntime {
    pub fn new(command: Vec<String>) -> Self {
        Self {
            command,
            client: Mutex::new(None),
            initializing: Mutex::new(()),
            starts: AtomicU64::new(0),
            closed: AtomicBool::new(false),
            shutdown: Mutex::new(Shutdown::Idle),
        }
    }

    /// Get the version validated for the retained native process without probing or starting it.
    pub async fn validated_version(&self) -> Option<String> {
        match self.client.lock().await.as_ref() {
            Some(RuntimeClient::Ready { _validation, .. }) => {
                Some(_validation.version().to_string())
            }
            Some(RuntimeClient::Unvalidated(_)) | None => None,
        }
    }

    #[cfg(test)]
    pub fn start_count(&self) -> u64 {
        self.starts.load(Ordering::Relaxed)
    }

    pub fn close_admission(&self) {
        self.closed.store(true, Ordering::Release);
    }

    /// Bind an externally supplied transport before this runtime starts a client.
    #[cfg(test)]
    pub async fn attach_streams<R, W>(
        &self,
        reader: R,
        writer: W,
        workspace: PathBuf,
    ) -> Result<(), ProviderError>
    where
        R: tokio::io::AsyncRead + Unpin + Send + 'static,
        W: tokio::io::AsyncWrite + Unpin + Send + 'static,
    {
        let _initializing = self.initializing.lock().await;
        let mut client = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) || client.is_some() {
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        *client = Some(RuntimeClient::Unvalidated(CopilotClient::new(
            Client::from_streams(reader, writer, workspace).map_err(|_| {
                provider_failure(
                    forge_provider_api::error::ProviderOperation::TurnStart,
                    forge_provider_api::error::ProviderFailureKind::Transport,
                )
            })?,
        )));
        Ok(())
    }

    /// Start and validate one runtime while keeping transport ownership visible to shutdown.
    pub async fn client(
        &self,
        workspace: &str,
    ) -> Result<super::handle::CopilotClient, ProviderError> {
        let _initializing = self.initializing.lock().await;
        if self.closed.load(Ordering::Acquire) {
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        let current = self.client.lock().await.clone();
        let transport = match current {
            Some(RuntimeClient::Ready { client, .. }) => {
                client.ensure_healthy()?;
                return Ok(client);
            }
            Some(RuntimeClient::Unvalidated(client)) => client,
            None => {
                let mut options = ClientOptions::new().with_cwd(workspace);
                if let Some(program) = self.command.first() {
                    options = options.with_program(CliProgram::Path(PathBuf::from(program)));
                    if self.command.len() > 1 {
                        options = options.with_prefix_args(self.command.iter().skip(1));
                    }
                }
                let started =
                    CopilotClient::new(Client::start(options).await.map_err(|error| {
                        let kind = if matches!(
                            error.kind(),
                            github_copilot_sdk::ErrorKind::BinaryNotFound { .. }
                        ) {
                            forge_provider_api::error::ProviderFailureKind::Runtime
                        } else {
                            forge_provider_api::error::ProviderFailureKind::Transport
                        };
                        provider_failure(
                            forge_provider_api::error::ProviderOperation::ThreadOpen,
                            kind,
                        )
                    })?);
                self.starts.fetch_add(1, Ordering::Relaxed);
                if self.closed.load(Ordering::Acquire) {
                    started.force_stop();
                    return Err(provider_failure(
                        forge_provider_api::error::ProviderOperation::ThreadOpen,
                        forge_provider_api::error::ProviderFailureKind::Rejected,
                    ));
                }
                *self.client.lock().await = Some(RuntimeClient::Unvalidated(started.clone()));
                started
            }
        };
        let validation = match validate_version(&transport.inner, &self.command).await {
            Ok(validation) => validation,
            Err(error) => {
                transport.force_stop();
                self.client.lock().await.take();
                return Err(error);
            }
        };
        let mut state = self.client.lock().await;
        if self.closed.load(Ordering::Acquire) {
            transport.force_stop();
            state.take();
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        let client = transport;
        *state = Some(RuntimeClient::Ready {
            client: client.clone(),
            _validation: validation,
        });
        Ok(client)
    }

    /// Identify the failed cached client without starting or replacing any native work.
    pub async fn failed_generation(&self) -> Option<super::client_generation::FailedGeneration> {
        let client = self.client.lock().await.clone();
        match client {
            Some(RuntimeClient::Ready { client, .. }) => {
                client.generation.failed_after_fencing().await
            }
            Some(RuntimeClient::Unvalidated(_)) | None => None,
        }
    }

    /// Collect only the failed cached client after its affected callback owners are fenced.
    pub async fn collect_failed_client(
        &self,
        failed: &super::client_generation::FailedGeneration,
    ) -> Result<super::client_generation::CollectedClient, ProviderError> {
        let current = {
            let _initializing = self.initializing.lock().await;
            self.client.lock().await.clone()
        };
        match current {
            Some(RuntimeClient::Ready { client, .. }) if failed.matches(&client.generation) => {
                client.generation.collect(failed).await
            }
            Some(RuntimeClient::Ready { .. }) | Some(RuntimeClient::Unvalidated(_)) | None => {
                Err(provider_failure(
                    forge_provider_api::error::ProviderOperation::InputReconcile,
                    forge_provider_api::error::ProviderFailureKind::Rejected,
                ))
            }
        }
    }

    /// Retire the stopped cache entry only after all of its registered session work is joined.
    pub async fn finish_recovery(
        &self,
        sessions: super::client_generation::CollectedSessions,
    ) -> Result<(), ProviderError> {
        let _initializing = self.initializing.lock().await;
        let mut current = self.client.lock().await;
        match current.as_ref() {
            Some(RuntimeClient::Ready { client, .. })
                if sessions.failed_generation().matches(&client.generation) =>
            {
                current.take();
                Ok(())
            }
            Some(RuntimeClient::Ready { .. }) | Some(RuntimeClient::Unvalidated(_)) | None => {
                Err(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Rejected,
                ))
            }
        }
    }

    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.close_admission();
        let _initializing = self.initializing.lock().await;
        let mut shutdown = self.shutdown.lock().await;
        if matches!(*shutdown, Shutdown::Idle | Shutdown::Complete(Err(_))) {
            let client = self.client.lock().await.clone();
            *shutdown = Shutdown::Running(tokio::spawn(async move {
                match client {
                    Some(client) => client.client().stop().await.map_err(|_| {
                        std::sync::Arc::new(provider_failure(
                            forge_provider_api::error::ProviderOperation::Shutdown,
                            forge_provider_api::error::ProviderFailureKind::Transport,
                        ))
                    }),
                    None => Ok(()),
                }
            }));
        }
        if let Shutdown::Running(task) = &mut *shutdown {
            let result = task.await.unwrap_or_else(|_| {
                Err(std::sync::Arc::new(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                )))
            });
            *shutdown = Shutdown::Complete(result);
        }
        if let Shutdown::Complete(Err(error)) = &*shutdown {
            let kind = match error.as_ref() {
                ProviderError::Failure(failure) => failure.kind(),
                _ => forge_provider_api::error::ProviderFailureKind::Unknown,
            };
            return Err(provider_failure(
                forge_provider_api::error::ProviderOperation::Shutdown,
                kind,
            ));
        }
        self.client.lock().await.take();
        Ok(())
    }

    /// Create an independent cleanup owner using the same native command.
    ///
    /// The caller must collect this owner's client even when history deletion fails.
    pub(super) fn cleanup_runtime(&self) -> Self {
        Self::new(self.command.clone())
    }
}

fn provider_failure(
    operation: forge_provider_api::error::ProviderOperation,
    kind: forge_provider_api::error::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
        operation, kind,
    ))
}

/// Validate the actual native client before retained or isolated session creation.
pub(crate) async fn validate_version(
    client: &Client,
    command: &[String],
) -> Result<forge_provider_api::version::ValidatedRuntime, ProviderError> {
    let requirement = forge_provider_api::version::RuntimeRequirement::new(
        crate::registry::select("copilot").map_err(|_| {
            ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
                forge_provider_api::error::ProviderOperation::ThreadOpen,
                forge_provider_api::error::ProviderFailureKind::Unknown,
            ))
        })?,
        command
            .first()
            .map(PathBuf::from)
            .unwrap_or_else(|| "SDK-managed Copilot".into()),
        semver::Version::new(1, 0, 103),
    );
    let status = super::operation::wait(client.call("status.get", None))
        .await?
        .map_err(|_| {
            requirement.probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable)
        })?;
    Ok(requirement.validate(status.get("version").and_then(serde_json::Value::as_str))?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::version::RuntimeVersionError;
    use serde_json::{Value, json};
    use std::sync::{Arc, Mutex as StdMutex};
    use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

    enum StatusReply {
        Version(&'static str),
        Missing,
        Failed,
    }

    enum VersionOutcome {
        Supported,
        Unsupported,
        Missing,
        Malformed,
        Failed,
    }

    /// Retain every actual SDK request so rejection cannot hide a session dispatch.
    struct RuntimePeer {
        methods: Arc<StdMutex<Vec<String>>>,
        task: tokio::task::JoinHandle<()>,
    }

    impl RuntimePeer {
        /// Bind the public stream attachment to a protocol peer without launching a CLI.
        async fn attach(runtime: &CopilotRuntime, reply: StatusReply) -> Self {
            let (client_write, mut peer_read) = tokio::io::duplex(64 * 1024);
            let (mut peer_write, client_read) = tokio::io::duplex(64 * 1024);
            runtime
                .attach_streams(client_read, client_write, std::env::temp_dir())
                .await
                .unwrap();
            let methods = Arc::new(StdMutex::new(Vec::new()));
            let observed = methods.clone();
            let task = tokio::spawn(async move {
                while let Some(request) = read_frame(&mut peer_read).await {
                    let method = request["method"].as_str().unwrap();
                    observed.lock().unwrap().push(method.to_owned());
                    let response = match method {
                        "status.get" => match &reply {
                            StatusReply::Version(version) => json!({"result":{"version":version}}),
                            StatusReply::Missing => json!({"result":{}}),
                            StatusReply::Failed => {
                                json!({"error":{"code":-32000,"message":"status probe failed"}})
                            }
                        },
                        "session.create" => {
                            assert!(
                                matches!(reply, StatusReply::Version("1.0.103" | "1.0.104")),
                                "unsupported runtime reached session creation"
                            );
                            json!({"result":{"sessionId":"version-probe","workspacePath":std::env::temp_dir()}})
                        }
                        "session.options.update" => json!({"result":{"success":true}}),
                        "session.detach" => json!({"result":{"success":true}}),
                        method => panic!("unexpected SDK request during version gate: {method}"),
                    };
                    reply_frame(&mut peer_write, &request, response).await;
                }
            });
            Self { methods, task }
        }

        /// Join transport collection before inspecting the complete native request sequence.
        async fn collected(self) -> Vec<String> {
            self.task.await.unwrap();
            self.methods.lock().unwrap().clone()
        }
    }

    /// Read one actual SDK frame while treating a clean closed transport as completion.
    async fn read_frame(reader: &mut DuplexStream) -> Option<Value> {
        let mut header = Vec::new();
        loop {
            match reader.read_u8().await {
                Ok(byte) => header.push(byte),
                Err(error)
                    if header.is_empty() && error.kind() == std::io::ErrorKind::UnexpectedEof =>
                {
                    return None;
                }
                Err(error) => panic!("version fixture frame header failed: {error}"),
            }
            if header.ends_with(b"\r\n\r\n") {
                break;
            }
            assert!(header.len() < 1024, "unbounded version fixture header");
        }
        let size: usize = std::str::from_utf8(&header)
            .unwrap()
            .trim()
            .strip_prefix("Content-Length: ")
            .unwrap()
            .parse()
            .unwrap();
        assert!(size < 64 * 1024);
        let mut body = vec![0; size];
        reader.read_exact(&mut body).await.unwrap();
        Some(serde_json::from_slice(&body).unwrap())
    }

    /// Preserve the original RPC identity for a status or session response.
    async fn reply_frame(writer: &mut DuplexStream, request: &Value, mut response: Value) {
        response["jsonrpc"] = json!("2.0");
        response["id"] = request["id"].clone();
        let body = serde_json::to_vec(&response).unwrap();
        writer
            .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
            .await
            .unwrap();
        writer.write_all(&body).await.unwrap();
        writer.flush().await.unwrap();
    }

    /// Validate the adapter's real SDK gate before any create, resume, or send is possible.
    #[tokio::test]
    async fn attached_runtime_enforces_minimum_before_session_dispatch() {
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            for (reply, outcome) in [
                (StatusReply::Version("1.0.102"), VersionOutcome::Unsupported),
                (StatusReply::Version("1.0.103"), VersionOutcome::Supported),
                (StatusReply::Version("1.0.104"), VersionOutcome::Supported),
                (StatusReply::Missing, VersionOutcome::Missing),
                (StatusReply::Version("not-a-version"), VersionOutcome::Malformed),
                (StatusReply::Failed, VersionOutcome::Failed),
            ] {
                let runtime = CopilotRuntime::new(vec!["fixture-selected-copilot".into()]);
                let peer = RuntimePeer::attach(&runtime, reply).await;
                let result = runtime.client(".").await;
                match outcome {
                    VersionOutcome::Supported => {
                        let client = result.unwrap();
                        runtime.client(".").await.unwrap();
                        let prepared = client.inner.prepare_session(github_copilot_sdk::SessionConfig::default().with_session_id(github_copilot_sdk::SessionId::new("version-probe"))).unwrap();
                        let session = prepared.start().await.unwrap();
                        session.disconnect().await.unwrap();
                    }
                    VersionOutcome::Unsupported => {
                        let error = result.err().expect("unsupported runtime passed validation");
                        assert!(matches!(&error, ProviderError::RuntimeVersion(RuntimeVersionError::Unsupported { detected, .. }) if *detected == semver::Version::new(1, 0, 102)));
                        let diagnostic = error.to_string();
                        assert!(diagnostic.contains("fixture-selected-copilot") && diagnostic.contains("1.0.103"));
                    }
                    VersionOutcome::Missing => assert!(matches!(result, Err(ProviderError::RuntimeVersion(RuntimeVersionError::Missing { .. })))),
                    VersionOutcome::Malformed => assert!(matches!(result, Err(ProviderError::RuntimeVersion(RuntimeVersionError::Malformed { .. })))),
                    VersionOutcome::Failed => {
                        let Err(ProviderError::RuntimeVersion(RuntimeVersionError::Probe { requirement, failure })) = result else {
                            panic!("failed probe lost its typed runtime context");
                        };
                        assert_eq!(requirement.provider(), &crate::registry::select("copilot").unwrap());
                        assert_eq!(requirement.runtime(), std::path::Path::new("fixture-selected-copilot"));
                        assert_eq!(requirement.minimum(), &semver::Version::new(1, 0, 103));
                        let forge_provider_api::version::RuntimeProbeFailure::Unavailable = failure else {
                            panic!("remote rejection was classified as a timeout");
                        };
                    },
                }
                runtime.shutdown().await.unwrap();
                let methods = peer.collected().await;
                assert_eq!(methods.iter().filter(|method| method.as_str() == "status.get").count(), 1, "a ready transport repeated version validation");
                if matches!(outcome, VersionOutcome::Supported) {
                    assert_eq!(methods.first().map(String::as_str), Some("status.get"));
                    assert_eq!(methods.iter().filter(|method| method.as_str() == "session.create").count(), 1);
                } else {
                    assert_eq!(methods, ["status.get"], "rejected runtime dispatched native session work");
                }
                assert_eq!(runtime.start_count(), 0, "stream fixture launched an executable");
            }
        }).await.expect("runtime version gate fixture timed out");
    }

    /// Bind cached validation to one process rather than a later replacement transport.
    #[tokio::test]
    async fn replacement_runtime_reprobes_actual_transport_version() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let original = CopilotRuntime::new(vec!["same-configured-command".into()]);
            let peer = RuntimePeer::attach(&original, StatusReply::Version("1.0.103")).await;
            original.client(".").await.unwrap();
            original.client(".").await.unwrap();
            original.shutdown().await.unwrap();
            assert_eq!(peer.collected().await, ["status.get"]);

            let replacement = CopilotRuntime::new(vec!["same-configured-command".into()]);
            let peer = RuntimePeer::attach(&replacement, StatusReply::Version("1.0.102")).await;
            assert!(matches!(
                replacement.client(".").await,
                Err(ProviderError::RuntimeVersion(
                    RuntimeVersionError::Unsupported { .. }
                ))
            ));
            replacement.shutdown().await.unwrap();
            assert_eq!(
                peer.collected().await,
                ["status.get"],
                "replacement inherited a previous process's validation"
            );
            assert_eq!(replacement.start_count(), 0);
        })
        .await
        .expect("replacement version fixture timed out");
    }

    /// Prevent retained session handles from issuing detach RPCs after successful shutdown.
    #[tokio::test]
    async fn shutdown_marks_collected_generation_unavailable() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (reader, _peer_writer) = tokio::io::duplex(64);
        let (_peer_reader, writer) = tokio::io::duplex(64);
        runtime
            .attach_streams(reader, writer, PathBuf::from("."))
            .await
            .unwrap();
        let client = runtime
            .client
            .lock()
            .await
            .as_ref()
            .unwrap()
            .client()
            .clone();
        runtime.shutdown().await.unwrap();
        assert!(client.is_stopped());
        assert!(
            client.generation.is_collected(),
            "successful SDK stop did not publish positive collection proof"
        );
        assert!(runtime.client.lock().await.is_none());
    }

    /// Keep collection owned when a stopped transport's shutdown waiter disappears.
    #[tokio::test]
    async fn cancelled_stopped_generation_collection_retains_shutdown_task() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (reader, _writer) = tokio::io::duplex(64);
        let (_reader, writer) = tokio::io::duplex(64);
        runtime
            .attach_streams(reader, writer, PathBuf::from("."))
            .await
            .unwrap();
        let client = runtime
            .client
            .lock()
            .await
            .as_ref()
            .unwrap()
            .client()
            .clone();
        client.force_stop();
        let (release, released) = tokio::sync::oneshot::channel();
        let collecting = client.clone();
        *runtime.shutdown.lock().await = Shutdown::Running(tokio::spawn(async move {
            released.await.map_err(|_| {
                std::sync::Arc::new(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                ))
            })?;
            collecting.stop().await.map_err(|_| {
                std::sync::Arc::new(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Transport,
                ))
            })
        }));
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(1), runtime.shutdown())
                .await
                .is_err()
        );
        assert!(matches!(
            *runtime.shutdown.lock().await,
            Shutdown::Running(_)
        ));
        assert!(runtime.client.lock().await.is_some());
        assert!(
            !client.is_stopped(),
            "collection is not proven while the worker is pending"
        );
        release.send(()).unwrap();
        runtime.shutdown().await.unwrap();
        assert!(client.is_stopped());
        assert!(runtime.client.lock().await.is_none());
    }

    /// Retry a failed stop instead of caching its error forever.
    #[tokio::test]
    async fn failed_collection_can_be_retried_without_starting_a_client() {
        let runtime = CopilotRuntime::new(Vec::new());
        *runtime.shutdown.lock().await =
            Shutdown::Complete(Err(std::sync::Arc::new(provider_failure(
                forge_provider_api::error::ProviderOperation::Shutdown,
                forge_provider_api::error::ProviderFailureKind::Transport,
            ))));
        runtime.shutdown().await.unwrap();
        assert!(matches!(
            *runtime.shutdown.lock().await,
            Shutdown::Complete(Ok(()))
        ));
        assert_eq!(runtime.start_count(), 0);
    }

    #[tokio::test]
    async fn shutdown_waiter_cancellation_retains_the_same_collection_task() {
        let runtime = CopilotRuntime::new(Vec::new());
        let (release, released) = tokio::sync::oneshot::channel();
        *runtime.shutdown.lock().await = Shutdown::Running(tokio::spawn(async move {
            released.await.map_err(|_| {
                std::sync::Arc::new(provider_failure(
                    forge_provider_api::error::ProviderOperation::Shutdown,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                ))
            })
        }));
        assert!(
            tokio::time::timeout(std::time::Duration::ZERO, runtime.shutdown())
                .await
                .is_err()
        );
        assert!(matches!(
            *runtime.shutdown.lock().await,
            Shutdown::Running(_)
        ));
        release.send(()).unwrap();
        runtime.shutdown().await.unwrap();
        runtime.shutdown().await.unwrap();
        assert_eq!(runtime.start_count(), 0);
        assert!(runtime.client(".").await.is_err());
    }

    /// Reopen the same runtime only after both collection proofs and revalidate its new peer.
    #[tokio::test]
    async fn failed_generation_recovery_revalidates_same_runtime_and_fences_old_callbacks() {
        use super::super::client_generation::CollectedSessions;
        use forge_provider_api::lifecycle::ObservationFailure;
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            for next_version in ["1.0.104", "1.0.102"] {
                let runtime = CopilotRuntime::new(vec!["fixture-selected-copilot".into()]);
                let original_peer =
                    RuntimePeer::attach(&runtime, StatusReply::Version("1.0.103")).await;
                let original = runtime.client(".").await.unwrap();
                runtime.client(".").await.unwrap();
                original.fail_generation(ObservationFailure::Disconnected);
                assert!(original.generation.is_stopped());
                assert!(
                    !original.generation.is_collected(),
                    "transport failure manufactured SDK collection proof"
                );
                let failed = runtime
                    .failed_generation()
                    .await
                    .expect("first native loss must fence cached client");
                assert!(matches!(
                    runtime.client(".").await,
                    Err(ProviderError::Observation(ObservationFailure::Disconnected))
                ));
                let client = runtime.collect_failed_client(&failed).await.unwrap();
                assert!(
                    runtime.client(".").await.is_err(),
                    "SDK stop alone reopened the cache before session collection"
                );
                // This fixture has no session work: its empty registry can immediately seal collection.
                let stale_proof = CollectedSessions::new(&client);
                runtime
                    .finish_recovery(CollectedSessions::new(&client))
                    .await
                    .unwrap();
                assert_eq!(original_peer.collected().await, ["status.get"]);

                let replacement_peer =
                    RuntimePeer::attach(&runtime, StatusReply::Version(next_version)).await;
                let replacement = runtime.client(".").await;
                assert!(
                    runtime.finish_recovery(stale_proof).await.is_err(),
                    "an old proof retired the replacement cache"
                );
                original.fail_generation(ObservationFailure::Overflow);
                if next_version == "1.0.104" {
                    let replacement = replacement.unwrap();
                    replacement.ensure_healthy().unwrap();
                    runtime.client(".").await.unwrap().ensure_healthy().unwrap();
                } else {
                    assert!(matches!(
                        replacement,
                        Err(ProviderError::RuntimeVersion(
                            RuntimeVersionError::Unsupported { .. }
                        ))
                    ));
                }
                runtime.shutdown().await.unwrap();
                assert_eq!(
                    replacement_peer.collected().await,
                    ["status.get"],
                    "recovery dispatched or replayed native input"
                );
            }
        })
        .await
        .expect("same-runtime recovery fixture timed out");
    }
}
