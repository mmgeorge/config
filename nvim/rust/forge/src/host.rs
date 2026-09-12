use std::collections::HashMap;
use std::future::{Future, poll_fn};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use forge_protocol::credit::{Consumption, ReceiveCredit};
use forge_protocol::frame::JsonLineReader;
use forge_protocol::message::{Message, Request, Response};
use forge_protocol::outbound::{self, MessageReceiver, MessageSender};
use serde::Deserialize;
use serde_json::{Value, json};
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt, BufReader};
use tokio::sync::{Semaphore, oneshot};
use tokio::task::{Id, JoinError, JoinHandle, JoinSet};
use tokio::time::{Instant, timeout};

use crate::router::{HostRouter, RoutedMethod, validate_initialize};
use crate::runtime::{ForgeRuntime, SHUTDOWN_DEADLINE};
const WRITER_STOP_DEADLINE: Duration = Duration::from_millis(100);

type TaskOutcome = std::result::Result<(Id, Result<()>), JoinError>;

#[derive(Deserialize)]
pub(crate) struct RoutedRequestEnvelope {
    #[serde(default)]
    pub(crate) session_id: Option<String>,
    #[serde(flatten)]
    pub(crate) request: Request,
}

/// Retains request tasks and their IDs until completion, including after a drain timeout.
#[derive(Default)]
pub(crate) struct RequestTaskStore {
    state: Mutex<RequestTaskState>,
}

#[derive(Default)]
struct RequestTaskState {
    closed: bool,
    task: JoinSet<Result<()>>,
    request_by_task: HashMap<Id, u64>,
}

/// Owns one connection's admission, writer, consumption credit, and request drainage.
pub(crate) struct ConnectionHost {
    runtime: Arc<ForgeRuntime>,
    router: Arc<HostRouter>,
    requests: Arc<RequestTaskStore>,
}

struct ConnectionIo<Input> {
    input: JsonLineReader<BufReader<Input>>,
    input_open: bool,
    sink: Option<MessageSender>,
    credit: Arc<ReceiveCredit>,
    writer: Option<JoinHandle<Result<()>>>,
    writer_stop: Option<oneshot::Sender<()>>,
}

impl RequestTaskStore {
    fn spawn(
        &self,
        request_id: u64,
        work: impl Future<Output = Result<()>> + Send + 'static,
    ) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .expect("Forge request task store poisoned");
        ensure!(!state.closed, "Forge request admission is closed");
        ensure!(
            state.request_by_task.len() < forge_protocol::MAX_ACTIVE_REQUESTS,
            "Forge request task capacity is full"
        );
        ensure!(
            !state
                .request_by_task
                .values()
                .any(|active| *active == request_id),
            "duplicate in-flight Forge request id: {request_id}"
        );
        let task = state.task.spawn(work);
        state.request_by_task.insert(task.id(), request_id);
        Ok(())
    }

    fn close(&self) {
        self.state
            .lock()
            .expect("Forge request task store poisoned")
            .closed = true;
    }

    fn abort_all(&self) {
        self.state
            .lock()
            .expect("Forge request task store poisoned")
            .task
            .abort_all();
    }

    fn pending(&self) -> Vec<u64> {
        let mut request: Vec<_> = self
            .state
            .lock()
            .expect("Forge request task store poisoned")
            .request_by_task
            .values()
            .copied()
            .collect();
        request.sort_unstable();
        request
    }

    fn is_empty(&self) -> bool {
        self.state
            .lock()
            .expect("Forge request task store poisoned")
            .task
            .is_empty()
    }

    fn contains(&self, request_id: u64) -> bool {
        self.state
            .lock()
            .expect("Forge request task store poisoned")
            .request_by_task
            .values()
            .any(|active| *active == request_id)
    }

    fn reap(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .expect("Forge request task store poisoned");
        while let Some(outcome) = state.task.try_join_next_with_id() {
            complete_request(&mut state, outcome)?;
        }
        Ok(())
    }

    async fn join_next(&self) -> Option<Result<()>> {
        poll_fn(|context| {
            let mut state = self
                .state
                .lock()
                .expect("Forge request task store poisoned");
            state
                .task
                .poll_join_next_with_id(context)
                .map(|outcome| outcome.map(|outcome| complete_request(&mut state, outcome)))
        })
        .await
    }
}

impl ConnectionHost {
    fn new(runtime: Arc<ForgeRuntime>) -> Self {
        Self {
            router: Arc::new(HostRouter::new(Arc::clone(&runtime))),
            requests: Arc::clone(&runtime.requests),
            runtime,
        }
    }

    async fn run<Input, Output>(&self, input: Input, mut output: Output) -> Result<()>
    where
        Input: AsyncRead + Unpin,
        Output: AsyncWrite + Unpin + Send + 'static,
    {
        let mut input = JsonLineReader::new(BufReader::new(input));
        let initialize_line = input
            .next_frame()
            .await?
            .context("Forge client closed before initialize")?;
        let initialize: RoutedRequestEnvelope =
            serde_json::from_slice(&initialize_line).context("decode Forge initialize request")?;
        ensure!(
            initialize.request.method == "initialize",
            "first Forge request must initialize the host"
        );
        let initialize_id = initialize.request.id;
        let git_config_cache_path = initialize
            .request
            .params
            .get("git_config_cache_path")
            .cloned();
        let recovery_directory = initialize.request.params.get("recovery_directory").cloned();
        let status_ignored_directory = initialize
            .request
            .params
            .get("status_ignored_directory")
            .cloned();
        let mut response = handshake_response(initialize.request)?;
        if response.error().is_none() {
            if let Some(directory) = recovery_directory.filter(|value| !value.is_null()) {
                let configured = serde_json::from_value(directory)
                    .map_err(anyhow::Error::from)
                    .and_then(|directory| self.runtime.github.configure_recovery(directory));
                if let Err(error) = configured {
                    response =
                        Response::failure(initialize_id, "invalid_request", format!("{error:#}"));
                }
            }
        }
        if response.error().is_none() {
            if let Some(directory) = status_ignored_directory.filter(|value| !value.is_null()) {
                let configured = serde_json::from_value(directory)
                    .map_err(anyhow::Error::from)
                    .and_then(|directory| {
                        self.runtime.status.configure_ignored_directory(directory)
                    });
                if let Err(error) = configured {
                    response =
                        Response::failure(initialize_id, "invalid_request", format!("{error:#}"));
                }
            }
        }
        if response.error().is_none() {
            if let Some(path) = git_config_cache_path.filter(|value| !value.is_null()) {
                let configured: Result<&'static str> = async {
                    let path: std::path::PathBuf = serde_json::from_value(path)?;
                    self.runtime
                        .repositories
                        .reads
                        .submit(path.as_os_str().len(), move |_| {
                            forge_git::config_location::configure(&path)
                        })?
                        .finish()
                        .await
                }
                .await;
                let cache = match configured {
                    Ok(state) => json!({ "state": state }),
                    Err(error) => {
                        json!({ "state": "error", "warning": format!("Git configuration location cache: {error:#}") })
                    }
                };
                response = Response::success(
                    initialize_id,
                    json!({
                        "protocol_version": forge_protocol::WIRE_VERSION,
                        "git_config_cache": cache,
                    }),
                )?;
            }
        }
        let accepted = response.error().is_none();
        if !accepted {
            output
                .write_all(&outbound::encode(
                    &Message::Response(response),
                    forge_protocol::MAX_FRAME_BYTES,
                )?)
                .await?;
            output
                .flush()
                .await
                .context("flush Forge handshake rejection")?;
            return Ok(());
        }

        let (sink, stream) = outbound::channel();
        sink.send(Message::Response(response))?;
        let credit = Arc::new(ReceiveCredit::default());
        let writer_credit = Arc::clone(&credit);
        let (writer_stop, stopped) = oneshot::channel();
        let writer = tokio::spawn(async move {
            tokio::select! {
                result = write_messages(output, stream, writer_credit) => result,
                _ = stopped => anyhow::bail!("Forge writer stopped before output drained"),
            }
        });
        let mut connection = ConnectionIo {
            input,
            input_open: true,
            sink: Some(sink),
            credit,
            writer: Some(writer),
            writer_stop: Some(writer_stop),
        };
        let ending = self.read_requests(&mut connection).await;
        self.shutdown(&mut connection, ending).await
    }

    async fn read_requests<Input: AsyncRead + Unpin>(
        &self,
        connection: &mut ConnectionIo<Input>,
    ) -> Result<Option<u64>> {
        let ordinary = Arc::new(Semaphore::new(forge_protocol::MAX_ACTIVE_REQUESTS - 1));
        let control = Arc::new(Semaphore::new(1));
        loop {
            let sink = connection.sink.as_ref().expect("active Forge output");
            let line = tokio::select! {
                result = connection.writer.as_mut().expect("active Forge writer") => {
                    connection.writer.take();
                    result.context("Forge writer task failed")??;
                    anyhow::bail!("Forge writer ended before connection shutdown");
                }
                error = sink.failed() => return Err(error.into()),
                joined = self.requests.join_next(), if !self.requests.is_empty() => {
                    if let Some(result) = joined { result?; }
                    continue;
                }
                line = connection.input.next_frame() => line?,
            };
            let Some(line) = line else {
                connection.input_open = false;
                return Ok(None);
            };
            let envelope = match serde_json::from_slice::<RoutedRequestEnvelope>(&line) {
                Ok(envelope) => envelope,
                Err(error) => {
                    sink.send(Message::Response(Response::failure(
                        0,
                        "invalid_request",
                        format!("decode Forge request: {error}"),
                    )))?;
                    continue;
                }
            };
            if envelope.request.method == "transport.consumed" {
                connection
                    .credit
                    .grant(serde_json::from_value::<Consumption>(
                        envelope.request.params,
                    )?)?;
                continue;
            }
            self.requests.reap()?;
            ensure!(
                !self.requests.contains(envelope.request.id),
                "duplicate in-flight Forge request id: {}",
                envelope.request.id
            );
            let admission = if matches!(
                envelope.request.method.as_str(),
                "turn.cancel" | "turn.restart" | "approval.resolve" | "shutdown"
            ) {
                &control
            } else {
                &ordinary
            };
            let permit = match Arc::clone(admission).try_acquire_owned() {
                Ok(permit) => permit,
                Err(_) => {
                    sink.send_control(Message::Response(Response::failure(
                        envelope.request.id,
                        "busy",
                        "Forge request admission is full",
                    )))?;
                    continue;
                }
            };
            let method = match self.router.prepare(&envelope).await {
                Ok(method) => method,
                Err(error) => {
                    sink.send(Message::Response(Response::failure(
                        envelope.request.id,
                        "request_failed",
                        format!("{error:#}"),
                    )))?;
                    continue;
                }
            };
            if matches!(method, RoutedMethod::Shutdown) {
                return Ok(Some(envelope.request.id));
            }
            let router = Arc::clone(&self.router);
            let sink = sink.clone();
            self.requests.spawn(envelope.request.id, async move {
                let _permit = permit;
                let request_id = envelope.request.id;
                if let Err(error) = router.route(envelope, method, &sink).await {
                    sink.send(Message::Response(Response::failure(
                        request_id,
                        "request_failed",
                        format!("{error:#}"),
                    )))?;
                }
                Ok(())
            })?;
        }
    }

    async fn shutdown<Input: AsyncRead + Unpin>(
        &self,
        connection: &mut ConnectionIo<Input>,
        ending: Result<Option<u64>>,
    ) -> Result<()> {
        self.requests.close();
        let mut failure = Vec::new();
        let request_id = match ending {
            Ok(request_id) => request_id,
            Err(error) => {
                failure.push(format!("{error:#}"));
                connection.input_open = false;
                None
            }
        };
        let deadline = Instant::now() + SHUTDOWN_DEADLINE;
        // A native reader can outlive request cancellation. The connection must still return a
        // terminal shutdown response and release stdio within its global deadline.
        let shutdown = timeout(SHUTDOWN_DEADLINE, self.runtime.shutdown());
        tokio::pin!(shutdown);
        let mut feature_done = false;
        let mut request_abort_started = false;
        loop {
            let pending = self.requests.pending();
            if feature_done
                && pending.is_empty()
                && let Some(sink) = connection.sink.take()
                && let Some(request_id) = request_id
                && connection.writer.is_some()
            {
                let response = if failure.is_empty() {
                    Response::success(request_id, json!({"shutdown":true}))?
                } else {
                    Response::failure(request_id, "shutdown_incomplete", failure.join("\n"))
                };
                if let Err(error) = sink.send_terminal(Message::Response(response)) {
                    failure.push(error.to_string());
                }
            }
            if feature_done && pending.is_empty() && connection.writer.is_none() {
                break;
            }
            tokio::select! {
                result = &mut shutdown, if !feature_done => {
                    feature_done = true;
                    match result {
                        Ok(Ok(())) => {}
                        Ok(Err(error)) => failure.push(format!("{error:#}")),
                        Err(_) => failure.push("Forge runtime shutdown deadline expired".into()),
                    }
                }
                joined = self.requests.join_next(), if !pending.is_empty() => {
                    if let Some(Err(error)) = joined { failure.push(format!("{error:#}")); }
                }
                result = async { connection.writer.as_mut().expect("pending Forge writer").await }, if connection.writer.is_some() => {
                    connection.writer.take();
                    match result {
                        Ok(Ok(())) => {},
                        Ok(Err(error)) => failure.push(format!("{error:#}")),
                        Err(error) => failure.push(format!("Forge writer task failed: {error}")),
                    }
                }
                line = connection.input.next_frame(), if connection.input_open => {
                    match line {
                        Ok(Some(line)) => {
                            let result = grant_credit(&connection.credit, &line);
                            if let Err(error) = result {
                                failure.push(format!("{error:#}"));
                                connection.input_open = false;
                            }
                        }
                        Ok(None) => connection.input_open = false,
                        Err(error) => {
                            failure.push(error.to_string());
                            connection.input_open = false;
                        }
                    }
                }
                _ = tokio::time::sleep_until(deadline), if !request_abort_started => {
                    if request_id.is_none() {
                        failure.push("Forge connection shutdown deadline expired before output drained after input EOF".into());
                        break;
                    }
                    request_abort_started = true;
                    if !pending.is_empty() {
                        failure.push(format!("Forge connection shutdown deadline expired, cancelling unfinished request IDs: {pending:?}, feature shutdown complete: {feature_done}"));
                        self.requests.abort_all();
                    }
                }
            }
        }
        connection.sink.take();
        connection.credit.close();
        if let Some(stop) = connection.writer_stop.take() {
            let _ = stop.send(());
        }
        if let Some(mut writer) = connection.writer.take()
            && timeout(WRITER_STOP_DEADLINE, &mut writer).await.is_err()
        {
            writer.abort();
            failure.push("Forge writer did not stop within its cancellation deadline".into());
        }
        ensure!(failure.is_empty(), "{}", failure.join("\n"));
        Ok(())
    }
}

pub(crate) struct NvimRunOutcome {
    pub(crate) result: Result<()>,
    pub(crate) shutdown_owned: bool,
}

impl NvimRunOutcome {
    fn pre_connection(error: anyhow::Error) -> Self {
        Self {
            result: Err(error),
            shutdown_owned: false,
        }
    }

    fn connection(result: Result<()>) -> Self {
        Self {
            result,
            shutdown_owned: true,
        }
    }
}

pub(crate) async fn run_nvim(runtime: Arc<ForgeRuntime>) -> NvimRunOutcome {
    let input = match forge_protocol::input::ThreadInput::new(std::io::stdin()) {
        Ok(input) => input,
        Err(error) => return NvimRunOutcome::pre_connection(error.into()),
    };
    NvimRunOutcome::connection(
        ConnectionHost::new(runtime)
            .run(input, tokio::io::stdout())
            .await,
    )
}

fn handshake_response(request: Request) -> Result<Response> {
    if request
        .params
        .get("protocol_version")
        .and_then(Value::as_u64)
        != Some(u64::from(forge_protocol::WIRE_VERSION))
    {
        return Ok(Response::failure_with_data(
            request.id,
            "protocol_mismatch",
            "Forge client and host wire versions differ. Rebuild Forge and restart the client.",
            json!({"expected_version":forge_protocol::WIRE_VERSION}),
        ));
    }
    if let Err(error) = validate_initialize(&request.params) {
        return Ok(Response::failure(
            request.id,
            "invalid_request",
            format!("{error:#}"),
        ));
    }
    Ok(Response::success(
        request.id,
        json!({"protocol_version":forge_protocol::WIRE_VERSION}),
    )?)
}

fn complete_request(state: &mut RequestTaskState, outcome: TaskOutcome) -> Result<()> {
    let task_id = match &outcome {
        Ok((task_id, _)) => *task_id,
        Err(error) => error.id(),
    };
    let request_id = state
        .request_by_task
        .remove(&task_id)
        .context("Forge completed an unregistered request task")?;
    let (_, result) = outcome.with_context(|| format!("Forge request {request_id} task failed"))?;
    result.with_context(|| format!("Forge request {request_id} failed to deliver its response"))
}

fn grant_credit(credit: &ReceiveCredit, line: &[u8]) -> Result<()> {
    let envelope: RoutedRequestEnvelope = serde_json::from_slice(line)?;
    ensure!(
        envelope.request.method == "transport.consumed",
        "request arrived after shutdown"
    );
    credit.grant(serde_json::from_value::<Consumption>(
        envelope.request.params,
    )?)?;
    Ok(())
}

async fn write_messages<Output: AsyncWrite + Unpin>(
    mut output: Output,
    mut stream: MessageReceiver,
    credit: Arc<ReceiveCredit>,
) -> Result<()> {
    while let Some(frame) = stream.recv().await? {
        credit.reserve(frame.bytes().len()).await?;
        output.write_all(frame.bytes()).await?;
        output.flush().await.context("flush Forge output")?;
        if frame.is_terminal() {
            break;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn request_timeout_retains_its_runtime_and_admission_until_completion() {
        let runtime = Arc::new(ForgeRuntime::new().unwrap());
        let retained = Arc::downgrade(&runtime);
        let requests = Arc::clone(&runtime.requests);
        let admission = Arc::new(Semaphore::new(1));
        let permit = Arc::clone(&admission).acquire_owned().await.unwrap();
        let (release, released) = oneshot::channel();
        requests
            .spawn(7, async move {
                let _runtime = runtime;
                let _permit = permit;
                released.await?;
                Ok(())
            })
            .unwrap();
        assert!(
            requests
                .spawn(7, async { panic!("duplicate request executed") })
                .is_err()
        );
        requests.close();
        assert!(requests.spawn(8, async { Ok(()) }).is_err());
        assert!(
            timeout(Duration::from_millis(10), requests.join_next())
                .await
                .is_err()
        );
        assert_eq!(requests.pending(), vec![7]);
        assert!(retained.upgrade().is_some());
        assert_eq!(admission.available_permits(), 0);
        release.send(()).unwrap();
        requests.join_next().await.unwrap().unwrap();
        assert!(requests.pending().is_empty());
        assert!(retained.upgrade().is_none());
        assert_eq!(admission.available_permits(), 1);
    }

    #[tokio::test]
    async fn panicked_request_reports_its_identity_and_leaves_no_registry_entry() {
        let requests = RequestTaskStore::default();
        requests
            .spawn(19, async { panic!("request fixture panic") })
            .unwrap();
        let failure = requests.join_next().await.unwrap().unwrap_err();
        assert!(failure.to_string().contains("Forge request 19 task failed"));
        assert!(requests.pending().is_empty());
    }

    #[tokio::test]
    async fn eof_stops_a_writer_blocked_inside_an_output_write() {
        let runtime = Arc::new(ForgeRuntime::new().unwrap());
        let host = ConnectionHost::new(Arc::clone(&runtime));
        let (mut sender, input) = tokio::io::duplex(1024);
        let (output, _unread) = tokio::io::duplex(1);
        sender.write_all(format!("{{\"id\":1,\"method\":\"initialize\",\"params\":{{\"protocol_version\":{}}}}}\n", forge_protocol::WIRE_VERSION).as_bytes()).await.unwrap();
        drop(sender);
        let failure = timeout(Duration::from_secs(3), host.run(input, output))
            .await
            .unwrap()
            .unwrap_err();
        assert!(
            failure
                .to_string()
                .contains("connection shutdown deadline expired")
        );
        assert!(runtime.requests.pending().is_empty());
    }

    #[tokio::test]
    async fn terminal_shutdown_waits_for_all_connection_requests() {
        use tokio::io::AsyncBufReadExt;

        let runtime = Arc::new(ForgeRuntime::new().unwrap());
        let retained = Arc::clone(&runtime);
        let (release, released) = oneshot::channel();
        runtime
            .requests
            .spawn(7, async move {
                let _runtime = retained;
                released.await?;
                Ok(())
            })
            .unwrap();
        let host = ConnectionHost::new(Arc::clone(&runtime));
        let (client, server) = tokio::io::duplex(4096);
        let (input, output) = tokio::io::split(server);
        let running = tokio::spawn(async move { host.run(input, output).await });
        let mut client = BufReader::new(client);
        client.get_mut().write_all(format!("{{\"id\":1,\"method\":\"initialize\",\"params\":{{\"protocol_version\":{}}}}}\n", forge_protocol::WIRE_VERSION).as_bytes()).await.unwrap();
        let mut response = Vec::new();
        client.read_until(b'\n', &mut response).await.unwrap();
        assert_eq!(serde_json::from_slice::<Value>(&response).unwrap()["id"], 1);
        client
            .get_mut()
            .write_all(b"{\"id\":8,\"method\":\"shutdown\",\"params\":{}}\n")
            .await
            .unwrap();
        timeout(Duration::from_secs(1), async {
            while runtime
                .harness
                .prepare(None, forge_harness::protocol::HarnessMethod::StateGet)
                .await
                .is_ok()
            {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        response.clear();
        assert!(
            timeout(
                Duration::from_millis(20),
                client.read_until(b'\n', &mut response)
            )
            .await
            .is_err()
        );
        assert_eq!(runtime.requests.pending(), vec![7]);
        release.send(()).unwrap();
        timeout(
            Duration::from_secs(1),
            client.read_until(b'\n', &mut response),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(
            serde_json::from_slice::<Value>(&response).unwrap()["result"]["shutdown"],
            true
        );
        running.await.unwrap().unwrap();
        assert!(runtime.requests.pending().is_empty());
    }
}
