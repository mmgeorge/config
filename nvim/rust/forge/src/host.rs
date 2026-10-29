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

use crate::router::{HostRouter, validate_initialize};
use crate::runtime::{ForgeRuntime, SHUTDOWN_DEADLINE};
const WRITER_STOP_DEADLINE: Duration = Duration::from_millis(100);
const RESERVED_CONTROL_REQUESTS: usize = 4;

mod preparation;

struct RequestAdmission {
    ordinary: Arc<Semaphore>,
    control: Arc<Semaphore>,
}

impl RequestAdmission {
    fn new() -> Self {
        Self {
            ordinary: Arc::new(Semaphore::new(
                forge_protocol::MAX_ACTIVE_REQUESTS - RESERVED_CONTROL_REQUESTS,
            )),
            control: Arc::new(Semaphore::new(RESERVED_CONTROL_REQUESTS)),
        }
    }

    fn acquire(
        &self,
        method: &str,
    ) -> Result<tokio::sync::OwnedSemaphorePermit, tokio::sync::TryAcquireError> {
        let control = forge_protocol::contract::method_policy(method).is_some_and(|policy| {
            policy.admission == forge_protocol::method_policy::Admission::Control
        });
        if control {
            Arc::clone(&self.control)
                .try_acquire_owned()
                .or_else(|_| Arc::clone(&self.ordinary).try_acquire_owned())
        } else {
            Arc::clone(&self.ordinary).try_acquire_owned()
        }
    }
}

type TaskOutcome = std::result::Result<(Id, Result<()>), JoinError>;

#[derive(Deserialize)]
pub(crate) struct RoutedRequestEnvelope {
    #[serde(skip)]
    pub(crate) harness_admission: Option<forge_harness::command::ExecutionPermit>,
    #[serde(default)]
    pub(crate) session_id: Option<String>,
    #[serde(flatten)]
    pub(crate) request: Request,
}

impl RoutedRequestEnvelope {
    pub(crate) fn validate_context(&self) -> Result<()> {
        forge_protocol::contract::method_policy(&self.request.method)
            .context("unknown Forge method")?
            .validate_session(self.session_id.as_deref())
            .map_err(anyhow::Error::msg)
    }
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

struct ConnectionInput<Input> {
    input: JsonLineReader<BufReader<Input>>,
    input_open: bool,
    credit: Arc<ReceiveCredit>,
}

struct ConnectionDrain<Input> {
    input: ConnectionInput<Input>,
    sink: Option<MessageSender>,
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
            initialize.request.method == "host.initialize",
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
        let mut response = handshake_response(initialize)?;
        if response.error().is_none()
            && let Some(directory) = recovery_directory.filter(|value| !value.is_null())
        {
            let configured = serde_json::from_value(directory)
                .map_err(anyhow::Error::from)
                .and_then(|directory| self.runtime.github.configure_recovery(directory));
            if let Err(error) = configured {
                response =
                    Response::failure(initialize_id, "invalid_request", format!("{error:#}"));
            }
        }
        if response.error().is_none()
            && let Some(directory) = status_ignored_directory.filter(|value| !value.is_null())
        {
            let configured = serde_json::from_value(directory)
                .map_err(anyhow::Error::from)
                .and_then(|directory| self.runtime.status.configure_ignored_directory(directory));
            if let Err(error) = configured {
                response =
                    Response::failure(initialize_id, "invalid_request", format!("{error:#}"));
            }
        }
        if response.error().is_none()
            && let Some(path) = git_config_cache_path.filter(|value| !value.is_null())
        {
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

        let credit = Arc::new(ReceiveCredit::default());
        let handshake = outbound::encode(
            &Message::Response(response),
            forge_protocol::MAX_FRAME_BYTES,
        )?;
        let (sink, stream) = outbound::channel();
        let writer_credit = Arc::clone(&credit);
        let (writer_stop, stopped) = oneshot::channel();
        let mut writer = tokio::spawn(async move {
            tokio::select! {
                result = write_messages(output, stream, writer_credit, handshake) => result,
                _ = stopped => anyhow::bail!("Forge writer stopped before output drained"),
            }
        });
        let mut input = ConnectionInput {
            input,
            input_open: true,
            credit,
        };
        let (ending, writer) = tokio::select! {
            result = &mut writer => {
                let ending = result.context("Forge writer task failed")
                    .and_then(|result| result)
                    .and_then(|_| anyhow::bail!("Forge writer ended before connection shutdown"));
                (ending, None)
            }
            ending = self.read_requests(&mut input, &sink) => (ending, Some(writer)),
        };
        let mut connection = ConnectionDrain {
            input,
            sink: Some(sink),
            writer,
            writer_stop: Some(writer_stop),
        };
        self.shutdown(&mut connection, ending).await
    }

    async fn read_requests<Input: AsyncRead + Unpin>(
        &self,
        connection: &mut ConnectionInput<Input>,
        sink: &MessageSender,
    ) -> Result<Option<u64>> {
        let admission = RequestAdmission::new();
        let preparation_order = Arc::new(preparation::PreparationOrder::default());
        loop {
            let line = tokio::select! {
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
            let mut envelope = match serde_json::from_slice::<RoutedRequestEnvelope>(&line) {
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
            if matches!(
                envelope.request.method.as_str(),
                "host.transport.consumed" | "host.shutdown"
            ) && let Err(error) = envelope.validate_context()
            {
                sink.send_control(Message::Response(Response::failure(
                    envelope.request.id,
                    "invalid_request",
                    format!("{error:#}"),
                )))?;
                continue;
            }
            if envelope.request.method == "host.transport.consumed" {
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
            let permit = match admission.acquire(&envelope.request.method) {
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
            if envelope.request.method == "host.shutdown" {
                return Ok(Some(envelope.request.id));
            }
            let router = Arc::clone(&self.router);
            let sink = sink.clone();
            let preparation_turn = preparation_order.reserve(&envelope)?;
            self.requests.spawn(envelope.request.id, async move {
                let _permit = permit;
                let request_id = envelope.request.id;
                let outcome = async {
                    if let Some(turn) = &preparation_turn {
                        turn.wait().await;
                    }
                    let prepared = router.prepare(&mut envelope).await;
                    drop(preparation_turn);
                    let method = prepared?;
                    router.route(envelope, method, &sink).await
                }
                .await;
                if let Err(error) = outcome {
                    let code = if error.is::<forge_harness::broker::RequestNotAdmitted>() {
                        "not_admitted"
                    } else {
                        "request_failed"
                    };
                    sink.send(Message::Response(Response::failure(
                        request_id,
                        code,
                        forge_harness::storage::error_message(&error),
                    )))?;
                }
                Ok(())
            })?;
        }
    }

    async fn shutdown<Input: AsyncRead + Unpin>(
        &self,
        connection: &mut ConnectionDrain<Input>,
        ending: Result<Option<u64>>,
    ) -> Result<()> {
        self.requests.close();
        let mut failure = Vec::new();
        let request_id = match ending {
            Ok(request_id) => request_id,
            Err(error) => {
                failure.push(format!("{error:#}"));
                connection.input.input_open = false;
                None
            }
        };
        let deadline = Instant::now() + SHUTDOWN_DEADLINE;
        // A native reader can outlive request cancellation. The connection must still return a
        // terminal shutdown response and release stdio within its global deadline.
        let shutdown = timeout(SHUTDOWN_DEADLINE, self.runtime.shutdown());
        tokio::pin!(shutdown);
        let mut feature_done = false;
        let mut subscription_done = false;
        let mut subscription = None;
        let mut request_abort_started = false;
        loop {
            let pending = self.requests.pending();
            if feature_done && pending.is_empty() && !subscription_done && subscription.is_none() {
                match self.router.finish_status_subscription() {
                    Ok(task) => {
                        subscription = task;
                        subscription_done = subscription.is_none();
                    }
                    Err(error) => {
                        failure.push(format!("{error:#}"));
                        subscription_done = true;
                    }
                }
            }
            if feature_done
                && pending.is_empty()
                && subscription_done
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
            if feature_done
                && pending.is_empty()
                && subscription_done
                && connection.writer.is_none()
            {
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
                result = pending_task(&mut subscription) => {
                    subscription.take();
                    subscription_done = true;
                    match result {
                        Ok(Ok(())) => {}
                        Ok(Err(error)) => failure.push(format!("Forge status subscription failed: {error:#}")),
                        Err(error) => failure.push(format!("Forge status subscription task failed: {error}")),
                    }
                }
                result = pending_task(&mut connection.writer) => {
                    connection.writer.take();
                    match result {
                        Ok(Ok(())) => {},
                        Ok(Err(error)) => failure.push(format!("{error:#}")),
                        Err(error) => failure.push(format!("Forge writer task failed: {error}")),
                    }
                }
                line = connection.input.input.next_frame(), if connection.input.input_open => {
                    match line {
                        Ok(Some(line)) => {
                            let result = grant_credit(&connection.input.credit, &line);
                            if let Err(error) = result {
                                failure.push(format!("{error:#}"));
                                connection.input.input_open = false;
                            }
                        }
                        Ok(None) => connection.input.input_open = false,
                        Err(error) => {
                            failure.push(error.to_string());
                            connection.input.input_open = false;
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
        match self.router.finish_status_subscription() {
            Ok(Some(task)) => subscription = Some(task),
            Ok(None) => {}
            Err(error) => failure.push(format!("{error:#}")),
        }
        if let Some(task) = subscription {
            task.abort();
            match task.await {
                Ok(Ok(())) => {}
                Ok(Err(error)) => {
                    failure.push(format!("Forge status subscription failed: {error:#}"))
                }
                Err(error) if error.is_cancelled() => {}
                Err(error) => {
                    failure.push(format!("Forge status subscription task failed: {error}"))
                }
            }
        }
        connection.sink.take();
        connection.input.credit.close();
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

async fn pending_task<Task: Future + Unpin>(task: &mut Option<Task>) -> Task::Output {
    match task {
        Some(task) => task.await,
        None => std::future::pending().await,
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

fn handshake_response(envelope: RoutedRequestEnvelope) -> Result<Response> {
    if let Err(error) = envelope.validate_context() {
        return Ok(Response::failure(
            envelope.request.id,
            "invalid_request",
            format!("{error:#}"),
        ));
    }
    let request = envelope.request;
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
    envelope.validate_context()?;
    ensure!(
        envelope.request.method == "host.transport.consumed",
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
    handshake: Vec<u8>,
) -> Result<()> {
    write_frame(&mut output, &credit, &handshake).await?;
    while let Some(frame) = stream.recv().await? {
        write_frame(&mut output, &credit, frame.bytes()).await?;
        if frame.is_terminal() {
            break;
        }
    }
    Ok(())
}

async fn write_frame<Output: AsyncWrite + Unpin>(
    output: &mut Output,
    credit: &ReceiveCredit,
    frame: &[u8],
) -> Result<()> {
    credit.reserve(frame.len()).await?;
    output.write_all(frame).await?;
    output.flush().await.context("flush Forge output")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handshake_rejects_session_context() {
        let envelope = serde_json::from_value(json!({
            "id":1,"method":"host.initialize","session_id":"session",
            "params":{"protocol_version":forge_protocol::WIRE_VERSION}
        }))
        .unwrap();
        let response = handshake_response(envelope).unwrap();
        assert!(response.error().is_some());
    }

    #[tokio::test]
    async fn control_frames_reject_session_context_without_stopping_the_host() {
        use tokio::io::AsyncBufReadExt;

        let host = ConnectionHost::new(Arc::new(ForgeRuntime::new().unwrap()));
        let (client, server) = tokio::io::duplex(4096);
        let (input, output) = tokio::io::split(server);
        let running = tokio::spawn(async move { host.run(input, output).await });
        let mut client = BufReader::new(client);
        let initialize = json!({"id":1,"method":"host.initialize","params":{"protocol_version":forge_protocol::WIRE_VERSION}});
        client
            .get_mut()
            .write_all(format!("{initialize}\n").as_bytes())
            .await
            .unwrap();
        let mut response = String::new();
        client.read_line(&mut response).await.unwrap();
        assert_eq!(serde_json::from_str::<Value>(&response).unwrap()["id"], 1);
        for (id, method, params) in [
            (2, "host.transport.consumed", json!({"bytes":0,"frames":0})),
            (3, "host.shutdown", json!({})),
        ] {
            let request = json!({"id":id,"method":method,"session_id":"session","params":params});
            client
                .get_mut()
                .write_all(format!("{request}\n").as_bytes())
                .await
                .unwrap();
            response.clear();
            timeout(Duration::from_secs(1), client.read_line(&mut response))
                .await
                .unwrap()
                .unwrap();
            let rejected: Value = serde_json::from_str(&response).unwrap();
            assert_eq!(rejected["id"], id);
            assert_eq!(rejected["error"]["code"], "invalid_request");
        }
        client
            .get_mut()
            .write_all(b"{\"id\":4,\"method\":\"host.shutdown\",\"params\":{}}\n")
            .await
            .unwrap();
        response.clear();
        timeout(Duration::from_secs(1), client.read_line(&mut response))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(
            serde_json::from_str::<Value>(&response).unwrap()["result"]["shutdown"],
            true
        );
        running.await.unwrap().unwrap();
    }

    #[test]
    fn control_requests_share_spare_capacity_and_survive_ordinary_saturation() {
        let admission = RequestAdmission::new();
        let ordinary: Vec<_> = (0..forge_protocol::MAX_ACTIVE_REQUESTS - RESERVED_CONTROL_REQUESTS)
            .map(|_| admission.acquire("session.state.get").unwrap())
            .collect();
        assert!(admission.acquire("session.state.get").is_err());
        let control: Vec<_> = [
            "session.health.get",
            "session.command.operation.get",
            "session.turn.cancel",
            "session.approval.resolve",
        ]
        .map(|method| admission.acquire(method).unwrap())
        .into();
        assert!(admission.acquire("host.shutdown").is_err());
        drop(ordinary);
        let additional = admission.acquire("session.command.apply").unwrap();
        assert!(admission.acquire("session.state.get").is_ok());
        drop(additional);
        drop(control);
        assert_eq!(
            admission.control.available_permits(),
            RESERVED_CONTROL_REQUESTS
        );
        assert_eq!(
            admission.ordinary.available_permits(),
            forge_protocol::MAX_ACTIVE_REQUESTS - RESERVED_CONTROL_REQUESTS
        );
    }

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
        sender.write_all(format!("{{\"id\":1,\"method\":\"host.initialize\",\"params\":{{\"protocol_version\":{}}}}}\n", forge_protocol::WIRE_VERSION).as_bytes()).await.unwrap();
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
    async fn eof_collects_status_subscription_while_runtime_update_source_remains_alive() {
        use tokio::io::{AsyncBufReadExt, AsyncReadExt};

        let runtime = Arc::new(ForgeRuntime::new().unwrap());
        let mut updates = runtime.status.subscribe_updates();
        let host = ConnectionHost::new(Arc::clone(&runtime));
        let (client, server) = tokio::io::duplex(4096);
        let (input, output) = tokio::io::split(server);
        let running = tokio::spawn(async move { host.run(input, output).await });
        let mut client = BufReader::new(client);
        client.get_mut().write_all(format!(
            "{{\"id\":1,\"method\":\"host.initialize\",\"params\":{{\"protocol_version\":{}}}}}\n{{\"id\":2,\"method\":\"session.state.get\",\"params\":{{}}}}\n",
            forge_protocol::WIRE_VERSION,
        ).as_bytes()).await.unwrap();
        for expected in [1, 2] {
            let mut response = Vec::new();
            client.read_until(b'\n', &mut response).await.unwrap();
            let response: Value = serde_json::from_slice(&response).unwrap();
            assert_eq!(response["id"], expected);
            if expected == 1 {
                assert!(response.get("error").is_none(), "{response}");
            } else {
                assert_eq!(response["error"]["code"], "request_failed", "{response}");
            }
        }
        client.get_mut().shutdown().await.unwrap();
        timeout(Duration::from_secs(3), async {
            let mut remaining = Vec::new();
            client.read_to_end(&mut remaining).await.unwrap();
            assert!(remaining.is_empty(), "{remaining:?}");
            running.await.unwrap().unwrap();
        })
        .await
        .expect("EOF did not collect the status subscription");
        assert!(runtime.requests.pending().is_empty());
        assert!(matches!(
            updates.try_recv(),
            Err(tokio::sync::broadcast::error::TryRecvError::Empty)
        ));
    }

    #[tokio::test]
    async fn waiting_preparation_does_not_block_an_independent_request() {
        use tokio::io::AsyncBufReadExt;

        let runtime = Arc::new(ForgeRuntime::new().unwrap());
        let host = ConnectionHost::new(runtime);
        let (arrived, waiting) = oneshot::channel();
        let (release, released) = oneshot::channel();
        *host.router.preparation_hold.lock().unwrap() = Some(crate::router::PreparationHold {
            request_id: 2,
            arrived,
            released,
        });
        let (client, server) = tokio::io::duplex(8192);
        let (input, output) = tokio::io::split(server);
        let running = tokio::spawn(async move { host.run(input, output).await });
        let mut client = BufReader::new(client);
        let initialize = serde_json::json!({"id":1,"method":"host.initialize","params":{"protocol_version":forge_protocol::WIRE_VERSION}});
        client
            .get_mut()
            .write_all(format!("{initialize}\n").as_bytes())
            .await
            .unwrap();
        let mut response = String::new();
        client.read_line(&mut response).await.unwrap();
        assert!(
            serde_json::from_str::<Value>(&response)
                .unwrap()
                .get("error")
                .is_none()
        );
        client
            .get_mut()
            .write_all(b"{\"id\":2,\"method\":\"repository.revision.list\",\"params\":{}}\n")
            .await
            .unwrap();
        timeout(Duration::from_secs(1), waiting)
            .await
            .unwrap()
            .unwrap();
        client
            .get_mut()
            .write_all(b"{\"id\":3,\"method\":\"repository.revision.list\",\"params\":{}}\n")
            .await
            .unwrap();
        response.clear();
        timeout(Duration::from_secs(1), client.read_line(&mut response))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(serde_json::from_str::<Value>(&response).unwrap()["id"], 3);
        release.send(()).unwrap();
        response.clear();
        timeout(Duration::from_secs(1), client.read_line(&mut response))
            .await
            .unwrap()
            .unwrap();
        assert_eq!(serde_json::from_str::<Value>(&response).unwrap()["id"], 2);
        client.get_mut().shutdown().await.unwrap();
        timeout(Duration::from_secs(3), running)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
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
        client.get_mut().write_all(format!("{{\"id\":1,\"method\":\"host.initialize\",\"params\":{{\"protocol_version\":{}}}}}\n", forge_protocol::WIRE_VERSION).as_bytes()).await.unwrap();
        let mut response = Vec::new();
        client.read_until(b'\n', &mut response).await.unwrap();
        assert_eq!(serde_json::from_slice::<Value>(&response).unwrap()["id"], 1);
        client
            .get_mut()
            .write_all(b"{\"id\":8,\"method\":\"host.shutdown\",\"params\":{}}\n")
            .await
            .unwrap();
        timeout(Duration::from_secs(1), async {
            while runtime
                .harness
                .prepare(
                    None,
                    forge_harness::protocol::RuntimeMethod::SessionStateGet,
                )
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
