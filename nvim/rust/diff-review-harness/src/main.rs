use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use diff_review_harness::broker::{
    BrokerRuntime, HarnessBroker, InitializeRequest, TurnCancellation,
};
use diff_review_harness::protocol::{BrokerEvent, BrokerMessage, BrokerRequest, BrokerResponse};
use diff_review_harness::session::SessionLeaseConflict;
use diff_review_harness::storage::SqliteStore;
use serde::Deserialize;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::{Mutex, RwLock, mpsc};

#[derive(Parser)]
#[command(name = "diff-review-harness")]
struct Argument {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Run the plan and goal control-tool MCP server.
    Mcp,
}

#[derive(Deserialize)]
struct RoutedBrokerRequest {
    #[serde(default)]
    session_id: Option<String>,
    #[serde(flatten)]
    request: BrokerRequest,
}

/// Owns one session's serialized state machine and out-of-band control lanes.
struct SessionController {
    broker: Mutex<HarnessBroker>,
    cancellation: Arc<TurnCancellation>,
    backend: Arc<dyn diff_review_harness::backend::Backend>,
    catalog_request: RwLock<diff_review_harness::backend::BackendCatalogRequest>,
    permission: Arc<diff_review_harness::backend::approval::PermissionCoordinator>,
}

impl SessionController {
    /// Build one independently serialized controller from a durable broker session.
    fn new(broker: HarnessBroker) -> Arc<Self> {
        let catalog_request = broker.backend_catalog_request();
        Arc::new(Self {
            cancellation: broker.turn_cancellation(),
            backend: broker.backend_handle(),
            catalog_request: RwLock::new(catalog_request),
            permission: broker.permission_coordinator(),
            broker: Mutex::new(broker),
        })
    }
}

/// Stores live session controllers for one persistent broker process.
struct SessionControllerRegistry {
    controller_by_id: RwLock<HashMap<String, Arc<SessionController>>>,
    runtime: Arc<BrokerRuntime>,
    initialize: InitializeRequest,
}

impl SessionControllerRegistry {
    /// Build the registry with the session selected by broker initialization.
    fn new(
        initial_session_id: String,
        controller: Arc<SessionController>,
        runtime: Arc<BrokerRuntime>,
        initialize: InitializeRequest,
    ) -> Arc<Self> {
        Arc::new(Self {
            controller_by_id: RwLock::new(HashMap::from([(initial_session_id, controller)])),
            runtime,
            initialize,
        })
    }

    /// Resolve an existing controller or resume its durable session into this process.
    async fn resolve(&self, session_id: &str) -> Result<Arc<SessionController>> {
        if let Some(controller) = self.controller_by_id.read().await.get(session_id).cloned() {
            return Ok(controller);
        }
        let mut controller_by_id = self.controller_by_id.write().await;
        if let Some(controller) = controller_by_id.get(session_id).cloned() {
            return Ok(controller);
        }
        let mut initialize = self.initialize.clone();
        initialize.session_id = Some(session_id.to_owned());
        initialize.lease_conflict_action = None;
        let broker = HarnessBroker::initialize_with_runtime(initialize, Arc::clone(&self.runtime))?;
        let controller = SessionController::new(broker);
        controller_by_id.insert(session_id.to_owned(), Arc::clone(&controller));
        Ok(controller)
    }

    /// Register a newly persisted session before its snapshot reaches Neovim.
    async fn register(&self, session_id: &str) -> Result<Arc<SessionController>> {
        self.resolve(session_id).await
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
    tokio::task::LocalSet::new()
        .run_until(async {
            match Argument::parse().command {
                Some(Command::Mcp) => diff_review_harness::control_tools::run_stdio().await,
                None => run_broker().await,
            }
        })
        .await
}

async fn run_broker() -> Result<()> {
    let mut input = BufReader::new(tokio::io::stdin()).lines();
    let initialize_line = input
        .next_line()
        .await?
        .context("Harness client closed before initialize")?;
    let initialize_envelope: RoutedBrokerRequest =
        serde_json::from_str(&initialize_line).context("decode Harness initialize request")?;
    let initialize_request = initialize_envelope.request;
    anyhow::ensure!(
        initialize_request.method == "initialize",
        "first Harness request must initialize the broker"
    );
    let initialize: InitializeRequest = serde_json::from_value(initialize_request.params.clone())?;
    let runtime = BrokerRuntime::initialize(&initialize)?;
    let broker =
        match HarnessBroker::initialize_with_runtime(initialize.clone(), Arc::clone(&runtime)) {
            Ok(broker) => broker,
            Err(error) => {
                let response = initialize_failure(initialize_request.id, &error);
                let mut output = tokio::io::stdout();
                write_message(&mut output, &BrokerMessage::Response(response)).await?;
                return Ok(());
            }
        };
    let initial_snapshot = broker.snapshot()?;
    let initial_session_id = initial_snapshot.session.id.clone();
    let registry = SessionControllerRegistry::new(
        initial_session_id.clone(),
        SessionController::new(broker),
        runtime,
        initialize,
    );

    let (message_sink, mut message_stream) = mpsc::unbounded_channel::<BrokerMessage>();
    message_sink.send(BrokerMessage::Response(BrokerResponse::success(
        initialize_request.id,
        initial_snapshot,
    )?))?;
    let writer = tokio::spawn(async move {
        let mut output = tokio::io::stdout();
        while let Some(message) = message_stream.recv().await {
            write_message(&mut output, &message).await?;
        }
        Ok::<(), anyhow::Error>(())
    });

    while let Some(line) = input.next_line().await? {
        let envelope = match serde_json::from_str::<RoutedBrokerRequest>(&line) {
            Ok(envelope) => envelope,
            Err(error) => {
                message_sink.send(BrokerMessage::Response(BrokerResponse::failure(
                    0,
                    "invalid_request",
                    format!("decode Harness request: {error}"),
                )))?;
                continue;
            }
        };
        let shutdown = envelope.request.method == "shutdown";
        let session_id = envelope
            .session_id
            .unwrap_or_else(|| initial_session_id.clone());
        let registry = Arc::clone(&registry);
        let message_sink = message_sink.clone();
        tokio::task::spawn_local(async move {
            let request_id = envelope.request.id;
            if let Err(error) =
                route_request(registry, session_id, envelope.request, &message_sink).await
            {
                let _ = message_sink.send(BrokerMessage::Response(BrokerResponse::failure(
                    request_id,
                    "request_failed",
                    format!("{error:#}"),
                )));
            }
        });
        if shutdown {
            break;
        }
    }
    drop(message_sink);
    writer.await??;
    Ok(())
}

async fn route_request(
    registry: Arc<SessionControllerRegistry>,
    session_id: String,
    request: BrokerRequest,
    message_sink: &mpsc::UnboundedSender<BrokerMessage>,
) -> Result<()> {
    if request.method == "session.resume" {
        return resume_session(registry, request, message_sink).await;
    }
    let controller = registry.resolve(&session_id).await?;
    if route_control_request(&controller, &request, message_sink).await? {
        return Ok(());
    }

    controller
        .cancellation
        .arm(request.method == "prompt.submit");
    let (event_sink, mut event_stream) = mpsc::unbounded_channel();
    let (data_root, lease_session_id, client_id) = controller.broker.lock().await.lease_identity();
    let (heartbeat_stop, heartbeat_stopped) = tokio::sync::oneshot::channel();
    let heartbeat = tokio::spawn(run_lease_heartbeat(
        data_root,
        lease_session_id,
        client_id,
        heartbeat_stopped,
    ));
    let message_sink_for_event = message_sink.clone();
    let routed_session_id = session_id.clone();
    let event_forwarder = tokio::spawn(async move {
        while let Some(event) = event_stream.recv().await {
            let _ = message_sink_for_event.send(BrokerMessage::Event(BrokerEvent {
                session_id: routed_session_id.clone(),
                event: "backend_event".into(),
                payload: serde_json::to_value(event).unwrap_or(Value::Null),
            }));
        }
    });
    let (result, catalog_request) = {
        let mut broker = controller.broker.lock().await;
        let result = broker.dispatch_stream(request, event_sink).await;
        (result, broker.backend_catalog_request())
    };
    *controller.catalog_request.write().await = catalog_request;
    let _ = heartbeat_stop.send(());
    let _ = heartbeat.await;
    event_forwarder.await?;

    if let Some(child_session_id) = result
        .response
        .result
        .as_ref()
        .and_then(|value| value.pointer("/session/id"))
        .and_then(Value::as_str)
        .filter(|child_session_id| *child_session_id != session_id)
    {
        registry.register(child_session_id).await?;
    }
    for event in result.event {
        message_sink.send(BrokerMessage::Event(event))?;
    }
    message_sink.send(BrokerMessage::Response(result.response))?;
    Ok(())
}

async fn resume_session(
    registry: Arc<SessionControllerRegistry>,
    request: BrokerRequest,
    message_sink: &mpsc::UnboundedSender<BrokerMessage>,
) -> Result<()> {
    let target_session_id = request
        .params
        .get("session_id")
        .and_then(Value::as_str)
        .context("session.resume requires session_id")?;
    let controller = registry.resolve(target_session_id).await?;
    let snapshot = controller.broker.lock().await.snapshot()?;
    message_sink.send(BrokerMessage::Response(BrokerResponse::success(
        request.id, snapshot,
    )?))?;
    Ok(())
}

async fn route_control_request(
    controller: &SessionController,
    request: &BrokerRequest,
    message_sink: &mpsc::UnboundedSender<BrokerMessage>,
) -> Result<bool> {
    let catalog_request = controller.catalog_request.read().await.clone();
    let response = match request.method.as_str() {
        "turn.cancel" => {
            if let Some(target) = request.params.get("target").and_then(parse_steer_target) {
                controller.backend.interrupt_target(target).await?;
            } else {
                let restore_prompt = request
                    .params
                    .get("restore_prompt_if_no_output")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                controller.cancellation.request(restore_prompt);
                controller.permission.cancel_all(None)?;
            }
            Some(BrokerResponse::success(
                request.id,
                json!({ "cancel_requested": true }),
            )?)
        }
        "turn.restart" => {
            controller.cancellation.request(false);
            controller.permission.cancel_all(None)?;
            Some(BrokerResponse::success(
                request.id,
                json!({ "restart_requested": true, "mode": request.params.get("mode") }),
            )?)
        }
        "turn.steer" => {
            let text = request
                .params
                .get("text")
                .and_then(Value::as_str)
                .map(str::trim)
                .filter(|text| !text.is_empty())
                .context("turn.steer requires non-empty text")?
                .to_owned();
            match request.params.get("target").and_then(parse_steer_target) {
                Some(target) => controller.backend.steer_target(text, target).await?,
                None => {
                    controller
                        .backend
                        .steer_session(&catalog_request.harness_session_id, text)
                        .await?
                }
            }
            Some(BrokerResponse::success(
                request.id,
                json!({ "steered": true }),
            )?)
        }
        "approval.resolve" => {
            let approval_id = request
                .params
                .get("approval_id")
                .and_then(Value::as_str)
                .context("approval.resolve requires approval_id")?;
            let choice_id = request
                .params
                .get("choice_id")
                .and_then(Value::as_str)
                .context("approval.resolve requires choice_id")?;
            let approval = controller
                .permission
                .resolve(approval_id, choice_id, None)?;
            Some(BrokerResponse::success(
                request.id,
                json!({ "resolved": true, "approval": approval }),
            )?)
        }
        "backend.skills" => Some(BrokerResponse::success(
            request.id,
            controller
                .backend
                .skill_list(catalog_request.clone())
                .await?,
        )?),
        "backend.skills.set_enabled" => {
            let name = request
                .params
                .get("name")
                .and_then(Value::as_str)
                .context("backend.skills.set_enabled requires name")?;
            let enabled = request
                .params
                .get("enabled")
                .and_then(Value::as_bool)
                .context("backend.skills.set_enabled requires enabled")?;
            Some(BrokerResponse::success(
                request.id,
                controller
                    .backend
                    .set_skill_enabled(catalog_request.clone(), name, enabled)
                    .await?,
            )?)
        }
        "backend.mcp" => {
            let server_list = tokio::time::timeout(
                Duration::from_secs(30),
                controller.backend.mcp_list(catalog_request.clone()),
            )
            .await
            .context("MCP status did not finish within 30 seconds")??;
            Some(BrokerResponse::success(request.id, server_list)?)
        }
        "backend.mcp.set_enabled" => {
            let name = request
                .params
                .get("name")
                .and_then(Value::as_str)
                .context("backend.mcp.set_enabled requires name")?;
            let enabled = request
                .params
                .get("enabled")
                .and_then(Value::as_bool)
                .context("backend.mcp.set_enabled requires enabled")?;
            let capability = controller.backend.descriptor().capability.catalog;
            let interrupted = !capability.live_mcp_mutation
                && controller
                    .backend
                    .has_active_turn(&catalog_request.harness_session_id)
                    .await;
            if interrupted {
                controller.cancellation.request(false);
                controller.permission.cancel_all(None)?;
            }
            let mut mutation = tokio::time::timeout(
                Duration::from_secs(30),
                controller
                    .backend
                    .set_mcp_enabled(catalog_request, name, enabled),
            )
            .await
            .context("MCP startup did not finish within 30 seconds")??;
            mutation.restart_required |= interrupted;
            Some(BrokerResponse::success(request.id, mutation)?)
        }
        _ => None,
    };
    if let Some(response) = response {
        message_sink.send(BrokerMessage::Response(response))?;
        return Ok(true);
    }
    Ok(false)
}

fn initialize_failure(request_id: u64, error: &anyhow::Error) -> BrokerResponse {
    error.downcast_ref::<SessionLeaseConflict>().map_or_else(
        || BrokerResponse::failure(request_id, "initialize_failed", format!("{error:#}")),
        |conflict| {
            BrokerResponse::failure_with_data(
                request_id,
                "session_lease_conflict",
                conflict.to_string(),
                json!({
                    "session_id": conflict.session_id,
                    "native_fork": conflict.native_fork,
                }),
            )
        },
    )
}

fn parse_steer_target(value: &Value) -> Option<diff_review_harness::backend::SteerTarget> {
    Some(diff_review_harness::backend::SteerTarget {
        thread_id: value.get("thread_id")?.as_str()?.to_owned(),
        turn_id: value.get("turn_id")?.as_str()?.to_owned(),
    })
}

async fn run_lease_heartbeat(
    data_root: PathBuf,
    session_id: String,
    client_id: String,
    mut stopped: tokio::sync::oneshot::Receiver<()>,
) {
    let mut interval = tokio::time::interval(Duration::from_secs(10));
    interval.tick().await;
    loop {
        tokio::select! {
            _ = &mut stopped => return,
            _ = interval.tick() => {
                let now_ms = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_millis() as i64;
                if let Ok(mut store) = SqliteStore::open(&data_root) {
                    let _ = store.renew_session_lease(&session_id, &client_id, now_ms);
                }
            }
        }
    }
}

async fn write_message(output: &mut tokio::io::Stdout, message: &BrokerMessage) -> Result<()> {
    output
        .write_all(serde_json::to_string(message)?.as_bytes())
        .await?;
    output.write_all(b"\n").await?;
    output
        .flush()
        .await
        .context("flush Harness broker response")
}
