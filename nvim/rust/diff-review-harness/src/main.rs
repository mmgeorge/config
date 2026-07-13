use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use diff_review_harness::backend::BackendEvent;
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::{BrokerEvent, BrokerMessage, BrokerRequest, BrokerResponse};
use diff_review_harness::session::SessionLeaseConflict;
use diff_review_harness::storage::SqliteStore;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

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

#[tokio::main]
async fn main() -> Result<()> {
    match Argument::parse().command {
        Some(Command::Mcp) => diff_review_harness::control_tools::run_stdio().await,
        None => run_broker().await,
    }
}

async fn run_broker() -> Result<()> {
    let mut input = BufReader::new(tokio::io::stdin()).lines();
    let mut output = tokio::io::stdout();
    let initialize_line = input
        .next_line()
        .await?
        .context("Harness client closed before initialize")?;
    let initialize_request: BrokerRequest =
        serde_json::from_str(&initialize_line).context("decode Harness initialize request")?;
    anyhow::ensure!(
        initialize_request.method == "initialize",
        "first Harness request must initialize the broker"
    );
    let initialize: InitializeRequest = serde_json::from_value(initialize_request.params)?;
    let mut broker = match HarnessBroker::initialize(initialize) {
        Ok(broker) => broker,
        Err(error) => {
            let response = error.downcast_ref::<SessionLeaseConflict>().map_or_else(
                || {
                    BrokerResponse::failure(
                        initialize_request.id,
                        "initialize_failed",
                        format!("{error:#}"),
                    )
                },
                |conflict| {
                    BrokerResponse::failure_with_data(
                        initialize_request.id,
                        "session_lease_conflict",
                        conflict.to_string(),
                        serde_json::json!({
                            "session_id": conflict.session_id,
                            "native_fork": conflict.native_fork,
                        }),
                    )
                },
            );
            write_message(&mut output, &BrokerMessage::Response(response)).await?;
            return Ok(());
        }
    };
    write_message(
        &mut output,
        &BrokerMessage::Response(BrokerResponse::success(
            initialize_request.id,
            broker.snapshot()?,
        )?),
    )
    .await?;

    let cancellation = broker.turn_cancellation();
    let mut pending_request: VecDeque<BrokerRequest> = VecDeque::new();
    let mut input_closed = false;
    loop {
        let request = match pending_request.pop_front() {
            Some(request) => request,
            None if input_closed => break,
            None => {
                let Some(line) = input.next_line().await? else {
                    break;
                };
                match serde_json::from_str(&line) {
                    Ok(request) => request,
                    Err(error) => {
                        write_invalid_request(&mut output, error).await?;
                        continue;
                    }
                }
            }
        };
        if request.method == "turn.cancel" {
            write_message(
                &mut output,
                &BrokerMessage::Response(BrokerResponse::success(
                    request.id,
                    serde_json::json!({ "cancel_requested": false }),
                )?),
            )
            .await?;
            continue;
        }
        if request.method == "turn.steer" {
            write_message(
                &mut output,
                &BrokerMessage::Response(BrokerResponse::failure(
                    request.id,
                    "steer_unavailable",
                    "Harness has no active turn to steer",
                )),
            )
            .await?;
            continue;
        }
        let shutdown = request.method == "shutdown";
        cancellation.arm();
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
        let backend = broker.backend_handle();
        let (steer_result_sink, mut steer_result_stream) = tokio::sync::mpsc::unbounded_channel();
        let mut pending_steer_count = 0_usize;
        let (data_root, session_id, client_id) = broker.lease_identity();
        let (heartbeat_stop, heartbeat_stopped) = tokio::sync::oneshot::channel();
        let heartbeat = tokio::spawn(run_lease_heartbeat(
            data_root,
            session_id,
            client_id,
            heartbeat_stopped,
        ));
        let dispatch = broker.dispatch_stream(request, event_sink);
        tokio::pin!(dispatch);
        let mut dispatch_result = None;
        let result = loop {
            if dispatch_result.is_some() && pending_steer_count == 0 {
                break dispatch_result.take().expect("dispatch result is present");
            }
            tokio::select! {
                Some(event) = event_stream.recv() => {
                    write_backend_event(&mut output, event).await?;
                }
                Some((request_id, steer_result)) = steer_result_stream.recv(), if pending_steer_count > 0 => {
                    pending_steer_count -= 1;
                    let response = match steer_result {
                        Ok(()) => BrokerResponse::success(
                            request_id,
                            serde_json::json!({ "steered": true }),
                        )?,
                        Err(error) => BrokerResponse::failure(
                            request_id,
                            "steer_failed",
                            format!("{error:#}"),
                        ),
                    };
                    write_message(&mut output, &BrokerMessage::Response(response)).await?;
                }
                line = input.next_line(), if !input_closed => {
                    match line? {
                        Some(line) => match serde_json::from_str::<BrokerRequest>(&line) {
                            Ok(cancel_request) if cancel_request.method == "turn.cancel" => {
                                cancellation.request();
                                write_message(
                                    &mut output,
                                    &BrokerMessage::Response(BrokerResponse::success(
                                        cancel_request.id,
                                        serde_json::json!({ "cancel_requested": true }),
                                    )?),
                                ).await?;
                            }
                            Ok(steer_request) if steer_request.method == "turn.steer" => {
                                let text = steer_request
                                    .params
                                    .get("text")
                                    .and_then(serde_json::Value::as_str)
                                    .map(str::trim)
                                    .filter(|text| !text.is_empty())
                                    .map(str::to_owned);
                                if let Some(text) = text {
                                    pending_steer_count += 1;
                                    let backend = Arc::clone(&backend);
                                    let steer_result_sink = steer_result_sink.clone();
                                    tokio::spawn(async move {
                                        let result = backend.steer(text).await;
                                        let _ = steer_result_sink.send((steer_request.id, result));
                                    });
                                } else {
                                    write_message(
                                        &mut output,
                                        &BrokerMessage::Response(BrokerResponse::failure(
                                            steer_request.id,
                                            "invalid_request",
                                            "turn.steer requires non-empty text",
                                        )),
                                    ).await?;
                                }
                            }
                            Ok(next_request) => pending_request.push_back(next_request),
                            Err(error) => write_invalid_request(&mut output, error).await?,
                        },
                        None => input_closed = true,
                    }
                }
                result = &mut dispatch, if dispatch_result.is_none() => {
                    dispatch_result = Some(result);
                }
            }
        };
        while let Ok(event) = event_stream.try_recv() {
            write_backend_event(&mut output, event).await?;
        }
        let _ = heartbeat_stop.send(());
        let _ = heartbeat.await;
        for event in result.event {
            write_message(&mut output, &BrokerMessage::Event(event)).await?;
        }
        write_message(&mut output, &BrokerMessage::Response(result.response)).await?;
        if shutdown {
            break;
        }
    }
    Ok(())
}

async fn write_invalid_request(
    output: &mut tokio::io::Stdout,
    error: serde_json::Error,
) -> Result<()> {
    write_message(
        output,
        &BrokerMessage::Response(BrokerResponse::failure(
            0,
            "invalid_request",
            format!("decode Harness request: {error}"),
        )),
    )
    .await
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

async fn write_backend_event(output: &mut tokio::io::Stdout, event: BackendEvent) -> Result<()> {
    write_message(
        output,
        &BrokerMessage::Event(BrokerEvent {
            event: "backend_event".into(),
            payload: serde_json::to_value(event)?,
        }),
    )
    .await
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
