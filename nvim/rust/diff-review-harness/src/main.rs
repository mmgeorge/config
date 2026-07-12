use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use diff_review_harness::backend::BackendEvent;
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::{BrokerEvent, BrokerMessage, BrokerRequest, BrokerResponse};
use diff_review_harness::storage::SqliteStore;
use std::path::PathBuf;
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
            write_message(
                &mut output,
                &BrokerMessage::Response(BrokerResponse::failure(
                    initialize_request.id,
                    "initialize_failed",
                    format!("{error:#}"),
                )),
            )
            .await?;
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

    while let Some(line) = input.next_line().await? {
        let request: BrokerRequest = match serde_json::from_str(&line) {
            Ok(request) => request,
            Err(error) => {
                write_message(
                    &mut output,
                    &BrokerMessage::Response(BrokerResponse::failure(
                        0,
                        "invalid_request",
                        format!("decode Harness request: {error}"),
                    )),
                )
                .await?;
                continue;
            }
        };
        let shutdown = request.method == "shutdown";
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
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
        let result = loop {
            tokio::select! {
                Some(event) = event_stream.recv() => {
                    write_backend_event(&mut output, event).await?;
                }
                result = &mut dispatch => break result,
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
