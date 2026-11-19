//! A stop whose native collection never returns still leaves `Stopping` without an editor
//! restart: the stop backstop force-terminates the binding, and the exchange settles cancelled.

use super::bootstrap::{initialize, service};
use super::owner_transition::submit_input;
use crate::exchange::command::{
    CancelExchangeRequest, Command, CommandFailure, CommandId, CommandRequest, ExecuteCommandError,
    InputControl,
};
use crate::provider::mock_response::MockResponseCommand;
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use std::time::Duration;

/// Bound each wait; the paused clock runs it out at once when the awaited state never arrives.
const BOUND: Duration = Duration::from_secs(600);
/// Poll on the paused clock, which lets the bound expire instead of starving it.
const POLL: Duration = Duration::from_secs(1);

/// Cancel a held turn whose interrupt never settles, and expect the backstop to settle it.
#[tokio::test(start_paused = true)]
async fn a_stop_whose_interrupt_never_returns_settles_cancelled() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        tokio::time::timeout(BOUND, async {
            while !controller.provider_ready().await {
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("provider startup never finished")?;
        let script = controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?;
        let held = script
            .dispatch(
                &session,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold = held["id"].as_str().context("hold has no id")?.to_owned();
        script
            .dispatch(&session, MockResponseCommand::HoldInterrupt)
            .await?;
        submit_input(
            &service,
            &controller,
            &session,
            2,
            "held-input",
            "Held",
            &sink,
        )
        .await?;
        script
            .dispatch(&session, MockResponseCommand::Wait { id: hold })
            .await?;
        let cancelled = controller
            .exchange_owner
            .execute_identified(
                CommandRequest {
                    id: CommandId::new("stuck-cancel")?,
                    command: Command::CancelExchange(CancelExchangeRequest::default()),
                    input_control: InputControl::AtExecution,
                },
                sink.clone(),
            )
            .await;
        anyhow::ensure!(
            matches!(
                cancelled,
                Err(ExecuteCommandError::Failed(CommandFailure::StopTimedOut))
            ),
            "the stuck cancel did not answer at the stop deadline: {cancelled:?}"
        );
        let settled = tokio::time::timeout(BOUND, async {
            loop {
                let exchange = SqliteStore::read_only(&controller.data_root)?
                    .list_exchange(&session)?
                    .into_iter()
                    .find(|exchange| exchange.input_message_id.as_deref() == Some("held-input"));
                if let Some(exchange) = exchange.filter(|exchange| exchange.is_finished()) {
                    return Ok::<_, anyhow::Error>(exchange.state());
                }
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .with_context(|| {
            let exchanges = SqliteStore::read_only(&controller.data_root)
                .and_then(|store| store.list_exchange(&session))
                .map(|exchanges| {
                    exchanges
                        .iter()
                        .map(|exchange| (exchange.input_message_id.clone(), exchange.state()))
                        .collect::<Vec<_>>()
                });
            let activity = controller
                .observer
                .activity
                .read()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .phase;
            format!("the stuck stop never settled its exchange: {exchanges:?}, {activity:?}")
        })??;
        anyhow::ensure!(
            settled == crate::exchange::ExchangeState::Cancelled,
            "the backstop settled the exchange as {settled:?}"
        );
        // The provider restarted, so the session runs a later prompt.
        tokio::time::timeout(BOUND, async {
            while !controller.provider_ready().await {
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("the provider never restarted after the backstop")?;
        submit_input(
            &service,
            &controller,
            &session,
            3,
            "after-stop",
            "After",
            &sink,
        )
        .await?;
        tokio::time::timeout(BOUND, async {
            loop {
                let finished = SqliteStore::read_only(&controller.data_root)?
                    .list_exchange(&session)?
                    .iter()
                    .any(|exchange| {
                        exchange.input_message_id.as_deref() == Some("after-stop")
                            && exchange.is_finished()
                    });
                if finished {
                    return Ok::<(), anyhow::Error>(());
                }
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("the session ran no prompt after the backstop")?
    }
    .await;
    let stopped = tokio::time::timeout(BOUND, service.shutdown(Duration::from_secs(5))).await;
    reader.abort();
    scenario?;
    stopped.context("the host never shut down")??;
    Ok(())
}
