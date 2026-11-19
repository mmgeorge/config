//! A session whose provider could not be restarted reopens from storage through
//! `session.resume`, keeping its transcript and input, while a healthy session keeps its owner,
//! and input sent into the failing turn settles with it instead of holding the restart.

use super::super::{HarnessService, SessionController, SessionControllerRegistry};
use super::bootstrap::{initialize, service};
use super::owner_transition::submit_input;
use crate::input::InputDelivery;
use crate::provider::mock_response::MockResponseCommand;
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use forge_protocol::outbound::MessageSender;
use serde_json::{Value, json};
use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// Bound each wait; the paused clock runs it out at once when the awaited state never arrives.
const BOUND: Duration = Duration::from_secs(600);
/// Poll on the paused clock, which lets the bound expire instead of starving it.
const POLL: Duration = Duration::from_secs(1);

/// Fail a turn after dispatch, which restarts the provider and leaves its input settled.
static FAILED_TURN: std::sync::LazyLock<Value> =
    std::sync::LazyLock::new(|| json!({"failed":"protocol"}));

/// Collect every frame the host sends, so publications never wait on a full output.
struct Output {
    message: Arc<std::sync::Mutex<Vec<Value>>>,
    reader: tokio::task::JoinHandle<Result<()>>,
}

impl Output {
    fn start(mut output: forge_protocol::outbound::MessageReceiver) -> Self {
        let message = Arc::new(std::sync::Mutex::new(Vec::new()));
        let received = message.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                received
                    .lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok(())
        });
        Self { message, reader }
    }

    /// Wait for the response to one request.
    async fn response(&self, id: u64) -> Result<Value> {
        tokio::time::timeout(BOUND, async {
            loop {
                if let Some(response) = self
                    .message
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|message| message["id"] == id)
                    .cloned()
                {
                    return response;
                }
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .with_context(|| format!("request {id} was never answered"))
    }
}

/// Drive one mock session whose next provider restart fails.
struct FailingSession {
    service: HarnessService,
    registry: Arc<SessionControllerRegistry>,
    controller: Arc<SessionController>,
    session: String,
    sink: MessageSender,
    output: Output,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _root: tempfile::TempDir,
}

impl FailingSession {
    /// Start the session and script how its next turn fails, which restarts the provider.
    async fn start(steps: Value, terminal: Value) -> Result<Self> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let service = service();
        service.open_session(1, initialize(&root, "mock")).await?;
        let registry = service.registry().await?;
        let session = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session).await?;
        let (sink, output) = forge_protocol::outbound::channel();
        let output = Output::start(output);
        controller.start_provider(&sink).await;
        controller.attach_input(&sink).await?;
        // Startup must finish first, so only the restart meets the failing initialization.
        tokio::time::timeout(BOUND, async {
            while !controller.provider_ready().await {
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("provider startup never finished")?;
        controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?
            .dispatch(
                &session,
                MockResponseCommand::Script {
                    script: serde_json::from_value(json!({"steps":steps,"terminal":terminal}))?,
                },
            )
            .await?;
        controller
            .initialization_fails
            .store(true, Ordering::Release);
        Ok(Self {
            service,
            registry,
            controller,
            session,
            sink,
            output,
            _root: root,
        })
    }

    /// Submit one input with an explicit intent through the public input request.
    async fn submit(&self, request_id: u64, input_id: &str, intent: &str) -> Result<()> {
        let control_revision = self
            .controller
            .coordinator
            .submissions
            .snapshot()?
            .control_revision;
        self.service
            .dispatch(
                Some(self.session.clone()),
                Request {
                    id: request_id,
                    method: "session.input.apply".into(),
                    params: json!({"id":input_id,"control_revision":control_revision,
                        "action":{"kind":"submit","message":{
                        "id":input_id,"text":input_id,"intent":intent,"attachments":[]}}}),
                },
                &self.sink,
            )
            .await
    }

    /// Wait until the owner publishes that the session cannot execute, and get its reason.
    async fn unavailable(&self) -> Result<String> {
        tokio::time::timeout(BOUND, async {
            loop {
                let activity = self
                    .controller
                    .observer
                    .activity
                    .read()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone();
                if activity.phase == crate::exchange::ActivityPhase::Unavailable {
                    return activity.unavailable_reason.unwrap_or_default();
                }
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("the failed restart never made the session unavailable")
    }

    /// Resume the session as the editor does, and get the controller that now owns it.
    async fn resume(&self, request_id: u64) -> Result<Arc<SessionController>> {
        self.service
            .dispatch(
                Some(self.session.clone()),
                Request {
                    id: request_id,
                    method: "session.resume".into(),
                    params: json!({"session_id":self.session}),
                },
                &self.sink,
            )
            .await?;
        let resumed = self.output.response(request_id).await?;
        anyhow::ensure!(resumed.get("error").is_none(), "{resumed}");
        self.registry.resolve(&self.session).await
    }

    /// Wait until the exchange for one input finishes.
    async fn exchange_finished(&self, controller: &SessionController, input: &str) -> Result<()> {
        tokio::time::timeout(BOUND, async {
            loop {
                // Read storage only once the queue has drained, so a stuck owner exhausts the
                // bound quickly instead of opening the database at every poll. The read-only
                // handle never contends with the owner's writes.
                if !controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
                {
                    tokio::time::sleep(POLL).await;
                    continue;
                }
                let finished = SqliteStore::read_only(&controller.data_root)?
                    .list_exchange(&self.session)?
                    .iter()
                    .any(|exchange| {
                        exchange.input_message_id.as_deref() == Some(input)
                            && exchange.is_finished()
                    });
                if finished {
                    return Ok::<(), anyhow::Error>(());
                }
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .with_context(|| format!("the exchange for {input} never finished"))?
    }

    /// Shut the host down after the scenario, reporting the scenario's failure first.
    async fn finish(self, scenario: Result<()>) -> Result<()> {
        let stopped =
            tokio::time::timeout(BOUND, self.service.shutdown(Duration::from_secs(5))).await;
        self.output.reader.abort();
        scenario?;
        stopped.context("the host never shut down")??;
        Ok(())
    }
}

/// List each queued input's identity and delivery phase, oldest first.
fn queue(controller: &SessionController) -> Result<Vec<(String, InputDelivery)>> {
    Ok(controller
        .coordinator
        .submissions
        .snapshot()?
        .messages
        .into_iter()
        .map(|message| (message.id.clone(), message.delivery()))
        .collect())
}

/// A session whose restart failed accepts a prompt once `session.resume` reopens it.
///
/// The failed owner refuses every prompt and never executes again, so resuming the session
/// must replace it with a controller resolved from storage.
#[tokio::test(start_paused = true)]
async fn reopening_a_failed_session_accepts_a_prompt() -> Result<()> {
    let fixture = FailingSession::start(json!([]), FAILED_TURN.clone()).await?;
    let scenario = async {
        submit_input(
            &fixture.service,
            &fixture.controller,
            &fixture.session,
            2,
            "failing-input",
            "This turn fails",
            &fixture.sink,
        )
        .await?;
        let reason = fixture.unavailable().await?;
        assert!(reason.contains("could not be restarted"), "{reason}");
        // The listener of the replaced generation ends rather than waking the owner forever.
        tokio::time::timeout(BOUND, async {
            while !fixture
                .controller
                .session_event_task
                .lock()
                .await
                .as_ref()
                .is_none_or(|task| task.is_finished())
            {
                tokio::time::sleep(POLL).await;
            }
        })
        .await
        .context("the failed generation's observation listener never ended")?;
        let reopened = fixture.resume(3).await?;
        assert!(
            !Arc::ptr_eq(&fixture.controller, &reopened),
            "session.resume kept the failed owner"
        );
        reopened.start_provider(&fixture.sink).await;
        reopened.attach_input(&fixture.sink).await?;
        submit_input(
            &fixture.service,
            &reopened,
            &fixture.session,
            4,
            "after-reopen",
            "Answer after the reopen",
            &fixture.sink,
        )
        .await?;
        fixture.exchange_finished(&reopened, "after-reopen").await
    }
    .await;
    fixture.finish(scenario).await
}

/// Reopening keeps the transcript and the input the failed owner left queued, and the queue
/// delivers once the user resumes it.
#[tokio::test(start_paused = true)]
async fn reopening_a_failed_session_keeps_its_transcript_and_queue() -> Result<()> {
    let fixture = FailingSession::start(
        json!([{"kind":"barrier","id":"failing-turn"}]),
        FAILED_TURN.clone(),
    )
    .await?;
    let script = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    let scenario = async {
        fixture.submit(2, "failing-input", "auto").await?;
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Wait {
                    id: "failing-turn".into(),
                },
            )
            .await?;
        fixture.submit(3, "queued-follow-up", "queue").await?;
        // Steering prepared while the turn fails must settle with it rather than hold the restart.
        fixture.submit(6, "auto-follow-up", "auto").await?;
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Release {
                    id: "failing-turn".into(),
                },
            )
            .await?;
        fixture.unavailable().await?;
        let failed_queue = queue(&fixture.controller)?;
        assert_eq!(
            failed_queue,
            [
                ("auto-follow-up".to_owned(), InputDelivery::Queued),
                ("queued-follow-up".to_owned(), InputDelivery::Queued),
            ],
            "the failed owner did not keep the follow-ups queued"
        );
        let exchanges = SqliteStore::read_only(&fixture.controller.data_root)?
            .list_exchange(&fixture.session)?
            .len();
        let reopened = fixture.resume(4).await?;
        assert_eq!(
            queue(&reopened)?,
            failed_queue,
            "the reopen changed the input queue"
        );
        assert_eq!(
            SqliteStore::read_only(&reopened.data_root)?
                .list_exchange(&fixture.session)?
                .len(),
            exchanges,
            "the reopen changed the transcript"
        );
        reopened.attach_input(&fixture.sink).await?;
        // A queue loaded from storage starts paused, as after a host restart, so the reopened
        // session waits for the user to resume it instead of sending it unasked.
        assert!(
            reopened.coordinator.submissions.snapshot()?.suspended,
            "the reopened session sent its saved queue unasked"
        );
        let control_revision = reopened
            .coordinator
            .submissions
            .snapshot()?
            .control_revision;
        fixture
            .service
            .dispatch(
                Some(fixture.session.clone()),
                Request {
                    id: 5,
                    method: "session.input.apply".into(),
                    params: json!({"id":"resume-queue","control_revision":control_revision,
                        "action":{"kind":"resume"}}),
                },
                &fixture.sink,
            )
            .await?;
        let resumed = fixture.output.response(5).await?;
        anyhow::ensure!(resumed.get("error").is_none(), "{resumed}");
        fixture
            .exchange_finished(&reopened, "queued-follow-up")
            .await
    }
    .await;
    fixture.finish(scenario).await
}

/// Reopening keeps an input whose delivery the failed owner could not confirm, which still
/// waits for the user to reconcile it.
#[tokio::test(start_paused = true)]
async fn reopening_a_failed_session_keeps_unconfirmed_input() -> Result<()> {
    let fixture = FailingSession::start(json!([]), json!("delivery_uncertain")).await?;
    let scenario = async {
        fixture.submit(2, "unconfirmed-input", "auto").await?;
        fixture.unavailable().await?;
        let failed_queue = queue(&fixture.controller)?;
        assert_eq!(
            failed_queue,
            [("unconfirmed-input".to_owned(), InputDelivery::Unknown)],
            "the failed owner did not keep the unconfirmed input"
        );
        let reopened = fixture.resume(3).await?;
        assert!(
            !Arc::ptr_eq(&fixture.controller, &reopened),
            "session.resume kept the failed owner"
        );
        assert_eq!(
            queue(&reopened)?,
            failed_queue,
            "the reopen dropped the unconfirmed input"
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Resuming a healthy loaded session keeps its owner.
#[tokio::test(start_paused = true)]
async fn resuming_a_healthy_session_keeps_its_owner() -> Result<()> {
    let fixture = FailingSession::start(json!([]), FAILED_TURN.clone()).await?;
    let scenario = async {
        let resumed = fixture.resume(2).await?;
        assert!(
            Arc::ptr_eq(&fixture.controller, &resumed),
            "session.resume replaced a healthy owner"
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A turn that fails refuses steering captured while it ran, rather than queueing it unread.
///
/// Steering captures the running turn's lane when the owner admits it and sends after its own
/// preparation, which can finish after the turn failed. The failed turn stays retained until a
/// restart replaces it, and the restart waits for that steering to settle, so a send the lane
/// still accepted waited for an answer no one would give. The test holds the captured lane as
/// an admitted delivery does and sends once the restart has failed, which leaves the failed turn
/// retained for good: the send must be refused at once.
#[tokio::test(start_paused = true)]
async fn a_failed_turn_refuses_steering_captured_while_it_ran() -> Result<()> {
    let fixture = FailingSession::start(
        json!([{"kind":"barrier","id":"failing-turn"}]),
        FAILED_TURN.clone(),
    )
    .await?;
    let script = fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?;
    let scenario = async {
        fixture.submit(2, "failing-input", "auto").await?;
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Wait {
                    id: "failing-turn".into(),
                },
            )
            .await?;
        let target = fixture
            .controller
            .broker
            .lock()
            .await
            .parent_input
            .capture()
            .context("the running turn exposed no steering lane")?;
        script
            .dispatch(
                &fixture.session,
                MockResponseCommand::Release {
                    id: "failing-turn".into(),
                },
            )
            .await?;
        fixture.unavailable().await?;
        assert!(!target.is_open(), "the failed turn kept accepting steering");
        let sent = tokio::time::timeout(BOUND, target.steer("Sent after the turn failed".into()))
            .await
            .context("steering into the failed turn was never answered")?;
        assert!(sent.is_err(), "the failed turn accepted steering");
        // Reopen the failed session, so host shutdown collects a healthy owner.
        fixture.resume(3).await?;
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Fail the session's failing turn and every close of its binding, then resume it.
///
/// The failed owner can then never retire its binding, so only forced termination lets the
/// reopen release the session; before it existed the reopen waited out its bound and failed.
async fn reopen_after_failed_close(fixture: &FailingSession) -> Result<Arc<SessionController>> {
    fixture
        .controller
        .provider_handle()
        .mock_response_control()
        .context("mock controls missing")?
        .dispatch(&fixture.session, MockResponseCommand::FailClose)
        .await?;
    submit_input(
        &fixture.service,
        &fixture.controller,
        &fixture.session,
        2,
        "failing-input",
        "This turn fails",
        &fixture.sink,
    )
    .await?;
    fixture.unavailable().await?;
    fixture.resume(3).await
}

/// A failed owner whose binding close keeps failing still reopens into a healthy controller.
#[tokio::test(start_paused = true)]
async fn reopening_terminates_a_binding_whose_close_fails() -> Result<()> {
    let fixture = FailingSession::start(json!([]), FAILED_TURN.clone()).await?;
    let scenario = async {
        let reopened = reopen_after_failed_close(&fixture).await?;
        assert!(
            !Arc::ptr_eq(&fixture.controller, &reopened),
            "session.resume kept the failed owner"
        );
        assert!(
            !reopened.execution_failed(),
            "the reopened controller is not healthy"
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// The controller reopened over a terminated binding accepts and finishes a prompt.
#[tokio::test(start_paused = true)]
async fn session_reopened_over_a_terminated_binding_accepts_a_prompt() -> Result<()> {
    let fixture = FailingSession::start(json!([]), FAILED_TURN.clone()).await?;
    let scenario = async {
        let reopened = reopen_after_failed_close(&fixture).await?;
        reopened.start_provider(&fixture.sink).await;
        reopened.attach_input(&fixture.sink).await?;
        submit_input(
            &fixture.service,
            &reopened,
            &fixture.session,
            4,
            "after-reopen",
            "Answer after the reopen",
            &fixture.sink,
        )
        .await?;
        fixture.exchange_finished(&reopened, "after-reopen").await
    }
    .await;
    fixture.finish(scenario).await
}
