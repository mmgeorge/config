//! Rollback through the session owner: which interactions it can rewind, what it collects first,
//! and what task and timeline state survives it.
use super::super::SessionController;
use super::bootstrap::service;
use super::owner_transition::{settle_exchanges, submit_input};
use crate::provider::mock_response::{MockResponseCommand, MockResponseControl};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};
use std::time::Duration;

/// Own one mock session, its outbound frames, and the provider's scripted responses.
struct RollbackScene {
    _workspace: tempfile::TempDir,
    _data: tempfile::TempDir,
    service: super::super::HarnessService,
    controller: Arc<SessionController>,
    session_id: String,
    script: Arc<MockResponseControl>,
    sink: forge_protocol::outbound::MessageSender,
    frames: Arc<Mutex<Vec<Value>>>,
    reader: tokio::task::JoinHandle<Result<()>>,
}

impl RollbackScene {
    /// Open a mock session on a committed Git workspace whose data lives outside that workspace.
    async fn start() -> Result<Self> {
        let workspace = crate::broker::test::git_repository();
        crate::checks::configure_fixture(workspace.path());
        let data = tempfile::tempdir()?;
        let service = service();
        let request: super::super::InitializeRequest = serde_json::from_value(json!({
            "workspace":workspace.path(),
            "data_root":data.path(),
            "client_id":"service-test",
            "provider":{"kind":"mock","command":["mock"]},
        }))?;
        service.open_session(1, request).await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let script = Arc::new(
            controller
                .provider_handle()
                .mock_response_control()
                .context("mock controls missing")?,
        );
        let (sink, mut output) = forge_protocol::outbound::channel();
        let frames = Arc::new(Mutex::new(Vec::<Value>::new()));
        let received = frames.clone();
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                received
                    .lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok::<(), anyhow::Error>(())
        });
        controller.start_provider(&sink).await;
        controller.attach_input(&sink).await?;
        Ok(Self {
            _workspace: workspace,
            _data: data,
            service,
            controller,
            session_id,
            script,
            sink,
            frames,
            reader,
        })
    }

    /// Submit one prompt and wait until `count` exchanges have settled.
    async fn exchange(&self, request_id: u64, text: &str, count: usize) -> Result<()> {
        submit_input(
            &self.service,
            &self.controller,
            &self.session_id,
            request_id,
            &format!("input-{request_id}"),
            text,
            &self.sink,
        )
        .await?;
        settle_exchanges(&self.controller, &self.session_id, count).await
    }

    /// Start a prompt the provider holds open, and return once the provider is executing it.
    async fn held_exchange(&self, request_id: u64, text: &str) -> Result<String> {
        let hold = self
            .script
            .dispatch(
                &self.session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
        submit_input(
            &self.service,
            &self.controller,
            &self.session_id,
            request_id,
            &format!("input-{request_id}"),
            text,
            &self.sink,
        )
        .await?;
        self.script
            .dispatch(
                &self.session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await?;
        Ok(hold_id)
    }

    /// Send one request and return its response frame.
    async fn request(&self, id: u64, method: &str, params: Value) -> Result<Value> {
        self.service
            .dispatch(
                Some(self.session_id.clone()),
                Request {
                    id,
                    method: method.into(),
                    params,
                },
                &self.sink,
            )
            .await?;
        tokio::time::timeout(Duration::from_secs(4), async {
            loop {
                let response = self
                    .frames
                    .lock()
                    .unwrap()
                    .iter()
                    .find(|frame| frame["id"] == id && frame.get("method").is_none())
                    .cloned();
                if let Some(response) = response {
                    break response;
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .with_context(|| format!("{method} was never answered"))
    }

    /// Roll back the conversation to before `exchange_id`, with the latest exchange as authority.
    async fn conversation_undo(&self, id: u64, exchange_id: &str) -> Result<Value> {
        let latest = self.exchanges()?.last().context("no exchanges")?.id.clone();
        self.request(
            id,
            "session.command.apply",
            json!({"operation_id":format!("undo-{id}"),"action":"undo","rollback":{
                "scope":"conversation","exchange_id":exchange_id,"latest_exchange_id":latest}}),
        )
        .await
    }

    fn exchanges(&self) -> Result<Vec<crate::exchange::Exchange>> {
        crate::storage::HistoryReader::current(&self.controller.data_root)?
            .list_exchange(&self.session_id)
    }

    /// Install a paused goal whose start the clock places at `created_at_ms`.
    fn install_paused_goal(&self, created_at_ms: i64) -> Result<()> {
        let mut store = SqliteStore::open(&self.controller.data_root)?;
        self.controller.task_controller.change(
            &mut store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Keep the goal across rollback".into(),
                Default::default(),
                created_at_ms,
            ))),
            created_at_ms,
        )?;
        let task = self
            .controller
            .task_controller
            .current()?
            .context("goal missing")?;
        self.controller.task_controller.change(
            &mut store,
            Some(&task.revision),
            crate::task::TaskChange::Pause,
            created_at_ms,
        )?;
        Ok(())
    }

    /// Collect the visible top-level session rows of the projected timeline.
    fn session_rows(&self) -> Result<Vec<crate::timeline::SessionEventKind>> {
        Ok(self
            .controller
            .observer
            .snapshot()?
            .timeline
            .into_iter()
            .filter_map(|entry| match entry {
                crate::timeline::TimelineEntry::SessionEvent { event, .. } => Some(event.detail),
                _ => None,
            })
            .collect())
    }

    async fn finish(self, scenario: Result<()>) -> Result<()> {
        let _ = self.script.cancel_session(&self.session_id).await;
        self.service.shutdown(Duration::from_secs(5)).await?;
        self.reader.abort();
        scenario
    }
}

/// Assert that a response frame carries no error.
fn succeeded(response: &Value) {
    assert!(
        response.get("error").is_none_or(Value::is_null),
        "{response}"
    );
}

/// A cancelled interaction keeps the native checkpoint its interrupted turn reported, so rolling
/// back the interaction after it rewinds the provider to that checkpoint instead of refusing the
/// rollback for a missing checkpoint.
#[tokio::test]
async fn conversation_rollback_rewinds_to_a_cancelled_interaction() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.exchange(10, "Keep the first exchange", 1).await?;
        scene
            .held_exchange(11, "Cancel the second exchange")
            .await?;
        succeeded(
            &scene
                .request(
                    12,
                    "session.turn.cancel",
                    json!({"operation_id":"cancel-second","restore_prompt_if_no_output":false}),
                )
                .await?,
        );
        settle_exchanges(&scene.controller, &scene.session_id, 2).await?;
        scene
            .exchange(13, "Roll back the third exchange", 3)
            .await?;
        let exchanges = scene.exchanges()?;
        assert_eq!(
            exchanges[1].state(),
            crate::exchange::ExchangeState::Cancelled
        );
        let native = scene
            .script
            .dispatch(&scene.session_id, MockResponseCommand::History)
            .await?;
        let checkpoints = native["checkpoints"]
            .as_array()
            .context("history checkpoints missing")?
            .clone();
        assert_eq!(checkpoints.len(), 3, "{native}");

        let response = scene.conversation_undo(20, &exchanges[2].id).await?;
        succeeded(&response);
        let native = scene
            .script
            .dispatch(&scene.session_id, MockResponseCommand::History)
            .await?;
        assert_eq!(native["checkpoints"], json!(checkpoints[..2]));
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// A files rollback chosen while a turn runs stops that turn before it previews the restore, as
/// confirming a conversation rollback does, instead of refusing the preview because a later
/// exchange is unfinished.
#[tokio::test]
async fn workspace_rollback_preview_collects_the_running_turn() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.exchange(10, "Keep the first exchange", 1).await?;
        scene.held_exchange(11, "Running when undo arrives").await?;
        let first = scene.exchanges()?[0].id.clone();
        let response = scene
            .request(
                20,
                "session.exchange.rollback.prepare",
                json!({"exchange_id":first}),
            )
            .await?;
        succeeded(&response);
        let preview_id = response["result"]["preview_id"]
            .as_str()
            .with_context(|| format!("preview missing: {response}"))?
            .to_owned();
        let exchanges = scene.exchanges()?;
        assert_eq!(exchanges.len(), 2);
        assert_eq!(
            exchanges[1].state(),
            crate::exchange::ExchangeState::Cancelled
        );
        succeeded(
            &scene
                .request(
                    21,
                    "session.exchange.rollback.apply",
                    json!({"preview_id":preview_id}),
                )
                .await?,
        );
        assert_eq!(
            scene.exchanges()?[0].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// Rolling back an interaction inside a goal keeps the goal, paused; rolling back past the goal's
/// start removes it. Neither rollback leaves a task row from the history it removed.
#[tokio::test]
async fn rollback_keeps_a_goal_it_does_not_rewind_past() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.install_paused_goal(1)?;
        scene
            .exchange(10, "First exchange inside the goal", 1)
            .await?;
        scene
            .exchange(11, "Second exchange inside the goal", 2)
            .await?;
        let exchanges = scene.exchanges()?;

        succeeded(&scene.conversation_undo(20, &exchanges[1].id).await?);
        let task = scene
            .controller
            .task_controller
            .current()?
            .context("rolling back an exchange inside the goal removed the goal")?;
        assert_eq!(task.status(), crate::task::TaskStatus::Paused);
        assert!(
            scene.session_rows()?.is_empty(),
            "{:?}",
            scene.session_rows()?
        );

        succeeded(&scene.conversation_undo(21, &exchanges[0].id).await?);
        assert!(scene.controller.task_controller.current()?.is_none());
        assert!(
            scene.session_rows()?.is_empty(),
            "{:?}",
            scene.session_rows()?
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// Task rows recorded during the rolled-back interactions leave the timeline with them.
#[tokio::test]
async fn rollback_removes_task_rows_of_the_rolled_back_interactions() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.exchange(10, "Before the goal", 1).await?;
        let started_at_ms = scene.exchanges()?[0]
            .completed_at_ms()
            .context("first exchange has no completion time")?
            + 1;
        scene.install_paused_goal(started_at_ms)?;
        scene.exchange(11, "Inside the goal", 2).await?;
        let mut store = SqliteStore::open(&scene.controller.data_root)?;
        let task = scene
            .controller
            .task_controller
            .current()?
            .context("goal missing")?;
        scene.controller.task_controller.change(
            &mut store,
            Some(&task.revision),
            crate::task::TaskChange::Clear,
            crate::broker::Clock::now_ms(&crate::broker::SystemClock),
        )?;
        assert!(
            scene
                .session_rows()?
                .iter()
                .any(|row| matches!(row, crate::timeline::SessionEventKind::TaskFinished { .. })),
            "the cleared goal recorded no row"
        );
        let second = scene.exchanges()?[1].id.clone();
        succeeded(&scene.conversation_undo(20, &second).await?);
        assert!(
            scene.session_rows()?.is_empty(),
            "{:?}",
            scene.session_rows()?
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}
