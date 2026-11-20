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

/// `/undo` issued while a turn runs sends the editor's requests in the editor's order: restore
/// recovery status, the interaction list, then the chosen rollback. Each request is served with the
/// turn still in flight; only the rollback itself stops the turn. The same holds for the "undo last
/// file restore" path, whose reverse preview comes from a recovery action.
#[tokio::test]
async fn undo_requests_are_served_while_a_turn_runs() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.exchange(10, "Keep the first exchange", 1).await?;
        scene.held_exchange(11, "Running when undo arrives").await?;

        let recovery = scene
            .request(20, "session.exchange.recovery", json!({}))
            .await?;
        succeeded(&recovery);
        assert_eq!(recovery["result"], Value::Null, "{recovery}");
        let listed = scene
            .request(21, "session.exchange.list", json!({}))
            .await?;
        succeeded(&listed);
        let listed = listed["result"]
            .as_array()
            .with_context(|| format!("exchange list missing: {listed}"))?
            .clone();
        assert_eq!(listed.len(), 2);
        let first = listed[0]["id"].as_str().context("first id")?.to_owned();
        let prepared = scene
            .request(
                22,
                "session.exchange.rollback.prepare",
                json!({"exchange_id":first}),
            )
            .await?;
        succeeded(&prepared);
        let preview_id = prepared["result"]["preview_id"]
            .as_str()
            .with_context(|| format!("preview missing: {prepared}"))?
            .to_owned();
        succeeded(
            &scene
                .request(
                    23,
                    "session.exchange.rollback.apply",
                    json!({"preview_id":preview_id}),
                )
                .await?,
        );

        scene
            .held_exchange(30, "Running when the restore is undone")
            .await?;
        let recovery = scene
            .request(31, "session.exchange.recovery", json!({}))
            .await?;
        succeeded(&recovery);
        assert_eq!(recovery["result"]["pending"], json!(false), "{recovery}");
        succeeded(
            &scene
                .request(32, "session.exchange.list", json!({}))
                .await?,
        );
        let reverse = scene
            .request(33, "session.exchange.recovery", json!({"action":"undo"}))
            .await?;
        succeeded(&reverse);
        let reverse_id = reverse["result"]["preview_id"]
            .as_str()
            .with_context(|| format!("reverse preview missing: {reverse}"))?
            .to_owned();
        succeeded(
            &scene
                .request(
                    34,
                    "session.exchange.rollback.apply",
                    json!({"preview_id":reverse_id}),
                )
                .await?,
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// The conversation-only choice of `/undo` reaches the same requests in the same order during a
/// running turn, and confirming it stops that turn and rewinds the provider conversation.
#[tokio::test]
async fn conversation_undo_requests_are_served_while_a_turn_runs() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene
            .exchange(10, "Keep nothing of this exchange", 1)
            .await?;
        scene.held_exchange(11, "Running when undo arrives").await?;
        let recovery = scene
            .request(20, "session.exchange.recovery", json!({}))
            .await?;
        succeeded(&recovery);
        let listed = scene
            .request(21, "session.exchange.list", json!({}))
            .await?;
        succeeded(&listed);
        let first = listed["result"][0]["id"]
            .as_str()
            .with_context(|| format!("exchange list missing: {listed}"))?
            .to_owned();
        succeeded(&scene.conversation_undo(22, &first).await?);
        let exchanges = scene.exchanges()?;
        assert_eq!(
            exchanges[1].state(),
            crate::exchange::ExchangeState::Cancelled
        );
        assert_eq!(
            exchanges[0].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// Arm a hold on the next native rewind and return its identity.
async fn hold_rewind(scene: &RollbackScene) -> Result<String> {
    let hold = scene
        .script
        .dispatch(&scene.session_id, MockResponseCommand::HoldRewind)
        .await?;
    Ok(hold["id"]
        .as_str()
        .context("rewind hold id missing")?
        .to_owned())
}

/// Prepare a files rollback of the newest of two settled exchanges and return its preview.
async fn prepared_files_rollback(scene: &RollbackScene) -> Result<String> {
    scene.exchange(10, "Keep the first exchange", 1).await?;
    scene
        .exchange(11, "Roll back the second exchange", 2)
        .await?;
    let second = scene.exchanges()?[1].id.clone();
    let prepared = scene
        .request(
            20,
            "session.exchange.rollback.prepare",
            json!({"exchange_id":second}),
        )
        .await?;
    succeeded(&prepared);
    Ok(prepared["result"]["preview_id"]
        .as_str()
        .with_context(|| format!("preview missing: {prepared}"))?
        .to_owned())
}

/// A files rollback restores files, then waits on the provider's conversation rewind before it
/// marks its journal complete. Throughout that wait the restore is running, not interrupted:
/// neither the session snapshot nor the recovery status may offer recovery for it.
#[tokio::test]
async fn a_running_restore_is_never_reported_interrupted() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        let preview_id = prepared_files_rollback(&scene).await?;
        let hold = hold_rewind(&scene).await?;
        let (applied, observed) = tokio::join!(
            scene.request(
                21,
                "session.exchange.rollback.apply",
                json!({"preview_id":preview_id}),
            ),
            async {
                scene
                    .script
                    .dispatch(
                        &scene.session_id,
                        MockResponseCommand::Wait { id: hold.clone() },
                    )
                    .await?;
                let reader = crate::storage::HistoryReader::current(&scene.controller.data_root)?;
                let journal = crate::checkpoint::RestoreJournal::progress(
                    &reader.objects,
                    &scene.session_id,
                )?
                .context("the restore wrote no journal before rewinding")?;
                assert_ne!(journal.state, crate::checkpoint::RestoreState::Complete);
                assert_eq!(
                    scene.controller.observer.snapshot()?.restore_recovery,
                    None,
                    "the snapshot reported a running restore as interrupted"
                );
                let status = scene
                    .request(22, "session.exchange.recovery", json!({}))
                    .await?;
                succeeded(&status);
                assert_eq!(status["result"]["pending"], json!(false), "{status}");
                assert_eq!(status["result"]["applying"], json!(true), "{status}");
                scene
                    .script
                    .dispatch(&scene.session_id, MockResponseCommand::Release { id: hold })
                    .await?;
                Ok::<(), anyhow::Error>(())
            }
        );
        observed?;
        succeeded(&applied?);
        assert_eq!(scene.controller.observer.snapshot()?.restore_recovery, None);
        let status = scene
            .request(23, "session.exchange.recovery", json!({}))
            .await?;
        assert_eq!(
            (&status["result"]["pending"], &status["result"]["applying"]),
            (&json!(false), &json!(false)),
            "{status}"
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// A restore whose conversation rewind fails after the files changed really stopped part-way, so
/// the snapshot and the recovery status both offer recovery once the rollback has failed.
#[tokio::test]
async fn a_restore_stopped_part_way_reports_recovery() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        let preview_id = prepared_files_rollback(&scene).await?;
        let hold = hold_rewind(&scene).await?;
        let (applied, cancelled) = tokio::join!(
            scene.request(
                21,
                "session.exchange.rollback.apply",
                json!({"preview_id":preview_id}),
            ),
            async {
                scene
                    .script
                    .dispatch(
                        &scene.session_id,
                        MockResponseCommand::Wait { id: hold.clone() },
                    )
                    .await?;
                scene
                    .script
                    .dispatch(&scene.session_id, MockResponseCommand::Cancel { id: hold })
                    .await
            }
        );
        cancelled?;
        let applied = applied?;
        assert!(
            applied.get("error").is_some_and(|error| !error.is_null()),
            "{applied}"
        );
        assert!(
            scene
                .controller
                .observer
                .snapshot()?
                .restore_recovery
                .is_some()
        );
        let status = scene
            .request(22, "session.exchange.recovery", json!({}))
            .await?;
        assert_eq!(status["result"]["pending"], json!(true), "{status}");
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// Fail the next native rewind once the files restore reaches it, while `request` runs.
async fn fail_rewind_during(
    scene: &RollbackScene,
    request: impl std::future::Future<Output = Result<Value>>,
) -> Result<Value> {
    let hold = hold_rewind(scene).await?;
    let (response, cancelled) = tokio::join!(request, async {
        scene
            .script
            .dispatch(
                &scene.session_id,
                MockResponseCommand::Wait { id: hold.clone() },
            )
            .await?;
        scene
            .script
            .dispatch(&scene.session_id, MockResponseCommand::Cancel { id: hold })
            .await
    });
    cancelled?;
    response
}

/// A files rollback whose conversation rewind fails after the files changed says what state the
/// workspace is in and how to leave it, and continuing the interrupted restore says the same when
/// the rewind fails again, rather than only that restoration did not complete.
#[tokio::test]
async fn a_rewind_failure_after_restoring_files_says_what_changed() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        let preview_id = prepared_files_rollback(&scene).await?;
        let applied = fail_rewind_during(
            &scene,
            scene.request(
                21,
                "session.exchange.rollback.apply",
                json!({"preview_id":preview_id}),
            ),
        )
        .await?;
        let continued = fail_rewind_during(
            &scene,
            scene.request(
                22,
                "session.exchange.recovery",
                json!({"action":"continue"}),
            ),
        )
        .await?;
        for response in [&applied, &continued] {
            let message = response["error"].to_string();
            assert!(
                message
                    .contains("the files were restored, but the conversation was not rolled back")
                    && message.contains("Continue interrupted restore")
                    && message.contains("Restore pre-restore files"),
                "{response}"
            );
        }
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// The first exchange names an unnamed session after its prompt. Rolling back every exchange
/// returns the session to unnamed, so no title describes a prompt the transcript no longer shows;
/// a name the user chose survives the same rollback.
#[tokio::test]
async fn rolling_back_every_exchange_releases_the_automatic_name() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        let session_name =
            || -> Result<String> { Ok(scene.controller.observer.snapshot()?.session.name) };
        scene
            .exchange(10, "Name the session from this prompt", 1)
            .await?;
        assert_eq!(session_name()?, "Name the session from this pro");
        let first = scene.exchanges()?[0].id.clone();
        succeeded(&scene.conversation_undo(20, &first).await?);
        assert_eq!(session_name()?, "");

        scene.exchange(30, "Name the session again", 2).await?;
        succeeded(
            &scene
                .request(
                    31,
                    "session.rename",
                    json!({"session_id":scene.session_id,"name":"Chosen name"}),
                )
                .await?,
        );
        let second = scene.exchanges()?[1].id.clone();
        succeeded(&scene.conversation_undo(32, &second).await?);
        assert_eq!(session_name()?, "Chosen name");
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// A session whose automatic name a rollback released is named again by its next first prompt,
/// although the rolled-back exchanges stay in history.
#[tokio::test]
async fn the_first_prompt_after_a_released_name_names_the_session_again() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        let session_name =
            || -> Result<String> { Ok(scene.controller.observer.snapshot()?.session.name) };
        scene.exchange(10, "Name the session first", 1).await?;
        let first = scene.exchanges()?[0].id.clone();
        succeeded(&scene.conversation_undo(20, &first).await?);
        assert_eq!(session_name()?, "");

        scene.exchange(30, "Name the session again", 2).await?;
        assert_eq!(session_name()?, "Name the session again");
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// The exchange of the running turn is itself a rollback target: confirming it stops the turn and
/// rolls that exchange back.
#[tokio::test]
async fn the_running_exchange_can_be_rolled_back() -> Result<()> {
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.exchange(10, "Keep the first exchange", 1).await?;
        scene
            .held_exchange(11, "Roll back the running exchange")
            .await?;
        let listed = scene
            .request(20, "session.exchange.list", json!({}))
            .await?;
        let running = listed["result"][1].clone();
        assert_eq!(running["state"], "running", "{listed}");
        let running_id = running["id"].as_str().context("running id")?.to_owned();
        succeeded(&scene.conversation_undo(21, &running_id).await?);
        let exchanges = scene.exchanges()?;
        assert_eq!(
            exchanges[0].disposition(),
            crate::exchange::HistoryDisposition::Current
        );
        assert_eq!(
            exchanges[1].disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        Ok(())
    }
    .await;
    scene.finish(scenario).await
}

/// Pausing a goal mid-turn parks that turn's exchange: it keeps its output and stays `running` in
/// history with no execution. The interaction list includes it marked paused, and a conversation
/// rollback to it closes it, rewinds the provider before it, and keeps the goal paused.
#[tokio::test]
async fn a_paused_goal_turn_is_listed_and_rolled_back() -> Result<()> {
    use forge_provider_api::testing::{
        TestHoldId, TestInputAcceptance, TestMessageContent, TestPromptScript, TestPromptStep,
        TestPromptTerminal,
    };
    let scene = RollbackScene::start().await?;
    let scenario = async {
        scene.install_paused_goal(1)?;
        scene
            .exchange(10, "An earlier exchange inside the goal", 1)
            .await?;
        scene
            .script
            .dispatch(
                &scene.session_id,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::BeforeResponse,
                        steps: vec![
                            TestPromptStep::Message {
                                content: TestMessageContent::Response,
                                text: "Wrote the goal's next file".into(),
                            },
                            TestPromptStep::Barrier {
                                id: TestHoldId::new("goal-turn-running")?,
                            },
                        ],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        succeeded(
            &scene
                .request(
                    20,
                    "session.command.apply",
                    json!({"operation_id":"resume-20","action":"resume"}),
                )
                .await?,
        );
        scene
            .script
            .dispatch(
                &scene.session_id,
                MockResponseCommand::Wait {
                    id: "goal-turn-running".into(),
                },
            )
            .await?;
        succeeded(
            &scene
                .request(
                    21,
                    "session.command.apply",
                    json!({"operation_id":"pause-21","action":"pause"}),
                )
                .await?,
        );
        let listed = scene
            .request(22, "session.exchange.list", json!({}))
            .await?;
        succeeded(&listed);
        let newest = listed["result"]
            .as_array()
            .and_then(|listed| listed.last())
            .with_context(|| format!("exchange list missing: {listed}"))?
            .clone();
        assert_eq!(newest["state"], "running", "{newest}");
        assert_eq!(newest["paused"], true, "{newest}");
        let newest_id = newest["id"].as_str().context("newest id")?.to_owned();
        succeeded(&scene.conversation_undo(23, &newest_id).await?);
        let rolled_back = scene
            .exchanges()?
            .into_iter()
            .find(|exchange| exchange.id == newest_id)
            .context("the paused turn disappeared")?;
        assert_eq!(
            rolled_back.disposition(),
            crate::exchange::HistoryDisposition::RolledBack
        );
        let task = scene
            .controller
            .task_controller
            .current()?
            .context("rolling back the paused turn removed the goal")?;
        assert_eq!(task.status(), crate::task::TaskStatus::Paused);
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
