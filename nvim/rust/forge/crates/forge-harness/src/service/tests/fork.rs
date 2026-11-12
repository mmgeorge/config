//! Fork a session while it is idle, executing, or itself a fresh fork, and open an aside of it.

use super::super::SessionController;
use super::bootstrap::initialize;
use super::owner_fixture::owner_transition_fixture;
use super::owner_transition::{finish_first_exchange, settle_exchanges, submit_input};
use crate::session::SessionStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

/// Persist a fork child directly, as a host does before its native fork task starts.
///
/// Resolving the child records the fork as interrupted, which leaves a `Failed` child holding the
/// fork point its source's state produced.
async fn interrupted_fork(
    registry: &Arc<super::super::SessionControllerRegistry>,
    source_session_id: &str,
    source_is_executing: bool,
) -> Result<(Arc<SessionController>, String)> {
    let capability = registry.runtime.provider_handle().descriptor().capability;
    let preparation = crate::broker::prepare_provider_fork(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        registry.initialize.provider.kind.as_str(),
        &capability,
        source_session_id,
        &json!({}),
        crate::broker::ForkOptions {
            purpose: crate::session::SessionPurpose::Conversation,
            source_is_executing: &|| source_is_executing,
        },
    )?;
    let child = registry.resolve(&preparation.child.id).await?;
    assert!(matches!(
        child.broker.lock().await.provider_fork_state(),
        crate::session::ProviderForkState::Failed { .. }
    ));
    Ok((child, preparation.child.id))
}

/// A source running its first turn holds no native history, so its fork opens a fresh provider
/// conversation instead of waiting on a native fork that cannot succeed.
#[tokio::test]
async fn fork_of_a_first_turn_opens_fresh_provider_history() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let registry = fixture.service.registry().await?;
    let (child, gate) = super::super::start_session_fork(
        registry.clone(),
        fixture.session.clone(),
        json!({"session_id": fixture.session}),
        None,
        crate::session::SessionPurpose::Conversation,
        &fixture.sink,
    )
    .await?;

    gate.wait().await?;
    let snapshot = child.broker.lock().await.snapshot()?;
    assert_eq!(
        snapshot.session.provider_fork_state,
        crate::session::ProviderForkState::Ready
    );
    assert_eq!(snapshot.session.provider_session_id, None);
    let timeline = serde_json::to_string(&snapshot.timeline)?;
    assert!(
        !timeline.contains("Keep this transcript through configuration"),
        "the unfinished exchange stays with its source: {timeline}"
    );
    Ok(())
}

/// A failed fork refuses input rather than opening fresh provider history that lacks the forked
/// conversation, and stays failed so a later input can retry it.
#[tokio::test]
async fn failed_fork_refuses_input_while_its_source_cannot_be_copied() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let registry = fixture.service.registry().await?;
        // A fork holds the turns it inherited and records no checkpoint of its own until its
        // first turn finishes. Forked again while idle, it leaves a child that expects its live
        // conversation, and the first turn it then starts gives that child no checkpoint to read
        // instead.
        let (source, source_id) = fixture.fork().await?;
        fixture
            .hold_turn(&source, &source_id, 30, "Start the forked work")
            .await?;
        let (child, child_id) = interrupted_fork(&registry, &source_id, false).await?;

        let submitted = fixture
            .service
            .dispatch(
                Some(child_id.clone()),
                Request {
                    id: 31,
                    method: "session.input.apply".into(),
                    params: json!({"id":"forked-input","action":{"kind":"submit","message":{"id":"forked-input","text":"What word did I ask you to remember?","intent":"auto","attachments":[]}}}),
                },
                &fixture.sink,
            )
            .await;

        let error = submitted
            .err()
            .context("input must not run before the fork is ready")?;
        assert!(
            format!("{error:#}").contains("the source session is running"),
            "{error:#}"
        );
        let snapshot = child.broker.lock().await.snapshot()?;
        assert!(matches!(
            snapshot.session.provider_fork_state,
            crate::session::ProviderForkState::Failed { .. }
        ));
        assert_eq!(snapshot.session.provider_session_id, None);
        assert!(
            crate::storage::SqliteStore::open(&child.data_root)?
                .list_exchange(&child_id)?
                .is_empty()
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Open one mock session whose first exchange finished and whose second is held mid-turn.
struct RunningSourceFixture {
    service: super::super::HarnessService,
    controller: Arc<SessionController>,
    session_id: String,
    script: crate::provider::mock_response::MockResponseControl,
    /// Holds the source's second turn open until a test releases it.
    hold_id: String,
    sink: forge_protocol::outbound::MessageSender,
    /// Every frame the host sent to the editor, oldest first.
    outbound_frames: Arc<std::sync::Mutex<Vec<serde_json::Value>>>,
    reader: tokio::task::JoinHandle<Result<()>>,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _root: tempfile::TempDir,
}

impl RunningSourceFixture {
    async fn start() -> Result<Self> {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let service = super::bootstrap::service();
        service.open_session(1, initialize(&root, "mock")).await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        let script = controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let outbound_frames = Arc::new(std::sync::Mutex::new(Vec::new()));
        let received_frames = Arc::clone(&outbound_frames);
        let reader = tokio::spawn(async move {
            while let Some(frame) = output.recv().await? {
                received_frames
                    .lock()
                    .unwrap()
                    .push(serde_json::from_slice(frame.bytes())?);
            }
            Ok::<(), anyhow::Error>(())
        });
        finish_first_exchange(&service, &controller, &session_id, &sink).await?;
        let mut fixture = Self {
            service,
            controller,
            session_id,
            script,
            hold_id: String::new(),
            sink,
            outbound_frames,
            reader,
            _root: root,
        };
        fixture.hold_id = fixture
            .hold_turn(
                &fixture.controller,
                &fixture.session_id,
                11,
                "Keep working on the task",
            )
            .await?;
        Ok(fixture)
    }

    /// Submit one input and return the hold's identity once the provider holds its turn open.
    async fn hold_turn(
        &self,
        controller: &SessionController,
        session_id: &str,
        request_id: u64,
        text: &str,
    ) -> Result<String> {
        use crate::provider::mock_response::MockResponseCommand;
        let hold = self
            .script
            .dispatch(
                session_id,
                MockResponseCommand::Hold {
                    mode: None,
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"].as_str().context("hold id missing")?.to_owned();
        submit_input(
            &self.service,
            controller,
            session_id,
            request_id,
            &format!("held-{request_id}"),
            text,
            &self.sink,
        )
        .await?;
        self.script
            .dispatch(
                session_id,
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await?;
        Ok(hold_id)
    }

    /// Let a held turn answer and finish.
    async fn release(&self, session_id: &str, hold_id: &str) -> Result<()> {
        use crate::provider::mock_response::MockResponseCommand;
        self.script
            .dispatch(
                session_id,
                MockResponseCommand::Release {
                    id: hold_id.to_owned(),
                },
            )
            .await?;
        Ok(())
    }

    async fn native_checkpoints(&self, session_id: &str) -> Result<Vec<serde_json::Value>> {
        use crate::provider::mock_response::MockResponseCommand;
        self.script
            .dispatch(session_id, MockResponseCommand::History)
            .await?["checkpoints"]
            .as_array()
            .cloned()
            .context("history checkpoints missing")
    }

    async fn fork(&self) -> Result<(Arc<SessionController>, String)> {
        self.fork_with(json!({"session_id": self.session_id})).await
    }

    /// Fork the running source with explicit `session.fork` parameters.
    async fn fork_with(
        &self,
        params: serde_json::Value,
    ) -> Result<(Arc<SessionController>, String)> {
        let (child, gate) = super::super::start_session_fork(
            self.service.registry().await?,
            self.session_id.clone(),
            params,
            None,
            crate::session::SessionPurpose::Conversation,
            &self.sink,
        )
        .await?;
        gate.wait().await?;
        let child_id = child.broker.lock().await.snapshot()?.session.id;
        Ok((child, child_id))
    }

    /// Wait for the host's response to one request.
    async fn response(&self, request_id: u64) -> serde_json::Value {
        loop {
            let response = self
                .outbound_frames
                .lock()
                .unwrap()
                .iter()
                .find(|frame| frame["id"] == request_id)
                .cloned();
            if let Some(response) = response {
                return response;
            }
            tokio::task::yield_now().await;
        }
    }

    /// Reopen a failed child's fork against the source's live conversation, as a fork that found
    /// its source idle would have attempted it, and wait for the attempt to settle.
    async fn fork_from_live_conversation(
        &self,
        child: &Arc<SessionController>,
        child_id: &str,
    ) -> Result<()> {
        let registry = self.service.registry().await?;
        let mut live_request = child
            .broker
            .lock()
            .await
            .retry_provider_fork()?
            .context("a failed fork has a next attempt")?;
        live_request.checkpoint_id = None;
        let gate = super::super::ProviderForkGate::new();
        registry
            .provider_fork_by_id
            .write()
            .await
            .insert(child_id.to_owned(), Arc::clone(&gate));
        super::super::spawn_provider_fork(
            &registry,
            &mut *registry.fork_task.lock().await,
            Arc::clone(child),
            Arc::clone(&gate),
            live_request,
            None,
            self.sink.clone(),
        );
        gate.wait().await?;
        Ok(())
    }

    /// Shut the host down and report the scenario's failure ahead of any shutdown failure, because
    /// a scenario that stopped midway is usually what left the host unable to quiesce.
    async fn finish(self, scenario: Result<()>) -> Result<()> {
        let shutdown = self.service.shutdown(Duration::from_secs(5)).await;
        self.reader.abort();
        scenario?;
        shutdown
    }
}

/// A fork taken while the source executes stops at the source's last finished exchange: the child
/// copies native history up to that checkpoint, omits the unfinished exchange, and runs input.
#[tokio::test]
async fn fork_of_a_running_session_stops_at_its_last_finished_exchange() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let (child, child_id) = fixture.fork().await?;

        let source_checkpoints = fixture.native_checkpoints(&fixture.session_id).await?;
        assert_eq!(source_checkpoints.len(), 1);
        assert_eq!(
            fixture.native_checkpoints(&child_id).await?,
            source_checkpoints,
            "the child copies native history through the settled checkpoint"
        );
        let timeline = serde_json::to_string(&child.broker.lock().await.snapshot()?.timeline)?;
        assert!(timeline.contains("Remember the word harbor"), "{timeline}");
        assert!(
            !timeline.contains("Keep working on the task"),
            "the unfinished exchange stays with its source: {timeline}"
        );
        assert!(
            timeline.contains(r#""unfinished_exchange_omitted":true"#),
            "{timeline}"
        );

        submit_input(
            &fixture.service,
            &child,
            &child_id,
            30,
            "forked-input",
            "What word did I ask you to remember?",
            &fixture.sink,
        )
        .await?;
        settle_exchanges(&child, &child_id, 1).await?;
        assert_eq!(
            fixture.native_checkpoints(&child_id).await?.len(),
            2,
            "the child's turn extends the copied history"
        );
        assert_eq!(
            fixture.controller.coordinator.execution.available_permits(),
            0,
            "the fork leaves the source turn running"
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A forked session records no checkpoint of its own until its first turn finishes, so forking it
/// during that turn has no boundary to copy and must leave no child behind.
#[tokio::test]
async fn fork_of_a_fork_during_its_first_turn_creates_no_session() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let registry = fixture.service.registry().await?;
        let (child, child_id) = fixture.fork().await?;
        fixture
            .hold_turn(&child, &child_id, 30, "Start the forked work")
            .await?;
        let store = crate::storage::SqliteStore::open(&child.data_root)?;
        let session_count = store.list_session(None)?.len();

        let rejected = super::super::start_session_fork(
            registry,
            child_id.clone(),
            json!({"session_id": child_id}),
            None,
            crate::session::SessionPurpose::Conversation,
            &fixture.sink,
        )
        .await;

        let error = rejected.err().context("the fork must be rejected")?;
        assert!(
            format!("{error:#}").contains("during its first turn"),
            "{error:#}"
        );
        assert_eq!(store.list_session(None)?.len(), session_count);
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A child whose fork failed starts a new native attempt on its next input. The attempt reads the
/// recorded checkpoint, so it succeeds while the source is still executing.
#[tokio::test]
async fn failed_fork_retries_on_the_next_input() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let registry = fixture.service.registry().await?;
        let (child, child_id) = interrupted_fork(&registry, &fixture.session_id, true).await?;

        submit_input(
            &fixture.service,
            &child,
            &child_id,
            30,
            "forked-input",
            "What word did I ask you to remember?",
            &fixture.sink,
        )
        .await?;
        settle_exchanges(&child, &child_id, 1).await?;

        assert_eq!(
            child.broker.lock().await.provider_fork_state(),
            crate::session::ProviderForkState::Ready
        );
        let source_checkpoints = fixture.native_checkpoints(&fixture.session_id).await?;
        let child_checkpoints = fixture.native_checkpoints(&child_id).await?;
        assert_eq!(child_checkpoints.len(), 2);
        assert_eq!(child_checkpoints[0], source_checkpoints[0]);
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A source that starts a turn after the fork selected its live conversation rejects that copy.
/// The fork task then reads the recorded checkpoint instead of failing the child.
#[tokio::test]
async fn fork_reads_the_checkpoint_when_the_live_conversation_is_executing() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let registry = fixture.service.registry().await?;
        let (child, child_id) = interrupted_fork(&registry, &fixture.session_id, true).await?;

        fixture
            .fork_from_live_conversation(&child, &child_id)
            .await?;

        assert_eq!(
            fixture.native_checkpoints(&child_id).await?,
            fixture.native_checkpoints(&fixture.session_id).await?
        );
        assert!(
            !registry
                .provider_fork_by_id
                .read()
                .await
                .contains_key(&child_id)
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// Cancelling a source after a fork was taken from its running turn stops that turn cleanly.
///
/// The fork first asks for the live conversation, which the provider refuses because the source
/// is executing, and then reads the settled checkpoint. Neither attempt may leave the source's
/// provider conversation in a state the stop cannot collect: the cancel succeeds, and the
/// source's next input runs on the same conversation.
#[tokio::test]
async fn cancelling_a_forked_source_stops_its_turn_cleanly() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let registry = fixture.service.registry().await?;
        let source_id = fixture.session_id.clone();
        let (child, child_id) = interrupted_fork(&registry, &source_id, true).await?;
        fixture
            .fork_from_live_conversation(&child, &child_id)
            .await?;
        let provider_session_id = fixture
            .controller
            .broker
            .lock()
            .await
            .snapshot()?
            .session
            .provider_session_id;

        fixture
            .service
            .dispatch(
                Some(source_id.clone()),
                Request {
                    id: 40,
                    method: "session.turn.cancel".into(),
                    params: json!({}),
                },
                &fixture.sink,
            )
            .await?;
        let cancelled = fixture.response(40).await;
        anyhow::ensure!(
            cancelled.get("error").is_none(),
            "cancel failed: {cancelled}"
        );
        settle_exchanges(&fixture.controller, &source_id, 2).await?;
        let store = crate::storage::SqliteStore::open(&fixture.controller.data_root)?;
        assert_eq!(
            store.list_exchange(&source_id)?[1].state(),
            crate::exchange::ExchangeState::Cancelled
        );

        submit_input(
            &fixture.service,
            &fixture.controller,
            &source_id,
            41,
            "after-cancel",
            "Continue after the cancel",
            &fixture.sink,
        )
        .await?;
        settle_exchanges(&fixture.controller, &source_id, 3).await?;
        assert_eq!(
            store.list_exchange(&source_id)?[2].state(),
            crate::exchange::ExchangeState::Complete
        );
        assert_eq!(
            fixture
                .controller
                .broker
                .lock()
                .await
                .snapshot()?
                .session
                .provider_session_id,
            provider_session_id,
            "the stop kept the source's provider conversation"
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// An aside reads its origin's running exchange and queues a labeled message that the origin runs
/// after its current turn.
///
/// The mock provider rejects a tool its turn was not offered, so both calls succeeding also proves
/// that an aside's turn offers the session-history tools and `origin_message`.
#[tokio::test]
async fn aside_inspects_and_messages_its_running_origin() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let origin_id = fixture.session_id.clone();
        let (aside, aside_id) = fixture
            .fork_with(json!({"session_id": origin_id, "aside": true}))
            .await?;
        assert_eq!(
            aside
                .broker
                .lock()
                .await
                .snapshot()?
                .session
                .aside
                .map(|origin| origin.session_id),
            Some(origin_id.clone())
        );

        let store = crate::storage::SqliteStore::open(&aside.data_root)?;
        let running_exchange_id = store.list_exchange(&origin_id)?[1].id.clone();
        fixture
            .script
            .dispatch(
                &aside_id,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::BeforeResponse,
                        steps: vec![
                            TestPromptStep::ToolCall {
                                name: "harness_session_exchange".into(),
                                arguments: json!({
                                    "session_id": origin_id,
                                    "exchange_id": running_exchange_id,
                                }),
                            },
                            TestPromptStep::ToolCall {
                                name: "origin_message".into(),
                                arguments: json!({"message": "Use the settled cache key."}),
                            },
                        ],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        submit_input(
            &fixture.service,
            &aside,
            &aside_id,
            31,
            "report-back",
            "Tell the origin what you found",
            &fixture.sink,
        )
        .await?;
        settle_exchanges(&aside, &aside_id, 1).await?;
        let aside_exchange = store.list_exchange(&aside_id)?.remove(0);
        let tools = aside_exchange
            .turn
            .iter()
            .flat_map(|turn| aside_exchange.tools_for_turn(turn.id()))
            .map(|tool| (tool.title.clone(), tool.failed))
            .collect::<Vec<_>>();
        assert_eq!(
            tools,
            [
                ("harness_session_exchange".to_owned(), false),
                ("origin_message".to_owned(), false)
            ]
        );

        // The origin is still inside its held turn, so the message waits behind that turn.
        let inputs = fixture
            .controller
            .coordinator
            .submissions
            .snapshot()?
            .messages;
        let [running, queued] = inputs.as_slice() else {
            anyhow::bail!("the origin holds its running input and the aside's message: {inputs:?}");
        };
        assert_eq!(running.text, "Keep working on the task");
        assert!(
            queued.text.starts_with("Message from aside session")
                && queued.text.contains(&aside_id)
                && queued.text.ends_with("Use the settled cache key."),
            "{}",
            queued.text
        );
        assert_eq!(queued.intent, crate::input::InputIntent::Queue);
        // The origin's model receives the aside's words as a `<session_message>` naming the aside,
        // never as its own user's text; the queue shows only the label.
        let Some(forge_provider_api::forge::TurnKind::SessionMessage { message }) = queued
            .turn()
            .map(forge_provider_api::forge::TurnInput::kind)
        else {
            anyhow::bail!("the aside's message is not a typed session message: {queued:?}");
        };
        assert_eq!(message.from_session_id, aside_id);
        assert_eq!(message.text, "Use the settled cache key.");
        assert_eq!(
            queued
                .turn()
                .and_then(forge_provider_api::forge::TurnInput::user_input),
            None
        );

        fixture.release(&origin_id, &fixture.hold_id).await?;
        settle_exchanges(&fixture.controller, &origin_id, 3).await?;
        assert!(
            store.list_exchange(&origin_id)?[2]
                .prompt
                .ends_with("Use the settled cache key.")
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A fork of an aside keeps the aside's origin, and an ordinary fork names none.
///
/// The aside has run no turn, so forking it opens its provider binding only as a fork source and
/// no owner ever reads that binding's events. The host must still close the binding at shutdown,
/// which `finish` verifies.
#[tokio::test]
async fn fork_of_an_unused_aside_keeps_its_origin_and_closes_cleanly() -> Result<()> {
    let fixture = RunningSourceFixture::start().await?;
    let scenario = async {
        let origin_id = fixture.session_id.clone();
        let (plain, _) = fixture.fork().await?;
        assert_eq!(plain.broker.lock().await.snapshot()?.session.aside, None);

        let (_, aside_id) = fixture
            .fork_with(json!({"session_id": origin_id, "aside": true}))
            .await?;
        let (nested, gate) = super::super::start_session_fork(
            fixture.service.registry().await?,
            aside_id.clone(),
            json!({"session_id": aside_id}),
            None,
            crate::session::SessionPurpose::Conversation,
            &fixture.sink,
        )
        .await?;
        gate.wait().await?;
        assert_eq!(
            nested
                .broker
                .lock()
                .await
                .snapshot()?
                .session
                .aside
                .map(|origin| origin.session_id),
            Some(origin_id)
        );
        Ok(())
    }
    .await;
    fixture.finish(scenario).await
}

/// A provider writes a conversation when its first turn runs, so a session that ran no turn has
/// none to copy: the Claude CLI and the Codex app-server both reject a native fork of one. The
/// fork of such a session, and the fork of that fork, therefore open fresh provider history
/// without asking the provider for a copy, and run input.
#[tokio::test]
async fn fork_of_a_session_that_ran_no_turn_opens_fresh_provider_history() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = super::bootstrap::service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move { while let Ok(Some(_)) = output.recv().await {} });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        let mut source_id = session_id.clone();
        let mut forks = Vec::new();
        for _ in 0..2 {
            let (fork, gate) = super::super::start_session_fork(
                registry.clone(),
                source_id.clone(),
                json!({"session_id": source_id}),
                None,
                crate::session::SessionPurpose::Conversation,
                &sink,
            )
            .await?;
            gate.wait().await?;
            let session = fork.broker.lock().await.snapshot()?.session;
            assert_eq!(
                session.provider_fork_state,
                crate::session::ProviderForkState::Ready
            );
            assert_eq!(
                session.provider_session_id, None,
                "the fork asked the provider to copy a conversation that does not exist"
            );
            source_id = session.id;
            forks.push(fork);
        }
        let nested = forks.last().context("the nested fork exists")?;
        submit_input(
            &service,
            nested,
            &source_id,
            30,
            "forked-input",
            "Start the forked work",
            &sink,
        )
        .await?;
        settle_exchanges(nested, &source_id, 1).await?;
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let shutdown = service.shutdown(Duration::from_secs(5)).await;
    reader.abort();
    scenario?;
    shutdown
}
