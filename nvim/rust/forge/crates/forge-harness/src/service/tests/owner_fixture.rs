use super::super::{HarnessService, SessionController};
use super::bootstrap::{initialize, service};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use forge_protocol::outbound::MessageSender;
use serde_json::json;
use std::sync::Arc;

/// Keep real owner admission and held generic provider work under deterministic test control.
pub(super) struct OwnerTransitionFixture {
    pub(super) root: tempfile::TempDir,
    pub(super) service: HarnessService,
    pub(super) controller: Arc<SessionController>,
    pub(super) session: String,
    pub(super) provider: Arc<crate::provider::Provider>,
    pub(super) engine: Arc<crate::provider::session_execution::SessionExecution>,
    pub(super) sink: MessageSender,
    pub(super) reader: tokio::task::JoinHandle<Result<()>>,
    pub(super) original: String,
}

/// Start one held root invocation without using retired restart or cancellation flags.
pub(super) async fn owner_transition_fixture() -> Result<OwnerTransitionFixture> {
    use crate::provider::mock_response::MockResponseCommand;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session).await?;
    controller.trace.configure_default(true)?;
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        None,
        crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
            "Preserve the original task".into(),
            Default::default(),
            1,
        ))),
        1,
    )?;
    let provider = controller.provider_handle();
    let script = provider
        .mock_response_control()
        .context("fixture controls missing")?;
    let hold = script
        .dispatch(
            &session,
            MockResponseCommand::Hold {
                mode: None,
                any_session: false,
            },
        )
        .await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok(())
    });
    controller.attach_input(&sink).await?;
    service.dispatch(Some(session.clone()), Request { id: 2, method: "session.input.apply".into(),
        params: json!({"id":"original-owner-input","action":{"kind":"submit","message":{"id":"original-owner-input","text":"Keep this transcript through configuration","intent":"auto","attachments":[]}}}) }, &sink).await?;
    tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        script.dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().unwrap().into(),
            },
        ),
    )
    .await??;
    let original = SqliteStore::read_only(&controller.data_root)?
        .latest_exchange(&session)?
        .unwrap()
        .id;
    let execution_registry = controller.broker.lock().await.execution_registry();
    let engine = execution_registry
        .owner(crate::provider::ThreadOwner::root(&session))
        .await?;
    Ok(OwnerTransitionFixture {
        root,
        service,
        controller,
        session,
        provider,
        engine,
        sink,
        reader,
        original,
    })
}

/// Preserve failed race fixtures and their final owner trace instead of losing timeout evidence.
pub(super) fn retain_owner_transition_failure(
    fixture: OwnerTransitionFixture,
    error: anyhow::Error,
) -> anyhow::Error {
    let trace_path = fixture
        .controller
        .trace
        .session_status(&fixture.session)
        .path;
    let trace = std::fs::read_to_string(&trace_path)
        .unwrap_or_else(|failure| format!("trace unavailable: {failure}"));
    let tail = trace
        .lines()
        .rev()
        .take(80)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n");
    let root = fixture.root.keep();
    error.context(format!(
        "retained owner fixture: {}; trace: {trace_path}\n{tail}",
        root.display()
    ))
}

/// Retain a custom provider fixture with the stage that stopped progressing.
pub(super) fn retain_cleanup_failure(
    root: tempfile::TempDir,
    controller: &SessionController,
    error: anyhow::Error,
) -> anyhow::Error {
    let session = controller.observer.session_id();
    let trace_path = controller.trace.session_status(session).path;
    let trace = std::fs::read_to_string(&trace_path)
        .unwrap_or_else(|failure| format!("trace unavailable: {failure}"));
    let tail = trace
        .lines()
        .rev()
        .take(80)
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect::<Vec<_>>()
        .join("\n");
    let retained = root.keep();
    error.context(format!(
        "retained cleanup fixture: {}; trace: {trace_path}\n{tail}",
        retained.display()
    ))
}
