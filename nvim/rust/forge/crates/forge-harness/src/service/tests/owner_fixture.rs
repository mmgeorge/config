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
    pub(super) service: HarnessService,
    pub(super) controller: Arc<SessionController>,
    pub(super) session: String,
    pub(super) provider: Arc<crate::provider::Provider>,
    /// Holds the root execution owner so the held invocation stays admitted.
    _engine: Arc<crate::provider::session_execution::SessionExecution>,
    pub(super) sink: MessageSender,
    pub(super) reader: tokio::task::JoinHandle<Result<()>>,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _root: tempfile::TempDir,
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
    script
        .dispatch(
            &session,
            MockResponseCommand::Wait {
                id: hold["id"].as_str().unwrap().into(),
            },
        )
        .await?;
    let execution_registry = controller.broker.lock().await.execution_registry();
    let engine = execution_registry
        .owner(crate::provider::ThreadOwner::root(&session))
        .await?;
    Ok(OwnerTransitionFixture {
        _root: root,
        service,
        controller,
        session,
        provider,
        _engine: engine,
        sink,
        reader,
    })
}
