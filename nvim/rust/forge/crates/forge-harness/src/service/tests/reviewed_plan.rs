use super::super::{HarnessService, SessionController};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use forge_protocol::outbound::MessageSender;
use serde_json::Value;
use serde_json::json;
use std::sync::Arc;

/// Submit a valid declaration design through the real owner and normalized tool adapter.
pub(super) async fn reviewed_execution_fixture(
    service: &HarnessService,
    controller: &Arc<SessionController>,
    session_id: &str,
    sink: &MessageSender,
) -> Result<Value> {
    reviewed_execution_fixture_with_acceptance(
        service,
        controller,
        session_id,
        sink,
        forge_provider_api::testing::TestInputAcceptance::BeforeResponse,
        false,
    )
    .await
}

/// Prepare review state at the selected native acceptance boundary.
pub(super) async fn reviewed_execution_fixture_with_acceptance(
    service: &HarnessService,
    controller: &Arc<SessionController>,
    session_id: &str,
    sink: &MessageSender,
    acceptance: forge_provider_api::testing::TestInputAcceptance,
    edit_and_resubmit: bool,
) -> Result<Value> {
    reviewed_fixture(
        service,
        controller,
        session_id,
        sink,
        acceptance,
        edit_and_resubmit,
        |_| Ok(()),
    )
    .await
}

async fn reviewed_fixture(
    service: &HarnessService,
    controller: &Arc<SessionController>,
    session_id: &str,
    sink: &MessageSender,
    acceptance: forge_provider_api::testing::TestInputAcceptance,
    edit_and_resubmit: bool,
    customize: impl FnOnce(&mut crate::plan::DeclarationDesign) -> Result<()>,
) -> Result<Value> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{TestPromptScript, TestPromptStep, TestPromptTerminal};
    controller.trace.configure(true)?;
    let workspace = controller.observer.snapshot()?.session.workspace;
    let mut document = crate::plan::test_fixture("fixture-review-plan", "Inspect the project");
    let mut design = crate::plan::DeclarationDesign::open(std::path::Path::new(&workspace))?;
    design.document.objective = "Preserve the project ownership contract".into();
    design.document.background = "A saved design for execution transition tests".into();
    design.document.design = "Keep application state in its owner.".into();
    design.proposed.insert(
        "src/owner.rs".into(),
        "/// Own application state.\npub struct Owner;\n".into(),
    );
    customize(&mut design)?;
    document.design = Some(design);
    let plan = controller
        .observer
        .plan_file
        .prepare_entry(session_id, "Inspect the project".into(), Some(document), 1)?
        .record;
    SqliteStore::open(&controller.data_root)?.save_plan(&plan)?;
    // Opening a test registry and attaching its input worker do not start the provider.
    service.start_provider_session(session_id, sink).await?;
    // Arm the scenario only after startup has installed the native session.
    async {
        while !controller.provider_ready().await {
            if let Some(error) = controller
                .provider_startup_error
                .lock()
                .expect("provider startup state poisoned")
                .clone()
            {
                anyhow::bail!("review fixture provider startup failed: {error}");
            }
            tokio::task::yield_now().await;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await?;
    let provider = controller.provider_handle();
    let mut steps = Vec::new();
    let mut submitted_version = plan.document_version;
    if edit_and_resubmit {
        steps.push(TestPromptStep::ToolCall {
            name: "plan_submit".into(),
            arguments: json!({"plan_id":plan.id,"expected_version":submitted_version}),
        });
        steps.push(TestPromptStep::ToolCall {
            name: "plan_overview_edit".into(),
            arguments: json!({"plan_id":plan.id,"expected_version":submitted_version,
                "design":"Commit each plan change before returning its tool response."}),
        });
        submitted_version = submitted_version
            .checked_add(1)
            .context("fixture version exhausted")?;
    }
    steps.push(TestPromptStep::ToolCall {
        name: "plan_submit".into(),
        arguments: json!({"plan_id":plan.id,"expected_version":submitted_version}),
    });
    provider
        .mock_response_control()
        .context("fixture control is missing")?
        .dispatch(
            session_id,
            MockResponseCommand::Script {
                script: TestPromptScript {
                    acceptance,
                    steps,
                    terminal: TestPromptTerminal::Completed,
                },
            },
        )
        .await?;
    service.dispatch(Some(session_id.to_owned()), Request {
        id: 31, method: "session.command.apply".into(),
        params: json!({"operation_id":"fixture-plan","action":"attach_plan","plan_id":plan.id}),
    }, sink).await?;
    let plan = loop {
        if let Some(task) = controller.task_controller.current()?
            && let crate::task::WorkflowSnapshot::Plan { plan } = task.workflow.clone()
            && plan.artifact.state == crate::plan::PlanState::AwaitingReview
            && controller.coordinator.execution.available_permits() > 0
        {
            break plan.artifact;
        }
        tokio::task::yield_now().await;
    };
    let artifact_journal = std::path::Path::new(&plan.working_path)
        .parent()
        .context("fixture plan working directory missing")?
        .join("artifact-recovery.json");
    anyhow::ensure!(
        !artifact_journal.exists(),
        "review fixture returned before artifact reconciliation: {}: {}",
        artifact_journal.display(),
        std::fs::read_to_string(&artifact_journal).unwrap_or_default()
    );
    Ok(
        json!({"operation_id":"fixture-execute","action":"execute","plan_id":plan.id,"digest":plan.review_digest}),
    )
}
