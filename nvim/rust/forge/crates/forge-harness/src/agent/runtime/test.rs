use super::*;
use crate::provider::ScopeOwner;
use crate::provider::submission::SubmissionStore;
use crate::provider::{PromptMode, ProviderInput, ServiceTier};
use crate::session::ApprovalMode;

/// Persist actual child mutation facts separately when the canonical native outcome is failure.
#[tokio::test]
async fn committed_child_file_change_survives_native_error() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let path = directory.path().join("changed.rs");
    std::fs::write(&path, "old")?;
    let context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
    let slot = context.provider_completion.clone();
    let retained = slot.clone();
    let changed_path = path.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )?,
        move |request, _writer, start, _cancel| {
            let slot = retained.clone();
            let path = changed_path.clone();
            async move {
                start.observed(1).await?;
                std::fs::write(&path, "new").map_err(crate::provider::test_support::io_failure)?;
                let mut completion = ProviderCompletion::default();
                completion
                    .host_file_changes
                    .push(crate::tools::lsp::HostFileChange {
                        scope: request.scope.clone(),
                        call_id: forge_provider_api::control::ToolCallId::new("committed-call")
                            .unwrap(),
                        native_item_id: None,
                        changes: forge_provider_api::ProviderChangeSet {
                            file: vec![forge_provider_api::ProviderFileChange {
                                path: path.to_string_lossy().into_owned(),
                                move_path: None,
                                kind: forge_provider_api::ProviderChangeKind::Update,
                                diff: "@@ -1 +1 @@\n-old\n+new\n".into(),
                            }],
                        },
                    });
                slot.publish(completion)?;
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Failed {
                        error: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                        retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                    },
                ))
            }
        },
    );
    let session = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let provider = session.provider_handle();
    let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
    let coordinator = crate::agent::AgentCoordinator::new("session".into());
    let (_cancel, cancellation) = watch::channel(false);
    let owner = ThreadOwner::new("session".into(), forge_provider_api::AgentId::new("child")?)?;
    AgentRuntime {
        execution_registry: Arc::new(
            crate::provider::session_execution::SessionExecutionRegistry::new(
                provider.clone(),
                trace.clone(),
            ),
        ),
        provider,
        tool_timeout: Default::default(),
        trace: trace.clone(),
        submission_store: SubmissionStore::open(directory.path(), trace)?,
        permission: crate::provider::approval::PermissionCoordinator::load(
            directory.path().join("permissions.json"),
            directory.path(),
        )?,
        request: ProviderRequest {
            dispatch: Default::default(),
            execution_owner: Some(owner.clone()),
            harness_session_id: "session".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("change"),
            mode: PromptMode::Chat,
            model: "fixture".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: Some(context),
        },
        task_id: "child-task".into(),
        cancellation,
        sender: coordinator.sender(),
        lsp_lease: None,
    }
    .run()
    .await?;
    let (output, changes) = loop {
        match coordinator
            .try_receive()
            .context("child completion missing")?
        {
            AgentCommand::Completed {
                output,
                host_file_changes,
                ..
            } => break (output, host_file_changes),
            AgentCommand::Event { .. } => {}
            AgentCommand::Request { .. } => anyhow::bail!("fixture requested unrelated work"),
        }
    };
    assert!(output.is_err());
    assert_eq!(changes.len(), 1);
    assert_eq!(changes[0].scope.owner(), owner);
    assert_eq!(changes[0].scope.exchange_id.as_str(), "child-task");
    assert_eq!(changes[0].call_id.as_str(), "committed-call");
    assert!(changes[0].native_item_id.is_none());
    assert_eq!(std::fs::read_to_string(path)?, "new");
    assert!(slot.preview()?.batches.is_empty());
    assert_eq!(slot.collected_file_changes(), changes);
    Ok(())
}
