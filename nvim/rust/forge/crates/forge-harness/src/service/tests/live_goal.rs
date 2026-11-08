use super::bootstrap::{initialize, service};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::Value;
use serde_json::json;
use std::time::Duration;

/// Logical goal controls commit before a provider that accepts only after its response.
#[tokio::test]
async fn live_goal_controls_commit_before_native_acceptance() -> Result<()> {
    use crate::provider::mock_response::MockResponseCommand;
    use forge_provider_api::testing::{
        TestInputAcceptance, TestPromptScript, TestPromptStep, TestPromptTerminal,
    };
    for (tool, field, message) in [
        ("goal_complete", "summary", "All requested work is complete"),
        (
            "goal_blocked",
            "reason",
            "The required external input is unavailable",
        ),
    ] {
        let root = tempfile::tempdir()?;
        crate::checks::configure_fixture(root.path());
        let service = service();
        service.open_session(1, initialize(&root, "mock")).await?;
        let registry = service.registry().await?;
        let session_id = registry.initial_session_id().to_owned();
        let controller = registry.resolve(&session_id).await?;
        controller.trace.configure(true)?;
        let (sink, mut output) = forge_protocol::outbound::channel();
        let reader = tokio::spawn(async move {
            while output.recv().await?.is_some() {}
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_input(&sink).await?;
        let mut arguments = serde_json::Map::new();
        arguments.insert(field.into(), message.into());
        controller
            .provider_handle()
            .mock_response_control()
            .context("mock control missing")?
            .dispatch(
                &session_id,
                MockResponseCommand::Script {
                    script: TestPromptScript {
                        acceptance: TestInputAcceptance::AfterResponse,
                        steps: vec![TestPromptStep::ToolCall {
                            name: tool.into(),
                            arguments: Value::Object(arguments),
                        }],
                        terminal: TestPromptTerminal::Completed,
                    },
                },
            )
            .await?;
        service.dispatch(Some(session_id.clone()), Request {
            id: 2, method: "session.command.apply".into(),
            params: json!({"operation_id":"live-goal","action":"goal","text":"Finish the requested work"}),
        }, &sink).await?;
        let settled = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                let store = SqliteStore::read_only(&controller.data_root)?;
                if store
                    .latest_exchange(&session_id)?
                    .is_some_and(|exchange| exchange.is_finished())
                    && controller.coordinator.execution.available_permits() > 0
                {
                    break Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await;
        if let Err(error) = settled {
            let trace = controller.trace.session_status(&session_id).path;
            let retained = root.keep();
            return Err(anyhow::Error::from(error)).context(format!(
                "{tool} did not settle; fixture={}; trace={trace}",
                retained.display()
            ));
        }
        settled??;
        let store = SqliteStore::read_only(&controller.data_root)?;
        if tool == "goal_complete" {
            assert!(controller.task_controller.current()?.is_none());
            assert!(store.list_session_event(&session_id)?.iter().any(|event| matches!(
                &event.detail, crate::timeline::SessionEventKind::TaskFinished { outcome }
                    if matches!(outcome.state, crate::task::TaskOutcomeState::Complete) && outcome.summary == message
            )));
        } else {
            let task = controller
                .task_controller
                .current()?
                .context("blocked goal disappeared")?;
            assert_eq!(task.status(), crate::task::TaskStatus::Blocked);
            let crate::task::WorkflowSnapshot::Goal { goal } = &task.workflow else {
                anyhow::bail!("blocked control changed task kind");
            };
            assert_eq!(goal.reported_status.as_deref(), Some(message));
            assert!(
                task.entry_accepted(),
                "true native acknowledgment must still be applied"
            );
        }
        let receipts = rusqlite::Connection::open_with_flags(
            crate::storage::state::directory(&controller.data_root)
                .join("provider-submissions.sqlite3"),
            rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        let receipt: String =
            receipts.query_row("SELECT id FROM submission LIMIT 1", [], |row| row.get(0))?;
        controller.trace.flush()?;
        let path = controller.trace.session_status(&session_id).path;
        let events = std::fs::read_to_string(&path)?
            .lines()
            .map(serde_json::from_str::<Value>)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        let committed = events
            .iter()
            .position(|event| event["event"] == "goal.control_committed")
            .context("goal control did not commit")?;
        let accepted = events
            .iter()
            .position(|event| {
                event["event"] == "provider.submission"
                    && event["payload"]["submission_id"] == receipt
                    && event["payload"]["delivery"] == "accepted"
            })
            .context("native acceptance missing")?;
        assert!(
            committed < accepted,
            "logical goal commit must precede true native acceptance"
        );
        assert_eq!(events[committed]["payload"]["entry_accepted"], false);
        assert!(
            events[committed]["payload"]["observed_turn"].is_null(),
            "live control must not manufacture a completed native turn"
        );
        service.shutdown(Duration::from_secs(5)).await?;
        drop(sink);
        reader.await??;
    }
    Ok(())
}
