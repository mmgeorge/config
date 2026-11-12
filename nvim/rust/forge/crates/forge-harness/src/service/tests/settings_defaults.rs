use super::bootstrap::{initialize, service};
use super::response_for;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::time::Duration;

#[tokio::test]
async fn global_defaults_do_not_create_or_modify_sessions() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .initialize_services(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    assert!(registry.controller_by_id.read().await.is_empty());
    let (sink, mut output) = forge_protocol::outbound::channel();
    service
        .dispatch(
            None,
            Request {
                id: 2,
                method: "settings.get".into(),
                params: json!({"provider":"mock", "include_models":true}),
            },
            &sink,
        )
        .await?;
    let defaults = response_for(&mut output, 2).await?["result"].clone();
    assert!(defaults["model_catalog"]["models"].is_array());
    let revision = defaults["revision"]
        .as_u64()
        .context("defaults revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 3,
                method: "settings.update".into(),
                params: json!({"scope":"global", "expected_revision":revision,
            "desired":{"approval_mode":"deny", "status_thoughts":false}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 3).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("saved revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 4,
                method: "settings.update".into(),
                params: json!({"provider":"mock", "expected_revision":revision,
            "provider_settings":{"model":"mock-model", "effort":"low"}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 4).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("provider revision missing")?;
    assert!(
        registry.controller_by_id.read().await.is_empty(),
        "defaults created a controller"
    );

    let opened = service
        .open_session(5, initialize(&fixture, "mock"))
        .await?;
    let session_id = opened.result().context("session result missing")?["session"]["id"]
        .as_str()
        .context("session id missing")?
        .to_owned();
    let controller = registry.resolve(&session_id).await?;
    let before = controller.broker.lock().await.snapshot()?.session;
    assert_eq!(before.approval_mode, crate::session::ApprovalMode::Deny);
    assert_eq!(before.model, "mock-model");
    service
        .dispatch(
            None,
            Request {
                id: 6,
                method: "settings.update".into(),
                params: json!({"scope":"global", "expected_revision":revision,
            "desired":{"approval_mode":"ask", "status_thoughts":true}}),
            },
            &sink,
        )
        .await?;
    let saved = response_for(&mut output, 6).await?["result"].clone();
    let revision = saved["revision"]
        .as_u64()
        .context("updated revision missing")?;
    service
        .dispatch(
            None,
            Request {
                id: 7,
                method: "settings.update".into(),
                params: json!({"provider":"mock", "expected_revision":revision,
            "provider_settings":{"effort":"high"}}),
            },
            &sink,
        )
        .await?;
    response_for(&mut output, 7).await?;
    let after = controller.broker.lock().await.snapshot()?.session;
    assert_eq!(after.approval_mode, before.approval_mode);
    assert_eq!(after.effort, before.effort);
    let next = crate::broker::prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        "service-test",
        "mock",
        false,
        &session_id,
        &json!({}),
        1,
    )?;
    assert_eq!(next.approval_mode, crate::session::ApprovalMode::Ask);
    assert_eq!(next.effort, "high");
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}
