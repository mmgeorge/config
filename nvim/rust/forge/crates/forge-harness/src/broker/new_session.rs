use crate::session::{ApprovalMode, HarnessSession, ProviderForkState, SessionStore};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use serde_json::Value;
use std::path::Path;
use uuid::Uuid;

/// Persist a provider-empty child without entering the source session's serialized controller.
pub fn prepare_new_session(
    data_root: &Path,
    client_id: &str,
    configured_backend: &str,
    native_compact: bool,
    source_session_id: &str,
    params: &Value,
    now_ms: i64,
) -> Result<HarnessSession> {
    let mut store = SqliteStore::open(data_root)?;
    let source = store
        .load_session(source_session_id)?
        .context("source session not found")?;
    anyhow::ensure!(
        source.backend == configured_backend,
        "source session uses a different configured backend"
    );
    let preference = store.load_preference(&source.workspace, &source.backend)?;
    let session_id = Uuid::new_v4().to_string();
    let mut child = HarnessSession {
        primary_agent_id: HarnessSession::primary_agent_id(&session_id),
        id: session_id,
        name: params
            .get("name")
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|name| !name.is_empty())
            .unwrap_or_default()
            .to_owned(),
        workspace: source.workspace,
        backend: source.backend,
        backend_session_id: None,
        provider_checkpoint_id: None,
        provider_fork_state: ProviderForkState::Ready,
        model: preference
            .as_ref()
            .map_or_else(|| source.model.clone(), |value| value.model.clone()),
        provider_label: source.provider_label,
        resolved_model: None,
        effort: preference
            .as_ref()
            .map_or_else(|| source.effort.clone(), |value| value.effort.clone()),
        plan_executor: preference
            .as_ref()
            .map_or(source.plan_executor, |value| value.plan_executor.clone()),
        plan_compact: preference
            .as_ref()
            .map_or(source.plan_compact, |value| value.plan_compact),
        plan_auto_approve_revisions: preference
            .as_ref()
            .map_or(source.plan_auto_approve_revisions, |value| {
                value.plan_auto_approve_revisions
            }),
        approval_timeout: source.approval_timeout,
        answer_timeout: source.answer_timeout,
        context_window: preference
            .as_ref()
            .and_then(|value| value.model_setting.get(&value.model))
            .and_then(|setting| setting.context_window.clone())
            .or(source.context_window),
        service_tier: source.service_tier,
        access: preference
            .as_ref()
            .map_or_else(|| source.access.clone(), |value| value.access.clone()),
        shell: preference
            .as_ref()
            .map_or(source.shell, |value| value.shell),
        approval_mode: ApprovalMode::Ask,
        current_task_id: None,
        default_approval: preference
            .as_ref()
            .map_or(source.default_approval, |value| value.default_approval),
        plan_approval: preference
            .as_ref()
            .map_or(source.plan_approval, |value| value.plan_approval),
        created_at_ms: now_ms,
        updated_at_ms: now_ms,
        goal_id: None,
        lease_owner: Some(client_id.to_owned()),
        lease_expires_at_ms: Some(now_ms + 30_000),
        native_fork: false,
        native_compact,
        context_usage: None,
        background: None,
        auto_compaction: Default::default(),
        last_compaction_turn: None,
    };
    let shared = crate::settings::SettingsStore::open(data_root)?.get(
        &child.workspace,
        &child.id,
        &child.backend,
    )?;
    shared.desired.apply(&mut child, native_compact)?;
    child.approval_mode = child.default_approval;
    shared.apply_provider_defaults(&mut child)?;
    if child.model == "default" && child.resolved_model.is_none() {
        child.resolved_model =
            store.latest_resolved_default_model(&child.workspace, &child.backend)?;
    }
    store.save_session(&child)?;
    Ok(child)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_conversation_reuses_resolved_default_before_catalog_request() {
        let fixture = tempfile::tempdir().unwrap();
        let mut broker = crate::broker::HarnessBroker::initialize(
            serde_json::from_value(serde_json::json!({
                "workspace": fixture.path(),
                "data_root": fixture.path().join("data"),
                "client_id": "cached-default-test",
                "backend": { "kind": "mock", "command": ["mock"] },
            }))
            .unwrap(),
        )
        .unwrap();
        broker.session.resolved_model = Some("confirmed-default".into());
        broker.save_session().unwrap();
        let created = prepare_new_session(
            &fixture.path().join("data"),
            "cached-default-test",
            "mock",
            false,
            &broker.session.id,
            &serde_json::json!({}),
            1,
        )
        .unwrap();
        assert_ne!(created.id, broker.session.id);
        assert_eq!(created.model, "default");
        assert_eq!(created.resolved_model.as_deref(), Some("confirmed-default"));
    }
}
