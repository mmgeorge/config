use crate::session::{PermissionMode, HarnessSession, ProviderForkState, SessionStore};
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
    let child = HarnessSession {
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
        model: preference.as_ref().map_or_else(|| source.model.clone(), |value| value.model.clone()),
        provider_label: source.provider_label,
        resolved_model: None,
        effort: preference.as_ref().map_or_else(|| source.effort.clone(), |value| value.effort.clone()),
        plan_executor: preference.as_ref().map_or(source.plan_executor, |value| value.plan_executor.clone()),
        plan_compact: preference.as_ref().map_or(source.plan_compact, |value| value.plan_compact),
        plan_auto_approve_revisions: preference.as_ref().map_or(source.plan_auto_approve_revisions, |value| value.plan_auto_approve_revisions),
        context_window: preference.as_ref().and_then(|value| value.model_setting.get(&value.model))
            .and_then(|setting| setting.context_window.clone()).or(source.context_window),
        service_tier: source.service_tier,
        access: preference.as_ref().map_or_else(|| source.access.clone(), |value| value.access.clone()),
        execution_mode: PermissionMode::Read,
        current_task_id: None, default_write_permission: preference.as_ref().map_or(source.default_write_permission, |value| value.default_write_permission), plan_permission: preference.as_ref().map_or(source.plan_permission, |value| value.plan_permission),
        created_at_ms: now_ms,
        updated_at_ms: now_ms,
        active_plan_id: None,
        goal_id: None,
        lease_owner: Some(client_id.to_owned()),
        lease_expires_at_ms: Some(now_ms + 30_000),
        native_fork: false,
        native_compact,
        context_usage: None,
    };
    store.save_session(&child)?;
    Ok(child)
}
