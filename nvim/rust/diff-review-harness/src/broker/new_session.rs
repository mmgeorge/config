use crate::session::{ExecutionMode, HarnessMode, HarnessSession, ProviderForkState, SessionStore};
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
    let child = HarnessSession {
        id: Uuid::new_v4().to_string(),
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
        model: source.model,
        provider_label: source.provider_label,
        resolved_model: source.resolved_model,
        effort: source.effort,
        context_window: source.context_window,
        fast_mode: source.fast_mode,
        execution_mode: ExecutionMode::Read,
        mode: HarnessMode::Read,
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
