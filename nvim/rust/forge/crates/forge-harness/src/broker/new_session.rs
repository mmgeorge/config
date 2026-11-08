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
    configured_provider: &str,
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
        source.provider == configured_provider,
        "source session uses a different configured provider"
    );
    let preference = store.load_preference(&source.workspace, &source.provider)?;
    let model = preference
        .as_ref()
        .map_or_else(|| source.model.clone(), |value| value.model.clone());
    let same_model = model == source.model;
    let context_window = preference
        .as_ref()
        .and_then(|value| value.model_setting.get(&model))
        .map(|setting| setting.context_window.clone())
        .unwrap_or_else(|| same_model.then(|| source.context_window.clone()).flatten());
    let session_id = Uuid::new_v4().to_string();
    let mut child = HarnessSession {
        purpose: crate::session::SessionPurpose::Conversation,
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
        provider: source.provider,
        provider_session_id: None,
        provider_checkpoint_id: None,
        provider_fork_state: ProviderForkState::Ready,
        model,
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
        approval_timeout: source.approval_timeout,
        tool_timeout: source.tool_timeout,
        context_window,
        service_tier: if same_model {
            source.service_tier
        } else {
            Default::default()
        },
        access: preference
            .as_ref()
            .map_or_else(|| source.access.clone(), |value| value.access.clone()),
        shell: preference
            .as_ref()
            .map_or(source.shell, |value| value.shell),
        approval_mode: ApprovalMode::Ask,
        active_task: None,
        created_at_ms: now_ms,
        updated_at_ms: now_ms,
        lease_owner: Some(client_id.to_owned()),
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
        &child.provider,
    )?;
    shared.desired.apply(&mut child, native_compact)?;
    shared.apply_provider_defaults(&mut child)?;
    store.save_session(&child)?;
    Ok(child)
}
