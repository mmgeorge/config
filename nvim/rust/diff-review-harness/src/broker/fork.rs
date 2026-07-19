use super::{HarnessBroker, resolve_fork_name};
use crate::backend::{
    BackendCapability, BackendForkRequest, BackendForkResult, BackendInput, BackendRequest,
    BackendTimingRecord, PromptMode,
};
use crate::protocol::BrokerEvent;
use crate::session::{
    ExecutionMode, HarnessSession, ProviderForkPoint, ProviderForkState, SessionStore,
};
use crate::storage::SqliteStore;
use crate::timeline::{SessionEventKind, SessionEventRecord};
use anyhow::{Context, Result};
use serde_json::{Value, json};
use std::collections::HashSet;
use std::path::Path;
use std::time::{Instant, SystemTime, UNIX_EPOCH};
use uuid::Uuid;

/// Represents a durable child session plus the provider work that materializes its native fork.
pub struct ForkPreparation {
    pub child: HarnessSession,
    pub backend_request: BackendForkRequest,
    pub timing: Vec<BackendTimingRecord>,
    pub total_duration_ms: f64,
}

/// Persist a child session before provider preparation so the client can open its timeline at once.
pub fn prepare_provider_fork(
    data_root: &Path,
    client_id: &str,
    configured_backend: &str,
    capability: &BackendCapability,
    default_source_session_id: &str,
    params: &Value,
) -> Result<ForkPreparation> {
    let fork_started = Instant::now();
    let persistence_started = Instant::now();
    let mut store = SqliteStore::open(data_root)?;
    let source_session_id = params
        .get("session_id")
        .and_then(Value::as_str)
        .unwrap_or(default_source_session_id);
    let source = store
        .load_session(source_session_id)?
        .context("source session not found")?;
    anyhow::ensure!(
        source.native_fork || (source.id == default_source_session_id && capability.native_fork),
        "native fork is unavailable for this backend session"
    );
    anyhow::ensure!(
        source.backend == configured_backend,
        "source session uses a different configured backend"
    );

    let target_session_id = Uuid::new_v4().to_string();
    let fork_point = ProviderForkPoint {
        backend_session_id: source.backend_session_id.clone(),
        checkpoint_id: source.provider_checkpoint_id.clone(),
    };
    let existing_name_set = store
        .list_session(Some(&source.workspace))?
        .into_iter()
        .map(|session| session.name)
        .collect::<HashSet<_>>();
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64;
    let mut child = source.clone();
    child.id.clone_from(&target_session_id);
    child.name = resolve_fork_name(params, &source.name, &existing_name_set);
    child.backend_session_id = None;
    child.provider_checkpoint_id = None;
    child.provider_fork_state = ProviderForkState::Preparing {
        source_session_id: source.id.clone(),
        point: fork_point.clone(),
    };
    child.context_usage.clone_from(&source.context_usage);
    child.execution_mode = ExecutionMode::Read;
    child.active_plan_id = None;
    child.goal_id = None;
    child.created_at_ms = now_ms;
    child.updated_at_ms = now_ms;
    child.lease_owner = Some(client_id.to_owned());
    child.lease_expires_at_ms = Some(now_ms + 30_000);
    store.save_session(&child)?;
    store.save_session_event(&SessionEventRecord {
        id: Uuid::new_v4().to_string(),
        session_id: child.id.clone(),
        created_at_ms: now_ms,
        detail: SessionEventKind::Forked {
            source_session_id: source.id.clone(),
            source_session_name: source.name.clone(),
        },
    })?;

    let backend_request = BackendForkRequest {
        source: BackendRequest {
            harness_session_id: source.id,
            workspace: source.workspace,
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: source.model,
            effort: source.effort,
            context_window: source.context_window,
            fast_mode: source.fast_mode,
            execution_mode: ExecutionMode::Read,
            backend_session_id: fork_point.backend_session_id,
        },
        target_harness_session_id: target_session_id,
        checkpoint_id: fork_point.checkpoint_id,
    };
    let timing = vec![BackendTimingRecord {
        phase: "broker.persist_child".into(),
        duration_ms: persistence_started.elapsed().as_secs_f64() * 1000.0,
    }];
    Ok(ForkPreparation {
        child,
        backend_request,
        timing,
        total_duration_ms: fork_started.elapsed().as_secs_f64() * 1000.0,
    })
}

impl HarnessBroker {
    /// Return the provider preparation state that gates child-session backend work.
    pub fn provider_fork_state(&self) -> ProviderForkState {
        self.session.provider_fork_state.clone()
    }

    /// Commit provider preparation into the durable child before queued work enters the backend.
    pub fn complete_provider_fork(
        &mut self,
        result: std::result::Result<BackendForkResult, String>,
    ) -> Result<BrokerEvent> {
        let ProviderForkState::Preparing {
            source_session_id,
            point,
        } = self.session.provider_fork_state.clone()
        else {
            anyhow::bail!("session does not have a pending provider fork")
        };
        let (event, timing) = match result {
            Ok(result) => {
                self.session.backend_session_id = Some(result.backend_session_id);
                self.session.provider_fork_state = ProviderForkState::Ready;
                ("session_fork_ready", result.timing)
            }
            Err(message) => {
                self.session.provider_fork_state = ProviderForkState::Failed {
                    source_session_id,
                    point,
                    message,
                };
                ("session_fork_failed", Vec::new())
            }
        };
        self.session.updated_at_ms = self.clock.now_ms();
        self.save_session()?;
        Ok(BrokerEvent {
            session_id: self.session.id.clone(),
            event: event.into(),
            payload: json!({ "session": self.session, "timing": timing }),
        })
    }
}
