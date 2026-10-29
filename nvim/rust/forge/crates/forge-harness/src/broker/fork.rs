use super::{HarnessBroker, resolve_fork_name};
use crate::backend::{
    BackendCapability, BackendForkRequest, BackendForkResult, BackendInput, BackendRequest,
    BackendTimingRecord, PromptMode,
};
use crate::session::{
    HarnessSession, ProviderForkPoint, ProviderForkState, SessionPurpose, SessionStore,
};
use crate::storage::SqliteStore;
use crate::timeline::{
    SessionEventKind, SessionEventRecord, TimelineEntry, TimelineProjection, TimelineProjector,
};
use anyhow::{Context, Result};
use forge_protocol::message::SessionEvent;
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
    purpose: SessionPurpose,
) -> Result<ForkPreparation> {
    let fork_started = Instant::now();
    let persistence_started = Instant::now();
    let mut store = SqliteStore::open(data_root)?;
    let source_session_id = params
        .get("session_id")
        .and_then(Value::as_str)
        .unwrap_or(default_source_session_id);
    let mut source = store
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
    source.access =
        crate::backend::BackendKind::parse(&source.backend)?.resolve_access(source.access);

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
    child.purpose = purpose;
    child.id.clone_from(&target_session_id);
    child.primary_agent_id = HarnessSession::primary_agent_id(&target_session_id);
    child.name = resolve_fork_name(params, &source.name, &existing_name_set);
    child.backend_session_id = None;
    child.provider_checkpoint_id = None;
    child.provider_fork_state = ProviderForkState::Preparing {
        source_session_id: source.id.clone(),
        point: fork_point.clone(),
    };
    child.context_usage.clone_from(&source.context_usage);
    child.approval_mode = source.approval_mode;

    child.active_task = None;
    child.created_at_ms = now_ms;
    child.updated_at_ms = now_ms;
    child.lease_owner = Some(client_id.to_owned());
    crate::settings::SettingsStore::open(data_root)?.fork_session(&source.id, &child.id)?;
    let plan_file = crate::plan::PlanFileStore::new(data_root, &source.workspace);
    let history = store.read_snapshot(|| {
        let interaction = store.list_exchange(&source.id)?;
        let plan_list = store.list_plan(&source.id)?;
        let agent_run_list = store
            .list_agent_run(&source.id)?
            .into_iter()
            .filter(|agent| !agent.is_primary())
            .collect::<Vec<_>>();
        let mut agent_exchange_list = Vec::new();
        for agent in &agent_run_list {
            agent_exchange_list.extend(store.list_agent_exchange(&agent.id)?);
        }
        let timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: interaction.clone(),
            plan_list: &plan_list,
            lifecycle_list: store.list_plan_lifecycle(&source.id)?,
            execution_list: store.list_plan_execution(&source.id)?,
            deviation_list: store.list_plan_deviation(&source.id)?,
            audit_list: store.list_plan_audit(&source.id)?,
            resolution_list: store.list_plan_resolution(&source.id)?,
            agent_run_list,
            agent_exchange_list: agent_exchange_list.clone(),
            session_event_list: store.list_session_event(&source.id)?,
            plan_file: &plan_file,
        })?;
        let timeline = TimelineProjector::with_history(
            timeline,
            crate::checks::CheckSupervisor::saved_history(
                &data_root.join(&source.id).join("checks"),
            )?,
            interaction.iter().chain(&agent_exchange_list),
        )?;
        let timeline = store.with_fork_history(&source.id, timeline, &plan_file)?;
        Ok(timeline
            .into_iter()
            .map(|entry| match entry {
                TimelineEntry::Inherited { .. } => entry,
                _ => TimelineEntry::Inherited {
                    captured_at_ms: now_ms,
                    entry: Box::new(entry),
                },
            })
            .collect::<Vec<_>>())
    })?;
    store.save_fork_history(
        &child,
        &history,
        &SessionEventRecord {
            id: Uuid::new_v4().to_string(),
            session_id: child.id.clone(),
            owner: crate::timeline::TimelineOwner::session(&child.id),
            created_at_ms: now_ms,
            detail: SessionEventKind::Forked {
                source_session_id: source.id.clone(),
                source_session_name: source.name.clone(),
            },
        },
    )?;

    let backend_request = BackendForkRequest {
        source: BackendRequest {
            execution_owner: None,
            harness_session_id: source.id,
            workspace: source.workspace,
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: source.model,
            effort: source.effort,
            context_window: source.context_window,
            service_tier: source.service_tier,
            access: source.access,
            approval_mode: source.approval_mode,
            backend_session_id: fork_point.backend_session_id,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
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
    ) -> Result<SessionEvent> {
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
        Ok(SessionEvent {
            session_id: self.session.id.clone(),
            event: event.into(),
            payload: json!({ "session": self.session_view()?, "timing": timing }),
        })
    }
}
