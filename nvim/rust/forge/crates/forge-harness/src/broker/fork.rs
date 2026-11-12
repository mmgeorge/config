use super::{HarnessBroker, resolve_fork_name};
use crate::exchange::{Exchange, HistoryDisposition};
use crate::provider::{
    PromptMode, ProviderCapability, ProviderForkRequest, ProviderForkResult, ProviderInput,
    ProviderRequest, ProviderTimingRecord,
};
use crate::session::{
    HarnessSession, ProviderForkPoint, ProviderForkState, SessionPurpose, SessionStore,
};
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

/// Selects how much of the source conversation a fork copies.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ForkBoundary {
    /// The source provider is idle, so the fork copies its whole conversation.
    Current,
    /// The source provider is executing, so the fork stops at the last finished exchange. A native
    /// fork cannot copy a turn that still runs, and the child must not inherit an exchange that no
    /// runtime will ever settle.
    Settled,
}

/// Selects the role of one fork and reports the state of its source.
#[derive(Clone, Copy)]
pub struct ForkOptions<'source> {
    pub purpose: SessionPurpose,
    /// Reports whether the source is executing a turn.
    ///
    /// `prepare_provider_fork` asks while it holds the history writer. A turn that starts later
    /// cannot commit its exchange until the writer is returned, so the answer and the history the
    /// fork copies describe the same moment. An answer taken before the writer was held could
    /// report an idle source whose next queued turn has already written its exchange.
    pub source_is_executing: &'source dyn Fn() -> bool,
}

/// Represents a durable child session plus the provider work that materializes its native fork.
pub struct ForkPreparation {
    pub child: HarnessSession,
    /// The first native attempt. Absent when the source holds no native history to copy, in which
    /// case the child is already `Ready` and opens a fresh provider conversation.
    pub provider_request: Option<ProviderForkRequest>,
    pub timing: Vec<ProviderTimingRecord>,
    pub total_duration_ms: f64,
}

/// Names the native history position that matches a set of inherited exchanges.
enum NativeBoundary {
    /// The newest inherited provider turn reported this checkpoint.
    Checkpoint(String),
    /// An inherited exchange ran provider turns and recorded no checkpoint after them.
    Unrecorded,
    /// No inherited exchange ran a provider turn.
    Empty,
}

impl NativeBoundary {
    /// Find the boundary after the newest inherited exchange that ran a provider turn.
    ///
    /// Conversation rollback selects its rewind position by the same rule.
    fn after<'exchange>(inherited: impl DoubleEndedIterator<Item = &'exchange Exchange>) -> Self {
        for exchange in inherited.rev() {
            if let Some(checkpoint) = &exchange.provider_checkpoint_id {
                return Self::Checkpoint(checkpoint.clone());
            }
            if !exchange.turn.is_empty() {
                return Self::Unrecorded;
            }
        }
        Self::Empty
    }
}

/// Build one native fork attempt for a durable child.
///
/// The child is a copy of its source, so its model, access, and approval configure the target, and
/// a retry uses the settings the child holds at that time. `checkpoint_id` selects an immutable
/// history position; without it the provider copies the source's live conversation.
fn provider_fork_request(
    source_session_id: &str,
    child: &HarnessSession,
    point: &ProviderForkPoint,
    checkpoint_id: Option<String>,
) -> Result<ProviderForkRequest> {
    Ok(ProviderForkRequest {
        source: ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: source_session_id.to_owned(),
            workspace: child.workspace.clone(),
            input: ProviderInput::from_user_text(""),
            mode: PromptMode::Chat,
            model: child.model.clone(),
            effort: child.effort.clone(),
            context_window: child.context_window.clone(),
            service_tier: child.service_tier,
            access: child.access.clone(),
            approval_mode: child.approval_mode,
            provider_session_id: point
                .provider_session_id
                .clone()
                .map(TryInto::try_into)
                .transpose()?,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
            child_agent: None,
        },
        target_harness_session_id: child.id.clone(),
        checkpoint_id,
        attempt_id: Uuid::new_v4().to_string(),
    })
}

/// Persist a child session before provider preparation so the client can open its timeline at once.
///
/// Every decision that can reject the fork precedes the first write, so a rejected fork leaves no
/// child session behind.
pub fn prepare_provider_fork(
    data_root: &Path,
    client_id: &str,
    configured_provider: &str,
    capability: &ProviderCapability,
    default_source_session_id: &str,
    params: &Value,
    options: ForkOptions<'_>,
) -> Result<ForkPreparation> {
    let fork_started = Instant::now();
    let persistence_started = Instant::now();
    let mut store = crate::storage::writer::HistoryWriter::lease_for(data_root)?;
    let boundary = if (options.source_is_executing)() {
        ForkBoundary::Settled
    } else {
        ForkBoundary::Current
    };
    let source_session_id = params
        .get("session_id")
        .and_then(Value::as_str)
        .unwrap_or(default_source_session_id);
    let mut source = store
        .load_session(source_session_id)?
        .context("source session not found")?;
    anyhow::ensure!(
        source.native_fork || (source.id == default_source_session_id && capability.native_fork),
        "native fork is unavailable for this provider session"
    );
    anyhow::ensure!(
        source.provider == configured_provider,
        "source session uses a different configured provider"
    );
    source.access =
        forge_providers::registry::resolve(&forge_providers::registry::select(&source.provider)?)?
            .capability
            .resolve_access(source.access);

    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64;
    let plan_file = crate::plan::PlanFileStore::new(data_root, &source.workspace)?;
    let (history, native_boundary, source_inherits_provider_turn, unfinished_exchange_omitted) =
        store.read_snapshot(|| {
            let interaction = store.list_exchange(&source.id)?;
            // An executing source keeps its unfinished exchanges: the native fork stops before them,
            // and no runtime of the child would ever settle their copies.
            let omitted_exchange_id = interaction
                .iter()
                .filter(|exchange| {
                    boundary == ForkBoundary::Settled
                        && exchange.disposition() == HistoryDisposition::Current
                        && !exchange.is_finished()
                })
                .map(|exchange| exchange.id.clone())
                .collect::<HashSet<_>>();
            let native_boundary = NativeBoundary::after(interaction.iter().filter(|exchange| {
                exchange.disposition() == HistoryDisposition::Current
                    && !omitted_exchange_id.contains(&exchange.id)
            }));
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
            let mut timeline = TimelineProjector::with_history(
                timeline,
                crate::checks::CheckSupervisor::saved_history(
                    &crate::storage::state::directory(data_root)
                        .join(&source.id)
                        .join("checks"),
                )?,
                interaction.iter().chain(&agent_exchange_list),
            )?;
            // The projector nests the events, checks, and agents an exchange owns inside its entry,
            // so removing the entry removes everything that belongs to the unfinished exchange.
            timeline.retain(|entry| {
            !matches!(entry, TimelineEntry::Exchange { id, .. } if omitted_exchange_id.contains(id))
        });
            let timeline = store.with_fork_history(&source.id, timeline, &plan_file)?;
            // A fork child's native conversation holds the turns its own fork copied, and those reach
            // this timeline only as inherited entries, never through the child's exchange list.
            let source_inherits_provider_turn = timeline.iter().any(|entry| {
                matches!(
                    entry,
                    TimelineEntry::Inherited { entry, .. }
                        if matches!(
                            &**entry,
                            TimelineEntry::Exchange { exchange, .. }
                                if exchange.disposition() == HistoryDisposition::Current
                                    && (exchange.provider_checkpoint_id.is_some()
                                        || !exchange.turn.is_empty())
                        )
                )
            });
            let history = timeline
                .into_iter()
                .map(|entry| match entry {
                    TimelineEntry::Inherited { .. } => entry,
                    _ => TimelineEntry::Inherited {
                        captured_at_ms: now_ms,
                        entry: Box::new(entry),
                    },
                })
                .collect::<Vec<_>>();
            Ok((
                history,
                native_boundary,
                source_inherits_provider_turn,
                !omitted_exchange_id.is_empty(),
            ))
        })?;

    let recorded_checkpoint_id = match &native_boundary {
        NativeBoundary::Checkpoint(checkpoint) => Some(checkpoint.clone()),
        NativeBoundary::Unrecorded | NativeBoundary::Empty => None,
    };
    // A settled fork must name an immutable position, because the live conversation is moving.
    let copies_native_history = match (boundary, &native_boundary) {
        // A provider writes a conversation when its first turn runs, so a source that neither ran
        // nor inherited a turn has none to copy: the Claude CLI and the Codex app-server both
        // reject that fork. The child opens fresh provider history, which is the same conversation.
        (_, NativeBoundary::Empty) if !source_inherits_provider_turn => false,
        (ForkBoundary::Current, _) => true,
        (ForkBoundary::Settled, NativeBoundary::Checkpoint(_)) => {
            anyhow::ensure!(
                source.provider_session_id.is_some(),
                "cannot fork while the session is running: it holds no provider conversation to copy. Fork again when its turn finishes"
            );
            true
        }
        (ForkBoundary::Settled, NativeBoundary::Unrecorded) => anyhow::bail!(
            "cannot fork while the session is running: its last finished exchange recorded no provider checkpoint. Fork again when its turn finishes"
        ),
        // A fork that inherited provider turns holds them in its native conversation, and records
        // no checkpoint of its own until its first turn finishes.
        (ForkBoundary::Settled, NativeBoundary::Empty) => anyhow::bail!(
            "cannot fork a forked session during its first turn. Fork again when that turn finishes"
        ),
    };
    let fork_point = if copies_native_history {
        ProviderForkPoint {
            provider_session_id: source.provider_session_id.clone(),
            checkpoint_id: recorded_checkpoint_id,
        }
    } else {
        ProviderForkPoint::default()
    };

    let target_session_id = Uuid::new_v4().to_string();
    let existing_name_set = store
        .list_session(Some(&source.workspace))?
        .into_iter()
        .map(|session| session.name)
        .collect::<HashSet<_>>();
    let mut child = source.clone();
    child.purpose = options.purpose;
    child.aside = match options.purpose {
        // `aside` opens an aside of the source. Without it the child keeps the source's own
        // marker, so a fork of an aside still reaches the aside's origin.
        SessionPurpose::Conversation
            if params.get("aside").and_then(Value::as_bool) == Some(true) =>
        {
            Some(crate::session::AsideOrigin {
                session_id: source.id.clone(),
                session_name: source.name.clone(),
            })
        }
        SessionPurpose::Conversation => source.aside.clone(),
        SessionPurpose::PlanReviewAnswer => None,
    };
    child.id.clone_from(&target_session_id);
    child.primary_agent_id = HarnessSession::primary_agent_id(&target_session_id);
    child.name = resolve_fork_name(params, &source.name, &existing_name_set);
    child.provider_session_id = None;
    child.provider_checkpoint_id = None;
    child.provider_fork_state = if copies_native_history {
        ProviderForkState::Preparing {
            source_session_id: source.id.clone(),
            point: fork_point.clone(),
        }
    } else {
        // The child opens a fresh provider conversation, which the source's usage does not
        // describe.
        child.context_usage = None;
        ProviderForkState::Ready
    };
    child.approval_mode = source.approval_mode;

    child.active_task = None;
    child.created_at_ms = now_ms;
    child.updated_at_ms = now_ms;
    child.lease_owner = Some(client_id.to_owned());
    // A settled fork reads the checkpoint. An idle source's live conversation is the more exact
    // boundary, and the recorded checkpoint stays in the fork point for a later attempt.
    let provider_request = copies_native_history
        .then(|| {
            provider_fork_request(
                &source.id,
                &child,
                &fork_point,
                match boundary {
                    ForkBoundary::Current => None,
                    ForkBoundary::Settled => fork_point.checkpoint_id.clone(),
                },
            )
        })
        .transpose()?;
    crate::settings::SettingsStore::open(data_root)?.fork_session(&source.id, &child.id)?;
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
                unfinished_exchange_omitted,
            },
        },
    )?;

    let timing = vec![ProviderTimingRecord {
        phase: "broker.persist_child".into(),
        duration_ms: persistence_started.elapsed().as_secs_f64() * 1000.0,
    }];
    Ok(ForkPreparation {
        child,
        provider_request,
        timing,
        total_duration_ms: fork_started.elapsed().as_secs_f64() * 1000.0,
    })
}

impl HarnessBroker {
    /// Return the provider preparation state that gates child-session provider work.
    pub fn provider_fork_state(&self) -> ProviderForkState {
        self.session.provider_fork_state.clone()
    }

    /// Reopen a failed provider fork and build its next native attempt.
    ///
    /// The attempt reads the recorded checkpoint when the fork point has one, because the source
    /// conversation may have advanced since the child captured its inherited history. Returns
    /// `None` when the fork is already ready.
    pub fn retry_provider_fork(&mut self) -> Result<Option<ProviderForkRequest>> {
        let (source_session_id, point) = match self.session.provider_fork_state.clone() {
            ProviderForkState::Ready => return Ok(None),
            ProviderForkState::Preparing { .. } => {
                anyhow::bail!("provider fork preparation is already running")
            }
            ProviderForkState::Failed {
                source_session_id,
                point,
                ..
            } => (source_session_id, point),
        };
        let request = provider_fork_request(
            &source_session_id,
            &self.session,
            &point,
            point.checkpoint_id.clone(),
        )?;
        let failed = std::mem::replace(
            &mut self.session.provider_fork_state,
            ProviderForkState::Preparing {
                source_session_id,
                point,
            },
        );
        self.session.updated_at_ms = self.clock.now_ms();
        if let Err(error) = self.save_session() {
            self.session.provider_fork_state = failed;
            return Err(error);
        }
        Ok(Some(request))
    }

    /// Build the checkpoint attempt that follows a live-conversation attempt the provider rejected
    /// because the source started executing.
    ///
    /// Returns `None` when the pending fork point recorded no checkpoint.
    pub fn provider_fork_checkpoint_request(&self) -> Result<Option<ProviderForkRequest>> {
        let ProviderForkState::Preparing {
            source_session_id,
            point,
        } = &self.session.provider_fork_state
        else {
            anyhow::bail!("session does not have a pending provider fork")
        };
        let Some(checkpoint_id) = point.checkpoint_id.clone() else {
            return Ok(None);
        };
        provider_fork_request(source_session_id, &self.session, point, Some(checkpoint_id))
            .map(Some)
    }

    /// Commit provider preparation into the durable child before queued work enters the provider.
    pub fn complete_provider_fork(
        &mut self,
        result: std::result::Result<ProviderForkResult, String>,
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
                self.session.provider_session_id = Some(result.provider_session_id.into());
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
            payload: json!({ "snapshot": self.snapshot()?, "timing": timing }),
        })
    }
}
