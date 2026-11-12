// This fixture builds storage through a handle of its own, outside any running host.
#![allow(clippy::disallowed_methods)]
//! Measure exchange serialization and timeline replacement without modifying retained state.

use std::{hint::black_box, time::Instant};

use anyhow::{Context, Result};
use forge_harness::{
    exchange::Exchange,
    session::{HarnessSession, SessionStore, state_machine::SessionPhase},
    storage::SqliteStore,
    timeline::stream::TimelineStream,
};
use rusqlite::{Connection, OpenFlags};

/// Read one exchange as diagnostic input and measure independent processing stages.
fn main() -> Result<()> {
    let arguments: Vec<_> = std::env::args().skip(1).collect();
    anyhow::ensure!(
        arguments.len() == 2,
        "usage: exchange_profile DATABASE EXCHANGE_ID"
    );
    let connection = Connection::open_with_flags(&arguments[0], OpenFlags::SQLITE_OPEN_READ_ONLY)?;
    let encoded: String = connection.query_row(
        "SELECT payload FROM exchange_record WHERE id=?1",
        [&arguments[1]],
        |row| row.get(0),
    )?;
    let mut exchange: Exchange = serde_json::from_str(&encoded)
        .context("diagnostic exchange is not readable by this build; no conversion attempted")?;
    println!("payload_bytes={}", encoded.len());
    measure("clone", || Ok(black_box(exchange.clone())))?;
    measure("serialize_value", || {
        Ok(black_box(serde_json::to_value(&exchange)?))
    })?;
    measure("serialize_string", || {
        Ok(black_box(serde_json::to_string(&exchange)?))
    })?;
    measure("parse_value", || {
        Ok(black_box(serde_json::from_str::<serde_json::Value>(
            &encoded,
        )?))
    })?;
    measure("decode_exchange", || {
        Ok(black_box(serde_json::from_str::<Exchange>(&encoded)?))
    })?;
    // Only the sample exchange is measured in a fresh, disconnected fixture. No
    // session is resumed or converted, and no provider or editor is attached.
    let temporary = tempfile::tempdir()?;
    let mut store = SqliteStore::open(temporary.path())?;
    let session = fixture_session(&exchange.session_id, temporary.path());
    store.save_session(&session)?;
    store.save_agent_run(&forge_harness::agent::Agent::primary(&session.id, 0))?;
    exchange.agent_id = session.primary_agent_id.clone();
    store.save_exchange(&exchange)?;
    measure("save_exchange", || store.save_exchange(&exchange))?;
    let mut timeline = TimelineStream::new(exchange.session_id.clone());
    timeline.update_live(Some(&exchange), None, SessionPhase::Idle, false)?;
    measure("replace_timeline", || {
        exchange.prompt.push('.');
        Ok(black_box(timeline.update_live(
            Some(&exchange),
            None,
            SessionPhase::Idle,
            false,
        )?))
    })?;
    Ok(())
}

/// Create disconnected fixture ownership without importing source session settings.
fn fixture_session(id: &str, workspace: &std::path::Path) -> HarnessSession {
    HarnessSession {
        aside: None,
        purpose: Default::default(),
        background: None,
        auto_compaction: Default::default(),
        last_compaction_turn: None,
        id: id.into(),
        primary_agent_id: HarnessSession::primary_agent_id(id),
        name: "Profile fixture".into(),
        workspace: workspace.to_string_lossy().into_owned(),
        provider: "mock".into(),
        provider_session_id: None,
        provider_checkpoint_id: None,
        provider_fork_state: forge_harness::session::ProviderForkState::Ready,
        model: "default".into(),
        model_label: None,
        provider_label: "Mock".into(),
        resolved_model: None,
        effort: "default".into(),
        plan_executor: Default::default(),
        plan_compact: false,
        approval_timeout: Default::default(),
        tool_timeout: Default::default(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        shell: Default::default(),
        approval_mode: forge_harness::session::ApprovalMode::Ask,
        active_task: None,
        created_at_ms: 0,
        updated_at_ms: 0,
        lease_owner: None,
        native_fork: false,
        native_compact: false,
        context_usage: None,
    }
}

/// Report individual samples, including destruction, without retaining payload contents.
fn measure<T>(name: &str, mut operation: impl FnMut() -> Result<T>) -> Result<()> {
    let mut samples = Vec::new();
    for _ in 0..5 {
        let started = Instant::now();
        drop(operation()?);
        samples.push(started.elapsed().as_secs_f64() * 1000.0);
    }
    println!("{name}_ms={samples:?}");
    Ok(())
}
