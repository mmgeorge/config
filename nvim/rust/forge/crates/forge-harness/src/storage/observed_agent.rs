//! Store observed native-child history separately from managed task scheduling.
use crate::provider::ScopeOwner;

use super::SqliteStore;
use crate::agent::ObservedAgent;
use crate::provider::events::NativeEventSource;
use crate::provider::{EventScope, ObservedAgentAssociation};
use anyhow::{Context, Result};
use forge_provider_api::agent::{AgentEvent, AgentId};
use forge_provider_api::{BindingId, ProviderEvent};
use rusqlite::{TransactionBehavior, params};

impl SqliteStore {
    /// Persist a registry-proven summary without creating managed agent/task records.
    pub(crate) fn observe_native_agent(
        &mut self,
        association: &ObservedAgentAssociation,
        source: &NativeEventSource,
        event: &AgentEvent,
        now_ms: i64,
    ) -> Result<ObservedAgent> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut observed = retained(&transaction, association, now_ms)?;
        observed.observe_agent(association, source, event, now_ms)?;
        write(&transaction, &observed)?;
        transaction.commit()?;
        Ok(observed)
    }

    /// Persist child thread/turn/item observations under their original binding and owner.
    pub(crate) fn observe_native_agent_event(
        &mut self,
        association: &ObservedAgentAssociation,
        source: &NativeEventSource,
        scope: &EventScope,
        event: &ProviderEvent,
        now_ms: i64,
    ) -> Result<ObservedAgent> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut observed = retained(&transaction, association, now_ms)?;
        observed.observe_event(association, source, scope, event, now_ms)?;
        write(&transaction, &observed)?;
        transaction.commit()?;
        Ok(observed)
    }

    /// Persist host policy and waiting state for the exact registered native child.
    pub(crate) fn observe_native_agent_host_event(
        &mut self,
        association: &ObservedAgentAssociation,
        scope: &EventScope,
        event: &crate::provider::HostEvent,
        now_ms: i64,
    ) -> Result<ObservedAgent> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut observed = retained(&transaction, association, now_ms)?;
        observed.observe_host_event(association, scope, event, now_ms)?;
        write(&transaction, &observed)?;
        transaction.commit()?;
        Ok(observed)
    }

    /// Persist committed native-child host mutations independently of provider-native item snapshots.
    pub(crate) fn observe_native_agent_file_change(
        &mut self,
        association: &ObservedAgentAssociation,
        effect: &crate::tools::lsp::HostFileChange,
        now_ms: i64,
    ) -> Result<ObservedAgent> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let mut observed = retained(&transaction, association, now_ms)?;
        observed.observe_file_change(association, effect, now_ms)?;
        write(&transaction, &observed)?;
        transaction.commit()?;
        Ok(observed)
    }

    /// End observation for one original binding without replacing native outcomes.
    pub(crate) fn mark_observed_binding_unavailable(
        &mut self,
        binding: &BindingId,
    ) -> Result<Vec<ObservedAgent>> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        transaction.execute(
            "INSERT OR IGNORE INTO observed_binding_unavailable(binding_id) VALUES(?1)",
            [binding.as_str()],
        )?;
        let mut records = select(
            &transaction,
            "SELECT binding_id,agent_id,session_id,payload FROM observed_agent_record WHERE binding_id=?1 ORDER BY rowid",
            [binding.as_str()],
        )?;
        for record in &mut records {
            if record.mark_unavailable() {
                write(&transaction, record)?;
            }
        }
        transaction.commit()?;
        Ok(records)
    }

    /// Load observed children in first-association order for one retained host session.
    pub(crate) fn list_observed_agents(&self, session_id: &str) -> Result<Vec<ObservedAgent>> {
        select(
            &self.connection,
            "SELECT binding_id,agent_id,session_id,payload FROM observed_agent_record WHERE session_id=?1 ORDER BY rowid",
            [session_id],
        )
    }
}

/// Preserve an explicit binding fence even when a late host-only effect creates its first child record.
fn retained(
    connection: &rusqlite::Connection,
    association: &ObservedAgentAssociation,
    now_ms: i64,
) -> Result<ObservedAgent> {
    let mut observed = load(connection, association.binding_id(), association.agent_id())?
        .map_or_else(|| ObservedAgent::registered(association, now_ms), Ok)?;
    let unavailable: bool = connection.query_row(
        "SELECT EXISTS(SELECT 1 FROM observed_binding_unavailable WHERE binding_id=?1)",
        [association.binding_id().as_str()],
        |row| row.get(0),
    )?;
    if unavailable {
        observed.mark_unavailable();
    }
    Ok(observed)
}

/// Decode one record through both its checked domain and immutable SQL ownership columns.
fn select(
    connection: &rusqlite::Connection,
    query: &str,
    parameters: impl rusqlite::Params,
) -> Result<Vec<ObservedAgent>> {
    let mut statement = connection.prepare(query)?;
    let rows = statement
        .query_map(parameters, |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, String>(2)?,
                row.get::<_, String>(3)?,
            ))
        })?
        .collect::<rusqlite::Result<Vec<_>>>()?;
    rows.into_iter()
        .map(|(binding, agent, session, payload)| {
            let record: ObservedAgent =
                serde_json::from_str(&payload).context("decode observed native child")?;
            anyhow::ensure!(
                record.key().binding_id().as_str() == binding
                    && record.key().agent_id().as_str() == agent
                    && record.parent_scope().owner().session_id().as_str() == session,
                "stored native child disagrees with its immutable row ownership"
            );
            Ok(record)
        })
        .collect()
}

/// Select one original-generation child by its composite checked identity.
fn load(
    connection: &rusqlite::Connection,
    binding: &BindingId,
    agent: &AgentId,
) -> Result<Option<ObservedAgent>> {
    let mut records = select(
        connection,
        "SELECT binding_id,agent_id,session_id,payload FROM observed_agent_record WHERE binding_id=?1 AND agent_id=?2",
        params![binding.as_str(), agent.as_str()],
    )?;
    Ok(records.pop())
}

/// Commit an acyclic original-generation child snapshot without touching managed task tables.
fn write(transaction: &rusqlite::Transaction<'_>, observed: &ObservedAgent) -> Result<()> {
    let mut siblings = select(
        transaction,
        "SELECT binding_id,agent_id,session_id,payload FROM observed_agent_record WHERE session_id=?1",
        [observed.parent_scope().owner().session_id().as_str()],
    )?;
    siblings.retain(|sibling| sibling.key() != observed.key());
    siblings.push(observed.clone());
    ObservedAgent::hierarchy(&siblings)?;
    let written = transaction.execute(
        "INSERT INTO observed_agent_record(binding_id,agent_id,session_id,payload) VALUES(?1,?2,?3,?4) \
         ON CONFLICT(binding_id,agent_id) DO UPDATE SET payload=excluded.payload \
         WHERE observed_agent_record.session_id=excluded.session_id",
        params![observed.key().binding_id().as_str(), observed.key().agent_id().as_str(),
            observed.parent_scope().owner().session_id().as_str(), serde_json::to_string(observed)?])?;
    anyhow::ensure!(
        written == 1,
        "observed native child cannot move between host sessions"
    );
    Ok(())
}

/// Recover observation only while the caller holds exclusive session recovery authority.
pub(super) fn recover_observed_agents(
    transaction: &rusqlite::Transaction<'_>,
    session_id: &str,
) -> Result<()> {
    let mut records = select(
        transaction,
        "SELECT binding_id,agent_id,session_id,payload FROM observed_agent_record WHERE session_id=?1",
        [session_id],
    )?;
    for record in &records {
        transaction.execute(
            "INSERT OR IGNORE INTO observed_binding_unavailable(binding_id) VALUES(?1)",
            [record.key().binding_id().as_str()],
        )?;
    }
    for record in &mut records {
        if record.mark_unavailable() {
            write(transaction, record)?;
        }
    }
    Ok(())
}
