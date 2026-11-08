//! Derive host continuation authority from canonical original parent/child history.
use super::HarnessBroker;
use crate::agent::{
    AgentSource, AgentTask, ObservedAgent, ObservedAgentAvailability, ObservedAgentState,
};
use crate::exchange::{Exchange, ExchangeState};
use crate::provider::ScopeOwner;
use crate::storage::SqliteStore;
use anyhow::Result;
use std::collections::HashSet;

impl HarnessBroker {
    /// Acknowledge only stopped reports actually included in a durably accepted original user input.
    pub(super) fn acknowledge_user_child_reports(
        &mut self,
        scope: &crate::provider::InvocationScope,
        receipt: &std::sync::Arc<crate::provider::submission::Submission>,
    ) -> Result<()> {
        let record = receipt.record()?;
        if !record.is_user_input()
            || !matches!(
                record.delivery,
                crate::provider::submission::SubmissionState::Accepted { .. }
            )
            || record.request.acknowledged_child_reports.is_empty()
        {
            return Ok(());
        }
        anyhow::ensure!(
            record.request.owner()? == scope.owner() && scope.owner().is_root(),
            "user child report receipt crosses original owner"
        );
        let tasks = self.store.list_agent_task(&self.session.id)?;
        let captured = record
            .request
            .acknowledged_child_reports
            .iter()
            .map(|id| {
                tasks
                    .iter()
                    .find(|task| task.id == id.as_str())
                    .cloned()
                    .ok_or_else(|| anyhow::anyhow!("accepted child report has no retained task"))
            })
            .collect::<Result<Vec<_>>>()?;
        let acknowledged = self.store.acknowledge_child_reports(scope, &captured)?;
        for (agent, task) in acknowledged {
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task)?;
        }
        Ok(())
    }

    /// Require new user authorization after a stopped own cycle or unresolved stopped descendant.
    pub(super) fn requires_explicit_continuation(&self, exchange: &Exchange) -> Result<bool> {
        let tasks = self.store.list_agent_task(&self.session.id)?;
        let observed = self.store.list_observed_agents(&self.session.id)?;
        continuation_blocked(
            exchange,
            &tasks,
            &observed,
            &self.store,
            &mut HashSet::new(),
        )
    }
}

/// Follow only retained model-delegation and exact native-parent relationships.
fn continuation_blocked(
    exchange: &Exchange,
    tasks: &[AgentTask],
    observed: &[ObservedAgent],
    store: &SqliteStore,
    ancestors: &mut HashSet<String>,
) -> Result<bool> {
    anyhow::ensure!(
        ancestors.insert(exchange.id.clone()),
        "child continuation history contains a cycle"
    );
    if exchange
        .turn
        .last()
        .is_some_and(|turn| turn.requires_explicit_continuation())
    {
        ancestors.remove(&exchange.id);
        return Ok(true);
    }
    for task in tasks.iter().filter(|task| {
        task.source == AgentSource::Model
            && task.parent_exchange_id == exchange.id
            && !task.parent_reported
    }) {
        if matches!(
            task.state,
            ExchangeState::Interrupted | ExchangeState::Cancelled | ExchangeState::Failed
        ) {
            ancestors.remove(&exchange.id);
            return Ok(true);
        }
        if let Some(child) = store.load_exchange(&task.id)?
            && continuation_blocked(&child, tasks, observed, store, ancestors)?
        {
            ancestors.remove(&exchange.id);
            return Ok(true);
        }
    }
    for child in observed.iter().filter(|child| {
        exchange
            .turn
            .iter()
            .any(|turn| &turn.provider().scope == child.parent_scope())
    }) {
        let stopped = matches!(
            child.state(),
            ObservedAgentState::Finished(
                forge_provider_api::agent::AgentOutcome::Failed(_)
                    | forge_provider_api::agent::AgentOutcome::Interrupted
            )
        );
        if stopped || child.availability() == ObservedAgentAvailability::Unavailable {
            ancestors.remove(&exchange.id);
            return Ok(true);
        }
        for child_exchange in child.exchanges() {
            if child_exchange.turn.last().is_some_and(|turn| {
                matches!(
                    turn.state(),
                    crate::turn::TurnState::Finished {
                        outcome: crate::turn::TurnOutcome::Failed { .. }
                            | crate::turn::TurnOutcome::Interrupted
                    }
                )
            }) || continuation_blocked(child_exchange, tasks, observed, store, ancestors)?
            {
                ancestors.remove(&exchange.id);
                return Ok(true);
            }
        }
    }
    ancestors.remove(&exchange.id);
    Ok(false)
}
