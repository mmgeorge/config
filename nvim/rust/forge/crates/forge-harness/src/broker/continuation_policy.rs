//! Derive host continuation authority from canonical original parent/child history.
use super::HarnessBroker;
use crate::agent::{
    AgentSource, AgentTask, ObservedAgent, ObservedAgentAvailability, ObservedAgentState,
};
use crate::exchange::{Exchange, ExchangeState};
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
            record.request.owner()? == scope.owner && scope.owner.is_root(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::agent::{
        AgentActiveState, AgentDelegation, AgentDescription, AgentEvent, AgentLifecycleEvent,
        AgentOutcome,
    };

    /// A successor root cycle cannot hide the later stop of its original registered descendant.
    #[test]
    fn stopped_child_of_earlier_parent_turn_requires_explicit_continuation() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let mut parent = Exchange::agent_admission(&broker.session.id, 1, "Parent".into(), 1);
        let first = crate::turn::test_support::address(&parent, "first");
        parent.start_turn(first.clone(), 1)?;
        parent.observe_event(
            &crate::turn::test_support::completed_event(
                &first,
                forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                2,
            ),
            2,
        )?;
        let second = crate::turn::test_support::address(&parent, "second");
        parent.start_turn(second, 3)?;
        let child_scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::new(
                first.scope.owner.session_id().clone(),
                forge_provider_api::AgentId::new("observed-child-owner")?,
            )?,
            parent.id.clone(),
        );
        let child_turn = crate::provider::test_support::address(&child_scope);
        let public = forge_provider_api::AgentId::new("native-child-run")?;
        let description = AgentDescription {
            parent_agent_id: Some(first.scope.owner.agent_id().clone()),
            delegation: Some(AgentDelegation {
                parent_thread_id: first.thread_id.clone(),
                parent_turn_id: first.turn_id.clone(),
                parent_item_id: None,
            }),
            thread_id: Some(child_turn.thread_id.clone()),
            definition: None,
            nickname: None,
            task: Some("Inspect".into()),
        };
        let binding = forge_provider_api::BindingId::new("original-binding")?;
        let proof = crate::provider::test_support::observed_agent_fixture(
            binding.clone(),
            first.scope.clone(),
            vec![(
                public.clone(),
                description.clone(),
                Some(child_turn.clone()),
            )],
            public,
            Some(child_turn.turn_id.clone()),
        )?;
        let mut child = ObservedAgent::registered(&proof, 1)?;
        child.observe_agent(
            &proof,
            &crate::provider::NativeEventSource {
                binding_id: binding.clone(),
                sequence: forge_provider_api::EventSequence::new(1),
            },
            &AgentEvent::Lifecycle {
                event: AgentLifecycleEvent::Started {
                    agent: description.clone(),
                    state: AgentActiveState::Running,
                },
            },
            1,
        )?;
        child.observe_agent(
            &proof,
            &crate::provider::NativeEventSource {
                binding_id: binding,
                sequence: forge_provider_api::EventSequence::new(2),
            },
            &AgentEvent::Lifecycle {
                event: AgentLifecycleEvent::Finished {
                    agent: description,
                    outcome: AgentOutcome::Interrupted,
                },
            },
            4,
        )?;
        assert!(continuation_blocked(
            &parent,
            &[],
            &[child],
            &broker.store,
            &mut HashSet::new()
        )?);
        assert!(matches!(
            parent.turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
                }
            }
        ));
        Ok(())
    }
}
