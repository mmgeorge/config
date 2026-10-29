use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::exchange::{Exchange, ExchangeNode};

/// Records the destination selected by a producer before work starts.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TimelineOwner {
    Session {
        session_id: String,
    },
    Exchange {
        session_id: String,
        exchange_id: String,
        node_count: usize,
    },
}

impl TimelineOwner {
    pub fn session(session_id: &str) -> Self {
        Self::Session {
            session_id: session_id.into(),
        }
    }

    pub fn exchange(exchange: &Exchange) -> Self {
        Self::Exchange {
            session_id: exchange.session_id.clone(),
            exchange_id: exchange.id.clone(),
            node_count: exchange
                .node_list
                .iter()
                .filter(|node| source_node(node))
                .count(),
        }
    }

    pub fn session_id(&self) -> &str {
        match self {
            Self::Session { session_id } | Self::Exchange { session_id, .. } => session_id,
        }
    }

    pub fn exchange_id(&self) -> Option<&str> {
        match self {
            Self::Session { .. } => None,
            Self::Exchange { exchange_id, .. } => Some(exchange_id),
        }
    }

    pub(crate) fn validate_exchange(&self, exchange: &Exchange) -> Result<()> {
        let Self::Exchange {
            session_id,
            exchange_id,
            node_count,
        } = self
        else {
            anyhow::bail!("a session event cannot be attached to an exchange");
        };
        ensure!(
            session_id == &exchange.session_id && exchange_id == &exchange.id,
            "timeline event has a different exchange owner"
        );
        ensure!(
            *node_count
                <= exchange
                    .node_list
                    .iter()
                    .filter(|node| source_node(node))
                    .count(),
            "timeline event references an unavailable source position"
        );
        Ok(())
    }
}

/// Carries owned side effects in the same ordered sequence as provider content.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineEvent {
    Check {
        run: Box<crate::checks::CheckRun>,
    },
    Session {
        event: Box<super::SessionEventRecord>,
    },
}

impl TimelineEvent {
    pub fn id(&self) -> &str {
        match self {
            Self::Check { run } => &run.id,
            Self::Session { event } => &event.id,
        }
    }

    pub fn owner(&self) -> &TimelineOwner {
        match self {
            Self::Check { run } => &run.owner,
            Self::Session { event } => &event.owner,
        }
    }

    fn created_at_ms(&self) -> i64 {
        match self {
            Self::Check { run } => run.started_at_ms,
            Self::Session { event } => event.created_at_ms,
        }
    }
}

pub(crate) fn source_node(node: &ExchangeNode) -> bool {
    !matches!(
        node,
        ExchangeNode::PlanEvent { .. } | ExchangeNode::TimelineEvent { .. }
    )
}

struct AnchoredEvent {
    event: TimelineEvent,
    position: usize,
}

impl AnchoredEvent {
    fn new(exchange: &Exchange, event: TimelineEvent) -> Result<Self> {
        event.owner().validate_exchange(exchange)?;
        let position = match event.owner() {
            TimelineOwner::Exchange { node_count, .. } => *node_count,
            TimelineOwner::Session { .. } => {
                anyhow::bail!("a session event cannot be attached to an exchange")
            }
        };
        Ok(Self { event, position })
    }
}

/// Rebuilds event positions from immutable source anchors without moving provider nodes.
pub(crate) fn insert_event(exchange: &mut Exchange, event: TimelineEvent) -> Result<()> {
    if let TimelineEvent::Check { run } = event {
        return exchange.record_check(*run);
    }
    let event = AnchoredEvent::new(exchange, event)?;
    let mut events: Vec<_> = exchange
        .node_list
        .iter()
        .filter_map(|node| {
            if let ExchangeNode::TimelineEvent { event } = node {
                Some(AnchoredEvent::new(exchange, (**event).clone()))
            } else {
                None
            }
        })
        .collect::<Result<_>>()?;
    if let Some(existing) = events
        .iter_mut()
        .find(|existing| existing.event.id() == event.event.id())
    {
        ensure!(
            existing.event.owner() == event.event.owner(),
            "timeline event ownership cannot change"
        );
        *existing = event;
    } else {
        events.push(event);
    }
    exchange
        .node_list
        .retain(|node| !matches!(node, ExchangeNode::TimelineEvent { .. }));
    events.sort_by(|left, right| {
        left.position
            .cmp(&right.position)
            .then_with(|| left.event.created_at_ms().cmp(&right.event.created_at_ms()))
            .then_with(|| left.event.id().cmp(right.event.id()))
    });
    let mut events = events.into_iter().peekable();
    let mut result = Vec::with_capacity(exchange.node_list.len() + events.len());
    let mut position = 0;
    for node in std::mem::take(&mut exchange.node_list) {
        while let Some(event) = events.next_if(|event| event.position <= position) {
            result.push(ExchangeNode::TimelineEvent {
                event: Box::new(event.event),
            });
        }
        position += usize::from(source_node(&node));
        result.push(node);
    }
    result.extend(events.map(|event| ExchangeNode::TimelineEvent {
        event: Box::new(event.event),
    }));
    exchange.node_list = result;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::timeline::{SessionEventKind, SessionEventRecord, TimelineEntry, TimelineProjector};

    fn exchange(id: &str) -> Exchange {
        serde_json::from_value(serde_json::json!({
            "id":id,"session_id":"session","agent_id":"primary","ordinal":1,
            "prompt":"request","state":"running","created_at_ms":0,
            "attributed_matches_checkpoint":true,"node_list":[]
        }))
        .unwrap()
    }

    fn event(owner: TimelineOwner, id: &str) -> TimelineEvent {
        TimelineEvent::Session {
            event: Box::new(SessionEventRecord {
                id: id.into(),
                session_id: owner.session_id().into(),
                owner,
                created_at_ms: 1,
                detail: SessionEventKind::Configuration {
                    operation_id: id.into(),
                    state: "failed".into(),
                    field: vec!["effort".into()],
                    reason: Some("unsupported effort".into()),
                },
            }),
        }
    }

    fn entry(exchange: Exchange) -> TimelineEntry {
        TimelineEntry::Exchange {
            id: exchange.id.clone(),

            created_at_ms: exchange.created_at_ms,
            exchange: exchange.into(),

            agent_by_id: Default::default(),
        }
    }

    fn run(owner: TimelineOwner) -> crate::checks::CheckRun {
        serde_json::from_value(serde_json::json!({
            "id":"checks","owner":owner,"revision":0,"completed_plan":null,
            "shell":"system","executable":null,"workspace":".","environment_digest":"test",
            "started_at_ms":1,"completed_at_ms":2,"state":"passed",
            "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":[]
        }))
        .unwrap()
    }

    #[test]
    fn captured_event_position_survives_append_replay_and_owner_rejection() -> Result<()> {
        let mut source = exchange("work");
        source.append_input(crate::exchange::InputIntent::Steering, "before".into(), 0)?;
        let owner = TimelineOwner::exchange(&source);
        let source_id = source.node_list[0].id().to_owned();
        insert_event(&mut source, event(owner.clone(), "event"))?;
        source.append_input(crate::exchange::InputIntent::Steering, "after".into(), 2)?;
        insert_event(&mut source, event(owner.clone(), "event"))?;
        assert_eq!(source.node_list[0].id(), source_id);
        assert_eq!(source.node_list[1].id(), "event");
        assert_eq!(source.node_list.len(), 3);
        let encoded = serde_json::to_value(&source)?;
        let mut wrong = owner.clone();
        if let TimelineOwner::Exchange { node_count, .. } = &mut wrong {
            *node_count = 0;
        }
        assert!(insert_event(&mut source, event(wrong, "event")).is_err());
        assert_eq!(serde_json::to_value(&source)?, encoded);
        let mut restored: Exchange = serde_json::from_value(encoded)?;
        insert_event(&mut restored, event(owner, "event"))?;
        assert_eq!(restored.node_list[1].id(), "event");
        assert_eq!(restored.node_list.len(), 3);
        Ok(())
    }

    #[test]
    fn nested_checks_attach_once_and_missing_owners_fail() -> Result<()> {
        let child = exchange("child");
        let run = run(TimelineOwner::exchange(&child));
        let mut parent = entry(exchange("parent"));
        if let TimelineEntry::Exchange { agent_by_id, .. } = &mut parent {
            agent_by_id.insert("child".into(), entry(child));
        }
        let timeline =
            TimelineProjector::with_checks(vec![parent.clone()], vec![run.clone(), run.clone()])?;
        let TimelineEntry::Exchange { agent_by_id, .. } = &timeline[0] else {
            unreachable!()
        };
        let TimelineEntry::Exchange { exchange, .. } = &agent_by_id["child"] else {
            unreachable!()
        };
        assert!(exchange.node_list.is_empty());
        assert_eq!(exchange.checks().count(), 1);
        assert_eq!(exchange.checks().next().unwrap().id, "checks");
        assert!(TimelineProjector::with_checks(Vec::new(), vec![run.clone()]).is_err());
        let mut stream = super::super::stream::TimelineStream::new("session".into());
        stream.initialize(vec![parent])?;
        stream.update_live(
            None,
            Some("parent"),
            crate::session::state_machine::SessionPhase::Idle,
            true,
        )?;
        stream.update_checks(&run)?;
        assert!(
            stream
                .entry_list()
                .iter()
                .all(|entry| matches!(entry, TimelineEntry::Status { .. }))
        );
        Ok(())
    }

    #[test]
    fn check_admission_retains_commentary_owner_through_late_completion_and_replay() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress};
        use crate::exchange::ExchangeEntry;
        let mut source = exchange("work");
        source.resume(0)?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        source.start_turn(address.clone(), 0)?;
        let commentary = |id: &str| BackendEvent {
            message: Some(crate::turn::test_support::message(
                id,
                Some(crate::backend::MessageRole::Commentary),
                id,
            )),
            received_at_ms: None,
            address: Some(address.clone()),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some(id.into()),
            data: serde_json::json!({"provider_message_id":id,"phase":"commentary"}),
            activity: None,
            summary: None,
            checklist_update: None,
        };
        source.observe_turn(&commentary("first"), 1)?;
        let admitted = run(TimelineOwner::exchange(&source));
        source.observe_turn(&commentary("second"), 2)?;
        insert_event(
            &mut source,
            TimelineEvent::Check {
                run: Box::new(admitted.clone()),
            },
        )?;
        insert_event(
            &mut source,
            TimelineEvent::Check {
                run: Box::new(admitted.clone()),
            },
        )?;
        let mut next = run(TimelineOwner::exchange(&source));
        next.id = "next".into();
        insert_event(
            &mut source,
            TimelineEvent::Check {
                run: Box::new(next),
            },
        )?;
        let mut restored: Exchange = serde_json::from_value(serde_json::to_value(&source)?)?;
        insert_event(
            &mut restored,
            TimelineEvent::Check {
                run: Box::new(admitted.clone()),
            },
        )?;
        let counts: Vec<_> = restored
            .entry
            .iter()
            .filter_map(|entry| match entry {
                ExchangeEntry::Commentary { commentary } => Some(commentary.check.len()),
                _ => None,
            })
            .collect();
        assert_eq!(counts, vec![1, 1]);
        assert_eq!(restored.checks().count(), 2);
        assert!(
            restored
                .node_list
                .iter()
                .all(|node| !matches!(node, ExchangeNode::TimelineEvent { .. }))
        );
        let before = serde_json::to_value(&restored)?;
        let mut reassigned = admitted;
        reassigned.owner = Box::new(TimelineOwner::exchange(&restored));
        assert!(
            insert_event(
                &mut restored,
                TimelineEvent::Check {
                    run: Box::new(reassigned)
                }
            )
            .is_err()
        );
        assert_eq!(serde_json::to_value(&restored)?, before);
        Ok(())
    }

    #[test]
    fn retired_exchange_history_is_excluded_explicitly() -> Result<()> {
        let mut removed = exchange("removed");
        let run = run(TimelineOwner::exchange(&removed));
        removed.finish(crate::exchange::ExchangeOutcome::Interrupted, 2)?;
        removed
            .set_disposition(crate::exchange::HistoryDisposition::RolledBack)
            .unwrap();
        assert!(TimelineProjector::with_history(Vec::new(), vec![run], &[removed])?.is_empty());
        Ok(())
    }
}
