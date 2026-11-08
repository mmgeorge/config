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
