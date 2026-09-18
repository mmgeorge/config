use serde::{Deserialize, Serialize};

use super::{
    PlanAudit, PlanDeviation, PlanExecutionLifecycleEvent, PlanLifecycleRecord,
    PlanResolutionRecord,
};
use crate::exchange::{Exchange, ExchangeNode};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Identifies a boundary in an exchange's append-only source node sequence.
pub struct ExchangeAnchor {
    /// Durable exchange identity created by admission.
    pub exchange_id: String,
    /// Number of source nodes preceding the event, excluding derived plan content.
    pub node_count: usize,
}

impl ExchangeAnchor {
    /// Capture the next insertion position without counting derived planning nodes.
    pub fn capture(exchange: &Exchange) -> Self {
        Self {
            exchange_id: exchange.id.clone(),
            node_count: exchange
                .node_list
                .iter()
                .filter(|node| !matches!(node, ExchangeNode::PlanEvent { .. }))
                .count(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Supplies planning content at an exchange-owned timeline position.
pub enum PlanEventContent {
    /// Plan review or question audit content with its recorded title.
    Lifecycle {
        title: String,
        lifecycle: PlanLifecycleRecord,
    },
    /// Scheduler transition or deviation emitted by execution.
    Execution { event: PlanExecutionLifecycleEvent },
    /// Terminal execution evidence and its audit references.
    Resolution {
        resolution: PlanResolutionRecord,
        deviation: Vec<PlanDeviation>,
        audit: Option<PlanAudit>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a stable event identity while the owning exchange streams new content.
pub struct ExchangePlanEvent {
    /// Stable audit identity reused for folds and live replacement.
    pub id: String,
    /// Source boundary captured when the event was recorded.
    pub node_count: usize,
    /// Resolved content for the exchange's renderer.
    pub content: PlanEventContent,
}

/// Merge derived planning content without moving it when provider nodes are appended.
pub(crate) fn insert_events(exchange: &mut Exchange, events: Vec<ExchangePlanEvent>) {
    exchange
        .node_list
        .retain(|node| !matches!(node, ExchangeNode::PlanEvent { .. }));
    let mut events = events;
    events.sort_by_key(|event| event.node_count);
    let source = std::mem::take(&mut exchange.node_list);
    let source_count = source.len();
    let mut ordered = Vec::with_capacity(source_count + events.len());
    let mut events = events.into_iter().peekable();
    for (position, node) in source.into_iter().enumerate() {
        while events
            .peek()
            .is_some_and(|event| event.node_count <= position)
        {
            ordered.push(ExchangeNode::PlanEvent {
                event: Box::new(events.next().unwrap()),
            });
        }
        ordered.push(node);
    }
    ordered.extend(events.map(|event| ExchangeNode::PlanEvent {
        event: Box::new(event),
    }));
    exchange.node_list = ordered;
}

/// Preserve derived events when live provider updates replace the exchange's source state.
pub(crate) fn replace_exchange(current: &mut Exchange, replacement: &Exchange) {
    let events = current
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::PlanEvent { event } => Some((**event).clone()),
            _ => None,
        })
        .collect();
    *current = replacement.clone();
    insert_events(current, events);
}
