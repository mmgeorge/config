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
                .filter(|node| !node.is_derived())
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
    /// When the event happened, from its durable record. The source boundary cannot tell a
    /// result the turn produced from a decision recorded after the turn ended, because both
    /// follow the turn's last node; the time can.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub occurred_at_ms: Option<i64>,
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
    let mut position = 0;
    for node in source {
        while events
            .peek()
            .is_some_and(|event| event.node_count <= position)
        {
            ordered.push(ExchangeNode::PlanEvent {
                event: Box::new(events.next().unwrap()),
            });
        }
        position += usize::from(!node.is_derived());
        ordered.push(node);
    }
    ordered.extend(events.map(|event| ExchangeNode::PlanEvent {
        event: Box::new(event),
    }));
    exchange.node_list = ordered;
}

/// Preserve derived events when live provider updates replace the exchange's source state.
///
/// The replacement is the in-memory record, which holds neither the planning events nor the
/// question rows a snapshot projected, so both are carried over from the presented exchange:
/// each projected question after the stored node it followed, and each planning event at its
/// anchor. The live transcript then keeps the rows and the order the next snapshot draws.
pub(crate) fn replacement_exchange(
    current: &Exchange,
    replacement: &Exchange,
) -> anyhow::Result<Exchange> {
    let events = current
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::PlanEvent { event } => Some((**event).clone()),
            _ => None,
        })
        .collect();
    let mut replacement = replacement.clone();
    carry_projected_questions(current, &mut replacement);
    insert_events(&mut replacement, events);
    for run in current.checks() {
        replacement.record_check(run.clone())?;
    }
    for node in &current.node_list {
        if let ExchangeNode::TimelineEvent { event } = node {
            crate::timeline::ownership::insert_event(&mut replacement, (**event).clone())?;
        }
    }
    Ok(replacement)
}

/// Place each question row `current` projected after the stored node it followed there.
///
/// A row whose preceding stored node the replacement lacks goes last, where a snapshot puts a
/// question whose asking call it cannot find.
fn carry_projected_questions(current: &Exchange, replacement: &mut Exchange) {
    let mut projected: Vec<(Option<&str>, &ExchangeNode)> = Vec::new();
    let mut preceding = None;
    for node in &current.node_list {
        if node.is_projected_question() {
            if replacement
                .node_list
                .iter()
                .all(|stored| stored.id() != node.id())
            {
                projected.push((preceding, node));
            }
        } else if !node.is_derived() {
            preceding = Some(node.id());
        }
    }
    if projected.is_empty() {
        return;
    }
    let source = std::mem::take(&mut replacement.node_list);
    let mut placed = Vec::with_capacity(source.len() + projected.len());
    let take_after = |anchor: Option<&str>, projected: &mut Vec<(Option<&str>, &ExchangeNode)>| {
        let (after, rest): (Vec<_>, Vec<_>) = std::mem::take(projected)
            .into_iter()
            .partition(|(preceding, _)| *preceding == anchor);
        *projected = rest;
        after
            .into_iter()
            .map(|(_, node)| node.clone())
            .collect::<Vec<_>>()
    };
    placed.extend(take_after(None, &mut projected));
    for node in source {
        let id = node.id().to_owned();
        placed.push(node);
        placed.extend(take_after(Some(&id), &mut projected));
    }
    placed.extend(projected.into_iter().map(|(_, node)| node.clone()));
    replacement.node_list = placed;
}

#[cfg(test)]
mod tests {
    use super::{ExchangePlanEvent, PlanEventContent, insert_events, replacement_exchange};
    use crate::exchange::{Exchange, ExchangeNode};
    use crate::plan::PlanExecutionLifecycleEvent;

    fn source_node(id: &str) -> ExchangeNode {
        serde_json::from_value(serde_json::json!({
            "kind":"turn_content","id":id,"turn_id":"turn","item":{"kind":"tool","id":id}
        }))
        .expect("fixture node is valid")
    }

    fn node_ids(exchange: &Exchange) -> Vec<&str> {
        exchange.node_list.iter().map(ExchangeNode::id).collect()
    }

    /// A snapshot projects an answered question after the call that asked it, and a live update
    /// rebuilds the exchange from its in-memory record, which holds no projected question. Both
    /// must place a plan event at the source position its anchor counted, and the live update
    /// keeps the projected question, so the transcript keeps one order between the two paths.
    #[test]
    fn projected_questions_keep_plan_events_at_their_anchor() {
        let mut source = crate::storage::test::running_exchange("exchange");
        source.node_list = vec![
            source_node("ask"),
            source_node("artifact"),
            source_node("response"),
        ];
        let event = ExchangePlanEvent {
            id: "submitted".into(),
            node_count: 2,
            occurred_at_ms: None,
            content: PlanEventContent::Execution {
                event: PlanExecutionLifecycleEvent::Completed {
                    title: "Plan".into(),
                },
            },
        };
        let mut snapshot = source.clone();
        snapshot.node_list.insert(
            1,
            ExchangeNode::projected_question(
                "asked",
                serde_json::from_value(serde_json::json!({"id":"asked","questions":[]}))
                    .expect("fixture question is valid"),
                None,
            ),
        );
        insert_events(&mut snapshot, vec![event]);
        assert_eq!(
            node_ids(&snapshot),
            ["ask", "question:asked", "artifact", "submitted", "response"]
        );
        let mut live = source;
        live.node_list.push(source_node("next"));
        let replaced = replacement_exchange(&snapshot, &live).expect("replacement succeeds");
        assert_eq!(
            node_ids(&replaced),
            [
                "ask",
                "question:asked",
                "artifact",
                "submitted",
                "response",
                "next"
            ]
        );
    }
}
