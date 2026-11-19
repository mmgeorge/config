//! Running child agents projected for the status area.
//!
//! Live children belong to status rather than transcript history: the transcript records only
//! each delegation and, once a child finishes, its summary. The status therefore carries one
//! snapshot per unfinished child exchange, with that exchange's own clock and tool accounting so
//! the elapsed and tool-time labels keep advancing between timeline updates.

use std::collections::HashSet;

use serde::Serialize;

use super::TimelineEntry;
use crate::exchange::{Exchange, ExchangeClock, ExchangeMetrics, ExchangeState};

/// One unfinished child exchange as the status area renders it.
#[derive(Clone, Debug, Serialize)]
pub struct StatusAgent {
    /// Child exchange identity, unique across the session's delegation tree.
    pub exchange_id: String,
    /// Nesting below the root session: zero for a root's direct child.
    pub depth: usize,
    /// Child task name.
    pub label: String,
    /// Resolved provider, model, and speed suffix, empty when no selection is known.
    pub selection_label: String,
    /// Whether the child exchange is still waiting to start.
    pub starting: bool,
    /// The child exchange's active execution clock.
    pub(crate) clock: ExchangeClock,
    /// The child exchange's tool interval accounting.
    pub(crate) metrics: ExchangeMetrics,
    /// Successfully completed tool calls in the child exchange.
    pub tool_completed: usize,
    /// Every tool call in the child exchange, including running and failed calls.
    pub tool_total: usize,
    /// Newest readable reasoning summary of the child's running turn, when shown.
    pub reasoning_summary: Option<String>,
}

impl StatusAgent {
    /// Return the child's active execution time at `now_ms`.
    pub fn elapsed_ms(&self, now_ms: i64) -> u64 {
        if self.starting {
            0
        } else {
            self.clock.elapsed(now_ms)
        }
    }

    /// Return the union of the child's tool execution intervals at `now_ms`.
    pub fn tool_ms(&self, now_ms: i64) -> Option<u64> {
        self.metrics.tool_ms(self.elapsed_ms(now_ms))
    }
}

/// Collect every unfinished managed child exchange in delegation order.
///
/// A reusable child can appear beneath several parent exchanges, so each child exchange is
/// listed once. Siblings are ordered by creation so the rows stay stable across updates.
pub(crate) fn running_agents(
    entry_list: &[TimelineEntry],
    status_thoughts: bool,
) -> Vec<StatusAgent> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    collect(
        entry_list.iter(),
        0,
        status_thoughts,
        &mut seen,
        &mut result,
    );
    result
}

fn collect<'source>(
    entry_list: impl IntoIterator<Item = &'source TimelineEntry>,
    depth: usize,
    status_thoughts: bool,
    seen: &mut HashSet<&'source str>,
    result: &mut Vec<StatusAgent>,
) {
    for entry in entry_list {
        match entry {
            TimelineEntry::Exchange { agent_by_id, .. } => {
                collect(
                    in_creation_order(agent_by_id.values()),
                    depth,
                    status_thoughts,
                    seen,
                    result,
                );
            }
            TimelineEntry::AgentLifecycle {
                run,
                exchange,
                agent,
                ..
            } => {
                for child in exchange.iter().filter(|exchange| !exchange.is_finished()) {
                    if !seen.insert(child.id.as_str()) {
                        continue;
                    }
                    result.push(snapshot(run, child, depth, status_thoughts));
                }
                collect(
                    in_creation_order(agent),
                    depth + 1,
                    status_thoughts,
                    seen,
                    result,
                );
            }
            _ => {}
        }
    }
}

/// Order sibling lifecycle entries, which a parent keys by agent identity, by creation.
fn in_creation_order<'source>(
    entry_list: impl IntoIterator<Item = &'source TimelineEntry>,
) -> Vec<&'source TimelineEntry> {
    let mut ordered = entry_list.into_iter().collect::<Vec<_>>();
    ordered.sort_by_cached_key(|entry| {
        let created_at_ms = match entry {
            TimelineEntry::AgentLifecycle { created_at_ms, .. } => *created_at_ms,
            _ => 0,
        };
        (created_at_ms, entry.id())
    });
    ordered
}

fn snapshot(
    run: &crate::agent::Agent,
    exchange: &Exchange,
    depth: usize,
    status_thoughts: bool,
) -> StatusAgent {
    StatusAgent {
        exchange_id: exchange.id.clone(),
        depth,
        label: run.label().to_owned(),
        selection_label: crate::buffer::projection::agent_selection_label(run),
        starting: exchange.state() == ExchangeState::Queued,
        clock: exchange.clock.clone(),
        metrics: exchange.metrics.clone(),
        tool_completed: exchange.tool_counter.completed,
        tool_total: exchange.tool_counter.total,
        reasoning_summary: status_thoughts
            .then(|| exchange.latest_reasoning_summary().map(str::to_owned))
            .flatten(),
    }
}
