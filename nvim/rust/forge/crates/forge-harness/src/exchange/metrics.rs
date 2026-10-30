use std::collections::{BTreeSet, HashMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::backend::usage::{TokenUsage, UsageUpdate};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
/// Owns exchange usage cursors and the union of observed non-model activity intervals.
pub struct ExchangeMetrics {
    /// Lower bound on active execution time observed before a runtime interruption.
    pub observed_elapsed_ms: u64,
    /// Completed model requests observed through distinct admitted usage reports.
    pub request_count: u64,
    /// Accumulated tool, approval, and delegated wait occupancy in active execution milliseconds.
    pub blocked_duration_ms: u64,
    /// Active elapsed coordinate at which overlapping blocked activity began.
    pub blocked_started_ms: Option<u64>,
    /// Whether all observed tool completions had a corresponding start.
    pub timing_complete: bool,
    /// Tool-only wall time, counting concurrent calls once.
    pub tool_duration_ms: u64,
    /// Active elapsed coordinate at which tool occupancy began.
    pub tool_started_ms: Option<u64>,
    /// Generated tokens captured at the latest admitted usage report.
    pub reported_output_tokens: Option<u64>,
    /// Response-time estimate captured with the latest admitted usage report.
    pub reported_response_ms: Option<u64>,
    /// Resume intent belongs to the live permission coordinator and cannot survive a host restart.
    #[serde(skip)]
    pub(crate) approval_resume: bool,
    tool: BTreeSet<String>,
    blocker: BTreeSet<String>,
    sample: HashSet<UsageSample>,
    cursor: HashMap<String, UsageCursor>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct UsageCursor {
    usage: TokenUsage,
    total: Option<u64>,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
struct UsageSample {
    scope: crate::backend::InvocationScope,
    thread_id: String,
    turn_id: Option<crate::backend::ProviderTurnId>,
    id: String,
}

impl ExchangeMetrics {
    /// Reports whether any permission request still suspends this exchange.
    pub(crate) fn awaiting_approval(&self) -> bool {
        self.blocker.iter().any(|id| id.starts_with("approval:"))
    }

    /// Reports a recorded parent wait for delegated execution.
    pub(crate) fn waiting_for_agent(&self) -> bool {
        self.blocker.contains("agent_wait")
    }

    /// Retains the latest active elapsed coordinate without counting offline time.
    pub(crate) fn observe_elapsed(&mut self, elapsed_ms: u64) {
        self.observed_elapsed_ms = self.observed_elapsed_ms.max(elapsed_ms);
    }

    /// Record one identified activity transition in the exchange's active elapsed coordinate.
    pub(crate) fn block(&mut self, id: String, running: bool, elapsed_ms: u64) {
        self.observe_elapsed(elapsed_ms);
        if id.starts_with("tool:") {
            if running {
                if self.tool.insert(id.clone()) && self.tool_started_ms.is_none() {
                    self.tool_started_ms = Some(elapsed_ms);
                }
            } else if self.tool.remove(&id) && self.tool.is_empty() {
                self.close_tools(elapsed_ms);
            }
        }
        if running {
            if self.blocker.insert(id) && self.blocked_started_ms.is_none() {
                self.blocked_started_ms = Some(elapsed_ms);
            }
        } else if self.blocker.remove(&id) && self.blocker.is_empty() {
            self.close(elapsed_ms);
        }
    }

    /// Cap outstanding activity at an interruption or finalization boundary.
    pub(crate) fn settle(&mut self, elapsed_ms: u64) {
        self.observe_elapsed(elapsed_ms);
        self.close(elapsed_ms);
        self.blocker.clear();
        self.close_tools(elapsed_ms);
        self.tool.clear();
        self.approval_resume = false;
    }

    /// Returns tool occupancy including currently running calls, when timing is complete.
    pub fn tool_ms(&self, elapsed_ms: u64) -> Option<u64> {
        self.timing_complete.then(|| {
            self.tool_duration_ms.saturating_add(
                self.tool_started_ms
                    .map_or(0, |started| elapsed_ms.saturating_sub(started)),
            )
        })
    }

    /// Captures both operands together so throughput stays fixed between usage reports.
    pub(crate) fn record_throughput(&mut self, output: Option<u64>, elapsed_ms: u64) {
        self.observe_elapsed(elapsed_ms);
        self.reported_output_tokens = output;
        self.reported_response_ms = self.response_ms(elapsed_ms);
    }

    fn close_tools(&mut self, elapsed_ms: u64) {
        if let Some(started) = self.tool_started_ms.take() {
            self.tool_duration_ms = self
                .tool_duration_ms
                .saturating_add(elapsed_ms.saturating_sub(started));
        }
    }

    /// Return active time outside observed tool, approval, and delegated wait intervals.
    pub fn response_ms(&self, elapsed_ms: u64) -> Option<u64> {
        if !self.timing_complete {
            return None;
        }
        let pending = self
            .blocked_started_ms
            .map_or(0, |started| elapsed_ms.saturating_sub(started));
        Some(elapsed_ms.saturating_sub(self.blocked_duration_ms.saturating_add(pending)))
    }

    /// Admit a new report once, excluding synthetic context adjustments and repeated snapshots.
    pub(crate) fn usage(
        &mut self,
        address: &crate::backend::TurnAddress,
        update: UsageUpdate,
    ) -> Option<TokenUsage> {
        let id = UsageSample {
            scope: address.scope.clone(),
            thread_id: address.thread_id.clone(),
            turn_id: update.cumulative.as_ref().map(|_| address.turn_id.clone()),
            id: update.id.clone(),
        };
        if !self.sample.insert(id) {
            return None;
        }
        let Some(cumulative) = update.cumulative else {
            self.request_count = self.request_count.saturating_add(1);
            return Some(update.usage);
        };
        let previous = self.cursor.insert(
            serde_json::to_string(&(&address.scope.owner, &address.thread_id))
                .expect("usage cursor identity contains serializable strings"),
            UsageCursor {
                usage: cumulative.clone(),
                total: update.cumulative_total,
            },
        );
        if previous.as_ref().is_some_and(|previous| {
            previous.usage == cumulative && previous.total == update.cumulative_total
        }) {
            return None;
        }
        // Codex context-window adjustments change totals without a billable model call.
        if update.usage.input == Some(0)
            && update.usage.output == Some(0)
            && update.cumulative_total.is_some_and(|total| total > 0)
        {
            return None;
        }
        self.request_count = self.request_count.saturating_add(1);
        previous
            .filter(|previous| {
                previous
                    .total
                    .zip(update.cumulative_total)
                    .is_none_or(|(previous, current)| current >= previous)
            })
            .and_then(|previous| cumulative.difference(&previous.usage))
            .or(Some(update.usage))
    }

    fn close(&mut self, elapsed_ms: u64) {
        if let Some(started) = self.blocked_started_ms.take() {
            self.blocked_duration_ms = self
                .blocked_duration_ms
                .saturating_add(elapsed_ms.saturating_sub(started));
        }
    }
}

impl Default for ExchangeMetrics {
    fn default() -> Self {
        Self {
            observed_elapsed_ms: 0,
            request_count: 0,
            blocked_duration_ms: 0,
            blocked_started_ms: None,
            timing_complete: true,
            tool_duration_ms: 0,
            tool_started_ms: None,
            reported_output_tokens: None,
            reported_response_ms: None,
            approval_resume: false,
            tool: BTreeSet::new(),
            blocker: BTreeSet::new(),
            sample: HashSet::new(),
            cursor: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn address(thread: &str, turn: &str) -> crate::backend::TurnAddress {
        crate::backend::TurnAddress {
            scope: crate::backend::InvocationScope {
                owner: crate::backend::ExecutionOwner::root("session"),
                exchange_id: "exchange".into(),
                invocation_id: "invocation".into(),
            },
            thread_id: thread.into(),
            turn_id: turn.into(),
        }
    }

    fn report(id: &str, input: u64, total: Option<u64>) -> UsageUpdate {
        let usage = TokenUsage {
            input: Some(input),
            cached_input: Some(input / 2),
            reasoning: Some(10),
            output: Some(30),
        };
        UsageUpdate {
            id: id.into(),
            usage: usage.clone(),
            cumulative: total.map(|input| TokenUsage {
                input: Some(input),
                ..usage
            }),
            cumulative_total: total.map(|input| input + 30),
        }
    }

    #[test]
    fn usage_reused_provider_ids_remain_distinct_across_invocations() {
        let mut metrics = ExchangeMetrics::default();
        let first = address("thread", "turn");
        let mut second = first.clone();
        second.scope.invocation_id = "next-invocation".into();
        assert!(metrics.usage(&first, report("report", 10, None)).is_some());
        assert!(metrics.usage(&second, report("report", 10, None)).is_some());
        assert!(metrics.usage(&first, report("report", 10, None)).is_none());
        let mut restored: ExchangeMetrics =
            serde_json::from_value(serde_json::to_value(metrics).unwrap()).unwrap();
        assert!(
            restored
                .usage(&second, report("report", 10, None))
                .is_none()
        );
        assert_eq!(restored.request_count, 2);
    }

    #[test]
    fn usage_deduplicates_snapshots_recovers_increments_and_handles_resets() {
        let mut metrics = ExchangeMetrics::default();
        assert_eq!(
            metrics
                .usage(&address("parent", "one"), report("first", 100, Some(1000)))
                .unwrap()
                .input,
            Some(100)
        );
        assert!(
            metrics
                .usage(&address("parent", "one"), report("first", 100, Some(1000)))
                .is_none()
        );
        assert!(
            metrics
                .usage(
                    &address("parent", "one"),
                    report("refresh", 100, Some(1000))
                )
                .is_none()
        );
        assert_eq!(
            metrics
                .usage(&address("parent", "one"), report("next", 100, Some(1300)))
                .unwrap()
                .input,
            Some(300)
        );
        assert_eq!(
            metrics
                .usage(&address("parent", "two"), report("reset", 40, Some(40)))
                .unwrap()
                .input,
            Some(40)
        );
        assert_eq!(
            metrics
                .usage(&address("child", "one"), report("first", 200, Some(1000)))
                .unwrap()
                .input,
            Some(200)
        );
        let replay: ExchangeMetrics =
            serde_json::from_value(serde_json::to_value(&metrics).unwrap()).unwrap();
        metrics = replay;
        assert_eq!(metrics.request_count, 4);
        assert!(
            metrics
                .usage(&address("parent", "two"), report("reset", 40, Some(40)))
                .is_none()
        );
        assert_eq!(metrics.request_count, 4);
    }

    #[test]
    fn usage_excludes_synthetic_context_adjustment_and_preserves_missing_counts() {
        let mut metrics = ExchangeMetrics::default();
        metrics
            .usage(&address("parent", "one"), report("first", 100, Some(1000)))
            .unwrap();
        let mut synthetic = report("compact", 0, Some(500));
        synthetic.usage.output = Some(0);
        assert!(
            metrics
                .usage(&address("parent", "one"), synthetic)
                .is_none()
        );
        assert_eq!(
            metrics
                .usage(&address("parent", "one"), report("next", 100, Some(600)))
                .unwrap()
                .input,
            Some(100)
        );
        let mut partial = report("copilot", 100, None);
        partial.usage.reasoning = None;
        let replay = partial.clone();
        let usage = metrics.usage(&address("parent", "two"), partial).unwrap();
        assert_eq!(usage.input, Some(100));
        assert_eq!(usage.non_reasoning_output(), None);
        assert!(metrics.usage(&address("parent", "three"), replay).is_none());
        assert_eq!(metrics.request_count, 3);
    }

    #[test]
    fn blocked_intervals_form_a_union_and_survive_persistence() {
        let mut metrics = ExchangeMetrics::default();
        metrics.block("tool:one".into(), true, 1000);
        metrics.block("tool:two".into(), true, 2000);
        metrics.block("approval".into(), true, 2500);
        metrics.block("tool:one".into(), false, 3000);
        assert_eq!(metrics.response_ms(3500), Some(1000));
        metrics = serde_json::from_value(serde_json::to_value(metrics).unwrap()).unwrap();
        metrics.block("approval".into(), false, 4000);
        metrics.block("tool:two".into(), false, 5000);
        metrics.block("tool:two".into(), false, 6000);
        assert_eq!(metrics.blocked_duration_ms, 4000);
        assert_eq!(metrics.response_ms(6000), Some(2000));
        metrics.block("pending".into(), true, 6000);
        metrics.settle(7000);
        assert_eq!(metrics.response_ms(7000), Some(2000));
        metrics.timing_complete = false;
        assert_eq!(metrics.response_ms(7000), None);
    }

    #[test]
    fn throughput_preserves_subsecond_tool_time() {
        let mut metrics = ExchangeMetrics::default();
        metrics.block("tool:first".into(), true, 1000);
        metrics.block("tool:first".into(), false, 1439);
        metrics.record_throughput(Some(4200), 6000);
        assert_eq!(metrics.tool_ms(6000), Some(439));
        assert_eq!(metrics.reported_output_tokens, Some(4200));
        assert_eq!(metrics.reported_response_ms, Some(5561));
    }

    #[test]
    fn tool_occupancy_excludes_other_waits_and_throughput_freezes_between_reports() {
        let mut metrics = ExchangeMetrics::default();
        metrics.block("tool:first".into(), true, 1000);
        metrics.block("approval".into(), true, 1500);
        metrics.block("tool:second".into(), true, 2000);
        metrics.block("tool:first".into(), false, 3000);
        assert_eq!(metrics.tool_ms(3500), Some(2500));
        metrics.block("tool:second".into(), false, 4000);
        metrics.block("approval".into(), false, 5000);
        assert_eq!(metrics.tool_ms(6000), Some(3000));
        metrics.record_throughput(Some(100), 6000);
        metrics = serde_json::from_value(serde_json::to_value(metrics).unwrap()).unwrap();
        assert_eq!(metrics.reported_response_ms, Some(2000));
        assert_eq!(metrics.response_ms(9000), Some(5000));
        assert_eq!(metrics.reported_response_ms, Some(2000));
        metrics.block("tool:interrupted".into(), true, 9000);
        metrics.settle(10000);
        assert_eq!(metrics.tool_ms(12000), Some(4000));
        metrics.record_throughput(Some(200), 10000);
        assert_eq!(metrics.reported_response_ms, Some(5000));
        metrics.timing_complete = false;
        assert_eq!(metrics.tool_ms(12000), None);
        metrics.record_throughput(Some(200), 12000);
        assert_eq!(metrics.reported_response_ms, None);
    }
}
