use crate::provider::ScopeOwner;
use std::collections::{BTreeSet, HashMap};

use serde::{Deserialize, Serialize};

use crate::provider::usage::TokenUsage;
use forge_provider_api::{TurnAddress, TurnResult, TurnUsageReport, UsageReportError};

/// Retain canonical accounting and the union of observed non-model activity intervals.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "StoredExchangeMetrics")]
pub struct ExchangeMetrics {
    /// Lower bound on active execution time observed before a runtime interruption.
    pub observed_elapsed_ms: u64,
    /// Identified model calls observed through canonical accounting reports.
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
    /// Canonical generated token expenditure after report and model-call deduplication, over the
    /// turns after the newest turn whose output count is unknown.
    pub reported_output_tokens: Option<u64>,
    /// Native measured duration of the same turns, unavailable when one turn's timing is unknown.
    pub reported_response_ms: Option<u64>,
    /// Whether the clock resumes when the last user wait ends. Resume intent belongs to the live
    /// owner and cannot survive a host restart.
    #[serde(skip)]
    pub(crate) user_wait_resume: bool,
    tool: BTreeSet<String>,
    blocker: BTreeSet<String>,
    reports: Vec<UsageSample>,
    completed_timing: Vec<TurnDuration>,
    /// Cumulative totals the provider threads had reported before this exchange began.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    usage_baseline: Vec<ThreadUsageBaseline>,
}

/// Validate the retained accounting ledger before restoring persisted metrics.
#[derive(Deserialize)]
#[serde(default)]
struct StoredExchangeMetrics {
    observed_elapsed_ms: u64,
    request_count: u64,
    blocked_duration_ms: u64,
    blocked_started_ms: Option<u64>,
    timing_complete: bool,
    tool_duration_ms: u64,
    tool_started_ms: Option<u64>,
    reported_output_tokens: Option<u64>,
    reported_response_ms: Option<u64>,
    tool: BTreeSet<String>,
    blocker: BTreeSet<String>,
    reports: Vec<UsageSample>,
    completed_timing: Vec<TurnDuration>,
    usage_baseline: Vec<ThreadUsageBaseline>,
}

impl Default for StoredExchangeMetrics {
    /// Create empty persisted metrics with complete timing until native evidence says otherwise.
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
            tool: Default::default(),
            blocker: Default::default(),
            reports: Vec::new(),
            completed_timing: Vec::new(),
            usage_baseline: Vec::new(),
        }
    }
}

impl TryFrom<StoredExchangeMetrics> for ExchangeMetrics {
    type Error = UsageReportError;

    /// Revalidate report ownership and complementary evidence before exposing restored aggregates.
    fn try_from(stored: StoredExchangeMetrics) -> Result<Self, Self::Error> {
        let StoredExchangeMetrics {
            observed_elapsed_ms,
            request_count: _,
            blocked_duration_ms,
            blocked_started_ms,
            timing_complete,
            tool_duration_ms,
            tool_started_ms,
            reported_output_tokens: _,
            reported_response_ms: _,
            tool,
            blocker,
            reports,
            completed_timing,
            usage_baseline,
        } = stored;
        let mut metrics = Self {
            observed_elapsed_ms,
            request_count: 0,
            blocked_duration_ms,
            blocked_started_ms,
            timing_complete,
            tool_duration_ms,
            tool_started_ms,
            reported_output_tokens: None,
            reported_response_ms: None,
            user_wait_resume: false,
            tool,
            blocker,
            reports: Vec::new(),
            completed_timing: Vec::new(),
            usage_baseline,
        };
        for sample in reports {
            metrics.usage_report(&sample.address, &sample.report)?;
        }
        for timing in completed_timing {
            metrics.validate_address(&timing.address)?;
            if let Some(existing) = metrics
                .completed_timing
                .iter()
                .find(|entry| entry.address == timing.address)
            {
                if existing.reported_duration_ms != timing.reported_duration_ms {
                    return Err(UsageReportError::MeasurementConflict);
                }
            } else {
                metrics.completed_timing.push(timing);
            }
        }
        metrics.refresh_accounting()?;
        Ok(metrics)
    }
}

/// Retain one original accounting identity together with its complete turn ownership.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct UsageSample {
    address: TurnAddress,
    report: TurnUsageReport,
}

/// Retain the latest cumulative token total one provider thread reported.
///
/// A provider that reports cumulative thread totals can repeat its latest total into a later
/// turn; Codex does so when a turn opens on a resumed thread. The exchange that first received
/// a total owns its tokens, so the next exchange inherits the total as a baseline and charges
/// nothing for a report that only repeats it. The baseline is durable because the repeat also
/// follows a host restart, when no process remembers the earlier total.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct ThreadUsageBaseline {
    thread_id: forge_provider_api::ThreadId,
    cumulative: TokenUsage,
    cumulative_total: Option<u64>,
}

/// Hold the accounting derived from one exchange's retained reports.
struct UsageLedger {
    request_count: u64,
    turn: Vec<TurnUsageTotal>,
    timing: Vec<TurnTimingTotal>,
    /// Latest cumulative total per thread, including totals inherited and never superseded.
    cumulative: Vec<ThreadUsageBaseline>,
}

/// Aggregate a positively identified model call across complementary native report IDs.
struct UsageCall {
    address: TurnAddress,
    report: TurnUsageReport,
}

/// Retain independently available token totals under one original normalized turn.
struct TurnUsageTotal {
    address: TurnAddress,
    usage: TokenUsage,
}

/// Preserve the optional native total supplied by the original canonical turn result.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct TurnDuration {
    address: TurnAddress,
    reported_duration_ms: Option<u64>,
}

/// Accumulate measured call time without replacing missing native timing with host elapsed time.
struct TurnTimingTotal {
    address: TurnAddress,
    duration_ms: Option<u64>,
}

/// Name the user waits that suspend the exchange clock, by the blocker identity prefix each uses.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum UserWait {
    /// A permission request awaiting the user's decision.
    Approval,
    /// A question a running tool waits on for the user's answer.
    Answer,
}

impl UserWait {
    /// Return the blocker identity prefix that marks this kind of wait.
    pub(crate) const fn prefix(self) -> &'static str {
        match self {
            Self::Approval => "approval:",
            Self::Answer => "question:",
        }
    }

    /// Classify a blocker identity as a user wait, if it is one.
    pub(crate) fn of(blocker: &str) -> Option<Self> {
        [Self::Approval, Self::Answer]
            .into_iter()
            .find(|wait| blocker.starts_with(wait.prefix()))
    }

    /// Build the blocker identity for one wait of this kind.
    pub(crate) fn blocker(self, id: &str) -> String {
        format!("{}{id}", self.prefix())
    }
}

impl ExchangeMetrics {
    /// Reports whether any permission request still suspends this exchange.
    pub(crate) fn awaiting_approval(&self) -> bool {
        self.blocker
            .iter()
            .any(|id| id.starts_with(UserWait::Approval.prefix()))
    }

    /// Reports whether a running tool still waits for the user's answer to a question.
    pub(crate) fn awaiting_answer(&self) -> bool {
        self.blocker
            .iter()
            .any(|id| id.starts_with(UserWait::Answer.prefix()))
    }

    /// Reports whether any wait for the user, an approval or an answer, suspends this exchange.
    pub(crate) fn awaiting_user(&self) -> bool {
        self.awaiting_approval() || self.awaiting_answer()
    }

    /// Identify the questions whose answers this exchange currently waits for.
    pub(crate) fn awaited_questions(&self) -> impl Iterator<Item = &str> {
        self.blocker
            .iter()
            .filter_map(|id| id.strip_prefix(UserWait::Answer.prefix()))
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
        self.user_wait_resume = false;
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

    fn close_tools(&mut self, elapsed_ms: u64) {
        if let Some(started) = self.tool_started_ms.take() {
            self.tool_duration_ms = self
                .tool_duration_ms
                .saturating_add(elapsed_ms.saturating_sub(started));
        }
    }

    /// Get host active time outside observed tool, approval, and delegated wait intervals.
    pub fn response_ms(&self, elapsed_ms: u64) -> Option<u64> {
        if !self.timing_complete {
            return None;
        }
        let pending = self
            .blocked_started_ms
            .map_or(0, |started| elapsed_ms.saturating_sub(started));
        Some(elapsed_ms.saturating_sub(self.blocked_duration_ms.saturating_add(pending)))
    }

    /// Merge canonical accounting atomically, retaining complementary measurements without replay charges.
    pub(crate) fn usage_report(
        &mut self,
        address: &TurnAddress,
        report: &TurnUsageReport,
    ) -> Result<bool, UsageReportError> {
        self.validate_address(address)?;
        let mut reports = self.reports.clone();
        if let Some(sample) = reports.iter_mut().find(|sample| {
            sample.address == *address && sample.report.report_id() == report.report_id()
        }) {
            if !sample.report.merge(report)? {
                return Ok(false);
            }
        } else {
            reports.push(UsageSample {
                address: address.clone(),
                report: report.clone(),
            });
        }
        let mut candidate = self.clone();
        candidate.reports = reports;
        candidate.refresh_accounting()?;
        *self = candidate;
        Ok(true)
    }

    /// Apply final accounting and its independent native total duration as one atomic batch.
    pub(crate) fn complete_turn(
        &mut self,
        address: &TurnAddress,
        result: &TurnResult,
    ) -> Result<bool, UsageReportError> {
        self.validate_address(address)?;
        let mut candidate = self.clone();
        let mut changed = false;
        for report in result.final_usage() {
            changed |= candidate.usage_report(address, report)?;
        }
        let duration = result.reported_duration_ms();
        if let Some(existing) = candidate
            .completed_timing
            .iter()
            .find(|entry| entry.address == *address)
        {
            if existing.reported_duration_ms != duration {
                return Err(UsageReportError::MeasurementConflict);
            }
        } else {
            candidate.completed_timing.push(TurnDuration {
                address: address.clone(),
                reported_duration_ms: duration,
            });
            changed = true;
        }
        candidate.refresh_accounting()?;
        *self = candidate;
        Ok(changed)
    }

    /// Get canonical token expenditure for the addressed turn without counting repeated snapshots.
    pub(crate) fn turn_usage(&self, address: &TurnAddress) -> Option<TokenUsage> {
        usage_totals(&self.usage_baseline, &self.reports)
            .expect("validated accounting ledger has no conflicting call measurements")
            .turn
            .into_iter()
            .find(|total| total.address == *address)
            .map(|total| total.usage)
    }

    /// Sum canonical expenditure while retaining unavailable categories across distinct calls.
    pub(crate) fn usage(&self) -> TokenUsage {
        let ledger = usage_totals(&self.usage_baseline, &self.reports)
            .expect("validated accounting ledger has no conflicting call measurements");
        let mut totals = ledger.turn.into_iter().map(|total| total.usage);
        let Some(mut usage) = totals.next() else {
            return Default::default();
        };
        for incoming in totals {
            usage.accumulate(&incoming);
        }
        usage
    }

    /// Create the metrics of the exchange that follows `previous` on the same provider threads.
    ///
    /// The new exchange inherits each thread's latest cumulative total, including a total
    /// `previous` itself inherited and never superseded, so a provider's repeat of that total
    /// stays charged to the exchange that first received it.
    pub(crate) fn following(previous: &Self) -> Self {
        Self {
            usage_baseline: usage_totals(&previous.usage_baseline, &previous.reports)
                .expect("validated accounting ledger has no conflicting call measurements")
                .cumulative,
            ..Self::default()
        }
    }

    /// Reject mismatched typed relationships before changing retained measurements.
    fn validate_address(&self, address: &TurnAddress) -> Result<(), UsageReportError> {
        if address.thread_id != address.scope.owner().thread_id()
            || address.scope.turn_id().as_ref() != Ok(&address.turn_id)
            || self.reports.iter().any(|sample| {
                sample.address.turn_id == address.turn_id && sample.address != *address
            })
            || self.completed_timing.iter().any(|sample| {
                sample.address.turn_id == address.turn_id && sample.address != *address
            })
        {
            return Err(UsageReportError::IdentityConflict);
        }
        Ok(())
    }

    /// Rebuild aggregate tokens and native timing exclusively from retained canonical observations.
    fn refresh_accounting(&mut self) -> Result<(), UsageReportError> {
        let UsageLedger {
            request_count,
            turn: totals,
            timing: timings,
            cumulative: _,
        } = usage_totals(&self.usage_baseline, &self.reports)?;
        // A turn whose output count is unknown, such as one a compaction or a configuration change
        // stopped mid-answer and ran again, leaves the exchange's output unknown. The turns after
        // the newest such turn produced the answer, so the output and its rate cover only them;
        // summing across the stopped turn dropped the rerun's figures from the heading.
        let answer_start = totals
            .iter()
            .rposition(|total| total.usage.output.is_none())
            .map_or(0, |unknown| unknown + 1);
        let answer_totals = &totals[answer_start..];
        let output = answer_totals
            .iter()
            .map(|total| total.usage.output)
            .reduce(|sum, output| {
                sum.zip(output)
                    .and_then(|(sum, added)| sum.checked_add(added))
            })
            .flatten();
        let mut addresses = answer_totals
            .iter()
            .map(|total| &total.address)
            .collect::<Vec<_>>();
        // A turn that reported only timing has no place among the answer's turns.
        if answer_start == 0 {
            for timing in &timings {
                if !addresses.contains(&&timing.address) {
                    addresses.push(&timing.address);
                }
            }
            for timing in &self.completed_timing {
                if !addresses.contains(&&timing.address) {
                    addresses.push(&timing.address);
                }
            }
        }
        let duration = if addresses.is_empty() {
            None
        } else {
            addresses.iter().try_fold(0_u64, |sum, address| {
                let duration = self
                    .completed_timing
                    .iter()
                    .find(|entry| &entry.address == *address)
                    .and_then(|entry| entry.reported_duration_ms)
                    .or_else(|| {
                        timings
                            .iter()
                            .find(|entry| &entry.address == *address)
                            .and_then(|entry| entry.duration_ms)
                    })?;
                sum.checked_add(duration)
            })
        };
        self.request_count = request_count;
        self.reported_output_tokens = output;
        self.reported_response_ms = duration;
        Ok(())
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
            user_wait_resume: false,
            tool: BTreeSet::new(),
            blocker: BTreeSet::new(),
            reports: Vec::new(),
            completed_timing: Vec::new(),
            usage_baseline: Vec::new(),
        }
    }
}

/// Coalesce identified calls before deriving chronological cumulative increments.
///
/// `baseline` holds the totals earlier exchanges already charged. It only identifies a report
/// that repeats such a total: the native thread behind a Harness thread can be replaced between
/// exchanges, so a changed total is no evidence of what the new exchange spent, and its first
/// report keeps the provider's own per-call breakdown.
fn usage_totals(
    baseline: &[ThreadUsageBaseline],
    samples: &[UsageSample],
) -> Result<UsageLedger, UsageReportError> {
    let mut calls: Vec<UsageCall> = Vec::new();
    let mut ordered = samples.iter().collect::<Vec<_>>();
    ordered.sort_by_key(|sample| *sample.report.observed_at_ms());
    for sample in ordered {
        let existing = sample.report.model_call_id().as_ref().and_then(|call| {
            calls.iter_mut().find(|retained| {
                retained.address == sample.address
                    && retained.report.model_call_id().as_ref() == Some(call)
            })
        });
        if let Some(existing) = existing {
            merge_call_report(&mut existing.report, &sample.report)?;
        } else {
            calls.push(UsageCall {
                address: sample.address.clone(),
                report: sample.report.clone(),
            });
        }
    }
    calls.sort_by_key(|call| *call.report.observed_at_ms());
    let mut cursor: HashMap<forge_provider_api::ThreadId, (TokenUsage, Option<u64>)> =
        HashMap::new();
    let mut totals: Vec<TurnUsageTotal> = Vec::new();
    let mut request_count = 0_u64;
    let mut timings: Vec<TurnTimingTotal> = Vec::new();
    for call in calls {
        let report = &call.report;
        let mut usage = report.usage().clone();
        let mut unchanged_snapshot = false;
        if let Some(cumulative) = report.cumulative() {
            if let Some((previous, previous_total)) = cursor.get(&call.address.thread_id) {
                unchanged_snapshot =
                    cumulative == previous && report.cumulative_total() == previous_total;
                let reset = previous_total
                    .zip(*report.cumulative_total())
                    .is_some_and(|(previous, current)| current < previous);
                if !unchanged_snapshot && !reset {
                    usage = Some(cumulative_increment(cumulative, previous, usage.as_ref()));
                }
            } else {
                unchanged_snapshot = baseline.iter().any(|inherited| {
                    inherited.thread_id == call.address.thread_id
                        && inherited.cumulative == *cumulative
                        && inherited.cumulative_total == *report.cumulative_total()
                });
            }
            cursor.insert(
                call.address.thread_id.clone(),
                (cumulative.clone(), *report.cumulative_total()),
            );
        }
        if unchanged_snapshot && report.model_call_id().is_none() {
            continue;
        }
        // One endpoint that no call identity, token count, or duration accompanies cannot be
        // attributed to a call. Counting it as a call without timing would erase the duration
        // of the call it actually ends, as Copilot's call-end event did.
        let identified_call = report.model_call_id().is_some()
            || report.reported_duration_ms().is_some()
            || (report.started_at_ms().is_some() && report.completed_at_ms().is_some())
            || (report.usage().is_some() && !unchanged_snapshot);
        if identified_call {
            request_count = request_count
                .checked_add(1)
                .ok_or(UsageReportError::MeasurementConflict)?;
            let duration = report.reported_duration_ms().or_else(|| {
                report
                    .started_at_ms()
                    .zip(*report.completed_at_ms())
                    .and_then(|(start, end)| {
                        u64::try_from(i128::from(end) - i128::from(start)).ok()
                    })
            });
            if let Some(timing) = timings
                .iter_mut()
                .find(|entry| entry.address == call.address)
            {
                timing.duration_ms = timing
                    .duration_ms
                    .zip(duration)
                    .and_then(|(sum, duration)| sum.checked_add(duration));
            } else {
                timings.push(TurnTimingTotal {
                    address: call.address.clone(),
                    duration_ms: duration,
                });
            }
        }
        // A merged model call without token evidence makes its categories unknown; a report
        // that measured only timing carries no token claim and leaves totals unchanged.
        if let Some(usage) =
            usage.or_else(|| report.model_call_id().is_some().then(TokenUsage::default))
        {
            if usage
                .cached_input
                .zip(usage.input)
                .is_some_and(|(cached, input)| cached > input)
                || usage
                    .reasoning
                    .zip(usage.output)
                    .is_some_and(|(reasoning, output)| reasoning > output)
            {
                return Err(UsageReportError::InvalidTokenBreakdown);
            }
            if let Some(total) = totals
                .iter_mut()
                .find(|total| total.address == call.address)
            {
                total.usage.accumulate(&usage);
            } else {
                totals.push(TurnUsageTotal {
                    address: call.address,
                    usage,
                });
            }
        }
    }
    let mut cumulative = baseline
        .iter()
        .filter(|inherited| !cursor.contains_key(&inherited.thread_id))
        .cloned()
        .collect::<Vec<_>>();
    cumulative.extend(
        cursor.into_iter().map(
            |(thread_id, (cumulative, cumulative_total))| ThreadUsageBaseline {
                thread_id,
                cumulative,
                cumulative_total,
            },
        ),
    );
    cumulative.sort_by(|left, right| left.thread_id.cmp(&right.thread_id));
    Ok(UsageLedger {
        request_count,
        turn: totals,
        timing: timings,
        cumulative,
    })
}

/// Preserve per-call observations when a cumulative category has no trustworthy monotonic baseline.
fn cumulative_increment(
    current: &TokenUsage,
    previous: &TokenUsage,
    fallback: Option<&TokenUsage>,
) -> TokenUsage {
    let increment = |current: Option<u64>, previous: Option<u64>, fallback: Option<u64>| {
        current
            .zip(previous)
            .and_then(|(current, previous)| current.checked_sub(previous))
            .or(fallback)
    };
    TokenUsage {
        input: increment(
            current.input,
            previous.input,
            fallback.and_then(|usage| usage.input),
        ),
        cached_input: increment(
            current.cached_input,
            previous.cached_input,
            fallback.and_then(|usage| usage.cached_input),
        ),
        reasoning: increment(
            current.reasoning,
            previous.reasoning,
            fallback.and_then(|usage| usage.reasoning),
        ),
        output: increment(
            current.output,
            previous.output,
            fallback.and_then(|usage| usage.output),
        ),
    }
}

/// Merge observations sharing a positive native call identity without treating report aliases as calls.
fn merge_call_report(
    retained: &mut TurnUsageReport,
    incoming: &TurnUsageReport,
) -> Result<(), UsageReportError> {
    let mut next = retained.clone();
    let id = retained.report_id().clone();
    let observed_at_ms = *incoming.observed_at_ms();
    let has_tokens = |usage: &TokenUsage| {
        usage.input.is_some()
            || usage.cached_input.is_some()
            || usage.reasoning.is_some()
            || usage.output.is_some()
    };
    if incoming.usage().as_ref().is_some_and(has_tokens)
        || incoming.cumulative().as_ref().is_some_and(has_tokens)
        || incoming.cumulative_total().is_some()
    {
        let report = TurnUsageReport::tokens(
            id.clone(),
            incoming.model_call_id().clone(),
            observed_at_ms,
            incoming.usage().clone(),
            incoming.cumulative().clone(),
            *incoming.cumulative_total(),
        )?;
        next.merge(&report)?;
    }
    if let Some(duration) = incoming.reported_duration_ms() {
        next.merge(&TurnUsageReport::accounting(
            id.clone(),
            observed_at_ms,
            None,
            Some(*duration),
        )?)?;
    }
    if incoming.started_at_ms().is_some() || incoming.completed_at_ms().is_some() {
        next.merge(&TurnUsageReport::interval(
            id,
            incoming.model_call_id().clone(),
            observed_at_ms,
            *incoming.started_at_ms(),
            *incoming.completed_at_ms(),
        )?)?;
    }
    *retained = next;
    Ok(())
}
