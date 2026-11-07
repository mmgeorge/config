//! Retained native response-cycle evidence, independent of collection and host commit.

use serde::{Deserialize, Serialize};

use crate::turn::TurnOutcome;
use crate::{CheckpointId, DeliveryAttemptId, SubmissionId, TurnUsageReport, UsageReportError};

/// Identify the exact journaled input attempt accounted for by native terminal evidence.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CoveredInput {
    submission_id: SubmissionId,
    attempt_id: DeliveryAttemptId,
}

impl CoveredInput {
    /// Create coverage from the journal's already validated identities.
    pub fn new(submission_id: SubmissionId, attempt_id: DeliveryAttemptId) -> Self {
        Self {
            submission_id,
            attempt_id,
        }
    }

    /// Get the logical input whose receipt established acceptance.
    pub fn submission_id(&self) -> &SubmissionId {
        &self.submission_id
    }

    /// Get the delivery attempt covered by this response cycle.
    pub fn attempt_id(&self) -> &DeliveryAttemptId {
        &self.attempt_id
    }
}

/// Preserve one native outcome and its exact input and accounting coverage.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "TurnResultWire")]
pub struct TurnResult {
    outcome: TurnOutcome,
    completed_at_ms: i64,
    reported_duration_ms: Option<u64>,
    covered_inputs: Vec<CoveredInput>,
    final_usage: Vec<TurnUsageReport>,
    checkpoint_id: Option<CheckpointId>,
}

/// Carry the retained native result after provider-owned foreground collection succeeds.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CompletedTurn {
    result: TurnResult,
    collected_at_ms: i64,
}

impl CompletedTurn {
    /// Record successful provider collection without changing the native result.
    pub(crate) fn new(result: TurnResult, collected_at_ms: i64) -> Self {
        Self {
            result,
            collected_at_ms,
        }
    }

    /// Get the original native outcome, accounting, and input coverage.
    pub fn result(&self) -> &TurnResult {
        &self.result
    }

    /// Get the collection timestamp independently of native completion time.
    pub fn collected_at_ms(&self) -> i64 {
        self.collected_at_ms
    }
}

#[derive(Deserialize)]
struct TurnResultWire {
    outcome: TurnOutcome,
    completed_at_ms: i64,
    reported_duration_ms: Option<u64>,
    covered_inputs: Vec<CoveredInput>,
    final_usage: Vec<TurnUsageReport>,
    checkpoint_id: Option<CheckpointId>,
}

/// Reject contradictory native completion evidence before retaining or loading it.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum TurnResultError {
    /// One logical input cannot be accounted for by conflicting accepted attempts.
    #[error("turn result covers conflicting attempts for one input")]
    ConflictingInput,
    /// Repeated final accounting must agree with measurements already supplied.
    #[error(transparent)]
    Usage(#[from] UsageReportError),
}

impl TurnResult {
    /// Create terminal evidence while merging identical coverage and complementary accounting.
    pub fn new(
        outcome: TurnOutcome,
        completed_at_ms: i64,
        reported_duration_ms: Option<u64>,
        covered_inputs: Vec<CoveredInput>,
        final_usage: Vec<TurnUsageReport>,
        checkpoint_id: Option<CheckpointId>,
    ) -> Result<Self, TurnResultError> {
        let mut coverage: Vec<CoveredInput> = Vec::new();
        for input in covered_inputs {
            if let Some(retained) = coverage
                .iter()
                .find(|retained| retained.submission_id == input.submission_id)
            {
                if retained != &input {
                    return Err(TurnResultError::ConflictingInput);
                }
            } else {
                coverage.push(input);
            }
        }
        let mut reports: Vec<TurnUsageReport> = Vec::new();
        for report in final_usage {
            if let Some(retained) = reports
                .iter_mut()
                .find(|retained| retained.report_id() == report.report_id())
            {
                retained.merge(&report)?;
            } else {
                reports.push(report);
            }
        }
        Ok(Self {
            outcome,
            completed_at_ms,
            reported_duration_ms,
            covered_inputs: coverage,
            final_usage: reports,
            checkpoint_id,
        })
    }

    /// Get the native outcome independently of provider collection or host success.
    pub fn outcome(&self) -> TurnOutcome {
        self.outcome
    }

    /// Get the timestamp retained when native completion was observed.
    pub fn completed_at_ms(&self) -> i64 {
        self.completed_at_ms
    }

    /// Get provider-reported duration without substituting host wall-clock elapsed time.
    pub fn reported_duration_ms(&self) -> Option<u64> {
        self.reported_duration_ms
    }

    /// Get the exact accepted input attempts covered by this cycle.
    pub fn covered_inputs(&self) -> &[CoveredInput] {
        &self.covered_inputs
    }

    /// Get identified final accounting without assigning it lifecycle authority.
    pub fn final_usage(&self) -> &[TurnUsageReport] {
        &self.final_usage
    }

    /// Get the native checkpoint boundary established by this result.
    pub fn checkpoint_id(&self) -> Option<&CheckpointId> {
        self.checkpoint_id.as_ref()
    }
}

impl TryFrom<TurnResultWire> for TurnResult {
    type Error = TurnResultError;

    /// Validate loaded results through the same boundary used by native drivers.
    fn try_from(wire: TurnResultWire) -> Result<Self, Self::Error> {
        let TurnResultWire {
            outcome,
            completed_at_ms,
            reported_duration_ms,
            covered_inputs,
            final_usage,
            checkpoint_id,
        } = wire;
        Self::new(
            outcome,
            completed_at_ms,
            reported_duration_ms,
            covered_inputs,
            final_usage,
            checkpoint_id,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Create exact journal coverage for result validation fixtures.
    fn input(attempt: &str) -> CoveredInput {
        CoveredInput::new(
            "input".try_into().unwrap(),
            DeliveryAttemptId::new(attempt).unwrap(),
        )
    }

    /// Conflicting attempts cannot enter through construction or persisted JSON.
    #[test]
    fn conflicting_coverage_is_rejected_at_each_boundary() {
        let outcome = TurnOutcome::Interrupted;
        assert_eq!(
            TurnResult::new(
                outcome,
                10,
                None,
                vec![input("one"), input("two")],
                vec![],
                None
            ),
            Err(TurnResultError::ConflictingInput)
        );
        let result = TurnResult::new(
            outcome,
            10,
            None,
            vec![input("one"), input("one")],
            vec![],
            None,
        )
        .unwrap();
        assert_eq!(result.covered_inputs(), &[input("one")]);
        let mut wire = serde_json::to_value(result).unwrap();
        wire["covered_inputs"]
            .as_array_mut()
            .unwrap()
            .push(serde_json::to_value(input("two")).unwrap());
        assert!(serde_json::from_value::<TurnResult>(wire.clone()).is_err());
        wire["covered_inputs"][1]["attempt_id"] = serde_json::json!(" ");
        assert!(serde_json::from_value::<TurnResult>(wire).is_err());
    }
}
