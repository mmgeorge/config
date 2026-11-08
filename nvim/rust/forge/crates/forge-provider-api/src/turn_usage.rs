//! Identified native accounting independent of turn completion.

use serde::{Deserialize, Serialize};

/// Retain one validated token or timing observation without assigning lifecycle authority.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "ReportWire")]
pub struct TurnUsageReport {
    report_id: crate::identity::UsageReportId,
    model_call_id: Option<crate::identity::ModelCallId>,
    observed_at_ms: i64,
    usage: Option<crate::usage::TokenUsage>,
    cumulative: Option<crate::usage::TokenUsage>,
    cumulative_total: Option<u64>,
    reported_duration_ms: Option<u64>,
    started_at_ms: Option<i64>,
    completed_at_ms: Option<i64>,
}

#[derive(Deserialize)]
struct ReportWire {
    report_id: crate::identity::UsageReportId,
    model_call_id: Option<crate::identity::ModelCallId>,
    observed_at_ms: i64,
    usage: Option<crate::usage::TokenUsage>,
    cumulative: Option<crate::usage::TokenUsage>,
    cumulative_total: Option<u64>,
    reported_duration_ms: Option<u64>,
    started_at_ms: Option<i64>,
    completed_at_ms: Option<i64>,
}

/// Reject accounting without any observed measurement.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum UsageReportError {
    /// No token or timing measurement was observed.
    #[error("usage report contains no token or timing observation")]
    EmptyObservation,
    /// Reported native timing endpoints are reversed.
    #[error("usage report completion precedes its start")]
    ReversedInterval,
    /// A reported subset exceeds its inclusive token category.
    #[error("usage report token breakdown exceeds its inclusive count")]
    InvalidTokenBreakdown,
    /// Repeated accounting refers to another report or model call.
    #[error("usage report identity conflicts with retained accounting")]
    IdentityConflict,
    /// Repeated accounting changes a measurement already observed.
    #[error("usage report conflicts with a retained measurement")]
    MeasurementConflict,
}

impl TurnUsageReport {
    /// Create one identified token and timing observation without inventing absent measurements.
    pub fn accounting(
        report_id: crate::identity::UsageReportId,
        observed_at_ms: i64,
        usage: Option<crate::usage::TokenUsage>,
        reported_duration_ms: Option<u64>,
    ) -> Result<Self, UsageReportError> {
        ReportWire {
            report_id,
            observed_at_ms,
            usage,
            reported_duration_ms,
            model_call_id: None,
            cumulative: None,
            cumulative_total: None,
            started_at_ms: None,
            completed_at_ms: None,
        }
        .try_into()
    }

    /// Create correlated native call accounting with independently reported tokens and duration.
    pub fn call(
        report_id: crate::identity::UsageReportId,
        model_call_id: Option<crate::identity::ModelCallId>,
        observed_at_ms: i64,
        usage: Option<crate::usage::TokenUsage>,
        reported_duration_ms: Option<u64>,
    ) -> Result<Self, UsageReportError> {
        ReportWire {
            report_id,
            model_call_id,
            observed_at_ms,
            usage,
            reported_duration_ms,
            cumulative: None,
            cumulative_total: None,
            started_at_ms: None,
            completed_at_ms: None,
        }
        .try_into()
    }
    /// Create identified timing endpoints without deriving a duration or inventing an absent boundary.
    pub fn interval(
        report_id: crate::identity::UsageReportId,
        model_call_id: Option<crate::identity::ModelCallId>,
        observed_at_ms: i64,
        started_at_ms: Option<i64>,
        completed_at_ms: Option<i64>,
    ) -> Result<Self, UsageReportError> {
        ReportWire {
            report_id,
            model_call_id,
            observed_at_ms,
            started_at_ms,
            completed_at_ms,
            usage: None,
            cumulative: None,
            cumulative_total: None,
            reported_duration_ms: None,
        }
        .try_into()
    }

    /// Create identified per-call and cumulative token observations without synthesizing counts.
    pub fn tokens(
        report_id: crate::identity::UsageReportId,
        model_call_id: Option<crate::identity::ModelCallId>,
        observed_at_ms: i64,
        usage: Option<crate::usage::TokenUsage>,
        cumulative: Option<crate::usage::TokenUsage>,
        cumulative_total: Option<u64>,
    ) -> Result<Self, UsageReportError> {
        ReportWire {
            report_id,
            model_call_id,
            observed_at_ms,
            usage,
            cumulative,
            cumulative_total,
            reported_duration_ms: None,
            started_at_ms: None,
            completed_at_ms: None,
        }
        .try_into()
    }
    /// Merge complementary observations atomically while rejecting conflicting replay.
    pub fn merge(&mut self, incoming: &Self) -> Result<bool, UsageReportError> {
        if self.report_id != incoming.report_id {
            return Err(UsageReportError::IdentityConflict);
        }
        let mut next = self.clone();
        merge_optional(&mut next.model_call_id, &incoming.model_call_id)
            .map_err(|_| UsageReportError::IdentityConflict)?;
        merge_tokens(&mut next.usage, &incoming.usage)?;
        merge_tokens(&mut next.cumulative, &incoming.cumulative)?;
        merge_optional(&mut next.cumulative_total, &incoming.cumulative_total)?;
        merge_optional(
            &mut next.reported_duration_ms,
            &incoming.reported_duration_ms,
        )?;
        merge_optional(&mut next.started_at_ms, &incoming.started_at_ms)?;
        merge_optional(&mut next.completed_at_ms, &incoming.completed_at_ms)?;
        validate_measurements(&next)?;
        let changed = next != *self;
        *self = next;
        Ok(changed)
    }

    /// Create an identified timing report without inventing token counts.
    pub fn timing(
        report_id: crate::identity::UsageReportId,
        observed_at_ms: i64,
        duration_ms: u64,
    ) -> Self {
        Self {
            report_id,
            observed_at_ms,
            reported_duration_ms: Some(duration_ms),
            model_call_id: None,
            usage: None,
            cumulative: None,
            cumulative_total: None,
            started_at_ms: None,
            completed_at_ms: None,
        }
    }

    /// Get the report id.
    pub fn report_id(&self) -> &crate::identity::UsageReportId {
        &self.report_id
    }

    /// Get the model call id.
    pub fn model_call_id(&self) -> &Option<crate::identity::ModelCallId> {
        &self.model_call_id
    }

    /// Get the observed at ms.
    pub fn observed_at_ms(&self) -> &i64 {
        &self.observed_at_ms
    }

    /// Get the usage.
    pub fn usage(&self) -> &Option<crate::usage::TokenUsage> {
        &self.usage
    }

    /// Get the cumulative.
    pub fn cumulative(&self) -> &Option<crate::usage::TokenUsage> {
        &self.cumulative
    }

    /// Get the cumulative total.
    pub fn cumulative_total(&self) -> &Option<u64> {
        &self.cumulative_total
    }

    /// Get the reported duration ms.
    pub fn reported_duration_ms(&self) -> &Option<u64> {
        &self.reported_duration_ms
    }

    /// Get the started at ms.
    pub fn started_at_ms(&self) -> &Option<i64> {
        &self.started_at_ms
    }

    /// Get the completed at ms.
    pub fn completed_at_ms(&self) -> &Option<i64> {
        &self.completed_at_ms
    }
}

impl TryFrom<ReportWire> for TurnUsageReport {
    type Error = UsageReportError;

    /// Validate deserialized measurements before admitting the report.
    fn try_from(wire: ReportWire) -> Result<Self, Self::Error> {
        let ReportWire {
            report_id,
            model_call_id,
            observed_at_ms,
            usage,
            cumulative,
            cumulative_total,
            reported_duration_ms,
            started_at_ms,
            completed_at_ms,
        } = wire;
        let has_tokens = |usage: &crate::usage::TokenUsage| {
            usage.input.is_some()
                || usage.cached_input.is_some()
                || usage.reasoning.is_some()
                || usage.output.is_some()
        };
        if !usage.as_ref().is_some_and(has_tokens)
            && !cumulative.as_ref().is_some_and(has_tokens)
            && cumulative_total.is_none()
            && reported_duration_ms.is_none()
            && started_at_ms.is_none()
            && completed_at_ms.is_none()
        {
            return Err(UsageReportError::EmptyObservation);
        }
        let report = Self {
            report_id,
            model_call_id,
            observed_at_ms,
            usage,
            cumulative,
            cumulative_total,
            reported_duration_ms,
            started_at_ms,
            completed_at_ms,
        };
        validate_measurements(&report)?;
        Ok(report)
    }
}

/// Validate independent observations before publication or an atomic merge.
fn validate_measurements(report: &TurnUsageReport) -> Result<(), UsageReportError> {
    if let (Some(start), Some(end)) = (report.started_at_ms, report.completed_at_ms)
        && end < start
    {
        return Err(UsageReportError::ReversedInterval);
    }
    for usage in [report.usage.as_ref(), report.cumulative.as_ref()]
        .into_iter()
        .flatten()
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
    }
    Ok(())
}
/// Retain an optional measurement once without treating absence as zero.
fn merge_optional<T: Clone + Eq>(
    current: &mut Option<T>,
    incoming: &Option<T>,
) -> Result<(), UsageReportError> {
    match (current.as_ref(), incoming) {
        (Some(old), Some(new)) if old != new => return Err(UsageReportError::MeasurementConflict),
        (None, Some(new)) => *current = Some(new.clone()),
        (Some(_), Some(_)) | (Some(_), None) | (None, None) => {}
    }
    Ok(())
}

/// Combine independently reported token categories without charging either observation twice.
fn merge_tokens(
    current: &mut Option<crate::usage::TokenUsage>,
    incoming: &Option<crate::usage::TokenUsage>,
) -> Result<(), UsageReportError> {
    if let Some(incoming) = incoming {
        let current = current.get_or_insert_with(Default::default);
        merge_optional(&mut current.input, &incoming.input)?;
        merge_optional(&mut current.cached_input, &incoming.cached_input)?;
        merge_optional(&mut current.reasoning, &incoming.reasoning)?;
        merge_optional(&mut current.output, &incoming.output)?;
    }
    Ok(())
}
