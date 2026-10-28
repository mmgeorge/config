use super::{ExchangeOutcome, ExchangeState, HistoryDisposition};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(from = "QuestionRecord", into = "QuestionRecord")]
/// Retains pending questions independently of an active clarification execution.
pub(crate) enum ExchangeQuestion {
    None,
    External,
    Waiting(crate::plan::PlanElicitation),
    Answering(crate::plan::PlanElicitation),
}

#[derive(Deserialize, Serialize)]
struct QuestionRecord {
    #[serde(default)]
    awaiting_input: bool,
    #[serde(default)]
    elicitation: Option<crate::plan::PlanElicitation>,
}

impl ExchangeQuestion {
    pub fn waiting(&self) -> bool {
        matches!(self, Self::External | Self::Waiting(_))
    }
    pub fn elicitation(&self) -> Option<&crate::plan::PlanElicitation> {
        match self {
            Self::Waiting(value) | Self::Answering(value) => Some(value),
            _ => None,
        }
    }
    pub fn elicitation_mut(&mut self) -> Option<&mut crate::plan::PlanElicitation> {
        match self {
            Self::Waiting(value) | Self::Answering(value) => Some(value),
            _ => None,
        }
    }
    pub fn set_waiting(&mut self, waiting: bool) {
        let previous = std::mem::replace(self, Self::None);
        *self = QuestionRecord {
            awaiting_input: waiting,
            elicitation: previous.into_elicitation(),
        }
        .into();
    }
    pub fn set_elicitation(&mut self, elicitation: Option<crate::plan::PlanElicitation>) {
        *self = QuestionRecord {
            awaiting_input: self.waiting(),
            elicitation,
        }
        .into();
    }
    pub fn take_elicitation(&mut self) -> Option<crate::plan::PlanElicitation> {
        let replacement = if self.waiting() {
            Self::External
        } else {
            Self::None
        };
        std::mem::replace(self, replacement).into_elicitation()
    }
    fn into_elicitation(self) -> Option<crate::plan::PlanElicitation> {
        match self {
            Self::Waiting(value) | Self::Answering(value) => Some(value),
            _ => None,
        }
    }
}

impl From<QuestionRecord> for ExchangeQuestion {
    fn from(record: QuestionRecord) -> Self {
        match (record.awaiting_input, record.elicitation) {
            (true, Some(value)) => Self::Waiting(value),
            (false, Some(value)) => Self::Answering(value),
            (true, None) => Self::External,
            (false, None) => Self::None,
        }
    }
}

impl From<ExchangeQuestion> for QuestionRecord {
    fn from(question: ExchangeQuestion) -> Self {
        Self {
            awaiting_input: question.waiting(),
            elicitation: question.into_elicitation(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(from = "ClockRecord", into = "ClockRecord")]
/// Owns the active interval or accumulated paused time without a separate running flag.
pub(crate) enum ExchangeClock {
    Paused { elapsed_ms: u64 },
    Running { elapsed_ms: u64, started_at_ms: i64 },
}

#[derive(Deserialize, Serialize)]
struct ClockRecord {
    #[serde(default)]
    duration_ms: u64,
    #[serde(default)]
    execution_started_at_ms: Option<i64>,
}

impl ExchangeClock {
    pub fn duration_ms(&self) -> u64 {
        match self {
            Self::Paused { elapsed_ms } | Self::Running { elapsed_ms, .. } => *elapsed_ms,
        }
    }
    pub fn started_at_ms(&self) -> Option<i64> {
        match self {
            Self::Running { started_at_ms, .. } => Some(*started_at_ms),
            Self::Paused { .. } => None,
        }
    }
    pub fn resume(&mut self, now_ms: i64) {
        if let Self::Paused { elapsed_ms } = self {
            *self = Self::Running {
                elapsed_ms: *elapsed_ms,
                started_at_ms: now_ms,
            };
        }
    }
    pub fn pause(&mut self, now_ms: i64) {
        let elapsed_ms = self.duration_ms().saturating_add(
            self.started_at_ms()
                .map_or(0, |started| now_ms.saturating_sub(started).max(0) as u64),
        );
        *self = Self::Paused { elapsed_ms };
    }
    pub fn elapsed(&self, now_ms: i64) -> u64 {
        self.duration_ms().saturating_add(
            self.started_at_ms()
                .map_or(0, |started| now_ms.saturating_sub(started).max(0) as u64),
        )
    }
    #[cfg(test)]
    pub fn fixture(duration_ms: u64, started_at_ms: Option<i64>) -> Self {
        ClockRecord {
            duration_ms,
            execution_started_at_ms: started_at_ms,
        }
        .into()
    }
}

impl From<ClockRecord> for ExchangeClock {
    fn from(record: ClockRecord) -> Self {
        match record.execution_started_at_ms {
            Some(started_at_ms) => Self::Running {
                elapsed_ms: record.duration_ms,
                started_at_ms,
            },
            None => Self::Paused {
                elapsed_ms: record.duration_ms,
            },
        }
    }
}
impl From<ExchangeClock> for ClockRecord {
    fn from(clock: ExchangeClock) -> Self {
        Self {
            duration_ms: clock.duration_ms(),
            execution_started_at_ms: clock.started_at_ms(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(from = "PhaseRecord", into = "PhaseRecord")]
/// Couples terminal outcomes to completion evidence and keeps legacy omissions explicit.
pub(crate) enum ExchangePhase {
    Queued,
    Running,
    Finalizing(ExchangeOutcome),
    LegacyFinalizing,
    Finished(Completion),
}

#[derive(Clone, Debug)]
pub(crate) struct Completion {
    outcome: ExchangeOutcome,
    timestamp: CompletionTimestamp,
    disposition: HistoryDisposition,
    finalized: bool,
}

#[derive(Clone, Copy, Debug)]
enum CompletionTimestamp {
    Recorded(i64),
    LegacyUnknown,
}

#[derive(Deserialize, Serialize)]
struct PhaseRecord {
    state: ExchangeState,
    #[serde(default)]
    disposition: HistoryDisposition,
    completed_at_ms: Option<i64>,
    #[serde(default)]
    finalization_outcome: Option<ExchangeOutcome>,
}

impl ExchangePhase {
    pub fn state(&self) -> ExchangeState {
        match self {
            Self::Queued => ExchangeState::Queued,
            Self::Running => ExchangeState::Running,
            Self::Finalizing(_) | Self::LegacyFinalizing => ExchangeState::Finalizing,
            Self::Finished(completion) => completion.outcome.state(),
        }
    }

    pub fn completed_at_ms(&self) -> Option<i64> {
        match self {
            Self::Finished(Completion {
                timestamp: CompletionTimestamp::Recorded(timestamp),
                ..
            }) => Some(*timestamp),
            _ => None,
        }
    }

    pub fn outcome(&self) -> Option<ExchangeOutcome> {
        match self {
            Self::Finalizing(outcome) => Some(*outcome),
            Self::Finished(completion) if completion.finalized => Some(completion.outcome),
            _ => None,
        }
    }

    pub fn disposition(&self) -> HistoryDisposition {
        match self {
            Self::Finished(completion) => completion.disposition,
            _ => HistoryDisposition::Current,
        }
    }

    pub fn complete(&mut self, outcome: ExchangeOutcome, timestamp: i64) {
        let finalized = matches!(self, Self::Finalizing(_) | Self::LegacyFinalizing);
        *self = Self::Finished(Completion {
            outcome,
            timestamp: CompletionTimestamp::Recorded(timestamp),
            disposition: HistoryDisposition::Current,
            finalized,
        });
    }

    pub fn set_disposition(&mut self, disposition: HistoryDisposition) -> anyhow::Result<()> {
        let Self::Finished(completion) = self else {
            anyhow::bail!("running exchange cannot leave current history");
        };
        completion.disposition = disposition;
        Ok(())
    }

    #[cfg(test)]
    pub fn fixture(
        state: ExchangeState,
        timestamp: Option<i64>,
        outcome: Option<ExchangeOutcome>,
    ) -> Self {
        PhaseRecord {
            state,
            completed_at_ms: timestamp,
            finalization_outcome: outcome,
            disposition: HistoryDisposition::Current,
        }
        .into()
    }
}

impl From<PhaseRecord> for ExchangePhase {
    fn from(record: PhaseRecord) -> Self {
        match record.state {
            ExchangeState::Queued => Self::Queued,
            ExchangeState::Running => Self::Running,
            ExchangeState::Finalizing => record
                .finalization_outcome
                .map(Self::Finalizing)
                .unwrap_or(Self::LegacyFinalizing),
            ExchangeState::Complete
            | ExchangeState::Failed
            | ExchangeState::Cancelled
            | ExchangeState::Interrupted => {
                let outcome = match record.state {
                    ExchangeState::Complete => ExchangeOutcome::Complete,
                    ExchangeState::Failed => ExchangeOutcome::Failed,
                    ExchangeState::Cancelled => ExchangeOutcome::Cancelled,
                    _ => ExchangeOutcome::Interrupted,
                };
                Self::Finished(Completion {
                    outcome,
                    timestamp: record
                        .completed_at_ms
                        .map(CompletionTimestamp::Recorded)
                        .unwrap_or(CompletionTimestamp::LegacyUnknown),
                    disposition: record.disposition,
                    finalized: record.finalization_outcome.is_some(),
                })
            }
        }
    }
}

impl From<ExchangePhase> for PhaseRecord {
    fn from(phase: ExchangePhase) -> Self {
        Self {
            state: phase.state(),
            completed_at_ms: phase.completed_at_ms(),
            disposition: phase.disposition(),
            finalization_outcome: phase.outcome(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phase_roundtrip_preserves_legacy_unknown_completion_and_finalization() {
        for state in [
            "queued",
            "running",
            "finalizing",
            "complete",
            "failed",
            "cancelled",
            "interrupted",
        ] {
            let record = serde_json::json!({"state":state,"completed_at_ms":null,"disposition":"current","finalization_outcome":null});
            let phase: ExchangePhase = serde_json::from_value(record.clone()).unwrap();
            assert_eq!(serde_json::to_value(phase).unwrap(), record);
        }
        let mut phase = ExchangePhase::Finalizing(ExchangeOutcome::Cancelled);
        phase.complete(ExchangeOutcome::Cancelled, 42);
        assert_eq!(phase.completed_at_ms(), Some(42));
        assert_eq!(phase.outcome(), Some(ExchangeOutcome::Cancelled));
        phase
            .set_disposition(HistoryDisposition::RolledBack)
            .unwrap();
        let restored: ExchangePhase =
            serde_json::from_value(serde_json::to_value(phase).unwrap()).unwrap();
        assert_eq!(restored.disposition(), HistoryDisposition::RolledBack);
        assert_eq!(restored.completed_at_ms(), Some(42));
    }

    #[test]
    fn execution_clock_pauses_once_and_roundtrips_active_intervals() {
        let mut clock = ExchangeClock::Paused { elapsed_ms: 5 };
        clock.resume(100);
        clock.resume(200);
        assert_eq!(clock.elapsed(250), 155);
        let mut restored: ExchangeClock =
            serde_json::from_value(serde_json::to_value(clock).unwrap()).unwrap();
        restored.pause(250);
        restored.pause(500);
        assert_eq!(restored.elapsed(1000), 155);
        restored.resume(1000);
        assert_eq!(restored.elapsed(1010), 165);
    }

    #[test]
    fn question_transitions_preserve_answers_and_external_waits() {
        let question = crate::plan::PlanElicitation::new(
            crate::plan::PlanQuestionSet::freeform("Choose a scope".into())
                .normalize()
                .unwrap(),
        );
        let mut state = ExchangeQuestion::None;
        state.set_waiting(true);
        assert!(state.waiting());
        assert!(state.elicitation().is_none());
        state.set_elicitation(Some(question));
        let identity = state.elicitation().unwrap().question_set.id.clone();
        state.set_waiting(false);
        assert!(!state.waiting());
        assert_eq!(state.elicitation().unwrap().question_set.id, identity);
        let mut restored: ExchangeQuestion =
            serde_json::from_value(serde_json::to_value(state).unwrap()).unwrap();
        restored.set_waiting(true);
        assert_eq!(
            restored.take_elicitation().unwrap().question_set.id,
            identity
        );
        assert!(restored.waiting());
        restored.set_waiting(false);
        assert!(matches!(restored, ExchangeQuestion::None));
    }
}
