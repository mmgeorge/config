//! Reduce admitted input attempts and the sole native turn completion.
use crate::provider::ScopeOwner;
mod cleanup;
mod inbox;
mod runtime;
pub(crate) use cleanup::{
    CleanupError, CleanupJoinFailure, CleanupWorkFailure, CleanupWorkKind, RetainedCleanupFailure,
};
pub(crate) use inbox::{NativeAvailabilityReceipt, ObservationEnd, SessionObservationInbox};
pub(crate) use runtime::{
    ContinuationAttempt, ExchangeCollection, HostEffectAdmission, HostEffectGuard, HostEffectId,
    HostEffectKind, HostEffectOutput, NativeReady, OwnerCloseReason, RegistryError,
    ResultEffectAdmission, SessionExecution, SessionExecutionRegistry, SettlementCell,
    SettlementCheckout, SettlementError,
};

use super::{InvocationId, InvocationScope, ThreadOwner};
use forge_provider_api::{
    CompletedTurn, DeliveryAttemptId, ThreadId, TurnId,
    lifecycle::{LifecycleError, ObservationFailure, SubmissionId},
};
use std::collections::{HashMap, HashSet};

/// Deliver original-owner activity or an actually committed input receipt to the session owner.
#[derive(Clone, Debug)]
pub enum OwnerObservation {
    /// Apply native or host activity through its canonical reducer.
    Event(super::HarnessEvent),
    /// Persist a committed physical child effect independently of native or foreground lifetimes.
    HostFileChange {
        binding_id: forge_provider_api::BindingId,
        scope: InvocationScope,
        change: crate::tools::lsp::HostFileChange,
    },
    /// Apply input presentation only after the original provider acceptance succeeded.
    InputAcceptance {
        scope: InvocationScope,
        receipt: std::sync::Arc<super::submission::Submission>,
        presentation: InputPresentation,
    },
}
impl OwnerObservation {
    /// Get the immutable owner captured before input dispatch or event publication.
    pub fn scope(&self) -> super::EventScope {
        match self {
            Self::Event(event) => event.scope().clone(),
            Self::InputAcceptance { scope, .. } | Self::HostFileChange { scope, .. } => {
                scope.clone().into()
            }
        }
    }
    /// Measure only serializable observations without exposing receipt capabilities to diagnostics.
    pub(crate) fn encoded_event_bytes(&self) -> Option<usize> {
        match self {
            Self::Event(event) => serde_json::to_vec(event).ok().map(|encoded| encoded.len()),
            Self::HostFileChange { change, .. } => {
                serde_json::to_vec(change).ok().map(|encoded| encoded.len())
            }
            Self::InputAcceptance { .. } => None,
        }
    }
    /// Get the native timestamp without assigning lifecycle meaning to host receipts.
    pub fn observed_at_ms(&self, fallback: i64) -> i64 {
        match self {
            Self::Event(event) => event.observed_at_ms(fallback),
            Self::InputAcceptance { .. } | Self::HostFileChange { .. } => fallback,
        }
    }
}
/// Preserve the user-visible role of an accepted input independently of provider notifications.
#[derive(Clone, Debug)]
pub enum InputPresentation {
    /// Append an explicit user addition to its original exchange.
    Steering { text: String },
    /// Append the accepted workflow entry and its original instruction.
    TaskStarted {
        start: crate::exchange::TaskStart,
        prompt: String,
    },
}

/// Preserve delivery independently of native completion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DeliveryState {
    Pending,
    Accepted,
    NotConsumed,
    Uncertain,
}
/// Identify the outstanding settlement barrier.
#[derive(Clone, Debug)]
pub(crate) enum ExecutionReadiness {
    Waiting,
    Complete,
    ObservationLost(ObservationFailure),
    DeliveryUncertain,
}
/// Retain original ownership and exact attempted-input coverage.
pub(crate) struct ExecutionState {
    owner: ThreadOwner,
    latest: Option<InvocationId>,
    invocations: HashMap<InvocationId, InvocationState>,
}
struct InvocationState {
    scope: InvocationScope,
    submissions: HashMap<SubmissionId, InputState>,
    turn: NativeTurnState,
    covered: HashSet<SubmissionId>,
    observation_failure: Option<ObservationFailure>,
}
/// Preserve the distinction between native lifecycle and an operation collected before native start.
enum NativeTurnState {
    /// Retain admission while no positive native work has been observed.
    Preparing,
    /// Retain positively observed native work until its sole completed result.
    Running,
    /// Preserve the authoritative collected native outcome.
    Completed(CompletedTurn),
    /// Preserve confirmed operation cancellation without inventing a native turn.
    NotStarted,
}

struct InputState {
    group_id: SubmissionId,
    attempt_id: DeliveryAttemptId,
    delivery: DeliveryState,
}
impl ExecutionState {
    /// Create one reducer for an independently controlled owner.
    pub(crate) fn new(owner: ThreadOwner) -> Self {
        Self {
            owner,
            latest: None,
            invocations: HashMap::new(),
        }
    }
    /// Register an independently delivered journal input before native dispatch.
    pub(crate) fn admit(
        &mut self,
        scope: InvocationScope,
        submission: SubmissionId,
        attempt_id: DeliveryAttemptId,
    ) -> Result<(), LifecycleError> {
        self.admit_group(scope, submission, attempt_id, &[])
    }
    /// Register recovered logical inputs under their original attempts and one atomic receipt group.
    pub(crate) fn admit_group(
        &mut self,
        scope: InvocationScope,
        submission: SubmissionId,
        attempt_id: DeliveryAttemptId,
        included: &[forge_provider_api::CoveredInput],
    ) -> Result<(), LifecycleError> {
        // Validate the complete receipt group before publishing any admission.
        if scope.owner() != self.owner {
            return Err(LifecycleError::OwnershipMismatch);
        }
        let mut inputs = vec![forge_provider_api::CoveredInput::new(
            submission.clone(),
            attempt_id,
        )];
        inputs.extend_from_slice(included);
        let mut identities = HashSet::new();
        for input in &inputs {
            if !identities.insert(input.submission_id()) {
                return Err(LifecycleError::DuplicateSubmission(
                    input.submission_id().clone(),
                ));
            }
        }
        if let Some(state) = self.invocations.get(&scope.invocation_id) {
            if state.scope != scope
                || matches!(
                    state.turn,
                    NativeTurnState::Completed(_) | NativeTurnState::NotStarted
                )
            {
                return Err(LifecycleError::OwnershipMismatch);
            }
            for input in &inputs {
                if state.submissions.contains_key(input.submission_id()) {
                    return Err(LifecycleError::DuplicateSubmission(
                        input.submission_id().clone(),
                    ));
                }
            }
        }
        // Commit all original identities together under the retained invocation.
        self.latest = Some(scope.invocation_id.clone());
        let state = self
            .invocations
            .entry(scope.invocation_id.clone())
            .or_insert_with(|| InvocationState {
                scope,
                submissions: HashMap::new(),
                turn: NativeTurnState::Preparing,
                covered: HashSet::new(),
                observation_failure: None,
            });
        for input in inputs {
            state.submissions.insert(
                input.submission_id().clone(),
                InputState {
                    group_id: submission.clone(),
                    attempt_id: input.attempt_id().clone(),
                    delivery: DeliveryState::Pending,
                },
            );
        }
        Ok(())
    }
    /// Get the most recently admitted scope for owner-scoped controls.
    pub(crate) fn latest_scope(&self) -> Option<&InvocationScope> {
        self.latest
            .as_ref()
            .and_then(|id| self.invocations.get(id))
            .map(|state| &state.scope)
    }
    /// Get the original scope while native work or input remains unresolved.
    pub(crate) fn active_scope(&self) -> Option<&InvocationScope> {
        self.invocations
            .values()
            .find(|state| {
                matches!(state.turn, NativeTurnState::Running)
                    || state
                        .submissions
                        .iter()
                        .any(|(id, input)| match input.delivery {
                            DeliveryState::Pending | DeliveryState::Uncertain => true,
                            DeliveryState::Accepted => {
                                !matches!(state.turn, NativeTurnState::NotStarted)
                                    && !state.covered.contains(id)
                            }
                            DeliveryState::NotConsumed => false,
                        })
            })
            .map(|state| &state.scope)
    }
    /// Recognize retained original ownership after foreground completion.
    pub(crate) fn contains(&self, scope: &InvocationScope) -> bool {
        self.invocations
            .get(&scope.invocation_id)
            .is_some_and(|state| &state.scope == scope)
    }
    /// Commit grouped acceptance while preserving each original input's native coverage requirement.
    pub(crate) fn delivery(
        &mut self,
        scope: &InvocationScope,
        submission: &SubmissionId,
        delivery: DeliveryState,
    ) -> Result<(), LifecycleError> {
        let state = self.invocation_mut(scope)?;
        let group_id = state
            .submissions
            .get(submission)
            .ok_or_else(|| LifecycleError::UnknownSubmission(submission.clone()))?
            .group_id
            .clone();
        for (id, input) in state
            .submissions
            .iter()
            .filter(|(_, input)| input.group_id == group_id)
        {
            if input.delivery != DeliveryState::Pending
                && input.delivery != delivery
                && !(input.delivery == DeliveryState::Uncertain
                    && delivery == DeliveryState::Accepted)
            {
                return Err(LifecycleError::DeliveryConflict(id.clone()));
            }
            if delivery == DeliveryState::Accepted
                && matches!(state.turn, NativeTurnState::Completed(_))
                && !state.covered.contains(id)
            {
                return Err(LifecycleError::TerminalConflict);
            }
        }
        for input in state
            .submissions
            .values_mut()
            .filter(|input| input.group_id == group_id)
        {
            input.delivery = delivery;
        }
        Ok(())
    }
    /// Validate a presentation receipt against the exact attempt already accepted by this owner.
    pub(crate) fn validate_acceptance(
        &self,
        scope: &InvocationScope,
        submission: &SubmissionId,
        attempt_id: &DeliveryAttemptId,
    ) -> Result<(), LifecycleError> {
        let state = self
            .invocations
            .get(&scope.invocation_id)
            .filter(|state| &state.scope == scope)
            .ok_or(LifecycleError::OwnershipMismatch)?;
        let input = state
            .submissions
            .get(submission)
            .ok_or_else(|| LifecycleError::UnknownSubmission(submission.clone()))?;
        if &input.attempt_id != attempt_id || input.delivery != DeliveryState::Accepted {
            return Err(LifecycleError::DeliveryConflict(submission.clone()));
        }
        Ok(())
    }

    /// Validate identity and immutable completion before consumption or mutation.
    pub(crate) fn validate_turn(
        &self,
        scope: &InvocationScope,
        thread_id: &ThreadId,
        turn_id: &TurnId,
        completed: Option<&CompletedTurn>,
    ) -> Result<(), LifecycleError> {
        let state = self
            .invocations
            .get(&scope.invocation_id)
            .filter(|state| &state.scope == scope)
            .ok_or(LifecycleError::OwnershipMismatch)?;
        if &scope.owner().thread_id() != thread_id
            || scope
                .turn_id()
                .map_err(|_| LifecycleError::OwnershipMismatch)?
                != *turn_id
        {
            return Err(LifecycleError::OwnershipMismatch);
        }
        if let Some(completed) = completed {
            match &state.turn {
                NativeTurnState::Preparing | NativeTurnState::NotStarted => {
                    return Err(LifecycleError::UnknownExecution);
                }
                NativeTurnState::Completed(previous) if previous != completed => {
                    return Err(LifecycleError::TerminalConflict);
                }
                NativeTurnState::Running | NativeTurnState::Completed(_) => {}
            }
            if state.submissions.iter().any(|(id, input)| {
                input.delivery == DeliveryState::Accepted
                    && !completed
                        .result()
                        .covered_inputs()
                        .iter()
                        .any(|covered| covered.submission_id() == id)
            }) {
                return Err(LifecycleError::TerminalConflict);
            }
            for covered in completed.result().covered_inputs() {
                let input = state
                    .submissions
                    .get(covered.submission_id())
                    .ok_or_else(|| {
                        LifecycleError::UnknownSubmission(covered.submission_id().clone())
                    })?;
                if &input.attempt_id != covered.attempt_id()
                    || input.delivery == DeliveryState::NotConsumed
                {
                    return Err(LifecycleError::DeliveryConflict(
                        covered.submission_id().clone(),
                    ));
                }
            }
        } else if matches!(
            state.turn,
            NativeTurnState::Completed(_) | NativeTurnState::NotStarted
        ) {
            return Err(LifecycleError::TerminalConflict);
        }
        Ok(())
    }
    /// Record positively observed native work independently of acceptance.
    pub(crate) fn started(
        &mut self,
        scope: &InvocationScope,
        thread_id: &ThreadId,
        turn_id: &TurnId,
    ) -> Result<(), LifecycleError> {
        self.validate_turn(scope, thread_id, turn_id, None)?;
        self.invocation_mut(scope)?.turn = NativeTurnState::Running;
        Ok(())
    }
    /// Retain the authoritative completion after exact attempt validation.
    pub(crate) fn completed(
        &mut self,
        scope: &InvocationScope,
        thread_id: &ThreadId,
        turn_id: &TurnId,
        completed: CompletedTurn,
    ) -> Result<(), LifecycleError> {
        self.validate_turn(scope, thread_id, turn_id, Some(&completed))?;
        let state = self.invocation_mut(scope)?;
        state.covered.extend(
            completed
                .result()
                .covered_inputs()
                .iter()
                .map(|input| input.submission_id().clone()),
        );
        state.turn = NativeTurnState::Completed(completed);
        Ok(())
    }
    /// Resolve only an exact collected operation whose watermark contained no positive native start.
    pub(crate) fn cancel_unstarted(
        &mut self,
        scope: &InvocationScope,
    ) -> Result<bool, LifecycleError> {
        let state = self.invocation_mut(scope)?;
        match state.turn {
            NativeTurnState::Preparing => {
                state.turn = NativeTurnState::NotStarted;
                Ok(true)
            }
            NativeTurnState::NotStarted => Ok(true),
            NativeTurnState::Running | NativeTurnState::Completed(_) => Ok(false),
        }
    }

    /// Identify collected no-start operations without assigning them a native outcome.
    pub(crate) fn not_started(&self, scope: &InvocationScope) -> bool {
        self.invocations
            .get(&scope.invocation_id)
            .is_some_and(|state| {
                &state.scope == scope && matches!(state.turn, NativeTurnState::NotStarted)
            })
    }

    /// Fence affected waits without inventing a native result.
    pub(crate) fn unavailable(&mut self, failure: ObservationFailure) {
        for state in self.invocations.values_mut() {
            state
                .observation_failure
                .get_or_insert_with(|| failure.clone());
        }
    }
    /// Require acceptance and exact terminal coverage before host settlement.
    pub(crate) fn readiness(
        &self,
        scope: &InvocationScope,
    ) -> Result<ExecutionReadiness, LifecycleError> {
        let state = self
            .invocations
            .get(&scope.invocation_id)
            .filter(|state| &state.scope == scope)
            .ok_or(LifecycleError::OwnershipMismatch)?;
        if let Some(failure) = &state.observation_failure {
            return Ok(ExecutionReadiness::ObservationLost(failure.clone()));
        }
        if state
            .submissions
            .values()
            .any(|input| input.delivery == DeliveryState::Uncertain)
        {
            return Ok(ExecutionReadiness::DeliveryUncertain);
        }
        if matches!(state.turn, NativeTurnState::Running)
            || state
                .submissions
                .iter()
                .any(|(id, input)| match input.delivery {
                    DeliveryState::Pending | DeliveryState::Uncertain => true,
                    DeliveryState::Accepted => {
                        !matches!(state.turn, NativeTurnState::NotStarted)
                            && !state.covered.contains(id)
                    }
                    DeliveryState::NotConsumed => false,
                })
        {
            return Ok(ExecutionReadiness::Waiting);
        }
        Ok(ExecutionReadiness::Complete)
    }
    /// Resolve a retained invocation without accepting owner aliases.
    fn invocation_mut(
        &mut self,
        scope: &InvocationScope,
    ) -> Result<&mut InvocationState, LifecycleError> {
        self.invocations
            .get_mut(&scope.invocation_id)
            .filter(|state| &state.scope == scope)
            .ok_or(LifecycleError::OwnershipMismatch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{CoveredInput, TurnResult, turn::TurnOutcome};
    /// Create exact terminal coverage for a retained attempt.
    fn completion(attempt: &str) -> CompletedTurn {
        crate::provider::test_support::completed_turn(
            InvocationScope::new(ThreadOwner::root("fixture"), "exchange"),
            TurnResult::new(
                TurnOutcome::Interrupted,
                10,
                None,
                vec![CoveredInput::new(
                    "input".try_into().unwrap(),
                    DeliveryAttemptId::new(attempt).unwrap(),
                )],
                vec![],
                None,
            )
            .unwrap(),
        )
        .unwrap()
    }
    /// Completion cannot manufacture acceptance or cover a different delivery attempt.
    #[test]
    fn completion_waits_for_exact_input_acceptance() {
        let owner = ThreadOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let turn = scope.turn_id().unwrap();
        let thread = owner.thread_id();
        let mut state = ExecutionState::new(owner);
        state
            .admit(
                scope.clone(),
                "input".try_into().unwrap(),
                DeliveryAttemptId::new("attempt").unwrap(),
            )
            .unwrap();
        state.started(&scope, &thread, &turn).unwrap();
        assert!(
            state
                .completed(&scope, &thread, &turn, completion("other"))
                .is_err()
        );
        let completed = completion("attempt");
        state
            .completed(&scope, &thread, &turn, completed.clone())
            .unwrap();
        assert!(matches!(
            state.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
        state
            .delivery(
                &scope,
                &"input".try_into().unwrap(),
                DeliveryState::Accepted,
            )
            .unwrap();
        assert!(matches!(
            state.readiness(&scope).unwrap(),
            ExecutionReadiness::Complete
        ));
        state.completed(&scope, &thread, &turn, completed).unwrap();
        assert!(state.started(&scope, &thread, &turn).is_err());
    }

    /// Lost observation fences accepted input without certifying native completion.
    #[test]
    fn unavailable_does_not_certify_completion() {
        let owner = ThreadOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut state = ExecutionState::new(owner);
        state
            .admit(
                scope.clone(),
                "input".try_into().unwrap(),
                DeliveryAttemptId::new("attempt").unwrap(),
            )
            .unwrap();
        state.unavailable(ObservationFailure::Disconnected);
        assert!(matches!(
            state.readiness(&scope).unwrap(),
            ExecutionReadiness::ObservationLost(ObservationFailure::Disconnected)
        ));
    }
}
