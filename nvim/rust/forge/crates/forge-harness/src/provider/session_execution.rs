//! Reduce delivery and native execution independently under one explicit owner.

mod cleanup;
mod inbox;
pub(crate) use cleanup::{
    CleanupError, CleanupWorkFailure, CleanupWorkKind, RetainedCleanupFailure,
};
mod runtime;
pub(crate) use inbox::{ObservationEnd, SessionObservationInbox};
pub(crate) use runtime::{
    ContinuationAttempt, HostEffectAdmission, HostEffectGuard, HostEffectId, HostEffectKind,
    HostEffectOutput, NativeReady, OwnerCloseReason, RegistryError, ResultEffectAdmission,
    SessionExecution, SessionExecutionRegistry, SettlementCell, SettlementCheckout,
    SettlementError,
};

use std::collections::{HashMap, HashSet};

use forge_provider_api::lifecycle::{
    ExecutionObservation, ExecutionResult, ExecutionUpdate, LifecycleError, ObservationFailure,
    SubmissionId,
};

use super::{ExecutionOwner, InvocationId, InvocationScope, ProviderTurnId, TurnAddress};

/// Preserve the native delivery evidence without interpreting execution as acknowledgment.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DeliveryState {
    Pending,
    Accepted,
    NotConsumed,
    Uncertain,
}

/// Identify why an invocation is not ready for host settlement.
#[derive(Clone, Debug)]
pub(crate) enum ExecutionReadiness {
    Waiting,
    Complete,
    ObservationLost(ObservationFailure),
    DeliveryUncertain,
}

/// Own delivery and lifecycle state for all retained invocations of one runtime.
pub(crate) struct ExecutionState {
    owner: ExecutionOwner,
    latest: Option<InvocationId>,
    invocations: HashMap<InvocationId, InvocationState>,
}

struct InvocationState {
    scope: InvocationScope,
    submissions: HashMap<SubmissionId, DeliveryState>,
    executions: HashMap<(String, ProviderTurnId), NativeExecution>,
    without_execution: Vec<(ExecutionResult, HashSet<SubmissionId>)>,
    covered: HashSet<SubmissionId>,
    observation_failure: Option<ObservationFailure>,
}

enum NativeExecution {
    Running,
    Finished {
        result: Box<ExecutionResult>,
        covered: HashSet<SubmissionId>,
    },
}

impl ExecutionState {
    /// Create the sole reducer for one independently controlled provider owner.
    pub(crate) fn new(owner: ExecutionOwner) -> Self {
        Self {
            owner,
            latest: None,
            invocations: HashMap::new(),
        }
    }

    /// Register ownership and a pending input before any write can emit native events.
    pub(crate) fn admit(
        &mut self,
        scope: InvocationScope,
        submission: SubmissionId,
    ) -> Result<(), LifecycleError> {
        if scope.owner != self.owner {
            return Err(LifecycleError::OwnershipMismatch);
        }
        if let Some(state) = self.invocations.get(&scope.invocation_id) {
            if state.scope != scope {
                return Err(LifecycleError::OwnershipMismatch);
            }
            if state.submissions.contains_key(&submission) {
                return Err(LifecycleError::DuplicateSubmission(submission));
            }
        }
        self.latest = Some(scope.invocation_id.clone());
        self.invocations
            .entry(scope.invocation_id.clone())
            .or_insert_with(|| InvocationState {
                scope,
                submissions: HashMap::new(),
                executions: HashMap::new(),
                without_execution: Vec::new(),
                covered: HashSet::new(),
                observation_failure: None,
            })
            .submissions
            .insert(submission, DeliveryState::Pending);
        Ok(())
    }

    /// Get the most recently admitted invocation when its owner receives an unscoped control.
    pub(crate) fn latest_scope(&self) -> Option<&InvocationScope> {
        self.latest
            .as_ref()
            .and_then(|id| self.invocations.get(id))
            .map(|state| &state.scope)
    }

    /// Get the retained scope whose native work still occupies this owner.
    pub(crate) fn active_scope(&self) -> Option<&InvocationScope> {
        self.invocations
            .values()
            .find(|state| {
                state
                    .executions
                    .values()
                    .any(|execution| matches!(execution, NativeExecution::Running))
                    || state.submissions.iter().any(|(id, delivery)| {
                        matches!(delivery, DeliveryState::Pending | DeliveryState::Uncertain)
                            || *delivery == DeliveryState::Accepted && !state.covered.contains(id)
                    })
            })
            .map(|state| &state.scope)
    }

    /// Recognize retained original ownership after its foreground waiter has completed.
    pub(crate) fn contains(&self, scope: &InvocationScope) -> bool {
        self.invocations
            .get(&scope.invocation_id)
            .is_some_and(|state| &state.scope == scope)
    }

    /// Commit an input's evidence without allowing terminal execution to imply acceptance.
    pub(crate) fn delivery(
        &mut self,
        scope: &InvocationScope,
        submission: &SubmissionId,
        delivery: DeliveryState,
    ) -> Result<(), LifecycleError> {
        let state = self.invocation_mut(scope)?;
        let previous = state
            .submissions
            .get(submission)
            .ok_or_else(|| LifecycleError::UnknownSubmission(submission.clone()))?;
        if *previous != DeliveryState::Pending
            && *previous != delivery
            && !(*previous == DeliveryState::Uncertain && delivery == DeliveryState::Accepted)
        {
            return Err(LifecycleError::DeliveryConflict(submission.clone()));
        }
        state.submissions.insert(submission.clone(), delivery);
        Ok(())
    }

    /// Reduce a validated live event while preserving its original invocation.
    pub(crate) fn observe(
        &mut self,
        observation: ExecutionObservation,
    ) -> Result<(), LifecycleError> {
        let (scope, update) = observation.into_parts();
        let state = self.invocation_mut(&scope)?;
        match update {
            ExecutionUpdate::Started { address } => {
                state
                    .executions
                    .entry(Self::key(address))
                    .or_insert(NativeExecution::Running);
            }
            ExecutionUpdate::Finished {
                address,
                covered_submissions,
                result,
                ..
            } => {
                let key = Self::key(address);
                let execution = state
                    .executions
                    .get(&key)
                    .ok_or(LifecycleError::UnknownExecution)?;
                for id in &covered_submissions {
                    if !state.submissions.contains_key(id) {
                        return Err(LifecycleError::UnknownSubmission(id.clone()));
                    }
                }
                let covered: HashSet<_> = covered_submissions.iter().cloned().collect();
                if let NativeExecution::Finished {
                    result: previous,
                    covered: previous_covered,
                } = execution
                {
                    if previous.as_ref() != &result || previous_covered != &covered {
                        return Err(LifecycleError::TerminalConflict);
                    }
                    return Ok(());
                }
                state.executions.insert(
                    key,
                    NativeExecution::Finished {
                        result: Box::new(result),
                        covered,
                    },
                );
                state.covered.extend(covered_submissions);
            }
            ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions,
                result,
                ..
            } => {
                for id in &covered_submissions {
                    if !state.submissions.contains_key(id) {
                        return Err(LifecycleError::UnknownSubmission(id.clone()));
                    }
                }
                let covered: HashSet<_> = covered_submissions.iter().cloned().collect();
                for (previous, previous_covered) in &state.without_execution {
                    if previous_covered == &covered {
                        return if previous == &result {
                            Ok(())
                        } else {
                            Err(LifecycleError::TerminalConflict)
                        };
                    }
                    if !previous_covered.is_disjoint(&covered) {
                        return Err(LifecycleError::TerminalConflict);
                    }
                }
                state.without_execution.push((result, covered));
                state.covered.extend(covered_submissions);
            }
            ExecutionUpdate::ObservationLost { failure, .. } => {
                state.observation_failure.get_or_insert(failure);
            }
        }
        Ok(())
    }

    /// Report settlement eligibility only after delivery and matching terminal coverage settle.
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
            .any(|delivery| *delivery == DeliveryState::Uncertain)
        {
            return Ok(ExecutionReadiness::DeliveryUncertain);
        }
        if state
            .submissions
            .iter()
            .any(|(id, delivery)| match delivery {
                DeliveryState::Pending => true,
                DeliveryState::Accepted => !state.covered.contains(id),
                DeliveryState::NotConsumed => false,
                DeliveryState::Uncertain => true,
            })
            || state
                .executions
                .values()
                .any(|execution| matches!(execution, NativeExecution::Running))
        {
            return Ok(ExecutionReadiness::Waiting);
        }
        Ok(ExecutionReadiness::Complete)
    }

    /// Resolve one invocation without permitting aliases across owners or exchanges.
    fn invocation_mut(
        &mut self,
        scope: &InvocationScope,
    ) -> Result<&mut InvocationState, LifecycleError> {
        self.invocations
            .get_mut(&scope.invocation_id)
            .filter(|state| &state.scope == scope)
            .ok_or(LifecycleError::OwnershipMismatch)
    }

    /// Retain the native thread and turn together because turn IDs are not globally unique.
    fn key(address: TurnAddress) -> (String, ProviderTurnId) {
        (address.thread_id, address.turn_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::completion::ProviderCompletion;

    fn input(value: &str) -> SubmissionId {
        SubmissionId::try_from(value.to_owned()).unwrap()
    }

    fn address(scope: &InvocationScope) -> TurnAddress {
        TurnAddress {
            scope: scope.clone(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        }
    }

    fn finish(scope: &InvocationScope, covered: Vec<SubmissionId>) -> ExecutionObservation {
        ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Finished {
                address: address(scope),
                covered_submissions: covered,
                completed_at_ms: 1,
                result: ExecutionResult::Completed {
                    completion: ProviderCompletion::default().into(),
                },
            },
        )
        .unwrap()
    }

    /// A certified goal-only result still waits for delivery and rejects conflicting replay.
    #[test]
    fn no_execution_completion_requires_acknowledgment_and_rejects_conflicts() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "goal");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("goal")).unwrap();
        let result = ExecutionResult::Completed {
            completion: ProviderCompletion::default().into(),
        };
        let observation = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions: vec![input("goal")],
                completed_at_ms: 1,
                result: result.clone(),
            },
        )
        .unwrap();
        engine.observe(observation.clone()).unwrap();
        engine.observe(observation).unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
        engine
            .delivery(&scope, &input("goal"), DeliveryState::Accepted)
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Complete
        ));
        let conflict = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions: vec![input("goal")],
                completed_at_ms: 2,
                result: ExecutionResult::Cancelled {
                    completion: ProviderCompletion::default().into(),
                },
            },
        )
        .unwrap();
        assert!(matches!(
            engine.observe(conflict),
            Err(LifecycleError::TerminalConflict)
        ));
    }

    /// Distinct native turns cannot settle another steer before its own terminal and acknowledgment.
    #[test]
    fn separate_steers_require_each_native_turn_and_acknowledgment() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        for id in ["prompt", "first-steer", "second-steer"] {
            engine.admit(scope.clone(), input(id)).unwrap();
        }
        engine
            .delivery(&scope, &input("prompt"), DeliveryState::Accepted)
            .unwrap();
        let first = address(&scope);
        let mut second = first.clone();
        second.turn_id = "second".into();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: first.clone(),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        let terminal = |address, covered_submissions| {
            ExecutionObservation::new(
                scope.clone(),
                ExecutionUpdate::Finished {
                    address,
                    covered_submissions,
                    completed_at_ms: 1,
                    result: ExecutionResult::Completed {
                        completion: ProviderCompletion::default().into(),
                    },
                },
            )
            .unwrap()
        };
        engine
            .observe(terminal(first, vec![input("prompt"), input("first-steer")]))
            .unwrap();
        engine
            .delivery(&scope, &input("first-steer"), DeliveryState::Accepted)
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: second.clone(),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        engine
            .observe(terminal(second, vec![input("second-steer")]))
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
        engine
            .delivery(&scope, &input("second-steer"), DeliveryState::Accepted)
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Complete
        ));
    }

    /// A late acknowledgment cannot cause older terminal evidence to cover newer input.
    #[test]
    fn terminal_before_ack_keeps_uncovered_steering_pending() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("prompt")).unwrap();
        engine
            .delivery(&scope, &input("prompt"), DeliveryState::Accepted)
            .unwrap();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: address(&scope),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        engine.admit(scope.clone(), input("steer")).unwrap();
        engine
            .observe(finish(&scope, vec![input("prompt")]))
            .unwrap();
        engine
            .delivery(&scope, &input("steer"), DeliveryState::Accepted)
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
    }

    /// Terminal coverage can precede acknowledgment without losing either piece of evidence.
    #[test]
    fn terminal_before_ack_settles_only_after_matching_acceptance() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("prompt")).unwrap();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: address(&scope),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        engine
            .observe(finish(&scope, vec![input("prompt")]))
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Waiting
        ));
        engine
            .delivery(&scope, &input("prompt"), DeliveryState::Accepted)
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::Complete
        ));
    }

    /// Loss before turn startup is terminal for local waiters without proving native completion.
    #[test]
    fn loss_resolves_waiter_before_native_start() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("prompt")).unwrap();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::ObservationLost {
                        failure: ObservationFailure::Disconnected,
                        completion: None,
                    },
                )
                .unwrap(),
            )
            .unwrap();
        assert!(matches!(
            engine.readiness(&scope).unwrap(),
            ExecutionReadiness::ObservationLost(ObservationFailure::Disconnected)
        ));
    }
    /// Duplicate native terminal delivery must repeat the exact per-execution coverage set.
    #[test]
    fn terminal_duplicate_rejects_changed_coverage() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("prompt")).unwrap();
        engine.admit(scope.clone(), input("steer")).unwrap();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: address(&scope),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        engine
            .observe(finish(&scope, vec![input("prompt"), input("steer")]))
            .unwrap();
        engine
            .observe(finish(&scope, vec![input("steer"), input("prompt")]))
            .unwrap();
        assert_eq!(
            engine.observe(finish(&scope, vec![input("prompt")])),
            Err(LifecycleError::TerminalConflict)
        );
    }

    /// Completion metrics are part of duplicate evidence even though persistence omits them.
    #[test]
    fn terminal_duplicate_rejects_changed_transient_metrics() {
        let owner = ExecutionOwner::root("session");
        let scope = InvocationScope::new(owner.clone(), "exchange");
        let mut engine = ExecutionState::new(owner);
        engine.admit(scope.clone(), input("prompt")).unwrap();
        engine
            .observe(
                ExecutionObservation::new(
                    scope.clone(),
                    ExecutionUpdate::Started {
                        address: address(&scope),
                    },
                )
                .unwrap(),
            )
            .unwrap();
        engine
            .observe(finish(&scope, vec![input("prompt")]))
            .unwrap();
        let mut completion = ProviderCompletion::default();
        completion.metrics.native_compact_update = Some(true);
        let changed = ExecutionObservation::new(
            scope.clone(),
            ExecutionUpdate::Finished {
                address: address(&scope),
                covered_submissions: vec![input("prompt")],
                completed_at_ms: 1,
                result: ExecutionResult::Completed {
                    completion: completion.into(),
                },
            },
        )
        .unwrap();
        assert_eq!(
            engine.observe(changed),
            Err(LifecycleError::TerminalConflict)
        );
    }
}
