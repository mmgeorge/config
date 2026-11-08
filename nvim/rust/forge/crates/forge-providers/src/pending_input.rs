//! Adapter-private cancellation ownership before a retained input installs its native collector.

use forge_provider_api::{
    BindingId, FailureDisposition, InvocationScope, OperationId, ProviderError, ProviderFailure,
    ProviderFailureKind, ProviderOperation, ProviderPhase,
};
use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex, Weak},
};
use tokio::sync::watch;

/// Serialize exact cancellation with registration, including cancellation before the input's first poll.
pub(crate) struct PendingInputs {
    state: Mutex<Registry>,
}

impl PendingInputs {
    /// Create an empty preparation namespace without admitting native work.
    pub(crate) fn new() -> Self {
        Self {
            state: Mutex::new(Registry::default()),
        }
    }

    /// Own one candidate work future before Store.observe can spawn it.
    /// Every replay creates its own guard: dropping a discarded candidate cannot finish the original worker.
    pub(crate) fn register(
        &self,
        binding: &BindingId,
        scope: &InvocationScope,
        operation: &OperationId,
    ) -> PendingInput {
        let mut registry = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let prevention = if registry.closed.contains(binding) {
            Prevention::BindingClosed
        } else if registry
            .cancelled
            .contains(&(binding.clone(), scope.clone()))
        {
            Prevention::InvocationCancelled
        } else {
            Prevention::Open
        };
        let state = Arc::new(Candidate {
            phase: watch::channel(Phase::Queued).0,
            prevention: watch::channel(prevention).0,
        });
        let candidates = registry
            .candidates
            .entry((binding.clone(), scope.clone(), operation.clone()))
            .or_default();
        candidates.retain(|candidate| candidate.strong_count() != 0);
        candidates.push(Arc::downgrade(&state));
        PendingInput { state }
    }

    /// Fence the exact scope atomically with its candidate snapshot, then wait only for native handoff or no-future proof.
    /// An absent scope still leaves a sticky tombstone; unrelated and successor scopes remain admissible.
    pub(crate) async fn cancel(&self, binding: &BindingId, scope: &InvocationScope) {
        let candidates = {
            let mut registry = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            registry.cancelled.insert((binding.clone(), scope.clone()));
            registry.candidates.values_mut().for_each(|candidates| {
                candidates.retain(|candidate| candidate.strong_count() != 0)
            });
            registry
                .candidates
                .iter()
                .filter(|((candidate_binding, candidate_scope, _), _)| {
                    candidate_binding == binding && candidate_scope == scope
                })
                .flat_map(|(_, candidates)| candidates.iter().filter_map(Weak::upgrade))
                .inspect(|candidate| {
                    candidate.prevention.send_if_modified(|prevention| {
                        if *prevention != Prevention::Open {
                            return false;
                        }
                        *prevention = Prevention::InvocationCancelled;
                        true
                    });
                })
                .collect::<Vec<_>>()
        };
        wait_for_handoff(candidates).await;
    }

    /// Revoke all preparation on one exact binding before its native collection owner is inspected.
    pub(crate) async fn cancel_binding(&self, binding: &BindingId) {
        self.fence_binding(binding).await;
    }

    /// Fence preparation synchronously, allowing native RPC retirement before awaiting queued handoff.
    /// The returned waiter owns only metadata and never holds the registry mutex across native cleanup.
    pub(crate) fn fence_binding(
        &self,
        binding: &BindingId,
    ) -> impl std::future::Future<Output = ()> + Send + 'static + use<> {
        let candidates = {
            let mut registry = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            registry.closed.insert(binding.clone());
            registry.candidates.values_mut().for_each(|candidates| {
                candidates.retain(|candidate| candidate.strong_count() != 0)
            });
            registry
                .candidates
                .iter()
                .filter(|((candidate_binding, _, _), _)| candidate_binding == binding)
                .flat_map(|(_, candidates)| candidates.iter().filter_map(Weak::upgrade))
                .inspect(|candidate| {
                    candidate.prevention.send_replace(Prevention::BindingClosed);
                })
                .collect::<Vec<_>>()
        };
        wait_for_handoff(candidates)
    }
}

/// Uniquely own the queued phase until a stable native cancellation/collection slot is installed.
pub(crate) struct PendingInput {
    state: Arc<Candidate>,
}

impl PendingInput {
    /// Get whether exact cancellation or binding close has revoked this preparation.
    pub(crate) fn is_cancelled(&self) -> bool {
        *self.state.prevention.borrow() != Prevention::Open
    }

    /// Wake preparation awaits without discarding any already-installed native collector.
    pub(crate) async fn cancelled(&self) {
        let mut changed = self.state.prevention.subscribe();
        loop {
            if *changed.borrow_and_update() != Prevention::Open {
                return;
            }
            changed
                .changed()
                .await
                .expect("pending preparation retains its cancellation sender");
        }
    }

    /// Reject prevented native work before mutation while preserving the Store's authoritative replay checks.
    pub(crate) fn check_admission(
        &self,
        operation: ProviderOperation,
    ) -> Result<(), ProviderError> {
        match *self.state.prevention.borrow() {
            Prevention::Open => Ok(()),
            Prevention::InvocationCancelled => Err(ProviderError::PreparationFailed(Box::new(
                ProviderFailure::new(operation, ProviderFailureKind::Cancelled)
                    .at_phase(ProviderPhase::Admission, FailureDisposition::Unchanged)
                    .into(),
            ))),
            Prevention::BindingClosed => Err(ProviderFailure::new(
                operation,
                ProviderFailureKind::BindingClosed,
            )
            .at_phase(ProviderPhase::Admission, FailureDisposition::Invalidated)
            .into()),
        }
    }

    /// Publish handoff only after exact cancellation and collection ownership exists, before native mutation.
    /// Cancellation waits must never depend on the later native acknowledgement or this worker finishing.
    pub(crate) fn registered(&self) {
        self.state.phase.send_if_modified(|phase| {
            if *phase != Phase::Queued {
                return false;
            }
            *phase = Phase::Registered;
            true
        });
    }
}

impl Drop for PendingInput {
    /// Prove this candidate can no longer install native ownership when its work exits or is rejected unpolled.
    fn drop(&mut self) {
        self.state.phase.send_if_modified(|phase| {
            if *phase != Phase::Queued {
                return false;
            }
            *phase = Phase::NoFutureRegistration;
            true
        });
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Phase {
    Queued,
    Registered,
    NoFutureRegistration,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Prevention {
    Open,
    InvocationCancelled,
    BindingClosed,
}

struct Candidate {
    phase: watch::Sender<Phase>,
    prevention: watch::Sender<Prevention>,
}

#[derive(Default)]
struct Registry {
    candidates: HashMap<(BindingId, InvocationScope, OperationId), Vec<Weak<Candidate>>>,
    cancelled: HashSet<(BindingId, InvocationScope)>,
    closed: HashSet<BindingId>,
}

/// Observe only preparation handoff; installed native work remains with its existing exact cancellation path.
async fn wait_for_handoff(candidates: Vec<Arc<Candidate>>) {
    for candidate in candidates {
        let mut phase = candidate.phase.subscribe();
        loop {
            if *phase.borrow_and_update() != Phase::Queued {
                break;
            }
            phase
                .changed()
                .await
                .expect("pending registry retains its phase sender");
        }
    }
}
