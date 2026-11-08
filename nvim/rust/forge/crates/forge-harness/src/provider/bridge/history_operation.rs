//! Retain immutable history requests and their original cancellation authority together.
use crate::provider::ScopeOwner;

use std::{
    collections::HashMap,
    future::Future,
    sync::{Arc, Mutex},
};

use crate::provider::ThreadOwner;
use forge_provider_api::{
    BindingId, Compaction, CompactionRequest, EventScope, OperationId, ProviderError,
    ProviderFailure, ProviderFailureKind, ProviderOperation, SessionForkRequest,
};
use tokio::sync::OnceCell;

use super::super::operation::{self, OperationControl};
use super::HostSession;

#[derive(Clone, Copy, Eq, PartialEq)]
enum HistoryKind {
    Fork,
    Compact,
}

impl HistoryKind {
    /// Get the operation category whose identity this record owns.
    fn operation(self) -> ProviderOperation {
        match self {
            Self::Fork => ProviderOperation::ThreadFork,
            Self::Compact => ProviderOperation::ThreadCompact,
        }
    }
}

#[derive(Clone)]
enum HistoryOperation {
    Fork(Arc<RetainedRequest<SessionForkRequest>>),
    Compact(Arc<RetainedRequest<PreparedCompaction>>),
}

impl HistoryOperation {
    /// Get the cancellation and deadline authority retained before preparation starts.
    fn control(&self) -> &OperationControl {
        match self {
            Self::Fork(request) => &request.control,
            Self::Compact(request) => &request.control,
        }
    }
}

enum HistoryState {
    Open(HashMap<OperationId, HistoryOperation>),
    Closed(HashMap<OperationId, HistoryOperation>),
}

struct RetainedRequest<T> {
    control: OperationControl,
    request: OnceCell<T>,
}

/// Bind compaction to the exact reader whose watermark must settle, including after replacement.
pub(super) struct CompactionTarget {
    host: Arc<HostSession>,
    owner: ThreadOwner,
    binding: BindingId,
}

impl CompactionTarget {
    /// Create a target while bridge admission still protects the binding and its reader.
    pub(super) fn new(host: Arc<HostSession>, owner: ThreadOwner, binding: BindingId) -> Self {
        Self {
            host,
            owner,
            binding,
        }
    }
}

struct PreparedCompaction {
    target: CompactionTarget,
    request: CompactionRequest,
}

/// Own each history operation's first prepared request until the provider bridge is collected.
pub(super) struct HistoryOperations {
    state: Mutex<HistoryState>,
}

impl Default for HistoryOperations {
    fn default() -> Self {
        Self {
            state: Mutex::new(HistoryState::Open(HashMap::new())),
        }
    }
}

impl HistoryOperations {
    /// Observe the original fork; retries never reload resources or substitute later settings.
    pub(super) async fn fork(
        &self,
        native: &dyn forge_provider_api::provider::Provider,
        trace: Arc<crate::trace::TraceStore>,
        core: &crate::exchange::core_log::CoreEventLog,
        id: OperationId,
        prepare: impl Future<Output = Result<SessionForkRequest, ProviderError>>,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        let operation = self.operation(id, HistoryKind::Fork)?;
        let HistoryOperation::Fork(retained) = operation else {
            unreachable!("operation kind was checked before returning the retained record")
        };
        let request = retained.request.get_or_try_init(|| prepare).await?;
        let context = retained.control.context();
        operation::observe(
            trace,
            core,
            ProviderOperation::ThreadFork,
            &context,
            EventScope::SessionOperation {
                thread_id: forge_provider_api::ThreadId::from(request.target.owner().clone()),
                operation_id: context.id().clone(),
            },
            None,
            native.thread_fork(context.clone(), request.clone()),
        )
        .await
    }

    /// Observe compaction and consume its original reader through the returned watermark.
    pub(super) async fn compact(
        &self,
        native: &dyn forge_provider_api::provider::Provider,
        trace: Arc<crate::trace::TraceStore>,
        core: &crate::exchange::core_log::CoreEventLog,
        id: OperationId,
        prepare: impl Future<Output = Result<CompactionTarget, ProviderError>>,
    ) -> Result<Compaction, ProviderError> {
        let operation = self.operation(id, HistoryKind::Compact)?;
        let HistoryOperation::Compact(retained) = operation else {
            unreachable!("operation kind was checked before returning the retained record")
        };
        let context = retained.control.context();
        let prepared = retained
            .request
            .get_or_try_init(|| async {
                let target = prepare.await?;
                Ok::<_, ProviderError>(PreparedCompaction {
                    request: CompactionRequest {
                        scope: EventScope::SessionOperation {
                            thread_id: forge_provider_api::ThreadId::from(target.owner.clone()),
                            operation_id: context.id().clone(),
                        },
                    },
                    target,
                })
            })
            .await?;
        let result = operation::observe(
            trace,
            core,
            ProviderOperation::ThreadCompact,
            &context,
            prepared.request.scope.clone(),
            Some(prepared.target.binding.clone()),
            native.thread_compact(
                context.clone(),
                &prepared.target.binding,
                prepared.request.clone(),
            ),
        )
        .await?;
        tokio::select! {
            // Preserve already-consumed completion evidence when cancellation arrives afterward.
            biased;
            result = super::wait_for_watermark(&prepared.target.host, result.watermark) => result?,
            interruption = context.interrupted() => return Err(interruption.failure(
                ProviderOperation::ThreadCompact,
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            )),
        }
        Ok(result.compaction)
    }

    /// Close preparation admission and interrupt every retained record before bridge shutdown waits.
    pub(super) fn close(&self) {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let (HistoryState::Open(operations) | HistoryState::Closed(operations)) =
            std::mem::replace(&mut *state, HistoryState::Closed(HashMap::new()));
        for operation in operations.values() {
            operation.control().cancel();
        }
        *state = HistoryState::Closed(operations);
    }

    /// Reserve a typed record before awaiting preparation; reject cross-kind identity reuse first.
    fn operation(
        &self,
        id: OperationId,
        kind: HistoryKind,
    ) -> Result<HistoryOperation, ProviderError> {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let HistoryState::Open(operations) = &mut *state else {
            return Err(
                ProviderFailure::new(kind.operation(), ProviderFailureKind::BindingClosed)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Admission,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into(),
            );
        };
        if let Some(operation) = operations.get(&id) {
            if operation.control().operation() != kind.operation() {
                return Err(ProviderFailure::new(
                    kind.operation(),
                    ProviderFailureKind::OperationConflict,
                )
                .into());
            }
            return Ok(operation.clone());
        }
        let control = OperationControl::new(kind.operation(), id.clone())?;
        let operation = match kind {
            HistoryKind::Fork => HistoryOperation::Fork(Arc::new(RetainedRequest {
                control,
                request: OnceCell::new(),
            })),
            HistoryKind::Compact => HistoryOperation::Compact(Arc::new(RetainedRequest {
                control,
                request: OnceCell::new(),
            })),
        };
        operations.insert(id, operation.clone());
        Ok(operation)
    }
}
