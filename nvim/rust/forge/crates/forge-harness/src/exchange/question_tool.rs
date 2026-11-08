//! Stage authenticated question-tool effects until the owner has applied same-step lifecycle transitions.
use crate::provider::ScopeOwner;

use super::{
    question_event::{QuestionChange, QuestionCreation, QuestionEventError, QuestionToolCall},
    question_wait::QuestionWaits,
    services::OwnerServices,
};
use tokio::sync::oneshot;

/// Carry the owner's post-command admission decision and exact live invocation.
#[derive(Clone, Copy)]
pub(super) enum QuestionToolAdmission<'scope> {
    /// Cancellation, shutdown, or unavailable storage prevents a new effect.
    Closed,
    /// Admission is open; a missing live scope still rejects authenticated callbacks.
    Open {
        scope: Option<&'scope crate::provider::InvocationScope>,
    },
}

/// Retain one tool callback without granting mutation authority during event intake.
pub(super) enum QuestionTool {
    /// Apply a model-owned revision, reply, or withdrawal after validating its original call.
    Change {
        change: QuestionChange,
        reply: oneshot::Sender<Result<crate::question::QuestionSnapshot, QuestionEventError>>,
    },
    /// Publish asynchronous questions under the original call's receipt.
    Create {
        creation: QuestionCreation,
        reply: oneshot::Sender<Result<Vec<crate::question::QuestionId>, QuestionEventError>>,
    },
    /// Publish and observe required questions without tying their lifetime to the caller future.
    Wait {
        creation: QuestionCreation,
        reply: oneshot::Sender<Result<crate::question::OwnedQuestionWait, QuestionEventError>>,
    },
}

impl QuestionTool {
    /// Defer a waiting destination while letting replies and other callbacks make progress.
    /// A closed admission gate must still consume and reject the original callback promptly.
    pub(super) fn waits_for_capacity(&self, waits: &QuestionWaits) -> bool {
        matches!(self, Self::Wait { .. }) && !waits.has_capacity()
    }

    /// Reject excess staged work without dropping its caller's completion observation.
    pub(super) fn reject(self) {
        match self {
            Self::Change { reply, .. } => {
                let _ = reply.send(Err(QuestionEventError::AdmissionClosed));
            }
            Self::Create { reply, .. } => {
                let _ = reply.send(Err(QuestionEventError::AdmissionClosed));
            }
            Self::Wait { reply, .. } => {
                let _ = reply.send(Err(QuestionEventError::AdmissionClosed));
            }
        }
    }

    /// Replay committed results first, then admit only a live invocation through the current owner gate.
    pub(super) fn apply(
        self,
        admission: QuestionToolAdmission<'_>,
        services: &OwnerServices,
        waits: &mut QuestionWaits,
        trace: &crate::trace::TraceStore,
        session: &str,
        step: u64,
    ) -> bool {
        match self {
            Self::Change { change, reply } => {
                let operation = change.operation_id().to_owned();
                trace_call(change.tool_call(), &operation, trace, session, step);
                let result = match change.replay(&services.questions) {
                    Ok(Some(snapshot)) => Ok(snapshot),
                    Ok(None) => validate_new_effect(change.tool_call(), admission, services)
                        .and_then(|()| change.apply(&services.questions, &services.tasks)),
                    Err(error) => Err(error),
                };
                respond(reply, result, &operation, trace, session, step)
            }
            Self::Create { creation, reply } => {
                let operation = creation.operation_id().to_owned();
                trace_call(creation.call(), &operation, trace, session, step);
                let result = match creation.replay(&services.questions) {
                    Ok(Some(ids)) => Ok(ids),
                    Ok(None) => validate_new_effect(creation.call(), admission, services)
                        .and_then(|()| creation.publish(&services.questions, services)),
                    Err(error) => Err(error),
                };
                respond(reply, result, &operation, trace, session, step)
            }
            Self::Wait { creation, reply } => {
                let operation = creation.operation_id().to_owned();
                trace_call(creation.call(), &operation, trace, session, step);
                let result = match creation.replay_wait(&services.questions) {
                    Ok(Some(_)) => waits.begin(creation, services, trace, session, step),
                    Ok(None) => validate_new_effect(creation.call(), admission, services)
                        .and_then(|()| waits.begin(creation, services, trace, session, step)),
                    Err(error) => Err(error),
                };
                respond(reply, result, &operation, trace, session, step)
            }
        }
    }
}

/// Authenticate the exact live source after cancellation/configuration has had its admission opportunity.
fn validate_new_effect(
    call: Option<&QuestionToolCall>,
    admission: QuestionToolAdmission<'_>,
    services: &OwnerServices,
) -> Result<(), QuestionEventError> {
    let QuestionToolAdmission::Open { scope: live_scope } = admission else {
        return Err(QuestionEventError::AdmissionClosed);
    };
    let Some(call) = call else {
        return if cfg!(test) {
            Ok(())
        } else {
            Err(QuestionEventError::ForeignScope)
        };
    };
    let store = crate::storage::SqliteStore::read_only(&services.root)
        .map_err(QuestionEventError::State)?;
    let source = store
        .load_exchange(call.scope().exchange_id.as_str())
        .map_err(QuestionEventError::State)?
        .ok_or(QuestionEventError::TargetMissing)?;
    validate_live_source(
        call.scope(),
        live_scope,
        services.observer.session_id(),
        &source,
    )
}

/// Authenticate against host admission without waiting for native turn observations to be persisted.
fn validate_live_source(
    scope: &crate::provider::InvocationScope,
    live_scope: Option<&crate::provider::InvocationScope>,
    session: &str,
    source: &super::Exchange,
) -> Result<(), QuestionEventError> {
    // A fast callback can reach owner intake before its native TurnStarted observation.
    if live_scope != Some(scope)
        || scope.owner().session_id().as_str() != session
        || source.session_id != session
    {
        return Err(QuestionEventError::ForeignScope);
    }
    if source.is_finished() {
        return Err(QuestionEventError::SourceFinished);
    }
    Ok(())
}

/// Log the authenticated tool provenance without trusting arguments to supply an invocation identity.
fn trace_call(
    call: Option<&QuestionToolCall>,
    operation: &str,
    trace: &crate::trace::TraceStore,
    session: &str,
    step: u64,
) {
    trace.record(session, "exchange.question.tool_admission", serde_json::json!({
        "operation_id":operation,"scope":call.map(QuestionToolCall::scope),"call_id":call.map(QuestionToolCall::call_id),"update_step":step,
    }));
}

/// Retain the original response category in correlated logs even when its observer has disconnected.
fn respond<T>(
    reply: oneshot::Sender<Result<T, QuestionEventError>>,
    result: Result<T, QuestionEventError>,
    operation: &str,
    trace: &crate::trace::TraceStore,
    session: &str,
    step: u64,
) -> bool {
    let changed = result.is_ok();
    let code = result.as_ref().err().map(QuestionEventError::code);
    let observed = reply.send(result).is_ok();
    trace.record(session, "exchange.question.response", serde_json::json!({"operation_id":operation,"observed":observed,"error_code":code,"update_step":step}));
    changed
}
