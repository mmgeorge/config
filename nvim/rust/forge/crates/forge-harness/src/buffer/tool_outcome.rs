use crate::exchange::{Exchange, ExchangeEntry, ThoughtCall};
use crate::provider::{
    ApprovalCancellationReason, ApprovalDecisionSource, ApprovalScope, PermissionApprovalEvent,
    PermissionDecision,
};

/// Report whether this call's item is the one whose tool deadline stopped the exchange.
pub(super) fn timed_out<'exchange>(
    exchange: &'exchange Exchange,
    call: &ThoughtCall<'_>,
) -> Option<&'exchange crate::provider::tool_timeout::ToolTimeoutRecord> {
    exchange.tool_timeout.iter().find(|record| {
        record.thread_id == call.turn.provider().thread_id
            && record.turn_id == call.turn.provider().turn_id
            && record.item_id.as_str() == call.tool.id
    })
}

/// Explain how a call ended when it did not succeed, preferring what the host itself decided.
///
/// Harness knows why it stopped or refused a call: an elapsed tool deadline, its own approval
/// decision, or an approval it cancelled. Only when the host decided nothing does the line fall
/// back to the provider's terminal diagnostic, so a provider's generic denial never hides the
/// host's reason.
pub(super) fn outcome_line(exchange: &Exchange, call: &ThoughtCall<'_>) -> Option<String> {
    if let Some(record) = timed_out(exchange, call) {
        return Some(format!(
            "Failed: timed out after {}; execution stopped.",
            record.timeout_label()
        ));
    }
    for event in &exchange.approval_resolution {
        let (scope, line) = match event {
            PermissionApprovalEvent::Resolved(resolution) => {
                let line = resolution
                    .targets
                    .iter()
                    .find_map(|target| match &target.decision {
                        PermissionDecision::Deny { reason, .. } => {
                            let label = match target.source {
                                ApprovalDecisionSource::User => "Rejected by user",
                                ApprovalDecisionSource::SavedPolicy => "Denied by Harness policy",
                                ApprovalDecisionSource::ApprovalMode => "Denied by approval mode",
                                ApprovalDecisionSource::Timeout => "Approval timed out",
                            };
                            Some(format!("{label}: {}", reason.as_str()))
                        }
                        PermissionDecision::TimedOut => {
                            Some("Approval timed out; automatically rejected.".into())
                        }
                        PermissionDecision::Allow { .. } => None,
                    });
                (&resolution.scope, line)
            }
            PermissionApprovalEvent::Cancelled(cancellation) => (
                &cancellation.scope,
                Some(cancellation_line(&cancellation.reason).into()),
            ),
            PermissionApprovalEvent::Requested(_) => continue,
        };
        let ApprovalScope::Item {
            thread_id,
            turn_id,
            item_id,
        } = scope
        else {
            continue;
        };
        if thread_id != &call.turn.provider().thread_id
            || turn_id != &call.turn.provider().turn_id
            || item_id.as_str() != call.tool.id
        {
            continue;
        }
        if line.is_some() {
            return line;
        }
    }
    match execution(exchange, call).and_then(|execution| execution.outcome.as_ref()) {
        Some(crate::provider::ToolOutcome::Denied { reason }) => {
            Some(format!("Denied: {}", reason.as_str()))
        }
        Some(crate::provider::ToolOutcome::Failed { diagnostic }) => {
            Some(format!("Failed: {}", diagnostic.message.as_str()))
        }
        Some(
            crate::provider::ToolOutcome::Cancelled | crate::provider::ToolOutcome::Interrupted,
        ) => Some("Interrupted before it finished.".into()),
        Some(crate::provider::ToolOutcome::Succeeded) => None,
        None => matches!(
            call.tool.state(),
            crate::turn::ToolState::Cancelled | crate::turn::ToolState::Interrupted
        )
        .then(|| "Interrupted before it finished.".into()),
    }
}

/// Find the provider-reported execution of `call` in its own turn.
pub(super) fn execution<'exchange>(
    exchange: &'exchange Exchange,
    call: &ThoughtCall<'_>,
) -> Option<&'exchange crate::turn::ToolExecution> {
    exchange
        .entry
        .iter()
        .filter_map(|entry| match entry {
            ExchangeEntry::Commentary { commentary } => Some(commentary),
            _ => None,
        })
        .flat_map(|commentary| &commentary.tool)
        .filter(|tools| tools.turn_id == call.turn.id())
        .find_map(|tools| tools.store.execution(&call.tool.id))
}

/// Name who ended a pending approval without a permission decision.
fn cancellation_line(reason: &ApprovalCancellationReason) -> &'static str {
    match reason {
        ApprovalCancellationReason::UserCancelled => "Cancelled by you; the turn stopped.",
        ApprovalCancellationReason::TurnInterrupted => {
            "Approval cancelled because the turn was interrupted."
        }
        ApprovalCancellationReason::ThreadClosed => {
            "Approval cancelled because the session closed."
        }
        ApprovalCancellationReason::RequesterGone => {
            "Approval cancelled because the provider withdrew the request."
        }
        ApprovalCancellationReason::PersistenceFailed(_) => {
            "Approval cancelled because Harness could not save the decision."
        }
    }
}
