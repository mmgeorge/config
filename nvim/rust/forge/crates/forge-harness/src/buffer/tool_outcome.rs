use crate::exchange::{Exchange, ExchangeEntry, ThoughtCall};
use crate::provider::{
    ApprovalDecisionSource, ApprovalScope, PermissionApprovalEvent, PermissionDecision,
};

/// Render the host's denial before falling back to a provider's terminal diagnostic.
pub(super) fn rejection(exchange: &Exchange, call: &ThoughtCall<'_>) -> Option<String> {
    for event in &exchange.approval_resolution {
        let PermissionApprovalEvent::Resolved(resolution) = event else {
            continue;
        };
        let ApprovalScope::Item {
            thread_id,
            turn_id,
            item_id,
        } = &resolution.scope
        else {
            continue;
        };
        if thread_id != &call.turn.provider().thread_id
            || turn_id != &call.turn.provider().turn_id
            || item_id.as_str() != call.tool.id
        {
            continue;
        }
        for target in &resolution.targets {
            match &target.decision {
                PermissionDecision::Deny { reason, .. } => {
                    let label = match target.source {
                        ApprovalDecisionSource::User => "Rejected by user",
                        ApprovalDecisionSource::SavedPolicy => "Denied by saved policy",
                        ApprovalDecisionSource::ApprovalMode => "Denied by approval mode",
                        ApprovalDecisionSource::Timeout => "Approval timed out",
                    };
                    return Some(format!("{label}: {}", reason.as_str()));
                }
                PermissionDecision::TimedOut => {
                    return Some("Approval timed out; automatically rejected.".into());
                }
                PermissionDecision::Allow { .. } => {}
            }
        }
    }
    let execution = exchange
        .entry
        .iter()
        .filter_map(|entry| match entry {
            ExchangeEntry::Commentary { commentary } => Some(commentary),
            _ => None,
        })
        .flat_map(|commentary| &commentary.tool)
        .filter(|tools| tools.turn_id == call.turn.id())
        .find_map(|tools| tools.store.execution(&call.tool.id))?;
    match execution.outcome.as_ref()? {
        crate::provider::ToolOutcome::Denied { reason } => {
            Some(format!("Denied: {}", reason.as_str()))
        }
        crate::provider::ToolOutcome::Failed { diagnostic } => {
            Some(format!("Failed: {}", diagnostic.message.as_str()))
        }
        crate::provider::ToolOutcome::Succeeded
        | crate::provider::ToolOutcome::Cancelled
        | crate::provider::ToolOutcome::Interrupted => None,
    }
}
