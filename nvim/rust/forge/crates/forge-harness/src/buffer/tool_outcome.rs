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

/// Choose a row's marker state: a call its tool deadline stopped has failed, whatever the
/// provider still reports.
///
/// Claude Code keeps reporting the stopped call as running until its turn ends, and the timer
/// refresh redrew the row from that state, so the row lost its failure marker beside
/// `Failed: timed out`. Every heading of a timed-out call takes this state.
pub(super) fn heading_state(
    exchange: &Exchange,
    turn: &crate::turn::Turn,
    tool: &crate::turn::ToolCall,
) -> crate::turn::ToolState {
    let stopped = exchange.tool_timeout.iter().any(|record| {
        record.thread_id == turn.provider().thread_id
            && record.turn_id == turn.provider().turn_id
            && record.item_id.as_str() == tool.id
    });
    if stopped {
        crate::turn::ToolState::Failed
    } else {
        tool.state()
    }
}

/// State that a tool deadline stopped the exchange, naming the call as its row does.
///
/// The record keeps the provider's raw title, which for Codex is the whole shell invocation;
/// the notice resolves the recorded call and names it through the row's own display rule.
pub(super) fn timeout_notice(
    exchange: &Exchange,
    record: &crate::provider::tool_timeout::ToolTimeoutRecord,
) -> String {
    let title = exchange
        .tools()
        .find(|(turn, call)| {
            turn.provider().thread_id == record.thread_id
                && turn.provider().turn_id == record.turn_id
                && call.id == record.item_id.as_str()
        })
        .map_or_else(
            || record.title.clone(),
            |(_, call)| {
                super::transcript::tool_display_title(
                    &call.kind,
                    &call.title,
                    call.command_shell.as_deref(),
                )
            },
        );
    format!(
        "Stopped: `{title}` exceeded its {} tool timeout.",
        record.timeout_label()
    )
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A timed-out call is a failed row, and its stop notice names the command as the row does,
    /// not by the provider's shell wrapper.
    ///
    /// Codex reports a command as its full shell invocation, so the notice read
    /// `"C:\…\pwsh.exe" -Command 'nu -c "sleep 45sec"'` beside a row that showed `sleep 45sec`.
    #[test]
    fn timed_out_call_is_failed_and_named_as_its_row_shows_it() -> anyhow::Result<()> {
        use forge_provider_api::ItemStart;
        let wrapper = r#""C:\Users\user\AppData\Local\Microsoft\WindowsApps\pwsh.exe" -Command 'nu -c "sleep 45sec"'"#;
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"held", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Run the held command", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let start = ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
            semantics: Default::default(),
            name: "shell".into(),
            title: wrapper.into(),
            arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({})),
        });
        crate::turn::test_support::tool(&mut exchange, &address, "held", start, Vec::new(), 1)?;
        let record = crate::provider::tool_timeout::ToolTimeoutRecord {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: forge_provider_api::ItemId::new("held")?,
            title: wrapper.into(),
            timeout_ms: 15_000,
        };
        let (turn, call) = exchange
            .tools()
            .next()
            .ok_or_else(|| anyhow::anyhow!("fixture has no tool"))?;
        assert_eq!(
            heading_state(&exchange, turn, call),
            crate::turn::ToolState::Running,
            "a call within its deadline keeps the provider's state"
        );
        exchange.tool_timeout.push(record.clone());
        let (turn, call) = exchange
            .tools()
            .next()
            .ok_or_else(|| anyhow::anyhow!("fixture has no tool"))?;
        assert_eq!(
            heading_state(&exchange, turn, call),
            crate::turn::ToolState::Failed,
            "a call its deadline stopped is failed while the provider still reports it running"
        );
        let (_, call) = exchange
            .tools()
            .next()
            .ok_or_else(|| anyhow::anyhow!("fixture has no tool"))?;
        let row = super::super::transcript::tool_display_title(
            &call.kind,
            &call.title,
            call.command_shell.as_deref(),
        );
        assert!(!row.contains("pwsh.exe"), "{row}");
        assert_eq!(
            timeout_notice(&exchange, &record),
            format!("Stopped: `{row}` exceeded its 15s tool timeout.")
        );
        Ok(())
    }
}
