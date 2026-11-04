use forge_provider_api::ProviderError;
use github_copilot_sdk::SessionEvent;
use serde_json::Value;

pub use super::decode::is_denied;

/// Rejects provider success when a root tool denial has no subsequent final response.
#[derive(Default)]
pub struct RejectionState {
    /// A root denial has not received a later final answer.
    pending: bool,
    /// A final root response addressed every denial observed in this prompt.
    pub answered: bool,
}

impl RejectionState {
    /// Tracks root denial and answer events without treating commentary as completion.
    pub fn observe(&mut self, event: &SessionEvent) {
        if event.agent_id.is_some() {
            return;
        }
        match event.event_type.as_str() {
            "tool.execution_complete" if is_denied(&event.data) => {
                self.answered = false;
                self.pending = true;
            }
            "assistant.message"
                if event
                    .data
                    .get("content")
                    .and_then(Value::as_str)
                    .is_some_and(|content| !content.trim().is_empty())
                    && event
                        .data
                        .get("toolRequests")
                        .and_then(Value::as_array)
                        .is_none_or(Vec::is_empty)
                    && matches!(
                        event.data.get("phase").and_then(Value::as_str),
                        None | Some("final_answer")
                    ) =>
            {
                self.answered |= self.pending;
                self.pending = false;
            }
            _ => {}
        }
    }

    /// Requires an answer or control handoff after a denied operation.
    pub fn finish(&self, control_handoff: bool) -> Result<(), ProviderError> {
        if control_handoff {
            return Ok(());
        }
        if self.pending {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::Rejected,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn event(kind: &str, data: Value) -> SessionEvent {
        serde_json::from_value(json!({
            "id":"event", "timestamp":"2026-10-27T12:00:00Z", "type":kind, "data":data
        }))
        .unwrap()
    }

    #[test]
    fn rejection_requires_a_later_final_response_or_control_handoff() {
        let rejected = event(
            "tool.execution_complete",
            json!({
                "success":false, "error":{"code":"rejected","message":"Denied by policy"}
            }),
        );
        let mut state = RejectionState::default();
        let output = false;
        state.observe(&event(
            "assistant.message",
            json!({"content":"Earlier answer","phase":"final_answer"}),
        ));
        state.observe(&rejected);
        assert!(state.finish(output).is_err());
        for kind in ["assistant.reasoning", "assistant.message_delta"] {
            state.observe(&event(kind, json!({"content":"Not a final response"})));
            assert!(state.finish(output).is_err());
        }
        state.observe(&event(
            "assistant.message",
            json!({"content":"Continuing","phase":"commentary"}),
        ));
        assert!(state.finish(output).is_err());
        let mut child = event(
            "assistant.message",
            json!({"content":"Child answer","phase":"final_answer"}),
        );
        child.agent_id = Some("child".into());
        state.observe(&child);
        assert!(state.finish(output).is_err());
        state.observe(&event(
            "assistant.message",
            json!({"content":"Cannot run that command.","phase":"final_answer"}),
        ));
        assert!(state.finish(output).is_ok());
        assert!(state.answered);
        state.observe(&rejected);
        assert!(!state.answered);
        assert!(state.finish(true).is_ok());
        let mut child = rejected.clone();
        child.agent_id = Some("child".into());
        let mut root = RejectionState::default();
        root.observe(&child);
        assert!(root.finish(output).is_ok());
    }

    #[test]
    fn native_denial_codes_share_one_classification() {
        for code in ["denied", "rejected", "execution", "timeout"] {
            for success in [false, true] {
                let data = json!({"success":success,"error":{"code":code,"message":"denial"}});
                let expected = !success && matches!(code, "denied" | "rejected");
                assert_eq!(is_denied(&data), expected);
                let mut state = RejectionState::default();
                state.observe(&event("tool.execution_complete", data));
                assert_eq!(state.finish(false).is_err(), expected);
            }
        }
    }
}
