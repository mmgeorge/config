use crate::backend::BackendCompletion;
use crate::session::ApprovalMode;
use anyhow::Result;
use github_copilot_sdk::{SessionEvent, handler::PermissionResult};
use serde_json::Value;

/// Recognizes both native denial codes without treating execution failures as rejections.
pub(super) fn is_denied(data: &Value) -> bool {
    data.get("success").and_then(Value::as_bool) == Some(false)
        && matches!(
            data.pointer("/error/code").and_then(Value::as_str),
            Some("denied" | "rejected")
        )
}

/// Denies execution with feedback that the provider can return to the model.
pub(super) fn deny(reason: &str, approval_mode: Option<ApprovalMode>) -> PermissionResult {
    let guidance = if approval_mode == Some(ApprovalMode::Deny) {
        super::super::prompt::REJECT_ALL_GUIDANCE
    } else {
        ""
    };
    PermissionResult::reject(Some(format!(
        "Tool call rejected. The requested operation was not executed. Reason: {reason} \
         Do not retry or bypass this denial. Continue with permitted actions, or explain the blocker to the user. {guidance}"
    )))
}

/// Rejects provider success when a root tool denial has no subsequent final response.
#[derive(Default)]
pub(super) struct RejectionState {
    /// Latest root denial not followed by a final answer.
    pending: Option<String>,
    /// A final root response addressed every denial observed in this prompt.
    pub(super) answered: bool,
}

impl RejectionState {
    /// Tracks root denial and answer events without treating commentary as completion.
    pub(super) fn observe(&mut self, event: &SessionEvent) {
        if event.agent_id.is_some() {
            return;
        }
        match event.event_type.as_str() {
            "tool.execution_complete" if is_denied(&event.data) => {
                self.answered = false;
                self.pending = Some(
                    event
                        .data
                        .pointer("/error/message")
                        .and_then(Value::as_str)
                        .unwrap_or("The provider rejected the tool call.")
                        .to_owned(),
                );
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
                self.answered |= self.pending.is_some();
                self.pending = None;
            }
            _ => {}
        }
    }

    /// Requires an answer or control handoff after a denied operation.
    pub(super) fn finish(&self, output: &BackendCompletion) -> Result<()> {
        if output.evidence.structured_complete
            || output.evidence.structured_blocked
            || output.plan_submit.is_some()
            || output.plan_question.is_some()
        {
            return Ok(());
        }
        if let Some(reason) = &self.pending {
            anyhow::bail!(
                "Copilot stopped after a tool rejection without a final response. Provider reported: {reason}"
            );
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
        let output = BackendCompletion::default();
        state.observe(&event(
            "assistant.message",
            json!({"content":"Earlier answer","phase":"final_answer"}),
        ));
        state.observe(&rejected);
        assert!(
            state
                .finish(&output)
                .unwrap_err()
                .to_string()
                .contains("Denied by policy")
        );
        for kind in ["assistant.reasoning", "assistant.message_delta"] {
            state.observe(&event(kind, json!({"content":"Not a final response"})));
            assert!(state.finish(&output).is_err());
        }
        state.observe(&event(
            "assistant.message",
            json!({"content":"Continuing","phase":"commentary"}),
        ));
        assert!(state.finish(&output).is_err());
        let mut child = event(
            "assistant.message",
            json!({"content":"Child answer","phase":"final_answer"}),
        );
        child.agent_id = Some("child".into());
        state.observe(&child);
        assert!(state.finish(&output).is_err());
        state.observe(&event(
            "assistant.message",
            json!({"content":"Cannot run that command.","phase":"final_answer"}),
        ));
        assert!(state.finish(&output).is_ok());
        assert!(state.answered);
        state.observe(&rejected);
        assert!(!state.answered);
        let mut control = BackendCompletion::default();
        control.evidence.structured_complete = true;
        assert!(state.finish(&control).is_ok());
        control.evidence.structured_complete = false;
        control.evidence.structured_blocked = true;
        assert!(state.finish(&control).is_ok());
        control.evidence.structured_blocked = false;
        control.plan_submit = Some(crate::backend::PlanSubmitRequest {
            plan_id: "plan".into(),
            expected_version: 1,
        });
        assert!(state.finish(&control).is_ok());
        control.plan_submit = None;
        control.plan_question = Some(crate::plan::PlanQuestionSet {
            id: "questions".into(),
            questions: Vec::new(),
        });
        assert!(state.finish(&control).is_ok());
        let mut child = rejected.clone();
        child.agent_id = Some("child".into());
        let mut root = RejectionState::default();
        root.observe(&child);
        assert!(root.finish(&output).is_ok());
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
                assert_eq!(
                    state.finish(&BackendCompletion::default()).is_err(),
                    expected
                );
            }
        }
    }
}
