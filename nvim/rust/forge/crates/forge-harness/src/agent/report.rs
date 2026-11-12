//! Recognize provider echoes of Harness-authored child-result input.
//!
//! Child outcomes reach the provider as `<agent_message type="final_answer">` elements without a
//! host submission, so a provider that echoes its input as a native user item cannot correlate the
//! echo by submission. The rendered block lets the exchange recognize those echoes as evidence of
//! input the parent's rows already show. Only the `final_answer` type counts: a child's own
//! `new_task` block is the child's input, not a report.

/// Report whether native user text is an echo of Harness-authored child-result input.
pub fn is_child_report_echo(text: &str) -> bool {
    let text = text.trim_start();
    text.starts_with(&format!("<{}>", forge_provider_api::forge::BLOCK_TAG))
        && text.contains("<agent_message type=\"final_answer\" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Recognize a rendered child continuation and leave user text alone.
    #[test]
    fn child_result_blocks_are_echoes() {
        let input = forge_provider_api::forge::TurnInput::child_continuation(vec![
            forge_provider_api::forge::AgentResult {
                agent: "/root/explore".into(),
                task_id: "t".into(),
                status: "completed".into(),
                response: Some("done".into()),
                error: None,
            },
        ])
        .unwrap();
        assert!(is_child_report_echo(
            &forge_provider_api::forge::input_block(&input)
        ));
        assert!(!is_child_report_echo(
            "<agent_message type=\"final_answer\" from=\"x\"> typed by a user"
        ));
    }
}
