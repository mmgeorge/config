mod blocked;
mod catalog;
mod complete;
mod status;

pub(in crate::tools) use catalog::{definitions, execute, is_tool, record};

#[cfg(test)]
mod tests {
    use super::*;

    /// Removing a rejected host control cannot leave behind fabricated native evidence.
    #[test]
    fn rejected_goal_control_preserves_only_genuine_native_evidence() -> crate::tools::ToolResult<()>
    {
        let task = crate::task::Task::goal("Goal".into(), Default::default(), 1);
        for (name, arguments, complete) in [
            ("goal_complete", serde_json::json!({"summary":"Done"}), true),
            (
                "goal_blocked",
                serde_json::json!({"reason":"Unavailable"}),
                false,
            ),
        ] {
            let invocation = crate::tools::ControlToolInvocation {
                call_id: None,
                name: name.into(),
                arguments,
                task_permit: Some(task.permit()),
            };
            for genuine in [false, true] {
                let mut output = crate::provider::ProviderCompletion::default();
                output.evidence.structured_complete = genuine;
                if complete {
                    complete::record(&invocation, &mut output)?;
                } else {
                    blocked::record(&invocation, &mut output)?;
                }
                output.goal_control.take();
                let (evidence, _) = output.goal_evidence();
                assert_eq!(evidence.structured_complete, genuine);
                assert!(!evidence.structured_blocked);
            }
        }
        Ok(())
    }
}
