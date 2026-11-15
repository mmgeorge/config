use super::host_event::{ApprovalScope, PermissionApprovalRequest};
use anyhow::Result;

/// Retain the expired requests separately from the instructions sent to their execution owner.
#[derive(Default)]
pub(crate) struct ApprovalTimeoutFeedback {
    feedback: String,
    request: Vec<PermissionApprovalRequest>,
}

impl ApprovalTimeoutFeedback {
    pub(super) fn request_push(&mut self, request: PermissionApprovalRequest) {
        if !self.feedback.is_empty() {
            self.feedback.push('\n');
        }
        self.feedback.push_str(&format!("Approval {} was automatically rejected because its timeout expired. This was not an explicit user rejection. The existing user instruction and permissions remain in effect. Continue authorized work, avoid repeating the expired request, and do not ask whether to continue work the user already requested.", request.request_id.as_str()));
        self.request.push(request);
    }

    pub(crate) fn feedback(&self) -> &str {
        &self.feedback
    }

    /// Format the human notice from original request records and their exact admitted tool items.
    pub(crate) fn display(&self, exchanges: &[crate::exchange::Exchange]) -> Result<String> {
        let mut display = String::new();
        for request in &self.request {
            if !display.is_empty() {
                display.push_str("\n\n");
            }
            display.push_str("Request\n  ");
            let question = request
                .reason
                .as_ref()
                .map(|reason| reason.as_str().to_owned())
                .unwrap_or_else(|| {
                    request
                        .targets
                        .iter()
                        .map(|target| target.title.as_str())
                        .collect::<Vec<_>>()
                        .join("; ")
                });
            display.push_str(&question.replace('\n', "\n  "));
            display.push_str("\n\nAction");
            for target in &request.targets {
                display.push_str(&format!(
                    "\n  {}\n    {}",
                    target.title,
                    target.detail.replace('\n', "\n    ")
                ));
                for command in &target.commands {
                    if command.source != target.detail {
                        display
                            .push_str(&format!("\n    {}", command.source.replace('\n', "\n    ")));
                    }
                }
            }
            if let ApprovalScope::Item {
                thread_id,
                turn_id,
                item_id,
            } = &request.scope
            {
                let execution = exchanges
                    .iter()
                    .flat_map(|exchange| &exchange.entry)
                    .filter_map(|entry| match entry {
                        crate::exchange::ExchangeEntry::Commentary { commentary } => {
                            Some(commentary)
                        }
                        _ => None,
                    })
                    .flat_map(|commentary| &commentary.tool)
                    .filter_map(|tools| tools.store.execution(item_id.as_str()))
                    .find(|execution| {
                        execution.address.thread_id == *thread_id
                            && execution.address.turn_id == *turn_id
                    });
                if let Some(execution) = execution {
                    display.push_str(&format!(
                        "\n  {}\n    {}",
                        execution.descriptor.name,
                        serde_json::to_string_pretty(execution.descriptor.arguments.value())?
                            .replace('\n', "\n    ")
                    ));
                    if !execution.input.is_empty() {
                        display.push_str(&format!(
                            "\n    {}",
                            execution.input.replace('\n', "\n    ")
                        ));
                    }
                }
            }
            display.push_str("\n\nAutomatically rejected when the timeout expired.\nNo user decision was recorded.");
        }
        Ok(display)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{
        ApprovalRequestId, ApprovalTargetId, PermissionApprovalTarget, ThreadOwner,
    };
    use forge_provider_api::permissions::PermissionTarget;

    fn request(id: &str) -> PermissionApprovalRequest {
        PermissionApprovalRequest {
            request_id: ApprovalRequestId::new(id).unwrap(),
            scope: ApprovalScope::Thread {
                thread_id: ThreadOwner::root("timeout").into(),
            },
            reason: Some(
                forge_provider_api::SanitizedMessage::new(
                    "Run the complete script?\nIt reports status.",
                )
                .unwrap(),
            ),
            targets: vec![PermissionApprovalTarget {
                target_id: ApprovalTargetId::new("command").unwrap(),
                target: PermissionTarget::Tool {
                    target: "shell".into(),
                },
                title: "Run command".into(),
                detail: "git status".into(),
                commands: vec![crate::provider::approval_view::ApprovalCommand {
                    source: "git status\ngit diff --stat".into(),
                    shell: forge_shell::shell::CommandShell::Bash,
                    focus_range_list: Vec::new(),
                    highlight_list: Vec::new(),
                }],
                choices: Vec::new(),
            }],
            deadline: None,
        }
    }

    #[test]
    fn retains_every_expired_request_and_its_complete_command() -> Result<()> {
        let mut feedback = ApprovalTimeoutFeedback::default();
        feedback.request_push(request("first"));
        feedback.request_push(request("second"));
        let display = feedback.display(&[])?;
        assert_eq!(display.matches("Request\n").count(), 2);
        assert_eq!(
            display
                .matches("Run the complete script?\n  It reports status.")
                .count(),
            2
        );
        assert_eq!(
            display.matches("git status\n    git diff --stat").count(),
            2
        );
        assert!(!display.contains("Continue authorized work"));
        assert!(feedback.feedback().contains("Approval first"));
        assert!(feedback.feedback().contains("Approval second"));
        assert!(!feedback.feedback().contains("git diff"));
        Ok(())
    }

    #[test]
    fn retains_arguments_from_the_original_item() -> Result<()> {
        let mut exchange = crate::storage::test::running_exchange("exchange");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "shell-item",
            forge_provider_api::ItemStart::ToolCall(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "shell".into(),
                title: "Run command".into(),
                arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({
                    "command":"git status\ngit diff --stat", "workdir":"/workspace"
                })),
            }),
            [],
            1,
        )?;
        let mut request = request("original");
        request.scope = ApprovalScope::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: forge_provider_api::ItemId::new("shell-item")?,
        };
        let mut feedback = ApprovalTimeoutFeedback::default();
        feedback.request_push(request);
        let display = feedback.display(&[exchange])?;
        assert!(display.contains("\"workdir\": \"/workspace\""));
        assert!(display.contains("\"command\": \"git status\\ngit diff --stat\""));
        Ok(())
    }
}
