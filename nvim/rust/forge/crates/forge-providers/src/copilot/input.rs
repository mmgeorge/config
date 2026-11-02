use forge_provider_api::input::{FileAttachment, MessageInput};
use github_copilot_sdk::types::{Attachment, MessageOptions};

/// Encode explicitly admitted files without discovering attachments in prompt text.
pub fn message(input: MessageInput) -> MessageOptions {
    let MessageInput {
        prompt,
        attachments,
    } = input;
    let message = MessageOptions::new(prompt);
    if attachments.is_empty() {
        return message;
    }
    message.with_attachments(
        attachments
            .into_iter()
            .map(|attachment| {
                let FileAttachment { path, display_name } = attachment;
                Attachment::File {
                    path,
                    display_name: Some(display_name),
                    line_range: None,
                }
            })
            .collect(),
    )
}
