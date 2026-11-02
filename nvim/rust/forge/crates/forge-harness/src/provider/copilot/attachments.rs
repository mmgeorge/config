use crate::attachment::{InputAttachment, escape};
use anyhow::Result;
/// Bind native Copilot files to the same explicit labels used in the literal prompt.
pub(crate) fn input(
    text: &str,
    selected: &[InputAttachment],
) -> Result<forge_provider_api::input::MessageInput> {
    use forge_provider_api::input::{FileAttachment, MessageInput};
    let mut prompt = String::new();
    let mut attachments = Vec::new();
    for selected in selected {
        prompt.push_str(&format!(
            "{}\nNative attachment: {}\n{}\n\n",
            selected.opening()?,
            escape(selected.name()),
            selected.closing()
        ));
        attachments.push(FileAttachment {
            path: selected.path()?,
            display_name: format!("{} {}", selected.label(), selected.name()),
        });
    }
    prompt.push_str(text);
    Ok(MessageInput {
        prompt,
        attachments,
    })
}
