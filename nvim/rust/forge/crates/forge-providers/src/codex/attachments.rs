use forge_provider_api::input::FileAttachment;
use serde_json::{Value, json};

/// Encode admitted files without resolving host storage or inspecting prompt text.
pub(super) fn input(text: String, attachments: &[FileAttachment]) -> Vec<Value> {
    let mut input = Vec::new();
    for attachment in attachments {
        input.push(json!({"type": "text", "text": attachment.context}));
        if attachment.media_type.starts_with("image/") {
            input.push(json!({"type": "localImage", "path": attachment.path}));
        }
    }
    input.push(json!({"type": "text", "text": text}));
    input
}
