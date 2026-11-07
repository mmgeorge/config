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

#[cfg(test)]
mod tests {
    use super::input;
    use forge_provider_api::input::FileAttachment;
    use std::{path::PathBuf, sync::Arc};

    #[test]
    fn admitted_image_uses_codex_local_image_framing_and_keeps_context() {
        let attachment = FileAttachment {
            path: PathBuf::from("workspace/imported/diagram.png"),
            label: "[Image #1]".into(),
            media_type: "image/png".into(),
            contents: Arc::from(&b"image-bytes"[..]),
            context: "<image name=[Image #1] path=\"diagram.png\">".into(),
        };

        let encoded = input("Describe this image".into(), &[attachment]);

        assert_eq!(encoded[0]["type"], "text");
        assert_eq!(
            encoded[0]["text"],
            "<image name=[Image #1] path=\"diagram.png\">"
        );
        assert_eq!(
            encoded[1],
            serde_json::json!({
                "type":"localImage", "path":"workspace/imported/diagram.png"
            })
        );
        assert_eq!(encoded[2]["text"], "Describe this image");
    }
}
