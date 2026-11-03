use base64::Engine;
use forge_provider_api::{ProviderError, input::FileAttachment};
use serde_json::{Value, json};

/// Encode admitted content using the negotiated native image capability.
pub(super) fn input(
    text: String,
    attachments: &[FileAttachment],
    supports_images: bool,
) -> Result<Vec<Value>, ProviderError> {
    let mut input = Vec::new();
    for attachment in attachments {
        input.push(json!({"type": "text", "text": attachment.context}));
        if attachment.media_type.starts_with("image/") {
            if !supports_images {
                return Err(ProviderError::Unsupported(
                    forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::ImageInput,
                    ),
                ));
            }
            input.push(json!({
                "type": "image",
                "mimeType": attachment.media_type,
                "data": base64::engine::general_purpose::STANDARD.encode(&attachment.contents),
            }));
        }
    }
    input.push(json!({"type": "text", "text": text}));
    Ok(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::input::FileAttachment;
    use std::sync::Arc;

    fn attachment(media_type: &str) -> FileAttachment {
        FileAttachment {
            path: "fixture.bin".into(),
            label: "fixture.bin".into(),
            media_type: media_type.into(),
            contents: Arc::from([0u8, 1, 2]),
            context: "Attached fixture.bin".into(),
        }
    }

    #[test]
    fn image_attachments_require_negotiated_support_and_encode_native_data() {
        let image = attachment("image/png");
        assert!(matches!(
            input(String::new(), std::slice::from_ref(&image), false),
            Err(ProviderError::Unsupported(_))
        ));
        let encoded = input("ask".into(), &[image], true).unwrap();
        assert_eq!(encoded[1]["type"], "image");
        assert_eq!(encoded[1]["mimeType"], "image/png");
        assert_eq!(encoded[1]["data"], "AAEC");
        assert_eq!(encoded[2]["text"], "ask");
    }
}
