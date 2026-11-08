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
