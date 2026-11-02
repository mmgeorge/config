use crate::attachment::InputAttachment;
use anyhow::{Context, Result, ensure};
use base64::Engine;
use serde_json::{Value, json};
/// Encode explicit ACP images off-thread without interpreting the prompt.
pub(crate) async fn input(
    text: String,
    selected: &[InputAttachment],
    image_supported: bool,
) -> Result<Vec<Value>> {
    if selected.is_empty() {
        return Ok(vec![json!({"type":"text", "text":text})]);
    }
    let selected = selected.to_vec();
    tokio::task::spawn_blocking(move || {
        let mut input = Vec::new();
        for selected in selected {
            if selected.media_type().starts_with("image/") {
                ensure!(image_supported, "this agent does not support image attachments");
                input.push(json!({"type":"text", "text":selected.opening()?}));
                let bytes = selected.bytes()?;
                input.push(json!({"type":"image", "mimeType":selected.media_type(), "data":base64::engine::general_purpose::STANDARD.encode(bytes)}));
                input.push(json!({"type":"text", "text":selected.closing()}));
            } else { input.push(json!({"type":"text", "text":selected.file_context()?})); }
        }
        input.push(json!({"type":"text", "text":text}));
        Ok(input)
    }).await.context("attachment preparation worker failed")?
}
