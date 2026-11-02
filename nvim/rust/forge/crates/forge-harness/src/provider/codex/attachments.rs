use crate::attachment::InputAttachment;
use anyhow::Result;
use serde_json::{Value, json};
/// Build native Codex image items and literal prompt text from already admitted files.
pub(crate) async fn input(text: String, selected: &[InputAttachment]) -> Result<Vec<Value>> {
    let mut input = Vec::new();
    for selected in selected {
        if selected.media_type().starts_with("image/") {
            input.push(json!({"type":"text", "text":selected.opening()?}));
            input.push(json!({"type":"localImage", "path":selected.path()?}));
            input.push(json!({"type":"text", "text":selected.closing()}));
        } else {
            input.push(json!({"type":"text", "text":selected.file_context()?}));
        }
    }
    input.push(json!({"type":"text", "text":text}));
    Ok(input)
}
