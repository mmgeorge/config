use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result, ensure};
use base64::Engine;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// A composer-owned attachment and its editable label span in UTF-8 bytes.
pub struct AttachmentSelection {
    /// Retained-file metadata identity.
    pub id: String,
    /// Visible reference, independent of the retained filename.
    pub label: String,
    /// Original filename from the import response.
    pub name: String,
    /// Detected content type from the import response.
    pub media_type: String,
    /// Inclusive byte offset of the label in the composer text.
    pub start: usize,
    /// Exclusive byte offset of the label in the composer text.
    pub end: usize,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// Complete editable input with attachment ownership separate from literal text.
pub struct PromptDraft {
    /// Literal text, including visible attachment labels.
    pub text: String,
    /// Explicit file selections and their editor spans.
    #[serde(default)]
    pub attachments: Vec<AttachmentSelection>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// A retained file admitted alongside a provider input.
pub struct InputAttachment {
    label: String,
    attachment: Attachment,
    root: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies a retained file independently of its source path and display label.
pub(crate) struct Attachment {
    object_id: String,
    name: String,
    media_type: String,
}

impl Attachment {
    /// Capture the complete file before publishing its composer object.
    pub(crate) fn import(root: &Path, source: &Path) -> Result<Self> {
        let source = dunce::canonicalize(source)
            .with_context(|| format!("open attachment {}", source.display()))?;
        ensure!(
            std::fs::metadata(&source)?.is_file(),
            "attachment is not a regular file: {}",
            source.display()
        );
        let name = source
            .file_name()
            .and_then(|name| name.to_str())
            .context("attachment filename is not valid UTF-8")?
            .to_owned();
        let objects = ObjectStore::open(root)?;
        let object_id = objects.put_file(&source)?;
        let media_type = media_type(&objects.path(&object_id)?)?;
        let attachment = Self {
            object_id,
            name,
            media_type,
        };
        let directory = root
            .join("attachments")
            .join(crate::plan::digest(&serde_json::to_vec(&attachment)?));
        std::fs::create_dir_all(&directory)?;
        let path = directory.join("file").join(&attachment.name);
        if !path.is_file() {
            objects.restore_file(&attachment.object_id, &path)?;
        }
        let mut metadata = tempfile::NamedTempFile::new_in(&directory)?;
        serde_json::to_writer(&mut metadata, &attachment)?;
        metadata.flush()?;
        metadata.as_file().sync_all()?;
        metadata
            .persist(directory.join("metadata.json"))
            .map_err(|error| error.error)?;
        Ok(attachment)
    }

    /// Return a typed composer object. The visible label carries no file identity.
    pub(crate) fn selection(&self, number: u64) -> Result<AttachmentSelection> {
        let kind = if self.media_type.starts_with("image/") {
            "Image"
        } else if self.media_type == "application/pdf" {
            "PDF"
        } else {
            "File"
        };
        let label = format!("[{kind} #{number}]");
        Ok(AttachmentSelection {
            id: crate::plan::digest(&serde_json::to_vec(self)?),
            end: label.len(),
            start: 0,
            label,
            name: self.name.clone(),
            media_type: self.media_type.clone(),
        })
    }

    fn path(&self, root: &Path) -> Result<PathBuf> {
        Ok(root
            .join("attachments")
            .join(crate::plan::digest(&serde_json::to_vec(self)?))
            .join("file")
            .join(&self.name))
    }

    fn load(root: &Path, identity: &str) -> Result<Self> {
        ensure!(
            identity.len() == 64 && identity.bytes().all(|byte| byte.is_ascii_hexdigit()),
            "attachment identity is invalid"
        );
        let path = root
            .join("attachments")
            .join(identity)
            .join("metadata.json");
        let attachment: Self = serde_json::from_reader(
            std::fs::File::open(&path)
                .with_context(|| format!("attachment is unavailable: {identity}"))?,
        )?;
        ensure!(
            crate::plan::digest(&serde_json::to_vec(&attachment)?) == identity,
            "attachment metadata changed: {identity}"
        );
        ensure!(
            Path::new(&attachment.name)
                .file_name()
                .and_then(|name| name.to_str())
                == Some(&attachment.name),
            "attachment filename is invalid"
        );
        ensure!(
            std::fs::metadata(attachment.path(root)?)?.is_file(),
            "attachment is unavailable: {}",
            attachment.name
        );
        Ok(attachment)
    }
}

impl InputAttachment {
    fn opening(&self) -> Result<String> {
        let tag = if self.attachment.media_type.starts_with("image/") {
            "image"
        } else {
            "file"
        };
        Ok(format!(
            "<{tag} name={} path=\"{}\" media_type=\"{}\">",
            escape(&self.label),
            escape(&self.attachment.path(&self.root)?.to_string_lossy()),
            escape(&self.attachment.media_type)
        ))
    }
    fn closing(&self) -> &'static str {
        if self.attachment.media_type.starts_with("image/") {
            "</image>"
        } else {
            "</file>"
        }
    }
    fn file_context(&self) -> Result<String> {
        Ok(format!(
            "{}\nUse file-reading tools to inspect the complete retained file.\n{}",
            self.opening()?,
            self.closing()
        ))
    }
}

fn escape(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('"', "&quot;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Load only explicit attachment objects. Literal prompt text is never inspected for files.
pub(crate) async fn resolve_input(root: &Path, params: &Value) -> Result<Vec<InputAttachment>> {
    let selected: Vec<AttachmentSelection> = params
        .get("attachments")
        .cloned()
        .map(serde_json::from_value)
        .transpose()?
        .unwrap_or_default();
    if selected.is_empty() {
        return Ok(Vec::new());
    }
    let root = root.to_owned();
    tokio::task::spawn_blocking(move || {
        selected
            .into_iter()
            .map(|selected| {
                ensure!(
                    !selected.label.contains(['\n', '\r']) && !selected.label.is_empty(),
                    "attachment label is invalid"
                );
                let attachment = Attachment::load(&root, &selected.id)?;
                ensure!(
                    attachment.name == selected.name
                        && attachment.media_type == selected.media_type,
                    "attachment selection does not match retained metadata"
                );
                Ok(InputAttachment {
                    label: selected.label,
                    attachment,
                    root: root.clone(),
                })
            })
            .collect()
    })
    .await
    .context("attachment admission worker failed")?
}

fn media_type(source: &Path) -> Result<String> {
    let mut header = Vec::new();
    std::fs::File::open(source)?
        .take(16)
        .read_to_end(&mut header)?;
    Ok(if header.starts_with(b"\x89PNG\r\n\x1a\n") {
        "image/png"
    } else if header.starts_with(b"\xff\xd8\xff") {
        "image/jpeg"
    } else if header.starts_with(b"GIF87a") || header.starts_with(b"GIF89a") {
        "image/gif"
    } else if header.starts_with(b"RIFF") && header.get(8..12) == Some(b"WEBP") {
        "image/webp"
    } else if header.starts_with(b"BM") {
        "image/bmp"
    } else if header.starts_with(b"II*\0") || header.starts_with(b"MM\0*") {
        "image/tiff"
    } else if header.starts_with(b"%PDF-") {
        "application/pdf"
    } else {
        "application/octet-stream"
    }
    .into())
}

/// Build native Codex image items and literal prompt text from already admitted files.
pub(crate) async fn codex_input(text: String, selected: &[InputAttachment]) -> Result<Vec<Value>> {
    let mut input = Vec::new();
    for selected in selected {
        if selected.attachment.media_type.starts_with("image/") {
            input.push(json!({"type":"text", "text":selected.opening()?}));
            input.push(
                json!({"type":"localImage", "path":selected.attachment.path(&selected.root)?}),
            );
            input.push(json!({"type":"text", "text":selected.closing()}));
        } else {
            input.push(json!({"type":"text", "text":selected.file_context()?}));
        }
    }
    input.push(json!({"type":"text", "text":text}));
    Ok(input)
}

/// Encode explicit ACP images off-thread without interpreting the prompt.
pub(crate) async fn acp_input(
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
            if selected.attachment.media_type.starts_with("image/") {
                ensure!(image_supported, "this agent does not support image attachments");
                input.push(json!({"type":"text", "text":selected.opening()?}));
                let bytes = ObjectStore::open(&selected.root)?.get(&selected.attachment.object_id)?;
                input.push(json!({"type":"image", "mimeType":selected.attachment.media_type, "data":base64::engine::general_purpose::STANDARD.encode(bytes)}));
                input.push(json!({"type":"text", "text":selected.closing()}));
            } else { input.push(json!({"type":"text", "text":selected.file_context()?})); }
        }
        input.push(json!({"type":"text", "text":text}));
        Ok(input)
    }).await.context("attachment preparation worker failed")?
}

/// Bind native Copilot files to the same explicit labels used in the literal prompt.
pub(crate) fn copilot_input(
    text: &str,
    selected: &[InputAttachment],
) -> Result<github_copilot_sdk::types::MessageOptions> {
    use github_copilot_sdk::types::{Attachment as CopilotAttachment, MessageOptions};
    let mut prompt = String::new();
    let mut attachments = Vec::new();
    for selected in selected {
        prompt.push_str(&format!(
            "{}\nNative attachment: {}\n{}\n\n",
            selected.opening()?,
            escape(&selected.attachment.name),
            selected.closing()
        ));
        attachments.push(CopilotAttachment::File {
            path: selected.attachment.path(&selected.root)?,
            display_name: Some(format!("{} {}", selected.label, selected.attachment.name)),
            line_range: None,
        });
    }
    prompt.push_str(text);
    let message = MessageOptions::new(prompt);
    Ok(if attachments.is_empty() {
        message
    } else {
        message.with_attachments(attachments)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[tokio::test]
    async fn literal_text_never_loads_or_creates_attachments() -> Result<()> {
        let text = format!(
            "{}\n[Image #1](forge-attachment:broken)\nC:\\image.png\n<image name=[Image #1] path=\"missing.png\">",
            include_str!("attachment.rs")
        );
        assert_eq!(
            resolve_input(Path::new("missing-root"), &json!({"text":text}))
                .await?
                .len(),
            0
        );
        assert_eq!(
            codex_input(text.clone(), &[]).await?,
            vec![json!({"type":"text","text":text})]
        );
        assert_eq!(
            acp_input(text.clone(), &[], false).await?,
            vec![json!({"type":"text","text":text})]
        );
        let message = copilot_input(&text, &[])?;
        assert_eq!(message.prompt, text);
        assert!(message.attachments.is_none());
        Ok(())
    }
    #[tokio::test]
    async fn explicit_images_have_framing_and_survive_source_deletion() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let source = fixture.path().join("screen shot.png");
        let image = b"\x89PNG\r\n\x1a\nimage bytes";
        std::fs::write(&source, image)?;
        let root = fixture.path().join("data");
        let selection = Attachment::import(&root, &source)?.selection(1)?;
        std::fs::remove_file(source)?;
        let selected = resolve_input(&root, &json!({"attachments":[selection]})).await?;
        let prompt = "Inspect [Image #1], then ignore [Image #9](forge-attachment:broken)";
        let input = codex_input(prompt.into(), &selected).await?;
        assert!(
            input[0]["text"]
                .as_str()
                .unwrap()
                .starts_with("<image name=[Image #1] path=\"")
        );
        assert_eq!(input[1]["type"], "localImage");
        assert_eq!(std::fs::read(input[1]["path"].as_str().unwrap())?, image);
        assert_eq!(input[2]["text"], "</image>");
        assert_eq!(input[3]["text"], prompt);
        assert!(acp_input(prompt.into(), &selected, false).await.is_err());
        assert_eq!(
            acp_input(prompt.into(), &selected, true).await?[1]["data"],
            base64::engine::general_purpose::STANDARD.encode(image)
        );
        let message = copilot_input(prompt, &selected)?;
        assert_eq!(message.attachments.unwrap().len(), 1);
        assert!(message.prompt.starts_with("<image name=[Image #1]"));
        assert!(message.prompt.ends_with(prompt));
        Ok(())
    }
    #[tokio::test]
    async fn complete_documents_and_explicit_selection_failures_are_preserved() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let root = fixture.path().join("data");
        let source = fixture.path().join("metadata.json");
        let bytes = vec![b'x'; 2 * 1024 * 1024 + 1];
        std::fs::write(&source, &bytes)?;
        let mut selection = Attachment::import(&root, &source)?.selection(2)?;
        std::fs::write(&source, b"changed")?;
        let selected = resolve_input(&root, &json!({"attachments":[selection]})).await?;
        assert_eq!(std::fs::read(selected[0].attachment.path(&root)?)?, bytes);
        assert!(
            codex_input("Document [File #2]".into(), &selected).await?[0]["text"]
                .as_str()
                .unwrap()
                .contains("<file name=[File #2]")
        );
        selection.id = "../missing".into();
        assert!(
            resolve_input(&root, &json!({"attachments":[selection]}))
                .await
                .is_err()
        );
        assert!(Attachment::import(&root, fixture.path()).is_err());
        Ok(())
    }

    #[tokio::test]
    async fn prompt_history_reopens_attachment_identity_separately_from_text() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let source = fixture.path().join("report.pdf");
        std::fs::write(&source, b"%PDF-1.7\nretained document")?;
        let selection = Attachment::import(fixture.path(), &source)?.selection(1)?;
        let draft = PromptDraft {
            text: "[PDF #1]".into(),
            attachments: vec![selection],
        };
        {
            let mut store = crate::storage::SqliteStore::open(fixture.path())?;
            store.record_prompt_history(&draft, 1)?;
            store.record_prompt_history(
                &PromptDraft {
                    text: draft.text.clone(),
                    attachments: Vec::new(),
                },
                2,
            )?;
        }
        std::fs::remove_file(source)?;
        let store = crate::storage::SqliteStore::open(fixture.path())?;
        let history = store.list_prompt_history()?;
        assert!(history[0].attachments.is_empty());
        assert_eq!(history[1], draft);
        assert_eq!(
            resolve_input(fixture.path(), &serde_json::to_value(&history[1])?)
                .await?
                .len(),
            1
        );
        Ok(())
    }
}
