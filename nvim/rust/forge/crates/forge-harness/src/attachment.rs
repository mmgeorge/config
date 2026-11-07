use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[cfg(test)]
use serde_json::json;
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

/// Identify which admitted attachment resource could not be prepared.
#[derive(Debug, thiserror::Error)]
pub(crate) enum AttachmentPreparationError {
    #[error("the retained attachment path is unavailable")]
    Path,
    #[error("the retained attachment contents are unavailable")]
    Contents,
    #[error("the attachment context could not be prepared")]
    Context,
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
        let directory = crate::storage::state::directory(root)
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
        Ok(crate::storage::state::directory(root)
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
        let path = crate::storage::state::directory(root)
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
    /// Prepare admitted file contents and context for a provider-neutral request.
    pub(crate) fn prepare(
        &self,
    ) -> std::result::Result<forge_provider_api::input::FileAttachment, AttachmentPreparationError>
    {
        Ok(forge_provider_api::input::FileAttachment {
            path: self.path().map_err(|_| AttachmentPreparationError::Path)?,
            label: self.label.clone(),
            media_type: self.media_type().to_owned(),
            contents: self
                .bytes()
                .map_err(|_| AttachmentPreparationError::Contents)?
                .into(),
            context: self
                .file_context()
                .map_err(|_| AttachmentPreparationError::Context)?,
        })
    }

    /// Get the admitted content type.
    pub(crate) fn media_type(&self) -> &str {
        &self.attachment.media_type
    }
    /// Resolve the retained file without reading prompt text.
    pub(crate) fn path(&self) -> Result<PathBuf> {
        self.attachment.path(&self.root)
    }
    /// Read the complete admitted object for an adapter encoder.
    pub(crate) fn bytes(&self) -> Result<Vec<u8>> {
        ObjectStore::open(&self.root)?.get(&self.attachment.object_id)
    }

    pub(crate) fn opening(&self) -> Result<String> {
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
    pub(crate) fn closing(&self) -> &'static str {
        if self.attachment.media_type.starts_with("image/") {
            "</image>"
        } else {
            "</file>"
        }
    }
    pub(crate) fn file_context(&self) -> Result<String> {
        Ok(format!(
            "{}\nUse file-reading tools to inspect the complete retained file.\n{}",
            self.opening()?,
            self.closing()
        ))
    }
}

pub(crate) fn escape(text: &str) -> String {
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
        Ok(())
    }
}
