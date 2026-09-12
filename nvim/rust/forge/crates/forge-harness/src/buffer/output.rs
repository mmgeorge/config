use std::path::{Path, PathBuf};

use anyhow::{Context, Result, ensure};
use forge_buffer::{
    block::{BlockMetadata, BufferBlock},
    document::BufferDocument,
    identity::{BlockId, DocumentId, DocumentRevision},
    patch::{BufferPatch, BufferSnapshot},
    text::BufferText,
};
use serde::Serialize;

use super::tool::{OwnedToolExport, ToolOutputView};

pub struct OutputDocument {
    document: BufferDocument,
    output: ToolOutputView,
    export: Option<OwnedToolExport>,
    complete: bool,
    retained: usize,
}

#[derive(Serialize)]
pub struct OutputDelivery {
    pub patch: Option<BufferPatch>,
    pub more: bool,
}

impl OutputDocument {
    pub fn new(id: DocumentId, mut output: ToolOutputView) -> Result<Self> {
        output.expand();
        Ok(Self {
            document: BufferDocument::new(id, Vec::new())?,
            output,
            export: None,
            complete: false,
            retained: 0,
        })
    }

    pub fn snapshot(&self) -> BufferSnapshot {
        self.document.snapshot()
    }

    pub fn retained_bytes(&self) -> usize {
        self.output.retained_bytes() + self.retained
    }

    pub fn demand(
        &mut self,
        revision: DocumentRevision,
        available: usize,
    ) -> Result<OutputDelivery> {
        ensure!(
            self.document.revision() == revision,
            "tool output document revision changed"
        );
        if self.complete {
            return Ok(OutputDelivery {
                patch: None,
                more: false,
            });
        }
        let Some(batch) = self.output.next_batch(256, 65536)? else {
            self.complete = true;
            return Ok(OutputDelivery {
                patch: None,
                more: false,
            });
        };
        let text = BufferText::from_rows(&batch.row)?;
        let retained = text.byte_count() + batch.row.len() * 32;
        ensure!(
            retained <= available,
            "tool view capacity is full, complete export remains available"
        );
        ensure!(
            self.retained + retained <= 48 * 1024 * 1024,
            "expanded output exceeds 48 MiB, complete export remains available"
        );
        let block = BufferBlock {
            id: BlockId(format!("tool:output:{}", batch.start_row)),
            text,
            metadata: BlockMetadata::default(),
        };
        let end = self.document.block_count();
        let patch = self.document.edit(end..end, vec![block])?;
        self.output.accept_batch(&batch)?;
        self.complete = batch.complete;
        self.retained += retained;
        Ok(OutputDelivery {
            patch,
            more: !self.complete,
        })
    }

    pub fn export(&mut self, directory: &Path) -> Result<PathBuf> {
        ensure!(
            directory.is_absolute(),
            "tool export directory must be absolute"
        );
        if self.export.is_none() {
            self.export = Some(self.output.export_saved_output(directory)?);
        }
        Ok(self
            .export
            .as_ref()
            .expect("owned export was installed")
            .path()
            .context("owned tool export has already closed")?
            .to_owned())
    }

    pub fn close(&mut self) -> Result<()> {
        if let Some(export) = &mut self.export {
            export.close()?;
        }
        self.export = None;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn rejected_demand_preserves_complete_source_and_export_lifetime() -> Result<()> {
        let source = (0..1001)
            .map(|line| format!("line {line}\n"))
            .collect::<String>();
        let output = ToolOutputView::new("call".into(), Arc::from(source.as_str()))?;
        let mut document = OutputDocument::new(DocumentId("tool:test".into()), output)?;
        assert!(document.demand(DocumentRevision(1), usize::MAX).is_err());
        let mut revision = DocumentRevision(0);
        loop {
            let delivery = document.demand(revision, usize::MAX)?;
            if let Some(patch) = delivery.patch {
                assert!(patch.next_rows - patch.base_rows <= 256);
                revision = patch.next;
            }
            if !delivery.more {
                break;
            }
        }
        assert_eq!(
            document
                .snapshot()
                .block
                .iter()
                .map(|block| block.text.row_count())
                .sum::<usize>(),
            1001
        );
        let directory = tempfile::tempdir()?;
        let path = document.export(directory.path())?;
        assert_eq!(std::fs::read_to_string(&path)?, source);
        document.close()?;
        assert!(!path.exists());
        Ok(())
    }
}
