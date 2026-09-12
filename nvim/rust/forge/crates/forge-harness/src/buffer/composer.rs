use anyhow::{Context, Result, ensure};
use forge_buffer::block::{BlockMetadata, BufferBlock, EditableRegion, TextPosition, TextRange};
use forge_buffer::document::BufferDocument;
use forge_buffer::editable::{LocalEdit, LocalEditResult};
use forge_buffer::identity::{BlockId, DocumentId, EditSequence, RegionId, RegionRevision};
use forge_buffer::patch::{BufferPatch, BufferSnapshot};
use forge_buffer::text::BufferText;
use serde::Serialize;

const MAX_COMPOSER_BYTES: usize = 65536;

pub struct ComposerDocument {
    document: BufferDocument,
    pending: Option<(u64, RegionRevision)>,
    sequence: u64,
    retraction: Option<ComposerRetraction>,
}

struct ComposerRetraction {
    token: u64,
    base: RegionRevision,
    revision: RegionRevision,
    source: BufferText,
    restored: bool,
}

#[derive(Debug, Serialize)]
pub struct ComposerSubmission {
    pub token: u64,
    pub text: String,
}

impl ComposerDocument {
    pub fn new(id: DocumentId, initial: BufferText) -> Result<Self> {
        ensure!(
            initial.byte_count() <= MAX_COMPOSER_BYTES
                && initial.row_count() <= 4096
                && initial.row_count() > 0,
            "initial composer exceeds 64 KiB or 4096 rows"
        );
        Ok(Self {
            document: BufferDocument::new(
                id,
                vec![composer_block(initial, RegionRevision(0), EditSequence(0))?],
            )?,
            pending: None,
            sequence: 0,
            retraction: None,
        })
    }

    pub fn snapshot(&self) -> BufferSnapshot {
        self.document.snapshot()
    }

    pub fn pending_token(&self) -> Option<u64> {
        self.pending.map(|(token, _)| token)
    }

    pub fn edit(&mut self, mut edit: LocalEdit) -> Result<LocalEditResult> {
        ensure!(
            edit.text.byte_count() <= MAX_COMPOSER_BYTES && edit.text.row_count() <= 4096,
            "composer edit exceeds 64 KiB or 4096 rows"
        );
        if let Some(retraction) = &self.retraction {
            let block = self
                .document
                .block(&BlockId("composer".into()))
                .context("composer source is missing")?;
            if (edit.base == retraction.base
                || (retraction.restored
                    && edit.base.0.checked_add(1) == Some(retraction.revision.0)))
                && block.metadata.editable_region[0].revision == retraction.revision
                && if retraction.restored {
                    block.text == retraction.source
                } else {
                    block.text.wire_rows() == vec![""]
                }
            {
                edit.base = retraction.revision;
            }
        }
        Ok(self.document.accept_local_edit(edit)?)
    }

    pub fn begin_submission(&mut self, revision: RegionRevision) -> Result<ComposerSubmission> {
        ensure!(
            self.pending.is_none(),
            "composer submission is already awaiting admission"
        );
        let block = self
            .document
            .block(&BlockId("composer".into()))
            .context("composer source is missing")?;
        ensure!(
            block.metadata.editable_region[0].revision == revision,
            "composer source changed before submission"
        );
        let text = block.text.wire_rows().join("\n");
        ensure!(!text.trim().is_empty(), "composer is empty");
        ensure!(text.len() <= MAX_COMPOSER_BYTES, "composer exceeds 64 KiB");
        self.sequence = self
            .sequence
            .checked_add(1)
            .filter(|sequence| *sequence <= forge_buffer::MAX_COUNTER)
            .context("composer submission identity exhausted")?;
        self.pending = Some((self.sequence, revision));
        Ok(ComposerSubmission {
            token: self.sequence,
            text,
        })
    }

    pub fn settle_submission(&mut self, token: u64, admitted: bool) -> Result<Option<BufferPatch>> {
        let (pending, revision) = self.pending.context("composer has no pending submission")?;
        ensure!(
            pending == token,
            "composer acknowledgement belongs to another submission"
        );
        let current = self
            .document
            .block(&BlockId("composer".into()))
            .context("composer source is missing")?
            .metadata
            .editable_region[0]
            .revision;
        let patch = if admitted && current == revision {
            let accepted_sequence = self
                .document
                .block(&BlockId("composer".into()))
                .context("composer source is missing")?
                .metadata
                .editable_region[0]
                .sequence;
            let source = self
                .document
                .block(&BlockId("composer".into()))
                .context("composer source is missing")?
                .text
                .clone();
            self.retraction = Some(ComposerRetraction {
                token,
                base: current,
                revision: current.next()?,
                source,
                restored: false,
            });
            self.document.edit(
                0..1,
                vec![composer_block(
                    BufferText::from_rows([""])?,
                    current.next()?,
                    accepted_sequence,
                )?],
            )?
        } else {
            None
        };
        self.pending = None;
        Ok(patch)
    }

    pub fn retract_submission(&mut self, token: u64) -> Result<Option<BufferPatch>> {
        let Some(retraction) = self.retraction.as_mut() else {
            return Ok(None);
        };
        if token != retraction.token || retraction.restored {
            return Ok(None);
        }
        let block = self
            .document
            .block(&BlockId("composer".into()))
            .context("composer source is missing")?;
        if block.metadata.editable_region[0].revision != retraction.revision
            || block.text.wire_rows() != vec![""]
        {
            return Ok(None);
        }
        let revision = retraction.revision.next()?;
        let accepted_sequence = block.metadata.editable_region[0].sequence;
        let patch = self.document.edit(
            0..1,
            vec![composer_block(
                retraction.source.clone(),
                revision,
                accepted_sequence,
            )?],
        )?;
        retraction.revision = revision;
        retraction.restored = true;
        Ok(patch)
    }
}

fn composer_block(
    text: BufferText,
    revision: RegionRevision,
    sequence: EditSequence,
) -> Result<BufferBlock> {
    ensure!(text.row_count() > 0, "composer requires a source row");
    let last = text.row_count() - 1;
    let end = TextPosition {
        row: last,
        column: text.row(last).context("composer last row missing")?.len(),
    };
    Ok(BufferBlock {
        id: BlockId("composer".into()),
        text,
        metadata: BlockMetadata {
            editable_region: vec![EditableRegion {
                id: RegionId("composer".into()),
                revision,
                sequence,
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end,
                },
            }],
            ..BlockMetadata::default()
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::identity::EditSequence;

    fn edit(source: &str, base: u64, sequence: u64) -> LocalEdit {
        LocalEdit {
            document: DocumentId("composer:test".into()),
            region: RegionId("composer".into()),
            base: RegionRevision(base),
            sequence: EditSequence(sequence),
            text: BufferText::from_rows(source.split('\n')).unwrap(),
        }
    }

    #[test]
    fn admission_never_clears_text_edited_after_submission() -> Result<()> {
        let mut composer = ComposerDocument::new(
            DocumentId("composer:test".into()),
            BufferText::from_rows([""])?,
        )?;
        assert!(matches!(
            composer.edit(edit("first prompt", 0, 1))?,
            LocalEditResult::Accepted { .. }
        ));
        let submission = composer.begin_submission(RegionRevision(1))?;
        assert_eq!(submission.text, "first prompt");
        assert!(composer.begin_submission(RegionRevision(1)).is_err());
        assert!(matches!(
            composer.edit(edit("next prompt", 1, 2))?,
            LocalEditResult::Accepted { .. }
        ));
        assert!(
            composer
                .settle_submission(submission.token, true)?
                .is_none()
        );
        let next = composer.begin_submission(RegionRevision(2))?;
        assert_eq!(next.text, "next prompt");
        assert!(composer.settle_submission(next.token, false)?.is_none());
        let retry = composer.begin_submission(RegionRevision(2))?;
        assert!(composer.settle_submission(retry.token, true)?.is_some());
        assert!(composer.begin_submission(RegionRevision(3)).is_err());
        Ok(())
    }

    #[test]
    fn retraction_restores_only_the_unchanged_cleared_composer() -> Result<()> {
        let mut composer = ComposerDocument::new(
            DocumentId("composer:test".into()),
            BufferText::from_rows(["submitted"])?,
        )?;
        let submitted = composer.begin_submission(RegionRevision(0))?;
        assert!(composer.settle_submission(submitted.token, true)?.is_some());
        assert!(composer.retract_submission(submitted.token)?.is_some());
        let retry = composer.begin_submission(RegionRevision(2))?;
        assert_eq!(retry.text, "submitted");
        assert!(composer.settle_submission(retry.token, true)?.is_some());
        assert!(matches!(
            composer.edit(edit("newer prompt", 3, 1))?,
            LocalEditResult::Accepted { .. }
        ));
        assert!(composer.retract_submission(retry.token)?.is_none());
        assert_eq!(
            composer.begin_submission(RegionRevision(4))?.text,
            "newer prompt"
        );
        Ok(())
    }

    #[test]
    fn delayed_typing_rebases_only_across_the_owned_admission_clear() -> Result<()> {
        let mut composer = ComposerDocument::new(
            DocumentId("composer:test".into()),
            BufferText::from_rows(["submitted"])?,
        )?;
        let submitted = composer.begin_submission(RegionRevision(0))?;
        let cleared = composer.settle_submission(submitted.token, true)?.unwrap();
        let LocalEditResult::Accepted {
            acknowledgement,
            patch,
        } = composer.edit(edit("newer typing", 0, 1))?
        else {
            panic!("delayed typing was rejected");
        };
        assert_eq!(patch.base, cleared.next);
        assert_eq!(acknowledgement.revision, RegionRevision(2));
        assert_eq!(acknowledgement.sequence.0, 1);
        assert!(matches!(
            composer.edit(edit("unrelated stale source", 0, 2))?,
            LocalEditResult::Conflict { .. }
        ));
        assert!(composer.retract_submission(submitted.token)?.is_none());
        assert_eq!(
            composer.begin_submission(RegionRevision(2))?.text,
            "newer typing"
        );
        Ok(())
    }

    #[test]
    fn delayed_typing_survives_clear_and_retraction_before_delivery() -> Result<()> {
        let mut composer = ComposerDocument::new(
            DocumentId("composer:test".into()),
            BufferText::from_rows(["submitted"])?,
        )?;
        let submitted = composer.begin_submission(RegionRevision(0))?;
        composer.settle_submission(submitted.token, true)?;
        composer.retract_submission(submitted.token)?;
        let LocalEditResult::Accepted {
            acknowledgement, ..
        } = composer.edit(edit("newer draft", 0, 1))?
        else {
            panic!("typing behind retraction was rejected");
        };
        assert_eq!(acknowledgement.revision, RegionRevision(3));
        assert!(composer.retract_submission(submitted.token)?.is_none());
        assert_eq!(
            composer.begin_submission(RegionRevision(3))?.text,
            "newer draft"
        );
        Ok(())
    }
}
