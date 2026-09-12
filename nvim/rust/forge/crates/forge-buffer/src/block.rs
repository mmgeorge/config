//! Block-relative metadata never encodes a feature's file or hunk model.

use serde::{Deserialize, Serialize};

use crate::ContractError;
use crate::identity::{BlockId, EditSequence, FoldId, RegionId, RegionRevision, TargetId};
use crate::text::BufferText;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TextPosition {
    pub row: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextRange {
    pub start: TextPosition,
    pub end: TextPosition,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TargetRange {
    pub id: TargetId,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Decoration {
    pub range: TextRange,
    pub capture: String,
    pub priority: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Conceal {
    pub range: TextRange,
    pub replacement: String,
    pub line: bool,
    pub priority: u16,
}

/// Displays source adornment outside the cursor row without changing source coordinates.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceOverlay {
    pub range: TextRange,
    pub text: String,
    pub capture: String,
    pub priority: u16,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EditableRegion {
    pub id: RegionId,
    pub revision: RegionRevision,
    #[serde(default)]
    pub sequence: EditSequence,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockAnchor {
    pub block: BlockId,
    pub position: TextPosition,
}

/// Declares a half-open native fold from its owning block to an exact endpoint block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FoldRange {
    pub id: FoldId,
    pub start: TextPosition,
    pub end: BlockAnchor,
    pub closed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextChunk {
    pub text: String,
    pub capture: String,
}

/// Inline virtual text preserves source-only buffer rows for native selection and search.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Gutter {
    pub position: TextPosition,
    pub chunk: Vec<TextChunk>,
    pub priority: u16,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockMetadata {
    pub target: Vec<TargetRange>,
    pub decoration: Vec<Decoration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub visible_decoration: Vec<Decoration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub source_highlight: Vec<Decoration>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub conceal: Vec<Conceal>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub source_overlay: Vec<SourceOverlay>,
    pub editable_region: Vec<EditableRegion>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub fold: Vec<FoldRange>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub gutter: Vec<Gutter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferBlock {
    pub id: BlockId,
    pub text: BufferText,
    pub metadata: BlockMetadata,
}

impl TextRange {
    pub fn validate(&self, text: &BufferText) -> Result<(), ContractError> {
        if self.start > self.end {
            return Err(ContractError("range is reversed"));
        }
        for position in [self.start, self.end] {
            if position.row == text.row_count() && position.column == 0 {
                continue;
            }
            let row = text
                .row(position.row)
                .ok_or(ContractError("range row is outside block"))?;
            if !row.is_char_boundary(position.column) {
                return Err(ContractError("range column is outside row or splits UTF-8"));
            }
        }
        Ok(())
    }
}

impl BufferBlock {
    pub fn validate(&self) -> Result<(), ContractError> {
        self.id.validate()?;
        for target in &self.metadata.target {
            target.id.validate()?;
            target.range.validate(&self.text)?;
        }
        for decoration in self
            .metadata
            .decoration
            .iter()
            .chain(&self.metadata.visible_decoration)
            .chain(&self.metadata.source_highlight)
        {
            decoration.range.validate(&self.text)?;
            if decoration.capture.is_empty() || decoration.capture.len() > 256 {
                return Err(ContractError("capture name must contain 1..=256 bytes"));
            }
        }
        for conceal in &self.metadata.conceal {
            conceal.range.validate(&self.text)?;
            if conceal.replacement.chars().count() > 1
                || conceal.replacement.chars().any(char::is_control)
            {
                return Err(ContractError(
                    "conceal replacement must be empty or one printable character",
                ));
            }
        }
        for overlay in &self.metadata.source_overlay {
            overlay.range.validate(&self.text)?;
            if overlay.range.start.row != overlay.range.end.row
                || overlay.range.start.row >= self.text.row_count()
                || overlay.text.is_empty()
                || overlay.text.len() > 8192
                || overlay.text.chars().any(char::is_control)
                || overlay.capture.is_empty()
                || overlay.capture.len() > 256
            {
                return Err(ContractError(
                    "source overlay requires a physical row range and printable text",
                ));
            }
        }
        for (index, region) in self.metadata.editable_region.iter().enumerate() {
            region.id.validate()?;
            region.revision.validate()?;
            region.sequence.validate()?;
            region.range.validate(&self.text)?;
            for previous in &self.metadata.editable_region[..index] {
                if region.id == previous.id
                    || (region.range.start < previous.range.end
                        && previous.range.start < region.range.end)
                {
                    return Err(ContractError("editable regions overlap or repeat identity"));
                }
            }
        }
        for fold in &self.metadata.fold {
            fold.id.validate()?;
            fold.end.block.validate()?;
            TextRange {
                start: fold.start,
                end: fold.start,
            }
            .validate(&self.text)?;
        }
        for gutter in &self.metadata.gutter {
            TextRange {
                start: gutter.position,
                end: gutter.position,
            }
            .validate(&self.text)?;
            if gutter.position.row == self.text.row_count() {
                return Err(ContractError("gutter requires a physical row"));
            }
            for chunk in &gutter.chunk {
                if chunk.text.contains(['\n', '\0'])
                    || chunk.capture.is_empty()
                    || chunk.capture.len() > 256
                {
                    return Err(ContractError("gutter chunk has invalid text or capture"));
                }
            }
        }
        Ok(())
    }

    pub fn target_at(&self, position: TextPosition) -> Option<&TargetId> {
        self.metadata
            .target
            .iter()
            .find(|target| target.range.start <= position && position < target.range.end)
            .map(|target| &target.id)
    }
}
