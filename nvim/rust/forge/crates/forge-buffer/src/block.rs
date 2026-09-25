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
    #[serde(default)]
    /// Close descendant folds when this heading is explicitly opened.
    pub collapse_children: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextChunk {
    pub text: String,
    pub capture: String,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
/// Controls whether a marker enters text layout or occupies the reserved sign column.
pub enum GutterPlacement {
    #[default]
    Inline,
    Sign,
}

/// Positions a marker relative to a physical source row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Gutter {
    #[serde(default)]
    pub placement: GutterPlacement,
    pub position: TextPosition,
    pub chunk: Vec<TextChunk>,
    pub priority: u16,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
/// Resolves the content column independently of an optional leading marker.
pub struct ContentLayout {
    /// Content column measured from the window's left edge, including its reserved marker column.
    pub indent: usize,
    /// Optional marker occupying the final two cells before content.
    pub marker: Option<TextChunk>,
    #[serde(default)]
    /// Projection-owned padding excluded from Markdown parser regions.
    pub source_indent: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockMetadata {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub layout: Option<ContentLayout>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub markdown: bool,
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

impl BlockMetadata {
    /// Charge retained vector storage and nested strings without serializing metadata.
    fn allocated_bytes(&self) -> usize {
        use std::mem::size_of;
        self.target.capacity() * size_of::<TargetRange>()
            + self.layout.as_ref().and_then(|layout| layout.marker.as_ref())
                .map_or(0, |marker| marker.text.capacity() + marker.capture.capacity())
            + self.decoration.capacity() * size_of::<Decoration>()
            + self.visible_decoration.capacity() * size_of::<Decoration>()
            + self.source_highlight.capacity() * size_of::<Decoration>()
            + self.conceal.capacity() * size_of::<Conceal>()
            + self.source_overlay.capacity() * size_of::<SourceOverlay>()
            + self.editable_region.capacity() * size_of::<EditableRegion>()
            + self.fold.capacity() * size_of::<FoldRange>()
            + self.gutter.capacity() * size_of::<Gutter>()
            + self
                .target
                .iter()
                .map(|target| target.id.0.capacity())
                .sum::<usize>()
            + self
                .decoration
                .iter()
                .chain(&self.visible_decoration)
                .chain(&self.source_highlight)
                .map(|span| span.capture.capacity())
                .sum::<usize>()
            + self
                .conceal
                .iter()
                .map(|conceal| conceal.replacement.capacity())
                .sum::<usize>()
            + self
                .source_overlay
                .iter()
                .map(|overlay| overlay.text.capacity() + overlay.capture.capacity())
                .sum::<usize>()
            + self
                .editable_region
                .iter()
                .map(|region| region.id.0.capacity())
                .sum::<usize>()
            + self
                .fold
                .iter()
                .map(|fold| fold.id.0.capacity() + fold.end.block.0.capacity())
                .sum::<usize>()
            + self
                .gutter
                .iter()
                .map(|gutter| {
                    gutter.chunk.capacity() * size_of::<TextChunk>()
                        + gutter
                            .chunk
                            .iter()
                            .map(|chunk| chunk.text.capacity() + chunk.capture.capacity())
                            .sum::<usize>()
                })
                .sum::<usize>()
    }
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
    /// Charge one retained block and every allocation it owns, excluding allocator bookkeeping.
    pub fn retained_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
            + self.id.0.capacity()
            + self.text.allocated_bytes()
            + self.metadata.allocated_bytes()
    }

    pub fn validate(&self) -> Result<(), ContractError> {
        if let Some(layout) = &self.metadata.layout {
            if !(2..=256).contains(&layout.indent) || layout.source_indent > layout.indent - 2 {
                return Err(ContractError("content indent must contain 2..=256 cells"));
            }
            if let Some(marker) = &layout.marker {
                if marker.text.chars().count() != 1 || marker.text.chars().any(char::is_control)
                    || marker.capture.is_empty() || marker.capture.len() > 256 {
                    return Err(ContractError("content marker requires one printable character and a capture"));
                }
            }
        }
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
