//! Shared multi-file saved diffs retain canonical sources and demand each body independently.

use crate::{
    display::{ChunkLimits, RowKind},
    engine::{DiffEngine, DiffRequest, EngineError},
    file_header::FileChange,
    patch::{PatchFile, UnifiedPatch},
    presentation::{DiffPresenter, FoldPolicy, PresentationOptions},
    saved_source::SavedDiffSource,
    source::SourcePair,
    source_projection::{SourceDiffElement, SourceDiffProjection, SourceExpansionState},
    syntax::{SyntaxEngine, SyntaxLanguage, SyntaxRequest},
    view::DiffViewOptions,
    workers::WorkPriority,
};
use forge_buffer::{
    ContractError,
    block::{
        BlockAnchor, BlockMetadata, BufferBlock, DeferredSection, TargetRange, TextPosition,
        TextRange,
    },
    identity::{BlockId, FoldId, TargetId},
    text::BufferText,
};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, ops::Range, sync::Arc};

#[derive(Clone)]
pub struct SavedDiffProjection {
    text: Arc<String>,
    file: Vec<SavedFileProjection>,
    options: DiffViewOptions,
}

#[derive(Clone)]
struct SavedFileProjection {
    key: String,
    path: String,
    display_path: String,
    metadata: Vec<Range<usize>>,
    change: FileChange,
    counts: (u64, u64),
    expanded: bool,
    expansion: Option<SourceExpansionState>,
    restore_rows: usize,
    content: SavedFileContent,
}

#[derive(Clone)]
enum SavedFileContent {
    Source {
        source: SourcePair,
        projection: Option<Box<SourceDiffProjection>>,
        diagnostic: Option<String>,
    },
    Raw {
        diagnostic: String,
        source: Range<usize>,
        offset: usize,
        row: Vec<Range<usize>>,
    },
}

#[derive(Clone, Debug)]
pub struct SavedDiffTarget {
    pub file: String,
    pub declaration: Option<String>,
}

pub struct SavedDiffFrame {
    pub block: Vec<BufferBlock>,
    pub target: BTreeMap<TargetId, SavedDiffTarget>,
}

/// Carries exact immutable analysis to the worker that owns projection installation.
pub struct SavedFileAnalysis {
    pub key: String,
    pub projection: SourceDiffProjection,
    pub diagnostic: Option<String>,
}

impl SavedDiffProjection {
    /// Validate complete captured comparisons before admitting semantic classification.
    /// Sparse or mismatched evidence retains its original patch rows in every control state.
    pub fn new(source: SavedDiffSource) -> Result<Self, String> {
        let patch = UnifiedPatch::parse(&source.text).map_err(|failure| failure.to_string())?;
        let mut captured = BTreeMap::new();
        for file in source.file {
            let key = (file.old_path, file.new_path);
            if captured.insert(key.clone(), file.source).is_some() {
                captured.insert(key, Err("captured source identity is duplicated".into()));
            }
        }
        let mut file = Vec::with_capacity(patch.file.len());
        for (index, patch) in patch.file.iter().enumerate() {
            let captured_source = captured
                .remove(&(patch.old_path.clone(), patch.new_path.clone()))
                .unwrap_or_else(|| {
                    Err(source
                        .diagnostic
                        .clone()
                        .unwrap_or_else(|| "complete captured sources are unavailable".into()))
                })
                .and_then(|source| {
                    if patch.matches_source(&source) {
                        Ok(source)
                    } else {
                        Err("captured sources do not match the complete saved comparison".into())
                    }
                });
            let path = patch
                .new_path
                .as_ref()
                .or(patch.old_path.as_ref())
                .ok_or("saved file path is missing")?
                .clone();
            let key = format!(
                "{index}:{:x}",
                Sha256::digest(format!("{:?}:{:?}", patch.old_path, patch.new_path).as_bytes())
            );
            let counts = patch.hunk.iter().flat_map(|hunk| &hunk.row).fold(
                (0, 0),
                |(added, removed), row| {
                    (
                        added + u64::from(row.kind() == RowKind::Added),
                        removed + u64::from(row.kind() == RowKind::Removed),
                    )
                },
            );
            let content = match captured_source {
                Ok(source) => SavedFileContent::Source {
                    source,
                    projection: None,
                    diagnostic: None,
                },
                Err(diagnostic) => SavedFileContent::Raw {
                    diagnostic,
                    source: patch.source_range.clone(),
                    offset: patch.source_range.start,
                    row: Vec::new(),
                },
            };
            file.push(SavedFileProjection {
                key,
                display_path: match (&patch.old_path, &patch.new_path) {
                    (Some(old), Some(new)) if old != new => format!("{old} \u{2192} {new}"),
                    _ => path.clone(),
                },
                metadata: metadata_ranges(patch, &source.text),
                path,
                counts,
                change: file_change(patch),
                expanded: true,
                expansion: None,
                restore_rows: 0,
                content,
            });
        }
        Ok(Self {
            text: source.text,
            file,
            options: DiffViewOptions::default(),
        })
    }

    pub fn identity(&self) -> String {
        let mut hash = Sha256::new();
        hash.update(self.text.as_bytes());
        for file in &self.file {
            hash.update(file.key.as_bytes());
            match &file.content {
                SavedFileContent::Source { source, .. } => {
                    for source in [&source.old, &source.new] {
                        let identity = source.identity();
                        hash.update(identity.content_hash);
                        hash.update(format!("{:?}", identity.representation).as_bytes());
                    }
                }
                SavedFileContent::Raw { diagnostic, .. } => hash.update(diagnostic.as_bytes()),
            }
        }
        format!("{:x}", hash.finalize())
    }

    pub fn options(&self) -> DiffViewOptions {
        self.options
    }

    /// Retain file headings without demanding bodies when embedded in a collapsed timeline tree.
    pub fn collapse_files(&mut self) {
        for file in &mut self.file {
            file.expanded = false;
        }
    }

    /// Carry file and declaration expansion across new captured sources by unambiguous path.
    /// Old source cursors and formatted rows remain owned by the previous projection.
    pub fn inherit_expansion(&mut self, previous: &Self) {
        for file in &mut self.file {
            let Some(key) = previous.key_for_path(&file.path) else {
                continue;
            };
            let prior = previous
                .file
                .iter()
                .find(|prior| prior.key == key)
                .expect("matched saved file");
            file.expanded = prior.expanded;
            if let SavedFileContent::Source {
                projection: Some(projection),
                ..
            } = &prior.content
            {
                file.expansion = Some(projection.expansion_state());
                file.restore_rows = projection.retained_source_rows();
            } else {
                file.expansion = prior.expansion.clone();
                file.restore_rows = prior.restore_rows;
            }
        }
    }

    /// Select open files for asynchronous analysis before installing a successor projection.
    pub fn expanded_files(&self) -> impl Iterator<Item = &str> {
        self.file
            .iter()
            .filter(|file| file.expanded)
            .map(|file| file.key.as_str())
    }

    /// Choose a file that still needs a visible source batch. Closed files do no body work.
    pub fn next_file(&self) -> Option<&str> {
        self.file
            .iter()
            .find(|file| file.expanded && file.more())
            .map(|file| file.key.as_str())
    }

    pub fn more(&self) -> bool {
        self.next_file().is_some()
    }

    /// Return immutable source input only until its native analysis has been installed.
    pub fn pending_source(&self, key: &str) -> Option<SourcePair> {
        match &self.file.iter().find(|file| file.key == key)?.content {
            SavedFileContent::Source {
                source,
                projection: None,
                ..
            } => Some(source.clone()),
            _ => None,
        }
    }

    pub fn path(&self, key: &str) -> Option<&str> {
        self.file
            .iter()
            .find(|file| file.key == key)
            .map(|file| file.path.as_str())
    }

    /// Resolve an unambiguous captured file path when adopting an existing timeline heading.
    pub fn key_for_path(&self, path: &str) -> Option<&str> {
        let mut matching = self.file.iter().filter(|file| file.path == path);
        let file = matching.next()?;
        matching.next().is_none().then_some(file.key.as_str())
    }

    /// Acquire cached diff and syntax analysis for one immutable file outside document locks.
    /// Syntax failures retain unclassified source rows and a diagnostic for the requested view.
    pub async fn analyze_file(
        &self,
        key: &str,
        diff: &Arc<DiffEngine>,
        syntax: &Arc<SyntaxEngine>,
        priority: WorkPriority,
    ) -> Result<Option<SavedFileAnalysis>, EngineError> {
        let Some(source) = self.pending_source(key) else {
            return Ok(None);
        };
        let analysis = diff
            .compare(DiffRequest {
                source: source.clone(),
                priority,
            })
            .await?;
        let mut before = None;
        let mut after = None;
        let mut diagnostic = None;
        if let Some(language) = self.path(key).and_then(SyntaxLanguage::for_path) {
            for (source, target) in [(source.old, &mut before), (source.new, &mut after)] {
                match syntax
                    .analyze(SyntaxRequest {
                        source,
                        language,
                        priority,
                        deadline: None,
                    })
                    .await
                {
                    Ok(syntax) => *target = Some(syntax),
                    Err(failure) => {
                        diagnostic =
                            Some(format!("native source syntax is unavailable: {failure:?}"))
                    }
                }
            }
        }
        Ok(Some(SavedFileAnalysis {
            key: key.into(),
            projection: SourceDiffProjection::new(analysis, before, after, Default::default()),
            diagnostic,
        }))
    }

    /// Adopt exact native analysis without replacing source ownership or publishing rows.
    pub fn install(
        &mut self,
        key: &str,
        mut projection: SourceDiffProjection,
        diagnostic: Option<String>,
    ) -> Result<(), ContractError> {
        let file = self
            .file
            .iter_mut()
            .find(|file| file.key == key)
            .ok_or(ContractError("saved source file disappeared"))?;
        let SavedFileContent::Source {
            source,
            projection: current,
            diagnostic: current_diagnostic,
        } = &mut file.content
        else {
            return Err(ContractError("saved file has no complete sources"));
        };
        let compared = projection.analysis().source();
        if source.old.identity() != compared.old.identity()
            || source.new.identity() != compared.new.identity()
        {
            return Err(ContractError(
                "saved analysis belongs to different captured sources",
            ));
        }
        if let Some(expansion) = file.expansion.take() {
            projection.restore_expansion(expansion);
        }
        *current = Some(Box::new(projection.reconfigure(self.options)?));
        *current_diagnostic = diagnostic;
        Ok(())
    }

    /// Reuse loaded bodies and immutable analysis while preparing a new control state.
    pub fn reconfigure(
        &self,
        options: DiffViewOptions,
        limits: ChunkLimits,
    ) -> Result<Self, ContractError> {
        let mut next = self.clone();
        next.options = options;
        for file in &mut next.file {
            if file.expanded
                && let SavedFileContent::Source {
                    projection: Some(projection),
                    ..
                } = &mut file.content
            {
                let mut configured = projection.reconfigure(options)?;
                if !configured.materialized() {
                    configured.demand(limits)?;
                }
                while configured.more() && configured.retained_source_rows() < file.restore_rows {
                    configured.demand(limits)?;
                }
                file.restore_rows = 0;
                **projection = configured;
            }
        }
        Ok(next)
    }

    pub fn demand(&mut self, key: &str, limits: ChunkLimits) -> Result<(), ContractError> {
        let file = self
            .file
            .iter_mut()
            .find(|file| file.key == key)
            .ok_or(ContractError("saved source file disappeared"))?;
        file.expanded = true;
        match &mut file.content {
            SavedFileContent::Source {
                projection: Some(projection),
                ..
            } => {
                if projection.options() != self.options {
                    **projection = projection.reconfigure(self.options)?;
                }
                projection.demand(limits)?;
                while projection.more() && projection.retained_source_rows() < file.restore_rows {
                    projection.demand(limits)?;
                }
                file.restore_rows = 0;
                Ok(())
            }
            SavedFileContent::Source {
                projection: None, ..
            } => Err(ContractError("saved source analysis is not prepared")),
            SavedFileContent::Raw {
                source,
                offset,
                row,
                ..
            } => {
                let mut bytes = 0;
                let mut delivered = 0;
                while *offset < source.end {
                    let remaining = &self.text.as_bytes()[*offset..source.end];
                    let length = memchr::memchr(b'\n', remaining)
                        .map_or(remaining.len(), |position| position + 1);
                    if delivered > 0
                        && (delivered >= limits.rows || bytes + length > limits.text_bytes)
                    {
                        break;
                    }
                    let end = *offset + length;
                    let text_end = end - usize::from(self.text.as_bytes()[end - 1] == b'\n');
                    row.push(*offset..text_end);
                    *offset = end;
                    delivered += 1;
                    bytes += length;
                    if delivered >= limits.rows || bytes >= limits.text_bytes {
                        break;
                    }
                }
                Ok(())
            }
        }
    }

    /// Reuse the cached page when reopening, or extend an already open section by one batch.
    pub fn section(
        &mut self,
        target: &SavedDiffTarget,
        expanded: bool,
        limits: ChunkLimits,
    ) -> Result<(), ContractError> {
        if let Some(identity) = &target.declaration {
            let file = self
                .file
                .iter_mut()
                .find(|file| file.key == target.file)
                .ok_or(ContractError("saved source file disappeared"))?;
            let SavedFileContent::Source {
                projection: Some(projection),
                ..
            } = &mut file.content
            else {
                return Err(ContractError("saved declaration has no prepared sources"));
            };
            projection.declaration(identity, expanded, limits)
        } else if expanded {
            let file = self
                .file
                .iter_mut()
                .find(|file| file.key == target.file)
                .ok_or(ContractError("saved source file disappeared"))?;
            let cached = !file.expanded
                && match &file.content {
                    SavedFileContent::Source {
                        projection: Some(projection),
                        ..
                    } => projection.options() == self.options && projection.materialized(),
                    SavedFileContent::Raw { source, offset, .. } => {
                        *offset > source.start || *offset == source.end
                    }
                    SavedFileContent::Source {
                        projection: None, ..
                    } => false,
                };
            file.expanded = true;
            if cached {
                Ok(())
            } else {
                self.demand(&target.file, limits)
            }
        } else {
            self.file
                .iter_mut()
                .find(|file| file.key == target.file)
                .ok_or(ContractError("saved source file disappeared"))?
                .expanded = false;
            Ok(())
        }
    }

    /// Render shared headings and folds, retaining targets for native on-demand expansion.
    /// Closed files emit their heading and boundary without formatting cached source bodies.
    pub fn render(&self, prefix: &str) -> Result<SavedDiffFrame, ContractError> {
        let presenter = DiffPresenter::new(PresentationOptions::default());
        let mut frame = SavedDiffFrame {
            block: Vec::new(),
            target: BTreeMap::new(),
        };
        for file in &self.file {
            let prefix = format!("{prefix}:file:{}", file.key);
            let identity = FoldId(prefix.clone());
            let start = frame.block.len();
            let mut header = presenter.file(
                BlockId(prefix.clone()),
                file.change,
                &file.display_path.replace(['\n', '\r'], " "),
                Some(file.counts),
            )?;
            header.metadata.content_node = Some(identity.clone());
            header.metadata.section.push(DeferredSection {
                id: identity.clone(),
                revision: 0,
                open: file.expanded,
                more: file.more(),
            });
            attach_target_map(
                &mut frame.target,
                &mut header,
                SavedDiffTarget {
                    file: file.key.clone(),
                    declaration: None,
                },
            );
            frame.block.push(header);
            if file.expanded {
                if matches!(file.content, SavedFileContent::Source { .. })
                    && !file.metadata.is_empty()
                {
                    frame.block.push(BufferBlock {
                        id: BlockId(format!("{prefix}:metadata")),
                        text: BufferText::from_rows(
                            file.metadata
                                .iter()
                                .map(|range| self.text[range.clone()].trim_end_matches('\r')),
                        )?,
                        metadata: BlockMetadata::default(),
                    });
                }
                match &file.content {
                    SavedFileContent::Source {
                        projection: Some(projection),
                        diagnostic,
                        ..
                    } => {
                        if self.options != DiffViewOptions::default()
                            && let Some(diagnostic) = diagnostic
                        {
                            frame.block.push(BufferBlock {
                                id: BlockId(format!("{prefix}:diagnostic")),
                                text: BufferText::from_rows(
                                    format!("Unclassified source changes: {diagnostic}").lines(),
                                )?,
                                metadata: BlockMetadata::default(),
                            });
                        }
                        frame
                            .block
                            .extend(projection.render(&prefix, |element, block| {
                                if let SourceDiffElement::Declaration(signature) = element {
                                    attach_target_map(
                                        &mut frame.target,
                                        block,
                                        SavedDiffTarget {
                                            file: file.key.clone(),
                                            declaration: Some(signature.identity.clone()),
                                        },
                                    );
                                }
                                Ok(())
                            })?);
                    }
                    SavedFileContent::Source {
                        projection: None, ..
                    } => {}
                    SavedFileContent::Raw {
                        diagnostic, row, ..
                    } => {
                        if self.options != DiffViewOptions::default() {
                            frame.block.push(BufferBlock {
                                id: BlockId(format!("{prefix}:fallback")),
                                text: BufferText::from_rows(
                                    format!("Unclassified source changes: {diagnostic}").lines(),
                                )?,
                                metadata: BlockMetadata::default(),
                            });
                        }
                        for (index, batch) in row.chunks(128).enumerate() {
                            frame.block.push(BufferBlock {
                                id: BlockId(format!("{prefix}:raw:{index}")),
                                text: BufferText::from_rows(
                                    batch.iter().map(|range| {
                                        self.text[range.clone()].trim_end_matches('\r')
                                    }),
                                )?,
                                metadata: BlockMetadata::default(),
                            });
                        }
                    }
                }
                if file.more() {
                    let mut more = BufferBlock {
                        id: BlockId(format!("{prefix}:more")),
                        text: BufferText::from_rows(["Additional diff rows are available"])?,
                        metadata: BlockMetadata {
                            content_node: Some(identity.clone()),
                            ..Default::default()
                        },
                    };
                    attach_target_map(
                        &mut frame.target,
                        &mut more,
                        SavedDiffTarget {
                            file: file.key.clone(),
                            declaration: None,
                        },
                    );
                    frame.block.push(more);
                }
            }
            for block in &mut frame.block[start..] {
                if block.metadata.content_node.is_none() {
                    block.metadata.content_node = Some(identity.clone());
                }
            }
            let boundary = BufferBlock {
                id: BlockId(format!("{prefix}:end")),
                text: BufferText::default(),
                metadata: BlockMetadata::default(),
            };
            DiffPresenter::fold(
                &mut frame.block[start],
                identity,
                BlockAnchor {
                    block: boundary.id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                },
                FoldPolicy {
                    closed: !file.expanded,
                    ..Default::default()
                },
            );
            frame.block.push(boundary);
        }
        if frame.block.is_empty() {
            frame.block.push(BufferBlock {
                id: BlockId(format!("{prefix}:empty")),
                text: BufferText::from_rows(["No changed files"])?,
                metadata: BlockMetadata::default(),
            });
        }
        Ok(frame)
    }
}

impl SavedFileProjection {
    fn more(&self) -> bool {
        match &self.content {
            SavedFileContent::Source { projection, .. } => projection
                .as_ref()
                .is_none_or(|projection| projection.more()),
            SavedFileContent::Raw { source, offset, .. } => *offset < source.end,
        }
    }
}

fn metadata_ranges(file: &PatchFile<'_>, source: &str) -> Vec<Range<usize>> {
    let mut result = Vec::new();
    let mut offset = file.source_range.start;
    for line in source[file.source_range.clone()].split_inclusive('\n') {
        if [
            "old mode ",
            "new mode ",
            "new file mode ",
            "deleted file mode ",
            "similarity index ",
            "dissimilarity index ",
            "copy from ",
            "copy to ",
        ]
        .iter()
        .any(|prefix| line.starts_with(prefix))
        {
            result.push(offset..offset + line.len() - usize::from(line.ends_with('\n')));
        }
        offset += line.len();
    }
    result
}

fn file_change(file: &PatchFile<'_>) -> FileChange {
    if file.old_path.is_none() {
        FileChange::Added
    } else if file.new_path.is_none() {
        FileChange::Deleted
    } else if file.old_path != file.new_path {
        FileChange::Renamed
    } else {
        FileChange::Modified
    }
}

fn attach_target_map(
    target_map: &mut BTreeMap<TargetId, SavedDiffTarget>,
    block: &mut BufferBlock,
    target: SavedDiffTarget,
) {
    let id = TargetId(block.id.0.clone());
    target_map.insert(id.clone(), target);
    block.metadata.target.push(TargetRange {
        id,
        range: TextRange {
            start: TextPosition { row: 0, column: 0 },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
    });
}
