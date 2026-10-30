//! Retained source presentations publish headings before demand-loaded implementation rows.

use std::{collections::BTreeSet, sync::Arc};

use forge_buffer::{
    ContractError,
    block::{
        BlockAnchor, BlockMetadata, BufferBlock, DeferredSection, FoldRange, SourceAnchor,
        TextPosition,
    },
    identity::{BlockId, FoldId},
    text::BufferText,
};
use sha2::{Digest, Sha256};

use crate::{
    cache::AnalysisHandle,
    display::{ChunkLimits, DisplayHunk, DisplayRow, DisplayState},
    presentation::{DiffPresenter, HunkHeading, PresentationOptions, declaration_body_summary},
    semantic_view::{SourceComparison, SourceDeclarationNode},
    source_view::{DeclarationSignature, SourceViewCursor},
    syntax::{SyntaxHandle, SyntaxLanguage},
    view::{DiffPresentation, DiffViewOptions, DiffVisibility},
};

/// Consumers attach their own actions to exact source rows and semantic headings.
pub enum SourceDiffElement<'source> {
    Hunk(&'source DisplayHunk),
    Row(&'source DisplayRow),
    Declaration(&'source DeclarationSignature),
}

/// One exact comparison retains analysis across independent visibility and presentation controls.
/// Full/All creates no declaration inventory. Semantic headings and bodies have separate demand.
#[derive(Clone)]
pub struct SourceDiffProjection {
    diff: AnalysisHandle,
    old_syntax: Option<SyntaxHandle>,
    new_syntax: Option<SyntaxHandle>,
    comparison: Option<Arc<SourceComparison>>,
    options: DiffViewOptions,
    presentation: PresentationOptions,
    full: Option<SourcePage>,
    section: Vec<SourceSection>,
    published: usize,
    fallback: Option<SourcePage>,
    expanded: BTreeSet<String>,
    expansion_quota: std::collections::BTreeMap<String, usize>,
    semantic_prepared: bool,
    alternate: Option<SourceVisibilityCache>,
}

/// Named expansion intent and previously delivered body rows, independent of source ownership.
#[derive(Clone, Default)]
pub struct SourceExpansionState {
    declaration: std::collections::BTreeMap<String, usize>,
}

#[derive(Clone)]
struct SourceSection {
    node: SourceDeclarationNode,
    signature: Option<Arc<DeclarationSignature>>,
    page: Option<SourcePage>,
}

#[derive(Clone)]
struct SourcePage {
    cursor: SourceViewCursor,
    row: Vec<Arc<DisplayRow>>,
    more: bool,
    demanded: bool,
}

#[derive(Clone)]
struct SourceVisibilityCache {
    full: Option<SourcePage>,
    section: Vec<SourceSection>,
    published: usize,
    fallback: Option<SourcePage>,
    semantic_prepared: bool,
}

impl SourceDiffProjection {
    pub fn new(
        diff: AnalysisHandle,
        old_syntax: Option<SyntaxHandle>,
        new_syntax: Option<SyntaxHandle>,
        presentation: PresentationOptions,
    ) -> Self {
        Self {
            full: Some(SourcePage::new(SourceViewCursor::full(
                diff.clone(),
                old_syntax.as_ref(),
                new_syntax.as_ref(),
            ))),
            diff,
            old_syntax,
            new_syntax,
            comparison: None,
            options: DiffViewOptions::default(),
            presentation,
            section: Vec::new(),
            published: 0,
            fallback: None,
            expanded: BTreeSet::new(),
            expansion_quota: Default::default(),
            semantic_prepared: false,
            alternate: None,
        }
    }

    /// Prepare a successor without changing the currently usable projection or reacquiring source.
    /// Call on an analysis worker. The document owner validates its request before publication.
    pub fn reconfigure(&self, options: DiffViewOptions) -> Result<Self, ContractError> {
        let mut next = self.clone();
        if options.visibility != self.options.visibility {
            let cached = next.alternate.take();
            next.alternate = Some(SourceVisibilityCache {
                full: next.full.take(),
                section: std::mem::take(&mut next.section),
                published: next.published,
                fallback: next.fallback.take(),
                semantic_prepared: next.semantic_prepared,
            });
            next.published = 0;
            next.semantic_prepared = false;
            if let Some(cached) = cached {
                next.full = cached.full;
                next.section = cached.section;
                next.published = cached.published;
                next.fallback = cached.fallback;
                next.semantic_prepared = cached.semantic_prepared;
            }
        }
        next.options = options;
        if options != DiffViewOptions::default() {
            if next.comparison.is_none() {
                next.comparison = Some(Arc::new(
                    SourceComparison::analyze(
                        &next.diff,
                        next.old_syntax.as_ref(),
                        next.new_syntax.as_ref(),
                    )
                    .map_err(|_| ContractError("semantic analysis belongs to another source"))?,
                ));
            }
            let comparison = next.comparison.as_ref().expect("prepared comparison");
            if next.full.is_none() {
                next.full = Some(SourcePage::new(
                    SourceViewCursor::new(
                        next.diff.clone(),
                        comparison.clone(),
                        None,
                        options.visibility,
                    )
                    .map_err(|_| ContractError("filtered source cursor is inconsistent"))?,
                ));
            }
            if options.presentation == DiffPresentation::Semantic && !next.semantic_prepared {
                next.section = comparison
                    .declaration_tree(options.visibility)
                    .into_iter()
                    .map(|node| SourceSection {
                        node,
                        signature: None,
                        page: None,
                    })
                    .collect();
                next.semantic_prepared = true;
            }
        } else if next.full.is_none() {
            next.full = Some(SourcePage::new(SourceViewCursor::full(
                next.diff.clone(),
                next.old_syntax.as_ref(),
                next.new_syntax.as_ref(),
            )));
        }
        if options.visibility == DiffVisibility::Public
            && self.options.visibility == DiffVisibility::All
        {
            let page = self
                .section
                .iter()
                .filter_map(|section| {
                    section
                        .page
                        .as_ref()
                        .map(|page| (section.node.change, page))
                })
                .collect::<std::collections::HashMap<_, _>>();
            let comparison = next.comparison.as_ref().expect("filtered comparison");
            for section in &mut next.section {
                if let Some(page) = page.get(&section.node.change) {
                    let mut filtered = (*page).clone();
                    filtered.cursor = filtered
                        .cursor
                        .with_visibility(options.visibility)
                        .map_err(|_| {
                            ContractError("filtered declaration cursor is inconsistent")
                        })?;
                    filtered
                        .row
                        .retain(|row| comparison.visible_row(row, options.visibility));
                    if section.page.as_ref().is_none_or(|page| {
                        filtered.row.len() > page.row.len() || !filtered.more && page.more
                    }) {
                        section.page = Some(filtered);
                    }
                }
            }
        }
        Ok(next)
    }

    pub fn options(&self) -> DiffViewOptions {
        self.options
    }

    pub fn analysis(&self) -> &AnalysisHandle {
        &self.diff
    }

    /// Identify one owner's exact source pair without hashing source bytes again.
    /// Presentation changes share this scope. Source replacement invalidates its coordinates.
    pub fn navigation_scope(&self, owner: &str) -> String {
        let mut hash = Sha256::new();
        hash.update(owner.as_bytes());
        for source in [&self.diff.source().old, &self.diff.source().new] {
            let identity = source.identity();
            hash.update(identity.content_hash);
            hash.update(format!("{:?}", identity.representation).as_bytes());
        }
        format!("{:x}", hash.finalize())
    }

    /// Replace source analysis while retaining named expansion choices and discarding old rows.
    pub fn replace_source(
        &self,
        diff: AnalysisHandle,
        old: Option<SyntaxHandle>,
        new: Option<SyntaxHandle>,
    ) -> Self {
        let mut next = Self::new(diff, old, new, self.presentation.clone());
        next.restore_expansion(self.expansion_state());
        next
    }

    /// Capture expansion choices without retaining old source cursors or formatted bodies.
    pub fn expansion_state(&self) -> SourceExpansionState {
        let mut declaration = self
            .expanded
            .iter()
            .map(|identity| {
                (
                    identity.clone(),
                    self.expansion_quota.get(identity).copied().unwrap_or(0),
                )
            })
            .collect::<std::collections::BTreeMap<_, _>>();
        for section in self
            .section
            .iter()
            .chain(self.alternate.iter().flat_map(|cache| &cache.section))
        {
            if let Some(signature) = &section.signature
                && let Some(quota) = declaration.get_mut(&signature.identity)
            {
                *quota = (*quota).max(section.page.as_ref().map_or(0, |page| page.row.len()));
            }
        }
        SourceExpansionState { declaration }
    }

    /// Restore named intent and demand each retained body quota when its signature is published.
    pub fn restore_expansion(&mut self, state: SourceExpansionState) {
        self.expanded = state.declaration.keys().cloned().collect();
        self.expansion_quota = state.declaration;
    }

    /// Read canonical Full groups without demanding source rows or semantic declarations.
    pub fn full_groups(&self) -> impl Iterator<Item = &DisplayHunk> {
        self.full
            .iter()
            .flat_map(|page| (0..).map_while(move |index| page.cursor.group(index)))
    }

    /// Count retained source rows in the active presentation, excluding generated hunk headings.
    pub fn retained_source_rows(&self) -> usize {
        if self.options.presentation == DiffPresentation::Full {
            return self.full.as_ref().map_or(0, |page| page.row.len());
        }
        self.section[..self.published]
            .iter()
            .map(|section| {
                let signature = section.signature.as_ref().expect("published declaration");
                signature.text.lines().count()
                    + if self.expanded.contains(&signature.identity) {
                        section.page.as_ref().map_or(0, |page| page.row.len())
                    } else {
                        0
                    }
            })
            .sum::<usize>()
            + self.fallback.as_ref().map_or(0, |page| page.row.len())
    }

    pub fn comparison(&self) -> Option<&Arc<SourceComparison>> {
        self.comparison.as_ref()
    }

    /// Resolve a published declaration to canonical edit regions without reading its body.
    pub fn declaration_hunks(
        &self,
        identity: &str,
    ) -> Result<Vec<crate::raw::RawHunkId>, ContractError> {
        let comparison = self
            .comparison
            .as_ref()
            .ok_or(ContractError("semantic comparison is missing"))?;
        let section = self.section[..self.published]
            .iter()
            .find(|section| {
                section
                    .signature
                    .as_ref()
                    .is_some_and(|signature| signature.identity == identity)
            })
            .ok_or(ContractError("semantic declaration is no longer published"))?;
        let change = &comparison.change[section.node.change];
        let mut region = Vec::new();
        for (inventory, index, old) in [
            (&comparison.before, change.before, true),
            (&comparison.after, change.after, false),
        ] {
            if let Some(index) = index {
                region.extend(crate::semantic_view::intersecting_edits(
                    &self.diff,
                    &inventory.declaration[index].source_bytes,
                    old,
                ));
            }
        }
        region.sort_unstable();
        region.dedup();
        Ok(region
            .into_iter()
            .map(|index| self.diff.hunks()[index].id)
            .collect())
    }

    pub fn materialized(&self) -> bool {
        match self.options.presentation {
            DiffPresentation::Full => self.full.as_ref().is_some_and(|page| page.demanded),
            DiffPresentation::Semantic => self.published > 0 || self.fallback.is_some(),
        }
    }

    /// Report document continuation independently of a selected declaration's body continuation.
    pub fn more(&self) -> bool {
        if self.options.presentation == DiffPresentation::Full {
            return self.full.as_ref().expect("prepared Full cursor").more;
        }
        self.published < self.section.len() || self.fallback.as_ref().is_none_or(|page| page.more)
    }

    /// Advance one bounded source batch, sharing its budget with semantic signatures.
    /// An empty filtered batch can retain continuation. No total-content limit is imposed.
    pub fn demand(&mut self, mut limits: ChunkLimits) -> Result<(), ContractError> {
        if self.options.presentation == DiffPresentation::Full {
            self.full
                .as_mut()
                .expect("prepared Full cursor")
                .demand(limits);
            return Ok(());
        }
        let comparison = self.comparison.as_ref().expect("semantic comparison");
        if self.published < self.section.len() {
            let mut rows = 0;
            let mut bytes = 0;
            while self.published < self.section.len() {
                let section = &mut self.section[self.published];
                let mut signature = comparison
                    .signature(section.node.change)
                    .map_err(|_| ContractError("semantic declaration is missing"))?;
                if self.options.visibility == DiffVisibility::Public {
                    signature.detail_changes = comparison.change[section.node.change]
                        .public_detail_region
                        .len();
                }
                let count = signature.text.lines().count();
                if rows > 0
                    && (rows + count > limits.rows
                        || bytes + signature.text.len() > limits.text_bytes)
                {
                    break;
                }
                rows += count;
                bytes += signature.text.len();
                if self.expanded.contains(&signature.identity) && section.page.is_none() {
                    let mut page = SourcePage::new(
                        SourceViewCursor::new(
                            self.diff.clone(),
                            comparison.clone(),
                            Some(section.node.change),
                            self.options.visibility,
                        )
                        .map_err(|_| ContractError("declaration source cursor is inconsistent"))?,
                    );
                    page.demand(limits);
                    let quota = self
                        .expansion_quota
                        .get(&signature.identity)
                        .copied()
                        .unwrap_or(0);
                    while page.more && page.row.len() < quota {
                        page.demand(limits);
                    }
                    section.page = Some(page);
                }
                section.signature = Some(Arc::new(signature));
                self.published += 1;
                if rows >= limits.rows || bytes >= limits.text_bytes {
                    break;
                }
            }
            if self.published < self.section.len()
                || rows >= limits.rows
                || bytes >= limits.text_bytes
            {
                return Ok(());
            }
            limits.rows -= rows;
            limits.text_bytes -= bytes;
        }
        if self.fallback.is_none() {
            self.fallback = Some(SourcePage::new(
                SourceViewCursor::unclassified(
                    self.diff.clone(),
                    comparison.clone(),
                    self.options.visibility,
                )
                .map_err(|_| ContractError("unclassified source cursor is inconsistent"))?,
            ));
        }
        self.fallback
            .as_mut()
            .expect("initialized fallback")
            .demand(limits);
        Ok(())
    }

    /// Set a declaration's shared expansion intent and deliver at most one body batch when open.
    /// Reopen a declaration at its cached page, or extend an already open body by one batch.
    pub fn declaration(
        &mut self,
        identity: &str,
        expanded: bool,
        limits: ChunkLimits,
    ) -> Result<(), ContractError> {
        if self.options.presentation != DiffPresentation::Semantic {
            return Err(ContractError("semantic presentation is not active"));
        }
        let comparison = self
            .comparison
            .as_ref()
            .ok_or(ContractError("semantic presentation is not active"))?;
        let section = self.section[..self.published]
            .iter_mut()
            .find(|section| {
                section
                    .signature
                    .as_ref()
                    .is_some_and(|signature| signature.identity == identity)
            })
            .ok_or(ContractError("semantic declaration is no longer published"))?;
        if !expanded {
            self.expanded.remove(identity);
            return Ok(());
        }
        let continuing = self.expanded.contains(identity);
        let cached = section.page.is_some();
        if !cached {
            section.page = Some(SourcePage::new(
                SourceViewCursor::new(
                    self.diff.clone(),
                    comparison.clone(),
                    Some(section.node.change),
                    self.options.visibility,
                )
                .map_err(|_| ContractError("declaration source cursor is inconsistent"))?,
            ));
        }
        if continuing || !cached {
            section
                .page
                .as_mut()
                .expect("initialized declaration body")
                .demand(limits);
        }
        self.expanded.insert(identity.to_owned());
        Ok(())
    }

    /// Render retained rows while the consumer supplies source-specific navigation and mutations.
    /// Collapsed bodies format no source rows. Heading identities survive presentation toggles.
    pub fn render(
        &self,
        prefix: &str,
        mut decorate: impl FnMut(SourceDiffElement<'_>, &mut BufferBlock) -> Result<(), ContractError>,
    ) -> Result<Vec<BufferBlock>, ContractError> {
        if self.options.presentation == DiffPresentation::Full {
            let mut block = self.render_page(
                prefix,
                self.full.as_ref().expect("prepared Full cursor"),
                None,
                &mut decorate,
            )?;
            if block.is_empty() && !self.more() {
                block.push(BufferBlock {
                    id: BlockId(format!("{prefix}:empty")),
                    text: BufferText::from_rows(["No source changes"])?,
                    metadata: BlockMetadata::default(),
                });
            }
            return Ok(block);
        }
        let navigation_scope = self.navigation_scope(prefix);
        let mut block = Vec::new();
        let mut open = Vec::<(usize, usize, FoldId)>::new();
        for (index, section) in self.section[..self.published].iter().enumerate() {
            while open.last().is_some_and(|(index, _, _)| {
                Some(self.section[*index].node.change) != section.node.parent
            }) {
                self.close_section(&mut block, open.pop().expect("open declaration"))?;
            }
            let signature = section.signature.as_ref().expect("published signature");
            let id = FoldId(format!(
                "{prefix}:declaration:{:x}",
                Sha256::digest(signature.identity.as_bytes())
            ));
            let mut heading = DiffPresenter::new(self.presentation.clone()).declaration_signature(
                BlockId(id.0.clone()),
                signature,
                self.old_syntax.as_ref(),
                self.new_syntax.as_ref(),
            )?;
            heading.metadata.content_node = Some(id.clone());
            let comparison = self
                .comparison
                .as_ref()
                .expect("prepared semantic comparison");
            let change = &comparison.change[section.node.change];
            heading.metadata.source_anchor = Some(SourceAnchor {
                owner: navigation_scope.clone(),
                before: change.before.map(|index| {
                    let declaration = &comparison.before.declaration[index];
                    declaration.start_line - 1..declaration.end_line
                }),
                after: change.after.map(|index| {
                    let declaration = &comparison.after.declaration[index];
                    declaration.start_line - 1..declaration.end_line
                }),
            });
            heading.metadata.section.push(DeferredSection {
                id: id.clone(),
                revision: 0,
                open: self.expanded.contains(&signature.identity),
                more: section.page.as_ref().is_none_or(|page| page.more),
            });
            decorate(SourceDiffElement::Declaration(signature), &mut heading)?;
            let opening = block.len();
            block.push(heading);
            if self.expanded.contains(&signature.identity)
                && let Some(page) = &section.page
            {
                block.extend(self.render_page(prefix, page, Some(&id), &mut decorate)?);
                if page.more {
                    let mut continuation = BufferBlock {
                        id: BlockId(format!("{}:more", id.0)),
                        text: BufferText::from_rows(["Additional source rows are available"])?,
                        metadata: BlockMetadata {
                            content_node: Some(id.clone()),
                            ..Default::default()
                        },
                    };
                    decorate(SourceDiffElement::Declaration(signature), &mut continuation)?;
                    block.push(continuation);
                }
            }
            open.push((index, opening, id));
        }
        while let Some(section) = open.pop() {
            self.close_section(&mut block, section)?;
        }
        if let Some(page) = &self.fallback
            && (!page.row.is_empty() || page.more)
        {
            block.push(BufferBlock {
                id: BlockId(format!("{prefix}:unclassified")),
                text: BufferText::from_rows(["Unclassified source changes"])?,
                metadata: BlockMetadata::default(),
            });
            block.extend(self.render_page(prefix, page, None, &mut decorate)?);
        }
        if block.is_empty() && !self.more() {
            block.push(BufferBlock {
                id: BlockId(format!("{prefix}:empty")),
                text: BufferText::from_rows(["No changes for the selected visibility"])?,
                metadata: BlockMetadata::default(),
            });
        }
        Ok(block)
    }

    fn render_page(
        &self,
        prefix: &str,
        page: &SourcePage,
        owner: Option<&FoldId>,
        decorate: &mut impl FnMut(SourceDiffElement<'_>, &mut BufferBlock) -> Result<(), ContractError>,
    ) -> Result<Vec<BufferBlock>, ContractError> {
        let mut presenter = DiffPresenter::new(self.presentation.clone());
        let navigation_scope = self.navigation_scope(prefix);
        let mut block = Vec::new();
        let mut opening = None;
        for (index, row) in page.row.iter().enumerate() {
            let group = page
                .cursor
                .group(row.group_index)
                .ok_or(ContractError("source display group is missing"))?;
            let scope = owner.map_or(prefix, |owner| owner.0.as_str());
            if let Some(mut heading) = presenter.begin_hunk(
                row.group_index,
                BlockId(format!("{scope}:group:{}", row.group_index)),
                group,
                HunkHeading::Ranges,
            )? {
                if let Some(previous) = opening.take() {
                    close_source_hunk(&mut block, previous);
                }
                heading.metadata.content_node = owner.cloned();
                heading.metadata.source_anchor = Some(SourceAnchor {
                    owner: navigation_scope.clone(),
                    before: (!group.old_lines.is_empty()).then(|| group.old_lines.clone()),
                    after: (!group.new_lines.is_empty()).then(|| group.new_lines.clone()),
                });
                decorate(SourceDiffElement::Hunk(group), &mut heading)?;
                opening = Some(block.len());
                block.push(heading);
            }
            let old = row
                .old_coordinate()
                .map_or_else(|| "none".into(), |coordinate| coordinate.line.to_string());
            let new = row
                .new_coordinate()
                .map_or_else(|| "none".into(), |coordinate| coordinate.line.to_string());
            let mut rendered = presenter.rows(
                BlockId(if row.is_gap() {
                    format!("{scope}:gap:{index}")
                } else {
                    format!("{prefix}:source:{old}:{new}")
                }),
                std::slice::from_ref(row),
                group,
                self.old_syntax.as_ref(),
                self.new_syntax.as_ref(),
            )?;
            rendered.metadata.content_node = owner.cloned();
            if !row.is_gap() {
                rendered.metadata.source_anchor = Some(SourceAnchor {
                    owner: navigation_scope.clone(),
                    before: row
                        .old_coordinate()
                        .map(|coordinate| coordinate.line..coordinate.line + 1),
                    after: row
                        .new_coordinate()
                        .map(|coordinate| coordinate.line..coordinate.line + 1),
                });
            }
            decorate(SourceDiffElement::Row(row), &mut rendered)?;
            block.push(rendered);
        }
        if let Some(opening) = opening {
            close_source_hunk(&mut block, opening);
        }
        Ok(block)
    }

    fn close_section(
        &self,
        block: &mut Vec<BufferBlock>,
        section: (usize, usize, FoldId),
    ) -> Result<(), ContractError> {
        let (index, opening, id) = section;
        let descriptor = &self.section[index];
        let signature = descriptor.signature.as_ref().expect("published signature");
        let end = BlockId(format!("{}:end", id.0));
        block.push(BufferBlock {
            id: end.clone(),
            text: BufferText::from_rows([""])?,
            metadata: BlockMetadata {
                content_node: Some(id.clone()),
                ..Default::default()
            },
        });
        let closing = if signature.text.ends_with('{') {
            "}"
        } else if [self.old_syntax.as_ref(), self.new_syntax.as_ref()]
            .into_iter()
            .flatten()
            .any(|syntax| syntax.language() == SyntaxLanguage::Lua)
        {
            "end"
        } else {
            ""
        };
        let start = TextPosition {
            row: block[opening].text.row_count() - 1,
            column: 0,
        };
        let heading_start = Some(BlockAnchor {
            block: block[opening].id.clone(),
            position: TextPosition { row: 0, column: 0 },
        });
        block[opening].metadata.fold.push(FoldRange {
            id,
            start,
            heading_start,
            end: BlockAnchor {
                block: end,
                position: TextPosition { row: 1, column: 0 },
            },
            collapsed_suffix: Some(declaration_body_summary(signature.detail_changes, closing)),
            closed: !self.expanded.contains(&signature.identity),
            collapse_children: false,
            expand_children: false,
        });
        Ok(())
    }
}

fn close_source_hunk(block: &mut Vec<BufferBlock>, opening: usize) {
    let id = FoldId(block[opening].id.0.clone());
    let boundary = BlockId(format!("{}:end", id.0));
    DiffPresenter::fold(
        &mut block[opening],
        id.clone(),
        BlockAnchor {
            block: boundary.clone(),
            position: TextPosition { row: 0, column: 0 },
        },
        Default::default(),
    );
    block.push(BufferBlock {
        id: boundary,
        text: Default::default(),
        metadata: BlockMetadata {
            content_node: Some(id),
            ..Default::default()
        },
    });
}

impl SourcePage {
    fn new(cursor: SourceViewCursor) -> Self {
        Self {
            cursor,
            row: Vec::new(),
            more: true,
            demanded: false,
        }
    }
    fn demand(&mut self, limits: ChunkLimits) {
        if !self.more {
            return;
        }
        self.demanded = true;
        let chunk = self.cursor.next_rows(limits);
        self.more = chunk.state == DisplayState::More;
        self.row.extend(chunk.rows.into_iter().map(Arc::new));
    }
}
