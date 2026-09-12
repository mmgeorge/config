use std::ops::ControlFlow;
use std::sync::{Arc, OnceLock};

use tree_sitter::{
    Node, ParseOptions, Parser, Point, QueryCursor, QueryCursorOptions, QueryPredicateArg, Range,
    StreamingIterator, Tree,
};

use crate::source::{SourceIdentity, SourceVersion};
use crate::workers::WorkBudget;

use super::query::{CompiledQuery, source};
use super::service::{SyntaxCharge, stop_error};
use super::{SyntaxError, SyntaxLanguage, SyntaxLimits};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SyntaxPoint {
    pub row: usize,
    pub column: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SyntaxRange {
    pub start_byte: usize,
    pub end_byte: usize,
    pub start: SyntaxPoint,
    pub end: SyntaxPoint,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SyntaxFamily {
    Highlight,
    Local,
    Injection,
    Context,
}

#[derive(Clone, Debug)]
pub struct SyntaxCapture {
    pub family: SyntaxFamily,
    pub language: SyntaxLanguage,
    pub name: Arc<str>,
    pub range: SyntaxRange,
    pub pattern: usize,
    pub tree: usize,
    pub priority: u16,
    pub conceal: Option<Arc<str>>,
    pub conceal_line: bool,
    pub url: Option<SyntaxRange>,
}

#[derive(Clone, Debug)]
pub struct SyntaxInjection {
    pub language: String,
    pub range: Vec<SyntaxRange>,
    pub available: bool,
}

#[derive(Clone)]
pub struct SyntaxHandle(pub(super) Arc<ParsedSyntax>);

pub(super) struct ParsedSyntax {
    pub(super) source: SourceVersion,
    language: SyntaxLanguage,
    capture: Vec<SyntaxCapture>,
    index: Vec<CaptureIndex>,
    injection: Vec<SyntaxInjection>,
    pub(super) tree: Vec<Tree>,
    pub(super) line: Vec<usize>,
    _charge: SyntaxCharge,
}

struct CaptureIndex {
    capture: usize,
    maximum_end: usize,
}

struct CaptureRows<'a> {
    capture: &'a [SyntaxCapture],
    index: &'a [CaptureIndex],
    pending: Vec<(usize, usize)>,
    start: usize,
    end: usize,
}

struct LanguageQuery {
    query: CompiledQuery,
    names: Vec<Arc<str>>,
}

struct ParseSession<'a> {
    parsed: ParsedSyntax,
    budget: &'a WorkBudget,
    limits: SyntaxLimits,
    line: Vec<usize>,
}

impl SyntaxHandle {
    pub fn source_identity(&self) -> SourceIdentity {
        self.0.source.identity()
    }
    pub fn language(&self) -> SyntaxLanguage {
        self.0.language
    }
    pub fn captures(&self) -> &[SyntaxCapture] {
        &self.0.capture
    }
    /// Visits captures intersecting an end-exclusive source row range without scanning the document.
    ///
    /// An interval index includes captures starting before the requested range. Consumers can
    /// stop iteration at their decoration budget without collecting the remaining matches.
    pub fn captures_intersecting_rows(
        &self,
        start: usize,
        end: usize,
    ) -> impl Iterator<Item = &SyntaxCapture> {
        CaptureRows {
            capture: &self.0.capture,
            index: &self.0.index,
            pending: if start < end {
                vec![(0, self.0.index.len())]
            } else {
                Vec::new()
            },
            start,
            end,
        }
    }
    pub fn injections(&self) -> &[SyntaxInjection] {
        &self.0.injection
    }
    pub fn tree_count(&self) -> usize {
        self.0.tree.len()
    }
    pub fn source(&self) -> &SourceVersion {
        &self.0.source
    }
}

pub(super) fn parse(
    source: SourceVersion,
    language: SyntaxLanguage,
    limits: SyntaxLimits,
    budget: &WorkBudget,
    charge: SyntaxCharge,
) -> Result<ParsedSyntax, SyntaxError> {
    let line = std::iter::once(0)
        .chain(
            source
                .bytes()
                .iter()
                .enumerate()
                .filter_map(|(index, byte)| (*byte == b'\n').then_some(index + 1)),
        )
        .collect();
    let mut session = ParseSession {
        parsed: ParsedSyntax {
            source,
            language,
            capture: Vec::new(),
            index: Vec::new(),
            injection: Vec::new(),
            tree: Vec::new(),
            line: Vec::new(),
            _charge: charge,
        },
        budget,
        limits,
        line,
    };
    budget.check().map_err(stop_error)?;
    session.parse_language(language, &[], 0)?;
    session.parsed.index = capture_index(&session.parsed.capture);
    session.parsed.line = session.line;
    let unused_capture_bytes = limits
        .captures
        .saturating_sub(session.parsed.capture.capacity())
        .saturating_mul(std::mem::size_of::<SyntaxCapture>());
    let unused_index_bytes = limits
        .captures
        .saturating_sub(session.parsed.index.capacity())
        .saturating_mul(std::mem::size_of::<CaptureIndex>());
    session
        .parsed
        ._charge
        .release_unused(unused_capture_bytes.saturating_add(unused_index_bytes));
    Ok(session.parsed)
}

impl<'a> Iterator for CaptureRows<'a> {
    type Item = &'a SyntaxCapture;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((start, end)) = self.pending.pop() {
            if start == end {
                continue;
            }
            let middle = start + (end - start) / 2;
            let node = &self.index[middle];
            if node.maximum_end <= self.start {
                continue;
            }
            let capture = &self.capture[node.capture];
            if capture.range.start.row < self.end {
                self.pending.push((middle + 1, end));
            }
            self.pending.push((start, middle));
            if capture.range.start.row < self.end && capture_end_row(capture) > self.start {
                return Some(capture);
            }
        }
        None
    }
}

fn capture_end_row(capture: &SyntaxCapture) -> usize {
    if capture.range.end.column == 0 && capture.range.end.row > capture.range.start.row {
        capture.range.end.row
    } else {
        capture.range.end.row.saturating_add(1)
    }
}

fn capture_index(capture: &[SyntaxCapture]) -> Vec<CaptureIndex> {
    fn fill(index: &mut [CaptureIndex], capture: &[SyntaxCapture]) -> usize {
        if index.is_empty() {
            return 0;
        }
        let middle = index.len() / 2;
        let (left, tail) = index.split_at_mut(middle);
        let (node, right) = tail.split_first_mut().unwrap();
        node.maximum_end = capture_end_row(&capture[node.capture])
            .max(fill(left, capture))
            .max(fill(right, capture));
        node.maximum_end
    }
    let mut index: Vec<_> = (0..capture.len())
        .map(|capture| CaptureIndex {
            capture,
            maximum_end: 0,
        })
        .collect();
    index.sort_unstable_by_key(|index| (capture[index.capture].range.start.row, index.capture));
    fill(&mut index, capture);
    index
}

impl ParseSession<'_> {
    fn parse_language(
        &mut self,
        language: SyntaxLanguage,
        included: &[Range],
        depth: usize,
    ) -> Result<(), SyntaxError> {
        self.budget.check().map_err(stop_error)?;
        if depth > self.limits.injection_depth
            || self.parsed.tree.len() >= self.limits.injection_trees
        {
            return Err(SyntaxError::InjectionLimit);
        }
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Language(error.to_string()))?;
        parser
            .set_included_ranges(included)
            .map_err(|error| SyntaxError::Language(error.to_string()))?;
        let budget = self.budget;
        let mut progress = |_: &tree_sitter::ParseState| {
            if budget.check().is_ok() {
                ControlFlow::Continue(())
            } else {
                ControlFlow::Break(())
            }
        };
        let bytes = self.parsed.source.bytes();
        let tree = parser
            .parse_with_options(
                &mut |offset, _| bytes.get(offset..).unwrap_or_default(),
                None,
                Some(ParseOptions::new().progress_callback(&mut progress)),
            )
            .ok_or_else(|| {
                budget
                    .check()
                    .err()
                    .map(stop_error)
                    .unwrap_or(SyntaxError::WorkerFailed)
            })?;
        let tree_index = self.parsed.tree.len();
        self.parsed.tree.push(tree.clone());
        let mut injection: Vec<(usize, String, Vec<SyntaxRange>)> = Vec::new();
        for (family, kind) in [
            (SyntaxFamily::Highlight, "highlights"),
            (SyntaxFamily::Local, "locals"),
            (SyntaxFamily::Context, "diff_context"),
            (SyntaxFamily::Injection, "injections"),
        ] {
            let query = language_query(language, kind)?;
            let mut cursor = QueryCursor::new();
            cursor.set_match_limit(4096);
            let mut query_progress = |_: &tree_sitter::QueryCursorState| {
                if budget.check().is_ok() {
                    ControlFlow::Continue(())
                } else {
                    ControlFlow::Break(())
                }
            };
            let mut matches = cursor.matches_with_options(
                &query.query.query,
                tree.root_node(),
                self.parsed.source.bytes(),
                QueryCursorOptions::new().progress_callback(&mut query_progress),
            );
            while let Some(matched) = matches.next() {
                budget.check().map_err(stop_error)?;
                if !query.query.accepts(matched, self.parsed.source.bytes()) {
                    continue;
                }
                let properties = query.query.query.property_settings(matched.pattern_index);
                let mut injected_language = properties
                    .iter()
                    .find(|property| property.key.as_ref() == "injection.language")
                    .and_then(|property| property.value.as_deref())
                    .map(str::to_owned);
                let mut ranges = Vec::new();
                for capture in matched.captures() {
                    let name = &query.names[capture.index as usize];
                    let directives = query.query.query.general_predicates(matched.pattern_index);
                    let range = self.capture_range(capture.node, capture.index, directives)?;
                    let mut url = None;
                    for directive in query.query.query.general_predicates(matched.pattern_index) {
                        if !matches!(directive.args.first(), Some(QueryPredicateArg::Capture(index)) if *index == capture.index)
                        {
                            continue;
                        }
                        match directive.operator.as_ref() {
                            "forge-url!" => {
                                if let Some(QueryPredicateArg::Capture(target)) =
                                    directive.args.get(1)
                                {
                                    url = matched
                                        .captures()
                                        .iter()
                                        .find(|capture| capture.index == *target)
                                        .map(|capture| {
                                            self.capture_range(
                                                capture.node,
                                                capture.index,
                                                directives,
                                            )
                                        })
                                        .transpose()?;
                                }
                            }
                            "set-lang-from-info-string!" => {
                                injected_language = self
                                    .parsed
                                    .source
                                    .text()
                                    .get(capture.node.byte_range())
                                    .and_then(|text| text.split_whitespace().next())
                                    .map(|text| text.trim_matches(['{', '}', '.']).to_owned())
                            }
                            "set-lang-from-mimetype!" => {
                                injected_language = mime_language(
                                    self.parsed
                                        .source
                                        .text()
                                        .get(capture.node.byte_range())
                                        .unwrap_or_default(),
                                )
                                .map(str::to_owned)
                            }
                            _ => {}
                        }
                    }
                    if family == SyntaxFamily::Injection {
                        if name.as_ref() == "injection.language" {
                            injected_language = self
                                .parsed
                                .source
                                .text()
                                .get(capture.node.byte_range())
                                .map(str::to_owned);
                        }
                        if name.as_ref() == "injection.content" && range.start_byte < range.end_byte
                        {
                            let include_children = properties.iter().any(|property| {
                                property.key.as_ref() == "injection.include-children"
                            });
                            ranges.extend(injection_ranges(capture.node, range, include_children));
                        }
                    }
                    if name.starts_with('_') && url.is_none() {
                        continue;
                    }
                    if self.parsed.capture.len() >= self.limits.captures {
                        return Err(SyntaxError::CaptureLimit);
                    }
                    let applicable = |property: &&tree_sitter::QueryProperty| {
                        property
                            .capture_id
                            .is_none_or(|index| index == capture.index as usize)
                    };
                    let priority = properties
                        .iter()
                        .filter(applicable)
                        .find(|property| property.key.as_ref() == "priority")
                        .and_then(|property| property.value.as_deref())
                        .and_then(|value| value.parse().ok())
                        .unwrap_or(100);
                    let conceal = properties
                        .iter()
                        .filter(applicable)
                        .find(|property| property.key.as_ref() == "conceal")
                        .and_then(|property| property.value.as_deref())
                        .map(Arc::from);
                    let conceal_line = properties.iter().filter(applicable).any(|property| {
                        property.key.as_ref() == "conceal_lines"
                            || property.key.as_ref() == "conceal_line"
                    });
                    self.parsed.capture.push(SyntaxCapture {
                        family,
                        language,
                        name: Arc::clone(name),
                        range,
                        pattern: matched.pattern_index,
                        tree: tree_index,
                        priority,
                        conceal,
                        conceal_line,
                        url,
                    });
                }
                if family == SyntaxFamily::Injection
                    && let Some(language) = injected_language
                    && !ranges.is_empty()
                {
                    let combined = properties
                        .iter()
                        .any(|property| property.key.as_ref() == "injection.combined");
                    if combined
                        && let Some((_, _, existing)) =
                            injection.iter_mut().find(|(pattern, name, _)| {
                                *pattern == matched.pattern_index && *name == language
                            })
                    {
                        existing.extend(ranges);
                    } else {
                        injection.push((matched.pattern_index, language, ranges));
                    }
                }
            }
            drop(matches);
            if cursor.did_exceed_match_limit() {
                return Err(SyntaxError::CaptureLimit);
            }
        }
        for (_, name, mut range) in injection {
            range.sort_by_key(|range| range.start_byte);
            range.dedup();
            let injected = SyntaxLanguage::from_name(&name);
            self.parsed.injection.push(SyntaxInjection {
                language: name,
                range: range.clone(),
                available: injected.is_some(),
            });
            if let Some(injected) = injected {
                let included: Vec<_> = range.iter().copied().map(native_range).collect();
                if language == injected
                    && included == self.parsed.tree[tree_index].included_ranges()
                {
                    continue;
                }
                self.parse_language(injected, &included, depth + 1)?;
            }
        }
        Ok(())
    }

    fn capture_range(
        &self,
        node: Node<'_>,
        capture: u32,
        directive: &[tree_sitter::QueryPredicate],
    ) -> Result<SyntaxRange, SyntaxError> {
        let mut range = node_range(node);
        for directive in directive {
            if directive.operator.as_ref() != "offset!"
                || !matches!(directive.args.first(), Some(QueryPredicateArg::Capture(index)) if *index == capture)
            {
                continue;
            }
            let offset: [i32; 4] = std::array::from_fn(|index| match &directive.args[index + 1] {
                QueryPredicateArg::String(value) => value.parse().unwrap(),
                _ => unreachable!(),
            });
            range = self.offset_range(range, &offset)?;
        }
        Ok(range)
    }

    fn offset_range(&self, range: SyntaxRange, offset: &[i32]) -> Result<SyntaxRange, SyntaxError> {
        let point = |point: SyntaxPoint,
                     row: i32,
                     column: i32|
         -> Result<(SyntaxPoint, usize), SyntaxError> {
            let row = point
                .row
                .checked_add_signed(row as isize)
                .ok_or_else(|| SyntaxError::Query("negative offset row".into()))?;
            let column = point
                .column
                .checked_add_signed(column as isize)
                .ok_or_else(|| SyntaxError::Query("negative offset column".into()))?;
            let byte = self
                .line
                .get(row)
                .and_then(|start| start.checked_add(column))
                .filter(|byte| *byte <= self.parsed.source.bytes().len())
                .ok_or_else(|| SyntaxError::Query("offset outside source".into()))?;
            Ok((SyntaxPoint { row, column }, byte))
        };
        let (start, start_byte) = point(range.start, offset[0], offset[1])?;
        let (end, end_byte) = point(range.end, offset[2], offset[3])?;
        if start_byte > end_byte {
            return Err(SyntaxError::Query("inverted offset range".into()));
        }
        Ok(SyntaxRange {
            start_byte,
            end_byte,
            start,
            end,
        })
    }
}

fn language_query(language: SyntaxLanguage, kind: &str) -> Result<Arc<LanguageQuery>, SyntaxError> {
    type Slot = OnceLock<Result<Arc<LanguageQuery>, SyntaxError>>;
    static QUERY: OnceLock<Vec<[Slot; 4]>> = OnceLock::new();
    let cache = QUERY.get_or_init(|| {
        SyntaxLanguage::ALL
            .iter()
            .map(|_| std::array::from_fn(|_| OnceLock::new()))
            .collect()
    });
    let index = SyntaxLanguage::ALL
        .iter()
        .position(|candidate| *candidate == language)
        .unwrap();
    let family = match kind {
        "highlights" => 0,
        "locals" => 1,
        "injections" => 2,
        _ => 3,
    };
    cache[index][family]
        .get_or_init(|| {
            let query = CompiledQuery::new(&language.grammar(), &source(language.name(), kind))?;
            let names = query
                .query
                .capture_names()
                .iter()
                .map(|name| Arc::from(*name))
                .collect();
            Ok(Arc::new(LanguageQuery { query, names }))
        })
        .clone()
}

fn node_range(node: Node<'_>) -> SyntaxRange {
    SyntaxRange {
        start_byte: node.start_byte(),
        end_byte: node.end_byte(),
        start: SyntaxPoint {
            row: node.start_position().row,
            column: node.start_position().column,
        },
        end: SyntaxPoint {
            row: node.end_position().row,
            column: node.end_position().column,
        },
    }
}

fn native_range(range: SyntaxRange) -> Range {
    Range {
        start_byte: range.start_byte,
        end_byte: range.end_byte,
        start_point: Point::new(range.start.row, range.start.column),
        end_point: Point::new(range.end.row, range.end.column),
    }
}

fn injection_ranges(
    node: Node<'_>,
    range: SyntaxRange,
    include_children: bool,
) -> Vec<SyntaxRange> {
    if include_children {
        return vec![range];
    }
    let mut result = Vec::new();
    let mut remaining = range;
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        let child = node_range(child);
        if child.end_byte <= remaining.start_byte || child.start_byte >= remaining.end_byte {
            continue;
        }
        if remaining.start_byte < child.start_byte {
            result.push(SyntaxRange {
                end_byte: child.start_byte,
                end: child.start,
                ..remaining
            });
        }
        remaining.start_byte = child.end_byte;
        remaining.start = child.end;
    }
    if remaining.start_byte < remaining.end_byte {
        result.push(remaining);
    }
    result
}

fn mime_language(mime: &str) -> Option<&str> {
    match mime {
        "module"
        | "text/javascript"
        | "application/javascript"
        | "text/ecmascript"
        | "application/ecmascript" => Some("javascript"),
        "text/typescript" | "application/typescript" => Some("typescript"),
        "application/json" | "application/ld+json" | "importmap" => Some("json"),
        "text/css" => Some("css"),
        "text/html" => Some("html"),
        _ => None,
    }
}
