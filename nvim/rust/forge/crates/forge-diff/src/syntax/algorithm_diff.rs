use std::borrow::Cow;

use super::{
    AlgorithmDiagnostic, AlgorithmInvocation, AlgorithmReference, AlgorithmValidationError,
    SyntaxError, algorithm_reference, algorithm_references, validate_algorithm,
};

/// Role of one authored Change row after removal of the behavior block's outer indentation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AlgorithmDiffLineKind {
    /// Statement present only before the change.
    Removed,
    /// Statement present only after the change.
    Added,
    /// Statement shared by both sides.
    Context,
    /// Top-level elision separating independent hunks.
    Separator,
    /// Unprefixed anchor comment shared by both sides.
    Comment,
    /// Whitespace-only row.
    Blank,
    /// Row outside the diff syntax, including legacy prose.
    Invalid,
}

/// Authored Change row split at its diff prefix.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AlgorithmDiffLine<'source> {
    /// Role of the row in the projections.
    pub kind: AlgorithmDiffLineKind,
    /// Source after the diff prefix, including statement indentation.
    pub payload: &'source str,
    /// Byte width of the diff prefix.
    pub prefix_width: usize,
    /// Zero-based byte column of a separator or prefixed elision in the authored row.
    pub elision_column: Option<usize>,
}

/// Classify a Change row using its authored prefix column.
pub fn classify_algorithm_diff_line(line: &str) -> AlgorithmDiffLine<'_> {
    use AlgorithmDiffLineKind::*;

    let (kind, payload) = if line.trim().is_empty() {
        (Blank, line)
    } else if let Some(payload) = line.strip_prefix("- ") {
        (Removed, payload)
    } else if let Some(payload) = line.strip_prefix("+ ") {
        (Added, payload)
    } else if let Some(payload) = line.strip_prefix("  ") {
        (Context, payload)
    } else if line.starts_with('#') {
        (Comment, line)
    } else if is_elision(line) {
        (Separator, line)
    } else {
        (Invalid, line)
    };
    let prefix_width = line.len() - payload.len();
    let statement = payload.trim_start();
    let elision_column = (kind == Separator || prefix_width > 0 && is_elision(statement))
        .then_some(line.len() - statement.len());
    AlgorithmDiffLine {
        kind,
        payload,
        prefix_width,
        elision_column,
    }
}

/// Validate both sides of each Change hunk and collect authored invocation sites.
///
/// Diagnostics and invocations use one-based lines and UTF-8 byte columns in the Change.
/// A shared site appears once. Parser failures propagate as syntax errors.
pub fn validate_algorithm_diff(
    source: &str,
) -> Result<Vec<AlgorithmInvocation>, AlgorithmValidationError> {
    use AlgorithmDiffLineKind::*;

    let rows = source
        .lines()
        .map(classify_algorithm_diff_line)
        .collect::<Vec<_>>();
    let mut diagnostics = Vec::new();
    let mut invocations = Vec::new();
    let mut has_delta = false;
    for (index, row) in rows.iter().enumerate() {
        has_delta |= matches!(row.kind, Added | Removed);
        let violation = if row.kind == Invalid {
            Some((
                1,
                "start Change lines with `- `, `+ `, two spaces of context, `#`, or `...`",
            ))
        } else if row.prefix_width > 0 && row.payload.trim().is_empty() {
            Some((
                row.prefix_width + 1,
                "write a statement or comment after the diff prefix",
            ))
        } else if row.prefix_width > 0 && row.elision_column == Some(row.prefix_width) {
            Some((
                row.prefix_width + 1,
                "write top-level `...` without a diff prefix",
            ))
        } else {
            None
        };
        if let Some((column, message)) = violation {
            diagnostics.push(AlgorithmDiagnostic {
                line: index + 1,
                column,
                message: message.into(),
            });
        }
    }
    if !has_delta && diagnostics.is_empty() {
        diagnostics.push(AlgorithmDiagnostic {
            line: 1,
            column: 1,
            message: "include at least one removed or added line in Change".into(),
        });
    }
    let mut start = 0;
    for hunk in rows.split(|row| row.kind == Separator) {
        for omitted in [Added, Removed] {
            if let Some(projection) = project_hunk(hunk, omitted) {
                match validate_algorithm(&projection) {
                    Ok(sites) => invocations.extend(sites.into_iter().filter_map(|mut site| {
                        let row = hunk.get(site.line - 1)?;
                        if row.elision_column.is_some() {
                            return None;
                        }
                        site.line += start;
                        site.column += row.prefix_width;
                        site.end_column += row.prefix_width;
                        Some(site)
                    })),
                    Err(AlgorithmValidationError::Invalid(violations)) => {
                        diagnostics.extend(violations.into_iter().map(|mut violation| {
                            if let Some(row) = hunk.get(violation.line - 1) {
                                violation.column += row.prefix_width;
                            }
                            violation.line += start;
                            violation
                        }));
                    }
                    Err(error) => return Err(error),
                }
            }
        }
        start += hunk.len() + 1;
    }
    if diagnostics.is_empty() {
        invocations.sort_by_key(|site| (site.line, site.column, site.end_column));
        invocations.dedup();
        Ok(invocations)
    } else {
        diagnostics.sort_by(|left, right| {
            (left.line, left.column, &left.message).cmp(&(right.line, right.column, &right.message))
        });
        diagnostics.dedup();
        Err(AlgorithmValidationError::Invalid(diagnostics))
    }
}

/// Mask a Change row for Algorithm syntax while preserving UTF-8 byte columns.
///
/// The row starts at the authored diff column. Elisions and invalid rows have no syntax source.
pub fn algorithm_diff_source_for_highlighting(line: &str) -> Option<Cow<'_, str>> {
    use AlgorithmDiffLineKind::*;

    let row = classify_algorithm_diff_line(line);
    if row.elision_column.is_some() {
        return None;
    }
    match row.kind {
        Added | Removed => Some(Cow::Owned(format!("  {}", row.payload))),
        Context | Comment | Blank => Some(Cow::Borrowed(line)),
        Separator | Invalid => None,
    }
}

/// Enumerate Change references on both sides, positioned in the authored text.
pub fn algorithm_diff_references(source: &str) -> Result<Vec<AlgorithmReference>, SyntaxError> {
    algorithm_references(&reference_source(source))
}

/// Select a Change reference by one-based line and UTF-8 byte column.
pub fn algorithm_diff_reference(
    source: &str,
    line: usize,
    column: usize,
) -> Result<Option<AlgorithmReference>, SyntaxError> {
    algorithm_reference(&reference_source(source), line, column)
}

fn is_elision(statement: &str) -> bool {
    statement.strip_prefix("...").is_some_and(|suffix| {
        let suffix = suffix.trim_start();
        suffix.is_empty() || suffix.starts_with('#')
    })
}

fn project_hunk(rows: &[AlgorithmDiffLine<'_>], omitted: AlgorithmDiffLineKind) -> Option<String> {
    use AlgorithmDiffLineKind::*;

    let mut projection = String::new();
    let mut has_statement = false;
    let mut has_shared_elision = false;
    let mut has_omitted_statement = false;
    for row in rows {
        let statement = row.payload.trim_start();
        let authored_statement =
            !statement.is_empty() && !statement.starts_with('#') && row.elision_column.is_none();
        if row.kind == omitted {
            has_omitted_statement |= authored_statement;
        } else if !matches!(row.kind, Invalid | Separator | Blank) {
            if let Some(column) = row.elision_column {
                let indent = column - row.prefix_width;
                if indent > 0 {
                    // Match the elision's byte width so diagnostics retain authored columns.
                    projection.push_str(&row.payload[..indent]);
                    projection.push_str("f()");
                    projection.push_str(&statement[3..]);
                    has_shared_elision |= row.kind == Context;
                    has_statement |= row.kind != Context;
                }
            } else {
                projection.push_str(row.payload);
                has_statement |= authored_statement;
            }
        }
        projection.push('\n');
    }
    // A shared elision can stand for the whole opposite side when a wrapper is added or removed.
    (has_statement || has_shared_elision && !has_omitted_statement).then_some(projection)
}

fn reference_source(source: &str) -> String {
    let mut masked = String::with_capacity(source.len());
    for line in source.lines() {
        if let Some(line) = algorithm_diff_source_for_highlighting(line) {
            masked.push_str(&line);
        }
        masked.push('\n');
    }
    masked
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_independent_before_and_after_hunks() {
        for source in [
            "- return old\n+ return new",
            "- if old\n+ if new\n    process(item)",
            "  prepare(item)\n+ record(item)",
            "- discard(item)",
            "...\n# first region\n- discard(item)\n... # next region\n...\n+ retain(item)\n...",
            "+ if ready\n    ... # existing body",
            "- if ready\n    ... # existing body",
            "  for each item in items\n+   if done(item)\n+     break\n    ...",
        ] {
            assert!(validate_algorithm_diff(source).is_ok(), "{source}");
        }
        let calls =
            validate_algorithm_diff("...\n- old()\n+ if ready\n+   ...\n...\n  shared()\n+ new()")
                .unwrap();
        assert_eq!(
            calls
                .iter()
                .map(|call| (call.name.as_str(), call.line, call.column, call.end_column))
                .collect::<Vec<_>>(),
            [("old", 2, 3, 6), ("shared", 6, 3, 9), ("new", 7, 3, 6)],
        );
    }

    #[test]
    fn rejects_invalid_lines_and_control_structure_on_either_side() {
        for source in [
            "",
            "# anchor\n  process(item)\n...",
            "Update the item.",
            "-",
            "+",
            "+ ",
            "- malformed prose\n+ process(item)",
            "- process(item)\n+ malformed prose",
            "- otherwise\n-   return\n+ return",
            "- return\n+ otherwise\n+   return",
            "- break\n+ continue",
            "+ ...",
            "+ record(item)\n...\n    ...",
            "+ if ready\n+ \t...",
        ] {
            assert!(
                matches!(
                    validate_algorithm_diff(source),
                    Err(AlgorithmValidationError::Invalid(_))
                ),
                "{source:?}"
            );
        }
    }

    #[test]
    fn reports_original_byte_columns_and_deduplicates_context_errors() {
        let source =
            "- return old\n+ return new\n\n...\n# anchor\n  name ← café }\n+ process(item)";
        let Err(AlgorithmValidationError::Invalid(diagnostics)) = validate_algorithm_diff(source)
        else {
            panic!("expected syntax diagnostics");
        };
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(
            (diagnostics[0].line, diagnostics[0].column),
            (6, source.lines().nth(5).unwrap().find('}').unwrap() + 1)
        );
    }

    #[test]
    fn preserves_reference_columns_across_markers_and_elisions() {
        let source = "# Café.hidden\n- old ← Café::first()\n+ new ← Café::second()\n  Café::shared()\n... # Café.hidden\n+ if ready\n    ... # Café.hidden";
        let references = algorithm_diff_references(source).unwrap();
        assert_eq!(
            references
                .iter()
                .filter(|reference| reference.name.starts_with("Café::") && reference.last)
                .map(|reference| (reference.line, reference.column, reference.segment.as_str()))
                .collect::<Vec<_>>(),
            [(2, 18, "first"), (3, 18, "second"), (4, 10, "shared")],
        );
        assert!(
            references
                .iter()
                .all(|reference| ![1, 5, 7].contains(&reference.line))
        );
        assert_eq!(
            algorithm_diff_reference(source, 3, 18)
                .unwrap()
                .unwrap()
                .segment,
            "second"
        );
        assert!(algorithm_diff_reference(source, 3, 1).unwrap().is_none());
    }
}
