use tree_sitter::{Node, Parser};

use super::{SyntaxError, SyntaxLanguage};

#[derive(Clone, Debug, Eq, PartialEq)]
/// Explicit invocation in an Algorithm, before matching its target against Constructs.
pub struct AlgorithmInvocation {
    /// Exact qualified target, without whitespace around member separators.
    pub name: String,
    /// One-based source line within the Algorithm content.
    pub line: usize,
    /// One-based UTF-8 byte column within the Algorithm content.
    pub column: usize,
    /// Exclusive one-based UTF-8 byte column of the complete invocation target.
    pub end_column: usize,
    /// Whether any argument lacks an explicit field name.
    pub has_positional_argument: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Parsed reference under a source cursor, independent of declaration resolution.
pub struct AlgorithmReference {
    /// One-based source line of the selected identifier.
    pub line: usize,
    /// One-based UTF-8 byte column of the selected identifier.
    pub column: usize,
    /// Qualified spelling through the selected identifier.
    pub prefix: String,
    /// Qualified reference spelling without separator whitespace.
    pub name: String,
    /// Identifier segment selected by the cursor, excluding separators and whitespace.
    pub segment: String,
    /// Whether the selected identifier is the first segment of the reference.
    pub first: bool,
    /// Whether the selected identifier is the final segment of the reference.
    pub last: bool,
    /// One-based byte column immediately after the reference.
    pub end_column: usize,
    /// Whether the reference is the target of an explicit invocation.
    pub invocation: bool,
    /// Whether this token names a field in an explicit type construction.
    pub construction_field: bool,
}

/// Select a reference by one-based line and byte column, excluding prose strings and comments.
/// Complete references remain available when another part of a draft is incomplete.
pub fn algorithm_reference(
    source: &str,
    line: usize,
    column: usize,
) -> Result<Option<AlgorithmReference>, SyntaxError> {
    Ok(algorithm_references(source)?.into_iter().find(|reference| {
        reference.line == line
            && reference.column <= column
            && column < reference.column + reference.segment.len()
    }))
}

/// Enumerate parsed identifier segments, excluding strings and comments even in incomplete drafts.
pub fn algorithm_references(source: &str) -> Result<Vec<AlgorithmReference>, SyntaxError> {
    let mut parser = Parser::new();
    parser
        .set_language(&SyntaxLanguage::Algorithm.grammar())
        .map_err(|error| SyntaxError::Language(error.to_string()))?;
    let tree = parser.parse(source, None).ok_or(SyntaxError::Cancelled)?;
    let mut cursor = tree.root_node().walk();
    let mut result = Vec::new();
    loop {
        let node = cursor.node();
        let start = node.start_position();
        let end = node.end_position();
        if node.kind() == "reference" && !node.has_error() && start.row == end.row {
            let mut children = node.walk();
            let identifiers = node.named_children(&mut children).collect::<Vec<_>>();
            for (index, identifier) in identifiers.iter().enumerate() {
                result.push(AlgorithmReference {
                    line: start.row + 1,
                    column: identifier.start_position().column + 1,
                    prefix: source[node.start_byte()..identifier.end_byte()]
                        .split_whitespace()
                        .collect(),
                    name: source[node.byte_range()].split_whitespace().collect(),
                    segment: source[identifier.byte_range()].to_owned(),
                    first: index == 0,
                    last: index + 1 == identifiers.len(),
                    end_column: end.column + 1,
                    invocation: node.parent().is_some_and(|parent| {
                        parent.kind() == "invocation"
                            && !parent.has_error()
                            && parent.child_by_field_name("function") == Some(node)
                    }),
                    construction_field: false,
                });
            }
        }
        if node.kind() == "named_argument"
            && !node.has_error()
            && let Some(identifier) = node.child_by_field_name("name")
            && let Some(invocation) = node.parent().and_then(|parent| parent.parent())
            && invocation.kind() == "invocation"
            && let Some(target) = invocation.child_by_field_name("function")
        {
            let target = source[target.byte_range()]
                .split_whitespace()
                .collect::<String>();
            if target
                .rsplit([':', '.'])
                .next()
                .and_then(|name| name.chars().next())
                .is_some_and(char::is_uppercase)
            {
                let segment = source[identifier.byte_range()].to_owned();
                let name = format!("{target}.{segment}");
                result.push(AlgorithmReference {
                    line: identifier.start_position().row + 1,
                    column: identifier.start_position().column + 1,
                    prefix: name.clone(),
                    name,
                    segment,
                    first: false,
                    last: true,
                    end_column: identifier.end_position().column + 1,
                    invocation: false,
                    construction_field: true,
                });
            }
        }
        if cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                return Ok(result);
            }
        }
    }
}

/// One located Algorithm violation, positioned within the Algorithm block itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AlgorithmDiagnostic {
    /// One-based line within the Algorithm content.
    pub line: usize,
    /// One-based UTF-8 byte column within the Algorithm content.
    pub column: usize,
    /// Correction-oriented description of the violation.
    pub message: String,
}

impl std::fmt::Display for AlgorithmDiagnostic {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "Algorithm line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

/// Distinguish authored pseudocode violations from parser infrastructure failures.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AlgorithmValidationError {
    /// Every violation found in one pass, in source order.
    Invalid(Vec<AlgorithmDiagnostic>),
    /// The parser could not run, so the source was not judged.
    Syntax(SyntaxError),
}

impl From<SyntaxError> for AlgorithmValidationError {
    fn from(error: SyntaxError) -> Self {
        Self::Syntax(error)
    }
}

impl std::fmt::Display for AlgorithmValidationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid(diagnostics) => {
                let rendered = diagnostics
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>();
                formatter.write_str(&rendered.join("; "))
            }
            Self::Syntax(error) => write!(formatter, "Algorithm parsing failed: {error:?}"),
        }
    }
}

impl std::error::Error for AlgorithmValidationError {}

#[derive(Clone, Copy)]
struct Block {
    indent: usize,
    body_indent: usize,
    conditional: bool,
    looping: bool,
}

/// Statement kind, indentation, line, and whether it opens a conditional body.
type PreviousStatement<'tree> = (&'tree str, usize, usize, bool);

/// Validate pseudocode structure and return its explicit invocation sites.
/// Reports every parse and structure violation at once without modifying the source.
/// Operations require calls, and expressions exclude natural-language prose.
pub fn validate_algorithm(
    source: &str,
) -> Result<Vec<AlgorithmInvocation>, AlgorithmValidationError> {
    let language = SyntaxLanguage::Algorithm.grammar();
    let mut parser = Parser::new();
    parser
        .set_language(&language)
        .map_err(|error| SyntaxError::Language(error.to_string()))?;
    let tree = parser.parse(source, None).ok_or(SyntaxError::Cancelled)?;
    let parse_diagnostics = parse_errors(tree.root_node(), source);
    if !parse_diagnostics.is_empty() {
        return Err(AlgorithmValidationError::Invalid(parse_diagnostics));
    }
    let mut diagnostics = Vec::new();
    let mut cursor = tree.root_node().walk();
    let mut blocks = Vec::<Block>::new();
    let mut previous = None::<PreviousStatement<'_>>;
    let mut count = 0;
    for statement in tree.root_node().named_children(&mut cursor) {
        if statement.kind() == "comment" {
            continue;
        }
        let position = statement.start_position();
        let line = position.row + 1;
        let indent = position.column;
        let kind = statement.kind();
        let conditional = kind == "if_statement"
            || kind == "otherwise_statement"
                && statement.child_by_field_name("condition").is_some();
        count += 1;
        let line_start = source[..statement.start_byte()]
            .rfind('\n')
            .map_or(0, |position| position + 1);
        let tab_indented = source[line_start..statement.start_byte()].contains('\t');
        if let Some(message) = statement_violation(
            tab_indented,
            kind,
            indent,
            previous,
            &mut blocks,
            &mut diagnostics,
        ) {
            diagnostics.push(diagnostic(line, indent + 1, message));
        }
        previous = Some((kind, indent, line, conditional));
    }
    if let Some((kind, indent, line, _)) = previous
        && opens_block(kind)
    {
        diagnostics.push(diagnostic(
            line,
            indent + 1,
            "control statement requires an indented body",
        ));
    }
    if count == 0 {
        diagnostics.push(diagnostic(1, 1, "at least one statement is required"));
    }
    let invocations = invocations(tree.root_node(), source, &mut diagnostics);
    if diagnostics.is_empty() {
        Ok(invocations)
    } else {
        diagnostics.sort_by_key(|diagnostic| (diagnostic.line, diagnostic.column));
        Err(AlgorithmValidationError::Invalid(diagnostics))
    }
}

/// Check one statement against the enclosing block stack. The stack is updated even when the
/// statement is rejected, so later statements are judged against the evident intended structure.
fn statement_violation(
    tab_indented: bool,
    kind: &str,
    indent: usize,
    previous: Option<PreviousStatement<'_>>,
    blocks: &mut Vec<Block>,
    diagnostics: &mut Vec<AlgorithmDiagnostic>,
) -> Option<&'static str> {
    if tab_indented {
        return Some("use spaces for indentation");
    }
    if kind == "section_header" {
        return Some("Algorithm headings belong outside the block");
    }
    if let Some((previous_kind, previous_indent, previous_line, conditional)) = previous {
        let opens = opens_block(previous_kind);
        if indent > previous_indent {
            if !opens {
                return Some("only if, otherwise, for each, and while open an indented body");
            }
            blocks.push(Block {
                indent: previous_indent,
                body_indent: indent,
                conditional,
                looping: matches!(previous_kind, "for_statement" | "while_statement"),
            });
        } else if opens {
            diagnostics.push(diagnostic(
                previous_line,
                previous_indent + 1,
                "control statement requires an indented body",
            ));
        }
    } else if indent != 0 {
        return Some("first statement must start at the block's base indentation");
    }
    let mut closed = None;
    while blocks.last().is_some_and(|block| indent <= block.indent) {
        closed = blocks.pop();
    }
    if indent != blocks.last().map_or(0, |block| block.body_indent) {
        return Some("indentation must match an enclosing body");
    }
    if kind == "otherwise_statement" && !closed.is_some_and(|block| block.conditional) {
        return Some("otherwise must follow an if body at the same indentation");
    }
    if matches!(kind, "break_statement" | "continue_statement")
        && !blocks.iter().any(|block| block.looping)
    {
        return Some("break and continue require an enclosing loop");
    }
    None
}

fn invocations(
    root: Node<'_>,
    source: &str,
    diagnostics: &mut Vec<AlgorithmDiagnostic>,
) -> Vec<AlgorithmInvocation> {
    let mut result = Vec::new();
    let mut cursor = root.walk();
    loop {
        let node = cursor.node();
        if node.kind() == "invocation" {
            let target = node
                .child_by_field_name("function")
                .expect("invocation has a target");
            let arguments = node
                .child_by_field_name("arguments")
                .expect("invocation has arguments");
            let mut field = std::collections::HashSet::new();
            let mut has_positional_argument = false;
            let mut argument_cursor = arguments.walk();
            for argument in arguments.named_children(&mut argument_cursor) {
                if argument.kind() == "named_argument" {
                    let name = argument
                        .child_by_field_name("name")
                        .expect("named argument has a name");
                    if !field.insert(&source[name.byte_range()]) {
                        let position = name.start_position();
                        diagnostics.push(diagnostic(
                            position.row + 1,
                            position.column + 1,
                            "duplicate named argument",
                        ));
                    }
                } else {
                    has_positional_argument = true;
                }
            }
            let position = target.start_position();
            result.push(AlgorithmInvocation {
                name: source[target.byte_range()].split_whitespace().collect(),
                line: position.row + 1,
                column: position.column + 1,
                end_column: target.end_position().column + 1,
                has_positional_argument,
            });
        }
        if cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                return result;
            }
        }
    }
}

fn opens_block(kind: &str) -> bool {
    matches!(
        kind,
        "if_statement" | "otherwise_statement" | "for_statement" | "while_statement"
    )
}

fn diagnostic(line: usize, column: usize, message: impl Into<String>) -> AlgorithmDiagnostic {
    AlgorithmDiagnostic {
        line,
        column,
        message: message.into(),
    }
}

/// Describe the first outermost ERROR or MISSING node on each line by the exact text it rejected,
/// so a correction never depends on guessing which token the parser refused.
fn parse_errors(root: Node<'_>, source: &str) -> Vec<AlgorithmDiagnostic> {
    let mut result = Vec::new();
    let mut cursor = root.walk();
    loop {
        let node = cursor.node();
        let failed = node.is_error() || node.is_missing();
        let position = node.start_position();
        // Recovery after the first error on a line yields more errors that restate it.
        if failed
            && result
                .last()
                .is_none_or(|previous: &AlgorithmDiagnostic| previous.line != position.row + 1)
        {
            result.push(diagnostic(
                position.row + 1,
                position.column + 1,
                parse_error_message(node, source),
            ));
        }
        if !failed && cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                return result;
            }
        }
    }
}

fn parse_error_message(node: Node<'_>, source: &str) -> String {
    if node.is_missing() {
        return format!("expected `{}`", node.kind());
    }
    let rejected = source[node.byte_range()]
        .lines()
        .next()
        .unwrap_or_default()
        .trim();
    let mut message = format!("unexpected `{rejected}`");
    let words = rejected.split_whitespace().collect::<Vec<_>>();
    if words.len() > 1
        && words.iter().all(|word| {
            word.chars()
                .all(|character| character.is_alphanumeric() || character == '_')
        })
    {
        message.push_str(
            "; write operations as calls such as `process(item)` and explanations as # comments",
        );
    }
    message
}

#[cfg(test)]
mod tests {
    use super::{AlgorithmValidationError, validate_algorithm};

    fn messages(source: &str) -> Vec<String> {
        match validate_algorithm(source) {
            Ok(_) => Vec::new(),
            Err(AlgorithmValidationError::Invalid(diagnostics)) => diagnostics
                .into_iter()
                .map(|diagnostic| diagnostic.to_string())
                .collect(),
            Err(AlgorithmValidationError::Syntax(error)) => panic!("parser failed: {error:?}"),
        }
    }

    #[test]
    fn ascii_arrow_assigns_like_the_unicode_arrow() {
        assert_eq!(messages("speed <- base * 2"), Vec::<String>::new());
        assert_eq!(messages("speed ← base * 2"), Vec::<String>::new());
    }

    #[test]
    fn mod_is_a_remainder_operator_and_remains_an_identifier() {
        assert_eq!(
            messages("if wave mod interval = 0\n  spawn(wave)"),
            Vec::<String>::new()
        );
        assert_eq!(
            messages("if wave % interval = 0\n  spawn(wave)"),
            Vec::<String>::new()
        );
        assert_eq!(messages("mod ← 3"), Vec::<String>::new());
    }

    #[test]
    fn for_loops_accept_python_style_headers() {
        assert_eq!(
            messages(
                "for projectile in projectiles:
  advance(projectile)"
            ),
            Vec::<String>::new()
        );
        assert_eq!(
            messages(
                "for each projectile in projectiles
  advance(projectile)"
            ),
            Vec::<String>::new()
        );
    }

    #[test]
    fn one_parse_error_is_reported_per_line() {
        let reported = messages(
            "destroyed ? ? overlapping the enemy
score ← 1",
        );
        assert_eq!(reported.len(), 1, "{reported:#?}");
    }

    #[test]
    fn less_than_negative_condition_is_not_an_assignment() {
        assert_eq!(
            messages("if speed <-1\n  stop(speed)"),
            Vec::<String>::new()
        );
    }

    #[test]
    fn every_independent_violation_is_reported_with_its_text() {
        let reported = messages("score ← 1\nprocess candidate now\nlimit ← ← 2");
        assert_eq!(reported.len(), 2, "{reported:#?}");
        assert!(reported[0].starts_with("Algorithm line 2"), "{reported:#?}");
        assert!(
            reported[0].contains("`process candidate now`"),
            "{reported:#?}"
        );
        assert!(reported[0].contains("# comments"), "{reported:#?}");
        assert!(reported[1].starts_with("Algorithm line 3"), "{reported:#?}");
        assert!(reported[1].contains("unexpected `←`"), "{reported:#?}");
    }

    #[test]
    fn structural_violations_are_collected_together() {
        let reported = messages("break\nscore ← 1\n    total ← 2");
        assert_eq!(reported.len(), 2, "{reported:#?}");
        assert!(reported[0].contains("break and continue require an enclosing loop"));
        assert!(reported[1].contains("only if, otherwise, for each, and while open"));
    }
}
