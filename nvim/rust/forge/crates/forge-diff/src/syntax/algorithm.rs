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

#[derive(Clone, Copy)]
struct Block {
    indent: usize,
    body_indent: usize,
    conditional: bool,
    looping: bool,
}

/// Validate pseudocode structure and return its explicit invocation sites.
/// Returns a line-and-column diagnostic without modifying the source.
/// Operations and conditions retain natural-language expressions.
pub fn validate_algorithm(source: &str) -> Result<Vec<AlgorithmInvocation>, SyntaxError> {
    let mut parser = Parser::new();
    parser
        .set_language(&SyntaxLanguage::Algorithm.grammar())
        .map_err(|error| SyntaxError::Language(error.to_string()))?;
    let tree = parser.parse(source, None).ok_or(SyntaxError::Cancelled)?;
    if let Some(node) = first_error(tree.root_node()) {
        let position = node.start_position();
        return Err(diagnostic(
            position.row + 1,
            position.column + 1,
            "invalid pseudocode statement, expression, or delimiter",
        ));
    }
    let mut cursor = tree.root_node().walk();
    let mut blocks = Vec::<Block>::new();
    let mut previous = None::<(&str, usize, usize, bool)>;
    let mut count = 0;
    for statement in tree.root_node().named_children(&mut cursor) {
        if statement.kind() == "comment" {
            continue;
        }
        let position = statement.start_position();
        let line = position.row + 1;
        let indent = position.column;
        let line_start = source[..statement.start_byte()]
            .rfind('\n')
            .map_or(0, |position| position + 1);
        if source[line_start..statement.start_byte()].contains('\t') {
            return Err(diagnostic(line, 1, "use spaces for indentation"));
        }
        let kind = statement.kind();
        if kind == "section_header" {
            return Err(diagnostic(
                line,
                indent + 1,
                "Algorithm headings belong outside the block",
            ));
        }
        if let Some((previous_kind, previous_indent, previous_line, conditional)) = previous {
            let opens = opens_block(previous_kind);
            if indent > previous_indent {
                if !opens {
                    return Err(diagnostic(
                        line,
                        indent + 1,
                        "only if, otherwise, for each, and while open an indented body",
                    ));
                }
                blocks.push(Block {
                    indent: previous_indent,
                    body_indent: indent,
                    conditional,
                    looping: matches!(previous_kind, "for_statement" | "while_statement"),
                });
            } else if opens {
                return Err(diagnostic(
                    previous_line,
                    previous_indent + 1,
                    "control statement requires an indented body",
                ));
            }
        } else if indent != 0 {
            return Err(diagnostic(
                line,
                indent + 1,
                "first statement must start at the block's base indentation",
            ));
        }
        let mut closed = None;
        while blocks.last().is_some_and(|block| indent <= block.indent) {
            closed = blocks.pop();
        }
        if indent != blocks.last().map_or(0, |block| block.body_indent) {
            return Err(diagnostic(
                line,
                indent + 1,
                "indentation must match an enclosing body",
            ));
        }
        if kind == "otherwise_statement" && !closed.is_some_and(|block| block.conditional) {
            return Err(diagnostic(
                line,
                indent + 1,
                "otherwise must follow an if body at the same indentation",
            ));
        }
        if matches!(kind, "break_statement" | "continue_statement")
            && !blocks.iter().any(|block| block.looping)
        {
            return Err(diagnostic(
                line,
                indent + 1,
                "break and continue require an enclosing loop",
            ));
        }
        let conditional = kind == "if_statement"
            || kind == "otherwise_statement"
                && statement.child_by_field_name("condition").is_some();
        previous = Some((kind, indent, line, conditional));
        count += 1;
    }
    if let Some((kind, indent, line, _)) = previous
        && opens_block(kind)
    {
        return Err(diagnostic(
            line,
            indent + 1,
            "control statement requires an indented body",
        ));
    }
    if count == 0 {
        return Err(diagnostic(1, 1, "at least one statement is required"));
    }
    invocations(tree.root_node(), source)
}

fn invocations(root: Node<'_>, source: &str) -> Result<Vec<AlgorithmInvocation>, SyntaxError> {
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
                        return Err(diagnostic(
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
                return Ok(result);
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

fn diagnostic(line: usize, column: usize, message: &str) -> SyntaxError {
    SyntaxError::Query(format!("Algorithm line {line}, column {column}: {message}"))
}

fn first_error(root: Node<'_>) -> Option<Node<'_>> {
    let mut cursor = root.walk();
    loop {
        let node = cursor.node();
        if node.is_error() || node.is_missing() {
            return Some(node);
        }
        if cursor.goto_first_child() {
            continue;
        }
        loop {
            if cursor.goto_next_sibling() {
                break;
            }
            if !cursor.goto_parent() {
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn cursor_selects_reference_segments_and_excludes_literals() {
        let source = "direction ← movement_direction(keys)\nabilities . start_dash(direction)\nreturn \"movement_direction(keys)\" # abilities.start_dash\nunfinished(";
        for (line, token, first, last, invocation) in [
            (1, "movement_direction", true, true, true),
            (1, "keys", true, true, false),
            (2, "abilities", true, false, true),
            (2, "start_dash", false, true, true),
        ] {
            let column = source.lines().nth(line - 1).unwrap().find(token).unwrap() + 1;
            let reference = super::algorithm_reference(source, line, column)
                .unwrap()
                .unwrap();
            assert_eq!(
                (
                    reference.segment.as_str(),
                    reference.first,
                    reference.last,
                    reference.invocation
                ),
                (token, first, last, invocation)
            );
        }
        for token in ["movement_direction", "abilities"] {
            let column = source.lines().nth(2).unwrap().find(token).unwrap() + 1;
            assert!(
                super::algorithm_reference(source, 3, column)
                    .unwrap()
                    .is_none()
            );
        }
        assert!(super::algorithm_reference(source, 2, 11).unwrap().is_none());
    }

    use super::*;

    #[test]
    fn validates_nested_control_flow_and_unicode_operations() {
        validate_algorithm("résultat ← none\nfor each candidate in candidates\n  if candidate.score ≥ 10\n    résultat ← candidate\n    break\n  otherwise if candidate.ready\n    continue\n  otherwise\n    inspect candidate\nreturn résultat").unwrap();
        validate_algorithm("if ready:\n  return value\notherwise:\n  return none").unwrap();
    }

    #[test]
    fn rejects_incomplete_statements_and_invalid_structure() {
        for source in [
            "if",
            "while",
            "value ←",
            "value ← (missing",
            "otherwise\n  return none",
            "if ready\nreturn none",
            "inspect value\n  return none",
            "break",
            "# only a comment",
            "if ready\n  inspect value\n return none",
            "if ready\n\treturn none",
        ] {
            assert!(validate_algorithm(source).is_err(), "accepted {source:?}");
        }
    }

    #[test]
    fn accepts_complete_content_beyond_former_display_thresholds() {
        let source = "inspect candidate\n".repeat(5000);
        validate_algorithm(&source).unwrap();
    }

    #[test]
    fn retains_explicit_constructors_nested_calls_and_named_arguments() {
        let source = "projectile ← combat::Projectile(damage = 1, previous_position = player.position)\nvelocity ← Velocity(linear = 560 × direction)\nmarker ← CombatEntity()\nresult ← Store::new(config = inspect(source))\nqueue spawn(projectile, velocity, marker)";
        let invocation = validate_algorithm(source).unwrap();
        assert_eq!(
            invocation
                .iter()
                .map(|site| (site.name.as_str(), site.has_positional_argument))
                .collect::<Vec<_>>(),
            vec![
                ("combat::Projectile", false),
                ("Velocity", false),
                ("CombatEntity", false),
                ("Store::new", false),
                ("inspect", true),
                ("spawn", true),
            ]
        );
        assert_eq!((invocation[0].line, invocation[0].column), (1, 16));
    }

    #[test]
    fn rejects_missing_or_duplicate_constructor_arguments() {
        for source in [
            "value ← Item(field =)",
            "value ← Item(field = 1, field = 2)",
            "value ← Item(field = 1,)",
        ] {
            assert!(validate_algorithm(source).is_err(), "accepted {source:?}");
        }
    }
}
