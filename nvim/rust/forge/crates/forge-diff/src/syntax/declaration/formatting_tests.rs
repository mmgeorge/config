use super::{DeclarationOverview, SyntaxLanguage, declaration_surrogate};
use tree_sitter::{Node, Parser};

const WIDTHS: [usize; 6] = [40, 60, 80, 100, 120, 240];

#[derive(Debug, PartialEq)]
struct Meaning {
    syntax: Vec<String>,
    comments: Vec<String>,
}

fn meaning(language: SyntaxLanguage, text: &str) -> Meaning {
    let source = declaration_surrogate(language, text).unwrap();
    let mut parser = Parser::new();
    parser.set_language(&language.grammar()).unwrap();
    let tree = parser.parse(&source, None).unwrap();
    if tree.root_node().has_error() {
        let mut pending = vec![tree.root_node()];
        let mut errors = Vec::new();
        while let Some(node) = pending.pop() {
            if node.is_error() || node.is_missing() {
                errors.push(format!(
                    "{} at {:?}: {:?}",
                    node.kind(),
                    node.start_position(),
                    &source[node.byte_range()]
                ));
            }
            let mut cursor = node.walk();
            pending.extend(node.children(&mut cursor));
        }
        panic!("invalid fixture or formatted syntax: {}", errors.join("\n"));
    }
    let mut result = Meaning {
        syntax: Vec::new(),
        comments: Vec::new(),
    };
    visit(tree.root_node(), &source, &mut result);
    result
}

fn visit(node: Node<'_>, source: &str, result: &mut Meaning) {
    if node.kind().contains("comment") {
        for line in source[node.byte_range()].lines() {
            let content = line
                .trim()
                .trim_start_matches('/')
                .trim_start_matches('!')
                .trim_start_matches('-')
                .trim_start_matches('*')
                .trim_end_matches("*/");
            result
                .comments
                .extend(content.split_whitespace().map(str::to_owned));
        }
        return;
    }
    // Optional separators can be inserted by formatting. The tree still distinguishes
    // tuple fields, type arguments, statements, and every enclosing declaration.
    if matches!(node.kind(), "," | ";") {
        return;
    }
    result.syntax.push(format!("({}", node.kind()));
    if node.child_count() == 0 {
        result.syntax.push(source[node.byte_range()].to_owned());
    } else {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            visit(child, source, result);
        }
    }
    result.syntax.push(")".into());
}

fn difference(expected: &[String], actual: &[String]) -> String {
    let index = expected
        .iter()
        .zip(actual)
        .position(|(left, right)| left != right)
        .unwrap_or(expected.len().min(actual.len()));
    format!(
        "token {index}: expected {:?}, observed {:?} (length {} -> {})",
        expected.get(index),
        actual.get(index),
        expected.len(),
        actual.len()
    )
}

fn inspect(path: &str, source: &str) -> Vec<String> {
    let language = DeclarationOverview::language(path).unwrap();
    let expected = meaning(language, source);
    DeclarationOverview::parse(path, source)
        .unwrap_or_else(|error| panic!("{path} fixture: {error:?}"));
    let mut failures = Vec::new();
    for newline in ["\n", "\r\n"] {
        let input = source.replace('\n', newline);
        for width in WIDTHS {
            let label = format!(
                "{path}, width {width}, {}",
                if newline == "\n" { "LF" } else { "CRLF" }
            );
            let mut current = input.clone();
            for pass in 1..=3 {
                let formatted = match DeclarationOverview::format_with_width(path, &current, width)
                {
                    Ok(formatted) => formatted,
                    Err(error) => {
                        failures.push(format!("{label}, pass {pass}: {error:?}"));
                        break;
                    }
                };
                let actual = meaning(language, &formatted);
                if actual.syntax != expected.syntax {
                    failures.push(format!(
                        "{label}, pass {pass}: syntax changed: {}",
                        difference(&expected.syntax, &actual.syntax)
                    ));
                    break;
                }
                if actual.comments != expected.comments {
                    failures.push(format!(
                        "{label}, pass {pass}: comment changed: {}",
                        difference(&expected.comments, &actual.comments)
                    ));
                    break;
                }
                if pass > 1 && current != formatted {
                    failures.push(format!(
                        "{label}, pass {pass}: formatting is not idempotent"
                    ));
                    break;
                }
                current = formatted;
            }
        }
    }
    // Presentation is another public entry into the same formatter, without wrapping.
    match DeclarationOverview::present(path, source) {
        Ok(presentation) => {
            let actual = meaning(language, &presentation.text);
            if actual != expected {
                failures.push(format!("{path}: presentation changed declaration meaning"));
            }
        }
        Err(error) => failures.push(format!("{path}: presentation failed: {error:?}")),
    }
    failures
}

fn check_corpus(path: &str, source: &str) {
    let language = DeclarationOverview::language(path).unwrap();
    let surrogate = declaration_surrogate(language, source).unwrap();
    let mut parser = Parser::new();
    parser.set_language(&language.grammar()).unwrap();
    let tree = parser.parse(&surrogate, None).unwrap();
    let lines: Vec<_> = source.lines().collect();
    let mut failures = inspect(path, source);
    let mut cursor = tree.root_node().walk();
    let mut start = 0;
    // Test declarations independently so one corrupted tuple does not hide failures
    // in a later trait, module, class, or callback in the large-file test.
    for node in tree.root_node().named_children(&mut cursor) {
        if node.kind().contains("comment") || node.kind().contains("attribute") {
            continue;
        }
        let end = (node.end_position().row + 1).min(lines.len());
        let declaration = format!("{}\n", lines[start..end].join("\n"));
        let label = format!("{path}:{}:{}", start + 1, node.kind());
        let issues = inspect(path, &declaration);
        failures.extend(issues.into_iter().map(|issue| format!("{label}: {issue}")));
        start = end;
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn rust_declaration_corpus_preserves_meaning() {
    check_corpus(
        "corpus.rs",
        include_str!("../../../tests/fixtures/declaration_formatting/rust.rs.txt"),
    );
}
