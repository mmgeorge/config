use super::{DeclarationOverview, SyntaxLanguage, declaration_surrogate};
use tree_sitter::{Node, Parser};

const WIDTHS: [usize; 6] = [40, 60, 80, 100, 120, 240];

#[derive(Debug, PartialEq)]
struct Meaning {
    syntax: Vec<String>,
    comments: Vec<String>,
}

fn meaning(language: SyntaxLanguage, text: &str) -> Meaning {
    let source = declaration_surrogate(language, text);
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

fn check(path: &str, source: &str) {
    let failures = inspect(path, source);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

fn check_corpus(path: &str, source: &str) {
    let language = DeclarationOverview::language(path).unwrap();
    let surrogate = declaration_surrogate(language, source);
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
        include_str!("../../tests/fixtures/declaration_formatting/rust.rs.txt"),
    );
}

#[test]
fn typescript_declaration_corpus_preserves_meaning() {
    let source = include_str!("../../tests/fixtures/declaration_formatting/typescript.ts.txt");
    for path in ["corpus.ts", "corpus.mts", "corpus.cts"] {
        check_corpus(path, source);
    }
}

#[test]
fn tsx_declaration_corpus_preserves_meaning() {
    check_corpus(
        "corpus.tsx",
        include_str!("../../tests/fixtures/declaration_formatting/tsx.tsx.txt"),
    );
}

#[test]
fn lua_declaration_corpus_preserves_meaning() {
    check_corpus(
        "corpus.lua",
        include_str!("../../tests/fixtures/declaration_formatting/lua.lua.txt"),
    );
}

#[test]
fn large_files_preserve_meaning_across_repeated_passes() {
    let mut failures = Vec::new();
    for (path, source) in [
        (
            "large.rs",
            include_str!("../../tests/fixtures/declaration_formatting/rust.rs.txt"),
        ),
        (
            "large.ts",
            include_str!("../../tests/fixtures/declaration_formatting/typescript.ts.txt"),
        ),
        (
            "large.tsx",
            include_str!("../../tests/fixtures/declaration_formatting/tsx.tsx.txt"),
        ),
        (
            "large.lua",
            include_str!("../../tests/fixtures/declaration_formatting/lua.lua.txt"),
        ),
    ] {
        let source = source.strip_suffix("return M\n").unwrap_or(source);
        let mut large = source.repeat(24);
        if path.ends_with(".lua") {
            large.push_str("return M\n");
        }
        assert!(
            large.lines().count() >= 1000,
            "{path} must exercise a large document"
        );
        failures.extend(inspect(path, &large));
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn comments_inside_parameter_and_type_lists_preserve_meaning() {
    let mut failures = Vec::new();
    for (path, source) in [
        (
            "parameters.rs",
            "pub fn request(\n// A borrowed request with a sufficiently long explanatory comment.\nrequest_identifier: &str,\n/* The callback borrows the response. */\nresponse_callback: fn(&str, usize) -> bool,\n) -> bool;\n",
        ),
        (
            "fields.rs",
            "pub struct Request {\npub id: u64, // Caller-owned identifier.\n/// Next field documentation.\npub label: &'static str,\n}\n",
        ),
        (
            "types.ts",
            "export type Pair = [\n// The first item.\nfirst: string,\n/* The second item. */\nsecond: number\n];\n",
        ),
        (
            "parameters.ts",
            "export function request(\n// Caller-owned request.\nrequest_identifier: string,\nresponse_callback: (value: string, count: number) => boolean\n): boolean;\n",
        ),
        (
            "fields.ts",
            "export interface Request {\nreadonly id: string; // Caller-owned identifier.\n/** Next field documentation. */\nlabel: 'two  spaces';\n}\n",
        ),
        (
            "parameters.tsx",
            "export const Component = (\n// Caller-owned properties.\nproperties: ViewProps,\nforwarded_reference: Ref<HTMLElement>\n): ReactNode =>;\n",
        ),
        (
            "parameters.lua",
            "local M\nfunction M.request(\n-- Caller-owned request.\nrequest_identifier,\nresponse_callback\n)\nreturn M\n",
        ),
    ] {
        failures.extend(inspect(path, source));
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn rust_tuple_field_comments_remain_separate_from_fields() {
    let mut failures = Vec::new();
    for (index, source) in [
        "pub struct Position(\n/// Center in arena world coordinates.\npub Vec2,\n);\n",
        "pub struct Pair(\n/// First value.\npub u32,\n/// Second value.\npub u64,\n);\n",
        "pub struct Pair(\npub u32, // First value.\npub u64, // Second value.\n);\n",
        "pub struct Attribute(\n#[cfg(feature = \"enabled\")]\n/// Optional field.\npub Vec<u8>,\n);\n",
        "pub enum Event { Payload(\n/// Payload bytes.\nVec<u8>,\n), Empty }\n",
        "pub struct Nested(\n/// Nested tuple.\npub (u32, u64),\n/// Function argument commas.\npub fn(u8, u16) -> u32,\n);\n",
    ].into_iter().enumerate() {
        failures.extend(inspect(&format!("tuple_{index}.rs"), source));
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn rust_union_declarations_are_admitted_before_formatting() {
    check(
        "union.rs",
        "pub union RawValue {\n/// Integral representation.\npub integer: u64,\n/// Floating representation.\npub decimal: f64,\n}\n",
    );
}

#[test]
fn rust_unsafe_extern_blocks_are_admitted_before_formatting() {
    check(
        "ffi.rs",
        "unsafe extern \"C\" { pub fn send(buffer: *const u8, length: usize); }\n",
    );
}

#[test]
fn tsx_function_expression_return_type_is_admitted_before_formatting() {
    check(
        "expression.tsx",
        "export const Component = function(properties: Props): ReactNode;\n",
    );
}

#[test]
fn lua_literal_require_without_parentheses_is_admitted_before_formatting() {
    check(
        "module.lua",
        "local module = require 'platform.module'\nreturn module\n",
    );
}

#[test]
fn meaning_oracle_detects_parseable_corruption() {
    let original = meaning(
        SyntaxLanguage::Rust,
        "pub struct Position(\n/// Coordinate.\npub Vec2,\n);\n",
    );
    let corrupted = meaning(
        SyntaxLanguage::Rust,
        "pub struct Position(\n/// Coordinate. pub Vec2,\n);\n",
    );
    assert_ne!(original.syntax, corrupted.syntax);
    assert_ne!(original.comments, corrupted.comments);
    let original = meaning(
        SyntaxLanguage::Typescript,
        "export type Label = 'two words';\n",
    );
    let corrupted = meaning(
        SyntaxLanguage::Typescript,
        "export type Label = 'twowords';\n",
    );
    assert_ne!(original.syntax, corrupted.syntax);
}

#[test]
fn configuration_corpus_is_preserved_exactly() {
    for (path, source) in [
        (
            "Cargo.toml",
            include_str!("../../tests/fixtures/declaration_formatting/config.toml"),
        ),
        (
            "config.json",
            include_str!("../../tests/fixtures/declaration_formatting/config.json"),
        ),
        (
            "tsconfig.json",
            include_str!("../../tests/fixtures/declaration_formatting/config.jsonc"),
        ),
        (
            "config.jsonc",
            include_str!("../../tests/fixtures/declaration_formatting/config.jsonc"),
        ),
        (
            "config.yaml",
            include_str!("../../tests/fixtures/declaration_formatting/config.yaml"),
        ),
        (
            "config.yml",
            include_str!("../../tests/fixtures/declaration_formatting/config.yaml"),
        ),
        (
            "config.xml",
            include_str!("../../tests/fixtures/declaration_formatting/config.xml"),
        ),
        (
            ".gitignore",
            include_str!("../../tests/fixtures/declaration_formatting/gitignore.txt"),
        ),
    ] {
        for newline in ["\n", "\r\n"] {
            let input = source.replace('\n', newline);
            for width in WIDTHS {
                let formatted =
                    DeclarationOverview::format_with_width(path, &input, width).unwrap();
                assert_eq!(formatted, input, "{path}, width {width}");
                assert_eq!(
                    DeclarationOverview::present(path, &formatted).unwrap().text,
                    input,
                    "{path}"
                );
            }
        }
    }
}
