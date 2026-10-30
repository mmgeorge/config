use std::ops::Range;

use super::{SyntaxHandle, SyntaxLanguage};
use tree_sitter::Node;

/// Source-line regions used to select excerpts from an exact parsed source.
#[derive(Clone, Debug, Default)]
pub struct SourceOutline {
    /// Nested declaration scopes in source order. Ranges are zero-based and end-exclusive.
    pub scope: Vec<SourceScope>,
    /// Recognized import statements, including multiline imports.
    pub import: Vec<Range<usize>>,
}

/// A declaration's complete extent and its signature and closing-delimiter rows.
#[derive(Clone, Debug)]
pub struct SourceScope {
    /// Syntactic kind and qualified lexical name used to pair old and new declarations.
    pub identity: String,
    /// Complete declaration, including its body.
    pub extent: Range<usize>,
    /// Attached attributes/documentation and signature through the opening delimiter.
    pub signature: Range<usize>,
    /// UTF-8 byte column after the signature on its final row.
    pub signature_end_column: usize,
    /// Whether the declaration represents a named or bound callable.
    pub is_callable: bool,
    /// Closing delimiter when it occupies a separate source row.
    pub closing: Option<usize>,
}

impl SyntaxHandle {
    /// Locate declaration and import rows without language-specific rendering rules.
    /// Malformed declarations retain ordinary diff rows without disabling valid siblings.
    pub fn outline(&self) -> SourceOutline {
        let mut outline = SourceOutline::default();
        let Some(tree) = self.0.tree.first() else {
            return outline;
        };
        let source = self.source().text();
        let source_lines: Vec<_> = source.lines().collect();
        let mut pending = vec![tree.root_node()];
        while let Some(node) = pending.pop() {
            let kind = node.kind();
            let import = matches!(
                kind,
                "import_statement"
                    | "import_from_statement"
                    | "future_import_statement"
                    | "import_declaration"
                    | "import_header"
                    | "import_alias"
                    | "import_directive"
                    | "use_declaration"
                    | "using_directive"
                    | "preproc_include"
            );
            let import_node = if import {
                Some(node)
            } else {
                require_import(node, self.language(), source)
            };
            if let Some(import_node) = import_node.filter(|import| !import.has_error()) {
                if complete_lines(import_node, source) {
                    outline.import.push(extent(import_node));
                }
                continue;
            }
            if let Some(body) = node.child_by_field_name("body")
                && let Some((declaration_node, name)) = declaration(node, source)
            {
                let prefix = declaration_prefix(declaration_node, source);
                let extent = prefix.start_position().row..extent(declaration_node).end;
                let mut signature_byte_end = body.start_byte();
                let mut cursor = node.walk();
                let children: Vec<_> = node.children(&mut cursor).collect();
                for child in children.into_iter().rev() {
                    if child.start_byte() >= body.start_byte() {
                        continue;
                    }
                    if child.kind().contains("comment") {
                        signature_byte_end = child.start_byte();
                    } else {
                        break;
                    }
                }
                if source.as_bytes().get(body.start_byte()) == Some(&b'{') {
                    signature_byte_end = body.start_byte() + 1;
                }
                let header = source[prefix.start_byte()..signature_byte_end].trim_end();
                let signature_end =
                    extent.start + header.bytes().filter(|byte| *byte == b'\n').count() + 1;
                let last = extent.end - 1;
                let closing = source_lines.get(last).and_then(|line| {
                    (line.trim() == "end"
                        || (source.as_bytes().get(body.start_byte()) == Some(&b'{')
                            && matches!(line.trim(), "}" | "};" | ");" | "];")))
                    .then_some(last)
                });
                outline.scope.push(SourceScope {
                    identity: lexical_identity(declaration_node, kind, name, source),
                    signature: extent.start..signature_end.max(extent.start + 1),
                    signature_end_column: source[..prefix.start_byte() + header.len()]
                        .rsplit('\n')
                        .next()
                        .unwrap_or_default()
                        .len(),
                    is_callable: node.child_by_field_name("parameters").is_some()
                        || node.child_by_field_name("parameter").is_some()
                        || matches!(kind, "lambda" | "lambda_expression" | "function_definition"),
                    extent,
                    closing,
                });
            }
            let mut cursor = node.walk();
            let children: Vec<_> = node.named_children(&mut cursor).collect();
            pending.extend(children.into_iter().rev());
        }
        outline
    }
}

fn extent(node: Node<'_>) -> Range<usize> {
    node.start_position().row..node.end_position().row + usize::from(node.end_position().column > 0)
}

fn declaration_prefix<'tree>(node: Node<'tree>, source: &str) -> Node<'tree> {
    let mut prefix = node;
    let mut previous = node.prev_named_sibling();
    while let Some(sibling) = previous {
        let comment = sibling.kind().contains("comment");
        let text = &source[sibling.byte_range()];
        let documentation = comment
            && ((text.starts_with("///") && !text.starts_with("////"))
                || (text.starts_with("/**") && !text.starts_with("/***")));
        if matches!(sibling.kind(), "attribute_item" | "decorator") || documentation {
            prefix = sibling;
        } else if !comment {
            break;
        }
        previous = sibling.prev_named_sibling();
    }
    prefix
}

pub(super) fn declaration_owner(mut node: Node<'_>) -> Node<'_> {
    while let Some(parent) = node.parent() {
        let wrapper = parent.child_by_field_name("definition") == Some(node)
            || parent.child_by_field_name("declaration") == Some(node)
            || (parent.kind() == "template_declaration"
                && parent.named_children(&mut parent.walk()).last() == Some(node))
            || (parent.kind() == "declaration"
                && node.kind() == "init_declarator"
                && parent
                    .children_by_field_name("declarator", &mut parent.walk())
                    .count()
                    == 1)
            || (matches!(
                parent.kind(),
                "lexical_declaration" | "variable_declaration"
            ) && parent.named_child_count() == 1);
        if !wrapper {
            break;
        }
        node = parent;
    }
    node
}

fn binding(node: Node<'_>) -> Option<(Node<'_>, Node<'_>)> {
    let mut value = node;
    while let Some(parent) = value.parent() {
        if !matches!(parent.kind(), "parenthesized_expression" | "parenthesized") {
            break;
        }
        value = parent;
    }
    let parent = value.parent()?;
    if parent.kind() == "assignment" && parent.child_by_field_name("right") == Some(value) {
        return Some((
            declaration_owner(parent),
            parent.child_by_field_name("left")?,
        ));
    }
    if parent.kind() == "init_declarator" && parent.child_by_field_name("value") == Some(value) {
        return Some((declaration_owner(parent), declarator_name(parent)?));
    }
    if matches!(
        parent.kind(),
        "variable_declarator"
            | "let_declaration"
            | "pair"
            | "public_field_definition"
            | "field_definition"
    ) && parent.child_by_field_name("value") == Some(value)
        && let Some(name) = parent
            .child_by_field_name("name")
            .or_else(|| parent.child_by_field_name("pattern"))
            .or_else(|| parent.child_by_field_name("key"))
    {
        return Some((declaration_owner(parent), name));
    }
    if parent.kind() != "expression_list" || parent.named_child_count() != 1 {
        return None;
    }
    let assignment = parent.parent()?;
    if assignment.kind() != "assignment_statement" {
        return None;
    }
    let variables = assignment.named_child(0)?;
    if variables.kind() != "variable_list" || variables.named_child_count() != 1 {
        return None;
    }
    Some((
        declaration_owner(assignment),
        variables.child_by_field_name("name")?,
    ))
}

fn declaration<'tree, 'source>(
    node: Node<'tree>,
    source: &'source str,
) -> Option<(Node<'tree>, &'source str)> {
    if node.has_error() {
        return None;
    }
    let kind = node.kind();
    if kind == "function_definition"
        && let Some(name) = declarator_name(node)
    {
        let owner = declaration_owner(node);
        return (!owner.has_error()).then_some((owner, source.get(name.byte_range())?));
    }
    let expression = matches!(
        kind,
        "closure_expression" | "arrow_function" | "function_expression" | "lambda"
    ) || kind.contains("lambda")
        || (kind == "function_definition" && node.child_by_field_name("name").is_none());
    let (owner, name) = if expression {
        binding(node)?
    } else {
        (declaration_owner(node), node.child_by_field_name("name")?)
    };
    (!owner.has_error()).then_some((owner, source.get(name.byte_range())?))
}

fn declarator_name(mut node: Node<'_>) -> Option<Node<'_>> {
    loop {
        if matches!(
            node.kind(),
            "identifier"
                | "field_identifier"
                | "qualified_identifier"
                | "operator_name"
                | "destructor_name"
                | "operator_cast"
        ) {
            return Some(node);
        }
        node = node.child_by_field_name("declarator").or_else(|| {
            matches!(
                node.kind(),
                "parenthesized_declarator" | "reference_declarator"
            )
            .then(|| node.named_child(0))
            .flatten()
        })?;
    }
}

fn lexical_identity(node: Node<'_>, kind: &str, name: &str, source: &str) -> String {
    let mut path = vec![format!("{kind}:{name}")];
    let mut ancestor = node.parent();
    while let Some(parent) = ancestor {
        if parent.child_by_field_name("body").is_some() {
            if let Some(name) = parent.child_by_field_name("name").or_else(|| {
                (parent.kind() == "function_definition")
                    .then(|| declarator_name(parent))
                    .flatten()
            }) {
                path.push(format!("{}:{}", parent.kind(), &source[name.byte_range()]));
            } else if parent.kind() == "impl_item"
                && let Some(owner) = parent.child_by_field_name("type")
            {
                let contract = parent
                    .child_by_field_name("trait")
                    .map(|contract| &source[contract.byte_range()])
                    .unwrap_or("");
                path.push(format!("impl:{contract}:{}", &source[owner.byte_range()]));
            }
        }
        ancestor = parent.parent();
    }
    path.reverse();
    path.join("/")
}

fn complete_lines(node: Node<'_>, source: &str) -> bool {
    let start = node.start_byte();
    let end = node.end_byte();
    let line_start = source[..start]
        .rfind('\n')
        .map_or(0, |position| position + 1);
    let line_end = source[end..]
        .find('\n')
        .map_or(source.len(), |position| end + position);
    source[line_start..start].trim().is_empty()
        && (source[..end].ends_with('\n') || source[end..line_end].trim().is_empty())
}

fn require_import<'tree>(
    node: Node<'tree>,
    language: SyntaxLanguage,
    source: &str,
) -> Option<Node<'tree>> {
    if !matches!(
        language,
        SyntaxLanguage::Lua
            | SyntaxLanguage::Javascript
            | SyntaxLanguage::Typescript
            | SyntaxLanguage::Tsx
    ) || !matches!(node.kind(), "function_call" | "call_expression")
        || node
            .named_child(0)
            .is_none_or(|name| source.get(name.byte_range()) != Some("require"))
    {
        return None;
    }
    let arguments = node.child_by_field_name("arguments")?;
    if arguments.named_child_count() != 1 || arguments.named_child(0)?.kind() != "string" {
        return None;
    }
    let (owner, _) = binding(node)?;
    let mut ancestor = owner.parent();
    while let Some(parent) = ancestor {
        if parent.child_by_field_name("body").is_some() {
            return None;
        }
        ancestor = parent.parent();
    }
    Some(owner)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Representation, SourceVersion};
    use crate::syntax::{SyntaxEngine, SyntaxRequest};
    use crate::workers::{AnalysisPool, PoolLimits, WorkPriority};
    use std::sync::Arc;

    async fn analyze(language: SyntaxLanguage, source: &str) -> SyntaxHandle {
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        })));
        engine
            .analyze(SyntaxRequest {
                source: SourceVersion::new(source.as_bytes().to_vec(), Representation::Raw)
                    .unwrap(),
                language,
                priority: WorkPriority::Foreground,
                deadline: None,
            })
            .await
            .unwrap()
    }

    #[tokio::test]
    async fn rust_bound_closure_retains_outer_and_binding_signatures() {
        let source = "fn outer() {\n    let calculate = |\n        value: i32\n    | {\n        value\n    };\n}\n";
        let handle = analyze(SyntaxLanguage::Rust, source).await;
        assert!(
            !handle.0.tree[0].root_node().has_error(),
            "{}",
            handle.0.tree[0].root_node().to_sexp()
        );
        let outline = handle.outline();
        assert_eq!(
            outline.scope.len(),
            2,
            "{}",
            handle.0.tree[0].root_node().to_sexp()
        );
        assert_eq!(outline.scope[0].signature, 0..1);
        assert_eq!(outline.scope[1].signature, 1..4);
    }

    #[tokio::test]
    async fn lua_binding_retains_assignment_signature_and_static_import() {
        let source = "local dependency = require('dependency')\nlocal calculate = function(value)\n    return value\nend\n";
        let handle = analyze(SyntaxLanguage::Lua, source).await;
        let outline = handle.outline();
        assert_eq!(
            outline.import,
            std::iter::once(0..1).collect::<Vec<_>>(),
            "{}",
            handle.0.tree[0].root_node().to_sexp()
        );
        assert_eq!(outline.scope.len(), 1);
        assert_eq!(outline.scope[0].signature, 1..2);
        assert_eq!(outline.scope[0].extent, 1..4);
    }

    #[tokio::test]
    async fn c_and_cpp_declarators_preserve_callable_ownership() {
        for (language, source, expected) in [
            (
                SyntaxLanguage::C,
                "#include <stddef.h>\nconst char *\nlookup(int value) {\n    return 0;\n}\n",
                vec![("lookup", 1..3)],
            ),
            (
                SyntaxLanguage::Cpp,
                "template<typename Value>\nValue &lookup(\n    Value &value\n) {\n    return value;\n}\n",
                vec![("lookup", 0..4)],
            ),
            (
                SyntaxLanguage::Cpp,
                "int Runner::calculate(int value) const {\n    return value;\n}\n",
                vec![("Runner::calculate", 0..1)],
            ),
            (
                SyntaxLanguage::Cpp,
                "void run() {\n    auto transform = [](int value) {\n        return value + 1;\n    };\n    consume([](int value) {\n        return value;\n    });\n}\n",
                vec![("run", 0..1), ("transform", 1..2)],
            ),
            (
                SyntaxLanguage::Cpp,
                "struct Runner {\n    Runner() {\n        start();\n    }\n    ~Runner() {\n        stop();\n    }\n    int operator()(int value) const {\n        return value;\n    }\n};\n",
                vec![
                    ("Runner", 0..1),
                    ("Runner", 1..2),
                    ("~Runner", 4..5),
                    ("operator()", 7..8),
                ],
            ),
        ] {
            let handle = analyze(language, source).await;
            let tree = handle.0.tree[0].root_node();
            assert!(!tree.has_error(), "{}", tree.to_sexp());
            let outline = handle.outline();
            assert_eq!(outline.scope.len(), expected.len(), "{source}: {outline:?}");
            for (scope, (name, signature)) in outline.scope.iter().zip(expected) {
                assert!(scope.identity.ends_with(&format!(":{name}")), "{scope:?}");
                assert_eq!(scope.signature, signature, "{scope:?}");
                assert_eq!(
                    scope.is_callable,
                    !scope.identity.ends_with("class_specifier:Runner")
                        && !scope.identity.ends_with("struct_specifier:Runner"),
                    "{scope:?}"
                );
            }
            if language == SyntaxLanguage::C {
                assert_eq!(outline.import, std::iter::once(0..1).collect::<Vec<_>>());
            }
        }
    }

    #[tokio::test]
    async fn scm_queries_parse_predicates_without_callable_scopes() {
        let source = "((function_definition\n  name: (identifier) @function)\n (#eq? @function \"calculate\"))\n";
        let handle = analyze(SyntaxLanguage::Query, source).await;
        assert!(!handle.0.tree[0].root_node().has_error());
        assert!(!handle.captures().is_empty());
        assert!(handle.outline().scope.is_empty());
    }

    #[tokio::test]
    async fn recovered_c_headers_keep_valid_functions_and_includes() {
        let allocation = include_str!("../../tests/fixtures/c_headers/alloc.h");
        let array = include_str!("../../tests/fixtures/c_headers/array.h");
        for language in [SyntaxLanguage::C, SyntaxLanguage::Cpp] {
            for (source, functions) in [(allocation, 0), (array, 7)] {
                let handle = analyze(language, source).await;
                if language == SyntaxLanguage::C {
                    assert!(handle.0.tree[0].root_node().has_error());
                }
                let outline = handle.outline();
                assert!(!outline.import.is_empty(), "{language:?}: {outline:?}");
                assert_eq!(outline.scope.len(), functions, "{language:?}: {outline:?}");
                assert!(outline.scope.iter().all(|scope| scope.is_callable));
            }
        }
    }

    #[tokio::test]
    async fn malformed_declarations_do_not_disable_valid_siblings() {
        for (language, source, valid_name) in [
            (
                SyntaxLanguage::C,
                "#include <stddef.h>\nint valid(int value) {\n    return value;\n}\nint broken( {\n    damaged();\n}\n",
                "valid",
            ),
            (
                SyntaxLanguage::Rust,
                "fn valid() {\n    work();\n}\nfn broken( {\n    damaged();\n}\n",
                "valid",
            ),
            (
                SyntaxLanguage::Typescript,
                "function valid() {\n    work();\n}\nfunction broken( {\n    damaged();\n}\n",
                "valid",
            ),
            (
                SyntaxLanguage::Python,
                "def valid():\n    work()\ndef broken(:\n    damaged()\n",
                "valid",
            ),
        ] {
            let handle = analyze(language, source).await;
            assert!(handle.0.tree[0].root_node().has_error());
            let outline = handle.outline();
            assert_eq!(outline.scope.len(), 1, "{language:?}: {outline:?}");
            assert!(
                outline.scope[0]
                    .identity
                    .ends_with(&format!(":{valid_name}"))
            );
        }
    }
}
