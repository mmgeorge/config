use tree_sitter::Node;

use super::{SyntaxHandle, SyntaxLanguage};

/// One declaration in an exact source snapshot. Coordinates count Unicode code points from one.
#[derive(Clone, Debug)]
pub struct SourceDeclaration {
    pub name: String,
    pub kind: String,
    pub signature: String,
    pub visibility: String,
    pub line: usize,
    pub column: usize,
    pub start_line: usize,
    pub end_line: usize,
    pub range_start_line: usize,
    pub range_start_column: usize,
    pub range_end_line: usize,
    pub range_end_column: usize,
    pub parent: Option<usize>,
    pub depth: usize,
}

impl SyntaxHandle {
    /// Extract declaration ranges without changing source or plan projection semantics.
    pub fn declarations(&self) -> Vec<SourceDeclaration> {
        let mut declaration = Vec::new();
        if let Some(tree) = self.0.tree.first() {
            visit(
                tree.root_node(),
                self.source().text(),
                self.language(),
                None,
                &mut declaration,
            );
        }
        declaration
    }
}

fn visit(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    parent: Option<usize>,
    result: &mut Vec<SourceDeclaration>,
) {
    let kind = node.kind();
    let selected = match language {
        SyntaxLanguage::Rust => matches!(
            kind,
            "function_item"
                | "function_signature_item"
                | "struct_item"
                | "enum_item"
                | "enum_variant"
                | "trait_item"
                | "impl_item"
                | "mod_item"
                | "type_item"
                | "const_item"
                | "static_item"
                | "field_declaration"
                | "macro_definition"
        ),
        SyntaxLanguage::Typescript | SyntaxLanguage::Tsx | SyntaxLanguage::Javascript => matches!(
            kind,
            "function_declaration"
                | "generator_function_declaration"
                | "class_declaration"
                | "abstract_class_declaration"
                | "interface_declaration"
                | "type_alias_declaration"
                | "enum_declaration"
                | "method_definition"
                | "method_signature"
                | "abstract_method_signature"
                | "public_field_definition"
                | "property_signature"
                | "variable_declarator"
                | "internal_module"
        ),
        SyntaxLanguage::Lua => matches!(
            kind,
            "function_declaration" | "variable_declaration" | "assignment_statement" | "field"
        ),
        _ => false,
    };
    let mut owner = parent;
    if selected
        && !(language == SyntaxLanguage::Lua
            && kind == "assignment_statement"
            && node
                .parent()
                .is_some_and(|parent| parent.kind() == "variable_declaration"))
    {
        let name_node = node.child_by_field_name("name").or_else(|| {
            if kind == "impl_item" {
                node.child_by_field_name("type")
            } else if language == SyntaxLanguage::Lua && kind != "field" {
                first_name(node)
            } else {
                None
            }
        });
        if let Some(name_node) = name_node {
            let name = text(name_node, source).to_owned();
            let body = node.child_by_field_name("body").or_else(|| {
                let mut cursor = node.walk();
                node.named_children(&mut cursor).find(|child| {
                    matches!(
                        child.kind(),
                        "field_declaration_list"
                            | "enum_variant_list"
                            | "declaration_list"
                            | "class_body"
                            | "interface_body"
                            | "statement_block"
                            | "block"
                    )
                })
            });
            let mut end = body.map_or(node.end_byte(), |body| body.start_byte());
            if let Some(value) = node.child_by_field_name("value") {
                if matches!(value.kind(), "arrow_function" | "function_expression") {
                    end = value
                        .child_by_field_name("body")
                        .map_or(value.end_byte(), |body| body.start_byte());
                } else if kind == "variable_declarator" || kind == "field" {
                    end = value.start_byte();
                }
            }
            if language == SyntaxLanguage::Lua
                && let Some(value) = lua_value(node)
            {
                end = if value.kind() == "function_definition" {
                    value
                        .child_by_field_name("body")
                        .map_or(value.end_byte().saturating_sub(3), |body| body.start_byte())
                } else {
                    value.start_byte()
                };
            }
            let mut signature = source[node.start_byte()..end]
                .trim()
                .trim_end_matches([';', ',', '=', ' '])
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ");
            if kind == "variable_declarator"
                && let Some(container) = node.parent().filter(|container| {
                    container.kind() == "lexical_declaration"
                        || container.kind() == "variable_declaration"
                })
            {
                signature = format!(
                    "{}{}",
                    &source[container.start_byte()..node.start_byte()],
                    signature
                );
            }
            if language == SyntaxLanguage::Lua && kind == "function_declaration" {
                signature = signature
                    .strip_suffix("end")
                    .unwrap_or(&signature)
                    .trim()
                    .to_owned();
            }
            let visibility = visibility(node, source, language, parent.map(|index| &result[index]));
            let line = name_node.start_position().row + 1;
            let line_start = source[..name_node.start_byte()]
                .rfind('\n')
                .map_or(0, |index| index + 1);
            let column = source[line_start..name_node.start_byte()].chars().count() + 1;
            let mut start_line = node.start_position().row + 1;
            for previous in source[..node.start_byte()]
                .lines()
                .rev()
                .skip(usize::from(node.start_position().column > 0))
            {
                let previous = previous.trim();
                if previous.starts_with("///")
                    || previous.starts_with("//!")
                    || previous.starts_with("---@")
                    || previous.starts_with("---")
                    || previous.starts_with("/**")
                    || previous.starts_with('*')
                    || previous.starts_with("#[")
                {
                    start_line = start_line.saturating_sub(1).max(1);
                } else {
                    break;
                }
            }
            owner = Some(result.len());
            result.push(SourceDeclaration {
                name,
                kind: kind.into(),
                signature,
                visibility,
                line,
                column,
                start_line,
                end_line: node.end_position().row + usize::from(node.end_position().column > 0),
                range_start_line: node.start_position().row + 1,
                range_start_column: unicode_column(source, node.start_byte()),
                range_end_line: node.end_position().row + 1,
                range_end_column: unicode_column(source, node.end_byte()),
                parent,
                depth: parent.map_or(1, |index| result[index].depth + 1),
            });
        }
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        visit(child, source, language, owner, result);
    }
}

fn unicode_column(source: &str, byte: usize) -> usize {
    let start = source[..byte].rfind('\n').map_or(0, |index| index + 1);
    source[start..byte].chars().count() + 1
}

fn first_name(node: Node<'_>) -> Option<Node<'_>> {
    if matches!(
        node.kind(),
        "identifier" | "dot_index_expression" | "method_index_expression"
    ) {
        return Some(node);
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if let Some(name) = first_name(child) {
            return Some(name);
        }
    }
    None
}

fn lua_value(node: Node<'_>) -> Option<Node<'_>> {
    if node.kind() == "field" {
        return node.child_by_field_name("value");
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "expression_list" {
            return child.named_child(0);
        }
        if child.kind() == "assignment_statement" {
            return lua_value(child);
        }
    }
    None
}

fn text<'source>(node: Node<'_>, source: &'source str) -> &'source str {
    &source[node.byte_range()]
}

fn visibility(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    parent: Option<&SourceDeclaration>,
) -> String {
    let mut cursor = node.walk();
    let modifier = node
        .named_children(&mut cursor)
        .find(|child| child.kind() == "visibility_modifier")
        .map(|child| text(child, source));
    let header = text(node, source);
    let value = match language {
        SyntaxLanguage::Rust => match modifier {
            Some("pub") => "public",
            Some("pub(self)") => "private",
            Some(_) => "internal",
            None if node.kind() == "impl_item" => "structural",
            None if parent.is_some_and(|parent| {
                matches!(parent.kind.as_str(), "trait_item" | "enum_item")
            }) =>
            {
                return parent.unwrap().visibility.clone();
            }
            None if parent.is_some_and(|parent| {
                parent.kind == "impl_item" && parent.signature.contains(" for ")
            }) =>
            {
                "unknown"
            }
            None => "private",
        },
        SyntaxLanguage::Typescript | SyntaxLanguage::Tsx | SyntaxLanguage::Javascript => {
            if header.starts_with("private ") || header.starts_with('#') {
                "private"
            } else if header.starts_with("protected ") {
                "internal"
            } else if parent.is_some_and(|parent| {
                matches!(
                    parent.kind.as_str(),
                    "class_declaration"
                        | "abstract_class_declaration"
                        | "interface_declaration"
                        | "enum_declaration"
                )
            }) {
                "public"
            } else {
                let mut ancestor = node.parent();
                let mut exported = false;
                while let Some(owner) = ancestor {
                    if owner.kind() == "export_statement" {
                        exported = true;
                        break;
                    }
                    if matches!(owner.kind(), "statement_block" | "program") {
                        break;
                    }
                    ancestor = owner.parent();
                }
                if exported { "public" } else { "private" }
            }
        }
        SyntaxLanguage::Lua => {
            let name = first_name(node)
                .map(|name| text(name, source))
                .unwrap_or("");
            let table = name.split(['.', ':']).next().unwrap_or(name);
            let mut root = node;
            while let Some(parent) = root.parent() {
                root = parent;
            }
            let mut cursor = root.walk();
            let returned = root
                .named_children(&mut cursor)
                .find(|child| child.kind() == "return_statement")
                .map(|returned| {
                    text(returned, source)
                        .trim_start_matches("return")
                        .split_whitespace()
                        .collect::<String>()
                })
                .unwrap_or_default();
            let exported = returned == table
                || returned.contains(&format!("={table},"))
                || returned.contains(&format!("={table}}}"));
            if exported
                || node.kind() == "field"
                    && parent.is_some_and(|parent| parent.visibility == "public")
            {
                "public"
            } else if header.starts_with("local ") {
                "private"
            } else if node.kind() == "function_declaration"
                && !name.contains('.')
                && !name.contains(':')
            {
                "public"
            } else {
                "unknown"
            }
        }
        _ => "unknown",
    };
    value.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Representation, SourceVersion};
    use crate::syntax::{SyntaxEngine, SyntaxRequest};
    use crate::workers::{AnalysisPool, PoolLimits, WorkPriority};
    use std::sync::Arc;

    #[tokio::test]
    async fn source_structure_preserves_ranges_visibility_and_unicode() {
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        })));
        for (language, source, expected) in [
            (
                SyntaxLanguage::Rust,
                "pub struct Store { pub count: usize, hidden: bool }\nimpl Store { pub fn save(&self) {} }\npub(crate) fn internal() {}\n",
                vec![
                    ("Store", "public"),
                    ("count", "public"),
                    ("hidden", "private"),
                    ("save", "public"),
                    ("internal", "internal"),
                ],
            ),
            (
                SyntaxLanguage::Typescript,
                "export class Store { public count: number; private secret: string; save(): void {} }\nconst hidden = 1;\n",
                vec![
                    ("Store", "public"),
                    ("count", "public"),
                    ("secret", "private"),
                    ("save", "public"),
                    ("hidden", "private"),
                ],
            ),
            (
                SyntaxLanguage::Javascript,
                "export function save(value) { return value; }\nconst hidden = 1;\n",
                vec![("save", "public"), ("hidden", "private")],
            ),
            (
                SyntaxLanguage::Lua,
                "local function save(value)\n  return value\nend\nreturn { save = save }\n",
                vec![("save", "public")],
            ),
            (
                SyntaxLanguage::Lua,
                "local M = { read = function(value) return value end }\nreturn M\n",
                vec![("M", "public"), ("read", "public")],
            ),
        ] {
            let handle = engine
                .analyze(SyntaxRequest {
                    language,
                    source: SourceVersion::new(source.as_bytes().to_vec(), Representation::Raw)
                        .unwrap(),
                    priority: WorkPriority::Foreground,
                    deadline: None,
                })
                .await
                .unwrap();
            let declarations = handle.declarations();
            for (name, visibility) in expected {
                let declaration = declarations
                    .iter()
                    .find(|declaration| declaration.name == name)
                    .unwrap_or_else(|| panic!("missing {name}: {declarations:?}"));
                assert_eq!(declaration.visibility, visibility, "{declarations:?}");
                assert!(
                    declaration.start_line <= declaration.line
                        && declaration.end_line >= declaration.line
                );
                assert!(!declaration.signature.contains("return value"));
            }
        }
    }
}
