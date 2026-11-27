use tree_sitter::Node;

use super::{DeclarationCalls, DeclarationOverview, SyntaxHandle, SyntaxLanguage};

/// Names a declaration and its inclusive, one-based range in a plan signature document.
#[derive(Debug)]
pub struct ReviewSymbol {
    /// Qualified name, including the callable ordinal when signatures repeat.
    pub name: String,
    /// First attached documentation, attribute, or declaration row.
    pub start: usize,
    /// First row of the declaration itself.
    pub signature: usize,
    /// Last row of the declaration.
    pub end: usize,
}

impl DeclarationOverview {
    /// Get type and callable anchors in source order using the plan's declaration grammar.
    pub fn review_symbols(path: &str, overview: &str) -> Option<Vec<ReviewSymbol>> {
        if super::ConfigurationFormat::for_path(path).is_some() {
            return Some(Vec::new());
        }
        let language = Self::language(path)?;
        let source = super::declaration::declaration_surrogate(language, overview).ok()?;
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language.grammar()).ok()?;
        let tree = parser.parse(&source, None)?;
        let root = tree.root_node();
        if root.has_error() {
            return None;
        }
        let lines: Vec<_> = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(offset, _)| offset + 1))
            .collect();
        let entries = declarations(root, &source, language, &lines, true);
        let separator = if language == SyntaxLanguage::Rust {
            "::"
        } else {
            "."
        };
        let mut symbols = Vec::new();
        for entry in entries.iter().filter(|entry| {
            is_review_type(&entry.kind)
                || matches!(
                    entry.kind.as_str(),
                    "abstract_method_signature" | "generator_function_declaration"
                )
        }) {
            let mut names = vec![entry.name.as_str()];
            let mut parent = entry.parent;
            while let Some(index) = parent {
                let entry = &entries[index];
                if entry.callable
                    || is_review_type(&entry.kind)
                    || matches!(
                        entry.kind.as_str(),
                        "mod_item" | "impl_item" | "internal_module"
                    )
                {
                    names.push(entry.name.as_str());
                }
                parent = entry.parent;
            }
            names.reverse();
            symbols.push(ReviewSymbol {
                name: names.join(separator),
                start: entry.start_line,
                signature: entry.range_start_line,
                end: entry.end_line,
            });
        }
        for callable in DeclarationCalls::from_tree(&source, language, &tree, true).ok()? {
            symbols.push(ReviewSymbol {
                name: callable.owner,
                start: callable.heading_line as usize,
                signature: callable.line as usize,
                end: callable.end_line as usize,
            });
        }
        symbols.sort_by_key(|symbol| symbol.start);
        Some(symbols)
    }
}

fn is_review_type(kind: &str) -> bool {
    matches!(
        kind,
        "struct_item"
            | "enum_item"
            | "trait_item"
            | "type_item"
            | "associated_type"
            | "class_declaration"
            | "abstract_class_declaration"
            | "class"
            | "interface_declaration"
            | "type_alias_declaration"
            | "enum_declaration"
    )
}

/// One declaration in an exact source snapshot. Coordinates count Unicode code points from one.
#[derive(Clone, Debug)]
pub struct SourceDeclaration {
    /// Exact UTF-8 byte extent, including attached documentation and attributes.
    pub source_bytes: std::ops::Range<usize>,
    /// Signature bytes before the executable body or initializer.
    pub signature_bytes: std::ops::Range<usize>,
    /// Body or initializer bytes, including delimiters belonging to this declaration.
    pub detail_bytes: std::ops::Range<usize>,
    /// Native syntax identifies a function or a binding whose value is a function.
    pub callable: bool,
    /// False when parser recovery leaves malformed or missing syntax in this declaration.
    pub syntax_valid: bool,
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
        if let Some(tree) = self.0.tree.first() {
            return declarations(
                tree.root_node(),
                self.source().text(),
                self.language(),
                &self.0.line,
                false,
            );
        }
        Vec::new()
    }
}

pub(super) fn declarations(
    root: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    lines: &[usize],
    plan_signatures: bool,
) -> Vec<SourceDeclaration> {
    let mut declaration = Vec::new();
    visit(
        root,
        source,
        language,
        None,
        &mut declaration,
        plan_signatures,
    );
    for entry in &mut declaration {
        if entry.start_line < entry.range_start_line {
            let start = lines[entry.start_line - 1];
            entry.source_bytes.start = start;
            entry.signature_bytes.start = start;
        }
    }
    declaration
}

fn visit(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    parent: Option<usize>,
    result: &mut Vec<SourceDeclaration>,
    plan_signatures: bool,
) {
    let kind = node.kind();
    let selected = match language {
        SyntaxLanguage::Rust => {
            matches!(
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
            ) || (plan_signatures && kind == "associated_type")
        }
        SyntaxLanguage::Typescript | SyntaxLanguage::Tsx | SyntaxLanguage::Javascript => {
            matches!(
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
            ) || (plan_signatures
                && (matches!(kind, "function_signature" | "enum_assignment")
                    || (kind == "property_identifier"
                        && node
                            .parent()
                            .is_some_and(|parent| parent.kind() == "enum_body"))))
        }
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
            if kind == "property_identifier" {
                Some(node)
            } else if kind == "impl_item" {
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
            let source_owner = super::outline::declaration_owner(node);
            let mut start_line = source_owner.start_position().row + 1;
            for previous in source[..source_owner.start_byte()]
                .lines()
                .rev()
                .skip(usize::from(source_owner.start_position().column > 0))
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
            let callable = matches!(
                kind,
                "function_item"
                    | "function_signature_item"
                    | "function_signature"
                    | "function_declaration"
                    | "generator_function_declaration"
                    | "method_definition"
                    | "method_signature"
                    | "abstract_method_signature"
            ) || node.child_by_field_name("value").is_some_and(|value| {
                matches!(value.kind(), "arrow_function" | "function_expression")
            }) || language == SyntaxLanguage::Lua
                && lua_value(node).is_some_and(|value| value.kind() == "function_definition");
            result.push(SourceDeclaration {
                source_bytes: source_owner.byte_range(),
                signature_bytes: source_owner.start_byte()..end,
                detail_bytes: end..source_owner.end_byte(),
                callable,
                syntax_valid: !source_owner.has_error() && !source_owner.is_missing(),
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
        visit(child, source, language, owner, result, plan_signatures);
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
