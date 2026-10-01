use std::collections::HashMap;

use tree_sitter::{Node, Parser, Query, QueryCursor, StreamingIterator};

use super::{SyntaxError, SyntaxLanguage};

/// Extracts and validates body-free declarations using the bundled source grammars.
pub struct DeclarationOverview;

/// Locates a token in the saved declaration document, independently of display layout.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DeclarationPosition {
    /// One-based line in the saved text.
    pub line: u32,
    /// Zero-based byte column in the saved text.
    pub column: u32,
}

/// Holds disposable formatted text and its mapping to the saved declaration document.
pub struct DeclarationPresentation {
    /// Text formatted with the current inspection rules.
    pub text: String,
    /// Saved text positions for display rows, with no target for inserted blank rows.
    pub source: Vec<Option<DeclarationPosition>>,
}

impl DeclarationOverview {
    /// Return the supported declaration language for a project source path.
    pub fn language(path: &str) -> Option<SyntaxLanguage> {
        match path.rsplit('.').next()? {
            "rs" => Some(SyntaxLanguage::Rust),
            "ts" => Some(SyntaxLanguage::Typescript),
            "tsx" => Some(SyntaxLanguage::Tsx),
            "lua" => Some(SyntaxLanguage::Lua),
            _ => None,
        }
    }

    /// Project source declarations while excluding executable bodies and initializers.
    pub fn extract(path: &str, source: &str) -> Result<String, SyntaxError> {
        Self::project(path, source, false)
    }

    fn project(path: &str, source: &str, formatted: bool) -> Result<String, SyntaxError> {
        let language = Self::language(path).ok_or_else(|| {
            SyntaxError::Query(format!("unsupported declaration language: {path}"))
        })?;
        let grammar = language.grammar();
        let mut parser = Parser::new();
        parser
            .set_language(&grammar)
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser
            .parse(source, None)
            .ok_or_else(|| SyntaxError::Query("declaration parsing was cancelled".into()))?;
        if tree.root_node().has_error() {
            return Err(SyntaxError::Query(format!(
                "invalid source syntax in {path}"
            )));
        }
        let query_text = match language {
            SyntaxLanguage::Rust => include_str!("../../query/forge/rust/design.scm"),
            SyntaxLanguage::Typescript | SyntaxLanguage::Tsx => {
                include_str!("../../query/forge/typescript/design.scm")
            }
            SyntaxLanguage::Lua => include_str!("../../query/forge/lua/design.scm"),
            _ => unreachable!(),
        };
        let query = Query::new(&grammar, query_text)
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let mut cursor = QueryCursor::new();
        let mut roles = HashMap::new();
        let mut matches = cursor.matches(&query, tree.root_node(), source.as_bytes());
        while let Some(matched) = matches.next() {
            for capture in matched.captures() {
                roles.insert(
                    capture.node.id(),
                    query.capture_names()[capture.index as usize],
                );
            }
        }
        let mut walk = tree.root_node().walk();
        let nodes = tree
            .root_node()
            .named_children(&mut walk)
            .filter(|node| retained(node.kind(), language))
            .collect::<Vec<_>>();
        let output = if formatted {
            format_group(&nodes, source, language, &roles, None)
        } else {
            nodes
                .into_iter()
                .map(|node| project_node(node, source, language, &roles, false))
                .filter(|text| !text.trim().is_empty())
                .collect::<Vec<_>>()
                .join("\n")
        };
        Ok(normalize(&output))
    }

    /// Admit signature-only text without changing its persisted layout.
    pub fn parse(path: &str, overview: &str) -> Result<String, SyntaxError> {
        if overview.len() > 1024 * 1024 || overview.contains('\0') {
            return Err(SyntaxError::Query(
                "declaration overview exceeds 1 MiB or contains NUL".into(),
            ));
        }
        let language = Self::language(path).ok_or_else(|| {
            SyntaxError::Query(format!("unsupported declaration language: {path}"))
        })?;
        let canonical = normalize(overview);
        let surrogate = declaration_surrogate(language, &canonical);
        let projected = Self::project(path, &surrogate, false)?;
        if projected
            .chars()
            .filter(|character| !character.is_whitespace())
            .collect::<String>()
            != canonical
                .chars()
                .filter(|character| !character.is_whitespace())
                .collect::<String>()
        {
            return Err(SyntaxError::Query(format!(
                "{path}: only declarations, documentation, and signature-only functions are allowed. Remove function bodies, initializers, and executable statements. Expected:\n{projected}"
            )));
        }
        Ok(overview.to_owned())
    }

    /// Format an admitted document and derive navigation positions without changing it.
    pub fn present(path: &str, overview: &str) -> Result<DeclarationPresentation, SyntaxError> {
        Self::parse(path, overview)?;
        let language = Self::language(path).ok_or_else(|| {
            SyntaxError::Query(format!("unsupported declaration language: {path}"))
        })?;
        let text = Self::format(path, overview)?;
        let original = layout_tokens(language, overview)?;
        let formatted = layout_tokens(language, &text)?;
        let mut source = vec![None; text.lines().count()];
        let mut formatted = formatted.into_iter().collect::<Vec<_>>();
        formatted.sort_by_key(|(_, (position, _))| (position.line, position.column));
        for (identity, (position, contents)) in formatted {
            let Some((saved, saved_contents)) = original.get(&identity) else {
                continue;
            };
            if contents != *saved_contents {
                continue;
            }
            for offset in 0..contents.lines().count().max(1) {
                let row = position.line as usize - 1 + offset;
                let Some(target) = source.get_mut(row) else {
                    continue;
                };
                let mapped = DeclarationPosition {
                    line: saved.line + offset as u32,
                    column: if offset == 0 { saved.column } else { 0 },
                };
                if target.is_none() {
                    *target = Some(mapped);
                }
            }
        }
        Ok(DeclarationPresentation { text, source })
    }

    fn format(path: &str, overview: &str) -> Result<String, SyntaxError> {
        let language = Self::language(path).unwrap();
        let text = Self::project(path, &declaration_surrogate(language, overview), true)?;
        format_indentation(language, &text)
    }
}

fn layout_tokens(
    language: SyntaxLanguage,
    text: &str,
) -> Result<std::collections::BTreeMap<Vec<(u16, usize)>, (DeclarationPosition, String)>, SyntaxError>
{
    let surrogate = declaration_surrogate(language, text);
    let mut parser = Parser::new();
    parser
        .set_language(&language.grammar())
        .map_err(|error| SyntaxError::Query(error.to_string()))?;
    let tree = parser
        .parse(&surrogate, None)
        .ok_or_else(|| SyntaxError::Query("declaration mapping was cancelled".into()))?;
    let lengths = text.lines().map(str::len).collect::<Vec<_>>();
    let mut tokens = std::collections::BTreeMap::new();
    collect_tokens(
        tree.root_node(),
        &surrogate,
        &lengths,
        &mut Vec::new(),
        &mut tokens,
    );
    Ok(tokens)
}

fn collect_tokens(
    node: Node<'_>,
    text: &str,
    lengths: &[usize],
    identity: &mut Vec<(u16, usize)>,
    tokens: &mut std::collections::BTreeMap<Vec<(u16, usize)>, (DeclarationPosition, String)>,
) {
    if node.child_count() == 0
        || matches!(node.kind(), "line_comment" | "block_comment" | "comment")
        || node.kind().contains("string")
        || node.kind().contains("template")
    {
        let start = node.start_position();
        if start.column < lengths.get(start.row).copied().unwrap_or(0) {
            let contents = &text[node.byte_range()];
            let contents = if matches!(node.kind(), "line_comment" | "block_comment" | "comment") {
                contents
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n")
            } else {
                contents.to_owned()
            };
            tokens.insert(
                identity.clone(),
                (
                    DeclarationPosition {
                        line: start.row as u32 + 1,
                        column: start.column as u32,
                    },
                    contents,
                ),
            );
        }
        return;
    }
    let mut occurrence = HashMap::<u16, usize>::new();
    let mut walk = node.walk();
    for child in node.children(&mut walk) {
        let ordinal = occurrence.entry(child.kind_id()).or_default();
        identity.push((child.kind_id(), *ordinal));
        *ordinal += 1;
        collect_tokens(child, text, lengths, identity, tokens);
        identity.pop();
    }
}

pub(super) fn declaration_surrogate(language: SyntaxLanguage, overview: &str) -> String {
    overview
        .lines()
        .map(|line| {
            let text = line.trim();
            if language == SyntaxLanguage::Lua
                && (text.starts_with("function ") || text.starts_with("local function "))
            {
                format!("{line} end\n")
            } else if language == SyntaxLanguage::Lua
                && !text.is_empty()
                && text
                    .chars()
                    .all(|character| character.is_alphanumeric() || matches!(character, '_' | '.'))
            {
                format!("{line} = nil\n")
            } else if matches!(language, SyntaxLanguage::Typescript | SyntaxLanguage::Tsx)
                && (text.trim_end_matches(';').ends_with("=>")
                    || text.contains("= function") && text.trim_end_matches(';').ends_with(')'))
            {
                format!(
                    "{} {{}}{}\n",
                    line.trim_end_matches(';'),
                    if text.ends_with(';') { ";" } else { "" }
                )
            } else {
                format!("{line}\n")
            }
        })
        .collect::<String>()
}

fn format_group(
    nodes: &[Node<'_>],
    source: &str,
    language: SyntaxLanguage,
    roles: &HashMap<usize, &str>,
    separator: Option<char>,
) -> String {
    let mut output = String::new();
    let mut attachment = Vec::new();
    let mut previous = "";
    for node in nodes {
        if matches!(
            node.kind(),
            "attribute_item"
                | "inner_attribute_item"
                | "line_comment"
                | "block_comment"
                | "comment"
        ) {
            attachment.push((
                node.kind() == "attribute_item",
                source[node.byte_range()].trim().to_owned(),
            ));
            continue;
        }
        let mut text = project_node(*node, source, language, roles, true)
            .trim()
            .to_owned();
        if text.is_empty() {
            continue;
        }
        let group = if matches!(
            node.kind(),
            "use_declaration" | "extern_crate_declaration" | "import_statement"
        ) {
            "import"
        } else if matches!(
            node.kind(),
            "field_declaration"
                | "enum_variant"
                | "enum_assignment"
                | "property_signature"
                | "public_field_definition"
        ) || separator == Some(',')
        {
            "field"
        } else {
            "declaration"
        };
        if !output.is_empty() {
            output.push('\n');
            if group == "declaration" || group != previous {
                output.push('\n');
            }
        }
        attachment.sort_by_key(|(attribute, _)| *attribute);
        for (_, text) in attachment.drain(..) {
            output.push_str(&text);
            output.push('\n');
        }
        if let Some(separator) = separator {
            if !text.ends_with(separator) {
                text.push(separator);
            }
        }
        output.push_str(&text);
        previous = group;
    }
    for (_, text) in attachment {
        if !output.is_empty() {
            output.push('\n');
        }
        output.push_str(&text);
    }
    output
}

fn format_indentation(language: SyntaxLanguage, text: &str) -> Result<String, SyntaxError> {
    let surrogate = declaration_surrogate(language, text);
    let mut parser = Parser::new();
    parser
        .set_language(&language.grammar())
        .map_err(|error| SyntaxError::Query(error.to_string()))?;
    let tree = parser
        .parse(&surrogate, None)
        .ok_or_else(|| SyntaxError::Query("declaration formatting was cancelled".into()))?;
    if tree.root_node().has_error() {
        return Err(SyntaxError::Query(
            "invalid formatted declaration syntax".into(),
        ));
    }
    let mut delimiters = Vec::new();
    let mut literal_rows = std::collections::HashSet::new();
    collect_layout(tree.root_node(), &mut delimiters, &mut literal_rows);
    delimiters.sort_unstable();
    let mut depth = 0usize;
    let mut cursor = 0usize;
    let mut output = String::new();
    for (row, line) in text.lines().enumerate() {
        while cursor < delimiters.len() && delimiters[cursor].0 < row {
            depth = depth.saturating_add_signed(delimiters[cursor].2);
            cursor += 1;
        }
        if !literal_rows.contains(&row) && line.trim().is_empty() && output.ends_with("\n\n") {
            continue;
        }
        if literal_rows.contains(&row) {
            output.push_str(line);
        } else if !line.trim().is_empty() {
            let closing = line
                .trim_start()
                .chars()
                .take_while(|character| matches!(character, '}' | ')' | ']' | '>'))
                .count();
            output.push_str(&"  ".repeat(depth.saturating_sub(closing)));
            output.push_str(line.trim_start());
        }
        output.push('\n');
    }
    Ok(normalize(&output))
}

fn collect_layout(
    node: Node<'_>,
    delimiters: &mut Vec<(usize, usize, isize)>,
    literal_rows: &mut std::collections::HashSet<usize>,
) {
    if node.kind().contains("string")
        || node.kind().contains("template")
        || node.kind() == "char_literal"
    {
        literal_rows.extend(node.start_position().row + 1..=node.end_position().row);
        return;
    }
    if let Some(change) = match node.kind() {
        "{" | "(" | "[" | "<" => Some(1),
        "}" | ")" | "]" | ">" => Some(-1),
        _ => None,
    } {
        delimiters.push((
            node.start_position().row,
            node.start_position().column,
            change,
        ));
    }
    let mut walk = node.walk();
    for child in node.children(&mut walk) {
        collect_layout(child, delimiters, literal_rows);
    }
}

fn retained(kind: &str, language: SyntaxLanguage) -> bool {
    match language {
        SyntaxLanguage::Rust => matches!(
            kind,
            "function_item"
                | "function_signature_item"
                | "struct_item"
                | "enum_item"
                | "trait_item"
                | "impl_item"
                | "mod_item"
                | "type_item"
                | "const_item"
                | "static_item"
                | "use_declaration"
                | "extern_crate_declaration"
                | "attribute_item"
                | "inner_attribute_item"
                | "line_comment"
                | "block_comment"
                | "foreign_mod_item"
        ),
        SyntaxLanguage::Typescript | SyntaxLanguage::Tsx => matches!(
            kind,
            "function_declaration"
                | "function_signature"
                | "generator_function_declaration"
                | "class_declaration"
                | "abstract_class_declaration"
                | "interface_declaration"
                | "enum_declaration"
                | "type_alias_declaration"
                | "lexical_declaration"
                | "variable_declaration"
                | "export_statement"
                | "import_statement"
                | "internal_module"
                | "ambient_declaration"
                | "comment"
        ),
        SyntaxLanguage::Lua => matches!(
            kind,
            "function_declaration"
                | "variable_declaration"
                | "assignment_statement"
                | "comment"
                | "return_statement"
        ),
        _ => false,
    }
}

fn project_node(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    roles: &HashMap<usize, &str>,
    formatted: bool,
) -> String {
    let text = &source[node.byte_range()];
    if node.kind() == "export_statement"
        && let Some(value) = node.child_by_field_name("value")
        && matches!(value.kind(), "arrow_function" | "function_expression")
        && let Some(body) = value.child_by_field_name("body")
    {
        return format!("{};", source[node.start_byte()..body.start_byte()].trim());
    }
    if node.kind() == "export_statement"
        && node.child_by_field_name("value").is_some_and(|value| {
            !matches!(value.kind(), "function_expression" | "class" | "identifier")
        })
    {
        return String::new();
    }
    if matches!(
        node.kind(),
        "class_static_block" | "macro_invocation" | "macro_definition" | "expression_statement"
    ) {
        return String::new();
    }
    if language == SyntaxLanguage::Lua {
        if node.kind() == "return_statement" {
            let value = text
                .trim()
                .strip_prefix("return ")
                .unwrap_or_default()
                .trim();
            return if !value.is_empty()
                && value
                    .chars()
                    .all(|character| character.is_alphanumeric() || character == '_')
            {
                text.into()
            } else {
                String::new()
            };
        }
        if matches!(node.kind(), "variable_declaration" | "assignment_statement") {
            let mut walk = node.walk();
            let assignment = if node.kind() == "variable_declaration" {
                node.named_children(&mut walk)
                    .find(|child| child.kind() == "assignment_statement")
            } else {
                Some(node)
            };
            if let Some(assignment) = assignment {
                let prefix = text
                    .split_once('=')
                    .map(|(prefix, _)| prefix.trim())
                    .unwrap_or(text);
                let mut walk = assignment.walk();
                if let Some(expressions) = assignment
                    .named_children(&mut walk)
                    .find(|child| child.kind() == "expression_list")
                {
                    let mut walk = expressions.walk();
                    if let Some(value) = expressions.named_children(&mut walk).next() {
                        let name = prefix.trim_start_matches("local ").trim();
                        if value.kind() == "function_definition" && !name.contains(',') {
                            if let Some(parameters) = value.child_by_field_name("parameters") {
                                return format!(
                                    "{}function {name}{}",
                                    if prefix.starts_with("local ") {
                                        "local "
                                    } else {
                                        ""
                                    },
                                    flatten(&source[parameters.byte_range()])
                                );
                            }
                        }
                        if value.kind() == "table_constructor" && !name.contains(',') {
                            let mut output = prefix.to_owned();
                            project_lua_table(value, name, source, &mut output);
                            return output;
                        }
                    }
                }
                return prefix.into();
            }
        }
    }
    if roles.get(&node.id()) == Some(&"design.callable") {
        if language == SyntaxLanguage::Lua {
            return node
                .child_by_field_name("parameters")
                .map(|parameters| flatten(&source[node.start_byte()..parameters.end_byte()]))
                .unwrap_or_default();
        }
        if let Some(body) = node.child_by_field_name("body") {
            return format!("{};", source[node.start_byte()..body.start_byte()].trim());
        }
    }
    if roles.get(&node.id()) == Some(&"design.value")
        || matches!(node.kind(), "enum_assignment" | "enum_variant")
    {
        if language == SyntaxLanguage::Lua {
            if let Some((prefix, _)) = text.split_once('=') {
                return prefix.trim_end().to_owned();
            }
        } else if let Some(value) = node.child_by_field_name("value") {
            if matches!(value.kind(), "arrow_function" | "function_expression") {
                if let Some(body) = value.child_by_field_name("body") {
                    return source[node.start_byte()..body.start_byte()]
                        .trim()
                        .to_owned();
                }
            }
            let prefix = source[node.start_byte()..value.start_byte()]
                .trim_end()
                .trim_end_matches('=')
                .trim_end();
            return format!(
                "{}{}",
                prefix,
                if text.trim_end().ends_with(';') {
                    ";"
                } else {
                    ""
                }
            );
        }
    }
    if formatted && node.kind() == "ordered_field_declaration_list" {
        let mut cursor = node.walk();
        let mut position = node.start_byte() + 1;
        let mut fields = Vec::new();
        for child in node.children(&mut cursor) {
            if child.kind() == "," || child.kind() == ")" {
                let field = source[position..child.start_byte()].trim();
                if !field.is_empty() { fields.push(flatten(field)); }
                position = child.end_byte();
            }
        }
        return format!("(\n{}\n)", fields.iter().map(|field| format!("{field},")).collect::<Vec<_>>().join("\n"));
    }
    if formatted
        && matches!(
            node.kind(),
            "declaration_list"
                | "field_declaration_list"
                | "enum_variant_list"
                | "class_body"
                | "object_type"
                | "enum_body"
                | "statement_block"
        )
    {
        let separator = match node.kind() {
            "field_declaration_list" | "enum_variant_list" | "enum_body" => Some(','),
            "class_body" | "object_type" => Some(';'),
            _ => None,
        };
        let mut walk = node.walk();
        let nodes = node.named_children(&mut walk).collect::<Vec<_>>();
        let contents = format_group(&nodes, source, language, roles, separator);
        return if contents.is_empty() {
            "{}".into()
        } else {
            format!("{{\n{contents}\n}}")
        };
    }
    let mut result = String::new();
    let mut position = node.start_byte();
    let mut walk = node.walk();
    for child in node.named_children(&mut walk) {
        result.push_str(&source[position..child.start_byte()]);
        result.push_str(&project_node(child, source, language, roles, formatted));
        position = child.end_byte();
    }
    result.push_str(&source[position..node.end_byte()]);
    result
}

fn project_lua_table(table: Node<'_>, owner: &str, source: &str, output: &mut String) {
    let mut walk = table.walk();
    for field in table.named_children(&mut walk) {
        if field.kind() == "comment" {
            output.push('\n');
            output.push_str(&source[field.byte_range()]);
            continue;
        }
        if field.kind() != "field" {
            continue;
        }
        let Some(name) = field.child_by_field_name("name") else {
            continue;
        };
        if name.kind() != "identifier" {
            continue;
        }
        let binding = format!("{owner}.{}", &source[name.byte_range()]);
        output.push('\n');
        match field.child_by_field_name("value") {
            Some(value) if value.kind() == "function_definition" => {
                if let Some(parameters) = value.child_by_field_name("parameters") {
                    output.push_str(&format!(
                        "function {binding}{}",
                        flatten(&source[parameters.byte_range()])
                    ));
                }
            }
            Some(value) if value.kind() == "table_constructor" => {
                output.push_str(&binding);
                project_lua_table(value, &binding, source, output);
            }
            _ => output.push_str(&binding),
        }
    }
}

fn flatten(text: &str) -> String {
    text.lines().map(str::trim).collect::<Vec<_>>().join(" ")
}

fn normalize(text: &str) -> String {
    let mut result = text.replace("\r\n", "\n").trim().to_owned();
    if !result.is_empty() {
        result.push('\n');
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arena_declarations_have_two_space_indentation_and_attached_documentation() {
        let source = "use bevy::prelude::*;\nuse crate::config::ArenaConfig;\n#[derive(Debug, Clone)]\n/// Owns validated configuration.\npub struct ArenaPlugin {\n    config: ArenaConfig,\n}\nimpl ArenaPlugin {\n    /// Takes ownership of configuration.\n    pub fn new(config: ArenaConfig) -> Self;\n    pub fn config(&self) -> &ArenaConfig;\n}\nimpl Default for ArenaPlugin {\n    fn default() -> Self;\n}\nimpl Plugin for ArenaPlugin {\n    fn build(&self, app: &mut App);\n}\n#[derive(SystemSet, Debug, Clone, Copy, PartialEq, Eq, Hash)]\npub(crate) enum ArenaSet {\n    Input,\n    Movement,\n}\n#[derive(Component)]\npub(crate) struct RoundEntity;\n";
        let expected = "use bevy::prelude::*;\nuse crate::config::ArenaConfig;\n\n/// Owns validated configuration.\n#[derive(Debug, Clone)]\npub struct ArenaPlugin {\n  config: ArenaConfig,\n}\n\nimpl ArenaPlugin {\n  /// Takes ownership of configuration.\n  pub fn new(config: ArenaConfig) -> Self;\n\n  pub fn config(&self) -> &ArenaConfig;\n}\n\nimpl Default for ArenaPlugin {\n  fn default() -> Self;\n}\n\nimpl Plugin for ArenaPlugin {\n  fn build(&self, app: &mut App);\n}\n\n#[derive(SystemSet, Debug, Clone, Copy, PartialEq, Eq, Hash)]\npub(crate) enum ArenaSet {\n  Input,\n  Movement,\n}\n\n#[derive(Component)]\npub(crate) struct RoundEntity;\n";
        assert_eq!(
            DeclarationOverview::present("arena.rs", source)
                .unwrap()
                .text,
            expected
        );
        assert_eq!(
            DeclarationOverview::present(
                "arena.rs",
                &DeclarationOverview::extract("arena.rs", source).unwrap()
            )
            .unwrap()
            .text,
            expected
        );
        assert_eq!(
            DeclarationOverview::parse("arena.rs", expected).unwrap(),
            expected
        );
    }

    #[test]
    fn nested_variant_fields_keep_commas_and_member_spacing() {
        let source = "pub enum State { Ready { id: u64, name: String }, Complete }\nimpl State { pub fn id(&self) -> u64 { 0 } pub fn reset(&mut self) {} }";
        let expected = "pub enum State {\n  Ready {\n    id: u64,\n    name: String,\n  },\n  Complete,\n}\n\nimpl State {\n  pub fn id(&self) -> u64;\n\n  pub fn reset(&mut self);\n}\n";
        assert_eq!(
            DeclarationOverview::present(
                "state.rs",
                &DeclarationOverview::extract("state.rs", source).unwrap()
            )
            .unwrap()
            .text,
            expected
        );
        assert_eq!(
            DeclarationOverview::parse("state.rs", expected).unwrap(),
            expected
        );
    }

    #[test]
    fn multiline_literal_types_retain_their_exact_contents() {
        let source = "export function label(value: `first  \n  second`): string { return value; }";
        let overview = DeclarationOverview::extract("labels.ts", source).unwrap();
        assert!(overview.contains("`first  \n  second`"));
        assert_eq!(
            DeclarationOverview::parse("labels.ts", &overview).unwrap(),
            overview
        );
    }

    #[test]
    fn rust_resources_and_nested_contracts_round_trip() {
        let source = r#"pub const CAPACITY: usize = 16;
static LABEL: &str = "ready";
pub mod storage {
    pub trait Store<T> {
        const LIMIT: usize = 32;
        fn get(&self, id: u64) -> Option<T>;
        fn reset(&mut self) { panic!("implementation"); }
    }
    pub enum State { Ready = 1, Complete = 2 }
}
"#;
        let overview = DeclarationOverview::extract("resources.rs", source).unwrap();
        let windows_overview =
            DeclarationOverview::extract("resources.rs", &source.replace('\n', "\r\n")).unwrap();
        assert_eq!(windows_overview, overview);
        assert_eq!(
            DeclarationOverview::parse("resources.rs", &windows_overview).unwrap(),
            windows_overview
        );
        assert!(!overview.contains("panic!"));
        assert!(!overview.contains("= 16"));
        assert!(overview.contains("const CAPACITY: usize;"));
        assert_eq!(
            DeclarationOverview::parse("resources.rs", &overview).unwrap(),
            overview
        );
    }

    #[test]
    fn typescript_nested_and_default_callables_round_trip() {
        let source = r#"export namespace Storage {
    export enum State { Ready = 1, Complete = 2 }
    export class Client {
        private cache: Map<string, string> = new Map();
        get size(): number { return this.cache.size; }
        set size(value: number) { this.cache.clear(); }
        async get<T>(id: string): Promise<T | undefined> { return undefined; }
    }
}
export default (id: string): string => id;
"#;
        let overview = DeclarationOverview::extract("contracts.ts", source).unwrap();
        assert!(!overview.contains("new Map"));
        assert!(!overview.contains("this.cache"));
        assert!(overview.contains("export default (id: string): string =>"));
        assert_eq!(
            DeclarationOverview::parse("contracts.ts", &overview).unwrap(),
            overview
        );
    }

    #[test]
    fn lua_nested_bindings_preserve_function_annotations() {
        let source = r#"local M = {
    storage = {
        ---@param id string
        ---@return string
        get = function(id) return id end,
    },
}
return M
"#;
        let overview = DeclarationOverview::extract("contracts.lua", source).unwrap();
        assert!(overview.contains("M.storage"));
        assert!(overview.contains("function M.storage.get(id)"));
        assert!(overview.contains("---@param id string"));
        assert!(!overview.contains("return id"));
        assert_eq!(
            DeclarationOverview::parse("contracts.lua", &overview).unwrap(),
            overview
        );
    }

    #[test]
    fn signature_projection_preserves_spaces_inside_literal_types() {
        let source =
            r#"export function label(value: "two  spaces"): "two  spaces" { return value; }"#;
        let overview = DeclarationOverview::extract("labels.ts", source).unwrap();
        assert!(overview.contains(r#""two  spaces""#));
        assert_eq!(
            DeclarationOverview::parse("labels.ts", &overview).unwrap(),
            overview
        );
    }

    #[test]
    fn rust_preserves_complete_contracts_and_excludes_bodies() {
        let source = "/// Texture owner.\npub struct Texture { pub id: u64, pending: Option<Vec<String>> }\nimpl Texture { pub fn request<'a>(&'a mut self, id: u64) -> Option<&'a String> { None } }\n";
        let overview = DeclarationOverview::extract("src/texture.rs", source).unwrap();
        assert!(overview.contains("pending: Option<Vec<String>>"));
        assert!(
            overview.contains("pub fn request<'a>(&'a mut self, id: u64) -> Option<&'a String>;")
        );
        assert!(!overview.contains("None"));
        assert_eq!(
            DeclarationOverview::parse("src/texture.rs", &overview).unwrap(),
            overview
        );
        let changed_body = source.replace("None", "todo!()");
        assert_eq!(
            DeclarationOverview::extract("src/texture.rs", &changed_body).unwrap(),
            overview
        );
        assert!(DeclarationOverview::parse("src/texture.rs", source).is_err());
    }

    #[test]
    fn typescript_and_lua_round_trip_without_implementations() {
        let source = "export interface Store { get(id: string): Promise<string>; }\nexport class Service { private store: Store; get(id: string): Promise<string> { return this.store.get(id); } }\n";
        let overview = DeclarationOverview::extract("service.ts", source).unwrap();
        assert!(overview.contains("private store: Store"));
        assert!(!overview.contains("return this"));
        assert_eq!(
            DeclarationOverview::parse("service.ts", &overview).unwrap(),
            overview
        );
        let source = "local M = {}\n---@param id string\n---@return string\nfunction M.get(id) return id end\nreturn M\n";
        let overview = DeclarationOverview::extract("store.lua", source).unwrap();
        assert!(overview.contains("---@param id string"));
        assert!(overview.contains("function M.get(id)"));
        assert!(!overview.contains("return id"));
        assert_eq!(
            DeclarationOverview::parse("store.lua", &overview).unwrap(),
            overview
        );
        assert!(DeclarationOverview::parse("store.lua", source).is_err());
    }

    #[test]
    fn callable_bindings_keep_signatures_without_inventing_return_types() {
        for (path, source) in [
            (
                "service.ts",
                "export const get = <T>(id: T) => id;\nexport class Service { static { launch(); } value = get(2); }\n",
            ),
            (
                "module.lua",
                "local M = { get = function(id) return id end, value = compute() }\nM.close = function() close() end\nreturn M\n",
            ),
        ] {
            let overview = DeclarationOverview::extract(path, source).unwrap();
            assert!(!overview.contains("unknown"));
            assert!(!overview.contains("launch()"));
            assert!(!overview.contains("compute()"));
            assert_eq!(
                DeclarationOverview::parse(path, &overview).unwrap(),
                overview
            );
            if path.ends_with("lua") {
                assert!(overview.contains("function M.get(id)"));
                assert!(overview.contains("function M.close()"));
            } else {
                assert!(overview.contains("get = <T>(id: T) =>"));
            }
        }
    }

    #[test]
    fn overview_admission_preserves_layout_and_presentation_is_disposable() {
        let text = "pub type RequestId = u64;\n\npub enum RequestStatus {\n    Pending,\n    Completed,\n}\n\nimpl Registry {\n    pub fn request(\n        &mut self,\n        id: RequestId,\n    ) -> RequestStatus;\n}\n";
        let parsed = DeclarationOverview::parse("registry.rs", text).unwrap();
        assert_eq!(parsed, text);
        let display = DeclarationOverview::present("registry.rs", &parsed).unwrap();
        assert!(display.text.contains("  pub fn request(\n    &mut self,"));
        assert_eq!(
            DeclarationOverview::parse("registry.rs", text).unwrap(),
            text
        );
        assert!(
            DeclarationOverview::parse(
                "registry.rs",
                &text.replace("-> RequestStatus;", "-> RequestStatus { todo!() }")
            )
            .is_err()
        );
    }

    #[test]
    fn display_positions_distinguish_compact_members_and_reordered_attachments() {
        let saved = "#[derive(Clone)]\n/// Registry.\npub struct Registry { first: u64, second: u64 }\nimpl Registry { pub fn first(&self) -> u64; pub fn second(&self) -> u64; }\n";
        let display = DeclarationOverview::present("registry.rs", saved).unwrap();
        for (row, text) in display.text.lines().enumerate() {
            if text.trim().is_empty() {
                assert_eq!(display.source[row], None);
                continue;
            }
            let position = display.source[row].expect("every declaration row maps to saved text");
            let original = saved.lines().nth(position.line as usize - 1).unwrap();
            let suffix = &original[position.column as usize..];
            if text.contains("second:") {
                assert!(suffix.starts_with("second:"));
            }
            if text.contains("fn second") {
                assert!(suffix.starts_with("pub fn second"));
            }
            if text.contains("/// Registry") {
                assert_eq!(position.line, 2);
            }
            if text.contains("#[derive") {
                assert_eq!(position.line, 1);
            }
        }
        assert_eq!(
            DeclarationOverview::parse("registry.rs", saved).unwrap(),
            saved
        );
    }

    #[test]
    fn native_language_display_mappings_keep_literal_and_annotation_rows() {
        for (path, saved) in [
            (
                "store.ts",
                "export interface Store { first: string; second: string; get(id: `first  \n  second`): string; }\n",
            ),
            (
                "store.lua",
                "local M\n---@param id string\nfunction M.get(id)\nreturn M\n",
            ),
        ] {
            let display = DeclarationOverview::present(path, saved).unwrap();
            for (row, text) in display.text.lines().enumerate() {
                if !text.trim().is_empty() {
                    assert!(display.source[row].is_some(), "{path}: {text}");
                }
            }
            assert_eq!(DeclarationOverview::parse(path, saved).unwrap(), saved);
        }
    }
}
