use std::collections::HashMap;

use tree_sitter::{Node, Parser, Query, QueryCursor, StreamingIterator};

use super::{ConfigurationFormat, SyntaxError, SyntaxLanguage};

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
    /// Locate a saved declaration token in the current inspection presentation.
    pub fn display_position(path: &str, saved: &str, source: DeclarationPosition) -> Result<DeclarationPosition, SyntaxError> {
        let presentation = Self::present(path, saved)?;
        let language = Self::language(path).ok_or_else(|| SyntaxError::Language(path.into()))?;
        let saved_tokens = layout_tokens(language, saved)?;
        let displayed_tokens = layout_tokens(language, &presentation.text)?;
        let identity = saved_tokens.iter().find(|(_, (position, _))| *position == source).map(|(identity, _)| identity).ok_or_else(|| SyntaxError::Query("saved declaration token is unavailable".into()))?;
        displayed_tokens.get(identity).map(|(position, _)| *position).ok_or_else(|| SyntaxError::Query("declaration token is hidden in the presentation".into()))
    }
    /// Map a selected rendered token to its saved declaration position.
    pub fn token_position(path: &str, saved: &str, anchor: DeclarationPosition, row: &str, column: usize) -> Result<DeclarationPosition, SyntaxError> {
        let presentation = Self::present(path, saved)?;
        let language = Self::language(path).ok_or_else(|| SyntaxError::Language(path.into()))?;
        let saved_tokens = layout_tokens(language, saved)?;
        let displayed_tokens = layout_tokens(language, &presentation.text)?;
        let display_row = presentation.source.iter().enumerate().find(|(index, position)| **position == Some(anchor) && presentation.text.lines().nth(*index) == Some(row))
            .or_else(|| presentation.source.iter().enumerate().find(|(_, position)| **position == Some(anchor))).map(|(index, _)| index as u32 + 1);
        if let Some(display_row) = display_row {
            for (identity, (position, token)) in displayed_tokens {
                if position.line == display_row && position.column as usize <= column && column < position.column as usize + token.len() {
                    if let Some((source, _)) = saved_tokens.get(&identity) { return Ok(*source); }
                }
            }
        }
        Err(SyntaxError::Query("cursor is not on a mapped declaration token".into()))
    }
    /// Admit source declarations and complete configuration documents independently of highlighting.
    pub fn supports(path: &str) -> bool {
        Self::language(path).is_some() || ConfigurationFormat::for_path(path).is_some()
    }

    /// Return the inspection filetype, including configuration without a bundled grammar.
    pub fn filetype(path: &str) -> &'static str {
        ConfigurationFormat::for_path(path)
            .map(ConfigurationFormat::name)
            .or_else(|| Self::language(path).map(SyntaxLanguage::name))
            .unwrap_or("text")
    }

    /// Return the bundled highlighting language when one exists for an admitted path.
    pub fn language(path: &str) -> Option<SyntaxLanguage> {
        match path.rsplit('.').next()? {
            "rs" => Some(SyntaxLanguage::Rust),
            "ts" | "mts" | "cts" => Some(SyntaxLanguage::Typescript),
            "tsx" => Some(SyntaxLanguage::Tsx),
            "lua" => Some(SyntaxLanguage::Lua),
            "toml" => Some(SyntaxLanguage::Toml),
            "json" | "jsonc" => Some(SyntaxLanguage::Json),
            "yaml" | "yml" => Some(SyntaxLanguage::Yaml),
            _ => None,
        }
    }

    /// Project source declarations while excluding executable bodies and initializers.
    pub fn extract(path: &str, source: &str) -> Result<String, SyntaxError> {
        Self::project(path, source, false)
    }

    fn project(path: &str, source: &str, formatted: bool) -> Result<String, SyntaxError> {
        if let Some(config) = ConfigurationFormat::for_path(path) {
            config.validate(path, source)?;
            return Ok(source.to_owned());
        }
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
        if let Some(config) = ConfigurationFormat::for_path(path) {
            config.validate(path, overview)?;
            return Ok(overview.to_owned());
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
        if ConfigurationFormat::for_path(path).is_some() {
            return Ok(DeclarationPresentation {
                text: overview.to_owned(),
                source: overview.lines().enumerate().map(|(row, _)| {
                    Some(DeclarationPosition { line: row as u32 + 1, column: 0 })
                }).collect(),
            });
        }
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

    /// Canonicalize declarations with a bounded line width without modifying source files.
    pub fn format_with_width(
        path: &str,
        overview: &str,
        line_width: usize,
    ) -> Result<String, SyntaxError> {
        if !(40..=240).contains(&line_width) {
            return Err(SyntaxError::Query(
                "declaration line width must be between 40 and 240".into(),
            ));
        }
        Self::parse(path, overview)?;
        if ConfigurationFormat::for_path(path).is_some() {
            return Ok(overview.to_owned());
        }
        let language = Self::language(path).unwrap();
        let text = Self::format(path, overview)?;
        let text = wrap_parameters(language, &text, line_width)?;
        let text = format_indentation(language, &text)?;
        let text = wrap_comments(language, &text, line_width)?;
        Self::parse(path, &text)?;
        Ok(text)
    }

    fn format(path: &str, overview: &str) -> Result<String, SyntaxError> {
        let language = Self::language(path).unwrap();
        let text = Self::project(path, &declaration_surrogate(language, overview), true)?;
        format_indentation(language, &text)
    }
}

fn formatting_tree(language: SyntaxLanguage, text: &str) -> Result<tree_sitter::Tree, SyntaxError> {
    let mut parser = Parser::new();
    parser
        .set_language(&language.grammar())
        .map_err(|error| SyntaxError::Query(error.to_string()))?;
    let tree = parser
        .parse(declaration_surrogate(language, text), None)
        .ok_or_else(|| SyntaxError::Query("declaration formatting was cancelled".into()))?;
    if tree.root_node().has_error() {
        return Err(SyntaxError::Query(
            "invalid declaration formatting input".into(),
        ));
    }
    Ok(tree)
}

fn wrap_parameters(
    language: SyntaxLanguage,
    text: &str,
    line_width: usize,
) -> Result<String, SyntaxError> {
    let tree = formatting_tree(language, text)?;
    let lines = text.lines().collect::<Vec<_>>();
    let mut offsets = Vec::new();
    let mut offset = 0;
    for line in &lines {
        offsets.push(offset);
        offset += line.len() + 1;
    }
    let mut replacement = Vec::new();
    collect_parameters(
        tree.root_node(),
        text,
        &lines,
        &offsets,
        line_width,
        &mut replacement,
    );
    replacement.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));
    let mut output = text.to_owned();
    for (start, end, contents) in replacement {
        output.replace_range(start..end, &contents);
    }
    Ok(output)
}

fn collect_parameters(
    node: Node<'_>,
    text: &str,
    lines: &[&str],
    offsets: &[usize],
    line_width: usize,
    replacement: &mut Vec<(usize, usize, String)>,
) {
    if matches!(node.kind(), "parameters" | "formal_parameters") {
        let first = node.start_position();
        let last = node.end_position();
        if first.row == last.row
            && lines
                .get(first.row)
                .is_some_and(|line| line.chars().count() > line_width)
        {
            let start = offsets[first.row] + first.column;
            let end = offsets[last.row] + last.column;
            if end <= text.len()
                && text.get(start..start + 1) == Some("(")
                && text.get(end - 1..end) == Some(")")
            {
                let mut walk = node.walk();
                let children = node.children(&mut walk).collect::<Vec<_>>();
                if !children
                    .iter()
                    .any(|child| child.kind().contains("comment"))
                {
                    let mut parts = Vec::new();
                    let mut cursor = start + 1;
                    for child in children.iter().filter(|child| child.kind() == ",") {
                        let comma = offsets[first.row] + child.start_position().column;
                        parts.push(text[cursor..comma + 1].trim());
                        cursor = comma + 1;
                    }
                    let final_part = text[cursor..end - 1].trim();
                    if !final_part.is_empty() {
                        parts.push(final_part);
                    }
                    if !parts.is_empty() {
                        replacement.push((start, end, format!("(\n{}\n)", parts.join("\n"))));
                    }
                }
            }
        }
        return;
    }
    if node.kind().contains("string")
        || node.kind().contains("template")
        || node.kind().contains("comment")
    {
        return;
    }
    let mut walk = node.walk();
    for child in node.children(&mut walk) {
        collect_parameters(child, text, lines, offsets, line_width, replacement);
    }
}

fn collect_comment_rows(node: Node<'_>, rows: &mut std::collections::HashSet<usize>) {
    if matches!(node.kind(), "line_comment" | "comment") {
        let first = node.start_position();
        let last = node.end_position();
        if first.row == last.row || last.row == first.row + 1 && last.column == 0 {
            rows.insert(first.row);
        }
        return;
    }
    if node.kind().contains("string")
        || node.kind().contains("template")
        || node.kind() == "block_comment"
    {
        return;
    }
    let mut walk = node.walk();
    for child in node.children(&mut walk) {
        collect_comment_rows(child, rows);
    }
}

fn wrap_comments(
    language: SyntaxLanguage,
    text: &str,
    line_width: usize,
) -> Result<String, SyntaxError> {
    let tree = formatting_tree(language, text)?;
    let mut rows = std::collections::HashSet::new();
    collect_comment_rows(tree.root_node(), &mut rows);
    let mut output = Vec::new();
    let mut paragraph = String::new();
    let mut prefix = String::new();
    let mut fenced = false;
    for (row, line) in text.lines().enumerate() {
        let trimmed = line.trim_start();
        let marker = ["///", "//!", "//", "--"]
            .into_iter()
            .find(|marker| trimmed.starts_with(marker));
        let Some(marker) = marker.filter(|_| rows.contains(&row)) else {
            flush_comment(&mut output, &mut paragraph, &prefix, line_width);
            output.push(line.to_owned());
            fenced = false;
            continue;
        };
        let contents = &trimmed[marker.len()..];
        let content = contents.trim();
        let next_prefix = format!("{}{} ", &line[..line.len() - trimmed.len()], marker);
        let fence = content.starts_with("```") || content.starts_with("~~~");
        let structured = content.is_empty()
            || fenced
            || fence
            || contents.starts_with("    ")
            || content.starts_with(['@', '#', '|', '-', '*', '>', '[', '<'])
            || content
                .chars()
                .next()
                .is_some_and(|character| character.is_ascii_digit());
        if prefix != next_prefix || structured {
            flush_comment(&mut output, &mut paragraph, &prefix, line_width);
        }
        prefix = next_prefix;
        if structured {
            output.push(line.to_owned());
            if fence {
                fenced = !fenced;
            }
        } else {
            if !paragraph.is_empty() {
                paragraph.push(' ');
            }
            paragraph.push_str(content);
        }
    }
    flush_comment(&mut output, &mut paragraph, &prefix, line_width);
    Ok(normalize(&output.join("\n")))
}

fn flush_comment(
    output: &mut Vec<String>,
    paragraph: &mut String,
    prefix: &str,
    line_width: usize,
) {
    if paragraph.is_empty() {
        return;
    }
    let mut line = prefix.to_owned();
    let mut inline_code = false;
    let mut word = String::new();
    let mut words = Vec::new();
    for part in paragraph.split_whitespace() {
        if !word.is_empty() {
            word.push(' ');
        }
        word.push_str(part);
        if part.chars().filter(|character| *character == '`').count() % 2 == 1 {
            inline_code = !inline_code;
        }
        if !inline_code {
            words.push(std::mem::take(&mut word));
        }
    }
    if !word.is_empty() {
        words.push(word);
    }
    for word in words {
        if line.len() > prefix.len() && line.chars().count() + 1 + word.chars().count() > line_width
        {
            output.push(line);
            line = prefix.to_owned();
        }
        if line.len() > prefix.len() {
            line.push(' ');
        }
        line.push_str(&word);
    }
    output.push(line);
    paragraph.clear();
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
    let mut lua_header = false;
    overview
        .lines()
        .map(|line| {
            let text = line.trim();
            if language == SyntaxLanguage::Lua
                && (text.starts_with("function ") || text.starts_with("local function "))
            {
                lua_header = !text.ends_with(')');
                if lua_header {
                    format!("{line}\n")
                } else {
                    format!("{line} end\n")
                }
            } else if lua_header {
                if text.ends_with(')') {
                    lua_header = false;
                    format!("{line} end\n")
                } else {
                    format!("{line}\n")
                }
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
                | "function_call"
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
                        if !prefix.contains(',')
                            && expressions.named_child_count() == 1
                            && lua_literal_require(&source[value.byte_range()]).is_some()
                        {
                            return text.into();
                        }
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
    if language == SyntaxLanguage::Lua && node.kind() == "function_call" {
        return if lua_literal_require(text).is_some() { text.into() } else { String::new() };
    }
    if roles.get(&node.id()) == Some(&"design.callable") {
        if language == SyntaxLanguage::Lua {
            return node
                .child_by_field_name("parameters")
                .map(|parameters| {
                    let header = &source[node.start_byte()..parameters.end_byte()];
                    if formatted {
                        header.to_owned()
                    } else {
                        flatten(header)
                    }
                })
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

fn lua_literal_require(source: &str) -> Option<&str> {
    let argument = source.trim().strip_prefix("require")?.trim_start().strip_prefix('(')?.trim_start();
    let quote = argument.chars().next()?;
    if !matches!(quote, '\'' | '"') { return None; }
    let (module, rest) = argument[1..].split_once(quote)?;
    rest.trim_start().starts_with(')').then_some(module)
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
    fn toml_configuration_retains_values_comments_and_multiline_string_contents() {
        let manifest = "# Engine package\r\n[package]\r\nname = \"arena\"\r\nversion = \"0.1.0\"\r\n\r\n[dependencies]\r\nbevy = { version = \"0.17\", default-features = false, features = [\"std\", \"bevy_sprite\"] }\r\n\r\n[package.metadata]\r\nnotes = \"\"\"first\r\n  preserve indentation\r\nlast\"\"\"\r\n";
        assert_eq!(DeclarationOverview::extract("Cargo.toml", manifest).unwrap(), manifest);
        assert_eq!(DeclarationOverview::parse("Cargo.toml", manifest).unwrap(), manifest);
        assert_eq!(DeclarationOverview::format_with_width("Cargo.toml", manifest, 40).unwrap(), manifest);
        let display = DeclarationOverview::present("Cargo.toml", manifest).unwrap();
        assert_eq!(display.text, manifest);
        let dependency_row = manifest.lines().position(|line| line.starts_with("bevy =")).unwrap();
        assert_eq!(display.source[dependency_row].unwrap().line, dependency_row as u32 + 1);
        assert!(DeclarationOverview::parse("Cargo.toml", "[dependencies\nbevy =").is_err());
    }

    #[test]
    fn configured_width_wraps_contracts_and_keeps_literals_and_fences_opaque() {
        let source = "/// This documentation describes the complete request contract and explains the lifetime of the returned handle across repeated calls.\n///\n/// ```rust\n/// let very_long_example = \"this example must remain exactly as written despite its length\";\n/// ```\npub fn request(texture: TextureId, registry: &mut TextureRegistry, decoder: &TextureDecoder) -> TextureRequest;\n";
        let formatted = DeclarationOverview::format_with_width("texture.rs", source, 60).unwrap();
        assert!(formatted.contains("pub fn request(\n  texture: TextureId,\n"));
        assert!(formatted.contains("/// let very_long_example = \"this example must remain exactly as written despite its length\";"));
        assert!(
            formatted
                .lines()
                .filter(|line| line.starts_with("/// ") && !line.starts_with("/// let"))
                .all(|line| line.chars().count() <= 60)
        );
        assert_eq!(
            DeclarationOverview::format_with_width("texture.rs", &formatted, 60).unwrap(),
            formatted
        );
        let wide = DeclarationOverview::format_with_width("texture.rs", source, 240).unwrap();
        assert!(wide.contains("pub fn request(texture:"));
        let literal = "export type Message = `first\n    /// this is literal content that must never be reformatted or interpreted as documentation\nlast`;\n";
        let formatted = DeclarationOverview::format_with_width("message.ts", literal, 40).unwrap();
        assert!(formatted.contains("    /// this is literal content that must never be reformatted or interpreted as documentation"));
    }

    #[test]
    fn parameter_wrapping_preserves_nested_types_and_native_callables() {
        for (path, source) in [
            (
                "registry.rs",
                "pub fn request(registry: &mut Registry, values: Result<(First, Second), Error>, decoder: Decoder) -> Handle;\n",
            ),
            (
                "registry.ts",
                "export interface Registry { request(registry: Registry, values: Map<string, [First, Second]>, decoder: Decoder): Handle; }\n",
            ),
            (
                "registry.tsx",
                "export const request = (registry: Registry, values: Map<string, [First, Second]>, decoder: Decoder): Handle =>;\n",
            ),
            (
                "registry.lua",
                "local M\nfunction M.request(registry_identifier, first_parameter_identifier, second_parameter_identifier)\nreturn M\n",
            ),
        ] {
            let formatted = DeclarationOverview::format_with_width(path, source, 60).unwrap();
            assert!(formatted.contains("(\n"), "{path}: {formatted}");
            DeclarationOverview::parse(path, &formatted).unwrap();
            assert_eq!(
                DeclarationOverview::format_with_width(path, &formatted, 60).unwrap(),
                formatted,
                "{path}"
            );
            let display = DeclarationOverview::present(path, &formatted).unwrap();
            assert!(display.text.contains("(\n"), "{path}: {}", display.text);
        }
    }

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

    #[test]
    fn lua_top_level_literal_imports_survive_declaration_round_trip() {
        let source = "local module = require('plugin.module')\nrequire(\"plugin.setup\")\nlocal label = \"require('fake')\"\n-- require('comment.only')\nfunction run()\n  require('runtime.only')\nend\n";
        let overview = DeclarationOverview::extract("init.lua", source).unwrap();
        assert!(overview.contains("require('plugin.module')"));
        assert!(overview.contains("require(\"plugin.setup\")"));
        assert!(!overview.contains("runtime.only"));
        let formatted = DeclarationOverview::format_with_width("init.lua", &overview, 80).unwrap();
        let index = crate::syntax::DeclarationIndex::extract("init.lua", &formatted).unwrap();
        assert_eq!(index.import.iter().filter_map(|import| import.source.as_deref()).collect::<Vec<_>>(), ["plugin.module", "plugin.setup"]);
    }
}
