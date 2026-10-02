//! Scope and reference extraction shared by declaration validation and navigation.

use super::{DeclarationOverview, DeclarationPosition, SyntaxError, SyntaxLanguage};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tree_sitter::{Node, Parser};

/// Accessibility attached to a declaration or re-export.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SymbolVisibility {
    Private,
    Crate,
    Parent,
    Public,
}

/// A named declaration with its lexical scope and exact source position.
#[derive(Clone, Debug)]
pub struct DeclarationSymbol {
    pub name: String,
    pub scope: Vec<String>,
    pub position: DeclarationPosition,
    pub visibility: SymbolVisibility,
    pub type_namespace: bool,
    pub value_namespace: bool,
    pub macro_namespace: bool,
    pub conditional: bool,
    pub global: bool,
    pub parameter: bool,
}

/// One identifier path used by a declaration, excluding executable bodies.
#[derive(Clone, Debug)]
pub struct DeclarationReference {
    pub path: Vec<String>,
    pub position: DeclarationPosition,
    pub length: usize,
    pub scope: Vec<String>,
    pub value_namespace: bool,
    pub macro_namespace: bool,
    pub conditional: bool,
}

/// An explicit import or export, retaining aliases and namespace imports.
#[derive(Clone, Debug)]
pub struct DeclarationImport {
    pub scope: Vec<String>,
    pub path: Vec<String>,
    pub source: Option<String>,
    pub alias: Option<String>,
    pub glob: bool,
    pub namespace: bool,
    pub export: bool,
    pub visibility: SymbolVisibility,
    pub position: DeclarationPosition,
    pub conditional: bool,
}

/// A Rust module whose declaration may select a separate source file.
#[derive(Clone, Debug)]
pub struct DeclarationModule {
    pub scope: Vec<String>,
    pub name: String,
    pub path: Option<String>,
    pub inline: bool,
    pub conditional: bool,
}

/// Body-free symbol evidence extracted from one source or declaration document.
#[derive(Clone, Debug, Default)]
pub struct DeclarationIndex {
    pub symbol: Vec<DeclarationSymbol>,
    pub reference: Vec<DeclarationReference>,
    pub import: Vec<DeclarationImport>,
    pub module: Vec<DeclarationModule>,
    pub external_module: bool,
    pub incomplete: bool,
    pub no_std: bool,
    pub no_implicit_prelude: bool,
    pub library_reference: Vec<String>,
    pub path_reference: Vec<String>,
    pub type_reference: Vec<String>,
}

/// Separates parser work from the declaration walk without retaining source contents.
#[derive(Default)]
pub struct DeclarationIndexTiming {
    pub parse: Duration,
    pub extract: Duration,
}

impl DeclarationIndex {
    /// Parse exact source positions without formatting or discarding private declarations.
    pub fn extract(path: &str, source: &str) -> Result<Self, SyntaxError> {
        Self::extract_timed(path, source).map(|(index, _)| index)
    }

    /// Return parser and declaration-walk durations for opt-in navigation profiling.
    pub fn extract_timed(
        path: &str,
        source: &str,
    ) -> Result<(Self, DeclarationIndexTiming), SyntaxError> {
        if source.len() > 8 * 1024 * 1024 {
            return Err(SyntaxError::MemoryLimit);
        }
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        if !matches!(
            language,
            SyntaxLanguage::Rust | SyntaxLanguage::Typescript | SyntaxLanguage::Tsx
        ) {
            return Err(SyntaxError::Language(path.into()));
        }
        let started = Instant::now();
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser.parse(source, None).ok_or(SyntaxError::Cancelled)?;
        let parse = started.elapsed();
        let started = Instant::now();
        let mut index = Self {
            incomplete: tree.root_node().has_error(),
            ..Self::default()
        };
        index.external_module = children(tree.root_node())
            .iter()
            .any(|node| matches!(node.kind(), "import_statement" | "export_statement"));
        index.no_std = source.lines().any(|line| line.trim() == "#![no_std]");
        index.no_implicit_prelude = source
            .lines()
            .any(|line| line.trim() == "#![no_implicit_prelude]");
        if language != SyntaxLanguage::Rust {
            for line in source
                .lines()
                .filter(|line| line.trim_start().starts_with("///"))
            {
                for (attribute, destination) in [
                    ("lib", &mut index.library_reference),
                    ("path", &mut index.path_reference),
                    ("types", &mut index.type_reference),
                ] {
                    if let Some(value) = quoted_attribute(line, attribute) {
                        destination.push(value);
                    }
                }
            }
        }
        walk(
            tree.root_node(),
            source,
            language,
            &[],
            "",
            0,
            false,
            false,
            false,
            &mut index,
            0,
        )?;
        Ok((index, DeclarationIndexTiming { parse, extract: started.elapsed() }))
    }
}

fn children(node: Node<'_>) -> Vec<Node<'_>> {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).collect()
}

fn contents<'source>(node: Node<'_>, source: &'source str) -> &'source str {
    &source[node.byte_range()]
}

fn position(node: Node<'_>) -> DeclarationPosition {
    DeclarationPosition {
        line: node.start_position().row as u32 + 1,
        column: node.start_position().column as u32,
    }
}

fn quoted_attribute(text: &str, name: &str) -> Option<String> {
    let rest = text
        .split_once(name)?
        .1
        .trim_start()
        .strip_prefix('=')?
        .trim_start();
    let quote = rest.chars().next()?;
    if !matches!(quote, '\'' | '"') {
        return None;
    }
    Some(rest[1..].split(quote).next()?.into())
}

fn visibility(node: Node<'_>, source: &str, exported: bool, rust: bool) -> SymbolVisibility {
    if !rust {
        return if exported {
            SymbolVisibility::Public
        } else {
            SymbolVisibility::Private
        };
    }
    let text = children(node)
        .into_iter()
        .find(|child| child.kind() == "visibility_modifier")
        .map(|child| contents(child, source))
        .unwrap_or("");
    match text {
        "pub" => SymbolVisibility::Public,
        "pub(crate)" => SymbolVisibility::Crate,
        "pub(self)" => SymbolVisibility::Private,
        "pub(super)" => SymbolVisibility::Parent,
        "" => SymbolVisibility::Private,
        _ => SymbolVisibility::Crate,
    }
}

fn walk(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    scope: &[String],
    attributes: &str,
    ordinal: usize,
    exported: bool,
    conditional: bool,
    global: bool,
    index: &mut DeclarationIndex,
    depth: usize,
) -> Result<(), SyntaxError> {
    if depth > 128 || index.symbol.len() + index.reference.len() + index.import.len() > 65536 {
        return Err(SyntaxError::CaptureLimit);
    }
    let rust = language == SyntaxLanguage::Rust;
    let kind = node.kind();
    if matches!(
        kind,
        "block"
            | "comment"
            | "line_comment"
            | "block_comment"
            | "attribute_item"
            | "inner_attribute_item"
            | "decorator"
    ) {
        return Ok(());
    }
    if kind == "statement_block"
        && !node.parent().is_some_and(|parent| {
            matches!(
                parent.kind(),
                "internal_module" | "module" | "ambient_declaration"
            )
        })
    {
        return Ok(());
    }
    if kind == "ambient_declaration"
        && contents(node, source)
            .trim_start()
            .starts_with("declare global")
    {
        for child in children(node) {
            walk(
                child,
                source,
                language,
                &[],
                "",
                0,
                false,
                conditional,
                true,
                index,
                depth + 1,
            )?;
        }
        return Ok(());
    }
    if kind == "macro_invocation" {
        index.incomplete = true;
        return Ok(());
    }
    if kind == "use_declaration" {
        if let Some(argument) = node.child_by_field_name("argument") {
            rust_import(
                argument,
                source,
                scope,
                &[],
                visibility(node, source, exported, true),
                conditional,
                index,
            );
        }
        return Ok(());
    }
    if !rust && matches!(kind, "import_statement" | "export_statement") {
        if contents(node, source).trim_start().starts_with("export =")
            || children(node)
                .iter()
                .any(|child| child.kind() == "import_require_clause")
        {
            index.incomplete = true;
        }
        typescript_import(node, source, scope, conditional, index);
        if let Some(declaration) = node.child_by_field_name("declaration") {
            return walk(
                declaration,
                source,
                language,
                scope,
                "",
                0,
                kind == "export_statement",
                conditional,
                global,
                index,
                depth + 1,
            );
        }
        for child in children(node).into_iter().filter(|child| {
            !matches!(
                child.kind(),
                "import_clause" | "export_clause" | "namespace_export" | "string" | "identifier"
            )
        }) {
            walk(
                child,
                source,
                language,
                scope,
                "",
                0,
                kind == "export_statement",
                conditional,
                global,
                index,
                depth + 1,
            )?;
        }
        return Ok(());
    }
    if kind == "extern_crate_declaration" {
        if let Some(name) = node.child_by_field_name("name") {
            index.import.push(DeclarationImport {
                scope: scope.to_vec(),
                path: vec![contents(name, source).into()],
                alias: node
                    .child_by_field_name("alias")
                    .map(|alias| contents(alias, source).into())
                    .or_else(|| Some(contents(name, source).into())),
                source: None,
                glob: false,
                namespace: true,
                export: false,
                visibility: visibility(node, source, false, true),
                position: position(name),
                conditional,
            });
        }
        return Ok(());
    }
    let declaration = matches!(
        kind,
        "struct_item"
            | "enum_item"
            | "trait_item"
            | "type_item"
            | "function_item"
            | "function_signature_item"
            | "mod_item"
            | "const_item"
            | "static_item"
            | "class_declaration"
            | "abstract_class_declaration"
            | "interface_declaration"
            | "type_alias_declaration"
            | "enum_declaration"
            | "function_declaration"
            | "function_signature"
            | "internal_module"
            | "module"
            | "method_signature"
            | "method_definition"
            | "field_declaration"
            | "public_field_definition"
            | "property_signature"
            | "associated_type"
            | "type_parameter"
            | "constrained_type_parameter"
            | "optional_type_parameter"
            | "variable_declarator"
            | "mapped_type_clause"
    );
    let mut nested = scope.to_vec();
    let mut name_node = None;
    if declaration {
        name_node = node.child_by_field_name("name");
        if let Some(name) = name_node {
            let name_text = contents(name, source).trim_matches(['\'', '"']).to_owned();
            if name_text == "global" && !rust {
                nested.clear();
            } else {
                let parameter = matches!(
                    kind,
                    "type_parameter"
                        | "constrained_type_parameter"
                        | "optional_type_parameter"
                        | "mapped_type_clause"
                );
                let type_namespace = matches!(
                    kind,
                    "struct_item"
                        | "enum_item"
                        | "trait_item"
                        | "type_item"
                        | "mod_item"
                        | "class_declaration"
                        | "abstract_class_declaration"
                        | "interface_declaration"
                        | "type_alias_declaration"
                        | "enum_declaration"
                        | "internal_module"
                        | "associated_type"
                ) || parameter;
                index.symbol.push(DeclarationSymbol {
                    name: name_text.clone(),
                    scope: scope.to_vec(),
                    position: position(name),
                    visibility: visibility(node, source, exported, rust),
                    type_namespace,
                    macro_namespace: kind == "mod_item",
                    value_namespace: !matches!(
                        kind,
                        "interface_declaration"
                            | "type_alias_declaration"
                            | "type_item"
                            | "trait_item"
                    ) && !parameter,
                    conditional,
                    global: global || !rust && !index.external_module && scope.is_empty(),
                    parameter,
                });
                if matches!(
                    kind,
                    "struct_item"
                        | "enum_item"
                        | "trait_item"
                        | "function_item"
                        | "function_signature_item"
                        | "mod_item"
                        | "class_declaration"
                        | "abstract_class_declaration"
                        | "interface_declaration"
                        | "function_declaration"
                        | "function_signature"
                        | "internal_module"
                        | "type_alias_declaration"
                        | "type_item"
                        | "method_signature"
                        | "method_definition"
                        | "property_signature"
                        | "field_declaration"
                        | "public_field_definition"
                        | "variable_declarator"
                ) {
                    nested.push(name_text);
                }
            }
        }
    }
    if matches!(
        kind,
        "function_type" | "constructor_type" | "conditional_type" | "index_signature"
    ) {
        nested.push(synthetic_scope(ordinal, node.kind()));
    }
    if kind == "infer_type" {
        if let Some(name) = children(node)
            .into_iter()
            .find(|child| child.kind() == "type_identifier")
        {
            index.symbol.push(DeclarationSymbol {
                name: contents(name, source).into(),
                scope: scope.to_vec(),
                position: position(name),
                visibility: SymbolVisibility::Private,
                type_namespace: true,
                macro_namespace: false,
                value_namespace: false,
                conditional,
                global: false,
                parameter: true,
            });
            return Ok(());
        }
    }
    if kind == "type_query" {
        if let Some(target) = children(node)
            .first()
            .filter(|target| matches!(target.kind(), "identifier" | "member_expression"))
        {
            let text = contents(*target, source);
            index.reference.push(DeclarationReference {
                path: text.split('.').map(str::to_owned).collect(),
                position: position(*target),
                length: text.len(),
                scope: scope.to_vec(),
                macro_namespace: false,
                value_namespace: true,
                conditional,
            });
        } else {
            index.incomplete = true;
        }
        return Ok(());
    }
    if kind == "mod_item" {
        if let Some(name) = name_node {
            index.module.push(DeclarationModule {
                scope: scope.to_vec(),
                name: contents(name, source).into(),
                path: quoted_attribute(attributes, "path"),
                inline: node.child_by_field_name("body").is_some(),
                conditional,
            });
        }
    }
    if kind == "impl_item" {
        if let Some(owner) = node.child_by_field_name("type") {
            let owner_text = contents(owner, source)
                .split('<')
                .next()
                .unwrap_or("")
                .trim()
                .to_owned();
            nested.push(owner_text);
            nested.push(synthetic_scope(ordinal, "impl"));
            index.symbol.push(DeclarationSymbol {
                name: "Self".into(),
                scope: nested.clone(),
                position: position(owner),
                visibility: SymbolVisibility::Private,
                type_namespace: true,
                macro_namespace: false,
                value_namespace: false,
                conditional,
                global: false,
                parameter: true,
            });
        }
    }
    if kind == "trait_item" {
        index.symbol.push(DeclarationSymbol {
            name: "Self".into(),
            scope: nested.clone(),
            position: position(node),
            visibility: SymbolVisibility::Private,
            type_namespace: true,
            macro_namespace: false,
            value_namespace: false,
            conditional,
            global: false,
            parameter: true,
        });
    }
    if matches!(
        kind,
        "scoped_type_identifier"
            | "nested_type_identifier"
            | "scoped_identifier"
            | "primitive_type"
            | "predefined_type"
    ) || kind == "type_identifier"
        || kind == "identifier"
            && node
                .parent()
                .is_some_and(|parent| parent.kind() == "extends_clause")
    {
        let text = contents(node, source);
        let path = text
            .split(if rust { "::" } else { "." })
            .map(|part| part.trim().to_owned())
            .collect::<Vec<_>>();
        index.reference.push(DeclarationReference {
            path,
            position: position(node),
            length: text.len(),
            scope: scope.to_vec(),
            macro_namespace: false,
            value_namespace: kind == "scoped_identifier" || kind == "identifier",
            conditional,
        });
        return Ok(());
    }
    let mut preceding = String::new();
    let mut preceding_conditional = false;
    let mut sibling_count = HashMap::new();
    let mut child_cursor = node.walk();
    for child in node.named_children(&mut child_cursor) {
        let count = sibling_count.entry(child.kind()).or_insert(0);
        let child_ordinal = *count;
        *count += 1;
        match child.kind() {
            "attribute_item" if rust => {
                rust_attribute(child, source, &nested, conditional, index);
                preceding.push_str(contents(child, source));
                preceding_conditional |= gates_declaration(child, source);
                continue;
            }
            "line_comment" | "block_comment" | "comment" => continue,
            _ => {}
        }
        let attributes = std::mem::take(&mut preceding);
        let attribute_conditional = std::mem::take(&mut preceding_conditional);
        if name_node.is_some_and(|name| name.id() == child.id()) {
            continue;
        }
        if matches!(
            kind,
            "variable_declarator"
                | "const_item"
                | "static_item"
                | "public_field_definition"
                | "enum_variant"
        ) && node
            .child_by_field_name("value")
            .is_some_and(|value| value.id() == child.id())
        {
            continue;
        }
        if kind == "variable_declarator"
            && node
                .child_by_field_name("name")
                .is_some_and(|name| name.id() == child.id())
        {
            continue;
        }
        let child_global =
            global || !rust && name_node.is_some_and(|name| contents(name, source) == "global");
        let child_exported = if !rust && matches!(kind, "internal_module" | "module") {
            node.parent()
                .is_some_and(|parent| parent.kind() == "ambient_declaration")
        } else {
            exported
        };
        let child_scope = if kind == "conditional_type"
            && node
                .child_by_field_name("alternative")
                .is_some_and(|alternative| alternative.id() == child.id())
        {
            scope
        } else {
            &nested
        };
        walk(
            child,
            source,
            language,
            child_scope,
            &attributes,
            child_ordinal,
            child_exported,
            conditional || attribute_conditional,
            child_global,
            index,
            depth + 1,
        )?;
    }
    Ok(())
}

// Derive names use Rust's macro namespace, independently of same-named traits.
fn rust_attribute(node: Node<'_>, source: &str, scope: &[String], conditional: bool, index: &mut DeclarationIndex) {
    let Some(attribute) = children(node).into_iter().find(|child| child.kind() == "attribute") else { return; };
    let parts = children(attribute);
    let Some(name) = parts.first() else { return; };
    let Some(arguments) = parts.iter().find(|child| child.kind() == "token_tree") else { return; };
    match contents(*name, source) {
        "derive" => {
            let mut path = Vec::new();
            let mut first = None;
            let mut cursor = arguments.walk();
            for token in arguments.children(&mut cursor) {
                if token.kind() == "identifier" {
                    first.get_or_insert(token);
                    path.push(contents(token, source).to_owned());
                } else if matches!(token.kind(), "," | ")") {
                    if let Some(start) = first.take() {
                        index.reference.push(DeclarationReference {
                            path: std::mem::take(&mut path), position: position(start),
                            length: token.start_byte() - start.start_byte(), scope: scope.to_vec(),
                            value_namespace: false, macro_namespace: true, conditional,
                        });
                    }
                }
            }
        }
        "proc_macro_derive" => {
            if let Some(name) = children(*arguments).into_iter().find(|child| child.kind() == "identifier") {
                index.symbol.push(DeclarationSymbol {
                    name: contents(name, source).into(), scope: scope.to_vec(), position: position(name),
                    visibility: SymbolVisibility::Public, type_namespace: false, value_namespace: false,
                    macro_namespace: true, conditional, global: false, parameter: false,
                });
            }
        }
        _ => {}
    }
}

// Only cfg gates the item. cfg_attr may add derives without removing its declaration.
fn gates_declaration(node: Node<'_>, source: &str) -> bool {
    node.kind() == "identifier" && contents(node, source) == "cfg"
        || children(node).into_iter().any(|child| gates_declaration(child, source))
}

fn synthetic_scope(ordinal: usize, role: &str) -> String {
    format!("@{role}:{ordinal}")
}

fn rust_import(
    node: Node<'_>,
    source: &str,
    scope: &[String],
    prefix: &[String],
    visibility: SymbolVisibility,
    conditional: bool,
    index: &mut DeclarationIndex,
) {
    match node.kind() {
        "use_list" => {
            for child in children(node) {
                rust_import(child, source, scope, prefix, visibility, conditional, index);
            }
        }
        "scoped_use_list" => {
            let mut path = prefix.to_vec();
            if let Some(root) = node.child_by_field_name("path") {
                path.extend(
                    contents(root, source)
                        .split("::")
                        .filter(|part| !part.is_empty())
                        .map(str::to_owned),
                );
            }
            if let Some(list) = node.child_by_field_name("list") {
                rust_import(list, source, scope, &path, visibility, conditional, index);
            }
        }
        _ => {
            let argument = if matches!(node.kind(), "use_as_clause" | "use_wildcard") {
                node.child_by_field_name("path").unwrap_or(node)
            } else {
                node
            };
            let text = contents(argument, source);
            let mut path = prefix.to_vec();
            path.extend(
                text.split("::")
                    .filter(|part| !part.is_empty())
                    .map(str::to_owned),
            );
            let glob = node.kind() == "use_wildcard" || path.last().is_some_and(|part| part == "*");
            if glob && path.last().is_some_and(|part| part == "*") {
                path.pop();
            }
            if path.last().is_some_and(|part| part == "self") && path.len() > 1 {
                path.pop();
            }
            let alias = node
                .child_by_field_name("alias")
                .map(|alias| contents(alias, source).into())
                .or_else(|| if glob { None } else { path.last().cloned() });
            index.import.push(DeclarationImport {
                scope: scope.to_vec(),
                path,
                source: None,
                alias,
                glob,
                namespace: false,
                export: visibility != SymbolVisibility::Private,
                visibility,
                position: position(node),
                conditional,
            });
        }
    }
}

fn typescript_import(
    node: Node<'_>,
    source: &str,
    scope: &[String],
    conditional: bool,
    index: &mut DeclarationIndex,
) {
    let origin = node.child_by_field_name("source").map(|origin| {
        contents(origin, source)
            .trim_matches(['\'', '"'])
            .to_owned()
    });
    let export = node.kind() == "export_statement";
    let mut imports = Vec::new();
    fn gather(
        node: Node<'_>,
        source: &str,
        imports: &mut Vec<(Vec<String>, Option<String>, bool, bool, DeclarationPosition)>,
    ) {
        match node.kind() {
            "import_specifier" | "export_specifier" => {
                if let Some(name) = node.child_by_field_name("name") {
                    let original = contents(name, source).to_owned();
                    let alias = node
                        .child_by_field_name("alias")
                        .map(|alias| contents(alias, source).into())
                        .unwrap_or_else(|| original.clone());
                    imports.push((vec![original], Some(alias), false, false, position(name)));
                }
            }
            "namespace_import" | "namespace_export" => {
                let alias = children(node)
                    .into_iter()
                    .find(|child| child.kind() == "identifier")
                    .map(|child| contents(child, source).into());
                imports.push((Vec::new(), alias, false, true, position(node)));
            }
            "import_clause" => {
                for child in children(node) {
                    if child.kind() == "identifier" {
                        imports.push((
                            vec!["default".into()],
                            Some(contents(child, source).into()),
                            false,
                            false,
                            position(child),
                        ));
                    } else {
                        gather(child, source, imports);
                    }
                }
            }
            "import_statement" | "export_statement" | "named_imports" | "export_clause" => {
                for child in children(node) {
                    gather(child, source, imports);
                }
            }
            _ => {}
        }
    }
    gather(node, source, &mut imports);
    if !export && origin.is_some() && imports.is_empty() {
        imports.push((Vec::new(), None, false, false, position(node)));
    }
    if export && origin.is_some() && imports.is_empty() {
        imports.push((Vec::new(), None, true, false, position(node)));
    }
    if export
        && contents(node, source)
            .trim_start()
            .starts_with("export default")
    {
        if let Some(declaration) = node.child_by_field_name("declaration") {
            if let Some(name) = declaration.child_by_field_name("name") {
                imports.push((
                    vec![contents(name, source).into()],
                    Some("default".into()),
                    false,
                    false,
                    position(name),
                ));
            }
        } else if let Some(value) = node
            .child_by_field_name("value")
            .filter(|value| value.kind() == "identifier")
        {
            imports.push((
                vec![contents(value, source).into()],
                Some("default".into()),
                false,
                false,
                position(value),
            ));
        }
    }
    for (path, alias, glob, namespace, location) in imports {
        index.import.push(DeclarationImport {
            scope: scope.to_vec(),
            path,
            source: origin.clone(),
            alias,
            glob,
            namespace,
            export,
            visibility: if export {
                SymbolVisibility::Public
            } else {
                SymbolVisibility::Private
            },
            position: location,
            conditional,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn documentation_runs_preserve_attributes_and_distinct_impl_scopes() {
        let mut source = "//! Extensive library documentation.\n".repeat(1000);
        source.push_str("#[cfg(feature = \"optional\")]\n/// Conditional model.\n#[path = \"other.rs\"]\npub mod optional;\n");
        source.push_str("pub struct Store;\nimpl<T> Store { pub fn first(value: T); }\n");
        source.push_str("impl<U> Store { pub fn second(value: U); }\n");
        let index = DeclarationIndex::extract("lib.rs", &source).unwrap();
        let module = index.module.iter().find(|module| module.name == "optional").unwrap();
        assert_eq!(module.path.as_deref(), Some("other.rs"));
        assert!(module.conditional);
        assert!(!index.symbol.iter().find(|symbol| symbol.name == "Store").unwrap().conditional);
        let first = index.symbol.iter().find(|symbol| symbol.name == "first").unwrap();
        let second = index.symbol.iter().find(|symbol| symbol.name == "second").unwrap();
        assert_ne!(first.scope, second.scope);
        for (method, parameter) in [(first, "T"), (second, "U")] {
            assert!(index.symbol.iter().any(|symbol| symbol.parameter && symbol.name == parameter
                && symbol.scope == method.scope));
        }
    }

    #[test]
    fn optional_derives_do_not_gate_declarations() {
        let index = DeclarationIndex::extract("lib.rs", "#[cfg_attr(feature = \"serialize\", derive(Serialize))]\n#[doc = \"cfg is mentioned here\"]\npub struct Transform;\n#[cfg_attr(feature = \"optional\", cfg(feature = \"enabled\"))]\npub struct Gated;\n").unwrap();
        assert!(!index.symbol.iter().find(|symbol| symbol.name == "Transform").unwrap().conditional);
        assert!(index.symbol.iter().find(|symbol| symbol.name == "Gated").unwrap().conditional);
    }

    #[test]
    fn rust_retains_scope_aliases_and_signature_references() {
        let index = DeclarationIndex::extract("lib.rs", "use crate::model::{Item as Entry, nested::*};\npub struct Store<T> { pub item: Option<T> }\nimpl<T> Store<T> { pub fn read(&self) -> Entry; }\n").unwrap();
        assert!(!index.incomplete);
        assert!(
            index
                .import
                .iter()
                .any(|import| import.alias.as_deref() == Some("Entry")
                    && import.path == ["crate", "model", "Item"])
        );
        assert!(
            index
                .import
                .iter()
                .any(|import| import.glob && import.path == ["crate", "model", "nested"])
        );
        assert!(
            index
                .symbol
                .iter()
                .any(|symbol| symbol.parameter && symbol.name == "T")
        );
        assert!(
            index
                .reference
                .iter()
                .any(|reference| reference.path == ["Option"])
        );
        assert!(
            index
                .reference
                .iter()
                .any(|reference| reference.path == ["Entry"])
        );
    }

    #[test]
    fn typescript_distinguishes_globals_exports_and_generics() {
        let index = DeclarationIndex::extract("api.ts", "import type { User as Person } from './model';\nexport interface Api<T> { load(): Promise<T>; user: Person; }\ndeclare global { interface ProjectGlobal {} }\nexport { User as PublicUser } from './model';\n").unwrap();
        assert!(!index.incomplete);
        assert!(
            index
                .import
                .iter()
                .any(|import| import.alias.as_deref() == Some("Person") && !import.export)
        );
        assert!(
            index
                .import
                .iter()
                .any(|import| import.alias.as_deref() == Some("PublicUser") && import.export)
        );
        assert!(index.symbol.iter().any(|symbol| symbol.name == "Api" && symbol.visibility == SymbolVisibility::Public));
        assert!(
            index
                .symbol
                .iter()
                .any(|symbol| symbol.name == "ProjectGlobal" && symbol.global)
        );
        assert!(
            index
                .reference
                .iter()
                .any(|reference| reference.path == ["Promise"])
        );
    }
}
