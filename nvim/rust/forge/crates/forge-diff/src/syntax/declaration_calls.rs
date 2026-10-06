use std::collections::HashMap;

use tree_sitter::{Node, Parser};

use super::{DeclarationOverview, SyntaxError, SyntaxLanguage};

/// A source call occurrence, retaining lexical order and receiver evidence.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclarationCall {
    /// Qualified callable name, or a receiver expression without type evidence.
    pub name: String,
    /// One-based source line of the call target.
    pub line: u32,
    /// Zero-based source byte column of the call target.
    pub column: u32,
    /// Local binding evidence prevents name-only resolution of an opaque target.
    pub unresolved: bool,
}

/// One callable's ownership, signature position, and direct call occurrences.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclarationCallable {
    /// Lexical callable owner with an ordinal suffix for repeated signatures.
    pub owner: String,
    /// One-based line of the callable name.
    pub line: u32,
    /// Zero-based byte column of the callable name.
    pub column: u32,
    /// One-based final signature line before the body.
    pub end_line: u32,
    /// Direct call occurrences in syntax extraction order.
    pub call: Vec<DeclarationCall>,
    /// Parameter value bindings that shadow same-named imported callables.
    pub binding: std::collections::BTreeSet<String>,
}

/// Extracts callable ownership and call expressions from supported source languages.
pub struct DeclarationCalls;

impl DeclarationCalls {
    /// Protect literal and comment rows when parsing the combined declaration interface.
    pub fn protected_lines(
        path: &str,
        source: &str,
    ) -> Result<std::collections::BTreeSet<usize>, SyntaxError> {
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser.parse(source, None).ok_or(SyntaxError::Cancelled)?;
        let mut protected = std::collections::BTreeSet::new();
        let mut work = vec![(tree.root_node(), 0)];
        while let Some((node, depth)) = work.pop() {
            if depth > 128 {
                return Err(SyntaxError::CaptureLimit);
            }
            if node.kind().contains("string") || node.kind().contains("comment") {
                protected.extend(node.start_position().row..=node.end_position().row);
            } else {
                let mut cursor = node.walk();
                work.extend(
                    node.named_children(&mut cursor)
                        .map(|child| (child, depth + 1)),
                );
            }
        }
        Ok(protected)
    }
    /// Read calls in source order, including closures but separating named functions.
    pub fn extract(
        path: &str,
        source: &str,
        declarations: bool,
    ) -> Result<Vec<DeclarationCallable>, SyntaxError> {
        if super::ConfigurationFormat::for_path(path).is_some() {
            return Ok(Vec::new());
        }
        if source.len() > 1024 * 1024 {
            return Err(SyntaxError::MemoryLimit);
        }
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        let parsed = if declarations {
            super::declaration::declaration_surrogate(language, source)
        } else {
            source.to_owned()
        };
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser.parse(&parsed, None).ok_or(SyntaxError::Cancelled)?;
        if tree.root_node().has_error() {
            return Err(SyntaxError::Query(format!(
                "invalid call extraction syntax in {path}"
            )));
        }
        Self::from_tree(&parsed, language, &tree, declarations)
    }

    pub(super) fn from_tree(
        source: &str,
        language: SyntaxLanguage,
        tree: &tree_sitter::Tree,
        declarations: bool,
    ) -> Result<Vec<DeclarationCallable>, SyntaxError> {
        let mut callable = Vec::new();
        collect(tree.root_node(), source, language, &[], &mut callable, 0)?;
        let mut return_type = HashMap::new();
        collect_returns(tree.root_node(), source, language, &[], &mut return_type, 0)?;
        if !declarations {
            let position = callable
                .iter()
                .enumerate()
                .map(|(index, function)| ((function.line, function.column), index))
                .collect();
            populate(
                tree.root_node(),
                source,
                language,
                &[],
                &return_type,
                &position,
                &mut callable,
                0,
            )?;
        }
        let mut occurrence = HashMap::<String, usize>::new();
        for function in &mut callable {
            let ordinal = occurrence.entry(function.owner.clone()).or_default();
            *ordinal += 1;
            if *ordinal > 1 {
                function.owner.push_str(&format!("#{ordinal}"));
            }
        }
        if callable
            .iter()
            .map(|function| function.call.len())
            .sum::<usize>()
            > 65536
        {
            return Err(SyntaxError::CaptureLimit);
        }
        Ok(callable)
    }
}

fn contents<'source>(node: Node<'_>, source: &'source str) -> &'source str {
    &source[node.byte_range()]
}

fn callable_name(node: Node<'_>, source: &str) -> Option<String> {
    if matches!(
        node.kind(),
        "function_item"
            | "function_signature_item"
            | "function_declaration"
            | "function_signature"
            | "method_definition"
            | "method_signature"
            | "function_definition"
    ) {
        if let Some(name) = node.child_by_field_name("name") {
            return Some(contents(name, source).to_owned());
        }
    }
    if matches!(
        node.kind(),
        "arrow_function" | "function_expression" | "function_definition"
    ) {
        let parent = node.parent()?;
        if matches!(
            parent.kind(),
            "variable_declarator" | "pair" | "field" | "assignment_statement" | "expression_list"
        ) {
            let name = parent
                .child_by_field_name("name")
                .or_else(|| parent.child_by_field_name("key"));
            if let Some(name) = name {
                return Some(contents(name, source).to_owned());
            }
            if parent.kind() == "expression_list" {
                let assignment = parent.parent()?;
                let mut cursor = assignment.walk();
                let variables = assignment
                    .named_children(&mut cursor)
                    .find(|child| child.kind() == "variable_list")?;
                return Some(contents(variables, source).to_owned());
            }
        }
    }
    None
}

fn container(node: Node<'_>, source: &str) -> Option<String> {
    if node.kind() == "table_constructor" {
        let parent = node.parent()?;
        if parent.kind() == "field" {
            return parent
                .child_by_field_name("name")
                .or_else(|| parent.child_by_field_name("key"))
                .map(|name| contents(name, source).trim_matches(['\'', '"']).to_owned());
        }
        if parent.kind() == "expression_list" {
            let assignment = parent.parent()?;
            let mut cursor = assignment.walk();
            return assignment
                .named_children(&mut cursor)
                .find(|child| child.kind() == "variable_list")
                .map(|name| contents(name, source).to_owned());
        }
    }
    if node.kind() == "impl_item" {
        return node
            .child_by_field_name("type")
            .map(|value| base_type(contents(value, source)));
    }
    if matches!(
        node.kind(),
        "mod_item"
            | "trait_item"
            | "class_declaration"
            | "abstract_class_declaration"
            | "interface_declaration"
            | "internal_module"
    ) {
        return node
            .child_by_field_name("name")
            .map(|value| contents(value, source).to_owned());
    }
    None
}

fn owner(scope: &[String], name: &str, language: SyntaxLanguage) -> String {
    let separator = if language == SyntaxLanguage::Rust {
        "::"
    } else {
        "."
    };
    scope
        .iter()
        .map(String::as_str)
        .chain([name])
        .collect::<Vec<_>>()
        .join(separator)
}

fn collect(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    scope: &[String],
    output: &mut Vec<DeclarationCallable>,
    depth: usize,
) -> Result<(), SyntaxError> {
    if depth > 128 || output.len() > 65536 {
        return Err(SyntaxError::CaptureLimit);
    }
    let mut nested = scope.to_vec();
    if let Some(name) = callable_name(node, source) {
        let mut binding = HashMap::new();
        if let Some(parameters) = node.child_by_field_name("parameters") {
            parameter_types(parameters, source, &mut binding);
        }
        let name_node = node.child_by_field_name("name").unwrap_or(node);
        let end = node
            .child_by_field_name("body")
            .map(|body| body.start_position().row)
            .unwrap_or(node.end_position().row);
        output.push(DeclarationCallable {
            owner: owner(scope, &name, language),
            line: name_node.start_position().row as u32 + 1,
            column: name_node.start_position().column as u32,
            end_line: end as u32 + 1,
            call: Vec::new(),
            binding: binding.into_keys().collect(),
        });
        nested.push(name);
    } else if let Some(name) = container(node, source) {
        nested.push(name);
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect(child, source, language, &nested, output, depth + 1)?;
    }
    Ok(())
}

fn collect_returns(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    scope: &[String],
    output: &mut HashMap<String, Option<String>>,
    depth: usize,
) -> Result<(), SyntaxError> {
    if depth > 128 {
        return Err(SyntaxError::CaptureLimit);
    }
    let mut nested = scope.to_vec();
    if let Some(name) = callable_name(node, source) {
        if let Some(value) = node.child_by_field_name("return_type") {
            let value = contents(value, source)
                .trim_start_matches(':')
                .trim()
                .split_inclusive(|character: char| !character.is_alphanumeric() && character != '_')
                .map(|token| {
                    let end = token
                        .find(|character: char| !character.is_alphanumeric() && character != '_')
                        .unwrap_or(token.len());
                    if &token[..end] == "Self" {
                        format!(
                            "{}{}",
                            scope.last().map(String::as_str).unwrap_or("Self"),
                            &token[end..]
                        )
                    } else {
                        token.into()
                    }
                })
                .collect::<String>();
            output
                .entry(owner(scope, &name, language))
                .and_modify(|previous| {
                    if previous.as_ref() != Some(&value) {
                        *previous = None;
                    }
                })
                .or_insert(Some(value.clone()));
            if !scope.is_empty() {
                output
                    .entry(name.clone())
                    .and_modify(|previous| {
                        if previous.as_ref() != Some(&value) {
                            *previous = None;
                        }
                    })
                    .or_insert(Some(value));
            }
        }
        nested.push(name);
    } else if let Some(name) = container(node, source).or_else(|| {
        matches!(node.kind(), "struct_item")
            .then(|| {
                node.child_by_field_name("name")
                    .map(|name| contents(name, source).to_owned())
            })
            .flatten()
    }) {
        nested.push(name);
    }
    if matches!(
        node.kind(),
        "field_declaration" | "public_field_definition" | "property_signature"
    ) {
        if let (Some(name), Some(value), Some(owner)) = (
            node.child_by_field_name("name"),
            node.child_by_field_name("type"),
            scope.last(),
        ) {
            output.insert(
                format!("field:{owner}.{}", contents(name, source)),
                Some(base_type(contents(value, source))),
            );
        }
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect_returns(child, source, language, &nested, output, depth + 1)?;
    }
    Ok(())
}

fn populate(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    scope: &[String],
    returns: &HashMap<String, Option<String>>,
    position: &HashMap<(u32, u32), usize>,
    output: &mut [DeclarationCallable],
    depth: usize,
) -> Result<(), SyntaxError> {
    if depth > 128 {
        return Err(SyntaxError::CaptureLimit);
    }
    let mut nested = scope.to_vec();
    if let Some(name) = callable_name(node, source) {
        let line = node
            .child_by_field_name("name")
            .unwrap_or(node)
            .start_position()
            .row as u32
            + 1;
        let column = node
            .child_by_field_name("name")
            .unwrap_or(node)
            .start_position()
            .column as u32;
        if let Some(item) = position
            .get(&(line, column))
            .and_then(|index| output.get_mut(*index))
        {
            let mut binding = HashMap::new();
            if let Some(container) = receiver_owner(node, source) {
                binding.insert("self".into(), Some(container.clone()));
                if matches!(node.kind(), "method_definition" | "arrow_function") {
                    binding.insert("this".into(), Some(container));
                }
            }
            if let Some(parameters) = node.child_by_field_name("parameters") {
                parameter_types(parameters, source, &mut binding);
            }
            lua_annotations(node, source, &mut binding);
            if let Some(body) = node.child_by_field_name("body") {
                calls(
                    body,
                    source,
                    language,
                    returns,
                    &mut binding,
                    &mut item.call,
                    0,
                )?;
            }
        }
        nested.push(name);
    } else if let Some(name) = container(node, source) {
        nested.push(name);
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        populate(
            child,
            source,
            language,
            &nested,
            returns,
            position,
            output,
            depth + 1,
        )?;
    }
    Ok(())
}

fn receiver_owner(node: Node<'_>, source: &str) -> Option<String> {
    if node.kind() == "function_definition" {
        if let Some((owner, _)) = node
            .child_by_field_name("name")
            .map(|name| contents(name, source))
            .and_then(|name| name.rsplit_once(':'))
        {
            return Some(owner.into());
        }
    }
    let mut parent = node.parent();
    while let Some(node) = parent {
        if matches!(
            node.kind(),
            "impl_item"
                | "trait_item"
                | "class_declaration"
                | "abstract_class_declaration"
                | "interface_declaration"
        ) {
            return container(node, source);
        }
        if matches!(
            node.kind(),
            "function_item"
                | "function_declaration"
                | "function_expression"
                | "function_definition"
        ) {
            return None;
        }
        parent = node.parent();
    }
    None
}

fn parameter_types(node: Node<'_>, source: &str, binding: &mut HashMap<String, Option<String>>) {
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "identifier" {
            binding.insert(contents(child, source).into(), None);
        }
        if let Some(name) = child
            .child_by_field_name("pattern")
            .or_else(|| child.child_by_field_name("name"))
        {
            let name = contents(name, source).trim_start_matches("mut ").to_owned();
            let value = child
                .child_by_field_name("type")
                .map(|value| base_type(contents(value, source)));
            binding.insert(name, value);
        }
    }
}

fn lua_annotations(node: Node<'_>, source: &str, binding: &mut HashMap<String, Option<String>>) {
    let prefix = &source[..node.start_byte()];
    for line in prefix
        .lines()
        .rev()
        .take_while(|line| line.trim().starts_with("--") || line.trim().is_empty())
    {
        if let Some(annotation) = line.trim().strip_prefix("---@param ") {
            let mut fields = annotation.split_whitespace();
            if let (Some(name), Some(value)) = (fields.next(), fields.next()) {
                binding.insert(name.into(), Some(value.into()));
            }
        }
    }
}

fn base_type(value: &str) -> String {
    let value = value
        .trim()
        .trim_start_matches(':')
        .trim()
        .trim_start_matches('&')
        .trim_start_matches('*')
        .trim();
    let value = if value.starts_with('\'') {
        value.split_once(' ').map(|(_, rest)| rest).unwrap_or(value)
    } else {
        value
    };
    value
        .trim_start_matches("mut ")
        .split(['<', '[', '|'])
        .next()
        .unwrap_or_default()
        .trim()
        .to_owned()
}

fn expression_type(
    node: Node<'_>,
    source: &str,
    returns: &HashMap<String, Option<String>>,
    binding: &HashMap<String, Option<String>>,
) -> Option<String> {
    match node.kind() {
        "identifier" | "self" | "this" => binding.get(contents(node, source)).cloned().flatten(),
        "struct_expression" | "new_expression" | "table_constructor" => node
            .child_by_field_name("name")
            .or_else(|| node.child_by_field_name("constructor"))
            .map(|value| base_type(contents(value, source))),
        "call_expression" | "function_call" => {
            let function = node
                .child_by_field_name("function")
                .or_else(|| node.child_by_field_name("name"))?;
            let name = call_name(function, source, returns, binding);
            returns
                .get(&name)
                .cloned()
                .flatten()
                .map(|value| base_type(&value))
        }
        "reference_expression" | "parenthesized_expression" | "await_expression" => {
            let mut cursor = node.walk();
            node.named_children(&mut cursor)
                .next()
                .and_then(|value| expression_type(value, source, returns, binding))
        }
        "try_expression" => {
            let mut cursor = node.walk();
            let value = node.named_children(&mut cursor).next()?;
            let function = value.child_by_field_name("function")?;
            let result = returns
                .get(&call_name(function, source, returns, binding))
                .cloned()
                .flatten()?;
            let inner = result
                .split_once('<')?
                .1
                .split(',')
                .next()?
                .trim_end_matches('>');
            Some(base_type(inner))
        }
        "field_expression" | "member_expression" => {
            let receiver = node
                .child_by_field_name("value")
                .or_else(|| node.child_by_field_name("object"))?;
            let field = node
                .child_by_field_name("field")
                .or_else(|| node.child_by_field_name("property"))?;
            let receiver = expression_type(receiver, source, returns, binding)?;
            returns
                .get(&format!("field:{receiver}.{}", contents(field, source)))
                .cloned()
                .flatten()
        }
        _ => None,
    }
}

fn call_name(
    function: Node<'_>,
    source: &str,
    returns: &HashMap<String, Option<String>>,
    binding: &HashMap<String, Option<String>>,
) -> String {
    if function.kind() == "generic_function" {
        if let Some(function) = function.child_by_field_name("function") {
            return call_name(function, source, returns, binding);
        }
    }
    if matches!(
        function.kind(),
        "field_expression"
            | "member_expression"
            | "method_index_expression"
            | "dot_index_expression"
    ) {
        let receiver = function
            .child_by_field_name("value")
            .or_else(|| function.child_by_field_name("object"))
            .or_else(|| function.child_by_field_name("table"));
        let member = function
            .child_by_field_name("field")
            .or_else(|| function.child_by_field_name("property"))
            .or_else(|| function.child_by_field_name("method"));
        if let (Some(receiver), Some(member)) = (receiver, member) {
            if let Some(value) = expression_type(receiver, source, returns, binding) {
                return format!(
                    "{value}{}{}",
                    if function.kind() == "field_expression" {
                        "::"
                    } else {
                        "."
                    },
                    contents(member, source)
                );
            }
            let receiver = if matches!(
                receiver.kind(),
                "identifier" | "field_expression" | "member_expression" | "dot_index_expression"
            ) {
                contents(receiver, source)
            } else {
                "<unresolved>"
            };
            return format!("{receiver}.{}", contents(member, source));
        }
    }
    let name = contents(function, source)
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    if name
        .chars()
        .all(|character| character.is_alphanumeric() || "_:.<>#".contains(character))
    {
        name
    } else {
        "<unresolved>".into()
    }
}

fn calls(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    returns: &HashMap<String, Option<String>>,
    binding: &mut HashMap<String, Option<String>>,
    output: &mut Vec<DeclarationCall>,
    depth: usize,
) -> Result<(), SyntaxError> {
    if depth > 128 || output.len() > 65536 {
        return Err(SyntaxError::CaptureLimit);
    }
    if callable_name(node, source).is_some() {
        return Ok(());
    }
    let boundary = matches!(
        node.kind(),
        "block"
            | "statement_block"
            | "closure_expression"
            | "arrow_function"
            | "function_expression"
    );
    let previous = boundary.then(|| binding.clone());
    if matches!(
        node.kind(),
        "closure_expression" | "arrow_function" | "function_expression"
    ) {
        if let Some(parameters) = node.child_by_field_name("parameters") {
            parameter_types(parameters, source, binding);
        }
    }
    let pending_binding = if matches!(node.kind(), "let_declaration" | "variable_declarator") {
        if let Some(name) = node
            .child_by_field_name("pattern")
            .or_else(|| node.child_by_field_name("name"))
        {
            let value = node
                .child_by_field_name("type")
                .map(|value| base_type(contents(value, source)))
                .or_else(|| {
                    node.child_by_field_name("value")
                        .and_then(|value| expression_type(value, source, returns, binding))
                });
            Some((
                contents(name, source).trim_start_matches("mut ").to_owned(),
                value,
            ))
        } else {
            None
        }
    } else {
        None
    };
    if matches!(
        node.kind(),
        "call_expression" | "function_call" | "new_expression"
    ) {
        if let Some(function) = node
            .child_by_field_name("function")
            .or_else(|| node.child_by_field_name("name"))
            .or_else(|| node.child_by_field_name("constructor"))
        {
            let name = call_name(function, source, returns, binding);
            if !(language == SyntaxLanguage::Rust && matches!(name.as_str(), "Ok" | "Err" | "Some"))
            {
                output.push(DeclarationCall {
                    name,
                    unresolved: shadowed_target(function, source, returns, binding),
                    line: function.start_position().row as u32 + 1,
                    column: function.start_position().column as u32,
                });
            }
        }
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        calls(child, source, language, returns, binding, output, depth + 1)?;
    }
    if let Some((name, value)) = pending_binding {
        binding.insert(name, value);
    }
    if let Some(previous) = previous {
        *binding = previous;
    }
    Ok(())
}

fn shadowed_target(
    function: Node<'_>,
    source: &str,
    returns: &HashMap<String, Option<String>>,
    binding: &HashMap<String, Option<String>>,
) -> bool {
    if function.kind() == "generic_function" {
        return function
            .child_by_field_name("function")
            .is_none_or(|function| shadowed_target(function, source, returns, binding));
    }
    if function.kind() == "identifier" {
        return binding.contains_key(contents(function, source));
    }
    if matches!(
        function.kind(),
        "field_expression"
            | "member_expression"
            | "method_index_expression"
            | "dot_index_expression"
    ) {
        let receiver = function
            .child_by_field_name("value")
            .or_else(|| function.child_by_field_name("object"))
            .or_else(|| function.child_by_field_name("table"));
        if let Some(mut receiver) = receiver {
            if expression_type(receiver, source, returns, binding).is_some() {
                return false;
            }
            while let Some(parent) = receiver
                .child_by_field_name("value")
                .or_else(|| receiver.child_by_field_name("object"))
                .or_else(|| receiver.child_by_field_name("table"))
            {
                receiver = parent;
            }
            return binding.contains_key(contents(receiver, source));
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::DeclarationCalls;

    #[test]
    fn rust_calls_preserve_occurrences_and_receiver_types() {
        let source = "struct Client; impl Client { fn send(&self) {} } fn run(client: &Client) { client.send(); z(); a(); z(); }";
        let functions = DeclarationCalls::extract("src/lib.rs", source, false).unwrap();
        let run = functions
            .iter()
            .find(|function| function.owner == "run")
            .unwrap();
        assert_eq!(
            run.call
                .iter()
                .map(|call| call.name.as_str())
                .collect::<Vec<_>>(),
            ["Client::send", "z", "a", "z"]
        );
    }

    #[test]
    fn shadowed_callable_names_retain_unresolved_evidence() {
        let source = "fn send() {} fn run(send: fn(), client: Unknown) { send(); client.send(); }";
        let functions = DeclarationCalls::extract("lib.rs", source, false).unwrap();
        let run = functions
            .iter()
            .find(|function| function.owner == "run")
            .unwrap();
        assert_eq!(run.call[0].name, "send");
        assert!(run.call[0].unresolved);
        assert_eq!(run.call[1].name, "Unknown::send");
    }

    #[test]
    fn typescript_and_tsx_calls_keep_named_owners() {
        for path in ["src/main.ts", "src/main.tsx"] {
            let source = "class Client { send() {} } function run(client: Client) { client.send(); done(); } const work = () => { run(new Client()); };";
            let functions = DeclarationCalls::extract(path, source, false).unwrap();
            let run = functions
                .iter()
                .find(|function| function.owner == "run")
                .unwrap();
            assert_eq!(
                run.call
                    .iter()
                    .map(|call| call.name.as_str())
                    .collect::<Vec<_>>(),
                ["Client.send", "done"]
            );
            assert!(functions.iter().any(|function| function.owner == "work"));
        }
    }

    #[test]
    fn lua_calls_use_parameter_annotations() {
        let source = "local M = {}\n---@param client Client\nfunction M.run(client)\n  client:send()\n  done()\nend\nreturn M\n";
        let functions = DeclarationCalls::extract("main.lua", source, false).unwrap();
        let run = functions
            .iter()
            .find(|function| function.owner == "M.run")
            .unwrap();
        assert_eq!(
            run.call
                .iter()
                .map(|call| call.name.as_str())
                .collect::<Vec<_>>(),
            ["Client.send", "done"]
        );
    }

    #[test]
    fn lua_table_functions_retain_their_module_owner() {
        let source = "local M = { run = function() send() end }\nreturn M\n";
        let (declaration, calls) =
            super::super::DeclarationOverview::extract_with_calls("main.lua", source).unwrap();
        let saved = DeclarationCalls::extract("main.lua", &declaration, true).unwrap();
        assert_eq!(calls[0].owner, "M.run");
        assert_eq!(saved[0].owner, calls[0].owner);
        assert_eq!(calls[0].call[0].name, "send");
    }

    #[test]
    fn rust_inference_uses_fields_constructors_and_scoped_bindings() {
        let source = "struct Client; impl Client { fn new() -> Self { Client } fn send(&self) {} } struct Service { client: Client } impl Service { fn run(&self) { self.client.send(); let client = Client::new(); client.send(); { let client = opaque(); client.send(); } client.send(); } }";
        let functions = DeclarationCalls::extract("lib.rs", source, false).unwrap();
        let run = functions
            .iter()
            .find(|function| function.owner == "Service::run")
            .unwrap();
        assert_eq!(
            run.call
                .iter()
                .map(|call| call.name.as_str())
                .collect::<Vec<_>>(),
            [
                "Client::send",
                "Client::new",
                "Client::send",
                "opaque",
                "client.send",
                "Client::send"
            ]
        );
    }
}
