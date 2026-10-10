use std::collections::BTreeMap;

use tree_sitter::{Node, Parser};

use super::{DeclarationOverview, SyntaxError, SyntaxLanguage};

/// One body-free declaration, independent of source layout and comments.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContractDeclaration {
    pub identity: String,
    pub signature: String,
    pub text: String,
    pub exposed: bool,
    pub position: Option<(u32, u32)>,
}

/// Structural declarations used by plan conformance and implementation reports.
#[derive(Clone, Debug, Default)]
pub struct DeclarationContract {
    pub declaration: BTreeMap<String, Vec<ContractDeclaration>>,
}

/// A required contract difference with readable, body-free source on both sides.
pub struct ContractDifference {
    pub identity: String,
    pub message: String,
    pub expected: String,
    pub observed: String,
}

impl DeclarationContract {
    /// Parse an existing declaration overview without inspecting function bodies.
    pub fn parse(path: &str, overview: &str) -> Result<Self, SyntaxError> {
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        let source = super::declaration::declaration_surrogate(language, overview);
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser.parse(&source, None).ok_or(SyntaxError::Cancelled)?;
        if tree.root_node().has_error() {
            return Err(SyntaxError::Query(format!(
                "invalid declaration contract in {path}"
            )));
        }
        let mut contract = Self::default();
        collect(
            tree.root_node(),
            &source,
            language,
            "",
            true,
            false,
            &mut contract,
        );
        Ok(contract)
    }

    /// Restrict syntactically public declarations to names exported by the module graph.
    pub fn retain_exposed(&mut self, positions: &std::collections::BTreeSet<(u32, u32)>) {
        for declaration in self.declaration.values_mut().flatten() {
            if let Some(position) = declaration.position {
                declaration.exposed &= positions.contains(&position);
            }
        }
    }

    /// Compare required declarations while allowing additional internal declarations.
    pub fn differences(&self, actual: &Self) -> Vec<String> {
        self.changes(actual).into_iter().map(|change| change.message).collect()
    }

    /// Compare contracts once, retaining source for declaration diff presentation.
    pub fn changes(&self, actual: &Self) -> Vec<ContractDifference> {
        let mut differences = Vec::new();
        for (identity, required) in &self.declaration {
            let observed = actual
                .declaration
                .get(identity)
                .map(Vec::as_slice)
                .unwrap_or_default();
            let mut remaining = observed.iter().collect::<Vec<_>>();
            for expected in required {
                if let Some(position) = remaining
                    .iter()
                    .position(|candidate| candidate.signature == expected.signature)
                {
                    remaining.remove(position);
                } else {
                    differences.push(ContractDifference {
                        identity: identity.clone(),
                        expected: expected.text.clone(),
                        observed: observed.iter().map(|item| item.text.as_str()).collect::<Vec<_>>().join("\n"),
                        message: format!(
                        "{identity}: expected {}; observed {}",
                        expected.signature,
                        observed
                            .iter()
                            .map(|item| item.signature.as_str())
                            .collect::<Vec<_>>()
                            .join(" | ")
                    )});
                }
            }
        }
        for (identity, observed) in &actual.declaration {
            let mut remaining = self
                .declaration
                .get(identity)
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();
            for declaration in observed {
                if let Some(position) = remaining
                    .iter()
                    .position(|expected| expected.signature == declaration.signature)
                {
                    remaining.remove(position);
                } else if declaration.exposed
                    && (!self.declaration.contains_key(identity)
                        || observed.len() > self.declaration[identity].len())
                {
                    differences.push(ContractDifference {
                        identity: identity.clone(),
                        expected: String::new(),
                        observed: declaration.text.clone(),
                        message: format!(
                        "{identity}: additional exposed declaration {}",
                        declaration.signature
                    )});
                }
            }
        }
        differences
    }
}

fn collect(
    node: Node<'_>,
    source: &str,
    language: SyntaxLanguage,
    scope: &str,
    reachable: bool,
    inherited_public: bool,
    contract: &mut DeclarationContract,
) {
    let mut cursor = node.walk();
    let mut attributes = String::new();
    let mut attribute_source = String::new();
    for child in node.named_children(&mut cursor) {
        let kind = child.kind();
        if kind.contains("comment") {
            continue;
        }
        if matches!(kind, "attribute_item" | "inner_attribute_item") {
            attributes.push_str(&tokens(child, source, None));
            attribute_source.push_str(&source[child.byte_range()]);
            attribute_source.push('\n');
            continue;
        }
        if kind == "import_statement" || kind == "extern_crate_declaration" {
            attributes.clear();
            attribute_source.clear();
            continue;
        }
        if kind == "export_statement" && child.child_by_field_name("declaration").is_some() {
            collect(child, source, language, scope, reachable, true, contract);
            continue;
        }
        let text = &source[child.byte_range()];
        let visibility = child.child_by_field_name("visibility").or_else(|| {
            child
                .named_children(&mut child.walk())
                .find(|item| item.kind() == "visibility_modifier")
        });
        let explicit_public =
            visibility.is_some_and(|item| source[item.byte_range()].trim() == "pub");
        let restricted = child.named_children(&mut child.walk()).any(|part| {
            part.kind() == "accessibility_modifier"
                && matches!(source[part.byte_range()].trim(), "private" | "protected")
        });
        let public = reachable
            && !restricted
            && (explicit_public
                || inherited_public
                || (language == SyntaxLanguage::Lua && !text.trim_start().starts_with("local ")));
        if kind == "use_declaration" && !public {
            attributes.clear();
            attribute_source.clear();
            continue;
        }
        let container = matches!(
            kind,
            "impl_item"
                | "trait_item"
                | "mod_item"
                | "class_declaration"
                | "abstract_class_declaration"
                | "interface_declaration"
                | "internal_module"
                | "foreign_mod_item"
        );
        let position = child.child_by_field_name("name").map(|name| {
            let position = name.start_position();
            (position.row as u32 + 1, position.column as u32)
        });
        let name = child
            .child_by_field_name("name")
            .map(|name| source[name.byte_range()].to_owned());
        let recognized = name.is_some()
            || matches!(
                kind,
                "use_declaration"
                    | "export_statement"
                    | "impl_item"
                    | "foreign_mod_item"
                    | "lexical_declaration"
                    | "variable_declaration"
                    | "assignment_statement"
                    | "return_statement"
            );
        if !recognized {
            attributes.clear();
            attribute_source.clear();
            continue;
        }
        let body = if container {
            child.child_by_field_name("body")
        } else {
            None
        };
        let signature = format!(
            "{attributes}{}",
            tokens(child, source, body.map(|body| body.id()))
        );
        let display_body = child.child_by_field_name("body");
        let display = if let Some(body) = display_body.filter(|_| container) {
            format!("{} {{}}", source[child.start_byte()..body.start_byte()].trim_end())
        } else if let Some(body) = display_body.filter(|_| matches!(kind,
            "function_item" | "function_declaration" | "method_definition")) {
            format!("{};", source[child.start_byte()..body.start_byte()].trim_end())
        } else {
            text.to_owned()
        };
        let role = match kind {
            "function_item"
            | "function_signature_item"
            | "function_signature"
            | "function_declaration" => "function",
            _ => kind,
        };
        let name = name.unwrap_or_else(|| signature.clone());
        let identity = format!("{scope}{role} {name}");
        let exposed = if kind == "impl_item" {
            reachable && child.child_by_field_name("trait").is_some()
        } else {
            public
        };
        contract
            .declaration
            .entry(identity.clone())
            .or_default()
            .push(ContractDeclaration {
                identity: identity.clone(),
                signature,
                text: format!("{attribute_source}{display}"),
                exposed,
                position,
            });
        if let Some(body) = body {
            let member_scope = format!("{identity}::");
            let member_reachable = if kind == "impl_item" {
                reachable
            } else {
                public
            };
            let implicit_public = matches!(
                kind,
                "trait_item"
                    | "interface_declaration"
                    | "foreign_mod_item"
                    | "class_declaration"
                    | "abstract_class_declaration"
            );
            collect(
                body,
                source,
                language,
                &member_scope,
                member_reachable,
                implicit_public,
                contract,
            );
        }
        attributes.clear();
        attribute_source.clear();
    }
}

fn tokens(node: Node<'_>, source: &str, omitted: Option<usize>) -> String {
    if Some(node.id()) == omitted || node.kind().contains("comment") {
        return String::new();
    }
    if node.child_count() == 0 {
        return if matches!(node.kind(), "," | ";") {
            String::new()
        } else {
            format!("{} ", &source[node.byte_range()])
        };
    }
    let mut output = String::new();
    // Named grammar nodes retain distinctions such as tuple versus parenthesized types.
    if node.is_named() {
        output.push_str(node.kind());
        output.push('(');
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        output.push_str(&tokens(child, source, omitted));
    }
    if node.is_named() {
        output.push(')');
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    fn contract(source: &str) -> DeclarationContract {
        let overview = DeclarationOverview::extract("lib.rs", source).unwrap();
        DeclarationContract::parse("lib.rs", &overview).unwrap()
    }

    #[test]
    fn layout_bodies_and_internal_helpers_do_not_change_required_contracts() {
        let expected = contract(
            "pub struct State { pub value: i32 } pub fn run(value: &mut State) { old(); }",
        );
        let actual = contract(
            "// comment\npub fn run(\nvalue: &mut State,\n) { new(); new(); }\npub struct State { pub value: i32, }\nfn helper() {} pub(crate) fn shared() {} mod internal { pub fn hidden() {} }",
        );
        assert!(
            expected.differences(&actual).is_empty(),
            "{:?}",
            expected.differences(&actual)
        );
    }

    #[test]
    fn all_contract_changes_and_new_exports_are_reported() {
        let expected = contract("pub struct State { pub value: i32 } pub fn run(value: i32) {}");
        let actual = contract(
            "pub struct State { pub value: bool } pub fn run(value: bool) {} pub fn added() {}",
        );
        assert_eq!(expected.differences(&actual).len(), 3);
    }

    #[test]
    fn methods_keep_their_owner_and_trait_contracts() {
        let expected = contract(
            "pub struct State; impl State { pub fn run(&self) {} } pub trait Work { fn go(&self); }",
        );
        let actual = contract(
            "pub struct State; impl State { fn helper() {} pub fn run(&self) {} } pub trait Work { fn go(&self); }",
        );
        assert!(expected.differences(&actual).is_empty());
        assert!(!expected.differences(&contract("pub struct State; impl State { pub fn run(&mut self) {} } pub trait Work { fn go(&self); }")).is_empty());
    }
    #[test]
    fn typescript_public_members_and_lua_local_helpers_keep_their_boundaries() {
        let parse = |path, source| {
            let overview = DeclarationOverview::extract(path, source).unwrap();
            DeclarationContract::parse(path, &overview).unwrap()
        };
        let expected = parse("api.ts", "export class Client { run(): void {} }");
        let helpers = parse(
            "api.ts",
            "export class Client { run(): void {} private helper(): void {} }",
        );
        assert!(expected.differences(&helpers).is_empty());
        let exposed = parse(
            "api.ts",
            "export class Client { run(): void {} added(): void {} }",
        );
        assert!(!expected.differences(&exposed).is_empty());
        let expected = parse("api.lua", "function run() end");
        let helpers = parse(
            "api.lua",
            "local function helper() end\nfunction run() helper() end",
        );
        assert!(expected.differences(&helpers).is_empty());
    }
}
