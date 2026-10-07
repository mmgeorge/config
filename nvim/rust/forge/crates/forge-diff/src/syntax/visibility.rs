use std::collections::HashMap;

use tree_sitter::{Node, Parser};

use super::declaration::{DeclarationOverview, declaration_surrogate};
use crate::syntax::{ConfigurationFormat, SyntaxError, SyntaxLanguage};

/// Classifies formatted declaration rows without changing saved declaration coordinates.
pub struct DeclarationVisibility {
    /// Row admission flags in the original formatted declaration document.
    pub rows: Vec<bool>,
    /// Display replacements for containers whose members are all hidden.
    pub replacement: HashMap<usize, String>,
}

impl DeclarationVisibility {
    /// Select inspection rows and abbreviate trait implementations without changing stored declarations.
    ///
    /// Public-only inspection includes public declarations and their attached documentation.
    ///
    /// Rust includes explicit public visibility, including crate and parent scopes.
    /// Trait members and enum variants inherit
    /// their owner's visibility. TypeScript exports expose members except private and
    /// protected members. Lua exposes nonlocal bindings and the returned table.
    pub fn analyze(path: &str, text: &str, public_only: bool) -> Result<Self, SyntaxError> {
        if ConfigurationFormat::for_path(path).is_some() {
            DeclarationOverview::parse(path, text)?;
            return Ok(Self { rows: vec![true; text.lines().count()], replacement: HashMap::new() });
        }
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        let surrogate = declaration_surrogate(language, text);
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let tree = parser
            .parse(&surrogate, None)
            .ok_or_else(|| SyntaxError::Query("visibility parsing was cancelled".into()))?;
        if tree.root_node().has_error() {
            return Err(SyntaxError::Query(
                "invalid declaration visibility syntax".into(),
            ));
        }
        let mut visibility = Visibility {
            language,
            text: &surrogate,
            rows: vec![!public_only; text.lines().count()],
            types: HashMap::new(),
            replacement: HashMap::new(),
            lines: text.lines().collect(),
            returned: text
                .lines()
                .find_map(|line| line.trim().strip_prefix("return ").map(str::to_owned)),
        };
        if public_only {
            visibility.collect_types(tree.root_node());
            visibility.group(tree.root_node(), Scope::Top);
        }
        let lines: Vec<_> = text.lines().collect();
        let mut preceding = false;
        let mut before = Vec::with_capacity(lines.len());
        for (row, line) in lines.iter().enumerate() {
            before.push(preceding);
            if !line.trim().is_empty() {
                preceding |= visibility.rows[row];
            }
        }
        let mut following = false;
        for (row, line) in lines.iter().enumerate().rev() {
            if line.trim().is_empty() {
                visibility.rows[row] |= before[row] && following;
            } else {
                following |= visibility.rows[row];
            }
        }
        visibility.elide_trait_implementation(tree.root_node());
        Ok(Self {
            rows: visibility.rows,
            replacement: visibility.replacement,
        })
    }
}

#[derive(Clone, Copy)]
enum Scope {
    Top,
    Explicit,
    Inherited,
    Members,
    Tuple,
}

struct Visibility<'text> {
    language: SyntaxLanguage,
    text: &'text str,
    rows: Vec<bool>,
    types: HashMap<String, bool>,
    returned: Option<String>,
    replacement: HashMap<usize, String>,
    lines: Vec<&'text str>,
}

impl Visibility<'_> {
    fn elide_trait_implementation(&mut self, node: Node<'_>) {
        if self.language == SyntaxLanguage::Rust
            && node.kind() == "impl_item"
            && node.child_by_field_name("trait").is_some()
            && let Some(body) = node.child_by_field_name("body")
        {
            let opening = body.start_position().row;
            let closing = body.end_position().row;
            if opening < closing && self.rows[opening] {
                for row in opening + 1..closing {
                    self.rows[row] = false;
                }
                self.replacement.insert(
                    opening,
                    format!(
                        "{}{}",
                        self.lines[opening].trim_end(),
                        self.lines[closing].trim_start()
                    ),
                );
                self.replacement.insert(closing, String::new());
            }
            return;
        }
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            self.elide_trait_implementation(child);
        }
    }

    fn public(&self, node: Node<'_>) -> bool {
        let mut cursor = node.walk();
        node.named_children(&mut cursor).any(|child| {
            child.kind() == "visibility_modifier"
        })
    }

    fn collect_types(&mut self, node: Node<'_>) {
        if matches!(
            node.kind(),
            "struct_item" | "enum_item" | "trait_item" | "type_item"
        ) {
            if let Some(name) = node.child_by_field_name("name") {
                self.types
                    .insert(self.text[name.byte_range()].into(), self.public(node));
            }
        }
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            self.collect_types(child);
        }
    }

    fn mark(&mut self, node: Node<'_>) {
        let end = node.end_position();
        let last = end.row.saturating_sub(usize::from(end.column == 0));
        for row in node.start_position().row..=last {
            if let Some(visible) = self.rows.get_mut(row) {
                *visible = true;
            }
        }
    }

    fn group(&mut self, owner: Node<'_>, scope: Scope) -> bool {
        let mut cursor = owner.walk();
        let mut attachment = Vec::new();
        let mut tuple_public = false;
        let mut any = false;
        for node in owner.named_children(&mut cursor) {
            if matches!(
                node.kind(),
                "attribute_item"
                    | "inner_attribute_item"
                    | "line_comment"
                    | "block_comment"
                    | "comment"
            ) {
                attachment.push(node);
                continue;
            }
            if matches!(scope, Scope::Tuple) && node.kind() == "visibility_modifier" {
                tuple_public = self.text[node.byte_range()].trim() == "pub";
                attachment.push(node);
                continue;
            }
            let visible = if matches!(scope, Scope::Tuple) {
                if tuple_public {
                    self.mark(node);
                }
                let visible = tuple_public;
                tuple_public = false;
                visible
            } else {
                self.item(node, scope)
            };
            if visible {
                for attached in attachment.drain(..) {
                    self.mark(attached);
                }
            } else {
                attachment.clear();
            }
            any |= visible;
        }
        any
    }

    fn container(
        &mut self,
        node: Node<'_>,
        body: Node<'_>,
        scope: Scope,
        allow_empty: bool,
    ) -> bool {
        let any = self.group(body, scope);
        if !any && !allow_empty {
            return false;
        }
        for row in node.start_position().row..=body.start_position().row {
            if let Some(visible) = self.rows.get_mut(row) {
                *visible = true;
            }
        }
        if let Some(visible) = self.rows.get_mut(body.end_position().row) {
            *visible = true;
        }
        if !any
            && node.kind() == "struct_item"
            && body.start_position().row != body.end_position().row
        {
            let opening = body.start_position().row;
            let closing = body.end_position().row;
            self.replacement.insert(
                opening,
                format!(
                    "{}{}",
                    self.lines[opening].trim_end(),
                    self.lines[closing].trim_start()
                ),
            );
            self.replacement.insert(closing, String::new());
        }
        any
    }

    fn item(&mut self, node: Node<'_>, scope: Scope) -> bool {
        if self.language == SyntaxLanguage::Rust {
            if node.kind() == "impl_item" {
                let Some(owner) = node.child_by_field_name("type") else {
                    return false;
                };
                let owner_name = self.text[owner.byte_range()]
                    .split('<')
                    .next()
                    .unwrap_or_default()
                    .trim();
                if self.types.get(owner_name) == Some(&false) {
                    return false;
                }
                if let Some(trait_node) = node.child_by_field_name("trait") {
                    let name = self.text[trait_node.byte_range()]
                        .split('<')
                        .next()
                        .unwrap_or_default()
                        .trim();
                    if self.types.get(name) == Some(&false) {
                        return false;
                    }
                }
                let Some(body) = node.child_by_field_name("body") else {
                    return false;
                };
                let member_scope = if node.child_by_field_name("trait").is_some() {
                    Scope::Inherited
                } else {
                    Scope::Explicit
                };
                return self.container(node, body, member_scope, false);
            }
            let entry_point = matches!(scope, Scope::Top)
                && matches!(node.kind(), "function_item" | "function_signature_item")
                && node.child_by_field_name("name")
                    .is_some_and(|name| &self.text[name.byte_range()] == "main");
            if !matches!(scope, Scope::Inherited) && !self.public(node) && !entry_point {
                return false;
            }
            if let Some(body) = node.child_by_field_name("body") {
                let child_scope = match body.kind() {
                    _ if node.kind() == "enum_variant" => Scope::Inherited,
                    "enum_variant_list" => Scope::Inherited,
                    "ordered_field_declaration_list" => Scope::Tuple,
                    _ if node.kind() == "trait_item" => Scope::Inherited,
                    _ => Scope::Explicit,
                };
                self.container(node, body, child_scope, true);
            } else {
                self.mark(node);
            }
            return true;
        }
        if matches!(
            self.language,
            SyntaxLanguage::Typescript | SyntaxLanguage::Tsx
        ) {
            if node.kind() == "export_statement" {
                if let Some(declaration) = node.child_by_field_name("declaration") {
                    return self.item(declaration, Scope::Inherited);
                }
                self.mark(node);
                return true;
            }
            if matches!(scope, Scope::Top) {
                return false;
            }
            let mut cursor = node.walk();
            if node.named_children(&mut cursor).any(|child| {
                child.kind() == "private_property_identifier"
                    || child.kind() == "accessibility_modifier"
                        && matches!(
                            self.text[child.byte_range()].trim(),
                            "private" | "protected"
                        )
            }) {
                return false;
            }
            if let Some(body) = node
                .child_by_field_name("body")
                .or_else(|| node.child_by_field_name("value"))
                && matches!(
                    body.kind(),
                    "class_body" | "interface_body" | "object_type" | "enum_body"
                )
            {
                self.container(node, body, Scope::Members, true);
            } else {
                self.mark(node);
            }
            return true;
        }
        let text = self.text[node.byte_range()].trim();
        let mut local_owner = false;
        if node.kind() == "variable_declaration" {
            let mut declaration_cursor = node.walk();
            for assignment in node.named_children(&mut declaration_cursor) {
                let mut assignment_cursor = assignment.walk();
                for variables in assignment.named_children(&mut assignment_cursor).filter(|child| child.kind() == "variable_list") {
                    let mut binding_cursor = variables.walk();
                    local_owner |= variables.named_children(&mut binding_cursor).any(|binding| {
                        binding.kind() == "identifier" && self.returned.as_deref() == Some(&self.text[binding.byte_range()])
                    });
                }
            }
        }
        if text.starts_with("local ") && !local_owner {
            return false;
        }
        self.mark(node);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn visible(path: &str, source: &str) -> String {
        let text = DeclarationOverview::present(path, source).unwrap().text;
        let visibility = DeclarationVisibility::analyze(path, &text, true).unwrap();
        text.lines()
            .zip(visibility.rows)
            .enumerate()
            .filter_map(|(row, (line, public))| {
                public.then(|| {
                    visibility
                        .replacement
                        .get(&row)
                        .map(String::as_str)
                        .unwrap_or(line)
                })
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn configuration_remains_visible_in_public_only_inspection() {
        let source = "# Package settings\n[package]\nname = \"arena\"\nversion = \"0.1.0\"\n\n[dependencies]\nbevy = \"0.17\"\n";
        assert_eq!(visible("Cargo.toml", source), source.trim_end());
    }

    #[test]
    fn public_filter_keeps_main_entry_point_and_attachments_but_hides_private_methods() {
        let source = "/// Starts the binary.\n#[tokio::main]\nasync fn main() -> Result<(), Error>;\n\nfn main_helper();\n\npub struct Handler;\n\nimpl Handler {\n  fn main();\n}\n";
        let output = visible("src/main.rs", source);
        assert!(output.contains("/// Starts the binary."));
        assert!(output.contains("#[tokio::main]"));
        assert!(output.contains("async fn main() -> Result<(), Error>;"));
        assert!(!output.contains("main_helper"));
        assert!(!output.contains("impl Handler"));
        assert!(!output.contains("  fn main();"));
    }

    #[test]
    fn trait_implementation_bodies_are_hidden_in_both_inspection_modes() {
        let source = "pub struct ArenaPlugin {}
impl ArenaPlugin { pub fn new() -> Self; fn internal(); }
impl Default for ArenaPlugin { fn default() -> Self; }";
        let text = DeclarationOverview::present("src/plugin.rs", source)
            .unwrap()
            .text;
        for public_only in [false, true] {
            let visibility =
                DeclarationVisibility::analyze("src/plugin.rs", &text, public_only).unwrap();
            let rendered = text
                .lines()
                .enumerate()
                .filter(|(row, _)| visibility.rows[*row])
                .map(|(row, line)| {
                    visibility
                        .replacement
                        .get(&row)
                        .map(String::as_str)
                        .unwrap_or(line)
                })
                .collect::<Vec<_>>()
                .join(
                    "
",
                );
            assert!(rendered.contains("impl Default for ArenaPlugin {}"));
            assert!(!rendered.contains("fn default"));
            assert!(rendered.contains("pub fn new"));
            assert_eq!(rendered.contains("fn internal"), !public_only);
            assert!(text.contains("fn default"));
        }
    }

    #[test]
    fn rust_visibility_keeps_public_contracts_and_hides_private_owners_and_attachments() {
        let text = visible(
            "lib.rs",
            "use external::Type;\n/// internal\n#[derive(Clone)]\nstruct Hidden;\npub struct Api { pub value: u64, secret: u64 }\nimpl Api { pub fn new() -> Self; fn secret(); }\nimpl Hidden { pub fn leaked(); }\npub(crate) struct Restricted;\npub trait Contract { fn call(&self); type Output; }\npub enum State { Ready, Done }\nimpl Contract for Api { fn call(&self); type Output = u64; }\n",
        );
        for private in [
            "use external",
            "internal",
            "derive",
            "Hidden",
            "secret",
            "leaked",
        ] {
            assert!(!text.contains(private), "{text}");
        }
        for public in [
            "pub(crate) struct Restricted",
            "pub struct Api",
            "pub value",
            "pub fn new",
            "fn call",
            "type Output",
            "Ready",
            "Done",
            "impl Contract for Api",
        ] {
            assert!(text.contains(public), "{text}");
        }
    }

    #[test]
    fn scoped_public_owners_members_and_attachments_remain_visible() {
        let source = "#[derive(Clone)]\n/// Crate state.\npub(crate) struct State { pub(super) value: u64, private: u64 }\nimpl State { pub(crate) fn new() -> Self; pub(super) fn update(&mut self); fn hidden(); }\npub(super) enum Phase { Ready, Done }\npub(crate) trait Contract { fn apply(&self); }\npub(super) fn configure();\nstruct Hidden;\nimpl Hidden { pub(crate) fn concealed(); }\n";
        let text = visible("state.rs", source);
        for retained in ["#[derive(Clone)]", "/// Crate state.", "pub(crate) struct State",
            "pub(super) value", "impl State", "pub(crate) fn new", "pub(super) fn update",
            "pub(super) enum Phase", "Ready", "Done", "pub(crate) trait Contract", "fn apply",
            "pub(super) fn configure"] {
            assert!(text.contains(retained), "missing {retained}: {text}");
        }
        for hidden in ["private:", "fn hidden", "struct Hidden", "fn concealed"] {
            assert!(!text.contains(hidden), "exposed {hidden}: {text}");
        }
    }

    #[test]
    fn typescript_and_lua_use_native_export_visibility() {
        let text = visible(
            "api.ts",
            "declare function hidden(): void;\nexport class Api { value: string; private secret: string; protected hidden(): void; public call(): void; }\n",
        );
        assert!(text.contains("value") && text.contains("call"), "{text}");
        assert!(
            !text.contains("secret") && !text.contains("hidden"),
            "{text}"
        );
        let text = visible(
            "api.lua",
            "local Api\nlocal function hidden()\nfunction Api.call()\nreturn Api\n",
        );
        assert!(
            text.contains("local Api") && text.contains("Api.call"),
            "{text}"
        );
        assert!(!text.contains("hidden"), "{text}");
    }

    #[test]
    fn tuple_fields_and_enum_payloads_keep_their_distinct_visibility() {
        let text = visible(
            "lib.rs",
            "pub struct Tuple(pub u64, String);\npub enum Event { Data { value: u64 }, Pair(u64, String) }\n",
        );
        assert!(
            text.contains("pub struct Tuple") && text.contains("pub u64"),
            "{text}"
        );
        let tuple = text.split("pub enum").next().unwrap();
        assert!(!tuple.contains("String"), "{text}");
        assert!(
            text.contains("value: u64") && text.contains("String"),
            "{text}"
        );
    }

    #[test]
    fn empty_public_structs_compact_without_losing_attributes_or_spacing() {
        let text = visible(
            "lib.rs",
            "#[derive(Debug, Clone)]\npub struct ArenaPlugin { config: u64 }\npub struct Next { pub value: u64 }\n",
        );
        assert!(
            text.contains("#[derive(Debug, Clone)]\npub struct ArenaPlugin {}"),
            "{text}"
        );
        assert!(!text.contains("config:"), "{text}");
        assert!(text.contains("\n\npub struct Next"), "{text}");
    }
}
