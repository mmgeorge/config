use super::{DeclarationCalls, DeclarationOverview, SourceDeclaration};
use std::collections::{BTreeMap, HashMap};
use tree_sitter::{Node, Parser};

/// Declaration tokens and authored behavior grouped by lexical owner.
#[derive(Default)]
pub struct ReviewDeclarationInventory {
    declarations: BTreeMap<String, Vec<String>>,
}

#[derive(Default)]
struct DeclarationTokens {
    content: String,
    implementation_signature: String,
}

impl ReviewDeclarationInventory {
    /// Parse one side of a plan using the same signature surrogates and callable owners as validation.
    pub fn parse(
        path: &str,
        overview: &str,
        mut behaviors: HashMap<&str, (&str, &str)>,
    ) -> Option<Self> {
        let language = DeclarationOverview::language(path)?;
        let source = super::declaration::declaration_surrogate(language, overview).ok()?;
        let mut parser = Parser::new();
        parser.set_language(&language.grammar()).ok()?;
        let tree = parser.parse(&source, None)?;
        let root = tree.root_node();
        if root.has_error() {
            return None;
        }
        let lines: Vec<_> = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        let entries = super::structure::declarations(root, &source, language, &lines, true);
        let mut contents: Vec<_> = entries
            .iter()
            .map(|_| DeclarationTokens::default())
            .collect();
        let mut result = Self::default();
        let mut next = 0;
        collect_tokens(
            root,
            &source,
            &entries,
            &mut next,
            &mut Vec::new(),
            &mut contents,
        );

        if !behaviors.is_empty() {
            let by_position: HashMap<_, _> = entries
                .iter()
                .enumerate()
                .map(|(index, entry)| {
                    let line = &source[lines[entry.line - 1]..];
                    let column = line
                        .char_indices()
                        .nth(entry.column - 1)
                        .map(|(offset, _)| offset)?;
                    Some(((entry.line as u32, column as u32), index))
                })
                .collect::<Option<_>>()?;
            for callable in DeclarationCalls::from_tree(&source, language, &tree, true).ok()? {
                if let Some((kind, text)) = behaviors.remove(callable.owner.as_str()) {
                    let index = *by_position.get(&(callable.line, callable.column))?;
                    contents[index].content.push_str(kind);
                    contents[index].content.push('\0');
                    contents[index].content.push_str(text);
                }
            }
            if !behaviors.is_empty() {
                return None;
            }
        }
        let mut identities = Vec::<String>::with_capacity(entries.len());
        for (entry, content) in entries.iter().zip(contents) {
            let scope = entry.parent.map_or("", |index| identities[index].as_str());
            // Several distinct trait implementations may belong to the same Rust type.
            let name = if entry.kind == "impl_item" {
                &content.implementation_signature
            } else {
                &entry.name
            };
            let kind = if entry.callable {
                "callable"
            } else if matches!(
                entry.kind.as_str(),
                "enum_assignment" | "property_identifier"
            ) {
                "enum_member"
            } else if entry.kind == "associated_type" {
                "type_item"
            } else {
                &entry.kind
            };
            let identity = format!("{scope}/{kind}:{name}");
            identities.push(identity.clone());
            result
                .declarations
                .entry(identity)
                .or_default()
                .push(content.content);
        }
        Some(result)
    }

    /// Count additions, modifications and removals once across the entire file.
    pub fn counts(&self, proposed: &Self) -> Option<[usize; 3]> {
        let mut counts = [0; 3];
        for (identity, old) in &self.declarations {
            let Some(new) = proposed.declarations.get(identity) else {
                counts[2] += old.len();
                continue;
            };
            if old == new {
                continue;
            }
            // Cancel identical overloads before pairing a single unambiguous revision.
            let mut remaining: BTreeMap<&str, usize> = BTreeMap::new();
            for value in new {
                *remaining.entry(value).or_default() += 1;
            }
            let mut removed = 0;
            for value in old {
                if let Some(count) = remaining
                    .get_mut(value.as_str())
                    .filter(|count| **count > 0)
                {
                    *count -= 1;
                } else {
                    removed += 1;
                }
            }
            let added = remaining.values().sum::<usize>();
            if added > 0 && removed > 0 {
                if added != 1 || removed != 1 {
                    return None;
                }
                counts[1] += 1;
            } else {
                counts[0] += added;
                counts[2] += removed;
            }
        }
        for (identity, new) in &proposed.declarations {
            if !self.declarations.contains_key(identity) {
                counts[0] += new.len();
            }
        }
        Some(counts)
    }
}

/// Assign semantic tokens to their innermost declaration. Comments and imports have no delta.
fn collect_tokens(
    node: Node<'_>,
    source: &str,
    entries: &[SourceDeclaration],
    next: &mut usize,
    active: &mut Vec<usize>,
    contents: &mut [DeclarationTokens],
) {
    if node.start_byte() == node.end_byte()
        || node.kind().contains("comment")
        || matches!(
            node.kind(),
            "use_declaration" | "import_statement" | "import_alias"
        )
        || (node.kind() == "attribute_item"
            && node
                .named_child(0)
                .and_then(|attribute| attribute.named_child(0))
                .is_some_and(|name| &source[name.byte_range()] == "doc"))
    {
        return;
    }
    if node.child_count() != 0 && !node.kind().contains("string") {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            collect_tokens(child, source, entries, next, active, contents);
        }
        return;
    }
    while *next < entries.len() && entries[*next].source_bytes.start <= node.start_byte() {
        active.push(*next);
        *next += 1;
    }
    while active
        .last()
        .is_some_and(|index| entries[*index].source_bytes.end <= node.start_byte())
    {
        active.pop();
    }
    // Member separators belong to the surrounding grammar, not to a second changed declaration.
    if matches!(node.kind(), "," | ";")
        && node.parent().is_some_and(|parent| {
            matches!(
                parent.kind(),
                "field_declaration_list"
                    | "enum_variant_list"
                    | "enum_body"
                    | "interface_body"
                    | "class_body"
                    | "object_type"
                    | "table_constructor"
            )
        })
    {
        return;
    }
    if let Some(&index) = active.last() {
        let tokens = &mut contents[index];
        let text = source[node.byte_range()].trim_end();
        tokens.content.push_str(text);
        tokens.content.push('\0');
        if entries[index].kind == "impl_item"
            && node.end_byte() <= entries[index].signature_bytes.end
            && (node.kind() == "impl" || !tokens.implementation_signature.is_empty())
        {
            tokens.implementation_signature.push_str(text);
            tokens.implementation_signature.push('\0');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declaration_summary_counts_members_and_behavior_once() {
        let old = "struct Ui { count: u32 }\nimpl Ui {\n /// Old constructor.\n fn new(name: String) -> Self;\n fn clear();\n fn repaint();\n}\nimpl Default for Ui { fn default() -> Self; }\n";
        let new = "struct Ui { count: u64, visible: bool }\nimpl Ui {\n /// New constructor.\n fn new() -> Self;\n fn repaint();\n}\nimpl Default for Ui { fn default() -> Self; }\n";
        let before = ReviewDeclarationInventory::parse("ui.rs", old, HashMap::new()).unwrap();
        let after = ReviewDeclarationInventory::parse(
            "ui.rs",
            new,
            HashMap::from([
                ("Ui::new", ("Change", "Install defaults.")),
                ("Ui::repaint", ("Change", "Defer the repaint.")),
            ]),
        )
        .unwrap();
        assert_eq!(before.counts(&after), Some([1, 3, 1]));
    }

    #[test]
    fn declaration_summary_handles_documentation_enum_members_and_new_or_deleted_files() {
        let parse =
            |text| ReviewDeclarationInventory::parse("lib.ts", text, HashMap::new()).unwrap();
        assert_eq!(
            parse("enum Color { Red, Blue }").counts(&parse("enum Color { Red = 1, Green }")),
            Some([1, 1, 1])
        );
        let parse =
            |text| ReviewDeclarationInventory::parse("lib.lua", text, HashMap::new()).unwrap();
        let old = parse("--- Old description.\nlocal function open()\n");
        let new = parse("--- New description.\nlocal function open()\n");
        assert_eq!(old.counts(&new), Some([0, 0, 0]));
        assert_eq!(parse("").counts(&new), Some([1, 0, 0]));
        assert_eq!(
            ReviewDeclarationInventory::default().counts(&new),
            Some([1, 0, 0])
        );
        assert_eq!(
            old.counts(&ReviewDeclarationInventory::default()),
            Some([0, 0, 1])
        );
    }

    #[test]
    fn declaration_summary_ignores_imports_and_comments_and_rejects_ambiguous_overloads() {
        let parse =
            |text| ReviewDeclarationInventory::parse("lib.rs", text, HashMap::new()).unwrap();
        assert_eq!(
            parse("use a::Item;\nfn old();").counts(&parse("use b::Item;\nfn new();")),
            Some([1, 0, 1])
        );
        let old = "use a::Item;\nstruct Ui;\nimpl Ui {\n /// Old docs.\n fn new(item: Item);\n fn clear();\n}\n";
        let new = "use b::Item;\nstruct Ui;\nimpl Ui {\n /// New docs.\n fn new();\n}\n";
        assert_eq!(parse(old).counts(&parse(new)), Some([0, 1, 1]));
        let docs = old
            .replace("Old docs.", "New docs.")
            .replace("a::Item", "b::Item");
        assert_eq!(parse(old).counts(&parse(&docs)), Some([0, 0, 0]));
        let parse =
            |text| ReviewDeclarationInventory::parse("lib.ts", text, HashMap::new()).unwrap();
        assert_eq!(
            parse("interface Api { run(x: string): void; run(x: number): void; }").counts(&parse(
                "interface Api { run(x: boolean): void; run(x: object): void; }"
            )),
            None
        );
        assert_eq!(
            parse("interface Api { count: number; }")
                .counts(&parse("interface Api { count: string; ready: boolean; }")),
            Some([1, 1, 0])
        );
    }
}
