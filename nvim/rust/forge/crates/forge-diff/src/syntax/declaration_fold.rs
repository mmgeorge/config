use tree_sitter::{Node, Parser};

use super::declaration::declaration_surrogate;
use super::{ConfigurationFormat, DeclarationOverview, SyntaxError, SyntaxLanguage};

/// Locates a declaration body in the disposable inspection text.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclarationFold {
    /// First attached documentation, attribute, or signature row.
    pub heading: usize,
    /// Opening brace row retained as the native fold summary.
    pub start: usize,
    /// Closing brace row included in the fold.
    pub end: usize,
    /// Initial native fold state before an explicit inspection choice.
    pub closed: bool,
    /// Text appended to the opening row when the body is collapsed.
    pub collapsed_suffix: String,
}

/// Extracts native container folds without changing saved declarations or visibility.
pub struct DeclarationFolding;

impl DeclarationFolding {
    /// Locate multiline Rust and TypeScript declaration bodies in formatted text.
    pub fn analyze(path: &str, text: &str) -> Result<Vec<DeclarationFold>, SyntaxError> {
        DeclarationOverview::parse(path, text)?;
        if ConfigurationFormat::for_path(path).is_some() {
            return Ok(Vec::new());
        }
        let language = DeclarationOverview::language(path)
            .ok_or_else(|| SyntaxError::Language(path.into()))?;
        if !matches!(
            language,
            SyntaxLanguage::Rust | SyntaxLanguage::Typescript | SyntaxLanguage::Tsx
        ) {
            return Ok(Vec::new());
        }
        let mut parser = Parser::new();
        parser
            .set_language(&language.grammar())
            .map_err(|error| SyntaxError::Query(error.to_string()))?;
        let source = declaration_surrogate(language, text);
        let tree = parser
            .parse(&source, None)
            .ok_or_else(|| SyntaxError::Query("declaration fold parsing was cancelled".into()))?;
        let mut folds = Vec::new();
        collect(
            tree.root_node(),
            &text.lines().collect::<Vec<_>>(),
            &mut folds,
        );
        Ok(folds)
    }
}

fn collect(node: Node<'_>, lines: &[&str], folds: &mut Vec<DeclarationFold>) {
    if matches!(
        node.kind(),
        "struct_item"
            | "enum_item"
            | "impl_item"
            | "trait_item"
            | "mod_item"
            | "class_declaration"
            | "abstract_class_declaration"
            | "class"
            | "interface_declaration"
            | "enum_declaration"
            | "internal_module"
    ) && let Some(body) = node.child_by_field_name("body")
    {
        let start = body.start_position().row;
        let end = body.end_position().row;
        if start < end
            && lines
                .get(start)
                .is_some_and(|line| line.trim_end().ends_with('{'))
            && lines
                .get(end)
                .is_some_and(|line| line.trim_start().starts_with('}'))
        {
            let mut heading = node.start_position().row;
            let mut preceding = node.prev_named_sibling();
            while let Some(previous) = preceding {
                if !matches!(
                    previous.kind(),
                    "attribute_item" | "line_comment" | "block_comment" | "comment"
                ) || previous.end_position().row + 1 < heading
                {
                    break;
                }
                heading = previous.start_position().row;
                preceding = previous.prev_named_sibling();
            }
            folds.push(DeclarationFold {
                heading,
                start,
                end,
                closed: node.kind() == "enum_item",
                collapsed_suffix: format!("...{}", lines[end].trim_start()),
            });
        }
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        collect(child, lines, folds);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn folds_container_bodies_without_hiding_attributes_or_empty_declarations() {
        for (path, text, count) in [
            (
                "lib.rs",
                "#[derive(Debug)]\npub enum Error {\n  /// Invalid size.\n  Size,\n}\n\npub struct State {\n  value: u64,\n}\n\nimpl State {\n  pub fn value(&self) -> u64;\n}\n\npub trait Store {\n  fn get(&self);\n}\n\npub struct Empty {}\n",
                4,
            ),
            (
                "api.ts",
                "export class Store {\n  private value: number;\n  get(): number;\n}\n\nexport interface Api {\n  get(): number;\n}\n",
                2,
            ),
        ] {
            let folds = DeclarationFolding::analyze(path, text).unwrap();
            assert_eq!(folds.len(), count);
            for fold in &folds {
                assert!(fold.heading <= fold.start && fold.start < fold.end);
                assert_eq!(fold.collapsed_suffix, "...}");
            }
            if path == "lib.rs" {
                assert_eq!(folds[0].heading, 0);
                assert_eq!(folds[0].start, 1);
                assert!(folds[0].closed);
                assert!(folds[1..].iter().all(|fold| !fold.closed));
            } else {
                assert!(folds.iter().all(|fold| !fold.closed));
            }
        }
    }
}
