//! Resolves narrative code references against the complete proposed declaration set.

use super::{DeclarationDesign, PlanNavigationAnchor, PlanReviewTarget};
use crate::declaration::DeclarationDestination;
use forge_buffer::block::{BufferBlock, Decoration, TargetRange, TextPosition, TextRange};
use forge_buffer::identity::TargetId;
use forge_buffer::markdown::MarkdownRenderer;
use forge_diff::syntax::{DeclarationIndex, DeclarationRole};
use std::collections::HashMap;
use std::path::Path;

pub(super) struct Symbol {
    pub destination: DeclarationDestination,
    pub capture: &'static str,
    qualified: String,
}

pub(super) struct NarrativeSymbols {
    pub proposed: Vec<Symbol>,
    baseline: Vec<Symbol>,
}

impl NarrativeSymbols {
    pub fn new(design: &DeclarationDesign, workspace: &Path) -> Self {
        Self {
            proposed: design.proposed.iter().flat_map(|(path, text)| symbols(path, text, workspace)).collect(),
            baseline: design.baseline.iter().flat_map(|(path, file)| symbols(path, &file.text, workspace)).collect(),
        }
    }

    pub fn matching(&self, name: &str) -> Vec<&Symbol> {
        let mut found = self.proposed.iter().filter(|symbol| matches(symbol, name)).collect::<Vec<_>>();
        found.sort_by_key(|symbol| (&symbol.destination.path, symbol.destination.line, symbol.destination.column));
        found
    }

    pub fn removed(&self, name: &str) -> bool {
        self.matching(name).is_empty() && self.baseline.iter().any(|symbol| matches(symbol, name))
    }

    /// Prefer the owning file's declaration and leave ambiguous external names unclassified.
    pub fn capture(&self, path: &str, side: &str, name: &str) -> Option<&'static str> {
        let symbols = if side == "baseline" { &self.baseline } else { &self.proposed };
        let matching = symbols.iter().filter(|symbol| matches(symbol, name)).collect::<Vec<_>>();
        let local = matching.iter().filter(|symbol| Path::new(&symbol.destination.path) == Path::new(path)).collect::<Vec<_>>();
        if !local.is_empty() {
            let capture = local[0].capture;
            return local.iter().all(|symbol| symbol.capture == capture).then_some(capture);
        }
        match matching.as_slice() { [symbol] => Some(symbol.capture), _ => None }
    }
}

fn matches(symbol: &Symbol, name: &str) -> bool {
    let name = name.replace("::", ".");
    symbol.qualified == name || (!name.contains('.') && symbol.destination.name == name)
}

fn symbols(path: &str, text: &str, workspace: &Path) -> Vec<Symbol> {
    let Ok(index) = DeclarationIndex::extract(path, text) else { return Vec::new() };
    index.symbol.into_iter().filter(|symbol| !symbol.parameter).map(|symbol| {
        let capture = match symbol.role {
            DeclarationRole::Callable => "@function",
            DeclarationRole::Type => "@type",
            DeclarationRole::Module => "@module",
            DeclarationRole::Property => "@variable.member",
            DeclarationRole::Variant => "@constant",
            DeclarationRole::Value | DeclarationRole::Binding => "@variable",
        };
        let mut qualified = symbol.scope;
        qualified.push(symbol.name.clone());
        Symbol {
            qualified: qualified.join("."), capture,
            destination: DeclarationDestination {
                path: workspace.join(path).to_string_lossy().into_owned(),
                line: symbol.position.line, column: symbol.position.column,
                name: symbol.name, proposed: true, module_file: false,
            },
        }
    }).collect()
}

pub(super) fn spans(design: &DeclarationDesign) -> HashMap<String, Vec<(std::ops::Range<usize>, String)>> {
    let mut spans = HashMap::new();
    for section in design.document.sections() {
        for (range, name) in MarkdownRenderer::inline_code(&section.text) {
            if !identifier(&name) || section.text[range.clone()].contains('\n') { continue; }
            let prefix = &section.text[..range.start];
            let row = prefix.bytes().filter(|byte| *byte == b'\n').count();
            let offset = prefix.rfind('\n').map_or(0, |position| position + 1);
            spans.entry(format!("plan:metadata:{}:{row}", section.path)).or_insert_with(Vec::new)
                .push((range.start - offset..range.end - offset, name));
        }
    }
    spans
}

fn identifier(name: &str) -> bool {
    !name.is_empty() && name.replace("::", ".").split('.').all(|part| {
        let mut characters = part.chars();
        characters.next().is_some_and(|character| character == '_' || character.is_alphabetic())
            && characters.all(|character| character == '_' || character.is_alphanumeric())
    })
}

pub(super) fn decorate(
    block: &mut BufferBlock, anchor: &PlanNavigationAnchor,
    spans: &HashMap<String, Vec<(std::ops::Range<usize>, String)>>,
    symbols: &NarrativeSymbols, target: &mut HashMap<TargetId, PlanNavigationAnchor>,
) {
    for (range, name) in spans.get(&block.id.0).into_iter().flatten() {
        let range = TextRange { start: TextPosition { row: 0, column: range.start }, end: TextPosition { row: 0, column: range.end } };
        let id = TargetId(format!("plan:symbol:{}:{}", block.id.0, range.start.column));
        block.metadata.target.insert(0, TargetRange { id: id.clone(), range: range.clone() });
        let mut anchor = anchor.clone();
        anchor.target = PlanReviewTarget::InlineSymbol { name: name.clone() };
        anchor.label = name.clone();
        target.insert(id, anchor);
        if let [symbol] = symbols.matching(name).as_slice() {
            block.metadata.decoration.push(Decoration { range, capture: symbol.capture.into(), priority: 130 });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_names_preserve_private_symbols_and_distinguish_removed_names() {
        let mut design = DeclarationDesign::default();
        design.proposed.insert("src/main.rs".into(), "struct Health {}\nfn configure_game() {}\nmod combat {}\n".into());
        design.baseline.insert("src/main.rs".into(), super::super::DeclarationFile { text: "fn removed() {}".into(), source_digest: String::new() });
        let symbols = NarrativeSymbols::new(&design, Path::new("/workspace"));
        assert_eq!(symbols.matching("Health")[0].capture, "@type");
        assert_eq!(symbols.matching("configure_game")[0].capture, "@function");
        assert_eq!(symbols.matching("combat")[0].capture, "@module");
        assert!(symbols.matching("configure").is_empty());
        assert!(symbols.removed("removed"));
        assert!(!symbols.removed("external"));
        design.proposed.insert("src/other.rs".into(), "mod combat { pub fn spawn() {} }\nstruct Health {}".into());
        let symbols = NarrativeSymbols::new(&design, Path::new("/workspace"));
        assert_eq!(symbols.matching("Health").len(), 2);
        assert_eq!(symbols.matching("combat::spawn").len(), 1);
        assert_eq!(symbols.matching("combat.spawn").len(), 1);
    }

    #[test]
    fn inline_spans_exclude_prose_fences_and_expressions() {
        let mut design = DeclarationDesign::default();
        design.document.design = "é `Health` and ``combat``.\n```rust\n`ignored`\n```\n`foo()` and `combat::spawn`".into();
        let spans = spans(&design);
        let first = &spans["plan:metadata:design:0"];
        assert_eq!(first[0], (3..11, "Health".into()));
        assert_eq!(first[1].1, "combat");
        assert!(!spans.contains_key("plan:metadata:design:2"));
        assert_eq!(spans["plan:metadata:design:4"][0].1, "combat::spawn");
    }
}
