use std::collections::BTreeSet;

use super::{SyntaxFamily, SyntaxHandle, SyntaxRange};
use tree_sitter::{Node, Point};

/// Describes structural context in zero-based coordinates of one immutable source version.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HunkContext {
    pub label: String,
    pub scope: SyntaxRange,
    pub ancestor: Vec<SyntaxRange>,
    pub before: Vec<usize>,
    pub after: Vec<usize>,
}

impl SyntaxHandle {
    /// Finds parent boundaries and nearby siblings in the same parsed source used for highlights.
    /// Context never determines which raw edits belong to a stageable display group.
    pub fn hunk_context(&self, row: usize) -> Option<HunkContext> {
        let mut scope: Vec<_> = self.captures_intersecting_rows(row, row + 1)
            .filter(|capture| capture.family == SyntaxFamily::Context && capture.name.as_ref() == "scope")
            .filter(|capture| capture.range.start.row <= row && final_row(capture.range) >= row)
            .collect();
        scope.sort_by_key(|capture| (capture.range.end_byte - capture.range.start_byte, std::cmp::Reverse(capture.tree)));
        let selected = *scope.first()?;
        scope.retain(|capture| capture.tree == selected.tree);
        scope.truncate(3);
        let mut names = Vec::new();
        for scope in scope.iter().rev() {
            if let Some(name) = self.captures_intersecting_rows(scope.range.start.row, final_row(scope.range) + 1)
                .filter(|capture| capture.family == SyntaxFamily::Context
                    && capture.name.as_ref() == "scope.name" && capture.tree == scope.tree
                    && capture.range.start_byte >= scope.range.start_byte
                    && capture.range.end_byte <= scope.range.end_byte)
                .min_by_key(|capture| capture.range.start_byte)
            {
                names.push(self.source().text().get(name.range.start_byte..name.range.end_byte)?.to_owned());
            }
        }
        if names.is_empty() { return None; }
        let root = self.0.tree.get(selected.tree)?.root_node();
        let mut node = row_node(root, &self.0.line, self.source().text(), row)?;
        let target_parent = node.parent();
        let mut before = BTreeSet::new();
        let mut after = BTreeSet::new();
        before.insert(selected.range.start.row);
        after.insert(final_row(selected.range));
        loop {
            if node.start_byte() < selected.range.start_byte || node.end_byte() > selected.range.end_byte { break; }
            if node.start_position().row < row { before.insert(node.start_position().row); }
            let end = node.end_position();
            let end_row = end.row.saturating_sub(usize::from(end.column == 0 && end.row > node.start_position().row));
            if end_row > row { after.insert(end_row); }
            let Some(parent) = node.parent() else { break; };
            node = parent;
        }
        for direction in [-1isize, 1] {
            for distance in 1..=3 {
                let Some(neighbor) = row.checked_add_signed(direction * distance) else { break; };
                if neighbor < selected.range.start.row || neighbor > final_row(selected.range) { break; }
                let Some(node) = row_node(root, &self.0.line, self.source().text(), neighbor) else { break; };
                if node.parent() != target_parent { break; }
                if direction < 0 { before.insert(neighbor); } else { after.insert(neighbor); }
            }
        }
        let ancestor: Vec<_> = scope.iter().skip(1).map(|scope| scope.range).collect();
        for ancestor in &ancestor {
            before.insert(ancestor.start.row);
            after.insert(final_row(*ancestor));
        }
        before.retain(|candidate| *candidate < row);
        after.retain(|candidate| *candidate > row);
        Some(HunkContext {
            label: names.join("."), scope: selected.range, ancestor,
            before: before.into_iter().collect(), after: after.into_iter().collect(),
        })
    }
}

fn final_row(range: SyntaxRange) -> usize {
    range.end.row.saturating_sub(usize::from(range.end.column == 0 && range.end.row > range.start.row))
}

fn row_node<'tree>(root: Node<'tree>, line: &[usize], source: &str, row: usize) -> Option<Node<'tree>> {
    let start = *line.get(row)?;
    let end = line.get(row + 1).copied().unwrap_or(source.len());
    let text = source.get(start..end)?.trim_end();
    if text.is_empty() { return None; }
    let column = text.len() - text.trim_start().len();
    let mut node = root.named_descendant_for_point_range(Point::new(row, column), Point::new(row, text.len()))?;
    while let Some(parent) = node.parent() {
        if parent.start_position().row != row || parent.end_position().row != row { break; }
        node = parent;
    }
    Some(node)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{source::{Representation, SourceVersion}, syntax::{SyntaxEngine, SyntaxLanguage, SyntaxLimits, SyntaxRequest}, workers::{AnalysisPool, PoolLimits, WorkPriority}};
    use std::sync::Arc;

    async fn syntax(language: SyntaxLanguage, source: &str) -> SyntaxHandle {
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits { workers: 1, jobs: 2, input_bytes: 8 * 1024 * 1024 })), SyntaxLimits::default());
        engine.analyze(SyntaxRequest {
            source: SourceVersion::new(source.as_bytes().to_vec(), Representation::GitCanonical).unwrap(),
            language, priority: WorkPriority::Foreground, deadline: None,
        }).await.unwrap()
    }

    #[tokio::test]
    async fn rust_parent_path_and_sibling_rows_are_exact() {
        let source = "impl Engine {\n    fn run() {\n        if ready {\n            let before = 1;\n            let changed = 2;\n            let after = 3;\n        }\n    }\n}\n";
        let syntax = syntax(SyntaxLanguage::Rust, source).await;
        let context = syntax.hunk_context(4).unwrap();
        assert_eq!(context.label, "Engine.run");
        assert_eq!((context.scope.start.row, context.scope.end.row), (1, 7));
        assert_eq!(context.before, vec![0, 1, 2, 3]);
        assert_eq!(context.after, vec![5, 6, 7, 8]);
    }

    #[tokio::test]
    async fn source_versions_do_not_reuse_renamed_or_removed_parent_context() {
        let old = syntax(SyntaxLanguage::Rust, "fn old() {\n    let value = 1;\n}\n").await;
        let new = syntax(SyntaxLanguage::Rust, "fn new() {\n    if ready {\n        let value = 2;\n    }\n}\n").await;
        assert_eq!(old.hunk_context(1).unwrap().label, "old");
        let context = new.hunk_context(2).unwrap();
        assert_eq!(context.label, "new");
        assert_eq!(context.before, vec![0, 1]);
        assert_eq!(context.after, vec![3, 4]);
        assert_eq!(old.hunk_context(1).unwrap().after, vec![2]);
    }

    #[tokio::test]
    async fn nested_typescript_scopes_use_native_queries() {
        let typescript = syntax(SyntaxLanguage::Typescript, "class Engine {\n  run() {\n    const value = 1;\n  }\n}\n").await;
        assert_eq!(typescript.hunk_context(2).unwrap().label, "Engine.run");
    }
}
