use std::ops::Range;

use super::SyntaxHandle;

/// Source-line regions used to select excerpts from an exact parsed source.
#[derive(Clone, Debug, Default)]
pub struct SourceOutline {
    /// Nested declaration scopes in source order. Ranges are zero-based and end-exclusive.
    pub scope: Vec<SourceScope>,
    /// Recognized import statements, including multiline imports.
    pub import: Vec<Range<usize>>,
}

/// A declaration's complete extent and its signature and closing-delimiter rows.
#[derive(Clone, Debug)]
pub struct SourceScope {
    /// Syntactic kind and lexical name used to pair old and new declarations.
    pub identity: String,
    /// Complete declaration, including its body.
    pub extent: Range<usize>,
    /// Signature through the opening delimiter, preserving multiline contracts.
    pub signature: Range<usize>,
    /// Closing delimiter when it occupies a separate source row.
    pub closing: Option<usize>,
}

impl SyntaxHandle {
    /// Locate declaration and import rows without language-specific rendering rules.
    /// Unrecoverable syntax yields an empty outline so callers retain ordinary diff rows.
    pub fn outline(&self) -> SourceOutline {
        let mut outline = SourceOutline::default();
        let Some(tree) = self.0.tree.first() else {
            return outline;
        };
        if tree.root_node().has_error() {
            return outline;
        }
        let source = self.source().text();
        let source_lines: Vec<_> = source.lines().collect();
        let mut pending = vec![tree.root_node()];
        while let Some(node) = pending.pop() {
            let kind = node.kind();
            let extent = node.start_position().row
                ..node.end_position().row + usize::from(node.end_position().column > 0);
            let require = matches!(kind, "function_call" | "call_expression")
                && node
                    .named_child(0)
                    .is_some_and(|name| source.get(name.byte_range()) == Some("require"));
            if kind.contains("import")
                || matches!(kind, "use_declaration" | "preproc_include")
                || require
            {
                outline.import.push(extent);
                continue;
            }
            if let Some(body) = node.child_by_field_name("body") {
                let declaration = node.child_by_field_name("name").is_some()
                    || node.child_by_field_name("parameters").is_some()
                    || kind.contains("function")
                    || kind.contains("method");
                if declaration && !extent.is_empty() {
                    let mut signature_byte_end = body.start_byte();
                    let mut cursor = node.walk();
                    let children: Vec<_> = node.children(&mut cursor).collect();
                    for child in children.into_iter().rev() {
                        if child.start_byte() >= body.start_byte() {
                            continue;
                        }
                        if child.kind().contains("comment") {
                            signature_byte_end = child.start_byte();
                        } else {
                            break;
                        }
                    }
                    if source.as_bytes().get(body.start_byte()) == Some(&b'{') {
                        signature_byte_end = body.start_byte() + 1;
                    }
                    let header = source[node.start_byte()..signature_byte_end].trim_end();
                    let signature_end =
                        extent.start + header.bytes().filter(|byte| *byte == b'\n').count() + 1;
                    let last = extent.end - 1;
                    let closing = source_lines.get(last).and_then(|line| {
                        matches!(line.trim(), "}" | "};" | "end" | ");" | "];").then_some(last)
                    });
                    outline.scope.push(SourceScope {
                        identity: format!(
                            "{kind}:{}",
                            node.child_by_field_name("name")
                                .and_then(|name| source.get(name.byte_range()))
                                .unwrap_or(kind)
                        ),
                        signature: extent.start..signature_end.max(extent.start + 1),
                        extent,
                        closing,
                    });
                }
            }
            let mut cursor = node.walk();
            let children: Vec<_> = node.named_children(&mut cursor).collect();
            pending.extend(children.into_iter().rev());
        }
        outline
    }
}
