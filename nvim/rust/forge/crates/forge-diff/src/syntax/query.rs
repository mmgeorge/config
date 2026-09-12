use std::collections::HashSet;

use super::assets::QUERY_ASSET;
use regex::Regex;
use tree_sitter::{Node, Query, QueryMatch, QueryPredicateArg};

use super::SyntaxError;

pub(super) struct CompiledQuery {
    pub query: Query,
    predicate: Vec<Vec<Predicate>>,
}

enum Predicate {
    Pattern {
        capture: u32,
        pattern: Regex,
        negative: bool,
    },
    Contains {
        capture: u32,
        text: Vec<String>,
    },
    Parent {
        capture: u32,
        kind: Vec<String>,
        recursive: bool,
        negative: bool,
    },
    Directive,
}

impl CompiledQuery {
    pub fn new(language: &tree_sitter::Language, source: &str) -> Result<Self, SyntaxError> {
        let url_directive = Regex::new(r"\(#set!\s+(@[\w.]+)\s+url\s+(@[\w.]+)\s*\)").unwrap();
        let source = url_directive.replace_all(source, "(#forge-url! $1 $2)");
        let query =
            Query::new(language, &source).map_err(|error| SyntaxError::Query(error.to_string()))?;
        let mut predicate = Vec::new();
        for index in 0..query.pattern_count() {
            if !query.property_predicates(index).is_empty() {
                return Err(SyntaxError::Query("unsupported property predicate".into()));
            }
            let mut compiled = Vec::new();
            for requested in query.general_predicates(index) {
                if requested.operator.as_ref() == "forge-url!"
                    && requested.args.len() == 2
                    && requested
                        .args
                        .iter()
                        .all(|argument| matches!(argument, QueryPredicateArg::Capture(_)))
                {
                    compiled.push(Predicate::Directive);
                    continue;
                }
                let capture = match requested.args.first() {
                    Some(QueryPredicateArg::Capture(capture)) => *capture,
                    _ => {
                        return Err(SyntaxError::Query(format!(
                            "{} requires a capture",
                            requested.operator
                        )));
                    }
                };
                let text: Result<Vec<_>, _> = requested
                    .args
                    .iter()
                    .skip(1)
                    .map(|argument| match argument {
                        QueryPredicateArg::String(text) => Ok(text.to_string()),
                        _ => Err(SyntaxError::Query(
                            "predicate expects string argument".into(),
                        )),
                    })
                    .collect();
                let text = text?;
                compiled.push(match requested.operator.as_ref() {
                    "lua-match?" | "not-lua-match?" if text.len() == 1 => Predicate::Pattern {
                        capture,
                        pattern: lua_pattern(&text[0])?,
                        negative: requested.operator.starts_with("not-"),
                    },
                    "contains?" if !text.is_empty() => Predicate::Contains { capture, text },
                    "has-parent?" | "not-has-parent?" | "has-ancestor?" | "not-has-ancestor?"
                        if !text.is_empty() =>
                    {
                        Predicate::Parent {
                            capture,
                            kind: text,
                            recursive: requested.operator.contains("ancestor"),
                            negative: requested.operator.starts_with("not-"),
                        }
                    }
                    "offset!"
                        if text.len() == 4
                            && text.iter().all(|value| value.parse::<i32>().is_ok()) =>
                    {
                        Predicate::Directive
                    }
                    "set-lang-from-info-string!" | "set-lang-from-mimetype!" if text.is_empty() => {
                        Predicate::Directive
                    }
                    _ => {
                        return Err(SyntaxError::Query(format!(
                            "unsupported predicate {}",
                            requested.operator
                        )));
                    }
                });
            }
            predicate.push(compiled);
        }
        Ok(Self { query, predicate })
    }

    pub fn accepts(&self, matched: &QueryMatch<'_, '_>, source: &[u8]) -> bool {
        self.predicate[matched.pattern_index]
            .iter()
            .all(|predicate| {
                let capture = match predicate {
                    Predicate::Pattern { capture, .. }
                    | Predicate::Contains { capture, .. }
                    | Predicate::Parent { capture, .. } => *capture,
                    Predicate::Directive => return true,
                };
                matched
                    .captures()
                    .iter()
                    .filter(|matched| matched.index == capture)
                    .all(|matched| {
                        let text = std::str::from_utf8(&source[matched.node.byte_range()])
                            .unwrap_or_default();
                        match predicate {
                            Predicate::Pattern {
                                pattern, negative, ..
                            } => pattern.is_match(text) != *negative,
                            Predicate::Contains { text: needle, .. } => {
                                needle.iter().any(|needle| text.contains(needle))
                            }
                            Predicate::Parent {
                                kind,
                                recursive,
                                negative,
                                ..
                            } => has_parent(matched.node, kind, *recursive) != *negative,
                            Predicate::Directive => true,
                        }
                    })
            })
    }
}

fn has_parent(mut node: Node<'_>, kind: &[String], recursive: bool) -> bool {
    while let Some(parent) = node.parent() {
        if kind.iter().any(|kind| parent.kind() == kind) {
            return true;
        }
        if !recursive {
            break;
        }
        node = parent;
    }
    false
}

fn lua_pattern(pattern: &str) -> Result<Regex, SyntaxError> {
    let mut output = String::from("(?s)");
    let mut characters = pattern.chars().peekable();
    let mut class = false;
    while let Some(character) = characters.next() {
        match character {
            '%' => {
                let escaped = characters
                    .next()
                    .ok_or_else(|| SyntaxError::Query("unfinished Lua escape".into()))?;
                let range = match escaped {
                    'u' => Some("A-Z"),
                    'l' => Some("a-z"),
                    'a' => Some("A-Za-z"),
                    'd' => Some("0-9"),
                    'w' => Some("A-Za-z0-9"),
                    's' => Some("\\t\\n\\r\\x0B\\x0C "),
                    'x' => Some("A-Fa-f0-9"),
                    _ => None,
                };
                if let Some(range) = range {
                    if !class {
                        output.push('[');
                    }
                    output.push_str(range);
                    if !class {
                        output.push(']');
                    }
                } else if escaped.is_ascii_alphabetic() {
                    return Err(SyntaxError::Query(format!(
                        "unsupported Lua class %{escaped}"
                    )));
                } else {
                    output.push_str(&regex::escape(&escaped.to_string()));
                }
            }
            '[' => {
                class = true;
                output.push('[');
            }
            ']' => {
                class = false;
                output.push(']');
            }
            '-' if !class => output.push_str("*?"),
            '\\' => output.push_str("\\\\"),
            '{' | '}' | '|' => output.push_str(&regex::escape(&character.to_string())),
            _ => output.push(character),
        }
    }
    Regex::new(&output).map_err(|error| SyntaxError::Query(error.to_string()))
}

pub(super) fn source(language: &str, kind: &str) -> String {
    let mut output = String::new();
    append_source(language, kind, &mut HashSet::new(), &mut output);
    output
}

fn append_source(language: &str, kind: &str, visited: &mut HashSet<String>, output: &mut String) {
    if !visited.insert(language.to_owned()) {
        return;
    }
    let selected = QUERY_ASSET
        .iter()
        .filter(|asset| asset.language == language && asset.kind == kind);
    let assets: Vec<_> = selected.collect();
    let replacement = assets.iter().find(|asset| {
        asset.origin == "repository" && !asset.text.lines().any(|line| line.trim() == "; extends")
    });
    let ordered: Vec<_> = if let Some(replacement) = replacement {
        vec![*replacement]
    } else {
        ["upstream", "repository", "forge"]
            .into_iter()
            .flat_map(|origin| {
                assets
                    .iter()
                    .copied()
                    .filter(move |asset| asset.origin == origin)
            })
            .collect()
    };
    for asset in ordered {
        for line in asset.text.lines() {
            if let Some(inherited) = line
                .trim_start_matches(';')
                .trim()
                .strip_prefix("inherits:")
            {
                for parent in inherited.split(',') {
                    append_source(
                        parent.trim().trim_matches(['(', ')']),
                        kind,
                        visited,
                        output,
                    );
                }
            }
        }
        output.push_str(asset.text);
        output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn configured_lua_patterns_preserve_classes_escapes_and_literal_braces() {
        for (pattern, text, expected) in [
            ("^[A-Z][A-Z%d_]*$", "VALUE_2", true),
            ("^[A-Z][A-Z%d_]*$", "value", false),
            ("%slang%s*=", " lang =", true),
            ("%${", "${value}", true),
            ("^\\\\if[a-zA-Z@]+$", "\\\\ifdefined", true),
            ("^[-][-][-]", "--- comment", true),
        ] {
            assert_eq!(
                lua_pattern(pattern).unwrap().is_match(text),
                expected,
                "{pattern}"
            );
        }
    }

    #[test]
    fn inherited_typescript_preserves_repository_overrides_and_ttsx_inheritance() {
        let typescript = source("typescript", "highlights");
        assert!(typescript.contains("@keyword.import"));
        assert!(typescript.contains("@declaration"));
        let text = source("tsx", "highlights");
        assert!(text.contains("@declaration"));
        assert!(text.contains("jsx_element"));
    }
}
