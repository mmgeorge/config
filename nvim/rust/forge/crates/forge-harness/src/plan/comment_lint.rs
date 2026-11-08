use std::collections::BTreeMap;

use anyhow::{Context, Result};
use forge_diff::syntax::{DeclarationOverview, DeclarationPosition};

use super::DeclarationDesign;
use crate::declaration::{DeclarationDiagnostic, DeclarationValidationError};

/// Reject newly authored comment openings without imposing new rules on captured source.
pub(super) fn validate(design: &DeclarationDesign) -> Result<()> {
    let mut diagnostic = Vec::new();
    let moved_origin = design
        .moved
        .iter()
        .map(|(origin, destination)| (destination.as_str(), origin.as_str()))
        .collect::<BTreeMap<_, _>>();
    for path in design.changed_paths() {
        let Some(proposed) = design.proposed.get(&path) else {
            continue;
        };
        if !proposed
            .as_bytes()
            .windows(7)
            .any(|word| word.eq_ignore_ascii_case(b"Returns"))
        {
            continue;
        }
        let origin = moved_origin.get(path.as_str()).copied().unwrap_or(&path);
        let mut baseline = BTreeMap::<String, usize>::new();
        if let Some(source) = design.baseline.get(origin) {
            for comment in comments(origin, &source.text)? {
                if starts_with_returns(&comment.text) {
                    *baseline.entry(comment.text).or_default() += 1;
                }
            }
        }
        for comment in comments(&path, proposed)? {
            if !starts_with_returns(&comment.text) {
                continue;
            }
            if let Some(count) = baseline.get_mut(&comment.text)
                && *count > 0
            {
                *count -= 1;
                continue;
            }
            diagnostic.push(DeclarationDiagnostic {
                path: path.clone(),
                line: comment.position.line,
                column: comment.position.column,
                reference: "Returns".into(),
                error: true,
                reason: "Plan comments must not start with 'Returns'. Use 'Get' instead.".into(),
            });
        }
    }
    if diagnostic.is_empty() {
        Ok(())
    } else {
        Err(DeclarationValidationError { diagnostic }.into())
    }
}

struct PlanComment {
    position: DeclarationPosition,
    end_line: u32,
    marker: Option<String>,
    text: String,
}

fn comments(path: &str, text: &str) -> Result<Vec<PlanComment>> {
    let extracted = DeclarationOverview::comments(path, text)
        .map_err(|error| anyhow::anyhow!("{error:?}"))
        .with_context(|| format!("extract plan comments from {path}"))?;
    let mut comment = Vec::<PlanComment>::new();
    for source in extracted {
        let raw = source.text.trim_end_matches(['\r', '\n']);
        let end_line = source.position.line + raw.lines().count().saturating_sub(1) as u32;
        let (marker, body) = comment_body(raw);
        let normalized = body.split_whitespace().collect::<Vec<_>>().join(" ");
        if let Some(previous) = comment.last_mut()
            && marker.is_some()
            && previous.marker.as_deref() == marker
            && previous.end_line + 1 == source.position.line
            && previous.position.column == source.position.column
        {
            if !previous.text.is_empty() && !normalized.is_empty() {
                previous.text.push(' ');
            }
            previous.text.push_str(&normalized);
            previous.end_line = end_line;
        } else {
            comment.push(PlanComment {
                position: source.position,
                end_line,
                marker: marker.map(str::to_owned),
                text: normalized,
            });
        }
    }
    Ok(comment)
}

fn comment_body(raw: &str) -> (Option<&str>, String) {
    if let Some(block) = raw.strip_prefix("<!--") {
        return (None, block.strip_suffix("-->").unwrap_or(block).to_owned());
    }
    if let Some(block) = raw.strip_prefix("/*") {
        let body = block
            .strip_suffix("*/")
            .unwrap_or(block)
            .trim_start_matches(['*', '!']);
        return (
            None,
            body.lines()
                .map(|line| line.trim_start().trim_start_matches('*'))
                .collect::<Vec<_>>()
                .join(" "),
        );
    }
    if let Some(block) = raw.strip_prefix("--[") {
        let equal = block.bytes().take_while(|byte| *byte == b'=').count();
        if block.as_bytes().get(equal) == Some(&b'[') {
            let closing = format!("]{}]", "=".repeat(equal));
            let body = &block[equal + 1..];
            return (None, body.strip_suffix(&closing).unwrap_or(body).to_owned());
        }
    }
    for marker in ["///", "//!", "//", "---", "--", "#"] {
        if let Some(body) = raw.strip_prefix(marker) {
            return (Some(marker), body.to_owned());
        }
    }
    (None, raw.to_owned())
}

fn starts_with_returns(text: &str) -> bool {
    text.get(..7)
        .is_some_and(|word| word.eq_ignore_ascii_case("Returns"))
        && text[7..]
            .chars()
            .next()
            .is_none_or(|character| !character.is_alphanumeric() && character != '_')
}
