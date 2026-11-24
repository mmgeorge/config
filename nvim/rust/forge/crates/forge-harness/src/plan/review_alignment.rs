use anyhow::Result;
use forge_diff::declaration_change::{DeclarationAlignment, DeclarationBoundary};
use forge_diff::source::SourceSide;
use std::collections::HashMap;

use super::calls::CallPresentation;

/// Adapt captured callable details to the shared source-independent comparison.
pub(super) fn compare(
    path: &str,
    destination: &str,
    before: Option<&CallPresentation>,
    after: Option<&CallPresentation>,
) -> Result<DeclarationAlignment> {
    let (Some(before), Some(after)) = (before, after) else {
        return Ok(DeclarationAlignment::default());
    };
    Ok(DeclarationAlignment::compare(
        &before.declaration.text,
        &after.declaration.text,
        &scopes(path, before)?,
        &scopes(destination, after)?,
    )?)
}

fn scopes<'source>(
    path: &str,
    source: &'source CallPresentation,
) -> Result<Vec<DeclarationBoundary<'source>>> {
    let mut result = Vec::new();
    for callable in source.callables(path)? {
        let name = callable
            .owner
            .rsplit_once('#')
            .filter(|(_, ordinal)| {
                !ordinal.is_empty() && ordinal.bytes().all(|byte| byte.is_ascii_digit())
            })
            .map_or(callable.owner.as_str(), |(name, _)| name);
        let start = source.declaration_row[&(callable.heading_line as usize - 1)];
        let signature = source.declaration_row[&(callable.end_line as usize - 1)];
        let end = source
            .body
            .get(&signature)
            .map_or(signature + 1, |body| body.end + 1);
        let signature = source.declaration_row[&(callable.line as usize - 1)];
        result.push(DeclarationBoundary {
            identity: name,
            lines: start..end,
            signature,
        });
    }
    Ok(result)
}

/// Pair the final rows of identical signatures before and after adding a plan behavior block.
pub(super) fn body_openings(
    before: Option<&CallPresentation>,
    after: Option<&CallPresentation>,
    alignment: &DeclarationAlignment,
) -> HashMap<(SourceSide, usize), usize> {
    let mut pairs = HashMap::new();
    let (Some(before), Some(after)) = (before, after) else {
        return pairs;
    };
    if after.body.is_empty() {
        return pairs;
    }
    let old_lines = before.plain.lines().collect::<Vec<_>>();
    let new_lines = after.plain.lines().collect::<Vec<_>>();
    for (&opening, body) in &after.body {
        let Some(old_heading) = alignment.baseline_signature(body.heading) else {
            continue;
        };
        let new_start = after.plain_row[body.heading];
        let new_end = after.plain_row[opening];
        let old_start = before.plain_row[old_heading];
        let old_end = old_start + (new_end - new_start);
        if old_lines.get(old_start..=old_end) != Some(&new_lines[new_start..=new_end]) {
            continue;
        }
        let old_opening = before.declaration_row[&old_end];
        if !before.body.contains_key(&old_opening) {
            pairs.insert((SourceSide::Old, old_opening), opening);
            pairs.insert((SourceSide::New, opening), old_opening);
        }
    }
    pairs
}
