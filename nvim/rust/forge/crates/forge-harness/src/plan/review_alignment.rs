use anyhow::Result;
use forge_diff::declaration_change::{DeclarationAlignment, DeclarationBoundary};

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
