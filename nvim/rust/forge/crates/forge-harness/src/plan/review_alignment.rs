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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ambiguous_overloads_and_reordered_callables_do_not_create_crossing_anchors() {
        let old = super::super::calls::present("example.ts",
            "declare function overloaded(value: string): void;\ndeclare function overloaded(value: number): void;\nfunction first(): void;\nfunction second(): void;", &[]).unwrap();
        let new = super::super::calls::present("example.ts",
            "declare function overloaded(value: number): void;\ndeclare function overloaded(value: string): void;\nfunction second(): void;\nfunction first(): void;", &[]).unwrap();
        let alignment = compare("example.ts", "example.ts", Some(&old), Some(&new)).unwrap();
        assert!(
            alignment
                .anchors
                .windows(2)
                .all(|pair| pair[0].old <= pair[1].old && pair[0].new <= pair[1].new)
        );
        let overload_end = old
            .callables("example.ts")
            .unwrap()
            .iter()
            .filter(|callable| callable.owner.starts_with("overloaded"))
            .map(|callable| callable.end_line as usize)
            .max()
            .unwrap();
        assert!(
            alignment
                .anchors
                .iter()
                .all(|anchor| anchor.old >= overload_end)
        );
    }
}
