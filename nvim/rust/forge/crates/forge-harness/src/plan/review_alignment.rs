use anyhow::Result;
use forge_diff::declaration_change::{DeclarationAlignment, DeclarationBoundary};
use forge_diff::display::{DisplayRow, RowKind};
use forge_diff::source::SourceSide;
use std::collections::HashMap;

use super::calls::CallPresentation;
use super::review_diff::FileDiff;

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

/// Pair signature replacements that have one source row on each side of the same raw edit.
/// Callable boundaries and retained parameter names distinguish replacements from deletions.
pub(super) fn signature_replacements<'diff>(
    before: Option<&CallPresentation>,
    after: Option<&CallPresentation>,
    diff: &'diff FileDiff,
) -> Result<HashMap<(SourceSide, usize), &'diff DisplayRow>> {
    let mut pairs = HashMap::new();
    let (Some(before), Some(after), Some(old_path), Some(new_path)) =
        (before, after, &diff.old_path, &diff.new_path)
    else {
        return Ok(pairs);
    };

    let old_callable = before
        .callables(old_path)?
        .iter()
        .map(|callable| {
            (
                before.declaration_row[&(callable.line as usize - 1)],
                callable,
            )
        })
        .collect::<HashMap<_, _>>();
    let mut old_rows = HashMap::new();
    let mut new_rows = HashMap::new();
    for row in diff.hunk.iter().flat_map(|hunk| &hunk.row) {
        match row.kind() {
            RowKind::Removed => old_rows.insert(row.old_coordinate().unwrap().line, row),
            RowKind::Added => new_rows.insert(row.new_coordinate().unwrap().line, row),
            RowKind::Context => None,
        };
    }

    let mut old_matches = HashMap::new();
    let mut new_matches = HashMap::new();
    for proposed in after.callables(new_path)? {
        let heading = after.declaration_row[&(proposed.line as usize - 1)];
        let Some(baseline) = diff
            .alignment
            .baseline_signature(heading)
            .and_then(|line| old_callable.get(&line))
        else {
            continue;
        };
        let parameters = proposed.binding_position.iter().filter_map(|(name, new)| {
            let old = baseline.binding_position.get(name)?;
            Some((old.line, new.line))
        });
        for (old, new) in [
            (baseline.line, proposed.line),
            (baseline.end_line, proposed.end_line),
        ]
        .into_iter()
        .chain(parameters)
        {
            let old = before.declaration_row[&(old as usize - 1)];
            let new = after.declaration_row[&(new as usize - 1)];
            // A removed body needs its own visible final signature to anchor its fold.
            if before.body.get(&old).is_some_and(|body| old + 2 < body.end) {
                continue;
            }
            let (Some(removed), Some(added)) = (old_rows.get(&old), new_rows.get(&new)) else {
                continue;
            };
            if removed.raw_id == added.raw_id && removed.raw_id.is_some() {
                record_match(&mut old_matches, old, new);
                record_match(&mut new_matches, new, old);
            }
        }
    }

    for (old, new) in old_matches {
        if let Some(new) = new
            && new_matches.get(&new) == Some(&Some(old))
        {
            pairs.insert((SourceSide::Old, old), new_rows[&new]);
            pairs.insert((SourceSide::New, new), old_rows[&old]);
        }
    }
    Ok(pairs)
}

/// Keep split or merged signature rows in the raw diff when a match is ambiguous.
fn record_match(matches: &mut HashMap<usize, Option<usize>>, line: usize, partner: usize) {
    matches
        .entry(line)
        .and_modify(|matched| {
            if *matched != Some(partner) {
                *matched = None;
            }
        })
        .or_insert(Some(partner));
}
