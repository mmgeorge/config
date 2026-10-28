use std::collections::{BTreeMap, HashSet};
use std::ops::Range;

use anyhow::Result;
use forge_diff::raw::LineAnchor;

use super::calls::CallPresentation;

/// Matches unambiguous callable identities before comparing their source lines.
#[derive(Default)]
pub(super) struct ReviewAlignment {
    /// Monotonic boundaries retain source order on both sides of the comparison.
    pub anchors: Vec<LineAnchor>,
    /// Changed callable signatures receive a modified marker even when their text is unchanged.
    pub modified_signature: HashSet<usize>,
    /// Added lines inside a matched changed callable are revisions of that callable.
    pub modified_body: Vec<Range<usize>>,
}

impl ReviewAlignment {
    /// Uses patience-style ordered anchors, with Tree-sitter identities instead of unique text lines.
    pub fn new(
        path: &str,
        destination: &str,
        before: Option<&CallPresentation>,
        after: Option<&CallPresentation>,
    ) -> Result<Self> {
        let (Some(before), Some(after)) = (before, after) else {
            return Ok(Self::default());
        };
        let old = scopes(path, before)?;
        let new = scopes(destination, after)?;
        let mut pairs = old
            .iter()
            .filter_map(|(name, scope)| new.get(name).map(|next| (scope, next)))
            .collect::<Vec<_>>();
        pairs.sort_by_key(|(scope, _)| scope.lines.start);

        let mut tails: Vec<usize> = Vec::new();
        let mut previous = vec![None; pairs.len()];
        for (index, (_, scope)) in pairs.iter().enumerate() {
            let length =
                tails.partition_point(|&tail| pairs[tail].1.lines.start < scope.lines.start);
            if length > 0 {
                previous[index] = Some(tails[length - 1]);
            }
            if length == tails.len() {
                tails.push(index);
            } else {
                tails[length] = index;
            }
        }
        let mut selected = Vec::new();
        let mut index = tails.last().copied();
        while let Some(current) = index {
            selected.push(current);
            index = previous[current];
        }
        selected.reverse();

        let old_lines = before.declaration.text.lines().collect::<Vec<_>>();
        let new_lines = after.declaration.text.lines().collect::<Vec<_>>();
        let mut result = Self::default();
        let mut end = LineAnchor { old: 0, new: 0 };
        for index in selected {
            let (old, new) = pairs[index];
            if old.lines.start < end.old || new.lines.start < end.new {
                continue;
            }
            result.anchors.push(LineAnchor {
                old: old.lines.start,
                new: new.lines.start,
            });
            end = LineAnchor {
                old: old.lines.end,
                new: new.lines.end,
            };
            result.anchors.push(end);
            if old_lines[old.lines.clone()] != new_lines[new.lines.clone()] {
                result.modified_signature.insert(new.signature);
                result.modified_body.push(new.lines.clone());
            }
        }
        Ok(result)
    }

    /// Styles revisions without changing old/new source coordinates or raw edit counts.
    pub fn modified(&self, row: &forge_diff::display::DisplayRow) -> bool {
        row.new_coordinate().is_some_and(|coordinate| {
            let next = self
                .modified_body
                .partition_point(|scope| scope.end <= coordinate.line);
            self.modified_signature.contains(&coordinate.line)
                || row.kind() == forge_diff::display::RowKind::Added
                    && self
                        .modified_body
                        .get(next)
                        .is_some_and(|scope| scope.contains(&coordinate.line))
        })
    }
}

struct CallableScope {
    lines: Range<usize>,
    signature: usize,
}

fn scopes(path: &str, source: &CallPresentation) -> Result<BTreeMap<String, CallableScope>> {
    let mut result = BTreeMap::new();
    let mut ambiguous = HashSet::new();
    for callable in source.callables(path)? {
        let name = callable
            .owner
            .rsplit_once('#')
            .filter(|(_, ordinal)| {
                !ordinal.is_empty() && ordinal.bytes().all(|byte| byte.is_ascii_digit())
            })
            .map_or(callable.owner.as_str(), |(name, _)| name)
            .to_owned();
        let start = source.declaration_row[&(callable.heading_line as usize - 1)];
        let signature = source.declaration_row[&(callable.end_line as usize - 1)];
        let end = source
            .body
            .get(&signature)
            .map_or(signature + 1, |body| body.end + 1);
        let signature = source.declaration_row[&(callable.line as usize - 1)];
        if result
            .insert(
                name.clone(),
                CallableScope {
                    lines: start..end,
                    signature,
                },
            )
            .is_some()
        {
            ambiguous.insert(name);
        }
    }
    result.retain(|name, _| !ambiguous.contains(name));
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
        let alignment =
            ReviewAlignment::new("example.ts", "example.ts", Some(&old), Some(&new)).unwrap();
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
