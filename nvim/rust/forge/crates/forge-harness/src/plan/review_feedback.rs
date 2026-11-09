use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, ensure};
use similar::{ChangeTag, TextDiff};

use super::{PlanAnnotation, PlanDocument, PlanReviewTarget};

struct FeedbackFile {
    baseline: String,
    proposed: String,
    comment: BTreeMap<usize, BTreeSet<(bool, u32)>>,
}

/// Extract complete selected plan lines and up to three context lines on each side.
pub(crate) fn selected_lines(
    rendered: &super::RenderedPlan,
    selection: &super::PlanAnnotationInput,
) -> Result<String> {
    let rows = rendered.markdown.lines().collect::<Vec<_>>();
    let start = usize::try_from(selection.start_line)?;
    let end = usize::try_from(selection.end_line)?;
    ensure!(
        start > 0 && start <= end && end <= rows.len(),
        "question selection is outside the reviewed plan"
    );
    let start = start - 1;
    let mut output = format!(
        "Selected plan lines: {}-{}\n",
        selection.start_line, selection.end_line
    );
    for (heading, range) in [
        ("Context before", start.saturating_sub(3)..start),
        ("Selected text", start..end),
        ("Context after", end..end.saturating_add(3).min(rows.len())),
    ] {
        output.push_str(&format!("\n{heading}:\n"));
        for index in range {
            output.push_str(&format!("{}: {}\n", index + 1, rows[index]));
        }
    }
    Ok(output)
}

/// Present resolved comments beside numbered declaration diffs without exposing anchor bookkeeping.
pub(crate) fn render(document: &PlanDocument, annotation: &[PlanAnnotation]) -> Result<String> {
    let design = document
        .design
        .as_ref()
        .context("review feedback requires a declaration design")?;
    let parsed = design.parsed()?;
    let mut files = BTreeMap::<String, FeedbackFile>::new();
    let mut general = BTreeSet::new();
    for (index, comment) in annotation.iter().enumerate() {
        for subject in &comment.subject {
            let (path, baseline, line) = match &subject.target {
                PlanReviewTarget::Declaration {
                    path, side, line, ..
                } => (path, side == "baseline", *line),
                PlanReviewTarget::Call { path, side, .. }
                | PlanReviewTarget::Behavior { path, side, .. } => (path, side == "baseline", 0),
                PlanReviewTarget::File { path } => (path, !parsed.proposed.contains_key(path), 0),
                _ => {
                    general.insert(index);
                    continue;
                }
            };
            let before = if baseline {
                path.clone()
            } else {
                design
                    .moved
                    .iter()
                    .find(|(_, destination)| *destination == path)
                    .map(|(origin, _)| origin.clone())
                    .unwrap_or_else(|| path.clone())
            };
            let after = design
                .moved
                .get(&before)
                .cloned()
                .unwrap_or_else(|| path.clone());
            let text = if baseline {
                design.baseline.get(path).map(|file| &file.text)
            } else {
                parsed.proposed.get(path)
            };
            let calls = if baseline {
                &design.baseline_calls
            } else {
                &parsed.proposed_calls
            };
            let line = if let Some(text) = text {
                let presentation = super::calls::insert(
                    path,
                    text,
                    calls.get(path).map(Vec::as_slice).unwrap_or_default(),
                    false,
                    false,
                )?;
                if let PlanReviewTarget::Call { owner, name, .. } = &subject.target {
                    presentation
                        .call_row
                        .iter()
                        .find(|(_, targets)| {
                            targets
                                .iter()
                                .any(|target| target.owner == *owner && target.name == *name)
                        })
                        .map(|(row, _)| *row as u32 + 1)
                        .context("reviewed call is absent from its saved snapshot")?
                } else if let PlanReviewTarget::Behavior { owner, offset, .. } = &subject.target {
                    presentation
                        .behavior_row
                        .iter()
                        .find(|(_, target)| target.0 == *owner && target.1 == *offset)
                        .map(|(row, _)| *row as u32 + 1)
                        .context("reviewed behavior is absent from its saved snapshot")?
                } else if line > 0 {
                    presentation
                        .declaration_row
                        .get(&(line as usize - 1))
                        .map(|row| *row as u32 + 1)
                        .context("reviewed declaration is absent from its saved snapshot")?
                } else {
                    line
                }
            } else {
                line
            };
            files
                .entry(after.clone())
                .or_insert_with(|| FeedbackFile {
                    baseline: before,
                    proposed: after,
                    comment: BTreeMap::new(),
                })
                .comment
                .entry(index)
                .or_default()
                .insert((baseline, line));
        }
    }
    let mut output = String::new();
    for file in files.values() {
        let before = design
            .baseline
            .get(&file.baseline)
            .map(|source| {
                super::calls::combined(
                    &file.baseline,
                    &source.text,
                    design
                        .baseline_calls
                        .get(&file.baseline)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()?
            .unwrap_or_default();
        let after = parsed
            .proposed
            .get(&file.proposed)
            .map(|text| {
                super::calls::combined(
                    &file.proposed,
                    text,
                    parsed
                        .proposed_calls
                        .get(&file.proposed)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()?
            .unwrap_or_default();
        let diff = TextDiff::from_lines(&before, &after);
        let changes = diff.iter_all_changes().collect::<Vec<_>>();
        if changes.is_empty() {
            ensure!(
                file.comment.values().flatten().all(|(_, line)| *line == 0),
                "review comment line is absent from its saved declaration"
            );
            output.push_str(&format!("{}\n\n", file.proposed));
            for index in file.comment.keys() {
                output.push_str(&format!("{}\n\n", annotation[*index].body));
            }
            continue;
        }
        let mut ranges = Vec::<(usize, usize)>::new();
        let mut position = BTreeMap::<usize, BTreeSet<usize>>::new();
        for (index, selected) in &file.comment {
            for (baseline, line) in selected {
                let row = if *line == 0 {
                    0
                } else {
                    changes
                        .iter()
                        .position(|change| {
                            (if *baseline {
                                change.old_index()
                            } else {
                                change.new_index()
                            }) == Some(*line as usize - 1)
                        })
                        .context("review comment line is absent from its saved declaration")?
                };
                position.entry(*index).or_default().insert(row);
                ranges.push((row.saturating_sub(3), (row + 4).min(changes.len())));
            }
        }
        ranges.sort_unstable();
        let mut excerpts = Vec::<(usize, usize)>::new();
        for (start, end) in ranges {
            if let Some(previous) = excerpts.last_mut().filter(|previous| start <= previous.1) {
                previous.1 = previous.1.max(end);
            } else {
                excerpts.push((start, end));
            }
        }
        for (start, end) in excerpts {
            if file.baseline == file.proposed {
                output.push_str(&format!("{}\n", file.proposed));
            } else {
                output.push_str(&format!("{} → {}\n", file.baseline, file.proposed));
            }
            let rows = &changes[start..end];
            let fence = "`".repeat(
                rows.iter()
                    .map(|row| {
                        row.value()
                            .split(|character| character != '`')
                            .map(str::len)
                            .max()
                            .unwrap_or(0)
                    })
                    .max()
                    .unwrap_or(0)
                    .max(2)
                    + 1,
            );
            output.push_str(&format!("{fence}text\n"));
            let width = rows
                .iter()
                .map(|row| {
                    (row.new_index().or(row.old_index()).unwrap() + 1)
                        .to_string()
                        .len()
                })
                .max()
                .unwrap_or(1);
            for row in rows {
                let marker = match row.tag() {
                    ChangeTag::Delete => '-',
                    ChangeTag::Insert => '+',
                    ChangeTag::Equal => ' ',
                };
                let line = row.new_index().or(row.old_index()).unwrap() + 1;
                output.push_str(&format!(
                    "{line:>width$}  {marker} {}\n",
                    row.value().trim_end_matches(['\r', '\n'])
                ));
            }
            output.push_str(&format!("{fence}\n\n"));
            for (index, selected) in &position {
                let lines = selected
                    .range(start..end)
                    .map(|row| {
                        changes[*row]
                            .new_index()
                            .or(changes[*row].old_index())
                            .unwrap()
                            + 1
                    })
                    .collect::<BTreeSet<_>>();
                if let (Some(first), Some(last)) = (lines.first(), lines.last()) {
                    if file.comment[index].iter().any(|(_, line)| *line == 0) {
                        output.push_str(&format!("{}\n\n", annotation[*index].body));
                        continue;
                    }
                    let label = if first == last {
                        first.to_string()
                    } else {
                        format!("{first}–{last}")
                    };
                    output.push_str(&format!("{label}: {}\n\n", annotation[*index].body));
                }
            }
        }
    }
    for index in general {
        let comment = &annotation[index];
        output.push_str(&format!("{}:\n{}\n\n", comment.label, comment.body));
    }
    if output.is_empty() {
        output.push_str("None");
    }
    Ok(output)
}
