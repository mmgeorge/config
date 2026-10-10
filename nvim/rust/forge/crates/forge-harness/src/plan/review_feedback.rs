use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, ensure};
use similar::{ChangeTag, TextDiff};

use super::{PlanAnnotation, PlanDocument, PlanReviewTarget};

struct FeedbackFile {
    baseline: String,
    proposed: String,
    comment: BTreeMap<usize, BTreeSet<(bool, u32)>>,
}

/// Present resolved comments beside numbered declaration diffs without exposing anchor bookkeeping.
pub(crate) fn render(document: &PlanDocument, annotation: &[PlanAnnotation]) -> Result<String> {
    let design = document
        .design
        .as_ref()
        .context("review feedback requires a declaration design")?;
    let mut files = BTreeMap::<String, FeedbackFile>::new();
    let mut general = BTreeSet::new();
    for (index, comment) in annotation.iter().enumerate() {
        for subject in &comment.subject {
            let (path, baseline, line) = match &subject.target {
                PlanReviewTarget::Declaration {
                    path, side, line, ..
                } => (path, side == "baseline", *line),
                PlanReviewTarget::Call { path, side, .. } | PlanReviewTarget::Change { path, side, .. } => (path, side == "baseline", 0),
                PlanReviewTarget::File { path } => (path, !design.proposed.contains_key(path), 0),
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
            let text = if baseline { design.baseline.get(path).map(|file| &file.text) } else { design.proposed.get(path) };
            let calls = if baseline { &design.baseline_calls } else { &design.proposed_calls };
            let line = if let Some(text) = text {
                let presentation = super::calls::insert(path, text, calls.get(path).map(Vec::as_slice).unwrap_or_default(), false, false)?;
                if let PlanReviewTarget::Call { owner, name, .. } = &subject.target {
                    presentation.call_row.iter().find(|(_, target)| target.0 == *owner && target.1 == *name).map(|(row, _)| *row as u32 + 1).context("reviewed call is absent from its saved snapshot")?
                } else if let PlanReviewTarget::Change { owner, offset, .. } = &subject.target {
                    presentation.change_row.iter().find(|(_, target)| target.0 == *owner && target.1 == *offset).map(|(row, _)| *row as u32 + 1).context("reviewed change is absent from its saved snapshot")?
                } else if line > 0 {
                    presentation.declaration_row.get(&(line as usize - 1)).map(|row| *row as u32 + 1).context("reviewed declaration is absent from its saved snapshot")?
                } else { line }
            } else { line };
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
            .map(|source| super::calls::combined(&file.baseline, &source.text, design.baseline_calls.get(&file.baseline).map(Vec::as_slice).unwrap_or_default())).transpose()?.unwrap_or_default();
        let after = design
            .proposed
            .get(&file.proposed)
            .map(|text| super::calls::combined(&file.proposed, text, design.proposed_calls.get(&file.proposed).map(Vec::as_slice).unwrap_or_default())).transpose()?.unwrap_or_default();
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
            ensure!(
                output.len() <= 8 * 1024 * 1024,
                "review feedback exceeds 8 MiB"
            );
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
            ensure!(
                output.len() <= 8 * 1024 * 1024,
                "review feedback exceeds 8 MiB"
            );
        }
    }
    for index in general {
        let comment = &annotation[index];
        output.push_str(&format!("{}:\n{}\n\n", comment.label, comment.body));
        ensure!(
            output.len() <= 8 * 1024 * 1024,
            "review feedback exceeds 8 MiB"
        );
    }
    if output.is_empty() {
        output.push_str("None");
    }
    Ok(output)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::{
        DeclarationDesign, DeclarationFile, PlanAnnotationSubject, document::test_fixture,
    };

    fn document(before: &str, after: &str) -> PlanDocument {
        let mut document = test_fixture("plan", "Review declarations.");
        let mut design = DeclarationDesign::default();
        design.baseline.insert(
            "src/controls.rs".into(),
            DeclarationFile {
                text: before.into(),
                source_digest: "baseline".into(),
            },
        );
        design
            .proposed
            .insert("src/controls.rs".into(), after.into());
        document.design = Some(design);
        document
    }

    fn comment(path: &str, side: &str, lines: &[u32], body: &str) -> PlanAnnotation {
        PlanAnnotation {
            subject: lines
                .iter()
                .map(|line| PlanAnnotationSubject {
                    target: PlanReviewTarget::Declaration {
                        path: path.into(),
                        side: side.into(),
                        line: *line,
                        column: None,
                    },
                    json_path: format!("internal.{side}.{line}"),
                    label: "internal label".into(),
                    path: Some(path.into()),
                })
                .collect(),
            label: "internal label".into(),
            body: body.into(),
        }
    }

    #[test]
    fn nearby_comments_share_one_excerpt_with_both_replacement_sides() {
        let document = document(
            "pub struct MovementInput {\n  pub direction: Vec2,\n}\n",
            "pub struct MovementInput {\n  pub velocity: Vec2,\n}\n",
        );
        let output = render(
            &document,
            &[
                comment(
                    "src/controls.rs",
                    "baseline",
                    &[2],
                    "Why replace direction with velocity?",
                ),
                comment(
                    "src/controls.rs",
                    "proposed",
                    &[2],
                    "Please document the units.",
                ),
            ],
        )
        .unwrap();
        assert_eq!(output.matches("src/controls.rs").count(), 1);
        assert!(output.contains("2  -   pub direction: Vec2,"));
        assert!(output.contains("2  +   pub velocity: Vec2,"));
        assert!(output.contains("2: Why replace direction with velocity?"));
        assert!(output.contains("2: Please document the units."));
        assert!(!output.contains("internal"));
        assert!(!output.contains("Comment on:"));
    }

    #[test]
    fn distant_comments_use_separate_bounded_excerpts_and_numbered_ranges() {
        let text = (1..=24)
            .map(|line| format!("line {line}\n"))
            .collect::<String>();
        let document = document(&text, &text);
        let output = render(
            &document,
            &[
                comment(
                    "src/controls.rs",
                    "proposed",
                    &[5, 6],
                    "Explain this range.",
                ),
                comment("src/controls.rs", "proposed", &[20], "Explain this line."),
            ],
        )
        .unwrap();
        assert_eq!(output.matches("src/controls.rs").count(), 2);
        assert!(output.contains("5–6: Explain this range."));
        assert!(output.contains("20: Explain this line."));
        assert!(output.contains("line 2\n"));
        assert!(output.contains("line 9\n"));
        assert!(output.contains("line 17\n"));
        assert!(output.contains("line 23\n"));
        for excluded in [1, 10, 16, 24] {
            assert!(!output.contains(&format!("line {excluded}\n")));
        }
    }

    #[test]
    fn baseline_context_uses_current_numbers_and_moves_retain_both_paths() {
        let mut document = document(
            "first\nunchanged\nlast\n",
            "added\nfirst\nunchanged\nlast\n",
        );
        let design = document.design.as_mut().unwrap();
        let proposed = design.proposed.remove("src/controls.rs").unwrap();
        design.proposed.insert("src/input.rs".into(), proposed);
        design
            .moved
            .insert("src/controls.rs".into(), "src/input.rs".into());
        let output = render(
            &document,
            &[
                comment("src/controls.rs", "baseline", &[2], "Keep this behavior."),
                comment("src/input.rs", "proposed", &[3], "Explain its purpose."),
            ],
        )
        .unwrap();
        assert_eq!(output.matches("src/controls.rs → src/input.rs").count(), 1);
        assert!(output.contains("3    unchanged"));
        assert!(output.contains("3: Keep this behavior."));
        assert!(output.contains("3: Explain its purpose."));
    }

    #[test]
    fn added_and_deleted_files_preserve_comments_and_escape_code_fences() {
        let mut document = document("deleted\n", "");
        let design = document.design.as_mut().unwrap();
        design.proposed.remove("src/controls.rs");
        design
            .proposed
            .insert("README.md".into(), "```rust\nexample\n```\n".into());
        let output = render(
            &document,
            &[
                comment("src/controls.rs", "baseline", &[1], "Why delete this?"),
                comment(
                    "README.md",
                    "proposed",
                    &[2],
                    "Clarify the example.\nPreserve this second line.",
                ),
            ],
        )
        .unwrap();
        assert!(output.contains("1  - deleted"));
        assert!(output.contains("1: Why delete this?"));
        assert!(output.contains("````text\n"));
        assert!(output.contains("2: Clarify the example.\nPreserve this second line."));
    }

    #[test]
    fn change_feedback_uses_saved_summary_rows_and_rejects_missing_offsets() {
        let mut document = document("pub fn run();\n", "pub fn run();\n");
        document.design.as_mut().unwrap().proposed_calls.insert("src/controls.rs".into(), vec![crate::plan::FunctionBody { evidence: None,
            owner: "run".into(), call: None, change: Some("Stop retrying authentication failures.\nRecord the final attempt.".into()),
        }]);
        let mut annotation = comment("src/controls.rs", "proposed", &[1], "Specify the retry limit.");
        annotation.subject[0].target = PlanReviewTarget::Change { path: "src/controls.rs".into(), side: "proposed".into(), owner: "run".into(), offset: 2 };
        let feedback = render(&document, &[annotation.clone()]).unwrap();
        assert!(feedback.contains("4: Specify the retry limit."));
        assert!(feedback.contains("Record the final attempt."));
        if let PlanReviewTarget::Change { offset, .. } = &mut annotation.subject[0].target { *offset = 99; }
        assert!(render(&document, &[annotation]).unwrap_err().to_string().contains("absent"));
    }

    #[test]
    fn section_and_empty_file_comments_remain_readable() {
        let document = document("", "");
        let annotations = [
            PlanAnnotation {
                subject: vec![PlanAnnotationSubject {
                    target: PlanReviewTarget::File {
                        path: "src/controls.rs".into(),
                    },
                    json_path: "internal".into(),
                    label: "internal".into(),
                    path: None,
                }],
                label: "File".into(),
                body: "Why is this empty?".into(),
            },
            PlanAnnotation {
                subject: vec![PlanAnnotationSubject {
                    target: PlanReviewTarget::Section {
                        section: crate::plan::PlanSection::Overview,
                    },
                    json_path: "internal".into(),
                    label: "Description".into(),
                    path: None,
                }],
                label: "Description".into(),
                body: "Describe the requirement.".into(),
            },
        ];
        let output = render(&document, &annotations).unwrap();
        assert!(output.contains("src/controls.rs\n\nWhy is this empty?"));
        assert!(output.contains("Description:\nDescribe the requirement."));
        assert_eq!(render(&document, &[]).unwrap(), "None");
    }

    #[test]
    fn missing_saved_lines_fail_before_sending_incomplete_feedback() {
        let document = document("before\n", "after\n");
        assert!(
            render(
                &document,
                &[comment("src/controls.rs", "proposed", &[10], "Missing.")]
            )
            .unwrap_err()
            .to_string()
            .contains("absent")
        );
        let empty = self::document("", "");
        assert!(
            render(
                &empty,
                &[comment("src/controls.rs", "proposed", &[1], "Missing.")]
            )
            .is_err()
        );
    }
}
