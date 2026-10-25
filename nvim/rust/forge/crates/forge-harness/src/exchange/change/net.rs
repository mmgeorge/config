use std::collections::BTreeMap;

use anyhow::{Context, Result, ensure};
use forge_diff::{display::RowKind, patch::PatchHunk};

#[derive(Clone, Debug, Eq, PartialEq)]
struct Line {
    text: String,
    no_newline: bool,
}

#[derive(Clone)]
enum Piece {
    Original { start: usize, end: Option<usize> },
    Inserted(Line),
}

struct FileContent {
    original: BTreeMap<usize, Line>,
    piece: Vec<Piece>,
}

/// Compose sequential reported hunks without reading the current workspace.
/// Unknown unchanged ranges remain sparse. Inconsistent reports return an error.
pub(super) fn compose(hunk_list: &[String]) -> Result<Vec<String>> {
    let mut content = FileContent {
        original: BTreeMap::new(),
        piece: vec![Piece::Original {
            start: 0,
            end: None,
        }],
    };
    for hunk in hunk_list {
        let patch = format!("diff --git a/file b/file\n--- a/file\n+++ b/file\n{hunk}\n");
        let parsed = forge_diff::patch::UnifiedPatch::parse(&patch)?;
        for hunk in &parsed.file[0].hunk {
            content.apply(hunk)?;
        }
    }
    content.hunks()
}

impl FileContent {
    fn split(&mut self, position: usize) -> Result<usize> {
        let mut cursor = 0;
        for index in 0..self.piece.len() {
            if cursor == position {
                return Ok(index);
            }
            match self.piece[index].clone() {
                Piece::Inserted(_) => cursor += 1,
                Piece::Original { start, end } => {
                    let offset = position - cursor;
                    if end.is_none_or(|end| offset < end - start) {
                        let middle = start
                            .checked_add(offset)
                            .context("patch line coordinate overflow")?;
                        self.piece[index] = Piece::Original {
                            start,
                            end: Some(middle),
                        };
                        self.piece
                            .insert(index + 1, Piece::Original { start: middle, end });
                        return Ok(index + 1);
                    }
                    cursor += end.expect("finite original range") - start;
                }
            }
        }
        ensure!(cursor == position, "patch starts beyond the reported file");
        Ok(self.piece.len())
    }

    fn apply(&mut self, hunk: &PatchHunk<'_>) -> Result<()> {
        // Earlier hunks already changed the file, so use this hunk's new-side offset.
        let start = self.split(hunk.new_lines.start)?;
        let finish = hunk
            .new_lines
            .start
            .checked_add(hunk.old_lines.len())
            .context("patch line coordinate overflow")?;
        let end = self.split(finish)?;
        let mut previous = Vec::new();
        for piece in &self.piece[start..end] {
            match piece {
                Piece::Original { start, end } => {
                    for position in *start..end.context("unbounded removed source range")? {
                        previous.push(Piece::Original {
                            start: position,
                            end: Some(position + 1),
                        });
                    }
                }
                Piece::Inserted(line) => previous.push(Piece::Inserted(line.clone())),
            }
        }
        let mut previous = previous.into_iter();
        let mut replacement = Vec::new();
        for row in &hunk.row {
            let line = Line {
                text: row.text.to_owned(),
                no_newline: row.no_newline,
            };
            if row.kind == RowKind::Added {
                replacement.push(Piece::Inserted(line));
                continue;
            }
            let piece = previous
                .next()
                .context("reported old-side line is missing")?;
            match &piece {
                Piece::Original { start, .. } => {
                    if let Some(known) = self.original.get(start) {
                        ensure!(
                            known == &line,
                            "sequential edits disagree on original line {}",
                            start + 1
                        );
                    } else {
                        self.original.insert(*start, line);
                    }
                }
                Piece::Inserted(known) => ensure!(
                    known == &line,
                    "sequential edit does not match preceding write"
                ),
            }
            if row.kind == RowKind::Context {
                replacement.push(piece);
            }
        }
        ensure!(
            previous.next().is_none(),
            "reported old-side range is incomplete"
        );
        self.piece.splice(start..end, replacement);
        Ok(())
    }

    fn hunks(self) -> Result<Vec<String>> {
        let mut output = Vec::new();
        let mut old_position = 0;
        let mut new_position = 0;
        let mut inserted = Vec::new();
        for piece in self.piece {
            match piece {
                Piece::Inserted(line) => inserted.push(line),
                Piece::Original { start, end } => {
                    let mut before = String::new();
                    for position in old_position..start {
                        append_line(
                            &mut before,
                            self.original
                                .get(&position)
                                .context("original text is missing from the reported edits")?,
                        );
                    }
                    let mut after = String::new();
                    for line in &inserted {
                        append_line(&mut after, line);
                    }
                    append_hunks(&mut output, &before, &after, old_position, new_position);
                    new_position += inserted.len();
                    inserted.clear();
                    let Some(end) = end else { break };
                    new_position += end - start;
                    old_position = end;
                }
            }
        }
        Ok(output)
    }
}

fn append_line(output: &mut String, line: &Line) {
    output.push_str(&line.text);
    if !line.no_newline {
        output.push('\n');
    }
}

fn append_hunks(
    output: &mut Vec<String>,
    before: &str,
    after: &str,
    old_position: usize,
    new_position: usize,
) {
    let diff = similar::TextDiff::from_lines(before, after);
    for group in diff.grouped_ops(0) {
        let first = group.first().expect("nonempty diff group");
        let last = group.last().expect("nonempty diff group");
        let old_start = old_position + first.old_range().start;
        let new_start = new_position + first.new_range().start;
        let old_count = last.old_range().end - first.old_range().start;
        let new_count = last.new_range().end - first.new_range().start;
        let mut hunk = format!(
            "@@ -{},{} +{},{} @@\n",
            old_start + usize::from(old_count != 0),
            old_count,
            new_start + usize::from(new_count != 0),
            new_count
        );
        for operation in &group {
            for change in diff.iter_changes(operation) {
                hunk.push(match change.tag() {
                    similar::ChangeTag::Delete => '-',
                    similar::ChangeTag::Insert => '+',
                    similar::ChangeTag::Equal => ' ',
                });
                hunk.push_str(change.value());
                if !change.value().ends_with('\n') {
                    hunk.push_str("\n\\ No newline at end of file\n");
                }
            }
        }
        output.push(hunk);
    }
}

#[cfg(test)]
mod tests {
    use super::compose;

    #[test]
    fn repeated_writes_show_only_original_and_final_content() {
        let result = compose(&[
            "@@ -1 +1 @@\n-old\n+middle".into(),
            "@@ -1 +1 @@\n-middle\n+final".into(),
        ])
        .unwrap()
        .join("");
        assert!(result.contains("-old\n+final\n"));
        assert!(!result.contains("middle"));
    }

    #[test]
    fn shifted_disjoint_hunks_and_reversions_keep_original_coordinates() {
        let result = compose(&[
            "@@ -1 +1,2 @@\n-old\n+middle\n+extra".into(),
            "@@ -1000000000 +1000000001 @@\n-far\n+away".into(),
            "@@ -1,2 +1 @@\n-middle\n-extra\n+old".into(),
        ])
        .unwrap();
        assert_eq!(
            result,
            vec!["@@ -1000000000,1 +1000000000,1 @@\n-far\n+away\n"]
        );
    }

    #[test]
    fn overlapping_context_and_missing_newline_survive_composition() {
        let result = compose(&[
            "@@ -1,3 +1,3 @@\n first\n-old\n+middle\n last\n\\ No newline at end of file".into(),
            "@@ -2,2 +2,2 @@\n-middle\n-last\n\\ No newline at end of file\n+final\n+end\n\\ No newline at end of file".into(),
        ]).unwrap().join("");
        assert!(result.contains(
            "-old\n-last\n\\ No newline at end of file\n+final\n+end\n\\ No newline at end of file"
        ));
    }

    #[test]
    fn inconsistent_sequential_reports_are_errors() {
        assert!(
            compose(&[
                "@@ -1 +1 @@\n-old\n+middle".into(),
                "@@ -1 +1 @@\n-unrelated\n+final".into(),
            ])
            .is_err()
        );
    }
}
