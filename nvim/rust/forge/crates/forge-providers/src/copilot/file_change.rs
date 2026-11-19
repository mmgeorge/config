use anyhow::{Context, Result, ensure};
use forge_diff::{display::RowKind, patch::UnifiedPatch};
use forge_provider_api::change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};

/// Load reported file changes from native tool output without reading workspace files.
pub fn from_output(output: &str, workspace: &str) -> Result<ProviderChangeSet> {
    let source = output.trim_start();
    if !source.starts_with("diff --git ") && !source.starts_with("--- ") {
        return Ok(ProviderChangeSet::default());
    }
    // Copilot uses "create file mode" where Git uses "new file mode".
    // Hunk rows retain their diff prefix and must not be rewritten.
    let normalized;
    let source = if source
        .lines()
        .any(|line| line.starts_with("create file mode "))
    {
        normalized = source
            .split_inclusive('\n')
            .map(|line| {
                line.strip_prefix("create file mode ")
                    .map_or_else(|| line.to_owned(), |mode| format!("new file mode {mode}"))
            })
            .collect::<String>();
        normalized.as_str()
    } else {
        source
    };
    let patch = UnifiedPatch::parse(source).context("decode file-tool result diff")?;
    let file = patch
        .file
        .into_iter()
        .map(|file| {
            // Copilot prefixes its null-device marker as a/dev/null or b/dev/null.
            // Decode it before a real workspace path can become relative dev/null.
            let old_path = file
                .old_path
                .as_deref()
                .filter(|path| *path != "dev/null")
                .map(|path| relative_path(path, workspace));
            let new_path = file
                .new_path
                .as_deref()
                .filter(|path| *path != "dev/null")
                .map(|path| relative_path(path, workspace));
            let path = old_path
                .as_ref()
                .or(new_path.as_ref())
                .context("Copilot file diff has no source path")?
                .clone();
            ensure!(
                file.hunk.iter().all(|hunk| {
                    (old_path.is_some() || hunk.old_lines.is_empty())
                        && (new_path.is_some() || hunk.new_lines.is_empty())
                }),
                "Copilot file diff has source rows for an absent file"
            );
            let kind = match (&old_path, &new_path) {
                (None, _) => ProviderChangeKind::Add,
                (_, None) => ProviderChangeKind::Delete,
                (Some(old), Some(new)) if old != new => ProviderChangeKind::Move,
                _ => ProviderChangeKind::Update,
            };
            let mut hunks = file.hunk;
            // Copilot splits created content on newlines and reports the empty remainder after
            // the final newline as one more added row, which would count a line the file does
            // not have. A sole row stays because a hunk cannot be empty.
            if old_path.is_none()
                && let Some(last) = hunks.last_mut()
                && last.row.len() > 1
                && last.row.last().is_some_and(|row| {
                    row.kind() == RowKind::Added && row.text.is_empty() && !row.no_newline
                })
            {
                let (old_range, trailing) = last
                    .header
                    .strip_prefix("@@ ")
                    .and_then(|ranges| ranges.split_once(" @@"))
                    .and_then(|(ranges, trailing)| {
                        Some((ranges.split_once(' ')?.0.to_owned(), trailing.to_owned()))
                    })
                    .context("Copilot file diff has an unreadable hunk header")?;
                last.row.pop();
                last.new_lines.end -= 1;
                last.header = format!(
                    "@@ {old_range} +{},{} @@{trailing}",
                    last.new_lines.start + 1,
                    last.new_lines.len()
                )
                .into();
            }
            let mut diff = String::new();
            for hunk in hunks {
                diff.push_str(&hunk.header);
                diff.push('\n');
                for row in hunk.row {
                    diff.push(match row.kind() {
                        RowKind::Added => '+',
                        RowKind::Removed => '-',
                        RowKind::Context => ' ',
                    });
                    diff.push_str(row.text);
                    diff.push('\n');
                    if row.no_newline {
                        diff.push_str("\\ No newline at end of file\n");
                    }
                }
            }
            Ok(ProviderFileChange {
                path,
                move_path: if kind == ProviderChangeKind::Move {
                    new_path
                } else {
                    None
                },
                kind,
                diff,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(ProviderChangeSet { file })
}

/// Report a changed path relative to the workspace when inside it, with `/` separators.
fn relative_path(path: &str, workspace: &str) -> String {
    crate::tool_display::workspace_relative(path, workspace)
        .unwrap_or_else(|| path.replace('\\', "/"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Copilot splits created content on newlines, so content ending in a newline reports one
    /// empty row more than the file has lines.
    #[test]
    fn created_file_omits_the_empty_row_after_its_final_newline() -> Result<()> {
        let workspace = "C:/work/project";
        let output = "\ndiff --git a/C:/work/project/notes/wave.txt b/C:/work/project/notes/wave.txt\ncreate file mode 100644\nindex 0000000..0000000\n--- a/dev/null\n+++ b/C:/work/project/notes/wave.txt\n@@ -1,0 +1,4 @@\n+wave\n+\n+crest\n+\n\n";
        let changes = from_output(output, workspace)?;
        assert_eq!(changes.file.len(), 1);
        let created = &changes.file[0];
        assert_eq!(created.kind, ProviderChangeKind::Add);
        assert_eq!(created.path, "notes/wave.txt");
        assert_eq!(created.diff, "@@ -1,0 +1,3 @@\n+wave\n+\n+crest\n");
        Ok(())
    }

    /// An existing file's final empty added row is a real blank line, and a created file's only
    /// row is the whole hunk, so neither is removed.
    #[test]
    fn final_empty_rows_outside_multirow_creations_are_preserved() -> Result<()> {
        let updated = "diff --git a/notes/wave.txt b/notes/wave.txt\n--- a/notes/wave.txt\n+++ b/notes/wave.txt\n@@ -1,1 +1,3 @@\n wave\n+crest\n+\n";
        let changes = from_output(updated, "")?;
        assert_eq!(changes.file[0].kind, ProviderChangeKind::Update);
        assert_eq!(changes.file[0].diff, "@@ -1,1 +1,3 @@\n wave\n+crest\n+\n");

        let empty = "diff --git a/notes/empty.txt b/notes/empty.txt\ncreate file mode 100644\n--- a/dev/null\n+++ b/notes/empty.txt\n@@ -1,0 +1,1 @@\n+\n";
        let changes = from_output(empty, "")?;
        assert_eq!(changes.file[0].kind, ProviderChangeKind::Add);
        assert_eq!(changes.file[0].diff, "@@ -1,0 +1,1 @@\n+\n");
        Ok(())
    }
}
