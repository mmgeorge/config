use crate::backend::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
use anyhow::{Context, Result};
use forge_diff::{display::RowKind, patch::UnifiedPatch};

/// Load reported file changes from native tool output without reading workspace files.
pub(super) fn from_output(output: &str, workspace: &str) -> Result<ProviderChangeSet> {
    let source = output.trim_start();
    if !source.starts_with("diff --git ") && !source.starts_with("--- ") {
        return Ok(ProviderChangeSet::default());
    }
    let patch = UnifiedPatch::parse(source).context("decode file-tool result diff")?;
    let file = patch
        .file
        .into_iter()
        .map(|file| {
            let old_path = file
                .old_path
                .as_deref()
                .map(|path| relative_path(path, workspace));
            let new_path = file
                .new_path
                .as_deref()
                .map(|path| relative_path(path, workspace));
            let kind = match (&old_path, &new_path) {
                (None, _) => ProviderChangeKind::Add,
                (_, None) => ProviderChangeKind::Delete,
                (Some(old), Some(new)) if old != new => ProviderChangeKind::Move,
                _ => ProviderChangeKind::Update,
            };
            let mut diff = String::new();
            for hunk in file.hunk {
                diff.push_str(&hunk.header);
                diff.push('\n');
                for row in hunk.row {
                    diff.push(match row.kind {
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
            ProviderFileChange {
                path: old_path
                    .as_ref()
                    .or(new_path.as_ref())
                    .expect("validated patch path")
                    .clone(),
                move_path: if kind == ProviderChangeKind::Move {
                    new_path
                } else {
                    None
                },
                kind,
                diff,
            }
        })
        .collect();
    Ok(ProviderChangeSet { file })
}

fn relative_path(path: &str, workspace: &str) -> String {
    let path = path.replace('\\', "/");
    let workspace = workspace.replace('\\', "/");
    let workspace = workspace.trim_end_matches('/');
    if !workspace.is_empty() {
        // Preserve workspace-relative identity when Copilot drops the leading slash in Git headers.
        for prefix in [workspace, workspace.trim_start_matches('/')] {
            if let Some(relative) = path
                .strip_prefix(prefix)
                .and_then(|suffix| suffix.strip_prefix('/'))
            {
                return relative.to_owned();
            }
        }
    }
    path
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn decodes_multiple_operations_and_exact_hunk_coordinates() {
        let changes = from_output(
            "\ndiff --git a/Users/me/project/src/old.rs b/Users/me/project/src/new.rs\n\
             similarity index 80%\nrename from /Users/me/project/src/old.rs\nrename to /Users/me/project/src/new.rs\n\
             --- a/Users/me/project/src/old.rs\n+++ b/Users/me/project/src/new.rs\n\
             @@ -40,2 +40,2 @@ fn run\n-old\n+new\n context\n\
             diff --git a/added.rs b/added.rs\nnew file mode 100644\n--- /dev/null\n+++ b/added.rs\n\
             @@ -0,0 +1 @@\n+added\n\\ No newline at end of file\n\
             diff --git a/deleted.rs b/deleted.rs\ndeleted file mode 100644\n--- a/deleted.rs\n+++ /dev/null\n\
             @@ -1 +0,0 @@\n-deleted\n",
            "/Users/me/project",
        ).unwrap();
        assert_eq!(changes.file.len(), 3);
        assert_eq!(changes.file[0].kind, ProviderChangeKind::Move);
        assert_eq!(changes.file[0].path, "src/old.rs");
        assert_eq!(changes.file[0].move_path.as_deref(), Some("src/new.rs"));
        assert!(
            changes.file[0]
                .diff
                .starts_with("@@ -40,2 +40,2 @@ fn run\n")
        );
        assert_eq!(changes.file[1].kind, ProviderChangeKind::Add);
        assert_eq!(
            changes.file[1].diff,
            "@@ -0,0 +1 @@\n+added\n\\ No newline at end of file\n"
        );
        assert_eq!(changes.file[2].kind, ProviderChangeKind::Delete);
        assert_eq!(changes.file[2].path, "deleted.rs");
    }

    #[test]
    fn normalizes_only_paths_inside_the_workspace() {
        for path in [
            "/Users/me/project/src/lib.rs",
            "Users/me/project/src/lib.rs",
            "src/lib.rs",
        ] {
            assert_eq!(relative_path(path, "/Users/me/project"), "src/lib.rs");
        }
        assert_eq!(
            relative_path("C:\\project\\src\\lib.rs", "C:\\project"),
            "src/lib.rs"
        );
        assert_eq!(
            relative_path("/Users/me/project-other/lib.rs", "/Users/me/project"),
            "/Users/me/project-other/lib.rs"
        );
    }

    #[test]
    fn distinguishes_plain_results_from_malformed_reported_diffs() {
        assert!(
            from_output("Modified 1 file(s): src/lib.rs", "")
                .unwrap()
                .is_empty()
        );
        assert!(from_output("diff --git a/file b/file\n@@ -1 +1 @@\n-old\n", "").is_err());
        let renamed = from_output(
            "diff --git a/old b/new\nsimilarity index 100%\nrename from old\nrename to new\n",
            "",
        )
        .unwrap();
        assert_eq!(renamed.file[0].kind, ProviderChangeKind::Move);
        assert!(renamed.file[0].diff.is_empty());
    }
}
