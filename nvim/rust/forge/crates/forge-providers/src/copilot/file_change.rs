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
            let mut diff = String::new();
            for hunk in file.hunk {
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
mod tests {
    use super::*;

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
}
