use crate::backend::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
use anyhow::{Context, Result, ensure};
use forge_diff::{display::RowKind, patch::UnifiedPatch};

/// Load reported file changes from native tool output without reading workspace files.
pub(super) fn from_output(output: &str, workspace: &str) -> Result<ProviderChangeSet> {
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
mod test {
    use super::*;

    #[test]
    fn copilot_creation_headers_preserve_all_files_and_literal_hunk_text() {
        let output = "\ndiff --git a/Users/me/project/old.rs b/Users/me/project/old.rs\n\
            index 0000000..0000000 100644\n--- a/Users/me/project/old.rs\n+++ b/Users/me/project/old.rs\n\
            @@ -1 +1 @@\n-before\n+after\n\
            diff --git a/Users/me/project/new.rs b/Users/me/project/new.rs\n\
            create file mode 100644\nindex 0000000..0000000\n--- a/dev/null\n+++ b/Users/me/project/new.rs\n\
            @@ -1,0 +1,2 @@\n+create file mode 100644\n+retained\n\
            diff --git a/Users/me/project/gone.rs b/dev/null\n\
            --- a/Users/me/project/gone.rs\n+++ b/dev/null\n@@ -1 +0,0 @@\n-gone\n";
        for source in [output.to_owned(), output.replace('\n', "\r\n")] {
            let changes = from_output(&source, "/Users/me/project").unwrap();
            assert_eq!(changes.file.len(), 3);
            assert_eq!(changes.file[0].kind, ProviderChangeKind::Update);
            assert_eq!(changes.file[0].path, "old.rs");
            assert!(
                changes.file[0]
                    .diff
                    .replace("\r\n", "\n")
                    .contains("-before\n+after")
            );
            assert_eq!(changes.file[1].kind, ProviderChangeKind::Add);
            assert_eq!(changes.file[1].path, "new.rs");
            assert!(changes.file[1].diff.contains("+create file mode 100644"));
            assert_eq!(changes.file[2].kind, ProviderChangeKind::Delete);
            assert_eq!(changes.file[2].path, "gone.rs");
            assert!(changes.file[2].diff.contains("-gone"));
            assert_creation_report_renders(changes);
        }
        assert!(
            UnifiedPatch::parse(output.trim_start()).is_err(),
            "provider dialect normalization must not change ordinary Git parsing"
        );
    }

    fn assert_creation_report_renders(changes: ProviderChangeSet) {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(serde_json::json!({
            "id":"creation", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Edit", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0).unwrap();
        exchange
            .observe_turn(
                &BackendEvent {
                    message: None,
                    received_at_ms: None,
                    address: Some(address),
                    turn_boundary: None,
                    kind: "tool".into(),
                    text: None,
                    data: serde_json::Value::Null,
                    summary: None,
                    checklist_update: None,
                    activity: Some(ToolActivity {
                        id: "edit".into(),
                        kind: ToolActivityKind::FileChange,
                        title: "edit".into(),
                        output: None,
                        output_delta: false,
                        status: Some(crate::backend::ToolStatus::Succeeded),
                        change: changes,
                    }),
                },
                1,
            )
            .unwrap();
        let projected = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(&exchange),
            &forge_buffer::width::WidthProfile::default(),
            false,
            &Default::default(),
            &Default::default(),
            &mut Default::default(),
        )
        .unwrap();
        let rendered = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(rendered.contains("Reported edits · 1 operation"));
        for path in ["old.rs", "new.rs", "gone.rs"] {
            assert!(rendered.contains(path));
        }
        assert!(
            !rendered.contains("details unavailable") && !rendered.contains("preview unavailable")
        );
    }

    #[test]
    fn prefixed_null_markers_render_as_additions_and_deletions() {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use crate::buffer::projection::{ProjectionSource, project};
        use crate::exchange::Exchange;
        use forge_buffer::width::WidthProfile;

        for workspace in ["/Users/me/project", "C:/project"] {
            let reported_root = workspace.trim_start_matches('/');
            let source = format!(
                "diff --git a/{reported_root}/added.rs b/{reported_root}/added.rs\n\
                 --- a/dev/null\n+++ b/{reported_root}/added.rs\n@@ -1,0 +1 @@\n+added\n\
                 diff --git a/{reported_root}/capture_probe.rs b/dev/null\n\
                 --- a/{reported_root}/capture_probe.rs\n+++ b/dev/null\n@@ -1,88 +0,0 @@\n{}",
                "-removed\n".repeat(88),
            );
            let changes = from_output(&source, workspace).unwrap();
            assert_eq!(changes.file.len(), 2);
            assert_eq!(changes.file[0].kind, ProviderChangeKind::Add);
            assert_eq!(changes.file[0].path, "added.rs");
            assert_eq!(changes.file[1].kind, ProviderChangeKind::Delete);
            assert_eq!(changes.file[1].path, "capture_probe.rs");
            assert!(changes.file.iter().all(|file| file.move_path.is_none()));

            let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
                "id":"changes", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Edit files", "kind":"chat", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(0).unwrap();
            let mut event = BackendEvent {
                message: None,
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                }),
                turn_boundary: Some(TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: serde_json::Value::Null,
                activity: None,
                summary: None,
                checklist_update: None,
            };
            exchange.observe_turn(&event, 0).unwrap();
            event.turn_boundary = None;
            event.kind = "tool".into();
            event.activity = Some(ToolActivity {
                id: "edit".into(),
                kind: ToolActivityKind::FileChange,
                title: "apply_patch".into(),
                output: Some(source),
                output_delta: false,
                status: Some(crate::backend::ToolStatus::Succeeded),
                change: changes,
            });
            exchange.observe_turn(&event, 1).unwrap();
            let saved = serde_json::to_string(&exchange).unwrap();
            let reopened: Exchange = serde_json::from_str(&saved).unwrap();
            let projected = project(
                ProjectionSource::Exchange(&reopened),
                &WidthProfile::default(),
                false,
                &Default::default(),
                &Default::default(),
                &mut crate::buffer::identity::DisplayRegistry::default(),
            )
            .unwrap();
            let rows = projected
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>();
            assert!(rows.contains(&"New      added.rs +1 -0"), "{rows:?}");
            assert!(
                rows.contains(&"Deleted  capture_probe.rs +0 -88"),
                "{rows:?}"
            );
            assert!(
                rows.iter()
                    .all(|row| !row.contains("Renamed") && !row.contains("dev/null"))
            );
        }
    }

    #[test]
    fn null_markers_preserve_real_workspace_paths_and_empty_files() {
        let workspace = "/Users/me/project";
        for (old, new, kind, path) in [
            (
                "a/dev/null",
                "b/Users/me/project/empty",
                ProviderChangeKind::Add,
                "empty",
            ),
            (
                "a/Users/me/project/empty",
                "b/dev/null",
                ProviderChangeKind::Delete,
                "empty",
            ),
            (
                "a/dev/null",
                "b/Users/me/project/dev/null",
                ProviderChangeKind::Add,
                "dev/null",
            ),
            (
                "a/Users/me/project/dev/null",
                "b/dev/null",
                ProviderChangeKind::Delete,
                "dev/null",
            ),
            (
                "a/Users/me/project/dev/null",
                "b/Users/me/project/dev/null",
                ProviderChangeKind::Update,
                "dev/null",
            ),
            (
                "a/Users/me/project/old",
                "b/Users/me/project/dev/null",
                ProviderChangeKind::Move,
                "old",
            ),
        ] {
            let source = format!("--- {old}\n+++ {new}\n");
            let changes = from_output(&source, workspace).unwrap();
            assert_eq!(changes.file[0].kind, kind, "{source}");
            assert_eq!(changes.file[0].path, path, "{source}");
            assert_eq!(
                changes.file[0].move_path.as_deref(),
                (kind == ProviderChangeKind::Move).then_some("dev/null")
            );
        }
        let ordinary =
            UnifiedPatch::parse("--- a/old\n+++ b/dev/null\n@@ -1 +1 @@\n-old\n+new\n").unwrap();
        assert_eq!(ordinary.file[0].new_path.as_deref(), Some("dev/null"));
        for source in [
            "--- a/dev/null\n+++ b/dev/null\n",
            "--- a/dev/null\n+++ b/Users/me/project/file\n@@ -1 +1 @@\n-old\n+new\n",
            "--- a/Users/me/project/file\n+++ b/dev/null\n@@ -1 +1 @@\n-old\n+new\n",
        ] {
            assert!(from_output(source, workspace).is_err(), "{source}");
        }
    }

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
