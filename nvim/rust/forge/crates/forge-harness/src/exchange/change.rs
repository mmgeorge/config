use crate::backend::{ProviderChangeKind, ProviderFileChange};
use crate::exchange::{Exchange, ToolCall};
use std::collections::BTreeSet;

mod net;

/// Stores normalized paths from successful provider-owned file changes.
#[derive(Default)]
pub struct ProviderChangeIndex {
    path_set: BTreeSet<String>,
}

impl ProviderChangeIndex {
    /// Merge successful provider file changes from one interaction timeline.
    pub fn record(&mut self, interaction: &Exchange) {
        for turn in &interaction.turn {
            for tool in turn.tools() {
                self.record_tool_list(std::slice::from_ref(tool));
            }
        }
    }

    fn record_tool_list(&mut self, tool_list: &[ToolCall]) {
        for tool in tool_list {
            if !successful_file_change(tool) {
                continue;
            }
            for change in &tool.change.file {
                self.path_set.insert(normalize_path(&change.path));
                if let Some(move_path) = change.move_path.as_deref() {
                    self.path_set.insert(normalize_path(move_path));
                }
            }
        }
    }

    /// Resolve normalized paths attributed to successful provider changes.
    pub fn paths(&self) -> &BTreeSet<String> {
        &self.path_set
    }

    /// Validate whether the provider attributed any path to this interaction.
    pub fn is_empty(&self) -> bool {
        self.path_set.is_empty()
    }
}

#[derive(Default)]
struct FileProjection {
    old_path: Option<String>,
    new_path: Option<String>,
    hunk_list: Vec<String>,
}

#[derive(Default)]
/// Accumulates reported edit operations without claiming a net workspace comparison.
pub struct ProviderDiffBuilder {
    projection: Vec<FileProjection>,
}

impl ProviderDiffBuilder {
    /// Build a unified operation diff from successful provider file-change tools.
    pub fn build(tool_list: &[ToolCall]) -> Option<String> {
        let mut builder = Self::default();
        builder.record_tools(tool_list.iter());
        builder.finish()
    }

    /// Compose adjacent successful writes into one diff per file.
    /// Inconsistent reports retain all operation hunks and include an explicit diagnostic.
    pub fn build_group(tool_list: &[&ToolCall]) -> (Option<String>, Option<String>) {
        let mut builder = Self::default();
        builder.record_tools(tool_list.iter().copied());
        let mut warning = Vec::new();
        for projection in &mut builder.projection {
            match net::compose(&projection.hunk_list) {
                Ok(hunks) => projection.hunk_list = hunks,
                Err(error) => warning.push(format!(
                    "{}: {error}",
                    projection
                        .new_path
                        .as_deref()
                        .or(projection.old_path.as_deref())
                        .unwrap_or("unknown")
                )),
            }
        }
        let warning = (!warning.is_empty()).then(|| {
            format!(
                "reported edits retained, net diff unavailable: {}",
                warning.join(", ")
            )
        });
        (builder.finish(), warning)
    }

    /// Include successful edits from each provider turn in an exchange.
    pub fn record(&mut self, exchange: &Exchange) {
        self.record_tools(exchange.turn.iter().flat_map(|turn| turn.tools()));
    }

    fn record_tools<'tool>(&mut self, tool_list: impl Iterator<Item = &'tool ToolCall>) {
        for tool in tool_list {
            if !successful_file_change(tool) {
                continue;
            }
            for change in &tool.change.file {
                merge_change(&mut self.projection, change);
            }
        }
    }

    /// Emit one file section per reported path, preserving successive edit hunks.
    pub fn finish(self) -> Option<String> {
        let mut output = String::new();
        for projection in self.projection {
            render_projection(&mut output, projection);
        }
        (!output.is_empty()).then_some(output)
    }
}

fn successful_file_change(tool: &ToolCall) -> bool {
    tool.kind == "file_change" && !tool.failed && successful_status(&tool.status)
}

fn successful_status(status: &str) -> bool {
    matches!(
        status.to_ascii_lowercase().as_str(),
        "completed" | "complete" | "success" | "succeeded"
    )
}

fn merge_change(projection_list: &mut Vec<FileProjection>, change: &ProviderFileChange) {
    let path = normalize_path(&change.path);
    let move_path = change.move_path.as_deref().map(normalize_path);
    let projection_index = projection_list.iter().position(|projection| {
        projection.new_path.as_deref() == Some(path.as_str())
            || projection.old_path.as_deref() == Some(path.as_str())
    });
    let projection = match projection_index {
        Some(index) => &mut projection_list[index],
        None => {
            let (old_path, new_path) = operation_path(change.kind, &path, move_path.as_deref());
            projection_list.push(FileProjection {
                old_path,
                new_path,
                hunk_list: Vec::new(),
            });
            projection_list.last_mut().expect("projection was appended")
        }
    };
    if projection_index.is_some() {
        projection.new_path = match change.kind {
            ProviderChangeKind::Delete => None,
            ProviderChangeKind::Move => move_path.or_else(|| Some(path.clone())),
            ProviderChangeKind::Add | ProviderChangeKind::Update => Some(path.clone()),
        };
    }
    projection.hunk_list.extend(operation_hunk_list(
        change.kind,
        ProviderFileChange::split_move_report(change.kind, &change.diff).0,
    ));
}

fn operation_path(
    kind: ProviderChangeKind,
    path: &str,
    move_path: Option<&str>,
) -> (Option<String>, Option<String>) {
    match kind {
        ProviderChangeKind::Add => (None, Some(path.to_owned())),
        ProviderChangeKind::Delete => (Some(path.to_owned()), None),
        ProviderChangeKind::Update => (Some(path.to_owned()), Some(path.to_owned())),
        ProviderChangeKind::Move => (
            Some(path.to_owned()),
            Some(move_path.unwrap_or(path).to_owned()),
        ),
    }
}

fn normalize_path(path: &str) -> String {
    path.replace('\\', "/")
        .trim_start_matches("a/")
        .trim_start_matches("b/")
        .to_owned()
}

fn extract_hunk_list(diff: &str) -> Vec<String> {
    let mut hunk_list = Vec::new();
    let mut current = Vec::new();
    for line in diff.split_terminator('\n') {
        if line.starts_with("@@ ") {
            if !current.is_empty() {
                hunk_list.push(current.join("\n"));
                current.clear();
            }
            current.push(line.to_owned());
        } else if !current.is_empty() && !line.starts_with("diff --git ") {
            current.push(line.to_owned());
        }
    }
    if !current.is_empty() {
        hunk_list.push(current.join("\n"));
    }
    hunk_list
}

fn operation_hunk_list(kind: ProviderChangeKind, diff: &str) -> Vec<String> {
    let hunk_list = extract_hunk_list(diff);
    if !hunk_list.is_empty() || diff.is_empty() {
        return hunk_list;
    }
    let line_list: Vec<_> = diff.lines().collect();
    if line_list.is_empty() {
        return Vec::new();
    }
    let line_count = line_list.len();
    match kind {
        ProviderChangeKind::Add => Some(format!(
            "@@ -0,0 +1,{line_count} @@\n{}",
            line_list
                .iter()
                .map(|line| format!("+{line}"))
                .collect::<Vec<_>>()
                .join("\n")
        )),
        ProviderChangeKind::Delete => Some(format!(
            "@@ -1,{line_count} +0,0 @@\n{}",
            line_list
                .iter()
                .map(|line| format!("-{line}"))
                .collect::<Vec<_>>()
                .join("\n")
        )),
        ProviderChangeKind::Update | ProviderChangeKind::Move => None,
    }
    .into_iter()
    .collect()
}

fn render_projection(output: &mut String, projection: FileProjection) {
    let Some(label_path) = projection
        .new_path
        .as_deref()
        .or(projection.old_path.as_deref())
    else {
        return;
    };
    let old_path = projection.old_path.as_deref().unwrap_or("/dev/null");
    let new_path = projection.new_path.as_deref().unwrap_or("/dev/null");
    let git_old = projection.old_path.as_deref().unwrap_or(label_path);
    let git_new = projection.new_path.as_deref().unwrap_or(label_path);
    output.push_str(&format!("diff --git a/{git_old} b/{git_new}\n"));
    if old_path == "/dev/null" {
        output.push_str("--- /dev/null\n");
    } else {
        output.push_str(&format!("--- a/{old_path}\n"));
    }
    if new_path == "/dev/null" {
        output.push_str("+++ /dev/null\n");
    } else {
        output.push_str(&format!("+++ b/{new_path}\n"));
    }
    for hunk in projection.hunk_list {
        output.push_str(&hunk);
        output.push('\n');
    }
}

#[cfg(test)]
mod test {
    use super::{ProviderChangeIndex, ProviderDiffBuilder};
    use crate::backend::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
    use crate::exchange::ToolCall;

    fn file_tool(id: &str, status: &str, file: Vec<ProviderFileChange>) -> ToolCall {
        ToolCall {
            started_at_ms: None,
            completed_at_ms: None,
            task_id: None,
            id: id.into(),
            kind: "file_change".into(),
            title: "file changes".into(),
            output: String::new(),
            status: status.into(),
            failed: status != "completed",
            change: ProviderChangeSet { file },
        }
    }

    #[test]
    fn saved_move_annotations_do_not_enter_patch_hunks() {
        let patch = "@@ -1 +1 @@\n-before\n+Moved to: source text\n";
        let report = format!("{patch}\n\nMoved to: D:/workspace/src/moved.rs");
        let tool = file_tool(
            "move",
            "completed",
            vec![ProviderFileChange {
                path: "src/original.rs".into(),
                move_path: None,
                kind: ProviderChangeKind::Update,
                diff: report,
            }],
        );
        let diff = ProviderDiffBuilder::build(&[tool]).unwrap();
        let parsed = forge_diff::patch::UnifiedPatch::parse(&diff).unwrap();
        assert_eq!(parsed.file[0].hunk[0].row[1].text, "Moved to: source text");
        assert!(!diff.contains("D:/workspace/src/moved.rs"));
    }

    #[test]
    fn merges_repeated_provider_edits_into_one_file_block() {
        let tool_list = vec![
            file_tool(
                "one",
                "completed",
                vec![ProviderFileChange {
                    path: "src/main.rs".into(),
                    move_path: None,
                    kind: ProviderChangeKind::Update,
                    diff: "@@ -1 +1 @@\n-old\n+middle".into(),
                }],
            ),
            file_tool(
                "two",
                "completed",
                vec![ProviderFileChange {
                    path: "src/main.rs".into(),
                    move_path: None,
                    kind: ProviderChangeKind::Update,
                    diff: "@@ -1 +1 @@\n-middle\n+new".into(),
                }],
            ),
        ];

        let diff = ProviderDiffBuilder::build(&tool_list).expect("provider diff");

        assert_eq!(diff.matches("diff --git").count(), 1);
        assert!(diff.contains("-old\n+middle"));
        assert!(diff.contains("-middle\n+new"));
    }

    #[test]
    fn excludes_failed_and_non_file_tools() {
        let failed = file_tool(
            "failed",
            "failed",
            vec![ProviderFileChange {
                path: "failed.rs".into(),
                move_path: None,
                kind: ProviderChangeKind::Add,
                diff: "@@ -0,0 +1 @@\n+failed".into(),
            }],
        );
        let command = ToolCall {
            started_at_ms: None,
            completed_at_ms: None,
            task_id: None,
            id: "command".into(),
            kind: "command".into(),
            title: "cargo fmt".into(),
            output: String::new(),
            status: "completed".into(),
            failed: false,
            change: ProviderChangeSet::default(),
        };

        assert!(ProviderDiffBuilder::build(&[failed, command]).is_none());
    }

    #[test]
    fn indexes_successful_provider_paths_and_move_destinations() {
        let completed = file_tool(
            "completed",
            "completed",
            vec![
                ProviderFileChange {
                    path: "a/src\\main.rs".into(),
                    move_path: None,
                    kind: ProviderChangeKind::Update,
                    diff: "@@ -1 +1 @@\n-old\n+new".into(),
                },
                ProviderFileChange {
                    path: "b/src/old.rs".into(),
                    move_path: Some("b/src/new.rs".into()),
                    kind: ProviderChangeKind::Move,
                    diff: String::new(),
                },
            ],
        );
        let failed = file_tool(
            "failed",
            "failed",
            vec![ProviderFileChange {
                path: "failed.rs".into(),
                move_path: None,
                kind: ProviderChangeKind::Add,
                diff: "failed".into(),
            }],
        );
        let mut index = ProviderChangeIndex::default();

        index.record_tool_list(&[completed, failed]);

        assert_eq!(
            index.paths().iter().map(String::as_str).collect::<Vec<_>>(),
            vec!["src/main.rs", "src/new.rs", "src/old.rs"]
        );
    }

    #[test]
    fn transient_files_do_not_emit_pathless_patches() {
        let mut tools = Vec::new();
        for (id, kind, path, move_path, diff) in [
            (
                "create",
                ProviderChangeKind::Add,
                "temporary.lua",
                None,
                "@@ -0,0 +1 @@\n+first",
            ),
            (
                "edit",
                ProviderChangeKind::Update,
                "temporary.lua",
                None,
                "@@ -1 +1 @@\n-first\n+second",
            ),
            (
                "move",
                ProviderChangeKind::Move,
                "temporary.lua",
                Some("renamed.lua"),
                "",
            ),
            (
                "remove",
                ProviderChangeKind::Delete,
                "renamed.lua",
                None,
                "@@ -1 +0,0 @@\n-second",
            ),
        ] {
            tools.push(file_tool(
                id,
                "completed",
                vec![ProviderFileChange {
                    path: path.into(),
                    move_path: move_path.map(str::to_owned),
                    kind,
                    diff: diff.into(),
                }],
            ));
        }
        let (diff, warning) = ProviderDiffBuilder::build_group(&tools.iter().collect::<Vec<_>>());
        assert!(diff.is_none());
        assert!(warning.is_none());
        assert!(ProviderDiffBuilder::build(&tools).is_none());

        tools.push(file_tool(
            "persist",
            "completed",
            vec![ProviderFileChange {
                path: "existing.lua".into(),
                move_path: None,
                kind: ProviderChangeKind::Update,
                diff: "@@ -1 +1 @@\n-before\n+after".into(),
            }],
        ));
        for diff in [
            ProviderDiffBuilder::build(&tools),
            ProviderDiffBuilder::build_group(&tools.iter().collect::<Vec<_>>()).0,
        ] {
            let diff = diff.expect("persistent edit");
            let patch = forge_diff::patch::UnifiedPatch::parse(&diff).unwrap();
            assert_eq!(patch.file.len(), 1);
            assert_eq!(patch.file[0].new_path.as_deref(), Some("existing.lua"));
            assert_eq!(patch.file[0].hunk[0].row[1].text, "after");
        }

        tools.push(file_tool(
            "recreate",
            "completed",
            vec![ProviderFileChange {
                path: "temporary.lua".into(),
                move_path: None,
                kind: ProviderChangeKind::Add,
                diff: "@@ -0,0 +1 @@\n+third".into(),
            }],
        ));
        let (diff, warning) = ProviderDiffBuilder::build_group(&tools.iter().collect::<Vec<_>>());
        assert!(warning.is_none());
        let diff = diff.unwrap();
        let patch = forge_diff::patch::UnifiedPatch::parse(&diff).unwrap();
        assert_eq!(patch.file.len(), 2);
        assert_eq!(patch.file[1].old_path, None);
        assert_eq!(patch.file[1].new_path.as_deref(), Some("temporary.lua"));
        assert_eq!(patch.file[1].hunk[0].row[0].text, "third");
    }

    #[test]
    fn preserves_file_order_and_operation_headers() {
        let tool = file_tool(
            "changes",
            "completed",
            vec![
                ProviderFileChange {
                    path: "src/new.rs".into(),
                    move_path: None,
                    kind: ProviderChangeKind::Add,
                    diff: "new\n".into(),
                },
                ProviderFileChange {
                    path: "src/old.rs".into(),
                    move_path: None,
                    kind: ProviderChangeKind::Delete,
                    diff: "@@ -1 +0,0 @@\n-old".into(),
                },
                ProviderFileChange {
                    path: "src/from.rs".into(),
                    move_path: Some("src/to.rs".into()),
                    kind: ProviderChangeKind::Move,
                    diff: "@@ -1 +1 @@\n-before\n+after".into(),
                },
            ],
        );

        let diff = ProviderDiffBuilder::build(&[tool]).expect("provider diff");

        let add_index = diff.find("diff --git a/src/new.rs").unwrap();
        let delete_index = diff.find("diff --git a/src/old.rs").unwrap();
        let move_index = diff.find("diff --git a/src/from.rs b/src/to.rs").unwrap();
        assert!(add_index < delete_index && delete_index < move_index);
        assert!(diff.contains("--- /dev/null\n+++ b/src/new.rs"));
        assert!(diff.contains("@@ -0,0 +1,1 @@\n+new"));
        assert!(diff.contains("--- a/src/old.rs\n+++ /dev/null"));
    }
}
