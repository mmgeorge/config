use crate::backend::{ProviderChangeKind, ProviderFileChange};
use crate::interaction::{CompletedTool, InteractionNode, InteractionRecord};
use std::collections::BTreeSet;

/// Stores normalized paths from successful provider-owned file changes.
#[derive(Default)]
pub struct ProviderChangeIndex {
    path_set: BTreeSet<String>,
}

impl ProviderChangeIndex {
    /// Merge successful provider file changes from one interaction timeline.
    pub fn record(&mut self, interaction: &InteractionRecord) {
        for node in &interaction.node_list {
            let InteractionNode::MainSegment { segment } = node else {
                continue;
            };
            for thought in &segment.thought {
                self.record_tool_list(&thought.tool);
            }
        }
    }

    fn record_tool_list(&mut self, tool_list: &[CompletedTool]) {
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

/// Builds one immutable provider-owned diff for a completed thought.
pub struct ProviderDiffBuilder;

impl ProviderDiffBuilder {
    /// Build a unified operation diff from successful provider file-change tools.
    pub fn build(tool_list: &[CompletedTool]) -> Option<String> {
        let mut projection_list = Vec::<FileProjection>::new();
        for tool in tool_list {
            if !successful_file_change(tool) {
                continue;
            }
            for change in &tool.change.file {
                merge_change(&mut projection_list, change);
            }
        }
        let mut output = String::new();
        for projection in projection_list {
            render_projection(&mut output, projection);
        }
        (!output.is_empty()).then_some(output)
    }
}

fn successful_file_change(tool: &CompletedTool) -> bool {
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
    projection
        .hunk_list
        .extend(operation_hunk_list(change.kind, &change.diff));
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
    for line in diff.lines() {
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
    let old_path = projection.old_path.as_deref().unwrap_or("/dev/null");
    let new_path = projection.new_path.as_deref().unwrap_or("/dev/null");
    let label_path = projection
        .new_path
        .as_deref()
        .or(projection.old_path.as_deref())
        .unwrap_or("unknown");
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
    use crate::interaction::CompletedTool;

    fn file_tool(id: &str, status: &str, file: Vec<ProviderFileChange>) -> CompletedTool {
        CompletedTool {
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
        let command = CompletedTool {
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
