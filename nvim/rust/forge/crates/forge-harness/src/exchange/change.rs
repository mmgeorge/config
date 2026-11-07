use crate::exchange::{Exchange, ToolCall};
use crate::provider::{ProviderChangeKind, ProviderFileChange};
use std::collections::BTreeSet;

/// Stores normalized paths from successful provider-owned file changes.
#[derive(Default)]
pub struct ProviderChangeIndex {
    path_set: BTreeSet<String>,
}

impl ProviderChangeIndex {
    /// Merge successful provider file changes from one interaction timeline.
    pub fn record(&mut self, interaction: &Exchange) {
        for effect in &interaction.host_file_changes {
            for change in &effect.changes.file {
                self.path_set.insert(normalize_path(&change.path));
                if let Some(path) = &change.move_path {
                    self.path_set.insert(normalize_path(path));
                }
            }
        }
        for turn in &interaction.turn {
            for tool in interaction.tools_for_turn(turn.id()) {
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

    /// Render actual committed host operations separately from native tool snapshots.
    pub(crate) fn build_changes(changes: &crate::provider::ProviderChangeSet) -> Option<String> {
        let mut builder = Self::default();
        for change in &changes.file {
            merge_change(&mut builder.projection, change);
        }
        builder.finish()
    }

    /// Include successful edits from each provider turn in an exchange.
    pub fn record(&mut self, exchange: &Exchange) {
        self.record_tools(exchange.tools().map(|(_, tool)| tool));
        for effect in &exchange.host_file_changes {
            for change in &effect.changes.file {
                merge_change(&mut self.projection, change);
            }
        }
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
    tool.kind == "file_change" && !tool.failed && tool.state() == crate::turn::ToolState::Completed
}

fn merge_change(projection_list: &mut Vec<FileProjection>, change: &ProviderFileChange) {
    let path = normalize_path(&change.path);
    let move_path = change.move_path.as_deref().map(normalize_path);
    let hunk_list = operation_hunk_list(change.kind, &change.diff);
    if let Some(projection) = projection_list.iter_mut().find(|projection| {
        projection.new_path.as_deref() == Some(path.as_str())
            || projection.old_path.as_deref() == Some(path.as_str())
    }) {
        projection.new_path = match change.kind {
            ProviderChangeKind::Delete => None,
            ProviderChangeKind::Move => move_path.or_else(|| Some(path.clone())),
            ProviderChangeKind::Add | ProviderChangeKind::Update => Some(path.clone()),
        };
        projection.hunk_list.extend(hunk_list);
    } else {
        let (old_path, new_path) = operation_path(change.kind, &path, move_path.as_deref());
        projection_list.push(FileProjection {
            old_path,
            new_path,
            hunk_list,
        });
    }
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
mod test {}

#[cfg(test)]
mod host_effect_tests {}
