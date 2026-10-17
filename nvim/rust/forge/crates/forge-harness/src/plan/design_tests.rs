use std::collections::BTreeSet;

use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Identifies the planned change to a test, independently of execution results.
pub enum DesignTestChange {
    New,
    Modified,
    Removed,
    Reused,
}

impl DesignTestChange {
    /// Supplies the review marker and shared foreground highlight.
    pub(super) fn presentation(self) -> (&'static str, &'static str) {
        match self {
            Self::New => ("+ ", "ForgeStatusFileNew"),
            Self::Modified => ("~ ", "ForgeStatusFileModified"),
            Self::Removed => ("− ", "ForgeStatusFileDeleted"),
            Self::Reused => ("", "Normal"),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Groups planned tests by source file, including inline test modules.
pub struct DesignTestFile {
    /// Names a project-relative file with forward slash separators.
    pub file: String,
    /// Lists each changed or reused test once within this file.
    pub cases: Vec<DesignTestCase>,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Specifies a named test and its observable contract without requiring source parsing.
pub struct DesignTestCase {
    /// Includes the module qualification when needed to identify the test.
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Optional runner target used when a name belongs to multiple executables.
    pub target: Option<String>,
    /// Distinguishes additions, edits, removals, and unchanged coverage.
    pub change: DesignTestChange,
    /// Describes the scenario and expected result, or the reason for removal.
    pub description: String,
}

#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(deny_unknown_fields)]
/// Identifies one inventory entry independently of its rendered row or array index.
pub(crate) struct DesignTestSelection {
    pub file: String,
    pub name: String,
}

/// Remove exactly the reviewed entries, rejecting stale or ambiguous selections before mutation.
pub(crate) fn remove(files: &mut Vec<DesignTestFile>, selected: &[DesignTestSelection]) -> Result<()> {
    ensure!(!selected.is_empty(), "select at least one planned test");
    let selection: BTreeSet<_> = selected.iter().collect();
    ensure!(selection.len() == selected.len(), "duplicate selected test");
    for test in &selection {
        ensure!(files.iter().any(|file| file.file == test.file && file.cases.iter().any(|case| case.name == test.name)),
            "selected test no longer exists: {}: {}", test.file, test.name);
    }
    for file in files.iter_mut() {
        file.cases.retain(|case| !selection.contains(&DesignTestSelection { file: file.file.clone(), name: case.name.clone() }));
    }
    files.retain(|file| !file.cases.is_empty());
    Ok(())
}

/// Validate unique file and test identities before committing a plan edit.
pub(super) fn validate(files: &[DesignTestFile]) -> Result<()> {
    let mut paths = BTreeSet::new();
    for file in files {
        super::design::validate_relative_path(&file.file)?;
        ensure!(file.file.trim() == file.file && !file.file.chars().any(char::is_control),
            "plan test file must have a printable, unpadded path");
        ensure!(paths.insert(&file.file), "duplicate plan test file: {}", file.file);
        ensure!(!file.cases.is_empty(), "plan test file requires at least one case: {}", file.file);
        let mut names = BTreeSet::new();
        for case in &file.cases {
            ensure!(case.target.as_ref().is_none_or(|target| !target.is_empty() && target.len() <= 256 && !target.chars().any(char::is_control)), "invalid planned test target");
            ensure!(!case.name.is_empty() && case.name.trim() == case.name
                && !case.name.chars().any(char::is_control), "plan test requires a nonempty, single-line name");
            ensure!(names.insert(&case.name), "duplicate plan test: {}: {}", file.file, case.name);
            ensure!(!case.description.trim().is_empty(), "plan test requires a description: {}: {}", file.file, case.name);
        }
    }
    Ok(())
}

/// Summarizes planned changes with foreground colors independent of the section label.
pub(super) fn summary(files: &[DesignTestFile]) -> Vec<forge_buffer::block::TextChunk> {
    let mut counts = [0usize; 4];
    for case in files.iter().flat_map(|file| &file.cases) {
        counts[match case.change {
            DesignTestChange::New => 0, DesignTestChange::Modified => 1,
            DesignTestChange::Removed => 2, DesignTestChange::Reused => 3,
        }] += 1;
    }
    let mut chunks = Vec::new();
    for ((count, label), capture) in counts.into_iter().zip(["new", "modified", "removed", "reused"])
        .zip(["ForgeAddRange", "ForgeModifyRange", "ForgeDeleteRange", "Comment"]) {
        if count == 0 { continue; }
        if !chunks.is_empty() {
            chunks.push(forge_buffer::block::TextChunk { text: " · ".into(), capture: "Comment".into() });
        }
        chunks.push(forge_buffer::block::TextChunk { text: format!("{count} {label}"), capture: capture.into() });
    }
    if chunks.is_empty() {
        chunks.push(forge_buffer::block::TextChunk { text: "None planned".into(), capture: "Comment".into() });
    }
    chunks
}

/// Projects the complete inventory for section reads and revision diffs.
pub(super) fn text(files: &[DesignTestFile]) -> String {
    if files.is_empty() { return "None planned".into(); }
    files.iter().map(|file| {
        let cases = file.cases.iter().map(|case| format!("  {}{}\n    {}",
            case.change.presentation().0, case.name, case.description.replace('\n', "\n    ")))
            .collect::<Vec<_>>().join("\n");
        format!("{}\n{cases}", file.file)
    }).collect::<Vec<_>>().join("\n\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complete_inventories_and_selections_preserve_every_file_and_case() {
        let mut files = (0..257).map(|index| DesignTestFile {
            file: format!("tests/source_{index}.rs"),
            cases: (0..5).map(|case| DesignTestCase {
                name: format!("case_{case}"), target: None,
                change: DesignTestChange::New, description: "Preserve complete coverage.".into(),
            }).collect(),
        }).collect::<Vec<_>>();
        validate(&files).unwrap();
        let selected = files.iter().flat_map(|file| file.cases.iter().map(|case| DesignTestSelection {
            file: file.file.clone(), name: case.name.clone(),
        })).collect::<Vec<_>>();
        assert_eq!(selected.len(), 1285);
        remove(&mut files, &selected).unwrap();
        assert!(files.is_empty());
    }
}
