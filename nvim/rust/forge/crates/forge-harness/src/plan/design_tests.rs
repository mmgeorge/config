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
    /// Distinguishes additions, edits, removals, and unchanged coverage.
    pub change: DesignTestChange,
    /// Describes the scenario and expected result, or the reason for removal.
    pub description: String,
}

/// Rejects ambiguous identities and unbounded inventories before a plan edit is committed.
pub(super) fn validate(files: &[DesignTestFile]) -> Result<()> {
    ensure!(files.len() <= 256, "plan tests exceeds 256 files");
    ensure!(files.iter().map(|file| file.cases.len()).sum::<usize>() <= 1024,
        "plan tests exceeds 1024 cases");
    let mut paths = BTreeSet::new();
    for file in files {
        super::design::validate_relative_path(&file.file)?;
        ensure!(file.file.trim() == file.file && !file.file.chars().any(char::is_control),
            "plan test file must have a printable, unpadded path");
        ensure!(paths.insert(&file.file), "duplicate plan test file: {}", file.file);
        ensure!(!file.cases.is_empty(), "plan test file requires at least one case: {}", file.file);
        let mut names = BTreeSet::new();
        for case in &file.cases {
            ensure!(!case.name.is_empty() && case.name.trim() == case.name
                && !case.name.chars().any(char::is_control), "plan test requires a nonempty, single-line name");
            ensure!(names.insert(&case.name), "duplicate plan test: {}: {}", file.file, case.name);
            ensure!(!case.description.trim().is_empty(), "plan test requires a description: {}: {}", file.file, case.name);
        }
    }
    Ok(())
}

/// Summarizes planned changes without presenting execution pass or fail status.
pub(super) fn summary(files: &[DesignTestFile]) -> String {
    let mut counts = [0usize; 4];
    for case in files.iter().flat_map(|file| &file.cases) {
        counts[match case.change {
            DesignTestChange::New => 0, DesignTestChange::Modified => 1,
            DesignTestChange::Removed => 2, DesignTestChange::Reused => 3,
        }] += 1;
    }
    let parts = counts.into_iter().zip(["new", "modified", "removed", "reused"])
        .filter(|(count, _)| *count > 0).map(|(count, label)| format!("{count} {label}"))
        .collect::<Vec<_>>();
    if parts.is_empty() { "None planned".into() } else { parts.join(" · ") }
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
