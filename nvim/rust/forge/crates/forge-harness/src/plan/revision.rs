use std::collections::BTreeMap;

use anyhow::Result;
use forge_diff::source::{Representation, SourcePair, SourceVersion};

use super::{DeclarationDesign, DeclarationFile, PlanDocument};

/// Compares submitted declaration snapshots independently of their review projection.
pub(crate) struct DeclarationDelta {
    pub files: String,
    pub document: String,
    pub baseline_paths: std::collections::BTreeSet<String>,
}

impl DeclarationDelta {
    /// Use the previous proposal as the baseline, or source declarations for the first submission.
    pub(crate) fn between(previous: Option<&PlanDocument>, current: &PlanDocument) -> Result<Self> {
        let design = current
            .design
            .as_ref()
            .expect("declaration delta requires a design");
        let previous = previous.and_then(|document| document.design.as_ref());
        let mut baseline = previous
            .map(|design| design.proposed.clone())
            .unwrap_or_else(|| {
                design
                    .baseline
                    .iter()
                    .map(|(path, file)| (path.clone(), file.text.clone()))
                    .collect()
            });
        let mut baseline_paths = std::collections::BTreeSet::new();
        let mut baseline_calls = previous.map(|design| design.proposed_calls.clone()).unwrap_or_else(|| design.baseline_calls.clone());
        if let Some(previous) = previous {
            for (path, file) in &design.baseline {
                if !previous.baseline.contains_key(path) && !previous.proposed.contains_key(path) {
                    baseline.insert(path.clone(), file.text.clone());
                    if let Some(calls) = design.baseline_calls.get(path) { baseline_calls.insert(path.clone(), calls.clone()); }
                    baseline_paths.insert(path.clone());
                }
            }
        }
        let mut moved = BTreeMap::new();
        for original in design.baseline.keys() {
            let before = previous
                .and_then(|design| design.moved.get(original))
                .unwrap_or(original);
            let after = design.moved.get(original).unwrap_or(original);
            if before != after
                && baseline.contains_key(before)
                && design.proposed.contains_key(after)
                && !design.proposed.contains_key(before)
            {
                moved.insert(before.clone(), after.clone());
            }
        }
        let delta = DeclarationDesign {
            baseline: baseline
                .into_iter()
                .map(|(path, text)| {
                    (
                        path,
                        DeclarationFile {
                            text,
                            source_digest: String::new(),
                        },
                    )
                })
                .collect(),
            proposed: design.proposed.clone(),
            baseline_calls,
            proposed_calls: design.proposed_calls.clone(),
            moved,
            ..DeclarationDesign::default()
        };
        let mut patch = Vec::new();
        for file in super::review_file_layout::order(&delta) {
            let before = delta.baseline.get(&file.baseline).map(|source| super::calls::combined(&file.baseline, &source.text, delta.baseline_calls.get(&file.baseline).map(Vec::as_slice).unwrap_or_default())).transpose()?;
            let after = delta.proposed.get(&file.proposed).map(|text| super::calls::combined(&file.proposed, text, delta.proposed_calls.get(&file.proposed).map(Vec::as_slice).unwrap_or_default())).transpose()?;
            write_file(
                &file.baseline,
                &file.proposed,
                before.as_deref(),
                after.as_deref(),
                &mut patch,
            )?;
        }
        let mut document = Vec::new();
        for (section, before, after) in [
            ("Task", previous.map(|design| design.document.task.as_str()), design.document.task.as_str()),
            ("Description", previous.map(|design| design.document.description.as_str()), design.document.description.as_str()),
        ] {
            write_file(section, section, before, Some(after), &mut document)?;
        }
        Ok(Self {
            files: String::from_utf8(patch)?,
            document: String::from_utf8(document)?,
            baseline_paths,
        })
    }
}

fn write_file(
    before_path: &str,
    after_path: &str,
    before: Option<&str>,
    after: Option<&str>,
    patch: &mut Vec<u8>,
) -> Result<()> {
    if before == after && before_path == after_path {
        return Ok(());
    }
    let diff = forge_diff::raw::compute_hunks(SourcePair {
        old: SourceVersion::new(
            before.unwrap_or_default().as_bytes().to_vec(),
            Representation::DisplayOnly,
        )?,
        new: SourceVersion::new(
            after.unwrap_or_default().as_bytes().to_vec(),
            Representation::DisplayOnly,
        )?,
    })?;
    let before_name = format!("a/{before_path}");
    let after_name = format!("b/{after_path}");
    forge_diff::unified::write_file_header(&before_name, &after_name, patch)?;
    if diff.hunks().is_empty() {
        if before.is_none() {
            patch.extend_from_slice(b"new file mode 100644\n");
        } else if after.is_none() {
            patch.extend_from_slice(b"deleted file mode 100644\n");
        }
    } else {
        forge_diff::unified::write_unified(
            &diff,
            if before.is_some() {
                &before_name
            } else {
                "/dev/null"
            },
            if after.is_some() {
                &after_name
            } else {
                "/dev/null"
            },
            3,
            patch,
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_diff::patch::UnifiedPatch;

    #[test]
    fn call_only_revisions_compare_saved_occurrences() {
        let mut previous = super::super::document::test_fixture("calls", "Calls");
        let mut design = DeclarationDesign::default();
        design.proposed.insert("main.rs".into(), "fn run();\n".into());
        design.proposed_calls.insert("main.rs".into(), vec![super::super::FunctionCalls { owner: "run".into(), call: vec![super::super::CallSite { name: "before".into(), source: None, unresolved: false }] }]);
        previous.design = Some(design);
        let mut current = previous.clone();
        current.design.as_mut().unwrap().proposed_calls.get_mut("main.rs").unwrap()[0].call[0].name = "after".into();
        let delta = DeclarationDelta::between(Some(&previous), &current).unwrap();
        assert!(delta.files.contains("-  before") && delta.files.contains("+  after"));
    }

    #[test]
    fn newly_captured_revision_files_compare_against_source_without_restoring_deletions() {
        let mut previous = super::super::document::test_fixture("lazy", "Lazy revision");
        let mut design = DeclarationDesign::default();
        design.baseline.insert("deleted.rs".into(), DeclarationFile { text: "pub struct Deleted;\n".into(), source_digest: String::new() });
        previous.design = Some(design);
        let mut current = previous.clone();
        let design = current.design.as_mut().unwrap();
        design.baseline.insert("Foo.rs".into(), DeclarationFile { text: "pub struct Before;\n".into(), source_digest: String::new() });
        design.proposed.insert("Foo.rs".into(), "pub struct After;\n".into());
        let delta = DeclarationDelta::between(Some(&previous), &current).unwrap();
        assert!(delta.files.contains("-pub struct Before;") && delta.files.contains("+pub struct After;"));
        assert!(!delta.files.contains("deleted.rs") && !delta.files.contains("new file mode"));
        assert_eq!(delta.baseline_paths, ["Foo.rs".into()].into());
    }

    #[test]
    fn declaration_changes_bound_context_and_separate_distant_edits() {
        let before = (1..=60).map(|line| format!("pub struct Item{line};\n")).collect::<String>();
        let after = before.replace("Item11;", "Changed11;").replace("Item41;", "Changed41;");
        let mut output = Vec::new();
        write_file("lib.rs", "lib.rs", Some(&before), Some(&after), &mut output).unwrap();
        let text = String::from_utf8(output).unwrap();
        let patch = UnifiedPatch::parse(&text).unwrap();
        assert_eq!(patch.file[0].hunk.len(), 2);
        assert_eq!(patch.file[0].hunk[0].header, "@@ -8,7 +8,7 @@");
        assert_eq!(patch.file[0].hunk[1].header, "@@ -38,7 +38,7 @@");
        assert!(patch.file[0].hunk.iter().all(|hunk| hunk.row.len() == 8));
        assert!(!text.contains("Item1;") && !text.contains("Item60;"));
    }

    #[test]
    fn revision_delta_compares_proposals_and_keeps_document_changes_separate() {
        let mut previous = super::super::document::test_fixture("revision", "Revision");
        let mut design = DeclarationDesign::default();
        design
            .proposed
            .insert("src/lib.rs".into(), "pub struct Before;\n".into());
        design
            .proposed
            .insert("src/unchanged.rs".into(), "pub struct Stable;\n".into());
        design
            .proposed
            .insert("src/removed.rs".into(), "pub struct Removed;\n".into());
        design.baseline.insert(
            "src/renamed.rs".into(),
            DeclarationFile {
                text: "pub struct Renamed;\n".into(),
                source_digest: String::new(),
            },
        );
        design
            .proposed
            .insert("src/renamed.rs".into(), "pub struct Renamed;\n".into());
        design.document.description = "Previous description".into();
        previous.design = Some(design);
        let mut current = previous.clone();
        let design = current.design.as_mut().unwrap();
        design
            .proposed
            .insert("src/lib.rs".into(), "pub struct After;\n".into());
        design.proposed.remove("src/removed.rs");
        design.proposed.remove("src/renamed.rs");
        design
            .proposed
            .insert("src/new_name.rs".into(), "pub struct Renamed;\n".into());
        design
            .moved
            .insert("src/renamed.rs".into(), "src/new_name.rs".into());
        design
            .proposed
            .insert("src/added.rs".into(), "pub struct Added;\n".into());
        design.document.description = "Revised description".into();
        let delta = DeclarationDelta::between(Some(&previous), &current).unwrap();
        let patch = UnifiedPatch::parse(&delta.files).unwrap();
        assert_eq!(patch.file.len(), 4);
        assert!(!delta.files.contains("unchanged.rs") && !delta.files.contains("Description:"));
        assert!(
            delta.files.contains("-pub struct Before;")
                && delta.files.contains("+pub struct After;")
        );
        assert!(
            patch
                .file
                .iter()
                .any(|file| file.old_path.as_deref() == Some("src/renamed.rs")
                    && file.new_path.as_deref() == Some("src/new_name.rs"))
        );
        assert!(
            delta.document.contains("Previous description")
                && delta.document.contains("Revised description")
        );
        let overview = UnifiedPatch::parse(&delta.document).unwrap();
        assert_eq!(overview.file.len(), 1);
        assert_eq!(overview.file[0].new_path.as_deref(), Some("Description"));
        assert!(!delta.document.contains("plan.json") && !delta.document.contains("\"description\""));
        let unchanged = DeclarationDelta::between(Some(&current), &current).unwrap();
        assert!(unchanged.files.is_empty() && unchanged.document.is_empty());
    }

    #[test]
    fn overview_deltas_preserve_plain_paragraphs_and_separate_changed_sections() {
        let mut previous = super::super::document::test_fixture("overview", "Overview");
        let mut design = DeclarationDesign::default();
        design.document.task = "Original task.".into();
        design.document.description = "First paragraph.\n\nOriginal second paragraph.".into();
        previous.design = Some(design);
        let mut current = previous.clone();
        let design = current.design.as_mut().unwrap();
        design.document.task = "Revised task.".into();
        design.document.description = "First paragraph.\n\nRevised `State` paragraph.".into();
        let delta = DeclarationDelta::between(Some(&previous), &current).unwrap();
        let patch = UnifiedPatch::parse(&delta.document).unwrap();
        assert_eq!(patch.file.len(), 2);
        assert_eq!(patch.file[0].new_path.as_deref(), Some("Task"));
        assert_eq!(patch.file[1].new_path.as_deref(), Some("Description"));
        assert!(delta.document.contains("-Original second paragraph.") && delta.document.contains("+Revised `State` paragraph."));
        assert!(!delta.document.contains("\\n") && !delta.document.contains("\"task\""));
    }

    #[test]
    fn initial_delta_uses_source_and_later_rename_reversal_uses_previous_destination() {
        let mut current = super::super::document::test_fixture("initial", "Initial");
        let mut design = DeclarationDesign::default();
        design.baseline.insert(
            "lib.rs".into(),
            DeclarationFile {
                text: "pub struct Existing;\n".into(),
                source_digest: String::new(),
            },
        );
        design
            .proposed
            .insert("renamed.rs".into(), "pub struct Existing;\n".into());
        design.moved.insert("lib.rs".into(), "renamed.rs".into());
        current.design = Some(design);
        let delta = DeclarationDelta::between(None, &current).unwrap();
        assert!(delta.files.contains("a/lib.rs b/renamed.rs"));
        let mut reverted = current.clone();
        let design = reverted.design.as_mut().unwrap();
        design.moved.clear();
        design.proposed.remove("renamed.rs");
        design
            .proposed
            .insert("lib.rs".into(), "pub struct Existing;\n".into());
        let delta = DeclarationDelta::between(Some(&current), &reverted).unwrap();
        assert!(delta.files.contains("a/renamed.rs b/lib.rs"));
    }
}
