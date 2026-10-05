use std::collections::BTreeMap;

use anyhow::Result;
use forge_diff::source::{Representation, SourcePair, SourceVersion};

use super::{DeclarationDesign, DeclarationFile, PlanDocument};

/// Compares submitted declaration snapshots independently of their review projection.
pub(crate) struct DeclarationDelta {
    pub files: String,
    pub document: String,
}

impl DeclarationDelta {
    /// Use the previous proposal as the baseline, or source declarations for the first submission.
    pub(crate) fn between(previous: Option<&PlanDocument>, current: &PlanDocument) -> Result<Self> {
        let design = current
            .design
            .as_ref()
            .expect("declaration delta requires a design");
        let previous = previous.and_then(|document| document.design.as_ref());
        let baseline = previous
            .map(|design| design.proposed.clone())
            .unwrap_or_else(|| {
                design
                    .baseline
                    .iter()
                    .map(|(path, file)| (path.clone(), file.text.clone()))
                    .collect()
            });
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
            moved,
            ..DeclarationDesign::default()
        };
        let mut patch = Vec::new();
        for file in super::review_file_layout::order(&delta) {
            write_file(
                &file.baseline,
                &file.proposed,
                delta
                    .baseline
                    .get(&file.baseline)
                    .map(|file| file.text.as_str()),
                delta.proposed.get(&file.proposed).map(String::as_str),
                &mut patch,
            )?;
        }
        let before_document = previous
            .map(|design| serde_json::to_string_pretty(&design.document))
            .transpose()?;
        let after_document = serde_json::to_string_pretty(&design.document)?;
        let mut document = Vec::new();
        write_file(
            "plan.json",
            "plan.json",
            before_document.as_deref(),
            Some(&after_document),
            &mut document,
        )?;
        Ok(Self {
            files: String::from_utf8(patch)?,
            document: String::from_utf8(document)?,
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
            before
                .map(str::len)
                .unwrap_or(0)
                .max(after.map(str::len).unwrap_or(0)),
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
        let unchanged = DeclarationDelta::between(Some(&current), &current).unwrap();
        assert!(unchanged.files.is_empty() && unchanged.document.is_empty());
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
