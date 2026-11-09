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
    pub(crate) fn between(
        previous: Option<&PlanDocument>,
        design: &DeclarationDesign,
    ) -> Result<Self> {
        let previous = previous.and_then(|document| document.design.as_ref());
        let previous_parsed = previous.map(DeclarationDesign::parsed).transpose()?;
        let parsed = design.parsed()?;
        let mut baseline = match (previous, previous_parsed) {
            (Some(previous), Some(previous_parsed)) => previous_parsed
                .proposed
                .iter()
                .map(|(path, text)| {
                    let file = DeclarationFile {
                        text: text.clone(),
                        source_digest: String::new(),
                        editable_text: previous
                            .proposed_source
                            .get(path)
                            .cloned()
                            .unwrap_or_default(),
                    };
                    (path.clone(), file)
                })
                .collect::<BTreeMap<_, _>>(),
            _ => design.baseline.clone(),
        };
        let mut baseline_paths = std::collections::BTreeSet::new();
        let mut baseline_calls = previous_parsed
            .map(|previous| previous.proposed_calls.clone())
            .unwrap_or_else(|| design.baseline_calls.clone());
        if let Some(previous) = previous {
            for (path, file) in &design.baseline {
                if !previous.baseline.contains_key(path)
                    && !previous.proposed_source.contains_key(path)
                {
                    baseline.insert(path.clone(), file.clone());
                    if let Some(calls) = design.baseline_calls.get(path) {
                        baseline_calls.insert(path.clone(), calls.clone());
                    }
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
                && design.proposed_source.contains_key(after)
                && !design.proposed_source.contains_key(before)
            {
                moved.insert(before.clone(), after.clone());
            }
        }
        let delta = DeclarationDesign {
            baseline,
            baseline_calls,
            moved,
            proposed_source: design.proposed_source.clone(),
            parsed: Some(super::ParsedDeclarations {
                proposed: parsed.proposed.clone(),
                proposed_calls: parsed.proposed_calls.clone(),
                ..Default::default()
            }),
            ..DeclarationDesign::default()
        };
        let delta_parsed = delta.parsed()?;
        let mut patch = Vec::new();
        for file in super::review_file_layout::order(&delta, &Default::default())? {
            let before = delta
                .baseline
                .get(&file.baseline)
                .map(|source| {
                    super::calls::combined(
                        &file.baseline,
                        &source.text,
                        delta
                            .baseline_calls
                            .get(&file.baseline)
                            .map(Vec::as_slice)
                            .unwrap_or_default(),
                    )
                })
                .transpose()?;
            let after = delta_parsed
                .proposed
                .get(&file.proposed)
                .map(|text| {
                    super::calls::combined(
                        &file.proposed,
                        text,
                        delta_parsed
                            .proposed_calls
                            .get(&file.proposed)
                            .map(Vec::as_slice)
                            .unwrap_or_default(),
                    )
                })
                .transpose()?;
            write_file(
                &file.baseline,
                &file.proposed,
                before.as_deref(),
                after.as_deref(),
                &mut patch,
            )?;
        }
        let mut document = Vec::new();
        let previous_sections = previous
            .map(|design| design.document.sections())
            .unwrap_or_default();
        for section in design.document.sections() {
            let before = previous_sections
                .iter()
                .find(|previous| previous.path == section.path)
                .map(|previous| previous.text.as_str())
                .filter(|text| !text.trim().is_empty());
            let after = (!section.text.trim().is_empty()).then_some(section.text.as_str());
            write_file(section.title, section.title, before, after, &mut document)?;
        }
        Ok(Self {
            files: String::from_utf8(patch)?,
            document: String::from_utf8(document)?,
            baseline_paths,
        })
    }
}

pub(crate) fn write_file(
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
    });
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
