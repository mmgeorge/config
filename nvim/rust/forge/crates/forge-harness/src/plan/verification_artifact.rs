//! Holds the Verify report the agent writes in `plan:verification.md` and the evidence list in
//! `plan:evidence.md`.
//!
//! The report is stored as the agent wrote it, and `plan_phase_done` parses it, reporting every
//! problem at its line. Each manual check gets one `N. status [IDs] note` row, and each defect one
//! `path:line:column: message [IDs]` finding, so Resolve receives locations rather than prose.
//! Harness derives the overall result from the rows and findings instead of asking for it.

use anyhow::Result;
use std::fmt::Write;
use std::path::Path;

use super::execution::{ManualCheckResult, VerificationReport, VerificationStatus, evidence_alias};
use crate::declaration::{DeclarationDiagnosticKind, DeclarationValidationError};

const PATH: &str = "plan:verification.md";

/// Describes one completed tool result the report may cite.
pub(crate) struct VerificationEvidenceEntry {
    /// Full evidence ID, shown through its short alias.
    pub(crate) id: String,
    pub(crate) title: String,
    pub(crate) state: String,
    pub(crate) output_preview: String,
}

/// Render the report template with one pending row per accepted manual check.
pub(crate) fn template(manual_checks: &[String]) -> String {
    let mut text = String::from(
        "# Verification\n\nReport each manual check as `N. status [IDs] note`, where status is \
         passed, failed, or blocked, and the IDs come from plan:evidence.md. A blocked note states \
         the cause. List each defect under Findings as `path:line:column: message [IDs]`, with `-` \
         as the path when no file applies. Explain reused evidence under Reuse reason.\n\n\
         ## Results\n",
    );
    for (index, check) in manual_checks.iter().enumerate() {
        text.push('\n');
        for line in check.lines() {
            let _ = writeln!(text, "> {line}");
        }
        let _ = writeln!(text, "{}. pending []", index + 1);
    }
    text.push_str("\n## Findings\n\n## Reuse reason\n");
    text
}

/// Render the citable evidence, newest first, with each entry's short alias.
pub(crate) fn evidence_text(evidence: &[VerificationEvidenceEntry]) -> String {
    let mut text = String::from("# Evidence\n\n");
    if evidence.is_empty() {
        text.push_str("No completed tool results yet.\n");
    }
    for entry in evidence {
        let _ = writeln!(
            text,
            "{}  {} · {}",
            evidence_alias(&entry.id),
            entry.title,
            entry.state
        );
        for line in entry
            .output_preview
            .lines()
            .filter(|line| !line.trim().is_empty())
        {
            let _ = writeln!(text, "  > {line}");
        }
    }
    text
}

/// Names the report section a parsed line belongs to.
#[derive(Clone, Copy)]
enum Section {
    Preamble,
    Results,
    Findings,
    Reuse,
}

/// Parse the report into a submitted assessment, or fail with every problem at its line.
///
/// `evidence_ids` are the full IDs this execution may cite; a citation may use either a full ID
/// or its alias, and the report keeps full IDs. Finding paths must name workspace files.
pub(crate) fn parse(
    text: &str,
    manual_checks: &[String],
    evidence_ids: &[&str],
    workspace: &Path,
) -> Result<VerificationReport> {
    let mut diagnostic = Vec::new();
    let mut error = |line: usize, reference: &str, reason: String| {
        diagnostic.push(super::artifact_diagnostic(
            PATH,
            line,
            DeclarationDiagnosticKind::VerificationReport,
            reference,
            reason,
        ));
    };
    let mut section = Section::Preamble;
    let mut rows: Vec<Option<ManualCheckResult>> = vec![None; manual_checks.len()];
    let mut findings = Vec::new();
    let mut finding_observed_in = Vec::new();
    let mut reuse = Vec::new();
    let mut seen = Vec::new();
    for (index, line) in text.lines().enumerate() {
        let number = index + 1;
        if let Some(heading) = line.strip_prefix("## ") {
            let heading = heading.trim();
            section = match heading {
                "Results" => Section::Results,
                "Findings" => Section::Findings,
                "Reuse reason" => Section::Reuse,
                _ => {
                    error(
                        number,
                        &format!("## {heading}"),
                        "unknown section; sections are Results, Findings, and Reuse reason".into(),
                    );
                    Section::Preamble
                }
            };
            if !matches!(section, Section::Preamble) {
                if seen.contains(&heading.to_owned()) {
                    error(
                        number,
                        &format!("## {heading}"),
                        "the section appears twice".into(),
                    );
                }
                seen.push(heading.to_owned());
            }
            continue;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('>') {
            continue;
        }
        match section {
            Section::Preamble => {}
            Section::Reuse => reuse.push(trimmed),
            Section::Results => {
                let Some((ordinal, rest)) = trimmed
                    .split_once(". ")
                    .and_then(|(ordinal, rest)| Some((ordinal.parse::<usize>().ok()?, rest)))
                else {
                    error(
                        number,
                        trimmed,
                        "results are `N. status [IDs] note` rows".into(),
                    );
                    continue;
                };
                let Some(slot) = ordinal.checked_sub(1).and_then(|index| rows.get_mut(index))
                else {
                    error(
                        number,
                        &ordinal.to_string(),
                        format!(
                            "the plan has {} manual checks; number rows 1 through {}",
                            manual_checks.len(),
                            manual_checks.len()
                        ),
                    );
                    continue;
                };
                let (status, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                let row_status = match status {
                    "passed" => VerificationStatus::Passed,
                    "failed" => VerificationStatus::Failed,
                    "blocked" => VerificationStatus::Blocked,
                    "pending" => {
                        error(
                            number,
                            status,
                            "perform the check and replace pending with passed, failed, or blocked"
                                .into(),
                        );
                        continue;
                    }
                    _ => {
                        error(
                            number,
                            status,
                            "the status must be passed, failed, or blocked".into(),
                        );
                        continue;
                    }
                };
                let Some((cited, note)) = citations(rest) else {
                    error(
                        number,
                        rest,
                        "follow the status with `[IDs]`, then the note".into(),
                    );
                    continue;
                };
                let mut observed_in = Vec::new();
                for reference in cited {
                    match resolve(reference, evidence_ids) {
                        Ok(id) => observed_in.push(id),
                        Err(reason) => error(number, reference, reason),
                    }
                }
                if note.is_empty() {
                    error(
                        number,
                        status,
                        "the row needs a note describing what was observed".into(),
                    );
                }
                if row_status == VerificationStatus::Passed && observed_in.is_empty() {
                    error(
                        number,
                        status,
                        "a passed check cites the evidence that shows it passing".into(),
                    );
                }
                if slot.is_some() {
                    error(
                        number,
                        &ordinal.to_string(),
                        "the check has two result rows".into(),
                    );
                    continue;
                }
                *slot = Some(ManualCheckResult {
                    label: manual_checks[ordinal - 1].clone(),
                    status: row_status,
                    note: note.to_owned(),
                    observed_in,
                });
            }
            Section::Findings => {
                let (body, cited) = match trimmed.rfind('[') {
                    Some(start) if trimmed.ends_with(']') => (
                        trimmed[..start].trim_end(),
                        trimmed[start + 1..trimmed.len() - 1]
                            .split([',', ' '])
                            .filter(|reference| !reference.is_empty())
                            .collect::<Vec<_>>(),
                    ),
                    _ => (trimmed, Vec::new()),
                };
                match finding(body, workspace) {
                    Ok(location) => {
                        findings.push(location);
                        for reference in cited {
                            match resolve(reference, evidence_ids) {
                                Ok(id) => finding_observed_in.push(id),
                                Err(reason) => error(number, reference, reason),
                            }
                        }
                    }
                    Err(reason) => error(number, body, reason),
                }
            }
        }
    }
    let mut results = Vec::new();
    for (index, row) in rows.into_iter().enumerate() {
        match row {
            Some(result) => results.push(result),
            None => error(
                1,
                &format!("{}.", index + 1),
                format!(
                    "manual check {} has no result row: {}",
                    index + 1,
                    manual_checks[index].lines().next().unwrap_or_default()
                ),
            ),
        }
    }
    let reuse_reason = reuse.join("\n");
    DeclarationValidationError::ensure_empty(diagnostic)?;
    let status = if !findings.is_empty()
        || results
            .iter()
            .any(|result| result.status == VerificationStatus::Failed)
    {
        VerificationStatus::Failed
    } else if results
        .iter()
        .any(|result| result.status == VerificationStatus::Blocked)
    {
        VerificationStatus::Blocked
    } else {
        VerificationStatus::Passed
    };
    let reason = results
        .iter()
        .enumerate()
        .filter(|(_, result)| result.status == VerificationStatus::Blocked)
        .map(|(index, result)| format!("Manual check {}: {}", index + 1, result.note))
        .collect::<Vec<_>>();
    finding_observed_in.dedup();
    Ok(VerificationReport {
        status,
        manual_checks: results,
        observed_in: finding_observed_in,
        findings,
        reason: (!reason.is_empty()).then(|| reason.join("\n")),
        reuse_reason: (!reuse_reason.is_empty()).then_some(reuse_reason),
    })
}

/// Split `[IDs] note` into its citations and note.
fn citations(rest: &str) -> Option<(Vec<&str>, &str)> {
    let (cited, note) = rest.trim_start().strip_prefix('[')?.split_once(']')?;
    Some((
        cited
            .split([',', ' '])
            .filter(|reference| !reference.is_empty())
            .collect(),
        note.trim(),
    ))
}

/// Resolve a citation to the full evidence ID it names, either directly or by alias.
fn resolve(reference: &str, evidence_ids: &[&str]) -> std::result::Result<String, String> {
    if evidence_ids.contains(&reference) {
        return Ok(reference.to_owned());
    }
    let matching = evidence_ids
        .iter()
        .filter(|id| evidence_alias(id) == reference)
        .collect::<Vec<_>>();
    match matching.as_slice() {
        [id] => Ok((**id).to_owned()),
        [] => Err(
            "not a completed tool result or passed Harness check in this execution; cite an ID \
             from plan:evidence.md"
                .into(),
        ),
        _ => Err("the alias matches several results; cite the full ID".into()),
    }
}

/// Validate a `path[:line[:column]]: message` finding and return it in canonical form.
fn finding(body: &str, workspace: &Path) -> std::result::Result<String, String> {
    const SHAPE: &str = "findings are `path:line:column: message`, with optional line and \
                         column, or `-: message` when no file applies";
    let mut parts = body.splitn(4, ':');
    let path = parts.next().unwrap_or_default().trim();
    let mut location = vec![path.to_owned()];
    let mut message = None;
    for part in parts.by_ref() {
        if message.is_none() && location.len() < 3 && part.trim().parse::<u32>().is_ok() {
            location.push(part.trim().to_owned());
        } else {
            message = Some(part);
            break;
        }
    }
    let rest = parts.collect::<Vec<_>>().join(":");
    let message = match message {
        Some(first) if rest.is_empty() => first.trim().to_owned(),
        Some(first) => format!("{first}:{rest}").trim().to_owned(),
        None => return Err(SHAPE.into()),
    };
    if path.is_empty() || message.is_empty() {
        return Err(SHAPE.into());
    }
    if path != "-" {
        if super::design::validate_relative_path(path).is_err() {
            return Err(format!("`{path}` is not a project-relative path"));
        }
        let source = super::workspace_source(workspace, path)
            .map_err(|error| format!("`{path}` cannot be read: {error}"))?
            .ok_or_else(|| format!("`{path}` does not exist in the workspace"))?;
        if let Some(line) = location.get(1).and_then(|line| line.parse::<usize>().ok()) {
            let count = source.lines().count();
            if line == 0 || line > count {
                return Err(format!(
                    "`{path}` has {count} lines; line {line} is outside it"
                ));
            }
        }
    }
    Ok(format!("{}: {message}", location.join(":")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::declaration::DeclarationDiagnostic;

    fn diagnostics(error: &anyhow::Error) -> Vec<DeclarationDiagnostic> {
        error
            .downcast_ref::<DeclarationValidationError>()
            .map(|error| error.diagnostic.clone())
            .unwrap_or_default()
    }

    fn manual_checks() -> Vec<String> {
        vec![
            "Open the window.".into(),
            "Resize it\nand observe the layout.".into(),
        ]
    }

    fn workspace() -> tempfile::TempDir {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("src/hud.rs"),
            "fn a() {}\nfn b() {}\n",
        )
        .unwrap();
        workspace
    }

    #[test]
    fn verification_template_lists_pending_rows_and_quotes() {
        let text = template(&manual_checks());
        assert!(text.contains("> Open the window.\n1. pending []\n"));
        assert!(text.contains("> Resize it\n> and observe the layout.\n2. pending []\n"));
        let workspace = workspace();
        let error = parse(&text, &manual_checks(), &[], workspace.path()).unwrap_err();
        assert_eq!(
            diagnostics(&error)
                .iter()
                .map(|diagnostic| diagnostic.line)
                .collect::<Vec<_>>(),
            vec![1, 1, 8, 12]
        );
    }

    #[test]
    fn verification_report_derives_failure_from_rows_and_findings() {
        let workspace = workspace();
        let alias = evidence_alias("tool-1");
        let text = template(&manual_checks())
            .replace(
                "1. pending []",
                &format!("1. passed [{alias}] The window opened."),
            )
            .replace("2. pending []", "2. failed [tool-1] Text clipped.")
            .replace(
                "## Findings\n",
                &format!("## Findings\nsrc/hud.rs:2:4: Score label overlaps [{alias}]\n"),
            );
        let report = parse(&text, &manual_checks(), &["tool-1"], workspace.path()).unwrap();
        assert_eq!(report.status, VerificationStatus::Failed);
        assert_eq!(
            report.manual_checks[0].observed_in,
            vec!["tool-1".to_owned()]
        );
        assert_eq!(
            report.manual_checks[1].label,
            "Resize it\nand observe the layout."
        );
        assert_eq!(
            report.findings,
            vec!["src/hud.rs:2:4: Score label overlaps".to_owned()]
        );
        assert_eq!(report.observed_in, vec!["tool-1".to_owned()]);
        report.validate_checks(&manual_checks()).unwrap();

        let blocked = template(&manual_checks())
            .replace("1. pending []", "1. passed [tool-1] Opened.")
            .replace("2. pending []", "2. blocked [] No display is available.");
        let report = parse(&blocked, &manual_checks(), &["tool-1"], workspace.path()).unwrap();
        assert_eq!(report.status, VerificationStatus::Blocked);
        assert_eq!(
            report.reason.as_deref(),
            Some("Manual check 2: No display is available.")
        );
    }

    #[test]
    fn verification_report_names_each_bad_line() {
        let workspace = workspace();
        let text = "# Verification\n\n## Results\n1. passed [E00000000] Opened.\n2. done [] x\n\
                    3. passed [tool-1] Extra.\n\n## Findings\nsrc/missing.rs:1: Gone\n\
                    src/hud.rs:9: Out of range\nno location\n";
        let error = parse(text, &manual_checks(), &["tool-1"], workspace.path()).unwrap_err();
        let reported = diagnostics(&error)
            .into_iter()
            .map(|diagnostic| (diagnostic.line, diagnostic.reference))
            .collect::<Vec<_>>();
        assert_eq!(
            reported,
            vec![
                (1, "2.".to_owned()),
                (4, "E00000000".to_owned()),
                (4, "passed".to_owned()),
                (5, "done".to_owned()),
                (6, "3".to_owned()),
                (9, "src/missing.rs:1: Gone".to_owned()),
                (10, "src/hud.rs:9: Out of range".to_owned()),
                (11, "no location".to_owned()),
            ]
        );
    }
}
