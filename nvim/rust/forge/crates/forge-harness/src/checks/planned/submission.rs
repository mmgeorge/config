//! Retain advisory discovery observations separately from execution evidence.

use super::super::discovery::DiscoveryInputs;
use crate::plan::{DeclarationDesign, DesignTestChange, PlanViolation};
use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// Describe the last review-time comparison without turning an observation into evidence.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub(crate) enum SubmissionWarningStatus {
    Matched,
    Stale { reason: String },
    Unavailable { reason: String },
}

impl SubmissionWarningStatus {
    /// Label the comparison time and preserve failures shown to the reviewer.
    pub(crate) fn label(&self) -> String {
        match self {
            Self::Matched => "Submission warnings — inputs matched when review opened".into(),
            Self::Stale { reason } => format!("Stale submission warnings — {reason}"),
            Self::Unavailable { reason } => {
                format!("Submission warnings — inputs not revalidated: {reason}")
            }
        }
    }
}

/// Identify one warning within the exact submitted test artifact.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub(crate) struct PlannedTestWarning {
    pub(crate) file: String,
    pub(crate) name: String,
    pub(crate) change: DesignTestChange,
    pub(crate) line: usize,
    pub(crate) message: String,
}

impl PlannedTestWarning {
    /// Render the authored location in the existing compiler-style tool response.
    pub(crate) fn violation(&self) -> PlanViolation {
        PlanViolation {
            path: format!("plan:tests.md:{}", self.line),
            message: self.message.clone(),
        }
    }
}

/// Preserve a submission's observations and the inputs under which they were collected.
/// These observations never certify a test result or a removed test's absence.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(try_from = "RawSubmissionWarnings")]
pub struct SubmissionWarnings {
    document_version: u64,
    plan_inputs: String,
    source_snapshot: String,
    inputs: DiscoveryInputs,
    warnings: Vec<PlannedTestWarning>,
}

impl SubmissionWarnings {
    /// Capture one complete static-discovery warning set for this authored revision.
    pub(in crate::checks) fn new(
        design: &DeclarationDesign,
        document_version: u64,
        source_snapshot: String,
        inputs: DiscoveryInputs,
        warnings: Vec<PlannedTestWarning>,
    ) -> Result<Self> {
        RawSubmissionWarnings {
            document_version,
            plan_inputs: fingerprint(design)?,
            source_snapshot,
            inputs,
            warnings,
        }
        .try_into()
    }

    /// Get the submitted warning entries without permitting mutation of their identities.
    pub(crate) fn warnings(&self) -> &[PlannedTestWarning] {
        &self.warnings
    }

    /// Check the authored artifact and reviewer choices before attaching saved line locations.
    pub(crate) fn matches_plan(&self, design: &DeclarationDesign) -> Result<bool> {
        Ok(self.plan_inputs == fingerprint(design)?)
    }
}

impl super::super::CheckSupervisor {
    /// Compare retained advisory inputs at review opening without repeating discovery.
    pub(crate) async fn submission_warning_status(
        &self,
        design: &DeclarationDesign,
    ) -> Option<SubmissionWarningStatus> {
        let report = design.submission_warnings.as_ref()?;
        let validate = || -> Result<()> {
            ensure!(
                report.matches_plan(design)?,
                "authored tests or reviewer choices changed; resubmit to replace these warnings"
            );
            report.inputs.validate(self)
        };
        if let Err(error) = validate() {
            return Some(SubmissionWarningStatus::Stale {
                reason: format!("{error:#}"),
            });
        }
        let capture = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            self.changes.capture(&self.session_id),
        )
        .await;
        let snapshot = match capture {
            Ok(Ok(snapshot)) => snapshot,
            Ok(Err(error)) => {
                return Some(SubmissionWarningStatus::Unavailable {
                    reason: format!("{error:#}"),
                });
            }
            Err(_) => {
                return Some(SubmissionWarningStatus::Unavailable {
                    reason: "source comparison timed out".into(),
                });
            }
        };
        if let Err(error) = validate() {
            return Some(SubmissionWarningStatus::Stale {
                reason: format!("{error:#}"),
            });
        }
        Some(if snapshot.id == report.source_snapshot {
            SubmissionWarningStatus::Matched
        } else {
            SubmissionWarningStatus::Stale {
                reason: "workspace source changed since submission".into(),
            }
        })
    }
}

/// Validate restored advisory records at their serialization boundary.
#[derive(Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
struct RawSubmissionWarnings {
    document_version: u64,
    plan_inputs: String,
    source_snapshot: String,
    inputs: DiscoveryInputs,
    warnings: Vec<PlannedTestWarning>,
}

impl TryFrom<RawSubmissionWarnings> for SubmissionWarnings {
    type Error = anyhow::Error;

    /// Reject records with missing revision, snapshot, or authored warning identity.
    fn try_from(raw: RawSubmissionWarnings) -> Result<Self> {
        let RawSubmissionWarnings {
            document_version,
            plan_inputs,
            source_snapshot,
            inputs,
            warnings,
        } = raw;
        ensure!(
            document_version > 0,
            "warning document version must be positive"
        );
        ensure!(!plan_inputs.is_empty(), "warning plan inputs are missing");
        ensure!(
            !source_snapshot.is_empty(),
            "warning source snapshot is missing"
        );
        let mut entries = std::collections::BTreeSet::new();
        for warning in &warnings {
            super::super::config::relative_path(&warning.file)?;
            ensure!(
                !warning.name.trim().is_empty()
                    && !warning.message.trim().is_empty()
                    && warning.line > 0,
                "warning authored identity or message is missing"
            );
            ensure!(
                warning.change != DesignTestChange::New,
                "new tests have no submission discovery warning"
            );
            ensure!(
                entries.insert((&warning.file, &warning.name)),
                "duplicate submitted test warning"
            );
        }
        Ok(Self {
            document_version,
            plan_inputs,
            source_snapshot,
            inputs,
            warnings,
        })
    }
}

/// Bind line locations to exact artifact text and the reviewer-owned selection inputs.
fn fingerprint(design: &DeclarationDesign) -> Result<String> {
    let tests = crate::plan::design_artifact_text(crate::plan::PlanArtifact::Tests, "", design)?;
    Ok(crate::plan::digest(&serde_json::to_vec(&(
        tests,
        &design.document.tests,
        &design.document.checks,
        &design.document.disabled_checks,
        &design.document.test_scopes,
    ))?))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Bind a warning to an authored entry without running discovery or a subprocess.
    fn fixture() -> (DeclarationDesign, SubmissionWarnings) {
        let mut design = DeclarationDesign::default();
        design.document.tests = serde_json::from_value(serde_json::json!([
            {"file":"tests.rs","cases":[{"name":"missing","change":"modified","description":"coverage"}]}
        ])).unwrap();
        design.artifact_text.insert(
            "plan:tests.md".into(),
            "## tests.rs\n- modified `missing`: coverage\n".into(),
        );
        let inputs: DiscoveryInputs = serde_json::from_value(serde_json::json!({
            "snapshot":{"digest":"configuration","config":{},"helper_digest":{".forge/checks/tests.nu":"helper"}},
            "task_id":"task",
            "overrides":{"enabled":{"tests":true}}
        })).unwrap();
        let report = SubmissionWarnings::new(
            &design,
            7,
            "source-snapshot".into(),
            inputs,
            vec![PlannedTestWarning {
                file: "tests.rs".into(),
                name: "missing".into(),
                change: DesignTestChange::Modified,
                line: 2,
                message: "planned test missing in tests.rs: not found".into(),
            }],
        )
        .unwrap();
        (design, report)
    }

    /// Restore the complete discovery context, and reject malformed warning identities.
    #[test]
    fn retained_context_round_trips_and_rejects_incomplete_records() {
        let (mut design, report) = fixture();
        design.submission_warnings = Some(report.clone());
        let restored: DeclarationDesign =
            serde_json::from_slice(&serde_json::to_vec(&design).unwrap()).unwrap();
        assert_eq!(restored.submission_warnings.as_ref(), Some(&report));
        assert!(report.matches_plan(&restored).unwrap());
        let original = serde_json::to_value(&report).unwrap();
        for field in [
            "document_version",
            "plan_inputs",
            "source_snapshot",
            "inputs",
            "warnings",
        ] {
            let mut invalid = original.clone();
            invalid.as_object_mut().unwrap().remove(field);
            assert!(
                serde_json::from_value::<SubmissionWarnings>(invalid).is_err(),
                "accepted missing {field}"
            );
        }
        for (field, value) in [
            ("line", serde_json::json!(0)),
            ("name", serde_json::json!("")),
            ("change", serde_json::json!("new")),
        ] {
            let mut invalid = original.clone();
            invalid["warnings"][0][field] = value;
            assert!(
                serde_json::from_value::<SubmissionWarnings>(invalid).is_err(),
                "accepted invalid {field}"
            );
        }
        let mut duplicate = original;
        let entry = duplicate["warnings"][0].clone();
        duplicate["warnings"].as_array_mut().unwrap().push(entry);
        assert!(serde_json::from_value::<SubmissionWarnings>(duplicate).is_err());
    }

    /// Do not reattach a saved line after an artifact or reviewer selection edit.
    #[test]
    fn artifact_and_selection_edits_make_saved_locations_stale() {
        let (design, report) = fixture();
        assert!(report.matches_plan(&design).unwrap());
        let mut edited = design.clone();
        edited
            .artifact_text
            .get_mut("plan:tests.md")
            .unwrap()
            .insert(0, '\n');
        assert!(!report.matches_plan(&edited).unwrap());
        let mut edited = design.clone();
        edited.document.tests[0].cases[0].name = "different".into();
        assert!(!report.matches_plan(&edited).unwrap());
        let mut edited = design.clone();
        edited.document.disabled_checks.push("tests".into());
        assert!(!report.matches_plan(&edited).unwrap());
        let mut edited = design;
        edited
            .document
            .test_scopes
            .insert("tests".into(), crate::plan::PlanTestScope::WholeSuite);
        assert!(!report.matches_plan(&edited).unwrap());
    }
}
