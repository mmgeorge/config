use crate::plan::verification_artifact as verification;
use crate::plan::{PlanArtifact, PlanDocument};
use crate::tools::ControlToolDefinition;
use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::Value;
use std::fmt::Write;

pub(super) const NAME: &str = "plan_status";

/// Describe the plan_status tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Show the active plan's identity, phase, shared version, artifacts, and changed project \
                files in the same shape during planning and execution. Supply plan_id. Execution adds the \
                plan:findings.md, plan:evidence.md, and plan:verification.md artifacts. Read any listed \
                path with plan_read; plan_submit and plan_phase_done report problems by line.",
        input_schema: strict_object_input_schema(vec![("plan_id", string_schema())], &["plan_id"]),
    }
}

/// Execute plan_status within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.require_readable_plan()?;
    let document = runtime
        .plan_document
        .clone()
        .context("plan status has no active canonical document")?;
    let requested = output
        .plan_read
        .as_deref()
        .context("plan status did not produce a plan ID")?;
    if requested != document.plan_id {
        return Err(crate::tools::ToolError::PlanIdMismatch {
            requested: requested.into(),
            active: document.plan_id.clone(),
        });
    }
    let execution = execution_state(runtime).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: status_text(&document, execution.as_ref())?,
    })
}

/// Inspect the active execution, or return nothing while the plan is still being authored.
pub(super) async fn execution_state(
    runtime: &crate::tools::runtime::ControlToolRuntime,
) -> Result<Option<Value>> {
    let Some(execution) = &runtime.context.execution else {
        return Ok(None);
    };
    Ok(Some(
        execution
            .send(crate::plan::execution::ExecutionOperation::Inspect)
            .await?,
    ))
}

/// Render the status in one shape for every phase; execution only adds its own artifacts.
fn status_text(document: &PlanDocument, execution: Option<&Value>) -> Result<String> {
    let design = document
        .design
        .as_ref()
        .context("plan has no semantic structure")?;
    let preview = crate::plan::preview_authored(design);
    let overview = &preview;
    let mut text = match execution {
        Some(state) => format!(
            "Plan {} · {} · revision {}\n",
            document.plan_id,
            state["phase"].as_str().unwrap_or("execution"),
            state["revision"]
        ),
        None => format!(
            "Plan {} · planning · version {}\n",
            document.plan_id, document.version
        ),
    };
    let overview_state = if [&overview.objective, &overview.background, &overview.design]
        .iter()
        .all(|text| text.trim().is_empty())
        && overview.usage.is_none()
        && overview.requirements.is_empty()
        && overview.decisions.is_empty()
        && overview.manual_checks.is_empty()
    {
        "empty".to_owned()
    } else {
        "written".to_owned()
    };
    let flows_state = count(overview.flows.len(), "flow", "flows");
    let test_count = overview.tests.iter().map(|file| file.cases.len()).sum();
    let tests_state = if test_count == 0 {
        "empty".to_owned()
    } else {
        format!(
            "{} in {}",
            count(test_count, "test", "tests"),
            count(overview.tests.len(), "file", "files")
        )
    };
    let checks_state = if overview.checks.is_empty() {
        "no additional checks selected".to_owned()
    } else {
        format!("{} selected", overview.checks.join(", "))
    };
    let mut rows = vec![
        (PlanArtifact::Overview, overview_state),
        (PlanArtifact::Flows, flows_state),
        (PlanArtifact::Tests, tests_state),
        (PlanArtifact::Checks, checks_state),
    ];
    if let Some(state) = execution {
        let findings = state["findings"].as_array().map_or(0, Vec::len);
        rows.push((
            PlanArtifact::Findings,
            format!("read-only · {}", count(findings, "finding", "findings")),
        ));
        rows.push((
            PlanArtifact::Evidence,
            format!(
                "read-only · {}",
                count(
                    state["verification_evidence"]
                        .as_array()
                        .map_or(0, Vec::len),
                    "result",
                    "results"
                )
            ),
        ));
        let access = if verifying(state) {
            "editable"
        } else {
            "read-only"
        };
        let draft = if state["verification_draft"].is_string() {
            "written"
        } else {
            "template"
        };
        rows.push((
            PlanArtifact::Verification,
            format!(
                "{access} · {draft} · {}",
                count(
                    overview.manual_checks.len(),
                    "manual check",
                    "manual checks"
                )
            ),
        ));
    }
    text.push_str("\nArtifacts\n");
    let width = rows
        .iter()
        .map(|(artifact, _)| artifact.path().len())
        .max()
        .unwrap_or_default();
    for (artifact, state) in rows {
        writeln!(text, "  {:width$}  {state}", artifact.path())?;
    }
    text.push_str("\nFiles\n");
    let files = changed_files(design);
    if files.is_empty() {
        text.push_str("  (none)\n");
    }
    for file in files {
        writeln!(text, "  {file}")?;
    }
    Ok(text)
}

fn count(value: usize, singular: &str, plural: &str) -> String {
    match value {
        0 => format!("no {plural}"),
        1 => format!("1 {singular}"),
        value => format!("{value} {plural}"),
    }
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    super::read::record(invocation, output)
}

/// List changed project files with Git-style markers, pairing moves with their destination.
fn changed_files(design: &crate::plan::DeclarationDesign) -> Vec<String> {
    let destinations = design
        .moved
        .values()
        .collect::<std::collections::BTreeSet<_>>();
    let mut rows = Vec::new();
    for path in design.changed_paths() {
        if destinations.contains(&path) {
            continue;
        }
        let row = match (
            design.baseline.contains_key(&path),
            design.proposed_source.contains_key(&path),
        ) {
            (true, false) => match design.moved.get(&path) {
                Some(destination) => format!("R {path} -> {destination}"),
                None => format!("D {path}"),
            },
            (false, true) => format!("A {path}"),
            _ => format!("M {path}"),
        };
        rows.push(row);
    }
    rows
}

/// Render a read-only execution artifact from the execution inspection response.
pub(super) fn execution_artifact_text(
    artifact: PlanArtifact,
    document: &PlanDocument,
    state: &Value,
) -> Result<String> {
    let mut text = String::new();
    match artifact {
        PlanArtifact::Findings => {
            text.push_str("# Findings\n\n");
            let findings = state["findings"].as_array().cloned().unwrap_or_default();
            if findings.is_empty() {
                text.push_str("No findings.\n");
            }
            for finding in findings {
                writeln!(
                    text,
                    "- {}",
                    finding.as_str().unwrap_or_default().replace('\n', "\n  ")
                )?;
            }
        }
        PlanArtifact::Evidence => text = verification::evidence_text(&evidence(state)),
        PlanArtifact::Verification => {
            text = match state["verification_draft"].as_str() {
                Some(draft) => draft.to_owned(),
                None => verification::template(
                    &document
                        .design
                        .as_ref()
                        .context("plan has no semantic structure")?
                        .document
                        .manual_checks,
                ),
            };
        }
        _ => crate::tools::error::bail!("{} is not an execution artifact", artifact.path()),
    }
    Ok(text)
}

/// Report whether the execution is in Verify, the only phase that edits its report.
pub(super) fn verifying(state: &Value) -> bool {
    state["phase"] == serde_json::json!(crate::plan::PlanPhase::Verify)
}

fn evidence(state: &Value) -> Vec<verification::VerificationEvidenceEntry> {
    state["verification_evidence"]
        .as_array()
        .into_iter()
        .flatten()
        .map(|entry| verification::VerificationEvidenceEntry {
            id: entry["id"].as_str().unwrap_or_default().to_owned(),
            title: entry["title"].as_str().unwrap_or_default().to_owned(),
            state: entry["state"].as_str().unwrap_or_default().to_owned(),
            output_preview: entry["output_preview"]
                .as_str()
                .unwrap_or_default()
                .to_owned(),
        })
        .collect()
}
