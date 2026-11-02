use crate::plan::{PlanDocument, PlanState};
use crate::provider::ProviderCompletion;
use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::{ControlToolInvocation, ControlToolResult, runtime::ControlToolRuntime};

/// Commit a file or overview edit while preserving version and execution ownership.
pub(super) async fn apply_edit(
    runtime: &mut ControlToolRuntime,
    mut invocation: ControlToolInvocation,
    mut output: ProviderCompletion,
) -> Result<ControlToolResult> {
    runtime.require_readable_plan()?;
    crate::tools::error::ensure!(
        !runtime.context.has_active_elicitation,
        "resolve pending Harness questions before editing the design"
    );
    let edit = output
        .design_patch
        .pop()
        .context("plan edit has no request")?;
    let document = runtime
        .plan_document
        .as_ref()
        .context("design patch has no active design")?;
    let workspace = runtime
        .context
        .workspace_root
        .as_deref()
        .context("design patch has no workspace root")?;
    let mut request = edit.into_patch(document, workspace)?;
    for (path, digest) in &runtime.inspected {
        request
            .source_digests
            .entry(path.clone())
            .or_insert_with(|| digest.clone());
    }
    let updated = document.patch_design(workspace, request.clone())?;
    if updated.version == document.version {
        return Ok(ControlToolResult {
            invocation: None,
            message: runtime.plan_version_message("Plan unchanged"),
        });
    }
    let updated_design = updated
        .design
        .as_ref()
        .context("patched plan has no declaration design")?;
    let previous_design = document
        .design
        .as_ref()
        .context("source plan has no declaration design")?;
    for (path, file) in &updated_design.baseline {
        if !previous_design.baseline.contains_key(path) {
            request
                .source_digests
                .insert(path.clone(), file.source_digest.clone());
        }
    }
    runtime
        .patch_source_digests
        .insert(request.expected_version, request.source_digests.clone());
    if invocation.name == "plan_file_edit" {
        invocation.arguments["source_digests"] = serde_json::to_value(&request.source_digests)?;
    }
    let delta = crate::plan::revision::DeclarationDelta::between(Some(document), updated_design)?;
    let mut confirmation = format!("{}{}", delta.document, delta.files);
    if confirmation.is_empty() {
        confirmation.push_str("No declaration changes.\n");
    }
    if let Some(execution) = &runtime.context.execution {
        if updated.version == document.version {
            return Ok(ControlToolResult {
                invocation: None,
                message: runtime.plan_version_message("Design unchanged"),
            });
        }
        let saved = execution
            .send(crate::plan::execution::ExecutionOperation::Draft(Box::new(
                updated.clone(),
            )))
            .await?;
        let updated: PlanDocument = serde_json::from_value(saved["document"].clone())?;
        runtime.context.plan_state = Some(PlanState::Revising);
        runtime.plan_document = Some(updated);
        return Ok(ControlToolResult {
            invocation: None,
            message: format!(
                "{}\n{confirmation}",
                runtime.plan_version_message("Execution revision draft saved")
            ),
        });
    }
    runtime.plan_document = Some(updated);
    if runtime.context.plan_state == Some(PlanState::AwaitingReview) {
        runtime.context.plan_state = Some(PlanState::Revising);
    }
    Ok(ControlToolResult {
        invocation: Some(invocation),
        message: format!(
            "{}\n{confirmation}",
            runtime.plan_version_message("Plan edit accepted")
        ),
    })
}
