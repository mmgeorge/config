use crate::plan::{PlanDocument, PlanState};
use crate::provider::ProviderCompletion;
use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::{ControlToolInvocation, ControlToolResult, runtime::ControlToolRuntime};

/// Commit an artifact or file edit while preserving version and execution ownership.
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
    let mut request = output
        .design_patch
        .pop()
        .context("plan edit has no request")?;
    if runtime
        .context
        .execution
        .as_ref()
        .is_some_and(|execution| execution.phase == crate::plan::PlanPhase::Verify)
    {
        return apply_verification_edit(runtime, request).await;
    }
    let document = runtime
        .plan_document
        .as_ref()
        .context("design patch has no active design")?;
    let workspace = runtime
        .context
        .workspace_root
        .as_deref()
        .context("design patch has no workspace root")?;
    for (path, digest) in &runtime.inspected {
        request
            .source_digests
            .entry(path.clone())
            .or_insert_with(|| digest.clone());
    }
    let mut updated = document.patch_design(workspace, request.clone())?;
    if runtime.context.execution.is_some()
        && let Some(design) = &mut updated.design
    {
        design.authoring_fingerprint = None;
    }
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
    invocation.arguments["source_digests"] = serde_json::to_value(&request.source_digests)?;
    let confirmation = changed_paths(document, &updated)?;
    if let Some(execution) = &runtime.context.execution {
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

/// Save a Verify report edit; the accepted plan itself stays immutable outside Resolve.
async fn apply_verification_edit(
    runtime: &mut ControlToolRuntime,
    request: crate::plan::DesignPatchRequest,
) -> Result<ControlToolResult> {
    use crate::plan::PlanArtifact;
    let document = runtime
        .plan_document
        .as_ref()
        .context("verification edit has no active plan")?;
    if request.plan_id != document.plan_id {
        return Err(crate::tools::ToolError::PlanIdMismatch {
            requested: request.plan_id,
            active: document.plan_id.clone(),
        });
    }
    crate::tools::error::ensure!(
        request.expected_version == document.version,
        "stale plan version: expected {}, active plan is version {}",
        request.expected_version,
        document.version
    );
    let state = super::status::execution_state(runtime)
        .await?
        .context("verification edit has no active execution")?;
    let path = PlanArtifact::Verification.path();
    let original =
        super::status::execution_artifact_text(PlanArtifact::Verification, document, &state)?;
    let patched = crate::plan::patch_single_file(&request.patch, path, &original)
        .map_err(|error| anyhow::anyhow!("{error}; in Verify, plan_edit accepts only {path}"))?;
    if patched == original {
        return Ok(ControlToolResult {
            invocation: None,
            message: format!("{path} unchanged."),
        });
    }
    runtime
        .context
        .execution
        .as_ref()
        .context("verification edit has no active execution")?
        .send(crate::plan::execution::ExecutionOperation::VerificationDraft(patched))
        .await?;
    Ok(ControlToolResult {
        invocation: None,
        message: format!(
            "Verification report saved; plan_phase_done reports any problems in it.\nChanged: {path}"
        ),
    })
}

/// Name the artifacts and project files an edit changed, without echoing their content.
fn changed_paths(before: &PlanDocument, after: &PlanDocument) -> Result<String> {
    let before_design = before
        .design
        .as_ref()
        .context("source plan has no declaration design")?;
    let after_design = after
        .design
        .as_ref()
        .context("patched plan has no declaration design")?;
    let mut changed =
        crate::plan::changed_artifacts(&before.title, before_design, &after.title, after_design)
            .into_iter()
            .map(|artifact| artifact.path().to_owned())
            .collect::<Vec<_>>();
    let paths = [before_design, after_design]
        .into_iter()
        .flat_map(|design| design.baseline.keys().chain(design.proposed_source.keys()))
        .collect::<std::collections::BTreeSet<_>>();
    for path in paths {
        if before_design.proposed_source.get(path) != after_design.proposed_source.get(path)
            || before_design.baseline.contains_key(path) != after_design.baseline.contains_key(path)
        {
            changed.push(path.clone());
        }
    }
    Ok(if changed.is_empty() {
        "No content changes.".into()
    } else {
        format!("Changed: {}", changed.join(", "))
    })
}
