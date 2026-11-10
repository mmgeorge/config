use crate::tools::error::{ToolContext, ToolResult};
use crate::tools::runtime::ControlToolRuntime;
use crate::tools::schema::{strict_object_input_schema, string_schema};
use crate::tools::{ControlToolDefinition, ControlToolInvocation, ControlToolResult};

pub(super) const NAME: &str = "plan_capture_file";

/// Describe fixed-destination capture without exposing a caller-selected output path.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Capture an existing repository-relative source or configuration file into the active planning directory's files/ tree at the same relative path. Supply only the source path, never a destination. Captures save trusted original source evidence; repeating capture preserves existing proposal edits and planned deletions. Call this before proposing any change to an existing file; never hand-write its proposal. Read the returned absolute proposal path, then edit it in place with your file-editing tool rather than shell commands. New repository files can be created directly under files/ without capture. Source proposals contain declarations, not executable bodies; configuration proposals contain complete contents.",
        input_schema: strict_object_input_schema(vec![("path", string_schema())], &["path"]),
    }
}

/// Prepare the captured copy under the invocation's existing task and permission authority.
pub(super) async fn execute(
    runtime: &mut ControlToolRuntime,
    invocation: ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> ToolResult<ControlToolResult> {
    runtime.require_editable_plan()?;
    let path = invocation.arguments["path"]
        .as_str()
        .context("capture requires a source path")?;
    let scope = runtime
        .context
        .plan_authoring
        .as_ref()
        .context("file capture requires an active filesystem planning task")?;
    if let Some(permissions) = &runtime.context.file_permissions {
        use forge_provider_api::permissions::{
            PermissionRequest, PermissionResponse, PermissionTarget,
        };
        let invocation_scope = runtime
            .context
            .question_invocation
            .as_ref()
            .context("capture has no original invocation")?;
        let request = PermissionRequest {
            id: uuid::Uuid::new_v4().to_string(),
            provider: forge_provider_api::ProviderId::new("harness")
                .expect("static provider identity is valid"),
            reason: Some("Capture source declarations into the planning directory".into()),
            target_list: vec![
                PermissionTarget::Read {
                    path: scope.source_path(path)?.to_string_lossy().into_owned(),
                },
                PermissionTarget::Write {
                    path: scope
                        .directory()
                        .join("files")
                        .join(path)
                        .to_string_lossy()
                        .into_owned(),
                },
            ],
        };
        match permissions.authorize(invocation_scope, request).await {
            PermissionResponse::Allow => {}
            PermissionResponse::Deny { reason } => {
                return Err(crate::tools::ToolError::Rejected(
                    reason.unwrap_or_else(|| "Capture file access was denied".into()),
                ));
            }
            PermissionResponse::Cancelled => return Err(crate::broker::TurnCancelled.into()),
        }
    } else {
        crate::tools::error::ensure!(
            !runtime.context.permission_required,
            "capture file permission authority is unavailable"
        );
    }
    let capture = || scope.capture(path);
    let destination =
        if let Some(authority) = crate::tools::TaskAuthority::capture(&runtime.context) {
            authority.commit(capture)?
        } else {
            capture()?
        };
    let state = if destination.try_exists()? {
        "proposal available; existing edits were preserved"
    } else {
        "planned deletion; the captured original was retained"
    };
    Ok(ControlToolResult {
        invocation: None,
        message: format!(
            "Proposal: {}\nRead-only original: {}\nState: {state}.",
            destination.display(),
            scope.baseline_path(path).display()
        ),
    })
}
