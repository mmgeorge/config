use crate::plan::{PlanArtifact, PlanDocument};
use crate::tools::ControlToolDefinition;
use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_read";

/// Describe the plan_read tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Read a plan artifact or a project file's proposed semantic structure without changing \
                the plan. Supply plan_id and a path. Plan artifacts are plan:overview.md, plan:flows.md, \
                plan:tests.md, and plan:checks.md, plus plan:findings.md, plan:evidence.md, and \
                plan:verification.md during execution; plan:verification.md holds the Verify report and \
                plan:evidence.md the results it may cite. Other paths are project-relative files; unedited files are read from the \
                workspace without being added to the plan. Set baseline to read the saved original of an \
                edited project file. Optional start_line/end_line select an inclusive range. Returns \
                numbered text and the shared version, plus a source digest for unedited files. Use \
                plan_status for the plan's artifacts and changed files.",
        input_schema: strict_object_input_schema(
            vec![
                ("plan_id", string_schema()),
                (
                    "path",
                    json!({"type":"string","description":"A plan: artifact path, or a project-relative .rs, .ts, .tsx, .lua, JSON/JSONC, TOML, YAML, XML, or .gitignore path."}),
                ),
                ("baseline", json!({"type":"boolean"})),
                ("start_line", json!({"type":"integer","minimum":1})),
                ("end_line", json!({"type":"integer","minimum":1})),
            ],
            &["plan_id", "path"],
        ),
    }
}

/// Execute plan_read within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.require_readable_plan()?;
    let document = runtime
        .plan_document
        .clone()
        .context("plan read has no active canonical document")?;
    let requested = output
        .plan_read
        .as_deref()
        .context("plan read did not produce a plan ID")?;
    if requested != document.plan_id {
        return Err(crate::tools::ToolError::PlanIdMismatch {
            requested: requested.into(),
            active: document.plan_id.clone(),
        });
    }
    let design = document
        .design
        .as_ref()
        .context("plan has no semantic structure")?;
    let workspace = runtime
        .context
        .workspace_root
        .clone()
        .context("file read has no workspace root")?;
    let path = invocation.arguments["path"]
        .as_str()
        .context("plan read requires a path")?;
    let baseline = invocation.arguments["baseline"].as_bool().unwrap_or(false);
    if let Some(artifact) = PlanArtifact::from_path(path)? {
        crate::tools::error::ensure!(
            !baseline,
            "{path} has no baseline; read it without baseline"
        );
        let text = if artifact.authored() {
            crate::plan::design_artifact_text(artifact, &document.title, design, &workspace)?
        } else {
            let state = super::status::execution_state(runtime)
                .await?
                .with_context(|| format!("{path} exists only while an accepted plan executes"))?;
            super::status::execution_artifact_text(artifact, &document, &state)?
        };
        let message = numbered(
            &document,
            path,
            "artifact",
            &text,
            None,
            &invocation.arguments,
        )?;
        return Ok(crate::tools::ControlToolResult {
            invocation: Some(invocation),
            message,
        });
    }
    let file = design.inspect(&workspace, path, baseline)?;
    let text = file["text"]
        .as_str()
        .context("declaration read has no text")?;
    let side = file["side"].as_str().unwrap_or("proposed");
    let digest = file["source_digest"].as_str();
    let message = numbered(&document, path, side, text, digest, &invocation.arguments)?;
    if let Some(digest) = digest {
        runtime.inspected.insert(path.into(), digest.into());
    }
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message,
    })
}

/// Presents an exact line range without JSON escaping or implementation bodies.
fn numbered(
    document: &PlanDocument,
    path: &str,
    side: &str,
    text: &str,
    digest: Option<&str>,
    arguments: &serde_json::Value,
) -> Result<String> {
    use std::fmt::Write;
    let start = arguments
        .get("start_line")
        .and_then(serde_json::Value::as_u64);
    let end = arguments
        .get("end_line")
        .and_then(serde_json::Value::as_u64);
    let total = text.lines().count();
    let start = start.unwrap_or(1);
    let end = end.unwrap_or(total as u64);
    crate::tools::error::ensure!(
        end >= start || (total == 0 && start == 1 && end == 0),
        "end_line precedes start_line"
    );
    crate::tools::error::ensure!(
        start <= total as u64 || (total == 0 && start == 1),
        "start_line is outside the file ({total} lines)"
    );
    let end = end.min(total as u64);
    let mut message = format!(
        "Plan {} · version {}\n{path} ({side}) · lines {start}-{end} of {total}\n",
        document.plan_id, document.version
    );
    if let Some(digest) = digest {
        writeln!(
            message,
            "Source digest: {digest} (uncaptured workspace file)"
        )?;
    }
    for (index, line) in text
        .lines()
        .enumerate()
        .skip(start.saturating_sub(1) as usize)
        .take(end.saturating_sub(start).saturating_add(1) as usize)
    {
        writeln!(message, "{}: {line}", index + 1)?;
    }
    Ok(message)
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    output.plan_read = invocation
        .arguments
        .get("plan_id")
        .and_then(serde_json::Value::as_str)
        .map(str::to_owned);
    output.structured_plan = output.plan_read.is_some();
    Ok(())
}
