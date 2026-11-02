use crate::plan::PlanDocument;
use crate::tools::ControlToolDefinition;
use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_file_read";

/// Describe the plan_file_read tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Read a file's proposed semantic structure or complete configuration text without changing the \
                plan. Supply plan_id and a project-relative path. Unedited files are read from the workspace \
                without being added to the plan. Set baseline to read the saved original of an edited file. \
                Optional start_line/end_line select an inclusive range. Returns numbered text, the shared \
                version, and a source digest for unedited files. Use plan_overview_read for the plan overview and \
                file inventory.",
        input_schema: strict_object_input_schema(
            vec![
                ("plan_id", string_schema()),
                (
                    "path",
                    json!({"type":"string","description":"Project-relative .rs, .ts, .tsx, .lua, JSON/JSONC, TOML, YAML, XML, or .gitignore path."}),
                ),
                ("baseline", json!({"type":"boolean"})),
                ("start_line", json!({"type":"integer","minimum":1})),
                ("end_line", json!({"type":"integer","minimum":1})),
            ],
            &["plan_id", "path"],
        ),
    }
}

/// Execute plan_file_read within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.require_readable_plan()?;
    let document = runtime
        .plan_document
        .as_ref()
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
        .as_deref()
        .context("file read has no workspace root")?;
    let path = invocation.arguments["path"]
        .as_str()
        .context("file read requires a path")?;
    let file = design.inspect(
        workspace,
        Some(path),
        invocation.arguments["baseline"].as_bool().unwrap_or(false),
    )?;
    let message = read_design(document, &file, &invocation.arguments)?;
    if let Some(digest) = file["source_digest"].as_str() {
        runtime.inspected.insert(path.into(), digest.into());
    }
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message,
    })
}

/// Presents an exact declaration range without JSON escaping or implementation bodies.
fn read_design(
    document: &PlanDocument,
    file: &serde_json::Value,
    arguments: &serde_json::Value,
) -> Result<String> {
    use std::fmt::Write;
    let path = arguments.get("path").and_then(serde_json::Value::as_str);
    let start = arguments
        .get("start_line")
        .and_then(serde_json::Value::as_u64);
    let end = arguments
        .get("end_line")
        .and_then(serde_json::Value::as_u64);
    crate::tools::error::ensure!(
        path.is_some() || (start.is_none() && end.is_none()),
        "line ranges require a file path"
    );
    let Some(path) = path else {
        return Ok(serde_json::to_string(
            &serde_json::json!({"plan_id":document.plan_id,"version":document.version,"file":file}),
        )?);
    };
    let text = file["text"]
        .as_str()
        .context("declaration read has no text")?;
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
    let side = file["side"].as_str().unwrap_or("proposed");
    let mut message = format!(
        "Plan {} · version {}\n{path} ({side}) · lines {start}-{end} of {total}\n",
        document.plan_id, document.version
    );
    if let Some(digest) = file["source_digest"].as_str() {
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
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    crate::tools::error::ensure!(
        invocation.arguments["path"].as_str() != Some("plan.json"),
        "use plan_overview_read to read the plan overview"
    );
    output.plan_read = invocation
        .arguments
        .get("plan_id")
        .and_then(serde_json::Value::as_str)
        .map(str::to_owned);
    output.structured_plan = output.plan_read.is_some();
    Ok(())
}
