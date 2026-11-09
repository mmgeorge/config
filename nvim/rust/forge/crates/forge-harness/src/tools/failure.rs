use crate::plan::{PlanDocument, PlanMutationError, PlanValidationError, PlanViolation};
use crate::tools::ControlToolInvocation;
use crate::tools::ToolError;
use jsonschema::ValidationError;
use jsonschema::error::ValidationErrorKind;
use serde::Serialize;
use serde_json::{Value, json};
use std::error::Error;
use std::fmt;
use std::fmt::Write as _;

#[derive(Clone, Debug, Serialize)]
pub(crate) struct ControlToolViolation {
    pub path: String,
    pub code: String,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected_shape: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hint: Option<String>,
}

#[derive(Clone, Debug)]
pub(crate) struct ControlToolArgumentError {
    pub violation: Vec<ControlToolViolation>,
}

impl fmt::Display for ControlToolArgumentError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "control-tool arguments contain {} structural violation(s)",
            self.violation.len()
        )
    }
}

impl std::error::Error for ControlToolArgumentError {}

/// Names the stage that rejected a control call, which selects the failure's opening line.
#[derive(Clone, Copy, Debug)]
enum ControlToolFailurePhase {
    ArgumentValidation,
    SemanticExecution,
    SubmissionValidation,
}

pub(crate) fn schema_violation_list(
    error: &ValidationError<'_>,
    schema: &Value,
) -> Vec<ControlToolViolation> {
    if let ValidationErrorKind::OneOfNotValid { context } | ValidationErrorKind::AnyOf { context } =
        error.kind()
        && let Some((branch_index, branch_schema)) = relevant_union_branch(error, schema)
        && let Some(branch_error_list) = context.get(branch_index)
        && !branch_error_list.is_empty()
    {
        let branch_hint = resolve_schema(branch_schema, schema)
            .get("description")
            .and_then(Value::as_str)
            .map(str::to_owned);
        let mut violation_list = branch_error_list
            .iter()
            .flat_map(|branch_error| schema_violation_list(branch_error, schema))
            .collect::<Vec<_>>();
        if let Some(branch_hint) = branch_hint {
            for violation in &mut violation_list {
                if violation.hint.is_none() {
                    violation.hint = Some(branch_hint.clone());
                }
            }
        }
        return violation_list;
    }
    vec![schema_violation(error, schema)]
}

fn relevant_union_branch<'a>(
    error: &ValidationError<'_>,
    schema: &'a Value,
) -> Option<(usize, &'a Value)> {
    let instance = error.instance();
    let branch_list = schema
        .pointer(&error.schema_path().to_string())?
        .as_array()?;
    if let Some(instance) = instance.as_object() {
        let discriminated = ["kind"]
            .into_iter()
            .filter_map(|discriminator| {
                instance
                    .get(discriminator)
                    .map(|value| (discriminator, value))
            })
            .chain(instance.iter().map(|(name, value)| (name.as_str(), value)))
            .find_map(|(discriminator, value)| {
                branch_list.iter().enumerate().find(|(_, branch)| {
                    resolve_schema(branch, schema)
                        .pointer(&format!("/properties/{discriminator}/const"))
                        == Some(value)
                })
            });
        if discriminated.is_some() {
            return discriminated;
        }
    }
    let mut matching_branch_list = branch_list
        .iter()
        .enumerate()
        .filter(|(_, branch)| schema_type_matches(resolve_schema(branch, schema), instance));
    let matching_branch = matching_branch_list.next()?;
    matching_branch_list
        .next()
        .is_none()
        .then_some(matching_branch)
}

fn resolve_schema<'a>(candidate: &'a Value, root: &'a Value) -> &'a Value {
    candidate
        .get("$ref")
        .and_then(Value::as_str)
        .and_then(|reference| reference.strip_prefix('#'))
        .and_then(|pointer| root.pointer(pointer))
        .unwrap_or(candidate)
}

fn schema_type_matches(schema: &Value, instance: &Value) -> bool {
    let matches = |expected: &str| match expected {
        "array" => instance.is_array(),
        "boolean" => instance.is_boolean(),
        "integer" => instance.as_i64().is_some() || instance.as_u64().is_some(),
        "null" => instance.is_null(),
        "number" => instance.is_number(),
        "object" => instance.is_object(),
        "string" => instance.is_string(),
        _ => false,
    };
    match schema.get("type") {
        Some(Value::String(expected)) => matches(expected),
        Some(Value::Array(expected_list)) => {
            expected_list.iter().filter_map(Value::as_str).any(matches)
        }
        _ => false,
    }
}

fn schema_violation(error: &ValidationError<'_>, schema: &Value) -> ControlToolViolation {
    let path = schema_violation_path(error);
    let path = if path.is_empty() {
        "<arguments>".to_owned()
    } else {
        path
    };
    let (code, message, expected_shape) = match error.kind() {
        ValidationErrorKind::AdditionalProperties { unexpected }
        | ValidationErrorKind::UnevaluatedProperties { unexpected } => (
            "unknown_field",
            unexpected_property_message(unexpected),
            allowed_property_shape(error, schema),
        ),
        // Report the object's complete required set, so a model that misses one field does not
        // read the error as a choice between alternative field sets.
        ValidationErrorKind::Required { property } => (
            "missing_field",
            format!("Required field `{property}` is missing."),
            Some(
                keyword_shape(error, schema, "required")
                    .unwrap_or_else(|| json!({ "required": [property] })),
            ),
        ),
        ValidationErrorKind::Type { .. } => (
            "type_mismatch",
            error.to_string(),
            keyword_shape(error, schema, "type"),
        ),
        ValidationErrorKind::Constant { expected_value } => (
            "invalid_value",
            error.to_string(),
            Some(json!({ "const": expected_value })),
        ),
        ValidationErrorKind::Enum { options } => (
            "invalid_value",
            error.to_string(),
            Some(json!({ "enum": options })),
        ),
        ValidationErrorKind::OneOfMultipleValid { .. }
        | ValidationErrorKind::OneOfNotValid { .. } => (
            "invalid_union_shape",
            error.to_string(),
            keyword_shape(error, schema, "one_of"),
        ),
        _ => (
            "schema_violation",
            error.to_string(),
            Some(json!({ "keyword": error.kind().keyword() })),
        ),
    };
    let hint = plan_edit_schema_hint(&path, code, error.kind());
    ControlToolViolation {
        path,
        code: code.to_owned(),
        message,
        expected_shape,
        hint,
    }
}

fn plan_edit_schema_hint(path: &str, code: &str, kind: &ValidationErrorKind) -> Option<String> {
    let edit_path = path.strip_prefix("proposed_changes.").unwrap_or(path);
    if ["tasks", "set.tasks", "rename.tasks", "delete.tasks"]
        .iter()
        .any(|prefix| edit_path == *prefix || edit_path.starts_with(&format!("{prefix}.")))
    {
        return Some("Replace the complete task tree under `stages`, with stable stage and task IDs, task `requires`, and each task's files and subtasks.".into());
    }
    let collection = ["entity_changes", "dependencies", "flows"]
        .into_iter()
        .find(|collection| {
            edit_path == *collection
                || edit_path.starts_with(&format!("rename.{collection}"))
                || edit_path.starts_with(&format!("set.{collection}"))
                || edit_path.starts_with(&format!("delete.{collection}"))
        })?;
    if edit_path == collection && code == "unknown_field" {
        return Some(format!(
            "Move complete resources directly under `set.{collection}`, rename current keys under `rename.{collection}`, or retract current keys under `delete.{collection}`."
        ));
    }
    let uses_key_value_wrapper = match kind {
        ValidationErrorKind::AdditionalProperties { unexpected }
        | ValidationErrorKind::UnevaluatedProperties { unexpected } => unexpected
            .iter()
            .any(|field| field == "key" || field == "value"),
        _ => false,
    };
    if edit_path.starts_with(&format!("set.{collection}"))
        && (edit_path.ends_with(".operation")
            || edit_path.ends_with(".target")
            || edit_path.ends_with(".key")
            || edit_path.ends_with(".value")
            || uses_key_value_wrapper)
    {
        return Some(format!(
            "Place each complete Plan Schema resource directly in `set.{collection}` without a key/value wrapper."
        ));
    }
    if edit_path.starts_with(&format!("set.{collection}"))
        && matches!(kind, ValidationErrorKind::Required { property } if property == "action")
    {
        return Some(
            "Add the required implementation `action` to the complete resource or nested change."
                .to_owned(),
        );
    }
    None
}

fn schema_violation_path(error: &ValidationError<'_>) -> String {
    let mut path = crate::tools::json_pointer_to_path(&error.instance_path().to_string());
    let unexpected = match error.kind() {
        ValidationErrorKind::AdditionalProperties { unexpected }
        | ValidationErrorKind::UnevaluatedProperties { unexpected }
            if unexpected.len() == 1 =>
        {
            unexpected.first()
        }
        _ => None,
    };
    if let Some(unexpected) = unexpected {
        if !path.is_empty() {
            path.push('.');
        }
        path.push_str(unexpected);
    }
    path
}

fn unexpected_property_message(unexpected: &[String]) -> String {
    match unexpected {
        [property] => format!("Field `{property}` is not allowed by this object shape."),
        _ => format!(
            "Fields {} are not allowed by this object shape.",
            unexpected
                .iter()
                .map(|property| format!("`{property}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

/// Render a failed control call as compiler-style text: one `location: error[code]: message`
/// line per problem, each shared correction once, and the retry instruction.
///
/// Only the model reads this text, so it carries no envelope fields the model already knows,
/// such as the tool name it called or the plan ID it supplied.
pub(crate) fn control_tool_failure_text(
    invocation: &ControlToolInvocation,
    error: &(dyn Error + 'static),
    document: Option<&PlanDocument>,
    authoring_directory: Option<&std::path::Path>,
) -> String {
    let (phase, mut violation) = if let Some(argument_error) =
        find_error::<ControlToolArgumentError>(error)
    {
        (
            ControlToolFailurePhase::ArgumentValidation,
            argument_error.violation.clone(),
        )
    } else if let Some(mutation_error) = find_error::<PlanMutationError>(error) {
        (
            failure_phase(invocation),
            vec![ControlToolViolation {
                path: mutation_error.path.clone(),
                code: mutation_error.code.clone(),
                message: mutation_error.message.clone(),
                expected_shape: Some(mutation_error.expected_shape.clone()),
                hint: Some(mutation_error.hint.clone()),
            }],
        )
    } else if let Some(validation_error) = find_error::<PlanValidationError>(error) {
        (
            failure_phase(invocation),
            validation_error
                .violation
                .iter()
                .map(plan_violation)
                .collect(),
        )
    } else if let Some(validation_error) =
        find_error::<crate::declaration::DeclarationValidationError>(error)
    {
        (
            failure_phase(invocation),
            validation_error
                .diagnostic
                .iter()
                .map(|diagnostic| ControlToolViolation {
                    path: format!(
                        "{}:{}:{}",
                        diagnostic.path,
                        diagnostic.line,
                        diagnostic.column + 1
                    ),
                    code: diagnostic.kind.code().into(),
                    message: if diagnostic.reference.is_empty() {
                        diagnostic.reason.clone()
                    } else {
                        format!("`{}`: {}", diagnostic.reference, diagnostic.reason)
                    },
                    expected_shape: None,
                    hint: Some(diagnostic.kind.hint().into()),
                })
                .collect(),
        )
    } else if let Some(question_error) = find_error::<crate::exchange::QuestionEventError>(error) {
        (
            failure_phase(invocation),
            vec![ControlToolViolation {
                path: "<question>".into(),
                code: question_error.code().into(),
                message: question_error.to_string(),
                expected_shape: None,
                hint: None,
            }],
        )
    } else if let Some(wait_error) = find_error::<crate::question::QuestionWaitError>(error) {
        (
            failure_phase(invocation),
            vec![ControlToolViolation {
                path: "<question_wait>".into(),
                code: wait_error.code().into(),
                message: wait_error.to_string(),
                expected_shape: None,
                hint: None,
            }],
        )
    } else if let Some((requested, active)) = plan_id_mismatch(error) {
        (
            failure_phase(invocation),
            vec![ControlToolViolation {
                path: "plan_id".into(),
                code: "plan_id_mismatch".into(),
                message: format!("requested plan {requested} does not match active plan {active}"),
                expected_shape: None,
                hint: Some(format!("Use plan_id {active}, which plan_status reports.")),
            }],
        )
    } else {
        let message = error_message(error);
        (
            failure_phase(invocation),
            vec![ControlToolViolation {
                path: "request".to_owned(),
                code: semantic_violation_code(&message).to_owned(),
                message,
                expected_shape: None,
                hint: None,
            }],
        )
    };
    let filesystem_planning = invocation.name == "plan_submit" && authoring_directory.is_some();
    if filesystem_planning {
        for item in &mut violation {
            if let Some(fields) = item
                .expected_shape
                .as_mut()
                .and_then(|shape| shape.get_mut("allowed_fields"))
                .and_then(Value::as_array_mut)
            {
                fields.retain(|field| {
                    !matches!(
                        field.as_str(),
                        Some("expected_version" | "authoring_snapshot" | "reason")
                    )
                });
            }
            if let Some(artifact) = item.path.strip_prefix("plan:") {
                item.path = format!("docs/{artifact}");
            } else if let Some(design) = document.and_then(|plan| plan.design.as_ref())
                && design
                    .proposed_source
                    .keys()
                    .any(|path| item.path == *path || item.path.starts_with(&format!("{path}:")))
            {
                item.path = format!("files/{}", item.path);
            }
        }
    }
    let active_version = if filesystem_planning {
        None
    } else {
        document.map(|plan| plan.version)
    };
    let code_hint = hoist_code_hints(&mut violation);
    let tool = invocation.name.as_str();
    let errors = match violation.len() {
        1 => "1 error".to_owned(),
        count => format!("{count} errors"),
    };
    let mut text = match (phase, active_version) {
        (ControlToolFailurePhase::ArgumentValidation, _) => {
            format!("{tool} rejected its arguments with {errors}:\n")
        }
        (ControlToolFailurePhase::SubmissionValidation, Some(version)) => {
            format!("{tool} rejected version {version} with {errors}:\n")
        }
        _ => format!("{tool} failed with {errors}:\n"),
    };
    for item in &violation {
        let _ = writeln!(
            text,
            "{}: error[{}]: {}",
            item.path, item.code, item.message
        );
        if let Some(hint) = &item.hint {
            let _ = writeln!(text, "  hint: {hint}");
        }
        if let Some(shape) = &item.expected_shape {
            let _ = writeln!(text, "  expected: {shape}");
        }
    }
    if !code_hint.is_empty() {
        text.push('\n');
        for (code, hint) in &code_hint {
            let _ = writeln!(text, "{code}: {hint}");
        }
    }
    let retry = match tool {
        // A patch is atomic, so saying so keeps the agent from patching text that a failed chunk
        // never wrote.
        "plan_edit" | "harness_plan_edit" => active_version.map(|version| {
            format!("No changes were applied. Fix them and retry with expected_version {version}.")
        }),
        "plan_submit" if filesystem_planning => Some("Fix the reported files in the planning directory, then call plan_submit with the plan ID. No expected version is required.".into()),
        "plan_submit" => active_version
            .map(|version| format!("Fix them and retry with expected_version {version}.")),
        "plan_phase_done"
            if code_hint.contains_key(
                crate::declaration::DeclarationDiagnosticKind::VerificationReport.code(),
            ) =>
        {
            Some(
                "Fix plan:verification.md with plan_edit, then call plan_phase_done again."
                    .to_owned(),
            )
        }
        _ => None,
    };
    if let Some(retry) = retry {
        text.push('\n');
        text.push_str(&retry);
        text.push('\n');
    }
    text
}

/// Move hints shared by every violation of a code to one entry per code, so a rejection with many
/// violations of one kind does not repeat the same correction text for each of them.
fn hoist_code_hints(
    violation: &mut [ControlToolViolation],
) -> std::collections::BTreeMap<String, String> {
    let mut code_hint = std::collections::BTreeMap::<String, Option<String>>::new();
    for item in violation.iter() {
        let Some(hint) = &item.hint else { continue };
        code_hint
            .entry(item.code.clone())
            .and_modify(|shared| {
                if shared.as_ref() != Some(hint) {
                    *shared = None;
                }
            })
            .or_insert_with(|| Some(hint.clone()));
    }
    let code_hint = code_hint
        .into_iter()
        .filter_map(|(code, hint)| hint.map(|hint| (code, hint)))
        .collect::<std::collections::BTreeMap<_, _>>();
    for item in violation.iter_mut() {
        if code_hint.contains_key(&item.code) {
            item.hint = None;
        }
    }
    code_hint
}

/// Locate a typed cause through tool and domain context wrappers.
fn find_error<'a, T: Error + 'static>(mut error: &'a (dyn Error + 'static)) -> Option<&'a T> {
    loop {
        if let Some(found) = error.downcast_ref::<T>() {
            return Some(found);
        }
        error = error.source()?;
    }
}

/// Locate an identity rejection even when an operation adds context.
fn plan_id_mismatch<'a>(mut error: &'a (dyn Error + 'static)) -> Option<(&'a str, &'a str)> {
    loop {
        if let Some(ToolError::PlanIdMismatch { requested, active }) =
            error.downcast_ref::<ToolError>()
        {
            return Some((requested, active));
        }
        error = error.source()?;
    }
}

/// Render all diagnostic context without discarding the underlying error.
fn error_message(mut error: &(dyn Error + 'static)) -> String {
    let mut messages = Vec::new();
    loop {
        let message = error.to_string();
        if messages.last() != Some(&message) {
            messages.push(message);
        }
        let Some(source) = error.source() else {
            break;
        };
        error = source;
    }
    messages.join(": ")
}

fn failure_phase(invocation: &ControlToolInvocation) -> ControlToolFailurePhase {
    if invocation.name == "plan_submit" {
        ControlToolFailurePhase::SubmissionValidation
    } else {
        ControlToolFailurePhase::SemanticExecution
    }
}

fn plan_violation(violation: &PlanViolation) -> ControlToolViolation {
    ControlToolViolation {
        path: violation.path.clone(),
        code: plan_violation_code(&violation.message).to_owned(),
        message: violation.message.clone(),
        expected_shape: None,
        hint: None,
    }
}

fn plan_violation_code(message: &str) -> &'static str {
    if message.contains("callable") && message.contains("does not exist") {
        "unknown_rust_callable"
    } else if message.contains("only returns a value and does not describe work") {
        "return_only_expansion"
    } else if message.contains("must contain at least one edge or branch") {
        "incomplete_flow_edge"
    } else if message.contains("does not exist") || message.contains("not found") {
        "unknown_reference"
    } else {
        "plan_invariant_violation"
    }
}

fn semantic_code(message: &str) -> &'static str {
    if message.contains("version conflict")
        || (message.contains("version") && message.contains("active version"))
    {
        "stale_version"
    } else if message.contains("plan id") && message.contains("active") {
        "wrong_plan_id"
    } else if message.contains("multiple operations") {
        "duplicate_target"
    } else if message.contains("already exists") {
        "target_conflict"
    } else if message.contains("not found") {
        "missing_target"
    } else {
        "semantic_execution_failed"
    }
}

fn semantic_violation_code(message: &str) -> &'static str {
    match semantic_code(message) {
        "stale_version" => "expected_version_mismatch",
        "wrong_plan_id" => "plan_id_mismatch",
        "duplicate_target" => "duplicate_target",
        "target_conflict" => "target_already_exists",
        "missing_target" => "missing_target",
        _ => "execution_error",
    }
}

fn allowed_property_shape(error: &ValidationError<'_>, schema: &Value) -> Option<Value> {
    let schema_path = error.schema_path().to_string();
    let object_path = schema_path
        .strip_suffix("/additionalProperties")
        .or_else(|| schema_path.strip_suffix("/unevaluatedProperties"))?;
    let allowed_field_list = schema
        .pointer(object_path)
        .and_then(|value| value.get("properties"))
        .and_then(Value::as_object)?
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    Some(json!({ "allowed_fields": allowed_field_list }))
}

fn keyword_shape(error: &ValidationError<'_>, schema: &Value, keyword: &str) -> Option<Value> {
    let keyword_path = error.schema_path().to_string();
    let expected_value = schema.pointer(&keyword_path)?.clone();
    Some(json!({ keyword: expected_value }))
}
