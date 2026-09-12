use crate::control_tools::ControlToolInvocation;
use crate::plan::{PlanDocument, PlanMutationError, PlanValidationError, PlanViolation};
use crate::rustdoc::RustApiValidationError;
use anyhow::Error;
use jsonschema::ValidationError;
use jsonschema::error::ValidationErrorKind;
use serde::Serialize;
use serde_json::{Value, json};
use std::fmt;

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

#[derive(Clone, Copy, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
enum ControlToolFailurePhase {
    ArgumentValidation,
    SemanticExecution,
    SubmissionValidation,
}

#[derive(Debug, Serialize)]
struct ControlToolFailure<'a> {
    ok: bool,
    tool: &'a str,
    phase: ControlToolFailurePhase,
    code: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    plan_id: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    active_version: Option<u64>,
    violation: Vec<ControlToolViolation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    retry: Option<ControlToolRetry>,
}

#[derive(Debug, Serialize)]
struct ControlToolRetry {
    action: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    expected_version: Option<u64>,
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
        ValidationErrorKind::Required { property } => (
            "missing_field",
            format!("Required field `{property}` is missing."),
            Some(json!({ "required": [property] })),
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
    let collection = ["entity_changes", "dependencies", "flows", "tasks"]
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
    let mut path = crate::control_tools::json_pointer_to_path(&error.instance_path().to_string());
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

pub(crate) fn control_tool_failure_json(
    invocation: &ControlToolInvocation,
    error: &Error,
    document: Option<&PlanDocument>,
) -> String {
    let (phase, code, violation) =
        if let Some(argument_error) = error.downcast_ref::<ControlToolArgumentError>() {
            (
                ControlToolFailurePhase::ArgumentValidation,
                "invalid_arguments".to_owned(),
                argument_error.violation.clone(),
            )
        } else if let Some(mutation_error) = error.downcast_ref::<PlanMutationError>() {
            (
                failure_phase(invocation),
                "invalid_plan_edit".to_owned(),
                vec![ControlToolViolation {
                    path: mutation_error.path.clone(),
                    code: mutation_error.code.clone(),
                    message: mutation_error.message.clone(),
                    expected_shape: Some(mutation_error.expected_shape.clone()),
                    hint: Some(mutation_error.hint.clone()),
                }],
            )
        } else if let Some(validation_error) = error.downcast_ref::<PlanValidationError>() {
            (
                failure_phase(invocation),
                "canonical_validation_failed".to_owned(),
                validation_error
                    .violation
                    .iter()
                    .map(plan_violation)
                    .collect(),
            )
        } else if let Some(validation_error) = error.downcast_ref::<RustApiValidationError>() {
            (
                failure_phase(invocation),
                "rust_api_validation_failed".to_owned(),
                validation_error
                    .violation
                    .iter()
                    .map(plan_violation)
                    .collect(),
            )
        } else {
            let message = format!("{error:#}");
            (
                failure_phase(invocation),
                semantic_code(&message).to_owned(),
                vec![ControlToolViolation {
                    path: "<request>".to_owned(),
                    code: semantic_violation_code(&message).to_owned(),
                    message,
                    expected_shape: None,
                    hint: None,
                }],
            )
        };
    let plan_id = document
        .map(|plan| plan.plan_id.as_str())
        .or_else(|| invocation.arguments.get("plan_id").and_then(Value::as_str));
    let active_version = document.map(|plan| plan.version);
    let retry = matches!(
        invocation.name.as_str(),
        "harness_plan_edit" | "harness_plan_submit"
    )
    .then_some(ControlToolRetry {
        action: "correct_and_retry",
        expected_version: active_version,
    });
    let failure = ControlToolFailure {
        ok: false,
        tool: invocation.name.as_str(),
        phase,
        code,
        plan_id,
        active_version,
        violation,
        retry,
    };
    serde_json::to_string(&failure).unwrap_or_else(|serialization_error| {
        json!({
            "ok": false,
            "tool": invocation.name,
            "phase": "semantic_execution",
            "code": "failure_serialization_failed",
            "violation": [{
                "path": "<response>",
                "code": "serialization_failed",
                "message": serialization_error.to_string()
            }]
        })
        .to_string()
    })
}

fn failure_phase(invocation: &ControlToolInvocation) -> ControlToolFailurePhase {
    if invocation.name == "harness_plan_submit" {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn formats_argument_failures_as_json_only() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".to_owned(),
            arguments: json!({ "plan_id": "plan-1", "expected_version": 2 }),
        };
        let error = Error::new(ControlToolArgumentError {
            violation: vec![ControlToolViolation {
                path: "set.flows[0]".to_owned(),
                code: "invalid_set_entry".to_owned(),
                message: "does not match one set entry".to_owned(),
                expected_shape: Some(json!({ "title": "Current flow" })),
                hint: Some("Send one complete flow resource.".to_owned()),
            }],
        });
        let document = crate::plan::test_fixture("plan-1", "Overview");

        let output = control_tool_failure_json(&invocation, &error, Some(&document));
        let failure: Value = serde_json::from_str(&output).expect("valid failure JSON");

        assert_eq!(failure["ok"], false);
        assert_eq!(failure["phase"], "argument_validation");
        assert_eq!(failure["code"], "invalid_arguments");
        assert_eq!(failure["violation"][0]["path"], "set.flows[0]");
        assert_eq!(
            failure["violation"][0]["hint"],
            "Send one complete flow resource."
        );
        assert_eq!(failure["retry"]["expected_version"], document.version);
        assert!(!output.starts_with("harness_plan_edit"));
    }

    #[test]
    fn classifies_version_conflicts_with_the_active_version() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".to_owned(),
            arguments: json!({ "plan_id": "plan-1", "expected_version": 2 }),
        };
        let document = crate::plan::test_fixture("plan-1", "Overview");
        let error = anyhow::anyhow!(
            "plan version conflict: expected 2, current {}",
            document.version
        );

        let output = control_tool_failure_json(&invocation, &error, Some(&document));
        let failure: Value = serde_json::from_str(&output).expect("valid failure JSON");

        assert_eq!(failure["code"], "stale_version");
        assert_eq!(failure["violation"][0]["code"], "expected_version_mismatch");
        assert_eq!(failure["active_version"], document.version);
    }

    #[test]
    fn formats_plan_mutation_errors_with_the_exact_path_and_hint() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".to_owned(),
            arguments: json!({ "plan_id": "plan-1", "expected_version": 2 }),
        };
        let document = crate::plan::test_fixture("plan-1", "Overview");
        let error = Error::new(PlanMutationError {
            path: "rename.flows[1].from".to_owned(),
            code: "missing_target".to_owned(),
            message: "flow `Old` does not exist".to_owned(),
            expected_shape: json!({ "from": "Current flow" }),
            hint: "Use the current title as the rename source.".to_owned(),
        });

        let output = control_tool_failure_json(&invocation, &error, Some(&document));
        let failure: Value = serde_json::from_str(&output).expect("valid failure JSON");

        assert_eq!(failure["code"], "invalid_plan_edit");
        assert_eq!(failure["violation"][0]["path"], "rename.flows[1].from");
        assert_eq!(failure["violation"][0]["code"], "missing_target");
        assert_eq!(
            failure["violation"][0]["expected_shape"]["from"],
            "Current flow"
        );
        assert_eq!(
            failure["violation"][0]["hint"],
            "Use the current title as the rename source."
        );
        assert_eq!(failure["retry"]["expected_version"], document.version);
    }

    #[test]
    fn derives_union_expectations_from_the_failing_schema() {
        let schema = json!({
            "oneOf": [
                {
                    "type": "object",
                    "properties": { "kind": { "const": "left" } },
                    "required": ["kind"],
                    "additionalProperties": false
                },
                {
                    "type": "object",
                    "properties": { "kind": { "const": "right" } },
                    "required": ["kind"],
                    "additionalProperties": false
                }
            ]
        });
        let validator = jsonschema::validator_for(&schema).unwrap();
        let violation_list = validator
            .iter_errors(&json!({ "kind": "unknown" }))
            .flat_map(|error| schema_violation_list(&error, &schema))
            .collect::<Vec<_>>();

        assert_eq!(violation_list.len(), 1);
        assert_eq!(violation_list[0].code, "invalid_union_shape");
        assert_eq!(
            violation_list[0]
                .expected_shape
                .as_ref()
                .and_then(|shape| shape["one_of"].as_array())
                .map(Vec::len),
            Some(2)
        );
    }

    #[test]
    fn formats_return_only_expansions_as_actionable_submission_json() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_submit".to_owned(),
            arguments: json!({ "plan_id": "plan-1", "expected_version": 1 }),
        };
        let document = crate::plan::test_fixture("plan-1", "Overview");
        let message = "Expansion only returns a value and does not describe work performed inside the parent relationship. Add a nested construct, call, read, write, send, or emit edge, add a meaningful branch, or remove the expansion and keep the return type on the parent edge.";
        let error = Error::new(PlanValidationError {
            phase: crate::plan::PlanValidationPhase::Submission,
            violation: vec![PlanViolation {
                path: "flows[0].edges[0].expansion".to_owned(),
                message: message.to_owned(),
            }],
        });

        let output = control_tool_failure_json(&invocation, &error, Some(&document));
        let failure: Value = serde_json::from_str(&output).expect("valid failure JSON");

        assert_eq!(failure["phase"], "submission_validation");
        assert_eq!(failure["code"], "canonical_validation_failed");
        assert_eq!(
            failure["violation"][0]["path"],
            "flows[0].edges[0].expansion"
        );
        assert_eq!(failure["violation"][0]["code"], "return_only_expansion");
        assert_eq!(failure["violation"][0]["message"], message);
        assert_eq!(failure["retry"]["action"], "correct_and_retry");
        assert_eq!(failure["retry"]["expected_version"], document.version);
    }
}
