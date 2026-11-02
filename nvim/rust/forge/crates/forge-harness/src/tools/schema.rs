use serde_json::{Map, Value, json};

/// Remove Rust documentation from the model-facing schema before the owning tool supplies guidance.
pub(in crate::tools) fn clear_descriptions(schema: &mut Value) {
    let Some(fields) = schema.as_object_mut() else {
        return;
    };
    fields.remove("description");
    for (keyword, value) in fields {
        match keyword.as_str() {
            "properties" | "patternProperties" | "$defs" | "definitions" | "dependentSchemas" => {
                if let Some(schemas) = value.as_object_mut() {
                    for schema in schemas.values_mut() {
                        clear_descriptions(schema);
                    }
                }
            }
            "allOf" | "anyOf" | "oneOf" | "prefixItems" => {
                if let Some(schemas) = value.as_array_mut() {
                    for schema in schemas {
                        clear_descriptions(schema);
                    }
                }
            }
            "items"
            | "additionalProperties"
            | "unevaluatedProperties"
            | "propertyNames"
            | "contains"
            | "not"
            | "if"
            | "then"
            | "else" => clear_descriptions(value),
            _ => {}
        }
    }
}

/// Attach field guidance to a generated schema without changing its validation constraints.
pub(in crate::tools) fn describe_fields(
    schema: &mut Value,
    path: &str,
    descriptions: &[(&str, &str)],
) {
    let fields = schema
        .pointer_mut(path)
        .and_then(Value::as_object_mut)
        .expect("tool schema description path must name generated properties");
    for (name, description) in descriptions {
        let field = fields
            .get_mut(*name)
            .and_then(Value::as_object_mut)
            .expect("tool schema description must name a generated field");
        field.insert("description".into(), Value::String((*description).into()));
    }
}

/// Describe an unconstrained string input.
pub(in crate::tools) fn string_schema() -> Value {
    json!({ "type": "string" })
}

/// Build a strict object schema with explicit required properties.
pub(in crate::tools) fn strict_object_input_schema(
    property_list: Vec<(&'static str, Value)>,
    required_list: &[&str],
) -> Value {
    let property_map = property_list
        .into_iter()
        .map(|(name, schema)| (name.to_owned(), schema))
        .collect::<Map<_, _>>();
    json!({
        "type": "object",
        "properties": property_map,
        "required": required_list,
        "additionalProperties": false
    })
}

#[cfg(test)]
pub(in crate::tools) fn plan_edit_input_schema() -> Value {
    crate::plan::plan_edit_request_schema()
}
