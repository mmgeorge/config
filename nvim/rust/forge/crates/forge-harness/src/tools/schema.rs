use serde_json::{Map, Value, json};

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
