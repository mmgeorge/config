use crate::tools::schema::strict_object_input_schema;
use serde_json::{Value, json};

/// Build shared repository pagination and diff selection inputs.
pub(super) fn input_schema(diff: bool, file: bool) -> Value {
    let mut properties = vec![("offset", json!({ "type": "integer", "minimum": 0 }))];
    if diff {
        properties.push((
            "side",
            json!({ "type": "string", "enum": ["staged", "unstaged"] }),
        ));
    }
    if file {
        properties.push((
            "path_hex",
            json!({ "type": "string", "pattern": "^([0-9a-fA-F]{2})+$" }),
        ));
    }
    strict_object_input_schema(properties, if file { &["path_hex"] } else { &[] })
}
