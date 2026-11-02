use serde_json::{Value, json};

/// Preserve the conversational answer schema used by existing clients.
pub fn question_answer_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "question_id": { "type": "string" },
            "response": {
                "type": "object",
                "properties": {
                    "kind": { "type": "string", "enum": ["selected", "other"] },
                    "option": { "type": "string" },
                    "feedback": { "type": "string" },
                    "text": { "type": "string" }
                },
                "required": ["kind"],
                "additionalProperties": false
            }
        },
        "required": ["question_id", "response"],
        "additionalProperties": false
    })
}
