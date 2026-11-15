use serde_json::{Value, json};

/// Suppress native workflows whose state belongs to Harness on every thread boundary.
pub(super) fn apply(parameters: &mut Value) {
    if !parameters["config"].is_object() {
        parameters["config"] = json!({});
    }
    let config = &mut parameters["config"];
    config["tools.experimental_request_user_input.enabled"] = json!(false);
    config["tools.update_plan.enabled"] = json!(false);
    config["features.goals"] = json!(false);
    config["features.multi_agent"] = json!(false);
    config["features.multi_agent_v2.disable_native_tools"] = json!(true);
    // Catalog-advertised messaging bypasses this flag in Codex; it is not a complete opt-out.
    config["features.send_message_to_user_async"] = json!(false);
}
