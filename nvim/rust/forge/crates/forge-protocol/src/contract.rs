//! Shared event vocabulary and payload requirements for host and editor.

use std::collections::BTreeMap;
use std::sync::OnceLock;
use serde::Deserialize;
use serde_json::Value;

#[derive(Deserialize)]
struct Event {
    route: String,
    required: BTreeMap<String, String>,
}

#[derive(Deserialize)]
struct Contract {
    version: u32,
    session: BTreeMap<String, Event>,
    backend: BTreeMap<String, Event>,
    request: BTreeMap<String, Event>,
    document: BTreeMap<String, Event>,
}

fn contract() -> &'static Contract {
    static CONTRACT: OnceLock<Contract> = OnceLock::new();
    CONTRACT.get_or_init(|| {
        let contract: Contract = serde_json::from_str(include_str!("../../../../../lua/forge/protocol_contract.json"))
            .expect("compiled event contract");
        assert_eq!(contract.version, crate::WIRE_VERSION);
        contract
    })
}

/// Selects only normalized backend events consumed by the editor.
pub fn backend_visible(kind: &str) -> bool {
    contract().backend.contains_key(kind)
}

/// Rejects unknown event variants and missing fields before serialization.
pub fn validate(channel: &str, name: &str, payload: &Value) -> Result<(), String> {
    let contract = contract();
    let events = match channel {
        "session" => &contract.session,
        "backend" => &contract.backend,
        "request" => &contract.request,
        "document" => &contract.document,
        _ => return Err(format!("unknown event channel: {channel}")),
    };
    let event = events.get(name).ok_or_else(|| format!("unknown {channel} event: {name}"))?;
    if !matches!(event.route.as_str(), "handle" | "refresh" | "ignore" | "transport" | "progress") {
        return Err(format!("invalid event route: {}", event.route));
    }
    if !payload.is_object() { return Err(format!("{channel} event {name} requires an object payload")); }
    for (path, expected) in &event.required {
        let value = path.split('.').try_fold(payload, |value, key| value.get(key));
        let valid = value.is_some_and(|value| match expected.as_str() {
            "string" => value.as_str().is_some_and(|text| !text.is_empty()),
            "nullable_string" => value.is_null() || value.as_str().is_some_and(|text| !text.is_empty()),
            "integer" => value.as_u64().is_some_and(|number| number <= (1 << 53) - 1),
            "object" => value.is_object(),
            "boolean" => value.is_boolean(),
            _ => false,
        });
        if !valid { return Err(format!("{channel} event {name}: {path} requires {expected}")); }
    }
    if channel == "session" && name == "backend_event" {
        validate("backend", payload["kind"].as_str().unwrap_or_default(), payload)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::message::{Message, SessionEvent};
    use serde_json::json;

    #[test]
    fn event_vocabulary_rejects_unknown_names_and_nested_payloads() {
        assert!(validate("session", "unregistered", &json!({})).is_err());
        assert!(validate("session", "backend_event", &json!({"kind":"unregistered"})).is_err());
        assert!(validate("session", "document_changed", &json!({"session_id":"session", "revision":-1})).is_err());
        assert!(validate("session", "exchange_updated", &json!([])).is_err());
        assert!(validate("backend", "prompt_submission", &json!({"data":{"document":"doc","token":"1","state":"accepted"}})).is_err());
        validate("backend", "prompt_submission", &json!({"data":{"document":"doc","token":1,"state":"accepted"}})).unwrap();
        validate("backend", "runtime_resolved", &json!({"data":{"session_id":"session","provider":"Codex","model":null}})).unwrap();
    }

    #[test]
    fn serialization_cannot_bypass_the_event_contract() {
        let mut event = SessionEvent { session_id:"session".into(), event:"backend_event".into(), payload:json!({"kind":"tool-output"}) };
        assert!(!backend_visible("tool-output"));
        assert!(serde_json::to_string(&Message::Event(event.clone())).is_err());
        event.payload = json!({"kind":"execution_state","data":{"session":{"id":"session"}}});
        assert!(serde_json::to_string(&Message::Event(event)).is_ok());
    }
}
