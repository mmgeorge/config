//! Shared event vocabulary and payload requirements for host and editor.

use serde_json::Value;

struct Event {
    name: &'static str,
    required: &'static [RequiredField],
}

struct RequiredField {
    path: &'static [&'static str],
    label: &'static str,
    accepts: fn(&Value) -> bool,
    description: &'static str,
}

include!(concat!(env!("OUT_DIR"), "/event_contract.rs"));
const _: () = assert!(CONTRACT_VERSION == crate::WIRE_VERSION);

/// Selects only normalized backend events consumed by the editor.
pub fn backend_visible(kind: &str) -> bool {
    BACKEND.iter().any(|event| event.name == kind)
}

/// Rejects unknown event variants and missing fields before serialization.
pub fn validate(channel: &str, name: &str, payload: &Value) -> Result<(), String> {
    let events = match channel {
        "session" => SESSION,
        "backend" => BACKEND,
        "request" => REQUEST,
        "document" => DOCUMENT,
        _ => return Err(format!("unknown event channel: {channel}")),
    };
    let event = events
        .iter()
        .find(|event| event.name == name)
        .ok_or_else(|| format!("unknown {channel} event: {name}"))?;
    if !payload.is_object() {
        return Err(format!("{channel} event {name} requires an object payload"));
    }
    for field in event.required {
        let value = field
            .path
            .iter()
            .try_fold(payload, |value, key| value.get(*key));
        let valid = value.is_some_and(field.accepts);
        if !valid {
            return Err(format!(
                "{channel} event {name}: {} requires {}",
                field.label, field.description
            ));
        }
    }
    if channel == "session" && name == "backend_event" {
        validate(
            "backend",
            payload["kind"].as_str().unwrap_or_default(),
            payload,
        )?;
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
        assert!(
            validate(
                "session",
                "document_changed",
                &json!({"session_id":"session", "revision":-1})
            )
            .is_err()
        );
        assert!(validate("session", "exchange_updated", &json!([])).is_err());
        assert!(
            validate(
                "backend",
                "prompt_submission",
                &json!({"data":{"document":"doc","token":"1","state":"accepted"}})
            )
            .is_err()
        );
        validate(
            "backend",
            "prompt_submission",
            &json!({"data":{"document":"doc","token":1,"state":"accepted"}}),
        )
        .unwrap();
        validate(
            "backend",
            "runtime_resolved",
            &json!({"data":{"session_id":"session","provider":"Codex","model":null}}),
        )
        .unwrap();
        validate(
            "backend",
            "prompt_submission",
            &json!({"data":{"document":null,"token":2,"state":"accepted"}}),
        )
        .unwrap();
        assert!(
            validate(
                "backend",
                "prompt_submission",
                &json!({"data":{"document":null,"token":"2","state":"accepted"}}),
            )
            .is_err()
        );
    }

    #[test]
    fn serialization_cannot_bypass_the_event_contract() {
        let mut event = SessionEvent {
            session_id: "session".into(),
            event: "backend_event".into(),
            payload: json!({"kind":"tool-output"}),
        };
        assert!(!backend_visible("tool-output"));
        assert!(serde_json::to_string(&Message::Event(event.clone())).is_err());
        event.payload = json!({"kind":"execution_state","data":{"session":{"id":"session"}}});
        assert!(serde_json::to_string(&Message::Event(event)).is_ok());
    }
}
