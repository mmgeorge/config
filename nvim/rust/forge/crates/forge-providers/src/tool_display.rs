//! Decode and redact native tool arguments for normalized display titles.

use serde_json::Value;

/// Format native arguments after decoding and redacting credential fields.
/// Reject empty names and invalid encoded JSON.
pub(crate) fn call_title(name: &str, arguments: &Value) -> Option<String> {
    let name = name.split_whitespace().collect::<Vec<_>>().join(" ");
    if name.is_empty() {
        return None;
    }
    let arguments = match arguments {
        Value::String(encoded) => serde_json::from_str(encoded).ok()?,
        arguments => arguments.clone(),
    };
    let mut arguments = redact_arguments(&arguments);
    arguments.sort_all_objects();
    Some(format!(
        "{name}({})",
        serde_json::to_string(&arguments).ok()?
    ))
}

/// Redact credentials recursively before displaying native tool arguments.
fn redact_arguments(value: &Value) -> Value {
    match value {
        Value::Array(values) => Value::Array(values.iter().map(redact_arguments).collect()),
        Value::Object(values) => Value::Object(
            values
                .iter()
                .map(|(key, value)| {
                    let value = if sensitive_argument(key) {
                        Value::String("[REDACTED]".into())
                    } else {
                        redact_arguments(value)
                    };
                    (key.clone(), value)
                })
                .collect(),
        ),
        _ => value.clone(),
    }
}

/// Recognize credential-bearing native argument names.
fn sensitive_argument(key: &str) -> bool {
    matches!(
        key.to_ascii_lowercase().replace(['-', '_'], "").as_str(),
        "token"
            | "apikey"
            | "key"
            | "secret"
            | "password"
            | "passphrase"
            | "authorization"
            | "bearer"
            | "cookie"
            | "session"
            | "credential"
            | "accesstoken"
            | "refreshtoken"
            | "clientsecret"
            | "privatekey"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep encoded native credentials out of the title while retaining useful arguments.
    #[test]
    fn native_encoded_arguments_are_redacted_before_display() {
        let arguments = serde_json::json!({
            "query": "lookup", "nested": [{"api_key": "native-secret-marker"}]
        });
        let title = call_title("search", &Value::String(arguments.to_string())).unwrap();
        assert!(title.contains("lookup"));
        assert!(title.contains("[REDACTED]"));
        assert!(!title.contains("native-secret-marker"));
        assert_eq!(call_title("search", &arguments).as_ref(), Some(&title));
        assert!(call_title("search", &Value::String("invalid native JSON".into())).is_none());
    }
}
