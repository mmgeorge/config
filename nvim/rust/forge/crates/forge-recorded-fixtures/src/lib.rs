//! Comparison of Rust protocol output with the recorded fixtures that Lua tests read.
//!
//! Lua adapter tests replay host responses from JSON fixtures committed under
//! `nvim/lua/forge/tests/fixtures/protocol/`. A Rust test that produces the same payload asserts
//! that its output still equals the recording, so a change to the Rust-to-Lua wire format fails
//! loudly instead of leaving the Lua tests passing against an outdated payload.
//!
//! Rust tests never write the recorded files. On a mismatch, [`assert_matches_recording`] writes
//! the new output to the OS temporary directory and panics with the command that copies it over
//! the recording. Re-recording is therefore a deliberate step: copy the file, then run the Lua
//! tests that read it.
//!
//! Before comparing, the output is normalized so the recording is stable across runs:
//! microsecond timings (`*_us` keys) become zero, every UUID inside a string becomes
//! `00000000-0000-0000-0000-000000000001`, `...-000000000002` and so on in order of first
//! appearance, `workspace` paths become `<workspace>`, and wall-clock ages such as
//! `1089 days ago` become `<relative time>`. Replacing only the UUID keeps the structure of
//! identifiers such as `review-<uuid>-1`, which Lua code parses. Ages are measured from the
//! current time to a fixture commit's fixed timestamp, so they grow while the payload stays
//! the same.

use std::collections::HashMap;
use std::path::PathBuf;

use serde_json::Value;

/// Returns the committed path of the recorded fixture named `file_name`.
pub fn recording_path(file_name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../../lua/forge/tests/fixtures/protocol")
        .join(file_name)
}

/// Asserts that `output`, once normalized, equals the recorded fixture named `file_name`.
///
/// # Panics
///
/// Panics when the recording is missing, unreadable, or different from the output. The new
/// output is first written under the OS temporary directory, and the message names it.
pub fn assert_matches_recording(file_name: &str, output: &Value) {
    let normalized = normalize(output);
    let recording = recording_path(file_name);
    let recorded = std::fs::read_to_string(&recording)
        .ok()
        .and_then(|text| serde_json::from_str::<Value>(&text).ok());
    if recorded.as_ref() == Some(&normalized) {
        return;
    }
    let candidate_directory = std::env::temp_dir().join("forge").join("recorded-fixtures");
    std::fs::create_dir_all(&candidate_directory)
        .expect("create recorded-fixture candidate directory");
    let candidate = candidate_directory.join(file_name);
    let mut encoded = serde_json::to_string_pretty(&normalized).expect("encode recorded fixture");
    encoded.push('\n');
    std::fs::write(&candidate, encoded).expect("write recorded-fixture candidate");
    let state = if recorded.is_some() {
        "no longer matches"
    } else {
        "is missing or unreadable"
    };
    panic!(
        "recorded protocol fixture `{file_name}` {state}.\n\
         If the Rust-to-Lua wire change is intended, record the new output and rerun the Lua \
         tests that read it:\n  cp '{}' '{}'",
        candidate.display(),
        std::path::absolute(&recording)
            .unwrap_or(recording.clone())
            .display(),
    );
}

/// Replaces values that change between identical runs with stable placeholders.
fn normalize(output: &Value) -> Value {
    let mut identifiers = HashMap::new();
    normalize_value(output, &mut identifiers)
}

fn normalize_value(value: &Value, identifiers: &mut HashMap<String, String>) -> Value {
    match value {
        Value::Object(fields) => Value::Object(
            fields
                .iter()
                .map(|(key, field)| {
                    let normalized = match (key.as_str(), field) {
                        (name, Value::Number(_)) if name.ends_with("_us") => Value::from(0),
                        ("workspace", Value::String(_)) => Value::String("<workspace>".into()),
                        _ => normalize_value(field, identifiers),
                    };
                    (key.clone(), normalized)
                })
                .collect(),
        ),
        Value::Array(items) => Value::Array(
            items
                .iter()
                .map(|item| normalize_value(item, identifiers))
                .collect(),
        ),
        Value::String(text) if is_relative_age(text) => Value::String("<relative time>".into()),
        Value::String(text) => Value::String(replace_uuids(text, identifiers)),
        other => other.clone(),
    }
}

/// Matches the complete `<count> <unit> ago` text that Status renders for a commit's age.
fn is_relative_age(text: &str) -> bool {
    let mut words = text.split(' ');
    matches!(
        (words.next(), words.next(), words.next(), words.next()),
        (Some(count), Some("seconds" | "minutes" | "hours" | "days"), Some("ago"), None)
            if !count.is_empty() && count.bytes().all(|byte| byte.is_ascii_digit())
    )
}

/// Replaces each UUID in `text` with a placeholder that is stable for the whole fixture.
fn replace_uuids(text: &str, identifiers: &mut HashMap<String, String>) -> String {
    const GROUPS: [usize; 5] = [8, 4, 4, 4, 12];
    const LENGTH: usize = 36;
    let bytes = text.as_bytes();
    let mut replaced = String::with_capacity(text.len());
    let mut index = 0;
    while index < bytes.len() {
        if index + LENGTH <= bytes.len() && is_uuid(&bytes[index..index + LENGTH], &GROUPS) {
            let found = &text[index..index + LENGTH];
            let next = identifiers.len() + 1;
            let placeholder = identifiers
                .entry(found.to_owned())
                .or_insert_with(|| format!("00000000-0000-0000-0000-{next:012}"));
            replaced.push_str(placeholder);
            index += LENGTH;
        } else {
            let character = text[index..]
                .chars()
                .next()
                .expect("index is a character boundary");
            replaced.push(character);
            index += character.len_utf8();
        }
    }
    replaced
}

fn is_uuid(candidate: &[u8], groups: &[usize]) -> bool {
    let mut position = 0;
    for (group, length) in groups.iter().enumerate() {
        if group > 0 {
            if candidate[position] != b'-' {
                return false;
            }
            position += 1;
        }
        if !candidate[position..position + length]
            .iter()
            .all(u8::is_ascii_hexdigit)
        {
            return false;
        }
        position += length;
    }
    true
}
