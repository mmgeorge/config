//! Human-readable check comparisons. Exact raw evidence remains available in the same review.

use anyhow::Result;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};

/// Describe each changed check independently, comparing normalized defaults rather than JSON layout.
pub(super) fn describe(before: &Value, after: &Value) -> Result<String> {
    let previous = checks(before)?;
    let proposed = checks(after)?;
    let names: BTreeSet<_> = previous.keys().chain(proposed.keys()).collect();
    let mut sections = Vec::new();
    let mut counts = [0; 3];
    for name in names {
        let old = previous.get(name);
        let new = proposed.get(name);
        if old == new {
            continue;
        }
        let (status, ordinal) = match (old, new) {
            (None, _) => ("Added", 0),
            (_, None) => ("Removed", 1),
            _ => ("Modified", 2),
        };
        counts[ordinal] += 1;
        let old = old.map(fields).transpose()?.unwrap_or_default();
        let new = new.map(fields).transpose()?.unwrap_or_default();
        let labels: BTreeSet<_> = old.keys().chain(new.keys()).collect();
        let mut body = String::new();
        for label in labels {
            let previous = old.get(label);
            let proposed = new.get(label);
            if previous == proposed {
                continue;
            }
            body.push_str(&format!(" {label}\n"));
            // Retain unchanged array elements and their order, especially executable arguments.
            let previous = previous.map(String::as_str).unwrap_or("");
            let proposed = proposed.map(String::as_str).unwrap_or("");
            for change in similar::TextDiff::from_lines(previous, proposed).iter_all_changes() {
                body.push(match change.tag() {
                    similar::ChangeTag::Delete => '-',
                    similar::ChangeTag::Insert => '+',
                    similar::ChangeTag::Equal => ' ',
                });
                body.push_str(change.value());
            }
        }
        // The hunk ranges describe presentation rows, not editable source coordinates.
        let old_count = body.lines().filter(|line| !line.starts_with('+')).count();
        let new_count = body.lines().filter(|line| !line.starts_with('-')).count();
        let diff = format!("--- {name}\n+++ {name}\n@@ -1,{old_count} +1,{new_count} @@\n{body}");
        sections.push(format!(
            "### Check: {name} ({status})\n{}",
            fenced("diff", &diff)
        ));
    }
    Ok(format!(
        "{} added · {} removed · {} modified\n\n{}",
        counts[0],
        counts[1],
        counts[2],
        sections.join("\n")
    ))
}

/// Choose a Markdown fence longer than every backtick run in the retained source.
pub(super) fn fenced(language: &str, source: &str) -> String {
    let longest = source
        .split(|character| character != '`')
        .map(str::len)
        .max()
        .unwrap_or(0);
    let fence = "`".repeat(longest.saturating_add(1).max(3));
    format!(
        "{fence}{language}\n{source}{}{fence}\n",
        if source.ends_with('\n') { "" } else { "\n" }
    )
}

/// Normalize authored defaults through the configuration decoder before comparison.
fn checks(value: &Value) -> Result<BTreeMap<String, super::spec::Check>> {
    if value.is_null() {
        return Ok(BTreeMap::new());
    }
    Ok(serde_json::from_value(value.clone())?)
}

/// Flatten every schema field into a readable path while preserving exact string and array values.
fn fields(check: &super::spec::Check) -> Result<BTreeMap<String, String>> {
    let mut result = BTreeMap::new();
    let mut value = serde_json::to_value(check)?;
    value["enabled"] = Value::Bool(check.enabled);
    value["trigger"]["auto"] = Value::Bool(check.trigger.auto);
    value["action"]["directory"] = Value::String(check.action.directory.clone());
    value["action"]["after"] = serde_json::to_value(&check.action.after)?;
    flatten("", &value, &mut result)?;
    Ok(result)
}

/// Use JSON scalar escaping so embedded newlines cannot masquerade as another review section.
fn flatten(path: &str, value: &Value, result: &mut BTreeMap<String, String>) -> Result<()> {
    match value {
        Value::Object(properties) if !properties.is_empty() => {
            for (name, child) in properties {
                let label = if path.is_empty() {
                    name.clone()
                } else {
                    format!("{path}.{name}")
                };
                flatten(&label, child, result)?;
            }
        }
        Value::Array(values) => {
            let text = if values.is_empty() {
                "  []\n".to_owned()
            } else {
                values
                    .iter()
                    .map(|value| Ok(format!("  {}\n", serde_json::to_string(value)?)))
                    .collect::<Result<String>>()?
            };
            result.insert(label(path).to_owned(), text);
        }
        _ => {
            result.insert(
                label(path).to_owned(),
                format!("  {}\n", serde_json::to_string(value)?),
            );
        }
    }
    Ok(())
}

/// Name common fields while leaving adapter options and input definitions fully qualified.
fn label(path: &str) -> &str {
    match path {
        "enabled" => "Enabled",
        "trigger.auto" => "Automatic",
        "trigger.paths" => "Paths",
        "trigger.exclude" => "Excluded paths",
        "action.category" => "Category",
        "action.label" => "Label",
        "action.directory" => "Directory",
        "action.after" => "Requires",
        "action.command" => "Command",
        "action.adapter.script.command" => "Script command",
        "action.prepare" => "Preparation",
        _ => path,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn review_fences_retain_helper_fences_as_source() {
        assert_eq!(
            fenced("diff", "+```\n+### text\n"),
            "````diff\n+```\n+### text\n````\n"
        );
    }

    #[test]
    fn check_review_preserves_arguments_defaults_and_nested_options() -> Result<()> {
        let old = json!({"lua-test":{"trigger":{},"action":{"category":"test",
            "after":["rust-build"],"adapter":{"script":{"command":["nu","file with spaces.nu"]}}}}});
        let mut new = old.clone();
        new["lua-test"]["action"]["after"] = json!([]);
        new["lua-test"]["action"]["adapter"]["script"]["command"] =
            json!(["nu", "file with spaces.nu", "--partition", "non-host"]);
        new["lua-test"]["enabled"] = json!(false);
        let review = describe(&old, &new)?;
        assert!(review.contains("Check: lua-test (Modified)"));
        assert!(review.contains("-  \"rust-build\"\n+  []"));
        assert!(review.contains("   \"file with spaces.nu\"\n+  \"--partition\""));
        assert!(review.contains(" Enabled\n-  true\n+  false"));
        assert!(!review.contains(" Directory\n"));
        let defaults = json!({"lua-test":{"enabled":true,"trigger":{"auto":false},"action":{"category":"test",
            "directory":".","after":["rust-build"],"adapter":{"script":{"command":["nu","file with spaces.nu"]}}}}});
        assert_eq!(
            describe(&old, &defaults)?,
            "0 added · 0 removed · 0 modified\n\n"
        );
        Ok(())
    }

    #[test]
    fn check_review_additions_removals_and_escaped_values_are_complete() -> Result<()> {
        let old =
            json!({"removed":{"trigger":{},"action":{"category":"custom","command":["old"]}}});
        let new = json!({"added":{"trigger":{"paths":["**/*.lua"]},"action":{"category":"custom",
            "command":["nu","line\n### forged"],"inputs":{"message":{"type":"string","description":"text","default":"λ"}}}}});
        let review = describe(&old, &new)?;
        assert!(review.starts_with("1 added · 1 removed · 0 modified"));
        assert!(review.contains("Check: added (Added)"));
        assert!(review.contains("Check: removed (Removed)"));
        assert!(review.contains("+  \"line\\n### forged\""));
        assert!(review.contains(" action.inputs.message.default\n+  \"λ\""));
        assert!(review.contains(" Enabled\n+  true"));
        Ok(())
    }
}
