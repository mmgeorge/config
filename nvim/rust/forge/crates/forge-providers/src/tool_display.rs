//! Decode and redact native tool arguments for normalized display titles.

use forge_provider_api::tool::ToolSemantics;
use serde_json::Value;

/// Recognize a native read tool by its normalized lowercase name.
///
/// Returns the read's semantics and, when it names any path, its display title: `Read` followed
/// by each path, relative to `workspace` when inside it and as reported otherwise. Native titles
/// such as `view({"path":"C:\\…"})` repeat the arguments as escaped JSON, so every adapter titles
/// reads here instead.
pub(crate) fn read_tool(
    name: &str,
    arguments: &Value,
    directory: Option<String>,
    workspace: &str,
) -> Option<(Option<String>, ToolSemantics)> {
    if !is_read_tool(name) {
        return None;
    }
    Some(read_paths(arguments, [], directory, workspace))
}

/// Recognize read tool names independently of the provider's argument representation.
pub(crate) fn is_read_tool(name: &str) -> bool {
    matches!(
        name,
        "read" | "read_file" | "readfile" | "view" | "view_file" | "viewfile"
    )
}

/// Build a read descriptor from native file locations and recognized argument paths.
pub(crate) fn read_paths<'location>(
    arguments: &'location Value,
    native_locations: impl IntoIterator<Item = &'location str>,
    directory: Option<String>,
    workspace: &str,
) -> (Option<String>, ToolSemantics) {
    let argument_path = [
        "path",
        "file_path",
        "filePath",
        "absolute_path",
        "AbsolutePath",
        "TargetFile",
    ]
    .iter()
    .find_map(|key| arguments.get(*key).and_then(Value::as_str));
    let argument_locations = arguments["locations"]
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|location| location["path"].as_str());
    let mut seen = std::collections::HashSet::new();
    let paths = argument_path
        .into_iter()
        .chain(argument_locations)
        .chain(native_locations)
        .filter(|path| !path.is_empty() && seen.insert(*path))
        .map(str::to_owned)
        .collect::<Vec<_>>();
    let title = (!paths.is_empty()).then(|| {
        let shown = paths
            .iter()
            .map(|path| workspace_relative(path, workspace).unwrap_or_else(|| path.clone()))
            .collect::<Vec<_>>();
        format!("Read {}", shown.join(", "))
    });
    (title, ToolSemantics::Read { paths, directory })
}

/// Express a reported path relative to the workspace, with `/` separators, when it is inside.
///
/// Copilot sometimes drops the leading `/` of a workspace path in Git headers, so a path that
/// starts with the workspace minus that slash also counts as inside.
pub(crate) fn workspace_relative(path: &str, workspace: &str) -> Option<String> {
    let path = display_path(path);
    let workspace = display_path(workspace);
    let workspace = workspace.trim_end_matches('/');
    if workspace.is_empty() || path.split('/').any(|component| component == "..") {
        return None;
    }
    let windows = workspace.as_bytes().get(1) == Some(&b':') || workspace.starts_with("//");
    [workspace, workspace.trim_start_matches('/')]
        .into_iter()
        .filter(|prefix| !windows || *prefix == workspace)
        .find_map(|prefix| {
            let suffix = if windows {
                let (path_prefix, suffix) = path.split_at_checked(prefix.len())?;
                path_prefix.eq_ignore_ascii_case(prefix).then_some(suffix)
            } else {
                path.strip_prefix(prefix)
            };
            suffix
                .and_then(|suffix| suffix.strip_prefix('/'))
                .map(str::to_owned)
        })
}

/// Normalize Windows separator and verbatim spellings for display comparison.
fn display_path(path: &str) -> String {
    let path = path.replace('\\', "/");
    if let Some(suffix) = path.strip_prefix("//?/UNC/") {
        format!("//{suffix}")
    } else if let Some(suffix) = path.strip_prefix("//?/") {
        suffix.to_owned()
    } else {
        path
    }
}

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
    use forge_provider_api::tool::ToolSemantics;
    use serde_json::json;

    /// A read inside the workspace is titled by its workspace-relative path.
    #[test]
    fn read_title_uses_the_workspace_relative_path() {
        let (title, semantics) = read_tool(
            "view",
            &json!({"path":"C:\\work\\project\\src\\money.ts"}),
            None,
            "C:\\work\\project",
        )
        .expect("view is a read tool");
        assert_eq!(title.as_deref(), Some("Read src/money.ts"));
        assert_eq!(
            semantics,
            ToolSemantics::Read {
                paths: vec!["C:\\work\\project\\src\\money.ts".into()],
                directory: None,
            }
        );
    }

    /// A read outside the workspace keeps its absolute path, and other tools are not reads.
    #[test]
    fn read_title_keeps_paths_outside_the_workspace() {
        let (title, _) = read_tool(
            "read_file",
            &json!({"file_path":"C:/elsewhere/notes.md"}),
            None,
            "C:/work/project",
        )
        .expect("read_file is a read tool");
        assert_eq!(title.as_deref(), Some("Read C:/elsewhere/notes.md"));
        let (title, _) =
            read_tool("view", &json!({}), None, "C:/work/project").expect("view is a read tool");
        assert_eq!(title, None);
        assert!(read_tool("grep", &json!({"path":"src"}), None, "C:/work/project").is_none());
    }

    /// Compare Windows path spellings without altering the retained path.
    #[test]
    fn windows_read_title_accepts_case_and_verbatim_path_variants() {
        let (title, semantics) = read_tool(
            "view",
            &json!({"path":r"\\?\d:\config\README.md"}),
            None,
            r"D:\Config",
        )
        .expect("view is a read tool");
        assert_eq!(title.as_deref(), Some("Read README.md"));
        assert_eq!(
            semantics,
            ToolSemantics::Read {
                paths: vec![r"\\?\d:\config\README.md".into()],
                directory: None,
            }
        );
    }
}
