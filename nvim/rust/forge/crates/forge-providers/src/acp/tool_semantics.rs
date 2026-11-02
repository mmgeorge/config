use forge_provider_api::tool::{ToolActivityKind, ToolInvocation, ToolSemantics};
use serde_json::Value;

/// Decode this integration's argument encodings before publishing operation semantics.
pub fn normalize_invocation(mut invocation: ToolInvocation) -> ToolInvocation {
    if let Some(encoded) = invocation.arguments.as_str()
        && let Ok(decoded) = serde_json::from_str(encoded)
    {
        invocation.arguments = decoded;
    }
    let arguments = &invocation.arguments;
    let string = |keys: &[&str]| {
        keys.iter()
            .find_map(|key| arguments.get(*key).and_then(Value::as_str))
            .map(str::to_owned)
    };
    let directory = string(&["cwd", "workdir"]);
    let name = invocation
        .name
        .split('(')
        .next()
        .unwrap_or(&invocation.name)
        .trim()
        .rsplit('.')
        .next()
        .unwrap_or(&invocation.name)
        .to_ascii_lowercase();
    let command = string(&["command", "cmd", "commandLine", "CommandLine"]).or_else(|| {
        (invocation.kind == ToolActivityKind::Command).then(|| invocation.title.clone())
    });
    if let Some(text) = command.filter(|_| {
        invocation.kind == ToolActivityKind::Command
            || matches!(
                name.as_str(),
                "bash" | "powershell" | "shell" | "exec_command" | "run_shell_command"
            )
    }) {
        let shell = string(&["shell"]).or_else(|| match name.as_str() {
            "bash" | "powershell" => Some(name.clone()),
            _ => None,
        });
        invocation.kind = ToolActivityKind::Command;
        invocation.title = text.clone();
        invocation.semantics = ToolSemantics::Command {
            text,
            shell,
            directory,
        };
    } else if matches!(
        name.as_str(),
        "read" | "read_file" | "readfile" | "view" | "view_file" | "viewfile"
    ) {
        let mut paths = string(&[
            "path",
            "file_path",
            "filePath",
            "absolute_path",
            "AbsolutePath",
            "TargetFile",
        ])
        .into_iter()
        .collect::<Vec<_>>();
        if let Some(locations) = arguments.get("locations").and_then(Value::as_array) {
            paths.extend(
                locations
                    .iter()
                    .filter_map(|value| value.get("path").and_then(Value::as_str))
                    .map(str::to_owned),
            );
        }
        invocation.semantics = ToolSemantics::Read { paths, directory };
    }
    invocation
}
