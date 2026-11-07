use forge_provider_api::tool::ToolSemantics;
use forge_provider_api::{InspectableArguments, ToolDescriptor};
use serde_json::Value;

/// Classify native ACP descriptor evidence before the immutable ItemStart is admitted.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ToolKind {
    Command,
    FileChange,
    ToolCall,
}

/// Decode this integration's argument encodings before publishing operation semantics.
pub(super) fn normalize_descriptor(
    mut kind: ToolKind,
    mut descriptor: ToolDescriptor,
) -> (ToolKind, ToolDescriptor) {
    if let Some(encoded) = descriptor.arguments.value().as_str()
        && let Ok(decoded) = serde_json::from_str(encoded)
    {
        descriptor.arguments = InspectableArguments::new(decoded);
    }
    let arguments = descriptor.arguments.value();
    let string = |keys: &[&str]| {
        keys.iter()
            .find_map(|key| arguments.get(*key).and_then(Value::as_str))
            .map(str::to_owned)
    };
    let directory = string(&["cwd", "workdir"]);
    let name = descriptor
        .name
        .split('(')
        .next()
        .unwrap_or(&descriptor.name)
        .trim()
        .rsplit('.')
        .next()
        .unwrap_or(&descriptor.name)
        .to_ascii_lowercase();
    let command = string(&["command", "cmd", "commandLine", "CommandLine"])
        .or_else(|| (kind == ToolKind::Command).then(|| descriptor.title.clone()));
    if let Some(text) = command.filter(|_| {
        kind == ToolKind::Command
            || matches!(
                name.as_str(),
                "bash" | "powershell" | "shell" | "exec_command" | "run_shell_command"
            )
    }) {
        let shell = string(&["shell"]).or_else(|| match name.as_str() {
            "bash" | "powershell" => Some(name.clone()),
            _ => None,
        });
        kind = ToolKind::Command;
        descriptor.title = text.clone();
        descriptor.semantics = ToolSemantics::Command {
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
        descriptor.semantics = ToolSemantics::Read { paths, directory };
    }
    (kind, descriptor)
}
