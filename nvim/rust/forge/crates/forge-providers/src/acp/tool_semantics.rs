use anyhow::{Context, ensure};
use forge_provider_api::tool::ToolSemantics;
use forge_provider_api::{InspectableArguments, ToolDescriptor};
use forge_shell::shell::CommandShell;
use serde_json::Value;
use std::borrow::Cow;

/// Classify native ACP descriptor evidence before the immutable ItemStart is admitted.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ToolKind {
    Command,
    FileChange,
    ToolCall,
}

/// Decode provider-encoded arguments for operations while retaining the wire value for inspection.
pub(super) fn decode_arguments(raw: &Value) -> Cow<'_, Value> {
    if let Some(encoded) = raw.as_str()
        && let Ok(decoded) = serde_json::from_str(encoded)
    {
        return Cow::Owned(decoded);
    }
    Cow::Borrowed(raw)
}

/// Recognize provider command names used for display and permission normalization.
pub(super) fn is_command_name(name: &str) -> bool {
    matches!(
        tool_name(name).as_str(),
        "bash" | "powershell" | "pwsh" | "shell" | "exec_command" | "run_command" | "run_shell_command"
    )
}

/// Resolve one command and its shell from mutually consistent native argument aliases.
pub(super) fn command_arguments<'input>(
    arguments: &'input Value,
    name: &str,
) -> anyhow::Result<Option<(&'input str, Option<CommandShell>)>> {
    let mut command = None;
    for field in ["command", "cmd", "commandLine", "CommandLine"] {
        let Some(value) = arguments.get(field) else {
            continue;
        };
        let text = value
            .as_str()
            .filter(|text| !text.trim().is_empty())
            .context("ACP command argument must be a nonempty string")?;
        ensure!(
            command.is_none_or(|previous| text == previous),
            "ACP command argument aliases disagree"
        );
        command = Some(text);
    }
    let Some(command) = command else {
        return Ok(None);
    };
    let named_shell = match tool_name(name).as_str() {
        "bash" => Some(CommandShell::Bash),
        "powershell" | "pwsh" => Some(CommandShell::PowerShell),
        _ => None,
    };
    let explicit_shell = arguments
        .get("shell")
        .map(|value| {
            value
                .as_str()
                .filter(|shell| !shell.trim().is_empty())
                .map(CommandShell::from_executable)
                .context("ACP shell argument must be a nonempty string")
        })
        .transpose()?;
    ensure!(
        explicit_shell.is_none() || named_shell.is_none() || explicit_shell == named_shell,
        "ACP command name and shell argument disagree"
    );
    Ok(Some((command, explicit_shell.or(named_shell))))
}

/// Derive the initial item category and current descriptor from a merged ACP snapshot.
pub(super) fn normalize_descriptor(snapshot: &Value, workspace: &str) -> (ToolKind, ToolDescriptor) {
    let native_kind = snapshot["kind"].as_str();
    let title = snapshot["title"].as_str().unwrap_or("Tool");
    let name = snapshot["name"].as_str().unwrap_or(title);
    let raw_arguments = &snapshot["rawInput"];
    let arguments = decode_arguments(raw_arguments);
    let directory = ["cwd", "workdir"]
        .into_iter()
        .find_map(|key| arguments.get(key).and_then(Value::as_str))
        .map(str::to_owned);
    let mut descriptor = ToolDescriptor {
        name: name.into(),
        title: title.into(),
        semantics: ToolSemantics::Other,
        arguments: InspectableArguments::new(raw_arguments.clone()),
    };
    let command = if native_kind == Some("execute")
        || matches!(native_kind, Some("other") | None) && is_command_name(name)
    {
        command_arguments(&arguments, name).ok().flatten()
    } else {
        None
    };
    if let Some((text, shell)) = command {
        descriptor.title = text.to_owned();
        descriptor.semantics = ToolSemantics::Command {
            text: text.to_owned(),
            shell: shell.map(|shell| {
                match shell {
                    CommandShell::PowerShell => "powershell",
                    CommandShell::Bash => "bash",
                    CommandShell::Zsh => "zsh",
                    CommandShell::Nushell => "nushell",
                    CommandShell::Unknown => "unknown",
                }
                .to_owned()
            }),
            directory,
        };
        return (ToolKind::Command, descriptor);
    }

    let kind = match native_kind {
        Some("edit" | "delete" | "move") => ToolKind::FileChange,
        Some("execute") => ToolKind::Command,
        _ => ToolKind::ToolCall,
    };
    if native_kind == Some("read")
        || matches!(native_kind, Some("other") | None)
            && crate::tool_display::is_read_tool(&tool_name(name))
    {
        let locations = snapshot["locations"]
            .as_array()
            .into_iter()
            .flatten()
            .filter_map(|location| location["path"].as_str());
        let (title, semantics) =
            crate::tool_display::read_paths(&arguments, locations, directory, workspace);
        if let Some(title) = title {
            descriptor.title = title;
        }
        descriptor.semantics = semantics;
    }
    (kind, descriptor)
}

/// Normalize native names and legacy function-call titles for operation recognition.
fn tool_name(name: &str) -> String {
    let name = name.split_once('(').map_or(name, |(name, _)| name).trim();
    name.rsplit('.').next().unwrap_or(name).to_ascii_lowercase()
}
