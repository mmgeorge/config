use super::decode::NativeToolKind;
use forge_provider_api::tool::{InspectableArguments, ToolDescriptor, ToolSemantics};
use serde_json::Value;

/// Decode this integration's argument encodings before publishing operation semantics.
///
/// Titles reads by their path relative to `workspace`, the binding's workspace.
pub(super) fn normalize_descriptor(
    mut invocation: ToolDescriptor,
    kind: NativeToolKind,
    workspace: &str,
) -> ToolDescriptor {
    let mut arguments = invocation.arguments.into_value();
    if let Some(encoded) = arguments.as_str()
        && let Ok(decoded) = serde_json::from_str(encoded)
    {
        arguments = decoded;
    }
    let arguments = &arguments;
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
    let command = string(&["command", "cmd", "commandLine", "CommandLine"])
        .or_else(|| (kind == NativeToolKind::Command).then(|| invocation.title.clone()));
    if let Some(text) = command.filter(|_| {
        kind == NativeToolKind::Command
            || matches!(
                name.as_str(),
                "bash" | "powershell" | "shell" | "exec_command" | "run_shell_command"
            )
    }) {
        let mut shell = string(&["shell"]).or_else(|| match name.as_str() {
            "bash" | "powershell" => Some(name.clone()),
            _ => None,
        });
        let mut text = text;
        // A command item carries its argv joined with POSIX quoting, as its approval does; read it
        // the way approval does so the row shows the launched script rather than its launcher.
        if kind == NativeToolKind::Command {
            let params = if arguments.get("command").is_some() {
                arguments.clone()
            } else {
                serde_json::json!({ "command": text })
            };
            if let Ok((script, script_shell)) = super::approval::provider_command(&params, true) {
                text = script;
                shell = script_shell
                    .and_then(shell_name)
                    .map(str::to_owned)
                    .or(shell);
            }
        }
        invocation.title = text.clone();
        invocation.semantics = ToolSemantics::Command {
            text,
            shell,
            directory,
        };
    } else if let Some((title, semantics)) =
        crate::tool_display::read_tool(&name, arguments, directory, workspace)
    {
        if let Some(title) = title {
            invocation.title = title;
        }
        invocation.semantics = semantics;
    }
    invocation.arguments = InspectableArguments::new(arguments.clone());
    invocation
}

/// Name a shell as `ToolSemantics::Command` consumers read it back through
/// `CommandShell::from_executable`.
fn shell_name(shell: forge_shell::shell::CommandShell) -> Option<&'static str> {
    use forge_shell::shell::CommandShell;
    match shell {
        CommandShell::PowerShell => Some("powershell"),
        CommandShell::Bash => Some("bash"),
        CommandShell::Zsh => Some("zsh"),
        CommandShell::Nushell => Some("nushell"),
        CommandShell::Unknown => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A Codex command item names the script its launcher runs, in the launched shell's language.
    ///
    /// Codex joins a command's argv with POSIX quoting on every platform, so a PowerShell script
    /// containing `$false` arrives as `"…pwsh.exe" -Command "…"'$false)'`. The row read that text
    /// in the platform shell, could not take the launcher apart, and showed the quoted wrapper
    /// instead of the command.
    #[test]
    fn command_items_name_the_launched_script_and_its_shell() {
        let script = "New-Item -ItemType Directory -Force -Path notes | Out-Null\n\
            [System.IO.File]::WriteAllText('notes/wave.txt', 'wave', [System.Text.UTF8Encoding]::new($false))";
        let launcher = r"C:\Users\person\AppData\Local\Microsoft\WindowsApps\pwsh.exe";
        let joined = shlex::try_join([launcher, "-Command", script]).unwrap();
        let descriptor = normalize_descriptor(
            ToolDescriptor {
                semantics: Default::default(),
                name: "commandExecution".into(),
                title: joined.clone(),
                arguments: InspectableArguments::new(serde_json::json!({
                    "command": joined,
                    "cwd": "workspace",
                })),
            },
            NativeToolKind::Command,
            "workspace",
        );
        assert_eq!(descriptor.title, script);
        assert_eq!(
            descriptor.semantics,
            ToolSemantics::Command {
                text: script.into(),
                shell: Some("powershell".into()),
                directory: Some("workspace".into()),
            }
        );
    }
}
