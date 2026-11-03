use forge_provider_api::access::DirectoryOperation;
use forge_provider_api::permissions::PermissionTarget;
use forge_shell::shell::CommandShell;
use serde_json::Value;

pub fn provider_shell(params: &Value) -> Option<CommandShell> {
    params
        .get("shell")
        .or_else(|| params.get("shellPath"))
        .and_then(Value::as_str)
        .map(CommandShell::from_executable)
}

pub fn provider_path(params: &Value) -> Option<String> {
    params
        .get("path")
        .and_then(Value::as_str)
        .or_else(|| params.get("fileName").and_then(Value::as_str))
        .or_else(|| params.get("file_name").and_then(Value::as_str))
        .or_else(|| params.get("filePath").and_then(Value::as_str))
        .or_else(|| params.get("file_path").and_then(Value::as_str))
        .or_else(|| params.pointer("/file/path").and_then(Value::as_str))
        .filter(|path| !path.trim().is_empty())
        .map(str::to_owned)
}

/// Decode structured operation fields without using display text as authorization evidence.
pub fn normalize_permission_payload(params: &Value) -> Vec<PermissionTarget> {
    if let Some(tool_call) = params.get("toolCall") {
        let input = &tool_call["rawInput"];
        let target = match tool_call.get("kind").and_then(Value::as_str) {
            Some("read" | "search") => PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Read,
            },
            Some("edit" | "delete" | "move") => PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Write,
            },
            Some("execute") => input
                .as_str()
                .or_else(|| input.get("command").and_then(Value::as_str))
                .filter(|command| !command.trim().is_empty())
                .map_or(PermissionTarget::Unknown, |command| {
                    PermissionTarget::Command {
                        command: command.to_owned(),
                        shell: provider_shell(params),
                    }
                }),
            Some("fetch") => input
                .get("url")
                .and_then(Value::as_str)
                .filter(|url| !url.trim().is_empty())
                .map_or(PermissionTarget::Unknown, |url| PermissionTarget::Network {
                    target: url.to_owned(),
                }),
            Some("mcp") => match (
                input.get("serverName").and_then(Value::as_str),
                input.get("toolName").and_then(Value::as_str),
            ) {
                (Some(server), Some(tool)) if !server.is_empty() && !tool.is_empty() => {
                    PermissionTarget::Mcp {
                        target: format!("{server}/{tool}"),
                    }
                }
                _ => PermissionTarget::Unknown,
            },
            _ => PermissionTarget::Unknown,
        };
        return vec![target];
    }
    let mut targets = Vec::new();
    for (operation, pointer) in [
        (DirectoryOperation::Read, "/permissions/fileSystem/read"),
        (DirectoryOperation::Write, "/permissions/fileSystem/write"),
    ] {
        if let Some(paths) = params.pointer(pointer) {
            let Some(paths) = paths.as_array() else {
                targets.push(PermissionTarget::Unknown);
                continue;
            };
            for path in paths {
                let target = match path.as_str().filter(|path| !path.trim().is_empty()) {
                    Some(path) => match operation {
                        DirectoryOperation::Read => PermissionTarget::Read {
                            path: path.to_owned(),
                        },
                        DirectoryOperation::Write => PermissionTarget::Write {
                            path: path.to_owned(),
                        },
                    },
                    None => PermissionTarget::Unknown,
                };
                targets.push(target);
            }
        }
    }
    if let Some(network) = params
        .pointer("/permissions/network")
        .filter(|value| !value.is_null() && **value != Value::Bool(false))
    {
        let target = network
            .get("host")
            .and_then(Value::as_str)
            .filter(|host| !host.trim().is_empty())
            .map(str::to_owned)
            .or_else(|| (network == &Value::Bool(true)).then(|| "*".to_owned()));
        targets.push(target.map_or(PermissionTarget::Unknown, |target| {
            PermissionTarget::Network { target }
        }));
    }
    if targets.is_empty() {
        targets.push(PermissionTarget::Unknown);
    }
    targets
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Keep native descriptions and payloads out of normalized permission fallbacks.
    #[test]
    fn unknown_operations_never_publish_native_payloads() {
        for payload in [
            json!({"unrecognized":"network private-native-payload"}),
            json!({"toolCall":{"kind":"execute","title":"private-native-payload","rawInput":{"unexpected":true}}}),
            json!({"toolCall":{"kind":"future-operation","title":"private-native-payload"}}),
            json!({"permissions":{"network":{"unexpected":"private-native-payload"}}}),
        ] {
            let targets = normalize_permission_payload(&payload);
            assert_eq!(targets, vec![PermissionTarget::Unknown]);
            assert!(
                !serde_json::to_string(&targets)
                    .unwrap()
                    .contains("private-native-payload")
            );
        }
    }

    /// Preserve known operations and reject any malformed member of a permission set.
    #[test]
    fn structured_targets_preserve_complete_operations() {
        assert_eq!(
            normalize_permission_payload(&json!({
                "permissions":{"fileSystem":{"read":["one"],"write":["two",null]},"network":true}
            })),
            vec![
                PermissionTarget::Read { path: "one".into() },
                PermissionTarget::Write { path: "two".into() },
                PermissionTarget::Unknown,
                PermissionTarget::Network { target: "*".into() },
            ]
        );
        assert_eq!(
            normalize_permission_payload(&json!({
                "toolCall":{"kind":"execute","rawInput":{"command":"echo safe"}}
            })),
            vec![PermissionTarget::Command {
                command: "echo safe".into(),
                shell: None
            }]
        );
    }
}
