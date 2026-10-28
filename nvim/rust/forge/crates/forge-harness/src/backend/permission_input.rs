use crate::permissions::directory::DirectoryOperation;
use crate::permissions::matcher::PermissionTarget;
use crate::permissions::shell::CommandShell;
use serde_json::Value;

pub(super) fn provider_shell(params: &Value) -> Option<CommandShell> {
    params
        .get("shell")
        .or_else(|| params.get("shellPath"))
        .and_then(Value::as_str)
        .map(CommandShell::from_executable)
}

pub(super) fn provider_path(params: &Value) -> Option<String> {
    params
        .get("path")
        .and_then(Value::as_str)
        .or_else(|| params.get("fileName").and_then(Value::as_str))
        .or_else(|| params.get("file_name").and_then(Value::as_str))
        .or_else(|| params.get("filePath").and_then(Value::as_str))
        .or_else(|| params.get("file_path").and_then(Value::as_str))
        .or_else(|| params.pointer("/file/path").and_then(Value::as_str))
        .map(str::to_owned)
}

pub(super) fn normalize_permission_payload(params: &Value, encoded: &str) -> Vec<PermissionTarget> {
    if let Some(tool_call) = params.get("toolCall") {
        let kind = tool_call
            .get("kind")
            .and_then(Value::as_str)
            .unwrap_or("tool");
        let title = tool_call
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or(kind);
        return match kind {
            "read" | "search" => vec![PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Read,
            }],
            "edit" | "delete" | "move" => vec![PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Write,
            }],
            "execute" => vec![PermissionTarget::Command {
                command: tool_call
                    .get("rawInput")
                    .map(Value::to_string)
                    .unwrap_or_else(|| title.into()),
                shell: provider_shell(params),
            }],
            "fetch" => vec![PermissionTarget::Network {
                target: title.into(),
            }],
            "mcp" => vec![PermissionTarget::Mcp {
                target: title.into(),
            }],
            _ => vec![PermissionTarget::Tool {
                target: format!("{kind}/{title}"),
            }],
        };
    }
    let mut target_list = Vec::new();
    for (namespace, pointer) in [
        ("read", "/permissions/fileSystem/read"),
        ("write", "/permissions/fileSystem/write"),
    ] {
        if let Some(path_list) = params.pointer(pointer).and_then(Value::as_array) {
            for path in path_list.iter().filter_map(Value::as_str) {
                target_list.push(if namespace == "read" {
                    PermissionTarget::Read { path: path.into() }
                } else {
                    PermissionTarget::Write { path: path.into() }
                });
            }
        }
    }
    let network = params.pointer("/permissions/network");
    if network.is_some_and(|value| !value.is_null())
        || encoded.to_ascii_lowercase().contains("network")
    {
        target_list.push(PermissionTarget::Network {
            target: network
                .and_then(|value| value.get("host"))
                .and_then(Value::as_str)
                .unwrap_or("*")
                .into(),
        });
    }
    if target_list.is_empty() {
        target_list.push(PermissionTarget::Tool {
            target: "provider/permission".into(),
        });
    }
    target_list
}
