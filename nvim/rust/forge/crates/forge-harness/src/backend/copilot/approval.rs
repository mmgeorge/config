use crate::backend::permission_input::{
    normalize_permission_payload, provider_path, provider_shell,
};
use crate::permissions::directory::DirectoryOperation;
use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
use serde_json::Value;

/// Normalize one Copilot SDK permission callback into the Harness permission model.
pub(crate) fn permission_from_copilot(
    id: String,
    kind: &str,
    params: Value,
    _workspace: &str,
) -> PermissionRequest {
    // Notifications wrap the same request that older RPC calls sent directly.
    let params = params.get("permissionRequest").cloned().unwrap_or(params);
    let kind = params.get("kind").and_then(Value::as_str).unwrap_or(kind);
    let encoded = params.to_string();
    let target_list = match kind {
        "shell" => vec![PermissionTarget::Command {
            command: params
                .get("fullCommandText")
                .or_else(|| params.get("command"))
                .or_else(|| params.get("fullCommand"))
                .or_else(|| params.get("full_command"))
                .and_then(Value::as_str)
                .unwrap_or(&encoded)
                .to_owned(),
            shell: provider_shell(&params),
        }],
        "write" => vec![provider_path(&params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Write,
            },
            |path| PermissionTarget::Write { path },
        )],
        "read" => vec![provider_path(&params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Read,
            },
            |path| PermissionTarget::Read { path },
        )],
        "url" => vec![PermissionTarget::Network {
            target: params
                .get("url")
                .or_else(|| params.get("target"))
                .and_then(Value::as_str)
                .unwrap_or("*")
                .to_owned(),
        }],
        "mcp" => vec![PermissionTarget::Mcp {
            target: {
                let tool = params
                    .get("toolName")
                    .or_else(|| params.get("tool_name"))
                    .or_else(|| params.get("name"))
                    .and_then(Value::as_str)
                    .unwrap_or("*");
                params
                    .get("serverName")
                    .and_then(Value::as_str)
                    .map_or_else(|| tool.to_owned(), |server| format!("{server}/{tool}"))
            },
        }],
        "custom-tool" | "hook" => vec![PermissionTarget::Tool {
            target: params
                .get("toolName")
                .and_then(Value::as_str)
                .unwrap_or(kind)
                .to_owned(),
        }],
        _ => normalize_permission_payload(&params, &encoded),
    };
    PermissionRequest {
        id,
        provider: "Copilot CLI".into(),
        reason: params
            .get("reason")
            .or_else(|| params.get("intention"))
            .or_else(|| params.get("warning"))
            .or_else(|| params.get("hookMessage"))
            .or_else(|| params.get("toolDescription"))
            .and_then(Value::as_str)
            .map(str::to_owned),
        target_list,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn callback_paths_and_operation_are_preserved_without_title_inference() {
        for operation in ["read", "write"] {
            let request = permission_from_copilot(
                "request".into(),
                operation,
                json!({"permissionRequest":{"kind":operation,"fileName":"folder/file.rs"}}),
                ".",
            );
            let expected = if operation == "write" {
                PermissionTarget::Write {
                    path: "folder/file.rs".into(),
                }
            } else {
                PermissionTarget::Read {
                    path: "folder/file.rs".into(),
                }
            };
            assert_eq!(request.target_list, vec![expected]);
        }
        let request = permission_from_copilot(
            "request".into(),
            "write",
            json!({"title":"edit apparently-allowed.rs"}),
            ".",
        );
        assert!(matches!(
            request.target_list[0],
            PermissionTarget::UnknownFile { .. }
        ));
    }
}
