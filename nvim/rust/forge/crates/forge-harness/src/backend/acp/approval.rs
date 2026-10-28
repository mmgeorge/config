use crate::backend::BackendKind;
use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
use anyhow::Context;
use serde_json::Value;

/// Resolve a complete ACP tool snapshot into shared permission targets.
pub(super) fn permission_request(
    kind: BackendKind,
    tool: &Value,
) -> anyhow::Result<PermissionRequest> {
    let input = &tool["rawInput"];
    let mut target_list = Vec::new();
    let native_tool = tool
        .pointer("/_meta/claudeCode/toolName")
        .and_then(Value::as_str);
    let mcp = tool.pointer("/_meta/mcp");
    let mcp_target = mcp
        .and_then(|metadata| {
            let name = metadata["tool"].as_str()?;
            Some(
                metadata["server"]
                    .as_str()
                    .map_or_else(|| name.to_owned(), |server| format!("{server}/{name}")),
            )
        })
        .or_else(|| {
            native_tool.and_then(|name| {
                let (server, name) = name.strip_prefix("mcp__")?.split_once("__")?;
                Some(format!("{server}/{name}"))
            })
        });
    if let Some(target) = mcp_target {
        target_list.push(PermissionTarget::Mcp { target });
    } else if tool["kind"] == "fetch" {
        target_list.push(PermissionTarget::Network {
            target: input["url"].as_str().unwrap_or("*").into(),
        });
    }
    if target_list.is_empty()
        && let Some(command) = input
            .get("command")
            .or_else(|| input.get("commandLine"))
            .or_else(|| input.get("CommandLine"))
            .and_then(Value::as_str)
    {
        target_list.push(PermissionTarget::Command {
            command: command.into(),
            shell: None,
        });
    }
    if target_list.is_empty() {
        let write = matches!(tool["kind"].as_str(), Some("edit" | "delete" | "move"));
        let mut paths = std::collections::BTreeSet::new();
        if let Some(locations) = tool.get("locations").filter(|value| !value.is_null()) {
            for location in locations
                .as_array()
                .context("ACP tool locations must be an array")?
            {
                paths.insert(
                    location["path"]
                        .as_str()
                        .filter(|path| !path.trim().is_empty())
                        .context("ACP tool location omitted path")?
                        .to_owned(),
                );
            }
        }
        if let Some(contents) = tool.get("content").and_then(Value::as_array) {
            for content in contents {
                if content["type"] == "diff" {
                    paths.insert(
                        content["path"]
                            .as_str()
                            .filter(|path| !path.trim().is_empty())
                            .context("ACP diff omitted path")?
                            .to_owned(),
                    );
                }
            }
        }
        for field in ["file_path", "path", "TargetFile", "AbsolutePath"] {
            if let Some(path) = input.get(field).filter(|value| !value.is_null()) {
                paths.insert(
                    path.as_str()
                        .filter(|path| !path.trim().is_empty())
                        .context("ACP file operation has an invalid path")?
                        .to_owned(),
                );
            }
        }
        for path in paths {
            target_list.push(if write {
                PermissionTarget::Write { path }
            } else {
                PermissionTarget::Read { path }
            });
        }
        if target_list.is_empty() {
            target_list.push(match tool["kind"].as_str() {
                Some("read" | "search") => PermissionTarget::UnknownFile {
                    operation: crate::permissions::directory::DirectoryOperation::Read,
                },
                Some("edit" | "delete" | "move") => PermissionTarget::UnknownFile {
                    operation: crate::permissions::directory::DirectoryOperation::Write,
                },
                _ => PermissionTarget::Tool {
                    target: tool["title"].as_str().unwrap_or("ACP tool").into(),
                },
            });
        }
    }
    Ok(PermissionRequest {
        id: format!("acp-{}", uuid::Uuid::new_v4()),
        provider: match kind {
            BackendKind::Claude => "claude",
            _ => "antigravity",
        }
        .into(),
        reason: tool["title"].as_str().map(str::to_owned),
        target_list,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn includes_every_location_and_diff_path_and_rejects_partial_targets() -> anyhow::Result<()> {
        let request = permission_request(
            BackendKind::Claude,
            &json!({
                "kind":"move","locations":[{"path":"before.rs"}],
                "content":[{"type":"diff","path":"after.rs"}],"rawInput":{"path":"third.rs"}
            }),
        )?;
        assert_eq!(request.target_list.len(), 3);
        assert!(request.target_list.contains(&PermissionTarget::Write {
            path: "before.rs".into()
        }));
        assert!(request.target_list.contains(&PermissionTarget::Write {
            path: "after.rs".into()
        }));
        assert!(
            permission_request(
                BackendKind::Claude,
                &json!({
                    "kind":"edit","locations":[{"path":"valid"},{}]
                })
            )
            .is_err()
        );
        Ok(())
    }
}
