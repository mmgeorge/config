use super::AcpKind as ProviderKind;
use anyhow::Context;
use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};
use serde_json::Value;

/// Resolve a complete ACP tool snapshot into shared permission targets.
pub fn permission_request(kind: ProviderKind, tool: &Value) -> anyhow::Result<PermissionRequest> {
    let input = &tool["rawInput"];
    let mut target_list = Vec::new();
    let mcp_target = tool.pointer("/_meta/mcp").and_then(|metadata| {
        let name = metadata["tool"].as_str()?;
        Some(
            metadata["server"]
                .as_str()
                .map_or_else(|| name.to_owned(), |server| format!("{server}/{name}")),
        )
    });
    if let Some(target) = mcp_target {
        target_list.push(
            match target
                .strip_prefix(HOST_TOOL_SERVER)
                .and_then(|name| name.strip_prefix('/'))
            {
                Some(name) => PermissionTarget::HostTool { name: name.into() },
                None => PermissionTarget::Mcp { target },
            },
        );
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
        anyhow::ensure!(
            paths.is_empty()
                || matches!(
                    tool["kind"].as_str(),
                    Some("read" | "search" | "edit" | "delete" | "move")
                ),
            "ACP file permission omitted known read/write operation semantics"
        );
        for path in paths {
            target_list.push(if write {
                PermissionTarget::Write { path }
            } else {
                PermissionTarget::Read { path }
            });
        }
        if target_list.is_empty() {
            match tool["kind"].as_str() {
                Some("read" | "search") => target_list.push(PermissionTarget::UnknownFile {
                    operation: forge_provider_api::access::DirectoryOperation::Read,
                }),
                Some("edit" | "delete" | "move") => {
                    target_list.push(PermissionTarget::UnknownFile {
                        operation: forge_provider_api::access::DirectoryOperation::Write,
                    })
                }
                _ => target_list.extend(native_tool_targets(tool)),
            }
        }
    }
    Ok(PermissionRequest {
        id: uuid::Uuid::new_v4().to_string(),
        provider: kind.id(),
        reason: None,
        target_list,
    })
}

/// Name of the MCP server through which Harness exposes its own control tools.
pub(super) const HOST_TOOL_SERVER: &str = "forge-harness-control";

/// Normalize a native tool that carries no file, command, network, or MCP target.
///
/// Agents ask permission for their own tools, such as a tool search. Denying them as unknown
/// operations silently blocks Harness workflows, because the agent discovers deferred Harness
/// control tools through that search. Loading only Harness control tool schemas carries those
/// tools' own host authority; any other native tool asks under its native name.
fn native_tool_targets(tool: &Value) -> Vec<PermissionTarget> {
    let Some(name) = tool["title"]
        .as_str()
        .map(str::trim)
        .filter(|name| !name.is_empty())
    else {
        return vec![PermissionTarget::Unknown];
    };
    if name == "ToolSearch"
        && let Some(host_tools) = tool["rawInput"]["query"]
            .as_str()
            .and_then(|query| query.trim().strip_prefix("select:"))
            .and_then(|selection| {
                selection
                    .split(',')
                    .map(|selected| {
                        selected
                            .trim()
                            .strip_prefix("mcp__")
                            .and_then(|name| name.strip_prefix(HOST_TOOL_SERVER))
                            .and_then(|name| name.strip_prefix("__"))
                            .filter(|name| !name.is_empty())
                    })
                    .collect::<Option<Vec<_>>>()
            })
            .filter(|host_tools| !host_tools.is_empty())
    {
        return host_tools
            .into_iter()
            .map(|name| PermissionTarget::HostTool { name: name.into() })
            .collect();
    }
    vec![PermissionTarget::Tool {
        target: name.to_owned(),
    }]
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn includes_every_location_and_diff_path_and_rejects_partial_targets() -> anyhow::Result<()> {
        let request = permission_request(
            ProviderKind::Antigravity,
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
                ProviderKind::Antigravity,
                &json!({
                    "kind":"edit","locations":[{"path":"valid"},{}]
                })
            )
            .is_err()
        );
        Ok(())
    }
    /// An opaque path-bearing native operation cannot gain write authority through a read decision.
    #[test]
    fn path_permissions_require_positive_native_read_or_write_semantics() {
        for provider in [ProviderKind::Antigravity] {
            for kind in [Value::Null, json!("other"), json!("future")] {
                assert!(
                    permission_request(provider, &json!({"kind":kind, "rawInput":{"path":"file"}}))
                        .is_err()
                );
            }
            let read = permission_request(
                provider,
                &json!({"kind":"read", "rawInput":{"path":"file"}}),
            )
            .unwrap();
            assert!(matches!(
                read.target_list.as_slice(),
                [PermissionTarget::Read { .. }]
            ));
            let write = permission_request(
                provider,
                &json!({"kind":"edit", "rawInput":{"path":"file"}}),
            )
            .unwrap();
            assert!(matches!(
                write.target_list.as_slice(),
                [PermissionTarget::Write { .. }]
            ));
        }
    }
}
