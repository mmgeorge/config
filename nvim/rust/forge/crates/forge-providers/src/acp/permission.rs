use super::AcpKind as ProviderKind;
use anyhow::Context;
use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};
use serde_json::Value;

/// Resolve a complete ACP tool snapshot into shared permission targets.
pub fn permission_request(kind: ProviderKind, tool: &Value) -> anyhow::Result<PermissionRequest> {
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
                _ => target_list.extend(native_tool_targets(native_tool, tool)),
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
/// Claude Code asks permission for its own tools such as `ToolSearch` and `Skill`. Denying them
/// as unknown operations silently blocks Harness workflows, because the agent discovers deferred
/// Harness control tools through `ToolSearch`. Loading only Harness control tool schemas carries
/// those tools' own host authority; any other native tool asks under its native name.
fn native_tool_targets(native_tool: Option<&str>, tool: &Value) -> Vec<PermissionTarget> {
    let Some(name) = native_tool
        .or_else(|| tool["title"].as_str())
        .map(str::trim)
        .filter(|name| !name.is_empty())
    else {
        return vec![PermissionTarget::Unknown];
    };
    if name == "ToolSearch"
        && let Some(host_tools) = tool["rawInput"]["query"]
            .as_str()
            .and_then(|query| query.trim().strip_prefix("select:"))
            .map(|selection| {
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
            .flatten()
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

    /// Normalize provider tool aliases before invoking host permission policy.
    #[test]
    fn control_alias_is_a_canonical_host_tool() -> anyhow::Result<()> {
        let request = permission_request(
            ProviderKind::Claude,
            &json!({
                "_meta": {"mcp": {"server": "forge-harness-control", "tool": "plan_read"}}
            }),
        )?;
        assert_eq!(
            request.target_list,
            vec![PermissionTarget::HostTool {
                name: "plan_read".into()
            }],
        );
        let external = permission_request(
            ProviderKind::Claude,
            &json!({"_meta": {"mcp": {"server": "external", "tool": "plan_read"}}}),
        )?;
        assert_eq!(
            external.target_list,
            vec![PermissionTarget::Mcp {
                target: "external/plan_read".into()
            }],
        );
        Ok(())
    }

    /// Native tools without file or command targets reach host policy instead of a silent denial.
    #[test]
    fn native_tools_without_targets_ask_by_name_and_host_tool_search_uses_host_authority()
    -> anyhow::Result<()> {
        let host_search = permission_request(
            ProviderKind::Claude,
            &json!({
                "kind":"other","title":"ToolSearch",
                "_meta":{"claudeCode":{"toolName":"ToolSearch"}},
                "rawInput":{"query":"select:mcp__forge-harness-control__goal_complete,mcp__forge-harness-control__goal_blocked","max_results":2}
            }),
        )?;
        assert_eq!(
            host_search.target_list,
            vec![
                PermissionTarget::HostTool {
                    name: "goal_complete".into()
                },
                PermissionTarget::HostTool {
                    name: "goal_blocked".into()
                },
            ],
        );
        for (tool, target) in [
            (
                json!({"kind":"other","title":"ToolSearch","rawInput":{"query":"select:mcp__external__tool"}}),
                "ToolSearch",
            ),
            (
                json!({"kind":"other","title":"Load skill: forge-managed:harness-checks",
                    "_meta":{"claudeCode":{"toolName":"Skill"}},"rawInput":{"skill":"forge-managed:harness-checks"}}),
                "Skill",
            ),
        ] {
            assert_eq!(
                permission_request(ProviderKind::Claude, &tool)?.target_list,
                vec![PermissionTarget::Tool {
                    target: target.into()
                }],
            );
        }
        assert_eq!(
            permission_request(ProviderKind::Claude, &json!({"kind":"other"}))?.target_list,
            vec![PermissionTarget::Unknown],
        );
        Ok(())
    }

    #[test]
    fn includes_every_location_and_diff_path_and_rejects_partial_targets() -> anyhow::Result<()> {
        let request = permission_request(
            ProviderKind::Claude,
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
                ProviderKind::Claude,
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
        for provider in [ProviderKind::Claude, ProviderKind::Antigravity] {
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
