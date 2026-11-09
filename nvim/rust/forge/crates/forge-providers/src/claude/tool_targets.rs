//! Map Claude Code tool calls to Harness permission targets and item semantics.

use forge_provider_api::permissions::PermissionTarget;
use forge_provider_api::tool::ToolSemantics;
use serde_json::Value;

/// Name of the in-process MCP server through which Harness exposes its control tools.
pub(crate) const HOST_TOOL_SERVER: &str = "forge-harness-control";

/// Classify a tool into the item category Harness renders.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ToolKind {
    Command,
    FileChange,
    ToolCall,
}

/// Get the item category of a native tool.
pub(crate) fn tool_kind(tool_name: &str) -> ToolKind {
    match tool_name {
        "Bash" | "PowerShell" => ToolKind::Command,
        "Write" | "Edit" | "MultiEdit" | "NotebookEdit" => ToolKind::FileChange,
        _ => ToolKind::ToolCall,
    }
}

/// Split an MCP tool name `mcp__<server>__<tool>` into its server and tool.
pub(crate) fn mcp_tool(tool_name: &str) -> Option<(&str, &str)> {
    tool_name.strip_prefix("mcp__")?.split_once("__")
}

fn string<'input>(input: &'input Value, keys: &[&str]) -> Option<&'input str> {
    keys.iter()
        .find_map(|key| input.get(*key).and_then(Value::as_str))
        .filter(|value| !value.trim().is_empty())
}

/// Derive typed semantics and a display title from a tool's input.
pub(crate) fn semantics(tool_name: &str, input: &Value) -> (String, ToolSemantics) {
    match tool_name {
        "Bash" | "PowerShell" => {
            let text = string(input, &["command"]).unwrap_or_default().to_owned();
            (
                text.clone(),
                ToolSemantics::Command {
                    text,
                    shell: Some(tool_name.to_ascii_lowercase()),
                    directory: None,
                },
            )
        }
        "Read" => {
            let paths = string(input, &["file_path"])
                .map(str::to_owned)
                .into_iter()
                .collect::<Vec<_>>();
            let title = paths
                .first()
                .map_or_else(|| tool_name.to_owned(), |path| format!("Read {path}"));
            (
                title,
                ToolSemantics::Read {
                    paths,
                    directory: None,
                },
            )
        }
        "Glob" | "Grep" => {
            let directory = searched_directory(tool_name, input);
            let pattern = string(input, &["pattern"]).unwrap_or_default();
            (
                format!("{tool_name} {pattern}"),
                ToolSemantics::Read {
                    paths: Vec::new(),
                    directory,
                },
            )
        }
        "ToolSearch" => {
            let query = string(input, &["query"]).unwrap_or_default();
            let title = match query.strip_prefix("select:") {
                Some(selection) => format!(
                    "Load tools {}",
                    selection
                        .split(',')
                        .map(|name| display_name(name.trim()))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                None => format!("Search tools for {query}"),
            };
            (title, ToolSemantics::Other)
        }
        "WebSearch" => {
            let query = string(input, &["query"]).unwrap_or_default();
            (format!("Search the web for {query}"), ToolSemantics::Other)
        }
        "WebFetch" => {
            let url = string(input, &["url"]).unwrap_or_default();
            (format!("Fetch {url}"), ToolSemantics::Other)
        }
        "Write" | "Edit" | "MultiEdit" | "NotebookEdit" => {
            let path = string(input, &["file_path", "notebook_path"]).unwrap_or_default();
            (format!("{tool_name} {path}"), ToolSemantics::Other)
        }
        _ => (
            crate::tool_display::call_title(display_name(tool_name), input)
                .unwrap_or_else(|| tool_name.to_owned()),
            ToolSemantics::Other,
        ),
    }
}

/// Present Harness control tools under their canonical names instead of their MCP names.
pub(crate) fn display_name(tool_name: &str) -> &str {
    match mcp_tool(tool_name) {
        Some((HOST_TOOL_SERVER, name)) => name,
        _ => tool_name,
    }
}

/// Resolve one permission request into the targets Harness authorizes.
pub(crate) fn permission_targets(
    tool_name: &str,
    input: &Value,
    blocked_path: Option<&str>,
) -> Vec<PermissionTarget> {
    let mut targets = Vec::new();
    if let Some((server, name)) = mcp_tool(tool_name) {
        targets.push(if server == HOST_TOOL_SERVER {
            PermissionTarget::HostTool { name: name.into() }
        } else {
            PermissionTarget::Mcp {
                target: format!("{server}/{name}"),
            }
        });
        return targets;
    }
    match tool_name {
        "Bash" | "PowerShell" => targets.push(PermissionTarget::Command {
            command: string(input, &["command"]).unwrap_or_default().into(),
            shell: None,
        }),
        "Read" | "NotebookRead" => match string(input, &["file_path", "notebook_path"]) {
            Some(path) => targets.push(PermissionTarget::Read { path: path.into() }),
            None => targets.push(PermissionTarget::UnknownFile {
                operation: forge_provider_api::access::DirectoryOperation::Read,
            }),
        },
        "Glob" | "Grep" => {
            if let Some(path) = string(input, &["path"]) {
                targets.push(PermissionTarget::Read { path: path.into() });
            }
            if let Some(directory) = searched_directory(tool_name, input)
                && !targets.contains(&PermissionTarget::Read {
                    path: directory.clone(),
                })
            {
                targets.push(PermissionTarget::Read { path: directory });
            }
        }
        "Write" | "Edit" | "MultiEdit" | "NotebookEdit" => {
            match string(input, &["file_path", "notebook_path"]) {
                Some(path) => targets.push(PermissionTarget::Write { path: path.into() }),
                None => targets.push(PermissionTarget::UnknownFile {
                    operation: forge_provider_api::access::DirectoryOperation::Write,
                }),
            }
        }
        "WebFetch" => targets.push(PermissionTarget::Network {
            target: string(input, &["url"]).unwrap_or("*").into(),
        }),
        "WebSearch" => targets.push(PermissionTarget::Network { target: "*".into() }),
        "ToolSearch" => targets.extend(tool_search_targets(input)),
        _ => targets.push(PermissionTarget::Tool {
            target: tool_name.to_owned(),
        }),
    }
    if let Some(path) = blocked_path.filter(|path| !path.trim().is_empty()) {
        let write = tool_kind(tool_name) == ToolKind::FileChange;
        let target = if write {
            PermissionTarget::Write { path: path.into() }
        } else {
            PermissionTarget::Read { path: path.into() }
        };
        if !targets.contains(&target) {
            targets.push(target);
        }
    }
    if targets.is_empty() {
        targets.push(PermissionTarget::Unknown);
    }
    targets
}

/// Treat loading only Harness control tool schemas as those tools' own host authority.
///
/// The agent discovers deferred Harness control tools through `ToolSearch`; asking about each
/// schema load separately would block every Harness workflow on an extra prompt.
fn tool_search_targets(input: &Value) -> Vec<PermissionTarget> {
    let host_tools = input["query"]
        .as_str()
        .and_then(|query| query.trim().strip_prefix("select:"))
        .and_then(|selection| {
            selection
                .split(',')
                .map(|selected| {
                    mcp_tool(selected.trim())
                        .filter(|(server, name)| *server == HOST_TOOL_SERVER && !name.is_empty())
                        .map(|(_, name)| name)
                })
                .collect::<Option<Vec<_>>>()
        })
        .filter(|host_tools| !host_tools.is_empty());
    match host_tools {
        Some(host_tools) => host_tools
            .into_iter()
            .map(|name| PermissionTarget::HostTool { name: name.into() })
            .collect(),
        None => vec![PermissionTarget::Tool {
            target: "ToolSearch".into(),
        }],
    }
}

/// Return the directory a search reads beyond any explicit `path`.
///
/// `Glob` and `Grep` search the session's working directory when they omit `path`. A `Glob`
/// pattern can also leave its base directory through `..` or an absolute prefix, so the read
/// target is the pattern's literal prefix joined onto the base rather than the base alone.
fn searched_directory(tool_name: &str, input: &Value) -> Option<String> {
    let base = string(input, &["path"]);
    match tool_name {
        "Glob" => {
            let pattern = input["pattern"].as_str().unwrap_or_default();
            let prefix = pattern
                .split(['/', '\\'])
                .take_while(|component| !component.contains(['*', '?', '[', '{']))
                .collect::<Vec<_>>()
                .join("/");
            if std::path::Path::new(&prefix).is_absolute() || pattern.starts_with(['/', '\\']) {
                return Some(if prefix.is_empty() {
                    "/".into()
                } else {
                    prefix
                });
            }
            let base = base.unwrap_or(".");
            Some(if prefix.is_empty() {
                base.to_owned()
            } else {
                format!("{}/{prefix}", base.trim_end_matches(['/', '\\']))
            })
        }
        "Grep" if base.is_none() => Some(".".into()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn read(path: &str) -> PermissionTarget {
        PermissionTarget::Read { path: path.into() }
    }

    /// Web tools read as their query or URL rather than the raw JSON input.
    #[test]
    fn web_tools_are_titled_by_their_query_or_url() {
        assert_eq!(
            semantics("WebSearch", &json!({"query":"hash maps","mode":"standard"})).0,
            "Search the web for hash maps"
        );
        assert_eq!(
            semantics(
                "WebFetch",
                &json!({"url":"https://example.com","prompt":"x"})
            )
            .0,
            "Fetch https://example.com"
        );
        assert_eq!(
            semantics(
                "ToolSearch",
                &json!({"query":"select:mcp__forge-harness-control__plan_read,Read","max_results":2})
            )
            .0,
            "Load tools plan_read, Read"
        );
    }

    #[test]
    fn searches_read_their_searched_directory() {
        assert_eq!(
            permission_targets(
                "Glob",
                &json!({"pattern":"nvim/crates/tests/**/*.rs"}),
                None
            ),
            [read("./nvim/crates/tests")]
        );
        assert_eq!(
            permission_targets("Glob", &json!({"pattern":"**/*.rs"}), None),
            [read(".")]
        );
        let escaping =
            permission_targets("Glob", &json!({"path":"src","pattern":"../../**"}), None);
        assert!(escaping.contains(&read("src")));
        assert!(escaping.contains(&read("src/../..")));
        assert_eq!(
            permission_targets("Glob", &json!({"pattern":"/etc/*.conf"}), None),
            [read("/etc")]
        );
        assert_eq!(
            permission_targets("Grep", &json!({"pattern":"fn main"}), None),
            [read(".")]
        );
        assert_eq!(
            permission_targets("Grep", &json!({"path":"src","pattern":"fn main"}), None),
            [read("src")]
        );
    }

    #[test]
    fn host_tools_and_their_schema_loads_carry_host_authority() {
        assert_eq!(
            permission_targets("mcp__forge-harness-control__plan_edit", &json!({}), None),
            [PermissionTarget::HostTool {
                name: "plan_edit".into()
            }]
        );
        assert_eq!(
            permission_targets(
                "ToolSearch",
                &json!({"query":"select:mcp__forge-harness-control__a,mcp__forge-harness-control__b"}),
                None
            ),
            [
                PermissionTarget::HostTool { name: "a".into() },
                PermissionTarget::HostTool { name: "b".into() }
            ]
        );
        assert_eq!(
            permission_targets("ToolSearch", &json!({"query":"select:Read"}), None),
            [PermissionTarget::Tool {
                target: "ToolSearch".into()
            }]
        );
        assert_eq!(
            permission_targets("mcp__github__search", &json!({}), None),
            [PermissionTarget::Mcp {
                target: "github/search".into()
            }]
        );
    }

    #[test]
    fn file_tools_request_read_or_write_authority_for_their_path() {
        assert_eq!(
            permission_targets("Edit", &json!({"file_path":"a.rs"}), None),
            [PermissionTarget::Write {
                path: "a.rs".into()
            }]
        );
        assert_eq!(
            permission_targets("Read", &json!({}), Some("secret.txt")),
            [
                PermissionTarget::UnknownFile {
                    operation: forge_provider_api::access::DirectoryOperation::Read
                },
                read("secret.txt")
            ]
        );
        assert!(matches!(
            permission_targets("Bash", &json!({"command":"ls"}), None).as_slice(),
            [PermissionTarget::Command { command, .. }] if command == "ls"
        ));
    }
}
