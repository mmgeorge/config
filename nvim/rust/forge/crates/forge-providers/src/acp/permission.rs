use super::AcpKind as ProviderKind;
use super::tool_semantics::{command_arguments, decode_arguments, is_command_name};
use anyhow::Context;
use forge_provider_api::access::DirectoryOperation;
use forge_provider_api::permissions::{PermissionRequest, PermissionSubject, PermissionTarget};
use serde_json::Value;
use std::collections::{BTreeSet, HashSet};

/// Name of the MCP server through which Harness exposes its own control tools.
pub(super) const HOST_TOOL_SERVER: &str = "forge-harness-control";

const FILE_PATH_FIELDS: &[&str] = &[
    "file_path",
    "filePath",
    "path",
    "absolute_path",
    "AbsolutePath",
    "TargetFile",
];

/// The two native actions Harness can honor without delegating persistence to the agent.
pub(super) struct OncePermissionOptions<'request> {
    pub(super) allow: &'request str,
    pub(super) reject: &'request str,
}

impl<'request> OncePermissionOptions<'request> {
    /// Validate the offered choices before Harness can commit either decision.
    pub(super) fn parse(options: &'request Value) -> Option<Self> {
        let mut allow = None;
        let mut reject = None;
        let mut ids = HashSet::new();
        for option in options.as_array()? {
            let id = nonempty_string(&option["optionId"])?;
            if !ids.insert(id) {
                return None;
            }
            let slot = match option["kind"].as_str()? {
                "allow_once" => &mut allow,
                "reject_once" => &mut reject,
                _ => continue,
            };
            if slot.replace(id).is_some() {
                return None;
            }
        }
        Some(Self {
            allow: allow?,
            reject: reject?,
        })
    }
}

/// The file authority that the native descriptor must establish.
#[derive(Clone, Copy, PartialEq, Eq)]
enum FileOperation {
    Read,
    Write,
    Move,
}

/// Resolve a complete ACP tool snapshot into shared permission targets.
///
/// `subject` names the host tool item the session admitted for this tool call, which only the
/// session can resolve.
pub fn permission_request(
    kind: ProviderKind,
    tool: &Value,
    subject: PermissionSubject,
    server_names: &[String],
) -> anyhow::Result<PermissionRequest> {
    let input = decode_arguments(&tool["rawInput"]);
    let programmatic_name = nonempty_string(&tool["name"]);
    let name = programmatic_name.or_else(|| nonempty_string(&tool["title"]));
    let operation = tool["kind"].as_str();
    let target_list = if let Some(target) = bundled_mcp_target(tool, server_names)? {
        vec![target]
    } else if operation == Some("fetch") {
        vec![PermissionTarget::Network {
            target: input["url"].as_str().unwrap_or("*").into(),
        }]
    } else if let Some(operation) = file_operation(operation, programmatic_name)? {
        anyhow::ensure!(
            !name.is_some_and(is_command_name),
            "ACP command name conflicts with file operation semantics"
        );
        file_targets(tool, &input, operation)?
    } else if let Some((command, shell)) = command_arguments(&input, name.unwrap_or_default())? {
        vec![PermissionTarget::Command {
            command: command.into(),
            shell,
        }]
    } else if operation == Some("execute") || name.is_some_and(is_command_name) {
        anyhow::bail!("ACP command permission omitted the command");
    } else {
        anyhow::ensure!(
            file_paths(tool, &input)?.is_empty(),
            "ACP file permission omitted known read/write operation semantics"
        );
        native_tool_targets(name, &input, server_names)
    };
    Ok(PermissionRequest {
        id: uuid::Uuid::new_v4().to_string(),
        provider: kind.id(),
        subject,
        reason: None,
        target_list,
    })
}

/// Resolve both MCP representations against the servers launched for this binding.
fn bundled_mcp_target(
    tool: &Value,
    server_names: &[String],
) -> anyhow::Result<Option<PermissionTarget>> {
    let metadata = match &tool["_meta"] {
        Value::Null => None,
        Value::Object(metadata) => metadata.get("mcp").filter(|value| !value.is_null()),
        _ => anyhow::bail!("ACP tool metadata must be an object"),
    };
    let name = optional_string(&tool["name"]).context("ACP tool name is invalid")?;
    let structured = metadata
        .map(|metadata| {
            let server =
                nonempty_string(&metadata["server"]).context("ACP MCP server is missing")?;
            let name = nonempty_string(&metadata["tool"]).context("ACP MCP tool is missing")?;
            anyhow::ensure!(
                server_names.iter().any(|launched| launched == server),
                "ACP MCP server was not launched"
            );
            Ok::<_, anyhow::Error>((server, name))
        })
        .transpose()?;
    let bundled = name
        .and_then(|name| name.strip_prefix("mcp_"))
        .map(|name| {
            let mut matches = server_names.iter().filter_map(|server| {
                let name = name.strip_prefix(server.as_str())?.strip_prefix('_')?;
                (!name.trim().is_empty()).then_some((server.as_str(), name))
            });
            match (matches.next(), matches.next()) {
                (Some(identity), None) => Ok(identity),
                _ => anyhow::bail!("ACP MCP tool name does not resolve to one launched server"),
            }
        })
        .transpose()?;
    if let (Some(structured), Some(_)) = (structured, name) {
        anyhow::ensure!(
            Some(structured) == bundled,
            "ACP MCP name conflicts with its structured identity"
        );
    }
    let Some((server, name)) = structured.or(bundled) else {
        return Ok(None);
    };
    Ok(Some(if server == HOST_TOOL_SERVER {
        PermissionTarget::HostTool { name: name.into() }
    } else {
        PermissionTarget::Mcp {
            target: format!("{server}/{name}"),
        }
    }))
}

/// Recognize file authority from the native kind or a known bundled operation name.
fn file_operation(kind: Option<&str>, name: Option<&str>) -> anyhow::Result<Option<FileOperation>> {
    let named = match name {
        Some("read_file") => Some(FileOperation::Read),
        Some("edit_file" | "write_file" | "delete_file") => Some(FileOperation::Write),
        Some("move_file") => Some(FileOperation::Move),
        _ => None,
    };
    let reported = match kind {
        Some("read" | "search") => Some(FileOperation::Read),
        Some("edit" | "delete") => Some(FileOperation::Write),
        Some("move") => Some(FileOperation::Move),
        None | Some("other") => return Ok(named),
        _ => None,
    };
    anyhow::ensure!(
        named.is_none() || named == reported,
        "ACP file name conflicts with its reported operation"
    );
    Ok(reported)
}

/// Require both move operands and authorize every reported path.
fn file_targets(
    tool: &Value,
    input: &Value,
    operation: FileOperation,
) -> anyhow::Result<Vec<PermissionTarget>> {
    anyhow::ensure!(
        input.is_null() || input.is_object(),
        "ACP file operation arguments must be an object"
    );
    let mut paths = reported_paths(tool, input)?;
    if matches!(operation, FileOperation::Move) {
        let source = path_fields(input, &["source_path", "sourcePath", "SourcePath"])?;
        let destination = path_fields(
            input,
            &["destination_path", "destinationPath", "DestinationPath"],
        )?;
        if source.is_empty() && destination.is_empty() {
            anyhow::ensure!(paths.len() >= 2, "ACP move omitted one or both endpoints");
        } else {
            anyhow::ensure!(
                !source.is_empty() && !destination.is_empty(),
                "ACP move omitted one or both endpoints"
            );
            paths.extend(source);
            paths.extend(destination);
        }
    }
    paths.extend(path_fields(input, FILE_PATH_FIELDS)?);
    let write = matches!(operation, FileOperation::Write | FileOperation::Move);
    if paths.is_empty() {
        return Ok(vec![PermissionTarget::UnknownFile {
            operation: if write {
                DirectoryOperation::Write
            } else {
                DirectoryOperation::Read
            },
        }]);
    }
    Ok(paths
        .into_iter()
        .map(|path| {
            if write {
                PermissionTarget::Write { path: path.into() }
            } else {
                PermissionTarget::Read { path: path.into() }
            }
        })
        .collect())
}

/// Collect path arguments and every protocol location and diff.
fn file_paths<'tool>(
    tool: &'tool Value,
    input: &'tool Value,
) -> anyhow::Result<BTreeSet<&'tool str>> {
    let mut paths = reported_paths(tool, input)?;
    paths.extend(path_fields(input, FILE_PATH_FIELDS)?);
    Ok(paths)
}

/// Collect location and diff paths from well-formed collections only.
fn reported_paths<'tool>(
    tool: &'tool Value,
    input: &'tool Value,
) -> anyhow::Result<BTreeSet<&'tool str>> {
    let mut paths = BTreeSet::new();
    for locations in [&tool["locations"], &input["locations"]] {
        for location in optional_array(locations).context("ACP tool locations must be an array")? {
            paths.insert(
                nonempty_string(&location["path"]).context("ACP tool location omitted path")?,
            );
        }
    }
    for content in optional_array(&tool["content"]).context("ACP tool content must be an array")? {
        let kind = content["type"]
            .as_str()
            .context("ACP tool content omitted type")?;
        if kind == "diff" {
            paths.insert(nonempty_string(&content["path"]).context("ACP diff omitted path")?);
        }
    }
    Ok(paths)
}

/// Collect every supplied argument alias and reject malformed paths.
fn path_fields<'input>(
    input: &'input Value,
    names: &[&str],
) -> anyhow::Result<BTreeSet<&'input str>> {
    names
        .iter()
        .filter_map(|field| optional_string(&input[*field]).transpose())
        .collect()
}

/// Read an optional string without accepting a blank or incorrectly typed value.
fn optional_string(value: &Value) -> anyhow::Result<Option<&str>> {
    if value.is_null() {
        return Ok(None);
    }
    nonempty_string(value)
        .map(Some)
        .context("ACP field must be a nonempty string")
}

/// Read an optional collection without dropping malformed evidence.
fn optional_array(value: &Value) -> anyhow::Result<&[Value]> {
    if value.is_null() {
        return Ok(&[]);
    }
    value
        .as_array()
        .map(Vec::as_slice)
        .context("expected an array")
}

/// Check a wire string for content without altering its spelling.
fn nonempty_string(value: &Value) -> Option<&str> {
    value.as_str().filter(|text| !text.trim().is_empty())
}

/// Normalize a native tool that carries no file, command, network, or MCP target.
///
/// Agents ask permission for their own tools, such as a tool search. Denying them as unknown
/// operations silently blocks Harness workflows, because the agent discovers deferred Harness
/// control tools through that search. A search limited to the mounted Forge catalog follows the
/// host endpoint route; any other native tool asks under its native name.
fn native_tool_targets(
    name: Option<&str>,
    input: &Value,
    server_names: &[String],
) -> Vec<PermissionTarget> {
    let Some(name) = name.map(str::trim) else {
        return vec![PermissionTarget::Unknown];
    };
    if (name.eq_ignore_ascii_case("ToolSearch") || name.eq_ignore_ascii_case("tool_search"))
        && server_names.iter().any(|server| server == HOST_TOOL_SERVER)
        && let Some(host_tools) = input["query"]
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
            PermissionSubject::Unattached,
            &[],
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
                }),
                PermissionSubject::Unattached,
                &[],
            )
            .is_err()
        );
        Ok(())
    }

    /// The bundled permission request reports the tool name without MCP metadata.
    #[test]
    fn bundled_names_preserve_host_and_external_mcp_authority() {
        let server_names = [HOST_TOOL_SERVER.into(), "project_tools".into()];
        for (name, expected) in [
            (
                "mcp_forge-harness-control_harness_config",
                PermissionTarget::HostTool {
                    name: "harness_config".into(),
                },
            ),
            (
                "mcp_project_tools_read_item",
                PermissionTarget::Mcp {
                    target: "project_tools/read_item".into(),
                },
            ),
        ] {
            let tool = serde_json::json!({"name":name,"kind":"other"});
            let request = permission_request(
                ProviderKind::Antigravity,
                &tool,
                PermissionSubject::Unattached,
                &server_names,
            )
            .unwrap();
            assert_eq!(request.target_list, vec![expected]);
        }
        let server_names = ["project".into(), "project_tools".into()];
        for name in [
            "mcp_project_tools_read_item",
            "mcp_unknown_read_item",
            "mcp_forge-harness-control_harness_config",
        ] {
            let tool = serde_json::json!({"name":name,"kind":"other"});
            assert!(bundled_mcp_target(&tool, &server_names).is_err());
        }
        let host_metadata =
            json!({"_meta":{"mcp":{"server":HOST_TOOL_SERVER,"tool":"harness_config"}}});
        assert!(bundled_mcp_target(&host_metadata, &server_names).is_err());
    }

    /// A malformed or contradictory MCP identity must never reach policy.
    #[test]
    fn bundled_names_reject_malformed_and_conflicting_mcp_metadata() {
        let server_names = vec!["project_tools".into(), "other_tools".into()];
        for metadata in [
            json!("unstructured metadata"),
            json!({"mcp":{"server":"other_tools","tool":"read_item"}}),
        ] {
            let tool = json!({"name":"mcp_project_tools_read_item","_meta":metadata});
            assert!(bundled_mcp_target(&tool, &server_names).is_err());
        }
        let tool = json!({
            "name":"mcp_project_tools_read_item",
            "_meta":{"mcp":{"server":"project_tools","tool":"read_item"}}
        });
        assert_eq!(
            bundled_mcp_target(&tool, &server_names).unwrap(),
            Some(PermissionTarget::Mcp {
                target: "project_tools/read_item".into()
            })
        );
    }

    /// A file move needs independent evidence of both endpoints.
    #[test]
    fn move_requires_both_endpoints_and_permissions_cover_every_path() {
        for tool in [
            json!({"kind":"move","locations":[{"path":"before.txt"}]}),
            json!({"kind":"move","rawInput":{"sourcePath":"before.txt"}}),
            json!({"kind":"edit","rawInput":{"filePath":"before.txt"},"content":{}}),
        ] {
            assert!(
                permission_request(
                    ProviderKind::Antigravity,
                    &tool,
                    PermissionSubject::Unattached,
                    &[]
                )
                .is_err()
            );
        }
        let tool = json!({
            "kind":"move",
            "rawInput":{"sourcePath":"before.txt","destinationPath":"after.txt"}
        });
        let request = permission_request(
            ProviderKind::Antigravity,
            &tool,
            PermissionSubject::Unattached,
            &[],
        )
        .unwrap();
        assert_eq!(
            request.target_list,
            vec![
                PermissionTarget::Write {
                    path: "after.txt".into()
                },
                PermissionTarget::Write {
                    path: "before.txt".into()
                },
            ]
        );
    }

    /// A command without executable source cannot fall back to a generic native tool.
    #[test]
    fn incomplete_execute_does_not_authorize_its_display_title() {
        let tool = json!({"kind":"execute","title":"Describe fixture"});
        assert!(
            permission_request(
                ProviderKind::Antigravity,
                &tool,
                PermissionSubject::Unattached,
                &[]
            )
            .is_err()
        );
    }

    /// An opaque path-bearing native operation cannot gain write authority through a read decision.
    #[test]
    fn path_permissions_require_positive_native_read_or_write_semantics() {
        for provider in [ProviderKind::Antigravity] {
            for kind in [Value::Null, json!("other"), json!("future")] {
                assert!(
                    permission_request(
                        provider,
                        &json!({"kind":kind, "rawInput":{"path":"file"}}),
                        PermissionSubject::Unattached,
                        &[],
                    )
                    .is_err()
                );
            }
            let read = permission_request(
                provider,
                &json!({"kind":"read", "rawInput":{"path":"file"}}),
                PermissionSubject::Unattached,
                &[],
            )
            .unwrap();
            assert!(matches!(
                read.target_list.as_slice(),
                [PermissionTarget::Read { .. }]
            ));
            let write = permission_request(
                provider,
                &json!({"kind":"edit", "rawInput":{"path":"file"}}),
                PermissionSubject::Unattached,
                &[],
            )
            .unwrap();
            assert!(matches!(
                write.target_list.as_slice(),
                [PermissionTarget::Write { .. }]
            ));
        }
    }
}
