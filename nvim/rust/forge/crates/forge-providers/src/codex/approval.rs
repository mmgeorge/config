use crate::permission_input::{normalize_permission_payload, provider_path, provider_shell};
use anyhow::{Context, Result, ensure};
use forge_provider_api::access::DirectoryOperation;
use forge_provider_api::permissions::{
    CommandAnalysisError, PermissionRequest, PermissionResponse, PermissionTarget,
};
use forge_shell::shell::{CommandAnalysisFailure, CommandShell};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use uuid::Uuid;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct OperationKey {
    thread_id: String,
    turn_id: String,
    item_id: String,
}

struct FileProposal {
    revision: String,
    targets: std::result::Result<Vec<PermissionTarget>, String>,
}

/// Retains authorization metadata only for the current connection's active operations.
#[derive(Clone, Default)]
pub struct ApprovalContext {
    proposal: Arc<Mutex<HashMap<OperationKey, FileProposal>>>,
}

/// Binds one permission evaluation to the proposal observed before request dispatch.
pub struct PreparedApproval {
    pub request: PermissionRequest,
    observation: Option<(ApprovalContext, OperationKey, String)>,
}

impl ApprovalContext {
    /// Apply ordered item metadata before asynchronous permission dispatch.
    pub fn observe(&self, message: &Value) {
        let method = message["method"].as_str().unwrap_or_default();
        let params = &message["params"];
        let mut proposal = self
            .proposal
            .lock()
            .expect("approval metadata lock poisoned");
        if method == "turn/completed" {
            let thread = params["threadId"].as_str();
            let turn = params
                .pointer("/turn/id")
                .and_then(Value::as_str)
                .or_else(|| params["turnId"].as_str());
            proposal.retain(|key, _| {
                Some(key.thread_id.as_str()) != thread || Some(key.turn_id.as_str()) != turn
            });
            return;
        }
        let Some(key) = operation_key(params) else {
            return;
        };
        if method == "item/completed" {
            proposal.remove(&key);
            return;
        }
        if method == "item/started"
            && params.pointer("/item/type").and_then(Value::as_str) == Some("fileChange")
        {
            let changes = &params["item"]["changes"];
            proposal.insert(
                key,
                FileProposal {
                    revision: digest(changes.to_string().as_bytes()),
                    targets: if changes.is_null() {
                        Ok(vec![PermissionTarget::UnknownFile {
                            operation: DirectoryOperation::Write,
                        }])
                    } else {
                        file_targets(changes).map_err(|error| format!("{error:#}"))
                    },
                },
            );
        } else if method == "item/fileChange/patchUpdated" {
            // A patch notification alone does not provide a complete structured target list.
            proposal.insert(
                key,
                FileProposal {
                    revision: digest(params.to_string().as_bytes()),
                    targets: Err(
                        "file proposal changed without a complete structured target list".into(),
                    ),
                },
            );
        }
    }

    /// Resolve every target, rejecting malformed metadata without returning a partial request.
    pub fn prepare(&self, message: &Value) -> Result<PreparedApproval> {
        let method = message["method"]
            .as_str()
            .context("approval omitted method")?;
        let mut request = permission_from_provider(method, message, "")?;
        let mut observation = None;
        if method == "item/fileChange/requestApproval" {
            let params = &message["params"];
            if let Some(key) = operation_key(params) {
                let proposal = self
                    .proposal
                    .lock()
                    .expect("approval metadata lock poisoned");
                if let Some(proposal) = proposal.get(&key) {
                    request.target_list = proposal
                        .targets
                        .as_ref()
                        .map_err(|error| anyhow::anyhow!(error.clone()))?
                        .clone();
                    observation = Some((self.clone(), key, proposal.revision.clone()));
                }
            }
            if observation.is_none() && !params["changes"].is_null() {
                request.target_list = file_targets(&params["changes"])?;
            }
        }
        Ok(PreparedApproval {
            request,
            observation,
        })
    }
}

impl PreparedApproval {
    /// Reject an acceptance if its proposal changed or retired after evaluation.
    pub fn validate(&self) -> Result<()> {
        if let Some((context, key, revision)) = &self.observation {
            let proposal = context
                .proposal
                .lock()
                .expect("approval metadata lock poisoned");
            ensure!(
                proposal
                    .get(key)
                    .is_some_and(|proposal| &proposal.revision == revision),
                "file proposal changed or retired while approval was pending"
            );
        }
        Ok(())
    }
}

fn operation_key(params: &Value) -> Option<OperationKey> {
    Some(OperationKey {
        thread_id: params["threadId"].as_str()?.to_owned(),
        turn_id: params["turnId"].as_str()?.to_owned(),
        item_id: params["itemId"]
            .as_str()
            .or_else(|| params.pointer("/item/id").and_then(Value::as_str))?
            .to_owned(),
    })
}

fn file_targets(changes: &Value) -> Result<Vec<PermissionTarget>> {
    let changes = changes
        .as_array()
        .context("file proposal omitted its structured changes")?;
    ensure!(!changes.is_empty(), "file proposal has no affected paths");
    let mut targets = Vec::new();
    let mut seen = HashSet::new();
    for change in changes {
        let path = change["path"]
            .as_str()
            .filter(|path| !path.trim().is_empty())
            .context("file change omitted path")?;
        let mut paths = vec![path];
        if let Some(destination) = change
            .pointer("/kind/move_path")
            .or_else(|| change.pointer("/kind/movePath"))
            && !destination.is_null()
        {
            paths.push(
                destination
                    .as_str()
                    .filter(|path| !path.trim().is_empty())
                    .context("file change has invalid move destination")?,
            );
        }
        for path in paths {
            let target = PermissionTarget::Write { path: path.into() };
            if seen.insert(path.to_owned()) {
                targets.push(target);
            }
        }
    }
    Ok(targets)
}

pub fn permission_from_provider(
    method: &str,
    message: &Value,
    _workspace: &str,
) -> Result<PermissionRequest> {
    let params = message.get("params").unwrap_or(&Value::Null);
    let id = format!("approval-{}", Uuid::new_v4());
    let provider = crate::registry::select("codex").expect("Codex is registered");
    let reason = None;
    let target_list = if method == "mcpServer/elicitation/request" {
        let server = params
            .get("serverName")
            .and_then(Value::as_str)
            .unwrap_or("unknown");
        let tool = params
            .pointer("/meta/toolName")
            .or_else(|| params.pointer("/meta/tool_name"))
            .and_then(Value::as_str)
            .unwrap_or("*");
        vec![PermissionTarget::Mcp {
            target: format!("{server}/{tool}"),
        }]
    } else if method.contains("commandExecution") || method == "terminal/create" {
        let (command, shell) = provider_command(params, method.contains("commandExecution"))?;
        vec![PermissionTarget::Command { command, shell }]
    } else if method.contains("fileChange") || method == "fs/write_text_file" {
        vec![provider_path(params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Write,
            },
            |path| PermissionTarget::Write { path },
        )]
    } else if method == "fs/read_text_file" {
        vec![provider_path(params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Read,
            },
            |path| PermissionTarget::Read { path },
        )]
    } else if method.contains("permission") || method.contains("permissions") {
        normalize_permission_payload(params)
    } else {
        vec![PermissionTarget::Unknown]
    };
    Ok(PermissionRequest {
        id,
        provider,
        reason,
        target_list,
    })
}

/// Encode the normalized host decision in the reply shape of the original Codex request.
///
/// Harness has already applied saved rules, grants, and deadlines; the adapter sees only whether
/// the request was allowed, denied, or cancelled.
pub fn codex_response(message: &Value, response: &PermissionResponse) -> Value {
    let allow = matches!(response, PermissionResponse::Allow);
    let cancelled = matches!(response, PermissionResponse::Cancelled);
    if message.get("method").and_then(Value::as_str) == Some("mcpServer/elicitation/request") {
        return json!({
            "action": if allow { "accept" } else if cancelled { "cancel" } else { "decline" },
            "content": if allow { json!({}) } else { Value::Null },
        });
    }
    if message.get("method").and_then(Value::as_str) == Some("item/permissions/requestApproval") {
        return if allow {
            json!({ "permissions": message.pointer("/params/permissions").cloned().unwrap_or(Value::Null), "scope": "turn" })
        } else {
            json!({ "permissions": {}, "scope": "turn" })
        };
    }
    json!({ "decision": match response {
        PermissionResponse::Allow => "accept",
        PermissionResponse::Deny { .. } => "decline",
        PermissionResponse::Cancelled => "cancel",
    }})
}

/// Recovers provider arguments before applying the selected shell's quoting rules.
fn provider_command(
    params: &Value,
    uses_argv_serialization: bool,
) -> Result<(String, Option<CommandShell>)> {
    let parse_error = || CommandAnalysisError {
        shell: provider_shell(params).unwrap_or(CommandShell::Unknown),
        failure: CommandAnalysisFailure::MalformedTransport,
    };
    let command = params.get("command");
    let arguments = if let Some(arguments) = params
        .get("args")
        .filter(|arguments| arguments.is_array())
        .or_else(|| command.filter(|command| command.is_array()))
        .and_then(Value::as_array)
    {
        let mut arguments = arguments
            .iter()
            .map(|argument| argument.as_str().map(str::to_owned))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(parse_error)?;
        if let Some(executable) = command.and_then(Value::as_str) {
            arguments.insert(0, executable.to_owned());
        }
        arguments
    } else if let Some(command) = command.and_then(Value::as_str) {
        if !uses_argv_serialization {
            return Ok((command.to_owned(), provider_shell(params)));
        }
        shlex::split(command).ok_or_else(parse_error)?
    } else {
        return Err(parse_error().into());
    };
    if arguments.is_empty() || arguments[0].is_empty() {
        return Err(parse_error().into());
    }
    if let Some((marker, shell)) = forge_shell::command::shell_script_argument(&arguments) {
        return Ok((arguments[marker + 1].clone(), Some(shell)));
    }
    let command =
        shlex::try_join(arguments.iter().map(String::as_str)).map_err(|_| parse_error())?;
    Ok((command, Some(CommandShell::Bash)))
}

fn digest(content: &[u8]) -> String {
    use sha2::Digest;
    hex::encode(sha2::Sha256::digest(content))
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_shell::command::normalize_command_in;

    fn started(thread: &str, turn: &str, item: &str, changes: Value) -> Value {
        json!({"method":"item/started","params":{"threadId":thread,"turnId":turn,
            "item":{"id":item,"type":"fileChange","changes":changes}}})
    }

    fn request(thread: &str, turn: &str, item: &str) -> Value {
        json!({"id":19,"method":"item/fileChange/requestApproval",
            "params":{"threadId":thread,"turnId":turn,"itemId":item,"grantRoot":null}})
    }

    #[test]
    fn correlates_all_paths_and_rename_destinations_without_crossing_turns() -> Result<()> {
        let context = ApprovalContext::default();
        context.observe(&started(
            "thread",
            "turn",
            "edit",
            json!([
                {"path":"source.rs","kind":{"type":"update","move_path":"moved.rs"},"diff":"old"},
                {"path":"second.rs","kind":{"type":"add"},"diff":"new"}
            ]),
        ));
        let prepared = context.prepare(&request("thread", "turn", "edit"))?;
        assert_eq!(
            prepared.request.target_list,
            vec![
                PermissionTarget::Write {
                    path: "source.rs".into()
                },
                PermissionTarget::Write {
                    path: "moved.rs".into()
                },
                PermissionTarget::Write {
                    path: "second.rs".into()
                }
            ]
        );
        for other in [
            request("other", "turn", "edit"),
            request("thread", "other", "edit"),
        ] {
            assert!(matches!(
                context.prepare(&other)?.request.target_list[0],
                PermissionTarget::UnknownFile { .. }
            ));
        }
        assert!(
            ApprovalContext::default()
                .prepare(&request("thread", "turn", "edit"))?
                .observation
                .is_none()
        );
        Ok(())
    }

    #[test]
    fn provider_command_quoting_roundtrips_every_supported_shell() {
        for (executable, shell) in [
            (
                r"C:\Program Files\PowerShell\7\pwsh.exe",
                CommandShell::PowerShell,
            ),
            (
                r"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe",
                CommandShell::PowerShell,
            ),
            ("/usr/bin/pwsh", CommandShell::PowerShell),
            ("/bin/sh", CommandShell::Bash),
            ("/bin/bash", CommandShell::Bash),
            ("/bin/zsh", CommandShell::Zsh),
            ("/opt/homebrew/bin/nu", CommandShell::Nushell),
            (r"C:\Program Files\nu\bin\nu.exe", CommandShell::Nushell),
        ] {
            for source in [
                r#"probe "timeline|markdown|table|wrap""#,
                r#"probe 'one; two & three | four'"#,
                r#"probe "" 'café 世界' 'C:\space path\file.txt'"#,
                "probe first\nprobe second",
                "probe 'quote\" and $value and `tick and \\path'",
            ] {
                let arguments = [executable, "-c", source];
                let rendered = shlex::try_join(arguments).unwrap();
                for params in [json!({"command":rendered}), json!({"args":arguments})] {
                    let message =
                        json!({"method":"item/commandExecution/requestApproval","params":params});
                    let request = permission_from_provider(
                        "item/commandExecution/requestApproval",
                        &message,
                        ".",
                    )
                    .unwrap();
                    assert_eq!(
                        request.target_list,
                        vec![PermissionTarget::Command {
                            command: source.into(),
                            shell: Some(shell),
                        }]
                    );
                    let parsed = normalize_command_in(source, shell);
                    assert!(parsed.failure.is_none(), "{shell:?}: {source}: {parsed:?}");
                    let expected = if source.contains('\n') { 2 } else { 1 };
                    assert_eq!(
                        parsed.invocation_list.len(),
                        expected,
                        "{shell:?}: {source}"
                    );
                    assert!(
                        parsed
                            .invocation_list
                            .iter()
                            .all(|command| command.command_name == "probe")
                    );
                }
            }
        }
    }

    #[test]
    fn provider_argv_preserves_literal_arguments_without_a_shell_launcher() {
        let arguments = ["rg", "-n", "a|b;$(probe)", "", r"C:\space path\file.txt"];
        for params in [
            json!({"command":shlex::try_join(arguments).unwrap()}),
            json!({"command":arguments}),
            json!({"command":"rg", "args":&arguments[1..]}),
        ] {
            let message = json!({"method":"item/commandExecution/requestApproval","params":params});
            let request =
                permission_from_provider("item/commandExecution/requestApproval", &message, ".")
                    .unwrap();
            let PermissionTarget::Command { command, shell } = &request.target_list[0] else {
                panic!()
            };
            let parsed = normalize_command_in(command, shell.unwrap());
            assert!(parsed.failure.is_none(), "{command}: {parsed:?}");
            assert_eq!(parsed.invocation_list.len(), 1);
            assert_eq!(parsed.invocation_list[0].token_list, arguments);
        }
    }

    #[test]
    fn malformed_native_command_approval_payloads_are_rejected() {
        for params in [
            json!({"command":"pwsh -c 'unfinished"}),
            json!({"command":""}),
            json!({"args":[]}),
            json!({"args":["pwsh", "-c", 42]}),
            json!({"command":"rg\u{0}tail"}),
        ] {
            let message = json!({"method":"item/commandExecution/requestApproval","params":params});
            let error =
                permission_from_provider("item/commandExecution/requestApproval", &message, ".")
                    .unwrap_err();
            assert!(
                error.downcast_ref::<CommandAnalysisError>().is_some(),
                "{error:#}"
            );
        }
    }

    #[test]
    fn native_approval_resolution_encodes_protocol_specific_replies() {
        let command = json!({"method":"item/commandExecution/requestApproval"});
        assert_eq!(
            codex_response(&command, &PermissionResponse::Deny { reason: None })["decision"],
            "decline"
        );
        let permissions = json!({"method":"item/permissions/requestApproval","params":{"permissions":{"write":true}}});
        assert_eq!(
            codex_response(&permissions, &PermissionResponse::Allow)["permissions"]["write"],
            true
        );
        let elicitation = json!({"method":"mcpServer/elicitation/request"});
        assert_eq!(
            codex_response(&elicitation, &PermissionResponse::Cancelled)["action"],
            "cancel"
        );
    }
}
