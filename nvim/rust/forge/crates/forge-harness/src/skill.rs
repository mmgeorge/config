use crate::backend::{BackendEvent, BackendEventSink, ExecutionOwner};
use crate::permissions::command::{CommandInvocation, normalize_command_in};
use crate::permissions::matcher::PermissionTarget;
use crate::permissions::shell::CommandShell;
use crate::settings::SkillResource;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a confirmed selected-skill read and its originating provider tool.
pub struct SkillRead {
    /// Identifies the selected catalog definition, independently of profile aliases.
    pub resource_id: String,
    /// Names the skill displayed in the timeline.
    pub name: String,
    /// Opens the canonical skill definition after runtime profiles are removed.
    pub path: String,
    /// Identifies the provider tool within its owning turn.
    pub tool_id: String,
}

/// Shares selected bundle containment between read detection and runtime approval.
pub(crate) struct SkillAccess {
    workspace: PathBuf,
    selected: Vec<SelectedSkill>,
}

impl SkillAccess {
    /// Resolve selected sources without granting their parent directories.
    pub(crate) fn new(workspace: &str, resource: &[SkillResource]) -> Result<Self> {
        let selected = resource
            .iter()
            .map(|resource| {
                let source = dunce::canonicalize(&resource.source)
                    .with_context(|| format!("resolve selected skill {}", resource.name))?;
                let directory = source
                    .parent()
                    .context("skill source has no directory")?
                    .to_path_buf();
                Ok(SelectedSkill {
                    resource: resource.clone(),
                    source,
                    directory,
                })
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(Self {
            workspace: PathBuf::from(workspace),
            selected,
        })
    }

    fn resolve(&self, path: &str, directory: Option<&str>) -> Option<PathBuf> {
        if path.is_empty() || path.contains(['*', '?', '$', '`']) {
            return None;
        }
        let path = Path::new(path);
        let directory = directory
            .map(PathBuf::from)
            .unwrap_or_else(|| self.workspace.clone());
        let directory = if directory.is_absolute() {
            directory
        } else {
            self.workspace.join(directory)
        };
        dunce::canonicalize(if path.is_absolute() {
            path.to_path_buf()
        } else {
            directory.join(path)
        })
        .ok()
    }

    /// Return an existing canonical target only when it remains in a selected bundle.
    pub(crate) fn contained(&self, path: &str) -> Option<PathBuf> {
        let path = self.resolve(path, None)?;
        self.selected
            .iter()
            .any(|skill| path.starts_with(&skill.directory))
            .then_some(path)
    }

    /// Grant only literal bundle reads or script launches, never inline interpreter code.
    pub(crate) fn approves(&self, target: &PermissionTarget) -> bool {
        match target {
            PermissionTarget::Read { path } => self.contained(path).is_some(),
            PermissionTarget::Command { command, shell } => {
                if command.contains(['>', '<']) {
                    return false;
                }
                let parsed = normalize_command_in(command, shell.unwrap_or_default());
                parsed.failure.is_none()
                    && !parsed.invocation_list.is_empty()
                    && parsed.invocation_list.iter().all(|invocation| {
                        if invocation.source.contains(['>', '<']) {
                            return false;
                        }
                        if let Some(path) = script_path(invocation) {
                            return Path::new(path).is_absolute()
                                && self.contained(path).is_some_and(|path| path.is_file());
                        }
                        let paths = read_paths(invocation);
                        !paths.is_empty()
                            && paths.iter().all(|path| {
                                Path::new(path).is_absolute() && self.contained(path).is_some()
                            })
                    })
            }
            _ => false,
        }
    }

    /// Resolve canonical read targets so profile aliases cannot bypass explicit source denials.
    pub(crate) fn permission_paths(&self, target: &PermissionTarget) -> Vec<PathBuf> {
        let paths = match target {
            PermissionTarget::Read { path } => vec![path.clone()],
            PermissionTarget::Command { command, shell } => {
                normalize_command_in(command, shell.unwrap_or_default())
                    .invocation_list
                    .iter()
                    .flat_map(|invocation| {
                        script_path(invocation)
                            .map(|path| vec![path.to_owned()])
                            .unwrap_or_else(|| read_paths(invocation))
                    })
                    .collect()
            }
            _ => Vec::new(),
        };
        paths
            .iter()
            .filter_map(|path| self.contained(path))
            .collect()
    }

    fn reads(&self, event: &BackendEvent) -> Vec<SkillRead> {
        let Some(activity) = &event.activity else {
            return Vec::new();
        };
        let data = event.data.get("data").unwrap_or(&event.data);
        let input = data
            .get("rawInput")
            .or_else(|| data.get("arguments"))
            .or_else(|| data.pointer("/params/item/arguments"))
            .unwrap_or(data);
        let input = if let Some(encoded) = input.as_str() {
            serde_json::from_str(encoded).unwrap_or(Value::Null)
        } else {
            input.clone()
        };
        let directory = string_at(&input, &["/cwd", "/workdir"])
            .or_else(|| string_at(data, &["/params/item/cwd", "/params/cwd"]));
        let command = string_at(&input, &["/command", "/commandLine", "/CommandLine"])
            .or_else(|| string_at(data, &["/params/item/command", "/params/command"]));
        let mut path = Vec::new();
        if let Some(command) = command {
            let shell = string_at(&input, &["/shell"])
                .map(CommandShell::from_executable)
                .unwrap_or_else(CommandShell::default);
            let parsed = normalize_command_in(command, shell);
            if parsed.failure.is_none() {
                for invocation in &parsed.invocation_list {
                    path.extend(read_paths(invocation));
                }
            }
        } else {
            let name = data
                .get("toolName")
                .and_then(Value::as_str)
                .unwrap_or(&activity.title);
            let name = name
                .split('(')
                .next()
                .unwrap_or(name)
                .trim()
                .to_ascii_lowercase();
            let read = match data.get("kind").and_then(Value::as_str) {
                Some(kind) => kind == "read",
                None => matches!(
                    name.as_str(),
                    "read" | "read_file" | "readfile" | "view" | "view_file" | "viewfile"
                ),
            };
            if read {
                if let Some(value) = string_at(
                    &input,
                    &[
                        "/path",
                        "/file_path",
                        "/filePath",
                        "/absolute_path",
                        "/AbsolutePath",
                        "/TargetFile",
                    ],
                ) {
                    path.push(value.to_owned());
                } else if let Some(locations) = data.get("locations").and_then(Value::as_array) {
                    path.extend(
                        locations
                            .iter()
                            .filter_map(|location| location["path"].as_str())
                            .map(str::to_owned),
                    );
                }
            }
        }
        path.into_iter()
            .filter_map(|path| {
                let path = self.resolve(&path, directory)?;
                let skill = self.selected.iter().find(|skill| skill.source == path)?;
                Some(SkillRead {
                    resource_id: skill.resource.id.clone(),
                    name: skill.resource.name.clone(),
                    path: skill.source.display().to_string(),
                    tool_id: activity.id.clone(),
                })
            })
            .collect()
    }
}

/// Reconciles sparse tool input and completion before attaching confirmed read metadata.
pub(crate) struct SkillObserver {
    access: Arc<SkillAccess>,
    pending: HashMap<String, Vec<SkillRead>>,
}

impl SkillObserver {
    /// Observe one execution using its immutable selected-skill catalog.
    pub(crate) fn new(access: Arc<SkillAccess>) -> Self {
        Self {
            access,
            pending: HashMap::new(),
        }
    }

    /// Annotate successful completions without treating progress or filenames in output as reads.
    pub(crate) fn observe(&mut self, event: &mut BackendEvent) {
        let Some(activity) = &event.activity else {
            return;
        };
        let Some(address) = &event.address else {
            return;
        };
        let identity = format!(
            "{}\0{}\0{}",
            address.thread_id, address.turn_id, activity.id
        );
        let reads = self.access.reads(event);
        if !reads.is_empty() {
            self.pending.insert(identity.clone(), reads);
        }
        let status = activity
            .status
            .as_deref()
            .unwrap_or_default()
            .to_ascii_lowercase();
        if !matches!(
            status.as_str(),
            "completed"
                | "complete"
                | "success"
                | "succeeded"
                | "failed"
                | "error"
                | "cancelled"
                | "canceled"
                | "interrupted"
                | "denied"
                | "declined"
                | "rejected"
        ) {
            return;
        }
        let Some(reads) = self.pending.remove(&identity) else {
            return;
        };
        let data = event.data.get("data").unwrap_or(&event.data);
        let exit = data
            .pointer("/params/item/exitCode")
            .or_else(|| data.pointer("/rawOutput/exitCode"))
            .or_else(|| data.pointer("/result/exitCode"))
            .and_then(Value::as_i64);
        if !matches!(
            status.as_str(),
            "completed" | "complete" | "success" | "succeeded"
        ) || exit.is_some_and(|exit| exit != 0)
            || data.get("success") == Some(&Value::Bool(false))
            || data.get("isError") == Some(&Value::Bool(true))
        {
            return;
        }
        if let Some(fields) = event.data.as_object_mut() {
            fields.insert(
                "skill_reads".into(),
                serde_json::to_value(reads).expect("skill reads serialize"),
            );
        }
    }
}

/// Removes an execution's temporary skill grants on every completion or cancellation path.
pub(crate) struct SkillExecution {
    pub(crate) observer: Arc<Mutex<SkillObserver>>,
    pub(crate) access: Arc<SkillAccess>,
    pub(crate) owner: ExecutionOwner,
    pub(crate) coordinator: Arc<crate::backend::approval::PermissionCoordinator>,
}

impl SkillExecution {
    /// Attach the shared observer to streamed delivery or annotate collected fallback events.
    pub(crate) fn sink(&self, sink: Option<BackendEventSink>) -> Option<BackendEventSink> {
        sink.map(|sink| sink.skills(self.observer.clone()))
    }

    /// Apply the same detection contract to callers without a live event sink.
    pub(crate) fn output(&self, output: &mut crate::backend::BackendOutput) -> Result<()> {
        let mut observer = self
            .observer
            .lock()
            .map_err(|_| anyhow::anyhow!("skill observer lock poisoned"))?;
        for event in &mut output.event {
            observer.observe(event);
        }
        Ok(())
    }
}

impl Drop for SkillExecution {
    fn drop(&mut self) {
        self.coordinator.release_skills(&self.owner, &self.access);
    }
}

struct SelectedSkill {
    resource: SkillResource,
    source: PathBuf,
    directory: PathBuf,
}

fn string_at<'value>(value: &'value Value, path: &[&str]) -> Option<&'value str> {
    path.iter()
        .find_map(|path| value.pointer(path).and_then(Value::as_str))
}

fn executable_name(command: &str) -> String {
    command
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or(command)
        .to_ascii_lowercase()
        .trim_end_matches(".exe")
        .to_owned()
}

fn read_paths(invocation: &CommandInvocation) -> Vec<String> {
    let name = executable_name(&invocation.command_name);
    if name == "sed"
        && invocation
            .token_list
            .get(1)
            .is_none_or(|argument| argument != "-n")
    {
        return Vec::new();
    }
    let mut arguments = invocation.token_list.iter().skip(1);
    let mut paths = Vec::new();
    while let Some(argument) = arguments.next() {
        let option = if name == "get-content" {
            argument.to_ascii_lowercase()
        } else {
            argument.clone()
        };
        match (name.as_str(), option.as_str()) {
            ("get-content", "-path" | "-literalpath") => {}
            ("get-content", "-raw") | ("cat", "--" | "-n" | "-b") | ("open", "--raw" | "-r") => {}
            ("get-content", "-totalcount" | "-tail") | ("head" | "tail", "-n" | "-c") => {
                if arguments
                    .next()
                    .and_then(|value| value.parse::<u64>().ok())
                    .is_none()
                {
                    return Vec::new();
                }
            }
            ("sed", "-n") => {
                let Some(expression) = arguments.next() else {
                    return Vec::new();
                };
                if !expression.ends_with('p')
                    || !expression[..expression.len() - 1]
                        .chars()
                        .all(|character| character.is_ascii_digit() || character == ',')
                {
                    return Vec::new();
                }
            }
            (
                "cat" | "get-content" | "head" | "tail" | "type" | "more" | "bat" | "open" | "sed",
                _,
            ) if !argument.starts_with('-') => paths.push(argument.clone()),
            _ => return Vec::new(),
        }
    }
    paths
}

fn script_path(invocation: &CommandInvocation) -> Option<&str> {
    let name = executable_name(&invocation.command_name);
    let mut arguments = invocation.token_list.iter().skip(1);
    let (extension, path) = match name.as_str() {
        "python" | "python3" => ("py", arguments.next()?.as_str()),
        "node" => ("js", arguments.next()?.as_str()),
        "nu" => ("nu", arguments.next()?.as_str()),
        "bash" | "sh" | "zsh" => ("sh", arguments.next()?.as_str()),
        "pwsh" | "powershell" => {
            let mut path = None;
            while let Some(argument) = arguments.next() {
                match argument.to_ascii_lowercase().as_str() {
                    "-noprofile" | "-noninteractive" | "-nologo" => {}
                    "-file" => {
                        path = Some(arguments.next()?.as_str());
                        break;
                    }
                    _ => return None,
                }
            }
            ("ps1", path?)
        }
        _ => ("", invocation.token_list.first()?.as_str()),
    };
    let actual = Path::new(path).extension()?.to_str()?.to_ascii_lowercase();
    let valid = if extension.is_empty() {
        matches!(
            actual.as_str(),
            "py" | "js" | "mjs" | "cjs" | "nu" | "sh" | "ps1"
        )
    } else {
        actual == extension || (extension == "js" && matches!(actual.as_str(), "mjs" | "cjs"))
    };
    (valid && !path.starts_with('-')).then_some(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{ProviderAddress, ToolActivity, ToolActivityKind};
    use serde_json::json;

    fn fixture() -> (tempfile::TempDir, Arc<SkillAccess>, PathBuf) {
        let directory = tempfile::tempdir().unwrap();
        let bundle = directory.path().join("skills/fixture");
        std::fs::create_dir_all(bundle.join("scripts")).unwrap();
        std::fs::write(bundle.join("SKILL.md"), "Instructions").unwrap();
        std::fs::write(bundle.join("scripts/check.py"), "print('ok')").unwrap();
        let resource = SkillResource {
            scope: None,
            id: "fixture-id".into(),
            name: "fixture".into(),
            description: String::new(),
            source: bundle.join("SKILL.md").display().to_string(),
            targets: Vec::new(),
            user_invocable: true,
        };
        let access =
            Arc::new(SkillAccess::new(directory.path().to_str().unwrap(), &[resource]).unwrap());
        (directory, access, bundle)
    }

    fn event(id: &str, data: Value, status: &str) -> BackendEvent {
        BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "provider".into(),
                turn_id: "turn".into(),
            }),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data,
            activity: Some(ToolActivity {
                id: id.into(),
                kind: ToolActivityKind::ToolCall,
                title: "Read".into(),
                output: Some("Instructions".into()),
                status: Some(status.into()),
                change: Default::default(),
                output_delta: false,
            }),
            summary: None,
            task_update: None,
        }
    }

    #[test]
    fn skill_reads_reconcile_each_provider_payload_without_using_activation_events() {
        let (_directory, access, bundle) = fixture();
        let path = bundle.join("SKILL.md").display().to_string();
        let command = format!("Get-Content -LiteralPath '{path}' -TotalCount 5");
        for (start, completed) in [
            (
                json!({"params":{"item":{"type":"commandExecution","command":command}}}),
                json!({"params":{"item":{"exitCode":0}}}),
            ),
            (
                json!({"data":{"toolName":"view","arguments":{"path":path}}}),
                json!({"data":{"success":true}}),
            ),
            (
                json!({"kind":"read","rawInput":{"file_path":path}}),
                json!({"kind":"read"}),
            ),
            (
                json!({"kind":"read","rawInput":{"AbsolutePath":path}}),
                json!({"kind":"read"}),
            ),
        ] {
            let mut observer = SkillObserver::new(access.clone());
            let mut start = event("read", start, "in_progress");
            observer.observe(&mut start);
            assert!(start.data.get("skill_reads").is_none());
            let mut progress = event(
                "read",
                json!({"params":{"delta":"partial output"}}),
                "in_progress",
            );
            progress.activity.as_mut().unwrap().status = None;
            observer.observe(&mut progress);
            assert!(progress.data.get("skill_reads").is_none());
            assert_eq!(observer.pending.len(), 1);
            let mut completed = event("read", completed, "completed");
            observer.observe(&mut completed);
            let reads: Vec<SkillRead> =
                serde_json::from_value(completed.data["skill_reads"].clone()).unwrap();
            assert_eq!(reads.len(), 1);
            assert_eq!(reads[0].resource_id, "fixture-id");
            assert_eq!(reads[0].tool_id, "read");
            assert!(observer.pending.is_empty());
        }
        for data in [
            json!({"kind":"search","rawInput":{"path":path}}),
            json!({"toolName":"list_directory","arguments":{"path":bundle}}),
            json!({"toolName":"skill.invoked","path":path}),
            json!({"rawInput":{"command":format!("Write-Output '{path}'")}}),
        ] {
            let mut observer = SkillObserver::new(access.clone());
            let mut unrelated = event("unrelated", data, "completed");
            observer.observe(&mut unrelated);
            assert!(unrelated.data.get("skill_reads").is_none());
        }
        for status in ["failed", "cancelled", "interrupted"] {
            let mut observer = SkillObserver::new(access.clone());
            let mut failed = event(
                "failed",
                json!({"kind":"read","rawInput":{"path":path}}),
                status,
            );
            observer.observe(&mut failed);
            assert!(failed.data.get("skill_reads").is_none());
        }
        let mut observer = SkillObserver::new(access);
        let mut failed = event(
            "exit",
            json!({"params":{"item":{"command":command,"exitCode":1}}}),
            "completed",
        );
        observer.observe(&mut failed);
        assert!(failed.data.get("skill_reads").is_none());
    }

    #[test]
    fn skill_grants_require_literal_contained_reads_or_script_launches() {
        let (directory, access, bundle) = fixture();
        let script = bundle.join("scripts/check.py").display().to_string();
        let source = bundle.join("SKILL.md").display().to_string();
        let shell = |command: String| PermissionTarget::Command {
            command,
            shell: Some(CommandShell::PowerShell),
        };
        assert!(access.approves(&PermissionTarget::Read {
            path: bundle.display().to_string()
        }));
        assert!(access.approves(&shell(format!("Get-Content -LiteralPath '{source}'"))));
        assert!(access.approves(&shell(format!("python '{script}' --help"))));
        assert!(!access.approves(&shell(format!("python -c \"print('{script}')\""))));
        assert!(!access.approves(&shell(format!("Get-Content '{source}' > output.txt"))));
        assert!(!access.approves(&shell(format!(
            "python '{script}'; Remove-Item outside.txt"
        ))));
        assert!(!access.approves(&PermissionTarget::Write { path: source }));
        assert!(!access.approves(&PermissionTarget::Read {
            path: directory.path().join("skills").display().to_string()
        }));
    }

    #[test]
    fn skill_aliases_resolve_to_sources_and_escaped_links_receive_no_grant() {
        let (directory, access, bundle) = fixture();
        let alias = directory.path().join("profile/fixture");
        let outside = directory.path().join("outside");
        std::fs::create_dir_all(alias.parent().unwrap()).unwrap();
        std::fs::create_dir_all(&outside).unwrap();
        std::fs::write(outside.join("secret.md"), "outside").unwrap();
        #[cfg(windows)]
        {
            junction::create(&bundle, &alias).unwrap();
            junction::create(&outside, bundle.join("escape")).unwrap();
        }
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&bundle, &alias).unwrap();
            std::os::unix::fs::symlink(&outside, bundle.join("escape")).unwrap();
        }
        let mut observer = SkillObserver::new(access.clone());
        let mut read = event(
            "alias",
            json!({"kind":"read","rawInput":{"path":alias.join("SKILL.md")}}),
            "completed",
        );
        observer.observe(&mut read);
        assert_eq!(
            read.data["skill_reads"][0]["path"],
            dunce::canonicalize(bundle.join("SKILL.md"))
                .unwrap()
                .display()
                .to_string()
        );
        assert!(!access.approves(&PermissionTarget::Read {
            path: alias.join("escape/secret.md").display().to_string()
        }));
        #[cfg(windows)]
        {
            junction::delete(bundle.join("escape")).unwrap();
            junction::delete(&alias).unwrap();
        }
    }

    #[tokio::test]
    async fn skill_diamonds_keep_turn_ownership_tool_history_and_file_actions_after_reload()
    -> Result<()> {
        use crate::backend::TurnBoundary;
        use crate::buffer::projection::{ProjectionSource, TranscriptAction};
        use crate::exchange::{Exchange, ExchangeNode};
        let (_directory, access, bundle) = fixture();
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"exchange","session_id":"session","agent_id":"primary","ordinal":1,
            "prompt":"Read skill","state":"running","created_at_ms":0,
            "attributed_matches_checkpoint":false,"node_list":[]
        }))?;
        exchange.resume(0)?;
        let (sink, mut stream) = crate::backend::events::channel();
        let sink = sink.skills(Arc::new(Mutex::new(SkillObserver::new(access))));
        for turn_id in ["first", "second"] {
            let mut started = event("start", Value::Null, "in_progress");
            started.activity = None;
            started.address.as_mut().unwrap().turn_id = turn_id.into();
            started.turn_boundary = Some(TurnBoundary::Started);
            sink.send_wait(started.clone()).await?;
            exchange.observe_turn(&stream.recv().await?.unwrap(), 1)?;
            for tool_id in ["read-one", "read-two"] {
                let mut read = event(
                    tool_id,
                    json!({"kind":"read","rawInput":{"path":bundle.join("SKILL.md")}}),
                    "completed",
                );
                read.address.as_mut().unwrap().turn_id = turn_id.into();
                sink.send_wait(read).await?;
                let read = stream.recv().await?.unwrap();
                exchange.observe_turn(&read, 2)?;
                exchange.observe_turn(&read, 3)?;
            }
            started.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&started, 4)?;
        }
        let exchange: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
        assert_eq!(
            exchange
                .node_list
                .iter()
                .filter(|node| matches!(node, ExchangeNode::SkillRead { .. }))
                .count(),
            2
        );
        assert_eq!(
            exchange.turn.iter().flat_map(|turn| turn.tools()).count(),
            4
        );
        let rendered = crate::buffer::projection::project(
            ProjectionSource::Exchange(&exchange),
            &forge_buffer::width::WidthProfile::default(),
            false,
            &HashMap::new(),
            &HashMap::new(),
        )?;
        let diamonds: Vec<_> = rendered
            .entry
            .block
            .iter()
            .filter(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("◇ Read skill · fixture"))
            })
            .collect();
        assert_eq!(diamonds.len(), 2);
        assert!(diamonds.iter().all(|block| block.metadata.fold.is_empty()));
        assert!(rendered.action.values().any(
            |action| matches!(action,TranscriptAction::File {path,line:1}
            if Path::new(path) == dunce::canonicalize(bundle.join("SKILL.md")).unwrap())
        ));
        assert_eq!(rendered.tool.len(), 4);
        Ok(())
    }
}
