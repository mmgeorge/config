use crate::permissions::command::{CommandInvocation, normalize_command_in};
use crate::permissions::matcher::PermissionTarget;
use crate::permissions::shell::CommandShell;
use crate::provider::{ProviderEvent, ThreadOwner};
use crate::settings::SkillResource;
use anyhow::{Context, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

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

    fn reads(
        &self,
        item_id: &forge_provider_api::ItemId,
        invocation: &forge_provider_api::tool::ToolDescriptor,
    ) -> Vec<SkillRead> {
        let (path, directory) = match &invocation.semantics {
            forge_provider_api::tool::ToolSemantics::Command {
                text,
                shell,
                directory,
            } => {
                let shell = shell
                    .as_deref()
                    .map(CommandShell::from_executable)
                    .unwrap_or_default();
                let parsed = normalize_command_in(text, shell);
                let path = if parsed.failure.is_none() {
                    parsed.invocation_list.iter().flat_map(read_paths).collect()
                } else {
                    Vec::new()
                };
                (path, directory.as_deref())
            }
            forge_provider_api::tool::ToolSemantics::Read { paths, directory } => {
                (paths.clone(), directory.as_deref())
            }
            forge_provider_api::tool::ToolSemantics::Other => return Vec::new(),
        };
        path.into_iter()
            .filter_map(|path| {
                let path = self.resolve(&path, directory)?;
                let skill = self.selected.iter().find(|skill| skill.source == path)?;
                Some(SkillRead {
                    resource_id: skill.resource.id.clone(),
                    name: skill.resource.name.clone(),
                    path: skill.source.display().to_string(),
                    tool_id: item_id.to_string(),
                })
            })
            .collect()
    }
}

/// Reconciles sparse tool input and completion before attaching confirmed read metadata.
pub(crate) struct SkillObserver {
    access: Arc<SkillAccess>,
    pending: HashMap<
        (
            forge_provider_api::ThreadId,
            forge_provider_api::TurnId,
            forge_provider_api::ItemId,
        ),
        Vec<SkillRead>,
    >,
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
    pub(crate) fn observe(&mut self, event: &ProviderEvent) -> Vec<SkillRead> {
        use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart};
        let ProviderEvent::Item {
            thread_id,
            turn_id,
            item_id,
            event,
        } = event
        else {
            return Vec::new();
        };
        let key = (thread_id.clone(), turn_id.clone(), item_id.clone());
        let descriptor = match event {
            ItemEvent::Started(
                ItemStart::CommandExecution(descriptor)
                | ItemStart::FileChange(descriptor)
                | ItemStart::ToolCall(descriptor),
            )
            | ItemEvent::Tool {
                event: forge_provider_api::tool::ToolItemEvent::DescriptorUpdated(descriptor),
            } => Some(descriptor),
            ItemEvent::Completed(
                ItemCompletion::CommandExecution(completion)
                | ItemCompletion::FileChange(completion)
                | ItemCompletion::ToolCall(completion),
            ) => completion.descriptor.as_ref(),
            ItemEvent::Started(
                ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_),
            )
            | ItemEvent::Message { .. }
            | ItemEvent::Tool { .. }
            | ItemEvent::Completed(
                ItemCompletion::UserMessage(_)
                | ItemCompletion::AgentMessage(_)
                | ItemCompletion::Reasoning(_),
            ) => None,
        };
        if let Some(descriptor) = descriptor {
            let reads = self.access.reads(item_id, descriptor);
            self.pending.insert(key.clone(), reads);
        }
        match event {
            ItemEvent::Completed(
                ItemCompletion::CommandExecution(completion)
                | ItemCompletion::FileChange(completion)
                | ItemCompletion::ToolCall(completion),
            ) => {
                let reads = self.pending.remove(&key).unwrap_or_default();
                if matches!(completion.outcome, crate::provider::ToolOutcome::Succeeded) {
                    reads
                } else {
                    Vec::new()
                }
            }
            ItemEvent::Started(_)
            | ItemEvent::Message { .. }
            | ItemEvent::Tool { .. }
            | ItemEvent::Completed(
                ItemCompletion::UserMessage(_)
                | ItemCompletion::AgentMessage(_)
                | ItemCompletion::Reasoning(_),
            ) => Vec::new(),
        }
    }
}

/// Removes an execution's temporary skill grants on every completion or cancellation path.
pub(crate) struct SkillExecution {
    pub(crate) observer: Arc<Mutex<SkillObserver>>,
    pub(crate) access: Arc<SkillAccess>,
    pub(crate) owner: ThreadOwner,
    pub(crate) coordinator: Arc<crate::provider::approval::PermissionCoordinator>,
}

impl SkillExecution {
    /// Observe normalized tool events without exposing provider wire decoding.
    ///
    /// The bridge relay calls this observer through `HostInvocation::observe`, which appends
    /// each derived skill read after its source event under the same invocation scope.
    pub(crate) fn event_observer(&self) -> Arc<dyn crate::provider::events::EventObserver> {
        Arc::new(SkillEventObserver(self.observer.clone()))
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

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
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

struct SkillEventObserver(Arc<Mutex<SkillObserver>>);

impl crate::provider::events::EventObserver for SkillEventObserver {
    fn additional_events(
        &self,
        _scope: &crate::provider::EventScope,
        event: &ProviderEvent,
    ) -> Result<Vec<crate::provider::HostEvent>, forge_provider_api::events::EventDeliveryError>
    {
        let read = self
            .0
            .lock()
            .map_err(|_| {
                forge_provider_api::events::EventDeliveryError::transport(
                    "skill observer lock poisoned",
                )
            })?
            .observe(event);
        match event {
            ProviderEvent::Item {
                thread_id,
                turn_id,
                item_id,
                ..
            } if !read.is_empty() => Ok(vec![crate::provider::HostEvent::SkillRead {
                thread_id: thread_id.clone(),
                turn_id: turn_id.clone(),
                item_id: item_id.clone(),
                read,
            }]),
            ProviderEvent::Provider { .. }
            | ProviderEvent::Thread { .. }
            | ProviderEvent::Turn { .. }
            | ProviderEvent::Item { .. }
            | ProviderEvent::Agent { .. } => Ok(Vec::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
