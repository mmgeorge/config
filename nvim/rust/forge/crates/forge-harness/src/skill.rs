use crate::permissions::command::{CommandInvocation, normalize_command_in};
use crate::permissions::matcher::PermissionTarget;
use crate::permissions::shell::CommandShell;
use crate::provider::{ExecutionOwner, ProviderEvent, ProviderEventSink};
use crate::settings::SkillResource;
use anyhow::{Context, Result};
#[cfg(test)]
use serde_json::Value;
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

    fn reads(&self, event: &ProviderEvent) -> Vec<SkillRead> {
        let ProviderEvent::Tool {
            address,
            update: crate::provider::ToolUpdate::Requested { invocation },
        } = event
        else {
            return Vec::new();
        };
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
                    tool_id: address.tool_id.to_string(),
                })
            })
            .collect()
    }
}

/// Reconciles sparse tool input and completion before attaching confirmed read metadata.
pub(crate) struct SkillObserver {
    access: Arc<SkillAccess>,
    pending: HashMap<crate::provider::ToolAddress, Vec<SkillRead>>,
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
        let ProviderEvent::Tool { address, update } = event else {
            return Vec::new();
        };
        let reads = self.access.reads(event);
        if !reads.is_empty() {
            self.pending.insert(address.clone(), reads);
        }
        let crate::provider::ToolUpdate::Finished { outcome } = update else {
            return Vec::new();
        };
        let reads = self.pending.remove(address).unwrap_or_default();
        if matches!(outcome, crate::provider::ToolOutcome::Succeeded) {
            reads
        } else {
            Vec::new()
        }
    }
}

/// Removes an execution's temporary skill grants on every completion or cancellation path.
pub(crate) struct SkillExecution {
    pub(crate) observer: Arc<Mutex<SkillObserver>>,
    pub(crate) access: Arc<SkillAccess>,
    pub(crate) owner: ExecutionOwner,
    pub(crate) coordinator: Arc<crate::provider::approval::PermissionCoordinator>,
}

impl SkillExecution {
    /// Attach the shared observer to streamed delivery or annotate collected fallback events.
    pub(crate) fn sink(&self, sink: Option<ProviderEventSink>) -> Option<ProviderEventSink> {
        sink.map(|sink| sink.with_observer(Arc::new(SkillEventObserver(self.observer.clone()))))
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

pub use forge_provider_api::skill::SkillRead;

struct SkillEventObserver(Arc<Mutex<SkillObserver>>);

impl forge_provider_api::events::EventObserver for SkillEventObserver {
    fn additional_events(
        &self,
        event: &ProviderEvent,
    ) -> Result<Vec<ProviderEvent>, forge_provider_api::events::EventDeliveryError> {
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
            ProviderEvent::Tool { address, .. } if !read.is_empty() => {
                Ok(vec![ProviderEvent::Tool {
                    address: address.clone(),
                    update: crate::provider::ToolUpdate::SkillRead { read },
                }])
            }
            _ => Ok(Vec::new()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::{TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};
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

    fn event(id: &str, update: ToolUpdate) -> ProviderEvent {
        ProviderEvent::Tool {
            address: crate::provider::ToolAddress {
                turn: crate::provider::TurnAddress {
                    scope: crate::provider::InvocationScope {
                        owner: ExecutionOwner::root("session"),
                        exchange_id: "exchange".into(),
                        invocation_id: "invocation".into(),
                    },
                    thread_id: "provider".into(),
                    turn_id: "turn".into(),
                },
                tool_id: id.into(),
            },
            update,
        }
    }

    fn requested(id: &str, name: &str, arguments: Value) -> ProviderEvent {
        event(
            id,
            ToolUpdate::Requested {
                invocation: forge_providers::copilot::normalize_invocation(ToolInvocation {
                    semantics: Default::default(),
                    kind: ToolActivityKind::ToolCall,
                    name: name.into(),
                    title: name.into(),
                    arguments,
                }),
            },
        )
    }

    #[test]
    fn skill_reads_reconcile_normalized_invocations_without_using_activation_events() {
        let (_directory, access, bundle) = fixture();
        let path = bundle.join("SKILL.md").display().to_string();
        let command = format!("Get-Content -LiteralPath '{path}' -TotalCount 5");
        for (name, arguments) in [
            ("powershell", json!({"command":command})),
            ("view", json!({"path":path})),
            ("read", json!({"file_path":path})),
            ("read", json!({"AbsolutePath":path})),
        ] {
            let mut observer = SkillObserver::new(access.clone());
            assert!(
                observer
                    .observe(&requested("read", name, arguments))
                    .is_empty()
            );
            assert!(
                observer
                    .observe(&event(
                        "read",
                        ToolUpdate::Output {
                            update: TextUpdate::Append("partial output".into()),
                        }
                    ))
                    .is_empty()
            );
            assert_eq!(observer.pending.len(), 1);
            let reads = observer.observe(&event(
                "read",
                ToolUpdate::Finished {
                    outcome: ToolOutcome::Succeeded,
                },
            ));
            assert_eq!(reads.len(), 1);
            assert_eq!(reads[0].resource_id, "fixture-id");
            assert_eq!(reads[0].tool_id, "read");
            assert!(observer.pending.is_empty());
        }
        for (name, arguments) in [
            ("search", json!({"path":path})),
            ("list_directory", json!({"path":bundle})),
            ("skill.invoked", json!({"path":path})),
            (
                "powershell",
                json!({"command":format!("Write-Output '{path}'")}),
            ),
        ] {
            let mut observer = SkillObserver::new(access.clone());
            observer.observe(&requested("unrelated", name, arguments));
            assert!(
                observer
                    .observe(&event(
                        "unrelated",
                        ToolUpdate::Finished {
                            outcome: ToolOutcome::Succeeded
                        }
                    ))
                    .is_empty()
            );
        }
        for outcome in [
            ToolOutcome::Failed {
                diagnostic: crate::provider::ToolDiagnostic {
                    code: None,
                    message: "exit 1".into(),
                },
            },
            ToolOutcome::Cancelled,
            ToolOutcome::Interrupted,
        ] {
            let mut observer = SkillObserver::new(access.clone());
            observer.observe(&requested("failed", "read", json!({"path":path})));
            assert!(
                observer
                    .observe(&event("failed", ToolUpdate::Finished { outcome }))
                    .is_empty()
            );
        }
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
        observer.observe(&requested(
            "alias",
            "read",
            json!({"path":alias.join("SKILL.md")}),
        ));
        let reads = observer.observe(&event(
            "alias",
            ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        ));
        assert_eq!(
            reads[0].path,
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
        use crate::buffer::projection::{ProjectionSource, TranscriptAction};
        use crate::exchange::{Exchange, ExchangeNode};
        use crate::provider::{TurnAddress, TurnUpdate};
        let (_directory, access, bundle) = fixture();
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"exchange","session_id":"session","agent_id":"primary","ordinal":1,
            "prompt":"Read skill","state":"running","created_at_ms":0,
            "attributed_matches_checkpoint":false,"node_list":[]
        }))?;
        exchange.resume(0)?;
        let binding = crate::turn::test_support::address(&exchange, "first").scope;
        let (sink, mut stream) = crate::provider::events::channel(binding.clone());
        let sink = sink.with_observer(Arc::new(SkillEventObserver(Arc::new(Mutex::new(
            SkillObserver::new(access),
        )))));
        for turn_id in ["first", "second"] {
            let turn = TurnAddress {
                scope: binding.clone(),
                thread_id: "provider".into(),
                turn_id: turn_id.into(),
            };
            exchange.observe_turn(
                &ProviderEvent::Turn {
                    address: turn.clone(),
                    update: TurnUpdate::Started { started_at_ms: 1 },
                },
                1,
            )?;
            for tool_id in ["read-one", "read-two"] {
                let address = crate::provider::ToolAddress {
                    turn: turn.clone(),
                    tool_id: tool_id.into(),
                };
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Requested {
                        invocation: ToolInvocation {
                            semantics: forge_provider_api::tool::ToolSemantics::Read {
                                paths: vec![bundle.join("SKILL.md").display().to_string()],
                                directory: None,
                            },
                            kind: ToolActivityKind::ToolCall,
                            name: "read".into(),
                            title: "Read".into(),
                            arguments: json!({"path":bundle.join("SKILL.md")}),
                        },
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Tool {
                    address,
                    update: ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                })
                .await?;
                while let Some(observation) = stream.try_next()? {
                    exchange.observe_turn(&observation, 2)?;
                }
            }
            exchange.observe_turn(
                &ProviderEvent::Turn {
                    address: turn,
                    update: TurnUpdate::Finished {
                        outcome: crate::turn::TurnOutcome::Completed,
                        completed_at_ms: 4,
                    },
                },
                4,
            )?;
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
        assert_eq!(exchange.tools().count(), 4);
        let rendered = crate::buffer::projection::project(
            ProjectionSource::Exchange(&exchange),
            &forge_buffer::width::WidthProfile::default(),
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
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
        assert!(
            diamonds
                .iter()
                .all(|block| block.metadata.layout.as_ref().unwrap().indent == 4)
        );
        assert!(rendered.action.values().any(
            |action| matches!(action,TranscriptAction::File {path,line:1}
            if Path::new(path) == dunce::canonicalize(bundle.join("SKILL.md")).unwrap())
        ));
        assert_eq!(rendered.tool.len(), 4);
        Ok(())
    }
}
