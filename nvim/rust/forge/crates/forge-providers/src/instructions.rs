//! Attribute instruction content at the native adapter boundary, never from model prose.

use std::collections::{HashMap, HashSet};
use std::path::{Component, Path, PathBuf};

use forge_provider_api::tool::ToolSemantics;
use forge_provider_api::{
    AdapterActivity, InstructionRead, ItemCompletion, ItemEvent, ItemId, ItemStart, ThreadId,
    ToolDescriptor, ToolItemEvent, ToolOutcome, TurnAddress, TurnId, TurnMetadataEvent,
};
use forge_shell::command::{CommandInvocation, normalize_command_in};
use forge_shell::shell::CommandShell;

/// Retain native instruction sources and successful reads for one provider binding.
#[derive(Default)]
pub(crate) struct InstructionTracker {
    initialized: bool,
    initial: Option<Vec<String>>,
    known: HashSet<String>,
    observed: HashSet<(ThreadId, TurnId, String)>,
    pending: HashMap<ItemId, Vec<String>>,
}

impl InstructionTracker {
    /// Report whether this binding has obtained its startup snapshot.
    pub(crate) fn initialized(&self) -> bool {
        self.initialized
    }
    /// Retain the first startup snapshot, without emitting discovered conditional sources.
    pub(crate) fn initialize(
        &mut self,
        included: Vec<String>,
        available: Vec<String>,
        workspace: &str,
    ) {
        if self.initialized {
            return;
        }
        self.initialized = true;
        for path in available.iter().chain(&included) {
            self.known.insert(path_key(&resolve(path, workspace)));
        }
        self.initial = Some(included);
    }

    /// Publish the startup snapshot only after this binding's first native turn starts.
    pub(crate) fn initial(
        &mut self,
        address: &TurnAddress,
        workspace: &str,
    ) -> Vec<AdapterActivity> {
        self.initial
            .take()
            .unwrap_or_default()
            .into_iter()
            .filter_map(|path| self.read(address, &path, workspace))
            .collect()
    }

    /// Attribute native loads and suppress overlapping reports of one path in the same turn.
    pub(crate) fn read(
        &mut self,
        address: &TurnAddress,
        path: &str,
        workspace: &str,
    ) -> Option<AdapterActivity> {
        self.read_in_turn(&address.thread_id, &address.turn_id, path, workspace)
    }

    fn read_in_turn(
        &mut self,
        thread_id: &ThreadId,
        turn_id: &TurnId,
        path: &str,
        workspace: &str,
    ) -> Option<AdapterActivity> {
        let path = resolve(path, workspace);
        let file_name = Path::new(&path).file_name()?.to_str()?.to_owned();
        if file_name.is_empty() || path.trim().is_empty() {
            return None;
        }
        let key = path_key(&path);
        self.known.insert(key.clone());
        if !self
            .observed
            .insert((thread_id.clone(), turn_id.clone(), key))
        {
            return None;
        }
        Some(AdapterActivity::Turn {
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
            event: TurnMetadataEvent::InstructionRead(InstructionRead { path, file_name }),
        })
    }

    /// Observe typed tool descriptors, reporting file access only after successful completion.
    pub(crate) fn observe(
        &mut self,
        activity: &AdapterActivity,
        workspace: &str,
    ) -> Vec<AdapterActivity> {
        let AdapterActivity::Item {
            thread_id,
            turn_id,
            item_id,
            event,
        } = activity
        else {
            return Vec::new();
        };
        let descriptor = match event {
            ItemEvent::Started(
                ItemStart::CommandExecution(descriptor) | ItemStart::ToolCall(descriptor),
            )
            | ItemEvent::Tool {
                event: ToolItemEvent::DescriptorUpdated(descriptor),
            } => Some(descriptor),
            ItemEvent::Completed(
                ItemCompletion::CommandExecution(completion) | ItemCompletion::ToolCall(completion),
            ) => completion.descriptor.as_ref(),
            _ => None,
        };
        if let Some(descriptor) = descriptor {
            self.pending
                .insert(item_id.clone(), read_paths(descriptor, workspace));
        }
        let ItemEvent::Completed(
            ItemCompletion::CommandExecution(completion) | ItemCompletion::ToolCall(completion),
        ) = event
        else {
            return Vec::new();
        };
        let paths = self.pending.remove(item_id).unwrap_or_default();
        if completion.outcome != ToolOutcome::Succeeded {
            return Vec::new();
        }
        paths
            .into_iter()
            .filter(|path| self.known.contains(&path_key(path)) || instruction_path(path))
            .collect::<Vec<_>>()
            .into_iter()
            .filter_map(|path| self.read_in_turn(thread_id, turn_id, &path, workspace))
            .collect()
    }
}

/// Resolve provider-native paths without opening files or guessing their contents.
fn resolve(path: &str, workspace: &str) -> String {
    if path.is_empty() {
        return String::new();
    }
    let portable = path.replace('\\', "/");
    let path = Path::new(&portable);
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        Path::new(workspace).join(path)
    };
    let mut resolved = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                resolved.pop();
            }
            component => resolved.push(component.as_os_str()),
        }
    }
    resolved.to_string_lossy().into_owned()
}

fn path_key(path: &str) -> String {
    let path = path.replace('\\', "/");
    if cfg!(windows) {
        path.to_lowercase()
    } else {
        path
    }
}

/// Recognize documented instruction locations, never arbitrary Markdown or skill files.
fn instruction_path(path: &str) -> bool {
    let path = path.replace('\\', "/");
    let name = path.rsplit('/').next().unwrap_or_default();
    matches!(
        name,
        "AGENTS.md"
            | "AGENTS.override.md"
            | "CLAUDE.md"
            | "CLAUDE.local.md"
            | "copilot-instructions.md"
    ) || (path.contains("/.claude/rules/") && name.ends_with(".md"))
        || ((path.contains("/.github/instructions/") || path.contains("/.copilot/instructions/"))
            && name.ends_with(".instructions.md"))
}

fn read_paths(descriptor: &ToolDescriptor, workspace: &str) -> Vec<String> {
    let (paths, directory) = match &descriptor.semantics {
        ToolSemantics::Read { paths, directory } => (paths.clone(), directory.as_deref()),
        ToolSemantics::Command {
            text,
            shell,
            directory,
        } => {
            let shell = shell
                .as_deref()
                .map(CommandShell::from_executable)
                .unwrap_or_default();
            let parsed = normalize_command_in(text, shell);
            if parsed.failure.is_some() {
                return Vec::new();
            }
            (
                parsed
                    .invocation_list
                    .iter()
                    .flat_map(CommandInvocation::read_paths)
                    .filter(|path| !path.contains(['*', '?', '[', ']']))
                    .collect(),
                directory.as_deref(),
            )
        }
        ToolSemantics::Other => return Vec::new(),
    };
    let directory = directory.map(|directory| resolve(directory, workspace));
    paths
        .into_iter()
        .map(|path| resolve(&path, directory.as_deref().unwrap_or(workspace)))
        .collect()
}

/// Parse only Copilot runtime instruction-discovery framing, never model-authored prose.
pub(crate) fn copilot_discovery_paths(content: &str) -> Vec<String> {
    content
        .split("<system_reminder>\n")
        .skip(1)
        .filter_map(|block| {
            let header = block.strip_prefix("Custom instructions from ")?;
            let (path, _) = header.split_once(". Apply these to any code you write here:\n")?;
            (!path.trim().is_empty() && !path.contains(['\n', '\r'])).then(|| path.to_owned())
        })
        .collect()
}

/// Preserve imported source paths carried inside a Copilot startup source's expanded body.
pub(crate) fn copilot_imports(content: &str) -> Vec<String> {
    content
        .split("<imported_custom_instruction source=\"")
        .skip(1)
        .filter_map(|block| block.split_once("\">\n").map(|(path, _)| path.to_owned()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{InspectableArguments, InvocationScope, ToolCompletion};
    use serde_json::json;

    fn address(turn: &str) -> TurnAddress {
        let scope = InvocationScope::new(
            forge_provider_api::testing::root_thread("instructions"),
            turn,
        );
        TurnAddress {
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope,
        }
    }

    fn read_activity(
        address: &TurnAddress,
        name: &str,
        semantics: ToolSemantics,
        outcome: ToolOutcome,
    ) -> AdapterActivity {
        AdapterActivity::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: ItemId::new(name).unwrap(),
            event: ItemEvent::Completed(ItemCompletion::ToolCall(ToolCompletion {
                outcome,
                descriptor: Some(ToolDescriptor {
                    name: "read".into(),
                    title: "Read file".into(),
                    arguments: InspectableArguments::new(json!({})),
                    semantics,
                }),
                output: None,
                changes: Default::default(),
            })),
        }
    }

    /// Startup never reports a discovered conditional body, and overlapping evidence deduplicates per turn.
    #[test]
    fn initial_snapshot_excludes_conditional_sources_and_preserves_later_reads() {
        let mut tracker = InstructionTracker::default();
        tracker.initialize(
            vec!["CLAUDE.md".into(), "imported.md".into()],
            vec![".claude/rules/scoped.md".into()],
            "/fixture",
        );
        let first = address("first");
        let initial = tracker.initial(&first, "/fixture");
        assert_eq!(initial.len(), 2);
        assert!(tracker.initial(&first, "/fixture").is_empty());
        assert!(tracker.read(&first, "./CLAUDE.md", "/fixture").is_none());
        assert!(
            tracker
                .read(&first, ".claude/rules/scoped.md", "/fixture")
                .is_some()
        );
        assert!(
            tracker
                .read(&address("second"), "CLAUDE.md", "/fixture")
                .is_some()
        );
        let AdapterActivity::Turn {
            event: TurnMetadataEvent::InstructionRead(read),
            ..
        } = &initial[0]
        else {
            panic!("instruction event")
        };
        assert_eq!(read.file_name, "CLAUDE.md");
        assert!(read.path.ends_with("CLAUDE.md"));
    }

    /// Explicit reads need a successful outcome and a known source or documented instruction location.
    #[test]
    fn explicit_reads_reject_failures_skills_and_ordinary_markdown() {
        let mut tracker = InstructionTracker::default();
        let owner = address("read");
        for (index, path, outcome, expected) in [
            ("denied", "AGENTS.md", ToolOutcome::Cancelled, 0),
            ("ordinary", "README.md", ToolOutcome::Succeeded, 0),
            ("skill", "skills/one/SKILL.md", ToolOutcome::Succeeded, 0),
            (
                "rule",
                ".github/instructions/nested.instructions.md",
                ToolOutcome::Succeeded,
                1,
            ),
            ("root", "AGENTS.md", ToolOutcome::Succeeded, 1),
        ] {
            let activity = read_activity(
                &owner,
                index,
                ToolSemantics::Read {
                    paths: vec![path.into()],
                    directory: None,
                },
                outcome,
            );
            assert_eq!(
                tracker.observe(&activity, "/fixture").len(),
                expected,
                "{path}"
            );
        }
    }

    /// Shell reads use the same literal-path parser as skill attribution, with no command execution.
    #[test]
    fn shell_reads_share_literal_command_semantics() {
        for (index, command, expected) in [
            ("read", "Get-Content -LiteralPath 'AGENTS.md' -Raw", 1),
            ("glob", "Get-Content -Path '.claude/rules/*.md'", 0),
            ("output", "Write-Output 'AGENTS.md'", 0),
            (
                "search",
                "Select-String -Path 'AGENTS.md' -Pattern 'rule'",
                0,
            ),
        ] {
            let mut tracker = InstructionTracker::default();
            let activity = read_activity(
                &address("shell"),
                index,
                ToolSemantics::Command {
                    text: command.into(),
                    shell: Some("pwsh".into()),
                    directory: Some("/fixture".into()),
                },
                ToolOutcome::Succeeded,
            );
            assert_eq!(tracker.observe(&activity, "/other").len(), expected);
        }
    }

    /// Only runtime instruction-discovery framing contributes file paths.
    #[test]
    fn copilot_framing_identifies_discovered_and_imported_files() {
        assert!(copilot_discovery_paths("The model says it read AGENTS.md").is_empty());
        let message = "\n<system_reminder>\nCustom instructions from nested/AGENTS.md. Apply these to any code you write here:\n\nRules\n</system_reminder>";
        assert_eq!(copilot_discovery_paths(message), ["nested/AGENTS.md"]);
        assert_eq!(
            copilot_imports(
                "<imported_custom_instruction source=\"/fixture/imported.md\">\nRules\n</imported_custom_instruction>"
            ),
            ["/fixture/imported.md"]
        );
    }
}
