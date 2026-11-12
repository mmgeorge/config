use super::command::{CommandInvocation, CommandNormalization, command_token_list};
use super::document::{
    CATEGORY_BASH, CATEGORY_EDIT, CATEGORY_ELEVATE, CATEGORY_MCP, CATEGORY_READ, CATEGORY_TOOL,
    CATEGORY_WEBFETCH, PermissionDecision, PermissionDocument,
};
use super::shell::CommandShell;
use std::ops::Range;
use std::path::{Component, Path, PathBuf};

pub use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};

/// Stores a validated permission document compiled for one workspace.
#[derive(Clone, Debug)]
pub struct CompiledPermissionDocument {
    pub document: PermissionDocument,
    workspace: PathBuf,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PermissionEvaluation {
    pub decision: PermissionDecision,
    pub matched_pattern_list: Vec<String>,
    /// Distinct targets that still require an explicit user decision.
    pub pending_target_list: Vec<PermissionTarget>,
    /// Preserves the source occurrences used by the same evaluation that requested approval.
    pub pending_command_list: Vec<PendingCommand>,
}

pub use forge_provider_api::permissions::CommandAnalysisError;

#[derive(Clone, Debug, Eq, PartialEq)]
/// Binds one unapproved executable occurrence to its complete original source.
pub struct PendingCommand {
    /// Identifies the pending target whose choices cover this occurrence.
    pub target: PermissionTarget,
    /// Retains the complete provider command before splitting or launcher decoding.
    pub source: String,
    /// Selects the grammar of the original source.
    pub shell: CommandShell,
    /// Locates the executable occurrence in original source byte coordinates.
    pub source_range: Range<usize>,
}

struct TargetEvaluation {
    target: PermissionTarget,
    decision: PermissionDecision,
    matched_pattern: Option<String>,
    command: Option<PendingCommand>,
}

fn normalized_path(path: &Path) -> Option<PathBuf> {
    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                if !result.pop() {
                    return None;
                }
            }
            _ => result.push(component.as_os_str()),
        }
    }
    Some(result)
}

#[cfg(windows)]
pub(crate) fn path_starts_with(path: &Path, root: &Path) -> bool {
    let path = path
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase();
    let root = root
        .to_string_lossy()
        .replace('\\', "/")
        .to_ascii_lowercase();
    path == root || path.starts_with(&(root.trim_end_matches('/').to_owned() + "/"))
}

#[cfg(not(windows))]
pub(crate) fn path_starts_with(path: &Path, root: &Path) -> bool {
    path.starts_with(root)
}

fn command_pattern_score(pattern: &str, invocation: &CommandInvocation) -> Option<usize> {
    let pattern_token_list = command_token_list(pattern).ok()?;
    super::command_adapter::pattern_score(&pattern_token_list, invocation)
}

fn qualified_pattern_score(pattern: &str, target: &str) -> Option<usize> {
    if pattern == "*" {
        return Some(0);
    }
    if let Some(prefix) = pattern.strip_suffix('*') {
        return target.starts_with(prefix).then_some(prefix.len());
    }
    (pattern == target).then_some(pattern.len() * 2 + 1)
}

fn path_pattern_score(pattern: &str, target: &str, workspace: &Path) -> Option<usize> {
    if pattern == "*" {
        return Some(0);
    }
    let pattern = pattern.strip_suffix("/**").unwrap_or(pattern);
    let root = if pattern == "." {
        workspace.to_path_buf()
    } else if Path::new(pattern).is_absolute() {
        PathBuf::from(pattern)
    } else {
        workspace.join(pattern.trim_start_matches("./"))
    };
    let root = normalized_path(&root)?;
    let target = normalized_path(Path::new(target))?;
    let score = root.components().count();
    if path_starts_with(&target, &root) {
        return Some(score);
    }
    let root = super::directory::resolve_path(&root, workspace).ok()?;
    let target = super::directory::resolve_path(&target, workspace).ok()?;
    path_starts_with(&target, &root).then_some(score)
}

fn combine_decision(result_list: &[TargetEvaluation]) -> PermissionDecision {
    if result_list
        .iter()
        .any(|result| result.decision == PermissionDecision::Deny)
    {
        PermissionDecision::Deny
    } else if result_list
        .iter()
        .any(|result| result.decision == PermissionDecision::Ask)
    {
        PermissionDecision::Ask
    } else {
        PermissionDecision::Allow
    }
}

impl CompiledPermissionDocument {
    pub fn compile(document: PermissionDocument, workspace: impl Into<PathBuf>) -> Self {
        Self {
            document,
            workspace: workspace.into(),
        }
    }

    pub fn workspace(&self) -> &Path {
        &self.workspace
    }

    fn best_rule(
        &self,
        category: &str,
        target: &str,
        invocation: Option<&CommandInvocation>,
    ) -> (PermissionDecision, Option<String>) {
        self.document
            .permission
            .get(category)
            .into_iter()
            .flat_map(|rule_map| rule_map.iter())
            .filter_map(|(pattern, decision)| {
                let score = if let Some(invocation) = invocation {
                    command_pattern_score(pattern, invocation)
                } else if matches!(category, CATEGORY_READ | CATEGORY_EDIT) {
                    path_pattern_score(pattern, target, &self.workspace)
                } else {
                    qualified_pattern_score(pattern, target)
                }?;
                Some((*decision, pattern.clone(), score))
            })
            .max_by_key(|(_, _, score)| *score)
            .map_or((PermissionDecision::Ask, None), |(decision, pattern, _)| {
                (decision, Some(format!("{category}:{pattern}")))
            })
    }

    fn evaluate_command(
        &self,
        command: &str,
        shell: Option<CommandShell>,
    ) -> Result<Vec<TargetEvaluation>, CommandAnalysisError> {
        let CommandNormalization {
            invocation_list,
            failure,
        } = super::command::normalize_command_in(command, shell.unwrap_or_default());
        if let Some(failure) = failure {
            return Err(CommandAnalysisError {
                shell: shell.unwrap_or_default(),
                failure,
            });
        }
        Ok(invocation_list
            .iter()
            .map(|invocation| {
                let (decision, matched_pattern) =
                    if invocation.token_list.first().is_some_and(|command| {
                        matches!(
                            command.to_ascii_lowercase().as_str(),
                            "read" | "write" | "webfetch" | "mcp" | "elevate" | "tool"
                        )
                    }) {
                        (PermissionDecision::Deny, None)
                    } else {
                        let (decision, pattern) =
                            self.best_rule(CATEGORY_BASH, command, Some(invocation));
                        (
                            if invocation.structural && decision != PermissionDecision::Deny {
                                PermissionDecision::Allow
                            } else {
                                decision
                            },
                            pattern,
                        )
                    };
                TargetEvaluation {
                    target: PermissionTarget::Command {
                        command: invocation.source.clone(),
                        shell: Some(invocation.shell),
                    },
                    decision,
                    matched_pattern,
                    command: (decision == PermissionDecision::Ask).then(|| PendingCommand {
                        target: PermissionTarget::Command {
                            command: invocation.source.clone(),
                            shell: Some(invocation.shell),
                        },
                        source: command.into(),
                        shell: shell.unwrap_or_default(),
                        source_range: invocation.source_range.clone(),
                    }),
                }
            })
            .collect())
    }

    /// Evaluates parsed targets, rejecting unresolved shell syntax before any approval is created.
    pub fn evaluate(
        &self,
        request: &PermissionRequest,
    ) -> Result<PermissionEvaluation, CommandAnalysisError> {
        let mut result_list = Vec::new();
        for target in &request.target_list {
            let (decision, matched_pattern) = match target {
                PermissionTarget::Unknown => (PermissionDecision::Deny, None),
                PermissionTarget::UnknownFile { .. } => (PermissionDecision::Ask, None),
                PermissionTarget::Command { command, shell } => {
                    result_list.extend(self.evaluate_command(command, *shell)?);
                    continue;
                }
                PermissionTarget::Read { path } => self.best_rule(CATEGORY_READ, path, None),
                PermissionTarget::Write { path } => self.best_rule(CATEGORY_EDIT, path, None),
                PermissionTarget::Network { target } => {
                    self.best_rule(CATEGORY_WEBFETCH, target, None)
                }
                PermissionTarget::Mcp { target } => self.best_rule(CATEGORY_MCP, target, None),
                PermissionTarget::Elevate { target } => {
                    self.best_rule(CATEGORY_ELEVATE, target, None)
                }
                PermissionTarget::Tool { target } => self.best_rule(CATEGORY_TOOL, target, None),
                PermissionTarget::HostTool { name } => {
                    let (decision, pattern) = self.best_rule(CATEGORY_TOOL, name, None);
                    (
                        if pattern.is_none() {
                            PermissionDecision::Allow
                        } else {
                            decision
                        },
                        pattern,
                    )
                }
            };
            result_list.push(TargetEvaluation {
                target: target.clone(),
                decision,
                matched_pattern,
                command: None,
            });
        }
        let mut pending_target_list = Vec::new();
        for result in &result_list {
            if result.decision == PermissionDecision::Ask
                && !pending_target_list.contains(&result.target)
            {
                pending_target_list.push(result.target.clone());
            }
        }
        Ok(PermissionEvaluation {
            decision: combine_decision(&result_list),
            pending_target_list,
            pending_command_list: result_list
                .iter()
                .filter(|result| result.decision == PermissionDecision::Ask)
                .filter_map(|result| result.command.clone())
                .collect(),
            matched_pattern_list: result_list
                .into_iter()
                .filter_map(|result| result.matched_pattern)
                .collect(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::permissions::document::parse_permission_document;

    fn compiled(source: &str) -> CompiledPermissionDocument {
        CompiledPermissionDocument::compile(parse_permission_document(source).unwrap(), "D:/repo")
    }

    /// Match Git operations through supported prefixes in every shell without changing source.
    #[test]
    fn git_adapter_matches_supported_prefixes_and_preserves_source() {
        let policy =
            compiled(r#"{"permission":{"bash":{"git ls-files":"allow","git commit":"deny"}}}"#);
        for shell in [
            CommandShell::PowerShell,
            CommandShell::Bash,
            CommandShell::Zsh,
            CommandShell::Nushell,
        ] {
            for command in [
                "git -C repo ls-files",
                "git --no-pager ls-files",
                "git -C 'repo space' --no-pager ls-files --cached",
                "git --no-pager -C root -C child ls-files",
                "git -C '' ls-files",
            ] {
                let result = policy
                    .evaluate(&PermissionRequest {
                        id: "git-adapter".into(),
                        provider: forge_provider_api::ProviderId::new("test").unwrap(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: command.into(),
                            shell: Some(shell),
                        }],
                    })
                    .unwrap();
                assert_eq!(
                    result.decision,
                    PermissionDecision::Allow,
                    "{shell:?}: {command}"
                );
            }
        }
        for (command, decision) in [
            (r"git -C D:\config ls-files", PermissionDecision::Allow),
            (
                "git -C repo ls-files; git --no-pager -C repo commit -m test",
                PermissionDecision::Deny,
            ),
            ("git --no-pager -C repo clean -fd", PermissionDecision::Ask),
        ] {
            let result = policy
                .evaluate(&PermissionRequest {
                    id: "git-source".into(),
                    provider: forge_provider_api::ProviderId::new("test").unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Command {
                        command: command.into(),
                        shell: Some(CommandShell::PowerShell),
                    }],
                })
                .unwrap();
            assert_eq!(result.decision, decision, "{command}");
            if decision == PermissionDecision::Ask {
                assert_eq!(result.pending_command_list[0].source, command);
                assert_eq!(
                    result.pending_command_list[0].source_range,
                    0..command.len()
                );
                assert_eq!(
                    result.pending_target_list[0],
                    PermissionTarget::Command {
                        command: command.into(),
                        shell: Some(CommandShell::PowerShell),
                    }
                );
            }
        }
    }

    /// Keep unknown global options and all subcommand arguments visible to ordinary rules.
    #[test]
    fn git_adapter_does_not_strip_unrecognized_or_subcommand_options() {
        let policy = compiled(
            r#"{"permission":{"bash":{"git ls-files":"allow","git branch --list":"allow","git -c color.ui=false ls-files":"allow"}}}"#,
        );
        for (command, expected) in [
            ("git -c color.ui=false ls-files", PermissionDecision::Allow),
            (
                "git -c core.sshCommand=custom ls-files",
                PermissionDecision::Ask,
            ),
            (
                "git -C repo -c color.ui=false ls-files",
                PermissionDecision::Ask,
            ),
            (
                "git --config-env=name=ENV ls-files",
                PermissionDecision::Ask,
            ),
            ("git --exec-path=custom ls-files", PermissionDecision::Ask),
            ("git --git-dir=custom ls-files", PermissionDecision::Ask),
            ("git --work-tree=custom ls-files", PermissionDecision::Ask),
            ("git --paginate ls-files", PermissionDecision::Ask),
            ("git -Crepo ls-files", PermissionDecision::Ask),
            ("git -C", PermissionDecision::Ask),
            ("git --no-pager", PermissionDecision::Ask),
            ("git -C repo branch --list", PermissionDecision::Allow),
            ("git -C repo branch --delete topic", PermissionDecision::Ask),
            (
                "git -C repo branch --no-pager --list",
                PermissionDecision::Ask,
            ),
        ] {
            let result = policy
                .evaluate(&PermissionRequest {
                    id: "git-options".into(),
                    provider: forge_provider_api::ProviderId::new("test").unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Command {
                        command: command.into(),
                        shell: Some(CommandShell::PowerShell),
                    }],
                })
                .unwrap();
            assert_eq!(result.decision, expected, "{command}");
        }
    }

    /// Retain context constraints and priority when options are reordered or repeated.
    #[test]
    fn git_adapter_preserves_explicit_context_rules() {
        let policy = compiled(
            r#"{"permission":{"bash":{"git ls-files":"allow","git -C secret ls-files":"deny","git --no-pager diff":"deny","git diff":"allow","git -C root -C child ls-files":"deny"}}}"#,
        );
        for (command, expected) in [
            (
                "git --no-pager -C secret ls-files",
                PermissionDecision::Deny,
            ),
            ("git -C public ls-files", PermissionDecision::Allow),
            (
                "git -C root --no-pager -C child ls-files",
                PermissionDecision::Deny,
            ),
            ("git -C root -C other ls-files", PermissionDecision::Allow),
            ("git -C repo --no-pager diff", PermissionDecision::Deny),
            ("git -C repo diff", PermissionDecision::Allow),
        ] {
            let result = policy
                .evaluate(&PermissionRequest {
                    id: "git-context".into(),
                    provider: forge_provider_api::ProviderId::new("test").unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Command {
                        command: command.into(),
                        shell: Some(CommandShell::PowerShell),
                    }],
                })
                .unwrap();
            assert_eq!(result.decision, expected, "{command}");
        }
    }

    /// Allow built-in tools by default while retaining explicit policy and resource checks.
    #[test]
    fn built_in_tools_default_to_allow_without_bypassing_saved_rules() {
        let request = |target| PermissionRequest {
            id: "built-in".into(),
            provider: forge_provider_api::ProviderId::new("fixture").unwrap(),
            reason: None,
            target_list: vec![target],
        };
        let defaults = compiled(r#"{"permission":{}}"#);
        for name in [
            "lsp_status",
            "lsp_rename_symbol",
            "harness_config",
            "goal_status",
        ] {
            let evaluation = defaults
                .evaluate(&request(PermissionTarget::HostTool { name: name.into() }))
                .unwrap();
            assert_eq!(evaluation.decision, PermissionDecision::Allow);
            assert!(evaluation.pending_target_list.is_empty());
        }
        for target in [
            PermissionTarget::Tool {
                target: "external_tool".into(),
            },
            PermissionTarget::Mcp {
                target: "server/tool".into(),
            },
            PermissionTarget::Write {
                path: "D:/repo/source.rs".into(),
            },
        ] {
            assert_eq!(
                defaults.evaluate(&request(target)).unwrap().decision,
                PermissionDecision::Ask
            );
        }
        let explicit =
            compiled(r#"{"permission":{"tool":{"lsp_*":"deny","harness_config":"ask"}}}"#);
        for (name, decision) in [
            ("lsp_status", PermissionDecision::Deny),
            ("harness_config", PermissionDecision::Ask),
        ] {
            assert_eq!(
                explicit
                    .evaluate(&request(PermissionTarget::HostTool { name: name.into() }))
                    .unwrap()
                    .decision,
                decision
            );
        }
    }

    /// Deny unclassified operations even when the user allows every known tool.
    #[test]
    fn unknown_operations_cannot_match_a_wildcard_allow() {
        let permission = compiled(
            r#"{"permission":{"tool":{"*":"allow"},"mcp":{"*":"allow"},"bash":{"*":"allow"}}}"#,
        );
        let request = PermissionRequest {
            id: "unknown".into(),
            provider: forge_provider_api::ProviderId::new("fixture").unwrap(),
            reason: None,
            target_list: vec![PermissionTarget::Unknown],
        };
        assert_eq!(
            permission.evaluate(&request).unwrap().decision,
            PermissionDecision::Deny
        );
    }

    #[test]
    fn nushell_script_files_retain_exact_rules_and_pipeline_denials() {
        let permission = compiled(
            r#"{"permission":{"bash":{"nu *":"allow","nu --no-config-file forbidden.nu":"deny","git reset *":"deny"}}}"#,
        );
        for (command, expected) in [
            ("nu --no-config-file checks.nu", PermissionDecision::Allow),
            ("nu --no-config-file forbidden.nu", PermissionDecision::Deny),
            (
                "nu --no-config-file checks.nu | git reset --hard",
                PermissionDecision::Deny,
            ),
        ] {
            let request = PermissionRequest {
                id: "nushell-file".into(),
                provider: forge_provider_api::ProviderId::new("test")
                    .expect("static provider identity is valid"),
                reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: command.into(),
                    shell: Some(CommandShell::Zsh),
                }],
            };
            assert_eq!(
                permission.evaluate(&request).unwrap().decision,
                expected,
                "{command}"
            );
        }
    }

    #[test]
    fn specific_command_rules_override_catch_all_and_compounds_keep_denials() {
        let permission =
            compiled(r#"{"permission":{"bash":{"*":"deny","git":"allow","git push":"ask"}}}"#);
        let mut request = PermissionRequest {
            id: "1".into(),
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: String::new(),
                shell: None,
            }],
        };
        for (command, expected) in [
            ("git status", PermissionDecision::Allow),
            ("git push origin main", PermissionDecision::Ask),
            ("git status && cargo test", PermissionDecision::Deny),
            ("git $(echo status)", PermissionDecision::Deny),
        ] {
            request.target_list = vec![PermissionTarget::Command {
                command: command.into(),
                shell: None,
            }];
            assert_eq!(
                permission.evaluate(&request).unwrap().decision,
                expected,
                "command: {command}"
            );
        }
        request.target_list = vec![PermissionTarget::Command {
            command: "git status".into(),
            shell: None,
        }];
        assert_eq!(
            compiled(r#"{"permission":{"bash":{"*":"allow","git status":"deny"}}}"#)
                .evaluate(&request)
                .unwrap()
                .decision,
            PermissionDecision::Deny
        );
    }
}
