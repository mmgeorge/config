use super::command::{CommandInvocation, CommandNormalization, command_token_list};
use super::document::{
    CATEGORY_BASH, CATEGORY_EDIT, CATEGORY_ELEVATE, CATEGORY_MCP, CATEGORY_READ, CATEGORY_TOOL,
    CATEGORY_WEBFETCH, PermissionDecision, PermissionDocument,
};
use crate::session::PermissionMode;
use serde::{Deserialize, Serialize};
use std::path::{Component, Path, PathBuf};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PermissionTarget {
    Command { command: String },
    Read { path: String },
    Write { path: String },
    Network { target: String },
    Mcp { target: String },
    Elevate { target: String },
    Tool { target: String },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PermissionRequest {
    pub id: String,
    pub provider: String,
    pub reason: Option<String>,
    pub target_list: Vec<PermissionTarget>,
}

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
fn path_starts_with(path: &Path, root: &Path) -> bool {
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
fn path_starts_with(path: &Path, root: &Path) -> bool {
    path.starts_with(root)
}

fn command_pattern_score(pattern: &str, invocation: &CommandInvocation) -> Option<usize> {
    let pattern_token_list = command_token_list(pattern).ok()?;
    let wildcard = pattern_token_list.last().is_some_and(|token| token == "*");
    let literal_count = pattern_token_list
        .len()
        .saturating_sub(usize::from(wildcard));
    if literal_count == 0 {
        return (pattern_token_list.len() == 1 && wildcard).then_some(0);
    }
    if invocation.token_list.len() < literal_count {
        return None;
    }
    let matches = pattern_token_list[..literal_count]
        .iter()
        .zip(&invocation.token_list)
        .all(|(expected, actual)| {
            if cfg!(windows) {
                expected.eq_ignore_ascii_case(actual)
            } else {
                expected == actual
            }
        });
    matches.then_some(literal_count * 2 + usize::from(!wildcard))
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
    path_starts_with(&target, &root).then_some(root.components().count())
}

fn combine_decision(result_list: &[(PermissionDecision, Option<String>)]) -> PermissionDecision {
    if result_list
        .iter()
        .any(|(decision, _)| *decision == PermissionDecision::Deny)
    {
        PermissionDecision::Deny
    } else if result_list
        .iter()
        .any(|(decision, _)| *decision == PermissionDecision::Ask)
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

    fn evaluate_command(&self, command: &str) -> Vec<(PermissionDecision, Option<String>)> {
        let CommandNormalization {
            invocation_list,
            ambiguous,
        } = super::command::normalize_command(command);
        if ambiguous || invocation_list.is_empty() {
            return vec![(PermissionDecision::Ask, None)];
        }
        invocation_list
            .iter()
            .map(|invocation| {
                if invocation.token_list.first().is_some_and(|command| {
                    matches!(
                        command.to_ascii_lowercase().as_str(),
                        "read" | "write" | "webfetch" | "mcp" | "elevate" | "tool"
                    )
                }) {
                    return (PermissionDecision::Deny, None);
                }
                self.best_rule(CATEGORY_BASH, command, Some(invocation))
            })
            .collect()
    }

    pub fn evaluate(
        &self,
        mode: PermissionMode,
        request: &PermissionRequest,
    ) -> PermissionEvaluation {
        let mut result_list = Vec::new();
        for target in &request.target_list {
            if mode == PermissionMode::Yolo {
                result_list.push((PermissionDecision::Allow, None));
                continue;
            }
            match target {
                PermissionTarget::Command { command } => {
                    result_list.extend(self.evaluate_command(command));
                }
                PermissionTarget::Read { path } => {
                    result_list.push(self.best_rule(CATEGORY_READ, path, None));
                }
                PermissionTarget::Write { path } => {
                    let (decision, pattern) = self.best_rule(CATEGORY_EDIT, path, None);
                    result_list.push((if mode == PermissionMode::Read && decision == PermissionDecision::Allow {
                        PermissionDecision::Ask
                    } else { decision }, pattern));
                }
                PermissionTarget::Network { target } => {
                    result_list.push(self.best_rule(CATEGORY_WEBFETCH, target, None));
                }
                PermissionTarget::Mcp { target } => {
                    result_list.push(self.best_rule(CATEGORY_MCP, target, None));
                }
                PermissionTarget::Elevate { target } => {
                    result_list.push(self.best_rule(CATEGORY_ELEVATE, target, None));
                }
                PermissionTarget::Tool { target } => {
                    result_list.push(self.best_rule(CATEGORY_TOOL, target, None));
                }
            }
        }
        PermissionEvaluation {
            decision: combine_decision(&result_list),
            matched_pattern_list: result_list
                .into_iter()
                .filter_map(|(_, pattern)| pattern)
                .collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::permissions::document::parse_permission_document;

    fn compiled(source: &str) -> CompiledPermissionDocument {
        CompiledPermissionDocument::compile(parse_permission_document(source).unwrap(), "D:/repo")
    }

    #[test]
    fn prefers_specific_command_denial_and_aggregates_compounds() {
        let permission = compiled(
            r#"{"permission":{"bash":{"git *":"allow","git commit":"deny","rg *":"allow"}}}"#,
        );
        let allowed = PermissionRequest {
            id: "1".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: "git status && rg foo".into(),
            }],
        };
        assert_eq!(
            permission.evaluate(PermissionMode::Read, &allowed).decision,
            PermissionDecision::Allow
        );
        let denied = PermissionRequest {
            target_list: vec![PermissionTarget::Command {
                command: "git commit -m test".into(),
            }],
            ..allowed
        };
        assert_eq!(
            permission.evaluate(PermissionMode::Read, &denied).decision,
            PermissionDecision::Deny
        );
    }

    #[test]
    fn catch_all_command_rule_supplies_the_default_decision() {
        let request = PermissionRequest {
            id: "1".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: "cargo test".into(),
            }],
        };
        for (rule, expected) in [
            ("allow", PermissionDecision::Allow),
            ("ask", PermissionDecision::Ask),
            ("deny", PermissionDecision::Deny),
        ] {
            let permission = compiled(&format!(r#"{{"permission":{{"bash":{{"*":"{rule}"}}}}}}"#));
            assert_eq!(
                permission.evaluate(PermissionMode::Read, &request).decision,
                expected
            );
        }
        assert_eq!(
            compiled(r#"{"permission":{"bash":{"git":"allow"}}}"#)
                .evaluate(PermissionMode::Read, &request)
                .decision,
            PermissionDecision::Ask
        );
    }

    #[test]
    fn specific_command_rules_override_catch_all_and_compounds_keep_denials() {
        let permission =
            compiled(r#"{"permission":{"bash":{"*":"deny","git":"allow","git push":"ask"}}}"#);
        let mut request = PermissionRequest {
            id: "1".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: String::new(),
            }],
        };
        for (command, expected) in [
            ("git status", PermissionDecision::Allow),
            ("git push origin main", PermissionDecision::Ask),
            ("git status && cargo test", PermissionDecision::Deny),
            ("", PermissionDecision::Ask),
            ("git $(echo status)", PermissionDecision::Ask),
        ] {
            request.target_list = vec![PermissionTarget::Command {
                command: command.into(),
            }];
            assert_eq!(
                permission.evaluate(PermissionMode::Read, &request).decision,
                expected,
                "command: {command}"
            );
        }
        request.target_list = vec![PermissionTarget::Command {
            command: "git status".into(),
        }];
        assert_eq!(
            compiled(r#"{"permission":{"bash":{"*":"allow","git status":"deny"}}}"#)
                .evaluate(PermissionMode::Read, &request)
                .decision,
            PermissionDecision::Deny
        );
    }

    #[test]
    fn approval_mode_does_not_define_filesystem_scope() {
        let permission = compiled(r#"{"permission":{"edit":{"*":"allow"}}}"#);
        let workspace_write = PermissionRequest {
            id: "1".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Write {
                path: "D:/repo/src/main.rs".into(),
            }],
        };
        assert_eq!(
            permission
                .evaluate(PermissionMode::Read, &workspace_write)
                .decision,
            PermissionDecision::Ask
        );
        assert_eq!(
            permission
                .evaluate(PermissionMode::Write, &workspace_write)
                .decision,
            PermissionDecision::Allow
        );
        let outside = PermissionRequest {
            target_list: vec![PermissionTarget::Write {
                path: "D:/outside.txt".into(),
            }],
            ..workspace_write
        };
        assert_eq!(
            permission.evaluate(PermissionMode::Write, &outside).decision,
            PermissionDecision::Allow
        );
        assert_eq!(
            permission.evaluate(PermissionMode::Write, &outside).decision,
            PermissionDecision::Allow
        );
    }
}
