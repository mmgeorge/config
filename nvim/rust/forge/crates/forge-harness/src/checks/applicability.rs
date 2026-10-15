use super::{CheckConfig, RunRequest};
use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
/// Selects automatic checks from changed project paths independently of execution scope.
pub enum CheckTrigger {
    /// Runs when the task changes at least one file.
    AnyChange,
    /// Runs when a changed path matches an include and no exclude pattern.
    Paths {
        /// Project-relative patterns supporting `*`, `?`, and whole-component `**`.
        include: Vec<String>,
        /// Patterns that remove individual paths from the included set.
        exclude: Vec<String>,
    },
    /// Runs only through explicit selection or as another check's prerequisite.
    Manual,
}

impl CheckTrigger {
    /// Rejects unsupported patterns instead of silently broadening automatic coverage.
    pub(crate) fn validate(&self) -> Result<()> {
        if let Self::Paths { include, exclude } = self {
            ensure!(
                !include.is_empty() && include.len() + exclude.len() <= 128,
                "path triggers require 1 to 128 patterns"
            );
            for pattern in include.iter().chain(exclude) {
                super::config::relative_path(pattern)?;
                ensure!(
                    !pattern.contains(['[', ']', '{', '}', '!'])
                        && pattern
                            .split('/')
                            .all(|component| !component.contains("**") || component == "**"),
                    "unsupported check trigger pattern: {pattern}"
                );
            }
        }
        Ok(())
    }

    /// Describes the saved trigger for plan reviews and check details.
    pub fn description(&self) -> String {
        match self {
            Self::AnyChange => "Any changed file".into(),
            Self::Manual => "Explicit runs only".into(),
            Self::Paths { include, exclude } => {
                let included = include.join(", ");
                if exclude.is_empty() {
                    included
                } else {
                    format!("{included}, excluding {}", exclude.join(", "))
                }
            }
        }
    }

    /// Tests a project-relative path without consulting its current filesystem existence.
    pub(crate) fn matches_path(&self, path: &str) -> bool {
        match self {
            Self::AnyChange => true,
            Self::Manual => false,
            Self::Paths { include, exclude } => {
                include.iter().any(|pattern| path_matches(pattern, path))
                    && !exclude.iter().any(|pattern| path_matches(pattern, path))
            }
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Explains automatic selection without equating an inapplicable check with a successful command.
pub struct CheckApplicability {
    /// Saved check identity.
    pub id: String,
    /// Whether this request selects the check, including required prerequisites.
    pub selected: bool,
    /// User-facing selection or exclusion reason.
    pub reason: String,
    /// Changed paths matched by the trigger, including deleted and old rename paths.
    pub matched_files: Vec<String>,
}

/// Resolves overlapping path triggers and includes prerequisites of selected checks.
pub(super) fn resolve(
    request: &RunRequest,
    config: &CheckConfig,
    files: Option<&[String]>,
    has_scope: bool,
) -> Result<Vec<CheckApplicability>> {
    ensure!(
        !request.all || (request.ids.is_empty() && request.selections.is_empty()),
        "choose all checks or explicit selections, not both"
    );
    let known: BTreeSet<_> = config
        .definitions
        .iter()
        .map(|check| check.id.as_str())
        .collect();
    ensure!(
        request.ids.iter().all(|id| known.contains(id.as_str())),
        "unknown check id"
    );
    ensure!(
        request
            .selections
            .keys()
            .all(|id| known.contains(id.as_str())
                && (request.ids.is_empty() || request.ids.contains(id))),
        "selection is outside the requested checks"
    );
    ensure!(
        config.definitions.iter().all(|check| check.enabled
            || (!request.ids.contains(&check.id) && !request.selections.contains_key(&check.id))),
        "selected check is disabled; re-enable it in /checks"
    );
    let mut result: BTreeMap<_, _> = config
        .definitions
        .iter()
        .map(|check| {
            let matched_files: Vec<_> = files.unwrap_or_default()
                .iter()
                .filter(|path| check.trigger.matches_path(path))
                .cloned()
                .collect();
            let (selected, reason) = if !check.enabled {
                (false, "Disabled for this task".into())
            } else if request.all
                || request.ids.contains(&check.id)
                || request.selections.contains_key(&check.id)
            {
                (true, "Explicitly requested".into())
            } else if !request.ids.is_empty() {
                (false, "Not requested".into())
            } else if matches!(check.trigger, CheckTrigger::Manual) {
                (false, "Explicit runs only".into())
            } else if !has_scope {
                (
                    false,
                    "No task change context. Select checks explicitly.".into(),
                )
            } else if files.is_none() {
                (true, "Task changes unavailable. Automatic trigger requires verification.".into())
            } else if matched_files.is_empty() {
                (false, "No matching task changes".into())
            } else {
                let summary = matched_files
                    .iter()
                    .take(3)
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                let suffix = if matched_files.len() > 3 {
                    format!(" (+{} more)", matched_files.len() - 3)
                } else {
                    String::new()
                };
                (true, format!("Changed: {summary}{suffix}"))
            };
            (
                check.id.clone(),
                CheckApplicability {
                    id: check.id.clone(),
                    selected,
                    reason,
                    matched_files,
                },
            )
        })
        .collect();
    loop {
        let mut added = false;
        for check in &config.definitions {
            if !result[&check.id].selected {
                continue;
            }
            for dependency in &check.depends_on {
                let prerequisite = result
                    .get_mut(dependency)
                    .ok_or_else(|| anyhow::anyhow!("missing prerequisite {dependency}"))?;
                if !prerequisite.selected {
                    prerequisite.selected = true;
                    prerequisite.reason = format!("Required by {}", check.name);
                    added = true;
                }
            }
        }
        if !added {
            break;
        }
    }
    Ok(config
        .definitions
        .iter()
        .map(|check| result.remove(&check.id).unwrap())
        .collect())
}

fn path_matches(pattern: &str, path: &str) -> bool {
    let pattern: Vec<_> = pattern.split('/').collect();
    let path: Vec<_> = path.split('/').collect();
    let mut matched = vec![false; path.len() + 1];
    matched[0] = true;
    for component in pattern {
        let mut next = vec![false; path.len() + 1];
        if component == "**" {
            next[0] = matched[0];
            for position in 1..=path.len() {
                next[position] = matched[position] || next[position - 1];
            }
        } else {
            for position in 1..=path.len() {
                next[position] =
                    matched[position - 1] && component_matches(component, path[position - 1]);
            }
        }
        matched = next;
    }
    matched[path.len()]
}

fn component_matches(pattern: &str, name: &str) -> bool {
    let pattern: Vec<_> = pattern.chars().collect();
    let name: Vec<_> = name.chars().collect();
    let (mut pattern_index, mut name_index, mut star, mut retry) = (0, 0, None, 0);
    while name_index < name.len() {
        if pattern_index < pattern.len()
            && (pattern[pattern_index] == '?' || pattern[pattern_index] == name[name_index])
        {
            pattern_index += 1;
            name_index += 1;
        } else if pattern.get(pattern_index) == Some(&'*') {
            star = Some(pattern_index);
            pattern_index += 1;
            retry = name_index;
        } else if let Some(position) = star {
            retry += 1;
            name_index = retry;
            pattern_index = position + 1;
        } else {
            return false;
        }
    }
    pattern[pattern_index..]
        .iter()
        .all(|character| *character == '*')
}
