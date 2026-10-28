use super::{CheckCategory, CheckConfig, CheckScope, CheckSelection};
use anyhow::Result;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
/// Describes a proposed command without recording it as an executed check.
pub struct CheckPreview {
    /// Saved definition identity used to correlate preview and execution.
    pub id: String,
    /// Execution stage shown before the command.
    pub category: CheckCategory,
    /// Concrete command, or the template when implementation is still required.
    pub command: String,
    /// Requested coverage policy.
    pub scope: CheckScope,
    /// Selection reason or the unresolved prerequisite.
    pub reason: String,
    /// Whether required input values or plan context are still unavailable.
    pub pending: bool,
    /// Whether the proposed change set selects this definition.
    pub selected: bool,
}

/// Resolves planned changes with the same invocation resolver used during Checking.
pub(crate) async fn plan(
    workspace: &Path,
    design: &crate::plan::DeclarationDesign,
) -> Result<Vec<CheckPreview>> {
    let Some(snapshot) = &design.check_configuration else {
        return Ok(Vec::new());
    };
    let files = design.changed_paths();
    let mut changed = Vec::new();
    let mut new = Vec::new();
    for file in &design.document.tests {
        for case in &file.cases {
            let selection = super::CheckCase {
                file: file.file.clone(),
                name: case.name.clone(),
                target: case.target.clone(),
            };
            if case.change == crate::plan::DesignTestChange::New {
                new.push(selection.clone());
            }
            if matches!(
                case.change,
                crate::plan::DesignTestChange::New | crate::plan::DesignTestChange::Modified
            ) {
                changed.push(selection);
            }
        }
    }
    let mut config = snapshot.config.clone();
    for check in &mut config.definitions {
        if design.document.disabled_checks.contains(&check.id) {
            check.enabled = false;
        }
    }
    resolve(
        workspace,
        &config,
        &files,
        Some(&changed),
        Some(&new),
        &design.document.checks,
    )
    .await
}

/// Builds a read-only preview from task inputs without creating run history or output files.
pub(super) async fn resolve(
    workspace: &Path,
    config: &CheckConfig,
    files: &[String],
    cases: Option<&[super::CheckCase]>,
    new_cases: Option<&[super::CheckCase]>,
    included: &[String],
) -> Result<Vec<CheckPreview>> {
    let request = crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
        include: included.to_vec(),
        ..Default::default()
    });
    let applicability = super::applicability::resolve(&request, config, Some(files), true)?;
    let request = crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
        ids: applicability
            .iter()
            .filter(|item| item.selected)
            .map(|item| item.id.clone())
            .collect(),
        ..Default::default()
    });
    let selection = if request.ids.is_empty() {
        BTreeMap::new()
    } else {
        request.selections(config, files, cases, new_cases, true)?
    };
    let mut result = Vec::new();
    for index in config.order_selected(&request.ids.iter().cloned().collect())? {
        let definition = &config.definitions[index];
        let applicable = &applicability[index];
        let scope = selection
            .get(&definition.id)
            .cloned()
            .unwrap_or(CheckSelection::All);
        let operation = super::supervisor::operation(definition, &scope).unwrap_or(&definition.all);
        let mut preview = CheckPreview {
            id: definition.id.clone(),
            category: definition.category,
            command: operation.display(),
            scope: definition.scope,
            reason: applicable.reason.clone(),
            pending: false,
            selected: applicable.selected,
        };
        if applicable.selected && scope.empty() {
            preview.selected = false;
            preview.pending = matches!(scope, CheckSelection::Cases { .. })
                && (if definition.scope == CheckScope::NewCases {
                    new_cases.is_none()
                } else {
                    cases.is_none()
                });
            preview.reason = if preview.pending {
                "Plan case inventory is unavailable"
            } else if definition.scope == CheckScope::NewCases {
                "No new tests in this plan"
            } else {
                "No matching changes"
            }
            .into();
        } else if applicable.selected {
            match super::invocation::resolve(
                workspace,
                &definition.directory,
                operation,
                &scope,
                &applicable.matched_files,
                cases.unwrap_or_default(),
            ) {
                Ok(invocations) => {
                    for invocation in invocations {
                        let mut item = preview.clone();
                        if operation.args.is_some() {
                            item.command = invocation.display();
                        }
                        if !invocation.files.is_empty() {
                            item.reason = if included.contains(&definition.id)
                                || applicable.reason.starts_with("Required by")
                            {
                                format!(
                                    "{}. Inputs: {}",
                                    applicable.reason,
                                    invocation.files.join(", ")
                                )
                            } else {
                                format!("Changed: {}", invocation.files.join(", "))
                            };
                        }
                        result.push(item);
                    }
                    continue;
                }
                Err(error) => {
                    preview.pending = true;
                    preview.reason = format!("Pending validation: {error:#}");
                }
            }
        }
        result.push(preview);
    }
    Ok(result)
}
