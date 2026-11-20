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
    let mut config = snapshot.config.clone();
    for check in &mut config.definitions {
        if design.document.disabled_checks.contains(&check.id) {
            check.enabled = false;
        }
    }
    let mut included = design.document.checks.clone();
    for check in &config.definitions {
        if check.enabled
            && check.category == CheckCategory::Test
            && (design.document.test_scopes.get(&check.id)
                == Some(&crate::plan::PlanTestScope::WholeSuite)
                || !design.document.tests.is_empty())
            && !included.contains(&check.id)
        {
            included.push(check.id.clone());
        }
    }
    resolve(workspace, &config, &files, &included, Some(design)).await
}

/// Builds a read-only preview from task inputs without creating run history or output files.
pub(super) async fn resolve(
    workspace: &Path,
    config: &CheckConfig,
    files: &[String],
    included: &[String],
    plan: Option<&crate::plan::DeclarationDesign>,
) -> Result<Vec<CheckPreview>> {
    let request = crate::checks::RunRequest::Selected(crate::checks::SelectedChecks {
        include: included.to_vec(),
        ..Default::default()
    });
    let inventory = super::ChangeInventory::Known(files.to_vec());
    let applicability = super::applicability::resolve(&request, config, &inventory)?;
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
        request.selections(config, &inventory)?
    };
    let mut result = Vec::new();
    let preparing = config
        .definitions
        .iter()
        .filter(|definition| {
            request.ids.contains(&definition.id)
                && definition
                    .runner
                    .command()
                    .is_some_and(|operation| operation.prepare)
        })
        .map(|definition| definition.id.clone())
        .collect();
    for index in config.order_preparing(&preparing)? {
        let definition = &config.definitions[index];
        let applicable = &applicability[index];
        let scope = selection
            .get(&definition.id)
            .cloned()
            .unwrap_or(CheckSelection::All);
        let operation = definition.runner.command();
        let mut preview = CheckPreview {
            id: definition.id.clone(),
            category: definition.category,
            command: definition.runner.display(),
            scope: definition.scope,
            reason: applicable.reason.clone(),
            pending: false,
            selected: applicable.selected,
        };
        if let Some(plan) = plan
            && definition.category == CheckCategory::Test
        {
            let plan = &plan.document;
            let whole = plan.test_scopes.get(&definition.id)
                == Some(&crate::plan::PlanTestScope::WholeSuite);
            let present = plan
                .tests
                .iter()
                .flat_map(|file| &file.cases)
                .any(|case| case.change != crate::plan::DesignTestChange::Removed);
            preview.selected = definition.enabled && (whole || present);
            preview.pending = preview.selected && !whole;
            preview.reason = if !definition.enabled {
                "Disabled for this plan"
            } else if whole {
                "Reviewer requested the whole test suite"
            } else if plan.tests.is_empty() {
                "No test execution requested by this plan"
            } else if !present {
                "No test execution requested; removed tests will be verified absent during Check"
            } else {
                "Planned test ownership will be resolved during Check"
            }
            .into();
            result.push(preview);
            continue;
        }
        if applicable.selected && scope.default_empty() {
            preview.selected = false;
            preview.reason = "No matching changes".into();
        } else if applicable.selected
            && let Some(operation) = operation
        {
            match super::invocation::resolve(
                workspace,
                &definition.directory,
                operation,
                &scope,
                &applicable.matched_files,
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Supply a plan with a manual test check to keep trigger selection out of ownership preview.
    fn design() -> crate::plan::DeclarationDesign {
        serde_json::from_value(json!({
            "baseline":{}, "moved":{},
            "check_configuration":{"digest":"preview", "helper_digest":{}, "config":{
                "build":{"trigger":{"auto":false},"action":{"category":"custom","command":["unused"]}},
                "unit":{"trigger":{"auto":false},"action":{"category":"test","after":["build"],"adapter":{"script":{"command":["unused"]}}}}
            }}
        })).unwrap()
    }

    /// Separate empty and removal-only inventories from execution without launching discovery.
    #[tokio::test]
    async fn plan_preview_distinguishes_removal_verification_from_test_execution() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = design();
        let preview = plan(workspace.path(), &design).await.unwrap();
        let unit = preview.iter().find(|item| item.id == "unit").unwrap();
        assert!(!unit.selected && !unit.pending);
        assert_eq!(unit.reason, "No test execution requested by this plan");
        design.document.tests = serde_json::from_value(json!([{
            "file":"tests.lua", "cases":[{"name":"old", "change":"removed", "description":"retired"}]
        }])).unwrap();
        let preview = plan(workspace.path(), &design).await.unwrap();
        let unit = preview.iter().find(|item| item.id == "unit").unwrap();
        assert!(!unit.selected && !unit.pending);
        assert!(unit.reason.contains("verified absent"));
        design.document.tests[0].cases[0].change = crate::plan::DesignTestChange::Reused;
        let preview = plan(workspace.path(), &design).await.unwrap();
        let unit = preview.iter().find(|item| item.id == "unit").unwrap();
        assert!(unit.selected && unit.pending);
        assert!(
            preview
                .iter()
                .find(|item| item.id == "build")
                .unwrap()
                .selected
        );
    }

    /// Include manual whole-suite choices and their prerequisites even without planned tests.
    #[tokio::test]
    async fn whole_suite_preview_selects_dependencies_and_honors_disabled_checks() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = design();
        design
            .document
            .test_scopes
            .insert("unit".into(), crate::plan::PlanTestScope::WholeSuite);
        let preview = plan(workspace.path(), &design).await.unwrap();
        let unit = preview.iter().find(|item| item.id == "unit").unwrap();
        assert!(unit.selected && !unit.pending);
        assert!(
            preview
                .iter()
                .find(|item| item.id == "build")
                .unwrap()
                .selected
        );
        design.document.disabled_checks.push("unit".into());
        let preview = plan(workspace.path(), &design).await.unwrap();
        assert!(preview.iter().all(|item| !item.selected));
        assert_eq!(
            preview
                .iter()
                .find(|item| item.id == "unit")
                .unwrap()
                .reason,
            "Disabled for this plan"
        );
    }
}
