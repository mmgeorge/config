use std::collections::HashSet;

use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Describes one independently triggered operation and its ordered execution.
pub struct DesignFlow {
    /// Names the operation independently of its implementation steps.
    pub title: String,
    /// States the purpose and ordering guarantees in concise prose.
    pub description: String,
    /// Names the event that starts this execution, independently of another flow's outputs.
    pub trigger: String,
    /// Runs these actions in order after the trigger.
    pub steps: Vec<DesignFlowStep>,
}

#[derive(Debug, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
/// Executes an action, records its effects, and conditionally executes branch steps.
pub struct DesignFlowStep {
    /// Names a function or an explicit operation.
    pub action: String,
    /// Records terminal changes to data or state.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub effects: Vec<DesignFlowEffect>,
    /// Executes each applicable conditional path before the next enclosing step.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub branches: Vec<DesignFlowBranch>,
}

impl<'de> Deserialize<'de> for DesignFlowStep {
    /// Loads saved steps without retaining their retired input lists.
    fn deserialize<Decoder>(decoder: Decoder) -> std::result::Result<Self, Decoder::Error>
    where
        Decoder: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct SavedStep {
            action: String,
            #[serde(default)]
            effects: Vec<DesignFlowEffect>,
            #[serde(default)]
            branches: Vec<DesignFlowBranch>,
            #[serde(default, rename = "reads")]
            _retired_inputs: Option<serde::de::IgnoredAny>,
        }

        let saved = SavedStep::deserialize(decoder)?;
        Ok(Self {
            action: saved.action,
            effects: saved.effects,
            branches: saved.branches,
        })
    }
}

impl Clone for DesignFlowStep {
    fn clone(&self) -> Self {
        let mut pending = vec![(false, self)];
        let mut finished: Vec<Self> = Vec::new();
        while let Some((visited, step)) = pending.pop() {
            if !visited {
                pending.push((true, step));
                for child in step.branches.iter().flat_map(|branch| &branch.steps).rev() {
                    pending.push((false, child));
                }
            } else {
                let count = step
                    .branches
                    .iter()
                    .map(|branch| branch.steps.len())
                    .sum::<usize>();
                let mut children = finished.split_off(finished.len() - count).into_iter();
                let branches = step
                    .branches
                    .iter()
                    .map(|branch| DesignFlowBranch {
                        condition: branch.condition.clone(),
                        steps: children.by_ref().take(branch.steps.len()).collect(),
                    })
                    .collect();
                finished.push(Self {
                    action: step.action.clone(),
                    effects: step.effects.clone(),
                    branches,
                });
            }
        }
        finished.pop().unwrap()
    }
}

impl PartialEq for DesignFlowStep {
    fn eq(&self, other: &Self) -> bool {
        let mut pending = vec![(self, other)];
        while let Some((left, right)) = pending.pop() {
            if left.action != right.action
                || left.effects != right.effects
                || left.branches.len() != right.branches.len()
            {
                return false;
            }
            for (left, right) in left.branches.iter().zip(&right.branches) {
                if left.condition != right.condition || left.steps.len() != right.steps.len() {
                    return false;
                }
                pending.extend(left.steps.iter().zip(&right.steps));
            }
        }
        true
    }
}

impl Eq for DesignFlowStep {}

impl Drop for DesignFlowStep {
    fn drop(&mut self) {
        let mut pending = std::mem::take(&mut self.branches);
        while let Some(mut branch) = pending.pop() {
            for mut step in branch.steps.drain(..) {
                pending.append(&mut step.branches);
            }
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Records an observable state change without an executable continuation.
pub struct DesignFlowEffect {
    /// Names the affected value, entity, or state.
    pub target: String,
    /// Describes the change, such as create, update, remove, or preserve.
    pub change: String,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Executes an ordered path only when its condition holds during the current flow.
pub struct DesignFlowBranch {
    /// States the predicate for entering this path, not a later independent event.
    pub condition: String,
    /// Runs these actions in order when the condition holds.
    pub steps: Vec<DesignFlowStep>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
/// Distinguishes native diagram rows from prose and export-only fence delimiters.
pub(super) enum FlowRowKind {
    Narrative,
    Diagram,
    Fence,
}

/// One flow row with its role in the canonical section text.
pub(super) struct FlowRow {
    pub text: String,
    pub kind: FlowRowKind,
}

/// Produces shared export and native-view rows without recursive rendering.
pub(super) fn rows(flows: &[DesignFlow]) -> Vec<FlowRow> {
    let mut output = Vec::new();
    for (index, flow) in flows.iter().enumerate() {
        if index > 0 {
            output.push(FlowRow {
                text: String::new(),
                kind: FlowRowKind::Narrative,
            });
        }
        for text in std::iter::once(format!("### {}", flow.title))
            .chain(std::iter::once(String::new()))
            .chain(flow.description.split('\n').map(str::to_owned))
            .chain(std::iter::once(String::new()))
        {
            output.push(FlowRow {
                text,
                kind: FlowRowKind::Narrative,
            });
        }
        output.push(FlowRow {
            text: "```text".into(),
            kind: FlowRowKind::Fence,
        });
        let mut diagram = vec![format!("On {}", flow.trigger)];
        append_steps(&flow.steps, &mut diagram);
        output.extend(diagram.into_iter().map(|text| FlowRow {
            text,
            kind: FlowRowKind::Diagram,
        }));
        output.push(FlowRow {
            text: "```".into(),
            kind: FlowRowKind::Fence,
        });
    }
    output
}

/// Validates roles and execution boundaries without limiting valid flow size or depth.
pub(super) fn validate(flows: &[DesignFlow]) -> Result<()> {
    let mut titles = HashSet::new();
    for flow in flows {
        validate_label(&flow.title)?;
        ensure!(
            titles.insert(&flow.title),
            "duplicate plan flow title: {}",
            flow.title
        );
        ensure!(
            !flow.description.trim().is_empty(),
            "plan flow requires a description"
        );
        validate_label(&flow.trigger)?;
        ensure!(
            !flow.steps.is_empty(),
            "plan flow requires at least one execution step"
        );
        let mut pending = flow.steps.iter().collect::<Vec<_>>();
        while let Some(step) = pending.pop() {
            validate_label(&step.action)?;
            for effect in &step.effects {
                validate_label(&effect.target)?;
                validate_label(&effect.change)?;
            }
            for branch in &step.branches {
                validate_label(&branch.condition)?;
                ensure!(
                    !branch.steps.is_empty(),
                    "plan flow branch requires at least one execution step"
                );
                pending.extend(&branch.steps);
            }
        }
    }
    Ok(())
}

fn validate_label(label: &str) -> Result<()> {
    ensure!(
        !label.trim().is_empty() && label.trim() == label && !label.chars().any(char::is_control),
        "plan flow labels must be nonempty, unpadded single lines"
    );
    ensure!(
        !label.contains('`'),
        "plan flow labels must use plain text without backticks"
    );
    Ok(())
}

/// Produces canonical Markdown shared by review, section reads, and revision diffs.
pub(super) fn text(flows: &[DesignFlow]) -> String {
    rows(flows)
        .into_iter()
        .map(|row| row.text)
        .collect::<Vec<_>>()
        .join("\n")
}

enum RenderItem<'flow> {
    Step(&'flow DesignFlowStep, String, &'static str),
    Branch(&'flow DesignFlowBranch, String, bool),
}

fn append_steps(steps: &[DesignFlowStep], rows: &mut Vec<String>) {
    let mut pending = steps
        .iter()
        .enumerate()
        .rev()
        .map(|(index, step)| {
            RenderItem::Step(step, "  ".into(), if index == 0 { "→ " } else { "↓ " })
        })
        .collect::<Vec<_>>();
    while let Some(item) = pending.pop() {
        match item {
            RenderItem::Step(step, prefix, marker) => {
                let mut line = format!("{prefix}{marker}{}", step.action);
                if let [effect] = step.effects.as_slice() {
                    line.push_str(&format!(" ┄▶ {} {}", effect.change, effect.target));
                }
                rows.push(line);
                if step.effects.len() != 1 {
                    for effect in &step.effects {
                        rows.push(format!("{prefix}  ┄▶ {} {}", effect.change, effect.target));
                    }
                }
                for (index, branch) in step.branches.iter().enumerate().rev() {
                    pending.push(RenderItem::Branch(
                        branch,
                        format!("{prefix}  "),
                        index + 1 == step.branches.len(),
                    ));
                }
            }
            RenderItem::Branch(branch, prefix, last) => {
                rows.push(format!(
                    "{prefix}{} if {}",
                    if last { "└─" } else { "├─" },
                    branch.condition
                ));
                let prefix = format!("{prefix}{}", if last { "   " } else { "│  " });
                for (index, step) in branch.steps.iter().enumerate().rev() {
                    pending.push(RenderItem::Step(
                        step,
                        prefix.clone(),
                        if index == 0 { "→ " } else { "↓ " },
                    ));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flow() -> DesignFlow {
        serde_json::from_value(serde_json::json!({
            "title":"Collect shields", "description":"Collection precedes contact processing.",
            "trigger":"pickup collected", "steps":[{
                "action":"collect_shield_pickups",
                "effects":[{"target":"PlayerAbilities", "change":"grant shield charges to"}]
            }]
        }))
        .unwrap()
    }

    #[test]
    fn effects_cannot_contain_execution_or_events() {
        for field in ["children", "steps", "branches", "action", "trigger"] {
            let mut value = serde_json::to_value(flow()).unwrap();
            value["steps"][0]["effects"][0][field] = serde_json::json!([]);
            assert!(
                serde_json::from_value::<DesignFlow>(value).is_err(),
                "accepted {field} on an effect"
            );
        }
        assert!(
            serde_json::from_value::<DesignFlow>(serde_json::json!({
                "title":"Old tree", "description":"Not supported", "root":{"text":"pickup"}
            }))
            .is_err()
        );
    }

    #[test]
    fn independent_triggers_and_ordered_steps_render_distinctly() {
        let mut collected = flow();
        collected.steps.push(DesignFlowStep {
            action: "update_hud".into(),
            effects: vec![],
            branches: vec![],
        });
        let mut contact = flow();
        contact.title = "Absorb contact".into();
        contact.trigger = "accepted enemy contact".into();
        contact.steps[0].action = "PlayerAbilities.take_hit".into();
        let rendered = text(&[collected, contact]);
        assert!(rendered.contains("On pickup collected\n  → collect_shield_pickups ┄▶ grant shield charges to PlayerAbilities\n  ↓ update_hud"));
        assert!(rendered.contains("On accepted enemy contact\n  → PlayerAbilities.take_hit"));
    }

    #[test]
    fn retired_inputs_are_absent_from_the_schema_and_saved_output() {
        let schema = serde_json::to_value(schemars::schema_for!(DesignFlowStep)).unwrap();
        let properties = schema["properties"].as_object().unwrap();
        assert_eq!(properties.len(), 3);
        for field in ["action", "effects", "branches"] {
            assert!(properties.contains_key(field), "missing {field}");
        }
        assert_eq!(schema["additionalProperties"], false);

        let mut saved = serde_json::to_value(flow()).unwrap();
        saved["steps"][0]["reads"] = serde_json::json!(["ShieldPickup"]);
        saved["steps"][0]["branches"] = serde_json::json!([{
            "condition": "shield is available",
            "steps": [{"action": "collect_shield", "reads": ["PlayerAbilities"]}]
        }]);
        let restored: DesignFlow = serde_json::from_value(saved).unwrap();
        validate(std::slice::from_ref(&restored)).unwrap();
        let serialized = serde_json::to_value(&restored).unwrap();
        assert!(serialized["steps"][0].get("reads").is_none());
        assert!(
            serialized["steps"][0]["branches"][0]["steps"][0]
                .get("reads")
                .is_none()
        );
        assert!(!text(std::slice::from_ref(&restored)).contains('←'));
        assert_eq!(
            restored,
            serde_json::from_value::<DesignFlow>(serialized.clone()).unwrap()
        );

        let mut invalid = serialized;
        invalid["steps"][0]["unexpected"] = serde_json::json!([]);
        assert!(serde_json::from_value::<DesignFlow>(invalid).is_err());
    }

    #[test]
    fn validation_requires_triggers_conditions_and_actions() {
        let mut invalid = flow();
        invalid.trigger.clear();
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps.clear();
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps[0].action = "bad\nlabel".into();
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps[0].branches.push(DesignFlowBranch {
            condition: String::new(),
            steps: vec![flow().steps.remove(0)],
        });
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps[0].branches.push(DesignFlowBranch {
            condition: "ready".into(),
            steps: vec![],
        });
        assert!(validate(&[invalid]).is_err());
    }

    #[test]
    fn complete_collections_and_nested_branches_have_no_content_cap() {
        let mut nested = flow().steps.remove(0);
        for _ in 0..40 {
            nested = DesignFlowStep {
                action: "inspect".into(),
                effects: vec![],
                branches: vec![DesignFlowBranch {
                    condition: "ready".into(),
                    steps: vec![nested],
                }],
            };
        }
        let mut deep = flow();
        deep.steps = vec![nested];
        let mut flows = (0..33)
            .map(|index| {
                let mut flow = flow();
                flow.title = format!("Flow {index}");
                flow.steps = vec![flow.steps[0].clone(); 513];
                flow
            })
            .collect::<Vec<_>>();
        flows.push(deep);
        validate(&flows).unwrap();
        let rendered = text(&flows);
        assert_eq!(
            rendered.matches("collect_shield_pickups").count(),
            33 * 513 + 1
        );
        assert_eq!(rendered.matches("if ready").count(), 40);
        let workspace = tempfile::tempdir().unwrap();
        let store =
            super::super::PlanFileStore::new(workspace.path().join("data"), workspace.path());
        let mut document =
            super::super::document::test_fixture("deep-flows", "Preserve complete flows");
        document.design = Some(super::super::DeclarationDesign::default());
        document.design.as_mut().unwrap().document.flows = flows;
        store
            .write_working_document("session", "deep-flows", &document)
            .unwrap();
        let reopened = store
            .read_working_document("session", "deep-flows")
            .unwrap();
        assert_eq!(reopened, document.clone());
        let projected = super::super::design_review::render(&reopened).unwrap();
        assert_eq!(projected.markdown.matches("if ready").count(), 40);
    }
}
