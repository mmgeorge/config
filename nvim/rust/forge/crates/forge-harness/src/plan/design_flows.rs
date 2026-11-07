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
        for text in std::iter::once(format!("### {}", label_text(&flow.title)))
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
        let mut diagram = vec![format!("On {}", label_text(&flow.trigger))];
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
            titles.insert(label_text(&flow.title)),
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
        !label.trim().is_empty(),
        "plan flow labels must be nonempty"
    );
    Ok(())
}

fn label_text(label: &str) -> String {
    label.split_whitespace().collect::<Vec<_>>().join(" ")
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
                let mut line = format!("{prefix}{marker}{}", label_text(&step.action));
                if let [effect] = step.effects.as_slice() {
                    line.push_str(&format!(
                        " ┄▶ {} {}",
                        label_text(&effect.change),
                        label_text(&effect.target)
                    ));
                }
                rows.push(line);
                if step.effects.len() != 1 {
                    for effect in &step.effects {
                        rows.push(format!(
                            "{prefix}  ┄▶ {} {}",
                            label_text(&effect.change),
                            label_text(&effect.target)
                        ));
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
                    label_text(&branch.condition)
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
    fn validation_requires_triggers_conditions_and_actions() {
        let mut invalid = flow();
        invalid.trigger.clear();
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps.clear();
        assert!(validate(&[invalid]).is_err());
        let mut invalid = flow();
        invalid.steps[0].action = " \n\t ".into();
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
}
