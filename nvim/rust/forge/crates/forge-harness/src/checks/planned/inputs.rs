//! Freeze planned ownership decisions independently of later configuration changes.

use crate::checks::discovery::DiscoveryInputs;
use crate::checks::model::CheckRunContext;
use crate::checks::verification::ResolutionRequirements;
use crate::checks::{CheckCategory, CheckSupervisor};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Bind source, configuration, task and reviewer choices, and run ownership.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "RawInputs")]
pub(in crate::checks) struct ResolutionInputs {
    source_snapshot: String,
    discovery: DiscoveryInputs,
    context: CheckRunContext,
    test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    verification: ResolutionRequirements,
}

impl ResolutionInputs {
    /// Capture one source-bound input before requesting any exact inventory.
    pub(super) fn capture(
        supervisor: &CheckSupervisor,
        run: &crate::checks::PreparedCheckRun,
        discovery: DiscoveryInputs,
        source: &crate::checkpoint::CheckpointRecord,
        test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    ) -> Result<Self> {
        let verification = ResolutionRequirements::capture(&supervisor.session_id, source)?;
        RawInputs {
            source_snapshot: source.id.clone(),
            discovery,
            context: run.context.clone(),
            test_scopes,
            verification,
        }
        .try_into()
    }

    /// Get the captured configuration that resolves planned test targets.
    pub(super) fn config(&self) -> &crate::checks::CheckConfig {
        &self.context.snapshot.config
    }

    /// Get the exact enabled test checks, independent of triggers and automatic selection.
    pub(super) fn participants(&self) -> Vec<&str> {
        self.context
            .snapshot
            .config
            .definitions
            .iter()
            .filter(|definition| {
                definition.category == CheckCategory::Test
                    && self
                        .context
                        .overrides
                        .get(&definition.id)
                        .copied()
                        .unwrap_or(definition.enabled)
            })
            .map(|definition| definition.id.as_str())
            .collect()
    }

    /// Get the retained source owner for this run.
    pub(in crate::checks) fn verification(&self) -> &ResolutionRequirements {
        &self.verification
    }

    /// Reject changed run inputs before accepting more evidence.
    pub(super) fn validate_live(
        &self,
        supervisor: &CheckSupervisor,
        context: &CheckRunContext,
    ) -> Result<()> {
        self.validate_context(context, &self.test_scopes)?;
        self.discovery.validate(supervisor)
    }

    /// Require the enclosing run and planned choices to describe this captured input.
    pub(super) fn validate_context(
        &self,
        context: &CheckRunContext,
        scopes: &BTreeMap<String, crate::plan::PlanTestScope>,
    ) -> Result<()> {
        ensure!(
            &self.context == context && &self.test_scopes == scopes,
            "planned resolution input differs from its run context or reviewer choices"
        );
        Ok(())
    }

    /// Require exact inventories to share this input's captured source identity.
    pub(super) fn validate_snapshot(&self, snapshot: &str) -> Result<()> {
        ensure!(
            self.source_snapshot == snapshot,
            "planned resolution source differs from its captured input"
        );
        Ok(())
    }
}

/// Decode all required input fields before establishing their cross-field invariants.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawInputs {
    source_snapshot: String,
    discovery: DiscoveryInputs,
    context: CheckRunContext,
    test_scopes: BTreeMap<String, crate::plan::PlanTestScope>,
    verification: ResolutionRequirements,
}

impl TryFrom<RawInputs> for ResolutionInputs {
    type Error = anyhow::Error;

    /// Reject snapshots and reviewer overrides that cannot describe one admitted configuration.
    fn try_from(raw: RawInputs) -> Result<Self> {
        let RawInputs {
            source_snapshot,
            discovery,
            context,
            test_scopes,
            verification,
        } = raw;
        ensure!(
            !source_snapshot.is_empty(),
            "planned resolution source identity is empty"
        );
        discovery.snapshot().config.validate()?;
        ensure!(
            discovery.snapshot().digest
                == crate::plan::digest(&serde_json::to_vec(&(
                    &discovery.snapshot().config,
                    &discovery.snapshot().helper_digest
                ))?),
            "planned discovery configuration digest is inconsistent"
        );
        discovery.validate_context(&context)?;
        let mut enabled = context.task_overrides.enabled.clone();
        for (check, selected) in &context.overrides {
            if !selected {
                enabled.insert(check.clone(), false);
            }
        }
        ensure!(
            enabled == context.overrides,
            "reviewer choices cannot enable a check outside the captured task settings"
        );
        for id in test_scopes.keys() {
            ensure!(
                context
                    .snapshot
                    .config
                    .definitions
                    .iter()
                    .any(|definition| &definition.id == id
                        && definition.category == CheckCategory::Test),
                "planned scope does not name a captured test check"
            );
        }
        Ok(Self {
            source_snapshot,
            discovery,
            context,
            test_scopes,
            verification,
        })
    }
}
