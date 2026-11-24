//! The owner and source recorded with planned check inputs.
//!
//! Preserve the existing record shape when reopening a plan. Historical mutation
//! entries describe the old run and do not participate in check selection.

use super::{CheckDefinition, CheckSelection};
use crate::checkpoint::CheckpointRecord;
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

const VERSION: u32 = crate::storage::state::VERSION;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct RequirementsRecord {
    version: u32,
    session_id: String,
    workspace: PathBuf,
    generation: u64,
    requirements: BTreeMap<String, Requirement>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct Requirement {
    generation: u64,
    task_id: Option<String>,
    definition: CheckDefinition,
    helper_digest: BTreeMap<String, String>,
    requested: CheckSelection,
    state: RequirementState,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case", deny_unknown_fields)]
enum RequirementState {
    Prepared,
    Pending,
    Satisfied {
        command: String,
        source_identity: String,
    },
}

/// Freeze the ledger and source identity from which planned verification obligations were derived.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawResolutionRequirements")]
pub(super) struct ResolutionRequirements {
    record: RequirementsRecord,
    source_identity: String,
}

impl ResolutionRequirements {
    /// Record the source owner without introducing follow-up verification work.
    pub(super) fn capture(session_id: &str, snapshot: &CheckpointRecord) -> Result<Self> {
        Ok(Self {
            record: RequirementsRecord {
                version: VERSION,
                session_id: session_id.to_owned(),
                workspace: Path::new(&snapshot.workspace).canonicalize()?,
                generation: 0,
                requirements: BTreeMap::new(),
            },
            source_identity: snapshot.id.clone(),
        })
    }

    /// Require retained obligations to belong to the run's session and workspace.
    pub(super) fn validate_owner(
        &self,
        owner: &crate::timeline::TimelineOwner,
        workspace: &str,
    ) -> Result<()> {
        ensure!(
            self.record.session_id == owner.session_id()
                && dunce::simplified(&self.record.workspace)
                    == dunce::simplified(Path::new(workspace)),
            "planned verification inputs belong to another session or workspace"
        );
        Ok(())
    }
}

/// Validate restored ledger snapshots independently of live filesystem access.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawResolutionRequirements {
    record: RequirementsRecord,
    source_identity: String,
}

impl TryFrom<RawResolutionRequirements> for ResolutionRequirements {
    type Error = anyhow::Error;

    /// Reject malformed obligations before they can alter planned execution selection.
    fn try_from(raw: RawResolutionRequirements) -> Result<Self> {
        let RawResolutionRequirements {
            record,
            source_identity,
        } = raw;
        record.validate()?;
        ensure!(
            !source_identity.is_empty(),
            "planned verification source identity is empty"
        );
        Ok(Self {
            record,
            source_identity,
        })
    }
}

impl RequirementsRecord {
    /// Validate the ledger's durable identities and supported mutation obligations.
    fn validate(&self) -> Result<()> {
        ensure!(
            self.version == VERSION,
            "unsupported verification ledger version {}",
            self.version
        );
        ensure!(
            !self.session_id.is_empty() && self.workspace.is_absolute(),
            "verification ledger owner is invalid"
        );
        for (command, requirement) in &self.requirements {
            ensure!(
                !command.is_empty() && !requirement.definition.id.is_empty(),
                "verification requirement identity is empty"
            );
            ensure!(
                requirement.generation > 0 && requirement.generation <= self.generation,
                "invalid verification requirement generation"
            );
            ensure!(
                matches!(
                    requirement.definition.category,
                    super::CheckCategory::Format | super::CheckCategory::Lint
                ),
                "verification requirement is not a fixable category"
            );
            let mut selection = requirement.requested.clone();
            selection.normalize()?;
            if let RequirementState::Satisfied {
                command,
                source_identity,
            } = &requirement.state
            {
                ensure!(
                    !command.is_empty() && !source_identity.is_empty(),
                    "verification satisfaction identity is empty"
                );
            }
        }
        Ok(())
    }
}
