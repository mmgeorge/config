use super::{CheckRun, CommandCheck};
use crate::checkpoint::CheckpointRecord;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CheckEvidence {
    pub checkpoint: CheckpointRecord,
    pub signature: String,
    pub source_identity: String,
}

struct PreparedCheck<'run> {
    run: &'run CheckRun,
    command: &'run CommandCheck,
    context: &'run super::model::CheckRunContext,
    selected: &'run super::model::CheckCommand,
    definition: &'run super::CheckDefinition,
}

impl<'run> PreparedCheck<'run> {
    fn new(run: &'run CheckRun, command: &'run CommandCheck) -> Result<Self> {
        let context = run
            .context
            .as_ref()
            .context("legacy check run has no execution context")?;
        let selected = command
            .context
            .as_ref()
            .context("legacy command has no execution context")?;
        let definition = context
            .snapshot
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == selected.definition_id)
            .context("check evidence has no captured definition")?;
        Ok(Self {
            run,
            command,
            context,
            selected,
            definition,
        })
    }

    /// Preserve command reuse while adapters await a complete dependency identity.
    fn reusable(&self) -> bool {
        match &self.definition.runner {
            super::CheckRunner::Command(_) => true,
            // Script argv does not identify tools launched from inside the script.
            super::CheckRunner::Adapter(
                super::adapter::AdapterConfig::Script(_)
                | super::adapter::AdapterConfig::RustNextest(_),
            ) => false,
        }
    }

    fn signature(&self) -> Result<String> {
        Ok(crate::plan::digest(&serde_json::to_vec(&(
            self.definition,
            &self.context.snapshot.helper_digest,
            &self.command.command,
            self.selected.purpose,
            &self.selected.directory,
            &self.selected.selection,
            self.selected
                .invocation
                .as_ref()
                .map(|invocation| &invocation.inputs),
            self.run.shell,
            &self.run.environment_digest,
        ))?))
    }
}

impl CheckEvidence {
    pub(super) fn capture(
        objects: &crate::storage::objects::ObjectStore,
        run: &CheckRun,
        command: &CommandCheck,
        before: CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<Option<Self>> {
        let prepared = PreparedCheck::new(run, command)?;
        let preparation = prepared.selected.purpose == super::CheckPurpose::Prepare;
        let checkpoint = if preparation { after.clone() } else { before };
        let source_identity = checkpoint.check_identity(objects, None, &[])?;
        Ok(Some(Self {
            checkpoint,
            signature: prepared.signature()?,
            source_identity,
        }))
    }

    pub(super) fn covers(
        &self,
        run: &CheckRun,
        current: &CommandCheck,
        checkpoint: &CheckpointRecord,
    ) -> Result<bool> {
        let prepared = PreparedCheck::new(run, current)?;
        Ok(prepared.reusable() && self.matches_inputs(run, current, checkpoint)?)
    }

    /// Match the captured inputs for cancellation suppression, independently of result reuse.
    pub(super) fn matches_inputs(
        &self,
        run: &CheckRun,
        current: &CommandCheck,
        checkpoint: &CheckpointRecord,
    ) -> Result<bool> {
        Ok(self.signature == signature(run, current)?
            && self.checkpoint.changed_paths(checkpoint)?.is_empty())
    }
}

pub(super) fn signature(run: &CheckRun, command: &CommandCheck) -> Result<String> {
    PreparedCheck::new(run, command)?.signature()
}
