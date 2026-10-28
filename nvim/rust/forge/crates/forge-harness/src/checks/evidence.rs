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
}

impl<'run> PreparedCheck<'run> {
    fn new(run: &'run CheckRun, command: &'run CommandCheck) -> Result<Self> {
        Ok(Self {
            run,
            command,
            context: run
                .context
                .as_ref()
                .context("legacy check run has no execution context")?,
            selected: command
                .context
                .as_ref()
                .context("legacy command has no execution context")?,
        })
    }

    fn signature(&self) -> Result<String> {
        let definition = self
            .context
            .snapshot
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == self.selected.definition_id);
        Ok(crate::plan::digest(&serde_json::to_vec(&(
            definition,
            &self.context.snapshot.helper_digest,
            &self.command.command,
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
        run: &CheckRun,
        command: &CommandCheck,
        before: CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<Option<Self>> {
        let prepared = PreparedCheck::new(run, command)?;
        let preparation = prepared
            .context
            .snapshot
            .config
            .definitions
            .iter()
            .any(|definition| {
                prepared.selected.definition_id == definition.id
                    && definition.purpose == super::config::CheckPurpose::Prepare
            });
        if !preparation && !before.changed_paths(after)?.is_empty() {
            return Ok(None);
        }
        let checkpoint = if preparation { after.clone() } else { before };
        let source_identity = checkpoint.check_identity(None, &[])?;
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
        Ok(self.signature == signature(run, current)?
            && self.checkpoint.changed_paths(checkpoint)?.is_empty())
    }
}

pub(super) fn signature(run: &CheckRun, command: &CommandCheck) -> Result<String> {
    PreparedCheck::new(run, command)?.signature()
}
