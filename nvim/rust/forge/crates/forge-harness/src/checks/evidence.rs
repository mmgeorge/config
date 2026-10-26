use super::{CheckRun, CommandCheck};
use crate::checkpoint::CheckpointRecord;
use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CheckEvidence {
    pub checkpoint: CheckpointRecord,
    pub signature: String,
    pub source_identity: String,
}

impl CheckEvidence {
    pub(super) fn capture(
        run: &CheckRun,
        command: &CommandCheck,
        before: CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<Option<Self>> {
        let preparation = run.context.as_ref().is_some_and(|context| {
            context
                .snapshot
                .config
                .definitions
                .iter()
                .any(|definition| {
                    command
                        .context
                        .as_ref()
                        .is_some_and(|command| command.definition_id == definition.id)
                        && definition.purpose == super::config::CheckPurpose::Prepare
                })
        });
        if !preparation && !before.changed_paths(after)?.is_empty() {
            return Ok(None);
        }
        let checkpoint = if preparation { after.clone() } else { before };
        let source_identity = checkpoint.check_identity(None, &[])?;
        Ok(Some(Self {
            checkpoint,
            signature: signature(run, command)?,
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
    let context = run.context.as_ref().expect("configured check run");
    let selected = command.context.as_ref().expect("configured command");
    let definition = context
        .snapshot
        .config
        .definitions
        .iter()
        .find(|definition| definition.id == selected.definition_id);
    Ok(crate::plan::digest(&serde_json::to_vec(&(
        definition,
        &context.snapshot.helper_digest,
        &command.command,
        &selected.directory,
        &selected.selection,
        selected
            .invocation
            .as_ref()
            .map(|invocation| &invocation.inputs),
        run.shell,
        &run.environment_digest,
    ))?))
}
