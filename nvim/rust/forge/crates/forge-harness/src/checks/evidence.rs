use super::{CheckRun, CheckSelection, CommandCheck};
use crate::checkpoint::CheckpointRecord;
use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains the exact source boundary and execution contract for a completed attempt.
pub struct CheckEvidence {
    /// Captured inputs before and after the command, verified to be stable.
    pub checkpoint: CheckpointRecord,
    /// Command, definition, helper, interpreter, and environment identity.
    pub signature: String,
    /// Content identity restricted to this command's input boundary.
    pub source_identity: String,
}

impl CheckEvidence {
    /// Captures an attempt only when its relevant inputs stayed unchanged.
    pub(super) fn capture(
        run: &CheckRun,
        command: &CommandCheck,
        before: CheckpointRecord,
        after: &CheckpointRecord,
    ) -> Result<Option<Self>> {
        if !unchanged(command, &before, after)? {
            return Ok(None);
        }
        let invocation = command
            .context
            .as_ref()
            .and_then(|context| context.invocation.as_ref());
        let directory = invocation
            .and_then(|invocation| invocation.package.as_ref())
            .map(|package| package.directory.as_str());
        let extra = invocation
            .map(|invocation| invocation.inputs.keys().cloned().collect::<Vec<_>>())
            .unwrap_or_default();
        let source_identity = before.check_identity(directory, &extra)?;
        Ok(Some(Self {
            checkpoint: before,
            signature: signature(run, command)?,
            source_identity,
        }))
    }

    /// Accepts only evidence covering the requested selection and current inputs.
    pub(super) fn covers(
        &self,
        previous: &CommandCheck,
        run: &CheckRun,
        current: &CommandCheck,
        checkpoint: &CheckpointRecord,
    ) -> Result<bool> {
        let Some(previous_context) = &previous.context else {
            return Ok(false);
        };
        let Some(current_context) = &current.context else {
            return Ok(false);
        };
        let package = current_context
            .invocation
            .as_ref()
            .and_then(|invocation| invocation.package.as_ref());
        let coverage = package.is_some()
            || match (&previous_context.selection, &current_context.selection) {
                (CheckSelection::All, _) => true,
                (
                    CheckSelection::Files { files: before },
                    CheckSelection::Files { files: after },
                ) => after.iter().all(|file| before.contains(file)),
                (
                    CheckSelection::Cases { cases: before },
                    CheckSelection::Cases { cases: after },
                ) => after.iter().all(|case| before.contains(case)),
                _ => false,
            };
        Ok(coverage
            && self.signature == signature(run, current)?
            && unchanged(current, &self.checkpoint, checkpoint)?)
    }
}

fn signature(run: &CheckRun, command: &CommandCheck) -> Result<String> {
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
        run.shell,
        &run.environment_digest,
    ))?))
}

fn unchanged(
    command: &CommandCheck,
    before: &CheckpointRecord,
    after: &CheckpointRecord,
) -> Result<bool> {
    let changes = before.changed_paths(after)?;
    let Some(package) = command
        .context
        .as_ref()
        .and_then(|context| context.invocation.as_ref())
        .and_then(|invocation| invocation.package.as_ref())
    else {
        return Ok(changes.is_empty());
    };
    let inputs = &command
        .context
        .as_ref()
        .unwrap()
        .invocation
        .as_ref()
        .unwrap()
        .inputs;
    Ok(!changes.iter().any(|path| {
        package.directory == "."
            || path == &package.directory
            || path.starts_with(&format!("{}/", package.directory))
            || inputs.contains_key(path)
    }))
}
