//! Reconcile execution-owned attempts with independently persisted check progress.

use super::{CheckRun, CheckState};
use anyhow::{Context, Result, ensure};
use std::path::{Component, Path};

/// Persist recovered attempts before the caller commits its recovered execution transaction.
///
/// A failed transaction can leave these same interrupted records on disk. Repeating recovery
/// consumes them without restoring execution authority or discarding collected results.
pub(crate) fn recover_runs(
    root: &Path,
    session_id: &str,
    admitted: &[CheckRun],
) -> Result<Vec<CheckRun>> {
    let mut recovered = Vec::with_capacity(admitted.len());
    for run in admitted {
        // Validate progress against the execution's admitted identity before selecting it.
        ensure!(
            run.owner.session_id() == session_id,
            "execution check belongs to another session"
        );
        let mut components = Path::new(&run.id).components();
        ensure!(
            matches!(components.next(), Some(Component::Normal(_))) && components.next().is_none(),
            "execution check identity is not a directory name"
        );
        let path = root.join(&run.id).join("record.json");
        let observed = match std::fs::symlink_metadata(&path) {
            Ok(_) => Some(
                super::supervisor::saved_record(&path, &run.id)?
                    .context("execution check history has an incompatible version")?,
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error).context("inspect execution check history"),
        };
        if let Some(observed) = &observed {
            validate_attempt(run, observed)?;
        }
        let mut result = if run.state == CheckState::Running {
            observed.as_ref().unwrap_or(run).clone()
        } else {
            // A committed execution outcome outranks progress whose publication lagged it.
            run.clone()
        };
        // Retire execution authority while preserving individual case observations.
        if run.state == CheckState::Running {
            result.interrupt();
            for command in &mut result.commands {
                if command.state == CheckState::Running {
                    command.state = CheckState::Interrupted;
                    command.completed_at_ms = result.completed_at_ms;
                    match std::fs::metadata(&command.output) {
                        Ok(metadata) => {
                            command.output_bytes = command.output_bytes.max(metadata.len())
                        }
                        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                        Err(error) => {
                            return Err(error).context("inspect interrupted check output");
                        }
                    }
                }
            }
        }
        // Make matching history durable before the execution transaction can commit.
        if observed.as_ref().map(crate::codec::to_value).transpose()?
            != Some(crate::codec::to_value(&result)?)
        {
            super::supervisor::write_record(&path, &result)
                .with_context(|| format!("persist recovered check {}", run.id))?;
        }
        recovered.push(result);
    }
    Ok(recovered)
}

/// Reject progress belonging to a different admission even when its file reused the run ID.
fn validate_attempt(admitted: &CheckRun, observed: &CheckRun) -> Result<()> {
    ensure!(
        admitted.id == observed.id
            && admitted.owner == observed.owner
            && admitted.revision == observed.revision
            && admitted.started_at_ms == observed.started_at_ms
            && admitted.workspace == observed.workspace
            && admitted.shell == observed.shell,
        "check history does not match its admitted execution attempt"
    );
    let same_context = match (&admitted.context, &observed.context) {
        (Some(admitted), Some(observed)) => {
            admitted.task_id == observed.task_id
                && admitted.snapshot == observed.snapshot
                && admitted.overrides == observed.overrides
                && admitted.task_overrides == observed.task_overrides
        }
        (None, None) => true,
        (Some(_), None) | (None, Some(_)) => false,
    };
    ensure!(
        same_context,
        "check history changed admitted configuration or task settings"
    );
    ensure!(
        match (&admitted.planned_tests, &observed.planned_tests) {
            (Some(admitted), Some(observed)) => admitted.same_request(observed),
            (None, None) => true,
            (Some(_), None) | (None, Some(_)) => false,
        },
        "check history changed the admitted planned-test request"
    );
    Ok(())
}
