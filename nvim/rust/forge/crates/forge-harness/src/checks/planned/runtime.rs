//! Resolve plan coverage under the admitted run's existing process and repository ownership.

use super::PlannedInventory;
use crate::checks::adapter::{CheckDiscoverRequest, DiscoverMode, Discovery};
use crate::checks::discovery::DiscoveryInputs;
use crate::checks::supervisor::{CheckCancellation, CheckDeadline};
use crate::checks::{
    CheckCategory, CheckSelection, CheckState, CheckSupervisor, ConfigSnapshot, PreparedCheckRun,
};
use anyhow::{Context, Result, ensure};

/// Distinguish cancellation, which already collected owned processes, from a discovery result.
enum OwnedDiscovery {
    Finished(Result<Discovery, crate::checks::owner::CheckOwnerFailure>),
    Cancelled,
}

/// Discover every participating check before choosing any per-check execution selection.
pub(in crate::checks) async fn resolve_run(
    supervisor: &CheckSupervisor,
    run: &mut PreparedCheckRun,
    deadline: CheckDeadline,
    inputs: &DiscoveryInputs,
    cancelled: &CheckCancellation,
    external: Option<&tokio::sync::watch::Receiver<bool>>,
    process_owner: &std::sync::Arc<dyn forge_shell::process::ProcessOwner>,
) -> Result<()> {
    let expected = run.context.snapshot.clone();
    let current = ConfigSnapshot::load(&supervisor.workspace)?;
    ensure!(
        expected.digest == current.digest && expected.helper_digest == current.helper_digest,
        "check configuration or helpers changed before planned test resolution"
    );
    let before = supervisor.changes.capture(&supervisor.session_id).await?;
    validate_inputs(supervisor, run, inputs)?;
    let scopes = run
        .planned_tests
        .as_ref()
        .context("planned inventory missing")?
        .test_scopes()
        .clone();
    let captured =
        super::inputs::ResolutionInputs::capture(supervisor, run, inputs.clone(), &before, scopes)?;
    let participants: Vec<String> = captured
        .participants()
        .into_iter()
        .map(str::to_owned)
        .collect();
    run.planned_tests
        .as_mut()
        .context("planned inventory missing")?
        .bind_inputs(captured)?;
    let mut inventories = Vec::new();

    // Discover inside the run reservation; acquiring a separate discovery reservation would deadlock.
    if run
        .planned_tests
        .as_ref()
        .context("planned inventory missing")?
        .needs_discovery()
    {
        for check in participants {
            deadline.check()?;
            let request = |mode| CheckDiscoverRequest {
                check: check.clone(),
                files: Vec::new(),
                within: Vec::new(),
                mode,
            };
            let OwnedDiscovery::Finished(result) = discover_owned(
                supervisor,
                inputs,
                request(DiscoverMode::Exact),
                deadline,
                cancelled,
                external,
                process_owner,
            )
            .await
            else {
                run.interrupt();
                return Ok(());
            };
            let mut inventory = PlannedInventory::new(check.clone(), result)?;
            if inventory.needs_locations() {
                deadline.check()?;
                let OwnedDiscovery::Finished(result) = discover_owned(
                    supervisor,
                    inputs,
                    request(DiscoverMode::Static),
                    deadline,
                    cancelled,
                    external,
                    process_owner,
                )
                .await
                else {
                    run.interrupt();
                    return Ok(());
                };
                inventory.supplement(result);
            }
            inventories.push(inventory);
        }
    }

    // Reject mixed inventories before any selected case can execute.
    let after = supervisor.changes.capture(&supervisor.session_id).await?;
    let current = ConfigSnapshot::load(&supervisor.workspace)?;
    validate_inputs(supervisor, run, inputs)?;
    let planned = run
        .planned_tests
        .as_mut()
        .context("planned inventory missing")?;
    planned.resolve(before.id.clone(), &inventories);
    deadline.check()?;
    if before.id != after.id
        || expected.digest != current.digest
        || expected.helper_digest != current.helper_digest
    {
        planned.invalidate("Workspace, check configuration, or helpers changed during planned test resolution; run Check again".into());
        return Ok(());
    }
    let selections = planned.selections();
    for command in &mut run.commands {
        if command.context.category != CheckCategory::Test {
            continue;
        }
        let definition = expected
            .config
            .definitions
            .iter()
            .find(|definition| definition.id == command.context.definition_id)
            .context("planned check has no captured definition")?;
        let enabled = run
            .context
            .overrides
            .get(&definition.id)
            .copied()
            .unwrap_or(definition.enabled);
        if !enabled {
            command.selected = false;
            command.state = CheckState::Skipped;
            command.error = Some("Disabled for this plan".into());
            continue;
        }
        if planned.scope(&definition.id) == crate::plan::PlanTestScope::WholeSuite {
            command.selected = true;
            command.state = CheckState::Pending;
            command.error = None;
            command.context.selection = CheckSelection::All;
            command.context.resolved = None;
            command.context.reason = "Reviewer requested the whole test suite".into();
            continue;
        }
        if let Some(selection) = selections.get(&command.context.definition_id) {
            command.selected = true;
            command.state = CheckState::Pending;
            command.error = None;
            command.context.selection = CheckSelection::Tests {
                tests: selection.clone(),
            };
            command.context.resolved = None;
            command.context.reason = "Required planned tests".into();
        } else {
            command.selected = false;
            command.state = CheckState::Skipped;
            command.error = Some("The plan lists no tests to run for this check".into());
            command.context.reason = "The plan lists no tests to run for this check".into();
        }
    }
    Ok(())
}

/// Collect each discovery under the parent run's existing ownership and absolute deadline.
async fn discover_owned(
    supervisor: &CheckSupervisor,
    inputs: &DiscoveryInputs,
    request: CheckDiscoverRequest,
    deadline: CheckDeadline,
    cancelled: &CheckCancellation,
    external: Option<&tokio::sync::watch::Receiver<bool>>,
    process_owner: &std::sync::Arc<dyn forge_shell::process::ProcessOwner>,
) -> OwnedDiscovery {
    let (stop, stopped) = tokio::sync::watch::channel(false);
    let operation = crate::checks::discovery::discover(
        supervisor,
        inputs.clone(),
        request,
        deadline.instant(),
        stopped,
        Some(process_owner.clone()),
    );
    tokio::pin!(operation);
    tokio::select! {
        biased;
        _ = cancelled.wait(external.cloned()) => {
            stop.send_replace(true);
            let _ = operation.await;
            OwnedDiscovery::Cancelled
        }
        result = &mut operation => OwnedDiscovery::Finished(result.map_err(crate::checks::owner::CheckOwnerFailure::Worker)),
    }
}

/// Invalidate per-case evidence when execution observes a different source or configuration.
pub(in crate::checks) async fn finish_run(
    supervisor: &CheckSupervisor,
    run: &mut PreparedCheckRun,
    inputs: &DiscoveryInputs,
) -> Result<()> {
    let after = supervisor.changes.capture(&supervisor.session_id).await?;
    validate_inputs(supervisor, run, inputs)?;
    let current = ConfigSnapshot::load(&supervisor.workspace)?;
    let planned = run
        .planned_tests
        .as_mut()
        .context("planned inventory missing")?;
    planned.record_results(&run.commands);
    if !planned.matches_snapshot(&after.id)
        || current.digest != run.context.snapshot.digest
        || current.helper_digest != run.context.snapshot.helper_digest
    {
        planned.invalidate("Workspace, check configuration, or helpers changed after planned test resolution; run Check again".into());
    }
    Ok(())
}

/// Invalidate the whole plan attempt when task settings or configuration change in flight.
pub(in crate::checks) fn validate_inputs(
    supervisor: &CheckSupervisor,
    run: &mut PreparedCheckRun,
    inputs: &DiscoveryInputs,
) -> Result<()> {
    let validation = inputs.validate(supervisor).and_then(|()| {
        if let Some(inputs) = run
            .planned_tests
            .as_ref()
            .and_then(super::PlannedTestExecution::inputs)
        {
            inputs.validate_live(supervisor, &run.context)?;
        }
        Ok(())
    });
    if let Err(error) = validation {
        if let Some(planned) = &mut run.planned_tests {
            planned.invalidate(format!("{error:#}"));
        }
        return Err(crate::checks::failure::CheckFailure::classified(
            CheckState::Failed,
            error,
        ));
    }
    Ok(())
}
