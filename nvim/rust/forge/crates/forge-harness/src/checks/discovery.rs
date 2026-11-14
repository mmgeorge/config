//! Execute owner-admitted discovery against a validated workspace snapshot.

use super::{
    CheckCategory, CheckRunner, CheckSupervisor, ConfigSnapshot,
    adapter::{
        AdapterContext, AdapterProcesses, CategoryAdapter, CheckDiscoverRequest, DiscoverRequest,
        Discovery,
    },
};
use anyhow::{Context, Result, ensure};
use tokio::sync::watch;

/// Freeze the configuration and task settings used to admit one discovery operation.
#[derive(
    Clone, Debug, serde::Deserialize, Eq, schemars::JsonSchema, PartialEq, serde::Serialize,
)]
pub(crate) struct DiscoveryInputs {
    snapshot: ConfigSnapshot,
    task_id: Option<String>,
    overrides: super::CheckOverrides,
}

impl DiscoveryInputs {
    /// Capture task enablement independently of trigger matching and reviewer exclusions.
    pub(crate) fn capture(runner: &CheckSupervisor, task_id: Option<&str>) -> Result<Self> {
        Ok(Self {
            snapshot: ConfigSnapshot::load(&runner.workspace)?,
            task_id: task_id.map(str::to_owned),
            overrides: runner.overrides(task_id)?,
        })
    }

    /// Get the configuration frozen for this operation.
    pub(super) fn snapshot(&self) -> &ConfigSnapshot {
        &self.snapshot
    }

    /// Get all task preferences frozen with the configuration.
    pub(super) fn overrides(&self) -> &super::CheckOverrides {
        &self.overrides
    }

    /// Require a prepared run to match the task settings captured during preparation.
    pub(super) fn for_run(runner: &CheckSupervisor, run: &super::PreparedCheckRun) -> Result<Self> {
        let inputs = Self::capture(runner, run.context.task_id.as_deref())?;
        inputs.validate_context(&run.context)?;
        Ok(inputs)
    }

    /// Require a retained run to describe this configuration and task-preference namespace.
    pub(super) fn validate_context(&self, context: &super::model::CheckRunContext) -> Result<()> {
        let mut expected = self.snapshot.clone();
        for definition in &mut expected.config.definitions {
            if let Some(auto) = self.overrides.auto.get(&definition.id) {
                definition.auto = *auto;
            }
            if let Some(scope) = self.overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        ensure!(
            expected == context.snapshot
                && self.overrides == context.task_overrides
                && self.task_id == context.task_id,
            "check configuration, helpers, or task settings changed since preparation"
        );
        Ok(())
    }

    /// Reject admission or results after any captured configuration or task setting changes.
    pub(super) fn validate(&self, runner: &CheckSupervisor) -> Result<()> {
        ensure!(
            self.snapshot == ConfigSnapshot::load(&runner.workspace)?
                && self.overrides == runner.overrides(self.task_id.as_deref())?,
            "check configuration, helpers, or task settings changed during discovery; discover again"
        );
        Ok(())
    }

    /// Get whether the captured task enables this configured definition.
    fn enabled(&self, definition: &super::CheckDefinition) -> bool {
        self.overrides
            .enabled
            .get(&definition.id)
            .copied()
            .unwrap_or(definition.enabled)
    }
}

/// Collect discovery under one deadline and preserve process collection on cancellation.
pub(super) async fn discover(
    runner: &CheckSupervisor,
    inputs: DiscoveryInputs,
    request: CheckDiscoverRequest,
    deadline: tokio::time::Instant,
    mut cancelled: watch::Receiver<bool>,
    process_owner: Option<std::sync::Arc<dyn forge_shell::process::ProcessOwner>>,
) -> Result<Discovery> {
    let (stop, stopped) = watch::channel(false);
    let scratch = tempfile::tempdir_in(&runner.root)?;
    let processes =
        AdapterProcesses::new(stopped, deadline, scratch.path().join("discovery.output"))
            .owner_bind(process_owner);
    let operation = collect(runner, &inputs, request, &processes, scratch.path());
    tokio::pin!(operation);
    tokio::select! {
        biased;
        _ = async { let _ = cancelled.wait_for(|value| *value).await; } => {
            let _ = stop.send(true);
            let _ = operation.await;
            anyhow::bail!("test discovery cancelled");
        }
        _ = tokio::time::sleep_until(deadline) => {
            let _ = stop.send(true);
            let _ = operation.await;
            anyhow::bail!("test discovery exceeded its tool timeout");
        }
        result = &mut operation => result,
    }
}

/// Reject stale inputs before exposing a discovered tree to its caller.
async fn collect(
    runner: &CheckSupervisor,
    inputs: &DiscoveryInputs,
    request: CheckDiscoverRequest,
    processes: &AdapterProcesses,
    scratch: &std::path::Path,
) -> Result<Discovery> {
    inputs.validate(runner)?;
    let definition = inputs
        .snapshot
        .config
        .definitions
        .iter()
        .find(|definition| definition.id == request.check)
        .context("unknown discovery check")?;
    ensure!(inputs.enabled(definition), "discovery check is disabled");
    ensure!(
        definition.category == CheckCategory::Test,
        "discovery requires a test category"
    );
    let adapter = match &definition.runner {
        CheckRunner::Adapter(options) => match options.build(CheckCategory::Test)? {
            CategoryAdapter::Test(adapter) => adapter,
            CategoryAdapter::Diagnostics(_) | CategoryAdapter::Format(_) => {
                anyhow::bail!("discovery requires a test adapter")
            }
        },
        CheckRunner::Command(_) => {
            anyhow::bail!("discovery requires a test adapter, not a plain command")
        }
    };
    let workspace = runner.workspace.canonicalize()?;
    let directory = workspace
        .join(super::config::relative_path(&definition.directory)?)
        .canonicalize()?;
    ensure!(
        directory.starts_with(&workspace),
        "check directory escapes the project"
    );
    let before = runner.changes.capture(&runner.session_id).await?;
    inputs.validate(runner)?;
    let context = AdapterContext {
        workspace: &workspace,
        directory: &directory,
        processes,
        scratch,
    };
    let result = adapter
        .discover(
            &context,
            &DiscoverRequest {
                within: request.within,
                files: request.files,
                mode: request.mode,
            },
        )
        .await?;
    let after = runner.changes.capture(&runner.session_id).await?;
    inputs.validate(runner)?;
    ensure!(
        before.id == after.id,
        "check configuration, helpers, or workspace changed during discovery; discover again"
    );
    Ok(result)
}
