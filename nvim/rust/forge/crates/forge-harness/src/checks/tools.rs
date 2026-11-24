use super::{CheckConfig, CheckSupervisor, ConfigSnapshot, RunRequest};
use crate::provider::ScopeOwner;
use crate::{
    provider::{HarnessEventSink, PromptMode},
    session::{ApprovalMode, CheckShell},
};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::{
    collections::BTreeMap,
    path::PathBuf,
    sync::{
        Arc, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};

#[derive(Clone, Debug, Default, Deserialize, Eq, schemars::JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains user-selected check states independently of the project configuration.
pub(crate) struct CheckOverrides {
    #[serde(default)]
    pub enabled: BTreeMap<String, bool>,
    #[serde(default)]
    /// Task-local automatic selection preferences.
    pub auto: BTreeMap<String, bool>,
    #[serde(default)]
    pub scopes: BTreeMap<String, super::CheckScope>,
}

#[derive(Clone, Debug, Deserialize, schemars::JsonSchema)]
#[serde(deny_unknown_fields)]
/// Captures one helper change with its previously inspected content identity.
pub(crate) struct CheckHelper {
    pub path: String,
    pub source: String,
    pub expected_digest: Option<String>,
}

#[derive(Clone, Debug, Deserialize, schemars::JsonSchema)]
#[serde(deny_unknown_fields)]
/// Publishes only check settings and explicitly captured managed helper files.
pub(crate) struct ConfigureChecks {
    pub expected_revision: String,
    pub config: CheckConfig,
    #[serde(default)]
    pub helpers: Vec<CheckHelper>,
}

/// Restricts provider check tools to one authenticated root interaction and captured task.
#[derive(Clone)]
pub(crate) struct CheckToolScope {
    runner: Arc<CheckSupervisor>,
    data_root: PathBuf,
    provider: String,
    task_id: Option<String>,
    shell: CheckShell,
    mode: PromptMode,
    permission: ApprovalMode,
    permissions: Arc<crate::provider::approval::PermissionCoordinator>,
    scope: crate::provider::InvocationScope,
    editing: Arc<AtomicBool>,
    operations: CheckToolOperations,
    agent: Arc<crate::agent::AgentCoordinator>,
    cancelled: tokio::sync::watch::Receiver<bool>,
    authorization: HarnessEventSink,
    delivery: Arc<Mutex<Option<CheckDelivery>>>,
    task_authority: Option<crate::tools::TaskAuthority>,
}

#[derive(Clone)]
struct CheckDelivery {
    lifecycle: Option<crate::broker::publication::SessionUpdateSink>,
}

/// Binds check operations to the live root turn and its setup/write authority.
pub(crate) struct CheckAuthority {
    pub task_authority: Option<crate::tools::TaskAuthority>,
    pub data_root: PathBuf,
    pub provider: String,
    pub task_id: Option<String>,
    pub shell: CheckShell,
    pub mode: PromptMode,
    pub permission: ApprovalMode,
    pub permissions: Arc<crate::provider::approval::PermissionCoordinator>,
    pub scope: crate::provider::InvocationScope,
    pub authorization_sink: HarnessEventSink,
    pub editing: Arc<AtomicBool>,
    pub operations: CheckToolOperations,
    pub agent: Arc<crate::agent::AgentCoordinator>,
    pub cancelled: tokio::sync::watch::Receiver<bool>,
    pub sink: Option<crate::broker::publication::SessionUpdateSink>,
}

/// Decide whether a foreground model may start `check_run` itself.
///
/// Verify permits read-only checks, Resolve permits explicit fixes, and ordinary chat permits
/// both. Check setup and active delegated agents exclude model-owned check operations.
pub(crate) fn model_check_operations(
    check_setup_active: bool,
    active_agent_count: usize,
    execution_phase: Option<crate::plan::execution::PlanPhase>,
) -> CheckToolOperations {
    if check_setup_active || active_agent_count != 0 {
        return CheckToolOperations::Unavailable;
    }
    match execution_phase {
        None => CheckToolOperations::Both,
        Some(crate::plan::execution::PlanPhase::Verify) => CheckToolOperations::Verify,
        Some(crate::plan::execution::PlanPhase::Resolve) => CheckToolOperations::Apply,
        Some(
            crate::plan::execution::PlanPhase::Implement
            | crate::plan::execution::PlanPhase::Checking,
        ) => CheckToolOperations::Unavailable,
    }
}

/// Restrict verification and source-changing operations independently by execution phase.
#[derive(Clone, Copy)]
pub(crate) enum CheckToolOperations {
    Unavailable,
    Verify,
    Apply,
    Both,
}

impl CheckToolOperations {
    /// Check the requested command purpose without granting another operation's authority.
    fn allows(self, purpose: super::CheckPurpose) -> bool {
        match (self, purpose) {
            (Self::Both, _)
            | (Self::Verify, super::CheckPurpose::Verify)
            | (Self::Apply, super::CheckPurpose::Prepare) => true,
            (Self::Unavailable, _)
            | (Self::Verify, super::CheckPurpose::Prepare)
            | (Self::Apply, super::CheckPurpose::Verify) => false,
        }
    }
}

pub(crate) struct CheckToolLease(Arc<CheckToolScope>);

impl CheckToolLease {
    /// Rebind presentation without replacing the invocation's tool authority.
    pub(crate) fn bind_lifecycle(
        &self,
        sink: Option<crate::broker::publication::SessionUpdateSink>,
    ) -> Result<()> {
        let mut delivery = self
            .0
            .delivery
            .lock()
            .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?;
        let delivery = delivery
            .as_mut()
            .context("check tools belong to an inactive interaction")?;
        delivery.lifecycle = sink;
        Ok(())
    }
}

impl Drop for CheckToolLease {
    fn drop(&mut self) {
        self.0
            .delivery
            .lock()
            .expect("check delivery lock poisoned")
            .take();
    }
}

impl CheckToolScope {
    /// Bind immutable invocation authority while retaining the foreground owner's revocation lease.
    pub(crate) fn bind_invocation(
        &self,
        scope: crate::provider::InvocationScope,
        authorization: HarnessEventSink,
    ) -> Result<Arc<Self>> {
        self.validate_live()?;
        ensure!(
            scope.owner() == self.scope.owner()
                && scope.exchange_id == self.scope.exchange_id
                && authorization.invocation_scope()? == &scope,
            "check invocation belongs to another interaction"
        );
        Ok(Arc::new(Self {
            scope,
            authorization,
            ..self.clone()
        }))
    }

    /// Resolve existing planned tests through static owner-managed discovery at one source snapshot.
    pub(crate) async fn planned_warnings(
        &self,
        design: &crate::plan::DeclarationDesign,
        document_version: u64,
    ) -> Result<Option<super::SubmissionWarnings>> {
        use super::adapter::{CheckDiscoverRequest, DiscoverMode};
        let files = design
            .document
            .tests
            .iter()
            .filter(|file| {
                file.cases
                    .iter()
                    .any(|case| case.change != crate::plan::DesignTestChange::New)
            })
            .map(|file| file.file.clone())
            .collect::<Vec<_>>();
        if files.is_empty() {
            return Ok(None);
        }
        // Validate the submitted configuration before collecting any check inventory.
        self.validate_live()?;
        let expected = design
            .check_configuration
            .as_ref()
            .context("planned discovery requires the validated check configuration")?;
        let inputs =
            super::discovery::DiscoveryInputs::capture(&self.runner, self.task_id.as_deref())?;
        let config = inputs.snapshot();
        ensure!(
            expected == config,
            "check configuration changed before planned discovery; submit again"
        );
        let overrides = inputs.overrides();
        let timeout = crate::settings::SettingsStore::open(&self.data_root)?
            .get(
                &self.runner.workspace.to_string_lossy(),
                self.scope.owner().session_id().as_str(),
                &self.provider,
            )?
            .desired
            .tool_timeout
            .unwrap_or_default()
            .require_expiry()?
            .duration();
        let deadline = crate::plan::resolution_deadline::ResolutionDeadline::restrict(
            tokio::time::Instant::now() + timeout,
        );
        let before = tokio::time::timeout_at(
            deadline,
            self.runner.changes.capture(&self.runner.session_id),
        )
        .await
        .context("planned discovery source capture exceeded its tool timeout")??;
        // Share one deadline across every owner-managed static discovery.
        let mut inventories = Vec::new();
        for definition in config.config.definitions.iter().filter(|definition| {
            definition.category == super::CheckCategory::Test
                && overrides
                    .enabled
                    .get(&definition.id)
                    .copied()
                    .unwrap_or(definition.enabled)
                && !design.document.disabled_checks.contains(&definition.id)
        }) {
            self.validate_live()?;
            let discovery = if tokio::time::Instant::now() >= deadline {
                Err(super::owner::CheckOwnerFailure::Worker(anyhow::anyhow!(
                    "planned discovery exceeded its tool timeout"
                )))
            } else {
                self.runner
                    .owner_sender()?
                    .discover(
                        self.scope.clone(),
                        self.task_authority.clone(),
                        CheckDiscoverRequest {
                            check: definition.id.clone(),
                            files: files.clone(),
                            within: Vec::new(),
                            mode: DiscoverMode::Static,
                        },
                        inputs.clone(),
                        deadline,
                        self.cancelled.clone(),
                        None,
                    )
                    .await
            };
            inventories.push(super::planned::PlannedInventory::new(
                definition.id.clone(),
                discovery,
            )?);
        }
        // Reject mixed snapshots and revoked selections before publishing warnings.
        self.validate_live()?;
        let after = tokio::time::timeout_at(
            deadline,
            self.runner.changes.capture(&self.runner.session_id),
        )
        .await
        .context("planned discovery validation exceeded its tool timeout")??;
        let current = ConfigSnapshot::load(&self.runner.workspace)?;
        ensure!(
            before.id == after.id
                && config == &current
                && overrides == &self.runner.overrides(self.task_id.as_deref())?,
            "check configuration, selection, helpers, or workspace changed during planned discovery; submit again"
        );
        Ok(Some(super::SubmissionWarnings::new(
            design,
            document_version,
            before.id,
            inputs,
            super::planned::submission_warnings(design, &inventories)?,
        )?))
    }

    /// Reads live permission so an existing turn observes user revocation.
    pub(crate) fn editing_enabled(&self) -> bool {
        self.editing.load(Ordering::Acquire)
    }

    /// Reject discovery after its requesting invocation loses ownership or is cancelled.
    fn validate_live(&self) -> Result<()> {
        if let Some(authority) = &self.task_authority {
            authority.validate_scope()?;
        }
        ensure!(
            !*self.cancelled.borrow()
                && self
                    .delivery
                    .lock()
                    .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
                    .is_some(),
            "check tools belong to an inactive interaction"
        );
        Ok(())
    }

    pub(crate) fn open(
        runner: Arc<CheckSupervisor>,
        authority: CheckAuthority,
    ) -> (Arc<Self>, CheckToolLease) {
        let CheckAuthority {
            task_authority,
            data_root,
            provider,
            task_id,
            shell,
            mode,
            permission,
            permissions,
            scope,
            authorization_sink,
            editing,
            operations,
            agent,
            cancelled,
            sink,
        } = authority;
        let scope = Arc::new(Self {
            runner,
            data_root,
            provider,
            task_id,
            shell,
            mode,
            permission,
            permissions,
            scope,
            editing,
            operations,
            agent,
            cancelled,
            authorization: authorization_sink,
            delivery: Arc::new(Mutex::new(Some(CheckDelivery { lifecycle: sink }))),
            task_authority,
        });
        (Arc::clone(&scope), CheckToolLease(scope))
    }

    pub(crate) async fn invoke(&self, name: &str, arguments: Value) -> Result<String> {
        self.invoke_inner(name, arguments, None).await
    }

    /// Preserve authenticated host tool identity when requesting owner-controlled check work.
    pub(crate) async fn invoke_with_identity(
        &self,
        name: &str,
        arguments: Value,
        identity: forge_provider_api::control::ToolCallId,
    ) -> Result<String> {
        self.invoke_inner(name, arguments, Some(identity)).await
    }

    async fn invoke_inner(
        &self,
        name: &str,
        arguments: Value,
        identity: Option<forge_provider_api::control::ToolCallId>,
    ) -> Result<String> {
        if let Some(authority) = &self.task_authority {
            authority.validate_scope()?;
            if !crate::tools::is_read_only(name) {
                authority.admit_mutation()?;
            }
        }
        let delivery = self
            .delivery
            .lock()
            .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
            .clone()
            .context("check tools belong to an inactive interaction")?;
        ensure!(
            !*self.cancelled.borrow(),
            "check tools belong to an inactive interaction"
        );
        if name == "check_run" {
            self.runner.refresh_changes().await?;
        }
        let result = match name {
            "check_definition_list" => {
                let settings = crate::settings::SettingsStore::open(&self.data_root)?
                    .get(
                        &self.runner.workspace.to_string_lossy(),
                        self.scope.owner().session_id().as_str(),
                        &self.provider,
                    )?
                    .desired;
                let mut definitions = super::response::definitions(&self.runner.workspace)?;
                let overrides = self.runner.overrides(self.task_id.as_deref())?;
                for definition in definitions["definitions"]
                    .as_array_mut()
                    .into_iter()
                    .flatten()
                {
                    let id = definition["check_id"]
                        .as_str()
                        .context("missing check identity")?
                        .to_owned();
                    if let Some(enabled) = overrides.enabled.get(&id) {
                        definition["enabled"] = json!(enabled);
                    }
                    if let Some(auto) = overrides.auto.get(&id) {
                        definition["auto"] = json!(auto);
                    }
                }
                definitions["automatic_checks"] = json!(settings.automatic_checks_enabled());
                definitions["check_timing"] = json!(settings.check_timing.unwrap_or_default());
                definitions["check_editing"] = json!(self.editing_enabled());
                definitions
            }
            "check_definition_details" => {
                let id = arguments["check_id"]
                    .as_str()
                    .context("check_id is required")?;
                super::response::definition(&self.runner.workspace, id)?
            }
            crate::tools::CHECK_DEFINITION_EDIT => {
                ensure!(
                    self.editing_enabled(),
                    "{}",
                    crate::tools::unavailable_tool_reason(crate::tools::CHECK_DEFINITION_EDIT)
                );
                ensure!(
                    self.agent.active_count() == 0,
                    "check setup cannot race active agents"
                );
                let request: ConfigureChecks = serde_json::from_value(arguments)?;
                let review = request.review(&self.runner.workspace)?;
                let original = super::config::read_document(&self.runner.workspace)?;
                let proposed = serde_json::to_value(&request.config)?;
                let before = original["checks"].as_object();
                let after = proposed
                    .as_object()
                    .context("check definitions must be an object")?;
                let names: std::collections::BTreeSet<_> = before
                    .into_iter()
                    .flat_map(|map| map.keys())
                    .chain(after.keys())
                    .collect();
                let changed_checks: Vec<_> = names
                    .into_iter()
                    .filter(|name| before.and_then(|map| map.get(*name)) != after.get(*name))
                    .cloned()
                    .collect();
                let changed_helpers: Vec<_> = request
                    .helpers
                    .iter()
                    .map(|helper| helper.path.clone())
                    .collect();
                let target_list = std::iter::once(".forge.json")
                    .chain(request.helpers.iter().map(|helper| helper.path.as_str()))
                    .map(
                        |path| crate::permissions::matcher::PermissionTarget::Write {
                            path: self
                                .runner
                                .workspace
                                .join(path)
                                .to_string_lossy()
                                .into_owned(),
                        },
                    )
                    .collect();
                self.authorize(self.permission, target_list, &self.authorization)
                    .await?;
                ensure!(
                    self.permission != ApprovalMode::Deny,
                    "check configuration review is denied by the selected approval policy"
                );
                if self.permission == ApprovalMode::Ask {
                    let resolution = self
                        .permissions
                        .review(
                            "Update project checks".into(),
                            review,
                            self.scope.clone(),
                            &self.authorization,
                        )
                        .await?;
                    ensure!(
                        resolution == crate::provider::approval::ApprovalResolution::AllowOnce,
                        "check configuration update was not approved"
                    );
                }
                ensure!(
                    !*self.cancelled.borrow()
                        && self
                            .delivery
                            .lock()
                            .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
                            .is_some(),
                    "check configuration was interrupted during review"
                );
                ensure!(
                    self.agent.active_count() == 0,
                    "an agent started during check configuration authorization"
                );
                let published = self
                    .runner
                    .configure_with_authority(
                        self.runner
                            .exchange_owner(self.scope.exchange_id.as_str())?,
                        request,
                        delivery.lifecycle.as_ref(),
                        Some(&self.editing),
                        self.task_authority.as_ref(),
                    )
                    .await?;
                json!({
                    "revision":published["revision"],"changed_checks":changed_checks,
                    "changed_helpers":changed_helpers,"checks_executed":false
                })
            }
            "check_discover" => {
                use super::adapter::{CheckDiscoverRequest, DiscoverMode};
                let request: CheckDiscoverRequest = serde_json::from_value(arguments)?;
                let inputs = super::discovery::DiscoveryInputs::capture(
                    &self.runner,
                    self.task_id.as_deref(),
                )?;
                let exact = request.mode == DiscoverMode::Exact;
                let quiescence = exact.then(|| self.agent.begin_quiescence());
                if exact {
                    ensure!(
                        self.operations.allows(super::CheckPurpose::Verify)
                            && self.agent.active_count() == 0
                            && !matches!(
                                self.mode,
                                PromptMode::Plan
                                    | PromptMode::PlanDiscussion
                                    | PromptMode::PlanQuestion
                            ),
                        "exact discovery requires check execution authority and no active writers"
                    );
                }
                let timeout = crate::settings::SettingsStore::open(&self.data_root)?
                    .get(
                        &self.runner.workspace.to_string_lossy(),
                        self.scope.owner().session_id().as_str(),
                        &self.provider,
                    )?
                    .desired
                    .tool_timeout
                    .unwrap_or_default()
                    .require_expiry()?
                    .duration();
                let deadline = tokio::time::Instant::now() + timeout;
                let discovered = self
                    .runner
                    .owner_sender()?
                    .discover(
                        self.scope.clone(),
                        self.task_authority.clone(),
                        request,
                        inputs,
                        deadline,
                        self.cancelled.clone(),
                        quiescence,
                    )
                    .await?;
                serde_json::to_value(discovered)?
            }
            "check_run" => {
                let request = RunRequest::try_from(serde_json::from_value::<
                    super::CheckRunRequest,
                >(arguments)?)?;
                ensure!(
                    self.operations.allows(request.purpose)
                        && self.agent.active_count() == 0
                        && !matches!(
                            self.mode,
                            PromptMode::Plan
                                | PromptMode::PlanDiscussion
                                | PromptMode::PlanQuestion
                        ),
                    "this check operation is unavailable: plan Verify permits read-only checks, Resolve permits fixes, and check setup or active delegated agents exclude both"
                );
                let call_id = identity
                    .clone()
                    .context("check_run requires authenticated tool-call identity")?;
                let launch_hold = self.agent.hold_launches()?;
                let retained_request = request.clone();
                let run = self.runner.prepare_request(
                    self.runner
                        .exchange_owner(self.scope.exchange_id.as_str())?,
                    request,
                    self.task_id.clone(),
                    self.shell,
                    "Agent requested".into(),
                )?;
                let run = self.runner.resolve(run).await?;
                ensure!(
                    self.agent.active_count() == 0,
                    "an agent started during check resolution"
                );
                json!({"run_id":self.runner.owner_sender()?.start(
                    self.scope.clone(), self.task_authority.clone(), call_id, retained_request, run, delivery.lifecycle, self.cancelled.clone(), launch_hold,
                ).await?})
            }
            "check_status" => {
                let id = arguments["run_id"].as_str().context("run_id is required")?;
                let wait_ms = arguments["wait_ms"].as_u64().unwrap_or(0);
                self.runner.status_summary(id, wait_ms).await?
            }
            "check_list" => self.runner.running_summaries()?,
            "check_detail" => {
                let request: super::DetailRequest = serde_json::from_value(arguments)?;
                let run = self.runner.status(&request.run_id, 0, false).await?;
                tokio::task::spawn_blocking(move || super::response::detail(&run, &request))
                    .await
                    .context("read completed check details")??
            }
            "check_cancel" => {
                let id = arguments["run_id"].as_str().context("run_id is required")?;
                self.runner
                    .owner_sender()?
                    .cancel(self.scope.clone(), id.to_owned())
                    .await?;
                json!({"run_id":id,"cancellation_requested":true})
            }
            _ => anyhow::bail!("unknown checks tool"),
        };
        if let Some(authority) = &self.task_authority {
            authority.validate_scope()?;
        }
        Ok(serde_json::to_string(&result)?)
    }

    async fn authorize(
        &self,
        mode: ApprovalMode,
        target_list: Vec<crate::permissions::matcher::PermissionTarget>,
        authorization: &HarnessEventSink,
    ) -> Result<()> {
        if target_list.is_empty() {
            return Ok(());
        }
        let resolution = self
            .permissions
            .authorize(
                mode,
                crate::permissions::matcher::PermissionRequest {
                    id: uuid::Uuid::new_v4().to_string(),
                    provider: forge_provider_api::ProviderId::new("harness-checks")
                        .expect("static provider identity is valid"),
                    subject: forge_provider_api::permissions::PermissionSubject::Unattached,
                    reason: Some("Configured project checks".into()),
                    target_list,
                },
                None,
                Some(authorization),
            )
            .await?;
        ensure!(
            matches!(
                resolution,
                crate::provider::approval::ApprovalResolution::AllowOnce
                    | crate::provider::approval::ApprovalResolution::AllowExact
                    | crate::provider::approval::ApprovalResolution::AllowBroad
            ),
            "check operation was not authorized"
        );
        ensure!(
            self.delivery
                .lock()
                .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
                .is_some()
                && !*self.cancelled.borrow(),
            "check operation was interrupted during authorization"
        );
        Ok(())
    }
}

impl ConfigureChecks {
    fn review(&self, workspace: &std::path::Path) -> Result<String> {
        self.config.validate()?;
        let document = super::config::read_document(workspace)?;
        ensure!(
            crate::plan::digest(&serde_json::to_vec(&document)?) == self.expected_revision,
            ".forge.json changed since setup began"
        );
        let before = serde_json::to_string_pretty(&document["checks"])?;
        let after = serde_json::to_string_pretty(&self.config)?;
        let mut detail = String::from(
            "Approve these project check changes and managed helper updates. No checks will run.\n\n",
        );
        detail.push_str(&super::review::describe(
            &document["checks"],
            &serde_json::to_value(&self.config)?,
        )?);
        detail.push_str("\n### Raw configuration diff\n");
        detail.push_str(&super::review::fenced(
            "raw-diff",
            &similar::TextDiff::from_lines(&before, &after)
                .unified_diff()
                .header(
                    ".forge.json checks (current)",
                    ".forge.json checks (proposed)",
                )
                .to_string(),
        ));
        if before == after {
            detail.push_str("Check definitions are unchanged.\n");
        }
        for helper in &self.helpers {
            let relative = super::config::relative_path(&helper.path)?;
            ensure!(
                helper.path.starts_with(".forge/checks/") && relative.file_name().is_some(),
                "helpers must be files under .forge/checks/"
            );
            let destination = workspace.join(relative);
            super::config::reject_links(workspace, &destination)?;
            let previous = match std::fs::read(&destination) {
                Ok(source) => Some(source),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                Err(error) => return Err(error.into()),
            };
            ensure!(
                previous.as_ref().map(|source| crate::plan::digest(source))
                    == helper.expected_digest,
                "helper changed since setup began: {}",
                helper.path
            );
            let before = previous
                .as_deref()
                .map(std::str::from_utf8)
                .transpose()?
                .unwrap_or("");
            detail.push_str(&format!(
                "\n### Helper: {}\n",
                serde_json::to_string(&helper.path)?
            ));
            detail.push_str(&super::review::fenced(
                "diff",
                &similar::TextDiff::from_lines(before, &helper.source)
                    .unified_diff()
                    .header(
                        &format!("{} (current)", helper.path),
                        &format!("{} (proposed)", helper.path),
                    )
                    .to_string(),
            ));
        }
        Ok(detail)
    }
}

impl CheckSupervisor {
    /// Adds effective completion policy and concrete task commands without running checks.
    pub(crate) async fn catalog_preview(
        &self,
        task_id: Option<&str>,
        shell: CheckShell,
        settings: &crate::settings::PortableSettings,
    ) -> Result<Value> {
        let mut catalog = self.catalog(task_id)?;
        catalog["automatic_checks"] = json!(settings.automatic_checks_enabled());
        catalog["check_timing"] = json!(settings.check_timing.unwrap_or_default());
        if let Ok(snapshot) = ConfigSnapshot::load(&self.workspace) {
            let overrides = self.overrides(task_id)?;
            let mut config = snapshot.config;
            for definition in &mut config.definitions {
                if let Some(auto) = overrides.auto.get(&definition.id) {
                    definition.auto = *auto;
                }
                if let Some(enabled) = overrides.enabled.get(&definition.id) {
                    definition.enabled = *enabled;
                }
                if let Some(scope) = overrides.scopes.get(&definition.id) {
                    definition.scope = *scope;
                }
            }
            if let Some(files) = self.changes.files()? {
                catalog["preview"] = serde_json::to_value(
                    super::preview::resolve(&self.workspace, &config, &files, &[], None).await?,
                )?;
                if let Some(checkpoint) = self.changes.snapshot().await? {
                    let run = self
                        .resolve(self.prepare_request(
                            self.session_owner(),
                            RunRequest::default(),
                            task_id.map(str::to_owned),
                            shell,
                            "Coverage preview".into(),
                        )?)
                        .await?;
                    let mut coverage = Vec::new();
                    for command in &run.commands {
                        if command.state != super::CheckState::Pending {
                            continue;
                        }
                        let previous = self.evidence(
                            &run.clone().into_record(),
                            &command.clone().into_record(),
                            &checkpoint,
                        )?;
                        coverage.push(json!({"id":command.context.definition_id,"command":command.command,
                            "pending":previous.is_none(),"evidence":previous.map(|(run_id, command)| json!({"run_id":run_id,"state":command.state,"command_id":command.id}))}));
                    }
                    catalog["coverage"] = json!(coverage);
                }
            }
        }
        if let Some(owner) = task_id.map(str::to_owned).or(self.changes.owner()?) {
            catalog["automatic"] =
                serde_json::to_value(super::automatic::AutomaticChecks::open(&self.root, &owner)?)?;
        }
        let checkpoint = self.changes.snapshot().await?;
        catalog["pending_verification"] = serde_json::to_value(
            self.verification
                .lock()
                .map_err(|_| anyhow::anyhow!("verification ledger lock poisoned"))?
                .pending(checkpoint.as_ref())?,
        )?;
        Ok(catalog)
    }
    /// Projects saved configuration, task controls, and history without inventing defaults.
    pub(crate) fn catalog(&self, task_id: Option<&str>) -> Result<Value> {
        let document = super::config::read_document(&self.workspace)?;
        let revision = crate::plan::digest(&serde_json::to_vec(&document)?);
        let overrides = self.overrides(task_id)?;
        let changes = self.changes.files()?;
        let snapshot = ConfigSnapshot::load(&self.workspace);
        let (configured, snapshot, error) = match snapshot {
            Ok(snapshot) => (true, Some(snapshot), None),
            Err(error) => (
                document.get("checks").is_some(),
                None,
                Some(format!("{error:#}")),
            ),
        };
        let applicability = snapshot
            .as_ref()
            .map(|snapshot| {
                let mut config = snapshot.config.clone();
                for check in &mut config.definitions {
                    if let Some(auto) = overrides.auto.get(&check.id) {
                        check.auto = *auto;
                    }
                    if let Some(enabled) = overrides.enabled.get(&check.id) {
                        check.enabled = *enabled;
                    }
                }
                super::applicability::resolve(
                    &RunRequest::default(),
                    &config,
                    &changes.clone().map_or(
                        super::ChangeInventory::Untracked,
                        super::ChangeInventory::Known,
                    ),
                )
            })
            .transpose()?
            .unwrap_or_default();
        Ok(
            json!({"configured":configured,"revision":revision,"snapshot":snapshot,"error":error,
            "overrides":overrides,"history":self.history()?,"active_run":self.active()?,"task_id":task_id,
            "changed_files":changes,"applicability":applicability}),
        )
    }

    fn override_path(&self, task_id: Option<&str>) -> PathBuf {
        self.root.join(format!(
            "overrides-{}.json",
            crate::plan::digest(task_id.unwrap_or("conversation").as_bytes())
        ))
    }

    pub(crate) fn overrides(&self, task_id: Option<&str>) -> Result<CheckOverrides> {
        match std::fs::read(self.override_path(task_id)) {
            Ok(source) => Ok(serde_json::from_slice(&source)?),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(Default::default()),
            Err(error) => Err(error.into()),
        }
    }

    /// Saves temporary states only for the captured task or ad hoc conversation.
    pub(crate) fn set_overrides(
        &self,
        task_id: Option<&str>,
        overrides: CheckOverrides,
    ) -> Result<Value> {
        ensure!(
            self.active()?.is_none(),
            "check settings cannot change while a run is active"
        );
        let snapshot = ConfigSnapshot::load(&self.workspace)?;
        for id in overrides
            .enabled
            .keys()
            .chain(overrides.scopes.keys())
            .chain(overrides.auto.keys())
        {
            let definition = snapshot
                .config
                .definitions
                .iter()
                .find(|definition| &definition.id == id)
                .context("unknown check id")?;
            if let Some(scope) = overrides.scopes.get(id) {
                ensure!(
                    scope.supports(definition.category),
                    "scope is incompatible with the check category"
                );
            }
        }
        let path = self.override_path(task_id);
        let temporary = path.with_extension("tmp");
        std::fs::write(&temporary, serde_json::to_vec(&overrides)?)?;
        std::fs::rename(temporary, path)?;
        self.catalog(task_id)
    }

    pub(crate) fn prepare_request(
        &self,
        owner: crate::timeline::TimelineOwner,
        request: RunRequest,
        task_id: Option<String>,
        shell: CheckShell,
        trigger: String,
    ) -> Result<super::model::PreparedCheckRun> {
        let mut snapshot = ConfigSnapshot::load(&self.workspace)?;
        let overrides = self.overrides(task_id.as_deref())?;
        let changes = self.changes.files()?;
        for definition in &mut snapshot.config.definitions {
            if let Some(auto) = overrides.auto.get(&definition.id) {
                definition.auto = *auto;
            }
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        self.prepare(
            snapshot,
            super::CheckAdmission {
                owner,
                request,
                overrides: overrides.enabled.clone(),
                task_overrides: overrides,
                inventory: changes.map_or(
                    super::ChangeInventory::Untracked,
                    super::ChangeInventory::Known,
                ),
                shell,
                trigger,
                task_id,
                revision: 0,
            },
        )
    }

    pub(super) async fn configure_with_authority(
        &self,
        owner: crate::timeline::TimelineOwner,
        request: ConfigureChecks,
        sink: Option<&crate::broker::publication::SessionUpdateSink>,
        editing: Option<&AtomicBool>,
        authority: Option<&crate::tools::TaskAuthority>,
    ) -> Result<Value> {
        if let Some(authority) = authority {
            authority.validate_scope()?;
            authority.admit_mutation()?;
        }
        ensure!(
            owner.session_id() == self.session_id,
            "check configuration crosses sessions"
        );
        crate::storage::writer::HistoryWriter::lease_for(&self.data_root)?
            .validate_timeline_owner(&owner)?;
        ensure!(
            self.active()?.is_none(),
            "check setup cannot change an active run"
        );
        request.config.validate()?;
        let original = super::config::read_document(&self.workspace)?;
        ensure!(
            crate::plan::digest(&serde_json::to_vec(&original)?) == request.expected_revision,
            ".forge.json changed since setup began"
        );
        let mut helpers = Vec::new();
        let mut helper_path = std::collections::BTreeSet::new();
        for helper in &request.helpers {
            let relative = super::config::relative_path(&helper.path)?;
            ensure!(
                helper.path.starts_with(".forge/checks/")
                    && relative
                        .extension()
                        .is_some_and(|extension| extension == "nu"),
                "helpers must be Nushell files under .forge/checks/"
            );
            let destination = self.workspace.join(relative);
            ensure!(
                helper_path.insert(destination.clone()),
                "duplicate helper path: {}",
                helper.path
            );
            super::config::reject_links(&self.workspace, &destination)?;
            let mut ancestor = destination.parent().context("helper has no parent")?;
            while !ancestor.exists() {
                ancestor = ancestor.parent().context("helper has no existing parent")?;
            }
            ensure!(
                ancestor
                    .canonicalize()?
                    .starts_with(self.workspace.canonicalize()?),
                "helper escapes the project"
            );
            let previous = match std::fs::read(&destination) {
                Ok(source) => Some(source),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
                Err(error) => return Err(error.into()),
            };
            ensure!(
                previous.as_ref().map(|source| crate::plan::digest(source))
                    == helper.expected_digest,
                "helper changed since setup began: {}",
                helper.path
            );
            helpers.push((destination, previous, helper.source.as_bytes().to_vec()));
        }
        let mut document = original.clone();
        document["checks"] = serde_json::to_value(&request.config)?;
        let replacement = serde_json::to_vec_pretty(&document)?;
        ensure!(
            super::config::read_document(&self.workspace)? == original,
            ".forge.json changed during validation"
        );
        let destination = self.workspace.join(".forge.json");
        ensure!(
            !destination.exists()
                || !std::fs::symlink_metadata(&destination)?
                    .file_type()
                    .is_symlink(),
            ".forge.json cannot be a symlink"
        );
        for (path, previous, _) in &helpers {
            ensure!(
                std::fs::read(path).ok().as_ref() == previous.as_ref(),
                "helper changed during validation"
            );
        }
        let temporary = self
            .workspace
            .join(format!(".forge.json.{}.tmp", uuid::Uuid::new_v4()));
        ensure!(
            editing.is_none_or(|permission| permission.load(Ordering::Acquire)),
            "Check editing was disabled before configuration publication"
        );
        let commit = || {
            ensure!(
                editing.is_none_or(|permission| permission.load(Ordering::Acquire)),
                "Check editing was disabled before configuration publication"
            );
            ensure!(
                super::config::read_document(&self.workspace)? == original,
                ".forge.json changed before setup publication"
            );
            for (path, previous, _) in &helpers {
                ensure!(
                    std::fs::read(path).ok().as_ref() == previous.as_ref(),
                    "helper changed before setup publication"
                );
            }
            for (path, _, source) in &helpers {
                super::config::reject_links(&self.workspace, path)?;
                std::fs::create_dir_all(path.parent().unwrap())?;
                std::fs::write(path, source)?;
            }
            std::fs::write(&temporary, &replacement)?;
            ensure!(
                super::config::read_document(&self.workspace)? == original,
                ".forge.json changed before setup publication"
            );
            std::fs::rename(&temporary, &destination)?;
            Ok::<_, anyhow::Error>(())
        };
        let publication = || {
            let result = commit();
            if result.is_err() {
                for (path, previous, source) in &helpers {
                    if std::fs::read(path).ok().as_ref() == Some(source) {
                        if let Some(previous) = previous {
                            std::fs::write(path, previous)?;
                        } else {
                            std::fs::remove_file(path)?;
                        }
                    }
                }
                if temporary.exists() {
                    std::fs::remove_file(&temporary)?;
                }
            }
            result
        };
        match authority {
            Some(authority) => authority.commit(publication)?,
            None => publication()?,
        }
        let snapshot = ConfigSnapshot::load(&self.workspace)?;
        let mut run = self.prepare(
            snapshot,
            super::CheckAdmission {
                owner,
                request: Default::default(),
                overrides: BTreeMap::new(),
                task_overrides: Default::default(),
                inventory: super::ChangeInventory::Known(Vec::new()),
                shell: CheckShell::default(),
                trigger: "Configuration saved".into(),
                task_id: None,
                revision: 0,
            },
        )?;
        run.commands.clear();
        run.lifecycle.clear();
        run.state = super::CheckState::Skipped;
        run.completed_at_ms = Some(super::supervisor::now_ms());
        run.record(
            super::CheckEventKind::ConfigurationSaved,
            None,
            format!(
                "Check configuration saved · {} definitions",
                request.config.definitions.len()
            ),
        );
        let run = run.into_record();
        if let Some(authority) = authority {
            self.publish_with_authority(&run, sink, authority)?;
        } else {
            self.publish(&run, sink).await?;
        }
        self.catalog(None)
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;

    #[test]
    fn check_approval_review_matches_editor_recording() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let original = json!({"checks":{"lua-test":{"trigger":{},"action":{
            "category":"test","after":["rust-build"],"adapter":{"script":{"command":["nu","tests.nu"]}}
        }},"rust-build":{"trigger":{},"action":{"category":"build","adapter":{"script":{"command":["nu","build.nu"]}}}}}});
        std::fs::write(
            workspace.path().join(".forge.json"),
            serde_json::to_vec(&original)?,
        )?;
        let mut proposed = original["checks"].clone();
        proposed["lua-test"]["action"]["after"] = json!([]);
        proposed["lua-test"]["action"]["adapter"]["script"]["command"] =
            json!(["nu", "tests.nu", "--partition", "non-host"]);
        let request = ConfigureChecks {
            expected_revision: crate::plan::digest(&serde_json::to_vec(&original)?),
            config: serde_json::from_value(proposed)?,
            helpers: vec![CheckHelper {
                path: ".forge/checks/example.nu".into(),
                source: "print 'ready'\n".into(),
                expected_digest: None,
            }],
        };
        let detail = request.review(workspace.path())?;
        forge_recorded_fixtures::assert_matches_recording(
            "check-approval-review.json",
            &json!({"detail":detail}),
        );
        assert_eq!(
            super::super::config::read_document(workspace.path())?,
            original
        );
        assert!(!workspace.path().join(".forge/checks/example.nu").exists());
        let mut stale = request;
        stale.expected_revision = "stale".into();
        assert!(stale.review(workspace.path()).is_err());
        Ok(())
    }
}
