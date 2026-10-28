use super::{CheckConfig, CheckSupervisor, ConfigSnapshot, RunRequest};
use crate::{
    backend::{BackendEventSink, PromptMode},
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

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
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
pub(crate) struct CheckToolScope {
    runner: Arc<CheckSupervisor>,
    data_root: PathBuf,
    backend: String,
    task_id: Option<String>,
    shell: CheckShell,
    mode: PromptMode,
    permission: ApprovalMode,
    permissions: Arc<crate::backend::approval::PermissionCoordinator>,
    address: crate::backend::ProviderAddress,
    editing: Arc<AtomicBool>,
    setup: bool,
    can_run: bool,
    agent: Arc<crate::agent::AgentCoordinator>,
    cancelled: Arc<AtomicBool>,
    delivery: Mutex<Option<CheckDelivery>>,
}

#[derive(Clone)]
struct CheckDelivery {
    authorization: BackendEventSink,
    lifecycle: Option<BackendEventSink>,
}

/// Binds check operations to the live root turn and its setup/write authority.
pub(crate) struct CheckAuthority {
    pub data_root: PathBuf,
    pub backend: String,
    pub task_id: Option<String>,
    pub shell: CheckShell,
    pub mode: PromptMode,
    pub permission: ApprovalMode,
    pub permissions: Arc<crate::backend::approval::PermissionCoordinator>,
    pub address: crate::backend::ProviderAddress,
    pub authorization_sink: BackendEventSink,
    pub editing: Arc<AtomicBool>,
    pub setup: bool,
    pub can_run: bool,
    pub agent: Arc<crate::agent::AgentCoordinator>,
    pub cancelled: Arc<AtomicBool>,
    pub sink: Option<BackendEventSink>,
}

pub(crate) struct CheckToolLease(Arc<CheckToolScope>);

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
    /// Reads live permission so an existing turn observes user revocation.
    pub(crate) fn editing_enabled(&self) -> bool {
        self.editing.load(Ordering::Acquire)
    }

    pub(crate) fn is_setup(&self) -> bool {
        self.setup
    }

    pub(crate) fn open(
        runner: Arc<CheckSupervisor>,
        authority: CheckAuthority,
    ) -> (Arc<Self>, CheckToolLease) {
        let CheckAuthority {
            data_root,
            backend,
            task_id,
            shell,
            mode,
            permission,
            permissions,
            address,
            authorization_sink,
            editing,
            setup,
            can_run,
            agent,
            cancelled,
            sink,
        } = authority;
        let scope = Arc::new(Self {
            runner,
            data_root,
            backend,
            task_id,
            shell,
            mode,
            permission,
            permissions,
            address,
            editing,
            setup,
            can_run,
            agent,
            cancelled,
            delivery: Mutex::new(Some(CheckDelivery {
                authorization: authorization_sink,
                lifecycle: sink,
            })),
        });
        (Arc::clone(&scope), CheckToolLease(scope))
    }

    pub(crate) async fn invoke(&self, name: &str, arguments: Value) -> Result<String> {
        let delivery = self
            .delivery
            .lock()
            .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
            .clone()
            .context("check tools belong to an inactive interaction")?;
        ensure!(
            !self.cancelled.load(Ordering::Acquire),
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
                        &self.address.thread_id,
                        &self.backend,
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
                definitions["automatic_checks"] = json!(settings.automatic_checks.unwrap_or(true));
                definitions["check_timing"] = json!(settings.check_timing.unwrap_or_default());
                definitions
            }
            "check_definition_details" => {
                let id = arguments["check_id"]
                    .as_str()
                    .context("check_id is required")?;
                super::response::definition(&self.runner.workspace, id)?
            }
            "check_definition_edit" => {
                ensure!(
                    self.editing_enabled(),
                    "Check editing is disabled. The user can enable it with /checks edit on, then request changes using the harness-checks skill."
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
                self.authorize(self.permission, target_list, &delivery.authorization)
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
                            self.address.clone(),
                            &delivery.authorization,
                        )
                        .await?;
                    ensure!(
                        resolution == crate::backend::approval::ApprovalResolution::AllowOnce,
                        "check configuration update was not approved"
                    );
                }
                ensure!(
                    !self.cancelled.load(Ordering::Acquire)
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
                    .configure(
                        self.runner.exchange_owner(&self.address.turn_id)?,
                        request,
                        delivery.lifecycle.as_ref(),
                        Some(&self.editing),
                    )
                    .await?;
                json!({
                    "revision":published["revision"],"changed_checks":changed_checks,
                    "changed_helpers":changed_helpers,"checks_executed":false
                })
            }
            "check_run" => {
                ensure!(
                    self.can_run
                        && self.agent.active_count() == 0
                        && !matches!(
                            self.mode,
                            PromptMode::Plan
                                | PromptMode::PlanDiscussion
                                | PromptMode::PlanQuestion
                                | PromptMode::ExecutePlan
                        ),
                    "automated checks are owned by Harness during plan execution and cannot race active writing agents"
                );
                let request = RunRequest::try_from(serde_json::from_value::<
                    super::CheckRunRequest,
                >(arguments)?)?;
                let run = self.runner.prepare_request(
                    self.runner.exchange_owner(&self.address.turn_id)?,
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
                json!({"run_id":self.runner.start(run, delivery.lifecycle,Some(Arc::clone(&self.cancelled)),None).await?})
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
                self.runner.cancel(id)?;
                json!({"run_id":id,"cancellation_requested":true})
            }
            _ => anyhow::bail!("unknown checks tool"),
        };
        Ok(serde_json::to_string(&result)?)
    }

    async fn authorize(
        &self,
        mode: ApprovalMode,
        target_list: Vec<crate::permissions::matcher::PermissionTarget>,
        authorization: &BackendEventSink,
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
                    provider: "harness-checks".into(),
                    reason: Some("Configured project checks".into()),
                    target_list,
                },
                Some(self.address.clone()),
                Some(authorization),
            )
            .await?;
        ensure!(
            matches!(
                resolution,
                crate::backend::approval::ApprovalResolution::AllowOnce
                    | crate::backend::approval::ApprovalResolution::AllowExact
                    | crate::backend::approval::ApprovalResolution::AllowBroad
            ),
            "check operation was not authorized"
        );
        ensure!(
            self.delivery
                .lock()
                .map_err(|_| anyhow::anyhow!("check delivery lock poisoned"))?
                .is_some()
                && !self.cancelled.load(Ordering::Acquire),
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
        detail.push_str(
            &similar::TextDiff::from_lines(&before, &after)
                .unified_diff()
                .header(
                    ".forge.json checks (current)",
                    ".forge.json checks (proposed)",
                )
                .to_string(),
        );
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
            detail.push_str(&format!("\nManaged helper: {}\n", helper.path));
            detail.push_str(
                &similar::TextDiff::from_lines(before, &helper.source)
                    .unified_diff()
                    .header(
                        &format!("{} (current)", helper.path),
                        &format!("{} (proposed)", helper.path),
                    )
                    .to_string(),
            );
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
        catalog["automatic_checks"] = json!(settings.automatic_checks.unwrap_or(true));
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
                    super::preview::resolve(&self.workspace, &config, &files, None, None, &[])
                        .await?,
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
                    changes.as_deref(),
                    changes.is_some(),
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
                    !matches!(scope, super::CheckScope::ChangedFiles) || definition.files.is_some(),
                    "file selection is unsupported"
                );
                ensure!(
                    !matches!(
                        scope,
                        super::CheckScope::ChangedCases | super::CheckScope::NewCases
                    ) || definition.cases.is_some(),
                    "case selection is unsupported"
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
                overrides: overrides.enabled,
                files: changes.clone(),
                cases: None,
                new_cases: None,
                has_scope: changes.is_some(),
                shell,
                trigger,
                task_id: task_id.or(self.changes.owner()?),
                revision: 0,
            },
        )
    }

    /// Publishes temporary user controls as project defaults and clears only their owning task.
    pub(crate) async fn save_defaults(
        &self,
        task_id: Option<&str>,
        sink: Option<&BackendEventSink>,
    ) -> Result<Value> {
        let catalog = self.catalog(task_id)?;
        let mut snapshot = ConfigSnapshot::load(&self.workspace)?;
        let overrides = self.overrides(task_id)?;
        for definition in &mut snapshot.config.definitions {
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
        self.configure(
            self.session_owner(),
            ConfigureChecks {
                expected_revision: catalog["revision"]
                    .as_str()
                    .context("configuration revision unavailable")?
                    .into(),
                config: snapshot.config,
                helpers: Vec::new(),
            },
            sink,
            None,
        )
        .await?;
        self.set_overrides(task_id, Default::default())
    }

    /// Validates captured identities before saving settings and opaque helper sources.
    pub(crate) async fn configure(
        &self,
        owner: crate::timeline::TimelineOwner,
        request: ConfigureChecks,
        sink: Option<&BackendEventSink>,
        editing: Option<&AtomicBool>,
    ) -> Result<Value> {
        ensure!(
            owner.session_id() == self.session_id,
            "check configuration crosses sessions"
        );
        crate::storage::SqliteStore::open(&self.data_root)?.validate_timeline_owner(&owner)?;
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
        let publication = (|| {
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
        })();
        if publication.is_err() {
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
        publication?;
        let snapshot = ConfigSnapshot::load(&self.workspace)?;
        let mut run = self.prepare(
            snapshot,
            super::CheckAdmission {
                owner,
                request: Default::default(),
                overrides: BTreeMap::new(),
                files: Some(Vec::new()),
                cases: None,
                new_cases: None,
                has_scope: true,
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
        self.publish(&run.into_record(), sink).await?;
        self.catalog(None)
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;

    #[test]
    fn check_configuration_review_includes_helper_changes_and_rejects_stale_captures() -> Result<()>
    {
        let workspace = tempfile::tempdir()?;
        let document = json!({"branch_prefix":"keep","checks":{}});
        std::fs::write(
            workspace.path().join(".forge.json"),
            serde_json::to_vec(&document)?,
        )?;
        std::fs::create_dir_all(workspace.path().join(".forge/checks"))?;
        let destination = workspace.path().join(".forge/checks/example.nu");
        let before = "def main [] { print 'before' }\n";
        std::fs::write(&destination, before)?;
        let request = ConfigureChecks {
            expected_revision: crate::plan::digest(&serde_json::to_vec(&document)?),
            config: serde_json::from_value(json!({}))?,
            helpers: vec![CheckHelper {
                path: ".forge/checks/example.nu".into(),
                source: "def main [] { print 'after' }\n".into(),
                expected_digest: Some(crate::plan::digest(before.as_bytes())),
            }],
        };
        let detail = request.review(workspace.path())?;
        assert!(detail.contains("Check definitions are unchanged."));
        assert!(detail.contains("Managed helper: .forge/checks/example.nu"));
        assert!(detail.contains("-def main [] { print 'before' }"));
        assert!(detail.contains("+def main [] { print 'after' }"));
        assert_eq!(std::fs::read_to_string(&destination)?, before);
        std::fs::write(&destination, "newer helper")?;
        assert!(
            request
                .review(workspace.path())
                .unwrap_err()
                .to_string()
                .contains("helper changed since setup began")
        );
        Ok(())
    }
}
