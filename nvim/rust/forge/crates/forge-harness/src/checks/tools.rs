use super::{CheckConfig, CheckRunner, ConfigSnapshot, RunRequest};
use crate::{
    backend::{BackendEventSink, PromptMode},
    session::{CheckShell, PermissionMode},
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
use tokio::io::AsyncWriteExt;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains user-selected check states independently of the project configuration.
pub(crate) struct CheckOverrides {
    #[serde(default)]
    pub enabled: BTreeMap<String, bool>,
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
    runner: Arc<CheckRunner>,
    task_id: Option<String>,
    shell: CheckShell,
    mode: PromptMode,
    permission: PermissionMode,
    permissions: Arc<crate::backend::approval::PermissionCoordinator>,
    address: crate::backend::ProviderAddress,
    setup: bool,
    setup_reviewed: bool,
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
    pub task_id: Option<String>,
    pub shell: CheckShell,
    pub mode: PromptMode,
    pub permission: PermissionMode,
    pub permissions: Arc<crate::backend::approval::PermissionCoordinator>,
    pub address: crate::backend::ProviderAddress,
    pub authorization_sink: BackendEventSink,
    pub setup: bool,
    /// Whether this setup exchange consumed an explicit user answer.
    pub setup_reviewed: bool,
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
    /// Reports the host-owned setup decision state to the deterministic fixture provider.
    pub(crate) fn setup_reviewed(&self) -> Option<bool> {
        self.setup.then_some(self.setup_reviewed)
    }

    pub(crate) fn open(
        runner: Arc<CheckRunner>,
        authority: CheckAuthority,
    ) -> (Arc<Self>, CheckToolLease) {
        let CheckAuthority {
            task_id,
            shell,
            mode,
            permission,
            permissions,
            address,
            authorization_sink,
            setup,
            setup_reviewed,
            can_run,
            agent,
            cancelled,
            sink,
        } = authority;
        let scope = Arc::new(Self {
            runner,
            task_id,
            shell,
            mode,
            permission,
            permissions,
            address,
            setup,
            setup_reviewed,
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
        if matches!(name, "harness_checks_list" | "harness_checks_run") {
            self.runner.refresh_changes().await?;
        }
        let result = match name {
            "harness_checks_list" => self.runner.catalog(self.task_id.as_deref())?,
            "harness_checks_configure" => {
                ensure!(
                    self.setup && self.setup_reviewed,
                    "Check setup must ask the user about functional areas, automatic suites, and coverage cost with harness_question_ask before saving. Continue /checks setup after the user answers."
                );
                ensure!(
                    self.agent.active_count() == 0,
                    "check setup cannot race active agents"
                );
                let request: ConfigureChecks = serde_json::from_value(arguments)?;
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
                self.authorize(
                    if self.permission == PermissionMode::Read {
                        PermissionMode::Write
                    } else {
                        self.permission
                    },
                    target_list,
                    &delivery.authorization,
                )
                .await?;
                ensure!(
                    self.agent.active_count() == 0,
                    "an agent started during check configuration authorization"
                );
                self.runner
                    .configure(request, delivery.lifecycle.as_ref())
                    .await?
            }
            "harness_checks_run" => {
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
                let request = serde_json::from_value(arguments)?;
                let run = self.runner.prepare_request(
                    request,
                    self.task_id.clone(),
                    self.shell,
                    "Agent requested".into(),
                )?;
                let target_list = run
                    .commands
                    .iter()
                    .filter(|check| check.state == super::CheckState::Pending)
                    .map(
                        |check| crate::permissions::matcher::PermissionTarget::Command {
                            command: check.command.clone(),
                            shell: Some(match self.shell {
                                CheckShell::Nushell => {
                                    crate::permissions::shell::CommandShell::Nushell
                                }
                                CheckShell::System => {
                                    crate::permissions::shell::CommandShell::default()
                                }
                            }),
                        },
                    )
                    .collect();
                self.authorize(self.permission, target_list, &delivery.authorization)
                    .await?;
                ensure!(
                    self.agent.active_count() == 0,
                    "an agent started during check authorization"
                );
                json!({"run_id":self.runner.start(run, delivery.lifecycle,Some(Arc::clone(&self.cancelled))).await?})
            }
            "harness_checks_status" => {
                let id = arguments["run_id"].as_str().context("run_id is required")?;
                serde_json::to_value(
                    self.runner
                        .status(id, arguments["wait_ms"].as_u64().unwrap_or(0))
                        .await?,
                )?
            }
            "harness_checks_cancel" => {
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
        mode: PermissionMode,
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

impl CheckRunner {
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
        for id in overrides.enabled.keys().chain(overrides.scopes.keys()) {
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
                    !matches!(scope, super::CheckScope::ChangedCases) || definition.cases.is_some(),
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
        request: RunRequest,
        task_id: Option<String>,
        shell: CheckShell,
        trigger: String,
    ) -> Result<super::CheckRun> {
        let mut snapshot = ConfigSnapshot::load(&self.workspace)?;
        let overrides = self.overrides(task_id.as_deref())?;
        let changes = self.changes.files()?;
        for definition in &mut snapshot.config.definitions {
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        self.prepare(
            snapshot,
            super::CheckAdmission {
                request,
                overrides: overrides.enabled,
                files: changes.clone(),
                cases: None,
                has_scope: changes.is_some(),
                shell,
                trigger,
                task_id,
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
            if let Some(enabled) = overrides.enabled.get(&definition.id) {
                definition.enabled = *enabled;
            }
            if let Some(scope) = overrides.scopes.get(&definition.id) {
                definition.scope = *scope;
            }
        }
        self.configure(
            ConfigureChecks {
                expected_revision: catalog["revision"]
                    .as_str()
                    .context("configuration revision unavailable")?
                    .into(),
                config: snapshot.config,
                helpers: Vec::new(),
            },
            sink,
        )
        .await?;
        self.set_overrides(task_id, Default::default())
    }

    /// Validates setup captures and helper syntax before saving settings without running checks.
    pub(crate) async fn configure(
        &self,
        request: ConfigureChecks,
        sink: Option<&BackendEventSink>,
    ) -> Result<Value> {
        ensure!(
            self.active()?.is_none(),
            "check setup cannot change an active run"
        );
        request.config.validate()?;
        ensure!(request.helpers.len() <= 64, "setup exceeds 64 helpers");
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
            ensure!(helper.source.len() <= 256 * 1024, "helper exceeds 256 KiB");
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
            let temporary = self
                .root
                .join(format!("validate-{}.nu", uuid::Uuid::new_v4()));
            std::fs::write(&temporary, &helper.source)?;
            let result = tokio::process::Command::new("nu")
                .kill_on_drop(true)
                .args([
                    "--no-config-file",
                    "--no-history",
                    "--stdin",
                    "-c",
                    "let script = ($in | str trim); if not (nu-check $script) { exit 1 }",
                ])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn();
            let validation = async {
                let mut child = result.context("start Nushell helper validation")?;
                child
                    .stdin
                    .take()
                    .context("helper validator stdin unavailable")?
                    .write_all(temporary.to_string_lossy().as_bytes())
                    .await?;
                let output = tokio::time::timeout(
                    std::time::Duration::from_secs(30),
                    child.wait_with_output(),
                )
                .await
                .context("helper validation timed out")??;
                ensure!(
                    output.status.success(),
                    "helper syntax validation failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
                Ok::<_, anyhow::Error>(())
            }
            .await;
            std::fs::remove_file(temporary)?;
            validation?;
            helpers.push((destination, previous, helper.source.as_bytes().to_vec()));
        }
        let mut document = original.clone();
        document["checks"] = serde_json::to_value(&request.config)?;
        let replacement = serde_json::to_vec_pretty(&document)?;
        ensure!(
            replacement.len() <= 1024 * 1024,
            ".forge.json exceeds 1 MiB"
        );
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
                request: Default::default(),
                overrides: BTreeMap::new(),
                files: Some(Vec::new()),
                cases: None,
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
        run.completed_at_ms = Some(super::runner::now_ms());
        run.record(
            super::CheckEventKind::ConfigurationSaved,
            None,
            format!(
                "Check configuration saved · {} definitions",
                request.config.definitions.len()
            ),
        );
        self.publish(&run, sink).await?;
        self.catalog(None)
    }
}
