use super::*;

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum ConfigurationCommand {
    Permission,
    Configure,
    Compact,
}

/// Retain the exact permission authority selected before a short host commit.
pub(crate) struct PermissionConfiguration {
    session_id: String,
    provider: String,
    provider_session_id: Option<String>,
    expected_access: crate::session::AccessPolicy,
    expected_approval: crate::session::ApprovalMode,
    expected_task: Option<crate::task::Task>,
    selected_access: Option<crate::session::AccessPolicy>,
    selected_approval: crate::session::ApprovalMode,
    resume: bool,
    operation_id: Option<String>,
}

/// Return owned presentation and any continuation prompt to the service driver.
pub(crate) struct PermissionConfigurationResult {
    pub(crate) delivery: PendingProviderDelivery,
    pub(crate) resume: Option<String>,
    pub(crate) value: Value,
}

/// Capture provider catalog work without retaining the broker across I/O.
pub(crate) struct ConfigurationCatalogJob {
    provider: Arc<Provider>,
    request: ProviderRequest,
    authority: Box<ConfigurationAuthority>,
}

pub(crate) struct ConfigurationCatalogResult {
    authority: Box<ConfigurationAuthority>,
    models: Vec<crate::provider::ProviderModel>,
}

/// Own a provider settings reconciliation independently of the broker lock.
pub(crate) struct ConfigurationReconcileJob {
    provider: Arc<Provider>,
    request: ProviderCatalogRequest,
    selection: crate::settings::PortableSettings,
    authority: Box<ConfigurationAuthority>,
    completed: Option<Arc<std::sync::atomic::AtomicBool>>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ExecutionOwner,
    controller: Arc<crate::task::SessionTaskController>,
}

pub(crate) struct ConfigurationReconcileResult {
    authority: Box<ConfigurationAuthority>,
    completed: Option<Arc<std::sync::atomic::AtomicBool>>,
    uncertainty: Option<ConfigurationWriteGuard>,
}

/// Fence an owner if an in-flight provider settings write loses its completion.
struct ConfigurationWriteGuard {
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ExecutionOwner,
    controller: Arc<crate::task::SessionTaskController>,
    armed: bool,
}

impl Drop for ConfigurationWriteGuard {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        self.controller.mark_native_goal_uncertain();
        let registry = Arc::clone(&self.registry);
        let owner = self.owner.clone();
        if let Err(error) = self.registry.retain_cleanup(
            crate::provider::session_execution::CleanupWorkKind::OwnerFence,
            async move {
                registry
                    .fence(&owner)
                    .await
                    .map_err(crate::provider::session_execution::CleanupWorkFailure::Provider)
            },
        ) {
            self.registry.record_cleanup_failure(error);
        }
    }
}

/// Retire provider-side execution registrations after configuration changes.
pub(crate) struct ConfigurationCleanupJob {
    provider: Arc<Provider>,
    session_id: String,
    authority: Box<ConfigurationAuthority>,
}

impl ConfigurationCleanupJob {
    pub(crate) async fn run(self) -> Result<ConfigurationReconcileResult> {
        self.provider.cleanup_execution(&self.session_id).await?;
        Ok(ConfigurationReconcileResult {
            authority: self.authority,
            completed: None,
            uncertainty: None,
        })
    }
}

pub(super) struct ConfigurationAuthority {
    session: crate::session::HarnessSession,
    pub(super) task: Option<crate::task::Task>,
    provider: Arc<Provider>,
    settings_revision: u64,
}

/// The validated host configuration held across provider reconciliation.
pub(crate) struct ConfigurationCandidate {
    authority: Box<ConfigurationAuthority>,
    params: Value,
    configured: Box<crate::session::HarnessSession>,
    access: Option<crate::session::AccessPolicy>,
    check_timing: Option<crate::checks::CheckTiming>,
    automatic_checks: Option<bool>,
    subagent_tools: Option<bool>,
    lsp_tools: Option<bool>,
    session_tools: Option<bool>,
    status_thoughts: Option<bool>,
    delegation_registry_changed: bool,
    update_model_preference: bool,
    resource_selection: Option<crate::settings::PortableSettings>,
    reconciled: Arc<std::sync::atomic::AtomicBool>,
    uncertainty: Option<ConfigurationWriteGuard>,
}

pub(crate) struct ConfigurationPreparation {
    authority: Box<ConfigurationAuthority>,
    params: Value,
    resource_selection: Option<crate::settings::PortableSettings>,
}

pub(crate) struct ConfigurationCommit {
    pub(crate) value: Value,
    pub(crate) events: Vec<SessionEvent>,
    pub(crate) cleanup: Option<ConfigurationCleanupJob>,
}

pub(crate) struct ConfigurationMcpNotices {
    pub(crate) deliveries: Vec<PendingProviderDelivery>,
    authority: Box<ConfigurationAuthority>,
    notices: Option<std::collections::HashMap<String, String>>,
}

impl ConfigurationCatalogJob {
    pub(crate) async fn run(self) -> Result<ConfigurationCatalogResult> {
        let models = self.provider.model_list(self.request).await?;
        Ok(ConfigurationCatalogResult {
            authority: self.authority,
            models,
        })
    }
}

impl ConfigurationReconcileJob {
    pub(crate) async fn run(self) -> Result<ConfigurationReconcileResult> {
        let lane = self.controller.native_goal_lane().lock_owned().await;
        self.controller.ensure_native_goal_certain()?;
        let guard = ConfigurationWriteGuard {
            registry: self.registry,
            owner: self.owner,
            controller: self.controller,
            armed: true,
        };
        // The spawned worker retains the transport lane even if its caller is
        // cancelled, so generation recovery cannot race a late provider write.
        let worker = guard.registry.retain_cleanup(
            crate::provider::session_execution::CleanupWorkKind::ConfigurationWrite,
            async move {
                let _lane = lane;
                self.provider
                    .reconcile_settings(self.request, &self.selection)
                    .await
                    .map_err(crate::provider::session_execution::CleanupWorkFailure::Provider)
            },
        )?;
        worker
            .await
            .map_err(|error| {
                anyhow::Error::new(error).context(crate::provider::ProviderError::DeliveryUncertain)
            })?
            .map_err(|error| {
                anyhow::Error::new(error).context(crate::provider::ProviderError::DeliveryUncertain)
            })?;
        guard.registry.collect_cleanup().await.map_err(|error| {
            anyhow::Error::new(error).context(crate::provider::ProviderError::DeliveryUncertain)
        })?;
        Ok(ConfigurationReconcileResult {
            authority: self.authority,
            completed: self.completed,
            uncertainty: Some(guard),
        })
    }
}

/// Preserve the paused exchange and its postprocessing while permission work resumes.
pub(crate) enum PermissionResume {
    NoWork,
    Staged {
        interaction: Box<foreground_dispatch::StagedInteraction>,
        clarification: Option<foreground_dispatch::ClarificationContext>,
        planning: Option<foreground_dispatch::PlanningContext>,
    },
}

impl ConfigurationPreparation {
    pub(crate) fn needs_catalog(&self, broker: &HarnessBroker) -> bool {
        let p = &self.params;
        let selection = p.get("model").and_then(Value::as_str).is_some()
            || p.get("effort").and_then(Value::as_str).is_some()
            || p.get("context_window").and_then(Value::as_str).is_some();
        let tier = p
            .get("service_tier")
            .and_then(|value| {
                serde_json::from_value::<crate::provider::ServiceTier>(value.clone()).ok()
            })
            .unwrap_or(broker.session.service_tier);
        let nonstandard_tier = tier != crate::provider::ServiceTier::Standard;
        p.get("plan_executor_model").is_some()
            || p.get("plan_executor_effort").is_some()
            || p.get("plan_executor_enabled").and_then(Value::as_bool) == Some(true)
            || (p.get("validate").and_then(Value::as_bool) == Some(true) && selection)
            || (nonstandard_tier
                && (p.get("service_tier").is_some()
                    || p.get("model").and_then(Value::as_str).is_some()))
    }
}

impl HarnessBroker {
    /// Complete a durable configuration before replacing a fenced provider generation.
    pub(crate) fn recover_pending_configuration(&mut self) -> Result<bool> {
        let controller = self.task_controller.clone();
        controller.with_task_snapshot(|_| {
            if !self
                .store
                .recover_configuration_intent(&self.data_root, &self.session.id)?
            {
                return Ok(false);
            }
            self.session = self
                .store
                .load_session(&self.session.id)?
                .context("recovered configuration session is missing")?;
            self.permission_coordinator
                .configure_timeout(&self.session.id, self.session.approval_timeout)?;
            let snapshot = crate::settings::SettingsStore::read_only(&self.data_root)?.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?;
            self.agent_coordinator
                .set_enabled(snapshot.desired.subagent_tools.unwrap_or(false));
            self.lsp.set_enabled(
                &self.session.id,
                snapshot.desired.lsp_tools.unwrap_or(false),
            );
            self.observer.status_thoughts.store(
                snapshot.desired.status_thoughts.unwrap_or(true),
                Ordering::Relaxed,
            );
            Ok(true)
        })
    }
    /// Stage operation progress for delivery outside the broker lock.
    pub(crate) fn prepare_configuration_progress(
        &mut self,
        operation_id: Option<&str>,
        state: ConfigurationProgressState,
    ) -> Result<Option<PendingProviderDelivery>> {
        operation_id
            .map(str::to_owned)
            .or_else(|| self.active_operation_id.clone())
            .map(|operation_id| {
                self.prepare_provider_delivery(AppliedProviderEvent::Update(
                    SessionUpdate::ConfigurationProgress {
                        operation_id,
                        state,
                    },
                ))
            })
            .transpose()
    }

    /// Confirm only the settings revision that this operation actually applied.
    pub(crate) fn confirm_configuration_application(&mut self, action: &Value) -> Result<()> {
        let Some(revision) = action
            .pointer("/config/settings_revision")
            .and_then(Value::as_u64)
        else {
            return Ok(());
        };
        let Some(operation) = action
            .get("operation_id")
            .and_then(Value::as_str)
            .or_else(|| action.pointer("/_command/id").and_then(Value::as_str))
            .or(self.active_operation_id.as_deref())
        else {
            return Ok(());
        };
        let settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let snapshot = settings.get(
            &self.session.workspace,
            &self.session.id,
            &self.session.provider,
        )?;
        anyhow::ensure!(
            snapshot.revision == revision,
            "settings changed before application confirmation"
        );
        let mut effective = snapshot.desired;
        effective.access = Some(self.session.access.clone());
        effective.shell = Some(self.session.shell);
        effective.plan_compact = Some(self.session.plan_compact);
        effective.approval_timeout = Some(self.session.approval_timeout);
        effective.tool_timeout = Some(self.session.tool_timeout);
        settings.confirm_application(&self.session.id, operation, revision, effective)?;
        Ok(())
    }

    /// Stage diagnostics for unavailable MCP resources in their original order.
    pub(crate) fn prepare_configuration_mcp_notices(&mut self) -> Result<ConfigurationMcpNotices> {
        let request = self.provider_catalog_request()?;
        let declaration = self.provider.descriptor();
        let Some(target) = declaration
            .capability
            .integration
            .resource_target
            .as_deref()
        else {
            return Ok(ConfigurationMcpNotices {
                deliveries: Vec::new(),
                authority: self.capture_configuration_authority()?,
                notices: None,
            });
        };
        let resources = crate::settings::SessionResources::load(
            &self.data_root,
            &request,
            &self.session.provider,
            target,
        )?;
        let mut deliveries = Vec::new();
        for (id, message) in &resources.launch_notice {
            if self.mcp_notice.get(id) == Some(message) {
                continue;
            }
            self.trace.record(
                &self.session.id,
                "mcp.start_skipped",
                json!({"resource_id":id,"reason":message}),
            );
            deliveries.push(
                self.prepare_provider_delivery(AppliedProviderEvent::Update(
                    SessionUpdate::Diagnostic {
                        diagnostic: crate::provider::ProviderDiagnostic {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::SanitizedMessage::new(message.clone())
                                .expect("host diagnostic is nonempty"),
                            fatal: false,
                        },
                    },
                ))?,
            );
        }
        Ok(ConfigurationMcpNotices {
            deliveries,
            authority: self.capture_configuration_authority()?,
            notices: Some(resources.launch_notice),
        })
    }

    pub(crate) fn commit_configuration_mcp_notices(
        &mut self,
        notices: ConfigurationMcpNotices,
    ) -> Result<()> {
        self.validate_configuration_authority(&notices.authority)?;
        if let Some(notices) = notices.notices {
            self.mcp_notice = notices;
        }
        Ok(())
    }

    pub(crate) fn prepare_configuration(
        &self,
        mut params: Value,
    ) -> Result<Box<ConfigurationPreparation>> {
        anyhow::ensure!(
            self.store
                .pending_configuration_intent(&self.session.id)?
                .is_none(),
            "session configuration requires durable recovery"
        );
        let mut resource_selection = None;
        if params.get("settings_revision").is_some() {
            let settings = crate::settings::SettingsStore::open(&self.data_root)?.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?;
            anyhow::ensure!(
                params["settings_revision"].as_u64() == Some(settings.revision),
                "settings revision was superseded"
            );
            params["access"] =
                serde_json::to_value(settings.desired.access.clone().unwrap_or_default())?;
            params["shell"] = serde_json::to_value(settings.desired.shell.unwrap_or_default())?;
            params["plan_compact"] = json!(
                settings.desired.plan_compact.unwrap_or(false) && self.capability.native_compact
            );
            params["approval_timeout"] =
                serde_json::to_value(settings.desired.approval_timeout.unwrap_or_default())?;
            params["tool_timeout"] =
                serde_json::to_value(settings.desired.tool_timeout.unwrap_or_default())?;

            let desired = serde_json::to_value(&settings.desired)?;
            for (key, value) in desired.as_object().context("settings must be an object")? {
                if !value.is_null() && key != "skill" && key != "mcp" && key != "plan_compact" {
                    params[key] = value.clone();
                }
            }
            resource_selection = Some(settings.desired);
        }
        Ok(Box::new(ConfigurationPreparation {
            authority: self.capture_configuration_authority()?,
            params,
            resource_selection,
        }))
    }

    pub(crate) fn build_configuration_candidate(
        &self,
        preparation: Box<ConfigurationPreparation>,
        models: &[crate::provider::ProviderModel],
    ) -> Result<Box<ConfigurationCandidate>> {
        self.validate_configuration_authority(&preparation.authority)?;
        let ConfigurationPreparation {
            authority,
            params,
            resource_selection,
        } = *preparation;
        let mut configured = Box::new(self.session.clone());
        let check_timing = params
            .get("check_timing")
            .map(|value| serde_json::from_value::<crate::checks::CheckTiming>(value.clone()))
            .transpose()?;
        let automatic_checks = params
            .get("automatic_checks")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        if let Some(value) = params.get("auto_compaction") {
            let policy: crate::session::AutoCompaction = serde_json::from_value(value.clone())?;
            policy.validate(configured.context_usage.as_ref())?;
            anyhow::ensure!(
                !policy.enabled || self.capability.native_compact,
                "This provider does not support automatic compaction"
            );
            configured.auto_compaction = policy;
        }
        configured.approval_timeout = self
            .permission_coordinator
            .approval_timeout(&self.session.id)?;
        let access = params
            .get("access")
            .map(|value| serde_json::from_value::<crate::session::AccessPolicy>(value.clone()))
            .transpose()?
            .map(crate::session::AccessPolicy::validate)
            .transpose()?;
        if let Some(value) = params.get("shell") {
            configured.shell = serde_json::from_value(value.clone())?;
            configured.shell.executable()?;
        }
        if let Some(value) = params.get("approval_mode") {
            configured.approval_mode = serde_json::from_value(value.clone())?;
        }
        if let Some(value) = params.get("approval_timeout") {
            configured.approval_timeout =
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .validate()?;
        }
        if let Some(value) = params.get("tool_timeout") {
            configured.tool_timeout =
                serde_json::from_value::<crate::session::InputTimeout>(value.clone())?
                    .require_expiry()?;
        }
        let requested_service_tier = params
            .get("service_tier")
            .map(|value| serde_json::from_value::<crate::provider::ServiceTier>(value.clone()))
            .transpose()?;
        if let Some(tier) = requested_service_tier {
            tier.validate(&self.capability)?;
        }
        let requested_model = params.get("model").and_then(Value::as_str);
        let requested_effort = params.get("effort").and_then(Value::as_str);
        let requested_context_window = params.get("context_window").and_then(Value::as_str);
        let validate_selection = params.get("validate").and_then(Value::as_bool) == Some(true);
        let mut plan_executor = self.session.plan_executor.clone();
        let mut plan_compact = self.session.plan_compact;
        if let Some(enabled) = params.get("plan_compact").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.native_compact,
                    "current provider does not support context compaction"
                );
            }
            plan_compact = enabled;
        }
        if let Some(enabled) = params.get("plan_executor_enabled").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.model_selection,
                    "current provider does not support model selection"
                );
                if plan_executor.model.is_none() {
                    let active_model = self
                        .session
                        .resolved_model
                        .clone()
                        .unwrap_or_else(|| self.session.model.clone());
                    let selected_model = if active_model == "default" {
                        models
                            .iter()
                            .find(|candidate| candidate.is_default)
                            .context("the provider has no default model")?
                            .id
                            .clone()
                    } else {
                        active_model
                    };
                    plan_executor.model = Some(selected_model);
                    plan_executor.effort = Some(self.session.effort.clone());
                }
            }
            plan_executor.enabled = enabled;
        }
        if let Some(model) = params.get("plan_executor_model").and_then(Value::as_str) {
            anyhow::ensure!(
                !model.trim().is_empty(),
                "plan executor model cannot be empty"
            );
            plan_executor.model = Some(model.to_owned());
        }
        if let Some(effort) = params.get("plan_executor_effort").and_then(Value::as_str) {
            anyhow::ensure!(
                !effort.trim().is_empty(),
                "plan executor thinking cannot be empty"
            );
            plan_executor.effort = Some(effort.to_owned());
        }
        if params.get("plan_executor_model").is_some()
            || params.get("plan_executor_effort").is_some()
            || params.get("plan_executor_enabled").and_then(Value::as_bool) == Some(true)
        {
            self.validate_configuration_executor(&plan_executor, models)?;
        }
        let selected_tier = requested_service_tier.unwrap_or(self.session.service_tier);
        let validate_tier = selected_tier != crate::provider::ServiceTier::Standard
            && (requested_service_tier.is_some() || requested_model.is_some());
        if validate_tier
            || (validate_selection
                && (requested_model.is_some()
                    || requested_effort.is_some()
                    || requested_context_window.is_some()))
        {
            let model_list = models;
            let selected_model = requested_model.unwrap_or_else(|| {
                self.session
                    .resolved_model
                    .as_deref()
                    .unwrap_or(&self.session.model)
            });
            let target_model = model_list.iter().find(|candidate| {
                if selected_model == "default" {
                    candidate.is_default
                } else {
                    candidate.id == selected_model
                }
            });
            if let Some(model) = requested_model {
                anyhow::ensure!(
                    target_model
                        .is_some_and(|candidate| model == "default" || candidate.id == model),
                    "model {model} is unavailable for this provider"
                );
            }
            if validate_tier {
                selected_tier.validate_model(
                    target_model.context("the selected model is unavailable for this provider")?,
                )?;
            }
            if let Some(effort) = requested_effort {
                let model =
                    target_model.context("the active model is unavailable for this provider")?;
                if !model.reasoning.is_empty() {
                    anyhow::ensure!(
                        model.reasoning.iter().any(|candidate| candidate == effort),
                        "reasoning effort {effort} is unavailable for model {}",
                        model.id
                    );
                }
            }
            if let Some(context_window) = requested_context_window {
                let model =
                    target_model.context("the active model is unavailable for this provider")?;
                if !model.context_window.is_empty() {
                    anyhow::ensure!(
                        model
                            .context_window
                            .iter()
                            .any(|candidate| candidate.id == context_window),
                        "context window {context_window} is unavailable for model {}",
                        model.id
                    );
                }
            }
        }
        if let Some(model) = params.get("model").and_then(Value::as_str) {
            anyhow::ensure!(!model.trim().is_empty(), "model cannot be empty");
            configured.model = model.to_owned();
            configured.resolved_model = (model != "default").then(|| model.to_owned());
        }
        if let Some(effort) = params.get("effort").and_then(Value::as_str) {
            anyhow::ensure!(!effort.trim().is_empty(), "effort cannot be empty");
            configured.effort = effort.to_owned();
        }
        if params.get("context_window").is_some() {
            configured.context_window = params
                .get("context_window")
                .and_then(Value::as_str)
                .map(str::to_owned);
        }
        if let Some(service_tier) = requested_service_tier {
            configured.service_tier = service_tier;
        }
        configured.plan_executor = plan_executor;
        configured.plan_compact = plan_compact;
        let subagent_tools = params.get("subagent_tools").and_then(Value::as_bool);
        let lsp_tools = params.get("lsp_tools").and_then(Value::as_bool);
        let session_tools = params
            .get("session_tools")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        let status_thoughts = params
            .get("status_thoughts")
            .map(|value| serde_json::from_value::<bool>(value.clone()))
            .transpose()?;
        let delegation_registry_changed = subagent_tools
            .is_some_and(|enabled| enabled != self.agent_coordinator.enabled())
            || lsp_tools.is_some_and(|enabled| enabled != self.lsp.enabled(&self.session.id))
            || session_tools.is_some();
        if let Some(access) = &access {
            configured.access = access.clone();
        }
        configured.access = forge_providers::registry::resolve(
            &forge_providers::registry::select(&configured.provider)?,
        )?
        .capability
        .resolve_access(configured.access.clone());
        let update_model_preference = requested_model.is_some()
            || requested_effort.is_some()
            || params.get("context_window").is_some();
        let reconciled = Arc::new(std::sync::atomic::AtomicBool::new(
            resource_selection.is_none(),
        ));
        Ok(Box::new(ConfigurationCandidate {
            authority,
            params,
            configured,
            access,
            check_timing,
            automatic_checks,
            subagent_tools,
            lsp_tools,
            session_tools,
            status_thoughts,
            delegation_registry_changed,
            update_model_preference,
            resource_selection,
            reconciled,
            uncertainty: None,
        }))
    }

    pub(crate) fn prepare_candidate_reconcile(
        &self,
        candidate: &ConfigurationCandidate,
    ) -> Result<Option<ConfigurationReconcileJob>> {
        self.validate_configuration_authority(&candidate.authority)?;
        candidate
            .resource_selection
            .as_ref()
            .map(|selection| {
                let mut job = self.prepare_configuration_reconcile(
                    ProviderCatalogRequest {
                        execution_owner: None,
                        harness_session_id: candidate.configured.id.clone(),
                        workspace: candidate.configured.workspace.clone(),
                        access: candidate.configured.access.clone(),
                        approval_mode: candidate.configured.approval_mode,
                        provider_session_id: candidate
                            .configured
                            .provider_session_id
                            .clone()
                            .map(TryInto::try_into)
                            .transpose()?,
                    },
                    selection.clone(),
                )?;
                job.completed = Some(Arc::clone(&candidate.reconciled));
                Ok(job)
            })
            .transpose()
    }

    pub(crate) fn commit_configuration_candidate(
        &mut self,
        mut candidate: Box<ConfigurationCandidate>,
    ) -> Result<ConfigurationCommit> {
        anyhow::ensure!(
            candidate.reconciled.load(Ordering::Acquire),
            "configuration provider reconciliation has not completed"
        );
        if let Err(error) = self.validate_configuration_authority(&candidate.authority) {
            if candidate.uncertainty.is_some() {
                return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
            }
            return Err(error);
        }
        let mut uncertainty = candidate.uncertainty.take();
        let expected_task = candidate.authority.task.clone();
        let controller = self.task_controller.clone();
        let expected_revision = candidate.authority.settings_revision;
        let workspace = candidate.configured.workspace.clone();
        let session_id = candidate.configured.id.clone();
        let provider_id = candidate.configured.provider.clone();
        let scope = if candidate.params.get("auto_compaction").is_some()
            || candidate.params["session_policy"] == true
        {
            crate::settings::SettingScope::Session
        } else {
            crate::settings::SettingScope::Global
        };
        let mut portable = crate::settings::PortableSettings::default();
        let mut changed = false;
        if candidate.params.get("settings_revision").is_none() {
            if let Some(access) = &candidate.access {
                portable.access = Some(access.clone());
                changed = true;
            }
            if candidate.params.get("shell").is_some() {
                portable.shell = Some(candidate.configured.shell);
                changed = true;
            }
            if let Some(value) = candidate.check_timing {
                portable.check_timing = Some(value);
                changed = true;
            }
            if let Some(value) = candidate.automatic_checks {
                portable.automatic_checks = Some(value);
                changed = true;
            }
            if candidate.params.get("auto_compaction").is_some() {
                portable.auto_compaction = Some(candidate.configured.auto_compaction.clone());
                changed = true;
            }
            if candidate.params.get("plan_compact").is_some() {
                portable.plan_compact = Some(candidate.configured.plan_compact);
                changed = true;
            }
            if candidate.params.get("approval_timeout").is_some() {
                portable.approval_timeout = Some(candidate.configured.approval_timeout);
                changed = true;
            }
            if candidate.params.get("tool_timeout").is_some() {
                portable.tool_timeout = Some(candidate.configured.tool_timeout);
                changed = true;
            }
            if let Some(enabled) = candidate.subagent_tools {
                portable.subagent_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = candidate.lsp_tools {
                portable.lsp_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = candidate.session_tools {
                portable.session_tools = Some(enabled);
                changed = true;
            }
            if let Some(enabled) = candidate.status_thoughts {
                portable.status_thoughts = Some(enabled);
                changed = true;
            }
        }
        let provider_setting = (candidate.params.get("settings_revision").is_none() && (candidate.update_model_preference || candidate.params.get("service_tier").is_some() || candidate.params.get("plan_executor_enabled").is_some())).then(|| json!({"model":candidate.configured.model,"effort":candidate.configured.effort,"context_window":candidate.configured.context_window,"service_tier":candidate.configured.service_tier,"plan_executor":candidate.configured.plan_executor}));
        let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let target = crate::settings::SettingTarget {
            scope,
            workspace: &workspace,
            session: &session_id,
        };
        let intent = crate::storage::configuration::ConfigurationIntent {
            id: format!("configuration:{}", uuid::Uuid::new_v4()),
            session: session_id.clone(),
            workspace: workspace.clone(),
            provider: provider_id.clone(),
            revision: expected_revision,
            scope,
            portable: changed.then_some(portable.clone()),
            provider_setting: provider_setting.clone(),
        };
        let committed = controller.with_task_snapshot(|current| {
        anyhow::ensure!(foreground_failure::same_retraction_task(current.as_ref(), expected_task.as_ref()), "configuration task authority changed before commit");
        settings.commit_configuration(expected_revision, target, changed.then_some(&portable), provider_setting.as_ref().map(|value| (provider_id.as_str(), value)), Some(&intent), || {
        let ConfigurationCandidate {
            params,
            configured,
            subagent_tools,
            lsp_tools,
            status_thoughts,
            delegation_registry_changed,
            update_model_preference,
            ..
        } = *candidate;
        let previous = std::mem::replace(&mut self.session, *configured);
        self.permission_coordinator
            .configure_timeout(&self.session.id, self.session.approval_timeout)?;
        self.session.approval_timeout = self.permission_coordinator.approval_timeout(&self.session.id)?;
        self.session.acquire_lease(&self.client_id)?;
        self.session.updated_at_ms = self.clock.now_ms();
        let prior_preference = self.store.load_preference(&self.session.workspace, &self.session.provider)?;
        let preference = if self.session.background.is_some() { None } else {
            let mut preference = preference_for_session(&self.session, prior_preference.clone());
            if !update_model_preference && let Some(previous) = prior_preference {
                preference.model = previous.model; preference.effort = previous.effort; preference.model_setting = previous.model_setting;
            }
            Some(preference)
        };
        if let Err(error) = self.store.commit_configuration_intent(&self.session, &self.client_id, preference.as_ref(), &intent) {
            self.permission_coordinator
                .configure_timeout(&previous.id, previous.approval_timeout)?;
            self.session = previous;
            return Err(error);
        }
        self.session.name = self.store.load_session(&self.session.id)?.context("owned session is missing")?.name;
        if let Some(enabled) = subagent_tools {
            self.agent_coordinator.set_enabled(enabled);
        }
        if let Some(enabled) = lsp_tools {
            self.lsp.set_enabled(&self.session.id, enabled);
        }
        if let Some(enabled) = status_thoughts {
            self.observer
                .status_thoughts
                .store(enabled, Ordering::Relaxed);
        }
        Ok((serde_json::to_value(self.session_view()?)?, vec![self.event("session_configured", json!({"session": self.session_view()?, "access_abbreviation": self.session.access.abbreviation()}))?], delegation_registry_changed && params.get("settings_revision").is_none()))
        })
        });
        let (value, events, needs_cleanup) = match committed {
            Ok(value) => value,
            Err(error) => {
                if self
                    .store
                    .pending_configuration_intent(&session_id)?
                    .is_some()
                {
                    self.task_controller.mark_native_goal_uncertain();
                    let registry = self.execution_registry.clone();
                    let owner = crate::provider::ExecutionOwner::root(&session_id);
                    if let Err(cleanup) = self.execution_registry.retain_cleanup(
                        crate::provider::session_execution::CleanupWorkKind::OwnerFence,
                        async move {
                            registry.fence(&owner).await.map_err(
                                crate::provider::session_execution::CleanupWorkFailure::Provider,
                            )
                        },
                    ) {
                        self.execution_registry.record_cleanup_failure(cleanup);
                    }
                    return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
                }
                return Err(error);
            }
        };
        if let Err(error) = self.store.clear_configuration_intent(&intent) {
            self.task_controller.mark_native_goal_uncertain();
            let registry = self.execution_registry.clone();
            let owner = crate::provider::ExecutionOwner::root(&session_id);
            if let Err(cleanup) = self.execution_registry.retain_cleanup(
                crate::provider::session_execution::CleanupWorkKind::OwnerFence,
                async move {
                    registry
                        .fence(&owner)
                        .await
                        .map_err(crate::provider::session_execution::CleanupWorkFailure::Provider)
                },
            ) {
                self.execution_registry.record_cleanup_failure(cleanup);
            }
            return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
        }
        let cleanup = if needs_cleanup {
            Some(self.prepare_configuration_cleanup()?)
        } else {
            None
        };
        if let Some(guard) = uncertainty.as_mut() {
            guard.armed = false;
        }
        Ok(ConfigurationCommit {
            value,
            events,
            cleanup,
        })
    }

    fn validate_configuration_executor(
        &self,
        executor: &PlanExecutor,
        model_list: &[crate::provider::ProviderModel],
    ) -> Result<()> {
        anyhow::ensure!(
            self.capability.model_selection,
            "current provider does not support model selection"
        );
        let model = executor
            .model
            .as_deref()
            .context("plan executor model is not configured")?;
        let effort = executor
            .effort
            .as_deref()
            .context("plan executor thinking is not configured")?;
        let selected = model_list
            .iter()
            .find(|candidate| candidate.id == model)
            .with_context(|| {
                format!("plan executor model {model} is unavailable for this provider")
            })?;
        anyhow::ensure!(
            selected.reasoning.is_empty()
                || selected
                    .reasoning
                    .iter()
                    .any(|candidate| candidate == effort),
            "plan executor thinking {effort} is unavailable for model {model}"
        );
        Ok(())
    }
}

impl HarnessBroker {
    /// Capture catalog work and the authority that must still own its result.
    pub(crate) fn prepare_configuration_catalog(&self) -> Result<ConfigurationCatalogJob> {
        self.prepare_configuration_catalog_for_mode(PromptMode::Chat)
    }

    pub(crate) fn prepare_configuration_catalog_for_mode(
        &self,
        mode: PromptMode,
    ) -> Result<ConfigurationCatalogJob> {
        Ok(ConfigurationCatalogJob {
            provider: Arc::clone(&self.provider),
            request: self.provider_request(ProviderInput::from_text(""), mode)?,
            authority: self.capture_configuration_authority()?,
        })
    }

    /// Reject a catalog result if its session, provider, task, or selected model changed.
    pub(crate) fn commit_configuration_catalog(
        &self,
        result: ConfigurationCatalogResult,
    ) -> Result<Vec<crate::provider::ProviderModel>> {
        self.validate_configuration_authority(&result.authority)?;
        Ok(result.models)
    }

    pub(crate) fn prepare_configuration_reconcile(
        &self,
        request: ProviderCatalogRequest,
        selection: crate::settings::PortableSettings,
    ) -> Result<ConfigurationReconcileJob> {
        self.task_controller.ensure_native_goal_certain()?;
        Ok(ConfigurationReconcileJob {
            provider: Arc::clone(&self.provider),
            request,
            selection,
            authority: self.capture_configuration_authority()?,
            completed: None,
            registry: Arc::clone(&self.execution_registry),
            owner: crate::provider::ExecutionOwner::root(&self.session.id),
            controller: Arc::clone(&self.task_controller),
        })
    }

    pub(crate) fn commit_configuration_reconcile(
        &self,
        mut result: ConfigurationReconcileResult,
    ) -> Result<()> {
        if let Err(error) = self.validate_configuration_authority(&result.authority) {
            if result.uncertainty.is_some() {
                return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
            }
            return Err(error);
        }
        if let Some(completed) = result.completed.take() {
            completed.store(true, Ordering::Release);
        }
        if let Some(guard) = result.uncertainty.as_mut() {
            guard.armed = false;
        }
        Ok(())
    }

    /// Hold a successful provider write fenced until the corresponding host commit succeeds.
    pub(crate) fn certify_candidate_reconcile(
        &self,
        candidate: &mut ConfigurationCandidate,
        mut result: ConfigurationReconcileResult,
    ) -> Result<()> {
        if let Err(error) = self.validate_configuration_authority(&result.authority) {
            return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
        }
        self.validate_configuration_authority(&candidate.authority)
            .map_err(|error| error.context(crate::provider::ProviderError::DeliveryUncertain))?;
        anyhow::ensure!(
            result.uncertainty.is_some(),
            "configuration reconciliation has no write guard"
        );
        let completed = result
            .completed
            .take()
            .context("configuration reconciliation has no candidate receipt")?;
        anyhow::ensure!(
            Arc::ptr_eq(&completed, &candidate.reconciled),
            "configuration reconciliation belongs to another candidate"
        );
        completed.store(true, Ordering::Release);
        candidate.uncertainty = result.uncertainty.take();
        Ok(())
    }

    pub(crate) fn prepare_configuration_cleanup(&self) -> Result<ConfigurationCleanupJob> {
        Ok(ConfigurationCleanupJob {
            provider: Arc::clone(&self.provider),
            session_id: self.session.id.clone(),
            authority: self.capture_configuration_authority()?,
        })
    }

    pub(super) fn capture_configuration_authority(&self) -> Result<Box<ConfigurationAuthority>> {
        let revision = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .revision;
        Ok(Box::new(ConfigurationAuthority {
            session: self.session.clone(),
            task: self.task_controller.current()?,
            provider: Arc::clone(&self.provider),
            settings_revision: revision,
        }))
    }

    pub(super) fn validate_configuration_authority(
        &self,
        authority: &ConfigurationAuthority,
    ) -> Result<()> {
        let expected = &authority.session;
        let current = &self.session;
        anyhow::ensure!(
            current.id == expected.id
                && Arc::ptr_eq(&self.provider, &authority.provider)
                && current.workspace == expected.workspace
                && current.provider == expected.provider
                && current.provider_session_id == expected.provider_session_id
                && current.model == expected.model
                && current.resolved_model == expected.resolved_model
                && current.effort == expected.effort
                && current.context_window == expected.context_window
                && current.service_tier == expected.service_tier
                && current.access == expected.access
                && current.approval_mode == expected.approval_mode
                && current.plan_executor == expected.plan_executor
                && current.plan_compact == expected.plan_compact
                && current.auto_compaction == expected.auto_compaction
                && current.context_usage == expected.context_usage
                && current.shell == expected.shell
                && current.approval_timeout == expected.approval_timeout
                && current.tool_timeout == expected.tool_timeout
                && crate::settings::SettingsStore::read_only(&self.data_root)?
                    .get(&current.workspace, &current.id, &current.provider)?
                    .revision
                    == authority.settings_revision
                && foreground_failure::same_retraction_task(
                    self.task_controller.current()?.as_ref(),
                    authority.task.as_ref()
                ),
            "configuration authority changed during provider work"
        );
        Ok(())
    }
    /// Capture a permission change without mutating provider or session state.
    pub(crate) fn prepare_permission_configuration(
        &self,
        action: &Value,
    ) -> Result<PermissionConfiguration> {
        anyhow::ensure!(
            self.store
                .pending_configuration_intent(&self.session.id)?
                .is_none(),
            "session configuration requires durable recovery"
        );
        let change: crate::task::PermissionChange = serde_json::from_value(
            action
                .get("change")
                .cloned()
                .context("permission change is required")?,
        )?;
        let mut access = self.session.access.clone();
        let mut approval = self.session.approval_mode;
        if let Some(selection) = action.get("permission_selection") {
            let selection: crate::task::PermissionSelection =
                serde_json::from_value(selection.clone())?;
            access.sandbox = selection.sandbox;
            access.read_access = selection.read_access;
            access.write_access = selection.write_access;
            approval = selection.approval_mode;
        } else {
            change.apply(
                &mut access,
                &mut approval,
                forge_providers::registry::resolve(&forge_providers::registry::select(
                    &self.session.provider,
                )?)?
                .capability
                .supports_sandbox(),
            );
        }
        if matches!(
            change,
            crate::task::PermissionChange::ToggleReadScope
                | crate::task::PermissionChange::ToggleWriteEnabled
                | crate::task::PermissionChange::ToggleWriteScope
        ) && let Some(preference) = crate::settings::SettingsStore::read_only(&self.data_root)?
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired
            .access
        {
            access.sandbox = preference.sandbox;
        }
        let selected_access = if matches!(change, crate::task::PermissionChange::SetApproval { .. })
        {
            None
        } else {
            Some(access.validate()?)
        };
        Ok(PermissionConfiguration {
            session_id: self.session.id.clone(),
            provider: self.session.provider.clone(),
            provider_session_id: self.session.provider_session_id.clone(),
            expected_access: self.session.access.clone(),
            expected_approval: self.session.approval_mode,
            expected_task: self.task_controller.current()?,
            selected_access,
            selected_approval: approval,
            resume: action
                .get("resume")
                .and_then(Value::as_bool)
                .unwrap_or(false),
            operation_id: action
                .get("operation_id")
                .and_then(Value::as_str)
                .or_else(|| action.pointer("/_command/id").and_then(Value::as_str))
                .or(self.active_operation_id.as_deref())
                .map(str::to_owned),
        })
    }

    /// Commit the captured permission change under the still-current task authority.
    pub(crate) fn commit_permission_configuration(
        &mut self,
        captured: PermissionConfiguration,
    ) -> Result<PermissionConfigurationResult> {
        let controller = self.task_controller.clone();
        controller.with_task_snapshot(|current| {
            anyhow::ensure!(foreground_failure::same_retraction_task(current.as_ref(), captured.expected_task.as_ref())
                && self.session.id == captured.session_id && self.session.provider == captured.provider
                && self.session.provider_session_id == captured.provider_session_id
                && self.session.access == captured.expected_access && self.session.approval_mode == captured.expected_approval,
                "permission authority changed before configuration commit");
            let previous = self.session.clone();
            let previous_access = previous.access.abbreviation().to_owned();
            let previous_approval = previous.approval_mode;
            let mut configured = previous.clone();
            configured.approval_mode = captured.selected_approval;
            configured.approval_timeout = self.permission_coordinator.approval_timeout(&configured.id)?;
            if let Some(access) = &captured.selected_access { configured.access = access.clone(); }
            configured.access = forge_providers::registry::resolve(&forge_providers::registry::select(&configured.provider)?)?.capability.resolve_access(configured.access);
            self.session = configured;
            self.permission_coordinator.configure_timeout(&self.session.id, self.session.approval_timeout)?;
            if let Err(error) = self.save_session() {
                self.permission_coordinator.configure_timeout(&previous.id, previous.approval_timeout)?;
                self.session = previous;
                return Err(error);
            }
            self.save_preference(false)?;
            if let Some(access) = captured.selected_access {
                let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
                let portable = crate::settings::PortableSettings { access: Some(access), ..Default::default() };
                settings.patch(crate::settings::SettingTarget { scope: crate::settings::SettingScope::Session,
                    workspace: &self.session.workspace, session: &self.session.id }, &portable)?;
            }
            let update = SessionUpdate::Configuration { session: Box::new(self.session_view()?),
                access_abbreviation: self.session.access.abbreviation().into(), operation_id: captured.operation_id,
                catalog: Box::new(self.provider_catalog_request()?) };
            let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Update(update))?;
            let resume = if captured.resume {
                let previous = self.store.latest_exchange(&self.session.id)?.context("interrupted exchange is missing")?;
                if previous.is_finished() {
                    None
                } else {
                    let notice = format!("The user changed filesystem access from {} to {} and approval mode from {} to {}. Continue the interrupted work under the updated policy. Saved approval rules still apply. Preserve completed work. Before retrying an interrupted operation, inspect its recorded outcome and any affected files.",
                        previous_access, self.session.access.abbreviation(), previous_approval.label(), captured.selected_approval.label());
                    Some(if previous.elicitation().is_some() { notice } else { format!("{notice}\n\nOriginal request: {}", previous.prompt) })
                }
            } else { None };
            let value = serde_json::to_value(self.snapshot()?)?;
            Ok(PermissionConfigurationResult { delivery, resume, value })
        })
    }

    /// Stage the existing paused exchange without polling provider work under the broker.
    pub(crate) fn prepare_permission_resume(&mut self, text: String) -> Result<PermissionResume> {
        let interaction = self
            .store
            .latest_exchange(&self.session.id)?
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.state() == ExchangeState::Running
                && interaction.execution_started_at_ms().is_none()
                && interaction
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "the latest Harness exchange is not paused for resumption"
        );
        let pending_plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let elicitation = pending_plan
            .as_ref()
            .and_then(|plan| plan.elicitation.as_ref())
            .or(interaction.elicitation());
        if let Some(elicitation) = elicitation {
            if !elicitation.clarification_active {
                return Ok(PermissionResume::NoWork);
            }
            let clarification = interaction
                .node_list
                .iter()
                .rev()
                .find_map(|node| match node {
                    ExchangeNode::ExchangeInput { prompt }
                        if prompt.intent == crate::exchange::InputIntent::Clarification
                            && prompt.question.as_ref().is_some_and(|target| {
                                target.set_id == elicitation.question_set.id
                            }) =>
                    {
                        Some(prompt)
                    }
                    _ => None,
                })
                .context("interrupted clarification has no recorded question input")?;
            let follow_up = format!("{}\n\n{text}", clarification.text);
            let elicitation_json = serde_json::to_string_pretty(elicitation)?;
            let prompt = if let Some(plan) = &pending_plan {
                PlanPrompt::clarification(&plan.request, &elicitation_json, &follow_up)
            } else {
                PlanPrompt::question_follow_up(&elicitation_json, &follow_up)
            };
            let mode = self.task_prompt_mode()?;
            let staged = self.stage_foreground_interaction(
                &prompt,
                mode,
                None,
                ProviderInput::from_text(prompt.clone()),
            )?;
            let clarification = Some(match pending_plan {
                Some(plan) => foreground_dispatch::ClarificationContext::Plan { plan_id: plan.id },
                None => foreground_dispatch::ClarificationContext::Interaction,
            });
            return Ok(PermissionResume::Staged {
                interaction: Box::new(staged),
                clarification,
                planning: None,
            });
        }
        anyhow::ensure!(
            !interaction.awaiting_input(),
            "the latest Harness exchange is awaiting a user decision"
        );
        let mode = self.task_prompt_mode()?;
        let planning = if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            let task = self
                .task_controller
                .current()?
                .context("planning requires an active task")?;
            Some(self.capture_planning_context(&task)?)
        } else {
            None
        };
        let staged = self.stage_foreground_interaction(
            &text,
            mode,
            None,
            ProviderInput::from_text(text.clone()),
        )?;
        Ok(PermissionResume::Staged {
            interaction: Box::new(staged),
            clarification: None,
            planning,
        })
    }

    /// Stage the existing task operation's resume progress for ordered delivery.
    pub(crate) fn prepare_permission_resume_progress(
        &mut self,
        operation_id: &str,
    ) -> Result<PendingProviderDelivery> {
        self.prepare_provider_delivery(AppliedProviderEvent::Update(
            SessionUpdate::ConfigurationProgress {
                operation_id: operation_id.to_owned(),
                state: ConfigurationProgressState::Resuming,
            },
        ))
    }

    /// Stage the common configuration notification and optional resume after compaction.
    pub(crate) fn finish_compaction_configuration(
        &mut self,
        action: &Value,
    ) -> Result<PermissionConfigurationResult> {
        let operation_id = action
            .get("operation_id")
            .and_then(Value::as_str)
            .or_else(|| action.pointer("/_command/id").and_then(Value::as_str))
            .or(self.active_operation_id.as_deref())
            .map(str::to_owned);
        let update = SessionUpdate::Configuration {
            session: Box::new(self.session_view()?),
            access_abbreviation: self.session.access.abbreviation().into(),
            operation_id,
            catalog: Box::new(self.provider_catalog_request()?),
        };
        let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Update(update))?;
        let resume = if action
            .get("resume")
            .and_then(Value::as_bool)
            .unwrap_or(false)
        {
            let previous = self
                .store
                .latest_exchange(&self.session.id)?
                .context("interrupted exchange is missing")?;
            let notice = "Continue the interrupted request using the current repository and completed tool results. Do not repeat completed actions.";
            if previous.is_finished() {
                None
            } else {
                Some(if previous.elicitation().is_some() {
                    notice.to_owned()
                } else {
                    format!("{notice}\n\nOriginal request: {}", previous.prompt)
                })
            }
        } else {
            None
        };
        Ok(PermissionConfigurationResult {
            delivery,
            resume,
            value: serde_json::to_value(self.snapshot()?)?,
        })
    }
    pub(super) async fn apply_configuration(
        &mut self,
        action: Value,
        command: ConfigurationCommand,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let previous_permission = self.session.approval_mode;
        let previous_access = self.session.access.abbreviation();
        let mut access = self.session.access.clone();
        let mut permission_access = None;
        let mut permission = self.session.approval_mode;
        if command == ConfigurationCommand::Permission {
            let change: crate::task::PermissionChange = serde_json::from_value(
                action
                    .get("change")
                    .cloned()
                    .context("permission change is required")?,
            )?;
            if let Some(selection) = action.get("permission_selection") {
                let selection: crate::task::PermissionSelection =
                    serde_json::from_value(selection.clone())?;
                access.sandbox = selection.sandbox;
                access.read_access = selection.read_access;
                access.write_access = selection.write_access;
                permission = selection.approval_mode;
            } else {
                change.apply(
                    &mut access,
                    &mut permission,
                    forge_providers::registry::resolve(&forge_providers::registry::select(
                        &self.session.provider,
                    )?)?
                    .capability
                    .supports_sandbox(),
                );
            }
            if matches!(
                change,
                crate::task::PermissionChange::ToggleReadScope
                    | crate::task::PermissionChange::ToggleWriteEnabled
                    | crate::task::PermissionChange::ToggleWriteScope
            ) && let Some(preference) =
                crate::settings::SettingsStore::read_only(&self.data_root)?
                    .get(
                        &self.session.workspace,
                        &self.session.id,
                        &self.session.provider,
                    )?
                    .desired
                    .access
            {
                access.sandbox = preference.sandbox;
            }
            if !matches!(change, crate::task::PermissionChange::SetApproval { .. }) {
                permission_access = Some(access.validate()?);
            }
        }
        let resume = action
            .get("resume")
            .and_then(Value::as_bool)
            .unwrap_or(false);
        if command == ConfigurationCommand::Configure {
            self.configuration_progress(ConfigurationProgressState::Applying)
                .await?;
        }
        match command {
            ConfigurationCommand::Configure => {
                Box::pin(
                    self.configure_session(
                        action
                            .get("config")
                            .cloned()
                            .context("configuration is required")?,
                    ),
                )
                .await?;
            }
            ConfigurationCommand::Compact => {
                Box::pin(self.compact_session(true)).await?;
            }
            ConfigurationCommand::Permission => {
                let mut config = json!({"approval_mode": permission, "session_policy": true});
                if let Some(access) = permission_access {
                    config["access"] = serde_json::to_value(access)?;
                }
                Box::pin(self.configure_session(config)).await?;
            }
        }
        if command == ConfigurationCommand::Configure
            && let Some(revision) = action
                .pointer("/config/settings_revision")
                .and_then(Value::as_u64)
            && self.active_operation_id.is_some()
        {
            self.configuration_progress(ConfigurationProgressState::Verifying)
                .await?;
            let operation = self
                .active_operation_id
                .as_deref()
                .expect("active configuration operation");
            let settings = crate::settings::SettingsStore::open(&self.data_root)?;
            let snapshot = settings.get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?;
            anyhow::ensure!(
                snapshot.revision == revision,
                "settings changed before application confirmation"
            );
            let mut effective = snapshot.desired;
            effective.access = Some(self.session.access.clone());
            effective.shell = Some(self.session.shell);
            effective.plan_compact = Some(self.session.plan_compact);
            effective.approval_timeout = Some(self.session.approval_timeout);
            effective.tool_timeout = Some(self.session.tool_timeout);
            settings.confirm_application(&self.session.id, operation, revision, effective)?;
        }
        if command == ConfigurationCommand::Configure {
            let request = self.provider_catalog_request()?;
            self.notify_unavailable_mcp(&request, &mut Vec::new())
                .await?;
        }
        self.emit_session_update(
            SessionUpdate::Configuration {
                session: Box::new(self.session_view()?),
                access_abbreviation: self.session.access.abbreviation().into(),
                operation_id: self.active_operation_id.clone(),
                catalog: Box::new(self.provider_catalog_request()?),
            },
            &mut Vec::new(),
        )
        .await?;
        let permission_notice = (command == ConfigurationCommand::Permission).then(|| format!(
            "The user changed filesystem access from {} to {} and approval mode from {} to {}. Continue the interrupted work under the updated policy. \
             Saved approval rules still apply. Preserve completed work. \
             Before retrying an interrupted operation, inspect its recorded outcome and any affected files.",
            previous_access, self.session.access.abbreviation(), previous_permission.label(), permission.label()
        ));
        if resume {
            self.configuration_progress(ConfigurationProgressState::Resuming)
                .await?;
            let previous = self
                .store
                .latest_exchange(&self.session.id)?
                .context("interrupted exchange is missing")?;
            let notice = permission_notice.as_deref().unwrap_or("Continue the interrupted request using the current repository and completed tool results. Do not repeat completed actions.");
            let prompt = if previous.elicitation().is_some() {
                notice.to_owned()
            } else {
                format!("{notice}\n\nOriginal request: {}", previous.prompt)
            };
            return Box::pin(self.resume_exchange(json!({"text":prompt}))).await;
        }
        Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
    }
}

#[cfg(test)]
mod owned_tests {
    use super::*;

    /// A failed settings COMMIT retains the host intent and replays it exactly once.
    #[tokio::test]
    async fn configuration_commit_failure_recovers_after_host_write() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let settings = crate::settings::SettingsStore::open(data.path())?;
        settings.execute_test_sql("CREATE TABLE configuration_test_parent(id INTEGER PRIMARY KEY); CREATE TABLE configuration_test_child(id INTEGER REFERENCES configuration_test_parent(id) DEFERRABLE INITIALLY DEFERRED); CREATE TRIGGER configuration_test_fail AFTER INSERT ON setting_operation BEGIN INSERT INTO configuration_test_child VALUES(1); END;")?;
        let preparation = broker
            .prepare_configuration(json!({"approval_mode":"allow","automatic_checks":true}))?;
        let candidate = broker.build_configuration_candidate(preparation, &[])?;
        assert!(broker.commit_configuration_candidate(candidate).is_err());
        assert_eq!(
            broker
                .store
                .load_session(&broker.session.id)?
                .unwrap()
                .approval_mode,
            crate::session::ApprovalMode::Allow
        );
        assert!(
            broker
                .store
                .pending_configuration_intent(&broker.session.id)?
                .is_some()
        );
        assert!(broker.task_controller.native_goal_is_uncertain());
        assert!(
            broker
                .store
                .save_owned_session(&broker.session, &broker.client_id)
                .is_err()
        );
        settings.execute_test_sql("DROP TRIGGER configuration_test_fail;")?;
        assert!(broker.recover_pending_configuration()?);
        let snapshot = settings.get(
            &broker.session.workspace,
            &broker.session.id,
            &broker.session.provider,
        )?;
        assert_eq!(snapshot.desired.automatic_checks, Some(true));
        assert!(
            broker
                .store
                .pending_configuration_intent(&broker.session.id)?
                .is_none()
        );
        assert!(!broker.recover_pending_configuration()?);
        assert_eq!(
            settings
                .get(
                    &broker.session.workspace,
                    &broker.session.id,
                    &broker.session.provider
                )?
                .revision,
            snapshot.revision
        );
        Ok(())
    }

    /// A committed receipt makes recovery clear the intent without replaying settings.
    #[tokio::test]
    async fn configuration_recovery_does_not_replay_committed_receipt() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.store.execute_configuration_test_sql("CREATE TRIGGER configuration_test_clear BEFORE DELETE ON command_receipt WHEN OLD.result='configuration_intent_v1' BEGIN SELECT RAISE(ABORT,'intent clear failed'); END;")?;
        let preparation = broker.prepare_configuration(json!({"automatic_checks":true}))?;
        let candidate = broker.build_configuration_candidate(preparation, &[])?;
        assert!(broker.commit_configuration_candidate(candidate).is_err());
        let settings = crate::settings::SettingsStore::open(data.path())?;
        let before = settings
            .get(
                &broker.session.workspace,
                &broker.session.id,
                &broker.session.provider,
            )?
            .revision;
        assert!(
            broker
                .store
                .pending_configuration_intent(&broker.session.id)?
                .is_some()
        );
        broker
            .store
            .execute_configuration_test_sql("DROP TRIGGER configuration_test_clear;")?;
        assert!(broker.recover_pending_configuration()?);
        assert_eq!(
            settings
                .get(
                    &broker.session.workspace,
                    &broker.session.id,
                    &broker.session.provider
                )?
                .revision,
            before
        );
        assert!(
            broker
                .store
                .pending_configuration_intent(&broker.session.id)?
                .is_none()
        );
        Ok(())
    }

    #[tokio::test]
    async fn ask_model_commit_rejects_superseded_settings() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let prepared = broker.prepare_ask_model_configuration(Value::Null)?;
        let mut settings = crate::settings::SettingsStore::open(data.path())?;
        settings.patch(
            crate::settings::SettingTarget {
                scope: crate::settings::SettingScope::Session,
                workspace: &broker.session.workspace,
                session: &broker.session.id,
            },
            &crate::settings::PortableSettings {
                status_thoughts: Some(true),
                ..Default::default()
            },
        )?;
        assert!(
            broker
                .commit_ask_model_configuration(prepared, &[])
                .is_err()
        );
        Ok(())
    }

    /// Catalog results cannot apply after the selected configuration changes.
    #[tokio::test]
    async fn configuration_catalog_rejects_stale_model_authority() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let job = broker.prepare_configuration_catalog()?;
        let result = ConfigurationCatalogResult {
            authority: job.authority,
            models: Vec::new(),
        };
        broker.session.model = "newer-model".to_owned();
        assert!(broker.commit_configuration_catalog(result).is_err());
        assert_eq!(broker.session.model, "newer-model");
        Ok(())
    }

    /// A validated candidate cannot overwrite a policy changed while work was unlocked.
    #[tokio::test]
    async fn configuration_candidate_rejects_changed_policy() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let preparation = broker.prepare_configuration(json!({"automatic_checks": true}))?;
        assert!(!preparation.needs_catalog(&broker));
        assert!(
            !broker
                .prepare_configuration(json!({"service_tier":"default"}))?
                .needs_catalog(&broker)
        );
        let candidate = broker.build_configuration_candidate(preparation, &[])?;
        broker.session.model = "newer-model".to_owned();
        assert!(broker.commit_configuration_candidate(candidate).is_err());
        assert_eq!(broker.session.model, "newer-model");
        Ok(())
    }

    /// Losing an in-flight provider write requires generation recovery before reuse.
    #[tokio::test]
    async fn abandoned_configuration_write_poison_fences_owner() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let guard = ConfigurationWriteGuard {
            registry: Arc::clone(&broker.execution_registry),
            owner: crate::provider::ExecutionOwner::root(&broker.session.id),
            controller: Arc::clone(&broker.task_controller),
            armed: true,
        };
        drop(guard);
        assert!(broker.task_controller.native_goal_is_uncertain());
        Ok(())
    }

    /// A captured permission change cannot overwrite a newer session policy.
    #[tokio::test]
    async fn permission_commit_rejects_newer_policy() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let captured = broker.prepare_permission_configuration(
            &json!({"change":{"kind":"set_approval","mode":"allow"}}),
        )?;
        let newer = if broker.session.approval_mode == crate::session::ApprovalMode::Deny {
            crate::session::ApprovalMode::Ask
        } else {
            crate::session::ApprovalMode::Deny
        };
        broker.session.approval_mode = newer;
        assert!(broker.commit_permission_configuration(captured).is_err());
        assert_eq!(broker.session.approval_mode, newer);
        Ok(())
    }

    /// A stable captured approval change commits locally without requesting a provider turn.
    #[tokio::test]
    async fn permission_commit_applies_stable_approval() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let before = broker.store.list_exchange(&broker.session.id)?.len();
        let captured = broker.prepare_permission_configuration(
            &json!({"change":{"kind":"set_approval","mode":"allow"}}),
        )?;
        let result = broker.commit_permission_configuration(captured)?;
        assert_eq!(
            broker.session.approval_mode,
            crate::session::ApprovalMode::Allow
        );
        assert!(result.resume.is_none());
        assert_eq!(
            broker.store.list_exchange(&broker.session.id)?.len(),
            before
        );
        Ok(())
    }

    /// A completed exchange must not replay when a queued write toggle reaches its owner.
    #[tokio::test]
    async fn write_toggle_does_not_resume_an_exchange_that_finished_during_cleanup() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let exchange = super::super::test::completed_interaction(
            "finished-before-toggle",
            &broker.session.id,
            Vec::new(),
        );
        broker.store.save_exchange(&exchange)?;
        let before = broker.session.access.write_access;
        let captured = broker.prepare_permission_configuration(
            &json!({"change":{"kind":"toggle_write_enabled"},"resume":true}),
        )?;
        let result = broker.commit_permission_configuration(captured)?;
        assert!(result.resume.is_none());
        assert_ne!(broker.session.access.write_access, before);
        let after = broker.store.latest_exchange(&broker.session.id)?.unwrap();
        assert_eq!(after.id, exchange.id);
        assert!(after.is_finished());
        Ok(())
    }
}
