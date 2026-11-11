use super::*;
use crate::exchange::command::ConfigurationRejection;

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum ConfigurationCommand {
    Permission,
    Configure,
    Compact,
}

/// Select affected execution from the effective candidate, never request labels or provider names.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ConfigurationEffect {
    /// Commit future policy or presentation without collecting native work.
    Local,
    /// Replace the root invocation while preserving independent child work.
    ReplaceRoot,
    /// Collect descendants whose inherited execution authority would change.
    ReplaceTree,
}

/// Resolve a retained configuration intent without launching work or replacing a live child.
pub(crate) enum PendingConfigurationRetry {
    /// Preserve the latest intent until independently observed native work is collected.
    Blocked,
    /// Discard an intent whose newest desired revision is already confirmed applied.
    Obsolete,
    /// Rebuild a fresh owned command from the exact raw patch or newest desired revision.
    Ready(Box<crate::exchange::command::SessionConfigurationPatch>),
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

/// Execution policy committed before a permission change, kept to describe that change on resume.
#[derive(Clone, Debug)]
pub(crate) struct PolicyBaseline {
    access: String,
    approval: crate::session::ApprovalMode,
}

impl PolicyBaseline {
    /// Tell the resumed model exactly which policy changed so it re-plans interrupted operations.
    pub(crate) fn resume_notice(
        &self,
        current_access: &str,
        current_approval: crate::session::ApprovalMode,
    ) -> String {
        format!(
            "The user changed filesystem access from {} to {} and approval mode from {} to {}. \
             Continue the interrupted work under the updated policy. Saved approval rules still apply. \
             Preserve completed work. Before retrying an interrupted operation, inspect its recorded \
             outcome and any affected files.",
            self.access,
            current_access,
            self.approval.label(),
            current_approval.label()
        )
    }
}

/// Return owned presentation and any continuation prompt to the service driver.
pub(crate) struct PermissionConfigurationResult {
    pub(crate) delivery: PendingProviderDelivery,
    pub(crate) resume: Option<String>,
    pub(crate) value: Value,
}

/// Capture provider catalog work without retaining the broker across I/O.
pub(crate) struct ConfigurationCatalogJob {
    deadline: Option<tokio::time::Instant>,
    provider: Arc<Provider>,
    request: ProviderCatalogRequest,
    authority: Box<ConfigurationAuthority>,
}

pub(crate) struct ConfigurationCatalogResult {
    authority: Box<ConfigurationAuthority>,
    models: Vec<crate::provider::ProviderModel>,
}

/// Own a provider settings reconciliation independently of the broker lock.
pub(crate) struct ConfigurationReconcileJob {
    deadline: Option<tokio::time::Instant>,
    provider: Arc<Provider>,
    request: ProviderRequest,
    selection: crate::settings::PortableSettings,
    authority: Box<ConfigurationAuthority>,
    completed: Option<Arc<std::sync::atomic::AtomicBool>>,
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
    controller: Arc<crate::task::SessionTaskController>,
}

pub(crate) struct ConfigurationReconcileResult {
    authority: Box<ConfigurationAuthority>,
    completed: Option<Arc<std::sync::atomic::AtomicBool>>,
    uncertainty: Option<ConfigurationWriteGuard>,
    receipt: crate::provider::ConfigureReceipt,
}

/// Fence an owner if an in-flight provider settings write loses its completion.
struct ConfigurationWriteGuard {
    registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    owner: crate::provider::ThreadOwner,
    controller: Arc<crate::task::SessionTaskController>,
    armed: bool,
}

impl Drop for ConfigurationWriteGuard {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        self.controller.mark_configuration_uncertain();
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
    question_model: Option<crate::settings::AskModel>,
    delegation_registry_changed: bool,
    update_model_preference: bool,
    resource_selection: Option<crate::settings::PortableSettings>,
    reconciled: Arc<std::sync::atomic::AtomicBool>,
    uncertainty: Option<ConfigurationWriteGuard>,
    receipt: Option<crate::provider::ConfigureReceipt>,
}

pub(crate) struct ConfigurationPreparation {
    authority: Box<ConfigurationAuthority>,
    params: Value,
    resource_selection: Option<crate::settings::PortableSettings>,
}

impl ConfigurationCandidate {
    /// Report whether this candidate applies a resource selection that needs fresh launch notices.
    pub(crate) fn has_resource_selection(&self) -> bool {
        self.resource_selection.is_some()
    }

    /// Get the execution scope affected by this validated effective change.
    pub(crate) fn effect(&self) -> ConfigurationEffect {
        let previous = &self.authority.session;
        let selected = &self.configured;
        if selected.access != previous.access
            || selected.approval_mode != previous.approval_mode
            || selected.shell != previous.shell
        {
            return ConfigurationEffect::ReplaceTree;
        }
        if selected.model != previous.model
            || selected.resolved_model != previous.resolved_model
            || selected.effort != previous.effort
            || selected.context_window != previous.context_window
            || selected.service_tier != previous.service_tier
            || self.delegation_registry_changed
            || self.resource_selection.is_some()
        {
            return ConfigurationEffect::ReplaceRoot;
        }
        ConfigurationEffect::Local
    }

    /// Preserve observations committed after preparation while applying only configuration fields.
    fn rebase(&mut self, current: &crate::session::HarnessSession) {
        let mut rebased = current.clone();
        let selected = self.configured.as_ref();
        rebased.model = selected.model.clone();
        rebased.resolved_model = if selected.model == self.authority.session.model {
            current.resolved_model.clone()
        } else {
            selected.resolved_model.clone()
        };
        rebased.effort = selected.effort.clone();
        rebased.context_window = selected.context_window.clone();
        if rebased.model != current.model
            || rebased.resolved_model != current.resolved_model
            || rebased.context_window != current.context_window
        {
            rebased.context_usage = None;
        }
        rebased.service_tier = selected.service_tier;
        rebased.access = selected.access.clone();
        rebased.approval_mode = selected.approval_mode;
        rebased.auto_compaction = selected.auto_compaction.clone();
        rebased.plan_executor = selected.plan_executor.clone();
        rebased.plan_compact = selected.plan_compact;
        rebased.shell = selected.shell;
        rebased.approval_timeout = selected.approval_timeout;
        rebased.tool_timeout = selected.tool_timeout;
        *self.configured = rebased;
    }
}

pub(crate) struct ConfigurationCommit {
    pub(crate) value: Value,
    pub(crate) events: Vec<SessionEvent>,
}

pub(crate) struct ConfigurationMcpNotices {
    pub(crate) deliveries: Vec<PendingProviderDelivery>,
    authority: Box<ConfigurationAuthority>,
    notices: Option<std::collections::HashMap<String, String>>,
}

impl ConfigurationCatalogJob {
    /// Restrict discovery to its enclosing configuration command's original deadline.
    pub(crate) fn restrict_deadline(&mut self, deadline: tokio::time::Instant) {
        self.deadline = Some(
            self.deadline
                .map_or(deadline, |current| current.min(deadline)),
        );
    }

    pub(crate) async fn run(self) -> Result<ConfigurationCatalogResult> {
        let context = self.provider.query_context(&self.request).await?;
        let models = self
            .provider
            .model_list_before(context, self.deadline)
            .await?;
        Ok(ConfigurationCatalogResult {
            authority: self.authority,
            models,
        })
    }
}

impl ConfigurationReconcileJob {
    /// Recognize authoritative native rejection without treating independent cleanup uncertainty as unchanged.
    pub(crate) fn unchanged_failure(error: &crate::provider::ProviderError) -> bool {
        use crate::provider::ProviderError;
        match error {
            ProviderError::Failure(failure) => {
                failure.operation() == forge_provider_api::ProviderOperation::ThreadConfigure
                    && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged
            }
            ProviderError::Combined { primary, secondary } => {
                Self::unchanged_failure(primary) && secondary.iter().all(Self::unchanged_failure)
            }
            ProviderError::PreparationFailed(cause) => Self::unchanged_failure(cause),
            ProviderError::Unsupported(_) => true,
            ProviderError::Contract(_)
            | ProviderError::Cleanup(_)
            | ProviderError::Observation(_)
            | ProviderError::RuntimeVersion(_)
            | ProviderError::RuntimeLaunch { .. }
            | ProviderError::PreparationCleanup { .. }
            | ProviderError::NotConsumed
            | ProviderError::DeliveryUncertain
            | ProviderError::Cancelled
            | ProviderError::Retryable(_)
            | ProviderError::Delivery(_) => false,
        }
    }

    /// Restrict native mutation to the original command budget before its worker starts.
    pub(crate) fn restrict_deadline(&mut self, deadline: tokio::time::Instant) {
        self.deadline = Some(
            self.deadline
                .map_or(deadline, |current| current.min(deadline)),
        );
    }

    pub(crate) async fn run(self) -> Result<ConfigurationReconcileResult> {
        let lane = self.controller.configuration_lane().lock_owned().await;
        self.controller.ensure_configuration_certain()?;
        let mut guard = ConfigurationWriteGuard {
            registry: self.registry,
            owner: self.owner,
            controller: self.controller,
            armed: true,
        };
        // The spawned worker retains the transport lane even if its caller is
        // cancelled, so generation recovery cannot race a late provider write.
        let receipt = Arc::new(std::sync::Mutex::new(None));
        let retained_receipt = receipt.clone();
        let worker = guard.registry.retain_cleanup(
            crate::provider::session_execution::CleanupWorkKind::ConfigurationWrite,
            async move {
                let _lane = lane;
                let configured = self
                    .provider
                    .thread_configure(self.request, &self.selection, self.deadline)
                    .await;
                // A completed native rejection is a result, not failed task collection.
                // Keep it outside the cleanup ledger so it cannot poison later commands.
                *retained_receipt.lock().map_err(|_| {
                    crate::provider::session_execution::CleanupWorkFailure::Provider(
                        crate::provider::ProviderError::DeliveryUncertain,
                    )
                })? = Some(configured);
                Ok(())
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
        let receipt = receipt
            .lock()
            .map_err(|_| anyhow::anyhow!("configuration receipt lock poisoned"))?
            .take()
            .context("configuration worker did not retain its binding receipt")?;
        let receipt = match receipt {
            Ok(receipt) => receipt,
            Err(error) => {
                if Self::unchanged_failure(&error) {
                    guard.armed = false;
                }
                return Err(error.into());
            }
        };
        Ok(ConfigurationReconcileResult {
            authority: self.authority,
            completed: self.completed,
            uncertainty: Some(guard),
            receipt,
        })
    }
}

/// Preserve the owner-selected paused exchange and its postprocessing during continuation.
pub(crate) enum SelectedContinuation {
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
            || p.get("model").and_then(Value::as_str).is_some()
            || p.pointer("/ask_model/model").is_some()
            || p.get("plan_executor_effort").is_some()
            || p.get("plan_executor_enabled").and_then(Value::as_bool) == Some(true)
            || selection
            || (nonstandard_tier
                && (p.get("service_tier").is_some()
                    || p.get("model").and_then(Value::as_str).is_some()))
    }
}

impl HarnessBroker {
    /// Validate and commit explicit Background model controls before creating its input exchange.
    pub(crate) async fn configure_background_model(
        &mut self,
        config: &crate::service::TextGenerationConfig,
    ) -> Result<crate::settings::ValidatedModelSelection> {
        anyhow::ensure!(
            self.session.background.is_some(),
            "Background request requires a Background session"
        );
        let context = self
            .provider
            .query_context(&self.provider_catalog_request()?)
            .await?;
        let catalog = self.provider.model_list(context).await?;
        let selection = crate::settings::ValidatedModelSelection::background(
            config,
            &self.capability,
            &catalog,
        )?;
        let mut selected = self.session.clone();
        selection.apply(&mut selected);
        self.store.save_owned_session(&selected, &self.client_id)?;
        self.session = selected;
        self.context_usage_fresh &= self.session.context_usage.is_some();
        Ok(selection)
    }

    /// Resolve inherited tuning before a controller can publish state or dispatch input.
    pub(crate) async fn resolve_inherited_model_settings(&mut self) -> Result<()> {
        let context = self
            .provider
            .query_context(&self.provider_catalog_request()?)
            .await?;
        // ACP agents negotiate their model catalog per binding, so workspace discovery before
        // the first binding can be unsupported, and an agent without model selection advertises
        // no catalog at all. The binding validates the inherited settings when it opens, so an
        // unsupported or empty workspace catalog leaves them unchanged.
        let models = match self.provider.model_list(context).await {
            Ok(models) if models.is_empty() => return Ok(()),
            Ok(models) => models,
            Err(crate::provider::ProviderError::Unsupported(_)) => return Ok(()),
            Err(error) => return Err(error.into()),
        };
        let model = models
            .iter()
            .find(|model| {
                if self.session.model == "default" {
                    model.is_default
                } else {
                    model.id == self.session.model
                }
            })
            .context(ConfigurationRejection::ModelUnavailable)?;
        let mut selected = self.session.clone();
        if selected.effort != "default" && !model.reasoning.contains(&selected.effort) {
            selected.effort = model
                .default_reasoning
                .as_ref()
                .filter(|choice| model.reasoning.contains(choice))
                .cloned()
                .unwrap_or_else(|| "default".into());
        }
        if selected.context_window.as_ref().is_some_and(|choice| {
            !model
                .context_window
                .iter()
                .any(|window| &window.id == choice)
        }) {
            selected.context_window = model
                .default_context_window
                .as_ref()
                .filter(|choice| {
                    model
                        .context_window
                        .iter()
                        .any(|window| &window.id == *choice)
                })
                .cloned();
        }
        if selected.service_tier.validate(&self.capability).is_err()
            || selected.service_tier.validate_model(model).is_err()
        {
            selected.service_tier = crate::provider::ServiceTier::Standard;
        }
        if selected.effort == self.session.effort
            && selected.context_window == self.session.context_window
            && selected.service_tier == self.session.service_tier
        {
            return Ok(());
        }
        if selected.context_window != self.session.context_window {
            selected.context_usage = None;
        }
        self.store.save_owned_session(&selected, &self.client_id)?;
        self.trace.record(
            &selected.id,
            "session.inherited_model_settings_resolved",
            json!({
                "model": selected.model,
                "effort": selected.effort,
                "context_window": selected.context_window,
                "service_tier": selected.service_tier,
            }),
        );
        self.session = selected;
        Ok(())
    }

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
    /// Revalidate a pending native configuration after consuming the original child-terminal prefix.
    pub(crate) fn pending_configuration_retry(
        &self,
        captured: &crate::exchange::command::SessionConfigurationPatch,
    ) -> Result<PendingConfigurationRetry> {
        let observed = self.store.list_observed_agents(&self.session.id)?;
        let root_bindings: std::collections::HashSet<_> = observed
            .iter()
            .filter(|agent| agent.parent_scope().owner().is_root())
            .map(|agent| agent.key().binding_id().clone())
            .collect();
        let blocked = observed.iter().any(|agent| {
            root_bindings.contains(agent.key().binding_id())
                && agent.availability() == crate::agent::ObservedAgentAvailability::Available
                && (matches!(agent.state(), crate::agent::ObservedAgentState::Active(_))
                    || agent.exchanges().iter().any(|exchange| {
                        exchange
                            .turn
                            .iter()
                            .any(|turn| matches!(turn.state(), crate::turn::TurnState::Running))
                    }))
        });
        if blocked {
            return Ok(PendingConfigurationRetry::Blocked);
        }
        if captured.application_revision().is_none() {
            return Ok(PendingConfigurationRetry::Ready(Box::new(captured.clone())));
        }
        let store = crate::settings::SettingsStore::read_only(&self.data_root)?;
        let snapshot = store.get(
            &self.session.workspace,
            &self.session.id,
            &self.session.provider,
        )?;
        if store.application(&self.session.id)?.applied_revision == Some(snapshot.revision) {
            return Ok(PendingConfigurationRetry::Obsolete);
        }
        Ok(PendingConfigurationRetry::Ready(serde_json::from_value(
            json!({"settings_revision":snapshot.revision}),
        )?))
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
        let fields: Vec<String> = action
            .pointer("/config/setting_fields")
            .cloned()
            .map(serde_json::from_value)
            .transpose()?
            .unwrap_or_default();
        self.store
            .save_session_event(&crate::timeline::SessionEventRecord {
                id: format!("{}:configuration:{operation}", self.session.id),
                session_id: self.session.id.clone(),
                owner: crate::timeline::TimelineOwner::session(&self.session.id),
                created_at_ms: self.clock.now_ms(),
                detail: crate::timeline::SessionEventKind::Configuration {
                    operation_id: operation.to_owned(),
                    state: "completed".into(),
                    field: fields,
                    reason: None,
                },
            })?;
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
                        diagnostic: super::publication::DiagnosticView {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::SanitizedMessage::new(message.clone())
                                .expect("host diagnostic is nonempty"),
                            severity: super::publication::DiagnosticSeverity::Warning,
                            scope: crate::provider::EventScope::Binding {
                                thread_id: forge_provider_api::ThreadId::from(
                                    crate::provider::ThreadOwner::root(&self.session.id),
                                ),
                            },
                            native_scope: forge_provider_api::WarningScope::Provider,
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
                ConfigurationRejection::SettingsChanged
            );
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
            params["access"] = serde_json::to_value(
                self.capability
                    .resolve_access(settings.desired.access.clone().unwrap_or_default()),
            )?;
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
            policy
                .validate(configured.context_usage.as_ref())
                .context(ConfigurationRejection::CompactionPolicyInvalid)?;
            anyhow::ensure!(
                !policy.enabled || self.capability.native_compact,
                ConfigurationRejection::CompactionUnavailable
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
            .transpose()
            .context(ConfigurationRejection::AccessPolicyInvalid)?;
        if let Some(value) = params.get("shell") {
            configured.shell = serde_json::from_value(value.clone())?;
            configured
                .shell
                .executable()
                .context(ConfigurationRejection::ShellUnavailable)?;
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
            tier.validate(&self.capability)
                .context(ConfigurationRejection::ServiceTierUnavailable)?;
        }
        let requested_model = params.get("model").and_then(Value::as_str);
        let requested_effort = params.get("effort").and_then(Value::as_str);
        let requested_context_window = params.get("context_window").and_then(Value::as_str);
        anyhow::ensure!(
            !requested_model.is_some_and(|model| model != self.session.model)
                || self.capability.model_selection,
            ConfigurationRejection::ModelUnavailable,
        );
        anyhow::ensure!(
            !requested_effort.is_some_and(|effort| effort != self.session.effort)
                || self.capability.effort_selection,
            ConfigurationRejection::EffortUnavailable,
        );
        let mut plan_executor = self.session.plan_executor.clone();
        let mut plan_compact = self.session.plan_compact;
        if let Some(enabled) = params.get("plan_compact").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.native_compact,
                    ConfigurationRejection::CompactionUnavailable
                );
            }
            plan_compact = enabled;
        }
        if let Some(enabled) = params.get("plan_executor_enabled").and_then(Value::as_bool) {
            if enabled {
                anyhow::ensure!(
                    self.capability.model_selection,
                    ConfigurationRejection::PlanExecutorUnavailable
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
                            .context(ConfigurationRejection::ModelUnavailable)?
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
        let mut selected_tier = requested_service_tier.unwrap_or(self.session.service_tier);
        let validate_tier = selected_tier != crate::provider::ServiceTier::Standard
            && (requested_service_tier.is_some() || requested_model.is_some());
        if requested_model.is_some()
            || validate_tier
            || requested_effort.is_some()
            || requested_context_window.is_some()
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
                    ConfigurationRejection::ModelUnavailable
                );
            }
            if requested_model.is_some_and(|model| model != self.session.model) {
                let model = target_model.context(ConfigurationRejection::ModelUnavailable)?;
                if requested_effort.is_none() {
                    configured.effort = model
                        .default_reasoning
                        .clone()
                        .unwrap_or_else(|| "default".into());
                }
                if params.get("context_window").is_none() {
                    configured.context_window = model.default_context_window.clone();
                }
                if requested_service_tier.is_none() && selected_tier.validate_model(model).is_err()
                {
                    selected_tier = crate::provider::ServiceTier::Standard;
                    configured.service_tier = selected_tier;
                }
            }
            if validate_tier {
                selected_tier
                    .validate_model(target_model.context(ConfigurationRejection::ModelUnavailable)?)
                    .context(ConfigurationRejection::ServiceTierUnavailable)?;
            }
            if let Some(effort) = requested_effort {
                let model = target_model.context(ConfigurationRejection::ModelUnavailable)?;
                anyhow::ensure!(
                    effort == "default"
                        || model.reasoning.iter().any(|candidate| candidate == effort),
                    ConfigurationRejection::EffortUnavailable
                );
            }
            if let Some(context_window) = requested_context_window {
                let model = target_model.context(ConfigurationRejection::ModelUnavailable)?;
                anyhow::ensure!(
                    model
                        .context_window
                        .iter()
                        .any(|candidate| candidate.id == context_window),
                    ConfigurationRejection::ContextWindowUnavailable
                );
            }
        }
        if let Some(model) = params.get("model").and_then(Value::as_str) {
            anyhow::ensure!(!model.trim().is_empty(), "model cannot be empty");
            if model != configured.model {
                configured.select_model(
                    model.to_owned(),
                    crate::session::catalog_model_label(models, model),
                );
            }
        }
        // An alias such as `opus` can name a newer model than the saved label describes.
        configured.refresh_model_label(models);
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
        let question_model = params
            .get("ask_model")
            .map(|value| {
                let selection: crate::settings::AskModel = if value.is_null() {
                    crate::settings::AskModel::default()
                } else {
                    serde_json::from_value(value.clone())?
                };
                if selection.model.is_some() {
                    anyhow::ensure!(
                        self.capability.model_selection,
                        ConfigurationRejection::ModelUnavailable
                    );
                }
                selection
                    .resolve(&configured, models)
                    .map_err(|error| match error {
                        crate::settings::AskModelError::ModelRequired
                        | crate::settings::AskModelError::ModelUnavailable => {
                            ConfigurationRejection::ModelUnavailable
                        }
                        crate::settings::AskModelError::EffortUnavailable => {
                            ConfigurationRejection::EffortUnavailable
                        }
                        crate::settings::AskModelError::ServiceTier(_) => {
                            ConfigurationRejection::ServiceTierUnavailable
                        }
                    })?;
                Ok::<_, anyhow::Error>(selection)
            })
            .transpose()?;
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
        // The candidate persists the requested policy as the saved preference, while the session
        // runs with the subset this provider can enforce, so an unsupported sandbox stays selected.
        // Isolation modes come only from the provider's advertised list, so an unknown mode is a
        // malformed request rather than a preference and still rejects before commit.
        if let Some(access) = &access {
            self.capability
                .validate_access(&crate::session::AccessPolicy {
                    sandbox: false,
                    ..access.clone()
                })
                .context(ConfigurationRejection::AccessPolicyInvalid)?;
            configured.access = self.capability.resolve_access(access.clone());
        }
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
            question_model,
            delegation_registry_changed,
            update_model_preference,
            resource_selection,
            reconciled,
            uncertainty: None,
            receipt: None,
        }))
    }

    pub(crate) fn prepare_candidate_reconcile(
        &self,
        candidate: &ConfigurationCandidate,
    ) -> Result<Option<ConfigurationReconcileJob>> {
        self.validate_configuration_authority(&candidate.authority)?;
        if matches!(candidate.effect(), ConfigurationEffect::Local) {
            return Ok(None);
        }
        self.validate_native_configuration_authority(&candidate.authority)?;
        let configured = &candidate.configured;
        let selection = match &candidate.resource_selection {
            Some(selection) => selection.clone(),
            None => {
                crate::settings::SettingsStore::open(&self.data_root)?
                    .get(&configured.workspace, &configured.id, &configured.provider)?
                    .desired
            }
        };
        let request = ProviderRequest {
            dispatch: Default::default(),
            execution_owner: None,
            harness_session_id: configured.id.clone(),
            workspace: configured.workspace.clone(),
            input: ProviderInput::from_text(""),
            mode: if configured.background.is_some() {
                PromptMode::Background
            } else {
                PromptMode::Chat
            },
            model: configured.model.clone(),
            effort: configured.effort.clone(),
            context_window: configured.context_window.clone(),
            service_tier: configured.service_tier,
            access: configured.access.clone(),
            approval_mode: configured.approval_mode,
            provider_session_id: configured
                .provider_session_id
                .clone()
                .map(TryInto::try_into)
                .transpose()?,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        };
        let mut job = self.prepare_configuration_reconcile(request, selection)?;
        job.completed = Some(Arc::clone(&candidate.reconciled));
        Ok(Some(job))
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
        if candidate.uncertainty.is_some() {
            self.validate_reconciled_configuration_authority(
                &candidate.authority,
                candidate
                    .receipt
                    .as_ref()
                    .context("configuration has no binding receipt")?,
            )?;
        }
        if candidate.params.get("auto_compaction").is_some() {
            candidate
                .configured
                .auto_compaction
                .validate(self.session.context_usage.as_ref())?;
        }
        candidate.rebase(&self.session);
        if let Some(crate::provider::ConfigureReceipt::Applied(receipt)) = &candidate.receipt {
            candidate.configured.provider_session_id =
                receipt.after_history().map(|id| id.as_str().to_owned());
        }
        let identity_receipt = candidate
            .receipt
            .as_ref()
            .and_then(|receipt| match receipt {
                crate::provider::ConfigureReceipt::Applied(receipt) => Some(receipt.clone()),
                crate::provider::ConfigureReceipt::Deferred => None,
            });
        let refresh_check_policy =
            candidate.automatic_checks.is_some() || candidate.resource_selection.is_some();
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
        let provider_setting = if candidate.params.get("settings_revision").is_none() {
            let mut fields = serde_json::Map::new();
            if candidate.update_model_preference
                || candidate.params.get("service_tier").is_some()
                || candidate.params.get("plan_executor_enabled").is_some()
                || candidate.params.get("plan_executor_model").is_some()
                || candidate.params.get("plan_executor_effort").is_some()
            {
                let selection = json!({"model":candidate.configured.model,"effort":candidate.configured.effort,
                    "context_window":candidate.configured.context_window,"service_tier":candidate.configured.service_tier,
                    "plan_executor":candidate.configured.plan_executor});
                fields.extend(
                    selection
                        .as_object()
                        .expect("provider preference encoding is an object")
                        .clone(),
                );
            }
            if let Some(selection) = &candidate.question_model {
                fields.insert("ask_model".into(), serde_json::to_value(selection)?);
            }
            (!fields.is_empty()).then_some(Value::Object(fields))
        } else {
            None
        };
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
            configured,
            subagent_tools,
            lsp_tools,
            status_thoughts,
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
        Ok((serde_json::to_value(self.session_view()?)?, vec![self.event("session_configured", json!({"session": self.session_view()?, "access_abbreviation": self.session.access.abbreviation()}))?]))
        })
        });
        let (value, events) = match committed {
            Ok(value) => value,
            Err(error) => {
                if self
                    .store
                    .pending_configuration_intent(&session_id)?
                    .is_some()
                {
                    self.task_controller.mark_configuration_uncertain();
                    let registry = self.execution_registry.clone();
                    let owner = crate::provider::ThreadOwner::root(&session_id);
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
            self.task_controller.mark_configuration_uncertain();
            let registry = self.execution_registry.clone();
            let owner = crate::provider::ThreadOwner::root(&session_id);
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
        if refresh_check_policy
            && !settings
                .get(&workspace, &session_id, &provider_id)?
                .desired
                .automatic_checks_enabled()
        {
            self.observer.completion_queue.finish()?;
        }
        if let Some(guard) = uncertainty.as_mut() {
            guard.armed = false;
        }
        if let Some(receipt) = identity_receipt {
            self.configuration_identity = Some(receipt);
        }
        Ok(ConfigurationCommit { value, events })
    }

    fn validate_configuration_executor(
        &self,
        executor: &PlanExecutor,
        model_list: &[crate::provider::ProviderModel],
    ) -> Result<()> {
        anyhow::ensure!(
            self.capability.model_selection,
            ConfigurationRejection::PlanExecutorUnavailable
        );
        let model = executor
            .model
            .as_deref()
            .context(ConfigurationRejection::PlanExecutorUnavailable)?;
        let effort = executor
            .effort
            .as_deref()
            .context(ConfigurationRejection::PlanExecutorUnavailable)?;
        let selected = model_list
            .iter()
            .find(|candidate| candidate.id == model)
            .context(ConfigurationRejection::PlanExecutorUnavailable)?;
        anyhow::ensure!(
            effort == "default"
                || selected
                    .reasoning
                    .iter()
                    .any(|candidate| candidate == effort),
            ConfigurationRejection::PlanExecutorUnavailable
        );
        Ok(())
    }
}

impl HarnessBroker {
    /// Capture catalog work and the authority that must still own its result.
    pub(crate) fn prepare_configuration_catalog(&self) -> Result<ConfigurationCatalogJob> {
        Ok(ConfigurationCatalogJob {
            deadline: None,
            provider: Arc::clone(&self.provider),
            request: self.provider_catalog_request()?,
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
        request: ProviderRequest,
        selection: crate::settings::PortableSettings,
    ) -> Result<ConfigurationReconcileJob> {
        self.task_controller.ensure_configuration_certain()?;
        Ok(ConfigurationReconcileJob {
            deadline: None,
            provider: Arc::clone(&self.provider),
            request,
            selection,
            authority: self.capture_configuration_authority()?,
            completed: None,
            registry: Arc::clone(&self.execution_registry),
            owner: crate::provider::ThreadOwner::root(&self.session.id),
            controller: Arc::clone(&self.task_controller),
        })
    }

    /// Hold a successful provider write fenced until the corresponding host commit succeeds.
    pub(crate) fn certify_candidate_reconcile(
        &self,
        candidate: &mut ConfigurationCandidate,
        mut result: ConfigurationReconcileResult,
    ) -> Result<()> {
        if let Err(error) =
            self.validate_reconciled_configuration_authority(&result.authority, &result.receipt)
        {
            return Err(error.context(crate::provider::ProviderError::DeliveryUncertain));
        }
        self.validate_reconciled_configuration_authority(&candidate.authority, &result.receipt)
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
        candidate.receipt = Some(result.receipt);
        Ok(())
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
                && current.model == expected.model
                && current.effort == expected.effort
                && current.context_window == expected.context_window
                && current.service_tier == expected.service_tier
                && current.access == expected.access
                && current.approval_mode == expected.approval_mode
                && current.plan_executor == expected.plan_executor
                && current.plan_compact == expected.plan_compact
                && current.auto_compaction == expected.auto_compaction
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

    /// Require the captured native conversation as well as host authority for provider writes and collection.
    fn validate_native_configuration_authority(
        &self,
        authority: &ConfigurationAuthority,
    ) -> Result<()> {
        self.validate_configuration_authority(authority)?;
        anyhow::ensure!(
            self.session.provider_session_id == authority.session.provider_session_id,
            "native configuration binding changed during provider work"
        );
        Ok(())
    }

    /// Admit an ordered history change only through this exact live binding's configuration receipt.
    fn validate_reconciled_configuration_authority(
        &self,
        authority: &ConfigurationAuthority,
        receipt: &crate::provider::ConfigureReceipt,
    ) -> Result<()> {
        self.validate_configuration_authority(authority)?;
        let crate::provider::ConfigureReceipt::Applied(receipt) = receipt else {
            return self.validate_native_configuration_authority(authority);
        };
        receipt.validate(&crate::provider::ThreadOwner::root(&self.session.id))?;
        let before = receipt.before_history().map(|id| id.as_str());
        let after = receipt.after_history().map(|id| id.as_str());
        let captured = authority.session.provider_session_id.as_deref();
        let current = self.session.provider_session_id.as_deref();
        anyhow::ensure!(
            captured.is_none() || captured == before,
            "configuration receipt belongs to another native history"
        );
        anyhow::ensure!(
            current == captured || current == after || captured.is_none() && current == before,
            "native history changed outside the configured binding receipt"
        );
        Ok(())
    }

    /// Persist only the latest consumed identity while a configured binding's weak proof remains live.
    pub(super) fn retain_session_identity(
        &mut self,
        owner: &crate::provider::ThreadOwner,
        identity: &forge_provider_api::ProviderSessionId,
    ) -> Result<()> {
        anyhow::ensure!(
            owner == &crate::provider::ThreadOwner::root(&self.session.id),
            "session identity crosses root ownership"
        );
        if let Some(receipt) = &self.configuration_identity {
            match receipt.observed_history(owner) {
                Ok(current) if current.as_ref() != Some(identity) => return Ok(()),
                Ok(_) => {}
                Err(_) => self.configuration_identity = None,
            }
        }
        self.session.provider_session_id = Some(identity.as_str().to_owned());
        self.save_session()?;
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
            let access = access.validate()?;
            if action.get("permission_selection").is_some() {
                self.capability
                    .validate_access(&access)
                    .context(ConfigurationRejection::AccessPolicyInvalid)?;
                Some(access)
            } else {
                Some(self.capability.resolve_access(access))
            }
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

    /// Capture the committed execution policy before a permission change replaces it.
    pub(crate) fn owner_policy_baseline(&self) -> PolicyBaseline {
        PolicyBaseline {
            access: self.session.access.abbreviation().to_owned(),
            approval: self.session.approval_mode,
        }
    }

    /// Resolve caller policy intent inside the owner before choosing affected execution.
    pub(crate) fn owner_permission_patch(
        &self,
        change: crate::task::PermissionChange,
    ) -> Result<crate::exchange::command::SessionConfigurationPatch> {
        let captured = self.prepare_permission_configuration(&json!({"change":change}))?;
        Ok(
            crate::exchange::command::SessionConfigurationPatch::execution_policy(
                captured.selected_access,
                captured.selected_approval,
            ),
        )
    }

    /// Commit the captured permission change under the still-current task authority.
    pub(crate) fn commit_permission_configuration(
        &mut self,
        captured: PermissionConfiguration,
    ) -> Result<PermissionConfigurationResult> {
        let controller = self.task_controller.clone();
        controller.with_task_snapshot(|current| {
            anyhow::ensure!(
                foreground_failure::same_retraction_task(
                    current.as_ref(),
                    captured.expected_task.as_ref()
                ) && self.session.id == captured.session_id
                    && self.session.provider == captured.provider
                    && self.session.provider_session_id == captured.provider_session_id
                    && self.session.access == captured.expected_access
                    && self.session.approval_mode == captured.expected_approval,
                "permission authority changed before configuration commit"
            );
            let previous = self.session.clone();
            let baseline = self.owner_policy_baseline();
            let mut configured = previous.clone();
            configured.approval_mode = captured.selected_approval;
            configured.approval_timeout = self
                .permission_coordinator
                .approval_timeout(&configured.id)?;
            if let Some(access) = &captured.selected_access {
                configured.access = access.clone();
            }
            self.session = configured;
            self.permission_coordinator
                .configure_timeout(&self.session.id, self.session.approval_timeout)?;
            if let Err(error) = self.save_session() {
                self.permission_coordinator
                    .configure_timeout(&previous.id, previous.approval_timeout)?;
                self.session = previous;
                return Err(error);
            }
            self.save_preference(false)?;
            if let Some(access) = captured.selected_access {
                let mut settings = crate::settings::SettingsStore::open(&self.data_root)?;
                let portable = crate::settings::PortableSettings {
                    access: Some(access),
                    ..Default::default()
                };
                settings.patch(
                    crate::settings::SettingTarget {
                        scope: crate::settings::SettingScope::Session,
                        workspace: &self.session.workspace,
                        session: &self.session.id,
                    },
                    &portable,
                )?;
            }
            let update = SessionUpdate::Configuration {
                snapshot: Box::new(self.snapshot()?),
                operation_id: captured.operation_id,
                catalog: Box::new(self.provider_catalog_request()?),
            };
            let delivery = self.prepare_provider_delivery(AppliedProviderEvent::Update(update))?;
            let resume = if captured.resume {
                let previous = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("interrupted exchange is missing")?;
                if previous.is_finished() {
                    None
                } else {
                    let notice = baseline.resume_notice(
                        self.session.access.abbreviation(),
                        captured.selected_approval,
                    );
                    Some(if previous.elicitation().is_some() {
                        notice
                    } else {
                        format!("{notice}\n\nOriginal request: {}", previous.prompt)
                    })
                }
            } else {
                None
            };
            let value = serde_json::to_value(self.snapshot()?)?;
            Ok(PermissionConfigurationResult {
                delivery,
                resume,
                value,
            })
        })
    }

    /// Stage the existing paused exchange without polling provider work under the broker.
    pub(crate) fn prepare_selected_continuation(
        &mut self,
        selected: &crate::provider::ExchangeId,
        text: String,
    ) -> Result<SelectedContinuation> {
        anyhow::ensure!(
            self.owner_resume_selected(selected)?,
            "selected exchange no longer requires resumption"
        );
        let interaction = self
            .store
            .load_exchange(selected.as_str())?
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.is_parked(),
            "the selected Harness exchange is not paused for resumption"
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
                return Ok(SelectedContinuation::NoWork);
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
            // Both are the user's words: the recorded clarification and the resumed reply.
            let follow_up = format!("{}\n\n{text}", clarification.text);
            let user = forge_provider_api::PromptInput::Text(follow_up.clone());
            let prompt = if pending_plan.is_some() {
                PlanPrompt::clarification(elicitation, user)
            } else {
                PlanPrompt::question_follow_up(elicitation, user)
            };
            let mode = self.task_prompt_mode()?;
            let staged = self.stage_selected_foreground_interaction(
                &follow_up,
                mode,
                None,
                ProviderInput::new(prompt),
                Some(selected),
            )?;
            let clarification = Some(match pending_plan {
                Some(plan) => foreground_dispatch::ClarificationContext::Plan { plan_id: plan.id },
                None => foreground_dispatch::ClarificationContext::Interaction,
            });
            return Ok(SelectedContinuation::Staged {
                interaction: Box::new(staged),
                clarification,
                planning: None,
            });
        }
        anyhow::ensure!(
            !interaction.awaiting_input(),
            "the selected Harness exchange is awaiting a user decision"
        );
        let mode = self.task_prompt_mode()?;
        let planning = match self.task_controller.current()? {
            Some(task) => self.planning_context(&task, mode)?,
            None if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) => {
                anyhow::bail!("planning requires an active task")
            }
            None => None,
        };
        let staged = self.stage_selected_foreground_interaction(
            &text,
            mode,
            None,
            ProviderInput::from_text(text.clone()),
            Some(selected),
        )?;
        Ok(SelectedContinuation::Staged {
            interaction: Box::new(staged),
            clarification: None,
            planning,
        })
    }

    pub(super) async fn apply_configuration(
        &mut self,
        action: Value,
        command: ConfigurationCommand,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let baseline = self.owner_policy_baseline();
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
                snapshot: Box::new(self.snapshot()?),
                operation_id: self.active_operation_id.clone(),
                catalog: Box::new(self.provider_catalog_request()?),
            },
            &mut Vec::new(),
        )
        .await?;
        let permission_notice = (command == ConfigurationCommand::Permission)
            .then(|| baseline.resume_notice(self.session.access.abbreviation(), permission));
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

    /// Preserve authoritative rejection without weakening the fence for uncertain writes.
    #[tokio::test]
    async fn configuration_native_rejection_preserves_binding_while_uncertainty_fences()
    -> Result<()> {
        use forge_provider_api::{
            FailureDisposition, ProviderFailure, ProviderFailureKind, ProviderOperation,
        };
        for disposition in [FailureDisposition::Unchanged, FailureDisposition::Uncertain] {
            let workspace = super::super::test::repository();
            let data = tempfile::tempdir()?;
            let mut broker =
                super::super::test::planning_question_broker(workspace.path(), data.path(), false);
            let owner = crate::provider::ThreadOwner::root(&broker.session.id);
            let descriptor = forge_providers::registry::descriptor(
                &forge_providers::registry::nonselectable_id().unwrap(),
            )
            .unwrap();
            let native = crate::provider::test_support::EventFixtureProvider::new(
                descriptor,
                |request, _writer, start, _| async move {
                    start.observed(0).await?;
                    Ok(forge_provider_api::TurnResult::new(
                        crate::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                        1,
                        None,
                        request.input_coverage(),
                        Vec::new(),
                        None,
                    )
                    .expect("fixture accepted input coverage has no conflicts"))
                },
            );
            let reject = Arc::new(std::sync::atomic::AtomicBool::new(false));
            let bindings = Arc::new(std::sync::Mutex::new(Vec::new()));
            native.with_session_configure({
                let reject = reject.clone();
                let bindings = bindings.clone();
                move |binding, _| {
                    let reject = reject.load(Ordering::Acquire);
                    bindings.lock().unwrap().push(binding);
                    async move {
                        if reject {
                            return Err(ProviderError::Failure(
                                ProviderFailure::new(
                                    ProviderOperation::ThreadConfigure,
                                    ProviderFailureKind::ResourceConflict,
                                )
                                .at_phase(
                                    forge_provider_api::ProviderPhase::Admission,
                                    disposition,
                                ),
                            ));
                        }
                        Ok(())
                    }
                }
            });
            let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
            broker.set_provider_for_test(fixture.provider_handle());
            let registry = broker.execution_registry();
            let engine = registry.owner(owner.clone()).await?;
            broker
                .provider
                .thread_open(
                    broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
                )
                .await?;
            let original = serde_json::to_value(&broker.session)?;
            reject.store(true, Ordering::Release);
            let preparation = broker.prepare_configuration(json!({"session_tools":false}))?;
            let candidate = broker.build_configuration_candidate(preparation, &[])?;
            let error = broker
                .prepare_candidate_reconcile(&candidate)?
                .unwrap()
                .run()
                .await
                .err()
                .context("native configuration unexpectedly succeeded")?;
            let native_error = error
                .downcast_ref::<ProviderError>()
                .context("native failure identity was lost")?;
            assert!(matches!(native_error, ProviderError::Failure(failure)
                if failure.kind() == ProviderFailureKind::ResourceConflict && failure.disposition() == disposition));
            assert_eq!(serde_json::to_value(&broker.session)?, original);
            assert_eq!(
                broker.task_controller.configuration_is_uncertain(),
                disposition == FailureDisposition::Uncertain
            );
            if disposition == FailureDisposition::Unchanged {
                assert!(ConfigurationReconcileJob::unchanged_failure(native_error));
                registry.collect_cleanup().await?;
                assert!(engine.is_healthy().await);
                reject.store(false, Ordering::Release);
                let preparation = broker.prepare_configuration(json!({"session_tools":false}))?;
                let mut candidate = broker.build_configuration_candidate(preparation, &[])?;
                let result = broker
                    .prepare_candidate_reconcile(&candidate)?
                    .unwrap()
                    .run()
                    .await?;
                broker.certify_candidate_reconcile(&mut candidate, result)?;
                broker.commit_configuration_candidate(candidate)?;
                {
                    let bindings = bindings.lock().unwrap();
                    assert!(
                        bindings.windows(2).all(|pair| pair[0] == pair[1]),
                        "unchanged rejection replaced its binding"
                    );
                }
                let scope =
                    crate::provider::InvocationScope::new(owner, "after-rejected-configuration");
                let (sink, _events) = crate::provider::events::channel(scope);
                engine
                    .run_once(
                        broker.provider_request(
                            ProviderInput::from_text("Prompt after rejected configuration"),
                            PromptMode::Chat,
                        )?,
                        sink,
                        &broker.submission_store,
                    )
                    .await?;
                engine.close().await?;
            } else {
                assert!(!ConfigurationReconcileJob::unchanged_failure(native_error));
                assert!(
                    broker
                        .prepare_configuration_reconcile(
                            broker
                                .provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
                            Default::default(),
                        )
                        .is_err(),
                    "uncertain write admitted another configuration"
                );
            }
            broker.provider.shutdown().await?;
        }
        Ok(())
    }

    /// Select whether a completed binding receipt still authorizes the pending host commit.
    #[derive(Clone, Copy)]
    enum IdentityReceiptBoundary {
        InboxPending,
    }

    /// Persist ordered identity before inbox delivery.
    #[tokio::test]
    async fn configuration_identity_receipt_preserves_ordered_history() -> Result<()> {
        assert_identity_receipt_boundary(IdentityReceiptBoundary::InboxPending).await
    }

    async fn assert_identity_receipt_boundary(boundary: IdentityReceiptBoundary) -> Result<()> {
        {
            let workspace = super::super::test::repository();
            let data = tempfile::tempdir()?;
            let mut broker =
                super::super::test::planning_question_broker(workspace.path(), data.path(), false);
            let owner = crate::provider::ThreadOwner::root(&broker.session.id);
            let descriptor = forge_providers::registry::descriptor(
                &forge_providers::registry::nonselectable_id().unwrap(),
            )
            .unwrap();
            let native = crate::provider::test_support::EventFixtureProvider::new(
                descriptor,
                |_, _, _, _| async {
                    panic!("configuration-only fixture must not dispatch a native prompt")
                },
            );
            native.with_session_configure({
                let native = Arc::downgrade(&native);
                let owner = owner.clone();
                let configured = Arc::new(std::sync::atomic::AtomicUsize::new(0));
                move |_, _| {
                    let native = native
                        .upgrade()
                        .expect("configuration keeps its adapter alive");
                    let owner = owner.clone();
                    let history = format!(
                        "configured-history-{}",
                        configured.fetch_add(1, Ordering::AcqRel)
                    );
                    async move {
                        native
                            .event_sink(&owner)
                            .await?
                            .send_wait(forge_provider_api::AdapterActivity::Thread {
                                thread_id: owner.thread_id(),
                                event: forge_provider_api::ThreadEvent::IdentityResolved(
                                    history.try_into().unwrap(),
                                ),
                            })
                            .await?;
                        Ok(())
                    }
                }
            });
            let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
            broker.set_provider_for_test(fixture.provider_handle());
            let registry = broker.execution_registry();
            let engine = registry.owner(owner.clone()).await?;
            broker
                .provider
                .thread_open(
                    broker.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?,
                )
                .await?;
            let preparation = broker.prepare_configuration(json!({"session_tools":false}))?;
            let mut candidate = broker.build_configuration_candidate(preparation, &[])?;
            let result = broker
                .prepare_candidate_reconcile(&candidate)?
                .context("tool change has no native reconciliation")?
                .run()
                .await?;
            assert!(
                broker.session.provider_session_id.is_none(),
                "fixture consumed its inbox before receipt certification"
            );
            match boundary {
                IdentityReceiptBoundary::InboxPending => {
                    broker.certify_candidate_reconcile(&mut candidate, result)?;
                    broker.commit_configuration_candidate(candidate)?;
                    assert_eq!(
                        broker.session.provider_session_id.as_deref(),
                        Some("configured-history-0")
                    );
                    let preparation =
                        broker.prepare_configuration(json!({"session_tools":true}))?;
                    let mut candidate = broker.build_configuration_candidate(preparation, &[])?;
                    let result = broker
                        .prepare_candidate_reconcile(&candidate)?
                        .context("second tool change has no reconciliation")?
                        .run()
                        .await?;
                    broker.certify_candidate_reconcile(&mut candidate, result)?;
                    broker.commit_configuration_candidate(candidate)?;
                    assert_eq!(
                        broker.session.provider_session_id.as_deref(),
                        Some("configured-history-1")
                    );
                    let first_identity: forge_provider_api::ProviderSessionId =
                        "configured-history-0".to_owned().try_into()?;
                    let second_identity: forge_provider_api::ProviderSessionId =
                        "configured-history-1".to_owned().try_into()?;
                    let is_identity =
                        |observation: &crate::provider::session_execution::OwnerObservation,
                         expected: &forge_provider_api::ProviderSessionId| {
                            if let crate::provider::session_execution::OwnerObservation::Event(
                                HarnessEvent::Native { event, .. },
                            ) = observation
                            {
                                matches!(event.as_ref(), ProviderEvent::Thread { event: forge_provider_api::ThreadEvent::IdentityResolved(id), .. } if id == expected)
                            } else {
                                false
                            }
                        };
                    let mut retained = Vec::new();
                    async {
                        loop {
                            retained.extend(broker.observer.session_events().drain()?);
                            if [&first_identity, &second_identity].iter().all(|expected| {
                                retained.iter().any(|event| is_identity(event, expected))
                            }) {
                                break;
                            }
                            tokio::task::yield_now().await;
                        }
                        Ok::<_, anyhow::Error>(())
                    }
                    .await?;
                    let prior_identity = retained
                        .iter()
                        .find(|event| is_identity(event, &first_identity))
                        .context("original identity frame missing")?
                        .clone();
                    let current_identity = retained
                        .iter()
                        .find(|event| is_identity(event, &second_identity))
                        .context("current identity frame missing")?
                        .clone();
                    broker.observe_session_event(prior_identity)?;
                    assert_eq!(
                        broker.session.provider_session_id.as_deref(),
                        Some("configured-history-1")
                    );
                    assert_eq!(
                        broker
                            .store
                            .load_session(&broker.session.id)?
                            .unwrap()
                            .provider_session_id
                            .as_deref(),
                        Some("configured-history-1")
                    );
                    broker.drain_session_events()?;
                    assert_eq!(
                        broker.session.provider_session_id.as_deref(),
                        Some("configured-history-1")
                    );
                    broker.observe_session_event(current_identity)?;
                    let (mut exchange, _, _) = broker
                        .test_stage_interaction_for_turn("Preserve configured identity", true, 100)
                        .await?;
                    for identity in [
                        None,
                        Some("configured-history-0"),
                        Some("configured-history-1"),
                    ] {
                        let completion = crate::provider::ProviderCompletion {
                            native: forge_provider_api::completion::ProviderCompletion {
                                provider_session_id: identity
                                    .map(|id| id.to_owned().try_into().unwrap()),
                                capability: broker.capability.clone(),
                                ..Default::default()
                            },
                            ..Default::default()
                        };
                        broker.stage_host_completion(
                            &mut exchange,
                            completion,
                            PromptMode::Chat,
                        )?;
                        assert_eq!(
                            broker.session.provider_session_id.as_deref(),
                            Some("configured-history-1"),
                            "terminal identity {identity:?} replaced the current configured history"
                        );
                        assert_eq!(
                            broker
                                .store
                                .load_session(&broker.session.id)?
                                .unwrap()
                                .provider_session_id
                                .as_deref(),
                            Some("configured-history-1")
                        );
                    }
                    assert_eq!(
                        broker.session.provider_session_id.as_deref(),
                        Some("configured-history-1")
                    );
                    assert!(!broker.task_controller.configuration_is_uncertain());
                }
            }
            registry.collect_cleanup().await?;
            engine.close().await?;
            fixture.provider_handle().shutdown().await?;
        }
        Ok(())
    }

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
        assert!(broker.task_controller.configuration_is_uncertain());
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
}
