use std::{collections::HashMap, path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};

use super::{AgentSelection, SpawnAgentRequest};
use crate::provider::{
    PromptMode, Provider, ProviderInput, ProviderKind, ProviderLaunch, ProviderRequest,
};

/// Resolves configured adapters while retaining the root's portable preference scope.
#[derive(Clone)]
pub struct AgentProviderFactory {
    #[cfg(test)]
    provider_by_provider: HashMap<String, Arc<Provider>>,
    launch_by_provider: HashMap<String, ProviderLaunch>,
    permission: Arc<crate::provider::approval::PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    settings_root: PathBuf,
    pub profiles: Arc<super::AgentProfileStore>,
}

impl AgentProviderFactory {
    pub fn new(
        root: ProviderLaunch,
        configured: HashMap<String, ProviderLaunch>,
        permission: Arc<crate::provider::approval::PermissionCoordinator>,
        trace: Arc<crate::trace::TraceStore>,
        settings_root: PathBuf,
    ) -> Self {
        let mut launch_by_provider = configured;
        launch_by_provider.insert(root.kind.clone(), root);
        let profiles = Arc::new(super::AgentProfileStore::new(
            settings_root.join("subagents.json"),
        ));
        Self {
            launch_by_provider,
            permission,
            trace,
            settings_root,
            profiles,
            #[cfg(test)]
            provider_by_provider: HashMap::new(),
        }
    }

    pub fn build(&self, provider: ProviderKind) -> Result<Arc<Provider>> {
        let name = provider_name(provider);
        #[cfg(test)]
        if let Some(provider) = self.provider_by_provider.get(name) {
            return Ok(provider.clone());
        }
        let launch = self
            .launch_by_provider
            .get(name)
            .cloned()
            .unwrap_or_else(|| ProviderLaunch {
                kind: name.into(),
                command: Vec::new(),
            });
        ensure!(
            ProviderKind::parse(&launch.kind)? == provider,
            "configured child adapter has a different provider kind"
        );
        Ok(Arc::from(crate::provider::build(
            launch,
            self.permission.clone(),
            self.trace.clone(),
            self.settings_root.clone(),
        )?))
    }

    /// Loads selectable models and capability constraints from the same child adapter.
    pub async fn catalog(
        &self,
        provider: ProviderKind,
        mut request: ProviderRequest,
    ) -> Result<AgentModelCatalog> {
        ensure!(
            request.owner()?.is_root(),
            "child model catalogs require root session ownership"
        );
        request.input = ProviderInput::from_text(String::new());
        request.model = "default".into();
        request.effort = "default".into();
        request.provider_session_id = None;
        request.control_context = None;
        request.access =
            crate::provider::registered_capability(provider).resolve_access(request.access);
        let provider = self.build(provider)?;
        let capability = provider.descriptor().capability;
        let model = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            provider.model_list(request),
        )
        .await
        .context("child model catalog exceeded 30 seconds");
        let cleanup = tokio::time::timeout(std::time::Duration::from_secs(30), provider.shutdown())
            .await
            .context("child model catalog cleanup exceeded 30 seconds");
        let model = model??;
        cleanup??;
        Ok(AgentModelCatalog {
            models: model,
            capability,
        })
    }

    #[cfg(test)]
    pub(crate) fn register(&mut self, provider: Arc<Provider>) {
        self.provider_by_provider
            .insert(provider_name(provider.descriptor().kind).into(), provider);
    }

    pub async fn resolve(
        &self,
        root: &ProviderRequest,
        root_provider: ProviderKind,
        spawn: &SpawnAgentRequest,
    ) -> Result<AgentSelection> {
        let (profile_name, profile) = self.profiles.resolve(spawn.profile.as_deref())?;
        let provider = if profile.provider == "inherit" {
            root_provider
        } else {
            ProviderKind::parse(&profile.provider)?
        };
        ensure!(
            provider != ProviderKind::Mock || root_provider == ProviderKind::Mock,
            "mock is not a user-selectable provider"
        );
        let same_provider = provider == root_provider;
        let model = if profile.model == "inherit" {
            root.model.clone()
        } else {
            profile.model.clone()
        };
        let effort = if profile.reasoning_effort == "inherit" {
            root.effort.clone()
        } else {
            profile.reasoning_effort.clone()
        };
        let service_tier = if profile.service_tier == "inherit" {
            root.service_tier
        } else {
            serde_json::from_value(serde_json::json!(profile.service_tier))?
        };
        let approval_mode = if profile.approval_mode == "inherit" {
            root.approval_mode
        } else {
            narrow_permission(
                root.approval_mode,
                serde_json::from_value(serde_json::json!(profile.approval_mode))?,
            )
        };
        let mut selection = AgentSelection {
            profile: profile_name,
            instructions: profile.instructions,
            provider,
            model,
            reasoning_effort: effort,
            context_window: if same_provider && profile.model == "inherit" {
                root.context_window.clone()
            } else {
                None
            },
            service_tier,
            access: crate::provider::registered_capability(provider)
                .resolve_access(root.access.clone()),
            approval_mode,
        };
        if profile.model != "inherit" || profile.reasoning_effort != "inherit" || !same_provider {
            ensure!(
                same_provider || profile.model != "inherit" || root.model != "default",
                "profile {} inherits an unresolved model from another provider. Select a model or default explicitly",
                selection.profile
            );
            let model_list = self.catalog(provider, root.clone()).await?.models;
            let model = if selection.model == "default" {
                model_list
                    .iter()
                    .find(|model| model.is_default)
                    .or_else(|| model_list.first())
            } else {
                model_list.iter().find(|model| model.id == selection.model)
            }
            .with_context(|| {
                format!(
                    "model {} is not available on {}",
                    selection.model,
                    provider_name(provider)
                )
            })?;
            selection.model = model.id.clone();
            selection.service_tier.validate_model(model)?;
            if selection.reasoning_effort == "default" {
                selection.reasoning_effort = model
                    .default_reasoning
                    .clone()
                    .unwrap_or_else(|| "default".into());
            } else {
                ensure!(
                    model
                        .reasoning
                        .iter()
                        .any(|effort| effort == &selection.reasoning_effort),
                    "reasoning effort {} is not supported by model {}",
                    selection.reasoning_effort,
                    selection.model
                );
            }
            if !selection.context_window.as_ref().is_some_and(|selected| {
                model
                    .context_window
                    .iter()
                    .any(|window| &window.id == selected)
            }) {
                selection.context_window = model.default_context_window.clone();
            }
        }
        Ok(selection)
    }

    pub fn request(
        &self,
        root: &ProviderRequest,
        work: &super::AgentWork,
        message: String,
    ) -> Result<ProviderRequest> {
        let agent = &work.agent;
        let selection = agent
            .selection
            .as_ref()
            .context("child has no resolved provider selection")?;
        ensure!(
            work.owner().session_id == root.harness_session_id && !work.owner().is_root(),
            "child request crosses root ownership"
        );
        let message = if selection.instructions.trim().is_empty() {
            message
        } else {
            format!(
                "Subagent profile instructions:\n{}\n\nAssigned task:\n{message}",
                selection.instructions
            )
        };
        Ok(ProviderRequest {
            execution_owner: Some(work.owner().clone()),
            harness_session_id: root.harness_session_id.clone(),
            workspace: root.workspace.clone(),
            input: ProviderInput::from_text(message),
            mode: PromptMode::Chat,
            model: selection.model.clone(),
            effort: selection.reasoning_effort.clone(),
            context_window: selection.context_window.clone(),
            service_tier: selection.service_tier,
            access: crate::provider::registered_capability(selection.provider)
                .resolve_access(root.access.clone()),
            approval_mode: narrow_permission(root.approval_mode, selection.approval_mode),
            provider_session_id: agent.provider_thread_id.clone(),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        })
    }
}

#[derive(serde::Serialize)]
/// Couples a provider's model list with the constraints used by profile editors.
pub struct AgentModelCatalog {
    /// Preserves the order advertised by the provider.
    pub models: Vec<crate::provider::ProviderModel>,
    /// Controls which tuning options the provider accepts.
    pub capability: crate::provider::ProviderCapability,
}

fn narrow_permission(
    root: crate::session::ApprovalMode,
    selected: crate::session::ApprovalMode,
) -> crate::session::ApprovalMode {
    use crate::session::ApprovalMode;
    match (root, selected) {
        (ApprovalMode::Deny, _) | (_, ApprovalMode::Deny) => ApprovalMode::Deny,
        (ApprovalMode::Ask, _) | (_, ApprovalMode::Ask) => ApprovalMode::Ask,
        _ => ApprovalMode::Allow,
    }
}

pub fn provider_name(provider: ProviderKind) -> &'static str {
    match provider {
        ProviderKind::Codex => "codex",
        ProviderKind::Copilot => "copilot",
        ProviderKind::Claude => "claude",
        ProviderKind::Antigravity => "antigravity",
        ProviderKind::Mock => "mock",
    }
}
