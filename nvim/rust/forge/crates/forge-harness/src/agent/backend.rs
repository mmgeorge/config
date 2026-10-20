use std::{collections::HashMap, path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};

use super::{Agent, AgentSelection, SpawnAgentRequest};
use crate::backend::{
    Backend, BackendInput, BackendKind, BackendLaunch, BackendRequest, ExecutionOwner, PromptMode,
};

/// Resolves configured adapters while retaining the root's portable preference scope.
#[derive(Clone)]
pub struct AgentBackendFactory {
    #[cfg(test)]
    backend_by_provider: HashMap<String, Arc<dyn Backend>>,
    launch_by_provider: HashMap<String, BackendLaunch>,
    permission: Arc<crate::backend::approval::PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    settings_root: PathBuf,
    pub profiles: Arc<super::AgentProfileStore>,
}

impl AgentBackendFactory {
    pub fn new(
        root: BackendLaunch,
        configured: HashMap<String, BackendLaunch>,
        permission: Arc<crate::backend::approval::PermissionCoordinator>,
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
            backend_by_provider: HashMap::new(),
        }
    }

    pub fn build(&self, provider: BackendKind) -> Result<Arc<dyn Backend>> {
        let name = provider_name(provider);
        #[cfg(test)]
        if let Some(backend) = self.backend_by_provider.get(name) {
            return Ok(backend.clone());
        }
        let launch = self
            .launch_by_provider
            .get(name)
            .cloned()
            .unwrap_or_else(|| BackendLaunch {
                kind: name.into(),
                command: Vec::new(),
            });
        ensure!(
            BackendKind::parse(&launch.kind)? == provider,
            "configured child adapter has a different provider kind"
        );
        Ok(Arc::from(crate::backend::build(
            launch,
            self.permission.clone(),
            self.trace.clone(),
            self.settings_root.clone(),
        )?))
    }

    /// Loads selectable models and capability constraints from the same child adapter.
    pub async fn catalog(
        &self,
        provider: BackendKind,
        mut request: BackendRequest,
    ) -> Result<AgentModelCatalog> {
        ensure!(
            request.owner()?.is_root(),
            "child model catalogs require root session ownership"
        );
        request.input = BackendInput::from_text(String::new());
        request.model = "default".into();
        request.effort = "default".into();
        request.backend_session_id = None;
        request.control_context = None;
        request.access = provider.resolve_access(request.access);
        let backend = self.build(provider)?;
        let capability = backend.descriptor().capability;
        let model = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            backend.model_list(request),
        )
        .await
        .context("child model catalog exceeded 30 seconds");
        let cleanup = tokio::time::timeout(std::time::Duration::from_secs(30), backend.shutdown())
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
    pub(crate) fn register(&mut self, backend: Arc<dyn Backend>) {
        self.backend_by_provider
            .insert(provider_name(backend.descriptor().kind).into(), backend);
    }

    pub async fn resolve(
        &self,
        root: &BackendRequest,
        root_provider: BackendKind,
        spawn: &SpawnAgentRequest,
    ) -> Result<AgentSelection> {
        let (profile_name, profile) = self.profiles.resolve(spawn.profile.as_deref())?;
        let provider = if profile.provider == "inherit" {
            root_provider
        } else {
            BackendKind::parse(&profile.provider)?
        };
        ensure!(
            provider != BackendKind::Mock || root_provider == BackendKind::Mock,
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
        let execution_mode = if profile.permission_mode == "inherit" {
            root.execution_mode
        } else {
            narrow_permission(
                root.execution_mode,
                serde_json::from_value(serde_json::json!(profile.permission_mode))?,
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
            access: provider.resolve_access(root.access.clone()),
            execution_mode,
        };
        ensure!(
            service_tier == crate::backend::ServiceTier::Standard
                || matches!(provider, BackendKind::Codex | BackendKind::Mock),
            "profile {} requests a speed tier unsupported by {}",
            selection.profile,
            provider_name(provider)
        );
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
        root: &BackendRequest,
        agent: &Agent,
        message: String,
    ) -> Result<BackendRequest> {
        let selection = agent
            .selection
            .as_ref()
            .context("child has no resolved provider selection")?;
        ensure!(
            agent.session_id == root.harness_session_id && !agent.is_primary(),
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
        Ok(BackendRequest {
            execution_owner: Some(ExecutionOwner {
                session_id: agent.session_id.clone(),
                agent_id: agent.id.clone(),
            }),
            harness_session_id: root.harness_session_id.clone(),
            workspace: root.workspace.clone(),
            input: BackendInput::from_text(message),
            mode: PromptMode::Chat,
            model: selection.model.clone(),
            effort: selection.reasoning_effort.clone(),
            context_window: selection.context_window.clone(),
            service_tier: selection.service_tier,
            access: selection.provider.resolve_access(root.access.clone()),
            execution_mode: narrow_permission(root.execution_mode, selection.execution_mode),
            backend_session_id: agent.provider_thread_id.clone(),
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
    pub models: Vec<crate::backend::BackendModel>,
    /// Controls which tuning options the provider accepts.
    pub capability: crate::backend::BackendCapability,
}

fn narrow_permission(
    root: crate::session::PermissionMode,
    selected: crate::session::PermissionMode,
) -> crate::session::PermissionMode {
    use crate::session::PermissionMode;
    match (root, selected) {
        (PermissionMode::Read, _) | (_, PermissionMode::Read) => PermissionMode::Read,
        (PermissionMode::Write, _) | (_, PermissionMode::Write) => PermissionMode::Write,
        _ => PermissionMode::Yolo,
    }
}

pub fn provider_name(provider: BackendKind) -> &'static str {
    match provider {
        BackendKind::Codex => "codex",
        BackendKind::Copilot => "copilot",
        BackendKind::Claude => "claude",
        BackendKind::Antigravity => "antigravity",
        BackendKind::Mock => "mock",
    }
}
