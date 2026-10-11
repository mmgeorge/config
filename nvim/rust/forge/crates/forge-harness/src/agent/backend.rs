use std::{collections::HashMap, path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};

use super::{Agent, AgentSelection, SpawnAgentRequest};
use crate::backend::{Backend, BackendInput, BackendKind, BackendLaunch, BackendRequest, ExecutionOwner, PromptMode};

/// Resolves configured adapters while retaining the root's portable preference scope.
#[derive(Clone)]
pub struct AgentBackendFactory {
    #[cfg(test)]
    backend_by_provider: HashMap<String,Arc<dyn Backend>>,
    launch_by_provider: HashMap<String, BackendLaunch>,
    permission: Arc<crate::backend::approval::PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    settings_root: PathBuf,
}

impl AgentBackendFactory {
    pub fn new(root: BackendLaunch, configured: HashMap<String, BackendLaunch>,
        permission: Arc<crate::backend::approval::PermissionCoordinator>, trace: Arc<crate::trace::TraceStore>, settings_root: PathBuf) -> Self {
        let mut launch_by_provider = configured;
        launch_by_provider.insert(root.kind.clone(), root);
        Self { launch_by_provider, permission, trace, settings_root,
            #[cfg(test)] backend_by_provider:HashMap::new() }
    }

    pub fn build(&self, provider: BackendKind) -> Result<Arc<dyn Backend>> {
        let name = provider_name(provider);
        #[cfg(test)]
        if let Some(backend) = self.backend_by_provider.get(name) {return Ok(backend.clone());}
        let launch = self.launch_by_provider.get(name).cloned().unwrap_or_else(|| BackendLaunch {
            kind: name.into(), command: Vec::new() });
        ensure!(BackendKind::parse(&launch.kind)? == provider, "configured child adapter has a different provider kind");
        Ok(Arc::from(crate::backend::build(launch, self.permission.clone(), self.trace.clone(), self.settings_root.clone())?))
    }

    pub async fn model_list(&self, provider: BackendKind, mut request: BackendRequest) -> Result<Vec<crate::backend::BackendModel>> {
        ensure!(request.owner()?.is_root(), "child model catalogs require root session ownership");
        request.input = BackendInput::from_text(String::new());
        request.model = "default".into();
        request.effort = "default".into();
        request.backend_session_id = None;
        request.control_context = None;
        let backend = self.build(provider)?;
        let model = tokio::time::timeout(std::time::Duration::from_secs(30),backend.model_list(request)).await
            .context("child model catalog exceeded 30 seconds");
        let cleanup = tokio::time::timeout(std::time::Duration::from_secs(30),backend.shutdown()).await
            .context("child model catalog cleanup exceeded 30 seconds");
        let model = model??;
        cleanup??;
        Ok(model)
    }

    #[cfg(test)]
    pub(crate) fn register(&mut self, backend: Arc<dyn Backend>) {
        self.backend_by_provider.insert(provider_name(backend.descriptor().kind).into(),backend);
    }

    pub async fn resolve(&self, root: &BackendRequest, root_provider: BackendKind, spawn: &SpawnAgentRequest) -> Result<AgentSelection> {
        let provider = spawn.provider.as_deref().map(BackendKind::parse).transpose()?.unwrap_or(root_provider);
        ensure!(provider != BackendKind::Mock || root_provider == BackendKind::Mock, "mock is not a user-selectable provider");
        validate_access(provider, &root.access)?;
        let same_provider = provider == root_provider;
        let defaults = crate::settings::SettingsStore::open(&self.settings_root)?.get(&root.workspace, &root.harness_session_id, provider_name(provider))?;
        let configured_model = defaults.provider.get("model").and_then(serde_json::Value::as_str).unwrap_or("default");
        let configured_effort = defaults.provider.get("effort").and_then(serde_json::Value::as_str).unwrap_or("default");
        let model = spawn.model.clone().unwrap_or_else(|| if same_provider { root.model.clone() } else { configured_model.into() });
        let effort = spawn.reasoning_effort.clone().unwrap_or_else(|| if same_provider && spawn.model.is_none() { root.effort.clone() } else { configured_effort.into() });
        let configured_context:Option<String> = defaults.provider.get("context_window")
            .map(|value|serde_json::from_value(value.clone())).transpose()?.flatten();
        let configured_tier = defaults.provider.get("service_tier")
            .map(|value|serde_json::from_value(value.clone())).transpose()?.unwrap_or_default();
        let mut selection = AgentSelection { provider, model, reasoning_effort: effort,
            context_window: if spawn.model.is_none() {
                if same_provider { root.context_window.clone() } else { configured_context }
            } else { None },
            service_tier: if same_provider { root.service_tier } else { configured_tier },
            access: root.access.clone(), execution_mode: root.execution_mode };
        if spawn.model.is_some() || spawn.reasoning_effort.is_some() || !same_provider {
            let model_list = self.model_list(provider,root.clone()).await?;
            let model = if selection.model == "default" {
                model_list.iter().find(|model|model.is_default).or_else(||model_list.first())
            } else { model_list.iter().find(|model|model.id == selection.model) }
                .with_context(||format!("model {} is not available on {}", selection.model, provider_name(provider)))?;
            selection.model = model.id.clone();
            if spawn.reasoning_effort.is_some() {
                ensure!(model.reasoning.iter().any(|effort|effort == &selection.reasoning_effort),
                    "reasoning effort {} is not supported by model {}", selection.reasoning_effort, selection.model);
            } else if !model.reasoning.iter().any(|effort|effort == &selection.reasoning_effort) {
                selection.reasoning_effort = model.default_reasoning.clone().unwrap_or_default();
            }
            if !selection.context_window.as_ref().is_some_and(|selected|
                model.context_window.iter().any(|window|&window.id == selected)) {
                selection.context_window = model.default_context_window.clone();
            }
        }
        Ok(selection)
    }

    pub fn request(&self, root: &BackendRequest, agent: &Agent, message: String) -> Result<BackendRequest> {
        let selection = agent.selection.as_ref().context("child has no resolved provider selection")?;
        validate_access(selection.provider, &root.access)?;
        ensure!(agent.session_id == root.harness_session_id && !agent.is_primary(), "child request crosses root ownership");
        Ok(BackendRequest { execution_owner: Some(ExecutionOwner { session_id: agent.session_id.clone(), agent_id: agent.id.clone() }),
            harness_session_id: root.harness_session_id.clone(), workspace: root.workspace.clone(), input: BackendInput::from_text(message),
            mode: PromptMode::Chat, model: selection.model.clone(), effort: selection.reasoning_effort.clone(),
            context_window: selection.context_window.clone(), service_tier: selection.service_tier, access: root.access.clone(),
            execution_mode: root.execution_mode, backend_session_id: agent.provider_thread_id.clone(), control_context: None })
    }
}

fn validate_access(provider: BackendKind, access: &crate::session::AccessPolicy) -> Result<()> {
    ensure!(!access.sandbox || matches!(provider, BackendKind::Codex | BackendKind::Mock),
        "selected backend cannot enforce the root's OS sandbox. Change the root access policy explicitly before spawning");
    Ok(())
}

pub fn provider_name(provider: BackendKind) -> &'static str {
    match provider { BackendKind::Codex => "codex", BackendKind::Copilot => "copilot", BackendKind::Claude => "claude",
        BackendKind::Antigravity => "antigravity", BackendKind::Mock => "mock" }
}
