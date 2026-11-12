use super::*;
use serde::{Deserialize, Serialize};

/// Provider-local defaults accepted without opening or mutating a conversation.
#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ProviderDefaults {
    #[serde(skip_serializing_if = "Option::is_none")]
    model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    effort: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    context_window: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    service_tier: Option<crate::provider::ServiceTier>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ask_model: Option<crate::settings::AskModel>,
}

pub(super) fn save(
    settings: &mut crate::settings::SettingsStore,
    revision: u64,
    provider: &str,
    value: &Value,
) -> Result<()> {
    let patch: ProviderDefaults = serde_json::from_value(value.clone())?;
    for value in [&patch.model, &patch.effort, &patch.context_window]
        .into_iter()
        .flatten()
    {
        ensure!(
            !value.trim().is_empty() && value.len() <= 256,
            "invalid provider default"
        );
    }
    settings.save_provider(revision, provider, &serde_json::to_value(patch)?)?;
    Ok(())
}

/// Query provider metadata with workspace context only; no session controller is resolved.
pub(super) async fn catalog(
    registry: &SessionControllerRegistry,
    provider: &str,
    workspace: &str,
) -> Result<Value> {
    let kind = forge_providers::registry::select(provider)?;
    let launch = registry
        .initialize
        .provider_launch_by_provider
        .get(&kind)
        .cloned()
        .or_else(|| {
            (kind == registry.initialize.provider.kind)
                .then(|| registry.initialize.provider.clone())
        })
        .unwrap_or_else(|| crate::provider::ProviderLaunch {
            kind,
            command: Vec::new(),
        });
    let adapter = crate::provider::build(
        launch,
        Arc::new(crate::provider::approval::PermissionCoordinator::new(
            registry.runtime.permission_store.clone(),
        )),
        registry.runtime.trace(),
        PathBuf::from(&registry.initialize.data_root),
    )?;
    let capability = adapter.descriptor().capability;
    let result = tokio::time::timeout(
        Duration::from_secs(30),
        adapter.model_list(forge_provider_api::QueryContext::Workspace(
            forge_provider_api::WorkspaceContext {
                workspace: workspace.to_owned(),
            },
        )),
    )
    .await
    .context("defaults model catalog exceeded 30 seconds");
    let cleanup = tokio::time::timeout(Duration::from_secs(30), adapter.shutdown())
        .await
        .context("defaults model catalog cleanup exceeded 30 seconds");
    let models = result??;
    cleanup??;
    Ok(json!({"models":models,"capability":capability}))
}
