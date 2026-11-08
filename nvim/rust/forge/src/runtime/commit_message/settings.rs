use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use forge_harness::{provider::ServiceTier, session::AutoCompaction, settings::SettingsStore};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ModelChoice {
    model: String,
    thinking: Option<String>,
    #[serde(default)]
    service_tier: ServiceTier,
}

/// Background preferences shared by every workspace.
///
/// `provider` selects the Background provider independently of any interactive chat. Saved
/// settings written before it existed decode without it and fall back to the defaults.
#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Preferences {
    auto_compaction: AutoCompaction,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    provider: Option<String>,
    models: BTreeMap<String, ModelChoice>,
}

/// Reject a provider whose registry entry cannot run tool-free Background sessions.
fn validate_provider(provider: &str) -> Result<()> {
    let kind = forge_providers::registry::select(provider)?;
    ensure!(
        forge_providers::registry::resolve(&kind)?
            .capability
            .integration
            .background_generation,
        "{provider} does not support Background sessions"
    );
    Ok(())
}

impl Preferences {
    fn validate(&self) -> Result<()> {
        self.auto_compaction.validate(None)?;
        if let Some(provider) = &self.provider {
            validate_provider(provider)?;
        }
        for (provider, choice) in &self.models {
            ensure!(
                !provider.trim().is_empty() && !choice.model.trim().is_empty(),
                "Background model cannot be empty"
            );
            validate_provider(provider)?;
            if let Some(thinking) = &choice.thinking {
                ensure!(
                    !thinking.trim().is_empty(),
                    "Background thinking setting cannot be empty"
                );
            }
        }
        Ok(())
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SettingsRequest {
    data_root: PathBuf,
    defaults: Preferences,
    expected_revision: Option<u64>,
    settings: Option<Preferences>,
}

fn apply(request: SettingsRequest) -> Result<Value> {
    request.defaults.validate()?;
    let mut store = SettingsStore::open(&request.data_root)?;
    let mut snapshot = store.get("", "", "background")?;
    if let Some(settings) = request.settings {
        settings.validate()?;
        store.save_provider(
            request
                .expected_revision
                .context("Background settings require an expected revision")?,
            "background",
            &serde_json::to_value(settings)?,
        )?;
        snapshot = store.get("", "", "background")?;
    }
    let mut settings = request.defaults;
    if !snapshot.provider.is_null() {
        let saved: Preferences = serde_json::from_value(snapshot.provider)?;
        saved.validate()?;
        settings.auto_compaction = saved.auto_compaction;
        settings.provider = saved.provider.or(settings.provider);
        settings.models.extend(saved.models);
    }
    if settings.provider.is_none() {
        settings.provider = forge_providers::registry::background_default_id()
            .map(|provider| provider.as_str().to_owned());
    }
    Ok(json!({"revision": snapshot.revision, "settings": settings}))
}

/// Read or atomically save global Background preferences without acquiring a conversation lease.
pub async fn dispatch(params: Value) -> Result<Value> {
    let request: SettingsRequest = serde_json::from_value(params)?;
    tokio::task::spawn_blocking(move || apply(request)).await?
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request(
        data_root: &std::path::Path,
        defaults: Value,
        settings: Option<Value>,
        revision: Option<u64>,
    ) -> SettingsRequest {
        serde_json::from_value(json!({
            "data_root": data_root, "defaults": defaults,
            "expected_revision": revision, "settings": settings,
        }))
        .unwrap()
    }

    fn defaults() -> Value {
        json!({"auto_compaction": {"enabled": true, "threshold_tokens": 128000},
            "models": {"codex": {"model": "gpt-6-luna", "thinking": "low"}}})
    }

    /// Settings saved before the provider field existed still load and take the registry default.
    #[test]
    fn saved_settings_without_provider_resolve_the_registry_default() {
        let directory = tempfile::tempdir().unwrap();
        let legacy =
            json!({"auto_compaction": {"enabled": false, "threshold_tokens": 64000}, "models": {}});
        apply(request(directory.path(), defaults(), Some(legacy), Some(0))).unwrap();
        let result = apply(request(directory.path(), defaults(), None, None)).unwrap();
        assert_eq!(result["settings"]["provider"], "codex");
        assert_eq!(result["settings"]["auto_compaction"]["enabled"], false);
    }

    /// A chosen Background provider survives later saves that change other preferences.
    #[test]
    fn saved_provider_survives_later_saves() {
        let directory = tempfile::tempdir().unwrap();
        let mut chosen = defaults();
        chosen["provider"] = json!("copilot");
        let saved = apply(request(directory.path(), defaults(), Some(chosen), Some(0))).unwrap();
        let mut changed = saved["settings"].clone();
        changed["auto_compaction"]["enabled"] = json!(false);
        changed.as_object_mut().unwrap().remove("provider");
        let revision = saved["revision"].as_u64();
        let result = apply(request(
            directory.path(),
            defaults(),
            Some(changed),
            revision,
        ))
        .unwrap();
        assert_eq!(result["settings"]["provider"], "copilot");
    }

    /// A provider that cannot run Background sessions fails loudly instead of being reset.
    #[test]
    fn unsupported_provider_is_rejected() {
        let directory = tempfile::tempdir().unwrap();
        let mut chosen = defaults();
        chosen["provider"] = json!("antigravity");
        let error = apply(request(directory.path(), defaults(), Some(chosen), Some(0)))
            .expect_err("antigravity cannot run Background sessions");
        assert!(
            error
                .to_string()
                .contains("does not support Background sessions")
        );
    }
}
