use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use forge_harness::{settings::SettingsStore, session::AutoCompaction};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ModelChoice {
    model: String,
    thinking: Option<String>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Preferences {
    auto_compaction: AutoCompaction,
    models: BTreeMap<String, ModelChoice>,
}

impl Preferences {
    fn validate(&self) -> Result<()> {
        self.auto_compaction.validate(None)?;
        for (provider, choice) in &self.models {
            ensure!(!provider.trim().is_empty() && !choice.model.trim().is_empty(), "Background model cannot be empty");
            ensure!(provider != "gemini", "Background sessions require a Harness provider");
            if let Some(thinking) = &choice.thinking {
                ensure!(["none", "minimal", "low", "medium", "high", "xhigh", "max"].contains(&thinking.as_str()), "Invalid Background thinking setting");
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
        store.save_provider(request.expected_revision.context("Background settings require an expected revision")?,
            "background", &serde_json::to_value(settings)?)?;
        snapshot = store.get("", "", "background")?;
    }
    let mut settings = request.defaults;
    if !snapshot.provider.is_null() {
        let saved: Preferences = serde_json::from_value(snapshot.provider)?;
        saved.validate()?;
        settings.auto_compaction = saved.auto_compaction;
        settings.models.extend(saved.models);
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
    #[test]
    fn defaults_persist_and_reject_stale_updates() -> Result<()> {
        let root = tempfile::tempdir()?;
        let make = || serde_json::from_value::<SettingsRequest>(json!({"data_root":root.path(),"defaults":{
            "models":{"codex":{"model":"gpt-6-luna","thinking":"low"}},
            "auto_compaction":{"enabled":true,"threshold_tokens":128000}}})).unwrap();
        let first = apply(make())?;
        let mut update = make();
        update.settings = Some(serde_json::from_value(first["settings"].clone())?);
        update.expected_revision = Some(0);
        assert_eq!(apply(update)?["revision"],1);
        assert_eq!(apply(make())?["settings"],first["settings"]);
        let mut stale = make(); stale.settings = Some(stale.defaults.clone()); stale.expected_revision=Some(0);
        assert!(apply(stale).is_err());
        Ok(())
    }
}
