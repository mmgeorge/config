use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use forge_harness::settings::SettingsStore;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
enum Source { Harness, Gemini }

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct ModelChoice {
    model: String,
    thinking: Option<Value>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct Preferences {
    source: Source,
    models: BTreeMap<String, ModelChoice>,
}

impl Preferences {
    fn validate(&self) -> Result<()> {
        for (provider, choice) in &self.models {
            ensure!(!provider.trim().is_empty() && !choice.model.trim().is_empty(), "Commit model cannot be empty");
            if let Some(thinking) = &choice.thinking {
                if provider == "gemini" {
                    ensure!(thinking.is_u64() || thinking.as_str().is_some_and(|value|
                        ["minimal", "low", "medium", "high"].contains(&value)), "Invalid Gemini thinking setting");
                } else {
                    ensure!(thinking.as_str().is_some_and(|value|
                        ["none", "minimal", "low", "medium", "high", "xhigh", "max"].contains(&value)),
                        "Invalid Harness thinking setting");
                }
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

fn apply(request: SettingsRequest, gemini_available: bool) -> Result<Value> {
    request.defaults.validate()?;
    let mut store = SettingsStore::open(&request.data_root)?;
    let mut snapshot = store.get("", "", "commit_generation")?;
    if let Some(settings) = request.settings {
        settings.validate()?;
        ensure!(gemini_available || !matches!(settings.source, Source::Gemini), "GEMINI_API_KEY is not available");
        store.save_provider(request.expected_revision.context("Commit settings require an expected revision")?,
            "commit_generation", &serde_json::to_value(settings)?)?;
        snapshot = store.get("", "", "commit_generation")?;
    }
    let mut settings = request.defaults;
    if !snapshot.provider.is_null() {
        let saved: Preferences = serde_json::from_value(snapshot.provider)?;
        saved.validate()?;
        settings.source = saved.source;
        settings.models.extend(saved.models);
    }
    let effective_source = if matches!(settings.source, Source::Gemini) && gemini_available { "gemini" } else { "harness" };
    Ok(json!({"revision": snapshot.revision, "settings": settings,
        "gemini_available": gemini_available, "effective_source": effective_source}))
}

/// Read or atomically save global commit preferences without acquiring a conversation lease.
pub async fn dispatch(params: Value) -> Result<Value> {
    let request: SettingsRequest = serde_json::from_value(params)?;
    let available = std::env::var_os("GEMINI_API_KEY").is_some_and(|value| !value.is_empty());
    tokio::task::spawn_blocking(move || apply(request, available)).await?
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request(root: &std::path::Path) -> SettingsRequest {
        serde_json::from_value(json!({"data_root":root,"defaults":{"source":"harness","models":{
            "codex":{"model":"gpt-6-luna","thinking":"low"},
            "gemini":{"model":"gemini-3.5-flash-lite","thinking":"minimal"}
        }}})).unwrap()
    }

    #[test]
    fn preferences_survive_reopen_and_reject_stale_or_unavailable_source() -> Result<()> {
        let root = tempfile::tempdir()?;
        let initial = apply(request(root.path()), true)?;
        let mut change = request(root.path());
        let mut settings: Preferences = serde_json::from_value(initial["settings"].clone())?;
        settings.source = Source::Gemini;
        settings.models.get_mut("codex").unwrap().model = "custom-model".into();
        change.settings = Some(settings.clone());
        change.expected_revision = Some(0);
        assert!(apply(change, false).is_err());
        assert_eq!(apply(request(root.path()), true)?["revision"], 0);
        let mut change = request(root.path());
        change.settings = Some(settings.clone());
        change.expected_revision = Some(0);
        assert_eq!(apply(change, true)?["revision"], 1);
        let reopened = apply(request(root.path()), false)?;
        assert_eq!(reopened["effective_source"], "harness");
        assert_eq!(reopened["settings"]["models"]["codex"]["model"], "custom-model");
        let mut stale = request(root.path());
        settings.source = Source::Harness;
        stale.settings = Some(settings);
        stale.expected_revision = Some(0);
        assert!(apply(stale, true).is_err());
        assert_eq!(apply(request(root.path()), true)?["effective_source"], "gemini");
        Ok(())
    }
}
