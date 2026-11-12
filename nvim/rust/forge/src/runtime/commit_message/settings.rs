use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{Context, Result, ensure};
use forge_harness::{provider::ServiceTier, session::AutoCompaction, settings::SettingsStore};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

/// Select independently saved model settings for persistent or isolated generation.
#[derive(Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Purpose {
    #[default]
    Background,
    Commit,
}

impl Purpose {
    fn key(self) -> &'static str {
        match self {
            Self::Background => "background",
            Self::Commit => "commit",
        }
    }
}

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

/// Keep Commit preferences editable for every registered provider; Background needs a durable runner.
fn validate_provider(provider: &str, purpose: Purpose) -> Result<()> {
    let kind = forge_providers::registry::select(provider)?;
    let integration = forge_providers::registry::resolve(&kind)?
        .capability
        .integration;
    ensure!(
        match purpose {
            Purpose::Background => integration.background_generation,
            Purpose::Commit => true,
        },
        "{provider} does not support {}",
        match purpose {
            Purpose::Background => "Background sessions",
            Purpose::Commit => "isolated generation",
        }
    );
    Ok(())
}

impl Preferences {
    fn validate(&self, purpose: Purpose) -> Result<()> {
        self.auto_compaction.validate(None)?;
        if let Some(provider) = &self.provider {
            validate_provider(provider, purpose)?;
        }
        for (provider, choice) in &self.models {
            ensure!(
                !provider.trim().is_empty() && !choice.model.trim().is_empty(),
                "Background model cannot be empty"
            );
            validate_provider(provider, purpose)?;
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
    #[serde(default)]
    purpose: Purpose,
    data_root: PathBuf,
    defaults: Preferences,
    expected_revision: Option<u64>,
    settings: Option<Preferences>,
}

fn apply(request: SettingsRequest) -> Result<Value> {
    request.defaults.validate(request.purpose)?;
    let mut store = SettingsStore::open(&request.data_root)?;
    let mut snapshot = store.get("", "", request.purpose.key())?;
    if let Some(settings) = request.settings {
        settings.validate(request.purpose)?;
        store.save_provider(
            request
                .expected_revision
                .context("Background settings require an expected revision")?,
            request.purpose.key(),
            &serde_json::to_value(settings)?,
        )?;
        snapshot = store.get("", "", request.purpose.key())?;
    }
    if snapshot.provider.is_null() && matches!(request.purpose, Purpose::Commit) {
        snapshot.provider = store.get("", "", "background")?.provider;
    }
    let mut settings = request.defaults;
    if !snapshot.provider.is_null() {
        let saved: Preferences = serde_json::from_value(snapshot.provider)?;
        saved.validate(request.purpose)?;
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

    /// Commit selections seed from earlier preferences and then save independently.
    #[test]
    fn commit_preferences_are_independent() {
        let directory = tempfile::tempdir().unwrap();
        let original = apply(request(
            directory.path(),
            defaults(),
            Some(defaults()),
            Some(0),
        ))
        .unwrap();
        let mut commit = request(directory.path(), defaults(), None, None);
        commit.purpose = Purpose::Commit;
        let seeded = apply(commit).unwrap();
        assert_eq!(seeded["settings"]["models"], original["settings"]["models"]);
        let mut selected = seeded["settings"].clone();
        selected["models"]["codex"]["model"] = json!("another-model");
        let mut commit = request(
            directory.path(),
            defaults(),
            Some(selected),
            seeded["revision"].as_u64(),
        );
        commit.purpose = Purpose::Commit;
        apply(commit).unwrap();
        let background = apply(request(directory.path(), defaults(), None, None)).unwrap();
        assert_eq!(
            background["settings"]["models"],
            original["settings"]["models"]
        );
        let mut commit = request(directory.path(), defaults(), None, None);
        commit.purpose = Purpose::Commit;
        assert_eq!(
            apply(commit).unwrap()["settings"]["models"]["codex"]["model"],
            "another-model"
        );
    }

    /// Mixed-provider defaults cannot disable the picker, and every registered choice persists.
    #[test]
    fn commit_preferences_accept_every_provider() {
        let directory = tempfile::tempdir().unwrap();
        let mut baseline = defaults();
        baseline["models"]["claude"] = json!({"model": "haiku"});
        baseline["models"]["antigravity"] = json!({"model": "default"});
        let mut read = request(directory.path(), baseline.clone(), None, None);
        read.purpose = Purpose::Commit;
        let mut snapshot = apply(read).unwrap();
        for provider in ["claude", "antigravity", "codex", "copilot"] {
            let mut selected = snapshot["settings"].clone();
            selected["provider"] = json!(provider);
            let mut save = request(
                directory.path(),
                baseline.clone(),
                Some(selected),
                snapshot["revision"].as_u64(),
            );
            save.purpose = Purpose::Commit;
            apply(save).unwrap();
            let mut read = request(directory.path(), baseline.clone(), None, None);
            read.purpose = Purpose::Commit;
            snapshot = apply(read).unwrap();
            assert_eq!(snapshot["settings"]["provider"], provider);
        }
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
