use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug, Serialize)]
pub struct TraceStatus {
    pub enabled: bool,
    pub path: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct TraceConfig {
    #[serde(default)]
    enabled: bool,
}

/// Appends unredacted Harness diagnostic records while tracing remains enabled.
pub struct TraceStore {
    path: PathBuf,
    config_path: PathBuf,
    config: Mutex<TraceConfig>,
    file: Mutex<Option<File>>,
}

impl TraceStore {
    /// Open the persistent trace configuration and append-only event file for one Harness data root.
    pub fn open(data_root: &Path) -> Result<Self> {
        let config_path = data_root.join("harness-trace-config.json");
        let config = fs::read_to_string(&config_path)
            .ok()
            .and_then(|contents| serde_json::from_str(&contents).ok())
            .unwrap_or(TraceConfig { enabled: false });
        let path = data_root.join("harness-trace.jsonl");
        let file = config
            .enabled
            .then(|| {
                OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&path)
                    .with_context(|| format!("open Harness trace {}", path.display()))
            })
            .transpose()?;
        Ok(Self {
            path,
            config_path,
            config: Mutex::new(config),
            file: Mutex::new(file),
        })
    }

    pub fn status(&self) -> TraceStatus {
        let enabled = self
            .config
            .lock()
            .map(|config| config.enabled)
            .unwrap_or(false);
        TraceStatus {
            enabled,
            path: self.path.to_string_lossy().into_owned(),
        }
    }

    pub fn configure(&self, enabled: bool) -> Result<TraceStatus> {
        let mut config = self
            .config
            .lock()
            .map_err(|_| anyhow::anyhow!("Harness trace configuration lock poisoned"))?;
        config.enabled = enabled;
        fs::write(&self.config_path, serde_json::to_vec_pretty(&*config)?).with_context(|| {
            format!(
                "write Harness trace configuration {}",
                self.config_path.display()
            )
        })?;
        let mut file = self
            .file
            .lock()
            .map_err(|_| anyhow::anyhow!("Harness trace file lock poisoned"))?;
        *file = if enabled {
            Some(
                OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&self.path)
                    .with_context(|| format!("open Harness trace {}", self.path.display()))?,
            )
        } else {
            None
        };
        drop(file);
        drop(config);
        self.record("global", "trace.configured", json!({ "enabled": enabled }));
        Ok(self.status())
    }

    pub fn toggle(&self) -> Result<TraceStatus> {
        self.configure(!self.status().enabled)
    }

    pub fn clear(&self) -> Result<TraceStatus> {
        let enabled = self.status().enabled;
        let mut file = self
            .file
            .lock()
            .map_err(|_| anyhow::anyhow!("Harness trace file lock poisoned"))?;
        *file = None;
        File::create(&self.path)
            .with_context(|| format!("clear Harness trace {}", self.path.display()))?;
        if enabled {
            *file = Some(OpenOptions::new().append(true).open(&self.path)?);
        }
        drop(file);
        self.record("global", "trace.cleared", Value::Null);
        Ok(self.status())
    }

    /// Append one raw boundary payload with its durable Harness session identity.
    pub fn record(&self, session_id: &str, event: &str, payload: Value) {
        if !self.status().enabled {
            return;
        }
        let record = json!({
            "timestamp_ms": SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_millis() as i64,
            "session_id": session_id,
            "event": event,
            "payload": payload,
        });
        let Ok(line) = serde_json::to_string(&record) else {
            return;
        };
        let Ok(mut file) = self.file.lock() else {
            return;
        };
        let Some(file) = file.as_mut() else {
            return;
        };
        let _ = writeln!(file, "{line}");
    }
}

#[cfg(test)]
mod test {
    use super::TraceStore;
    use serde_json::json;
    use tempfile::tempdir;

    #[test]
    fn appends_session_identified_records_only_when_enabled() {
        let directory = tempdir().unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        trace.record("session-a", "ignored", json!({ "value": 0 }));
        assert!(!directory.path().join("harness-trace.jsonl").exists());

        trace.configure(true).unwrap();
        trace.record("session-a", "recorded", json!({ "secret": "retained" }));
        let contents =
            std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
        assert!(contents.contains("session-a"));
        assert!(contents.contains("retained"));
    }

    #[test]
    fn persists_enablement_and_clears_the_append_only_file() {
        let directory = tempdir().unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        trace.configure(true).unwrap();
        trace.record("session-a", "first", json!({}));
        drop(trace);

        let trace = TraceStore::open(directory.path()).unwrap();
        assert!(trace.status().enabled);
        trace.clear().unwrap();
        let contents =
            std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
        assert!(contents.contains("trace.cleared"));
        assert!(!contents.contains("\"event\":\"first\""));
    }
}
