use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::fs::{self, File, OpenOptions};
use std::io::{Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

const MAX_TRACE_BYTES: u64 = 15 * 1024 * 1024;

fn bounded_text(text: &str) -> &str {
    let mut end = text.len().min(256);
    while !text.is_char_boundary(end) {
        end -= 1;
    }
    &text[..end]
}

fn metadata_payload(payload: &Value) -> Value {
    let Some(object) = payload.as_object() else {
        return Value::Null;
    };
    let mut metadata = serde_json::Map::new();
    for (key, value) in object.iter().take(32) {
        if !matches!(
            key.as_str(),
            "method"
                | "operation"
                | "operation_id"
                | "request_id"
                | "session_id"
                | "provider"
                | "event_type"
                | "status"
                | "code"
                | "enabled"
                | "cached"
                | "cancelled"
                | "generation"
                | "revision"
                | "phase"
                | "elapsed_ms"
                | "duration_ms"
                | "count"
                | "rows"
                | "bytes"
                | "source_bytes"
                | "result_bytes"
                | "queue_bytes"
                | "queue_count"
                | "active_jobs"
                | "retained_bytes"
        ) {
            continue;
        }
        let value = match value {
            Value::String(text) => Value::String(bounded_text(text).to_owned()),
            Value::Bool(_) | Value::Number(_) | Value::Null => value.clone(),
            _ => continue,
        };
        metadata.insert(key.clone(), value);
    }
    Value::Object(metadata)
}

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

/// Retains bounded Harness diagnostic metadata while tracing remains enabled.
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
                    .read(true)
                    .write(true)
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
                    .read(true)
                    .write(true)
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
        let cleared = OpenOptions::new()
            .create(true)
            .read(true)
            .write(true)
            .open(&self.path)
            .with_context(|| format!("clear Harness trace {}", self.path.display()))?;
        cleared.lock()?;
        let result = cleared.set_len(0);
        let unlocked = cleared.unlock();
        result.and(unlocked)?;
        if enabled {
            *file = Some(cleared);
        }
        drop(file);
        self.record("global", "trace.cleared", Value::Null);
        Ok(self.status())
    }

    /// Append at most 32 scalar metadata fields with a bounded session identity.
    pub fn record(&self, session_id: &str, event: &str, payload: Value) {
        self.record_ref(session_id, event, &payload);
    }

    /// Reads allowlisted metadata without cloning a provider message or its content.
    pub fn record_ref(&self, session_id: &str, event: &str, payload: &Value) {
        if !self.status().enabled {
            return;
        }
        let record = json!({
            "timestamp_ms": SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_millis() as i64,
            "session_id": bounded_text(session_id),
            "event": bounded_text(event),
            "payload": metadata_payload(payload),
        });
        let Ok(line) = serde_json::to_string(&record) else {
            return;
        };
        if line.len() > 16 * 1024 {
            eprintln!("Forge Harness trace record exceeded its 16 KiB encoding limit");
            return;
        }
        let Ok(mut file) = self.file.lock() else {
            return;
        };
        let Some(file) = file.as_mut() else {
            return;
        };
        let written = (|| -> std::io::Result<()> {
            file.lock()?;
            let appended = (|| -> std::io::Result<()> {
                if file.metadata()?.len() + line.len() as u64 + 1 > MAX_TRACE_BYTES {
                    file.set_len(0)?;
                }
                file.seek(SeekFrom::End(0))?;
                writeln!(file, "{line}")
            })();
            let unlocked = file.unlock();
            appended.and(unlocked)
        })();
        if let Err(error) = written {
            eprintln!("Forge Harness trace write failed: {error}");
        }
    }
}

#[cfg(test)]
mod test {
    use super::TraceStore;
    use serde_json::json;
    use tempfile::tempdir;

    #[test]
    fn bounds_metadata_and_truncates_before_crossing_file_limit() {
        let directory = tempdir().unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        trace.configure(true).unwrap();
        let path = directory.path().join("harness-trace.jsonl");
        std::fs::OpenOptions::new()
            .write(true)
            .open(&path)
            .unwrap()
            .set_len(super::MAX_TRACE_BYTES)
            .unwrap();
        trace.record(
            "session",
            "bounded",
            json!({ "method": "x".repeat(1_000_000), "body": "private body", "prompt": "private prompt", "authorization": "private credential", "nested": { "body": "ignored" } }),
        );
        let contents = std::fs::read_to_string(&path).unwrap();
        assert!(contents.len() < 1024);
        let record: serde_json::Value = serde_json::from_str(contents.trim()).unwrap();
        assert_eq!(record["payload"]["method"].as_str().unwrap().len(), 256);
        assert!(record["payload"].get("body").is_none());
        assert!(record["payload"].get("prompt").is_none());
        assert!(record["payload"].get("authorization").is_none());
        assert!(record["payload"].get("nested").is_none());
    }

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
        assert!(!contents.contains("retained"));
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

    #[test]
    fn independent_handles_append_after_another_owner_clears() {
        let directory = tempdir().unwrap();
        let first = TraceStore::open(directory.path()).unwrap();
        first.configure(true).unwrap();
        let second = TraceStore::open(directory.path()).unwrap();
        first.record("first", "before", json!({}));
        second.clear().unwrap();
        first.record("first", "after_first", json!({}));
        second.record("second", "after_second", json!({}));
        let contents =
            std::fs::read_to_string(directory.path().join("harness-trace.jsonl")).unwrap();
        let event = contents
            .lines()
            .map(|line| {
                serde_json::from_str::<serde_json::Value>(line).unwrap()["event"]
                    .as_str()
                    .unwrap()
                    .to_owned()
            })
            .collect::<Vec<_>>();
        assert_eq!(event, vec!["trace.cleared", "after_first", "after_second"]);
    }
}
