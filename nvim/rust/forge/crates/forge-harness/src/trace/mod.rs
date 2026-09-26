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
                | "exchange_id"
                | "thread_id"
                | "turn_id"
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
    pub configured: bool,
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
            configured: self.config_path.exists(),
        }
    }

    /// Resolve the detailed log owned by a Harness session.
    pub fn session_status(&self, session_id: &str) -> TraceStatus {
        let mut status = self.status();
        status.path = self.session_path(session_id).to_string_lossy().into_owned();
        status
    }

    fn session_path(&self, session_id: &str) -> PathBuf {
        let name = if !session_id.is_empty() && session_id.bytes().all(|byte| byte.is_ascii_alphanumeric() || byte == b'-') {
            session_id.to_owned()
        } else { format!("_{}", hex::encode(session_id.as_bytes())) };
        self.path.parent().unwrap().join("logs").join(format!("{name}.jsonl"))
    }

    /// Apply the editor default only before an explicit preference has been saved.
    pub fn configure_default(&self, enabled: bool) -> Result<TraceStatus> {
        if self.config_path.exists() { return Ok(self.status()); }
        self.configure(enabled)
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
        if let Err(error) = self.record_detail(session_id, event, payload) {
            eprintln!("Forge Harness detailed trace write failed: {error}");
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

    fn record_detail(&self, session_id: &str, event: &str, payload: &Value) -> Result<()> {
        let path = self.session_path(session_id);
        fs::create_dir_all(path.parent().unwrap())?;
        let lock = OpenOptions::new().create(true).read(true).write(true)
            .open(path.with_extension("lock"))?;
        lock.lock()?;
        let mut record = json!({
            "timestamp_ms": SystemTime::now().duration_since(UNIX_EPOCH).unwrap_or_default().as_millis() as i64,
            "session_id": session_id, "event": event, "payload": redact(payload),
        });
        let mut line = serde_json::to_vec(&record)?;
        if line.len() as u64 > MAX_TRACE_BYTES {
            record["payload"] = json!({ "omitted": "record exceeds 15 MiB", "bytes": line.len() });
            line = serde_json::to_vec(&record)?;
        }
        if path.metadata().is_ok_and(|metadata| metadata.len() + line.len() as u64 + 1 > MAX_TRACE_BYTES) {
            for generation in (1..=3).rev() {
                let source = if generation == 1 { path.clone() } else { path.with_extension(format!("jsonl.{}", generation - 1)) };
                let destination = path.with_extension(format!("jsonl.{generation}"));
                if source.exists() { fs::copy(source, destination)?; }
            }
            File::create(&path)?;
        }
        let mut file = OpenOptions::new().create(true).append(true).open(path)?;
        file.write_all(&line)?;
        file.write_all(b"\n")?;
        lock.unlock()?;
        Ok(())
    }
}

fn redact(value: &Value) -> Value {
    match value {
        Value::Object(object) => Value::Object(object.iter().map(|(key, value)| {
            let secret = matches!(key.to_ascii_lowercase().as_str(),
                "authorization" | "api_key" | "apikey" | "access_token" | "refresh_token" | "password" | "secret");
            (key.clone(), if secret { Value::String("[redacted]".into()) } else { redact(value) })
        }).collect()),
        Value::Array(array) => Value::Array(array.iter().map(redact).collect()),
        _ => value.clone(),
    }
}

#[cfg(test)]
mod test {
    use super::TraceStore;
    use serde_json::json;
    use tempfile::tempdir;

    #[test]
    fn detailed_logs_preserve_bodies_isolate_sessions_and_redact_credentials() {
        let directory = tempdir().unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        trace.configure(true).unwrap();
        trace.record("session-a", "model.sent", json!({"params":{"prompt":"full prompt","authorization":"secret"}}));
        trace.record("session-b", "model.received", json!({"text":"other conversation"}));
        let contents = std::fs::read_to_string(trace.session_status("session-a").path).unwrap();
        assert!(contents.contains("full prompt"));
        assert!(contents.contains("[redacted]"));
        assert!(!contents.contains("secret"));
        assert!(!contents.contains("other conversation"));
        trace.configure(false).unwrap();
        trace.record("session-a", "model.sent", json!({"text":"disabled"}));
        assert_eq!(contents, std::fs::read_to_string(trace.session_status("session-a").path).unwrap());
        drop(trace);
        let reopened = TraceStore::open(directory.path()).unwrap();
        assert!(!reopened.configure_default(true).unwrap().enabled);
    }

    #[test]
    fn detailed_log_rotates_without_silently_erasing_the_previous_segment() {
        let directory = tempdir().unwrap();
        let trace = TraceStore::open(directory.path()).unwrap();
        trace.configure(true).unwrap();
        trace.record("session", "first", json!({}));
        let path = trace.session_path("session");
        std::fs::OpenOptions::new().write(true).open(&path).unwrap().set_len(super::MAX_TRACE_BYTES).unwrap();
        trace.record("session", "second", json!({}));
        assert!(std::fs::read_to_string(&path).unwrap().contains("second"));
        assert!(path.with_extension("jsonl.1").exists());
        assert!(path.metadata().unwrap().len() < 1024);
    }

    #[test]
    fn rejection_metadata_preserves_execution_identity_without_message_content() {
        let metadata = super::metadata_payload(&json!({
            "exchange_id":"exchange", "thread_id":"thread", "turn_id":"turn",
            "event_type":"assistant_message", "code":"execution_unknown_or_settled",
            "text":"private response"
        }));
        assert_eq!(metadata["exchange_id"], "exchange");
        assert_eq!(metadata["thread_id"], "thread");
        assert_eq!(metadata["turn_id"], "turn");
        assert_eq!(metadata["code"], "execution_unknown_or_settled");
        assert!(metadata.get("text").is_none());
    }

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
