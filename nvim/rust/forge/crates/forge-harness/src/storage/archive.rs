use anyhow::{Context, Result};
use serde_json::Value;

use super::objects::ObjectStore;
use crate::timeline::TimelineEntry;

/// Moves inherited output into immutable objects so deleting its original session is safe.
pub(super) fn encode(value: &mut Value, objects: &ObjectStore) -> Result<()> {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        if let Some(object) = value.as_object_mut() {
            if object.contains_key("session_id") && object.contains_key("turn") {
                let mut archived = object
                    .remove("archived_output")
                    .unwrap_or_else(|| serde_json::json!({}));
                super::tool_output::visit_tool_records(object, |turn_id, tool_id, tool| {
                    if let Some(output) = tool.get_mut("output") {
                        let text = output.as_str().context("inherited output is not text")?;
                        if !text.is_empty() {
                            archived[format!("{turn_id}:{tool_id}")] =
                                Value::String(objects.put(text.as_bytes())?);
                            *output = Value::String(String::new());
                        }
                    }
                    Ok(())
                })?;
                if archived
                    .as_object()
                    .is_some_and(|entries| !entries.is_empty())
                {
                    object.insert("archived_output".into(), archived);
                }
            }
            for child in object.values_mut() {
                encode(child, objects)?;
            }
        } else if let Some(array) = value.as_array_mut() {
            for child in array {
                encode(child, objects)?;
            }
        }
        Ok(())
    })
}

/// Attaches runtime storage access without serializing machine-specific paths into history.
pub(super) fn attach(entry: &mut TimelineEntry, root: &std::path::Path) {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || match entry {
        TimelineEntry::Inherited { entry, .. } => attach(entry, root),
        TimelineEntry::Exchange {
            exchange,
            agent_by_id,
            ..
        } => {
            exchange.output_store = Some(root.to_owned());
            for child in agent_by_id.values_mut() {
                attach(child, root);
            }
        }
        TimelineEntry::AgentLifecycle {
            exchange, agent, ..
        } => {
            for exchange in exchange {
                exchange.output_store = Some(root.to_owned());
            }
            for child in agent {
                attach(child, root);
            }
        }
        _ => {}
    });
}
