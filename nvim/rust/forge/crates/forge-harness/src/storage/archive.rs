use anyhow::{Context, Result};
use rusqlite::{Connection, OptionalExtension, params};
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
                for turn in object
                    .get_mut("turn")
                    .and_then(Value::as_array_mut)
                    .into_iter()
                    .flatten()
                {
                    let turn_id = turn["id"]
                        .as_str()
                        .context("inherited turn has no identity")?
                        .to_owned();
                    for (tool_id, tool) in
                        turn["tool"]["item"].as_object_mut().into_iter().flatten()
                    {
                        if let Some(output) = tool.get_mut("output") {
                            let text = output.as_str().context("inherited output is not text")?;
                            if !text.is_empty() {
                                archived[format!("{turn_id}:{tool_id}")] =
                                    Value::String(objects.put(text.as_bytes())?);
                                *output = Value::String(String::new());
                            }
                        }
                    }
                }
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

/// Converts legacy inherited output once, within the owning schema migration.
pub(super) fn migrate(connection: &Connection, objects: &ObjectStore) -> Result<()> {
    let mut cursor = (String::new(), -1_i64);
    loop {
        let next: Option<(String, i64, String)> = connection.query_row(
            "SELECT session_id,ordinal,payload FROM fork_history_record WHERE (session_id,ordinal)>(?1,?2) ORDER BY session_id,ordinal LIMIT 1",
            params![cursor.0,cursor.1], |row| Ok((row.get(0)?,row.get(1)?,row.get(2)?)),
        ).optional()?;
        let Some((session, ordinal, payload)) = next else {
            break;
        };
        let mut value: Value = crate::codec::from_str(&payload)?;
        encode(&mut value, objects)?;
        connection.execute(
            "UPDATE fork_history_record SET payload=?3 WHERE session_id=?1 AND ordinal=?2",
            params![session, ordinal, super::encode(&value)?],
        )?;
        cursor = (session, ordinal);
    }
    Ok(())
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
