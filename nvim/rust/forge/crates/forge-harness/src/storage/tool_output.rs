use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, TransactionBehavior, params};
use serde_json::Value;

use super::SqliteStore;
use crate::exchange::Exchange;

/// Holds one character-bounded page from the currently recorded output.
pub(crate) struct ToolOutputPage {
    /// Recorded text, without ANSI conversion or repeated tool execution.
    pub(crate) output: String,
    /// Next character offset when more recorded text remains.
    pub(crate) next_offset: Option<usize>,
    /// Number of currently recorded Unicode scalar values.
    pub(crate) total_chars: usize,
}

pub(super) fn initialize(connection: &mut Connection) -> Result<()> {
    connection.execute_batch(
        "CREATE TABLE IF NOT EXISTS tool_output_owner (
        exchange_id TEXT NOT NULL REFERENCES exchange_record(id) ON DELETE CASCADE,
        turn_id TEXT NOT NULL, tool_id TEXT NOT NULL,
        PRIMARY KEY(exchange_id,turn_id,tool_id)
    );
        CREATE TABLE IF NOT EXISTS tool_output_chunk (
        exchange_id TEXT NOT NULL REFERENCES exchange_record(id) ON DELETE CASCADE,
        turn_id TEXT NOT NULL, tool_id TEXT NOT NULL, byte_offset INTEGER NOT NULL,
        output TEXT NOT NULL, PRIMARY KEY(exchange_id,turn_id,tool_id,byte_offset)
    );",
    )?;
    let transaction = connection.transaction_with_behavior(TransactionBehavior::Immediate)?;
    let records = {
        let mut statement = transaction.prepare("SELECT id,payload FROM exchange_record WHERE coalesce(json_extract(payload,'$.tool_output_format'),0)<>2")?;
        statement
            .query_map([], |row| {
                Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
            })?
            .collect::<rusqlite::Result<Vec<_>>>()?
    };
    for (id, payload) in records {
        let mut value: Value = serde_json::from_str(&payload)?;
        visit_output(&mut value, |turn, tool, output| {
            transaction.execute("INSERT OR IGNORE INTO tool_output_owner VALUES(?1,?2,?3)",params![id,turn,tool])?;
            // Recovery can rewrite metadata with empty inline output while retaining chunks.
            if !output.is_empty() {
                replace(&transaction, &id, turn, tool, output)?;
            }
            Ok(())
        })?;
        value["tool_output_format"] = Value::from(2);
        transaction.execute(
            "UPDATE exchange_record SET payload=?2 WHERE id=?1",
            params![id, serde_json::to_string(&value)?],
        )?;
    }
    transaction.commit()?;
    Ok(())
}

fn visit_output(
    value: &mut Value,
    mut visit: impl FnMut(&str, &str, &str) -> Result<()>,
) -> Result<()> {
    for turn in value["turn"].as_array_mut().into_iter().flatten() {
        let turn_id = turn["id"]
            .as_str()
            .context("stored turn has no identity")?
            .to_owned();
        for (tool_id, tool) in turn["tool"]["item"].as_object_mut().into_iter().flatten() {
            let output = tool["output"]
                .as_str()
                .context("stored tool output is not text")?;
            visit(&turn_id, tool_id, output)?;
            tool["output"] = Value::String(String::new());
        }
    }
    Ok(())
}

fn replace(
    connection: &Connection,
    exchange: &str,
    turn: &str,
    tool: &str,
    output: &str,
) -> Result<()> {
    connection.execute(
        "DELETE FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
        params![exchange, turn, tool],
    )?;
    if !output.is_empty() {
        connection.execute(
            "INSERT INTO tool_output_chunk VALUES(?1,?2,?3,0,?4)",
            params![exchange, turn, tool, output],
        )?;
    }
    Ok(())
}

pub(super) fn encode(exchange: &Exchange) -> Result<String> {
    let mut value = serde_json::to_value(exchange)?;
    visit_output(&mut value, |_, _, _| Ok(()))?;
    value["tool_output_format"] = Value::from(2);
    Ok(serde_json::to_string(&value)?)
}

pub(super) fn synchronize(connection: &Connection, exchange: &Exchange) -> Result<()> {
    connection.execute("DELETE FROM tool_output_owner WHERE exchange_id=?1",[&exchange.id])?;
    for turn in &exchange.turn {
        for tool in turn.tools() {
            connection.execute("INSERT INTO tool_output_owner VALUES(?1,?2,?3)",params![exchange.id,turn.id(),tool.id])?;
            let previous = read(connection, &exchange.id, turn.id(), &tool.id)?;
            if previous != tool.output {
                replace(connection, &exchange.id, turn.id(), &tool.id, &tool.output)?;
            }
        }
    }
    connection.execute("DELETE FROM tool_output_chunk WHERE exchange_id=?1 AND NOT EXISTS(SELECT 1 FROM tool_output_owner AS owner WHERE owner.exchange_id=tool_output_chunk.exchange_id AND owner.turn_id=tool_output_chunk.turn_id AND owner.tool_id=tool_output_chunk.tool_id)",[&exchange.id])?;
    Ok(())
}

fn read(connection: &Connection, exchange: &str, turn: &str, tool: &str) -> Result<String> {
    let mut statement = connection.prepare("SELECT byte_offset,output FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset")?;
    let mut output = String::new();
    for row in statement.query_map(params![exchange, turn, tool], |row| {
        Ok((row.get::<_, i64>(0)?, row.get::<_, String>(1)?))
    })? {
        let (offset, chunk) = row?;
        ensure!(
            offset >= 0 && offset as usize == output.len(),
            "stored tool output has a discontinuous byte offset"
        );
        output.push_str(&chunk);
    }
    Ok(output)
}

impl SqliteStore {
    /// Reads timeline metadata without hydrating any stored tool output.
    pub(crate) fn load_exchange_metadata(&self, id: &str) -> Result<Option<Exchange>> {
        Ok(self.list_payload("SELECT payload FROM exchange_record WHERE id=?1", [id])?.pop())
    }

    /// Reads at most 32000 characters while preserving UTF-8 across stored chunks.
    pub(crate) fn tool_output_page(&self, exchange: &str, turn: &str, tool: &str,
        offset: usize, limit: usize) -> Result<ToolOutputPage> {
        ensure!((1..=32000).contains(&limit), "output limit must be from 1 to 32000 characters");
        let mut statement = self.connection.prepare(
            "SELECT byte_offset,length(cast(output AS BLOB)) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset")?;
        let mut rows = statement.query(params![exchange,turn,tool])?;
        let mut output = String::new();
        let mut total_chars = 0usize;
        let mut total_bytes = 0i64;
        let page_end = offset.checked_add(limit).context("output offset exceeds supported range")?;
        while let Some(row) = rows.next()? {
            let byte_offset: i64 = row.get(0)?;
            let bytes: i64 = row.get(1)?;
            ensure!(byte_offset == total_bytes, "stored tool output has a discontinuous byte offset");
            total_bytes = total_bytes.checked_add(bytes).context("tool output byte count overflow")?;
            let mut consumed = 0i64;
            while consumed < bytes {
                let window: Vec<u8> = self.connection.query_row(
                    "SELECT substr(cast(output AS BLOB),?5,65539) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND byte_offset=?4",
                    params![exchange,turn,tool,byte_offset,consumed + 1], |row|row.get(0))?;
                let valid = match std::str::from_utf8(&window) {
                    Ok(text) => text.len(),
                    Err(error) if error.error_len().is_none() => error.valid_up_to(),
                    Err(error) => return Err(error).context("stored tool output is not UTF-8"),
                };
                ensure!(valid > 0, "stored tool output has an incomplete UTF-8 character");
                for character in std::str::from_utf8(&window[..valid])?.chars() {
                    if (offset..page_end).contains(&total_chars) { output.push(character); }
                    total_chars = total_chars.checked_add(1).context("tool output character count overflow")?;
                }
                consumed += valid as i64;
            }
        }
        ensure!(offset <= total_chars, "output offset exceeds currently recorded text");
        let next = page_end.min(total_chars);
        Ok(ToolOutputPage {output,next_offset:(next < total_chars).then_some(next),total_chars})
    }

    pub(crate) fn save_tool_output_delta(
        &mut self,
        exchange: &Exchange,
        event: &crate::backend::BackendEvent,
    ) -> Result<()> {
        let address = event
            .address
            .as_ref()
            .context("tool output has no provider owner")?;
        let activity = event.activity.as_ref().context("tool output has no call")?;
        let delta = activity
            .output
            .as_deref()
            .context("tool output has no delta")?;
        let turn = exchange
            .turn
            .iter()
            .find(|turn| turn.provider() == address)
            .context("tool output turn is missing")?;
        let tool = turn
            .tools()
            .find(|tool| tool.id == activity.id)
            .context("tool output call is missing")?;
        let admitted: bool = self.connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM tool_output_owner WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3)",
            params![exchange.id,turn.id(),tool.id], |row| row.get(0))?;
        if !admitted {
            return self.save_exchange(exchange);
        }
        let offset = tool
            .output
            .len()
            .checked_sub(delta.len())
            .context("tool delta exceeds output")?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let persisted: i64 = transaction.query_row("SELECT coalesce((SELECT byte_offset+length(cast(output AS BLOB)) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset DESC LIMIT 1),0)", params![exchange.id,turn.id(),tool.id], |row| row.get(0))?;
        ensure!(
            persisted >= 0 && persisted as usize == offset,
            "tool output append does not follow the committed byte offset"
        );
        if !delta.is_empty() {
            transaction.execute(
                "INSERT INTO tool_output_chunk VALUES(?1,?2,?3,?4,?5)",
                params![exchange.id, turn.id(), tool.id, offset as i64, delta],
            )?;
        }
        transaction.commit()?;
        Ok(())
    }

    pub(super) fn list_exchange_payload<P: rusqlite::Params>(
        &self,
        query: &str,
        params: P,
    ) -> Result<Vec<Exchange>> {
        let payloads: Vec<Value> = self.list_payload(query, params)?;
        payloads
            .into_iter()
            .map(|mut value| {
                let exchange_id = value["id"]
                    .as_str()
                    .context("stored exchange has no identity")?
                    .to_owned();
                for turn in value["turn"].as_array_mut().into_iter().flatten() {
                    let turn_id = turn["id"]
                        .as_str()
                        .context("stored turn has no identity")?
                        .to_owned();
                    for (tool_id, tool) in
                        turn["tool"]["item"].as_object_mut().into_iter().flatten()
                    {
                        tool["output"] =
                            Value::String(read(&self.connection, &exchange_id, &turn_id, tool_id)?);
                    }
                }
                Ok(serde_json::from_value(value)?)
            })
            .collect()
    }
}
