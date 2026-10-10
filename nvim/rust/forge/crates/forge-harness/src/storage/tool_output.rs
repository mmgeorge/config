use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, TransactionBehavior, params};
use serde_json::Value;

use super::SqliteStore;
use crate::exchange::Exchange;

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
