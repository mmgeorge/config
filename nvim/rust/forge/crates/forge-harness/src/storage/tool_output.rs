use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};
use serde_json::Value;
use sha2::{Digest, Sha256};

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

pub(super) fn initialize(connection: &Connection) -> Result<()> {
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
    initialize_positions(connection)?;
    let transaction = connection;
    let mut cursor: Option<i64> = None;
    loop {
        let record: Option<(i64, String, String)> = transaction.query_row(
            "SELECT rowid,id,payload FROM exchange_record WHERE (?1 IS NULL OR rowid>?1) AND coalesce(json_extract(payload,'$.tool_output_format'),0)<>2 ORDER BY rowid LIMIT 1",
            [cursor], |row| Ok((row.get(0)?,row.get(1)?,row.get(2)?)),
        ).optional()?;
        let Some((row, id, payload)) = record else {
            break;
        };
        let mut value: Value = serde_json::from_str(&payload)?;
        visit_output(&mut value, |turn, tool, output| {
            transaction.execute(
                "INSERT OR IGNORE INTO tool_output_owner VALUES(?1,?2,?3)",
                params![id, turn, tool],
            )?;
            // Recovery can rewrite metadata with empty inline output while retaining chunks.
            if !output.is_empty() {
                replace(transaction, &id, turn, tool, &output.into())?;
            }
            Ok(())
        })?;
        value["tool_output_format"] = Value::from(2);
        transaction.execute(
            "UPDATE exchange_record SET payload=?2 WHERE id=?1",
            params![id, serde_json::to_string(&value)?],
        )?;
        cursor = Some(row);
    }
    initialize_positions(connection)?;
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

pub(super) fn initialize_positions(connection: &Connection) -> Result<()> {
    connection.execute_batch("CREATE TABLE IF NOT EXISTS tool_output_summary (
        exchange_id TEXT NOT NULL,turn_id TEXT NOT NULL,tool_id TEXT NOT NULL,
        bytes INTEGER NOT NULL,characters INTEGER NOT NULL,digest TEXT NOT NULL,
        PRIMARY KEY(exchange_id,turn_id,tool_id),
        FOREIGN KEY(exchange_id,turn_id,tool_id) REFERENCES tool_output_owner(exchange_id,turn_id,tool_id) ON DELETE CASCADE);
        CREATE TABLE IF NOT EXISTS tool_output_position (
        exchange_id TEXT NOT NULL,turn_id TEXT NOT NULL,tool_id TEXT NOT NULL,
        byte_offset INTEGER NOT NULL,char_offset INTEGER NOT NULL,char_count INTEGER NOT NULL,
        PRIMARY KEY(exchange_id,turn_id,tool_id,byte_offset),
        FOREIGN KEY(exchange_id,turn_id,tool_id) REFERENCES tool_output_owner(exchange_id,turn_id,tool_id) ON DELETE CASCADE);
        CREATE INDEX IF NOT EXISTS tool_output_character_order ON tool_output_position(exchange_id,turn_id,tool_id,char_offset);")?;
    connection.execute_batch("CREATE TEMP TABLE tool_output_upgrade(byte_offset INTEGER PRIMARY KEY,char_offset INTEGER NOT NULL,char_count INTEGER NOT NULL,output TEXT NOT NULL)")?;
    let mut cursor = 0i64;
    loop {
        let owner: Option<(i64,String,String,String)> = connection.query_row("SELECT rowid,exchange_id,turn_id,tool_id FROM tool_output_owner WHERE rowid>?1 AND NOT EXISTS(SELECT 1 FROM tool_output_summary s WHERE s.exchange_id=tool_output_owner.exchange_id AND s.turn_id=tool_output_owner.turn_id AND s.tool_id=tool_output_owner.tool_id) ORDER BY rowid LIMIT 1",[cursor],|row|Ok((row.get(0)?,row.get(1)?,row.get(2)?,row.get(3)?))).optional()?;
        let Some((row, exchange, turn, tool)) = owner else {
            break;
        };
        migrate_output(connection, &exchange, &turn, &tool)?;
        cursor = row;
    }
    connection.execute_batch("DROP TABLE tool_output_upgrade")?;
    Ok(())
}

fn migrate_output(connection: &Connection, exchange: &str, turn: &str, tool: &str) -> Result<()> {
    // Stage validated windows before replacing legacy chunks in the migration transaction.
    connection.execute("DELETE FROM tool_output_upgrade", [])?;
    let mut hash = Sha256::new();
    let mut bytes = 0i64;
    let mut characters = 0i64;
    let mut statement = connection.prepare("SELECT byte_offset,length(cast(output AS BLOB)) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset")?;
    let mut rows = statement.query(params![exchange, turn, tool])?;
    while let Some(row) = rows.next()? {
        let start: i64 = row.get(0)?;
        let length: i64 = row.get(1)?;
        ensure!(
            start == bytes,
            "stored tool output has a discontinuous byte offset"
        );
        let mut consumed = 0i64;
        while consumed < length {
            let window:Vec<u8> = connection.query_row("SELECT substr(cast(output AS BLOB),?5,16384) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND byte_offset=?4",params![exchange,turn,tool,start,consumed+1],|row|row.get(0))?;
            let valid = match std::str::from_utf8(&window) {
                Ok(text) => text.len(),
                Err(error) if error.error_len().is_none() => error.valid_up_to(),
                Err(error) => return Err(error).context("stored tool output is not UTF-8"),
            };
            ensure!(
                valid > 0,
                "stored tool output ended inside a UTF-8 character"
            );
            let text = std::str::from_utf8(&window[..valid])?;
            let count = text.chars().count() as i64;
            connection.execute(
                "INSERT INTO tool_output_upgrade VALUES(?1,?2,?3,?4)",
                params![bytes, characters, count, text],
            )?;
            hash.update(text.as_bytes());
            bytes += valid as i64;
            characters += count;
            consumed += valid as i64;
        }
    }
    drop(rows);
    drop(statement);
    // Publish indexed chunks and their summary together with the schema version.
    connection.execute(
        "DELETE FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
        params![exchange, turn, tool],
    )?;
    connection.execute(
        "INSERT INTO tool_output_chunk SELECT ?1,?2,?3,byte_offset,output FROM tool_output_upgrade",
        params![exchange, turn, tool],
    )?;
    connection.execute("INSERT INTO tool_output_position SELECT ?1,?2,?3,byte_offset,char_offset,char_count FROM tool_output_upgrade",params![exchange,turn,tool])?;
    connection.execute(
        "INSERT INTO tool_output_summary VALUES(?1,?2,?3,?4,?5,?6)",
        params![
            exchange,
            turn,
            tool,
            bytes,
            characters,
            hex::encode(hash.finalize())
        ],
    )?;
    Ok(())
}

fn append_chunks(
    connection: &Connection,
    exchange: &str,
    turn: &str,
    tool: &str,
    text: &str,
    mut bytes: usize,
    mut characters: usize,
) -> Result<()> {
    let mut remaining = text;
    while !remaining.is_empty() {
        let mut end = remaining.len().min(16384);
        while !remaining.is_char_boundary(end) {
            end -= 1;
        }
        let chunk = &remaining[..end];
        let count = chunk.chars().count();
        connection.execute(
            "INSERT INTO tool_output_chunk VALUES(?1,?2,?3,?4,?5)",
            params![exchange, turn, tool, bytes as i64, chunk],
        )?;
        connection.execute(
            "INSERT INTO tool_output_position VALUES(?1,?2,?3,?4,?5,?6)",
            params![
                exchange,
                turn,
                tool,
                bytes as i64,
                characters as i64,
                count as i64
            ],
        )?;
        bytes += end;
        characters += count;
        remaining = &remaining[end..];
    }
    Ok(())
}

fn save_summary(
    connection: &Connection,
    exchange: &str,
    turn: &str,
    tool: &str,
    output: &crate::turn::ToolOutputText,
) -> Result<()> {
    connection.execute("INSERT INTO tool_output_summary VALUES(?1,?2,?3,?4,?5,?6) ON CONFLICT(exchange_id,turn_id,tool_id) DO UPDATE SET bytes=excluded.bytes,characters=excluded.characters,digest=excluded.digest",params![exchange,turn,tool,output.len() as i64,output.characters() as i64,output.digest()])?;
    Ok(())
}

fn replace(
    connection: &Connection,
    exchange: &str,
    turn: &str,
    tool: &str,
    output: &crate::turn::ToolOutputText,
) -> Result<()> {
    connection.execute(
        "DELETE FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
        params![exchange, turn, tool],
    )?;
    connection.execute(
        "DELETE FROM tool_output_position WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
        params![exchange, turn, tool],
    )?;
    append_chunks(connection, exchange, turn, tool, output, 0, 0)?;
    save_summary(connection, exchange, turn, tool, output)?;
    Ok(())
}

pub(super) fn encode(exchange: &Exchange) -> Result<String> {
    let mut metadata = exchange.clone();
    for turn in &mut metadata.turn {
        turn.omit_tool_output();
    }
    let mut value = serde_json::to_value(&metadata)?;
    value["tool_output_format"] = Value::from(2);
    Ok(serde_json::to_string(&value)?)
}

pub(super) fn synchronize(connection: &Connection, exchange: &Exchange) -> Result<()> {
    let mut statement = connection.prepare("SELECT o.turn_id,o.tool_id,s.digest FROM tool_output_owner o LEFT JOIN tool_output_summary s USING(exchange_id,turn_id,tool_id) WHERE o.exchange_id=?1")?;
    let mut previous = statement
        .query_map([&exchange.id], |row| {
            Ok((
                (row.get::<_, String>(0)?, row.get::<_, String>(1)?),
                row.get::<_, Option<String>>(2)?,
            ))
        })?
        .collect::<rusqlite::Result<std::collections::HashMap<_, _>>>()?;
    for turn in &exchange.turn {
        for tool in turn.tools() {
            let stored = previous.remove(&(turn.id().to_owned(), tool.id.clone()));
            if stored.is_none() {
                connection.execute(
                    "INSERT INTO tool_output_owner VALUES(?1,?2,?3)",
                    params![exchange.id, turn.id(), tool.id],
                )?;
            }
            if stored.flatten().as_deref() != Some(tool.output.digest()) {
                replace(connection, &exchange.id, turn.id(), &tool.id, &tool.output)?;
            }
        }
    }
    for ((turn, tool), _) in previous {
        connection.execute(
            "DELETE FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
            params![exchange.id, turn, tool],
        )?;
        connection.execute(
            "DELETE FROM tool_output_owner WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",
            params![exchange.id, turn, tool],
        )?;
    }
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
    pub(crate) fn list_session_agent_exchange_metadata(
        &self,
        session: &str,
        agents: &[crate::agent::Agent],
    ) -> Result<Vec<Exchange>> {
        let mut exchange: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id IN (SELECT value FROM json_each(?2)) ORDER BY agent_id,ordinal",
            params![session,serde_json::to_string(&agents.iter().map(|agent|agent.id.as_str()).collect::<Vec<_>>())?],
        )?;
        for item in &mut exchange {
            item.output_store = Some(self.data_root.clone());
        }
        Ok(exchange)
    }
    /// Loads display metadata while leaving every tool-output chunk in SQLite.
    pub(crate) fn list_exchange_metadata(&self, session: &str) -> Result<Vec<Exchange>> {
        let mut exchange: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal",
            params![session, crate::session::HarnessSession::primary_agent_id(session)],
        )?;
        for item in &mut exchange {
            item.output_store = Some(self.data_root.clone());
        }
        Ok(exchange)
    }

    /// Reads one byte window and its committed source length without loading other output.
    pub(crate) fn tool_output_bytes(
        &self,
        exchange: &str,
        turn: &str,
        tool: &str,
        offset: u64,
        limit: u64,
    ) -> Result<(Vec<u8>, u64)> {
        self.read_snapshot(|| {
            let total: i64 = self.connection.query_row(
                "SELECT coalesce((SELECT byte_offset+length(cast(output AS BLOB)) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset DESC LIMIT 1),0)",
                params![exchange,turn,tool], |row| row.get(0),
            )?;
            ensure!(total >= 0, "stored tool output has a negative length");
            let total = total as u64;
            if offset >= total { return Ok((Vec::new(), total)); }
            let mut statement = self.connection.prepare(
                "SELECT byte_offset,substr(cast(output AS BLOB),max(0,?4-byte_offset)+1,min(length(cast(output AS BLOB)),?5-byte_offset)-max(0,?4-byte_offset)) FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND byte_offset >= (SELECT byte_offset FROM tool_output_chunk WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND byte_offset<=?4 ORDER BY byte_offset DESC LIMIT 1) AND byte_offset<?5 ORDER BY byte_offset")?;
            let mut output = Vec::new();
            let end = offset.saturating_add(limit).min(total);
            for row in statement.query_map(params![exchange,turn,tool,offset as i64,end as i64], |row| Ok((row.get::<_,i64>(0)?,row.get::<_,Vec<u8>>(1)?)))? {
                let (start, bytes) = row?;
                ensure!(start >= 0 && (start as u64).max(offset) == offset + output.len() as u64, "stored tool output has a discontinuous byte offset");
                output.extend(bytes);
            }
            ensure!(output.len() as u64 == end.saturating_sub(offset), "stored tool output ended before its committed length");
            Ok((output,total))
        })
    }

    /// Reads timeline metadata without hydrating any stored tool output.
    pub(crate) fn load_exchange_metadata(&self, id: &str) -> Result<Option<Exchange>> {
        Ok(self
            .list_payload("SELECT payload FROM exchange_record WHERE id=?1", [id])?
            .pop())
    }

    /// Seeks a character page through indexed chunk offsets on one committed snapshot.
    pub(crate) fn tool_output_page(
        &self,
        exchange: &str,
        turn: &str,
        tool: &str,
        offset: usize,
        limit: usize,
    ) -> Result<ToolOutputPage> {
        ensure!(limit > 0, "output page must contain at least one character");
        self.read_snapshot(|| {
            let total: i64 = self.connection.query_row("SELECT coalesce((SELECT characters FROM tool_output_summary WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3),0)",params![exchange,turn,tool],|row|row.get(0))?;
            let total_chars = usize::try_from(total).context("invalid stored character count")?;
            ensure!(offset <= total_chars,"output offset exceeds currently recorded text");
            let next = offset.saturating_add(limit).min(total_chars);
            let mut output = String::new();
            if next > offset {
                let mut statement = self.connection.prepare("SELECT p.char_offset,p.char_count,c.output FROM tool_output_position p JOIN tool_output_chunk c USING(exchange_id,turn_id,tool_id,byte_offset) WHERE p.exchange_id=?1 AND p.turn_id=?2 AND p.tool_id=?3 AND p.char_offset >= (SELECT char_offset FROM tool_output_position WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND char_offset<=?4 ORDER BY char_offset DESC LIMIT 1) AND p.char_offset<?5 ORDER BY p.char_offset")?;
                let mut observed = offset;
                for row in statement.query_map(params![exchange,turn,tool,offset as i64,next as i64],|row|Ok((row.get::<_,i64>(0)?,row.get::<_,i64>(1)?,row.get::<_,String>(2)?)))? {
                    let (start,count,text) = row?;
                    let start = usize::try_from(start).context("invalid stored character offset")?;
                    let count = usize::try_from(count).context("invalid stored character count")?;
                    ensure!(start.max(offset)==observed,"stored tool output has a discontinuous character offset");
                    let selected: String = text.chars().skip(offset.saturating_sub(start)).take(next-start.max(offset)).collect();
                    ensure!(text.chars().count()==count,"stored tool output has an invalid character count");
                    observed += selected.chars().count();
                    output.push_str(&selected);
                }
                ensure!(observed==next,"stored tool output ended before its committed character count");
            }
            Ok(ToolOutputPage { output, next_offset:(next<total_chars).then_some(next), total_chars })
        })
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
        let (persisted,characters): (i64,i64) = transaction.query_row("SELECT bytes,characters FROM tool_output_summary WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3",params![exchange.id,turn.id(),tool.id],|row|Ok((row.get(0)?,row.get(1)?)))?;
        let persisted = usize::try_from(persisted).context("invalid stored byte count")?;
        let characters = usize::try_from(characters).context("invalid stored character count")?;
        ensure!(
            persisted == offset,
            "tool output append does not follow the committed byte offset"
        );
        append_chunks(
            &transaction,
            &exchange.id,
            turn.id(),
            &tool.id,
            delta,
            offset,
            characters,
        )?;
        save_summary(
            &transaction,
            &exchange.id,
            turn.id(),
            &tool.id,
            &tool.output,
        )?;
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
