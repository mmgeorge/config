use super::{decode, decode_current_session};
use crate::{exchange::Exchange, session::HarnessSession};
use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, OpenFlags, OptionalExtension, params};
use std::path::Path;

/// Reads committed history without initializing schemas, hydrating output, or acquiring ownership.
pub(crate) struct SessionReader {
    connection: Connection,
}

impl SessionReader {
    pub(crate) fn open(root: &Path) -> Result<Self> {
        let connection = Connection::open_with_flags(
            root.join("harness.sqlite3"),
            OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        connection.busy_timeout(std::time::Duration::from_secs(5))?;
        connection.execute_batch("BEGIN DEFERRED")?;
        Ok(Self { connection })
    }

    pub(crate) fn sessions(
        &self,
        mut visit: impl FnMut(HarnessSession) -> Result<()>,
    ) -> Result<()> {
        let mut statement = self
            .connection
            .prepare("SELECT payload FROM session_record ORDER BY updated_at_ms DESC,id")?;
        for row in statement.query_map([], |row| row.get::<_, String>(0))? {
            if let Some(session) = decode_current_session(&row?)? {
                visit(session)?;
            }
        }
        Ok(())
    }

    pub(crate) fn session(&self, id: &str) -> Result<HarnessSession> {
        let payload: String = self
            .connection
            .query_row(
                "SELECT payload FROM session_record WHERE id=?1",
                [id],
                |row| row.get(0),
            )
            .optional()?
            .context("session does not exist")?;
        decode_current_session(&payload)?.context("session has an incompatible format")
    }

    pub(crate) fn exchanges(
        &self,
        session: &HarnessSession,
        mut visit: impl FnMut(Exchange) -> Result<()>,
    ) -> Result<()> {
        let mut statement = self.connection.prepare(
            "SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal,id",
        )?;
        for row in statement.query_map(params![session.id, session.primary_agent_id], |row| {
            row.get::<_, String>(0)
        })? {
            visit(decode(&row?)?)?;
        }
        Ok(())
    }

    pub(crate) fn exchange(&self, session: &str, id: &str) -> Result<Exchange> {
        let payload: String = self
            .connection
            .query_row(
                "SELECT payload FROM exchange_record WHERE session_id=?1 AND id=?2",
                params![session, id],
                |row| row.get(0),
            )
            .optional()?
            .context("exchange does not belong to the selected session")?;
        decode(&payload)
    }

    pub(crate) fn task(&self, session: &HarnessSession) -> Result<serde_json::Value> {
        let Some(id) = &session.current_task_id else {
            return Ok(serde_json::Value::Null);
        };
        let payload: String = self
            .connection
            .query_row(
                "SELECT payload FROM task_record WHERE session_id=?1 AND id=?2",
                params![session.id, id],
                |row| row.get(0),
            )
            .optional()?
            .context("selected session task is missing")?;
        let task: crate::task::TaskRecord = decode(&payload)?;
        Ok(
            serde_json::json!({"id":task.id,"kind":task.kind,"status":task.status,
            "phase":task.phase,"reason":task.reason}),
        )
    }

    pub(crate) fn output(
        &self,
        exchange: &str,
        turn: &str,
        tool: &str,
        mut visit: impl FnMut(&str) -> Result<()>,
    ) -> Result<()> {
        let mut statement = self.connection.prepare(
            "SELECT byte_offset,length(cast(output AS BLOB)) FROM tool_output_chunk
             WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 ORDER BY byte_offset",
        )?;
        let mut expected = 0i64;
        for row in statement.query_map(params![exchange, turn, tool], |row| {
            Ok((row.get::<_, i64>(0)?, row.get::<_, i64>(1)?))
        })? {
            let (byte_offset, bytes) = row?;
            ensure!(
                byte_offset == expected,
                "tool output has discontinuous stored chunks"
            );
            expected = expected
                .checked_add(bytes)
                .context("tool output offset overflow")?;
            let mut offset = 0i64;
            while offset < bytes {
                let window: Vec<u8> = self.connection.query_row(
                    "SELECT substr(cast(output AS BLOB),?5,16003) FROM tool_output_chunk
                     WHERE exchange_id=?1 AND turn_id=?2 AND tool_id=?3 AND byte_offset=?4",
                    params![exchange, turn, tool, byte_offset, offset + 1],
                    |row| row.get(0),
                )?;
                let valid = match std::str::from_utf8(&window) {
                    Ok(text) => text.len(),
                    Err(error) if error.error_len().is_none() => error.valid_up_to(),
                    Err(error) => return Err(error).context("stored tool output is not UTF-8"),
                };
                ensure!(valid > 0, "stored output ended inside a UTF-8 character");
                offset += valid as i64;
                visit(std::str::from_utf8(&window[..valid])?)?;
            }
        }
        Ok(())
    }
}
