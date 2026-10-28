use anyhow::{Context, Result, ensure};
use rusqlite::{OptionalExtension, TransactionBehavior, params};
use serde::Deserialize;
use serde_json::{Value, json};

use super::SqliteStore;
use crate::plan::{PlanElicitation, PlanQuestionResponse};

pub(super) fn preserve_answers(
    transaction: &rusqlite::Transaction<'_>,
    table: &str,
    id: &str,
    encoded: String,
) -> Result<String> {
    let mut incoming: Value = serde_json::from_str(&encoded)?;
    for (pointer, path) in [
        ("/elicitation", "$.elicitation"),
        ("/acceptance/elicitation", "$.acceptance.elicitation"),
    ] {
        let Some(proposed) = incoming
            .pointer_mut(pointer)
            .filter(|value| value.is_object())
        else {
            continue;
        };
        let stored: Option<String> = transaction
            .query_row(
                &format!("SELECT json_extract(payload,?2) FROM {table} WHERE id=?1"),
                params![id, path],
                |row| row.get(0),
            )
            .optional()?
            .flatten();
        let Some(stored) = stored else { continue };
        let stored: Value = serde_json::from_str(&stored)?;
        if stored.pointer("/question_set/id") == proposed.pointer("/question_set/id")
            && stored["revision"] == proposed["revision"]
            && stored["answer_revision"].as_u64().unwrap_or(0)
                > proposed["answer_revision"].as_u64().unwrap_or(0)
        {
            *proposed = stored;
        }
    }
    Ok(serde_json::to_string(&incoming)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;

    #[test]
    fn answer_receipt_is_atomic_and_preserves_newer_answers() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
            "prompt":"question","kind":"chat","state":"complete","turn":[],"node_list":[],
            "created_at_ms":1,"attributed_matches_checkpoint":false,"awaiting_input":true
        }))?;
        exchange.elicitation = Some(PlanElicitation::new(serde_json::from_value(json!({
            "id":"set","questions":[{"id":"question","header":"Choice","question":"Which?",
                "options":[{"label":"First","description":"First choice","recommended":true}],
                "allow_freeform":true}]
        }))?));
        store.save_exchange(&exchange)?;
        let request = json!({"operation_id":"answer","target":{"owner":"interaction","id":"exchange",
            "set_id":"set","revision":1},"question_id":"question","response":{"kind":"selected","option":"First"}});
        store.connection.execute_batch("CREATE TRIGGER fail_receipt BEFORE INSERT ON command_receipt BEGIN SELECT RAISE(ABORT,'receipt failed'); END;")?;
        assert!(store.answer_question_command("session", &request).is_err());
        assert!(
            store
                .load_exchange("exchange")?
                .unwrap()
                .elicitation
                .unwrap()
                .answer
                .is_empty()
        );
        store
            .connection
            .execute_batch("DROP TRIGGER fail_receipt")?;
        let result = store.answer_question_command("session", &request)?;
        let mut newer = request.clone();
        newer["operation_id"] = json!("newer");
        newer["answer_order"] = json!(["writer", 2]);
        newer["response"] = json!({"kind":"other","text":"Newer answer"});
        store.answer_question_command("session", &newer)?;
        let mut delayed = request.clone();
        delayed["operation_id"] = json!("delayed");
        delayed["answer_order"] = json!(["writer", 1]);
        let latest = store.answer_question_command("session", &delayed)?;
        assert_eq!(
            latest["active_elicitation"]["elicitation"]["answer"][0]["response"]["text"],
            "Newer answer"
        );
        store.save_exchange(&exchange)?;
        assert_eq!(
            store
                .load_exchange("exchange")?
                .unwrap()
                .elicitation
                .unwrap()
                .answer
                .len(),
            1
        );
        let mut reopened = SqliteStore::open(root.path())?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            result
        );
        let mut changed = request.clone();
        changed["response"] = json!({"kind":"other","text":"Different"});
        assert!(
            reopened
                .answer_question_command("session", &changed)
                .is_err()
        );
        exchange.elicitation = None;
        exchange.awaiting_input = false;
        store.save_exchange(&exchange)?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            result
        );
        let mut stale = request;
        stale["operation_id"] = json!("late");
        assert!(reopened.answer_question_command("session", &stale).is_err());
        Ok(())
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum QuestionOwner {
    Interaction,
    Plan,
    PlanAcceptance,
}

#[derive(Deserialize)]
struct QuestionTarget {
    owner: QuestionOwner,
    id: String,
    set_id: String,
    revision: u32,
}

impl SqliteStore {
    /// Commits one identified answer and its retry receipt in the same transaction.
    pub(crate) fn answer_question_command(
        &mut self,
        session_id: &str,
        request: &Value,
    ) -> Result<Value> {
        let identity = request["operation_id"]
            .as_str()
            .context("question command identity is required")?;
        let target: QuestionTarget = serde_json::from_value(request["target"].clone())?;
        let question_id = request["question_id"]
            .as_str()
            .context("question identity is required")?;
        let response: PlanQuestionResponse = serde_json::from_value(request["response"].clone())?;
        let encoded = serde_json::to_string(request)?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let previous: Option<(String, String)> = transaction
            .query_row(
                "SELECT request,result FROM command_receipt WHERE session_id=?1 AND id=?2",
                params![session_id, identity],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .optional()?;
        if let Some((previous, result)) = previous {
            ensure!(
                previous == encoded,
                "question command identity was reused for a different answer"
            );
            return Ok(serde_json::from_str(&result)?);
        }
        let (table, pointer, condition, owner) = match target.owner {
            QuestionOwner::Interaction => (
                "exchange_record",
                "$.elicitation",
                "json_extract(payload,'$.awaiting_input')=1",
                "interaction",
            ),
            QuestionOwner::Plan => (
                "plan_record",
                "$.elicitation",
                "json_extract(payload,'$.state')='awaiting_input'",
                "plan",
            ),
            QuestionOwner::PlanAcceptance => (
                "plan_record",
                "$.acceptance.elicitation",
                "json_extract(payload,'$.state')='awaiting_review'",
                "plan_acceptance",
            ),
        };
        let text: Option<String> = transaction.query_row(
            &format!("SELECT json_extract(payload,?3) FROM {table} WHERE session_id=?1 AND id=?2 AND {condition}"),
            params![session_id, target.id, pointer], |row| row.get(0),
        ).optional()?.flatten();
        let mut elicitation: PlanElicitation = serde_json::from_str(
            &text.context("question owner is no longer awaiting this answer")?,
        )?;
        ensure!(
            elicitation.question_set.id == target.set_id && elicitation.revision == target.revision,
            "question set changed before the answer was committed"
        );
        let order: Option<(String, u64)> = request
            .get("answer_order")
            .map(|value| serde_json::from_value(value.clone()))
            .transpose()?;
        let superseded =
            order.as_ref().is_some_and(|(writer, sequence)| {
                elicitation.answer_order.get(question_id).is_some_and(
                    |(previous_writer, previous)| previous_writer == writer && previous >= sequence,
                )
            });
        if !superseded {
            elicitation.answer(question_id, response)?;
            if let Some(order) = order {
                elicitation
                    .answer_order
                    .insert(question_id.to_owned(), order);
            }
        }
        transaction.execute(
            &format!("UPDATE {table} SET payload=json_set(payload,?3,json(?4)) WHERE session_id=?1 AND id=?2"),
            params![session_id, target.id, pointer, serde_json::to_string(&elicitation)?],
        )?;
        let result = json!({"operation_id":identity,"active_elicitation":{
            "owner":owner,
            "exchange_id":if owner == "interaction" { Some(&target.id) } else { None },
            "plan_id":if owner != "interaction" { Some(&target.id) } else { None },
            "elicitation":elicitation,
        }});
        transaction.execute(
            "INSERT INTO command_receipt(session_id,id,request,result) VALUES(?1,?2,?3,?4)",
            params![
                session_id,
                identity,
                encoded,
                serde_json::to_string(&result)?
            ],
        )?;
        transaction.commit()?;
        self.publish_admission(session_id)?;
        Ok(result)
    }
}
