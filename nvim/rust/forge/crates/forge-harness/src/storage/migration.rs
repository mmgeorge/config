use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, params};
use serde_json::{Value, json};

/// Converts legacy routing metadata before the current transcript codec reads it.
pub(super) fn migrate(connection: &Connection) -> Result<()> {
    let session = records(connection, "session_record")?;
    for (id, mut envelope) in session {
        let version = envelope["format_version"].as_u64();
        if !matches!(version, Some(51 | 52)) {
            continue;
        }
        let mut legacy: LegacySession = serde_json::from_value(envelope["session"].clone())?;
        ensure!(
            legacy.id == id,
            "task migration session identity does not match its row"
        );
        if legacy.active_task.is_none() {
            legacy.active_task = selected_task(connection, &legacy)?;
        }
        if let Some(active) = &legacy.active_task {
            validate_task(connection, &id, active)?;
            if active.status().terminal() {
                archive_terminal_task(connection, &id, active)?;
                legacy.active_task = None;
            }
        }
        envelope["session"]["active_task"] = serde_json::to_value(legacy.active_task)?;
        let session = envelope["session"]
            .as_object_mut()
            .context("session is not an object")?;
        session.remove("current_task_id");
        session.remove("goal_id");
        envelope["format_version"] = json!(super::SESSION_FORMAT_VERSION);
        write_record(connection, "session_record", &id, &envelope)?;
    }

    let exchange = records(connection, "exchange_record")?;
    migrate_anchors(connection, &exchange)?;
    for (id, mut value) in exchange {
        migrate_exchange(&mut value)?;
        write_record(connection, "exchange_record", &id, &value)?;
    }
    let mut statement = connection.prepare(
        "SELECT session_id,ordinal,payload FROM fork_history_record ORDER BY session_id,ordinal",
    )?;
    let mut rows = statement.query([])?;
    while let Some(row) = rows.next()? {
        let session_id: String = row.get(0)?;
        let ordinal: i64 = row.get(1)?;
        let mut value: Value = super::decode(&row.get::<_, String>(2)?)?;
        migrate_inherited(&mut value)?;
        connection.execute(
            "UPDATE fork_history_record SET payload=?3 WHERE session_id=?1 AND ordinal=?2",
            params![session_id, ordinal, super::encode(&value)?],
        )?;
    }
    Ok(())
}

fn archive_terminal_task(
    connection: &Connection,
    session_id: &str,
    task: &crate::task::ActiveTask,
) -> Result<()> {
    let state = if task.status() == crate::task::TaskStatus::Completed {
        crate::task::TaskOutcomeState::Complete
    } else {
        crate::task::TaskOutcomeState::Cancelled
    };
    let event = crate::timeline::SessionEventRecord {
        id: format!("{session_id}:task-finished:{}", task.revision.as_str()),
        session_id: session_id.into(),
        owner: crate::timeline::TimelineOwner::session(session_id),
        created_at_ms: task.updated_at_ms,
        detail: crate::timeline::SessionEventKind::TaskFinished {
            outcome: crate::task::TaskOutcome {
                kind: task.kind(),
                title: task.title().into(),
                state,
                summary: "Task finished before migration".into(),
            },
        },
    };
    connection.execute("INSERT INTO session_event_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO NOTHING",params![event.id,session_id,super::encode(&event)?])?;
    let artifact = match &task.workflow {
        crate::task::TaskWorkflow::Plan { plan } => Some((
            "plan_record",
            plan.artifact.id.as_str(),
            serde_json::to_value(&plan.artifact)?,
        )),
        crate::task::TaskWorkflow::Execute { execution } => Some((
            "plan_execution_record",
            execution.record.id.as_str(),
            serde_json::to_value(&execution.record)?,
        )),
        crate::task::TaskWorkflow::Goal { .. } => None,
    };
    if let Some((table, id, mut artifact)) = artifact {
        artifact["schema_version"] = json!(crate::plan::PLAN_SCHEMA_VERSION);
        write_record(connection, table, id, &artifact)?;
    }
    Ok(())
}

pub(super) fn validate_task(
    connection: &Connection,
    session_id: &str,
    active: &crate::task::ActiveTask,
) -> Result<()> {
    match &active.workflow {
        crate::task::TaskWorkflow::Goal { .. } => {}
        crate::task::TaskWorkflow::Plan { plan } => {
            ensure!(
                plan.artifact.session_id == session_id,
                "active plan belongs to another session"
            );
            let artifact: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", &plan.artifact.id)?
                    .context("active plan artifact is missing")?;
            ensure!(
                artifact.session_id == session_id,
                "active plan artifact belongs to another session"
            );
        }
        crate::task::TaskWorkflow::Execute { execution } => {
            ensure!(
                execution.record.session_id == session_id,
                "active execution belongs to another session"
            );
            let artifact: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", &execution.record.plan_id)?
                    .context("active execution plan is missing")?;
            ensure!(
                artifact.session_id == session_id,
                "active execution plan belongs to another session"
            );
            let record: crate::plan::PlanExecutionRecord = super::task::read_record(
                connection,
                "plan_execution_record",
                &execution.record.id,
            )?
            .context("active execution artifact is missing")?;
            ensure!(
                record.session_id == session_id && record.plan_id == artifact.id,
                "active execution artifact has inconsistent ownership"
            );
        }
    }
    Ok(())
}

fn records(connection: &Connection, table: &str) -> Result<Vec<(String, Value)>> {
    let mut statement =
        connection.prepare(&format!("SELECT id,payload FROM {table} ORDER BY rowid"))?;
    let mut rows = statement.query([])?;
    let mut records = Vec::new();
    while let Some(row) = rows.next()? {
        records.push((row.get(0)?, super::decode(&row.get::<_, String>(1)?)?));
    }
    Ok(records)
}

fn write_record(connection: &Connection, table: &str, id: &str, value: &Value) -> Result<()> {
    connection.execute(
        &format!("UPDATE {table} SET payload=?2 WHERE id=?1"),
        params![id, super::encode(value)?],
    )?;
    Ok(())
}

fn migrate_inherited(value: &mut Value) -> Result<()> {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        if value.get("session_id").is_some() && value.get("node_list").is_some() {
            migrate_exchange(value)?;
        }
        match value {
            Value::Object(object) => {
                for child in object.values_mut() {
                    migrate_inherited(child)?;
                }
            }
            Value::Array(array) => {
                for child in array {
                    migrate_inherited(child)?;
                }
            }
            _ => {}
        }
        Ok(())
    })
}

fn migrate_exchange(value: &mut Value) -> Result<()> {
    let object = value.as_object_mut().context("exchange is not an object")?;
    if !object.contains_key("checklist") {
        if let Some(checklist) = object.remove("task") {
            object.insert("checklist".into(), checklist);
        }
    } else {
        object.remove("task");
    }
    if object
        .get("origin")
        .is_none_or(|origin| origin["kind"] == "user")
        && let Some(marker) = object.get("lifecycle").and_then(Value::as_str)
        && marker.ends_with(" continued")
    {
        object.insert(
            "origin".into(),
            json!({"kind":"task_continuation","reason":marker}),
        );
    }
    let execution = object.get("kind").and_then(Value::as_str) == Some("plan_execution");
    let phase = object.get("execution_phase").cloned();
    let exchange_id = object
        .get("id")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_owned();
    if let Some(nodes) = object.get_mut("node_list").and_then(Value::as_array_mut) {
        for node in nodes.iter_mut() {
            if node["kind"] == "artifact_change" && !node["change"]["declaration"].is_null() {
                node["change"]["revision_request"] = json!(execution);
            }
        }
        if let Some(phase) =
            phase.filter(|phase| phase["phase"] == "verify" && !phase["outcome"].is_null())
        {
            ensure!(
                !exchange_id.is_empty(),
                "verification exchange identity is missing"
            );
            let id = format!("{exchange_id}:verification");
            if !nodes.iter().any(|node| node["id"] == id) {
                nodes.push(json!({"kind":"verification_result","id":id,"result":{
                    "outcome":phase["outcome"], "summary":phase["summary"],
                    "findings":phase.get("findings").cloned().unwrap_or_else(|| json!([])),
                    "verification":phase["verification"],
                    "declaration_findings":phase.get("declaration_findings").cloned().unwrap_or_else(|| json!([])),
                    "declaration_changes":phase.get("declaration_changes").cloned().unwrap_or_else(|| json!([])),
                    "verification_tools":phase.get("verification_tools").cloned().unwrap_or_else(|| json!({}))
                }}));
            }
        }
    }
    for key in [
        "lifecycle",
        "kind",
        "plan_id",
        "execution_id",
        "execution_phase",
        "goal_id",
    ] {
        object.remove(key);
    }
    Ok(())
}

fn migrate_anchors(connection: &Connection, exchange: &[(String, Value)]) -> Result<()> {
    for table in [
        "plan_lifecycle_record",
        "plan_execution_record",
        "plan_resolution_record",
    ] {
        for (id, mut record) in records(connection, table)? {
            let session_id = record["session_id"]
                .as_str()
                .context("artifact session is missing")?
                .to_owned();
            let plan_id = record["plan_id"]
                .as_str()
                .context("artifact plan is missing")?
                .to_owned();
            if table == "plan_execution_record" {
                migrate_execution_anchors(&mut record, exchange)?;
            } else if record["anchor"].is_null() {
                let (time, admission, execution_id) = if table == "plan_lifecycle_record" {
                    (
                        record["created_at_ms"]
                            .as_i64()
                            .context("plan event time is missing")?,
                        matches!(
                            record["kind"].as_str(),
                            Some("accepted" | "changes_requested")
                        ),
                        None,
                    )
                } else {
                    (
                        record["resolved_at_ms"]
                            .as_i64()
                            .context("resolution time is missing")?,
                        false,
                        record["execution_id"].as_str(),
                    )
                };
                record["anchor"] = find_anchor(
                    exchange,
                    &session_id,
                    &plan_id,
                    execution_id,
                    time,
                    admission,
                )
                .unwrap_or(Value::Null);
            }
            write_record(connection, table, &id, &record)?;
        }
    }
    for (id, mut envelope) in records(connection, "session_record")? {
        if let Some(record) = envelope.pointer_mut("/session/active_task/execution/record") {
            migrate_execution_anchors(record, exchange)?;
            write_record(connection, "session_record", &id, &envelope)?;
        }
    }
    Ok(())
}

fn migrate_execution_anchors(record: &mut Value, exchange: &[(String, Value)]) -> Result<()> {
    let session_id = record["session_id"]
        .as_str()
        .context("execution session is missing")?
        .to_owned();
    let plan_id = record["plan_id"]
        .as_str()
        .context("execution plan is missing")?
        .to_owned();
    let execution_id = record["id"]
        .as_str()
        .context("execution identity is missing")?
        .to_owned();
    if let Some(lifecycle) = record["lifecycle"].as_array_mut() {
        for event in lifecycle {
            if event["anchor"].is_null() {
                let time = event["occurred_at_ms"]
                    .as_i64()
                    .context("execution event time is missing")?;
                let direct = event["after_exchange_id"]
                    .as_str()
                    .and_then(|after| {
                        exchange
                            .iter()
                            .find(|(id, value)| id == after && value["session_id"] == session_id)
                    })
                    .map(|(_, value)| anchor(value, time));
                event["anchor"] = direct
                    .or_else(|| {
                        find_anchor(
                            exchange,
                            &session_id,
                            &plan_id,
                            Some(&execution_id),
                            time,
                            true,
                        )
                    })
                    .unwrap_or(Value::Null);
            }
        }
    }
    record
        .as_object_mut()
        .context("execution is not an object")?
        .remove("goal_id");
    Ok(())
}

fn find_anchor(
    exchange: &[(String, Value)],
    session_id: &str,
    plan_id: &str,
    execution_id: Option<&str>,
    time: i64,
    admission: bool,
) -> Option<Value> {
    let matching: Vec<&Value> = exchange
        .iter()
        .map(|(_, value)| value)
        .filter(|value| {
            value["session_id"] == session_id
                && value["plan_id"] == plan_id
                && execution_id.is_none_or(|id| value["execution_id"] == id)
        })
        .collect();
    let selected = if admission {
        matching
            .iter()
            .find(|value| {
                value["created_at_ms"]
                    .as_i64()
                    .is_some_and(|created| created >= time)
            })
            .or_else(|| {
                matching.iter().rev().find(|value| {
                    value["created_at_ms"]
                        .as_i64()
                        .is_some_and(|created| created <= time)
                })
            })
    } else {
        matching
            .iter()
            .rev()
            .find(|value| {
                value["created_at_ms"]
                    .as_i64()
                    .is_some_and(|created| created <= time)
            })
            .or_else(|| matching.first())
    }?;
    Some(anchor(selected, time))
}

fn anchor(exchange: &Value, time: i64) -> Value {
    let mut count = 0;
    if exchange["created_at_ms"]
        .as_i64()
        .is_some_and(|created| time > created)
        && let Some(nodes) = exchange["node_list"].as_array()
    {
        for node in nodes {
            if node["kind"] == "plan_event" || node["kind"] == "timeline_event" {
                continue;
            }
            if node["kind"] == "exchange_input"
                && node["prompt"]["intent"] == "clarification"
                && node["prompt"]["created_at_ms"]
                    .as_i64()
                    .is_some_and(|created| created >= time)
            {
                break;
            }
            count += 1;
        }
    }
    json!({"exchange_id":exchange["id"],"node_count":count})
}

#[derive(serde::Deserialize)]
struct LegacySession {
    id: String,
    #[serde(default)]
    current_task_id: Option<String>,
    #[serde(default)]
    active_task: Option<crate::task::ActiveTask>,
}

fn selected_task(
    connection: &rusqlite::Connection,
    session: &LegacySession,
) -> Result<Option<crate::task::ActiveTask>> {
    use crate::task::{TaskKind, TaskStatus};
    use rusqlite::OptionalExtension;
    let Some(selected) = session.current_task_id.as_deref() else {
        return Ok(None);
    };
    let task: LegacyTask = super::task::read_record(connection, "task_record", selected)?
        .context("selected task record is missing")?;
    ensure!(
        task.session_id == session.id && task.id == selected,
        "selected task belongs to another session"
    );
    if task.status.terminal() {
        return Ok(None);
    }
    let mut active = match task.kind {
        TaskKind::Goal => {
            let id = task
                .goal_id
                .as_deref()
                .context("selected goal reference is missing")?;
            let goal: LegacyGoal = super::task::read_record(connection, "goal_record", id)?
                .context("selected goal record is missing")?;
            ensure!(
                goal.session_id == session.id,
                "selected goal belongs to another session"
            );
            let mut active = crate::task::ActiveTask::goal(
                goal.objective,
                goal.continuation,
                goal.native,
                goal.updated_at_ms,
            );
            let crate::task::TaskWorkflow::Goal { goal: current } = &mut active.workflow else {
                unreachable!()
            };
            current.state = if task.status == TaskStatus::Paused {
                crate::goal::GoalState::Paused
            } else {
                goal.state
            };
            active
        }
        TaskKind::Plan => {
            let id = task
                .plan_id
                .as_deref()
                .context("selected plan reference is missing")?;
            let plan: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", id)?
                    .context("selected plan record is missing")?;
            ensure!(
                plan.session_id == session.id,
                "selected plan belongs to another session"
            );
            let mut active = crate::task::ActiveTask::plan(plan, task.updated_at_ms);
            let crate::task::TaskWorkflow::Plan { plan } = &mut active.workflow else {
                unreachable!()
            };
            plan.paused = task.status == TaskStatus::Paused;
            active
        }
        TaskKind::Execute => {
            let plan_id = task
                .plan_id
                .as_deref()
                .context("selected execution plan reference is missing")?;
            let selected_plan: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", plan_id)?
                    .context("selected execution plan is missing")?;
            ensure!(
                selected_plan.session_id == session.id,
                "selected execution plan belongs to another session"
            );
            let execution_id: String = connection.query_row(
                "SELECT id FROM plan_execution_record WHERE session_id=?1 AND json_extract(payload,'$.plan_id')=?2 ORDER BY rowid DESC LIMIT 1",
                params![session.id,plan_id],|row|row.get(0)
            ).optional()?.context("selected execution record is missing")?;
            let legacy_execution: Value =
                super::task::read_record(connection, "plan_execution_record", &execution_id)?
                    .context("selected execution record is missing")?;
            let goal_id = legacy_execution["goal_id"]
                .as_str()
                .context("selected execution continuation reference is missing")?
                .to_owned();
            let execution: crate::plan::PlanExecutionRecord =
                serde_json::from_value(legacy_execution)?;
            ensure!(
                execution.session_id == session.id,
                "selected execution belongs to another session"
            );
            let goal: LegacyGoal = super::task::read_record(connection, "goal_record", &goal_id)?
                .context("selected execution continuation state is missing")?;
            ensure!(
                goal.session_id == session.id && task.goal_id.as_deref() == Some(&goal.id),
                "selected execution continuation reference is invalid"
            );
            crate::task::ActiveTask::execute(
                task.title.clone(),
                execution,
                goal.continuation,
                goal.native,
                task.updated_at_ms,
            )
        }
    };
    active.created_at_ms = task.created_at_ms;
    active.updated_at_ms = task.updated_at_ms;
    if active.status().terminal() {
        Ok(None)
    } else {
        Ok(Some(active))
    }
}

#[derive(Clone, serde::Deserialize)]
struct LegacyTask {
    id: String,
    session_id: String,
    title: String,
    kind: crate::task::TaskKind,
    status: crate::task::TaskStatus,
    plan_id: Option<String>,
    goal_id: Option<String>,
    created_at_ms: i64,
    updated_at_ms: i64,
}

#[derive(Clone, serde::Deserialize)]
struct LegacyGoal {
    id: String,
    session_id: String,
    objective: String,
    state: crate::goal::GoalState,
    #[serde(flatten)]
    continuation: crate::session::continuation::ContinuationBudget,
    native: bool,
    updated_at_ms: i64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;

    fn legacy_session(
        store: &mut super::super::SqliteStore,
        id: &str,
        version: u32,
        selected: Option<&str>,
    ) -> Result<()> {
        store.save_session(&super::super::test::session(id, "workspace"))?;
        store.connection.execute("UPDATE session_record SET payload=json_set(payload,'$.format_version',?2,'$.session.current_task_id',?3,'$.session.active_task',NULL) WHERE id=?1", params![id,version,selected])?;
        Ok(())
    }

    fn legacy_goal(
        store: &super::super::SqliteStore,
        session: &str,
        task: &str,
        goal_session: &str,
    ) -> Result<()> {
        let goal = json!({"id":"goal","session_id":goal_session,"objective":"Preserved objective","state":"paused","native":false,"updated_at_ms":2,
            "turn_count":0,"max_turn_count":10,"consecutive_no_progress":0,"max_consecutive_no_progress":3});
        store.connection.execute(
            "INSERT INTO goal_record(id,session_id,payload) VALUES('goal',?1,?2)",
            params![goal_session, super::super::encode(&goal)?],
        )?;
        let task = json!({"id":task,"session_id":session,"title":"Preserved objective","kind":"goal","status":"paused","goal_id":"goal","plan_id":null,"created_at_ms":1,"updated_at_ms":2});
        store.connection.execute(
            "INSERT INTO task_record(id,session_id,payload) VALUES(?1,?2,?3)",
            params![task["id"].as_str(), session, super::super::encode(&task)?],
        )?;
        Ok(())
    }

    #[test]
    fn task_migration_preserves_transcripts_and_artifacts() -> Result<()> {
        for version in [51, 52] {
            let root = tempfile::tempdir()?;
            let mut store = super::super::SqliteStore::open(root.path())?;
            legacy_session(&mut store, "session", version, Some("task"))?;
            legacy_goal(&store, "session", "task", "session")?;
            let source = json!({"id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,"prompt":"Keep the full prompt","state":"complete","created_at_ms":1,"kind":"plan_execution","plan_id":"plan","execution_id":"execution","goal_id":"goal","lifecycle":"Goal continued","task":{"items":[]},"execution_phase":{"phase":"verify","outcome":"passed","summary":"Verification complete","findings":[]},"node_list":[]});
            store.connection.execute("INSERT INTO exchange_record(id,session_id,agent_id,ordinal,payload) VALUES('exchange','session','session:agent:primary',1,?1)", [super::super::encode(&source)?])?;
            store.connection.pragma_update(None, "user_version", 4)?;
            drop(store);
            let store = super::super::SqliteStore::open(root.path())?;
            let active = store.load_active_task("session")?.unwrap();
            assert_eq!(active.title(), "Preserved objective");
            assert_eq!(active.status(), crate::task::TaskStatus::Paused);
            let payload: String = store.connection.query_row(
                "SELECT payload FROM exchange_record WHERE id='exchange'",
                [],
                |row| row.get(0),
            )?;
            let migrated: Value = super::super::decode(&payload)?;
            assert_eq!(migrated["prompt"], source["prompt"]);
            assert_eq!(migrated["origin"]["kind"], "task_continuation");
            assert!(migrated.get("task").is_none() && migrated.get("kind").is_none());
            assert_eq!(migrated["node_list"][0]["kind"], "verification_result");
            assert_eq!(
                migrated["node_list"][0]["result"]["summary"],
                "Verification complete"
            );
        }
        Ok(())
    }

    #[test]
    fn intermediate_active_slot_preserves_entry_and_overrides_legacy_selection() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = super::super::SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        let mut active = crate::task::ActiveTask::goal(
            "Authoritative objective".into(),
            Default::default(),
            false,
            1,
        );
        active.request_entry("Start this exact objective".into(), 2)?;
        let revision = active.revision.clone();
        let entry = active.entry.clone();
        store.commit_task_change("session", None, 2, |slot| {
            *slot = Some(active);
            Ok(())
        })?;
        store.connection.execute("UPDATE session_record SET payload=json_set(payload,'$.format_version',52,'$.session.current_task_id','missing-legacy-record')", [])?;
        store.connection.pragma_update(None, "user_version", 4)?;
        drop(store);
        let store = super::super::SqliteStore::open(root.path())?;
        let active = store.load_active_task("session")?.unwrap();
        assert_eq!(active.revision, revision);
        assert_eq!(active.entry, entry);
        assert_eq!(active.title(), "Authoritative objective");
        Ok(())
    }

    #[test]
    fn selected_plan_and_blocked_execution_preserve_artifact_identity() -> Result<()> {
        for kind in ["plan", "execute"] {
            let root = tempfile::tempdir()?;
            let mut store = super::super::SqliteStore::open(root.path())?;
            legacy_session(&mut store, "session", 51, Some("task"))?;
            let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
                "id":"plan","session_id":"session","request":"Preserved plan","state":"awaiting_review",
                "working_path":"","model_revision":1,"user_revision":0,"created_at_ms":0,"updated_at_ms":0
            }))?;
            store.save_plan(&plan)?;
            let task = json!({"id":"task","session_id":"session","title":"Preserved plan","kind":kind,"status":if kind=="plan" {"paused"} else {"blocked"},"goal_id":if kind=="execute" {Some("goal")} else {None},"plan_id":"plan","created_at_ms":1,"updated_at_ms":2});
            store.connection.execute(
                "INSERT INTO task_record(id,session_id,payload) VALUES('task','session',?1)",
                [super::super::encode(&task)?],
            )?;
            if kind == "execute" {
                let mut goal = serde_json::to_value(
                    crate::session::continuation::ContinuationBudget::default(),
                )?;
                for (key, value) in [
                    ("id", json!("goal")),
                    ("session_id", json!("session")),
                    ("objective", json!("Preserved plan")),
                    ("state", json!("blocked")),
                    ("native", json!(false)),
                    ("updated_at_ms", json!(2)),
                ] {
                    goal[key] = value;
                }
                store.connection.execute(
                    "INSERT INTO goal_record(id,session_id,payload) VALUES('goal','session',?1)",
                    [super::super::encode(&goal)?],
                )?;
                let execution: crate::plan::PlanExecutionRecord = serde_json::from_value(json!({
                    "id":"execution","session_id":"session","plan_id":"plan","goal_id":"goal",
                    "state":"blocked","phase":"verify","original_revision":1,"revision":1,"generation":1,
                    "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
                    "verification":[],"revision_history":[],"review_paused":false,"observed_source":{},
                    "observed_generation":0,"observed_check":[],"created_at_ms":0
                }))?;
                store.save_plan_execution(&execution)?;
                store.connection.execute("UPDATE plan_execution_record SET payload=json_set(payload,'$.goal_id','goal') WHERE id='execution'", [])?;
            }
            store.connection.pragma_update(None, "user_version", 4)?;
            drop(store);
            let store = super::super::SqliteStore::open(root.path())?;
            let active = store.load_active_task("session")?.unwrap();
            match active.workflow {
                crate::task::TaskWorkflow::Plan { plan } => {
                    assert_eq!(plan.artifact.id, "plan");
                    assert!(plan.paused);
                }
                crate::task::TaskWorkflow::Execute { execution } => {
                    assert_eq!(execution.record.id, "execution");
                    assert_eq!(
                        execution.record.state,
                        crate::plan::PlanExecutionState::Blocked
                    );
                }
                _ => panic!("selected workflow was not preserved"),
            }
            assert_eq!(store.list_plan("session")?.len(), 1);
        }
        Ok(())
    }

    #[test]
    fn malformed_or_foreign_selection_rolls_back_every_session() -> Result<()> {
        for foreign in [false, true] {
            let root = tempfile::tempdir()?;
            let mut store = super::super::SqliteStore::open(root.path())?;
            legacy_session(&mut store, "first", 51, None)?;
            legacy_session(&mut store, "broken", 52, Some("task"))?;
            if foreign {
                legacy_session(&mut store, "foreign", 52, None)?;
                legacy_goal(&store, "broken", "task", "foreign")?;
            }
            store.connection.pragma_update(None, "user_version", 4)?;
            drop(store);
            assert!(super::super::SqliteStore::open(root.path()).is_err());
            let connection = Connection::open(root.path().join("harness.sqlite3"))?;
            let version: i64 =
                connection.pragma_query_value(None, "user_version", |row| row.get(0))?;
            assert_eq!(version, 4);
            let changed:i64=connection.query_row("SELECT count(*) FROM session_record WHERE json_extract(payload,'$.format_version')=53",[],|row|row.get(0))?;
            assert_eq!(changed, 0);
        }
        Ok(())
    }

    #[test]
    fn artifact_anchors_are_captured_before_workflow_metadata_is_removed() -> Result<()> {
        let exchange = vec![(
            "exchange".into(),
            json!({"id":"exchange","session_id":"session","plan_id":"plan","execution_id":"execution","created_at_ms":1,"node_list":[{"kind":"turn_content"},{"kind":"plan_event"},{"kind":"exchange_input","prompt":{"intent":"clarification","created_at_ms":4}}]}),
        )];
        assert_eq!(
            find_anchor(&exchange, "session", "plan", Some("execution"), 3, false),
            Some(json!({"exchange_id":"exchange","node_count":1}))
        );
        assert_eq!(
            find_anchor(&exchange, "session", "plan", None, 1, true),
            Some(json!({"exchange_id":"exchange","node_count":0}))
        );
        Ok(())
    }
}
