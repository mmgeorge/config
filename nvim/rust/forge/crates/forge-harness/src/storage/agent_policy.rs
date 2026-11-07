use crate::agent::{Agent, AgentContinuationPolicy, AgentSource, AgentTask};
use crate::provider::ScopeOwner;
use anyhow::{Context, Result, ensure};
use forge_provider_api::ExchangeId;
use rusqlite::{Connection, OptionalExtension, params};

impl super::SqliteStore {
    /// Acknowledge captured child reports after an original user input was accepted.
    pub(crate) fn acknowledge_child_reports(
        &mut self,
        parent_scope: &crate::provider::InvocationScope,
        captured: &[AgentTask],
    ) -> Result<Vec<(Agent, AgentTask)>> {
        let parent_owner = parent_scope.owner();
        let session_id = parent_owner.session_id();
        ensure!(
            parent_scope.owner().agent_id().as_str()
                == crate::session::HarnessSession::primary_agent_id(session_id.as_str()),
            "only the original root owner can acknowledge child reports"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let parent_owned: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM exchange_record WHERE id=?1 AND session_id=?2 AND agent_id=?3)",
            params![parent_scope.exchange_id.as_str(),session_id.as_str(),parent_scope.owner().agent_id().as_str()],
            |row|row.get(0),
        )?;
        ensure!(
            parent_owned,
            "child report receipt has no original parent exchange"
        );
        let mut seen = std::collections::HashSet::new();
        let mut acknowledged = Vec::with_capacity(captured.len());
        for expected in captured {
            ensure!(
                seen.insert(&expected.id),
                "child report receipt repeats a task"
            );
            let payload: Option<String> = transaction.query_row(
                "SELECT payload FROM agent_task_record WHERE id=?1 AND session_id=?2 AND agent_id=?3",
                params![expected.id,session_id.as_str(),expected.agent_id],|row|row.get(0),
            ).optional()?;
            let mut task: AgentTask = super::decode(
                &payload.context("child report receipt references an unknown task")?,
            )?;
            ensure!(
                task.session_id == session_id.as_str()
                    && task.id == expected.id
                    && task.agent_id == expected.agent_id
                    && task.parent_exchange_id == parent_scope.exchange_id.as_str()
                    && task.source == AgentSource::Model
                    && task.completed_at_ms.is_some()
                    && task.state == expected.state
                    && task.completed_at_ms == expected.completed_at_ms,
                "child report receipt differs from the original completed task"
            );
            let payload: String = transaction.query_row(
                "SELECT payload FROM agent_run_record WHERE id=?1 AND session_id=?2",
                params![task.agent_id, session_id.as_str()],
                |row| row.get(0),
            )?;
            let mut agent: Agent = super::decode(&payload)?;
            ensure!(
                agent.id == task.agent_id && agent.session_id == task.session_id,
                "child report receipt changes the persisted agent owner"
            );
            validate(&transaction, &agent)?;
            if task.parent_reported && agent.continuation == AgentContinuationPolicy::Ready {
                continue;
            }
            let task_id = ExchangeId::from(task.id.as_str());
            ensure!(
                matches!(&agent.continuation, AgentContinuationPolicy::RequiresAuthorization {task_id: stopped} if stopped == &task_id),
                "child report receipt no longer matches the stopped task policy"
            );
            agent.authorize_continuation(&task_id)?;
            task.parent_reported = true;
            acknowledged.push((agent, task));
        }
        for (agent, task) in &acknowledged {
            super::SqliteStore::write_agent_task(&transaction, task)?;
            transaction.execute(
                "UPDATE agent_run_record SET payload=?1 WHERE id=?2 AND session_id=?3",
                params![super::encode(agent)?, agent.id, agent.session_id],
            )?;
        }
        transaction.commit()?;
        Ok(acknowledged)
    }
}

pub(super) fn validate(connection: &Connection, agent: &Agent) -> Result<()> {
    let AgentContinuationPolicy::RequiresAuthorization { task_id } = &agent.continuation else {
        return Ok(());
    };
    let payload: Option<String> = connection
        .query_row(
            "SELECT payload FROM agent_task_record WHERE id=?1 AND session_id=?2 AND agent_id=?3",
            params![task_id.as_str(), agent.session_id, agent.id],
            |row| row.get(0),
        )
        .optional()?;
    let task: AgentTask = super::decode(
        &payload.context("child continuation references an unknown or foreign task")?,
    )?;
    ensure!(
        task.id == task_id.as_str()
            && task.session_id == agent.session_id
            && task.agent_id == agent.id
            && task.completed_at_ms.is_some()
            && matches!(
                task.state,
                crate::exchange::ExchangeState::Complete
                    | crate::exchange::ExchangeState::Failed
                    | crate::exchange::ExchangeState::Cancelled
                    | crate::exchange::ExchangeState::Interrupted
            ),
        "child continuation does not reference a completed task with its persisted ownership"
    );
    let owns_exchange: bool = connection.query_row(
        "SELECT EXISTS(SELECT 1 FROM exchange_record WHERE id=?1 AND session_id=?2 AND agent_id=?3)",
        params![task.id, agent.session_id, agent.id],
        |row| row.get(0),
    )?;
    ensure!(
        owns_exchange,
        "child continuation task has no owned exchange"
    );
    Ok(())
}

pub(super) fn admit(
    connection: &Connection,
    incoming: &Agent,
    task: &AgentTask,
    authorization: Option<&ExchangeId>,
) -> Result<()> {
    let stored: Option<String> = connection
        .query_row(
            "SELECT payload FROM agent_run_record WHERE id=?1 AND session_id=?2",
            params![incoming.id, incoming.session_id],
            |row| row.get(0),
        )
        .optional()?;
    let Some(stored) = stored else {
        ensure!(
            authorization.is_none(),
            "child authorization has no persisted identity"
        );
        return validate(connection, incoming);
    };
    let stored: Agent = super::decode(&stored)?;
    ensure!(
        stored.id == incoming.id && stored.session_id == incoming.session_id,
        "child admission identity differs from its persisted owner"
    );
    validate(connection, &stored)?;
    match (&stored.continuation, authorization) {
        (AgentContinuationPolicy::Ready, None) => {
            ensure!(
                incoming.continuation == stored.continuation,
                "child admission changes its continuation policy"
            );
        }
        (AgentContinuationPolicy::RequiresAuthorization { task_id }, Some(grant)) => {
            ensure!(
                task.source == AgentSource::User && grant == task_id,
                "child continuation authorization is stale or is not an explicit user grant"
            );
            ensure!(
                incoming.continuation == AgentContinuationPolicy::Ready,
                "authorized child admission did not acknowledge the stopped task"
            );
        }
        (AgentContinuationPolicy::RequiresAuthorization { .. }, None) => {
            ensure!(
                incoming.continuation == stored.continuation,
                "child admission cannot implicitly clear a stopped task policy"
            );
        }
        (AgentContinuationPolicy::Ready, Some(_)) => {
            anyhow::bail!("child continuation authorization no longer matches a stopped task");
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;

    fn fixture() -> Result<(Connection, Agent, AgentTask)> {
        let connection = Connection::open_in_memory()?;
        connection.execute_batch(
            "CREATE TABLE agent_run_record(id TEXT,session_id TEXT,payload TEXT);
             CREATE TABLE agent_task_record(id TEXT,session_id TEXT,agent_id TEXT,payload TEXT);
             CREATE TABLE exchange_record(id TEXT,session_id TEXT,agent_id TEXT);",
        )?;
        let mut agent = Agent::pending("session", "review", "review", 1);
        let task = AgentTask {
            id: "stopped-task".into(),
            session_id: agent.session_id.clone(),
            agent_id: agent.id.clone(),
            operation_id: "operation".into(),
            ordinal: 1,
            source: AgentSource::User,
            parent_exchange_id: "parent".into(),
            message: "review".into(),
            state: crate::exchange::ExchangeState::Failed,
            final_response: None,
            error: Some("observation unavailable".into()),
            parent_reported: false,
            created_at_ms: 1,
            last_activity_at_ms: Some(2),
            completed_at_ms: Some(3),
        };
        agent.continuation = AgentContinuationPolicy::RequiresAuthorization {
            task_id: ExchangeId::from(task.id.as_str()),
        };
        connection.execute(
            "INSERT INTO exchange_record VALUES(?1,?2,?3)",
            params![task.id, task.session_id, task.agent_id],
        )?;
        connection.execute(
            "INSERT INTO agent_task_record VALUES(?1,?2,?3,?4)",
            params![
                task.id,
                task.session_id,
                task.agent_id,
                super::super::encode(&task)?
            ],
        )?;
        connection.execute(
            "INSERT INTO agent_run_record VALUES(?1,?2,?3)",
            params![agent.id, agent.session_id, super::super::encode(&agent)?],
        )?;
        Ok((connection, agent, task))
    }

    #[test]
    fn stopped_task_pointer_rejects_unknown_and_foreign_ownership() -> Result<()> {
        let (connection, agent, _) = fixture()?;
        validate(&connection, &agent)?;
        connection.execute("UPDATE agent_task_record SET agent_id='foreign'", [])?;
        assert!(validate(&connection, &agent).is_err());
        connection.execute("DELETE FROM agent_task_record", [])?;
        assert!(validate(&connection, &agent).is_err());
        assert!(matches!(
            agent.continuation,
            AgentContinuationPolicy::RequiresAuthorization { .. }
        ));
        Ok(())
    }

    #[test]
    fn only_matching_user_authorization_clears_latest_durable_policy() -> Result<()> {
        let (connection, mut agent, mut task) = fixture()?;
        let grant = ExchangeId::from("stopped-task");
        agent.authorize_continuation(&grant)?;
        assert!(admit(&connection, &agent, &task, None).is_err());
        assert!(admit(&connection, &agent, &task, Some(&ExchangeId::from("other"))).is_err());
        task.source = AgentSource::Model;
        assert!(admit(&connection, &agent, &task, Some(&grant)).is_err());
        task.source = AgentSource::User;
        admit(&connection, &agent, &task, Some(&grant))?;
        let payload: String =
            connection.query_row("SELECT payload FROM agent_task_record", [], |row| {
                row.get(0)
            })?;
        let retained: AgentTask = super::super::decode(&payload)?;
        assert_eq!(retained.state, crate::exchange::ExchangeState::Failed);
        assert_eq!(retained.error.as_deref(), Some("observation unavailable"));
        connection.execute(
            "UPDATE agent_run_record SET payload=?1",
            [super::super::encode(&agent)?],
        )?;
        assert!(admit(&connection, &agent, &task, Some(&grant)).is_err());
        Ok(())
    }

    #[test]
    fn accepted_parent_report_acknowledgement_is_atomic_and_preserves_child_failure() -> Result<()>
    {
        let temporary = tempfile::tempdir()?;
        let mut store = super::super::SqliteStore::open(temporary.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_agent_run(&Agent::primary("session", 0))?;
        let (_, mut agent, mut task) = fixture()?;
        let stopped = agent.continuation.clone();
        agent.continuation = AgentContinuationPolicy::Ready;
        let mut parent =
            crate::exchange::Exchange::agent_admission("session", 1, "review".into(), 1);
        task.source = AgentSource::Model;
        task.parent_exchange_id = parent.id.clone();
        parent.append_agent_task(&task)?;
        store.admit_agent_task(&agent, &task, &parent, None)?;
        let mut child = store.load_exchange(&task.id)?.context("child admitted")?;
        child.finish(crate::exchange::ExchangeOutcome::Failed, 3)?;
        agent.continuation = stopped;
        store.complete_agent_task(&agent, &task, &child)?;
        let scope = crate::provider::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(crate::provider::ThreadOwner::root(
                "session",
            )),
            exchange_id: ExchangeId::from(parent.id.as_str()),
            invocation_id: forge_provider_api::InvocationId::from("accepted-parent-input"),
        };
        let mut stale = task.clone();
        stale.state = crate::exchange::ExchangeState::Complete;
        assert!(store.acknowledge_child_reports(&scope, &[stale]).is_err());
        assert!(matches!(
            store
                .list_agent_run("session")?
                .into_iter()
                .find(|run| run.id == agent.id)
                .unwrap()
                .continuation,
            AgentContinuationPolicy::RequiresAuthorization { .. }
        ));
        assert!(!store.list_agent_task("session")?[0].parent_reported);
        let acknowledged = store.acknowledge_child_reports(&scope, std::slice::from_ref(&task))?;
        assert_eq!(acknowledged.len(), 1);
        assert_eq!(
            acknowledged[0].0.continuation,
            AgentContinuationPolicy::Ready
        );
        assert!(acknowledged[0].1.parent_reported);
        assert_eq!(
            acknowledged[0].1.state,
            crate::exchange::ExchangeState::Failed
        );
        assert_eq!(acknowledged[0].1.error, task.error);
        assert!(
            store
                .acknowledge_child_reports(&scope, std::slice::from_ref(&task))?
                .is_empty()
        );
        let mut newer = task.clone();
        newer.id = "newer-stopped-task".into();
        newer.operation_id = "newer-operation".into();
        newer.ordinal = 2;
        parent.append_agent_task(&newer)?;
        store.save_exchange(&parent)?;
        store.save_agent_task(&newer)?;
        let mut updated = acknowledged[0].0.clone();
        updated.continuation = AgentContinuationPolicy::RequiresAuthorization {
            task_id: ExchangeId::from(newer.id.as_str()),
        };
        store.save_agent_run(&updated)?;
        assert!(store.acknowledge_child_reports(&scope, &[task]).is_err());
        Ok(())
    }
}
