use std::sync::{Arc, atomic::{AtomicU64, Ordering}};

use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use tokio::sync::watch;

use super::{AgentCoordinator, AgentOperation, AgentRequest, AgentSource};
use crate::backend::ExecutionOwner;

/// Binds model tool authority to one authenticated root turn and parent exchange.
pub struct AgentToolScope {
    owner: ExecutionOwner,
    parent_exchange_id: String,
    coordinator: Arc<AgentCoordinator>,
    expired: watch::Sender<bool>,
    invocation: AtomicU64,
    observed_activity: AtomicU64,
    identity: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn tool_authority_rejects_children_foreign_roots_disabled_and_expired_turns() {
        let coordinator = AgentCoordinator::new("session".into());
        let child = ExecutionOwner {session_id:"session".into(),agent_id:"child".into()};
        assert!(AgentToolScope::new(child,"parent".into(),coordinator.clone()).is_err());
        assert!(AgentToolScope::new(ExecutionOwner::root("foreign"),"parent".into(),coordinator.clone()).is_err());
        let (scope,lease) = AgentToolScope::new(ExecutionOwner::root("session"),"parent".into(),coordinator.clone()).unwrap();
        assert!(scope.invoke("harness_list_agents",json!({})).await.is_err());
        assert!(coordinator.try_receive().is_none());
        coordinator.set_enabled(true);
        assert!(scope.available());
        assert!(scope.invoke("harness_spawn_agent",json!({"task_name":"forged","message":"task","execution_owner":{"session_id":"foreign"}})).await.is_err());
        assert!(coordinator.try_receive().is_none());
        drop(lease);
        assert!(!scope.available());
        assert!(scope.invoke("harness_list_agents",json!({})).await.is_err());
        assert!(coordinator.try_receive().is_none());
    }

    #[tokio::test]
    async fn expiring_a_queued_tool_closes_acknowledgment_and_retains_authenticated_owner() {
        let coordinator = AgentCoordinator::new("session".into());
        coordinator.set_enabled(true);
        let (scope,lease) = AgentToolScope::new(ExecutionOwner::root("session"),"parent".into(),coordinator.clone()).unwrap();
        let mut revision = coordinator.subscribe();
        let invocation = tokio::spawn(async move {scope.invoke("harness_list_agents",json!({})).await});
        tokio::time::timeout(std::time::Duration::from_secs(1),revision.changed()).await.unwrap().unwrap();
        drop(lease);
        assert!(tokio::time::timeout(std::time::Duration::from_secs(1),invocation).await.unwrap().unwrap().is_err());
        let Some(super::super::AgentCommand::Request {authority:Some(authority),response,..}) = coordinator.try_receive() else {panic!("missing authenticated queued invocation")};
        assert_eq!(authority.owner,ExecutionOwner::root("session"));
        assert_eq!(authority.parent_exchange_id,"parent");
        assert!(*authority.expired.borrow());
        assert!(response.is_closed());
    }
}

/// Revokes a provider turn's authority even when its prompt future is cancelled.
pub struct AgentToolLease(Arc<AgentToolScope>);

impl Drop for AgentToolLease {
    fn drop(&mut self) { self.0.expired.send_replace(true); }
}

impl AgentToolScope {
    pub fn new(owner: ExecutionOwner, parent_exchange_id: String, coordinator: Arc<AgentCoordinator>) -> Result<(Arc<Self>, AgentToolLease)> {
        ensure!(owner.is_root(), "children cannot acquire agent tool authority");
        ensure!(coordinator.owns(&owner), "agent tool authority crosses root session ownership");
        let (expired, _) = watch::channel(false);
        let observed_activity = AtomicU64::new(coordinator.activity_revision());
        let scope = Arc::new(Self {owner,parent_exchange_id,coordinator,expired,invocation:AtomicU64::new(0),observed_activity,identity:uuid::Uuid::new_v4().to_string()});
        Ok((scope.clone(),AgentToolLease(scope)))
    }

    pub fn available(&self) -> bool {self.owner.is_root() && !*self.expired.borrow() && self.coordinator.enabled()}

    pub async fn invoke(&self, name: &str, arguments: Value) -> Result<String> {
        ensure!(self.available(), "agent tools require an enabled authenticated active root turn");
        let text = |field: &str| arguments[field].as_str().filter(|text|!text.trim().is_empty()).map(str::to_owned)
            .with_context(||format!("{name} requires nonempty {field}"));
        let operation = match name {
            "harness_spawn_agent" => AgentOperation::Spawn(serde_json::from_value(arguments)?),
            "harness_send_message" => AgentOperation::Message {target:text("target")?,message:text("message")?},
            "harness_followup_task" => AgentOperation::Followup {target:text("target")?,message:text("message")?},
            "harness_interrupt_agent" => AgentOperation::Interrupt {target:text("target")?},
            "harness_list_agents" => AgentOperation::List {prefix:arguments["path_prefix"].as_str().map(str::to_owned)},
            "harness_wait_agent" => return self.wait(arguments["timeout_ms"].as_u64().unwrap_or(30000)).await,
            _ => anyhow::bail!("unknown agent tool: {name}"),
        };
        let response = self.request(operation).await?;
        Ok(serde_json::to_string(&response)?)
    }

    async fn request(&self, operation: AgentOperation) -> Result<Value> {
        let operation_id = format!("{}:{}",self.identity,self.invocation.fetch_add(1,Ordering::Relaxed));
        let mut expired = self.expired.subscribe();
        let sender = self.coordinator.sender();
        tokio::select! {
            biased;
            _ = expired.changed() => anyhow::bail!("agent tool context expired"),
            response = sender.request_authenticated(AgentRequest {operation_id,source:AgentSource::Model,operation},
                super::coordinator::AgentRequestAuthority {owner:self.owner.clone(),parent_exchange_id:self.parent_exchange_id.clone(),expired:self.expired.subscribe()}) => response,
        }
    }

    async fn wait(&self, timeout_ms: u64) -> Result<String> {
        ensure!((10000..=3600000).contains(&timeout_ms), "wait timeout must be between 10000 and 3600000 ms");
        let mut activity = self.coordinator.activity();
        let mut expired = self.expired.subscribe();
        let deadline = tokio::time::Instant::now() + std::time::Duration::from_millis(timeout_ms);
        let timed_out = loop {
            if !self.coordinator.unreported_result().is_empty()
                || self.coordinator.activity_revision() != self.observed_activity.load(Ordering::Acquire)
                || self.coordinator.active_count() == 0 {break false;}
            tokio::select! {
                biased;
                _ = expired.changed() => anyhow::bail!("agent wait interrupted by root turn expiration"),
                changed = activity.changed() => {changed.context("agent activity owner closed")?; break false;},
                _ = tokio::time::sleep_until(deadline) => break true,
            }
        };
        ensure!(self.available(), "agent tool context expired or disabled during wait");
        let observed_activity = self.coordinator.activity_revision();
        let result = self.coordinator.unreported_result();
        if !result.is_empty() {
            self.request(AgentOperation::Report {task_id:result.iter().map(|task|task.id.clone()).collect()}).await?;
        }
        self.observed_activity.store(observed_activity, Ordering::Release);
        Ok(serde_json::to_string(&json!({"timed_out":timed_out,"results":result,"agents":self.coordinator.snapshot(None)
            .into_iter().map(|(agent,task)|json!({"agent":agent,"latest_task":task})).collect::<Vec<_>>()}))?)
    }
}
