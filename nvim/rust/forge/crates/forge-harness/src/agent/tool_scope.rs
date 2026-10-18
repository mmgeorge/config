use std::sync::{Arc, atomic::{AtomicU64, Ordering}};

use anyhow::{Context, Result, ensure};
use serde_json::Value;
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
    identity: String,
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use super::*;

    #[tokio::test]
    async fn tool_authority_rejects_children_foreign_roots_disabled_and_expired_turns() {
        let coordinator = AgentCoordinator::new("session".into());
        let child = ExecutionOwner {session_id:"session".into(),agent_id:"child".into()};
        assert!(AgentToolScope::new(child,"parent".into(),coordinator.clone()).is_err());
        assert!(AgentToolScope::new(ExecutionOwner::root("foreign"),"parent".into(),coordinator.clone()).is_err());
        let (scope,lease) = AgentToolScope::new(ExecutionOwner::root("session"),"parent".into(),coordinator.clone()).unwrap();
        assert!(scope.invoke("agent_list",json!({})).await.is_err());
        assert!(scope.invoke("agent_activity",json!({"target":"child"})).await.is_err());
        assert!(scope.invoke("agent_tool_output",json!({"call_ref":"foreign"})).await.is_err());
        assert!(coordinator.try_receive().is_none());
        coordinator.set_enabled(true);
        assert!(scope.available());
        for limit in [0,21] {
            assert!(scope.invoke("agent_activity",json!({"target":"child","limit":limit})).await.is_err());
        }
        for limit in [0,32001] {
            assert!(scope.invoke("agent_tool_output",json!({"call_ref":"foreign","limit":limit})).await.is_err());
        }
        assert!(scope.invoke("agent_spawn",json!({"profile":"worker","text":"task","execution_owner":{"session_id":"foreign"}})).await.is_err());
        assert!(coordinator.try_receive().is_none());
        coordinator.set_enabled(false);
        assert!(!scope.available());
        assert!(scope.invoke("agent_spawn",json!({"profile":"worker","text":"task"})).await.is_err());
        assert!(scope.invoke("agent_followup",json!({"target":"worker-1","message":"task"})).await.is_err());
        assert!(coordinator.try_receive().is_none());
        coordinator.set_enabled(true);
        assert!(scope.available(), "reenabling must restore the same live root scope");
        drop(lease);
        assert!(!scope.available());
        assert!(scope.invoke("agent_list",json!({})).await.is_err());
        assert!(coordinator.try_receive().is_none());
    }

    #[tokio::test]
    async fn expiring_a_queued_tool_closes_acknowledgment_and_retains_authenticated_owner() {
        let coordinator = AgentCoordinator::new("session".into());
        coordinator.set_enabled(true);
        let (scope,lease) = AgentToolScope::new(ExecutionOwner::root("session"),"parent".into(),coordinator.clone()).unwrap();
        let mut revision = coordinator.subscribe();
        let invocation = tokio::spawn(async move {scope.invoke("agent_list",json!({})).await});
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
        let scope = Arc::new(Self {owner,parent_exchange_id,coordinator,expired,invocation:AtomicU64::new(0),identity:uuid::Uuid::new_v4().to_string()});
        Ok((scope.clone(),AgentToolLease(scope)))
    }

    pub fn available(&self) -> bool {self.owner.is_root() && !*self.expired.borrow() && self.coordinator.enabled()}

    pub fn profile_schema(&self) -> Option<Value> { self.coordinator.profile_schema() }

    pub async fn invoke(&self, name: &str, arguments: Value) -> Result<String> {
        ensure!(self.available(), "agent tools require an enabled authenticated active root turn");
        let text = |field: &str| arguments[field].as_str().filter(|text|!text.trim().is_empty()).map(str::to_owned)
            .with_context(||format!("{name} requires nonempty {field}"));
        let operation = match name {
            "agent_spawn" => AgentOperation::Spawn(serde_json::from_value(arguments)?),
            "agent_send" => AgentOperation::Message {target:text("target")?,message:text("message")?},
            "agent_followup" => AgentOperation::Followup {target:text("target")?,message:text("message")?},
            "agent_interrupt" => AgentOperation::Interrupt {target:text("target")?},
            "agent_list" => AgentOperation::List {prefix:arguments["path_prefix"].as_str().map(str::to_owned)},
            "agent_activity" => {
                let limit = arguments.get("limit").map(|value| serde_json::from_value::<usize>(value.clone())).transpose()?;
                if let Some(limit) = limit { super::activity::validate_limit(limit)?; }
                AgentOperation::Activity {target:text("target")?,limit}
            }
            "agent_tool_output" => {
                let offset = arguments.get("offset").map(|value| serde_json::from_value::<usize>(value.clone())).transpose()?.unwrap_or(0);
                let limit = arguments.get("limit").map(|value| serde_json::from_value::<usize>(value.clone())).transpose()?.unwrap_or(8000);
                ensure!((1..=32000).contains(&limit), "output limit must be from 1 to 32000 characters");
                AgentOperation::ToolOutput {call_ref:text("call_ref")?,offset,limit}
            }
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

}
