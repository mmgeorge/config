use crate::permissions::command::{
    broad_command_pattern, exact_command_pattern, normalize_command_in,
};
use crate::permissions::directory::{DirectoryOperation, DirectoryPolicy};
use crate::permissions::document::{
    CATEGORY_BASH, CATEGORY_EDIT, CATEGORY_ELEVATE, CATEGORY_MCP, CATEGORY_READ, CATEGORY_TOOL,
    CATEGORY_WEBFETCH, PermissionDecision,
};
use crate::permissions::matcher::{
    PendingCommand, PermissionEvaluation, PermissionRequest, PermissionTarget,
};
use crate::permissions::shell;
use crate::permissions::store::PermissionStore;
use crate::provider::{ProviderEvent, ProviderEventSink};
use crate::session::{ApprovalMode, InputTimeout, ResolutionSource};
use anyhow::{Context, Result};
#[cfg(test)]
use serde_json::json;
use std::collections::{BTreeMap, HashMap};
use std::path::Path;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::oneshot;
use uuid::Uuid;

struct PendingApproval {
    token: Uuid,
    view: ApprovalRequestView,
    response: oneshot::Sender<ApprovalResolution>,
    resolution_sink: Option<ProviderEventSink>,
    directory_policy: Option<Arc<DirectoryPolicy>>,
    authority: Option<Arc<ApprovalAuthority>>,
}

/// Serialize retirement with approval admission and persistent decisions for one native generation.
#[derive(Default)]
pub(crate) struct ApprovalAuthority(Mutex<bool>);

impl ApprovalAuthority {
    /// Revoke this generation after any already-committing decision completes.
    pub(crate) fn retire(&self) -> Result<()> {
        *self
            .0
            .lock()
            .map_err(|_| anyhow::anyhow!("approval authority poisoned"))? = true;
        Ok(())
    }
}

struct PendingRegistration<'registry> {
    registry: &'registry Mutex<BTreeMap<String, PendingApproval>>,
    id: String,
    token: Uuid,
}

impl Drop for PendingRegistration<'_> {
    fn drop(&mut self) {
        if let Ok(mut pending) = self.registry.lock()
            && pending
                .get(&self.id)
                .is_some_and(|entry| entry.token == self.token)
        {
            pending.remove(&self.id);
        }
    }
}

pub struct PermissionCoordinator {
    pending_map: Mutex<BTreeMap<String, PendingApproval>>,
    store: Arc<RwLock<PermissionStore>>,
    timeout_by_session: Mutex<BTreeMap<String, InputTimeout>>,
    feedback_by_owner: Mutex<HashMap<crate::provider::ExecutionOwner, String>>,
    feedback_watch:
        Mutex<HashMap<crate::provider::ExecutionOwner, tokio::sync::watch::Sender<bool>>>,
    skill_by_owner: Mutex<HashMap<crate::provider::ExecutionOwner, Arc<crate::skill::SkillAccess>>>,
    directory_by_scope: Mutex<HashMap<crate::provider::InvocationScope, Arc<DirectoryPolicy>>>,
}

impl PermissionCoordinator {
    /// Report the captured write-off boundary for an active execution owner.
    pub(crate) fn writes_disabled(
        &self,
        scope: Option<&crate::provider::InvocationScope>,
    ) -> Result<bool> {
        let policy = self
            .directory_by_scope
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?;
        Ok(scope
            .and_then(|scope| policy.get(scope))
            .is_some_and(|policy| policy.access.write_access == crate::session::WriteScope::Off))
    }
    pub fn new(store: Arc<RwLock<PermissionStore>>) -> Self {
        Self {
            pending_map: Mutex::new(BTreeMap::new()),
            store,
            timeout_by_session: Mutex::new(BTreeMap::new()),
            feedback_by_owner: Mutex::new(HashMap::new()),
            feedback_watch: Mutex::new(HashMap::new()),
            skill_by_owner: Mutex::new(HashMap::new()),
            directory_by_scope: Mutex::new(HashMap::new()),
        }
    }

    pub(crate) fn directories(
        self: &Arc<Self>,
        root: Option<&Path>,
        request: &crate::provider::ProviderRequest,
        scope: &crate::provider::InvocationScope,
    ) -> Result<DirectoryExecution> {
        anyhow::ensure!(
            request.owner()? == scope.owner,
            "directory policy belongs to a different invocation owner"
        );
        let policy = Arc::new(DirectoryPolicy {
            access: request.access.clone(),
            workspace: request.workspace.clone().into(),
            session_id: request.harness_session_id.clone(),
            settings_root: root.map(Path::to_owned),
        });
        let mut directory = self
            .directory_by_scope
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?;
        anyhow::ensure!(
            !directory.contains_key(scope),
            "directory access already belongs to an active execution"
        );
        directory.insert(scope.clone(), policy.clone());
        Ok(DirectoryExecution {
            scope: scope.clone(),
            policy,
            coordinator: self.clone(),
        })
    }

    /// Bind selected bundle access to one provider execution and revoke it when the guard drops.
    pub(crate) fn skills(
        self: &Arc<Self>,
        root: Option<&Path>,
        request: &crate::provider::ProviderRequest,
        descriptor: &crate::provider::ProviderDescriptor,
    ) -> Result<crate::skill::SkillExecution> {
        let target = descriptor
            .capability
            .integration
            .resource_target
            .as_deref()
            .context("provider has no managed resource target")?;
        let provider = descriptor.kind.as_str();
        let selected = if request.mode == crate::provider::PromptMode::Background {
            Vec::new()
        } else if let Some(root) = root {
            crate::settings::SessionResources::load(
                root,
                &crate::provider::ProviderCatalogRequest {
                    execution_owner: request.execution_owner.clone(),
                    harness_session_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    access: request.access.clone(),
                    approval_mode: request.approval_mode,
                    provider_session_id: request.provider_session_id.clone(),
                },
                provider,
                target,
            )?
            .selected_skill
        } else {
            Vec::new()
        };
        let access = Arc::new(crate::skill::SkillAccess::new(
            &request.workspace,
            &selected,
        )?);
        let owner = request.owner()?;
        let mut scope = self
            .skill_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("skill permission lock poisoned"))?;
        anyhow::ensure!(
            !scope.contains_key(&owner),
            "skill access already belongs to an active execution"
        );
        scope.insert(owner.clone(), access.clone());
        Ok(crate::skill::SkillExecution {
            observer: Arc::new(Mutex::new(crate::skill::SkillObserver::new(access.clone()))),
            access,
            owner,
            coordinator: self.clone(),
        })
    }

    /// Revoke only the matching execution generation's temporary bundle grants.
    pub(crate) fn release_skills(
        &self,
        owner: &crate::provider::ExecutionOwner,
        access: &Arc<crate::skill::SkillAccess>,
    ) {
        if let Ok(mut scope) = self.skill_by_owner.lock()
            && scope
                .get(owner)
                .is_some_and(|current| Arc::ptr_eq(current, access))
        {
            scope.remove(owner);
        }
    }

    /// Configure future requests without extending any pending deadline.
    pub fn configure_timeout(&self, session_id: &str, timeout: InputTimeout) -> Result<()> {
        self.timeout_by_session
            .lock()
            .map_err(|_| anyhow::anyhow!("approval timeout lock poisoned"))?
            .insert(session_id.to_owned(), timeout.validate()?);
        Ok(())
    }

    /// Read the current settings for one session independently of active provider work.
    pub fn approval_timeout(&self, session_id: &str) -> Result<InputTimeout> {
        Ok(self
            .timeout_by_session
            .lock()
            .map_err(|_| anyhow::anyhow!("approval timeout lock poisoned"))?
            .get(session_id)
            .copied()
            .unwrap_or_default())
    }

    /// Consume one owner's automatic-rejection guidance at its next continuation boundary.
    pub fn take_timeout_feedback(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> Result<Option<String>> {
        let feedback = self
            .feedback_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?
            .remove(owner);
        self.publish_timeout_feedback(owner)?;
        Ok(feedback)
    }

    /// Subscribe to one owner's retained feedback without losing changes during registration.
    pub(crate) fn subscribe_timeout_feedback(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> Result<tokio::sync::watch::Receiver<bool>> {
        let feedback = self
            .feedback_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?;
        let mut watch = self
            .feedback_watch
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback subscription lock poisoned"))?;
        watch.retain(|_, sender| sender.receiver_count() > 0);
        Ok(watch
            .entry(owner.clone())
            .or_insert_with(|| tokio::sync::watch::channel(feedback.contains_key(owner)).0)
            .subscribe())
    }

    fn publish_timeout_feedback(&self, owner: &crate::provider::ExecutionOwner) -> Result<()> {
        let feedback = self
            .feedback_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?;
        let mut watch = self
            .feedback_watch
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback subscription lock poisoned"))?;
        watch.retain(|_, sender| sender.receiver_count() > 0);
        if let Some(sender) = watch.get(owner) {
            sender.send_if_modified(|pending| {
                let next = feedback.contains_key(owner);
                if *pending == next {
                    false
                } else {
                    *pending = next;
                    true
                }
            });
        }
        Ok(())
    }

    /// Check whether one owner needs a serialized continuation after denial.
    pub fn has_timeout_feedback(&self, owner: &crate::provider::ExecutionOwner) -> Result<bool> {
        Ok(self
            .feedback_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?
            .contains_key(owner))
    }

    pub fn store(&self) -> Arc<RwLock<PermissionStore>> {
        Arc::clone(&self.store)
    }

    pub fn transient(workspace: impl AsRef<Path>) -> Result<Arc<Self>> {
        let directory = std::env::temp_dir()
            .join("forge-harness")
            .join(Uuid::new_v4().to_string());
        let store = PermissionStore::load(directory.join("permissions.json"), workspace.as_ref())?;
        Ok(Arc::new(Self::new(Arc::new(RwLock::new(store)))))
    }

    /// Evaluate file access and saved rules before resolving ordinary tool approval.
    pub async fn authorize(
        &self,
        approval_mode: ApprovalMode,
        request: PermissionRequest,
        address: Option<crate::provider::TurnAddress>,
        event_sink: Option<&ProviderEventSink>,
    ) -> Result<ApprovalResolution> {
        self.authorize_scoped(approval_mode, request, address, event_sink, None)
            .await
    }

    /// Evaluate approval under a revocable native callback generation.
    pub(crate) async fn authorize_scoped(
        &self,
        approval_mode: ApprovalMode,
        mut request: PermissionRequest,
        address: Option<crate::provider::TurnAddress>,
        event_sink: Option<&ProviderEventSink>,
        authority: Option<Arc<ApprovalAuthority>>,
    ) -> Result<ApprovalResolution> {
        if request.target_list.is_empty()
            || request
                .target_list
                .iter()
                .any(|target| matches!(target, PermissionTarget::Unknown))
        {
            return Ok(ApprovalResolution::DenyPolicy);
        }
        let directory_policy = address
            .as_ref()
            .map(|address| &address.scope)
            .or_else(|| event_sink.and_then(|sink| sink.scope().invocation()))
            .map(|scope| {
                self.directory_by_scope
                    .lock()
                    .map(|directory| directory.get(scope).cloned())
                    .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))
            })
            .transpose()?
            .flatten();
        let evaluated = (|| -> Result<PermissionEvaluation> {
            let store = self
                .store
                .read()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?;
            if let Some(policy) = &directory_policy {
                let mut evaluation = policy.evaluate(&store.compiled(), &mut request)?;
                if protected_target(&request, &store) {
                    evaluation.decision = PermissionDecision::Deny;
                }
                Ok(evaluation)
            } else {
                let mut evaluation = store.compiled().evaluate(&request)?;
                if protected_target(&request, &store) {
                    evaluation.decision = PermissionDecision::Deny;
                }
                Ok(evaluation)
            }
        })();
        let mut evaluation = match evaluated {
            Ok(evaluation) => evaluation,
            Err(error) => {
                if let Some(sink) = event_sink {
                    sink.send_wait(ProviderEvent::Diagnostic {
                        scope: sink.scope().clone(),
                        diagnostic: crate::provider::ProviderDiagnostic {
                            kind: forge_provider_api::ProviderFailureKind::Authorization,
                            message: forge_provider_api::SanitizedMessage::new(
                                "Permission evaluation failed",
                            )
                            .expect("static diagnostic is nonempty"),
                            fatal: false,
                        },
                    })
                    .await?;
                }
                return Err(error);
            }
        };
        if evaluation.decision != PermissionDecision::Deny {
            let scopes = self
                .skill_by_owner
                .lock()
                .map_err(|_| anyhow::anyhow!("skill permission lock poisoned"))?;
            let access = address
                .as_ref()
                .map(|address| &address.scope.owner)
                .or_else(|| event_sink.map(|sink| sink.scope().owner()))
                .and_then(|owner| scopes.get(owner).cloned());
            drop(scopes);
            if let Some(access) = access {
                let mut granted = Vec::new();
                let targets = request
                    .target_list
                    .iter()
                    .chain(evaluation.pending_target_list.iter());
                for target in targets {
                    if !access.approves(target) {
                        continue;
                    }
                    if matches!(target, PermissionTarget::Command { .. })
                        && evaluation.pending_command_list.iter().any(|command| {
                            &command.target == target && command.source.contains(['>', '<'])
                        })
                    {
                        continue;
                    }
                    let canonical = access.permission_paths(target);
                    let mut canonical = PermissionRequest {
                        id: request.id.clone(),
                        provider: request.provider.clone(),
                        reason: None,
                        target_list: canonical
                            .into_iter()
                            .map(|path| PermissionTarget::Read {
                                path: path.display().to_string(),
                            })
                            .collect(),
                    };
                    let compiled = self
                        .store
                        .read()
                        .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                        .compiled();
                    let canonical = if let Some(policy) = &directory_policy {
                        policy.evaluate(&compiled, &mut canonical)?
                    } else {
                        compiled.evaluate(&canonical)?
                    };
                    if canonical.decision == PermissionDecision::Deny {
                        return Ok(ApprovalResolution::DenyPolicy);
                    }
                    granted.push(target.clone());
                }
                evaluation
                    .pending_target_list
                    .retain(|target| !granted.contains(target));
                evaluation
                    .pending_command_list
                    .retain(|command| !granted.contains(&command.target));
                if evaluation.pending_target_list.is_empty() {
                    evaluation.decision = PermissionDecision::Allow;
                }
            }
        }
        match evaluation.decision {
            PermissionDecision::Allow => return Ok(ApprovalResolution::AllowOnce),
            PermissionDecision::Deny => return Ok(ApprovalResolution::DenyPolicy),
            PermissionDecision::Ask => match approval_mode {
                ApprovalMode::Deny => return Ok(ApprovalResolution::DenyMode),
                ApprovalMode::Allow => return Ok(ApprovalResolution::AllowOnce),
                ApprovalMode::Ask => {}
            },
        }
        let execution_owner = address
            .as_ref()
            .map(|address| address.scope.owner.clone())
            .or_else(|| event_sink.map(|sink| sink.scope().owner().clone()));
        if let Some(owner) = &execution_owner {
            request.id = format!("{}:{}:{}", owner.session_id, owner.agent_id, request.id);
        }
        let mut view = approval_view(&request, &evaluation);
        if let Some(policy) = &directory_policy {
            for (item, target) in view
                .item_list
                .iter_mut()
                .zip(&evaluation.pending_target_list)
            {
                if let Some(grant) = policy.grant(target)? {
                    let workspace = dunce::canonicalize(&policy.workspace)?;
                    if grant.operation == DirectoryOperation::Write
                        || !crate::permissions::matcher::path_starts_with(
                            Path::new(&grant.path),
                            &workspace,
                        )
                    {
                        for choice in &mut item.choice_list {
                            if matches!(
                                choice.resolution,
                                ApprovalResolution::AllowBroad | ApprovalResolution::DenyBroad
                            ) {
                                for (_, pattern) in &mut choice.rule_list {
                                    *pattern = grant.path.clone();
                                }
                                let action = if choice.resolution == ApprovalResolution::AllowBroad
                                {
                                    "allow"
                                } else {
                                    "deny"
                                };
                                choice.label = format!("Always {action} directory: {}", grant.path);
                            }
                        }
                    }
                    if let PermissionTarget::Write { path } = target
                        && crate::permissions::matcher::path_starts_with(
                            Path::new(path),
                            &workspace,
                        )
                    {
                        if Path::new(&grant.path) == workspace {
                            item.choice_list.retain(|choice| choice.id != "allow_broad");
                        }
                        item.choice_list.insert(
                            1,
                            ApprovalChoice {
                                id: "allow_workspace_write".into(),
                                label: format!(
                                    "Always allow writes in this workspace: {}",
                                    workspace.display()
                                ),
                                resolution: ApprovalResolution::AllowBroad,
                                rule_list: vec![(
                                    CATEGORY_EDIT.into(),
                                    workspace.to_string_lossy().into_owned(),
                                )],
                                directory: None,
                            },
                        );
                    }
                    if policy.settings_root.is_none() {
                        continue;
                    }
                    let label = match grant.operation {
                        DirectoryOperation::Read => "readable",
                        DirectoryOperation::Write => "writable",
                    };
                    item.choice_list.insert(
                        1,
                        ApprovalChoice {
                            id: "allow_global_directory".into(),
                            label: format!(
                                "Allow and add to global {label} directories: {}",
                                grant.path
                            ),
                            resolution: ApprovalResolution::AllowOnce,
                            rule_list: Vec::new(),
                            directory: Some(grant),
                        },
                    );
                }
            }
        }
        view.execution_owner = execution_owner;
        self.wait_for_approval(view, address, event_sink, directory_policy, authority)
            .await
    }

    /// Reviews one exact proposal without creating reusable permission grants.
    pub(crate) async fn review(
        &self,
        title: String,
        detail: String,
        scope: crate::provider::InvocationScope,
        event_sink: &ProviderEventSink,
    ) -> Result<ApprovalResolution> {
        let view = ApprovalRequestView {
            id: Uuid::new_v4().to_string(),
            execution_owner: Some(scope.owner.clone()),
            exchange_id: Some(scope.exchange_id.to_string()),
            turn_id: None,
            provider: None,
            reason: None,
            deadline: None,
            resolution_source: ResolutionSource::User,
            item_list: vec![ApprovalItem {
                id: "proposal".into(),
                title,
                detail,
                command_list: Vec::new(),
                choice_list: vec![
                    ApprovalChoice {
                        id: "allow_once".into(),
                        label: "Approve update".into(),
                        resolution: ApprovalResolution::AllowOnce,
                        rule_list: Vec::new(),
                        directory: None,
                    },
                    ApprovalChoice {
                        id: "deny_once".into(),
                        label: "Reject update".into(),
                        resolution: ApprovalResolution::DenyOnce,
                        rule_list: Vec::new(),
                        directory: None,
                    },
                    ApprovalChoice {
                        id: "cancel".into(),
                        label: "Cancel update".into(),
                        resolution: ApprovalResolution::Cancel,
                        rule_list: Vec::new(),
                        directory: None,
                    },
                ],
            }],
        };
        self.wait_for_approval(view, None, Some(event_sink), None, None)
            .await
    }

    async fn wait_for_approval(
        &self,
        mut view: ApprovalRequestView,
        _address: Option<crate::provider::TurnAddress>,
        event_sink: Option<&ProviderEventSink>,
        directory_policy: Option<Arc<DirectoryPolicy>>,
        authority: Option<Arc<ApprovalAuthority>>,
    ) -> Result<ApprovalResolution> {
        let id = view.id.clone();
        let timeout = self
            .timeout_by_session
            .lock()
            .map_err(|_| anyhow::anyhow!("approval timeout lock poisoned"))?
            .get(
                view.execution_owner
                    .as_ref()
                    .map(|owner| owner.session_id.as_str())
                    .unwrap_or_default(),
            )
            .copied()
            .unwrap_or_default();
        let now_ms = i64::try_from(
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)?
                .as_millis(),
        )?;
        view.deadline = timeout.deadline(now_ms)?;
        let expires = tokio::time::Instant::now() + timeout.duration();
        let (sender, mut receiver) = oneshot::channel();
        let token = Uuid::new_v4();
        {
            let generation = authority
                .as_ref()
                .map(|authority| {
                    authority
                        .0
                        .lock()
                        .map_err(|_| anyhow::anyhow!("approval authority poisoned"))
                })
                .transpose()?;
            anyhow::ensure!(
                !generation.as_ref().is_some_and(|retired| **retired),
                "provider approval generation is retired"
            );
            let mut pending = self
                .pending_map
                .lock()
                .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
            anyhow::ensure!(
                !pending.contains_key(&id),
                "approval request {id} is already pending"
            );
            pending.insert(
                id.clone(),
                PendingApproval {
                    token,
                    view: view.clone(),
                    response: sender,
                    resolution_sink: event_sink.cloned(),
                    directory_policy,
                    authority: authority.clone(),
                },
            );
        }
        let _registration = PendingRegistration {
            registry: &self.pending_map,
            id: id.clone(),
            token,
        };
        if let Some(event_sink) = event_sink {
            event_sink
                .send_wait(ProviderEvent::Approval {
                    scope: event_sink.invocation_scope()?.clone(),
                    update: crate::provider::ApprovalUpdate::Requested(view.clone()),
                })
                .await?;
        }
        let expiry = async {
            if timeout.amount == 0 {
                std::future::pending::<()>().await;
            }
            tokio::time::sleep_until(expires).await;
        };
        tokio::select! {
            result = &mut receiver => result.with_context(|| format!("approval request {id} was cancelled")),
            () = expiry => {
                let pending = {
                    let generation = authority.as_ref().map(|authority| authority.0.lock().map_err(|_| anyhow::anyhow!("approval authority poisoned"))).transpose()?;
                    if generation.as_ref().is_some_and(|retired| **retired) {
                        return Ok(ApprovalResolution::Cancel);
                    }
                    let mut pending = self.pending_map.lock().map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?.remove(&id);
                    if let Some(pending) = &mut pending {
                        pending.view.resolution_source = ResolutionSource::Timeout;
                        if let Some(owner) = &pending.view.execution_owner {
                            self.feedback_by_owner.lock().map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?
                                .insert(owner.clone(), format!("Approval {} was automatically rejected because its timeout expired. This was not an explicit user rejection. The existing user instruction and permissions remain in effect. Continue authorized work, avoid repeating the expired request, and do not ask whether to continue work the user already requested.", pending.view.id));
                            self.publish_timeout_feedback(owner)?;
                        }
                    }
                    pending
                };
                if let Some(pending) = pending {
                    Self::emit_lifecycle(event_sink, "approval_resolved", &pending.view).await?;
                    Ok(ApprovalResolution::TimedOut)
                } else {
                    // A manual resolver already owns the terminal decision and its persistence.
                    receiver.await.with_context(|| format!("approval request {id} was cancelled"))
                }
            }
        }
    }

    /// Attach canonical execution ownership before publishing or snapshotting an approval.
    pub(crate) fn bind_owner(
        &self,
        id: &str,
        exchange_id: &str,
        turn_id: Option<&str>,
    ) -> Result<Option<ApprovalRequestView>> {
        let turn_id = turn_id.map(forge_provider_api::ProviderTurnId::from);
        let mut pending = self
            .pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
        let Some(approval) = pending.get_mut(id) else {
            return Ok(None);
        };
        anyhow::ensure!(
            approval
                .view
                .exchange_id
                .as_deref()
                .is_none_or(|owner| owner == exchange_id),
            "approval cannot change its owning exchange"
        );
        approval.view.exchange_id = Some(exchange_id.to_owned());
        anyhow::ensure!(
            approval
                .view
                .turn_id
                .as_ref()
                .is_none_or(|owner| Some(owner) == turn_id.as_ref()),
            "approval cannot change its owning turn"
        );
        approval.view.turn_id = turn_id;
        Ok(Some(approval.view.clone()))
    }

    /// Resolve a pending request and publish its terminal event on the requesting execution's stream.
    /// Use `event_sink` only when the request was registered without a stream.
    pub async fn resolve(
        &self,
        approval_id: &str,
        answer_list: &[ApprovalAnswer],
        event_sink: Option<&ProviderEventSink>,
    ) -> Result<ApprovalRequestView> {
        let (pending, resolution, directory_list, persisted) = {
            let (token, authority) = self
                .pending_map
                .lock()
                .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?
                .get(approval_id)
                .map(|pending| (pending.token, pending.authority.clone()))
                .with_context(|| format!("approval request is no longer pending: {approval_id}"))?;
            let generation = authority
                .as_ref()
                .map(|authority| {
                    authority
                        .0
                        .lock()
                        .map_err(|_| anyhow::anyhow!("approval authority poisoned"))
                })
                .transpose()?;
            anyhow::ensure!(
                !generation.as_ref().is_some_and(|retired| **retired),
                "provider approval generation is retired"
            );
            let (pending, resolution, rule_list, directory_list) = {
                let mut pending_map = self
                    .pending_map
                    .lock()
                    .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
                let std::collections::btree_map::Entry::Occupied(entry) =
                    pending_map.entry(approval_id.to_owned())
                else {
                    anyhow::bail!("approval request is no longer pending: {approval_id}");
                };
                let pending = entry.get();
                anyhow::ensure!(
                    pending.token == token,
                    "approval request was replaced: {approval_id}"
                );
                anyhow::ensure!(
                    !pending
                        .view
                        .deadline
                        .as_ref()
                        .is_some_and(|deadline| deadline.expired(0)),
                    "approval request timed out: {approval_id}"
                );
                anyhow::ensure!(
                    answer_list.len() == pending.view.item_list.len(),
                    "every approval item requires one answer"
                );
                let mut answer_map = BTreeMap::new();
                for answer in answer_list {
                    anyhow::ensure!(
                        answer_map
                            .insert(&answer.item_id, &answer.choice_id)
                            .is_none(),
                        "duplicate approval item: {}",
                        answer.item_id
                    );
                }
                let mut resolution = ApprovalResolution::AllowOnce;
                let mut rule_map = BTreeMap::new();
                let mut directory_list = Vec::new();
                for item in &pending.view.item_list {
                    let choice_id = answer_map
                        .get(&item.id)
                        .with_context(|| format!("missing approval item: {}", item.id))?;
                    let choice = item
                        .choice_list
                        .iter()
                        .find(|choice| &choice.id == *choice_id)
                        .with_context(|| format!("unknown approval choice: {choice_id}"))?;
                    if let Some(directory) = &choice.directory
                        && !directory_list.contains(directory)
                    {
                        directory_list.push(directory.clone());
                    }
                    match choice.resolution {
                        ApprovalResolution::Cancel => resolution = ApprovalResolution::Cancel,
                        ApprovalResolution::TimedOut
                        | ApprovalResolution::DenyOnce
                        | ApprovalResolution::DenyExact
                        | ApprovalResolution::DenyBroad
                            if resolution != ApprovalResolution::Cancel =>
                        {
                            resolution = ApprovalResolution::DenyOnce
                        }
                        _ => {}
                    }
                    let decision = match choice.resolution {
                        ApprovalResolution::AllowExact | ApprovalResolution::AllowBroad => {
                            Some(PermissionDecision::Allow)
                        }
                        ApprovalResolution::DenyExact | ApprovalResolution::DenyBroad => {
                            Some(PermissionDecision::Deny)
                        }
                        _ => None,
                    };
                    if let Some(decision) = decision {
                        for rule in &choice.rule_list {
                            if let Some(previous) = rule_map.insert(rule.clone(), decision) {
                                anyhow::ensure!(
                                    previous == decision,
                                    "conflicting approval decisions for {} {}",
                                    rule.0,
                                    rule.1
                                );
                            }
                        }
                    }
                }
                let rule_list = if resolution == ApprovalResolution::Cancel {
                    Vec::new()
                } else {
                    rule_map
                        .into_iter()
                        .map(|((category, pattern), decision)| (category, pattern, decision))
                        .collect()
                };
                anyhow::ensure!(
                    !pending.response.is_closed(),
                    "provider no longer waits for approval {approval_id}"
                );
                let pending = entry.remove();
                if resolution != ApprovalResolution::AllowOnce {
                    directory_list.clear();
                }
                (pending, resolution, rule_list, directory_list)
            };
            let persisted = (|| -> Result<()> {
                if !directory_list.is_empty() {
                    pending
                        .directory_policy
                        .as_ref()
                        .context("directory policy is no longer available")?
                        .save(&directory_list)?;
                }
                if !rule_list.is_empty() {
                    let mut store = self
                        .store
                        .write()
                        .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?;
                    if resolution == ApprovalResolution::AllowOnce
                        && rule_list
                            .iter()
                            .all(|(_, _, decision)| *decision == PermissionDecision::Allow)
                    {
                        store.grant_approved(&rule_list)?;
                    } else {
                        store.set_rule_list(&rule_list)?;
                    }
                }
                Ok(())
            })();
            (pending, resolution, directory_list, persisted)
        };
        let event_sink = pending.resolution_sink.as_ref().or(event_sink);
        if let Err(error) = persisted {
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view).await?;
            return Err(error);
        }
        // Queue the terminal event before the provider can resume and finish its turn.
        if !directory_list.is_empty()
            && let Some(sink) = event_sink
        {
            sink.send_wait(ProviderEvent::DirectoryAccess {
                scope: sink.invocation_scope()?.clone(),
                grant: directory_list,
            })
            .await?;
        }
        Self::emit_lifecycle(event_sink, "approval_resolved", &pending.view).await?;
        if pending.response.send(resolution).is_err() {
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view).await?;
            anyhow::bail!("provider no longer waits for approval {approval_id}");
        }
        Ok(pending.view)
    }

    pub fn pending_list(&self) -> Result<Vec<ApprovalRequestView>> {
        Ok(self
            .pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?
            .values()
            .map(|pending| pending.view.clone())
            .collect())
    }

    pub async fn cancel_all(&self, event_sink: Option<&ProviderEventSink>) -> Result<()> {
        self.feedback_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?
            .clear();
        for sender in self
            .feedback_watch
            .lock()
            .map_err(|_| anyhow::anyhow!("approval feedback subscription lock poisoned"))?
            .values()
        {
            sender.send_if_modified(|pending| {
                if *pending {
                    *pending = false;
                    true
                } else {
                    false
                }
            });
        }
        let pending_list = {
            let mut pending_map = self
                .pending_map
                .lock()
                .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
            std::mem::take(&mut *pending_map)
                .into_values()
                .collect::<Vec<_>>()
        };
        Self::cancel_pending(pending_list, event_sink).await
    }

    /// Cancel only approvals owned by the interrupted execution.
    pub async fn cancel_owner(
        &self,
        owner: &crate::provider::ExecutionOwner,
        event_sink: Option<&ProviderEventSink>,
    ) -> Result<()> {
        let pending_list = self.take_owner_approvals(owner)?;
        Self::cancel_pending(pending_list, event_sink).await
    }

    /// Publish cancellation on each request's own stream and release every waiter on failure.
    async fn cancel_pending(
        pending_list: Vec<PendingApproval>,
        event_sink: Option<&ProviderEventSink>,
    ) -> Result<()> {
        let mut first_error = None;
        for pending in pending_list {
            let sink = pending.resolution_sink.as_ref().or(event_sink);
            let result = Self::emit_lifecycle(sink, "approval_cancelled", &pending.view).await;
            let _ = pending.response.send(ApprovalResolution::Cancel);
            if let Err(error) = result {
                first_error.get_or_insert(error);
            }
        }
        match first_error {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    /// Begin cancellation synchronously after callback authority is checked.
    pub(crate) fn cancel_owner_immediate(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> Result<()> {
        for pending in self.take_owner_approvals(owner)? {
            let _ = pending.response.send(ApprovalResolution::Cancel);
        }
        Ok(())
    }

    fn take_owner_approvals(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> Result<Vec<PendingApproval>> {
        self.take_timeout_feedback(owner)?;
        let pending_list = {
            let mut pending_map = self
                .pending_map
                .lock()
                .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
            let id_list: Vec<_> = pending_map
                .iter()
                .filter(|(_, pending)| pending.view.execution_owner.as_ref() == Some(owner))
                .map(|(id, _)| id.clone())
                .collect();
            id_list
                .into_iter()
                .filter_map(|id| pending_map.remove(&id))
                .collect::<Vec<_>>()
        };
        Ok(pending_list)
    }

    async fn emit_lifecycle(
        event_sink: Option<&ProviderEventSink>,
        kind: &str,
        view: &ApprovalRequestView,
    ) -> Result<()> {
        if let Some(event_sink) = event_sink {
            let update = match kind {
                "approval_resolved" => crate::provider::ApprovalUpdate::Resolved(view.clone()),
                "approval_cancelled" => crate::provider::ApprovalUpdate::Cancelled(view.clone()),
                _ => anyhow::bail!("unsupported approval lifecycle: {kind}"),
            };
            event_sink
                .send_wait(ProviderEvent::Approval {
                    scope: event_sink.invocation_scope()?.clone(),
                    update,
                })
                .await?;
        }
        Ok(())
    }
}

/// Revokes only the matching execution's reported file authorization when its provider settles.
pub(crate) struct DirectoryExecution {
    scope: crate::provider::InvocationScope,
    policy: Arc<DirectoryPolicy>,
    coordinator: Arc<PermissionCoordinator>,
}

impl DirectoryExecution {
    /// Get the directory evaluator owned by this admitted provider invocation.
    pub(crate) fn policy(&self) -> Arc<DirectoryPolicy> {
        self.policy.clone()
    }
}

impl Drop for DirectoryExecution {
    fn drop(&mut self) {
        if let Ok(mut directory) = self.coordinator.directory_by_scope.lock()
            && directory
                .get(&self.scope)
                .is_some_and(|policy| Arc::ptr_eq(policy, &self.policy))
        {
            directory.remove(&self.scope);
        }
    }
}

fn target_rule_list(target: &PermissionTarget, broad: bool) -> Vec<(String, String)> {
    let mut rule_list = Vec::new();
    match target {
        PermissionTarget::Unknown | PermissionTarget::UnknownFile { .. } => {}
        PermissionTarget::Command { command, shell } => {
            let normalization = normalize_command_in(command, shell.unwrap_or_default());
            if normalization.failure.is_some() {
                return rule_list;
            }
            for invocation in &normalization.invocation_list {
                let pattern = if broad {
                    broad_command_pattern(invocation)
                } else {
                    exact_command_pattern(invocation)
                };
                if let Some(pattern) = pattern {
                    rule_list.push((CATEGORY_BASH.into(), pattern));
                }
            }
        }
        PermissionTarget::Read { path } => rule_list.push((
            CATEGORY_READ.into(),
            if broad { ".".into() } else { path.clone() },
        )),
        PermissionTarget::Write { path } => rule_list.push((
            CATEGORY_EDIT.into(),
            if broad { ".".into() } else { path.clone() },
        )),
        PermissionTarget::Network { target } => rule_list.push((
            CATEGORY_WEBFETCH.into(),
            if broad { "*".into() } else { target.clone() },
        )),
        PermissionTarget::Mcp { target } => rule_list.push((
            CATEGORY_MCP.into(),
            if broad {
                target
                    .split('/')
                    .next()
                    .map_or_else(|| "*".into(), |server| format!("{server}/*"))
            } else {
                target.clone()
            },
        )),
        PermissionTarget::Elevate { target } => rule_list.push((
            CATEGORY_ELEVATE.into(),
            if broad { "*".into() } else { target.clone() },
        )),
        PermissionTarget::Tool { target } => rule_list.push((
            CATEGORY_TOOL.into(),
            if broad { "*".into() } else { target.clone() },
        )),
        PermissionTarget::HostTool { name } => rule_list.push((
            CATEGORY_TOOL.into(),
            if broad { "*".into() } else { name.clone() },
        )),
    }
    rule_list.sort();
    rule_list.dedup();
    rule_list
}

fn display_rule_list(rule_list: &[(String, String)]) -> String {
    rule_list
        .iter()
        .map(|(category, pattern)| {
            let label = if category == CATEGORY_BASH {
                "shell"
            } else {
                category.as_str()
            };
            format!("{label} {pattern}")
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn approval_view(
    request: &PermissionRequest,
    evaluation: &PermissionEvaluation,
) -> ApprovalRequestView {
    ApprovalRequestView {
        id: request.id.clone(),
        deadline: None,
        resolution_source: ResolutionSource::User,
        execution_owner: None,
        exchange_id: None,
        turn_id: None,
        provider: Some(request.provider.clone()),
        reason: request.reason.clone(),
        item_list: evaluation
            .pending_target_list
            .iter()
            .enumerate()
            .map(|(index, target)| {
                let mut item = approval_item(target, index);
                item.command_list = command_context(&evaluation.pending_command_list, target);
                item
            })
            .collect(),
    }
}

fn approval_item(target: &PermissionTarget, index: usize) -> ApprovalItem {
    let (title, detail) = match target {
        PermissionTarget::Unknown => ("Unknown operation".into(), "The operation could not be normalized".into()),
        PermissionTarget::UnknownFile { operation } => (
            format!("{operation:?} path not supplied"),
            "The provider did not report a file path. Directory limits and protected paths cannot be verified. Allow only this request or reject it.".into(),
        ),
        PermissionTarget::Command { command, shell } => (
            "Run command".into(),
            crate::permissions::command::display_command(command, shell.unwrap_or_default()).source,
        ),
        PermissionTarget::Read { path } => ("Read path".into(), path.clone()),
        PermissionTarget::Write { path } => ("Write path".into(), path.clone()),
        PermissionTarget::Network { target } => ("Access network".into(), target.clone()),
        PermissionTarget::Mcp { target } => ("Call MCP tool".into(), target.clone()),
        PermissionTarget::Elevate { target } => ("Elevate process".into(), target.clone()),
        PermissionTarget::Tool { target } => ("Call provider tool".into(), target.clone()),
        PermissionTarget::HostTool { name } => ("Call host tool".into(), name.clone()),
    };
    let exact_rule_list = target_rule_list(target, false);
    let broad_rule_list = target_rule_list(target, true);
    let persistent = !exact_rule_list.is_empty();
    let mut item = ApprovalItem {
        id: index.to_string(),
        title,
        detail,
        command_list: Vec::new(),
        choice_list: vec![
            ApprovalChoice {
                id: "allow_once".into(),
                label: "Allow once".into(),
                resolution: ApprovalResolution::AllowOnce,
                rule_list: Vec::new(),
                directory: None,
            },
            ApprovalChoice {
                id: "allow_exact".into(),
                label: format!(
                    "Always allow exact: {}",
                    display_rule_list(&exact_rule_list)
                ),
                resolution: ApprovalResolution::AllowExact,
                rule_list: exact_rule_list.clone(),
                directory: None,
            },
            ApprovalChoice {
                id: "allow_broad".into(),
                label: format!(
                    "Always allow broad: {}",
                    display_rule_list(&broad_rule_list)
                ),
                resolution: ApprovalResolution::AllowBroad,
                rule_list: broad_rule_list.clone(),
                directory: None,
            },
            ApprovalChoice {
                id: "deny_once".into(),
                label: "Deny once".into(),
                resolution: ApprovalResolution::DenyOnce,
                rule_list: Vec::new(),
                directory: None,
            },
            ApprovalChoice {
                id: "deny_exact".into(),
                label: format!("Always deny exact: {}", display_rule_list(&exact_rule_list)),
                resolution: ApprovalResolution::DenyExact,
                rule_list: exact_rule_list,
                directory: None,
            },
            ApprovalChoice {
                id: "deny_broad".into(),
                label: format!("Always deny broad: {}", display_rule_list(&broad_rule_list)),
                resolution: ApprovalResolution::DenyBroad,
                rule_list: broad_rule_list,
                directory: None,
            },
            ApprovalChoice {
                id: "cancel".into(),
                label: "Reject all".into(),
                resolution: ApprovalResolution::Cancel,
                rule_list: Vec::new(),
                directory: None,
            },
        ],
    };
    if !persistent {
        item.choice_list.retain(|choice| {
            matches!(
                choice.resolution,
                ApprovalResolution::AllowOnce
                    | ApprovalResolution::DenyOnce
                    | ApprovalResolution::Cancel
            )
        });
    }
    item
}

fn command_context(
    pending_list: &[PendingCommand],
    target: &PermissionTarget,
) -> Vec<ApprovalCommand> {
    let mut result: Vec<ApprovalCommand> = Vec::new();
    for pending in pending_list
        .iter()
        .filter(|pending| pending.target == *target)
    {
        let display = crate::permissions::command::display_command(&pending.source, pending.shell);
        let Some(focus) = display.project_range(&pending.source_range) else {
            continue;
        };
        if let Some(context) = result
            .iter_mut()
            .find(|context| context.source == display.source && context.shell == display.shell)
        {
            context.focus_range_list.push(focus);
        } else {
            result.push(ApprovalCommand {
                highlight_list: shell::highlights(&display.source, display.shell),
                source: display.source,
                shell: display.shell,
                focus_range_list: vec![focus],
            });
        }
    }
    result
}

pub fn protected_target(request: &PermissionRequest, store: &PermissionStore) -> bool {
    request.target_list.iter().any(|target| match target {
        PermissionTarget::Write { path } => store.protects(Path::new(path)),
        PermissionTarget::Command { command, shell } => {
            store.protects_command_in(command, shell.unwrap_or_default())
        }
        _ => false,
    })
}

pub use forge_provider_api::approval::{
    ApprovalAnswer, ApprovalChoice, ApprovalCommand, ApprovalItem, ApprovalRequestView,
    ApprovalResolution,
};

#[cfg(test)]
mod test {
    use crate::provider::{ApprovalUpdate, ExecutionOwner, InvocationScope, TurnAddress};
    use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};
    use forge_shell::shell::CommandShell;

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ExecutionOwner::root("approval"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn normalized_permission(id: &str, target_list: Vec<PermissionTarget>) -> PermissionRequest {
        PermissionRequest {
            id: id.into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: None,
            target_list,
        }
    }

    fn approval(event: ProviderEvent) -> ApprovalRequestView {
        let ProviderEvent::Approval {
            scope,
            update: ApprovalUpdate::Requested(view),
        } = event
        else {
            panic!("expected requested approval");
        };
        assert_eq!(view.execution_owner.as_ref(), Some(&scope.owner));
        view
    }

    #[tokio::test]
    async fn approval_modes_resolve_rules_without_persisting_automatic_decisions() -> Result<()> {
        for mode in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            for decision in [
                PermissionDecision::Ask,
                PermissionDecision::Allow,
                PermissionDecision::Deny,
            ] {
                let directory = tempfile::tempdir()?;
                let coordinator = PermissionCoordinator::transient(directory.path())?;
                coordinator.store().write().unwrap().set_rule_list(&[(
                    CATEGORY_TOOL.into(),
                    "sample".into(),
                    decision,
                )])?;
                let before = coordinator.store.read().unwrap().open().1;
                let (sink, mut stream) = crate::provider::events::channel(scope());
                let waiting_coordinator = coordinator.clone();
                let waiting = tokio::spawn(async move {
                    waiting_coordinator
                        .authorize(
                            mode,
                            PermissionRequest {
                                id: "decision".into(),
                                provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                                reason: None,
                                target_list: vec![PermissionTarget::Tool {
                                    target: "sample".into(),
                                }],
                            },
                            None,
                            Some(&sink),
                        )
                        .await
                });
                let expected = match (decision, mode) {
                    (PermissionDecision::Deny, _) => ApprovalResolution::DenyPolicy,
                    (PermissionDecision::Ask, ApprovalMode::Deny) => ApprovalResolution::DenyMode,
                    (PermissionDecision::Allow, _)
                    | (PermissionDecision::Ask, ApprovalMode::Allow) => {
                        ApprovalResolution::AllowOnce
                    }
                    (PermissionDecision::Ask, ApprovalMode::Ask) => {
                        let event =
                            tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
                                .await?
                                .context("approval stream closed")?
                                .context("approval event missing")?;
                        let view = approval(event);
                        coordinator
                            .resolve(
                                &view.id,
                                &[ApprovalAnswer {
                                    item_id: view.item_list[0].id.clone(),
                                    choice_id: "allow_once".into(),
                                }],
                                None,
                            )
                            .await?;
                        ApprovalResolution::AllowOnce
                    }
                };
                assert_eq!(waiting.await??, expected);
                assert!(coordinator.pending_list()?.is_empty());
                assert_eq!(coordinator.store.read().unwrap().open().1, before);
            }
        }
        Ok(())
    }

    #[tokio::test]
    async fn directory_approval_persists_global_grants_before_releasing_requests() -> Result<()> {
        use super::*;
        let root = tempfile::tempdir()?;
        let workspace = root.path().join("workspace");
        let external = root.path().join("external");
        std::fs::create_dir(&workspace)?;
        std::fs::create_dir_all(external.join("nested"))?;
        let initialized = std::process::Command::new("git")
            .args(["init", "-q"])
            .arg(&external)
            .output()?;
        anyhow::ensure!(
            initialized.status.success(),
            "initialize approval fixture: {}",
            String::from_utf8_lossy(&initialized.stderr)
        );
        let settings_root = root.path().join("settings");
        let coordinator = PermissionCoordinator::transient(&workspace)?;
        coordinator.store().write().unwrap().set_rule_list(&[(
            CATEGORY_EDIT.into(),
            ".".into(),
            PermissionDecision::Ask,
        )])?;
        let request = crate::provider::ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "directory-session".into(),
            workspace: workspace.display().to_string(),
            input: crate::provider::ProviderInput::from_text("Read and write"),
            mode: crate::provider::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: crate::session::AccessPolicy {
                sandbox: false,
                read_access: crate::session::DirectoryScope::Workspace,
                ..Default::default()
            },
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let address = Some(TurnAddress {
            scope: InvocationScope {
                owner: request.owner()?,
                ..scope()
            },
            thread_id: "native".into(),
            turn_id: "turn".into(),
        });
        let directories = coordinator.directories(
            Some(&settings_root),
            &request,
            &address.as_ref().unwrap().scope,
        )?;
        let later_scope = crate::provider::InvocationScope::new(request.owner()?, "later-policy");
        let mut later_request = request.clone();
        later_request.access.write_access = crate::session::WriteScope::Off;
        let later_directories =
            coordinator.directories(Some(&settings_root), &later_request, &later_scope)?;
        assert!(coordinator.writes_disabled(Some(&later_scope))?);
        assert!(!coordinator.writes_disabled(Some(&address.as_ref().unwrap().scope))?);
        drop(later_directories);

        assert!(
            coordinator
                .directories(
                    Some(&settings_root),
                    &request,
                    &address.as_ref().unwrap().scope
                )
                .is_err(),
            "live ownership was replaced"
        );
        let mut permission = normalized_permission(
            "directory",
            vec![PermissionTarget::Read {
                path: external.join("read.txt").to_string_lossy().into_owned(),
            }],
        );
        let write = normalized_permission(
            "write",
            vec![PermissionTarget::Write {
                path: external
                    .join("nested/new/deep/write.txt")
                    .to_string_lossy()
                    .into_owned(),
            }],
        );
        permission.target_list.extend(write.target_list);
        let write = normalized_permission(
            "workspace-write",
            vec![PermissionTarget::Write {
                path: workspace
                    .join("new/deep/write.txt")
                    .to_string_lossy()
                    .into_owned(),
            }],
        );
        permission.target_list.extend(write.target_list);
        let original = permission.clone();
        let (sink, mut stream) =
            crate::provider::events::channel(address.as_ref().unwrap().scope.clone());
        let waiting_coordinator = coordinator.clone();
        let waiting_address = address.clone();
        let waiter = tokio::spawn(async move {
            waiting_coordinator
                .authorize(ApprovalMode::Ask, permission, waiting_address, Some(&sink))
                .await
        });
        let event = tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
            .await?
            .unwrap()
            .unwrap();
        let approval = approval(event);
        assert_eq!(approval.item_list.len(), 3);
        let answers = approval
            .item_list
            .iter()
            .map(|item| {
                if item.detail.starts_with(
                    &dunce::canonicalize(&workspace)
                        .unwrap()
                        .display()
                        .to_string(),
                ) {
                    let choice = item
                        .choice_list
                        .iter()
                        .find(|choice| choice.id == "allow_workspace_write")
                        .unwrap();
                    assert_eq!(
                        choice.rule_list,
                        vec![(
                            CATEGORY_EDIT.into(),
                            dunce::canonicalize(&workspace)
                                .unwrap()
                                .display()
                                .to_string()
                        )]
                    );
                    assert!(choice.directory.is_none());
                    assert!(choice.label.contains("this workspace"));
                    return ApprovalAnswer {
                        item_id: item.id.clone(),
                        choice_id: choice.id.clone(),
                    };
                }
                assert!(
                    item.choice_list
                        .iter()
                        .all(|choice| choice.id != "allow_workspace_write")
                );
                let choice = item
                    .choice_list
                    .iter()
                    .find(|choice| choice.id == "allow_global_directory")
                    .unwrap();
                assert_eq!(
                    choice.directory.as_ref().unwrap().path,
                    dunce::canonicalize(&external)
                        .unwrap()
                        .display()
                        .to_string()
                );
                if item.title == "Write path" {
                    let broad = item
                        .choice_list
                        .iter()
                        .find(|choice| choice.id == "allow_broad")
                        .unwrap();
                    assert_eq!(
                        broad.rule_list,
                        vec![(
                            CATEGORY_EDIT.into(),
                            choice.directory.as_ref().unwrap().path.clone()
                        )]
                    );
                }
                ApprovalAnswer {
                    item_id: item.id.clone(),
                    choice_id: choice.id.clone(),
                }
            })
            .collect::<Vec<_>>();
        coordinator.resolve(&approval.id, &answers, None).await?;
        let global = crate::settings::SettingsStore::open(&settings_root)?
            .layer_settings(crate::settings::SettingScope::Global, "", "")?
            .access
            .unwrap();
        assert_eq!(global.readable_directory.len(), 1);
        assert_eq!(global.writable_directory.len(), 1);
        assert_eq!(waiter.await??, ApprovalResolution::AllowOnce);
        let first = stream.recv().await.unwrap().unwrap();
        assert!(matches!(first, ProviderEvent::DirectoryAccess { grant, .. } if !grant.is_empty()));
        assert!(matches!(
            stream.recv().await.unwrap().unwrap(),
            ProviderEvent::Approval {
                update: ApprovalUpdate::Resolved(_),
                ..
            }
        ));
        assert_eq!(
            coordinator
                .authorize(ApprovalMode::Ask, original.clone(), address.clone(), None)
                .await?,
            ApprovalResolution::AllowOnce
        );
        let workspace_rule = dunce::canonicalize(&workspace)?.display().to_string();
        assert_eq!(
            coordinator
                .store()
                .read()
                .unwrap()
                .compiled()
                .document
                .permission[CATEGORY_EDIT][&workspace_rule],
            PermissionDecision::Allow
        );
        assert_eq!(
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    normalized_permission(
                        "saved-write",
                        vec![PermissionTarget::Write {
                            path: workspace
                                .join("another/file.txt")
                                .to_string_lossy()
                                .into_owned()
                        }]
                    ),
                    address.clone(),
                    None,
                )
                .await?,
            ApprovalResolution::AllowOnce
        );
        let protected = Path::new(&request.workspace)
            .join("protected.txt")
            .display()
            .to_string();
        coordinator.store().write().unwrap().set_rule_list(&[(
            CATEGORY_EDIT.into(),
            protected.clone(),
            PermissionDecision::Deny,
        )])?;
        assert_eq!(
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    normalized_permission(
                        "protected",
                        vec![PermissionTarget::Write {
                            path: protected.clone()
                        }]
                    ),
                    address.clone(),
                    None,
                )
                .await?,
            ApprovalResolution::DenyPolicy
        );
        drop(directories);
        assert!(coordinator.directory_by_scope.lock().unwrap().is_empty());
        let _directories = coordinator.directories(
            Some(&settings_root),
            &request,
            &address.as_ref().unwrap().scope,
        )?;
        assert_eq!(
            coordinator
                .authorize(ApprovalMode::Ask, original, address.clone(), None)
                .await?,
            ApprovalResolution::AllowOnce
        );
        let missing = normalized_permission(
            "missing",
            vec![PermissionTarget::UnknownFile {
                operation: forge_provider_api::access::DirectoryOperation::Read,
            }],
        );
        assert!(matches!(
            missing.target_list[0],
            PermissionTarget::UnknownFile { .. }
        ));
        assert_eq!(
            coordinator
                .store()
                .read()
                .unwrap()
                .compiled()
                .evaluate(&missing)?
                .decision,
            PermissionDecision::Ask
        );
        assert_eq!(
            approval_item(&missing.target_list[0], 0).choice_list.len(),
            3
        );
        let path = external.join("read.txt");
        coordinator.store().write().unwrap().set_rule_list(&[(
            CATEGORY_READ.into(),
            dunce::canonicalize(&external)?
                .join("read.txt")
                .display()
                .to_string(),
            PermissionDecision::Deny,
        )])?;
        let denied = normalized_permission(
            "denied",
            vec![PermissionTarget::Read {
                path: path.to_string_lossy().into_owned(),
            }],
        );
        assert_eq!(
            coordinator
                .authorize(ApprovalMode::Allow, denied, address, None)
                .await?,
            ApprovalResolution::DenyPolicy
        );
        let invalid = PermissionRequest {
            id: "invalid".into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: None,
            target_list: vec![PermissionTarget::Read {
                path: String::new(),
            }],
        };
        let address = Some(TurnAddress {
            scope: InvocationScope {
                owner: request.owner()?,
                ..scope()
            },
            thread_id: "native".into(),
            turn_id: "turn".into(),
        });
        let (sink, mut stream) =
            crate::provider::events::channel(address.as_ref().unwrap().scope.clone());
        let failure = coordinator
            .authorize(ApprovalMode::Ask, invalid, address, Some(&sink))
            .await
            .unwrap_err();
        assert!(failure.to_string().contains("empty"));
        let event = stream.recv().await.unwrap().unwrap();
        assert!(matches!(
            event,
            ProviderEvent::Diagnostic { diagnostic, .. } if diagnostic.kind == forge_provider_api::ProviderFailureKind::Authorization && diagnostic.message.as_str() == "Permission evaluation failed"
        ));
        Ok(())
    }

    /// Bind and revoke declared skill authority without requiring a second global provider registration.
    #[test]
    fn skill_scope_uses_supplied_descriptor_without_global_relookup() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let coordinator = PermissionCoordinator::transient(directory.path())?;
        let id =
            forge_providers::registry::nonselectable_id().expect("fixture provider is registered");
        let mut descriptor =
            forge_providers::registry::descriptor(&id).expect("fixture metadata exists");
        descriptor.kind = crate::provider::ProviderId::new("unregistered-skill-fixture")?;
        assert!(forge_providers::registry::descriptor(&descriptor.kind).is_err());
        let request = crate::provider::ProviderRequest {
            dispatch: Default::default(),
            execution_owner: None,
            harness_session_id: "descriptor-skill-session".into(),
            workspace: directory.path().to_string_lossy().into_owned(),
            input: crate::provider::ProviderInput::from_text("Read declared skills"),
            mode: crate::provider::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let owner = request.owner()?;
        let skills = coordinator.skills(None, &request, &descriptor)?;
        assert!(
            coordinator
                .skill_by_owner
                .lock()
                .unwrap()
                .contains_key(&owner)
        );
        drop(skills);
        assert!(
            !coordinator
                .skill_by_owner
                .lock()
                .unwrap()
                .contains_key(&owner)
        );
        Ok(())
    }

    #[tokio::test]
    async fn skill_runtime_grants_preserve_denials_selection_compounds_and_revocation() -> Result<()>
    {
        use super::*;
        let directory = tempfile::tempdir()?;
        let bundle = directory.path().join(".rulesync/skills/fixture");
        std::fs::create_dir_all(bundle.join("scripts"))?;
        std::fs::write(
            bundle.join("SKILL.md"),
            "---\nname: fixture\ntargets: ['*']\n---\nInstructions",
        )?;
        std::fs::write(bundle.join("scripts/check.py"), "print('ok')")?;
        let permission_path = directory.path().join("permissions.json");
        std::fs::write(
            &permission_path,
            r#"{"permission":{"read":{"*":"ask"},"bash":{"*":"ask"}}}"#,
        )?;
        let store = Arc::new(RwLock::new(PermissionStore::load(
            &permission_path,
            directory.path(),
        )?));
        let coordinator = Arc::new(PermissionCoordinator::new(store.clone()));
        let request = crate::provider::ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: directory.path().display().to_string(),
            input: crate::provider::ProviderInput::from_text("Read skill"),
            mode: crate::provider::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: crate::session::AccessPolicy::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let address = Some(TurnAddress {
            scope: InvocationScope {
                owner: request.owner()?,
                ..scope()
            },
            thread_id: "provider".into(),
            turn_id: "turn".into(),
        });
        let skills = coordinator.skills(
            Some(directory.path()),
            &request,
            &forge_providers::registry::descriptor(&forge_providers::registry::default_id())
                .expect("default provider is registered"),
        )?;
        let original = store.read().unwrap().open().1;
        let source = bundle.join("SKILL.md").display().to_string();
        let script = bundle.join("scripts/check.py").display().to_string();
        let operation = |target| PermissionRequest {
            id: "request".into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: None,
            target_list: vec![target],
        };
        let command = |command| PermissionTarget::Command {
            command,
            shell: Some(CommandShell::PowerShell),
        };
        for target in [
            PermissionTarget::Read {
                path: source.clone(),
            },
            PermissionTarget::Read {
                path: bundle.display().to_string(),
            },
            command(format!("Get-Content -LiteralPath '{source}'")),
            command(format!("python '{script}' --help")),
        ] {
            let granted = tokio::time::timeout(
                std::time::Duration::from_secs(1),
                coordinator.authorize(ApprovalMode::Ask, operation(target), address.clone(), None),
            )
            .await??;
            assert_eq!(granted, ApprovalResolution::AllowOnce);
        }
        assert_eq!(store.read().unwrap().open().1, original);
        for target in [
            PermissionTarget::Write {
                path: source.clone(),
            },
            PermissionTarget::Read {
                path: directory.path().display().to_string(),
            },
            command(format!("python '{script}'; Write-Output unrelated")),
            command(format!("Get-Content '{source}' > outside.txt")),
            command(format!("python -c \"print('{script}')\"")),
        ] {
            let result = tokio::time::timeout(
                std::time::Duration::from_millis(20),
                coordinator.authorize(
                    ApprovalMode::Ask,
                    operation(target.clone()),
                    address.clone(),
                    None,
                ),
            )
            .await;
            assert!(
                !matches!(
                    result,
                    Ok(Ok(ApprovalResolution::AllowOnce
                        | ApprovalResolution::AllowExact
                        | ApprovalResolution::AllowBroad))
                ),
                "unexpected skill grant for {target:?}: {result:?}"
            );
            assert!(coordinator.pending_list()?.is_empty());
        }
        store.write().unwrap().set_rule_list(&[(
            CATEGORY_BASH.into(),
            "python *".into(),
            PermissionDecision::Deny,
        )])?;
        assert_eq!(
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    operation(command(format!("python '{script}'"))),
                    address.clone(),
                    None
                )
                .await?,
            ApprovalResolution::DenyPolicy
        );
        store.write().unwrap().set_rule_list(&[(
            CATEGORY_READ.into(),
            source.clone(),
            PermissionDecision::Deny,
        )])?;
        assert_eq!(
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    operation(command(format!("Get-Content '{source}'"))),
                    address.clone(),
                    None
                )
                .await?,
            ApprovalResolution::DenyPolicy
        );
        let alias = directory.path().join("profile-alias");
        #[cfg(windows)]
        junction::create(&bundle, &alias)?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(&bundle, &alias)?;
        store.write().unwrap().set_rule_list(&[(
            CATEGORY_READ.into(),
            "*".into(),
            PermissionDecision::Allow,
        )])?;
        assert_eq!(
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    operation(PermissionTarget::Read {
                        path: alias.join("SKILL.md").display().to_string()
                    }),
                    address.clone(),
                    None
                )
                .await?,
            ApprovalResolution::DenyPolicy
        );
        #[cfg(windows)]
        junction::delete(&alias)?;
        #[cfg(unix)]
        std::fs::remove_file(&alias)?;
        drop(skills);
        assert!(coordinator.skill_by_owner.lock().unwrap().is_empty());
        store.write().unwrap().save(&original)?;
        assert!(
            tokio::time::timeout(
                std::time::Duration::from_millis(20),
                coordinator.authorize(
                    ApprovalMode::Ask,
                    operation(PermissionTarget::Read {
                        path: source.clone()
                    }),
                    address.clone(),
                    None
                )
            )
            .await
            .is_err()
        );
        let mut settings = crate::settings::SettingsStore::open(directory.path())?;
        let catalog = settings.catalog(
            &request.workspace,
            &request.harness_session_id,
            forge_providers::registry::default_id().as_str(),
        )?;
        let mut desired = crate::settings::PortableSettings::default();
        desired.skill.insert(catalog.skill[0].id.clone(), false);
        settings.update(
            0,
            crate::settings::SettingScope::Global,
            &request.workspace,
            &request.harness_session_id,
            &desired,
        )?;
        let disabled = coordinator.skills(
            Some(directory.path()),
            &request,
            &forge_providers::registry::descriptor(&forge_providers::registry::default_id())
                .expect("default provider is registered"),
        )?;
        assert!(
            tokio::time::timeout(
                std::time::Duration::from_millis(20),
                coordinator.authorize(
                    ApprovalMode::Ask,
                    operation(PermissionTarget::Read { path: source }),
                    address,
                    None
                )
            )
            .await
            .is_err()
        );
        drop(disabled);
        Ok(())
    }

    #[tokio::test]
    async fn shell_parse_errors_never_open_approval_or_change_permission_rules() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("permissions.json");
        std::fs::write(&path, r#"{"permission":{"bash":{"*":"allow"}}}"#).unwrap();
        let before = std::fs::read(&path).unwrap();
        let store = PermissionStore::load(&path, directory.path()).unwrap();
        let coordinator = PermissionCoordinator::new(Arc::new(RwLock::new(store)));
        let (sink, mut stream) = crate::provider::events::channel(scope());
        for mode in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            for _callback in 0..1 {
                let request = PermissionRequest {
                    id: "parse-error".into(),
                    provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Command {
                        command: "probe 'unfinished".into(),
                        shell: Some(crate::permissions::shell::CommandShell::PowerShell),
                    }],
                };
                let failure = tokio::time::timeout(
                    std::time::Duration::from_secs(1),
                    coordinator.authorize(mode, request, None, Some(&sink)),
                )
                .await
                .unwrap()
                .unwrap_err();
                assert!(
                    failure
                        .downcast_ref::<crate::permissions::matcher::CommandAnalysisError>()
                        .is_some()
                );
                assert!(failure.to_string().contains("shell_command_parse_error"));
                assert!(failure.to_string().contains("split independent operations"));
                assert!(coordinator.pending_list().unwrap().is_empty());
                let event = stream.try_recv().unwrap();
                assert!(matches!(
                    event,
                    ProviderEvent::Diagnostic { diagnostic, .. }
                        if diagnostic.message.as_str().contains("shell_command_parse_error")
                ));
                assert!(stream.try_recv().is_err());
            }
            let corrected = PermissionRequest {
                id: "parse-error".into(),
                provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: "probe corrected".into(),
                    shell: Some(crate::permissions::shell::CommandShell::PowerShell),
                }],
            };
            assert_eq!(
                coordinator
                    .authorize(mode, corrected, None, Some(&sink))
                    .await
                    .unwrap(),
                ApprovalResolution::AllowOnce
            );
        }
        assert_eq!(std::fs::read(path).unwrap(), before);
    }

    #[tokio::test]
    async fn input_timeout_rejects_once_without_rules_and_preserves_other_owners() {
        use super::*;
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), directory.path())
                .unwrap(),
        ));
        let before = std::fs::read(directory.path().join("permissions.json")).unwrap();
        let coordinator = Arc::new(PermissionCoordinator::new(store));
        let expired_owner = crate::provider::ExecutionOwner::root("expired");
        let mut expired_feedback = coordinator
            .subscribe_timeout_feedback(&expired_owner)
            .unwrap();
        let waiting_feedback = coordinator
            .subscribe_timeout_feedback(&crate::provider::ExecutionOwner::root("waiting"))
            .unwrap();
        coordinator
            .configure_timeout(
                "expired",
                InputTimeout {
                    amount: 1,
                    unit: crate::session::TimeoutUnit::S,
                },
            )
            .unwrap();
        coordinator
            .configure_timeout(
                "waiting",
                InputTimeout {
                    amount: 0,
                    unit: crate::session::TimeoutUnit::M,
                },
            )
            .unwrap();
        let mut tasks = Vec::new();
        let mut streams = Vec::new();
        for session in ["expired", "waiting"] {
            let owner = crate::provider::ExecutionOwner::root(session);
            let coordinator = coordinator.clone();
            let (sink, stream) =
                crate::provider::events::channel(InvocationScope { owner, ..scope() });
            streams.push(stream);
            tasks.push(tokio::spawn(async move {
                coordinator
                    .authorize(
                        ApprovalMode::Ask,
                        PermissionRequest {
                            id: "same-id".into(),
                            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                            reason: None,
                            target_list: vec![PermissionTarget::Command {
                                command: "echo timeout".into(),
                                shell: None,
                            }],
                        },
                        Some(TurnAddress {
                            scope: sink.invocation_scope().unwrap().clone(),
                            thread_id: session.into(),
                            turn_id: "turn".into(),
                        }),
                        Some(&sink),
                    )
                    .await
                    .unwrap()
            }));
        }
        for stream in &mut streams {
            approval(stream.recv().await.unwrap().unwrap());
        }
        let pending = coordinator.pending_list().unwrap();
        let original = pending
            .iter()
            .find(|view| {
                view.execution_owner.as_ref().unwrap().session_id
                    == forge_provider_api::SessionId::from("expired")
            })
            .unwrap();
        let original_deadline = original.deadline.as_ref().unwrap().expires_at_ms;
        coordinator
            .configure_timeout(
                "expired",
                InputTimeout {
                    amount: 5,
                    unit: crate::session::TimeoutUnit::H,
                },
            )
            .unwrap();
        assert_eq!(
            coordinator
                .pending_list()
                .unwrap()
                .iter()
                .find(|view| view.id == original.id)
                .unwrap()
                .deadline
                .as_ref()
                .unwrap()
                .expires_at_ms,
            original_deadline
        );
        let expired = tokio::time::timeout(std::time::Duration::from_secs(3), streams[0].recv())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        assert!(matches!(
            expired,
            ProviderEvent::Approval {
                update: ApprovalUpdate::Resolved(view),
                ..
            } if view.resolution_source == ResolutionSource::Timeout
        ));
        assert!(expired_feedback.has_changed().unwrap());
        assert!(*expired_feedback.borrow_and_update());
        assert!(!waiting_feedback.has_changed().unwrap());
        assert_eq!(tasks.remove(0).await.unwrap(), ApprovalResolution::TimedOut);
        let pending = coordinator.pending_list().unwrap();
        assert_eq!(pending.len(), 1);
        assert!(pending[0].deadline.is_none());
        assert!(
            coordinator
                .has_timeout_feedback(&crate::provider::ExecutionOwner::root("expired"))
                .unwrap()
        );
        assert!(
            !coordinator
                .has_timeout_feedback(&crate::provider::ExecutionOwner::root("waiting"))
                .unwrap()
        );
        assert_eq!(
            std::fs::read(directory.path().join("permissions.json")).unwrap(),
            before
        );
        coordinator
            .cancel_owner(&crate::provider::ExecutionOwner::root("waiting"), None)
            .await
            .unwrap();
        assert_eq!(tasks.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(coordinator.pending_list().unwrap().is_empty());
        assert!(coordinator.resolve(&original.id, &[], None).await.is_err());
        let late_feedback = coordinator
            .subscribe_timeout_feedback(&expired_owner)
            .unwrap();
        assert!(*late_feedback.borrow());
        assert!(
            coordinator
                .take_timeout_feedback(&expired_owner)
                .unwrap()
                .is_some()
        );
        assert!(expired_feedback.has_changed().unwrap());
        assert!(!*expired_feedback.borrow_and_update());
        assert!(!*late_feedback.borrow());
    }

    #[tokio::test]
    async fn input_timeout_registration_is_removed_when_provider_wait_is_dropped() {
        let coordinator = PermissionCoordinator::transient(".").unwrap();
        let (sink, mut stream) = crate::provider::events::channel(scope());
        let provider = coordinator.clone();
        let waiter = tokio::spawn(async move {
            provider
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "abandoned".into(),
                        provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "probe".into(),
                            shell: None,
                        }],
                    },
                    None,
                    Some(&sink),
                )
                .await
        });
        stream.recv().await.unwrap().unwrap();
        assert_eq!(coordinator.pending_list().unwrap().len(), 1);
        waiter.abort();
        assert!(waiter.await.unwrap_err().is_cancelled());
        assert!(coordinator.pending_list().unwrap().is_empty());
    }

    use super::*;
    use crate::permissions::store::PermissionStore;

    #[tokio::test]
    async fn cancelling_one_owned_child_preserves_sibling_approval_with_the_same_provider_id() {
        let coordinator = PermissionCoordinator::transient(".").unwrap();
        let mut waiter = Vec::new();
        let mut channels = Vec::new();
        for agent_id in ["first", "second"] {
            let coordinator = coordinator.clone();
            let owner = crate::provider::ExecutionOwner {
                session_id: "root".into(),
                agent_id: agent_id.into(),
            };
            let (sink, mut stream) =
                crate::provider::events::channel(InvocationScope { owner, ..scope() });
            let waiting_sink = sink.clone();
            waiter.push(tokio::spawn(async move {
                coordinator
                    .authorize(
                        ApprovalMode::Ask,
                        PermissionRequest {
                            id: "same-provider-id".into(),
                            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                            reason: None,
                            target_list: vec![PermissionTarget::Command {
                                command: "probe task".into(),
                                shell: None,
                            }],
                        },
                        Some(TurnAddress {
                            scope: waiting_sink.invocation_scope().unwrap().clone(),
                            thread_id: "thread".into(),
                            turn_id: "turn".into(),
                        }),
                        Some(&waiting_sink),
                    )
                    .await
                    .unwrap()
            }));
            approval(stream.recv().await.unwrap().unwrap());
            channels.push((sink, stream));
        }
        assert_eq!(coordinator.pending_list().unwrap().len(), 2);
        coordinator
            .cancel_owner(
                &crate::provider::ExecutionOwner {
                    session_id: "root".into(),
                    agent_id: "first".into(),
                },
                None,
            )
            .await
            .unwrap();
        assert!(matches!(
            channels[0].1.recv().await.unwrap().unwrap(),
            ProviderEvent::Approval {
                update: ApprovalUpdate::Cancelled(_),
                ..
            }
        ));
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(!waiter[0].is_finished());
        let pending = coordinator.pending_list().unwrap();
        assert_eq!(pending.len(), 1);
        assert_eq!(
            pending[0].execution_owner.as_ref().unwrap().agent_id,
            forge_provider_api::AgentId::from("second")
        );
        coordinator
            .cancel_owner(
                &crate::provider::ExecutionOwner {
                    session_id: "root".into(),
                    agent_id: "second".into(),
                },
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(matches!(
            channels[1].1.recv().await.unwrap().unwrap(),
            ProviderEvent::Approval {
                update: ApprovalUpdate::Cancelled(_),
                ..
            }
        ));
    }

    fn evaluate_review(request: &PermissionRequest) -> PermissionEvaluation {
        let document = crate::permissions::document::parse_permission_document(
            r#"{"permission":{"bash":{"*":"allow","probe *":"ask","git *":"ask","whoami *":"ask"}}}"#,
        )
        .unwrap();
        crate::permissions::matcher::CompiledPermissionDocument::compile(document, ".")
            .evaluate(request)
            .unwrap()
    }

    #[test]
    fn builds_exact_and_broad_command_choices() {
        let request = PermissionRequest {
            id: "one".into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: "git status --short".into(),
                shell: None,
            }],
        };
        let view = approval_view(&request, &evaluate_review(&request));
        assert!(
            view.item_list[0].choice_list[1]
                .rule_list
                .contains(&("bash".into(), "git status --short".into()))
        );
        assert!(
            view.item_list[0].choice_list[2]
                .rule_list
                .contains(&("bash".into(), "git *".into()))
        );
    }

    #[tokio::test]
    async fn reviews_only_unapproved_commands_and_persists_each_selected_scope() {
        let directory = tempfile::tempdir().unwrap();
        let mut permission_store =
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap();
        permission_store
            .set_rule_list(&[
                (
                    "bash".into(),
                    "Get-Location".into(),
                    PermissionDecision::Allow,
                ),
                (
                    "bash".into(),
                    "Get-ChildItem *".into(),
                    PermissionDecision::Allow,
                ),
            ])
            .unwrap();
        let original = permission_store.open().1;
        let store = Arc::new(RwLock::new(permission_store));
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator.authorize(ApprovalMode::Ask, PermissionRequest {
                id: "versions".into(), provider: forge_provider_api::ProviderId::new("mock").unwrap(), reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: r#""C:/Program Files/PowerShell/7/pwsh.exe" -Command 'Get-Location; Get-ChildItem -Force; rustc --version; cargo --version; rustup show active-toolchain; rustc --version'"#.into(), shell: None
                }],
            }, None, Some(&event_sink)).await.unwrap()
        });
        let event = event_stream.recv().await.unwrap().unwrap();
        let view = approval(event);
        assert_eq!(
            view.item_list
                .iter()
                .map(|item| item.detail.as_str())
                .collect::<Vec<_>>(),
            vec![
                "rustc --version",
                "cargo --version",
                "rustup show active-toolchain"
            ]
        );
        assert_eq!(
            view.item_list[0].choice_list[1].label,
            "Always allow exact: shell rustc --version"
        );
        assert_eq!(
            view.item_list[0].choice_list[2].rule_list,
            vec![("bash".into(), "rustc *".into())]
        );
        assert!(!waiting.is_finished());
        let answer_list = vec![
            ApprovalAnswer {
                item_id: "0".into(),
                choice_id: "allow_exact".into(),
            },
            ApprovalAnswer {
                item_id: "1".into(),
                choice_id: "deny_exact".into(),
            },
            ApprovalAnswer {
                item_id: "2".into(),
                choice_id: "allow_once".into(),
            },
        ];
        assert!(
            coordinator
                .resolve(&view.id, &answer_list[..1], None)
                .await
                .is_err()
        );
        let mut duplicate = answer_list.clone();
        duplicate[2].item_id = "1".into();
        assert!(
            coordinator
                .resolve(&view.id, &duplicate, None)
                .await
                .is_err()
        );
        let mut unknown = answer_list.clone();
        unknown[1].choice_id = "allow_everything".into();
        assert!(coordinator.resolve(&view.id, &unknown, None).await.is_err());
        assert_eq!(store.read().unwrap().open().1, original);
        assert_eq!(coordinator.pending_list().unwrap().len(), 1);
        coordinator
            .resolve(&view.id, &answer_list, None)
            .await
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::DenyOnce);
        let rules = store.read().unwrap().compiled();
        let command_rules = &rules.document.permission["bash"];
        assert_eq!(command_rules["rustc --version"], PermissionDecision::Allow);
        assert_eq!(command_rules["cargo --version"], PermissionDecision::Deny);
        assert_eq!(command_rules["Get-Location"], PermissionDecision::Allow);
        assert_eq!(command_rules["Get-ChildItem *"], PermissionDecision::Allow);
        assert!(!command_rules.contains_key("rustup show active-toolchain"));
        assert!(!command_rules.contains_key("rustc *"));
    }

    #[test]
    fn ambiguous_commands_never_offer_partial_persistent_rules() {
        let item = approval_item(
            &PermissionTarget::Command {
                command: "cargo --version; & $secret".into(),
                shell: None,
            },
            0,
        );
        assert_eq!(item.detail, "cargo --version; & $secret");
        assert_eq!(
            item.choice_list
                .iter()
                .map(|choice| choice.id.as_str())
                .collect::<Vec<_>>(),
            vec!["allow_once", "deny_once", "cancel"]
        );
    }

    #[test]
    fn permission_context_shows_only_the_script_and_marks_the_loop_command() {
        let request = PermissionRequest { id:"loop".into(),provider:forge_provider_api::ProviderId::new("mock").unwrap(),reason:None,
            target_list:vec![PermissionTarget::Command {
                command:r#""C:\Program Files\PowerShell\7\pwsh.exe" -Command 'foreach ($iteration in 1..1) { whoami.exe /groups }'"#.into(),
                shell:Some(CommandShell::PowerShell),
            }] };
        let view = approval_view(&request, &evaluate_review(&request));
        assert_eq!(view.item_list.len(), 1);
        let item = &view.item_list[0];
        assert_eq!(item.detail, "whoami.exe /groups");
        let command = &item.command_list[0];
        assert_eq!(
            command.source,
            "foreach ($iteration in 1..1) { whoami.exe /groups }"
        );
        assert_eq!(
            &command.source[command.focus_range_list[0].clone()],
            "whoami.exe /groups"
        );
        assert!(
            item.choice_list
                .iter()
                .all(|choice| !choice.label.contains("1..1"))
        );
    }

    #[test]
    fn normalized_command_permissions_retain_shell_identity_and_script_boundaries() {
        for (script, shell) in [
            ("1 | ForEach-Object { probe $_ }", CommandShell::PowerShell),
            ("for x in one; do probe $x; done", CommandShell::Bash),
            ("repeat 2 probe x", CommandShell::Zsh),
            ("[1] | each {|x| probe $x }", CommandShell::Nushell),
        ] {
            let request = normalized_permission(
                "id",
                vec![PermissionTarget::Command {
                    command: script.into(),
                    shell: Some(shell),
                }],
            );
            assert_eq!(
                request.target_list,
                vec![PermissionTarget::Command {
                    command: script.into(),
                    shell: Some(shell),
                }]
            );
            assert!(normalize_command_in(script, shell).failure.is_none());
        }
    }

    #[test]
    fn normalized_shell_script_permission_matches_host_rules() {
        let source = r#"rg -n "timeline|markdown|table|wrap" C:\Users\fixture\notes\REFERENCE.md"#;
        let request = normalized_permission(
            "id",
            vec![PermissionTarget::Command {
                command: source.into(),
                shell: Some(CommandShell::PowerShell),
            }],
        );
        let parsed = normalize_command_in(source, CommandShell::PowerShell);
        assert!(parsed.failure.is_none());
        assert_eq!(parsed.invocation_list.len(), 1);
        assert_eq!(
            parsed.invocation_list[0].token_list,
            [
                "rg",
                "-n",
                "timeline|markdown|table|wrap",
                r"C:\Users\fixture\notes\REFERENCE.md",
            ]
        );
        let rules = json!({"permission":{"bash":{"*":"ask", "rg *":"allow"}}});
        let compiled = crate::permissions::matcher::CompiledPermissionDocument::compile(
            crate::permissions::document::parse_permission_document(&rules.to_string()).unwrap(),
            ".",
        );
        let evaluation = compiled.evaluate(&request).unwrap();
        assert_eq!(evaluation.decision, PermissionDecision::Allow);
        assert!(evaluation.pending_target_list.is_empty());
    }

    #[tokio::test]
    async fn allow_callback_does_not_publish_or_persist_an_approval() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let coordinator = PermissionCoordinator::transient(directory.path())?;
        let before = coordinator.store.read().unwrap().open().1;
        let (sink, mut stream) = crate::provider::events::channel(scope());
        let resolution = coordinator
            .authorize(
                ApprovalMode::Allow,
                PermissionRequest {
                    id: "callback".into(),
                    provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Write {
                        path: "review.txt".into(),
                    }],
                },
                None,
                Some(&sink),
            )
            .await?;
        assert_eq!(resolution, ApprovalResolution::AllowOnce);
        assert!(coordinator.pending_list()?.is_empty());
        assert!(
            tokio::time::timeout(std::time::Duration::from_millis(10), stream.recv())
                .await
                .is_err()
        );
        assert_eq!(coordinator.store.read().unwrap().open().1, before);
        Ok(())
    }

    #[tokio::test]
    async fn cancellation_does_not_persist_staged_choices() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap(),
        ));
        let original = store.read().unwrap().open().1;
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "cancel-versions".into(),
                        provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "rustc --version; cargo --version".into(),
                            shell: None,
                        }],
                    },
                    None,
                    Some(&event_sink),
                )
                .await
                .unwrap()
        });
        let view = approval(event_stream.recv().await.unwrap().unwrap());
        coordinator
            .resolve(
                &view.id,
                &[
                    ApprovalAnswer {
                        item_id: "0".into(),
                        choice_id: "allow_exact".into(),
                    },
                    ApprovalAnswer {
                        item_id: "1".into(),
                        choice_id: "cancel".into(),
                    },
                ],
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::Cancel);
        assert_eq!(store.read().unwrap().open().1, original);
    }

    #[tokio::test]
    async fn pauses_asked_permissions_and_persists_the_selected_rule() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap(),
        ));
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let request = PermissionRequest {
            id: "approval-one".into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: Some("inspect repository".into()),
            target_list: vec![PermissionTarget::Command {
                command: "rg TODO src".into(),
                shell: None,
            }],
        };
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(ApprovalMode::Ask, request, None, Some(&waiting_event_sink))
                .await
                .unwrap()
        });
        let event = event_stream.recv().await.unwrap().unwrap();
        let view = approval(event);
        let owned = coordinator
            .bind_owner(&view.id, "child-exchange", Some("child-turn"))
            .unwrap()
            .unwrap();
        assert_eq!(owned.exchange_id.as_deref(), Some("child-exchange"));
        let snapshot = coordinator.pending_list().unwrap();
        assert_eq!(snapshot[0].exchange_id, owned.exchange_id);
        assert_eq!(
            snapshot[0].turn_id,
            Some(forge_provider_api::ProviderTurnId::from("child-turn"))
        );
        assert!(
            coordinator
                .bind_owner(&view.id, "other-exchange", Some("other-turn"))
                .is_err()
        );
        coordinator
            .resolve(
                &view.id,
                &[ApprovalAnswer {
                    item_id: "0".into(),
                    choice_id: "allow_broad".into(),
                }],
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::AllowOnce);
        let resolved = tokio::time::timeout(std::time::Duration::from_secs(1), event_stream.recv())
            .await
            .expect("manual resolution must reach the requesting execution")
            .unwrap()
            .unwrap();
        let ProviderEvent::Approval {
            update: ApprovalUpdate::Resolved(resolved_view),
            ..
        } = resolved
        else {
            panic!("expected resolved approval");
        };
        assert_eq!(resolved_view.exchange_id, owned.exchange_id);
        assert_eq!(resolved_view.turn_id, owned.turn_id);
        let source = store.read().unwrap().open().1;
        assert!(source.contains("\"rg *\": \"allow\""));
    }

    #[tokio::test]
    async fn persists_an_always_deny_choice_in_the_shared_document() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap(),
        ));
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "approval-deny".into(),
                        provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "rg TODO src".into(),
                            shell: None,
                        }],
                    },
                    None,
                    Some(&waiting_event_sink),
                )
                .await
                .unwrap()
        });
        let view = approval(event_stream.recv().await.unwrap().unwrap());
        coordinator
            .resolve(
                &view.id,
                &[ApprovalAnswer {
                    item_id: "0".into(),
                    choice_id: "deny_broad".into(),
                }],
                Some(&event_sink),
            )
            .await
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::DenyOnce);
        assert_eq!(
            store.read().unwrap().compiled().document.permission["bash"]["rg *"],
            PermissionDecision::Deny
        );
    }

    #[tokio::test]
    async fn cancellation_clears_every_pending_approval_and_emits_lifecycle_events() {
        let coordinator = PermissionCoordinator::transient("D:/repo").unwrap();
        let (event_sink, mut event_stream) = crate::provider::events::channel(scope());
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "approval-cancel".into(),
                        provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "rg TODO".into(),
                            shell: None,
                        }],
                    },
                    None,
                    Some(&waiting_event_sink),
                )
                .await
                .unwrap()
        });
        approval(event_stream.recv().await.unwrap().unwrap());
        coordinator.cancel_all(None).await.unwrap();
        assert!(matches!(
            event_stream.recv().await.unwrap().unwrap(),
            ProviderEvent::Approval {
                update: ApprovalUpdate::Cancelled(_),
                ..
            }
        ));
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::Cancel);
        assert!(coordinator.pending_list().unwrap().is_empty());
    }
    #[test]
    fn command_review_preserves_full_context_and_only_marks_executable_occurrences() {
        for (shell, source) in [
            (
                CommandShell::PowerShell,
                "Write-Output 'probe x'; ForEach-Object { probe x; probe x }",
            ),
            (
                CommandShell::Bash,
                "printf '%s' 'probe x'; for entry in one; do probe x; probe x; done",
            ),
            (
                CommandShell::Zsh,
                "print 'probe x'; repeat 2 { probe x; probe x; }",
            ),
            (
                CommandShell::Nushell,
                "print 'probe x'; [1] | each {|entry| probe x; probe x }",
            ),
        ] {
            let request = PermissionRequest {
                id: "context".into(),
                provider: forge_provider_api::ProviderId::new("mock").unwrap(),
                reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: source.into(),
                    shell: Some(shell),
                }],
            };
            let view = approval_view(&request, &evaluate_review(&request));
            let item = &view.item_list[0];
            assert_eq!(item.detail, "probe x");
            assert_eq!(item.command_list.len(), 1, "{shell:?}");
            let context = &item.command_list[0];
            assert_eq!(context.source, source);
            assert_eq!(context.focus_range_list.len(), 2, "{shell:?}");
            for range in &context.focus_range_list {
                assert_eq!(&source[range.clone()], "probe x");
                assert_ne!(
                    range.start,
                    source.find("probe x").unwrap(),
                    "quoted data cannot become an approval marker"
                );
            }
            assert!(
                context
                    .highlight_list
                    .iter()
                    .any(|capture| capture.group == "ForgeHarnessCommand")
            );
            let exact = item
                .choice_list
                .iter()
                .find(|choice| choice.id == "allow_exact")
                .unwrap();
            assert_eq!(exact.rule_list, vec![("bash".into(), "probe x".into())]);
        }
    }

    #[test]
    fn normalized_spaced_powershell_permission_uses_host_rules() {
        let source = "Get-Content Cargo.toml, .forge.json, .gitignore";
        let request = normalized_permission(
            "spaced-array",
            vec![PermissionTarget::Command {
                command: source.into(),
                shell: Some(CommandShell::PowerShell),
            }],
        );
        assert_eq!(
            request.target_list,
            vec![PermissionTarget::Command {
                command: source.into(),
                shell: Some(CommandShell::PowerShell),
            }]
        );
        for decision in ["allow", "ask", "deny"] {
            let rules = json!({"permission":{"bash":{"*":"allow","Get-Content *":decision}}});
            let compiled = crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::parse_permission_document(&rules.to_string())
                    .unwrap(),
                ".",
            );
            let evaluation = compiled.evaluate(&request).unwrap();
            if decision == "ask" {
                let view = approval_view(&request, &evaluation);
                assert_eq!(view.item_list.len(), 1);
                assert_eq!(view.item_list[0].command_list[0].source, source);
                assert_eq!(
                    view.item_list[0].command_list[0].focus_range_list,
                    vec![0..source.len()]
                );
            }
            assert_eq!(
                evaluation.decision,
                match decision {
                    "allow" => PermissionDecision::Allow,
                    "ask" => PermissionDecision::Ask,
                    _ => PermissionDecision::Deny,
                }
            );
        }
    }

    #[test]
    fn comma_argument_review_marks_each_command_in_the_original_script() {
        let source = "probe one,two | probe three,four; probe five,six";
        let target = PermissionTarget::Command {
            command: source.into(),
            shell: Some(CommandShell::PowerShell),
        };
        let request = PermissionRequest {
            id: "comma-arguments".into(),
            provider: forge_provider_api::ProviderId::new("mock").unwrap(),
            reason: None,
            target_list: vec![target.clone()],
        };
        let view = approval_view(&request, &evaluate_review(&request));
        assert_eq!(view.item_list.len(), 3);
        for (item, command) in
            view.item_list
                .iter()
                .zip(["probe one,two", "probe three,four", "probe five,six"])
        {
            let start = source.find(command).unwrap();
            assert_eq!(item.command_list[0].source, source);
            assert_eq!(
                item.command_list[0].focus_range_list,
                vec![start..start + command.len()]
            );
            assert!(
                item.choice_list
                    .iter()
                    .any(|choice| !choice.rule_list.is_empty())
            );
        }
    }
}
