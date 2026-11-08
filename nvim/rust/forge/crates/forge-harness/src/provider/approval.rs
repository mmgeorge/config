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
use crate::provider::ScopeOwner;
use crate::provider::events::{HarnessEventSink, HostEvent};
use crate::provider::host_event::*;
use crate::session::{ApprovalMode, InputTimeout, ResolutionSource};
use anyhow::{Context, Result};
use forge_provider_api::SanitizedMessage;
use std::collections::{BTreeMap, HashMap};
use std::path::Path;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::oneshot;
use uuid::Uuid;

/// Retain the original execution's publication, policy, and revocation authority while approval waits.
struct ApprovalExecution<'scope> {
    address: Option<crate::provider::TurnAddress>,
    event_sink: Option<&'scope HarnessEventSink>,
    directory_policy: Option<Arc<DirectoryPolicy>>,
    authority: Option<Arc<ApprovalAuthority>>,
    kind: ApprovalExecutionKind,
}

struct PendingApproval {
    token: Uuid,
    view: ApprovalRequestView,
    presentation: PermissionApprovalRequest,
    execution_kind: ApprovalExecutionKind,
    response: oneshot::Sender<ApprovalResolution>,
    resolution_sink: Option<HarnessEventSink>,
    directory_policy: Option<Arc<DirectoryPolicy>>,
    authority: Option<Arc<ApprovalAuthority>>,
    /// Takes the same expiry branch as the deadline timer; see `expire_pending_for_owner`.
    expiry_trigger: Arc<tokio::sync::Notify>,
}

/// Capture immutable directory and selected-skill authority for independently retained native children.
#[derive(Clone)]
pub(crate) struct PermissionInheritance {
    directory: Option<Arc<DirectoryPolicy>>,
    skills: Option<Arc<crate::skill::SkillAccess>>,
}

/// Revoke one child's inherited grants when its independent callback owner is retired.
#[must_use = "dropping the lease revokes inherited child grants"]
pub(crate) struct InheritedPermissionLease {
    _directory: DirectoryExecution,
    _skills: Option<crate::skill::SkillExecution>,
}

impl InheritedPermissionLease {
    /// Get the selected-skill observer retained for this child's independent native items.
    pub(crate) fn skill_observer(&self) -> Option<Arc<dyn crate::provider::events::EventObserver>> {
        self._skills
            .as_ref()
            .map(crate::skill::SkillExecution::event_observer)
    }
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
    /// Remove only this request generation and announce a disconnected requester.
    fn drop(&mut self) {
        let pending = self.registry.lock().ok().and_then(|mut pending| {
            if pending
                .get(&self.id)
                .is_some_and(|entry| entry.token == self.token)
            {
                pending.remove(&self.id)
            } else {
                None
            }
        });
        if let Some(pending) = pending
            && let Some(sink) = &pending.resolution_sink
            && let Ok(cancelled_at_ms) = now_ms()
        {
            let _ = sink.send(crate::provider::HarnessEvent::Host {
                scope: sink.scope().clone(),
                event: HostEvent::PermissionApproval {
                    execution_kind: pending.execution_kind,
                    event: PermissionApprovalEvent::Cancelled(PermissionApprovalCancellation {
                        request_id: pending.presentation.request_id,
                        scope: pending.presentation.scope,
                        reason: ApprovalCancellationReason::RequesterGone,
                        cancelled_at_ms,
                    }),
                },
            });
        }
    }
}

pub struct PermissionCoordinator {
    pending_map: Mutex<BTreeMap<String, PendingApproval>>,
    store: Arc<RwLock<PermissionStore>>,
    timeout_by_session: Mutex<BTreeMap<String, InputTimeout>>,
    feedback_by_owner: Mutex<HashMap<crate::provider::ThreadOwner, String>>,
    feedback_watch: Mutex<HashMap<crate::provider::ThreadOwner, tokio::sync::watch::Sender<bool>>>,
    skill_by_owner: Mutex<HashMap<crate::provider::ThreadOwner, Arc<crate::skill::SkillAccess>>>,
    directory_by_scope: Mutex<HashMap<crate::provider::InvocationScope, Arc<DirectoryPolicy>>>,
}

impl PermissionCoordinator {
    /// Capture the original policy before parent foreground collection releases its leases.
    pub(crate) fn capture_inheritance(
        &self,
        scope: &crate::provider::InvocationScope,
    ) -> Result<PermissionInheritance> {
        let directory = self
            .directory_by_scope
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?
            .get(scope)
            .cloned();
        let skills = self
            .skill_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("skill permission lock poisoned"))?
            .get(&scope.owner())
            .cloned();
        Ok(PermissionInheritance { directory, skills })
    }

    /// Install only a proven child's captured policy under its original independent owner.
    pub(crate) fn inherit(
        self: &Arc<Self>,
        scope: &crate::provider::InvocationScope,
        captured: &PermissionInheritance,
    ) -> Result<InheritedPermissionLease> {
        let policy = captured
            .directory
            .clone()
            .context("native child has no captured directory policy")?;
        let mut directories = self
            .directory_by_scope
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?;
        let mut skills = self
            .skill_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("skill permission lock poisoned"))?;
        anyhow::ensure!(
            !directories.contains_key(scope) && !skills.contains_key(&scope.owner()),
            "native child policy already has an independent owner"
        );
        directories.insert(scope.clone(), policy.clone());
        let skills = captured.skills.as_ref().map(|access| {
            skills.insert(scope.owner().clone(), access.clone());
            crate::skill::SkillExecution {
                observer: Arc::new(Mutex::new(crate::skill::SkillObserver::new(access.clone()))),
                access: access.clone(),
                owner: scope.owner().clone(),
                coordinator: self.clone(),
            }
        });
        Ok(InheritedPermissionLease {
            _directory: DirectoryExecution {
                scope: scope.clone(),
                policy,
                coordinator: self.clone(),
            },
            _skills: skills,
        })
    }

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
            request.owner()? == scope.owner(),
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
        owner: &crate::provider::ThreadOwner,
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
        owner: &crate::provider::ThreadOwner,
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
        owner: &crate::provider::ThreadOwner,
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

    fn publish_timeout_feedback(&self, owner: &crate::provider::ThreadOwner) -> Result<()> {
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

    /// Expire every timed approval that one owner awaits, exactly as its elapsed deadline would.
    ///
    /// Only the mock provider's deterministic control surface calls this, so integration
    /// fixtures can exercise automatic rejection without waiting on the real clock. Approvals
    /// without a deadline are left pending, and the count of triggered approvals is returned.
    pub(crate) fn expire_pending_for_owner(
        &self,
        owner: &crate::provider::ThreadOwner,
    ) -> Result<usize> {
        let pending_map = self
            .pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
        let mut expired_count = 0;
        for pending in pending_map.values().filter(|pending| {
            pending.view.deadline.is_some() && pending.view.execution_owner.as_ref() == Some(owner)
        }) {
            // A stored permit lets a trigger that precedes the waiter's poll still win.
            pending.expiry_trigger.notify_one();
            expired_count += 1;
        }
        Ok(expired_count)
    }

    /// Check whether one owner needs a serialized continuation after denial.
    pub fn has_timeout_feedback(&self, owner: &crate::provider::ThreadOwner) -> Result<bool> {
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
        event_sink: Option<&HarnessEventSink>,
    ) -> Result<ApprovalResolution> {
        self.authorize_scoped(
            approval_mode,
            request,
            address,
            event_sink,
            None,
            ApprovalExecutionKind::Managed,
        )
        .await
    }

    /// Evaluate approval under a revocable native callback generation.
    pub(crate) async fn authorize_scoped(
        &self,
        approval_mode: ApprovalMode,
        mut request: PermissionRequest,
        address: Option<crate::provider::TurnAddress>,
        event_sink: Option<&HarnessEventSink>,
        authority: Option<Arc<ApprovalAuthority>>,
        execution_kind: ApprovalExecutionKind,
    ) -> Result<ApprovalResolution> {
        // Native callback correlation remains in the adapter; host approvals own independent identities.
        request.id = Uuid::new_v4().to_string();
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
                    sink.send_host(HostEvent::Failure {
                        failure: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Authorization,
                        ),
                        // Evaluation errors originate in Harness policy code rather than
                        // provider payloads, so the cause is safe and necessary to show.
                        message: forge_provider_api::SanitizedMessage::new(format!(
                            "Permission evaluation failed: {error:#}"
                        ))
                        .expect("diagnostic has a static prefix"),
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
                .map(|address| address.scope.owner())
                .or_else(|| event_sink.map(|sink| sink.scope().owner()))
                .and_then(|owner| scopes.get(&owner).cloned());
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
            PermissionDecision::Allow => {
                Self::emit_automatic(
                    event_sink,
                    address.as_ref(),
                    &request,
                    ApprovalResolution::AllowOnce,
                    ApprovalDecisionSource::SavedPolicy,
                    execution_kind,
                )
                .await?;
                return Ok(ApprovalResolution::AllowOnce);
            }
            PermissionDecision::Deny => {
                Self::emit_automatic(
                    event_sink,
                    address.as_ref(),
                    &request,
                    ApprovalResolution::DenyPolicy,
                    ApprovalDecisionSource::SavedPolicy,
                    execution_kind,
                )
                .await?;
                return Ok(ApprovalResolution::DenyPolicy);
            }
            PermissionDecision::Ask => match approval_mode {
                ApprovalMode::Deny => {
                    Self::emit_automatic(
                        event_sink,
                        address.as_ref(),
                        &request,
                        ApprovalResolution::DenyMode,
                        ApprovalDecisionSource::ApprovalMode,
                        execution_kind,
                    )
                    .await?;
                    return Ok(ApprovalResolution::DenyMode);
                }
                ApprovalMode::Allow => {
                    Self::emit_automatic(
                        event_sink,
                        address.as_ref(),
                        &request,
                        ApprovalResolution::AllowOnce,
                        ApprovalDecisionSource::ApprovalMode,
                        execution_kind,
                    )
                    .await?;
                    return Ok(ApprovalResolution::AllowOnce);
                }
                ApprovalMode::Ask => {}
            },
        }
        let execution_owner = address
            .as_ref()
            .map(|address| address.scope.owner().clone())
            .or_else(|| event_sink.map(|sink| sink.scope().owner().clone()));
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
        self.wait_for_approval(
            view,
            evaluation.pending_target_list,
            ApprovalExecution {
                address,
                event_sink,
                directory_policy,
                authority,
                kind: execution_kind,
            },
        )
        .await
    }

    /// Reviews one exact proposal without creating reusable permission grants.
    pub(crate) async fn review(
        &self,
        title: String,
        detail: String,
        scope: crate::provider::InvocationScope,
        event_sink: &HarnessEventSink,
    ) -> Result<ApprovalResolution> {
        let view = ApprovalRequestView {
            id: Uuid::new_v4().to_string(),
            execution_owner: Some(scope.owner().clone()),
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
        self.wait_for_approval(
            view,
            vec![PermissionTarget::HostTool {
                name: "review_proposal".into(),
            }],
            ApprovalExecution {
                address: None,
                event_sink: Some(event_sink),
                directory_policy: None,
                authority: None,
                kind: ApprovalExecutionKind::Managed,
            },
        )
        .await
    }

    /// Await a decision under the original execution's policy and revocation authority.
    async fn wait_for_approval(
        &self,
        mut view: ApprovalRequestView,
        targets: Vec<PermissionTarget>,
        execution: ApprovalExecution<'_>,
    ) -> Result<ApprovalResolution> {
        let ApprovalExecution {
            address,
            event_sink,
            directory_policy,
            authority,
            kind: execution_kind,
        } = execution;
        if let Some(sink) = event_sink {
            view.exchange_id = sink
                .scope()
                .invocation()
                .map(|scope| scope.exchange_id.to_string());
        }
        if let Some(address) = &address {
            view.turn_id = Some(address.turn_id.clone());
        }
        let id = view.id.clone();
        let timeout = self
            .timeout_by_session
            .lock()
            .map_err(|_| anyhow::anyhow!("approval timeout lock poisoned"))?
            .get(
                view.execution_owner
                    .as_ref()
                    .map(|owner| owner.session_id().as_str())
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
        let presentation = permission_request(&view, targets, address.as_ref(), event_sink)?;
        let expires = tokio::time::Instant::now() + timeout.duration();
        let (sender, mut receiver) = oneshot::channel();
        let token = Uuid::new_v4();
        let expiry_trigger = Arc::new(tokio::sync::Notify::new());
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
                    presentation: presentation.clone(),
                    execution_kind,
                    response: sender,
                    resolution_sink: event_sink.cloned(),
                    directory_policy,
                    authority: authority.clone(),
                    expiry_trigger: Arc::clone(&expiry_trigger),
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
                .send_host(HostEvent::PermissionApproval {
                    execution_kind,
                    event: PermissionApprovalEvent::Requested(presentation),
                })
                .await?;
        }
        let expiry = async {
            if timeout.amount == 0 {
                std::future::pending::<()>().await;
            }
            tokio::select! {
                () = tokio::time::sleep_until(expires) => {}
                () = expiry_trigger.notified() => {}
            }
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
                    Self::emit_resolution(event_sink, &pending, &[], &[], true).await?;
                    Ok(ApprovalResolution::TimedOut)
                } else {
                    // A manual resolver already owns the terminal decision and its persistence.
                    receiver.await.with_context(|| format!("approval request {id} was cancelled"))
                }
            }
        }
    }

    /// Resolve a pending request and publish its terminal event on the requesting execution's stream.
    /// Use `event_sink` only when the request was registered without a stream.
    pub async fn resolve(
        &self,
        approval_id: &str,
        answer_list: &[ApprovalAnswer],
        event_sink: Option<&HarnessEventSink>,
    ) -> Result<ApprovalRequestView> {
        let (pending, resolution, directory_list, selected, persisted) = {
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
            let (pending, resolution, rule_list, directory_list, selected) = {
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
                let mut selected = Vec::new();
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
                    selected.push((item.id.clone(), choice.clone()));
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
                (pending, resolution, rule_list, directory_list, selected)
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
            (pending, resolution, directory_list, selected, persisted)
        };
        let event_sink = pending.resolution_sink.as_ref().or(event_sink);
        if let Err(error) = persisted {
            Self::emit_cancelled(
                event_sink,
                &pending,
                ApprovalCancellationReason::PersistenceFailed(
                    crate::provider::lifecycle_error::diagnostic(
                        forge_provider_api::ProviderOperation::TurnStart,
                        error.as_ref(),
                    ),
                ),
            )
            .await?;
            return Err(error);
        }
        // Queue the terminal event before the provider can resume and finish its turn.
        if !directory_list.is_empty()
            && let Some(sink) = event_sink
        {
            for grant in directory_list.iter().cloned() {
                sink.send_host(HostEvent::DirectoryAccessGranted(DirectoryAccessGrant {
                    request_id: pending.presentation.request_id.clone(),
                    scope: pending.presentation.scope.clone(),
                    grant,
                    granted_at_ms: now_ms()?,
                }))
                .await?;
            }
        }
        if resolution == ApprovalResolution::Cancel {
            Self::emit_cancelled(
                event_sink,
                &pending,
                ApprovalCancellationReason::UserCancelled,
            )
            .await?;
        } else {
            Self::emit_resolution(event_sink, &pending, &selected, &directory_list, false).await?;
        }
        // A committed decision stays committed even if the native requester disconnects.
        pending
            .response
            .send(resolution)
            .map_err(|_| anyhow::anyhow!("provider no longer waits for approval {approval_id}"))?;
        Ok(pending.view)
    }

    /// Project pending requests with their original host control owner and exact offered choices.
    pub fn pending_presentations(&self) -> Result<Vec<ApprovalRequestPresentation>> {
        self.pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?
            .values()
            .map(|pending| {
                Ok(ApprovalRequestPresentation {
                    request: pending.presentation.clone(),
                    execution_kind: pending.execution_kind,
                    execution_owner: pending
                        .view
                        .execution_owner
                        .clone()
                        .context("pending approval has no original owner")?,
                    exchange_id: pending.view.exchange_id.clone(),
                })
            })
            .collect()
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

    pub async fn cancel_all(&self, event_sink: Option<&HarnessEventSink>) -> Result<()> {
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
        Self::cancel_pending(
            pending_list,
            event_sink,
            ApprovalCancellationReason::ThreadClosed,
        )
        .await
    }

    /// Cancel only approvals owned by the interrupted execution.
    pub async fn cancel_owner(
        &self,
        owner: &crate::provider::ThreadOwner,
        event_sink: Option<&HarnessEventSink>,
    ) -> Result<()> {
        let pending_list = self.take_owner_approvals(owner)?;
        Self::cancel_pending(
            pending_list,
            event_sink,
            ApprovalCancellationReason::TurnInterrupted,
        )
        .await
    }

    /// Publish cancellation on each request's own stream and release every waiter on failure.
    async fn cancel_pending(
        pending_list: Vec<PendingApproval>,
        event_sink: Option<&HarnessEventSink>,
        reason: ApprovalCancellationReason,
    ) -> Result<()> {
        let mut first_error = None;
        for pending in pending_list {
            let sink = pending.resolution_sink.as_ref().or(event_sink);
            let result = Self::emit_cancelled(sink, &pending, reason.clone()).await;
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
        owner: &crate::provider::ThreadOwner,
    ) -> Result<()> {
        for pending in self.take_owner_approvals(owner)? {
            let _ = pending.response.send(ApprovalResolution::Cancel);
        }
        Ok(())
    }

    fn take_owner_approvals(
        &self,
        owner: &crate::provider::ThreadOwner,
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

    /// Preserve automatic permission authority without inventing a pending user choice.
    async fn emit_automatic(
        sink: Option<&HarnessEventSink>,
        address: Option<&crate::provider::TurnAddress>,
        request: &PermissionRequest,
        resolution: ApprovalResolution,
        source: ApprovalDecisionSource,
        execution_kind: ApprovalExecutionKind,
    ) -> Result<()> {
        let Some(sink) = sink else {
            return Ok(());
        };
        let scope = match address {
            Some(address) => ApprovalScope::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
            },
            None => ApprovalScope::Thread {
                thread_id: sink.scope().owner().thread_id(),
            },
        };
        let choice = ApprovalChoice {
            id: "automatic".into(),
            label: "Automatic permission decision".into(),
            resolution,
            rule_list: Vec::new(),
            directory: None,
        };
        let targets = request
            .target_list
            .iter()
            .enumerate()
            .map(|(index, _)| {
                Ok(ResolvedPermissionTarget {
                    target_id: ApprovalTargetId::new(index.to_string())?,
                    decision: choice_decision(&choice)?,
                    source,
                    rules: Vec::new(),
                    directory_grants: Vec::new(),
                })
            })
            .collect::<Result<Vec<_>>>()?;
        sink.send_host(HostEvent::PermissionApproval {
            execution_kind,
            event: PermissionApprovalEvent::Resolved(PermissionApprovalResolution {
                request_id: ApprovalRequestId::new(request.id.clone())?,
                scope,
                targets,
                resolved_at_ms: now_ms()?,
            }),
        })
        .await?;
        Ok(())
    }

    /// Publish committed per-target choices with their actual decision source.
    async fn emit_resolution(
        event_sink: Option<&HarnessEventSink>,
        pending: &PendingApproval,
        selected: &[(String, ApprovalChoice)],
        committed_directory_grants: &[crate::permissions::directory::DirectoryGrant],
        timed_out: bool,
    ) -> Result<()> {
        if let Some(sink) = event_sink {
            let targets = if timed_out {
                pending
                    .presentation
                    .targets
                    .iter()
                    .map(|target| ResolvedPermissionTarget {
                        target_id: target.target_id.clone(),
                        decision: super::host_event::PermissionDecision::TimedOut,
                        source: ApprovalDecisionSource::Timeout,
                        rules: Vec::new(),
                        directory_grants: Vec::new(),
                    })
                    .collect()
            } else {
                selected
                    .iter()
                    .map(|(id, choice)| {
                        Ok(ResolvedPermissionTarget {
                            target_id: ApprovalTargetId::new(id.clone())?,
                            decision: choice_decision(choice)?,
                            source: ApprovalDecisionSource::User,
                            rules: choice_rules(choice)?,
                            directory_grants: choice
                                .directory
                                .iter()
                                .filter(|grant| committed_directory_grants.contains(grant))
                                .cloned()
                                .collect(),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?
            };
            sink.send_host(HostEvent::PermissionApproval {
                execution_kind: pending.execution_kind,
                event: PermissionApprovalEvent::Resolved(PermissionApprovalResolution {
                    request_id: pending.presentation.request_id.clone(),
                    scope: pending.presentation.scope.clone(),
                    targets,
                    resolved_at_ms: now_ms()?,
                }),
            })
            .await?;
        }
        Ok(())
    }

    /// End a request without fabricating a user denial.
    async fn emit_cancelled(
        event_sink: Option<&HarnessEventSink>,
        pending: &PendingApproval,
        reason: ApprovalCancellationReason,
    ) -> Result<()> {
        if let Some(sink) = event_sink {
            sink.send_host(HostEvent::PermissionApproval {
                execution_kind: pending.execution_kind,
                event: PermissionApprovalEvent::Cancelled(PermissionApprovalCancellation {
                    request_id: pending.presentation.request_id.clone(),
                    scope: pending.presentation.scope.clone(),
                    reason,
                    cancelled_at_ms: now_ms()?,
                }),
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

pub use super::approval_view::{
    ApprovalAnswer, ApprovalChoice, ApprovalCommand, ApprovalItem, ApprovalRequestView,
    ApprovalResolution,
};

/// Capture the immutable permission owner and independently identified target choices.
fn permission_request(
    view: &ApprovalRequestView,
    targets: Vec<PermissionTarget>,
    address: Option<&crate::provider::TurnAddress>,
    sink: Option<&HarnessEventSink>,
) -> Result<PermissionApprovalRequest> {
    let scope = match address {
        Some(address) => ApprovalScope::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
        },
        None => ApprovalScope::Thread {
            thread_id: view
                .execution_owner
                .clone()
                .or_else(|| sink.map(|sink| sink.scope().owner()))
                .context("approval has no original owner")?
                .thread_id(),
        },
    };
    anyhow::ensure!(
        targets.len() == view.item_list.len(),
        "approval targets do not match presentation items"
    );
    let targets = view
        .item_list
        .iter()
        .zip(targets)
        .map(|(item, target)| {
            let choices = item
                .choice_list
                .iter()
                .filter(|choice| choice.resolution != ApprovalResolution::Cancel)
                .map(|choice| {
                    Ok(PermissionApprovalChoice::new(
                        choice.id.clone(),
                        choice.label.clone(),
                        choice_decision(choice)?,
                        choice_rules(choice)?,
                        choice.directory.iter().cloned().collect(),
                    )?)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(PermissionApprovalTarget {
                target_id: ApprovalTargetId::new(item.id.clone())?,
                target,
                title: item.title.clone(),
                detail: item.detail.clone(),
                commands: item.command_list.clone(),
                choices,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    Ok(PermissionApprovalRequest {
        request_id: ApprovalRequestId::new(view.id.clone())?,
        scope,
        reason: view
            .reason
            .as_ref()
            .map(|reason| SanitizedMessage::new(reason.clone()))
            .transpose()?,
        targets,
        deadline: view.deadline.clone(),
    })
}

/// Translate callback dispositions without conflating timeout or cancellation.
fn choice_decision(choice: &ApprovalChoice) -> Result<super::host_event::PermissionDecision> {
    use super::host_event::PermissionDecision as Decision;
    let scope = match choice.resolution {
        ApprovalResolution::AllowOnce if choice.directory.is_some() => {
            PermissionRuleScope::BroadRule
        }
        ApprovalResolution::AllowOnce
        | ApprovalResolution::DenyOnce
        | ApprovalResolution::DenyPolicy
        | ApprovalResolution::DenyMode => PermissionRuleScope::Once,
        ApprovalResolution::AllowExact | ApprovalResolution::DenyExact => {
            PermissionRuleScope::ExactRule
        }
        ApprovalResolution::AllowBroad | ApprovalResolution::DenyBroad => {
            PermissionRuleScope::BroadRule
        }
        ApprovalResolution::TimedOut => return Ok(Decision::TimedOut),
        ApprovalResolution::Cancel => anyhow::bail!("cancellation is not a permission decision"),
    };
    Ok(match choice.resolution {
        ApprovalResolution::AllowOnce
        | ApprovalResolution::AllowExact
        | ApprovalResolution::AllowBroad => Decision::Allow { scope },
        ApprovalResolution::DenyOnce
        | ApprovalResolution::DenyExact
        | ApprovalResolution::DenyBroad
        | ApprovalResolution::DenyPolicy
        | ApprovalResolution::DenyMode => Decision::Deny {
            scope,
            reason: SanitizedMessage::new("Permission denied").expect("static denial is nonempty"),
        },
        ApprovalResolution::TimedOut | ApprovalResolution::Cancel => {
            unreachable!("terminal dispositions returned before scope selection")
        }
    })
}

/// Preserve the established category and exact displayed saved pattern.
fn choice_rules(choice: &ApprovalChoice) -> Result<Vec<ApprovalRulePattern>> {
    choice
        .rule_list
        .iter()
        .map(|(category, pattern)| {
            Ok(ApprovalRulePattern {
                category: ApprovalRuleCategory::from_saved_category(category)
                    .context("unsupported permission rule category")?,
                pattern: pattern.clone(),
            })
        })
        .collect()
}

/// Read a checked wall-clock timestamp for committed host notifications.
fn now_ms() -> Result<i64> {
    Ok(i64::try_from(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)?
            .as_millis(),
    )?)
}

#[cfg(test)]
mod test {
    use crate::provider::session_execution::OwnerObservation;
    use crate::provider::{HarnessEvent, InvocationScope, ThreadOwner, TurnAddress};
    use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};

    fn scope() -> InvocationScope {
        InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(ThreadOwner::root("approval")),
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

    fn approval(event: OwnerObservation) -> ApprovalRequestView {
        let OwnerObservation::Event(HarnessEvent::Host {
            scope,
            event:
                HostEvent::PermissionApproval {
                    execution_kind: _,
                    event: PermissionApprovalEvent::Requested(request),
                },
        }) = event
        else {
            panic!("expected requested approval");
        };
        let item_list = request
            .targets
            .iter()
            .map(|target| ApprovalItem {
                id: target.target_id.as_str().to_owned(),
                title: target.title.clone(),
                detail: target.detail.clone(),
                command_list: target.commands.clone(),
                choice_list: target
                    .choices
                    .iter()
                    .map(|choice| ApprovalChoice {
                        id: choice.choice_id().to_owned(),
                        label: choice.label().to_owned(),
                        resolution: match choice.decision() {
                            super::super::PermissionDecision::Allow {
                                scope: PermissionRuleScope::Once,
                            } => ApprovalResolution::AllowOnce,
                            super::super::PermissionDecision::Allow {
                                scope: PermissionRuleScope::ExactRule,
                            } => ApprovalResolution::AllowExact,
                            super::super::PermissionDecision::Allow {
                                scope: PermissionRuleScope::BroadRule,
                            } => ApprovalResolution::AllowBroad,
                            super::super::PermissionDecision::Deny {
                                scope: PermissionRuleScope::Once,
                                ..
                            } => ApprovalResolution::DenyOnce,
                            super::super::PermissionDecision::Deny {
                                scope: PermissionRuleScope::ExactRule,
                                ..
                            } => ApprovalResolution::DenyExact,
                            super::super::PermissionDecision::Deny {
                                scope: PermissionRuleScope::BroadRule,
                                ..
                            } => ApprovalResolution::DenyBroad,
                            super::super::PermissionDecision::TimedOut => {
                                panic!("timeout was offered as a choice")
                            }
                        },
                        rule_list: choice
                            .rules()
                            .iter()
                            .map(|rule| {
                                (
                                    rule.category.saved_category().to_owned(),
                                    rule.pattern.clone(),
                                )
                            })
                            .collect(),
                        directory: choice.directory_grants().first().cloned(),
                    })
                    .collect(),
            })
            .collect();
        ApprovalRequestView {
            id: request.request_id.as_str().to_owned(),
            execution_owner: Some(scope.owner().clone()),
            exchange_id: scope
                .invocation()
                .map(|scope| scope.exchange_id.to_string()),
            turn_id: None,
            provider: None,
            reason: request.reason.map(|reason| reason.to_string()),
            item_list,
            deadline: request.deadline,
            resolution_source: ResolutionSource::User,
        }
    }

    #[tokio::test]
    async fn directory_approval_persists_global_grants_before_releasing_requests() -> Result<()> {
        use super::*;
        let root = tempfile::tempdir()?;
        let workspace = root.path().join("workspace");
        let external = root.path().join("external");
        std::fs::create_dir(&workspace)?;
        std::fs::create_dir_all(external.join("nested"))?;
        forge_git::test_support::init_repository(&external);
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
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        };
        let address = Some(TurnAddress {
            scope: InvocationScope {
                thread_id: forge_provider_api::ThreadId::from(request.owner()?),
                ..scope()
            },
            thread_id: forge_provider_api::ThreadId::new("native").unwrap(),
            turn_id: forge_provider_api::TurnId::new("turn").unwrap(),
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
        let event = stream.recv().await.unwrap().unwrap();
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
        for _ in 0..2 {
            assert!(matches!(
                stream.recv().await.unwrap().unwrap(),
                OwnerObservation::Event(HarnessEvent::Host {
                    event: HostEvent::DirectoryAccessGranted(_),
                    ..
                })
            ));
        }
        assert!(matches!(
            stream.recv().await.unwrap().unwrap(),
            OwnerObservation::Event(HarnessEvent::Host {
                event: HostEvent::PermissionApproval {
                    execution_kind: _,
                    event: PermissionApprovalEvent::Resolved(_)
                },
                ..
            })
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
                thread_id: forge_provider_api::ThreadId::from(request.owner()?),
                ..scope()
            },
            thread_id: forge_provider_api::ThreadId::new("native").unwrap(),
            turn_id: forge_provider_api::TurnId::new("turn").unwrap(),
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
            OwnerObservation::Event(HarnessEvent::Host { event: HostEvent::Failure { failure, message }, .. }) if failure.kind() == forge_provider_api::ProviderFailureKind::Authorization && message.as_str().starts_with("Permission evaluation failed: ") && message.as_str().contains("empty")
        ));
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
                let failure = coordinator
                    .authorize(mode, request, None, Some(&sink))
                    .await
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
                    OwnerObservation::Event(HarnessEvent::Host { event: HostEvent::Failure { failure, .. }, .. })
                        if failure.kind() == forge_provider_api::ProviderFailureKind::Authorization
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
            assert!(matches!(
                stream.try_recv().unwrap(),
                OwnerObservation::Event(HarnessEvent::Host {
                    event: HostEvent::PermissionApproval {
                        event: PermissionApprovalEvent::Resolved(_),
                        ..
                    },
                    ..
                })
            ));
        }
        assert_eq!(std::fs::read(path).unwrap(), before);
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
            let owner = crate::provider::ThreadOwner::new(
                "root".into(),
                forge_provider_api::AgentId::new(agent_id).unwrap(),
            )
            .expect("registered session and agent identities are nonempty");
            let (sink, mut stream) = crate::provider::events::channel(InvocationScope {
                thread_id: owner.thread_id(),
                ..scope()
            });
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
                            thread_id: forge_provider_api::ThreadId::new("thread").unwrap(),
                            turn_id: forge_provider_api::TurnId::new("turn").unwrap(),
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
                &crate::provider::ThreadOwner::new(
                    "root".into(),
                    forge_provider_api::AgentId::new("first").unwrap(),
                )
                .expect("registered session and agent identities are nonempty"),
                None,
            )
            .await
            .unwrap();
        assert!(matches!(
            channels[0].1.recv().await.unwrap().unwrap(),
            OwnerObservation::Event(HarnessEvent::Host {
                event: HostEvent::PermissionApproval {
                    execution_kind: _,
                    event: PermissionApprovalEvent::Cancelled(_)
                },
                ..
            })
        ));
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(!waiter[0].is_finished());
        let pending = coordinator.pending_list().unwrap();
        assert_eq!(pending.len(), 1);
        assert_eq!(
            *pending[0].execution_owner.as_ref().unwrap().agent_id(),
            forge_provider_api::AgentId::new("second").unwrap()
        );
        coordinator
            .cancel_owner(
                &crate::provider::ThreadOwner::new(
                    "root".into(),
                    forge_provider_api::AgentId::new("second").unwrap(),
                )
                .expect("registered session and agent identities are nonempty"),
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(matches!(
            channels[1].1.recv().await.unwrap().unwrap(),
            OwnerObservation::Event(HarnessEvent::Host {
                event: HostEvent::PermissionApproval {
                    execution_kind: _,
                    event: PermissionApprovalEvent::Cancelled(_)
                },
                ..
            })
        ));
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
        let owned = coordinator.pending_list().unwrap().remove(0);
        assert_eq!(
            owned.exchange_id.as_deref(),
            Some(scope().exchange_id.as_str())
        );
        assert!(owned.turn_id.is_none());
        let snapshot = coordinator.pending_presentations().unwrap();
        assert_eq!(snapshot[0].execution_owner, scope().owner());
        assert_eq!(snapshot[0].request.request_id.as_str(), view.id);
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
        let resolved = event_stream.recv().await.unwrap().unwrap();
        let OwnerObservation::Event(HarnessEvent::Host {
            event:
                HostEvent::PermissionApproval {
                    execution_kind: _,
                    event: PermissionApprovalEvent::Resolved(resolved_view),
                },
            ..
        }) = resolved
        else {
            panic!("expected resolved approval");
        };
        assert!(matches!(
            resolved_view.scope,
            ApprovalScope::Thread { .. } | ApprovalScope::Turn { .. }
        ));
        assert_eq!(resolved_view.targets.len(), owned.item_list.len());
        let source = store.read().unwrap().open().1;
        assert!(source.contains("\"rg *\": \"allow\""));
    }
}
