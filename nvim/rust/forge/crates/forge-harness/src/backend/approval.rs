use crate::backend::{BackendEvent, BackendEventSink};
use crate::permissions::command::{
    broad_command_pattern, exact_command_pattern, normalize_command_in,
};
use crate::permissions::directory::{DirectoryGrant, DirectoryOperation, DirectoryPolicy};
use crate::permissions::document::{
    CATEGORY_BASH, CATEGORY_EDIT, CATEGORY_ELEVATE, CATEGORY_MCP, CATEGORY_READ, CATEGORY_TOOL,
    CATEGORY_WEBFETCH, PermissionDecision,
};
use crate::permissions::matcher::{
    PendingCommand, PermissionEvaluation, PermissionRequest, PermissionTarget,
};
use crate::permissions::shell::{self, CommandHighlight, CommandShell};
use crate::permissions::store::PermissionStore;
use crate::session::{ApprovalMode, InputDeadline, InputTimeout, ResolutionSource};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::{BTreeMap, HashMap};
use std::ops::Range;
use std::path::Path;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::oneshot;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalResolution {
    AllowOnce,
    AllowExact,
    AllowBroad,
    DenyOnce,
    DenyExact,
    DenyBroad,
    /// Automatic denial by saved permissions or filesystem access policy.
    DenyPolicy,
    /// Automatic denial because approval prompts are disabled.
    DenyMode,
    /// Rejects an expired request without recording a persistent rule.
    TimedOut,
    Cancel,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalChoice {
    pub id: String,
    pub label: String,
    pub resolution: ApprovalResolution,
    #[serde(default)]
    pub rule_list: Vec<(String, String)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory: Option<DirectoryGrant>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRequestView {
    pub id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<crate::backend::ExecutionOwner>,
    pub exchange_id: Option<String>,
    pub turn_id: Option<String>,
    pub provider: String,
    pub reason: Option<String>,
    pub item_list: Vec<ApprovalItem>,
    /// Captures the original deadline independently of the active window.
    #[serde(default)]
    pub deadline: Option<InputDeadline>,
    /// Distinguishes automatic rejection from an explicit user decision.
    #[serde(default)]
    pub resolution_source: ResolutionSource,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// One unapproved target with choices scoped to that target.
pub struct ApprovalItem {
    pub id: String,
    pub title: String,
    pub detail: String,
    /// Full source commands containing this target, with all matching executable occurrences.
    pub command_list: Vec<ApprovalCommand>,
    pub choice_list: Vec<ApprovalChoice>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Preserves command context and source coordinates for formatting and scoped approval markers.
pub struct ApprovalCommand {
    /// Retains the complete script payload with shell launchers removed for presentation.
    pub source: String,
    /// Selects the language of the retained source.
    pub shell: CommandShell,
    /// Locates only executable occurrences covered by this item's choices.
    pub focus_range_list: Vec<Range<usize>>,
    /// Colors the original source independently of its approval markers.
    pub highlight_list: Vec<CommandHighlight>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// An explicit choice for one item in a pending approval request.
pub struct ApprovalAnswer {
    pub item_id: String,
    pub choice_id: String,
}

struct PendingApproval {
    token: Uuid,
    view: ApprovalRequestView,
    response: oneshot::Sender<ApprovalResolution>,
    resolution_sink: Option<BackendEventSink>,
    directory_policy: Option<Arc<DirectoryPolicy>>,
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
    timeout_by_session: Mutex<BTreeMap<String, (InputTimeout, InputTimeout)>>,
    feedback_by_owner: Mutex<HashMap<crate::backend::ExecutionOwner, String>>,
    feedback_watch:
        Mutex<HashMap<crate::backend::ExecutionOwner, tokio::sync::watch::Sender<bool>>>,
    skill_by_owner: Mutex<HashMap<crate::backend::ExecutionOwner, Arc<crate::skill::SkillAccess>>>,
    directory_by_owner: Mutex<HashMap<crate::backend::ExecutionOwner, Arc<DirectoryPolicy>>>,
}

impl PermissionCoordinator {
    /// Report the captured write-off boundary for an active execution owner.
    pub(crate) fn writes_disabled(
        &self,
        owner: Option<&crate::backend::ExecutionOwner>,
    ) -> Result<bool> {
        let policy = self
            .directory_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?;
        Ok(owner
            .and_then(|owner| policy.get(owner))
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
            directory_by_owner: Mutex::new(HashMap::new()),
        }
    }

    pub(crate) fn directories(
        self: &Arc<Self>,
        root: Option<&Path>,
        request: &crate::backend::BackendRequest,
    ) -> Result<DirectoryExecution> {
        let owner = request.owner()?;
        let policy = Arc::new(DirectoryPolicy {
            access: request.access.clone(),
            workspace: request.workspace.clone().into(),
            session_id: request.harness_session_id.clone(),
            settings_root: root.map(Path::to_owned),
        });
        let mut directory = self
            .directory_by_owner
            .lock()
            .map_err(|_| anyhow::anyhow!("directory permission lock poisoned"))?;
        anyhow::ensure!(
            !directory.contains_key(&owner),
            "directory access already belongs to an active execution"
        );
        directory.insert(owner.clone(), policy.clone());
        Ok(DirectoryExecution {
            owner,
            policy,
            coordinator: self.clone(),
        })
    }

    /// Bind selected bundle access to one provider execution and revoke it when the guard drops.
    pub(crate) fn skills(
        self: &Arc<Self>,
        root: Option<&Path>,
        request: &crate::backend::BackendRequest,
        backend: crate::backend::BackendKind,
    ) -> Result<crate::skill::SkillExecution> {
        let (provider, target) = match backend {
            crate::backend::BackendKind::Codex => ("codex", "codexcli"),
            crate::backend::BackendKind::Copilot => ("copilot", "copilotcli"),
            crate::backend::BackendKind::Claude => ("claude", "claudecode"),
            crate::backend::BackendKind::Antigravity => ("antigravity", "antigravity-cli"),
            _ => ("mock", "mock"),
        };
        let selected = if request.mode == crate::backend::PromptMode::Background {
            Vec::new()
        } else if let Some(root) = root {
            crate::settings::SessionResources::load(
                root,
                &crate::backend::BackendCatalogRequest {
                    execution_owner: request.execution_owner.clone(),
                    harness_session_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    access: request.access.clone(),
                    approval_mode: request.approval_mode,
                    backend_session_id: request.backend_session_id.clone(),
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
        owner: &crate::backend::ExecutionOwner,
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
            .entry(session_id.to_owned())
            .or_default()
            .0 = timeout.validate()?;
        Ok(())
    }

    /// Publish both future-input settings without touching pending requests.
    pub fn configure_input_timeouts(
        &self,
        session_id: &str,
        approval: InputTimeout,
        answer: InputTimeout,
    ) -> Result<()> {
        self.timeout_by_session
            .lock()
            .map_err(|_| anyhow::anyhow!("approval timeout lock poisoned"))?
            .insert(
                session_id.to_owned(),
                (approval.validate()?, answer.validate()?),
            );
        Ok(())
    }

    /// Read the current settings for one session independently of active provider work.
    pub fn input_timeouts(&self, session_id: &str) -> Result<(InputTimeout, InputTimeout)> {
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
        owner: &crate::backend::ExecutionOwner,
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
        owner: &crate::backend::ExecutionOwner,
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

    fn publish_timeout_feedback(&self, owner: &crate::backend::ExecutionOwner) -> Result<()> {
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
    pub fn has_timeout_feedback(&self, owner: &crate::backend::ExecutionOwner) -> Result<bool> {
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
        mut request: PermissionRequest,
        address: Option<crate::backend::ProviderAddress>,
        event_sink: Option<&BackendEventSink>,
    ) -> Result<ApprovalResolution> {
        let directory_policy = address
            .as_ref()
            .and_then(|address| address.owner.as_ref())
            .map(|owner| {
                self.directory_by_owner
                    .lock()
                    .map(|directory| directory.get(owner).cloned())
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
                    sink.send_wait(BackendEvent {
                        received_at_ms: None,
                        address: address.clone(),
                        turn_boundary: None,
                        kind: "error".into(),
                        text: Some(format!("Permission evaluation failed: {error:#}")),
                        data: json!({"error":format!("{error:#}")}),
                        activity: None,
                        summary: None,
                        checklist_update: None,
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
                .and_then(|address| address.owner.as_ref())
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
        let execution_owner = address.as_ref().and_then(|address| address.owner.clone());
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
        self.wait_for_approval(view, address, event_sink, directory_policy)
            .await
    }

    /// Reviews one exact proposal without creating reusable permission grants.
    pub(crate) async fn review(
        &self,
        title: String,
        detail: String,
        address: crate::backend::ProviderAddress,
        event_sink: &BackendEventSink,
    ) -> Result<ApprovalResolution> {
        let view = ApprovalRequestView {
            id: Uuid::new_v4().to_string(),
            execution_owner: address.owner.clone(),
            exchange_id: Some(address.turn_id.clone()),
            turn_id: None,
            provider: "Harness".into(),
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
        self.wait_for_approval(view, Some(address), Some(event_sink), None)
            .await
    }

    async fn wait_for_approval(
        &self,
        mut view: ApprovalRequestView,
        address: Option<crate::backend::ProviderAddress>,
        event_sink: Option<&BackendEventSink>,
        directory_policy: Option<Arc<DirectoryPolicy>>,
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
            .unwrap_or_default()
            .0;
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
                .send_wait(BackendEvent {
                    received_at_ms: None,
                    address: address.clone(),
                    turn_boundary: None,
                    kind: "approval_requested".into(),
                    text: None,
                    data: serde_json::to_value(&view)?,
                    activity: None,
                    summary: None,
                    checklist_update: None,
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
                let pending = self.pending_map.lock().map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?.remove(&id);
                if let Some(mut pending) = pending {
                    pending.view.resolution_source = ResolutionSource::Timeout;
                    if let Some(owner) = &pending.view.execution_owner {
                        self.feedback_by_owner.lock().map_err(|_| anyhow::anyhow!("approval feedback lock poisoned"))?
                            .insert(owner.clone(), format!("Approval {} was automatically rejected because its timeout expired. This was not an explicit user rejection. The existing user instruction and permissions remain in effect. Continue authorized work, avoid repeating the expired request, and do not ask whether to continue work the user already requested.", pending.view.id));
                        self.publish_timeout_feedback(owner)?;
                    }
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
                .as_deref()
                .is_none_or(|owner| Some(owner) == turn_id),
            "approval cannot change its owning turn"
        );
        approval.view.turn_id = turn_id.map(str::to_owned);
        Ok(Some(approval.view.clone()))
    }

    /// Resolve a pending request and publish its terminal event on the requesting execution's stream.
    /// Use `event_sink` only when the request was registered without a stream.
    pub async fn resolve(
        &self,
        approval_id: &str,
        answer_list: &[ApprovalAnswer],
        event_sink: Option<&BackendEventSink>,
    ) -> Result<ApprovalRequestView> {
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
        let event_sink = pending.resolution_sink.as_ref().or(event_sink);
        let persisted = (|| -> Result<()> {
            if !directory_list.is_empty() {
                pending
                    .directory_policy
                    .as_ref()
                    .context("directory policy is no longer available")?
                    .save(&directory_list)?;
            }
            if !rule_list.is_empty() {
                self.store
                    .write()
                    .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                    .set_rule_list(&rule_list)?;
            }
            Ok(())
        })();
        if let Err(error) = persisted {
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view).await?;
            return Err(error);
        }
        // Queue the terminal event before the provider can resume and finish its turn.
        if !directory_list.is_empty()
            && let Some(sink) = event_sink
        {
            sink.send_wait(BackendEvent {
                address: None,
                turn_boundary: None,
                received_at_ms: None,
                kind: "directory_access_granted".into(),
                text: None,
                activity: None,
                summary: None,
                checklist_update: None,
                data: serde_json::to_value(&directory_list)?,
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

    pub async fn cancel_all(&self, event_sink: Option<&BackendEventSink>) -> Result<()> {
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
        for pending in pending_list {
            let _ = pending.response.send(ApprovalResolution::Cancel);
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view).await?;
        }
        Ok(())
    }

    /// Cancel only approvals owned by the interrupted execution.
    pub async fn cancel_owner(
        &self,
        owner: &crate::backend::ExecutionOwner,
        event_sink: Option<&BackendEventSink>,
    ) -> Result<()> {
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
        for pending in pending_list {
            let _ = pending.response.send(ApprovalResolution::Cancel);
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view).await?;
        }
        Ok(())
    }

    async fn emit_lifecycle(
        event_sink: Option<&BackendEventSink>,
        kind: &str,
        view: &ApprovalRequestView,
    ) -> Result<()> {
        if let Some(event_sink) = event_sink {
            let _ = event_sink
                .send_wait(BackendEvent {
                    received_at_ms: None,
                    address: None,
                    turn_boundary: None,
                    kind: kind.into(),
                    text: None,
                    data: serde_json::to_value(view)?,
                    activity: None,
                    summary: None,
                    checklist_update: None,
                })
                .await;
        }
        Ok(())
    }
}

/// Revokes only the matching execution's reported file authorization when its provider settles.
pub(crate) struct DirectoryExecution {
    owner: crate::backend::ExecutionOwner,
    policy: Arc<DirectoryPolicy>,
    coordinator: Arc<PermissionCoordinator>,
}

impl Drop for DirectoryExecution {
    fn drop(&mut self) {
        if let Ok(mut directory) = self.coordinator.directory_by_owner.lock()
            && directory
                .get(&self.owner)
                .is_some_and(|policy| Arc::ptr_eq(policy, &self.policy))
        {
            directory.remove(&self.owner);
        }
    }
}

fn target_rule_list(target: &PermissionTarget, broad: bool) -> Vec<(String, String)> {
    let mut rule_list = Vec::new();
    match target {
        PermissionTarget::UnknownFile { .. } => {}
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
        provider: request.provider.clone(),
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

#[cfg(test)]
mod test {
    use crate::backend::codex::approval::{codex_response, permission_from_provider};
    use crate::backend::copilot::approval::permission_from_copilot;
    use crate::permissions::matcher::CommandAnalysisError;
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
                let (sink, mut stream) = crate::backend::events::channel();
                let waiting_coordinator = coordinator.clone();
                let waiting = tokio::spawn(async move {
                    waiting_coordinator
                        .authorize(
                            mode,
                            PermissionRequest {
                                id: "decision".into(),
                                provider: "test".into(),
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
                        assert_eq!(event.kind, "approval_requested");
                        let view: ApprovalRequestView = serde_json::from_value(event.data)?;
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
    async fn directory_approval_persists_global_grants_before_releasing_copilot_requests()
    -> Result<()> {
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
        let request = crate::backend::BackendRequest {
            execution_owner: None,
            harness_session_id: "directory-session".into(),
            workspace: workspace.display().to_string(),
            input: crate::backend::BackendInput::from_text("Read and write"),
            mode: crate::backend::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: crate::session::AccessPolicy {
                sandbox: false,
                read_access: crate::session::DirectoryScope::Workspace,
                ..Default::default()
            },
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let address = Some(crate::backend::ProviderAddress {
            owner: Some(request.owner()?),
            thread_id: "native".into(),
            turn_id: "turn".into(),
        });
        let directories = coordinator.directories(Some(&settings_root), &request)?;
        assert!(
            coordinator
                .directories(Some(&settings_root), &request)
                .is_err(),
            "live ownership was replaced"
        );
        let mut permission = permission_from_copilot(
            "directory".into(),
            "read",
            json!({"path":external.join("read.txt")}),
            &request.workspace,
        );
        let write = permission_from_copilot(
            "write".into(),
            "write",
            json!({"path":external.join("nested/new/deep/write.txt")}),
            &request.workspace,
        );
        permission.target_list.extend(write.target_list);
        let write = permission_from_copilot(
            "workspace-write".into(),
            "write",
            json!({"path":workspace.join("new/deep/write.txt")}),
            &request.workspace,
        );
        permission.target_list.extend(write.target_list);
        let original = permission.clone();
        let (sink, mut stream) = crate::backend::events::channel();
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
        assert_eq!(event.kind, "approval_requested");
        let approval: ApprovalRequestView = serde_json::from_value(event.data)?;
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
        assert_eq!(first.kind, "directory_access_granted");
        assert_eq!(
            stream.recv().await.unwrap().unwrap().kind,
            "approval_resolved"
        );
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
                    permission_from_copilot(
                        "saved-write".into(),
                        "write",
                        json!({"path":workspace.join("another/file.txt")}),
                        &request.workspace
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
                    permission_from_copilot(
                        "protected".into(),
                        "write",
                        json!({"path":protected}),
                        &request.workspace,
                    ),
                    address.clone(),
                    None,
                )
                .await?,
            ApprovalResolution::DenyPolicy
        );
        drop(directories);
        assert!(coordinator.directory_by_owner.lock().unwrap().is_empty());
        let _directories = coordinator.directories(Some(&settings_root), &request)?;
        assert_eq!(
            coordinator
                .authorize(ApprovalMode::Ask, original, address.clone(), None)
                .await?,
            ApprovalResolution::AllowOnce
        );
        let missing =
            permission_from_copilot("missing".into(), "read", json!({}), &request.workspace);
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
        let denied = permission_from_copilot(
            "denied".into(),
            "read",
            json!({"path":path}),
            &request.workspace,
        );
        assert_eq!(
            coordinator
                .authorize(ApprovalMode::Allow, denied, address, None)
                .await?,
            ApprovalResolution::DenyPolicy
        );
        let invalid = PermissionRequest {
            id: "invalid".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Read {
                path: String::new(),
            }],
        };
        let (sink, mut stream) = crate::backend::events::channel();
        let address = Some(crate::backend::ProviderAddress {
            owner: Some(request.owner()?),
            thread_id: "native".into(),
            turn_id: "turn".into(),
        });
        let failure = coordinator
            .authorize(ApprovalMode::Ask, invalid, address, Some(&sink))
            .await
            .unwrap_err();
        assert!(failure.to_string().contains("empty"));
        let event = stream.recv().await.unwrap().unwrap();
        assert_eq!(event.kind, "error");
        assert!(event.text.unwrap().contains("empty"));
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
        let request = crate::backend::BackendRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: directory.path().display().to_string(),
            input: crate::backend::BackendInput::from_text("Read skill"),
            mode: crate::backend::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: crate::session::AccessPolicy::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let address = Some(crate::backend::ProviderAddress {
            owner: Some(request.owner()?),
            thread_id: "provider".into(),
            turn_id: "turn".into(),
        });
        let skills = coordinator.skills(
            Some(directory.path()),
            &request,
            crate::backend::BackendKind::Codex,
        )?;
        let original = store.read().unwrap().open().1;
        let source = bundle.join("SKILL.md").display().to_string();
        let script = bundle.join("scripts/check.py").display().to_string();
        let operation = |target| PermissionRequest {
            id: "request".into(),
            provider: "test".into(),
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
        let catalog = settings.catalog(&request.workspace, &request.harness_session_id, "codex")?;
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
            crate::backend::BackendKind::Codex,
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
        let (sink, mut stream) = crate::backend::events::channel();
        for mode in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            for _callback in 0..1 {
                let request = PermissionRequest {
                    id: "parse-error".into(),
                    provider: "test".into(),
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
                assert_eq!(event.kind, "error");
                assert!(event.text.unwrap().contains("shell_command_parse_error"));
                assert!(stream.try_recv().is_err());
            }
            let corrected = PermissionRequest {
                id: "parse-error".into(),
                provider: "test".into(),
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
        let expired_owner = crate::backend::ExecutionOwner::root("expired");
        let mut expired_feedback = coordinator
            .subscribe_timeout_feedback(&expired_owner)
            .unwrap();
        let waiting_feedback = coordinator
            .subscribe_timeout_feedback(&crate::backend::ExecutionOwner::root("waiting"))
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
        let (sink, mut stream) = crate::backend::events::channel();
        let mut tasks = Vec::new();
        for session in ["expired", "waiting"] {
            let owner = crate::backend::ExecutionOwner::root(session);
            let coordinator = coordinator.clone();
            let sink = sink.clone();
            tasks.push(tokio::spawn(async move {
                coordinator
                    .authorize(
                        ApprovalMode::Ask,
                        PermissionRequest {
                            id: "same-id".into(),
                            provider: "test".into(),
                            reason: None,
                            target_list: vec![PermissionTarget::Command {
                                command: "echo timeout".into(),
                                shell: None,
                            }],
                        },
                        Some(crate::backend::ProviderAddress {
                            owner: Some(owner),
                            thread_id: session.into(),
                            turn_id: "turn".into(),
                        }),
                        Some(&sink),
                    )
                    .await
                    .unwrap()
            }));
        }
        for _ in 0..2 {
            assert_eq!(
                stream.recv().await.unwrap().unwrap().kind,
                "approval_requested"
            );
        }
        let pending = coordinator.pending_list().unwrap();
        let original = pending
            .iter()
            .find(|view| view.execution_owner.as_ref().unwrap().session_id == "expired")
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
        let expired = tokio::time::timeout(std::time::Duration::from_secs(3), stream.recv())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        assert_eq!(expired.data["resolution_source"], "timeout");
        assert!(expired_feedback.has_changed().unwrap());
        assert!(*expired_feedback.borrow_and_update());
        assert!(!waiting_feedback.has_changed().unwrap());
        assert_eq!(tasks.remove(0).await.unwrap(), ApprovalResolution::TimedOut);
        assert_eq!(
            codex_response(
                &json!({"method":"item/commandExecution/requestApproval"}),
                ApprovalResolution::TimedOut
            )["decision"],
            "decline"
        );
        let pending = coordinator.pending_list().unwrap();
        assert_eq!(pending.len(), 1);
        assert!(pending[0].deadline.is_none());
        assert!(
            coordinator
                .has_timeout_feedback(&crate::backend::ExecutionOwner::root("expired"))
                .unwrap()
        );
        assert!(
            !coordinator
                .has_timeout_feedback(&crate::backend::ExecutionOwner::root("waiting"))
                .unwrap()
        );
        assert_eq!(
            std::fs::read(directory.path().join("permissions.json")).unwrap(),
            before
        );
        coordinator
            .cancel_owner(&crate::backend::ExecutionOwner::root("waiting"), None)
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
        let (sink, mut stream) = crate::backend::events::channel();
        let provider = coordinator.clone();
        let waiter = tokio::spawn(async move {
            provider
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "abandoned".into(),
                        provider: "test".into(),
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
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let mut waiter = Vec::new();
        for agent_id in ["first", "second"] {
            let coordinator = coordinator.clone();
            let sink = event_sink.clone();
            let owner = crate::backend::ExecutionOwner {
                session_id: "root".into(),
                agent_id: agent_id.into(),
            };
            waiter.push(tokio::spawn(async move {
                coordinator
                    .authorize(
                        ApprovalMode::Ask,
                        PermissionRequest {
                            id: "same-provider-id".into(),
                            provider: "test".into(),
                            reason: None,
                            target_list: vec![PermissionTarget::Command {
                                command: "probe task".into(),
                                shell: None,
                            }],
                        },
                        Some(crate::backend::ProviderAddress {
                            owner: Some(owner),
                            thread_id: "thread".into(),
                            turn_id: "turn".into(),
                        }),
                        Some(&sink),
                    )
                    .await
                    .unwrap()
            }));
            assert_eq!(
                event_stream.recv().await.unwrap().unwrap().kind,
                "approval_requested"
            );
        }
        assert_eq!(coordinator.pending_list().unwrap().len(), 2);
        coordinator
            .cancel_owner(
                &crate::backend::ExecutionOwner {
                    session_id: "root".into(),
                    agent_id: "first".into(),
                },
                Some(&event_sink),
            )
            .await
            .unwrap();
        assert_eq!(
            event_stream.recv().await.unwrap().unwrap().kind,
            "approval_cancelled"
        );
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
        assert!(!waiter[0].is_finished());
        let pending = coordinator.pending_list().unwrap();
        assert_eq!(pending.len(), 1);
        assert_eq!(
            pending[0].execution_owner.as_ref().unwrap().agent_id,
            "second"
        );
        coordinator
            .cancel_owner(
                &crate::backend::ExecutionOwner {
                    session_id: "root".into(),
                    agent_id: "second".into(),
                },
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiter.remove(0).await.unwrap(), ApprovalResolution::Cancel);
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
    fn unwraps_copilot_permission_notifications_before_matching_policy() {
        let request = permission_from_copilot(
            "shell-one".into(),
            "unknown",
            json!({"permissionRequest": {
                "kind":"shell", "fullCommandText":"Write-Output audit", "intention":"Run audit"
            }, "requestId":"shell-one"}),
            "D:/repo",
        );
        assert_eq!(
            request.target_list,
            vec![PermissionTarget::Command {
                command: "Write-Output audit".into(),
                shell: None
            }]
        );
        assert_eq!(request.reason.as_deref(), Some("Run audit"));
        let mcp = permission_from_copilot(
            "mcp".into(),
            "unknown",
            json!({
                "permissionRequest":{"kind":"mcp", "serverName":"github", "toolName":"search"}
            }),
            "D:/repo",
        );
        assert_eq!(
            mcp.target_list,
            vec![PermissionTarget::Mcp {
                target: "github/search".into()
            }]
        );
        let custom = permission_from_copilot(
            "custom".into(),
            "unknown",
            json!({
                "permissionRequest":{"kind":"custom-tool", "toolName":"question_ask", "toolDescription":"Ask a question"}
            }),
            "D:/repo",
        );
        assert_eq!(
            custom.target_list,
            vec![PermissionTarget::Tool {
                target: "question_ask".into()
            }]
        );
        assert_eq!(custom.reason.as_deref(), Some("Ask a question"));
    }

    #[test]
    fn maps_copilot_permission_kinds_into_shared_targets() {
        let write = permission_from_copilot(
            "write-one".into(),
            "write",
            json!({ "fileName": "D:/repo/src/main.rs", "intention": "Apply edit" }),
            "D:/repo",
        );
        assert_eq!(write.provider, "Copilot CLI");
        assert_eq!(write.reason.as_deref(), Some("Apply edit"));
        assert_eq!(
            write.target_list,
            vec![PermissionTarget::Write {
                path: "D:/repo/src/main.rs".into(),
            }]
        );

        let mcp = permission_from_copilot(
            "mcp-one".into(),
            "mcp",
            json!({ "toolName": "github.search_code" }),
            "D:/repo",
        );
        assert_eq!(
            mcp.target_list,
            vec![PermissionTarget::Mcp {
                target: "github.search_code".into(),
            }]
        );
    }

    #[test]
    fn builds_exact_and_broad_command_choices() {
        let request = PermissionRequest {
            id: "one".into(),
            provider: "test".into(),
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
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator.authorize(ApprovalMode::Ask, PermissionRequest {
                id: "versions".into(), provider: "test".into(), reason: None,
                target_list: vec![PermissionTarget::Command {
                    command: r#""C:/Program Files/PowerShell/7/pwsh.exe" -Command 'Get-Location; Get-ChildItem -Force; rustc --version; cargo --version; rustup show active-toolchain; rustc --version'"#.into(), shell: None
                }],
            }, None, Some(&event_sink)).await.unwrap()
        });
        let event = event_stream.recv().await.unwrap().unwrap();
        let view: ApprovalRequestView = serde_json::from_value(event.data).unwrap();
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
                .resolve("versions", &answer_list[..1], None)
                .await
                .is_err()
        );
        let mut duplicate = answer_list.clone();
        duplicate[2].item_id = "1".into();
        assert!(
            coordinator
                .resolve("versions", &duplicate, None)
                .await
                .is_err()
        );
        let mut unknown = answer_list.clone();
        unknown[1].choice_id = "allow_everything".into();
        assert!(
            coordinator
                .resolve("versions", &unknown, None)
                .await
                .is_err()
        );
        assert_eq!(store.read().unwrap().open().1, original);
        assert_eq!(coordinator.pending_list().unwrap().len(), 1);
        coordinator
            .resolve("versions", &answer_list, None)
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
        let request = PermissionRequest { id:"loop".into(),provider:"test".into(),reason:None,
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
    fn retains_shell_identity_and_argv_script_boundaries() {
        for (executable, script, shell) in [
            (
                "pwsh",
                "1 | ForEach-Object { probe $_ }",
                CommandShell::PowerShell,
            ),
            (
                "/bin/bash",
                "for x in one; do probe $x; done",
                CommandShell::Bash,
            ),
            ("/bin/zsh", "repeat 2 probe x", CommandShell::Zsh),
            ("nu", "[1] | each {|x| probe $x }", CommandShell::Nushell),
        ] {
            let request = permission_from_provider(
                "item/commandExecution/requestApproval",
                &json!({
                    "params": { "args": [executable, "-c", script] }
                }),
                ".",
            )
            .unwrap();
            assert_eq!(
                request.target_list,
                vec![PermissionTarget::Command {
                    command: script.into(),
                    shell: Some(shell)
                }]
            );
            assert!(normalize_command_in(script, shell).failure.is_none());
            let request = permission_from_copilot(
                "id".into(),
                "shell",
                json!({
                    "fullCommandText": script, "shell": executable
                }),
                ".",
            );
            assert_eq!(
                request.target_list,
                vec![PermissionTarget::Command {
                    command: script.into(),
                    shell: Some(shell)
                }]
            );
        }
    }

    #[test]
    fn codex_transport_quoting_preserves_shell_scripts_and_approval_targets() {
        let source =
            r#"rg -n "timeline|markdown|table|wrap" C:\Users\mattm\.codex\memories\MEMORY.md"#;
        let rendered = r#""C:\\Program Files\\PowerShell\\7\\pwsh.exe" -Command "rg -n \"timeline|markdown|table|wrap\" C:\\Users\\mattm\\.codex\\memories\\MEMORY.md""#;
        let request = permission_from_provider(
            "item/commandExecution/requestApproval",
            &json!({"params": {"command": rendered,
                "commandActions": [{"type":"unknown", "command":"misleading summary"}],
                "proposedExecpolicyAmendment": ["unrelated"]}}),
            ".",
        )
        .unwrap();
        assert_eq!(
            request.target_list,
            vec![PermissionTarget::Command {
                command: source.into(),
                shell: Some(CommandShell::PowerShell),
            }]
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
                r"C:\Users\mattm\.codex\memories\MEMORY.md",
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

    #[test]
    fn codex_transport_roundtrips_quoting_for_every_supported_shell() {
        for (executable, shell) in [
            (
                r"C:\Program Files\PowerShell\7\pwsh.exe",
                CommandShell::PowerShell,
            ),
            (
                r"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe",
                CommandShell::PowerShell,
            ),
            ("/usr/bin/pwsh", CommandShell::PowerShell),
            ("/bin/sh", CommandShell::Bash),
            ("/bin/bash", CommandShell::Bash),
            ("/bin/zsh", CommandShell::Zsh),
            ("/opt/homebrew/bin/nu", CommandShell::Nushell),
            (r"C:\Program Files\nu\bin\nu.exe", CommandShell::Nushell),
        ] {
            for source in [
                r#"probe "timeline|markdown|table|wrap""#,
                r#"probe 'one; two & three | four'"#,
                r#"probe "" 'café 世界' 'C:\space path\file.txt'"#,
                "probe first\nprobe second",
                "probe 'quote\" and $value and `tick and \\path'",
            ] {
                let arguments = [executable, "-c", source];
                let rendered = shlex::try_join(arguments).unwrap();
                for params in [json!({"command":rendered}), json!({"args":arguments})] {
                    let request = permission_from_provider(
                        "item/commandExecution/requestApproval",
                        &json!({"params":params}),
                        ".",
                    )
                    .unwrap();
                    assert_eq!(
                        request.target_list,
                        vec![PermissionTarget::Command {
                            command: source.into(),
                            shell: Some(shell),
                        }]
                    );
                    let parsed = normalize_command_in(source, shell);
                    assert!(parsed.failure.is_none(), "{shell:?}: {source}: {parsed:?}");
                    let expected = if source.contains('\n') { 2 } else { 1 };
                    assert_eq!(
                        parsed.invocation_list.len(),
                        expected,
                        "{shell:?}: {source}"
                    );
                    assert!(
                        parsed
                            .invocation_list
                            .iter()
                            .all(|command| command.command_name == "probe")
                    );
                }
            }
        }
    }

    #[test]
    fn provider_argv_preserves_literal_arguments_without_a_shell_launcher() {
        let arguments = ["rg", "-n", "a|b;$(probe)", "", r"C:\space path\file.txt"];
        for params in [
            json!({"command":shlex::try_join(arguments).unwrap()}),
            json!({"command":arguments}),
            json!({"command":"rg", "args":&arguments[1..]}),
        ] {
            let request = permission_from_provider(
                "item/commandExecution/requestApproval",
                &json!({"params":params}),
                ".",
            )
            .unwrap();
            let PermissionTarget::Command { command, shell } = &request.target_list[0] else {
                panic!()
            };
            let parsed = normalize_command_in(command, shell.unwrap());
            assert!(parsed.failure.is_none(), "{command}: {parsed:?}");
            assert_eq!(parsed.invocation_list.len(), 1);
            assert_eq!(parsed.invocation_list[0].token_list, arguments);
        }
    }

    #[test]
    fn invalid_provider_transport_returns_parse_errors() {
        for params in [
            json!({"command":"pwsh -c 'unfinished"}),
            json!({"command":""}),
            json!({"args":[]}),
            json!({"args":["pwsh", "-c", 42]}),
            json!({"command":"rg\u{0}tail"}),
        ] {
            let error = permission_from_provider(
                "item/commandExecution/requestApproval",
                &json!({"params":params}),
                ".",
            )
            .unwrap_err();
            assert!(
                error.downcast_ref::<CommandAnalysisError>().is_some(),
                "{error:#}"
            );
        }
    }

    #[tokio::test]
    async fn allow_callback_does_not_publish_or_persist_an_approval() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let coordinator = PermissionCoordinator::transient(directory.path())?;
        let before = coordinator.store.read().unwrap().open().1;
        let (sink, mut stream) = crate::backend::events::channel();
        let resolution = coordinator
            .authorize(
                ApprovalMode::Allow,
                PermissionRequest {
                    id: "callback".into(),
                    provider: "test".into(),
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
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "cancel-versions".into(),
                        provider: "test".into(),
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
        event_stream.recv().await.unwrap().unwrap();
        coordinator
            .resolve(
                "cancel-versions",
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
            provider: "test".into(),
            reason: Some("inspect repository".into()),
            target_list: vec![PermissionTarget::Command {
                command: "rg TODO src".into(),
                shell: None,
            }],
        };
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(ApprovalMode::Ask, request, None, Some(&waiting_event_sink))
                .await
                .unwrap()
        });
        let event = event_stream.recv().await.unwrap().unwrap();
        assert_eq!(event.kind, "approval_requested");
        let owned = coordinator
            .bind_owner("approval-one", "child-exchange", Some("child-turn"))
            .unwrap()
            .unwrap();
        assert_eq!(owned.exchange_id.as_deref(), Some("child-exchange"));
        let snapshot = coordinator.pending_list().unwrap();
        assert_eq!(snapshot[0].exchange_id, owned.exchange_id);
        assert_eq!(snapshot[0].turn_id.as_deref(), Some("child-turn"));
        assert!(
            coordinator
                .bind_owner("approval-one", "other-exchange", Some("other-turn"))
                .is_err()
        );
        coordinator
            .resolve(
                "approval-one",
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
        assert_eq!(resolved.kind, "approval_resolved");
        let resolved_view: ApprovalRequestView = serde_json::from_value(resolved.data).unwrap();
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
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "approval-deny".into(),
                        provider: "test".into(),
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
        assert_eq!(
            event_stream.recv().await.unwrap().unwrap().kind,
            "approval_requested"
        );
        coordinator
            .resolve(
                "approval-deny",
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
        let (event_sink, mut event_stream) = crate::backend::events::channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "approval-cancel".into(),
                        provider: "test".into(),
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
        assert_eq!(
            event_stream.recv().await.unwrap().unwrap().kind,
            "approval_requested"
        );
        coordinator.cancel_all(Some(&event_sink)).await.unwrap();
        assert_eq!(
            event_stream.recv().await.unwrap().unwrap().kind,
            "approval_cancelled"
        );
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
                provider: "test".into(),
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
    fn spaced_powershell_arrays_survive_provider_approval_transport() {
        let source = "Get-Content Cargo.toml, .forge.json, .gitignore";
        let captured = r#""C:\Program Files\PowerShell\7\pwsh.exe" -Command 'Get-Content Cargo.toml, .forge.json, .gitignore'"#;
        let requests = [
            permission_from_provider(
                "item/commandExecution/requestApproval",
                &json!({"params":{"command":captured}}),
                ".",
            )
            .unwrap(),
            permission_from_provider(
                "item/commandExecution/requestApproval",
                &json!({"params":{"args":["pwsh", "-Command", source]}}),
                ".",
            )
            .unwrap(),
            permission_from_copilot(
                "spaced-array".into(),
                "shell",
                json!({"fullCommandText":source,"shell":"pwsh"}),
                ".",
            ),
        ];
        for request in requests {
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
            provider: "test".into(),
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
