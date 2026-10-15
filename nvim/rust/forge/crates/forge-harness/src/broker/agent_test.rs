fn save_child_profile(broker: &HarnessBroker, name: &str, profile: crate::agent::AgentProfile) -> Result<()> {
    let snapshot = broker.agent_backend_factory.profiles.snapshot()?;
    let mut document = snapshot.document;
    document.profiles.insert(name.into(),profile);
    broker.agent_backend_factory.profiles.save(&snapshot.revision,document)?;
    Ok(())
}

struct RoutingBackend {
    kind:crate::backend::BackendKind,
    turn:std::sync::atomic::AtomicUsize,
    request:std::sync::Mutex<Vec<BackendRequest>>,
    reject_steering:bool,
}

#[tokio::test]
async fn profile_selection_captures_tuning_and_instructions_without_widening_permissions() -> Result<()> {
    use crate::agent::{AgentProfile, SpawnAgentRequest};
    use crate::backend::BackendKind;
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.agent_backend_factory.register(Arc::new(RoutingBackend { kind:BackendKind::Codex,
        turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false }));
    let mut root = broker.backend_request(BackendInput::from_text("parent secret"),PromptMode::Chat);
    root.model = "parent-model".into();
    root.effort = "high".into();
    root.execution_mode = PermissionMode::Write;
    let inherited = broker.agent_backend_factory.resolve(&root,BackendKind::Mock,
        &SpawnAgentRequest { profile:None,text:"task".into() }).await?;
    assert_eq!(inherited.model, "parent-model");
    assert_eq!(inherited.reasoning_effort, "high");
    save_child_profile(&broker,"review",AgentProfile { provider:"codex".into(),model:"default".into(),
        reasoning_effort:"default".into(),service_tier:"default".into(),permission_mode:"read".into(),
        instructions:"First instructions".into(),..Default::default() })?;
    let spawn = SpawnAgentRequest { profile:Some("review".into()),text:"task".into() };
    let selected = broker.agent_backend_factory.resolve(&root,BackendKind::Mock,&spawn).await?;
    assert_eq!(selected.provider,BackendKind::Codex);
    assert_eq!(selected.model,"fixture-model");
    assert_eq!(selected.reasoning_effort,"low");
    assert_eq!(selected.execution_mode,PermissionMode::Read);
    let mut child = crate::agent::Agent::pending(&root.harness_session_id,"review","task",1);
    child.selection = Some(selected);
    save_child_profile(&broker,"review",AgentProfile { instructions:"Changed instructions".into(),..Default::default() })?;
    root.model = "changed-parent-model".into();
    root.execution_mode = PermissionMode::Yolo;
    let request = broker.agent_backend_factory.request(&root,&child,"follow-up".into())?;
    assert_eq!(request.model,"fixture-model");
    assert_eq!(request.execution_mode,PermissionMode::Read);
    assert!(matches!(request.input,BackendInput::Text {text} if text.contains("First instructions")
        && !text.contains("Changed instructions") && !text.contains("parent secret")));
    assert!(request.backend_session_id.is_none());
    assert!(request.control_context.is_none());
    let next = broker.agent_backend_factory.resolve(&root,BackendKind::Mock,&spawn).await?;
    assert_eq!(next.model,"changed-parent-model");
    assert_eq!(next.instructions,"Changed instructions");
    save_child_profile(&broker,"review",AgentProfile { provider:"codex".into(),..Default::default() })?;
    assert!(broker.agent_backend_factory.resolve(&root,BackendKind::Mock,&spawn).await.is_err(),
        "a foreign provider must reject an inherited unavailable model");
    root.execution_mode = PermissionMode::Read;
    child.selection.as_mut().unwrap().execution_mode = PermissionMode::Yolo;
    assert_eq!(broker.agent_backend_factory.request(&root,&child,"task".into())?.execution_mode,PermissionMode::Read);
    Ok(())
}

fn admit_child_fixture(broker: &mut HarnessBroker, name: &str) -> Result<crate::agent::AgentAdmission> {
    let mut parent = Exchange::agent_admission(&broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,"Admission".into(),100);
    let spawn = crate::agent::SpawnAgentRequest { profile: Some(name.into()), text: "Inspect".into() };
    let request = crate::agent::AgentRequest {operation_id:name.into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Spawn(spawn.clone())};
    let selection = crate::agent::AgentSelection { profile: "worker".into(), instructions: String::new(), provider:crate::backend::BackendKind::Mock,
        model:"mock".into(),reasoning_effort:"low".into(),context_window:None,service_tier:Default::default(),
        access:broker.session.access.clone(),execution_mode:broker.session.execution_mode};
    let admission = broker.agent_coordinator.reserve_spawn(&request,&spawn,selection,&parent.id,100)?;
    parent.append_agent_task(&admission.task)?;
    broker.store.admit_agent_task(&admission.agent,&admission.task,&parent)?;
    broker.agent_coordinator.commit(&admission)?;
    broker.agent_registry.insert(admission.agent.clone());
    Ok(admission)
}

fn child_event(kind: &str) -> BackendEvent {
    BackendEvent {received_at_ms:None,address:Some(crate::backend::ProviderAddress {owner:None,
        thread_id:"durable-child-conversation".into(),turn_id:"child-turn".into()}),
        turn_boundary:(kind == "turn_started").then_some(crate::backend::TurnBoundary::Started),
        kind:kind.into(),text:None,data:Value::Null,activity:None,summary:None,task_update:None}
}

async fn query_child(broker: &mut HarnessBroker, operation: crate::agent::AgentOperation) -> Result<Value> {
    broker.handle_agent_request(crate::agent::AgentRequest {operation_id:Uuid::new_v4().to_string(),
        source:crate::agent::AgentSource::User,operation},None,&mut Vec::new()).await
}

#[tokio::test]
async fn agent_activity_omits_output_and_pages_stable_owned_calls_across_followups() -> Result<()> {
    use crate::agent::AgentOperation;
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"inspection")?;
    let sibling = admit_child_fixture(&mut broker,"sibling-inspection")?;
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    broker.observe_managed_agent(owner.clone(),&admission.task.id,child_event("turn_started"),&mut Vec::new()).await?;
    for ordinal in 0..6 {
        let mut thought = child_event("assistant_message");
        thought.data = json!({"provider_message_id":format!("thought-{ordinal}"),"phase":"commentary","message_update":"snapshot"});
        thought.text = Some(if ordinal == 5 {"思".repeat(4001)} else {format!("Thought {ordinal}")});
        broker.observe_managed_agent(owner.clone(),&admission.task.id,thought,&mut Vec::new()).await?;
    }
    let source = format!("{}\0tail", "output-é😀".repeat(9000));
    let mut update = child_event("tool");
    update.activity = Some(serde_json::from_value(json!({"id":"same-provider-call","kind":"command",
        "title":"Inspect compilation diagnostics","status":"inProgress","output":source}))?);
    broker.observe_managed_agent(owner.clone(),&admission.task.id,update.clone(),&mut Vec::new()).await?;
    let activity = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:None}).await?;
    assert_eq!(activity["entries"].as_array().unwrap().len(),5);
    assert_eq!(activity["has_more"],true);
    let thought = &activity["entries"][3];
    assert_eq!(thought["text"].as_str().unwrap().chars().count(),4000);
    assert_eq!(thought["truncated"],true);
    let tool = &activity["entries"][4];
    assert_eq!(tool["status"],"running");
    assert!(tool.get("output").is_none());
    assert!(!activity.to_string().contains("output-é"));
    let call_ref = tool["call_ref"].as_str().unwrap().to_owned();
    let sibling_owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:sibling.agent.id.clone()};
    broker.observe_managed_agent(sibling_owner.clone(),&sibling.task.id,child_event("turn_started"),&mut Vec::new()).await?;
    let mut sibling_tool = update.clone();
    sibling_tool.activity.as_mut().unwrap().output = Some("SIBLING OUTPUT".into());
    broker.observe_managed_agent(sibling_owner,&sibling.task.id,sibling_tool,&mut Vec::new()).await?;
    let sibling_activity = query_child(&mut broker,AgentOperation::Activity {target:sibling.agent.id.clone(),limit:Some(1)}).await?;
    let sibling_ref = sibling_activity["entries"][0]["call_ref"].as_str().unwrap().to_owned();
    assert_ne!(sibling_ref,call_ref,"identical native call IDs must remain child-scoped");
    let sibling_page = query_child(&mut broker,AgentOperation::ToolOutput {call_ref:sibling_ref,offset:0,limit:8000}).await?;
    assert_eq!(sibling_page["output"],"SIBLING OUTPUT");
    let page = query_child(&mut broker,AgentOperation::ToolOutput {call_ref:call_ref.clone(),offset:65530,limit:17}).await?;
    assert_eq!(page["output"],source.chars().skip(65530).take(17).collect::<String>());
    assert_eq!(page["next_offset"],65547);
    assert_eq!(page["total_chars"],source.chars().count());
    let tail = query_child(&mut broker,AgentOperation::ToolOutput {call_ref:call_ref.clone(),offset:source.chars().count()-5,limit:10}).await?;
    assert_eq!(tail["output"],"\0tail");
    assert!(tail["next_offset"].is_null());
    update.activity.as_mut().unwrap().output = Some("APPENDED".into());
    update.activity.as_mut().unwrap().output_delta = true;
    broker.observe_managed_agent(owner.clone(),&admission.task.id,update.clone(),&mut Vec::new()).await?;
    let appended = query_child(&mut broker,AgentOperation::ToolOutput {call_ref:call_ref.clone(),offset:source.chars().count(),limit:8000}).await?;
    assert_eq!(appended["output"],"APPENDED");
    assert_eq!(appended["status"],"running");
    assert!(appended["next_offset"].is_null());
    let last_activity = broker.agent_coordinator.task(&admission.task.id).unwrap().last_activity_at_ms;
    let repeated = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(1)}).await?;
    assert_eq!(repeated["last_activity_at_ms"],json!(last_activity),"inspection must not count as child activity");

    let compact = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(2)}).await?;
    assert_eq!(compact["entries"].as_array().unwrap().len(),2);
    let expanded = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(20)}).await?;
    assert_eq!(expanded["entries"].as_array().unwrap().len(),7);
    assert_eq!(expanded["has_more"],false);
    let default = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:None}).await?;
    assert_eq!(default["entries"].as_array().unwrap().len(),5);
    assert!(query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(21)}).await.is_err());
    assert!(query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(0)}).await.is_err());

    let followup_request = crate::agent::AgentRequest {operation_id:"inspection-followup".into(),source:crate::agent::AgentSource::User,
        operation:AgentOperation::Followup {target:admission.agent.id.clone(),message:"Next task".into()}};
    let followup = broker.agent_coordinator.reserve_followup(&followup_request,&admission.agent.id,"Next task",&admission.task.parent_exchange_id,101)?;
    let mut parent = broker.store.load_exchange(&admission.task.parent_exchange_id)?.unwrap();
    parent.append_agent_task(&followup.task)?;
    broker.store.admit_agent_task(&followup.agent,&followup.task,&parent)?;
    broker.agent_coordinator.commit(&followup)?;
    let active = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(1)}).await?;
    assert_eq!(active["task_id"],admission.task.id,"queued follow-up must not hide active progress");

    update.activity.as_mut().unwrap().status = Some("failed".into());
    update.activity.as_mut().unwrap().output_delta = false;
    update.activity.as_mut().unwrap().output = Some("failure details".into());
    broker.observe_managed_agent(owner.clone(),&admission.task.id,update.clone(),&mut Vec::new()).await?;
    let mut final_message = child_event("assistant_message");
    final_message.data = json!({"provider_message_id":"final","phase":"final_answer","message_update":"snapshot"});
    final_message.text = Some("FINAL ANSWER MUST NOT ENTER ACTIVITY".into());
    broker.observe_managed_agent(owner.clone(),&admission.task.id,final_message,&mut Vec::new()).await?;
    let before_completion = query_child(&mut broker,AgentOperation::Activity {target:admission.agent.id.clone(),limit:Some(20)}).await?;
    assert!(!before_completion.to_string().contains("FINAL ANSWER MUST NOT ENTER ACTIVITY"));
    broker.complete_managed_agent(owner.clone(),admission.task.id.clone(),Err(anyhow::anyhow!("failed")),None,&mut Vec::new()).await?;
    let old_page = query_child(&mut broker,AgentOperation::ToolOutput {call_ref:call_ref.clone(),offset:0,limit:8000}).await?;
    assert_eq!(old_page["status"],"failed");
    assert_eq!(old_page["failed"],true);
    assert_eq!(old_page["output"],"failure details");

    let foreign = crate::agent::AgentToolReference {session_id:"other-session".into(),agent_id:admission.agent.id.clone(),
        exchange_id:admission.task.id.clone(),turn_id:"child-turn".into(),tool_id:"same-provider-call".into()};
    let forged = format!("agent-tool:{}",hex::encode(serde_json::to_vec(&foreign)?));
    assert!(query_child(&mut broker,AgentOperation::ToolOutput {call_ref:forged,offset:0,limit:8}).await.is_err());
    let crossed = crate::agent::AgentToolReference {session_id:broker.session.id.clone(),agent_id:sibling.agent.id.clone(),..foreign};
    let forged = format!("agent-tool:{}",hex::encode(serde_json::to_vec(&crossed)?));
    assert!(query_child(&mut broker,AgentOperation::ToolOutput {call_ref:forged,offset:0,limit:8}).await.is_err());
    assert!(query_child(&mut broker,AgentOperation::ToolOutput {call_ref,offset:999999,limit:8}).await.is_err());
    Ok(())
}

#[cfg(windows)]
#[tokio::test]
async fn child_execution_blocks_automatic_verification_without_mutating_the_goal() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = check_fixture(repository.path(),data.path(),vec!["Write-Output verified".into()],false);
    let admission = admit_child_fixture(&mut broker,"verification-child")?;
    assert!(broker.run_checks().await.unwrap_err().to_string().contains("child execution"));
    let execution = broker.store.load_plan_execution("check-execution")?.unwrap();
    assert!(execution.check_runs.is_empty());
    assert_eq!(execution.state,PlanExecutionState::Active);
    assert_eq!(broker.active_goal()?.state,GoalState::Active);
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id};
    broker.complete_managed_agent(owner,admission.task.id,Err(anyhow::anyhow!("stopped")),None,&mut Vec::new()).await?;
    broker.run_checks().await?;
    assert_eq!(broker.active_goal()?.state,GoalState::Complete);
    Ok(())
}

#[tokio::test]
async fn child_summary_preserves_running_exchange_when_followup_is_queued() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"queued-followup")?;
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    broker.observe_managed_agent(owner,&admission.task.id,child_event("turn_started"),&mut Vec::new()).await?;
    let request = crate::agent::AgentRequest {operation_id:"queued".into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Followup {target:admission.agent.id.clone(),message:"Next".into()}};
    let queued = broker.agent_coordinator.reserve_followup(&request,&admission.agent.id,"Next",&admission.task.parent_exchange_id,101)?;
    let mut parent = broker.store.load_exchange(&admission.task.parent_exchange_id)?.unwrap();
    parent.append_agent_task(&queued.task)?;
    broker.store.admit_agent_task(&queued.agent,&queued.task,&parent)?;
    broker.agent_coordinator.commit(&queued)?;
    let snapshot = broker.snapshot()?;
    let summary = &snapshot.agent.summary[&admission.agent.id];
    assert_eq!(summary["state"],"running");
    assert_eq!(summary["latest_exchange_id"],admission.task.id);
    assert_eq!(summary["target"]["turn_id"],"child-turn");
    assert_eq!(summary["exchange_count"],2);
    Ok(())
}

#[tokio::test]
async fn child_tool_configuration_resumes_the_same_goal_exchange() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    broker.backend = broker.agent_backend_factory.build(crate::backend::BackendKind::Mock)?;
    let child = admit_child_fixture(&mut broker,"config-child")?;
    let goal = GoalRecord {id:"configuration-goal".into(),session_id:broker.session.id.clone(),
        objective:"Continue work".into(),state:GoalState::Active,continuation:Default::default(),native:false,
        created_at_ms:1,updated_at_ms:1};
    broker.store.save_goal(&goal)?;
    let mut task = crate::task::TaskRecord::new(broker.session.id.clone(),goal.objective.clone(),
        crate::task::TaskKind::Goal,broker.session.execution_mode,1);
    task.goal_id = Some(goal.id.clone());
    task.begin(1)?;
    broker.store.save_task(&task)?;
    broker.session.current_task_id = Some(task.id.clone());
    broker.session.goal_id = Some(goal.id.clone());
    broker.save_session()?;
    let mut parent = broker.store.load_exchange(&child.task.parent_exchange_id)?.unwrap();
    parent.kind = ExchangeKind::Chat;
    parent.state = ExchangeState::Running;
    parent.goal_id = Some(goal.id.clone());
    parent.execution_started_at_ms = None;
    broker.store.save_exchange(&parent)?;
    broker.transition_task(json!({"action":"configure","resume":true,"config":{"subagent_tools":true}})).await?;
    assert_eq!(broker.store.list_exchange(&broker.session.id)?.last().unwrap().id,parent.id);
    assert_eq!(broker.current_task()?.unwrap().status,crate::task::TaskStatus::Running);
    assert_eq!(broker.active_goal()?.state,GoalState::Active);
    assert_eq!(broker.agent_coordinator.active_count(),1);
    assert!(broker.agent_coordinator.enabled());
    Ok(())
}

#[tokio::test]
async fn child_approval_events_reach_the_wire_and_interrupt_preserves_sibling_approval() -> Result<()> {
    use crate::permissions::matcher::{PermissionRequest,PermissionTarget};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let first = admit_child_fixture(&mut broker,"approval-first")?;
    let sibling = admit_child_fixture(&mut broker,"approval-sibling")?;
    let (sink,mut stream) = crate::backend::events::channel();
    let mut waiters = Vec::new();
    let mut event = Vec::new();
    for admission in [&first,&sibling] {
        let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
        broker.observe_managed_agent(owner.clone(),&admission.task.id,child_event("turn_started"),&mut event).await?;
        let permission = broker.permission_coordinator.clone();
        let sink = sink.clone();
        waiters.push(tokio::spawn(async move {
            permission.authorize_callback(PermissionMode::Read,true,PermissionRequest {id:"approval".into(),provider:"Codex".into(),
                reason:None,target_list:vec![PermissionTarget::Command {command:"probe task".into(),shell:None}]},
                Some(crate::backend::ProviderAddress {owner:Some(owner),thread_id:"durable-child-conversation".into(),turn_id:"child-turn".into()}),Some(&sink)).await
        }));
        let requested = tokio::time::timeout(std::time::Duration::from_secs(2),stream.recv()).await??
            .context("approval event missing")?;
        let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
        broker.observe_managed_agent(owner,&admission.task.id,requested,&mut event).await?;
        let activity = query_child(&mut broker,crate::agent::AgentOperation::Activity {
            target:admission.agent.id.clone(),limit:None}).await?;
        assert_eq!(activity["awaiting_approval"],true);
        assert!(event.iter().any(|event|event.payload["kind"] == "approval_requested"
            && event.payload["data"]["exchange_id"] == admission.task.id
            && event.payload["data"]["execution_owner"]["agent_id"] == admission.agent.id));
    }
    broker.interrupt_managed_agent(&first.agent.id,&mut event).await?;
    assert_eq!(waiters.remove(0).await??,crate::backend::approval::ApprovalResolution::Cancel);
    assert!(!waiters[0].is_finished());
    assert_eq!(broker.permission_coordinator.pending_list()?.len(),1);
    let first_activity = query_child(&mut broker,crate::agent::AgentOperation::Activity {target:first.agent.id.clone(),limit:None}).await?;
    let sibling_activity = query_child(&mut broker,crate::agent::AgentOperation::Activity {target:sibling.agent.id.clone(),limit:None}).await?;
    assert_eq!(first_activity["awaiting_approval"],false);
    assert_eq!(sibling_activity["awaiting_approval"],true);
    assert!(event.iter().any(|event|event.payload["kind"] == "approval_cancelled"
        && event.payload["data"]["execution_owner"]["agent_id"] == first.agent.id));
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:sibling.agent.id.clone()};
    broker.complete_managed_agent(owner,sibling.task.id.clone(),Err(anyhow::anyhow!("provider failed")),None,&mut event).await?;
    assert_eq!(waiters.remove(0).await??,crate::backend::approval::ApprovalResolution::Cancel);
    assert!(broker.permission_coordinator.pending_list()?.is_empty());
    assert!(event.iter().any(|event|event.payload["kind"] == "agent_updated"
        && event.payload["data"]["summary"][&sibling.agent.id]["state"] == "failed"));
    Ok(())
}

#[tokio::test]
async fn interrupted_child_retains_conversation_and_all_children_guard_workspace_rollback() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"retained")?;
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    let mut event = Vec::new();
    broker.observe_managed_agent(owner.clone(),&admission.task.id,child_event("turn_started"),&mut event).await?;
    assert!(broker.prepare_rollback(json!({"exchange_id":"unrelated"})).await.unwrap_err().to_string().contains("child execution"));
    assert!(broker.apply_rollback(json!({"preview_id":"unrelated"})).await.unwrap_err().to_string().contains("child execution"));
    broker.interrupt_managed_agent(&admission.agent.id,&mut event).await?;
    broker.complete_managed_agent(owner,admission.task.id.clone(),Err(anyhow::anyhow!("interrupted")),None,&mut event).await?;
    let retained = broker.agent_coordinator.resolve(&admission.agent.id)?;
    assert_eq!(retained.provider_thread_id.as_deref(),Some("durable-child-conversation"));
    let request = broker.agent_backend_factory.request(&broker.backend_request(BackendInput::from_text(""),PromptMode::Chat),&retained,"Follow-up".into())?;
    assert_eq!(request.backend_session_id,retained.provider_thread_id);
    broker.store.interrupt_detached_execution(&broker.session.id)?;
    assert_eq!(broker.store.list_agent_run(&broker.session.id)?.into_iter().find(|agent|agent.id == retained.id).unwrap().provider_thread_id,retained.provider_thread_id);
    Ok(())
}

#[tokio::test]
async fn child_admission_and_followup_disable_unsupported_root_sandbox() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"policy")?;
    let mut root = broker.backend_request(BackendInput::from_text(""),PromptMode::Chat);
    root.access.sandbox = true;
    assert!(root.access.sandbox);
    for provider in [crate::backend::BackendKind::Copilot,crate::backend::BackendKind::Claude,crate::backend::BackendKind::Antigravity] {
        broker.agent_backend_factory.register(Arc::new(RoutingBackend {kind:provider,
            turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false}));
        save_child_profile(&broker, "unsandboxed", crate::agent::AgentProfile {
            provider:crate::agent::provider_name(provider).into(),model:"fixture-model".into(),
            reasoning_effort:"low".into(),..Default::default()
        })?;
        let spawn = crate::agent::SpawnAgentRequest { profile: Some("unsandboxed".into()), text: "Inspect".into() };
        let selected = broker.agent_backend_factory.resolve(&root,crate::backend::BackendKind::Codex,&spawn).await?;
        assert!(!selected.access.sandbox);
        let mut agent = admission.agent.clone();
        agent.selection.as_mut().unwrap().provider = provider;
        assert!(!broker.agent_backend_factory.request(&root,&agent,"Follow-up".into())?.access.sandbox);
        assert!(root.access.sandbox);
    }
    Ok(())
}

#[tokio::test]
async fn managed_child_replays_and_late_failures_preserve_sibling_and_followup_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let mut parent = Exchange::agent_admission(&broker.session.id,1,"Admission".into(),100);
    let selection = crate::agent::AgentSelection { profile: "worker".into(), instructions: String::new(),
        provider:crate::backend::BackendKind::Mock,model:"mock-model".into(),reasoning_effort:"low".into(),
        context_window:None,service_tier:Default::default(),access:broker.session.access.clone(),
        execution_mode:broker.session.execution_mode,
    };
    let mut admission = Vec::new();
    for name in ["first","sibling"] {
        let spawn = crate::agent::SpawnAgentRequest { profile: Some(name.into()), text: "Inspect".into() };
        let request = crate::agent::AgentRequest {operation_id:name.into(),source:crate::agent::AgentSource::User,
            operation:crate::agent::AgentOperation::Spawn(spawn.clone())};
        let admitted = broker.agent_coordinator.reserve_spawn(&request,&spawn,selection.clone(),&parent.id,100)?;
        parent.append_agent_task(&admitted.task)?;
        broker.store.admit_agent_task(&admitted.agent,&admitted.task,&parent)?;
        broker.agent_coordinator.commit(&admitted)?;
        broker.agent_registry.insert(admitted.agent.clone());
        admission.push(admitted);
    }
    let mut event = Vec::new();
    let mut update = BackendEvent {received_at_ms:None,
        address:Some(crate::backend::ProviderAddress {owner:None,thread_id:"shared-native-id".into(),turn_id:"turn".into()}),
        turn_boundary:Some(crate::backend::TurnBoundary::Started),kind:"turn_started".into(),text:None,
        data:Value::Null,activity:None,summary:None,task_update:None};
    for admitted in &admission {
        let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admitted.agent.id.clone()};
        broker.observe_managed_agent(owner,&admitted.task.id,update.clone(),&mut event).await?;
    }
    let first = &admission[0];
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:first.agent.id.clone()};
    update.turn_boundary = None;
    update.kind = "assistant_message".into();
    update.data = json!({"provider_message_id":"thought","phase":"commentary","message_update":"snapshot"});
    update.text = Some("First child thought".into());
    broker.observe_managed_agent(owner.clone(),&first.task.id,update.clone(),&mut event).await?;
    broker.observe_managed_agent(owner.clone(),&first.task.id,update.clone(),&mut event).await?;
    let retained = broker.store.load_exchange(&first.task.id)?.unwrap();
    assert_eq!(retained.turn.iter().flat_map(|turn|turn.messages()).count(),1);
    assert_eq!(retained.turn[0].messages()[0].text(),"First child thought");
    assert!(broker.store.load_exchange(&admission[1].task.id)?.unwrap().turn[0].messages().is_empty());
    let mut foreign = update.clone();
    foreign.address.as_mut().unwrap().owner = Some(crate::backend::ExecutionOwner {
        session_id:broker.session.id.clone(),agent_id:admission[1].agent.id.clone()});
    assert!(broker.observe_managed_agent(owner.clone(),&first.task.id,foreign,&mut event).await.is_err());
    broker.child_backend_by_agent.insert(first.agent.id.clone(),Arc::new(RoutingBackend {
        kind:crate::backend::BackendKind::Mock,turn:std::sync::atomic::AtomicUsize::new(0),
        request:Default::default(),reject_steering:true}));
    let message = broker.handle_agent_request(crate::agent::AgentRequest {
        operation_id:"steering-race".into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Message {target:first.agent.id.clone(),message:"Retain after rejected steering".into()},
    },None,&mut event).await?;
    assert_eq!(message["delivery"],"queued");
    assert_eq!(broker.agent_coordinator.resolve(&first.agent.id)?.inbox[0].message,"Retain after rejected steering");
    update.data = json!({"provider_message_id":"final","phase":"final_answer","message_update":"snapshot"});
    update.text = Some("Child final response".into());
    broker.observe_managed_agent(owner.clone(),&first.task.id,update.clone(),&mut event).await?;
    broker.complete_managed_agent(owner.clone(),first.task.id.clone(),Err(anyhow::anyhow!("provider failed")),None,&mut event).await?;
    assert_eq!(broker.agent_coordinator.task(&first.task.id).unwrap().final_response.as_deref(),Some("Child final response"));
    let request = crate::agent::AgentRequest {operation_id:"followup-event-test".into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Followup {target:first.agent.id.clone(),message:"Follow-up".into()}};
    let next = broker.agent_coordinator.reserve_followup(&request,&first.agent.id,"Follow-up",&parent.id,101)?;
    parent.append_agent_task(&next.task)?;
    broker.store.admit_agent_task(&next.agent,&next.task,&parent)?;
    broker.agent_coordinator.commit(&next)?;
    update.turn_boundary = Some(crate::backend::TurnBoundary::Started);
    update.text = None;
    broker.observe_managed_agent(owner.clone(),&next.task.id,update.clone(),&mut event).await?;
    update.turn_boundary = None;
    update.text = Some("New task thought".into());
    broker.observe_managed_agent(owner.clone(),&next.task.id,update.clone(),&mut event).await?;
    assert_eq!(broker.store.load_exchange(&next.task.id)?.unwrap().turn[0].messages()[0].text(),"New task thought");
    let saved_next = serde_json::to_value(broker.store.load_exchange(&next.task.id)?)?;
    update.text = Some("LATE OLD TASK".into());
    broker.observe_managed_agent(owner.clone(),&first.task.id,update,&mut event).await?;
    broker.complete_managed_agent(owner,first.task.id.clone(),Err(anyhow::anyhow!("duplicate failure")),None,&mut event).await?;
    assert_eq!(serde_json::to_value(broker.store.load_exchange(&next.task.id)?)?,saved_next);
    assert_eq!(broker.child_exchange_runtime_by_agent[&first.agent.id].exchange.id,next.task.id);
    assert_eq!(broker.agent_coordinator.task(&first.task.id).unwrap().error.as_deref(),Some("provider failed"));
    assert_eq!(broker.agent_coordinator.active_count(),2);
    assert!(broker.store.list_agent_exchange(&broker.session.primary_agent_id)?.iter().all(|exchange|exchange.turn.is_empty()));
    Ok(())
}

#[async_trait::async_trait]
impl Backend for RoutingBackend {
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        crate::backend::BackendDescriptor {kind:self.kind,label:crate::agent::provider_name(self.kind).into(),
            capability:crate::backend::mock_capability()}
    }
    async fn shutdown(&self) -> Result<()> {Ok(())}
    async fn has_active_turn(&self,_session_id:&str) -> bool {self.reject_steering}
    async fn steer_session(&self,_session_id:&str,_input:crate::backend::SteeringInput) -> Result<()> {Err(anyhow::anyhow!("steering was not accepted"))}
    async fn cancel_session(&self,_session_id:&str) -> Result<()> {Ok(())}
    async fn cleanup_execution(&self,_session_id:&str) -> Result<()> {Ok(())}
    async fn active_session_id_for(&self,_session_id:&str) -> Option<String> {None}
    async fn model_list(&self,_request:BackendRequest) -> Result<Vec<crate::backend::BackendModel>> {
        Ok(vec![serde_json::from_value(json!({"id":"fixture-model","reasoning":["low"],"default_reasoning":"low",
            "selected_reasoning":null,"context_window":[{"id":"extended","token_limit":200000}],"default_context_window":null,"selected_context_window":null,
            "vision":false,"description":null,"is_default":true}))?])
    }
    async fn prompt_stream(&self,request:BackendRequest,sink:Option<BackendEventSink>) -> Result<crate::backend::BackendOutput> {
        let ordinal=self.turn.fetch_add(1,Ordering::AcqRel);
        let response = if request.owner()?.is_root() {
            if ordinal == 0 {
                let scope=request.control_context.as_ref().and_then(|context|context.agent.as_ref()).context("missing root authority")?;
                scope.invoke("harness_spawn_agent",json!({"profile":"routing-child","text":"CHILD_RESULT"})).await?;
                "Waiting"
            } else {
                anyhow::ensure!(ordinal == 1,"root continuation duplicated");
                anyhow::ensure!(request.backend_session_id.as_deref() == Some("root-history"),"root history changed");
                anyhow::ensure!(matches!(&request.input,BackendInput::Text {text} if text.contains("CHILD_RESULT")),"missing child result");
                "SYNTHESIZED_RESULT"
            }
        } else {
            anyhow::ensure!(request.control_context.is_none(),"child inherited root workflow authority");
            anyhow::ensure!(request.backend_session_id.is_none(),"fresh child inherited provider history");
            anyhow::ensure!(matches!(&request.input,BackendInput::Text {text} if text.ends_with("CHILD_RESULT")),"child inherited parent transcript");
            self.request.lock().unwrap().push(request.clone());
            "CHILD_RESULT"
        };
        if let Some(sink)=sink {
            sink.send_wait(BackendEvent {received_at_ms:None,address:None,turn_boundary:None,kind:"assistant_message".into(),
                text:Some(response.into()),data:Value::Null,activity:None,summary:None,task_update:None}).await?;
        }
        Ok(crate::backend::BackendOutput {backend_session_id:Some(if request.owner()?.is_root() {"root-history"} else {"child-history"}.into()),
            capability:self.descriptor().capability,..Default::default()})
    }
}

#[tokio::test]
async fn cross_provider_children_resolve_profile_model_reasoning_and_service_tier() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    broker.agent_backend_factory.register(Arc::new(RoutingBackend {kind:crate::backend::BackendKind::Codex,
        turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false}));
    save_child_profile(&broker,"configured-child",crate::agent::AgentProfile {
        provider:"codex".into(),model:"fixture-model".into(),reasoning_effort:"low".into(),
        service_tier:"fast".into(),..Default::default()
    })?;
    let root = broker.backend_request(BackendInput::from_text(""),PromptMode::Chat);
    let spawn = crate::agent::SpawnAgentRequest { profile: Some("configured-child".into()), text: "Inspect".into() };
    let selection = broker.agent_backend_factory.resolve(&root,crate::backend::BackendKind::Mock,&spawn).await?;
    assert_eq!(selection.model,"fixture-model");
    assert_eq!(selection.reasoning_effort,"low");
    assert_eq!(selection.context_window,None);
    assert_eq!(selection.service_tier,crate::backend::ServiceTier::Fast);
    assert_eq!(selection.access.sandbox,root.access.sandbox);
    assert_eq!(selection.execution_mode,root.execution_mode);
    Ok(())
}

#[tokio::test]
async fn every_provider_pairing_preserves_child_history_and_root_continuation_ownership() -> Result<()> {
    use crate::backend::BackendKind;
    let provider=[BackendKind::Codex,BackendKind::Copilot,BackendKind::Claude,BackendKind::Antigravity];
    for root_provider in provider {
        for child_provider in provider {
            let repository=repository();
            let data=tempfile::tempdir()?;
            let mut broker=planning_question_broker(repository.path(),data.path(),false);
            broker.session.backend=crate::agent::provider_name(root_provider).into();
            broker.session.access.sandbox=false;
            broker.agent_coordinator.set_enabled(true);
            let root=Arc::new(RoutingBackend {kind:root_provider,
                turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false});
            let child=Arc::new(RoutingBackend {kind:child_provider,turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false});
            save_child_profile(&broker,"routing-child",crate::agent::AgentProfile {
                provider:crate::agent::provider_name(child_provider).into(),model:"fixture-model".into(),
                reasoning_effort:"default".into(),service_tier:"default".into(),..Default::default()
            })?;
            broker.backend=root.clone();
            broker.agent_backend_factory.register(child.clone());
            tokio::time::timeout(std::time::Duration::from_secs(5),broker.run_interaction(
                "PARENT_PRIVATE_HISTORY".into(),PromptMode::Chat,Some(ExchangeAdmission::chat("PARENT_PRIVATE_HISTORY".into()))))
                .await.context("provider routing did not settle within five seconds")??;
            let task=broker.store.list_agent_task(&broker.session.id)?;
            assert_eq!(task.len(),1,"{root_provider:?} -> {child_provider:?}");
            assert_eq!(task[0].state,ExchangeState::Complete);
            assert!(task[0].parent_reported);
            let request=child.request.lock().unwrap();
            assert_eq!(request.len(),1);
            assert_eq!(request[0].harness_session_id,broker.session.id);
            assert_ne!(request[0].runtime_key()?,broker.session.id);
            assert_eq!(broker.agent_coordinator.resolve(&task[0].agent_id)?.selection.unwrap().provider,child_provider);
            assert_eq!(root.turn.load(Ordering::Acquire),2);
            assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(),1);
            assert_eq!(broker.agent_coordinator.active_count(),0);
        }
    }
    Ok(())
}

#[tokio::test]
async fn durable_operation_receipts_prevent_duplicate_child_tasks_before_and_after_completion() -> Result<()> {
    let repository=repository();
    let data=tempfile::tempdir()?;
    let mut broker=planning_question_broker(repository.path(),data.path(),false);
    let request=json!({"profile":"worker","text":"FIRST","operation_id":"spawn-receipt"});
    let first=broker.start_agent(request.clone()).await?.0;
    let repeated=broker.start_agent(request.clone()).await?.0;
    assert_eq!(first["agent"]["id"],repeated["agent"]["id"]);
    assert_eq!(first["task"]["id"],repeated["task"]["id"]);
    settle_managed_children(&mut broker).await?;
    let completed=broker.start_agent(request.clone()).await?.0;
    assert_eq!(first["task"]["id"],completed["task"]["id"]);
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(),1);
    assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(),1);
    let mut changed=request;
    changed["text"]=json!("DIFFERENT");
    assert!(broker.start_agent(changed).await.is_err());
    let session_id=broker.session.id.clone();
    broker.backend.shutdown().await?;
    drop(broker);
    let mut recovered=HarnessBroker::initialize(InitializeRequest {
        backend_launch_by_provider:Default::default(),config_dir:None,data_root:data.path().to_string_lossy().into_owned(),
        permission_file:None,workspace:repository.path().to_string_lossy().into_owned(),client_id:"receipt-reopen".into(),
        backend:BackendLaunch {kind:"mock".into(),command:vec!["mock".into()]},model:"mock-model".into(),effort:"low".into(),
        session_id:Some(session_id),new_session_name:None,goal_max_turns:20,lease_conflict_action:None})?;
    let replayed=recovered.start_agent(json!({"profile":"worker","text":"FIRST","operation_id":"spawn-receipt"})).await?.0;
    assert_eq!(first["task"]["id"],replayed["task"]["id"]);
    assert_eq!(recovered.agent_coordinator.active_count(),0);
    assert_eq!(recovered.store.list_agent_task(&recovered.session.id)?.len(),1);
    Ok(())
}

#[tokio::test]
async fn rejected_followup_preserves_inbox_and_current_root_policy() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    broker.agent_backend_factory.register(Arc::new(RoutingBackend {
        kind:crate::backend::BackendKind::Mock,turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false,
    }));
    let admitted = broker.start_agent(json!({"profile":"worker","text":"CHILD_RESULT"})).await?.0;
    let target = admitted["agent"]["id"].as_str().unwrap().to_owned();
    settle_managed_children(&mut broker).await?;
    broker.handle_agent_request(crate::agent::AgentRequest {
        operation_id:"inbox".into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Message {target:target.clone(),message:"Retain this information".into()},
    },None,&mut Vec::new()).await?;
    let mut agent = broker.agent_coordinator.resolve(&target)?;
    agent.selection.as_mut().unwrap().execution_mode = PermissionMode::Yolo;
    agent.selection.as_mut().unwrap().access.sandbox = false;
    let root = broker.backend_request(BackendInput::from_text(""),PromptMode::Chat);
    let request = broker.agent_backend_factory.request(&root,&agent,"NEXT".into())?;
    assert_eq!(request.execution_mode,root.execution_mode);
    assert_eq!(request.access.sandbox,root.access.sandbox);
    assert_eq!(request.backend_session_id,agent.provider_thread_id);
    broker.handle_agent_request(crate::agent::AgentRequest {
        operation_id:"followup".into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Followup {target:target.clone(),message:"NEXT".into()},
    },None,&mut Vec::new()).await?;
    settle_managed_children(&mut broker).await?;
    let agent = broker.agent_coordinator.resolve(&target)?;
    assert_eq!(agent.inbox[0].delivery,crate::agent::MessageDelivery::Queued);
    assert_eq!(agent.inbox[0].task_id,None);
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.last().unwrap().state,ExchangeState::Failed);
    assert_eq!(broker.agent_coordinator.active_count(),0);
    Ok(())
}


#[derive(Default)]
struct ResultReceivingBackend {
    turn: std::sync::atomic::AtomicUsize,
    active: std::sync::atomic::AtomicBool,
    incoming: tokio::sync::Notify,
    receipt: std::sync::Mutex<Vec<(String,tokio::sync::oneshot::Sender<()>)>>,
    message: std::sync::Mutex<Vec<String>>,
    input: std::sync::Mutex<Vec<String>>,
}

#[async_trait::async_trait]
impl Backend for ResultReceivingBackend {
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        crate::backend::BackendDescriptor {kind:crate::backend::BackendKind::Mock,
            label:"Result receiver".into(),capability:crate::backend::mock_capability()}
    }
    async fn shutdown(&self) -> Result<()> {Ok(())}
    async fn has_active_turn(&self,_session_id:&str) -> bool {self.active.load(Ordering::Acquire)}
    async fn cancel_session(&self,_session_id:&str) -> Result<()> {Ok(())}
    async fn cleanup_execution(&self,_session_id:&str) -> Result<()> {Ok(())}
    async fn active_session_id_for(&self,_session_id:&str) -> Option<String> {Some("result-parent-history".into())}
    async fn steer_session(&self,_session_id:&str,input:crate::backend::SteeringInput) -> Result<()> {
        anyhow::ensure!(matches!(input,crate::backend::SteeringInput::AgentResult(_)),"child completion used user steering");
        let (receipt,accepted)=tokio::sync::oneshot::channel();
        self.receipt.lock().unwrap().push((input.text().to_owned(),receipt));
        self.incoming.notify_one();
        accepted.await.context("parent stopped polling before result acceptance")
    }
    async fn prompt_stream(&self,request:BackendRequest,sink:Option<BackendEventSink>) -> Result<crate::backend::BackendOutput> {
        self.turn.fetch_add(1,Ordering::AcqRel);
        self.input.lock().unwrap().push(request.input.text());
        self.active.store(true,Ordering::Release);
        if !request.input.text().starts_with("EXPLICIT_NEXT_TURN") {
            let scope=request.control_context.as_ref().and_then(|context|context.agent.as_ref()).context("root authority missing")?;
            for task in ["first", "second"] {
                scope.invoke("harness_spawn_agent",json!({"profile":"worker","text":format!("CHILD_REPORT_{task}")})).await?;
            }
            loop {
                let notified=self.incoming.notified();
                for (message,receipt) in self.receipt.lock().unwrap().drain(..) {
                    self.message.lock().unwrap().push(message);
                    let _=receipt.send(());
                }
                if self.message.lock().unwrap().len() == 2 {break;}
                notified.await;
            }
        }
        self.active.store(false,Ordering::Release);
        if let Some(sink)=sink {
            sink.send_wait(BackendEvent {received_at_ms:None,address:None,turn_boundary:None,kind:"assistant_message".into(),
                text:Some("SYNTHESIZED_RESULT".into()),data:Value::Null,activity:None,summary:None,task_update:None}).await?;
        }
        Ok(crate::backend::BackendOutput {backend_session_id:Some("result-parent-history".into()),
            capability:self.descriptor().capability,..Default::default()})
    }
}

#[tokio::test]
async fn child_results_reach_active_parent_once_without_wait() -> Result<()> {
    {
        let repository=repository();
        let data=tempfile::tempdir()?;
        let mut broker=planning_question_broker(repository.path(),data.path(),false);
        let backend=Arc::new(ResultReceivingBackend::default());
        broker.backend=backend.clone();
        broker.agent_coordinator.set_enabled(true);
        tokio::time::timeout(std::time::Duration::from_secs(5),broker.run_interaction("Research".into(),PromptMode::Chat,
            Some(ExchangeAdmission::chat("Research".into())))).await??;
        assert_eq!(backend.turn.load(Ordering::Acquire),1,"accepted delivery started a duplicate parent turn");
        let message=backend.message.lock().unwrap();
        assert_eq!(message.len(),2);
        for (task,result) in broker.store.list_agent_task(&broker.session.id)?.iter().zip(["first","second"]) {
            assert!(task.parent_reported);
            assert_eq!(message.iter().filter(|message|message.contains(&task.id)).count(),1);
            assert!(message.iter().any(|message|message.contains(&format!("CHILD_REPORT_{result}"))));
        }
        for message in message.iter() {
            let payload:Value=serde_json::from_str(message.split_once('\n').unwrap().1)?;
            assert_eq!(payload.as_object().unwrap().len(),5);
            assert!(payload.get("instructions").is_none() && payload.get("parent_reported").is_none());
        }
        let exchanges=broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchanges.len(),1);
        assert_eq!(exchanges[0].state,ExchangeState::Complete);
        assert!(!exchanges[0].turn.iter().flat_map(|turn|turn.messages()).any(|message|message.text().contains("CHILD_REPORT_")),
            "child result was rendered as a parent message");
    }
    Ok(())
}

#[tokio::test]
async fn idle_child_result_is_retained_until_one_explicit_parent_turn() -> Result<()> {
    let repository=repository();
    let data=tempfile::tempdir()?;
    let mut broker=planning_question_broker(repository.path(),data.path(),false);
    let backend=Arc::new(ResultReceivingBackend::default());
    broker.backend=backend.clone();
    let admission=admit_child_fixture(&mut broker,"idle-result")?;
    let owner=crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    let mut event=Vec::new();
    broker.observe_managed_agent(owner.clone(),&admission.task.id,child_event("turn_started"),&mut event).await?;
    let mut response=child_event("assistant_message");
    response.text=Some("RETAINED_CHILD_REPORT".into());
    response.data=json!({"phase":"final_answer","provider_message_id":"result"});
    broker.observe_managed_agent(owner.clone(),&admission.task.id,response,&mut event).await?;
    let mut finished=child_event("turn_completed");
    finished.turn_boundary=Some(crate::backend::TurnBoundary::Finished {outcome:crate::turn::TurnOutcome::Completed});
    broker.observe_managed_agent(owner.clone(),&admission.task.id,finished,&mut event).await?;
    broker.complete_managed_agent(owner.clone(),admission.task.id.clone(),Ok(Default::default()),None,&mut event).await?;
    broker.complete_managed_agent(owner,admission.task.id.clone(),Err(anyhow::anyhow!("duplicate")),None,&mut event).await?;
    assert_eq!(backend.turn.load(Ordering::Acquire),0);
    assert!(!broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    for _ in 0..2 {
        broker.run_interaction("EXPLICIT_NEXT_TURN".into(),PromptMode::Chat,
            Some(ExchangeAdmission::chat("EXPLICIT_NEXT_TURN".into()))).await?;
    }
    let input=backend.input.lock().unwrap();
    assert!(input[0].contains("RETAINED_CHILD_REPORT"));
    assert_eq!(input[0].matches("RETAINED_CHILD_REPORT").count(),1);
    assert!(!input[1].contains("RETAINED_CHILD_REPORT"));
    assert!(broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    Ok(())
}


#[tokio::test]
async fn rejected_active_result_delivery_retains_report_for_next_input() -> Result<()> {
    let repository=repository();
    let data=tempfile::tempdir()?;
    let mut broker=planning_question_broker(repository.path(),data.path(),false);
    broker.backend=Arc::new(RoutingBackend {kind:crate::backend::BackendKind::Mock,
        turn:Default::default(),request:Default::default(),reject_steering:true});
    let admission=admit_child_fixture(&mut broker,"rejected-result")?;
    let owner=crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    broker.complete_managed_agent(owner,admission.task.id.clone(),Err(anyhow::anyhow!("CHILD_FAILURE")),None,&mut Vec::new()).await?;
    let mut attempted=std::collections::HashSet::new();
    let mut delivery=tokio::task::JoinSet::new();
    broker.start_agent_result_delivery(&[],&mut attempted,&mut delivery)?;
    let result=delivery.join_next().await.context("missing attempted delivery")?;
    broker.finish_agent_result_delivery(result)?;
    assert_eq!(broker.agent_coordinator.unreported_result().len(),1);
    assert!(!broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    broker.start_agent_result_delivery(&[],&mut attempted,&mut delivery)?;
    assert!(delivery.is_empty(),"rejected delivery was retried within the same provider turn");
    let backend=Arc::new(ResultReceivingBackend::default());
    broker.backend=backend.clone();
    broker.run_interaction("EXPLICIT_NEXT_TURN".into(),PromptMode::Chat,
        Some(ExchangeAdmission::chat("EXPLICIT_NEXT_TURN".into()))).await?;
    assert!(backend.input.lock().unwrap()[0].contains("CHILD_FAILURE"));
    assert!(broker.store.list_agent_task(&broker.session.id)?[0].parent_reported);
    Ok(())
}


struct SuspendedParentBackend {
    blocked: AtomicBool,
    root_turn: std::sync::atomic::AtomicUsize,
    root_started: tokio::sync::Semaphore,
    child_started: tokio::sync::Semaphore,
    child_release: [tokio::sync::Semaphore; 2],
    input: std::sync::Mutex<Vec<String>>,
}

impl Default for SuspendedParentBackend {
    fn default() -> Self {
        Self {
            blocked: Default::default(),
            root_turn: Default::default(),
            root_started: tokio::sync::Semaphore::new(0),
            child_started: tokio::sync::Semaphore::new(0),
            child_release: std::array::from_fn(|_| tokio::sync::Semaphore::new(0)),
            input: Default::default(),
        }
    }
}

#[async_trait::async_trait]
impl Backend for SuspendedParentBackend {
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        let mut capability = crate::backend::mock_capability();
        capability.native_steer = false;
        crate::backend::BackendDescriptor {
            kind: crate::backend::BackendKind::Mock,
            label: "Suspended parent".into(),
            capability,
        }
    }
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
    async fn has_active_turn(&self, _session_id: &str) -> bool {
        false
    }
    async fn cancel_session(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }
    async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }
    async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
        Some("suspended-history".into())
    }
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        sink: Option<BackendEventSink>,
    ) -> Result<crate::backend::BackendOutput> {
        let response = if request.owner()?.is_root() {
            self.input.lock().unwrap().push(request.input.text());
            let ordinal = self.root_turn.fetch_add(1, Ordering::AcqRel);
            if ordinal == 0 {
                let scope = request
                    .control_context
                    .as_ref()
                    .and_then(|context| context.agent.as_ref())
                    .context("root authority missing")?;
                for name in ["ONE", "TWO"] {
                    scope
                        .invoke(
                            "harness_spawn_agent",
                            json!({"profile":"worker","text":format!("CHILD_{name}")}),
                        )
                        .await?;
                }
            }
            self.root_started.add_permits(1);
            "PARENT_RESPONSE".to_owned()
        } else {
            let index = usize::from(request.input.text().ends_with("CHILD_TWO"));
            self.child_started.add_permits(1);
            self.child_release[index].acquire().await?.forget();
            format!("RESULT_{}", if index == 0 { "ONE" } else { "TWO" })
        };
        if let Some(sink) = sink {
            sink.send_wait(BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some(response),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            })
            .await?;
        }
        let mut output = crate::backend::BackendOutput {
            backend_session_id: Some("suspended-history".into()),
            capability: self.descriptor().capability,
            ..Default::default()
        };
        if self.blocked.load(Ordering::Acquire) && request.input.text() == "Research both" {
            output.plan_question = Some(crate::plan::PlanQuestionSet::freeform("Which research scope?".into()));
        }
        Ok(output)
    }
}

#[tokio::test]
async fn suspended_parent_joins_children_and_user_input_wakes_without_native_steering() -> Result<()>
{
    for user_input in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = Arc::new(SuspendedParentBackend::default());
        broker.backend = backend.clone();
        broker.agent_backend_factory.register(backend.clone());
        broker.agent_coordinator.set_enabled(true);
        let coordinator = broker.agent_coordinator.clone();
        let input = broker.parent_input.clone();
        let task = tokio::spawn(async move {
            let result = broker
                .run_interaction(
                    "Research both".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat("Research both".into())),
                )
                .await;
            (broker, result)
        });
        tokio::time::timeout(
            std::time::Duration::from_secs(3),
            backend.child_started.acquire_many(2),
        )
        .await??
        .forget();
        backend.root_started.acquire().await?.forget();
        backend.child_release[0].add_permits(1);
        tokio::time::timeout(std::time::Duration::from_secs(3), async {
            while coordinator.active_count() != 1 {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        assert_eq!(
            backend.root_turn.load(Ordering::Acquire),
            1,
            "first child resumed the idle parent before its sibling settled"
        );
        if user_input {
            tokio::time::timeout(
                std::time::Duration::from_secs(3),
                input.steer("USER_CORRECTION".into()),
            )
            .await??;
            tokio::time::timeout(
                std::time::Duration::from_secs(3),
                backend.root_started.acquire(),
            )
            .await??
            .forget();
            assert_eq!(backend.root_turn.load(Ordering::Acquire), 2);
            assert_eq!(
                coordinator.active_count(),
                1,
                "user input cancelled the remaining child"
            );
        }
        backend.child_release[1].add_permits(1);
        let (broker, result) =
            tokio::time::timeout(std::time::Duration::from_secs(3), task).await??;
        let (_, events) = result?;
        let statuses = events
            .iter()
            .filter_map(|event| event.payload.get("status"))
            .filter_map(|status| status.get("kind").and_then(Value::as_str))
            .collect::<Vec<_>>();
        assert!(statuses.contains(&"waiting_for_agent"), "{statuses:?}");
        let waiting = statuses
            .iter()
            .position(|status| *status == "waiting_for_agent")
            .unwrap();
        assert!(statuses[waiting + 1..].contains(&"working"), "{statuses:?}");
        assert_eq!(
            backend.root_turn.load(Ordering::Acquire),
            if user_input { 3 } else { 2 }
        );
        let inputs = backend.input.lock().unwrap();
        for result in ["RESULT_ONE", "RESULT_TWO"] {
            assert_eq!(
                inputs
                    .iter()
                    .map(|input| input.matches(result).count())
                    .sum::<usize>(),
                1
            );
        }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state, ExchangeState::Complete);
        assert_eq!(exchange[0].node_list.iter().filter(|node|matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput {prompt} if prompt.text=="USER_CORRECTION")).count(),usize::from(user_input));
        assert!(
            broker
                .store
                .list_agent_task(&broker.session.id)?
                .iter()
                .all(|task| task.parent_reported)
        );
    }
    Ok(())
}

#[tokio::test]
async fn parent_cancellation_and_user_decision_prevent_child_continuation() -> Result<()> {
    for user_decision in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let backend = Arc::new(SuspendedParentBackend::default());
        backend.blocked.store(user_decision, Ordering::Release);
        broker.backend = backend.clone();
        broker.agent_backend_factory.register(backend.clone());
        broker.agent_coordinator.set_enabled(true);
        let cancellation = broker.turn_cancellation();
        let coordinator = broker.agent_coordinator.clone();
        let task = tokio::spawn(async move {
            let result = broker
                .run_interaction(
                    "Research both".into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat("Research both".into())),
                )
                .await;
            (broker, result)
        });
        tokio::time::timeout(
            std::time::Duration::from_secs(3),
            backend.child_started.acquire_many(2),
        )
        .await??
        .forget();
        backend.root_started.acquire().await?.forget();
        let quiescence = if !user_decision {
            let guard = coordinator.begin_quiescence();
            tokio::time::timeout(std::time::Duration::from_secs(3), coordinator.sender().request(crate::agent::AgentRequest {
                operation_id: "cancel-children".into(),
                source: crate::agent::AgentSource::User,
                operation: crate::agent::AgentOperation::InterruptAll,
            })).await??;
            assert_eq!(backend.root_turn.load(Ordering::Acquire), 1);
            cancellation.request(false);
            Some(guard)
        } else { None };
        let (mut broker, result) =
            tokio::time::timeout(std::time::Duration::from_secs(3), task).await??;
        if user_decision {
            result?;
        } else {
            let error = result.unwrap_err();
            assert!(error.is::<TurnCancelled>(), "{error:#}");
        }
        backend.child_release[0].add_permits(1);
        backend.child_release[1].add_permits(1);
        tokio::time::timeout(std::time::Duration::from_secs(3), async {
            while broker.agent_coordinator.active_count() != 0 {
                broker.drain_agent_commands(None).await?;
                tokio::task::yield_now().await;
            }
            Ok::<_, anyhow::Error>(())
        })
        .await??;
        assert_eq!(backend.root_turn.load(Ordering::Acquire), 1);
        assert!(!broker.parent_input.is_active());
        assert_eq!(broker.agent_coordinator.unreported_result().len(), 2);
        drop(quiescence);
    }
    Ok(())
}
