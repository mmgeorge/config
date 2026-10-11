struct RoutingBackend {
    kind:crate::backend::BackendKind,
    child_provider:Option<String>,
    turn:std::sync::atomic::AtomicUsize,
    request:std::sync::Mutex<Vec<BackendRequest>>,
    reject_steering:bool,
}

fn admit_child_fixture(broker: &mut HarnessBroker, name: &str) -> Result<crate::agent::AgentAdmission> {
    let mut parent = Exchange::agent_admission(&broker.session.id,
        broker.store.next_exchange_ordinal(&broker.session.id)?,"Admission".into(),100);
    let spawn = crate::agent::SpawnAgentRequest {task_name:name.into(),message:"Inspect".into(),
        provider:None,model:None,reasoning_effort:None};
    let request = crate::agent::AgentRequest {operation_id:name.into(),source:crate::agent::AgentSource::User,
        operation:crate::agent::AgentOperation::Spawn(spawn.clone())};
    let selection = crate::agent::AgentSelection {provider:crate::backend::BackendKind::Mock,
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
        assert!(event.iter().any(|event|event.payload["kind"] == "approval_requested"
            && event.payload["data"]["exchange_id"] == admission.task.id
            && event.payload["data"]["execution_owner"]["agent_id"] == admission.agent.id));
    }
    broker.interrupt_managed_agent(&first.agent.id,&mut event).await?;
    assert_eq!(waiters.remove(0).await??,crate::backend::approval::ApprovalResolution::Cancel);
    assert!(!waiters[0].is_finished());
    assert_eq!(broker.permission_coordinator.pending_list()?.len(),1);
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
async fn child_wait_observes_manual_activity_and_returns_its_completed_result() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"manual-wait")?;
    broker.agent_coordinator.set_enabled(true);
    let (scope,_lease) = crate::agent::AgentToolScope::new(crate::backend::ExecutionOwner::root(&broker.session.id),
        admission.task.parent_exchange_id.clone(),broker.agent_coordinator.clone())?;
    let waiting_scope = scope.clone();
    let waiting = tokio::spawn(async move {waiting_scope.invoke("harness_wait_agent",json!({"timeout_ms":10000})).await});
    tokio::task::yield_now().await;
    assert!(!waiting.is_finished(),"manual child must keep wait pending");
    let owner = crate::backend::ExecutionOwner {session_id:broker.session.id.clone(),agent_id:admission.agent.id.clone()};
    let mut event = Vec::new();
    let mut thought = child_event("assistant_message");
    thought.text = Some("Latest thought".into());
    thought.data = json!({"phase":"commentary","provider_message_id":"thought"});
    broker.observe_managed_agent(owner.clone(),&admission.task.id,thought,&mut event).await?;
    let result:Value = serde_json::from_str(&tokio::time::timeout(std::time::Duration::from_secs(2),waiting).await???)?;
    assert_eq!(result["timed_out"],false);
    assert_eq!(result["agents"][0]["latest_task"]["state"],"running");
    broker.complete_managed_agent(owner,admission.task.id.clone(),Err(anyhow::anyhow!("completed with failure")),None,&mut event).await?;
    let mut revision = broker.agent_coordinator.subscribe();
    let waiting = tokio::spawn(async move {scope.invoke("harness_wait_agent",json!({"timeout_ms":10000})).await});
    tokio::time::timeout(std::time::Duration::from_secs(2),revision.changed()).await??;
    let mut parent = broker.store.load_exchange(&admission.task.parent_exchange_id)?.unwrap();
    broker.drain_agent_commands(Some(&mut parent)).await?;
    let result:Value = serde_json::from_str(&tokio::time::timeout(std::time::Duration::from_secs(2),waiting).await???)?;
    assert_eq!(result["results"][0]["id"],admission.task.id);
    assert_eq!(result["results"][0]["error"],"completed with failure");
    assert!(broker.agent_coordinator.unreported_result().is_empty());
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
async fn child_admission_and_followup_reject_unsupported_root_sandbox() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let admission = admit_child_fixture(&mut broker,"policy")?;
    let root = broker.backend_request(BackendInput::from_text(""),PromptMode::Chat);
    assert!(root.access.sandbox);
    for provider in [crate::backend::BackendKind::Copilot,crate::backend::BackendKind::Claude,crate::backend::BackendKind::Antigravity] {
        let spawn = crate::agent::SpawnAgentRequest {task_name:"blocked".into(),message:"Inspect".into(),
            provider:Some(crate::agent::provider_name(provider).into()),model:None,reasoning_effort:None};
        assert!(broker.agent_backend_factory.resolve(&root,crate::backend::BackendKind::Codex,&spawn).await.unwrap_err().to_string().contains("OS sandbox"));
        let mut agent = admission.agent.clone();
        agent.selection.as_mut().unwrap().provider = provider;
        assert!(broker.agent_backend_factory.request(&root,&agent,"Follow-up".into()).unwrap_err().to_string().contains("OS sandbox"));
    }
    Ok(())
}

#[tokio::test]
async fn managed_child_replays_and_late_failures_preserve_sibling_and_followup_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    let mut parent = Exchange::agent_admission(&broker.session.id,1,"Admission".into(),100);
    let selection = crate::agent::AgentSelection {
        provider:crate::backend::BackendKind::Mock,model:"mock-model".into(),reasoning_effort:"low".into(),
        context_window:None,service_tier:Default::default(),access:broker.session.access.clone(),
        execution_mode:broker.session.execution_mode,
    };
    let mut admission = Vec::new();
    for name in ["first","sibling"] {
        let spawn = crate::agent::SpawnAgentRequest {task_name:name.into(),message:"Inspect".into(),
            provider:None,model:None,reasoning_effort:None};
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
        kind:crate::backend::BackendKind::Mock,child_provider:None,turn:std::sync::atomic::AtomicUsize::new(0),
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
    async fn steer_session(&self,_session_id:&str,_text:String) -> Result<()> {Err(anyhow::anyhow!("steering was not accepted"))}
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
                scope.invoke("harness_spawn_agent",json!({"task_name":"routing-child","message":"CHILD_RESULT",
                    "provider":self.child_provider,"model":"fixture-model"})).await?;
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
            anyhow::ensure!(matches!(&request.input,BackendInput::Text {text} if text == "CHILD_RESULT"),"child inherited parent transcript");
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
async fn cross_provider_children_resolve_saved_model_reasoning_context_and_service_tier() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(),data.path(),false);
    broker.agent_backend_factory.register(Arc::new(RoutingBackend {kind:crate::backend::BackendKind::Codex,
        child_provider:None,turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false}));
    let mut settings = crate::settings::SettingsStore::open(data.path())?;
    let revision = settings.get(&broker.session.workspace,&broker.session.id,"codex")?.revision;
    settings.save_provider(revision,"codex",&json!({"model":"fixture-model","effort":"low",
        "context_window":"extended","service_tier":"fast"}))?;
    let root = broker.backend_request(BackendInput::from_text(""),PromptMode::Chat);
    let spawn = crate::agent::SpawnAgentRequest {task_name:"configured-child".into(),message:"Inspect".into(),
        provider:Some("codex".into()),model:None,reasoning_effort:None};
    let selection = broker.agent_backend_factory.resolve(&root,crate::backend::BackendKind::Mock,&spawn).await?;
    assert_eq!(selection.model,"fixture-model");
    assert_eq!(selection.reasoning_effort,"low");
    assert_eq!(selection.context_window.as_deref(),Some("extended"));
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
            let root=Arc::new(RoutingBackend {kind:root_provider,child_provider:Some(crate::agent::provider_name(child_provider).into()),
                turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false});
            let child=Arc::new(RoutingBackend {kind:child_provider,child_provider:None,turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false});
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
    let request=json!({"task_name":"receipt-child","message":"FIRST","operation_id":"spawn-receipt"});
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
    changed["message"]=json!("DIFFERENT");
    assert!(broker.start_agent(changed).await.is_err());
    let session_id=broker.session.id.clone();
    broker.backend.shutdown().await?;
    drop(broker);
    let mut recovered=HarnessBroker::initialize(InitializeRequest {
        backend_launch_by_provider:Default::default(),config_dir:None,data_root:data.path().to_string_lossy().into_owned(),
        permission_file:None,workspace:repository.path().to_string_lossy().into_owned(),client_id:"receipt-reopen".into(),
        backend:BackendLaunch {kind:"mock".into(),command:vec!["mock".into()]},model:"mock-model".into(),effort:"low".into(),
        session_id:Some(session_id),new_session_name:None,goal_max_turns:20,lease_conflict_action:None})?;
    let replayed=recovered.start_agent(json!({"task_name":"receipt-child","message":"FIRST","operation_id":"spawn-receipt"})).await?.0;
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
        kind:crate::backend::BackendKind::Mock,child_provider:None,turn:std::sync::atomic::AtomicUsize::new(0),request:Default::default(),reject_steering:false,
    }));
    let admitted = broker.start_agent(json!({"task_name":"rejected-child","message":"CHILD_RESULT"})).await?.0;
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
