use anyhow::{Context, Result, ensure};
use forge_harness::backend::approval::PermissionCoordinator;
use forge_harness::backend::codex::CodexBackend;
use forge_harness::backend::copilot::CopilotBackend;
use forge_harness::backend::{
    Backend, BackendError, BackendEvent, BackendInput, BackendRequest, InvocationScope, ToolUpdate,
};
use forge_harness::broker::{HarnessBroker, InitializeRequest};
use forge_harness::session::{ApprovalMode, SessionStore};
use forge_harness::trace::TraceStore;
use forge_protocol::message::Request;
use serde_json::json;
use std::path::Path;
use std::sync::Arc;

/// Own a production task coordinator and drain its client stream during each request.
struct WorkflowService {
    service: forge_harness::service::HarnessService,
    session_id: String,
    sink: forge_protocol::outbound::MessageSender,
    output: forge_protocol::outbound::MessageReceiver,
    request_id: u64,
    data_root: std::path::PathBuf,
    event: Vec<serde_json::Value>,
    last_transition: Option<String>,
}

impl WorkflowService {
    /// Open one isolated session with the production task execution supervisor.
    async fn open(initialize: InitializeRequest) -> Result<Self> {
        let data_root = std::path::PathBuf::from(&initialize.data_root);
        let diff = forge_diff::engine::DiffEngine::new(Default::default(), 1);
        let syntax = forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool());
        let service = forge_harness::service::HarnessService::new(
            Arc::new(forge_git::store::RepositoryStore::default()),
            diff,
            syntax,
        );
        let opened = service.open_session(1, initialize).await?;
        ensure!(
            opened.error().is_none(),
            "open session failed: {:?}",
            opened.error()
        );
        let session_id = opened
            .result()
            .and_then(|value| value["session"]["id"].as_str())
            .context("opened session omitted identity")?
            .to_owned();
        let (sink, output) = forge_protocol::outbound::channel();
        service.start_provider_session(&session_id, &sink).await?;
        Ok(Self {
            service,
            session_id,
            sink,
            output,
            request_id: 1,
            data_root,
            event: Vec::new(),
            last_transition: None,
        })
    }

    /// Await the correlated response while consuming provider progress without backpressure.
    async fn request(
        &mut self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<serde_json::Value> {
        self.request_id += 1;
        let request_id = self.request_id;
        let execution = self.service.dispatch(
            Some(self.session_id.clone()),
            Request {
                id: request_id,
                method: method.into(),
                params,
            },
            &self.sink,
        );
        tokio::pin!(execution);
        let mut dispatched = false;
        let mut response = None;
        let mut failure = None;
        let mut observed_phase = None;
        loop {
            if dispatched && let Some(response) = response.take() {
                ensure!(
                    failure.is_none(),
                    "{}; evidence: {}",
                    failure.unwrap_or_default(),
                    self.data_root.display()
                );
                return Ok(response);
            }
            tokio::select! {
                result = &mut execution, if !dispatched => {
                    result.with_context(|| format!("{method} dispatch; evidence: {}", self.data_root.display()))?;
                    dispatched = true;
                }
                frame = self.output.recv() => {
                let frame = frame?.context("service stream closed before response")?;
                let value: serde_json::Value = serde_json::from_slice(frame.bytes())?;
                if value.get("event").is_some() { self.event.push(value.clone()); }
                if matches!(value["event"].as_str(), Some("plan_changed" | "goal_changed" | "task_operation" | "execution_state" | "session_changed")) {
                    let phase = format!("{}: state={}, task={}", value["event"], value["payload"]["state"], value["payload"]["session"]["active_task"]["kind"]);
                    if observed_phase.as_ref() != Some(&phase) {
                        eprintln!("{}: {method}: {phase}; evidence: {}", self.session_id, self.data_root.display());
                        observed_phase = Some(phase);
                    }
                }
                // Finish dispatch before reporting failures; dropping it can abandon admitted work.
                if value["event"] == "error" || value["payload"]["kind"] == "error" {
                    failure = Some(format!("service failed: {value}"));
                }
                if value["id"].as_u64() == Some(request_id) {
                    if value.get("error").is_some() {
                        failure = Some(format!("{method} failed: {value}"));
                    }
                    response = Some(value["result"].clone());
                }
            }
            }
        }
    }

    /// Publish changed lifecycle observations with the retained fixture evidence directory.
    fn log_transition(&mut self, transition: String) {
        if self.last_transition.as_ref() != Some(&transition) {
            eprintln!(
                "{}: {transition}; evidence: {}",
                self.session_id,
                self.data_root.display()
            );
            self.last_transition = Some(transition);
        }
    }

    /// Recognize durable completion of this objective after its active task owner is released.
    fn completed_goal(&self, objective: &str) -> Result<bool> {
        Ok(forge_harness::storage::SqliteStore::open(&self.data_root)?
            .list_session_event(&self.session_id)?
            .iter()
            .any(|event| matches!(&event.detail, forge_harness::timeline::SessionEventKind::TaskFinished { outcome }
                if outcome.kind == forge_harness::task::TaskKind::Goal
                    && outcome.title == objective
                    && matches!(outcome.state, forge_harness::task::TaskOutcomeState::Complete))))
    }

    /// Observe task publication until its explicit expected state arrives.
    async fn wait_plan(
        &mut self,
        expected: forge_harness::plan::PlanState,
    ) -> Result<serde_json::Value> {
        loop {
            let snapshot = self.request("session.state.get", json!({})).await?;
            self.assert_exchange_health()?;
            if !snapshot["active_plan"].is_null() {
                let plan: forge_harness::plan::PlanRecord =
                    serde_json::from_value(snapshot["active_plan"].clone())?;
                self.log_transition(format!(
                    "plan {} {:?}; waiting for {expected:?}",
                    plan.id, plan.state
                ));
                if plan.state == expected {
                    // Questions and review intentionally retain a running exchange at human control points.
                    if expected == forge_harness::plan::PlanState::AwaitingInput {
                        ensure!(
                            plan.elicitation
                                .as_ref()
                                .and_then(|question| question.current_question())
                                .is_some(),
                            "awaiting-input plan omitted its question; evidence: {}",
                            self.data_root.display()
                        );
                    }
                    if expected == forge_harness::plan::PlanState::AwaitingReview {
                        ensure!(
                            plan.submitted_version.is_some()
                                && Path::new(&plan.working_path).exists(),
                            "review-ready plan omitted its submitted artifact; evidence: {}",
                            self.data_root.display()
                        );
                    }
                    return Ok(snapshot);
                }
                ensure!(
                    plan.state != forge_harness::plan::PlanState::ConfiguringChecks,
                    "configured fixture unexpectedly requested check setup"
                );
                self.require_pending_work(&format!("plan {expected:?}; current {:?}", plan.state))
                    .await?;
            }
            tokio::time::sleep(std::time::Duration::from_millis(25)).await;
        }
    }

    /// Reject an unreachable expected state after queued and active work have both ended.
    async fn require_pending_work(&mut self, expected: &str) -> Result<()> {
        let input = self.request("session.input.get", json!({})).await?;
        let queued = input["messages"]
            .as_array()
            .context("input snapshot omitted messages")?
            .iter()
            .any(|message| matches!(message["delivery"].as_str(), Some("queued" | "delivering")));
        let store = forge_harness::storage::SqliteStore::open(&self.data_root)?;
        let history = store.list_exchange(&self.session_id)?;
        let task_running = self.has_running_task(&store)?;
        ensure!(
            input["execution_busy"] != false
                || queued
                || task_running
                || history
                    .last()
                    .is_none_or(|exchange| !exchange.is_finished()),
            "service settled before {expected}; evidence: {}; input={input}; last exchange={:?}; events={:?}",
            self.data_root.display(),
            history
                .last()
                .map(|exchange| (exchange.id.as_str(), exchange.state())),
            self.event
        );
        Ok(())
    }

    /// Get whether canonical task ownership still permits admission between provider exchanges.
    fn has_running_task(&self, store: &forge_harness::storage::SqliteStore) -> Result<bool> {
        Ok(store
            .load_session(&self.session_id)?
            .context("workflow session is missing")?
            .active_task
            .is_some_and(|task| task.status() == forge_harness::task::TaskStatus::Running))
    }

    /// Fail promptly if task execution or finalization has already failed.
    fn assert_exchange_health(&self) -> Result<()> {
        let history = forge_harness::storage::SqliteStore::open(&self.data_root)?
            .list_exchange(&self.session_id)?;
        for exchange in history {
            ensure!(
                exchange.finalization_error.is_none(),
                "exchange finalization failed: {:?}",
                exchange.finalization_error
            );
            ensure!(
                !matches!(
                    exchange.state(),
                    forge_harness::exchange::ExchangeState::Failed
                        | forge_harness::exchange::ExchangeState::Cancelled
                        | forge_harness::exchange::ExchangeState::Interrupted
                ),
                "task exchange {} settled {:?}; service events: {:?}",
                exchange.id,
                exchange.state(),
                self.event
            );
        }
        Ok(())
    }

    /// Drain the service until the input lane and its persisted exchange have both settled.
    async fn wait_idle(&mut self) -> Result<()> {
        loop {
            let input = self.request("session.input.get", json!({})).await?;
            self.assert_exchange_health()?;
            let queued = input["messages"]
                .as_array()
                .context("input snapshot omitted messages")?
                .iter()
                .any(|message| {
                    matches!(message["delivery"].as_str(), Some("queued" | "delivering"))
                });
            let store = forge_harness::storage::SqliteStore::open(&self.data_root)?;
            let history = store.list_exchange(&self.session_id)?;
            let task_running = self.has_running_task(&store)?;
            self.log_transition(format!(
                "settlement: execution_busy={}, queued={queued}, exchange={:?}",
                input["execution_busy"],
                history.last().map(|exchange| exchange.state())
            ));
            if input["execution_busy"] == false
                && !queued
                && !task_running
                && history
                    .last()
                    .is_some_and(|exchange| exchange.is_finished())
            {
                return Ok(());
            }
            tokio::time::sleep(std::time::Duration::from_millis(25)).await;
        }
    }

    /// Collect every provider and background task owned by this fixture.
    async fn shutdown(&self) -> Result<()> {
        self.service
            .shutdown(std::time::Duration::from_secs(10))
            .await
    }
}

/// Prepare a temporary project whose provider configuration excludes personal MCP servers.
fn initialize(provider: &str, root: &Path) -> Result<InitializeRequest> {
    let config = root.join("config");
    let workspace = root.join("workspace");
    std::fs::create_dir_all(config.join("rulesync-global/.rulesync"))?;
    std::fs::create_dir_all(&workspace)?;
    std::fs::write(
        config.join("rulesync-global/.rulesync/mcp.json"),
        "{\"mcpServers\":{}}\n",
    )?;
    ensure!(
        std::process::Command::new("git")
            .args(["init", "--quiet"])
            .current_dir(&workspace)
            .status()?
            .success()
    );
    std::fs::write(workspace.join("input.txt"), "WORKFLOW-72931\n")?;
    std::fs::write(
        workspace.join(".forge.json"),
        serde_json::to_vec_pretty(&json!({
            "checks":{"fixture-whitespace":{"trigger":{"auto":true},"action":{
                "command":["git","diff","--check"],"label":"Check fixture whitespace","category":"format"
            }}}
        }))?,
    )?;
    let data_root = root.join("data");
    std::fs::create_dir_all(&data_root)?;
    TraceStore::open(&data_root)?.configure(true)?;
    Ok(serde_json::from_value(json!({
        "config_dir":config,"data_root":data_root,"workspace":workspace,
        "client_id":format!("live-{provider}-workflow"),"model":"gpt-6-luna","effort":"low",
        "backend":{"kind":provider,"command":if provider=="codex" {vec!["codex","app-server"]} else {Vec::new()}}
    }))?)
}

/// Require every user-level operation to settle without a broker protocol failure.
async fn dispatch(
    broker: &mut HarnessBroker,
    id: u64,
    method: &str,
    params: serde_json::Value,
) -> Result<()> {
    let result = broker
        .dispatch(Request {
            id,
            method: method.into(),
            params,
        })
        .await;
    ensure!(
        result.response.error().is_none(),
        "{method} failed: {:?}",
        result.response.error()
    );
    Ok(())
}

/// Recognize an explicit command exit result inside native or wrapper tool output.
fn command_exited(output: &str, expected: i64) -> bool {
    fn contains_exit(value: &serde_json::Value, expected: i64) -> bool {
        match value {
            serde_json::Value::Object(fields) => fields.iter().any(|(name, value)| {
                matches!(name.as_str(), "exit_code" | "exitCode")
                    && value.as_i64() == Some(expected)
                    || contains_exit(value, expected)
            }),
            serde_json::Value::Array(values) => {
                values.iter().any(|value| contains_exit(value, expected))
            }
            serde_json::Value::String(text) => command_exited(text, expected),
            _ => false,
        }
    }
    if let Ok(value) = serde_json::from_str::<serde_json::Value>(output) {
        return contains_exit(&value, expected);
    }
    let normalized = output.to_ascii_lowercase();
    [
        format!("exit code: {expected}"),
        format!("exited with code {expected}"),
        format!("completed with exit code {expected}>"),
    ]
    .iter()
    .any(|marker| normalized.contains(marker))
}

/// Exercise tool execution, recovery, task completion, checkpoints, and durable reopening.
async fn tool_workflow(provider: &str) -> Result<()> {
    let fixture = tempfile::Builder::new()
        .prefix("forge-live-workflow-")
        .tempdir()?
        .keep();
    eprintln!("{provider}: tool workflow; evidence: {}", fixture.display());
    let mut initialize = initialize(provider, &fixture)?;
    let workspace = Path::new(&initialize.workspace).to_owned();
    let mut broker = HarnessBroker::initialize(initialize.clone())?;
    let initial_backend = broker.backend_handle();
    let mut resumed_backend: Option<Arc<dyn Backend>> = None;
    let result: Result<()> = async {
    let session_id = broker.snapshot()?.session.id;
    dispatch(
        &mut broker,
        1,
        "session.rename",
        json!({"session_id":session_id,"name":"Workflow before first exchange"}),
    )
    .await?;
    dispatch(
        &mut broker,
        2,
        "session.permission.set",
        json!({"change":{"kind":"set_approval","mode":"allow"}}),
    )
    .await?;
    let failure_command = if cfg!(windows) {
        "Write-Output 'EXPECTED-WORKFLOW-FAILURE'; exit 7"
    } else {
        "sh -c 'printf EXPECTED-WORKFLOW-FAILURE; exit 7'"
    };
    let task_instruction = if provider == "codex" {
        "Perform the steps in order and report their completion."
    } else {
        "Use your native SQL todos tool to create three tasks: Read input; Write output; Check command recovery. Select only the current task as in progress, mark each complete as you finish, and complete all three checklist tasks."
    };
    let file_write_instruction = if cfg!(windows) {
        "Use Set-Content or Add-Content for PowerShell file writes, or use an available file-editing tool."
    } else {
        "Use an available file-editing tool for text changes."
    };
    eprintln!("{provider}: starting tool execution and expected command failure; evidence: {}", fixture.display());
    dispatch(&mut broker, 3, "session.prompt.submit", json!({"text":format!(
        "This is an isolated provider integration test. {task_instruction} {file_write_instruction} Perform these steps with actual tools: read input.txt; create output.txt containing only the input's token; run `{failure_command}` once and observe its expected failure; then run `git status --short` successfully. Do not stop after the expected exit 7. Do not modify other files. Finish with WORKFLOW-DONE."
    )})).await?;
    let snapshot = broker.snapshot()?;
    std::fs::write(fixture.join("initial-exchanges.json"), serde_json::to_vec_pretty(&snapshot.exchange)?)?;
    ensure!(
        std::fs::read_to_string(workspace.join("output.txt")).with_context(|| format!("provider did not create output.txt; exchange evidence: {}", fixture.join("initial-exchanges.json").display()))?.trim_start_matches('\u{feff}').trim()
            == "WORKFLOW-72931",
        "provider did not write the requested file"
    );
    ensure!(snapshot.session.resolved_model.as_deref() == Some("gpt-6-luna"), "provider resolved an unexpected model: {:?}; evidence: {}", snapshot.session.resolved_model, fixture.display());
    if provider == "copilot" {
    let checklist = snapshot
        .exchange
        .iter()
        .find_map(|exchange| exchange.checklist.as_ref())
        .with_context(|| format!("native task creation was not published: {}", serde_json::to_string(&snapshot.exchange).unwrap_or_default()))?;
    ensure!(
        checklist.current.len() == 3,
        "native checklist lost tasks: {checklist:?}"
    );
    ensure!(
        checklist
            .current
            .iter()
            .all(|item| item.status == forge_harness::backend::ChecklistStatus::Completed),
        "native task updates did not settle: {checklist:?}"
    );
    ensure!(
        checklist.current.iter().any(|item| item.attributed),
        "tool execution was not attributed to selected checklist task"
    );
    }
    let history = forge_harness::storage::SqliteStore::open(Path::new(&initialize.data_root))?
        .list_exchange(&snapshot.session.id)?;
    let tools = history
        .iter()
        .flat_map(|exchange| exchange.tools().map(|(_, tool)| tool))
        .collect::<Vec<_>>();
    ensure!(
        tools.len() >= 3,
        "workflow did not exercise multiple tools: {tools:?}"
    );
    let failed_position = tools.iter().position(|tool| tool.output.contains("EXPECTED-WORKFLOW-FAILURE") && (tool.failed || command_exited(&tool.output, 7)))
        .with_context(|| format!("expected failure output was not retained on a failed tool: {tools:#?}"))?;
    ensure!(
        tools[failed_position + 1..]
            .iter()
            .any(|tool| (tool.title.contains("git status") || tool.output.contains("git status"))
                && tool.state() == forge_harness::turn::ToolState::Completed
                && (tool.kind == "command" || command_exited(&tool.output, 0))),
        "a successful git status command did not follow the expected failure: {tools:#?}"
    );
    ensure!(
        snapshot
            .exchange
            .iter()
            .all(|exchange| exchange.state() == forge_harness::exchange::ExchangeState::Complete),
        "expected command failure incorrectly failed the exchange"
    );
    ensure!(
        snapshot
            .exchange
            .iter()
            .any(|exchange| exchange.checkpoint_before.is_some()
                && exchange.checkpoint_after.is_some()),
        "tool workflow omitted checkpoint boundaries"
    );
    initialize.session_id = Some(snapshot.session.id.clone());
    let original_provider_id = snapshot
        .session
        .backend_session_id
        .clone()
        .context("missing durable provider identity")?;
    drop(snapshot);
    initial_backend.shutdown().await?;
    drop(broker);

    let mut reopened = HarnessBroker::initialize(initialize.clone())?;
    eprintln!("{provider}: reopened durable provider session for file editing; evidence: {}", fixture.display());
    resumed_backend = Some(reopened.backend_handle());
    ensure!(
        reopened.snapshot()?.session.name == "Workflow before first exchange",
        "local rename was lost on reopen"
    );
    ensure!(
        reopened.snapshot()?.session.backend_session_id.as_deref()
            == Some(original_provider_id.as_str()),
        "completed history discarded its provider identity"
    );
    dispatch(&mut reopened, 4, "session.prompt.submit", json!({"text":format!(
        "{file_write_instruction} Read output.txt using a tool, then append the marker RESUMED-WORKFLOW after its existing token. Preserve the token already present. Finish with RESUME-DONE."
    )})).await?;
    let resumed_content = std::fs::read_to_string(workspace.join("output.txt"))?;
    let resumed_gap = resumed_content.trim_start_matches('\u{feff}').trim()
        .strip_prefix("WORKFLOW-72931")
        .and_then(|content| content.strip_suffix("RESUMED-WORKFLOW"));
    ensure!(
        resumed_gap.is_some_and(|gap| gap.chars().all(char::is_whitespace)),
        "resumed tool turn failed to preserve and append the requested markers: {resumed_content:?}"
    );
    drop(reopened);
    if let Some(backend) = &resumed_backend { backend.shutdown().await?; }
    let mut task_service = WorkflowService::open(initialize.clone()).await?;
    eprintln!("{provider}: starting Harness Goal task after reopen; evidence: {}", fixture.display());
    let task_result: Result<()> = async {
    let objective = "Read output.txt with a tool and verify it contains both WORKFLOW-72931 and RESUMED-WORKFLOW. Do not modify files. Once verified, mark the goal complete with a brief summary.";
    task_service.request("session.goal.set", json!({"objective":objective})).await?;
    loop {
        task_service.assert_exchange_health()?;
        let snapshot = task_service.request("session.state.get", json!({})).await?;
        let session: forge_harness::session::HarnessSession = serde_json::from_value(snapshot["session"].clone())?;
        task_service.log_transition(format!("goal: {:?}", session.active_task.as_ref().map(|task| task.status())));
        if task_service.completed_goal(objective)? && session.active_task.is_none() {
            break;
        }
        // Completion can commit between the snapshot and the input-lane observation.
        if let Err(failure) = task_service.require_pending_work("completed goal").await
            && !task_service.completed_goal(objective)?
        {
            return Err(failure);
        }
        tokio::time::sleep(std::time::Duration::from_millis(25)).await;
    }
    task_service.wait_idle().await?;
    let final_exchange = forge_harness::storage::SqliteStore::open(Path::new(&initialize.data_root))?.list_exchange(task_service.session_id.as_str())?;
    ensure!(
        final_exchange.len() >= 3,
        "goal did not create its own exchange"
    );
    ensure!(
        final_exchange
            .iter()
            .all(|exchange| exchange.finalization_error.is_none()),
        "workflow retained a finalization error"
    );
    ensure!(final_exchange.last().is_some_and(|exchange| exchange.tools().any(|(_, tool)| tool.output.contains("WORKFLOW-72931") && tool.output.contains("RESUMED-WORKFLOW"))), "goal did not verify the resumed file through a tool; evidence: {}", fixture.display());
    Ok(())
    }.await;
    let task_cleanup = task_service.shutdown().await;
    task_result?;
    task_cleanup?;
    eprintln!(
        "{provider}: read/write, expected tool failure, recovery, checkpoints, renamed durable reopen, edit, and goal completion passed"
    );
    Ok(())
    }.await;
    let initial_cleanup = initial_backend.shutdown().await;
    let resumed_cleanup = match resumed_backend {
        Some(backend) => backend.shutdown().await,
        None => Ok(()),
    };
    result.with_context(|| format!("{provider} tool workflow; evidence: {}", fixture.display()))?;
    initial_cleanup?;
    resumed_cleanup
}

/// Exercise plan question admission, explicit user feedback, and review-ready submission.
async fn plan_feedback(provider: &str) -> Result<()> {
    let fixture = tempfile::Builder::new()
        .prefix("forge-live-plan-")
        .tempdir()?
        .keep();
    eprintln!("{provider}: plan feedback; evidence: {}", fixture.display());
    let initialize = initialize(provider, &fixture)?;
    let workspace = Path::new(&initialize.workspace).to_owned();
    std::fs::write(workspace.join("settings.json"), "{\"name\":\"workflow\"}\n")?;
    let mut service = WorkflowService::open(initialize).await?;
    let result: Result<()> = async {
    service.request("session.permission.set", json!({"change":{"kind":"set_approval","mode":"allow"}})).await?;
    service.request("session.prompt.submit", json!({"text":
        "/plan Add a test_type field to settings.json. Do not modify workspace files. Your first action must call question_ask with exactly one question asking which test type the field should select. Provide exactly two options named Unit tests and Integration tests. After my answer, propose the complete JSON configuration with test_type set to unit or integration accordingly, then submit the plan for review. Do not edit or submit the plan until I answer."
    })).await?;
    let snapshot = service.wait_plan(forge_harness::plan::PlanState::AwaitingInput).await?;
    let plan: forge_harness::plan::PlanRecord = serde_json::from_value(snapshot["active_plan"].clone())?;
    ensure!(
        plan.state == forge_harness::plan::PlanState::AwaitingInput,
        "provider did not pause for plan feedback: {:?}",
        plan.state
    );
    let elicitation = plan.elicitation.context("missing plan question")?;
    let question = elicitation
        .current_question()
        .context("plan question was empty")?;
    ensure!(
        question.options.len() == 2,
        "question options do not match the requested choice"
    );
    let selected = question
        .options
        .iter()
        .find(|option| option.label.to_ascii_lowercase().contains("integration"))
        .context("missing Integration tests option")?;
    service.request("session.question.answer", json!({
        "question_id":question.id,"response":{"kind":"selected","option":selected.label,"feedback":null}
    })).await?;
    service.request(
        "session.question.continue",
        serde_json::Value::Null,
    )
    .await?;
    let snapshot = service.wait_plan(forge_harness::plan::PlanState::AwaitingReview).await?;
    let plan: forge_harness::plan::PlanRecord = serde_json::from_value(snapshot["active_plan"].clone())?;
    ensure!(
        plan.state == forge_harness::plan::PlanState::AwaitingReview,
        "plan did not become review-ready: {:?}",
        plan.state
    );
    ensure!(
        plan.elicitation.is_none(),
        "resolved plan question remained active"
    );
    ensure!(
        plan.question_ledger.resolution.len() == 1,
        "feedback was replayed or lost"
    );
    let document = forge_harness::plan::PlanFileStore::new(&service.data_root, &workspace)
        .read_working_document(&service.session_id, &plan.id)?;
    let design = document.design.context("submitted plan omitted its semantic design")?;
    let proposed: serde_json::Value = serde_json::from_str(design.proposed.get("settings.json").context("submitted design omitted settings.json")?)?;
    ensure!(proposed["name"] == "workflow" && proposed["test_type"] == "integration", "submitted design lost the selected configuration: {proposed}; evidence: {}", fixture.display());
    ensure!(design.check_preview.iter().any(|check| check.id == "fixture-whitespace" && check.selected), "submitted design omitted its configured automatic check; evidence: {}", fixture.display());
    ensure!(
        snapshot["artifact"].as_array().is_some_and(|artifact| artifact.len() == 1)
            && Path::new(snapshot["artifact"][0]["working_path"].as_str().context("missing artifact path")?).exists(),
        "submitted plan artifact missing"
    );
    ensure!(
        std::fs::read_to_string(workspace.join("settings.json"))? == "{\"name\":\"workflow\"}\n",
        "planning modified the source file"
    );
    eprintln!(
        "{provider}: plan question, explicit user answer, feedback continuation, and review-ready artifact passed"
    );
    Ok(())
    }.await;
    let cleanup = service.shutdown().await;
    result.with_context(|| format!("{provider} plan feedback; evidence: {}", fixture.display()))?;
    cleanup
}

/// Construct an isolated provider for active-tool interruption and retry.
fn backend(
    provider: &str,
    initialize: &InitializeRequest,
    request: &BackendRequest,
) -> Result<Box<dyn Backend>> {
    let permission = PermissionCoordinator::transient(&request.workspace)?;
    let root = Path::new(&initialize.data_root);
    let trace = Arc::new(TraceStore::open(root)?);
    Ok(match provider {
        "codex" => Box::new(CodexBackend::new_with_permission_coordinator(
            vec!["codex".into(), "app-server".into()],
            permission,
            trace,
            Some(root.into()),
        )?) as Box<dyn Backend>,
        "copilot" => Box::new(CopilotBackend::new_with_permission_coordinator(
            Vec::new(),
            permission,
            trace,
            Some(root.into()),
        )?) as Box<dyn Backend>,
        _ => unreachable!("test provider is explicit"),
    })
}

/// Interrupt an actual command, drain its execution, and submit a successful replacement turn.
async fn cancelled_tool_retry(provider: &str) -> Result<()> {
    let fixture = tempfile::Builder::new()
        .prefix("forge-live-cancel-")
        .tempdir()?
        .keep();
    eprintln!(
        "{provider}: cancellation and retry; evidence: {}",
        fixture.display()
    );
    let initialize = initialize(provider, &fixture)?;
    forge_harness::settings::SettingsStore::open(Path::new(&initialize.data_root))?
        .configure_catalog(Path::new(
            initialize.config_dir.as_deref().context("missing config")?,
        ))?;
    let catalog = super::catalog_request(
        Path::new(&initialize.workspace),
        &format!("{provider}-cancel-retry"),
    );
    let mut request = super::prompt_request(&catalog);
    request.model = "gpt-6-luna".into();
    request.approval_mode = ApprovalMode::Allow;
    request.access.sandbox = provider == "codex";
    let sentinel = Path::new(&initialize.workspace).join("command-started.txt");
    let finished = Path::new(&initialize.workspace).join("command-finished.txt");
    let command = if cfg!(windows) {
        format!(
            "Set-Content -LiteralPath '{}' -Value 'CANCEL-WORKFLOW-STARTED' -NoNewline; Start-Sleep -Seconds 4; Set-Content -LiteralPath '{}' -Value 'CANCEL-WORKFLOW-FINISHED' -NoNewline",
            sentinel
                .to_string_lossy()
                .replace('\\', "/")
                .replace('\'', "''"),
            finished
                .to_string_lossy()
                .replace('\\', "/")
                .replace('\'', "''")
        )
    } else {
        format!(
            "sh -c \"printf CANCEL-WORKFLOW-STARTED > '{}'; sleep 4; printf CANCEL-WORKFLOW-FINISHED > '{}'\"",
            sentinel.to_string_lossy().replace('\'', "'\\''"),
            finished.to_string_lossy().replace('\'', "'\\''")
        )
    };
    request.input = BackendInput::from_text(format!(
        "Run exactly this command using your command tool: `{command}`. Wait until the command exits before reporting completion. If the tool returns a still-running session, keep polling that session until it exits. Do not run other commands."
    ));
    let backend = backend(provider, &initialize, &request)?;
    let result: Result<()> = async {
        request.backend_session_id = backend.initialize_session(request.clone()).await?;
        let (sink, mut stream) = forge_harness::backend::events::channel(InvocationScope::new(request.owner()?, "cancel-tool"));
        let execution = backend.prompt_stream(request.clone(), sink);
        tokio::pin!(execution);
        let mut observed = Vec::new();
        loop {
            tokio::select! {
                output = &mut execution => anyhow::bail!("provider completed before active-tool cancellation: {output:?}; observed events: {observed:#?}"),
                event = stream.recv() => {
                    let event = event?.context("event stream ended before command started")?;
                    if let BackendEvent::Tool { update: ToolUpdate::Requested { invocation }, .. } = &event {
                        eprintln!("{provider}: requested tool {} ({:?})", invocation.name, invocation.kind);
                    }
                    observed.push(event);
                }
                _ = tokio::time::sleep(std::time::Duration::from_millis(25)) => {
                    if std::fs::read_to_string(&sentinel).is_ok_and(|content| content == "CANCEL-WORKFLOW-STARTED") {
                        eprintln!("{provider}: command wrote its start sentinel, cancelling active execution");
                        break;
                    }
                }
            }
        }
        let command_started_at = tokio::time::Instant::now();
        let cancellation_session_id = request.harness_session_id.clone();
        let cancellation = async {
            tokio::join!(backend.cancel_session(&cancellation_session_id), &mut execution)
        };
        tokio::pin!(cancellation);
        let mut stream_open = true;
        let (cancelled, completed) = loop {
            tokio::select! {
                settled = &mut cancellation => break settled,
                event = stream.recv(), if stream_open => {
                    if let Some(event) = event? {
                        observed.push(event);
                    } else {
                        stream_open = false;
                    }
                }
            }
        };
        cancelled?;
        while let Some(event) = stream.try_next()? {
            observed.push(event);
        }
        std::fs::write(fixture.join("cancelled-events.json"), serde_json::to_vec_pretty(&observed)?)?;
        match completed {
            Err(BackendError::Cancelled) => {}
            Ok(_) => ensure!(observed.iter().any(|event| matches!(event,
                BackendEvent::Turn { update: forge_harness::backend::TurnUpdate::Finished {
                    outcome: forge_harness::turn::TurnOutcome::Cancelled | forge_harness::turn::TurnOutcome::Interrupted, ..
                }, .. }
            )), "cancelled command omitted interrupted-turn evidence: {observed:#?}"),
            Err(failure) => return Err(failure.into()),
        }
        // Observe past the command's scheduled write; a closed turn alone does not prove process cancellation.
        tokio::time::sleep_until(command_started_at + std::time::Duration::from_secs(5)).await;
        ensure!(!finished.exists(), "cancelled native command continued executing after turn settlement");
        request.backend_session_id = backend.active_session_id_for(&request.harness_session_id).await;
        request.input = BackendInput::from_text("The previous command was intentionally cancelled. Use a tool to read input.txt, then reply with its exact token. Do not run another sleep.");
        let retried = backend.prompt(request).await?;
        ensure!(retried.event.iter().any(|event| matches!(event, BackendEvent::Tool { update: ToolUpdate::Finished { .. }, .. })), "retry did not execute a tool");
        ensure!(serde_json::to_string(&retried.event)?.contains("WORKFLOW-72931"), "retry lost workspace access");
        eprintln!("{provider}: active command cancellation, drained execution, and successful tool retry passed");
        Ok(())
    }.await;
    backend.shutdown().await?;
    result.with_context(|| {
        format!(
            "{provider} cancellation workflow; evidence: {}",
            fixture.display()
        )
    })
}

#[tokio::test]
#[ignore = "requires authenticated Codex and executes tools in an isolated project"]
async fn codex_tool_task_and_resume_workflow() -> Result<()> {
    tool_workflow("codex").await
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and executes tools in an isolated project"]
async fn copilot_tool_task_and_resume_workflow() -> Result<()> {
    tool_workflow("copilot").await
}

#[tokio::test]
#[ignore = "requires authenticated Codex and interrupts a real command"]
async fn codex_cancelled_tool_can_retry() -> Result<()> {
    cancelled_tool_retry("codex").await
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and interrupts a real command"]
async fn copilot_cancelled_tool_can_retry() -> Result<()> {
    cancelled_tool_retry("copilot").await
}

#[tokio::test]
#[ignore = "requires authenticated Codex and performs planning with user feedback"]
async fn codex_plan_question_and_review_workflow() -> Result<()> {
    plan_feedback("codex").await
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and performs planning with user feedback"]
async fn copilot_plan_question_and_review_workflow() -> Result<()> {
    plan_feedback("copilot").await
}
