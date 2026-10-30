use super::*;
use crate::session::SessionStore;
use crate::settings::{PortableSettings, SettingScope};
use crate::storage::SqliteStore;

fn session(id: &str, workspace: &Path) -> HarnessSession {
    serde_json::from_value(json!({
        "id":id,"primary_agent_id":HarnessSession::primary_agent_id(id),"name":id,
        "workspace":workspace,"backend":"copilot","model":"default","effort":"medium",
        "provider_fork_state":{"state":"ready"},"approval_mode":"ask",
        "created_at_ms":1,"updated_at_ms":1
    }))
    .unwrap()
}

fn exchange(session: &HarnessSession, id: &str, text: &str) -> Exchange {
    serde_json::from_value(json!({
        "id":id,"session_id":session.id,"agent_id":session.primary_agent_id,"ordinal":1,
        "prompt":text,"state":"complete","created_at_ms":0,"completed_at_ms":1,
        "attributed_matches_checkpoint":true,"node_list":[]
    }))
    .unwrap()
}

struct Fixture {
    root: tempfile::TempDir,
    workspace: tempfile::TempDir,
    scope: Arc<HarnessInspectionScope>,
    _lease: InspectionLease,
}

impl Fixture {
    fn new() -> Result<Self> {
        let root = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let session = session("current", workspace.path());
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&session)?;
        store.save_exchange(&exchange(&session, "exchange", "Recover deletion labels"))?;
        let settings = SettingsStore::open(root.path())?;
        settings.configure_catalog(workspace.path())?;
        let policy = Arc::new(CompiledPermissionDocument::compile(
            crate::permissions::document::default_permission_document(),
            workspace.path(),
        ));
        let (scope, lease) = HarnessInspectionScope::open(
            session,
            root.path().into(),
            "permissions.json".into(),
            policy,
            Arc::new(AtomicBool::new(false)),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        )?;
        Ok(Self {
            root,
            workspace,
            scope,
            _lease: lease,
        })
    }

    fn invoke(&self, name: &str, args: Value) -> Result<Value> {
        super::super::validate_arguments(&super::super::ControlToolInvocation {
            task_permit: None,
            name: name.into(),
            arguments: args.clone(),
        })?;
        crate::codec::from_str(&self.scope.read(name, args)?).map_err(Into::into)
    }
}

#[test]
fn session_inspection_defaults_filters_config_and_readonly_storage() -> Result<()> {
    let fixture = Fixture::new()?;
    let other_workspace = tempfile::tempdir()?;
    let other = session("other", other_workspace.path());
    let mut store = SqliteStore::open(fixture.root.path())?;
    store.save_session(&other)?;
    store.save_exchange(&exchange(&other, "other-exchange", "Recover earlier work"))?;
    let before = std::fs::read(fixture.root.path().join("settings.db"))?;
    let response = fixture.invoke("harness_session_search", json!({"query":"recover"}))?;
    assert_eq!(response["items"].as_array().unwrap().len(), 2);
    for filters in [
        json!({"query":"recover","session_id":"current"}),
        json!({"query":"recover","workspace":fixture.workspace.path()}),
        json!({"query":"recover","session_id":"current","workspace":fixture.workspace.path()}),
    ] {
        assert_eq!(
            fixture.invoke("harness_session_search", filters)?["items"]
                .as_array()
                .unwrap()
                .len(),
            1
        );
    }
    assert!(
        fixture
            .invoke(
                "harness_session_search",
                json!({"query":"x","session_id":"missing"})
            )
            .is_err()
    );
    assert!(
        fixture
            .invoke("harness_session_search", json!({"query":""}))
            .is_err()
    );
    let status = fixture.invoke("harness_session_status", json!({}))?;
    assert_eq!(status["items"][0]["session_id"], "current");
    assert_eq!(status["items"][0]["approval_mode"], "ask");
    assert!(status["items"][0]["task"].is_null());
    let config = fixture.invoke("harness_config", json!({}))?;
    assert_eq!(
        config["items"][0]["locations"]["config_directory"],
        json!(dunce::canonicalize(fixture.workspace.path())?)
    );
    assert!(config.to_string().contains("plan_auto_approve_revisions"));
    let messages = fixture.invoke("harness_session_exchange_list", json!({}))?;
    assert_eq!(
        messages["items"][0]["messages"][0]["text"],
        "Recover deletion labels"
    );
    assert!(
        fixture
            .invoke(
                "harness_session_exchange",
                json!({"exchange_id":"other-exchange"})
            )
            .is_err()
    );
    assert_eq!(
        before,
        std::fs::read(fixture.root.path().join("settings.db"))?
    );
    Ok(())
}

#[test]
fn disabling_history_keeps_config_and_skill_but_revokes_runtime_reads() -> Result<()> {
    let fixture = Fixture::new()?;
    let mut settings = SettingsStore::open(fixture.root.path())?;
    let snapshot = settings.get(&fixture.scope.session.workspace, "current", "copilot")?;
    settings.update(
        snapshot.revision,
        SettingScope::Session,
        &fixture.scope.session.workspace,
        "current",
        &PortableSettings {
            session_tools: Some(false),
            ..Default::default()
        },
    )?;
    assert!(fixture.scope.check("harness_session_list").is_err());
    assert!(fixture.invoke("harness_config", json!({})).is_ok());
    let (scope, _lease) = HarnessInspectionScope::open(
        fixture.scope.session.clone(),
        fixture.root.path().into(),
        "permissions.json".into(),
        fixture.scope.policy.clone(),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    assert!(!scope.enabled());
    let mut context = super::super::ControlTurnContext::inactive(crate::backend::PromptMode::Chat);
    context.harness = Some(scope);
    let tools = super::super::ControlToolRegistry.definition_list_for_context(Some(&context), true);
    assert!(tools.iter().any(|tool| tool.name == "harness_config"));
    assert!(
        !tools
            .iter()
            .any(|tool| tool.name.starts_with("harness_session_"))
    );
    assert!(
        !super::super::ControlToolRegistry
            .definition_list_for_context(Some(&context), false)
            .iter()
            .any(|tool| is_tool(tool.name))
    );
    context.mode = crate::backend::PromptMode::Background;
    assert!(
        super::super::ControlToolRegistry
            .definition_list_for_context(Some(&context), true)
            .is_empty()
    );
    let skill = include_str!(
        "../../../../../../../../rulesync-global/.rulesync/skills/harness-sessions/SKILL.md"
    );
    assert!(skill.contains("harness_config"));
    let skill_directory = fixture
        .workspace
        .path()
        .join(".rulesync/skills/harness-sessions");
    std::fs::create_dir_all(&skill_directory)?;
    std::fs::write(skill_directory.join("SKILL.md"), skill)?;
    let resources = crate::settings::SessionResources::load(
        fixture.root.path(),
        &crate::backend::BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "current".into(),
            workspace: fixture.scope.session.workspace.clone(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            backend_session_id: None,
        },
        "copilot",
        "copilotcli",
    )?;
    assert!(
        resources
            .selected_skill
            .iter()
            .any(|skill| skill.name == "harness-sessions")
    );
    Ok(())
}

#[test]
fn paging_preserves_unicode_snapshot_and_rejects_foreign_cursors() -> Result<()> {
    let fixture = Fixture::new()?;
    let mut store = SqliteStore::open(fixture.root.path())?;
    let text = "retained \u{96ea}".repeat(12000);
    store.save_exchange(&exchange(&fixture.scope.session, "exchange", &text))?;
    let mut arguments = json!({});
    let mut fragments = String::new();
    loop {
        let response = fixture.invoke("harness_session_exchange_list", arguments.clone())?;
        if let Some(fragment) = response["fragment"].as_str() {
            fragments.push_str(fragment);
        }
        let Some(cursor) = response["next_cursor"].as_str() else {
            break;
        };
        assert!(
            fixture
                .invoke("harness_session_list", json!({"cursor":cursor}))
                .is_err()
        );
        arguments["cursor"] = json!(cursor);
        store.save_exchange(&exchange(&fixture.scope.session, "exchange", "new version"))?;
    }
    let reconstructed: Value = serde_json::from_str(fragments.trim())?;
    assert_eq!(reconstructed["messages"][0]["text"], text);
    fixture.scope.cancelled.store(true, Ordering::Release);
    assert!(fixture.scope.check("harness_config").is_err());
    let scope = fixture.scope.clone();
    drop(fixture);
    assert!(scope.pages.lock().unwrap().is_empty());
    assert!(scope.check("harness_config").is_err());
    Ok(())
}

#[test]
fn log_metadata_excludes_private_content_and_reports_absence() -> Result<()> {
    let fixture = Fixture::new()?;
    assert_eq!(
        fixture.invoke("harness_session_log", json!({}))?["items"][0]["unavailable"],
        "logs"
    );
    fixture.scope.trace.configure(true)?;
    fixture.scope.trace.record(
        "current",
        "provider.reply",
        json!({
            "method":"turn/complete","status":"failed","reasoning":"private reasoning",
            "authorization":"credential","body":"private transport","count":3
        }),
    );
    let log = fixture.invoke("harness_session_log", json!({}))?;
    let encoded = log.to_string();
    assert!(encoded.contains("turn/complete"));
    assert!(!encoded.contains("private") && !encoded.contains("credential"));
    let native = crate::trace::inspection_record(&json!({
        "event":"copilot.session.event",
        "payload":{"type":"tool.execution_complete","data":{
            "toolCallId":"call","success":true,"arguments":"credential","reasoningText":"private"
        }}
    }));
    assert_eq!(native["payload"]["type"], "tool.execution_complete");
    assert_eq!(native["payload"]["data"]["toolCallId"], "call");
    assert_eq!(native["payload"]["data"]["success"], true);
    assert!(!native.to_string().contains("credential") && !native.to_string().contains("private"));
    let method = "diagnostic".repeat(300);
    fixture
        .scope
        .trace
        .record("current", "long.metadata", json!({"method":method}));
    let path = PathBuf::from(fixture.scope.trace.session_status("current").path);
    std::fs::copy(&path, path.with_extension("jsonl.1"))?;
    std::fs::write(&path, "")?;
    let rotated = fixture.invoke("harness_session_log", json!({}))?;
    assert!(
        rotated["items"]
            .as_array()
            .unwrap()
            .iter()
            .any(|record| record["payload"]["method"] == method)
    );
    assert!(
        fixture
            .invoke("harness_config", json!({"session_id":"other"}))
            .is_err()
    );
    Ok(())
}

#[test]
fn session_status_separates_approval_access_and_active_task() -> Result<()> {
    let fixture = Fixture::new()?;
    let mut store = SqliteStore::open(fixture.root.path())?;
    let mut selected = fixture.scope.session.clone();
    selected.approval_mode = crate::session::ApprovalMode::Deny;
    selected.access.write_access = crate::session::WriteScope::Off;
    selected.default_approval = crate::session::ApprovalMode::Allow;
    selected.active_task = Some(crate::task::ActiveTask::goal(
        "Inspect".into(),
        Default::default(),
        false,
        1,
    ));
    store.save_session(&selected)?;
    let (scope, _lease) = HarnessInspectionScope::open(
        selected,
        fixture.root.path().into(),
        "permissions.json".into(),
        fixture.scope.policy.clone(),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    let value: Value = serde_json::from_str(&scope.read("harness_session_status", json!({}))?)?;
    assert_eq!(value["items"][0]["approval_mode"], "deny");
    assert_eq!(value["items"][0]["access"]["write_access"], "off");
    assert_eq!(value["items"][0]["task"]["kind"], "goal");
    let config: Value = serde_json::from_str(&scope.read("harness_config", json!({}))?)?;
    assert_eq!(config["items"][0]["approval_defaults"]["work"], "allow");
    Ok(())
}

#[test]
fn search_respects_workspace_read_boundary_and_explicit_denials() -> Result<()> {
    let mut fixture = Fixture::new()?;
    let outside = tempfile::tempdir()?;
    let other = session("outside", outside.path());
    let mut store = SqliteStore::open(fixture.root.path())?;
    store.save_session(&other)?;
    store.save_exchange(&exchange(
        &other,
        "outside-exchange",
        "Recover private work",
    ))?;
    let session = HarnessSession {
        access: crate::session::AccessPolicy {
            read_access: crate::session::DirectoryScope::Workspace,
            ..Default::default()
        },
        ..fixture.scope.session.clone()
    };
    let (scope, lease) = HarnessInspectionScope::open(
        session,
        fixture.root.path().into(),
        "permissions.json".into(),
        fixture.scope.policy.clone(),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    fixture.scope = scope;
    fixture._lease = lease;
    assert_eq!(
        fixture.invoke("harness_session_search", json!({"query":"recover"}))?["items"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert!(
        fixture
            .invoke("harness_session_status", json!({"session_id":"outside"}))
            .is_err()
    );
    let denied = CompiledPermissionDocument::compile(
        crate::permissions::document::parse_permission_document(
            r#"{"permission":{"read":{"*":"deny"}}}"#,
        )?,
        fixture.workspace.path(),
    );
    let (scope, _lease) = HarnessInspectionScope::open(
        fixture.scope.session.clone(),
        fixture.root.path().into(),
        "permissions.json".into(),
        Arc::new(denied),
        Arc::new(AtomicBool::new(false)),
        fixture.scope.trace.clone(),
    )?;
    assert!(!scope.allowed(&fixture.scope.session.workspace)?);
    Ok(())
}

#[test]
fn conversation_order_tool_output_and_ambiguous_occurrences_are_preserved() -> Result<()> {
    use crate::backend::{TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};
    let fixture = Fixture::new()?;
    let mut store = SqliteStore::open(fixture.root.path())?;
    let mut exchange = exchange(&fixture.scope.session, "exchange", "Initial question");
    exchange.phase = crate::exchange::ExchangePhase::Running;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    exchange.resume(0)?;
    let address = crate::turn::test_support::address(&exchange, "one");
    exchange.start_turn(address.clone(), 0)?;
    let event = crate::turn::test_support::message(
        &address,
        "progress",
        Some(crate::backend::MessageRole::Commentary),
        "progress excluded",
    );
    exchange.observe_turn(&event, 1)?;
    exchange.append_input(
        crate::exchange::InputIntent::Steering,
        "Follow-up question".into(),
        2,
    )?;
    let event = crate::turn::test_support::message(
        &address,
        "answer",
        Some(crate::backend::MessageRole::Response),
        "Final answer",
    );
    exchange.observe_turn(&event, 3)?;
    let output = format!("{}NEEDLE\0{}", "x".repeat(15998), "\u{96ea}".repeat(20000));
    crate::turn::test_support::tool(
        &mut exchange,
        &address,
        "reused",
        ToolInvocation {
            kind: ToolActivityKind::ToolCall,
            name: "inspect".into(),
            title: "inspect".into(),
            arguments: json!({}),
        },
        [
            ToolUpdate::Started,
            ToolUpdate::Output {
                update: TextUpdate::Replace(output.clone()),
            },
            ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        ],
        4,
    )?;
    store.save_exchange(&exchange)?;
    let messages = fixture.invoke("harness_session_exchange_list", json!({}))?;
    assert_eq!(
        messages["items"][0]["messages"],
        json!([
            {"role":"user","text":"Initial question"},
            {"role":"user","text":"Follow-up question","intent":"steering"},
            {"role":"assistant","text":"Final answer"},
        ])
    );
    let matches = fixture.invoke(
        "harness_session_search",
        json!({"query":"needle","search_in":["tool_outputs"]}),
    )?;
    assert!(!matches["items"].as_array().unwrap().is_empty());
    let turn_id = exchange.turn[0].id().to_owned();
    let mut args = json!({"exchange_id":"exchange","tool_call_id":"reused","turn_id":turn_id,"fields":["output","input"]});
    let mut reconstructed = String::new();
    let mut fragments = String::new();
    let mut unavailable = false;
    loop {
        let response = fixture.invoke("harness_session_tool", args.clone())?;
        let mut records = response["items"].as_array().cloned().unwrap_or_default();
        if let Some(fragment) = response["fragment"].as_str() {
            fragments.push_str(fragment);
            if response["record_complete"] == true {
                records.push(serde_json::from_str(fragments.trim())?);
                fragments.clear();
            }
        }
        for record in records {
            if record["field"] == "output" {
                assert_eq!(
                    record["offset"].as_u64().unwrap() as usize,
                    reconstructed.chars().count()
                );
                reconstructed.push_str(record["text"].as_str().unwrap());
            }
            unavailable |= record["field"] == "input" && record["unavailable"] == true;
        }
        let Some(cursor) = response["next_cursor"].as_str() else {
            break;
        };
        args["cursor"] = json!(cursor);
    }
    assert_eq!(reconstructed, output);
    assert!(unavailable);
    let mut raw = serde_json::to_value(&exchange)?;
    let mut duplicate = raw["turn"][0].clone();
    duplicate["id"] = json!("second-local-turn");
    duplicate["provider"]["turn_id"] = json!("two");
    raw["turn"].as_array_mut().unwrap().push(duplicate);
    let tools = raw["entry"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|entry| entry.get_mut("commentary")?.get_mut("tool")?.as_array_mut())
        .find(|tools| !tools.is_empty())
        .unwrap();
    let mut duplicate_tools = tools[0].clone();
    duplicate_tools["turn_id"] = json!("second-local-turn");
    tools.push(duplicate_tools);
    raw["node_list"].as_array_mut().unwrap().push(json!({
        "kind":"turn_content", "id":"second-local-turn:tool:reused",
        "turn_id":"second-local-turn", "item":{"kind":"tool","id":"reused"}
    }));
    let duplicated: Exchange = serde_json::from_value(raw)?;
    store.save_exchange(&duplicated)?;
    assert!(
        fixture
            .invoke(
                "harness_session_tool",
                json!({"exchange_id":"exchange","tool_call_id":"reused"})
            )
            .is_err()
    );
    assert!(fixture.invoke("harness_session_tool",json!({"exchange_id":"exchange","tool_call_id":"reused","turn_id":turn_id,"fields":["input"]})).is_ok());
    Ok(())
}

#[test]
fn repository_filter_groups_linked_worktrees_without_grouping_other_repositories() -> Result<()> {
    let fixture = Fixture::new()?;
    let repository = fixture.workspace.path();
    let git = |arguments: &[&str]| -> Result<()> {
        let result = std::process::Command::new("git")
            .args(arguments)
            .current_dir(repository)
            .output()?;
        ensure!(
            result.status.success(),
            "git failed: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        Ok(())
    };
    git(&["init", "--quiet"])?;
    git(&[
        "-c",
        "user.name=Fixture",
        "-c",
        "user.email=fixture@example.invalid",
        "commit",
        "--allow-empty",
        "--quiet",
        "-m",
        "fixture",
    ])?;
    let worktree = fixture.root.path().join("worktree");
    git(&[
        "worktree",
        "add",
        "--quiet",
        "-b",
        "inspection",
        worktree.to_str().context("test path not UTF-8")?,
    ])?;
    let mut store = SqliteStore::open(fixture.root.path())?;
    store.save_session(&session("worktree", &worktree))?;
    let outside = tempfile::tempdir()?;
    store.save_session(&session("outside", outside.path()))?;
    let id = repository_id(repository.to_str().unwrap())?.unwrap();
    let response = fixture.invoke("harness_session_list", json!({"repository_id":id}))?;
    assert_eq!(response["items"].as_array().unwrap().len(), 2);
    let exact = fixture.invoke(
        "harness_session_list",
        json!({"repository_id":id,"workspace":worktree}),
    )?;
    assert_eq!(exact["items"].as_array().unwrap().len(), 1);
    Ok(())
}
