mod contract;
mod page;
#[cfg(test)]
mod tests;

use crate::exchange::{Exchange, ExchangeNode, ExchangeState};
use crate::permissions::{
    directory::DirectoryPolicy,
    document::PermissionDecision,
    matcher::{CompiledPermissionDocument, PermissionRequest, PermissionTarget},
};
use crate::session::HarnessSession;
use crate::settings::SettingsStore;
use crate::storage::inspection::SessionReader;
use crate::turn::{MessageDelivery, MessageKind, TurnItem};
use anyhow::{Context, Result, ensure};
pub(super) use contract::definitions;
use page::InspectionPage;
use serde::Deserialize;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};

pub(super) fn is_tool(name: &str) -> bool {
    name == "harness_config" || name.starts_with("harness_session_")
}

/// Owns authenticated history reads and immutable continuation files for one root turn.
pub(crate) struct HarnessInspectionScope {
    session: HarnessSession,
    data_root: PathBuf,
    permission_path: String,
    policy: Arc<CompiledPermissionDocument>,
    cancelled: Arc<AtomicBool>,
    active: AtomicBool,
    pages: Mutex<HashMap<String, InspectionPage>>,
    trace: Arc<crate::trace::TraceStore>,
    enabled: bool,
}

pub(crate) struct InspectionLease(Arc<HarnessInspectionScope>);
impl Drop for InspectionLease {
    fn drop(&mut self) {
        self.0.active.store(false, Ordering::Release);
        self.0
            .pages
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clear();
    }
}

#[derive(Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct Inspection {
    session_id: Option<String>,
    repository_id: Option<String>,
    workspace: Option<String>,
    query: Option<String>,
    search_in: Option<Vec<String>>,
    exchange_id: Option<String>,
    tool_call_id: Option<String>,
    turn_id: Option<String>,
    fields: Option<Vec<String>>,
    cursor: Option<String>,
}

impl HarnessInspectionScope {
    pub(crate) fn open(
        session: HarnessSession,
        data_root: PathBuf,
        permission_path: String,
        policy: Arc<CompiledPermissionDocument>,
        cancelled: Arc<AtomicBool>,
        trace: Arc<crate::trace::TraceStore>,
    ) -> Result<(Arc<Self>, InspectionLease)> {
        let enabled = SettingsStore::read_only(&data_root)?
            .get(&session.workspace, &session.id, &session.backend)?
            .desired
            .session_tools
            .unwrap_or(true);
        let scope = Arc::new(Self {
            session,
            data_root,
            permission_path,
            policy,
            cancelled,
            trace,
            enabled,
            active: AtomicBool::new(true),
            pages: Mutex::default(),
        });
        Ok((scope.clone(), InspectionLease(scope)))
    }

    pub(crate) fn enabled(&self) -> bool {
        self.enabled
    }

    fn check(&self, name: &str) -> Result<()> {
        ensure!(
            self.active.load(Ordering::Acquire) && !self.cancelled.load(Ordering::Acquire),
            "Harness inspection belongs to an inactive interaction"
        );
        if name != "harness_config" {
            ensure!(
                SettingsStore::read_only(&self.data_root)?
                    .get(
                        &self.session.workspace,
                        &self.session.id,
                        &self.session.backend
                    )?
                    .desired
                    .session_tools
                    .unwrap_or(true),
                "Harness session tools are disabled in /config"
            );
        }
        Ok(())
    }

    fn allowed(&self, workspace: &str) -> Result<bool> {
        let mut request = PermissionRequest {
            id: "harness-inspection".into(),
            provider: "harness".into(),
            reason: None,
            target_list: vec![PermissionTarget::Read {
                path: workspace.into(),
            }],
        };
        let mut policy = DirectoryPolicy {
            access: self.session.access.clone(),
            workspace: self.session.workspace.clone().into(),
            session_id: self.session.id.clone(),
            settings_root: None,
        };
        policy.access = policy.access_from(&SettingsStore::read_only(&self.data_root)?)?;
        Ok(policy.evaluate(&self.policy, &mut request)?.decision == PermissionDecision::Allow)
    }

    pub(crate) async fn invoke(self: &Arc<Self>, name: &str, arguments: Value) -> Result<String> {
        self.check(name)?;
        let scope = self.clone();
        let task_name = name.to_owned();
        let response =
            tokio::task::spawn_blocking(move || scope.read(&task_name, arguments)).await??;
        self.check(name)?;
        Ok(response)
    }

    fn read(&self, name: &str, mut arguments: Value) -> Result<String> {
        self.check(name)?;
        let input: Inspection = serde_json::from_value(arguments.clone())?;
        if name == "harness_session_search" {
            ensure!(
                input
                    .query
                    .as_deref()
                    .is_some_and(|query| !query.trim().is_empty()),
                "search query must not be empty"
            );
        }
        arguments
            .as_object_mut()
            .context("arguments must be an object")?
            .remove("cursor");
        let identity = format!("{name}:{}", serde_json::to_string(&arguments)?);
        if let Some(cursor) = &input.cursor {
            let (id, offset) = cursor
                .rsplit_once(':')
                .context("invalid inspection cursor")?;
            let offset = offset.parse::<u64>().context("invalid inspection offset")?;
            let mut pages = self
                .pages
                .lock()
                .map_err(|_| anyhow::anyhow!("inspection snapshot lock poisoned"))?;
            let page = pages
                .get_mut(id)
                .context("cursor expired or belongs to another interaction")?;
            ensure!(page.identity == identity, "cursor belongs to another query");
            for workspace in &page.workspace {
                ensure!(
                    self.allowed(workspace)?,
                    "read authority for snapshot was revoked"
                );
            }
            self.check(name)?;
            return self.page_response(id, page, offset);
        }
        let mut page = InspectionPage::new(identity)?;
        let reader = SessionReader::open(&self.data_root)?;
        if matches!(name, "harness_session_list" | "harness_session_search") {
            if let Some(id) = &input.session_id {
                self.selected(&reader, Some(id))?;
            }
            let mut unavailable_inputs = false;
            reader.sessions(|session| {
                self.check(name)?;
                if !self.allowed(&session.workspace)? { return Ok(()); }
                if input.session_id.as_ref().is_some_and(|id| id!=&session.id)
                    || input.workspace.as_ref().is_some_and(|workspace|!crate::workspace::same(workspace,&session.workspace)) {
                    return Ok(());
                }
                let repository_id=repository_id(&session.workspace)?;
                if input.repository_id.as_ref().is_some_and(|id|Some(id)!=repository_id.as_ref()) { return Ok(()); }
                page.workspace.push(session.workspace.clone());
                if name=="harness_session_list" {
                    page.push(json!({"session_id":session.id,"title":session.name,
                        "workspace":session.workspace,"repository_id":repository_id,"updated_at_ms":session.updated_at_ms}))?;
                } else {
                    let query=input.query.as_deref().context("search requires query")?;
                    ensure!(!query.trim().is_empty(),"search query must not be empty");
                    let search=input.search_in.clone().unwrap_or_else(||vec!["messages".into()]);
                    unavailable_inputs|=search.iter().any(|field|field=="tool_inputs");
                    if search.iter().any(|field|field=="messages")
                        && let Some(excerpt)=excerpt(&session.name,query) {
                        page.push(json!({"session_id":session.id,"workspace":session.workspace,
                            "matched_field":"title","excerpt":excerpt}))?;
                    }
                    reader.exchanges(&session,|exchange| {
                        self.check(name)?;
                        if search.iter().any(|field|field=="messages") {
                            for message in conversation(&exchange) {
                                let text=message["text"].as_str().unwrap_or_default();
                                if let Some(excerpt)=excerpt(text,query) {
                                    page.push(json!({"session_id":session.id,"exchange_id":exchange.id,
                                        "workspace":session.workspace,"matched_field":"messages","excerpt":excerpt}))?;
                                }
                            }
                        }
                        if search.iter().any(|field|field=="tool_outputs") {
                            for turn in &exchange.turn { for tool in exchange.tools_for_turn(turn.id()) {
                                let mut tail=String::new();
                                reader.output(&exchange.id,turn.id(),&tool.id,|text| {
                                    self.check(name)?;
                                    let combined=format!("{tail}{text}");
                                    if let Some(excerpt)=excerpt(&combined,query) {
                                        page.push(json!({"session_id":session.id,"exchange_id":exchange.id,
                                            "workspace":session.workspace,"turn_id":turn.id(),"tool_call_id":tool.id,
                                            "matched_field":"tool_outputs","excerpt":excerpt}))?;
                                    }
                                    tail=combined.chars().rev().take(query.chars().count()).collect::<String>().chars().rev().collect();
                                    Ok(())
                                })?;
                            }}
                        }
                        Ok(())
                    })?;
                }
                Ok(())
            })?;
            if unavailable_inputs {
                page.push(json!({"unavailable":"tool_inputs","reason":"Structured tool inputs are not retained in exchange storage. Use session logs for diagnostic metadata."}))?;
            }
        } else {
            let session = self.selected(&reader, input.session_id.as_deref())?;
            page.workspace.push(session.workspace.clone());
            match name {
                "harness_config" => page.push(self.config(&reader, &session)?)?,
                "harness_session_status" => page.push(self.status(&reader, &session)?)?,
                "harness_session_exchange_list" => reader.exchanges(&session, |exchange| {
                    self.check(name)?;
                    page.push(exchange_summary(&exchange))
                })?,
                "harness_session_exchange" => {
                    let exchange = reader.exchange(
                        &session.id,
                        input
                            .exchange_id
                            .as_deref()
                            .context("exchange_id is required")?,
                    )?;
                    page.push(exchange_summary(&exchange))?;
                    for turn in &exchange.turn {
                        for tool in exchange.tools_for_turn(turn.id()) {
                            page.push(
                                json!({"kind":"tool","turn_id":turn.id(),"tool_call_id":tool.id,
                            "name":tool.title,"status":tool.status}),
                            )?;
                        }
                    }
                    for decision in &exchange.approval_resolution {
                        page.push(json!({"kind":"approval","id":decision.id,"resolution_source":decision.resolution_source}))?;
                    }
                    for run in exchange.checks() {
                        page.push(json!({"kind":"check","run_id":run.id,"state":run.state}))?;
                    }
                    for node in &exchange.node_list {
                        match node {
                            ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                                crate::timeline::TimelineEvent::Check { .. } =>
                                    anyhow::bail!("exchange checks must belong to commentary"),
                                crate::timeline::TimelineEvent::Session { event } =>
                                    page.push(json!({"kind":"session_event","id":event.id,"detail":event.detail}))?,
                            },
                            ExchangeNode::AgentReference { agent } =>
                                page.push(json!({"kind":"agent","agent_id":agent.child_agent_id,
                                    "exchange_id":agent.child_exchange_id,"task":agent.task}))?,
                            ExchangeNode::QuestionPresented { id,question,answer } =>
                                page.push(json!({"kind":"question","id":id,"question":question,"answer":answer}))?,
                            _=>{}
                        }
                    }
                }
                "harness_session_tool" => {
                    let exchange = reader.exchange(
                        &session.id,
                        input
                            .exchange_id
                            .as_deref()
                            .context("exchange_id is required")?,
                    )?;
                    let tool_id = input
                        .tool_call_id
                        .as_deref()
                        .context("tool_call_id is required")?;
                    let candidates = exchange
                        .turn
                        .iter()
                        .filter(|turn| input.turn_id.as_deref().is_none_or(|id| id == turn.id()))
                        .flat_map(|turn| {
                            exchange
                                .tools_for_turn(turn.id())
                                .filter(move |tool| tool.id == tool_id)
                                .map(move |tool| (turn, tool))
                        })
                        .collect::<Vec<_>>();
                    ensure!(
                        candidates.len() == 1,
                        "tool occurrence is missing or ambiguous. Select its turn_id"
                    );
                    let (turn, tool) = candidates[0];
                    page.push(json!({"session_id":session.id,"exchange_id":exchange.id,
                        "turn_id":turn.id(),"tool_call_id":tool.id,"name":tool.title,"status":tool.status}))?;
                    for field in input
                        .fields
                        .clone()
                        .unwrap_or_else(|| vec!["output".into()])
                    {
                        match field.as_str() {
                            "input" => page.push(json!({"field":"input","unavailable":true,"reason":"Structured input was not retained"}))?,
                            "normalized_changes" => page.push(json!({"field":"normalized_changes","value":tool.change}))?,
                            "output" => {
                                let mut offset=0usize;
                                reader.output(&exchange.id,turn.id(),&tool.id,|text| {
                                    self.check(name)?;
                                    page.push(json!({"field":"output","offset":offset,"text":text}))?;
                                    offset+=text.chars().count(); Ok(())
                                })?;
                                if offset==0 { page.push(json!({"field":"output","offset":0,"text":""}))?; }
                            }
                            _ => anyhow::bail!("unknown tool field"),
                        }
                    }
                }
                "harness_session_log" => self.logs(&session, &mut page)?,
                _ => anyhow::bail!("unknown Harness inspection tool"),
            }
        }
        self.check(name)?;
        for workspace in &page.workspace {
            ensure!(
                self.allowed(workspace)?,
                "read authority for snapshot was revoked"
            );
        }
        let id = uuid::Uuid::new_v4().to_string();
        let result = self.page_response(&id, &mut page, 0)?;
        let mut pages = self
            .pages
            .lock()
            .map_err(|_| anyhow::anyhow!("inspection snapshot lock poisoned"))?;
        self.check(name)?;
        pages.insert(id, page);
        Ok(result)
    }

    fn page_response(&self, id: &str, page: &mut InspectionPage, offset: u64) -> Result<String> {
        let mut value = page.read(offset)?;
        let next = value
            .as_object_mut()
            .unwrap()
            .remove("next_offset")
            .and_then(|offset| offset.as_u64());
        if let Some(offset) = next {
            value["next_cursor"] = json!(format!("{id}:{offset}"));
        }
        Ok(serde_json::to_string(&value)?)
    }

    fn selected(&self, reader: &SessionReader, id: Option<&str>) -> Result<HarnessSession> {
        let id = id.unwrap_or(&self.session.id);
        let session = if id == self.session.id {
            self.session.clone()
        } else {
            reader.session(id)?
        };
        ensure!(
            self.allowed(&session.workspace)?,
            "current read policy does not authorize this session workspace"
        );
        Ok(session)
    }

    fn status(&self, reader: &SessionReader, session: &HarnessSession) -> Result<Value> {
        Ok(
            json!({"session_id":session.id,"workspace":session.workspace,
            "approval_mode":session.approval_mode,"access":session.access,
            "task":reader.task(session)?,"observation":if session.id==self.session.id {"authenticated_turn_start"} else {"last_persisted"}}),
        )
    }

    fn config(&self, reader: &SessionReader, session: &HarnessSession) -> Result<Value> {
        let settings = SettingsStore::read_only(&self.data_root)?;
        let snapshot = settings.get(&session.workspace, &session.id, &session.backend)?;
        let root = settings.catalog_root()?;
        let source = root.as_ref().map(|root| root.join("nvim"));
        let mut result = self.status(reader, session)?;
        result["settings"] = serde_json::to_value(&snapshot.desired)?;
        result["settings"]["session_tools"] = json!(snapshot.desired.session_tools.unwrap_or(true));
        result["settings"]["automatic_checks"] = json!(snapshot.desired.automatic_checks_enabled());
        result["settings"]["check_timing"] =
            json!(snapshot.desired.check_timing.unwrap_or_default());
        result["settings"]["check_editing"] =
            json!(snapshot.desired.check_editing.unwrap_or(false));
        result["settings"]["lsp_tools"] = json!(snapshot.desired.lsp_tools.unwrap_or(false));
        result["settings"]["subagent_tools"] =
            json!(snapshot.desired.subagent_tools.unwrap_or(false));
        result["approval_defaults"] = json!({"work":session.default_approval,"plan":session.plan_approval,
            "plan_auto_approve_revisions":session.plan_auto_approve_revisions,
            "approval_timeout":session.approval_timeout,"answer_timeout":session.answer_timeout});
        result["permissions"] = serde_json::to_value(&self.policy.document)?;
        result["locations"] = json!({"config_directory":root,"forge_rust":source.as_ref().map(|root|root.join("rust/forge")),
            "forge_lua":source.as_ref().map(|root|root.join("lua/forge")),
            "documentation":source.as_ref().map(|root|root.join("contributing/forge/README.md")),
            "permissions":self.permission_path,"sessions_database":self.data_root.join("harness.sqlite3"),
            "settings_database":self.data_root.join("settings.db"),"project_checks":Path::new(&session.workspace).join(".forge.json"),
            "session_log":self.trace.session_status(&session.id).path});
        Ok(result)
    }

    fn logs(&self, session: &HarnessSession, page: &mut InspectionPage) -> Result<()> {
        let current = PathBuf::from(self.trace.session_status(&session.id).path);
        let mut found = false;
        for path in (1..=3)
            .rev()
            .map(|generation| current.with_extension(format!("jsonl.{generation}")))
            .chain([current.clone()])
        {
            let file = match std::fs::File::open(&path) {
                Ok(file) => file,
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
                Err(error) => return Err(error).context("open session log"),
            };
            found = true;
            for line in BufReader::new(file).lines() {
                self.check("harness_session_log")?;
                let record: Value =
                    crate::codec::from_str(&line?).context("decode session log record")?;
                // Raw provider transports can contain private reasoning and embedded credentials.
                // Expose the diagnostic metadata allowlist, not those transport bodies.
                page.push(crate::trace::inspection_record(&record))?;
            }
        }
        if !found {
            page.push(json!({"session_id":session.id,"unavailable":"logs","reason":"No retained session log"}))?;
        }
        Ok(())
    }
}

fn repository_id(workspace: &str) -> Result<Option<String>> {
    Ok(forge_git::discover_identity(Path::new(workspace))?
        .map(|identity| hex::encode(identity.common_directory.to_string_lossy().as_bytes())))
}

fn conversation(exchange: &Exchange) -> Vec<Value> {
    let mut messages = vec![json!({"role":"user","text":exchange.prompt})];
    let mut seen = HashSet::new();
    for node in &exchange.node_list {
        match node {
            ExchangeNode::ExchangeInput { prompt } => {
                messages.push(json!({"role":"user","text":prompt.text,"intent":prompt.intent}))
            }
            ExchangeNode::TurnContent {
                turn_id,
                item: TurnItem::Message { id },
                ..
            } => {
                if let Some(message) = exchange
                    .turn
                    .iter()
                    .find(|turn| turn.id() == turn_id)
                    .and_then(|turn| turn.messages().iter().find(|message| message.id() == id))
                    && message.kind() == MessageKind::Assistant
                    && message.delivery() == MessageDelivery::Final
                {
                    seen.insert(id.as_str());
                    messages.push(json!({"role":"assistant","text":message.text()}));
                }
            }
            _ => {}
        }
    }
    for message in exchange.turn.iter().flat_map(|turn| turn.messages()) {
        if message.kind() == MessageKind::Assistant
            && message.delivery() == MessageDelivery::Final
            && !seen.contains(message.id())
        {
            messages.push(json!({"role":"assistant","text":message.text()}));
        }
    }
    messages
}

fn exchange_summary(exchange: &Exchange) -> Value {
    let mut value = json!({"session_id":exchange.session_id,"exchange_id":exchange.id,"messages":conversation(exchange)});
    if exchange.state() != ExchangeState::Complete {
        value["state"] = json!(exchange.state());
    }
    if !exchange
        .turn
        .iter()
        .flat_map(|turn| turn.messages())
        .any(|message| {
            message.kind() == MessageKind::Assistant && message.delivery() == MessageDelivery::Final
        })
    {
        value["final_answer_available"] = json!(false);
    }
    value
}

fn excerpt(text: &str, query: &str) -> Option<String> {
    let characters = text.chars().collect::<Vec<_>>();
    // Find on folded text, then derive context using character positions rather than byte offsets.
    let folded = text.to_lowercase();
    let start = folded.find(&query.to_lowercase())?;
    let mut folded_bytes = 0;
    let position = text
        .chars()
        .take_while(|character| {
            let before = folded_bytes;
            folded_bytes += character.to_lowercase().map(char::len_utf8).sum::<usize>();
            before < start
        })
        .count();
    Some(
        characters
            .into_iter()
            .skip(position.saturating_sub(100))
            .take(query.chars().count() + 200)
            .collect(),
    )
}
